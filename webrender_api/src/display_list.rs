/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use euclid::SideOffsets2D;
use peek_poke::{ensure_red_zone, peek_from_slice, poke_extend_vec};
use peek_poke::{poke_inplace_slice, poke_into_vec, Poke};
#[cfg(feature = "deserialize")]
use serde::de::Deserializer;
#[cfg(feature = "serialize")]
use serde::ser::{Serializer, SerializeSeq};
use serde::{Deserialize, Serialize};
use std::io::{stdout, Write};
use std::marker::PhantomData;
use std::ops::Range;
use std::mem;
use std::collections::HashMap;
use time::precise_time_ns;
// local imports
use crate::display_item as di;
use crate::api::{PipelineId, PropertyBinding};
use crate::gradient_builder::GradientBuilder;
use crate::color::ColorF;
use crate::font::{FontInstanceKey, GlyphInstance, GlyphOptions};
use crate::image::{ColorDepth, ImageKey};
use crate::units::*;


// We don't want to push a long text-run. If a text-run is too long, split it into several parts.
// This needs to be set to (renderer::MAX_VERTEX_TEXTURE_WIDTH - VECS_PER_TEXT_RUN) * 2
pub const MAX_TEXT_RUN_LENGTH: usize = 2040;

// See ROOT_REFERENCE_FRAME_SPATIAL_ID and ROOT_SCROLL_NODE_SPATIAL_ID
// TODO(mrobinson): It would be a good idea to eliminate the root scroll frame which is only
// used by Servo.
const FIRST_SPATIAL_NODE_INDEX: usize = 2;

// See ROOT_SCROLL_NODE_SPATIAL_ID
const FIRST_CLIP_NODE_INDEX: usize = 1;

#[repr(C)]
#[derive(Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ItemRange<'a, T> {
    bytes: &'a [u8],
    _boo: PhantomData<T>,
}

impl<'a, T> Copy for ItemRange<'a, T> {}
impl<'a, T> Clone for ItemRange<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Default for ItemRange<'a, T> {
    fn default() -> Self {
        ItemRange {
            bytes: Default::default(),
            _boo: PhantomData,
        }
    }
}

impl<'a, T> ItemRange<'a, T> {
    pub fn is_empty(&self) -> bool {
        // Nothing more than space for a length (0).
        self.bytes.len() <= mem::size_of::<usize>()
    }
}

impl<'a, T: Default> ItemRange<'a, T> {
    pub fn iter(&self) -> AuxIter<'a, T> {
        AuxIter::new(T::default(), self.bytes)
    }
}

impl<'a, T> IntoIterator for ItemRange<'a, T>
where
    T: Copy + Default + peek_poke::Peek,
{
    type Item = T;
    type IntoIter = AuxIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Copy, Clone)]
pub struct TempFilterData<'a> {
    pub func_types: ItemRange<'a, di::ComponentTransferFuncType>,
    pub r_values: ItemRange<'a, f32>,
    pub g_values: ItemRange<'a, f32>,
    pub b_values: ItemRange<'a, f32>,
    pub a_values: ItemRange<'a, f32>,
}

/// A display list.
#[derive(Clone, Default)]
pub struct BuiltDisplayList {
    /// Serde encoded bytes. Mostly DisplayItems, but some mixed in slices.
    data: Vec<u8>,
    descriptor: BuiltDisplayListDescriptor,
}

/// Describes the memory layout of a display list.
///
/// A display list consists of some number of display list items, followed by a number of display
/// items.
#[repr(C)]
#[derive(Copy, Clone, Default, Deserialize, Serialize)]
pub struct BuiltDisplayListDescriptor {
    /// The first IPC time stamp: before any work has been done
    builder_start_time: u64,
    /// The second IPC time stamp: after serialization
    builder_finish_time: u64,
    /// The third IPC time stamp: just before sending
    send_start_time: u64,
    /// The amount of clipping nodes created while building this display list.
    total_clip_nodes: usize,
    /// The amount of spatial nodes created while building this display list.
    total_spatial_nodes: usize,
}

pub struct BuiltDisplayListIter<'a> {
    list: &'a BuiltDisplayList,
    data: &'a [u8],
    cur_item: di::DisplayItem,
    cur_stops: ItemRange<'a, di::GradientStop>,
    cur_glyphs: ItemRange<'a, GlyphInstance>,
    cur_filters: ItemRange<'a, di::FilterOp>,
    cur_filter_data: Vec<TempFilterData<'a>>,
    cur_filter_primitives: ItemRange<'a, di::FilterPrimitive>,
    cur_clip_chain_items: ItemRange<'a, di::ClipId>,
    cur_complex_clip: ItemRange<'a, di::ComplexClipRegion>,
    peeking: Peek,
    /// Should just be initialized but never populated in release builds
    debug_stats: DebugStats,
}

/// Internal info used for more detailed analysis of serialized display lists
#[allow(dead_code)]
struct DebugStats {
    /// Last address in the buffer we pointed to, for computing serialized sizes
    last_addr: usize,
    stats: HashMap<&'static str, ItemStats>,
}

impl DebugStats {
    #[cfg(feature = "display_list_stats")]
    fn _update_entry(&mut self, name: &'static str, item_count: usize, byte_count: usize) {
        let entry = self.stats.entry(name).or_default();
        entry.total_count += item_count;
        entry.num_bytes += byte_count;
    }

    /// Computes the number of bytes we've processed since we last called
    /// this method, so we can compute the serialized size of a display item.
    #[cfg(feature = "display_list_stats")]
    fn debug_num_bytes(&mut self, data: &[u8]) -> usize {
        let old_addr = self.last_addr;
        let new_addr = data.as_ptr() as usize;
        let delta = new_addr - old_addr;
        self.last_addr = new_addr;

        delta
    }

    /// Logs stats for the last deserialized display item
    #[cfg(feature = "display_list_stats")]
    fn log_item(&mut self, data: &[u8], item: &di::DisplayItem) {
        let num_bytes = self.debug_num_bytes(data);
        self._update_entry(item.debug_name(), 1, num_bytes);
    }

    /// Logs the stats for the given serialized slice
    #[cfg(feature = "display_list_stats")]
    fn log_slice<T: Peek>(
        &mut self,
        slice_name: &'static str,
        range: &ItemRange<T>,
    ) {
        // Run this so log_item_stats is accurate, but ignore its result
        // because log_slice_stats may be called after multiple slices have been
        // processed, and the `range` has everything we need.
        self.last_addr = range.bytes.as_ptr() as usize + range.bytes.len();

        self._update_entry(slice_name, range.iter().len(), range.bytes.len());
    }

    #[cfg(not(feature = "display_list_stats"))]
    fn log_slice<T>(&mut self, _slice_name: &str, _range: &ItemRange<T>) {
        /* no-op */
    }
}

/// Stats for an individual item
#[derive(Copy, Clone, Debug, Default)]
pub struct ItemStats {
    /// How many instances of this kind of item we deserialized
    pub total_count: usize,
    /// How many bytes we processed for this kind of item
    pub num_bytes: usize,
}

pub struct DisplayItemRef<'a: 'b, 'b> {
    iter: &'b BuiltDisplayListIter<'a>,
}

#[derive(PartialEq)]
enum Peek {
    StartPeeking,
    IsPeeking,
    NotPeeking,
}

#[derive(Clone)]
pub struct AuxIter<'a, T> {
    item: T,
    data: &'a [u8],
    size: usize,
//    _boo: PhantomData<T>,
}

impl BuiltDisplayListDescriptor {}

impl BuiltDisplayList {
    pub fn from_data(data: Vec<u8>, descriptor: BuiltDisplayListDescriptor) -> Self {
        BuiltDisplayList { data, descriptor }
    }

    pub fn into_data(mut self) -> (Vec<u8>, BuiltDisplayListDescriptor) {
        self.descriptor.send_start_time = precise_time_ns();
        (self.data, self.descriptor)
    }

    pub fn data(&self) -> &[u8] {
        &self.data[..]
    }

    // Currently redundant with data, but may be useful if we add extra data to dl
    pub fn item_slice(&self) -> &[u8] {
        &self.data[..]
    }

    pub fn descriptor(&self) -> &BuiltDisplayListDescriptor {
        &self.descriptor
    }

    pub fn times(&self) -> (u64, u64, u64) {
        (
            self.descriptor.builder_start_time,
            self.descriptor.builder_finish_time,
            self.descriptor.send_start_time,
        )
    }

    pub fn total_clip_nodes(&self) -> usize {
        self.descriptor.total_clip_nodes
    }

    pub fn total_spatial_nodes(&self) -> usize {
        self.descriptor.total_spatial_nodes
    }

    pub fn iter(&self) -> BuiltDisplayListIter {
        BuiltDisplayListIter::new(self)
    }
}

/// Returns the byte-range the slice occupied.
fn skip_slice<'a, T: peek_poke::Peek>(data: &mut &'a [u8]) -> ItemRange<'a, T> {
    let mut skip_offset = 0usize;
    *data = peek_from_slice(data, &mut skip_offset);
    let (skip, rest) = data.split_at(skip_offset);

    // Adjust data pointer to skip read values
    *data = rest;

    ItemRange {
        bytes: skip,
        _boo: PhantomData,
    }
}

impl<'a> BuiltDisplayListIter<'a> {
    pub fn new(list: &'a BuiltDisplayList) -> Self {
        Self::new_with_list_and_data(list, list.item_slice())
    }

    pub fn new_with_list_and_data(list: &'a BuiltDisplayList, data: &'a [u8]) -> Self {
        BuiltDisplayListIter {
            list,
            data,
            cur_item: di::DisplayItem::PopStackingContext,
            cur_stops: ItemRange::default(),
            cur_glyphs: ItemRange::default(),
            cur_filters: ItemRange::default(),
            cur_filter_data: Vec::new(),
            cur_filter_primitives: ItemRange::default(),
            cur_clip_chain_items: ItemRange::default(),
            cur_complex_clip: ItemRange::default(),
            peeking: Peek::NotPeeking,
            debug_stats: DebugStats {
                last_addr: data.as_ptr() as usize,
                stats: HashMap::default(),
            }
        }
    }

    pub fn display_list(&self) -> &'a BuiltDisplayList {
        self.list
    }

    pub fn next<'b>(&'b mut self) -> Option<DisplayItemRef<'a, 'b>> {
        use crate::DisplayItem::*;

        match self.peeking {
            Peek::IsPeeking => {
                self.peeking = Peek::NotPeeking;
                return Some(self.as_ref());
            }
            Peek::StartPeeking => {
                self.peeking = Peek::IsPeeking;
            }
            Peek::NotPeeking => { /* do nothing */ }
        }

        // Don't let these bleed into another item
        self.cur_stops = ItemRange::default();
        self.cur_complex_clip = ItemRange::default();
        self.cur_clip_chain_items = ItemRange::default();

        loop {
            self.next_raw()?;
            match self.cur_item {
                SetGradientStops |
                SetFilterOps |
                SetFilterData |
                SetFilterPrimitives => {
                    // These are marker items for populating other display items, don't yield them.
                    continue;
                }
                _ => {
                    break;
                }
            }
        }

        Some(self.as_ref())
    }

    /// Gets the next display item, even if it's a dummy. Also doesn't handle peeking
    /// and may leave irrelevant ranges live (so a Clip may have GradientStops if
    /// for some reason you ask).
    pub fn next_raw<'b>(&'b mut self) -> Option<DisplayItemRef<'a, 'b>> {
        use crate::DisplayItem::*;

        // A "red zone" of DisplayItem::max_size() bytes has been added to the
        // end of the serialized display list. If this amount, or less, is
        // remaining then we've reached the end of the display list.
        if self.data.len() <= di::DisplayItem::max_size() {
            return None;
        }

        self.data = peek_from_slice(self.data, &mut self.cur_item);
        self.log_item_stats();

        match self.cur_item {
            SetGradientStops => {
                self.cur_stops = skip_slice::<di::GradientStop>(&mut self.data);
                self.debug_stats.log_slice("set_gradient_stops.stops", &self.cur_stops);
            }
            SetFilterOps => {
                self.cur_filters = skip_slice::<di::FilterOp>(&mut self.data);
                self.debug_stats.log_slice("set_filter_ops.ops", &self.cur_filters);
            }
            SetFilterData => {
                self.cur_filter_data.push(TempFilterData {
                    func_types: skip_slice::<di::ComponentTransferFuncType>(&mut self.data),
                    r_values: skip_slice::<f32>(&mut self.data),
                    g_values: skip_slice::<f32>(&mut self.data),
                    b_values: skip_slice::<f32>(&mut self.data),
                    a_values: skip_slice::<f32>(&mut self.data),
                });

                let data = *self.cur_filter_data.last().unwrap();
                self.debug_stats.log_slice("set_filter_data.func_types", &data.func_types);
                self.debug_stats.log_slice("set_filter_data.r_values", &data.r_values);
                self.debug_stats.log_slice("set_filter_data.g_values", &data.g_values);
                self.debug_stats.log_slice("set_filter_data.b_values", &data.b_values);
                self.debug_stats.log_slice("set_filter_data.a_values", &data.a_values);
            }
            SetFilterPrimitives => {
                self.cur_filter_primitives = skip_slice::<di::FilterPrimitive>(&mut self.data);
                self.debug_stats.log_slice("set_filter_primitives.primitives", &self.cur_filter_primitives);
            }
            ClipChain(_) => {
                self.cur_clip_chain_items = skip_slice::<di::ClipId>(&mut self.data);
                self.debug_stats.log_slice("clip_chain.clip_ids", &self.cur_clip_chain_items);
            }
            Clip(_) | ScrollFrame(_) => {
                self.cur_complex_clip = skip_slice::<di::ComplexClipRegion>(&mut self.data);
                let name = if let Clip(_) = self.cur_item {
                    "clip.complex_clips"
                } else {
                    "scroll_frame.complex_clips"
                };
                self.debug_stats.log_slice(name, &self.cur_complex_clip);
            }
            Text(_) => {
                self.cur_glyphs = skip_slice::<GlyphInstance>(&mut self.data);
                self.debug_stats.log_slice("text.glyphs", &self.cur_glyphs);
            }
            _ => { /* do nothing */ }
        }

        Some(self.as_ref())
    }

    pub fn as_ref<'b>(&'b self) -> DisplayItemRef<'a, 'b> {
        DisplayItemRef { iter: self }
    }

    pub fn skip_current_stacking_context(&mut self) {
        let mut depth = 0;
        while let Some(item) = self.next() {
            match *item.item() {
                di::DisplayItem::PushStackingContext(..) => depth += 1,
                di::DisplayItem::PopStackingContext if depth == 0 => return,
                di::DisplayItem::PopStackingContext => depth -= 1,
                _ => {}
            }
            debug_assert!(depth >= 0);
        }
    }

    pub fn current_stacking_context_empty(&mut self) -> bool {
        match self.peek() {
            Some(item) => *item.item() == di::DisplayItem::PopStackingContext,
            None => true,
        }
    }

    pub fn peek<'b>(&'b mut self) -> Option<DisplayItemRef<'a, 'b>> {
        if self.peeking == Peek::NotPeeking {
            self.peeking = Peek::StartPeeking;
            self.next()
        } else {
            Some(self.as_ref())
        }
    }

    /// Get the debug stats for what this iterator has deserialized.
    /// Should always be empty in release builds.
    pub fn debug_stats(&mut self) -> Vec<(&'static str, ItemStats)> {
        let mut result = self.debug_stats.stats.drain().collect::<Vec<_>>();
        result.sort_by_key(|stats| stats.0);
        result
    }

    /// Adds the debug stats from another to our own, assuming we are a sub-iter of the other
    /// (so we can ignore where they were in the traversal).
    pub fn merge_debug_stats_from(&mut self, other: &mut Self) {
        for (key, other_entry) in other.debug_stats.stats.iter() {
            let entry = self.debug_stats.stats.entry(key).or_default();

            entry.total_count += other_entry.total_count;
            entry.num_bytes += other_entry.num_bytes;
        }
    }

    /// Logs stats for the last deserialized display item
    #[cfg(feature = "display_list_stats")]
    fn log_item_stats(&mut self) {
        self.debug_stats.log_item(self.data, &self.cur_item);
    }

    #[cfg(not(feature = "display_list_stats"))]
    fn log_item_stats(&mut self) { /* no-op */ }
}

// Some of these might just become ItemRanges
impl<'a, 'b> DisplayItemRef<'a, 'b> {
    pub fn item(&self) -> &di::DisplayItem {
        &self.iter.cur_item
    }

    pub fn complex_clip(&self) -> ItemRange<di::ComplexClipRegion> {
        self.iter.cur_complex_clip
    }

    pub fn gradient_stops(&self) -> ItemRange<di::GradientStop> {
        self.iter.cur_stops
    }

    pub fn glyphs(&self) -> ItemRange<GlyphInstance> {
        self.iter.cur_glyphs
    }

    pub fn filters(&self) -> ItemRange<di::FilterOp> {
        self.iter.cur_filters
    }

    pub fn filter_datas(&self) -> &Vec<TempFilterData> {
        &self.iter.cur_filter_data
    }

    pub fn filter_primitives(&self) -> ItemRange<di::FilterPrimitive> {
        self.iter.cur_filter_primitives
    }

    pub fn clip_chain_items(&self) -> ItemRange<di::ClipId> {
        self.iter.cur_clip_chain_items
    }

    pub fn display_list(&self) -> &BuiltDisplayList {
        self.iter.display_list()
    }

    // Creates a new iterator where this element's iterator is, to hack around borrowck.
    pub fn sub_iter(&self) -> BuiltDisplayListIter<'a> {
        BuiltDisplayListIter::new_with_list_and_data(self.iter.list, self.iter.data)
    }
}

impl<'a, T> AuxIter<'a, T> {
    pub fn new(item: T, mut data: &'a [u8]) -> Self {
        let mut size = 0usize;
        if !data.is_empty() {
            data = peek_from_slice(data, &mut size);
        };

        AuxIter {
            item,
            data,
            size,
//            _boo: PhantomData,
        }
    }
}

impl<'a, T: Copy + peek_poke::Peek> Iterator for AuxIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.size == 0 {
            None
        } else {
            self.size -= 1;
            self.data = peek_from_slice(self.data, &mut self.item);
            Some(self.item)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.size, Some(self.size))
    }
}

impl<'a, T: Copy + peek_poke::Peek> ::std::iter::ExactSizeIterator for AuxIter<'a, T> {}


#[cfg(feature = "serialize")]
impl Serialize for BuiltDisplayList {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use crate::display_item::DisplayItem as Real;
        use crate::display_item::DebugDisplayItem as Debug;

        let mut seq = serializer.serialize_seq(None)?;
        let mut traversal = self.iter();
        while let Some(item) = traversal.next_raw() {
            let serial_di = match *item.item() {
                Real::Clip(v) => Debug::Clip(
                    v,
                    item.iter.cur_complex_clip.iter().collect()
                ),
                Real::ClipChain(v) => Debug::ClipChain(
                    v,
                    item.iter.cur_clip_chain_items.iter().collect()
                ),
                Real::ScrollFrame(v) => Debug::ScrollFrame(
                    v,
                    item.iter.cur_complex_clip.iter().collect()
                ),
                Real::Text(v) => Debug::Text(
                    v,
                    item.iter.cur_glyphs.iter().collect()
                ),
                Real::SetFilterOps => Debug::SetFilterOps(
                    item.iter.cur_filters.iter().collect()
                ),
                Real::SetFilterData => {
                    debug_assert!(!item.iter.cur_filter_data.is_empty());
                    let temp_filter_data = &item.iter.cur_filter_data[item.iter.cur_filter_data.len()-1];

                    let func_types: Vec<di::ComponentTransferFuncType> =
                        temp_filter_data.func_types.iter().collect();
                    debug_assert!(func_types.len() == 4);
                    Debug::SetFilterData(di::FilterData {
                        func_r_type: func_types[0],
                        r_values: temp_filter_data.r_values.iter().collect(),
                        func_g_type: func_types[1],
                        g_values: temp_filter_data.g_values.iter().collect(),
                        func_b_type: func_types[2],
                        b_values: temp_filter_data.b_values.iter().collect(),
                        func_a_type: func_types[3],
                        a_values: temp_filter_data.a_values.iter().collect(),
                    })
                },
                Real::SetFilterPrimitives => Debug::SetFilterPrimitives(
                    item.iter.cur_filter_primitives.iter().collect()
                ),
                Real::SetGradientStops => Debug::SetGradientStops(
                    item.iter.cur_stops.iter().collect()
                ),
                Real::StickyFrame(v) => Debug::StickyFrame(v),
                Real::Rectangle(v) => Debug::Rectangle(v),
                Real::ClearRectangle(v) => Debug::ClearRectangle(v),
                Real::HitTest(v) => Debug::HitTest(v),
                Real::Line(v) => Debug::Line(v),
                Real::Image(v) => Debug::Image(v),
                Real::YuvImage(v) => Debug::YuvImage(v),
                Real::Border(v) => Debug::Border(v),
                Real::BoxShadow(v) => Debug::BoxShadow(v),
                Real::Gradient(v) => Debug::Gradient(v),
                Real::RadialGradient(v) => Debug::RadialGradient(v),
                Real::Iframe(v) => Debug::Iframe(v),
                Real::PushReferenceFrame(v) => Debug::PushReferenceFrame(v),
                Real::PushStackingContext(v) => Debug::PushStackingContext(v),
                Real::PushShadow(v) => Debug::PushShadow(v),

                Real::PopReferenceFrame => Debug::PopReferenceFrame,
                Real::PopStackingContext => Debug::PopStackingContext,
                Real::PopAllShadows => Debug::PopAllShadows,
            };
            seq.serialize_element(&serial_di)?
        }
        seq.end()
    }
}

// The purpose of this implementation is to deserialize
// a display list from one format just to immediately
// serialize then into a "built" `Vec<u8>`.

#[cfg(feature = "deserialize")]
impl<'de> Deserialize<'de> for BuiltDisplayList {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use crate::display_item::DisplayItem as Real;
        use crate::display_item::DebugDisplayItem as Debug;

        let list = Vec::<Debug>::deserialize(deserializer)?;

        let mut data = Vec::new();
        let mut temp = Vec::new();
        let mut total_clip_nodes = FIRST_CLIP_NODE_INDEX;
        let mut total_spatial_nodes = FIRST_SPATIAL_NODE_INDEX;
        for complete in list {
            let item = match complete {
                Debug::Clip(v, complex_clips) => {
                    total_clip_nodes += 1;
                    DisplayListBuilder::push_iter_impl(&mut temp, complex_clips);
                    Real::Clip(v)
                },
                Debug::ClipChain(v, clip_chain_ids) => {
                    DisplayListBuilder::push_iter_impl(&mut temp, clip_chain_ids);
                    Real::ClipChain(v)
                }
                Debug::ScrollFrame(v, complex_clips) => {
                    total_spatial_nodes += 1;
                    total_clip_nodes += 1;
                    DisplayListBuilder::push_iter_impl(&mut temp, complex_clips);
                    Real::ScrollFrame(v)
                }
                Debug::StickyFrame(v) => {
                    total_spatial_nodes += 1;
                    Real::StickyFrame(v)
                }
                Debug::Text(v, glyphs) => {
                    DisplayListBuilder::push_iter_impl(&mut temp, glyphs);
                    Real::Text(v)
                },
                Debug::Iframe(v) => {
                    total_clip_nodes += 1;
                    Real::Iframe(v)
                }
                Debug::PushReferenceFrame(v) => {
                    total_spatial_nodes += 1;
                    Real::PushReferenceFrame(v)
                }
                Debug::SetFilterOps(filters) => {
                    DisplayListBuilder::push_iter_impl(&mut temp, filters);
                    Real::SetFilterOps
                },
                Debug::SetFilterData(filter_data) => {
                    let func_types: Vec<di::ComponentTransferFuncType> =
                        [filter_data.func_r_type,
                         filter_data.func_g_type,
                         filter_data.func_b_type,
                         filter_data.func_a_type].to_vec();
                    DisplayListBuilder::push_iter_impl(&mut temp, func_types);
                    DisplayListBuilder::push_iter_impl(&mut temp, filter_data.r_values);
                    DisplayListBuilder::push_iter_impl(&mut temp, filter_data.g_values);
                    DisplayListBuilder::push_iter_impl(&mut temp, filter_data.b_values);
                    DisplayListBuilder::push_iter_impl(&mut temp, filter_data.a_values);
                    Real::SetFilterData
                },
                Debug::SetFilterPrimitives(filter_primitives) => {
                    DisplayListBuilder::push_iter_impl(&mut temp, filter_primitives);
                    Real::SetFilterPrimitives
                }
                Debug::SetGradientStops(stops) => {
                    DisplayListBuilder::push_iter_impl(&mut temp, stops);
                    Real::SetGradientStops
                },

                Debug::Rectangle(v) => Real::Rectangle(v),
                Debug::ClearRectangle(v) => Real::ClearRectangle(v),
                Debug::HitTest(v) => Real::HitTest(v),
                Debug::Line(v) => Real::Line(v),
                Debug::Image(v) => Real::Image(v),
                Debug::YuvImage(v) => Real::YuvImage(v),
                Debug::Border(v) => Real::Border(v),
                Debug::BoxShadow(v) => Real::BoxShadow(v),
                Debug::Gradient(v) => Real::Gradient(v),
                Debug::RadialGradient(v) => Real::RadialGradient(v),
                Debug::PushStackingContext(v) => Real::PushStackingContext(v),
                Debug::PushShadow(v) => Real::PushShadow(v),

                Debug::PopStackingContext => Real::PopStackingContext,
                Debug::PopReferenceFrame => Real::PopReferenceFrame,
                Debug::PopAllShadows => Real::PopAllShadows,
            };
            poke_into_vec(&item, &mut data);
            // the aux data is serialized after the item, hence the temporary
            data.extend(temp.drain(..));
        }

        Ok(BuiltDisplayList {
            data,
            descriptor: BuiltDisplayListDescriptor {
                builder_start_time: 0,
                builder_finish_time: 1,
                send_start_time: 0,
                total_clip_nodes,
                total_spatial_nodes,
            },
        })
    }
}

#[derive(Clone, Debug)]
pub struct SaveState {
    dl_len: usize,
    next_clip_index: usize,
    next_spatial_index: usize,
    next_clip_chain_id: u64,
}

#[derive(Clone)]
pub struct DisplayListBuilder {
    pub data: Vec<u8>,
    pub pipeline_id: PipelineId,
    next_clip_index: usize,
    next_spatial_index: usize,
    next_clip_chain_id: u64,
    builder_start_time: u64,

    /// The size of the content of this display list. This is used to allow scrolling
    /// outside the bounds of the display list items themselves.
    content_size: LayoutSize,
    save_state: Option<SaveState>,
}

impl DisplayListBuilder {
    pub fn new(pipeline_id: PipelineId, content_size: LayoutSize) -> Self {
        Self::with_capacity(pipeline_id, content_size, 0)
    }

    pub fn with_capacity(
        pipeline_id: PipelineId,
        content_size: LayoutSize,
        capacity: usize,
    ) -> Self {
        let start_time = precise_time_ns();

        DisplayListBuilder {
            data: Vec::with_capacity(capacity),
            pipeline_id,
            next_clip_index: FIRST_CLIP_NODE_INDEX,
            next_spatial_index: FIRST_SPATIAL_NODE_INDEX,
            next_clip_chain_id: 0,
            builder_start_time: start_time,
            content_size,
            save_state: None,
        }
    }

    /// Return the content size for this display list
    pub fn content_size(&self) -> LayoutSize {
        self.content_size
    }

    /// Saves the current display list state, so it may be `restore()`'d.
    ///
    /// # Conditions:
    ///
    /// * Doesn't support popping clips that were pushed before the save.
    /// * Doesn't support nested saves.
    /// * Must call `clear_save()` if the restore becomes unnecessary.
    pub fn save(&mut self) {
        assert!(self.save_state.is_none(), "DisplayListBuilder doesn't support nested saves");

        self.save_state = Some(SaveState {
            dl_len: self.data.len(),
            next_clip_index: self.next_clip_index,
            next_spatial_index: self.next_spatial_index,
            next_clip_chain_id: self.next_clip_chain_id,
        });
    }

    /// Restores the state of the builder to when `save()` was last called.
    pub fn restore(&mut self) {
        let state = self.save_state.take().expect("No save to restore DisplayListBuilder from");

        self.data.truncate(state.dl_len);
        self.next_clip_index = state.next_clip_index;
        self.next_spatial_index = state.next_spatial_index;
        self.next_clip_chain_id = state.next_clip_chain_id;
    }

    /// Discards the builder's save (indicating the attempted operation was successful).
    pub fn clear_save(&mut self) {
        self.save_state.take().expect("No save to clear in DisplayListBuilder");
    }

    /// Print the display items in the list to stdout.
    pub fn print_display_list(&mut self) {
        self.emit_display_list(0, Range { start: None, end: None }, stdout());
    }

    /// Emits a debug representation of display items in the list, for debugging
    /// purposes. If the range's start parameter is specified, only display
    /// items starting at that index (inclusive) will be printed. If the range's
    /// end parameter is specified, only display items before that index
    /// (exclusive) will be printed. Calling this function with end <= start is
    /// allowed but is just a waste of CPU cycles. The function emits the
    /// debug representation of the selected display items, one per line, with
    /// the given indent, to the provided sink object. The return value is
    /// the total number of items in the display list, which allows the
    /// caller to subsequently invoke this function to only dump the newly-added
    /// items.
    pub fn emit_display_list<W>(
        &mut self,
        indent: usize,
        range: Range<Option<usize>>,
        mut sink: W,
    ) -> usize
    where
        W: Write
    {
        let mut temp = BuiltDisplayList::default();
        mem::swap(&mut temp.data, &mut self.data);

        let mut index: usize = 0;
        {
            let mut iter = BuiltDisplayListIter::new(&temp);
            while let Some(item) = iter.next_raw() {
                if index >= range.start.unwrap_or(0) && range.end.map_or(true, |e| index < e) {
                    writeln!(sink, "{}{:?}", "  ".repeat(indent), item.item()).unwrap();
                }
                index += 1;
            }
        }

        self.data = temp.data;
        index
    }

    /// Add an item to the display list.
    ///
    /// NOTE: It is usually preferable to use the specialized methods to push
    /// display items. Pushing unexpected or invalid items here may
    /// result in WebRender panicking or behaving in unexpected ways.
    #[inline]
    pub fn push_item(&mut self, item: &di::DisplayItem) {
        poke_into_vec(item, &mut self.data);
    }

    fn push_iter_impl<I>(data: &mut Vec<u8>, iter_source: I)
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator,
        I::Item: Poke,
    {
        let iter = iter_source.into_iter();
        let len = iter.len();
        // Format:
        // payload_byte_size: usize, item_count: usize, [I; item_count]

        // Track the the location of where to write byte size with offsets
        // instead of pointers because data may be moved in memory during
        // `serialize_iter_fast`.
        let byte_size_offset = data.len();

        // We write a dummy value so there's room for later
        poke_into_vec(&0usize, data);
        poke_into_vec(&len, data);
        let count = poke_extend_vec(iter, data);
        debug_assert_eq!(len, count);

        // Add red zone
        ensure_red_zone::<I::Item>(data);

        // Now write the actual byte_size
        let final_offset = data.len();
        debug_assert!(final_offset >= (byte_size_offset + mem::size_of::<usize>()));
        let byte_size = final_offset - byte_size_offset - mem::size_of::<usize>();
        poke_inplace_slice(&byte_size, &mut data[byte_size_offset..]);
    }

    /// Push items from an iterator to the display list.
    ///
    /// NOTE: Pushing unexpected or invalid items to the display list
    /// may result in panic and confusion.
    pub fn push_iter<I>(&mut self, iter: I)
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator,
        I::Item: Poke,
    {
        Self::push_iter_impl(&mut self.data, iter);
    }

    pub fn push_rect(
        &mut self,
        common: &di::CommonItemProperties,
        color: ColorF,
    ) {
        let item = di::DisplayItem::Rectangle(di::RectangleDisplayItem {
            common: *common,
            color
        });
        self.push_item(&item);
    }

    pub fn push_clear_rect(
        &mut self,
        common: &di::CommonItemProperties,
    ) {
        let item = di::DisplayItem::ClearRectangle(di::ClearRectangleDisplayItem {
            common: *common,
        });
        self.push_item(&item);
    }

    pub fn push_hit_test(
        &mut self,
        common: &di::CommonItemProperties,
    ) {
        let item = di::DisplayItem::HitTest(di::HitTestDisplayItem {
            common: *common,
        });
        self.push_item(&item);
    }

    pub fn push_line(
        &mut self,
        common: &di::CommonItemProperties,
        area: &LayoutRect,
        wavy_line_thickness: f32,
        orientation: di::LineOrientation,
        color: &ColorF,
        style: di::LineStyle,
    ) {
        let item = di::DisplayItem::Line(di::LineDisplayItem {
            common: *common,
            area: *area,
            wavy_line_thickness,
            orientation,
            color: *color,
            style,
        });

        self.push_item(&item);
    }

    pub fn push_image(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        stretch_size: LayoutSize,
        tile_spacing: LayoutSize,
        image_rendering: di::ImageRendering,
        alpha_type: di::AlphaType,
        key: ImageKey,
        color: ColorF,
    ) {
        let item = di::DisplayItem::Image(di::ImageDisplayItem {
            common: *common,
            bounds,
            image_key: key,
            stretch_size,
            tile_spacing,
            image_rendering,
            alpha_type,
            color,
        });

        self.push_item(&item);
    }

    /// Push a yuv image. All planar data in yuv image should use the same buffer type.
    pub fn push_yuv_image(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        yuv_data: di::YuvData,
        color_depth: ColorDepth,
        color_space: di::YuvColorSpace,
        image_rendering: di::ImageRendering,
    ) {
        let item = di::DisplayItem::YuvImage(di::YuvImageDisplayItem {
            common: *common,
            bounds,
            yuv_data,
            color_depth,
            color_space,
            image_rendering,
        });
        self.push_item(&item);
    }

    pub fn push_text(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        glyphs: &[GlyphInstance],
        font_key: FontInstanceKey,
        color: ColorF,
        glyph_options: Option<GlyphOptions>,
    ) {
        let item = di::DisplayItem::Text(di::TextDisplayItem {
            common: *common,
            bounds,
            color,
            font_key,
            glyph_options,
        });

        for split_glyphs in glyphs.chunks(MAX_TEXT_RUN_LENGTH) {
            self.push_item(&item);
            self.push_iter(split_glyphs);
        }
    }

    /// NOTE: gradients must be pushed in the order they're created
    /// because create_gradient stores the stops in anticipation.
    pub fn create_gradient(
        &mut self,
        start_point: LayoutPoint,
        end_point: LayoutPoint,
        stops: Vec<di::GradientStop>,
        extend_mode: di::ExtendMode,
    ) -> di::Gradient {
        let mut builder = GradientBuilder::with_stops(stops);
        let gradient = builder.gradient(start_point, end_point, extend_mode);
        self.push_stops(builder.stops());
        gradient
    }

    /// NOTE: gradients must be pushed in the order they're created
    /// because create_gradient stores the stops in anticipation.
    pub fn create_radial_gradient(
        &mut self,
        center: LayoutPoint,
        radius: LayoutSize,
        stops: Vec<di::GradientStop>,
        extend_mode: di::ExtendMode,
    ) -> di::RadialGradient {
        let mut builder = GradientBuilder::with_stops(stops);
        let gradient = builder.radial_gradient(center, radius, extend_mode);
        self.push_stops(builder.stops());
        gradient
    }

    pub fn push_border(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        widths: LayoutSideOffsets,
        details: di::BorderDetails,
    ) {
        let item = di::DisplayItem::Border(di::BorderDisplayItem {
            common: *common,
            bounds,
            details,
            widths,
        });

        self.push_item(&item);
    }

    pub fn push_box_shadow(
        &mut self,
        common: &di::CommonItemProperties,
        box_bounds: LayoutRect,
        offset: LayoutVector2D,
        color: ColorF,
        blur_radius: f32,
        spread_radius: f32,
        border_radius: di::BorderRadius,
        clip_mode: di::BoxShadowClipMode,
    ) {
        let item = di::DisplayItem::BoxShadow(di::BoxShadowDisplayItem {
            common: *common,
            box_bounds,
            offset,
            color,
            blur_radius,
            spread_radius,
            border_radius,
            clip_mode,
        });

        self.push_item(&item);
    }

    /// Pushes a linear gradient to be displayed.
    ///
    /// The gradient itself is described in the
    /// `gradient` parameter. It is drawn on
    /// a "tile" with the dimensions from `tile_size`.
    /// These tiles are now repeated to the right and
    /// to the bottom infinitely. If `tile_spacing`
    /// is not zero spacers with the given dimensions
    /// are inserted between the tiles as seams.
    ///
    /// The origin of the tiles is given in `layout.rect.origin`.
    /// If the gradient should only be displayed once limit
    /// the `layout.rect.size` to a single tile.
    /// The gradient is only visible within the local clip.
    pub fn push_gradient(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        gradient: di::Gradient,
        tile_size: LayoutSize,
        tile_spacing: LayoutSize,
    ) {
        let item = di::DisplayItem::Gradient(di::GradientDisplayItem {
            common: *common,
            bounds,
            gradient,
            tile_size,
            tile_spacing,
        });

        self.push_item(&item);
    }

    /// Pushes a radial gradient to be displayed.
    ///
    /// See [`push_gradient`](#method.push_gradient) for explanation.
    pub fn push_radial_gradient(
        &mut self,
        common: &di::CommonItemProperties,
        bounds: LayoutRect,
        gradient: di::RadialGradient,
        tile_size: LayoutSize,
        tile_spacing: LayoutSize,
    ) {
        let item = di::DisplayItem::RadialGradient(di::RadialGradientDisplayItem {
            common: *common,
            bounds,
            gradient,
            tile_size,
            tile_spacing,
        });

        self.push_item(&item);
    }

    pub fn push_reference_frame(
        &mut self,
        origin: LayoutPoint,
        parent_spatial_id: di::SpatialId,
        transform_style: di::TransformStyle,
        transform: PropertyBinding<LayoutTransform>,
        kind: di::ReferenceFrameKind,
    ) -> di::SpatialId {
        let id = self.generate_spatial_index();

        let item = di::DisplayItem::PushReferenceFrame(di::ReferenceFrameDisplayListItem {
            parent_spatial_id,
            origin,
            reference_frame: di::ReferenceFrame {
                transform_style,
                transform,
                kind,
                id,
            },
        });

        self.push_item(&item);
        id
    }

    pub fn pop_reference_frame(&mut self) {
        self.push_item(&di::DisplayItem::PopReferenceFrame);
    }

    pub fn push_stacking_context(
        &mut self,
        origin: LayoutPoint,
        spatial_id: di::SpatialId,
        is_backface_visible: bool,
        clip_id: Option<di::ClipId>,
        transform_style: di::TransformStyle,
        mix_blend_mode: di::MixBlendMode,
        filters: &[di::FilterOp],
        filter_datas: &[di::FilterData],
        filter_primitives: &[di::FilterPrimitive],
        raster_space: di::RasterSpace,
        cache_tiles: bool,
    ) {
        if filters.len() > 0 {
            self.push_item(&di::DisplayItem::SetFilterOps);
            self.push_iter(filters);
        }

        for filter_data in filter_datas {
            let func_types = [
                filter_data.func_r_type, filter_data.func_g_type,
                filter_data.func_b_type, filter_data.func_a_type];
            self.push_item(&di::DisplayItem::SetFilterData);
            self.push_iter(&func_types);
            self.push_iter(&filter_data.r_values);
            self.push_iter(&filter_data.g_values);
            self.push_iter(&filter_data.b_values);
            self.push_iter(&filter_data.a_values);
        }

        if !filter_primitives.is_empty() {
            self.push_item(&di::DisplayItem::SetFilterPrimitives);
            self.push_iter(filter_primitives);
        }

        let item = di::DisplayItem::PushStackingContext(di::PushStackingContextDisplayItem {
            origin,
            spatial_id,
            is_backface_visible,
            stacking_context: di::StackingContext {
                transform_style,
                mix_blend_mode,
                clip_id,
                raster_space,
                cache_tiles,
            },
        });

        self.push_item(&item);
    }

    /// Helper for examples/ code.
    pub fn push_simple_stacking_context(
        &mut self,
        origin: LayoutPoint,
        spatial_id: di::SpatialId,
        is_backface_visible: bool,
    ) {
        self.push_simple_stacking_context_with_filters(origin, spatial_id, is_backface_visible, &[], &[], &[]);
    }

    /// Helper for examples/ code.
    pub fn push_simple_stacking_context_with_filters(
        &mut self,
        origin: LayoutPoint,
        spatial_id: di::SpatialId,
        is_backface_visible: bool,
        filters: &[di::FilterOp],
        filter_datas: &[di::FilterData],
        filter_primitives: &[di::FilterPrimitive],
    ) {
        self.push_stacking_context(
            origin,
            spatial_id,
            is_backface_visible,
            None,
            di::TransformStyle::Flat,
            di::MixBlendMode::Normal,
            filters,
            filter_datas,
            filter_primitives,
            di::RasterSpace::Screen,
            /* cache_tiles = */ false,
        );
    }

    pub fn pop_stacking_context(&mut self) {
        self.push_item(&di::DisplayItem::PopStackingContext);
    }

    pub fn push_stops(&mut self, stops: &[di::GradientStop]) {
        if stops.is_empty() {
            return;
        }
        self.push_item(&di::DisplayItem::SetGradientStops);
        self.push_iter(stops);
    }

    fn generate_clip_index(&mut self) -> di::ClipId {
        self.next_clip_index += 1;
        di::ClipId::Clip(self.next_clip_index - 1, self.pipeline_id)
    }

    fn generate_spatial_index(&mut self) -> di::SpatialId {
        self.next_spatial_index += 1;
        di::SpatialId::new(self.next_spatial_index - 1, self.pipeline_id)
    }

    fn generate_clip_chain_id(&mut self) -> di::ClipChainId {
        self.next_clip_chain_id += 1;
        di::ClipChainId(self.next_clip_chain_id - 1, self.pipeline_id)
    }

    pub fn define_scroll_frame<I>(
        &mut self,
        parent_space_and_clip: &di::SpaceAndClipInfo,
        external_id: Option<di::ExternalScrollId>,
        content_rect: LayoutRect,
        clip_rect: LayoutRect,
        complex_clips: I,
        image_mask: Option<di::ImageMask>,
        scroll_sensitivity: di::ScrollSensitivity,
        external_scroll_offset: LayoutVector2D,
    ) -> di::SpaceAndClipInfo
    where
        I: IntoIterator<Item = di::ComplexClipRegion>,
        I::IntoIter: ExactSizeIterator + Clone,
    {
        let clip_id = self.generate_clip_index();
        let scroll_frame_id = self.generate_spatial_index();
        let item = di::DisplayItem::ScrollFrame(di::ScrollFrameDisplayItem {
            content_rect,
            clip_rect,
            parent_space_and_clip: *parent_space_and_clip,
            clip_id,
            scroll_frame_id,
            external_id,
            image_mask,
            scroll_sensitivity,
            external_scroll_offset,
        });

        self.push_item(&item);
        self.push_iter(complex_clips);

        di::SpaceAndClipInfo {
            spatial_id: scroll_frame_id,
            clip_id,
        }
    }

    pub fn define_clip_chain<I>(
        &mut self,
        parent: Option<di::ClipChainId>,
        clips: I,
    ) -> di::ClipChainId
    where
        I: IntoIterator<Item = di::ClipId>,
        I::IntoIter: ExactSizeIterator + Clone,
    {
        let id = self.generate_clip_chain_id();
        self.push_item(&di::DisplayItem::ClipChain(di::ClipChainItem { id, parent }));
        self.push_iter(clips);
        id
    }

    pub fn define_clip<I>(
        &mut self,
        parent_space_and_clip: &di::SpaceAndClipInfo,
        clip_rect: LayoutRect,
        complex_clips: I,
        image_mask: Option<di::ImageMask>,
    ) -> di::ClipId
    where
        I: IntoIterator<Item = di::ComplexClipRegion>,
        I::IntoIter: ExactSizeIterator + Clone,
    {
        let id = self.generate_clip_index();
        let item = di::DisplayItem::Clip(di::ClipDisplayItem {
            id,
            parent_space_and_clip: *parent_space_and_clip,
            clip_rect,
            image_mask,
        });

        self.push_item(&item);
        self.push_iter(complex_clips);
        id
    }

    pub fn define_sticky_frame(
        &mut self,
        parent_spatial_id: di::SpatialId,
        frame_rect: LayoutRect,
        margins: SideOffsets2D<Option<f32>, LayoutPixel>,
        vertical_offset_bounds: di::StickyOffsetBounds,
        horizontal_offset_bounds: di::StickyOffsetBounds,
        previously_applied_offset: LayoutVector2D,
    ) -> di::SpatialId {
        let id = self.generate_spatial_index();
        let item = di::DisplayItem::StickyFrame(di::StickyFrameDisplayItem {
            parent_spatial_id,
            id,
            bounds: frame_rect,
            margins,
            vertical_offset_bounds,
            horizontal_offset_bounds,
            previously_applied_offset,
        });

        self.push_item(&item);
        id
    }

    pub fn push_iframe(
        &mut self,
        bounds: LayoutRect,
        clip_rect: LayoutRect,
        space_and_clip: &di::SpaceAndClipInfo,
        pipeline_id: PipelineId,
        ignore_missing_pipeline: bool
    ) {
        let item = di::DisplayItem::Iframe(di::IframeDisplayItem {
            bounds,
            clip_rect,
            space_and_clip: *space_and_clip,
            pipeline_id,
            ignore_missing_pipeline,
        });
        self.push_item(&item);
    }

    pub fn push_shadow(
        &mut self,
        space_and_clip: &di::SpaceAndClipInfo,
        shadow: di::Shadow,
        should_inflate: bool,
    ) {
        let item = di::DisplayItem::PushShadow(di::PushShadowDisplayItem {
            space_and_clip: *space_and_clip,
            shadow,
            should_inflate,
        });
        self.push_item(&item);
    }

    pub fn pop_all_shadows(&mut self) {
        self.push_item(&di::DisplayItem::PopAllShadows);
    }

    pub fn finalize(mut self) -> (PipelineId, LayoutSize, BuiltDisplayList) {
        assert!(self.save_state.is_none(), "Finalized DisplayListBuilder with a pending save");

        // Add `DisplayItem::max_size` zone of zeroes to the end of display list
        // so there is at least this amount available in the display list during
        // serialization.
        ensure_red_zone::<di::DisplayItem>(&mut self.data);

        let end_time = precise_time_ns();

        (
            self.pipeline_id,
            self.content_size,
            BuiltDisplayList {
                descriptor: BuiltDisplayListDescriptor {
                    builder_start_time: self.builder_start_time,
                    builder_finish_time: end_time,
                    send_start_time: 0,
                    total_clip_nodes: self.next_clip_index,
                    total_spatial_nodes: self.next_spatial_index,
                },
                data: self.data,
            },
        )
    }
}
