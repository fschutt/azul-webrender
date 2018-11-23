/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![deny(missing_docs)]

extern crate serde_bytes;

use font::{FontInstanceKey, FontInstanceData, FontKey, FontTemplate};
use std::sync::Arc;
use {DeviceIntPoint, DeviceIntRect, DeviceIntSize, LayoutIntRect};
use {BlobDirtyRect, IdNamespace, TileOffset, TileSize};
use euclid::{size2, TypedRect, num::Zero};
use std::ops::{Add, Sub};

/// An opaque identifier describing an image registered with WebRender.
/// This is used as a handle to reference images, and is used as the
/// hash map key for the actual image storage in the `ResourceCache`.
#[repr(C)]
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ImageKey(pub IdNamespace, pub u32);

impl ImageKey {
    /// Placeholder Image key, used to represent None.
    pub const DUMMY: Self = ImageKey(IdNamespace(0), 0);

    /// Mints a new ImageKey. The given ID must be unique.
    pub fn new(namespace: IdNamespace, key: u32) -> Self {
        ImageKey(namespace, key)
    }
}

/// An opaque identifier describing a blob image registered with WebRender.
/// This is used as a handle to reference blob images, and can be used as an
/// image in display items.
#[repr(C)]
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct BlobImageKey(pub ImageKey);

impl BlobImageKey {
    /// Interpret this blob image as an image for a display item.
    pub fn as_image(&self) -> ImageKey {
        self.0
    }
}

/// An arbitrary identifier for an external image provided by the
/// application. It must be a unique identifier for each external
/// image.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ExternalImageId(pub u64);

/// Specifies the type of texture target in driver terms.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum TextureTarget {
    /// Standard texture. This maps to GL_TEXTURE_2D in OpenGL.
    Default = 0,
    /// Array texture. This maps to GL_TEXTURE_2D_ARRAY in OpenGL. See
    /// https://www.khronos.org/opengl/wiki/Array_Texture for background
    /// on Array textures.
    Array = 1,
    /// Rectange texture. This maps to GL_TEXTURE_RECTANGLE in OpenGL. This
    /// is similar to a standard texture, with a few subtle differences
    /// (no mipmaps, non-power-of-two dimensions, different coordinate space)
    /// that make it useful for representing the kinds of textures we use
    /// in WebRender. See https://www.khronos.org/opengl/wiki/Rectangle_Texture
    /// for background on Rectangle textures.
    Rect = 2,
    /// External texture. This maps to GL_TEXTURE_EXTERNAL_OES in OpenGL, which
    /// is an extension. This is used for image formats that OpenGL doesn't
    /// understand, particularly YUV. See
    /// https://www.khronos.org/registry/OpenGL/extensions/OES/OES_EGL_image_external.txt
    External = 3,
}

/// Storage format identifier for externally-managed images.
#[repr(u32)]
#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ExternalImageType {
    /// The image is texture-backed.
    TextureHandle(TextureTarget),
    /// The image is heap-allocated by the embedding.
    Buffer,
}

/// Descriptor for external image resources. See `ImageData`.
#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ExternalImageData {
    /// The identifier of this external image, provided by the embedding.
    pub id: ExternalImageId,
    /// For multi-plane images (i.e. YUV), indicates the plane of the
    /// original image that this struct represents. 0 for single-plane images.
    pub channel_index: u8,
    /// Storage format identifier.
    pub image_type: ExternalImageType,
}

/// Specifies the format of a series of pixels, in driver terms.
#[repr(u32)]
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum ImageFormat {
    /// One-channel, byte storage. The "red" doesn't map to the color
    /// red per se, and is just the way that OpenGL has historically referred
    /// to single-channel buffers.
    R8 = 1,
    /// One-channel, short storage
    R16 = 2,
    /// Four channels, byte storage.
    BGRA8 = 3,
    /// Four channels, float storage.
    RGBAF32 = 4,
    /// Two-channels, byte storage. Similar to `R8`, this just means
    /// "two channels" rather than "red and green".
    RG8 = 5,
    /// Four channels, signed integer storage.
    RGBAI32 = 6,
}

impl ImageFormat {
    /// Returns the number of bytes per pixel for the given format.
    pub fn bytes_per_pixel(self) -> i32 {
        match self {
            ImageFormat::R8 => 1,
            ImageFormat::R16 => 2,
            ImageFormat::BGRA8 => 4,
            ImageFormat::RGBAF32 => 16,
            ImageFormat::RG8 => 2,
            ImageFormat::RGBAI32 => 16,
        }
    }
}

/// Specifies the color depth of an image. Currently only used for YUV images.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum ColorDepth {
    /// 8 bits image (most common)
    Color8,
    /// 10 bits image
    Color10,
    /// 12 bits image
    Color12,
    /// 16 bits image
    Color16,
}

impl ColorDepth {
    /// Return the numerical bit depth value for the type.
    pub fn bit_depth(self) -> u32 {
        match self {
            ColorDepth::Color8 => 8,
            ColorDepth::Color10 => 10,
            ColorDepth::Color12 => 12,
            ColorDepth::Color16 => 16,
        }
    }
    /// 10 and 12 bits images are encoded using 16 bits integer, we need to
    /// rescale the 10 or 12 bits value to extend to 16 bits.
    pub fn rescaling_factor(self) -> f32 {
        match self {
            ColorDepth::Color8 => 1.0,
            ColorDepth::Color10 => 64.0,
            ColorDepth::Color12 => 16.0,
            ColorDepth::Color16 => 1.0,
        }
    }
}

/// Metadata (but not storage) describing an image In WebRender.
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ImageDescriptor {
    /// Format of the image data.
    pub format: ImageFormat,
    /// Width and length of the image data, in pixels.
    pub size: DeviceIntSize,
    /// The number of bytes from the start of one row to the next. If non-None,
    /// `compute_stride` will return this value, otherwise it returns
    /// `width * bpp`. Different source of images have different alignment
    /// constraints for rows, so the stride isn't always equal to width * bpp.
    pub stride: Option<i32>,
    /// Offset in bytes of the first pixel of this image in its backing buffer.
    /// This is used for tiling, wherein WebRender extracts chunks of input images
    /// in order to cache, manipulate, and render them individually. This offset
    /// tells the texture upload machinery where to find the bytes to upload for
    /// this tile. Non-tiled images generally set this to zero.
    pub offset: i32,
    /// Whether this image is opaque, or has an alpha channel. Avoiding blending
    /// for opaque surfaces is an important optimization.
    pub is_opaque: bool,
    /// Whether to allow the driver to automatically generate mipmaps. If images
    /// are already downscaled appropriately, mipmap generation can be wasted
    /// work, and cause performance problems on some cards/drivers.
    ///
    /// See https://github.com/servo/webrender/pull/2555/
    pub allow_mipmaps: bool,
}

impl ImageDescriptor {
    /// Mints a new ImageDescriptor.
    pub fn new(
        width: i32,
        height: i32,
        format: ImageFormat,
        is_opaque: bool,
        allow_mipmaps: bool,
    ) -> Self {
        ImageDescriptor {
            size: size2(width, height),
            format,
            stride: None,
            offset: 0,
            is_opaque,
            allow_mipmaps,
        }
    }

    /// Returns the stride, either via an explicit stride stashed on the object
    /// or by the default computation.
    pub fn compute_stride(&self) -> i32 {
        self.stride.unwrap_or(self.size.width * self.format.bytes_per_pixel())
    }

    /// Computes the total size of the image, in bytes.
    pub fn compute_total_size(&self) -> i32 {
        self.compute_stride() * self.size.height
    }

    /// Computes the bounding rectangle for the image, rooted at (0, 0).
    pub fn full_rect(&self) -> DeviceIntRect {
        DeviceIntRect::new(
            DeviceIntPoint::zero(),
            self.size,
        )
    }
}

/// Represents the backing store of an arbitrary series of pixels for display by
/// WebRender. This storage can take several forms.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ImageData {
    /// A simple series of bytes, provided by the embedding and owned by WebRender.
    /// The format is stored out-of-band, currently in ImageDescriptor.
    Raw(#[serde(with = "serde_image_data_raw")] Arc<Vec<u8>>),
    /// An image owned by the embedding, and referenced by WebRender. This may
    /// take the form of a texture or a heap-allocated buffer.
    External(ExternalImageData),
}

mod serde_image_data_raw {
    extern crate serde_bytes;

    use std::sync::Arc;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(bytes: &Arc<Vec<u8>>, serializer: S) -> Result<S::Ok, S::Error> {
        serde_bytes::serialize(bytes.as_slice(), serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Arc<Vec<u8>>, D::Error> {
        serde_bytes::deserialize(deserializer).map(Arc::new)
    }
}

impl ImageData {
    /// Mints a new raw ImageData, taking ownership of the bytes.
    pub fn new(bytes: Vec<u8>) -> Self {
        ImageData::Raw(Arc::new(bytes))
    }

    /// Mints a new raw ImageData from Arc-ed bytes.
    pub fn new_shared(bytes: Arc<Vec<u8>>) -> Self {
        ImageData::Raw(bytes)
    }
}

/// The resources exposed by the resource cache available for use by the blob rasterizer.
pub trait BlobImageResources {
    /// Returns the `FontTemplate` for the given key.
    fn get_font_data(&self, key: FontKey) -> &FontTemplate;
    /// Returns the `FontInstanceData` for the given key, if found.
    fn get_font_instance_data(&self, key: FontInstanceKey) -> Option<FontInstanceData>;
}

/// A handler on the render backend that can create rasterizer objects which will
/// be sent to the scene builder thread to execute the rasterization.
///
/// The handler is responsible for collecting resources, managing/updating blob commands
/// and creating the rasterizer objects, but isn't expected to do any rasterization itself.
pub trait BlobImageHandler: Send {
    /// Creates a snapshot of the current state of blob images in the handler.
    fn create_blob_rasterizer(&mut self) -> Box<AsyncBlobImageRasterizer>;

    /// A hook to let the blob image handler update any state related to resources that
    /// are not bundled in the blob recording itself.
    fn prepare_resources(
        &mut self,
        services: &BlobImageResources,
        requests: &[BlobImageParams],
    );

    /// Register a blob image.
    fn add(&mut self, key: BlobImageKey, data: Arc<BlobImageData>, tiling: Option<TileSize>);

    /// Update an already registered blob image.
    fn update(&mut self, key: BlobImageKey, data: Arc<BlobImageData>, dirty_rect: &BlobDirtyRect);

    /// Delete an already registered blob image.
    fn delete(&mut self, key: BlobImageKey);

    /// A hook to let the handler clean up any state related to a font which the resource
    /// cache is about to delete.
    fn delete_font(&mut self, key: FontKey);

    /// A hook to let the handler clean up any state related to a font instance which the
    /// resource cache is about to delete.
    fn delete_font_instance(&mut self, key: FontInstanceKey);

    /// A hook to let the handler clean up any state related a given namespace before the
    /// resource cache deletes them.
    fn clear_namespace(&mut self, namespace: IdNamespace);
}

/// A group of rasterization requests to execute synchronously on the scene builder thread.
pub trait AsyncBlobImageRasterizer : Send {
    /// Rasterize the requests.
    ///
    /// Gecko uses te priority hint to schedule work in a way that minimizes the risk
    /// of high priority work being blocked by (or enqued behind) low priority work.
    fn rasterize(
        &mut self,
        requests: &[BlobImageParams],
        low_priority: bool
    ) -> Vec<(BlobImageRequest, BlobImageResult)>;
}


/// Input parameters for the BlobImageRasterizer.
#[derive(Copy, Clone, Debug)]
pub struct BlobImageParams {
    /// A key that identifies the blob image rasterization request.
    pub request: BlobImageRequest,
    /// Description of the format of the blob's output image.
    pub descriptor: BlobImageDescriptor,
    /// An optional sub-rectangle of the image to avoid re-rasterizing
    /// the entire image when only a portion is updated.
    ///
    /// If set to None the entire image is rasterized.
    pub dirty_rect: BlobDirtyRect,
}

/// The possible states of a Dirty rect.
///
/// This exists because people kept getting confused with `Option<Rect>`.
#[derive(Debug, Serialize, Deserialize)]
pub enum DirtyRect<T: Copy, U> {
    /// Everything is Dirty, equivalent to Partial(image_bounds)
    All,
    /// Some specific amount is dirty
    Partial(TypedRect<T, U>)
}

impl<T, U> DirtyRect<T, U>
where
    T: Copy + Clone
        + PartialOrd + PartialEq
        + Add<T, Output = T>
        + Sub<T, Output = T>
        + Zero
{
    /// Creates an empty DirtyRect (indicating nothing is invalid)
    pub fn empty() -> Self {
        DirtyRect::Partial(TypedRect::zero())
    }

    /// Returns whether the dirty rect is empty
    pub fn is_empty(&self) -> bool {
        match self {
            DirtyRect::All => false,
            DirtyRect::Partial(rect) => rect.is_empty(),
        }
    }

    /// Replaces self with the empty rect and returns the old value.
    pub fn replace_with_empty(&mut self) -> Self {
        ::std::mem::replace(self, DirtyRect::empty())
    }

    /// Maps over the contents of Partial.
    pub fn map<F>(self, func: F) -> Self
        where F: FnOnce(TypedRect<T, U>) -> TypedRect<T, U>,
    {
        use DirtyRect::*;

        match self {
            All        => All,
            Partial(rect) => Partial(func(rect)),
        }
    }

    /// Unions the dirty rects.
    pub fn union(&self, other: &Self) -> Self {
        use DirtyRect::*;

        match (*self, *other) {
            (All, _) | (_, All)        => All,
            (Partial(rect1), Partial(rect2)) => Partial(rect1.union(&rect2)),
        }
    }

    /// Intersects the dirty rects.
    pub fn intersection(&self, other: &Self) -> Self {
        use DirtyRect::*;

        match (*self, *other) {
            (All, rect) | (rect, All)  => rect,
            (Partial(rect1), Partial(rect2)) => Partial(rect1.intersection(&rect2)
                                                                   .unwrap_or(TypedRect::zero()))
        }
    }

    /// Converts the dirty rect into a subrect of the given one via intersection.
    pub fn to_subrect_of(&self, rect: &TypedRect<T, U>) -> TypedRect<T, U> {
        use DirtyRect::*;

        match *self {
            All              => *rect,
            Partial(dirty_rect) => dirty_rect.intersection(rect)
                                               .unwrap_or(TypedRect::zero()),
        }
    }
}

impl<T: Copy, U> Copy for DirtyRect<T, U> {}
impl<T: Copy, U> Clone for DirtyRect<T, U> {
    fn clone(&self) -> Self { *self }
}

impl<T: Copy, U> From<TypedRect<T, U>> for DirtyRect<T, U> {
    fn from(rect: TypedRect<T, U>) -> Self {
        DirtyRect::Partial(rect)
    }
}

/// Backing store for blob image command streams.
pub type BlobImageData = Vec<u8>;

/// Result type for blob raserization.
pub type BlobImageResult = Result<RasterizedBlobImage, BlobImageError>;

/// Metadata (but not storage) for a blob image.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct BlobImageDescriptor {
    /// Surface of the image or tile to render in the same coordinate space as
    /// the drawing commands.
    pub rect: LayoutIntRect,
    /// Format for the data in the backing store.
    pub format: ImageFormat,
}

/// Representation of a rasterized blob image. This is obtained by passing
/// `BlobImageData` to the embedding via the rasterization callback.
pub struct RasterizedBlobImage {
    /// The rectangle that was rasterized in device pixels, relative to the
    /// image or tile.
    pub rasterized_rect: DeviceIntRect,
    /// Backing store. The format is stored out of band in `BlobImageDescriptor`.
    pub data: Arc<Vec<u8>>,
}

/// Error code for when blob rasterization failed.
#[derive(Clone, Debug)]
pub enum BlobImageError {
    /// Out of memory.
    Oom,
    /// Other failure, embedding-specified.
    Other(String),
}



/// A key identifying blob image rasterization work requested from the blob
/// image rasterizer.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlobImageRequest {
    /// Unique handle to the image.
    pub key: BlobImageKey,
    /// Tiling offset in number of tiles, if applicable.
    ///
    /// `None` if the image will not be tiled.
    pub tile: Option<TileOffset>,
}
