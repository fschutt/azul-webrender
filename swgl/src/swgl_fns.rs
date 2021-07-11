/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
#![allow(unused_variables)]

use gl_context_loader::*;
use gl_context_loader::gl::*;
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_void};
use std::ptr;
use std::str;

pub type GLdouble = gl_context_loader::c_double;

#[allow(unused)]
macro_rules! debug {
    ($($x:tt)*) => {};
}

#[repr(C)]
struct LockedTexture {
    _private: [u8; 0],
}

#[allow(dead_code)]
extern "C" {
    fn ActiveTexture(texture: GLenum);
    fn BindTexture(target: GLenum, texture: GLuint);
    fn BindBuffer(target: GLenum, buffer: GLuint);
    fn BindVertexArray(vao: GLuint);
    fn BindFramebuffer(target: GLenum, fb: GLuint);
    fn BindRenderbuffer(target: GLenum, rb: GLuint);
    fn BlendFunc(srgb: GLenum, drgb: GLenum, sa: GLenum, da: GLenum);
    fn BlendColor(r: GLfloat, g: GLfloat, b: GLfloat, a: GLfloat);
    fn BlendEquation(mode: GLenum);
    fn Enable(cap: GLenum);
    fn Disable(cap: GLenum);
    fn GenQueries(n: GLsizei, result: *mut GLuint);
    fn BeginQuery(target: GLenum, id: GLuint);
    fn EndQuery(target: GLenum);
    fn GetQueryObjectui64v(id: GLuint, pname: GLenum, params: *mut GLuint64);
    fn GenBuffers(n: i32, result: *mut GLuint);
    fn GenTextures(n: i32, result: *mut GLuint);
    fn GenFramebuffers(n: i32, result: *mut GLuint);
    fn GenRenderbuffers(n: i32, result: *mut GLuint);
    fn BufferData(target: GLenum, size: GLsizeiptr, data: *const GLvoid, usage: GLenum);
    fn BufferSubData(target: GLenum, offset: GLintptr, size: GLsizeiptr, data: *const GLvoid);
    fn MapBuffer(target: GLenum, access: GLbitfield) -> *mut c_void;
    fn MapBufferRange(
        target: GLenum,
        offset: GLintptr,
        length: GLsizeiptr,
        access: GLbitfield,
    ) -> *mut c_void;
    fn UnmapBuffer(target: GLenum) -> GLboolean;
    fn TexStorage2D(
        target: GLenum,
        levels: GLint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
    );
    fn FramebufferTexture2D(
        target: GLenum,
        attachment: GLenum,
        textarget: GLenum,
        texture: GLuint,
        level: GLint,
    );
    fn CheckFramebufferStatus(target: GLenum) -> GLenum;
    fn InvalidateFramebuffer(target: GLenum, num_attachments: GLsizei, attachments: *const GLenum);
    fn TexImage2D(
        target: GLenum,
        level: GLint,
        internal_format: GLint,
        width: GLsizei,
        height: GLsizei,
        border: GLint,
        format: GLenum,
        ty: GLenum,
        data: *const c_void,
    );
    fn TexSubImage2D(
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        ty: GLenum,
        data: *const c_void,
    );
    fn GenerateMipmap(target: GLenum);
    fn GetUniformLocation(program: GLuint, name: *const GLchar) -> GLint;
    fn BindAttribLocation(program: GLuint, index: GLuint, name: *const GLchar);
    fn GetAttribLocation(program: GLuint, name: *const GLchar) -> GLint;
    fn GenVertexArrays(n: i32, result: *mut GLuint);
    fn VertexAttribPointer(
        index: GLuint,
        size: GLint,
        type_: GLenum,
        normalized: GLboolean,
        stride: GLsizei,
        offset: *const GLvoid,
    );
    fn VertexAttribIPointer(
        index: GLuint,
        size: GLint,
        type_: GLenum,
        stride: GLsizei,
        offset: *const GLvoid,
    );
    fn CreateShader(shader_type: GLenum) -> GLuint;
    fn AttachShader(program: GLuint, shader: GLuint);
    fn CreateProgram() -> GLuint;
    fn Uniform1i(location: GLint, v0: GLint);
    fn Uniform4fv(location: GLint, count: GLsizei, value: *const GLfloat);
    fn UniformMatrix4fv(
        location: GLint,
        count: GLsizei,
        transpose: GLboolean,
        value: *const GLfloat,
    );
    fn DrawElementsInstanced(
        mode: GLenum,
        count: GLsizei,
        type_: GLenum,
        indices: GLintptr,
        instancecount: GLsizei,
    );
    fn EnableVertexAttribArray(index: GLuint);
    fn VertexAttribDivisor(index: GLuint, divisor: GLuint);
    fn LinkProgram(program: GLuint);
    fn GetLinkStatus(program: GLuint) -> GLint;
    fn UseProgram(program: GLuint);
    fn SetViewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei);
    fn FramebufferRenderbuffer(
        target: GLenum,
        attachment: GLenum,
        renderbuffertarget: GLenum,
        renderbuffer: GLuint,
    );
    fn RenderbufferStorage(target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei);
    fn DepthMask(flag: GLboolean);
    fn DepthFunc(func: GLenum);
    fn SetScissor(x: GLint, y: GLint, width: GLsizei, height: GLsizei);
    fn ClearColor(r: GLfloat, g: GLfloat, b: GLfloat, a: GLfloat);
    fn ClearDepth(depth: GLdouble);
    fn Clear(mask: GLbitfield);
    fn ClearTexSubImage(
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        zoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        depth: GLsizei,
        format: GLenum,
        ty: GLenum,
        data: *const c_void,
    );
    fn ClearTexImage(target: GLenum, level: GLint, format: GLenum, ty: GLenum, data: *const c_void);
    fn ClearColorRect(
        fbo: GLuint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        r: GLfloat,
        g: GLfloat,
        b: GLfloat,
        a: GLfloat,
    );
    fn PixelStorei(name: GLenum, param: GLint);
    fn ReadPixels(
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        ty: GLenum,
        data: *mut c_void,
    );
    fn Finish();
    fn ShaderSourceByName(shader: GLuint, name: *const GLchar);
    fn TexParameteri(target: GLenum, pname: GLenum, param: GLint);
    fn CopyImageSubData(
        src_name: GLuint,
        src_target: GLenum,
        src_level: GLint,
        src_x: GLint,
        src_y: GLint,
        src_z: GLint,
        dst_name: GLuint,
        dst_target: GLenum,
        dst_level: GLint,
        dst_x: GLint,
        dst_y: GLint,
        dst_z: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        src_depth: GLsizei,
    );
    fn CopyTexSubImage2D(
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
    );
    fn BlitFramebuffer(
        src_x0: GLint,
        src_y0: GLint,
        src_x1: GLint,
        src_y1: GLint,
        dst_x0: GLint,
        dst_y0: GLint,
        dst_x1: GLint,
        dst_y1: GLint,
        mask: GLbitfield,
        filter: GLenum,
    );
    fn GetIntegerv(pname: GLenum, params: *mut GLint);
    fn GetBooleanv(pname: GLenum, params: *mut GLboolean);
    fn GetString(name: GLenum) -> *const c_char;
    fn GetStringi(name: GLenum, index: GLuint) -> *const c_char;
    fn GetError() -> GLenum;
    fn InitDefaultFramebuffer(
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        stride: i32,
        buf: *mut c_void,
    );
    fn GetColorBuffer(
        fbo: GLuint,
        flush: GLboolean,
        width: *mut i32,
        height: *mut i32,
        stride: *mut i32,
    ) -> *mut c_void;
    fn ResolveFramebuffer(fbo: GLuint);
    fn SetTextureBuffer(
        tex: GLuint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
        stride: GLsizei,
        buf: *mut c_void,
        min_width: GLsizei,
        min_height: GLsizei,
    );
    fn SetTextureParameter(tex: GLuint, pname: GLenum, param: GLint);
    fn DeleteTexture(n: GLuint);
    fn DeleteRenderbuffer(n: GLuint);
    fn DeleteFramebuffer(n: GLuint);
    fn DeleteBuffer(n: GLuint);
    fn DeleteVertexArray(n: GLuint);
    fn DeleteQuery(n: GLuint);
    fn DeleteShader(shader: GLuint);
    fn DeleteProgram(program: GLuint);
    fn LockFramebuffer(fbo: GLuint) -> *mut LockedTexture;
    fn LockTexture(tex: GLuint) -> *mut LockedTexture;
    fn LockResource(resource: *mut LockedTexture);
    fn UnlockResource(resource: *mut LockedTexture);
    fn GetResourceBuffer(
        resource: *mut LockedTexture,
        width: *mut i32,
        height: *mut i32,
        stride: *mut i32,
    ) -> *mut c_void;
    fn Composite(
        locked_dst: *mut LockedTexture,
        locked_src: *mut LockedTexture,
        src_x: GLint,
        src_y: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        dst_x: GLint,
        dst_y: GLint,
        dst_width: GLsizei,
        dst_height: GLsizei,
        opaque: GLboolean,
        flip: GLboolean,
        filter: GLenum,
        clip_x: GLint,
        clip_y: GLint,
        clip_width: GLsizei,
        clip_height: GLsizei,
    );
    fn CompositeYUV(
        locked_dst: *mut LockedTexture,
        locked_y: *mut LockedTexture,
        locked_u: *mut LockedTexture,
        locked_v: *mut LockedTexture,
        color_space: YuvRangedColorSpace,
        color_depth: GLuint,
        src_x: GLint,
        src_y: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        dst_x: GLint,
        dst_y: GLint,
        dst_width: GLsizei,
        dst_height: GLsizei,
        flip: GLboolean,
        clip_x: GLint,
        clip_y: GLint,
        clip_width: GLsizei,
        clip_height: GLsizei,
    );
    fn CreateContext() -> *mut c_void;
    fn ReferenceContext(ctx: *mut c_void);
    fn DestroyContext(ctx: *mut c_void);
    fn MakeCurrent(ctx: *mut c_void);
    fn ReportMemory(ctx: *mut c_void, size_of_op: unsafe extern "C" fn(ptr: *const c_void) -> usize) -> usize;
}

#[derive(Clone, Copy)]
pub struct Context(*mut c_void);

impl Context {
    pub fn create() -> Self {
        Context(unsafe { CreateContext() })
    }

    pub fn reference(&self) {
        unsafe {
            ReferenceContext(self.0);
        }
    }

    pub fn destroy(&self) {
        unsafe {
            DestroyContext(self.0);
        }
    }

    pub fn make_current(&self) {
        unsafe {
            MakeCurrent(self.0);
        }
    }

    pub fn init_default_framebuffer(
        &self,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        stride: i32,
        buf: *mut c_void,
    ) {
        unsafe {
            InitDefaultFramebuffer(x, y, width, height, stride, buf);
        }
    }

    pub fn get_color_buffer(&self, fbo: GLuint, flush: bool) -> (*mut c_void, i32, i32, i32) {
        unsafe {
            let mut width: i32 = 0;
            let mut height: i32 = 0;
            let mut stride: i32 = 0;
            let data_ptr = GetColorBuffer(
                fbo,
                flush as GLboolean,
                &mut width,
                &mut height,
                &mut stride,
            );
            (data_ptr, width, height, stride)
        }
    }

    pub fn resolve_framebuffer(&self, fbo: GLuint) {
        unsafe {
            ResolveFramebuffer(fbo);
        }
    }

    pub fn clear_color_rect(
        &self,
        fbo: GLuint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        r: f32,
        g: f32,
        b: f32,
        a: f32,
    ) {
        unsafe {
            ClearColorRect(fbo, xoffset, yoffset, width, height, r, g, b, a);
        }
    }

    pub fn set_texture_buffer(
        &self,
        tex: GLuint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
        stride: GLsizei,
        buf: *mut c_void,
        min_width: GLsizei,
        min_height: GLsizei,
    ) {
        unsafe {
            SetTextureBuffer(
                tex,
                internal_format,
                width,
                height,
                stride,
                buf,
                min_width,
                min_height,
            );
        }
    }

    pub fn set_texture_parameter(&self, tex: GLuint, pname: GLenum, param: GLint) {
        unsafe {
            SetTextureParameter(tex, pname, param);
        }
    }

    pub fn lock_framebuffer(&self, fbo: GLuint) -> Option<LockedResource> {
        unsafe {
            let resource = LockFramebuffer(fbo);
            if resource != ptr::null_mut() {
                Some(LockedResource(resource))
            } else {
                None
            }
        }
    }

    pub fn lock_texture(&self, tex: GLuint) -> Option<LockedResource> {
        unsafe {
            let resource = LockTexture(tex);
            if resource != ptr::null_mut() {
                Some(LockedResource(resource))
            } else {
                None
            }
        }
    }

    pub fn report_memory(&self, size_of_op: unsafe extern "C" fn(ptr: *const c_void) -> usize) -> usize {
        unsafe { ReportMemory(self.0, size_of_op) }
    }
}

impl From<*mut c_void> for Context {
    fn from(ptr: *mut c_void) -> Self {
        Context(ptr)
    }
}

impl From<Context> for *mut c_void {
    fn from(ctx: Context) -> Self {
        ctx.0
    }
}

fn calculate_length(width: GLsizei, height: GLsizei, format: GLenum, pixel_type: GLenum) -> usize {
    let colors = match format {
        RED => 1,
        RGB => 3,
        BGR => 3,

        RGBA => 4,
        BGRA => 4,

        ALPHA => 1,
        R16 => 1,
        LUMINANCE => 1,
        DEPTH_COMPONENT => 1,
        _ => panic!("unsupported format for read_pixels: {:?}", format),
    };
    let depth = match pixel_type {
        UNSIGNED_BYTE => 1,
        UNSIGNED_SHORT => 2,
        SHORT => 2,
        FLOAT => 4,
        UNSIGNED_INT_8_8_8_8_REV => 1,
        _ => panic!("unsupported pixel_type for read_pixels: {:?}", pixel_type),
    };

    return (width * height * colors * depth) as usize;
}

impl Context {
    pub fn get_type(&self) -> GlType {
        GlType::Gl
    }

    pub fn buffer_data_untyped(
        &self,
        target: GLenum,
        size: GLsizeiptr,
        data: *const GLvoid,
        usage: GLenum,
    ) {
        debug!(
            "buffer_data_untyped {} {} {:?} {}",
            target, size, data, usage
        );
        //panic!();
        unsafe {
            BufferData(target, size, data, usage);
        }
    }

    pub fn buffer_sub_data_untyped(
        &self,
        target: GLenum,
        offset: isize,
        size: GLsizeiptr,
        data: *const GLvoid,
    ) {
        debug!(
            "buffer_sub_data_untyped {} {} {} {:?}",
            target, offset, size, data
        );
        //panic!();
        unsafe {
            BufferSubData(target, offset, size, data);
        }
    }

    pub fn map_buffer(&self, target: GLenum, access: GLbitfield) -> *mut c_void {
        unsafe { MapBuffer(target, access) }
    }

    pub fn map_buffer_range(
        &self,
        target: GLenum,
        offset: GLintptr,
        length: GLsizeiptr,
        access: GLbitfield,
    ) -> *mut c_void {
        unsafe { MapBufferRange(target, offset, length, access) }
    }

    pub fn unmap_buffer(&self, target: GLenum) -> GLboolean {
        unsafe { UnmapBuffer(target) }
    }

    pub fn shader_source(&self, shader: GLuint, strings: &[&[u8]]) {
        //panic!();
        debug!("shader_source {}", shader);
        //for s in strings {
        //    debug!("{}", str::from_utf8(s).unwrap());
        //}
        //panic!();
        for s in strings {
            let u = str::from_utf8(s).unwrap();
            const PREFIX: &'static str = "// shader: ";
            if let Some(start) = u.find(PREFIX) {
                if let Some(end) = u[start..].find('\n') {
                    let name = u[start + PREFIX.len()..start + end].trim();
                    debug!("shader name: {}", name);
                    unsafe {
                        let c_string = CString::new(name).unwrap();
                        ShaderSourceByName(shader, c_string.as_ptr());
                        return;
                    }
                }
            }
        }
        panic!("unknown shader");
    }

    pub fn tex_buffer(&self, target: GLenum, internal_format: GLenum, buffer: GLuint) {
        panic!();
    }

    pub fn read_buffer(&self, mode: GLenum) {
        panic!();
    }

    pub fn read_pixels_into_buffer(
        &self,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        pixel_type: GLenum,
        dst_buffer: &mut [u8],
    ) {
        // Assumes that the user properly allocated the size for dst_buffer.
        assert!(calculate_length(width, height, format, pixel_type) == dst_buffer.len());

        unsafe {
            ReadPixels(
                x,
                y,
                width,
                height,
                format,
                pixel_type,
                dst_buffer.as_mut_ptr() as *mut c_void,
            );
        }
    }

    pub fn read_pixels(
        &self,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        pixel_type: GLenum,
    ) -> Vec<u8> {
        let len = calculate_length(width, height, format, pixel_type);
        let mut pixels: Vec<u8> = Vec::new();
        pixels.reserve(len);
        unsafe {
            pixels.set_len(len);
        }

        self.read_pixels_into_buffer(
            x,
            y,
            width,
            height,
            format,
            pixel_type,
            pixels.as_mut_slice(),
        );

        pixels
    }

    pub unsafe fn read_pixels_into_pbo(
        &self,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        pixel_type: GLenum,
    ) {
        ReadPixels(x, y, width, height, format, pixel_type, ptr::null_mut());
    }

    pub fn sample_coverage(&self, value: GLclampf, invert: bool) {
        panic!();
    }

    pub fn polygon_offset(&self, factor: GLfloat, units: GLfloat) {
        panic!();
    }

    pub fn pixel_store_i(&self, name: GLenum, param: GLint) {
        //panic!();
        debug!("pixel_store_i {:x} {}", name, param);
        unsafe {
            PixelStorei(name, param);
        }
    }

    pub fn gen_buffers(&self, n: GLsizei) -> Vec<GLuint> {
        //panic!();
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenBuffers(n, result.as_mut_ptr());
        }
        result
    }

    pub fn gen_renderbuffers(&self, n: GLsizei) -> Vec<GLuint> {
        debug!("gen_renderbuffers {}", n);
        //panic!();
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenRenderbuffers(n, result.as_mut_ptr());
        }
        result
    }

    pub fn gen_framebuffers(&self, n: GLsizei) -> Vec<GLuint> {
        //panic!();
        debug!("gen_framebuffers {}", n);
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenFramebuffers(n, result.as_mut_ptr());
        }
        result
    }

    pub fn gen_textures(&self, n: GLsizei) -> Vec<GLuint> {
        //panic!();
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenTextures(n, result.as_mut_ptr());
        }
        result
    }

    pub fn gen_vertex_arrays(&self, n: GLsizei) -> Vec<GLuint> {
        //panic!();
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenVertexArrays(n, result.as_mut_ptr());
        }
        result
    }

    pub fn gen_vertex_arrays_apple(&self, n: GLsizei) -> Vec<GLuint> {
        self.gen_vertex_arrays(n)
    }

    pub fn gen_queries(&self, n: GLsizei) -> Vec<GLuint> {
        let mut result = vec![0 as GLuint; n as usize];
        unsafe {
            GenQueries(n, result.as_mut_ptr());
        }
        result
    }

    pub fn begin_query(&self, target: GLenum, id: GLuint) {
        unsafe {
            BeginQuery(target, id);
        }
    }

    pub fn end_query(&self, target: GLenum) {
        unsafe {
            EndQuery(target);
        }
    }

    pub fn query_counter(&self, id: GLuint, target: GLenum) {
        panic!();
    }

    pub fn get_query_object_iv(&self, id: GLuint, pname: GLenum) -> i32 {
        panic!();
        //0
    }

    pub fn get_query_object_uiv(&self, id: GLuint, pname: GLenum) -> u32 {
        panic!();
        //0
    }

    pub fn get_query_object_i64v(&self, id: GLuint, pname: GLenum) -> i64 {
        panic!();
        //0
    }

    pub fn get_query_object_ui64v(&self, id: GLuint, pname: GLenum) -> u64 {
        let mut result = 0;
        unsafe {
            GetQueryObjectui64v(id, pname, &mut result);
        }
        result
    }

    pub fn delete_queries(&self, queries: &[GLuint]) {
        unsafe {
            for q in queries {
                DeleteQuery(*q);
            }
        }
    }

    pub fn delete_vertex_arrays(&self, vertex_arrays: &[GLuint]) {
        unsafe {
            for v in vertex_arrays {
                DeleteVertexArray(*v);
            }
        }
    }

    pub fn delete_vertex_arrays_apple(&self, vertex_arrays: &[GLuint]) {
        self.delete_vertex_arrays(vertex_arrays)
    }

    pub fn delete_buffers(&self, buffers: &[GLuint]) {
        unsafe {
            for b in buffers {
                DeleteBuffer(*b);
            }
        }
    }

    pub fn delete_renderbuffers(&self, renderbuffers: &[GLuint]) {
        unsafe {
            for r in renderbuffers {
                DeleteRenderbuffer(*r);
            }
        }
    }

    pub fn delete_framebuffers(&self, framebuffers: &[GLuint]) {
        unsafe {
            for f in framebuffers {
                DeleteFramebuffer(*f);
            }
        }
    }

    pub fn delete_textures(&self, textures: &[GLuint]) {
        unsafe {
            for t in textures {
                DeleteTexture(*t);
            }
        }
    }

    pub fn framebuffer_renderbuffer(
        &self,
        target: GLenum,
        attachment: GLenum,
        renderbuffertarget: GLenum,
        renderbuffer: GLuint,
    ) {
        debug!(
            "framebufer_renderbuffer {} {} {} {}",
            target, attachment, renderbuffertarget, renderbuffer
        );
        //panic!();
        unsafe {
            FramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer);
        }
    }

    pub fn renderbuffer_storage(
        &self,
        target: GLenum,
        internalformat: GLenum,
        width: GLsizei,
        height: GLsizei,
    ) {
        debug!(
            "renderbuffer_storage {} {} {} {}",
            target, internalformat, width, height
        );
        //panic!();
        unsafe {
            RenderbufferStorage(target, internalformat, width, height);
        }
    }

    pub fn depth_func(&self, func: GLenum) {
        debug!("depth_func {}", func);
        //panic!();
        unsafe {
            DepthFunc(func);
        }
    }

    pub fn active_texture(&self, texture: GLenum) {
        //panic!();
        unsafe {
            ActiveTexture(texture);
        }
    }

    pub fn attach_shader(&self, program: GLuint, shader: GLuint) {
        debug!("attach shader {} {}", program, shader);
        //panic!();
        unsafe {
            AttachShader(program, shader);
        }
    }

    pub fn bind_attrib_location(&self, program: GLuint, index: GLuint, name: &str) {
        debug!("bind_attrib_location {} {} {}", program, index, name);
        //panic!();
        let c_string = CString::new(name).unwrap();
        unsafe { BindAttribLocation(program, index, c_string.as_ptr()) }
    }

    // https://www.khronos.org/registry/OpenGL-Refpages/es2.0/xhtml/glGetUniform.xml
    pub unsafe fn get_uniform_iv(&self, program: GLuint, location: GLint, result: &mut [GLint]) {
        panic!();
        //assert!(!result.is_empty());
    }

    // https://www.khronos.org/registry/OpenGL-Refpages/es2.0/xhtml/glGetUniform.xml
    pub unsafe fn get_uniform_fv(&self, program: GLuint, location: GLint, result: &mut [GLfloat]) {
        panic!();
        //assert!(!result.is_empty());
    }

    pub fn get_uniform_block_index(&self, program: GLuint, name: &str) -> GLuint {
        panic!();
        //0
    }

    pub fn get_uniform_indices(&self, program: GLuint, names: &[&str]) -> Vec<GLuint> {
        panic!();
        //Vec::new()
    }

    pub fn bind_buffer_base(&self, target: GLenum, index: GLuint, buffer: GLuint) {
        panic!();
    }

    pub fn bind_buffer_range(
        &self,
        target: GLenum,
        index: GLuint,
        buffer: GLuint,
        offset: GLintptr,
        size: GLsizeiptr,
    ) {
        panic!();
    }

    pub fn uniform_block_binding(
        &self,
        program: GLuint,
        uniform_block_index: GLuint,
        uniform_block_binding: GLuint,
    ) {
        panic!();
    }

    pub fn bind_buffer(&self, target: GLenum, buffer: GLuint) {
        //panic!();
        unsafe {
            BindBuffer(target, buffer);
        }
    }

    pub fn bind_vertex_array(&self, vao: GLuint) {
        //panic!();
        unsafe {
            BindVertexArray(vao);
        }
    }

    pub fn bind_vertex_array_apple(&self, vao: GLuint) {
        self.bind_vertex_array(vao)
    }

    pub fn bind_renderbuffer(&self, target: GLenum, renderbuffer: GLuint) {
        debug!("bind_renderbuffer {} {}", target, renderbuffer);
        //panic!();
        unsafe {
            BindRenderbuffer(target, renderbuffer);
        }
    }

    pub fn bind_framebuffer(&self, target: GLenum, framebuffer: GLuint) {
        debug!("bind_framebuffer {} {}", target, framebuffer);
        //panic!();
        unsafe {
            BindFramebuffer(target, framebuffer);
        }
    }

    pub fn bind_texture(&self, target: GLenum, texture: GLuint) {
        //panic!();
        unsafe {
            BindTexture(target, texture);
        }
    }

    pub fn draw_buffers(&self, bufs: &[GLenum]) {
        panic!();
        //unsafe {}
    }

    // FIXME: Does not verify buffer size -- unsafe!
    pub fn tex_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        internal_format: GLint,
        width: GLsizei,
        height: GLsizei,
        border: GLint,
        format: GLenum,
        ty: GLenum,
        opt_data: Option<&[u8]>,
    ) {
        unsafe {
            let pdata = match opt_data {
                Some(data) => data.as_ptr() as *const GLvoid,
                None => ptr::null(),
            };
            TexImage2D(
                target,
                level,
                internal_format,
                width,
                height,
                border,
                format,
                ty,
                pdata,
            );
        }
    }

    pub fn compressed_tex_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
        border: GLint,
        data: &[u8],
    ) {
        panic!();
    }

    pub fn compressed_tex_sub_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        data: &[u8],
    ) {
        panic!();
    }

    pub fn tex_image_3d(
        &self,
        target: GLenum,
        level: GLint,
        internal_format: GLint,
        width: GLsizei,
        height: GLsizei,
        depth: GLsizei,
        border: GLint,
        format: GLenum,
        ty: GLenum,
        opt_data: Option<&[u8]>,
    ) {
        panic!();
    }

    pub fn copy_tex_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        internal_format: GLenum,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
        border: GLint,
    ) {
        panic!();
    }

    pub fn copy_tex_sub_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
    ) {
        unsafe {
            CopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height);
        }
    }

    pub fn copy_tex_sub_image_3d(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        zoffset: GLint,
        x: GLint,
        y: GLint,
        width: GLsizei,
        height: GLsizei,
    ) {
        panic!();
    }

    pub fn tex_sub_image_2d(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        ty: GLenum,
        data: &[u8],
    ) {
        debug!(
            "tex_sub_image_2d {} {} {} {} {} {} {} {}",
            target, level, xoffset, yoffset, width, height, format, ty
        );
        //panic!();
        unsafe {
            TexSubImage2D(
                target,
                level,
                xoffset,
                yoffset,
                width,
                height,
                format,
                ty,
                data.as_ptr() as *const c_void,
            );
        }
    }

    pub fn tex_sub_image_2d_pbo(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        format: GLenum,
        ty: GLenum,
        offset: usize,
    ) {
        debug!(
            "tex_sub_image_2d_pbo {} {} {} {} {} {} {} {} {}",
            target, level, xoffset, yoffset, width, height, format, ty, offset
        );
        //panic!();
        unsafe {
            TexSubImage2D(
                target,
                level,
                xoffset,
                yoffset,
                width,
                height,
                format,
                ty,
                offset as *const c_void,
            );
        }
    }

    pub fn tex_sub_image_3d(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        zoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        depth: GLsizei,
        format: GLenum,
        ty: GLenum,
        data: &[u8],
    ) {
        debug!("tex_sub_image_3d");
        panic!();
    }

    pub fn tex_sub_image_3d_pbo(
        &self,
        target: GLenum,
        level: GLint,
        xoffset: GLint,
        yoffset: GLint,
        zoffset: GLint,
        width: GLsizei,
        height: GLsizei,
        depth: GLsizei,
        format: GLenum,
        ty: GLenum,
        offset: usize,
    ) {
        panic!();
    }

    pub fn tex_storage_2d(
        &self,
        target: GLenum,
        levels: GLint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
    ) {
        //panic!();
        unsafe {
            TexStorage2D(target, levels, internal_format, width, height);
        }
    }

    pub fn tex_storage_3d(
        &self,
        target: GLenum,
        levels: GLint,
        internal_format: GLenum,
        width: GLsizei,
        height: GLsizei,
        depth: GLsizei,
    ) {
        panic!();
    }

    pub fn get_tex_image_into_buffer(
        &self,
        target: GLenum,
        level: GLint,
        format: GLenum,
        ty: GLenum,
        output: &mut [u8],
    ) {
        panic!();
    }

    pub unsafe fn copy_image_sub_data(
        &self,
        src_name: GLuint,
        src_target: GLenum,
        src_level: GLint,
        src_x: GLint,
        src_y: GLint,
        src_z: GLint,
        dst_name: GLuint,
        dst_target: GLenum,
        dst_level: GLint,
        dst_x: GLint,
        dst_y: GLint,
        dst_z: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        src_depth: GLsizei,
    ) {
        CopyImageSubData(
            src_name, src_target, src_level, src_x, src_y, src_z, dst_name, dst_target, dst_level,
            dst_x, dst_y, dst_z, src_width, src_height, src_depth,
        );
    }

    pub fn invalidate_framebuffer(&self, target: GLenum, attachments: &[GLenum]) {
        unsafe {
            InvalidateFramebuffer(target, attachments.len() as GLsizei, attachments.as_ptr());
        }
    }

    pub fn invalidate_sub_framebuffer(
        &self,
        target: GLenum,
        attachments: &[GLenum],
        xoffset: GLint,
        yoffset: GLint,
        width: GLsizei,
        height: GLsizei,
    ) {
    }

    #[inline]
    pub unsafe fn get_integer_v(&self, name: GLenum, result: &mut [GLint]) {
        //panic!();
        assert!(!result.is_empty());
        GetIntegerv(name, result.as_mut_ptr());
    }

    #[inline]
    pub unsafe fn get_integer_64v(&self, name: GLenum, result: &mut [GLint64]) {
        panic!();
        //assert!(!result.is_empty());
    }

    #[inline]
    pub unsafe fn get_integer_iv(&self, name: GLenum, index: GLuint, result: &mut [GLint]) {
        panic!();
        //assert!(!result.is_empty());
    }

    #[inline]
    pub unsafe fn get_integer_64iv(&self, name: GLenum, index: GLuint, result: &mut [GLint64]) {
        panic!();
        //assert!(!result.is_empty());
    }

    #[inline]
    pub unsafe fn get_boolean_v(&self, name: GLenum, result: &mut [GLboolean]) {
        debug!("get_boolean_v {}", name);
        //panic!();
        assert!(!result.is_empty());
        GetBooleanv(name, result.as_mut_ptr());
    }

    #[inline]
    pub unsafe fn get_float_v(&self, name: GLenum, result: &mut [GLfloat]) {
        panic!();
        //assert!(!result.is_empty());
    }

    pub fn get_framebuffer_attachment_parameter_iv(
        &self,
        target: GLenum,
        attachment: GLenum,
        pname: GLenum,
    ) -> GLint {
        panic!();
        //0
    }

    pub fn get_renderbuffer_parameter_iv(&self, target: GLenum, pname: GLenum) -> GLint {
        panic!();
        //0
    }

    pub fn get_tex_parameter_iv(&self, target: GLenum, pname: GLenum) -> GLint {
        panic!();
        //0
    }

    pub fn get_tex_parameter_fv(&self, target: GLenum, pname: GLenum) -> GLfloat {
        panic!();
        //0.0
    }

    pub fn tex_parameter_i(&self, target: GLenum, pname: GLenum, param: GLint) {
        //panic!();
        unsafe {
            TexParameteri(target, pname, param);
        }
    }

    pub fn tex_parameter_f(&self, target: GLenum, pname: GLenum, param: GLfloat) {
        panic!();
    }

    pub fn framebuffer_texture_2d(
        &self,
        target: GLenum,
        attachment: GLenum,
        textarget: GLenum,
        texture: GLuint,
        level: GLint,
    ) {
        debug!(
            "framebuffer_texture_2d {} {} {} {} {}",
            target, attachment, textarget, texture, level
        );
        //panic!();
        unsafe {
            FramebufferTexture2D(target, attachment, textarget, texture, level);
        }
    }

    pub fn framebuffer_texture_layer(
        &self,
        target: GLenum,
        attachment: GLenum,
        texture: GLuint,
        level: GLint,
        layer: GLint,
    ) {
        debug!(
            "framebuffer_texture_layer {} {} {} {} {}",
            target, attachment, texture, level, layer
        );
        panic!();
    }

    pub fn blit_framebuffer(
        &self,
        src_x0: GLint,
        src_y0: GLint,
        src_x1: GLint,
        src_y1: GLint,
        dst_x0: GLint,
        dst_y0: GLint,
        dst_x1: GLint,
        dst_y1: GLint,
        mask: GLbitfield,
        filter: GLenum,
    ) {
        unsafe {
            BlitFramebuffer(
                src_x0, src_y0, src_x1, src_y1, dst_x0, dst_y0, dst_x1, dst_y1, mask, filter,
            );
        }
    }

    pub fn vertex_attrib_4f(&self, index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) {
        panic!();
    }

    pub fn vertex_attrib_pointer_f32(
        &self,
        index: GLuint,
        size: GLint,
        normalized: bool,
        stride: GLsizei,
        offset: GLuint,
    ) {
        panic!();
    }

    pub fn vertex_attrib_pointer(
        &self,
        index: GLuint,
        size: GLint,
        type_: GLenum,
        normalized: bool,
        stride: GLsizei,
        offset: GLuint,
    ) {
        debug!(
            "vertex_attrib_pointer {} {} {} {} {} {}",
            index, size, type_, normalized, stride, offset
        );
        //panic!();
        unsafe {
            VertexAttribPointer(
                index,
                size,
                type_,
                normalized as GLboolean,
                stride,
                offset as *const GLvoid,
            );
        }
    }

    pub fn vertex_attrib_i_pointer(
        &self,
        index: GLuint,
        size: GLint,
        type_: GLenum,
        stride: GLsizei,
        offset: GLuint,
    ) {
        debug!(
            "vertex_attrib_i_pointer {} {} {} {} {}",
            index, size, type_, stride, offset
        );
        //panic!();
        unsafe {
            VertexAttribIPointer(index, size, type_, stride, offset as *const GLvoid);
        }
    }

    pub fn vertex_attrib_divisor(&self, index: GLuint, divisor: GLuint) {
        debug!("vertex_attrib_divisor {} {}", index, divisor);
        //assert!(index == 0 && divisor == 0);
        //panic!();
        unsafe {
            VertexAttribDivisor(index, divisor);
        }
    }

    pub fn viewport(&self, x: GLint, y: GLint, width: GLsizei, height: GLsizei) {
        debug!("viewport {} {} {} {}", x, y, width, height);
        //panic!();
        unsafe {
            SetViewport(x, y, width, height);
        }
    }

    pub fn scissor(&self, x: GLint, y: GLint, width: GLsizei, height: GLsizei) {
        //panic!();
        unsafe {
            SetScissor(x, y, width, height);
        }
    }

    pub fn line_width(&self, width: GLfloat) {
        panic!();
    }

    pub fn use_program(&self, program: GLuint) {
        //panic!();
        unsafe {
            UseProgram(program);
        }
    }

    pub fn validate_program(&self, program: GLuint) {
        panic!();
    }

    pub fn draw_arrays(&self, mode: GLenum, first: GLint, count: GLsizei) {
        unsafe {
            DrawElementsInstanced(mode, count, NONE, first as GLintptr, 1);
        }
    }

    pub fn draw_arrays_instanced(
        &self,
        mode: GLenum,
        first: GLint,
        count: GLsizei,
        primcount: GLsizei,
    ) {
        unsafe {
            DrawElementsInstanced(mode, count, NONE, first as GLintptr, primcount);
        }
    }

    pub fn draw_elements(
        &self,
        mode: GLenum,
        count: GLsizei,
        element_type: GLenum,
        indices_offset: GLuint,
    ) {
        debug!(
            "draw_elements {} {} {} {} {}",
            mode, count, element_type, indices_offset
        );
        //panic!();
        unsafe {
            DrawElementsInstanced(mode, count, element_type, indices_offset as GLintptr, 1);
        }
    }

    pub fn draw_elements_instanced(
        &self,
        mode: GLenum,
        count: GLsizei,
        element_type: GLenum,
        indices_offset: GLuint,
        primcount: GLsizei,
    ) {
        debug!(
            "draw_elements_instanced {} {} {} {} {}",
            mode, count, element_type, indices_offset, primcount
        );
        //panic!();
        unsafe {
            DrawElementsInstanced(
                mode,
                count,
                element_type,
                indices_offset as GLintptr,
                primcount,
            );
        }
    }

    pub fn blend_color(&self, r: f32, g: f32, b: f32, a: f32) {
        unsafe {
            BlendColor(r, g, b, a);
        }
    }

    pub fn blend_func(&self, sfactor: GLenum, dfactor: GLenum) {
        unsafe {
            BlendFunc(sfactor, dfactor, sfactor, dfactor);
        }
    }

    pub fn blend_func_separate(
        &self,
        src_rgb: GLenum,
        dest_rgb: GLenum,
        src_alpha: GLenum,
        dest_alpha: GLenum,
    ) {
        unsafe {
            BlendFunc(src_rgb, dest_rgb, src_alpha, dest_alpha);
        }
    }

    pub fn blend_equation(&self, mode: GLenum) {
        unsafe {
            BlendEquation(mode);
        }
    }

    pub fn blend_equation_separate(&self, mode_rgb: GLenum, mode_alpha: GLenum) {
        panic!();
    }

    pub fn color_mask(&self, r: bool, g: bool, b: bool, a: bool) {
        panic!();
    }

    pub fn cull_face(&self, mode: GLenum) {
        panic!();
    }

    pub fn front_face(&self, mode: GLenum) {
        panic!();
    }

    pub fn enable(&self, cap: GLenum) {
        debug!("enable {}", cap);
        //panic!();
        unsafe {
            Enable(cap);
        }
    }

    pub fn disable(&self, cap: GLenum) {
        debug!("disable {}", cap);
        //panic!();
        unsafe {
            Disable(cap);
        }
    }

    pub fn hint(&self, param_name: GLenum, param_val: GLenum) {
        panic!();
    }

    pub fn is_enabled(&self, cap: GLenum) -> GLboolean {
        panic!();
        //0
    }

    pub fn is_shader(&self, shader: GLuint) -> GLboolean {
        panic!();
        //0
    }

    pub fn is_texture(&self, texture: GLenum) -> GLboolean {
        panic!();
        //0
    }

    pub fn is_framebuffer(&self, framebuffer: GLenum) -> GLboolean {
        panic!();
        //0
    }

    pub fn is_renderbuffer(&self, renderbuffer: GLenum) -> GLboolean {
        panic!();
        //0
    }

    pub fn check_frame_buffer_status(&self, target: GLenum) -> GLenum {
        debug!("check_frame_buffer_status {}", target);
        //panic!();
        unsafe { CheckFramebufferStatus(target) }
    }

    pub fn enable_vertex_attrib_array(&self, index: GLuint) {
        //panic!();
        debug!("enable_vertex_attrib_array {}", index);
        unsafe {
            EnableVertexAttribArray(index);
            //assert_eq!(index, 0);
        }
    }

    pub fn disable_vertex_attrib_array(&self, index: GLuint) {
        panic!();
    }

    pub fn uniform_1f(&self, location: GLint, v0: GLfloat) {
        panic!();
    }

    pub fn uniform_1fv(&self, location: GLint, values: &[f32]) {
        panic!();
    }

    pub fn uniform_1i(&self, location: GLint, v0: GLint) {
        debug!("uniform_1i {} {}", location, v0);
        //panic!();
        unsafe {
            Uniform1i(location, v0);
        }
    }

    pub fn uniform_1iv(&self, location: GLint, values: &[i32]) {
        panic!();
    }

    pub fn uniform_1ui(&self, location: GLint, v0: GLuint) {
        panic!();
    }

    pub fn uniform_2f(&self, location: GLint, v0: GLfloat, v1: GLfloat) {
        panic!();
    }

    pub fn uniform_2fv(&self, location: GLint, values: &[f32]) {
        panic!();
    }

    pub fn uniform_2i(&self, location: GLint, v0: GLint, v1: GLint) {
        panic!();
    }

    pub fn uniform_2iv(&self, location: GLint, values: &[i32]) {
        panic!();
    }

    pub fn uniform_2ui(&self, location: GLint, v0: GLuint, v1: GLuint) {
        panic!();
    }

    pub fn uniform_3f(&self, location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) {
        panic!();
    }

    pub fn uniform_3fv(&self, location: GLint, values: &[f32]) {
        panic!();
    }

    pub fn uniform_3i(&self, location: GLint, v0: GLint, v1: GLint, v2: GLint) {
        panic!();
    }

    pub fn uniform_3iv(&self, location: GLint, values: &[i32]) {
        panic!();
    }

    pub fn uniform_3ui(&self, location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) {
        panic!();
    }

    pub fn uniform_4f(&self, location: GLint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) {
        panic!();
    }

    pub fn uniform_4i(&self, location: GLint, x: GLint, y: GLint, z: GLint, w: GLint) {
        panic!();
    }

    pub fn uniform_4iv(&self, location: GLint, values: &[i32]) {
        panic!();
    }

    pub fn uniform_4ui(&self, location: GLint, x: GLuint, y: GLuint, z: GLuint, w: GLuint) {
        panic!();
    }

    pub fn uniform_4fv(&self, location: GLint, values: &[f32]) {
        unsafe {
            Uniform4fv(location, (values.len() / 4) as GLsizei, values.as_ptr());
        }
    }

    pub fn uniform_matrix_2fv(&self, location: GLint, transpose: bool, value: &[f32]) {
        panic!();
    }

    pub fn uniform_matrix_3fv(&self, location: GLint, transpose: bool, value: &[f32]) {
        panic!();
    }

    pub fn uniform_matrix_4fv(&self, location: GLint, transpose: bool, value: &[f32]) {
        debug!("uniform_matrix_4fv {} {} {:?}", location, transpose, value);
        //panic!();
        unsafe {
            UniformMatrix4fv(
                location,
                (value.len() / 16) as GLsizei,
                transpose as GLboolean,
                value.as_ptr(),
            );
        }
    }

    pub fn depth_mask(&self, flag: bool) {
        debug!("depth_mask {}", flag);
        //panic!();
        unsafe {
            DepthMask(flag as GLboolean);
        }
    }

    pub fn depth_range(&self, near: f64, far: f64) {
        panic!();
    }

    pub fn get_active_attrib(&self, program: GLuint, index: GLuint) -> (i32, u32, String) {
        panic!();
        //(0, 0, String::new())
    }

    pub fn get_active_uniform(&self, program: GLuint, index: GLuint) -> (i32, u32, String) {
        panic!();
        //(0, 0, String::new())
    }

    pub fn get_active_uniforms_iv(
        &self,
        program: GLuint,
        indices: Vec<GLuint>,
        pname: GLenum,
    ) -> Vec<GLint> {
        panic!();
        //Vec::new()
    }

    pub fn get_active_uniform_block_i(&self, program: GLuint, index: GLuint, pname: GLenum) -> GLint {
        panic!();
        //0
    }

    pub fn get_active_uniform_block_iv(
        &self,
        program: GLuint,
        index: GLuint,
        pname: GLenum,
    ) -> Vec<GLint> {
        panic!();
        //Vec::new()
    }

    pub fn get_active_uniform_block_name(&self, program: GLuint, index: GLuint) -> String {
        panic!();
        //String::new()
    }

    pub fn get_attrib_location(&self, program: GLuint, name: &str) -> c_int {
        let name = CString::new(name).unwrap();
        unsafe { GetAttribLocation(program, name.as_ptr()) }
    }

    pub fn get_frag_data_location(&self, program: GLuint, name: &str) -> c_int {
        panic!();
        //0
    }

    pub fn get_uniform_location(&self, program: GLuint, name: &str) -> c_int {
        debug!("get_uniform_location {} {}", program, name);
        //panic!();
        let name = CString::new(name).unwrap();
        unsafe { GetUniformLocation(program, name.as_ptr()) }
    }

    pub fn get_program_info_log(&self, program: GLuint) -> String {
        debug!("get_program_info_log {}", program);
        String::new()
    }

    #[inline]
    pub unsafe fn get_program_iv(&self, program: GLuint, pname: GLenum, result: &mut [GLint]) {
        debug!("get_program_iv {}", pname);
        //panic!();
        assert!(!result.is_empty());
        //#define GL_LINK_STATUS                    0x8B82
        if pname == 0x8b82 {
            result[0] = GetLinkStatus(program);
        }
    }

    pub fn get_program_binary(&self, program: GLuint) -> (Vec<u8>, GLenum) {
        panic!();
        //(Vec::new(), NONE)
    }

    pub fn program_binary(&self, program: GLuint, format: GLenum, binary: &[u8]) {
        panic!();
    }

    pub fn program_parameter_i(&self, program: GLuint, pname: GLenum, value: GLint) {
        panic!();
    }

    #[inline]
    pub unsafe fn get_vertex_attrib_iv(&self, index: GLuint, pname: GLenum, result: &mut [GLint]) {
        panic!();
        //assert!(!result.is_empty());
    }

    #[inline]
    pub unsafe fn get_vertex_attrib_fv(&self, index: GLuint, pname: GLenum, result: &mut [GLfloat]) {
        panic!();
        //assert!(!result.is_empty());
    }

    pub fn get_vertex_attrib_pointer_v(&self, index: GLuint, pname: GLenum) -> GLsizeiptr {
        panic!();
        //0
    }

    pub fn get_buffer_parameter_iv(&self, target: GLuint, pname: GLenum) -> GLint {
        panic!();
        //0
    }

    pub fn get_shader_info_log(&self, shader: GLuint) -> String {
        debug!("get_shader_info_log {}", shader);
        //panic!();
        String::new()
    }

    pub fn get_string(&self, which: GLenum) -> String {
        // panic!();
        unsafe {
            let llstr = GetString(which);
            if !llstr.is_null() {
                return str::from_utf8_unchecked(CStr::from_ptr(llstr).to_bytes()).to_string();
            } else {
                return "".to_string();
            }
        }
    }

    pub fn get_string_i(&self, which: GLenum, index: GLuint) -> String {
        //panic!();
        unsafe {
            let llstr = GetStringi(which, index);
            if !llstr.is_null() {
                str::from_utf8_unchecked(CStr::from_ptr(llstr).to_bytes()).to_string()
            } else {
                "".to_string()
            }
        }
    }

    pub unsafe fn get_shader_iv(&self, shader: GLuint, pname: GLenum, result: &mut [GLint]) {
        debug!("get_shader_iv");
        //panic!();
        assert!(!result.is_empty());
        if pname == 0x8B81
        /*gl::COMPILE_STATUS*/
        {
            result[0] = 1;
        }
    }

    pub fn get_shader_precision_format(
        &self,
        _shader_type: GLuint,
        precision_type: GLuint,
    ) -> (GLint, GLint, GLint) {
        // gl.GetShaderPrecisionFormat is not available until OpenGL 4.1.
        // Fallback to OpenGL standard precissions that most desktop hardware support.
        match precision_type {
            LOW_FLOAT | MEDIUM_FLOAT | HIGH_FLOAT => {
                // Fallback to IEEE 754 single precision
                // Range: from -2^127 to 2^127
                // Significand precision: 23 bits
                (127, 127, 23)
            }
            LOW_INT | MEDIUM_INT | HIGH_INT => {
                // Fallback to single precision integer
                // Range: from -2^24 to 2^24
                // Precision: For integer formats this value is always 0
                (24, 24, 0)
            }
            _ => (0, 0, 0),
        }
    }

    pub fn compile_shader(&self, shader: GLuint) {
        debug!("compile_shader {}", shader);
        //panic!();
    }

    pub fn create_program(&self) -> GLuint {
        debug!("create_program");
        //panic!();
        unsafe { CreateProgram() }
    }

    pub fn delete_program(&self, program: GLuint) {
        unsafe {
            DeleteProgram(program);
        }
    }

    pub fn create_shader(&self, shader_type: GLenum) -> GLuint {
        debug!("create_shader {}", shader_type);
        //panic!();
        unsafe { CreateShader(shader_type) }
    }

    pub fn delete_shader(&self, shader: GLuint) {
        debug!("delete_shader {}", shader);
        //panic!();
        unsafe {
            DeleteShader(shader);
        }
    }

    pub fn detach_shader(&self, program: GLuint, shader: GLuint) {
        debug!("detach_shader {} {}", program, shader);
        //panic!();
    }

    pub fn link_program(&self, program: GLuint) {
        debug!("link_program {}", program);
        //panic!();
        unsafe {
            LinkProgram(program);
        }
    }

    pub fn clear_color(&self, r: f32, g: f32, b: f32, a: f32) {
        //panic!();
        unsafe {
            ClearColor(r, g, b, a);
        }
    }

    pub fn clear(&self, buffer_mask: GLbitfield) {
        debug!("clear {}", buffer_mask);
        //panic!();
        unsafe {
            Clear(buffer_mask);
        }
    }

    pub fn clear_depth(&self, depth: f64) {
        debug!("clear_depth {}", depth);
        //panic!();
        unsafe {
            ClearDepth(depth as GLclampd);
        }
    }

    pub fn clear_stencil(&self, s: GLint) {
        panic!();
    }

    pub fn flush(&self) {}

    pub fn finish(&self) {
        unsafe {
            Finish();
        }
    }

    pub fn get_error(&self) -> GLenum {
        //panic!();
        unsafe { GetError() }
    }

    pub fn stencil_mask(&self, mask: GLuint) {
        panic!();
    }

    pub fn stencil_mask_separate(&self, face: GLenum, mask: GLuint) {
        panic!();
    }

    pub fn stencil_func(&self, func: GLenum, ref_: GLint, mask: GLuint) {
        panic!();
    }

    pub fn stencil_func_separate(&self, face: GLenum, func: GLenum, ref_: GLint, mask: GLuint) {
        panic!();
    }

    pub fn stencil_op(&self, sfail: GLenum, dpfail: GLenum, dppass: GLenum) {
        panic!();
    }

    pub fn stencil_op_separate(&self, face: GLenum, sfail: GLenum, dpfail: GLenum, dppass: GLenum) {
        panic!();
    }

    pub fn egl_image_target_texture2d_oes(&self, target: GLenum, image: GLeglImageOES) {
        panic!("not supported")
    }

    pub fn egl_image_target_renderbuffer_storage_oes(&self, target: GLenum, image: GLeglImageOES) {
        panic!("not supported")
    }

    pub fn generate_mipmap(&self, target: GLenum) {
        unsafe {
            GenerateMipmap(target);
        }
    }

    pub fn insert_event_marker_ext(&self, message: &str) {
        panic!();
    }

    pub fn push_group_marker_ext(&self, message: &str) {
        debug!("push group {}", message);
        panic!();
    }

    pub fn pop_group_marker_ext(&self) {
        debug!("pop group");
        panic!();
    }

    pub fn debug_message_insert_khr(
        &self,
        source: GLenum,
        type_: GLenum,
        id: GLuint,
        severity: GLenum,
        message: &str,
    ) {
        panic!();
    }

    pub fn push_debug_group_khr(&self, source: GLenum, id: GLuint, message: &str) {
        panic!();
    }

    pub fn pop_debug_group_khr(&self) {
        panic!();
    }

    pub fn fence_sync(&self, condition: GLenum, flags: GLbitfield) -> GLsync {
        panic!();
        //ptr::null()
    }

    pub fn client_wait_sync(&self, sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum {
        panic!();
    }

    pub fn wait_sync(&self, sync: GLsync, flags: GLbitfield, timeout: GLuint64) {
        panic!();
    }

    pub fn texture_range_apple(&self, target: GLenum, data: &[u8]) {
        panic!();
    }

    pub fn delete_sync(&self, sync: GLsync) {
        panic!();
    }

    pub fn gen_fences_apple(&self, n: GLsizei) -> Vec<GLuint> {
        panic!();
        //Vec::new()
    }

    pub fn delete_fences_apple(&self, fences: &[GLuint]) {
        panic!();
    }

    pub fn set_fence_apple(&self, fence: GLuint) {
        panic!();
    }

    pub fn finish_fence_apple(&self, fence: GLuint) {
        panic!();
    }

    pub fn test_fence_apple(&self, fence: GLuint) {
        panic!();
    }

    pub fn test_object_apple(&self, object: GLenum, name: GLuint) -> GLboolean {
        panic!();
        //0
    }

    pub fn finish_object_apple(&self, object: GLenum, name: GLuint) {
        panic!();
    }

    // GL_ARB_blend_func_extended
    pub fn bind_frag_data_location_indexed(
        &self,
        program: GLuint,
        color_number: GLuint,
        index: GLuint,
        name: &str,
    ) {
        panic!();
    }

    pub fn get_frag_data_index(&self, program: GLuint, name: &str) -> GLint {
        panic!();
        //-1
    }

    // GL_KHR_debug
    pub fn get_debug_messages(&self) -> Vec<DebugMessage> {
        Vec::new()
    }

    pub fn provoking_vertex_angle(&self, _mode: GLenum) {
        unimplemented!("This extension is GLES only");
    }

    // GL_KHR_blend_equation_advanced
    pub fn blend_barrier_khr(&self) {
        // No barrier required, so nothing to do
    }

    // GL_CHROMIUM_copy_texture
    pub fn copy_texture_chromium(
        &self,
        _source_id: GLuint,
        _source_level: GLint,
        _dest_target: GLenum,
        _dest_id: GLuint,
        _dest_level: GLint,
        _internal_format: GLint,
        _dest_type: GLenum,
        _unpack_flip_y: GLboolean,
        _unpack_premultiply_alpha: GLboolean,
        _unpack_unmultiply_alpha: GLboolean,
    ) {
        unimplemented!("This extension is GLES only");
    }
    pub fn copy_sub_texture_chromium(
        &self,
        _source_id: GLuint,
        _source_level: GLint,
        _dest_target: GLenum,
        _dest_id: GLuint,
        _dest_level: GLint,
        _x_offset: GLint,
        _y_offset: GLint,
        _x: GLint,
        _y: GLint,
        _width: GLsizei,
        _height: GLsizei,
        _unpack_flip_y: GLboolean,
        _unpack_premultiply_alpha: GLboolean,
        _unpack_unmultiply_alpha: GLboolean,
    ) {
        unimplemented!("This extension is GLES only");
    }

    // GL_ANGLE_copy_texture_3d
    pub fn copy_texture_3d_angle(
        &self,
        _source_id: GLuint,
        _source_level: GLint,
        _dest_target: GLenum,
        _dest_id: GLuint,
        _dest_level: GLint,
        _internal_format: GLint,
        _dest_type: GLenum,
        _unpack_flip_y: GLboolean,
        _unpack_premultiply_alpha: GLboolean,
        _unpack_unmultiply_alpha: GLboolean,
    ) {
        unimplemented!("Not supported by SWGL");
    }

    pub fn copy_sub_texture_3d_angle(
        &self,
        _source_id: GLuint,
        _source_level: GLint,
        _dest_target: GLenum,
        _dest_id: GLuint,
        _dest_level: GLint,
        _x_offset: GLint,
        _y_offset: GLint,
        _z_offset: GLint,
        _x: GLint,
        _y: GLint,
        _z: GLint,
        _width: GLsizei,
        _height: GLsizei,
        _depth: GLsizei,
        _unpack_flip_y: GLboolean,
        _unpack_premultiply_alpha: GLboolean,
        _unpack_unmultiply_alpha: GLboolean,
    ) {
        unimplemented!("Not supported by SWGL");
    }

    pub fn buffer_storage(
        &self,
        target: GLenum,
        size: GLsizeiptr,
        data: *const GLvoid,
        flags: GLbitfield,
    ) {
        unimplemented!("Not supported by SWGL");
    }

    pub fn flush_mapped_buffer_range(&self, target: GLenum, offset: GLintptr, length: GLsizeiptr) {
        unimplemented!("Not supported by SWGL");
    }
}

/// A resource that is intended for sharing between threads.
/// Locked resources such as textures or framebuffers will
/// not allow any further modifications while it remains
/// locked. The resource will be unlocked when LockedResource
/// is dropped.
pub struct LockedResource(*mut LockedTexture);

unsafe impl Send for LockedResource {}
unsafe impl Sync for LockedResource {}

#[repr(u8)]
pub enum YuvRangedColorSpace {
    Rec601Narrow = 0,
    Rec601Full,
    Rec709Narrow,
    Rec709Full,
    Rec2020Narrow,
    Rec2020Full,
    GbrIdentity,
}

impl LockedResource {
    /// Composites from a locked resource to another locked resource. The band
    /// offset and height are relative to the destination rectangle and specify
    /// how to clip the composition into appropriate range for this band.
    pub fn composite(
        &self,
        locked_src: &LockedResource,
        src_x: GLint,
        src_y: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        dst_x: GLint,
        dst_y: GLint,
        dst_width: GLsizei,
        dst_height: GLsizei,
        opaque: bool,
        flip: bool,
        filter: GLenum,
        clip_x: GLint,
        clip_y: GLint,
        clip_width: GLsizei,
        clip_height: GLsizei,
    ) {
        unsafe {
            Composite(
                self.0,
                locked_src.0,
                src_x,
                src_y,
                src_width,
                src_height,
                dst_x,
                dst_y,
                dst_width,
                dst_height,
                opaque as GLboolean,
                flip as GLboolean,
                filter,
                clip_x,
                clip_y,
                clip_width,
                clip_height,
            );
        }
    }

    /// Composites from locked resources representing YUV planes
    pub fn composite_yuv(
        &self,
        locked_y: &LockedResource,
        locked_u: &LockedResource,
        locked_v: &LockedResource,
        color_space: YuvRangedColorSpace,
        color_depth: GLuint,
        src_x: GLint,
        src_y: GLint,
        src_width: GLsizei,
        src_height: GLsizei,
        dst_x: GLint,
        dst_y: GLint,
        dst_width: GLsizei,
        dst_height: GLsizei,
        flip: bool,
        clip_x: GLint,
        clip_y: GLint,
        clip_width: GLsizei,
        clip_height: GLsizei,
    ) {
        unsafe {
            CompositeYUV(
                self.0,
                locked_y.0,
                locked_u.0,
                locked_v.0,
                color_space,
                color_depth,
                src_x,
                src_y,
                src_width,
                src_height,
                dst_x,
                dst_y,
                dst_width,
                dst_height,
                flip as GLboolean,
                clip_x,
                clip_y,
                clip_width,
                clip_height,
            );
        }
    }

    /// Get the underlying buffer for a locked resource
    pub fn get_buffer(&self) -> (*mut c_void, i32, i32, i32) {
        unsafe {
            let mut width: i32 = 0;
            let mut height: i32 = 0;
            let mut stride: i32 = 0;
            let data_ptr = GetResourceBuffer(self.0, &mut width, &mut height, &mut stride);
            (data_ptr, width, height, stride)
        }
    }
}

impl Clone for LockedResource {
    fn clone(&self) -> Self {
        unsafe {
            LockResource(self.0);
        }
        LockedResource(self.0)
    }
}

impl Drop for LockedResource {
    fn drop(&mut self) {
        unsafe {
            UnlockResource(self.0);
        }
    }
}
