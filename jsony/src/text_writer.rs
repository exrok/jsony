use std::{borrow::Cow, mem::MaybeUninit};

use crate::{
    byte_writer::BytesWriter,
    json::{AlwaysArray, AlwaysObject, AlwaysString},
};

/// A UTF-8 wrapper around a [BytesWriter].
///
/// Conceptually, a TextWriter is somewhere between a `String` and a `dyn std::fmt::Write`.
///
/// Like `dyn std::fmt::Write`, `TextWriter` can be backed by many types like:
/// - `Vec<u8>`
/// - `&mut [MaybeUninit<u8>]`
/// - `String`
/// - `dyn std::io::Write`
///
/// However, common operations avoid dynamic dispatch most of the time by restricting
/// dispatch to out of capacity situations where either the underlying buffer needs to be
/// resized or flushed.
///
/// Additionally, many helper methods are provided for common operations for the text formats
/// supported by jsony.
#[repr(C)]
pub struct TextWriter<'a> {
    #[doc(hidden)]
    pub joining: bool,
    #[doc(hidden)]
    pub flags: u16,
    buffer: BytesWriter<'a>,
}

/// Conversion into a `TextWriter` with content extraction.
///
/// This is primaryily used in [crate::to_json_into].
pub trait IntoTextWriter<'a> {
    type Output;

    /// Convert Self into TextWriter, preserving the contents.
    fn into_text_writer(self) -> TextWriter<'a>;

    /// Should return Output corresponding to added context since the
    /// creation from `into_text_writer()`.
    ///
    /// If `finish_writing` is called on an instance of TextWriter
    /// not create via [IntoTextWriter::into_text_writer] of the same type then this
    /// method may panic.
    fn finish_writing(buffer: TextWriter<'a>) -> Self::Output;
}

impl<'a> IntoTextWriter<'a> for &'a mut String {
    type Output = &'a str;
    fn into_text_writer(self) -> TextWriter<'a> {
        // Safety: TextWriter will only append valid utf-8
        TextWriter::with_buffer(BytesWriter::from(unsafe { self.as_mut_vec() }))
    }
    fn finish_writing(buffer: TextWriter<'a>) -> &'a str {
        buffer.into_backed_str()
    }
}

impl<'a> IntoTextWriter<'a> for &'a mut Vec<u8> {
    type Output = &'a str;
    fn into_text_writer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::from(self))
    }
    fn finish_writing(buffer: TextWriter<'a>) -> &'a str {
        buffer.into_backed_str()
    }
}

pub type DynWrite<'a> = &'a mut (dyn std::io::Write + Send);
impl<'a> IntoTextWriter<'a> for DynWrite<'a> {
    type Output = Result<usize, std::io::Error>;
    fn into_text_writer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::new_writer(self))
    }
    fn finish_writing(buffer: TextWriter<'a>) -> Result<usize, std::io::Error> {
        buffer.buffer.into_write_finish()
    }
}

impl<'a> IntoTextWriter<'a> for &'a mut [MaybeUninit<u8>] {
    type Output = Cow<'a, str>;
    fn into_text_writer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::from(self))
    }
    fn finish_writing(buffer: TextWriter<'a>) -> Cow<'a, str> {
        buffer.into_borrowed_cow()
    }
}

impl Default for TextWriter<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> TextWriter<'a> {
    #[doc(hidden)]
    pub const WITH_SECRETS: u16 = 1;
    /// Note: We do not want to make this public.
    fn with_buffer(buffer: BytesWriter<'a>) -> TextWriter<'a> {
        TextWriter {
            joining: false,
            flags: 0,
            buffer,
        }
    }
    pub fn has(&self, flags: u16) -> bool {
        (self.flags & flags) == flags
    }
    pub fn new() -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::new())
    }
    pub fn with_capacity(capacity: usize) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::with_capacity(capacity))
    }
    pub fn start_json_object(&mut self) {
        if !self.joining {
            self.buffer.push(b'{');
        }
        self.joining = false;
    }
    pub fn start_json_string(&mut self) {
        if !self.joining {
            self.buffer.push(b'"');
        }
        self.joining = false;
    }
    pub fn start_json_array(&mut self) {
        if !self.joining {
            self.buffer.push(b'[');
        }
        self.joining = false;
    }
    pub fn push_comma(&mut self) {
        self.buffer.push(b',');
    }
    pub fn push_colon(&mut self) {
        self.buffer.push(b':');
    }
    pub fn smart_array_comma(&mut self) -> AlwaysArray {
        if let Some(ch) = self.buffer.last() {
            if *ch != b'[' && *ch != b',' {
                self.buffer.push(b',');
            }
        }
        AlwaysArray
    }
    pub fn smart_object_comma(&mut self) -> AlwaysArray {
        if let Some(ch) = self.buffer.last() {
            if *ch != b'{' && *ch != b',' {
                self.buffer.push(b',');
            }
        }
        AlwaysArray
    }
    pub fn end_json_array(&mut self) -> AlwaysArray {
        if let Some(ch) = self.buffer.last() {
            if *ch == b',' {
                *ch = b']'
            } else {
                self.buffer.push(b']');
            }
        }
        AlwaysArray
    }
    pub fn end_json_object(&mut self) -> AlwaysObject {
        if let Some(ch) = self.buffer.last() {
            if *ch == b',' {
                *ch = b'}'
            } else {
                self.buffer.push(b'}');
            }
        }
        AlwaysObject
    }
    pub fn end_json_string(&mut self) -> AlwaysString {
        self.buffer.push(b'"');
        AlwaysString
    }
    pub fn join_parent_json_value_with_next(&mut self) {
        debug_assert!(!self.joining);
        self.joining = true;
    }
    pub fn join_string_with_next_value(&mut self) {
        debug_assert!(!self.joining);
        self.buffer.saturting_pop();
        self.joining = true;
    }
    pub fn finite_f64(&mut self, value: f64) {
        unsafe {
            self.buffer.reserve_small(24);
            let amount = ryu::raw::format64(value, self.buffer.tail_ptr());
            self.buffer.advance(amount);
        }
    }
    pub fn finite_f32(&mut self, value: f32) {
        unsafe {
            self.buffer.reserve_small(16);
            let amount = ryu::raw::format32(value, self.buffer.tail_ptr());
            self.buffer.advance(amount);
        }
    }
    pub fn join_object_with_next_value(&mut self) {
        debug_assert!(!self.joining);
        self.joining = false;
        if let Some([a, b]) = self.buffer.last_2() {
            debug_assert_eq!(*b, b'}');
            if *a == b'{' {
                self.buffer.saturting_pop();
            } else {
                *b = b','
            }
        } else {
            debug_assert!(false, "Attempt to join when buffer was empty")
        }
    }
    pub fn join_array_with_next_value(&mut self) {
        debug_assert!(!self.joining);
        self.joining = false;
        if let Some([a, b]) = self.buffer.last_2() {
            debug_assert_eq!(*b, b']');
            if *a == b'[' {
                self.buffer.saturting_pop();
            } else {
                *b = b','
            }
        } else {
            debug_assert!(false, "Attempt to join when buffer was empty")
        }
    }
    pub fn into_string(self) -> String {
        // Safety:
        // Since the only way to create an JsonBuffer with an owned buffer
        // is for it to start empty, and we only append utf-8, the vec
        // is guaranteed to be utf-8
        unsafe { String::from_utf8_unchecked(self.buffer.owned_into_vec()) }
    }

    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.buffer.buffer_slice()) }
    }

    pub fn buffer_slice(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.buffer.buffer_slice()) }
    }
    fn into_backed_str(self) -> &'a str {
        // Safety:
        // This will contain only appended utf-8 strings
        unsafe { std::str::from_utf8_unchecked(self.buffer.into_backed_with_extended_slice()) }
    }
    fn into_borrowed_cow(self) -> Cow<'a, str> {
        unsafe { self.buffer.into_cow_utf8_unchecked() }
    }
    pub fn push_str(&mut self, text: &str) {
        self.buffer.push_bytes(text.as_bytes());
    }
    /// # Safety
    /// ch must be an ascii character
    pub unsafe fn push_unchecked_ascii(&mut self, ch: u8) {
        self.buffer.push(ch);
    }
    /// # Safety
    /// text must be valid UTF-8
    pub(crate) unsafe fn push_unchecked_utf8(&mut self, text: &[u8]) {
        self.buffer.push_bytes(text);
    }

    pub fn clear(&mut self) {
        self.buffer.clear()
    }
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
}
