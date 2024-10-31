use std::{borrow::Cow, mem::MaybeUninit};

use crate::{
    byte_writer::BytesWriter,
    json::{ArrayValue, ObjectValue, StringValue},
};

#[repr(C)]
pub struct TextWriter<'a> {
    #[doc(hidden)]
    pub joining: bool,
    buffer: BytesWriter<'a>,
}

pub trait TextSink<'a> {
    type Output;
    fn buffer(self) -> TextWriter<'a>;
    fn finish(buffer: TextWriter<'a>) -> Self::Output;
}

impl<'a> TextSink<'a> for &'a mut String {
    type Output = &'a str;
    fn buffer(self) -> TextWriter<'a> {
        // Safety: TextWriter will only appened valid utf-8
        TextWriter::with_buffer(BytesWriter::from(unsafe { self.as_mut_vec() }))
    }
    fn finish(buffer: TextWriter<'a>) -> &'a str {
        buffer.into_backed_str()
    }
}

impl<'a> TextSink<'a> for &'a mut Vec<u8> {
    type Output = &'a str;
    fn buffer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::from(self))
    }
    fn finish(buffer: TextWriter<'a>) -> &'a str {
        buffer.into_backed_str()
    }
}

type DynWrite<'a> = &'a mut (dyn std::io::Write + Send);
impl<'a> TextSink<'a> for DynWrite<'a> {
    type Output = Result<usize, std::io::Error>;
    fn buffer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::new_writer(self))
    }
    fn finish(buffer: TextWriter<'a>) -> Result<usize, std::io::Error> {
        buffer.buffer.into_write_finish()
    }
}
impl<'a> TextSink<'a> for &'a mut [MaybeUninit<u8>] {
    type Output = Cow<'a, str>;
    fn buffer(self) -> TextWriter<'a> {
        TextWriter::with_buffer(BytesWriter::from(self))
    }
    fn finish(buffer: TextWriter<'a>) -> Cow<'a, str> {
        buffer.into_borrowed_cow()
    }
}

impl<'a> TextWriter<'a> {
    /// Note: We do not want to make this public.
    fn with_buffer(buffer: BytesWriter<'a>) -> TextWriter<'a> {
        TextWriter {
            joining: false,
            buffer,
        }
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
    pub fn smart_array_comma(&mut self) -> ArrayValue {
        if let Some(ch) = self.buffer.last() {
            if *ch != b'[' && *ch != b',' {
                self.buffer.push(b',');
            }
        }
        ArrayValue
    }
    pub fn smart_object_comma(&mut self) -> ArrayValue {
        if let Some(ch) = self.buffer.last() {
            if *ch != b'{' && *ch != b',' {
                self.buffer.push(b',');
            }
        }
        ArrayValue
    }
    pub fn end_json_array(&mut self) -> ArrayValue {
        if let Some(ch) = self.buffer.last() {
            if *ch == b',' {
                *ch = b']'
            } else {
                self.buffer.push(b']');
            }
        }
        ArrayValue
    }
    pub fn end_json_object(&mut self) -> ObjectValue {
        if let Some(ch) = self.buffer.last() {
            if *ch == b',' {
                *ch = b'}'
            } else {
                self.buffer.push(b'}');
            }
        }
        ObjectValue
    }
    pub fn end_json_string(&mut self) -> StringValue {
        self.buffer.push(b'"');
        StringValue
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
            let amount = ryu::raw::format64(value, self.buffer.head_ptr());
            self.buffer.advance(amount);
        }
    }
    pub fn finite_f32(&mut self, value: f32) {
        unsafe {
            self.buffer.reserve_small(16);
            let amount = ryu::raw::format32(value, self.buffer.head_ptr());
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
    pub unsafe fn push_unchecked_ascii(&mut self, text: u8) {
        self.buffer.push(text);
    }
    pub(crate) unsafe fn push_unchecked_utf8(&mut self, text: &[u8]) {
        self.buffer.push_bytes(text);
    }
}
