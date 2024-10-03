#![allow(unused)]
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
};
#[doc(hidden)]
pub mod __internal;
pub mod binary;
pub mod json;
mod lazy_parser;
pub mod parser;
pub mod strings;
pub mod value;

/// If `POD` is `false` then this trait is safe to implement
/// `POD` = true implies that `Self` is valid for all bit
/// patterns and has no padding.
pub unsafe trait FromBinary<'a>: Sized {
    const POD: bool = false;

    fn binary_decode(decoder: &mut Decoder<'a>) -> Self;

    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

pub unsafe trait ToBinary {
    const POD: bool = false;
    fn binary_encode(&self, encoder: &mut Vec<u8>);

    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

pub unsafe trait FromJson<'a>: Sized + 'a {
    /// Safety to Call:
    /// - dest: must be an aligned pointer to self which is valid for writes.
    /// Safety to implemented:
    /// - If `Ok(())` is returned then `dest` must be initilized to a valid Self
    #[inline]
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match Self::decode_json(parser) {
            Ok(value) => {
                dest.cast::<Self>().write(value);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    // Allows users to relatively safety
    #[inline]
    fn decode_json(parser: &mut Parser<'a>) -> Result<Self, &'static DecodeError> {
        let mut value = std::mem::MaybeUninit::<Self>::uninit();
        if let Err(err) = unsafe {
            Self::emplace_from_json(NonNull::new_unchecked(value.as_mut_ptr()).cast(), parser)
        } {
            Err(err)
        } else {
            Ok(unsafe { value.assume_init() })
        }
    }
}

/// Lazy JSON parser
pub fn drill(input: &str) -> &lazy_parser::Value {
    lazy_parser::Value::new(input)
}

unsafe fn inner_from_json<'a>(
    value: NonNull<()>,
    func: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
    json: &'a [u8],
) -> Result<(), &'static DecodeError> {
    let mut parser = Parser::new(json);
    match unsafe { func(value, &mut parser) } {
        Ok(()) => Ok(()),
        Err(err) => {
            // todo add drop
            println!("Error At: {:?}", parser);
            Err(err)
        }
    }
}
#[inline]
pub fn from_json<'a, T: FromJson<'a>>(json: &'a str) -> Result<T, &'static DecodeError> {
    let mut value = std::mem::MaybeUninit::<T>::uninit();
    match unsafe {
        inner_from_json(
            NonNull::new_unchecked(value.as_mut_ptr()).cast(),
            T::emplace_from_json,
            json.as_bytes(),
        )
    } {
        Ok(()) => Ok(unsafe { value.assume_init() }),
        Err(err) => Err(err),
    }
}

pub fn to_json<T: ?Sized + ToJson>(value: &T) -> String {
    let mut buf = RawBuf::new();
    buf.value(value);
    buf.string
}

pub fn append_json<T: ?Sized + ToJson>(value: &T, output: &mut String) {
    let mut buf = unsafe { &mut *(output as *mut _ as *mut RawBuf) };
    buf.value(value);
}

pub fn from_binary<'a, T: FromBinary<'a>>(slice: &'a [u8]) -> Result<T, FromBinaryError> {
    let mut decoder = Decoder::new(slice);
    let value = Ok(T::binary_decode(&mut decoder));
    if let Some(error) = decoder.consume_error() {
        return Err(error);
    } else {
        value
    }
}

pub fn to_binary<T: ToBinary + ?Sized>(value: &T) -> Vec<u8> {
    let mut encoder = Vec::new();
    value.binary_encode(&mut encoder);
    encoder
}

use binary::{Decoder, FromBinaryError};
use json::DecodeError;
pub use jsony_macros::{append_object, array, object, Jsony};
use parser::Parser;

#[repr(transparent)]
pub struct ArrayBuf {
    pub buffer: RawBuf,
}

impl ArrayBuf {
    pub fn push<T: ToJson>(&mut self, value: &T) {
        self.buffer.value(value);
        self.buffer.push_comma();
    }
}

#[repr(transparent)]
pub struct ObjectBuf {
    pub buffer: RawBuf,
}

impl ObjectBuf {
    pub fn key<'a, T: ToJson<Into = StringBuf> + ?Sized>(
        &'a mut self,
        key: &T,
    ) -> ObjectValueWriter<'a> {
        self.buffer.key(key);
        self.buffer.push_colon();
        ObjectValueWriter {
            buffer: &mut self.buffer,
        }
    }
}

#[repr(transparent)]
pub struct ObjectValueWriter<'a> {
    pub buffer: &'a mut RawBuf,
}

impl<'a> ObjectValueWriter<'a> {
    pub fn value<T: ToJson>(mut self, value: &T) {
        self.buffer.value(value);
        self.buffer.push_comma();
    }
}

#[repr(transparent)]
pub struct StringBuf {
    pub buffer: RawBuf,
}

impl StringBuf {
    fn push(&mut self, ch: char) {
        ch.encode_utf8(&mut [0; 4]).jsonify_into(self);
    }
    fn push_str(&mut self, s: &str) {
        s.jsonify_into(self);
    }
}

impl std::fmt::Write for StringBuf {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        s.jsonify_into(self);
        Ok(())
    }
}

#[repr(transparent)]
pub struct RawBuf {
    pub string: String,
}

impl RawBuf {
    #[inline]
    pub fn push_close_array(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b']');
        }
    }
    #[inline]
    pub fn push_open_array(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b'[');
        }
    }
    #[inline]
    pub fn push_open_object(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b'{');
        }
    }
    #[inline]
    pub fn push_close_object(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b'}');
        }
    }
    #[inline]
    pub fn push_colon(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b':');
        }
    }
    #[inline]
    pub fn smart_close_object(&mut self) {
        unsafe {
            let buf = self.string.as_mut_vec();
            if let Some(x @ b',') = buf.last_mut() {
                *x = b'}';
            } else {
                buf.push(b'}');
            }
        }
    }
    #[inline]
    pub fn smart_close_array(&mut self) {
        unsafe {
            let buf = self.string.as_mut_vec();
            if let Some(x @ b',') = buf.last_mut() {
                *x = b']';
            } else {
                buf.push(b']');
            }
        }
    }
    #[inline]
    pub fn push_comma(&mut self) {
        unsafe {
            self.string.as_mut_vec().push(b',');
        }
    }
    pub const fn new() -> RawBuf {
        RawBuf {
            string: String::new(),
        }
    }
    pub fn with_capacity(capacity: usize) -> RawBuf {
        RawBuf {
            string: String::with_capacity(capacity),
        }
    }
    pub fn raw_array<'a>(&'a mut self, value: &impl ToJson<Into = ArrayBuf>) {
        let field: &'a mut _ = unsafe { &mut *(self as *mut _ as *mut ArrayBuf) };
        value.jsonify_into(field);
    }
    pub fn raw_object<'a>(&'a mut self, value: &impl ToJson<Into = ObjectBuf>) {
        let field: &'a mut _ = unsafe { &mut *(self as *mut _ as *mut ObjectBuf) };
        value.jsonify_into(field);
    }
    pub fn raw_key<'a>(&'a mut self, value: &impl ToJson<Into = StringBuf>) {
        let field: &'a mut _ = unsafe { &mut *(self as *mut _ as *mut StringBuf) };
        value.jsonify_into(field);
    }
    pub fn key<K: ToJson<Into = StringBuf> + ?Sized>(&mut self, value: &K) {
        let field: &mut _ = StringBuf::from_builder(self);
        value.jsonify_into(field);
        field.terminate()
    }
    pub fn value<K: ToJson + ?Sized>(&mut self, value: &K) {
        let field: &mut _ = <_ as OutputBuffer>::from_builder(self);
        value.jsonify_into(field);
        field.terminate()
    }
}

impl<'a, K: OutputBuffer> std::ops::Deref for Writer<'a, K> {
    type Target = K;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl<'a, K: OutputBuffer> std::ops::DerefMut for Writer<'a, K> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
pub struct Writer<'a, K: OutputBuffer>(&'a mut K);

impl<'a, K: OutputBuffer> Drop for Writer<'a, K> {
    fn drop(&mut self) {
        self.0.terminate();
    }
}
mod __private {
    pub trait Sealed {}
}
impl __private::Sealed for ArrayBuf {}
impl __private::Sealed for RawBuf {}
impl __private::Sealed for StringBuf {}
impl __private::Sealed for ObjectBuf {}

pub trait OutputBuffer: __private::Sealed {
    #[doc(hidden)]
    fn from_builder(builder: &mut RawBuf) -> &mut Self;
    #[doc(hidden)]
    fn terminate(&mut self);
}

impl OutputBuffer for RawBuf {
    #[inline]
    fn from_builder(builder: &mut RawBuf) -> &mut Self {
        builder
    }
    fn terminate(&mut self) {}
}
impl RawBuf {
    pub fn writer<K: OutputBuffer>(&mut self) -> Writer<'_, K> {
        Writer(K::from_builder(self))
    }
}

impl OutputBuffer for StringBuf {
    #[inline]
    fn from_builder(builder: &mut RawBuf) -> &mut Self {
        unsafe {
            builder.string.as_mut_vec().push(b'"');
            &mut *(builder as *mut _ as *mut StringBuf)
        }
    }
    fn terminate(&mut self) {
        unsafe {
            self.buffer.string.as_mut_vec().push(b'"');
        }
    }
}
impl OutputBuffer for ArrayBuf {
    #[inline]
    fn from_builder(builder: &mut RawBuf) -> &mut Self {
        builder.push_open_array();
        unsafe { &mut *(builder as *mut _ as *mut ArrayBuf) }
    }
    fn terminate(&mut self) {
        unsafe {
            let buf = self.buffer.string.as_mut_vec();
            if let Some(x @ b',') = buf.last_mut() {
                *x = b']';
            } else {
                buf.push(b']');
            }
        }
    }
}
impl ArrayBuf {
    pub fn from_builder_raw(builder: &mut RawBuf) -> &mut ArrayBuf {
        unsafe { &mut *(builder as *mut _ as *mut ArrayBuf) }
    }
}
impl ObjectBuf {
    pub fn from_builder_raw(builder: &mut RawBuf) -> &mut ObjectBuf {
        unsafe { &mut *(builder as *mut _ as *mut ObjectBuf) }
    }
}
impl OutputBuffer for ObjectBuf {
    #[inline]
    fn from_builder(builder: &mut RawBuf) -> &mut Self {
        builder.push_open_object();
        unsafe { &mut *(builder as *mut _ as *mut ObjectBuf) }
    }
    fn terminate(&mut self) {
        unsafe {
            let buf = self.buffer.string.as_mut_vec();
            if let Some(x @ b',') = buf.last_mut() {
                *x = b'}';
            } else {
                buf.push(b'}');
            }
        }
    }
}

pub trait ToJson {
    type Into: OutputBuffer;
    fn jsonify_into(&self, output: &mut Self::Into);
}

macro_rules! into_json_itoa {
    ($($ty:ty)*) => {
        $(
            impl ToJson for $ty {
                type Into = RawBuf;
                fn jsonify_into(&self, x: &mut RawBuf) {
                    let mut buffer = itoa::Buffer::new();
                    x.string.push_str(buffer.format(*self));
                }
            }
        )*
    };
}

into_json_itoa![u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize];

impl ToJson for char {
    type Into = StringBuf;
    fn jsonify_into(&self, output: &mut StringBuf) {
        output.buffer.string.push(*self);
    }
}

impl ToJson for f32 {
    type Into = RawBuf;
    fn jsonify_into(&self, x: &mut RawBuf) {
        if !self.is_finite() {
            x.string.push_str("null");
            return;
        }
        unsafe {
            let v = x.string.as_mut_vec();
            v.reserve(16);
            let amount = ryu::raw::format32(*self, v.spare_capacity_mut().as_mut_ptr() as *mut u8);
            v.set_len(v.len() + amount);
        }
    }
}
impl ToJson for f64 {
    type Into = RawBuf;
    fn jsonify_into(&self, x: &mut RawBuf) {
        if !self.is_finite() {
            x.string.push_str("null");
            return;
        }
        unsafe {
            let v = x.string.as_mut_vec();
            v.reserve(24);
            let amount = ryu::raw::format64(*self, v.spare_capacity_mut().as_mut_ptr() as *mut u8);
            v.set_len(v.len() + amount);
        }
    }
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
    __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];

enum CharEscape {
    /// An escaped quote `"`
    Quote,
    /// An escaped reverse solidus `\`
    ReverseSolidus,
    /// An escaped solidus `/`
    Solidus,
    /// An escaped backspace character (usually escaped as `\b`)
    Backspace,
    /// An escaped form feed character (usually escaped as `\f`)
    FormFeed,
    /// An escaped line feed character (usually escaped as `\n`)
    LineFeed,
    /// An escaped carriage return character (usually escaped as `\r`)
    CarriageReturn,
    /// An escaped tab character (usually escaped as `\t`)
    Tab,
    /// An escaped ASCII plane control character (usually escaped as
    /// `\u00XX` where `XX` are two hex characters)
    AsciiControl(u8),
}

impl CharEscape {
    #[inline]
    fn from_escape_table(escape: u8, byte: u8) -> CharEscape {
        match escape {
            self::BB => CharEscape::Backspace,
            self::TT => CharEscape::Tab,
            self::NN => CharEscape::LineFeed,
            self::FF => CharEscape::FormFeed,
            self::RR => CharEscape::CarriageReturn,
            self::QU => CharEscape::Quote,
            self::BS => CharEscape::ReverseSolidus,
            self::UU => CharEscape::AsciiControl(byte),
            _ => unreachable!(),
        }
    }
}

impl ToJson for str {
    type Into = StringBuf;
    fn jsonify_into(&self, output: &mut StringBuf) {
        let bytes = self.as_bytes();
        let mut start = 0;
        unsafe {
            let buf = output.buffer.string.as_mut_vec();
            for (i, &byte) in bytes.iter().enumerate() {
                let escape = ESCAPE[byte as usize];
                if escape == 0 {
                    continue;
                }

                if start < i {
                    buf.extend_from_slice(&bytes[start..i]);
                }

                let char_escape = CharEscape::from_escape_table(escape, byte);
                use self::CharEscape::*;
                start = i + 1;

                let s = match char_escape {
                    Quote => b"\\\"",
                    ReverseSolidus => b"\\\\",
                    Solidus => b"\\/",
                    Backspace => b"\\b",
                    FormFeed => b"\\f",
                    LineFeed => b"\\n",
                    CarriageReturn => b"\\r",
                    Tab => b"\\t",
                    AsciiControl(byte) => {
                        static HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";
                        let bytes = &[
                            b'\\',
                            b'u',
                            b'0',
                            b'0',
                            HEX_DIGITS[(byte >> 4) as usize],
                            HEX_DIGITS[(byte & 0xF) as usize],
                        ];
                        buf.extend_from_slice(bytes);
                        continue;
                    }
                };
                buf.extend_from_slice(s);
            }

            if start == bytes.len() {
                return;
            }

            buf.extend_from_slice(&bytes[start..]);
        }
    }
}

impl<T: ToJson> ToJson for Option<T> {
    type Into = RawBuf;
    fn jsonify_into(&self, output: &mut RawBuf) {
        if let Some(value) = self {
            output.value(value);
        } else {
            output.string.push_str("null");
        }
    }
}

impl<T: ToJson, const N: usize> ToJson for [T; N] {
    type Into = ArrayBuf;
    fn jsonify_into(&self, array: &mut ArrayBuf) {
        self.as_slice().jsonify_into(array)
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    type Into = ArrayBuf;
    fn jsonify_into(&self, array: &mut ArrayBuf) {
        self.as_slice().jsonify_into(array);
    }
}

impl<T: ToJson> ToJson for [T] {
    type Into = ArrayBuf;
    fn jsonify_into(&self, array: &mut ArrayBuf) {
        for value in self {
            let x = T::Into::from_builder(&mut array.buffer);
            <_ as ToJson>::jsonify_into(&value, x);
            x.terminate();
            array.buffer.push_comma();
        }
    }
}

impl<'a, T: ToJson + ?Sized + Clone> ToJson for Cow<'a, T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(&*self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Rc<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(&*self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Box<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(&*self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Arc<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(&*self, output)
    }
}

impl ToJson for String {
    type Into = StringBuf;

    fn jsonify_into(&self, output: &mut Self::Into) {
        self.as_str().jsonify_into(output)
    }
}
impl<T: ToJson + ?Sized> ToJson for &T {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(*self, output)
    }
}

impl<V: ToJson, S> ToJson for HashSet<V, S> {
    type Into = ArrayBuf;

    fn jsonify_into(&self, output: &mut Self::Into) {
        for value in self {
            output.buffer.value(value);
            output.buffer.push_comma();
        }
    }
}
impl<K: ToJson<Into = StringBuf>, V: ToJson, S> ToJson for HashMap<K, V, S> {
    type Into = ObjectBuf;

    fn jsonify_into(&self, output: &mut Self::Into) {
        for (key, value) in self {
            output.buffer.key(key);
            output.buffer.push_colon();
            output.buffer.value(value);
            output.buffer.push_comma();
        }
    }
}

impl ToJson for bool {
    type Into = RawBuf;
    fn jsonify_into(&self, output: &mut RawBuf) {
        if *self {
            output.string.push_str("true");
        } else {
            output.string.push_str("false");
        }
    }
}
