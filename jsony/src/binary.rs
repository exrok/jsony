//! Binary encoding module for jsony.
//!
//! The Jasoni binary encoding format is a simple and straightforward format designed for efficiency.
//! Each field is serialized in order based on the definitions in derives.
//!
//! # Encoding Rules
//!
//! | Type | Encoding |
//! |------|----------|
//! | Basic integer types | Serialized directly in little-endian format |
//! | Collections (strings, maps, arrays) | Length followed by each component |
//! | Length encoding | For values < 255: single byte <br> For values >= 255: 0xFF byte followed by U64 in little-endian |
//!
//! # Implementation Details
//!
//! - Optimized for compile-time efficiency
//! - Decoding and encoding do not use `Result` for error handling
//! - Errors are stored in the decoder and checked at the end
//! - No support for optional values currently
//! - Struct derives result in straightforward, error-handling-free decoding
//!
//! # Optimization
//!
//! Binary encoding types can be marked as "plain old data" to enable direct memory copying,
//! which is particularly beneficial for arrays of scalars.
//!
//! This implementation differs from other crates in its focus on compile-time efficiency
//! and straightforward error handling approach.
#![allow(clippy::transmute_int_to_float, reason = "simpler to use same method")]

use super::{FromBinary, ToBinary};
use crate::BytesWriter;
use std::{
    alloc::Layout,
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hash},
    ptr::NonNull,
    time::Duration,
};

pub struct FromBinaryError {
    message: Cow<'static, str>,
}
impl FromBinaryError {
    pub fn message(&self) -> &str {
        &self.message
    }
}
impl std::fmt::Display for FromBinaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::fmt::Debug for FromBinaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for FromBinaryError {}

impl<'a> Decoder<'a> {
    pub fn consume_error(&mut self) -> Option<FromBinaryError> {
        if self.eof {
            Some(FromBinaryError {
                message: Cow::Borrowed("Unexpected EOF"),
            })
        } else {
            self.error
                .take()
                .map(|error| FromBinaryError { message: error })
        }
    }
    pub fn new(slice: &'a [u8]) -> Self {
        let start = NonNull::new(slice.as_ptr() as *mut u8).unwrap();
        let end = NonNull::new(unsafe { slice.as_ptr().add(slice.len()) as *mut u8 }).unwrap();
        Self {
            start,
            end,
            eof: false,
            error: None,
            _marker: std::marker::PhantomData,
        }
    }
}

/// A binary decoder for reading encoded data.
///
/// This struct allows reading bytes from the provided input according to the binary encoding format
/// specified in the `binary` module. It also keeps track of any errors encountered during parsing.
///
/// # Error Handling
///
/// When the end of file (EOF) is reached, an error is stored, and subsequent reads will return:
/// - `0` for individual bytes
/// - Zero-filled arrays
/// - Empty slices
pub struct Decoder<'a> {
    start: NonNull<u8>,
    end: NonNull<u8>,
    eof: bool,
    error: Option<Cow<'static, str>>,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Decoder<'a> {
    fn try_borrow_or_copy_untyped_slice(&mut self, layout: Layout) -> (bool, *const u8, usize) {
        'return_empty: {
            let len = self.read_length();
            if len == 0 {
                break 'return_empty;
            }
            let Some(byte_len) = len.checked_mul(layout.size()) else {
                self.report_static_error("Invalid length, overflowed");
                break 'return_empty;
            };
            if byte_len > self.remaining_size() {
                self.eof = true;
                break 'return_empty;
            }
            let ptr = self.start.as_ptr();
            self.start = unsafe { NonNull::new_unchecked(ptr.add(byte_len)) };
            if ptr.addr() & (layout.align() - 1) != 0 {
                let alloc = unsafe {
                    std::alloc::alloc(Layout::from_size_align_unchecked(byte_len, layout.align()))
                };
                if alloc.is_null() {
                    self.report_static_error("Allocation failed");
                    break 'return_empty;
                }
                unsafe {
                    std::ptr::copy_nonoverlapping(ptr, alloc, byte_len);
                }
                return (true, alloc, len);
            } else {
                return (false, ptr, len);
            }
        }
        return (false, std::ptr::without_provenance_mut(layout.align()), 0);
    }

    pub unsafe fn pod_type<T>(&mut self) -> T {
        let size = size_of::<T>();
        if size > self.remaining_size() {
            self.eof = true;
            unsafe { std::mem::zeroed() }
        } else {
            let addr = self.start;
            self.start = addr.add(size);
            addr.cast::<T>().read_unaligned()
        }
    }
    fn try_borrow_untyped_slice(&mut self, layout: Layout) -> (*const u8, usize) {
        'error: {
            let len = self.read_length();
            let Some(byte_len) = len.checked_mul(layout.size()) else {
                self.report_static_error("Invalid length, overflowed");
                break 'error;
            };
            if byte_len > self.remaining_size() {
                self.eof = true;
                break 'error;
            }
            let ptr = self.start.as_ptr();
            self.start = unsafe { NonNull::new_unchecked(ptr.add(byte_len)) };
            if ptr.addr() & (layout.align() - 1) != 0 {
                self.report_static_error("Attempted to borrowed slice of unaligned data");
                break 'error;
            }
            return (ptr, len);
        }
        return (std::ptr::without_provenance_mut(layout.align()), 0);
    }

    fn remaining_size(&self) -> usize {
        unsafe { self.end.as_ptr().offset_from(self.start.as_ptr()) as usize }
    }
    /// Reports a static error message.
    ///
    /// If no error has been set yet, this method stores the provided error message
    /// and resets the internal pointer to the start of the input.
    pub fn report_static_error(&mut self, error: &'static str) {
        if self.error.is_none() {
            self.error = Some(Cow::Borrowed(error));
            self.end = self.start;
        }
    }
    /// Reports a formatted error message.
    ///
    /// If no error has been set yet, this method stores the provided formatted error message
    /// and resets the internal pointer to the start of the input.
    pub fn report_error(&mut self, error: std::fmt::Arguments) {
        if self.error.is_none() {
            self.error = Some(Cow::Owned(error.to_string()));
            self.end = self.start;
        }
    }
    /// Reads a fixed-size byte array from the input.
    ///
    /// Returns a reference to the read array, or a zero-filled array if EOF is reached.
    pub fn byte_array<const N: usize>(&mut self) -> &'a [u8; N] {
        unsafe {
            let nstart = self.start.add(N);
            if nstart > self.end {
                self.eof = true;
                return &[0; N];
            }
            let ptr = self.start.as_ptr() as *const [u8; N];
            self.start = nstart;
            &*ptr
        }
    }

    /// Reads a single byte from the input.
    ///
    /// Returns the read byte, or 0 if EOF is reached.
    pub fn byte(&mut self) -> u8 {
        unsafe {
            if self.start.as_ptr() >= self.end.as_ptr() {
                self.eof = true;
                return 0;
            }
            let value = *self.start.as_ptr();
            self.start = self.start.add(1);
            value
        }
    }

    /// Reads a length value from the input.
    ///
    /// Returns the decoded length as a `usize`, or 0 if EOF is reached.
    pub fn read_length(&mut self) -> usize {
        unsafe {
            if self.start.as_ptr() >= self.end.as_ptr() {
                self.eof = true;
                return 0;
            }
            let value = *self.start.as_ptr();
            self.start = self.start.add(1);
            if value != 255 {
                value as usize
            } else {
                <u64 as FromBinary>::decode_binary(self) as usize
            }
        }
    }

    /// Reads a slice of bytes from the input.
    ///
    /// Returns a reference to the read slice, or an empty slice if EOF is reached.
    fn byte_slice(&mut self, n: usize) -> &'a [u8] {
        unsafe {
            if n > self.remaining_size() {
                self.eof = true;
                return &[];
            }
            let nstart = self.start.add(n);
            let ptr = std::slice::from_raw_parts(self.start.as_ptr(), n);
            self.start = nstart;
            ptr
        }
    }
}

/// Writes a length value to the given `BytesWriter` using Jsony's standard binary format.
///
/// # Implementation Details
///
/// - For lengths less than 255, the value is stored as a single byte.
/// - For lengths 255 or greater:
///   1. A byte containing 255 is emitted.
///   2. The length is then written as a u64 in little-endian format.
///
/// This encoding scheme optimizes for common (smaller) lengths while still supporting larger values.
/// The overhead of storing longer lengths is minimal compared to the size of the content they measure.
///
/// # Arguments
///
/// * `encoder` - The `BytesWriter` to write the length to.
/// * `len` - The length value to encode.
pub fn write_length(encoder: &mut BytesWriter, len: usize) {
    if len >= 255 {
        encoder.push(255);
        encoder.push_bytes(&(len as u64).to_le_bytes());
    } else {
        encoder.push(len as u8);
    }
}

macro_rules! impl_bincode_for_numeric_primitive {
    ($(( $ty:ty, $sz: tt )),* $(,)?) => {
        $(
            unsafe impl ToBinary for $ty {
                const POD: bool = true;
                fn encode_binary(&self, encoder: &mut BytesWriter) {
                    encoder.push_bytes(&self.to_le_bytes());
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    *self = self.to_le()
                }
            }
            unsafe impl<'a> FromBinary<'a> for $ty {
                const POD: bool = true;
                fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
                    unsafe {
                        let nstart = decoder.start.add($sz);
                        if nstart.as_ptr() > decoder.end.as_ptr() {
                            decoder.eof = true;
                            return 0;
                        }
                        let ptr = decoder.start.as_ptr() as *const [u8; $sz];
                        decoder.start = nstart;
                        <$ty>::from_le_bytes(*ptr)
                    }
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    *self = self.to_le()
                }
            }
        )*
    };
}

unsafe impl ToBinary for u8 {
    const POD: bool = true;
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        encoder.push(*self)
    }
}

unsafe impl<'a> FromBinary<'a> for u8 {
    const POD: bool = true;
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        decoder.byte()
    }
}

impl_bincode_for_numeric_primitive! {
    (u16, 2),
    (u32, 4),
    (u64, 8),
    (u128, 16),
}

macro_rules! impl_bincode_via_transmute {
    ($( $src:ty as $reuse: ty ),* $(,)?) => {
        $(
            unsafe impl ToBinary for $src {
                const POD: bool = true;
                fn encode_binary(&self, encoder: &mut BytesWriter) {
                    <$reuse>::encode_binary(
                        unsafe { &*(self as *const _ as *const $reuse) },
                        encoder
                    )
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        unsafe { &mut *(self as *mut _ as *mut $reuse) }
                    )
                }
            }
            unsafe impl<'a> FromBinary<'a> for $src {
                const POD: bool = true;
                fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
                    unsafe {
                        std::mem::transmute::<$reuse, $src>(<$reuse>::decode_binary(decoder))
                    }
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        unsafe { &mut *(self as *mut _ as *mut $reuse) }
                    )
                }
            }
        )*
    };
}

impl_bincode_via_transmute! {
    f32 as u32,
    f64 as u64,
    i8 as u8,
    i16 as u16,
    i32 as u32,
    i64 as u64,
    i128 as u128,
}

unsafe impl ToBinary for bool {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        encoder.push(*self as u8)
    }
}

unsafe impl<'a> FromBinary<'a> for bool {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        u8::decode_binary(decoder) == 1
    }
}

// Without length prefix
fn encode_array_body<T: ToBinary>(slice: &[T], encoder: &mut BytesWriter) {
    let can_memcopy = const { T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1) };
    if can_memcopy {
        let byte_size = std::mem::size_of_val(slice);
        encoder.push_bytes(unsafe {
            std::slice::from_raw_parts(slice.as_ptr().cast::<u8>(), byte_size)
        });
    } else {
        for value in slice {
            value.encode_binary(encoder)
        }
    }
}

unsafe impl<T: ToBinary> ToBinary for [T] {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        encode_array_body(self, encoder);
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Cow<'a, [T]>
where
    T: ToOwned + Clone,
{
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        // ideally we would like avoid this post mono assertion
        // but until https://github.com/rust-lang/rust/issues/92827 is stabilized
        // this the best we can do.
        if const { T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1) } {
            // todo: could get perf improvement through a bit specializing but opt builds probably fine
            // already.
            let (alloc, ptr, length) = decoder.try_borrow_or_copy_untyped_slice(Layout::new::<T>());
            if alloc {
                Cow::Owned(unsafe {
                    Vec::from_raw_parts(ptr as *const T as *mut T, length, length)
                })
            } else {
                Cow::Borrowed(unsafe { std::slice::from_raw_parts(ptr as *const T, length) })
            }
        } else {
            Cow::Owned(Vec::<T>::decode_binary(decoder))
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for &'a [T] {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        // ideally we would like avoid this post mono assertion
        // but until https://github.com/rust-lang/rust/issues/92827 is stabilized
        // this the best we can do.
        const { assert!(T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1)) }
        // todo: could get perf improvement through a bit specializing but opt builds probably fine
        // already.
        let (ptr, length) = decoder.try_borrow_untyped_slice(Layout::new::<T>());
        unsafe { std::slice::from_raw_parts(ptr as *const T, length) }
    }
}

// unsafe impl<'a> FromBinary<'a> for &'a [u8] {
//     fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
//         let len = decoder.read_length();
//         decoder.byte_slice(len)
//     }
// }
unsafe impl ToBinary for String {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        self.as_bytes().encode_binary(encoder);
    }
}

unsafe impl ToBinary for str {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        self.as_bytes().encode_binary(encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for &'a str {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        match std::str::from_utf8(<&[u8]>::decode_binary(decoder)) {
            Ok(s) => s,
            Err(err) => {
                decoder.report_error(format_args!("Invalid Utf-8 string: {err:?}"));
                ""
            }
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>, const N: usize> FromBinary<'a> for [T; N] {
    const POD: bool = T::POD;

    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        if T::POD {
            let byte_size = N * std::mem::size_of::<T>();
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                let array = unsafe { std::ptr::read_unaligned::<[T; N]>(bytes.as_ptr().cast()) };

                #[cfg(not(target_endian = "little"))]
                for value in &mut array {
                    T::endian_transform(value);
                }
                return array;
            }
        }
        std::array::from_fn(|_| T::decode_binary(decoder))
    }

    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {
        if T::POD {
            for value in self {
                T::endian_transform(value);
            }
        }
    }
}
unsafe impl<T: ToBinary, const N: usize> ToBinary for [T; N] {
    const POD: bool = T::POD;

    fn encode_binary(&self, encoder: &mut BytesWriter) {
        encode_array_body(self, encoder)
    }

    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {
        if T::POD {
            for value in self {
                T::endian_transform(value);
            }
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Vec<T> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        if T::POD {
            let byte_size = len * std::mem::size_of::<T>();
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                let mut vec: Vec<T> = Vec::with_capacity(len);
                unsafe {
                    std::ptr::copy_nonoverlapping::<u8>(
                        bytes.as_ptr(),
                        vec.as_mut_ptr().cast::<u8>(),
                        byte_size,
                    );
                    vec.set_len(len);
                }
                #[cfg(not(target_endian = "little"))]
                for value in &mut vec {
                    T::endian_transform(value);
                }
                vec
            } else {
                Vec::new()
            }
        } else {
            let mut vec = Vec::with_capacity(len.min(4096));
            for _ in 0..len {
                vec.push(T::decode_binary(decoder));
                if decoder.eof {
                    break;
                }
            }
            vec
        }
    }
}
unsafe impl<T: ToBinary> ToBinary for Vec<T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        <[T]>::encode_binary(self, encoder);
    }
}

unsafe impl<T: ToBinary + ?Sized> ToBinary for &mut T {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (**self).encode_binary(encoder)
    }
}

unsafe impl<T: ToBinary + ?Sized> ToBinary for &T {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (**self).encode_binary(encoder)
    }
}

unsafe impl<K: ToBinary, V: ToBinary, S> ToBinary for HashMap<K, V, S> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for (k, v) in self {
            k.encode_binary(encoder);
            v.encode_binary(encoder);
        }
    }
}

unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, V: FromBinary<'a>, S: BuildHasher + Default>
    FromBinary<'a> for HashMap<K, V, S>
{
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map: Self = HashMap::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            let k = K::decode_binary(decoder);
            let v = V::decode_binary(decoder);
            map.insert(k, v);
        }
        map
    }
}

unsafe impl<K: ToBinary, S> ToBinary for HashSet<K, S> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for k in self {
            k.encode_binary(encoder);
        }
    }
}

unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, S: BuildHasher + Default> FromBinary<'a>
    for HashSet<K, S>
{
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map: Self = HashSet::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            map.insert(K::decode_binary(decoder));
        }
        map
    }
}

unsafe impl<T: ToBinary> ToBinary for Box<T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        T::encode_binary(self, encoder)
    }
}

unsafe impl<'a> FromBinary<'a> for String {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <&str>::decode_binary(decoder).into()
    }
}

unsafe impl<'a> FromBinary<'a> for Box<str> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <&str>::decode_binary(decoder).into()
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Box<[T]> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <Vec<T>>::decode_binary(decoder).into()
    }
}

unsafe impl<T: ToBinary> ToBinary for Option<T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        if let Some(value) = self {
            encoder.push(1);
            value.encode_binary(encoder);
        } else {
            encoder.push(0);
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Option<T> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        if u8::decode_binary(decoder) == 1 {
            Some(T::decode_binary(decoder))
        } else {
            None
        }
    }
}

unsafe impl ToBinary for char {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (*self as u32).encode_binary(encoder)
    }
}

unsafe impl<'a> FromBinary<'a> for char {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let raw = u32::decode_binary(decoder);
        if let Some(ch) = char::from_u32(raw) {
            ch
        } else {
            decoder.report_error(format_args!(
                "Invalid char not a Unicode scalar value: 0x{:04x}",
                raw
            ));
            '\0'
        }
    }
}

unsafe impl ToBinary for Duration {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        let secs = self.as_secs();
        let nanos = self.subsec_nanos();
        secs.encode_binary(encoder);
        nanos.encode_binary(encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for Duration {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let secs = u64::decode_binary(decoder);
        let nanos = u32::decode_binary(decoder);
        Duration::new(secs, nanos)
    }
}

unsafe impl<'a, T: ToBinary + ToOwned + ?Sized> ToBinary for Cow<'a, T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        self.as_ref().encode_binary(encoder)
    }
}

// // Note we don't implement the blank impl for Cow so that we do the optimized thing
// // for the following. However, we provide a helper todo the owned version
// unsafe impl<'a> FromBinary<'a> for Cow<'a, [u8]> {
//     fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
//         Cow::Borrowed(<&'a [u8]>::decode_binary(decoder))
//     }
// }

unsafe impl<'a> FromBinary<'a> for Cow<'a, str> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        Cow::Borrowed(<&'a str>::decode_binary(decoder))
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use crate::from_binary;
    use crate::BytesWriter;

    use super::*;
    #[derive(Default)]
    struct Tester<'a> {
        buffer: BytesWriter<'a>,
    }
    impl Tester<'_> {
        #[track_caller]
        fn assert_roundtrip<'a, T: FromBinary<'a> + ToBinary + PartialEq + Debug>(
            &'a mut self,
            value: &T,
        ) {
            unsafe {
                self.buffer.set_len(0);
            }
            value.encode_binary(&mut self.buffer);
            let result = crate::from_binary::<T>(self.buffer.buffer_slice()).unwrap();
            assert_eq!(value, &result);
        }
        #[track_caller]
        fn assert_roundtrip_all<T: for<'a> FromBinary<'a> + ToBinary + PartialEq + Debug>(
            &'_ mut self,
            values: &[T],
        ) {
            for value in values {
                self.assert_roundtrip(value);
            }
        }
    }
    #[test]
    fn numbers() {
        let mut tester = Tester::default();
        tester.assert_roundtrip_all(&[i8::MIN, 0, i8::MAX]);
        tester.assert_roundtrip_all(&[u8::MIN, 0, u8::MAX]);
        tester.assert_roundtrip_all(&[u16::MIN, 0, u16::MAX]);
        tester.assert_roundtrip_all(&[i16::MIN, 0, i16::MAX]);
        tester.assert_roundtrip_all(&[u32::MIN, 0, u32::MAX]);
        tester.assert_roundtrip_all(&[i32::MIN, 0, i32::MAX]);
        tester.assert_roundtrip_all(&[u64::MIN, 0, u64::MAX]);
        tester.assert_roundtrip_all(&[i64::MIN, 0, i64::MAX]);
        tester.assert_roundtrip_all(&[u128::MIN, 0, u128::MAX]);
        tester.assert_roundtrip_all(&[i128::MIN, 0, i128::MAX]);
        tester.assert_roundtrip_all(&[f32::MIN, 0.0, f32::MAX]);
        tester.assert_roundtrip_all(&[f64::MIN, 0.0, f64::MAX]);
    }

    #[test]
    fn strings() {
        let a = String::from_utf8(vec![b'a'; 254]).unwrap();
        let b = String::from_utf8(vec![b'a'; 255]).unwrap();
        let c = String::from_utf8(vec![b'a'; 256]).unwrap();
        let inputs = ["", "hello", &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.encode_binary(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<&str>(encoded).unwrap(), *input);
        }
        let mut output: BytesWriter = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<str> = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<str>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: String = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(from_binary::<String>(output.buffer_slice()).unwrap(), boxed);
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Cow<'_, str> = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Cow<'_, str>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }

    #[test]
    fn duration() {
        let mut tester = Tester::default();
        tester.assert_roundtrip_all(&[Duration::from_millis(58345), Duration::MAX, Duration::ZERO]);
    }

    #[test]
    fn char() {
        let mut tester = Tester::default();
        tester.assert_roundtrip_all(&['a', '𝄞']);
        assert_eq!(
            crate::from_binary::<char>(&['x' as u8, 0, 0, 0]).unwrap(),
            'x'
        );
        assert!(crate::from_binary::<char>(&['x' as u8, 0, 0, 0xff]).is_err());
    }

    #[test]
    fn byte_arrays() {
        let a = vec![b'a'; 254];
        let b = vec![b'a'; 255];
        let c = vec![b'a'; 256];
        let inputs = [&b""[..], b"hello", &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.encode_binary(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<&[u8]>(encoded).unwrap(), *input);
        }
        let mut output = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<[u8]> = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<[u8]>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Vec<u8> = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Vec<u8>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }
    #[test]
    fn pod_arrays() {
        let a: Vec<u32> = (0..254u32)
            .map(|i| 0xdeadbeafu32.wrapping_mul(i) ^ i)
            .collect();
        let b = vec![b'a' as u32; 255];
        let c = vec![b'a' as u32; 256];
        let inputs = [&[][..], &[1u32, 2, 3, 4, 5], &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.encode_binary(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<Vec<u32>>(encoded).unwrap(), *input);
        }
        let mut output = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<[u32]> = (*input).into();
            boxed.encode_binary(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<[u32]>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }
    #[test]
    fn nested_pod_arrays() {
        let inputs: [&[[u32; 2]]; 2] = [&[][..], &[[1u32, 2], [3, 4], [5, 3]]];
        let encoded = inputs.map(|array| {
            let mut buf = BytesWriter::new();
            array.encode_binary(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<Vec<[u32; 2]>>(encoded).unwrap(), *input);
        }
    }
    #[test]
    fn complex_arrays() {
        let inputs: &[&str] = &["hello", "", "nice"];
        let encoded = crate::to_binary(inputs);
        assert_eq!(&from_binary::<Vec<&str>>(&encoded).unwrap(), inputs)
    }
}

macro_rules! tuple_impls {
    ($(($($tn: ident: $ty: tt),*)),* $(,)?) => {
        $(
            unsafe impl<$($ty: ToBinary),*> ToBinary for ($($ty,)*) {
                fn encode_binary(&self, encoder: &mut BytesWriter) {
                    let ($($tn),*,) = self;
                    $($tn.encode_binary(encoder);)*
                }
            }
            unsafe impl<'a, $($ty: FromBinary<'a>),*> FromBinary<'a> for ($($ty,)*) {
                fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
                    ($( $ty::decode_binary(decoder), )*)
                }
            }
        )*
    };
}

tuple_impls! {
    (t0: T0),
    (t0: T0, t1: T1),
    (t0: T0, t1: T1, t2: T2),
}
