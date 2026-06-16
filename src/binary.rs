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
#![allow(unnecessary_transmutes)] // reduces code size

use super::{FromBinary, ToBinary};
use crate::BytesWriter;
use std::{
    alloc::Layout,
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
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
        if let Some(message) = self.error.take() {
            Some(FromBinaryError { message })
        } else if self.eof {
            Some(FromBinaryError {
                message: Cow::Borrowed("Unexpected EOF"),
            })
        } else {
            None
        }
    }
    pub fn new(slice: &'a [u8]) -> Self {
        let start = NonNull::new(slice.as_ptr() as *mut u8).unwrap();
        // SAFETY: adding `slice.len()` to `slice.as_ptr()` yields the
        // one-past-the-end pointer for the same slice allocation.
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
    pub error: Option<Cow<'static, str>>,
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
            // SAFETY: `byte_len <= remaining_size`, so advancing by `byte_len`
            // stays within the decoder's input allocation.
            self.start = unsafe { NonNull::new_unchecked(ptr.add(byte_len)) };
            if ptr.addr() & (layout.align() - 1) != 0 {
                // SAFETY: `byte_len` came from checked multiplication, and
                // `layout.align()` is a valid non-zero power-of-two alignment.
                let alloc = unsafe {
                    std::alloc::alloc(Layout::from_size_align_unchecked(byte_len, layout.align()))
                };
                if alloc.is_null() {
                    self.report_static_error("Allocation failed");
                    break 'return_empty;
                }
                // SAFETY: `alloc` points to `byte_len` bytes and `ptr` points
                // to `byte_len` remaining input bytes. The allocation is fresh,
                // so the regions do not overlap.
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

    /// # Safety
    ///
    /// `T` must be valid for every bit pattern, including all-zero bytes and
    /// the next `size_of::<T>()` bytes in the decoder. This is normally upheld
    /// by calling this only for `FromBinary::POD` types.
    pub unsafe fn pod_type<T>(&mut self) -> T {
        let size = size_of::<T>();
        if size > self.remaining_size() {
            self.eof = true;
            // SAFETY: the caller guarantees all-zero bytes are a valid `T`.
            unsafe { std::mem::zeroed() }
        } else {
            let addr = self.start;
            // SAFETY: `size <= remaining_size`, so advancing stays within the
            // decoder input. `read_unaligned` is used because the input bytes
            // may not be aligned for `T`, and the caller guarantees the bytes
            // are a valid `T` representation.
            unsafe {
                self.start = addr.add(size);
                addr.cast::<T>().read_unaligned()
            }
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
            // SAFETY: `byte_len <= remaining_size`, so advancing by `byte_len`
            // stays within the decoder's input allocation.
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
        // SAFETY: `start` and `end` are maintained as pointers into the same
        // input slice allocation, with `start <= end`.
        unsafe { self.end.as_ptr().offset_from(self.start.as_ptr()) as usize }
    }
    /// Reports a static error message.
    ///
    /// If no error has been set yet, this method stores the provided error message
    /// and resets the internal pointer to the start of the input.
    pub fn report_static_error(&mut self, error: &'static str) {
        if self.error.is_none() && !self.eof {
            self.error = Some(Cow::Borrowed(error));
            self.end = self.start;
        }
    }
    /// Reports a formatted error message.
    ///
    /// If no error has been set yet, this method stores the provided formatted error message
    /// and resets the internal pointer to the start of the input.
    pub fn report_error(&mut self, error: std::fmt::Arguments) {
        if self.error.is_none() && !self.eof {
            self.error = Some(Cow::Owned(error.to_string()));
            self.end = self.start;
        }
    }
    /// Reads a fixed-size byte array from the input.
    ///
    /// Returns a reference to the read array, or a zero-filled array if EOF is reached.
    pub fn byte_array<const N: usize>(&mut self) -> &'a [u8; N] {
        // SAFETY: after checking `N <= remaining_size`, the next `N` bytes are
        // inside the input slice. `[u8; N]` has alignment 1, so the cast is
        // aligned, and the returned reference is tied to the input lifetime.
        unsafe {
            if N > self.remaining_size() {
                self.eof = true;
                return &[0; N];
            }
            let ptr = self.start.as_ptr() as *const [u8; N];
            self.start = self.start.add(N);
            &*ptr
        }
    }

    /// Reads a single byte from the input.
    ///
    /// Returns the read byte, or 0 if EOF is reached.
    pub fn byte(&mut self) -> u8 {
        // SAFETY: when `start < end`, `start` points to at least one remaining
        // byte in the input slice.
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
        // SAFETY: when `start < end`, `start` points to one remaining byte in
        // the input slice. Advancing by one stays in bounds.
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
                let value = <u64 as FromBinary>::decode_binary(self);
                #[cfg(target_pointer_width = "64")]
                {
                    value as usize
                }
                #[cfg(not(target_pointer_width = "64"))]
                {
                    if value > usize::MAX as u64 {
                        self.report_static_error("Invalid length, overflowed");
                        0
                    } else {
                        value as usize
                    }
                }
            }
        }
    }

    /// Reads a slice of bytes from the input.
    ///
    /// Returns a reference to the read slice, or an empty slice if EOF is reached.
    pub fn byte_slice(&mut self, n: usize) -> &'a [u8] {
        // SAFETY: after checking `n <= remaining_size`, the next `n` bytes form
        // a valid subslice of the decoder input. `u8` alignment is 1.
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
            // SAFETY: `$ty` is an integer primitive: every bit pattern is
            // valid, there is no padding, and the implementation writes the
            // canonical little-endian scalar bytes required by the POD
            // contract.
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
            // SAFETY: same POD argument as `ToBinary`: integer primitives have
            // no padding and every byte pattern of the fixed-size little-endian
            // encoding is a valid `$ty`.
            unsafe impl<'a> FromBinary<'a> for $ty {
                const POD: bool = true;
                fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
                    // SAFETY: after the size check, the next `$sz` bytes are
                    // in bounds. Integer byte arrays have alignment 1, and
                    // advancing by `$sz` stays within the decoder input.
                    unsafe {
                        if decoder.remaining_size() < $sz {
                            decoder.eof = true;
                            return 0;
                        }
                        let ptr = decoder.start.as_ptr() as *const [u8; $sz];
                        decoder.start = decoder.start.add($sz);
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

// SAFETY: `u8` is a one-byte integer primitive. Every bit pattern is valid, so
// marking it POD satisfies the raw-copy contract.
unsafe impl ToBinary for u8 {
    const POD: bool = true;
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        encoder.push(*self)
    }
}

// SAFETY: same POD argument as `ToBinary for u8`; decoding consumes one byte
// and any byte is a valid `u8`.
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
            // SAFETY: each macro invocation pairs scalar types of equal size
            // with no padding. All bit patterns are valid for the source type
            // (`f32`/`f64`, signed integers), so POD raw copying is sound.
            unsafe impl ToBinary for $src {
                const POD: bool = true;
                fn encode_binary(&self, encoder: &mut BytesWriter) {
                    <$reuse>::encode_binary(
                        // SAFETY: each `$src`/`$reuse` pair has the same size
                        // and bit layout for raw encoding (`f*` with its IEEE
                        // integer bits, signed integers with same-width
                        // unsigned integers).
                        unsafe { &*(self as *const _ as *const $reuse) },
                        encoder
                    )
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        // SAFETY: same layout pairing as `encode_binary`, with
                        // unique access through `&mut self`.
                        unsafe { &mut *(self as *mut _ as *mut $reuse) }
                    )
                }
            }
            // SAFETY: the decoded `$reuse` scalar has the same size as `$src`,
            // and every bit pattern is valid for both sides of each listed
            // pair. The POD contract is therefore preserved.
            unsafe impl<'a> FromBinary<'a> for $src {
                const POD: bool = true;
                fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
                    // SAFETY: `$reuse` and `$src` are same-size scalar types,
                    // and every decoded `$reuse` bit pattern is a valid `$src`
                    // for these pairs.
                    unsafe {
                        std::mem::transmute::<$reuse, $src>(<$reuse>::decode_binary(decoder))
                    }
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        // SAFETY: same layout pairing as `encode_binary`, with
                        // unique access through `&mut self`.
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

// SAFETY: this impl does not opt into POD. It encodes through a checked `u8`
// representation, so no raw-copy invariants are claimed.
unsafe impl ToBinary for bool {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        encoder.push(*self as u8)
    }
}

// SAFETY: this impl does not opt into POD. It constructs a valid `bool` from a
// comparison result, not by reinterpreting arbitrary input bytes as `bool`.
unsafe impl<'a> FromBinary<'a> for bool {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        match u8::decode_binary(decoder) {
            0 => false,
            1 => true,
            value => {
                decoder.report_error(format_args!("Invalid bool tag: {value}"));
                false
            }
        }
    }
}

// Without length prefix
fn encode_array_body<T: ToBinary>(slice: &[T], encoder: &mut BytesWriter) {
    let can_memcopy = const { T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1) };
    if can_memcopy {
        let byte_size = std::mem::size_of_val(slice);
        // SAFETY: the `POD` contract says all initialized bytes of `T` may be
        // copied directly for this format. The endian condition ensures the raw
        // in-memory representation matches the encoded representation.
        encoder.push_bytes(unsafe {
            std::slice::from_raw_parts(slice.as_ptr().cast::<u8>(), byte_size)
        });
    } else {
        for value in slice {
            value.encode_binary(encoder)
        }
    }
}

// SAFETY: this impl does not opt into POD for slices themselves; elements are
// encoded one-by-one unless `T::POD` lets `encode_array_body` use its documented
// raw byte path.
unsafe impl<T: ToBinary> ToBinary for [T] {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        encode_array_body(self, encoder);
    }
}

// SAFETY: this impl does not mark `Cow<[T]>` as POD. The borrowed fast path is
// guarded by `T::POD`, endian compatibility, and decoder alignment checks; the
// fallback decodes into an owned `Vec<T>`.
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
            if const { size_of::<T>() == 0 } {
                let length = decoder.read_length();
                // SAFETY: ZST slices use a dangling aligned pointer and carry
                // no element storage. `NonNull::<T>::dangling()` is aligned for
                // `T`.
                Cow::Borrowed(unsafe {
                    std::slice::from_raw_parts(NonNull::<T>::dangling().as_ptr(), length)
                })
            } else {
                let (alloc, ptr, length) =
                    decoder.try_borrow_or_copy_untyped_slice(Layout::new::<T>());
                if alloc {
                    // SAFETY: `try_borrow_or_copy_untyped_slice` returned a
                    // fresh allocation containing `length` properly aligned POD
                    // elements of `T`; capacity equals length.
                    Cow::Owned(unsafe {
                        Vec::from_raw_parts(ptr as *const T as *mut T, length, length)
                    })
                } else {
                    // SAFETY: the decoder returned an input pointer aligned for
                    // `T` with `length` POD elements still borrowed from the
                    // original input.
                    Cow::Borrowed(unsafe { std::slice::from_raw_parts(ptr as *const T, length) })
                }
            }
        } else {
            Cow::Owned(Vec::<T>::decode_binary(decoder))
        }
    }
}

// SAFETY: this impl does not mark references as POD. The const assertion
// requires `T` to be POD and endian-compatible before returning a borrowed
// slice, and `try_borrow_untyped_slice` enforces bounds and alignment.
unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for &'a [T] {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        // ideally we would like avoid this post mono assertion
        // but until https://github.com/rust-lang/rust/issues/92827 is stabilized
        // this the best we can do.
        const { assert!(T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1)) }
        // todo: could get perf improvement through a bit specializing but opt builds probably fine
        // already.
        if const { size_of::<T>() == 0 } {
            let length = decoder.read_length();
            // SAFETY: ZST slices use a dangling aligned pointer and carry no
            // element storage.
            return unsafe {
                std::slice::from_raw_parts(NonNull::<T>::dangling().as_ptr(), length)
            };
        }
        let (ptr, length) = decoder.try_borrow_untyped_slice(Layout::new::<T>());
        // SAFETY: `try_borrow_untyped_slice` verifies the borrowed input pointer
        // is aligned for `T` and that the byte range contains `length` POD
        // elements.
        unsafe { std::slice::from_raw_parts(ptr as *const T, length) }
    }
}

// unsafe impl<'a> FromBinary<'a> for &'a [u8] {
//     fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
//         let len = decoder.read_length();
//         decoder.byte_slice(len)
//     }
// }
// SAFETY: this impl does not opt into POD; it delegates to the byte-slice
// encoder for the string's UTF-8 bytes.
unsafe impl ToBinary for String {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        self.as_bytes().encode_binary(encoder);
    }
}

// SAFETY: this impl does not opt into POD; `str` is encoded only as its valid
// UTF-8 byte sequence.
unsafe impl ToBinary for str {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        self.as_bytes().encode_binary(encoder);
    }
}

// SAFETY: this impl does not opt into POD. It decodes bytes through
// `str::from_utf8`, so only valid UTF-8 can become `&str`.
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

// SAFETY: `[T; N]` is POD exactly when `T` is POD. Rust arrays are contiguous
// repetitions of `T` with no inter-element padding, and endian transformation is
// applied per element on non-little-endian targets.
unsafe impl<'a, T: FromBinary<'a>, const N: usize> FromBinary<'a> for [T; N] {
    const POD: bool = T::POD;

    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        if T::POD {
            let byte_size = std::mem::size_of::<[T; N]>();
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                // SAFETY: `T::POD` means every copied bit pattern is a valid
                // `T`; `read_unaligned` handles arbitrary input alignment.
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
// SAFETY: same array POD argument as `FromBinary`: raw-copy encoding is used
// only through `encode_array_body`, which checks `T::POD` and endian
// compatibility.
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

// SAFETY: `Vec<T>` is not marked POD. Its raw-copy decode path is guarded by
// `T::POD`; otherwise elements are decoded and pushed through safe Vec APIs.
unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Vec<T> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        if T::POD {
            let Some(byte_size) = len.checked_mul(std::mem::size_of::<T>()) else {
                decoder.report_static_error("Invalid length, overflowed");
                return Vec::new();
            };
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                let mut vec: Vec<T> = Vec::with_capacity(len);
                // SAFETY: `vec` has capacity for `len` elements, and
                // `byte_size == len * size_of::<T>()`. `T::POD` guarantees the
                // copied bytes are valid initialized `T` values; `set_len`
                // follows only after the copy completes.
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
// SAFETY: `Vec<T>` is not marked POD; it encodes as a length plus the slice
// body, which relies on `T`'s own `ToBinary` implementation.
unsafe impl<T: ToBinary> ToBinary for Vec<T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        <[T]>::encode_binary(self, encoder);
    }
}

// SAFETY: references are not marked POD and only forward encoding to the
// referent's `ToBinary` implementation.
unsafe impl<T: ToBinary + ?Sized> ToBinary for &mut T {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (**self).encode_binary(encoder)
    }
}

// SAFETY: references are not marked POD and only forward encoding to the
// referent's `ToBinary` implementation.
unsafe impl<T: ToBinary + ?Sized> ToBinary for &T {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (**self).encode_binary(encoder)
    }
}

// SAFETY: maps are not marked POD. Encoding writes length and then delegates to
// the key/value `ToBinary` impls for each live entry.
unsafe impl<K: ToBinary, V: ToBinary, S> ToBinary for HashMap<K, V, S> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for (k, v) in self {
            k.encode_binary(encoder);
            v.encode_binary(encoder);
        }
    }
}

// SAFETY: maps are not marked POD. Decoding constructs entries through `K` and
// `V`'s `FromBinary` impls and inserts only fully constructed values.
unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, V: FromBinary<'a>, S: BuildHasher + Default>
    FromBinary<'a> for HashMap<K, V, S>
{
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map: Self = HashMap::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            let k = K::decode_binary(decoder);
            let v = V::decode_binary(decoder);
            if decoder.eof {
                break;
            }
            map.insert(k, v);
        }
        map
    }
}

// SAFETY: sets are not marked POD. Encoding writes length and delegates each
// element to `K`'s `ToBinary` impl.
unsafe impl<K: ToBinary, S> ToBinary for HashSet<K, S> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for k in self {
            k.encode_binary(encoder);
        }
    }
}

// SAFETY: sets are not marked POD. Decoding constructs elements through `K`'s
// `FromBinary` impl before inserting them into the set.
unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, S: BuildHasher + Default> FromBinary<'a>
    for HashSet<K, S>
{
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map: Self = HashSet::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            let k = K::decode_binary(decoder);
            if decoder.eof {
                break;
            }
            map.insert(k);
        }
        map
    }
}

// SAFETY: maps are not marked POD. Encoding writes length and delegates to
// key/value `ToBinary` impls for each live entry.
unsafe impl<K: ToBinary, V: ToBinary> ToBinary for BTreeMap<K, V> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for (k, v) in self {
            k.encode_binary(encoder);
            v.encode_binary(encoder);
        }
    }
}

// SAFETY: maps are not marked POD. Decoding constructs entries through `K` and
// `V`'s `FromBinary` impls and inserts only fully constructed values.
unsafe impl<'a, K: FromBinary<'a> + Ord, V: FromBinary<'a>> FromBinary<'a> for BTreeMap<K, V> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map = BTreeMap::new();
        for _ in 0..len {
            let k = K::decode_binary(decoder);
            let v = V::decode_binary(decoder);
            if decoder.eof {
                break;
            }
            map.insert(k, v);
        }
        map
    }
}

// SAFETY: sets are not marked POD. Encoding writes length and delegates each
// element to `K`'s `ToBinary` impl.
unsafe impl<K: ToBinary> ToBinary for BTreeSet<K> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for k in self {
            k.encode_binary(encoder);
        }
    }
}

// SAFETY: sets are not marked POD. Decoding constructs elements through `K`'s
// `FromBinary` impl before inserting them into the set.
unsafe impl<'a, K: FromBinary<'a> + Ord> FromBinary<'a> for BTreeSet<K> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut set = BTreeSet::new();
        for _ in 0..len {
            let k = K::decode_binary(decoder);
            if decoder.eof {
                break;
            }
            set.insert(k);
        }
        set
    }
}

// SAFETY: boxes are not marked POD. Encoding forwards to the initialized
// pointee's `ToBinary` implementation.
unsafe impl<T: ToBinary + ?Sized> ToBinary for Box<T> {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        T::encode_binary(self, encoder)
    }
}

// SAFETY: strings are not marked POD. Decoding first obtains a valid `&str`,
// then copies it into an owned `String`.
unsafe impl<'a> FromBinary<'a> for String {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <&str>::decode_binary(decoder).into()
    }
}

// SAFETY: boxed strings are not marked POD. Decoding first obtains a valid
// `&str`, then copies it into an owned `Box<str>`.
unsafe impl<'a> FromBinary<'a> for Box<str> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <&str>::decode_binary(decoder).into()
    }
}

// SAFETY: boxed slices are not marked POD. Decoding constructs a `Vec<T>` whose
// elements are valid by `T`'s `FromBinary` impl, then converts it to a box.
unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Box<[T]> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        <Vec<T>>::decode_binary(decoder).into()
    }
}

// SAFETY: boxes are not marked POD. Decoding constructs a valid `T` first, then
// places it in a `Box`.
unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Box<T> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        Box::new(T::decode_binary(decoder))
    }
}

// SAFETY: options are not marked POD. Encoding uses an explicit tag and only
// delegates to `T` when a value is present.
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

// SAFETY: options are not marked POD. Decoding constructs `Some(T)` only from a
// fully decoded `T`; malformed tags report a decoder error and return a
// placeholder `None`.
unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Option<T> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        match u8::decode_binary(decoder) {
            0 => None,
            1 => Some(T::decode_binary(decoder)),
            value => {
                decoder.report_error(format_args!("Invalid option tag: {value}"));
                None
            }
        }
    }
}

// SAFETY: `char` is not marked POD. Encoding writes the Unicode scalar value as
// a checked `u32` representation.
unsafe impl ToBinary for char {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        (*self as u32).encode_binary(encoder)
    }
}

// SAFETY: `char` is not marked POD. Decoding validates the `u32` with
// `char::from_u32` before constructing a `char`.
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

// SAFETY: `Duration` is not marked POD. Encoding uses the public seconds and
// nanoseconds accessors, preserving `Duration`'s invariant.
unsafe impl ToBinary for Duration {
    fn encode_binary(&self, encoder: &mut BytesWriter) {
        let secs = self.as_secs();
        let nanos = self.subsec_nanos();
        secs.encode_binary(encoder);
        nanos.encode_binary(encoder);
    }
}

// SAFETY: `Duration` is not marked POD. Decoding uses `Duration::new`, which
// constructs a valid normalized duration from the decoded components.
unsafe impl<'a> FromBinary<'a> for Duration {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        let secs = u64::decode_binary(decoder);
        let nanos = u32::decode_binary(decoder);
        Duration::new(secs, nanos)
    }
}

// SAFETY: `Cow<T>` is not marked POD. Encoding forwards to the borrowed target,
// so it claims no representation invariants for the enum itself.
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

// SAFETY: `Cow<str>` is not marked POD. Decoding obtains a UTF-8-validated
// borrowed `&str` and wraps it without changing its lifetime.
unsafe impl<'a> FromBinary<'a> for Cow<'a, str> {
    fn decode_binary(decoder: &mut Decoder<'a>) -> Self {
        Cow::Borrowed(<&'a str>::decode_binary(decoder))
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use crate::BytesWriter;
    use crate::from_binary;

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
            // SAFETY: setting the length to zero does not expose any
            // uninitialized bytes and preserves the writer's capacity.
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
    fn boxed_value() {
        // Box<T> forwards to the inner T's encoding, so a boxed value is byte
        // identical to the bare value and decodes back through Box::new.
        let mut output = BytesWriter::new();
        let value: u32 = 0xdead_beef;
        let mut bare = BytesWriter::new();
        value.encode_binary(&mut bare);
        let boxed: Box<u32> = Box::new(value);
        boxed.encode_binary(&mut output);
        assert_eq!(output.buffer_slice(), bare.buffer_slice());
        assert_eq!(
            from_binary::<Box<u32>>(output.buffer_slice()).unwrap(),
            boxed
        );

        let mut tester = Tester::default();
        tester.assert_roundtrip_all(&[Box::new(String::from("hello")), Box::new(String::new())]);
        tester.assert_roundtrip(&Box::new(vec![1u32, 2, 3]));
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
            // SAFETY: tuples are not marked POD here. Encoding delegates to
            // each field's `ToBinary` implementation in tuple order.
            unsafe impl<$($ty: ToBinary),*> ToBinary for ($($ty,)*) {
                fn encode_binary(&self, encoder: &mut BytesWriter) {
                    let ($($tn),*,) = self;
                    $($tn.encode_binary(encoder);)*
                }
            }
            // SAFETY: tuples are not marked POD here. Decoding constructs the
            // tuple directly from fully decoded field values in tuple order.
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
