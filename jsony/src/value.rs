#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum Kind {
    Null,
    True,
    False,
    String,
    Number,
    Map,
    Array,
    Empty,
}
use std::{ops::Index, ptr::NonNull};

use Kind::{Empty, False, Map, Null, Number, True};

const HASH_MAP_THRESHHOLD: usize = 8;

use crate::{
    json::{decode_object_sequence, AnyValue, DecodeError},
    parser::Peek,
    FromJson, ToJson,
};

impl Kind {
    pub(crate) const STRINGLY_MAX_LENGTH: u32 = Kind::Number.swizzeled() | 0x1FFF_FFFF;
    pub(crate) const fn swizzeled(self) -> u32 {
        (self as u32) << 29
    }
    pub(crate) const fn matches_length(self, len: u32) -> bool {
        self.swizzeled() == (len & (!0x1FFF_FFFF))
    }
}

// #[cfg(target_pointer_width = "64")]
// const K: usize = 0xf1357aea2e62a9c5;
// #[cfg(target_pointer_width = "32")]
// const K: usize = 0x93d765dd;

// Nothing special, digits of pi.
const SEED1: u64 = 0x243f6a8885a308d3;
const SEED2: u64 = 0x13198a2e03707344;
const PREVENT_TRIVIAL_ZERO_COLLAPSE: u64 = 0xa4093822299f31d0;

#[inline]
fn multiply_mix(x: u64, y: u64) -> u64 {
    #[cfg(target_pointer_width = "64")]
    {
        // We compute the full u64 x u64 -> u128 product, this is a single mul
        // instruction on x86-64, one mul plus one mulhi on ARM64.
        let full = (x as u128) * (y as u128);
        let lo = full as u64;
        let hi = (full >> 64) as u64;

        // The middle bits of the full product fluctuate the most with small
        // changes in the input. This is the top bits of lo and the bottom bits
        // of hi. We can thus make the entire output fluctuate with small
        // changes to the input by XOR'ing these two halves.
        lo ^ hi

        // Unfortunately both 2^64 + 1 and 2^64 - 1 have small prime factors,
        // otherwise combining with + or - could result in a really strong hash, as:
        //     x * y = 2^64 * hi + lo = (-1) * hi + lo = lo - hi,   (mod 2^64 + 1)
        //     x * y = 2^64 * hi + lo =    1 * hi + lo = lo + hi,   (mod 2^64 - 1)
        // Multiplicative hashing is universal in a field (like mod p).
    }

    #[cfg(target_pointer_width = "32")]
    {
        // u64 x u64 -> u128 product is prohibitively expensive on 32-bit.
        // Decompose into 32-bit parts.
        let lx = x as u32;
        let ly = y as u32;
        let hx = (x >> 32) as u32;
        let hy = (y >> 32) as u32;

        // u32 x u32 -> u64 the low bits of one with the high bits of the other.
        let afull = (lx as u64) * (hy as u64);
        let bfull = (hx as u64) * (ly as u64);

        // Combine, swapping low/high of one of them so the upper bits of the
        // product of one combine with the lower bits of the other.
        afull ^ bfull.rotate_right(32)
    }
}

/// A wyhash-inspired non-collision-resistant hash for strings/slices designed
/// by Orson Peters, with a focus on small strings and small codesize.
///
/// The 64-bit version of this hash passes the SMHasher3 test suite on the full
/// 64-bit output, that is, f(hash_bytes(b) ^ f(seed)) for some good avalanching
/// permutation f() passed all tests with zero failures. When using the 32-bit
/// version of multiply_mix this hash has a few non-catastrophic failures where
/// there are a handful more collisions than an optimal hash would give.
///
/// We don't bother avalanching here as we'll feed this hash into a
/// multiplication after which we take the high bits, which avalanches for us.
#[inline]
fn hash_bytes(bytes: &[u8]) -> u64 {
    let len = bytes.len();
    let mut s0 = SEED1;
    let mut s1 = SEED2;

    if len <= 16 {
        // XOR the input into s0, s1.
        if len >= 8 {
            s0 ^= u64::from_le_bytes(bytes[0..8].try_into().unwrap());
            s1 ^= u64::from_le_bytes(bytes[len - 8..].try_into().unwrap());
        } else if len >= 4 {
            s0 ^= u32::from_le_bytes(bytes[0..4].try_into().unwrap()) as u64;
            s1 ^= u32::from_le_bytes(bytes[len - 4..].try_into().unwrap()) as u64;
        } else if len > 0 {
            let lo = bytes[0];
            let mid = bytes[len / 2];
            let hi = bytes[len - 1];
            s0 ^= lo as u64;
            s1 ^= ((hi as u64) << 8) | mid as u64;
        }
    } else {
        // Handle bulk (can partially overlap with suffix).
        let mut off = 0;
        while off < len - 16 {
            let x = u64::from_le_bytes(bytes[off..off + 8].try_into().unwrap());
            let y = u64::from_le_bytes(bytes[off + 8..off + 16].try_into().unwrap());

            // Replace s1 with a mix of s0, x, and y, and s0 with s1.
            // This ensures the compiler can unroll this loop into two
            // independent streams, one operating on s0, the other on s1.
            //
            // Since zeroes are a common input we prevent an immediate trivial
            // collapse of the hash function by XOR'ing a constant with y.
            let t = multiply_mix(s0 ^ x, PREVENT_TRIVIAL_ZERO_COLLAPSE ^ y);
            s0 = s1;
            s1 = t;
            off += 16;
        }

        let suffix = &bytes[len - 16..];
        s0 ^= u64::from_le_bytes(suffix[0..8].try_into().unwrap());
        s1 ^= u64::from_le_bytes(suffix[8..16].try_into().unwrap());
    }

    multiply_mix(s0, s1) ^ (len as u64)
}

// Compact Cow
pub struct JsonKey<'a> {
    // we store to capacity only for dropping later, 0 capacity indicates shared
    capacity: u32,
    length: u32,
    ptr: NonNull<u8>,
    marker: std::marker::PhantomData<&'a ()>,
}
unsafe impl<'a> FromJson<'a> for JsonKey<'a> {
    fn json_decode(
        parser: &mut crate::parser::Parser<'a>,
    ) -> Result<Self, &'static crate::json::DecodeError> {
        let slice = parser.take_cow_string()?;
        if let Some(cow) = JsonKey::try_from_cow(slice) {
            Ok(cow)
        } else {
            Err(&DecodeError {
                message: "Json Key too large",
            })
        }
    }
}

impl<'a> JsonKey<'a> {
    pub fn try_from_cow(cow: std::borrow::Cow<'a, str>) -> Option<Self> {
        match cow {
            std::borrow::Cow::Borrowed(slice) => JsonKey::try_from_str(slice),
            std::borrow::Cow::Owned(string) => JsonKey::try_from_string(string),
        }
    }
    pub fn try_from_string(mut string: String) -> Option<Self> {
        let len = string.len();
        if len > MAX_LENGTH {
            return None;
        }
        if string.capacity() > u32::MAX as usize {
            string.shrink_to_fit();
        }
        let mut string = std::mem::ManuallyDrop::new(string);
        let capacity = string.capacity() as u32;
        let length = len as u32;
        let ptr = string.as_mut_ptr();
        Some(JsonKey {
            capacity,
            length,
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            marker: std::marker::PhantomData,
        })
    }

    pub fn try_from_str(slice: &'a str) -> Option<Self> {
        let len = slice.len();
        if len > u32::MAX as usize {
            return None;
        }
        let ptr = slice.as_ptr() as *mut u8;
        Some(JsonKey {
            capacity: 0,
            length: len as u32,
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            marker: std::marker::PhantomData,
        })
    }
    pub fn as_str(&self) -> &'a str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.ptr.as_ptr(),
                self.length as usize,
            ))
        }
    }
}

impl<'a> std::ops::Deref for JsonKey<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<'a> std::fmt::Debug for JsonKey<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl<'a> Drop for JsonKey<'a> {
    fn drop(&mut self) {
        if self.capacity > 0 {
            unsafe {
                Vec::from_raw_parts(
                    self.ptr.as_ptr(),
                    self.length as usize,
                    self.capacity as usize,
                );
            }
        }
    }
}

const MAX_LENGTH: usize = 0x1FFF_FFFF;
const KIND_OFFSET: usize = 29;
pub struct JsonItem<'a> {
    capacity: u32,
    /// We use the top 3 bits, to store the kind.
    /// This limits arrays and such to 512MB
    length: u32,
    ptr: NonNull<()>,
    marker: std::marker::PhantomData<&'a ()>,
}

// nocheckin: not sure it should be both?????
unsafe impl Send for JsonItem<'_> {}
unsafe impl Sync for JsonItem<'_> {}

impl std::fmt::Debug for JsonItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.json_type() {
            Kind::Array => self.as_array().unwrap().fmt(f),
            Kind::Map => {
                let entries = self.as_object_entries().unwrap();
                let mut map = f.debug_map();
                for (key, value) in entries {
                    map.entry(key, value);
                }
                map.finish()
            }
            Kind::Empty => f.write_str("Empty"),
            Kind::String => self.as_str().unwrap().fmt(f),
            _ => f.write_str(self.as_str().unwrap()),
        }
    }
}

impl<'a> Drop for JsonItem<'a> {
    fn drop(&mut self) {
        match self.json_type() {
            Kind::Array => {
                unsafe {
                    Vec::from_raw_parts(
                        self.ptr.as_ptr() as *mut JsonItem,
                        self.length as usize & 0x1FFF_FFFF,
                        self.capacity as usize,
                    );
                };
            }
            Kind::Map => unsafe {
                Vec::from_raw_parts(
                    self.ptr.as_ptr() as *mut (JsonKey<'a>, JsonItem<'a>),
                    self.length as usize & 0x1FFF_FFFF,
                    self.capacity as usize,
                );
            },
            Kind::String => {
                if self.capacity > 0 {
                    unsafe {
                        Vec::from_raw_parts(
                            self.ptr.as_ptr() as *mut u8,
                            self.length as usize & 0x1FFF_FFFF,
                            self.capacity as usize,
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

impl<'a> ToJson for JsonItem<'a> {
    type Kind = AnyValue;

    fn json_encode__jsony(&self, output: &mut crate::TextWriter) -> Self::Kind {
        match self.json_type() {
            Null => output.push_str("null"),
            True => output.push_str("true"),
            False => output.push_str("false"),
            Kind::String => {
                self.as_str().unwrap().json_encode__jsony(output);
            }
            Number => {
                output.push_str(self.as_str().unwrap());
            }
            Map => {
                output.start_json_object();
                for (key, value) in self.as_object_entries().unwrap() {
                    key.json_encode__jsony(output);
                    output.push_colon();
                    value.json_encode__jsony(output);
                    output.push_comma();
                }
                output.end_json_object();
            }
            Kind::Array => {
                self.as_array().unwrap().json_encode__jsony(output);
            }
            Empty => output.push_str("null"),
        };
        AnyValue
    }
}
unsafe impl<'a> FromJson<'a> for JsonItem<'a> {
    fn json_decode(
        parser: &mut crate::parser::Parser<'a>,
    ) -> Result<Self, &'static crate::json::DecodeError> {
        match parser.peek()? {
            Peek::Array => Ok(JsonItem::new_array(
                <Vec<JsonItem<'a>> as FromJson<'a>>::json_decode(parser)?,
            )),
            Peek::Object => {
                let mut data = Vec::<(JsonKey<'a>, JsonItem<'a>)>::new();
                decode_object_sequence(parser, |k: JsonKey<'a>, v: JsonItem<'a>| {
                    data.push((k, v));
                    Ok(())
                })?;
                Ok(JsonItem::new_map(data))
            }
            Peek::String => Ok(JsonItem::new_string(
                <JsonKey<'a> as FromJson<'a>>::json_decode(parser)?,
            )),
            Peek::True => {
                parser.discard_seen_true()?;
                return Ok(JsonItem::new(b"true", True));
            }
            Peek::False => {
                parser.discard_seen_false()?;
                return Ok(JsonItem::new(b"false", False));
            }
            Peek::Null => {
                parser.discard_seen_null()?;
                return Ok(JsonItem::new(b"null", Null));
            }
            //assume number
            _ => {
                let slice = parser.consume_numeric_literal()?;
                if !is_json_number(slice.as_bytes()) {
                    return Err(&DecodeError {
                        message: "Invalid number",
                    });
                }
                Ok(JsonItem::new(slice.as_bytes(), Number))
            }
        }
    }
}

fn is_json_number(value: &[u8]) -> bool {
    let mut i = 0;

    // Check for optional leading '-'
    if let Some(&b'-') = value.get(i) {
        i += 1;
    }

    // Check for integer part
    match value.get(i) {
        Some(b'0') => {
            i += 1;
            // If '0' is followed by another digit, it's invalid
            if let Some(&digit) = value.get(i) {
                if digit.is_ascii_digit() {
                    return false;
                }
            }
        }
        Some(b'1'..=b'9') => {
            i += 1;
            while let Some(&digit) = value.get(i) {
                if !digit.is_ascii_digit() {
                    break;
                }
                i += 1;
            }
        }
        _ => return false, // No valid integer part found
    }

    // Check for optional fractional part
    if let Some(&b'.') = value.get(i) {
        i += 1;
        // Fractional part must have at least one digit
        match value.get(i) {
            Some(digit) if digit.is_ascii_digit() => {
                i += 1;
                while let Some(&digit) = value.get(i) {
                    if !digit.is_ascii_digit() {
                        break;
                    }
                    i += 1;
                }
            }
            _ => return false, // Invalid or missing fractional part
        }
    }

    // Check for optional exponent part
    if let Some(&e) = value.get(i) {
        if e == b'e' || e == b'E' {
            i += 1;
            // Optional sign in exponent
            if let Some(&sign) = value.get(i) {
                if sign == b'+' || sign == b'-' {
                    i += 1;
                }
            }
            // Exponent part must have at least one digit
            match value.get(i) {
                Some(digit) if digit.is_ascii_digit() => {
                    i += 1;
                    while let Some(&digit) = value.get(i) {
                        if !digit.is_ascii_digit() {
                            break;
                        }
                        i += 1;
                    }
                }
                _ => return false, // Invalid or missing exponent part
            }
        }
    }

    // Ensure we've consumed the entire slice
    i == value.len()
}

impl<'a> JsonItem<'a> {
    fn json_type(&self) -> Kind {
        unsafe { std::mem::transmute::<u8, Kind>((self.length >> KIND_OFFSET) as u8) }
    }

    pub(crate) fn new_string(raw_key: JsonKey<'a>) -> Self {
        let raw_key = std::mem::ManuallyDrop::new(raw_key);
        JsonItem {
            capacity: raw_key.capacity,
            length: raw_key.length | Kind::String.swizzeled(),
            ptr: unsafe { NonNull::new_unchecked(raw_key.ptr.as_ptr() as *mut ()) },
            marker: std::marker::PhantomData,
        }
    }
    pub(crate) fn new(slice: &'a [u8], json_type: Kind) -> Self {
        //todo should assert not json map nor json array nor string
        if slice.len() > MAX_LENGTH {
            return JsonItem {
                capacity: 0,
                //todo better error for this
                length: Kind::Empty.swizzeled(),
                ptr: NonNull::dangling(),
                marker: std::marker::PhantomData,
            };
        }
        JsonItem {
            capacity: 0,
            length: slice.len() as u32 | json_type.swizzeled(),
            ptr: unsafe { NonNull::new_unchecked(slice.as_ptr() as *const _ as *mut ()) },
            marker: std::marker::PhantomData,
        }
    }

    pub(crate) fn new_array(mut array: Vec<JsonItem<'a>>) -> Self {
        if array.len() > MAX_LENGTH {
            return JsonItem::empty();
        }
        if array.capacity() > u32::MAX as usize {
            array.shrink_to_fit();
        }
        let capacity = array.capacity() as u32;
        let length = array.len() as u32 | Kind::Array.swizzeled();
        let ptr = unsafe { NonNull::new_unchecked(array.as_mut_ptr() as *mut ()) };
        std::mem::forget(array);
        JsonItem {
            capacity,
            length,
            ptr,
            marker: std::marker::PhantomData,
        }
    }
    pub(crate) fn new_map(mut map: Vec<(JsonKey<'a>, JsonItem<'a>)>) -> Self {
        if map.len() > MAX_LENGTH {
            return JsonItem::empty();
        }
        if map.capacity() > u32::MAX as usize {
            map.shrink_to_fit();
        }
        if map.len() >= HASH_MAP_THRESHHOLD {
            assert_eq!(size_of::<(JsonKey<'a>, JsonItem<'a>)>(), 8 * 4);
            map.reserve_exact((map.len() + 1) / 2);

            // - 1 to avoid bad cases.
            let len = (map.len() * 4) - 1;
            unsafe {
                // println!("{:?}", map.spare_capacity_mut().len());
                let extra = map.spare_capacity_mut().as_mut_ptr().cast::<u64>();
                // println!("{}", *extra);
                // zero the memeory
                for i in 0..map.len() {
                    *extra.add(i) = 0;
                }
                let hashes = std::slice::from_raw_parts_mut(extra.cast::<u32>(), len);

                // TODO be more careful about not invalidating the pointers via not accessing map
                'next_key: for (i, (key, _)) in map.iter().enumerate() {
                    let index = hash_bytes(key.as_bytes()) as usize % hashes.len();
                    for x in &mut hashes[index..] {
                        if *x == 0 {
                            *x = i as u32 | 0x8000_0000;
                            continue 'next_key;
                        } else {
                            // mark probe overflow
                            *x |= 0x4000_0000;
                        }
                    }
                    for x in &mut hashes[..index] {
                        if *x == 0 {
                            *x = i as u32 | 0x8000_0000;
                            continue 'next_key;
                        } else {
                            // mark probe overflow
                            *x |= 0x4000_0000;
                        }
                    }
                    unreachable!()
                }
                // let len = map.len() * 2;

                // map.sort_by(|(a, _), (b, _)| fast_string_ord(a.as_str(), b.as_str()))
            }
        }
        let capacity = map.capacity() as u32;
        let length = map.len() as u32 | Kind::Map.swizzeled();
        let ptr = unsafe { NonNull::new_unchecked(map.as_mut_ptr() as *mut ()) };
        std::mem::forget(map);
        JsonItem {
            capacity,
            length,
            ptr,
            marker: std::marker::PhantomData,
        }
    }
    pub const fn empty() -> Self {
        JsonItem {
            capacity: 0,
            length: Kind::Empty.swizzeled(),
            ptr: NonNull::dangling(),
            marker: std::marker::PhantomData,
        }
    }
    // must be Stringly kind
    unsafe fn as_bytes(&self) -> &'a [u8] {
        // Safety: parse consumes a UTF-8 slice and we always split at char boundaries
        // thus the resulting slice is still UTF-8.
        unsafe {
            std::slice::from_raw_parts(
                self.ptr.as_ptr() as *const u8,
                (self.length & 0x000F_FFFF) as usize,
            )
        }
    }

    pub fn as_str(&self) -> Option<&'a str> {
        if self.length > Kind::STRINGLY_MAX_LENGTH {
            None
        } else {
            // Safety: parse consumes a UTF-8 slice and we always split at char boundaries
            // thus the resulting slice is still UTF-8.
            Some(unsafe { std::str::from_utf8_unchecked(self.as_bytes()) })
        }
    }
    pub fn as_f64(&self) -> Option<f64> {
        if Number.matches_length(self.length) {
            self.as_str()?.parse::<f64>().ok()
        } else {
            None
        }
    }

    pub fn as_i128(&self) -> Option<i128> {
        if Number.matches_length(self.length) {
            self.as_str()?.parse::<i128>().ok()
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self.json_type() {
            True => Some(true),
            False => Some(false),
            _ => None,
        }
    }

    pub fn is_null(&self) -> bool {
        Null.matches_length(self.length)
    }
    pub fn is_empty_item(&self) -> bool {
        self.length == 0
    }

    pub fn exists(&self) -> bool {
        self.json_type() != Empty
    }

    pub fn get_type(&self) -> Kind {
        self.json_type()
    }
    pub fn entries(&self) -> Option<std::slice::Iter<'a, (JsonKey<'a>, JsonItem<'a>)>> {
        if self.json_type() == Map {
            let len = self.length & 0x1FFF_FFFF;
            let ptr = self.ptr.as_ptr() as *const (JsonKey<'a>, JsonItem<'a>);
            Some(unsafe { std::slice::from_raw_parts(ptr, len as usize).iter() })
        } else {
            None
        }
    }
    pub fn elements(&self) -> Option<std::slice::Iter<'a, JsonItem<'a>>> {
        if self.json_type() == Kind::Array {
            let len = self.length & 0x1FFF_FFFF;
            let ptr = self.ptr.as_ptr() as *const JsonItem<'a>;
            Some(unsafe { std::slice::from_raw_parts(ptr, len as usize).iter() })
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<&'a [JsonItem<'a>]> {
        if self.json_type() == Kind::Array {
            let len = self.length & 0x1FFF_FFFF;
            let ptr = self.ptr.as_ptr() as *const JsonItem<'a>;
            Some(unsafe { std::slice::from_raw_parts(ptr, len as usize) })
        } else {
            None
        }
    }
    pub fn as_object_entries(&self) -> Option<&'a [(JsonKey<'a>, JsonItem<'a>)]> {
        if self.json_type() == Kind::Map {
            let len = self.length & 0x1FFF_FFFF;
            let ptr = self.ptr.as_ptr() as *const (JsonKey<'a>, JsonItem<'a>);
            Some(unsafe { std::slice::from_raw_parts(ptr, len as usize) })
        } else {
            None
        }
    }
    pub fn index_object(&self, key: &str) -> &JsonItem<'a> {
        if self.json_type() == Kind::Map {
            let len = (self.length & 0x1FFF_FFFF) as usize;
            let ptr = self.ptr.as_ptr() as *const (JsonKey<'a>, JsonItem<'a>);
            let items = unsafe { std::slice::from_raw_parts(ptr, len) };
            if len < HASH_MAP_THRESHHOLD {
                for (k, v) in items {
                    if k.as_str() == key {
                        return v;
                    }
                }
            } else {
                let hashes = unsafe {
                    std::slice::from_raw_parts(ptr.add(len).cast::<u32>(), (len * 4) - 1)
                };
                let index = hash_bytes(key.as_bytes()) as usize % hashes.len();
                let first_cell = hashes[index];
                if first_cell >= 0x8000_0000 {
                    let idx = first_cell & 0x1FFF_FFFF;
                    if items[idx as usize].0.as_str() == key {
                        return &items[idx as usize].1;
                    }
                }
                if first_cell < 0xC000_0000 {
                    return &EMPTY_ITEM;
                }
                for x in &hashes[index..] {
                    let v = *x;
                    let idx = v & 0x1FFF_FFFF;
                    if items[idx as usize].0.as_str() == key {
                        return &items[idx as usize].1;
                    }
                    if v < 0xC000_0000 {
                        return &EMPTY_ITEM;
                    }
                }
                for x in &hashes[..index] {
                    let v = *x;
                    let idx = v & 0x1FFF_FFFF;
                    if items[idx as usize].0.as_str() == key {
                        return &items[idx as usize].1;
                    }
                    if v < 0xC000_0000 {
                        return &EMPTY_ITEM;
                    }
                }

                // for (item_hash, item) in hashes.iter().zip(items) {
                //     if hash != *item_hash {
                //         continue;
                //     }
                //     if (item.0.as_str() == key) {
                //         return &item.1;
                //     }
                // }
            }
        }
        &EMPTY_ITEM
    }
}

static EMPTY_ITEM: JsonItem<'static> = JsonItem::empty();
impl<'a> std::ops::Index<&str> for JsonItem<'a> {
    type Output = JsonItem<'a>;

    fn index(&self, index: &str) -> &Self::Output {
        self.index_object(index)
    }
}

impl<'a> Index<usize> for JsonItem<'a> {
    type Output = JsonItem<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        if let Some(array) = self.as_array() {
            if index < array.len() {
                return &array[index];
            }
        }
        &EMPTY_ITEM
    }
}
