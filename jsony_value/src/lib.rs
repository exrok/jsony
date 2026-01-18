//! Memory-efficient JSON value types for `jsony`.
//!
//! Values use a 16-byte tagged union representation, allowing
//! [`Option<Value>`] to also be 16 bytes through niche optimization.
//!
//! # Key Types
//!
//! - [`Value`] - Tagged union representing any JSON value
//! - [`ValueString`] - Strings that can be borrowed or owned
//! - [`ValueNumber`] - Discriminated union of `U64`, `I64`, `F64`
//! - [`ValueList`] - Growable array of values
//! - [`ValueMap`] - JSON object support duplicate keys
//!
//! # Examples
//!
//! ```
//! use jsony_value::{Value, ValueString, ValueMap, ValueList};
//!
//! // Create values from Rust types
//! let num: Value = 42i64.into();
//! let text: Value = "hello".into();
//! let flag: Value = true.into();
//!
//! // Build a JSON object
//! let map: Value = [
//!     ("name", Value::from("Alice")),
//!     ("age", Value::from(30i64)),
//! ].into_iter().collect();
//! ```

/// Type alias for the hash builder used in [`ValueMap`].
pub type DefaultHashBuilder = foldhash::fast::RandomState;
pub(crate) mod strings;

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;

#[macro_use]
mod macros;
mod binary;
mod control;
mod value_list;
mod value_map;
fn oom() -> ! {
    panic!()
}
use jsony::{
    json::{DecodeError, Peek},
    FromJson,
};
use strings::fmt_string;
pub use value_list::ValueList;
pub use value_map::{Entry, MatchIter, OccupiedEntry, VacantEntry, ValueMap, ValueMapBuilder};
use value_map::{ObjectEntry, INDEX_ENTRY_COUNT};

#[repr(C, align(8))]
struct NumberHeader {
    tag: u32,
    _pad: u32,
}

impl NumberHeader {
    const I64: u64 = unsafe {
        std::mem::transmute::<_, u64>(NumberHeader {
            tag: (Kind::Number as u32) | (1 << 4),
            _pad: 0,
        })
    };
    const U64: u64 = unsafe {
        std::mem::transmute::<_, u64>(NumberHeader {
            tag: (Kind::Number as u32) | (2 << 4),
            _pad: 0,
        })
    };
    const F64: u64 = unsafe {
        std::mem::transmute::<_, u64>(NumberHeader {
            tag: (Kind::Number as u32) | (3 << 4),
            _pad: 0,
        })
    };
}

/// Represents a JSON boolean value.
///
/// The boolean is stored as a `u64` where `0` represents `false` and any
/// non-zero value represents `true`.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ValueBoolean {
    tag: CapacityTag,
    meta: u32,
    /// The raw boolean value. Zero is `false`, non-zero is `true`.
    pub value: u64,
}

impl std::cmp::PartialEq<bool> for ValueBoolean {
    fn eq(&self, other: &bool) -> bool {
        (self.value != 0) == *other
    }
}

impl PartialEq for ValueBoolean {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

/// Represents a JSON number as one of three numeric types.
///
/// JSON numbers are parsed into the most appropriate representation:
/// - Positive integers that fit in `u64` become [`ValueNumber::U64`]
/// - Negative integers that fit in `i64` become [`ValueNumber::I64`]
/// - Numbers with decimal points or exponents become [`ValueNumber::F64`]
///
/// # Examples
///
/// ```
/// use jsony_value::ValueNumber;
///
/// let unsigned = ValueNumber::U64(42);
/// assert_eq!(unsigned.as_u64(), Some(42));
///
/// let signed = ValueNumber::I64(-10);
/// assert_eq!(signed.as_i64(), Some(-10));
///
/// let float = ValueNumber::F64(3.14);
/// assert_eq!(float.as_f64(), Some(3.14));
/// ```
#[repr(u64, C)]
#[derive(Copy, Clone, Debug)]
pub enum ValueNumber {
    /// An unsigned 64-bit integer.
    U64(u64) = NumberHeader::U64,
    /// A signed 64-bit integer.
    I64(i64) = NumberHeader::I64,
    /// A 64-bit floating-point number.
    F64(f64) = NumberHeader::F64,
}

impl PartialEq for ValueNumber {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::U64(l0), Self::U64(r0)) => l0 == r0,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::F64(l0), Self::F64(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl std::fmt::Display for ValueNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueNumber::U64(n) => n.fmt(f),
            ValueNumber::I64(n) => n.fmt(f),
            ValueNumber::F64(n) => {
                if n.is_finite() {
                    n.fmt(f)
                } else {
                    f.write_str("null")
                }
            }
        }
    }
}

impl From<ValueNumber> for Value<'_> {
    fn from(value: ValueNumber) -> Self {
        unsafe { std::mem::transmute::<ValueNumber, Value<'static>>(value) }
    }
}

impl ValueNumber {
    /// Converts the number to `f64`.
    ///
    /// Returns [`Some(f64)`] for all variants, converting integers to floating-point.
    ///
    /// [`Some(f64)`]: Option::Some
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            ValueNumber::I64(num) => Some(*num as f64),
            ValueNumber::U64(num) => Some(*num as f64),
            ValueNumber::F64(num) => Some(*num),
        }
    }

    /// Converts the number to `u64` if possible.
    ///
    /// Returns [`None`] if the number is negative or a floating-point value.
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            ValueNumber::I64(num) => {
                if *num >= 0 {
                    Some(*num as u64)
                } else {
                    None
                }
            }
            ValueNumber::U64(num) => Some(*num),
            ValueNumber::F64(..) => None,
        }
    }

    /// Converts the number to `i64` if possible.
    ///
    /// Returns [`None`] if the number is a floating-point value or exceeds `i64::MAX`.
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ValueNumber::I64(num) => Some(*num),
            ValueNumber::U64(num) => {
                if *num <= i64::MAX as u64 {
                    Some(*num as i64)
                } else {
                    None
                }
            }
            ValueNumber::F64(..) => None,
        }
    }
}

impl<'a> Value<'a> {
    /// The JSON null value.
    ///
    /// # Examples
    ///
    /// ```
    /// use jsony_value::{Value, ValueRef};
    ///
    /// let null = Value::NULL;
    /// assert!(matches!(null.as_ref(), ValueRef::Null(_)));
    /// ```
    pub const NULL: Value<'a> = Value {
        tag: CapacityTag::NULL,
        meta: 0,
        data: ValueData { null: () },
        marker: PhantomData,
    };
}

#[derive(Clone, Copy)]
#[allow(unused)]
union ValueData<'a> {
    null: (),
    uint: u64,
    int: i64,
    float: f64,
    string: NonNull<u8>,
    list_items: NonNull<Value<'a>>,
    map_entries: NonNull<ObjectEntry<'a>>,
}

use std::num::NonZeroU32;

// This tag stores discriminant for the value in the lowest 3 bits,
// If the 4th bit is set if and only if the type is non-copy and requires
// drop
#[derive(Clone, Copy, PartialEq, Eq)]
struct CapacityTag {
    raw: NonZeroU32,
}

const MIN_CAPACITY: u32 = 1 << 3;
impl CapacityTag {
    fn is_copy(self) -> bool {
        (self.raw.get() & 0b1_000) == 0
    }
    fn kind(self) -> Kind {
        unsafe { std::mem::transmute::<u8, Kind>((self.raw.get() & 0b111) as u8) }
    }
    fn capacity(self) -> u32 {
        self.raw.get() & !(0b111)
    }
    fn map_entry_capacity(self) -> i64 {
        (self.raw.get() as i64).wrapping_sub((Kind::Map as i64) + (INDEX_ENTRY_COUNT as i64))
    }
    const fn new_list(capacity: u32) -> CapacityTag {
        debug_assert!(capacity & 0b111 == 0);
        CapacityTag {
            // Saftey: Kind::List is non-zero
            raw: unsafe { NonZeroU32::new_unchecked(capacity | (Kind::List as u32)) },
        }
    }
    fn map_has_index(self) -> bool {
        self.raw.get() > (MIN_CAPACITY | Kind::Map as u32)
    }
    const fn new_map(capacity: u32) -> CapacityTag {
        CapacityTag {
            // Saftey: Kind::List is non-zero
            raw: unsafe { NonZeroU32::new_unchecked(capacity | (Kind::Map as u32)) },
        }
    }
    const OWNED_OTHER: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(0b1_000 | (Kind::Other as u32)),
        }
    };
    const NULL: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(Kind::Null as u32),
        }
    };
    const BOOLEAN: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(Kind::Boolean as u32),
        }
    };
    const BORROWED_OTHER: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(Kind::Other as u32),
        }
    };
    const OWNED_STRING: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(0b1_000 | (Kind::String as u32)),
        }
    };
    const BORROWED_STRING: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(Kind::String as u32),
        }
    };
    const STATIC_STRING: CapacityTag = unsafe {
        CapacityTag {
            raw: NonZeroU32::new_unchecked(0b10_000 | (Kind::String as u32)),
        }
    };
}

/// A compact tagged union representing any JSON value.
///
/// `Value` can hold null, booleans, numbers, strings, arrays, or objects.
/// It is optimized for size, and [`Option<Value>`] has the same size as `Value`.
///
/// Borrowed data (strings from parsed JSON) avoids allocation. Call [`to_owned`]
/// to convert borrowed data to `'static` lifetime.
///
/// # Examples
///
/// ```
/// use jsony_value::{Value, ValueRef};
///
/// let value: Value = 42i64.into();
/// if let ValueRef::Number(n) = value.as_ref() {
///     assert_eq!(n.as_i64(), Some(42));
/// }
/// ```
///
/// [`Option<Value>`]: std::option::Option
/// [`to_owned`]: Value::to_owned
#[repr(C, align(8))]
pub struct Value<'a> {
    tag: CapacityTag,
    meta: u32,
    data: ValueData<'a>,
    marker: PhantomData<&'a ()>,
}

impl<'a, 'b> PartialEq<Value<'b>> for Value<'a> {
    fn eq(&self, other: &Value<'b>) -> bool {
        match (self.as_ref(), other.as_ref()) {
            (ValueRef::Null(_), ValueRef::Null(_)) => true,
            (ValueRef::Number(n1), ValueRef::Number(n2)) => n1 == n2,
            (ValueRef::String(s1), ValueRef::String(s2)) => s1 == s2,
            (ValueRef::Other(s1), ValueRef::Other(s2)) => s1 == s2,
            (ValueRef::Map(m1), ValueRef::Map(m2)) => m1 == m2,
            (ValueRef::List(l1), ValueRef::List(l2)) => l1 == l2,
            (ValueRef::Boolean(b1), ValueRef::Boolean(b2)) => b1 == b2,
            _ => false,
        }
    }
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.as_ref() {
            ValueRef::Null(_) => f.write_str("null"),
            ValueRef::Number(n) => match n {
                ValueNumber::U64(n) => n.fmt(f),
                ValueNumber::I64(n) => n.fmt(f),
                ValueNumber::F64(n) => n.fmt(f),
            },
            ValueRef::String(s) => fmt_string(s, f),
            ValueRef::Other(s) => fmt_string(s, f),
            ValueRef::Map(map) => {
                f.write_str("{")?;
                let mut first = true;
                for (k, v) in map.entries() {
                    if !first {
                        f.write_str(", ")?;
                    }
                    first = false;
                    fmt_string(k, f)?;
                    f.write_str(": ")?;
                    v.fmt(f)?;
                }
                f.write_str("}")
            }
            ValueRef::List(list) => {
                f.write_str("[")?;
                let mut first = true;
                for v in list.as_slice() {
                    if !first {
                        f.write_str(", ")?;
                    }
                    first = false;
                    v.fmt(f)?;
                }
                f.write_str("]")
            }
            ValueRef::Boolean(boolean) => (boolean.value != 0).fmt(f),
        }
    }
}
impl std::fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.as_ref() {
            ValueRef::Null(_) => f.write_str("Null"),
            ValueRef::Number(n) => n.fmt(f),
            ValueRef::String(s) => write!(f, "String({:?})", s.as_str()),
            ValueRef::Other(s) => write!(f, "Other({:?})", s.as_str()),
            ValueRef::Map(map) => {
                f.write_str("Map ")?;
                map.fmt(f)
            }
            ValueRef::List(list) => {
                f.write_str("List ")?;
                list.fmt(f)
            }
            ValueRef::Boolean(boolean) => (boolean.value != 0).fmt(f),
        }
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(value: &'a str) -> Self {
        ValueString::from_borrowed(value).into()
    }
}

impl<'a> From<Box<str>> for ValueString<'a> {
    fn from(value: Box<str>) -> Self {
        ValueString::from_owned(value)
    }
}

impl<'a> From<&'a str> for ValueString<'a> {
    fn from(value: &'a str) -> Self {
        ValueString::from_borrowed(value)
    }
}

impl<'a> From<ValueString<'a>> for Value<'a> {
    fn from(value: ValueString<'a>) -> Self {
        let value = ManuallyDrop::new(value);
        Value {
            tag: value.tag,
            meta: value.len,
            data: ValueData { string: value.ptr },
            marker: PhantomData,
        }
    }
}

impl<'a> From<ValueList<'a>> for Value<'a> {
    fn from(value: ValueList<'a>) -> Self {
        let value = ManuallyDrop::new(value);
        Value {
            tag: value.tag,
            meta: value.len,
            data: ValueData {
                list_items: value.ptr,
            },
            marker: PhantomData,
        }
    }
}

impl<'a> From<ValueBoolean> for Value<'a> {
    fn from(value: ValueBoolean) -> Self {
        Value {
            tag: value.tag,
            meta: value.meta,
            data: ValueData { uint: value.value },
            marker: PhantomData,
        }
    }
}

// # Value Representation Tricks
// On 64 bit platforms Value is has a size of 16 bytes and is aligned to 8 bytes.
// meaning it get passed around with two registers. Additionally, contains a niche
// so that `Option<Value>` is also 16 bytes.
//
// Genernally we avoid boxing, a borrowed string does not allocate. However to achive
// this that capacity has been limited to a maximum of `u32::MAX`. We use the lower
// bits of capacity sa the type tag and force alignment of capacity. This means that
// Vec cannot cheaply be converted to Value type in general.
// if tag & 0b1_000 == 0, then value is Copy and needs no drop.

impl<'a> Clone for Value<'a> {
    fn clone(&self) -> Self {
        if self.tag.is_copy() {
            return Value {
                tag: self.tag,
                meta: self.meta,
                data: self.data,
                marker: PhantomData,
            };
        }
        match self.as_ref() {
            ValueRef::String(string) | ValueRef::Other(string) => string.clone().into(),
            ValueRef::Map(map) => map.clone().into(),
            ValueRef::List(list) => list.clone().into(),
            _ => {
                unreachable!()
            }
        }
    }
}

impl<'a> Drop for Value<'a> {
    fn drop(&mut self) {
        if self.tag.is_copy() {
            return;
        }
        match self.tag.kind() {
            Kind::List => unsafe {
                std::ptr::drop_in_place(self.as_list_unchecked());
            },
            Kind::Map => unsafe {
                std::ptr::drop_in_place(self.as_map_unchecked());
            },
            Kind::Other | Kind::String => unsafe {
                std::ptr::drop_in_place(self.as_string_unchecked());
            },
            Kind::Number | Kind::Null | Kind::Boolean => unsafe {
                std::hint::unreachable_unchecked()
            },
        }
    }
}

impl<'a> Value<'a> {
    /// Recursively sorts all objects in this value by their keys.
    ///
    /// This is useful for deterministic serialization or comparison.
    pub fn sort_all_objects(&mut self) {
        match self.as_mut() {
            ValueMut::Map(m) => m.sort(),
            ValueMut::List(l) => {
                for i in l.as_mut_slice() {
                    i.sort_all_objects();
                }
            }
            _ => (),
        }
    }

    fn to_owned_in_place(&mut self) {
        match self.as_mut() {
            ValueMut::Other(s) | ValueMut::String(s) => s.to_owned_in_place(),
            ValueMut::Map(m) => m.to_owned_in_place(),
            ValueMut::List(l) => l.to_owned_in_place(),
            ValueMut::Number(_) | ValueMut::Null(_) | ValueMut::Boolean(_) => {
                return;
            }
        }
    }

    /// Converts borrowed data to owned, returning a `'static` lifetime value.
    ///
    /// Strings that reference external data are copied to the heap.
    /// Values that are already owned are returned unchanged.
    pub fn to_owned(mut self) -> Value<'static> {
        self.to_owned_in_place();
        unsafe { std::mem::transmute::<Value, Value<'static>>(self) }
    }
    fn tag(&self) -> Kind {
        unsafe { std::mem::transmute(*(self as *const _ as *const u32) as u8 & 0b111) }
    }
    fn as_mut<'b>(&'b mut self) -> ValueMut<'b, 'a> {
        unsafe {
            std::mem::transmute(ReprValueMut {
                tag: self.tag() as u8,
                ptr: self,
            })
        }
    }

    /// Returns a reference view for pattern matching on the value's type.
    ///
    /// # Examples
    ///
    /// ```
    /// use jsony_value::{Value, ValueRef, ValueNumber};
    ///
    /// let value: Value = 42i64.into();
    /// match value.as_ref() {
    ///     ValueRef::Number(n) => println!("number: {}", n),
    ///     ValueRef::String(s) => println!("string: {}", s.as_str()),
    ///     ValueRef::Null(_) => println!("null"),
    ///     _ => println!("other"),
    /// }
    /// ```
    pub fn as_ref<'b>(&'b self) -> ValueRef<'b, 'a> {
        unsafe {
            std::mem::transmute(ReprValueRef {
                tag: self.tag() as u8,
                ptr: self,
            })
        }
    }

    unsafe fn as_string_unchecked(&mut self) -> &mut ValueString<'a> {
        unsafe { &mut *(self as *mut _ as *mut ValueString<'a>) }
    }
    unsafe fn as_list_unchecked(&mut self) -> &mut ValueList<'a> {
        unsafe { &mut *(self as *mut _ as *mut ValueList<'a>) }
    }
    unsafe fn as_map_unchecked(&mut self) -> &mut ValueMap<'a> {
        unsafe { &mut *(self as *mut _ as *mut ValueMap<'a>) }
    }

    /// Returns a convenient enum view with primitive types extracted.
    ///
    /// Unlike [`as_ref`], this extracts primitive values directly (e.g., `bool`
    /// instead of `&ValueBoolean`), making it easier to work with simple types.
    ///
    /// [`as_ref`]: Value::as_ref
    pub fn of<'b>(&'b self) -> ValueOf<'b, 'a> {
        match self.as_ref() {
            ValueRef::Null(_) => ValueOf::Null,
            ValueRef::Boolean(boolean) => ValueOf::Boolean(boolean.value != 0),
            ValueRef::Number(num) => match num {
                ValueNumber::F64(n) => ValueOf::F64(*n),
                ValueNumber::U64(n) => ValueOf::U64(*n),
                ValueNumber::I64(n) => ValueOf::I64(*n),
            },
            ValueRef::String(text) => ValueOf::String(text),
            ValueRef::Other(text) => ValueOf::Other(text),
            ValueRef::Map(map) => ValueOf::Map(map),
            ValueRef::List(list) => ValueOf::List(list.as_slice()),
        }
    }

    /// Attempts to interpret the value as `f64`.
    ///
    /// Returns [`Some(f64)`] for numbers and parseable "Other" strings.
    ///
    /// [`Some(f64)`]: Option::Some
    pub fn as_f64(&self) -> Option<f64> {
        match self.as_ref() {
            ValueRef::Number(num) => num.as_f64(),
            ValueRef::Other(text) => text.parse().ok(),
            _ => None,
        }
    }

    /// Attempts to interpret the value as `u64`.
    ///
    /// Returns [`Some(u64)`] for non-negative integer numbers and parseable "Other" strings.
    ///
    /// [`Some(u64)`]: Option::Some
    pub fn as_u64(&self) -> Option<u64> {
        match self.as_ref() {
            ValueRef::Number(num) => num.as_u64(),
            ValueRef::Other(text) => text.parse().ok(),
            _ => None,
        }
    }

    /// Attempts to interpret the value as `i64`.
    ///
    /// Returns [`Some(i64)`] for integer numbers within range and parseable "Other" strings.
    ///
    /// [`Some(i64)`]: Option::Some
    pub fn as_i64(&self) -> Option<i64> {
        match self.as_ref() {
            ValueRef::Number(num) => num.as_i64(),
            ValueRef::Other(text) => text.parse().ok(),
            _ => None,
        }
    }
}

#[repr(C)]
struct ReprValueRef<'a, 'v> {
    tag: u8,
    ptr: &'a Value<'v>,
}

/// Discriminant for the type of a [`Value`].
#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum Kind {
    /// A JSON array.
    List = 1,
    /// A JSON object.
    Map = 2,
    /// A JSON string.
    String = 3,
    /// An unparsed raw string, used for numbers too large for [`ValueNumber`]
    /// or values with ambiguous types (e.g., unquoted strings that could be
    /// numbers).
    Other = 4,
    /// A JSON boolean.
    Boolean = 5,
    /// A JSON number.
    Number = 6,
    /// JSON null.
    Null = 7,
}

/// A convenient view of a [`Value`] with primitive types extracted.
///
/// This is returned by [`Value::of`] and provides direct access to primitive
/// values like `bool`, `f64`, etc., instead of wrapper types.
#[repr(u8)]
pub enum ValueOf<'a, 'b> {
    /// JSON null.
    Null,
    /// A boolean value.
    Boolean(bool),
    /// A floating-point number.
    F64(f64),
    /// An unsigned integer.
    U64(u64),
    /// A signed integer.
    I64(i64),
    /// A string slice.
    String(&'a str),
    /// An unparsed raw string.
    Other(&'a str),
    /// A reference to a JSON object.
    Map(&'a ValueMap<'b>),
    /// A slice of array elements.
    List(&'a [Value<'b>]),
}

/// A mutable reference view of a [`Value`] for pattern matching.
#[repr(u8)]
pub enum ValueMut<'a, 'b> {
    /// JSON null.
    Null(&'a mut ()) = Kind::Null as u8,
    /// A mutable reference to a number.
    Number(&'a mut ValueNumber) = Kind::Number as u8,
    /// A mutable reference to a string.
    String(&'a mut ValueString<'b>) = Kind::String as u8,
    /// A mutable reference to an unparsed raw string.
    Other(&'a mut ValueString<'b>) = Kind::Other as u8,
    /// A mutable reference to a JSON object.
    Map(&'a mut ValueMap<'b>) = Kind::Map as u8,
    /// A mutable reference to a JSON array.
    List(&'a mut ValueList<'b>) = Kind::List as u8,
    /// A mutable reference to a boolean.
    Boolean(&'a mut ValueBoolean) = Kind::Boolean as u8,
}

/// An immutable reference view of a [`Value`] for pattern matching.
///
/// Returned by [`Value::as_ref`].
///
/// # Examples
///
/// ```
/// use jsony_value::{Value, ValueRef};
///
/// let value: Value = "hello".into();
/// match value.as_ref() {
///     ValueRef::String(s) => assert_eq!(s.as_str(), "hello"),
///     _ => panic!("expected string"),
/// }
/// ```
#[repr(u8)]
pub enum ValueRef<'a, 'b> {
    /// JSON null.
    Null(&'a ()) = Kind::Null as u8,
    /// A reference to a number.
    Number(&'a ValueNumber) = Kind::Number as u8,
    /// A reference to a string.
    String(&'a ValueString<'b>) = Kind::String as u8,
    /// A reference to an unparsed raw string.
    Other(&'a ValueString<'b>) = Kind::Other as u8,
    /// A reference to a JSON object.
    Map(&'a ValueMap<'b>) = Kind::Map as u8,
    /// A reference to a JSON array.
    List(&'a ValueList<'b>) = Kind::List as u8,
    /// A reference to a boolean.
    Boolean(&'a ValueBoolean) = Kind::Boolean as u8,
}

#[repr(C)]
struct ReprValueMut<'a, 'v> {
    tag: u8,
    ptr: &'a mut Value<'v>,
}

/// A string that can be either borrowed or owned.
///
/// When parsing JSON, strings without escape sequences are borrowed from the
/// input buffer to avoid allocation. Strings with escape sequences are allocated
/// as owned. Call [`to_owned`] to ensure the string has `'static` lifetime.
///
/// `ValueString` implements [`Deref<Target = str>`], so it can be used anywhere
/// a `&str` is expected.
///
/// # Examples
///
/// ```
/// use jsony_value::ValueString;
///
/// // Borrowed from a string slice
/// let borrowed = ValueString::from_borrowed("hello");
/// assert_eq!(borrowed.as_str(), "hello");
///
/// // Owned from a boxed string
/// let owned = ValueString::from_owned("world".into());
/// assert_eq!(owned.as_str(), "world");
/// ```
///
/// [`to_owned`]: ValueString::to_owned
/// [`Deref<Target = str>`]: std::ops::Deref
#[repr(C)]
pub struct ValueString<'a> {
    tag: CapacityTag,
    len: u32,
    ptr: NonNull<u8>,
    marker: PhantomData<&'a str>,
}

impl<'a, 'b> PartialEq<ValueString<'b>> for ValueString<'a> {
    fn eq(&self, other: &ValueString<'b>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl std::fmt::Debug for ValueString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl std::fmt::Display for ValueString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl ValueString<'_> {
    /// Returns the string as a `&str`.
    pub fn as_str(&self) -> &str {
        &*self
    }
    pub(crate) fn to_owned_in_place(&mut self) {
        if self.tag != CapacityTag::BORROWED_STRING {
            return;
        }
        self.ptr = strndup(self.ptr, self.len as usize);
        self.tag = CapacityTag::OWNED_STRING;
    }

    /// Converts borrowed data to owned, returning a `'static` lifetime string.
    ///
    /// Borrowed strings are copied to the heap. Already-owned strings are
    /// returned unchanged.
    pub fn to_owned(mut self) -> ValueString<'static> {
        self.to_owned_in_place();
        unsafe { std::mem::transmute::<ValueString, ValueString<'static>>(self) }
    }
}

impl<'a> std::ops::Deref for ValueString<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.ptr.as_ptr(),
                self.len as usize,
            ))
        }
    }
}

impl<'a> Drop for ValueString<'a> {
    fn drop(&mut self) {
        if self.tag.is_copy() || self.len == 0 {
            return;
        }
        unsafe {
            std::alloc::dealloc(
                self.ptr.as_ptr(),
                std::alloc::Layout::from_size_align_unchecked(self.len as usize, 1),
            );
        }
    }
}

impl<'a> ValueString<'a> {
    /// Creates a borrowed string from a string slice.
    ///
    /// The resulting `ValueString` references the original string without
    /// allocating. The string must outlive the `ValueString`.
    ///
    /// # Panics
    ///
    /// Panics if the string length exceeds `u32::MAX`.
    pub fn from_borrowed(text: &'a str) -> ValueString<'a> {
        let len = text.len().try_into().expect("JsonStr bounds exceeed");
        ValueString {
            tag: CapacityTag::BORROWED_STRING,
            len,
            ptr: unsafe { NonNull::new_unchecked(text.as_ptr() as *mut u8) },
            marker: PhantomData,
        }
    }

    /// Creates an owned string from a boxed string.
    ///
    /// Takes ownership of the boxed string, returning a `'static` lifetime string.
    ///
    /// # Panics
    ///
    /// Panics if the string length exceeds `u32::MAX`.
    pub fn from_owned(text: Box<str>) -> ValueString<'static> {
        let len = text.len().try_into().expect("JsonStr bounds exceeed");
        ValueString {
            tag: CapacityTag::OWNED_STRING,
            len,
            ptr: unsafe { NonNull::new_unchecked(Box::into_raw(text) as *mut u8) },
            marker: PhantomData,
        }
    }

    /// Creates a borrowed "Other" string from a string slice.
    ///
    /// "Other" strings represent unparsed raw values, such as numbers too large
    /// to fit in [`ValueNumber`].
    ///
    /// # Panics
    ///
    /// Panics if the string length exceeds `u32::MAX`.
    pub fn other_borrowed(text: &'a str) -> ValueString<'a> {
        let len = text.len().try_into().expect("JsonStr bounds exceeed");
        ValueString {
            tag: CapacityTag::BORROWED_OTHER,
            len,
            ptr: unsafe { NonNull::new_unchecked(text.as_ptr() as *mut u8) },
            marker: PhantomData,
        }
    }

    /// Creates an owned "Other" string from a boxed string.
    ///
    /// "Other" strings represent unparsed raw values, such as numbers too large
    /// to fit in [`ValueNumber`].
    ///
    /// # Panics
    ///
    /// Panics if the string length exceeds `u32::MAX`.
    pub fn other_owned(text: Box<str>) -> ValueString<'static> {
        let len = text.len().try_into().expect("JsonStr bounds exceeed");
        ValueString {
            tag: CapacityTag::OWNED_OTHER,
            len,
            ptr: unsafe { NonNull::new_unchecked(Box::into_raw(text) as *mut u8) },
            marker: PhantomData,
        }
    }

    /// Creates a string from a static string literal.
    ///
    /// The string is neither borrowed nor owned; it references static memory
    /// and never needs to be freed.
    ///
    /// # Panics
    ///
    /// Panics if the string length exceeds `u32::MAX`.
    pub fn from_static(text: &'static str) -> ValueString<'static> {
        let len = text.len().try_into().expect("JsonStr bounds exceeded");
        ValueString {
            tag: CapacityTag::STATIC_STRING,
            len,
            ptr: unsafe { NonNull::new_unchecked(text.as_ptr() as *mut u8) },
            marker: PhantomData,
        }
    }
}

fn strndup(ptr: NonNull<u8>, size: usize) -> NonNull<u8> {
    if size == 0 {
        return NonNull::dangling();
    }
    let layout = unsafe { std::alloc::Layout::from_size_align_unchecked(size, 1) };
    unsafe {
        let new_ptr = std::alloc::alloc(layout);
        if new_ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        // Copy the string content
        std::ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr, size);
        NonNull::new_unchecked(new_ptr)
    }
}

impl<'a> Clone for ValueString<'a> {
    fn clone(&self) -> ValueString<'a> {
        if self.tag.is_copy() {
            // For borrowed strings, we can simply copy the pointer and other fields
            ValueString {
                tag: self.tag,
                len: self.len,
                ptr: self.ptr,
                marker: PhantomData,
            }
        } else {
            ValueString {
                tag: self.tag,
                len: self.len,
                ptr: strndup(self.ptr, self.len as usize),
                marker: PhantomData,
            }
        }
    }
}

/// The error type for `try_reserve` methods.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TryReserveError {
    /// Error due to the computed capacity exceeding the collection's maximum
    /// (usually `isize::MAX` bytes).
    CapacityOverflow,

    /// The memory allocator returned an error
    AllocError {
        /// The layout of the allocation request that failed.
        layout: std::alloc::Layout,
    },
}

impl From<i64> for Value<'_> {
    fn from(value: i64) -> Self {
        Value::from(ValueNumber::I64(value))
    }
}

impl From<u64> for Value<'_> {
    fn from(value: u64) -> Self {
        Value::from(ValueNumber::U64(value))
    }
}

macro_rules! from_num_as_i64 {
    ($($ty:ty),*) => {
    $(
        impl From<$ty> for Value<'_> {
            fn from(value: $ty) -> Self {
                Value::from(ValueNumber::I64(value as i64))
            }
        }
    )*
  };
}

from_num_as_i64! {u8,i8,i16,u16,u32,i32}

impl From<f64> for Value<'_> {
    fn from(value: f64) -> Self {
        Value::from(ValueNumber::F64(value))
    }
}

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Value::from(ValueBoolean {
            tag: CapacityTag::BOOLEAN,
            meta: 0,
            value: value as u64,
        })
    }
}

impl<'a, T: Into<Value<'a>>> From<Option<T>> for Value<'a> {
    fn from(value: Option<T>) -> Self {
        if let Some(value) = value {
            value.into()
        } else {
            Value::NULL
        }
    }
}

impl From<f32> for Value<'_> {
    fn from(value: f32) -> Self {
        Value::from(ValueNumber::F64(value as f64))
    }
}

impl<'a, V: Into<Value<'a>>> FromIterator<V> for Value<'a> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Value::from(ValueList::from_iter(iter))
    }
}

impl<'a, K: Into<ValueString<'a>>, V: Into<Value<'a>>> FromIterator<(K, V)> for Value<'a> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Value::from(ValueMap::from_iter(iter))
    }
}

impl<'a> std::ops::Index<&str> for Value<'a> {
    type Output = Value<'a>;

    fn index(&self, index: &str) -> &Self::Output {
        if let ValueRef::Map(map) = self.as_ref() {
            &map[index]
        } else {
            const { &Value::NULL }
        }
    }
}

unsafe impl<'a> FromJson<'a> for ValueMap<'a> {
    fn decode_json(
        parser: &mut jsony::json::Parser<'a>,
    ) -> Result<Self, &'static jsony::json::DecodeError> {
        let mut map = ValueMapBuilder::new();
        parser.decode_object_sequence::<ValueString<'a>, Value<'a>>(|key, value| {
            map.insert(key, value);
            Ok(())
        })?;
        Ok(map.build())
    }
}
unsafe impl<'a> FromJson<'a> for ValueList<'a> {
    fn decode_json(
        parser: &mut jsony::json::Parser<'a>,
    ) -> Result<Self, &'static jsony::json::DecodeError> {
        let mut array = ValueList::new();
        parser.decode_array_sequence::<Value<'a>>(|list| {
            array.push(list);
            Ok(())
        })?;

        Ok(array)
    }
}
unsafe impl<'a> FromJson<'a> for ValueNumber {
    fn decode_json(
        parser: &mut jsony::json::Parser<'a>,
    ) -> Result<Self, &'static jsony::json::DecodeError> {
        //todo optimize
        let text = parser.at.consume_numeric_literal()?;
        if text.starts_with("-") {
            if let Ok(value) = text.parse::<i64>() {
                return Ok(ValueNumber::I64(value));
            }
        } else {
            if let Ok(value) = text.parse::<u64>() {
                return Ok(ValueNumber::U64(value));
            }
        }
        match text.parse::<f64>() {
            Ok(num) => Ok(ValueNumber::F64(num)),
            Err(_) => Err(&DecodeError {
                message: "Invalid number",
            }),
        }
    }
}

unsafe impl<'a> FromJson<'a> for Value<'a> {
    fn decode_json(
        parser: &mut jsony::json::Parser<'a>,
    ) -> Result<Self, &'static jsony::json::DecodeError> {
        match parser.peek()? {
            Peek::Array => Ok(Value::from(ValueList::decode_json(parser)?)),
            Peek::Object => Ok(Value::from(ValueMap::decode_json(parser)?)),
            Peek::String => Ok(Value::from(ValueString::decode_json(parser)?)),
            Peek::False => {
                parser.at.discard_seen_false()?;
                Ok(Value::from(false))
            }
            Peek::True => {
                parser.at.discard_seen_true()?;
                Ok(Value::from(true))
            }
            Peek::Null => {
                parser.at.discard_seen_null()?;
                Ok(Value::NULL)
            }
            _ => Ok(Value::from(ValueNumber::decode_json(parser)?)),
        }
    }
}

unsafe impl<'a> FromJson<'a> for ValueString<'a> {
    fn decode_json(
        parser: &mut jsony::json::Parser<'a>,
    ) -> Result<Self, &'static jsony::json::DecodeError> {
        let result = parser.at.take_string(&mut parser.scratch)?;
        match parser.at.ctx.try_extend_lifetime(result) {
            Some(value) => Ok(ValueString::from_borrowed(value)),
            None => Ok(ValueString::from_owned(result.into())),
        }
    }
}
