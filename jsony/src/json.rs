//! # Lower level JSON utilities and definitions
//!
//! ## Example
//! ```rust
//! use jsony::json;
//! let mut buffer = jsony::TextWriter::new();
//! {
//!     let mut object = json::ObjectWriter::new(&mut buffer);
//!     object.key("ten").value(&true);
//!
//!     let mut array = object.key("list").array();
//!     array.push(&0u32);
//!     array.extend(&[true, false]);
//! }
//! assert_eq!(
//!     buffer.as_str(),
//!     r#"{"ten":true,"list":[0,true,false]}"#
//! );
//!
//! ```
use crate::{
    parser::InnerParser, text::FromText, text_writer::TextWriter, FromJson, RawJson, ToJson,
};
use std::{
    alloc::Layout,
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::{BuildHasher, Hash},
    marker::PhantomData,
    path::{Path, PathBuf},
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
};

pub use crate::parser::{Parser, Peek};

pub struct ParserWithBorrowedKey<'a, 'b> {
    key: *const str,
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> ParserWithBorrowedKey<'a, 'b> {
    /// # Safety
    /// key must have a lifetime greater or equal to 'b + 'a
    pub unsafe fn new(key: *const str, parser: &'b mut Parser<'a>) -> Self {
        Self { key, parser }
    }
    pub fn key(&self) -> &str {
        unsafe { &*self.key }
    }
    pub(crate) fn key_with_inner_parser<'c>(&'c mut self) -> (&'c str, &'c mut InnerParser<'a>) {
        (unsafe { &*self.key }, &mut self.parser.at)
    }
    pub(crate) fn parser<'c>(&'c self) -> &'c Parser<'a> {
        self.parser
    }
    pub(crate) fn reborrow<'c>(&'c mut self) -> ParserWithBorrowedKey<'a, 'c> {
        ParserWithBorrowedKey {
            key: self.key,
            parser: self.parser,
        }
    }
    pub fn into_parser(self) -> &'b mut Parser<'a> {
        self.parser
    }
}

pub trait FieldVisitor<'a> {
    fn visit(
        &mut self,
        borrowed: ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError>;
    fn complete(&mut self) -> Result<(), &'static DecodeError>;
    /// # Safety
    /// must only call once.
    unsafe fn destroy(&mut self);
}

#[doc(hidden)]
pub struct FuncFieldVisitor<'a> {
    visit: unsafe fn(
        ptr: NonNull<()>,
        borrowed: ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError>,
    drop: unsafe fn(ptr: NonNull<()>),
    ptr: NonNull<()>,
}

impl<'a> FieldVisitor<'a> for FuncFieldVisitor<'a> {
    fn visit(
        &mut self,
        borrowed: ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError> {
        unsafe { (self.visit)(self.ptr, borrowed) }
    }
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        Ok(())
    }
    unsafe fn destroy(&mut self) {
        unsafe { (self.drop)(self.ptr) }
    }
}

unsafe impl<'a, K, T> FromJsonFieldVisitor<'a> for BTreeMap<K, T>
where
    K: FromText<'a> + Ord + Eq,
    T: FromJson<'a>,
{
    type Visitor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Visitor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, mut borrowed| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (key, at) = borrowed.key_with_inner_parser();
                let key = K::from_text(&mut at.ctx, key)?;
                let value = T::decode_json(borrowed.into_parser())?;
                map.insert(key, value);
                Ok(())
            },
            drop: |ptr| {
                let map = ptr.cast::<Self>();
                std::ptr::drop_in_place(map.as_ptr());
            },
            ptr,
        }
    }
}

unsafe impl<'a, K, T> FromJsonFieldVisitor<'a> for HashMap<K, T>
where
    K: FromText<'a> + Hash + Eq,
    T: FromJson<'a>,
{
    type Visitor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Visitor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, mut borrowed| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (field, at) = borrowed.key_with_inner_parser();
                let key = K::from_text(&mut at.ctx, field)?;
                let value = T::decode_json(borrowed.into_parser())?;
                map.insert(key, value);
                Ok(())
            },
            drop: |ptr| {
                let map = ptr.cast::<Self>();
                std::ptr::drop_in_place(map.as_ptr());
            },
            ptr,
        }
    }
}

unsafe impl<'a, K, T> FromJsonFieldVisitor<'a> for Vec<(K, T)>
where
    K: FromText<'a>,
    T: FromJson<'a>,
{
    type Visitor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Visitor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, mut borrowed| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (field, at) = borrowed.key_with_inner_parser();
                let key = K::from_text(&mut at.ctx, field)?;
                let value = T::decode_json(borrowed.into_parser())?;
                map.push((key, value));
                Ok(())
            },
            drop: |ptr| {
                let map = ptr.cast::<Self>();
                std::ptr::drop_in_place(map.as_ptr());
            },
            ptr,
        }
    }
}

/// # Safety
/// If the Self::Visitor complete method returns `Ok(())` then `Self` must be have
/// initialized within the given ptr.
pub unsafe trait FromJsonFieldVisitor<'a> {
    type Visitor: FieldVisitor<'a>;
    /// # Safety
    /// dest, must be a pointer to Self although possilibly uninitialized
    /// and be valid for writes.
    unsafe fn new_field_visitor(ptr: NonNull<()>, parser: &Parser<'a>) -> Self::Visitor;
}

#[derive(Debug, PartialEq, Eq)]
pub struct DecodeError {
    pub message: &'static str,
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message)
    }
}

static INVALID_NUMERIC_LITERAL: DecodeError = DecodeError {
    message: "Invalid numeric literal",
};

static STRING_CONTAINS_INVALID_ESCAPE_LITERALS: DecodeError = DecodeError {
    message: "String contains invalid escape literals",
};

macro_rules! direct_impl_integer_decode {
    ($($ty:ty),*) => {
        $(
            unsafe impl<'a> FromJson<'a> for $ty {
                unsafe fn emplace_from_json(
                    dest: NonNull<()>,
                    parser: &mut Parser<'a>,
                ) -> Result<(), &'static DecodeError> {
                    match parser.at.consume_numeric_literal() {
                        Ok(value) => {
                            if let Ok(numeric) = value.parse::<$ty>() {
                                dest.cast::<$ty>().write(numeric);
                                return Ok(());
                            }
                            Err(&INVALID_NUMERIC_LITERAL)
                        }
                        Err(err) => return Err(err),
                    }
                }
            }
        )*
    };
}

direct_impl_integer_decode! {
    i128, u128, u64, i64, f32, f64, u32, i32, u16, i16, u8, i8, isize, usize
}

unsafe impl<'a> FromJson<'a> for Box<std::path::Path> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.at.take_string(&mut parser.scratch) {
            Ok(string) => {
                dest.cast::<Box<std::path::Path>>()
                    .write(std::path::Path::new(string).into());
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}
unsafe impl<'a> FromJson<'a> for PathBuf {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.at.take_string(&mut parser.scratch) {
            Ok(string) => {
                dest.cast::<PathBuf>().write(PathBuf::from(string));
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}
unsafe impl<'a> FromJson<'a> for &'a str {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.take_borrowed_string() {
            Ok(raw_str) => {
                dest.cast::<&str>().write(raw_str);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'a> FromJson<'a> for String {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.take_string() {
            Ok(raw_str) => {
                dest.cast::<String>().write(String::from(raw_str));
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'a, T: FromJson<'a>> FromJson<'a> for Box<[T]> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match <Vec<T> as FromJson<'a>>::decode_json(parser) {
            Ok(value) => unsafe { dest.cast::<Box<[T]>>().write(value.into_boxed_slice()) },
            Err(err) => return Err(err),
        }
        Ok(())
    }
}

unsafe impl<'a, T: Sized + FromJson<'a>> FromJson<'a> for Box<T> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        let mut raw = Box::<T>::new_uninit();
        <T as FromJson<'a>>::emplace_from_json(
            NonNull::new_unchecked(raw.as_mut_ptr()).cast(),
            parser,
        )?;
        dest.cast::<Box<T>>().write(raw.assume_init());
        Ok(())
    }
}

unsafe impl<'a> FromJson<'a> for Box<str> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.take_string() {
            Ok(raw_str) => {
                dest.cast::<Box<str>>().write(raw_str.into());
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'a> FromJson<'a> for Cow<'a, str> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.take_cow_string() {
            Ok(raw_str) => {
                dest.cast::<Cow<str>>().write(raw_str);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
}

impl<'a, T: ToJson + ToOwned + ?Sized> ToJson for Cow<'a, T> {
    type Kind = T::Kind;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> Self::Kind {
        self.as_ref().encode_json__jsony(output)
    }
}

#[cfg(target_pointer_width = "32")]
const MAX_SIZE: usize = (isize::MAX / 2) as usize;
#[cfg(target_pointer_width = "64")]
const MAX_SIZE: usize = 1usize << 42;

/// Decodes a size sized array returning the raw parts in (pointer, len, capacity).
/// Safety `emplace_from_json` must be consume a pointer to ZST.
#[cold]
unsafe fn zero_sized_type_erased_decode_vec<'de>(
    emplace_from_json: unsafe fn(NonNull<()>, &mut Parser<'de>) -> Result<(), &'static DecodeError>,
    parser: &mut Parser<'de>,
) -> ((*mut u8, usize, usize), Result<(), &'static DecodeError>) {
    let mut len = 0;
    let value = 'failure: {
        let mut value = match parser.at.enter_array() {
            Ok(it) => it,
            Err(err) => break 'failure Err(err),
        };
        while value.is_some() {
            unsafe {
                if let Err(err) = emplace_from_json(NonNull::dangling(), parser) {
                    break 'failure Err(err);
                }
                len += 1;
            }
            value = match parser.at.array_step() {
                Ok(it) => it,
                Err(err) => break 'failure Err(err),
            };
        }
        Ok(())
    };
    ((std::ptr::dangling_mut::<u8>(), len, usize::MAX), value)
}

/// Decodes an array returning the raw parts in (pointer, len, capacity).
/// The Layout must correspond the type `emplace_from_json` is expected.
unsafe fn type_erased_decode_vec<'de>(
    layout: Layout,
    emplace_from_json: unsafe fn(NonNull<()>, &mut Parser<'de>) -> Result<(), &'static DecodeError>,
    parser: &mut Parser<'de>,
) -> ((*mut u8, usize, usize), Result<(), &'static DecodeError>) {
    if layout.size() == 0 {
        return zero_sized_type_erased_decode_vec(emplace_from_json, parser);
    }
    let max_cap = MAX_SIZE / layout.size();
    let mut ptr: *mut u8 = std::ptr::without_provenance_mut(layout.align());
    let mut capacity = 0;
    let mut len = 0;
    let value = 'failure: {
        let mut value = match parser.at.enter_array() {
            Ok(it) => it,
            Err(err) => break 'failure Err(err),
        };
        while value.is_some() {
            unsafe {
                if len == capacity {
                    if capacity == 0 {
                        capacity = if layout.size() > 4096 { 1 } else { 4 };
                        ptr = std::alloc::alloc(Layout::from_size_align_unchecked(
                            layout.size() * capacity,
                            layout.align(),
                        ));
                    } else {
                        if capacity >= max_cap {
                            break 'failure Err(&DecodeError {
                                message: "Array too large",
                            });
                        }
                        let new_capacity = capacity * 2;

                        ptr = std::alloc::realloc(
                            ptr,
                            Layout::from_size_align_unchecked(
                                layout.size() * capacity,
                                layout.align(),
                            ),
                            new_capacity * layout.size(),
                        );
                        capacity = new_capacity;
                    }
                    if ptr.is_null() {
                        std::alloc::handle_alloc_error(layout);
                    }
                }
                if let Err(err) = emplace_from_json(
                    NonNull::new_unchecked(ptr).add(len * layout.size()).cast(),
                    parser,
                ) {
                    break 'failure Err(err);
                }
                len += 1;
            }
            value = match parser.at.array_step() {
                Ok(it) => it,
                Err(err) => break 'failure Err(err),
            };
        }
        Ok(())
    };

    ((ptr, len, capacity), value)
}

unsafe impl<'de, T: FromJson<'de>> FromJson<'de> for Vec<T> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let ((ptr, len, cap), result) = type_erased_decode_vec(
            Layout::new::<T>(),
            <T as FromJson<'de>>::emplace_from_json,
            parser,
        );
        let vec = Vec::from_raw_parts(ptr as *mut T, len, cap);
        if result.is_ok() {
            dest.cast::<Vec<T>>().write(vec);
            Ok(())
        } else {
            result
        }
    }
}

unsafe impl<'de, K: FromJson<'de> + Ord, V: FromJson<'de>> FromJson<'de> for BTreeMap<K, V> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<BTreeMap<K, V>>().write(BTreeMap::new());
        let map = &mut *dest.cast::<BTreeMap<K, V>>().as_ptr();
        let result = parser.decode_object_sequence(|k, v| {
            map.insert(k, v);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<BTreeMap<K, V>>().as_ptr());
            }
        }
        result
    }
}
unsafe impl<'de, K: FromJson<'de> + Ord> FromJson<'de> for BTreeSet<K> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<BTreeSet<K>>().write(BTreeSet::new());
        let map = &mut *dest.cast::<BTreeSet<K>>().as_ptr();
        let result = parser.decode_array_sequence(|k| {
            map.insert(k);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<BTreeSet<K>>().as_ptr());
            }
        }
        result
    }
}
unsafe impl<'de, K: FromJson<'de> + Eq + Hash, S: Default + BuildHasher + 'de> FromJson<'de>
    for HashSet<K, S>
{
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<HashSet<K, S>>().write(HashSet::default());
        let map = &mut *dest.cast::<HashSet<K, S>>().as_ptr();
        let result = parser.decode_array_sequence(|k| {
            map.insert(k);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<HashSet<K, S>>().as_ptr());
            }
        }
        result
    }
}

unsafe impl<'de, K: FromJson<'de> + Eq + Hash, V: FromJson<'de>, S: Default + BuildHasher + 'de>
    FromJson<'de> for HashMap<K, V, S>
{
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<HashMap<K, V, S>>().write(HashMap::default());
        let map = &mut *dest.cast::<HashMap<K, V, S>>().as_ptr();
        let result = parser.decode_object_sequence(|k, v| {
            map.insert(k, v);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<HashMap<K, V, S>>().as_ptr());
            }
        }
        result
    }
}

unsafe impl<'a, T: FromJson<'a>> FromJson<'a> for Option<T> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.at.peek() {
            Ok(Peek::Null) => {
                if let Err(err) = parser.at.discard_seen_null() {
                    return Err(err);
                }
                dest.cast::<Option<T>>().write(None);
                Ok(())
            }
            Ok(_) => {
                // not sure if this is legal or not or not??
                // Currently enums can't use padding to store discrements
                // Thus, the discriminant must store in niche
                if const { size_of::<Option<T>>() == size_of::<T>() } {
                    T::emplace_from_json(dest, parser)
                } else {
                    let mut value = std::mem::MaybeUninit::<T>::uninit();
                    if let Err(err) = T::emplace_from_json(
                        NonNull::new_unchecked(value.as_mut_ptr()).cast(),
                        parser,
                    ) {
                        return Err(err);
                    };
                    dest.cast::<Option<T>>().write(Some(value.assume_init()));
                    Ok(())
                }
            }
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'a> FromJson<'a> for bool {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.at.peek() {
            Ok(Peek::True) => {
                parser.at.discard_seen_true()?;
                dest.cast::<bool>().write(true);
                Ok(())
            }
            Ok(Peek::False) => {
                parser.at.discard_seen_false()?;
                dest.cast::<bool>().write(false);
                Ok(())
            }
            _ => Err(&DecodeError {
                message: "Invalid boolean literal",
            }),
        }
    }
}

unsafe impl<'a> FromJson<'a> for char {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.take_string() {
            Ok(raw_str) => {
                let mut chars = raw_str.chars();
                let ch = chars.next();
                if chars.next().is_some() {
                    return Err(&DecodeError {
                        message: "Expected a single char",
                    });
                }
                if let Some(ch) = ch {
                    dest.cast::<char>().write(ch);
                    return Ok(());
                }
                Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS)
            }
            Err(err) => Err(err),
        }
    }
}
// assumes n > 0
unsafe fn decode_into_array<'de>(
    dest: NonNull<()>,
    parser: &mut Parser<'de>,
    size: usize,
    n: usize,
    decode: unsafe fn(NonNull<()>, &mut Parser<'de>) -> Result<(), &'static DecodeError>,
    drop_func: Option<impl Fn(NonNull<()>)>,
) -> Result<(), &'static DecodeError> {
    parser.at.enter_array()?;
    let mut current = dest;
    let err = 'error: {
        for i in 0..n {
            if let Err(err) = decode(current, parser) {
                break 'error Err(err);
            }
            current = current.byte_add(size);
            match parser.at.array_step() {
                Ok(None) => {
                    if i + 1 == n {
                        return Ok(());
                    } else {
                        break 'error Err(&DecodeError {
                            message: "Array length mismatch",
                        });
                    }
                }
                Ok(Some(_)) => continue,
                Err(err) => break 'error Err(err),
            }
        }
        Err(&DecodeError {
            message: "Array to long",
        })
    };
    if let Some(drop_func) = drop_func {
        let mut ptr = dest;
        while ptr < current {
            drop_func(ptr);
            ptr = ptr.byte_add(size);
        }
    }
    err
}

static ARRAY_LENGTH_MISMATCH: DecodeError = DecodeError {
    message: "Array length mismatch",
};

unsafe impl FromJson<'_> for Box<RawJson> {
    fn decode_json(parser: &mut Parser<'_>) -> Result<Self, &'static DecodeError> {
        let start = parser.at.index;
        parser.at.skip_value()?;
        let raw =
            unsafe { std::str::from_utf8_unchecked(&parser.at.ctx.input[start..parser.at.index]) };
        Ok(RawJson::new_boxed_unchecked(raw.into()))
    }
}

unsafe impl<'de> FromJson<'de> for &'de RawJson {
    fn decode_json(parser: &mut Parser<'de>) -> Result<Self, &'static DecodeError> {
        let start = parser.at.index;
        parser.at.skip_value()?;
        let raw =
            unsafe { std::str::from_utf8_unchecked(&parser.at.ctx.input[start..parser.at.index]) };
        Ok(RawJson::new_unchecked(raw))
    }
}

impl ToJson for std::path::Path {
    type Kind = AlwaysString;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> Self::Kind {
        // TODO Should probably make this op in due to lossy nature
        self.to_string_lossy().encode_json__jsony(output)
    }
}

impl ToJson for PathBuf {
    type Kind = AlwaysString;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> Self::Kind {
        // TODO Should probably make this op in due to lossy nature
        let path: &Path = self;
        path.encode_json__jsony(output)
    }
}
impl ToJson for RawJson {
    type Kind = AnyValue;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> Self::Kind {
        output.push_str(&self.raw);
        AnyValue
    }
}

unsafe impl<'de> FromJson<'de> for () {
    unsafe fn emplace_from_json(
        _dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        match parser.at.enter_array() {
            Ok(None) => Ok(()),
            Ok(_) => Err(&ARRAY_LENGTH_MISMATCH),
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'de, T0: FromJson<'de>> FromJson<'de> for (T0,) {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let value = parser.at.enter_array()?;
        if value.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        <T0 as FromJson<'de>>::emplace_from_json(
            dest.byte_add(std::mem::offset_of!(Self, 0)),
            parser,
        )?;
        let error = match parser.at.array_step() {
            Ok(None) => return Ok(()),
            Ok(Some(_)) => &ARRAY_LENGTH_MISMATCH,
            Err(err) => err,
        };
        std::ptr::drop_in_place(dest.cast::<T0>().as_ptr());
        Err(error)
    }
}

// todo should add more tuples
unsafe impl<'de, T0: FromJson<'de>, T1: FromJson<'de>> FromJson<'de> for (T0, T1) {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        if parser.at.enter_array()?.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        let error: &'static DecodeError;
        't0: {
            if let Err(err) = <T0 as FromJson<'de>>::emplace_from_json(
                dest.byte_add(std::mem::offset_of!(Self, 0)),
                parser,
            ) {
                error = err;
                break 't0;
            }
            't1: {
                if let Err(err) = parser.at.array_step_expecting_more() {
                    error = err;
                    break 't1;
                }
                if let Err(err) = <T1 as FromJson<'de>>::emplace_from_json(
                    dest.byte_add(std::mem::offset_of!(Self, 1)),
                    parser,
                ) {
                    error = err;
                    break 't1;
                }
                match parser.at.array_step() {
                    Ok(None) => return Ok(()),
                    Ok(Some(_)) => error = &ARRAY_LENGTH_MISMATCH,
                    Err(err) => error = err,
                }
                std::ptr::drop_in_place(dest.cast::<T1>().as_ptr());
            }
            std::ptr::drop_in_place(dest.cast::<T0>().as_ptr());
        }
        Err(error)
    }
}

unsafe impl<'de, T0: FromJson<'de>, T1: FromJson<'de>, T2: FromJson<'de>> FromJson<'de>
    for (T0, T1, T2)
{
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        if parser.at.enter_array()?.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        let error: &'static DecodeError;
        't0: {
            if let Err(err) = <T0 as FromJson<'de>>::emplace_from_json(
                dest.byte_add(std::mem::offset_of!(Self, 0)),
                parser,
            ) {
                error = err;
                break 't0;
            }
            't1: {
                if let Err(err) = parser.at.array_step_expecting_more() {
                    error = err;
                    break 't1;
                }
                if let Err(err) = <T1 as FromJson<'de>>::emplace_from_json(
                    dest.byte_add(std::mem::offset_of!(Self, 1)),
                    parser,
                ) {
                    error = err;
                    break 't1;
                }
                't2: {
                    if let Err(err) = parser.at.array_step_expecting_more() {
                        error = err;
                        break 't2;
                    }
                    if let Err(err) = <T2 as FromJson<'de>>::emplace_from_json(
                        dest.byte_add(std::mem::offset_of!(Self, 2)),
                        parser,
                    ) {
                        error = err;
                        break 't2;
                    }
                    match parser.at.array_step() {
                        Ok(None) => return Ok(()),
                        Ok(Some(_)) => error = &ARRAY_LENGTH_MISMATCH,
                        Err(err) => error = err,
                    }
                    std::ptr::drop_in_place(dest.cast::<T2>().as_ptr());
                }
                std::ptr::drop_in_place(dest.cast::<T1>().as_ptr());
            }
            std::ptr::drop_in_place(dest.cast::<T0>().as_ptr());
        }
        Err(error)
    }
}
#[allow(clippy::type_complexity)]
#[inline(never)]
unsafe fn dyn_tuple_decode<'a>(
    dest: NonNull<()>,
    parser: &mut Parser<'a>,
    fields: &[(
        usize,
        unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
    )],
    drops: &[unsafe fn(NonNull<()>)],
) -> Result<(), &'static DecodeError> {
    if parser.at.enter_array()?.is_none() {
        if fields.is_empty() {
            return Ok(());
        } else {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
    }
    let mut fields = fields.iter();
    let mut complete = 0;
    let error = 'with_error: {
        if let Some((offset, decode_fn)) = fields.next() {
            decode_fn(dest.byte_add(*offset), parser)?;
            complete += 1;
            for (offset, decode_fn) in fields {
                if let Err(err) = parser.at.array_step() {
                    break 'with_error err;
                }
                if let Err(err) = decode_fn(dest.byte_add(*offset), parser) {
                    break 'with_error err;
                }
                complete += 1;
            }
        }
        match parser.at.array_step() {
            Ok(None) => return Ok(()),
            Ok(Some(_)) => &ARRAY_LENGTH_MISMATCH,
            Err(err) => err,
        }
    };
    for drop_fn in drops.iter().take(complete) {
        drop_fn(dest);
    }
    Err(error)
}

macro_rules! tuple_impls {
    ($({$($field:literal : $tn: tt),*}),*) => {
        $(
        unsafe impl<'de, $($tn: FromJson<'de>),*>
            FromJson<'de> for ($($tn),*)
        {
            unsafe fn emplace_from_json(
                dest: NonNull<()>,
                parser: &mut Parser<'de>,
            ) -> Result<(), &'static DecodeError> {
                dyn_tuple_decode(
                    dest,
                    parser,
                    &[
                        $((
                            std::mem::offset_of!(Self, $field),
                            $tn::emplace_from_json
                        )),*
                    ],
                    &[$(|ptr| std::ptr::drop_in_place(ptr.cast::<$tn>().as_ptr())),*],
                )
            }
        }
        )*
    };
}

tuple_impls! {
    {0: T0, 1: T1, 2: T2, 3: T3},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8}
}

unsafe impl<'de, T: FromJson<'de>, const N: usize> FromJson<'de> for [T; N] {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        if N == 0 {
            return match parser.at.enter_array() {
                Ok(None) => Ok(()),
                Err(err) => Err(err),
                _ => Err(&DecodeError {
                    message: "Array length mismatch",
                }),
            };
        }
        decode_into_array(
            dest,
            parser,
            size_of::<T>(),
            N,
            T::emplace_from_json,
            if std::mem::needs_drop::<T>() {
                Some(|ptr: NonNull<()>| std::ptr::drop_in_place(ptr.cast::<T>().as_ptr()))
            } else {
                None
            },
        )
    }
}

/// [ToJson::Kind] Marker type indicating an number is always generated
pub struct AlwaysNumber;
/// [ToJson::Kind] Marker type indicating an array is always generated
pub struct AlwaysArray;
/// [ToJson::Kind] Marker type indicating an object is always generated
pub struct AlwaysObject;
/// [ToJson::Kind] Marker type indicating a string is always generated
pub struct AlwaysString;
/// [ToJson::Kind] Marker type indicating any type may be generated
pub struct AnyValue;

pub trait JsonKeyKind: crate::__private::Sealed {
    fn key_prefix(output: &mut TextWriter);
    fn key_suffix(output: &mut TextWriter);
}

impl JsonKeyKind for AlwaysString {
    fn key_prefix(_: &mut TextWriter) {}
    fn key_suffix(output: &mut TextWriter) {
        output.push_colon();
    }
}

impl JsonKeyKind for AlwaysNumber {
    fn key_prefix(output: &mut TextWriter) {
        unsafe {
            output.push_unchecked_ascii(b'"');
        }
    }
    fn key_suffix(output: &mut TextWriter) {
        output.push_str("\":");
    }
}

pub trait JsonValueKind: crate::__private::Sealed {}
impl crate::__private::Sealed for AlwaysNumber {}
impl JsonValueKind for AlwaysNumber {}
impl crate::__private::Sealed for AlwaysArray {}
impl JsonValueKind for AlwaysArray {}
impl crate::__private::Sealed for AnyValue {}
impl JsonValueKind for AnyValue {}
impl crate::__private::Sealed for AlwaysString {}
impl JsonValueKind for AlwaysString {}
impl crate::__private::Sealed for AlwaysObject {}
impl JsonValueKind for AlwaysObject {}

macro_rules! into_json_itoa {
    ($($ty:ty)*) => {
        $(
            impl ToJson for $ty {
                type Kind = AlwaysNumber;
                fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysNumber {
                    let mut buffer = itoa::Buffer::new();
                    output.push_str(buffer.format(*self));
                    AlwaysNumber
                }
            }
        )*
    };
}

into_json_itoa![u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize];

impl ToJson for f32 {
    type Kind = AlwaysNumber;
    fn encode_json__jsony(&self, x: &mut TextWriter) -> AlwaysNumber {
        if !self.is_finite() {
            x.push_str("null");
            return AlwaysNumber;
        }
        x.finite_f32(*self);
        AlwaysNumber
    }
}
impl ToJson for f64 {
    type Kind = AlwaysNumber;
    fn encode_json__jsony(&self, x: &mut TextWriter) -> AlwaysNumber {
        if !self.is_finite() {
            x.push_str("null");
            return AlwaysNumber;
        }
        x.finite_f64(*self);
        AlwaysNumber
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
    #[allow(dead_code)]
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
    type Kind = AlwaysString;
    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysString {
        output.start_json_string();
        let bytes = self.as_bytes();
        let mut start = 0;
        for (i, &byte) in bytes.iter().enumerate() {
            let escape = ESCAPE[byte as usize];
            if escape == 0 {
                continue;
            }

            if start < i {
                unsafe {
                    output.push_unchecked_utf8(&bytes[start..i]);
                }
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
                    unsafe {
                        output.push_unchecked_utf8(bytes);
                    }
                    continue;
                }
            };
            unsafe {
                output.push_unchecked_utf8(s);
            }
        }

        if start != bytes.len() {
            unsafe {
                output.push_unchecked_utf8(&bytes[start..]);
            }
        }
        output.end_json_string();
        AlwaysString
    }
}

impl<T: ToJson> ToJson for Option<T> {
    type Kind = AnyValue;
    fn encode_json__jsony(&self, output: &mut TextWriter) -> AnyValue {
        if let Some(value) = self {
            value.encode_json__jsony(output);
        } else {
            output.push_str("null");
        }
        AnyValue
    }
}

impl<T: ToJson, const N: usize> ToJson for [T; N] {
    type Kind = AlwaysArray;
    fn encode_json__jsony(&self, array: &mut TextWriter) -> AlwaysArray {
        self.as_slice().encode_json__jsony(array)
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    type Kind = AlwaysArray;
    fn encode_json__jsony(&self, array: &mut TextWriter) -> AlwaysArray {
        self.as_slice().encode_json__jsony(array)
    }
}

impl<T: ToJson> ToJson for [T] {
    type Kind = AlwaysArray;
    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysArray {
        output.start_json_array();
        for value in self {
            value.encode_json__jsony(output);
            output.push_comma();
        }
        output.end_json_array()
    }
}

impl<T: ToJson + ?Sized> ToJson for Rc<T> {
    type Kind = T::Kind;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::encode_json__jsony(self, output)
    }
}

impl<T: ToJson + ?Sized> ToJson for Box<T> {
    type Kind = T::Kind;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::encode_json__jsony(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Arc<T> {
    type Kind = T::Kind;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::encode_json__jsony(self, output)
    }
}

impl ToJson for String {
    type Kind = AlwaysString;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysString {
        self.as_str().encode_json__jsony(output)
    }
}
impl<T: ToJson + ?Sized> ToJson for &T {
    type Kind = T::Kind;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::encode_json__jsony(*self, output)
    }
}

impl<V: ToJson, S> ToJson for HashSet<V, S> {
    type Kind = AlwaysArray;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysArray {
        output.start_json_array();
        for value in self {
            value.encode_json__jsony(output);
            output.push_comma();
        }
        output.end_json_array()
    }
}

impl<K: ToJson<Kind: JsonKeyKind>, V: ToJson> ToJson for BTreeMap<K, V> {
    type Kind = AlwaysObject;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysObject {
        output.start_json_object();
        for (key, value) in self {
            <K::Kind as JsonKeyKind>::key_prefix(output);
            key.encode_json__jsony(output);
            <K::Kind as JsonKeyKind>::key_suffix(output);
            value.encode_json__jsony(output);
            output.push_comma();
        }
        output.end_json_object()
    }
}

impl<K: ToJson<Kind: JsonKeyKind>, V: ToJson, S> ToJson for HashMap<K, V, S> {
    type Kind = AlwaysObject;

    fn encode_json__jsony(&self, output: &mut TextWriter) -> AlwaysObject {
        output.start_json_object();
        for (key, value) in self {
            <K::Kind as JsonKeyKind>::key_prefix(output);
            key.encode_json__jsony(output);
            <K::Kind as JsonKeyKind>::key_suffix(output);
            value.encode_json__jsony(output);
            output.push_comma();
        }
        output.end_json_object()
    }
}

macro_rules! to_json_tuple_impls {
    ($({$($field:literal : $tn: tt),*}),*) => {
        $(
        impl <$($tn: ToJson),*>
            ToJson for ($($tn,)*)
        {
            type Kind = AlwaysArray;

            fn encode_json__jsony(&self, output: &mut TextWriter) -> Self::Kind {
                output.start_json_array();
                #[allow(non_snake_case)]
                let ($($tn,)*) = self;
                $(
                    $tn.encode_json__jsony(output);
                    output.push_comma();
                )*
                output.end_json_array()
            }
        }
        )*
    };
}

to_json_tuple_impls! {
    {0: T0},
    {0: T0, 1: T1},
    {0: T0, 1: T1, 2: T2},
    {0: T0, 1: T1, 2: T2, 3: T3},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7},
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8}
}

impl ToJson for bool {
    type Kind = AnyValue;
    fn encode_json__jsony(&self, output: &mut TextWriter) -> AnyValue {
        if *self {
            output.push_str("true");
        } else {
            output.push_str("false");
        }
        AnyValue
    }
}

/// JSON Array Helper for constructing a JSON array in a TextWriter.
pub struct ArrayWriter<'a, 'b> {
    writer: &'a mut TextWriter<'b>,
}

impl Drop for ArrayWriter<'_, '_> {
    fn drop(&mut self) {
        self.writer.end_json_array();
    }
}

impl<'a, 'b> ArrayWriter<'a, 'b> {
    /// Create new ArrayWriter appending to the provided TextWriter.
    pub fn new(writer: &'a mut TextWriter<'b>) -> Self {
        writer.start_json_array();
        ArrayWriter { writer }
    }

    /// Construct a reference to an ArrayWriter that will avoid calling the drop handler.
    /// Useful, when you want the internals of an array with the opening and closing brackets,.
    /// useful when flattening an array.
    pub fn non_terminating<'k>(value: &'k mut &'a mut TextWriter<'b>) -> &'k mut Self {
        unsafe { std::mem::transmute(value) }
    }

    /// Push a value onto the array via an ValueWriter
    pub fn value_writer<'k>(&'k mut self) -> ValueWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ValueWriter {
            writer: self.writer,
        }
    }

    /// Push a object onto array via an ObjectWriter.
    pub fn object<'k>(&'k mut self) -> ObjectWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ObjectWriter::new(self.writer)
    }

    /// Push a nested array onto array via an ObjectWriter.
    pub fn array<'k>(&'k mut self) -> ArrayWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ArrayWriter::new(self.writer)
    }

    #[doc(hidden)]
    pub fn inner_writer<'j>(&'j mut self) -> &'j mut TextWriter<'b> {
        self.writer
    }

    pub fn push<'k, Kind: JsonValueKind>(
        &'k mut self,
        a: &(impl ToJson<Kind = Kind> + ?Sized),
    ) -> ValueExtender<'k, 'b, Kind> {
        self.writer.smart_array_comma();
        a.encode_json__jsony(self.writer);
        ValueExtender {
            writer: self.writer,
            phantom: PhantomData,
        }
    }

    /// Flatten an array implementing ToJson into current array.
    pub fn extend(&mut self, a: &dyn ToJson<Kind = AlwaysArray>) {
        self.writer.smart_array_comma();
        self.writer.join_parent_json_value_with_next();
        a.encode_json__jsony(self.writer);
        self.writer.join_array_with_next_value();
        self.writer.joining = false;
    }
}

/// JSON Object Helper for constructing a JSON Object in a TextWriter.
pub struct ObjectWriter<'a, 'b> {
    writer: &'a mut TextWriter<'b>,
}
impl Drop for ObjectWriter<'_, '_> {
    fn drop(&mut self) {
        self.writer.end_json_object();
    }
}
impl<'a, 'b> ObjectWriter<'a, 'b> {
    pub fn new(writer: &'a mut TextWriter<'b>) -> Self {
        writer.start_json_object();
        ObjectWriter { writer }
    }
    pub fn non_terminating<'k>(value: &'k mut &'a mut TextWriter<'b>) -> &'k mut Self {
        unsafe { std::mem::transmute(value) }
    }
    #[doc(hidden)]
    pub fn inner_writer<'j>(&'j mut self) -> &'j mut TextWriter<'b> {
        self.writer
    }
    pub fn extend(&mut self, a: &dyn ToJson<Kind = AlwaysObject>) {
        self.writer.smart_object_comma();
        self.writer.join_parent_json_value_with_next();
        a.encode_json__jsony(self.writer);
        self.writer.join_object_with_next_value();
        self.writer.joining = false;
    }
    pub fn dyn_key<'q>(&'q mut self, a: &dyn ToJson<Kind = AlwaysString>) -> ValueWriter<'q, 'b> {
        self.writer.smart_object_comma();
        a.encode_json__jsony(self.writer);
        self.writer.push_colon();
        ValueWriter {
            writer: self.writer,
        }
    }
    pub fn key<'q>(&'q mut self, a: &str) -> ValueWriter<'q, 'b> {
        self.writer.smart_object_comma();
        a.encode_json__jsony(self.writer);
        self.writer.push_colon();
        ValueWriter {
            writer: self.writer,
        }
    }
}
pub struct ValueWriter<'a, 'b> {
    writer: &'a mut TextWriter<'b>,
}
#[repr(transparent)]
pub struct ValueExtender<'a, 'b, T> {
    writer: &'a mut TextWriter<'b>,
    phantom: PhantomData<T>,
}

impl Drop for ValueWriter<'_, '_> {
    fn drop(&mut self) {
        self.writer.push_str("null");
    }
}

impl ValueExtender<'_, '_, AlwaysObject> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysObject>) {
        self.writer.join_object_with_next_value();
        value.encode_json__jsony(self.writer);
    }
}

impl ValueExtender<'_, '_, AlwaysArray> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysArray>) {
        self.writer.join_array_with_next_value();
        value.encode_json__jsony(self.writer);
    }
}

impl ValueExtender<'_, '_, AlwaysString> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysString>) {
        self.writer.join_string_with_next_value();
        value.encode_json__jsony(self.writer);
    }
}

impl<'a, 'b> ValueWriter<'a, 'b> {
    pub fn new(writer: &'a mut TextWriter<'b>) -> Self {
        ValueWriter { writer }
    }
    pub fn into_inner(self) -> &'a mut TextWriter<'b> {
        unsafe { std::mem::transmute(self) }
    }
    pub fn value<Kind: JsonValueKind>(
        self,
        a: &(impl ToJson<Kind = Kind> + ?Sized),
    ) -> ValueExtender<'a, 'b, Kind> {
        let writer = self.into_inner();
        a.encode_json__jsony(writer);
        ValueExtender {
            writer,
            phantom: PhantomData,
        }
    }
    pub fn object(self) -> ObjectWriter<'a, 'b> {
        ObjectWriter::new(self.into_inner())
    }
    pub fn array(self) -> ArrayWriter<'a, 'b> {
        ArrayWriter::new(self.into_inner())
    }
}
