// #[cfg(test)]
// mod tests;
use crate::{FromJson, OutputBuffer, ToJson};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::Hash,
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
};

use crate::parser::{Parser, Peek};

pub trait FieldVistor<'a> {
    fn visit(&mut self, field: &str, parser: &mut Parser<'a>) -> Result<(), &'static DecodeError>;
    fn complete(&mut self) -> Result<(), &'static DecodeError>;
    /// # Safety
    /// must only call once.
    unsafe fn destroy(&mut self);
}

#[doc(hidden)]
pub struct FuncFieldVisitor<'a> {
    visit: unsafe fn(
        ptr: NonNull<()>,
        field: &str,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError>,
    drop: unsafe fn(ptr: NonNull<()>),
    ptr: NonNull<()>,
}

impl<'a> FieldVistor<'a> for FuncFieldVisitor<'a> {
    fn visit(&mut self, field: &str, parser: &mut Parser<'a>) -> Result<(), &'static DecodeError> {
        unsafe { (self.visit)(self.ptr, field, parser) }
    }
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        Ok(())
    }
    unsafe fn destroy(&mut self) {
        unsafe { (self.drop)(self.ptr) }
    }
}

pub trait FromJsonStr<'a>: Sized {
    fn from_json_str(inner: &str, parser: &Parser<'a>) -> Result<Self, &'static DecodeError>;
}
impl<'a> FromJsonStr<'a> for &'a str {
    fn from_json_str(inner: &str, parser: &Parser<'a>) -> Result<Self, &'static DecodeError> {
        if let Some(text) = parser.try_zerocopy(inner) {
            Ok(text)
        } else {
            Err(&DecodeError {
                message: "String contains escapes",
            })
        }
    }
}
impl<'a> FromJsonStr<'a> for Cow<'a, str> {
    fn from_json_str(inner: &str, parser: &Parser<'a>) -> Result<Self, &'static DecodeError> {
        if let Some(text) = parser.try_zerocopy(inner) {
            Ok(Cow::Borrowed(text))
        } else {
            Ok(Cow::Owned(inner.to_owned()))
        }
    }
}
impl<'a> FromJsonStr<'a> for String {
    fn from_json_str(inner: &str, parser: &Parser<'a>) -> Result<Self, &'static DecodeError> {
        Ok(inner.into())
    }
}
impl<'a> FromJsonStr<'a> for Box<str> {
    fn from_json_str(inner: &str, parser: &Parser<'a>) -> Result<Self, &'static DecodeError> {
        Ok(inner.into())
    }
}

unsafe impl<'a, K, T> FromJsonFieldVisitor<'a> for HashMap<K, T>
where
    K: FromJsonStr<'a> + Hash + Eq,
    T: FromJson<'a>,
{
    type Vistor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, parser: &Parser<'a>) -> Self::Vistor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, field, parser| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let key = K::from_json_str(field, parser)?;
                let value = T::decode_json(parser)?;
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
    K: FromJsonStr<'a>,
    T: FromJson<'a>,
{
    type Vistor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, parser: &Parser<'a>) -> Self::Vistor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, field, parser| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let key = K::from_json_str(field, parser)?;
                let value = T::decode_json(parser)?;
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

unsafe trait FromJsonFieldVisitor<'a> {
    type Vistor: FieldVistor<'a>;
    unsafe fn new_field_visitor(ptr: NonNull<()>, parser: &Parser<'a>) -> Self::Vistor;
}

#[derive(Debug)]
pub struct DecodeError {
    pub message: &'static str,
}

static INVALID_NUMERIC_LITERAL: DecodeError = DecodeError {
    message: "Invalid numeric literal",
};
static STRING_CONTAINS_ESCAPES: DecodeError = DecodeError {
    message: "String contains escapes",
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
                    match parser.consume_numeric_literal() {
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

unsafe impl<'a> FromJson<'a> for &'a str {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.read_string_unescaped() {
            Ok(raw_str) => {
                if let Ok(value) = crate::strings::escape_to_str(raw_str) {
                    dest.cast::<&str>().write(value);
                    return Ok(());
                }
                // todo make escape_to_str return proper erros
                Err(&STRING_CONTAINS_ESCAPES)
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
        match parser.read_string_unescaped() {
            Ok(raw_str) => {
                if let Ok(value) = crate::strings::escape_to_string(raw_str) {
                    dest.cast::<String>().write(value);
                    return Ok(());
                }
                Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS)
            }
            Err(err) => Err(err),
        }
    }
}

unsafe impl<'a> FromJson<'a> for Box<str> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.read_string_unescaped() {
            Ok(raw_str) => {
                if let Ok(value) = crate::strings::escape_to_string(raw_str) {
                    dest.cast::<Box<str>>().write(value.into());
                    return Ok(());
                }
                Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS)
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
        match parser.read_string_unescaped() {
            Ok(raw_str) => {
                if let Ok(value) = crate::strings::escape_to_string(raw_str) {
                    dest.cast::<Cow<str>>().write(Cow::Owned(value));
                    return Ok(());
                }
                Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS)
            }
            Err(err) => Err(err),
        }
    }
}

fn decode_array_sequence<'de, K: FromJson<'de>>(
    parser: &mut Parser<'de>,
    mut func: impl FnMut(K) -> Result<(), &'static DecodeError>,
) -> Result<(), &'static DecodeError> {
    match parser.enter_array() {
        Ok(Some(value)) => (),
        Ok(None) => return Ok(()),
        Err(err) => return Err(err),
    };
    loop {
        K::decode_json(parser)?;
        match func(K::decode_json(parser)?) {
            Ok(()) => (),
            Err(err) => return Err(err),
        }
        match parser.array_step() {
            Ok(Some(_)) => (),
            Ok(None) => return Ok(()),
            Err(err) => return Err(err),
        }
    }
}

pub(crate) fn decode_object_sequence<'de, K: FromJson<'de>, V: FromJson<'de>>(
    parser: &mut Parser<'de>,
    mut func: impl FnMut(K, V) -> Result<(), &'static DecodeError>,
) -> Result<(), &'static DecodeError> {
    match parser.enter_seen_object_at_first_key() {
        Ok(Some(())) => (),
        Ok(None) => return Ok(()),
        Err(err) => return Err(err),
    }
    loop {
        let key = match K::decode_json(parser) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        parser.discard_colon()?;
        let value = match V::decode_json(parser) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        match func(key, value) {
            Ok(()) => (),
            Err(err) => return Err(err),
        }
        match parser.object_step_at_key() {
            Ok(Some(())) => (),
            Ok(None) => return Ok(()),
            Err(err) => return Err(err),
        }
    }
}

unsafe impl<'de, T: FromJson<'de>> FromJson<'de> for Vec<T> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let mut array = Vec::<T>::new();
        let mut value = parser.enter_array()?;
        while value.is_some() {
            unsafe {
                array.reserve(1);
                let len = array.len();
                if let Err(err) = <T as FromJson<'de>>::emplace_from_json(
                    NonNull::new_unchecked(array.as_mut_ptr()).add(len).cast(),
                    parser,
                ) {
                    println!("Where we at: {:?}", parser);
                    return Err(err);
                } else {
                    array.set_len(len + 1);
                }
            }
            value = parser.array_step()?;
        }
        dest.cast::<Vec<T>>().write(array);
        Ok(())
    }
}

unsafe impl<'de, K: FromJson<'de> + Ord, V: FromJson<'de>> FromJson<'de> for BTreeMap<K, V> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<BTreeMap<K, V>>().write(BTreeMap::new());
        let map = &mut *dest.cast::<BTreeMap<K, V>>().as_ptr();
        let result = decode_object_sequence(parser, |k, v| {
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
        let result = decode_array_sequence(parser, |k| {
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
unsafe impl<'de, K: FromJson<'de> + Eq + Hash> FromJson<'de> for HashSet<K> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<HashSet<K>>().write(HashSet::new());
        let map = &mut *dest.cast::<HashSet<K>>().as_ptr();
        let result = decode_array_sequence(parser, |k| {
            map.insert(k);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<HashSet<K>>().as_ptr());
            }
        }
        result
    }
}
unsafe impl<'de, K: FromJson<'de> + Eq + Hash, V: FromJson<'de>> FromJson<'de> for HashMap<K, V> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        dest.cast::<HashMap<K, V>>().write(HashMap::new());
        let map = &mut *dest.cast::<HashMap<K, V>>().as_ptr();
        let result = decode_object_sequence(parser, |k, v| {
            map.insert(k, v);
            Ok(())
        });
        if result.is_err() {
            unsafe {
                std::ptr::drop_in_place(dest.cast::<HashMap<K, V>>().as_ptr());
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
        match parser.peek() {
            Ok(Peek::Null) => {
                parser.discard_seen_null()?;
                dest.cast::<Option<T>>().write(None);
                Ok(())
            }
            Ok(_) => {
                // not sure if this is legal or not or not??
                // Currently enums can't use padding to store discrements
                // Thus, the discriminant must store in niche
                if size_of::<Option<T>>() == size_of::<T>() {
                    T::emplace_from_json(dest, parser)
                } else {
                    let mut value = std::mem::MaybeUninit::<T>::uninit();
                    T::emplace_from_json(
                        NonNull::new_unchecked(value.as_mut_ptr()).cast(),
                        parser,
                    )?;
                    dest.cast::<Option<T>>().write(Some(value.assume_init()));
                    Ok(())
                }
            }
            Err(err) => Err(err),
        }
    }
}

direct_impl_integer_decode! {
    i128, u128, u64, i64, f32, f64, u32, i32, u16, i16, u8, i8, isize, usize
}

unsafe impl<'a> FromJson<'a> for bool {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match parser.peek() {
            Ok(Peek::True) => {
                parser.discard_seen_true()?;
                dest.cast::<bool>().write(true);
                Ok(())
            }
            Ok(Peek::False) => {
                parser.discard_seen_false()?;
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
        match parser.read_string_unescaped() {
            Ok(raw_str) => {
                if let Ok(value) = crate::strings::escape_to_str(raw_str) {
                    let mut chars = value.chars();
                    let mut ch = chars.next();
                    if chars.next().is_some() {
                        return Err(&DecodeError {
                            message: "Expected a single char",
                        });
                    }
                    if let Some(ch) = ch {
                        dest.cast::<char>().write(ch);
                        return Ok(());
                    }
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
    let mut value = parser.enter_array()?;
    let mut current = dest;
    let err = 'error: {
        for i in 0..n {
            if let Err(err) = decode(current, parser) {
                break 'error Err(err);
            }
            current = current.byte_add(size);
            match parser.array_step() {
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

unsafe impl<'de> FromJson<'de> for () {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let mut value = parser.enter_array()?;
        if value.is_none() {
            return Ok(());
        }
        Err(&ARRAY_LENGTH_MISMATCH)
    }
}

unsafe impl<'de, T0: FromJson<'de>> FromJson<'de> for (T0,) {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let mut value = parser.enter_array()?;
        if value.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        <T0 as FromJson<'de>>::emplace_from_json(
            dest.byte_add(std::mem::offset_of!(Self, 0)),
            parser,
        )?;
        let error = match parser.array_step() {
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
        if parser.enter_array()?.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        let mut error: &'static DecodeError;
        't0: {
            if let Err(err) = <T0 as FromJson<'de>>::emplace_from_json(
                dest.byte_add(std::mem::offset_of!(Self, 0)),
                parser,
            ) {
                error = err;
                break 't0;
            }
            't1: {
                if let Err(err) = parser.array_step_expecting_more() {
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
                match parser.array_step() {
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
        if parser.enter_array()?.is_none() {
            return Err(&ARRAY_LENGTH_MISMATCH);
        }
        let mut error: &'static DecodeError;
        't0: {
            if let Err(err) = <T0 as FromJson<'de>>::emplace_from_json(
                dest.byte_add(std::mem::offset_of!(Self, 0)),
                parser,
            ) {
                error = err;
                break 't0;
            }
            't1: {
                if let Err(err) = parser.array_step_expecting_more() {
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
                    if let Err(err) = parser.array_step_expecting_more() {
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
                    match parser.array_step() {
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

#[inline(never)]
unsafe fn dyn_tuple_decode<'a>(
    dest: NonNull<()>,
    parser: &mut Parser<'a>,
    fields: &[(
        usize,
        unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
    )],
    drops: &[(unsafe fn(NonNull<()>))],
) -> Result<(), &'static DecodeError> {
    if parser.enter_array()?.is_none() {
        if (fields.is_empty()) {
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
                if let Err(err) = parser.array_step() {
                    break 'with_error err;
                }
                if let Err(err) = decode_fn(dest.byte_add(*offset), parser) {
                    break 'with_error err;
                }
                complete += 1;
            }
        }
        match parser.array_step() {
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
    {0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7}
}

unsafe impl<'de, T: FromJson<'de>, const N: usize> FromJson<'de> for [T; N] {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        if (N == 0) {
            return match parser.enter_array() {
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

impl Default for RawBuf {
    fn default() -> Self {
        Self::new()
    }
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
impl crate::__private::Sealed for ArrayBuf {}
impl crate::__private::Sealed for RawBuf {}
impl crate::__private::Sealed for StringBuf {}
impl crate::__private::Sealed for ObjectBuf {}

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

impl<'a, T: ToJson + Clone> ToJson for Cow<'a, T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Rc<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Box<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Arc<T> {
    type Into = T::Into;

    fn jsonify_into(&self, output: &mut Self::Into) {
        <T as ToJson>::jsonify_into(self, output)
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
