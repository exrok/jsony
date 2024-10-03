// #[cfg(test)]
// mod tests;
use crate::FromJson;
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::Hash,
    ptr::NonNull,
};

use crate::parser::{Parser, Peek};

pub trait FieldVistor<'a> {
    fn visit(&mut self, field: &str, parser: &mut Parser<'a>) -> Result<(), &'static DecodeError>;
    fn complete(&mut self) -> Result<(), &'static DecodeError>;
    // Safety must only call once.
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
                return Err(&STRING_CONTAINS_ESCAPES);
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
                return Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS);
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
                return Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS);
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
                return Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS);
            }
            Err(err) => Err(err),
        }
    }
}

fn decode_array_sequence<'de, K: FromJson<'de>>(
    parser: &mut Parser<'de>,
    mut func: impl FnMut(K) -> Result<(), &'static DecodeError>,
) -> Result<(), &'static DecodeError> {
    let value = match parser.enter_array() {
        Ok(Some(value)) => (),
        Ok(None) => return Ok(()),
        Err(err) => return Err(err),
    };
    loop {
        if let Err(err) = K::decode_json(parser) {
            return Err(err);
        }
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
        if let Err(err) = parser.discard_colon() {
            return Err(err);
        }
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
                if let Err(err) = parser.discard_seen_null() {
                    return Err(err);
                }
                dest.cast::<Option<T>>().write(None);
                Ok(())
            }
            Ok(_) => {
                // not sure if this is legal or not or not??
                // Currently enums can't use padding to store discrements
                // Thus, the discriminant must store in niche
                if size_of::<Option<T>>() == size_of::<T>() {
                    return T::emplace_from_json(dest, parser);
                } else {
                    let mut value = std::mem::MaybeUninit::<T>::uninit();
                    if let Err(err) = T::emplace_from_json(
                        NonNull::new_unchecked(value.as_mut_ptr()).cast(),
                        parser,
                    ) {
                        return Err(err);
                    }
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
                if let Err(err) = parser.discard_seen_true() {
                    return Err(err);
                }
                dest.cast::<bool>().write(true);
                Ok(())
            }
            Ok(Peek::False) => {
                if let Err(err) = parser.discard_seen_false() {
                    return Err(err);
                }
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
                return Err(&STRING_CONTAINS_INVALID_ESCAPE_LITERALS);
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
        return Err(&ARRAY_LENGTH_MISMATCH);
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
        if let Err(err) = <T0 as FromJson<'de>>::emplace_from_json(
            dest.byte_add(std::mem::offset_of!(Self, 0)),
            parser,
        ) {
            return Err(err);
        }
        let error = match parser.array_step() {
            Ok(None) => return Ok(()),
            Ok(Some(_)) => &ARRAY_LENGTH_MISMATCH,
            Err(err) => err,
        };
        std::ptr::drop_in_place(dest.cast::<T0>().as_ptr());
        return Err(error);
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
                    error = &err;
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
        return Err(error);
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
                    error = &err;
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
                        error = &err;
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
        return Err(error);
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
            if let Err(err) = decode_fn(dest.byte_add(*offset), parser) {
                return Err(err);
            }
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
