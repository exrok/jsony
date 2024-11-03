// #[cfg(test)]
// mod tests;
use crate::{text::FromText, text_writer::TextWriter, FromJson, LazyValue, ToJson};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::Hash,
    marker::PhantomData,
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
};

use crate::parser::{Parser, Peek};

pub unsafe trait FieldVistor<'a> {
    unsafe fn visit(
        &mut self,
        field: *const str,
        parser: &mut Parser<'a>,
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
        field: *const str,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError>,
    drop: unsafe fn(ptr: NonNull<()>),
    ptr: NonNull<()>,
}

unsafe impl<'a> FieldVistor<'a> for FuncFieldVisitor<'a> {
    unsafe fn visit(
        &mut self,
        field: *const str,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        unsafe { (self.visit)(self.ptr, field, parser) }
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
    type Vistor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Vistor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, field, parser| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (field, ctx) = unsafe { parser.unfreeze_with_context(field) };
                let key = K::from_text(ctx, field)?;
                let value = T::json_decode(parser)?;
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
    type Vistor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Vistor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, field, parser| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (field, ctx) = unsafe { parser.unfreeze_with_context(field) };
                let key = K::from_text(ctx, field)?;
                let value = T::json_decode(parser)?;
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
    type Vistor = FuncFieldVisitor<'a>;

    unsafe fn new_field_visitor(ptr: NonNull<()>, _parser: &Parser<'a>) -> Self::Vistor {
        ptr.cast::<Self>().write(Self::new());
        // this common pattern can be generlized safely once we get safe transmute
        FuncFieldVisitor {
            visit: |ptr, field, parser| {
                let map = &mut *ptr.cast::<Self>().as_ptr();
                let (field, ctx) = unsafe { parser.unfreeze_with_context(field) };
                let key = K::from_text(ctx, field)?;
                let value = T::json_decode(parser)?;
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

pub unsafe trait FromJsonFieldVisitor<'a> {
    type Vistor: FieldVistor<'a>;
    unsafe fn new_field_visitor(ptr: NonNull<()>, parser: &Parser<'a>) -> Self::Vistor;
}

#[derive(Debug)]
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
unsafe impl<'a, T: Sized + FromJson<'a>> FromJson<'a> for Box<T> {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        let mut raw = Box::<T>::new_uninit();
        if let Err(err) = <T as FromJson<'a>>::emplace_from_json(
            NonNull::new_unchecked(raw.as_mut_ptr()).cast(),
            parser,
        ) {
            return Err(err);
        }
        dest.cast::<Box<T>>().write(raw.assume_init());
        Ok(())
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
        Ok(Some(_)) => (),
        Ok(None) => return Ok(()),
        Err(err) => return Err(err),
    };
    loop {
        K::json_decode(parser)?;
        match func(K::json_decode(parser)?) {
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
        let key = match K::json_decode(parser) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        parser.discard_colon()?;
        let value = match V::json_decode(parser) {
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
    parser.enter_array()?;
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

unsafe impl<'de> FromJson<'de> for &'de LazyValue {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        let start = parser.index;
        if let Err(err) = parser.skip_value() {
            return Err(err);
        }
        let raw = unsafe { std::str::from_utf8_unchecked(&parser.ctx.data[start..parser.index]) };
        dest.cast::<&'de LazyValue>().write(LazyValue::new(raw));
        Ok(())
    }
}
impl ToJson for &'_ LazyValue {
    type Kind = AnyValue;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> Self::Kind {
        if self.is_error() {
            output.push_str("null");
        } else {
            output.push_str(&self.raw);
        }
        AnyValue
    }
}

unsafe impl<'de> FromJson<'de> for () {
    unsafe fn emplace_from_json(
        _dest: NonNull<()>,
        parser: &mut Parser<'de>,
    ) -> Result<(), &'static DecodeError> {
        match parser.enter_array() {
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
        let value = parser.enter_array()?;
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
    drops: &[unsafe fn(NonNull<()>)],
) -> Result<(), &'static DecodeError> {
    if parser.enter_array()?.is_none() {
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
        if N == 0 {
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

/// [ToJson::Kind] Marker type indicating an array is always generated
pub struct AlwaysArray;
/// [ToJson::Kind] Marker type indicating an object is always generated
pub struct AlwaysObject;
/// [ToJson::Kind] Marker type indicating a string is always generated
pub struct AlwaysString;
/// [ToJson::Kind] Marker type indicating any type may be generated
pub struct AnyValue;

pub trait JsonValueKind: crate::__private::Sealed {}
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
                type Kind = AnyValue;
                fn json_encode__jsony(&self, output: &mut TextWriter) -> AnyValue {
                    let mut buffer = itoa::Buffer::new();
                    output.push_str(buffer.format(*self));
                    AnyValue
                }
            }
        )*
    };
}

into_json_itoa![u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize];

impl ToJson for f32 {
    type Kind = AnyValue;
    fn json_encode__jsony(&self, x: &mut TextWriter) -> AnyValue {
        if !self.is_finite() {
            x.push_str("null");
            return AnyValue;
        }
        x.finite_f32(*self);
        AnyValue
    }
}
impl ToJson for f64 {
    type Kind = AnyValue;
    fn json_encode__jsony(&self, x: &mut TextWriter) -> AnyValue {
        if !self.is_finite() {
            x.push_str("null");
            return AnyValue;
        }
        x.finite_f64(*self);
        AnyValue
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
    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysString {
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
    fn json_encode__jsony(&self, output: &mut TextWriter) -> AnyValue {
        if let Some(value) = self {
            value.json_encode__jsony(output);
        } else {
            output.push_str("null");
        }
        AnyValue
    }
}

impl<T: ToJson, const N: usize> ToJson for [T; N] {
    type Kind = AlwaysArray;
    fn json_encode__jsony(&self, array: &mut TextWriter) -> AlwaysArray {
        self.as_slice().json_encode__jsony(array)
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    type Kind = AlwaysArray;
    fn json_encode__jsony(&self, array: &mut TextWriter) -> AlwaysArray {
        self.as_slice().json_encode__jsony(array)
    }
}

impl<T: ToJson> ToJson for [T] {
    type Kind = AlwaysArray;
    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysArray {
        output.start_json_array();
        for value in self {
            value.json_encode__jsony(output);
            output.push_comma();
        }
        output.end_json_array()
    }
}

impl<'a, T: ToJson + ToOwned + ?Sized> ToJson for Cow<'a, T> {
    type Kind = T::Kind;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::json_encode__jsony(self.as_ref(), output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Rc<T> {
    type Kind = T::Kind;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::json_encode__jsony(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Box<T> {
    type Kind = T::Kind;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::json_encode__jsony(self, output)
    }
}
impl<T: ToJson + ?Sized> ToJson for Arc<T> {
    type Kind = T::Kind;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::json_encode__jsony(self, output)
    }
}

impl ToJson for String {
    type Kind = AlwaysString;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysString {
        self.as_str().json_encode__jsony(output)
    }
}
impl<T: ToJson + ?Sized> ToJson for &T {
    type Kind = T::Kind;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> T::Kind {
        <T as ToJson>::json_encode__jsony(*self, output)
    }
}

impl<V: ToJson, S> ToJson for HashSet<V, S> {
    type Kind = AlwaysArray;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysArray {
        output.start_json_array();
        for value in self {
            value.json_encode__jsony(output);
            output.push_comma();
        }
        output.end_json_array()
    }
}

impl<K: ToJson<Kind = AlwaysString>, V: ToJson> ToJson for BTreeMap<K, V> {
    type Kind = AlwaysObject;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysObject {
        output.start_json_object();
        for (key, value) in self {
            key.json_encode__jsony(output);
            output.push_colon();
            value.json_encode__jsony(output);
            output.push_comma();
        }
        output.end_json_object()
    }
}

impl<K: ToJson<Kind = AlwaysString>, V: ToJson, S> ToJson for HashMap<K, V, S> {
    type Kind = AlwaysObject;

    fn json_encode__jsony(&self, output: &mut TextWriter) -> AlwaysObject {
        output.start_json_object();
        for (key, value) in self {
            key.json_encode__jsony(output);
            output.push_colon();
            value.json_encode__jsony(output);
            output.push_comma();
        }
        output.end_json_object()
    }
}

impl ToJson for bool {
    type Kind = AnyValue;
    fn json_encode__jsony(&self, output: &mut TextWriter) -> AnyValue {
        if *self {
            output.push_str("true");
        } else {
            output.push_str("false");
        }
        AnyValue
    }
}

pub struct ArrayWriter<'a, 'b> {
    writer: &'a mut TextWriter<'b>,
}
impl<'a, 'b> Drop for ArrayWriter<'a, 'b> {
    fn drop(&mut self) {
        self.writer.end_json_array();
    }
}
impl<'a, 'b> ArrayWriter<'a, 'b> {
    pub fn new(writer: &'a mut TextWriter<'b>) -> Self {
        writer.start_json_array();
        ArrayWriter { writer }
    }
    pub fn non_terminating<'k>(value: &'k mut &'a mut TextWriter<'b>) -> &'k mut Self {
        unsafe { std::mem::transmute(value) }
    }
    pub fn value_writer<'k>(&'k mut self) -> ValueWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ValueWriter {
            writer: self.writer,
        }
    }
    pub fn object<'k>(&'k mut self) -> ObjectWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ObjectWriter::new(self.writer)
    }
    pub fn array<'k>(&'k mut self) -> ArrayWriter<'k, 'b> {
        self.writer.smart_array_comma();
        ArrayWriter::new(self.writer)
    }
    #[doc(hidden)]
    pub fn inner_writer<'j>(&'j mut self) -> &'j mut TextWriter<'b> {
        &mut self.writer
    }
    pub fn push<'k, Kind: JsonValueKind>(
        &'k mut self,
        a: &(impl ToJson<Kind = Kind> + ?Sized),
    ) -> ValueExtender<'k, 'b, Kind> {
        self.writer.smart_array_comma();
        a.json_encode__jsony(self.writer);
        ValueExtender {
            writer: self.writer,
            phantom: PhantomData,
        }
    }
    pub fn extend(&mut self, a: &dyn ToJson<Kind = AlwaysArray>) {
        self.writer.smart_array_comma();
        self.writer.join_parent_json_value_with_next();
        a.json_encode__jsony(self.writer);
        self.writer.join_array_with_next_value();
        self.writer.joining = false;
    }
}
pub struct ObjectWriter<'a, 'b> {
    writer: &'a mut TextWriter<'b>,
}
impl<'a, 'b> Drop for ObjectWriter<'a, 'b> {
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
        &mut self.writer
    }
    pub fn extend(&mut self, a: &dyn ToJson<Kind = AlwaysObject>) {
        self.writer.smart_object_comma();
        self.writer.join_parent_json_value_with_next();
        a.json_encode__jsony(self.writer);
        self.writer.join_object_with_next_value();
        self.writer.joining = false;
    }
    pub fn dyn_key<'q>(&'q mut self, a: &dyn ToJson<Kind = AlwaysString>) -> ValueWriter<'q, 'b> {
        self.writer.smart_object_comma();
        a.json_encode__jsony(self.writer);
        self.writer.push_colon();
        ValueWriter {
            writer: self.writer,
        }
    }
    pub fn key<'q>(&'q mut self, a: &str) -> ValueWriter<'q, 'b> {
        self.writer.smart_object_comma();
        a.json_encode__jsony(self.writer);
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

impl<'a, 'b> Drop for ValueWriter<'a, 'b> {
    fn drop(&mut self) {
        self.writer.push_str("null");
    }
}

impl<'a, 'b> ValueExtender<'a, 'b, AlwaysObject> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysObject>) {
        self.writer.join_object_with_next_value();
        value.json_encode__jsony(self.writer);
    }
}

impl<'a, 'b> ValueExtender<'a, 'b, AlwaysArray> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysArray>) {
        self.writer.join_array_with_next_value();
        value.json_encode__jsony(self.writer);
    }
}

impl<'a, 'b> ValueExtender<'a, 'b, AlwaysString> {
    pub fn extend(&mut self, value: &dyn ToJson<Kind = AlwaysString>) {
        self.writer.join_string_with_next_value();
        value.json_encode__jsony(self.writer);
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
        a.json_encode__jsony(writer);
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
