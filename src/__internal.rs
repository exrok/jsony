use std::{marker::PhantomData, ptr::NonNull};
pub struct NestedDynamicFieldDecoder<'a, T: FieldVisitor<'a>> {
    pub inner: T,
    pub destination: NonNull<()>,
    pub schema: ObjectSchema<'a>,
    pub bitset: u64,
    pub required: u64,
}

#[repr(transparent)]
pub struct UnsafeReturn;

pub struct DynamicFieldDecoder<'a> {
    pub destination: NonNull<()>,
    pub schema: ObjectSchema<'a>,
    pub alias: &'static [(usize, &'static str)],
    pub bitset: u64,
    pub required: u64,
}

/// helper function used for writing the default value from the Jsony derive macros
pub unsafe fn default_default<T: Default>(ptr: ::std::ptr::NonNull<()>) -> UnsafeReturn {
    // SAFETY: the caller supplies writable storage for a `T` field. This helper
    // writes exactly one initialized `T` into that location.
    unsafe {
        (ptr.as_ptr() as *mut T).write(T::default());
    }
    UnsafeReturn
}

pub const unsafe fn erase<'a>(
    input: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
) -> for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError> {
    // SAFETY: this only erases the parser lifetime in the function pointer type.
    // The returned function remains unsafe to call; callers must ensure it is
    // invoked only with a parser/input lifetime compatible with the original
    // `input` function.
    unsafe { std::mem::transmute(input) }
}

type Foo = for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError>;
#[inline(always)]
pub const unsafe fn erased_emplace_from_json<'a, T: crate::FromJson<'a>>() -> Foo {
    // SAFETY: this erases the concrete parser lifetime from `T`'s emplace
    // function so generated static schemas can store it. The resulting function
    // pointer is still unsafe to call; the schema decoder must only call it with
    // the same input lifetime used to instantiate `T: FromJson<'a>`.
    unsafe {
        std::mem::transmute(
            <T as crate::FromJson<'a>>::emplace_from_json
                as unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
        )
    }
}

pub unsafe fn erased_drop_in_place<M: Sized>(ptr: NonNull<()>) {
    // SAFETY: the caller guarantees `ptr` points to an initialized `M`.
    unsafe {
        std::ptr::drop_in_place(ptr.as_ptr() as *mut M);
    }
}

#[doc(hidden)]
pub struct SkipFieldVisitor<F> {
    pub skipped_field: &'static str,
    pub visitor: F,
}

impl<'a, F: FieldVisitor<'a>> FieldVisitor<'a> for SkipFieldVisitor<F> {
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        self.visitor.complete()
    }
    unsafe fn destroy(&mut self) {
        // SAFETY: `SkipFieldVisitor::destroy` has the same single-call
        // contract as the wrapped visitor's `destroy`.
        unsafe { self.visitor.destroy() }
    }
    fn visit(
        &mut self,
        borrowed: crate::json::ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError> {
        if borrowed.key() == self.skipped_field {
            borrowed.into_parser().at.skip_value()
        } else {
            self.visitor.visit(borrowed)
        }
    }
}

impl<'a> FieldVisitor<'a> for DynamicFieldDecoder<'a> {
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        if self.bitset & self.required != self.required {
            return Err(&MISSING_REQUIRED_FIELDS);
        }
        for (i, (emplace_default, field)) in self
            .schema
            .inner
            .defaults
            .iter()
            .zip(self.schema.fields())
            .enumerate()
        {
            if self.bitset & (1 << i) == 0 {
                // SAFETY: `destination` is the base of the object being
                // decoded, `field.offset` comes from the schema for that
                // object, and the bitset shows this field has not yet been
                // initialized.
                unsafe {
                    emplace_default(self.destination.byte_add(field.offset));
                }
            }
        }
        Ok(())
    }
    unsafe fn destroy(&mut self) {
        for (i, field) in self.schema.fields().iter().enumerate() {
            if self.bitset & (1 << i) != 0 {
                // SAFETY: the bitset records only fields successfully
                // initialized by this decoder, and `field.offset` points to the
                // corresponding field storage inside `destination`.
                unsafe {
                    (self.schema.inner.drops[i])(self.destination.byte_add(field.offset));
                }
            }
        }
    }
    fn visit(
        &mut self,
        mut borrowed: ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError> {
        let field_name = borrowed.key();
        'unused: {
            let (index, field) = 'found: {
                let fields = self.schema.fields();
                for (index, field) in fields.iter().enumerate() {
                    if field.name != field_name {
                        continue;
                    }
                    break 'found (index, field);
                }
                for (index, alias_name) in self.alias {
                    if *alias_name != field_name {
                        continue;
                    }
                    break 'found (*index, &fields[*index]);
                }
                break 'unused;
            };
            let parser = borrowed.into_parser();
            let mask = 1 << index;
            if self.bitset & mask != 0 {
                if let JsonParentContext::None = parser.parent_context {
                    parser.parent_context = JsonParentContext::ObjectKey(field.name);
                }
                return Err(&DUPLICATE_FIELD);
            }
            // SAFETY: `destination + field.offset` is the uninitialized storage
            // for this schema field, and `field.decode` is the matching
            // generated emplace function. Duplicate fields are rejected before
            // this call, so the destination field is not already initialized.
            if let Err(err) =
                unsafe { (field.decode)(self.destination.byte_add(field.offset), parser) }
            {
                if let JsonParentContext::None = parser.parent_context {
                    parser.parent_context = JsonParentContext::ObjectKey(field.name);
                }
                return Err(err);
            }
            self.bitset |= mask;
            return Ok(());
        }
        if let Some(vis) = borrowed.parser().visit_unused_field {
            vis(borrowed.reborrow());
        }

        return borrowed.into_parser().at.skip_value();
    }
}

use crate::{
    error::{DUPLICATE_FIELD, MISSING_REQUIRED_FIELDS},
    json::{FieldVisitor, ParserWithBorrowedKey},
    parser::{JsonParentContext, Parser},
};

/// Array decoder for derived multi-field tuple structs. Re-exported from
/// [`crate::json`] so generated code can reach it under the `__internal` path.
pub use crate::json::dyn_tuple_decode;

type DecodeFn<'a> = unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>;
use super::DecodeError;

// maximum number fields.
// currently 63 based on how we compute the bit masks.
pub const MAX_FIELDS: usize = 63;
pub struct Field<'a> {
    pub name: &'static str,
    pub offset: usize,
    pub decode: DecodeFn<'a>,
}

// gets sorted by defaults
// might add a static drop at some point.
pub struct ObjectSchemaInner {
    pub fields: &'static [Field<'static>],
    pub drops: &'static [unsafe fn(NonNull<()>)],
    pub defaults: &'static [unsafe fn(NonNull<()>) -> UnsafeReturn],
}

#[derive(Clone, Copy)]
pub struct ObjectSchema<'a> {
    pub inner: &'static ObjectSchemaInner,
    pub phantom: PhantomData<&'a ()>,
}

impl<'a> ObjectSchema<'a> {
    pub fn fields(&self) -> &[Field<'a>] {
        // SAFETY: schemas store field descriptors in static memory. Generated
        // schema construction erases the parser lifetime from each decode
        // function; callers of those functions remain unsafe and must uphold
        // the lifetime compatibility required by the schema.
        unsafe {
            #[allow(clippy::unnecessary_cast, reason = "clippy false positive")]
            std::slice::from_raw_parts(
                self.inner.fields.as_ptr() as *const Field<'a>,
                self.inner.fields.len(),
            )
        }
    }
}

/// Alias-table index meaning "match this key, then discard it" rather than
/// decode it into a field.
///
/// Entries in the `alias` slice passed to [`ObjectSchema::decode_with_alias`]
/// map a key to a field index. An entry using this index resolves to no field,
/// so the matched key is skipped without being forwarded to the unused-field
/// visitor. Derived code uses it to swallow an internal tag key that shares the
/// variant object with the real fields. The value is unreachable as a real
/// index: a schema holds at most 64 fields (the seen/required bitset is a
/// `u64`), and no slice can be `usize::MAX` long.
pub const SKIP_FIELD_ALIAS_INDEX: usize = usize::MAX;

impl<'a> ObjectSchema<'a> {
    pub unsafe fn decode_with_alias(
        &self,
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
        mut unused: Option<&mut dyn FieldVisitor<'a>>,
        alias: &[(usize, &'static str)],
    ) -> Result<(), &'static DecodeError> {
        let all = (1u64 << self.inner.fields.len()) - 1;
        let mut bitset = 0;

        let error = 'error: {
            match parser.at.enter_object(&mut parser.scratch) {
                Ok(Some(mut key)) => {
                    'key_loop: loop {
                        'next: {
                            'unused_dont_forward: {
                                'unused: {
                                    let (index, field) = 'found: {
                                        let fields = self.fields();
                                        for (index, field) in fields.iter().enumerate() {
                                            if field.name != key {
                                                continue;
                                            }
                                            break 'found (index, field);
                                        }
                                        for (index, alias_name) in alias {
                                            if *alias_name != key {
                                                continue;
                                            }
                                            // An out-of-bounds index (i.e.
                                            // `SKIP_FIELD_ALIAS_INDEX`) means
                                            // skip the matched key.
                                            if let Some(field) = fields.get(*index) {
                                                break 'found (*index, field);
                                            } else {
                                                break 'unused_dont_forward;
                                            }
                                        }
                                        break 'unused;
                                    };
                                    let mask = 1 << index;
                                    if bitset & mask != 0 {
                                        if let JsonParentContext::None = parser.parent_context {
                                            parser.parent_context =
                                                JsonParentContext::ObjectKey(field.name);
                                        }

                                        break 'error &DUPLICATE_FIELD;
                                    }

                                    // SAFETY: the caller provides `dest` as
                                    // writable storage for the object described
                                    // by this schema. This field has not been
                                    // seen yet, so its slot is uninitialized.
                                    if let Err(err) = unsafe {
                                        (field.decode)(dest.byte_add(field.offset), parser)
                                    } {
                                        if let JsonParentContext::None = parser.parent_context {
                                            parser.parent_context =
                                                JsonParentContext::ObjectKey(field.name);
                                        }
                                        break 'error err;
                                    }
                                    bitset |= mask;
                                    break 'next;
                                }
                                if let Some(ref mut unused_processor) = unused {
                                    // SAFETY: `key` was returned by this parser
                                    // for the current object step and remains
                                    // live until the parser advances to the
                                    // next key below.
                                    let borrowed =
                                        unsafe { ParserWithBorrowedKey::new(key, parser) };
                                    if let Err(err) = unused_processor.visit(borrowed) {
                                        break 'error err;
                                    }
                                    break 'next;
                                }
                                if let Some(vis) = parser.visit_unused_field {
                                    // SAFETY: same key lifetime argument as the
                                    // unused-field visitor path above.
                                    vis(unsafe { ParserWithBorrowedKey::new(key, parser) });
                                }
                            }

                            if let Err(error) = parser.at.skip_value() {
                                break 'error error;
                            }
                        }

                        match parser.at.object_step(&mut parser.scratch) {
                            Ok(Some(next_key2)) => {
                                key = next_key2;
                                continue 'key_loop;
                            }
                            Ok(None) => {
                                break 'key_loop;
                            }
                            Err(err) => break 'error err,
                        }
                    }
                }
                Ok(None) => {}
                Err(err) => break 'error err,
            };
            let default = (1u64 << self.inner.defaults.len()) - 1;
            if (bitset | default) & all != all {
                parser.parent_context = JsonParentContext::Schema {
                    schema: self.inner,
                    mask: all & !(bitset | default),
                };
                break 'error &MISSING_REQUIRED_FIELDS;
            }
            if let Some(visitor) = &mut unused {
                if let Err(err) = visitor.complete() {
                    break 'error err;
                }
            }
            // todo can optimize
            for (i, (emplace_default, field)) in
                self.inner.defaults.iter().zip(self.fields()).enumerate()
            {
                if bitset & (1 << i) == 0 {
                    // SAFETY: fields missing from `bitset` have not been
                    // initialized, and defaults are paired with the first
                    // fields in schema order by generated schema construction.
                    unsafe {
                        emplace_default(dest.byte_add(field.offset));
                    }
                }
            }
            return Ok(());
        };

        for (i, (drop, field)) in self.inner.drops.iter().zip(self.fields()).enumerate() {
            if bitset & (1 << i) != 0 {
                // SAFETY: the bitset records exactly the fields initialized
                // before the decode error.
                unsafe {
                    drop(dest.byte_add(field.offset));
                }
            }
        }
        if let Some(visitor) = unused {
            // SAFETY: the visitor has not completed successfully and is being
            // destroyed exactly once on the error path.
            unsafe { visitor.destroy() }
        }
        Err(error)
    }
    pub unsafe fn decode(
        &self,
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
        mut unused: Option<&mut dyn FieldVisitor<'a>>,
    ) -> Result<(), &'static DecodeError> {
        let all = (1u64 << self.inner.fields.len()) - 1;
        let mut bitset = 0;

        let error = 'with_next_key: {
            match parser.at.enter_object(&mut parser.scratch) {
                Ok(Some(mut key)) => {
                    'key_loop: loop {
                        'next: {
                            for (index, field) in self.fields().iter().enumerate() {
                                let mask = 1 << index;
                                if field.name != key {
                                    continue;
                                }
                                if bitset & mask != 0 {
                                    if let JsonParentContext::None = parser.parent_context {
                                        parser.parent_context =
                                            JsonParentContext::ObjectKey(field.name);
                                    }

                                    break 'with_next_key &DUPLICATE_FIELD;
                                }
                                // SAFETY: the caller provides `dest` as storage
                                // for the object described by this schema. This
                                // field is not marked initialized in `bitset`.
                                if let Err(err) =
                                    unsafe { (field.decode)(dest.byte_add(field.offset), parser) }
                                {
                                    if let JsonParentContext::None = parser.parent_context {
                                        parser.parent_context =
                                            JsonParentContext::ObjectKey(field.name);
                                    }
                                    break 'with_next_key err;
                                }
                                bitset |= mask;
                                break 'next;
                            }

                            if let Some(ref mut unused_processor) = unused {
                                // SAFETY: `key` was returned by this parser for
                                // the current object step and remains live
                                // until the parser advances to the next key.
                                let borrowed = unsafe { ParserWithBorrowedKey::new(key, parser) };
                                if let Err(err) = unused_processor.visit(borrowed) {
                                    break 'with_next_key err;
                                }
                                break 'next;
                            }

                            if let Some(vis) = parser.visit_unused_field {
                                // SAFETY: same key lifetime argument as the
                                // unused-field visitor path above.
                                vis(unsafe { ParserWithBorrowedKey::new(key, parser) });
                            }

                            if let Err(error) = parser.at.skip_value() {
                                break 'with_next_key error;
                            }
                        }

                        match parser.at.object_step(&mut parser.scratch) {
                            Ok(Some(next_key2)) => {
                                key = next_key2;
                                continue 'key_loop;
                            }
                            Ok(None) => {
                                break 'key_loop;
                            }
                            Err(err) => break 'with_next_key err,
                        }
                    }
                }
                Ok(None) => {}
                Err(err) => break 'with_next_key err,
            };
            let default = (1u64 << self.inner.defaults.len()) - 1;
            if (bitset | default) & all != all {
                parser.parent_context = JsonParentContext::Schema {
                    schema: self.inner,
                    mask: all & !(bitset | default),
                };
                break 'with_next_key &MISSING_REQUIRED_FIELDS;
            }
            if let Some(visitor) = &mut unused {
                if let Err(err) = visitor.complete() {
                    break 'with_next_key err;
                }
            }
            // todo can optimize
            for (i, (emplace_default, field)) in
                self.inner.defaults.iter().zip(self.fields()).enumerate()
            {
                if bitset & (1 << i) == 0 {
                    // SAFETY: fields missing from `bitset` have not been
                    // initialized, and defaults are paired with the first
                    // fields in schema order by generated schema construction.
                    unsafe {
                        emplace_default(dest.byte_add(field.offset));
                    }
                }
            }
            return Ok(());
        };

        for (i, (drop, field)) in self.inner.drops.iter().zip(self.fields()).enumerate() {
            if bitset & (1 << i) != 0 {
                // SAFETY: the bitset records exactly the fields initialized
                // before the decode error.
                unsafe {
                    drop(dest.byte_add(field.offset));
                }
            }
        }
        if let Some(visitor) = unused {
            // SAFETY: the visitor has not completed successfully and is being
            // destroyed exactly once on the error path.
            unsafe { visitor.destroy() }
        }
        Err(error)
    }
}

// Note that we make P generic here so that we couple lifetime with T
// P will always be &mut Parser<'_>
pub const unsafe fn emplace_json_for_with_attribute<P, T, F>(
    _func: &F,
) -> for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError>
where
    F: Fn(P) -> Result<T, &'static DecodeError>,
{
    const { assert!(std::mem::size_of::<F>() == 0) }
    let func: unsafe fn(dest: NonNull<()>, parser: P) -> Result<(), &'static DecodeError> =
        |dest: NonNull<()>, parser: P| -> Result<(), &'static DecodeError> {
            // SAFETY: the const assertion above proves `F` is zero-sized, so
            // constructing it from `()` reads no bytes and creates the function
            // item/closure value represented by `F`.
            let func = unsafe { std::mem::transmute_copy::<(), F>(&()) };
            match func(parser) {
                Ok(value) => {
                    let value: T = value;
                    // SAFETY: callers invoke this returned emplace function
                    // with writable storage for `T`.
                    unsafe {
                        dest.cast::<T>().write(value);
                    }
                    Ok(())
                }
                Err(err) => Err(err),
            }
        };
    // SAFETY: the returned function pointer erases only the parser lifetime so
    // it can be stored in generated static schema data. Calling it remains
    // unsafe and must use the lifetime-compatible parser type.
    unsafe { std::mem::transmute(func) }
}

// Safety: The function returned has had the Parser lifetime erased, this done so that
// it can be stored in a static to workaround current limitations in the rust solver.
//
// You must not used the returned function pointer with an incompatible life time. Generally
// this enforced by ObjectSchema, where we this used internally and generated by proc macros.
//
// Panics if the size of F is not zero, as long as it's a simple function this should be true.
pub const unsafe fn emplace_json_for_validate_attribute<'a, T: crate::FromJson<'a>, F>(
    _func: &F,
) -> for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError>
where
    F: Fn(&T) -> Result<(), String>,
{
    const { assert!(std::mem::size_of::<F>() == 0) }
    let func: unsafe fn(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> =
        |dest: NonNull<()>, parser: &mut Parser<'a>| -> Result<(), &'static DecodeError> {
            // SAFETY: callers invoke this returned emplace function with
            // writable storage for `T`, which is exactly `T::emplace_from_json`'s
            // destination contract.
            match unsafe { T::emplace_from_json(dest, parser) } {
                Ok(()) => {
                    // SAFETY: the const assertion above proves `F` is
                    // zero-sized, so no bytes are read to construct it.
                    let func = unsafe { std::mem::transmute_copy::<(), F>(&()) };
                    // SAFETY: `T::emplace_from_json` returned `Ok`, so `dest`
                    // now contains an initialized `T`.
                    match func(unsafe { &*dest.cast().as_ptr() }) {
                        Ok(_) => Ok(()),
                        Err(err) => {
                            // SAFETY: validation failed after `T` was
                            // initialized, so drop that initialized value before
                            // reporting the error.
                            unsafe {
                                dest.cast::<T>().drop_in_place();
                            }
                            parser.report_error(err);
                            return Err(&crate::error::CUSTOM_FIELD_VALIDATION_ERROR);
                        }
                    }
                }
                Err(err) => Err(err),
            }
        };
    // SAFETY: the returned function pointer erases only the parser lifetime so
    // it can be stored in generated static schema data. Calling it remains
    // unsafe and must use the lifetime-compatible parser type.
    unsafe { std::mem::transmute(func) }
}
