use std::{marker::PhantomData, ptr::NonNull};
pub struct NestedDynamicFieldDecoder<'a, T: FieldVistor<'a>> {
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
    pub bitset: u64,
    pub required: u64,
}

pub const unsafe fn erase<'a>(
    input: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
) -> for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError> {
    unsafe { std::mem::transmute(input) }
}

type Foo = for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError>;
#[inline(always)]
pub const unsafe fn erased_emplace_from_json<'a, T: crate::FromJson<'a>>() -> Foo {
    std::mem::transmute(
        <T as crate::FromJson<'a>>::emplace_from_json
            as unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
    )
}
pub unsafe fn erased_drop_in_place<M: Sized>(ptr: NonNull<()>) {
    std::ptr::drop_in_place(ptr.as_ptr() as *mut M);
}

#[doc(hidden)]
pub struct SkipFieldVisitor<F> {
    pub skipped_field: &'static str,
    pub visitor: F,
}

unsafe impl<'a, F: FieldVistor<'a>> FieldVistor<'a> for SkipFieldVisitor<F> {
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        self.visitor.complete()
    }
    unsafe fn destroy(&mut self) {
        self.visitor.destroy()
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

unsafe impl<'a> FieldVistor<'a> for DynamicFieldDecoder<'a> {
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        if self.bitset & self.required != self.required {
            return Err(&DecodeError {
                message: "Missing required fields",
            });
        }
        //todo fill defaults
        Ok(())
    }
    unsafe fn destroy(&mut self) {
        for (i, field) in self.schema.fields().iter().enumerate() {
            if self.bitset & (1 << i) != 0 {
                (self.schema.inner.drops[i])(self.destination.byte_add(field.offset));
            }
        }
    }
    fn visit(
        &mut self,
        borrowed: ParserWithBorrowedKey<'a, '_>,
    ) -> Result<(), &'static DecodeError> {
        let field_name = borrowed.key();
        for (i, field) in self.schema.fields().iter().enumerate() {
            if field.name == field_name {
                let parser = borrowed.into_parser();
                let mask = 1 << i;
                if self.bitset & mask != 0 {
                    if let JsonParentContext::None = parser.parent_context {
                        parser.parent_context = JsonParentContext::ObjectKey(field.name);
                    }
                    return Err(&DUPLICATE_FIELD);
                } else {
                    if let Err(err) =
                        unsafe { (field.decode)(self.destination.byte_add(field.offset), parser) }
                    {
                        if let JsonParentContext::None = parser.parent_context {
                            parser.parent_context = JsonParentContext::ObjectKey(field.name);
                        }
                        return Err(err);
                    }
                    self.bitset |= mask;
                }
                return Ok(());
            }
        }
        Ok(())
    }
}

use crate::{
    json::{FieldVistor, ParserWithBorrowedKey},
    parser::{JsonParentContext, Parser, DUPLICATE_FIELD, MISSING_REQUIRED_FIELDS},
};

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
    fn fields(&self) -> &[Field<'a>] {
        unsafe {
            std::slice::from_raw_parts(
                self.inner.fields.as_ptr() as *const Field<'a>,
                self.inner.fields.len(),
            )
        }
    }
}

impl<'a> ObjectSchema<'a> {
    pub unsafe fn decode(
        &self,
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
        mut unused: Option<&mut dyn FieldVistor<'a>>,
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
                                //todo porpotogate error
                                if let Err(err) =
                                    (field.decode)(dest.byte_add(field.offset), parser)
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
                                // Safety: Safe since the `key` was provided by the same parses and still
                                // valid here (since it compiles and wasn't casted to a pointer earlier).
                                let borrowed = unsafe { ParserWithBorrowedKey::new(key, parser) };
                                if let Err(err) = unused_processor.visit(borrowed) {
                                    break 'with_next_key err;
                                }
                            } else if let Err(error) = parser.at.skip_value() {
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
                    emplace_default(dest.byte_add(field.offset));
                }
            }
            return Ok(());
        };

        for (i, (drop, field)) in self.inner.drops.iter().zip(self.fields()).enumerate() {
            if bitset & (1 << i) != 0 {
                drop(dest.byte_add(field.offset));
            }
        }
        if let Some(vistor) = unused {
            vistor.destroy()
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
    F: 'static + Fn(P) -> Result<T, &'static DecodeError>,
{
    const { assert!(std::mem::size_of::<F>() == 0) }
    let func: unsafe fn(dest: NonNull<()>, parser: P) -> Result<(), &'static DecodeError> = unsafe {
        |dest: NonNull<()>, parser: P| -> Result<(), &'static DecodeError> {
            // safety: from above assert F is ZST.
            let func = std::mem::transmute_copy::<(), F>(&());
            match func(parser) {
                Ok(value) => {
                    let value: T = value;
                    dest.cast::<T>().write(value);
                    Ok(())
                }
                Err(err) => Err(err),
            }
        }
    };
    unsafe { std::mem::transmute(func) }
}
