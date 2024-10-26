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
    unsafe fn visit(
        &mut self,
        field_name: *const str,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        let field_name = unsafe { parser.unfreeze(field_name) };
        for (i, field) in self.schema.fields().iter().enumerate() {
            if field.name == field_name {
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
    json::FieldVistor,
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
        mut unsued: Option<&mut dyn FieldVistor<'a>>,
    ) -> Result<(), &'static DecodeError> {
        let all = (1u64 << self.inner.fields.len()) - 1;
        let mut bitset = 0;

        let error = 'with_next_key: {
            match parser.enter_object() {
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
                            if let Some(ref mut unsued_processor) = unsued {
                                if let Err(err) = unsued_processor.visit(key, parser) {
                                    break 'with_next_key err;
                                }
                            } else if let Err(error) = parser.skip_value() {
                                break 'with_next_key error;
                            }
                        }

                        match parser.object_step() {
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
            if let Some(visitor) = &mut unsued {
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
        if let Some(vistor) = unsued {
            vistor.destroy()
        }
        Err(error)
    }
}
