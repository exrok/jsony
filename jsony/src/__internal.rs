use std::{marker::PhantomData, ptr::NonNull};
struct NestedDynamicFieldDecoder<'a, T: FieldVistor<'a>> {
    inner: T,
    destination: NonNull<()>,
    schema: ObjectSchema<'a>,
    bitset: u64,
    required: u64,
}

struct DynamicFieldDecoder<'a> {
    destination: NonNull<()>,
    schema: ObjectSchema<'a>,
    bitset: u64,
    required: u64,
}

pub const unsafe fn erase<'a>(
    input: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
) -> for<'b> unsafe fn(NonNull<()>, &mut Parser<'b>) -> Result<(), &'static DecodeError> {
    unsafe { std::mem::transmute(input) }
}

impl<'a> FieldVistor<'a> for DynamicFieldDecoder<'a> {
    fn complete(&mut self) -> Result<(), &'static DecodeError> {
        todo!()
    }
    // called on Err
    unsafe fn destroy(&mut self) {
        vec!["asdfasd".to_string()].join(",");
        todo!()
    }
    fn visit(
        &mut self,
        field_name: &str,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        for (i, field) in self.schema.fields().iter().enumerate() {
            if field.name == field_name {
                let mask = 1 << i;
                if self.bitset & mask != 0 {
                    return Err(&DecodeError {
                        message: "Duplicate field",
                    });
                } else {
                    if let Err(err) =
                        unsafe { (field.decode)(self.destination.byte_add(field.offset), parser) }
                    {
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

use crate::{json::FieldVistor, parser::Parser};

type DecodeFn<'a> = unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>;
use super::DecodeError;

// maximum number fields.
// currently 63 based on how we compute the bit masks.
const MAX_FIELDS: usize = 63;
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
    pub defaults: &'static [unsafe fn(NonNull<()>)],
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
        // let defaults = (1u64 << self.inner.defaults.len()) - 1;
        let all = ((1u64 << self.inner.fields.len()) - 1);
        let mut bitset = 0;
        let mut key = match parser.enter_object() {
            Ok(Some(key)) => key,
            Ok(None) => {
                //todo should actually check which fiedls where reuured
                return Err(&DecodeError {
                    message: "Missing All fields",
                });
            }
            Err(err) => return Err(err),
        };

        let error = 'with_next_key: loop {
            'next: {
                for (index, field) in self.fields().iter().enumerate() {
                    let mask = 1 << index;
                    if field.name != key {
                        continue;
                    }
                    if bitset & mask != 0 {
                        break 'with_next_key &DecodeError {
                            message: "Duplicate field",
                        };
                    }
                    //todo porpotogate error
                    if let Err(err) = (field.decode)(dest.byte_add(field.offset), parser) {
                        break 'with_next_key err;
                    }
                    bitset |= mask;
                    if bitset & all == all {
                        // todo handle error opimtize
                        while let Ok(Some(_)) = parser.object_step() {
                            if let Err(err) = parser.skip_value() {
                                break 'with_next_key err;
                            }
                        }
                        return Ok(());
                    }
                    break 'next;
                }
                if let Some(ref mut unsued_processor) = unsued {
                    if let Err(err) = unsued_processor.visit(key, parser) {
                        break 'with_next_key err;
                    }
                } else {
                    if let Err(error) = parser.skip_value() {
                        break 'with_next_key error;
                    }
                }
            }

            match parser.object_step() {
                Ok(Some(next_key2)) => {
                    key = next_key2;
                    continue 'with_next_key;
                }
                Ok(None) => {
                    let default = (1u64 << self.inner.defaults.len()) - 1;
                    if (bitset | default) & all != all {
                        break 'with_next_key &DecodeError {
                            message: "Missing required fields",
                        };
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
                }
                Err(err) => break 'with_next_key err,
            }
        };

        for (i, (drop, field)) in self.inner.drops.iter().zip(self.fields()).enumerate() {
            if bitset & (1 << i) != 0 {
                drop(dest.byte_add(field.offset));
            }
        }
        return Err(error);
    }
}

#[inline(never)]
unsafe fn decode_object<'a, 'b>(
    dest: NonNull<()>,
    parser: &'b mut Parser<'a>,
    target: u64,
    fields: &[(
        &'static str,
        u64,
        usize,
        unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
    )],
    drops: &[(unsafe fn(NonNull<()>), usize)],
    mut unsued: Option<&mut dyn FieldVistor<'a>>,
) -> Result<(), &'static DecodeError> {
    let mut bitset = 0u64;
    let mut key = match parser.enter_object() {
        Ok(Some(key)) => key,
        Ok(None) => {
            //todo should actually check which fiedls where reuured
            return Err(&DecodeError {
                message: "Missing All fields",
            });
        }
        Err(err) => return Err(err),
    };
    'with_next_key: loop {
        for (field_key, mask, offset, func) in fields {
            if key == *field_key {
                if bitset & mask != 0 {
                    break 'with_next_key;
                } else {
                    //todo porpotogate error
                    if func(dest.byte_add(*offset), parser).is_err() {
                        break 'with_next_key;
                    }
                    bitset |= mask;
                }
                if bitset & target == target {
                    // todo can opimtize
                    while let Ok(Some(_)) = parser.object_step() {
                        if parser.skip_value().is_err() {
                            break;
                        }
                    }
                    return Ok(());
                }
                if let Ok(Some(next_key2)) = parser.object_step() {
                    key = next_key2;
                    continue 'with_next_key;
                } else {
                    break 'with_next_key;
                }
            }
        }
        if let Some(ref mut unsued_processor) = unsued {
            if let Err(err) = unsued_processor.visit(key, parser) {
                return Err(err);
            }
        }
    }
    for (i, (drop, offset)) in drops.iter().enumerate() {
        if bitset & (1 << i) != 0 {
            drop(dest.byte_add(*offset));
        }
    }
    return Err(&DecodeError { message: "TODO" });
}
