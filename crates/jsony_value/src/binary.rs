use jsony::{FromBinary, Jsony, ToBinary};

use crate::{Value, ValueList, ValueMap, ValueString};

// Probably could combine length and type often
#[derive(Jsony)]
#[jsony(FromBinary)]
enum TypeTag {
    Null = 0,
    True = 1,
    False = 2,
    F64 = 3,
    U64 = 4,
    I64 = 5,
    String = 6,
    Other = 7,
    Map = 8,
    List = 9,
}

unsafe impl<'a> ToBinary for ValueString<'a> {
    fn encode_binary(&self, encoder: &mut jsony::BytesWriter) {
        self.as_str().encode_binary(encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for ValueString<'a> {
    fn decode_binary(decoder: &mut jsony::binary::Decoder<'a>) -> Self {
        ValueString::from_borrowed(<&'a str>::decode_binary(decoder))
    }
}

unsafe impl<'a> ToBinary for ValueList<'a> {
    fn encode_binary(&self, encoder: &mut jsony::BytesWriter) {
        self.as_slice().encode_binary(encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for ValueList<'a> {
    fn decode_binary(decoder: &mut jsony::binary::Decoder<'a>) -> Self {
        // todo this avoid this allocation
        let vec: Vec<Value<'a>> = FromBinary::decode_binary(decoder);
        ValueList::from_iter(vec)
    }
}

unsafe impl<'a> ToBinary for ValueMap<'a> {
    fn encode_binary(&self, encoder: &mut jsony::BytesWriter) {
        self.entries().encode_binary(encoder);
    }
}
unsafe impl<'a> FromBinary<'a> for ValueMap<'a> {
    fn decode_binary(decoder: &mut jsony::binary::Decoder<'a>) -> Self {
        // todo this avoid this allocation
        let vec: Vec<(ValueString<'a>, Value<'a>)> = FromBinary::decode_binary(decoder);
        ValueMap::from_iter(vec)
    }
}

unsafe impl<'a> FromBinary<'a> for Value<'a> {
    fn decode_binary(decoder: &mut jsony::binary::Decoder<'a>) -> Self {
        match TypeTag::decode_binary(decoder) {
            TypeTag::Null => Value::NULL,
            TypeTag::U64 => Value::from(u64::decode_binary(decoder)),
            TypeTag::I64 => Value::from(i64::decode_binary(decoder)),
            TypeTag::F64 => Value::from(f64::decode_binary(decoder)),
            TypeTag::String => Value::from(ValueString::decode_binary(decoder)),
            TypeTag::Other => Value::from(ValueString::other_borrowed(<&'a str>::decode_binary(
                decoder,
            ))),
            TypeTag::Map => Value::from(ValueMap::decode_binary(decoder)),
            TypeTag::List => Value::from(ValueList::decode_binary(decoder)),
            TypeTag::True => Value::from(true),
            TypeTag::False => Value::from(false),
        }
    }
}

unsafe impl<'a> ToBinary for Value<'a> {
    fn encode_binary(&self, encoder: &mut jsony::BytesWriter) {
        match self.as_ref() {
            crate::ValueRef::Null(_) => encoder.push(TypeTag::Null as u8),
            crate::ValueRef::Number(value_number) => match value_number {
                crate::ValueNumber::U64(val) => {
                    encoder.push(TypeTag::U64 as u8);
                    val.encode_binary(encoder);
                }
                crate::ValueNumber::I64(val) => {
                    encoder.push(TypeTag::I64 as u8);
                    val.encode_binary(encoder);
                }
                crate::ValueNumber::F64(val) => {
                    encoder.push(TypeTag::F64 as u8);
                    val.encode_binary(encoder);
                }
            },
            crate::ValueRef::String(value_string) => {
                encoder.push(TypeTag::String as u8);
                value_string.encode_binary(encoder);
            }
            crate::ValueRef::Other(value_string) => {
                encoder.push(TypeTag::Other as u8);
                value_string.encode_binary(encoder);
            }
            crate::ValueRef::Map(value_map) => {
                encoder.push(TypeTag::Map as u8);
                value_map.encode_binary(encoder);
            }
            crate::ValueRef::List(value_list) => {
                encoder.push(TypeTag::List as u8);
                value_list.as_slice().encode_binary(encoder);
            }
            crate::ValueRef::Boolean(value_boolean) => {
                if *value_boolean == true {
                    encoder.push(TypeTag::True as u8)
                } else {
                    encoder.push(TypeTag::False as u8)
                }
            }
        }
    }
}
