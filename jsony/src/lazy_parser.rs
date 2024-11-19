//! Lazy JSON parsing that allows access keys of objects and arrays
//! without parsing the entire string or intermediate objects.
//!
//! Ideal for when you have deeply nested json but only care about some
//! inner part. Further, all index access implement null coalescing meaning
//! when an
//!
//! Example:
//! ```ignore
//! let json = Value::new(r#"{"a": {"b": [{"c": "contents"}]}}"#);
//! let contents: String = json["a"]["b"][0]["c"].parse().unwrap();
//! assert_eq!(json[&"a"][&"name_of_missing_key"][0]["c"].key_error(), Some("name_of_missing_key"));
//! ```

use crate::{json::DecodeError, parser::InnerParser, text::Ctx, MaybeJson};

use crate::{from_json_with_config, FromJson, JsonError, JsonParserConfig};

static OBJECT_INDEX_ERROR: DecodeError = DecodeError {
    message: "Object Key Index Error",
};

static ARRAY_INDEX_ERROR_0: DecodeError = DecodeError {
    message: "Array Index of 0 is out of range",
};
static ARRAY_INDEX_ERROR_1: DecodeError = DecodeError {
    message: "Array Index of 1 is out of range",
};
static ARRAY_INDEX_ERROR_2: DecodeError = DecodeError {
    message: "Array Index of 2 is out of range",
};
static ARRAY_INDEX_ERROR_N: DecodeError = DecodeError {
    message: "Array Index out of range",
};

impl std::ops::Index<&'static &'static str> for MaybeJson {
    type Output = MaybeJson;

    /// If the current value is an object, get the value of the corresponding key
    fn index(&self, key: &'static &'static str) -> &Self::Output {
        if self.is_error() {
            return self;
        }
        let mut parser = InnerParser {
            index: 0,
            ctx: Ctx::new(&self.raw),
            config: Default::default(),
        };
        match parser.index_object(key) {
            Ok(true) => MaybeJson::new(unsafe {
                std::str::from_utf8_unchecked(&parser.ctx.data[parser.index..])
            }),
            Ok(false) => MaybeJson::from_object_index_error(key),
            Err(err) => MaybeJson::from_decode_error(err),
        }
    }
}

impl std::ops::Index<&str> for MaybeJson {
    type Output = MaybeJson;

    /// If the current value is an object, get the value of the corresponding key
    fn index(&self, key: &str) -> &Self::Output {
        if self.is_error() {
            return self;
        }
        let mut parser = InnerParser {
            index: 0,
            ctx: Ctx::new(&self.raw),
            config: Default::default(),
        };
        match parser.index_object(key) {
            Ok(true) => MaybeJson::new(unsafe {
                std::str::from_utf8_unchecked(&parser.ctx.data[parser.index..])
            }),
            Ok(false) => MaybeJson::from_decode_error(&OBJECT_INDEX_ERROR),
            Err(err) => MaybeJson::from_decode_error(err),
        }
    }
}

impl std::ops::Index<usize> for MaybeJson {
    type Output = MaybeJson;

    /// If the current value is an array, get the value of the corresponding index
    fn index(&self, index: usize) -> &Self::Output {
        if self.is_error() {
            return self;
        }
        let mut parser = InnerParser {
            index: 0,
            ctx: Ctx::new(&self.raw),
            config: Default::default(),
        };
        match parser.index_array(index) {
            Ok(true) => MaybeJson::new(unsafe {
                std::str::from_utf8_unchecked(&parser.ctx.data[parser.index..])
            }),
            Ok(false) => MaybeJson::from_decode_error(match index {
                0 => &ARRAY_INDEX_ERROR_0,
                1 => &ARRAY_INDEX_ERROR_1,
                2 => &ARRAY_INDEX_ERROR_2,
                _ => &ARRAY_INDEX_ERROR_N,
            }),
            Err(err) => MaybeJson::from_decode_error(err),
        }
    }
}

impl std::fmt::Debug for MaybeJson {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(error) = self.decode_error() {
            let mut builder = f.debug_struct("MaybeJson");
            builder.field("error", error);
            if let Some(key) = self.key_error() {
                builder.field("key", &key);
            }
            builder.finish()
        } else {
            f.debug_struct("MaybeJson")
                .field("raw", &&self.raw)
                .finish()
        }
    }
}

impl MaybeJson {
    pub fn from_object_index_error(key: &'static &'static str) -> &'static MaybeJson {
        let ptr = key as *const &'static str as *const u8;
        // todo switch to strict provenece once it lands
        let ptr = (ptr as usize | 1) as *const u8;
        unsafe { &*(core::ptr::slice_from_raw_parts(ptr, 0) as *const MaybeJson) }
    }
    pub const fn from_decode_error(decode_error: &'static DecodeError) -> &'static MaybeJson {
        unsafe {
            &*(core::ptr::slice_from_raw_parts(decode_error as *const _ as *const u8, 0)
                as *const MaybeJson)
        }
    }
    pub fn parse<'a, T: FromJson<'a>>(&'a self) -> Result<T, JsonError> {
        if let Some(error) = self.json_error() {
            return Err(error);
        }
        from_json_with_config::<'a, T>(
            &self.raw,
            JsonParserConfig {
                recursion_limit: 128,
                allow_trailing_commas: false,
                allow_comments: false,
                allow_unquoted_field_keys: false,
                allow_trailing_data: true,
            },
        )
    }

    pub fn json_error(&self) -> Option<JsonError> {
        let Some(error) = self.decode_error() else {
            return None;
        };
        let mut context: Option<String> = None;
        if error == &OBJECT_INDEX_ERROR {
            if let Some(key) = self.key_error() {
                context = Some(format!("Object key not found: {key:?}"));
            }
        }
        Some(JsonError::new(error, context))
    }

    pub fn decode_error(&self) -> Option<&'static DecodeError> {
        if !self.is_error() {
            return None;
        }
        let tagged = self.raw.as_ptr() as usize;
        if tagged & 0b1 == 1 {
            Some(&DecodeError {
                message: "Object Key Index Error",
            })
        } else {
            Some(unsafe { &*(self.raw.as_ptr() as *const DecodeError) })
        }
    }

    /// If the value is currently a MissingObjectKey get the key that causes
    /// error is possible.
    pub fn key_error(&self) -> Option<&'static str> {
        if !self.is_error() {
            return None;
        }
        let tagged = self.raw.as_ptr() as usize;
        if tagged & 0b1 == 1 {
            let ptr = (tagged & !0b1) as *const u8;
            Some(unsafe { *(ptr as *const &'static str) })
        } else {
            None
        }
    }

    pub fn is_error(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn null() -> &'static MaybeJson {
        MaybeJson::new("")
    }

    pub fn new(raw: &str) -> &MaybeJson {
        if raw.is_empty() {
            MaybeJson::from_decode_error(&DecodeError {
                message: "Empty object is not valid JSON",
            })
        } else {
            unsafe { &*(raw as *const str as *const MaybeJson) }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn keys_with_escapes() {
        let input = r#"{"v\r\f\n\t\"\\": 23, "\u0031\u0032\u0033": 41}"#;
        let json = MaybeJson::new(input);
        assert_eq!(23u32, json["v\r\x0C\n\t\"\\"].parse().unwrap());
        assert_eq!(41u32, json["123"].parse().unwrap());
    }

    #[test]
    fn arrays() {
        let value = MaybeJson::new(stringify!([
            {
                "inner": [42, "nice"]
            },
            "wow",
            false,
            true,
            -123.456,
            null
        ]));
        assert_eq!(42u64, value[0]["inner"][0].parse::<u64>().unwrap());
        assert_eq!("nice", value[0]["inner"][1].parse::<String>().unwrap());
        assert_eq!("wow", value[1].parse::<String>().unwrap());
        assert!(!value[2].parse::<bool>().unwrap());
        assert!(value[3].parse::<bool>().unwrap());
        assert_eq!(-123.456, value[4].parse::<f64>().unwrap());
        assert_eq!(None::<u32>, value[5].parse::<Option<u32>>().unwrap());
    }

    #[test]
    fn errors() {
        let value = MaybeJson::new(stringify!({
            "hello": 124,
            "nice": {"inner": [1,2,3]}
        }));
        assert_eq!(value["nice"]["inner"][0].parse::<u64>().unwrap(), 1);
        assert!(MaybeJson::new("[]")[0].decode_error().is_some());
        assert!(value["nice"]["inner"][5].decode_error().is_some());

        assert_eq!(value[&"nice"][&"not_inner"].key_error(), Some("not_inner"));

        assert!(value["nice"]["not_inner"].decode_error().is_some());

        assert!(value[0].decode_error().is_some());

        assert!(value["nice"]["inner"]["key_for_array"]
            .decode_error()
            .is_some());
    }
}
