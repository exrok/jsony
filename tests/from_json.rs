use std::borrow::Cow;

use jsony::{JsonParserConfig, Jsony, from_json, parser::TRAILING_COMMA};

macro_rules! obj {
    ($($tt:tt)*) => {
        stringify! { { $($tt)* } }
    };
}

macro_rules! arr {
    ($($tt:tt)*) => {
        stringify! { [ $($tt)* ] }
    };
}

#[test]
fn simple_struct() {
    #[derive(Jsony)]
    pub struct Simple {
        a1: i32,
        a2: bool,
    }
    let x = from_json::<Simple>(obj! { "a1": 23,  "a2": true }).unwrap();
    assert_eq!(x.a1, 23);
    assert!(x.a2);
    let x = from_json::<Simple>(obj! { "a2": true, "a1": 23 }).unwrap();
    assert_eq!(x.a1, 23);
    assert!(x.a2);
    assert!(from_json::<Simple>(obj! { "a1": 23, }).is_err());
    assert!(from_json::<Simple>(obj! { "a1": 23, "a2": 1 }).is_err());
    assert!(from_json::<Simple>("{").is_err());
    assert!(from_json::<Simple>("}").is_err());
    assert!(from_json::<Simple>("{...}").is_err());

    #[derive(Jsony)]
    pub struct Nested {
        x: Simple,
        y: Simple,
    }

    let nested = from_json::<Nested>(obj! {
        "y": {
            "a1": 22,
            "a2": false
        },
        "x": {
            "a2": true,
            "a1": -4353
        }
    })
    .unwrap();
    assert_eq!(nested.x.a1, -4353);
    assert!(nested.x.a2);
    assert_eq!(nested.y.a1, 22);
    assert!(!nested.y.a2);
}

#[test]
fn tuple_struct() {
    #[derive(Jsony, Debug, PartialEq)]
    pub struct Triple(i32, String, bool);

    assert_eq!(
        from_json::<Triple>(arr![-7, "hi", true]).unwrap(),
        Triple(-7, "hi".to_string(), true)
    );
    // Length mismatches and non-array inputs are rejected.
    assert!(from_json::<Triple>(arr![1, "x"]).is_err());
    assert!(from_json::<Triple>(arr![1, "x", true, 5]).is_err());
    assert!(from_json::<Triple>("5").is_err());

    #[derive(Jsony, Debug, PartialEq)]
    pub struct Pair(u8, u8);
    assert_eq!(from_json::<Pair>(arr![1, 2]).unwrap(), Pair(1, 2));
}

#[test]
fn defaults() {
    #[derive(Jsony, Default)]
    struct Simple {
        #[jsony(default = 203)]
        a1: i32,
        #[jsony(default = false)]
        a2: bool,
    }
    let x = from_json::<Simple>(obj! { "a1": 23,  "a2": true }).unwrap();
    assert_eq!(x.a1, 23);
    assert!(x.a2);
    let x = from_json::<Simple>(obj! {}).unwrap();
    assert_eq!(x.a1, 203);
    assert!(!x.a2);

    #[derive(Jsony)]
    struct Nested {
        x: Simple,
        #[jsony(default)]
        y: Simple,
    }

    let nested = from_json::<Nested>(obj! {
        "y": { },
        "x": { "a2": true}
    })
    .unwrap();
    assert_eq!(nested.x.a1, 203);
    assert!(nested.x.a2);
    assert_eq!(nested.y.a1, 203);
    assert!(!nested.y.a2);

    let nested = from_json::<Nested>(obj! {
        "x": { "a2": true}
    })
    .unwrap();
    assert_eq!(nested.x.a1, 203);
    assert!(nested.x.a2);
    assert_eq!(nested.y.a1, 0);
    assert!(!nested.y.a2);
}

#[test]
fn enum_default_tagging() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    enum Simple {
        Alpha(u32),
        Beta { a: String, b: Vec<u8> },
        Delta,
        Zeta,
    }
    let x = from_json::<Vec<Simple>>(arr! [
        { "Alpha": 23 },
        "Zeta",
        { "Beta": { "a": "test", "b": [1, 2, 3] } },
        "Delta"
    ])
    .unwrap();
    assert_eq!(
        &x,
        &[
            Simple::Alpha(23),
            Simple::Zeta,
            Simple::Beta {
                a: "test".to_string(),
                b: vec![1, 2, 3],
            },
            Simple::Delta,
        ]
    )
}

#[test]
fn flattening_in_enum() {
    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(FromJson, tag = "kind")]
    enum Status<'a> {
        Online,
        Error {
            #[jsony(default = i64::MAX)]
            code: i64,
            message: Cow<'a, str>,
            #[jsony(flatten)]
            properties: Vec<(String, bool)>,
        },
        Offline,
    }

    let static_text: String = jsony::object! {
        kind: "Error",
        message: "System Failure",
        data: true,
        value: false
    };
    let status: Status = jsony::from_json(&static_text).unwrap();
    assert_eq!(
        status,
        Status::Error {
            code: i64::MAX,
            message: "System Failure".into(),
            properties: vec![("data".to_string(), true), ("value".to_string(), false),],
        }
    );
}

#[test]
fn recursion_limits() {
    #[derive(Jsony, Debug)]
    struct NestedObject {
        a: Option<Box<NestedObject>>,
    }
    fn recurse(depth: usize, prefix: &str, terminal: &str, suffix: &str) -> String {
        let mut out = String::with_capacity(depth * (prefix.len() + suffix.len()) + terminal.len());
        (0..depth).for_each(|_| out.push_str(prefix));
        out.push_str(terminal);
        (0..depth).for_each(|_| out.push_str(suffix));
        println!("{}", out);
        out
    }
    from_json::<NestedObject>(&recurse(128, "{\"a\":", "null", "}")).unwrap();
    let err = from_json::<NestedObject>(&recurse(129, "{\"a\":", "null", "}"))
        .err()
        .unwrap();
    assert!(err.to_string().contains("limit"));

    #[derive(Jsony, Debug)]
    struct NestedArray(Vec<NestedArray>);

    from_json::<NestedArray>(&recurse(128, "[", "[]", "]")).unwrap();
    let err = from_json::<NestedArray>(&recurse(129, "[", "[]", "]"))
        .err()
        .unwrap();
    assert!(err.to_string().contains("limit"));

    #[derive(Jsony)]
    #[jsony(untagged)]
    #[allow(unused)]
    enum Untagged {
        Array(Vec<Untagged>),
        // If untagged to doesn't make sure to restore remaining limit after
        // failing a match on this `FakeObject` then we'll fail.
        FakeObject { key: bool },
        Object { key: Option<Box<Untagged>> },
        Number(f64),
    }

    let foo = recurse(63, "{\"key\": [", "0", "]}");
    from_json::<Vec<Untagged>>(&format!("[{foo},{foo},{foo}]")).unwrap();
}

#[test]
fn escapes() {
    #[derive(Jsony, Debug, PartialEq, Eq)]
    struct Object<'a> {
        key: Cow<'a, str>,
    }
    fn obj(key: &str) -> Object<'_> {
        Object { key: key.into() }
    }
    assert_eq!(
        from_json::<Object>(r#"{ "ke\u0079": "" }"#).unwrap(),
        obj(""),
        "Should decoded needless escaped text"
    );
    assert!(
        from_json::<String>("\"AA4\\uDCACA4\\\"D\\\"\"\r\r").is_err(),
        "lone trailing surrogate escape should be rejected"
    );
}

#[test]
fn trailing_commas() {
    assert_eq!(
        jsony::from_json::<Vec<bool>>(arr![true, false,])
            .err()
            .unwrap()
            .decoding_error(),
        &TRAILING_COMMA
    );
    assert_eq!(
        jsony::from_json_with_config::<Vec<bool>>(
            arr![true, false,],
            JsonParserConfig {
                allow_trailing_commas: true,
                ..Default::default()
            }
        )
        .unwrap(),
        vec![true, false]
    );

    #[derive(Jsony, Debug, PartialEq, Eq)]
    struct Example<'a> {
        value: &'a str,
    }

    assert_eq!(
        jsony::from_json::<Example>(obj! { "value": "hello", })
            .err()
            .unwrap()
            .decoding_error(),
        &TRAILING_COMMA
    );
    assert_eq!(
        jsony::from_json_with_config::<Example>(
            obj! { "value": "hello", },
            JsonParserConfig {
                allow_trailing_commas: true,
                ..Default::default()
            }
        )
        .unwrap(),
        Example { value: "hello" }
    );
}

#[cfg(feature = "json_comments")]
#[test]
fn comments() {
    let input_1 = &"[
            /* hello
                -foo
                - nice
                asdfasdfa
            */
            true, // some text here
            false // some more comments
            ,true
            ]
            // trailing comment
            ";
    assert!(jsony::from_json::<Vec<bool>>(input_1).is_err());
    assert_eq!(
        jsony::from_json_with_config::<Vec<bool>>(
            input_1,
            JsonParserConfig {
                allow_comments: true,
                ..Default::default()
            }
        )
        .unwrap(),
        vec![true, false, true]
    );
}

#[test]
fn unquoted_keys() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    struct UnquotedStruct<'a> {
        kind: &'a str,
        data: bool,
    }
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(tag = "kind")]
    enum UnquotedEnum {
        Value { data: bool },
    }
    let input_1 = &r#"
        {
            kind: "Value",
            data: true
        }
    "#;
    assert!(jsony::from_json::<UnquotedStruct>(input_1).is_err());
    assert!(jsony::from_json::<UnquotedEnum>(input_1).is_err());
    assert_eq!(
        jsony::from_json_with_config::<UnquotedEnum>(
            input_1,
            JsonParserConfig {
                allow_unquoted_field_keys: true,
                ..Default::default()
            }
        )
        .unwrap(),
        UnquotedEnum::Value { data: true }
    );
    assert_eq!(
        jsony::from_json_with_config::<UnquotedStruct>(
            input_1,
            JsonParserConfig {
                allow_unquoted_field_keys: true,
                ..Default::default()
            }
        )
        .unwrap(),
        UnquotedStruct {
            kind: "Value",
            data: true
        }
    );
}

#[test]
fn from_vec() {
    let input = jsony::array![
        for value in 0..100;
        format!("0x{:08x}", value)
    ];
    let vec = jsony::from_json::<Vec<String>>(&input).unwrap();
    let _ = vec.clone();
}

#[test]
fn from_vec_zst() {
    let input = jsony::array![ for _ in 0..100; [] ];
    let vec = jsony::from_json::<Vec<[u8; 0]>>(&input).unwrap();
    assert_eq!(vec.len(), 100);
    let mut count = 0;
    for item in &vec {
        count += item.len();
    }
    assert_eq!(count, 0);
    let _ = vec.clone();
}

#[test]
fn to_from_str() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(ToStr, FromStr)]
    enum SimpleEnum {
        Alpha,
        Beta,
        #[jsony(rename = "Charlie")]
        Canary,
    }
    assert_eq!(SimpleEnum::Alpha.to_str(), "Alpha");
    let value = vec![SimpleEnum::Alpha];
    let _test_is_static: &'static str = (&value[0]).to_str();

    assert_eq!(SimpleEnum::Beta.to_str(), "Beta");
    assert_eq!(SimpleEnum::Canary.to_str(), "Charlie");

    assert_eq!(SimpleEnum::Alpha, "Alpha".parse().expect("Alpha"));
    assert_eq!(SimpleEnum::Beta, "Beta".parse().expect("Beta"));
    assert_eq!(SimpleEnum::Canary, "Charlie".parse().expect("Charlie"));
    assert!(
        "Canary".parse::<SimpleEnum>().is_err(),
        "Canary was renamed to Charlie"
    );
}

#[test]
fn tag_content_enum_with_attribute_whitespace() {
    mod parsed_string {
        use jsony::json::DecodeError;
        use std::{fmt::Display, str::FromStr};
        pub fn decode_json<T: FromStr>(
            parser: &mut jsony::parser::Parser<'_>,
        ) -> Result<T, &'static DecodeError>
        where
            T::Err: Display,
        {
            match T::from_str(parser.take_string()?) {
                Ok(value) => Ok(value),
                Err(err) => {
                    parser.report_error(format!("FromStr failed: {err}"));
                    Err(&DecodeError {
                        message: "FromStr failed",
                    })
                }
            }
        }
    }

    #[derive(Jsony, Debug, PartialEq, Eq)]
    #[jsony(tag = "type", content = "value", rename_all = "kebab-case")]
    enum Scalar {
        Integer(#[jsony(with = parsed_string)] i64),
    }

    // No whitespace
    let no_ws = r#"{"type":"integer","value":"42"}"#;
    assert_eq!(from_json::<Scalar>(no_ws).unwrap(), Scalar::Integer(42));

    // Whitespace after colons (the bug: parser didn't skip whitespace before
    // reading tag value string in tag/content enums)
    let with_ws = r#"{"type": "integer", "value": "42"}"#;
    assert_eq!(from_json::<Scalar>(with_ws).unwrap(), Scalar::Integer(42));

    // Content before tag (different code path in tag_query_at_content_next_object)
    let content_first = r#"{"value": "42", "type": "integer"}"#;
    assert_eq!(
        from_json::<Scalar>(content_first).unwrap(),
        Scalar::Integer(42)
    );
}

#[test]
fn to_from_str_with_json() {
    // Json may utilize the ToStr/FromStr impls we test here that also
    // adding json doesn't break either
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(ToStr, FromStr, Json)]
    enum SimpleEnum {
        Alpha,
        Beta,
        #[jsony(rename = "Charlie")]
        Canary,
    }
    assert_eq!(SimpleEnum::Alpha.to_str(), "Alpha");
    let value = vec![SimpleEnum::Alpha];
    let _test_is_static: &'static str = (&value[0]).to_str();

    assert_eq!(SimpleEnum::Beta.to_str(), "Beta");
    assert_eq!(SimpleEnum::Canary.to_str(), "Charlie");

    assert_eq!(SimpleEnum::Alpha, "Alpha".parse().expect("Alpha"));
    assert_eq!(SimpleEnum::Beta, "Beta".parse().expect("Beta"));
    assert_eq!(SimpleEnum::Canary, "Charlie".parse().expect("Charlie"));
    assert!(
        "Canary".parse::<SimpleEnum>().is_err(),
        "Canary was renamed to Charlie"
    );
    assert_eq!(jsony::to_json(&SimpleEnum::Alpha), "\"Alpha\"");
    assert_eq!(jsony::to_json(&SimpleEnum::Canary), "\"Charlie\"");

    assert_eq!(
        jsony::from_json::<SimpleEnum>("\"Alpha\"").unwrap(),
        SimpleEnum::Alpha
    );
    assert_eq!(
        jsony::from_json::<SimpleEnum>("\"Charlie\"").unwrap(),
        SimpleEnum::Canary
    );
}

#[test]
fn generic_enum_struct_variant() {
    // Regression: a generic enum with a struct variant used to emit a nested
    // `type __TEMP = (T,)` alias that referenced `T` from the outer impl (E0401)
    // and failed to compile. The alias now re-declares the target's generics.
    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json)]
    enum E<T> {
        A { x: T },
        B,
    }

    for original in [E::A { x: 7i32 }, E::B] {
        let s = jsony::to_json(&original);
        let back: E<i32> = jsony::from_json(&s).unwrap();
        assert_eq!(original, back);
    }

    // Two type parameters across distinct struct-variant fields.
    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json)]
    enum Pair<T, U> {
        Both { a: T, b: U },
        Neither,
    }

    let original = Pair::Both {
        a: "hi".to_string(),
        b: vec![1u8, 2, 3],
    };
    let s = jsony::to_json(&original);
    let back: Pair<String, Vec<u8>> = jsony::from_json(&s).unwrap();
    assert_eq!(original, back);
}

#[test]
fn wide_struct_uses_indexed_decode() {
    macro_rules! define_wide_required {
        ($($field:ident: $value:expr),+ $(,)?) => {
            #[allow(dead_code)]
            #[derive(Jsony, Debug, PartialEq)]
            struct WideRequired {
                $($field: u16,)+
            }

            impl WideRequired {
                fn expected() -> Self {
                    Self {
                        $($field: $value,)+
                    }
                }
            }

            #[allow(dead_code)]
            #[derive(Jsony, Debug, PartialEq)]
            #[jsony(tag = "kind")]
            enum WideEnum {
                Variant {
                    $($field: u16,)+
                }
            }

            impl WideEnum {
                fn expected() -> Self {
                    Self::Variant {
                        $($field: $value,)+
                    }
                }
            }
        };
    }

    define_wide_required! {
        f00: 0, f01: 1, f02: 2, f03: 3, f04: 4, f05: 5, f06: 6, f07: 7, f08: 8, f09: 9,
        f10: 10, f11: 11, f12: 12, f13: 13, f14: 14, f15: 15, f16: 16, f17: 17, f18: 18, f19: 19,
        f20: 20, f21: 21, f22: 22, f23: 23, f24: 24, f25: 25, f26: 26, f27: 27, f28: 28, f29: 29,
        f30: 30, f31: 31, f32: 32, f33: 33, f34: 34, f35: 35, f36: 36, f37: 37, f38: 38, f39: 39,
        f40: 40, f41: 41, f42: 42, f43: 43, f44: 44, f45: 45, f46: 46, f47: 47, f48: 48, f49: 49,
        f50: 50, f51: 51, f52: 52, f53: 53, f54: 54, f55: 55, f56: 56, f57: 57, f58: 58, f59: 59,
        f60: 60, f61: 61, f62: 62, f63: 63, f64: 64, f65: 65, f66: 66, f67: 67, f68: 68, f69: 69,
    }

    let mut json = String::from("{");
    for i in (0..70).rev() {
        if i != 69 {
            json.push(',');
        }
        json.push_str(&format!(r#""f{i:02}":{i}"#));
    }
    json.push('}');

    assert_eq!(
        from_json::<WideRequired>(&json).unwrap(),
        WideRequired::expected()
    );

    let mut enum_json = String::from(r#"{"kind":"Variant""#);
    for i in (0..70).rev() {
        enum_json.push(',');
        enum_json.push_str(&format!(r#""f{i:02}":{i}"#));
    }
    enum_json.push('}');
    assert_eq!(
        from_json::<WideEnum>(&enum_json).unwrap(),
        WideEnum::expected()
    );

    let err = from_json::<WideRequired>(r#"{"f69":69}"#).unwrap_err();
    let msg = err.to_string();
    assert_eq!(err.decoding_error(), &jsony::error::MISSING_REQUIRED_FIELDS);
    assert!(msg.contains("\"f00\""), "{msg}");

    let err = from_json::<WideRequired>(r#"{"f69":69,"f69":70}"#).unwrap_err();
    assert_eq!(err.decoding_error(), &jsony::error::DUPLICATE_FIELD);
    assert!(err.to_string().contains("@ key \"f69\""));
}

#[test]
fn wide_struct_defaults_and_aliases_use_indexed_decode() {
    macro_rules! define_wide_default_alias {
        ($($field:ident,)+ @last $last:ident) => {
            #[allow(dead_code)]
            #[derive(Jsony, Debug, Default, PartialEq)]
            struct WideDefaultAlias {
                $(
                    #[jsony(default)]
                    $field: u16,
                )+
                #[jsony(default, alias = "last")]
                $last: u16,
            }
        };
    }

    define_wide_default_alias! {
        f00, f01, f02, f03, f04, f05, f06, f07, f08, f09,
        f10, f11, f12, f13, f14, f15, f16, f17, f18, f19,
        f20, f21, f22, f23, f24, f25, f26, f27, f28, f29,
        f30, f31, f32, f33, f34, f35, f36, f37, f38, f39,
        f40, f41, f42, f43, f44, f45, f46, f47, f48, f49,
        f50, f51, f52, f53, f54, f55, f56, f57, f58, f59,
        f60, f61, f62, f63, f64, f65, f66, f67, f68,
        @last f69
    }

    let value = from_json::<WideDefaultAlias>(r#"{"last":69,"unknown":1}"#).unwrap();
    assert_eq!(value.f00, 0);
    assert_eq!(value.f69, 69);
}

#[test]
fn wide_flattenable_uses_indexed_decode() {
    macro_rules! define_wide_flattenable {
        ($($field:ident: $value:expr,)+ @last $last:ident: $last_value:expr $(,)?) => {
            #[allow(dead_code)]
            #[derive(Jsony, Debug, PartialEq)]
            #[jsony(FromJson, Flattenable)]
            struct WideFlattenable {
                #[jsony(default = 1000)]
                f00: u16,
                $($field: u16,)+
                #[jsony(alias = "last")]
                $last: u16,
            }

            impl WideFlattenable {
                fn expected() -> Self {
                    Self {
                        f00: 1000,
                        $($field: $value,)+
                        $last: $last_value,
                    }
                }
            }

            #[allow(dead_code)]
            #[derive(Jsony, Debug, PartialEq)]
            #[jsony(FromJson)]
            struct WideFlattenWrapper {
                head: u16,
                #[jsony(flatten)]
                inner: WideFlattenable,
                tail: u16,
            }

            impl WideFlattenWrapper {
                fn expected() -> Self {
                    Self {
                        head: 7,
                        inner: WideFlattenable::expected(),
                        tail: 9,
                    }
                }
            }
        };
    }

    define_wide_flattenable! {
        f01: 1, f02: 2, f03: 3, f04: 4, f05: 5, f06: 6, f07: 7, f08: 8, f09: 9,
        f10: 10, f11: 11, f12: 12, f13: 13, f14: 14, f15: 15, f16: 16, f17: 17, f18: 18, f19: 19,
        f20: 20, f21: 21, f22: 22, f23: 23, f24: 24, f25: 25, f26: 26, f27: 27, f28: 28, f29: 29,
        f30: 30, f31: 31, f32: 32, f33: 33, f34: 34, f35: 35, f36: 36, f37: 37, f38: 38, f39: 39,
        f40: 40, f41: 41, f42: 42, f43: 43, f44: 44, f45: 45, f46: 46, f47: 47, f48: 48, f49: 49,
        f50: 50, f51: 51, f52: 52, f53: 53, f54: 54, f55: 55, f56: 56, f57: 57, f58: 58, f59: 59,
        f60: 60, f61: 61, f62: 62, f63: 63, f64: 64, f65: 65, f66: 66, f67: 67, f68: 68,
        @last f69: 69,
    }

    let mut json = String::from(r#"{"last":69"#);
    for i in (1..69).rev() {
        json.push(',');
        json.push_str(&format!(r#""f{i:02}":{i}"#));
    }
    json.push('}');

    assert_eq!(
        from_json::<WideFlattenable>(&json).unwrap(),
        WideFlattenable::expected()
    );

    let mut wrapper_json = String::from(r#"{"head":7,"last":69"#);
    for i in (1..69).rev() {
        wrapper_json.push(',');
        wrapper_json.push_str(&format!(r#""f{i:02}":{i}"#));
    }
    wrapper_json.push_str(r#","unknown":true,"tail":9}"#);

    assert_eq!(
        from_json::<WideFlattenWrapper>(&wrapper_json).unwrap(),
        WideFlattenWrapper::expected()
    );

    let err = from_json::<WideFlattenable>(r#"{"last":69}"#).unwrap_err();
    assert_eq!(err.decoding_error(), &jsony::error::MISSING_REQUIRED_FIELDS);

    let err =
        from_json::<WideFlattenWrapper>(r#"{"head":7,"last":69,"f69":70,"tail":9}"#).unwrap_err();
    assert_eq!(err.decoding_error(), &jsony::error::DUPLICATE_FIELD);
}
