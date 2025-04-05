use std::borrow::Cow;

use jsony::{from_json, parser::TRAILING_COMMA, JsonParserConfig, Jsony};

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
