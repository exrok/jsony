use jsony::{from_json, Jsony};

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
    assert_eq!(x.a2, true);
    let x = from_json::<Simple>(obj! { "a2": true, "a1": 23 }).unwrap();
    assert_eq!(x.a1, 23);
    assert_eq!(x.a2, true);
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
    assert_eq!(nested.x.a2, true);
    assert_eq!(nested.y.a1, 22);
    assert_eq!(nested.y.a2, false);
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
    assert_eq!(x.a2, true);
    let x = from_json::<Simple>(obj! {}).unwrap();
    assert_eq!(x.a1, 203);
    assert_eq!(x.a2, false);

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
    assert_eq!(nested.x.a2, true);
    assert_eq!(nested.y.a1, 203);
    assert_eq!(nested.y.a2, false);

    let nested = from_json::<Nested>(obj! {
        "x": { "a2": true}
    })
    .unwrap();
    assert_eq!(nested.x.a1, 203);
    assert_eq!(nested.x.a2, true);
    assert_eq!(nested.y.a1, 0);
    assert_eq!(nested.y.a2, false);
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
