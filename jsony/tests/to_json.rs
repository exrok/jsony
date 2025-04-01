use std::collections::HashMap;

use jsony::Jsony;

macro_rules! flat_stringify_tt {
    ({$($tt:tt)*}) => {concat!("{", $(flat_stringify_tt!($tt),)* "}")};
    ([$($tt:tt)*]) => {concat!("[", $(flat_stringify_tt!($tt),)* "]")};
    (($literal:literal)) => {$literal};
    ($tt:tt) => {stringify!($tt)};
}

macro_rules! compact_stringify {
    ($($tt:tt)*) => {concat!($(flat_stringify_tt!{$tt}),*)};
}

macro_rules! assert_object_eq {
    (( $($input:tt)* ), {$($expected:tt)*}) => {
        assert_eq!(
            jsony::to_json(&$($input)*).as_str(),
            compact_stringify!({$($expected)*})
        )
    };
}
#[test]
fn flatten() {
    #[derive(Jsony, Debug)]
    #[jsony(ToJson)]
    struct FlattenExample<'a> {
        #[jsony(flatten)]
        a: HashMap<&'a str, i64>,
        #[jsony(flatten)]
        b: HashMap<&'a str, i64>,
        val: u32,
        #[jsony(flatten)]
        c: HashMap<&'a str, i64>,
        #[jsony(flatten)]
        d: HashMap<&'a str, i64>,
    }
    assert_object_eq!((FlattenExample {
        a: HashMap::new(),
        b: HashMap::new(),
        val: 23,
        c: HashMap::new(),
        d: HashMap::new(),
    }), {
        "val": 23
    });

    assert_object_eq!((FlattenExample {
        a: [("a0", 0)].into_iter().collect(),
        b: HashMap::new(),
        val: 23,
        c: HashMap::new(),
        d: HashMap::new(),
    }), {
        "a0": 0,
        "val": 23
    });
    assert_object_eq!((FlattenExample {
        a: [("a0", 0)].into_iter().collect(),
        b: [("b1", 1)].into_iter().collect(),
        val: 23,
        c: HashMap::new(),
        d: HashMap::new(),
    }), {
        "a0": 0,
        "b1": 1,
        "val": 23
    });

    assert_object_eq!((FlattenExample {
        a: HashMap::new(),
        b: HashMap::new(),
        val: 23,
        c: [("c0", 0)].into_iter().collect(),
        d: [("d1", 1)].into_iter().collect(),
    }), {
        "val": 23,
        "c0": 0,
        "d1": 1
    });
}

#[test]
fn flatten_via_iterator() {
    #[derive(Jsony, Debug)]
    #[jsony(ToJson)]
    struct FlattenExample<'a> {
        #[jsony(flatten, via = Iterator)]
        a: Vec<(&'a str, i64)>,
        #[jsony(flatten, via = Iterator)]
        b: Vec<(&'a str, i64)>,
        val: u32,
        #[jsony(flatten, via = Iterator)]
        c: Vec<(&'a str, i64)>,
        #[jsony(flatten, via = Iterator)]
        d: Vec<(&'a str, i64)>,
    }
    assert_object_eq!((FlattenExample {
        a: Vec::new(),
        b: Vec::new(),
        val: 23,
        c: Vec::new(),
        d: Vec::new(),
    }), {
        "val": 23
    });

    assert_object_eq!((FlattenExample {
        a: [("a0", 0)].into_iter().collect(),
        b: Vec::new(),
        val: 23,
        c: Vec::new(),
        d: Vec::new(),
    }), {
        "a0": 0,
        "val": 23
    });
    assert_object_eq!((FlattenExample {
        a: [("a0", 0)].into_iter().collect(),
        b: [("b1", 1)].into_iter().collect(),
        val: 23,
        c: Vec::new(),
        d: Vec::new(),
    }), {
        "a0": 0,
        "b1": 1,
        "val": 23
    });

    assert_object_eq!((FlattenExample {
        a: Vec::new(),
        b: Vec::new(),
        val: 23,
        c: [("c0", 0)].into_iter().collect(),
        d: [("d1", 1)].into_iter().collect(),
    }), {
        "val": 23,
        "c0": 0,
        "d1": 1
    });
}

#[test]
fn to_vec() {
    let mut data = vec![b'a', b'b', b'c'];
    assert_eq!(
        jsony::to_json_into(&"hello_this_is_a_long_string", &mut data),
        "\"hello_this_is_a_long_string\""
    );
    assert_eq!(jsony::to_json_into(&false, &mut data), "false");
    assert_eq!(
        String::from_utf8(data).unwrap(),
        "abc\"hello_this_is_a_long_string\"false"
    );

    let mut data = Vec::new();
    assert_eq!(jsony::to_json_into(&100u32, &mut data), "100");
    assert_eq!(jsony::to_json_into(&false, &mut data), "false");
    assert_eq!(String::from_utf8(data).unwrap(), "100false");
}
