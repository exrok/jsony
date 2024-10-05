use jsony::{object, ToJson};

#[test]
fn empty_object() {
    assert_eq!(object! {}, "{}")
}

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
    ({ $($input:tt)* }, {$($expected:tt)*}) => {
        assert_eq!(
            object! {$($input)*},
            compact_stringify!({$($expected)*})
        )
    };
}

#[test]
fn simple_objects() {
    assert_object_eq!(
        {
            simple_key: "simple_value",
            number: 324,
            truthy: false,
        },
        {
            "simple_key": "simple_value",
            "number": 324,
            "truthy": false
        }
    );

    assert_object_eq!(
        {
            simple_key: "simple_value",
            nested: {
                key: "value",
                nested_more: {
                    even_more: "wow"
                }
            },
        },
        {
            "simple_key": "simple_value",
            "nested": {
                "key": "value",
                "nested_more": {
                    "even_more": "wow"
                }
            }
        }
    );
}

struct StructOther {
    field2: u32,
    vector: Vec<u32>,
}
struct Struct {
    field: u32,
    other: StructOther,
    array: [u32; 2],
}
impl ToJson for StructOther {
    type Into = jsony::json::RawBuf;

    fn jsonify_into(&self, output: &mut Self::Into) {
        self.field2.jsonify_into(output);
    }
}

#[test]
fn expression_values() {
    let data = Struct {
        field: 32,
        other: StructOther {
            field2: 42,
            vector: vec![1, 2, 3],
        },
        array: [4, 2],
    };

    assert_object_eq!(
        {
            array: [1,2,3,4],
            number: data.field,
        },
        {
            "array": [1,2,3,4],
            "number": 32
        }
    );

    assert_object_eq!(
        {
            method: data.array.len(),
            method_nested: data.other.vector.len(),
            needs_parens: data.array[0],
        },
        {
            "method": 2,
            "method_nested": 3,
            "needs_parens": 4
        }
    );

    assert_object_eq!(
        {
            path_access: u32::MAX,
            init_struct: StructOther{
                field2: 32,
                vector: vec![1,3,3]
            }
        },
        {
            "path_access": 4294967295,
            "init_struct": 32
        }
    );
}

// #[test]
// fn appending() {
//     let mut builder = JsonBuilder::default();
//     {
//         let mut obj = builder.object();
//         append_object!(
//             obj;
//             what: 32,
//             value: "test"
//         );
//         append_object!(
//             obj;
//             nested: {
//                 value: false
//             },
//         );
//     }

//     assert_eq! {
//         builder.build().unwrap().string,
//         compact_stringify!({
//             "what": 32,
//             "value": "test",
//             "nested": {
//                 "value": false
//             }
//         })
//     }
// }

#[test]
fn simple_match() {
    let data = Struct {
        field: 32,
        other: StructOther {
            field2: 42,
            vector: vec![1, 2, 3],
        },
        array: [4, 2],
    };
    let expect = [
        (0, r#"{"value":"hello"}"#),
        (1, r#"{"value":false}"#),
        (2, r#"{"value":[1,2,3,4]}"#),
        (3, r#"{"value":0}"#),
        (4, r#"{"value":{"nested":"works"}}"#),
        (5, r#"{"value":999}"#),
        (6, r#"{"value":42}"#),
        (7, r#"{"value":3}"#),
        (8, r#"{"value":4}"#),
    ];
    for (i, expect) in expect {
        assert_eq!(
            object! {
                value: match i {
                    0 => "hello",
                    1 => false,
                    2 => [1,2,3,4],
                    3 => u32::MIN,
                    4 => {
                        nested: "works"
                    },
                    5 => StructOther{
                        field2: 999,
                        vector: vec![1,3,3]
                    },
                    6 => data.other.field2,
                    7 => data.other.vector.len(),
                    _ => 1+1+1+1
                },
            },
            expect
        );
    }
}

enum TestEnum {
    Value { value: u32, other: bool },
    Fieldless,
    Unitly(String),
}

#[test]
fn array_match_pattern() {
    let data = [
        TestEnum::Value {
            value: 234,
            other: false,
        },
        TestEnum::Fieldless,
        TestEnum::Unitly("STUFF".into()),
    ];

    assert_object_eq!(
        {
            all: [
                for value in data;
                {
                    field: match value{
                        TestEnum::Value{value, other} => {
                            value, other, more: "nice"
                        },
                        TestEnum::Fieldless => "text",
                        TestEnum::Unitly(value) => {value},
                    }
                }
            ]
        },
        {
            "all": [
                {"field": {"value": 234, "other": false, "more": "nice"}},
                {"field": "text"},
                {"field": {"value": "STUFF"} }
            ]
        }
    )
}

#[test]
fn for_match() {
    let data = [
        TestEnum::Value {
            value: 234,
            other: false,
        },
        TestEnum::Fieldless,
        TestEnum::Unitly("STUFF".into()),
    ];

    assert_object_eq!(
        {
            all: [
                for value in data;
                match value {
                    TestEnum::Value{value, other} => {
                        value, other, more: "nice"
                    },
                    TestEnum::Fieldless => {nice: "text"},
                    TestEnum::Unitly(value) => {value},
                }
            ]
        },
        {
            "all": [
                {"value": 234, "other": false, "more": "nice"},
                {"nice": "text"},
                {"value": "STUFF"}
            ]
        }
    )
}

#[test]
fn array() {
    let data = [1, 2, 3, 4];

    assert_object_eq!(
        {
            nums: [for value in data; value],
            nested: [for value in data; [value]],
            object: [for value in data; {value}],
            expr: [for value in data; value * value],
        },
        {
            "nums": [1,2,3,4],
            "nested": [[1],[2],[3],[4]],
            "object": [
                {"value": 1},
                {"value": 2},
                {"value": 3},
                {"value": 4}
            ],
            "expr": [1,4,9,16]
        }
    )
}

#[test]
fn basic_control_flow() {
    assert_object_eq!(
        {
            ifs: if false {
                "hello"
            } else {
                "bye"
            },
            matches: match 3 {
                0 => false,
                3 => true,
                _ => false
            },
        },
        {
            "ifs": "bye",
            "matches": true
        }
    )
}

// #[test]
// fn if_condition() {
//     assert_object_eq!(
//         {
//             [for value in [0, 1]] value: {
//                 [if value == 1] only_1: 1
//             },
//         },
//         {
//             "value": [{}, {"only_1": 1}]
//         }
//     );

//     let value1: Option<u32> = None;
//     let value2: Option<u32> = Some(1);
//     assert_object_eq!(
//         {
//             [if let Some(value) = value1] got1: value,
//             [if let Some(value) = value2] got2: value,
//         },
//         {
//             "got2": 1
//         }
//     )
// }

// #[test]
// fn if_flatten() {
//     let value1: Option<u32> = None;
//     let value2: Option<u32> = Some(2);
//     assert_object_eq!(
//         {
//             [if let Some(value) = value1] r#flatten: {
//                 value,
//                 other1: false
//             },
//             [if let Some(value) = value2] r#flatten: {
//                 value,
//                 other2: true
//             },
//         },
//         {
//             "value": 2,
//             "other2": true
//         }
//     )
// }

// #[test]
// fn func() {
//     let borrow = vec![1, 2, 3, 4];
//     assert_object_eq!(
//         {
//             nothing: |value| {},
//             scalar: |value| { value.value(&32) },
//             array: |value| {
//                 let mut array = value.array();
//                 array.value(&3);
//                 array.value(&false);
//                 array.value(&"wow");
//             },
//             object: |value| {
//                 let mut obj = value.object();
//                 obj.key("hello").value(&123);
//                 obj.key("wow").value(&321);
//             },
//             borrowed: |value| {
//                 value.value(&borrow);
//             },
//         },
//         {
//             "nothing": null,
//             "scalar": 32,
//             "array": [3, false, "wow"],
//             "object": {
//                 "hello": 123,
//                 "wow": 321
//             },
//             "borrowed": [1,2,3,4]
//         }
//     );
//     drop(borrow);
// }

// #[test]
// fn func_flatten() {
//     assert_object_eq!(
//         {
//             r#flatten: |obj| {
//                 obj.key("hello").value(&123);
//                 obj.key("wow").value(&321);
//             },
//         },
//         {
//             "hello": 123,
//             "wow": 321
//         }
//     );

//     assert_object_eq!(
//         {
//             r#flatten: |obj| {},
//             nice: false
//         },
//         {
//             "nice": false
//         }
//     );
// }
