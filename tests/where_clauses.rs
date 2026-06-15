// Exercises `where` clauses on the derive target across every position and
// trait path. The derive_tester corpus only checks the macro expands without a
// `compile_error!`; this file is the oracle that the generated impls actually
// compile and round-trip. Each impl repeats the target's `where` clause, which
// Rust requires when naming a type that carries one.
use jsony::Jsony;

// Braced struct: clause before the body. Json -> ToJson + FromJson.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(Json)]
struct WhereStruct<T>
where
    T: Clone,
{
    a: T,
    b: u32,
}

// Tuple struct: clause after the body. Binary -> ToBinary + FromBinary.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(Binary)]
struct WhereTuple<T>(T)
where
    T: Clone;

// Enum: clause before the body, with a unit, tuple and struct-like variant.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(Json)]
enum WhereEnum<T>
where
    T: Clone,
{
    Unit,
    Tuple(T),
    Struct { x: T },
}

#[test]
fn where_struct_json_round_trip() {
    let value = WhereStruct {
        a: "hi".to_string(),
        b: 7,
    };
    let json = jsony::to_json(&value);
    let back = jsony::from_json::<WhereStruct<String>>(&json).unwrap();
    assert_eq!(back, value);
}

#[test]
fn where_tuple_binary_round_trip() {
    let value = WhereTuple(42i32);
    let bytes = jsony::to_binary(&value);
    let back = jsony::from_binary::<WhereTuple<i32>>(&bytes).unwrap();
    assert_eq!(back, value);
}

#[test]
fn where_enum_json_round_trip() {
    for value in [
        WhereEnum::Unit,
        WhereEnum::Tuple("v".to_string()),
        WhereEnum::Struct { x: "w".to_string() },
    ] {
        let json = jsony::to_json(&value);
        let back = jsony::from_json::<WhereEnum<String>>(&json).unwrap();
        assert_eq!(back, value);
    }
}
