// These tests deliberately do NOT import jsony's traits (FromJson, ToJson,
// FromBinary, ToBinary). Derived output must be self-contained and reference
// every jsony trait by an explicit path. Only the `Jsony` derive macro itself
// is brought into scope.
//
// To also guard against name collisions, decoy traits sharing the jsony trait
// names are declared in scope. A correct derive resolves to `::jsony::*` and
// ignores these.
use jsony::Jsony;

#[allow(dead_code)]
trait FromJson {}
#[allow(dead_code)]
trait ToJson {}
#[allow(dead_code)]
trait FromBinary {}
#[allow(dead_code)]
trait ToBinary {}

#[derive(Jsony)]
#[jsony(FromJson, ToJson, FromBinary, ToBinary)]
struct Wrapper<T> {
    inner: T,
    count: u32,
}

#[test]
fn generic_derive_is_self_contained() {
    let w = Wrapper {
        inner: 5i32,
        count: 2u32,
    };
    let s = jsony::to_json(&w);
    let back = jsony::from_json::<Wrapper<i32>>(&s).unwrap();
    assert_eq!(back.inner, 5);
    assert_eq!(back.count, 2);

    let bytes = jsony::to_binary(&w);
    let from_bin = jsony::from_binary::<Wrapper<i32>>(&bytes).unwrap();
    assert_eq!(from_bin.inner, 5);
    assert_eq!(from_bin.count, 2);
}
