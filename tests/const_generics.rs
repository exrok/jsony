// Const-generic support for `#[derive(Jsony)]`. These compile the derived
// output (the corpus harness in derive_tester does not), so they verify that
// const generics render correctly in declaration position (`impl<const N: …>`,
// `type __TEMP<const N: …>`) and type-argument position (`Ty<N>`, `offset_of`).
use jsony::Jsony;

// Const generic present on the impl header but unused by any field. Proves the
// `impl<const N: usize> … for C<N>` header compiles across all four codecs.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(FromJson, ToJson, FromBinary, ToBinary)]
struct C<const N: usize> {
    a: u32,
}

#[test]
fn const_generic_unused_in_fields() {
    let original: C<3> = C { a: 42 };
    let s = jsony::to_json(&original);
    let back: C<3> = jsony::from_json(&s).unwrap();
    assert_eq!(original, back);

    let bytes = jsony::to_binary(&original);
    let back: C<3> = jsony::from_binary(&bytes).unwrap();
    assert_eq!(original, back);
}

// Const generic used in a field type `[u32; N]`. The library implements the
// codecs for `[T; N]`, so the field round-trips and `offset_of!(Holder<N>, …)`
// must emit a bare `N`.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(FromJson, ToJson, FromBinary, ToBinary)]
struct Holder<const N: usize> {
    items: [u32; N],
}

#[test]
fn const_generic_in_field() {
    let original: Holder<3> = Holder { items: [1, 2, 3] };
    let s = jsony::to_json(&original);
    assert_eq!(s, r#"{"items":[1,2,3]}"#);
    let back: Holder<3> = jsony::from_json(&s).unwrap();
    assert_eq!(original, back);

    let bytes = jsony::to_binary(&original);
    let back: Holder<3> = jsony::from_binary(&bytes).unwrap();
    assert_eq!(original, back);
}

// Lifetime + type param + const generic together, exercising the full generic
// list in both impl and type-argument positions.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(FromJson, ToJson)]
struct Mixed<'a, T, const N: usize> {
    t: T,
    s: &'a str,
    arr: [u32; N],
}

#[test]
fn const_generic_mixed_with_lifetime_and_type() {
    let original: Mixed<i32, 2> = Mixed {
        t: 7,
        s: "hi",
        arr: [10, 20],
    };
    let s = jsony::to_json(&original);
    let back: Mixed<i32, 2> = jsony::from_json(&s).unwrap();
    assert_eq!(original, back);
}

// Enum struct variant referencing the const generic. This drives the nested
// `type __TEMP<const N: usize> = ([u32; N],)` alias declaration, which must
// declare the const in definition form.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(Json)]
enum CGE<const N: usize> {
    V { a: [u32; N] },
    Unit,
}

#[test]
fn const_generic_enum_struct_variant() {
    for original in [CGE::V { a: [1u32, 2, 3] }, CGE::Unit] {
        let s = jsony::to_json(&original);
        let back: CGE<3> = jsony::from_json(&s).unwrap();
        assert_eq!(original, back);
    }
}

// Const generic with a default. The default is dropped from the impl header
// (impl headers cannot carry one) while the `usize` type is retained.
#[derive(Jsony, Debug, PartialEq)]
#[jsony(Json)]
struct WithDefault<const N: usize = 4> {
    a: u32,
}

#[test]
fn const_generic_with_default() {
    let original: WithDefault = WithDefault { a: 1 };
    let s = jsony::to_json(&original);
    let back: WithDefault = jsony::from_json(&s).unwrap();
    assert_eq!(original, back);

    let explicit: WithDefault<8> = WithDefault { a: 2 };
    let s = jsony::to_json(&explicit);
    let back: WithDefault<8> = jsony::from_json(&s).unwrap();
    assert_eq!(explicit, back);
}
