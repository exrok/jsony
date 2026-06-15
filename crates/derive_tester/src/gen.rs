//! Case model + deterministic generation.
//!
//! A `Case` is fully determined by its `u64` id (used as an RNG seed), so any
//! failing case is reproducible from its id alone. The model and the JSON input
//! sampler are deliberately coupled: whatever attributes a case emits, the
//! sampler produces matching, round-trip-safe JSON.

use jsony::{TextWriter, ToJson};
use rand::prelude::*;

use crate::casing::RenameRule;
use crate::datagen::Rand;
use crate::schema::Type;

#[derive(Clone, Copy)]
pub struct TraitSet {
    pub from_json: bool,
    pub to_json: bool,
    pub from_binary: bool,
    pub to_binary: bool,
}

impl TraitSet {
    pub fn binary(&self) -> bool {
        self.from_binary && self.to_binary
    }
    /// The `#[jsony(...)]` trait list, e.g. `FromJson, ToJson, FromBinary, ToBinary`.
    fn attr_list(&self) -> String {
        let mut parts = Vec::new();
        if self.from_json {
            parts.push("FromJson");
        }
        if self.to_json {
            parts.push("ToJson");
        }
        if self.from_binary {
            parts.push("FromBinary");
        }
        if self.to_binary {
            parts.push("ToBinary");
        }
        parts.join(", ")
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum EnumRepr {
    External,
    Internal,
    Adjacent,
    /// `#[jsony(untagged)]` — no tag wrapper. Variants are tried in declaration
    /// order and the first structurally-matching one wins. JSON-only. The
    /// generator keeps variants disjoint (see [`gen_untagged_variants`]) so the
    /// match is unambiguous and the round-trip oracle stays valid.
    Untagged,
}

pub const TAG: &str = "kind";
pub const CONTENT: &str = "data";

/// The constant emitted (and asserted) for the `Const` default. A non-zero
/// value distinguishes "default applied" from "field left zero-initialized",
/// and fits every generated integer type.
pub const DEFAULT_CONST: i64 = 7;

/// How a field's `#[jsony(default)]` is rendered, and whether the sampler can
/// make an absolute assertion about the value it fills in.
#[derive(Clone, Copy, PartialEq)]
pub enum DefaultSpec {
    /// No `default` attribute. The field is always present in the input.
    None,
    /// `#[jsony(default)]` — the bare form, fills with `Default::default()`.
    Bare,
    /// `#[jsony(default = Default::default())]` — the custom-expression branch
    /// (`DefaultKind::Custom`), distinct codegen from the bare form.
    Expr,
    /// `#[jsony(default = DEFAULT_CONST)]` on an integer field. The known
    /// constant lets the sampler assert the filled value absolutely: an input
    /// that omits the field must decode equal to one that sets it to the
    /// constant.
    Const,
}

impl DefaultSpec {
    /// Whether the field carries any `default`, so the sampler may omit it.
    fn is_set(self) -> bool {
        self != DefaultSpec::None
    }
}

/// The integer sentinel a rejecting validator refuses. A small positive value
/// fits every integer field type (`TryFrom<u8>` succeeds) and is distinct from
/// the common `MIN`/`MAX`/`0` boundary values the int sampler favours, so the
/// avoidance in [`FieldSpec::sample_value`] almost never has to perturb a value.
pub const VALIDATE_SENTINEL: i64 = 13;

/// How a field's `#[jsony(validate = ...)]` is rendered, and whether the sampler
/// can drive a rejecting input through it.
#[derive(Clone, Copy, PartialEq)]
pub enum ValidateKind {
    /// No `validate` attribute.
    None,
    /// `validate = dt_validate_ok` — the always-`Ok` generic validator. Proves the
    /// validate codegen runs and does not corrupt a well-formed decode. Any type.
    Ok,
    /// `validate = dt_validate_reject` — a function validator rejecting the integer
    /// [`VALIDATE_SENTINEL`]. Integer fields only.
    Reject,
    /// `validate = jsony::require!(|v| *v != SENTINEL, ..)` — the `require!` closure
    /// flavor, rejecting the same sentinel. Integer fields only.
    Require,
}

impl ValidateKind {
    /// Whether this validator rejects the sentinel. Such a field's well-formed
    /// inputs must avoid the sentinel, and a sentinel input is a BAD input.
    fn rejects(self) -> bool {
        matches!(self, ValidateKind::Reject | ValidateKind::Require)
    }
}

/// Integer types, the only ones that take a `Const` default (a bare integer
/// literal coerces to any of them, and its JSON form is the decimal digits).
fn is_int(ty: Type) -> bool {
    matches!(
        ty,
        Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::U32
            | Type::I32
            | Type::U64
            | Type::I64
            | Type::U128
            | Type::I128
    )
}

pub struct FieldSpec {
    pub name: String,
    pub ty: Type<'static>,
    /// `#[jsony(rename = "...")]`; the JSON key actually used.
    pub rename: Option<String>,
    /// `#[jsony(alias = "...")]`; an additional accepted input key.
    pub alias: Option<String>,
    /// `#[jsony(default ...)]` — the field may be omitted from the input.
    pub default: DefaultSpec,
    /// `#[jsony(skip)]` — never present in JSON, filled with Default.
    pub skip: bool,
    /// `#[jsony(with = jsony::helper::json_string)]` — the field's value is
    /// serialized as a JSON string containing the value's own JSON. The sampler
    /// emits the matching transformed input (see [`FieldSpec::with_wrap`]).
    pub with: bool,
    /// `#[jsony(validate = ...)]` — a validator runs on the decoded value.
    /// Mutually exclusive with `with` (jsony rejects both on one field). The
    /// rejecting flavors ([`ValidateKind::Reject`]/[`ValidateKind::Require`]) are
    /// keyed on [`VALIDATE_SENTINEL`].
    pub validate: ValidateKind,
}

impl FieldSpec {
    /// The canonical JSON key for this field. An explicit `rename` wins
    /// outright, otherwise the active `rename_all`/`rename_all_fields` rule
    /// recases the declared name. This mirrors jsony's `field_name_literal`.
    fn key(&self, rule: RenameRule) -> String {
        match &self.rename {
            Some(r) => r.clone(),
            None => rule.apply_to_field(&self.name),
        }
    }

    /// Pick a key to write into a sampled input: the canonical key, or, when the
    /// field has an alias, the alias half the time (exercising the alias parse
    /// path). The write side always emits the canonical key, so round-trip
    /// identity holds either way. `alias` is a literal key unaffected by
    /// `rename_all`.
    fn input_key(&self, rule: RenameRule, rand: &mut Rand) -> String {
        match &self.alias {
            // The encode oracle needs the canonical key (the re-encoded form), so
            // never substitute the alias under `full`.
            Some(a) if !rand.full && rand.rng.gen_bool(0.5) => a.clone(),
            _ => self.key(rule),
        }
    }

    /// Map a field value's native JSON text to the on-the-wire form. With
    /// `#[jsony(with = jsony::helper::json_string)]` the value's JSON is embedded
    /// as a JSON string (`42` becomes `"42"`), matching what the helper encodes,
    /// so the sampled input drives the `with` decode path and re-encoding
    /// reproduces it. Otherwise the value is passed through unchanged.
    fn with_wrap(&self, native: String) -> String {
        if self.with {
            json_string(&native)
        } else {
            native
        }
    }

    /// The on-the-wire value text for a well-formed input: a random value's JSON
    /// with the `with` transform applied. For a rejecting validator the value is
    /// kept off [`VALIDATE_SENTINEL`] so the input stays well-formed (the sentinel
    /// is reserved for the BAD input built by [`build_validate_reject`]).
    fn sample_value(&self, rand: &mut Rand) -> String {
        let mut native = raw_value(self.ty, rand);
        if self.validate.rejects() && native == VALIDATE_SENTINEL.to_string() {
            native = (VALIDATE_SENTINEL + 1).to_string();
        }
        self.with_wrap(native)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum VarKind {
    Unit,
    Tuple,
    Struct,
}

pub struct VariantSpec {
    pub name: String,
    pub kind: VarKind,
    pub fields: Vec<FieldSpec>,
    /// `#[jsony(rename = "...")]` on the variant; the JSON variant name actually
    /// used. An explicit rename wins outright, bypassing `rename_all`.
    pub rename: Option<String>,
    /// `#[jsony(rename_all = "...")]` on the variant; recases this variant's own
    /// struct-field keys, overriding the container `rename_all_fields`/`rename_all`
    /// for those fields. Only meaningful for struct variants.
    pub rename_all: RenameRule,
    /// `#[jsony(other)]` — the catch-all variant for an unknown tag (FromJson).
    /// Only one per enum, and only on a unit variant.
    pub other: bool,
}

impl VariantSpec {
    /// The JSON name for this variant: an explicit `rename` wins outright,
    /// otherwise the container `rename_all` recases the declared name. Mirrors
    /// jsony's variant-name resolution.
    fn json_name(&self, rule: RenameRule) -> String {
        match &self.rename {
            Some(r) => r.clone(),
            None => rule.apply_to_variant(&self.name),
        }
    }

    /// Rename rule applied to this variant's struct-field keys. Mirrors jsony's
    /// `variant_field_rename_rule`: the per-variant `rename_all` wins first, then
    /// the container `rename_all_fields`, then the container `rename_all`.
    fn field_rule(&self, case: &Case) -> RenameRule {
        if self.rename_all != RenameRule::None {
            self.rename_all
        } else if case.rename_all_fields != RenameRule::None {
            case.rename_all_fields
        } else {
            case.rename_all
        }
    }
}

/// A variant of a `ToStr`/`FromStr` enum (fieldless). Its string form follows an
/// explicit per-variant `rename`, else the container `rename_all`.
pub struct StrVariant {
    pub name: String,
    pub rename: Option<String>,
}

impl StrVariant {
    /// The string `to_str` yields and `parse` accepts for this variant.
    fn expected(&self, rule: RenameRule) -> String {
        match &self.rename {
            Some(r) => r.clone(),
            None => rule.apply_to_variant(&self.name),
        }
    }

    /// The expected string under the case's `rename_all`.
    pub fn expected_str(&self, case: &Case) -> String {
        self.expected(case.rename_all)
    }
}

pub enum Body {
    Named(Vec<FieldSpec>),
    Tuple(Vec<FieldSpec>),
    Unit,
    Enum {
        repr: EnumRepr,
        variants: Vec<VariantSpec>,
    },
    /// A fieldless enum deriving `#[jsony(ToStr, FromStr)]`. Checked by a dedicated
    /// string oracle (no JSON/binary), see `emit::emit_str_arm`.
    Str(Vec<StrVariant>),
    /// A binary version family: several `#[jsony(Binary, version ...)]` structs
    /// sharing a field plan, exercised by a dedicated cross-version oracle (no
    /// JSON), see `emit::emit_version_arm`. Emits its own type definitions.
    Version(VersionFamily),
    /// A `#[jsony(Binary, zerocopy)]` POD struct, exercised by a dedicated
    /// alignment oracle (no JSON), see `emit::emit_pod_arm`. Emits its own type
    /// definitions (the POD struct plus transparent POD / non-POD wrappers).
    Pod(PodFamily),
    /// A ToJson-only struct with a `#[jsony(flatten, via = Iterator)] Vec<(String,
    /// i64)>` field, exercised by a dedicated encode oracle (no decode: via=Iterator
    /// is encode-only and does not dedup). Emits its own type definition.
    ViaIter(ViaFamily),
    /// A self-contained with-helper / binary-rejection case. Emits its own type
    /// definition and a fixed oracle (no per-input JSON).
    Helper(HelperKind),
}

/// A ToJson-only `via = Iterator` family: a leading `a: u32` field plus a flattened
/// `Vec<(String, i64)>` rendered as inline object members in insertion order,
/// duplicate keys preserved (no dedup).
pub struct ViaFamily {
    pub lead: u32,
    pub pairs: Vec<(String, i64)>,
}

/// A self-contained with-helper / binary-rejection case, each checked by a fixed
/// oracle (no per-input JSON). Covers the helper-breadth and binary-rejection gaps.
#[derive(Clone, Copy)]
pub enum HelperKind {
    /// `Vec<(String, i64)>` via `jsony::helper::object_as_vec_of_tuple`: decodes
    /// from / encodes to a JSON object, order-preserving.
    ObjVecString,
    /// `Vec<(u32, u32)>` via the same helper, exercising numeric (quoted) keys.
    ObjVecNum,
    /// `Cow<'a, [&'a str]>` via `#[jsony(From with = jsony::helper::owned_cow)]`,
    /// round-tripped in both JSON and binary.
    OwnedCow,
    /// A `#[jsony(Binary)]` struct with a `require!` validator; the sentinel value
    /// is encoded and must be **rejected** on binary decode (the binary-rejection
    /// gap, complementing the JSON `KIND_REJECT` path).
    BinReject,
}

/// Pick a helper kind deterministically.
fn gen_helper_kind(rng: &mut StdRng) -> HelperKind {
    match rng.gen_range(0..4) {
        0 => HelperKind::ObjVecString,
        1 => HelperKind::ObjVecNum,
        2 => HelperKind::OwnedCow,
        _ => HelperKind::BinReject,
    }
}

/// Build a `via = Iterator` family. Keys are drawn from a tiny pool so duplicate
/// keys occur naturally (the no-dedup property), values are distinct per entry.
fn gen_via_family(rng: &mut StdRng) -> ViaFamily {
    const KEYS: &[&str] = &["x", "y", "z"];
    let n = rng.gen_range(0..4);
    let pairs = (0..n)
        .map(|i| {
            let k = KEYS[rng.gen_range(0..KEYS.len())].to_string();
            (k, (i as i64) * 7 - 3)
        })
        .collect();
    ViaFamily {
        lead: rng.gen_range(0..1000),
        pairs,
    }
}

/// One field in a version family's shared field plan. Field `i` is present in a
/// schema iff `intro <= schema.cur`. `intro == 0` marks a required base field
/// carried across every version; `intro > 0` fields are introduced later and,
/// when decoding bytes whose version predates the field, are filled from `fill`.
pub struct VersionField {
    pub ty: Type<'static>,
    pub intro: u16,
    /// A deterministically-sampled value literal (the value actually encoded).
    pub value: String,
    /// `Some(lit)` => `#[jsony(default = lit)]`; the value filled when the field
    /// is absent from older bytes. `None` => no `default` attribute, so jsony
    /// fills `Default::default()` ([`VersionField::fill`] returns the zero/false
    /// literal in that case). Always distinct from `value` so the fill is
    /// observable.
    pub default: Option<String>,
}

impl VersionField {
    /// The literal jsony must fill when this field is absent from the decoded
    /// bytes: the explicit `default`, else the type's `Default` (`0` / `false`).
    pub fn fill(&self) -> &str {
        match &self.default {
            Some(d) => d,
            None => match self.ty {
                Type::Bool => "false",
                _ => "0",
            },
        }
    }
}

/// A decoder/encoder schema in a version family: a struct containing the field
/// plan's fields with `intro <= cur`, deriving `#[jsony(Binary, version ...)]`
/// with the given accepted version window `[min, cur]`.
pub struct VersionSchema {
    pub min: u16,
    pub cur: u16,
}

/// A binary version family: a shared field plan instantiated at several schema
/// versions. The oracle encodes each schema and cross-decodes it as every other,
/// asserting in-window decodes fill the right defaults and out-of-window decodes
/// error (see `emit::emit_version_arm`).
pub struct VersionFamily {
    /// Tuple-struct bodies instead of named.
    pub tuple: bool,
    /// The shared field plan, ordered by non-decreasing `intro` (base fields
    /// first). `value`/`default` are deterministically sampled per field so the
    /// cross-version assertions are absolute.
    pub fields: Vec<VersionField>,
    /// The instantiated schemas (declared `{name}_S{idx}`).
    pub schemas: Vec<VersionSchema>,
}

/// One field of a `zerocopy` POD struct: a fixed-width scalar (or fixed array of
/// one), all sharing a single alignment class so `repr(C)` introduces no padding.
pub struct PodField {
    pub ty: Type<'static>,
    pub value: String,
}

/// A `#[jsony(Binary, zerocopy)] #[repr(C)]` POD struct: its fields all share one
/// alignment class, so the layout is gap-free and the `POD` const-asserts hold.
/// Exercised by a self-contained alignment oracle (see `emit::emit_pod_arm`).
pub struct PodFamily {
    pub fields: Vec<PodField>,
}

/// A flattened field on a top-level named struct: `#[jsony(flatten)] {field}:
/// {kind}`. At most one per container (jsony's `FromJson` supports only one). The
/// flattened members are inlined into the parent object; the parent `rename_all`
/// does NOT recase them (proven by the encode oracle, which requires the verbatim
/// keys to re-encode under a recasing parent).
pub struct FlattenSpec {
    /// The parent field name holding the flattened value (always emitted last).
    pub field: String,
    pub kind: FlattenKind,
}

pub enum FlattenKind {
    /// `#[jsony(flatten)]` over a `HashMap`/`BTreeMap<String, V>`: unknown parent
    /// keys are absorbed into the map on decode and inlined on encode.
    Map(Type<'static>),
    /// `#[jsony(flatten)]` over a companion `#[jsony(Json, Flattenable)]` struct
    /// (emitted as `{parent}_F`): its fields are inlined verbatim into the parent.
    /// JSON-only (the companion derives no binary codec).
    Companion {
        /// The companion type name (`{parent}_F`).
        name: String,
        /// The companion's fields (plain scalars, names `g0..`, keys verbatim).
        fields: Vec<FieldSpec>,
    },
}

impl FlattenSpec {
    /// Whether the flattened value carries (or contains) a float, so the case is
    /// excluded from the exact-string encode oracle (see [`case_has_float`]).
    fn has_float(&self) -> bool {
        match &self.kind {
            FlattenKind::Map(ty) => ty_has_float_pub(*ty),
            FlattenKind::Companion { fields, .. } => {
                fields.iter().any(|f| ty_has_float_pub(f.ty))
            }
        }
    }
}

/// Generate the flatten spec for a parent named struct. `binary` constrains the
/// map flavor (BTreeMap has no binary codec); the companion flavor is JSON-only
/// and is only chosen when the caller has already dropped binary (see
/// `case_from_id`). `parent` names the companion type.
fn gen_flatten(rng: &mut StdRng, parent: &str, binary: bool, allow_companion: bool) -> FlattenSpec {
    // Map flavor (always available) or companion flavor (JSON-only).
    if !allow_companion || rng.gen_bool(0.55) {
        // Flatten map: string-keyed so the inlined keys are `m_<n>` verbatim.
        let ty = if !binary && rng.gen_bool(0.4) {
            Type::BTreeMap(&Type::String, &Type::U32)
        } else if rng.gen_bool(0.5) {
            Type::Map(&Type::String, &Type::I64)
        } else {
            Type::Map(&Type::String, &Type::String)
        };
        FlattenSpec {
            field: "fl".to_string(),
            kind: FlattenKind::Map(ty),
        }
    } else {
        // Companion Flattenable struct with 1..=3 plain scalar fields.
        const SCALARS: &[Type<'static>] = &[Type::U32, Type::I64, Type::Bool, Type::String];
        let n = rng.gen_range(1..=3);
        let fields = (0..n)
            .map(|i| FieldSpec {
                name: format!("g{i}"),
                ty: *SCALARS.choose(rng).unwrap(),
                rename: None,
                alias: None,
                default: DefaultSpec::None,
                skip: false,
                with: false,
                validate: ValidateKind::None,
            })
            .collect();
        FlattenSpec {
            field: "fl".to_string(),
            kind: FlattenKind::Companion {
                name: format!("{parent}_F"),
                fields,
            },
        }
    }
}

/// A `#[jsony(skip_if = ..., default)]` field on a top-level named struct
/// (ToJson-only behavior: the predicate omits the field on *encode*; decode reads
/// it normally, refilling the default when absent). The default is chosen equal
/// to the predicate's trigger value, so a present trigger value round-trips
/// (encode omits → decode refills the same value).
pub struct SkipIfField {
    /// The parent field name, e.g. `sk`.
    pub name: String,
    pub kind: SkipIfKind,
}

#[derive(Clone, Copy)]
pub enum SkipIfKind {
    /// `String` field, `skip_if = str::is_empty` (a path predicate). Trigger: `""`.
    StrIsEmpty,
    /// `u32` field, `skip_if = |v| *v == 0` (a closure predicate). Trigger: `0`.
    IntIsZero,
}

impl SkipIfKind {
    /// The Rust field type spelling.
    pub fn type_str(self) -> &'static str {
        match self {
            SkipIfKind::StrIsEmpty => "String",
            SkipIfKind::IntIsZero => "u32",
        }
    }
    /// The `skip_if = ...` attribute value.
    pub fn predicate(self) -> &'static str {
        match self {
            SkipIfKind::StrIsEmpty => "str::is_empty",
            SkipIfKind::IntIsZero => "|v| *v == 0",
        }
    }
    /// The JSON for the trigger (default) value the predicate fires on.
    fn trigger_json(self) -> &'static str {
        match self {
            SkipIfKind::StrIsEmpty => "\"\"",
            SkipIfKind::IntIsZero => "0",
        }
    }
    /// A JSON value the predicate does NOT fire on (so the field is kept).
    fn kept_json(self) -> &'static str {
        match self {
            SkipIfKind::StrIsEmpty => "\"keep\"",
            SkipIfKind::IntIsZero => "42",
        }
    }
}

/// An asymmetric-`with` field on a top-level named struct: `#[jsony(FromJson with
/// = dt_from_str, ToJson with = dt_to_string)] {name}: u32`. The decode and encode
/// helpers are *different* modules (the split-prefix codegen path). The on-wire
/// form is a JSON **string** holding the decimal; decode parses it, encode renders
/// it back, so it round-trips and the encode oracle proves the string form.
pub struct WithPairField {
    /// The parent field name, e.g. `wp`.
    pub name: String,
}

/// Public float-recursion helper shared by [`case_has_float`] and
/// [`FlattenSpec::has_float`].
fn ty_has_float_pub(ty: Type) -> bool {
    match ty {
        Type::F32 | Type::F64 => true,
        Type::Ref(t)
        | Type::Slice(t)
        | Type::Vec(t)
        | Type::Box(t)
        | Type::Cow(t)
        | Type::Option(t)
        | Type::Array(t, _) => ty_has_float_pub(*t),
        Type::Map(k, v) | Type::BTreeMap(k, v) | Type::TupleVec(k, v) => {
            ty_has_float_pub(*k) || ty_has_float_pub(*v)
        }
        _ => false,
    }
}

pub struct Case {
    pub name: String,
    pub id: u64,
    pub traits: TraitSet,
    pub body: Body,
    /// `#[jsony(transparent)]` — a single-field struct serialized as its inner
    /// value rather than as an object/array.
    pub transparent: bool,
    /// `#[jsony(rename_all = "...")]`. On a struct it recases field keys; on an
    /// enum it recases variant names and (as the fallback rule) struct-variant
    /// field keys.
    pub rename_all: RenameRule,
    /// `#[jsony(rename_all_fields = "...")]`, enum-only. Recases struct-variant
    /// field keys, overriding `rename_all` for fields while leaving variant
    /// names on `rename_all`.
    pub rename_all_fields: RenameRule,
    /// `#[jsony(ignore_tag_adjacent_fields)]`, externally-tagged enums only. Extra
    /// unknown keys in the tag object are ignored on decode rather than rejected.
    /// The sampler couples this: with the flag, an input carrying injected extras
    /// must decode equal to the clean input; without it, the same input is BAD.
    pub ignore_tag_adjacent: bool,
    /// `#[jsony(flatten)]` field on a top-level named struct (at most one). Its
    /// members are inlined into the parent object; the parent `rename_all` does
    /// not recase them. `None` for every other shape.
    pub flatten: Option<FlattenSpec>,
    /// `#[jsony(skip_if = ..., default)]` field on a top-level named struct
    /// (mutually exclusive with `flatten` here). Encode omits it on the trigger
    /// value; decode refills the matching default. `None` otherwise.
    pub skip_if: Option<SkipIfField>,
    /// An asymmetric-`with` field (split `FromJson with`/`ToJson with`) on a named
    /// struct. Mutually exclusive with `flatten`/`skip_if`. `None` otherwise.
    pub with_pair: Option<WithPairField>,
}

impl Case {
    /// The `#[jsony(...)]` container attribute contents.
    pub fn container_attr(&self) -> String {
        // A Str enum derives only `ToStr`/`FromStr` (no JSON/binary traits, no tag).
        if let Body::Str(_) = &self.body {
            let mut s = "ToStr, FromStr".to_string();
            if let Some(v) = self.rename_all.attr_value() {
                s.push_str(&format!(", rename_all = {v:?}"));
            }
            return s;
        }
        let mut s = self.traits.attr_list();
        if self.transparent {
            s.push_str(", transparent");
        }
        if let Body::Enum { repr, .. } = &self.body {
            match repr {
                EnumRepr::External => {}
                EnumRepr::Internal => s.push_str(&format!(", tag = {TAG:?}")),
                EnumRepr::Adjacent => {
                    s.push_str(&format!(", tag = {TAG:?}, content = {CONTENT:?}"))
                }
                EnumRepr::Untagged => s.push_str(", untagged"),
            }
        }
        if self.ignore_tag_adjacent {
            s.push_str(", ignore_tag_adjacent_fields");
        }
        if let Some(v) = self.rename_all.attr_value() {
            s.push_str(&format!(", rename_all = {v:?}"));
        }
        if let Some(v) = self.rename_all_fields.attr_value() {
            s.push_str(&format!(", rename_all_fields = {v:?}"));
        }
        s
    }

    /// Rename rule applied to top-level struct field keys.
    fn struct_field_rule(&self) -> RenameRule {
        self.rename_all
    }
}

/// Self-contained, owned field types that impl Default + the jsony traits.
///
/// `binary` excludes types lacking binary impls (see the jsony-gap note below).
fn type_choices(binary: bool) -> Vec<Type<'static>> {
    use Type::*;
    let mut v = vec![
        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        U64,
        I64,
        U128,
        I128,
        F32,
        F64,
        Bool,
        String,
        Option(&U32),
        Option(&String),
        Option(&Bool),
        Option(&I64),
        Vec(&U32),
        Vec(&String),
        Vec(&Bool),
        Vec(&I64),
        Vec(&F64),
        // Nested containers: the inner type drives a second layer of the
        // generated decode/encode, and each combination still owns its data,
        // impls Default, and impls every jsony trait.
        Vec(&Option(&U32)),
        Vec(&Option(&String)),
        Option(&Vec(&I64)),
        Option(&Vec(&String)),
        Vec(&Vec(&U32)),
        Box(&Vec(&String)),
        Vec(&Box(&U32)),
        Option(&Box(&String)),
        // Fixed-length arrays, including the empty (ZST) case. jsony decodes
        // `[T; N]` element-by-element and requires exactly N. Lengths stay <= 32
        // so `[T; N]: Default` holds (needed when a field draws default/skip).
        Array(&U8, 4),
        Array(&U32, 3),
        Array(&I64, 2),
        Array(&String, 2),
        Array(&Bool, 0),
        Array(&F64, 5),
        // Wide-aligned element (16-byte) stresses the array stride/offset math.
        Array(&U128, 2),
        // Array of `Option` carries `null` holes and a drop-on-error path per
        // element when an inner allocation is live.
        Array(&Option(&U32), 3),
        Array(&Option(&String), 2),
        // Borrowed types (zero-copy decode), all four jsony traits. `&'a str`
        // borrows escape-free input (the sampler keeps `&str` values escape-free);
        // `Cow<'a, str>` borrows or owns. Nestings exercise the borrowed element
        // decode path and the lifetime threading on the container.
        Ref(&Str),          // &'a str
        Cow(&Str),          // Cow<'a, str>
        Option(&Ref(&Str)), // Option<&'a str>
        Option(&Cow(&Str)),
        Vec(&Cow(&Str)),
        // Maps: JSON objects with string-kind keys. `HashMap` has a binary codec,
        // so it joins both pools; numeric keys exercise the quoted-number key path.
        Map(&String, &I64),
        Map(&U32, &Bool),
        Map(&String, &Vec(&U32)),
        // `Vec<(K, V)>`: a positional pair-array (binary-capable). Its object form
        // is reached only via the `object_as_vec_of_tuple` helper / `via`.
        TupleVec(&String, &I64),
        TupleVec(&U32, &U32),
    ];
    // `BTreeMap` implements FromJson/ToJson but has no binary codec (see
    // docs/known-issues.md), so it is restricted to the JSON-only pool.
    if !binary {
        v.push(BTreeMap(&String, &U32));
        v.push(BTreeMap(&I64, &Bool));
    }
    v.push(Box(&U32));
    v.push(Box(&String));
    v
}

fn gen_fields(
    rng: &mut StdRng,
    count: usize,
    attrs: bool,
    binary: bool,
    allow_alias: bool,
    allow_reject: bool,
) -> Vec<FieldSpec> {
    let choices = type_choices(binary);
    let mut fields = Vec::new();
    for i in 0..count {
        let name = format!("f{i}");
        let ty = *choices.choose(rng).unwrap();
        let (mut rename, mut alias, mut skip) = (None, None, false);
        let (mut default, mut with, mut validate) = (DefaultSpec::None, false, ValidateKind::None);
        if attrs {
            if rng.gen_bool(0.15) {
                skip = true;
            } else {
                if rng.gen_bool(0.25) {
                    rename = Some(format!("ren_{name}"));
                }
                if allow_alias && rng.gen_bool(0.25) {
                    alias = Some(format!("al_{name}"));
                }
                if rng.gen_bool(0.25) {
                    // A `Const` default on integers carries an absolute value
                    // check; the other forms exercise the bare and custom-expr
                    // codegen branches.
                    default = if is_int(ty) && rng.gen_bool(0.5) {
                        DefaultSpec::Const
                    } else if rng.gen_bool(0.5) {
                        DefaultSpec::Expr
                    } else {
                        DefaultSpec::Bare
                    };
                }
                // `with` and `validate` are mutually exclusive (jsony rejects
                // both on one field). `with` re-encodes the value as a JSON
                // string via the shipped `json_string` helper, so it stays
                // round-trip-safe for every generated type. The rejecting
                // validators only attach to integer fields in a position where
                // the sampler can also emit a sentinel BAD input (top-level named
                // structs), otherwise the always-`Ok` validator is used.
                match rng.gen_range(0..4) {
                    // `with = json_string` needs `T: for<'a> FromJson<'a>`, which
                    // borrowed types do not satisfy, so only owned fields get it.
                    0 if ty.lifetimes() == 0 => with = true,
                    1 => {
                        validate = if allow_reject && is_int(ty) {
                            match rng.gen_range(0..3) {
                                0 => ValidateKind::Reject,
                                1 => ValidateKind::Require,
                                _ => ValidateKind::Ok,
                            }
                        } else {
                            ValidateKind::Ok
                        };
                    }
                    _ => {}
                }
            }
        }
        fields.push(FieldSpec {
            name,
            ty,
            rename,
            alias,
            default,
            skip,
            with,
            validate,
        });
    }
    fields
}

/// Enable `#[jsony(with = json_string)]` on tuple-position fields with moderate
/// probability. `with` is the field attribute meaningful on positional (unnamed)
/// fields (`rename`/`alias` key only named fields), and the `json_string` helper
/// keeps every generated type round-trip-safe. Codegen supports `with` on both
/// tuple-struct and tuple-variant fields.
fn add_tuple_with(rng: &mut StdRng, fields: &mut [FieldSpec]) {
    for f in fields {
        // `json_string` needs `for<'a> FromJson<'a>`; skip borrowed-type fields.
        if f.ty.lifetimes() == 0 && rng.gen_bool(0.3) {
            f.with = true;
        }
    }
}

/// A single plain field (no rename/alias/default/skip), for transparent structs.
fn gen_plain_field(rng: &mut StdRng, binary: bool) -> FieldSpec {
    let ty = *type_choices(binary).choose(rng).unwrap();
    FieldSpec {
        name: "f0".to_string(),
        ty,
        rename: None,
        alias: None,
        default: DefaultSpec::None,
        skip: false,
        with: false,
        validate: ValidateKind::None,
    }
}

fn gen_variant(rng: &mut StdRng, idx: usize, repr: EnumRepr, binary: bool) -> VariantSpec {
    let name = format!("V{idx}");
    // Internal tagging supports only unit + struct variants.
    let kind = if repr == EnumRepr::Internal {
        if rng.gen_bool(0.5) {
            VarKind::Unit
        } else {
            VarKind::Struct
        }
    } else {
        match rng.gen_range(0..3) {
            0 => VarKind::Unit,
            1 => VarKind::Tuple,
            _ => VarKind::Struct,
        }
    };
    let fields = match kind {
        VarKind::Unit => Vec::new(),
        // Tuple variants currently support exactly one field. `with` is the one
        // attribute valid on the positional field.
        VarKind::Tuple => {
            let mut tf = gen_fields(rng, 1, false, binary, false, false);
            add_tuple_with(rng, &mut tf);
            tf
        }
        VarKind::Struct => {
            let n = rng.gen_range(1..4);
            // alias on enum-variant struct fields is now honored by jsony, so the
            // sampler exercises the alias parse path here too (see the resolved
            // finding in docs/derive-tester.md). Rejecting validators are not used
            // on variant fields (the BAD-input builder targets top-level structs).
            gen_fields(rng, n, true, binary, true, false)
        }
    };
    // Per-variant `#[jsony(rename = "...")]` overrides `rename_all` for the JSON
    // variant name. Applies to every representation (the external key, the inline
    // tag value, the adjacent tag value).
    let rename = if rng.gen_bool(0.25) {
        Some(format!("ren_V{idx}"))
    } else {
        None
    };
    // Per-variant `#[jsony(rename_all = "...")]` recases this variant's own field
    // keys, taking precedence over the container's `rename_all_fields`/`rename_all`.
    // Only meaningful (and only emitted) on struct variants, which have field keys.
    let rename_all = if kind == VarKind::Struct && rng.gen_bool(0.35) {
        RenameRule::random(rng)
    } else {
        RenameRule::None
    };
    VariantSpec {
        name,
        kind,
        fields,
        rename,
        rename_all,
        other: false,
    }
}

/// Generate disjoint variants for an untagged enum.
///
/// Untagged decode tries variants in declaration order and the first
/// structurally-matching one wins, so the round-trip oracle is only valid if no
/// two variants can match the same JSON. Disjointness is guaranteed by
/// construction, independent of declaration order:
///
///   - At most one unit variant, owning the `null` JSON kind (an untagged unit
///     variant decodes from / encodes to `null`).
///   - At most one scalar-newtype variant per remaining JSON kind (`bool` /
///     `number` / `string` / `array`). Each carries a fixed, non-`Option` type
///     so its JSON kind is fixed.
///   - Struct variants get globally-unique field names (prefixed with a
///     per-variant counter) and a required anchor field, so an object encoded
///     for one variant is missing every other variant's required key and carries
///     keys unknown to them. Any earlier struct variant therefore fails to
///     decode it, and matching falls through to the intended variant.
fn gen_untagged_variants(rng: &mut StdRng) -> Vec<VariantSpec> {
    // Scalar-kind newtype slots, each a distinct JSON kind. Non-`Option` so the
    // kind is fixed (an `Option` newtype would also match `null`).
    const SCALAR_SLOTS: &[Type<'static>] = &[
        Type::Bool,            // bool kind
        Type::I64,             // number kind (one number variant only)
        Type::String,          // string kind
        Type::Vec(&Type::U32), // array kind
    ];
    let mut protos: Vec<(VarKind, Vec<FieldSpec>)> = Vec::new();
    // At most one unit variant, owning the `null` JSON kind: an untagged unit
    // variant decodes from `null` and encodes to `null`. Two unit variants would
    // both match `null` and break disjointness. The scalar slots are non-`Option`
    // and struct variants require an object, so nothing else claims `null`.
    if rng.gen_bool(0.5) {
        protos.push((VarKind::Unit, Vec::new()));
    }
    // A random subset of the scalar-kind newtypes, each at most once.
    for ty in SCALAR_SLOTS {
        if rng.gen_bool(0.5) {
            protos.push((
                VarKind::Tuple,
                vec![FieldSpec {
                    name: "f0".to_string(),
                    ty: *ty,
                    rename: None,
                    alias: None,
                    default: DefaultSpec::None,
                    skip: false,
                    with: false,
                    validate: ValidateKind::None,
                }],
            ));
        }
    }
    // 0..=2 struct variants. Fields get prefixed names (assigned below) and a
    // required anchor field, so each struct variant owns a unique key set.
    let n_struct = rng.gen_range(0..3);
    for _ in 0..n_struct {
        let count = rng.gen_range(1..4);
        // No rename/alias: the prefixed declared name is the disjointness anchor,
        // and a `rename` would drop the prefix and risk a cross-variant key
        // collision. `default`/`skip`/`with` on the non-anchor fields stay.
        let mut fields = gen_fields(rng, count, true, false, false, false);
        for f in fields.iter_mut() {
            f.rename = None;
            f.alias = None;
        }
        // Force the first field to be a present, required, untransformed anchor.
        // It MUST be a non-`Option`, non-default, non-skip scalar: an `Option`
        // field is optional even without a `default`, so an all-`Option` struct
        // variant would have no required key and match any object (untagged skips
        // unknown keys), shadowing later variants. A required unique anchor per
        // variant is what makes the disjointness reasoning hold.
        const ANCHOR_TYPES: &[Type<'static>] = &[Type::U32, Type::I64, Type::Bool, Type::String];
        fields[0].ty = *ANCHOR_TYPES.choose(rng).unwrap();
        fields[0].skip = false;
        fields[0].default = DefaultSpec::None;
        fields[0].with = false;
        protos.push((VarKind::Struct, fields));
    }
    // Always at least one variant.
    if protos.is_empty() {
        protos.push((
            VarKind::Tuple,
            vec![FieldSpec {
                name: "f0".to_string(),
                ty: Type::Bool,
                rename: None,
                alias: None,
                default: DefaultSpec::None,
                skip: false,
                with: false,
                validate: ValidateKind::None,
            }],
        ));
    }
    // Declaration order must not affect correctness; shuffle to exercise that.
    protos.shuffle(rng);
    protos
        .into_iter()
        .enumerate()
        .map(|(i, (kind, mut fields))| {
            // Globally-unique field names keep struct variants' key sets disjoint.
            for f in fields.iter_mut() {
                f.name = format!("v{i}_{}", f.name);
            }
            VariantSpec {
                name: format!("V{i}"),
                kind,
                fields,
                rename: None,
                rename_all: RenameRule::None,
                other: false,
            }
        })
        .collect()
}

/// Generate the variants of a `ToStr`/`FromStr` enum: 1..5 fieldless variants,
/// each sometimes carrying an explicit `rename`. Distinct declared names (`V0`,
/// `V1`, ...) and the disjoint `ren_V{i}` rename space keep every variant's string
/// form unique, so `to_str`/`parse` is unambiguous under any `rename_all`.
fn gen_str_enum(rng: &mut StdRng) -> Vec<StrVariant> {
    let n = rng.gen_range(1..5);
    (0..n)
        .map(|i| {
            let name = format!("V{i}");
            let rename = if rng.gen_bool(0.3) {
                Some(format!("ren_V{i}"))
            } else {
                None
            };
            StrVariant { name, rename }
        })
        .collect()
}

/// Field types used in version families: owned, fixed-width, `Default` + binary.
/// (Borrowed `&str` version fields are added once the borrowed-type track exists.)
const VERSION_TYPES: &[Type<'static>] = &[Type::U32, Type::I32, Type::U64, Type::Bool];

/// The Rust spelling of a version field type.
pub fn version_type_str(ty: Type) -> &'static str {
    match ty {
        Type::U32 => "u32",
        Type::I32 => "i32",
        Type::U64 => "u64",
        Type::Bool => "bool",
        _ => unreachable!("non-version field type"),
    }
}

/// A per-field encoded value literal. Distinct per field index so a decode that
/// reads the wrong field's bytes (aliasing) is caught, and distinct from the
/// field's fill value so a present field is distinguishable from a defaulted one.
fn version_value(ty: Type, idx: usize) -> String {
    let i = idx as i64;
    match ty {
        Type::U32 => format!("{}u32", 300 + i * 7),
        Type::I32 => format!("{}i32", -(40 + i * 3)),
        Type::U64 => format!("{}u64", 900_000 + i * 13),
        // Always `true` so it differs from the `false` Default-fill, keeping the
        // defaulted-vs-present distinction observable for bool fields.
        Type::Bool => "true".to_string(),
        _ => unreachable!("non-version field type"),
    }
}

/// A per-field explicit `default` literal, distinct per field and from
/// [`version_value`], so an absolute "this exact default was filled" check holds.
fn version_default(ty: Type, idx: usize) -> String {
    let i = idx as i64;
    match ty {
        Type::U32 => format!("{}u32", 100 + i),
        Type::I32 => format!("{}i32", -(100 + i)),
        Type::U64 => format!("{}u64", 500 + i),
        Type::Bool => "false".to_string(),
        _ => unreachable!("non-version field type"),
    }
}

/// Build a binary version family: a shared field plan (base fields at `intro = 0`,
/// then at least one field introduced at each version `1..=nversions`) instantiated
/// as schemas `S_0..S_nversions` (each accepting `[0, k]` via the auto `version`)
/// plus one min-windowed schema (`version = M..`, accepting `[M, nversions]`).
fn gen_version_family(rng: &mut StdRng) -> VersionFamily {
    let tuple = rng.gen_bool(0.5);
    let nversions: u16 = rng.gen_range(1..=3);
    let nbase = rng.gen_range(1..=2);
    let mut fields: Vec<VersionField> = Vec::new();
    let mut idx = 0usize;
    for _ in 0..nbase {
        let ty = *VERSION_TYPES.choose(rng).unwrap();
        fields.push(VersionField {
            ty,
            intro: 0,
            value: version_value(ty, idx),
            default: None,
        });
        idx += 1;
    }
    for v in 1..=nversions {
        let cnt = rng.gen_range(1..=2);
        for _ in 0..cnt {
            let ty = *VERSION_TYPES.choose(rng).unwrap();
            // Mix explicit `default = ..` fields with bare versioned fields (which
            // fall back to `Default::default()`), exercising both fill paths.
            let default = if rng.gen_bool(0.6) {
                Some(version_default(ty, idx))
            } else {
                None
            };
            fields.push(VersionField {
                ty,
                intro: v,
                value: version_value(ty, idx),
                default,
            });
            idx += 1;
        }
    }
    let mut schemas: Vec<VersionSchema> = (0..=nversions)
        .map(|cur| VersionSchema { min: 0, cur })
        .collect();
    // A min-windowed schema accepting `[M, nversions]`, so encodings below `M`
    // must be rejected.
    let m = rng.gen_range(1..=nversions);
    schemas.push(VersionSchema {
        min: m,
        cur: nversions,
    });
    VersionFamily {
        tuple,
        fields,
        schemas,
    }
}

/// `Type::Array` of `scalar` with `n` elements, using a `'static` element
/// reference (const-promoted), so POD families can carry fixed arrays.
fn pod_array(scalar: Type, n: usize) -> Type<'static> {
    match scalar {
        Type::U8 => Type::Array(&Type::U8, n),
        Type::I8 => Type::Array(&Type::I8, n),
        Type::U16 => Type::Array(&Type::U16, n),
        Type::I16 => Type::Array(&Type::I16, n),
        Type::U32 => Type::Array(&Type::U32, n),
        Type::I32 => Type::Array(&Type::I32, n),
        Type::F32 => Type::Array(&Type::F32, n),
        Type::U64 => Type::Array(&Type::U64, n),
        Type::I64 => Type::Array(&Type::I64, n),
        Type::F64 => Type::Array(&Type::F64, n),
        _ => unreachable!("non-POD scalar"),
    }
}

/// A finite, in-range Rust literal for a POD scalar, distinct per `n` so a decode
/// that crosses field/stride boundaries is caught. Floats stay finite (NaN would
/// break the `PartialEq` round-trip even though the bytes copy exactly).
fn pod_scalar_value(scalar: Type, n: usize) -> String {
    let n = n as i64;
    match scalar {
        Type::U8 => format!("{}u8", (n % 200) + 1),
        Type::I8 => format!("{}i8", (n % 100) + 1),
        Type::U16 => format!("{}u16", 1000 + (n % 5000)),
        Type::I16 => format!("{}i16", -(1000 + (n % 5000))),
        Type::U32 => format!("{}u32", 100_000 + n),
        Type::I32 => format!("{}i32", -(100_000 + n)),
        Type::F32 => format!("{}.5f32", n),
        Type::U64 => format!("{}u64", 10_000_000 + n),
        Type::I64 => format!("{}i64", -(10_000_000 + n)),
        Type::F64 => format!("{}.25f64", n),
        _ => unreachable!("non-POD scalar"),
    }
}

/// A literal for a POD field (scalar or fixed array), seeded by `n`.
pub fn pod_value(ty: Type, n: usize) -> String {
    match ty {
        Type::Array(elem, len) => {
            let mut s = String::from("[");
            for k in 0..len {
                if k > 0 {
                    s.push_str(", ");
                }
                s.push_str(&pod_scalar_value(*elem, n * 16 + k));
            }
            s.push(']');
            s
        }
        scalar => pod_scalar_value(scalar, n),
    }
}

/// Build a `zerocopy` POD family. All fields share one alignment class
/// (`1`/`2`/`4`/`8` bytes), so a `repr(C)` layout has no internal gaps and the
/// total size is a multiple of the class — i.e. no padding, so the `POD`
/// const-asserts hold and the raw bytes round-trip.
fn gen_pod_family(rng: &mut StdRng) -> PodFamily {
    let class = *[1usize, 2, 4, 8].choose(rng).unwrap();
    let scalars: &[Type<'static>] = match class {
        1 => &[Type::U8, Type::I8],
        2 => &[Type::U16, Type::I16],
        4 => &[Type::U32, Type::I32, Type::F32],
        _ => &[Type::U64, Type::I64, Type::F64],
    };
    let nfields = rng.gen_range(2..=5);
    let mut fields = Vec::new();
    for i in 0..nfields {
        let scalar = *scalars.choose(rng).unwrap();
        let ty = if rng.gen_bool(0.3) {
            pod_array(scalar, rng.gen_range(1..=4))
        } else {
            scalar
        };
        fields.push(PodField {
            ty,
            value: pod_value(ty, i + 1),
        });
    }
    PodFamily { fields }
}

/// SplitMix64 finalizer. Sequential ids (`0, 1, 2, ...`) seeded directly into
/// `StdRng` produce correlated streams (nearby seeds share early draws), which
/// biases sampling and, for small member counts, makes shuffles collapse to the
/// identity. Mixing the id first decorrelates the streams while keeping the
/// mapping a pure function of the id, so `case <id>` still reproduces exactly.
pub fn mix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^ (x >> 31)
}

/// Build the deterministic case for `id`.
pub fn case_from_id(id: u64) -> Case {
    let mut rng = StdRng::seed_from_u64(mix64(id));
    let binary = rng.gen_bool(0.5);
    // JSON trait direction. `Both` is the common path. `FromJson`-only is a
    // distinct codegen path the symmetric case never reaches, so generate it
    // sometimes. (`ToJson`-only needs a value source independent of decoding and
    // is left for the value-literal oracle.)
    let from_json_only = rng.gen_bool(0.2);
    let mut traits = TraitSet {
        from_json: true,
        to_json: !from_json_only,
        from_binary: binary,
        to_binary: binary,
    };
    let mut transparent = false;
    let body = match rng.gen_range(0..17) {
        16 => {
            // A self-contained with-helper / binary-rejection case.
            traits = TraitSet {
                from_json: false,
                to_json: false,
                from_binary: false,
                to_binary: false,
            };
            Body::Helper(gen_helper_kind(&mut rng))
        }
        15 => {
            // A ToJson-only `via = Iterator` family: self-contained encode oracle.
            traits = TraitSet {
                from_json: false,
                to_json: false,
                from_binary: false,
                to_binary: false,
            };
            Body::ViaIter(gen_via_family(&mut rng))
        }
        14 => {
            // A `zerocopy` POD struct: manages its own `#[jsony(Binary, zerocopy)]
            // #[repr(C)]` derive and is checked by the dedicated alignment oracle.
            traits = TraitSet {
                from_json: false,
                to_json: false,
                from_binary: false,
                to_binary: false,
            };
            Body::Pod(gen_pod_family(&mut rng))
        }
        13 => {
            // A binary version family: manages its own per-schema `#[jsony(Binary,
            // version ...)]` derives and is checked by the dedicated cross-version
            // oracle, so the case-level traits are unused.
            traits = TraitSet {
                from_json: false,
                to_json: false,
                from_binary: false,
                to_binary: false,
            };
            Body::Version(gen_version_family(&mut rng))
        }
        12 => {
            // A `ToStr`/`FromStr` enum: no JSON/binary traits (none of the round
            // trip applies), checked by the dedicated string oracle.
            traits = TraitSet {
                from_json: false,
                to_json: false,
                from_binary: false,
                to_binary: false,
            };
            Body::Str(gen_str_enum(&mut rng))
        }
        0..=3 => {
            let n = rng.gen_range(1..6);
            Body::Named(gen_fields(&mut rng, n, true, binary, true, true))
        }
        4..=5 => {
            let n = rng.gen_range(1..4);
            // `with` applies to positional tuple-struct fields the same as to tuple
            // variants: the transform runs on both encode and decode, and a
            // single-field newtype reports the AnyValue marker (the `with` output
            // is opaque) rather than the inner type's marker.
            let mut tf = gen_fields(&mut rng, n, false, binary, false, false);
            add_tuple_with(&mut rng, &mut tf);
            Body::Tuple(tf)
        }
        6 => Body::Unit,
        7 => {
            // Transparent named newtype: serialized as the inner value.
            // Restricted to JSON traits: transparent binary additionally
            // requires the inner type be Pod, which most generated types are not.
            transparent = true;
            traits.from_binary = false;
            traits.to_binary = false;
            Body::Named(vec![gen_plain_field(&mut rng, false)])
        }
        _ => {
            let repr = match rng.gen_range(0..5) {
                0 => EnumRepr::Internal,
                1 => EnumRepr::Adjacent,
                2 => EnumRepr::Untagged,
                _ => EnumRepr::External,
            };
            if repr == EnumRepr::Untagged {
                // Untagged is a JSON-only tag mode; the binary codec encodes a
                // discriminant regardless, so restrict to JSON to keep the
                // generator's disjointness reasoning to the JSON side.
                traits.from_binary = false;
                traits.to_binary = false;
                Body::Enum {
                    repr,
                    variants: gen_untagged_variants(&mut rng),
                }
            } else {
                let n = rng.gen_range(1..4);
                let mut variants: Vec<VariantSpec> = (0..n)
                    .map(|i| gen_variant(&mut rng, i, repr, binary))
                    .collect();
                // Optionally designate one unit variant the FromJson catch-all `other`.
                // Requires another variant to remain (a meaningful enum keeps at least
                // one concrete arm), so only when there are at least two variants.
                if variants.len() >= 2 && rng.gen_bool(0.3) {
                    if let Some(v) = variants.iter_mut().find(|v| v.kind == VarKind::Unit) {
                        v.other = true;
                    }
                }
                Body::Enum { repr, variants }
            }
        }
    };
    // `rename_all` recases keys, so it only matters for shapes that emit keys.
    // Transparent structs serialize as their inner value (no keys), tuple/unit
    // structs are positional, so it is restricted to named structs and enums.
    let (mut rename_all, mut rename_all_fields) = (RenameRule::None, RenameRule::None);
    match &body {
        Body::Named(_) if !transparent => {
            if rng.gen_bool(0.35) {
                rename_all = RenameRule::random(&mut rng);
            }
        }
        Body::Enum { .. } => {
            if rng.gen_bool(0.35) {
                rename_all = RenameRule::random(&mut rng);
            }
            if rng.gen_bool(0.35) {
                rename_all_fields = RenameRule::random(&mut rng);
            }
        }
        // A Str enum's variant strings follow `rename_all`; set it often so the
        // recasing paths are exercised against `to_str`/`parse`.
        Body::Str(_) => {
            if rng.gen_bool(0.6) {
                rename_all = RenameRule::random(&mut rng);
            }
        }
        _ => {}
    }
    // `ignore_tag_adjacent_fields` only applies to externally-tagged enums (the
    // adjacent-key skip is part of the external-tag decode loop). Set it on a
    // fraction of them; the sampler emits the injected EQ input when set.
    //
    // It composes with an `other` variant: in the object decode loop the
    // adjacent-skip takes priority (unknown keys are skipped to reach the tag),
    // while the `other` catch-all still absorbs an unknown bare-string tag via
    // the stringly path.
    let ignore_tag_adjacent = matches!(
        &body,
        Body::Enum {
            repr: EnumRepr::External,
            ..
        }
    ) && rng.gen_bool(0.4);
    // `#[jsony(flatten)]` on a top-level named (non-transparent) struct. The
    // companion flavor is JSON-only (its companion derives no binary codec), so
    // choosing it drops the parent's binary traits. A recasing `rename_all` (set
    // independently above) is the point of the flatten encode oracle: it must
    // leave the inlined keys verbatim.
    let flatten = if matches!(&body, Body::Named(_)) && !transparent && rng.gen_bool(0.3) {
        let fl = gen_flatten(&mut rng, &format!("T{id}"), traits.binary(), true);
        if let FlattenKind::Companion { .. } = &fl.kind {
            traits.from_binary = false;
            traits.to_binary = false;
        }
        Some(fl)
    } else {
        None
    };
    // `skip_if` (ToJson-only behavior) on a named struct, mutually exclusive with
    // flatten so the appended-field ordering stays simple. JSON-only: `skip_if` is
    // an encode-side attribute and is not part of the binary codec.
    let skip_if = if flatten.is_none()
        && matches!(&body, Body::Named(_))
        && !transparent
        && traits.to_json
        && rng.gen_bool(0.25)
    {
        traits.from_binary = false;
        traits.to_binary = false;
        let kind = if rng.gen_bool(0.5) {
            SkipIfKind::StrIsEmpty
        } else {
            SkipIfKind::IntIsZero
        };
        Some(SkipIfField {
            name: "sk".to_string(),
            kind,
        })
    } else {
        None
    };
    // Asymmetric `with` field (JSON-only split-prefix path), mutually exclusive
    // with the other appended-field features.
    let with_pair = if flatten.is_none()
        && skip_if.is_none()
        && matches!(&body, Body::Named(_))
        && !transparent
        && traits.to_json
        && rng.gen_bool(0.2)
    {
        traits.from_binary = false;
        traits.to_binary = false;
        Some(WithPairField {
            name: "wp".to_string(),
        })
    } else {
        None
    };
    Case {
        name: format!("T{id}"),
        id,
        traits,
        body,
        transparent,
        rename_all,
        rename_all_fields,
        ignore_tag_adjacent,
        flatten,
        skip_if,
        with_pair,
    }
}

// --- Input sampling (coupled with the emitted attributes) ---
//
// Sampling builds a small structural JSON tree (`Node`) whose leaves are
// pre-rendered field-value text from `datagen`. Field values never contain JSON
// objects (no struct types appear as field types), so the only reorderable
// members are the structural objects this module builds. Rendering the one tree
// several ways yields inputs that must all decode to the same value (the
// permutation and whitespace variants) plus a duplicate-key input that must
// fail cleanly. Building the tree once keeps every rendering's keys and values
// identical, so any decode difference between them is a real jsony bug rather
// than a sampling divergence.

/// A structural JSON node. `Raw` is opaque pre-rendered text (a scalar, string,
/// array, or `null`); `Object` is a member list whose ordering and duplication
/// the renderers vary.
enum Node {
    Raw(String),
    Object(Vec<(String, Node)>),
}

/// JSON-encode one field value to a string via the shared `datagen` encoder.
fn raw_value(ty: Type, rand: &mut Rand) -> String {
    let mut out = TextWriter::new();
    ty.json_encode(rand, &mut out);
    out.into_string()
}

/// JSON-encode a string literal (object keys and variant-name tag values).
fn json_string(s: &str) -> String {
    let mut out = TextWriter::new();
    s.encode_json__jsony(&mut out);
    out.into_string()
}

/// Build an object node for a set of named fields, honouring rename / alias /
/// default / skip. `default` fields may be omitted, `skip` fields are always
/// omitted. `rule` recases each key (the active `rename_all`/`rename_all_fields`).
fn build_named_object(fields: &[FieldSpec], rule: RenameRule, rand: &mut Rand) -> Node {
    let mut members = Vec::new();
    for field in fields {
        if field.skip {
            continue;
        }
        // Under `full` (the encode oracle) every defaultable field is present, so
        // the re-encoded value carries exactly these members.
        if field.default.is_set() && !rand.full && rand.rng.gen_bool(0.5) {
            continue;
        }
        let key = field.input_key(rule, rand);
        let value = field.sample_value(rand);
        members.push((key, Node::Raw(value)));
    }
    Node::Object(members)
}

/// The members a flattened field contributes to its parent object: `(key,
/// value-json)` pairs with **verbatim** keys (never recased by the parent
/// `rename_all`). Under `full` (the encode oracle) the map flavor emits exactly
/// one entry so the inlined re-encode is order-stable.
fn flatten_members(fl: &FlattenSpec, rand: &mut Rand) -> Vec<(String, String)> {
    match &fl.kind {
        FlattenKind::Map(ty) => {
            let v = match ty {
                Type::Map(_, v) | Type::BTreeMap(_, v) => **v,
                _ => unreachable!("flatten map is a Map/BTreeMap"),
            };
            let n = if rand.full {
                1
            } else {
                rand.rng.gen_range(0..=2usize)
            };
            (0..n)
                .map(|i| (format!("m_{i}"), raw_value(v, rand)))
                .collect()
        }
        // Companion fields are plain (no rename/alias/default), so the key is the
        // declared name verbatim and the value is the field's sampled value.
        FlattenKind::Companion { fields, .. } => fields
            .iter()
            .map(|f| (f.name.clone(), f.sample_value(rand)))
            .collect(),
    }
}

/// Append a case's flattened members onto an object's member list (the parent
/// keys come first; the flatten field is declared last, so its inlined keys
/// follow). The keys are verbatim — applying the parent's `rename_all` here would
/// be the bug the encode oracle is built to catch.
fn append_flatten(node: &mut Node, case: &Case, rand: &mut Rand) {
    let Some(fl) = &case.flatten else { return };
    if let Node::Object(members) = node {
        for (k, v) in flatten_members(fl, rand) {
            members.push((k, Node::Raw(v)));
        }
    }
}

/// Append the `skip_if` field's member to the parent object (non-`full` only).
/// Any value round-trips: a non-trigger value is kept by encode; a trigger value
/// is omitted by encode and refilled by the matching default on decode. The key
/// IS recased by the parent `rename_all` (a `skip_if` field is an ordinary field).
/// Omitted under `full`, where the single-column encode oracle wants the field
/// absent so the re-encode (which omits on the default) matches.
fn append_skip_if(node: &mut Node, case: &Case, rand: &mut Rand) {
    let Some(sk) = &case.skip_if else { return };
    if rand.full {
        return;
    }
    if let Node::Object(members) = node {
        let key = case.struct_field_rule().apply_to_field(&sk.name);
        let value = match sk.kind {
            SkipIfKind::StrIsEmpty => raw_value(Type::String, rand),
            SkipIfKind::IntIsZero => raw_value(Type::U32, rand),
        };
        members.push((key, Node::Raw(value)));
    }
}

/// Append the asymmetric-`with` field's member: a JSON **string** holding a
/// decimal (decode parses it to `u32`, encode renders it back). Present in both
/// `full` and non-`full` (the form is symmetric, so the single-column encode
/// oracle proves the string round-trips). The key is recased by `rename_all`.
fn append_with_pair(node: &mut Node, case: &Case, rand: &mut Rand) {
    let Some(wp) = &case.with_pair else { return };
    if let Node::Object(members) = node {
        let key = case.struct_field_rule().apply_to_field(&wp.name);
        let n: u32 = rand.rng.gen();
        members.push((key, Node::Raw(format!("\"{n}\""))));
    }
}

fn build_variant_content(v: &VariantSpec, rule: RenameRule, rand: &mut Rand) -> Node {
    match v.kind {
        VarKind::Unit => Node::Raw("null".to_string()),
        VarKind::Tuple => Node::Raw(v.fields[0].sample_value(rand)),
        VarKind::Struct => build_named_object(&v.fields, rule, rand),
    }
}

fn build_struct_body(case: &Case, rand: &mut Rand) -> Node {
    if case.transparent {
        // Single-field newtype: serialized as the inner value.
        if let Body::Named(fields) = &case.body {
            return Node::Raw(raw_value(fields[0].ty, rand));
        }
    }
    match &case.body {
        Body::Named(fields) => {
            let mut node = build_named_object(fields, case.struct_field_rule(), rand);
            append_flatten(&mut node, case, rand);
            append_skip_if(&mut node, case, rand);
            append_with_pair(&mut node, case, rand);
            node
        }
        Body::Tuple(fields) => {
            if fields.len() == 1 {
                // Newtype: serializes transparently as the inner value (which the
                // field's `with` transform, if any, wraps).
                Node::Raw(fields[0].sample_value(rand))
            } else {
                // Multi-field tuple struct: a positional JSON array. Each element
                // is the field's sampled value, honouring its `with` transform.
                let mut out = String::from("[");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push(',');
                    }
                    out.push_str(&field.sample_value(rand));
                }
                out.push(']');
                Node::Raw(out)
            }
        }
        // jsony serializes a unit struct as an empty object.
        Body::Unit => Node::Raw("{}".to_string()),
        Body::Enum { .. } => unreachable!("handled in build_node"),
        Body::Str(_) => unreachable!("str enums are checked by the dedicated arm, not sampled"),
        Body::Version(_) => unreachable!("version families are checked by the dedicated arm"),
        Body::Pod(_) => unreachable!("POD families are checked by the dedicated arm"),
        Body::ViaIter(_) => unreachable!("via=Iterator families are checked by the dedicated arm"),
        Body::Helper(_) => unreachable!("helper families are checked by the dedicated arm"),
    }
}

/// The exact JSON `to_json` must produce for a `via = Iterator` family: the
/// leading `a` field, then the flattened pairs inline in insertion order with
/// duplicate keys preserved. Keys are simple alphabetic, so no escaping is needed.
pub fn via_expected(fam: &ViaFamily) -> String {
    use std::fmt::Write as _;
    let mut s = format!("{{\"a\":{}", fam.lead);
    for (k, v) in &fam.pairs {
        let _ = write!(s, ",\"{k}\":{v}");
    }
    s.push('}');
    s
}

fn build_enum(case: &Case, repr: EnumRepr, variants: &[VariantSpec], rand: &mut Rand) -> Node {
    let v = &variants[rand.rng.gen_range(0..variants.len())];
    // Variant names follow an explicit per-variant `rename`, else `rename_all`;
    // struct-variant field keys follow the per-variant `rename_all`, then
    // `rename_all_fields`, then `rename_all` (see `VariantSpec::field_rule`).
    let name = v.json_name(case.rename_all);
    let field_rule = v.field_rule(case);
    match repr {
        EnumRepr::External => match v.kind {
            VarKind::Unit => Node::Raw(json_string(&name)),
            VarKind::Tuple | VarKind::Struct => {
                Node::Object(vec![(name, build_variant_content(v, field_rule, rand))])
            }
        },
        EnumRepr::Internal => {
            let mut members = vec![(TAG.to_string(), Node::Raw(json_string(&name)))];
            if v.kind == VarKind::Struct {
                // Internal tagging inlines the struct fields beside the tag.
                if let Node::Object(inner) = build_named_object(&v.fields, field_rule, rand) {
                    members.extend(inner);
                }
            }
            Node::Object(members)
        }
        EnumRepr::Adjacent => {
            let mut members = vec![(TAG.to_string(), Node::Raw(json_string(&name)))];
            if v.kind != VarKind::Unit {
                members.push((
                    CONTENT.to_string(),
                    build_variant_content(v, field_rule, rand),
                ));
            }
            Node::Object(members)
        }
        // No tag wrapper: the chosen variant's content is the whole value. A unit
        // variant is `null`, a newtype is its inner value, a struct is a bare
        // object. Disjointness (see `gen_untagged_variants`) guarantees this value
        // round-trips back to the same variant.
        EnumRepr::Untagged => build_variant_content(v, field_rule, rand),
    }
}

fn build_node(case: &Case, rand: &mut Rand) -> Node {
    match &case.body {
        Body::Enum { repr, variants } => build_enum(case, *repr, variants, rand),
        _ => build_struct_body(case, rand),
    }
}

/// Render the canonical compact form: members in declaration order.
fn render_canonical(node: &Node, out: &mut String) {
    match node {
        Node::Raw(s) => out.push_str(s),
        Node::Object(members) => {
            out.push('{');
            for (i, (k, v)) in members.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                out.push_str(&json_string(k));
                out.push(':');
                render_canonical(v, out);
            }
            out.push('}');
        }
    }
}

/// Render with every object's members in a shuffled order. A conforming decoder
/// must produce the same value as the canonical order: membership is keyed by
/// name, and the order also shifts which member the parser reads through its
/// first-key versus subsequent-key paths.
fn render_permuted(node: &Node, rng: &mut StdRng, out: &mut String) {
    match node {
        Node::Raw(s) => out.push_str(s),
        Node::Object(members) => {
            let mut order: Vec<usize> = (0..members.len()).collect();
            order.shuffle(rng);
            // Force a real reordering when the shuffle lands on the identity (for
            // these structured seeds and small member counts it often does), so
            // the permutation invariant is actually exercised.
            if members.len() >= 2 && order.iter().enumerate().all(|(i, &v)| i == v) {
                order.rotate_left(1);
            }
            out.push('{');
            for (j, &i) in order.iter().enumerate() {
                if j > 0 {
                    out.push(',');
                }
                out.push_str(&json_string(&members[i].0));
                out.push(':');
                render_permuted(&members[i].1, rng, out);
            }
            out.push('}');
        }
    }
}

/// Render with insignificant spaces injected around structural tokens. A
/// conforming decoder must ignore them and produce the canonical value. Only
/// spaces are used (the record format is tab-delimited and line-based, so tabs
/// and newlines are unavailable) and only at object boundaries (`Raw` leaves are
/// opaque), exercising the object whitespace-skipping path.
fn render_spaced(node: &Node, rng: &mut StdRng, out: &mut String) {
    fn sp(rng: &mut StdRng, out: &mut String) {
        for _ in 0..rng.gen_range(0..3) {
            out.push(' ');
        }
    }
    match node {
        Node::Raw(s) => out.push_str(s),
        Node::Object(members) => {
            out.push('{');
            sp(rng, out);
            for (i, (k, v)) in members.iter().enumerate() {
                if i > 0 {
                    sp(rng, out);
                    out.push(',');
                    sp(rng, out);
                }
                out.push_str(&json_string(k));
                sp(rng, out);
                out.push(':');
                sp(rng, out);
                render_spaced(v, rng, out);
            }
            sp(rng, out);
            out.push('}');
        }
    }
}

/// Render the canonical form with the top-level object's first member emitted
/// twice (same key and value). jsony rejects duplicate struct keys, so this is a
/// `BAD` input that must fail cleanly. The first occurrence decodes before the
/// duplicate is detected, so it drives the error-path drop of an already-built
/// (possibly heap-owning) field, feeding the leak check. Returns `None` when the
/// top-level node is not an object with at least one member.
fn render_dup(node: &Node) -> Option<String> {
    let Node::Object(members) = node else {
        return None;
    };
    let (k0, v0) = members.first()?;
    let mut out = String::new();
    out.push('{');
    out.push_str(&json_string(k0));
    out.push(':');
    render_canonical(v0, &mut out);
    for (k, v) in members {
        out.push(',');
        out.push_str(&json_string(k));
        out.push(':');
        render_canonical(v, &mut out);
    }
    out.push('}');
    Some(out)
}

/// Render an externally-tagged enum object with extra unknown keys injected
/// around the variant tag. The injected keys never collide with a variant name
/// (no variant name contains `$`), and their values exercise scalar, nested-array,
/// and nested-object skipping. Returns `None` unless `node` is an object with at
/// least one member (i.e. a tuple/struct variant; unit variants render as a bare
/// string and have no object to inject into). With `ignore_tag_adjacent_fields`
/// set the result must decode equal to the canonical input, otherwise it is a BAD
/// input that must error cleanly.
fn render_adjacent(node: &Node, rng: &mut StdRng) -> Option<String> {
    let Node::Object(members) = node else {
        return None;
    };
    if members.is_empty() {
        return None;
    }
    const EXTRAS: [(&str, &str); 3] = [
        ("$adj0", "12345"),
        ("$adj1", "[1,[2,3],{\"k\":[4,5]}]"),
        ("$adj2", "{\"a\":1,\"b\":[true,null,{\"c\":2}]}"),
    ];
    // How many extras to inject, and how many of them precede the tag (the rest
    // follow it). Covers before-only, after-only, and straddling placements.
    let count = rng.gen_range(1..=EXTRAS.len());
    let before = rng.gen_range(0..=count);
    let mut out = String::from("{");
    let mut first = true;
    let push_extra = |out: &mut String, first: &mut bool, k: &str, v: &str| {
        if !*first {
            out.push(',');
        }
        *first = false;
        out.push_str(&json_string(k));
        out.push(':');
        out.push_str(v);
    };
    for &(k, v) in &EXTRAS[..before] {
        push_extra(&mut out, &mut first, k, v);
    }
    for (k, v) in members {
        if !first {
            out.push(',');
        }
        first = false;
        out.push_str(&json_string(k));
        out.push(':');
        render_canonical(v, &mut out);
    }
    for &(k, v) in &EXTRAS[before..count] {
        push_extra(&mut out, &mut first, k, v);
    }
    out.push('}');
    Some(out)
}

/// Whether the case is an externally-tagged enum (the only shape `render_adjacent`
/// and `ignore_tag_adjacent_fields` apply to).
fn external_tagged(case: &Case) -> bool {
    matches!(
        &case.body,
        Body::Enum {
            repr: EnumRepr::External,
            ..
        }
    )
}

/// Render a member list as a compact JSON object. Values are already raw JSON
/// text, keys are JSON-string-encoded.
fn render_members(members: &[(String, String)]) -> String {
    let mut out = String::from("{");
    for (i, (k, v)) in members.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        out.push_str(&json_string(k));
        out.push(':');
        out.push_str(v);
    }
    out.push('}');
    out
}

/// Build the absolute default-value check for a top-level named struct that has
/// a `Const`-default field: a pair `(present, omitted)` of object inputs that
/// differ only in that one member. `present` sets it to [`DEFAULT_CONST`];
/// `omitted` drops it, forcing jsony to fill the default. A conforming decoder
/// produces the same value from both, so they go in as an `EQ` pair. This
/// asserts the *value* jsony fills, which round-trip alone cannot observe (a
/// wrong-but-consistent default round-trips fine). `None` unless the case is a
/// non-transparent named struct with such a field.
fn build_default_eq(case: &Case, seed: u64) -> Option<(String, String)> {
    if case.transparent {
        return None;
    }
    let Body::Named(fields) = &case.body else {
        return None;
    };
    let target = fields
        .iter()
        .position(|f| !f.skip && f.default == DefaultSpec::Const)?;
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(seed ^ 0x5EED_0DEF_A017_C0DE)),
        steam: 95,
        full: false,
    };
    let rule = case.struct_field_rule();
    let mut members = Vec::new();
    for (i, field) in fields.iter().enumerate() {
        if field.skip {
            continue;
        }
        let value = if i == target {
            field.with_wrap(DEFAULT_CONST.to_string())
        } else {
            field.sample_value(&mut rand)
        };
        members.push((field.key(rule), value));
    }
    // Include the flattened / with-pair members so the input is structurally
    // complete (companion and with-pair fields are required); they are identical
    // in both renderings, so the EQ assertion still isolates the target field.
    if let Some(fl) = &case.flatten {
        members.extend(flatten_members(fl, &mut rand));
    }
    if let Some(wp) = &case.with_pair {
        members.push((rule.apply_to_field(&wp.name), format!("\"{}\"", rand.rng.gen::<u32>())));
    }
    let present = render_members(&members);
    members.remove(
        members
            .iter()
            .position(|(k, _)| *k == fields[target].key(rule))?,
    );
    let omitted = render_members(&members);
    Some((present, omitted))
}

/// Build a rejecting-validator BAD input: a structurally-complete top-level
/// object whose one rejecting-validate field is set to [`VALIDATE_SENTINEL`]. A
/// conforming decoder runs the validator on that field and aborts the decode
/// cleanly, dropping the already-built earlier fields (the partial-init drop
/// path). All other fields take ordinary, sentinel-free values, so the *only*
/// reason the input fails is the validator firing. `None` unless the case is a
/// non-transparent named struct with such a field.
///
/// The rejecting assertion is JSON-only (the input is JSON). The binary validate
/// path is exercised for non-corruption by the round trip, which only ever feeds
/// it sentinel-free values, so binary rejection is not separately asserted.
fn build_validate_reject(case: &Case, seed: u64) -> Option<String> {
    if case.transparent {
        return None;
    }
    let Body::Named(fields) = &case.body else {
        return None;
    };
    let target = fields
        .iter()
        .position(|f| !f.skip && f.validate.rejects())?;
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(seed ^ 0x5EED_BAD0_DEFA_17ED)),
        steam: 95,
        full: false,
    };
    let rule = case.struct_field_rule();
    let mut members = Vec::new();
    for (i, field) in fields.iter().enumerate() {
        if field.skip {
            continue;
        }
        let value = if i == target {
            VALIDATE_SENTINEL.to_string()
        } else {
            field.sample_value(&mut rand)
        };
        members.push((field.key(rule), value));
    }
    // Include flattened / with-pair members so the input is structurally complete
    // and the only decode failure is the validator firing on the sentinel.
    if let Some(fl) = &case.flatten {
        members.extend(flatten_members(fl, &mut rand));
    }
    if let Some(wp) = &case.with_pair {
        members.push((rule.apply_to_field(&wp.name), format!("\"{}\"", rand.rng.gen::<u32>())));
    }
    Some(render_members(&members))
}

/// Build the `other`-variant decode check: a pair `(known, unknown)` that must
/// both decode to the catch-all `other` variant. `known` carries the other
/// variant's own tag name (a known name that maps to it directly), `unknown`
/// carries a tag no declared variant uses (which the `other` rule must absorb).
/// A conforming decoder yields the same value from both, so they go in as an EQ
/// pair. This is what proves the catch-all actually fires: round-trip alone never
/// presents an unknown tag. `None` unless the enum has an `other` variant.
fn build_other_eq(case: &Case) -> Option<(String, String)> {
    let Body::Enum { repr, variants } = &case.body else {
        return None;
    };
    let other = variants.iter().find(|v| v.other)?;
    let known = other.json_name(case.rename_all);
    let unknown = "__dt_unknown_tag__";
    let render = |tag: &str| -> String {
        match repr {
            EnumRepr::External => json_string(tag),
            EnumRepr::Internal | EnumRepr::Adjacent => {
                render_members(&[(TAG.to_string(), json_string(tag))])
            }
            // Untagged enums never carry an `other` variant, so `build_other_eq`
            // returns above via the `?` before this closure is ever called.
            EnumRepr::Untagged => unreachable!("untagged enums have no other variant"),
        }
    };
    Some((render(&known), render(unknown)))
}

/// Renderings of one sampled value: the canonical input plus structural variants
/// for the soundness checks.
pub struct Sample {
    /// Canonical compact JSON (the well-formed `o` input).
    pub canonical: String,
    /// Same value, object members shuffled. Must decode equal to `canonical`.
    pub permuted: String,
    /// Same value, spaces injected. Must decode equal to `canonical`.
    pub spaced: String,
    /// Duplicate-key variant. Must fail cleanly. `None` when inapplicable.
    pub dup: Option<String>,
    /// Absolute default-value check: `(field present at the constant, field
    /// omitted)`. The two must decode equal. `None` when inapplicable.
    pub default_eq: Option<(String, String)>,
    /// Externally-tagged enum object with extra unknown keys injected around the
    /// tag. With `ignore_tag_adjacent_fields` it must decode equal to `canonical`
    /// (an EQ input); without the flag it must error (a BAD input). `None` unless
    /// the case is an external enum and the sampled variant rendered as an object.
    pub adjacent: Option<String>,
    /// A rejecting-validator BAD input (a field set to [`VALIDATE_SENTINEL`]). Must
    /// fail to decode. `None` unless the case has a rejecting-validate field.
    pub validate_reject: Option<String>,
    /// The `other`-variant check: `(known-tag input, unknown-tag input)`, both of
    /// which must decode to the catch-all variant. `None` unless the enum has one.
    pub other_eq: Option<(String, String)>,
}

/// Build one sample and all its renderings, deterministic in `sample_seed`.
pub fn sample(case: &Case, sample_seed: u64) -> Sample {
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(sample_seed)),
        steam: 95,
        full: false,
    };
    let node = build_node(case, &mut rand);
    // A separate RNG drives the transforms so the canonical value stays fixed
    // regardless of how many spaces/permutations the variants draw.
    let mut trng = StdRng::seed_from_u64(mix64(sample_seed ^ 0xD1B5_4A32_D192_ED03));
    let mut canonical = String::new();
    render_canonical(&node, &mut canonical);
    let mut permuted = String::new();
    render_permuted(&node, &mut trng, &mut permuted);
    let mut spaced = String::new();
    render_spaced(&node, &mut trng, &mut spaced);
    let dup = render_dup(&node);
    let default_eq = build_default_eq(case, sample_seed);
    let adjacent = if external_tagged(case) {
        render_adjacent(&node, &mut trng)
    } else {
        None
    };
    let validate_reject = build_validate_reject(case, sample_seed);
    let other_eq = build_other_eq(case);
    Sample {
        canonical,
        permuted,
        spaced,
        dup,
        default_eq,
        adjacent,
        validate_reject,
        other_eq,
    }
}

/// Produce one canonical JSON input for `case`, deterministic in `sample_seed`.
pub fn sample_json(case: &Case, sample_seed: u64) -> String {
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(sample_seed)),
        steam: 95,
        full: false,
    };
    let node = build_node(case, &mut rand);
    let mut out = String::new();
    render_canonical(&node, &mut out);
    out
}

/// Whether any field type in the case is (or contains) a float. The F64 sampler
/// reuses the f32 generator, so a decoded `f64` re-encodes to a string that need
/// not byte-match the input; such cases are excluded from the exact-string encode
/// oracle (their encoding is still covered by the self-consistent round trip).
fn case_has_float(case: &Case) -> bool {
    let any = |fields: &[FieldSpec]| fields.iter().any(|f| ty_has_float_pub(f.ty));
    let body_float = match &case.body {
        Body::Named(f) | Body::Tuple(f) => any(f),
        Body::Enum { variants, .. } => variants.iter().any(|v| any(&v.fields)),
        _ => false,
    };
    body_float || case.flatten.as_ref().is_some_and(|fl| fl.has_float())
}

/// Build the absolute `skip_if` encode checks as two-column `(input, expected)`
/// records: decode `input`, re-encode, compare to `expected`. Two records:
///
///   - **kept**: the `skip_if` field is present at a non-trigger value, so encode
///     keeps it (`expected == input`). Proves the predicate-false path emits it.
///   - **omitted**: the field is present at the trigger value, so encode omits it
///     (`expected` drops it). Proves the predicate-true path actually fires.
///
/// The regular fields are rendered `full` (canonical), so the comparison is
/// byte-absolute. `None` unless the case has a `skip_if` field and is float-free.
pub fn build_skip_if_encode(case: &Case, seed: u64) -> Option<Vec<(String, String)>> {
    let sk = case.skip_if.as_ref()?;
    if case_has_float(case) {
        return None;
    }
    let Body::Named(fields) = &case.body else {
        return None;
    };
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(seed ^ 0x5417_1F00_DEFA_0001)),
        steam: 95,
        full: true,
    };
    let rule = case.struct_field_rule();
    let mut base: Vec<(String, String)> = Vec::new();
    for field in fields {
        if field.skip {
            continue;
        }
        base.push((field.key(rule), field.sample_value(&mut rand)));
    }
    let sk_key = rule.apply_to_field(&sk.name);
    // The skip_if field is declared last, so it sits after the regular fields.
    let mut kept = base.clone();
    kept.push((sk_key.clone(), sk.kind.kept_json().to_string()));
    let kept_json = render_members(&kept);
    let mut trig = base.clone();
    trig.push((sk_key.clone(), sk.kind.trigger_json().to_string()));
    let trig_input = render_members(&trig);
    let trig_expected = render_members(&base);
    Some(vec![
        (kept_json.clone(), kept_json),
        (trig_input, trig_expected),
    ])
}

/// Build the encode-oracle input: a full canonical JSON object (every non-skip
/// field present, canonical keys) that the re-encoded decoded value must
/// byte-match. This is the absolute encode check the self-consistent round trip
/// cannot make (a wrong-but-consistent encoding round-trips fine). `None` unless
/// the case has both JSON directions and no float field (see [`case_has_float`]).
pub fn sample_encode(case: &Case, sample_seed: u64) -> Option<String> {
    if !case.traits.from_json || !case.traits.to_json {
        return None;
    }
    if case_has_float(case) {
        return None;
    }
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(mix64(sample_seed ^ 0xE5C0_DE17_5EED_1234)),
        steam: 95,
        full: true,
    };
    let node = build_node(case, &mut rand);
    let mut out = String::new();
    render_canonical(&node, &mut out);
    Some(out)
}
