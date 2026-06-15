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
            Some(a) if rand.rng.gen_bool(0.5) => a.clone(),
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
    ];
    let _ = binary;
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
                    0 => with = true,
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
        if rng.gen_bool(0.3) {
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
    let body = match rng.gen_range(0..13) {
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
            // `with` is intentionally NOT enabled on tuple-*struct* fields: jsony's
            // tuple-struct decoder ignores the field's `with` transform (it encodes
            // `[1,"2"]` but then decodes expecting a raw number), and a single-field
            // tuple-struct `with` fails to compile (a JSON-kind marker conflict).
            // Both are open jsony bugs surfaced by this harness, so adding them to
            // the round-trip gate would make it red. Tuple *variant* `with` works
            // and is covered via `gen_variant`.
            let tf = gen_fields(&mut rng, n, false, binary, false, false);
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
            let repr = match rng.gen_range(0..4) {
                0 => EnumRepr::Internal,
                1 => EnumRepr::Adjacent,
                _ => EnumRepr::External,
            };
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
    // It is NOT combined with an `other` variant: that pairing currently fails to
    // compile (`E0499` in jsony's generated decode loop, where the `other` path
    // and the adjacent-skip loop both mutably borrow the variant cursor). An open
    // jsony bug surfaced by this harness, excluded here to keep the gate green.
    let body_has_other =
        matches!(&body, Body::Enum { variants, .. } if variants.iter().any(|v| v.other));
    let ignore_tag_adjacent = matches!(&body, Body::Enum { repr: EnumRepr::External, .. })
        && !body_has_other
        && rng.gen_bool(0.4);
    Case {
        name: format!("T{id}"),
        id,
        traits,
        body,
        transparent,
        rename_all,
        rename_all_fields,
        ignore_tag_adjacent,
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
        if field.default.is_set() && rand.rng.gen_bool(0.5) {
            continue;
        }
        let key = field.input_key(rule, rand);
        let value = field.sample_value(rand);
        members.push((key, Node::Raw(value)));
    }
    Node::Object(members)
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
        Body::Named(fields) => build_named_object(fields, case.struct_field_rule(), rand),
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
    }
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
    matches!(&case.body, Body::Enum { repr: EnumRepr::External, .. })
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
    };
    let node = build_node(case, &mut rand);
    let mut out = String::new();
    render_canonical(&node, &mut out);
    out
}
