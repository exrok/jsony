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

pub struct FieldSpec {
    pub name: String,
    pub ty: Type<'static>,
    /// `#[jsony(rename = "...")]`; the JSON key actually used.
    pub rename: Option<String>,
    /// `#[jsony(alias = "...")]`; an additional accepted input key.
    pub alias: Option<String>,
    /// `#[jsony(default)]` — may be omitted from the input.
    pub default: bool,
    /// `#[jsony(skip)]` — never present in JSON, filled with Default.
    pub skip: bool,
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
}

pub enum Body {
    Named(Vec<FieldSpec>),
    Tuple(Vec<FieldSpec>),
    Unit,
    Enum {
        repr: EnumRepr,
        variants: Vec<VariantSpec>,
    },
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
}

impl Case {
    /// The `#[jsony(...)]` container attribute contents.
    pub fn container_attr(&self) -> String {
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

    /// Rename rule applied to enum struct-variant field keys: `rename_all_fields`
    /// when set, otherwise the container `rename_all` (jsony falls back to it).
    fn variant_field_rule(&self) -> RenameRule {
        if self.rename_all_fields != RenameRule::None {
            self.rename_all_fields
        } else {
            self.rename_all
        }
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
) -> Vec<FieldSpec> {
    let choices = type_choices(binary);
    let mut fields = Vec::new();
    for i in 0..count {
        let name = format!("f{i}");
        let ty = *choices.choose(rng).unwrap();
        let (mut rename, mut alias, mut default, mut skip) = (None, None, false, false);
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
                default = rng.gen_bool(0.25);
            }
        }
        fields.push(FieldSpec {
            name,
            ty,
            rename,
            alias,
            default,
            skip,
        });
    }
    fields
}

/// A single plain field (no rename/alias/default/skip), for transparent structs.
fn gen_plain_field(rng: &mut StdRng, binary: bool) -> FieldSpec {
    let ty = *type_choices(binary).choose(rng).unwrap();
    FieldSpec {
        name: "f0".to_string(),
        ty,
        rename: None,
        alias: None,
        default: false,
        skip: false,
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
        // Tuple variants currently support exactly one field.
        VarKind::Tuple => gen_fields(rng, 1, false, binary, false),
        VarKind::Struct => {
            let n = rng.gen_range(1..4);
            // alias on enum-variant struct fields is now honored by jsony, so the
            // sampler exercises the alias parse path here too (see the resolved
            // finding in docs/derive-tester.md).
            gen_fields(rng, n, true, binary, true)
        }
    };
    VariantSpec { name, kind, fields }
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
    let body = match rng.gen_range(0..12) {
        0..=3 => {
            let n = rng.gen_range(1..6);
            Body::Named(gen_fields(&mut rng, n, true, binary, true))
        }
        4..=5 => {
            let n = rng.gen_range(1..4);
            Body::Tuple(gen_fields(&mut rng, n, false, binary, false))
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
            let variants = (0..n)
                .map(|i| gen_variant(&mut rng, i, repr, binary))
                .collect();
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
        _ => {}
    }
    Case {
        name: format!("T{id}"),
        id,
        traits,
        body,
        transparent,
        rename_all,
        rename_all_fields,
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
        if field.default && rand.rng.gen_bool(0.5) {
            continue;
        }
        let key = field.input_key(rule, rand);
        members.push((key, Node::Raw(raw_value(field.ty, rand))));
    }
    Node::Object(members)
}

fn build_variant_content(v: &VariantSpec, rule: RenameRule, rand: &mut Rand) -> Node {
    match v.kind {
        VarKind::Unit => Node::Raw("null".to_string()),
        VarKind::Tuple => Node::Raw(raw_value(v.fields[0].ty, rand)),
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
                // Newtype: serializes transparently as the inner value.
                Node::Raw(raw_value(fields[0].ty, rand))
            } else {
                // Multi-field tuple struct: a positional JSON array.
                let mut out = TextWriter::new();
                out.start_json_array();
                for field in fields {
                    field.ty.json_encode(rand, &mut out);
                    out.push_comma();
                }
                out.end_json_array();
                Node::Raw(out.into_string())
            }
        }
        // jsony serializes a unit struct as an empty object.
        Body::Unit => Node::Raw("{}".to_string()),
        Body::Enum { .. } => unreachable!("handled in build_node"),
    }
}

fn build_enum(case: &Case, repr: EnumRepr, variants: &[VariantSpec], rand: &mut Rand) -> Node {
    let v = &variants[rand.rng.gen_range(0..variants.len())];
    // Variant names follow `rename_all`; struct-variant field keys follow
    // `rename_all_fields` (falling back to `rename_all`).
    let name = case.rename_all.apply_to_variant(&v.name);
    let field_rule = case.variant_field_rule();
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
    Sample {
        canonical,
        permuted,
        spaced,
        dup,
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
