//! Case model + deterministic generation.
//!
//! A `Case` is fully determined by its `u64` id (used as an RNG seed), so any
//! failing case is reproducible from its id alone. The model and the JSON input
//! sampler are deliberately coupled: whatever attributes a case emits, the
//! sampler produces matching, round-trip-safe JSON.

use jsony::{TextWriter, ToJson};
use rand::prelude::*;

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
    /// The canonical JSON key for this field (rename overrides the declared name).
    fn key(&self) -> &str {
        self.rename.as_deref().unwrap_or(&self.name)
    }

    /// Pick a key to write into a sampled input: the canonical key, or, when the
    /// field has an alias, the alias half the time (exercising the alias parse
    /// path). The write side always emits the canonical key, so round-trip
    /// identity holds either way.
    fn input_key(&self, rand: &mut Rand) -> &str {
        match &self.alias {
            Some(a) if rand.rng.gen_bool(0.5) => a,
            _ => self.key(),
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
        s
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
            // alias on enum-variant struct fields is not honored by jsony (the
            // parser only matches the canonical/renamed key), so it is excluded
            // here to keep the round-trip oracle sound. Tracked as a finding.
            gen_fields(rng, n, true, binary, false)
        }
    };
    VariantSpec { name, kind, fields }
}

/// Build the deterministic case for `id`.
pub fn case_from_id(id: u64) -> Case {
    let mut rng = StdRng::seed_from_u64(id);
    let binary = rng.gen_bool(0.5);
    let mut traits = TraitSet {
        from_json: true,
        to_json: true,
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
    Case {
        name: format!("T{id}"),
        id,
        traits,
        body,
        transparent,
    }
}

// --- Input sampling (coupled with the emitted attributes) ---

fn write_value(ty: Type, rand: &mut Rand, out: &mut TextWriter) {
    ty.json_encode(rand, out);
}

/// Write a JSON object for a set of named fields, honouring rename / default /
/// skip. `default` fields may be omitted; `skip` fields are always omitted.
fn write_named_object(fields: &[FieldSpec], rand: &mut Rand, out: &mut TextWriter) {
    out.start_json_object();
    for field in fields {
        if field.skip {
            continue;
        }
        if field.default && rand.rng.gen_bool(0.5) {
            continue;
        }
        let key = field.input_key(rand).to_string();
        key.encode_json__jsony(out);
        out.push_colon();
        write_value(field.ty, rand, out);
        out.push_comma();
    }
    out.end_json_object();
}

fn write_struct_body(case: &Case, rand: &mut Rand, out: &mut TextWriter) {
    if case.transparent {
        // Single-field newtype: serialized as the inner value.
        if let Body::Named(fields) = &case.body {
            write_value(fields[0].ty, rand, out);
            return;
        }
    }
    match &case.body {
        Body::Named(fields) => write_named_object(fields, rand, out),
        Body::Tuple(fields) => {
            if fields.len() == 1 {
                // Newtype: serializes transparently as the inner value.
                write_value(fields[0].ty, rand, out);
            } else {
                // Multi-field tuple struct: a JSON array (pending the jsony-gap fix).
                out.start_json_array();
                for field in fields {
                    write_value(field.ty, rand, out);
                    out.push_comma();
                }
                out.end_json_array();
            }
        }
        // jsony serializes a unit struct as an empty object.
        Body::Unit => out.push_str("{}"),
        Body::Enum { .. } => unreachable!("handled in sample_json"),
    }
}

/// Write the inline fields of a struct variant directly into the current object
/// (used by internal tagging).
fn write_inline_fields(fields: &[FieldSpec], rand: &mut Rand, out: &mut TextWriter) {
    for field in fields {
        if field.skip {
            continue;
        }
        if field.default && rand.rng.gen_bool(0.5) {
            continue;
        }
        let key = field.input_key(rand).to_string();
        key.encode_json__jsony(out);
        out.push_colon();
        write_value(field.ty, rand, out);
        out.push_comma();
    }
}

fn write_variant_content(v: &VariantSpec, rand: &mut Rand, out: &mut TextWriter) {
    match v.kind {
        VarKind::Unit => out.push_str("null"),
        VarKind::Tuple => write_value(v.fields[0].ty, rand, out),
        VarKind::Struct => write_named_object(&v.fields, rand, out),
    }
}

fn write_enum(repr: EnumRepr, variants: &[VariantSpec], rand: &mut Rand, out: &mut TextWriter) {
    let v = &variants[rand.rng.gen_range(0..variants.len())];
    match repr {
        EnumRepr::External => match v.kind {
            VarKind::Unit => {
                v.name.as_str().encode_json__jsony(out);
            }
            VarKind::Tuple | VarKind::Struct => {
                out.start_json_object();
                v.name.as_str().encode_json__jsony(out);
                out.push_colon();
                write_variant_content(v, rand, out);
                out.push_comma();
                out.end_json_object();
            }
        },
        EnumRepr::Internal => {
            out.start_json_object();
            TAG.encode_json__jsony(out);
            out.push_colon();
            v.name.as_str().encode_json__jsony(out);
            out.push_comma();
            if v.kind == VarKind::Struct {
                write_inline_fields(&v.fields, rand, out);
            }
            out.end_json_object();
        }
        EnumRepr::Adjacent => {
            out.start_json_object();
            TAG.encode_json__jsony(out);
            out.push_colon();
            v.name.as_str().encode_json__jsony(out);
            out.push_comma();
            if v.kind != VarKind::Unit {
                CONTENT.encode_json__jsony(out);
                out.push_colon();
                write_variant_content(v, rand, out);
                out.push_comma();
            }
            out.end_json_object();
        }
    }
}

/// Produce one JSON input for `case`, deterministic in `sample_seed`.
pub fn sample_json(case: &Case, sample_seed: u64) -> String {
    let mut out = TextWriter::new();
    let mut rand = Rand {
        rng: StdRng::seed_from_u64(sample_seed),
        steam: 95,
    };
    match &case.body {
        Body::Enum { repr, variants } => write_enum(*repr, variants, &mut rand, &mut out),
        _ => write_struct_body(case, &mut rand, &mut out),
    }
    out.into_string()
}
