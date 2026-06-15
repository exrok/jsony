use std::borrow::Cow;

use jsony::{TextWriter, ToJson};
use proc_macro2::{Literal, TokenTree};
use rand::distributions::{DistString, Distribution, Standard, Uniform};
use rand::prelude::*;

use crate::features::{FieldAnnotator, FieldFeatureDecl};
use crate::schema::{ItemKind, Struct, Type};

pub struct Rand {
    pub rng: StdRng,
    pub steam: i64,
    /// When set, the JSON sampler emits a *full canonical* object: every non-skip
    /// field present, under its canonical key (no alias, no default omission). Used
    /// by the encode oracle, where the re-encoded value must byte-match the input.
    pub full: bool,
}

impl Rand {
    pub fn is_option_some(&mut self) -> bool {
        if self.steam <= 1 {
            return false;
        }
        if self.rng.gen_bool(0.9) {
            self.steam -= 2;
            true
        } else {
            false
        }
    }

    pub fn container_len(&mut self) -> usize {
        if self.steam <= 0 {
            return 0;
        }
        let upper = (self.steam / 4).min(2);
        let used = self.rng.gen_range(0..upper as usize);
        self.steam -= used as i64;
        used
    }

    pub fn string(&mut self) -> String {
        let rng = &mut self.rng;
        let len = if rng.gen_bool(0.97) {
            rng.gen_range(0..32)
        } else {
            rng.gen_range(32..512)
        };
        match rng.gen_range(0..10) {
            0 => String::new(),
            1 => rand::distributions::Standard.sample_string(rng, len),
            2 => Uniform::new(0u8, 128)
                .map(|c| c as char)
                .sample_iter(rng.clone())
                .take(len)
                .collect(),
            _ => rand::distributions::Alphanumeric.sample_string(rng, len),
        }
    }

    /// An escape-free string for `&str` fields. jsony decodes `&'a str` with
    /// `take_borrowed_string`, which **errors** on any string needing an escape
    /// (it can only borrow the raw input). Alphanumeric content JSON-encodes
    /// verbatim, so it borrows cleanly and round-trips byte-for-byte.
    pub fn borrowed_string(&mut self) -> String {
        let len = self.rng.gen_range(0..24);
        rand::distributions::Alphanumeric.sample_string(&mut self.rng, len)
    }

    pub fn gen_base_sample<T: Copy>(&mut self, base: &[T]) -> T
    where
        Standard: Distribution<T>,
    {
        if self.rng.gen_bool(0.75) {
            *base.choose(&mut self.rng).unwrap()
        } else {
            self.rng.gen()
        }
    }
}

impl<'a> Struct<'a> {
    pub fn random_json(&'a self, rand: &mut Rand, out: &mut TextWriter) {
        match self.kind {
            ItemKind::Tuple => {
                for field in self.fields {
                    field.ty().json_encode(rand, out);
                }
            }
            ItemKind::Struct => {
                out.start_json_object();
                for field in self.fields {
                    field.name.as_str().encode_json__jsony(out);
                    out.push_colon();
                    field.ty().json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_object();
            }
        }
    }

    pub fn random_adv_json(&'a self, rand: &mut Rand, out: &mut TextWriter) {
        let mut fields = FieldAnnotator::of_struct(self);
        match self.kind {
            ItemKind::Tuple => {
                for field in self.fields {
                    field.ty().json_encode(rand, out);
                }
            }
            ItemKind::Struct => {
                out.start_json_object();
                'to_next_field: while let Some((field, mut features)) = fields.next() {
                    let mut name = Cow::Borrowed(field.name.as_str());
                    let mut skip = false;
                    while let Some(feature) = features.next() {
                        match feature {
                            FieldFeatureDecl::Skip => {
                                if rand.rng.gen_bool(0.25) {
                                    skip = true;
                                }
                            }
                            FieldFeatureDecl::TrivialDefault => {
                                if rand.rng.gen_bool(0.25) {
                                    skip = true;
                                }
                            }
                            FieldFeatureDecl::Default { .. } => {
                                if rand.rng.gen_bool(0.25) {
                                    skip = true;
                                }
                            }
                            FieldFeatureDecl::Rename(value) => name = Cow::Owned(value),
                        }
                        if skip {
                            while let Some(_) = features.next() {}
                            continue 'to_next_field;
                        }
                    }
                    name.encode_json__jsony(out);
                    out.push_colon();
                    field.ty().json_encode_adv(rand, out);
                    out.push_comma();
                }
                out.end_json_object();
            }
        }
    }
}

/// A distinct integer key value for entry `idx`. Signed key types alternate sign
/// so negative quoted-number keys (`"-1"`) are exercised; unsigned types stay
/// non-negative. Distinct across `idx` so multi-entry maps keep distinct keys.
fn map_key_int(k: &Type, idx: usize) -> i64 {
    let i = idx as i64;
    let signed = matches!(k, Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128);
    if signed && idx % 2 == 0 {
        -(i + 1)
    } else {
        i
    }
}

/// Write a string-kind map key plus its trailing colon, byte-identical to how
/// jsony renders that key on the `ToJson` side. String keys are `m_<idx>` (the
/// `m_` prefix keeps flattened map keys disjoint from sibling `f<n>` field keys);
/// integer keys render as the quoted decimal jsony emits for a numeric key
/// (including negative keys for signed types).
fn write_map_key(k: &Type, idx: usize, out: &mut TextWriter) {
    match k {
        Type::String | Type::Str => {
            format!("m_{idx}").as_str().encode_json__jsony(out);
            out.push_colon();
        }
        Type::U8
        | Type::I8
        | Type::U16
        | Type::I16
        | Type::U32
        | Type::I32
        | Type::U64
        | Type::I64
        | Type::U128
        | Type::I128 => {
            let mut buffer = itoa::Buffer::new();
            out.push_str("\"");
            out.push_str(buffer.format(map_key_int(k, idx)));
            out.push_str("\":");
        }
        other => panic!("unsupported map key type: {other:?}"),
    }
}

/// JSON-encode a `HashMap`/`BTreeMap` value as a JSON object. Under `full` (the
/// encode oracle) exactly one entry is emitted so the re-encode is order-stable
/// regardless of the concrete map type; otherwise 0..=2 distinct-keyed entries.
fn json_encode_map(k: &Type, v: &Type, rand: &mut Rand, out: &mut TextWriter) {
    let n = if rand.full {
        1
    } else {
        rand.rng.gen_range(0..=2usize)
    };
    out.start_json_object();
    for i in 0..n {
        write_map_key(k, i, out);
        v.json_encode(rand, out);
        out.push_comma();
    }
    out.end_json_object();
}

/// Native JSON for a `Vec<(K, V)>`: an array of two-element `[key, value]`
/// arrays (jsony's tuple `ToJson` is an array). Keys are encoded natively here
/// (numeric keys are bare numbers, not strings) — the object form is only
/// produced by the `object_as_vec_of_tuple` helper or `via = Iterator`.
fn json_encode_tuple_vec(k: &Type, v: &Type, rand: &mut Rand, out: &mut TextWriter) {
    let n = if rand.full {
        1
    } else {
        rand.rng.gen_range(0..=2usize)
    };
    out.start_json_array();
    for i in 0..n {
        out.start_json_array();
        match k {
            Type::String | Type::Str => {
                format!("m_{i}").as_str().encode_json__jsony(out);
            }
            _ => {
                let mut buffer = itoa::Buffer::new();
                out.push_str(buffer.format(i as i64));
            }
        }
        out.push_comma();
        v.json_encode(rand, out);
        out.push_comma();
        out.end_json_array();
        out.push_comma();
    }
    out.end_json_array();
}

impl<'b> Type<'b> {
    pub fn generate_random_default(&self, out: &mut Vec<TokenTree>, seed: u64) {
        let mut rand = Rand {
            rng: StdRng::seed_from_u64(seed),
            steam: 100,
            full: false,
        };
        match self {
            Type::U8 => splat!(out; [#Literal::u8_unsuffixed(rand.rng.gen())]),
            Type::I8 => splat!(out; [#Literal::i8_unsuffixed(rand.rng.gen())]),
            Type::U16 => splat!(out; [#Literal::u16_unsuffixed(rand.rng.gen())]),
            Type::I16 => splat!(out; [#Literal::i16_unsuffixed(rand.rng.gen())]),
            Type::U32 => splat!(out; [#Literal::u32_unsuffixed(rand.rng.gen())]),
            Type::I32 => splat!(out; [#Literal::i32_unsuffixed(rand.rng.gen())]),
            Type::U64 => splat!(out; [#Literal::u64_unsuffixed(rand.rng.gen())]),
            Type::I64 => splat!(out; [#Literal::i64_unsuffixed(rand.rng.gen())]),
            Type::I128 => splat!(out; [#Literal::i128_unsuffixed(rand.rng.gen())]),
            Type::U128 => splat!(out; [#Literal::u128_unsuffixed(rand.rng.gen())]),
            Type::F32 => splat!(out; [#Literal::f32_unsuffixed(rand.rng.gen())]),
            Type::F64 => splat!(out; [#Literal::f64_unsuffixed(rand.rng.gen())]),
            Type::Bool => {
                if rand.rng.gen_bool(0.5) {
                    splat!(out; false)
                } else {
                    splat!(out; true)
                }
            }
            Type::Str => splat!(out; [#Literal::string(&rand.string())]),
            Type::String => splat!(out; [#Literal::string(&rand.string())].into()),
            _ => panic!("This type doesn't implement random default"),
        }
    }

    pub fn json_encode(&self, rand: &mut Rand, out: &mut TextWriter) {
        let mut buffer = itoa::Buffer::new();
        let raw = match self {
            Type::U8 => buffer.format(rand.gen_base_sample(&[u8::MIN, u8::MAX])),
            Type::I8 => buffer.format(rand.gen_base_sample(&[i8::MIN, i8::MAX])),
            Type::U16 => buffer.format(rand.gen_base_sample(&[u16::MIN, u16::MAX])),
            Type::I16 => buffer.format(rand.gen_base_sample(&[i16::MIN, i16::MAX])),
            Type::U32 => buffer.format(rand.gen_base_sample(&[u32::MIN, u32::MAX])),
            Type::I32 => buffer.format(rand.gen_base_sample(&[i32::MIN, i32::MAX])),
            Type::U64 => buffer.format(rand.gen_base_sample(&[u64::MIN, u64::MAX])),
            Type::I64 => buffer.format(rand.gen_base_sample(&[i64::MIN, i64::MAX])),
            Type::I128 => buffer.format(rand.gen_base_sample(&[i128::MIN, i128::MAX])),
            Type::U128 => buffer.format(rand.gen_base_sample(&[u128::MIN, u128::MAX])),
            Type::F32 => {
                rand.rng.gen::<f32>().encode_json__jsony(out);
                return;
            }
            Type::F64 => {
                rand.rng.gen::<f32>().encode_json__jsony(out);
                return;
            }
            Type::Bool => {
                let value = rand.rng.gen_bool(0.5);
                value.encode_json__jsony(out);
                return;
            }
            Type::Str => {
                rand.string().encode_json__jsony(out);
                return;
            }
            Type::String => {
                rand.string().encode_json__jsony(out);
                return;
            }
            Type::Ref(inner) => {
                // `&str` borrows from the input, so its value must be escape-free.
                if matches!(inner, Type::Str) {
                    rand.borrowed_string().encode_json__jsony(out);
                } else {
                    inner.json_encode(rand, out);
                }
                return;
            }
            Type::Slice(inner) => {
                out.start_json_array();
                for _ in 0..rand.rng.gen_range(0..10) {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Cow(inner) => {
                inner.json_encode(rand, out);
                return;
            }
            Type::Vec(inner) => {
                out.start_json_array();
                for _ in 0..rand.rng.gen_range(0..10) {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Box(inner) => {
                inner.json_encode(rand, out);
                return;
            }
            Type::Option(inner) => {
                if rand.is_option_some() {
                    inner.json_encode(rand, out);
                    return;
                } else {
                    "null"
                }
            }
            Type::Array(inner, n) => {
                // jsony's `[T; N]` decode requires exactly N elements.
                out.start_json_array();
                for _ in 0..*n {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Struct(record) => {
                record.random_json(rand, out);
                return;
            }
            Type::Map(k, v) | Type::BTreeMap(k, v) => {
                json_encode_map(k, v, rand, out);
                return;
            }
            Type::TupleVec(k, v) => {
                json_encode_tuple_vec(k, v, rand, out);
                return;
            }
            Type::Enum(_) => todo!(),
            Type::Generic(_) => todo!(),
        };
        out.push_str(raw);
    }

    pub fn json_encode_adv(&self, rand: &mut Rand, out: &mut TextWriter) {
        let mut buffer = itoa::Buffer::new();
        let raw = match self {
            Type::U8 => buffer.format(rand.gen_base_sample(&[u8::MIN, u8::MAX])),
            Type::I8 => buffer.format(rand.gen_base_sample(&[i8::MIN, i8::MAX])),
            Type::U16 => buffer.format(rand.gen_base_sample(&[u16::MIN, u16::MAX])),
            Type::I16 => buffer.format(rand.gen_base_sample(&[i16::MIN, i16::MAX])),
            Type::U32 => buffer.format(rand.gen_base_sample(&[u32::MIN, u32::MAX])),
            Type::I32 => buffer.format(rand.gen_base_sample(&[i32::MIN, i32::MAX])),
            Type::U64 => buffer.format(rand.gen_base_sample(&[u64::MIN, u64::MAX])),
            Type::I64 => buffer.format(rand.gen_base_sample(&[i64::MIN, i64::MAX])),
            Type::I128 => buffer.format(rand.gen_base_sample(&[i128::MIN, i128::MAX])),
            Type::U128 => buffer.format(rand.gen_base_sample(&[u128::MIN, u128::MAX])),
            Type::F32 => {
                rand.rng.gen::<f32>().encode_json__jsony(out);
                return;
            }
            Type::F64 => {
                rand.rng.gen::<f32>().encode_json__jsony(out);
                return;
            }
            Type::Bool => {
                let value = rand.rng.gen_bool(0.5);
                value.encode_json__jsony(out);
                return;
            }
            Type::Str => {
                rand.string().encode_json__jsony(out);
                return;
            }
            Type::String => {
                rand.string().encode_json__jsony(out);
                return;
            }
            Type::Ref(inner) => {
                inner.json_encode(rand, out);
                return;
            }
            Type::Slice(inner) => {
                out.start_json_array();
                for _ in 0..rand.rng.gen_range(0..10) {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Cow(inner) => {
                inner.json_encode(rand, out);
                return;
            }
            Type::Vec(inner) => {
                out.start_json_array();
                for _ in 0..rand.rng.gen_range(0..10) {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Box(inner) => {
                inner.json_encode(rand, out);
                return;
            }
            Type::Option(inner) => {
                if rand.is_option_some() {
                    inner.json_encode(rand, out);
                    return;
                } else {
                    "null"
                }
            }
            Type::Array(inner, n) => {
                // jsony's `[T; N]` decode requires exactly N elements.
                out.start_json_array();
                for _ in 0..*n {
                    inner.json_encode(rand, out);
                    out.push_comma();
                }
                out.end_json_array();
                return;
            }
            Type::Struct(record) => {
                record.random_adv_json(rand, out);
                return;
            }
            Type::Map(k, v) | Type::BTreeMap(k, v) => {
                json_encode_map(k, v, rand, out);
                return;
            }
            Type::TupleVec(k, v) => {
                json_encode_tuple_vec(k, v, rand, out);
                return;
            }
            Type::Enum(_) => todo!(),
            Type::Generic(_) => todo!(),
        };
        out.push_str(raw);
    }
}
