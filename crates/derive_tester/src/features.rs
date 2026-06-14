use std::collections::HashSet;

use rand::distributions::DistString;
use rand::prelude::*;

use crate::schema::{Field, Struct, Type};

macro_rules! select {
    (enable: [$($enable:ident),* $(,)?], restricted: [$($disable:ident),* $(,)?]) => {
        FeatureSelect {
            enabled: FieldFeature::bitset(&[$(FieldFeature::$enable),*]),
            restricted: FieldFeature::bitset(&[$(FieldFeature::$disable),*]),
        }
    };
}

pub struct FeatureSelect {
    pub enabled: u64,
    pub restricted: u64,
}

impl FeatureSelect {
    pub fn expand(&mut self, set: FeatureSelect) {
        self.enabled |= set.enabled;
        self.restricted |= set.restricted;
    }

    pub fn next(&mut self, rng: &mut StdRng) -> Option<(FieldFeature, u64)> {
        let target = self.enabled & (!self.restricted);
        let len = target.count_ones();
        if len == 0 {
            return None;
        }
        let i = nth_set(target, rng.gen_range(0..len)).trailing_zeros();
        let f = unsafe { std::mem::transmute::<u8, FieldFeature>(i as u8) };
        self.expand(f.features_when_enabled());
        Some((f, rng.gen()))
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum FieldFeature {
    TrivalDefault,
    Default,
    Rename,
    Skip,
}

impl FieldFeature {
    pub const fn features_when_enabled(&self) -> FeatureSelect {
        use FieldFeature as F;
        match self {
            F::TrivalDefault => FeatureSelect {
                enabled: 0,
                restricted: F::bitset(&[F::Default, F::TrivalDefault]),
            },
            F::Default => FeatureSelect {
                enabled: 0,
                restricted: F::bitset(&[F::Default, F::TrivalDefault]),
            },
            F::Rename => FeatureSelect {
                enabled: 0,
                restricted: F::bitset(&[F::Rename]),
            },
            F::Skip => FeatureSelect {
                enabled: 0,
                restricted: F::bitset(&[F::Skip]),
            },
        }
    }

    pub const fn bitset(mut features: &'static [FieldFeature]) -> u64 {
        let mut result = 0u64;
        while let [value, rest @ ..] = features {
            result |= 1 << (*value as u64);
            features = rest;
        }
        result
    }
}

fn nth_set(set: u64, n: u32) -> u64 {
    unsafe { std::arch::x86_64::_pdep_u64(1 << n, set) }
}

pub enum FieldFeatureDecl {
    Default { seed: u64 },
    TrivalDefault,
    Rename(String),
    Skip,
}

pub struct FieldFeatureGenerator<'b, 'a> {
    parent: &'b mut FieldAnnotator<'a>,
    features: FeatureSelect,
    remaining: u32,
}

impl<'b, 'a> FieldFeatureGenerator<'b, 'a> {
    pub fn next(&mut self) -> Option<FieldFeatureDecl> {
        if self.remaining == 0 {
            return None;
        }
        self.remaining -= 1;
        let (feature, value_seed) = self.features.next(&mut self.parent.rng)?;
        match feature {
            FieldFeature::TrivalDefault => Some(FieldFeatureDecl::TrivalDefault),
            FieldFeature::Default => Some(FieldFeatureDecl::Default { seed: value_seed }),
            FieldFeature::Rename => {
                let mut rand = crate::datagen::Rand {
                    rng: StdRng::seed_from_u64(value_seed),
                    steam: 100,
                };
                let mut foo = rand::distributions::Alphanumeric.sample_string(&mut rand.rng, 3);
                while self.parent.used_names.contains(&foo) {
                    foo = rand::distributions::Alphanumeric.sample_string(&mut rand.rng, 3);
                }
                self.parent.used_names.insert(foo.clone());
                Some(FieldFeatureDecl::Rename(foo))
            }
            FieldFeature::Skip => Some(FieldFeatureDecl::Skip),
        }
    }
}

pub struct FieldAnnotator<'a> {
    rng: StdRng,
    used_names: HashSet<String>,
    ruct: &'a Struct<'a>,
    at: usize,
}

impl<'a> FieldAnnotator<'a> {
    pub fn of_struct(ruct: &'a Struct<'a>) -> FieldAnnotator<'a> {
        FieldAnnotator {
            rng: StdRng::seed_from_u64(ruct.seed),
            used_names: HashSet::new(),
            ruct,
            at: 0,
        }
    }

    pub fn next<'b>(&'b mut self) -> Option<(&'a Field<'a>, FieldFeatureGenerator<'b, 'a>)> {
        let field = self.ruct.fields.get(self.at)?;
        self.at += 1;
        let remaining: u32 = self.rng.gen_range(0..3);
        let mut feature_set = select! {
            enable: [Rename],
            restricted: []
        };
        feature_set.expand(field.ty().field_feature_set());
        Some((
            field,
            FieldFeatureGenerator {
                parent: self,
                features: feature_set,
                remaining,
            },
        ))
    }
}

impl<'b> Type<'b> {
    pub fn field_feature_set(&self) -> FeatureSelect {
        match self {
            Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::U32
            | Type::I32
            | Type::U64
            | Type::I64
            | Type::I128
            | Type::U128
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Str
            | Type::String => select! {
                enable: [Default, TrivalDefault, Skip],
                restricted: []
            },
            _ => FeatureSelect {
                enabled: 0,
                restricted: 0,
            },
        }
    }
}
