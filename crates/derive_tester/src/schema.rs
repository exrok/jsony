use std::cell::Cell;

use bumpalo::Bump;
use proc_macro2::{Ident, Literal, TokenTree};

pub type FLAGS = u32;
pub const BUILT_IN: FLAGS = 1 << 0;
pub const SCALAR: FLAGS = 1 << 1;
pub const IMPL_TO_JSON: FLAGS = 1 << 2;
pub const IMPL_FROM_JSON: FLAGS = 1 << 3;
pub const IMPL_JSON: FLAGS = IMPL_TO_JSON | IMPL_FROM_JSON;
pub const IMPL_TO_BINARY: FLAGS = 1 << 4;
pub const IMPL_FROM_BINARY: FLAGS = 1 << 5;
pub const IMPL_BINARY: FLAGS = IMPL_TO_BINARY | IMPL_FROM_BINARY;
pub const IMPL_FROM_TEXT: FLAGS = 1 << 6;
pub const IMPL_FROM_STR: FLAGS = 1 << 7;
pub const HAS_LT1: FLAGS = 1 << 8;
pub const UNSIZED: FLAGS = 1 << 9;
pub const IMPL_TO_TOML: FLAGS = 1 << 10;
pub const IMPL_FROM_TOML: FLAGS = 1 << 11;
pub const IMPL_TOML: FLAGS = IMPL_TO_TOML | IMPL_FROM_TOML;

pub type TypeIndex = usize;
pub type LifetimeSet = u8;

#[derive(Clone, Copy, Debug, Hash)]
pub enum Type<'b> {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    I128,
    U128,
    F32,
    F64,
    Bool,
    Str,
    String,
    Ref(&'b Type<'b>),
    Slice(&'b Type<'b>),
    Vec(&'b Type<'b>),
    Box(&'b Type<'b>),
    Cow(&'b Type<'b>),
    Option(&'b Type<'b>),
    /// `[T; N]`, a fixed-length array.
    Array(&'b Type<'b>, usize),
    Struct(&'b Struct<'b>),
    Enum(&'b Enum<'b>),
    Generic(char),
    /// `HashMap<K, V>`. Encodes/decodes as a JSON object (string-kind keys).
    Map(&'b Type<'b>, &'b Type<'b>),
    /// `BTreeMap<K, V>`. JSON object with sorted-key encode, plus a binary codec
    /// (length-prefixed entries in key order).
    BTreeMap(&'b Type<'b>, &'b Type<'b>),
    /// `Vec<(K, V)>`. A positional list of pairs; the `object_as_vec_of_tuple`
    /// helper and `via = Iterator` render it as a JSON object instead.
    TupleVec(&'b Type<'b>, &'b Type<'b>),
}

#[derive(Debug)]
pub struct Field<'b> {
    pub name: Ident,
    pub inner_type: Cell<Type<'b>>,
    pub rename: Option<&'b str>,
    pub default: bool,
}

impl<'b> std::hash::Hash for Field<'b> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.inner_type.get().hash(state);
        self.rename.hash(state);
    }
}

impl<'b> Field<'b> {
    pub fn ty(&self) -> Type<'b> {
        self.inner_type.get()
    }
}

#[derive(Debug, Hash)]
pub struct Enum<'a> {
    pub name: Ident,
    pub variants: Vec<EnumVariant<'a>>,
    pub flags: FLAGS,
}

#[derive(Debug, Hash)]
pub struct EnumVariant<'a> {
    pub name: Ident,
    pub fields: &'a [Field<'a>],
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum ItemKind {
    Tuple,
    Struct,
}

#[derive(Debug, Hash)]
pub struct Struct<'a> {
    pub name: Ident,
    pub fields: &'a [Field<'a>],
    pub kind: ItemKind,
    pub lifetimes: LifetimeSet,
    pub stack_depth: usize,
    pub seed: u64,
    pub flags: FLAGS,
}

pub struct Types<'b> {
    pub bump: &'b Bump,
    pub db: Vec<&'b Type<'b>>,
}

impl<'b> Types<'b> {
    pub fn insert_synthetic_seeds(&mut self) {
        self.insert(Type::I8);
        self.insert(Type::U8);
        self.insert(Type::F32);
        self.insert(Type::Option(&Type::String));
        self.insert(Type::Vec(&Type::U32));
        self.insert(Type::I32);
        self.insert(Type::U64);
        self.insert(Type::Bool);
        self.insert(Type::String);
    }

    pub fn insert(&mut self, ty: Type<'b>) -> &'b Type<'b> {
        let value: &'b Type<'b> = &*self.bump.alloc(ty);
        self.db.push(value);
        value
    }

    pub fn create_struct(
        &mut self,
        kind: ItemKind,
        name: Ident,
        fields: Vec<Field<'b>>,
        seed: u64,
        flags: FLAGS,
    ) -> &'b Struct<'b> {
        let fields = &*self.bump.alloc_slice_fill_iter(fields.into_iter());
        let mut lifetimes = 0;
        let mut stack_depth = 0;
        for field in fields {
            stack_depth = stack_depth.max(field.ty().stack_depth());
            lifetimes |= field.inner_type.get().lifetimes();
        }
        self.bump.alloc(Struct {
            fields,
            kind,
            name,
            lifetimes,
            stack_depth: stack_depth + 1,
            seed,
            flags,
        })
    }

    pub fn insert_struct(
        &mut self,
        kind: ItemKind,
        name: Ident,
        fields: Vec<Field<'b>>,
        seed: u64,
        flags: FLAGS,
    ) -> &'b Type<'b> {
        let ty = Type::Struct(self.create_struct(kind, name, fields, seed, flags));
        self.insert(ty)
    }

    pub fn alloc_fields(&self, fields: Vec<Field<'b>>) -> &'b [Field<'b>] {
        self.bump.alloc_slice_fill_iter(fields.into_iter())
    }

    pub fn create_enum(
        &self,
        name: Ident,
        variants: Vec<EnumVariant<'b>>,
        flags: FLAGS,
    ) -> &'b Enum<'b> {
        self.bump.alloc(Enum {
            name,
            variants,
            flags,
        })
    }
}

impl<'b> Type<'b> {
    pub fn stack_depth(&self) -> usize {
        match self {
            Type::Struct(record) => record.stack_depth,
            Type::Option(value) => value.stack_depth(),
            _ => 0,
        }
    }

    pub fn lifetimes(&self) -> LifetimeSet {
        let mut lifetimes = 0;
        let mut current: &Type = self;
        loop {
            match current {
                Type::Ref(inner) | Type::Cow(inner) => {
                    lifetimes |= 1;
                    current = inner;
                    continue;
                }
                Type::Slice(t)
                | Type::Vec(t)
                | Type::Box(t)
                | Type::Option(t)
                | Type::Array(t, _) => {
                    current = t;
                    continue;
                }
                Type::Struct(record) => {
                    lifetimes |= record.lifetimes;
                    return lifetimes;
                }
                Type::Map(k, v) | Type::BTreeMap(k, v) | Type::TupleVec(k, v) => {
                    lifetimes |= k.lifetimes() | v.lifetimes();
                    return lifetimes;
                }
                _ => return lifetimes,
            }
        }
    }

    pub fn gen(&self, out: &mut Vec<TokenTree>) {
        let ident_name = match self {
            Type::U8 => "u8",
            Type::I8 => "i8",
            Type::U16 => "u16",
            Type::I16 => "i16",
            Type::U32 => "u32",
            Type::I32 => "i32",
            Type::U64 => "u64",
            Type::I64 => "i64",
            Type::I128 => "i128",
            Type::U128 => "u128",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Bool => "bool",
            Type::Str => "str",
            Type::String => "String",
            Type::Ref(inner) => {
                splat!(out; &~a [inner.gen(out)]);
                return;
            }
            Type::Slice(inner) => {
                splat!(out; [[ [inner.gen(out)] ]]);
                return;
            }
            Type::Cow(inner) => {
                splat!(out; Cow<~a, [inner.gen(out)] >);
                return;
            }
            Type::Vec(inner) => {
                splat!(out; Vec < [inner.gen(out)] >);
                return;
            }
            Type::Box(inner) => {
                splat!(out; Box < [inner.gen(out)] >);
                return;
            }
            Type::Array(inner, n) => {
                splat!(out; [[ [inner.gen(out)] ; [#Literal::usize_unsuffixed(*n)] ]]);
                return;
            }
            Type::Struct(record) => {
                splat!(out; [#record.name] [?(record.lifetimes != 0)<~a>]);
                return;
            }
            Type::Enum(adt) => {
                out.push(adt.name.clone().into());
                return;
            }
            Type::Option(inner) => {
                splat!(out; Option < [inner.gen(out)] >);
                return;
            }
            Type::Generic(ch) => {
                out.push(TokenTree::Ident(Ident::owned(
                    &*ch.encode_utf8(&mut [0u8; 4][..]),
                )));
                return;
            }
            Type::Map(k, v) => {
                splat!(out; HashMap < [k.gen(out)] , [v.gen(out)] >);
                return;
            }
            Type::BTreeMap(k, v) => {
                splat!(out; BTreeMap < [k.gen(out)] , [v.gen(out)] >);
                return;
            }
            Type::TupleVec(k, v) => {
                splat!(out; Vec < ([k.gen(out)] , [v.gen(out)]) >);
                return;
            }
        };
        out.push(Ident::Static(&ident_name).into());
    }
}
