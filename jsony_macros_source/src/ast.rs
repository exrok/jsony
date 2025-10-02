use crate::{case::RenameRule, util::Allocator, Error};

use proc_macro2::{Delimiter, Ident, Literal, Span, TokenStream, TokenTree};

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum GenericKind {
    Lifetime,
    Type,
    Const,
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct Generic<'a> {
    pub kind: GenericKind,
    pub ident: &'a Ident,
    pub bounds: &'a [TokenTree],
}
#[cfg_attr(feature = "debug", derive(Debug))]
pub enum DeriveTargetKind {
    TupleStruct,
    Struct,
    Enum,
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum Via {
    Iterator,
    None,
}

#[cfg_attr(feature = "debug", derive(Debug))]
struct FieldAttr {
    enabled: TraitSet,
    #[allow(dead_code)]
    span: Span,
    inner: FieldAttrInner,
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum DefaultKind {
    Default,
    Custom(Vec<TokenTree>),
}

#[cfg_attr(feature = "debug", derive(Debug))]
enum FieldAttrInner {
    Rename(Literal),
    Flatten,
    Skip(Vec<TokenTree>),
    Via(Via),
    Default(DefaultKind),
    With(Vec<TokenTree>),
    Validate(Vec<TokenTree>),
    Alias(Literal),
    Version(Literal),
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct FieldAttrs {
    attrs: Vec<FieldAttr>,
    flags: u64,
}

impl FieldAttrs {
    pub fn version(&self) -> Option<&Literal> {
        let mask = const { 0b1111u64 << (9 * TRAIT_COUNT) };
        if mask & self.flags == 0 {
            return None;
        }
        for attr in &self.attrs {
            if let FieldAttrInner::Version(lit) = &attr.inner {
                return Some(lit);
            }
        }
        None
    }
    pub fn alias(&self, for_trait: TraitSet) -> Option<&Literal> {
        let mask = const { 0b1111u64 << (8 * TRAIT_COUNT) };
        if mask & self.flags == 0 {
            return None;
        }
        for attr in &self.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Alias(lit) = &attr.inner {
                    return Some(lit);
                }
            }
        }
        None
    }
    pub fn rename(&self, for_trait: TraitSet) -> Option<&Literal> {
        // ranem is at bottom
        if self.flags & for_trait as u64 == 0 {
            return None;
        }
        for attr in &self.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Rename(lit) = &attr.inner {
                    return Some(lit);
                }
            }
        }
        None
    }
    pub fn has_other(&self) -> bool {
        self.flags & (0b1111 << (7u64 * TRAIT_COUNT)) != 0
    }
}

#[allow(clippy::derivable_impls)]
impl Default for FieldAttrs {
    fn default() -> Self {
        Self {
            attrs: Default::default(),
            flags: 0,
        }
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum Tag {
    Inline(String),
    Untagged,
    Default,
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum Repr {
    Transparent,
    C,
    Default,
    Unknown,
}

type EnumFlag = u8;
pub const ENUM_CONTAINS_UNIT_VARIANT: u8 = 0b0000_0001;
pub const ENUM_CONTAINS_STRUCT_VARIANT: u8 = 0b0000_0010;
pub const ENUM_CONTAINS_TUPLE_VARIANT: u8 = 0b0000_0100;
pub const ENUM_HAS_EXTERNAL_TAG: u8 = 0b0000_1000;
pub const ENUM_HAS_INLINE_TAG: u8 = 0b0001_0000;
pub const ENUM_HAS_NO_TAG: u8 = 0b0010_0000;

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct DeriveTargetInner<'a> {
    pub name: Ident,
    pub generics: Vec<Generic<'a>>,
    pub where_clauses: &'a [TokenTree],
    pub path_override: Option<Literal>,
    pub generic_field_types: Vec<&'a [TokenTree]>,
    pub transparent_impl: bool,
    pub to_json: bool,
    pub from_json: bool,
    pub to_binary: bool,
    pub from_binary: bool,
    pub to_str: bool,
    pub from_str: bool,
    pub pod: bool,
    pub rename_all: RenameRule,
    pub enum_flags: EnumFlag,
    pub flattenable: bool,
    pub ignore_tag_adjacent_fields: bool,
    pub content: Option<String>,
    pub tag: Tag,
    pub repr: Repr,
    pub version: Option<u16>,
    pub min_version: u16,
}

impl<'a> DeriveTargetInner<'a> {
    pub fn has_lifetime(&self) -> bool {
        self.generics
            .iter()
            .any(|x| matches!(x.kind, GenericKind::Lifetime))
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct Field<'a> {
    pub name: &'a Ident,
    pub ty: &'a [TokenTree],
    #[allow(dead_code)]
    pub attr: &'a FieldAttrs,
    pub flags: u32,
}

impl<'a> EnumVariant<'a> {
    pub fn rename(&self, for_trait: TraitSet) -> Option<&Literal> {
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Rename(lit) = &attr.inner {
                    return Some(lit);
                }
            }
        }
        None
    }
}

impl<'a> Field<'a> {
    pub fn skip(&self, for_trait: TraitSet) -> Option<&[TokenTree]> {
        // todo optimize via flags
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Skip(skip) = &attr.inner {
                    return Some(skip);
                }
            }
        }
        None
    }
    pub fn validate(&self, for_trait: TraitSet) -> Option<&[TokenTree]> {
        // todo optimize via flags
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Validate(with) = &attr.inner {
                    return Some(with);
                }
            }
        }
        None
    }
    pub fn with(&self, for_trait: TraitSet) -> Option<&[TokenTree]> {
        // todo optimize via flags
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::With(with) = &attr.inner {
                    return Some(with);
                }
            }
        }
        None
    }
    pub fn via(&self, for_trait: TraitSet) -> &Via {
        // todo optimize via flags
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Via(via) = &attr.inner {
                    return via;
                }
            }
        }
        &Via::None
    }
    pub fn flatten(&self, for_trait: TraitSet) -> bool {
        // todo optimize via flags
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Flatten = attr.inner {
                    return true;
                }
            }
        }
        false
    }
    pub fn default(&self, for_trait: TraitSet) -> Option<&DefaultKind> {
        for attr in &self.attr.attrs {
            if attr.enabled & for_trait != 0 {
                if let FieldAttrInner::Default(tokens) = &attr.inner {
                    return Some(tokens);
                }
            }
        }
        None
    }
}

impl<'a> Field<'a> {
    pub const GENERIC: u32 = 1u32 << 0;
    pub const IN_TUPLE: u32 = 1u32 << 1;
    // pub const WITH_TO_JSON_FLATTEN: u32 = 1u32 << 2;
    pub const WITH_FROM_JSON_FLATTEN: u32 = 1u32 << 3;
    // pub const WITH_TO_JSON_DEFAULT: u32 = 1u32 << 4;
    pub const WITH_FROM_JSON_DEFAULT: u32 = 1u32 << 5;
    // pub const WITH_TO_JSON_SKIP: u32 = 1u32 << 6;
    pub const WITH_FROM_JSON_SKIP: u32 = 1u32 << 7;
    pub const WITH_TO_BINARY_SKIP: u32 = 1u32 << 8;
    pub const WITH_FROM_BINARY_SKIP: u32 = 1u32 << 9;

    pub const WITH_FROM_JSON_ALIAS: u32 = 1u32 << 10;
    // pub const WITH_DEFAULT: u32 = 1u32 << 5;
    #[allow(dead_code)]
    pub fn is(&self, flags: u32) -> bool {
        self.flags & flags != 0
    }
    #[allow(dead_code)]
    pub fn is_all(&self, flags: u32) -> bool {
        self.flags & flags == flags
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct EnumVariant<'a> {
    pub name: &'a Ident,
    pub fields: &'a [Field<'a>],
    pub kind: EnumKind,
    #[allow(dead_code)]
    pub attr: &'a FieldAttrs,
}

#[cfg_attr(feature = "debug", derive(Debug))]
pub enum EnumKind {
    Tuple,
    Struct,
    None,
}

impl Copy for EnumKind {}
impl Clone for EnumKind {
    fn clone(&self) -> Self {
        *self
    }
}
//@DELETE_START
macro_rules! throw {
    ($literal: literal @ $span: expr, $($tt:tt)*) => {
        Error::span_msg_ctx($literal, &($($tt)*), $span)
    };
    ($literal: literal, $($tt:tt)*) => {
        Error::msg_ctx($literal, &($($tt)*))
    };
    ($literal: literal @ $span: expr) => {
        Error::span_msg($literal, $span)
    };
    ($literal: literal) => {
        Error::msg($literal)
    };
}

macro_rules! expect_next {
    ($ident:ident, $expr:expr) => {
        match ($expr).next() {
            Some(TokenTree::$ident(t)) => t,
            Some(tt) => Error::span_msg_ctx(
                concat!("Expected a ", stringify!($ident), "but found a "),
                &kind_of_token(&tt),
                tt.span(),
            ),
            None => Error::msg("Unexpected EOF"),
        }
    };
}

macro_rules! next {
    ($expr:expr) => {
        match ($expr).next() {
            Some(t) => t,
            None => Error::msg("Unexpected EOF"),
        }
    };
}

fn kind_of_token(token: &TokenTree) -> &'static str {
    match token {
        TokenTree::Group(_) => "Group",
        TokenTree::Ident(_) => "Ident",
        TokenTree::Punct(_) => "Punct",
        TokenTree::Literal(_) => "Literal",
    }
}

// helps reduce LLVM bloat but keeping th drop in here
// of the string here
fn ident_eq(ident: &Ident, value: &str) -> bool {
    ident.to_string() == value
}
fn parse_container_attr(
    target: &mut DeriveTargetInner<'_>,
    attr: Ident,
    mut value: &mut [TokenTree],
) {
    let key = attr.to_string();
    match key.as_str() {
        "transparent" => {
            target.transparent_impl = true;
        }
        "ToJson" => {
            target.to_json = true;
        }
        "zerocopy" => {
            target.pod = true;
        }
        "ignore_tag_adjacent_fields" => {
            target.ignore_tag_adjacent_fields = true;
        }
        "Flattenable" => {
            target.flattenable = true;
        }
        "Str" => {
            target.from_str = true;
            target.to_str = true;
        }
        "FromStr" => {
            target.from_str = true;
        }
        "ToStr" => {
            target.to_str = true;
        }
        "FromJson" => {
            target.from_json = true;
        }
        "ToBinary" => {
            target.to_binary = true;
        }
        "Binary" => {
            target.to_binary = true;
            target.from_binary = true;
        }
        "Json" => {
            target.to_json = true;
            target.from_json = true;
        }
        "FromBinary" => {
            target.from_binary = true;
        }
        "untagged" => match &target.tag {
            Tag::Default => target.tag = Tag::Untagged,
            _ => throw!("Duplicate tag attribute" @ attr.span()),
        },
        "tag" => {
            match target.tag {
                Tag::Inline(_) => throw!("Duplicate tag attribute" @ attr.span()),
                Tag::Untagged => throw!("Cannot have a tag & be untagged" @ attr.span()),
                Tag::Default => (),
            }
            let [TokenTree::Literal(tag_name), rest @ ..] = value else {
                throw!("Expected a tag" @ attr.span())
            };
            value = rest;
            match crate::lit::literal_inline(tag_name.to_string()) {
                crate::lit::InlineKind::String(s) | crate::lit::InlineKind::Raw(s) => {
                    target.tag = Tag::Inline(s)
                }
                crate::lit::InlineKind::None => {
                    throw!("Expected a string literal" @ tag_name.span())
                }
            }
        }
        "version" => {
            if target.version.is_some() {
                Error::span_msg("Duplicate version attribute", attr.span())
            }
            use TokenTree::{Literal as L, Punct as P};
            let (min, current) = 'ok: {
                match value {
                    [L(version_number)] => break 'ok (None, Some(version_number)),
                    [L(min_version), P(d1), P(d2)] => {
                        if d1.as_char() == '.' && d2.as_char() == '.' {
                            break 'ok (Some(min_version), None);
                        }
                    }
                    [L(min_version), P(d1), P(d2), P(eq), L(version_number)] => {
                        if d1.as_char() == '.' && d2.as_char() == '.' && eq.as_char() == '=' {
                            break 'ok (Some(min_version), Some(version_number));
                        }
                    }
                    [] => break 'ok (None, None),
                    _ => (),
                }
                Error::span_msg(
                    "Expected one of `V, MIN.., MIN..=V` or no version",
                    attr.span(),
                );
            };
            if let Some(min) = min {
                match min.to_string().parse::<u16>() {
                    Ok(value) => target.min_version = value,
                    Err(_err) => {
                        Error::span_msg("Expected a version number between 0-65534", min.span())
                    }
                }
            }
            if let Some(current) = current {
                match current.to_string().parse::<u16>() {
                    Ok(value) => target.version = Some(value),
                    Err(_err) => {
                        Error::span_msg("Expected a version number between 0-65534", current.span())
                    }
                }
            } else {
                // Automatic version
                target.version = Some(u16::MAX);
            }
            value = &mut [];
        }
        "content" => {
            if target.content.is_some() {
                throw!("Duplicate content attribute" @ attr.span())
            }
            let [TokenTree::Literal(content_name), rest @ ..] = value else {
                throw!("Expected the field of content" @ attr.span())
            };
            value = rest;
            match crate::lit::literal_inline(content_name.to_string()) {
                crate::lit::InlineKind::String(s) | crate::lit::InlineKind::Raw(s) => {
                    target.content = Some(s)
                }
                crate::lit::InlineKind::None => {
                    throw!("Expected a string literal" @ content_name.span())
                }
            }
        }
        "rename_all" => {
            if target.rename_all != RenameRule::None {
                throw!("Duplicate rename_all attribute" @ attr.span())
            }
            let [TokenTree::Literal(rename), rest @ ..] = value else {
                throw!("Expected a literal" @ attr.span())
            };
            value = rest;
            target.rename_all = RenameRule::from_literal(&rename);
        }
        _ => throw!("Unknown attribute" @ attr.span()),
    }
    if !value.is_empty() {
        throw!("Extra value tokens for" @ attr.span(), attr)
    }
}

fn extract_container_attr(target: &mut DeriveTargetInner, stream: TokenStream) {
    let mut toks = stream.into_iter();
    let Some(TokenTree::Ident(ident)) = toks.next() else {
        return;
    };
    let Some(TokenTree::Group(group)) = toks.next() else {
        return;
    };
    let name = ident.to_string();
    if name == "jsony" {
        parse_attrs(group.stream(), &mut |_, attr, value| {
            parse_container_attr(target, attr, value);
        });
    } else if name == "repr" {
        for toks in group.stream() {
            let TokenTree::Ident(ident) = toks else {
                continue;
            };
            let name = ident.to_string();
            match name.as_str() {
                "transparent" => {
                    target.repr = Repr::Transparent;
                }
                "C" => {
                    target.repr = Repr::C;
                }
                "packed" => {
                    throw!("repr(packed) not supported")
                }
                _ => {
                    target.repr = Repr::Unknown;
                }
            }
        }
    }
}

pub fn extract_derive_target<'a>(
    target: &mut DeriveTargetInner<'a>,
    toks: &'a [TokenTree],
) -> (DeriveTargetKind, TokenStream) {
    let mut toks = toks.iter();
    let kind = loop {
        let ident = match next!(toks) {
            TokenTree::Ident(ident) => ident,
            TokenTree::Punct(ch) if ch.as_char() == '#' => {
                extract_container_attr(target, expect_next!(Group, toks).stream());
                continue;
            }
            _ => continue,
        };
        let ident = ident.to_string();
        match ident.as_str() {
            "struct" => break DeriveTargetKind::Struct,
            "enum" => break DeriveTargetKind::Enum,
            _ => continue,
        }
    };

    target.name = expect_next!(Ident, toks).clone();

    match toks.next() {
        Some(TokenTree::Group(group)) => {
            return (
                if group.delimiter() == Delimiter::Parenthesis {
                    DeriveTargetKind::TupleStruct
                } else {
                    kind
                },
                group.stream(),
            );
        }
        Some(TokenTree::Punct(ch)) if ch.as_char() == '<' => (),
        Some(TokenTree::Punct(ch)) if ch.as_char() == ';' => {
            return (kind, TokenStream::new());
        }
        None => throw!("Empty body"),
        f => throw!("Unhandled feature", f.unwrap()),
    }
    'parsing_generics: while let Some(tt) = toks.next() {
        let mut keep = true;
        let (kind, ident, at_colon) = match tt {
            TokenTree::Ident(ident) => match next!(toks) {
                TokenTree::Group(_) => throw!("Unexpected group"),
                TokenTree::Ident(next_ident) => {
                    if ident_eq(ident, "const") {
                        throw!("unexpected ident", &next_ident)
                    }
                    (GenericKind::Const, next_ident, false)
                }
                TokenTree::Punct(ch) => match ch.as_char() as u8 {
                    b':' => (GenericKind::Type, ident, true),
                    b'=' => {
                        keep = false;
                        (GenericKind::Type, ident, true)
                    }
                    b',' => {
                        target.generics.push(Generic {
                            kind: GenericKind::Type,
                            ident,
                            bounds: &[],
                        });
                        continue;
                    }
                    b'>' => {
                        target.generics.push(Generic {
                            kind: GenericKind::Type,
                            ident,
                            bounds: &[],
                        });
                        break 'parsing_generics;
                    }
                    chr => {
                        throw!("Unexpected token after first ident in generic", chr)
                    }
                },
                tok => {
                    throw!("Unexpected token after first ident in generic", tok)
                }
            },
            TokenTree::Punct(p) => {
                let ch = p.as_char();
                if ch == '\'' {
                    match next!(toks) {
                        TokenTree::Ident(ident) => (GenericKind::Lifetime, ident, false),
                        _ => {
                            throw!("expected ident")
                        }
                    }
                } else {
                    if ch == ',' {
                        continue;
                    }
                    if ch == '>' {
                        break 'parsing_generics;
                    }
                    throw!("Unexpected Punct")
                }
            }
            TokenTree::Group(_) => {
                throw!("Unhanlded");
            }
            _ => throw!("Unhanlded"),
        };
        target.generics.push(Generic {
            kind,
            ident,
            bounds: &[],
        });
        let Some(generic) = target.generics.last_mut() else {
            // Due to using cargo-expand we can't use the unreachable macro here
            unsafe { std::hint::unreachable_unchecked() }
        };
        if !at_colon {
            // could have we attr
            match next!(toks) {
                TokenTree::Punct(ch) => match ch.as_char() {
                    ',' => {
                        continue;
                    }
                    '>' => {
                        break 'parsing_generics;
                    }

                    ':' => (),
                    _ => throw!("unexpected char"),
                },
                _ => throw!("Unexpected tok"),
            }
        }
        let from = toks.as_slice();
        let mut depth = 0i32;
        loop {
            let tok = next!(toks);
            if let TokenTree::Punct(p) = &tok {
                match p.as_char() as u8 {
                    b',' => break,
                    b'=' => {
                        if depth == 0 {
                            generic.bounds = if keep {
                                &from[..(from.len() - toks.len()) - 1]
                            } else {
                                &[]
                            };
                            keep = false;
                        }
                    }
                    b'<' => depth += 1,
                    b'>' => {
                        depth -= 1;
                        if depth < 0 {
                            generic.bounds = if keep {
                                &from[..(from.len() - toks.len()) - 1]
                            } else {
                                &[]
                            };
                            break 'parsing_generics;
                        }
                    }
                    _ => (),
                }
            }
        }
        generic.bounds = if keep {
            &from[..(from.len() - toks.len()) - 1]
        } else {
            &[]
        };
    }
    match next!(toks) {
        TokenTree::Group(group) => (
            if group.delimiter() == Delimiter::Parenthesis {
                DeriveTargetKind::TupleStruct
            } else {
                kind
            },
            group.stream(),
        ),
        TokenTree::Ident(tok) => {
            if ident_eq(tok, "where") {
                throw!("Expected where clause" @ tok.span());
            }
            let [where_clauses @ .., TokenTree::Group(group)] = toks.as_slice() else {
                throw!("Expected body after where clauses")
            };
            target.where_clauses = where_clauses;
            (
                if group.delimiter() == Delimiter::Parenthesis {
                    DeriveTargetKind::TupleStruct
                } else {
                    kind
                },
                group.stream(),
            )
        }
        tok => throw!("Expected either body or where clause", tok),
    }
}

const TRAIT_COUNT: u64 = 4;
fn parse_single_field_attr(
    attrs: &mut FieldAttrs,
    mut trait_set: TraitSet,
    ident: Ident,
    value: &mut Vec<TokenTree>,
) {
    let name = ident.to_string();
    if trait_set == 0 {
        trait_set = TO_JSON | FROM_JSON | TO_BINARY | FROM_BINARY;
    }
    let offset = match name.as_str() {
        "rename" => {
            let Some(TokenTree::Literal(rename)) = value.pop() else {
                throw!("Expected a literal" @ ident.span())
            };
            if !value.is_empty() {
                throw!("Unexpected a single literal" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Rename(rename),
            });
            0u64 * TRAIT_COUNT
        }
        "via" => {
            let Some(TokenTree::Ident(via_ident)) = value.pop() else {
                throw!("Expected a value" @ ident.span())
            };
            if !value.is_empty() {
                throw!("Unexpected a single literal" @ ident.span())
            }
            let via = via_ident.to_string();
            match via.as_str() {
                "Iterator" => {
                    attrs.attrs.push(FieldAttr {
                        enabled: trait_set,
                        span: ident.span(),
                        inner: FieldAttrInner::Via(Via::Iterator),
                    });
                }
                _ => {
                    throw!("Unknown via value" @ via_ident.span())
                }
            }

            1u64 * TRAIT_COUNT
        }
        "default" => {
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Default(if value.is_empty() {
                    DefaultKind::Default
                } else {
                    DefaultKind::Custom(std::mem::take(value))
                }),
            });
            3u64 * TRAIT_COUNT
        }
        "flatten" => {
            if !value.is_empty() {
                throw!("flatten doesn't take any arguments" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Flatten,
            });
            4u64 * TRAIT_COUNT
        }
        "with" => {
            if (attrs.flags >> (10 * TRAIT_COUNT)) & FROM_JSON as u64 != 0 {
                throw!("`with` and `validate` are not supported for the same field" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::With(std::mem::take(value)),
            });
            5u64 * TRAIT_COUNT
        }
        "skip" => {
            if !value.is_empty() {
                throw!("flatten doesn't take any arguments" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Skip(Vec::new()),
            });
            6u64 * TRAIT_COUNT
        }
        "skip_if" => {
            if value.is_empty() {
                throw!("Unexpected a function specifying skip criteria" @ ident.span())
            }
            // only supports for TO_JSON
            trait_set &= TO_JSON;
            attrs.attrs.push(FieldAttr {
                enabled: trait_set & TO_JSON,
                span: ident.span(),
                inner: FieldAttrInner::Skip(std::mem::take(value)),
            });
            6u64 * TRAIT_COUNT
        }
        "other" => {
            if !value.is_empty() {
                throw!("other doesn't take any arguments" @ ident.span())
            }
            // Note: Currently this value used for `has_other` and must be changed there.
            // Todo use less error prone mechanism to store this.
            7u64 * TRAIT_COUNT
        }
        "alias" => {
            let Some(TokenTree::Literal(alias)) = value.pop() else {
                throw!("Expected a literal" @ ident.span())
            };
            if !value.is_empty() {
                throw!("Unexpected a single literal" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Alias(alias),
            });
            8u64 * TRAIT_COUNT
        }
        "version" => {
            let Some(TokenTree::Literal(version)) = value.pop() else {
                throw!("Expected a version number" @ ident.span())
            };
            if !value.is_empty() {
                throw!("Unexpected a single number" @ ident.span())
            }

            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Version(version),
            });
            9u64 * TRAIT_COUNT
        }
        "validate" => {
            if (attrs.flags >> (5 * TRAIT_COUNT)) & FROM_JSON as u64 != 0 {
                throw!("`with` and `validate` not supported for the same field" @ ident.span())
            }
            attrs.attrs.push(FieldAttr {
                enabled: trait_set,
                span: ident.span(),
                inner: FieldAttrInner::Validate(std::mem::take(value)),
            });
            10u64 * TRAIT_COUNT
        }
        _ => throw!("Unknown attr field" @ ident.span()),
    };
    let mask = (trait_set as u64) << offset;
    if attrs.flags & mask != 0 {
        throw!("Duplicate attribute" @ ident.span())
    }
    attrs.flags |= mask;
}

fn extract_jsony_attr(group: TokenStream) -> Option<TokenStream> {
    let mut toks = group.into_iter();
    {
        let Some(TokenTree::Ident(ident)) = toks.next() else {
            return None;
        };
        if !ident_eq(&ident, "jsony") {
            return None;
        }
    }
    let Some(TokenTree::Group(group)) = toks.next() else {
        return None;
    };
    Some(group.stream())
}

pub type TraitSet = u8;
pub const TO_JSON: TraitSet = 1 << 0;
pub const FROM_JSON: TraitSet = 1 << 1;
pub const TO_BINARY: TraitSet = 1 << 2;
pub const FROM_BINARY: TraitSet = 1 << 3;
fn parse_attrs(toks: TokenStream, func: &mut dyn FnMut(TraitSet, Ident, &mut Vec<TokenTree>)) {
    let mut toks = toks.into_iter();
    let mut buf: Vec<TokenTree> = Vec::new();
    'outer: while let Some(tok) = toks.next() {
        let TokenTree::Ident(mut ident) = tok else {
            throw!("Expected ident" @ tok.span())
        };
        let mut trait_set = 0;
        'processing: loop {
            if let Some(sep) = toks.next() {
                let sep = match sep {
                    TokenTree::Punct(sep) => sep,
                    TokenTree::Ident(true_ident) => {
                        let text = ident.to_string();
                        match text.as_str() {
                            "ToJson" => trait_set |= TO_JSON,
                            "FromJson" => trait_set |= FROM_JSON,
                            "ToBinary" => trait_set |= TO_BINARY,
                            "FromBinary" => trait_set |= FROM_BINARY,
                            "To" => trait_set |= TO_JSON | TO_BINARY,
                            "From" => trait_set |= FROM_JSON | FROM_BINARY,
                            _ => throw!("Expected trait or alias" @ ident.span()),
                        }
                        ident = true_ident;
                        continue 'processing;
                    }
                    _ => {
                        throw!("Expected either `=` or `,`" @ sep.span());
                    }
                };
                match sep.as_char() {
                    '=' => (),
                    ',' => {
                        func(trait_set, ident, &mut buf);
                        continue 'outer;
                    }
                    _ => throw!("Expected either `=` or `,`" @ sep.span()),
                }
                let mut in_pipe = false;
                for tok in toks.by_ref() {
                    if let TokenTree::Punct(punct) = &tok {
                        if punct.as_char() == '|' {
                            in_pipe ^= true;
                        }
                        if punct.as_char() == ',' && !in_pipe {
                            break;
                        }
                    }
                    buf.push(tok);
                }
            }
            break;
        }
        func(trait_set, ident, &mut buf);
        buf.clear();
    }
}

fn parse_field_attr<'a>(
    current: &mut Option<&'a mut FieldAttrs>,
    attr_buf: &mut Allocator<'a, FieldAttrs>,
    toks: TokenStream,
) {
    let Some(attrs) = extract_jsony_attr(toks) else {
        return;
    };
    let attr = current.get_or_insert_with(|| attr_buf.alloc_default());
    parse_attrs(attrs, &mut |set, ident, buf| {
        parse_single_field_attr(attr, set, ident, buf)
    })
}

struct VariantTemp<'a> {
    name: &'a Ident,
    start: usize,
    end: usize,
    attr: &'a FieldAttrs,
    kind: EnumKind,
}

pub fn scan_fields<'a>(target: &mut DeriveTargetInner<'a>, fields: &mut Vec<Field<'a>>) {
    let type_generic_names: Vec<_> = target
        .generics
        .iter()
        .filter_map(|a| {
            if !matches!(a.kind, GenericKind::Type) {
                return None;
            }
            Some(a.ident.to_string())
        })
        .collect();
    let min_len = type_generic_names.iter().map(|x| x.len()).max();

    let mut max_version = target.min_version;
    for field in fields {
        if let Some(version) = field.attr.version() {
            if let Ok(num) = version.to_string().parse::<u16>() {
                if target.version.is_none() {
                    throw!("field versions require a version attribute on the container" @ version.span())
                }
                if max_version < num {
                    max_version = num;
                }
            } else {
                throw!("Expected a version number between 0-65534" @ version.span())
            }
        }
        if let Some(min_length) = min_len {
            for tt in field.ty {
                let TokenTree::Ident(ident) = tt else {
                    continue;
                };
                let ident = ident.to_string();
                if ident.len() < min_length {
                    continue;
                }
                if type_generic_names.iter().any(|x| ident == *x) {
                    field.flags |= Field::GENERIC;
                    target.generic_field_types.push(field.ty);
                    break;
                }
            }
        }
    }

    if let Some(version) = target.version {
        if version == u16::MAX {
            target.version = Some(max_version);
        } else if max_version > version {
            throw!("Field version is greater than container version")
        }
    }
}

pub fn parse_enum<'a>(
    target: &mut DeriveTargetInner<'a>,
    fields: &'a [TokenTree],
    tt_buf: &'a mut Vec<TokenTree>,
    field_buf: &'a mut Vec<Field<'a>>,
    attr_buf: &mut Allocator<'a, FieldAttrs>,
) -> Vec<EnumVariant<'a>> {
    let mut temp = parse_inner_enum_variants(fields, tt_buf, attr_buf);
    match &target.tag {
        Tag::Inline(_) => target.enum_flags |= ENUM_HAS_INLINE_TAG,
        Tag::Untagged => target.enum_flags |= ENUM_HAS_NO_TAG,
        Tag::Default => target.enum_flags |= ENUM_HAS_EXTERNAL_TAG,
    }
    {
        for variant in &mut temp {
            match variant.kind {
                EnumKind::Tuple => {
                    target.enum_flags |= ENUM_CONTAINS_TUPLE_VARIANT;
                    let start = field_buf.len();
                    parse_tuple_fields(
                        variant.name,
                        field_buf,
                        &tt_buf[variant.start..variant.end],
                        attr_buf,
                    );
                    variant.start = start;
                    variant.end = field_buf.len();
                }
                EnumKind::Struct => {
                    target.enum_flags |= ENUM_CONTAINS_STRUCT_VARIANT;
                    let start = field_buf.len();
                    parse_struct_fields(field_buf, &tt_buf[variant.start..variant.end], attr_buf);
                    variant.start = start;
                    variant.end = field_buf.len();
                }
                EnumKind::None => {
                    target.enum_flags |= ENUM_CONTAINS_UNIT_VARIANT;
                }
            }
        }
    }
    scan_fields(target, field_buf);
    temp.into_iter()
        .map(|var| EnumVariant {
            name: var.name,
            fields: if matches!(var.kind, EnumKind::None) {
                &[]
            } else {
                &field_buf[var.start..var.end]
            },
            kind: var.kind,
            attr: var.attr,
        })
        .collect()
}

fn parse_inner_enum_variants<'a>(
    fields: &'a [TokenTree],
    tt_buffer: &mut Vec<TokenTree>,
    attr_buffer: &mut Allocator<'a, FieldAttrs>,
) -> Vec<VariantTemp<'a>> {
    let mut f = fields.iter().enumerate();
    let mut enums: Vec<VariantTemp<'a>> = Vec::new();
    let mut next_attr: Option<&'a mut FieldAttrs> = None;
    loop {
        let i = if let Some((i, tok)) = f.next() {
            let TokenTree::Punct(punct) = tok else {
                continue;
            };
            let ch = punct.as_char() as u8;
            if ch == b'#' {
                let Some((_, TokenTree::Group(group))) = f.next() else {
                    throw!("Expected attr after" @ punct.span())
                };
                parse_field_attr(&mut next_attr, attr_buffer, group.stream());
                continue;
            }
            if ch == b',' {
            } else if ch == b'=' {
                let mut colon_stage = 0;
                while let Some((_, tok)) = f.next() {
                    let TokenTree::Punct(punct) = tok else {
                        continue;
                    };
                    match punct.as_char() as u8 {
                        b':' => {
                            colon_stage += 1;
                            continue;
                        }
                        b'<' => {
                            if colon_stage == 2 {
                                let mut depth = 1i32;
                                loop {
                                    let Some((_e, tok)) = f.next() else {
                                        throw!(
                                            "Unexpected EOF while parsing type in enum expression"
                                        );
                                    };
                                    let TokenTree::Punct(punct) = tok else {
                                        continue;
                                    };
                                    match punct.as_char() as u8 {
                                        b'<' => depth += 1,
                                        b'>' => {
                                            depth -= 1;
                                            if depth <= 0 {
                                                break;
                                            }
                                        }
                                        _ => continue,
                                    }
                                }
                            }
                        }
                        b',' => break,
                        _ => (),
                    }
                    colon_stage = 0;
                }
            } else {
                continue;
            };
            i
        } else {
            fields.len()
        };

        let Some(tok) = fields.get(i.saturating_sub(1)) else {
            throw!("Baddness")
        };

        let start = tt_buffer.len();
        let (name, kind) = match tok {
            TokenTree::Group(group) => {
                tt_buffer.extend(group.stream());
                let Some(TokenTree::Ident(ident)) = fields.get(i.saturating_sub(2)) else {
                    throw!("Expected ident" @ group.span())
                };
                (
                    ident,
                    if group.delimiter() == Delimiter::Brace {
                        EnumKind::Struct
                    } else {
                        EnumKind::Tuple
                    },
                )
            }
            TokenTree::Ident(ident) => (ident, EnumKind::None),
            tok => throw!("Expected either an ident or group" @ tok.span()),
        };
        enums.push(VariantTemp {
            name,
            start,
            end: tt_buffer.len(),
            attr: if let Some(attr) = next_attr.take() {
                attr
            } else {
                &DEFAULT_ATTR.0
            },
            kind,
        });
        if f.len() == 0 {
            break;
        }
    }
    enums
}

fn flags_from_attr(attr: &Option<&mut FieldAttrs>) -> u32 {
    let mut f = 0;
    if let Some(attr) = attr {
        for attr in &attr.attrs {
            let enabled = attr.enabled as u32;
            match &attr.inner {
                FieldAttrInner::Flatten => f |= (enabled & 0b11) << 2,
                FieldAttrInner::Default(..) => f |= (enabled & 0b11) << 4,
                FieldAttrInner::Skip(..) => f |= (enabled & 0b1111) << 6,
                FieldAttrInner::Alias(..) => {
                    if attr.enabled & FROM_JSON != 0 {
                        f |= Field::WITH_FROM_JSON_ALIAS;
                    }
                }
                _ => (),
            }
        }
    }
    f
}

pub fn parse_tuple_fields<'a>(
    fake_name: &'a Ident,
    output: &mut Vec<Field<'a>>,
    fields: &'a [TokenTree],
    attr_buf: &mut Allocator<'a, FieldAttrs>,
) {
    let mut f = fields.iter().enumerate();
    let mut next_attr: Option<&mut FieldAttrs> = None;
    while let Some((mut i, tok)) = f.next() {
        if let TokenTree::Punct(punct) = tok {
            if punct.as_char() == '#' {
                let Some((_, TokenTree::Group(group))) = f.next() else {
                    throw!("Expected attr after" @ punct.span())
                };
                parse_field_attr(&mut next_attr, attr_buf, group.stream());
                continue;
            }
        };
        let mut depth = 0i32;
        let end = loop {
            let Some((e, tok)) = f.next() else {
                break fields.len();
            };
            let TokenTree::Punct(punct) = tok else {
                continue;
            };
            match punct.as_char() as u8 {
                b'<' => depth += 1,
                b'>' => depth -= 1,
                b',' if depth <= 0 => break e,
                _ => continue,
            }
        };

        // Remove visibility to store just the type
        if let TokenTree::Ident(ident) = &fields[i] {
            if ident_eq(ident, "pub") {
                i += 1;
                if i + 1 != end {
                    if let TokenTree::Group(group) = &fields[i] {
                        if group.delimiter() == Delimiter::Parenthesis {
                            i += 1;
                        }
                    }
                }
            }
        }

        output.push(Field {
            name: fake_name,
            ty: &fields[i..end],
            flags: Field::IN_TUPLE | flags_from_attr(&next_attr),
            attr: if let Some(attr) = next_attr.take() {
                attr
            } else {
                &DEFAULT_ATTR.0
            },
        })
    }
}

struct DefaultFieldAttr(FieldAttrs);
unsafe impl Sync for DefaultFieldAttr {}
static DEFAULT_ATTR: DefaultFieldAttr = const {
    DefaultFieldAttr(FieldAttrs {
        attrs: Vec::new(),
        flags: 0,
    })
};

pub fn parse_struct_fields<'a>(
    output: &mut Vec<Field<'a>>,
    fields: &'a [TokenTree],
    attr_buf: &mut Allocator<'a, FieldAttrs>,
) {
    let mut f = fields.iter().enumerate();
    let mut next_attr: Option<&'a mut FieldAttrs> = None;
    while let Some((i, tok)) = f.next() {
        let TokenTree::Punct(punct) = tok else {
            continue;
        };
        let ch = punct.as_char() as u8;
        if ch == b'#' {
            let Some((_, TokenTree::Group(group))) = f.next() else {
                throw!("Expected attr after" @ punct.span())
            };
            parse_field_attr(&mut next_attr, attr_buf, group.stream());
            continue;
        }
        if ch != b':' {
            continue;
        }
        let Some(TokenTree::Ident(name)) = fields.get(i.wrapping_sub(1)) else {
            throw!("Expected field name before :" @ punct.span())
        };
        let mut depth = 0i32;
        let end = loop {
            let Some((e, tok)) = f.next() else {
                break fields.len();
            };
            let TokenTree::Punct(punct) = tok else {
                continue;
            };
            match punct.as_char() as u8 {
                b'<' => depth += 1,
                b'>' => depth -= 1,
                b',' if depth <= 0 => break e,
                _ => continue,
            }
        };
        if let [TokenTree::Ident(ident), TokenTree::Punct(punct), ..] = &fields[i + 1..end] {
            if punct.as_char() == '<' && ident_eq(ident, "Option") {
                let attr = next_attr.get_or_insert_with(|| attr_buf.alloc_default());
                let oo_mask = (FROM_JSON as u64) << (3u64 * TRAIT_COUNT);
                if attr.flags & oo_mask == 0 {
                    attr.attrs.push(FieldAttr {
                        enabled: FROM_JSON,
                        span: ident.span(),
                        inner: FieldAttrInner::Default(DefaultKind::Default),
                    });
                }
            }
        }
        output.push(Field {
            name,
            ty: &fields[i + 1..end],
            flags: Field::IN_TUPLE | flags_from_attr(&next_attr),
            attr: if let Some(attr) = next_attr.take() {
                attr
            } else {
                &DEFAULT_ATTR.0
            },
        })
    }
}
