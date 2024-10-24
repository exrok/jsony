// mod template2;

use crate::{case::RenameRule, util::Allocator, Error};
use proc_macro::{Delimiter, Ident, Literal, TokenStream, TokenTree};
pub enum GenericKind {
    Lifetime,
    Type,
    Const,
}
pub struct Generic<'a> {
    pub kind: GenericKind,
    pub ident: &'a Ident,
    pub bounds: &'a [TokenTree],
}
pub enum DeriveTargetKind {
    TupleStruct,
    Struct,
    Enum,
}
pub struct FieldAttr {
    pub rename: Option<Literal>,
    /// eventually probably have kind here but lets keep it simple for now
    pub flatten: bool,
    pub default: Option<Vec<TokenTree>>,
}
#[allow(clippy::derivable_impls)]
impl Default for FieldAttr {
    fn default() -> Self {
        Self {
            rename: Default::default(),
            flatten: false,
            default: None,
        }
    }
}
pub enum Tag {
    Inline(String),
    Untagged,
    Default,
}
pub enum Repr {
    Transparent,
    C,
    Default,
    Unknown,
}
pub struct DeriveTargetInner<'a> {
    pub name: Ident,
    pub generics: Vec<Generic<'a>>,
    pub where_clauses: &'a [TokenTree],
    pub path_override: Option<Literal>,
    pub generic_field_types: Vec<&'a [TokenTree]>,
    pub to_json: bool,
    pub from_json: bool,
    pub to_binary: bool,
    pub from_binary: bool,
    pub rename_all: RenameRule,
    pub flattenable: bool,
    pub content: Option<String>,
    pub tag: Tag,
    pub repr: Repr,
}
impl<'a> DeriveTargetInner<'a> {
    pub fn has_lifetime(&self) -> bool {
        self.generics.iter().any(|x| match x.kind {
            GenericKind::Lifetime => true,
            _ => false,
        })
    }
}
pub struct Field<'a> {
    pub name: &'a Ident,
    pub ty: &'a [TokenTree],
    #[allow(dead_code)]
    pub attr: &'a FieldAttr,
    pub flags: u32,
}
impl<'a> Field<'a> {
    pub const GENERIC: u32 = 1u32 << 0;
    pub const IN_TUPLE: u32 = 1u32 << 1;
    pub const WITH_FLATTEN: u32 = 1u32 << 2;
    pub const WITH_DEFAULT: u32 = 1u32 << 3;
    #[allow(dead_code)]
    pub fn is(&self, flags: u32) -> bool {
        self.flags & flags != 0
    }
    #[allow(dead_code)]
    pub fn is_all(&self, flags: u32) -> bool {
        self.flags & flags == flags
    }
}
pub struct EnumVariant<'a> {
    pub name: &'a Ident,
    pub fields: &'a [Field<'a>],
    pub kind: EnumKind,
    #[allow(dead_code)]
    pub attr: &'a FieldAttr,
}
pub enum EnumKind {
    Tuple,
    Struct,
    None,
}
fn kind_of_token(token: &TokenTree) -> &'static str {
    match token {
        TokenTree::Group(_) => "Group",
        TokenTree::Ident(_) => "Ident",
        TokenTree::Punct(_) => "Punct",
        TokenTree::Literal(_) => "Literal",
    }
}
fn ident_eq(ident: &Ident, value: &str) -> bool {
    ident.to_string() == value
}
fn parse_container_attr(
    target: &mut DeriveTargetInner<'_>,
    attr: Ident,
    mut value: &mut [TokenTree],
) -> Result<(), Error> {
    let key = attr.to_string();
    match key.as_str() {
        "ToJson" => {
            target.to_json = true;
        }
        "Flattenable" => {
            target.flattenable = true;
        }
        "FromJson" => {
            target.from_json = true;
        }
        "ToBinary" => {
            target.to_binary = true;
        }
        "FromBinary" => {
            target.from_binary = true;
        }
        "untagged" => match &target.tag {
            Tag::Default => target.tag = Tag::Untagged,
            _ => return Err(Error::span_msg("Duplicate tag attribute", attr.span())),
        },
        "tag" => {
            match target.tag {
                Tag::Inline(_) => {
                    return Err(Error::span_msg("Duplicate tag attribute", attr.span()))
                }
                Tag::Untagged => {
                    return Err(Error::span_msg(
                        "Cannot of a tag & be untagged",
                        attr.span(),
                    ))
                }
                Tag::Default => (),
            }
            let [TokenTree::Literal(tag_name), rest @ ..] = value else {
                return Err(Error::span_msg("Expected a tag", attr.span()));
            };
            value = rest;
            match crate::lit::literal_inline(tag_name.to_string()) {
                crate::lit::InlineKind::String(s) | crate::lit::InlineKind::Raw(s) => {
                    target.tag = Tag::Inline(s)
                }
                crate::lit::InlineKind::None => {
                    return Err(Error::span_msg(
                        "Expected a string literal",
                        tag_name.span(),
                    ))
                }
            }
        }
        "content" => {
            if target.content.is_some() {
                return Err(Error::span_msg("Duplicate content attribute", attr.span()));
            }
            let [TokenTree::Literal(content_name), rest @ ..] = value else {
                return Err(Error::span_msg(
                    "Expected the field of content",
                    attr.span(),
                ));
            };
            value = rest;
            match crate::lit::literal_inline(content_name.to_string()) {
                crate::lit::InlineKind::String(s) | crate::lit::InlineKind::Raw(s) => {
                    target.content = Some(s)
                }
                crate::lit::InlineKind::None => {
                    return Err(Error::span_msg(
                        "Expected a string literal",
                        content_name.span(),
                    ))
                }
            }
        }
        "rename_all" => {
            if target.rename_all != RenameRule::None {
                return Err(Error::span_msg(
                    "Duplicate rename_all attribute",
                    attr.span(),
                ));
            }
            let [TokenTree::Literal(rename), rest @ ..] = value else {
                return Err(Error::span_msg("Expected a literal", attr.span()));
            };
            value = rest;
            target.rename_all = match RenameRule::from_literal(&rename) {
                Ok(value) => value,
                Err(err) => return Err(err),
            }
        }
        _ => return Err(Error::span_msg("Unknown attribute", attr.span())),
    }
    if !value.is_empty() {
        return Err(Error::span_msg_ctx(
            "Extra value tokens for",
            &(attr),
            attr.span(),
        ));
    }
    Ok(())
}
fn extract_container_attr(
    target: &mut DeriveTargetInner,
    stream: TokenStream,
) -> Result<(), Error> {
    let mut toks = stream.into_iter();
    let Some(TokenTree::Ident(ident)) = toks.next() else {
        return Ok(());
    };
    let Some(TokenTree::Group(group)) = toks.next() else {
        return Ok(());
    };
    let name = ident.to_string();
    if name == "jsony" {
        parse_attrs(
            group.stream(),
            &mut (|attr, value| parse_container_attr(target, attr, value)),
        )
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
                "packet" => return Err(Error::msg("repr(packed) not supported")),
                _ => {
                    target.repr = Repr::Unknown;
                }
            }
        }
        Ok(())
    } else {
        Ok(())
    }
}
pub fn extract_derive_target<'a>(
    target: &mut DeriveTargetInner<'a>,
    toks: &'a [TokenTree],
) -> Result<(DeriveTargetKind, TokenStream), Error> {
    let mut toks = toks.iter();
    let kind = loop {
        let ident = match match (toks).next() {
            Some(t) => t,
            None => return Err(Error::msg("Unexpected EOF")),
        } {
            TokenTree::Ident(ident) => ident,
            TokenTree::Punct(ch) if ch.as_char() == '#' => {
                extract_container_attr(
                    target,
                    match (toks).next() {
                        Some(TokenTree::Group(t)) => t,
                        Some(tt) => {
                            return Err(Error::span_msg_ctx(
                                "Expected a Groupbut found a ",
                                &kind_of_token(&tt),
                                tt.span(),
                            ))
                        }
                        None => return Err(Error::msg("Unexpected EOF")),
                    }
                    .stream(),
                )?;
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
    target.name = match (toks).next() {
        Some(TokenTree::Ident(t)) => t,
        Some(tt) => {
            return Err(Error::span_msg_ctx(
                "Expected a Identbut found a ",
                &kind_of_token(&tt),
                tt.span(),
            ))
        }
        None => return Err(Error::msg("Unexpected EOF")),
    }
    .clone();
    match toks.next() {
        Some(TokenTree::Group(group)) => {
            return Ok((
                if group.delimiter() == Delimiter::Parenthesis {
                    DeriveTargetKind::TupleStruct
                } else {
                    kind
                },
                group.stream(),
            ));
        }
        Some(TokenTree::Punct(ch)) if ch.as_char() == '<' => (),
        Some(TokenTree::Punct(ch)) if ch.as_char() == ';' => {
            return Ok((kind, TokenStream::new()));
        }
        None => return Err(Error::msg("Empty body")),
        f => return Err(Error::msg_ctx("Unhandled feature", &(f.unwrap()))),
    }
    'parsing_generics: while let Some(tt) = toks.next() {
        let mut keep = true;
        let (kind, ident, at_colon) = match tt {
            TokenTree::Ident(ident) => match match (toks).next() {
                Some(t) => t,
                None => return Err(Error::msg("Unexpected EOF")),
            } {
                TokenTree::Group(_) => return Err(Error::msg("Unexpected group")),
                TokenTree::Ident(next_ident) => {
                    if ident_eq(&ident, "const") {
                        return Err(Error::msg_ctx("unexpeced ident", &(&next_ident)));
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
                        return Err(Error::msg_ctx(
                            "Unexpected token after first ident in generic",
                            &(chr),
                        ))
                    }
                },
                tok => {
                    return Err(Error::msg_ctx(
                        "Unexpected token after first ident in generic",
                        &(tok),
                    ))
                }
            },
            TokenTree::Punct(p) => {
                let ch = p.as_char();
                if ch == '\'' {
                    match match (toks).next() {
                        Some(t) => t,
                        None => return Err(Error::msg("Unexpected EOF")),
                    } {
                        TokenTree::Ident(ident) => (GenericKind::Lifetime, ident, false),
                        _ => return Err(Error::msg("expected ident")),
                    }
                } else {
                    if ch == ',' {
                        continue;
                    }
                    if ch == '>' {
                        break 'parsing_generics;
                    }
                    return Err(Error::msg("Unexpected Punct"));
                }
            }
            TokenTree::Group(_) => {
                return Err(Error::msg("Unhanlded"));
            }
            _ => return Err(Error::msg("Unhanlded")),
        };
        target.generics.push(Generic {
            kind,
            ident,
            bounds: &[],
        });
        let Some(generic) = target.generics.last_mut() else {
            unsafe { std::hint::unreachable_unchecked() }
        };
        if !at_colon {
            match match (toks).next() {
                Some(t) => t,
                None => return Err(Error::msg("Unexpected EOF")),
            } {
                TokenTree::Punct(ch) => match ch.as_char() {
                    ',' => {
                        continue;
                    }
                    '>' => {
                        break 'parsing_generics;
                    }
                    ':' => (),
                    _ => return Err(Error::msg("unexpected char")),
                },
                _ => return Err(Error::msg("Unexpected tok")),
            }
        }
        let from = toks.as_slice();
        let mut depth = 0i32;
        loop {
            let tok = match (toks).next() {
                Some(t) => t,
                None => return Err(Error::msg("Unexpected EOF")),
            };
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
    match match (toks).next() {
        Some(t) => t,
        None => return Err(Error::msg("Unexpected EOF")),
    } {
        TokenTree::Group(group) => Ok((
            if group.delimiter() == Delimiter::Parenthesis {
                DeriveTargetKind::TupleStruct
            } else {
                kind
            },
            group.stream(),
        )),
        TokenTree::Ident(tok) => {
            if ident_eq(tok, "where") {
                return Err(Error::span_msg("Expected where clause", tok.span()));
            }
            let [where_clauses @ .., TokenTree::Group(group)] = toks.as_slice() else {
                return Err(Error::msg("Expected body after where clauses"));
            };
            target.where_clauses = where_clauses;
            Ok((
                if group.delimiter() == Delimiter::Parenthesis {
                    DeriveTargetKind::TupleStruct
                } else {
                    kind
                },
                group.stream(),
            ))
        }
        tok => {
            return Err(Error::msg_ctx(
                "Expected either body or where clause",
                &(tok),
            ))
        }
    }
}
fn parse_single_field_attr(
    attrs: &mut FieldAttr,
    ident: Ident,
    value: &mut Vec<TokenTree>,
) -> Result<(), Error> {
    let name = ident.to_string();
    match name.as_str() {
        "rename" => {
            if attrs.rename.is_some() {
                return Err(Error::span_msg("Duplicate rename attribute", ident.span()));
            }
            let Some(TokenTree::Literal(rename)) = value.pop() else {
                return Err(Error::span_msg("Expected a literal", ident.span()));
            };
            if !value.is_empty() {
                return Err(Error::span_msg("Unexpected a single literal", ident.span()));
            }
            attrs.rename = Some(rename);
            Ok(())
        }
        "default" => {
            if attrs.default.is_some() {
                return Err(Error::span_msg("Duplicate rename attribute", ident.span()));
            }
            attrs.default = Some(if value.is_empty() {
                crate::default_call_tokens(ident.span())
            } else {
                std::mem::take(value)
            });
            Ok(())
        }
        "flatten" => {
            if attrs.flatten {
                return Err(Error::span_msg("Duplicate rename attribute", ident.span()));
            }
            if !value.is_empty() {
                return Err(Error::span_msg(
                    "flatten doesn't take any arguments",
                    ident.span(),
                ));
            }
            attrs.flatten = true;
            Ok(())
        }
        _ => return Err(Error::span_msg("Unknown attr field", ident.span())),
    }
}
fn extract_jsony_attr(group: TokenStream) -> Option<TokenStream> {
    let mut toks = group.into_iter();
    {
        let Some(TokenTree::Ident(ident)) = toks.next() else {
            return None;
        };
        if ident.to_string() != "jsony" {
            return None;
        }
    }
    let Some(TokenTree::Group(group)) = toks.next() else {
        return None;
    };
    Some(group.stream())
}
fn parse_attrs(
    toks: TokenStream,
    func: &mut dyn FnMut(Ident, &mut Vec<TokenTree>) -> Result<(), Error>,
) -> Result<(), Error> {
    let mut toks = toks.into_iter();
    let mut buf: Vec<TokenTree> = Vec::new();
    while let Some(tok) = toks.next() {
        let TokenTree::Ident(ident) = tok else {
            return Err(Error::span_msg("Expected ident", tok.span()));
        };
        if let Some(sep) = toks.next() {
            let TokenTree::Punct(sep) = sep else {
                return Err(Error::span_msg("Expected either `=` or `,`", sep.span()));
            };
            match sep.as_char() {
                '=' => (),
                ',' => {
                    if let Err(err) = func(ident, &mut buf) {
                        return Err(err);
                    }
                    continue;
                }
                _ => return Err(Error::span_msg("Expected either `=` or `,`", sep.span())),
            }
            for tok in toks.by_ref() {
                if let TokenTree::Punct(punct) = &tok {
                    if punct.as_char() == ',' {
                        break;
                    }
                }
                buf.push(tok);
            }
        }
        if let Err(err) = func(ident, &mut buf) {
            return Err(err);
        }
        buf.clear();
    }
    Ok(())
}
fn parse_field_attr<'a>(
    current: &mut Option<&'a mut FieldAttr>,
    attr_buf: &mut Allocator<'a, FieldAttr>,
    toks: TokenStream,
) -> Result<(), Error> {
    let Some(attrs) = extract_jsony_attr(toks) else {
        return Ok(());
    };
    let attr = current.get_or_insert_with(|| attr_buf.alloc_default());
    parse_attrs(
        attrs,
        &mut (|ident, buf| parse_single_field_attr(attr, ident, buf)),
    )
}
struct VariantTemp<'a> {
    name: &'a Ident,
    start: usize,
    end: usize,
    attr: &'a FieldAttr,
    kind: EnumKind,
}
pub fn scan_fields<'a>(target: &mut DeriveTargetInner<'a>, fields: &mut Vec<Field<'a>>) {
    let type_generic_names: Vec<_> = target
        .generics
        .iter()
        .filter_map(|a| {
            if !match a.kind {
                GenericKind::Type => true,
                _ => false,
            } {
                return None;
            }
            Some(a.ident.to_string())
        })
        .collect();
    let Some(min_length) = type_generic_names.iter().map(|x| x.len()).max() else {
        return;
    };
    for field in fields {
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
pub fn parse_enum<'a>(
    target: &mut DeriveTargetInner<'a>,
    fields: &'a [TokenTree],
    tt_buf: &'a mut Vec<TokenTree>,
    field_buf: &'a mut Vec<Field<'a>>,
    attr_buf: &mut Allocator<'a, FieldAttr>,
) -> Result<Vec<EnumVariant<'a>>, Error> {
    let mut temp = parse_inner_enum_variants(fields, tt_buf, attr_buf)?;
    {
        for variant in &mut temp {
            match variant.kind {
                EnumKind::Tuple => {
                    let start = field_buf.len();
                    parse_tuple_fields(
                        variant.name,
                        field_buf,
                        &tt_buf[variant.start..variant.end],
                        attr_buf,
                    )?;
                    variant.start = start;
                    variant.end = field_buf.len();
                }
                EnumKind::Struct => {
                    let start = field_buf.len();
                    parse_struct_fields(field_buf, &tt_buf[variant.start..variant.end], attr_buf)?;
                    variant.start = start;
                    variant.end = field_buf.len();
                }
                EnumKind::None => (),
            }
        }
    }
    scan_fields(target, field_buf);
    Ok(temp
        .into_iter()
        .map(|var| EnumVariant {
            name: var.name,
            fields: if match var.kind {
                EnumKind::None => true,
                _ => false,
            } {
                &[]
            } else {
                &field_buf[var.start..var.end]
            },
            kind: var.kind,
            attr: var.attr,
        })
        .collect())
}
fn parse_inner_enum_variants<'a>(
    fields: &'a [TokenTree],
    tt_buffer: &mut Vec<TokenTree>,
    attr_buffer: &mut Allocator<'a, FieldAttr>,
) -> Result<Vec<VariantTemp<'a>>, Error> {
    let mut f = fields.iter().enumerate();
    let mut enums: Vec<VariantTemp<'a>> = Vec::new();
    let mut next_attr: Option<&'a mut FieldAttr> = None;
    loop {
        let i = if let Some((i, tok)) = f.next() {
            let TokenTree::Punct(punct) = tok else {
                continue;
            };
            let ch = punct.as_char() as u8;
            if ch == b'#' {
                let Some((_, TokenTree::Group(group))) = f.next() else {
                    return Err(Error::span_msg("Expected attr after", punct.span()));
                };
                if let Err(err) = parse_field_attr(&mut next_attr, attr_buffer, group.stream()) {
                    return Err(err);
                }
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
                                        return Err(Error::msg(
                                            "Unexpected EOF while parsing type in enum expression",
                                        ));
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
            return Err(Error::msg("Baddness"));
        };
        let start = tt_buffer.len();
        let (name, kind) = match tok {
            TokenTree::Group(group) => {
                tt_buffer.extend(group.stream());
                let Some(TokenTree::Ident(ident)) = fields.get(i.saturating_sub(2)) else {
                    return Err(Error::span_msg("Expected ident", group.span()));
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
            tok => {
                return Err(Error::span_msg(
                    "Expected either an ident or group",
                    tok.span(),
                ))
            }
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
    Ok(enums)
}
fn flags_from_attr(attr: &Option<&mut FieldAttr>) -> u32 {
    let mut f = 0;
    if let Some(attr) = attr {
        if attr.flatten {
            f |= Field::WITH_FLATTEN;
        }
        if attr.default.is_some() {
            f |= Field::WITH_DEFAULT;
        }
    }
    f
}
pub fn parse_tuple_fields<'a>(
    fake_name: &'a Ident,
    output: &mut Vec<Field<'a>>,
    fields: &'a [TokenTree],
    attr_buf: &mut Allocator<'a, FieldAttr>,
) -> Result<(), Error> {
    let mut f = fields.iter().enumerate();
    let mut next_attr: Option<&mut FieldAttr> = None;
    while let Some((i, tok)) = f.next() {
        if let TokenTree::Punct(punct) = tok {
            if punct.as_char() == '#' {
                let Some((_, TokenTree::Group(group))) = f.next() else {
                    return Err(Error::span_msg("Expected attr after", punct.span()));
                };
                if let Err(err) = parse_field_attr(&mut next_attr, attr_buf, group.stream()) {
                    return Err(err);
                }
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
    Ok(())
}
struct DefaultFieldAttr(FieldAttr);
unsafe impl Sync for DefaultFieldAttr {}
static DEFAULT_ATTR: DefaultFieldAttr = const {
    DefaultFieldAttr(FieldAttr {
        rename: None,
        flatten: false,
        default: None,
    })
};
pub fn parse_struct_fields<'a>(
    output: &mut Vec<Field<'a>>,
    fields: &'a [TokenTree],
    attr_buf: &mut Allocator<'a, FieldAttr>,
) -> Result<(), Error> {
    let mut f = fields.iter().enumerate();
    let mut next_attr: Option<&'a mut FieldAttr> = None;
    while let Some((i, tok)) = f.next() {
        let TokenTree::Punct(punct) = tok else {
            continue;
        };
        let ch = punct.as_char() as u8;
        if ch == b'#' {
            let Some((_, TokenTree::Group(group))) = f.next() else {
                return Err(Error::span_msg("Expected attr after", punct.span()));
            };
            if let Err(err) = parse_field_attr(&mut next_attr, attr_buf, group.stream()) {
                return Err(err);
            }
            continue;
        }
        if ch != b':' {
            continue;
        }
        let Some(TokenTree::Ident(name)) = fields.get(i.wrapping_sub(1)) else {
            return Err(Error::span_msg(
                "Expected field name before :",
                punct.span(),
            ));
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
    Ok(())
}
