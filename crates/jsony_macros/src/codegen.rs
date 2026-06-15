use crate::ast::{
    self, DefaultKind, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field,
    FieldAttrs, Generic, GenericKind, Tag, TraitSet, Via, ENUM_CONTAINS_STRUCT_VARIANT,
    ENUM_CONTAINS_TUPLE_VARIANT, ENUM_CONTAINS_UNIT_VARIANT, ENUM_HAS_EXTERNAL_TAG, FROM_BINARY,
    FROM_JSON, TO_BINARY, TO_JSON,
};
use crate::case::RenameRule;
use crate::util::MemoryPool;
use crate::writer::RustWriter;
use crate::Error;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[allow(unused)]
enum StaticToken {
    Ident(&'static str),
    Punct(char, bool),
}
#[allow(unused)]
use StaticToken::Ident as StaticIdent;
#[allow(unused)]
use StaticToken::Punct as StaticPunct;
#[allow(unused)]
fn tt_append_blit(output: &mut RustWriter, chr: &str) {
    output
        .buf
        .extend(chr.as_bytes().iter().map(|tok| match *tok {
            1 => TokenTree::Ident(Ident::new("hello", Span::call_site())),
            v => TokenTree::Punct(Punct::new(
                ':',
                if v & 0b1 == 0 {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                },
            )),
        }));
}

struct GenericBoundFormatting {
    lifetimes: bool,
    bounds: bool,
}
fn fmt_generics(buffer: &mut RustWriter, generics: &[Generic], fmt: GenericBoundFormatting) {
    let mut first = true;
    for generic in generics {
        if !fmt.lifetimes
            && match generic.kind {
                GenericKind::Lifetime => true,
                _ => false,
            }
        {
            continue;
        }
        if first {
            first = false;
        } else {
            buffer.blit_punct(13);
        }
        match generic.kind {
            GenericKind::Lifetime => {
                buffer.blit_punct(7);
            }
            GenericKind::Type => (),
            GenericKind::Const => {
                buffer.blit_ident(180);
            }
        }
        buffer.buf.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            buffer.blit_punct(9);
            for tok in generic.bounds {
                buffer.buf.push(tok.clone());
            }
        }
    }
}
const DEAD_USE: GenericBoundFormatting = GenericBoundFormatting {
    lifetimes: false,
    bounds: false,
};
const USE: GenericBoundFormatting = GenericBoundFormatting {
    lifetimes: true,
    bounds: false,
};
const DEF: GenericBoundFormatting = GenericBoundFormatting {
    lifetimes: true,
    bounds: true,
};
fn bodyless_impl_from(
    output: &mut RustWriter,
    sub: Option<Ident>,
    trait_name: Ident,
    Ctx {
        target,
        lifetime,
        crate_path,
        generics,
        ..
    }: &Ctx,
) {
    let any_generics = !target.generics.is_empty();
    {
        output.blit(0, 3);
        output.push_ident(lifetime);
        if !generics.is_empty() {
            output.blit_punct(13);
            fmt_generics(output, generics, DEF);
        };
        output.blit_punct(1);
        output.buf.extend_from_slice(&crate_path);
        output.blit(3, 2);
        if let Some(sub) = &sub {
            output.push_ident(sub);
            output.blit(3, 2);
        };
        output.push_ident(&trait_name);
        output.blit(1, 2);
        output.push_ident(lifetime);
        output.blit(5, 2);
        output.push_ident(&target.name);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, USE);
            output.blit_punct(1);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(169);
            for ty in &target.generic_field_types {
                output.buf.extend_from_slice(ty);
                output.blit_punct(9);
                output.buf.extend_from_slice(&crate_path);
                output.blit(3, 2);
                if let Some(sub) = &sub {
                    output.push_ident(sub);
                    output.blit(3, 2);
                };
                output.push_ident(&trait_name);
                output.blit(1, 2);
                output.push_ident(lifetime);
                output.blit(7, 2);
            }
            output.buf.extend_from_slice(&target.where_clauses);
        };
    };
}
fn impl_from_binary(
    out: &mut RustWriter,
    ctx: &Ctx,
    inner: TokenStream,
    pod_forward: Option<&Field>,
) {
    {
        out.blit_punct(14);
        {
            let at = out.buf.len();
            out.blit_ident(165);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit_ident(184);
        {
            bodyless_impl_from(out, None, Ident::new("FromBinary", Span::call_site()), ctx)
        };
        {
            let at = out.buf.len();
            {
                if ctx.target.pod {
                    out.blit(9, 7);
                } else if let Some(pod_field) = pod_forward {
                    out.blit(16, 6);
                    out.buf.extend_from_slice(pod_field.ty);
                    out.blit(22, 12);
                }
            };
            out.blit(34, 2);
            {
                let at = out.buf.len();
                out.blit(36, 4);
                out.buf.extend_from_slice(&ctx.crate_path);
                out.blit(40, 8);
                out.push_ident(&ctx.lifetime);
                out.blit_punct(1);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(48, 3);
            {
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
}
fn impl_from_json_field_visitor(
    output: &mut RustWriter,
    ctx: &Ctx,
    ty: &dyn Fn(&mut RustWriter),
    inner: TokenStream,
) {
    {
        output.blit_ident(184);
        {
            bodyless_impl_from(
                output,
                Some(Ident::new("json", Span::call_site())),
                Ident::new("FromJsonFieldVisitor", Span::call_site()),
                ctx,
            )
        };
        {
            let at = output.buf.len();
            output.blit(51, 3);
            ty(output);
            output.blit(54, 4);
            {
                let at = output.buf.len();
                output.blit(58, 18);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(76, 8);
                output.push_ident(&ctx.lifetime);
                output.blit_punct(1);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(84, 6);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
}
fn impl_from_json(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) {
    {
        output.blit_punct(14);
        {
            let at = output.buf.len();
            output.blit_ident(165);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit_ident(184);
        {
            bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)
        };
        {
            let at = output.buf.len();
            output.blit(90, 3);
            {
                let at = output.buf.len();
                output.blit(93, 19);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(76, 8);
                output.push_ident(&ctx.lifetime);
                output.blit_punct(1);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(112, 27);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
}
fn impl_to_binary(
    out: &mut RustWriter,
    Ctx {
        target, crate_path, ..
    }: &Ctx,
    inner: TokenStream,
    pod_forward: Option<&Field>,
) {
    let any_generics = !target.generics.is_empty();
    {
        out.blit_punct(14);
        {
            let at = out.buf.len();
            out.blit_ident(165);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit(139, 2);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, DEF);
            out.blit_punct(1);
        };
        out.buf.extend_from_slice(&crate_path);
        out.blit(141, 4);
        out.push_ident(&target.name);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, USE);
            out.blit_punct(1);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            out.blit_ident(169);
            {
                for ty in &target.generic_field_types {
                    out.buf.extend_from_slice(ty);
                    out.blit_punct(9);
                    out.buf.extend_from_slice(&crate_path);
                    out.blit(145, 4);
                }
            };
        };
        {
            let at = out.buf.len();
            {
                if target.pod {
                    out.blit(9, 7);
                } else if let Some(pod_field) = pod_forward {
                    out.blit(16, 6);
                    out.buf.extend_from_slice(pod_field.ty);
                    out.blit(22, 12);
                }
            };
            out.blit(149, 2);
            {
                let at = out.buf.len();
                out.blit(151, 13);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
}
enum ToJsonKind<'a> {
    Static(&'static str),
    Forward(&'a Field<'a>),
}
fn impl_to_json(
    output: &mut RustWriter,
    kind: ToJsonKind,
    Ctx {
        target, crate_path, ..
    }: &Ctx,
    inner: TokenStream,
) {
    let any_generics = !target.generics.is_empty();
    {
        output.blit_punct(14);
        {
            let at = output.buf.len();
            output.blit_ident(165);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit_ident(159);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, DEF);
            output.blit_punct(1);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(164, 4);
        output.push_ident(&target.name);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, USE);
            output.blit_punct(1);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(169);
            {
                for ty in &target.generic_field_types {
                    output.buf.extend_from_slice(ty);
                    output.blit_punct(9);
                    output.buf.extend_from_slice(&crate_path);
                    output.blit(168, 4);
                }
            };
        };
        {
            let at = output.buf.len();
            output.blit(172, 3);
            {
                match kind {
                    ToJsonKind::Static(kind) => {
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(132, 5);
                        output.buf.push(Ident::new(kind, Span::call_site()).into());
                    }
                    ToJsonKind::Forward(field) => {
                        output.blit_punct(3);
                        output.buf.extend_from_slice(field.ty);
                        output.blit_ident(191);
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(175, 7);
                    }
                }
            };
            output.blit(182, 3);
            {
                let at = output.buf.len();
                output.blit(185, 11);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(196, 6);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
}
struct Ctx<'a> {
    lifetime: Ident,
    generics: &'a [Generic<'a>],
    crate_path: Vec<TokenTree>,
    target: &'a DeriveTargetInner<'a>,
    temp: Vec<Ident>,
}
impl<'a> Ctx<'a> {
    pub fn dead_target_type(&self, out: &mut RustWriter) {
        {
            out.push_ident(&self.target.name);
            if !self.target.generics.is_empty() {
                out.blit_punct(3);
                fmt_generics(out, &self.target.generics, DEAD_USE);
                out.blit_punct(1);
            };
        }
    }
    pub fn target_type(&self, out: &mut RustWriter) {
        {
            out.push_ident(&self.target.name);
            if !self.target.generics.is_empty() {
                out.blit_punct(3);
                fmt_generics(out, &self.target.generics, USE);
                out.blit_punct(1);
            };
        }
    }
    fn new(out: &mut RustWriter, target: &'a DeriveTargetInner) -> Ctx<'a> {
        let crate_path = if let Some(value) = &target.path_override {
            let content = value.to_string();
            #[allow(unused)]
            let inner = &content[1..content.len() - 1];
            {
                out.blit(23, 3);
            };
            std::mem::take(&mut out.buf)
        } else {
            {
                out.blit(23, 3);
            };
            std::mem::take(&mut out.buf)
        };
        let (lt, generics) = if let [Generic {
            kind: GenericKind::Lifetime,
            ident,
            bounds,
        }, rest @ ..] = &target.generics[..]
        {
            if !bounds.is_empty() {
                Error::msg("Bounded lifetimes currently unsupported")
            }
            ((*ident).clone(), rest)
        } else {
            (Ident::new("de", Span::call_site()), &target.generics[..])
        };
        Ctx {
            lifetime: lt,
            generics,
            crate_path,
            target,
            temp: Vec::new(),
        }
    }
}
fn var(num: usize) -> Ident {
    let mut buf = [b'z', b'0', b'0'];
    buf[2] = ((num % 10) as u8) + b'0';
    let a = (num / 10) as u8;
    if a > 0 && a < 10 {
        buf[1] = a + b'0';
    };
    let x = unsafe { std::str::from_utf8_unchecked(&buf) };
    Ident::new(x, Span::call_site())
}
fn encode_binary_field(
    output: &mut RustWriter,
    ctx: &Ctx,
    field: &Field,
    place: &dyn Fn(&mut RustWriter),
) {
    if field.flags & Field::WITH_TO_BINARY_SKIP != 0 {
        return;
    }
    {
        {
            if let Some(path) = field.with(TO_BINARY) {
                output.buf.extend_from_slice(path);
                output.blit(3, 2);
                output.buf.push(with_method(path, "encode_binary"));
            } else {
                output.blit_punct(3);
                output.buf.extend_from_slice(field.ty);
                output.blit_ident(191);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(202, 7);
            }
        };
        {
            let at = output.buf.len();
            place(output);
            output.blit(153, 2);
            output.tt_group(Delimiter::Parenthesis, at);
        };
        output.blit_punct(0);
    };
}
fn decode_binary_field(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    if field.flags & Field::WITH_FROM_BINARY_SKIP != 0 {
        field_from_default(out, field, FROM_BINARY);
        return;
    }
    let start = out.buf.len();
    {
        {
            if let Some(path) = field.with(FROM_BINARY) {
                out.buf.extend_from_slice(path);
                out.blit(3, 2);
                out.buf.push(with_method(path, "decode_binary"));
            } else {
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit_ident(191);
                    ctx.FromBinary(out);
                    out.blit(209, 4);
                }
            }
        };
        {
            let at = out.buf.len();
            out.blit_ident(171);
            out.tt_group(Delimiter::Parenthesis, at);
        };
    };
    if let Some(version) = field.attr.version() {
        let group = out.split_off_stream(start);
        {
            out.blit(213, 4);
            {
                out.buf.push(TokenTree::Literal(version.clone()))
            };
            {
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, group)))
            };
            out.blit_ident(148);
            {
                let at = out.buf.len();
                {
                    if let Some(DefaultKind::Custom(field)) = field.default(FROM_BINARY) {
                        out.buf.extend_from_slice(&field);
                    } else {
                        out.blit(217, 5);
                    }
                };
                out.tt_group(Delimiter::Brace, at);
            };
        };
    }
    if let Some(with) = field.validate(FROM_BINARY) {
        let group = out.split_off_stream(start);
        {
            {
                let at = out.buf.len();
                out.blit(222, 3);
                {
                    out.buf
                        .push(TokenTree::Group(Group::new(Delimiter::None, group)))
                };
                out.blit(225, 5);
                {
                    let at = out.buf.len();
                    out.blit_punct(4);
                    out.buf.extend_from_slice(field.ty);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(230, 17);
                out.buf.extend_from_slice(with);
                out.blit(247, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(5);
                {
                    let at = out.buf.len();
                    out.blit_ident(150);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit(251, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit(253, 5);
                    {
                        let at = out.buf.len();
                        out.blit(258, 4);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(123);
                out.tt_group(Delimiter::Brace, at);
            };
        }
    }
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        out.buf.extend_from_slice(&self.crate_path);
        out.blit(262, 5);
        out.push_ident(&self.lifetime);
        out.blit_punct(1);
    }
}
/// A method-name token (e.g. `decode_json`) carrying the span of the `with`
/// path's final segment. When the user's `with` module is missing that method,
/// rustc blames the path in the attribute rather than the `#[derive(..)]` entry
/// point, which is where a generated ident's default span would otherwise land.
fn with_method(path: &[TokenTree], method: &'static str) -> TokenTree {
    let span = path
        .last()
        .map(TokenTree::span)
        .unwrap_or_else(Span::call_site);
    TokenTree::Ident(Ident::new(method, span))
}
fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    if let Some(with) = field.with(FROM_JSON) {
        {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(267, 22);
            out.push_ident(&ctx.lifetime);
            out.blit(7, 2);
            out.buf.extend_from_slice(field.ty);
            out.blit(289, 3);
            {
                let at = out.buf.len();
                out.blit_punct(4);
                out.buf.extend_from_slice(with);
                out.blit(3, 2);
                out.buf.push(with_method(with, "decode_json"));
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
    } else if let Some(with) = field.validate(FROM_JSON) {
        {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(292, 9);
            out.buf.extend_from_slice(field.ty);
            out.blit(289, 3);
            {
                let at = out.buf.len();
                out.blit_punct(4);
                {
                    let at = out.buf.len();
                    out.buf.extend_from_slice(with);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
    } else {
        out.buf.extend_from_slice(&ctx.crate_path);
        out.blit(301, 10);
        out.push_ident(&ctx.lifetime);
        out.blit_punct(13);
        out.buf.extend_from_slice(field.ty);
        out.blit(311, 2);
    }
}
fn field_name_literal(_ctx: &Ctx, field: &Field, rename_rule: RenameRule) -> Literal {
    if let Some(name) = field.attr.rename(FROM_JSON) {
        return name.clone();
    }
    if rename_rule != RenameRule::None {
        Literal::string(&rename_rule.apply_to_field(&field.name.to_string()))
    } else {
        Literal::string(&field.name.to_string())
    }
}
fn variant_name_json(ctx: &Ctx, field: &EnumVariant, output: &mut String) {
    output.push('"');
    if let Some(name) = field.rename(TO_JSON) {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                crate::template::raw_escape(&value, output);
            }
            _ => Error::span_msg("Invalid rename value expected a string", name.span()),
        }
    } else if ctx.target.rename_all != RenameRule::None {
        output.push_str(
            &ctx.target
                .rename_all
                .apply_to_variant(&field.name.to_string()),
        );
    } else {
        output.push_str(&field.name.to_string());
    }
    output.push('"');
}
fn field_name_json(_ctx: &Ctx, field: &Field, output: &mut String, rename_rule: RenameRule) {
    output.push('"');
    if let Some(name) = &field.attr.rename(TO_JSON) {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                crate::template::raw_escape(&value, output);
            }
            _ => Error::span_msg("Invalid rename value expected a string", name.span()),
        }
    } else if rename_rule != RenameRule::None {
        output.push_str(&rename_rule.apply_to_field(&field.name.to_string()));
    } else {
        output.push_str(&field.name.to_string());
    }
    output.push('"');
}
fn struct_schema(
    out: &mut RustWriter,
    ctx: &Ctx,
    fields: &[&Field],
    temp_tuple: Option<&Ident>,
    field_rename: RenameRule,
) {
    let ag_gen = if ctx.target.generics.iter().any(|g| !match g.kind {
        GenericKind::Lifetime => true,
        _ => false,
    }) {
        let x = out.buf.len();
        out.blit_punct(3);
        fmt_generics(
            out,
            &ctx.target.generics,
            GenericBoundFormatting {
                lifetimes: false,
                bounds: false,
            },
        );
        out.blit_punct(1);
        TokenTree::Group(Group::new(Delimiter::None, out.buf.drain(x..).collect()))
    } else {
        TokenTree::Group(Group::new(Delimiter::None, TokenStream::new()))
    };
    let ts = {
        let len = out.buf.len();
        for (i, field) in fields.iter().enumerate() {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(313, 6);
            {
                let at = out.buf.len();
                out.blit(319, 2);
                out.buf
                    .push(field_name_literal(ctx, field, field_rename).into());
                out.blit(321, 13);
                {
                    let at = out.buf.len();
                    {
                        if let Some(ty) = temp_tuple {
                            out.push_ident(&ty);
                            out.blit_punct(13);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                        } else {
                            out.push_ident(&ctx.target.name);
                            out.buf.push(ag_gen.clone());
                            out.blit_punct(13);
                            out.push_ident(field.name);
                        }
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(334, 3);
                schema_field_decode(out, ctx, field);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(13);
        }
        out.split_off_stream(len)
    };
    let schema_fields = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.buf.len();
        {
            for field in fields {
                out.blit(337, 12);
                out.buf.extend_from_slice(field.ty);
                out.blit(7, 2);
            }
        };
        out.split_off_stream(len)
    };
    let schema_drops = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.buf.len();
        {
            for field in fields {
                let Some(default) = &field.default(FROM_JSON) else {
                    break;
                };
                match default {
                    ast::DefaultKind::Default => {
                        out.blit(349, 12);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(7, 2);
                    }
                    ast::DefaultKind::Custom(expr) => {
                        out.blit(361, 16);
                        {
                            let at = out.buf.len();
                            out.blit(377, 3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit_punct(5);
                            out.buf.extend_from_slice(expr);
                            out.blit(54, 2);
                            {
                                let at = out.buf.len();
                                out.blit(380, 6);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(194);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(386, 9);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit_punct(13);
                    }
                }
            }
        };
        out.split_off_stream(len)
    };
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    {
        out.blit(395, 9);
        {
            let at = out.buf.len();
            out.blit(404, 3);
            out.buf.push(schema_fields);
            out.blit(407, 4);
            out.buf.push(schema_drops);
            out.blit(411, 4);
            out.buf.push(schema_defaults);
            out.blit_punct(13);
            out.tt_group(Delimiter::Brace, at);
        };
    }
}
fn schema_ordered_fields<'a>(fields: &'a [Field<'a>]) -> Vec<&'a Field<'a>> {
    let mut buf = Vec::with_capacity(fields.len());
    for field in fields {
        if field.flags
            & (Field::WITH_FROM_JSON_DEFAULT
                | Field::WITH_FROM_JSON_FLATTEN
                | Field::WITH_FROM_JSON_SKIP)
            == Field::WITH_FROM_JSON_DEFAULT
        {
            buf.push(field);
        }
    }
    for field in fields {
        if field.flags
            & (Field::WITH_FROM_JSON_DEFAULT
                | Field::WITH_FROM_JSON_FLATTEN
                | Field::WITH_FROM_JSON_SKIP)
            == 0
        {
            buf.push(field);
        }
    }
    buf
}
fn required_bitset(ordered: &[&Field]) -> u64 {
    let mut defaults = 0;
    for field in ordered {
        if field.flags & (Field::WITH_FROM_JSON_DEFAULT | Field::WITH_FROM_JSON_SKIP) != 0 {
            defaults += 1;
            continue;
        }
        break;
    }
    ((1 << ordered.len()) - 1) ^ ((1 << defaults) - 1)
}
fn tuple_struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
    let head = out.buf.len();
    match fields {
        [] => Error::msg("FromJson not implemented for Tuples without fields yet."),
        [field] => {
            let place = |out: &mut RustWriter| {
                out.blit_ident(193);
                if !match ctx.target.repr {
                    ast::Repr::Transparent | ast::Repr::C => true,
                    _ => false,
                } {
                    out.blit(415, 2);
                    {
                        let at = out.buf.len();
                        out.blit(324, 10);
                        {
                            let at = out.buf.len();
                            ctx.dead_target_type(out);
                            out.blit_punct(13);
                            out.buf.push(Literal::usize_unsuffixed(0).into());
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
            };
            if let Some(with) = field.with(FROM_JSON) {
                {
                    out.buf.extend_from_slice(&ctx.crate_path);
                    out.blit(267, 22);
                    out.push_ident(&ctx.lifetime);
                    out.blit(7, 2);
                    out.buf.extend_from_slice(field.ty);
                    out.blit(289, 3);
                    {
                        let at = out.buf.len();
                        out.blit_punct(4);
                        out.buf.extend_from_slice(with);
                        out.blit(3, 2);
                        out.buf.push(with_method(with, "decode_json"));
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    {
                        let at = out.buf.len();
                        place(out);
                        out.blit(72, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
            } else {
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit(417, 9);
                    out.push_ident(&ctx.lifetime);
                    out.blit(426, 5);
                    {
                        let at = out.buf.len();
                        place(out);
                        out.blit(72, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
            }
        }
        fields => {
            {
                out.blit(431, 9);
                {
                    let at = out.buf.len();
                    out.blit(440, 5);
                    {
                        let at = out.buf.len();
                        for (i, field) in fields.iter().enumerate() {
                            {
                                let at = out.buf.len();
                                out.blit(324, 10);
                                {
                                    let at = out.buf.len();
                                    ctx.dead_target_type(out);
                                    out.blit_punct(13);
                                    out.buf.push(Literal::usize_unsuffixed(i).into());
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                                {
                                    if let Some(with) = field.with(FROM_JSON) {
                                        {
                                            out.buf.extend_from_slice(&ctx.crate_path);
                                            out.blit(267, 22);
                                            out.push_ident(&ctx.lifetime);
                                            out.blit(7, 2);
                                            out.buf.extend_from_slice(field.ty);
                                            out.blit(289, 3);
                                            {
                                                let at = out.buf.len();
                                                out.blit_punct(4);
                                                out.buf.extend_from_slice(with);
                                                out.blit(3, 2);
                                                out.buf.push(with_method(with, "decode_json"));
                                                out.tt_group(Delimiter::Parenthesis, at);
                                            };
                                        }
                                    } else {
                                        out.blit_punct(3);
                                        out.buf.extend_from_slice(field.ty);
                                        out.blit(417, 9);
                                        out.push_ident(&ctx.lifetime);
                                        out.blit(426, 5);
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(13);
                        }
                        out.tt_group(Delimiter::Bracket, at);
                    };
                    out.blit(125, 2);
                    {
                        let at = out.buf.len();
                        for field in fields.iter() {
                            out.blit(445, 12);
                            {
                                let at = out.buf.len();
                                out.blit(457, 6);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(463, 5);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(13);
                        }
                        out.tt_group(Delimiter::Bracket, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
        }
    }
    let stream = out.split_off_stream(head);
    impl_from_json(out, ctx, stream);
}
fn tuple_struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
    let head = out.buf.len();
    let kind = match fields {
        [] => {
            {
                out.blit(468, 3);
                {
                    let at = out.buf.len();
                    {
                        out.buf.push(TokenTree::Literal(Literal::string("null")));
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            ToJsonKind::Static("AnyValue")
        }
        [field] => {
            if let Some(with) = field.with(TO_JSON) {
                {
                    out.buf.extend_from_slice(with);
                    out.blit(3, 2);
                    out.buf.push(with_method(with, "encode_json"));
                    {
                        let at = out.buf.len();
                        out.blit(471, 3);
                        out.buf
                            .push(TokenTree::Literal(Literal::usize_unsuffixed(0)));
                        out.blit(187, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(474, 10);
                };
                ToJsonKind::Static("AnyValue")
            } else {
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit(484, 11);
                    {
                        let at = out.buf.len();
                        out.blit(471, 3);
                        out.buf
                            .push(TokenTree::Literal(Literal::usize_unsuffixed(0)));
                        out.blit(187, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
                ToJsonKind::Forward(field)
            }
        }
        _ => {
            let mut first = true;
            {
                out.blit(495, 5);
                for (i, field) in fields.iter().enumerate() {
                    {
                        if first {
                            first = false;
                        } else {
                            out.blit(500, 5);
                        }
                    };
                    {
                        if let Some(with) = field.with(TO_JSON) {
                            out.buf.extend_from_slice(with);
                            out.blit(3, 2);
                            out.buf.push(with_method(with, "encode_json"));
                        } else {
                            out.blit_punct(3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit(484, 11);
                        }
                    };
                    {
                        let at = out.buf.len();
                        out.blit(471, 3);
                        out.buf
                            .push(TokenTree::Literal(Literal::usize_unsuffixed(i)));
                        out.blit(187, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
                out.blit(505, 4);
            };
            ToJsonKind::Static("AlwaysArray")
        }
    };
    let stream = out.split_off_stream(head);
    impl_to_json(out, kind, ctx, stream)
}
use TokenTree as Tok;
/// If the attr value begins with `| ident |` then it `| ident: & ty |` will be
/// written instead, otherwise it pass as is.
fn with_injected_closure_arg_type(out: &mut RustWriter, attr_value: &[Tok], ty: &[Tok]) -> bool {
    if let [Tok::Punct(bar1), Tok::Ident(binding), Tok::Punct(bar2), rest @ ..] = attr_value {
        if bar1.as_char() == '|' && bar2.as_char() == '|' {
            {
                out.blit_punct(11);
                out.push_ident(binding);
                out.blit(37, 2);
                out.buf.extend_from_slice(ty);
                out.blit_punct(11);
                out.buf.extend_from_slice(rest);
            };
            return false;
        }
    }
    if let [Tok::Punct(bar1), Tok::Ident(binding), Tok::Punct(comma), Tok::Ident(writer), Tok::Punct(bar2), rest @ ..] =
        attr_value
    {
        if bar1.as_char() == '|' && bar2.as_char() == '|' && comma.as_char() == ',' {
            {
                out.blit_punct(11);
                out.push_ident(binding);
                out.blit(37, 2);
                out.buf.extend_from_slice(ty);
                out.blit_punct(13);
                out.push_ident(writer);
                out.blit(509, 9);
                out.buf.extend_from_slice(rest);
            };
            return true;
        }
    }
    {
        out.buf.extend_from_slice(attr_value);
    };
    false
}
fn inner_struct_to_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    fields: &[Field],
    text: &mut String,
    on_self: bool,
    field_rename: RenameRule,
) {
    let mut first = true;
    {
        {
            for (i, field) in fields.iter().enumerate() {
                let if_skip_body = if let Some(skip_fn) = field.skip(TO_JSON) {
                    if skip_fn.is_empty() {
                        continue;
                    }
                    if !first {
                        text.push(',');
                    }
                    if !text.is_empty() {
                        if text == "," {
                            out.blit(500, 5);
                        } else {
                            {
                                out.blit(468, 3);
                                {
                                    let at = out.buf.len();
                                    {
                                        out.buf.push(TokenTree::Literal(Literal::string(&text)));
                                    };
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                            };
                        }
                        text.clear();
                    }
                    let with_writer;
                    {
                        out.blit(518, 2);
                        {
                            let at = out.buf.len();
                            {
                                with_writer =
                                    with_injected_closure_arg_type(out, skip_fn, &field.ty)
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        {
                            let at = out.buf.len();
                            {
                                if on_self {
                                    out.blit(471, 3);
                                    out.push_ident(field.name);
                                } else {
                                    out.push_ident(&ctx.temp[i]);
                                }
                            };
                            {
                                if with_writer {
                                    out.blit(187, 2);
                                }
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    };
                    Some(out.buf.len())
                } else {
                    if !first {
                        text.push(',');
                    }
                    None
                };
                let flattened = field.flatten(TO_JSON);
                first = flattened || if_skip_body.is_some();
                if !flattened {
                    field_name_json(ctx, field, text, field_rename);
                    text.push(':');
                }
                if !text.is_empty() {
                    if text == "," {
                        out.blit(500, 5);
                    } else {
                        {
                            out.blit(468, 3);
                            {
                                let at = out.buf.len();
                                {
                                    out.buf.push(TokenTree::Literal(Literal::string(&text)));
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                        };
                    }
                    text.clear();
                }
                if let Via::Iterator = field.via(TO_JSON) {
                    if flattened {
                        {
                            out.blit_ident(158);
                            {
                                let at = out.buf.len();
                                out.blit(520, 3);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_ident(42);
                            {
                                let at = out.buf.len();
                                {
                                    if on_self {
                                        out.blit(471, 3);
                                        out.push_ident(field.name);
                                    } else {
                                        out.push_ident(&ctx.temp[i]);
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(523, 3);
                            {
                                let at = out.buf.len();
                                out.blit(526, 16);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(202);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(542, 9);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(202);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(499, 6);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        };
                    } else {
                        Error::span_msg(
                            "ToJson Via = Iterator, only supported with flatten currently.",
                            field.name.span(),
                        );
                    }
                } else {
                    if flattened {
                        out.blit(551, 18);
                    }
                    {
                        {
                            if let Some(with) = field.with(TO_JSON) {
                                out.buf.extend_from_slice(with);
                                out.blit(3, 2);
                                out.buf.push(with_method(with, "encode_json"));
                            } else {
                                out.blit_punct(3);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(484, 11);
                            }
                        };
                        {
                            let at = out.buf.len();
                            {
                                if on_self {
                                    out.blit(471, 3);
                                    out.push_ident(field.name);
                                } else {
                                    out.push_ident(&ctx.temp[i]);
                                }
                            };
                            out.blit(187, 2);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                    };
                    if flattened {
                        out.blit(569, 5);
                    }
                }
                if let Some(from) = if_skip_body {
                    if !flattened {
                        out.blit(500, 5);
                    }
                    let inner =
                        TokenTree::Group(Group::new(Delimiter::Brace, out.split_off_stream(from)));
                    out.buf.push(inner);
                }
            }
        };
    };
}
fn field_from_default(out: &mut RustWriter, field: &Field, set: TraitSet) {
    {
        {
            if let Some(explicit_default) = field.default(set) {
                {
                    {
                        let at = out.buf.len();
                        out.blit(574, 7);
                        out.buf.extend_from_slice(field.ty);
                        {
                            let at = out.buf.len();
                            {
                                match explicit_default {
                                    ast::DefaultKind::Default => {
                                        out.blit(217, 5);
                                    }
                                    ast::DefaultKind::Custom(expr) => {
                                        out.buf.extend_from_slice(expr);
                                    }
                                }
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(581, 3);
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            } else {
                out.blit(217, 5);
            }
        };
    };
}
fn struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
    let mut text = String::new();
    let body = {
        let len = out.buf.len();
        out.blit(584, 5);
        inner_struct_to_json(out, ctx, fields, &mut text, true, ctx.target.rename_all);
        out.blit(589, 4);
        out.split_off_stream(len)
    };
    impl_to_json(out, ToJsonKind::Static("AlwaysObject"), ctx, body)
}
fn struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
    let mut flattening: Option<&Field> = None;
    let mut has_skips = false;
    for field in fields {
        if field.flags & Field::WITH_FROM_JSON_SKIP != 0 {
            has_skips = true;
        }
        if field.flatten(FROM_JSON) {
            if flattening.is_some() {
                Error::span_msg(
                    "Only one flatten field is currently supported",
                    field.name.span(),
                );
            }
            flattening = Some(field);
        }
    }
    let ordered_fields = schema_ordered_fields(fields);
    {
        if ctx.target.flattenable {
            out.blit(593, 6);
            out.push_ident(&ctx.lifetime);
            if !ctx.generics.is_empty() {
                out.blit_punct(13);
                fmt_generics(out, ctx.generics, DEF);
            };
            out.blit(599, 15);
            out.push_ident(&ctx.lifetime);
            out.blit_punct(1);
            if !ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty() {
                out.blit_ident(169);
                for ty in &ctx.target.generic_field_types {
                    out.buf.extend_from_slice(ty);
                    out.blit(614, 9);
                    out.push_ident(&ctx.lifetime);
                    out.blit_punct(1);
                }
            };
            {
                let at = out.buf.len();
                out.blit(623, 13);
                out.push_ident(&ctx.lifetime);
                out.blit_punct(1);
                {
                    let at = out.buf.len();
                    out.blit(636, 4);
                    {
                        let at = out.buf.len();
                        struct_schema(out, ctx, &ordered_fields, None, ctx.target.rename_all);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(640, 12);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        };
        {
            let start = out.buf.len();
            if let Some(flatten_field) = flattening {
                {
                    out.blit(652, 5);
                    out.buf.extend_from_slice(flatten_field.ty);
                    out.blit(657, 14);
                    {
                        let at = out.buf.len();
                        out.blit(671, 3);
                        {
                            let at = out.buf.len();
                            out.blit(326, 8);
                            {
                                let at = out.buf.len();
                                ctx.dead_target_type(out);
                                out.blit_punct(13);
                                out.push_ident(flatten_field.name);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(441, 3);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
            }
            if has_skips {
                out.blit(674, 3);
            }
            if ctx.target.flattenable {
                {
                    out.blit(677, 5);
                    out.push_ident(&ctx.lifetime);
                    if !ctx.generics.is_empty() {
                        out.blit_punct(13);
                        fmt_generics(out, ctx.generics, USE);
                    };
                    out.blit(311, 2);
                }
            } else {
                {
                    out.blit(623, 13);
                    out.push_ident(&ctx.lifetime);
                    out.blit_punct(1);
                    {
                        let at = out.buf.len();
                        out.blit(636, 4);
                        {
                            let at = out.buf.len();
                            struct_schema(out, ctx, &ordered_fields, None, ctx.target.rename_all);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(640, 12);
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            }
            let mut any_field_flag = 0;
            for field in fields {
                any_field_flag |= field.flags;
            }
            let has_alias = any_field_flag & Field::WITH_FROM_JSON_ALIAS != 0;
            {
                out.blit_punct(2);
                {
                    if has_alias {
                        out.blit_ident(88);
                    } else {
                        out.blit_ident(119);
                    }
                };
                {
                    let at = out.buf.len();
                    out.blit(440, 4);
                    {
                        if flattening.is_some() {
                            {
                                out.blit_ident(163);
                                {
                                    let at = out.buf.len();
                                    out.blit(682, 3);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                            }
                        } else {
                            out.blit_ident(141);
                        }
                    };
                    if has_alias {
                        out.blit(125, 2);
                        {
                            let at = out.buf.len();
                            {
                                for (i, field) in ordered_fields.iter().enumerate() {
                                    if let Some(alias) = field.attr.alias(FROM_JSON) {
                                        {
                                            {
                                                let at = out.buf.len();
                                                out.buf.push(TokenTree::Literal(
                                                    Literal::usize_unsuffixed(i),
                                                ));
                                                out.blit_punct(13);
                                                out.buf.push(TokenTree::Literal(alias.clone()));
                                                out.tt_group(Delimiter::Parenthesis, at);
                                            };
                                            out.blit_punct(13);
                                        }
                                    }
                                }
                            };
                            out.tt_group(Delimiter::Bracket, at);
                        };
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            if has_skips {
                {
                    out.blit(247, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(199);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(685, 2);
                    {
                        let at = out.buf.len();
                        out.blit(687, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(199);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    {
                        for field in fields {
                            if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                                continue;
                            }
                            {
                                out.blit(671, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(324, 10);
                                    {
                                        let at = out.buf.len();
                                        ctx.dead_target_type(out);
                                        out.blit_punct(13);
                                        out.push_ident(field.name);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(458, 5);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(689, 4);
                                {
                                    let at = out.buf.len();
                                    field_from_default(out, field, FROM_JSON);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                            }
                        }
                    };
                    out.blit_ident(197);
                    {
                        let at = out.buf.len();
                        out.tt_group_empty(Delimiter::Parenthesis);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
            }
            let ts = out.split_off_stream(start);
            impl_from_json(out, ctx, ts);
            if ctx.target.flattenable {
                if has_skips {
                    Error::msg("Flattenable does not yet support skipped fields")
                }
                let body = {
                    let len = out.buf.len();
                    out.blit(693, 7);
                    {
                        let at = out.buf.len();
                        out.blit(700, 11);
                        out.push_ident(&ctx.lifetime);
                        if !ctx.generics.is_empty() {
                            out.blit_punct(13);
                            fmt_generics(out, ctx.generics, USE);
                        };
                        out.blit(711, 6);
                        {
                            let at = out.buf.len();
                            if has_alias {
                                {
                                    for (i, field) in ordered_fields.iter().enumerate() {
                                        if let Some(alias) = field.attr.alias(FROM_JSON) {
                                            {
                                                {
                                                    let at = out.buf.len();
                                                    out.buf.push(TokenTree::Literal(
                                                        Literal::usize_unsuffixed(i),
                                                    ));
                                                    out.blit_punct(13);
                                                    out.buf.push(TokenTree::Literal(alias.clone()));
                                                    out.tt_group(Delimiter::Parenthesis, at);
                                                };
                                                out.blit_punct(13);
                                            }
                                        }
                                    }
                                };
                            };
                            out.tt_group(Delimiter::Bracket, at);
                        };
                        out.blit(717, 3);
                        out.buf.push(TokenTree::Literal(Literal::u64_unsuffixed(0)));
                        out.blit(720, 3);
                        out.buf.push(TokenTree::Literal(Literal::u64_unsuffixed(
                            required_bitset(&ordered_fields),
                        )));
                        out.blit_punct(13);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.split_off_stream(len)
                };
                impl_from_json_field_visitor(
                    out,
                    ctx,
                    &|out| {
                        out.blit(723, 9);
                        out.push_ident(&ctx.lifetime);
                        out.blit_punct(1);
                    },
                    body,
                )
            }
        };
    };
}
fn variant_key_literal(ctx: &Ctx, variant: &EnumVariant) -> Literal {
    if let Some(name) = variant.attr.rename(FROM_JSON) {
        return name.clone();
    }
    if ctx.target.rename_all != RenameRule::None {
        let res = &&ctx
            .target
            .rename_all
            .apply_to_variant(&variant.name.to_string());
        Literal::string(res)
    } else {
        Literal::string(&variant.name.to_string())
    }
}
/// Emit the match-arm pattern (without the `=>`) for a variant: `Self::Variant`,
/// `Self::Variant(t0, t1, ..)`, or `Self::Variant { f0: t0, .. }`. Shared by the
/// to_json and to_binary emitters. `rename_to_temp` binds struct fields to the
/// scratch temp idents (to_json) instead of by name (to_binary).
fn emit_variant_pattern(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    rename_to_temp: bool,
) {
    {
        out.push_ident(&ctx.target.name);
        out.blit(3, 2);
        out.push_ident(variant.name);
    }
    match variant.kind {
        EnumKind::Tuple => {
            {
                let at = out.buf.len();
                {
                    for (i, _) in variant.fields.iter().enumerate() {
                        out.push_ident(&ctx.temp[i]);
                        out.blit_punct(13);
                    }
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
        EnumKind::Struct => {
            {
                let at = out.buf.len();
                {
                    for (i, field) in variant.fields.iter().enumerate() {
                        if rename_to_temp {
                            out.push_ident(field.name);
                            out.blit_punct(9);
                            out.push_ident(&ctx.temp[i]);
                            out.blit_punct(13);
                        } else {
                            out.push_ident(field.name);
                            out.blit_punct(13);
                        }
                    }
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
        EnumKind::None => {}
    }
}
fn enum_to_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let mut text = String::with_capacity(64);
    let start = out.buf.len();
    let all_objects =
        ctx.target.enum_flags & (ENUM_CONTAINS_UNIT_VARIANT | ENUM_CONTAINS_TUPLE_VARIANT) == 0;
    if let Tag::Inline(tag_name) = &ctx.target.tag {
        {
            out.blit(584, 5);
        };
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        {
            out.blit(468, 3);
            {
                let at = out.buf.len();
                out.buf.push(Literal::string(&text).into());
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
        };
    } else if all_objects
        && match ctx.target.tag {
            Tag::Default => true,
            _ => false,
        }
    {
        out.blit(584, 5);
    }
    {
        out.blit(732, 2);
        {
            let at = out.buf.len();
            {
                for variant in variants {
                    emit_variant_pattern(out, ctx, variant, true);
                    {
                        out.blit(734, 2);
                        {
                            text.clear();
                            enum_variant_to_json(out, ctx, variant, &mut text, all_objects);
                        };
                    }
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
    if let Tag::Inline(..) = &ctx.target.tag {
        out.blit(736, 5);
    } else if all_objects
        && match ctx.target.tag {
            Tag::Default => true,
            _ => false,
        }
    {
        out.blit(736, 5);
    }
    {
        out.blit(741, 5);
    };
    let stream = out.buf.drain(start..).collect();
    let kind = if all_objects {
        "AlwaysObject"
    } else if ctx.target.enum_flags
        & (ENUM_CONTAINS_TUPLE_VARIANT
            | ENUM_CONTAINS_STRUCT_VARIANT
            | ENUM_CONTAINS_UNIT_VARIANT
            | ENUM_HAS_EXTERNAL_TAG)
        == (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG)
    {
        "AlwaysString"
    } else {
        "AnyValue"
    };
    impl_to_json(out, ToJsonKind::Static(kind), ctx, stream)
}
fn variant_field_rename_rule(ctx: &Ctx, variant: &EnumVariant) -> RenameRule {
    if variant.rename_all != RenameRule::None {
        variant.rename_all
    } else if ctx.target.rename_all_fields != RenameRule::None {
        ctx.target.rename_all_fields
    } else {
        ctx.target.rename_all
    }
}
fn enum_variant_to_json_struct(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
) {
    let field_rename = variant_field_rename_rule(ctx, variant);
    {
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    out.blit(584, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => text.push('{'),
                Tag::Default => text.push('{'),
                _ => (),
            }
        };
        inner_struct_to_json(out, ctx, &variant.fields, text, false, field_rename);
        {
            if !text.is_empty() {
                {
                    out.blit(468, 3);
                    {
                        let at = out.buf.len();
                        out.buf.push(Literal::string(&text).into());
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                text.clear();
            }
        };
        out.blit_punct(0);
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    out.blit(736, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => {
                    out.blit(736, 5);
                }
                Tag::Default => {
                    out.blit(736, 5);
                }
                _ => (),
            }
        };
    };
}
fn enum_variant_to_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
    all_objects: bool,
) {
    let start = out.buf.len();
    match &ctx.target.tag {
        Tag::Inline(..) => {
            if let EnumKind::None = variant.kind {
            } else {
                variant_name_json(ctx, variant, text);
                if let Some(content) = &ctx.target.content {
                    text.push_str(",\"");
                    crate::template::raw_escape(&content, text);
                    text.push_str("\":");
                } else {
                    text.push_str(",");
                }
            }
        }
        Tag::Untagged => (),
        Tag::Default => {
            if let EnumKind::None = variant.kind {
            } else {
                if !all_objects {
                    out.blit(584, 5);
                }
                variant_name_json(ctx, variant, text);
                text.push_str(":");
            }
        }
    }
    match variant.kind {
        EnumKind::Tuple => {
            let [field] = variant.fields else {
                Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                )
            };
            if !text.is_empty() {
                {
                    out.blit(468, 3);
                    {
                        let at = out.buf.len();
                        out.buf.push(Literal::string(&text).into());
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                text.clear();
            }
            {
                {
                    if let Some(with) = field.with(TO_JSON) {
                        out.buf.extend_from_slice(with);
                        out.blit(3, 2);
                        out.buf.push(with_method(with, "encode_json"));
                    } else {
                        out.blit_punct(3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(484, 11);
                    }
                };
                {
                    let at = out.buf.len();
                    out.push_ident(&ctx.temp[0]);
                    out.blit(187, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
            }
        }
        EnumKind::Struct => {
            enum_variant_to_json_struct(out, ctx, variant, text);
        }
        EnumKind::None => {
            variant_name_json(ctx, variant, text);
            {
                out.blit(468, 3);
                {
                    let at = out.buf.len();
                    out.buf.push(Literal::string(&text).into());
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
            };
            text.clear();
        }
    };
    if !all_objects {
        match &ctx.target.tag {
            Tag::Default => {
                if let EnumKind::None = variant.kind {
                } else {
                    out.blit(736, 5);
                }
            }
            _ => (),
        }
    }
    let ts = out.buf.drain(start..).collect();
    out.buf
        .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
}
fn enum_variant_from_json_struct(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) {
    let field_rename = variant_field_rename_rule(ctx, variant);
    let ordered_fields = schema_ordered_fields(variant.fields);
    let mut flattening: Option<&Field> = None;
    let mut any_field_flag = 0;
    for field in variant.fields {
        any_field_flag |= field.flags;
        if field.flatten(FROM_JSON) {
            if flattening.is_some() {
                Error::span_msg(
                    "Only one flatten field is currently supported",
                    field.name.span(),
                )
            }
            flattening = Some(field);
        }
    }
    let is_inline_tag = match ctx.target.tag {
        Tag::Inline(_) => true,
        _ => false,
    };
    let has_field_alias = any_field_flag & Field::WITH_FROM_JSON_ALIAS != 0;
    let use_alias = is_inline_tag || has_field_alias;
    {
        out.blit(746, 2);
        if ctx.target.has_lifetime() {
            out.blit(1, 2);
            out.push_ident(&ctx.lifetime);
            out.blit_punct(1);
        };
        out.blit_punct(5);
        {
            let at = out.buf.len();
            for field in &ordered_fields {
                out.buf.extend_from_slice(field.ty);
                out.blit_punct(13);
            }
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit(748, 13);
        {
            let at = out.buf.len();
            out.blit(761, 3);
            {
                let at = out.buf.len();
                out.blit_punct(4);
                {
                    struct_schema(
                        out,
                        ctx,
                        &ordered_fields,
                        Some(&Ident::new("__TEMP", Span::call_site())),
                        field_rename,
                    )
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(764, 13);
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit(777, 24);
        {
            if let Some(flatten_field) = flattening {
                {
                    out.blit(801, 16);
                    out.buf.extend_from_slice(flatten_field.ty);
                    out.blit(817, 11);
                    out.buf.extend_from_slice(flatten_field.ty);
                    out.blit(657, 14);
                    {
                        let at = out.buf.len();
                        out.blit(828, 12);
                        {
                            let at = out.buf.len();
                            out.blit(840, 7);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(441, 3);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
            }
        };
        out.blit(248, 3);
        {
            let at = out.buf.len();
            out.blit_ident(146);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit(847, 3);
        {
            if use_alias {
                out.blit_ident(88);
            } else {
                out.blit_ident(119);
            }
        };
        {
            let at = out.buf.len();
            out.blit(828, 12);
            {
                let at = out.buf.len();
                out.blit(850, 7);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(441, 3);
            {
                if flattening.is_some() {
                    {
                        out.blit_ident(163);
                        {
                            let at = out.buf.len();
                            out.blit(857, 3);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                } else {
                    out.blit_ident(141);
                }
            };
            out.blit_punct(13);
            if use_alias {
                out.blit_punct(4);
                {
                    let at = out.buf.len();
                    {
                        if let Tag::Inline(tag) = &ctx.target.tag {
                            {
                                {
                                    let at = out.buf.len();
                                    out.blit(860, 10);
                                    out.buf.push(Literal::string(tag).into());
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                            }
                        }
                    };
                    {
                        for (i, field) in ordered_fields.iter().enumerate() {
                            if let Some(alias) = field.attr.alias(FROM_JSON) {
                                {
                                    {
                                        let at = out.buf.len();
                                        out.buf
                                            .push(TokenTree::Literal(Literal::usize_unsuffixed(i)));
                                        out.blit_punct(13);
                                        out.buf.push(TokenTree::Literal(alias.clone()));
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.blit_punct(13);
                                }
                            }
                        }
                    };
                    out.tt_group(Delimiter::Bracket, at);
                };
            };
            out.tt_group(Delimiter::Parenthesis, at);
        };
        {
            let at = out.buf.len();
            if !untagged {
                out.blit(687, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(146);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_ident(148);
        {
            let at = out.buf.len();
            out.blit(870, 14);
            ctx.target_type(out);
            out.blit(689, 4);
            {
                let at = out.buf.len();
                out.push_ident(&ctx.target.name);
                out.blit(3, 2);
                out.push_ident(variant.name);
                {
                    let at = out.buf.len();
                    for (i, field) in ordered_fields.iter().enumerate() {
                        out.push_ident(field.name);
                        out.blit(884, 3);
                        out.buf.push(Literal::usize_unsuffixed(i).into());
                        out.blit_punct(13);
                    }
                    {
                        if let Some(flatten_field) = flattening {
                            out.push_ident(flatten_field.name);
                            out.blit(887, 6);
                        }
                    };
                    {
                        for field in variant.fields {
                            if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                                continue;
                            }
                            {
                                out.push_ident(field.name);
                                out.blit_punct(9);
                                field_from_default(out, field, FROM_JSON);
                                out.blit_punct(13);
                            }
                        }
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if untagged || ctx.target.ignore_tag_adjacent_fields {
                out.blit(893, 3);
            };
            out.tt_group(Delimiter::Brace, at);
        };
    }
}
fn other_variant_key(out: &mut RustWriter, field: &Field) {
    {
        out.blit(896, 4);
        {
            let at = out.buf.len();
            out.blit(900, 2);
            {
                let at = out.buf.len();
                out.blit(902, 5);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit(907, 6);
        out.buf.extend_from_slice(field.ty);
        out.blit(913, 14);
        {
            let at = out.buf.len();
            out.blit(927, 9);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        {
            let at = out.buf.len();
            out.blit_ident(197);
            {
                let at = out.buf.len();
                out.blit_ident(194);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(936, 5);
            {
                let at = out.buf.len();
                out.blit_ident(199);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(941, 4);
            {
                let at = out.buf.len();
                out.blit_ident(199);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_punct(0);
    };
}
fn enum_variant_from_json(out: &mut RustWriter, ctx: &Ctx, variant: &EnumVariant, untagged: bool) {
    let start = out.buf.len();
    match variant.kind {
        EnumKind::Tuple => {
            if ctx.target.content.is_some() {
                {
                    out.blit(945, 3);
                    {
                        let at = out.buf.len();
                        out.blit(687, 2);
                        {
                            let at = out.buf.len();
                            out.blit(948, 10);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            }
            let [field] = variant.fields else {
                Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                )
            };
            {
                out.blit_ident(192);
                {
                    if let Some(with) = field.with(FROM_JSON) {
                        out.buf.extend_from_slice(with);
                    } else {
                        out.blit_punct(3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(417, 9);
                        out.push_ident(&ctx.lifetime);
                        out.blit(426, 2);
                    }
                };
                out.blit(958, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(200);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(197);
                    {
                        let at = out.buf.len();
                        out.blit_ident(194);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(734, 2);
                    {
                        let at = out.buf.len();
                        {
                            if let Some(with) = field.validate(FROM_JSON) {
                                {
                                    out.blit(226, 4);
                                    {
                                        let at = out.buf.len();
                                        out.blit_punct(4);
                                        out.buf.extend_from_slice(field.ty);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.blit(230, 17);
                                    out.buf.extend_from_slice(with);
                                    out.blit(247, 4);
                                    {
                                        let at = out.buf.len();
                                        out.blit_ident(199);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.blit_punct(5);
                                    {
                                        let at = out.buf.len();
                                        out.blit_ident(150);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    {
                                        let at = out.buf.len();
                                        out.blit(961, 2);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    {
                                        let at = out.buf.len();
                                        out.blit(687, 2);
                                        {
                                            let at = out.buf.len();
                                            out.blit(963, 10);
                                            out.tt_group(Delimiter::Parenthesis, at);
                                        };
                                        out.tt_group(Delimiter::Brace, at);
                                    };
                                }
                            }
                        };
                        out.blit(878, 6);
                        ctx.target_type(out);
                        out.blit(689, 4);
                        {
                            let at = out.buf.len();
                            out.push_ident(&ctx.target.name);
                            out.blit(3, 2);
                            out.push_ident(variant.name);
                            {
                                let at = out.buf.len();
                                out.blit_ident(194);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        if untagged || ctx.target.ignore_tag_adjacent_fields {
                            out.blit(893, 3);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(939, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(146);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(734, 2);
                    {
                        let at = out.buf.len();
                        if !untagged {
                            out.blit(687, 2);
                            {
                                let at = out.buf.len();
                                out.blit_ident(146);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
            }
        }
        EnumKind::Struct => {
            if ctx.target.content.is_some() {
                {
                    out.blit(945, 3);
                    {
                        let at = out.buf.len();
                        out.blit(687, 2);
                        {
                            let at = out.buf.len();
                            out.blit(948, 10);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            }
            enum_variant_from_json_struct(out, ctx, variant, untagged);
        }
        EnumKind::None => {
            {
                if let Tag::Inline(..) = ctx.target.tag {
                    if ctx.target.content.is_none() {
                        {
                            out.blit(248, 3);
                            {
                                let at = out.buf.len();
                                out.blit_ident(199);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(973, 5);
                            {
                                let at = out.buf.len();
                                out.blit(687, 2);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(199);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    }
                }
            };
            out.blit(878, 6);
            ctx.target_type(out);
            out.blit(689, 4);
            {
                let at = out.buf.len();
                out.push_ident(&ctx.target.name);
                out.blit(3, 2);
                out.push_ident(variant.name);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if untagged || ctx.target.ignore_tag_adjacent_fields {
                out.blit(893, 3);
            };
        }
    };
    out.tt_group(Delimiter::Brace, start);
}
fn find_other_variant<'a, 'b>(variants: &'b [EnumVariant<'a>]) -> Option<&'b EnumVariant<'a>> {
    let mut other: Option<&EnumVariant> = None;
    for variant in variants {
        if variant.attr.has_other() {
            if other.is_some() {
                Error::span_msg(
                    "Only one other variant is currently supported.",
                    variant.name.span(),
                )
            }
            other = Some(variant);
        }
    }
    other
}
fn stringly_enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let other = find_other_variant(variants);
    let body = {
        let len = out.buf.len();
        out.blit(978, 5);
        {
            let at = out.buf.len();
            out.blit_ident(197);
            {
                let at = out.buf.len();
                out.blit_ident(182);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(734, 2);
            {
                let at = out.buf.len();
                out.blit(983, 5);
                {
                    let at = out.buf.len();
                    for variant in variants {
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit(734, 2);
                        out.push_ident(&ctx.target.name);
                        out.blit(3, 2);
                        out.push_ident(&variant.name);
                        out.blit_punct(13);
                    }
                    enum_from_json_unknown_variant(out, ctx, other, true);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(877, 7);
                ctx.target_type(out);
                out.blit(689, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(194);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(201);
            {
                let at = out.buf.len();
                out.blit_ident(199);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(941, 4);
            {
                let at = out.buf.len();
                out.blit_ident(199);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(13);
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_ident(197);
        {
            let at = out.buf.len();
            out.tt_group_empty(Delimiter::Parenthesis);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.split_off_stream(len)
    };
    impl_from_json(out, ctx, body)
}
fn enum_from_json_unknown_variant(
    out: &mut RustWriter,
    ctx: &Ctx,
    other: Option<&EnumVariant>,
    stringly: bool,
) {
    {
        out.blit(988, 3);
    };
    let start = out.buf.len();
    if ctx.target.ignore_tag_adjacent_fields && !stringly {
        {
            out.blit(248, 3);
            {
                let at = out.buf.len();
                out.blit_ident(199);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(973, 5);
            {
                let at = out.buf.len();
                out.blit(687, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(991, 5);
            {
                let at = out.buf.len();
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.blit_ident(141);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(941, 4);
                {
                    let at = out.buf.len();
                    out.blit(996, 8);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1004, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(163);
                    {
                        let at = out.buf.len();
                        out.blit_ident(99);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                {
                    let at = out.buf.len();
                    out.blit(1006, 6);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(939, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(941, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    } else if let Some(other) = &other {
        match other.fields {
            [] => (),
            [field] => other_variant_key(out, field),
            [_f1, f2, ..] => Error::span_msg(
                "Other variants may only have upto a single field",
                f2.name.span(),
            ),
        }
        if !stringly {
            let start = out.buf.len();
            {
                out.blit(248, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(973, 5);
                {
                    let at = out.buf.len();
                    out.blit(687, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(199);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
            };
            if ctx.target.content.is_some() {
                let skipbody = out.split_off_stream(start);
                out.blit(1012, 2);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, skipbody)));
            }
        }
        let start = out.buf.len();
        {
            out.push_ident(&ctx.target.name);
            out.blit(3, 2);
            out.push_ident(other.name);
            {
                if let [field] = other.fields {
                    match other.kind {
                        EnumKind::Tuple => {
                            let at = out.buf.len();
                            out.blit_ident(128);
                            out.tt_group(Delimiter::Parenthesis, at);
                        }
                        EnumKind::Struct => {
                            let at = out.buf.len();
                            out.push_ident(field.name);
                            out.blit(1014, 2);
                            out.tt_group(Delimiter::Brace, at);
                        }
                        EnumKind::None => (),
                    }
                }
            };
        };
        if !stringly {
            let value = out.split_off_stream(start);
            {
                out.blit(878, 6);
                ctx.target_type(out);
                out.blit(689, 4);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Parenthesis, value)));
                out.blit_punct(0);
            }
        }
    } else {
        {
            out.blit(1016, 11);
            {
                let at = out.buf.len();
                out.blit_ident(182);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1027, 3);
            {
                let at = out.buf.len();
                out.blit(1030, 10);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
        }
    }
    out.tt_group(Delimiter::Brace, start);
}
fn enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    if ctx.target.flattenable {
        Error::msg("Flattening enums not supported yet.")
    }
    let mut mixed_strings_and_objects = false;
    let inline_tag = match &ctx.target.tag {
        Tag::Inline(literal) => Some(literal),
        Tag::Untagged => {
            let outer = out.buf.len();
            {
                out.blit(1040, 11);
            };
            let start = out.buf.len();
            'always_succeed: {
                for (i, variant) in variants.iter().enumerate() {
                    if let EnumKind::None = variant.kind {
                        {
                            out.blit(878, 6);
                            ctx.target_type(out);
                            out.blit(689, 4);
                            {
                                let at = out.buf.len();
                                out.push_ident(&ctx.target.name);
                                out.blit(3, 2);
                                out.push_ident(variant.name);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                        };
                        break 'always_succeed;
                    }
                    {
                        if i != 0 {
                            out.blit(1051, 3);
                            {
                                let at = out.buf.len();
                                out.blit(1054, 2);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                        };
                        enum_variant_from_json(out, ctx, variant, true);
                    }
                }
                {
                    out.blit(687, 2);
                    {
                        let at = out.buf.len();
                        out.blit(1056, 10);
                        {
                            let at = out.buf.len();
                            out.blit(1066, 2);
                            out.buf.push(
                                Literal::string("Untagged enum didn't match any variant").into(),
                            );
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                }
            }
            out.tt_group(Delimiter::Brace, start);
            {
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            let ts = out.split_off_stream(outer);
            impl_from_json(out, ctx, ts);
            return;
        }
        Tag::Default => {
            if ctx.target.enum_flags & (ENUM_CONTAINS_TUPLE_VARIANT | ENUM_CONTAINS_STRUCT_VARIANT)
                == 0
            {
                return stringly_enum_from_json(out, ctx, variants);
            } else if ctx.target.enum_flags & ENUM_CONTAINS_UNIT_VARIANT != 0 {
                mixed_strings_and_objects = true;
            }
            None
        }
    };
    let other = find_other_variant(variants);
    let body_start = out.buf.len();
    if let Some(tag) = inline_tag {
        {
            if ctx.target.content.is_some() {
                out.blit(1068, 5);
            };
            out.blit(1073, 6);
            {
                if let Some(content) = &ctx.target.content {
                    {
                        out.blit_ident(63);
                        {
                            let at = out.buf.len();
                            out.buf.push(Literal::string(tag).into());
                            out.blit_punct(13);
                            out.buf.push(Literal::string(content).into());
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                } else {
                    {
                        out.blit_ident(64);
                        {
                            let at = out.buf.len();
                            out.buf.push(Literal::string(tag).into());
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                }
            };
            {
                let at = out.buf.len();
                {
                    if let Some(..) = &ctx.target.content {
                        {
                            out.blit_ident(197);
                            {
                                let at = out.buf.len();
                                {
                                    let at = out.buf.len();
                                    out.blit(1079, 3);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(734, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1082, 5);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    } else {
                        {
                            out.blit_ident(197);
                            {
                                let at = out.buf.len();
                                out.blit_ident(194);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(936, 4);
                        }
                    }
                };
                out.blit_ident(201);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(941, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    } else {
        {
            out.blit(1087, 2);
            {
                let at = out.buf.len();
                out.blit_ident(163);
                {
                    let at = out.buf.len();
                    if ctx.target.ignore_tag_adjacent_fields {
                        out.blit_ident(195);
                    };
                    out.blit_ident(182);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(973, 3);
            {
                if mixed_strings_and_objects {
                    out.blit_ident(35);
                } else {
                    out.blit_ident(34);
                }
            };
            out.blit(1089, 2);
            {
                let at = out.buf.len();
                out.blit(687, 2);
                {
                    let at = out.buf.len();
                    out.blit(1091, 10);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    }
    if ctx.target.ignore_tag_adjacent_fields {
        out.blit(1101, 4);
    }
    let variant_dispatch_start = out.buf.len();
    {
        out.blit(986, 2);
        {
            let at = out.buf.len();
            {
                for variant in variants {
                    if mixed_strings_and_objects {
                        if let EnumKind::None = variant.kind {
                            continue;
                        }
                    }
                    if other.is_some() && variant.attr.has_other() {
                        continue;
                    }
                    {
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit(734, 2);
                        enum_variant_from_json(out, ctx, variant, false);
                        out.blit_punct(13);
                    };
                }
            };
            enum_from_json_unknown_variant(out, ctx, other, false);
            out.tt_group(Delimiter::Brace, at);
        };
    };
    if let Some(_) = inline_tag {
        if ctx.target.content.is_some() {
            {
                out.blit(1012, 2);
                {
                    let at = out.buf.len();
                    out.blit(1105, 5);
                    {
                        let at = out.buf.len();
                        out.blit_ident(197);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(1110, 3);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(1113, 4);
                        {
                            let at = out.buf.len();
                            out.blit(1117, 10);
                            ctx.target_type(out);
                            out.blit_punct(1);
                            {
                                let at = out.buf.len();
                                out.blit(1127, 7);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(1134, 2);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(148);
                {
                    let at = out.buf.len();
                    out.blit_ident(197);
                    {
                        let at = out.buf.len();
                        out.tt_group_empty(Delimiter::Parenthesis);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
            }
        } else {
            {
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            }
        }
    } else if ctx.target.ignore_tag_adjacent_fields {
        out.tt_group(Delimiter::Brace, variant_dispatch_start);
        {
            out.blit(1105, 5);
            {
                let at = out.buf.len();
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1136, 4);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1113, 4);
                {
                    let at = out.buf.len();
                    out.blit(1117, 10);
                    ctx.target_type(out);
                    out.blit_punct(1);
                    {
                        let at = out.buf.len();
                        out.blit(1127, 7);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(1140, 3);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    } else {
        {
            out.blit(1143, 8);
            {
                let at = out.buf.len();
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.blit_ident(141);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                {
                    let at = out.buf.len();
                    out.blit(1138, 2);
                    {
                        let at = out.buf.len();
                        out.tt_group_empty(Delimiter::Parenthesis);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(939, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(201);
                    {
                        let at = out.buf.len();
                        out.blit_ident(199);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(1004, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(163);
                    {
                        let at = out.buf.len();
                        out.blit_ident(185);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(201);
                    {
                        let at = out.buf.len();
                        out.blit(1151, 8);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(1159, 11);
            ctx.target_type(out);
            out.blit_punct(1);
            {
                let at = out.buf.len();
                out.blit(1127, 7);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1170, 4);
        }
    }
    let mut body = out.split_off_stream(body_start);
    if mixed_strings_and_objects {
        body = {
            let len = out.buf.len();
            out.blit(1174, 5);
            {
                let at = out.buf.len();
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.blit(1179, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
                out.blit(1004, 2);
                {
                    let at = out.buf.len();
                    out.blit(1191, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1203, 4);
                {
                    let at = out.buf.len();
                    out.blit(1207, 4);
                    out.tt_group(Delimiter::Brace, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(197);
                    {
                        let at = out.buf.len();
                        out.blit_ident(182);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(734, 2);
                    {
                        let at = out.buf.len();
                        out.blit(983, 5);
                        {
                            let at = out.buf.len();
                            {
                                for variant in variants {
                                    if let EnumKind::None = variant.kind {
                                        out.buf.push(variant_key_literal(ctx, variant).into());
                                        out.blit(734, 2);
                                        out.push_ident(&ctx.target.name);
                                        out.blit(3, 2);
                                        out.push_ident(variant.name);
                                        out.blit_punct(13);
                                    }
                                }
                            };
                            enum_from_json_unknown_variant(out, ctx, other, true);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(877, 7);
                        ctx.target_type(out);
                        out.blit(689, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(194);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(1211, 3);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_ident(201);
                    {
                        let at = out.buf.len();
                        out.blit_ident(199);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(941, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(199);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(1004, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(185);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(734, 2);
                {
                    let at = out.buf.len();
                    out.blit(687, 2);
                    {
                        let at = out.buf.len();
                        out.blit(1214, 8);
                        {
                            let at = out.buf.len();
                            out.blit(1066, 2);
                            out.buf.push(
                                Literal::string("Expected either an object or a string").into(),
                            );
                            out.blit_punct(13);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(201);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(941, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(199);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.split_off_stream(len)
        }
    }
    impl_from_json(out, ctx, body);
}
fn handle_pod_binary_any_struct(out: &mut RustWriter, ctx: &Ctx<'_>, fields: &[Field]) -> bool {
    if !ctx.target.generics.is_empty() {
        Error::msg("Pod derive doesn't support generics or lifetimes yet.");
    }
    if !match ctx.target.repr {
        ast::Repr::Transparent | ast::Repr::C => true,
        _ => false,
    } {
        Error::msg("Pod type must be either repr(transparent) or repr(C)");
    }
    {
        out.blit(1222, 9);
        {
            let at = out.buf.len();
            out.blit(1231, 3);
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit(1234, 3);
        {
            let at = out.buf.len();
            for (i, field) in fields.iter().enumerate() {
                if i != 0 {
                    out.blit_punct(4);
                };
                out.blit_punct(3);
                out.buf.extend_from_slice(field.ty);
                out.blit(1237, 4);
                {
                    if ctx.target.from_binary {
                        out.blit_ident(139);
                    } else {
                        out.blit_ident(145);
                    }
                };
                out.blit(29, 4);
            }
            out.blit_punct(13);
            {
                out.buf.push(TokenTree::Literal(Literal::string(
                    "Not all fields implement POD",
                )))
            };
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit(1234, 3);
        {
            let at = out.buf.len();
            out.blit(1241, 4);
            out.buf.push(TokenTree::from(ctx.target.name.clone()));
            out.blit(1245, 4);
            for (i, field) in fields.iter().enumerate() {
                if i != 0 {
                    out.blit_punct(18);
                };
                out.blit(1241, 4);
                out.buf.extend_from_slice(field.ty);
                out.blit(311, 2);
            }
            out.blit_punct(13);
            {
                out.buf.push(TokenTree::Literal(Literal::string(
                    "struct has gaps between fields",
                )))
            };
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit_punct(0);
    };
    if fields.len() < 2 {
        return false;
    }
    if ctx.target.to_binary {
        let start = out.buf.len();
        {
            out.blit_ident(184);
            {
                let at = out.buf.len();
                out.blit(1249, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(189);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
        };
        let body = out.split_off_stream(start);
        impl_to_binary(out, &ctx, body, None);
    }
    if ctx.target.from_binary {
        let start = out.buf.len();
        {
            out.blit_ident(184);
            {
                let at = out.buf.len();
                out.blit(1252, 4);
                out.tt_group(Delimiter::Brace, at);
            };
        };
        let body = out.split_off_stream(start);
        impl_from_binary(out, &ctx, body, None);
    }
    true
}
fn handle_struct(output: &mut RustWriter, target: &DeriveTargetInner, fields: &[Field]) {
    let ctx = Ctx::new(output, target);
    if target.from_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                Error::msg("Struct must contain a single field to use transparent")
            };
            if !match target.repr {
                ast::Repr::Transparent => true,
                _ => false,
            } {
                Error::msg("transparent FromJson requires #[repr(transparent)]")
            }
            let body = {
                let len = output.buf.len();
                output.blit_punct(3);
                output.buf.extend_from_slice(single_field.ty);
                output.blit_ident(191);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(1256, 7);
                {
                    let at = output.buf.len();
                    output.blit(440, 3);
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.split_off_stream(len)
            };
            impl_from_json(output, &ctx, body);
        } else {
            struct_from_json(output, &ctx, fields);
        }
    }
    if target.to_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                Error::msg("Struct must contain a single field to use transparent")
            };
            let body = {
                let len = output.buf.len();
                output.blit(472, 2);
                output.push_ident(single_field.name);
                output.blit(540, 2);
                {
                    let at = output.buf.len();
                    output.blit_ident(202);
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.split_off_stream(len)
            };
            impl_to_json(output, ToJsonKind::Forward(&single_field), &ctx, body);
        } else {
            struct_to_json(output, &ctx, fields);
        }
    }
    if target.pod {
        if handle_pod_binary_any_struct(output, &ctx, fields) {
            return;
        }
    }
    let mut auto_pod = None;
    if target.transparent_impl
        && match target.repr {
            ast::Repr::Transparent | ast::Repr::C => true,
            _ => false,
        }
    {
        if let [field] = &fields {
            auto_pod = Some(field);
        }
    }
    if target.to_binary {
        let start = output.buf.len();
        if let Some(version) = target.version {
            {
                output.blit(1263, 3);
                {
                    let at = output.buf.len();
                    {
                        output
                            .buf
                            .push(TokenTree::Literal(Literal::u16_unsuffixed(version)))
                    };
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.blit_punct(0);
            }
        }
        for field in fields {
            encode_binary_field(output, &ctx, field, &|out| {
                out.blit(471, 3);
                out.push_ident(field.name);
            })
        }
        let body = output.split_off_stream(start);
        impl_to_binary(output, &ctx, body, auto_pod);
    }
    if target.from_binary {
        let start = output.buf.len();
        {
            decode_binary_version(output, &ctx);
            output.push_ident(&target.name);
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            (output).push_ident(field.name);
                            (output).blit_punct(9);
                            decode_binary_field(output, &ctx, field);
                            (output).blit_punct(13);
                        }
                    }
                };
                output.tt_group(Delimiter::Brace, at);
            };
        };
        let body = output.split_off_stream(start);
        impl_from_binary(output, &ctx, body, auto_pod);
    }
}
fn decode_binary_version(out: &mut RustWriter, ctx: &Ctx) {
    let Some(version) = ctx.target.version else {
        return;
    };
    {
        out.blit(1266, 11);
        out.buf
            .push(TokenTree::Literal(Literal::u16_unsuffixed(version)));
        if ctx.target.min_version > 0 {
            out.blit(1277, 4);
            out.buf.push(TokenTree::Literal(Literal::u16_unsuffixed(
                ctx.target.min_version,
            )));
        };
        {
            let at = out.buf.len();
            out.blit(1281, 3);
            {
                let at = out.buf.len();
                out.blit(1284, 2);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::Literal(Literal::string(
                        "{} unknown version = {} found. (versions {}..={} supported)",
                    )));
                    out.blit_punct(13);
                    out.buf.push(TokenTree::Literal(Literal::string(
                        &ctx.target.name.to_string(),
                    )));
                    out.blit(1286, 3);
                    out.buf.push(TokenTree::Literal(Literal::u16_unsuffixed(
                        ctx.target.min_version,
                    )));
                    out.blit_punct(13);
                    out.buf
                        .push(TokenTree::Literal(Literal::u16_unsuffixed(version)));
                    out.blit_punct(13);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            out.tt_group(Delimiter::Brace, at);
        };
    };
}
fn handle_tuple_struct(output: &mut RustWriter, target: &DeriveTargetInner, fields: &[Field]) {
    let ctx = Ctx::new(output, target);
    if target.from_json {
        tuple_struct_from_json(output, &ctx, fields);
    }
    if target.to_json {
        tuple_struct_to_json(output, &ctx, fields);
    }
    if target.pod {
        if handle_pod_binary_any_struct(output, &ctx, fields) {
            return;
        }
    }
    let mut auto_pod = None;
    if target.transparent_impl
        && match target.repr {
            ast::Repr::Transparent | ast::Repr::C => true,
            _ => false,
        }
    {
        if let [field] = &fields {
            auto_pod = Some(field);
        }
    }
    if target.to_binary {
        let body = {
            let len = output.buf.len();
            if let Some(version) = target.version {
                output.blit(1263, 3);
                {
                    let at = output.buf.len();
                    {
                        output
                            .buf
                            .push(TokenTree::Literal(Literal::u16_unsuffixed(version)))
                    };
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.blit_punct(0);
            };
            {
                for (i, field) in fields.iter().enumerate() {
                    encode_binary_field(output, &ctx, field, &|out| {
                        out.blit(471, 3);
                        out.buf
                            .push(TokenTree::Literal(Literal::usize_unsuffixed(i)));
                    })
                }
            };
            output.split_off_stream(len)
        };
        impl_to_binary(output, &ctx, body, auto_pod)
    }
    if target.from_binary {
        let body = {
            let len = output.buf.len();
            decode_binary_version(output, &ctx);
            output.push_ident(&target.name);
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            decode_binary_field(output, &ctx, field);
                            (output).blit_punct(13);
                        }
                    }
                };
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.split_off_stream(len)
        };
        impl_from_binary(output, &ctx, body, auto_pod)
    }
}
fn enum_to_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let target = &ctx.target;
    let any_generics = !target.generics.is_empty();
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        Error::msg(
            "FromStr enum must not have any tuple or struct variants nor a tag configuration",
        )
    }
    {
        out.blit_punct(14);
        {
            let at = out.buf.len();
            out.blit_ident(165);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit_ident(159);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, DEF);
            out.blit_punct(1);
        };
        out.push_ident(&target.name);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, USE);
            out.blit_punct(1);
        };
        if !target.where_clauses.is_empty() {
            out.blit(1289, 2);
            out.buf.extend_from_slice(&target.where_clauses);
        };
        {
            let at = out.buf.len();
            out.blit(1291, 3);
            {
                let at = out.buf.len();
                out.blit(151, 2);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1294, 6);
            {
                let at = out.buf.len();
                out.blit(732, 2);
                {
                    let at = out.buf.len();
                    for variant in variants {
                        out.push_ident(&target.name);
                        out.blit(3, 2);
                        out.push_ident(variant.name);
                        out.blit(734, 2);
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit_punct(13);
                    }
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
}
fn enum_from_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let target = &ctx.target;
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        Error::msg(
            "FromStr enum must not have any tuple or struct variants nor a tag configuration",
        )
    }
    let any_generics = !target.generics.is_empty();
    {
        out.blit_punct(14);
        {
            let at = out.buf.len();
            out.blit_ident(165);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit_ident(159);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, DEF);
            out.blit_punct(1);
        };
        out.blit(1300, 10);
        out.push_ident(&target.name);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, USE);
            out.blit_punct(1);
        };
        if !target.where_clauses.is_empty() {
            out.blit(1289, 2);
            out.buf.extend_from_slice(&target.where_clauses);
        };
        {
            let at = out.buf.len();
            out.blit(1310, 15);
            {
                let at = out.buf.len();
                out.blit(1325, 4);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1329, 16);
            {
                let at = out.buf.len();
                out.blit_ident(197);
                {
                    let at = out.buf.len();
                    out.blit(1345, 2);
                    {
                        let at = out.buf.len();
                        for variant in variants {
                            out.buf.push(variant_key_literal(ctx, variant).into());
                            out.blit(734, 2);
                            out.push_ident(&target.name);
                            out.blit(3, 2);
                            out.push_ident(variant.name);
                            out.blit_punct(13);
                        }
                        out.blit(1347, 5);
                        {
                            let at = out.buf.len();
                            out.blit(1313, 9);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
}
fn enum_to_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let body = {
        let len = out.buf.len();
        if let Some(version) = ctx.target.version {
            out.blit(1263, 3);
            {
                let at = out.buf.len();
                {
                    out.buf
                        .push(TokenTree::Literal(Literal::u16_unsuffixed(version)))
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
        };
        out.blit(732, 2);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    emit_variant_pattern(out, ctx, variant, false);
                    match variant.kind {
                        EnumKind::Tuple => {
                            out.blit(734, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1263, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf
                                        .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        encode_binary_field(out, ctx, field, &|out| {
                                            out.push_ident(&ctx.temp[i]);
                                        })
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::Struct => {
                            out.blit(734, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1263, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf
                                        .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                {
                                    for field in variant.fields {
                                        encode_binary_field(out, ctx, field, &|out| {
                                            out.push_ident(field.name);
                                        })
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::None => {
                            out.blit(734, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1263, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        TokenTree::Literal(Literal::u8_unsuffixed(i as u8)).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    }
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.split_off_stream(len)
    };
    impl_to_binary(out, ctx, body, None);
}
fn enum_from_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let mut fallback = &variants[0];
    for variant in variants {
        if match variant.kind {
            EnumKind::None => true,
            _ => false,
        } {
            fallback = variant;
            break;
        }
    }
    let body = {
        let len = out.buf.len();
        decode_binary_version(out, ctx);
        out.blit(1352, 5);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        out.buf
                            .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                        out.blit(734, 2);
                        decode_binary_enum_variant(out, ctx, variant);
                        out.blit_punct(13);
                    }
                }
            };
            out.blit(1357, 3);
            {
                let at = out.buf.len();
                out.blit(1281, 3);
                {
                    let at = out.buf.len();
                    out.blit(1284, 2);
                    {
                        let at = out.buf.len();
                        out.buf.push(TokenTree::Literal(Literal::string(
                            "{} unknown binary enum tag = {}",
                        )));
                        out.blit_punct(13);
                        out.buf.push(TokenTree::Literal(Literal::string(
                            &ctx.target.name.to_string(),
                        )));
                        out.blit(1360, 3);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                decode_binary_enum_variant(out, ctx, fallback);
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.split_off_stream(len)
    };
    impl_from_binary(out, ctx, body, None);
}
fn decode_binary_enum_variant(out: &mut RustWriter, ctx: &Ctx, variant: &EnumVariant) {
    match variant.kind {
        EnumKind::Tuple => {
            out.push_ident(&ctx.target.name);
            out.blit(3, 2);
            out.push_ident(variant.name);
            {
                let at = out.buf.len();
                {
                    for field in variant.fields {
                        {
                            decode_binary_field(out, ctx, field);
                            out.blit_punct(13);
                        }
                    }
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
        EnumKind::Struct => {
            out.push_ident(&ctx.target.name);
            out.blit(3, 2);
            out.push_ident(variant.name);
            {
                let at = out.buf.len();
                {
                    for field in variant.fields {
                        {
                            out.push_ident(field.name);
                            out.blit_punct(9);
                            decode_binary_field(out, ctx, field);
                            out.blit_punct(13);
                        }
                    }
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
        EnumKind::None => {
            out.push_ident(&ctx.target.name);
            out.blit(3, 2);
            out.push_ident(variant.name);
        }
    }
}
fn handle_enum(output: &mut RustWriter, target: &DeriveTargetInner, variants: &[EnumVariant]) {
    let mut ctx = Ctx::new(output, target);
    let mut max_tuples = 0;
    for var in variants {
        if match var.kind {
            EnumKind::Tuple | EnumKind::Struct => true,
            _ => false,
        } {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp.clear();
    for n in 0..max_tuples {
        ctx.temp.push(var(n));
    }
    if target.from_str {
        enum_from_str(output, &ctx, variants);
    }
    if target.to_str {
        enum_to_str(output, &ctx, variants);
    }
    if target.to_json {
        enum_to_json(output, &ctx, variants);
    }
    if target.from_json {
        enum_from_json(output, &ctx, variants);
    }
    if target.to_binary {
        enum_to_binary(output, &ctx, variants);
    }
    if target.from_binary {
        enum_from_binary(output, &ctx, variants);
    }
}
pub fn inner_derive(stream: TokenStream) -> TokenStream {
    let outer_tokens: Vec<TokenTree> = stream.into_iter().collect();
    let mut target = DeriveTargetInner {
        transparent_impl: false,
        name: Ident::new("a", Span::call_site()),
        generics: Vec::new(),
        generic_field_types: Vec::new(),
        ignore_tag_adjacent_fields: false,
        where_clauses: &[],
        path_override: None,
        from_binary: false,
        from_json: false,
        to_binary: false,
        to_json: false,
        to_str: false,
        pod: false,
        from_str: false,
        enum_flags: 0,
        content: None,
        flattenable: false,
        tag: Tag::Default,
        rename_all: crate::case::RenameRule::None,
        rename_all_fields: crate::case::RenameRule::None,
        repr: ast::Repr::Default,
        version: None,
        min_version: 0,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens);
    if !(target.from_binary
        || target.to_binary
        || target.to_json
        || target.from_json
        || target.from_str
        || target.to_str)
    {
        target.from_json = true;
    }
    let field_toks: Vec<TokenTree> = body.into_iter().collect();
    let mut tt_buf = Vec::<TokenTree>::new();
    let mut field_buf = Vec::<Field>::new();
    let mut pool = MemoryPool::<FieldAttrs>::new();
    let mut attr_buf = pool.allocator();
    let mut rust_writer = RustWriter::new();
    match kind {
        DeriveTargetKind::Struct => {
            ast::parse_struct_fields(&mut field_buf, &field_toks, &mut attr_buf);
            ast::scan_fields(&mut target, &mut field_buf);
            handle_struct(&mut rust_writer, &target, &field_buf);
        }
        DeriveTargetKind::TupleStruct => {
            let t = Ident::new("a", Span::call_site());
            ast::parse_tuple_fields(&t, &mut field_buf, &field_toks, &mut attr_buf);
            ast::scan_fields(&mut target, &mut field_buf);
            handle_tuple_struct(&mut rust_writer, &target, &field_buf);
        }
        DeriveTargetKind::Enum => {
            let variants = ast::parse_enum(
                &mut target,
                &field_toks,
                &mut tt_buf,
                &mut field_buf,
                &mut attr_buf,
            );
            handle_enum(&mut rust_writer, &target, &variants);
        }
    }
    let ts = rust_writer.split_off_stream(0);
    {
        let len = (&mut rust_writer).buf.len();
        (&mut rust_writer).blit_punct(14);
        {
            let at = (&mut rust_writer).buf.len();
            (&mut rust_writer).blit_ident(19);
            {
                let at = (&mut rust_writer).buf.len();
                (&mut rust_writer).blit(1363, 4);
                (&mut rust_writer).tt_group(Delimiter::Parenthesis, at);
            };
            (&mut rust_writer).tt_group(Delimiter::Bracket, at);
        };
        (&mut rust_writer).blit(1367, 5);
        (&mut rust_writer)
            .buf
            .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
        (&mut rust_writer).blit_punct(0);
        (&mut rust_writer).split_off_stream(len)
    }
}
pub fn derive(stream: TokenStream) -> TokenStream {
    Error::try_catch_handle(stream, inner_derive)
}
