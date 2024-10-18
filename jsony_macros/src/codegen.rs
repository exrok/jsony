use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttr, Generic,
    GenericKind, Tag,
};
use crate::case::RenameRule;
use crate::util::MemoryPool;
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
fn tt_append_blit(output: &mut Vec<TokenTree>, chr: &str) {
    output.extend(chr.as_bytes().iter().map(|tok| match *tok {
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
#[allow(unused)]
fn tt_append(output: &mut Vec<TokenTree>, chr: &'static [StaticToken]) {
    output.extend(chr.iter().map(|tok| match tok {
        StaticIdent(value) => TokenTree::Ident(Ident::new(value, Span::call_site())),
        StaticPunct(chr, spacing) => TokenTree::Punct(Punct::new(
            *chr,
            if *spacing {
                Spacing::Joint
            } else {
                Spacing::Alone
            },
        )),
    }));
}
fn tt_punct_alone(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Alone)));
}
fn tt_punct_joint(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Joint)));
}
fn tt_ident(out: &mut Vec<TokenTree>, ident: &str) {
    out.push(TokenTree::Ident(Ident::new(ident, Span::call_site())));
}
fn tt_group_empty(out: &mut Vec<TokenTree>, delimiter: Delimiter) {
    out.push(TokenTree::Group(Group::new(delimiter, TokenStream::new())));
}
fn tt_group(out: &mut Vec<TokenTree>, delimiter: Delimiter, from: usize) {
    let group = TokenTree::Group(Group::new(
        delimiter,
        TokenStream::from_iter(out.drain(from..)),
    ));
    out.push(group);
}

struct GenericBoundFormating {
    lifetimes: bool,
    bounds: bool,
}
fn fmt_generics(buffer: &mut Vec<TokenTree>, generics: &[Generic], fmt: GenericBoundFormating) {
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
            tt_punct_alone(buffer, ',');
        }
        match generic.kind {
            GenericKind::Lifetime => {
                tt_punct_joint(buffer, '\'');
            }
            GenericKind::Type => (),
            GenericKind::Const => {
                tt_ident(buffer, "const");
            }
        }
        buffer.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            tt_punct_alone(buffer, ':');
            buffer.extend(generic.bounds.iter().cloned());
        }
    }
}
const DEAD_USE: GenericBoundFormating = GenericBoundFormating {
    lifetimes: false,
    bounds: false,
};
const USE: GenericBoundFormating = GenericBoundFormating {
    lifetimes: true,
    bounds: false,
};
const DEF: GenericBoundFormating = GenericBoundFormating {
    lifetimes: true,
    bounds: true,
};
fn bodyless_impl_from(
    output: &mut Vec<TokenTree>,
    sub: Option<Ident>,
    trait_name: Ident,
    Ctx {
        target,
        lifetime,
        crate_path,
        generics,
        ..
    }: &Ctx,
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        tt_ident(output, "impl");
        tt_punct_alone(output, '<');
        tt_punct_joint(output, '\'');
        output.push(TokenTree::from(lifetime.clone()));
        if !generics.is_empty() {
            tt_punct_alone(output, ',');
            {
                fmt_generics(output, generics, DEF)
            };
        };
        tt_punct_alone(output, '>');
        output.extend_from_slice(&crate_path);
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
        if let Some(sub) = sub {
            output.push(TokenTree::from(sub.clone()));
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
        };
        output.push(TokenTree::from(trait_name.clone()));
        tt_punct_alone(output, '<');
        tt_punct_joint(output, '\'');
        output.push(TokenTree::from(lifetime.clone()));
        tt_punct_alone(output, '>');
        tt_ident(output, "for");
        output.push(TokenTree::from(target.name.clone()));
        if any_generics {
            tt_punct_alone(output, '<');
            {
                fmt_generics(output, &target.generics, USE)
            };
            tt_punct_alone(output, '>');
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            tt_ident(output, "where");
            for ty in &target.generic_field_types {
                output.extend_from_slice(ty);
                tt_punct_alone(output, ':');
                output.push(TokenTree::from(trait_name.clone()));
                tt_punct_alone(output, '<');
                tt_punct_joint(output, '\'');
                output.push(TokenTree::from(lifetime.clone()));
                tt_punct_alone(output, '>');
                tt_punct_alone(output, ',');
            }
            output.extend_from_slice(&target.where_clauses);
        };
    };
    Ok(())
}
fn impl_from_binary(
    output: &mut Vec<TokenTree>,
    ctx: &Ctx,
    inner: TokenStream,
) -> Result<(), Error> {
    {
        tt_ident(output, "unsafe");
        if let Err(err) = bodyless_impl_from(
            output,
            None,
            Ident::new("FromBinary", Span::call_site()),
            ctx,
        ) {
            return Err(err);
        };
        {
            let at = output.len();
            tt_ident(output, "fn");
            tt_ident(output, "binary_decode");
            {
                let at = output.len();
                tt_ident(output, "decoder");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                tt_ident(output, "mut");
                output.extend_from_slice(&ctx.crate_path);
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "binary");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "Decoder");
                tt_punct_alone(output, '<');
                tt_punct_joint(output, '\'');
                output.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(output, '>');
                tt_group(output, Delimiter::Parenthesis, at);
            };
            tt_punct_joint(output, '-');
            tt_punct_alone(output, '>');
            tt_ident(output, "Self");
            {
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            tt_group(output, Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_from_json_field_visitor(
    output: &mut Vec<TokenTree>,
    ctx: &Ctx,
    ty: &dyn Fn(&mut Vec<TokenTree>),
    inner: TokenStream,
) -> Result<(), Error> {
    {
        tt_ident(output, "unsafe");
        if let Err(err) = bodyless_impl_from(
            output,
            Some(Ident::new("json", Span::call_site())),
            Ident::new("FromJsonFieldVisitor", Span::call_site()),
            ctx,
        ) {
            return Err(err);
        };
        {
            let at = output.len();
            tt_ident(output, "type");
            tt_ident(output, "Vistor");
            tt_punct_alone(output, '=');
            {
                ty(output)
            };
            tt_punct_alone(output, ';');
            tt_ident(output, "unsafe");
            tt_ident(output, "fn");
            tt_ident(output, "new_field_visitor");
            {
                let at = output.len();
                tt_ident(output, "dst");
                tt_punct_alone(output, ':');
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "std");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "ptr");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "NonNull");
                tt_punct_alone(output, '<');
                tt_group_empty(output, Delimiter::Parenthesis);
                tt_punct_alone(output, '>');
                tt_punct_alone(output, ',');
                tt_ident(output, "parser");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                output.extend_from_slice(&ctx.crate_path);
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "parser");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "Parser");
                tt_punct_alone(output, '<');
                tt_punct_joint(output, '\'');
                output.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(output, '>');
                tt_group(output, Delimiter::Parenthesis, at);
            };
            tt_punct_joint(output, '-');
            tt_punct_alone(output, '>');
            tt_ident(output, "Self");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "Vistor");
            {
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            tt_group(output, Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_from_json(output: &mut Vec<TokenTree>, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    {
        tt_ident(output, "unsafe");
        if let Err(err) =
            bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)
        {
            return Err(err);
        };
        {
            let at = output.len();
            tt_ident(output, "unsafe");
            tt_ident(output, "fn");
            tt_ident(output, "emplace_from_json");
            {
                let at = output.len();
                tt_ident(output, "dst");
                tt_punct_alone(output, ':');
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "std");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "ptr");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "NonNull");
                tt_punct_alone(output, '<');
                tt_group_empty(output, Delimiter::Parenthesis);
                tt_punct_alone(output, '>');
                tt_punct_alone(output, ',');
                tt_ident(output, "parser");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                tt_ident(output, "mut");
                output.extend_from_slice(&ctx.crate_path);
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "parser");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "Parser");
                tt_punct_alone(output, '<');
                tt_punct_joint(output, '\'');
                output.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(output, '>');
                tt_group(output, Delimiter::Parenthesis, at);
            };
            tt_punct_joint(output, '-');
            tt_punct_alone(output, '>');
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "std");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "result");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "Result");
            tt_punct_alone(output, '<');
            tt_group_empty(output, Delimiter::Parenthesis);
            tt_punct_alone(output, ',');
            tt_punct_alone(output, '&');
            tt_punct_joint(output, '\'');
            tt_ident(output, "static");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "jsony");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "json");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "DecodeError");
            tt_punct_alone(output, '>');
            {
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            tt_group(output, Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_to_binary(
    output: &mut Vec<TokenTree>,
    Ctx {
        target, crate_path, ..
    }: &Ctx,
    inner: TokenStream,
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        tt_ident(output, "unsafe");
        tt_ident(output, "impl");
        if any_generics {
            tt_punct_alone(output, '<');
            {
                fmt_generics(output, &target.generics, DEF)
            };
            tt_punct_alone(output, '>');
        };
        output.extend_from_slice(&crate_path);
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
        tt_ident(output, "ToBinary");
        tt_ident(output, "for");
        output.push(TokenTree::from(target.name.clone()));
        if any_generics {
            tt_punct_alone(output, '<');
            {
                fmt_generics(output, &target.generics, USE)
            };
            tt_punct_alone(output, '>');
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            tt_ident(output, "where");
            {
                for ty in &target.generic_field_types {
                    {
                        output.extend_from_slice(ty);
                        tt_punct_alone(output, ':');
                        tt_ident(output, "ToBinary");
                        tt_punct_alone(output, ',');
                    }
                }
            };
        };
        {
            let at = output.len();
            tt_ident(output, "fn");
            tt_ident(output, "binary_encode");
            {
                let at = output.len();
                tt_punct_alone(output, '&');
                tt_ident(output, "self");
                tt_punct_alone(output, ',');
                tt_ident(output, "encoder");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                tt_ident(output, "mut");
                tt_ident(output, "Vec");
                tt_punct_alone(output, '<');
                tt_ident(output, "u8");
                tt_punct_alone(output, '>');
                tt_group(output, Delimiter::Parenthesis, at);
            };
            {
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            tt_group(output, Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_to_json(
    output: &mut Vec<TokenTree>,
    kind: &str,
    Ctx {
        target, crate_path, ..
    }: &Ctx,
    inner: TokenStream,
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        tt_ident(output, "impl");
        if any_generics {
            tt_punct_alone(output, '<');
            {
                fmt_generics(output, &target.generics, DEF)
            };
            tt_punct_alone(output, '>');
        };
        output.extend_from_slice(&crate_path);
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
        tt_ident(output, "ToJson");
        tt_ident(output, "for");
        output.push(TokenTree::from(target.name.clone()));
        if any_generics {
            tt_punct_alone(output, '<');
            {
                fmt_generics(output, &target.generics, USE)
            };
            tt_punct_alone(output, '>');
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            tt_ident(output, "where");
            {
                for ty in &target.generic_field_types {
                    {
                        output.extend_from_slice(ty);
                        tt_punct_alone(output, ':');
                        output.extend_from_slice(&crate_path);
                        tt_punct_joint(output, ':');
                        tt_punct_alone(output, ':');
                        tt_ident(output, "ToBinary");
                        tt_punct_alone(output, ',');
                    }
                }
            };
        };
        {
            let at = output.len();
            tt_ident(output, "type");
            tt_ident(output, "Kind");
            tt_punct_alone(output, '=');
            output.extend_from_slice(&crate_path);
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "json");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            output.push(Ident::new(kind, Span::call_site()).into());
            tt_punct_alone(output, ';');
            tt_ident(output, "fn");
            tt_ident(output, "jsonify_into");
            {
                let at = output.len();
                tt_punct_alone(output, '&');
                tt_ident(output, "self");
                tt_punct_alone(output, ',');
                tt_ident(output, "out");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                tt_ident(output, "mut");
                tt_ident(output, "jsony");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "TextWriter");
                tt_group(output, Delimiter::Parenthesis, at);
            };
            tt_punct_joint(output, '-');
            tt_punct_alone(output, '>');
            tt_ident(output, "Self");
            tt_punct_joint(output, ':');
            tt_punct_alone(output, ':');
            tt_ident(output, "Kind");
            {
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            tt_group(output, Delimiter::Brace, at);
        };
    };
    Ok(())
}
struct Ctx<'a> {
    lifetime: Ident,
    generics: &'a [Generic<'a>],
    crate_path: Vec<TokenTree>,
    target: &'a DeriveTargetInner<'a>,
    temp: Vec<Ident>,
}
impl<'a> Ctx<'a> {
    pub fn dead_target_type(&self, out: &mut Vec<TokenTree>) {
        {
            out.push(TokenTree::from(self.target.name.clone()));
            if !self.target.generics.is_empty() {
                tt_punct_alone(out, '<');
                {
                    fmt_generics(out, &self.target.generics, DEAD_USE)
                };
                tt_punct_alone(out, '>');
            };
        }
    }
    pub fn target_type(&self, out: &mut Vec<TokenTree>) {
        {
            out.push(TokenTree::from(self.target.name.clone()));
            if !self.target.generics.is_empty() {
                tt_punct_alone(out, '<');
                {
                    fmt_generics(out, &self.target.generics, USE)
                };
                tt_punct_alone(out, '>');
            };
        }
    }
    fn new(target: &'a DeriveTargetInner) -> Result<Ctx<'a>, Error> {
        let crate_path = if let Some(value) = &target.path_override {
            let content = value.to_string();
            #[allow(unused)]
            let inner = &content[1..content.len() - 1];
            let mut out = Vec::<TokenTree>::new();
            {
                tt_punct_joint(&mut out, ':');
                tt_punct_alone(&mut out, ':');
                tt_ident(&mut out, "jsony");
            };
            out
        } else {
            let mut out = Vec::<TokenTree>::new();
            {
                tt_punct_joint(&mut out, ':');
                tt_punct_alone(&mut out, ':');
                tt_ident(&mut out, "jsony");
            };
            out
        };
        let (lt, generics) = if let [Generic {
            kind: GenericKind::Lifetime,
            ident,
            bounds,
        }, rest @ ..] = &target.generics[..]
        {
            if !bounds.is_empty() {
                return Err(Error::msg("Bounded lifetimes currently unsupported"));
            }
            ((*ident).clone(), rest)
        } else {
            (Ident::new("de", Span::call_site()), &target.generics[..])
        };
        Ok(Ctx {
            lifetime: lt,
            generics,
            crate_path,
            target,
            temp: Vec::new(),
        })
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
fn binary_encode_field(
    output: &mut Vec<TokenTree>,
    ctx: &Ctx,
    field: &Field,
    place: &dyn Fn(&mut Vec<TokenTree>),
) {
    {
        tt_punct_alone(output, '<');
        output.extend_from_slice(field.ty);
        tt_ident(output, "as");
        output.extend_from_slice(&ctx.crate_path);
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
        tt_ident(output, "ToBinary");
        tt_punct_alone(output, '>');
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
        tt_ident(output, "binary_encode");
        {
            let at = output.len();
            {
                place(output)
            };
            tt_punct_alone(output, ',');
            tt_ident(output, "encoder");
            tt_group(output, Delimiter::Parenthesis, at);
        };
        tt_punct_alone(output, ';');
    };
}
fn binary_decode_field(out: &mut Vec<TokenTree>, ctx: &Ctx, field: &Field) {
    {
        tt_punct_alone(out, '<');
        out.extend_from_slice(field.ty);
        tt_ident(out, "as");
        {
            ctx.FromBinary(out)
        };
        tt_punct_alone(out, '>');
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "binary_decode");
        {
            let at = out.len();
            tt_ident(out, "decoder");
            tt_group(out, Delimiter::Parenthesis, at);
        };
    };
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromJson(&self, out: &mut Vec<TokenTree>) {
        {
            out.extend_from_slice(&self.crate_path);
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "FromJson");
            tt_punct_alone(out, '<');
            tt_punct_joint(out, '\'');
            out.push(TokenTree::from(self.lifetime.clone()));
            tt_punct_alone(out, '>');
        }
    }
    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut Vec<TokenTree>) {
        {
            out.extend_from_slice(&self.crate_path);
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "FromBinary");
            tt_punct_alone(out, '<');
            tt_punct_joint(out, '\'');
            out.push(TokenTree::from(self.lifetime.clone()));
            tt_punct_alone(out, '>');
        }
    }
}
fn schema_field_decode(out: &mut Vec<TokenTree>, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    {
        out.extend_from_slice(&ctx.crate_path);
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "__internal");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "erase");
        {
            let at = out.len();
            tt_punct_alone(out, '<');
            out.extend_from_slice(field.ty);
            tt_ident(out, "as");
            {
                ctx.FromJson(out)
            };
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "emplace_from_json");
            tt_group(out, Delimiter::Parenthesis, at);
        };
    }
    Ok(())
}
fn field_name_literal(ctx: &Ctx, field: &Field) -> Literal {
    if let Some(name) = &field.attr.rename {
        return name.clone();
    }
    if ctx.target.rename_all != RenameRule::None {
        Literal::string(
            &ctx.target
                .rename_all
                .apply_to_field(&field.name.to_string()),
        )
    } else {
        Literal::string(&field.name.to_string())
    }
}
fn variant_name_json(ctx: &Ctx, field: &EnumVariant, output: &mut String) -> Result<(), Error> {
    output.push('"');
    if let Some(name) = &field.attr.rename {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                crate::template::raw_escape(&value, output);
            }
            _ => {
                return Err(Error::span_msg(
                    "Invalid rename value expected a string",
                    name.span(),
                ))
            }
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
    Ok(())
}
fn field_name_json(ctx: &Ctx, field: &Field, output: &mut String) -> Result<(), Error> {
    output.push('"');
    if let Some(name) = &field.attr.rename {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                crate::template::raw_escape(&value, output);
            }
            _ => {
                return Err(Error::span_msg(
                    "Invalid rename value expected a string",
                    name.span(),
                ))
            }
        }
    } else if ctx.target.rename_all != RenameRule::None {
        output.push_str(
            &ctx.target
                .rename_all
                .apply_to_field(&field.name.to_string()),
        );
    } else {
        output.push_str(&field.name.to_string());
    }
    output.push('"');
    Ok(())
}
fn struct_schema(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    fields: &[&Field],
    temp_tuple: Option<&Ident>,
) -> Result<(), Error> {
    let ag_gen = if ctx.target.generics.iter().any(|g| !match g.kind {
        GenericKind::Lifetime => true,
        _ => false,
    }) {
        let x = out.len();
        tt_punct_alone(out, '<');
        fmt_generics(
            out,
            &ctx.target.generics,
            GenericBoundFormating {
                lifetimes: false,
                bounds: false,
            },
        );
        tt_punct_alone(out, '>');
        TokenTree::Group(Group::new(Delimiter::None, out.drain(x..).collect()))
    } else {
        TokenTree::Group(Group::new(Delimiter::None, TokenStream::new()))
    };
    let ts = {
        let len = out.len();
        for (i, field) in fields.iter().enumerate() {
            out.extend_from_slice(&ctx.crate_path);
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "__internal");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "Field");
            {
                let at = out.len();
                tt_ident(out, "name");
                tt_punct_alone(out, ':');
                out.push(field_name_literal(ctx, field).into());
                tt_punct_alone(out, ',');
                tt_ident(out, "offset");
                tt_punct_alone(out, ':');
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "mem");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "offset_of");
                tt_punct_alone(out, '!');
                {
                    let at = out.len();
                    {
                        if let Some(ty) = temp_tuple {
                            {
                                out.push(TokenTree::from(ty.clone()));
                                tt_punct_alone(out, ',');
                                out.push(Literal::usize_unsuffixed(i).into());
                            }
                        } else {
                            {
                                out.push(TokenTree::from(ctx.target.name.clone()));
                                out.push(TokenTree::from(ag_gen.clone()));
                                tt_punct_alone(out, ',');
                                out.push(TokenTree::from(field.name.clone()));
                            }
                        }
                    };
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "decode");
                tt_punct_alone(out, ':');
                if let Err(err) = schema_field_decode(out, ctx, field) {
                    return Err(err);
                };
                tt_group(out, Delimiter::Brace, at);
            };
            tt_punct_alone(out, ',');
        }
        TokenStream::from_iter(out.drain(len..))
    };
    let schema_fields = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.len();
        {
            for field in fields {
                {
                    tt_ident(out, "std");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "mem");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "transmute");
                    {
                        let at = out.len();
                        tt_ident(out, "std");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "ptr");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "drop_in_place");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_punct_alone(out, '<');
                        out.extend_from_slice(field.ty);
                        tt_punct_alone(out, '>');
                        tt_ident(out, "as");
                        tt_ident(out, "unsafe");
                        tt_ident(out, "fn");
                        {
                            let at = out.len();
                            tt_punct_alone(out, '*');
                            tt_ident(out, "mut");
                            out.extend_from_slice(field.ty);
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ',');
                }
            }
        };
        TokenStream::from_iter(out.drain(len..))
    };
    let schema_drops = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.len();
        {
            for field in fields {
                let Some(default) = &field.attr.default else {
                    break;
                };
                {
                    tt_punct_alone(out, '|');
                    tt_ident(out, "ptr");
                    tt_punct_alone(out, ':');
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "std");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "ptr");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "NonNull");
                    tt_punct_alone(out, '<');
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, '>');
                    tt_punct_alone(out, '|');
                    {
                        let at = out.len();
                        tt_ident(out, "let");
                        tt_ident(out, "value");
                        tt_punct_alone(out, ':');
                        out.extend_from_slice(field.ty);
                        tt_punct_alone(out, '=');
                        out.extend_from_slice(default);
                        tt_punct_alone(out, ';');
                        tt_ident(out, "unsafe");
                        {
                            let at = out.len();
                            tt_ident(out, "ptr");
                            tt_punct_alone(out, '.');
                            tt_ident(out, "cast");
                            tt_group_empty(out, Delimiter::Parenthesis);
                            tt_punct_alone(out, '.');
                            tt_ident(out, "write");
                            {
                                let at = out.len();
                                tt_ident(out, "value");
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_alone(out, ';');
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "jsony");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "__internal");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "UnsafeReturn");
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ',');
                }
            }
        };
        TokenStream::from_iter(out.drain(len..))
    };
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    {
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "jsony");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "__internal");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "ObjectSchemaInner");
        {
            let at = out.len();
            tt_ident(out, "fields");
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '&');
            out.push(schema_fields);
            tt_punct_alone(out, ',');
            tt_ident(out, "drops");
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '&');
            out.push(schema_drops);
            tt_punct_alone(out, ',');
            tt_ident(out, "defaults");
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '&');
            out.push(schema_defaults);
            tt_punct_alone(out, ',');
            tt_group(out, Delimiter::Brace, at);
        };
    }
    Ok(())
}
fn body_of_struct_from_json_with_flatten(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    flatten_field: &Field,
) -> TokenStream {
    {
        let len = out.len();
        tt_ident(out, "let");
        tt_ident(out, "mut");
        tt_ident(out, "flatten_visitor");
        tt_punct_alone(out, '=');
        tt_punct_alone(out, '<');
        out.extend_from_slice(flatten_field.ty);
        tt_ident(out, "as");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "jsony");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "json");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "FromJsonFieldVisitor");
        tt_punct_alone(out, '>');
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "new_field_visitor");
        {
            let at = out.len();
            tt_ident(out, "dst");
            tt_punct_alone(out, '.');
            tt_ident(out, "byte_add");
            {
                let at = out.len();
                tt_ident(out, "offset_of");
                tt_punct_alone(out, '!');
                {
                    let at = out.len();
                    {
                        ctx.dead_target_type(out)
                    };
                    tt_punct_alone(out, ',');
                    out.push(TokenTree::from(flatten_field.name.clone()));
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ',');
            tt_ident(out, "parser");
            tt_punct_alone(out, ',');
            tt_group(out, Delimiter::Parenthesis, at);
        };
        tt_punct_alone(out, ';');
        tt_ident(out, "__schema_inner");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_punct_alone(out, '<');
        tt_punct_joint(out, '\'');
        out.push(TokenTree::from(ctx.lifetime.clone()));
        if !ctx.generics.is_empty() {
            tt_punct_alone(out, ',');
            {
                fmt_generics(out, ctx.generics, USE)
            };
        };
        tt_punct_alone(out, '>');
        tt_group_empty(out, Delimiter::Parenthesis);
        tt_punct_alone(out, '.');
        tt_ident(out, "decode");
        {
            let at = out.len();
            tt_ident(out, "dst");
            tt_punct_alone(out, ',');
            tt_ident(out, "parser");
            tt_punct_alone(out, ',');
            tt_ident(out, "Some");
            {
                let at = out.len();
                tt_punct_alone(out, '&');
                tt_ident(out, "mut");
                tt_ident(out, "flatten_visitor");
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_group(out, Delimiter::Parenthesis, at);
        };
        TokenStream::from_iter(out.drain(len..))
    }
}
fn schema_ordered_fields<'a>(fields: &'a [Field<'a>]) -> Vec<&'a Field<'a>> {
    let mut buf = Vec::with_capacity(fields.len());
    for field in fields {
        if field.flags & (Field::WITH_DEFAULT | Field::WITH_FLATTEN) == Field::WITH_DEFAULT {
            buf.push(field);
        }
    }
    for field in fields {
        if field.flags & (Field::WITH_DEFAULT | Field::WITH_FLATTEN) == 0 {
            buf.push(field);
        }
    }
    buf
}
fn required_bitset(ordered: &[&Field]) -> u64 {
    let mut defaults = 0;
    for field in ordered {
        if field.flags & Field::WITH_DEFAULT != 0 {
            defaults += 1;
            continue;
        }
        break;
    }
    ((1 << ordered.len()) - 1) ^ ((1 << defaults) - 1)
}
fn struct_to_json(out: &mut Vec<TokenTree>, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut text = String::new();
    let mut first = true;
    let body = {
        let len = out.len();
        tt_ident(out, "out");
        tt_punct_alone(out, '.');
        tt_ident(out, "start_json_object");
        tt_group_empty(out, Delimiter::Parenthesis);
        tt_punct_alone(out, ';');
        for field in fields {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "push_str");
            {
                let at = out.len();
                {
                    text.clear();
                    if first {
                        first = false;
                    } else {
                        text.push(',');
                    }
                    if let Err(err) = field_name_json(ctx, field, &mut text) {
                        return Err(err);
                    }
                    text.push(':');
                    out.push(TokenTree::Literal(Literal::string(&text)));
                };
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_punct_alone(out, '<');
            out.extend_from_slice(field.ty);
            tt_ident(out, "as");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "ToJson");
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsonify_into");
            {
                let at = out.len();
                tt_punct_alone(out, '&');
                tt_ident(out, "self");
                tt_punct_alone(out, '.');
                out.push(TokenTree::from(field.name.clone()));
                tt_punct_alone(out, ',');
                tt_ident(out, "out");
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
        }
        tt_ident(out, "out");
        tt_punct_alone(out, '.');
        tt_ident(out, "end_json_object");
        tt_group_empty(out, Delimiter::Parenthesis);
        TokenStream::from_iter(out.drain(len..))
    };
    impl_to_json(out, "ObjectValue", ctx, body)
}
fn struct_from_json(out: &mut Vec<TokenTree>, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut flattening: Option<&Field> = None;
    for field in fields {
        if field.attr.flatten {
            if flattening.is_some() {
                return Err(Error::span_msg(
                    "Only one flatten field is currently supproted",
                    field.name.span(),
                ));
            }
            flattening = Some(field);
        }
    }
    let ordered_fields = schema_ordered_fields(fields);
    {
        tt_ident(out, "const");
        tt_ident(out, "_");
        tt_punct_alone(out, ':');
        tt_group_empty(out, Delimiter::Parenthesis);
        tt_punct_alone(out, '=');
        {
            let at = out.len();
            tt_ident(out, "const");
            tt_ident(out, "unsafe");
            tt_ident(out, "fn");
            tt_ident(out, "__schema_inner");
            tt_punct_alone(out, '<');
            tt_punct_joint(out, '\'');
            out.push(TokenTree::from(ctx.lifetime.clone()));
            if !ctx.generics.is_empty() {
                tt_punct_alone(out, ',');
                {
                    fmt_generics(out, ctx.generics, DEF)
                };
            };
            tt_punct_alone(out, '>');
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_joint(out, '-');
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "__internal");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "ObjectSchema");
            tt_punct_alone(out, '<');
            tt_punct_joint(out, '\'');
            out.push(TokenTree::from(ctx.lifetime.clone()));
            tt_punct_alone(out, '>');
            if !ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty() {
                tt_ident(out, "where");
                for ty in &ctx.target.generic_field_types {
                    out.extend_from_slice(ty);
                    tt_punct_alone(out, ':');
                    tt_ident(out, "FromJson");
                    tt_punct_alone(out, '<');
                    tt_punct_joint(out, '\'');
                    out.push(TokenTree::from(ctx.lifetime.clone()));
                    tt_punct_alone(out, '>');
                }
            };
            {
                let at = out.len();
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "jsony");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "__internal");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "ObjectSchema");
                {
                    let at = out.len();
                    tt_ident(out, "inner");
                    tt_punct_alone(out, ':');
                    tt_punct_alone(out, '&');
                    tt_ident(out, "const");
                    {
                        let at = out.len();
                        {
                            struct_schema(out, ctx, &ordered_fields, None)?
                        };
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ',');
                    tt_ident(out, "phantom");
                    tt_punct_alone(out, ':');
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "std");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "marker");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "PhantomData");
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_group(out, Delimiter::Brace, at);
            };
            {
                let ts = if let Some(flatten_field) = flattening {
                    body_of_struct_from_json_with_flatten(out, ctx, flatten_field)
                } else {
                    {
                        let len = out.len();
                        tt_ident(out, "__schema_inner");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_punct_alone(out, '<');
                        tt_punct_joint(out, '\'');
                        out.push(TokenTree::from(ctx.lifetime.clone()));
                        if !ctx.generics.is_empty() {
                            tt_punct_alone(out, ',');
                            {
                                fmt_generics(out, ctx.generics, USE)
                            };
                        };
                        tt_punct_alone(out, '>');
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, '.');
                        tt_ident(out, "decode");
                        {
                            let at = out.len();
                            tt_ident(out, "dst");
                            tt_punct_alone(out, ',');
                            tt_ident(out, "parser");
                            tt_punct_alone(out, ',');
                            tt_ident(out, "None");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        TokenStream::from_iter(out.drain(len..))
                    }
                };
                impl_from_json(out, ctx, ts)?;
                if ctx.target.flattenable {
                    let body = {
                        let len = out.len();
                        tt_ident(out, "jsony");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "__internal");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "DynamicFieldDecoder");
                        {
                            let at = out.len();
                            tt_ident(out, "destination");
                            tt_punct_alone(out, ':');
                            tt_ident(out, "dst");
                            tt_punct_alone(out, ',');
                            tt_ident(out, "schema");
                            tt_punct_alone(out, ':');
                            tt_ident(out, "__schema_inner");
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            tt_punct_alone(out, '<');
                            tt_punct_joint(out, '\'');
                            out.push(TokenTree::from(ctx.lifetime.clone()));
                            if !ctx.generics.is_empty() {
                                tt_punct_alone(out, ',');
                                {
                                    fmt_generics(out, ctx.generics, USE)
                                };
                            };
                            tt_punct_alone(out, '>');
                            tt_group_empty(out, Delimiter::Parenthesis);
                            tt_punct_alone(out, ',');
                            tt_ident(out, "bitset");
                            tt_punct_alone(out, ':');
                            out.push(TokenTree::from(Literal::u64_unsuffixed(0).clone()));
                            tt_punct_alone(out, ',');
                            tt_ident(out, "required");
                            tt_punct_alone(out, ':');
                            out.push(TokenTree::from(
                                Literal::u64_unsuffixed(required_bitset(&ordered_fields)).clone(),
                            ));
                            tt_punct_alone(out, ',');
                            tt_group(out, Delimiter::Brace, at);
                        };
                        TokenStream::from_iter(out.drain(len..))
                    };
                    impl_from_json_field_visitor(
                        out,
                        ctx,
                        &(|out| {
                            tt_ident(out, "jsony");
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            tt_ident(out, "__internal");
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            tt_ident(out, "DynamicFieldDecoder");
                            tt_punct_alone(out, '<');
                            tt_punct_joint(out, '\'');
                            out.push(TokenTree::from(ctx.lifetime.clone()));
                            tt_punct_alone(out, '>');
                        }),
                        body,
                    )?;
                }
            };
            tt_group(out, Delimiter::Brace, at);
        };
        tt_punct_alone(out, ';');
    };
    Ok(())
}
fn variant_key_literal(_ctx: &Ctx, variant: &EnumVariant) -> Literal {
    let buf = variant.name.to_string();
    Literal::string(&buf)
}
fn enum_to_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let mut text = String::with_capacity(64);
    let start = out.len();
    let mut all_objects = true;
    if let Tag::Inline(tag_name) = &ctx.target.tag {
        {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "start_json_object");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
        };
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "push_str");
            {
                let at = out.len();
                out.push(Literal::string(&text).into());
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
        };
    } else if all_objects {
        {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "start_json_object");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
        };
    }
    for variant in variants {
        if let EnumKind::Struct = variant.kind {
            continue;
        }
        all_objects = false;
        break;
    }
    {
        tt_ident(out, "match");
        tt_ident(out, "self");
        {
            let at = out.len();
            {
                for (_, variant) in variants.iter().enumerate() {
                    {
                        out.push(TokenTree::from(ctx.target.name.clone()));
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        out.push(TokenTree::from(variant.name.clone()));
                    }
                    match variant.kind {
                        EnumKind::Tuple => {
                            {
                                let at = out.len();
                                {
                                    for (i, _) in variant.fields.iter().enumerate() {
                                        {
                                            out.push(TokenTree::from(ctx.temp[i].clone()));
                                            tt_punct_alone(out, ',');
                                        }
                                    }
                                };
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::Struct => {
                            {
                                let at = out.len();
                                {
                                    for field in variant.fields {
                                        {
                                            out.push(TokenTree::from(field.name.clone()));
                                            tt_punct_alone(out, ',');
                                        }
                                    }
                                };
                                tt_group(out, Delimiter::Brace, at);
                            };
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::None => {
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                    }
                }
            };
            tt_group(out, Delimiter::Brace, at);
        };
    };
    if let Tag::Inline(..) = &ctx.target.tag {
        {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "end_json_object");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
        };
    } else if all_objects {
        {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "end_json_object");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
        };
    }
    {
        tt_ident(out, "Self");
        tt_punct_joint(out, ':');
        tt_punct_alone(out, ':');
        tt_ident(out, "Kind");
        tt_group_empty(out, Delimiter::Brace);
    };
    let stream = out.drain(start..).collect();
    let kind = if all_objects {
        "ObjectValue"
    } else {
        "StringValue"
    };
    impl_to_json(out, kind, ctx, stream)
}
fn enum_variant_to_json_struct(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
) -> Result<(), Error> {
    let mut flattening: Option<&Field> = None;
    for field in variant.fields {
        if field.attr.flatten {
            if flattening.is_some() {
                return Err(Error::span_msg(
                    "Only one flatten field is currently supproted",
                    field.name.span(),
                ));
            }
            flattening = Some(field);
        }
    }
    if flattening.is_some() {
        return Err(Error::msg("Flattening in ToJson not implemented yet"));
    }
    let mut first = true;
    {
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    tt_ident(out, "out");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "start_json_object");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, ';');
                }
                Tag::Inline(..) if ctx.target.content.is_some() => text.push('{'),
                Tag::Default => text.push('{'),
                _ => (),
            }
        };
        for field in variant.fields {
            tt_ident(out, "out");
            tt_punct_alone(out, '.');
            tt_ident(out, "push_str");
            {
                let at = out.len();
                {
                    if first {
                        first = false;
                    } else {
                        text.push(',');
                    }
                    if let Err(err) = field_name_json(ctx, field, text) {
                        return Err(err);
                    }
                    text.push(':');
                    out.push(TokenTree::Literal(Literal::string(&text)));
                    text.clear();
                };
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_punct_alone(out, '<');
            out.extend_from_slice(field.ty);
            tt_ident(out, "as");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "ToJson");
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsonify_into");
            {
                let at = out.len();
                out.push(TokenTree::from(field.name.clone()));
                tt_punct_alone(out, ',');
                tt_ident(out, "out");
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
        }
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    tt_ident(out, "out");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "end_json_object");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, ';');
                }
                Tag::Default => {
                    text.push('}');
                    {
                        tt_ident(out, "out");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "push_str");
                        {
                            let at = out.len();
                            out.push(Literal::string(&text).into());
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                    };
                }
                Tag::Inline(..) if ctx.target.content.is_some() => {
                    text.push('}');
                    {
                        tt_ident(out, "out");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "push_str");
                        {
                            let at = out.len();
                            out.push(Literal::string(&text).into());
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                    };
                }
                _ => (),
            }
        };
    };
    Ok(())
}
fn enum_variant_to_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
    all_objects: bool,
) -> Result<(), Error> {
    let start = out.len();
    match &ctx.target.tag {
        Tag::Inline(..) => {
            variant_name_json(ctx, variant, text)?;
            if let Some(content) = &ctx.target.content {
                text.push_str(",\"");
                crate::template::raw_escape(&content, text);
                text.push_str("\":");
            } else {
                text.push_str(",");
            }
        }
        Tag::Untagged => (),
        Tag::Default => {
            if let EnumKind::None = variant.kind {
            } else {
                if !all_objects {
                    {
                        tt_ident(out, "out");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "start_json_object");
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, ';');
                    };
                }
                variant_name_json(ctx, variant, text)?;
                text.push_str(":");
            }
        }
    }
    match variant.kind {
        EnumKind::Tuple => {
            let [field] = variant.fields else {
                return Err(Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                ));
            };
            {
                tt_punct_alone(out, '<');
                out.extend_from_slice(field.ty);
                tt_ident(out, "as");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "jsony");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "ToJson");
                tt_punct_alone(out, '>');
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "jsonify_into");
                {
                    let at = out.len();
                    out.push(TokenTree::from(ctx.temp[0].clone()));
                    tt_punct_alone(out, ',');
                    tt_ident(out, "out");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
            }
        }
        EnumKind::Struct => {
            if let Err(err) = enum_variant_to_json_struct(out, ctx, variant, text) {
                return Err(err);
            }
        }
        EnumKind::None => {
            text.push('"');
            variant_name_json(ctx, variant, text)?;
            text.push('"');
            {
                tt_ident(out, "out");
                tt_punct_alone(out, '.');
                tt_ident(out, "push_str");
                {
                    let at = out.len();
                    out.push(Literal::string(&text).into());
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
            };
            text.clear();
        }
    };
    if !all_objects {
        match &ctx.target.tag {
            Tag::Default => {
                if let EnumKind::None = variant.kind {
                } else {
                    {
                        tt_ident(out, "out");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "end_json_object");
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, ';');
                    };
                }
            }
            _ => (),
        }
    }
    let ts = out.drain(start..).collect();
    out.push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
    Ok(())
}
fn enum_variant_from_json_struct(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) -> Result<(), Error> {
    let ordered_fields = schema_ordered_fields(variant.fields);
    let mut flattening: Option<&Field> = None;
    for field in variant.fields {
        if field.attr.flatten {
            if flattening.is_some() {
                return Err(Error::span_msg(
                    "Only one flatten field is currently supproted",
                    field.name.span(),
                ));
            }
            flattening = Some(field);
        }
    }
    if let Some(flatten_field) = flattening {
        {
            tt_ident(out, "type");
            tt_ident(out, "__TEMP");
            if ctx.target.has_lifetime() {
                tt_punct_alone(out, '<');
                tt_punct_joint(out, '\'');
                out.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(out, '>');
            };
            tt_punct_alone(out, '=');
            {
                let at = out.len();
                for field in &ordered_fields {
                    out.extend_from_slice(field.ty);
                    tt_punct_alone(out, ',');
                }
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "schema");
            tt_punct_alone(out, '=');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "__internal");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "ObjectSchema");
            {
                let at = out.len();
                tt_ident(out, "inner");
                tt_punct_alone(out, ':');
                tt_ident(out, "const");
                {
                    let at = out.len();
                    tt_punct_alone(out, '&');
                    if let Err(err) = struct_schema(
                        out,
                        ctx,
                        &ordered_fields,
                        Some(&Ident::new("__TEMP", Span::call_site())),
                    ) {
                        return Err(err);
                    };
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "phantom");
                tt_punct_alone(out, ':');
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "marker");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "PhantomData");
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Brace, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "mut");
            tt_ident(out, "temp");
            tt_punct_alone(out, '=');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "std");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "mem");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "MaybeUninit");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '<');
            tt_ident(out, "__TEMP");
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "uninit");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "mut");
            tt_ident(out, "temp_flatten");
            tt_punct_alone(out, '=');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "std");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "mem");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "MaybeUninit");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '<');
            out.extend_from_slice(flatten_field.ty);
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "uninit");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "mut");
            tt_ident(out, "flatten_visitor");
            tt_punct_alone(out, '=');
            tt_punct_alone(out, '<');
            out.extend_from_slice(flatten_field.ty);
            tt_ident(out, "as");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "json");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "FromJsonFieldVisitor");
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "new_field_visitor");
            {
                let at = out.len();
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "ptr");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "NonNull");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "new_unchecked");
                {
                    let at = out.len();
                    tt_ident(out, "temp_flatten");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "as_mut_ptr");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, '.');
                    tt_ident(out, "cast");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "parser");
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "if");
            tt_ident(out, "let");
            tt_ident(out, "Err");
            {
                let at = out.len();
                tt_ident(out, "_err");
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, '=');
            tt_ident(out, "schema");
            tt_punct_alone(out, '.');
            tt_ident(out, "decode");
            {
                let at = out.len();
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "ptr");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "NonNull");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "new_unchecked");
                {
                    let at = out.len();
                    tt_ident(out, "temp");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "as_mut_ptr");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, '.');
                    tt_ident(out, "cast");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "parser");
                tt_punct_alone(out, ',');
                tt_ident(out, "Some");
                {
                    let at = out.len();
                    tt_punct_alone(out, '&');
                    tt_ident(out, "mut");
                    tt_ident(out, "flatten_visitor");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Parenthesis, at);
            };
            {
                let at = out.len();
                if !untagged {
                    tt_ident(out, "return");
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_ident(out, "_err");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ';');
                };
                tt_group(out, Delimiter::Brace, at);
            };
            tt_ident(out, "else");
            {
                let at = out.len();
                tt_ident(out, "let");
                tt_ident(out, "temp2");
                tt_punct_alone(out, '=');
                tt_ident(out, "temp");
                tt_punct_alone(out, '.');
                tt_ident(out, "assume_init");
                tt_group_empty(out, Delimiter::Parenthesis);
                tt_punct_alone(out, ';');
                tt_ident(out, "dst");
                tt_punct_alone(out, '.');
                tt_ident(out, "cast");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_punct_alone(out, '<');
                {
                    ctx.target_type(out)
                };
                tt_punct_alone(out, '>');
                tt_group_empty(out, Delimiter::Parenthesis);
                tt_punct_alone(out, '.');
                tt_ident(out, "write");
                {
                    let at = out.len();
                    out.push(TokenTree::from(ctx.target.name.clone()));
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    out.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.push(TokenTree::from(field.name.clone()));
                            tt_punct_alone(out, ':');
                            tt_ident(out, "temp2");
                            tt_punct_alone(out, '.');
                            out.push(Literal::usize_unsuffixed(i).into());
                            tt_punct_alone(out, ',');
                        }
                        out.push(TokenTree::from(flatten_field.name.clone()));
                        tt_punct_alone(out, ':');
                        tt_ident(out, "temp_flatten");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "assume_init");
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, ',');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
                if untagged {
                    tt_ident(out, "break");
                    tt_punct_joint(out, '\'');
                    tt_ident(out, "success");
                };
                tt_group(out, Delimiter::Brace, at);
            };
        }
    } else {
        {
            tt_ident(out, "type");
            tt_ident(out, "__TEMP");
            if ctx.target.has_lifetime() {
                tt_punct_alone(out, '<');
                tt_punct_joint(out, '\'');
                out.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(out, '>');
            };
            tt_punct_alone(out, '=');
            {
                let at = out.len();
                for field in &ordered_fields {
                    out.extend_from_slice(field.ty);
                    tt_punct_alone(out, ',');
                }
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "schema");
            tt_punct_alone(out, '=');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "jsony");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "__internal");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "ObjectSchema");
            {
                let at = out.len();
                tt_ident(out, "inner");
                tt_punct_alone(out, ':');
                tt_ident(out, "const");
                {
                    let at = out.len();
                    tt_punct_alone(out, '&');
                    if let Err(err) = struct_schema(
                        out,
                        ctx,
                        &ordered_fields,
                        Some(&Ident::new("__TEMP", Span::call_site())),
                    ) {
                        return Err(err);
                    };
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "phantom");
                tt_punct_alone(out, ':');
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "marker");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "PhantomData");
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Brace, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "mut");
            tt_ident(out, "temp");
            tt_punct_alone(out, '=');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "std");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "mem");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "MaybeUninit");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '<');
            tt_ident(out, "__TEMP");
            tt_punct_alone(out, '>');
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_ident(out, "uninit");
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, ';');
            tt_ident(out, "if");
            tt_ident(out, "let");
            tt_ident(out, "Err");
            {
                let at = out.len();
                tt_ident(out, "_err");
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, '=');
            tt_ident(out, "schema");
            tt_punct_alone(out, '.');
            tt_ident(out, "decode");
            {
                let at = out.len();
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "std");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "ptr");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "NonNull");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "new_unchecked");
                {
                    let at = out.len();
                    tt_ident(out, "temp");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "as_mut_ptr");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_punct_alone(out, '.');
                    tt_ident(out, "cast");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "parser");
                tt_punct_alone(out, ',');
                tt_ident(out, "None");
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Parenthesis, at);
            };
            {
                let at = out.len();
                if !untagged {
                    tt_ident(out, "return");
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_ident(out, "_err");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ';');
                };
                tt_group(out, Delimiter::Brace, at);
            };
            tt_ident(out, "else");
            {
                let at = out.len();
                tt_ident(out, "let");
                tt_ident(out, "temp2");
                tt_punct_alone(out, '=');
                tt_ident(out, "temp");
                tt_punct_alone(out, '.');
                tt_ident(out, "assume_init");
                tt_group_empty(out, Delimiter::Parenthesis);
                tt_punct_alone(out, ';');
                tt_ident(out, "dst");
                tt_punct_alone(out, '.');
                tt_ident(out, "cast");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_punct_alone(out, '<');
                {
                    ctx.target_type(out)
                };
                tt_punct_alone(out, '>');
                tt_group_empty(out, Delimiter::Parenthesis);
                tt_punct_alone(out, '.');
                tt_ident(out, "write");
                {
                    let at = out.len();
                    out.push(TokenTree::from(ctx.target.name.clone()));
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    out.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.push(TokenTree::from(field.name.clone()));
                            tt_punct_alone(out, ':');
                            tt_ident(out, "temp2");
                            tt_punct_alone(out, '.');
                            out.push(Literal::usize_unsuffixed(i).into());
                            tt_punct_alone(out, ',');
                        }
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
                if untagged {
                    tt_ident(out, "break");
                    tt_punct_joint(out, '\'');
                    tt_ident(out, "success");
                };
                tt_group(out, Delimiter::Brace, at);
            };
        }
    };
    Ok(())
}
fn enum_variant_from_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) -> Result<(), Error> {
    let start = out.len();
    match variant.kind {
        EnumKind::Tuple => {
            let [field] = variant.fields else {
                return Err(Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                ));
            };
            {
                tt_ident(out, "match");
                tt_punct_alone(out, '<');
                out.extend_from_slice(field.ty);
                tt_ident(out, "as");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "jsony");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "FromJson");
                tt_punct_alone(out, '<');
                tt_punct_joint(out, '\'');
                out.push(TokenTree::from(ctx.lifetime.clone()));
                tt_punct_alone(out, '>');
                tt_punct_alone(out, '>');
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "decode_json");
                {
                    let at = out.len();
                    tt_ident(out, "parser");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                {
                    let at = out.len();
                    tt_ident(out, "Ok");
                    {
                        let at = out.len();
                        tt_ident(out, "value");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_joint(out, '=');
                    tt_punct_alone(out, '>');
                    {
                        let at = out.len();
                        tt_ident(out, "dst");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "cast");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_punct_alone(out, '<');
                        {
                            ctx.target_type(out)
                        };
                        tt_punct_alone(out, '>');
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, '.');
                        tt_ident(out, "write");
                        {
                            let at = out.len();
                            out.push(TokenTree::from(ctx.target.name.clone()));
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            out.push(TokenTree::from(variant.name.clone()));
                            {
                                let at = out.len();
                                tt_ident(out, "value");
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                        if untagged {
                            tt_ident(out, "break");
                            tt_punct_joint(out, '\'');
                            tt_ident(out, "success");
                        };
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ',');
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_ident(out, "_err");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_joint(out, '=');
                    tt_punct_alone(out, '>');
                    {
                        let at = out.len();
                        if !untagged {
                            tt_ident(out, "return");
                            tt_ident(out, "Err");
                            {
                                let at = out.len();
                                tt_ident(out, "_err");
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_alone(out, ';');
                        };
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_group(out, Delimiter::Brace, at);
                };
            }
        }
        EnumKind::Struct => {
            if let Err(err) = enum_variant_from_json_struct(out, ctx, variant, untagged) {
                return Err(err);
            }
        }
        EnumKind::None => {
            tt_ident(out, "dst");
            tt_punct_alone(out, '.');
            tt_ident(out, "cast");
            tt_punct_joint(out, ':');
            tt_punct_alone(out, ':');
            tt_punct_alone(out, '<');
            {
                ctx.target_type(out)
            };
            tt_punct_alone(out, '>');
            tt_group_empty(out, Delimiter::Parenthesis);
            tt_punct_alone(out, '.');
            tt_ident(out, "write");
            {
                let at = out.len();
                out.push(TokenTree::from(ctx.target.name.clone()));
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                out.push(TokenTree::from(variant.name.clone()));
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            if untagged {
                tt_ident(out, "break");
                tt_punct_joint(out, '\'');
                tt_ident(out, "success");
            };
        }
    };
    let ts = TokenStream::from_iter(out.drain(start..));
    out.push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
    Ok(())
}
fn enum_from_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    if ctx.target.flattenable {
        return Err(Error::msg("Flattening enums not supported yet."));
    }
    let mut mixed_strings_and_objects = false;
    let inline_tag = match &ctx.target.tag {
        Tag::Inline(literal) => Some(literal),
        Tag::Untagged => {
            let body = {
                let len = out.len();
                tt_ident(out, "let");
                tt_ident(out, "initial_index");
                tt_punct_alone(out, '=');
                tt_ident(out, "parser");
                tt_punct_alone(out, '.');
                tt_ident(out, "index");
                tt_punct_alone(out, ';');
                tt_punct_joint(out, '\'');
                tt_ident(out, "success");
                tt_punct_alone(out, ':');
                {
                    let at = out.len();
                    for (i, variant) in variants.iter().enumerate() {
                        {
                            let at = out.len();
                            if i != 0 {
                                tt_ident(out, "parser");
                                tt_punct_alone(out, '.');
                                tt_ident(out, "index");
                                tt_punct_alone(out, '=');
                                tt_ident(out, "initial_index");
                                tt_punct_alone(out, ';');
                            };
                            if let Err(err) = enum_variant_from_json(out, ctx, variant, true) {
                                return Err(err);
                            };
                            tt_group(out, Delimiter::Brace, at);
                        };
                    }
                    tt_ident(out, "return");
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_punct_alone(out, '&');
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "jsony");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "json");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "DecodeError");
                        {
                            let at = out.len();
                            tt_ident(out, "message");
                            tt_punct_alone(out, ':');
                            out.push(
                                Literal::string("Untagged enum didn't match any variant").into(),
                            );
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_ident(out, "parser");
                tt_punct_alone(out, '.');
                tt_ident(out, "clear_error");
                tt_group_empty(out, Delimiter::Parenthesis);
                tt_punct_alone(out, ';');
                tt_ident(out, "Ok");
                {
                    let at = out.len();
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                TokenStream::from_iter(out.drain(len..))
            };
            impl_from_json(out, ctx, body)?;
            return Ok(());
        }
        Tag::Default => {
            for variants in variants {
                if let EnumKind::None = variants.kind {
                    mixed_strings_and_objects = true;
                    break;
                }
            }
            None
        }
    };
    let mut body = {
        let len = out.len();
        {
            if let Some(tag) = inline_tag {
                {
                    tt_ident(out, "let");
                    tt_ident(out, "variant");
                    tt_punct_alone(out, '=');
                    tt_ident(out, "match");
                    tt_ident(out, "parser");
                    tt_punct_alone(out, '.');
                    {
                        if let Some(content) = &ctx.target.content {
                            {
                                tt_ident(out, "tag_query_at_content_next_object");
                                {
                                    let at = out.len();
                                    out.push(Literal::string(tag).into());
                                    tt_punct_alone(out, ',');
                                    out.push(Literal::string(content).into());
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                            }
                        } else {
                            {
                                tt_ident(out, "tag_query_next_object");
                                {
                                    let at = out.len();
                                    out.push(Literal::string(tag).into());
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                            }
                        }
                    };
                    {
                        let at = out.len();
                        tt_ident(out, "Ok");
                        {
                            let at = out.len();
                            tt_ident(out, "value");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        tt_ident(out, "value");
                        tt_punct_alone(out, ',');
                        tt_ident(out, "Err");
                        {
                            let at = out.len();
                            tt_ident(out, "err");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        tt_ident(out, "return");
                        tt_ident(out, "Err");
                        {
                            let at = out.len();
                            tt_ident(out, "err");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ',');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ';');
                }
            } else {
                {
                    tt_ident(out, "let");
                    tt_ident(out, "Ok");
                    {
                        let at = out.len();
                        tt_ident(out, "Some");
                        {
                            let at = out.len();
                            tt_ident(out, "variant");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, '=');
                    tt_ident(out, "parser");
                    tt_punct_alone(out, '.');
                    {
                        if mixed_strings_and_objects {
                            {
                                tt_ident(out, "enter_seen_object");
                            }
                        } else {
                            {
                                tt_ident(out, "enter_object");
                            }
                        }
                    };
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_ident(out, "else");
                    {
                        let at = out.len();
                        tt_ident(out, "return");
                        tt_ident(out, "Err");
                        {
                            let at = out.len();
                            tt_punct_alone(out, '&');
                            tt_ident(out, "jsony");
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            tt_ident(out, "json");
                            tt_punct_joint(out, ':');
                            tt_punct_alone(out, ':');
                            tt_ident(out, "DecodeError");
                            {
                                let at = out.len();
                                tt_ident(out, "message");
                                tt_punct_alone(out, ':');
                                out.push(
                                    Literal::string("Expected single field object for enum").into(),
                                );
                                tt_group(out, Delimiter::Brace, at);
                            };
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ';');
                }
            }
        };
        tt_ident(out, "match");
        tt_ident(out, "variant");
        {
            let at = out.len();
            {
                for variant in variants {
                    if mixed_strings_and_objects {
                        if let EnumKind::None = variant.kind {
                            continue;
                        }
                    }
                    {
                        out.push(variant_key_literal(ctx, variant).into());
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        if let Err(err) = enum_variant_from_json(out, ctx, variant, false) {
                            return Err(err);
                        };
                        tt_punct_alone(out, ',');
                    };
                }
            };
            tt_ident(out, "_");
            tt_punct_joint(out, '=');
            tt_punct_alone(out, '>');
            {
                let at = out.len();
                tt_ident(out, "parser");
                tt_punct_alone(out, '.');
                tt_ident(out, "report_error");
                {
                    let at = out.len();
                    tt_ident(out, "variant");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "to_string");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
                tt_ident(out, "return");
                tt_ident(out, "Err");
                {
                    let at = out.len();
                    tt_punct_alone(out, '&');
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "jsony");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "parser");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "UNKNOWN_VARIANT");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
                tt_group(out, Delimiter::Brace, at);
            };
            tt_group(out, Delimiter::Brace, at);
        };
        {
            if let Some(_) = inline_tag {
                if ctx.target.content.is_some() {
                    {
                        tt_ident(out, "parser");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "discard_remaining_object_fields");
                        tt_group_empty(out, Delimiter::Parenthesis);
                    }
                } else {
                    {
                        tt_ident(out, "Ok");
                        {
                            let at = out.len();
                            tt_group_empty(out, Delimiter::Parenthesis);
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                    }
                }
            } else {
                {
                    tt_ident(out, "let");
                    tt_ident(out, "err");
                    tt_punct_alone(out, '=');
                    tt_ident(out, "match");
                    tt_ident(out, "parser");
                    tt_punct_alone(out, '.');
                    tt_ident(out, "object_step");
                    tt_group_empty(out, Delimiter::Parenthesis);
                    {
                        let at = out.len();
                        tt_ident(out, "Ok");
                        {
                            let at = out.len();
                            tt_ident(out, "None");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        {
                            let at = out.len();
                            tt_ident(out, "return");
                            tt_ident(out, "Ok");
                            {
                                let at = out.len();
                                tt_group_empty(out, Delimiter::Parenthesis);
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_punct_alone(out, ',');
                        tt_ident(out, "Err");
                        {
                            let at = out.len();
                            tt_ident(out, "err");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        {
                            let at = out.len();
                            tt_ident(out, "Err");
                            {
                                let at = out.len();
                                tt_ident(out, "err");
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_punct_alone(out, ',');
                        tt_ident(out, "Ok");
                        {
                            let at = out.len();
                            tt_ident(out, "Some");
                            {
                                let at = out.len();
                                tt_ident(out, "_");
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        {
                            let at = out.len();
                            tt_ident(out, "Err");
                            {
                                let at = out.len();
                                tt_punct_alone(out, '&');
                                tt_ident(out, "jsony");
                                tt_punct_joint(out, ':');
                                tt_punct_alone(out, ':');
                                tt_ident(out, "json");
                                tt_punct_joint(out, ':');
                                tt_punct_alone(out, ':');
                                tt_ident(out, "DecodeError");
                                {
                                    let at = out.len();
                                    tt_ident(out, "message");
                                    tt_punct_alone(out, ':');
                                    out.push(
                                        Literal::string("More the one field in enum tab object")
                                            .into(),
                                    );
                                    tt_group(out, Delimiter::Brace, at);
                                };
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_punct_alone(out, ',');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_punct_alone(out, ';');
                    tt_ident(out, "std");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "ptr");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "drop_in_place");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_punct_alone(out, '<');
                    {
                        ctx.target_type(out)
                    };
                    tt_punct_alone(out, '>');
                    {
                        let at = out.len();
                        tt_ident(out, "dst");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "cast");
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, '.');
                        tt_ident(out, "as_mut");
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ';');
                    tt_ident(out, "return");
                    tt_ident(out, "err");
                    tt_punct_alone(out, ';');
                }
            }
        };
        TokenStream::from_iter(out.drain(len..))
    };
    if mixed_strings_and_objects {
        body = {
            let len = out.len();
            tt_ident(out, "match");
            tt_ident(out, "parser");
            tt_punct_alone(out, '.');
            tt_ident(out, "peek");
            tt_group_empty(out, Delimiter::Parenthesis);
            {
                let at = out.len();
                tt_ident(out, "Ok");
                {
                    let at = out.len();
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "jsony");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "parser");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "Peek");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "Object");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_joint(out, '=');
                tt_punct_alone(out, '>');
                out.push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
                tt_punct_alone(out, ',');
                tt_ident(out, "Ok");
                {
                    let at = out.len();
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "jsony");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "parser");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "Peek");
                    tt_punct_joint(out, ':');
                    tt_punct_alone(out, ':');
                    tt_ident(out, "String");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_joint(out, '=');
                tt_punct_alone(out, '>');
                tt_ident(out, "match");
                tt_ident(out, "parser");
                tt_punct_alone(out, '.');
                tt_ident(out, "read_seen_string_unescaped");
                tt_group_empty(out, Delimiter::Parenthesis);
                {
                    let at = out.len();
                    tt_ident(out, "Ok");
                    {
                        let at = out.len();
                        tt_ident(out, "variant");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_joint(out, '=');
                    tt_punct_alone(out, '>');
                    {
                        let at = out.len();
                        tt_ident(out, "let");
                        tt_ident(out, "value");
                        tt_punct_alone(out, '=');
                        tt_ident(out, "match");
                        tt_ident(out, "variant");
                        {
                            let at = out.len();
                            {
                                for variant in variants {
                                    if let EnumKind::None = variant.kind {
                                        {
                                            out.push(variant_key_literal(ctx, variant).into());
                                            tt_punct_joint(out, '=');
                                            tt_punct_alone(out, '>');
                                            out.push(TokenTree::from(ctx.target.name.clone()));
                                            tt_punct_joint(out, ':');
                                            tt_punct_alone(out, ':');
                                            out.push(TokenTree::from(variant.name.clone()));
                                            tt_punct_alone(out, ',');
                                        };
                                    }
                                }
                            };
                            tt_ident(out, "_");
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                let at = out.len();
                                tt_ident(out, "parser");
                                tt_punct_alone(out, '.');
                                tt_ident(out, "report_error");
                                {
                                    let at = out.len();
                                    tt_ident(out, "variant");
                                    tt_punct_alone(out, '.');
                                    tt_ident(out, "to_string");
                                    tt_group_empty(out, Delimiter::Parenthesis);
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                                tt_punct_alone(out, ';');
                                tt_ident(out, "return");
                                tt_ident(out, "Err");
                                {
                                    let at = out.len();
                                    tt_punct_alone(out, '&');
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    tt_ident(out, "jsony");
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    tt_ident(out, "parser");
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    tt_ident(out, "UNKNOWN_VARIANT");
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                                tt_punct_alone(out, ';');
                                tt_group(out, Delimiter::Brace, at);
                            };
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_punct_alone(out, ';');
                        tt_ident(out, "dst");
                        tt_punct_alone(out, '.');
                        tt_ident(out, "cast");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_punct_alone(out, '<');
                        {
                            ctx.target_type(out)
                        };
                        tt_punct_alone(out, '>');
                        tt_group_empty(out, Delimiter::Parenthesis);
                        tt_punct_alone(out, '.');
                        tt_ident(out, "write");
                        {
                            let at = out.len();
                            tt_ident(out, "value");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                        tt_ident(out, "return");
                        tt_ident(out, "Ok");
                        {
                            let at = out.len();
                            tt_group_empty(out, Delimiter::Parenthesis);
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_ident(out, "err");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_joint(out, '=');
                    tt_punct_alone(out, '>');
                    tt_ident(out, "return");
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_ident(out, "err");
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ',');
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_punct_alone(out, ',');
                tt_ident(out, "Ok");
                {
                    let at = out.len();
                    tt_ident(out, "_");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_joint(out, '=');
                tt_punct_alone(out, '>');
                {
                    let at = out.len();
                    tt_ident(out, "return");
                    tt_ident(out, "Err");
                    {
                        let at = out.len();
                        tt_punct_alone(out, '&');
                        tt_ident(out, "jsony");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "json");
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        tt_ident(out, "DecodeError");
                        {
                            let at = out.len();
                            tt_ident(out, "message");
                            tt_punct_alone(out, ':');
                            out.push(
                                Literal::string("Expected either an object or a string").into(),
                            );
                            tt_punct_alone(out, ',');
                            tt_group(out, Delimiter::Brace, at);
                        };
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, ';');
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_ident(out, "Err");
                {
                    let at = out.len();
                    tt_ident(out, "err");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_joint(out, '=');
                tt_punct_alone(out, '>');
                tt_ident(out, "return");
                tt_ident(out, "Err");
                {
                    let at = out.len();
                    tt_ident(out, "err");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ',');
                tt_group(out, Delimiter::Brace, at);
            };
            TokenStream::from_iter(out.drain(len..))
        }
    }
    impl_from_json(out, ctx, body)?;
    Ok(())
}
fn handle_struct(target: &DeriveTargetInner, fields: &[Field]) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let ctx = Ctx::new(target)?;
    if target.from_json {
        struct_from_json(&mut output, &ctx, fields)?;
    }
    if target.to_json {
        struct_to_json(&mut output, &ctx, fields)?;
    }
    if target.to_binary {
        let body = {
            let len = output.len();
            {
                for field in fields {
                    binary_encode_field(
                        &mut output,
                        &ctx,
                        field,
                        &(|out| {
                            tt_punct_alone(out, '&');
                            tt_ident(out, "self");
                            tt_punct_alone(out, '.');
                            out.push(TokenTree::from(field.name.clone()));
                        }),
                    )
                }
            };
            TokenStream::from_iter((&mut output).drain(len..))
        };
        impl_to_binary(&mut output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.len();
            output.push(TokenTree::from(target.name.clone()));
            {
                let at = output.len();
                {
                    for field in fields {
                        {
                            output.push(TokenTree::from(field.name.clone()));
                            tt_punct_alone(&mut output, ':');
                            {
                                binary_decode_field(&mut output, &ctx, field)
                            };
                            tt_punct_alone(&mut output, ',');
                        }
                    }
                };
                tt_group(&mut output, Delimiter::Brace, at);
            };
            TokenStream::from_iter((&mut output).drain(len..))
        };
        impl_from_binary(&mut output, &ctx, body)?;
    }
    Ok(output.drain(..).collect())
}
fn handle_tuple_struct(target: &DeriveTargetInner, fields: &[Field]) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let ctx = Ctx::new(target)?;
    if target.to_binary {
        let body = {
            let len = output.len();
            {
                for (i, field) in fields.iter().enumerate() {
                    binary_encode_field(
                        &mut output,
                        &ctx,
                        field,
                        &(|out| {
                            tt_punct_alone(out, '&');
                            tt_ident(out, "self");
                            tt_punct_alone(out, '.');
                            out.push(TokenTree::from(Literal::usize_unsuffixed(i).clone()));
                        }),
                    )
                }
            };
            TokenStream::from_iter((&mut output).drain(len..))
        };
        impl_to_binary(&mut output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.len();
            output.push(TokenTree::from(target.name.clone()));
            {
                let at = output.len();
                {
                    for field in fields {
                        {
                            {
                                binary_decode_field(&mut output, &ctx, field)
                            };
                            tt_punct_alone(&mut output, ',');
                        }
                    }
                };
                tt_group(&mut output, Delimiter::Parenthesis, at);
            };
            TokenStream::from_iter((&mut output).drain(len..))
        };
        impl_from_binary(&mut output, &ctx, body)?;
    }
    Ok(output.drain(..).collect())
}
fn enum_to_binary(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.len();
        tt_ident(out, "match");
        tt_ident(out, "self");
        {
            let at = out.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        out.push(TokenTree::from(ctx.target.name.clone()));
                        tt_punct_joint(out, ':');
                        tt_punct_alone(out, ':');
                        out.push(TokenTree::from(variant.name.clone()));
                    }
                    match variant.kind {
                        EnumKind::Tuple => {
                            {
                                let at = out.len();
                                {
                                    for (i, _) in variant.fields.iter().enumerate() {
                                        {
                                            out.push(TokenTree::from(ctx.temp[i].clone()));
                                            tt_punct_alone(out, ',');
                                        }
                                    }
                                };
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                let at = out.len();
                                tt_ident(out, "encoder");
                                tt_punct_alone(out, '.');
                                tt_ident(out, "push");
                                {
                                    let at = out.len();
                                    out.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                                tt_punct_alone(out, ';');
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        binary_encode_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.push(TokenTree::from(ctx.temp[i].clone()));
                                            }),
                                        )
                                    }
                                };
                                tt_group(out, Delimiter::Brace, at);
                            };
                        }
                        EnumKind::Struct => {
                            {
                                let at = out.len();
                                {
                                    for field in variant.fields {
                                        {
                                            out.push(TokenTree::from(field.name.clone()));
                                            tt_punct_alone(out, ',');
                                        }
                                    }
                                };
                                tt_group(out, Delimiter::Brace, at);
                            };
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                let at = out.len();
                                tt_ident(out, "encoder");
                                tt_punct_alone(out, '.');
                                tt_ident(out, "push");
                                {
                                    let at = out.len();
                                    out.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                                tt_punct_alone(out, ';');
                                {
                                    for field in variant.fields {
                                        binary_encode_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.push(TokenTree::from(field.name.clone()));
                                            }),
                                        )
                                    }
                                };
                                tt_group(out, Delimiter::Brace, at);
                            };
                        }
                        EnumKind::None => {
                            tt_punct_joint(out, '=');
                            tt_punct_alone(out, '>');
                            {
                                let at = out.len();
                                tt_ident(out, "encoder");
                                tt_punct_alone(out, '.');
                                tt_ident(out, "push");
                                {
                                    let at = out.len();
                                    out.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    tt_group(out, Delimiter::Parenthesis, at);
                                };
                                tt_punct_alone(out, ';');
                                tt_group(out, Delimiter::Brace, at);
                            };
                        }
                    }
                }
            };
            tt_group(out, Delimiter::Brace, at);
        };
        TokenStream::from_iter(out.drain(len..))
    };
    impl_to_binary(out, ctx, body)
}
fn enum_from_binary(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.len();
        tt_ident(out, "match");
        tt_ident(out, "decoder");
        tt_punct_alone(out, '.');
        tt_ident(out, "byte");
        tt_group_empty(out, Delimiter::Parenthesis);
        {
            let at = out.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        {
                            if i + 1 == variants.len() {
                                {
                                    tt_ident(out, "_");
                                }
                            } else {
                                {
                                    out.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                }
                            }
                        };
                        tt_punct_joint(out, '=');
                        tt_punct_alone(out, '>');
                        {
                            match variant.kind {
                                EnumKind::Tuple => {
                                    out.push(TokenTree::from(ctx.target.name.clone()));
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    out.push(TokenTree::from(variant.name.clone()));
                                    {
                                        let at = out.len();
                                        {
                                            for field in variant.fields {
                                                {
                                                    {
                                                        binary_decode_field(out, ctx, field)
                                                    };
                                                    tt_punct_alone(out, ',');
                                                }
                                            }
                                        };
                                        tt_group(out, Delimiter::Parenthesis, at);
                                    };
                                }
                                EnumKind::Struct => {
                                    out.push(TokenTree::from(ctx.target.name.clone()));
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    out.push(TokenTree::from(variant.name.clone()));
                                    {
                                        let at = out.len();
                                        {
                                            for field in variant.fields {
                                                {
                                                    out.push(TokenTree::from(field.name.clone()));
                                                    tt_punct_alone(out, ':');
                                                    {
                                                        binary_decode_field(out, ctx, field)
                                                    };
                                                    tt_punct_alone(out, ',');
                                                }
                                            }
                                        };
                                        tt_group(out, Delimiter::Brace, at);
                                    };
                                }
                                EnumKind::None => {
                                    out.push(TokenTree::from(ctx.target.name.clone()));
                                    tt_punct_joint(out, ':');
                                    tt_punct_alone(out, ':');
                                    out.push(TokenTree::from(variant.name.clone()));
                                }
                            }
                        };
                        tt_punct_alone(out, ',');
                    }
                }
            };
            tt_group(out, Delimiter::Brace, at);
        };
        TokenStream::from_iter(out.drain(len..))
    };
    impl_from_binary(out, ctx, body)
}
fn handle_enum(target: &DeriveTargetInner, variants: &[EnumVariant]) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let mut ctx = Ctx::new(target)?;
    let mut max_tuples = 0;
    for var in variants {
        if match var.kind {
            EnumKind::Tuple => true,
            _ => false,
        } {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp = (0..max_tuples).map(var).collect::<Vec<_>>();
    if target.to_json {
        enum_to_json(&mut output, &ctx, variants)?;
    }
    if target.from_json {
        enum_from_json(&mut output, &ctx, variants)?;
    }
    if target.to_binary {
        enum_to_binary(&mut output, &ctx, variants)?;
    }
    if target.from_binary {
        enum_from_binary(&mut output, &ctx, variants)?;
    }
    Ok(output.drain(..).collect())
}
pub fn inner_derive(stream: TokenStream) -> Result<TokenStream, Error> {
    let outer_tokens: Vec<TokenTree> = stream.into_iter().collect();
    let mut target = DeriveTargetInner {
        name: Ident::new("a", Span::call_site()),
        generics: Vec::new(),
        generic_field_types: Vec::new(),
        where_clauses: &[],
        path_override: None,
        from_binary: false,
        from_json: false,
        to_binary: false,
        to_json: false,
        content: None,
        flattenable: false,
        tag: Tag::Default,
        rename_all: crate::case::RenameRule::None,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens)?;
    if !(target.from_binary || target.to_binary || target.to_json || target.from_json) {
        target.from_json = true;
    }
    let field_toks: Vec<TokenTree> = body.into_iter().collect();
    let mut tt_buf = Vec::<TokenTree>::new();
    let mut field_buf = Vec::<Field>::new();
    let mut pool = MemoryPool::<FieldAttr>::new();
    let mut attr_buf = pool.allocator();
    match kind {
        DeriveTargetKind::Struct => {
            match ast::parse_struct_fields(&mut field_buf, &field_toks, &mut attr_buf) {
                Ok(_) => {
                    ast::scan_fields(&mut target, &mut field_buf);
                    handle_struct(&target, &field_buf)
                }
                Err(err) => Err(err),
            }
        }
        DeriveTargetKind::TupleStruct => {
            match ast::parse_tuple_fields(
                &Ident::new("a", Span::call_site()),
                &mut field_buf,
                &field_toks,
                &mut attr_buf,
            ) {
                Ok(_) => {
                    ast::scan_fields(&mut target, &mut field_buf);
                    handle_tuple_struct(&target, &field_buf)
                }
                Err(err) => Err(err),
            }
        }
        DeriveTargetKind::Enum => {
            match ast::parse_enum(
                &mut target,
                &field_toks,
                &mut tt_buf,
                &mut field_buf,
                &mut attr_buf,
            ) {
                Ok(enums) => handle_enum(&target, &enums),
                Err(err) => Err(err),
            }
        }
    }
}
pub fn derive(stream: TokenStream) -> TokenStream {
    match inner_derive(stream) {
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
