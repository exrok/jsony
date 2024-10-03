use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttr, Generic,
    GenericKind,
};
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
fn tt_append(foo: &mut Vec<TokenTree>, chr: &'static [StaticToken]) {
    foo.extend(chr.iter().map(|tok| match tok {
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
        if fmt.bounds {
            if !generic.bounds.is_empty() {
                tt_punct_alone(buffer, ':');
                buffer.extend(generic.bounds.iter().cloned());
            }
        }
    }
}
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
                fmt_generics(output, &generics, DEF)
            };
        };
        tt_punct_alone(output, '>');
        output.extend_from_slice(&crate_path);
        tt_punct_joint(output, ':');
        tt_punct_alone(output, ':');
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
        if let Err(err) =
            bodyless_impl_from(output, Ident::new("FromBinary", Span::call_site()), ctx)
        {
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
fn impl_from_json(output: &mut Vec<TokenTree>, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    {
        tt_ident(output, "unsafe");
        if let Err(err) = bodyless_impl_from(output, Ident::new("FromJson", Span::call_site()), ctx)
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
                {
                    let at = output.len();
                    tt_group(output, Delimiter::Parenthesis, at);
                };
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
            {
                let at = output.len();
                tt_group(output, Delimiter::Parenthesis, at);
            };
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
struct Ctx<'a> {
    lifetime: Ident,
    generics: &'a [Generic<'a>],
    crate_path: Vec<TokenTree>,
    target: &'a DeriveTargetInner<'a>,
    #[allow(dead_code)]
    attrs: &'a [FieldAttr],
    temp: Vec<Ident>,
}
impl<'a> Ctx<'a> {
    pub fn target_type(&self, out: &mut Vec<TokenTree>) {
        {
            out.push(TokenTree::from(self.target.name.clone()));
            if self.target.generics.len() > 0 {
                tt_punct_alone(out, '<');
                {
                    fmt_generics(out, &self.target.generics, USE)
                };
                tt_punct_alone(out, '>');
            };
        }
    }
    fn new(target: &'a DeriveTargetInner, attrs: &'a [FieldAttr]) -> Result<Ctx<'a>, Error> {
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
            attrs,
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
fn struct_schema(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    fields: &[Field],
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
                out.push(Literal::string(&field.name.to_string()).into());
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
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, Default::default()));
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
fn struct_from_json(out: &mut Vec<TokenTree>, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    {
        tt_ident(out, "const");
        tt_ident(out, "_");
        tt_punct_alone(out, ':');
        {
            let at = out.len();
            tt_group(out, Delimiter::Parenthesis, at);
        };
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
                    fmt_generics(out, &ctx.generics, DEF)
                };
            };
            tt_punct_alone(out, '>');
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
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
                            struct_schema(out, ctx, fields, None)?
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
                let ts = {
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
                            fmt_generics(out, &ctx.generics, USE)
                        };
                    };
                    tt_punct_alone(out, '>');
                    {
                        let at = out.len();
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
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
                };
                impl_from_json(out, ctx, ts)?;
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
fn enum_variant_from_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variant: &EnumVariant,
) -> Result<(), Error> {
    let body = match variant.kind {
        EnumKind::Tuple => {
            let [field] = variant.fields else {
                return Err(Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                ));
            };
            {
                let len = out.len();
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
                        {
                            let at = out.len();
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
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
                        tt_ident(out, "return");
                        tt_ident(out, "Err");
                        {
                            let at = out.len();
                            tt_ident(out, "err");
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                        tt_group(out, Delimiter::Brace, at);
                    };
                    tt_group(out, Delimiter::Brace, at);
                };
                TokenStream::from_iter(out.drain(len..))
            }
        }
        EnumKind::Struct => {
            let len = out.len();
            tt_ident(out, "type");
            tt_ident(out, "__TEMP");
            tt_punct_alone(out, '=');
            {
                let at = out.len();
                for field in variant.fields {
                    out.extend_from_slice(field.ty);
                    tt_punct_alone(out, ',');
                }
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "let");
            tt_ident(out, "schema");
            tt_punct_alone(out, '=');
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
                        variant.fields,
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
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_ident(out, "if");
            tt_ident(out, "let");
            tt_ident(out, "Err");
            {
                let at = out.len();
                tt_ident(out, "err");
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
                    {
                        let at = out.len();
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(out, '.');
                    tt_ident(out, "cast");
                    {
                        let at = out.len();
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
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
                tt_ident(out, "return");
                tt_ident(out, "Err");
                {
                    let at = out.len();
                    tt_ident(out, "err");
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_group(out, Delimiter::Brace, at);
            };
            tt_ident(out, "let");
            tt_ident(out, "temp2");
            tt_punct_alone(out, '=');
            tt_ident(out, "temp");
            tt_punct_alone(out, '.');
            tt_ident(out, "assume_init");
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
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
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
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
                    for (i, field) in variant.fields.iter().enumerate() {
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
            TokenStream::from_iter(out.drain(len..))
        }
        EnumKind::None => {
            let len = out.len();
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
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
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
            TokenStream::from_iter(out.drain(len..))
        }
    };
    out.push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
    Ok(())
}
fn enum_from_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.len();
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
        tt_ident(out, "enter_object");
        {
            let at = out.len();
            tt_group(out, Delimiter::Parenthesis, at);
        };
        tt_ident(out, "else");
        {
            let at = out.len();
            tt_ident(out, "return");
            tt_ident(out, "Err");
            {
                let at = out.len();
                tt_punct_alone(out, '&');
                tt_ident(out, "DecodeError");
                {
                    let at = out.len();
                    tt_ident(out, "message");
                    tt_punct_alone(out, ':');
                    out.push(Literal::string("Expected single field object for enum").into());
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, ';');
            tt_group(out, Delimiter::Brace, at);
        };
        tt_punct_alone(out, ';');
        tt_ident(out, "match");
        tt_ident(out, "variant");
        {
            let at = out.len();
            for (_, variant) in variants.iter().enumerate() {
                out.push(variant_key_literal(ctx, variant).into());
                tt_punct_joint(out, '=');
                tt_punct_alone(out, '>');
                if let Err(err) = enum_variant_from_json(out, ctx, variant) {
                    return Err(err);
                };
                tt_punct_alone(out, ',');
            }
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
                    {
                        let at = out.len();
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
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
        tt_ident(out, "let");
        tt_ident(out, "err");
        tt_punct_alone(out, '=');
        tt_ident(out, "match");
        tt_ident(out, "parser");
        tt_punct_alone(out, '.');
        tt_ident(out, "object_step");
        {
            let at = out.len();
            tt_group(out, Delimiter::Parenthesis, at);
        };
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
                    {
                        let at = out.len();
                        tt_group(out, Delimiter::Parenthesis, at);
                    };
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
                    tt_ident(out, "DecodeError");
                    {
                        let at = out.len();
                        tt_ident(out, "message");
                        tt_punct_alone(out, ':');
                        out.push(Literal::string("More the one field in enum tab object").into());
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
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(out, '.');
            tt_ident(out, "as_mut");
            {
                let at = out.len();
                tt_group(out, Delimiter::Parenthesis, at);
            };
            tt_group(out, Delimiter::Parenthesis, at);
        };
        tt_punct_alone(out, ';');
        tt_ident(out, "return");
        tt_ident(out, "err");
        tt_punct_alone(out, ';');
        TokenStream::from_iter(out.drain(len..))
    };
    impl_from_json(out, ctx, body)?;
    Ok(())
}
fn handle_struct(
    target: &DeriveTargetInner,
    fields: &[Field],
    attrs: &[FieldAttr],
) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let ctx = Ctx::new(target, attrs)?;
    if target.from_json {
        struct_from_json(&mut output, &ctx, fields)?;
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
                                binary_decode_field(&mut output, &ctx, &field)
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
fn handle_tuple_struct(
    target: &DeriveTargetInner,
    fields: &[Field],
    attrs: &[FieldAttr],
) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let ctx = Ctx::new(target, attrs)?;
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
                                binary_decode_field(&mut output, &ctx, &field)
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
                                            &ctx,
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
                                            &ctx,
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
    impl_to_binary(out, &ctx, body)
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
        {
            let at = out.len();
            tt_group(out, Delimiter::Parenthesis, at);
        };
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
                                                        binary_decode_field(out, &ctx, field)
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
                                                        binary_decode_field(out, &ctx, field)
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
    impl_from_binary(out, &ctx, body)
}
fn handle_enum(
    target: &DeriveTargetInner,
    variants: &[EnumVariant],
    attrs: &[FieldAttr],
) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let mut ctx = Ctx::new(target, attrs)?;
    let mut max_tuples = 0;
    for var in variants {
        if match var.kind {
            EnumKind::Tuple => true,
            _ => false,
        } {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp = (0..max_tuples).map(|i| var(i)).collect::<Vec<_>>();
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
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens)?;
    if !(target.from_binary || target.to_binary || target.to_json || target.from_json) {
        target.from_json = true;
    }
    let field_toks: Vec<TokenTree> = body.into_iter().collect();
    let mut tt_buf = Vec::<TokenTree>::new();
    let mut field_buf = Vec::<Field>::new();
    let mut attr_buf = Vec::<FieldAttr>::new();
    match kind {
        DeriveTargetKind::Struct => {
            match ast::parse_struct_fields(&mut field_buf, &field_toks, &mut attr_buf) {
                Ok(_) => {
                    ast::scan_fields(&mut target, &mut field_buf);
                    handle_struct(&target, &field_buf, &attr_buf)
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
                    handle_tuple_struct(&target, &field_buf, &attr_buf)
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
                Ok(enums) => handle_enum(&target, &enums, &attr_buf),
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
