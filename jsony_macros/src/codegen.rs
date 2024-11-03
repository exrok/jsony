use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttrs, Generic,
    GenericKind, Tag, Via, FROM_BINARY, FROM_JSON, TO_BINARY, TO_JSON,
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

struct GenericBoundFormating {
    lifetimes: bool,
    bounds: bool,
}
fn fmt_generics(buffer: &mut RustWriter, generics: &[Generic], fmt: GenericBoundFormating) {
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
            buffer.blit_punct(1);
        }
        match generic.kind {
            GenericKind::Lifetime => {
                buffer.blit_punct(6);
            }
            GenericKind::Type => (),
            GenericKind::Const => {
                buffer.blit_ident(126);
            }
        }
        buffer.buf.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            buffer.blit_punct(9);
            buffer.buf.extend(generic.bounds.iter().cloned());
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
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        output.blit(0, 3);
        output.buf.push(TokenTree::from(lifetime.clone()));
        if !generics.is_empty() {
            output.blit_punct(1);
            {
                fmt_generics(output, generics, DEF)
            };
        };
        output.blit_punct(2);
        output.buf.extend_from_slice(&crate_path);
        output.blit(3, 2);
        if let Some(sub) = sub {
            output.buf.push(TokenTree::from(sub.clone()));
            output.blit(3, 2);
        };
        output.buf.push(TokenTree::from(trait_name.clone()));
        output.blit(1, 2);
        output.buf.push(TokenTree::from(lifetime.clone()));
        output.blit(5, 2);
        output.buf.push(TokenTree::from(target.name.clone()));
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, USE)
            };
            output.blit_punct(2);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(116);
            for ty in &target.generic_field_types {
                output.buf.extend_from_slice(ty);
                output.blit_punct(9);
                output.buf.push(TokenTree::from(trait_name.clone()));
                output.blit(1, 2);
                output.buf.push(TokenTree::from(lifetime.clone()));
                output.blit(7, 2);
            }
            output.buf.extend_from_slice(&target.where_clauses);
        };
    };
    Ok(())
}
fn impl_from_binary(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    {
        output.blit_ident(141);
        if let Err(err) = bodyless_impl_from(
            output,
            None,
            Ident::new("FromBinary", Span::call_site()),
            ctx,
        ) {
            return Err(err);
        };
        {
            let at = output.buf.len();
            output.blit(9, 2);
            {
                let at = output.buf.len();
                output.blit(11, 4);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(15, 8);
                output.buf.push(TokenTree::from(ctx.lifetime.clone()));
                output.blit_punct(2);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(23, 3);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_from_json_field_visitor(
    output: &mut RustWriter,
    ctx: &Ctx,
    ty: &dyn Fn(&mut RustWriter),
    inner: TokenStream,
) -> Result<(), Error> {
    {
        output.blit_ident(141);
        if let Err(err) = bodyless_impl_from(
            output,
            Some(Ident::new("json", Span::call_site())),
            Ident::new("FromJsonFieldVisitor", Span::call_site()),
            ctx,
        ) {
            return Err(err);
        };
        {
            let at = output.buf.len();
            output.blit(26, 3);
            {
                ty(output)
            };
            output.blit(29, 4);
            {
                let at = output.buf.len();
                output.blit(33, 18);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(51, 8);
                output.buf.push(TokenTree::from(ctx.lifetime.clone()));
                output.blit_punct(2);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(59, 6);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_from_json(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    {
        output.blit_ident(141);
        if let Err(err) =
            bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)
        {
            return Err(err);
        };
        {
            let at = output.buf.len();
            output.blit(65, 3);
            {
                let at = output.buf.len();
                output.blit(68, 19);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(51, 8);
                output.buf.push(TokenTree::from(ctx.lifetime.clone()));
                output.blit_punct(2);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(87, 27);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn impl_to_binary(
    output: &mut RustWriter,
    Ctx {
        target, crate_path, ..
    }: &Ctx,
    inner: TokenStream,
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        output.blit(114, 2);
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, DEF)
            };
            output.blit_punct(2);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(116, 4);
        output.buf.push(TokenTree::from(target.name.clone()));
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, USE)
            };
            output.blit_punct(2);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(116);
            {
                for ty in &target.generic_field_types {
                    {
                        output.buf.extend_from_slice(ty);
                        output.blit(120, 3);
                    }
                }
            };
        };
        {
            let at = output.buf.len();
            output.blit(123, 2);
            {
                let at = output.buf.len();
                output.blit(125, 13);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
        };
    };
    Ok(())
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
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        output.blit_ident(89);
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, DEF)
            };
            output.blit_punct(2);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(138, 4);
        output.buf.push(TokenTree::from(target.name.clone()));
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, USE)
            };
            output.blit_punct(2);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(116);
            {
                for ty in &target.generic_field_types {
                    {
                        output.buf.extend_from_slice(ty);
                        output.blit_punct(9);
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(142, 4);
                    }
                }
            };
        };
        {
            let at = output.buf.len();
            output.blit(146, 3);
            {
                match kind {
                    ToJsonKind::Static(kind) => {
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(107, 5);
                        output.buf.push(Ident::new(kind, Span::call_site()).into());
                    }
                    ToJsonKind::Forward(field) => {
                        output.blit_punct(3);
                        output.buf.extend_from_slice(field.ty);
                        output.blit_ident(147);
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(149, 7);
                    }
                }
            };
            output.blit(156, 3);
            {
                let at = output.buf.len();
                output.blit(159, 11);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(170, 6);
            {
                output
                    .buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            };
            output.tt_group(Delimiter::Brace, at);
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
    pub fn dead_target_type(&self, out: &mut RustWriter) {
        {
            out.buf.push(TokenTree::from(self.target.name.clone()));
            if !self.target.generics.is_empty() {
                out.blit_punct(3);
                {
                    fmt_generics(out, &self.target.generics, DEAD_USE)
                };
                out.blit_punct(2);
            };
        }
    }
    pub fn target_type(&self, out: &mut RustWriter) {
        {
            out.buf.push(TokenTree::from(self.target.name.clone()));
            if !self.target.generics.is_empty() {
                out.blit_punct(3);
                {
                    fmt_generics(out, &self.target.generics, USE)
                };
                out.blit_punct(2);
            };
        }
    }
    fn new(out: &mut RustWriter, target: &'a DeriveTargetInner) -> Result<Ctx<'a>, Error> {
        let crate_path = if let Some(value) = &target.path_override {
            let content = value.to_string();
            #[allow(unused)]
            let inner = &content[1..content.len() - 1];
            {
                out.blit(104, 3);
            };
            std::mem::take(&mut out.buf)
        } else {
            {
                out.blit(104, 3);
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
    output: &mut RustWriter,
    ctx: &Ctx,
    field: &Field,
    place: &dyn Fn(&mut RustWriter),
) {
    {
        {
            if let Some(path) = field.with(TO_BINARY) {
                {
                    output.buf.extend_from_slice(path);
                }
            } else {
                {
                    output.blit_punct(3);
                    output.buf.extend_from_slice(field.ty);
                    output.blit_ident(147);
                    output.buf.extend_from_slice(&ctx.crate_path);
                    output.blit(176, 4);
                }
            }
        };
        output.blit(180, 3);
        {
            let at = output.buf.len();
            {
                place(output)
            };
            output.blit(127, 2);
            output.tt_group(Delimiter::Parenthesis, at);
        };
        output.blit_punct(13);
    };
}
fn binary_decode_field(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    {
        {
            if let Some(path) = field.with(FROM_BINARY) {
                {
                    out.buf.extend_from_slice(path);
                }
            } else {
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit_ident(147);
                    {
                        ctx.FromBinary(out)
                    };
                    out.blit_punct(2);
                }
            }
        };
        out.blit(183, 3);
        {
            let at = out.buf.len();
            out.blit_ident(86);
            out.tt_group(Delimiter::Parenthesis, at);
        };
    };
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromJson(&self, out: &mut RustWriter) {
        {
            out.buf.extend_from_slice(&self.crate_path);
            out.blit(186, 5);
            out.buf.push(TokenTree::from(self.lifetime.clone()));
            out.blit_punct(2);
        }
    }
    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        {
            out.buf.extend_from_slice(&self.crate_path);
            out.blit(191, 5);
            out.buf.push(TokenTree::from(self.lifetime.clone()));
            out.blit_punct(2);
        }
    }
}
fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    if let Some(with) = field.with(FROM_JSON) {
        {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(196, 22);
            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
            out.blit(7, 2);
            out.buf.extend_from_slice(field.ty);
            out.blit(218, 3);
            {
                let at = out.buf.len();
                out.blit_punct(4);
                out.buf.extend_from_slice(with);
                out.blit(221, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
    } else {
        {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(224, 6);
            {
                let at = out.buf.len();
                out.blit_punct(3);
                out.buf.extend_from_slice(field.ty);
                out.blit_ident(147);
                {
                    ctx.FromJson(out)
                };
                out.blit(230, 4);
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
    }
    Ok(())
}
fn field_name_literal(ctx: &Ctx, field: &Field) -> Literal {
    if let Some(name) = field.rename(FROM_JSON) {
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
    if let Some(name) = field.rename(TO_JSON) {
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
    if let Some(name) = &field.rename(TO_JSON) {
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
    out: &mut RustWriter,
    ctx: &Ctx,
    fields: &[&Field],
    temp_tuple: Option<&Ident>,
) -> Result<(), Error> {
    let ag_gen = if ctx.target.generics.iter().any(|g| !match g.kind {
        GenericKind::Lifetime => true,
        _ => false,
    }) {
        let x = out.buf.len();
        out.blit_punct(3);
        fmt_generics(
            out,
            &ctx.target.generics,
            GenericBoundFormating {
                lifetimes: false,
                bounds: false,
            },
        );
        out.blit_punct(2);
        TokenTree::Group(Group::new(Delimiter::None, out.buf.drain(x..).collect()))
    } else {
        TokenTree::Group(Group::new(Delimiter::None, TokenStream::new()))
    };
    let ts = {
        let len = out.buf.len();
        for (i, field) in fields.iter().enumerate() {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(234, 6);
            {
                let at = out.buf.len();
                out.blit(240, 2);
                out.buf.push(field_name_literal(ctx, field).into());
                out.blit(242, 13);
                {
                    let at = out.buf.len();
                    {
                        if let Some(ty) = temp_tuple {
                            {
                                out.buf.push(TokenTree::from(ty.clone()));
                                out.blit_punct(1);
                                out.buf.push(Literal::usize_unsuffixed(i).into());
                            }
                        } else {
                            {
                                out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                out.buf.push(TokenTree::from(ag_gen.clone()));
                                out.blit_punct(1);
                                out.buf.push(TokenTree::from(field.name.clone()));
                            }
                        }
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(255, 3);
                if let Err(err) = schema_field_decode(out, ctx, field) {
                    return Err(err);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(1);
        }
        out.split_off_stream(len)
    };
    let schema_fields = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.buf.len();
        {
            for field in fields {
                {
                    out.blit(258, 7);
                    {
                        let at = out.buf.len();
                        out.blit(265, 10);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(275, 4);
                        {
                            let at = out.buf.len();
                            out.blit(279, 2);
                            out.buf.extend_from_slice(field.ty);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(1);
                }
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
                {
                    out.blit(281, 16);
                    {
                        let at = out.buf.len();
                        out.blit(297, 3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit_punct(5);
                        out.buf.extend_from_slice(default);
                        out.blit(29, 2);
                        {
                            let at = out.buf.len();
                            out.blit(300, 6);
                            {
                                let at = out.buf.len();
                                out.blit_ident(146);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(13);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(306, 9);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(1);
                }
            }
        };
        out.split_off_stream(len)
    };
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    {
        out.blit(315, 9);
        {
            let at = out.buf.len();
            out.blit(324, 3);
            out.buf.push(schema_fields);
            out.blit(327, 4);
            out.buf.push(schema_drops);
            out.blit(331, 4);
            out.buf.push(schema_defaults);
            out.blit_punct(1);
            out.tt_group(Delimiter::Brace, at);
        };
    }
    Ok(())
}
fn body_of_struct_from_json_with_flatten(
    out: &mut RustWriter,
    ctx: &Ctx,
    flatten_field: &Field,
) -> TokenStream {
    {
        let len = out.buf.len();
        out.blit(335, 5);
        out.buf.extend_from_slice(flatten_field.ty);
        out.blit(340, 14);
        {
            let at = out.buf.len();
            out.blit(354, 3);
            {
                let at = out.buf.len();
                out.blit(253, 2);
                {
                    let at = out.buf.len();
                    {
                        ctx.dead_target_type(out)
                    };
                    out.blit_punct(1);
                    out.buf.push(TokenTree::from(flatten_field.name.clone()));
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(357, 3);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit(360, 6);
        out.buf.push(TokenTree::from(ctx.lifetime.clone()));
        if !ctx.generics.is_empty() {
            out.blit_punct(1);
            {
                fmt_generics(out, ctx.generics, USE)
            };
        };
        out.blit(366, 4);
        {
            let at = out.buf.len();
            out.blit(370, 5);
            {
                let at = out.buf.len();
                out.blit(375, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.split_off_stream(len)
    }
}
fn schema_ordered_fields<'a>(fields: &'a [Field<'a>]) -> Vec<&'a Field<'a>> {
    let mut buf = Vec::with_capacity(fields.len());
    for field in fields {
        if field.flags & (Field::WITH_TO_JSON_DEFAULT | Field::WITH_TO_JSON_FLATTEN)
            == Field::WITH_TO_JSON_DEFAULT
        {
            buf.push(field);
        }
    }
    for field in fields {
        if field.flags & (Field::WITH_TO_JSON_DEFAULT | Field::WITH_TO_JSON_FLATTEN) == 0 {
            buf.push(field);
        }
    }
    buf
}
fn required_bitset(ordered: &[&Field]) -> u64 {
    let mut defaults = 0;
    for field in ordered {
        if field.flags & Field::WITH_TO_JSON_DEFAULT != 0 {
            defaults += 1;
            continue;
        }
        break;
    }
    ((1 << ordered.len()) - 1) ^ ((1 << defaults) - 1)
}
fn tuple_struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let head = out.buf.len();
    match fields {
        [] => {
            return Err(Error::msg(
                "FromJson not implemented for Tuples without fields yet.",
            ))
        }
        [field] => {
            {
                out.blit_punct(3);
                out.buf.extend_from_slice(field.ty);
                out.blit(378, 9);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit(387, 5);
                {
                    let at = out.buf.len();
                    out.blit_ident(148);
                    if !match ctx.target.repr {
                        ast::Repr::Transparent | ast::Repr::C => true,
                        _ => false,
                    } {
                        out.blit(355, 2);
                        {
                            let at = out.buf.len();
                            out.blit(245, 10);
                            {
                                let at = out.buf.len();
                                {
                                    ctx.dead_target_type(out)
                                };
                                out.blit_punct(1);
                                out.buf.push(Literal::usize_unsuffixed(0).into());
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    };
                    out.blit(47, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            ToJsonKind::Forward(field)
        }
        _ => {
            return Err(Error::msg(
                "FromJson not implemented for Tuples with mulitple fields yet.",
            ))
        }
    };
    let stream = out.split_off_stream(head);
    impl_from_json(out, ctx, stream)
}
fn tuple_struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let head = out.buf.len();
    let kind = match fields {
        [] => {
            {
                out.blit(392, 3);
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
            {
                {
                    if let Some(with) = field.with(TO_JSON) {
                        {
                            out.buf.extend_from_slice(with);
                            out.blit(395, 3);
                        }
                    } else {
                        {
                            out.blit_punct(3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit(398, 11);
                        }
                    }
                };
                {
                    let at = out.buf.len();
                    out.blit(409, 3);
                    out.buf
                        .push(TokenTree::from(Literal::usize_unsuffixed(0).clone()));
                    out.blit(161, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            ToJsonKind::Forward(field)
        }
        _ => {
            let mut first = true;
            {
                out.blit(412, 5);
                for (i, field) in fields.iter().enumerate() {
                    {
                        if first {
                            first = false;
                        } else {
                            {
                                out.blit(417, 4);
                            };
                        }
                    };
                    {
                        if let Some(with) = field.with(TO_JSON) {
                            {
                                out.buf.extend_from_slice(with);
                                out.blit(395, 3);
                            }
                        } else {
                            {
                                out.blit_punct(3);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(398, 11);
                            }
                        }
                    };
                    {
                        let at = out.buf.len();
                        out.blit(409, 3);
                        out.buf
                            .push(TokenTree::from(Literal::usize_unsuffixed(i).clone()));
                        out.blit(161, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                }
                out.blit(421, 4);
            };
            ToJsonKind::Static("AlwaysArray")
        }
    };
    let stream = out.split_off_stream(head);
    impl_to_json(out, kind, ctx, stream)
}
fn inner_struct_to_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    fields: &[Field],
    text: &mut String,
    on_self: bool,
) -> Result<(), Error> {
    let mut first = true;
    {
        {
            for (i, field) in fields.iter().enumerate() {
                if !first {
                    text.push(',');
                }
                let flattened = field.flatten(TO_JSON);
                first = flattened;
                if !flattened {
                    if let Err(err) = field_name_json(ctx, field, text) {
                        return Err(err);
                    }
                    text.push(':');
                }
                if !text.is_empty() {
                    if text == "," {
                        {
                            out.blit(425, 5);
                        };
                    } else {
                        {
                            out.blit(392, 3);
                            {
                                let at = out.buf.len();
                                {
                                    out.buf.push(TokenTree::Literal(Literal::string(&text)));
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(13);
                        };
                    }
                    text.clear();
                }
                if let Via::Iterator = field.via(TO_JSON) {
                    if flattened {
                        {
                            out.blit_ident(110);
                            {
                                let at = out.buf.len();
                                out.blit(430, 3);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_ident(27);
                            {
                                let at = out.buf.len();
                                {
                                    if on_self {
                                        {
                                            out.blit(409, 3);
                                            out.buf.push(TokenTree::from(field.name.clone()));
                                        }
                                    } else {
                                        {
                                            out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(433, 3);
                            {
                                let at = out.buf.len();
                                out.blit(436, 16);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(157);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(452, 9);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(157);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(461, 6);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        };
                        continue;
                    }
                }
                if flattened {
                    {
                        out.blit(467, 18);
                    };
                }
                {
                    {
                        if let Some(with) = field.with(TO_JSON) {
                            {
                                out.buf.extend_from_slice(with);
                                out.blit(395, 3);
                            }
                        } else {
                            {
                                out.blit_punct(3);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(398, 11);
                            }
                        }
                    };
                    {
                        let at = out.buf.len();
                        {
                            if on_self {
                                {
                                    out.blit(409, 3);
                                    out.buf.push(TokenTree::from(field.name.clone()));
                                }
                            } else {
                                {
                                    out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                }
                            }
                        };
                        out.blit(161, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                };
                if flattened {
                    {
                        out.blit(485, 5);
                    };
                }
            }
        };
    };
    Ok(())
}
fn struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut text = String::new();
    let body = {
        let len = out.buf.len();
        out.blit(490, 5);
        {
            inner_struct_to_json(out, ctx, fields, &mut text, true)?
        };
        out.blit(495, 4);
        out.split_off_stream(len)
    };
    impl_to_json(out, ToJsonKind::Static("AlwaysObject"), ctx, body)
}
fn struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut flattening: Option<&Field> = None;
    for field in fields {
        if field.flatten(FROM_JSON) {
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
        out.blit(499, 5);
        {
            let at = out.buf.len();
            out.blit(504, 6);
            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
            if !ctx.generics.is_empty() {
                out.blit_punct(1);
                {
                    fmt_generics(out, ctx.generics, DEF)
                };
            };
            out.blit(510, 15);
            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
            out.blit_punct(2);
            if !ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty() {
                out.blit_ident(116);
                for ty in &ctx.target.generic_field_types {
                    out.buf.extend_from_slice(ty);
                    out.blit(187, 4);
                    out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                    out.blit_punct(2);
                }
            };
            {
                let at = out.buf.len();
                out.blit(514, 9);
                {
                    let at = out.buf.len();
                    out.blit(525, 4);
                    {
                        let at = out.buf.len();
                        {
                            struct_schema(out, ctx, &ordered_fields, None)?
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(529, 12);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            {
                let ts = if let Some(flatten_field) = flattening {
                    body_of_struct_from_json_with_flatten(out, ctx, flatten_field)
                } else {
                    {
                        let len = out.buf.len();
                        out.blit(361, 5);
                        out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                        if !ctx.generics.is_empty() {
                            out.blit_punct(1);
                            {
                                fmt_generics(out, ctx.generics, USE)
                            };
                        };
                        out.blit(366, 4);
                        {
                            let at = out.buf.len();
                            out.blit(541, 5);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.split_off_stream(len)
                    }
                };
                impl_from_json(out, ctx, ts)?;
                if ctx.target.flattenable {
                    let body = {
                        let len = out.buf.len();
                        out.blit(546, 7);
                        {
                            let at = out.buf.len();
                            out.blit(553, 11);
                            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                            if !ctx.generics.is_empty() {
                                out.blit_punct(1);
                                {
                                    fmt_generics(out, ctx.generics, USE)
                                };
                            };
                            out.blit(564, 5);
                            out.buf
                                .push(TokenTree::from(Literal::u64_unsuffixed(0).clone()));
                            out.blit(569, 3);
                            out.buf.push(TokenTree::from(
                                Literal::u64_unsuffixed(required_bitset(&ordered_fields)).clone(),
                            ));
                            out.blit_punct(1);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.split_off_stream(len)
                    };
                    impl_from_json_field_visitor(
                        out,
                        ctx,
                        &(|out| {
                            out.blit(572, 9);
                            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                            out.blit_punct(2);
                        }),
                        body,
                    )?;
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_punct(13);
    };
    Ok(())
}
fn variant_key_literal(_ctx: &Ctx, variant: &EnumVariant) -> Literal {
    let buf = variant.name.to_string();
    Literal::string(&buf)
}
fn enum_to_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let mut text = String::with_capacity(64);
    let start = out.buf.len();
    let mut all_objects = true;
    for variant in variants {
        if let EnumKind::Struct = variant.kind {
            continue;
        }
        all_objects = false;
        break;
    }
    if let Tag::Inline(tag_name) = &ctx.target.tag {
        {
            out.blit(490, 5);
        };
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        {
            out.blit(392, 3);
            {
                let at = out.buf.len();
                out.buf.push(Literal::string(&text).into());
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(13);
        };
    } else if all_objects {
        {
            out.blit(490, 5);
        };
    }
    {
        out.blit(581, 2);
        {
            let at = out.buf.len();
            {
                for (_, variant) in variants.iter().enumerate() {
                    {
                        out.buf.push(TokenTree::from(ctx.target.name.clone()));
                        out.blit(3, 2);
                        out.buf.push(TokenTree::from(variant.name.clone()));
                    }
                    match variant.kind {
                        EnumKind::Tuple => {
                            {
                                let at = out.buf.len();
                                {
                                    for (i, _) in variant.fields.iter().enumerate() {
                                        {
                                            out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(583, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::Struct => {
                            {
                                let at = out.buf.len();
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        {
                                            out.buf.push(TokenTree::from(field.name.clone()));
                                            out.blit_punct(9);
                                            out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(583, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::None => {
                            out.blit(583, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                    }
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
    if let Tag::Inline(..) = &ctx.target.tag {
        {
            out.blit(585, 5);
        };
    } else if all_objects {
        {
            out.blit(585, 5);
        };
    }
    {
        out.blit(590, 5);
    };
    let stream = out.buf.drain(start..).collect();
    let kind = if all_objects {
        "AlwaysObject"
    } else {
        "AlwaysString"
    };
    impl_to_json(out, ToJsonKind::Static(kind), ctx, stream)
}
fn enum_variant_to_json_struct(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
) -> Result<(), Error> {
    {
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    out.blit(490, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => text.push('{'),
                Tag::Default => text.push('{'),
                _ => (),
            }
        };
        {
            inner_struct_to_json(out, ctx, &variant.fields, text, false)?
        };
        out.blit_punct(13);
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    out.blit(585, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => {
                    out.blit(585, 5);
                }
                Tag::Default => {
                    out.blit(585, 5);
                }
                _ => (),
            }
        };
    };
    Ok(())
}
fn enum_variant_to_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    text: &mut String,
    all_objects: bool,
) -> Result<(), Error> {
    let start = out.buf.len();
    match &ctx.target.tag {
        Tag::Inline(..) => {
            if let EnumKind::None = variant.kind {
            } else {
                variant_name_json(ctx, variant, text)?;
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
                    {
                        out.blit(490, 5);
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
            if !text.is_empty() {
                {
                    out.blit(392, 3);
                    {
                        let at = out.buf.len();
                        out.buf.push(Literal::string(&text).into());
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                };
                text.clear();
            }
            {
                {
                    if let Some(with) = field.with(TO_JSON) {
                        {
                            out.buf.extend_from_slice(with);
                            out.blit(395, 3);
                        }
                    } else {
                        {
                            out.blit_punct(3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit(398, 11);
                        }
                    }
                };
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.temp[0].clone()));
                    out.blit(161, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
            }
        }
        EnumKind::Struct => {
            if let Err(err) = enum_variant_to_json_struct(out, ctx, variant, text) {
                return Err(err);
            }
        }
        EnumKind::None => {
            variant_name_json(ctx, variant, text)?;
            {
                out.blit(392, 3);
                {
                    let at = out.buf.len();
                    out.buf.push(Literal::string(&text).into());
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
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
                        out.blit(585, 5);
                    };
                }
            }
            _ => (),
        }
    }
    let ts = out.buf.drain(start..).collect();
    out.buf
        .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
    Ok(())
}
fn enum_variant_from_json_struct(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) -> Result<(), Error> {
    let ordered_fields = schema_ordered_fields(variant.fields);
    let mut flattening: Option<&Field> = None;
    for field in variant.fields {
        if field.flatten(FROM_JSON) {
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
            out.blit(595, 2);
            if ctx.target.has_lifetime() {
                out.blit(1, 2);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit_punct(2);
            };
            out.blit_punct(5);
            {
                let at = out.buf.len();
                for field in &ordered_fields {
                    out.buf.extend_from_slice(field.ty);
                    out.blit_punct(1);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(597, 13);
            {
                let at = out.buf.len();
                out.blit(610, 3);
                {
                    let at = out.buf.len();
                    out.blit_punct(4);
                    if let Err(err) = struct_schema(
                        out,
                        ctx,
                        &ordered_fields,
                        Some(&Ident::new("__TEMP", Span::call_site())),
                    ) {
                        return Err(err);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(613, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(626, 40);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(666, 11);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(340, 14);
            {
                let at = out.buf.len();
                out.blit(677, 12);
                {
                    let at = out.buf.len();
                    out.blit(689, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(357, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(13);
            if let Tag::Inline(tag_name) = &ctx.target.tag {
                if ctx.target.content.is_none() {
                    out.blit(696, 11);
                    {
                        let at = out.buf.len();
                        out.blit(707, 2);
                        out.buf.push(Literal::string(tag_name).into());
                        out.blit(709, 4);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(13);
                };
            };
            out.blit(713, 3);
            {
                let at = out.buf.len();
                out.blit_ident(125);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(716, 4);
            {
                let at = out.buf.len();
                out.blit(677, 12);
                {
                    let at = out.buf.len();
                    out.blit(720, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(371, 4);
                {
                    let at = out.buf.len();
                    out.blit(375, 3);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(1);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(727, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(125);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(109);
            {
                let at = out.buf.len();
                out.blit(729, 14);
                {
                    ctx.target_type(out)
                };
                out.blit(743, 4);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                    out.blit(3, 2);
                    out.buf.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.buf.push(TokenTree::from(field.name.clone()));
                            out.blit(747, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(1);
                        }
                        out.buf.push(TokenTree::from(flatten_field.name.clone()));
                        out.blit(750, 6);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                if untagged {
                    out.blit(756, 3);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
    } else {
        {
            out.blit(595, 2);
            if ctx.target.has_lifetime() {
                out.blit(1, 2);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit_punct(2);
            };
            out.blit_punct(5);
            {
                let at = out.buf.len();
                for field in &ordered_fields {
                    out.buf.extend_from_slice(field.ty);
                    out.blit_punct(1);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(597, 13);
            {
                let at = out.buf.len();
                out.blit(610, 3);
                {
                    let at = out.buf.len();
                    out.blit_punct(4);
                    if let Err(err) = struct_schema(
                        out,
                        ctx,
                        &ordered_fields,
                        Some(&Ident::new("__TEMP", Span::call_site())),
                    ) {
                        return Err(err);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(613, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(759, 27);
            {
                let at = out.buf.len();
                out.blit_ident(125);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(716, 4);
            {
                let at = out.buf.len();
                out.blit(677, 12);
                {
                    let at = out.buf.len();
                    out.blit(720, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(786, 5);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(727, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(125);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(109);
            {
                let at = out.buf.len();
                out.blit(729, 14);
                {
                    ctx.target_type(out)
                };
                out.blit(743, 4);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                    out.blit(3, 2);
                    out.buf.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.buf.push(TokenTree::from(field.name.clone()));
                            out.blit(747, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(1);
                        }
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                if untagged {
                    out.blit(756, 3);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
    };
    Ok(())
}
fn enum_variant_from_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) -> Result<(), Error> {
    let start = out.buf.len();
    match variant.kind {
        EnumKind::Tuple => {
            if ctx.target.content.is_some() {
                {
                    out.blit(791, 3);
                    {
                        let at = out.buf.len();
                        out.blit(727, 2);
                        {
                            let at = out.buf.len();
                            out.blit(794, 10);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            }
            let [field] = variant.fields else {
                return Err(Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                ));
            };
            {
                out.blit_ident(138);
                {
                    if let Some(with) = field.with(FROM_JSON) {
                        {
                            out.buf.extend_from_slice(with);
                        }
                    } else {
                        {
                            out.blit_punct(3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit(378, 9);
                            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                            out.blit(387, 2);
                        }
                    }
                };
                out.blit(221, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(155);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(149);
                    {
                        let at = out.buf.len();
                        out.blit_ident(146);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(583, 2);
                    {
                        let at = out.buf.len();
                        out.blit(737, 6);
                        {
                            ctx.target_type(out)
                        };
                        out.blit(743, 4);
                        {
                            let at = out.buf.len();
                            out.buf.push(TokenTree::from(ctx.target.name.clone()));
                            out.blit(3, 2);
                            out.buf.push(TokenTree::from(variant.name.clone()));
                            {
                                let at = out.buf.len();
                                out.blit_ident(146);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(13);
                        if untagged {
                            out.blit(756, 3);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(804, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(125);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(583, 2);
                    {
                        let at = out.buf.len();
                        if !untagged {
                            out.blit(727, 2);
                            {
                                let at = out.buf.len();
                                out.blit_ident(125);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(13);
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
                    out.blit(791, 3);
                    {
                        let at = out.buf.len();
                        out.blit(727, 2);
                        {
                            let at = out.buf.len();
                            out.blit(794, 10);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            }
            if let Err(err) = enum_variant_from_json_struct(out, ctx, variant, untagged) {
                return Err(err);
            }
        }
        EnumKind::None => {
            {
                if let Tag::Inline(..) = ctx.target.tag {
                    if ctx.target.content.is_none() {
                        {
                            out.blit(713, 3);
                            {
                                let at = out.buf.len();
                                out.blit_ident(145);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(806, 5);
                            {
                                let at = out.buf.len();
                                out.blit(727, 2);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(145);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    }
                }
            };
            out.blit(737, 6);
            {
                ctx.target_type(out)
            };
            out.blit(743, 4);
            {
                let at = out.buf.len();
                out.buf.push(TokenTree::from(ctx.target.name.clone()));
                out.blit(3, 2);
                out.buf.push(TokenTree::from(variant.name.clone()));
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(13);
            if untagged {
                out.blit(756, 3);
            };
        }
    };
    let ts = TokenStream::from_iter(out.buf.drain(start..));
    out.buf
        .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
    Ok(())
}
fn enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    if ctx.target.flattenable {
        return Err(Error::msg("Flattening enums not supported yet."));
    }
    let mut mixed_strings_and_objects = false;
    let inline_tag = match &ctx.target.tag {
        Tag::Inline(literal) => Some(literal),
        Tag::Untagged => {
            let body = {
                let len = out.buf.len();
                out.blit(811, 11);
                {
                    let at = out.buf.len();
                    for (i, variant) in variants.iter().enumerate() {
                        {
                            let at = out.buf.len();
                            if i != 0 {
                                out.blit(822, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(825, 2);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                            };
                            if let Err(err) = enum_variant_from_json(out, ctx, variant, true) {
                                return Err(err);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                    }
                    out.blit(727, 2);
                    {
                        let at = out.buf.len();
                        out.blit(827, 10);
                        {
                            let at = out.buf.len();
                            out.blit(837, 2);
                            out.buf.push(
                                Literal::string("Untagged enum didn't match any variant").into(),
                            );
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(149);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.split_off_stream(len)
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
        let len = out.buf.len();
        {
            if let Some(tag) = inline_tag {
                {
                    if ctx.target.content.is_some() {
                        out.blit(839, 5);
                    };
                    out.blit(844, 6);
                    {
                        if let Some(content) = &ctx.target.content {
                            {
                                out.blit_ident(47);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(Literal::string(tag).into());
                                    out.blit_punct(1);
                                    out.buf.push(Literal::string(content).into());
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                            }
                        } else {
                            {
                                out.blit_ident(48);
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
                                    out.blit_ident(149);
                                    {
                                        let at = out.buf.len();
                                        {
                                            let at = out.buf.len();
                                            out.blit(850, 3);
                                            out.tt_group(Delimiter::Parenthesis, at);
                                        };
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.blit(583, 2);
                                    {
                                        let at = out.buf.len();
                                        out.blit(853, 5);
                                        out.tt_group(Delimiter::Brace, at);
                                    };
                                }
                            } else {
                                {
                                    out.blit_ident(149);
                                    {
                                        let at = out.buf.len();
                                        out.blit_ident(146);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.blit(858, 4);
                                }
                            }
                        };
                        out.blit_ident(154);
                        {
                            let at = out.buf.len();
                            out.blit_ident(145);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(862, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(145);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(1);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(13);
                }
            } else {
                {
                    out.blit(866, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(105);
                        {
                            let at = out.buf.len();
                            out.blit_ident(136);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(806, 3);
                    {
                        if mixed_strings_and_objects {
                            {
                                out.blit_ident(23);
                            }
                        } else {
                            {
                                out.blit_ident(22);
                            }
                        }
                    };
                    out.blit(868, 2);
                    {
                        let at = out.buf.len();
                        out.blit(727, 2);
                        {
                            let at = out.buf.len();
                            out.blit(870, 8);
                            {
                                let at = out.buf.len();
                                out.blit(837, 2);
                                out.buf.push(
                                    Literal::string("Expected single field object for enum").into(),
                                );
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(13);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(13);
                }
            }
        };
        out.blit(878, 2);
        {
            let at = out.buf.len();
            {
                for variant in variants {
                    if mixed_strings_and_objects {
                        if let EnumKind::None = variant.kind {
                            continue;
                        }
                    }
                    {
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit(583, 2);
                        if let Err(err) = enum_variant_from_json(out, ctx, variant, false) {
                            return Err(err);
                        };
                        out.blit_punct(1);
                    };
                }
            };
            out.blit(880, 3);
            {
                let at = out.buf.len();
                out.blit(883, 11);
                {
                    let at = out.buf.len();
                    out.blit_ident(136);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(894, 3);
                {
                    let at = out.buf.len();
                    out.blit(897, 10);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        {
            if let Some(_) = inline_tag {
                if ctx.target.content.is_some() {
                    {
                        out.blit(907, 2);
                        {
                            let at = out.buf.len();
                            out.blit(909, 4);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit_ident(109);
                        {
                            let at = out.buf.len();
                            out.blit_ident(149);
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
                        out.blit_ident(149);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                }
            } else {
                {
                    out.blit(913, 8);
                    {
                        let at = out.buf.len();
                        out.blit_ident(149);
                        {
                            let at = out.buf.len();
                            out.blit_ident(79);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(583, 2);
                        {
                            let at = out.buf.len();
                            out.blit(921, 2);
                            {
                                let at = out.buf.len();
                                out.tt_group_empty(Delimiter::Parenthesis);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(804, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(145);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(583, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(154);
                            {
                                let at = out.buf.len();
                                out.blit_ident(145);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(923, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(105);
                            {
                                let at = out.buf.len();
                                out.blit_ident(143);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(583, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(154);
                            {
                                let at = out.buf.len();
                                out.blit(870, 8);
                                {
                                    let at = out.buf.len();
                                    out.blit(837, 2);
                                    out.buf.push(
                                        Literal::string("More the one field in enum tab object")
                                            .into(),
                                    );
                                    out.tt_group(Delimiter::Brace, at);
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit_punct(1);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(925, 11);
                    {
                        ctx.target_type(out)
                    };
                    out.blit_punct(2);
                    {
                        let at = out.buf.len();
                        out.blit(936, 7);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(943, 4);
                }
            }
        };
        out.split_off_stream(len)
    };
    if mixed_strings_and_objects {
        body = {
            let len = out.buf.len();
            out.blit(947, 5);
            {
                let at = out.buf.len();
                out.blit_ident(149);
                {
                    let at = out.buf.len();
                    out.blit(952, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(583, 2);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
                out.blit(923, 2);
                {
                    let at = out.buf.len();
                    out.blit(964, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(976, 7);
                {
                    let at = out.buf.len();
                    out.blit_ident(149);
                    {
                        let at = out.buf.len();
                        out.blit_ident(136);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(583, 2);
                    {
                        let at = out.buf.len();
                        out.blit(983, 5);
                        {
                            let at = out.buf.len();
                            {
                                for variant in variants {
                                    if let EnumKind::None = variant.kind {
                                        {
                                            out.buf.push(variant_key_literal(ctx, variant).into());
                                            out.blit(583, 2);
                                            out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                            out.blit(3, 2);
                                            out.buf.push(TokenTree::from(variant.name.clone()));
                                            out.blit_punct(1);
                                        };
                                    }
                                }
                            };
                            out.blit(880, 3);
                            {
                                let at = out.buf.len();
                                out.blit(891, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(886, 4);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(894, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(897, 10);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(736, 7);
                        {
                            ctx.target_type(out)
                        };
                        out.blit(743, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(146);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(988, 3);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(13);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_ident(154);
                    {
                        let at = out.buf.len();
                        out.blit_ident(145);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(862, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(145);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(1);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(923, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(143);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(583, 2);
                {
                    let at = out.buf.len();
                    out.blit(727, 2);
                    {
                        let at = out.buf.len();
                        out.blit(870, 8);
                        {
                            let at = out.buf.len();
                            out.blit(837, 2);
                            out.buf.push(
                                Literal::string("Expected either an object or a string").into(),
                            );
                            out.blit_punct(1);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(13);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(154);
                {
                    let at = out.buf.len();
                    out.blit_ident(145);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(862, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(145);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(1);
                out.tt_group(Delimiter::Brace, at);
            };
            out.split_off_stream(len)
        }
    }
    impl_from_json(out, ctx, body)?;
    Ok(())
}
fn handle_struct(target: &DeriveTargetInner, fields: &[Field]) -> Result<TokenStream, Error> {
    let mut output = RustWriter::new();
    let ctx = Ctx::new(&mut output, target)?;
    if target.from_json {
        struct_from_json(&mut output, &ctx, fields)?;
    }
    if target.to_json {
        struct_to_json(&mut output, &ctx, fields)?;
    }
    if target.to_binary {
        let body = {
            let len = output.buf.len();
            {
                for field in fields {
                    binary_encode_field(
                        &mut output,
                        &ctx,
                        field,
                        &(|out| {
                            out.blit(409, 3);
                            out.buf.push(TokenTree::from(field.name.clone()));
                        }),
                    )
                }
            };
            output.split_off_stream(len)
        };
        impl_to_binary(&mut output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.buf.len();
            output.buf.push(TokenTree::from(target.name.clone()));
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            output.buf.push(TokenTree::from(field.name.clone()));
                            output.blit_punct(9);
                            {
                                binary_decode_field(&mut output, &ctx, field)
                            };
                            output.blit_punct(1);
                        }
                    }
                };
                output.tt_group(Delimiter::Brace, at);
            };
            output.split_off_stream(len)
        };
        impl_from_binary(&mut output, &ctx, body)?;
    }
    Ok(output.buf.drain(..).collect())
}
fn handle_tuple_struct(target: &DeriveTargetInner, fields: &[Field]) -> Result<TokenStream, Error> {
    let mut output = RustWriter::new();
    let ctx = Ctx::new(&mut output, target)?;
    if target.from_json {
        tuple_struct_from_json(&mut output, &ctx, fields)?;
    }
    if target.to_json {
        tuple_struct_to_json(&mut output, &ctx, fields)?;
    }
    if target.to_binary {
        let body = {
            let len = output.buf.len();
            {
                for (i, field) in fields.iter().enumerate() {
                    binary_encode_field(
                        &mut output,
                        &ctx,
                        field,
                        &(|out| {
                            out.blit(409, 3);
                            out.buf
                                .push(TokenTree::from(Literal::usize_unsuffixed(i).clone()));
                        }),
                    )
                }
            };
            output.split_off_stream(len)
        };
        impl_to_binary(&mut output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.buf.len();
            output.buf.push(TokenTree::from(target.name.clone()));
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            {
                                binary_decode_field(&mut output, &ctx, field)
                            };
                            output.blit_punct(1);
                        }
                    }
                };
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.split_off_stream(len)
        };
        impl_from_binary(&mut output, &ctx, body)?;
    }
    Ok(output.buf.drain(..).collect())
}
fn enum_to_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let body = {
        let len = out.buf.len();
        out.blit(581, 2);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        out.buf.push(TokenTree::from(ctx.target.name.clone()));
                        out.blit(3, 2);
                        out.buf.push(TokenTree::from(variant.name.clone()));
                    }
                    match variant.kind {
                        EnumKind::Tuple => {
                            {
                                let at = out.buf.len();
                                {
                                    for (i, _) in variant.fields.iter().enumerate() {
                                        {
                                            out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(583, 2);
                            {
                                let at = out.buf.len();
                                out.blit(991, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        binary_encode_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            }),
                                        )
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::Struct => {
                            {
                                let at = out.buf.len();
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        {
                                            out.buf.push(TokenTree::from(field.name.clone()));
                                            out.blit_punct(9);
                                            out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(583, 2);
                            {
                                let at = out.buf.len();
                                out.blit(991, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
                                {
                                    for field in variant.fields {
                                        binary_encode_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.buf.push(TokenTree::from(ctx.temp[i].clone()));
                                            }),
                                        )
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::None => {
                            out.blit(583, 2);
                            {
                                let at = out.buf.len();
                                out.blit(991, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(13);
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
    impl_to_binary(out, ctx, body)
}
fn enum_from_binary(
    out: &mut RustWriter,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.buf.len();
        out.blit(994, 5);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        {
                            if i + 1 == variants.len() {
                                {
                                    out.blit_ident(143);
                                }
                            } else {
                                {
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                }
                            }
                        };
                        out.blit(583, 2);
                        {
                            match variant.kind {
                                EnumKind::Tuple => {
                                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                    out.blit(3, 2);
                                    out.buf.push(TokenTree::from(variant.name.clone()));
                                    {
                                        let at = out.buf.len();
                                        {
                                            for field in variant.fields {
                                                {
                                                    {
                                                        binary_decode_field(out, ctx, field)
                                                    };
                                                    out.blit_punct(1);
                                                }
                                            }
                                        };
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                }
                                EnumKind::Struct => {
                                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                    out.blit(3, 2);
                                    out.buf.push(TokenTree::from(variant.name.clone()));
                                    {
                                        let at = out.buf.len();
                                        {
                                            for field in variant.fields {
                                                {
                                                    out.buf
                                                        .push(TokenTree::from(field.name.clone()));
                                                    out.blit_punct(9);
                                                    {
                                                        binary_decode_field(out, ctx, field)
                                                    };
                                                    out.blit_punct(1);
                                                }
                                            }
                                        };
                                        out.tt_group(Delimiter::Brace, at);
                                    };
                                }
                                EnumKind::None => {
                                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                    out.blit(3, 2);
                                    out.buf.push(TokenTree::from(variant.name.clone()));
                                }
                            }
                        };
                        out.blit_punct(1);
                    }
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.split_off_stream(len)
    };
    impl_from_binary(out, ctx, body)
}
fn handle_enum(target: &DeriveTargetInner, variants: &[EnumVariant]) -> Result<TokenStream, Error> {
    let mut output = RustWriter::new();
    let mut ctx = Ctx::new(&mut output, target)?;
    let mut max_tuples = 0;
    for var in variants {
        if match var.kind {
            EnumKind::Tuple | EnumKind::Struct => true,
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
    Ok(output.buf.drain(..).collect())
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
        repr: ast::Repr::Default,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens)?;
    if !(target.from_binary || target.to_binary || target.to_json || target.from_json) {
        target.from_json = true;
    }
    let field_toks: Vec<TokenTree> = body.into_iter().collect();
    let mut tt_buf = Vec::<TokenTree>::new();
    let mut field_buf = Vec::<Field>::new();
    let mut pool = MemoryPool::<FieldAttrs>::new();
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
    match inner_derive(stream.clone()) {
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
