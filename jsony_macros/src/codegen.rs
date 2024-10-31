use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttr, Generic,
    GenericKind, Tag, Via,
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
                buffer.blit_ident(123);
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
            output.blit_ident(112);
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
        output.blit_ident(136);
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
        output.blit_ident(136);
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
        output.blit_ident(136);
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
            output.blit_ident(112);
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
                output.blit(125, 11);
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
        output.blit_ident(87);
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, DEF)
            };
            output.blit_punct(2);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(136, 4);
        output.buf.push(TokenTree::from(target.name.clone()));
        if any_generics {
            output.blit_punct(3);
            {
                fmt_generics(output, &target.generics, USE)
            };
            output.blit_punct(2);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(112);
            {
                for ty in &target.generic_field_types {
                    {
                        output.buf.extend_from_slice(ty);
                        output.blit_punct(9);
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(140, 4);
                    }
                }
            };
        };
        {
            let at = output.buf.len();
            output.blit(144, 3);
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
                        output.blit_ident(142);
                        output.buf.extend_from_slice(&crate_path);
                        output.blit(147, 7);
                    }
                }
            };
            output.blit(154, 3);
            {
                let at = output.buf.len();
                output.blit(157, 11);
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.blit(168, 6);
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
        output.blit_punct(3);
        output.buf.extend_from_slice(field.ty);
        output.blit_ident(142);
        output.buf.extend_from_slice(&ctx.crate_path);
        output.blit(174, 7);
        {
            let at = output.buf.len();
            {
                place(output)
            };
            output.blit(127, 2);
            output.tt_group(Delimiter::Parenthesis, at);
        };
        output.blit_punct(0);
    };
}
fn binary_decode_field(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    {
        out.blit_punct(3);
        out.buf.extend_from_slice(field.ty);
        out.blit_ident(142);
        {
            ctx.FromBinary(out)
        };
        out.blit(181, 4);
        {
            let at = out.buf.len();
            out.blit_ident(83);
            out.tt_group(Delimiter::Parenthesis, at);
        };
    };
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromJson(&self, out: &mut RustWriter) {
        {
            out.buf.extend_from_slice(&self.crate_path);
            out.blit(185, 5);
            out.buf.push(TokenTree::from(self.lifetime.clone()));
            out.blit_punct(2);
        }
    }
    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        {
            out.buf.extend_from_slice(&self.crate_path);
            out.blit(190, 5);
            out.buf.push(TokenTree::from(self.lifetime.clone()));
            out.blit_punct(2);
        }
    }
}
fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    {
        out.buf.extend_from_slice(&ctx.crate_path);
        out.blit(195, 6);
        {
            let at = out.buf.len();
            out.blit_punct(3);
            out.buf.extend_from_slice(field.ty);
            out.blit_ident(142);
            {
                ctx.FromJson(out)
            };
            out.blit(201, 4);
            out.tt_group(Delimiter::Parenthesis, at);
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
            out.blit(205, 6);
            {
                let at = out.buf.len();
                out.blit(211, 2);
                out.buf.push(field_name_literal(ctx, field).into());
                out.blit(213, 13);
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
                out.blit(226, 3);
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
                    out.blit(229, 7);
                    {
                        let at = out.buf.len();
                        out.blit(236, 10);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(246, 4);
                        {
                            let at = out.buf.len();
                            out.blit(250, 2);
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
                let Some(default) = &field.attr.default else {
                    break;
                };
                {
                    out.blit(252, 16);
                    {
                        let at = out.buf.len();
                        out.blit(268, 3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit_punct(4);
                        out.buf.extend_from_slice(default);
                        out.blit(29, 2);
                        {
                            let at = out.buf.len();
                            out.blit(271, 6);
                            {
                                let at = out.buf.len();
                                out.blit_ident(137);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(277, 9);
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
        out.blit(286, 9);
        {
            let at = out.buf.len();
            out.blit(295, 3);
            out.buf.push(schema_fields);
            out.blit(298, 4);
            out.buf.push(schema_drops);
            out.blit(302, 4);
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
        out.blit(306, 5);
        out.buf.extend_from_slice(flatten_field.ty);
        out.blit(311, 14);
        {
            let at = out.buf.len();
            out.blit(325, 3);
            {
                let at = out.buf.len();
                out.blit(224, 2);
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
            out.blit(328, 3);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        out.blit(331, 6);
        out.buf.push(TokenTree::from(ctx.lifetime.clone()));
        if !ctx.generics.is_empty() {
            out.blit_punct(1);
            {
                fmt_generics(out, ctx.generics, USE)
            };
        };
        out.blit(337, 4);
        {
            let at = out.buf.len();
            out.blit(341, 5);
            {
                let at = out.buf.len();
                out.blit(346, 3);
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
                out.blit(349, 9);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit(358, 5);
                {
                    let at = out.buf.len();
                    out.blit_ident(143);
                    if !match ctx.target.repr {
                        ast::Repr::Transparent | ast::Repr::C => true,
                        _ => false,
                    } {
                        out.blit(326, 2);
                        {
                            let at = out.buf.len();
                            out.blit(216, 10);
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
                out.blit(363, 3);
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
                out.blit_punct(3);
                out.buf.extend_from_slice(field.ty);
                out.blit(366, 11);
                {
                    let at = out.buf.len();
                    out.blit(377, 3);
                    out.buf
                        .push(TokenTree::from(Literal::usize_unsuffixed(0).clone()));
                    out.blit(159, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            ToJsonKind::Forward(field)
        }
        _ => {
            let mut first = true;
            {
                out.blit(380, 5);
                for (i, field) in fields.iter().enumerate() {
                    {
                        if first {
                            first = false;
                        } else {
                            {
                                out.blit(385, 4);
                            };
                        }
                    };
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit(366, 11);
                    {
                        let at = out.buf.len();
                        out.blit(377, 3);
                        out.buf
                            .push(TokenTree::from(Literal::usize_unsuffixed(i).clone()));
                        out.blit(159, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
                out.blit(389, 4);
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
            for field in fields {
                if !first {
                    text.push(',');
                }
                first = field.attr.flatten;
                if !field.attr.flatten {
                    if let Err(err) = field_name_json(ctx, field, text) {
                        return Err(err);
                    }
                    text.push(':');
                }
                if !text.is_empty() {
                    if text == "," {
                        {
                            out.blit(393, 5);
                        };
                    } else {
                        {
                            out.blit(363, 3);
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
                if let Via::Iterator = field.attr.via {
                    if field.attr.flatten {
                        {
                            out.blit_ident(107);
                            {
                                let at = out.buf.len();
                                out.blit(398, 3);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_ident(26);
                            {
                                let at = out.buf.len();
                                if on_self {
                                    out.blit(378, 2);
                                };
                                out.buf.push(TokenTree::from(field.name.clone()));
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(401, 3);
                            {
                                let at = out.buf.len();
                                out.blit(404, 16);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(150);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(420, 9);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(150);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(429, 6);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        };
                        continue;
                    }
                }
                if field.attr.flatten {
                    {
                        out.blit(435, 18);
                    };
                }
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit(366, 11);
                    {
                        let at = out.buf.len();
                        if on_self {
                            out.blit(377, 3);
                        };
                        out.buf.push(TokenTree::from(field.name.clone()));
                        out.blit(159, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                if field.attr.flatten {
                    {
                        out.blit(453, 5);
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
        out.blit(458, 5);
        {
            inner_struct_to_json(out, ctx, fields, &mut text, true)?
        };
        out.blit(463, 4);
        out.split_off_stream(len)
    };
    impl_to_json(out, ToJsonKind::Static("AlwaysObject"), ctx, body)
}
fn struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
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
        out.blit(467, 5);
        {
            let at = out.buf.len();
            out.blit(472, 6);
            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
            if !ctx.generics.is_empty() {
                out.blit_punct(1);
                {
                    fmt_generics(out, ctx.generics, DEF)
                };
            };
            out.blit(478, 15);
            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
            out.blit_punct(2);
            if !ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty() {
                out.blit_ident(112);
                for ty in &ctx.target.generic_field_types {
                    out.buf.extend_from_slice(ty);
                    out.blit(186, 4);
                    out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                    out.blit_punct(2);
                }
            };
            {
                let at = out.buf.len();
                out.blit(482, 9);
                {
                    let at = out.buf.len();
                    out.blit(493, 4);
                    {
                        let at = out.buf.len();
                        {
                            struct_schema(out, ctx, &ordered_fields, None)?
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(497, 12);
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
                        out.blit(332, 5);
                        out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                        if !ctx.generics.is_empty() {
                            out.blit_punct(1);
                            {
                                fmt_generics(out, ctx.generics, USE)
                            };
                        };
                        out.blit(337, 4);
                        {
                            let at = out.buf.len();
                            out.blit(509, 5);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.split_off_stream(len)
                    }
                };
                impl_from_json(out, ctx, ts)?;
                if ctx.target.flattenable {
                    let body = {
                        let len = out.buf.len();
                        out.blit(514, 7);
                        {
                            let at = out.buf.len();
                            out.blit(521, 11);
                            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                            if !ctx.generics.is_empty() {
                                out.blit_punct(1);
                                {
                                    fmt_generics(out, ctx.generics, USE)
                                };
                            };
                            out.blit(532, 5);
                            out.buf
                                .push(TokenTree::from(Literal::u64_unsuffixed(0).clone()));
                            out.blit(537, 3);
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
                            out.blit(540, 9);
                            out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                            out.blit_punct(2);
                        }),
                        body,
                    )?;
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_punct(0);
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
    if let Tag::Inline(tag_name) = &ctx.target.tag {
        {
            out.blit(458, 5);
        };
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        {
            out.blit(363, 3);
            {
                let at = out.buf.len();
                out.buf.push(Literal::string(&text).into());
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
        };
    } else if all_objects {
        {
            out.blit(458, 5);
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
        out.blit(549, 2);
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
                            out.blit(551, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::Struct => {
                            {
                                let at = out.buf.len();
                                {
                                    for field in variant.fields {
                                        {
                                            out.buf.push(TokenTree::from(field.name.clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(551, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::None => {
                            out.blit(551, 2);
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
            out.blit(553, 5);
        };
    } else if all_objects {
        {
            out.blit(553, 5);
        };
    }
    {
        out.blit(558, 5);
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
                    out.blit(458, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => text.push('{'),
                Tag::Default => text.push('{'),
                _ => (),
            }
        };
        {
            inner_struct_to_json(out, ctx, &variant.fields, text, false)?
        };
        out.blit_punct(0);
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
                        out.blit(458, 5);
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
                out.blit_punct(3);
                out.buf.extend_from_slice(field.ty);
                out.blit(366, 11);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.temp[0].clone()));
                    out.blit(159, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
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
                out.blit(363, 3);
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
                    {
                        out.blit(553, 5);
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
            out.blit(563, 2);
            if ctx.target.has_lifetime() {
                out.blit(1, 2);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit_punct(2);
            };
            out.blit_punct(4);
            {
                let at = out.buf.len();
                for field in &ordered_fields {
                    out.buf.extend_from_slice(field.ty);
                    out.blit_punct(1);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(565, 13);
            {
                let at = out.buf.len();
                out.blit(578, 3);
                {
                    let at = out.buf.len();
                    out.blit_punct(5);
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
                out.blit(581, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(594, 40);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(634, 11);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(311, 14);
            {
                let at = out.buf.len();
                out.blit(645, 12);
                {
                    let at = out.buf.len();
                    out.blit(657, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(328, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if let Tag::Inline(tag_name) = &ctx.target.tag {
                if ctx.target.content.is_none() {
                    out.blit(664, 11);
                    {
                        let at = out.buf.len();
                        out.blit(675, 2);
                        out.buf.push(Literal::string(tag_name).into());
                        out.blit(677, 4);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(0);
                };
            };
            out.blit(681, 3);
            {
                let at = out.buf.len();
                out.blit_ident(122);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(684, 4);
            {
                let at = out.buf.len();
                out.blit(645, 12);
                {
                    let at = out.buf.len();
                    out.blit(688, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(342, 4);
                {
                    let at = out.buf.len();
                    out.blit(346, 3);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(1);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(695, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(122);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(84);
            {
                let at = out.buf.len();
                out.blit(697, 14);
                {
                    ctx.target_type(out)
                };
                out.blit(711, 4);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                    out.blit(3, 2);
                    out.buf.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.buf.push(TokenTree::from(field.name.clone()));
                            out.blit(715, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(1);
                        }
                        out.buf.push(TokenTree::from(flatten_field.name.clone()));
                        out.blit(718, 6);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                if untagged {
                    out.blit(724, 3);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
    } else {
        {
            out.blit(563, 2);
            if ctx.target.has_lifetime() {
                out.blit(1, 2);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit_punct(2);
            };
            out.blit_punct(4);
            {
                let at = out.buf.len();
                for field in &ordered_fields {
                    out.buf.extend_from_slice(field.ty);
                    out.blit_punct(1);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(565, 13);
            {
                let at = out.buf.len();
                out.blit(578, 3);
                {
                    let at = out.buf.len();
                    out.blit_punct(5);
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
                out.blit(581, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(727, 27);
            {
                let at = out.buf.len();
                out.blit_ident(122);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(684, 4);
            {
                let at = out.buf.len();
                out.blit(645, 12);
                {
                    let at = out.buf.len();
                    out.blit(688, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(754, 5);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(695, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(122);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(84);
            {
                let at = out.buf.len();
                out.blit(697, 14);
                {
                    ctx.target_type(out)
                };
                out.blit(711, 4);
                {
                    let at = out.buf.len();
                    out.buf.push(TokenTree::from(ctx.target.name.clone()));
                    out.blit(3, 2);
                    out.buf.push(TokenTree::from(variant.name.clone()));
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.buf.push(TokenTree::from(field.name.clone()));
                            out.blit(715, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(1);
                        }
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                if untagged {
                    out.blit(724, 3);
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
            let [field] = variant.fields else {
                return Err(Error::span_msg(
                    "Only single field enum tuples are currently supported.",
                    variant.name.span(),
                ));
            };
            {
                out.blit(759, 2);
                out.buf.extend_from_slice(field.ty);
                out.blit(349, 9);
                out.buf.push(TokenTree::from(ctx.lifetime.clone()));
                out.blit(761, 5);
                {
                    let at = out.buf.len();
                    out.blit_ident(149);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(141);
                    {
                        let at = out.buf.len();
                        out.blit_ident(137);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(551, 2);
                    {
                        let at = out.buf.len();
                        out.blit(705, 6);
                        {
                            ctx.target_type(out)
                        };
                        out.blit(711, 4);
                        {
                            let at = out.buf.len();
                            out.buf.push(TokenTree::from(ctx.target.name.clone()));
                            out.blit(3, 2);
                            out.buf.push(TokenTree::from(variant.name.clone()));
                            {
                                let at = out.buf.len();
                                out.blit_ident(137);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        if untagged {
                            out.blit(724, 3);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(766, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(122);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(551, 2);
                    {
                        let at = out.buf.len();
                        if !untagged {
                            out.blit(695, 2);
                            {
                                let at = out.buf.len();
                                out.blit_ident(122);
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
            if let Err(err) = enum_variant_from_json_struct(out, ctx, variant, untagged) {
                return Err(err);
            }
        }
        EnumKind::None => {
            out.blit(705, 6);
            {
                ctx.target_type(out)
            };
            out.blit(711, 4);
            {
                let at = out.buf.len();
                out.buf.push(TokenTree::from(ctx.target.name.clone()));
                out.blit(3, 2);
                out.buf.push(TokenTree::from(variant.name.clone()));
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if untagged {
                out.blit(724, 3);
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
                out.blit(768, 11);
                {
                    let at = out.buf.len();
                    for (i, variant) in variants.iter().enumerate() {
                        {
                            let at = out.buf.len();
                            if i != 0 {
                                out.blit(779, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(782, 2);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                            };
                            if let Err(err) = enum_variant_from_json(out, ctx, variant, true) {
                                return Err(err);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                    }
                    out.blit(695, 2);
                    {
                        let at = out.buf.len();
                        out.blit(784, 10);
                        {
                            let at = out.buf.len();
                            out.blit(794, 2);
                            out.buf.push(
                                Literal::string("Untagged enum didn't match any variant").into(),
                            );
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(141);
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
                    out.blit(796, 6);
                    {
                        if let Some(content) = &ctx.target.content {
                            {
                                out.blit_ident(45);
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
                                out.blit_ident(46);
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
                        out.blit_ident(141);
                        {
                            let at = out.buf.len();
                            out.blit_ident(137);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(802, 5);
                        {
                            let at = out.buf.len();
                            out.blit_ident(132);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(807, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(132);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(1);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(0);
                }
            } else {
                {
                    out.blit(811, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(103);
                        {
                            let at = out.buf.len();
                            out.blit_ident(130);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(770, 3);
                    {
                        if mixed_strings_and_objects {
                            {
                                out.blit_ident(22);
                            }
                        } else {
                            {
                                out.blit_ident(21);
                            }
                        }
                    };
                    out.blit(813, 2);
                    {
                        let at = out.buf.len();
                        out.blit(695, 2);
                        {
                            let at = out.buf.len();
                            out.blit(815, 8);
                            {
                                let at = out.buf.len();
                                out.blit(794, 2);
                                out.buf.push(
                                    Literal::string("Expected single field object for enum").into(),
                                );
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(0);
                }
            }
        };
        out.blit(823, 2);
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
                        out.blit(551, 2);
                        if let Err(err) = enum_variant_from_json(out, ctx, variant, false) {
                            return Err(err);
                        };
                        out.blit_punct(1);
                    };
                }
            };
            out.blit(825, 3);
            {
                let at = out.buf.len();
                out.blit(828, 11);
                {
                    let at = out.buf.len();
                    out.blit_ident(130);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(839, 3);
                {
                    let at = out.buf.len();
                    out.blit(842, 10);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        {
            if let Some(_) = inline_tag {
                if ctx.target.content.is_some() {
                    {
                        out.blit(852, 4);
                    }
                } else {
                    {
                        out.blit_ident(141);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                }
            } else {
                {
                    out.blit(856, 8);
                    {
                        let at = out.buf.len();
                        out.blit_ident(141);
                        {
                            let at = out.buf.len();
                            out.blit_ident(77);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(551, 2);
                        {
                            let at = out.buf.len();
                            out.blit(864, 2);
                            {
                                let at = out.buf.len();
                                out.tt_group_empty(Delimiter::Parenthesis);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(766, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(132);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(551, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(147);
                            {
                                let at = out.buf.len();
                                out.blit_ident(132);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(866, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(103);
                            {
                                let at = out.buf.len();
                                out.blit_ident(138);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(551, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(147);
                            {
                                let at = out.buf.len();
                                out.blit(815, 8);
                                {
                                    let at = out.buf.len();
                                    out.blit(794, 2);
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
                    out.blit(868, 11);
                    {
                        ctx.target_type(out)
                    };
                    out.blit_punct(2);
                    {
                        let at = out.buf.len();
                        out.blit(879, 7);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(886, 4);
                }
            }
        };
        out.split_off_stream(len)
    };
    if mixed_strings_and_objects {
        body = {
            let len = out.buf.len();
            out.blit(890, 5);
            {
                let at = out.buf.len();
                out.blit_ident(141);
                {
                    let at = out.buf.len();
                    out.blit(895, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(551, 2);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
                out.blit(866, 2);
                {
                    let at = out.buf.len();
                    out.blit(907, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(919, 7);
                {
                    let at = out.buf.len();
                    out.blit_ident(141);
                    {
                        let at = out.buf.len();
                        out.blit_ident(130);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(551, 2);
                    {
                        let at = out.buf.len();
                        out.blit(926, 5);
                        {
                            let at = out.buf.len();
                            {
                                for variant in variants {
                                    if let EnumKind::None = variant.kind {
                                        {
                                            out.buf.push(variant_key_literal(ctx, variant).into());
                                            out.blit(551, 2);
                                            out.buf.push(TokenTree::from(ctx.target.name.clone()));
                                            out.blit(3, 2);
                                            out.buf.push(TokenTree::from(variant.name.clone()));
                                            out.blit_punct(1);
                                        };
                                    }
                                }
                            };
                            out.blit(825, 3);
                            {
                                let at = out.buf.len();
                                out.blit(836, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(831, 4);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(839, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(842, 10);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(704, 7);
                        {
                            ctx.target_type(out)
                        };
                        out.blit(711, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(137);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(931, 3);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_ident(147);
                    {
                        let at = out.buf.len();
                        out.blit_ident(132);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(807, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(132);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(1);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(866, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(138);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(551, 2);
                {
                    let at = out.buf.len();
                    out.blit(695, 2);
                    {
                        let at = out.buf.len();
                        out.blit(815, 8);
                        {
                            let at = out.buf.len();
                            out.blit(794, 2);
                            out.buf.push(
                                Literal::string("Expected either an object or a string").into(),
                            );
                            out.blit_punct(1);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(147);
                {
                    let at = out.buf.len();
                    out.blit_ident(132);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(807, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(132);
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
                            out.blit(377, 3);
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
                            out.blit(377, 3);
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
        out.blit(549, 2);
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
                            out.blit(551, 2);
                            {
                                let at = out.buf.len();
                                out.blit(934, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
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
                                    for field in variant.fields {
                                        {
                                            out.buf.push(TokenTree::from(field.name.clone()));
                                            out.blit_punct(1);
                                        }
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(551, 2);
                            {
                                let at = out.buf.len();
                                out.blit(934, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                {
                                    for field in variant.fields {
                                        binary_encode_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.buf.push(TokenTree::from(field.name.clone()));
                                            }),
                                        )
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::None => {
                            out.blit(551, 2);
                            {
                                let at = out.buf.len();
                                out.blit(934, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
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
    impl_to_binary(out, ctx, body)
}
fn enum_from_binary(
    out: &mut RustWriter,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.buf.len();
        out.blit(937, 5);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        {
                            if i + 1 == variants.len() {
                                {
                                    out.blit_ident(138);
                                }
                            } else {
                                {
                                    out.buf.push(TokenTree::from(
                                        Literal::u8_unsuffixed(i as u8).clone(),
                                    ));
                                }
                            }
                        };
                        out.blit(551, 2);
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
    match inner_derive(stream.clone()) {
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
