use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttrs, Generic,
    GenericKind, Tag, TraitSet, Via, ENUM_CONTAINS_STRUCT_VARIANT, ENUM_CONTAINS_TUPLE_VARIANT,
    ENUM_CONTAINS_UNIT_VARIANT, ENUM_HAS_EXTERNAL_TAG, FROM_BINARY, FROM_JSON, TO_BINARY, TO_JSON,
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
            buffer.blit_punct(2);
        }
        match generic.kind {
            GenericKind::Lifetime => {
                buffer.blit_punct(7);
            }
            GenericKind::Type => (),
            GenericKind::Const => {
                buffer.blit_ident(160);
            }
        }
        buffer.buf.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            buffer.blit_punct(9);
            buffer.buf.extend(generic.bounds.iter().cloned());
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
) -> Result<(), Error> {
    let any_generics = !target.generics.is_empty();
    {
        output.blit(0, 3);
        output.push_ident(lifetime);
        if !generics.is_empty() {
            output.blit_punct(2);
            fmt_generics(output, generics, DEF);
        };
        output.blit_punct(1);
        output.buf.extend_from_slice(&crate_path);
        output.blit(3, 2);
        if let Some(sub) = sub {
            output.push_ident(&sub);
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
            output.blit_ident(158);
            for ty in &target.generic_field_types {
                output.buf.extend_from_slice(ty);
                output.blit_punct(9);
                output.push_ident(&trait_name);
                output.blit(1, 2);
                output.push_ident(lifetime);
                output.blit(7, 2);
            }
            output.buf.extend_from_slice(&target.where_clauses);
        };
    };
    Ok(())
}
fn impl_from_binary(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    {
        output.blit_punct(8);
        {
            let at = output.buf.len();
            output.blit_ident(153);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit_ident(168);
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
                output.push_ident(&ctx.lifetime);
                output.blit_punct(1);
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
        output.blit_ident(168);
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
            ty(output);
            output.blit(29, 4);
            {
                let at = output.buf.len();
                output.blit(33, 18);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(51, 8);
                output.push_ident(&ctx.lifetime);
                output.blit_punct(1);
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
        output.blit_punct(8);
        {
            let at = output.buf.len();
            output.blit_ident(153);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit_ident(168);
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
                output.push_ident(&ctx.lifetime);
                output.blit_punct(1);
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
        output.blit_punct(8);
        {
            let at = output.buf.len();
            output.blit_ident(153);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit(114, 2);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, DEF);
            output.blit_punct(1);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(116, 4);
        output.push_ident(&target.name);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, USE);
            output.blit_punct(1);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(158);
            {
                for ty in &target.generic_field_types {
                    output.buf.extend_from_slice(ty);
                    output.blit(120, 3);
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
        output.blit_punct(8);
        {
            let at = output.buf.len();
            output.blit_ident(153);
            output.tt_group(Delimiter::Bracket, at);
        };
        output.blit_ident(146);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, DEF);
            output.blit_punct(1);
        };
        output.buf.extend_from_slice(&crate_path);
        output.blit(138, 4);
        output.push_ident(&target.name);
        if any_generics {
            output.blit_punct(3);
            fmt_generics(output, &target.generics, USE);
            output.blit_punct(1);
        };
        if !target.where_clauses.is_empty() || !target.generic_field_types.is_empty() {
            output.blit_ident(158);
            {
                for ty in &target.generic_field_types {
                    output.buf.extend_from_slice(ty);
                    output.blit_punct(9);
                    output.buf.extend_from_slice(&crate_path);
                    output.blit(142, 4);
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
                        output.blit_ident(172);
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
            } else {
                output.blit_punct(3);
                output.buf.extend_from_slice(field.ty);
                output.blit_ident(172);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(176, 4);
            }
        };
        output.blit(180, 3);
        {
            let at = output.buf.len();
            place(output);
            output.blit(127, 2);
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
    {
        {
            if let Some(path) = field.with(FROM_BINARY) {
                out.buf.extend_from_slice(path);
            } else {
                {
                    out.blit_punct(3);
                    out.buf.extend_from_slice(field.ty);
                    out.blit_ident(172);
                    ctx.FromBinary(out);
                    out.blit_punct(1);
                }
            }
        };
        out.blit(183, 3);
        {
            let at = out.buf.len();
            out.blit_ident(113);
            out.tt_group(Delimiter::Parenthesis, at);
        };
    };
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        out.buf.extend_from_slice(&self.crate_path);
        out.blit(186, 5);
        out.push_ident(&self.lifetime);
        out.blit_punct(1);
    }
}
fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    if let Some(with) = field.with(FROM_JSON) {
        {
            out.buf.extend_from_slice(&ctx.crate_path);
            out.blit(191, 22);
            out.push_ident(&ctx.lifetime);
            out.blit(7, 2);
            out.buf.extend_from_slice(field.ty);
            out.blit(213, 3);
            {
                let at = out.buf.len();
                out.blit_punct(4);
                out.buf.extend_from_slice(with);
                out.blit(216, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
        }
    } else {
        out.buf.extend_from_slice(&ctx.crate_path);
        out.blit(219, 10);
        out.push_ident(&ctx.lifetime);
        out.blit_punct(2);
        out.buf.extend_from_slice(field.ty);
        out.blit(229, 2);
    }
    Ok(())
}
fn field_name_literal(ctx: &Ctx, field: &Field) -> Literal {
    if let Some(name) = field.attr.rename(FROM_JSON) {
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
    if let Some(name) = &field.attr.rename(TO_JSON) {
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
            out.blit(231, 6);
            {
                let at = out.buf.len();
                out.blit(237, 2);
                out.buf.push(field_name_literal(ctx, field).into());
                out.blit(239, 13);
                {
                    let at = out.buf.len();
                    {
                        if let Some(ty) = temp_tuple {
                            out.push_ident(&ty);
                            out.blit_punct(2);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                        } else {
                            out.push_ident(&ctx.target.name);
                            out.buf.push(ag_gen.clone());
                            out.blit_punct(2);
                            out.push_ident(field.name);
                        }
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(252, 3);
                if let Err(err) = schema_field_decode(out, ctx, field) {
                    return Err(err);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(2);
        }
        out.split_off_stream(len)
    };
    let schema_fields = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = {
        let len = out.buf.len();
        {
            for field in fields {
                out.blit(255, 12);
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
                        out.blit(267, 12);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(7, 2);
                    }
                    ast::DefaultKind::Custom(expr) => {
                        out.blit(279, 16);
                        {
                            let at = out.buf.len();
                            out.blit(295, 3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit_punct(5);
                            out.buf.extend_from_slice(expr);
                            out.blit(29, 2);
                            {
                                let at = out.buf.len();
                                out.blit(298, 6);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(179);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(304, 9);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit_punct(2);
                    }
                }
            }
        };
        out.split_off_stream(len)
    };
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    {
        out.blit(313, 9);
        {
            let at = out.buf.len();
            out.blit(322, 3);
            out.buf.push(schema_fields);
            out.blit(325, 4);
            out.buf.push(schema_drops);
            out.blit(329, 4);
            out.buf.push(schema_defaults);
            out.blit_punct(2);
            out.tt_group(Delimiter::Brace, at);
        };
    }
    Ok(())
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
                out.blit(333, 9);
                out.push_ident(&ctx.lifetime);
                out.blit(342, 5);
                {
                    let at = out.buf.len();
                    out.blit_ident(178);
                    if !match ctx.target.repr {
                        ast::Repr::Transparent | ast::Repr::C => true,
                        _ => false,
                    } {
                        out.blit(347, 2);
                        {
                            let at = out.buf.len();
                            out.blit(242, 10);
                            {
                                let at = out.buf.len();
                                ctx.dead_target_type(out);
                                out.blit_punct(2);
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
                "FromJson not implemented for Tuples with multiple fields yet.",
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
                out.blit(349, 3);
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
                        out.buf.extend_from_slice(with);
                        out.blit(352, 3);
                    } else {
                        out.blit_punct(3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(355, 11);
                    }
                };
                {
                    let at = out.buf.len();
                    out.blit(366, 3);
                    out.buf
                        .push(TokenTree::Literal(Literal::usize_unsuffixed(0)));
                    out.blit(161, 2);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            ToJsonKind::Forward(field)
        }
        _ => {
            let mut first = true;
            {
                out.blit(369, 5);
                for (i, field) in fields.iter().enumerate() {
                    {
                        if first {
                            first = false;
                        } else {
                            out.blit(374, 4);
                        }
                    };
                    {
                        if let Some(with) = field.with(TO_JSON) {
                            out.buf.extend_from_slice(with);
                            out.blit(352, 3);
                        } else {
                            out.blit_punct(3);
                            out.buf.extend_from_slice(field.ty);
                            out.blit(355, 11);
                        }
                    };
                    {
                        let at = out.buf.len();
                        out.blit(366, 3);
                        out.buf
                            .push(TokenTree::Literal(Literal::usize_unsuffixed(i)));
                        out.blit(161, 2);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
                out.blit(378, 4);
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
fn with_injected_closure_arg_type(out: &mut RustWriter, attr_value: &[Tok], ty: &[Tok]) {
    if let [Tok::Punct(bar1), Tok::Ident(binding), Tok::Punct(bar2), rest @ ..] = attr_value {
        if bar1.as_char() == '|' && bar2.as_char() == '|' {
            {
                out.blit_punct(14);
                out.push_ident(binding);
                out.blit(12, 2);
                out.buf.extend_from_slice(ty);
                out.blit_punct(14);
                out.buf.extend_from_slice(rest);
            };
            return;
        }
    }
    {
        out.buf.extend_from_slice(attr_value);
    }
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
                let if_skip_body = if let Some(skip_fn) = field.skip(TO_JSON) {
                    if skip_fn.is_empty() {
                        continue;
                    }
                    if !first {
                        text.push(',');
                    }
                    if !text.is_empty() {
                        if text == "," {
                            out.blit(382, 5);
                        } else {
                            {
                                out.blit(349, 3);
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
                    {
                        out.blit(387, 2);
                        {
                            let at = out.buf.len();
                            with_injected_closure_arg_type(out, skip_fn, &field.ty);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        {
                            let at = out.buf.len();
                            {
                                if on_self {
                                    out.blit(366, 3);
                                    out.push_ident(field.name);
                                } else {
                                    out.push_ident(&ctx.temp[i]);
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
                    if let Err(err) = field_name_json(ctx, field, text) {
                        return Err(err);
                    }
                    text.push(':');
                }
                if !text.is_empty() {
                    if text == "," {
                        out.blit(382, 5);
                    } else {
                        {
                            out.blit(349, 3);
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
                            out.blit_ident(145);
                            {
                                let at = out.buf.len();
                                out.blit(389, 3);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_ident(40);
                            {
                                let at = out.buf.len();
                                {
                                    if on_self {
                                        out.blit(366, 3);
                                        out.push_ident(field.name);
                                    } else {
                                        out.push_ident(&ctx.temp[i]);
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(392, 3);
                            {
                                let at = out.buf.len();
                                out.blit(395, 16);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(186);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(411, 9);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(186);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(420, 6);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        };
                    }
                } else {
                    if flattened {
                        out.blit(426, 18);
                    }
                    {
                        {
                            if let Some(with) = field.with(TO_JSON) {
                                out.buf.extend_from_slice(with);
                                out.blit(352, 3);
                            } else {
                                out.blit_punct(3);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(355, 11);
                            }
                        };
                        {
                            let at = out.buf.len();
                            {
                                if on_self {
                                    out.blit(366, 3);
                                    out.push_ident(field.name);
                                } else {
                                    out.push_ident(&ctx.temp[i]);
                                }
                            };
                            out.blit(161, 2);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                    };
                    if flattened {
                        out.blit(444, 5);
                    }
                }
                if let Some(from) = if_skip_body {
                    if !flattened {
                        out.blit(382, 5);
                    }
                    let inner =
                        TokenTree::Group(Group::new(Delimiter::Brace, out.split_off_stream(from)));
                    out.buf.push(inner);
                }
            }
        };
    };
    Ok(())
}
fn field_from_default(out: &mut RustWriter, field: &Field, set: TraitSet) {
    {
        {
            if let Some(explicit_default) = field.default(set) {
                {
                    {
                        let at = out.buf.len();
                        out.blit(449, 7);
                        out.buf.extend_from_slice(field.ty);
                        {
                            let at = out.buf.len();
                            {
                                match explicit_default {
                                    ast::DefaultKind::Default => {
                                        out.blit(456, 5);
                                    }
                                    ast::DefaultKind::Custom(expr) => {
                                        out.buf.extend_from_slice(expr);
                                    }
                                }
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(461, 3);
                        out.tt_group(Delimiter::Brace, at);
                    };
                }
            } else {
                out.blit(456, 5);
            }
        };
    };
}
fn struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut text = String::new();
    let body = {
        let len = out.buf.len();
        out.blit(464, 5);
        {
            inner_struct_to_json(out, ctx, fields, &mut text, true)?
        };
        out.blit(469, 4);
        out.split_off_stream(len)
    };
    impl_to_json(out, ToJsonKind::Static("AlwaysObject"), ctx, body)
}
fn struct_from_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut flattening: Option<&Field> = None;
    let mut has_skips = false;
    for field in fields {
        if field.flags & Field::WITH_FROM_JSON_SKIP != 0 {
            has_skips = true;
        }
        if field.flatten(FROM_JSON) {
            if flattening.is_some() {
                return Err(Error::span_msg(
                    "Only one flatten field is currently supported",
                    field.name.span(),
                ));
            }
            flattening = Some(field);
        }
    }
    let ordered_fields = schema_ordered_fields(fields);
    {
        if ctx.target.flattenable {
            out.blit(473, 6);
            out.push_ident(&ctx.lifetime);
            if !ctx.generics.is_empty() {
                out.blit_punct(2);
                fmt_generics(out, ctx.generics, DEF);
            };
            out.blit(479, 15);
            out.push_ident(&ctx.lifetime);
            out.blit_punct(1);
            if !ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty() {
                out.blit_ident(158);
                for ty in &ctx.target.generic_field_types {
                    out.buf.extend_from_slice(ty);
                    out.blit(338, 4);
                    out.push_ident(&ctx.lifetime);
                    out.blit_punct(1);
                }
            };
            {
                let at = out.buf.len();
                out.blit(494, 13);
                out.push_ident(&ctx.lifetime);
                out.blit_punct(1);
                {
                    let at = out.buf.len();
                    out.blit(507, 4);
                    {
                        let at = out.buf.len();
                        {
                            struct_schema(out, ctx, &ordered_fields, None)?
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(511, 12);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        };
        {
            let start = out.buf.len();
            if let Some(flatten_field) = flattening {
                {
                    out.blit(523, 5);
                    out.buf.extend_from_slice(flatten_field.ty);
                    out.blit(528, 14);
                    {
                        let at = out.buf.len();
                        out.blit(542, 3);
                        {
                            let at = out.buf.len();
                            out.blit(250, 2);
                            {
                                let at = out.buf.len();
                                ctx.dead_target_type(out);
                                out.blit_punct(2);
                                out.push_ident(flatten_field.name);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(545, 3);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                }
            }
            if has_skips {
                out.blit(548, 3);
            }
            if ctx.target.flattenable {
                {
                    out.blit(551, 5);
                    out.push_ident(&ctx.lifetime);
                    if !ctx.generics.is_empty() {
                        out.blit_punct(2);
                        fmt_generics(out, ctx.generics, USE);
                    };
                    out.blit(229, 2);
                }
            } else {
                {
                    out.blit(494, 13);
                    out.push_ident(&ctx.lifetime);
                    out.blit_punct(1);
                    {
                        let at = out.buf.len();
                        out.blit(507, 4);
                        {
                            let at = out.buf.len();
                            {
                                struct_schema(out, ctx, &ordered_fields, None)?
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(511, 12);
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
                out.blit_punct(13);
                {
                    if has_alias {
                        out.blit_ident(26);
                    } else {
                        out.blit_ident(131);
                    }
                };
                {
                    let at = out.buf.len();
                    out.blit(556, 4);
                    {
                        if flattening.is_some() {
                            {
                                out.blit_ident(143);
                                {
                                    let at = out.buf.len();
                                    out.blit(560, 3);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                            }
                        } else {
                            out.blit_ident(127);
                        }
                    };
                    if has_alias {
                        out.blit(100, 2);
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
                                                out.blit_punct(2);
                                                out.buf.push(TokenTree::Literal(alias.clone()));
                                                out.tt_group(Delimiter::Parenthesis, at);
                                            };
                                            out.blit_punct(2);
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
                    out.blit(563, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(567, 2);
                    {
                        let at = out.buf.len();
                        out.blit(569, 2);
                        {
                            let at = out.buf.len();
                            out.blit_ident(183);
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
                                out.blit(542, 3);
                                {
                                    let at = out.buf.len();
                                    out.blit(242, 10);
                                    {
                                        let at = out.buf.len();
                                        ctx.dead_target_type(out);
                                        out.blit_punct(2);
                                        out.push_ident(field.name);
                                        out.tt_group(Delimiter::Parenthesis, at);
                                    };
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit(571, 5);
                                out.buf.extend_from_slice(field.ty);
                                out.blit(576, 4);
                                {
                                    let at = out.buf.len();
                                    field_from_default(out, field, FROM_JSON);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                            }
                        }
                    };
                    out.blit_ident(181);
                    {
                        let at = out.buf.len();
                        out.tt_group_empty(Delimiter::Parenthesis);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                };
            }
            let ts = out.split_off_stream(start);
            impl_from_json(out, ctx, ts)?;
            if ctx.target.flattenable {
                if has_skips {
                    return Err(Error::msg(
                        "Flattenable does not yet support skipped fields",
                    ));
                }
                let body = {
                    let len = out.buf.len();
                    out.blit(580, 7);
                    {
                        let at = out.buf.len();
                        out.blit(587, 11);
                        out.push_ident(&ctx.lifetime);
                        if !ctx.generics.is_empty() {
                            out.blit_punct(2);
                            fmt_generics(out, ctx.generics, USE);
                        };
                        out.blit(598, 5);
                        out.buf.push(TokenTree::Literal(Literal::u64_unsuffixed(0)));
                        out.blit(603, 3);
                        out.buf.push(TokenTree::Literal(Literal::u64_unsuffixed(
                            required_bitset(&ordered_fields),
                        )));
                        out.blit_punct(2);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.split_off_stream(len)
                };
                impl_from_json_field_visitor(
                    out,
                    ctx,
                    &(|out| {
                        out.blit(606, 9);
                        out.push_ident(&ctx.lifetime);
                        out.blit_punct(1);
                    }),
                    body,
                )?;
            }
        };
    };
    Ok(())
}
fn variant_key_literal(ctx: &Ctx, variant: &EnumVariant) -> Literal {
    if let Some(name) = variant.attr.rename(FROM_JSON) {
        return name.clone();
    }
    if ctx.target.rename_all != RenameRule::None {
        Literal::string(
            &ctx.target
                .rename_all
                .apply_to_field(&variant.name.to_string()),
        )
    } else {
        Literal::string(&variant.name.to_string())
    }
}
fn enum_to_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let mut text = String::with_capacity(64);
    let start = out.buf.len();
    let all_objects =
        ctx.target.enum_flags & (ENUM_CONTAINS_UNIT_VARIANT | ENUM_CONTAINS_TUPLE_VARIANT) == 0;
    if let Tag::Inline(tag_name) = &ctx.target.tag {
        {
            out.blit(464, 5);
        };
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        {
            out.blit(349, 3);
            {
                let at = out.buf.len();
                out.buf.push(Literal::string(&text).into());
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
        };
    } else if all_objects {
        out.blit(464, 5);
    }
    {
        out.blit(615, 2);
        {
            let at = out.buf.len();
            {
                for (_, variant) in variants.iter().enumerate() {
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
                                        out.blit_punct(2);
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(617, 2);
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
                                        out.push_ident(field.name);
                                        out.blit_punct(9);
                                        out.push_ident(&ctx.temp[i]);
                                        out.blit_punct(2);
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(617, 2);
                            {
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                            };
                        }
                        EnumKind::None => {
                            out.blit(617, 2);
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
        out.blit(619, 5);
    } else if all_objects {
        out.blit(619, 5);
    }
    {
        out.blit(624, 5);
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
                    out.blit(464, 5);
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
        {
            match ctx.target.tag {
                Tag::Untagged => {
                    out.blit(619, 5);
                }
                Tag::Inline(..) if ctx.target.content.is_some() => {
                    out.blit(619, 5);
                }
                Tag::Default => {
                    out.blit(619, 5);
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
                    out.blit(464, 5);
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
                    out.blit(349, 3);
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
                        out.blit(352, 3);
                    } else {
                        out.blit_punct(3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(355, 11);
                    }
                };
                {
                    let at = out.buf.len();
                    out.push_ident(&ctx.temp[0]);
                    out.blit(161, 2);
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
                out.blit(349, 3);
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
                    out.blit(619, 5);
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
                    "Only one flatten field is currently supported",
                    field.name.span(),
                ));
            }
            flattening = Some(field);
        }
    }
    if let Some(flatten_field) = flattening {
        {
            out.blit(629, 2);
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
                    out.blit_punct(2);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(631, 13);
            {
                let at = out.buf.len();
                out.blit(644, 3);
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
                out.blit(647, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(660, 40);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(700, 11);
            out.buf.extend_from_slice(flatten_field.ty);
            out.blit(528, 14);
            {
                let at = out.buf.len();
                out.blit(711, 12);
                {
                    let at = out.buf.len();
                    out.blit(723, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(545, 3);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if let Tag::Inline(tag_name) = &ctx.target.tag {
                if ctx.target.content.is_none() {
                    out.blit(730, 11);
                    {
                        let at = out.buf.len();
                        out.blit(741, 2);
                        out.buf.push(Literal::string(tag_name).into());
                        out.blit(743, 4);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_punct(0);
                };
            };
            out.blit(564, 3);
            {
                let at = out.buf.len();
                out.blit_ident(151);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(747, 4);
            {
                let at = out.buf.len();
                out.blit(711, 12);
                {
                    let at = out.buf.len();
                    out.blit(751, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(758, 4);
                {
                    let at = out.buf.len();
                    out.blit(762, 3);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(2);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(151);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(132);
            {
                let at = out.buf.len();
                out.blit(765, 14);
                ctx.target_type(out);
                out.blit(576, 4);
                {
                    let at = out.buf.len();
                    out.push_ident(&ctx.target.name);
                    out.blit(3, 2);
                    out.push_ident(variant.name);
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.push_ident(field.name);
                            out.blit(779, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(2);
                        }
                        out.push_ident(flatten_field.name);
                        out.blit(782, 6);
                        {
                            for field in variant.fields {
                                if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                                    continue;
                                }
                                {
                                    out.push_ident(field.name);
                                    out.blit_punct(9);
                                    field_from_default(out, field, FROM_JSON);
                                    out.blit_punct(2);
                                }
                            }
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                if untagged || ctx.target.ignore_tag_adjacent_fields {
                    out.blit(788, 3);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
    } else {
        {
            out.blit(629, 2);
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
                    out.blit_punct(2);
                }
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(631, 13);
            {
                let at = out.buf.len();
                out.blit(644, 3);
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
                out.blit(647, 13);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(791, 27);
            {
                let at = out.buf.len();
                out.blit_ident(151);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(747, 4);
            {
                let at = out.buf.len();
                out.blit(711, 12);
                {
                    let at = out.buf.len();
                    out.blit(751, 7);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(818, 5);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            {
                let at = out.buf.len();
                if !untagged {
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(151);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(132);
            {
                let at = out.buf.len();
                out.blit(765, 14);
                ctx.target_type(out);
                out.blit(576, 4);
                {
                    let at = out.buf.len();
                    out.push_ident(&ctx.target.name);
                    out.blit(3, 2);
                    out.push_ident(variant.name);
                    {
                        let at = out.buf.len();
                        for (i, field) in ordered_fields.iter().enumerate() {
                            out.push_ident(field.name);
                            out.blit(779, 3);
                            out.buf.push(Literal::usize_unsuffixed(i).into());
                            out.blit_punct(2);
                        }
                        {
                            for field in variant.fields {
                                if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                                    continue;
                                }
                                {
                                    out.push_ident(field.name);
                                    out.blit_punct(9);
                                    field_from_default(out, field, FROM_JSON);
                                    out.blit_punct(2);
                                }
                            }
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                if untagged || ctx.target.ignore_tag_adjacent_fields {
                    out.blit(788, 3);
                };
                out.tt_group(Delimiter::Brace, at);
            };
        }
    };
    Ok(())
}
fn other_variant_key(out: &mut RustWriter, field: &Field) {
    {
        out.blit(823, 4);
        {
            let at = out.buf.len();
            out.blit(827, 2);
            {
                let at = out.buf.len();
                out.blit(829, 5);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit(834, 6);
        out.buf.extend_from_slice(field.ty);
        out.blit(840, 14);
        {
            let at = out.buf.len();
            out.blit(854, 9);
            out.tt_group(Delimiter::Parenthesis, at);
        };
        {
            let at = out.buf.len();
            out.blit_ident(181);
            {
                let at = out.buf.len();
                out.blit_ident(179);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(863, 5);
            {
                let at = out.buf.len();
                out.blit_ident(183);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(868, 4);
            {
                let at = out.buf.len();
                out.blit_ident(183);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_punct(0);
    };
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
                    out.blit(872, 3);
                    {
                        let at = out.buf.len();
                        out.blit(569, 2);
                        {
                            let at = out.buf.len();
                            out.blit(875, 10);
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
                out.blit_ident(177);
                {
                    if let Some(with) = field.with(FROM_JSON) {
                        out.buf.extend_from_slice(with);
                    } else {
                        out.blit_punct(3);
                        out.buf.extend_from_slice(field.ty);
                        out.blit(333, 9);
                        out.push_ident(&ctx.lifetime);
                        out.blit(342, 2);
                    }
                };
                out.blit(216, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(185);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(181);
                    {
                        let at = out.buf.len();
                        out.blit_ident(179);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(617, 2);
                    {
                        let at = out.buf.len();
                        out.blit(773, 6);
                        ctx.target_type(out);
                        out.blit(576, 4);
                        {
                            let at = out.buf.len();
                            out.push_ident(&ctx.target.name);
                            out.blit(3, 2);
                            out.push_ident(variant.name);
                            {
                                let at = out.buf.len();
                                out.blit_ident(179);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        if untagged || ctx.target.ignore_tag_adjacent_fields {
                            out.blit(788, 3);
                        };
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(866, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(151);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(617, 2);
                    {
                        let at = out.buf.len();
                        if !untagged {
                            out.blit(569, 2);
                            {
                                let at = out.buf.len();
                                out.blit_ident(151);
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
                    out.blit(872, 3);
                    {
                        let at = out.buf.len();
                        out.blit(569, 2);
                        {
                            let at = out.buf.len();
                            out.blit(875, 10);
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
                            out.blit(564, 3);
                            {
                                let at = out.buf.len();
                                out.blit_ident(183);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(885, 5);
                            {
                                let at = out.buf.len();
                                out.blit(569, 2);
                                {
                                    let at = out.buf.len();
                                    out.blit_ident(183);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    }
                }
            };
            out.blit(773, 6);
            ctx.target_type(out);
            out.blit(576, 4);
            {
                let at = out.buf.len();
                out.push_ident(&ctx.target.name);
                out.blit(3, 2);
                out.push_ident(variant.name);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(0);
            if untagged || ctx.target.ignore_tag_adjacent_fields {
                out.blit(788, 3);
            };
        }
    };
    out.tt_group(Delimiter::Brace, start);
    Ok(())
}
fn stringly_enum_from_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let mut other: Option<&EnumVariant> = None;
    for variant in variants {
        if variant.attr.has_other() {
            if other.is_some() {
                return Err(Error::span_msg(
                    "Only one other variant is currently supported.",
                    variant.name.span(),
                ));
            }
            other = Some(variant);
        }
    }
    let body = {
        let len = out.buf.len();
        out.blit(890, 5);
        {
            let at = out.buf.len();
            out.blit_ident(181);
            {
                let at = out.buf.len();
                out.blit_ident(171);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(617, 2);
            {
                let at = out.buf.len();
                out.blit(895, 5);
                {
                    let at = out.buf.len();
                    for variant in variants {
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit(617, 2);
                        out.push_ident(&ctx.target.name);
                        out.blit(3, 2);
                        out.push_ident(&variant.name);
                        out.blit_punct(2);
                    }
                    if let Err(err) = enum_from_json_unknown_variant(out, ctx, other, true) {
                        return Err(err);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(772, 7);
                ctx.target_type(out);
                out.blit(576, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(179);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_ident(187);
            {
                let at = out.buf.len();
                out.blit_ident(183);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(868, 4);
            {
                let at = out.buf.len();
                out.blit_ident(183);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit_punct(2);
            out.tt_group(Delimiter::Brace, at);
        };
        out.blit_ident(181);
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
) -> Result<(), Error> {
    {
        out.blit(900, 3);
    };
    let start = out.buf.len();
    if let Some(other) = &other {
        match other.fields {
            [] => (),
            [field] => other_variant_key(out, field),
            [_f1, f2, ..] => {
                return Err(Error::span_msg(
                    "Other variants may only have upto a single field",
                    f2.name.span(),
                ))
            }
        }
        if !stringly {
            let start = out.buf.len();
            {
                out.blit(564, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(885, 5);
                {
                    let at = out.buf.len();
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
            };
            if ctx.target.content.is_some() {
                let skipbody = out.split_off_stream(start);
                out.blit(903, 2);
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
                            out.blit_ident(119);
                            out.tt_group(Delimiter::Parenthesis, at);
                        }
                        EnumKind::Struct => {
                            let at = out.buf.len();
                            out.push_ident(field.name);
                            out.blit(905, 2);
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
                out.blit(773, 6);
                ctx.target_type(out);
                out.blit(576, 4);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Parenthesis, value)));
                out.blit_punct(0);
            }
        }
    } else {
        if ctx.target.ignore_tag_adjacent_fields && !stringly {
            {
                out.blit(564, 3);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(885, 5);
                {
                    let at = out.buf.len();
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(907, 5);
                {
                    let at = out.buf.len();
                    out.blit_ident(181);
                    {
                        let at = out.buf.len();
                        out.blit_ident(127);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(868, 4);
                    {
                        let at = out.buf.len();
                        out.blit(912, 8);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(920, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(143);
                        {
                            let at = out.buf.len();
                            out.blit_ident(96);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(617, 2);
                    {
                        let at = out.buf.len();
                        out.blit(922, 6);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit(866, 2);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(868, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(2);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_punct(0);
            }
        } else {
            {
                out.blit(928, 11);
                {
                    let at = out.buf.len();
                    out.blit_ident(171);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(939, 3);
                {
                    let at = out.buf.len();
                    out.blit(942, 10);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
            }
        }
    }
    out.tt_group(Delimiter::Brace, start);
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
            let outer = out.buf.len();
            {
                out.blit(952, 11);
            };
            let start = out.buf.len();
            'always_succeed: {
                for (i, variant) in variants.iter().enumerate() {
                    if let EnumKind::None = variant.kind {
                        {
                            out.blit(773, 6);
                            ctx.target_type(out);
                            out.blit(576, 4);
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
                            out.blit(963, 3);
                            {
                                let at = out.buf.len();
                                out.blit(966, 2);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit_punct(0);
                        };
                        if let Err(err) = enum_variant_from_json(out, ctx, variant, true) {
                            return Err(err);
                        };
                    }
                }
                {
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit(968, 10);
                        {
                            let at = out.buf.len();
                            out.blit(978, 2);
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
                out.blit_ident(181);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
            };
            let ts = out.split_off_stream(outer);
            impl_from_json(out, ctx, ts)?;
            return Ok(());
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
    let mut other: Option<&EnumVariant> = None;
    for variant in variants {
        if variant.attr.has_other() {
            if other.is_some() {
                return Err(Error::span_msg(
                    "Only one other variant is currently supported.",
                    variant.name.span(),
                ));
            }
            other = Some(variant);
        }
    }
    let body_start = out.buf.len();
    if let Some(tag) = inline_tag {
        {
            if ctx.target.content.is_some() {
                out.blit(980, 5);
            };
            out.blit(985, 6);
            {
                if let Some(content) = &ctx.target.content {
                    {
                        out.blit_ident(61);
                        {
                            let at = out.buf.len();
                            out.buf.push(Literal::string(tag).into());
                            out.blit_punct(2);
                            out.buf.push(Literal::string(content).into());
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                    }
                } else {
                    {
                        out.blit_ident(62);
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
                            out.blit_ident(181);
                            {
                                let at = out.buf.len();
                                {
                                    let at = out.buf.len();
                                    out.blit(991, 3);
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(617, 2);
                            {
                                let at = out.buf.len();
                                out.blit(994, 5);
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                    } else {
                        {
                            out.blit_ident(181);
                            {
                                let at = out.buf.len();
                                out.blit_ident(179);
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(863, 4);
                        }
                    }
                };
                out.blit_ident(187);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(868, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(2);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    } else {
        {
            out.blit(999, 2);
            {
                let at = out.buf.len();
                out.blit_ident(143);
                {
                    let at = out.buf.len();
                    if ctx.target.ignore_tag_adjacent_fields {
                        out.blit_ident(182);
                    };
                    out.blit_ident(171);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(885, 3);
            {
                if mixed_strings_and_objects {
                    out.blit_ident(33);
                } else {
                    out.blit_ident(32);
                }
            };
            out.blit(1001, 2);
            {
                let at = out.buf.len();
                out.blit(569, 2);
                {
                    let at = out.buf.len();
                    out.blit(1003, 10);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(0);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    }
    if ctx.target.ignore_tag_adjacent_fields {
        out.blit(1013, 4);
    }
    let variant_dispatch_start = out.buf.len();
    {
        out.blit(898, 2);
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
                        out.blit(617, 2);
                        if let Err(err) = enum_variant_from_json(out, ctx, variant, false) {
                            return Err(err);
                        };
                        out.blit_punct(2);
                    };
                }
            };
            if let Err(err) = enum_from_json_unknown_variant(out, ctx, other, false) {
                return Err(err);
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
    if let Some(_) = inline_tag {
        if ctx.target.content.is_some() {
            {
                out.blit(903, 2);
                {
                    let at = out.buf.len();
                    out.blit(1017, 4);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(132);
                {
                    let at = out.buf.len();
                    out.blit_ident(181);
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
                out.blit_ident(181);
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
            out.blit(1021, 5);
            {
                let at = out.buf.len();
                out.blit_ident(181);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1026, 4);
                {
                    let at = out.buf.len();
                    out.tt_group_empty(Delimiter::Parenthesis);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1030, 4);
                {
                    let at = out.buf.len();
                    out.blit(1034, 10);
                    ctx.target_type(out);
                    out.blit_punct(1);
                    {
                        let at = out.buf.len();
                        out.blit(1044, 7);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(1051, 3);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_punct(2);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit_punct(0);
        }
    } else {
        {
            out.blit(1054, 8);
            {
                let at = out.buf.len();
                out.blit_ident(181);
                {
                    let at = out.buf.len();
                    out.blit_ident(127);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(617, 2);
                {
                    let at = out.buf.len();
                    out.blit(1028, 2);
                    {
                        let at = out.buf.len();
                        out.tt_group_empty(Delimiter::Parenthesis);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(866, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(617, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(187);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(920, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(143);
                    {
                        let at = out.buf.len();
                        out.blit_ident(169);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(617, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(187);
                    {
                        let at = out.buf.len();
                        out.blit(1062, 8);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_punct(2);
                out.tt_group(Delimiter::Brace, at);
            };
            out.blit(1070, 11);
            ctx.target_type(out);
            out.blit_punct(1);
            {
                let at = out.buf.len();
                out.blit(1044, 7);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1081, 4);
        }
    }
    let mut body = out.split_off_stream(body_start);
    if mixed_strings_and_objects {
        body = {
            let len = out.buf.len();
            out.blit(1085, 5);
            {
                let at = out.buf.len();
                out.blit_ident(181);
                {
                    let at = out.buf.len();
                    out.blit(1090, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(617, 2);
                out.buf
                    .push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
                out.blit(920, 2);
                {
                    let at = out.buf.len();
                    out.blit(1102, 12);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(1114, 4);
                {
                    let at = out.buf.len();
                    out.blit(1118, 4);
                    out.tt_group(Delimiter::Brace, at);
                };
                {
                    let at = out.buf.len();
                    out.blit_ident(181);
                    {
                        let at = out.buf.len();
                        out.blit_ident(171);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(617, 2);
                    {
                        let at = out.buf.len();
                        out.blit(895, 5);
                        {
                            let at = out.buf.len();
                            {
                                for variant in variants {
                                    if let EnumKind::None = variant.kind {
                                        out.buf.push(variant_key_literal(ctx, variant).into());
                                        out.blit(617, 2);
                                        out.push_ident(&ctx.target.name);
                                        out.blit(3, 2);
                                        out.push_ident(variant.name);
                                        out.blit_punct(2);
                                    }
                                }
                            };
                            if let Err(err) = enum_from_json_unknown_variant(out, ctx, other, true)
                            {
                                return Err(err);
                            };
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.blit(772, 7);
                        ctx.target_type(out);
                        out.blit(576, 4);
                        {
                            let at = out.buf.len();
                            out.blit_ident(179);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit(1122, 3);
                        {
                            let at = out.buf.len();
                            out.tt_group_empty(Delimiter::Parenthesis);
                            out.tt_group(Delimiter::Parenthesis, at);
                        };
                        out.blit_punct(0);
                        out.tt_group(Delimiter::Brace, at);
                    };
                    out.blit_ident(187);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit(868, 4);
                    {
                        let at = out.buf.len();
                        out.blit_ident(183);
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(2);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit(920, 2);
                {
                    let at = out.buf.len();
                    out.blit_ident(169);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(617, 2);
                {
                    let at = out.buf.len();
                    out.blit(569, 2);
                    {
                        let at = out.buf.len();
                        out.blit(1125, 8);
                        {
                            let at = out.buf.len();
                            out.blit(978, 2);
                            out.buf.push(
                                Literal::string("Expected either an object or a string").into(),
                            );
                            out.blit_punct(2);
                            out.tt_group(Delimiter::Brace, at);
                        };
                        out.tt_group(Delimiter::Parenthesis, at);
                    };
                    out.blit_punct(0);
                    out.tt_group(Delimiter::Brace, at);
                };
                out.blit_ident(187);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit(868, 4);
                {
                    let at = out.buf.len();
                    out.blit_ident(183);
                    out.tt_group(Delimiter::Parenthesis, at);
                };
                out.blit_punct(2);
                out.tt_group(Delimiter::Brace, at);
            };
            out.split_off_stream(len)
        }
    }
    impl_from_json(out, ctx, body)?;
    Ok(())
}
fn handle_struct(
    output: &mut RustWriter,
    target: &DeriveTargetInner,
    fields: &[Field],
) -> Result<(), Error> {
    let ctx = Ctx::new(output, target)?;
    if target.from_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                return Err(Error::msg(
                    "Struct must contain a single field to use transparent",
                ));
            };
            if !match target.repr {
                ast::Repr::Transparent => true,
                _ => false,
            } {
                return Err(Error::msg(
                    "transparent FromJson requires #[repr(transparent)]",
                ));
            }
            let body = {
                let len = output.buf.len();
                output.blit_punct(3);
                output.buf.extend_from_slice(single_field.ty);
                output.blit_ident(172);
                output.buf.extend_from_slice(&ctx.crate_path);
                output.blit(1133, 7);
                {
                    let at = output.buf.len();
                    output.blit(556, 3);
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.split_off_stream(len)
            };
            impl_from_json(output, &ctx, body)?;
        } else {
            struct_from_json(output, &ctx, fields)?;
        }
    }
    if target.to_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                return Err(Error::msg(
                    "Struct must contain a single field to use transparent",
                ));
            };
            let body = {
                let len = output.buf.len();
                output.blit(367, 2);
                output.push_ident(single_field.name);
                output.blit(409, 2);
                {
                    let at = output.buf.len();
                    output.blit_ident(186);
                    output.tt_group(Delimiter::Parenthesis, at);
                };
                output.split_off_stream(len)
            };
            impl_to_json(output, ToJsonKind::Forward(&single_field), &ctx, body)?;
        } else {
            struct_to_json(output, &ctx, fields)?;
        }
    }
    if target.to_binary {
        let body = {
            let len = output.buf.len();
            {
                for field in fields {
                    encode_binary_field(
                        output,
                        &ctx,
                        field,
                        &(|out| {
                            out.blit(366, 3);
                            out.push_ident(field.name);
                        }),
                    )
                }
            };
            output.split_off_stream(len)
        };
        impl_to_binary(output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.buf.len();
            output.push_ident(&target.name);
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            (output).push_ident(field.name);
                            (output).blit_punct(9);
                            decode_binary_field(output, &ctx, field);
                            (output).blit_punct(2);
                        }
                    }
                };
                output.tt_group(Delimiter::Brace, at);
            };
            output.split_off_stream(len)
        };
        impl_from_binary(output, &ctx, body)?;
    }
    Ok(())
}
fn handle_tuple_struct(
    output: &mut RustWriter,
    target: &DeriveTargetInner,
    fields: &[Field],
) -> Result<(), Error> {
    let ctx = Ctx::new(output, target)?;
    if target.from_json {
        tuple_struct_from_json(output, &ctx, fields)?;
    }
    if target.to_json {
        tuple_struct_to_json(output, &ctx, fields)?;
    }
    if target.to_binary {
        let body = {
            let len = output.buf.len();
            {
                for (i, field) in fields.iter().enumerate() {
                    encode_binary_field(
                        output,
                        &ctx,
                        field,
                        &(|out| {
                            out.blit(366, 3);
                            out.buf
                                .push(TokenTree::Literal(Literal::usize_unsuffixed(i)));
                        }),
                    )
                }
            };
            output.split_off_stream(len)
        };
        impl_to_binary(output, &ctx, body)?;
    }
    if target.from_binary {
        let body = {
            let len = output.buf.len();
            output.push_ident(&target.name);
            {
                let at = output.buf.len();
                {
                    for field in fields {
                        {
                            decode_binary_field(output, &ctx, field);
                            (output).blit_punct(2);
                        }
                    }
                };
                output.tt_group(Delimiter::Parenthesis, at);
            };
            output.split_off_stream(len)
        };
        impl_from_binary(output, &ctx, body)?;
    }
    Ok(())
}
fn enum_to_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let target = &ctx.target;
    let any_generics = !target.generics.is_empty();
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        return Err(Error::msg(
            "FromStr enum must not have any tuple or struct variants nor a tag configuratino",
        ));
    }
    {
        out.blit_punct(8);
        {
            let at = out.buf.len();
            out.blit_ident(153);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit_ident(146);
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
            out.blit(1140, 2);
            out.buf.extend_from_slice(&target.where_clauses);
        };
        {
            let at = out.buf.len();
            out.blit(1142, 3);
            {
                let at = out.buf.len();
                out.blit(125, 2);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1145, 6);
            {
                let at = out.buf.len();
                out.blit(615, 2);
                {
                    let at = out.buf.len();
                    for variant in variants {
                        out.push_ident(&target.name);
                        out.blit(3, 2);
                        out.push_ident(variant.name);
                        out.blit(617, 2);
                        out.buf.push(variant_key_literal(ctx, variant).into());
                        out.blit_punct(2);
                    }
                    out.tt_group(Delimiter::Brace, at);
                };
                out.tt_group(Delimiter::Brace, at);
            };
            out.tt_group(Delimiter::Brace, at);
        };
    };
    Ok(())
}
fn enum_from_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let target = &ctx.target;
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        return Err(Error::msg(
            "FromStr enum must not have any tuple or struct variants nor a tag configuratino",
        ));
    }
    let any_generics = !target.generics.is_empty();
    {
        out.blit_punct(8);
        {
            let at = out.buf.len();
            out.blit_ident(153);
            out.tt_group(Delimiter::Bracket, at);
        };
        out.blit_ident(146);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, DEF);
            out.blit_punct(1);
        };
        out.blit(1151, 10);
        out.push_ident(&target.name);
        if any_generics {
            out.blit_punct(3);
            fmt_generics(out, &target.generics, USE);
            out.blit_punct(1);
        };
        if !target.where_clauses.is_empty() {
            out.blit(1140, 2);
            out.buf.extend_from_slice(&target.where_clauses);
        };
        {
            let at = out.buf.len();
            out.blit(1161, 15);
            {
                let at = out.buf.len();
                out.blit(1176, 4);
                out.tt_group(Delimiter::Parenthesis, at);
            };
            out.blit(1180, 16);
            {
                let at = out.buf.len();
                out.blit_ident(181);
                {
                    let at = out.buf.len();
                    out.blit(1196, 2);
                    {
                        let at = out.buf.len();
                        for variant in variants {
                            out.buf.push(variant_key_literal(ctx, variant).into());
                            out.blit(617, 2);
                            out.push_ident(&target.name);
                            out.blit(3, 2);
                            out.push_ident(variant.name);
                            out.blit_punct(2);
                        }
                        out.blit(1198, 5);
                        {
                            let at = out.buf.len();
                            out.blit(1164, 9);
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
    Ok(())
}
fn enum_to_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    let body = {
        let len = out.buf.len();
        out.blit(615, 2);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
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
                                        out.blit_punct(2);
                                    }
                                };
                                out.tt_group(Delimiter::Parenthesis, at);
                            };
                            out.blit(617, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1203, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf
                                        .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        encode_binary_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.push_ident(&ctx.temp[i]);
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
                                        out.push_ident(field.name);
                                        out.blit_punct(9);
                                        out.push_ident(&ctx.temp[i]);
                                        out.blit_punct(2);
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                            out.blit(617, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1203, 3);
                                {
                                    let at = out.buf.len();
                                    out.buf
                                        .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                                    out.tt_group(Delimiter::Parenthesis, at);
                                };
                                out.blit_punct(0);
                                {
                                    for (i, field) in variant.fields.iter().enumerate() {
                                        encode_binary_field(
                                            out,
                                            ctx,
                                            field,
                                            &(|out| {
                                                out.push_ident(&ctx.temp[i]);
                                            }),
                                        )
                                    }
                                };
                                out.tt_group(Delimiter::Brace, at);
                            };
                        }
                        EnumKind::None => {
                            out.blit(617, 2);
                            {
                                let at = out.buf.len();
                                out.blit(1203, 3);
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
    impl_to_binary(out, ctx, body)
}
fn enum_from_binary(
    out: &mut RustWriter,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = {
        let len = out.buf.len();
        out.blit(1206, 5);
        {
            let at = out.buf.len();
            {
                for (i, variant) in variants.iter().enumerate() {
                    {
                        {
                            if i + 1 == variants.len() {
                                out.blit_ident(169);
                            } else {
                                out.buf
                                    .push(TokenTree::Literal(Literal::u8_unsuffixed(i as u8)));
                            }
                        };
                        out.blit(617, 2);
                        {
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
                                                    out.blit_punct(2);
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
                                                    out.blit_punct(2);
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
                        };
                        out.blit_punct(2);
                    }
                }
            };
            out.tt_group(Delimiter::Brace, at);
        };
        out.split_off_stream(len)
    };
    impl_from_binary(out, ctx, body)
}
fn handle_enum(
    output: &mut RustWriter,
    target: &DeriveTargetInner,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let mut ctx = Ctx::new(output, target)?;
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
    if target.from_str {
        enum_from_str(output, &ctx, variants)?;
    }
    if target.to_str {
        enum_to_str(output, &ctx, variants)?;
    }
    if target.to_json {
        enum_to_json(output, &ctx, variants)?;
    }
    if target.from_json {
        enum_from_json(output, &ctx, variants)?;
    }
    if target.to_binary {
        enum_to_binary(output, &ctx, variants)?;
    }
    if target.from_binary {
        enum_from_binary(output, &ctx, variants)?;
    }
    Ok(())
}
pub fn inner_derive(stream: TokenStream) -> Result<TokenStream, Error> {
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
        from_str: false,
        enum_flags: 0,
        content: None,
        flattenable: false,
        tag: Tag::Default,
        rename_all: crate::case::RenameRule::None,
        repr: ast::Repr::Default,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens)?;
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
            match ast::parse_struct_fields(&mut field_buf, &field_toks, &mut attr_buf) {
                Ok(_) => {
                    ast::scan_fields(&mut target, &mut field_buf);
                    handle_struct(&mut rust_writer, &target, &field_buf)?;
                }
                Err(err) => return Err(err),
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
                    handle_tuple_struct(&mut rust_writer, &target, &field_buf)?;
                }
                Err(err) => return Err(err),
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
                Ok(enums) => {
                    handle_enum(&mut rust_writer, &target, &enums)?;
                }
                Err(err) => return Err(err),
            }
        }
    }
    let ts = rust_writer.split_off_stream(0);
    Ok({
        let len = (&mut rust_writer).buf.len();
        (&mut rust_writer).blit_punct(8);
        {
            let at = (&mut rust_writer).buf.len();
            (&mut rust_writer).blit_ident(17);
            {
                let at = (&mut rust_writer).buf.len();
                (&mut rust_writer).blit(1211, 4);
                (&mut rust_writer).tt_group(Delimiter::Parenthesis, at);
            };
            (&mut rust_writer).tt_group(Delimiter::Bracket, at);
        };
        (&mut rust_writer).blit(1215, 5);
        (&mut rust_writer)
            .buf
            .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
        (&mut rust_writer).blit_punct(0);
        (&mut rust_writer).split_off_stream(len)
    })
}
pub fn derive(stream: TokenStream) -> TokenStream {
    match inner_derive(stream) {
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
