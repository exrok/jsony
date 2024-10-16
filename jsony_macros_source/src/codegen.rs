use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttr, Generic,
    GenericKind,
};
use crate::case::RenameRule;
use crate::util::MemoryPool;
use crate::Error;
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[rustfmt::skip]
macro_rules! throw {
    ($literal: literal @ $span: expr, $($tt:tt)*) => { return Err(Error::span_msg_ctx($literal, &($($tt)*), $span)) };
    ($literal: literal, $($tt:tt)*) => { return Err(Error::msg_ctx($literal, &($($tt)*))) };
    ($literal: literal @ $span: expr) => { return Err(Error::span_msg($literal, $span)) };
    ($literal: literal) => { return Err(Error::msg($literal)) };
}

#[allow(unused)]
enum StaticToken {
    Ident(&'static str),
    // bool: true if alonecarg
    Punct(char, bool),
}
#[allow(unused)]
use StaticToken::Ident as StaticIdent;
#[allow(unused)]
use StaticToken::Punct as StaticPunct;

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
fn tt_group(out: &mut Vec<TokenTree>, delimiter: Delimiter, from: usize) {
    let group = TokenTree::Group(Group::new(
        delimiter,
        TokenStream::from_iter(out.drain(from..)),
    ));
    out.push(group);
}

#[rustfmt::skip]
macro_rules! append_tok {
    ($ident:ident $d:tt) => { tt_ident($d, stringify!($ident)) };
    ({$($tt:tt)*} $d: tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* tt_group($d, Delimiter::Brace, at);
    }};
    (($($tt:tt)*) $d: tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* tt_group($d, Delimiter::Parenthesis, at);
    }};
    ([[$($tt:tt)*]] $d:tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* tt_group($d, Delimiter::Bracket, at);
    }};
    (_ $d:tt) => { tt_ident($d, "_") };
    ([$ident:ident] $d:tt) => {
        $d.push($($tt)*)
    };
    ([?($($cond:tt)*) $($body:tt)*] $d:tt) => {
        if $($cond)* { $(append_tok!($body $d);)* }
    };
    ([@$($tt:tt)*] $d:tt) => {
        $d.push($($tt)*)
    };
    ([try $($tt:tt)*] $d:tt) => {
        if let Err(err) = $($tt)* { return Err(err); }
    };
    ([for ($($iter:tt)*) {$($body:tt)*}] $d:tt) => {
        for $($iter)* { $(append_tok!($body $d);)* }
    };
    ([#$($tt:tt)*] $d:tt) => {
        $d.push(TokenTree::from($($tt)*.clone()))
    };
    ([~$($tt:tt)*] $d:tt) => {
        $d.extend_from_slice($($tt)*)
    };
    ([$($rust:tt)*] $d:tt) => {{
         $($rust)*
    }};
    (# $d:tt) => { tt_punct_joint($d, '\'') };
    (: $d:tt) => { tt_punct_alone($d, ':') };
    (~ $d:tt) => { tt_punct_joint($d, '-') };
    (< $d:tt) => { tt_punct_alone($d, '<') };
    (% $d:tt) => { tt_punct_joint($d, ':') };
    (:: $d:tt) => { tt_punct_joint($d, ':'); tt_punct_alone($d, ':'); };
    (-> $d:tt) => { tt_punct_joint($d, '-'); tt_punct_alone($d, '>'); };
    (=> $d:tt) => { tt_punct_joint($d, '='); tt_punct_alone($d, '>'); };
    (> $d:tt) => { tt_punct_alone($d, '>') };
    (! $d:tt) => { tt_punct_alone($d, '!') };
    (| $d:tt) => { tt_punct_alone($d, '|') };
    (. $d:tt) => { tt_punct_alone($d, '.') };
    (; $d:tt) => { tt_punct_alone($d, ';') };
    (& $d:tt) => { tt_punct_alone($d, '&') };
    (= $d:tt) => { tt_punct_alone($d, '=') };
    (, $d:tt) => { tt_punct_alone($d, ',') };
    (* $d:tt) => { tt_punct_alone($d, '*') };
}

macro_rules! splat { ($d:tt; $($tt:tt)*) => { { $(append_tok!($tt $d);)* } } }

macro_rules! token_stream { ($d:tt; $($tt:tt)*) => {{
    let len = $d.len(); $(append_tok!($tt $d);)* TokenStream::from_iter($d.drain(len..))
}}}

struct GenericBoundFormating {
    lifetimes: bool,
    bounds: bool,
}
fn fmt_generics(buffer: &mut Vec<TokenTree>, generics: &[Generic], fmt: GenericBoundFormating) {
    let mut first = true;
    for generic in generics {
        if !fmt.lifetimes && matches!(generic.kind, GenericKind::Lifetime) {
            continue;
        }
        if first {
            first = false;
        } else {
            append_tok!(,buffer);
        }
        match generic.kind {
            GenericKind::Lifetime => {
                append_tok!(#buffer);
            }
            GenericKind::Type => (),
            GenericKind::Const => {
                append_tok!(const buffer);
            }
        }
        buffer.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            append_tok!(: buffer);
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
    splat! {
        output;
        impl <#[#lifetime] [?(!generics.is_empty()), [fmt_generics(output, generics, DEF)]] >
         [~&crate_path]::[?(let Some(sub) = sub) [#sub]::][#trait_name]<#[#lifetime]> for [#target.name][?(any_generics) <
            [fmt_generics(output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [for (ty in &target.generic_field_types) {
                [~ty]: [#trait_name]<#[#lifetime]>,
            }] [~&target.where_clauses] ]
    };
    Ok(())
}
fn impl_from_binary(
    output: &mut Vec<TokenTree>,
    ctx: &Ctx,
    inner: TokenStream,
) -> Result<(), Error> {
    splat! {
        output;
        unsafe [try bodyless_impl_from(output, None, Ident::new("FromBinary", Span::call_site()), ctx)] {
            fn binary_decode(decoder: &mut [~&ctx.crate_path]::binary::Decoder<#[#ctx.lifetime]>) -> Self [
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
    Ok(())
}

fn impl_from_json_field_visitor(
    output: &mut Vec<TokenTree>,
    ctx: &Ctx,
    ty: &dyn Fn(&mut Vec<TokenTree>),
    inner: TokenStream,
) -> Result<(), Error> {
    splat! {
        output;
        unsafe [try bodyless_impl_from(
            output,
            Some(Ident::new("json", Span::call_site())),
            Ident::new("FromJsonFieldVisitor", Span::call_site()),
            ctx
        )] {
            type Vistor = [ty(output)];

            unsafe fn new_field_visitor(
                dst: ::std::ptr::NonNull<()>,
                parser: & [~&ctx.crate_path]::parser::Parser<#[#ctx.lifetime]>
            ) -> Self::Vistor [
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
    Ok(())
}

fn impl_from_json(output: &mut Vec<TokenTree>, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    splat! {
        output;
        unsafe [try bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)] {
            unsafe fn emplace_from_json(dst: ::std::ptr::NonNull<()>, parser: &mut [~&ctx.crate_path]::parser::Parser<#[#ctx.lifetime]>)
            -> ::std::result::Result<(), &#static ::jsony::json::DecodeError> [
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        output;
        unsafe impl [?(any_generics) < [fmt_generics(output, &target.generics, DEF)] >]
         [~&crate_path]::ToBinary for [#target.name][?(any_generics) <
            [fmt_generics( output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [
                for ty in &target.generic_field_types {
                    splat!(output; [~ty]: ToBinary,)
                }
             ]] {
            fn binary_encode(&self, encoder: &mut Vec<u8>) [
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        output;
        impl [?(any_generics) < [fmt_generics(output, &target.generics, DEF)] >]
         [~&crate_path]::ToJson for [#target.name][?(any_generics) <
            [fmt_generics( output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [
                for ty in &target.generic_field_types {
                    splat!(output; [~ty]: [~&crate_path]::ToBinary,)
                }
             ]] {
            type Kind = [~&crate_path]::json::[@Ident::new(kind, Span::call_site()).into()];
            fn jsonify_into(&self, out: &mut jsony::TextWriter) -> Self::Kind [
                output.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
        splat!(out; [#self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, DEAD_USE
        )]>])
    }
    pub fn target_type(&self, out: &mut Vec<TokenTree>) {
        splat!(out; [#self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, USE
        )]>])
    }
    fn new(target: &'a DeriveTargetInner) -> Result<Ctx<'a>, Error> {
        let crate_path = if let Some(value) = &target.path_override {
            let content = value.to_string();
            // assumes normal string and no escapes probalby shouldn't
            #[allow(unused)]
            let inner = &content[1..content.len() - 1];
            // TokenStream::from_str(inner)
            //     .unwrap()
            //     .into_iter()
            //     .collect::<Vec<TokenTree>>()
            let mut out = Vec::<TokenTree>::new();
            splat!((&mut out);::jsony);
            out
        } else {
            let mut out = Vec::<TokenTree>::new();
            splat!((&mut out);::jsony);
            out
        };
        let (lt, generics) = if let [Generic {
            kind: GenericKind::Lifetime,
            ident,
            bounds,
        }, rest @ ..] = &target.generics[..]
        {
            if !bounds.is_empty() {
                throw!("Bounded lifetimes currently unsupported")
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
    splat! {
        output;
        <[~field.ty] as [~&ctx.crate_path]::ToBinary>::binary_encode(
            [place(output)], encoder
        );
    };
}

fn binary_decode_field(out: &mut Vec<TokenTree>, ctx: &Ctx, field: &Field) {
    splat! {
        out;
        <[~field.ty] as [ctx.FromBinary(out)]>::binary_decode(
            decoder
        )
    };
}
impl Ctx<'_> {
    #[allow(non_snake_case)]
    fn FromJson(&self, out: &mut Vec<TokenTree>) {
        splat!(out;  [~&self.crate_path]::FromJson<#[#self.lifetime]>)
    }

    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut Vec<TokenTree>) {
        splat!(out;  [~&self.crate_path]::FromBinary<#[#self.lifetime]>)
    }
}

fn schema_field_decode(out: &mut Vec<TokenTree>, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    splat! { out; [~&ctx.crate_path]::__internal::erase(<[~field.ty] as [ctx.FromJson(out)]>::emplace_from_json) }
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

fn field_name_json(ctx: &Ctx, field: &Field, output: &mut String) -> Result<(), Error> {
    output.push('"');
    if let Some(name) = &field.attr.rename {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                output.push_str(&value);
            }
            _ => {
                throw!("Invalid rename value expected a string" @ name.span())
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
// fn unflattened_fields<'a>(
//     field: &'a [Field],
//     ctx: &'a Ctx<'a>,
// ) -> impl Iterator<Item = &'a Field<'a>> {
//     field
//         .iter()
//         .filter(move |f| ctx.attr(f).map_or(true, |a| !a.flatten))
// }

fn struct_schema(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    fields: &[&Field],
    temp_tuple: Option<&Ident>,
) -> Result<(), Error> {
    let ag_gen = if ctx
        .target
        .generics
        .iter()
        .any(|g| !matches!(g.kind, GenericKind::Lifetime))
    {
        let x = out.len();
        append_tok!(< out);
        fmt_generics(
            out,
            &ctx.target.generics,
            GenericBoundFormating {
                lifetimes: false,
                bounds: false,
            },
        );
        append_tok!(> out);
        TokenTree::Group(Group::new(Delimiter::None, out.drain(x..).collect()))
    } else {
        TokenTree::Group(Group::new(Delimiter::None, TokenStream::new()))
    };
    let ts = token_stream!(out; [
        for ((i, field) in fields.iter().enumerate()) {
            [~&ctx.crate_path]::__internal::Field {
                name: [@field_name_literal(ctx, field).into()],
                offset: ::std::mem::offset_of!([
                    if let Some(ty) = temp_tuple {
                        splat!(out; [#ty], [@Literal::usize_unsuffixed(i).into()])
                    } else {
                        splat!(out; [#ctx.target.name][#ag_gen], [#field.name])
                    }
                ]),
                decode: [try schema_field_decode(out, ctx, field)]
            },
        }
    ]);
    let schema_fields = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = token_stream!(out; [
        for field in fields {
            splat!{
                out;
                std::mem::transmute(
                    std::ptr::drop_in_place::<[~field.ty]> as unsafe fn(*mut [~field.ty])
                ),
            }
        }
    ]);
    let schema_drops = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = token_stream!(out; [
        for field in fields {
            let Some(default) = &field.attr.default else {
                break;
            };
            splat!{
                out;
                |ptr: ::std::ptr::NonNull<()>| {
                    //todo need to guard gaainst returns and such
                    let value: [~field.ty] = [~default];
                    unsafe {
                        ptr.cast().write(value);
                    }
                    ::jsony::__internal::UnsafeReturn
                },
            }
        }
    ]);
    let schema_defaults = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    splat! { out;
        ::jsony::__internal::ObjectSchemaInner {
            fields: &[@schema_fields],
            drops: &[@schema_drops],
            defaults: &[@schema_defaults],
        }
    }
    Ok(())
}

fn body_of_struct_from_json_with_flatten(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    flatten_field: &Field,
) -> TokenStream {
    token_stream!(
        out;
        let mut flatten_visitor =
            <[~flatten_field.ty] as ::jsony::json::FromJsonFieldVisitor>::new_field_visitor(
                dst.byte_add(offset_of!([ctx.dead_target_type(out)], [#flatten_field.name])),
                parser,
            );
        __schema_inner::<
            #[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
        >().decode(dst, parser, Some(&mut flatten_visitor))
    )
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
    let body = token_stream!(
        out;
        out.start_json_object();
        [for (field in fields) {
            out.push_str([
                text.clear();
                if first {
                    first = false;
                }  else {
                    text.push(',');
                }
                if let Err(err) = field_name_json(ctx, field, &mut text) {
                    return Err(err)
                }
                text.push(':');
                out.push(TokenTree::Literal(Literal::string(&text)));
            ]);
            <[~field.ty] as ::jsony::ToJson>::jsonify_into(&self.[#field.name], out);
        }]
        out.end_json_object()
    );

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

    splat!(out;
       const _: () = {
            const unsafe fn __schema_inner<#[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, DEF)]] >()
                ~> ::jsony::__internal::ObjectSchema<#[#ctx.lifetime]>
                [?(!ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty())
             where [
                for (ty in &ctx.target.generic_field_types) {
                    [~ty]: FromJson<#[#ctx.lifetime]>
                }
             ]]
                {
                    ::jsony::__internal::ObjectSchema {
                        inner: &const { [struct_schema(out, ctx, &ordered_fields, None)?] },
                        phantom: ::std::marker::PhantomData
                    }
                }
            [
                let ts = if let Some(flatten_field) = flattening {
                    body_of_struct_from_json_with_flatten(out, ctx, flatten_field)
                } else {
                    token_stream!(out; __schema_inner::<
                        #[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
                    >().decode(dst, parser, None))
                };
                impl_from_json(out, ctx, ts)?;
                if ctx.target.flattenable {
                    let body = token_stream!(
                        out;
                        jsony::__internal::DynamicFieldDecoder {
                            destination: dst,
                            schema: __schema_inner::<
                                #[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
                            >(),
                            bitset: [#Literal::u64_unsuffixed(0)],
                            required: [#Literal::u64_unsuffixed(required_bitset(&ordered_fields))],
                        }
                    );
                    impl_from_json_field_visitor(
                        out,
                        ctx,
                        &|out| splat!(out; jsony::__internal::DynamicFieldDecoder<#[#ctx.lifetime]>),
                        body
                    )?;

                }
            ]
        };
    );

    Ok(())
}

fn variant_key_literal(_ctx: &Ctx, variant: &EnumVariant) -> Literal {
    // todo add all transforms and renames etc
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
                throw!("Only single field enum tuples are currently supported." @ variant.name.span())
            };
            // Can optimize when #![feature(offset_of_enum)] stabilizes:
            //  https://github.com/rust-lang/rust/issues/120141
            token_stream! {
                out;
                match < [~field.ty] as ::jsony::FromJson<#[#ctx.lifetime]> >::decode_json(parser) {
                    Ok(value) => {
                        dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name](value));
                    },
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
        }
        EnumKind::Struct => {
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
                token_stream! {
                    out;
                    // Note a type alias with an un-used generic is not an error
                    type __TEMP[?(ctx.target.has_lifetime())<#[#ctx.lifetime]>] = ([for (field in &ordered_fields) { [~field.ty], }]);
                    let schema = ::jsony::__internal::ObjectSchema{
                        inner: const { &[try struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
                        phantom: ::std::marker::PhantomData,
                    };
                    let mut temp = ::std::mem::MaybeUninit::<__TEMP>::uninit();
                    let mut temp_flatten = ::std::mem::MaybeUninit::<[~flatten_field.ty]>::uninit();
                    let mut flatten_visitor =
                        <[~flatten_field.ty] as ::jsony::json::FromJsonFieldVisitor>::new_field_visitor(
                            ::std::ptr::NonNull::new_unchecked(temp_flatten.as_mut_ptr().cast()),
                            parser,
                        );
                    if let Err(err) = schema.decode(
                        ::std::ptr::NonNull::new_unchecked(temp.as_mut_ptr().cast()),
                        parser,
                        Some(&mut flatten_visitor),
                    ) {
                        return Err(err)
                    }
                    let temp2 = temp.assume_init();
                    dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name] {
                        [for ((i, field) in ordered_fields.iter().enumerate()) {
                            [#field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                        }]
                        [#flatten_field.name]: temp_flatten.assume_init(),
                    });
                }
            } else {
                token_stream! {
                    out;
                    // Note a type alias with an un-used generic is not an error
                    type __TEMP[?(ctx.target.has_lifetime())<#[#ctx.lifetime]>] = ([for (field in variant.fields) { [~field.ty], }]);
                    let schema = ::jsony::__internal::ObjectSchema{
                        inner: const { &[try struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
                        phantom: ::std::marker::PhantomData,
                    };
                    let mut temp = ::std::mem::MaybeUninit::<__TEMP>::uninit();
                    if let Err(err) = schema.decode(
                        ::std::ptr::NonNull::new_unchecked(temp.as_mut_ptr().cast()),
                        parser,
                        None,
                    ) {
                        return Err(err)
                    }
                    let temp2 = temp.assume_init();
                    dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name] {
                        [for ((i, field) in variant.fields.iter().enumerate()) {
                            [#field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                        }]
                    });
                }
            }
        }
        EnumKind::None => token_stream! {
            out;
            dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name])
        },
    };
    out.push(TokenTree::Group(Group::new(Delimiter::Brace, body)));
    Ok(())
}

fn enum_from_json(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    if ctx.target.flattenable {
        throw!("Flattening enums not supported yet.")
    }
    let body = token_stream! { out;
        let Ok(Some(variant)) = parser.enter_object() else {
            //todo better errors
            return Err(&DecodeError {  message: [@Literal::string("Expected single field object for enum").into()] });
        };
        match variant {
            [for ((_, variant) in variants.iter().enumerate()) {
                [@variant_key_literal(ctx, variant).into()] => [try enum_variant_from_json(out, ctx, variant)],
            }]
            _ => {
                parser.report_error(variant.to_string());
                return Err(&::jsony::parser::UNKNOWN_VARIANT);
            }
        }
        let err = match parser.object_step() {
            Ok(None) => {return Ok(())},
            Err(err) => {Err(err)},
            Ok(Some(_)) => {
                Err(&DecodeError {  message: [@Literal::string("More the one field in enum tab object").into()] })
            },
        };
        std::ptr::drop_in_place::<[ctx.target_type(out)]>(dst.cast().as_mut());
        return err;
    };

    impl_from_json(out, ctx, body)?;

    Ok(())
}

// bincode test
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
        let body = token_stream! { (&mut output); [
            for field in fields {
                binary_encode_field(&mut output, &ctx, field, &|out| splat!{out; &self.[#field.name]})
            }
        ]};
        impl_to_binary(&mut output, &ctx, body)?;
    }

    if target.from_binary {
        let body = token_stream! {(&mut output);
            [#target.name] {
                [for field in fields {
                    splat!{(&mut output);
                        [#field.name]: [binary_decode_field(&mut output, &ctx, field)],
                    }
                }]
            }
        };
        impl_from_binary(&mut output, &ctx, body)?;
    }

    Ok(output.drain(..).collect())
}

fn handle_tuple_struct(target: &DeriveTargetInner, fields: &[Field]) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let ctx = Ctx::new(target)?;

    if target.to_binary {
        let body = token_stream! { (&mut output); [
            for (i, field) in fields.iter().enumerate() {
                binary_encode_field(&mut output, &ctx, field, &|out| splat!{out; &self.[#Literal::usize_unsuffixed(i)]})
            }
        ]};
        impl_to_binary(&mut output, &ctx, body)?;
    }

    if target.from_binary {
        let body = token_stream! {(&mut output);
            [#target.name] (
                [for field in fields {
                    splat!{(&mut output); [binary_decode_field(&mut output, &ctx, field)], }
                }]
            )
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
    let body = token_stream! { out;
        match self {[
            for (i, variant) in variants.iter().enumerate() {
                splat!{out; [#ctx.target.name]::[#variant.name]}
                match variant.kind {
                    EnumKind::Tuple => {
                        splat!{out; (
                            [for (i, _) in variant.fields.iter().enumerate() { splat!(out; [#ctx.temp[i]],) }]
                        ) => {
                            encoder.push([#Literal::u8_unsuffixed(i as u8)]);
                            [for (i, field) in variant.fields.iter().enumerate() {
                                binary_encode_field(out, ctx, field, &|out| splat!{out; [#ctx.temp[i]]})
                            }]
                        }}
                    },
                    EnumKind::Struct => {
                        splat!{out; {
                            [for field in variant.fields { splat!(out; [#field.name],) }]
                        } => {
                            encoder.push([#Literal::u8_unsuffixed(i as u8)]);
                            [for field in variant.fields {
                                binary_encode_field(out, ctx, field, &|out| splat!{out; [#field.name]})
                            }]
                        }}
                    },
                    EnumKind::None => {
                        splat!{out;  => {
                            encoder.push([#Literal::u8_unsuffixed(i as u8)]);
                        }}
                    },
                }
            }
        ]}
    };
    impl_to_binary(out, ctx, body)
}

fn enum_from_binary(
    out: &mut Vec<TokenTree>,
    ctx: &Ctx,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let body = token_stream! { out;
        match decoder.byte() {[
            for (i, variant) in variants.iter().enumerate() {
                splat!{out; [if i + 1 == variants.len() {
                    splat!(out; _)
                }else {
                    splat!(out; [#Literal::u8_unsuffixed(i as u8)])
                }] =>
                    [match variant.kind {
                        EnumKind::Tuple => {
                            splat!{out;
                                [#ctx.target.name]::[#variant.name](
                                    [for field in variant.fields {
                                        splat!{out; [binary_decode_field(out, ctx, field)], }
                                    }]
                                )
                            }
                        }
                        EnumKind::Struct => {
                            splat!{out;
                                [#ctx.target.name]::[#variant.name]{
                                    [for field in variant.fields {
                                        splat!{out; [#field.name]: [binary_decode_field(out, ctx, field)], }
                                    }]
                                }
                            }
                        }
                        EnumKind::None => splat!{out; [#ctx.target.name]::[#variant.name] }
                    }],
                }
            }
        ]}
    };
    impl_from_binary(out, ctx, body)
}

fn handle_enum(target: &DeriveTargetInner, variants: &[EnumVariant]) -> Result<TokenStream, Error> {
    let mut output = Vec::<TokenTree>::new();
    let mut ctx = Ctx::new(target)?;
    let mut max_tuples = 0;
    for var in variants {
        if matches!(var.kind, EnumKind::Tuple) {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp = (0..max_tuples).map(var).collect::<Vec<_>>();
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
        flattenable: false,
        rename_all: crate::case::RenameRule::None,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens)?;

    // Default to from json
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
        // Todo might want to wrap this with stuff
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
