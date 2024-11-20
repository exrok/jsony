use crate::ast::{
    self, DeriveTargetInner, DeriveTargetKind, EnumKind, EnumVariant, Field, FieldAttrs, Generic,
    GenericKind, Tag, TraitSet, Via, FROM_BINARY, FROM_JSON, TO_BINARY, TO_JSON,
};
use crate::case::RenameRule;
use crate::util::MemoryPool;
use crate::writer::RustWriter;
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

#[rustfmt::skip]
macro_rules! append_tok {
    ($ident:ident $d:tt) => {
       $d.tt_ident(stringify!($ident))
    };
    ({} $d: tt) => {
        $d.tt_group_empty(Delimiter::Brace)
    };
    (() $d: tt) => {
        $d.tt_group_empty(Delimiter::Parenthesis)
    };
    ([] $d:tt) => {
        $d.tt_group_empty(Delimiter::Bracket)
    };
    ({$($tt:tt)*} $d: tt) => {{
        let at = $d.buf.len(); $(append_tok!($tt $d);)* $d.tt_group(Delimiter::Brace, at);
    }};
    (($($tt:tt)*) $d: tt) => {{
        let at = $d.buf.len(); $(append_tok!($tt $d);)* $d.tt_group(Delimiter::Parenthesis, at);
    }};
    ([[$($tt:tt)*]] $d:tt) => {{
        let at = $d.buf.len(); $(append_tok!($tt $d);)* $d.tt_group(Delimiter::Bracket, at);
    }};
    (_ $d:tt) => { $d.tt_ident("_") };
    ([$ident:ident] $d:tt) => {
        $d.buf.push($($tt)*)
    };
    ([?($($cond:tt)*) $($body:tt)*] $d:tt) => {
        if $($cond)* { $(append_tok!($body $d);)* }
    };
    ([@$($tt:tt)*] $d:tt) => {
        $d.buf.push($($tt)*)
    };
    ([try $($tt:tt)*] $d:tt) => {
        if let Err(err) = $($tt)* { return Err(err); }
    };
    ([for ($($iter:tt)*) {$($body:tt)*}] $d:tt) => {
        for $($iter)* { $(append_tok!($body $d);)* }
    };
    ([#$($tt:tt)*] $d:tt) => {
        $d.buf.push(TokenTree::from($($tt)*.clone()))
    };
    ([~$($tt:tt)*] $d:tt) => {
        $d.buf.extend_from_slice($($tt)*)
    };
    ([$($rust:tt)*] $d:tt) => {{
         $($rust)*
    }};
    (# $d:tt) => { $d.tt_punct_joint('\'') };
    (: $d:tt) => { $d.tt_punct_alone(':') };
    (~ $d:tt) => { $d.tt_punct_joint('#') };
    (< $d:tt) => { $d.tt_punct_alone('<') };
    (% $d:tt) => { $d.tt_punct_joint(':') };
    (:: $d:tt) => {$d.tt_punct_joint(':'); $d.tt_punct_alone(':') };
    (-> $d:tt) => {$d.tt_punct_joint('-'); $d.tt_punct_alone('>') };
    (=> $d:tt) => {$d.tt_punct_joint('='); $d.tt_punct_alone('>') };
    (> $d:tt) => { $d.tt_punct_alone('>') };
    (! $d:tt) => { $d.tt_punct_alone('!') };
    (| $d:tt) => { $d.tt_punct_alone('|') };
    (. $d:tt) => { $d.tt_punct_alone('.') };
    (; $d:tt) => { $d.tt_punct_alone(';') };
    (& $d:tt) => { $d.tt_punct_alone('&') };
    (= $d:tt) => { $d.tt_punct_alone('=') };
    (, $d:tt) => { $d.tt_punct_alone(',') };
    (* $d:tt) => { $d.tt_punct_alone('*') };
}

macro_rules! splat { ($d:tt; $($tt:tt)*) => { { $(append_tok!($tt $d);)* } } }

macro_rules! token_stream { ($d:tt; $($tt:tt)*) => {{
    let len = $d.buf.len(); $(append_tok!($tt $d);)* $d.split_off_stream(len)
}}}

struct GenericBoundFormatting {
    lifetimes: bool,
    bounds: bool,
}
fn fmt_generics(buffer: &mut RustWriter, generics: &[Generic], fmt: GenericBoundFormatting) {
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
        buffer.buf.push(generic.ident.clone().into());
        if fmt.bounds && !generic.bounds.is_empty() {
            append_tok!(: buffer);
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
fn impl_from_binary(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    splat! {
        output;
        ~[[automatically_derived]]
        unsafe [try bodyless_impl_from(output, None, Ident::new("FromBinary", Span::call_site()), ctx)] {
            fn decode_binary(decoder: &mut [~&ctx.crate_path]::binary::Decoder<#[#ctx.lifetime]>) -> Self [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
    Ok(())
}

fn impl_from_json_field_visitor(
    output: &mut RustWriter,
    ctx: &Ctx,
    ty: &dyn Fn(&mut RustWriter),
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
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
    Ok(())
}

fn impl_from_json(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) -> Result<(), Error> {
    splat! {
        output;
        ~[[automatically_derived]]
        unsafe [try bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)] {
            unsafe fn emplace_from_json(dst: ::std::ptr::NonNull<()>, parser: &mut [~&ctx.crate_path]::parser::Parser<#[#ctx.lifetime]>)
            -> ::std::result::Result<(), &#static ::jsony::json::DecodeError> [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        output;
        ~[[automatically_derived]]
        unsafe impl [?(any_generics) < [fmt_generics(output, &target.generics, DEF)] >]
         [~&crate_path]::ToBinary for [#target.name][?(any_generics) <
            [fmt_generics( output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [
                for ty in &target.generic_field_types {
                    splat!(output; [~ty]: ToBinary,)
                }
             ]] {
            fn encode_binary(&self, encoder: &mut ::jsony::BytesWriter) [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        output;
        ~[[automatically_derived]]
        impl [?(any_generics) < [fmt_generics(output, &target.generics, DEF)] >]
         [~&crate_path]::ToJson for [#target.name][?(any_generics) <
            [fmt_generics( output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [
                for ty in &target.generic_field_types {
                    splat!(output; [~ty]: [~&crate_path]::ToJson,)
                }
             ]] {
            type Kind = [
                match kind {
                    ToJsonKind::Static(kind) => {
                        splat!(output; [~&crate_path]::json::[@Ident::new(kind, Span::call_site()).into()])
                    },
                    ToJsonKind::Forward(field) => {
                        splat!(output; <[~field.ty] as [~&crate_path]::ToJson>::Kind)
                    },
                }
            ];
            fn encode_json__jsony(&self, out: &mut jsony::TextWriter) -> Self::Kind [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
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
    pub fn dead_target_type(&self, out: &mut RustWriter) {
        splat!(out; [#self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, DEAD_USE
        )]>])
    }
    pub fn target_type(&self, out: &mut RustWriter) {
        splat!(out; [#self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, USE
        )]>])
    }
    fn new(out: &mut RustWriter, target: &'a DeriveTargetInner) -> Result<Ctx<'a>, Error> {
        // if !out.buf.is_empty() {
        //     todo!();
        // }
        let crate_path = if let Some(value) = &target.path_override {
            let content = value.to_string();
            // assumes normal string and no escapes probably shouldn't
            #[allow(unused)]
            let inner = &content[1..content.len() - 1];
            // TokenStream::from_str(inner)
            //     .unwrap()
            //     .into_iter()
            //     .collect::<Vec<TokenTree>>()
            // let mut out = RustWriter::new();
            splat!(out; ::jsony);
            std::mem::take(&mut out.buf)
        } else {
            splat!(out; ::jsony);
            std::mem::take(&mut out.buf)
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

fn encode_binary_field(
    output: &mut RustWriter,
    ctx: &Ctx,
    field: &Field,
    place: &dyn Fn(&mut RustWriter),
) {
    if field.flags & Field::WITH_TO_BINARY_SKIP != 0 {
        return;
    }
    splat! {
        output;
        [
            if let Some(path) = field.with(TO_BINARY) {
                splat!(output; [~path])
            } else {
                splat!(output; <[~field.ty] as [~&ctx.crate_path]::ToBinary>)
            }
        ]
        ::encode_binary(
            [place(output)], encoder
        );
    };
}

fn decode_binary_field(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    if field.flags & Field::WITH_FROM_BINARY_SKIP != 0 {
        field_from_default(out, field, FROM_BINARY);
        return;
    }
    splat! {
        out;
        [
            if let Some(path) = field.with(FROM_BINARY) {
                splat!(out; [~path])
            } else {
                splat!(out; <[~field.ty] as [ctx.FromBinary(out)]>)
            }
        ]
        ::decode_binary(
            decoder
        )
    };
}
impl Ctx<'_> {
    // #[allow(non_snake_case)]
    // fn FromJson(&self, out: &mut RustWriter) {
    //     splat!(out;  [~&self.crate_path]::FromJson<#[#self.lifetime]>)
    // }

    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        splat!(out;  [~&self.crate_path]::FromBinary<#[#self.lifetime]>)
    }
}

fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) -> Result<(), Error> {
    if let Some(with) = field.with(FROM_JSON) {
        splat! { out;
           [~&ctx.crate_path]::__internal::emplace_json_for_with_attribute::<&mut ::jsony::parser::Parser<#[#ctx.lifetime]>, [~field.ty], _>(
               &[~with]::decode_json
           )
        }
    } else {
        splat! { out; [~&ctx.crate_path]::__internal::erased_emplace_from_json::<#[#ctx.lifetime], [~field.ty]>() }
        // splat! { out; [~&ctx.crate_path]::__internal::erase(
        //     <[~field.ty] as [ctx.FromJson(out)]>::emplace_from_json
        // ) }
    }
    Ok(())
}

fn field_name_literal(ctx: &Ctx, field: &Field) -> Literal {
    // todo pass context in
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

// Probably can merge with field name json
fn variant_name_json(ctx: &Ctx, field: &EnumVariant, output: &mut String) -> Result<(), Error> {
    output.push('"');
    if let Some(name) = field.rename(TO_JSON) {
        match crate::lit::literal_inline(name.to_string()) {
            crate::lit::InlineKind::String(value) => {
                crate::template::raw_escape(&value, output);
            }
            _ => {
                throw!("Invalid rename value expected a string" @ name.span())
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
    out: &mut RustWriter,
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
        let x = out.buf.len();
        append_tok!(< out);
        fmt_generics(
            out,
            &ctx.target.generics,
            GenericBoundFormatting {
                lifetimes: false,
                bounds: false,
            },
        );
        append_tok!(> out);
        TokenTree::Group(Group::new(Delimiter::None, out.buf.drain(x..).collect()))
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
                ::jsony::__internal::erased_drop_in_place::<[~field.ty]>,
            }
        }
    ]);
    let schema_drops = TokenTree::Group(Group::new(Delimiter::Bracket, ts));
    let ts = token_stream!(out; [
        for field in fields {
            let Some(default) = &field.default(FROM_JSON) else {
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
            throw!("FromJson not implemented for Tuples without fields yet.")
        }
        [field] => {
            //tod immple with
            splat!(out;
                < [~field.ty] as ::jsony::FromJson<#[#ctx.lifetime]> >::emplace_from_json(
                    // to remove addition is repr(transparent) or repr(c)
                    dst[?(!matches!(ctx.target.repr, ast::Repr::Transparent | ast::Repr::C))
                        .byte_add(
                            ::std::mem::offset_of!(
                                [ctx.dead_target_type(out)],
                                [@Literal::usize_unsuffixed(0).into()]
                            )
                        )
                    ],
                    parser
                )
            );
            ToJsonKind::Forward(field)
        }
        _ => {
            throw!("FromJson not implemented for Tuples with multiple fields yet.")
        }
    };
    let stream = out.split_off_stream(head);
    impl_from_json(out, ctx, stream)
}

fn tuple_struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let head = out.buf.len();
    let kind = match fields {
        [] => {
            splat!(out; out.push_str([
                out.buf.push(TokenTree::Literal(Literal::string("null")));
            ]));
            ToJsonKind::Static("AnyValue")
        }
        [field] => {
            splat!(out;
                [
                    if let Some(with) = field.with(TO_JSON) {
                        splat!(out; [~with]::encode_json)
                    } else {
                        splat!(out; <[~field.ty] as ::jsony::ToJson>::encode_json__jsony)
                    }
                ]
                (&self.[#Literal::usize_unsuffixed(0)], out)
            );
            ToJsonKind::Forward(field)
        }
        _ => {
            let mut first = true;
            splat!(
                out;
                out.start_json_array();
                [for ((i, field) in fields.iter().enumerate()) {
                    [if first {
                        first = false;
                    } else {
                        splat!(out; out.push_comma());
                    }]
                    [
                        if let Some(with) = field.with(TO_JSON) {
                            splat!(out; [~with]::encode_json)
                        } else {
                            splat!(out; <[~field.ty] as ::jsony::ToJson>::encode_json__jsony)
                        }
                    ](&self.[#Literal::usize_unsuffixed(i)], out);
                }]
                out.end_json_array()
            );
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
            splat!(out; | [#binding]: &[~ty] | [~rest]);
            return;
        }
    }
    splat!(out; [~attr_value])
}

fn inner_struct_to_json(
    out: &mut RustWriter,
    ctx: &Ctx,
    fields: &[Field],
    text: &mut String,
    on_self: bool,
) -> Result<(), Error> {
    let mut first = true;
    splat!(
        out;
        [for (i, field) in fields.iter().enumerate() {
            let if_skip_body = if let Some(skip_fn) = field.skip(TO_JSON) {
                if skip_fn.is_empty() {
                    // Empty skip fn implies always skip
                    continue;
                }
                if !first {
                    text.push(',');
                }
                if !text.is_empty() {
                    if text == "," {
                        splat!(out; out.push_comma(););
                    } else {
                        splat!(out;
                            out.push_str([
                                out.buf.push(TokenTree::Literal(Literal::string(&text)));
                            ]);
                        );
                    }
                    text.clear();
                }
                splat!(out;
                    if !([with_injected_closure_arg_type(out, skip_fn, &field.ty)])([if on_self {
                        splat!(out; &self.[#field.name])
                    } else {
                        splat!(out; [#ctx.temp[i]])
                    }])
                );
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
                    return Err(err)
                }
                text.push(':');
            }
            if !text.is_empty() {
                if text == "," {
                    splat!(out; out.push_comma(););
                } else {
                    splat!(out;
                        out.push_str([
                            out.buf.push(TokenTree::Literal(Literal::string(&text)));
                        ]);
                    );
                }
                text.clear();
            }
            if let Via::Iterator = field.via(TO_JSON) {
                if flattened {
                    splat!(out;
                        for (key, value) in ([if on_self {
                            splat!(out; &self.[#field.name])
                        } else {
                            splat!(out; [#ctx.temp[i]])
                        }]).iter() {
                            let _: ::jsony::json::AlwaysString = key.encode_json__jsony(out);
                            out.push_colon();
                            value.encode_json__jsony(out);
                            out.push_comma();
                        }
                    );
                }
            } else {
                if flattened {
                    splat!(out;
                        out.join_parent_json_value_with_next();
                        let _: ::jsony::json::AlwaysObject =
                    );
                }
                splat!(out; [
                            if let Some(with) = field.with(TO_JSON) {
                                splat!(out; [~with]::encode_json)
                            } else {
                                splat!(out; <[~field.ty] as ::jsony::ToJson>::encode_json__jsony)
                            }
                        ]([if on_self {
                    splat!(out; &self.[#field.name])
                } else {
                    splat!(out; [#ctx.temp[i]])
                }], out););
                if flattened {
                    splat!(out; out.join_object_with_next_value(););
                }
            }
            if let Some(from) = if_skip_body {
                if !flattened {
                    splat!(out; out.push_comma(););
                }
                let inner = TokenTree::Group(Group::new(Delimiter::Brace, out.split_off_stream(from)));
                out.buf.push(inner);
            }

        }]
    );
    Ok(())
}

fn field_from_default(out: &mut RustWriter, field: &Field, set: TraitSet) {
    // todo optimize this.
    splat!(
        out; [
            if let Some(explicit_default) = field.default(set) {
                splat!(
                    out;
                    {
                        let __scope_jsony = | | -> [~field.ty] {
                            return [~explicit_default]
                        };
                        __scope_jsony()
                    }
                )
            } else {
                splat!( out; Default::default())
            }
        ]
    );
}

fn struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) -> Result<(), Error> {
    let mut text = String::new();
    let body = token_stream!(
        out;
        out.start_json_object();
        [inner_struct_to_json(out, ctx, fields, &mut text, true)?]
        out.end_json_object()
    );

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

    splat!(out;
        [?(ctx.target.flattenable)
        const unsafe fn __schema_inner<#[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, DEF)]] >()
            ~> ::jsony::__internal::ObjectSchema<#[#ctx.lifetime]>
            [?(!ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty())
            where [
            for (ty in &ctx.target.generic_field_types) {
                [~ty]: FromJson<#[#ctx.lifetime]>
            }
            ]]
            {
                ::jsony::__internal::ObjectSchema::<#[#ctx.lifetime]> {
                    inner: &const { [struct_schema(out, ctx, &ordered_fields, None)?] },
                    phantom: ::std::marker::PhantomData
                }
            }
        ]
        [
            let start = out.buf.len();
            if let Some(flatten_field) = flattening {
                splat!(out;
                    let mut __flatten_visitor_jsony =
                        <[~flatten_field.ty] as ::jsony::json::FromJsonFieldVisitor>::new_field_visitor(
                            dst.byte_add(offset_of!([ctx.dead_target_type(out)], [#flatten_field.name])),
                            parser,
                        );
                )
            }
            if has_skips {
                splat!(out; let __result = );
            }
            if ctx.target.flattenable {
                splat!(out; __schema_inner::<
                    #[#ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
                >())
            } else {
                splat!(out;
                ::jsony::__internal::ObjectSchema::<#[#ctx.lifetime]> {
                    inner: &const { [struct_schema(out, ctx, &ordered_fields, None)?] },
                    phantom: ::std::marker::PhantomData
                })
            }
            splat!(out; .decode(dst, parser, [if flattening.is_some() {
                splat!(out; Some(&mut __flatten_visitor_jsony))
            } else {
                splat!(out; None)
            }]));
            if has_skips {
                splat!(out;
                    ; //<-- close off let stmt
                    if let Err(err) = __result {
                        return Err(err)
                    }
                    [for field in fields {
                        if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                            continue;
                        }
                        splat!(
                            out;
                            dst.byte_add(::std::mem::offset_of!([ctx.dead_target_type(out)], [#field.name]))
                                .cast::<[~field.ty]>()
                                .write([field_from_default(out, field, FROM_JSON)]);
                        )
                    }]

                    Ok(())
                );
            }
            let ts = out.split_off_stream(start);
            impl_from_json(out, ctx, ts)?;
            if ctx.target.flattenable {
                if has_skips {
                    throw!("Flattenable does not yet support skipped fields")
                }
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
    );

    Ok(())
}

fn variant_key_literal(_ctx: &Ctx, variant: &EnumVariant) -> Literal {
    // todo add all transforms and renames etc
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
        splat!(out; out.start_json_object(););
        text.push('"');
        crate::template::raw_escape(&tag_name, &mut text);
        text.push_str("\":");
        splat! { out; out.push_str([@Literal::string(&text).into()]); };
    } else if all_objects {
        splat!(out; out.start_json_object(););
    }
    splat!(
        out;
        match self {[
            for (_, variant) in variants.iter().enumerate() {
                splat!{out; [#ctx.target.name]::[#variant.name]}
                match variant.kind {
                    EnumKind::Tuple => {
                        splat!{out; (
                            [for (i, _) in variant.fields.iter().enumerate() { splat!(out; [#ctx.temp[i]],) }]
                        ) => [
                            text.clear();
                            enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                        ]}
                    },
                    EnumKind::Struct => {
                        splat!{out; {
                            [for (i, field) in variant.fields.iter().enumerate() { splat!(out; [#field.name]: [#ctx.temp[i]],) }]
                        } => [
                            text.clear();
                            enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                        ]}
                    },
                    EnumKind::None => {
                        splat!{out; => [
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects)?;
                        ]}
                    },
                }
            }
        ]}
    );
    if let Tag::Inline(..) = &ctx.target.tag {
        splat! { out; out.end_json_object(); };
    } else if all_objects {
        splat! { out; out.end_json_object(); };
    }
    splat! { out; Self::Kind {} };
    let stream = out.buf.drain(start..).collect();
    //todo should sometimes be AlwaysString
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
    splat!(
        out;
        [
            match ctx.target.tag {
                Tag::Untagged => splat!(out;out.start_json_object();),
                Tag::Inline(..) if ctx.target.content.is_some() => text.push('{'),
                Tag::Default  => {
                    text.push('{')
                },
                _ => ()
            }
        ]
        [inner_struct_to_json(out, ctx, &variant.fields, text, false)?];
        [
            // opt: Could do a static '}' push if we know that there isn't a trailing comma which
            // can occur when flattening
            match ctx.target.tag {
                Tag::Untagged => splat!(out;out.end_json_object();),
                Tag::Inline(..) if ctx.target.content.is_some() => splat!(out;out.end_json_object();),
                Tag::Default  => {
                    splat!(out;out.end_json_object();)
                    // text.push('}')
                },
                _ => ()
            }
            // if !text.is_empty() {
            //     splat!(out; out.push_str([@Literal::string(&text).into()]););
            //     text.clear();
            // }
        ]
    );
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
                //optimizations can be done here depending
                // whether all the enum variants None or a object etect
                // to move this out of each variant.
                if !all_objects {
                    splat!(out; out.start_json_object(););
                }
                variant_name_json(ctx, variant, text)?;
                text.push_str(":");
            }
        }
    }
    match variant.kind {
        EnumKind::Tuple => {
            let [field] = variant.fields else {
                throw!("Only single field enum tuples are currently supported." @ variant.name.span())
            };
            if !text.is_empty() {
                splat! {
                    out;
                    out.push_str([@Literal::string(&text).into()]);
                };
                text.clear();
            }
            splat! {
                out;
                [
                    if let Some(with) = field.with(TO_JSON) {
                        splat!(out; [~with]::encode_json)
                    } else {
                        splat!(out; <[~field.ty] as ::jsony::ToJson>::encode_json__jsony)
                    }
                ]
                ([#ctx.temp[0]], out);
            }
        }
        EnumKind::Struct => {
            if let Err(err) = enum_variant_to_json_struct(out, ctx, variant, text) {
                return Err(err);
            }
        }
        EnumKind::None => {
            variant_name_json(ctx, variant, text)?;
            splat! {
                out;
                out.push_str([@Literal::string(&text).into()]);
            };
            text.clear();
        }
    };

    if !all_objects {
        match &ctx.target.tag {
            Tag::Default => {
                if let EnumKind::None = variant.kind {
                } else {
                    //optimizations can be done here depending
                    // whether all the enum variants None or a object etect
                    // to move this out of each variant.
                    splat!(out; out.end_json_object(););
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
        splat! {
            out;
            // Note a type alias with an un-used generic is not an error
            type __TEMP[?(ctx.target.has_lifetime())<#[#ctx.lifetime]>] = ([for (field in &ordered_fields) { [~field.ty], }]);
            let schema = ::jsony::__internal::ObjectSchema{
                inner: const { &[try struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
                phantom: ::std::marker::PhantomData,
            };
            let mut temp = ::std::mem::MaybeUninit::<__TEMP>::uninit();
            let mut temp_flatten = ::std::mem::MaybeUninit::<[~flatten_field.ty]>::uninit();
            // todo should handle unneed mut
            let mut flatten_visitor =
                <[~flatten_field.ty] as ::jsony::json::FromJsonFieldVisitor>::new_field_visitor(
                    ::std::ptr::NonNull::new_unchecked(temp_flatten.as_mut_ptr().cast()),
                    parser,
                );
            [?(let Tag::Inline(tag_name) = &ctx.target.tag)
                [?(ctx.target.content.is_none())
                    let mut flatten_visitor = jsony::__internal::SkipFieldVisitor{
                        skipped_field: [@Literal::string(tag_name).into()],
                        visitor: flatten_visitor
                    };
                ]
            ]
            if let Err(_err) = schema.decode(
                ::std::ptr::NonNull::new_unchecked(temp.as_mut_ptr().cast()),
                parser,
                Some(&mut flatten_visitor),
            ) {
                [?(!untagged) return Err(_err);]
            } else {
                let temp2 = temp.assume_init();
                dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name] {
                    [for ((i, field) in ordered_fields.iter().enumerate()) {
                        [#field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                    }]
                    [#flatten_field.name]: temp_flatten.assume_init(),
                    [for field in variant.fields {
                        if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                            continue;
                        }
                        splat!(out; [#field.name]: [field_from_default(out, field, FROM_JSON)],)
                    }]
                });
                [?(untagged) break #success]
            }
        }
    } else {
        splat! {
            out;
            // Note a type alias with an un-used generic is not an error
            type __TEMP[?(ctx.target.has_lifetime())<#[#ctx.lifetime]>] = ([for (field in &ordered_fields) { [~field.ty], }]);
            let schema = ::jsony::__internal::ObjectSchema{
                inner: const { &[try struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
                phantom: ::std::marker::PhantomData,
            };
            let mut temp = ::std::mem::MaybeUninit::<__TEMP>::uninit();
            if let Err(_err) = schema.decode(
                ::std::ptr::NonNull::new_unchecked(temp.as_mut_ptr().cast()),
                parser,
                None,
            ) {
                [?(!untagged) return Err(_err);]
            } else {
                let temp2 = temp.assume_init();
                dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name] {
                    [for ((i, field) in ordered_fields.iter().enumerate()) {
                        [#field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                    }]
                    [for field in variant.fields {
                        if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                            continue;
                        }
                        splat!(out; [#field.name]: [field_from_default(out, field, FROM_JSON)],)
                    }]
                });
                [?(untagged) break #success]
            }
        }
    };
    Ok(())
}

fn other_variant_key(out: &mut RustWriter, field: &Field) {
    splat!(out;
        let erased = unsafe {
            &*(variant as *const str)
        };
        let other_tag = match <[~field.ty] as ::jsony::text::FromText>::from_text(
            &mut parser.at.ctx,
            erased
        ) {
            Ok(value) => value,
            Err(err) => return Err(err)
        };
    );
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
                splat! {
                    out;
                    if !at_content {
                        return Err(&::jsony::parser::MISSING_CONTENT_TAG)
                    }
                }
            }
            let [field] = variant.fields else {
                throw!("Only single field enum tuples are currently supported." @ variant.name.span())
            };
            // Can optimize when #![feature(offset_of_enum)] stabilizes:
            //  https://github.com/rust-lang/rust/issues/120141
            splat! {
                out;
                match [
                    if let Some(with) = field.with(FROM_JSON) {
                        splat!(out; [~with])
                    } else {
                        splat!(out; < [~field.ty] as ::jsony::FromJson<#[#ctx.lifetime]> >)
                    }
                ]::decode_json(parser) {
                    Ok(value) => {
                        dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name](value));
                        [?(untagged) break #success]
                    },
                    Err(_err) => {
                        [?(!untagged) return Err(_err);]
                    }
                }
            }
        }
        EnumKind::Struct => {
            if ctx.target.content.is_some() {
                splat! {
                    out;
                    if !at_content {
                        return Err(&::jsony::parser::MISSING_CONTENT_TAG)
                    }
                }
            }
            if let Err(err) = enum_variant_from_json_struct(out, ctx, variant, untagged) {
                return Err(err);
            }
        }
        EnumKind::None => splat! {
            out;
            [
                if let Tag::Inline(..) = ctx.target.tag {
                    if ctx.target.content.is_none() {
                        splat!{
                            out;
                            if let Err(err) = parser.skip_value() {
                                return Err(err);
                            }
                        }
                    }
                }
            ]
            dst.cast::<[ctx.target_type(out)]>().write([#ctx.target.name]::[#variant.name]);
            [?(untagged) break #success]
        },
    };
    let ts = TokenStream::from_iter(out.buf.drain(start..));
    out.buf
        .push(TokenTree::Group(Group::new(Delimiter::Brace, ts)));
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
                throw!("Only one other variant is currently supported." @ variant.name.span())
            }
            other = Some(variant);
        }
    }
    let body = token_stream! {
        out;
        match parser.take_string() {
            Ok(variant) => {
                let value = match variant {
                    [for (variant in variants) {
                        [@variant_key_literal(ctx, variant).into()]
                            => [#ctx.target.name]::[#variant.name],
                    }]
                    [try enum_from_json_unknown_variant(out, ctx, other, true)]
                };
                dst.cast::<[ctx.target_type(out)]>().write(value);
            }
            Err(err) => return Err(err),
        }
        Ok(())
    };
    impl_from_json(out, ctx, body)
}

fn enum_from_json_unknown_variant(
    out: &mut RustWriter,
    ctx: &Ctx,
    other: Option<&EnumVariant>,
    stringly: bool,
) -> Result<(), Error> {
    splat!(out;
        _ => {
            [
                if let Some(other) = &other {
                    match other.fields {
                        [] => (),
                        [field] => other_variant_key(out, field),
                        [_f1, f2, ..] => {
                            throw!("Other variants may only have upto a single field" @ f2.name.span())
                        }
                    }
                    if !stringly {
                        let start = out.buf.len();
                        splat!(
                            out;
                            if let Err(err) = parser.skip_value() {
                                return Err(err)
                            }
                        );
                        if ctx.target.content.is_some() {
                            let skipbody = out.split_off_stream(start);
                            splat!(out; if at_content [@TokenTree::Group(Group::new(Delimiter::Brace, skipbody))])
                        }
                    }
                    let start = out.buf.len();
                    splat!(
                        out;
                        [#ctx.target.name]::[#other.name]
                        [if let [field] = other.fields {
                            match other.kind {
                                EnumKind::Tuple => splat!(out; (other_tag)),
                                EnumKind::Struct => splat!(out; {[#field.name]: other_tag}),
                                EnumKind::None => (),
                            }
                        }]
                    );
                    if !stringly {
                        let value = out.split_off_stream(start);
                        splat!(out; dst.cast::<[ctx.target_type(out)]>().write [
                            @TokenTree::Group(Group::new(Delimiter::Parenthesis, value))
                        ];)
                    }
                } else {
                    splat!(
                        out;
                        let variant = variant.to_string();
                        parser.report_error(variant);
                        return Err(&::jsony::parser::UNKNOWN_VARIANT);
                    )
                }
            ]
        }
    );
    Ok(())
}

fn enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
    if ctx.target.flattenable {
        throw!("Flattening enums not supported yet.")
    }

    let mut mixed_strings_and_objects = false;
    let inline_tag = match &ctx.target.tag {
        Tag::Inline(literal) => Some(literal),
        Tag::Untagged => {
            let body = token_stream! {
                out;
                let snapshot = parser.snapshot();
                #success: {
                    [for ((i, variant) in variants.iter().enumerate()) {
                        {
                            [?(i != 0) parser.restore_for_retry(&snapshot); ]
                            [try enum_variant_from_json(out, ctx, variant, true)]
                        }
                    }]
                    return Err(&::jsony::json::DecodeError{
                        message: [@Literal::string("Untagged enum didn't match any variant").into()]
                    })
                }
                Ok(())
            };
            impl_from_json(out, ctx, body)?;
            return Ok(());
        }
        Tag::Default => {
            let mut kinds = [0usize, 0, 0];
            for variant in variants {
                kinds[variant.kind as usize] += 1;
            }
            let none_count = kinds[EnumKind::None as usize];
            if none_count == variants.len() {
                return stringly_enum_from_json(out, ctx, variants);
            } else if none_count != 0 {
                mixed_strings_and_objects = true;
            }
            None
        }
    };
    let mut other: Option<&EnumVariant> = None;
    for variant in variants {
        if variant.attr.has_other() {
            if other.is_some() {
                throw!("Only one other variant is currently supported." @ variant.name.span())
            }
            other = Some(variant);
        }
    }
    let mut body = token_stream! { out;
        [
            if let Some(tag) = inline_tag {
                splat!(
                    out;
                    [?(ctx.target.content.is_some()) let at_content: bool;]
                    let variant = match parser.
                    [
                        if let Some(content) = &ctx.target.content {
                            splat!(out; tag_query_at_content_next_object([@Literal::string(tag).into()], [@Literal::string(content).into()]))
                        } else {
                            splat!(out; tag_query_next_object([@Literal::string(tag).into()]))
                        }
                    ]

                     {
                        [if let Some(..) = &ctx.target.content {
                            splat!(out; Ok((value, is_at_content)) => {
                                at_content = is_at_content;
                                value
                            })
                        } else {
                            splat!(out; Ok(value) => value,)
                        }]
                        Err(err) => return Err(err),
                    };
                )
            } else {
                splat!(
                    out;
                    let Ok(Some(variant)) = parser.[
                        //todo could opt this
                        if mixed_strings_and_objects {
                            splat!(out; enter_seen_object)
                        } else {
                            splat!(out; enter_object)
                        }
                    ]() else {
                        //todo better errors
                        return Err(&jsony::json::DecodeError {
                            message: [@Literal::string("Expected single field object for enum").into()]
                        });
                    };
                )
            }
        ]
        match variant {
            [for variant in variants {
                if mixed_strings_and_objects {
                    if let EnumKind::None = variant.kind {
                        continue;
                    }
                }
                if other.is_some() && variant.attr.has_other() {
                    continue;
                }

                splat!(out;
                    [@variant_key_literal(ctx, variant).into()]
                        => [try enum_variant_from_json(out, ctx, variant, false)],
                );
            }]
            [try enum_from_json_unknown_variant(out, ctx, other, false)]
        }
        [
            if let Some(_) = inline_tag {
                if ctx.target.content.is_some() {
                    //todo this leaks?
                    splat!(out; if at_content {parser.discard_remaining_object_fields()} else {Ok(())})
                } else {
                    splat!(out; Ok(()))
                }
            } else {
                splat!(
                    out;
                    let err = match parser.object_step() {
                        Ok(None) => {return Ok(())},
                        Err(err) => {Err(err)},
                        Ok(Some(_)) => {
                            Err(&jsony::json::DecodeError {  message: [@Literal::string("More the one field in enum tab object").into()] })
                        },
                    };
                    std::ptr::drop_in_place::<[ctx.target_type(out)]>(dst.cast().as_mut());
                    return err;
                )
            }
        ]
    };

    if mixed_strings_and_objects {
        body = token_stream!(
            out;
            match parser.peek() {
                Ok(::jsony::parser::Peek::Object) => [@TokenTree::Group(Group::new(Delimiter::Brace, body))],
                Ok(::jsony::parser::Peek::String) => match unsafe {parser.read_seen_string()} {
                    Ok(variant) => {
                        let value = match variant {
                            [for variant in variants {
                                if let EnumKind::None = variant.kind {
                                    splat!(out; [@variant_key_literal(ctx, variant).into()] => [#ctx.target.name]::[#variant.name],);
                                }
                            }]
                            [try enum_from_json_unknown_variant(out, ctx, other, true)]
                        };
                        dst.cast::<[ctx.target_type(out)]>().write(value);
                        return Ok(());
                    }
                    Err(err) => return Err(err),
                },
                Ok(_) => {
                    return Err(&jsony::json::DecodeError {
                        message: [@Literal::string("Expected either an object or a string").into()] ,
                    });
                }
                Err(err) => return Err(err),
            }
        )
    }

    impl_from_json(out, ctx, body)?;

    Ok(())
}

// bincode test
fn handle_struct(
    output: &mut RustWriter,
    target: &DeriveTargetInner,
    fields: &[Field],
) -> Result<(), Error> {
    let ctx = Ctx::new(output, target)?;
    if target.from_json {
        struct_from_json(output, &ctx, fields)?;
    }
    if target.to_json {
        struct_to_json(output, &ctx, fields)?;
    }

    if target.to_binary {
        let body = token_stream! { output; [
            for field in fields {
                encode_binary_field(output, &ctx, field, &|out| splat!{out; &self.[#field.name]})
            }
        ]};
        impl_to_binary(output, &ctx, body)?;
    }

    if target.from_binary {
        let body = token_stream! {output;
            [#target.name] {
                [for field in fields {
                    splat!{(output);
                        [#field.name]: [decode_binary_field(output, &ctx, field)],
                    }
                }]
            }
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
        let body = token_stream! {output; [
            for (i, field) in fields.iter().enumerate() {
                encode_binary_field(output, &ctx, field, &|out| splat!{out; &self.[#Literal::usize_unsuffixed(i)]})
            }
        ]};
        impl_to_binary(output, &ctx, body)?;
    }

    if target.from_binary {
        let body = token_stream! {output;
            [#target.name] (
                [for field in fields {
                    splat!{(output); [decode_binary_field(output, &ctx, field)], }
                }]
            )
        };
        impl_from_binary(output, &ctx, body)?;
    }

    Ok(())
}

fn enum_to_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) -> Result<(), Error> {
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
                                encode_binary_field(out, ctx, field, &|out| splat!{out; [#ctx.temp[i]]})
                            }]
                        }}
                    },
                    EnumKind::Struct => {
                        splat!{out; {
                            [for (i, field) in variant.fields.iter().enumerate() { splat!(out; [#field.name]: [#ctx.temp[i]],) }]
                        } => {
                            encoder.push([#Literal::u8_unsuffixed(i as u8)]);
                            [for field in variant.fields {
                                encode_binary_field(out, ctx, field, &|out| splat!{out; [#ctx.temp[i]]})
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
    out: &mut RustWriter,
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
                                        splat!{out; [decode_binary_field(out, ctx, field)], }
                                    }]
                                )
                            }
                        }
                        EnumKind::Struct => {
                            splat!{out;
                                [#ctx.target.name]::[#variant.name]{
                                    [for field in variant.fields {
                                        splat!{out; [#field.name]: [decode_binary_field(out, ctx, field)], }
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

fn handle_enum(
    output: &mut RustWriter,
    target: &DeriveTargetInner,
    variants: &[EnumVariant],
) -> Result<(), Error> {
    let mut ctx = Ctx::new(output, target)?;
    let mut max_tuples = 0;
    for var in variants {
        if matches!(var.kind, EnumKind::Tuple | EnumKind::Struct) {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp = (0..max_tuples).map(var).collect::<Vec<_>>();
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

    // Default to from json
    if !(target.from_binary || target.to_binary || target.to_json || target.from_json) {
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
    Ok(token_stream!(
        (&mut rust_writer);
        ~[[allow(clippy::question_mark)]]
        const _: () = [@TokenTree::Group(Group::new(Delimiter::Brace, ts))];
    ))
}

pub fn derive(stream: TokenStream) -> TokenStream {
    match inner_derive(stream.clone()) {
        Ok(e) => e,
        Err(err) => err.to_compiler_error(),
    }
}
