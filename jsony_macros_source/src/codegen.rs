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
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[rustfmt::skip]
macro_rules! throw {
    ($literal: literal @ $span: expr, $($tt:tt)*) => { Error::span_msg_ctx($literal, &($($tt)*), $span) };
    ($literal: literal, $($tt:tt)*) => { Error::msg_ctx($literal, &($($tt)*)) };
    ($literal: literal @ $span: expr) => { Error::span_msg($literal, $span) };
    ($literal: literal) => { Error::msg($literal) };
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
    ([#: $($tt:tt)*] $d:tt) => {
        $d.push_ident($($tt)*)
    };
    ([@$($tt:tt)*] $d:tt) => {
        $d.buf.push($($tt)*)
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
    (+ $d:tt) => { $d.tt_punct_alone('+') };
    (~ $d:tt) => { $d.tt_punct_joint('#') };
    (< $d:tt) => { $d.tt_punct_alone('<') };
    (|| $d:tt) => { $d.tt_punct_joint('|');$d.tt_punct_alone('|') };
    (% $d:tt) => { $d.tt_punct_joint(':') };
    (== $d:tt) => {$d.tt_punct_joint('='); $d.tt_punct_alone('=') };
    (:: $d:tt) => {$d.tt_punct_joint(':'); $d.tt_punct_alone(':') };
    (-> $d:tt) => {$d.tt_punct_joint('-'); $d.tt_punct_alone('>') };
    (=> $d:tt) => {$d.tt_punct_joint('='); $d.tt_punct_alone('>') };
    (>= $d:tt) => {$d.tt_punct_joint('>'); $d.tt_punct_alone('=') };
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
) {
    let any_generics = !target.generics.is_empty();
    splat! {
        output;
        impl <#[#: lifetime] [?(!generics.is_empty()), [fmt_generics(output, generics, DEF)]] >
         [~&crate_path]::[?(let Some(sub) = sub) [#: &sub]::][#: &trait_name]<#[#: lifetime]> for [#: &target.name][?(any_generics) <
            [fmt_generics(output, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [for (ty in &target.generic_field_types) {
                [~ty]: [#: &trait_name]<#[#: lifetime]>,
            }] [~&target.where_clauses] ]
    };
}

fn impl_from_binary(
    out: &mut RustWriter,
    ctx: &Ctx,
    inner: TokenStream,
    pod_forward: Option<&Field>,
) {
    splat! {
        out;
        ~[[automatically_derived]]
        unsafe [bodyless_impl_from(out, None, Ident::new("FromBinary", Span::call_site()), ctx)] {
            [if ctx.target.pod {
                splat!(out; const POD: bool = true;)
            } else if let Some(pod_field) = pod_forward {
                splat!(out; const POD: bool = <[~pod_field.ty] as ::jsony::FromBinary>::POD;)
            }]
            fn decode_binary(decoder: &mut [~&ctx.crate_path]::binary::Decoder<#[#: &ctx.lifetime]>) -> Self [
                out.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
}

fn impl_from_json_field_visitor(
    output: &mut RustWriter,
    ctx: &Ctx,
    ty: &dyn Fn(&mut RustWriter),
    inner: TokenStream,
) {
    splat! {
        output;
        unsafe [bodyless_impl_from(
            output,
            Some(Ident::new("json", Span::call_site())),
            Ident::new("FromJsonFieldVisitor", Span::call_site()),
            ctx
        )] {
            type Visitor = [ty(output)];

            unsafe fn new_field_visitor(
                dst: ::std::ptr::NonNull<()>,
                parser: & [~&ctx.crate_path]::parser::Parser<#[#: &ctx.lifetime]>
            ) -> Self::Visitor [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
    };
}

fn impl_from_json(output: &mut RustWriter, ctx: &Ctx, inner: TokenStream) {
    splat! {
        output;
        ~[[automatically_derived]]
        unsafe [bodyless_impl_from(output, None, Ident::new("FromJson", Span::call_site()), ctx)] {
            unsafe fn emplace_from_json(dst: ::std::ptr::NonNull<()>, parser: &mut [~&ctx.crate_path]::parser::Parser<#[#: &ctx.lifetime]>)
            -> ::std::result::Result<(), &#static ::jsony::json::DecodeError> [
                output.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        out;
        ~[[automatically_derived]]
        unsafe impl [?(any_generics) < [fmt_generics(out, &target.generics, DEF)] >]
         [~&crate_path]::ToBinary for [#: &target.name][?(any_generics) <
            [fmt_generics( out, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty() || !target.generic_field_types.is_empty())
             where [
                for ty in &target.generic_field_types {
                    splat!(out; [~ty]: ToBinary,)
                }
             ]] {
            [if target.pod {
                splat!(out; const POD: bool = true;)
            } else if let Some(pod_field) = pod_forward {
                splat!(out; const POD: bool = <[~pod_field.ty] as ::jsony::FromBinary>::POD;)
            }]
            fn encode_binary(&self, encoder: &mut ::jsony::BytesWriter) [
                out.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)))
            ]
        }
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
    splat! {
        output;
        ~[[automatically_derived]]
        impl [?(any_generics) < [fmt_generics(output, &target.generics, DEF)] >]
         [~&crate_path]::ToJson for [#: &target.name][?(any_generics) <
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
        splat!(out; [#: &self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, DEAD_USE
        )]>])
    }
    pub fn target_type(&self, out: &mut RustWriter) {
        splat!(out; [#: &self.target.name] [?(!self.target.generics.is_empty()) < [fmt_generics(
            out, &self.target.generics, USE
        )]>])
    }
    fn new(out: &mut RustWriter, target: &'a DeriveTargetInner) -> Ctx<'a> {
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
    let start = out.buf.len();
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
    if let Some(version) = field.attr.version() {
        let group = out.split_off_stream(start);
        splat! {
            out;
            if version >= [out.buf.push(TokenTree::Literal(version.clone()))]
            [out.buf.push(TokenTree::Group(Group::new(Delimiter::Brace, group)))]
            else {
                [if let Some(DefaultKind::Custom(field)) = field.default(FROM_BINARY) {
                    out.buf.extend_from_slice(&field);
                } else {
                    splat!(out; Default::default())
                }]
            }
        };
    }
}
impl Ctx<'_> {
    // #[allow(non_snake_case)]
    // fn FromJson(&self, out: &mut RustWriter) {
    //     splat!(out;  [~&self.crate_path]::FromJson<#[#self.lifetime]>)
    // }

    #[allow(non_snake_case)]
    fn FromBinary(&self, out: &mut RustWriter) {
        splat!(out;  [~&self.crate_path]::FromBinary<#[#: &self.lifetime]>)
    }
}

fn schema_field_decode(out: &mut RustWriter, ctx: &Ctx, field: &Field) {
    if let Some(with) = field.with(FROM_JSON) {
        splat! { out;
           [~&ctx.crate_path]::__internal::emplace_json_for_with_attribute::<&mut ::jsony::parser::Parser<#[#: &ctx.lifetime]>, [~field.ty], _>(
               &[~with]::decode_json
           )
        }
    } else {
        splat! { out; [~&ctx.crate_path]::__internal::erased_emplace_from_json::<#[#: &ctx.lifetime], [~field.ty]>() }
    }
}

fn field_name_literal(ctx: &Ctx, field: &Field) -> Literal {
    // todo pass context in
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

// Probably can merge with field name json
fn variant_name_json(ctx: &Ctx, field: &EnumVariant, output: &mut String) {
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
}

fn field_name_json(ctx: &Ctx, field: &Field, output: &mut String) {
    output.push('"');
    if let Some(name) = &field.attr.rename(TO_JSON) {
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
}

fn struct_schema(out: &mut RustWriter, ctx: &Ctx, fields: &[&Field], temp_tuple: Option<&Ident>) {
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
                        splat!(out; [#: &ty], [@Literal::usize_unsuffixed(i).into()])
                    } else {
                        splat!(out; [#: &ctx.target.name][@ag_gen.clone()], [#: field.name])
                    }
                ]),
                decode: [schema_field_decode(out, ctx, field)]
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
            match default {
                ast::DefaultKind::Default => {
                    splat!{
                        out;
                        ::jsony::__internal::default_default::<[~field.ty]>,
                    }

                },
                ast::DefaultKind::Custom(expr) => {
                    splat!{
                        out;
                        |ptr: ::std::ptr::NonNull<()>| {
                            //todo need to guard gaainst returns and such
                            let value: [~field.ty] = [~expr];
                            unsafe {
                                ptr.cast().write(value);
                            }
                            ::jsony::__internal::UnsafeReturn
                        },
                    }
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
        [] => {
            throw!("FromJson not implemented for Tuples without fields yet.")
        }
        [field] => {
            //tod immple with
            splat!(out;
                < [~field.ty] as ::jsony::FromJson<#[#: &ctx.lifetime]> >::emplace_from_json(
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
    impl_from_json(out, ctx, stream);
}

fn tuple_struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
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
                (&self.[@TokenTree::Literal(Literal::usize_unsuffixed(0))], out)
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
                    ](&self.[@TokenTree::Literal(Literal::usize_unsuffixed(i))], out);
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
            splat!(out; | [#: binding]: &[~ty] | [~rest]);
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
) {
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
                        splat!(out; &self.[#: field.name])
                    } else {
                        splat!(out; [#: &ctx.temp[i]])
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
                field_name_json(ctx, field, text);
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
                            splat!(out; &self.[#: field.name])
                        } else {
                            splat!(out; [#: &ctx.temp[i]])
                        }]).iter() {
                            let _: ::jsony::json::AlwaysString = key.encode_json__jsony(out);
                            out.push_colon();
                            value.encode_json__jsony(out);
                            out.push_comma();
                        }
                    );
                } else {
                    throw!("ToJson Via = Iterator, only supported with flatten currently." @ field.name.span());
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
                    splat!(out; &self.[#: field.name])
                } else {
                    splat!(out; [#: &ctx.temp[i]])
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
                            [
                                match explicit_default {
                                    ast::DefaultKind::Default => {
                                        splat!(out; Default::default())
                                    },
                                    ast::DefaultKind::Custom(expr) => {
                                        splat!(out; [~expr])
                                    },
                                }
                            ]
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

fn struct_to_json(out: &mut RustWriter, ctx: &Ctx, fields: &[Field]) {
    let mut text = String::new();
    let body = token_stream!(
        out;
        out.start_json_object();
        [inner_struct_to_json(out, ctx, fields, &mut text, true)]
        out.end_json_object()
    );

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

    splat!(out;
        [?(ctx.target.flattenable)
        const unsafe fn __schema_inner<#[#: &ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, DEF)]] >()
            ~> ::jsony::__internal::ObjectSchema<#[#: &ctx.lifetime]>
            [?(!ctx.target.where_clauses.is_empty() || !ctx.target.generic_field_types.is_empty())
            where [
            for (ty in &ctx.target.generic_field_types) {
                [~ty]: FromJson<#[#: &ctx.lifetime]>
            }
            ]]
            {
                ::jsony::__internal::ObjectSchema::<#[#: &ctx.lifetime]> {
                    inner: &const { [struct_schema(out, ctx, &ordered_fields, None)] },
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
                            dst.byte_add(offset_of!([ctx.dead_target_type(out)], [#: flatten_field.name])),
                            parser,
                        );
                )
            }
            if has_skips {
                splat!(out; let __result = );
            }
            if ctx.target.flattenable {
                splat!(out; __schema_inner::<
                    #[#: &ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
                >())
            } else {
                splat!(out;
                ::jsony::__internal::ObjectSchema::<#[#: &ctx.lifetime]> {
                    inner: &const { [struct_schema(out, ctx, &ordered_fields, None)] },
                    phantom: ::std::marker::PhantomData
                })
            }
            let mut any_field_flag = 0;
            for field in fields {
                any_field_flag |= field.flags;
            }
            let has_alias = any_field_flag & Field::WITH_FROM_JSON_ALIAS != 0;
            splat!(out; .[
                if has_alias {
                    splat!(out; decode_with_alias)
                } else {
                    splat!(out; decode)
                }
            ](dst, parser, [if flattening.is_some() {
                splat!(out; Some(&mut __flatten_visitor_jsony))
            } else {
                splat!(out; None)
            }]
            [?(has_alias) , &[[
                [
                    for (i, field) in ordered_fields.iter().enumerate() {
                        if let Some(alias) = field.attr.alias(FROM_JSON) {
                            splat!(out; (
                                [@TokenTree::Literal(Literal::usize_unsuffixed(i))],
                                [@TokenTree::Literal(alias.clone())]
                            ),)
                        }
                    }
                ]
            ]] ]
            ));
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
                            dst.byte_add(::std::mem::offset_of!([ctx.dead_target_type(out)], [#: field.name]))
                                .cast::<[~field.ty]>()
                                .write([field_from_default(out, field, FROM_JSON)]);
                        )
                    }]

                    Ok(())
                );
            }
            let ts = out.split_off_stream(start);
            impl_from_json(out, ctx, ts);
            if ctx.target.flattenable {
                if has_skips {
                    throw!("Flattenable does not yet support skipped fields")
                }
                let body = token_stream!(
                    out;
                    jsony::__internal::DynamicFieldDecoder {
                        destination: dst,
                        schema: __schema_inner::<
                            #[#: &ctx.lifetime] [?(!ctx.generics.is_empty()), [fmt_generics(out, ctx.generics, USE)]]
                        >(),
                        bitset: [@TokenTree::Literal(Literal::u64_unsuffixed(0))],
                        required: [@TokenTree::Literal(Literal::u64_unsuffixed(required_bitset(&ordered_fields)))],
                    }
                );
                impl_from_json_field_visitor(
                    out,
                    ctx,
                    &|out| splat!(out; jsony::__internal::DynamicFieldDecoder<#[#: &ctx.lifetime]>),
                    body
                )

            }
        ]
    );
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

fn enum_to_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let mut text = String::with_capacity(64);
    let start = out.buf.len();
    // TODO make this logic smarter
    let all_objects =
        ctx.target.enum_flags & (ENUM_CONTAINS_UNIT_VARIANT | ENUM_CONTAINS_TUPLE_VARIANT) == 0;
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
                splat!{out; [#: &ctx.target.name]::[#: variant.name]}
                match variant.kind {
                    EnumKind::Tuple => {
                        splat!{out; (
                            [for (i, _) in variant.fields.iter().enumerate() { splat!(out; [#: &ctx.temp[i]],) }]
                        ) => [
                            text.clear();
                            enum_variant_to_json(out, ctx, variant, &mut text, all_objects);
                        ]}
                    },
                    EnumKind::Struct => {
                        splat!{out; {
                            [for (i, field) in variant.fields.iter().enumerate() { splat!(out; [#: field.name]: [#: &ctx.temp[i]],) }]
                        } => [
                            text.clear();
                            enum_variant_to_json(out, ctx, variant, &mut text, all_objects);
                        ]}
                    },
                    EnumKind::None => {
                        splat!{out; => [
                                text.clear();
                                enum_variant_to_json(out, ctx, variant, &mut text, all_objects);
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
) {
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
        [inner_struct_to_json(out, ctx, &variant.fields, text, false)];
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
                //optimizations can be done here depending
                // whether all the enum variants None or a object etect
                // to move this out of each variant.
                if !all_objects {
                    splat!(out; out.start_json_object(););
                }
                variant_name_json(ctx, variant, text);
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
                ([#: &ctx.temp[0]], out);
            }
        }
        EnumKind::Struct => {
            enum_variant_to_json_struct(out, ctx, variant, text);
        }
        EnumKind::None => {
            variant_name_json(ctx, variant, text);
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
}
fn enum_variant_from_json_struct(
    out: &mut RustWriter,
    ctx: &Ctx,
    variant: &EnumVariant,
    untagged: bool,
) {
    let ordered_fields = schema_ordered_fields(variant.fields);
    let mut flattening: Option<&Field> = None;
    for field in variant.fields {
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
    if let Some(flatten_field) = flattening {
        splat! {
            out;
            // Note a type alias with an un-used generic is not an error
            type __TEMP[?(ctx.target.has_lifetime())<#[#: &ctx.lifetime]>] = ([for (field in &ordered_fields) { [~field.ty], }]);
            let schema = ::jsony::__internal::ObjectSchema{
                inner: const { &[struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
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
                dst.cast::<[ctx.target_type(out)]>().write([#: &ctx.target.name]::[#: variant.name] {
                    [for ((i, field) in ordered_fields.iter().enumerate()) {
                        [#: field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                    }]
                    [#: flatten_field.name]: temp_flatten.assume_init(),
                    [for field in variant.fields {
                        if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                            continue;
                        }
                        splat!(out; [#: field.name]: [field_from_default(out, field, FROM_JSON)],)
                    }]
                });
                [?(untagged || ctx.target.ignore_tag_adjacent_fields) break #success]
            }
        }
    } else {
        splat! {
            out;
            // Note a type alias with an un-used generic is not an error
            type __TEMP[?(ctx.target.has_lifetime())<#[#: &ctx.lifetime]>] = ([for (field in &ordered_fields) { [~field.ty], }]);
            let schema = ::jsony::__internal::ObjectSchema{
                inner: const { &[struct_schema(out, ctx, &ordered_fields, Some(&Ident::new("__TEMP", Span::call_site())))]},
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
                dst.cast::<[ctx.target_type(out)]>().write([#: &ctx.target.name]::[#: variant.name] {
                    [for ((i, field) in ordered_fields.iter().enumerate()) {
                        [#: field.name]: temp2.[@Literal::usize_unsuffixed(i).into()],
                    }]
                    [for field in variant.fields {
                        if field.flags & Field::WITH_FROM_JSON_SKIP == 0 {
                            continue;
                        }
                        splat!(out; [#: field.name]: [field_from_default(out, field, FROM_JSON)],)
                    }]
                });
                [?(untagged || ctx.target.ignore_tag_adjacent_fields) break #success]
            }
        }
    };
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

fn enum_variant_from_json(out: &mut RustWriter, ctx: &Ctx, variant: &EnumVariant, untagged: bool) {
    let start = out.buf.len();
    match variant.kind {
        EnumKind::Tuple => {
            if ctx.target.content.is_some() {
                splat! {
                    out;
                    if !at_content {
                        return Err(&::jsony::error::MISSING_CONTENT_TAG)
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
                        splat!(out; < [~field.ty] as ::jsony::FromJson<#[#: &ctx.lifetime]> >)
                    }
                ]::decode_json(parser) {
                    Ok(value) => {
                        dst.cast::<[ctx.target_type(out)]>().write([#: &ctx.target.name]::[#: variant.name](value));
                        [?(untagged || ctx.target.ignore_tag_adjacent_fields) break #success]
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
                        return Err(&::jsony::error::MISSING_CONTENT_TAG)
                    }
                }
            }
            enum_variant_from_json_struct(out, ctx, variant, untagged);
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
            dst.cast::<[ctx.target_type(out)]>().write([#: &ctx.target.name]::[#: variant.name]);
            [?(untagged || ctx.target.ignore_tag_adjacent_fields) break #success]
        },
    };
    out.tt_group(Delimiter::Brace, start);
}

fn stringly_enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
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
                            => [#: &ctx.target.name]::[#: &variant.name],
                    }]
                    [enum_from_json_unknown_variant(out, ctx, other, true)]
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
) {
    splat!(out; _ =>);
    let start = out.buf.len();
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
            [#: &ctx.target.name]::[#: other.name]
            [if let [field] = other.fields {
                match other.kind {
                    EnumKind::Tuple => splat!(out; (other_tag)),
                    EnumKind::Struct => splat!(out; {[#: field.name]: other_tag}),
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
        if ctx.target.ignore_tag_adjacent_fields && !stringly {
            splat!(
                out;
                if let Err(err) = parser.skip_value() {
                    return Err(err)
                }
                match parser.object_step() {
                    Ok(None) => return Err(&jsony::error::NO_FIELD_MATCHED_AN_ENUM_VARIANT),
                    Ok(Some(next_field)) => {
                        variant = next_field;
                        continue;
                    },
                    Err(err) => return Err(err),
                };
            )
        } else {
            splat!(
                out;
                let variant = variant.to_string();
                parser.report_error(variant);
                return Err(&::jsony::error::UNKNOWN_VARIANT);
            )
        }
    }
    out.tt_group(Delimiter::Brace, start);
}

fn enum_from_json(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    if ctx.target.flattenable {
        throw!("Flattening enums not supported yet.")
    }
    let mut mixed_strings_and_objects = false;
    let inline_tag = match &ctx.target.tag {
        Tag::Inline(literal) => Some(literal),
        Tag::Untagged => {
            let outer = out.buf.len();
            splat!(out; let snapshot = parser.snapshot(); #success:);
            let start = out.buf.len();
            'always_succeed: {
                for (i, variant) in variants.iter().enumerate() {
                    if let EnumKind::None = variant.kind {
                        splat!(
                            out;
                            dst.cast::<[ctx.target_type(out)]>().write([#: &ctx.target.name]::[#: variant.name]);
                        );
                        break 'always_succeed;
                    }
                    splat!(
                        out;
                        [?(i != 0) parser.restore_for_retry(&snapshot); ]
                        [enum_variant_from_json(out, ctx, variant, true)]
                    )
                }
                splat!(
                    out;
                    return Err(&::jsony::json::DecodeError{
                        message: [@Literal::string("Untagged enum didn't match any variant").into()]
                    })
                )
            }
            out.tt_group(Delimiter::Brace, start);
            splat!(out; Ok(()));
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
    let mut other: Option<&EnumVariant> = None;
    for variant in variants {
        if variant.attr.has_other() {
            if other.is_some() {
                throw!("Only one other variant is currently supported." @ variant.name.span())
            }
            other = Some(variant);
        }
    }
    let body_start = out.buf.len();
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
            let Ok(Some([?(ctx.target.ignore_tag_adjacent_fields) mut] variant)) = parser.[
                //todo could opt this
                if mixed_strings_and_objects {
                    splat!(out; enter_seen_object)
                } else {
                    splat!(out; enter_object)
                }
            ]() else {
                //todo better errors
                return Err(&::jsony::error::EMPTY_OBJECT_FOR_EXTERNALLY_TAGGED_ENUM);
            };
        )
    }
    if ctx.target.ignore_tag_adjacent_fields {
        splat!(out; #success: loop)
    }
    let variant_dispatch_start = out.buf.len();
    splat! { out;
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
                        => [enum_variant_from_json(out, ctx, variant, false)],
                );
            }]
            [enum_from_json_unknown_variant(out, ctx, other, false)]
        }
    };

    if let Some(_) = inline_tag {
        if ctx.target.content.is_some() {
            //todo this leaks?
            splat!(out; if at_content {parser.discard_remaining_object_fields()} else {Ok(())})
        } else {
            splat!(out; Ok(()))
        }
    } else if ctx.target.ignore_tag_adjacent_fields {
        // computed from the above splat!(out; #success: loop)
        out.tt_group(Delimiter::Brace, variant_dispatch_start);
        splat!(
            out;
            match parser.discard_remaining_object_fields() {
                Ok(()) => return Ok(()),
                err => {
                    std::ptr::drop_in_place::<[ctx.target_type(out)]>(dst.cast().as_mut());
                    return err
                },
            };
        )
    } else {
        splat!(
            out;
            let err = match parser.object_step() {
                Ok(None) => {return Ok(())},
                Err(err) => {Err(err)},
                Ok(Some(_)) => {
                    Err(&jsony::error::MULTIPLE_FIELDS_FOR_EXTERNALLY_TAGGED_ENUM)
                },
            };
            std::ptr::drop_in_place::<[ctx.target_type(out)]>(dst.cast().as_mut());
            return err;
        )
    }

    let mut body = out.split_off_stream(body_start);

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
                                    splat!(out; [@variant_key_literal(ctx, variant).into()] => [#: &ctx.target.name]::[#: variant.name],);
                                }
                            }]
                            [enum_from_json_unknown_variant(out, ctx, other, true)]
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

    impl_from_json(out, ctx, body);
}

fn handle_pod_binary_any_struct(out: &mut RustWriter, ctx: &Ctx<'_>, fields: &[Field]) -> bool {
    if !ctx.target.generics.is_empty() {
        throw!("Pod derive doesn't support generics or lifetimes yet.");
    }
    if !matches!(ctx.target.repr, ast::Repr::Transparent | ast::Repr::C) {
        throw!("Pod type must be either repr(transparent) or repr(C)");
    }
    splat!(out;
        use ::std::mem::{size_of, offset_of};
        // Don't need to do this because of the repr requirement in theory we could support
        // pod types using the follow asserts without requiring Repr(C) but then the it would
        // depend on the compiler version. In such a case it we would have a to TryPod or some
        // such where the POD is merely optimization.
        // [?(!matches!(ctx.target.repr, ast::Repr::Transparent | ast::Repr::C))
        //     assert!(
        //         {
        //             [for (field in fields) {
        //                 let [#: field.name] = offset_of!([
        //                     splat!(out; [ctx.dead_target_type(out)], [#: field.name])
        //                 ]);
        //             }]
        //             [for ((i, field) in fields.windows(2).enumerate()) {
        //                 [?(i != 0) &]
        //                 // todo should be less or equal
        //                 ( [#: field[0].name] < [#: field[1].name])
        //             }]

        //         },
        //         [out.buf.push(TokenTree::Literal(Literal::string("Not all fields are ordered")))]
        //     );
        // ]
        assert!(
            [for ((i, field) in fields.iter().enumerate()) {
                [?(i != 0) &]
                <[~field.ty] as jsony::[
                    if ctx.target.from_binary {
                        splat!(out; FromBinary);
                    } else {
                        splat!(out; ToBinary);
                    }
                ]>::POD
            }],
            [out.buf.push(TokenTree::Literal(Literal::string("Not all fields implement POD")))]
        );

        assert!(
            size_of::<[#ctx.target.name]>() ==
            [for ((i, field) in fields.iter().enumerate()) {
                [?(i != 0) +]
                size_of::<[~field.ty]>()
            }],
            [out.buf.push(TokenTree::Literal(Literal::string("struct has gaps between fields")))]
        );
    );

    if fields.len() < 2 {
        return false;
    }

    // Note we currently omit the endian transform for now, as I doubt anyone
    // will make use of it before jsony 0.2 which significantly restructure how
    // the system works.
    if ctx.target.to_binary {
        let start = out.buf.len();
        splat! { out; unsafe {
            encoder.push_as_bytes(self);
        }};
        let body = out.split_off_stream(start);
        impl_to_binary(out, &ctx, body, None);
    }

    if ctx.target.from_binary {
        let start = out.buf.len();
        splat! {out;
            unsafe { decoder.pod_type() }
        };
        let body = out.split_off_stream(start);
        impl_from_binary(out, &ctx, body, None);
    }
    true
}

// bincode test
fn handle_struct(output: &mut RustWriter, target: &DeriveTargetInner, fields: &[Field]) {
    let ctx = Ctx::new(output, target);
    if target.from_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                throw!("Struct must contain a single field to use transparent")
            };
            if !matches!(target.repr, ast::Repr::Transparent) {
                throw!("transparent FromJson requires #[repr(transparent)]")
            }
            let body = token_stream! {
                output;
                <[~single_field.ty] as [~&ctx.crate_path]::FromJson>::emplace_from_json(
                    dst, parser
                )
            };
            impl_from_json(output, &ctx, body);
        } else {
            struct_from_json(output, &ctx, fields);
        }
    }
    if target.to_json {
        if target.transparent_impl {
            let [single_field] = fields else {
                throw!("Struct must contain a single field to use transparent")
            };
            let body = token_stream! {output;
                self.[#: single_field.name].encode_json__jsony(out)
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
    if target.transparent_impl && matches!(target.repr, ast::Repr::Transparent | ast::Repr::C) {
        if let [field] = &fields {
            auto_pod = Some(field);
        }
    }

    if target.to_binary {
        let start = output.buf.len();
        if let Some(version) = target.version {
            splat! {output;
                encoder.push([output.buf.push(TokenTree::Literal(Literal::u16_unsuffixed(version)))]);
            }
        }

        for field in fields {
            encode_binary_field(
                output,
                &ctx,
                field,
                &|out| splat! {out; &self.[#: field.name]},
            )
        }
        let body = output.split_off_stream(start);
        impl_to_binary(output, &ctx, body, auto_pod);
    }

    if target.from_binary {
        let start = output.buf.len();
        splat! {output;
            [decode_binary_version(output, &ctx)]
            [#: &target.name] {
                [for field in fields {
                    splat!{(output);
                        [#: field.name]: [decode_binary_field(output, &ctx, field)],
                    }
                }]
            }
        };
        let body = output.split_off_stream(start);
        impl_from_binary(output, &ctx, body, auto_pod);
    }
}

fn decode_binary_version(out: &mut RustWriter, ctx: &Ctx) {
    let Some(version) = ctx.target.version else {
        return;
    };
    splat!(
        out;
        let version = decoder.byte();
        // todo what if we support all the version
        if version > [@TokenTree::Literal(Literal::u16_unsuffixed(version))]
            [?(ctx.target.min_version > 0) || version <  [@TokenTree::Literal(Literal::u16_unsuffixed(ctx.target.min_version))]]
         {
            decoder.report_error(
                format_args!(
                    [@TokenTree::Literal(Literal::string("{} unknown version = {} found. (versions {}..={} supported)"))],
                    [@TokenTree::Literal(Literal::string(&ctx.target.name.to_string()))],
                    version,
                    [@TokenTree::Literal(Literal::u16_unsuffixed(ctx.target.min_version))],
                    [@TokenTree::Literal(Literal::u16_unsuffixed(version))],
                )

            );
        }
    );
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
    if target.transparent_impl && matches!(target.repr, ast::Repr::Transparent | ast::Repr::C) {
        if let [field] = &fields {
            auto_pod = Some(field);
        }
    }

    if target.to_binary {
        let body = token_stream! {output;
            [?(let Some(version) = target.version) encoder.push([output.buf.push(TokenTree::Literal(Literal::u16_unsuffixed(version)))]);]
            [
            for (i, field) in fields.iter().enumerate() {
                encode_binary_field(output, &ctx, field, &|out| splat!{out; &self.[@TokenTree::Literal(Literal::usize_unsuffixed(i))]})
            }
        ]};
        impl_to_binary(output, &ctx, body, auto_pod)
    }

    if target.from_binary {
        let body = token_stream! {output;
            [decode_binary_version(output, &ctx)]
            [#: &target.name] (
                [for field in fields {
                    splat!{(output); [decode_binary_field(output, &ctx, field)], }
                }]
            )
        };
        impl_from_binary(output, &ctx, body, auto_pod)
    }
}

fn enum_to_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let target = &ctx.target;
    let any_generics = !target.generics.is_empty();
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        throw!("FromStr enum must not have any tuple or struct variants nor a tag configuratino")
    }
    splat! {
        out;
        ~[[automatically_derived]]
        impl [?(any_generics) < [fmt_generics(out, &target.generics, DEF)] >]
          [#: &target.name][?(any_generics) <
            [fmt_generics( out, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty())
            where [] [~&target.where_clauses]] {
            pub fn to_str(&self) -> & # static str {
                match self {
                    [for (variant in variants) {
                        [#: &target.name]::[#: variant.name] => [@variant_key_literal(ctx, variant).into()],
                    }]
                }
            }
        }
    };
}

fn enum_from_str(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let target = &ctx.target;
    if target.enum_flags != (ENUM_CONTAINS_UNIT_VARIANT | ENUM_HAS_EXTERNAL_TAG) {
        throw!("FromStr enum must not have any tuple or struct variants nor a tag configuratino")
    }
    let any_generics = !target.generics.is_empty();
    splat! {
        out;
        ~[[automatically_derived]]
        impl [?(any_generics) < [fmt_generics(out, &target.generics, DEF)] >]
        ::std::str::FromStr for
          [#: &target.name][?(any_generics) <
            [fmt_generics( out, &target.generics, USE)]
        >]  [?(!target.where_clauses.is_empty())
            where [] [~&target.where_clauses]] {
                type Err = ::jsony::error::UnknownVariant;
            fn from_str(value: &str) -> Result<Self, ::jsony::error::UnknownVariant> {
                Ok(match value {
                    [for (variant in variants) {
                        [@variant_key_literal(ctx, variant).into()] => [#: &target.name]::[#: variant.name],
                    }]
                    _ => return Err(::jsony::error::UnknownVariant)
                })
            }
        }
    };
}

fn enum_to_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let body = token_stream! { out;
        [?(let Some(version) = ctx.target.version) encoder.push([out.buf.push(TokenTree::Literal(Literal::u16_unsuffixed(version)))]);]
        match self {[
            for (i, variant) in variants.iter().enumerate() {
                splat!{out; [#: &ctx.target.name]::[#: variant.name]}
                match variant.kind {
                    EnumKind::Tuple => {
                        splat!{out; (
                            [for (i, _) in variant.fields.iter().enumerate() { splat!(out; [#: &ctx.temp[i]],) }]
                        ) => {
                            encoder.push([@TokenTree::Literal(Literal::u8_unsuffixed(i as u8))]);
                            [for (i, field) in variant.fields.iter().enumerate() {
                                encode_binary_field(out, ctx, field, &|out| splat!{out; [#: &ctx.temp[i]]})
                            }]
                        }}
                    },
                    EnumKind::Struct => {
                        splat!{out; {
                            [for field in variant.fields { splat!(out; [#: field.name],) }]
                        } => {
                            encoder.push([@TokenTree::Literal(Literal::u8_unsuffixed(i as u8))]);
                            [for field in variant.fields {
                                encode_binary_field(out, ctx, field, &|out| splat!{out; [#: field.name]})
                            }]
                        }}
                    },
                    EnumKind::None => {
                        splat!{out;  => {
                            encoder.push([#TokenTree::Literal(Literal::u8_unsuffixed(i as u8))]);
                        }}
                    },
                }
            }
        ]}
    };
    impl_to_binary(out, ctx, body, None);
}

fn enum_from_binary(out: &mut RustWriter, ctx: &Ctx, variants: &[EnumVariant]) {
    let body = token_stream! { out;
        // TODO ENUM SHOUlD support per variant versions.
        [decode_binary_version(out, ctx)]
        match decoder.byte() {[
            for (i, variant) in variants.iter().enumerate() {
                splat!{out; [if i + 1 == variants.len() {
                    splat!(out; _)
                }else {
                    splat!(out; [@TokenTree::Literal(Literal::u8_unsuffixed(i as u8))])
                }] =>
                    [match variant.kind {
                        EnumKind::Tuple => {
                            splat!{out;
                                [#: &ctx.target.name]::[#: variant.name](
                                    [for field in variant.fields {
                                        splat!{out; [decode_binary_field(out, ctx, field)], }
                                    }]
                                )
                            }
                        }
                        EnumKind::Struct => {
                            splat!{out;
                                [#: &ctx.target.name]::[#: variant.name]{
                                    [for field in variant.fields {
                                        splat!{out; [#: field.name]: [decode_binary_field(out, ctx, field)], }
                                    }]
                                }
                            }
                        }
                        EnumKind::None => splat!{out; [#: &ctx.target.name]::[#: variant.name] }
                    }],
                }
            }
        ]}
    };
    impl_from_binary(out, ctx, body, None);
}

fn handle_enum(output: &mut RustWriter, target: &DeriveTargetInner, variants: &[EnumVariant]) {
    let mut ctx = Ctx::new(output, target);
    let mut max_tuples = 0;
    for var in variants {
        if matches!(var.kind, EnumKind::Tuple | EnumKind::Struct) {
            max_tuples = max_tuples.max(var.fields.len());
        }
    }
    ctx.temp = (0..max_tuples).map(var).collect::<Vec<_>>();
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
        repr: ast::Repr::Default,
        version: None,
        min_version: 0,
    };
    let (kind, body) = ast::extract_derive_target(&mut target, &outer_tokens);

    // Default to from json
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
    token_stream!(
        (&mut rust_writer);
        ~[[allow(clippy::question_mark)]]
        const _: () = [@TokenTree::Group(Group::new(Delimiter::Brace, ts))];
    )
}

pub fn derive(stream: TokenStream) -> TokenStream {
    Error::try_catch_handle(stream, inner_derive)
}
