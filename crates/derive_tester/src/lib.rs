//! Near-exhaustive testing framework for `#[derive(Jsony)]`.
//!
//! The schema model, token codegen, data generator, and the direct-`rustc`
//! compile pipeline are copied verbatim from the jsony build-time benchmark
//! (`schema`, `token`, `datagen`, `features`, `library`, `bench`, `task`). The
//! test-mode layers (`gen`, `casing`, `emit`, `compile`, `run`) are built on
//! top.
#![allow(dead_code)]

use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, TokenStream, TokenTree};

// pub(crate) so $crate:: paths in the codegen macros can reach them.

pub(crate) fn tt_punct_alone(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Alone)));
}
pub(crate) fn tt_punct_joint(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Joint)));
}
pub(crate) fn tt_ident(out: &mut Vec<TokenTree>, ident: &'static str) {
    out.push(TokenTree::Ident(Ident::new_static(ident)));
}
pub(crate) fn tt_group_empty(out: &mut Vec<TokenTree>, delimiter: Delimiter) {
    out.push(TokenTree::Group(Group::new(delimiter, TokenStream::new())));
}
pub(crate) fn tt_group(out: &mut Vec<TokenTree>, delimiter: Delimiter, from: usize) {
    let group = TokenTree::Group(Group::new(
        delimiter,
        TokenStream::from_iter(out.drain(from..)),
    ));
    out.push(group);
}

#[rustfmt::skip]
macro_rules! append_tok {
    ($ident:ident $d:tt) => {
       $crate::tt_ident($d, stringify!($ident))
    };
    ({} $d: tt) => {
        $crate::tt_group_empty($d, proc_macro2::Delimiter::Brace);
    };
    (() $d: tt) => {
        $crate::tt_group_empty($d, proc_macro2::Delimiter::Parenthesis);
    };
    ([] $d:tt) => {
        $crate::tt_group_empty($d, proc_macro2::Delimiter::Bracket);
    };
    ({$($tt:tt)*} $d: tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* $crate::tt_group($d, proc_macro2::Delimiter::Brace, at);
    }};
    (($($tt:tt)*) $d: tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* $crate::tt_group($d, proc_macro2::Delimiter::Parenthesis, at);
    }};
    ([[$($tt:tt)*]] $d:tt) => {{
        let at = $d.len(); $(append_tok!($tt $d);)* $crate::tt_group($d, proc_macro2::Delimiter::Bracket, at);
    }};
    (_ $d:tt) => { $crate::tt_ident($d, "_") };
    ([$ident:ident] $d:tt) => {
        $d.push($($tt)*)
    };
    ([?($($cond:tt)*) $($body:tt)*] $d:tt) => {
        if $($cond)* { $(append_tok!($body $d);)* }
    };
    ([@$($tt:tt)*] $d:tt) => {
        $d.push(($($tt)*).into())
    };
    ([try $($tt:tt)*] $d:tt) => {
        if let Err(err) = $($tt)* { return Err(err); }
    };
    ([for ($($iter:tt)*) {$($body:tt)*}] $d:tt) => {
        for $($iter)* { $(append_tok!($body $d);)* }
    };
    ([#$($tt:tt)*] $d:tt) => {
        $d.push(proc_macro2::TokenTree::from($($tt)*.clone()))
    };
    ([~$($tt:tt)*] $d:tt) => {
        $d.extend_from_slice($($tt)*)
    };
    ([$($rust:tt)*] $d:tt) => {{
         $($rust)*
    }};
    (~ $d:tt) => { $crate::tt_punct_joint($d, '\'') };
    (: $d:tt) => { $crate::tt_punct_alone($d, ':') };
    (# $d:tt) => { $crate::tt_punct_joint($d, '#') };
    (< $d:tt) => { $crate::tt_punct_alone($d, '<') };
    (% $d:tt) => { $crate::tt_punct_joint($d, ':') };
    (:: $d:tt) => { $crate::tt_punct_joint($d, ':'); $crate::tt_punct_alone($d, ':'); };
    (.. $d:tt) => { $crate::tt_punct_joint($d, '.'); $crate::tt_punct_alone($d, '.'); };
    (-> $d:tt) => { $crate::tt_punct_joint($d, '-'); $crate::tt_punct_alone($d, '>'); };
    (=> $d:tt) => { $crate::tt_punct_joint($d, '='); $crate::tt_punct_alone($d, '>'); };
    (== $d:tt) => { $crate::tt_punct_joint($d, '='); $crate::tt_punct_alone($d, '='); };
    (> $d:tt) => { $crate::tt_punct_alone($d, '>') };
    (! $d:tt) => { $crate::tt_punct_alone($d, '!') };
    (| $d:tt) => { $crate::tt_punct_alone($d, '|') };
    (+ $d:tt) => { $crate::tt_punct_alone($d, '+') };
    (. $d:tt) => { $crate::tt_punct_alone($d, '.') };
    (; $d:tt) => { $crate::tt_punct_alone($d, ';') };
    (& $d:tt) => { $crate::tt_punct_alone($d, '&') };
    (= $d:tt) => { $crate::tt_punct_alone($d, '=') };
    (, $d:tt) => { $crate::tt_punct_alone($d, ',') };
    (* $d:tt) => { $crate::tt_punct_alone($d, '*') };
    ($literal:literal $d:tt) => {
        $d.push(proc_macro2::TokenTree::Literal(proc_macro2::Literal::string($literal)))
    };
}

macro_rules! splat { ($d:tt; $($tt:tt)*) => { { $(append_tok!($tt $d);)* } } }
macro_rules! sfn { ($d:tt; $($tt:tt)*) => { & (|$d: &mut Vec<proc_macro2::TokenTree>| { $(append_tok!($tt $d);)* }) } }

// --- Copied verbatim from the build-time benchmark ---
mod bench;
mod datagen;
mod features;
mod library;
mod schema;
mod task;
mod token;

// --- Test-mode layers built on top ---
mod casing;
mod compile;
mod emit;
mod gen;
pub mod run;
