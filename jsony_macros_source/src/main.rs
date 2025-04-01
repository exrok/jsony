#![allow(clippy::question_mark)]
mod ast;
mod case;
mod codegen;
mod lit;
mod template;
mod writer;
// mod template2;
#[allow(unused)]
mod util;

use std::str::FromStr;

use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
pub(crate) fn default_call_tokens(span: Span) -> Vec<TokenTree> {
    vec![
        TokenTree::Ident(Ident::new("Default", span)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("default", span)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
    ]
}

/// Not we store this here to avoid issues with cargo expand
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum Flatten {
    None,
    Object,
    Array,
}

#[derive(Debug)]
struct InnerError {
    span: Span,
    message: String,
}
#[derive(Debug)]
pub(crate) struct Error(Box<InnerError>);

impl Error {
    pub(crate) fn to_compiler_error(&self) -> TokenStream {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.0.message))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(self.0.span);
        group.set_span(self.0.span);

        let stmt = TokenStream::from_iter([
            TokenTree::Ident(Ident::new("compile_error", self.0.span)),
            punc,
            group,
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ]);
        TokenStream::from_iter([TokenTree::Group(Group::new(
            Delimiter::Brace,
            TokenStream::from_iter([stmt]),
        ))])
    }
    #[cold]
    #[inline(never)]
    #[track_caller]
    pub(crate) fn msg(message: &str) -> Error {
        println!("{}> {}", std::panic::Location::caller(), message);
        Error(Box::new(InnerError {
            span: Span::call_site(),
            message: message.to_string(),
        }))
    }
    #[cold]
    #[inline(never)]
    #[track_caller]
    #[rustfmt::skip]
    pub(crate) fn msg_ctx(message: &str, fmt: &dyn std::fmt::Display) -> Error {
        println!( "{}> {}: {}", std::panic::Location::caller(), message, fmt);
        Error(Box::new(InnerError {
            span: Span::call_site(),
            message: format!("{}: {}", message, fmt),
        }))
    }
    #[cold]
    #[inline(never)]
    #[track_caller]
    pub(crate) fn span_msg(message: &str, span: Span) -> Error {
        println!("{}> {}", std::panic::Location::caller(), message);
        Error(Box::new(InnerError {
            span,
            message: message.to_string(),
        }))
    }
    #[cold]
    #[inline(never)]
    #[track_caller]
    pub(crate) fn span_msg_ctx(message: &str, fmt: &dyn std::fmt::Display, span: Span) -> Error {
        println!("{}> {}", std::panic::Location::caller(), message);
        Error(Box::new(InnerError {
            span,
            message: format!("{}: {}", fmt, message),
        }))
    }
}

#[allow(dead_code, unused)]
macro_rules! tokens {
    ($($tt:tt)*) => {
        TokenStream::from_str(stringify!($($tt)*)).unwrap()
    };
}

fn main() {
    if false {
        template::array(tokens!());
        template::object(tokens!());
        codegen::derive(tokens!());
    }
    util::print_pretty(codegen::derive(tokens! {

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(Binary, Json)]
    struct Sys<'a> {
        #[jsony(From with = owned_cow)]
        shared: Cow<'a, [&'a str]>,
        beta: bool,
    }

    }));
}
