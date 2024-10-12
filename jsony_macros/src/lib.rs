#![allow(warnings)]
extern crate proc_macro;
mod ast;
mod codegen;
mod lit;
mod template;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

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
pub(crate) struct Error(InnerError);

impl Error {
    pub(crate) fn to_compiler_error(&self) -> TokenStream {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.0.message))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(self.0.span);
        group.set_span(self.0.span);

        TokenStream::from_iter([
            TokenTree::Ident(Ident::new("compile_error", self.0.span)),
            punc,
            group,
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ])
    }
    pub(crate) fn msg(message: &str) -> Error {
        Error(InnerError {
            span: Span::call_site(),
            message: message.to_string(),
        })
    }
    pub(crate) fn msg_ctx(message: &str, fmt: &dyn std::fmt::Display) -> Error {
        Error(InnerError {
            span: Span::call_site(),
            message: format!("{message}: {fmt}"),
        })
    }
    pub(crate) fn span_msg(message: &str, span: Span) -> Error {
        Error(InnerError {
            span,
            message: message.to_string(),
        })
    }
    pub(crate) fn span_msg_ctx(message: &str, fmt: &dyn std::fmt::Display, span: Span) -> Error {
        Error(InnerError {
            span,
            message: format!("{}: {}", fmt, message),
        })
    }
}

#[proc_macro]
pub fn array(input: TokenStream) -> TokenStream {
    template::array(input)
}

#[proc_macro]
pub fn object(input: TokenStream) -> TokenStream {
    template::object(input)
}

#[proc_macro_derive(Jsony, attributes(jsony))]
pub fn derive_jsony(input: TokenStream) -> TokenStream {
    codegen::derive(input)
}
