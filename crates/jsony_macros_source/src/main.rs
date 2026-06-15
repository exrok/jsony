#![allow(clippy::question_mark)]
mod ast;
mod case;
mod context;
#[macro_use]
mod codegen;
#[cfg(test)]
mod derive_tests;
mod lit;
mod template;
mod writer;
// mod template2;
#[allow(unused)]
mod util;

use std::str::FromStr;

use proc_macro2::TokenStream;

pub(crate) use context::{Error, Flatten};

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

    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(ToJson, tag= "type")]
    enum Foo {
        Foo{
            #[jsony(skip_if = |_, writer| x > 10)]
            billy: u32
        }
    }

    }));
}
