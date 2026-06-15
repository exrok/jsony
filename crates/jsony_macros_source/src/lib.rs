//! Library face of the derive macro source.
//!
//! The crate is primarily the proc-macro *source* transformed into
//! `jsony_macros` by the preprocessor (`cargo expand --bin`). This `lib` target
//! exposes the same code paths as a normal Rust library so a harness can drive
//! the derive in-process (no compiler subprocess) and a coverage tool can
//! instrument it. See `crates/derive_tester` (the `expand` mode).
//!
//! The boundary is deliberately `&str -> ExpandStats`: token types never cross
//! the crate boundary, so a consumer is free to use a different `proc-macro2`.
//!
//! Modules that only serve the `object!`/`array!` template macros (`template`,
//! parts of `writer`) are unused by the derive-only `expand_str`, hence the
//! crate-wide `dead_code` allow. Coverage of those paths is honestly reported as
//! low when only derives are driven.
#![allow(clippy::question_mark, dead_code)]

mod ast;
mod case;
mod context;
#[macro_use]
mod codegen;
mod lit;
mod template;
#[allow(unused)]
mod util;
mod writer;

use std::str::FromStr;

use proc_macro2::{Delimiter, TokenStream, TokenTree};

pub(crate) use context::{Error, Flatten};

/// Result of [`expand_str`]: how many items were expanded, how many of those
/// yielded a `compile_error!` (the derive rejected the input — for a well-formed
/// generated type this must stay zero, so a nonzero count flags a harness or
/// codegen bug), and the total byte length of the generated output. The byte
/// count exists so a caller can fold it into a running total and keep the
/// optimizer from eliding the expansion.
#[derive(Default, Clone, Copy)]
pub struct ExpandStats {
    pub items: u64,
    pub errors: u64,
    pub output_bytes: u64,
}

impl ExpandStats {
    fn add(&mut self, other: ExpandStats) {
        self.items += other.items;
        self.errors += other.errors;
        self.output_bytes += other.output_bytes;
    }
}

/// Drive `#[derive(Jsony)]` over every item in `src`, in-process, the way the
/// proc-macro entry point does.
///
/// `src` is tokenized, split into top-level items, and each item is handed to
/// [`codegen::derive`] (the same panic-catching entry the compiler invokes, so a
/// malformed item yields a `compile_error!` token stream instead of aborting).
/// An item is any run of attributes followed by a `struct`/`enum` whose body ends
/// at the first brace group or trailing `;`, which covers every shape the
/// `derive_tester` data generator emits (named, tuple, unit, and enum, with or
/// without generics).
pub fn expand_str(src: &str) -> ExpandStats {
    let stream = TokenStream::from_str(src).expect("expand_str: source failed to tokenize as Rust");
    let mut stats = ExpandStats::default();
    let mut item: Vec<TokenTree> = Vec::new();
    let mut seen_kw = false;
    for tt in stream {
        let boundary = match &tt {
            TokenTree::Ident(id) => {
                let kw = id.to_string();
                if kw == "struct" || kw == "enum" {
                    seen_kw = true;
                }
                false
            }
            TokenTree::Group(g) => seen_kw && g.delimiter() == Delimiter::Brace,
            TokenTree::Punct(p) => seen_kw && p.as_char() == ';',
            TokenTree::Literal(_) => false,
        };
        item.push(tt);
        if boundary {
            stats.add(expand_item(&mut item));
            seen_kw = false;
        }
    }
    // A trailing run with no terminator (should not occur for well-formed input)
    // is still expanded so nothing is silently dropped.
    stats.add(expand_item(&mut item));
    stats
}

/// Expand one collected item, draining `item`. Empty drains (e.g. trailing
/// whitespace after the last item) contribute nothing.
fn expand_item(item: &mut Vec<TokenTree>) -> ExpandStats {
    if item.is_empty() {
        return ExpandStats::default();
    }
    let ts = TokenStream::from_iter(item.drain(..));
    let out = codegen::derive(ts).to_string();
    ExpandStats {
        items: 1,
        // `derive` catches panics and returns a `compile_error!` token stream.
        // jsony's normal output never contains that identifier, so its presence
        // means the item was rejected.
        errors: out.contains("compile_error").into(),
        output_bytes: out.len() as u64,
    }
}
