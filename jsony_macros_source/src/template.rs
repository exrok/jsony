use crate::writer::RustWriter;
use crate::{
    lit::{self, literal_inline},
    Flatten,
};
use proc_macro2::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};

fn parend(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Parenthesis, ts))
}
fn braced(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Brace, ts))
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
    __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];

#[allow(unused)]
fn tt_append_blit(output: &mut Vec<TokenTree>, chr: &str) {
    output.extend(chr.as_bytes().iter().map(|tok| match *tok {
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
    (~ $d:tt) => { $d.tt_punct_joint('-') };
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

fn is_char(tt: &TokenTree, ch: char) -> bool {
    if let TokenTree::Punct(p) = tt {
        if p.as_char() == ch {
            return true;
        }
    }
    false
}

use crate::Error;

struct Codegen {
    out: RustWriter,
    need_mut_builder: bool,
    builder: Ident,
    text: String,
    initial_capacity: usize,
    error: Option<crate::Error>,
    flatten: Flatten,
    writer: Option<Vec<TokenTree>>,
}

fn munch_expr(input: &mut IntoIter, output: &mut Vec<TokenTree>) {
    output.clear();
    //Todo this doesn't handle generics properly
    for tok in input.by_ref() {
        if is_char(&tok, ',') {
            break;
        }
        output.push(tok);
    }
}
fn is_flatten(toks: &[TokenTree]) -> bool {
    if let [ch1, ch2, ..] = toks {
        is_char(ch1, '.') && is_char(ch2, '.')
    } else {
        false
    }
}

fn str_lit(lit: &str) -> TokenTree {
    TokenTree::Literal(Literal::string(lit))
}

enum Attr {
    None,
    If {
        contents: Vec<TokenTree>,
        codegen_height: usize,
    },
}

pub(crate) fn raw_escape(raw: &str, output: &mut String) {
    let bytes = raw.as_bytes();
    let mut start = 0;
    unsafe {
        let buf = output.as_mut_vec();
        for (i, &byte) in bytes.iter().enumerate() {
            let escape = ESCAPE[byte as usize];
            if escape == 0 {
                continue;
            }

            if start < i {
                buf.extend_from_slice(&bytes[start..i]);
            }

            start = i + 1;

            let s = match escape {
                self::QU => b"\\\"",
                self::BS => b"\\\\",
                self::BB => b"\\b",
                self::FF => b"\\f",
                self::NN => b"\\n",
                self::RR => b"\\r",
                self::TT => b"\\t",
                self::UU => {
                    static HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";
                    let bytes = &[
                        b'\\',
                        b'u',
                        b'0',
                        b'0',
                        HEX_DIGITS[(byte >> 4) as usize],
                        HEX_DIGITS[(byte & 0xF) as usize],
                    ];
                    buf.extend_from_slice(bytes);
                    continue;
                }
                _ => {
                    //I would just use unreachable!() but can't due to current use of cargo expand limitations
                    std::hint::unreachable_unchecked()
                }
            };
            buf.extend_from_slice(s);
        }

        if start == bytes.len() {
            return;
        }

        buf.extend_from_slice(&bytes[start..]);
    }
}

impl Codegen {
    fn parse_object(&mut self, mut input: IntoIter, top_most: bool) {
        //todo opt
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        let mut outer_first_token = true;
        let mut attr = Attr::None;
        'outer: while let Some(mut t) = input.next() {
            macro_rules! bail {
                ($span: expr; $str: literal) => {{
                    self.set_err($span, $str);
                    return;
                }};
            }
            macro_rules! expect_next {
                () => {
                    if let Some(got) = input.next() {
                        got
                    } else {
                        self.eof(Span::call_site());
                        return;
                    }
                };
            }
            loop {
                let first_token = outer_first_token;
                outer_first_token = false;
                // types of supported keys (all one root TokenTree)
                // - String Literal: "key"
                // - Ident:           key
                // - Variable:       [key]

                match &t {
                    TokenTree::Punct(ch) if ch.as_char() == '@' => {
                        let TokenTree::Group(group) = expect_next!() else {
                            bail!(ch.span(); "Expected [] attr");
                        };
                        let contents: Vec<TokenTree> = group.stream().into_iter().collect();
                        if let Some(TokenTree::Ident(ident)) = contents.first() {
                            if ident.to_string() == "if" {
                                self.flush_text();
                                attr = Attr::If {
                                    contents,
                                    codegen_height: self.out.buf.len(),
                                };
                            }
                        }
                        continue 'outer;
                    }
                    TokenTree::Ident(value) => {
                        let contents = &value.to_string();
                        if contents == "in" {
                            if !first_token || !top_most {
                                bail!(value.span(); "in keyword only allowed at beginning of template macro")
                            }
                            let mut toks: Vec<TokenTree> = Vec::new();
                            loop {
                                let tok = expect_next!();
                                if is_char(&tok, ';') {
                                    break;
                                }
                                toks.push(tok);
                            }
                            outer_first_token = true;
                            self.in_writer(toks);
                            continue 'outer;
                        }
                    }
                    _ => {}
                }
                // There inner block here for parsing the speicifc case
                let col = 'value: {
                    if let Some(col) = input.next() {
                        // todo should be handling when there are many commands
                        if !is_char(&col, ',') {
                            break 'value col;
                        }
                    }
                    match t {
                        TokenTree::Ident(value) => {
                            let contents = &value.to_string();
                            self.pre_escaped_key(&contents);
                            self.value_from_expression(value.span(), TokenTree::Ident(value));
                            self.text.push(',');
                            self.entry_completed(&mut attr);
                            continue 'outer;
                        }
                        _ => {
                            if self.error.is_none() {
                                self.set_err(t.span(), "Expected colon");
                            }
                            return;
                        }
                    }
                };
                if !is_char(&col, ':') {
                    if is_char(&t, '.') && is_char(&col, '.') {
                        value_tokens.clear();
                        while let Some(tok) = input.next() {
                            if is_char(&tok, ',') {
                                break;
                            }
                            value_tokens.push(tok);
                        }
                        //todo handle empty here to
                        let prev = self.flatten;
                        self.flatten = Flatten::Object;
                        self.insert_value(col.span(), &mut value_tokens);
                        self.flatten = prev;
                        self.entry_completed(&mut attr);
                        continue 'outer;
                    }
                    if let TokenTree::Ident(ident) = &t {
                        #[allow(clippy::cmp_owned)]
                        if ident.to_string() != "for" {
                            bail!(col.span(); "Unexpected 1")
                        } else {
                            if !first_token {
                                bail!(col.span(); "For comprehensions must be in there own object")
                            }
                        }
                    } else {
                        bail!(col.span(); "Unexpected 2")
                    }
                    value_tokens.clear();
                    value_tokens.push(t);
                    value_tokens.push(col);
                    loop {
                        let tok = expect_next!();
                        if is_char(&tok, ';') {
                            break;
                        }
                        value_tokens.push(tok);
                    }
                    self.flush_text();
                    self.out.buf.extend(value_tokens.drain(..));
                    splat!((self.out); {
                        [
                            self.parse_object(input, false);
                            self.flush_text();
                        ]
                    });
                    return;

                    // continue 'outer;
                }
                // Accumulate Expression in
                value_tokens.clear();
                for tok in input.by_ref() {
                    if is_char(&tok, ',') {
                        break;
                    }
                    value_tokens.push(tok);
                }

                match &t {
                    TokenTree::Group(g) => {
                        if g.delimiter() != Delimiter::Bracket {
                            bail!(g.span(); "Expected [key], key or \"key\".")
                        }
                        self.dyn_key(g.span(), g.stream());
                    }
                    TokenTree::Ident(ident) => {
                        self.pre_escaped_key(&ident.to_string());
                    }
                    TokenTree::Punct(x) => bail!(x.span(); "Unexpected Punc"),
                    TokenTree::Literal(x) => match literal_inline(x.to_string()) {
                        lit::InlineKind::String(content) => {
                            self.text.push('"');
                            self.raw_escape_inline_value(&content);
                            self.text.push_str("\":");
                        }
                        lit::InlineKind::Raw(_) => {
                            bail!(x.span(); "Unexpected key")
                        }
                        lit::InlineKind::None => bail!(x.span(); "Unexpected key"),
                    },
                };

                // now we have: key and value
                self.insert_value(col.span(), &mut value_tokens);
                self.text.push(',');
                self.entry_completed(&mut attr);

                if let Some(tt) = input.next() {
                    t = tt;
                } else {
                    break;
                }
            }
        }
    }
    fn eof(&mut self, span: Span) {
        if let None = self.error {
            self.error = Some(Error::span_msg("Unexpected EOF", span));
        }
    }
    fn set_err(&mut self, span: Span, msg: &str) {
        if let None = self.error {
            self.error = Some(Error::span_msg(msg, span));
        }
    }
    fn entry_completed(&mut self, attr: &mut Attr) {
        if matches!(attr, Attr::None) {
            return;
        }
        let attr = std::mem::replace(attr, Attr::None);
        match attr {
            Attr::None => unsafe { std::hint::unreachable_unchecked() },
            Attr::If {
                contents,
                codegen_height,
            } => {
                self.flush_text();
                let body = TokenTree::Group(Group::new(
                    Delimiter::Brace,
                    self.out.buf.drain(codegen_height..).collect(),
                ));
                self.out.buf.extend(contents);
                self.out.buf.push(body);
            }
        }
    }
    fn in_writer(&mut self, writer: Vec<TokenTree>) {
        // remove extras
        self.text.clear();
        self.writer = Some(writer);
    }
    fn new(builder: Ident) -> Codegen {
        Codegen {
            out: RustWriter::new(),

            need_mut_builder: false,
            builder,
            initial_capacity: 0,
            text: String::with_capacity(64),
            error: None,
            writer: None,
            flatten: Flatten::None,
        }
    }
    fn flush_text(&mut self) {
        if self.text.is_empty() {
            return;
        }
        self.initial_capacity += self.text.len();
        if self.text.len() == 1 {
            let out = &mut self.out;
            let byte = self.text.as_bytes()[0];
            match byte {
                b':' => {
                    splat!(out; [#self.builder].push_colon(););
                }
                b',' => {
                    splat!(out; [#self.builder].push_comma(););
                }
                _ => {
                    splat!(out; unsafe {
                        [#self.builder].push_unchecked_ascii(
                            [@TokenTree::Literal(Literal::byte_character(
                                self.text.as_bytes()[0]
                            ))]
                        );
                    });
                }
            }
        } else {
            let out = &mut self.out;
            splat!(out; [#self.builder].push_str(
                [@str_lit(&self.text)]
            ););
        }
        self.text.clear();
    }
    fn expand_match(&mut self, values: &mut Vec<TokenTree>) {
        self.flush_text();
        let token = values.pop().unwrap();
        // check len make sure expression for match exists.
        let TokenTree::Group(group) = token else {
            self.set_err(token.span(), "Expected Blocked for Match Patterns");
            return;
        };
        if group.delimiter() != Delimiter::Brace {
            self.set_err(group.span(), "Expected Blocked for Match Patterns");
            return;
        };
        let start = self.out.buf.len();
        let mut input = group.stream().into_iter();
        let mut peq = false;
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        while let Some(tok) = input.next() {
            if !(peq && is_char(&tok, '>')) {
                peq = is_char(&tok, '=');
                self.out.buf.push(tok);
                continue;
            }
            let span = tok.span();
            self.out.buf.push(tok);
            munch_expr(&mut input, &mut value_tokens);
            splat!((self.out); {[
                self.insert_value(span, &mut value_tokens);
                self.flush_text();
            ]});
        }
        // Why this line?
        let mut matches = Group::new(Delimiter::Brace, self.out.buf.drain(start..).collect());
        matches.set_span(group.span());
        self.out.buf.extend(values.drain(..));
        splat!((self.out); [@TokenTree::Group(matches)];);
    }

    fn parse_inline_array(&mut self, span: Span, stream: TokenStream) -> bool {
        let mut token_values: Vec<TokenTree> = Vec::new();
        let mut input = stream.into_iter();

        // todo don't use this span, should use the previous comma or something.

        munch_expr(&mut input, &mut token_values);

        if let Some(split) = token_values.iter().position(|tok| is_char(tok, ';')) {
            if let Some(TokenTree::Ident(value)) = token_values.first() {
                #[allow(clippy::cmp_owned)]
                if value.to_string() == "for" {
                    let f = self.flatten;
                    self.flatten = Flatten::None;
                    if f == Flatten::None {
                        self.text.push('[');
                    }
                    self.flush_text();
                    self.out
                        .buf
                        .extend(token_values.drain(..split + 1).take(split));
                    splat!((self.out); {[
                        self.insert_value(span, &mut token_values);
                        self.text.push(',');
                        self.flush_text();
                    ]});
                    if f == Flatten::None {
                        self.flush_text();
                        splat!((self.out); [#self.builder].end_json_array(););
                    }
                    // todo ensure the was the end
                    return true;
                }
            }
            return false;
        }
        let f = self.flatten;
        self.flatten = Flatten::None;
        if f == Flatten::None {
            self.begin_inline_array();
        }
        if token_values.is_empty() {
            if f == Flatten::None {
                self.end_inline_array();
            }
            return true;
        }

        if is_flatten(&token_values) {
            let prev = self.flatten;
            self.flatten = Flatten::Array;
            token_values.drain(..2);
            self.insert_value(span, &mut token_values);
            self.flatten = prev;
        } else {
            self.insert_value(span, &mut token_values);
            self.text.push(',');
        }

        // todo don't rely on the internal details of size hint being accurated
        while input.size_hint().0 > 0 {
            munch_expr(&mut input, &mut token_values);
            if is_flatten(&token_values) {
                let prev = self.flatten;
                self.flatten = Flatten::Array;
                token_values.drain(..2);
                self.insert_value(span, &mut token_values);
                self.flatten = prev;
            } else {
                self.insert_value(span, &mut token_values);
                self.text.push(',');
            }
        }

        if f == Flatten::None {
            self.end_inline_array();
        }
        true
    }
    fn require_scalar_or_error(&mut self, span: Span) -> bool {
        match self.flatten {
            Flatten::None => false,
            Flatten::Object => {
                self.set_err(span, "Expected object to flatten");
                true
            }
            Flatten::Array => {
                self.set_err(span, "Expected array to flatten");
                true
            }
        }
    }

    fn insert_value(&mut self, span: Span, values: &mut Vec<TokenTree>) {
        let [first, rest @ ..] = &**values else {
            self.set_err(span, "Expected value after colon");
            return;
        };
        match first {
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {}
                Delimiter::Brace => {
                    match self.flatten {
                        Flatten::None => {
                            self.begin_inline_object();
                            self.parse_object(group.stream().into_iter(), false);
                            self.end_inline_object();
                        }
                        Flatten::Object => {
                            let prev = self.flatten;
                            self.flatten = Flatten::None;
                            self.parse_object(group.stream().into_iter(), false);
                            self.flatten = prev;
                        }
                        Flatten::Array => {
                            self.set_err(
                                group.span(),
                                "Expected array to flatten but found object",
                            );
                            return;
                        }
                    }
                    if let [first_after_brace, ..] = rest {
                        self.set_err(
                            first_after_brace.span(),
                            "Trailing tokens found after object value, expected a comma",
                        );
                    }
                    return;
                }
                Delimiter::Bracket => {
                    if rest.is_empty() {
                        if self.parse_inline_array(group.span(), group.stream()) {
                            return;
                        }
                    } else {
                        // give specific error for common case of forgetting a comma.
                        // in surface rust syntax the following is invalid.
                        if let [TokenTree::Ident(key), TokenTree::Punct(punct), ..] = rest {
                            if punct.as_char() == ':' {
                                self.set_err(
                                    key.span(),
                                    "Ident token found after array value, expected a comma",
                                );
                                return;
                            }
                        }
                    }
                }
                Delimiter::None => (),
            },
            TokenTree::Ident(ident) => {
                let name = ident.to_string();
                if values.len() == 1 {
                    match name.as_str() {
                        "None" => {
                            if self.require_scalar_or_error(ident.span()) {
                                return;
                            }
                            self.raw_inline_value("null");
                            return;
                        }
                        "true" => {
                            if self.require_scalar_or_error(ident.span()) {
                                return;
                            }
                            self.raw_inline_value("true");
                            return;
                        }
                        "false" => {
                            if self.require_scalar_or_error(ident.span()) {
                                return;
                            }
                            self.raw_inline_value("false");
                            return;
                        }
                        _ => (),
                    }
                }
                match name.as_str() {
                    "match" => {
                        return self.expand_match(values);
                    }
                    "todo" => {
                        if let Some(next) = values.get(1) {
                            if is_char(next, '!') {
                                if let Some(TokenTree::Group(_)) = values.get(2) {
                                    if values.len() == 3 {
                                        self.out.buf.extend(values.drain(..));
                                        splat!((self.out); ;);
                                        return;
                                    }
                                }
                            }
                        }
                    }
                    _ => (),
                }
            }
            TokenTree::Literal(lit) => {
                if values.len() == 1 {
                    match literal_inline(lit.to_string()) {
                        lit::InlineKind::String(value) => {
                            if self.require_scalar_or_error(lit.span()) {
                                return;
                            }
                            self.text.push('"');
                            self.raw_escape_inline_value(&value);
                            self.text.push('"');
                            return;
                        }
                        lit::InlineKind::Raw(raw) => {
                            if self.require_scalar_or_error(lit.span()) {
                                return;
                            }
                            self.raw_inline_value(&raw);
                            return;
                        }
                        lit::InlineKind::None => (),
                    }
                }
            }
            TokenTree::Punct(start) => {
                if start.as_char() == '|' {
                    if let [_, TokenTree::Ident(binding), TokenTree::Punct(end), ..] = &**values {
                        if end.as_char() == '|' {
                            self.flush_text();
                            let out = &mut self.out;
                            splat!(out; {
                                // TODO need to think about unifying the builder type when in vs not.
                                // Currently one is ref the other is direct.
                                let mut [#binding] = [match self.flatten {
                                    Flatten::None => splat!(out; ::jsony::json::ValueWriter::new([#self.builder])),
                                    Flatten::Object => {
                                        self.need_mut_builder = true;
                                        splat!(out; ::jsony::json::ObjectWriter::non_terminating(&mut [#self.builder]));
                                    },
                                    Flatten::Array => {
                                        self.need_mut_builder = true;
                                        splat!(out; ::jsony::json::ArrayWriter::non_terminating(&mut [#self.builder]));
                                    }
                                }];
                                [@TokenTree::Group(Group::new(Delimiter::Brace, values.drain(3..).collect()))];
                            });
                            return;
                        }
                    }
                }
            }
        }

        if values.len() == 1 {
            self.value_from_expression(first.span(), values.pop().unwrap())
        } else {
            self.value_from_expression(
                first.span(),
                TokenTree::Group(Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::from_iter(values.drain(..)),
                )),
            )
        }
    }
    fn begin_inline_array(&mut self) {
        self.text.push('[');
    }

    fn end_inline_array(&mut self) {
        unsafe {
            if let Some(ch) = self.text.as_mut_vec().last_mut() {
                if *ch == b',' {
                    *ch = b']';
                    return;
                }
                if *ch == b'[' {
                    self.text.push(']');
                    return;
                }
            }
        }
        self.flush_text();
        splat!((self.out); [#self.builder].end_json_array(););
    }
    fn begin_inline_object(&mut self) {
        self.text.push('{');
    }
    fn end_inline_object(&mut self) {
        unsafe {
            if let Some(ch) = self.text.as_mut_vec().last_mut() {
                if *ch == b',' {
                    *ch = b'}';
                    return;
                }
                if *ch == b'{' {
                    self.text.push('}');
                    return;
                }
            }
        }
        self.flush_text();
        splat!((self.out); [#self.builder].end_json_object(););
    }
    fn dyn_key(&mut self, _span: Span, expr: TokenStream) {
        self.flush_text();
        splat! {(self.out);
            let _: ::jsony::json::AlwaysString =
                [@parend(expr)].encode_json__jsony([#self.builder]);
        };
        self.text.push_str(":");
    }
    fn pre_escaped_key(&mut self, raw: &str) {
        self.text.push('"');
        self.text.push_str(raw);
        self.text.push_str("\":");
    }
    fn raw_inline_value(&mut self, raw: &str) {
        self.text.push_str(raw);
    }
    fn raw_escape_inline_value(&mut self, raw: &str) {
        raw_escape(raw, &mut self.text);
    }

    fn value_from_expression(&mut self, _span: Span, expr: TokenTree) {
        self.initial_capacity += 2;
        self.flush_text();
        match self.flatten {
            Flatten::None => {
                splat! {(self.out);
                    [@expr].encode_json__jsony([#self.builder]);
                }
            }
            Flatten::Object => {
                splat! {(self.out);
                    [#self.builder].join_parent_json_value_with_next();
                    let _: ::jsony::json::AlwaysObject = [@expr].encode_json__jsony(
                        [#self.builder]
                    );
                    [#self.builder].join_object_with_next_value();
                }
            }
            Flatten::Array => {
                splat! {(self.out);
                    [#self.builder].join_parent_json_value_with_next();
                    let _: ::jsony::json::AlwaysArray = [@expr].encode_json__jsony(
                        [#self.builder]
                    );
                    [#self.builder].join_array_with_next_value();
                }
            }
        }
    }
    fn finish_creating(mut self) -> TokenStream {
        self.flush_error();
        if let Some(writer) = self.writer.take() {
            // TODO handle self.out case
            self.flush_text();
            let braced = braced(self.out.buf.drain(..).collect());
            return token_stream! {
                (self.out);
                {
                    use ::jsony::ToJson;
                    let mut object_writer: &mut ::jsony::json::ObjectWriter = [~&writer];
                    // TODO, detect if mut is needed or not
                    let [?(self.need_mut_builder) mut] [#self.builder] = object_writer.inner_writer();
                    [#self.builder].smart_object_comma();
                    [@braced]
                }
            };
        }
        if self.out.buf.is_empty() {
            return token_stream!(
                (self.out);
                String::from([@str_lit(&self.text)])
            );
        }
        self.flush_text();
        let capacity = (self.initial_capacity + 16) & (!0b1111);
        let braced = braced(self.out.buf.drain(..).collect());
        let out = &mut self.out;
        token_stream! {
            out;
            {
                use ::jsony::ToJson;
                let mut _builder = jsony::TextWriter::with_capacity(
                    [#Literal::usize_unsuffixed(capacity)]
                );
                {
                    // TODO, detect if mut is needed or not
                    let [?(self.need_mut_builder) mut] [#self.builder] = &mut _builder;
                    [@braced]
                }
                _builder.into_string()
            }
        }
    }
    fn flush_error(&mut self) {
        if let Some(err) = self.error.take() {
            self.out.buf.extend(err.to_compiler_error(true));
        }
    }
}

pub fn array(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    codegen.parse_inline_array(Span::call_site(), input);

    codegen.finish_creating()
}

pub fn object(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    codegen.begin_inline_object();
    codegen.parse_object(input.into_iter(), true);
    if codegen.writer.is_none() {
        codegen.end_inline_object();
    }

    if let Some(error) = codegen.error {
        return error.to_compiler_error(true);
    }

    codegen.finish_creating()
}
