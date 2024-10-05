use crate::{
    lit::{self, literal_inline},
    Flatten,
};
use proc_macro::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
fn chr(ch: char) -> TokenTree {
    TokenTree::Punct(Punct::new(ch, Spacing::Alone))
}
fn j(ch: char) -> TokenTree {
    TokenTree::Punct(Punct::new(ch, Spacing::Joint))
}
fn parend(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Parenthesis, ts))
}
fn braced(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Brace, ts))
}
fn with_span(mut tt: TokenTree, span: Span) -> TokenTree {
    tt.set_span(span);
    tt
}
const BB: u8 = b'b';
const TT: u8 = b't';
const NN: u8 = b'n';
const FF: u8 = b'f';
const RR: u8 = b'r';
const QU: u8 = b'"';
const BS: u8 = b'\\';
const UU: u8 = b'u';
const __: u8 = 0;
static ESCAPE: [u8; 256] = [
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU,
    UU, UU, UU, UU, UU, UU, UU, UU, __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
];
struct ObjectParser<'a> {
    codegen: &'a mut Codegen,
    error: Option<(Span, Box<str>)>,
}
fn is_char(tt: &TokenTree, ch: char) -> bool {
    if let TokenTree::Punct(p) = tt {
        if p.as_char() == ch {
            return true;
        }
    }
    false
}
impl<'a> ObjectParser<'a> {
    fn eof(&mut self, span: Span) {
        if self.error.is_none() {
            self.error = Some((span, "Unexpected EOF".into()))
        }
    }
    fn parse(&mut self, mut input: IntoIter) {
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        'outer: while let Some(mut t) = input.next() {
            loop {
                let col = 'value: {
                    if let Some(col) = input.next() {
                        if !is_char(&col, ',') {
                            break 'value col;
                        }
                    }
                    match t {
                        TokenTree::Ident(value) => {
                            self.codegen.pre_escaped_key(&value.to_string());
                            self.codegen.value_from_expression(
                                value.span(),
                                TokenStream::from_iter([TokenTree::Ident(value)]),
                            );
                            self.codegen.text.push(',');
                            continue 'outer;
                        }
                        _ => {
                            if self.error.is_none() {
                                self.error = Some((t.span(), "Expected colon".into()))
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
                        let prev = self.codegen.flatten;
                        self.codegen.flatten = Flatten::Object;
                        self.codegen.insert_value(col.span(), &mut value_tokens);
                        self.codegen.flatten = prev;
                        continue 'outer;
                    }
                    if let TokenTree::Ident(ident) = &t {
                        #[allow(clippy::cmp_owned)]
                        if ident.to_string() != "for" {
                            {
                                if self.error.is_none() {
                                    self.error = Some((col.span(), "Unexpected".to_string().into()))
                                }
                                return;
                            }
                        }
                    } else {
                        {
                            if self.error.is_none() {
                                self.error = Some((col.span(), "Unexpected".to_string().into()))
                            }
                            return;
                        }
                    }
                    value_tokens.clear();
                    value_tokens.push(t);
                    value_tokens.push(col);
                    let span;
                    loop {
                        let tok = if let Some(got) = input.next() {
                            got
                        } else {
                            self.eof(Span::call_site());
                            return;
                        };
                        if is_char(&tok, ';') {
                            span = tok.span();
                            break;
                        }
                        value_tokens.push(tok);
                    }
                    self.codegen.flush_text();
                    self.codegen.out.extend(value_tokens.drain(..));
                    let stream = self.codegen.wrapped(|gen| {
                        gen.parser_key_value(span, &mut input);
                    });
                    self.codegen.out.extend([braced(stream)]);
                    continue 'outer;
                }
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
                            {
                                if self.error.is_none() {
                                    self.error = Some((
                                        g.span(),
                                        "Expected [key], key or \"key\".".to_string().into(),
                                    ))
                                }
                                return;
                            }
                        }
                        self.codegen.dyn_key(g.span(), g.stream());
                    }
                    TokenTree::Ident(ident) => {
                        self.codegen.pre_escaped_key(&ident.to_string());
                    }
                    TokenTree::Punct(x) => {
                        if self.error.is_none() {
                            self.error = Some((x.span(), "Unexpected Punc".to_string().into()))
                        }
                        return;
                    }
                    TokenTree::Literal(x) => match literal_inline(x.to_string()) {
                        lit::InlineKind::String(content) => {
                            self.codegen.text.push('"');
                            self.codegen.raw_escape_inline_value(&content);
                            self.codegen.text.push_str("\":");
                        }
                        lit::InlineKind::Raw(_) => {
                            if self.error.is_none() {
                                self.error = Some((x.span(), "Unexpected key".to_string().into()))
                            }
                            return;
                        }
                        lit::InlineKind::None => {
                            if self.error.is_none() {
                                self.error = Some((x.span(), "Unexpected key".to_string().into()))
                            }
                            return;
                        }
                    },
                };
                self.codegen.insert_value(col.span(), &mut value_tokens);
                self.codegen.text.push(',');
                if let Some(tt) = input.next() {
                    t = tt;
                } else {
                    break;
                }
            }
        }
    }
}
struct Codegen {
    out: TokenStream,
    builder: Ident,
    text: String,
    initial_capacity: usize,
    error: Option<(Span, Box<str>)>,
    flatten: Flatten,
}
fn munch_expr(input: &mut IntoIter, output: &mut Vec<TokenTree>) {
    output.clear();
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
impl Codegen {
    fn new(builder: Ident) -> Codegen {
        Codegen {
            out: TokenStream::new(),
            builder,
            initial_capacity: 0,
            text: String::with_capacity(64),
            error: None,
            flatten: Flatten::None,
        }
    }
    fn builder(&self) -> TokenTree {
        TokenTree::Ident(self.builder.clone())
    }
    fn flush_text(&mut self) {
        if self.text.is_empty() {
            return;
        }
        self.initial_capacity += self.text.len();
        self.out.extend([
            self.builder(),
            chr('.'),
            TokenTree::Ident(Ident::new("string", Span::call_site())),
            chr('.'),
            TokenTree::Ident(Ident::new("push_str", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.text))]),
            )),
            chr(';'),
        ]);
        self.text.clear();
    }
    fn expand_match(&mut self, values: &mut Vec<TokenTree>) {
        self.flush_text();
        let token = values.pop().unwrap();
        let TokenTree::Group(group) = token else {
            self.error = Some((token.span(), "Expected Blocked for Match Patterns".into()));
            return;
        };
        if group.delimiter() != Delimiter::Brace {
            self.error = Some((group.span(), "Expected Blocked for Match Patterns".into()));
            return;
        };
        let mut patterns = TokenStream::default();
        let mut input = group.stream().into_iter();
        let mut peq = false;
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        while let Some(tok) = input.next() {
            if !(peq && is_char(&tok, '>')) {
                peq = is_char(&tok, '=');
                patterns.extend([tok]);
                continue;
            }
            let span = tok.span();
            patterns.extend([tok]);
            munch_expr(&mut input, &mut value_tokens);
            let stream = self.wrapped(|gen| gen.insert_value(span, &mut value_tokens));
            patterns.extend([braced(stream), chr(',')]);
        }
        self.out.extend(values.drain(..));
        self.out
            .extend([with_span(braced(patterns), group.span()), chr(';')]);
    }
    fn wrapped(&mut self, mut func: impl FnMut(&mut Self)) -> TokenStream {
        self.flush_text();
        let old = std::mem::take(&mut self.out);
        func(self);
        self.flush_text();
        std::mem::replace(&mut self.out, old)
    }
    fn parse_inline_array(&mut self, span: Span, stream: TokenStream) -> bool {
        let mut token_values: Vec<TokenTree> = Vec::new();
        let mut input = stream.into_iter();
        munch_expr(&mut input, &mut token_values);
        if let Some(split) = token_values.iter().position(|tok| is_char(tok, ';')) {
            if let Some(TokenTree::Ident(value)) = token_values.first() {
                #[allow(clippy::cmp_owned)]
                if value.to_string() == "for" {
                    let f = self.flatten;
                    self.flatten = Flatten::None;
                    if f == Flatten::None {
                        self.begin_inline_array();
                    }
                    self.flush_text();
                    self.out.extend(token_values.drain(..split + 1).take(split));
                    let stream = self.wrapped(|gen| {
                        gen.insert_value(span, &mut token_values);
                        gen.text.push(',');
                    });
                    self.out.extend([braced(stream)]);
                    if f == Flatten::None {
                        self.out.extend([
                            self.builder(),
                            chr('.'),
                            TokenTree::Ident(Ident::new("smart_close_array", Span::call_site())),
                            TokenTree::Group(Group::new(
                                Delimiter::Parenthesis,
                                TokenStream::default(),
                            )),
                            chr(';'),
                        ]);
                    }
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
                self.error = Some((span, "Expected object to flatten".into()));
                true
            }
            Flatten::Array => {
                self.error = Some((span, "Expected array to flatten".into()));
                true
            }
        }
    }
    fn inline_into_json(&mut self, span: Span, args: TokenStream, body: TokenStream) {
        self.flush_text();
        let mut args = args.into_iter();
        let Some(ident) = args.next() else {
            self.error(span, "Expected binding in inline");
            return;
        };
        let mut x = TokenStream::from_iter([
            TokenTree::Ident(Ident::new("let", Span::call_site())),
            ident.clone(),
        ]);
        x.extend(args);
        match self.flatten {
            Flatten::None => {
                x.extend([
                    chr('='),
                    chr('<'),
                    TokenTree::Ident(Ident::new("_", Span::call_site())),
                    TokenTree::Ident(Ident::new("as", Span::call_site())),
                    TokenTree::Ident(Ident::new("jsony", Span::call_site())),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("OutputBuffer", Span::call_site())),
                    chr('>'),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("from_builder", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([
                            chr('&'),
                            TokenTree::Ident(Ident::new("mut", Span::call_site())),
                            self.builder(),
                        ]),
                    )),
                    chr(';'),
                    braced(body),
                    chr('<'),
                    TokenTree::Ident(Ident::new("_", Span::call_site())),
                    TokenTree::Ident(Ident::new("as", Span::call_site())),
                    TokenTree::Ident(Ident::new("OutputBuffer", Span::call_site())),
                    chr('>'),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("terminate", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([ident]),
                    )),
                ]);
            }
            Flatten::Object => {
                x.extend([
                    chr('='),
                    TokenTree::Ident(Ident::new("jsony", Span::call_site())),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("ObjectBuf", Span::call_site())),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("from_builder_raw", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([
                            chr('&'),
                            TokenTree::Ident(Ident::new("mut", Span::call_site())),
                            self.builder(),
                        ]),
                    )),
                    chr(';'),
                    braced(body),
                ]);
            }
            Flatten::Array => {
                x.extend([
                    chr('='),
                    TokenTree::Ident(Ident::new("jsony", Span::call_site())),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("ArrayBuf", Span::call_site())),
                    j(':'),
                    chr(':'),
                    TokenTree::Ident(Ident::new("from_builder_raw", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([
                            chr('&'),
                            TokenTree::Ident(Ident::new("mut", Span::call_site())),
                            self.builder(),
                        ]),
                    )),
                    chr(';'),
                    braced(body),
                ]);
            }
        }
        self.out.extend([braced(x)]);
    }
    fn insert_value(&mut self, span: Span, values: &mut Vec<TokenTree>) {
        if values.is_empty() {
            self.error = Some((span, "Expected value after colon".into()));
            return;
        }
        match &values[0] {
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    if let [_, eq, ge, ..] = &values[..] {
                        if is_char(eq, '=') && is_char(ge, '>') {
                            self.inline_into_json(
                                group.span(),
                                group.stream(),
                                TokenStream::from_iter(values.drain(3..)),
                            );
                            return;
                        }
                    }
                }
                Delimiter::Brace => {
                    let mut parser = ObjectParser {
                        codegen: self,
                        error: None,
                    };
                    match parser.codegen.flatten {
                        Flatten::None => {
                            parser.codegen.begin_inline_object();
                            parser.parse(group.stream().into_iter());
                            parser.codegen.end_inline_object();
                        }
                        Flatten::Object => {
                            let prev = parser.codegen.flatten;
                            parser.codegen.flatten = Flatten::None;
                            parser.parse(group.stream().into_iter());
                            parser.codegen.flatten = prev;
                        }
                        Flatten::Array => {
                            self.error = Some((
                                group.span(),
                                "Expected array to flatten but found object".into(),
                            ));
                            return;
                        }
                    }
                    if parser.error.is_some() {
                        self.error = parser.error;
                    }
                    return;
                }
                Delimiter::Bracket => {
                    if self.parse_inline_array(group.span(), group.stream()) {
                        return;
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
                                        self.out.extend(values.drain(..));
                                        self.out.extend([chr(';')]);
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
            TokenTree::Punct(_) => (),
        }
        self.value_from_expression(values[0].span(), TokenStream::from_iter(values.drain(..)))
    }
    fn begin_inline_array(&mut self) {
        self.text.push('[');
    }
    fn error(&mut self, span: Span, msg: impl Into<String>) {
        if self.error.is_none() {
            self.error = Some((span, msg.into().into()));
        }
    }
    fn parser_key_value(&mut self, span: Span, input: &mut IntoIter) {
        let Some(key) = input.next() else {
            self.error(span, "Expected key, value pair after for in object");
            return;
        };
        let Some(colon) = input.next() else {
            self.error(key.span(), "Expected colon after key");
            return;
        };
        if !is_char(&colon, ':') {
            self.error(colon.span(), "Expected colon after key");
            return;
        }
        match &key {
            TokenTree::Group(g) => {
                if g.delimiter() != Delimiter::Bracket {
                    self.error(colon.span(), "Expected [key], key or \"key\".");
                    return;
                }
                self.dyn_key(g.span(), g.stream());
            }
            TokenTree::Ident(ident) => {
                self.pre_escaped_key(&ident.to_string());
            }
            TokenTree::Punct(_x) => {
                self.error(colon.span(), "Expected [key], key or \"key\".");
                return;
            }
            TokenTree::Literal(x) => match literal_inline(x.to_string()) {
                lit::InlineKind::String(content) => {
                    self.text.push('"');
                    self.raw_escape_inline_value(&content);
                    self.text.push_str("\":");
                }
                lit::InlineKind::Raw(_) => {
                    self.error(colon.span(), "Expected [key], key or \"key\".");
                    return;
                }
                lit::InlineKind::None => {
                    self.error(colon.span(), "Expected [key], key or \"key\".");
                    return;
                }
            },
        };
        let mut value_tokens = Vec::new();
        for tok in input.by_ref() {
            if is_char(&tok, ',') {
                break;
            }
            value_tokens.push(tok);
        }
        self.insert_value(colon.span(), &mut value_tokens);
        self.text.push(',');
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
        self.out.extend([
            self.builder(),
            chr('.'),
            TokenTree::Ident(Ident::new("smart_close_array", Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::default())),
            chr(';'),
        ]);
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
        self.out.extend([
            self.builder(),
            chr('.'),
            TokenTree::Ident(Ident::new("smart_close_object", Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::default())),
            chr(';'),
        ]);
    }
    fn dyn_key(&mut self, span: Span, expr: TokenStream) {
        self.text.push('"');
        self.flush_text();
        self.out.extend([
            self.builder(),
            chr('.'),
            TokenTree::Ident(Ident::new("raw_key", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([with_span(chr('&'), span), with_span(parend(expr), span)]),
            )),
            chr(';'),
        ]);
        self.text.push_str("\":");
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
        let bytes = raw.as_bytes();
        let mut start = 0;
        unsafe {
            let buf = self.text.as_mut_vec();
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
                    _ => std::hint::unreachable_unchecked(),
                };
                buf.extend_from_slice(s);
            }
            if start == bytes.len() {
                return;
            }
            buf.extend_from_slice(&bytes[start..]);
        }
    }
    fn value_from_expression(&mut self, span: Span, expr: TokenStream) {
        self.initial_capacity += 2;
        self.flush_text();
        let expr = with_span(
            TokenTree::Group(Group::new(Delimiter::Parenthesis, expr)),
            span,
        );
        match self.flatten {
            Flatten::None => {
                self.out.extend([
                    self.builder(),
                    chr('.'),
                    TokenTree::Ident(Ident::new("value", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([with_span(chr('&'), span), expr]),
                    )),
                    chr(';'),
                ]);
            }
            Flatten::Object => {
                self.out.extend([
                    self.builder(),
                    chr('.'),
                    TokenTree::Ident(Ident::new("raw_object", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([with_span(chr('&'), span), expr]),
                    )),
                    chr(';'),
                ]);
            }
            Flatten::Array => {
                self.out.extend([
                    self.builder(),
                    chr('.'),
                    TokenTree::Ident(Ident::new("raw_array", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([with_span(chr('&'), span), expr]),
                    )),
                    chr(';'),
                ]);
            }
        }
    }
    fn finish_creating(mut self) -> TokenStream {
        self.flush_error();
        if self.out.is_empty() {
            return TokenStream::from_iter([
                TokenTree::Ident(Ident::new("String", Span::call_site())),
                j(':'),
                chr(':'),
                TokenTree::Ident(Ident::new("from", Span::call_site())),
                TokenTree::Group(Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.text))]),
                )),
            ]);
        }
        self.flush_text();
        let builder = self.builder();
        let capacity = (self.initial_capacity + 16) & (!0b1111);
        let res = TokenStream::from_iter([
            TokenTree::Ident(Ident::new("use", Span::call_site())),
            TokenTree::Ident(Ident::new("jsony", Span::call_site())),
            j(':'),
            chr(':'),
            TokenTree::Group(Group::new(
                Delimiter::Brace,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("OutputBuffer", Span::call_site())),
                    chr(','),
                    TokenTree::Ident(Ident::new("ToJson", Span::call_site())),
                ]),
            )),
            chr(';'),
            TokenTree::Ident(Ident::new("let", Span::call_site())),
            TokenTree::Ident(Ident::new("mut", Span::call_site())),
            self.builder(),
            chr('='),
            TokenTree::Ident(Ident::new("jsony", Span::call_site())),
            j(':'),
            chr(':'),
            TokenTree::Ident(Ident::new("json", Span::call_site())),
            j(':'),
            chr(':'),
            TokenTree::Ident(Ident::new("RawBuf", Span::call_site())),
            j(':'),
            chr(':'),
            TokenTree::Ident(Ident::new("with_capacity", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([TokenTree::Literal(Literal::usize_unsuffixed(capacity))]),
            )),
            chr(';'),
            { braced(self.out) },
            builder,
            chr('.'),
            TokenTree::Ident(Ident::new("string", Span::call_site())),
        ]);
        TokenStream::from_iter([TokenTree::Group(Group::new(Delimiter::Brace, res))])
    }
    fn flush_error(&mut self) {
        if let Some((span, error)) = self.error.take() {
            let mut group = TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([TokenTree::Literal(Literal::string(&error))]),
            ));
            let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
            punc.set_span(span);
            group.set_span(span);
            self.out.extend([
                TokenTree::Ident(Ident::new("compile_error", span)),
                punc,
                group,
                TokenTree::Punct(Punct::new(';', Spacing::Alone)),
            ]);
        }
    }
    fn finish_append_object(mut self, name: Ident) -> TokenStream {
        self.flush_error();
        self.flush_text();
        let res = TokenStream::from_iter([
            TokenTree::Ident(Ident::new("use", Span::call_site())),
            TokenTree::Ident(Ident::new("jsony", Span::call_site())),
            j(':'),
            chr(':'),
            TokenTree::Group(Group::new(
                Delimiter::Brace,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("OutputBuffer", Span::call_site())),
                    chr(','),
                    TokenTree::Ident(Ident::new("ToJson", Span::call_site())),
                ]),
            )),
            chr(';'),
            TokenTree::Ident(Ident::new("let", Span::call_site())),
            self.builder(),
            chr('='),
            chr('&'),
            TokenTree::Ident(Ident::new("mut", Span::call_site())),
            TokenTree::Ident(name.clone()),
            chr('.'),
            TokenTree::Ident(Ident::new("buffer", Span::call_site())),
            chr(';'),
            { braced(self.out) },
        ]);
        TokenStream::from_iter([TokenTree::Group(Group::new(Delimiter::Brace, res))])
    }
}
pub fn array(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    codegen.parse_inline_array(Span::call_site(), input);
    codegen.finish_creating()
}
pub fn object(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    let mut parser = ObjectParser {
        codegen: &mut codegen,
        error: None,
    };
    parser.codegen.begin_inline_object();
    parser.parse(input.into_iter());
    parser.codegen.end_inline_object();
    if let Some((span, error)) = parser.error {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&error))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(span);
        group.set_span(span);
        return TokenStream::from_iter([
            TokenTree::Ident(Ident::new("compile_error", span)),
            punc,
            group,
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ]);
    }
    codegen.finish_creating()
}
pub fn append_object(input: TokenStream) -> TokenStream {
    let mut input = input.into_iter();
    let Some(TokenTree::Ident(ident)) = input.next() else {
        return TokenStream::new();
    };
    let _ = input.next();
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    let mut parser = ObjectParser {
        codegen: &mut codegen,
        error: None,
    };
    parser.parse(input);
    if let Some((span, error)) = parser.error {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&error))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(span);
        group.set_span(span);
        return TokenStream::from_iter([
            TokenTree::Ident(Ident::new("compile_error", span)),
            punc,
            group,
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ]);
    }
    codegen.finish_append_object(ident)
}
