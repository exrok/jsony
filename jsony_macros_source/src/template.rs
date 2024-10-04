use crate::{
    lit::{self, literal_inline},
    Flatten,
};
use proc_macro2::{
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

macro_rules! tok {
    ($ident:ident) => {
        TokenTree::Ident(Ident::new(stringify!($ident), Span::call_site()))
    };
    (_) => {
        TokenTree::Ident(Ident::new("_", Span::call_site()))
    };
    (()) => {
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::default()))
    };
    (( $($tt:tt)*) ) => {
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::from_iter(toks!($($tt)*))))
    };
    ([{ $($tt:tt)* }] ) => {
        TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::from_iter(toks!($($tt)*))))
    };
    ([[ $($tt:tt)* ]] ) => {
        TokenTree::Group(Group::new(Delimiter::Bracket, TokenStream::from_iter(toks!($($tt)*))))
    };
    ({$($tt:tt)*}) => {
        $($tt)*
    };
    ([$com:literal]) => {
        TokenTree::Literal(Literal::string($com))
    };
    ([$com:literal; $ident:ident]) => {
        TokenTree::Literal(Literal::$ident($com))
    };
    ([_, $span: expr]) => {
        TokenTree::Ident(Ident::new("_", $span))
    };
    ([$ident:ident, $span: expr]) => {
        TokenTree::Ident(Ident::new(stringify!($ident), $span))
    };
    (<) => {
        chr('<')
    };
    (%) => {
        j(':')
    };
    (:) => {
        chr(':')
    };
    (>) => {
        chr('>')
    };
    (!) => {
        chr('!')
    };
    (.) => {
        chr('.')
    };
    (~) => {
        j('-')
    };
    (;) => {
       chr(';')
    };
    (&) => {
        chr('&')
    };
    (=) => {
        chr('=')
    };
    (,) => {
        chr(',')
    };
    (*) => {
        chr('*')
    };
    (#) => {
        j('\'')
    };
    ($com:ident; $tt:tt) => {
        TokenTree::from($com.$tt.clone())
    };
}

macro_rules! toks {
    ($($tt:tt)*) => {
        [$(tok!($tt)),*]
    }
}

macro_rules! braces {
    ( ) => {
        TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::default()))
    };
    ( $($tt:tt)* ) => {
        TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::from_iter(toks!($($tt)*))))
    }
}

struct ObjectParser<'a> {
    codegen: &'a mut Codegen,
    // pairs: Vec<Pair>,
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
        //todo opt
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        'outer: while let Some(mut t) = input.next() {
            macro_rules! bail {
                ($span: expr; $str: literal) => {{
                    if self.error.is_none() {
                        self.error = Some(($span, $str.to_string().into()))
                    }
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
                ($token_kind: ident) => {
                    if let Some(got) = input.next() {
                        if let TokenTree::$token_kind(value) = got {
                            value
                        } else {
                            self.err(
                                got.span(),
                                concat!("Expected ", stringify!($token_kind), " ", line!())
                                    .to_string(),
                            );
                            continue 'outer;
                        }
                    } else {
                        self.eof(Span::call_site());
                        return;
                    }
                };
                ($ch: literal) => {
                    if let Some(got) = input.next() {
                        if let TokenTree::Punct(value) = got {
                            if value.as_char() == $ch {
                                value
                            } else {
                                self.err(
                                    value.span(),
                                    concat!("Expected ", stringify!($ch)).to_string(),
                                );
                                continue 'outer;
                            }
                        } else {
                            self.err(
                                got.span(),
                                concat!("Expected ", stringify!($ch)).to_string(),
                            );
                            continue 'outer;
                        }
                    } else {
                        self.eof(Span::call_site());
                        return;
                    }
                };
            }
            loop {
                // types of supported keys (all one root TokenTree)
                // - String Literal: "key"
                // - Ident:           key
                // - Variable:       [key]

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
                                self.error = Some(
                                    (t.span(), "Expected colon".into()), //todo explain
                                )
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
                        let prev = self.codegen.flatten;
                        self.codegen.flatten = Flatten::Object;
                        self.codegen.insert_value(col.span(), &mut value_tokens);
                        self.codegen.flatten = prev;
                        continue 'outer;
                    }
                    if let TokenTree::Ident(ident) = &t {
                        #[allow(clippy::cmp_owned)]
                        if ident.to_string() != "for" {
                            bail!(col.span(); "Unexpected")
                        }
                    } else {
                        bail!(col.span(); "Unexpected")
                    }
                    value_tokens.clear();
                    value_tokens.push(t);
                    value_tokens.push(col);
                    let span;
                    loop {
                        let tok = expect_next!();
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
                        self.codegen.dyn_key(g.span(), g.stream());
                    }
                    TokenTree::Ident(ident) => {
                        self.codegen.pre_escaped_key(&ident.to_string());
                    }
                    TokenTree::Punct(x) => bail!(x.span(); "Unexpected Punc"),
                    TokenTree::Literal(x) => match literal_inline(x.to_string()) {
                        lit::InlineKind::String(content) => {
                            self.codegen.text.push('"');
                            self.codegen.raw_escape_inline_value(&content);
                            self.codegen.text.push_str("\":");
                        }
                        lit::InlineKind::Raw(_) => {
                            bail!(x.span(); "Unexpected key")
                        }
                        lit::InlineKind::None => bail!(x.span(); "Unexpected key"),
                    },
                };

                // now we have: key and value
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
        self.out.extend(toks![
            {self.builder()}.string.push_str({TokenTree::Literal(Literal::string(&self.text))});
        ]);
        self.text.clear();
    }
    fn expand_match(&mut self, values: &mut Vec<TokenTree>) {
        self.flush_text();
        let token = values.pop().unwrap();
        // check len make sure expression for match exists.
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
            patterns.extend(toks![{ braced(stream) },]);
        }
        self.out.extend(values.drain(..));
        self.out
            .extend([with_span(braced(patterns), group.span()), tok!(;)]);
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

        // todo don't use this span, should use the previous comma or something.

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
                        self.out
                            .extend(toks![{self.builder()}.smart_close_array();]);
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
        let mut x = TokenStream::from_iter(toks![let {ident.clone()}]);
        x.extend(args);
        match self.flatten {
            Flatten::None => {
                x.extend(toks![
                    = <_ as jsony%:OutputBuffer>%:from_builder(&mut {self.builder()});
                    {braced(body)}
                    <_ as OutputBuffer>%:terminate({ident})
                ]);
            }
            Flatten::Object => {
                x.extend(toks![
                    = jsony%:ObjectBuf%:from_builder_raw(&mut {self.builder()});
                    {braced(body)}
                ]);
            }
            Flatten::Array => {
                x.extend(toks![
                    = jsony%:ArrayBuf%:from_builder_raw(&mut {self.builder()});
                    {braced(body)}
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
                                        self.out.extend(toks!(;));
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
        //todo handle null error;
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
            //todo
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
        self.out.extend(toks![
            {self.builder()}.smart_close_array();
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
        self.out.extend(toks![
            {self.builder()}.smart_close_object();
        ]);
    }
    fn dyn_key(&mut self, span: Span, expr: TokenStream) {
        self.text.push('"');
        self.flush_text();
        self.out.extend(toks![{self.builder()}.raw_key(
            {with_span(tok!(&), span)}
            {with_span(parend(expr), span) }
        );]);
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

    fn value_from_expression(&mut self, span: Span, expr: TokenStream) {
        self.initial_capacity += 2;
        self.flush_text();
        let expr = with_span(
            TokenTree::Group(Group::new(Delimiter::Parenthesis, expr)),
            span,
        );
        match self.flatten {
            Flatten::None => {
                self.out.extend(toks![{self.builder()}.value(
                    {with_span(tok!(&), span)}
                    { expr }
                );]);
            }
            Flatten::Object => {
                self.out.extend(toks![{self.builder()}.raw_object(
                    {with_span(tok!(&), span)}
                    { expr }
                );]);
            }
            Flatten::Array => {
                self.out.extend(toks![{self.builder()}.raw_array(
                    {with_span(tok!(&), span)}
                    { expr }
                );]);
            }
        }
    }
    fn finish_creating(mut self) -> TokenStream {
        self.flush_error();
        if self.out.is_empty() {
            return TokenStream::from_iter(toks![
                String%:from({
                    TokenTree::Literal(Literal::string(&self.text))
                })
            ]);
        }
        self.flush_text();
        let builder = self.builder();

        let capacity = (self.initial_capacity + 16) & (!0b1111);

        let res = TokenStream::from_iter(toks![
            use jsony%:{braces!(OutputBuffer, ToJson)};
            let mut {self.builder()} = jsony%:RawBuf %:with_capacity({
                TokenTree::Literal(Literal::usize_unsuffixed(capacity))
            });
            {{braced(self.out)}}
            { builder }.string
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

        let res = TokenStream::from_iter(toks![
            use jsony%:{braces!(OutputBuffer, ToJson)};
            let {self.builder()} = &mut {TokenTree::Ident(name.clone())}.buffer;
            {{braced(self.out)}}
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
        //todo
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
