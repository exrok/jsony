use crate::{
    lit::{self, literal_inline},
    Flatten,
};
use proc_macro::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
fn parend(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Parenthesis, ts))
}
fn braced(ts: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Brace, ts))
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
fn tt_punct_alone(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Alone)));
}
fn tt_punct_joint(out: &mut Vec<TokenTree>, chr: char) {
    out.push(TokenTree::Punct(Punct::new(chr, Spacing::Joint)));
}
fn tt_ident(out: &mut Vec<TokenTree>, ident: &str) {
    out.push(TokenTree::Ident(Ident::new(ident, Span::call_site())));
}
fn tt_group(out: &mut Vec<TokenTree>, delimiter: Delimiter, from: usize) {
    let group = TokenTree::Group(Group::new(
        delimiter,
        TokenStream::from_iter(out.drain(from..)),
    ));
    out.push(group);
}

struct ObjectParser<'a> {
    topmost: bool,
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
        let mut outer_first_token = true;
        let mut attr = Attr::None;
        'outer: while let Some(mut t) = input.next() {
            loop {
                let first_token = outer_first_token;
                outer_first_token = false;
                match &t {
                    TokenTree::Punct(ch) if ch.as_char() == '@' => {
                        let TokenTree::Group(group) = (if let Some(got) = input.next() {
                            got
                        } else {
                            self.eof(Span::call_site());
                            return;
                        }) else {
                            {
                                if self.error.is_none() {
                                    self.error =
                                        Some((ch.span(), "Expected [] attr".to_string().into()))
                                }
                                return;
                            };
                        };
                        let contents: Vec<TokenTree> = group.stream().into_iter().collect();
                        if let Some(TokenTree::Ident(ident)) = contents.first() {
                            if ident.to_string() == "if" {
                                self.codegen.flush_text();
                                attr = Attr::If {
                                    contents,
                                    codegen_height: self.codegen.out.len(),
                                };
                            }
                        }
                        continue 'outer;
                    }
                    TokenTree::Ident(value) => {
                        let contents = &value.to_string();
                        if contents == "in" {
                            if !first_token || !self.topmost {
                                {
                                    if self.error.is_none() {
                                        self.error =
                                                            Some((value.span(),
                                                                    "in keyword only allowed at beginning of template macro".to_string().into()))
                                    }
                                    return;
                                }
                            }
                            let mut toks: Vec<TokenTree> = Vec::new();
                            loop {
                                let tok = if let Some(got) = input.next() {
                                    got
                                } else {
                                    self.eof(Span::call_site());
                                    return;
                                };
                                if is_char(&tok, ';') {
                                    break;
                                }
                                toks.push(tok);
                            }
                            outer_first_token = true;
                            self.topmost = false;
                            self.codegen.in_writer(toks);
                            continue 'outer;
                        }
                    }
                    _ => {}
                }
                let col = 'value: {
                    if let Some(col) = input.next() {
                        if !is_char(&col, ',') {
                            break 'value col;
                        }
                    }
                    match t {
                        TokenTree::Ident(value) => {
                            let contents = &value.to_string();
                            self.codegen.pre_escaped_key(&contents);
                            self.codegen
                                .value_from_expression(value.span(), TokenTree::Ident(value));
                            self.codegen.text.push(',');
                            self.codegen.entry_completed(&mut attr);
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
                        self.codegen.entry_completed(&mut attr);
                        continue 'outer;
                    }
                    if let TokenTree::Ident(ident) = &t {
                        #[allow(clippy::cmp_owned)]
                        if ident.to_string() != "for" {
                            {
                                if self.error.is_none() {
                                    self.error =
                                        Some((col.span(), "Unexpected 1".to_string().into()))
                                }
                                return;
                            }
                        } else {
                            if !first_token {
                                {
                                    if self.error.is_none() {
                                        self.error = Some((
                                            col.span(),
                                            "For comprehensions must be in there own object"
                                                .to_string()
                                                .into(),
                                        ))
                                    }
                                    return;
                                }
                            }
                        }
                    } else {
                        {
                            if self.error.is_none() {
                                self.error = Some((col.span(), "Unexpected 2".to_string().into()))
                            }
                            return;
                        }
                    }
                    value_tokens.clear();
                    value_tokens.push(t);
                    value_tokens.push(col);
                    loop {
                        let tok = if let Some(got) = input.next() {
                            got
                        } else {
                            self.eof(Span::call_site());
                            return;
                        };
                        if is_char(&tok, ';') {
                            break;
                        }
                        value_tokens.push(tok);
                    }
                    self.codegen.flush_text();
                    self.codegen.out.extend(value_tokens.drain(..));
                    {
                        {
                            let at = (&mut self.codegen.out).len();
                            {
                                self.parse(input);
                                self.codegen.flush_text();
                            };
                            tt_group((&mut self.codegen.out), Delimiter::Brace, at);
                        };
                    };
                    return;
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
                self.codegen.entry_completed(&mut attr);
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
    out: Vec<TokenTree>,
    need_mut_builder: bool,
    builder: Ident,
    text: String,
    initial_capacity: usize,
    error: Option<(Span, Box<str>)>,
    flatten: Flatten,
    writer: Option<Vec<TokenTree>>,
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
impl Codegen {
    fn entry_completed(&mut self, attr: &mut Attr) {
        if match attr {
            Attr::None => true,
            _ => false,
        } {
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
                    self.out.drain(codegen_height..).collect(),
                ));
                self.out.extend(contents);
                self.out.push(body);
            }
        }
    }
    fn in_writer(&mut self, writer: Vec<TokenTree>) {
        self.text.clear();
        self.writer = Some(writer);
    }
    fn new(builder: Ident) -> Codegen {
        Codegen {
            out: Vec::new(),
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
                    {
                        out.push(TokenTree::from(self.builder.clone()));
                        tt_punct_alone(out, '.');
                        tt_ident(out, "push_colon");
                        {
                            let at = out.len();
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                    };
                }
                b',' => {
                    {
                        out.push(TokenTree::from(self.builder.clone()));
                        tt_punct_alone(out, '.');
                        tt_ident(out, "push_comma");
                        {
                            let at = out.len();
                            tt_group(out, Delimiter::Parenthesis, at);
                        };
                        tt_punct_alone(out, ';');
                    };
                }
                _ => {
                    {
                        tt_ident(out, "unsafe");
                        {
                            let at = out.len();
                            out.push(TokenTree::from(self.builder.clone()));
                            tt_punct_alone(out, '.');
                            tt_ident(out, "push_unchecked_ascii");
                            {
                                let at = out.len();
                                out.push(TokenTree::Literal(Literal::byte_character(
                                    self.text.as_bytes()[0],
                                )));
                                tt_group(out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_alone(out, ';');
                            tt_group(out, Delimiter::Brace, at);
                        };
                    };
                }
            }
        } else {
            let out = &mut self.out;
            {
                out.push(TokenTree::from(self.builder.clone()));
                tt_punct_alone(out, '.');
                tt_ident(out, "push_str");
                {
                    let at = out.len();
                    out.push(str_lit(&self.text));
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
            };
        }
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
        let start = self.out.len();
        let mut input = group.stream().into_iter();
        let mut peq = false;
        let mut value_tokens: Vec<TokenTree> = Vec::new();
        while let Some(tok) = input.next() {
            if !(peq && is_char(&tok, '>')) {
                peq = is_char(&tok, '=');
                self.out.push(tok);
                continue;
            }
            let span = tok.span();
            self.out.push(tok);
            munch_expr(&mut input, &mut value_tokens);
            {
                {
                    let at = (&mut self.out).len();
                    {
                        self.insert_value(span, &mut value_tokens);
                        self.flush_text();
                    };
                    tt_group(&mut self.out, Delimiter::Brace, at);
                };
            };
        }
        let mut matches = Group::new(Delimiter::Brace, self.out.drain(start..).collect());
        matches.set_span(group.span());
        self.out.extend(values.drain(..));
        {
            (&mut self.out).push(TokenTree::Group(matches));
            tt_punct_alone(&mut self.out, ';');
        };
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
                        self.text.push('[');
                    }
                    self.flush_text();
                    self.out.extend(token_values.drain(..split + 1).take(split));
                    {
                        {
                            let at = (&mut self.out).len();
                            {
                                self.insert_value(span, &mut token_values);
                                self.text.push(',');
                                self.flush_text();
                            };
                            tt_group(&mut self.out, Delimiter::Brace, at);
                        };
                    };
                    if f == Flatten::None {
                        self.flush_text();
                        {
                            (&mut self.out).push(TokenTree::from(self.builder.clone()));
                            tt_punct_alone(&mut self.out, '.');
                            tt_ident(&mut self.out, "end_json_array");
                            {
                                let at = (&mut self.out).len();
                                tt_group(&mut self.out, Delimiter::Parenthesis, at);
                            };
                            tt_punct_alone(&mut self.out, ';');
                        };
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
    /// This was for function calls
    fn insert_value(&mut self, span: Span, values: &mut Vec<TokenTree>) {
        if values.is_empty() {
            self.error = Some((span, "Expected value after colon".into()));
            return;
        }
        match &values[0] {
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    self.error = Some((span, "Unhandled".into()));
                    return;
                }
                Delimiter::Brace => {
                    let mut parser = ObjectParser {
                        topmost: false,
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
                                        {
                                            tt_punct_alone(&mut self.out, ';');
                                        };
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
                    if let [_, TokenTree::Ident(binding), TokenTree::Punct(end), ..] = &values[..] {
                        if end.as_char() == '|' {
                            self.flush_text();
                            let out = &mut self.out;
                            {
                                {
                                    let at = out.len();
                                    tt_ident(out, "let");
                                    tt_ident(out, "mut");
                                    out.push(TokenTree::from(binding.clone()));
                                    tt_punct_alone(out, '=');
                                    {
                                        match self.flatten {
                                            Flatten::None => {
                                                tt_punct_joint(out, ':');
                                                tt_punct_alone(out, ':');
                                                tt_ident(out, "jsony");
                                                tt_punct_joint(out, ':');
                                                tt_punct_alone(out, ':');
                                                tt_ident(out, "json");
                                                tt_punct_joint(out, ':');
                                                tt_punct_alone(out, ':');
                                                tt_ident(out, "ValueWriter");
                                                tt_punct_joint(out, ':');
                                                tt_punct_alone(out, ':');
                                                tt_ident(out, "new");
                                                {
                                                    let at = out.len();
                                                    out.push(TokenTree::from(self.builder.clone()));
                                                    tt_group(out, Delimiter::Parenthesis, at);
                                                };
                                            }
                                            Flatten::Object => {
                                                self.need_mut_builder = true;
                                                {
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "jsony");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "json");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "ObjectWriter");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "non_terminating");
                                                    {
                                                        let at = out.len();
                                                        tt_punct_alone(out, '&');
                                                        tt_ident(out, "mut");
                                                        out.push(TokenTree::from(
                                                            self.builder.clone(),
                                                        ));
                                                        tt_group(out, Delimiter::Parenthesis, at);
                                                    };
                                                };
                                            }
                                            Flatten::Array => {
                                                self.need_mut_builder = true;
                                                {
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "jsony");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "json");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "ArrayWriter");
                                                    tt_punct_joint(out, ':');
                                                    tt_punct_alone(out, ':');
                                                    tt_ident(out, "non_terminating");
                                                    {
                                                        let at = out.len();
                                                        tt_punct_alone(out, '&');
                                                        tt_ident(out, "mut");
                                                        out.push(TokenTree::from(
                                                            self.builder.clone(),
                                                        ));
                                                        tt_group(out, Delimiter::Parenthesis, at);
                                                    };
                                                };
                                            }
                                        }
                                    };
                                    tt_punct_alone(out, ';');
                                    out.push(TokenTree::Group(Group::new(
                                        Delimiter::Brace,
                                        values.drain(3..).collect(),
                                    )));
                                    tt_punct_alone(out, ';');
                                    tt_group(out, Delimiter::Brace, at);
                                };
                            };
                            return;
                        }
                    }
                }
            }
        }
        if values.len() == 1 {
            self.value_from_expression(values[0].span(), values.pop().unwrap())
        } else {
            self.value_from_expression(
                values[0].span(),
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
        {
            (&mut self.out).push(TokenTree::from(self.builder.clone()));
            tt_punct_alone(&mut self.out, '.');
            tt_ident(&mut self.out, "end_json_array");
            {
                let at = (&mut self.out).len();
                tt_group(&mut self.out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(&mut self.out, ';');
        };
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
        {
            (&mut self.out).push(TokenTree::from(self.builder.clone()));
            tt_punct_alone(&mut self.out, '.');
            tt_ident(&mut self.out, "end_json_object");
            {
                let at = (&mut self.out).len();
                tt_group(&mut self.out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(&mut self.out, ';');
        };
    }
    fn dyn_key(&mut self, _span: Span, expr: TokenStream) {
        self.flush_text();
        {
            tt_ident(&mut self.out, "let");
            tt_ident(&mut self.out, "_");
            tt_punct_alone(&mut self.out, ':');
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "jsony");
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "json");
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "StringValue");
            tt_punct_alone(&mut self.out, '=');
            tt_punct_alone(&mut self.out, '<');
            tt_ident(&mut self.out, "_");
            tt_ident(&mut self.out, "as");
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "jsony");
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "ToJson");
            tt_punct_alone(&mut self.out, '>');
            tt_punct_joint(&mut self.out, ':');
            tt_punct_alone(&mut self.out, ':');
            tt_ident(&mut self.out, "jsonify_into");
            {
                let at = (&mut self.out).len();
                tt_punct_alone(&mut self.out, '&');
                (&mut self.out).push(parend(expr));
                tt_punct_alone(&mut self.out, ',');
                (&mut self.out).push(TokenTree::from(self.builder.clone()));
                tt_group(&mut self.out, Delimiter::Parenthesis, at);
            };
            tt_punct_alone(&mut self.out, ';');
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
    fn value_from_expression(&mut self, _span: Span, expr: TokenTree) {
        self.initial_capacity += 2;
        self.flush_text();
        match self.flatten {
            Flatten::None => {
                tt_punct_alone(&mut self.out, '<');
                tt_ident(&mut self.out, "_");
                tt_ident(&mut self.out, "as");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsony");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "ToJson");
                tt_punct_alone(&mut self.out, '>');
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsonify_into");
                {
                    let at = (&mut self.out).len();
                    tt_punct_alone(&mut self.out, '&');
                    (&mut self.out).push(expr);
                    tt_punct_alone(&mut self.out, ',');
                    (&mut self.out).push(TokenTree::from(self.builder.clone()));
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
            }
            Flatten::Object => {
                (&mut self.out).push(TokenTree::from(self.builder.clone()));
                tt_punct_alone(&mut self.out, '.');
                tt_ident(&mut self.out, "join_parent_json_value_with_next");
                {
                    let at = (&mut self.out).len();
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
                tt_ident(&mut self.out, "let");
                tt_ident(&mut self.out, "_");
                tt_punct_alone(&mut self.out, ':');
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsony");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "json");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "ObjectValue");
                tt_punct_alone(&mut self.out, '=');
                tt_punct_alone(&mut self.out, '<');
                tt_ident(&mut self.out, "_");
                tt_ident(&mut self.out, "as");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsony");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "ToJson");
                tt_punct_alone(&mut self.out, '>');
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsonify_into");
                {
                    let at = (&mut self.out).len();
                    tt_punct_alone(&mut self.out, '&');
                    (&mut self.out).push(expr);
                    tt_punct_alone(&mut self.out, ',');
                    (&mut self.out).push(TokenTree::from(self.builder.clone()));
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
                (&mut self.out).push(TokenTree::from(self.builder.clone()));
                tt_punct_alone(&mut self.out, '.');
                tt_ident(&mut self.out, "join_object_with_next_value");
                {
                    let at = (&mut self.out).len();
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
            }
            Flatten::Array => {
                (&mut self.out).push(TokenTree::from(self.builder.clone()));
                tt_punct_alone(&mut self.out, '.');
                tt_ident(&mut self.out, "join_parent_json_value_with_next");
                {
                    let at = (&mut self.out).len();
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
                tt_ident(&mut self.out, "let");
                tt_ident(&mut self.out, "_");
                tt_punct_alone(&mut self.out, ':');
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsony");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "json");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "ArrayValue");
                tt_punct_alone(&mut self.out, '=');
                tt_punct_alone(&mut self.out, '<');
                tt_ident(&mut self.out, "_");
                tt_ident(&mut self.out, "as");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsony");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "ToJson");
                tt_punct_alone(&mut self.out, '>');
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "jsonify_into");
                {
                    let at = (&mut self.out).len();
                    tt_punct_alone(&mut self.out, '&');
                    (&mut self.out).push(expr);
                    tt_punct_alone(&mut self.out, ',');
                    (&mut self.out).push(TokenTree::from(self.builder.clone()));
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
                (&mut self.out).push(TokenTree::from(self.builder.clone()));
                tt_punct_alone(&mut self.out, '.');
                tt_ident(&mut self.out, "join_array_with_next_value");
                {
                    let at = (&mut self.out).len();
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(&mut self.out, ';');
            }
        }
    }
    fn finish_creating(mut self) -> TokenStream {
        self.flush_error();
        if let Some(writer) = self.writer.take() {
            self.flush_text();
            let braced = braced(self.out.drain(..).collect());
            return {
                let len = (&mut self.out).len();
                {
                    let at = (&mut self.out).len();
                    tt_ident(&mut self.out, "let");
                    tt_ident(&mut self.out, "mut");
                    tt_ident(&mut self.out, "object_writer");
                    tt_punct_alone(&mut self.out, ':');
                    tt_punct_alone(&mut self.out, '&');
                    tt_ident(&mut self.out, "mut");
                    tt_punct_joint(&mut self.out, ':');
                    tt_punct_alone(&mut self.out, ':');
                    tt_ident(&mut self.out, "jsony");
                    tt_punct_joint(&mut self.out, ':');
                    tt_punct_alone(&mut self.out, ':');
                    tt_ident(&mut self.out, "json");
                    tt_punct_joint(&mut self.out, ':');
                    tt_punct_alone(&mut self.out, ':');
                    tt_ident(&mut self.out, "ObjectWriter");
                    tt_punct_alone(&mut self.out, '=');
                    (&mut self.out).extend_from_slice(&writer);
                    tt_punct_alone(&mut self.out, ';');
                    tt_ident(&mut self.out, "let");
                    if self.need_mut_builder {
                        tt_ident(&mut self.out, "mut");
                    };
                    tt_ident(&mut self.out, "builder");
                    tt_punct_alone(&mut self.out, '=');
                    tt_ident(&mut self.out, "object_writer");
                    tt_punct_alone(&mut self.out, '.');
                    tt_ident(&mut self.out, "inner_writer");
                    {
                        let at = (&mut self.out).len();
                        tt_group(&mut self.out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(&mut self.out, ';');
                    tt_ident(&mut self.out, "builder");
                    tt_punct_alone(&mut self.out, '.');
                    tt_ident(&mut self.out, "smart_object_comma");
                    {
                        let at = (&mut self.out).len();
                        tt_group(&mut self.out, Delimiter::Parenthesis, at);
                    };
                    tt_punct_alone(&mut self.out, ';');
                    (&mut self.out).push(braced);
                    tt_group(&mut self.out, Delimiter::Brace, at);
                };
                TokenStream::from_iter((&mut self.out).drain(len..))
            };
        }
        if self.out.is_empty() {
            return {
                let len = (&mut self.out).len();
                tt_ident(&mut self.out, "String");
                tt_punct_joint(&mut self.out, ':');
                tt_punct_alone(&mut self.out, ':');
                tt_ident(&mut self.out, "from");
                {
                    let at = (&mut self.out).len();
                    (&mut self.out).push(str_lit(&self.text));
                    tt_group(&mut self.out, Delimiter::Parenthesis, at);
                };
                TokenStream::from_iter((&mut self.out).drain(len..))
            };
        }
        self.flush_text();
        let capacity = (self.initial_capacity + 16) & (!0b1111);
        let braced = braced(self.out.drain(..).collect());
        let out = &mut self.out;
        {
            let len = out.len();
            {
                let at = out.len();
                tt_ident(out, "let");
                tt_ident(out, "mut");
                tt_ident(out, "_builder");
                tt_punct_alone(out, '=');
                tt_ident(out, "jsony");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "TextWriter");
                tt_punct_joint(out, ':');
                tt_punct_alone(out, ':');
                tt_ident(out, "with_capacity");
                {
                    let at = out.len();
                    out.push(TokenTree::from(Literal::usize_unsuffixed(capacity).clone()));
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_punct_alone(out, ';');
                {
                    let at = out.len();
                    tt_ident(out, "let");
                    if self.need_mut_builder {
                        tt_ident(out, "mut");
                    };
                    out.push(TokenTree::from(self.builder.clone()));
                    tt_punct_alone(out, '=');
                    tt_punct_alone(out, '&');
                    tt_ident(out, "mut");
                    tt_ident(out, "_builder");
                    tt_punct_alone(out, ';');
                    out.push(braced);
                    tt_group(out, Delimiter::Brace, at);
                };
                tt_ident(out, "_builder");
                tt_punct_alone(out, '.');
                tt_ident(out, "into_string");
                {
                    let at = out.len();
                    tt_group(out, Delimiter::Parenthesis, at);
                };
                tt_group(out, Delimiter::Brace, at);
            };
            TokenStream::from_iter(out.drain(len..))
        }
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
}
pub fn array(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    codegen.parse_inline_array(Span::call_site(), input);
    codegen.finish_creating()
}
pub fn object(input: TokenStream) -> TokenStream {
    let mut codegen = Codegen::new(Ident::new("builder", Span::call_site()));
    let mut parser = ObjectParser {
        topmost: true,
        codegen: &mut codegen,
        error: None,
    };
    parser.codegen.begin_inline_object();
    parser.parse(input.into_iter());
    if parser.codegen.writer.is_none() {
        parser.codegen.end_inline_object();
    }
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
