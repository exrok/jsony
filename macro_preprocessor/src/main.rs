use ra_ap_rustc_lexer::{tokenize, TokenKind};
use std::ops::Range;
use std::path::PathBuf;
use std::slice::Iter as SliceIter;

#[derive(Debug, Hash, PartialEq, Eq)]
enum Kind {
    PushIdent,
    PushPunctAlone,
    PushPunctJoint,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Fnctl<'a> {
    kind: Kind,
    buffer: &'a str,
    literal: &'a str,
}
fn munch_thing<'a>(
    raw: &'a str,
    from: &mut SliceIter<'_, (TokenKind, Range<usize>)>,
) -> Option<(Fnctl<'a>, Range<usize>)> {
    let (start, kind) = loop {
        match from.next() {
            Some((TokenKind::Ident, range)) => match &raw[range.clone()] {
                "tt_ident" => break (range, Kind::PushIdent),
                "tt_punct_joint" => break (range, Kind::PushPunctJoint),
                "tt_punct_alone" => break (range, Kind::PushPunctAlone),
                _ => return None,
            },
            Some((TokenKind::Whitespace, _)) => continue,
            _ => return None,
        }
    };
    let mut toks = from.clone();
    // let mut toks = toks.filter(|(tok, _)| !matches!(tok, TokenKind::Whitespace));
    let mut next = || loop {
        match toks.next() {
            Some((TokenKind::Whitespace, _)) => continue,
            other => return other,
        }
    };
    let (paren, paren_span) = next()?;
    if *paren != TokenKind::OpenParen {
        return None;
    }
    let buffer_name = loop {
        let (tok, tok_span) = next()?;
        if *tok == TokenKind::Comma {
            break raw[paren_span.end..tok_span.start].trim();
        }
    };
    let (TokenKind::Literal { .. }, value) = next()? else {
        return None;
    };
    if next()?.0 != TokenKind::CloseParen {
        return None;
    }
    let (TokenKind::Semi, end) = next()? else {
        return None;
    };
    *from = toks;
    Some((
        Fnctl {
            kind,
            buffer: buffer_name,
            literal: &raw[value.clone()],
        },
        start.start..end.end,
    ))
}

fn modules(input: &str) -> impl Iterator<Item = (&str, &str)> {
    input.split("\nmod ").filter_map(|foo| {
        let (name, rest) = foo.split_once("{\n")?;
        let (body, _) = rest.split_once("\n}")?;
        Some((name.trim(), body))
    })
}

fn line_by_line_transform(input: &str) -> String {
    let mut output = Vec::with_capacity(input.len());
    let input = input.as_bytes();
    let mut iter = memchr::memchr_iter(b'\n', input);
    let mut line_start = 0;
    let mut write_from = 0;
    loop {
        let mut content_start = line_start;
        let mut indent = 0;
        while input.get(content_start) == Some(&b' ') {
            indent += 1;
            content_start += 1;
        }
        let prefix = b"use proc_macro2";
        if input[content_start..].starts_with(b"#[rustfmt::skip]") {
            output.extend_from_slice(&input[write_from..content_start]);
            write_from = content_start + "#[rustfmt::skip]".len();
        } else if input[content_start..].starts_with(prefix) {
            output.extend_from_slice(&input[write_from..content_start]);
            write_from = content_start + prefix.len();
            output.extend_from_slice(b"use proc_macro");
        } else if input[content_start..].starts_with(b"macro_rules! ") {
            output.extend_from_slice(&input[write_from..line_start]);
            loop {
                let line_start = iter.next().expect("Well formed macro") + 1;
                let mut inner_content_start = line_start;
                let mut inner_indent = 0;
                while input.get(inner_content_start) == Some(&b' ') {
                    inner_indent += 1;
                    inner_content_start += 1;
                }
                if inner_indent != indent {
                    continue;
                }
                if input[inner_content_start..].starts_with(b"}\n") {
                    write_from = inner_content_start + 2;
                    break;
                }
            }
        }

        line_start = match iter.next() {
            Some(end) => end + 1,
            None => break,
        };
    }
    output.extend_from_slice(&input[write_from..]);
    unsafe { String::from_utf8_unchecked(output) }
}

#[allow(dead_code)]
fn merge_tts(bytes: &[u8]) -> Vec<u8> {
    let mut end = 0;
    let data = std::str::from_utf8(bytes).unwrap();
    let tokens: Vec<(TokenKind, Range<usize>)> = tokenize(&data)
        .map(move |tok| {
            let start = end;
            end += tok.len as usize;
            (tok.kind, start..end)
        })
        .collect();
    let mut iter = tokens.iter();
    // let mut thetoks: HashSet<Fnctl<'_>> = HashSet::new();
    let mut output = Vec::with_capacity(data.len());
    let mut write_from = 0;
    let mut handle_group = |group: &[(Fnctl, std::ops::Range<usize>)]| {
        if group.len() <= 1 {
            return;
        }
        output.extend_from_slice(&bytes[write_from..group[0].1.start]);
        write_from = group.last().unwrap().1.end;
        output.extend_from_slice(b"tt_append(");
        output.extend_from_slice(group[0].0.buffer.as_bytes());
        output.extend_from_slice(b",&[");
        for tt in group {
            match tt.0.kind {
                Kind::PushIdent => {
                    output.extend_from_slice(b"StaticIdent(");
                    output.extend_from_slice(tt.0.literal.as_bytes());
                    output.extend_from_slice(b"),");
                }
                Kind::PushPunctAlone => {
                    output.extend_from_slice(b"StaticPunct(");
                    output.extend_from_slice(tt.0.literal.as_bytes());
                    output.extend_from_slice(b",true),");
                }
                Kind::PushPunctJoint => {
                    output.extend_from_slice(b"StaticPunct(");
                    output.extend_from_slice(tt.0.literal.as_bytes());
                    output.extend_from_slice(b",false),");
                }
            }
        }
        output.extend_from_slice(b"]);");
    };
    let mut current_group: Vec<(Fnctl, std::ops::Range<usize>)> = Vec::new();
    let mut current_buffer = "";
    while iter.len() > 0 {
        if let Some((fnctl, range)) = munch_thing(&data, &mut iter) {
            if fnctl.buffer != current_buffer {
                if !current_group.is_empty() {
                    handle_group(&current_group);
                    current_group.clear();
                }
                current_buffer = fnctl.buffer;
                current_group.push((fnctl, range))
            } else {
                current_buffer = fnctl.buffer;
                current_group.push((fnctl, range))
            }
        } else {
            if !current_group.is_empty() {
                handle_group(&current_group);
                current_group.clear();
            }
        }
    }
    handle_group(&current_group);
    output.extend_from_slice(&bytes[write_from..]);
    output
}
#[test]
fn tt_merging2() {
    let value = merge_tts(
        br#"
tt_ident(&mut output, "encoder");
tt_punct_alone(&mut output, '.');
foo();
tt_punct_alone(output, '<');
tt_punct_joint(output, '\'');
tt_punct_joint(dude, '\'');
"#,
    );
    let expected = r#"
tt_append(&mut output,&[StaticIdent("encoder"),StaticPunct('.',true),]);
foo();
tt_append(output,&[StaticPunct('<',true),StaticPunct('\'',false),]);
tt_punct_joint(dude, '\'');
"#;
    assert_eq!(std::str::from_utf8(&value).unwrap().trim(), expected.trim())
}
#[test]
fn tt_merging() {
    let value = merge_tts(
        br#"
            {
                let at = output.len();
                tt_ident(output, "decoder");
                tt_punct_alone(output, ':');
                tt_punct_alone(output, '&');
                tt_ident(output, "mut");
                output.extend_from_slice(&crate_path);
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "binary");
                tt_punct_joint(output, ':');
                tt_punct_alone(output, ':');
                tt_ident(output, "Decoder");
                tt_punct_alone(output, '<');
                tt_punct_joint(output, '\'');
                output.push(TokenTree::from(lifetime.clone()));
                tt_punct_alone(output, '>');
                tt_group(output, Delimiter::Parenthesis, at);
            };
"#,
    );
    let expected = r#"
tt_append(&mut output,&[StaticIdent("encoder"),StaticPunct('.',true),]);
foo();
tt_append(output,&[StaticPunct('<',true),StaticPunct('\'',false),]);
tt_punct_joint(dude, '\'');
"#;
    let fmted = std::str::from_utf8(&value).unwrap().trim();
    println!("{}", fmted);
    assert_eq!(fmted, expected.trim())
}

fn pipe_rustfmt(data: &[u8]) -> Vec<u8> {
    let mut rustfmt = std::process::Command::new("rustfmt")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write;
    rustfmt.stdin.as_mut().unwrap().write_all(data).unwrap();
    let output = rustfmt.wait_with_output().unwrap();
    output.stdout
}

fn main() {
    let mut args = std::env::args();
    let _ = args.next();
    let source_crate_path: PathBuf = args.next().unwrap().into();
    let final_crate_path: PathBuf = args.next().unwrap().into();

    let output = std::process::Command::new("cargo")
        .args([
            "expand",
            "--bin",
            "jsony_macros_source",
            "--ugly",
            "--color",
            "never",
            "--no-default-features",
        ])
        .current_dir(source_crate_path)
        .output()
        .unwrap();

    let data = String::from_utf8(output.stdout).unwrap();
    for (name, body) in modules(&data) {
        if name == "ast" {
            let res = line_by_line_transform(body);
            let tx = pipe_rustfmt(res.as_bytes());
            let res = std::str::from_utf8(&tx).unwrap();
            let res = res.replace("((&mut output),", "(&mut output,");
            let res = res.replace(" (&mut output).", " output.");
            let res = res.replace("((&mut out),", "(&mut out,");
            let res = res.replace(" (&mut out).", " out.");
            let res = res.replace(
                "::core::panicking::panic(\"not yet implemented\")",
                "todo!()",
            );
            std::fs::write(
                final_crate_path.join("src/ast.rs"),
                pipe_rustfmt(res.as_bytes()),
            )
            .unwrap();
        } else if name == "codegen" {
            let res = line_by_line_transform(body);
            let tx = pipe_rustfmt(res.as_bytes());
            let res = std::str::from_utf8(&tx).unwrap();
            let res = res.replace("((&mut output),", "(&mut output,");
            let res = res.replace(" (&mut output).", " output.");
            let res = res.replace("((&mut out),", "(&mut out,");
            let res = res.replace(" (&mut out).", " out.");
            let res = res.replace(
                "::core::panicking::panic(\"not yet implemented\")",
                "todo!()",
            );
            let res = res.as_bytes();
            // let res = merge_tts(res);

            std::fs::write(final_crate_path.join("src/codegen.rs"), pipe_rustfmt(&res)).unwrap();
        } else if name == "template" {
            let res = line_by_line_transform(body);
            let tx = pipe_rustfmt(res.as_bytes());
            let res = std::str::from_utf8(&tx).unwrap();
            let res = res.replace("((&mut output),", "(&mut output,");
            let res = res.replace("((&mut self.out),", "(&mut self.out,");
            let res = res.replace(" (&mut output).", " output.");
            let res = res.replace("((&mut out),", "(&mut out,");
            let res = res.replace(" (&mut out).", " out.");
            let res = res.replace(
                "::core::panicking::panic(\"not yet implemented\")",
                "todo!()",
            );

            std::fs::write(
                final_crate_path.join("src/template.rs"),
                pipe_rustfmt(res.as_bytes()),
            )
            .unwrap();
        }
    }
}
