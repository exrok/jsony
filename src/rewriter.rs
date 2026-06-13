//! JSON prettifier: re-formats a JSON document with newlines and indentation.
//!
//! The implementation walks the input with the existing [`crate::parser`]
//! primitives and copies tokens verbatim from the source. It is intentionally
//! non-generic and avoids closures to keep the codegen footprint small.

use crate::json::DecodeError;
use crate::parser::{InnerParser, Parser, Peek};
use crate::{JsonError, JsonParserConfig};

/// Configuration for [`prettify`].
///
/// `indent` is the string emitted per nesting level (typically `"  "` or
/// `"\t"`). `parser` controls the same leniency knobs as the rest of the
/// crate (trailing commas, comments, trailing data).
///
/// `max_line_width` enables single-line rendering of small containers. A
/// container is kept on one line when its compact form fits the width
/// remaining at its nesting depth (`max_line_width - depth * indent.len()`).
/// The in-line key prefix is not counted, so a long key may overshoot the
/// width. `0` (the default) always expands every container.
///
/// `max_inline_depth` caps how many container levels may stack on one inlined
/// line: the outermost inlined container is level 1, a container nested inside
/// it is level 2, and so on. A container is expanded if inlining it would
/// exceed this. This bounds the work of a single inline attempt and doubles as
/// a sizing knob. The default imposes no limit.
///
/// Comments (when allowed) are dropped from the output. Unquoted field keys
/// are not supported by the prettifier and will return an error even if
/// `allow_unquoted_field_keys` is set on the parser config.
#[derive(Clone, Copy)]
pub struct PrettifyConfig<'a> {
    /// Indent string emitted once per nesting level.
    pub indent: &'a str,
    /// Approximate target line width in bytes. `0` disables inlining.
    pub max_line_width: usize,
    /// Maximum container nesting permitted on a single inlined line.
    pub max_inline_depth: usize,
    /// Parser configuration controlling leniency and recursion limits.
    pub parser: JsonParserConfig,
}

impl Default for PrettifyConfig<'_> {
    fn default() -> Self {
        Self {
            indent: "  ",
            max_line_width: 0,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig::default(),
        }
    }
}

static TRAILING_CHARACTERS: DecodeError = DecodeError {
    message: "Trailing characters",
};

/// Re-formats a JSON document with newlines and indentation.
///
/// Numbers and strings are copied verbatim from the input rather than being
/// decoded and re-encoded. Comments (when allowed via the parser config) are
/// stripped from the output.
///
/// # Errors
///
/// Returns a [`JsonError`] if the input is not valid JSON under the given
/// parser configuration.
///
/// # Example
///
/// ```
/// let input = r#"{"a":[1,2,3]}"#;
/// let out = jsony::prettify(input, &Default::default()).unwrap();
/// assert_eq!(out, "{\n  \"a\": [\n    1,\n    2,\n    3\n  ]\n}");
/// ```
pub fn prettify(json: &str, config: &PrettifyConfig<'_>) -> Result<String, JsonError> {
    let mut parser = Parser::new(json, config.parser);
    let mut out = String::with_capacity(json.len() + json.len() / 8);
    match walk(
        &mut parser,
        json,
        &mut out,
        config.indent,
        config.max_line_width,
        config.max_inline_depth,
    ) {
        Ok(()) => {
            if config.parser.allow_trailing_data || parser.at.eat_whitespace().is_none() {
                Ok(out)
            } else {
                Err(JsonError::new(&TRAILING_CHARACTERS, None))
            }
        }
        Err(err) => Err(JsonError::extract(err, &mut parser)),
    }
}

fn write_newline_indent(out: &mut String, indent: &str, depth: usize) {
    out.push('\n');
    for _ in 0..depth {
        out.push_str(indent);
    }
}

/// Snapshots the parser index, advances past one scalar value (including
/// strings, where the parser walks escapes for us), then copies the consumed
/// source slice verbatim. Caller must ensure the parser is not at `{` or `[`.
fn copy_scalar_verbatim(
    p: &mut InnerParser<'_>,
    json: &str,
    out: &mut String,
) -> Result<(), &'static DecodeError> {
    let start = p.index;
    match p.skip_value() {
        Ok(()) => {}
        Err(e) => return Err(e),
    }
    out.push_str(&json[start..p.index]);
    Ok(())
}

fn emit_key(
    p: &mut InnerParser<'_>,
    json: &str,
    out: &mut String,
) -> Result<(), &'static DecodeError> {
    match copy_scalar_verbatim(p, json, out) {
        Ok(()) => {}
        Err(e) => return Err(e),
    }
    match p.discard_colon() {
        Ok(()) => {}
        Err(e) => return Err(e),
    }
    out.push_str(": ");
    Ok(())
}

/// Copies a scalar (or object key) verbatim while honoring the inline budget.
///
/// Unlike [`copy_scalar_verbatim`] the source span is measured before being
/// appended, so an oversized scalar aborts the inline attempt without first
/// copying its bytes into `out`. Returns `false` on overflow or parse error.
fn flat_scalar(p: &mut InnerParser<'_>, json: &str, out: &mut String, limit: usize) -> bool {
    let start = p.index;
    if p.skip_value().is_err() {
        return false;
    }
    let end = p.index;
    if out.len() + (end - start) > limit {
        return false;
    }
    out.push_str(&json[start..end]);
    true
}

/// Emits the compact, single-line rendering of the value at the cursor.
///
/// Separators are `", "` between elements and `": "` after keys, with no
/// newlines or indentation. Returns `false` the moment the output would exceed
/// `limit` (the maximum permitted `out.len()`), the container nesting would
/// exceed `depth` levels, or the input is malformed. On `false` the caller must
/// truncate `out` and restore the parser. `depth` is the number of container
/// levels still permitted (the current container consumes one); it also caps
/// recursion, bounding the work of a single inline attempt.
fn flat_value(
    p: &mut InnerParser<'_>,
    json: &str,
    out: &mut String,
    limit: usize,
    depth: usize,
    peek: Peek,
) -> bool {
    match peek {
        Peek::Array => {
            if depth == 0 {
                return false;
            }
            out.push('[');
            match p.enter_seen_array() {
                Ok(Some(mut elem)) => loop {
                    if !flat_value(p, json, out, limit, depth - 1, elem) {
                        return false;
                    }
                    match p.array_step() {
                        Ok(Some(next)) => {
                            out.push_str(", ");
                            if out.len() > limit {
                                return false;
                            }
                            elem = next;
                        }
                        Ok(None) => {
                            out.push(']');
                            return out.len() <= limit;
                        }
                        Err(_) => return false,
                    }
                },
                Ok(None) => {
                    out.push(']');
                    out.len() <= limit
                }
                Err(_) => false,
            }
        }
        Peek::Object => {
            if depth == 0 {
                return false;
            }
            out.push('{');
            match p.enter_seen_object_at_first_key() {
                Ok(Some(())) => loop {
                    if !flat_scalar(p, json, out, limit) {
                        return false;
                    }
                    if p.discard_colon().is_err() {
                        return false;
                    }
                    out.push_str(": ");
                    if out.len() > limit {
                        return false;
                    }
                    let value = match p.peek() {
                        Ok(value) => value,
                        Err(_) => return false,
                    };
                    if !flat_value(p, json, out, limit, depth - 1, value) {
                        return false;
                    }
                    match p.object_step_at_key() {
                        Ok(Some(())) => {
                            out.push_str(", ");
                            if out.len() > limit {
                                return false;
                            }
                        }
                        Ok(None) => {
                            out.push('}');
                            return out.len() <= limit;
                        }
                        Err(_) => return false,
                    }
                },
                Ok(None) => {
                    out.push('}');
                    out.len() <= limit
                }
                Err(_) => false,
            }
        }
        _ => flat_scalar(p, json, out, limit),
    }
}

/// One entry per open container. The vector length is the current nesting
/// depth used for indentation. Growth is bounded by the parser's
/// `recursion_limit`: every push is paired with a `recursion_limit` decrement
/// inside `enter_seen_*`, and every pop with an increment inside the `*_step`
/// close, so the walk stays iterative and cannot overflow the native stack.
#[derive(Clone, Copy)]
enum Frame {
    Array,
    Object,
}

fn walk(
    parser: &mut Parser<'_>,
    json: &str,
    out: &mut String,
    indent: &str,
    max_line_width: usize,
    max_inline_depth: usize,
) -> Result<(), &'static DecodeError> {
    let mut stack: Vec<Frame> = Vec::with_capacity(16);

    let mut peek = match parser.at.peek() {
        Ok(p) => p,
        Err(e) => return Err(e),
    };

    'outer: loop {
        // Try to keep the container on a single line when its compact form fits
        // the width remaining at this depth. On overflow or parse error the
        // partial output is truncated and the parser rewound, then the expanded
        // path below runs (re-surfacing any genuine error).
        let budget = max_line_width.saturating_sub(stack.len() * indent.len());
        let inlined =
            if budget > 0 && max_inline_depth > 0 && matches!(peek, Peek::Array | Peek::Object) {
                let start = out.len();
                let snap_index = parser.at.index;
                let snap_limit = parser.at.config.recursion_limit;
                if flat_value(
                    &mut parser.at,
                    json,
                    out,
                    start + budget,
                    max_inline_depth,
                    peek,
                ) {
                    true
                } else {
                    out.truncate(start);
                    parser.at.index = snap_index;
                    parser.at.config.recursion_limit = snap_limit;
                    false
                }
            } else {
                false
            };

        if !inlined {
            match peek {
                Peek::Array => {
                    out.push('[');
                    match parser.at.enter_seen_array() {
                        Ok(Some(p)) => {
                            stack.push(Frame::Array);
                            write_newline_indent(out, indent, stack.len());
                            peek = p;
                            continue 'outer;
                        }
                        Ok(None) => {
                            out.push(']');
                        }
                        Err(e) => return Err(e),
                    }
                }
                Peek::Object => {
                    out.push('{');
                    match parser.at.enter_seen_object_at_first_key() {
                        Ok(Some(())) => {
                            stack.push(Frame::Object);
                            write_newline_indent(out, indent, stack.len());
                            match emit_key(&mut parser.at, json, out) {
                                Ok(()) => {}
                                Err(e) => return Err(e),
                            }
                            peek = match parser.at.peek() {
                                Ok(p) => p,
                                Err(e) => return Err(e),
                            };
                            continue 'outer;
                        }
                        Ok(None) => {
                            out.push('}');
                        }
                        Err(e) => return Err(e),
                    }
                }
                _ => match copy_scalar_verbatim(&mut parser.at, json, out) {
                    Ok(()) => {}
                    Err(e) => return Err(e),
                },
            }
        }

        loop {
            let top = match stack.last() {
                Some(&t) => t,
                None => return Ok(()),
            };
            match top {
                Frame::Object => match parser.at.object_step_at_key() {
                    Ok(Some(())) => {
                        out.push(',');
                        write_newline_indent(out, indent, stack.len());
                        match emit_key(&mut parser.at, json, out) {
                            Ok(()) => {}
                            Err(e) => return Err(e),
                        }
                        peek = match parser.at.peek() {
                            Ok(p) => p,
                            Err(e) => return Err(e),
                        };
                        continue 'outer;
                    }
                    Ok(None) => {
                        stack.pop();
                        write_newline_indent(out, indent, stack.len());
                        out.push('}');
                    }
                    Err(e) => return Err(e),
                },
                Frame::Array => match parser.at.array_step() {
                    Ok(Some(p)) => {
                        out.push(',');
                        write_newline_indent(out, indent, stack.len());
                        peek = p;
                        continue 'outer;
                    }
                    Ok(None) => {
                        stack.pop();
                        write_newline_indent(out, indent, stack.len());
                        out.push(']');
                    }
                    Err(e) => return Err(e),
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pretty(json: &str) -> String {
        prettify(json, &PrettifyConfig::default()).unwrap()
    }

    #[test]
    fn empty_containers() {
        assert_eq!(pretty("[]"), "[]");
        assert_eq!(pretty("{}"), "{}");
        assert_eq!(pretty("  []  "), "[]");
    }

    #[test]
    fn scalars() {
        assert_eq!(pretty("null"), "null");
        assert_eq!(pretty("true"), "true");
        assert_eq!(pretty("false"), "false");
        assert_eq!(pretty("42"), "42");
        assert_eq!(pretty("-3.14e2"), "-3.14e2");
        assert_eq!(pretty(r#""hello""#), r#""hello""#);
    }

    #[test]
    fn nested_mixed() {
        let input = r#"{"a":[1,2,{"b":null}],"c":"x\n"}"#;
        let expected = "{\n  \"a\": [\n    1,\n    2,\n    {\n      \"b\": null\n    }\n  ],\n  \"c\": \"x\\n\"\n}";
        assert_eq!(pretty(input), expected);
    }

    #[test]
    fn strings_preserve_escapes_verbatim() {
        // Escape sequences and UTF-8 bytes are copied byte-for-byte.
        let input = r#"["a\nb","ÿ","\\","\""]"#;
        let expected = "[\n  \"a\\nb\",\n  \"ÿ\",\n  \"\\\\\",\n  \"\\\"\"\n]";
        assert_eq!(pretty(input), expected);

        // \u escapes also pass through unchanged (not decoded to UTF-8 bytes).
        let input_u = "[\"\\u00ff\"]";
        let expected_u = "[\n  \"\\u00ff\"\n]";
        assert_eq!(pretty(input_u), expected_u);
    }

    #[test]
    fn numbers_verbatim() {
        let input = r#"[0,1,-1,1.5,1e10,1.5e-3,1E+2]"#;
        let expected = "[\n  0,\n  1,\n  -1,\n  1.5,\n  1e10,\n  1.5e-3,\n  1E+2\n]";
        assert_eq!(pretty(input), expected);
    }

    #[test]
    fn custom_indent() {
        let cfg = PrettifyConfig {
            indent: "\t",
            max_line_width: 0,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig::default(),
        };
        let out = prettify(r#"{"a":1}"#, &cfg).unwrap();
        assert_eq!(out, "{\n\t\"a\": 1\n}");
    }

    #[test]
    fn nested_empty_containers_inline() {
        assert_eq!(pretty(r#"[[],{}]"#), "[\n  [],\n  {}\n]");
    }

    #[test]
    fn malformed_returns_error() {
        assert!(prettify("{", &PrettifyConfig::default()).is_err());
        assert!(prettify("[1,]", &PrettifyConfig::default()).is_err());
        assert!(prettify("not json", &PrettifyConfig::default()).is_err());
    }

    #[test]
    fn trailing_data_rejected_by_default() {
        assert!(prettify("1 2", &PrettifyConfig::default()).is_err());
    }

    #[test]
    fn trailing_data_allowed_with_flag() {
        let cfg = PrettifyConfig {
            indent: "  ",
            max_line_width: 0,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig {
                allow_trailing_data: true,
                ..JsonParserConfig::default()
            },
        };
        assert_eq!(prettify("1 garbage", &cfg).unwrap(), "1");
    }

    #[test]
    fn trailing_commas_lenient() {
        let cfg = PrettifyConfig {
            indent: "  ",
            max_line_width: 0,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig {
                allow_trailing_commas: true,
                ..JsonParserConfig::default()
            },
        };
        assert_eq!(prettify("[1,2,]", &cfg).unwrap(), "[\n  1,\n  2\n]");
    }

    #[test]
    fn deep_nesting_is_iterative() {
        // A naive recursive prettifier would overflow the native stack here.
        // The iterative walk must surface the parser's recursion_limit error
        // instead of crashing.
        let deep = format!("{}1{}", "[".repeat(100_000), "]".repeat(100_000));
        assert!(prettify(&deep, &PrettifyConfig::default()).is_err());

        // Nesting within the default recursion_limit round-trips.
        let shallow = format!("{}1{}", "[".repeat(100), "]".repeat(100));
        assert!(prettify(&shallow, &PrettifyConfig::default()).is_ok());
    }

    #[test]
    fn unquoted_keys_unsupported() {
        let cfg = PrettifyConfig {
            indent: "  ",
            max_line_width: 0,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig {
                allow_unquoted_field_keys: true,
                ..JsonParserConfig::default()
            },
        };
        assert!(prettify("{a:1}", &cfg).is_err());
    }

    fn pretty_width(json: &str, max_line_width: usize) -> String {
        pretty_full(json, max_line_width, usize::MAX)
    }

    fn pretty_full(json: &str, max_line_width: usize, max_inline_depth: usize) -> String {
        let cfg = PrettifyConfig {
            indent: "  ",
            max_line_width,
            max_inline_depth,
            parser: JsonParserConfig::default(),
        };
        prettify(json, &cfg).unwrap()
    }

    #[test]
    fn inline_small_array() {
        // The object (flat 23 > 20) expands, its array value (flat 16 <= 18)
        // inlines, and the inner arrays inline within it.
        assert_eq!(
            pretty_width(r#"{"a":[[0,1],[1,0]]}"#, 20),
            "{\n  \"a\": [[0, 1], [1, 0]]\n}"
        );
    }

    #[test]
    fn inline_whole_document() {
        // A width wide enough for the whole compact form collapses everything.
        assert_eq!(
            pretty_width(r#"{"a":[[0,1],[1,0]]}"#, 80),
            "{\"a\": [[0, 1], [1, 0]]}"
        );
    }

    #[test]
    fn width_zero_is_always_expand() {
        // Width 0 reproduces the always-expand behavior exactly.
        let input = r#"{"a":[1,2,3]}"#;
        assert_eq!(pretty_width(input, 0), pretty(input));
    }

    #[test]
    fn depth_tightens_budget() {
        // `[1, 2]` is 6 bytes. At depth 1 the budget is 8 - 2 = 6 so it inlines;
        // one level deeper the budget is 8 - 4 = 4 so the same array expands.
        // The filler siblings keep each ancestor too wide to collapse.
        let shallow = pretty_width(r#"[[1,2],0,0,0,0,0]"#, 8);
        assert!(shallow.contains("[1, 2]"), "{shallow}");

        let deep = pretty_width(r#"[[[1,2],0,0,0,0,0]]"#, 8);
        assert!(!deep.contains("[1, 2]"), "{deep}");
    }

    #[test]
    fn width_boundary() {
        // Top-level array `[1, 2, 3]` is exactly 9 bytes.
        assert_eq!(pretty_width(r#"[1,2,3]"#, 9), "[1, 2, 3]");
        assert_eq!(pretty_width(r#"[1,2,3]"#, 8), "[\n  1,\n  2,\n  3\n]");
    }

    #[test]
    fn inline_mixed_object() {
        // The short value inlines, the long one expands.
        assert_eq!(
            pretty_width(r#"{"a":[1,2],"b":[100,200,300,400]}"#, 14),
            "{\n  \"a\": [1, 2],\n  \"b\": [\n    100,\n    200,\n    300,\n    400\n  ]\n}"
        );
    }

    #[test]
    fn inline_depth_caps_nesting() {
        // Width is generous, so only the depth cap matters. The levels are:
        // object (1), the array value (2), the inner arrays (3).
        let input = r#"{"a":[[0,1],[1,0]]}"#;

        // Depth 1: only scalar-only containers inline, so just the inner arrays.
        assert_eq!(
            pretty_full(input, 80, 1),
            "{\n  \"a\": [\n    [0, 1],\n    [1, 0]\n  ]\n}"
        );
        // Depth 2: the array value inlines (its inner arrays are level 2 within
        // it), but the object still expands.
        assert_eq!(pretty_full(input, 80, 2), "{\n  \"a\": [[0, 1], [1, 0]]\n}");
        // Depth 3: the whole document collapses.
        assert_eq!(pretty_full(input, 80, 3), "{\"a\": [[0, 1], [1, 0]]}");
    }

    #[test]
    fn inline_preserves_errors() {
        let cfg = PrettifyConfig {
            indent: "  ",
            max_line_width: 80,
            max_inline_depth: usize::MAX,
            parser: JsonParserConfig::default(),
        };
        assert!(prettify("[1,2,]", &cfg).is_err());
        assert!(prettify("{\"a\":}", &cfg).is_err());
        assert!(prettify("[1,2", &cfg).is_err());
    }
}
