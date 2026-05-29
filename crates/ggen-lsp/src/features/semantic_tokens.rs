//! Real semantic-token highlighting for ggen's four law surfaces.
//!
//! This module produces genuine, delta-encoded [`SemanticToken`]s (LSP relative
//! line/start encoding) for RDF (`.ttl`/`.nt`/`.nq`), SPARQL (`.rq`/`.sparql`),
//! TOML (`ggen.toml`/`.toml`), and Tera (`.tera`). No analyzer state is needed —
//! the tokenizers operate directly on the document text, line by line, so the
//! server can call [`semantic_tokens`] with just a [`FileType`] and the content.
//!
//! ## Legend index contract
//!
//! The numeric `token_type` of each emitted token is an index into the legend
//! declared in `server.rs`'s `semantic_tokens_provider`. The order is fixed and
//! **must** match that legend exactly:
//!
//! | index | token type  |
//! |-------|-------------|
//! | 0     | NAMESPACE   |
//! | 1     | CLASS       |
//! | 2     | PROPERTY    |
//! | 3     | VARIABLE    |
//! | 4     | KEYWORD     |
//! | 5     | STRING      |
//! | 6     | NUMBER      |
//! | 7     | COMMENT     |
//! | 8     | FUNCTION    |
//!
//! The tokenization knowledge (SPARQL keywords, Tera keywords/filters, TOML
//! section/key shapes, RDF prefix/literal shapes) is reused from the per-grammar
//! analyzers in `crate::analyzers`.

use tower_lsp::lsp_types::{SemanticToken, SemanticTokens};

use crate::state::FileType;

/// Legend indices. The order MUST match `server.rs`'s `SemanticTokensLegend`.
pub const TT_NAMESPACE: u32 = 0;
pub const TT_CLASS: u32 = 1;
pub const TT_PROPERTY: u32 = 2;
pub const TT_VARIABLE: u32 = 3;
pub const TT_KEYWORD: u32 = 4;
pub const TT_STRING: u32 = 5;
pub const TT_NUMBER: u32 = 6;
pub const TT_COMMENT: u32 = 7;
pub const TT_FUNCTION: u32 = 8;

/// SPARQL keywords highlighted as KEYWORD (mirrors `sparql_analyzer::KEYWORDS`,
/// expanded to the single-token aggregate keywords actually scanned per word).
const SPARQL_KEYWORDS: &[&str] = &[
    "SELECT",
    "CONSTRUCT",
    "ASK",
    "DESCRIBE",
    "WHERE",
    "PREFIX",
    "BASE",
    "ORDER",
    "BY",
    "GROUP",
    "LIMIT",
    "OFFSET",
    "FILTER",
    "OPTIONAL",
    "UNION",
    "DISTINCT",
    "BIND",
    "VALUES",
    "MINUS",
    "HAVING",
    "SERVICE",
    "FROM",
    "AS",
    "REDUCED",
    "GRAPH",
];

/// Tera control keywords (mirrors `tera_analyzer::KEYWORDS`).
const TERA_KEYWORDS: &[&str] = &[
    "if",
    "elif",
    "else",
    "endif",
    "for",
    "endfor",
    "block",
    "endblock",
    "set",
    "include",
    "extends",
    "macro",
    "endmacro",
    "filter",
    "endfilter",
    "raw",
    "endraw",
    "in",
    "and",
    "or",
    "not",
];

/// An absolute (line, start, length, type) token before delta encoding.
#[derive(Debug, Clone, Copy)]
struct AbsToken {
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
}

/// Produce real, delta-encoded semantic tokens for the given law surface.
///
/// Returns `None` only for [`FileType::Unknown`] or when no tokens were found
/// (so the editor falls back gracefully).
#[must_use]
pub fn semantic_tokens(file_type: FileType, content: &str) -> Option<SemanticTokens> {
    let mut abs = match file_type {
        FileType::Rdf => tokenize_rdf(content),
        FileType::Sparql => tokenize_sparql(content),
        FileType::Toml => tokenize_toml(content),
        FileType::Tera => tokenize_tera(content),
        FileType::Unknown => return None,
    };
    if abs.is_empty() {
        return None;
    }
    // Delta encoding requires tokens in document order; tokenizers may push out
    // of column order (e.g. a trailing comment before earlier words on the same
    // line). Sort by (line, start) and drop any positional duplicate.
    abs.sort_by_key(|t| (t.line, t.start));
    abs.dedup_by_key(|t| (t.line, t.start));
    Some(SemanticTokens {
        result_id: None,
        data: delta_encode(&abs),
    })
}

/// Convert absolute tokens (already in document order) into LSP delta encoding:
/// each token's line/start is relative to the previous token.
fn delta_encode(tokens: &[AbsToken]) -> Vec<SemanticToken> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for t in tokens {
        let delta_line = t.line - prev_line;
        let delta_start = if delta_line == 0 {
            t.start - prev_start
        } else {
            t.start
        };
        out.push(SemanticToken {
            delta_line,
            delta_start,
            length: t.length,
            token_type: t.token_type,
            token_modifiers_bitset: 0,
        });
        prev_line = t.line;
        prev_start = t.start;
    }
    out
}

/// Push a token, guarding against zero-length and out-of-range casts.
fn push(out: &mut Vec<AbsToken>, line: usize, start: usize, length: usize, token_type: u32) {
    if length == 0 {
        return;
    }
    out.push(AbsToken {
        line: u32::try_from(line).unwrap_or(u32::MAX),
        start: u32::try_from(start).unwrap_or(u32::MAX),
        length: u32::try_from(length).unwrap_or(u32::MAX),
        token_type,
    });
}

// ---------------------------------------------------------------------------
// RDF (.ttl / .nt / .nq)
// ---------------------------------------------------------------------------

/// Tokenize Turtle-family RDF: comments, `@prefix`/`PREFIX` declarations →
/// NAMESPACE, `a`/keyword tokens → KEYWORD, string literals → STRING, and
/// prefixed names (`ex:Thing`) → CLASS/PROPERTY by capitalization heuristic.
fn tokenize_rdf(content: &str) -> Vec<AbsToken> {
    let mut out = Vec::new();
    for (line_idx, raw) in content.lines().enumerate() {
        // Comment to end of line.
        if let Some(hash) = find_line_comment(raw, '#') {
            push(&mut out, line_idx, hash, raw.len() - hash, TT_COMMENT);
        }
        let code = comment_stripped(raw, '#');

        // String literals first (so prefixed-name scan skips inside-quote text).
        let string_spans = scan_string_spans(code);
        for (s, e) in &string_spans {
            push(&mut out, line_idx, *s, e - s, TT_STRING);
        }

        // Word-level scan for keywords / prefix decls / prefixed names.
        for (start, word) in words(code) {
            if in_any_span(start, &string_spans) {
                continue;
            }
            let lower = word.to_ascii_lowercase();
            let upper = word.to_ascii_uppercase();
            if word == "@prefix" || word == "@base" || upper == "PREFIX" || upper == "BASE" {
                push(&mut out, line_idx, start, word.len(), TT_NAMESPACE);
            } else if word == "a" {
                // RDF `a` shorthand for rdf:type.
                push(&mut out, line_idx, start, 1, TT_KEYWORD);
            } else if let Some(colon) = word.find(':') {
                // Prefixed name: `prefix:Local`. The declaration label (`ex:` in
                // `@prefix ex:`) and the prefix part read as NAMESPACE; a local
                // part starting uppercase → CLASS, lowercase → PROPERTY.
                let local = &word[colon + 1..];
                if local.is_empty() {
                    push(&mut out, line_idx, start, word.len(), TT_NAMESPACE);
                } else if local.chars().next().is_some_and(char::is_uppercase) {
                    push(&mut out, line_idx, start, word.len(), TT_CLASS);
                } else {
                    push(&mut out, line_idx, start, word.len(), TT_PROPERTY);
                }
            } else if lower == "true" || lower == "false" {
                push(&mut out, line_idx, start, word.len(), TT_KEYWORD);
            } else if is_numeric_literal(word) {
                push(&mut out, line_idx, start, word.len(), TT_NUMBER);
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// SPARQL (.rq / .sparql)
// ---------------------------------------------------------------------------

/// Tokenize SPARQL: `#` comments, query keywords → KEYWORD, `?`/`$` variables →
/// VARIABLE, `PREFIX`/`BASE` → NAMESPACE, string literals → STRING, numerics →
/// NUMBER.
fn tokenize_sparql(content: &str) -> Vec<AbsToken> {
    let mut out = Vec::new();
    for (line_idx, raw) in content.lines().enumerate() {
        if let Some(hash) = find_line_comment(raw, '#') {
            push(&mut out, line_idx, hash, raw.len() - hash, TT_COMMENT);
        }
        let code = comment_stripped(raw, '#');

        let string_spans = scan_string_spans(code);
        for (s, e) in &string_spans {
            push(&mut out, line_idx, *s, e - s, TT_STRING);
        }

        for (start, word) in words_with(code, |c| {
            c.is_alphanumeric() || c == '_' || c == '?' || c == '$'
        }) {
            if in_any_span(start, &string_spans) {
                continue;
            }
            if (word.starts_with('?') || word.starts_with('$')) && word.len() > 1 {
                push(&mut out, line_idx, start, word.len(), TT_VARIABLE);
                continue;
            }
            let upper = word.to_ascii_uppercase();
            if upper == "PREFIX" || upper == "BASE" {
                push(&mut out, line_idx, start, word.len(), TT_NAMESPACE);
            } else if SPARQL_KEYWORDS.contains(&upper.as_str()) {
                push(&mut out, line_idx, start, word.len(), TT_KEYWORD);
            } else if is_numeric_literal(word) {
                push(&mut out, line_idx, start, word.len(), TT_NUMBER);
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// TOML (ggen.toml / .toml)
// ---------------------------------------------------------------------------

/// Tokenize TOML: `#` comments, `[section]`/`[[array]]` headers → NAMESPACE,
/// `key =` left-hand sides → PROPERTY, quoted string values → STRING, bare
/// numbers → NUMBER, `true`/`false` → KEYWORD.
fn tokenize_toml(content: &str) -> Vec<AbsToken> {
    let mut out = Vec::new();
    for (line_idx, raw) in content.lines().enumerate() {
        if let Some(hash) = find_line_comment(raw, '#') {
            push(&mut out, line_idx, hash, raw.len() - hash, TT_COMMENT);
        }
        let code = comment_stripped(raw, '#');
        let trimmed = code.trim_start();
        let indent = code.len() - trimmed.len();

        // Section header: [name] or [[name]].
        let t = trimmed.trim_end();
        if t.starts_with('[') && t.ends_with(']') {
            push(&mut out, line_idx, indent, t.len(), TT_NAMESPACE);
            continue;
        }

        // key = value
        if let Some(eq) = code.find('=') {
            let key_raw = &code[..eq];
            let key = key_raw.trim();
            if !key.is_empty() {
                // Column of the key's first non-whitespace char.
                let key_col = code.len() - code.trim_start().len();
                push(&mut out, line_idx, key_col, key.len(), TT_PROPERTY);
            }
            let _ = indent;
            // Value side.
            let value_off = eq + 1;
            let value_str = &code[value_off..];
            let string_spans = scan_string_spans(value_str);
            for (s, e) in &string_spans {
                push(&mut out, line_idx, value_off + s, e - s, TT_STRING);
            }
            for (start, word) in words_with(value_str, |c| {
                c.is_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '+'
            }) {
                let abs_start = value_off + start;
                if in_any_span(start, &string_spans) {
                    continue;
                }
                let lower = word.to_ascii_lowercase();
                if lower == "true" || lower == "false" {
                    push(&mut out, line_idx, abs_start, word.len(), TT_KEYWORD);
                } else if is_numeric_literal(word) {
                    push(&mut out, line_idx, abs_start, word.len(), TT_NUMBER);
                }
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Tera (.tera)
// ---------------------------------------------------------------------------

/// Tokenize Tera: `{{ ... }}` expression and `{% ... %}` statement delimiters →
/// KEYWORD, control keywords → KEYWORD, and pipe-applied filters (`| upper`) →
/// FUNCTION. Tokenizing scans only inside delimiter regions so literal text is
/// left unstyled.
fn tokenize_tera(content: &str) -> Vec<AbsToken> {
    let mut out = Vec::new();
    for (line_idx, raw) in content.lines().enumerate() {
        for region in tera_regions(raw) {
            let (open, close) = region.delims;
            // Opening delimiter.
            push(&mut out, line_idx, region.start, open.len(), TT_KEYWORD);
            // Closing delimiter.
            let close_start = region.end - close.len();
            push(&mut out, line_idx, close_start, close.len(), TT_KEYWORD);

            let inner = &raw[region.start + open.len()..close_start];
            let inner_off = region.start + open.len();
            for (start, word) in words_with(inner, |c| c.is_alphanumeric() || c == '_') {
                let abs = inner_off + start;
                // A `|` immediately before this word marks it as a filter (FUNCTION).
                let is_filter = inner[..start].trim_end().ends_with('|');
                if is_filter {
                    push(&mut out, line_idx, abs, word.len(), TT_FUNCTION);
                } else if TERA_KEYWORDS.contains(&word) {
                    push(&mut out, line_idx, abs, word.len(), TT_KEYWORD);
                }
            }
        }
    }
    out
}

struct TeraRegion {
    start: usize,
    end: usize,
    delims: (&'static str, &'static str),
}

/// Find `{{ }}` and `{% %}` regions on a single line (no multi-line support;
/// editors re-request per change so single-line coverage is the useful 80%).
fn tera_regions(line: &str) -> Vec<TeraRegion> {
    let mut regions = Vec::new();
    let bytes = line.as_bytes();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b'{' && (bytes[i + 1] == b'{' || bytes[i + 1] == b'%') {
            let (open, close): (&str, &str) = if bytes[i + 1] == b'{' {
                ("{{", "}}")
            } else {
                ("{%", "%}")
            };
            if let Some(rel) = line[i + 2..].find(close) {
                let end = i + 2 + rel + close.len();
                regions.push(TeraRegion {
                    start: i,
                    end,
                    delims: (open, close),
                });
                i = end;
                continue;
            }
        }
        i += 1;
    }
    regions
}

// ---------------------------------------------------------------------------
// Shared scanners
// ---------------------------------------------------------------------------

/// Byte offset of a line-comment marker `c` that is not inside a string literal.
fn find_line_comment(line: &str, c: char) -> Option<usize> {
    let mut in_str = false;
    let mut quote = '"';
    for (i, ch) in line.char_indices() {
        if in_str {
            if ch == quote {
                in_str = false;
            }
        } else if ch == '"' || ch == '\'' {
            in_str = true;
            quote = ch;
        } else if ch == c {
            return Some(i);
        }
    }
    None
}

/// The portion of `line` before any line-comment marker `c`.
fn comment_stripped(line: &str, c: char) -> &str {
    match find_line_comment(line, c) {
        Some(i) => &line[..i],
        None => line,
    }
}

/// Spans (start, end byte) of double/single-quoted string literals in `s`.
fn scan_string_spans(s: &str) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let ch = bytes[i];
        if ch == b'"' || ch == b'\'' {
            let quote = ch;
            let start = i;
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' {
                    i += 2;
                    continue;
                }
                if bytes[i] == quote {
                    i += 1;
                    break;
                }
                i += 1;
            }
            spans.push((start, i.min(s.len())));
        } else {
            i += 1;
        }
    }
    spans
}

/// True if byte `pos` falls within any (start, end) span.
fn in_any_span(pos: usize, spans: &[(usize, usize)]) -> bool {
    spans.iter().any(|(s, e)| pos >= *s && pos < *e)
}

/// Iterate (byte_start, word) over whitespace/punctuation-delimited words, where
/// a word char is alphanumeric, `_`, `@`, or `:` (covers `@prefix`, `ex:Thing`).
fn words(s: &str) -> Vec<(usize, &str)> {
    words_with(s, |c| {
        c.is_alphanumeric() || c == '_' || c == '@' || c == ':' || c == '-' || c == '.'
    })
}

/// Generic word splitter with a custom "is word char" predicate.
fn words_with(s: &str, is_word: impl Fn(char) -> bool) -> Vec<(usize, &str)> {
    let mut out = Vec::new();
    let mut start: Option<usize> = None;
    for (i, ch) in s.char_indices() {
        if is_word(ch) {
            if start.is_none() {
                start = Some(i);
            }
        } else if let Some(st) = start.take() {
            out.push((st, &s[st..i]));
        }
    }
    if let Some(st) = start {
        out.push((st, &s[st..]));
    }
    out
}

/// True if `word` parses as an integer or decimal numeric literal.
fn is_numeric_literal(word: &str) -> bool {
    let w = word.trim();
    if w.is_empty() {
        return false;
    }
    if w.parse::<i64>().is_ok() || w.parse::<f64>().is_ok() {
        // Reject lone signs / dots that f64 would also reject — parse already does.
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Decode delta-encoded tokens back to absolute (line, start, len, type) for
    /// readable assertions — exercises the encoding round-trip too.
    fn decode(tokens: &SemanticTokens) -> Vec<(u32, u32, u32, u32)> {
        let mut out = Vec::new();
        let mut line = 0u32;
        let mut start = 0u32;
        for t in &tokens.data {
            line += t.delta_line;
            if t.delta_line == 0 {
                start += t.delta_start;
            } else {
                start = t.delta_start;
            }
            out.push((line, start, t.length, t.token_type));
        }
        out
    }

    fn types(tokens: &SemanticTokens) -> Vec<u32> {
        tokens.data.iter().map(|t| t.token_type).collect()
    }

    #[test]
    fn rdf_real_content_produces_namespace_class_property_string_comment() {
        let ttl = "@prefix ex: <http://example.org/> .\n\
                   # a comment line\n\
                   ex:Widget a ex:Thing ;\n\
                     ex:label \"hello world\" .\n";
        let tokens = semantic_tokens(FileType::Rdf, ttl).expect("rdf tokens");
        assert!(!tokens.data.is_empty(), "expected non-empty RDF tokens");
        let ty = types(&tokens);
        assert!(ty.contains(&TT_NAMESPACE), "expected @prefix → NAMESPACE");
        assert!(
            ty.contains(&TT_CLASS),
            "expected ex:Widget/ex:Thing → CLASS"
        );
        assert!(ty.contains(&TT_PROPERTY), "expected ex:label → PROPERTY");
        assert!(ty.contains(&TT_KEYWORD), "expected `a` → KEYWORD");
        assert!(ty.contains(&TT_STRING), "expected \"hello world\" → STRING");
        assert!(ty.contains(&TT_COMMENT), "expected # comment → COMMENT");

        // The comment on line 1 starts at column 0 and spans the whole line.
        let decoded = decode(&tokens);
        assert!(
            decoded
                .iter()
                .any(|(l, c, _, t)| *l == 1 && *c == 0 && *t == TT_COMMENT),
            "comment token should anchor line 1 col 0: {decoded:?}"
        );
    }

    #[test]
    fn sparql_real_content_produces_keyword_variable_namespace() {
        let rq = "PREFIX ex: <http://example.org/>\n\
                  # find widgets\n\
                  SELECT ?s ?label WHERE {\n\
                    ?s a ex:Widget ;\n\
                       ex:label ?label .\n\
                    FILTER(?label != \"skip\")\n\
                  }\n";
        let tokens = semantic_tokens(FileType::Sparql, rq).expect("sparql tokens");
        assert!(!tokens.data.is_empty());
        let ty = types(&tokens);
        assert!(ty.contains(&TT_NAMESPACE), "PREFIX → NAMESPACE");
        assert!(ty.contains(&TT_KEYWORD), "SELECT/WHERE/FILTER → KEYWORD");
        assert!(ty.contains(&TT_VARIABLE), "?s/?label → VARIABLE");
        assert!(ty.contains(&TT_STRING), "\"skip\" → STRING");
        assert!(ty.contains(&TT_COMMENT), "# comment → COMMENT");

        // Count the variable occurrences (?s twice, ?label three times).
        let var_count = ty.iter().filter(|&&t| t == TT_VARIABLE).count();
        assert!(
            var_count >= 4,
            "expected several variable tokens, got {var_count}"
        );
    }

    #[test]
    fn toml_real_content_produces_section_property_string_number_comment() {
        let toml = "# ggen config\n\
                    [project]\n\
                    name = \"demo\"\n\
                    version = 3\n\
                    ratio = 1.5\n\
                    strict = true\n";
        let tokens = semantic_tokens(FileType::Toml, toml).expect("toml tokens");
        assert!(!tokens.data.is_empty());
        let ty = types(&tokens);
        assert!(ty.contains(&TT_NAMESPACE), "[project] → NAMESPACE");
        assert!(ty.contains(&TT_PROPERTY), "name/version → PROPERTY");
        assert!(ty.contains(&TT_STRING), "\"demo\" → STRING");
        assert!(ty.contains(&TT_NUMBER), "3 / 1.5 → NUMBER");
        assert!(ty.contains(&TT_KEYWORD), "true → KEYWORD");
        assert!(ty.contains(&TT_COMMENT), "# comment → COMMENT");

        // The section header token must be on line 1 at column 0.
        let decoded = decode(&tokens);
        assert!(
            decoded
                .iter()
                .any(|(l, c, len, t)| *l == 1 && *c == 0 && *len == 9 && *t == TT_NAMESPACE),
            "[project] should be a 9-char NAMESPACE token at 1:0: {decoded:?}"
        );
    }

    #[test]
    fn tera_real_content_produces_keyword_and_function_filter() {
        let tera = "{% for item in items %}\n\
                    {{ item.name | upper }}\n\
                    {% endfor %}\n";
        let tokens = semantic_tokens(FileType::Tera, tera).expect("tera tokens");
        assert!(!tokens.data.is_empty(), "expected non-empty Tera tokens");
        let ty = types(&tokens);
        assert!(ty.contains(&TT_KEYWORD), "delimiters/for/endfor → KEYWORD");
        assert!(ty.contains(&TT_FUNCTION), "| upper → FUNCTION filter");
    }

    #[test]
    fn unknown_file_type_yields_none() {
        assert!(semantic_tokens(FileType::Unknown, "anything").is_none());
    }

    #[test]
    fn empty_content_yields_none() {
        assert!(semantic_tokens(FileType::Rdf, "").is_none());
        assert!(semantic_tokens(FileType::Toml, "\n\n").is_none());
    }

    #[test]
    fn delta_encoding_is_monotonic_and_relative() {
        // Two tokens on the same line then one on a later line: verify deltas.
        let toml = "[a]\nx = 1\n";
        let tokens = semantic_tokens(FileType::Toml, toml).expect("tokens");
        // First token's delta_line is absolute-from-0.
        assert_eq!(tokens.data[0].delta_line, 0, "first token starts at line 0");
        // Decoding must reproduce non-decreasing line numbers.
        let decoded = decode(&tokens);
        let mut last_line = 0;
        for (l, _, _, _) in decoded {
            assert!(l >= last_line, "lines must be non-decreasing");
            last_line = l;
        }
    }
}
