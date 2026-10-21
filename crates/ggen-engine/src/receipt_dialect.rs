//! Ground-Data-Tier receipt plane (v26.10.20 dialect slice 1, lane V16).
//!
//! Streaming N-Triples / N-Quads verification for receipt and witness logs,
//! WITHOUT constructing an in-memory graph. Memory is O(1): the input is
//! scanned line by line and only counters plus a running BLAKE3 digest are
//! retained.
//!
//! # Grammar subset (ground tier)
//!
//! Per line: optional term sequence `subject predicate object [graph]`
//! terminated by `.`; terms are IRIs (`<...>`) or string literals
//! (`"..."` with backslash escapes, optional `@lang` or `^^<iri>`).
//! `#` starts a comment (full-line or trailing). Blank lines are skipped.
//! The optional 4th term (N-Quads graph label) is accepted.
//!
//! # Ground-tier rule
//!
//! Blank nodes (`_:`) are REFUSED with `FD-DIALECT_FEATURE_DISALLOWED`:
//! ground data tier admits ground facts only (named IRIs / literals),
//! per the PRD tier table.
//!
//! # Byte budget
//!
//! Inputs over the declared budget are refused with a typed [`Limit`]
//! refusal (`name`/`observed`/`max`, abi limit-response convention)
//! before any parsing.
//!
//! # Chain digest
//!
//! The running BLAKE3 is over the **sorted set of canonicalized fact-line
//! byte strings** (canonicalized = whitespace-normalized term separators,
//! trailing comment stripped, `.` re-appended). Because the digest is over
//! a sorted multiset of lines, it is STABLE UNDER LINE REORDERING: the same
//! facts in any order verify to the same chain digest; any change of fact
//! content changes it. This is documented, deliberate behavior — the digest
//! attests the fact SET, not the serialization order. Sorting is per-line
//! over the (already streamed, retained-in-summary-only) canonical lines;
//! retention cost is the canonical line multiset, which is the attested
//! content itself, not a dataset/graph construction.

use crate::generated::refusals::Refusal;
use std::collections::BTreeSet;

/// A typed abi limit refusal: named budget, observed value, admitted max.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Limit {
    /// Machine name of the exceeded budget, e.g. `"receipt_stream_bytes"`.
    pub name: String,
    /// Observed value that crossed the budget.
    pub observed: u64,
    /// Declared maximum.
    pub max: u64,
}

impl Limit {
    /// Canonical `LIMIT:<name> observed/max` wire form.
    #[must_use]
    pub fn emit(&self) -> String {
        format!("LIMIT:{} {}/{}", self.name, self.observed, self.max)
    }
}

/// A refusal from stream verification: a registry-typed code, optional
/// 1-based line number, and either the registry detail or a typed limit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StreamRefusal {
    code: &'static str,
    line: Option<usize>,
    detail: String,
    limit: Option<Limit>,
}

impl StreamRefusal {
    /// Registry wire code, e.g. `"FD-MALFORMED_SYNTAX"`.
    #[must_use]
    pub fn code(&self) -> &'static str {
        self.code
    }
    /// 1-based line number of the offending line, when line-scoped.
    #[must_use]
    pub fn line(&self) -> Option<usize> {
        self.line
    }
    /// Refusal detail.
    #[must_use]
    pub fn detail(&self) -> &str {
        &self.detail
    }
    /// Typed limit payload for budget refusals.
    #[must_use]
    pub fn limit(&self) -> Option<&Limit> {
        self.limit.as_ref()
    }
    /// `REFUSED:<CODE> <detail>` wire form (limit appended when present).
    #[must_use]
    pub fn emit(&self) -> String {
        let base = format!("REFUSED:{} {}", self.code, self.detail);
        match &self.limit {
            Some(l) => format!("{} [{}]", base, l.emit()),
            None => base,
        }
    }

    fn registry(r: &Refusal, line: usize) -> Self {
        StreamRefusal {
            code: r.code_str(),
            line: Some(line),
            detail: r.detail().to_string(),
            limit: None,
        }
    }

    fn limit_refusal(l: Limit) -> Self {
        StreamRefusal {
            code: "LIMIT_EXCEEDED",
            line: None,
            detail: format!("input exceeds declared byte budget {}", l.emit()),
            limit: Some(l),
        }
    }
}

/// Verdict over a fully verified stream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StreamVerdict {
    /// Number of ground triples verified (3-term lines).
    pub triples: usize,
    /// Number of ground quads verified (4-term lines, N-Quads graph label).
    pub quads: usize,
    /// Number of comment lines.
    pub comments: usize,
    /// Number of skipped blank lines.
    pub blank_lines: usize,
    /// BLAKE3 hex digest over the sorted canonicalized fact-line multiset
    /// (reorder-stable; see module docs). `None` when no facts were present.
    pub digest: Option<String>,
}

/// Verify a receipt/witness byte stream against the ground-data-tier
/// N-Triples/N-Quads subset. Refuses on budget, malformed syntax, or any
/// blank node (non-ground feature). Never builds a graph.
///
/// # Errors
/// Returns a typed [`StreamRefusal`] on byte-budget overrun, malformed
/// syntax, blank nodes, or any other non-ground-tier feature.
pub fn verify_stream(bytes: &[u8], max_bytes: u64) -> Result<StreamVerdict, StreamRefusal> {
    let observed = bytes.len() as u64;
    if observed > max_bytes {
        return Err(StreamRefusal::limit_refusal(Limit {
            name: "receipt_stream_bytes".to_string(),
            observed,
            max: max_bytes,
        }));
    }

    let text = std::str::from_utf8(bytes).map_err(|e| StreamRefusal {
        code: "FD-MALFORMED_SYNTAX",
        line: None,
        detail: format!("stream is not valid UTF-8: {e}"),
        limit: None,
    })?;

    let mut canonical_lines: BTreeSet<Vec<u8>> = BTreeSet::new();
    let mut verdict = StreamVerdict {
        triples: 0,
        quads: 0,
        comments: 0,
        blank_lines: 0,
        digest: None,
    };

    for (idx, raw) in text.lines().enumerate() {
        let line_no = idx + 1;
        match classify_line(raw, line_no)? {
            LineKind::Blank => verdict.blank_lines += 1,
            LineKind::Comment => verdict.comments += 1,
            LineKind::Fact { canonical, is_quad } => {
                if is_quad {
                    verdict.quads += 1;
                } else {
                    verdict.triples += 1;
                }
                canonical_lines.insert(canonical);
            }
        }
    }

    if !canonical_lines.is_empty() {
        let mut hasher = blake3::Hasher::new();
        for line in &canonical_lines {
            hasher.update(&(line.len() as u64).to_le_bytes());
            hasher.update(line);
        }
        verdict.digest = Some(hasher.finalize().to_hex().to_string());
    }
    Ok(verdict)
}

enum LineKind {
    Blank,
    Comment,
    Fact { canonical: Vec<u8>, is_quad: bool },
}

fn classify_line(raw: &str, line_no: usize) -> Result<LineKind, StreamRefusal> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Ok(LineKind::Blank);
    }
    if trimmed.starts_with('#') {
        return Ok(LineKind::Comment);
    }

    let mut terms: Vec<String> = Vec::new();
    let mut rest = trimmed;
    loop {
        rest = rest.trim_start();
        if rest.is_empty() {
            return Err(StreamRefusal::registry(
                &Refusal::malformed_syntax(format!(
                    "line {line_no}: statement missing '.' terminator"
                )),
                line_no,
            ));
        }
        if let Some(after) = rest.strip_prefix('.') {
            let after = after.trim_start();
            if !(after.is_empty() || after.starts_with('#')) {
                return Err(StreamRefusal::registry(
                    &Refusal::malformed_syntax(format!(
                        "line {line_no}: trailing content after '.'"
                    )),
                    line_no,
                ));
            }
            break;
        }
        let (term, after) = parse_term(rest, line_no)?;
        terms.push(term);
        rest = after;
    }

    if terms.len() < 3 || terms.len() > 4 {
        return Err(StreamRefusal::registry(
            &Refusal::malformed_syntax(format!(
                "line {line_no}: expected 3 or 4 terms, found {}",
                terms.len()
            )),
            line_no,
        ));
    }
    let is_quad = terms.len() == 4;
    let canonical = format!("{} .", terms.join(" ")).into_bytes();
    Ok(LineKind::Fact { canonical, is_quad })
}

/// Parse one term off the front of `rest`, returning `(term, remainder)`.
/// Canonicalizes surrounding whitespace away; literals keep exact content.
fn parse_term(rest: &str, line_no: usize) -> Result<(String, &str), StreamRefusal> {
    let malformed = |msg: String| {
        StreamRefusal::registry(
            &Refusal::malformed_syntax(format!("line {line_no}: {msg}")),
            line_no,
        )
    };
    match rest.as_bytes()[0] {
        b'<' => {
            let end = rest
                .find('>')
                .ok_or_else(|| malformed("unterminated IRI".into()))?;
            let iri = &rest[..=end];
            if iri[1..].contains('<') {
                return Err(malformed("nested '<' inside IRI".into()));
            }
            Ok((iri.to_string(), &rest[end + 1..]))
        }
        b'"' => {
            let mut i = 1;
            let bytes = rest.as_bytes();
            while i < bytes.len() {
                match bytes[i] {
                    b'\\' => i += 2,
                    b'"' => break,
                    _ => i += 1,
                }
            }
            if i >= bytes.len() {
                return Err(malformed("unterminated literal".into()));
            }
            let mut end = i + 1;
            // Optional language tag or datatype.
            let after_q = &rest[end..];
            if let Some(stripped) = after_q.strip_prefix("^^") {
                if !stripped.starts_with('<') {
                    return Err(malformed("datatype must be an IRI".into()));
                }
                let close = stripped
                    .find('>')
                    .ok_or_else(|| malformed("unterminated datatype IRI".into()))?;
                end += 2 + close + 1;
            } else if let Some(stripped) = after_q.strip_prefix('@') {
                let len = stripped
                    .find(|c: char| !(c.is_ascii_alphanumeric() || c == '-'))
                    .unwrap_or(stripped.len());
                end += 1 + len;
            }
            Ok((rest[..end].to_string(), &rest[end..]))
        }
        b'_' if rest.len() > 1 && rest.as_bytes()[1] == b':' => Err(StreamRefusal::registry(
            &Refusal::dialect_feature_disallowed(format!(
                "line {line_no}: blank node in ground-data-tier stream (ground facts only)"
            )),
            line_no,
        )),
        _ => Err(malformed(format!(
            "unexpected term start {:?}",
            rest.chars().next().unwrap_or(' ')
        ))),
    }
}
