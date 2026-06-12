//! Helpers for building LSP diagnostics from ggen's law-surface checks.
//!
//! All diagnostics carry `source = "ggen-lsp"` and, where the violation maps to
//! one of ggen's runtime error codes (E00XX), that code is attached so the editor
//! shows the *same* law the pipeline enforces at `ggen sync` time.

use lsp_max::lsp_types_max::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

/// Build a diagnostic spanning a single line/column range.
///
/// `line`/`col` pairs are 0-based (LSP convention). Callers converting from
/// oxrdfio's 1-based positions must subtract 1 first (see [`from_oxrdfio`]).
#[must_use]
pub fn at(
    start_line: u32, start_col: u32, end_line: u32, end_col: u32, severity: DiagnosticSeverity,
    code: Option<&str>, message: impl Into<String>,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: start_line,
                character: start_col,
            },
            end: Position {
                line: end_line,
                character: end_col,
            },
        },
        severity: Some(severity),
        code: code.map(|c| NumberOrString::String(c.to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: message.into(),
        related_information: None,
        tags: None,
        data: Some(serde_json::json!({"source_id": "ggen_lsp_observer"})),
    }
}

/// Build a `MaxDiagnostic` (LSP + ggen metadata) spanning a single line/column range.
#[must_use]
pub fn max(
    start_line: u32, start_col: u32, end_line: u32, end_col: u32, severity: DiagnosticSeverity,
    code: Option<&str>, message: impl Into<String>, axis: LawAxis,
) -> MaxDiagnostic {
    let msg = message.into();
    let diag = at(start_line, start_col, end_line, end_col, severity, code, &msg);
    let code_str = code.unwrap_or("GGEN-UNKNOWN").to_string();

    let id = {
        let mut h = blake3::Hasher::new();
        h.update(msg.as_bytes());
        format!("{}-{:.8}", code_str, h.finalize().to_hex())
    };

    MaxDiagnostic {
        lsp: diag,
        diagnostic_id: id,
        law_id: code_str,
        law_axis: axis,
        violated_invariant: msg,
        ..MaxDiagnostic::default()
    }
}

/// Convert oxrdfio's 1-based `(line, column)` positions to a 0-based diagnostic.
#[must_use]
pub fn from_oxrdfio(
    line: u64, column: u64, end_line: u64, end_column: u64, severity: DiagnosticSeverity,
    code: Option<&str>, message: impl Into<String>,
) -> Diagnostic {
    let to0 = |v: u64| u32::try_from(v.saturating_sub(1)).unwrap_or(0);
    at(
        to0(line),
        to0(column),
        to0(end_line),
        to0(end_column),
        severity,
        code,
        message,
    )
}

/// Convert oxrdfio's 1-based `(line, column)` positions to a 0-based `MaxDiagnostic`.
#[must_use]
pub fn max_from_oxrdfio(
    line: u64, column: u64, end_line: u64, end_column: u64, severity: DiagnosticSeverity,
    code: Option<&str>, message: impl Into<String>, axis: LawAxis,
) -> MaxDiagnostic {
    let to0 = |v: u64| u32::try_from(v.saturating_sub(1)).unwrap_or(0);
    max(
        to0(line),
        to0(column),
        to0(end_line),
        to0(end_column),
        severity,
        code,
        message,
        axis,
    )
}

/// A whole-line diagnostic (columns 0..=`u32::MAX`), for checks without a precise
/// column (e.g. SPARQL parse errors). `line` is 0-based.
#[must_use]
pub fn whole_line(
    line: u32, severity: DiagnosticSeverity, code: Option<&str>, message: impl Into<String>,
) -> Diagnostic {
    at(line, 0, line, u32::MAX, severity, code, message)
}

/// A whole-line `MaxDiagnostic`. `line` is 0-based.
#[must_use]
pub fn max_whole_line(
    line: u32, severity: DiagnosticSeverity, code: Option<&str>, message: impl Into<String>,
    axis: LawAxis,
) -> MaxDiagnostic {
    max(line, 0, line, u32::MAX, severity, code, message, axis)
}
