//! GGEN-HARNESS-001 — harness-mismatch detector (pure function).
//!
//! The *harness* of a crate is its declared proof/test topology: the
//! `[[test]]` / `[[bench]]` tables in `Cargo.toml` that carry an explicit
//! `path = "..."`. The DECLARED topology must be consistent with the ACTUAL
//! proof files on disk. A mismatch — a declared target whose `path` does not
//! resolve to a file — is GGEN-HARNESS-001 (`harness_mismatch`, ERROR,
//! release_blocking). cf. real commit `47656dbf` ("replace non-existent
//! benchmark targets").
//!
//! This module is the pure analogue of [`crate::analyzers::tera_analyzer`]'s
//! `unbound_projection_diagnostics`: it reads NO files. The I/O — parsing the
//! manifest tables and walking the proof dirs — is done by
//! [`crate::harness_index::HarnessIndex`], which hands this function the already
//! resolved declared targets and the set of files that actually exist. Keeping
//! the predicate pure makes it trivially testable and free of any child-LSP /
//! subprocess dependency.
//!
//! ## Source-law repair only
//!
//! The diagnostic anchors on the offending manifest's `path = "..."` line — the
//! producer/declaration surface. The repair is ALWAYS source law: fix the
//! `Cargo.toml` declaration (align the `path`, or remove the dead target) OR
//! create/rename the proof file the declaration points at. The route NEVER
//! fabricates a passing proof and NEVER points a declaration at an emitted /
//! generated output. (The route itself lives in `route::registry`; this module
//! only produces the diagnostic.)

use std::collections::BTreeSet;
use std::path::PathBuf;

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

/// Stable diagnostic code for a harness mismatch.
pub const GGEN_HARNESS_001: &str = "GGEN-HARNESS-001";

/// A declared proof/test/bench target from a harness manifest (`Cargo.toml`).
///
/// Produced by [`crate::harness_index::HarnessIndex`] from a `[[test]]` /
/// `[[bench]]` table that carries an explicit `path`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredTarget {
    /// The target `name` from the manifest table.
    pub name: String,
    /// Resolved absolute path the declaration points at (`<manifest_dir>/path`).
    pub path: PathBuf,
    /// The `Cargo.toml` the table lives in — the squiggle/declaration surface.
    pub manifest: PathBuf,
    /// 0-based line of the `path = "..."` entry (best-effort; 0 if not located).
    pub line: u32,
}

/// PURE detector: for each declared target whose resolved `path` is NOT in
/// `existing_files`, emit a GGEN-HARNESS-001 ERROR anchored on its declaration
/// line.
///
/// Reads NO files — paths are pre-resolved by [`crate::harness_index::HarnessIndex`]
/// (mirrors `detect_tpl_001`'s no-I/O contract). The returned diagnostics all
/// anchor on the producer surface (the `Cargo.toml` declaration), never on the
/// consumer (the missing proof file) and never on an emitted output.
#[must_use]
pub fn harness_mismatch_diagnostics(
    targets: &[DeclaredTarget], existing_files: &BTreeSet<PathBuf>,
) -> Vec<Diagnostic> {
    targets
        .iter()
        .filter(|t| !existing_files.contains(&t.path))
        .map(|t| Diagnostic {
            range: Range {
                start: Position {
                    line: t.line,
                    character: 0,
                },
                end: Position {
                    line: t.line,
                    character: 0,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String(GGEN_HARNESS_001.to_string())),
            source: Some("ggen-lsp".to_string()),
            message: format!(
                "harness mismatch: declared target `{}` points at `{}` which does not exist \
                 on disk. Fix the Cargo.toml [[test]]/[[bench]] path or create/rename the proof \
                 file (source law). Never fabricate a passing proof.",
                t.name,
                t.path.display()
            ),
            ..Default::default()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn target(name: &str, path: &str, line: u32) -> DeclaredTarget {
        DeclaredTarget {
            name: name.to_string(),
            path: PathBuf::from(path),
            manifest: PathBuf::from("/proj/Cargo.toml"),
            line,
        }
    }

    fn existing(paths: &[&str]) -> BTreeSet<PathBuf> {
        paths.iter().map(PathBuf::from).collect()
    }

    #[test]
    fn declared_but_missing_raises_one_error() {
        let targets = vec![target("proof", "/proj/tests/proof/nonexistent.rs", 7)];
        let diags = harness_mismatch_diagnostics(&targets, &existing(&[]));

        assert_eq!(diags.len(), 1);
        let d = &diags[0];
        assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(
            d.code,
            Some(NumberOrString::String(GGEN_HARNESS_001.to_string()))
        );
        assert_eq!(d.range.start.line, 7, "anchors on the declaration line");
        assert!(
            d.message.contains("proof") && d.message.contains("nonexistent.rs"),
            "message names the target and the missing path: {}",
            d.message
        );
    }

    #[test]
    fn declared_and_present_raises_nothing() {
        let targets = vec![target("proof", "/proj/tests/proof/run.rs", 7)];
        let diags =
            harness_mismatch_diagnostics(&targets, &existing(&["/proj/tests/proof/run.rs"]));
        assert!(
            diags.is_empty(),
            "a declared target whose file exists is lawful: {diags:?}"
        );
    }

    #[test]
    fn multiple_mismatches_raise_n_errors() {
        let targets = vec![
            target("a", "/proj/tests/a.rs", 1),
            target("b", "/proj/tests/b.rs", 2),
            target("ok", "/proj/tests/ok.rs", 3),
        ];
        let diags = harness_mismatch_diagnostics(&targets, &existing(&["/proj/tests/ok.rs"]));
        assert_eq!(diags.len(), 2, "only the two missing targets raise");
    }

    #[test]
    fn empty_inputs_raise_nothing() {
        assert!(harness_mismatch_diagnostics(&[], &existing(&[])).is_empty());
    }

    #[test]
    fn message_never_advises_fabricating_a_proof() {
        let targets = vec![target("proof", "/proj/tests/x.rs", 0)];
        let diags = harness_mismatch_diagnostics(&targets, &existing(&[]));
        let msg = diags[0].message.to_lowercase();
        assert!(
            msg.contains("never fabricate"),
            "the diagnostic must reinforce source-law repair, not fabrication"
        );
        // It must point at the source-law surfaces, never an emitted output.
        for forbidden in ["out/", "output/", "dist/", "gen/"] {
            assert!(
                !msg.contains(forbidden),
                "message names emitted output {forbidden:?}"
            );
        }
    }
}
