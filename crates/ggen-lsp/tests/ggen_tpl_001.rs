#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! Integration tests proving GGEN-TPL-001 ("unbound projection variable in
//! template") from real project context.
//!
//! Chicago TDD: every test loads a *real* `ggen.toml` project tree from disk via
//! [`ProjectIndex::from_root`] and runs the *real* [`detect_tpl_001`] analyzer.
//! No mocks, no test doubles, no fabricated diagnostics.
//!
//! ## Shared API contract (binding — see PRE_IMPLEMENTATION_INVENTORY)
//!
//! Verified against the landed source on `feat/ggen-tpl-001-living-lsp`:
//!
//! * Agent 1 — `ggen_lsp::project_index::ProjectIndex::from_root(&Path)
//!   -> Result<ProjectIndex, IndexError>`. `ProjectIndex` exposes the public
//!   field `rule_entries: Vec<RuleIndexEntry>`; each `RuleIndexEntry` carries a
//!   public `issues: Vec<String>` for index/source-level problems.
//! * Agent 2 — `ggen_lsp::analyzers::detect_tpl_001(&ProjectIndex)
//!   -> Vec<(PathBuf, Vec<lsp_max::lsp_types::Diagnostic>)>`. Each emitted
//!   `Diagnostic` carries `code == "GGEN-TPL-001"` and
//!   `severity == DiagnosticSeverity::ERROR`.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use ggen_lsp::analyzers::{detect_out_001, detect_tpl_001};
use ggen_lsp::project_index::ProjectIndex;

/// Absolute path to a fixture project root under this crate's `tests/fixtures`.
fn fixture_root(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("ggen_tpl_001")
        .join(name)
}

/// Load a project index, asserting the load itself succeeds (real I/O).
fn load(root: &Path) -> ProjectIndex {
    ProjectIndex::from_root(root)
        .unwrap_or_else(|e| panic!("ProjectIndex::from_root({}) failed: {e:?}", root.display()))
}

use lsp_max_protocol::MaxDiagnostic;

/// True if a `Diagnostic.code` renders as exactly `GGEN-TPL-001`.
fn is_tpl_001(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-TPL-001"
    )
}

/// Count GGEN-TPL-001 diagnostics across the per-file grouping returned by
/// `detect_tpl_001` (`Vec<(PathBuf, Vec<MaxDiagnostic>)>`).
fn count_tpl_001(grouped: &[(PathBuf, Vec<MaxDiagnostic>)]) -> usize {
    grouped
        .iter()
        .flat_map(|(_, diags)| diags.iter())
        .filter(|d| is_tpl_001(&d.lsp))
        .count()
}

/// True if any GGEN-TPL-001 diagnostic is at ERROR severity.
fn has_tpl_001_error(grouped: &[(PathBuf, Vec<MaxDiagnostic>)]) -> bool {
    grouped
        .iter()
        .flat_map(|(_, diags)| diags.iter())
        .any(|d| is_tpl_001(&d.lsp) && d.lsp.severity == Some(DiagnosticSeverity::ERROR))
}

/// 1. A rule whose template consumes only bound projection variables must NOT
///    raise GGEN-TPL-001.
#[test]
fn valid_rule_emits_no_tpl_001() {
    let project = load(&fixture_root("valid_rule"));

    let diags = detect_tpl_001(&project);

    assert_eq!(
        count_tpl_001(&diags),
        0,
        "valid_rule binds `name` and the template consumes only `name`; \
         no GGEN-TPL-001 expected. Got: {diags:?}"
    );
}

/// 2. A template that consumes a projection variable the query never binds
///    (`row["title"]` when the query only binds `?name`) must raise at least one
///    GGEN-TPL-001 diagnostic at ERROR severity.
#[test]
fn unbound_template_var_emits_tpl_001_error() {
    let project = load(&fixture_root("invalid_unbound_template_var"));

    let diags = detect_tpl_001(&project);

    assert!(
        count_tpl_001(&diags) >= 1,
        "template accesses unbound `title`; GGEN-TPL-001 must fire. Got: {diags:?}"
    );
    assert!(
        has_tpl_001_error(&diags),
        "GGEN-TPL-001 must be reported at ERROR severity. Got: {diags:?}"
    );
}

/// 3. The inline-query path (`query = {{ inline = "..." }}`) must resolve its
///    bindings the same way the file path does: a template consuming only the
///    inline-bound `name` raises no GGEN-TPL-001.
#[test]
fn inline_query_valid_emits_no_tpl_001() {
    let project = load(&fixture_root("inline_query_valid"));

    let diags = detect_tpl_001(&project);

    assert_eq!(
        count_tpl_001(&diags),
        0,
        "inline query binds `name`; template consumes only `name`; \
         no GGEN-TPL-001 expected. Got: {diags:?}"
    );
}

/// 4. A rule pointing at a non-existent template file must surface an INDEX-level
///    issue on its `RuleIndexEntry.issues` (not a TPL-001 diagnostic). A missing
///    source is an index/source defect, not an unbound-projection defect, so
///    `detect_tpl_001` must NOT emit GGEN-TPL-001 for it.
#[test]
fn missing_template_file_is_index_issue_not_tpl_001() {
    let project = load(&fixture_root("missing_template_file"));

    // The index must record the missing template as an entry-level issue.
    let any_entry_issue = project
        .rule_entries
        .iter()
        .any(|entry| !entry.issues.is_empty());
    assert!(
        any_entry_issue,
        "a rule references templates/missing.tera which does not exist; \
         the ProjectIndex must record an entry-level `issues` item for it. \
         rule_entries: {:?}",
        project.rule_entries
    );

    // The unbound-projection analyzer must stay silent about a missing source.
    let diags = detect_tpl_001(&project);
    assert_eq!(
        count_tpl_001(&diags),
        0,
        "a missing template file is an index/source diagnostic, NOT \
         unbound_projection; detect_tpl_001 must not emit GGEN-TPL-001. Got: {diags:?}"
    );
}

/// 5. The analysis path is read-only: loading a project and running the analyzer
///    must NEVER materialize the declared `output_file`. Uses a TempDir copy of a
///    fixture so the absence assertion is unambiguous and the repo fixtures stay
///    pristine.
#[test]
fn analysis_never_materializes_output_file() {
    let src = fixture_root("valid_rule");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Sanity: the declared output_file ("out.txt") must not pre-exist in the copy.
    let declared_output = dst.join("out.txt");
    assert!(
        !declared_output.exists(),
        "fixture copy unexpectedly already contains the declared output file"
    );

    let project = load(&dst);
    let _ = detect_tpl_001(&project);

    assert!(
        !declared_output.exists(),
        "ProjectIndex::from_root + detect_tpl_001 must be read-only and must \
         NOT write the declared output_file ({}).",
        declared_output.display()
    );
}

/// GGEN-OUT-001 (unbound projection variable in a templated OUTPUT PATH) is now
/// ACTIVE (GALL-OUT-001). The fixture (`output_path_unbound_next_phase`) declares
/// `output_file = "out/{{ slug }}.txt"` where `slug` is unbound by the query, with
/// a clean template body — so `detect_out_001` raises exactly one GGEN-OUT-001
/// anchored on the `ggen.toml`, and `detect_tpl_001` stays silent.
#[test]
fn output_path_unbound_emits_out_001() {
    let project = load(&fixture_root("output_path_unbound_next_phase"));

    // The OUT detector fires on the unbound output-path variable, anchored on
    // ggen.toml.
    let out = detect_out_001(&project);
    let out_codes: Vec<String> = out
        .iter()
        .flat_map(|(_, diags)| diags.iter())
        .filter_map(|d| match &d.lsp.code {
            Some(NumberOrString::String(s)) => Some(s.clone()),
            _ => None,
        })
        .collect();
    assert!(
        out_codes.iter().any(|c| c == "GGEN-OUT-001"),
        "an unbound OUTPUT-PATH variable must raise GGEN-OUT-001. Got: {out_codes:?}"
    );
    // The OUT diagnostic anchors on the ggen.toml manifest, not the template.
    assert!(
        out.iter().all(|(p, _)| p.ends_with("ggen.toml")),
        "GGEN-OUT-001 must anchor on ggen.toml. Got anchors: {:?}",
        out.iter().map(|(p, _)| p.clone()).collect::<Vec<_>>()
    );
    // The template BODY is clean (consumes only the bound `name`), so the TPL
    // detector stays silent — proving OUT-001 is independent of TPL-001.
    let tpl = detect_tpl_001(&project);
    assert!(
        tpl.is_empty(),
        "the template body is clean; GGEN-TPL-001 must NOT fire. Got: {tpl:?}"
    );
}

/// Recursively copy a directory tree using only std (no external deps beyond
/// tempfile). Real filesystem I/O — Chicago TDD.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if file_type.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
