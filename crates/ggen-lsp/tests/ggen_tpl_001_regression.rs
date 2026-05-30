//! GALL-CHECKPOINT-001B — regression / no-scope-creep boundary tests for
//! GGEN-TPL-001 (Agent 4 of 001B).
//!
//! These tests lock the checkpoint boundaries so a later patch cannot silently
//! expand scope (introduce a child-LSP dependency, write emitted output during
//! analysis, or route a GGEN-TPL-001 repair at an emitted-output path), and so the
//! ACTIVE peer species (HARNESS-001, OUT-001) never leak into the TPL surface.
//!
//! GALL-CHECKPOINT-002 update: GGEN-HARNESS-001 is now ACTIVE (its detector
//! flipped on). The barrier below asserts the activation AND that a TPL fixture
//! still raises ZERO GGEN-HARNESS-001 (no cross-species leak in either
//! direction — the symmetric HARNESS→TPL barrier lives in
//! `ggen_harness_001_living_loop.rs`).
//!
//! GALL-OUT-001 update: GGEN-OUT-001 (`unbound_output_path`) is now ACTIVE — the
//! dual of TPL-001 on the `ggen.toml`/SPARQL surfaces. The barriers below assert
//! the activation AND that (a) the TPL `detect_tpl_001` detector NEVER emits
//! GGEN-OUT-001 (different consumer surface), and (b) running `detect_out_001`
//! over the TPL invalid fixture (whose `output_file` is the static `out.txt`)
//! raises ZERO GGEN-OUT-001 (no cross-species leak in either direction — the
//! symmetric OUT→TPL barrier lives in `ggen_out_001_living_loop.rs`).
//!
//! Chicago TDD: every test that exercises detection loads a *real* `ggen.toml`
//! project tree from disk via [`ProjectIndex::from_root`] and runs the *real*
//! [`detect_tpl_001`] analyzer. No mocks, no test doubles, no fabricated
//! diagnostics. The species/route assertions read the *real* compiled-in
//! registries.
//!
//! ## What each boundary locks
//!
//! | Test | Boundary locked |
//! |------|-----------------|
//! | `out_001_is_active_in_species_registry` | GGEN-OUT-001 present with `detector_active == true` (GALL-OUT-001) |
//! | `tpl_detector_never_emits_out_001` | `detect_tpl_001` over an unbound OUTPUT-PATH var emits no GGEN-OUT-001 (consumer-surface separation) |
//! | `out_detector_silent_on_static_output_path_of_tpl_fixture` | `detect_out_001` over the TPL invalid fixture (static `out.txt`) emits no GGEN-OUT-001 |
//! | `harness_001_is_active` | GGEN-HARNESS-001 present with `detector_active == true` (CHECKPOINT-002) |
//! | `detect_tpl_001_runs_without_any_child_lsp` | detection is pure Rust — no external binary / child LSP crept in |
//! | `valid_fixture_stays_clean` | valid fixture → no GGEN-TPL-001 |
//! | `invalid_fixture_emits_only_tpl_001` | invalid fixture → only GGEN-TPL-001, no other GGEN species code |
//! | `analysis_writes_no_emitted_output_files` | index + detect + headless check materialize no `output_file` |
//! | `tpl_001_route_targets_only_source_law` | GGEN-TPL-001 route steps target source law, never an emitted-output path |

use std::path::{Path, PathBuf};

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

use ggen_lsp::analyzers::{detect_out_001, detect_tpl_001};
use ggen_lsp::project_index::ProjectIndex;
use ggen_lsp::route::{species_for, EditTemplate, RouteRegistry};

// ───────────────────────── fixture / diagnostic helpers ─────────────────────

/// Absolute path to a regression-fixture project root.
fn fixture_root(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("ggen_tpl_001_regression")
        .join(name)
}

/// Load a project index, asserting the load itself succeeds (real I/O).
fn load(root: &Path) -> ProjectIndex {
    ProjectIndex::from_root(root)
        .unwrap_or_else(|e| panic!("ProjectIndex::from_root({}) failed: {e:?}", root.display()))
}

/// The exact diagnostic code string (`GGEN-TPL-001`, `GGEN-OUT-001`, …) or `None`.
fn code_str(d: &Diagnostic) -> Option<&str> {
    match &d.code {
        Some(NumberOrString::String(s)) => Some(s.as_str()),
        _ => None,
    }
}

/// All diagnostic code strings across the per-file grouping returned by
/// `detect_tpl_001` (`Vec<(PathBuf, Vec<Diagnostic>)>`).
fn all_codes(grouped: &[(PathBuf, Vec<Diagnostic>)]) -> Vec<String> {
    grouped
        .iter()
        .flat_map(|(_, diags)| diags.iter())
        .filter_map(|d| code_str(d).map(str::to_string))
        .collect()
}

/// Recursively copy a directory tree using only std (+ tempfile). Real I/O.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// A minimal GGEN-TPL-001 diagnostic for route-selection tests (range only —
/// `select_for_diagnostic` keys on the code, the route's site is the range).
fn tpl_001_diag() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 2,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 12,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String("GGEN-TPL-001".to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "unbound projection: `title`".to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

// ─────────────────── 1. GGEN-OUT-001 active + no TPL leak ────────────────────

/// 1a. GGEN-OUT-001 is now ACTIVE (GALL-OUT-001): its detector compares each
/// rule's dynamic `output_file` Tera pattern against the rule's SPARQL SELECT
/// vars. This barrier locks the activation so a later patch cannot silently
/// regress it back to metadata-only.
#[test]
fn out_001_is_active_in_species_registry() {
    let species = species_for("GGEN-OUT-001").expect("GGEN-OUT-001 must be registered");
    assert!(
        species.detector_active,
        "GGEN-OUT-001 detector must be active (GALL-OUT-001)"
    );
}

/// 1b. The TPL detector (`detect_tpl_001`) must NEVER emit GGEN-OUT-001 — even
/// over a fixture whose OUTPUT PATH carries an unbound var
/// (`output_file = "out/{{ slug }}.txt"`, `slug` not in the SELECT, template BODY
/// clean). OUT-001 lives in `detect_out_001`, anchored on `ggen.toml`, not in the
/// TPL detector anchored on the `.tera` body. (And, since the body is clean, no
/// GGEN-TPL-001 either.) Locks the consumer-surface separation.
#[test]
fn tpl_detector_never_emits_out_001() {
    let project = load(&fixture_root("unbound_output_path"));

    let diags = detect_tpl_001(&project);
    let codes = all_codes(&diags);

    assert!(
        !codes.iter().any(|c| c == "GGEN-OUT-001"),
        "the TPL detector must NEVER emit GGEN-OUT-001 (that is detect_out_001's job, \
         anchored on ggen.toml). Got codes: {codes:?}"
    );
    // The template body binds only `name`, so the TPL-001 detector is also silent
    // — the unbound `slug` lives only in the output path.
    assert!(
        !codes.iter().any(|c| c == "GGEN-TPL-001"),
        "the template body is clean (`name` only); GGEN-TPL-001 must not fire on \
         an output-path-only unbound var. Got codes: {codes:?}"
    );
}

/// 1c. OUT→TPL no-leak barrier: running `detect_out_001` over the TPL `invalid`
/// fixture (template body consumes unbound `title`, but `output_file` is the
/// STATIC `out.txt`) must raise ZERO GGEN-OUT-001 — a static output path is silent
/// by construction. Proves the active OUT-001 detector does not contaminate a
/// TPL-only defect.
#[test]
fn out_detector_silent_on_static_output_path_of_tpl_fixture() {
    let project = load(&fixture_root("invalid"));

    let diags = detect_out_001(&project);
    let codes = all_codes(&diags);

    assert!(
        codes.is_empty(),
        "the TPL invalid fixture has a STATIC output_file (out.txt); detect_out_001 \
         must stay silent. Got codes: {codes:?}"
    );
}

// ─────────────────────── 2. GGEN-HARNESS-001 metadata only ──────────────────

/// 2. GGEN-HARNESS-001 is now ACTIVE (GALL-CHECKPOINT-002): its detector compares
/// Cargo.toml [[test]]/[[bench]] explicit-`path` declarations against the proof
/// files on disk. This barrier locks the activation so a later patch cannot
/// silently regress it back to metadata-only.
#[test]
fn harness_001_is_active() {
    let species = species_for("GGEN-HARNESS-001").expect("GGEN-HARNESS-001 must be registered");
    assert!(
        species.detector_active,
        "GGEN-HARNESS-001 detector must be active (GALL-CHECKPOINT-002)"
    );
}

// ─────────────── 3. no child-LSP dependency (pure-Rust detection) ────────────

/// 3. `detect_tpl_001` is pure Rust and must run with zero external binaries /
/// child LSPs available. We sanity-prove this by running it against the invalid
/// fixture in a process that resolves no child LSP (this integration test never
/// spawns one) and asserting it returns the expected GGEN-TPL-001 diagnostic.
/// If a child-LSP dependency had crept in, detection would require an external
/// process and this in-process call would not produce the diagnostic.
#[test]
fn detect_tpl_001_runs_without_any_child_lsp() {
    let project = load(&fixture_root("invalid"));

    // Pure, in-process call — no subprocess, no socket, no env var read.
    let diags = detect_tpl_001(&project);
    let codes = all_codes(&diags);

    assert!(
        codes.iter().any(|c| c == "GGEN-TPL-001"),
        "detect_tpl_001 must work as pure Rust (no child LSP) and emit GGEN-TPL-001 \
         on the invalid fixture. Got codes: {codes:?}"
    );
}

// ─────────────────────────── 4. valid fixture clean ─────────────────────────

/// 4. The valid fixture (query `SELECT ?name`, template consumes `name` /
/// `row["name"]`) must remain clean: no GGEN-TPL-001.
#[test]
fn valid_fixture_stays_clean() {
    let project = load(&fixture_root("valid"));

    let codes = all_codes(&detect_tpl_001(&project));

    assert!(
        !codes.iter().any(|c| c == "GGEN-TPL-001"),
        "valid fixture binds and consumes only `name`; no GGEN-TPL-001 expected. \
         Got codes: {codes:?}"
    );
}

// ──────────────────── 5. invalid fixture: ONLY GGEN-TPL-001 ──────────────────

/// 5. The invalid fixture (query `SELECT ?name`, template reads `row["title"]`)
/// must emit GGEN-TPL-001 at ERROR severity and NO OTHER `GGEN-*` species code.
/// Locks the blast radius: the unbound-projection defect produces exactly one
/// species and never leaks OUT-001 / HARNESS-001 / PROOF-001.
#[test]
fn invalid_fixture_emits_only_tpl_001() {
    let project = load(&fixture_root("invalid"));

    let grouped = detect_tpl_001(&project);
    let codes = all_codes(&grouped);

    assert!(
        codes.iter().any(|c| c == "GGEN-TPL-001"),
        "invalid fixture accesses unbound `title`; GGEN-TPL-001 must fire. \
         Got codes: {codes:?}"
    );
    // Severity: at least one GGEN-TPL-001 at ERROR.
    assert!(
        grouped.iter().flat_map(|(_, d)| d.iter()).any(|d| {
            code_str(d) == Some("GGEN-TPL-001") && d.severity == Some(DiagnosticSeverity::ERROR)
        }),
        "GGEN-TPL-001 must be reported at ERROR severity. Got: {grouped:?}"
    );
    // Blast-radius lock: no OTHER ggen species code may appear.
    let forbidden = ["GGEN-OUT-001", "GGEN-HARNESS-001", "GGEN-PROOF-001"];
    for f in forbidden {
        assert!(
            !codes.iter().any(|c| c == f),
            "invalid fixture must emit ONLY GGEN-TPL-001; found leaked species {f:?}. \
             Got codes: {codes:?}"
        );
    }
    // Every emitted ggen code must be exactly GGEN-TPL-001 (catch any future code).
    for c in &codes {
        if c.starts_with("GGEN-") {
            assert_eq!(
                c, "GGEN-TPL-001",
                "only GGEN-TPL-001 may be emitted for this fixture; saw {c:?}"
            );
        }
    }
}

// ───────────── 6. analysis writes NO emitted output files ────────────────────

/// 6. The analysis path is read-only. On a TempDir copy of the invalid fixture,
/// building `ProjectIndex::from_root`, running `detect_tpl_001`, AND running the
/// headless `check_files_in_root` gate must NEVER materialize the declared
/// `output_file` (`out.txt`). Locks against an accidental emit-during-analysis.
#[test]
fn analysis_writes_no_emitted_output_files() {
    let src = fixture_root("invalid");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    let declared_output = dst.join("out.txt");
    assert!(
        !declared_output.exists(),
        "fixture copy unexpectedly already contains the declared output file"
    );

    // (a) index + pure detector
    let project = load(&dst);
    let _ = detect_tpl_001(&project);
    assert!(
        !declared_output.exists(),
        "ProjectIndex::from_root + detect_tpl_001 must be read-only; \
         must NOT write {}",
        declared_output.display()
    );

    // (b) headless gate over all law surfaces in the copied tree.
    let paths = vec![
        dst.join("ggen.toml"),
        dst.join("queries/items.rq"),
        dst.join("templates/item.tera"),
        dst.join("schema/domain.ttl"),
    ];
    let report = ggen_lsp::check::check_files_in_root(&dst, &paths, true);

    // The gate must SEE the unbound-projection error (sanity that it ran), and
    // must NOT have written the emitted output.
    assert!(
        report.has_errors(),
        "headless gate must observe the GGEN-TPL-001 ERROR on the invalid fixture"
    );
    assert!(
        !declared_output.exists(),
        "check_files_in_root must be read-only; must NOT write the declared \
         output_file ({})",
        declared_output.display()
    );

    // No stray `out/` directory either (the emitted-output marker dirs).
    assert!(
        !dst.join("out").exists(),
        "analysis must not create an emitted-output directory"
    );
}

// ───────── 7. GGEN-TPL-001 route targets only source law, never output ───────

/// 7. The route selected for a GGEN-TPL-001 diagnostic must target source law
/// (ggen.toml / SPARQL / Tera template) on every step, and NO step may reference
/// an emitted-output path (`out/`, `output/`, `dist/`, `gen/`, the rule's
/// `output_file`). Every step is advisory (NoOp) — inspect-only actuation.
/// Locks the load-bearing invariant that a repair never edits emitted output.
#[test]
fn tpl_001_route_targets_only_source_law() {
    let registry = RouteRegistry::seeded();
    let diag = tpl_001_diag();

    let route = registry
        .select_for_diagnostic(&diag)
        .expect("a GGEN-TPL-001 diagnostic must resolve to a seeded source-law route");

    assert!(
        !route.steps.nodes.is_empty(),
        "the GGEN-TPL-001 route must have at least one step"
    );

    // Emitted-output *path* markers that must NEVER appear in any step title or
    // the route description. These are concrete output-path fragments (the
    // rule's `output_file` and the conventional emitted dirs). We deliberately
    // do NOT forbid the bare word "emitted": the route description lawfully
    // says it "never edits emitted output" — a disclaimer of output editing,
    // which is the invariant we are LOCKING, not violating.
    const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "out.txt"];
    // Source-law surfaces a step must reference.
    const SOURCE_LAW: &[&str] = &["sparql", "template", "ggen.toml"];

    for step in &route.steps.nodes {
        // Inspect-only: advisory NoOp edits — never an output-mutating edit.
        assert!(
            matches!(step.edit, EditTemplate::NoOp),
            "GGEN-TPL-001 step {:?} must be advisory (NoOp), never an output edit",
            step.id
        );

        let title = step.title.to_lowercase();

        for forbidden in FORBIDDEN_OUTPUT {
            assert!(
                !title.contains(forbidden),
                "GGEN-TPL-001 step title {:?} references forbidden emitted-output \
                 marker {forbidden:?} — a repair must never target emitted output",
                step.title
            );
        }

        assert!(
            SOURCE_LAW.iter().any(|s| title.contains(s)),
            "GGEN-TPL-001 step title {:?} must target a source-law surface \
             (SPARQL / Tera template / ggen.toml)",
            step.title
        );
    }

    // The route description, too, must not name an emitted-output path.
    let desc = route.description.to_lowercase();
    for forbidden in FORBIDDEN_OUTPUT {
        assert!(
            !desc.contains(forbidden),
            "GGEN-TPL-001 route description references forbidden emitted-output \
             marker {forbidden:?}: {:?}",
            route.description
        );
    }
}
