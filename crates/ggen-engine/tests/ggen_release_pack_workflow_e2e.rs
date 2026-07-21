//! Chicago-TDD e2e proof for L5 condition 1's stage-A narrowing: the
//! `grp:MarketplaceDocsWorkflow` individual added to
//! `packs/ggen-release-pack/ontology.ttl` (literal-body-template pattern copied
//! verbatim from the pre-existing `grp:PublishCandidateWorkflow`) must regenerate
//! `.github/workflows/marketplace-docs.yml` byte-identically, and a second sync
//! must be a content no-op (the same idempotency proof already established for
//! `.github/workflows/publish-candidate.yml` and recorded as `gen:G2`/`gen:G3`
//! elsewhere in this repo).
//!
//! Real collaborators only: the real `packs/ggen-release-pack/` directory and
//! the real `.specify/templates/docs/ggen_marketplace_docs_workflow.yml.tera`
//! template are copied byte-for-byte (`std::fs::copy`, not fabricated content)
//! into a fresh `TempDir`, then the real `ggen` binary (`CliHarness`) is spawned
//! as a subprocess against that tree -- same "real filesystem, real oxigraph,
//! real Tera, real new `ggen` binary" shape as `framework_packs_e2e.rs`, whose
//! `write_synthetic_pack`/scaffold helpers this file's `copy_tree` mirrors.
//!
//! This test does NOT run `ggen sync run` against the actual checked-out repo
//! root (no existing test in this crate does that -- confirmed by inspection of
//! `framework_packs_e2e.rs`, `reflexive_law_e2e.rs`, `receipt_chain_e2e.rs`,
//! `cli_boundary.rs`: every one of them scaffolds a synthetic project in a
//! `TempDir`). Instead it builds a faithful synthetic consumer that imports the
//! REAL pack ontology and REAL template, and additionally cross-checks the
//! synthetic regeneration's bytes against the actual committed
//! `.github/workflows/marketplace-docs.yml` in this checkout -- proving the
//! committed file IS what this generation rule produces, not just that some
//! generation is idempotent in the abstract.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Workspace root, resolved from this test binary's own compile-time location
/// (mirrors `product_mirror_conformance.rs`'s `workspace_root` helper) so the
/// test works regardless of the directory `cargo test` is invoked from.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("crates/ggen-engine -> crates -> workspace root")
        .to_path_buf()
}

/// Recursively copy `src` into `dst` (verbatim from `framework_packs_e2e.rs`'s
/// `copy_tree` helper -- integration test binaries do not share a `mod common`
/// in this crate, so each file keeps its own copy by established convention;
/// see `reflexive_law_e2e.rs`'s module doc for the same note).
fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src).expect("read_dir") {
        let entry = entry.expect("entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy");
        }
    }
}

/// Scaffold a synthetic consumer project that imports the REAL
/// `packs/ggen-release-pack/ontology.ttl` and REAL
/// `.specify/templates/docs/ggen_marketplace_docs_workflow.yml.tera`, wired
/// with a `[[generation.rules]]` entry identical in shape to the real one this
/// change added to the root `ggen.toml`. Returns `(tempdir, project_root)`.
fn scaffold_marketplace_docs_consumer() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    let root = workspace_root();
    let project = dir.path().join("consumer");

    // Real pack directory, copied whole, INSIDE the consumer project root --
    // mirrors this actual repo's own layout (packs/ is a subdirectory of the
    // repo root, sibling of ggen.toml itself), which is what the
    // `[ontology].imports = ["packs/..."]` path below resolves relative to
    // (project-root-relative, per `resolve_template_source`/ontology import
    // resolution in `crates/ggen-engine/src/generation_rules.rs` /
    // `crates/ggen-engine/src/sync.rs`).
    copy_tree(
        &root.join("packs/ggen-release-pack"),
        &project.join("packs/ggen-release-pack"),
    );

    // Real template file, copied byte-for-byte -- not re-authored here.
    std::fs::create_dir_all(project.join(".specify/templates/docs")).expect("mkdir templates");
    std::fs::copy(
        root.join(".specify/templates/docs/ggen_marketplace_docs_workflow.yml.tera"),
        project.join(".specify/templates/docs/ggen_marketplace_docs_workflow.yml.tera"),
    )
    .expect("copy real template");

    // Minimal local ontology: no triples of its own are needed, only the
    // pack import (the SPARQL SELECT below reads exclusively from
    // grp:MarketplaceDocsWorkflow, defined in the imported pack ontology).
    std::fs::write(
        project.join("ontology.ttl"),
        "@prefix ex: <http://example.org/marker#> .\n",
    )
    .expect("write ontology.ttl");

    std::fs::write(
        project.join("ggen.toml"),
        r#"[project]
name = "consumer"
version = "0.0.1"

[ontology]
source = "ontology.ttl"
imports = ["packs/ggen-release-pack/ontology.ttl"]

[templates]
dir = "templates"

[generation]
output_dir = "."

[[generation.rules]]
name        = "ggen-release-marketplace-docs-workflow"
query       = { inline = """
PREFIX grp: <http://seanchatmangpt.github.io/packs/ggen-release#>
SELECT ?body WHERE { grp:MarketplaceDocsWorkflow grp:workflowBody ?body . } ORDER BY ?body
""" }
template    = { file = ".specify/templates/docs/ggen_marketplace_docs_workflow.yml.tera" }
output_file = ".github/workflows/marketplace-docs.yml"
skip_empty  = false
mode        = "Overwrite"
"#,
    )
    .expect("write ggen.toml");

    // `[templates] dir = "templates"` requires the directory to exist even
    // though no ad-hoc `.tmpl` files live in it for this rule (the pack's own
    // literal-body rule needs no per-consumer template scan hits).
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates dir");

    (dir, project)
}

/// The synthetic regeneration of `.github/workflows/marketplace-docs.yml`,
/// via the REAL pack ontology + REAL template, is byte-identical to the file
/// actually committed in this checkout -- proving the committed file IS what
/// this generation rule produces, not merely "some template renders
/// something."
#[test]
fn marketplace_docs_workflow_regenerates_byte_identical_to_committed_file() {
    let (_dir, project) = scaffold_marketplace_docs_consumer();

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    let generated = std::fs::read_to_string(project.join(".github/workflows/marketplace-docs.yml"))
        .expect("read generated workflow file");
    let committed =
        std::fs::read_to_string(workspace_root().join(".github/workflows/marketplace-docs.yml"))
            .expect("read committed workflow file");

    assert_eq!(
        generated, committed,
        "synthetic regeneration (from the real pack ontology + real template) must be \
         byte-identical to the committed .github/workflows/marketplace-docs.yml"
    );
}

/// A second `ggen sync run` over the same synthetic consumer must be a
/// content no-op -- the same idempotency proof already established for
/// `.github/workflows/publish-candidate.yml` (`gen:G2`/`gen:G3`).
#[test]
fn marketplace_docs_workflow_second_sync_is_idempotent() {
    let (_dir, project) = scaffold_marketplace_docs_consumer();

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();
    let first = std::fs::read_to_string(project.join(".github/workflows/marketplace-docs.yml"))
        .expect("read first-run output");

    let second_run = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync");
    second_run.assert_success();
    let second = std::fs::read_to_string(project.join(".github/workflows/marketplace-docs.yml"))
        .expect("read second-run output");

    assert_eq!(
        first, second,
        "second sync must not change .github/workflows/marketplace-docs.yml content"
    );

    // The engine's own dry-run classification for the second run must report
    // the file as unchanged, not merely happen to produce matching bytes by
    // coincidence -- same "unchanged: content identical" decision surfaced by
    // `ggen sync run --dry-run`'s JSON `decisions` map for the real
    // `publish-candidate.yml` rule.
    let dry_run_output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--dry-run"])
        .current_dir(&project)
        .run()
        .expect("dry-run after second sync");
    dry_run_output.assert_success();
    dry_run_output.assert_stdout_contains("unchanged: content identical");
}
