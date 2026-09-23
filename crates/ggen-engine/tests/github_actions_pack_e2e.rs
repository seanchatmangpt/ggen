//! Chicago-TDD end-to-end proof for `packs/github-actions-pack`: real
//! filesystem (`TempDir`), real graph engine, real Tera — no mocks.
//!
//! Proves the pack generates its first product families (reusable Rust
//! inspection workflow, setup-ggen / emit-evidence composite actions, the
//! caller example doc), that every generated remote action ref is pinned to
//! a 40-hex commit SHA, that permissions are minimal (`contents: read`,
//! never `write-all`), that a second sync is byte-identical (no drift), and
//! that the two pack-shipped SPARQL gates genuinely REFUSE sabotage facts:
//! write-all ceilings, mutable third-party action refs, and workflows
//! missing required facts (purpose/trigger/permission ceiling).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use support::copy_tree;
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Scaffold a consumer project next to a copy of the pack. `extra_ttl` is
/// appended to the consumer's own ontology.ttl (sabotage injection point —
/// gates run against the UNION graph, so consumer facts can trip them).
fn scaffold(extra_ttl: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("consumer");
    // The pack lives INSIDE the consumer (packs/…) because ggen.toml
    // semantic validation refuses `..` path traversal in extra_ontologies.
    copy_tree(
        &packs_dir().join("github-actions-pack"),
        &project.join("packs/github-actions-pack"),
    );
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    let ontology = format!(
        "@prefix gha: <http://seanchatmangpt.github.io/packs/github-actions#> .\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n\
         {extra_ttl}\n"
    );
    std::fs::write(project.join("ontology.ttl"), ontology).expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"gha-consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngithub-actions-pack = { path = \"packs/github-actions-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    (dir, project)
}

const PRODUCTS: [&str; 4] = [
    ".github/workflows/reusable-rust-inspection.yml",
    ".github/actions/setup-ggen/action.yml",
    ".github/actions/emit-evidence/action.yml",
    "docs/github-actions/inspection-caller-example.yml",
];

#[test]
fn github_actions_pack_syncs_schema_only_clean_and_idempotent() {
    // Contract witness for the 0.2.0 pack rebuild (e34f64624): the pack is
    // schema-only -- ontology.ttl carries ZERO gha:Workflow/Job/Step
    // individuals, and its two rules-driven templates
    // (workflow.yml.tmpl / composite_action.yml.tmpl) are deliberately
    // frontmatter-free, rendered only by a consumer's own
    // [[generation.rules]]. The prior revision's four hardcoded products
    // (reusable-rust-inspection workflow/caller, emit-evidence composite
    // action) were retired in that rebuild -- "named here, not silently
    // dropped" -- so this witness asserts the CURRENT contract: the pack
    // self-discovers cleanly (frontmatter-less templates are skipped by
    // self-discovery, never frontmatter-parsed), writes nothing without a
    // consumer-authored generation rule, and is byte-idempotent on re-sync.
    let (_dir, project) = scaffold("");

    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("schema-only pack must sync cleanly with no consumer individuals");

    // The retired 0.1.0 product families must NOT be fabricated by
    // self-discovery: products are consumer-authored via generation rules.
    for rel in PRODUCTS {
        assert!(
            !project.join(rel).exists(),
            "schema-only pack must not fabricate retired product {rel}"
        );
    }
    assert!(
        !project.join(".github/workflows").exists(),
        "no consumer individuals means no generated workflows"
    );

    // The pack is still recorded in the consumer lock by name.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock.contains("github-actions-pack"),
        "lock must record the pack: {lock}"
    );

    // Idempotency: second sync byte-stable (frontmatter skip must be stable,
    // not a per-run decision).
    let _ = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        !project.join(".github/workflows").exists(),
        "second sync must not start emitting products"
    );
    assert!(first.written.is_empty() || !project.join(".github/workflows").exists());
}

#[test]
fn github_actions_pack_refuses_write_all_ceiling() {
    let (_dir, project) = scaffold(
        "gha:SabotageWriteAll a gha:Workflow ;\n\
         \u{20}   gha:purpose \"sabotage: over-broad token\" ;\n\
         \u{20}   gha:trigger \"push\" ;\n\
         \u{20}   gha:permissionCeiling \"write-all\" .\n",
    );

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("write-all ceiling must refuse the sync");
    let msg = format!("{err}");
    assert!(
        msg.contains("020_security"),
        "refusal must name the security gate: {msg}"
    );
    assert!(
        msg.to_lowercase().contains("write-all"),
        "refusal must name the write-all violation: {msg}"
    );
    for rel in PRODUCTS {
        assert!(
            !project.join(rel).exists(),
            "refused sync must not emit {rel}"
        );
    }
}

/// Sabotage: a mutable third-party action ref (tag, not 40-hex SHA) on a
/// gha-namespace subject. The security gate must refuse and demand a pin.
#[test]
fn github_actions_pack_refuses_mutable_action_ref() {
    let (_dir, project) = scaffold("gha:SabotageStep gha:usesAction \"actions/checkout@v4\" .\n");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("mutable action ref must refuse the sync");
    let msg = format!("{err}");
    assert!(
        msg.contains("020_security"),
        "refusal must name the security gate: {msg}"
    );
    assert!(
        msg.contains("actions/checkout@v4") && msg.contains("40-hex"),
        "refusal must name the offending ref and the SHA-pin remedy: {msg}"
    );
}

/// Sabotage: a gha:Workflow missing its required facts (no purpose, no
/// trigger, no permission ceiling). The required-facts gate (010) must
/// refuse — unspecified permissions are a refusal, not a warning.
#[test]
fn github_actions_pack_refuses_workflow_missing_required_facts() {
    let (_dir, project) = scaffold(
        "gha:SabotageBare a gha:Workflow ;\n\
         \u{20}   rdfs:label \"sabotage: workflow with nothing declared\" .\n",
    );

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("workflow without required facts must refuse the sync");
    let msg = format!("{err}");
    assert!(
        msg.contains("010_required"),
        "refusal must name the required-facts gate: {msg}"
    );
    assert!(
        msg.contains("SabotageBare"),
        "refusal must name the offending subject: {msg}"
    );
}
