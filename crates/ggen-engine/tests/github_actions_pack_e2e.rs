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

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

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
         [packs]\ngithub-actions-pack = { path = \"packs/github-actions-pack\", \
         extra_ontologies = [\"packs/github-actions-pack/bodies.ttl\"] }\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    (dir, project)
}

fn read(project: &Path, rel: &str) -> String {
    std::fs::read_to_string(project.join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

const PRODUCTS: [&str; 4] = [
    ".github/workflows/reusable-rust-inspection.yml",
    ".github/actions/setup-ggen/action.yml",
    ".github/actions/emit-evidence/action.yml",
    "docs/github-actions/inspection-caller-example.yml",
];

/// Every `uses:` line naming a remote action (`owner/repo@ref`, not a
/// repo-local `./` path) must be pinned to a 40-hex commit SHA.
fn assert_all_remote_refs_pinned(rel: &str, body: &str) {
    for line in body.lines() {
        let trimmed = line.trim();
        let Some(rest) = trimmed.strip_prefix("uses:").or_else(|| {
            trimmed.strip_prefix("- uses:")
        }) else {
            continue;
        };
        let spec = rest.trim().trim_matches(|c| c == '"' || c == '\'');
        if spec.starts_with("./") {
            continue; // repo-local composite action: no ref to pin
        }
        let Some((_, r)) = spec.split_once('@') else {
            panic!("{rel}: remote uses without a ref: {line}");
        };
        // Comments after the SHA (e.g. `# v4`) are allowed.
        let sha = r.split_whitespace().next().unwrap_or("");
        assert!(
            sha.len() == 40 && sha.chars().all(|c| c.is_ascii_hexdigit()),
            "{rel}: mutable third-party action ref (not a 40-hex SHA): {line}"
        );
    }
}

#[test]
fn github_actions_pack_generates_products_pinned_minimal_and_idempotent() {
    let (_dir, project) = scaffold("");

    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert!(!first.written.is_empty(), "pack must write files");

    // (1) All four first-generation product families exist.
    for rel in PRODUCTS {
        assert!(project.join(rel).is_file(), "missing {rel}");
    }

    // (2) The reusable inspection workflow: generated marker, workflow_call
    // trigger, minimal permission ceiling, derivation comment, and every
    // remote action ref pinned to a 40-hex SHA.
    let wf = read(&project, PRODUCTS[0]);
    assert!(
        wf.contains("GENERATED by ggen sync from packs/github-actions-pack"),
        "workflow must carry the generated marker: {wf}"
    );
    assert!(wf.contains("workflow_call"), "reusable trigger: {wf}");
    assert!(
        wf.contains("permissions:") && wf.contains("contents: read"),
        "minimal ceiling must be declared: {wf}"
    );
    assert!(
        wf.contains("Permission derivation"),
        "derivation comment must survive generation: {wf}"
    );

    // (3) Security invariants hold in every generated product: no
    // write-all, no unpinned remote refs, no pull_request_target.
    for rel in PRODUCTS {
        let body = read(&project, rel);
        assert!(
            !body.to_lowercase().contains("write-all"),
            "{rel} must never grant write-all: {body}"
        );
        assert!(
            !body.contains("pull_request_target"),
            "{rel} must not introduce pull_request_target: {body}"
        );
        assert_all_remote_refs_pinned(rel, &body);
    }

    // (4) The composite actions are structurally real composite actions.
    for rel in [PRODUCTS[1], PRODUCTS[2]] {
        let body = read(&project, rel);
        assert!(
            body.contains("runs:") && body.contains("composite"),
            "{rel} must be a composite action: {body}"
        );
    }

    // (5) The caller example calls the generated reusable workflow.
    let caller = read(&project, PRODUCTS[3]);
    assert!(
        caller.contains("reusable-rust-inspection.yml"),
        "caller example must reference the reusable workflow: {caller}"
    );

    // (6) Idempotency: second sync writes nothing; products byte-identical.
    let snapshots: Vec<String> = PRODUCTS.iter().map(|rel| read(&project, rel)).collect();
    let second = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second sync wrote: {:?}",
        second.written
    );
    for (rel, before) in PRODUCTS.iter().zip(snapshots) {
        assert_eq!(
            read(&project, rel),
            before,
            "{rel} must be byte-identical across identical syncs"
        );
    }
}

/// Sabotage: a gha:Workflow with a write-all permission ceiling. The
/// security gate (020) must refuse the sync and say why.
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
    let (_dir, project) = scaffold(
        "gha:SabotageStep gha:usesAction \"actions/checkout@v4\" .\n",
    );

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
