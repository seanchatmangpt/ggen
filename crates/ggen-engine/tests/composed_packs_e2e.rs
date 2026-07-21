//! Chicago-TDD end-to-end proof that the maximal pack set composes into ONE
//! consumer project: real filesystem (`TempDir`), the real `ggen` binary as a
//! subprocess (`CliHarness`), no mocks. Track E2 of the pack-composition work.
//!
//! Composed set (30 packs): the 23 previously-proven framework packs,
//! gh-terraform-pack, and the 6 packs fixed by Track E1
//! (feat/pack-composition-fixes): clap-noun-verb-pack, affidavit-pack,
//! dogfood-lifecycle-pack, self-monitoring-pack, ma-case-study-pack,
//! ggen-constitution-pack.
//!
//! Excluded by design:
//! - ggen-verify-pack: the verification capstone — it refuses to sync unless
//!   consumer-provided `ver:` evidence facts exist, which this hermetic
//!   consumer deliberately does not fabricate.
//! - tcps-core-pack: DEFERRED-FROZEN in E1 — its flat-`src/` output layout is
//!   byte-identity-frozen against the 製品版 v26.7.19 reference tree
//!   (`examples/tcps-generated/tests/tcps_conformance_e2e.rs`), and under
//!   composition it would collide with the consumer's own `src/`.
//!
//! Ground truth (manual hermetic run, 2026-07-20, this branch): sync exit 0,
//! 1139 files written, 30 `[packs.*]` lock entries, second sync tree
//! byte-identical outside `.ggen-v2/` receipts, ~3.5s wall clock — hence no
//! `#[ignore]`.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// The 30 composed packs (see module doc for provenance of each group).
const COMPOSED_PACKS: [&str; 30] = [
    // 23 previously-proven framework packs
    "anti-llm-cheat-lsp-pack",
    "cargo-cicd-pack",
    "chicago-tdd-tools-pack",
    "claude-code-pack",
    "ggen-release-pack",
    "level-five-book-pack",
    "lsp-max-pack",
    "mcpp-pack",
    "mfact-pack",
    "mfw-pack",
    "osx-clnr-pack",
    "praxis-core-pack",
    "star-toml-pack",
    "tcps-cli-pack",
    "tcps-ffi-pack",
    "tcps-release-pack",
    "tcps-std-pack",
    "tcps-wasm-pack",
    "wasm4pm-algorithms-pack",
    "wasm4pm-cognition-pack",
    "wasm4pm-compat-pack",
    "wasm4pm-facts-pack",
    "wasm4pm-pack",
    // TCPS repository-management pack (PR #320)
    "gh-terraform-pack",
    // The 6 packs fixed by Track E1
    "clap-noun-verb-pack",
    "affidavit-pack",
    "dogfood-lifecycle-pack",
    "self-monitoring-pack",
    "ma-case-study-pack",
    "ggen-constitution-pack",
];

/// Repository `packs/` directory (relative to this crate's manifest).
fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Recursively copy `src` into `dst`.
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

/// Copy every composed pack plus a minimal consumer project into a fresh
/// TempDir; the `[packs]` table is generated programmatically over
/// `COMPOSED_PACKS`. The consumer has an empty local ontology and an empty
/// local templates dir, so only pack templates run over pack ontologies.
fn scaffold_composed_project() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    for pack in COMPOSED_PACKS {
        copy_tree(&packs_dir().join(pack), &dir.path().join(pack));
    }

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");

    let packs_table: String = COMPOSED_PACKS
        .iter()
        .map(|p| format!("{p} = {{ path = \"../{p}\" }}\n"))
        .collect();
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_table}\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    (dir, project)
}

/// Recursively hash every file under `root`, excluding `.ggen-v2/` (receipts
/// legitimately re-chain on every sync) — returns sorted (rel_path, blake3).
fn tree_digest(root: &Path) -> Vec<(String, String)> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let path = entry.expect("entry").path();
            let rel = path
                .strip_prefix(root)
                .expect("strip")
                .to_string_lossy()
                .into_owned();
            if rel == ".ggen-v2" {
                continue;
            }
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                let bytes = std::fs::read(&path).expect("read file");
                out.push((rel, blake3::hash(&bytes).to_hex().to_string()));
            }
        }
    }
    let mut out = Vec::new();
    walk(root, root, &mut out);
    out.sort();
    out
}

#[test]
fn thirty_packs_compose_into_one_consumer() {
    let (_dir, project) = scaffold_composed_project();

    // (1) One sync over the union graph of all 30 packs succeeds.
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_success();

    // (2) Volume: >= 900 generated files (ground-truth run: 1139 written;
    // 23-pack baseline was 915). Count real files on disk, excluding the
    // scaffold's own 2 inputs and bookkeeping dirs.
    let generated = tree_digest(&project)
        .into_iter()
        .filter(|(rel, _)| rel != "ggen.toml" && rel != "ontology.ttl" && rel != "ggen.lock")
        .count();
    assert!(
        generated >= 900,
        "expected >= 900 generated files, found {generated}"
    );

    // (3) ggen.lock lists every composed pack with a blake3 content hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    for pack in COMPOSED_PACKS {
        assert!(
            lock.contains(&format!("[packs.{pack}]")),
            "ggen.lock missing [packs.{pack}]"
        );
    }
    assert!(
        lock.matches("content_hash = \"blake3:").count() >= COMPOSED_PACKS.len(),
        "lock must carry a blake3 content_hash per pack: {lock}"
    );

    // (4) Spot-check one load-bearing artifact per representative pack.
    let spot_checks: [(&str, &str, &str); 6] = [
        (
            "gh-terraform-pack",
            "infra/terraform/github/labels.tf",
            "github_issue_label",
        ),
        (
            "lsp-max-pack",
            "rules/lsp_max_lspmax_unwrap_001.toml",
            "LSPMAX-UNWRAP-001",
        ),
        (
            "star-toml-pack",
            "src/star_toml_config.rs",
            "Generated by star-toml-pack",
        ),
        (
            "wasm4pm-compat-pack",
            "src/wasm4pm_compat_events.rs",
            "pub enum EmittedEventType",
        ),
        (
            "ma-case-study-pack",
            "docs/ma-case-study-pack-vocabulary.md",
            "",
        ),
        ("ggen-constitution-pack", "docs/CONSTITUTION-pack.md", ""),
    ];
    for (pack, rel, needle) in spot_checks {
        let path = project.join(rel);
        assert!(path.is_file(), "{pack} must write {rel}");
        if !needle.is_empty() {
            let content = std::fs::read_to_string(&path).expect(rel);
            assert!(content.contains(needle), "{rel} missing {needle:?}");
        }
    }

    // (5) Idempotence: a second sync leaves the tree byte-identical outside
    // `.ggen-v2/` receipts, including a byte-identical ggen.lock. (The engine
    // reports mode=Overwrite files as "written" even when identical, so the
    // proof is byte identity, not the written-count.)
    let before = tree_digest(&project);
    let output2 = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run second sync");
    output2.assert_success();
    let after = tree_digest(&project);
    assert_eq!(
        before, after,
        "second sync must be byte-identical outside .ggen-v2/"
    );
}
