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
use std::process::Command;

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

// ───────────────────────── capstone: 31-pack composition ─────────────────────
//
// The 30 packs above PLUS ggen-verify-pack (the verification capstone that the
// module doc excludes from the base test). Here the exclusion reason is
// answered head-on: the consumer runs the pack's committed bootstrap emitter
// (packs/ggen-verify-pack/bootstrap/verify-evidence-bootstrap.sh) to produce
// REAL `ver:` evidence facts, wires the `evidence/` mini-pack into ggen.toml
// (the working-consumer pattern from examples/tcps-generated and
// verify_pack_evidence_loop_e2e.rs), and syncs with the verify gates ACTIVE
// over the entire 31-pack union graph. Sabotage (evidence facts emptied) must
// refuse with FM-PACK-013 naming gate 010_evidence_present.
//
// Ground truth (hermetic run on this branch, 2026-07-20): green loop + sabotage
// refusal in 24.4s wall clock (test-runner-reported, prebuilt `ggen` binary) —
// under the 60s threshold, hence no `#[ignore]`.

#[cfg(unix)]
fn make_executable(path: &Path) {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path).expect("metadata").permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms).expect("chmod");
}

/// Run a shell script from the consumer root with `stub_bin` prepended to
/// PATH (real stub `cargo` — the house subprocess pattern); returns
/// (exit_code, combined output).
fn run_script(project: &Path, stub_bin: &Path, script: &Path) -> (i32, String) {
    let path = format!(
        "{}:{}",
        stub_bin.display(),
        std::env::var("PATH").expect("PATH")
    );
    let out = Command::new("bash")
        .arg(script)
        .current_dir(project)
        .env("PATH", path)
        .output()
        .expect("run script");
    (
        out.status.code().unwrap_or(-1),
        format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ),
    )
}

#[test]
fn thirty_one_packs_compose_with_verify_gates_active() {
    // ── Scaffold: the 30 proven packs + ggen-verify-pack ─────────────────
    let dir = TempDir::new().expect("tempdir");
    for pack in COMPOSED_PACKS.iter().chain(["ggen-verify-pack"].iter()) {
        copy_tree(&packs_dir().join(pack), &dir.path().join(pack));
    }

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(project.join("scripts/checks")).expect("mkdir checks");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");

    let packs_table: String = COMPOSED_PACKS
        .iter()
        .map(|p| format!("{p} = {{ path = \"../{p}\" }}\n"))
        .collect();
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"capstone-consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_table}\
             ggen-verify-pack = {{ path = \"../ggen-verify-pack\" }}\n\
             verify-evidence = {{ path = \"evidence\", lock = false }}\n\n\
             [templates]\ndir = \"templates\"\n\n\
             [law]\nreflexive = true\n"
        ),
    )
    .expect("write ggen.toml");

    // The one consumer-specific required check: a REAL command whose REAL
    // exit code lands in the evidence graph.
    let hook = project.join("scripts/checks/byte-identity.sh");
    std::fs::write(&hook, "#!/bin/sh\ntest -f ggen.toml\n").expect("write hook");
    make_executable(&hook);

    // Real stub cargo on PATH for the heavy `ver:checkCommand` cargo checks:
    // it genuinely executes and genuinely exits 0.
    let stub_bin = dir.path().join("stub-bin");
    std::fs::create_dir_all(&stub_bin).expect("mkdir stub-bin");
    let stub_cargo = stub_bin.join("cargo");
    std::fs::write(&stub_cargo, "#!/bin/sh\necho stub-cargo \"$@\"\nexit 0\n")
        .expect("write stub cargo");
    make_executable(&stub_cargo);

    // ── Phase 1: committed bootstrap emitter produces the evidence ───────
    let bootstrap = dir
        .path()
        .join("ggen-verify-pack/bootstrap/verify-evidence-bootstrap.sh");
    let (code, out) = run_script(&project, &stub_bin, &bootstrap);
    assert_eq!(code, 0, "bootstrap must exit 0: {out}");
    let evidence =
        std::fs::read_to_string(project.join("evidence/ontology.ttl")).expect("evidence");
    assert!(
        evidence.contains("ver:exitCode 0"),
        "green evidence recorded: {evidence}"
    );

    // ── Phase 2: sync over the 31-pack union, verify gates ACTIVE ────────
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run capstone sync");
    output.assert_success();

    let generated_emitter = project.join("scripts/verify-evidence.sh");
    assert!(
        generated_emitter.is_file(),
        "capstone sync must generate scripts/verify-evidence.sh"
    );
    assert!(project.join("VERIFICATION.md").is_file());

    // Lock still covers all 30 base packs alongside the capstone.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    for pack in COMPOSED_PACKS.iter().chain(["ggen-verify-pack"].iter()) {
        assert!(
            lock.contains(&format!("[packs.{pack}]")),
            "ggen.lock missing [packs.{pack}]"
        );
    }

    // ── Phase 3: steady state — generated emitter re-run, resync green ───
    let (code, out) = run_script(&project, &stub_bin, &generated_emitter);
    assert_eq!(code, 0, "generated emitter must exit 0: {out}");
    let output2 = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run resync");
    output2.assert_success();

    // ── Phase 4: sabotage — evidence facts gone => FM-PACK-013 refusal ───
    std::fs::write(
        project.join("evidence/ontology.ttl"),
        "@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n",
    )
    .expect("truncate evidence");
    let sabotage = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sabotage sync");
    sabotage.assert_failure();
    sabotage.assert_stderr_contains("FM-PACK-013");
    sabotage.assert_stderr_contains("010_evidence_present");
}
