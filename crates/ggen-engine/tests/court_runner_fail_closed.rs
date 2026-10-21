#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Fail-closed court: a corrupted/lying digest mid-protocol must exit 2
//! (`COURT_BLOCKED`) and must still restore the target byte-exact.
//!
//! Chicago discipline: real bash subprocess, real files, real `patch`,
//! assertions on the observed exit code and on-disk state — no doubles.

use std::fs;
use std::process::Command;

use ggen_engine::court::{compile_court, parse_mutant_catalog};

/// Catalog + target whose expectedPostHash lies: the shim lands cleanly
/// but its real digest cannot match the bound one, so step 3 must refuse
/// with exit 2 BEFORE the after-gate ever runs — and the EXIT trap must
/// restore the target byte-exact.
#[test]
fn corrupt_post_hash_mid_protocol_is_court_blocked_exit_2() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path();

    let target_rel = "lib/gate.ex";
    fs::create_dir_all(root.join("lib")).expect("lib dir");
    fs::write(root.join(target_rel), "gate: ok\n").expect("target file");

    let patch = "--- a/lib/gate.ex\n+++ b/lib/gate.ex\n@@ -1 +1 @@\n-gate: ok\n+gate: mutated\n";
    let catalog = parse_mutant_catalog(&format!(
        r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#m> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "{target_rel}" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "{}" ;
    vc:expectedPostHash "{}" ;
    vc:shimTs 1000 ;
    vc:witnessTs 2000 .
"#,
        patch.replace('\n', "\\n"),
        "0".repeat(64),
    ))
    .expect("catalog parses");
    let out_dir = root.join("out");
    compile_court(&catalog, &out_dir).expect("court compiles");

    // A gate that passes on the clean tree (so step 2 does not fire first).
    let output = Command::new("bash")
        .arg(out_dir.join("runner.sh"))
        .env("GATE_CMD", "true")
        .current_dir(root)
        .output()
        .expect("runner runs");

    assert_eq!(
        output.status.code(),
        Some(2),
        "corrupt post-hash mid-protocol must exit COURT_BLOCKED(2), got {:?}: {} {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("COURT_BLOCKED") && stderr.contains("post-shim digest"),
        "typed refusal names the failing step: {stderr}"
    );

    // Fail-closed includes restore: the target is byte-identical to baseline.
    let restored = fs::read(root.join(target_rel)).expect("target still present");
    assert_eq!(
        restored, b"gate: ok\n",
        "mid-protocol failure restores byte-exact"
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("restored on exit"),
        "the restore-on-exit guard is witnessed in stdout"
    );
}

/// A gate already refusing the clean tree breaks the court premise:
/// exit 2 at step 2 (before), never a vacuous pass.
#[test]
fn gate_failing_clean_tree_is_court_blocked_exit_2() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path();

    let target_rel = "lib/gate.ex";
    fs::create_dir_all(root.join("lib")).expect("lib dir");
    fs::write(root.join(target_rel), "gate: ok\n").expect("target file");

    let patch = "--- a/lib/gate.ex\n+++ b/lib/gate.ex\n@@ -1 +1 @@\n-gate: ok\n+gate: mutated\n";
    // expectedPostHash computed for real this time: sha256("gate: mutated\n").
    let catalog = parse_mutant_catalog(&format!(
        r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#m> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "{target_rel}" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "{}" ;
    vc:expectedPostHash "{}" ;
    vc:shimTs 1000 ;
    vc:witnessTs 2000 .
"#,
        patch.replace('\n', "\\n"),
        sha256_hex(b"gate: mutated\n"),
    ))
    .expect("catalog parses");
    let out_dir = root.join("out");
    compile_court(&catalog, &out_dir).expect("court compiles");

    // Gate refuses even the clean tree — premise broken.
    let output = Command::new("bash")
        .arg(out_dir.join("runner.sh"))
        .env("GATE_CMD", "false")
        .current_dir(root)
        .output()
        .expect("runner runs");

    assert_eq!(
        output.status.code(),
        Some(2),
        "premise break is COURT_BLOCKED"
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("before:"),
        "the failing step is named: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        fs::read(root.join(target_rel)).expect("target"),
        b"gate: ok\n",
        "clean-tree premise failure restores byte-exact"
    );
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::Digest as _;
    hex::encode(sha2::Sha256::digest(bytes))
}
