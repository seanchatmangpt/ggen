//!
//! No trybuild in this workspace (verified: zero `trybuild` matches across all
//! `Cargo.toml`s), so the compile-fail court is the abb-sbb doc-test pattern:
//! `compile_fail` doctests inside `src/generated/refusals.rs` (negative) plus
//! a runnable positive-control doctest. This integration test drives the real
//! `cargo test --doc` gate for both crates as a subprocess and asserts on real
//! exit status — plus positive-control state assertions here in-process.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: unwrap/expect/panic allowed in test code

use std::process::Command;

fn workspace_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("crates/ggen-engine is two levels below the workspace root")
        .to_path_buf()
}

/// Positive control: the generated refusal compiles and renders the canonical
/// `REFUSED:<CODE> <detail>` wire form.
#[test]
fn positive_control_emit_renders_registry_code() {
    let r = ggen_engine::generated::refusals::Refusal::witness_stale("receipt predates shim");
    assert_eq!(r.code_str(), "WITNESS_STALE");
    assert_eq!(r.emit(), "REFUSED:WITNESS_STALE receipt predates shim");
    assert_eq!(r.detail(), "receipt predates shim");
}

/// Every registry code has a constructor and every `emit()` is code-exact.
#[test]
fn all_seven_registry_codes_emit_closed_vocabulary() {
    let cases = [
        (
            ggen_engine::generated::refusals::Refusal::envelope_undeclared("d"),
            "E-ENVELOPE-UNDECLARED",
        ),
        (
            ggen_engine::generated::refusals::Refusal::refused_stale_lease("d"),
            "REFUSED_STALE_LEASE",
        ),
        (
            ggen_engine::generated::refusals::Refusal::vacuous_court("d"),
            "VACUOUS_COURT",
        ),
        (
            ggen_engine::generated::refusals::Refusal::control_refused("d"),
            "CONTROL_REFUSED",
        ),
        (
            ggen_engine::generated::refusals::Refusal::restore_tamper("d"),
            "RESTORE_TAMPER",
        ),
        (
            ggen_engine::generated::refusals::Refusal::witness_stale("d"),
            "WITNESS_STALE",
        ),
        (
            ggen_engine::generated::refusals::Refusal::census_stale("d"),
            "CENSUS_STALE",
        ),
    ];
    assert_eq!(cases.len(), 7, "registry is closed at 7 codes (v26.10.20)");
    for (r, code) in cases {
        assert_eq!(r.emit(), format!("REFUSED:{code} d"), "code {code}");
    }
}

/// Negative control: the `compile_fail` doctests in both crates' generated
/// refusals modules must run and pass (a `compile_fail` block that stops
/// compiling-for-the-wrong-reason, or is silently skipped, fails this gate).
///
/// Both crates are driven in ONE cargo invocation
/// (`cargo test -p ggen-engine -p ggen-abb-sbb --doc refusals`) so the
/// doc-test compilation graph is built once, shared deps reused; per-crate
/// assertions split the combined output on cargo's `Doc-tests <pkg>` section
/// headers. A missing header for either crate = that crate's surface silently
/// skipped = gate failure.
#[test]
fn doc_test_compile_fail_surface_passes() {
    const CRATES: [&str; 2] = ["ggen-engine", "ggen-abb-sbb"];
    let out = Command::new("cargo")
        .args([
            "test",
            "-p",
            "ggen-engine",
            "-p",
            "ggen-abb-sbb",
            "--doc",
            "refusals",
        ])
        .current_dir(workspace_root())
        .output()
        .expect("spawn cargo test --doc");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "doc-test compile-fail surface failed:\n{stdout}\n--- stderr ---\n{}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Anti-vacuity, per crate: libtest labels every doctest result line with
    // the source path (`test crates/<pkg>/src/generated/refusals.rs - ...`),
    // so a crate whose compile-fail surface actually executed always yields
    // such lines; a silently skipped or fully-filtered crate yields none.
    // (Cargo's `Doc-tests <pkg>` headers are on stderr and are NOT used for
    // attribution — the stdout path labels are the per-crate ground truth.)
    for target in CRATES {
        let marker = format!("test crates/{target}/src/generated/refusals.rs");
        let ran = stdout.lines().filter(|l| l.starts_with(&marker)).count();
        assert!(
            ran > 0,
            "no doctests ran for {target} — vacuous gate:\n{stdout}"
        );
        let any_failed = stdout
            .lines()
            .any(|l| l.starts_with(&marker) && l.contains("FAILED"));
        assert!(!any_failed, "doctest FAILED for {target}:\n{stdout}");
    }
}
