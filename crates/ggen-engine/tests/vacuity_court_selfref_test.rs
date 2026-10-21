//! Vacuity-court self-referential tests (lane V14, v26.10.20).
//!
//! The court must classify its OWN inputs honestly across the full typed
//! verdict vocabulary, at both the library seam and the real `ggen court
//! compile` CLI boundary:
//!
//! 1. malformed catalog (missing `vc:target`) → `CatalogMalformed`; CLI
//!    exit 2 (`COURT_BLOCKED`);
//! 2. stale witness (`shim_ts > witness_ts`) → `Refused{WITNESS_STALE}`;
//!    CLI exit 1 (VACUOUS GATE) with "VACUOUS GATE" on stderr;
//! 3. missing timestamps → `Unknown`; CLI exit 3 (loud SKIP);
//! 4. fresh witness (`witness_ts >= shim_ts`) → `Pass`; CLI exit 0, all
//!    four artifacts on disk with sha256 digests matching the manifest;
//! 5. determinism KAT: the same catalog compiled twice yields byte-identical
//!    artifact digests.
//!
//! Consumes the landed A12 seam (`crates/ggen-engine/src/court/
//! {mod,catalog,compile}.rs`): `parse_mutant_catalog`/`read_mutant_catalog`
//! over the `vc:` Turtle namespace, `compile_court(catalog, out_dir) ->
//! CourtResult<CourtManifest>`, `flip_verdict` enforcing
//! `witness_ts >= shim_ts`.
//!
//! Chicago school: real tempdir files, real RDF parsing, real subprocess
//! `ggen` runs, real sha256 recomputation over files on disk. No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;
use std::process::Command;

use sha2::Digest as _;

use ggen_engine::court::{parse_mutant_catalog, read_mutant_catalog, CourtError, CourtVerdict};

/// Well-formed catalog with a fresh witness (2000 >= 1000) — the Pass case.
const FRESH_CATALOG: &str = r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#mutant-kd-c7> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "lib/gate.ex" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "SHIM-PATCH-BYTES\n" ;
    vc:expectedPostHash "deadbeef" ;
    vc:shimTs 1000 ;
    vc:witnessTs 2000 .
"#;

fn write_catalog(dir: &std::path::Path, name: &str, ttl: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    fs::write(&path, ttl).expect("write catalog ttl");
    path
}

/// Resolve the real `ggen` binary the same way `receipt_chain_e2e.rs` does:
/// `CARGO_BIN_EXE_ggen`, then workspace `target/{debug,release}/ggen`, then
/// fail loudly at resolution rather than at first spawn.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }
    let manifest_dir = std::path::PathBuf::from(
        std::env::var_os("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR"),
    );
    let mut dir: &std::path::Path = manifest_dir.as_path();
    let target_root = loop {
        if dir.join("Cargo.lock").exists() {
            break dir.join("target");
        }
        match dir.parent() {
            Some(p) => dir = p,
            None => panic!("could not resolve the `ggen` binary; build with `cargo build -p ggen-cli-lib --bin ggen`"),
        }
    };
    for profile in &["debug", "release"] {
        let candidate = target_root.join(profile).join("ggen");
        if candidate.is_file() {
            return candidate;
        }
    }
    panic!(
        "could not resolve the `ggen` binary; build with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Run the real CLI: `ggen court compile --catalog <path> --out <dir>`.
fn run_cli(catalog: &std::path::Path, out: &std::path::Path) -> std::process::Output {
    Command::new(ggen_bin())
        .args([
            "court",
            "compile",
            "--catalog",
            &catalog.to_string_lossy(),
            "--out",
            &out.to_string_lossy(),
        ])
        .output()
        .expect("ggen court compile spawns")
}

// ---------------------------------------------------------------------------
// 1. Malformed catalog → CatalogMalformed, CLI exit 2 (COURT_BLOCKED)
// ---------------------------------------------------------------------------

#[test]
fn malformed_catalog_missing_target_is_catalog_malformed() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    // vc:target removed entirely — a required field is never defaulted.
    let malformed = FRESH_CATALOG.replace("    vc:target \"lib/gate.ex\" ;\n", "");
    assert!(
        !malformed.contains("vc:target"),
        "premise: the replacement actually removed vc:target"
    );

    let err = parse_mutant_catalog(&malformed).expect_err("missing vc:target must refuse");
    assert!(
        matches!(err, CourtError::CatalogMalformed(_)),
        "typed CatalogMalformed, got {err:?}"
    );

    // The file-reader seam agrees.
    let catalog_path = write_catalog(tmp.path(), "malformed.ttl", &malformed);
    let err = read_mutant_catalog(&catalog_path).expect_err("reader refuses too");
    assert!(matches!(err, CourtError::CatalogMalformed(_)), "{err:?}");
}

#[test]
fn malformed_catalog_cli_exits_2_court_blocked() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let malformed = FRESH_CATALOG.replace("    vc:target \"lib/gate.ex\" ;\n", "");
    let catalog_path = write_catalog(tmp.path(), "malformed.ttl", &malformed);
    let out = tmp.path().join("out");

    let output = run_cli(&catalog_path, &out);
    assert_eq!(
        output.status.code(),
        Some(2),
        "malformed catalog must exit COURT_BLOCKED(2), got {:?}: stdout={} stderr={}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("COURT_BLOCKED"),
        "stderr names the COURT_BLOCKED class"
    );
    assert!(
        !out.exists(),
        "a malformed catalog must not manufacture artifacts"
    );
}

// ---------------------------------------------------------------------------
// 2. Stale witness → Refused{WITNESS_STALE}, CLI exit 1 (VACUOUS GATE)
// ---------------------------------------------------------------------------

#[test]
fn stale_witness_is_refused_witness_stale() {
    // shim_ts (2000) is AFTER witness_ts (1000): the receipt predates the
    // shim it claims to witness — never an authorized flip.
    let stale = FRESH_CATALOG
        .replace("vc:shimTs 1000", "vc:shimTs 2000")
        .replace("vc:witnessTs 2000", "vc:witnessTs 1000");
    let catalog = parse_mutant_catalog(&stale).expect("stale catalog still well-formed");

    let verdict = ggen_engine::court::flip_verdict(&catalog);
    match &verdict {
        CourtVerdict::Refused {
            code,
            target,
            detail,
        } => {
            assert_eq!(code, "WITNESS_STALE");
            assert_eq!(target.as_deref(), Some("lib/gate.ex"));
            assert!(detail.contains("1000") && detail.contains("2000"));
        }
        other => panic!("expected Refused{{WITNESS_STALE}}, got {other:?}"),
    }
    assert_eq!(
        verdict.exit_code(),
        ggen_engine::court::EXIT_VACUOUS,
        "WITNESS_STALE maps to the VACUOUS GATE exit"
    );
}

#[test]
fn stale_witness_cli_exits_1_vacuous_gate_on_stderr() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let stale = FRESH_CATALOG
        .replace("vc:shimTs 1000", "vc:shimTs 2000")
        .replace("vc:witnessTs 2000", "vc:witnessTs 1000");
    let catalog_path = write_catalog(tmp.path(), "stale.ttl", &stale);
    let out = tmp.path().join("out");

    let output = run_cli(&catalog_path, &out);
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert_eq!(
        output.status.code(),
        Some(1),
        "stale witness must exit VACUOUS(1), got {:?}: stderr={stderr}",
        output.status.code(),
    );
    assert!(
        stderr.contains("VACUOUS GATE"),
        "stderr must carry VACUOUS GATE, got: {stderr}"
    );
    assert!(
        stderr.contains("WITNESS_STALE"),
        "stderr must name the typed refusal code, got: {stderr}"
    );
    // Artifacts are still compiled (the refusal is compiled in) — but the
    // flip is refused, so the runner must not carry a PASS status.
    let runner = fs::read_to_string(out.join("runner.sh")).expect("runner compiled");
    assert!(
        runner.contains("STATUS=\"REFUSED\""),
        "a refused court compiles the refusal guard into its runner"
    );
}

// ---------------------------------------------------------------------------
// 3. Missing timestamps → Unknown, CLI exit 3 (loud SKIP)
// ---------------------------------------------------------------------------

#[test]
fn missing_timestamps_is_unknown_loud_skip() {
    let silent = FRESH_CATALOG
        .replace("    vc:shimTs 1000 ;\n", "")
        .replace("    vc:witnessTs 2000 .\n", " .\n");
    let catalog = parse_mutant_catalog(&silent).expect("catalog well-formed sans timestamps");
    assert_eq!(catalog.shim_ts, None);
    assert_eq!(catalog.witness_ts, None);

    let verdict = ggen_engine::court::flip_verdict(&catalog);
    assert!(
        matches!(verdict, CourtVerdict::Unknown { .. }),
        "missing timestamps must be Unknown, got {verdict:?}"
    );
    assert_eq!(
        verdict.exit_code(),
        ggen_engine::court::EXIT_SKIP,
        "Unknown maps to the loud-SKIP exit"
    );
}

#[test]
fn missing_timestamps_cli_exits_3_loud_skip() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let silent = FRESH_CATALOG
        .replace("    vc:shimTs 1000 ;\n", "")
        .replace("    vc:witnessTs 2000 .\n", " .\n");
    let catalog_path = write_catalog(tmp.path(), "silent.ttl", &silent);
    let out = tmp.path().join("out");

    let output = run_cli(&catalog_path, &out);
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert_eq!(
        output.status.code(),
        Some(3),
        "unverifiable flip must exit loud SKIP(3), got {:?}: stderr={stderr}",
        output.status.code(),
    );
    assert!(
        !stderr.trim().is_empty(),
        "a SKIP is loud: stderr must be non-empty (never a silent pass)"
    );
}

// ---------------------------------------------------------------------------
// 4. Fresh witness → Pass, CLI exit 0, artifacts with matching sha256s
// ---------------------------------------------------------------------------

#[test]
fn fresh_witness_is_pass() {
    let catalog = parse_mutant_catalog(FRESH_CATALOG).expect("well-formed catalog");
    let verdict = ggen_engine::court::flip_verdict(&catalog);
    assert_eq!(
        verdict,
        CourtVerdict::Pass {
            witness_ts: 2000,
            shim_ts: 1000
        },
        "witness (2000) at/after shim (1000) authorizes the flip"
    );
    assert_eq!(verdict.exit_code(), 0);
}

#[test]
fn fresh_witness_cli_exits_0_with_artifacts_and_matching_sha256s() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let catalog_path = write_catalog(tmp.path(), "fresh.ttl", FRESH_CATALOG);
    let out = tmp.path().join("out");

    let output = run_cli(&catalog_path, &out);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    assert_eq!(
        output.status.code(),
        Some(0),
        "fresh witness must pass, got {:?}: stdout={stdout} stderr={}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(stdout.contains("PASS"), "stdout carries the PASS status");

    // All four artifacts exist on disk.
    for name in ["runner.sh", "register_row.sh", "fixture.rs", "shim.patch"] {
        assert!(out.join(name).is_file(), "artifact {name} must be emitted");
    }

    // stdout carries per-artifact sha256s; recompute each from the disk
    // bytes and require an exact match against what the court reported.
    for name in ["runner.sh", "register_row.sh", "fixture.rs", "shim.patch"] {
        let bytes = fs::read(out.join(name)).expect("artifact bytes");
        let digest: String = hex::encode(sha2::Sha256::digest(&bytes));
        assert!(
            stdout.contains(&digest),
            "stdout must report the real sha256 of {name} ({digest})"
        );
    }
}

// ---------------------------------------------------------------------------
// 5. Determinism KAT: same catalog twice → byte-identical artifact digests
// ---------------------------------------------------------------------------

#[test]
fn same_catalog_twice_yields_byte_identical_manifest_digests() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let catalog_path = write_catalog(tmp.path(), "kat.ttl", FRESH_CATALOG);

    let first = ggen_engine::court::compile_court_from_catalog_file(
        &catalog_path,
        &tmp.path().join("run-a"),
    )
    .expect("first compile");
    let second = ggen_engine::court::compile_court_from_catalog_file(
        &catalog_path,
        &tmp.path().join("run-b"),
    )
    .expect("second compile");

    assert_eq!(
        first, second,
        "compilation is deterministic: same catalog, same digests"
    );
    // And the digests describe real, identical bytes in both output dirs.
    for (name, sha) in &first.artifacts {
        let a = fs::read(tmp.path().join("run-a").join(name)).expect("run-a artifact");
        let b = fs::read(tmp.path().join("run-b").join(name)).expect("run-b artifact");
        assert_eq!(a, b, "{name} bytes identical across runs");
        let digest: String = hex::encode(sha2::Sha256::digest(&a));
        assert_eq!(&digest, sha, "{name} manifest digest matches disk bytes");
    }
}
