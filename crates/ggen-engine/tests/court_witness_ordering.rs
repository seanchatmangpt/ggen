//! authorize the KD-COVERS flip — the compiled runner exits 1 (VACUOUS
//! GATE, loud) and the register row's compiled guard refuses.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: unwrap/expect/panic allowed in test code

use std::fs;
use std::process::Command;

use ggen_engine::court::{compile_court, parse_mutant_catalog, CourtVerdict};

fn catalog_with_timestamps(witness_ts: u64, shim_ts: u64) -> ggen_engine::court::MutantCatalog {
    parse_mutant_catalog(&format!(
        r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#m> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "lib/gate.ex" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "x\n" ;
    vc:expectedPostHash "abc" ;
    vc:shimTs {shim_ts} ;
    vc:witnessTs {witness_ts} .
"#
    ))
    .expect("catalog parses")
}

#[test]
fn witness_predating_shim_refuses_flip() {
    let verdict = ggen_engine::court::flip_verdict(&catalog_with_timestamps(500, 1000));
    assert_eq!(
        verdict,
        CourtVerdict::Refused {
            code: "WITNESS_STALE".to_string(),
            target: Some("lib/gate.ex".to_string()),
            detail: "witness_ts (500) predates shim_ts (1000) — a witness receipt older \
                     than the shim it witnesses can never authorize the KD-COVERS flip"
                .to_string(),
        },
        "a stale witness can never flip"
    );
    assert_eq!(
        verdict.exit_code(),
        1,
        "WITNESS_STALE surfaces as VACUOUS GATE exit 1"
    );
}

#[test]
fn witness_at_or_after_shim_authorizes_flip() {
    let verdict = ggen_engine::court::flip_verdict(&catalog_with_timestamps(1000, 1000));
    assert_eq!(
        verdict,
        CourtVerdict::Pass {
            witness_ts: 1000,
            shim_ts: 1000
        },
        "witness == shim_ts authorizes (at-or-after)"
    );

    let verdict = ggen_engine::court::flip_verdict(&catalog_with_timestamps(2000, 1000));
    assert_eq!(
        verdict,
        CourtVerdict::Pass {
            witness_ts: 2000,
            shim_ts: 1000
        }
    );
}

#[test]
fn missing_timestamps_is_unknown_loud_skip() {
    let verdict = ggen_engine::court::flip_verdict(&catalog_with_timestamps_only_shim());
    assert!(
        matches!(verdict, CourtVerdict::Unknown { .. }),
        "unverifiable flip is Unknown, got {verdict:?}"
    );
    assert_eq!(
        verdict.exit_code(),
        3,
        "Unknown surfaces as loud SKIP exit 3"
    );
}

fn catalog_with_timestamps_only_shim() -> ggen_engine::court::MutantCatalog {
    parse_mutant_catalog(
        r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#m> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "lib/gate.ex" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "x\n" ;
    vc:expectedPostHash "abc" ;
    vc:shimTs 1000 .
"#,
    )
    .expect("catalog parses")
}

/// End-to-end over the real compiled runner: the stale-witness refusal is
/// compiled INTO the artifacts, so `bash runner.sh` exits 1 without even
/// needing `GATE_CMD` — the flip is refused before any mutation could run.
#[test]
fn compiled_runner_refuses_stale_witness_with_exit_1() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path();
    let out_dir = root.join("out");
    let manifest = compile_court(&catalog_with_timestamps(500, 1000), &out_dir)
        .expect("stale-witness court still compiles artifacts (with the refusal baked in)");
    assert!(matches!(manifest.verdict, CourtVerdict::Refused { .. }));

    let row = fs::read_to_string(out_dir.join("register_row.sh")).expect("row on disk");
    assert!(
        row.contains("COURT_STATUS=\"REFUSED\""),
        "row carries the refusal: {row}"
    );
    assert!(row.contains("WITNESS_STALE"), "row names the typed code");

    let runner = fs::read_to_string(out_dir.join("runner.sh")).expect("runner on disk");
    assert!(runner.contains("VACUOUS GATE (WITNESS_STALE)"));

    // No GATE_CMD set: the refusal must fire before the GATE_CMD check.
    let output = Command::new("bash")
        .arg(out_dir.join("runner.sh"))
        .current_dir(root)
        .output()
        .expect("runner runs");
    assert_eq!(
        output.status.code(),
        Some(1),
        "stale witness = VACUOUS GATE exit 1, got {:?}: {} {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("WITNESS_STALE"),
        "loud, typed refusal on stderr"
    );
}

/// The register row's compiled guard enforces `witness_ts` >= `shim_ts`
/// structurally: sourcing the row and calling the guard refuses with 1.
#[test]
fn register_row_guard_refuses_stale_witness() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let out_dir = tmp.path().join("out");
    compile_court(&catalog_with_timestamps(500, 1000), &out_dir).expect("compiles");

    let script = tmp.path().join("check.sh");
    fs::write(
        &script,
        format!(
            "source {}/register_row.sh\ncourt_register_guard\n",
            out_dir.display()
        ),
    )
    .expect("write check script");

    let output = Command::new("bash")
        .arg(&script)
        .output()
        .expect("guard runs");
    assert_eq!(
        output.status.code(),
        Some(1),
        "guard refuses the stale flip"
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("flip refused"),
        "loud refusal"
    );
}
