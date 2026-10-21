#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Known-Answer Test for the court compiler: fixture catalog in →
//! byte-exact runner, register row, and fixture out.
//!
//! The expected artifacts are derived by literal placeholder substitution
//! over the committed templates — the KAT pins that rendering is exactly
//! that substitution and nothing more (no timestamps, no ambient state).

use std::fs;
use std::path::Path;

use ggen_engine::court::{compile_court_from_catalog_file, read_mutant_catalog, CourtVerdict};
use sha2::Digest as _;

const CATALOG_TTL: &str = r#"@prefix vc: <http://ggen.org/vacuity#> .
<http://ggen.org/vacuity#mutant-kd-c7> a vc:Mutant ;
    vc:gateId "KD-C7" ;
    vc:target "lib/gate.ex" ;
    vc:mutantKind "flip-condition" ;
    vc:shimPatch "SHIM-PATCH-BYTES\n" ;
    vc:expectedPostHash "deadbeef" ;
    vc:shimTs 1000 ;
    vc:witnessTs 2000 .
"#;

fn substitute(template: &str, pairs: &[(&str, &str)]) -> String {
    let mut out = template.to_string();
    for (placeholder, value) in pairs {
        out = out.replace(placeholder, value);
    }
    out
}

#[test]
fn fixture_catalog_compiles_byte_exact_artifacts() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let catalog_path = tmp.path().join("catalog.ttl");
    fs::write(&catalog_path, CATALOG_TTL).expect("write catalog");
    let out_dir = tmp.path().join("out");

    let manifest = compile_court_from_catalog_file(&catalog_path, &out_dir)
        .expect("court compiles from a well-formed catalog");
    assert_eq!(manifest.catalog_gate_id, "KD-C7");
    assert_eq!(
        manifest.verdict,
        CourtVerdict::Pass {
            witness_ts: 2000,
            shim_ts: 1000
        },
        "witness (2000) at/after shim (1000) authorizes the flip"
    );

    let pairs: &[(&str, &str)] = &[
        ("{{ gate_id }}", "KD-C7"),
        ("{{ target }}", "lib/gate.ex"),
        ("{{ mutant_kind }}", "flip-condition"),
        ("{{ expected_post_hash }}", "deadbeef"),
        ("{{ status }}", "PASS"),
        ("{{ code }}", ""),
        ("{{ detail }}", ""),
        ("{{ shim_ts }}", "1000"),
        ("{{ witness_ts }}", "2000"),
    ];
    let expected_runner = substitute(include_str!("../templates/court/runner.sh.tera"), pairs);
    let expected_row = substitute(
        include_str!("../templates/court/register_row.sh.tera"),
        pairs,
    );
    let expected_fixture = substitute(include_str!("../templates/court/fixture.rs.tera"), pairs);

    // Byte-exact, not contains: the register hashes what actually ran.
    assert_eq!(
        fs::read(out_dir.join("runner.sh")).expect("runner on disk"),
        expected_runner.as_bytes(),
        "runner.sh must be byte-exact"
    );
    assert_eq!(
        fs::read(out_dir.join("register_row.sh")).expect("row on disk"),
        expected_row.as_bytes(),
        "register_row.sh must be byte-exact"
    );
    assert_eq!(
        fs::read(out_dir.join("fixture.rs")).expect("fixture on disk"),
        expected_fixture.as_bytes(),
        "fixture.rs must be byte-exact"
    );
    assert_eq!(
        fs::read(out_dir.join("shim.patch")).expect("patch on disk"),
        b"SHIM-PATCH-BYTES\n" as &[u8],
        "shim.patch must carry the catalog patch verbatim"
    );

    // Manifest digests match the files actually written.
    for (name, sha) in &manifest.artifacts {
        let bytes = fs::read(out_dir.join(name)).expect("manifest artifact exists");
        let hexed = hex::encode(sha2::Sha256::digest(&bytes));
        assert_eq!(&hexed, sha, "{name} digest");
    }
}

#[test]
fn compile_is_deterministic_across_runs() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let catalog_path = tmp.path().join("catalog.ttl");
    fs::write(&catalog_path, CATALOG_TTL).expect("write catalog");

    let first = compile_court_from_catalog_file(&catalog_path, &tmp.path().join("a"))
        .expect("first compile");
    let second = compile_court_from_catalog_file(&catalog_path, &tmp.path().join("b"))
        .expect("second compile");
    assert_eq!(
        first, second,
        "compilation is deterministic: same catalog, same bytes"
    );
}

#[test]
fn malformed_catalog_is_typed_blocked_never_defaulted() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let catalog_path = tmp.path().join("catalog.ttl");
    // expectedPostHash missing entirely.
    fs::write(
        &catalog_path,
        CATALOG_TTL.replace(" vc:expectedPostHash \"deadbeef\" ;\n", ""),
    )
    .expect("write catalog");
    let err = compile_court_from_catalog_file(&catalog_path, &tmp.path().join("out"))
        .expect_err("missing required field must refuse");
    assert!(
        matches!(err, ggen_engine::court::CourtError::CatalogMalformed(_)),
        "typed CatalogMalformed, got {err:?}"
    );
    assert!(
        !Path::new(&tmp.path().join("out")).exists(),
        "a malformed catalog must not manufacture artifacts"
    );

    // The reader surface agrees.
    let err = read_mutant_catalog(&catalog_path).expect_err("reader refuses too");
    assert!(matches!(
        err,
        ggen_engine::court::CourtError::CatalogMalformed(_)
    ));
}
