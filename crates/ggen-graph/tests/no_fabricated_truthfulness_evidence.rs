#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Regression guard for the "self-audit log is fabricated evidence" bug.
//!
//! `ggen_graph::ocel::self_audit::generate_self_audit_log` builds a deterministic, hand-authored
//! `OcelLog` with fields that *look* like real observations (a `CommandRun.exit_code`, an
//! `EvidenceArtifact.sha256`, `CoverageMatrix.line_coverage`/`branch_coverage`, and
//! `TestPassed`/`CheckpointPromoted`/etc. event timestamps) but are actually hardcoded literal
//! constants -- the sha256 is literally `sha256("test")`, the exit code was never produced by
//! running anything, the coverage numbers were never measured, and the timestamps are
//! compile-time-fixed, not read from a clock at event time. A prior version of this crate wrote
//! that fabricated log to `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` (via a
//! `src/bin/emit_audit.rs` binary) and then had a "truthfulness adjudicator" script
//! (`scripts/gall/external/99_adjudicate_truthfulness.sh`, transitively also
//! `99_adjudicate_witnessed_truthfulness.sh` via `09_verify_ocel_self_audit.sh`) read that file
//! back and compute a Promoted/Refused verdict partly from it -- i.e. a real-looking
//! truthfulness gate was, in part, checking that a hardcoded fake-data generator produced
//! internally self-consistent fake data, never that any command actually ran, any test actually
//! passed, or any coverage was actually measured.
//!
//! This test asserts that fabrication-to-evidence pipeline stays severed:
//! - the binaries whose sole job was writing/"verifying" the fabricated log as if it were real
//!   evidence (`emit_audit.rs`, `verify_audit.rs`) do not exist;
//! - the wrapper/ring script that regenerated and structurally re-checked that log
//!   (`09_verify_ocel_self_audit.sh`, in both its `scripts/gall/` and `scripts/gall/external/`
//!   locations) does not exist;
//! - the top-level adjudicator scripts no longer read the fabricated OCEL file as evidence
//!   (no `OCEL_FILE=".../vision2030.self_audit.ocel.json"` assignment) and no longer invoke the
//!   deleted ring script;
//! - `generate_self_audit_log`'s own module docs still carry the "fixture data, not real
//!   evidence" warning, so a future change can't silently repurpose it as an evidence source
//!   without at least deleting that warning first.
//!
//! `generate_self_audit_log` itself is untouched and continues to be used as fixture input for
//! the real OCEL round-trip-projection tests (`ocel::self_audit::tests` in this crate's `lib.rs`
//! and `tests/ocel_self_audit.rs`) -- that usage is legitimate Chicago TDD (real
//! `DeterministicGraph`, real SPARQL queries, fixture input data) and is not what this test
//! guards against.

use std::error::Error;
use std::fs;
use std::path::Path;

fn repo_root() -> &'static Path {
    // crates/ggen-graph/tests -> crates/ggen-graph -> crates -> repo root
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("CARGO_MANIFEST_DIR should be crates/ggen-graph under the repo root")
}

#[test]
fn test_fabricated_evidence_binaries_and_scripts_are_gone() {
    let root = repo_root();

    let must_not_exist = [
        "crates/ggen-graph/src/bin/emit_audit.rs",
        "crates/ggen-graph/src/bin/verify_audit.rs",
        "scripts/gall/external/09_verify_ocel_self_audit.sh",
        "scripts/gall/emit_ocel_self_audit.sh",
        "scripts/gall/verify_ocel_self_audit.sh",
    ];

    for rel in must_not_exist {
        let path = root.join(rel);
        assert!(
            !path.exists(),
            "{} must not exist: it delivered/verified ggen_graph::ocel::self_audit::generate_self_audit_log's fixture data as if it were real truthfulness evidence (fake exit_code, sha256(\"test\"), fabricated coverage, fixed timestamps). If this is intentionally being reintroduced, it must derive every field from a real, actually-executed observation -- not hardcoded literals.",
            rel
        );
    }
}

#[test]
fn test_adjudicators_no_longer_trust_fabricated_ocel_file_as_evidence() -> Result<(), Box<dyn Error>>
{
    let root = repo_root();

    // These scripts are still part of the truthfulness/promotion adjudication rings; they must
    // no longer treat crates/ggen-graph/audit/vision2030.self_audit.ocel.json as evidence.
    let adjudicators = [
        "scripts/gall/external/99_adjudicate_truthfulness.sh",
        "scripts/gall/external/99_adjudicate_witnessed_truthfulness.sh",
        "scripts/gall/external/13_adjudicate_gall_promotion.sh",
    ];

    let mut violations = Vec::new();

    for rel in adjudicators {
        let path = root.join(rel);
        let content = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("expected {} to exist and be readable: {}", rel, e));

        // A live bash assignment binding OCEL_FILE to the fabricated log would mean this
        // adjudicator is reading it back in as evidence again.
        if content.contains("OCEL_FILE=\"crates/ggen-graph/audit/vision2030.self_audit.ocel.json\"")
        {
            violations.push(format!(
                "{} still assigns OCEL_FILE to the fabricated self-audit log",
                rel
            ));
        }

        // A quoted array-entry reference (matching how live SCRIPTS array entries are written
        // elsewhere in these files) to the deleted ring script would mean this adjudicator still
        // tries to invoke it.
        if content.contains("\"scripts/gall/external/09_verify_ocel_self_audit.sh\"") {
            violations.push(format!(
                "{} still references \"scripts/gall/external/09_verify_ocel_self_audit.sh\" as a live array entry",
                rel
            ));
        }
    }

    assert!(
        violations.is_empty(),
        "Truthfulness/promotion adjudicators still trust fabricated self-audit data:\n{}",
        violations.join("\n")
    );

    Ok(())
}

#[test]
fn test_self_audit_module_still_documents_fixture_only_role() -> Result<(), Box<dyn Error>> {
    let root = repo_root();
    let path = root.join("crates/ggen-graph/src/ocel/self_audit.rs");
    let content = fs::read_to_string(&path)?;

    assert!(
        content.contains("Fixture data, not real evidence"),
        "crates/ggen-graph/src/ocel/self_audit.rs must keep documenting that \
         generate_self_audit_log returns fixture data, not real evidence -- this is the guard \
         against a future change re-wiring its output into a truthfulness/compliance gate \
         without deriving the fields from real observations first."
    );

    Ok(())
}

#[test]
fn test_coverage_matrix_no_longer_documents_deleted_verify_commands() -> Result<(), Box<dyn Error>>
{
    let root = repo_root();
    let path = root.join("crates/ggen-graph/src/ocel/coverage.rs");
    let content = fs::read_to_string(&path)?;

    // Match the *active* Rust string-literal form (`"...".to_string()`), not a `//` comment
    // that merely explains, in prose, what used to be there -- this file's own historical notes
    // legitimately mention these dead commands as explanation.
    assert!(
        !content.contains("\"cargo run -p ggen-graph --bin emit_audit\".to_string()"),
        "coverage.rs must not document \"cargo run -p ggen-graph --bin emit_audit\" as a live \
         command to verify a requirement: that binary was deleted."
    );
    assert!(
        !content.contains("\"cargo run -p ggen-graph --bin verify_audit\".to_string()"),
        "coverage.rs must not document \"cargo run -p ggen-graph --bin verify_audit\" as a live \
         command to verify a requirement: that binary was deleted."
    );
    assert!(
        !content.contains("crates/ggen-graph/src/bin/verify_audit.rs"),
        "coverage.rs must not list the deleted crates/ggen-graph/src/bin/verify_audit.rs as a \
         requirement source file -- scripts/gall/external/10_verify_coverage_matrix.sh checks \
         that every listed source_file actually exists on disk."
    );

    Ok(())
}
