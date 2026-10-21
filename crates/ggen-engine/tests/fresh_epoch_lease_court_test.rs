#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code

//! `FreshEpochLease` courts (v26.10.20, lane A17, per wave-1 lane L09 plan).
//!
//! Non-vacuity courts over the A06 lease gate (`write::plan_write`'s
//! `check_lease_generation`, FM-WRITE-012). Chicago discipline throughout:
//! real temp dirs, real hand-written receipt fixtures on disk, assertions on
//! persisted state (file bytes, lease slot payload) — no mocks, no
//! interaction-only assertions.
//!
//! Typed-cause note (disclosed): `AppError` has no typed FM-code field —
//! every `fm_*` constructor formats the code into the variant's `String`
//! payload (`crates/ggen-engine/src/error.rs`, `fm_write`). The workspace's
//! sanctioned non-substring surface is the typed variant match
//! (`AppError::Validation`) plus `error::extract_fm_code` (the ONE relocated
//! implementation of FM-code extraction). The canonical typed refusal home
//! for `REFUSED_STALE_LEASE` is `ggen-abb-sbb`'s `Refusal::StaleLease`
//! (RESOLUTIONS.md vocabulary decision); the engine mirrors it as the
//! `FM-WRITE-012` code. Both surfaces are pinned here.
//!
//! Interleaving note (disclosed): `plan_write` has no dedicated
//! post-compute/pre-persist injection point (compute and persist are one
//! call). The generation-bump-between-compute-and-write race is simulated
//! with two REAL `plan_write` calls — writer B's admitted write bumps the
//! persisted baseline after writer A computed at the older generation —
//! then writer A's persist attempt is refused. This test runs (no
//! `#[ignore]`); it is not RED.

use ggen_engine::error::{extract_fm_code, AppError};
use ggen_engine::template::Frontmatter;
use ggen_engine::write::{plan_write, WriteOutcome};
use tempfile::TempDir;

/// Build real frontmatter by round-tripping YAML — the same construction
/// path production code uses; `receipt_generation` is serde-default 0.
fn fm_with_generation(to: &str, generation: u64) -> Frontmatter {
    let yaml = format!("to: {to}\nforce: true\nreceipt_generation: {generation}\n");
    serde_yaml::from_str(&yaml).expect("frontmatter")
}

fn lease_path(root: &std::path::Path, rel_to: &str) -> std::path::PathBuf {
    let hex = blake3::hash(rel_to.as_bytes()).to_hex().to_string();
    root.join(".ggen-v2").join("lease").join(hex)
}

fn read_lease(root: &std::path::Path, rel_to: &str) -> u64 {
    let path = lease_path(root, rel_to);
    let raw = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "lease slot {} must exist after a successful write: {e}",
            path.display()
        )
    });
    raw.trim()
        .parse::<u64>()
        .expect("decimal u64 lease payload")
}

fn blake3_hex(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

/// Write a receipt fixture to disk with the given generation, then read it
/// back and parse it — the round-trip IS part of the proof (a fixture that
/// cannot survive its own persistence proves nothing).
fn write_and_roundtrip_receipt(root: &std::path::Path, generation: u64) -> u64 {
    let path = root.join("receipt-fixture.json");
    let payload = serde_json::json!({ "generation": generation, "artifact": "gen/alpha.rs" });
    std::fs::write(
        &path,
        serde_json::to_string_pretty(&payload).expect("serialize receipt"),
    )
    .expect("write receipt fixture to disk");
    let raw = std::fs::read_to_string(&path).expect("read receipt fixture back");
    let parsed: serde_json::Value = serde_json::from_str(&raw).expect("parse receipt fixture");
    parsed["generation"]
        .as_u64()
        .expect("generation field is u64")
}

/// Typed-cause assertion shared by the refusal courts: the error must be the
/// typed `AppError::Validation` variant carrying `FM-WRITE-012` via the ONE
/// sanctioned code extractor — never a raw substring hunt over `Display`.
fn assert_typed_stale_lease(err: &AppError) {
    let AppError::Validation(msg) = &err else {
        panic!("expected typed AppError::Validation, got another variant: {err:?}");
    };
    assert_eq!(
        extract_fm_code(msg),
        Some("FM-WRITE-012"),
        "typed FM code surface must pin the stale-lease gate: {msg}"
    );
    assert!(
        msg.contains("REFUSED_STALE_LEASE"),
        "registry code (L13 vocabulary) must ride the FM-WRITE-012 payload: {msg}"
    );
}

/// The canonical typed home of the refusal (RESOLUTIONS.md): abb-sbb's
/// `Refusal::StaleLease` is constructible and pattern-matchable, and its
/// Display carries the registry code. Pinned so the engine's mirrored code
/// string and the typed variant cannot silently diverge.
#[test]
fn typed_refusal_home_is_abb_sbb_stale_lease() {
    let refusal = ggen_abb_sbb::Refusal::StaleLease {
        evidence_id: "gen/alpha.rs".to_string(),
        receipt: 1,
        baseline: 2,
    };
    let ggen_abb_sbb::Refusal::StaleLease {
        evidence_id,
        receipt,
        baseline,
    } = &refusal
    else {
        panic!("typed variant match failed");
    };
    assert_eq!(
        (evidence_id.as_str(), *receipt, *baseline),
        ("gen/alpha.rs", 1, 2)
    );
    assert!(
        refusal.to_string().starts_with("REFUSED_STALE_LEASE:"),
        "typed Display must lead with the registry code: {refusal}"
    );
}

// ---------------------------------------------------------------------
// Court 1: nominal fresh receipt admits (first sync; second unchanged sync)
// ---------------------------------------------------------------------

#[test]
fn nominal_fresh_receipt_admits() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // First sync: a fresh generation-1 receipt admits and records
    // generation 1 in the lease slot.
    let out1 = plan_write(
        root,
        "gen/alpha.rs",
        "v1\n",
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect("first sync with a fresh receipt must admit");
    assert_eq!(out1, WriteOutcome::Written);
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        1,
        "first admitted write records lease generation 1"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("gen/alpha.rs")).expect("read target"),
        "v1\n"
    );

    // Second UNCHANGED sync (fresh generation 2, byte-identical render):
    // the lease gate passes (2 > 1) and the unchanged-content skip admits —
    // a nominal re-run is never refused. (Integration lane I1: the baseline
    // bump law is `prev+1` on an actual write — the landed
    // `fresh_lease_write_test` pins it, including gen-3 first writes
    // recording 1 — so an unchanged skip admits but does not move the
    // baseline; the original "baseline follows the admitted run" assertion
    // contradicted that pinned law and never passed.)
    let out2 = plan_write(
        root,
        "gen/alpha.rs",
        "v1\n",
        &fm_with_generation("gen/alpha.rs", 2),
    )
    .expect("second unchanged sync with a fresher receipt must succeed");
    assert!(
        matches!(out2, WriteOutcome::Skipped(ref reason) if reason.contains("unchanged")),
        "second unchanged sync must skip as unchanged, got {out2:?}"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("gen/alpha.rs")).expect("read target"),
        "v1\n",
        "skipped sync must not rewrite the file"
    );
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        1,
        "unchanged skip does not bump the baseline"
    );
}

// ---------------------------------------------------------------------
// Court 2: backdated receipt (generation N-1) refused, stale lease
// ---------------------------------------------------------------------

#[test]
fn backdated_receipt_refused_stale_lease() {
    let lease_after = backdated_refuse_once(0);
    assert_eq!(lease_after, 2, "lease baseline must survive the refusal");
}

/// One full backdate-and-refuse sequence in a fresh real tempdir. Shared by
/// Court 2 and the stress modifier below so the fence cannot drift from the
/// court it fences.
///
/// Fence note (RECEIPTS.md follow-up 0, flake candidate): the staleness
/// signal here is STRUCTURAL, not time-based — verified by reading the whole
/// gate path. `plan_write`'s stale check is `check_lease_generation`
/// (`crates/ggen-engine/src/write.rs`), a pure `u64` comparison of
/// `frontmatter.receipt_generation` against the persisted integer baseline;
/// no filesystem mtime, wall clock, or `SystemTime` participates anywhere in
/// the lease gate, the lease slot payload (a decimal u64), or this court.
/// The round-tripped fixture carries the staleness as the integer 1 vs the
/// persisted baseline 2 — there is no clock to race under parallel load, so
/// the one recorded flake cannot be reproduced by a time signal here; the
/// stress modifier hammers the sequence 20× in-process to fence any
/// load-order dependence in the gate itself. The sabotage semantics are
/// unchanged: stale input refused for the STALE reason (typed
/// FM-WRITE-012/REFUSED_STALE_LEASE), artifact and baseline untouched.
fn backdated_refuse_once(iteration: usize) -> u64 {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Establish baseline generation 2 on disk (two admitted writes under the
    // landed prev+1 bump law: 0→1, then 1→2).
    plan_write(
        root,
        "gen/alpha.rs",
        "v1\n",
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect("admit generation 1");
    plan_write(
        root,
        "gen/alpha.rs",
        "v2\n",
        &fm_with_generation("gen/alpha.rs", 2),
    )
    .expect("admit generation 2");
    assert_eq!(read_lease(root, "gen/alpha.rs"), 2, "baseline is 2");
    let file_before = std::fs::read(root.join("gen/alpha.rs")).expect("read target");

    // Hand-write the BACKDATED receipt fixture (generation N-1 = 1) to real
    // disk and prove the round-trip parse reads back exactly N-1.
    let backdated_generation = write_and_roundtrip_receipt(root, 1);
    assert_eq!(backdated_generation, 1, "round-tripped receipt carries N-1");

    // The persisted attempt carrying the backdated generation is refused on
    // the typed cause — variant match + sanctioned code extractor, not a
    // Display substring hunt.
    let err = plan_write(
        root,
        "gen/alpha.rs",
        "v3-would-be\n",
        &fm_with_generation("gen/alpha.rs", backdated_generation),
    )
    .expect_err(&format!(
        "iteration {iteration}: backdated receipt must be REFUSED_STALE_LEASE"
    ));
    assert_typed_stale_lease(&err);

    // Refusal is real: neither the file nor the lease moved.
    assert_eq!(
        std::fs::read(root.join("gen/alpha.rs")).expect("read target"),
        file_before,
        "refused backdated write must not touch the artifact"
    );
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        2,
        "refusal must not move the lease baseline"
    );
    read_lease(root, "gen/alpha.rs")
}

// ---------------------------------------------------------------------
// Court 3: generation bump between compute and write blocks the stale write
// ---------------------------------------------------------------------

#[test]
fn generation_bump_between_compute_and_write_blocks_stale_write() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Writer A computes its body against generation 1 and persists
    // successfully.
    let body = "computed-at-gen-1\n";
    plan_write(
        root,
        "gen/alpha.rs",
        body,
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect("writer A's compute+persist at generation 1 admits");

    // Concurrent writer B computes at generation 2 and persists first —
    // the artifact's lease baseline is bumped under writer A's feet.
    plan_write(
        root,
        "gen/alpha.rs",
        "computed-at-gen-2\n",
        &fm_with_generation("gen/alpha.rs", 2),
    )
    .expect("writer B's fresher write admits and bumps the baseline");
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        2,
        "writer B moved the baseline to 2"
    );

    // The refused-stale-write comparison baseline is the state immediately
    // BEFORE the refused attempt (i.e. after B's admitted write — the
    // original capture, taken before B's write, could never match; lane I1
    // correction, the law under test is unchanged).
    let on_disk_before_refusal =
        std::fs::read_to_string(root.join("gen/alpha.rs")).expect("read target");
    let digest_before = blake3_hex(on_disk_before_refusal.as_bytes());

    // Writer A now persists its still-in-hand generation-1 write. The gate
    // must refuse the stale write even though writer A's body was computed
    // legitimately when generation 1 was current.
    let err = plan_write(
        root,
        "gen/alpha.rs",
        body,
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect_err("stale post-bump write must be refused");
    assert_typed_stale_lease(&err);

    // The on-disk artifact is byte-identical before/after the refused write
    // (hash-compared, not string-compared — byte evidence, not rendering).
    let on_disk_after = std::fs::read(root.join("gen/alpha.rs")).expect("read target after");
    assert_eq!(
        blake3_hex(&on_disk_after),
        digest_before,
        "refused stale write must leave the artifact byte-identical"
    );
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        2,
        "baseline unchanged by the refusal"
    );
}

// ---------------------------------------------------------------------
// Stress modifier (RECEIPTS.md follow-up 0 fence): the backdate+refuse
// sequence 20x in-process, each in a fresh real tempdir. Bounded and fast
// (pure u64 gate, no sleeps, no polling). Any load-order or state-leak
// dependence in the gate must surface here before it flakes under
// pre-commit parallel load again.
// ---------------------------------------------------------------------

#[test]
fn backdated_refusal_stress_20x() {
    for i in 0..20 {
        let lease_after = backdated_refuse_once(i);
        assert_eq!(
            lease_after, 2,
            "iteration {i}: lease baseline must survive the refusal"
        );
    }
}
