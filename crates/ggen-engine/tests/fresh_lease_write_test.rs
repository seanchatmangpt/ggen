//! `FreshEpochLease` persistence (v26.10.20, lanes A06/L08).
//!
//! Exercises `write::plan_write`'s lease gate and slot IO against the real
//! filesystem: lease slots under `<root>/.ggen-v2/lease/<blake3-hex(rel_to)>`,
//! stale-epoch refusal (`FM-WRITE-012` / `REFUSED_STALE_LEASE`), migration of
//! a missing slot, and fail-closed behavior on a corrupt slot. Chicago style:
//! real temp dirs, real files, assertions on persisted state.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code

use ggen_engine::template::Frontmatter;
use ggen_engine::write::{plan_write, WriteOutcome};
use tempfile::TempDir;

/// Build real frontmatter by round-tripping YAML — the same construction path
/// production code uses (Frontmatter has no struct-literal constructor in
/// tests here; `receipt_generation` is serde-default 0 unless overridden).
fn fm(to: &str) -> Frontmatter {
    let yaml = format!("to: {to}\nforce: true\n");
    serde_yaml::from_str(&yaml).expect("frontmatter")
}

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

#[test]
fn stale_generation_is_refused_before_any_write() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Establish baseline generation 1 with a first successful write.
    let out1 = plan_write(
        root,
        "gen/alpha.rs",
        "v1\n",
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect("first write admits");
    assert_eq!(out1, WriteOutcome::Written);
    assert_eq!(read_lease(root, "gen/alpha.rs"), 1);

    // A replayed generation-1 receipt (not strictly fresher than baseline 1)
    // is refused even with force and even with DIFFERENT content.
    let err = plan_write(
        root,
        "gen/alpha.rs",
        "v2\n",
        &fm_with_generation("gen/alpha.rs", 1),
    )
    .expect_err("stale generation must refuse");
    assert!(err.to_string().contains("FM-WRITE-012"), "{err}");
    assert!(err.to_string().contains("REFUSED_STALE_LEASE"), "{err}");
    // The on-disk file was not modified by the refused attempt.
    assert_eq!(
        std::fs::read_to_string(root.join("gen/alpha.rs")).expect("read target"),
        "v1\n"
    );
    assert_eq!(
        read_lease(root, "gen/alpha.rs"),
        1,
        "baseline unchanged on refusal"
    );
}

#[test]
fn stale_refusal_precedes_the_unchanged_content_skip() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    plan_write(root, "x.rs", "same\n", &fm_with_generation("x.rs", 3)).expect("admit gen 3");
    assert_eq!(read_lease(root, "x.rs"), 1);

    // Byte-identical content, but an epoch no longer fresher than the
    // baseline the first write recorded: the unchanged-content skip must
    // NOT admit; the lease gate fires first.
    let err = plan_write(root, "x.rs", "same\n", &fm_with_generation("x.rs", 1))
        .expect_err("identical content must not bypass the lease gate");
    assert!(err.to_string().contains("FM-WRITE-012"), "{err}");
}

#[test]
fn fresh_generation_is_admitted_and_baseline_bumped() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    plan_write(
        root,
        "gen/beta.rs",
        "v1\n",
        &fm_with_generation("gen/beta.rs", 1),
    )
    .expect("first write");
    assert_eq!(read_lease(root, "gen/beta.rs"), 1);

    let out = plan_write(
        root,
        "gen/beta.rs",
        "v2\n",
        &fm_with_generation("gen/beta.rs", 2),
    )
    .expect("fresher generation admits");
    assert_eq!(out, WriteOutcome::Written);
    assert_eq!(
        std::fs::read_to_string(root.join("gen/beta.rs")).expect("read target"),
        "v2\n"
    );
    assert_eq!(
        read_lease(root, "gen/beta.rs"),
        2,
        "baseline bumped to prev+1"
    );
}

#[test]
fn missing_slot_migrates_to_generation_one() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // No lease slot exists; a generation-0 (migration) receipt admits once…
    let out = plan_write(root, "legacy/old.rs", "content\n", &fm("legacy/old.rs"))
        .expect("missing slot = baseline 0 admits");
    assert_eq!(out, WriteOutcome::Written);
    // …and the successful write records generation 1.
    assert_eq!(read_lease(root, "legacy/old.rs"), 1);

    // After migration, a generation-0 receipt is stale against baseline 1.
    let err = plan_write(root, "legacy/old.rs", "again\n", &fm("legacy/old.rs"))
        .expect_err("generation-0 receipt vs non-zero baseline is stale");
    assert!(err.to_string().contains("FM-WRITE-012"), "{err}");
}

#[test]
fn corrupt_slot_fails_closed() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let slot = lease_path(root, "c.rs");
    std::fs::create_dir_all(slot.parent().expect("lease dir")).expect("mkdir lease dir");
    std::fs::write(&slot, "not-a-number").expect("seed corrupt slot");

    let err = plan_write(root, "c.rs", "content\n", &fm("c.rs"))
        .expect_err("corrupt slot must fail closed");
    assert!(err.to_string().contains("FM-WRITE-012"), "{err}");
    assert!(err.to_string().contains("REFUSED_STALE_LEASE"), "{err}");
    // Nothing was written under an unknown lease state.
    assert!(
        !root.join("c.rs").exists(),
        "refused write must not create the target"
    );
}

#[test]
fn injected_outcome_also_bumps_the_lease() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    std::fs::create_dir_all(root.join("host")).expect("host dir");
    std::fs::write(root.join("host/mod.rs"), "// SLOT\n").expect("seed host");

    let yaml = "to: host/mod.rs\ninject: true\nbefore: \"// SLOT\"\nreceipt_generation: 5\n";
    let f: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter");
    let out = plan_write(root, "host/mod.rs", "injected\n", &f).expect("inject admits");
    assert_eq!(out, WriteOutcome::Injected);
    // Baseline bumps prev+1 (missing slot = 0), regardless of how far ahead
    // the request's receipt generation was.
    assert_eq!(
        read_lease(root, "host/mod.rs"),
        1,
        "prev(0)+1 is written on first slot"
    );
}

#[test]
fn lease_slot_path_is_blake3_keyed_under_ggenv2_lease() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    plan_write(
        root,
        "deep/nested/file.rs",
        "x\n",
        &fm("deep/nested/file.rs"),
    )
    .expect("write");

    let expected = root.join(".ggen-v2").join("lease").join(
        blake3::hash("deep/nested/file.rs".as_bytes())
            .to_hex()
            .to_string(),
    );
    assert!(
        expected.exists(),
        "slot must exist at {}",
        expected.display()
    );
    // Payload format: decimal u64 + newline.
    let raw = std::fs::read_to_string(&expected).expect("read slot");
    assert_eq!(
        raw, "1\n",
        "slot payload is decimal u64 with trailing newline"
    );
    // Path-shaped derivation would nest by directory; blake3-keying stays flat.
    assert!(!root.join(".ggen-v2/lease/deep").exists());
}

#[test]
fn refusal_display_carries_the_refused_stale_lease_token() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    plan_write(root, "d.rs", "v1\n", &fm_with_generation("d.rs", 1)).expect("admit");
    let err = plan_write(root, "d.rs", "v2\n", &fm_with_generation("d.rs", 1))
        .expect_err("stale refused");
    // REFUSED_STALE_LEASE is the registry code (L13 vocabulary); it leads the
    // FM-WRITE-012 payload so typed refusals are greppable from the Display.
    let msg = err.to_string();
    let code_pos = msg.find("[FM-WRITE-012]").expect("FM code present");
    let token_pos = msg
        .find("REFUSED_STALE_LEASE")
        .expect("refusal code present");
    assert!(token_pos > code_pos, "token follows the FM code: {msg}");
    assert!(
        msg[code_pos..].starts_with("[FM-WRITE-012] REFUSED_STALE_LEASE"),
        "{msg}"
    );
}
