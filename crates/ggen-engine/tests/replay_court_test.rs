//! Replay-court tests (Chicago school): real tempdir writes, real re-hash
//! round-trips off disk, real JSON envelopes read back and promoted (or
//! typed-refused). No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;

use ggen_engine::portable_receipt::PORTABLE_RECEIPT_REL_PATH;
use ggen_engine::replay_court::{
    execute_and_persist, execute_replay_court, verdict_for, EnvelopeView, ReplayVerdict,
    REPLAY_PASS,
};

/// A real sync-shaped root: one landed write on disk, an ALIVE envelope
/// binding its digest.
struct Fixture {
    root: std::path::PathBuf,
    envelope_path: std::path::PathBuf,
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::Digest as _;
    hex::encode(sha2::Sha256::digest(bytes))
}

fn write_envelope(root: &std::path::Path, standing: &str, consequences: &[(&str, &str)]) -> std::path::PathBuf {
    let envelope = serde_json::json!({
        "schema": "https://ggen.dev/receipt/pack/v1",
        "spec": "RFC-GPACK-001-v26.9.17",
        "standing": standing,
        "consequences": consequences.iter().map(|(target, sha)| serde_json::json!({
            "target": target,
            "operation": "MANAGED_WRITE",
            "sha256": sha,
        })).collect::<Vec<_>>(),
        "replay": { "status": "UNKNOWN" },
    });
    let path = root.join(PORTABLE_RECEIPT_REL_PATH);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(&path, serde_json::to_vec_pretty(&envelope).unwrap()).unwrap();
    path
}

fn fixture(alive: bool) -> Fixture {
    let root = std::env::temp_dir().join(format!(
        "ggen-replay-court-{}-{}",
        std::process::id(),
        uuid_like()
    ));
    fs::create_dir_all(root.join("generated")).unwrap();
    let body = b"pub struct Widget;\n".as_slice();
    fs::write(root.join("generated/widget.rs"), body).unwrap();
    let standing = if alive { "ALIVE" } else { "PARTIAL_ALIVE" };
    let envelope_path = write_envelope(
        &root,
        standing,
        &[("generated/widget.rs", &sha256_hex(body))],
    );
    Fixture {
        root,
        envelope_path,
    }
}

fn uuid_like() -> u128 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos()
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

/// Round-trip: real write, real re-hash, full match -> PASS with per-file
/// evidence and a recompute timestamp.
#[test]
fn replay_court_passes_on_clean_state_reconstruction() {
    let fx = fixture(true);
    let verdict = execute_replay_court(&fx.root, &fx.envelope_path).unwrap();
    let ReplayVerdict::Pass {
        verified_at_unix_ms,
        digests,
    } = verdict
    else {
        panic!("expected PASS, got {verdict:?}");
    };
    assert!(verified_at_unix_ms > 0, "recompute timestamp must be bound");
    assert_eq!(digests.len(), 1);
    let digest = &digests[0];
    assert_eq!(digest.target, "generated/widget.rs");
    assert_eq!(
        digest.recomputed_sha256,
        sha256_hex(b"pub struct Widget;\n")
    );
    assert_eq!(digest.bound_sha256, digest.recomputed_sha256);
}

/// Corrupt one byte of a landed consequence -> typed REPLAY_MISMATCH, never
/// a silent pass.
#[test]
fn replay_court_refuses_typed_on_corrupted_byte() {
    let fx = fixture(true);
    fs::write(fx.root.join("generated/widget.rs"), b"pub struct Widget;\n// tampered\n").unwrap();
    let verdict = execute_replay_court(&fx.root, &fx.envelope_path).unwrap();
    assert_eq!(
        verdict,
        ReplayVerdict::Refused {
            code: "REPLAY_MISMATCH".to_string(),
            target: Some("generated/widget.rs".to_string()),
            detail: verdict_detail(&verdict),
        }
    );
    assert!(!verdict.is_pass());
}

/// Consequence missing from disk -> typed REPLAY_ABSENT.
#[test]
fn replay_court_refuses_typed_on_missing_file() {
    let fx = fixture(true);
    fs::remove_file(fx.root.join("generated/widget.rs")).unwrap();
    let verdict = execute_replay_court(&fx.root, &fx.envelope_path).unwrap();
    let ReplayVerdict::Refused { code, target, .. } = verdict else {
        panic!("expected Refused, got {verdict:?}");
    };
    assert_eq!(code, "REPLAY_ABSENT");
    assert_eq!(target.as_deref(), Some("generated/widget.rs"));
}

/// PARTIAL_ALIVE never promotes: the court reports UNKNOWN, not PASS, and
/// touches no consequence on disk even if those consequences would match.
#[test]
fn partial_alive_envelope_never_promotes() {
    let fx = fixture(false);
    let verdict = execute_replay_court(&fx.root, &fx.envelope_path).unwrap();
    let ReplayVerdict::Unknown { reason } = verdict else {
        panic!("expected Unknown, got {verdict:?}");
    };
    assert!(reason.contains("PARTIAL_ALIVE"), "{reason}");
}

/// Pure-rule form: REFUSED standing never promotes either.
#[test]
fn refused_standing_never_promotes() {
    let root = std::env::temp_dir().join(format!("ggen-replay-court-refused-{}", uuid_like()));
    fs::create_dir_all(&root).unwrap();
    let envelope = EnvelopeView {
        standing: "REFUSED:GATE_VIOLATION".to_string(),
        consequences: vec![],
    };
    let verdict = verdict_for(&root, &envelope);
    assert!(matches!(verdict, ReplayVerdict::Unknown { .. }));
    assert!(!verdict.is_pass());
    let _ = fs::remove_dir_all(root);
}

/// Sabotage twin: an attacker forges a twin envelope whose bound digests do
/// not match any landed state — the court refuses typed, and `execute_and_
/// persist` never rewrites the envelope to PASS.
#[test]
fn sabotage_twin_envelope_never_becomes_pass() {
    let fx = fixture(true);
    // Overwrite the envelope with fabricated digests over the real landed file.
    write_envelope(&fx.root, "ALIVE", &[("generated/widget.rs", &"0".repeat(64))]);
    let verdict = execute_and_persist(&fx.root, &fx.envelope_path).unwrap();
    assert!(!verdict.is_pass(), "sabotaged twin must not pass: {verdict:?}");

    // And the persisted envelope was not rewritten to PASS.
    let persisted = fs::read_to_string(&fx.envelope_path).unwrap();
    let value: serde_json::Value = serde_json::from_str(&persisted).unwrap();
    assert_eq!(value["replay"]["status"].as_str(), Some("UNKNOWN"));
}

/// Persistence promotion: a genuine clean-state reconstruction rewrites the
/// envelope's replay.status from UNKNOWN to PASS; a non-Pass verdict leaves
/// the envelope byte-identical.
#[test]
fn execute_and_persist_promotes_only_on_pass() {
    let fx = fixture(true);
    let verdict = execute_and_persist(&fx.root, &fx.envelope_path).unwrap();
    assert!(verdict.is_pass());
    let persisted: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(&fx.envelope_path).unwrap()).unwrap();
    assert_eq!(persisted["replay"]["status"].as_str(), Some(REPLAY_PASS));

    // Non-Pass path leaves the envelope untouched (fresh PARTIAL_ALIVE fixture
    // reusing the same landed file).
    let fx2 = fixture(false);
    let before = fs::read_to_string(&fx2.envelope_path).unwrap();
    let verdict2 = execute_and_persist(&fx2.root, &fx2.envelope_path).unwrap();
    assert!(!verdict2.is_pass());
    assert_eq!(before, fs::read_to_string(&fx2.envelope_path).unwrap());
}

/// Missing-envelope fail-closed: a nonexistent envelope is a typed
/// `[FM-CHAIN-019]` error, never an UNKNOWN standing that could masquerade as
/// evidence either way.
#[test]
fn missing_envelope_is_typed_error_not_unknown() {
    let root = std::env::temp_dir().join(format!("ggen-replay-court-missing-{}", uuid_like()));
    fs::create_dir_all(&root).unwrap();
    let err = execute_replay_court(&root, &root.join("nope/receipt-portable.json"))
        .expect_err("missing envelope must error");
    assert!(err.to_string().contains("FM-CHAIN-019"), "{err}");
    let _ = fs::remove_dir_all(root);
}

/// Extract the detail string for the corruption assertion above (the verdict
/// is consumed by pattern match, so recompute it here).
fn verdict_detail(verdict: &ReplayVerdict) -> String {
    match verdict {
        ReplayVerdict::Refused { detail, .. } => detail.clone(),
        _ => String::new(),
    }
}
