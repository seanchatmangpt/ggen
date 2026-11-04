//! Chicago-style tests for the `SovereignLease` + `EpochPlan` ports.
//!
//! Real collaborators throughout: real Ed25519 keys (`ed25519-dalek`,
//! deterministic `SigningKey::from_bytes` — RFC 8032 keys, no RNG), real
//! serde_json candidate/watermark maps shaped like the JSONL plane. No
//! mocks; assertions on final state (admitted records, typed refusals).

use ed25519_dalek::{SigningKey, VerifyingKey};
use ggen_sjira::epoch::{self, Refusal as EpochRefusal};
use ggen_sjira::lease::{
    admit, canonical_json, sign_record, SignatureRecord, SovereignLease, SovereignRefusal,
};
use serde_json::json;
use std::collections::BTreeMap;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Deterministic Ed25519 keypair from a fixed 32-byte seed (no RNG).
fn key(seed_byte: u8) -> SigningKey {
    let seed = [seed_byte; 32];
    SigningKey::from_bytes(&seed)
}

fn lease_with(sigs: Vec<SignatureRecord>) -> SovereignLease {
    SovereignLease {
        lease_id: "lease-1".into(),
        subject_iri: "sj:WorkOrder/e80-09".into(),
        shapes: ["ggen:PackShape".to_string()].to_vec(),
        ontology_digest: "sha256:PRE".into(),
        granted_at: "2026-11-03T00:00:00Z".into(),
        signatures: sigs,
    }
}

fn keys(seed_bytes: &[u8]) -> BTreeMap<String, VerifyingKey> {
    seed_bytes
        .iter()
        .map(|b| {
            let k = key(*b);
            (format!("signer-{}", b), k.verifying_key())
        })
        .collect()
}

fn two_sig_lease() -> SovereignLease {
    two_sig_lease_from(lease_with(vec![]))
}

/// Signs `base` with two distinct deterministic keys.
fn two_sig_lease_from(base: SovereignLease) -> SovereignLease {
    let mut sigs = vec![
        sign_record(&base, "signer-1", &key(1)),
        sign_record(&base, "signer-2", &key(2)),
    ];
    sigs.sort_by(|a, b| a.signer.cmp(&b.signer));
    SovereignLease {
        signatures: sigs,
        ..base
    }
}

// ── sovereign lease ───────────────────────────────────────────────────────

#[test]
fn two_distinct_signatures_admit() {
    let lease = two_sig_lease();
    let pk = keys(&[1, 2]);
    let admitted = admit(&lease, "sha256:CURRENT", &pk).expect("2-sig lease admits");
    assert_eq!(admitted.admitted, true);
    assert_eq!(admitted.verified_signers, vec!["signer-1", "signer-2"]);
    assert_eq!(admitted.lease_id, "lease-1");
    // admitted record carries no signatures key
    let j = admitted.to_json();
    assert!(j.get("signatures").is_none());
    assert_eq!(j.get("admitted"), Some(&json!(true)));
}

#[test]
fn one_signature_refuses_insufficient() {
    let lease = two_sig_lease();
    let single = SovereignLease {
        signatures: vec![lease.signatures[0].clone()],
        ..lease
    };
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&single, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::InsufficientSignatures)
    );
}

#[test]
fn two_signatures_same_signer_refuses_insufficient() {
    let lease = lease_with(vec![]);
    let s1 = sign_record(&lease, "signer-1", &key(1));
    let s1_again = sign_record(&lease, "signer-1", &key(1));
    let dup = SovereignLease {
        signatures: vec![s1, s1_again],
        ..lease
    };
    let pk = keys(&[1]);
    assert_eq!(
        admit(&dup, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::InsufficientSignatures)
    );
}

#[test]
fn unchanged_ontology_digest_refuses_non_monotonic() {
    // lease over an ontology digest EQUAL to the current pack digest = no
    // evolution delta = no-op lease, refused.
    let lease = two_sig_lease();
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&lease, "sha256:PRE", &pk),
        Err(SovereignRefusal::ShapeDigestUnchanged)
    );
}

#[test]
fn tampered_signature_refuses_invalid() {
    let mut lease = two_sig_lease();
    let mut bad = lease.signatures[1].clone();
    bad.sig = format!("{:0128x}", 0u128); // well-formed hex, wrong signature
    lease.signatures[1] = bad;
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&lease, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::SignatureInvalid)
    );
}

#[test]
fn unsigned_field_change_breaks_verification() {
    // Mutate a SIGNED field after signing: canonical bytes change, the
    // signatures no longer verify — the lease is bound to its bytes.
    let mut lease = two_sig_lease();
    lease.subject_iri = "sj:WorkOrder/TAMPERED".into();
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&lease, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::SignatureInvalid)
    );
}

#[test]
fn granted_at_field_change_breaks_verification() {
    // The granted_at VALUE is inside the canonical signed bytes: tampering
    // the exact recorded timestamp after signing must refuse — the lease
    // binds to the timestamp itself, not merely to granted_at's presence.
    let mut lease = two_sig_lease();
    assert_eq!(lease.granted_at, "2026-11-03T00:00:00Z");
    lease.granted_at = "2030-01-01T00:00:00Z".into();
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&lease, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::SignatureInvalid)
    );
}

#[test]
fn unknown_signer_refuses_invalid() {
    let lease = two_sig_lease();
    let pk = keys(&[1]); // signer-2's key missing
    assert_eq!(
        admit(&lease, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::SignatureInvalid)
    );
}

#[test]
fn empty_shapes_refuse_invalid_lease() {
    let mut lease = two_sig_lease();
    lease.shapes = vec![];
    let pk = keys(&[1, 2]);
    assert_eq!(
        admit(&lease, "sha256:CURRENT", &pk),
        Err(SovereignRefusal::InvalidLease)
    );
}

#[test]
fn no_wall_clock_in_admit_decision() {
    // granted_at is recorded and shape-checked, never evaluated: an absurd
    // past/future/garbage timestamp admits identically, and admit is pure —
    // two calls with identical input return identical output.
    let pk = keys(&[1, 2]);
    for granted_at in [
        "1970-01-01T00:00:00Z",
        "9999-12-31T23:59:59Z",
        "not-a-timestamp-at-all",
    ] {
        // build + sign WITH the odd timestamp: the signatures bind the bytes
        // (mutating a signed field is the tamper law, tested separately);
        // the wall-clock law is that admit never EVALUATES the timestamp.
        let base = lease_with(vec![]);
        let base = SovereignLease {
            granted_at: granted_at.into(),
            ..base
        };
        let lease = two_sig_lease_from(base);
        let a1 = admit(&lease, "sha256:CURRENT", &pk);
        let a2 = admit(&lease, "sha256:CURRENT", &pk);
        assert_eq!(a1, a2);
        assert!(
            a1.is_ok(),
            "granted_at {:?} must not gate admission",
            granted_at
        );
    }
}

#[test]
fn canonical_json_is_sorted_key_compact_minus_signatures() {
    let lease = lease_with(vec![]);
    let c = canonical_json(&lease);
    assert!(c.starts_with('{') && c.ends_with('}'));
    assert!(
        !c.contains("signatures"),
        "derived/signed-over keys excluded"
    );
    // sorted key order: granted_at < lease_id < ontology_digest < shapes < subject_iri
    let g = c.find("\"granted_at\"").expect("granted_at present");
    let l = c.find("\"lease_id\"").expect("lease_id present");
    let o = c
        .find("\"ontology_digest\"")
        .expect("ontology_digest present");
    let s = c.find("\"shapes\"").expect("shapes present");
    let iri = c.find("\"subject_iri\"").expect("subject_iri present");
    assert!(g < l && l < o && o < s && s < iri);
    assert!(!c.contains(' '), "compact: no whitespace");
}

#[test]
fn canonical_bytes_change_with_fields() {
    let a = canonical_json(&lease_with(vec![]));
    let mut b = lease_with(vec![]);
    b.ontology_digest = "sha256:OTHER".into();
    assert_ne!(a, canonical_json(&b));
}

// ── epoch plan ────────────────────────────────────────────────────────────

fn watermark(globs: Option<Vec<&str>>) -> serde_json::Value {
    let mut wm = json!({
        "files": [
            {"path": "lib/foo.ex", "blob_sha": "aaa"},
            {"path": "lib/bar.ex", "blob_sha": "bbb"}
        ]
    });
    if let Some(gs) = globs {
        let gs: Vec<serde_json::Value> = gs.into_iter().map(|g| json!(g)).collect();
        wm.as_object_mut()
            .expect("wm object")
            .insert("implementation_globs".into(), json!(gs));
    }
    wm
}

fn order(
    epoch: &str, plan_touches: Vec<&str>, manufacture_plan: serde_json::Value,
) -> serde_json::Value {
    json!({
        "epoch": epoch,
        "plan_touches": plan_touches,
        "manufacture_plan": manufacture_plan
    })
}

#[test]
fn epoch_fresh_generated_plan_admits() {
    // lib/foo.ex is pre-watermark, but the plan routes it through ggen
    // (generated) — the lawful semantic-reuse direction.
    let o = order(
        "v26.11.0",
        vec!["lib/foo.ex"],
        json!({"lib/foo.ex": "generated"}),
    );
    assert_eq!(epoch::check(&o, Some(&watermark(None))), Ok(()));
}

#[test]
fn epoch_residue_plan_admits() {
    let o = order(
        "v26.11.0",
        vec!["lib/foo.ex"],
        json!({"lib/foo.ex": "residue"}),
    );
    assert_eq!(epoch::check(&o, Some(&watermark(None))), Ok(()));
}

#[test]
fn legacy_edit_refuses() {
    // pre-watermark path with NO fresh plan — the regressed direction.
    let o = order("v26.11.0", vec!["lib/foo.ex"], json!({}));
    assert_eq!(
        epoch::check(&o, Some(&watermark(None))),
        Err(EpochRefusal::LegacyEdit)
    );
}

#[test]
fn unattributed_implementation_refuses() {
    // a NEW file on the implementation plane (not pre-watermark) with no
    // fresh plan — unattributed implementation byte.
    let o = order("v26.11.0", vec!["lib/new_thing.ex"], json!({}));
    assert_eq!(
        epoch::check(&o, Some(&watermark(None))),
        Err(EpochRefusal::UnattributedImplementation)
    );
}

#[test]
fn reuses_pre_watermark_artifact_refuses() {
    let mut o = order("v26.11.0", vec![], json!({}));
    o.as_object_mut()
        .expect("obj")
        .insert("source_artifacts".into(), json!(["lib/bar.ex"]));
    assert_eq!(
        epoch::check(&o, Some(&watermark(None))),
        Err(EpochRefusal::ReusesPreWatermarkArtifact)
    );
}

#[test]
fn violation_precedence_legacy_edit_first() {
    // both a legacy edit AND an unattributed implementation present:
    // legacy_edit wins (rule order).
    let o = order(
        "v26.11.0",
        vec!["lib/foo.ex", "lib/new_thing.ex"],
        json!({}),
    );
    assert_eq!(
        epoch::check(&o, Some(&watermark(None))),
        Err(EpochRefusal::LegacyEdit)
    );
}

#[test]
fn missing_watermark_fails_closed() {
    let o = order("v26.11.0", vec!["lib/foo.ex"], json!({}));
    assert_eq!(
        epoch::check(&o, None),
        Err(EpochRefusal::WatermarkUnavailable)
    );
}

#[test]
fn malformed_watermark_fails_closed() {
    let o = order("v26.11.0", vec!["lib/foo.ex"], json!({}));
    let wm = json!({"head_sha": "abc"}); // no "files"
    assert_eq!(
        epoch::check(&o, Some(&wm)),
        Err(EpochRefusal::WatermarkUnavailable)
    );
}

#[test]
fn no_epoch_field_is_byte_identical_passthrough() {
    // candidates without "epoch" never gate — even against a watermark that
    // would refuse them.
    let o = json!({"plan_touches": ["lib/foo.ex"], "manufacture_plan": {}});
    assert_eq!(epoch::check(&o, Some(&watermark(None))), Ok(()));
    let empty_epoch = json!({"epoch": "", "plan_touches": ["lib/foo.ex"]});
    assert_eq!(epoch::check(&empty_epoch, Some(&watermark(None))), Ok(()));
}

#[test]
fn recorded_globs_union_default_set() {
    // watermark records ONLY the historical lib glob; the default set still
    // binds config and migrations planes at admission (fail closed, no
    // admission≠proof split).
    let wm = watermark(Some(vec!["lib/**/*.ex"]));
    let cfg = order("v26.11.0", vec!["config/prod.exs"], json!({}));
    assert_eq!(
        epoch::check(&cfg, Some(&wm)),
        Err(EpochRefusal::UnattributedImplementation)
    );
    let mig = order(
        "v26.11.0",
        vec!["priv/repo/migrations/001_init.exs"],
        json!({}),
    );
    assert_eq!(
        epoch::check(&mig, Some(&wm)),
        Err(EpochRefusal::UnattributedImplementation)
    );
    // and a fresh plan rescues them
    let cfg_fresh = order(
        "v26.11.0",
        vec!["config/prod.exs"],
        json!({"config/prod.exs": "generated"}),
    );
    assert_eq!(epoch::check(&cfg_fresh, Some(&wm)), Ok(()));
}

#[test]
fn glob_dialect_double_star_crosses_directories() {
    let wm = watermark(Some(vec!["lib/**/*.ex"]));
    // `**/` may match NONE: lib/foo.ex itself is implementation-plane
    let deep = order("v26.11.0", vec!["lib/a/b/c/deep.ex"], json!({}));
    assert_eq!(
        epoch::check(&deep, Some(&wm)),
        Err(EpochRefusal::UnattributedImplementation)
    );
    // `?` is one non-slash character: `lib/????/one.ex` matches lib/abcd/one.ex
    let wm_q = watermark(Some(vec!["lib/????/one.ex"]));
    let q_hit = order(
        "v26.11.0",
        vec!["lib/abcd/one.ex"],
        json!({"lib/abcd/one.ex": "generated"}),
    );
    assert_eq!(epoch::check(&q_hit, Some(&wm_q)), Ok(()));
    let q_unplanned = order("v26.11.0", vec!["lib/abcd/one.ex"], json!({}));
    assert_eq!(
        epoch::check(&q_unplanned, Some(&wm_q)),
        Err(EpochRefusal::UnattributedImplementation)
    );
}
