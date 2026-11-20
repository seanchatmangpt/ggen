//! Sovereign ceiling lease — native Rust port of
//! `GgenIgniter.SemanticJira.SovereignLease` (`sovereign_lease.ex`).
//!
//! The multi-party Ed25519-signed grant that a work order with
//! `authority_requirement: "SOVEREIGN"` must carry: strictly monotonic
//! ontology evolution is leased, not self-declared.
//!
//! Laws (all enforced by [`admit`]):
//!
//! * **Multi-party**: at least 2 signature records from at least 2 DISTINCT
//!   signers. Every record is an Ed25519 signature (`alg: "EdDSA"`) over the
//!   canonical lease bytes ([`canonical_json`]), verified against the
//!   caller-supplied public keys.
//! * **Strictly monotonic evolution**: a lease is granted over a pre-evolution
//!   `ontology_digest`; admitting it against the CURRENT pack ontology digest
//!   requires a real digest delta — [`Refusal::ShapeDigestUnchanged`]
//!   otherwise, because a lease over an unchanged ontology is a no-op.
//! * **No wall clock in the admit decision**: `granted_at` is recorded and
//!   shape-checked, never evaluated. The time law lives in the presenting
//!   system.
//!
//! Refusals: [`SovereignRefusal`] with `InsufficientSignatures`,
//! `SignatureInvalid`, `ShapeDigestUnchanged`, plus the fail-closed shape
//! guard `InvalidLease` for input that is not even a well-formed lease.
//!
//! IO-free. `#![forbid(unsafe_code)]` (crate-level).

use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::Serialize;
use std::collections::BTreeMap;

const ALG: &str = "EdDSA";
const MIN_SIGNATURES: usize = 2;

/// One Ed25519 signature record over the canonical lease bytes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SignatureRecord {
    pub signer: String,
    pub alg: String,
    /// Lowercase hex of the raw 64-byte Ed25519 signature.
    pub sig: String,
}

/// The sovereign lease (authority kind 0x04).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SovereignLease {
    pub lease_id: String,
    pub subject_iri: String,
    pub shapes: Vec<String>,
    /// The pre-evolution ontology digest the lease was granted over.
    pub ontology_digest: String,
    /// Recorded, shape-checked only — never evaluated in the admit decision.
    pub granted_at: String,
    pub signatures: Vec<SignatureRecord>,
}

/// Typed refusal of the sovereign admit decision (fail closed).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SovereignRefusal {
    #[error("invalid lease: input is not a well-formed sovereign lease")]
    InvalidLease,
    #[error("shape digest unchanged: a lease over an unchanged ontology is a no-op")]
    ShapeDigestUnchanged,
    #[error("insufficient signatures: need >= 2 signatures from >= 2 distinct signers")]
    InsufficientSignatures,
    #[error("signature invalid: a signature record failed Ed25519 verification")]
    SignatureInvalid,
}

/// The admitted record: the lease minus signatures, plus the sorted
/// `verified_signers` and `admitted: true`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AdmittedLease {
    pub lease_id: String,
    pub subject_iri: String,
    pub shapes: Vec<String>,
    pub ontology_digest: String,
    pub granted_at: String,
    pub verified_signers: Vec<String>,
    pub admitted: bool,
}

impl AdmittedLease {
    /// JSON value shape of the admitted record (mirrors the igniter's
    /// string-keyed map minus `"signatures"`, plus `"admitted"` and
    /// `"verified_signers"`).
    pub fn to_json(&self) -> serde_json::Value {
        serde_json::to_value(self).expect("AdmittedLease serializes infallibly")
    }
}

/// EdDSA-signs the canonical bytes of `lease`, returning one signature record.
pub fn sign_record(lease: &SovereignLease, signer: &str, priv_key: &SigningKey) -> SignatureRecord {
    let msg = canonical_json(lease);
    let sig = priv_key.sign(msg.as_bytes());
    SignatureRecord {
        signer: signer.to_string(),
        alg: ALG.to_string(),
        sig: hex_lower(&sig.to_bytes()),
    }
}

/// Admits `lease` for ontology evolution TO the current pack ontology digest
/// `pack_ontology_digest`. `public_keys` maps signer id to Ed25519 verifying
/// key. Pure; no wall clock, no IO.
pub fn admit(
    lease: &SovereignLease, pack_ontology_digest: &str,
    public_keys: &BTreeMap<String, VerifyingKey>,
) -> Result<AdmittedLease, SovereignRefusal> {
    shape(lease)?;
    digest_delta(lease, pack_ontology_digest)?;
    signature_gate(lease, public_keys)?;

    let mut signers: Vec<String> = lease.signatures.iter().map(|s| s.signer.clone()).collect();
    signers.sort();
    signers.dedup();

    Ok(AdmittedLease {
        lease_id: lease.lease_id.clone(),
        subject_iri: lease.subject_iri.clone(),
        shapes: lease.shapes.clone(),
        ontology_digest: lease.ontology_digest.clone(),
        granted_at: lease.granted_at.clone(),
        verified_signers: signers,
        admitted: true,
    })
}

/// The canonical lease bytes: sorted-key compact JSON of the lease minus the
/// `signatures` key and every derived key. [`sign_record`] and [`admit`] sign
/// and verify exactly these bytes.
pub fn canonical_json(lease: &SovereignLease) -> String {
    // Sorted-key pairs (mirrors the Elixir sort of Jason-encoded pairs).
    let mut pairs: Vec<(String, String)> = vec![
        (
            "granted_at".to_string(),
            serde_json::to_string(&lease.granted_at).expect("str serializes"),
        ),
        (
            "lease_id".to_string(),
            serde_json::to_string(&lease.lease_id).expect("str serializes"),
        ),
        (
            "ontology_digest".to_string(),
            serde_json::to_string(&lease.ontology_digest).expect("str serializes"),
        ),
        (
            "shapes".to_string(),
            serde_json::to_string(&lease.shapes).expect("vec serializes"),
        ),
        (
            "subject_iri".to_string(),
            serde_json::to_string(&lease.subject_iri).expect("str serializes"),
        ),
    ];
    pairs.sort();
    // the igniter JSON-encodes the KEY too (Jason.encode!(k)) — quoted
    let body: Vec<String> = pairs
        .into_iter()
        .map(|(k, v)| {
            format!(
                "{}:{}",
                serde_json::to_string(&k).expect("key serializes"),
                v
            )
        })
        .collect();
    format!("{{{}}}", body.join(","))
}

// ── gates ─────────────────────────────────────────────────────────────────

fn shape(lease: &SovereignLease) -> Result<(), SovereignRefusal> {
    // scalars are non-optional in the struct; the shape law binds the lists.
    if lease.shapes.is_empty() {
        Err(SovereignRefusal::InvalidLease)
    } else {
        Ok(())
    }
}

fn digest_delta(
    lease: &SovereignLease, pack_ontology_digest: &str,
) -> Result<(), SovereignRefusal> {
    if lease.ontology_digest == pack_ontology_digest {
        Err(SovereignRefusal::ShapeDigestUnchanged)
    } else {
        Ok(())
    }
}

/// The signature loop: the multi-party law (>= 2 signatures from >= 2
/// DISTINCT signers) and the Ed25519 verification both live behind this one
/// gate — a single named seam so the anti-vacuity court can bypass exactly
/// this line and require the 1-signature test to fail.
fn signature_gate(
    lease: &SovereignLease, public_keys: &BTreeMap<String, VerifyingKey>,
) -> Result<(), SovereignRefusal> {
    count_gate(lease)?;
    verify_all(lease, public_keys)
}

fn count_gate(lease: &SovereignLease) -> Result<(), SovereignRefusal> {
    let distinct = lease
        .signatures
        .iter()
        .map(|s| &s.signer)
        .collect::<std::collections::BTreeSet<_>>();
    if lease.signatures.len() >= MIN_SIGNATURES && distinct.len() >= MIN_SIGNATURES {
        Ok(())
    } else {
        Err(SovereignRefusal::InsufficientSignatures)
    }
}

fn verify_all(
    lease: &SovereignLease, public_keys: &BTreeMap<String, VerifyingKey>,
) -> Result<(), SovereignRefusal> {
    let msg = canonical_json(lease);
    for rec in &lease.signatures {
        if !signature_record_ok(rec, &msg, public_keys) {
            return Err(SovereignRefusal::SignatureInvalid);
        }
    }
    Ok(())
}

fn signature_record_ok(
    rec: &SignatureRecord, msg: &str, public_keys: &BTreeMap<String, VerifyingKey>,
) -> bool {
    let Some(pub_key) = public_keys.get(&rec.signer) else {
        return false;
    };
    if rec.alg != ALG {
        return false;
    }
    let Some(raw) = decode_sig_hex(&rec.sig) else {
        return false;
    };
    let sig = Signature::from_bytes(&raw);
    pub_key.verify(msg.as_bytes(), &sig).is_ok()
}

fn decode_sig_hex(sig: &str) -> Option<[u8; 64]> {
    if sig.len() != 128 || !sig.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    let mut out = [0u8; 64];
    for (i, chunk) in sig.as_bytes().chunks(2).enumerate() {
        // minimal unblock (SLA, E80-03 lane): `.ok()` on Option is illegal —
        // `to_digit` already returns Option; plain `?` preserves the law.
        let hi = (chunk[0] as char).to_digit(16)? as u8;
        let lo = (chunk[1] as char).to_digit(16)? as u8;
        out[i] = (hi << 4) | lo;
    }
    Some(out)
}

fn hex_lower(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}
