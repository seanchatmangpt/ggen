//! Receipt envelope (`chatmangpt.receipt.envelope.v1`).
//!
//! The envelope is a domain-agnostic Ed25519-signed wrapper that lets multiple
//! producer schemas (AutomlPlan, RalphPlan, UCausalReceipt, ConformanceResult,
//! …) share one verifiable causal chain without flattening their native
//! semantics. It does **not** know about producer-side fields; it only commits
//! to:
//!
//!   * the producer identity (`system` + `kind`),
//!   * the BLAKE3 hash of the producer's native payload bytes,
//!   * the optional on-disk path of that payload,
//!   * the previous envelope's BLAKE3 hash,
//!   * a portfolio Ed25519 signature.
//!
//! This file is intentionally separate from [`crate::Receipt`]. The
//! existing `Receipt` chain remains untouched for backward compatibility; the
//! envelope chain is a new, independent ledger.

use crate::error::{ReceiptError, Result};
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::{Deserialize, Serialize};

/// Schema discriminator for v1 envelopes.
pub const ENVELOPE_SCHEMA: &str = "chatmangpt.receipt.envelope.v1";
/// Canonical signature algorithm identifier (only Ed25519 in v1).
pub const SIGNATURE_ALGORITHM: &str = "Ed25519";
/// Prefix for BLAKE3 hash strings used in payloads and chain links.
pub const HASH_PREFIX: &str = "blake3:";

/// Identity of the producing system + the native artifact kind.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Producer {
    /// Producing system, e.g. `"dteam"`, `"ggen"`, `"unibit"`, `"wasm4pm"`, `"pictl"`.
    pub system: String,
    /// Native artifact kind, e.g. `"automl-plan"`, `"ralph-plan"`,
    /// `"ucausal-receipt"`, `"conformance-result"`.
    pub kind: String,
}

/// Reference to the producer-native payload that this envelope commits to.
///
/// `hash` is BLAKE3 over the canonical payload bytes (typically the on-disk
/// JSON file's contents) and is prefixed `blake3:` for forward-compat.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PayloadRef {
    /// Producer-native schema identifier (e.g. `"chatmangpt.ralph.plan.v1"`).
    pub schema: String,
    /// `blake3:<64 hex>` hash of the payload bytes.
    pub hash: String,
    /// Optional path to the payload artifact on disk.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
}

/// Ed25519 signature block.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvelopeSignature {
    /// Always `"Ed25519"` for v1.
    pub algorithm: String,
    /// Operator-defined identifier of the verifying key (e.g. file path,
    /// fingerprint, or external KMS reference). Free-form; not authenticated.
    pub public_key_ref: String,
    /// Lowercase-hex Ed25519 signature over the canonical envelope.
    pub value: String,
}

/// Chain link: previous envelope's BLAKE3 hash and this envelope's hash.
/// Both are `blake3:<hex>` strings. `previous_envelope_hash` is `None` for
/// genesis envelopes.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvelopeChainLink {
    /// `blake3:<hex>` of the previous envelope (`None` for genesis).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub previous_envelope_hash: Option<String>,
    /// `blake3:<hex>` of this envelope (filled in by [`ReceiptEnvelope::sign`]).
    pub own_hash: String,
}

/// A signed receipt envelope.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReceiptEnvelope {
    /// Schema discriminator. Must be [`ENVELOPE_SCHEMA`].
    pub schema: String,
    /// Stable envelope identifier (e.g. `"recenv-mcpp-..."`).
    pub envelope_id: String,
    /// Operation/work-unit identifier this envelope commits to.
    pub operation_id: String,
    /// UTC timestamp at envelope construction.
    pub timestamp: DateTime<Utc>,
    /// Producing system + native artifact kind.
    pub producer: Producer,
    /// Hash + reference to the producer-native payload.
    pub payload: PayloadRef,
    /// Ed25519 signature block.
    pub signature: EnvelopeSignature,
    /// Chain link (previous envelope hash + own hash).
    pub chain: EnvelopeChainLink,
}

impl ReceiptEnvelope {
    /// Construct an unsigned envelope. The returned envelope has empty
    /// signature/own-hash fields; pass it through [`Self::sign`] to finalize.
    pub fn new(
        envelope_id: impl Into<String>, operation_id: impl Into<String>, producer: Producer,
        payload: PayloadRef, previous_envelope_hash: Option<String>,
        public_key_ref: impl Into<String>,
    ) -> Self {
        Self {
            schema: ENVELOPE_SCHEMA.to_string(),
            envelope_id: envelope_id.into(),
            operation_id: operation_id.into(),
            timestamp: Utc::now(),
            producer,
            payload,
            signature: EnvelopeSignature {
                algorithm: SIGNATURE_ALGORITHM.to_string(),
                public_key_ref: public_key_ref.into(),
                value: String::new(),
            },
            chain: EnvelopeChainLink {
                previous_envelope_hash,
                own_hash: String::new(),
            },
        }
    }

    /// Canonical bytes that are signed. Produced by serializing the envelope
    /// with `signature.value` and `chain.own_hash` zeroed out so the same
    /// bytes can be reproduced for verification.
    fn signing_message(&self) -> Result<Vec<u8>> {
        let mut clone = self.clone();
        clone.signature.value.clear();
        clone.chain.own_hash.clear();
        let json = serde_json::to_string(&clone)?;
        Ok(json.into_bytes())
    }

    /// Compute the canonical BLAKE3 hash of this envelope (with own_hash
    /// excluded so the value is well-defined). Returns `blake3:<hex>`.
    pub fn hash(&self) -> Result<String> {
        let mut clone = self.clone();
        clone.chain.own_hash.clear();
        let json = serde_json::to_string(&clone)?;
        let h = blake3::hash(json.as_bytes());
        Ok(format!("{}{}", HASH_PREFIX, h.to_hex()))
    }

    /// Sign the envelope with `signing_key` and populate `chain.own_hash`.
    pub fn sign(mut self, signing_key: &SigningKey) -> Result<Self> {
        if self.schema != ENVELOPE_SCHEMA {
            return Err(ReceiptError::InvalidReceipt(format!(
                "envelope schema must be {} (got {})",
                ENVELOPE_SCHEMA, self.schema
            )));
        }
        let message = self.signing_message()?;
        let signature = signing_key.sign(&message);
        self.signature.value = hex::encode(signature.to_bytes());
        // own_hash is computed *after* signing because the signature is part
        // of the envelope's identity in the chain.
        self.chain.own_hash = self.hash()?;
        Ok(self)
    }

    /// Verify the envelope's Ed25519 signature against `verifying_key`.
    /// Does not validate chain linkage; use [`EnvelopeChain::verify`] for that.
    pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> {
        if self.schema != ENVELOPE_SCHEMA {
            return Err(ReceiptError::InvalidReceipt(format!(
                "envelope schema must be {} (got {})",
                ENVELOPE_SCHEMA, self.schema
            )));
        }
        let message = self.signing_message()?;
        let signature_bytes =
            hex::decode(&self.signature.value).map_err(|_| ReceiptError::InvalidSignature)?;
        let signature =
            Signature::from_slice(&signature_bytes).map_err(|_| ReceiptError::InvalidSignature)?;
        verifying_key
            .verify(&message, &signature)
            .map_err(|_| ReceiptError::InvalidSignature)?;

        // Defense in depth: confirm own_hash matches recomputed value.
        let recomputed = self.hash()?;
        if recomputed != self.chain.own_hash {
            return Err(ReceiptError::InvalidReceipt(
                "envelope own_hash does not match recomputed hash".into(),
            ));
        }
        Ok(())
    }
}

/// Compute BLAKE3 hash of a payload's bytes. Returns `blake3:<hex>`.
pub fn payload_hash(bytes: &[u8]) -> String {
    let h = blake3::hash(bytes);
    format!("{}{}", HASH_PREFIX, h.to_hex())
}

/// A chain of cryptographically linked envelopes.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EnvelopeChain {
    /// All envelopes in the chain, ordered from genesis to latest.
    pub envelopes: Vec<ReceiptEnvelope>,
}

impl EnvelopeChain {
    /// Create an empty chain.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a chain from a genesis envelope. Errors if `genesis` carries a
    /// `previous_envelope_hash`.
    pub fn from_genesis(genesis: ReceiptEnvelope) -> Result<Self> {
        if genesis.chain.previous_envelope_hash.is_some() {
            return Err(ReceiptError::InvalidReceipt(
                "genesis envelope must have no previous_envelope_hash".into(),
            ));
        }
        Ok(Self {
            envelopes: vec![genesis],
        })
    }

    /// Append `envelope` to the chain. Its `previous_envelope_hash` must
    /// equal the previous envelope's `own_hash`, or be `None` when the chain
    /// is empty (genesis case).
    pub fn append(&mut self, envelope: ReceiptEnvelope) -> Result<()> {
        if self.envelopes.is_empty() {
            if envelope.chain.previous_envelope_hash.is_some() {
                return Err(ReceiptError::InvalidChain(
                    "first envelope must be genesis (no previous hash)".into(),
                ));
            }
        } else if let Some(last) = self.envelopes.last() {
            let last_hash = &last.chain.own_hash;
            match &envelope.chain.previous_envelope_hash {
                Some(prev) if prev == last_hash => {}
                Some(prev) => {
                    return Err(ReceiptError::HashMismatch {
                        expected: last_hash.clone(),
                        actual: prev.clone(),
                    });
                }
                None => {
                    return Err(ReceiptError::InvalidChain(
                        "non-genesis envelope must have a previous hash".into(),
                    ));
                }
            }
        } else {
            // Empty chain but we already checked is_empty above, so this shouldn't happen
            // but keep it for completeness
        }
        self.envelopes.push(envelope);
        Ok(())
    }

    /// Number of envelopes currently in the chain.
    pub fn len(&self) -> usize {
        self.envelopes.len()
    }

    /// `true` if the chain has no envelopes.
    pub fn is_empty(&self) -> bool {
        self.envelopes.is_empty()
    }

    /// The most recent envelope, if any.
    pub fn last(&self) -> Option<&ReceiptEnvelope> {
        self.envelopes.last()
    }

    /// The genesis envelope, if any.
    pub fn genesis(&self) -> Option<&ReceiptEnvelope> {
        self.envelopes.first()
    }

    /// Verify every envelope's signature and the BLAKE3 chain link integrity.
    pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> {
        if self.envelopes.is_empty() {
            return Ok(());
        }

        let first = &self.envelopes[0];
        if first.chain.previous_envelope_hash.is_some() {
            return Err(ReceiptError::InvalidChain(
                "genesis envelope must have no previous_envelope_hash".into(),
            ));
        }
        first.verify(verifying_key)?;

        for i in 1..self.envelopes.len() {
            let curr = &self.envelopes[i];
            let prev = &self.envelopes[i - 1];
            curr.verify(verifying_key)?;
            match &curr.chain.previous_envelope_hash {
                Some(p) if p == &prev.chain.own_hash => {}
                Some(p) => {
                    return Err(ReceiptError::HashMismatch {
                        expected: prev.chain.own_hash.clone(),
                        actual: p.clone(),
                    });
                }
                None => {
                    return Err(ReceiptError::InvalidChain(format!(
                        "envelope at index {} missing previous hash",
                        i
                    )));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
/// Test module: unwrap() is acceptable in tests for setup and assertions.
mod tests {
    use super::*;
    use crate::generate_keypair;

    fn payload(schema: &str) -> PayloadRef {
        PayloadRef {
            schema: schema.into(),
            hash: payload_hash(b"hello world"),
            path: Some("/tmp/example.json".into()),
        }
    }

    fn producer(kind: &str) -> Producer {
        Producer {
            system: "dteam".into(),
            kind: kind.into(),
        }
    }

    #[test]
    fn round_trip_sign_verify() {
        let (sk, vk) = generate_keypair();
        let env = ReceiptEnvelope::new(
            "recenv-test-001",
            "obl-test-001",
            producer("ralph-plan"),
            payload("chatmangpt.ralph.plan.v1"),
            None,
            "~/.config/ggen/portfolio.ed25519.pub",
        )
        .sign(&sk)
        .unwrap();

        env.verify(&vk).unwrap();
        assert!(env.chain.own_hash.starts_with(HASH_PREFIX));
        assert_eq!(env.signature.value.len(), 128); // 64 raw bytes hex-encoded
    }

    #[test]
    fn payload_hash_is_blake3_prefixed() {
        let h = payload_hash(b"abc");
        assert!(h.starts_with(HASH_PREFIX));
        let raw = blake3::hash(b"abc").to_hex().to_string();
        assert_eq!(h, format!("{}{}", HASH_PREFIX, raw));
    }

    #[test]
    fn chain_links_correctly() {
        let (sk, vk) = generate_keypair();

        let g = ReceiptEnvelope::new(
            "recenv-1",
            "obl-1",
            producer("automl-plan"),
            payload("automl"),
            None,
            "kref",
        )
        .sign(&sk)
        .unwrap();

        let g_hash = g.chain.own_hash.clone();

        let next = ReceiptEnvelope::new(
            "recenv-2",
            "obl-2",
            producer("ralph-plan"),
            payload("ralph"),
            Some(g_hash.clone()),
            "kref",
        )
        .sign(&sk)
        .unwrap();

        let mut chain = EnvelopeChain::from_genesis(g).unwrap();
        chain.append(next).unwrap();
        chain.verify(&vk).unwrap();
        assert_eq!(chain.len(), 2);
        assert_eq!(
            chain.envelopes[1].chain.previous_envelope_hash.as_deref(),
            Some(g_hash.as_str())
        );
    }

    #[test]
    fn chain_rejects_wrong_previous_hash() {
        let (sk, _vk) = generate_keypair();
        let g = ReceiptEnvelope::new(
            "g",
            "obl-g",
            producer("automl-plan"),
            payload("a"),
            None,
            "kref",
        )
        .sign(&sk)
        .unwrap();
        let bad = ReceiptEnvelope::new(
            "b",
            "obl-b",
            producer("ralph-plan"),
            payload("b"),
            Some("blake3:0".repeat(64)),
            "kref",
        )
        .sign(&sk)
        .unwrap();
        let mut chain = EnvelopeChain::from_genesis(g).unwrap();
        assert!(chain.append(bad).is_err());
    }

    #[test]
    fn tampered_payload_fails_verify() {
        let (sk, vk) = generate_keypair();
        let mut env = ReceiptEnvelope::new(
            "e",
            "obl",
            producer("ralph-plan"),
            payload("ralph"),
            None,
            "kref",
        )
        .sign(&sk)
        .unwrap();
        env.payload.hash = payload_hash(b"different bytes");
        // own_hash now stale; verify must reject either via signature or hash check.
        assert!(env.verify(&vk).is_err());
    }
}
