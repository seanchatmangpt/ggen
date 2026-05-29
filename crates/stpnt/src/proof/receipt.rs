use blake3;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Errors that can occur while constructing or verifying a stewardship receipt.
///
/// A receipt is a proof object: it must never claim provenance it does not have.
/// These error variants make placeholder / zero-hash / empty-signature receipts
/// fail loudly instead of being silently accepted (see
/// `.claude/rules/coding-agent-mistakes.md` §4.2).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReceiptError {
    /// An empty signing key was supplied; it cannot produce a valid signature.
    EmptyKey,
}

impl std::fmt::Display for ReceiptError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReceiptError::EmptyKey => write!(
                f,
                "signing key must be non-empty; an empty key cannot produce a valid signature"
            ),
        }
    }
}

impl std::error::Error for ReceiptError {}

/// Result alias for receipt operations.
pub type Result<T> = std::result::Result<T, ReceiptError>;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct StewardshipReceipt {
    pub input_state_hash: [u8; 32],
    pub canon_hash: [u8; 32],
    pub route_hash: [u8; 32],
    pub obligation_hash: [u8; 32],
    pub prior_receipt_hash: Option<[u8; 32]>,
    pub actor_hash: [u8; 32],
    pub output_state_hash: [u8; 32],
    pub timestamp: DateTime<Utc>,
    /// Keyed-BLAKE3 MAC over the receipt's content hash. An EMPTY signature
    /// means the receipt is unsigned and therefore INVALID — `verify()` returns
    /// `false` for it. No code path may treat an empty-signature receipt as a
    /// proven transition.
    pub signature: Vec<u8>,
}

impl StewardshipReceipt {
    /// Construct an UNSIGNED receipt with real, content-derived hashes.
    ///
    /// Every hash is computed from the real bytes the caller supplies — there are
    /// no zero-hash placeholders. `route` and `obligation` are now required inputs
    /// so the constructor cannot fabricate provenance for a transition whose route
    /// or obligation it never saw.
    ///
    /// The returned receipt has an EMPTY signature and is therefore NOT yet valid.
    /// Call [`StewardshipReceipt::sign`] with a non-empty key to make it verifiable.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        input: &[u8], canon: &[&str], route: &[u8], obligation: &[u8],
        prior_hash: Option<[u8; 32]>, actor: &str, output: &[u8],
    ) -> Self {
        let mut canon_hasher = blake3::Hasher::new();
        for c in canon {
            canon_hasher.update(c.as_bytes());
        }

        Self {
            input_state_hash: blake3::hash(input).into(),
            canon_hash: canon_hasher.finalize().into(),
            route_hash: blake3::hash(route).into(),
            obligation_hash: blake3::hash(obligation).into(),
            prior_receipt_hash: prior_hash,
            actor_hash: blake3::hash(actor.as_bytes()).into(),
            output_state_hash: blake3::hash(output).into(),
            timestamp: Utc::now(),
            signature: Vec::new(), // unsigned until sign() is called
        }
    }

    /// Deterministic content hash binding every provenance field of this receipt.
    ///
    /// This is the message that the signature authenticates. It does NOT include
    /// the signature itself (the signature signs the content, not itself).
    fn content_hash(&self) -> [u8; 32] {
        let mut h = blake3::Hasher::new();
        h.update(&self.input_state_hash);
        h.update(&self.canon_hash);
        h.update(&self.route_hash);
        h.update(&self.obligation_hash);
        match self.prior_receipt_hash {
            Some(p) => {
                h.update(&[1u8]);
                h.update(&p);
            }
            None => {
                h.update(&[0u8]);
            }
        }
        h.update(&self.actor_hash);
        h.update(&self.output_state_hash);
        h.update(self.timestamp.to_rfc3339().as_bytes());
        h.finalize().into()
    }

    /// Compute the keyed-BLAKE3 MAC of this receipt's content under `key`.
    ///
    /// Returns `Err(ReceiptError::EmptyKey)` for an empty key: a meaningless key
    /// must not be allowed to mint a "valid" signature.
    fn mac(&self, key: &[u8]) -> Result<[u8; 32]> {
        if key.is_empty() {
            return Err(ReceiptError::EmptyKey);
        }
        // Derive a fixed-size keyed-hash key from arbitrary-length key material,
        // then MAC the content hash. blake3::keyed_hash requires a 32-byte key.
        let derived = blake3::hash(key);
        let mac = blake3::keyed_hash(derived.as_bytes(), &self.content_hash());
        Ok(*mac.as_bytes())
    }

    /// Sign this receipt in place with `key`, replacing any empty placeholder
    /// signature with a real keyed-BLAKE3 MAC over the receipt content.
    ///
    /// Returns `Err(ReceiptError::EmptyKey)` if `key` is empty.
    pub fn sign(&mut self, key: &[u8]) -> Result<()> {
        let mac = self.mac(key)?;
        self.signature = mac.to_vec();
        Ok(())
    }

    /// Verify this receipt's signature against `key`.
    ///
    /// Returns `false` (NOT an error) when:
    ///   * the signature is empty (the unsigned placeholder state), or
    ///   * the signature does not match the keyed MAC of the content.
    ///
    /// This is the invariant gate: an empty-signature receipt can never verify
    /// as valid, so the placeholder receipt produced by `new()` is rejected until
    /// it has been honestly signed.
    pub fn verify(&self, key: &[u8]) -> bool {
        if self.signature.is_empty() {
            return false;
        }
        match self.mac(key) {
            Ok(expected) => {
                // Constant-time-ish comparison over equal-length byte slices.
                self.signature.as_slice() == expected.as_slice()
            }
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ZERO: [u8; 32] = [0u8; 32];

    fn fixture() -> StewardshipReceipt {
        StewardshipReceipt::new(
            b"input-state",
            &["canon:csc-1", "canon:steward"],
            b"route:assign-steward",
            b"obligation:welcome-followup",
            None,
            "actor:steward-001",
            b"output-state",
        )
    }

    // --- Real hashes: no zero-hash placeholders ---

    #[test]
    fn route_and_obligation_hashes_are_real_not_zero() {
        let r = fixture();
        assert_ne!(
            r.route_hash, ZERO,
            "route_hash must be a real BLAKE3 hash, not the [0u8;32] placeholder"
        );
        assert_ne!(
            r.obligation_hash, ZERO,
            "obligation_hash must be a real BLAKE3 hash, not the [0u8;32] placeholder"
        );
        // And they must equal the independently-computed BLAKE3 of the same bytes.
        assert_eq!(
            r.route_hash,
            <[u8; 32]>::from(blake3::hash(b"route:assign-steward"))
        );
        assert_eq!(
            r.obligation_hash,
            <[u8; 32]>::from(blake3::hash(b"obligation:welcome-followup"))
        );
    }

    #[test]
    fn equal_inputs_yield_equal_provenance_hashes_determinism() {
        let a = fixture();
        let b = fixture();
        assert_eq!(a.route_hash, b.route_hash);
        assert_eq!(a.obligation_hash, b.obligation_hash);
        assert_eq!(a.input_state_hash, b.input_state_hash);
        assert_eq!(a.output_state_hash, b.output_state_hash);
        assert_eq!(a.canon_hash, b.canon_hash);
    }

    #[test]
    fn different_route_inputs_yield_different_hashes() {
        let base = fixture();
        let other = StewardshipReceipt::new(
            b"input-state",
            &["canon:csc-1", "canon:steward"],
            b"route:DIFFERENT-route",
            b"obligation:welcome-followup",
            None,
            "actor:steward-001",
            b"output-state",
        );
        assert_ne!(
            base.route_hash, other.route_hash,
            "different route bytes must produce different route_hash"
        );
        // obligation unchanged, so it stays equal — proving the hash binds the right field.
        assert_eq!(base.obligation_hash, other.obligation_hash);
    }

    #[test]
    fn different_obligation_inputs_yield_different_hashes() {
        let base = fixture();
        let other = StewardshipReceipt::new(
            b"input-state",
            &["canon:csc-1", "canon:steward"],
            b"route:assign-steward",
            b"obligation:DIFFERENT-obligation",
            None,
            "actor:steward-001",
            b"output-state",
        );
        assert_ne!(base.obligation_hash, other.obligation_hash);
        assert_eq!(base.route_hash, other.route_hash);
    }

    // --- Sabotage / invariant: empty signature MUST NOT verify ---

    #[test]
    fn freshly_constructed_receipt_is_unsigned_and_does_not_verify() {
        let r = fixture();
        assert!(
            r.signature.is_empty(),
            "new() must produce an explicitly unsigned receipt"
        );
        // The core invariant: an unsigned (empty-signature) receipt is NOT valid
        // under ANY key.
        assert!(
            !r.verify(b"any-key"),
            "an empty-signature receipt must never verify as valid"
        );
        assert!(
            !r.verify(b""),
            "empty key + empty signature must not verify"
        );
    }

    #[test]
    fn manually_emptied_signature_does_not_verify() {
        let mut r = fixture();
        r.sign(b"steward-signing-key")
            .expect("signing with a real key succeeds");
        assert!(r.verify(b"steward-signing-key"));
        // Sabotage: blank the signature back to the placeholder state.
        r.signature = Vec::new();
        assert!(
            !r.verify(b"steward-signing-key"),
            "blanking the signature must invalidate the receipt"
        );
    }

    // --- Honest signing path works and is tamper-evident ---

    #[test]
    fn signed_receipt_verifies_with_correct_key_only() {
        let mut r = fixture();
        r.sign(b"correct-key").expect("signing succeeds");
        assert!(
            !r.signature.is_empty(),
            "signing must populate a real signature"
        );
        assert!(
            r.verify(b"correct-key"),
            "receipt must verify under the signing key"
        );
        assert!(
            !r.verify(b"wrong-key"),
            "receipt must NOT verify under a different key"
        );
    }

    #[test]
    fn signing_with_empty_key_is_rejected() {
        let mut r = fixture();
        let err = r.sign(b"");
        assert!(
            matches!(err, Err(ReceiptError::EmptyKey)),
            "empty key must be refused, not silently signed"
        );
        // Receipt remains unsigned and therefore invalid.
        assert!(r.signature.is_empty());
        assert!(!r.verify(b"anything"));
    }

    #[test]
    fn signature_is_tamper_evident_against_field_mutation() {
        let mut r = fixture();
        r.sign(b"k").expect("signing succeeds");
        assert!(r.verify(b"k"));
        // Tamper with a provenance field after signing.
        r.output_state_hash = [9u8; 32];
        assert!(
            !r.verify(b"k"),
            "mutating a signed field must break verification"
        );
    }
}
