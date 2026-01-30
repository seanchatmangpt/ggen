//! Text-Blind Audit Verification
//!
//! Auditors verify contract compliance WITHOUT seeing payload data.
//! Only hashes, Merkle proofs, and metadata are exposed.
//!
//! Core principle: Verification of correctness without data exposure.
//! This enables privacy-preserving compliance audits where:
//! - Auditors can verify that operations followed contract rules
//! - Auditors cannot see the actual input/output data
//! - Merkle proofs cryptographically bind claims to evidence
//! - Receipt chains provide temporal ordering and tamper detection

use crate::crypto::{hash_sha256, hash_sha256_bytes, KeyPair, Signature};
use crate::error::{McpError, McpResult};
use crate::types::{EvidenceBundle, Receipt};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Trait for Merkle proof verification (implemented by mcp-merkle)
pub trait MerkleProofVerifier {
    /// Get the leaf hash
    fn leaf(&self) -> [u8; 32];
    /// Compute root from proof
    fn compute_root(&self) -> [u8; 32];
    /// Verify the proof is valid
    fn verify(&self) -> bool;
}

/// Simple proof structure for text-blind verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimpleProof {
    pub leaf: [u8; 32],
    pub root: [u8; 32],
    pub path: Vec<([u8; 32], bool)>,
}

impl MerkleProofVerifier for SimpleProof {
    fn leaf(&self) -> [u8; 32] { self.leaf }

    fn compute_root(&self) -> [u8; 32] {
        use crate::crypto::combine_hashes;
        let mut current = self.leaf;
        for (sibling, is_right) in &self.path {
            current = if *is_right {
                combine_hashes(&current, sibling)
            } else {
                combine_hashes(sibling, &current)
            };
        }
        current
    }

    fn verify(&self) -> bool {
        self.compute_root() == self.root
    }
}

/// Audit claim that can be verified without payload access
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditClaim {
    /// Unique identifier for this claim
    pub claim_id: String,
    /// Type of compliance being claimed
    pub claim_type: ClaimType,
    /// Contract ID this claim applies to
    pub contract_id: String,
    /// Epoch range this claim covers (inclusive)
    pub epoch_range: (u64, u64),
    /// Hash of the evidence supporting this claim
    pub evidence_hash: String,
    /// When this claim was created
    pub timestamp: DateTime<Utc>,
}

impl AuditClaim {
    /// Create a new audit claim
    pub fn new(
        claim_type: ClaimType,
        contract_id: impl Into<String>,
        epoch_range: (u64, u64),
        evidence_hash: impl Into<String>,
    ) -> Self {
        let claim_id = format!(
            "claim-{}-{}",
            Utc::now().timestamp_nanos_opt().unwrap_or(0),
            uuid::Uuid::new_v4()
        );

        Self {
            claim_id,
            claim_type,
            contract_id: contract_id.into(),
            epoch_range,
            evidence_hash: evidence_hash.into(),
            timestamp: Utc::now(),
        }
    }

    /// Compute the claim hash for signing/verification
    pub fn compute_hash(&self) -> String {
        let hash_input = format!(
            "{}|{:?}|{}|{}-{}|{}|{}",
            self.claim_id,
            self.claim_type,
            self.contract_id,
            self.epoch_range.0,
            self.epoch_range.1,
            self.evidence_hash,
            self.timestamp.to_rfc3339()
        );
        hash_sha256(hash_input.as_bytes())
    }

    /// Compute the claim hash as bytes
    pub fn compute_hash_bytes(&self) -> [u8; 32] {
        let hash_input = format!(
            "{}|{:?}|{}|{}-{}|{}|{}",
            self.claim_id,
            self.claim_type,
            self.contract_id,
            self.epoch_range.0,
            self.epoch_range.1,
            self.evidence_hash,
            self.timestamp.to_rfc3339()
        );
        hash_sha256_bytes(hash_input.as_bytes())
    }

    /// Validate epoch range is valid
    pub fn is_valid_epoch_range(&self) -> bool {
        self.epoch_range.0 <= self.epoch_range.1
    }
}

/// Types of compliance claims that can be verified
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClaimType {
    /// All operations stayed within envelope constraints
    EnvelopeCompliance,
    /// No unauthorized capability usage detected
    CapabilityCompliance,
    /// All refusals were legitimate (proper reasons)
    RefusalIntegrity,
    /// Receipt chain is unbroken and properly linked
    ChainIntegrity,
    /// Kill switch was never bypassed
    KillSwitchIntegrity,
}

impl ClaimType {
    /// Get a human-readable description
    pub fn description(&self) -> &'static str {
        match self {
            Self::EnvelopeCompliance => "All operations stayed within envelope constraints",
            Self::CapabilityCompliance => "No unauthorized capability usage detected",
            Self::RefusalIntegrity => "All refusals were legitimate with proper reasons",
            Self::ChainIntegrity => "Receipt chain is unbroken and properly linked",
            Self::KillSwitchIntegrity => "Kill switch was never bypassed",
        }
    }
}

/// A signed audit claim with cryptographic proof
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignedClaim {
    /// The underlying claim
    pub claim: AuditClaim,
    /// Signature over the claim hash
    pub signature: String,
    /// Public key hash of the signer
    pub signer_id: String,
}

impl SignedClaim {
    /// Create and sign a claim
    pub fn sign(claim: AuditClaim, keypair: &KeyPair) -> Self {
        let claim_hash = claim.compute_hash();
        let signature = keypair.sign(claim_hash.as_bytes());

        Self {
            claim,
            signature: signature.to_hex(),
            signer_id: keypair.public_key_hash(),
        }
    }

    /// Verify the signature on this claim
    pub fn verify(&self, keypair: &KeyPair) -> McpResult<()> {
        let claim_hash = self.claim.compute_hash();
        let sig_bytes = hex::decode(&self.signature)
            .map_err(|e| McpError::InvalidSignature(e.to_string()))?;
        let signature = Signature::from_bytes(sig_bytes)?;
        keypair.verify(claim_hash.as_bytes(), &signature)
    }
}

/// Verifier that validates claims without seeing data
///
/// The TextBlindVerifier maintains a set of trusted Merkle roots
/// from audited epochs. It can verify claims by checking that:
/// 1. The claim's evidence hash is in a trusted tree (via Merkle proof)
/// 2. The proof is cryptographically valid
/// 3. No payload data is required or exposed
pub struct TextBlindVerifier {
    /// Trusted Merkle roots (from audited epochs)
    trusted_roots: HashSet<[u8; 32]>,
    /// Trusted signer public key hashes
    trusted_signers: HashSet<String>,
}

impl TextBlindVerifier {
    /// Create a new verifier with no trusted roots
    pub fn new() -> Self {
        Self {
            trusted_roots: HashSet::new(),
            trusted_signers: HashSet::new(),
        }
    }

    /// Add a trusted Merkle root (from an audited epoch)
    pub fn trust_root(&mut self, root: [u8; 32]) {
        self.trusted_roots.insert(root);
    }

    /// Add a trusted Merkle root from hex string
    pub fn trust_root_hex(&mut self, root_hex: &str) -> McpResult<()> {
        let bytes = hex::decode(root_hex)
            .map_err(|e| McpError::InvalidInput(format!("Invalid hex: {}", e)))?;
        if bytes.len() != 32 {
            return Err(McpError::InvalidInput(format!(
                "Expected 32 bytes, got {}",
                bytes.len()
            )));
        }
        let mut root = [0u8; 32];
        root.copy_from_slice(&bytes);
        self.trust_root(root);
        Ok(())
    }

    /// Remove a trusted root
    pub fn untrust_root(&mut self, root: &[u8; 32]) -> bool {
        self.trusted_roots.remove(root)
    }

    /// Check if a root is trusted
    pub fn is_root_trusted(&self, root: &[u8; 32]) -> bool {
        self.trusted_roots.contains(root)
    }

    /// Get number of trusted roots
    pub fn trusted_root_count(&self) -> usize {
        self.trusted_roots.len()
    }

    /// Add a trusted signer
    pub fn trust_signer(&mut self, signer_public_key_hash: String) {
        self.trusted_signers.insert(signer_public_key_hash);
    }

    /// Check if a signer is trusted
    pub fn is_signer_trusted(&self, signer_id: &str) -> bool {
        self.trusted_signers.contains(signer_id)
    }

    /// Verify a claim using only hashes and proofs
    ///
    /// This is text-blind verification:
    /// - The verifier never sees the actual payload data
    /// - Only the hash (leaf), proof path, and root are examined
    /// - The claim is valid if the proof reconstructs to a trusted root
    pub fn verify_claim<P: MerkleProofVerifier>(
        &self,
        claim: &AuditClaim,
        proof: &P,
    ) -> McpResult<bool> {
        // Validate epoch range
        if !claim.is_valid_epoch_range() {
            return Err(McpError::InvalidInput(format!(
                "Invalid epoch range: {} > {}",
                claim.epoch_range.0, claim.epoch_range.1
            )));
        }

        // Verify the proof computes to a trusted root
        let computed_root = proof.compute_root();

        if !self.is_root_trusted(&computed_root) {
            return Ok(false);
        }

        // Verify the claim's evidence hash matches the proof leaf
        let evidence_bytes = hex::decode(&claim.evidence_hash)
            .map_err(|e| McpError::InvalidInput(format!("Invalid evidence hash: {}", e)))?;

        if evidence_bytes.len() != 32 {
            return Err(McpError::InvalidInput(format!(
                "Evidence hash must be 32 bytes, got {}",
                evidence_bytes.len()
            )));
        }

        let mut evidence_arr = [0u8; 32];
        evidence_arr.copy_from_slice(&evidence_bytes);

        // The proof's leaf should match the evidence hash
        if proof.leaf() != evidence_arr {
            return Ok(false);
        }

        // Verify the proof itself is valid
        Ok(proof.verify())
    }

    /// Verify a claim with its signed wrapper
    pub fn verify_signed_claim<P: MerkleProofVerifier>(
        &self,
        signed: &SignedClaim,
        proof: &P,
        keypair: &KeyPair,
    ) -> McpResult<SignedClaimVerification> {
        // Verify signature first
        let signature_valid = signed.verify(keypair).is_ok();

        // Verify the claim itself
        let claim_valid = self.verify_claim(&signed.claim, proof)?;

        // Check if signer is trusted
        let signer_trusted = self.is_signer_trusted(&signed.signer_id);

        Ok(SignedClaimVerification {
            signature_valid,
            claim_valid,
            signer_trusted,
            overall_valid: signature_valid && claim_valid && signer_trusted,
        })
    }

    /// Batch verify multiple claims
    ///
    /// Returns detailed results showing which claims passed/failed.
    pub fn verify_batch<P: MerkleProofVerifier>(
        &self,
        claims: &[(AuditClaim, P)],
    ) -> McpResult<BatchResult> {
        let mut verified = 0;
        let mut failed = Vec::new();

        for (claim, proof) in claims {
            match self.verify_claim(claim, proof) {
                Ok(true) => verified += 1,
                Ok(false) => failed.push(claim.claim_id.clone()),
                Err(e) => {
                    failed.push(format!("{}: {}", claim.claim_id, e));
                }
            }
        }

        Ok(BatchResult {
            total: claims.len(),
            verified,
            failed,
        })
    }

    /// Verify an evidence bundle's integrity
    pub fn verify_bundle(&self, bundle: &EvidenceBundle) -> McpResult<BundleVerification> {
        // Parse the bundle's merkle root
        let root_bytes = hex::decode(&bundle.receipt_chain_root)
            .map_err(|e| McpError::InvalidInput(format!("Invalid merkle root: {}", e)))?;

        if root_bytes.len() != 32 {
            return Err(McpError::InvalidInput("Merkle root must be 32 bytes".to_string()));
        }

        let mut root = [0u8; 32];
        root.copy_from_slice(&root_bytes);

        let root_trusted = self.is_root_trusted(&root);
        let has_attestations = !bundle.attestations.is_empty();

        Ok(BundleVerification {
            bundle_id: bundle.bundle_id.clone(),
            root_trusted,
            attestation_count: bundle.attestations.len(),
            valid: root_trusted && has_attestations,
        })
    }
}

impl Default for TextBlindVerifier {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of verifying a signed claim
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignedClaimVerification {
    /// Whether the signature is valid
    pub signature_valid: bool,
    /// Whether the claim proof is valid
    pub claim_valid: bool,
    /// Whether the signer is trusted
    pub signer_trusted: bool,
    /// Overall validity (all checks passed)
    pub overall_valid: bool,
}

/// Result of batch verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchResult {
    /// Total number of claims processed
    pub total: usize,
    /// Number of claims that verified successfully
    pub verified: usize,
    /// Claim IDs (or error messages) that failed verification
    pub failed: Vec<String>,
}

impl BatchResult {
    /// Check if all claims verified
    pub fn all_verified(&self) -> bool {
        self.verified == self.total && self.failed.is_empty()
    }

    /// Get verification rate as percentage
    pub fn verification_rate(&self) -> f64 {
        if self.total == 0 {
            return 100.0;
        }
        (self.verified as f64 / self.total as f64) * 100.0
    }
}

/// Result of verifying an evidence bundle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleVerification {
    /// Bundle ID
    pub bundle_id: String,
    /// Whether the merkle root is trusted
    pub root_trusted: bool,
    /// Number of attestations present
    pub attestation_count: usize,
    /// Overall validity
    pub valid: bool,
}

/// Builder for creating audit claims with evidence
pub struct AuditClaimBuilder {
    claim_type: ClaimType,
    contract_id: String,
    epoch_range: (u64, u64),
    receipts: Vec<Receipt>,
}

impl AuditClaimBuilder {
    /// Start building a new claim
    pub fn new(claim_type: ClaimType, contract_id: impl Into<String>) -> Self {
        Self {
            claim_type,
            contract_id: contract_id.into(),
            epoch_range: (0, 0),
            receipts: Vec::new(),
        }
    }

    /// Set the epoch range
    pub fn epoch_range(mut self, start: u64, end: u64) -> Self {
        self.epoch_range = (start, end);
        self
    }

    /// Add receipt evidence
    pub fn add_receipt(mut self, receipt: Receipt) -> Self {
        self.receipts.push(receipt);
        self
    }

    /// Add multiple receipts
    pub fn add_receipts(mut self, receipts: impl IntoIterator<Item = Receipt>) -> Self {
        self.receipts.extend(receipts);
        self
    }

    /// Build the claim and generate evidence hash
    ///
    /// Returns the claim with evidence hash computed from receipt chain.
    /// Use mcp-merkle to build the actual Merkle tree for proofs.
    pub fn build(self) -> (AuditClaim, Vec<[u8; 32]>) {
        // Collect receipt hashes for Merkle tree construction
        let receipt_hashes: Vec<[u8; 32]> = self.receipts
            .iter()
            .map(|r| hash_sha256_bytes(r.receipt_hash.as_bytes()))
            .collect();

        // Use first receipt's hash as evidence (or genesis if empty)
        let evidence_hash = if let Some(receipt) = self.receipts.first() {
            hash_sha256(receipt.receipt_hash.as_bytes())
        } else {
            crate::GENESIS_HASH.to_string()
        };

        let claim = AuditClaim::new(
            self.claim_type,
            self.contract_id,
            self.epoch_range,
            evidence_hash,
        );

        (claim, receipt_hashes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::crypto::hash_sha256_bytes;
    use crate::types::ExecutionMetrics;

    fn create_test_receipt(sequence: u64) -> Receipt {
        Receipt::new(
            sequence,
            crate::GENESIS_HASH,
            "test_op",
            "contract-001",
            "input-hash",
            "output-hash",
            ExecutionMetrics::default(),
        )
    }

    #[test]
    fn test_audit_claim_creation() {
        let claim = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 10),
            "evidence-hash-abc123",
        );

        assert!(claim.claim_id.starts_with("claim-"));
        assert_eq!(claim.claim_type, ClaimType::EnvelopeCompliance);
        assert_eq!(claim.contract_id, "contract-001");
        assert_eq!(claim.epoch_range, (1, 10));
    }

    #[test]
    fn test_audit_claim_hash_determinism() {
        let claim = AuditClaim {
            claim_id: "claim-fixed".to_string(),
            claim_type: ClaimType::ChainIntegrity,
            contract_id: "contract-001".to_string(),
            epoch_range: (1, 5),
            evidence_hash: "abc123".to_string(),
            timestamp: DateTime::parse_from_rfc3339("2024-01-01T00:00:00Z")
                .unwrap()
                .with_timezone(&Utc),
        };

        let hash1 = claim.compute_hash();
        let hash2 = claim.compute_hash();

        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64); // SHA-256 hex
    }

    #[test]
    fn test_epoch_range_validation() {
        let valid_claim = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 10),
            "evidence",
        );
        assert!(valid_claim.is_valid_epoch_range());

        let invalid_claim = AuditClaim {
            epoch_range: (10, 1), // Invalid: start > end
            ..valid_claim
        };
        assert!(!invalid_claim.is_valid_epoch_range());
    }

    #[test]
    fn test_claim_type_descriptions() {
        assert!(!ClaimType::EnvelopeCompliance.description().is_empty());
        assert!(!ClaimType::CapabilityCompliance.description().is_empty());
        assert!(!ClaimType::RefusalIntegrity.description().is_empty());
        assert!(!ClaimType::ChainIntegrity.description().is_empty());
        assert!(!ClaimType::KillSwitchIntegrity.description().is_empty());
    }

    #[test]
    fn test_text_blind_verifier_creation() {
        let verifier = TextBlindVerifier::new();
        assert_eq!(verifier.trusted_root_count(), 0);
    }

    #[test]
    fn test_trust_root() {
        let mut verifier = TextBlindVerifier::new();
        let root = [42u8; 32];

        verifier.trust_root(root);

        assert!(verifier.is_root_trusted(&root));
        assert_eq!(verifier.trusted_root_count(), 1);
    }

    #[test]
    fn test_trust_root_hex() {
        let mut verifier = TextBlindVerifier::new();
        let root_hex = "0000000000000000000000000000000000000000000000000000000000000001";

        assert!(verifier.trust_root_hex(root_hex).is_ok());
        assert_eq!(verifier.trusted_root_count(), 1);
    }

    #[test]
    fn test_trust_root_hex_invalid() {
        let mut verifier = TextBlindVerifier::new();

        // Invalid hex
        assert!(verifier.trust_root_hex("not-hex").is_err());

        // Wrong length
        assert!(verifier.trust_root_hex("0001").is_err());
    }

    #[test]
    fn test_untrust_root() {
        let mut verifier = TextBlindVerifier::new();
        let root = [42u8; 32];

        verifier.trust_root(root);
        assert!(verifier.untrust_root(&root));
        assert!(!verifier.is_root_trusted(&root));
    }

    #[test]
    fn test_verify_claim_with_valid_proof() {
        let mut verifier = TextBlindVerifier::new();

        // Create a simple proof (single leaf = root)
        let data = b"receipt-data";
        let leaf = hash_sha256_bytes(data);
        let root = leaf; // Single leaf tree: leaf == root

        verifier.trust_root(root);

        // Create a claim with the leaf hash as evidence
        let claim = AuditClaim::new(
            ClaimType::ChainIntegrity,
            "contract-001",
            (1, 1),
            hex::encode(leaf),
        );

        // Create SimpleProof for single leaf (no path needed)
        let proof = SimpleProof {
            leaf,
            root,
            path: vec![],
        };

        // Verify the claim
        let result = verifier.verify_claim(&claim, &proof);
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_verify_claim_untrusted_root() {
        let verifier = TextBlindVerifier::new(); // No trusted roots

        let leaf = hash_sha256_bytes(b"data");
        let root = leaf;

        let claim = AuditClaim::new(
            ClaimType::ChainIntegrity,
            "contract-001",
            (1, 1),
            hex::encode(leaf),
        );

        let proof = SimpleProof {
            leaf,
            root,
            path: vec![],
        };
        let result = verifier.verify_claim(&claim, &proof);

        assert!(result.is_ok());
        assert!(!result.unwrap()); // Should fail: untrusted root
    }

    #[test]
    fn test_verify_claim_evidence_mismatch() {
        let mut verifier = TextBlindVerifier::new();

        let leaf = hash_sha256_bytes(b"data");
        let root = leaf;
        verifier.trust_root(root);

        // Claim with wrong evidence hash
        let wrong_evidence = hash_sha256_bytes(b"different-data");
        let claim = AuditClaim::new(
            ClaimType::ChainIntegrity,
            "contract-001",
            (1, 1),
            hex::encode(wrong_evidence),
        );

        let proof = SimpleProof {
            leaf,
            root,
            path: vec![],
        };
        let result = verifier.verify_claim(&claim, &proof);

        assert!(result.is_ok());
        assert!(!result.unwrap()); // Should fail: evidence mismatch
    }

    #[test]
    fn test_verify_claim_invalid_epoch_range() {
        let mut verifier = TextBlindVerifier::new();
        verifier.trust_root([0u8; 32]);

        let claim = AuditClaim {
            claim_id: "test".to_string(),
            claim_type: ClaimType::ChainIntegrity,
            contract_id: "contract-001".to_string(),
            epoch_range: (10, 1), // Invalid range
            evidence_hash: hex::encode([0u8; 32]),
            timestamp: Utc::now(),
        };

        let proof = SimpleProof {
            leaf: [0u8; 32],
            root: [0u8; 32],
            path: vec![],
        };

        let result = verifier.verify_claim(&claim, &proof);
        assert!(result.is_err());
    }

    #[test]
    fn test_batch_verification_all_pass() {
        use crate::crypto::combine_hashes;

        let mut verifier = TextBlindVerifier::new();

        // Create 2 leaves and compute root manually
        let leaf0 = hash_sha256_bytes(b"receipt-0");
        let leaf1 = hash_sha256_bytes(b"receipt-1");
        let root = combine_hashes(&leaf0, &leaf1);

        verifier.trust_root(root);

        // Create claims with proofs
        let claim0 = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 3),
            hex::encode(leaf0),
        );
        let proof0 = SimpleProof {
            leaf: leaf0,
            root,
            path: vec![(leaf1, true)], // sibling on right
        };

        let claim1 = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 3),
            hex::encode(leaf1),
        );
        let proof1 = SimpleProof {
            leaf: leaf1,
            root,
            path: vec![(leaf0, false)], // sibling on left
        };

        let claims = vec![(claim0, proof0), (claim1, proof1)];
        let result = verifier.verify_batch(&claims).unwrap();

        assert_eq!(result.total, 2);
        assert_eq!(result.verified, 2);
        assert!(result.failed.is_empty());
        assert!(result.all_verified());
        assert_eq!(result.verification_rate(), 100.0);
    }

    #[test]
    fn test_batch_verification_partial_pass() {
        let mut verifier = TextBlindVerifier::new();

        let leaf = hash_sha256_bytes(b"valid");
        let root = leaf; // Single leaf tree
        verifier.trust_root(root);

        let valid_claim = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 1),
            hex::encode(leaf),
        );
        let valid_proof = SimpleProof { leaf, root, path: vec![] };

        // Invalid claim (wrong evidence)
        let invalid_claim = AuditClaim::new(
            ClaimType::EnvelopeCompliance,
            "contract-001",
            (1, 1),
            hex::encode([1u8; 32]),
        );

        let claims = vec![
            (valid_claim, valid_proof.clone()),
            (invalid_claim, valid_proof),
        ];

        let result = verifier.verify_batch(&claims).unwrap();

        assert_eq!(result.total, 2);
        assert_eq!(result.verified, 1);
        assert_eq!(result.failed.len(), 1);
        assert!(!result.all_verified());
        assert_eq!(result.verification_rate(), 50.0);
    }

    #[test]
    fn test_signed_claim() {
        let keypair = KeyPair::generate().unwrap();

        let claim = AuditClaim::new(
            ClaimType::CapabilityCompliance,
            "contract-001",
            (1, 5),
            "evidence-hash",
        );

        let signed = SignedClaim::sign(claim, &keypair);

        assert!(!signed.signature.is_empty());
        assert!(!signed.signer_id.is_empty());
        assert!(signed.verify(&keypair).is_ok());
    }

    #[test]
    fn test_signed_claim_wrong_key() {
        let keypair1 = KeyPair::generate().unwrap();
        let keypair2 = KeyPair::generate().unwrap();

        let claim = AuditClaim::new(
            ClaimType::ChainIntegrity,
            "contract-001",
            (1, 1),
            "evidence",
        );

        let signed = SignedClaim::sign(claim, &keypair1);

        // Verify with wrong key should fail
        assert!(signed.verify(&keypair2).is_err());
    }

    #[test]
    fn test_verify_signed_claim() {
        let mut verifier = TextBlindVerifier::new();
        let keypair = KeyPair::generate().unwrap();

        // Trust the signer
        verifier.trust_signer(keypair.public_key_hash());

        // Create proof and trust root
        let leaf = hash_sha256_bytes(b"data");
        let root = leaf; // Single leaf tree
        verifier.trust_root(root);

        // Create and sign claim
        let claim = AuditClaim::new(
            ClaimType::ChainIntegrity,
            "contract-001",
            (1, 1),
            hex::encode(leaf),
        );
        let signed = SignedClaim::sign(claim, &keypair);

        let proof = SimpleProof { leaf, root, path: vec![] };
        let result = verifier.verify_signed_claim(&signed, &proof, &keypair).unwrap();

        assert!(result.signature_valid);
        assert!(result.claim_valid);
        assert!(result.signer_trusted);
        assert!(result.overall_valid);
    }

    #[test]
    fn test_audit_claim_builder() {
        let receipt1 = create_test_receipt(1);
        let receipt2 = create_test_receipt(2);

        let (claim, tree) = AuditClaimBuilder::new(ClaimType::ChainIntegrity, "contract-001")
            .epoch_range(1, 5)
            .add_receipt(receipt1)
            .add_receipt(receipt2)
            .build();

        assert_eq!(claim.claim_type, ClaimType::ChainIntegrity);
        assert_eq!(claim.epoch_range, (1, 5));
        assert_eq!(tree.len(), 2);
    }

    #[test]
    fn test_batch_result_empty() {
        let result = BatchResult {
            total: 0,
            verified: 0,
            failed: vec![],
        };

        assert!(result.all_verified());
        assert_eq!(result.verification_rate(), 100.0);
    }

    #[test]
    fn test_trust_signer() {
        let mut verifier = TextBlindVerifier::new();
        let signer_id = "signer-abc123";

        assert!(!verifier.is_signer_trusted(signer_id));

        verifier.trust_signer(signer_id.to_string());

        assert!(verifier.is_signer_trusted(signer_id));
    }

    #[test]
    fn test_verify_bundle() {
        let mut verifier = TextBlindVerifier::new();

        // Create a bundle with a known root
        let root = [42u8; 32];
        verifier.trust_root(root);

        let mut bundle = EvidenceBundle::new_empty("gpt-4");
        bundle.receipt_chain_root = hex::encode(root);
        bundle.add_attestation("attestation-1".to_string());

        let result = verifier.verify_bundle(&bundle).unwrap();

        assert!(result.root_trusted);
        assert_eq!(result.attestation_count, 1);
        assert!(result.valid);
    }

    #[test]
    fn test_verify_bundle_untrusted_root() {
        let verifier = TextBlindVerifier::new(); // No trusted roots

        let bundle = EvidenceBundle::new_empty("gpt-4");
        let result = verifier.verify_bundle(&bundle).unwrap();

        assert!(!result.root_trusted);
        assert!(!result.valid);
    }

    #[test]
    fn test_verify_bundle_no_attestations() {
        let mut verifier = TextBlindVerifier::new();

        let root = [42u8; 32];
        verifier.trust_root(root);

        let mut bundle = EvidenceBundle::new_empty("gpt-4");
        bundle.receipt_chain_root = hex::encode(root);
        // No attestations

        let result = verifier.verify_bundle(&bundle).unwrap();

        assert!(result.root_trusted);
        assert_eq!(result.attestation_count, 0);
        assert!(!result.valid); // Invalid without attestations
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use crate::crypto::hash_sha256_bytes;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_claim_hash_deterministic(
            contract_id in "[a-z0-9-]{1,20}",
            epoch_start in 0u64..1000,
            epoch_end in 0u64..1000,
            evidence in "[a-f0-9]{64}",
        ) {
            let start = epoch_start.min(epoch_end);
            let end = epoch_start.max(epoch_end);

            let claim = AuditClaim {
                claim_id: "test".to_string(),
                claim_type: ClaimType::ChainIntegrity,
                contract_id,
                epoch_range: (start, end),
                evidence_hash: evidence,
                timestamp: DateTime::parse_from_rfc3339("2024-01-01T00:00:00Z")
                    .unwrap()
                    .with_timezone(&Utc),
            };

            let hash1 = claim.compute_hash();
            let hash2 = claim.compute_hash();

            prop_assert_eq!(hash1.clone(), hash2);
            prop_assert_eq!(hash1.len(), 64);
        }

        #[test]
        fn prop_verification_consistent(data: Vec<u8>) {
            prop_assume!(!data.is_empty());

            let mut verifier = TextBlindVerifier::new();

            let leaf = hash_sha256_bytes(&data);
            let root = leaf; // Single leaf tree
            verifier.trust_root(root);

            let claim = AuditClaim::new(
                ClaimType::ChainIntegrity,
                "contract",
                (1, 1),
                hex::encode(leaf),
            );

            let proof = SimpleProof { leaf, root, path: vec![] };

            // Verify multiple times
            let r1 = verifier.verify_claim(&claim, &proof);
            let r2 = verifier.verify_claim(&claim, &proof);

            prop_assert_eq!(r1.ok(), r2.ok());
        }

        #[test]
        fn prop_batch_result_invariants(verified in 0usize..100, failed_count in 0usize..100) {
            let total = verified + failed_count;
            let failed: Vec<String> = (0..failed_count).map(|i| format!("claim-{}", i)).collect();

            let result = BatchResult {
                total,
                verified,
                failed,
            };

            prop_assert_eq!(result.all_verified(), failed_count == 0 && verified == total);

            if total == 0 {
                prop_assert_eq!(result.verification_rate(), 100.0);
            } else {
                let expected_rate = (verified as f64 / total as f64) * 100.0;
                prop_assert!((result.verification_rate() - expected_rate).abs() < 0.001);
            }
        }
    }
}
