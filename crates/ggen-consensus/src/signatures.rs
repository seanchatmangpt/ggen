//! Multi-signature aggregation for Byzantine consensus

use crate::{ConsensusError, Digest, ReplicaId, Result};
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Multi-signature aggregation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MultiSignature {
    /// Message digest being signed
    pub digest: Digest,
    /// Individual signatures from replicas
    pub signatures: HashMap<ReplicaId, Vec<u8>>,
}

impl MultiSignature {
    /// Create a new multi-signature
    pub fn new(digest: Digest) -> Self {
        Self {
            digest,
            signatures: HashMap::new(),
        }
    }

    /// Add a signature from a replica
    pub fn add_signature(&mut self, replica_id: ReplicaId, signature: Vec<u8>) -> Result<()> {
        if self.signatures.contains_key(&replica_id) {
            return Err(ConsensusError::DuplicateVote { replica_id });
        }

        self.signatures.insert(replica_id, signature);
        Ok(())
    }

    /// Get number of signatures
    pub fn signature_count(&self) -> usize {
        self.signatures.len()
    }

    /// Get all replica IDs that have signed
    pub fn signers(&self) -> HashSet<ReplicaId> {
        self.signatures.keys().copied().collect()
    }

    /// Check if a replica has signed
    pub fn has_signed(&self, replica_id: ReplicaId) -> bool {
        self.signatures.contains_key(&replica_id)
    }

    /// Verify all signatures
    pub fn verify_all(&self, public_keys: &HashMap<ReplicaId, VerifyingKey>) -> Result<()> {
        for (replica_id, signature_bytes) in &self.signatures {
            let public_key = public_keys.get(replica_id).ok_or_else(|| {
                ConsensusError::ByzantineFault {
                    reason: format!("Unknown replica {}", replica_id),
                }
            })?;

            let signature = Signature::from_slice(signature_bytes).map_err(|_| {
                ConsensusError::InvalidSignature {
                    replica_id: *replica_id,
                }
            })?;

            public_key.verify_strict(&self.digest, &signature).map_err(|_| {
                ConsensusError::InvalidSignature {
                    replica_id: *replica_id,
                }
            })?;
        }

        Ok(())
    }
}

/// Signature aggregator for collecting and verifying signatures
#[derive(Debug, Clone)]
pub struct SignatureAggregator {
    /// Public keys for all replicas
    public_keys: HashMap<ReplicaId, VerifyingKey>,
}

impl SignatureAggregator {
    /// Create a new signature aggregator
    pub fn new(public_keys: HashMap<ReplicaId, VerifyingKey>) -> Self {
        Self { public_keys }
    }

    /// Sign a message with a signing key
    pub fn sign_message(&self, signing_key: &SigningKey, message: &[u8]) -> Vec<u8> {
        let digest = blake3::hash(message);
        let signature = signing_key.sign(digest.as_bytes());
        signature.to_bytes().to_vec()
    }

    /// Verify a single signature
    pub fn verify_signature(
        &self,
        replica_id: ReplicaId,
        message: &[u8],
        signature_bytes: &[u8],
    ) -> Result<()> {
        let public_key = self.public_keys.get(&replica_id).ok_or_else(|| {
            ConsensusError::ByzantineFault {
                reason: format!("Unknown replica {}", replica_id),
            }
        })?;

        let signature = Signature::from_slice(signature_bytes).map_err(|_| {
            ConsensusError::InvalidSignature { replica_id }
        })?;

        let digest = blake3::hash(message);
        public_key
            .verify_strict(digest.as_bytes(), &signature)
            .map_err(|_| ConsensusError::InvalidSignature { replica_id })?;

        Ok(())
    }

    /// Create a multi-signature from individual signatures
    pub fn aggregate_signatures(
        &self,
        digest: Digest,
        signatures: Vec<(ReplicaId, Vec<u8>)>,
    ) -> Result<MultiSignature> {
        let mut multi_sig = MultiSignature::new(digest);

        for (replica_id, signature) in signatures {
            multi_sig.add_signature(replica_id, signature)?;
        }

        // Verify all signatures
        multi_sig.verify_all(&self.public_keys)?;

        Ok(multi_sig)
    }

    /// Verify a multi-signature
    pub fn verify_multi_signature(&self, multi_sig: &MultiSignature) -> Result<()> {
        multi_sig.verify_all(&self.public_keys)
    }

    /// Get public key for a replica
    pub fn get_public_key(&self, replica_id: ReplicaId) -> Option<&VerifyingKey> {
        self.public_keys.get(&replica_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_keys(count: usize) -> Vec<(ReplicaId, SigningKey, VerifyingKey)> {
        (0..count)
            .map(|i| {
                let signing_key = SigningKey::from_bytes(&[i as u8; 32]);
                let verifying_key = signing_key.verifying_key();
                (i as u64, signing_key, verifying_key)
            })
            .collect()
    }

    #[test]
    fn test_multi_signature_creation() {
        // Arrange
        let digest = [1u8; 32];

        // Act
        let multi_sig = MultiSignature::new(digest);

        // Assert
        assert_eq!(multi_sig.digest, digest);
        assert_eq!(multi_sig.signature_count(), 0);
    }

    #[test]
    fn test_add_signature() {
        // Arrange
        let digest = [1u8; 32];
        let mut multi_sig = MultiSignature::new(digest);

        // Act
        let result = multi_sig.add_signature(0, vec![1, 2, 3]);

        // Assert
        assert!(result.is_ok());
        assert_eq!(multi_sig.signature_count(), 1);
        assert!(multi_sig.has_signed(0));
    }

    #[test]
    fn test_duplicate_signature() {
        // Arrange
        let digest = [1u8; 32];
        let mut multi_sig = MultiSignature::new(digest);
        multi_sig.add_signature(0, vec![1, 2, 3]).unwrap();

        // Act
        let result = multi_sig.add_signature(0, vec![4, 5, 6]);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::DuplicateVote { replica_id: 0 })
        ));
    }

    #[test]
    fn test_signature_aggregator_sign_and_verify() {
        // Arrange
        let keys = create_test_keys(4);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();
        let aggregator = SignatureAggregator::new(public_keys);

        let message = b"test message";
        let (replica_id, signing_key, _) = &keys[0];

        // Act
        let signature = aggregator.sign_message(signing_key, message);
        let result = aggregator.verify_signature(*replica_id, message, &signature);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_signature_aggregator_invalid_signature() {
        // Arrange
        let keys = create_test_keys(4);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();
        let aggregator = SignatureAggregator::new(public_keys);

        let message = b"test message";
        let fake_signature = vec![0u8; 64];

        // Act
        let result = aggregator.verify_signature(0, message, &fake_signature);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::InvalidSignature { replica_id: 0 })
        ));
    }

    #[test]
    fn test_aggregate_multiple_signatures() {
        // Arrange
        let keys = create_test_keys(4);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();
        let aggregator = SignatureAggregator::new(public_keys);

        let message = b"test message";
        let digest = blake3::hash(message).into();

        // Create signatures from 3 replicas
        let signatures: Vec<_> = keys[0..3]
            .iter()
            .map(|(id, sk, _)| {
                let sig = aggregator.sign_message(sk, message);
                (*id, sig)
            })
            .collect();

        // Act
        let multi_sig = aggregator.aggregate_signatures(digest, signatures);

        // Assert
        assert!(multi_sig.is_ok());
        let multi_sig = multi_sig.unwrap();
        assert_eq!(multi_sig.signature_count(), 3);
        assert!(multi_sig.has_signed(0));
        assert!(multi_sig.has_signed(1));
        assert!(multi_sig.has_signed(2));
        assert!(!multi_sig.has_signed(3));
    }

    #[test]
    fn test_verify_multi_signature() {
        // Arrange
        let keys = create_test_keys(4);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();
        let aggregator = SignatureAggregator::new(public_keys);

        let message = b"test message";
        let digest = blake3::hash(message).into();

        let signatures: Vec<_> = keys[0..3]
            .iter()
            .map(|(id, sk, _)| {
                let sig = aggregator.sign_message(sk, message);
                (*id, sig)
            })
            .collect();

        let multi_sig = aggregator.aggregate_signatures(digest, signatures).unwrap();

        // Act
        let result = aggregator.verify_multi_signature(&multi_sig);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_tampered_signature_detection() {
        // Arrange
        let keys = create_test_keys(4);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();
        let aggregator = SignatureAggregator::new(public_keys);

        let message = b"test message";
        let digest = blake3::hash(message).into();

        // Create valid signature from replica 0
        let valid_sig = aggregator.sign_message(&keys[0].1, message);

        // Tamper with the signature
        let mut tampered_sig = valid_sig.clone();
        tampered_sig[0] ^= 0xFF;

        // Act
        let result = aggregator.aggregate_signatures(digest, vec![(0, tampered_sig)]);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_signers_list() {
        // Arrange
        let digest = [1u8; 32];
        let mut multi_sig = MultiSignature::new(digest);

        // Act
        multi_sig.add_signature(0, vec![1, 2, 3]).unwrap();
        multi_sig.add_signature(2, vec![4, 5, 6]).unwrap();
        multi_sig.add_signature(5, vec![7, 8, 9]).unwrap();

        let signers = multi_sig.signers();

        // Assert
        assert_eq!(signers.len(), 3);
        assert!(signers.contains(&0));
        assert!(signers.contains(&2));
        assert!(signers.contains(&5));
        assert!(!signers.contains(&1));
    }
}
