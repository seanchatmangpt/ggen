//! Canonical hash computation
//!
//! Provides deterministic hash computation using SHA-256.
//! Same input always produces same hash.

use crate::{CanonicalError, Result};
use sha2::{Digest, Sha256};

/// Compute SHA-256 hash of bytes
///
/// # Errors
///
/// Returns error if input cannot be hashed (should never fail for valid bytes)
pub fn compute_hash<T: AsRef<[u8]>>(data: &T) -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(data.as_ref());
    let result = hasher.finalize();
    Ok(hex::encode(result))
}

/// Compute hash of multiple inputs concatenated
///
/// # Errors
///
/// Returns error if inputs cannot be hashed
pub fn compute_hash_multi<T: AsRef<[u8]>>(data: &[T]) -> Result<String> {
    let mut hasher = Sha256::new();
    for item in data {
        hasher.update(item.as_ref());
    }
    let result = hasher.finalize();
    Ok(hex::encode(result))
}

/// Hash verifier for determinism testing
pub struct HashVerifier {
    expected: String,
}

impl HashVerifier {
    /// Create a new hash verifier
    pub fn new(expected: String) -> Self {
        Self { expected }
    }

    /// Verify that data matches expected hash
    ///
    /// # Errors
    ///
    /// Returns error if hash does not match or computation fails
    pub fn verify<T: AsRef<[u8]>>(&self, data: &T) -> Result<()> {
        let actual = compute_hash(data)?;
        if actual == self.expected {
            Ok(())
        } else {
            Err(CanonicalError::Hash(format!(
                "Hash mismatch: expected {}, got {}",
                self.expected, actual
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deterministic_hash() {
        let data = b"test data";
        let hash1 = compute_hash(&data).unwrap();
        let hash2 = compute_hash(&data).unwrap();
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_hash_length() {
        let data = b"test";
        let hash = compute_hash(&data).unwrap();
        assert_eq!(hash.len(), 64); // SHA-256 = 32 bytes = 64 hex chars
    }

    #[test]
    fn test_multi_hash_deterministic() {
        let data = vec![b"part1".as_slice(), b"part2".as_slice(), b"part3".as_slice()];
        let hash1 = compute_hash_multi(&data).unwrap();
        let hash2 = compute_hash_multi(&data).unwrap();
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_hash_verifier_success() {
        let data = b"test data";
        let hash = compute_hash(&data).unwrap();
        let verifier = HashVerifier::new(hash);
        assert!(verifier.verify(&data).is_ok());
    }

    #[test]
    fn test_hash_verifier_failure() {
        let data1 = b"test data";
        let data2 = b"different data";
        let hash1 = compute_hash(&data1).unwrap();
        let verifier = HashVerifier::new(hash1);
        assert!(verifier.verify(&data2).is_err());
    }
}
