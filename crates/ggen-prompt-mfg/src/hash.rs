//! Cryptographic hashing for prompt deduplication
//!
//! Computes deterministic SHA-256 hashes of prompts for deduplication and verification.

use crate::{PromptError, Result};
use sha2::{Digest, Sha256};

/// Compute SHA-256 hash of prompt content
///
/// # Arguments
///
/// * `content` - Prompt content to hash
///
/// # Returns
///
/// Hexadecimal string representation of SHA-256 hash
///
/// # Errors
///
/// Returns error if hashing fails (should be infallible in practice)
pub fn compute_prompt_hash(content: &str) -> Result<String> {
    compute_hash(&content)
}

/// Compute SHA-256 hash of arbitrary bytes
///
/// # Arguments
///
/// * `data` - Data to hash
///
/// # Returns
///
/// Hexadecimal string representation of SHA-256 hash
///
/// # Errors
///
/// Returns error if encoding fails (should be infallible in practice)
pub fn compute_hash<T: AsRef<[u8]>>(data: &T) -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(data.as_ref());
    let result = hasher.finalize();
    Ok(hex::encode(result))
}

/// Verify prompt hash matches content
///
/// # Arguments
///
/// * `content` - Prompt content to verify
/// * `expected_hash` - Expected hash value
///
/// # Returns
///
/// `Ok(true)` if hash matches, `Ok(false)` if mismatch, `Err` on computation error
///
/// # Errors
///
/// Returns error if hash computation fails
pub fn verify_prompt_hash(content: &str, expected_hash: &str) -> Result<bool> {
    let computed = compute_prompt_hash(content)?;
    Ok(computed == expected_hash)
}

/// Compute hash of prompt IR structure
///
/// Uses canonical JSON serialization for determinism
///
/// # Arguments
///
/// * `ir` - Prompt IR to hash
///
/// # Returns
///
/// Hexadecimal string representation of SHA-256 hash
///
/// # Errors
///
/// Returns error if serialization or hashing fails
pub fn compute_ir_hash(ir: &crate::ir::PromptIR) -> Result<String> {
    // Serialize to canonical JSON
    let json = serde_json::to_string(ir)
        .map_err(|e| PromptError::Serialization(e.to_string()))?;

    compute_hash(&json.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_prompt_hash() {
        let content = "Test prompt content";
        let hash = compute_prompt_hash(content);
        assert!(hash.is_ok());

        let hash_str = hash.unwrap();
        assert_eq!(hash_str.len(), 64); // SHA-256 produces 64 hex characters
    }

    #[test]
    fn test_hash_determinism() {
        let content = "Deterministic test";

        let hash1 = compute_prompt_hash(content).unwrap();
        let hash2 = compute_prompt_hash(content).unwrap();

        assert_eq!(hash1, hash2, "Hash must be deterministic");
    }

    #[test]
    fn test_different_content_different_hash() {
        let content1 = "Content A";
        let content2 = "Content B";

        let hash1 = compute_prompt_hash(content1).unwrap();
        let hash2 = compute_prompt_hash(content2).unwrap();

        assert_ne!(hash1, hash2, "Different content must produce different hashes");
    }

    #[test]
    fn test_verify_prompt_hash() {
        let content = "Verification test";
        let hash = compute_prompt_hash(content).unwrap();

        let verified = verify_prompt_hash(content, &hash).unwrap();
        assert!(verified, "Hash verification should succeed");

        let wrong_hash = "0000000000000000000000000000000000000000000000000000000000000000";
        let verified_wrong = verify_prompt_hash(content, wrong_hash).unwrap();
        assert!(!verified_wrong, "Wrong hash should not verify");
    }

    #[test]
    fn test_empty_content_hash() {
        let hash = compute_prompt_hash("");
        assert!(hash.is_ok());

        // SHA-256 of empty string is known value
        let expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
        assert_eq!(hash.unwrap(), expected);
    }

    #[test]
    fn test_compute_hash_generic() {
        let data1 = b"test data";
        let data2 = "test data";

        let hash1 = compute_hash(&data1).unwrap();
        let hash2 = compute_hash(&data2).unwrap();

        assert_eq!(hash1, hash2, "Hash should work with different types");
    }

    #[test]
    fn test_ir_hash() {
        use crate::ir::{PromptIR, PromptMetadata};
        use std::collections::BTreeMap;

        let ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        let hash = compute_ir_hash(&ir);
        assert!(hash.is_ok());
    }

    #[test]
    fn test_ir_hash_determinism() {
        use crate::ir::{PromptIR, PromptMetadata};
        use std::collections::BTreeMap;

        let ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        let hash1 = compute_ir_hash(&ir).unwrap();
        let hash2 = compute_ir_hash(&ir).unwrap();

        assert_eq!(hash1, hash2, "IR hash must be deterministic");
    }
}
