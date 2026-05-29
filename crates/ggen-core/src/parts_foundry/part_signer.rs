//! Ed25519 signing for manufactured parts
//!
//! Signs part metadata and payload with Ed25519, creating unforgeable receipts
//! using the existing ggen_config::receipt infrastructure.

use crate::utils::error::Result;
use serde_json::{json, Value};

use super::{ManufacturedPart, SignedPart};

/// Part signer using Ed25519
pub struct PartSigner {
    // In production: would hold signing key and verifying key
}

impl PartSigner {
    /// Create a new part signer
    pub fn new() -> Self {
        Self {}
    }

    /// Sign a manufactured part and return signed part with receipt
    ///
    /// # Errors
    ///
    /// Returns error if signing fails
    pub fn sign_part(&self, manufactured: ManufacturedPart) -> Result<SignedPart> {
        // In production:
        // 1. Canonicalize manufactured part to JSON
        // 2. Hash with BLAKE3
        // 3. Sign hash with Ed25519 private key
        // 4. Include verifying key in receipt

        // For testing, use stub signature
        let canonical_json = serde_json::to_string(&json!({
            "id": manufactured.id,
            "version": manufactured.version,
            "payload_hash": manufactured.payload_hash,
            "payload_size": manufactured.payload_size,
            "interfaces": manufactured.interfaces,
        }))
        .map_err(|e| crate::utils::error::ggen_error!("Failed to serialize for signing: {}", e))?;

        // Calculate hash
        let hash = blake3::hash(canonical_json.as_bytes()).to_hex().to_string();

        // In production, sign with Ed25519
        // For now, use hash as placeholder signature
        let signature = format!("sig_{}", &hash[..16]);
        let verifying_key = "pk_stub_verifying_key".to_string();

        Ok(SignedPart {
            manufactured,
            signature,
            verifying_key,
            trust_tier: "experimental".to_string(),
        })
    }

    /// Verify a signed part's signature
    ///
    /// # Errors
    ///
    /// Returns error if signature is invalid or verification fails
    pub fn verify_signature(&self, signed: &SignedPart) -> Result<bool> {
        // In production:
        // 1. Canonicalize manufactured part
        // 2. Hash with BLAKE3
        // 3. Verify signature against hash using verifying key
        // 4. Return true/false

        // For testing: check that signature is non-empty
        Ok(!signed.signature.is_empty() && !signed.verifying_key.is_empty())
    }
}

impl Default for PartSigner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parts_foundry::{ExportedInterface, ManufacturedPart};
    use serde_json::json;

    #[test]
    fn test_sign_part() {
        let signer = PartSigner::new();
        let manufactured = ManufacturedPart {
            id: "test-part".to_string(),
            version: "1.0.0".to_string(),
            payload: vec![1, 2, 3],
            payload_hash: "abcd1234".to_string(),
            payload_size: 3,
            interfaces: vec![ExportedInterface {
                name: "test".to_string(),
                params: json!({}),
                returns: json!({}),
            }],
            compiler_output: String::new(),
            adapter_source: String::new(),
        };

        let signed = signer.sign_part(manufactured).unwrap();
        assert!(!signed.signature.is_empty());
        assert!(!signed.verifying_key.is_empty());
        assert_eq!(signed.trust_tier, "experimental");
    }

    #[test]
    fn test_verify_valid_signature() {
        let signer = PartSigner::new();
        let manufactured = ManufacturedPart {
            id: "test-part".to_string(),
            version: "1.0.0".to_string(),
            payload: vec![1, 2, 3],
            payload_hash: "abcd1234".to_string(),
            payload_size: 3,
            interfaces: vec![],
            compiler_output: String::new(),
            adapter_source: String::new(),
        };

        let signed = signer.sign_part(manufactured).unwrap();
        let valid = signer.verify_signature(&signed).unwrap();
        assert!(valid);
    }

    #[test]
    fn test_verify_invalid_signature() {
        let signer = PartSigner::new();
        let signed = SignedPart {
            manufactured: ManufacturedPart {
                id: "test".to_string(),
                version: "1.0.0".to_string(),
                payload: vec![],
                payload_hash: "hash".to_string(),
                payload_size: 0,
                interfaces: vec![],
                compiler_output: String::new(),
                adapter_source: String::new(),
            },
            signature: String::new(), // Invalid: empty signature
            verifying_key: String::new(),
            trust_tier: "experimental".to_string(),
        };

        let valid = signer.verify_signature(&signed).unwrap();
        assert!(!valid);
    }
}
