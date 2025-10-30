use serde::{Deserialize, Serialize};
use std::fmt;

/// Cryptographic signature for package verification
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Signature {
    pub algorithm: SignatureAlgorithm,
    pub value: Vec<u8>,
    pub public_key: PublicKey,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl Signature {
    pub fn new(
        algorithm: SignatureAlgorithm,
        value: Vec<u8>,
        public_key: PublicKey,
    ) -> Self {
        Self {
            algorithm,
            value,
            public_key,
            timestamp: chrono::Utc::now(),
        }
    }

    pub fn to_hex(&self) -> String {
        hex::encode(&self.value)
    }

    pub fn from_hex(
        algorithm: SignatureAlgorithm,
        hex_value: &str,
        public_key: PublicKey,
    ) -> Result<Self, hex::FromHexError> {
        let value = hex::decode(hex_value)?;
        Ok(Self {
            algorithm,
            value,
            public_key,
            timestamp: chrono::Utc::now(),
        })
    }
}

/// Supported signature algorithms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SignatureAlgorithm {
    Ed25519,
    EcdsaP256,
    Rsa2048,
    Rsa4096,
}

impl fmt::Display for SignatureAlgorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ed25519 => write!(f, "ed25519"),
            Self::EcdsaP256 => write!(f, "ecdsa-p256"),
            Self::Rsa2048 => write!(f, "rsa-2048"),
            Self::Rsa4096 => write!(f, "rsa-4096"),
        }
    }
}

/// Public key for signature verification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PublicKey {
    pub algorithm: SignatureAlgorithm,
    pub key_data: Vec<u8>,
    pub fingerprint: String,
}

impl PublicKey {
    pub fn new(algorithm: SignatureAlgorithm, key_data: Vec<u8>) -> Self {
        let fingerprint = Self::compute_fingerprint(&key_data);
        Self {
            algorithm,
            key_data,
            fingerprint,
        }
    }

    pub fn to_pem(&self) -> String {
        // This is a simplified version - in production, use proper PEM encoding
        format!(
            "-----BEGIN PUBLIC KEY-----\n{}\n-----END PUBLIC KEY-----",
            base64::encode(&self.key_data)
        )
    }

    pub fn from_pem(pem: &str) -> crate::error::Result<Self> {
        // Simplified PEM parsing - in production, use proper PEM library
        let cleaned = pem
            .replace("-----BEGIN PUBLIC KEY-----", "")
            .replace("-----END PUBLIC KEY-----", "")
            .replace('\n', "")
            .replace(' ', "");

        let key_data = base64::decode(&cleaned).map_err(|e| {
            crate::error::MarketplaceError::verification_error(
                format!("Invalid PEM encoding: {}", e),
                "public key parsing",
            )
        })?;

        // For now, assume Ed25519 - in production, parse algorithm from PEM
        Ok(Self::new(SignatureAlgorithm::Ed25519, key_data))
    }

    fn compute_fingerprint(key_data: &[u8]) -> String {
        use sha2::{Digest, Sha256};
        let hash = Sha256::digest(key_data);
        hex::encode(hash)
    }
}

impl fmt::Display for PublicKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.algorithm, &self.fingerprint[..16])
    }
}

/// Key pair for signing operations (private key should be protected)
#[derive(Debug)]
pub struct KeyPair {
    pub public_key: PublicKey,
    private_key: Vec<u8>,
}

impl KeyPair {
    pub fn new(algorithm: SignatureAlgorithm, private_key: Vec<u8>, public_key: Vec<u8>) -> Self {
        Self {
            public_key: PublicKey::new(algorithm, public_key),
            private_key,
        }
    }

    pub fn private_key_bytes(&self) -> &[u8] {
        &self.private_key
    }
}

// Ensure private keys are not accidentally exposed
impl Drop for KeyPair {
    fn drop(&mut self) {
        // Zero out private key memory on drop
        for byte in &mut self.private_key {
            *byte = 0;
        }
    }
}

/// Signature verification result
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationResult {
    Valid,
    Invalid { reason: String },
    KeyNotFound,
    UnsupportedAlgorithm,
}

impl VerificationResult {
    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Valid)
    }

    pub fn to_result(&self) -> crate::error::Result<()> {
        match self {
            Self::Valid => Ok(()),
            Self::Invalid { reason } => Err(crate::error::MarketplaceError::verification_error(
                reason.clone(),
                "signature verification",
            )),
            Self::KeyNotFound => Err(crate::error::MarketplaceError::verification_error(
                "public key not found",
                "signature verification",
            )),
            Self::UnsupportedAlgorithm => {
                Err(crate::error::MarketplaceError::verification_error(
                    "unsupported signature algorithm",
                    "signature verification",
                ))
            }
        }
    }
}
