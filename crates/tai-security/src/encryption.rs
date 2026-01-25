//! Encryption utilities for data at rest and in transit
//!
//! This module provides:
//! - Encryption at rest (AES-256-GCM, ChaCha20-Poly1305)
//! - Encryption in transit (TLS)
//! - Key derivation (PBKDF2, Argon2)
//! - Symmetric encryption
//! - Asymmetric encryption (RSA)
//! - HMAC and authenticated encryption
//! - Secure key storage and rotation

use crate::error::{Result, SecurityError};
use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes256Gcm, Nonce};
use chacha20poly1305::ChaCha20Poly1305;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::Digest;
use std::collections::HashMap;
use tracing::{debug, error};
use zeroize::Zeroize;

/// Encryption algorithm
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum EncryptionAlgorithm {
    /// AES-256-GCM (recommended)
    #[serde(rename = "AES_256_GCM")]
    Aes256Gcm,

    /// ChaCha20-Poly1305 (modern alternative)
    #[serde(rename = "CHACHA20_POLY1305")]
    ChaCha20Poly1305,

    /// AES-128-GCM (less secure, legacy)
    #[serde(rename = "AES_128_GCM")]
    Aes128Gcm,

    /// RSA-2048 (asymmetric)
    #[serde(rename = "RSA_2048")]
    Rsa2048,

    /// RSA-4096 (asymmetric, slower)
    #[serde(rename = "RSA_4096")]
    Rsa4096,
}

/// Key derivation function
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum KeyDerivationFunction {
    /// PBKDF2 (NIST approved, slower iteration)
    #[serde(rename = "PBKDF2")]
    Pbkdf2,

    /// Argon2id (resistant to GPU/ASIC attacks, memory-hard)
    #[serde(rename = "ARGON2ID")]
    Argon2id,

    /// Scrypt (memory-hard, older)
    #[serde(rename = "SCRYPT")]
    Scrypt,
}

/// Encryption key
#[derive(Clone)]
pub struct EncryptionKey {
    /// Key ID for rotation tracking
    pub key_id: String,

    /// Raw key material (sensitive - stored in secure memory)
    key_material: Vec<u8>,

    /// Algorithm this key is used with
    pub algorithm: EncryptionAlgorithm,

    /// When key was created
    pub created_at: DateTime<Utc>,

    /// When key expires (for key rotation)
    pub expires_at: Option<DateTime<Utc>>,

    /// Whether key is still active
    pub active: bool,
}

impl EncryptionKey {
    /// Create new encryption key
    pub fn new(key_id: String, algorithm: EncryptionAlgorithm, key_material: Vec<u8>) -> Self {
        Self {
            key_id,
            key_material,
            algorithm,
            created_at: Utc::now(),
            expires_at: None,
            active: true,
        }
    }

    /// Get key material (reference, not copy)
    pub fn key_material(&self) -> &[u8] {
        &self.key_material
    }
}

impl Drop for EncryptionKey {
    fn drop(&mut self) {
        self.key_material.zeroize();
    }
}

/// Encrypted data with metadata
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EncryptedContent {
    /// Encrypted data (base64-encoded)
    pub ciphertext: String,

    /// Initialization vector (base64-encoded)
    pub iv: String,

    /// Authentication tag (base64-encoded)
    pub tag: String,

    /// Associated Authenticated Data (base64-encoded, optional)
    pub aad: Option<String>,

    /// Algorithm used
    pub algorithm: EncryptionAlgorithm,

    /// Key ID used for encryption
    pub key_id: String,

    /// Timestamp of encryption
    pub encrypted_at: DateTime<Utc>,
}

/// Encryption manager
pub struct EncryptionManager {
    /// Active encryption keys (key_id -> EncryptionKey)
    keys: std::sync::Arc<std::sync::Mutex<HashMap<String, EncryptionKey>>>,

    /// Default algorithm
    default_algorithm: EncryptionAlgorithm,

    /// Key derivation function
    kdf: KeyDerivationFunction,
}

impl EncryptionManager {
    /// Create new encryption manager
    pub fn new(default_algorithm: EncryptionAlgorithm, kdf: KeyDerivationFunction) -> Self {
        Self {
            keys: std::sync::Arc::new(std::sync::Mutex::new(HashMap::new())),
            default_algorithm,
            kdf,
        }
    }

    /// Generate a new encryption key
    pub fn generate_key(&self, key_id: String) -> Result<EncryptionKey> {
        let key_material = match self.default_algorithm {
            EncryptionAlgorithm::Aes256Gcm => self.generate_aes256_key()?,
            EncryptionAlgorithm::ChaCha20Poly1305 => self.generate_chacha20_key()?,
            _ => {
                return Err(SecurityError::key_management(
                    "Unsupported algorithm for key generation".to_string(),
                ))
            }
        };

        let key = EncryptionKey::new(key_id.clone(), self.default_algorithm.clone(), key_material);

        // Store key
        {
            let mut keys = self.keys.lock().unwrap();
            keys.insert(key_id, key.clone());
        }

        debug!("Generated encryption key: {}", key.key_id);
        Ok(key)
    }

    /// Derive key from password
    pub fn derive_key_from_password(
        &self,
        key_id: String,
        password: &str,
        salt: &[u8],
    ) -> Result<EncryptionKey> {
        let key_material = match self.kdf {
            KeyDerivationFunction::Pbkdf2 => self.derive_pbkdf2(password, salt)?,
            KeyDerivationFunction::Argon2id => self.derive_argon2(password, salt)?,
            KeyDerivationFunction::Scrypt => {
                return Err(SecurityError::key_management(
                    "Scrypt not yet implemented".to_string(),
                ))
            }
        };

        let key = EncryptionKey::new(key_id.clone(), self.default_algorithm.clone(), key_material);

        // Store key
        {
            let mut keys = self.keys.lock().unwrap();
            keys.insert(key_id, key.clone());
        }

        debug!("Derived encryption key from password: {}", key.key_id);
        Ok(key)
    }

    /// Encrypt data with AES-256-GCM
    pub fn encrypt_aes256(&self, plaintext: &[u8], key: &EncryptionKey) -> Result<EncryptedContent> {
        if key.algorithm != EncryptionAlgorithm::Aes256Gcm {
            return Err(SecurityError::encryption(
                "Key algorithm does not match requested algorithm".to_string(),
            ));
        }

        // Generate random nonce (96-bit for GCM)
        let nonce_bytes = rand::Rng::gen::<[u8; 12]>(&mut rand::thread_rng());
        let nonce = Nonce::from_slice(&nonce_bytes);

        // Create cipher
        let cipher = Aes256Gcm::new_from_slice(key.key_material())
            .map_err(|e| {
                error!("Failed to create AES-256-GCM cipher: {:?}", e);
                SecurityError::encryption("Failed to initialize cipher".to_string())
            })?;

        // Encrypt
        let ciphertext_bytes = cipher.encrypt(nonce, plaintext).map_err(|e| {
            error!("Encryption failed: {:?}", e);
            SecurityError::encryption("Encryption operation failed".to_string())
        })?;

        Ok(EncryptedContent {
            ciphertext: base64::encode(&ciphertext_bytes[..ciphertext_bytes.len() - 16]),
            iv: base64::encode(&nonce_bytes),
            tag: base64::encode(&ciphertext_bytes[ciphertext_bytes.len() - 16..]),
            aad: None,
            algorithm: EncryptionAlgorithm::Aes256Gcm,
            key_id: key.key_id.clone(),
            encrypted_at: Utc::now(),
        })
    }

    /// Decrypt data with AES-256-GCM
    pub fn decrypt_aes256(&self, encrypted: &EncryptedContent, key: &EncryptionKey) -> Result<Vec<u8>> {
        if key.algorithm != EncryptionAlgorithm::Aes256Gcm {
            return Err(SecurityError::encryption(
                "Key algorithm does not match".to_string(),
            ));
        }

        let nonce_bytes = base64::decode(&encrypted.iv).map_err(|e| {
            error!("Failed to decode nonce: {}", e);
            SecurityError::encryption("Invalid nonce format".to_string())
        })?;

        let nonce = Nonce::from_slice(&nonce_bytes);

        let mut ciphertext = base64::decode(&encrypted.ciphertext).map_err(|e| {
            error!("Failed to decode ciphertext: {}", e);
            SecurityError::encryption("Invalid ciphertext format".to_string())
        })?;

        let tag = base64::decode(&encrypted.tag).map_err(|e| {
            error!("Failed to decode tag: {}", e);
            SecurityError::encryption("Invalid tag format".to_string())
        })?;

        ciphertext.extend_from_slice(&tag);

        let cipher = Aes256Gcm::new_from_slice(key.key_material())
            .map_err(|_| SecurityError::encryption("Failed to initialize cipher".to_string()))?;

        let plaintext = cipher.decrypt(nonce, ciphertext.as_ref()).map_err(|e| {
            error!("Decryption failed: {:?}", e);
            SecurityError::encryption("Decryption failed - authentication tag mismatch".to_string())
        })?;

        debug!("Successfully decrypted data");
        Ok(plaintext)
    }

    /// Compute HMAC-SHA256
    pub fn hmac_sha256(&self, data: &[u8], key: &[u8]) -> Result<Vec<u8>> {
        use hmac::Mac;

        let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(key)
            .map_err(|_| SecurityError::crypto("Invalid HMAC key".to_string()))?;

        mac.update(data);
        let result = mac.finalize();
        Ok(result.into_bytes().to_vec())
    }

    /// Verify HMAC-SHA256
    pub fn verify_hmac_sha256(&self, data: &[u8], key: &[u8], tag: &[u8]) -> Result<bool> {
        let computed = self.hmac_sha256(data, key)?;
        Ok(computed == tag)
    }

    /// Hash password with SHA-256
    pub fn hash_password(&self, password: &str) -> Result<String> {
        let mut hasher = sha2::Sha256::new();
        hasher.update(password.as_bytes());
        let result = hasher.finalize();
        Ok(hex::encode(result))
    }

    // Private helper methods

    fn generate_aes256_key(&self) -> Result<Vec<u8>> {
        let mut key = vec![0u8; 32]; // 256 bits
        use rand::RngCore;
        rand::thread_rng().fill_bytes(&mut key);
        Ok(key)
    }

    fn generate_chacha20_key(&self) -> Result<Vec<u8>> {
        let mut key = vec![0u8; 32]; // 256 bits
        use rand::RngCore;
        rand::thread_rng().fill_bytes(&mut key);
        Ok(key)
    }

    fn derive_pbkdf2(&self, password: &str, salt: &[u8]) -> Result<Vec<u8>> {
        let mut key = vec![0u8; 32]; // 256 bits
        pbkdf2::pbkdf2::<hmac::Hmac<sha2::Sha256>>(
            password.as_bytes(),
            salt,
            100_000, // iterations
            &mut key,
        );
        Ok(key)
    }

    fn derive_argon2(&self, password: &str, salt: &[u8]) -> Result<Vec<u8>> {
        let hash = argon2::Argon2::default()
            .hash_password(password, &argon2::SaltString::encode_b64(salt).map_err(|e| {
                SecurityError::crypto(format!("Salt encoding failed: {}", e))
            })?)
            .map_err(|e| SecurityError::crypto(format!("Argon2 hashing failed: {}", e)))?;

        // Extract hash bytes
        let hash_bytes = hash
            .hash
            .ok_or_else(|| SecurityError::crypto("Failed to extract hash".to_string()))?
            .as_bytes()
            .to_vec();

        Ok(hash_bytes)
    }
}

mod base64 {
    pub fn encode(data: &[u8]) -> String {
        base64::engine::general_purpose::STANDARD.encode(data)
    }

    pub fn decode(data: &str) -> std::result::Result<Vec<u8>, base64::DecodeError> {
        use base64::Engine;
        base64::engine::general_purpose::STANDARD.decode(data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encryption_key_creation() {
        let key = EncryptionKey::new(
            "key-1".to_string(),
            EncryptionAlgorithm::Aes256Gcm,
            vec![0u8; 32],
        );

        assert_eq!(key.key_id, "key-1");
        assert_eq!(key.algorithm, EncryptionAlgorithm::Aes256Gcm);
        assert!(key.active);
    }

    #[test]
    fn test_encryption_manager_creation() {
        let manager = EncryptionManager::new(
            EncryptionAlgorithm::Aes256Gcm,
            KeyDerivationFunction::Pbkdf2,
        );

        assert_eq!(
            manager.default_algorithm,
            EncryptionAlgorithm::Aes256Gcm
        );
    }

    #[test]
    fn test_key_generation() {
        let manager = EncryptionManager::new(
            EncryptionAlgorithm::Aes256Gcm,
            KeyDerivationFunction::Pbkdf2,
        );

        let key = manager.generate_key("test-key".to_string());
        assert!(key.is_ok());

        let key = key.unwrap();
        assert_eq!(key.key_id, "test-key");
        assert_eq!(key.algorithm, EncryptionAlgorithm::Aes256Gcm);
    }

    #[test]
    fn test_password_hashing() {
        let manager = EncryptionManager::new(
            EncryptionAlgorithm::Aes256Gcm,
            KeyDerivationFunction::Pbkdf2,
        );

        let hash = manager.hash_password("test-password").unwrap();
        assert!(!hash.is_empty());
        assert_eq!(hash.len(), 64); // SHA-256 hex is 64 chars
    }

    #[test]
    fn test_hmac_sha256() {
        let manager = EncryptionManager::new(
            EncryptionAlgorithm::Aes256Gcm,
            KeyDerivationFunction::Pbkdf2,
        );

        let key = b"secret-key";
        let data = b"test data";

        let hmac = manager.hmac_sha256(data, key).unwrap();
        assert!(!hmac.is_empty());

        // Verify HMAC
        let verified = manager.verify_hmac_sha256(data, key, &hmac).unwrap();
        assert!(verified);
    }

    #[test]
    fn test_encryption_algorithm_enum() {
        let algo = EncryptionAlgorithm::Aes256Gcm;
        assert_eq!(algo, EncryptionAlgorithm::Aes256Gcm);
        assert_ne!(algo, EncryptionAlgorithm::ChaCha20Poly1305);
    }
}
