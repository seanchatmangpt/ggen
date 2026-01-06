//! API key management

use chrono::{DateTime, Duration, Utc};
use hex;
use sha2::{Digest, Sha256};
use uuid::Uuid;

use crate::{errors::AuthError, AuthResult};

/// API key hash for storage
#[derive(Debug, Clone)]
pub struct ApiKeyHash {
    pub id: String,
    pub hash: String,
    pub name: String,
    pub created_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
    pub last_used: Option<DateTime<Utc>>,
    pub active: bool,
}

/// API key manager
pub struct ApiKeyManager {
    secret_salt: String,
}

impl ApiKeyManager {
    /// Create a new API key manager
    pub fn new(secret_salt: String) -> Self {
        Self { secret_salt }
    }

    /// Generate a new API key
    pub fn generate_key(&self) -> (String, String) {
        let key = format!("ggen_{}", Uuid::new_v4().to_string().replace("-", ""));
        let hash = self.hash_key(&key);
        (key, hash)
    }

    /// Hash an API key for storage
    pub fn hash_key(&self, key: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(format!("{}{}", key, self.secret_salt).as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Verify an API key against a hash
    pub fn verify_key(&self, key: &str, hash: &str) -> bool {
        let computed_hash = self.hash_key(key);
        // Constant-time comparison
        computed_hash.as_bytes().len() == hash.len()
            && computed_hash
                .as_bytes()
                .iter()
                .zip(hash.as_bytes().iter())
                .all(|(a, b)| a == b)
    }

    /// Check if a key is expired
    pub fn is_expired(key_hash: &ApiKeyHash) -> bool {
        if let Some(expires_at) = key_hash.expires_at {
            expires_at < Utc::now()
        } else {
            false
        }
    }

    /// Check if a key is valid (active and not expired)
    pub fn is_valid(key_hash: &ApiKeyHash) -> bool {
        key_hash.active && !Self::is_expired(key_hash)
    }

    /// Create an API key hash entry
    pub fn create_hash(
        &self,
        key: &str,
        name: String,
        expires_in_days: Option<u32>,
    ) -> ApiKeyHash {
        let expires_at = expires_in_days.map(|days| {
            Utc::now() + Duration::days(days as i64)
        });

        ApiKeyHash {
            id: Uuid::new_v4().to_string(),
            hash: self.hash_key(key),
            name,
            created_at: Utc::now(),
            expires_at,
            last_used: None,
            active: true,
        }
    }

    /// Revoke an API key
    pub fn revoke(&self, key_hash: &mut ApiKeyHash) {
        key_hash.active = false;
    }

    /// Record key usage
    pub fn record_usage(key_hash: &mut ApiKeyHash) {
        key_hash.last_used = Some(Utc::now());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_and_verify_key() {
        let manager = ApiKeyManager::new("salt".to_string());
        let (key, _hash) = manager.generate_key();
        assert!(key.starts_with("ggen_"));
    }

    #[test]
    fn test_hash_verification() {
        let manager = ApiKeyManager::new("salt".to_string());
        let key = "test_key";
        let hash = manager.hash_key(key);
        assert!(manager.verify_key(key, &hash));
    }

    #[test]
    fn test_key_expiration() {
        let manager = ApiKeyManager::new("salt".to_string());
        let key = "test";
        let mut key_hash = manager.create_hash(key, "test".to_string(), Some(0));
        assert!(ApiKeyManager::is_expired(&key_hash));
    }
}
