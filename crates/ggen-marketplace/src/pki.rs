//! PKI manager for trusted key management and revocation.
//!
//! Loads trusted public keys from `.ggen/trusted-keys.toml` and maintains
//! a revocation list for key lifecycle management. Uses Ed25519 keys from
//! `ggen-receipt` as the canonical key type (no separate certificate parsing).

use std::collections::{HashMap, HashSet};
use std::path::Path;

use ed25519_dalek::VerifyingKey;
use serde::{Deserialize, Serialize};
use tracing::{debug, instrument, warn};

use crate::error::{Error, Result};

/// Re-export for API consumers.
pub use ed25519_dalek::VerifyingKey as PublicKey;

/// Deserialization format for a single trusted key in `trusted-keys.toml`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TrustedKeyEntry {
    /// Human-readable identifier (e.g. `"ggen-official"`, `"team-alice"`).
    pub key_id: String,
    /// Ed25519 public key as 64-character lowercase hex string.
    pub public_key_hex: String,
    /// Optional descriptive label.
    #[serde(default)]
    pub label: String,
}

/// Top-level TOML structure for `.ggen/trusted-keys.toml`.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct TrustedKeysConfig {
    /// List of trusted public keys.
    #[serde(default)]
    pub trusted_keys: Vec<TrustedKeyEntry>,
    /// Key IDs that have been revoked.
    #[serde(default)]
    pub revoked_keys: Vec<String>,
}

/// Ed25519 public keys are always 32 bytes.
const ED25519_PUBLIC_KEY_SIZE: usize = 32;

/// Manages trusted Ed25519 public keys and a revocation list.
#[derive(Debug, Clone)]
pub struct PkiManager {
    trusted_keys: HashMap<String, VerifyingKey>,
    key_revocation_list: HashSet<String>,
}

impl Default for PkiManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PkiManager {
    /// Create an empty PKI manager with no trusted keys.
    #[must_use]
    pub fn new() -> Self {
        Self {
            trusted_keys: HashMap::new(),
            key_revocation_list: HashSet::new(),
        }
    }

    /// Load trusted keys from a TOML file, merging into the existing state.
    #[instrument(name = "pki.load_trusted_keys", skip(self), fields(path = %path.display()))]
    pub fn load_trusted_keys(&mut self, path: &Path) -> Result<()> {
        let contents = std::fs::read_to_string(path)?;
        let config: TrustedKeysConfig =
            toml::from_str(&contents).map_err(|e| Error::ConfigError(format!(
                "Failed to parse {}: {}",
                path.display(),
                e
            )))?;

        debug!(
            "Loading {} trusted keys and {} revoked entries from {}",
            config.trusted_keys.len(),
            config.revoked_keys.len(),
            path.display()
        );

        for entry in &config.trusted_keys {
            let key_bytes = hex::decode(&entry.public_key_hex).map_err(|e| {
                Error::CryptoError(format!(
                    "Invalid hex for key '{}': {}",
                    entry.key_id, e
                ))
            })?;

            if key_bytes.len() != ED25519_PUBLIC_KEY_SIZE {
                return Err(Error::CryptoError(format!(
                    "Key '{}' must be {} bytes, got {}",
                    entry.key_id,
                    ED25519_PUBLIC_KEY_SIZE,
                    key_bytes.len()
                )));
            }

            let mut arr = [0u8; ED25519_PUBLIC_KEY_SIZE];
            arr.copy_from_slice(&key_bytes);

            let verifying_key = VerifyingKey::from_bytes(&arr).map_err(|e| {
                Error::CryptoError(format!(
                    "Invalid Ed25519 public key for '{}': {}",
                    entry.key_id, e
                ))
            })?;

            self.trusted_keys.insert(entry.key_id.clone(), verifying_key);
            debug!(key_id = %entry.key_id, "Trusted key loaded");
        }

        for revoked_id in &config.revoked_keys {
            self.key_revocation_list.insert(revoked_id.clone());
            warn!(key_id = %revoked_id, "Key revoked");
        }

        Ok(())
    }

    /// Check whether a key ID has been revoked.
    #[must_use]
    #[instrument(name = "pki.is_key_revoked", skip(self), fields(key_id))]
    pub fn is_key_revoked(&self, key_id: &str) -> bool {
        let revoked = self.key_revocation_list.contains(key_id);
        debug!(key_id, revoked, "Revocation check");
        revoked
    }

    /// Check whether a `VerifyingKey` is trusted and not revoked.
    #[must_use]
    #[instrument(name = "pki.verify_key_trusted", skip(self), fields(key_fingerprint))]
    pub fn verify_key_trusted(&self, key: &VerifyingKey) -> bool {
        let key_hex = hex::encode(key.to_bytes());
        tracing::Span::current().record(
            "key_fingerprint",
            &key_hex[..16.min(key_hex.len())],
        );

        for (key_id, trusted) in &self.trusted_keys {
            if trusted.to_bytes() == key.to_bytes() {
                if self.key_revocation_list.contains(key_id) {
                    debug!(key_id, "Key is trusted but has been revoked");
                    return false;
                }
                debug!(key_id, "Key is trusted and not revoked");
                return true;
            }
        }

        debug!("Key not found in trusted set");
        false
    }

    /// Look up a trusted key by its `key_id`.
    #[must_use]
    pub fn get_key(&self, key_id: &str) -> Option<&VerifyingKey> {
        self.trusted_keys.get(key_id)
    }

    /// Number of trusted keys currently loaded.
    #[must_use]
    pub fn trusted_key_count(&self) -> usize {
        self.trusted_keys.len()
    }

    /// Number of revoked key IDs.
    #[must_use]
    pub fn revoked_key_count(&self) -> usize {
        self.key_revocation_list.len()
    }

    /// Revoke a key by its `key_id`.
    pub fn revoke_key(&mut self, key_id: &str) -> bool {
        if !self.trusted_keys.contains_key(key_id) {
            return false;
        }
        self.key_revocation_list.insert(key_id.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn write_config(config: &TrustedKeysConfig) -> NamedTempFile {
        let mut f = NamedTempFile::new().expect("create temp file");
        let toml_str = toml::to_string_pretty(config).expect("serialize config");
        f.write_all(toml_str.as_bytes()).expect("write config to temp file");
        f
    }

    fn make_trusted_entry(id: &str) -> (TrustedKeyEntry, VerifyingKey) {
        let (_sk, vk) = ggen_receipt::generate_keypair();
        let entry = TrustedKeyEntry {
            key_id: id.to_string(),
            public_key_hex: hex::encode(vk.to_bytes()),
            label: format!("{} key", id),
        };
        (entry, vk)
    }

    #[test]
    fn test_load_empty_config() {
        let config = TrustedKeysConfig::default();
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load empty config");
        assert_eq!(mgr.trusted_key_count(), 0);
        assert_eq!(mgr.revoked_key_count(), 0);
    }

    #[test]
    fn test_load_single_trusted_key() {
        let (entry, vk) = make_trusted_entry("test-key-1");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load single key");
        assert_eq!(mgr.trusted_key_count(), 1);
        let fetched = mgr.get_key("test-key-1").expect("key should exist");
        assert_eq!(fetched.to_bytes(), vk.to_bytes());
    }

    #[test]
    fn test_load_multiple_keys_with_revocation() {
        let (entry_a, _vk_a) = make_trusted_entry("key-a");
        let (entry_b, _vk_b) = make_trusted_entry("key-b");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry_a, entry_b],
            revoked_keys: vec!["key-b".to_string()],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load config");
        assert_eq!(mgr.trusted_key_count(), 2);
        assert_eq!(mgr.revoked_key_count(), 1);
    }

    #[test]
    fn test_load_merges_with_existing_state() {
        let (entry_a, _) = make_trusted_entry("key-a");
        let config_a = TrustedKeysConfig {
            trusted_keys: vec![entry_a],
            revoked_keys: vec![],
        };
        let f_a = write_config(&config_a);
        let (entry_b, _) = make_trusted_entry("key-b");
        let config_b = TrustedKeysConfig {
            trusted_keys: vec![entry_b],
            revoked_keys: vec![],
        };
        let f_b = write_config(&config_b);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f_a.path()).expect("first load");
        mgr.load_trusted_keys(f_b.path()).expect("second load");
        assert_eq!(mgr.trusted_key_count(), 2);
    }

    #[test]
    fn test_load_rejects_invalid_hex() {
        let config = TrustedKeysConfig {
            trusted_keys: vec![TrustedKeyEntry {
                key_id: "bad-hex".to_string(),
                public_key_hex: "zzzz".to_string(),
                label: String::new(),
            }],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        let result = mgr.load_trusted_keys(f.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Invalid hex"));
    }

    #[test]
    fn test_load_rejects_wrong_length() {
        let config = TrustedKeysConfig {
            trusted_keys: vec![TrustedKeyEntry {
                key_id: "short-key".to_string(),
                public_key_hex: "ab".to_string(),
                label: String::new(),
            }],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        let result = mgr.load_trusted_keys(f.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("must be 32 bytes"));
    }

    #[test]
    fn test_load_nonexistent_file() {
        let mut mgr = PkiManager::new();
        assert!(mgr.load_trusted_keys(Path::new("/nonexistent/trusted-keys.toml")).is_err());
    }

    #[test]
    fn test_load_invalid_toml() {
        let mut f = NamedTempFile::new().expect("create temp file");
        f.write_all(b"this is not toml {{{{").expect("write garbage");
        let mut mgr = PkiManager::new();
        assert!(mgr.load_trusted_keys(f.path()).is_err());
    }

    #[test]
    fn test_is_key_revoked_true() {
        let config = TrustedKeysConfig {
            trusted_keys: vec![],
            revoked_keys: vec!["revoked-1".to_string()],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(mgr.is_key_revoked("revoked-1"));
    }

    #[test]
    fn test_is_key_revoked_false() {
        let config = TrustedKeysConfig::default();
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(!mgr.is_key_revoked("any-key"));
    }

    #[test]
    fn test_verify_key_trusted_success() {
        let (entry, vk) = make_trusted_entry("trusted-1");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(mgr.verify_key_trusted(&vk));
    }

    #[test]
    fn test_verify_key_trusted_revoked() {
        let (entry, vk) = make_trusted_entry("trusted-revoked");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec!["trusted-revoked".to_string()],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(!mgr.verify_key_trusted(&vk));
    }

    #[test]
    fn test_verify_key_trusted_unknown_key() {
        let mgr = PkiManager::new();
        let (_sk, vk) = ggen_receipt::generate_keypair();
        assert!(!mgr.verify_key_trusted(&vk));
    }

    #[test]
    fn test_verify_key_trusted_different_key_same_set() {
        let (entry_a, vk_a) = make_trusted_entry("key-a");
        let (_entry_b, vk_b) = make_trusted_entry("key-b");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry_a],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(mgr.verify_key_trusted(&vk_a));
        assert!(!mgr.verify_key_trusted(&vk_b));
    }

    #[test]
    fn test_revoke_key_new() {
        let (entry, _vk) = make_trusted_entry("to-revoke");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(mgr.revoke_key("to-revoke"));
        assert!(mgr.is_key_revoked("to-revoke"));
    }

    #[test]
    fn test_revoke_key_already_revoked() {
        let (entry, _vk) = make_trusted_entry("double-revoke");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec!["double-revoke".to_string()],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        assert!(!mgr.revoke_key("double-revoke"));
    }

    #[test]
    fn test_revoke_key_unknown() {
        let mut mgr = PkiManager::new();
        assert!(!mgr.revoke_key("nonexistent"));
    }

    #[test]
    fn test_trusted_keys_config_toml_roundtrip() {
        let config = TrustedKeysConfig {
            trusted_keys: vec![TrustedKeyEntry {
                key_id: "official".to_string(),
                public_key_hex: "a".repeat(64),
                label: "Official signing key".to_string(),
            }],
            revoked_keys: vec!["old-key".to_string()],
        };
        let toml_str = toml::to_string(&config).expect("serialize");
        let back: TrustedKeysConfig = toml::from_str(&toml_str).expect("deserialize");
        assert_eq!(config, back);
    }

    #[test]
    fn test_get_key_found() {
        let (entry, vk) = make_trusted_entry("lookup-test");
        let config = TrustedKeysConfig {
            trusted_keys: vec![entry],
            revoked_keys: vec![],
        };
        let f = write_config(&config);
        let mut mgr = PkiManager::new();
        mgr.load_trusted_keys(f.path()).expect("load");
        let found = mgr.get_key("lookup-test");
        assert!(found.is_some());
        assert_eq!(found.unwrap().to_bytes(), vk.to_bytes());
    }

    #[test]
    fn test_get_key_missing() {
        let mgr = PkiManager::new();
        assert!(mgr.get_key("nope").is_none());
    }
}
