//! Public Key Infrastructure (PKI) manager for trusted key management.
//!
//! This module provides a PKI subsystem for managing trusted Ed25519 public keys
//! used for receipt verification, package signing, and trust-chain validation.
//!
//! ## Features
//!
//! - **Trusted Key Store**: TOML-backed registry of trusted public keys
//! - **Key Addition/Removal**: Add and revoke trust for public keys with fingerprinting
//! - **Verification**: Verify Ed25519 signatures against the trusted key set
//! - **Config Discovery**: Auto-discover `.ggen/trusted-keys.toml` in project and user dirs
//! - **Fingerprint Generation**: SHA-256 fingerprints for human-readable key identification
//!
//! ## Configuration File Format
//!
//! The trusted keys file (`.ggen/trusted-keys.toml`) has the following structure:
//!
//! ```toml
//! version = "1.0"
//!
//! [[keys]]
//! name = "ggen-project-signing"
//! public_key = "base64-encoded-ed25519-public-key"
//! fingerprint = "sha256:hex-encoded-fingerprint"
//! purpose = "receipt-verification"
//! added_at = "2026-04-01T12:00:00Z"
//! expires_at = "2027-04-01T12:00:00Z"
//! ```
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::pki::{PkiManager, TrustedKeysConfig, TrustedKeyEntry};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load trusted keys from the default config path
//! let config = TrustedKeysConfig::load_from_project()?;
//! let mut manager = PkiManager::new(config);
//!
//! // Add a trusted key
//! manager.add_key(TrustedKeyEntry::new(
//!     "my-signing-key".to_string(),
//!     "base64-public-key-here".to_string(),
//!     "receipt-verification".to_string(),
//! ))?;
//!
//! // Verify a signature against all trusted keys
//! let message = b"important data";
//! let signature_hex = "hex-encoded-signature";
//! let verified = manager.verify_against_trusted(message, signature_hex)?;
//! # Ok(())
//! # }
//! ```

use base64::{engine::general_purpose, Engine as _};
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Verifier, VerifyingKey};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Current version of the trusted-keys.toml config format.
const CONFIG_VERSION: &str = "1.0";

/// Default filename for the trusted keys configuration.
const TRUSTED_KEYS_FILENAME: &str = "trusted-keys.toml";

/// Supported purposes for trusted keys.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum KeyPurpose {
    /// Key is used for receipt verification.
    ReceiptVerification,
    /// Key is used for package signing.
    PackageSigning,
    /// Key is used for template signing.
    TemplateSigning,
    /// Key is used for general-purpose signing.
    General,
}

impl KeyPurpose {
    /// Parse a purpose string into a `KeyPurpose` enum.
    pub fn from_str_lossy(s: &str) -> Self {
        match s {
            "receipt-verification" => Self::ReceiptVerification,
            "package-signing" => Self::PackageSigning,
            "template-signing" => Self::TemplateSigning,
            _ => Self::General,
        }
    }
}

impl std::fmt::Display for KeyPurpose {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReceiptVerification => write!(f, "receipt-verification"),
            Self::PackageSigning => write!(f, "package-signing"),
            Self::TemplateSigning => write!(f, "template-signing"),
            Self::General => write!(f, "general"),
        }
    }
}

/// A single trusted key entry in the key store.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrustedKeyEntry {
    /// Human-readable name for this key (e.g., "ggen-project-signing").
    pub name: String,
    /// Base64-encoded Ed25519 public key.
    pub public_key: String,
    /// SHA-256 fingerprint of the public key, prefixed with "sha256:".
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fingerprint: Option<String>,
    /// Purpose of the trusted key.
    pub purpose: String,
    /// Timestamp when the key was added to the trust store.
    pub added_at: DateTime<Utc>,
    /// Optional expiration timestamp for the trusted key.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires_at: Option<DateTime<Utc>>,
}

impl TrustedKeyEntry {
    /// Create a new trusted key entry.
    ///
    /// The fingerprint is computed automatically from the public key bytes.
    /// The `added_at` timestamp is set to the current UTC time.
    pub fn new(name: String, public_key: String, purpose: String) -> Self {
        let fingerprint = Self::compute_fingerprint(&public_key);
        Self {
            name,
            public_key,
            fingerprint: Some(fingerprint),
            purpose,
            added_at: Utc::now(),
            expires_at: None,
        }
    }

    /// Create a new trusted key entry with an explicit expiration time.
    pub fn with_expiry(
        name: String, public_key: String, purpose: String, expires_at: DateTime<Utc>,
    ) -> Self {
        let mut entry = Self::new(name, public_key, purpose);
        entry.expires_at = Some(expires_at);
        entry
    }

    /// Compute a SHA-256 fingerprint for a base64-encoded public key.
    ///
    /// Returns a string of the form `"sha256:<hex>"`.
    pub fn compute_fingerprint(public_key_b64: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(public_key_b64.as_bytes());
        let hash = hasher.finalize();
        format!("sha256:{}", hex::encode(hash))
    }

    /// Decode the base64 public key into raw bytes.
    pub fn decode_public_key(&self) -> Result<Vec<u8>> {
        general_purpose::STANDARD
            .decode(&self.public_key)
            .map_err(|e| Error::with_context("Failed to decode public key base64", &e.to_string()))
    }

    /// Check whether this key entry has expired.
    pub fn is_expired(&self) -> bool {
        match self.expires_at {
            Some(expiry) => Utc::now() > expiry,
            None => false,
        }
    }

    /// Parse the purpose string into a `KeyPurpose` enum.
    pub fn parsed_purpose(&self) -> KeyPurpose {
        KeyPurpose::from_str_lossy(&self.purpose)
    }
}

/// Configuration file structure for `.ggen/trusted-keys.toml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrustedKeysConfig {
    /// Config format version (currently "1.0").
    pub version: String,
    /// List of trusted key entries.
    #[serde(default)]
    pub keys: Vec<TrustedKeyEntry>,
}

impl Default for TrustedKeysConfig {
    fn default() -> Self {
        Self {
            version: CONFIG_VERSION.to_string(),
            keys: Vec::new(),
        }
    }
}

impl TrustedKeysConfig {
    /// Create a new empty config with the current format version.
    pub fn new() -> Self {
        Self::default()
    }

    /// Load the trusted keys configuration from a specific file path.
    pub fn load_from_path(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::with_context("Failed to read trusted-keys.toml", &e.to_string()))?;

        let config: TrustedKeysConfig = toml::from_str(&content).map_err(|e| {
            Error::with_context("Failed to parse trusted-keys.toml", &e.to_string())
        })?;

        // Validate version
        if config.version != CONFIG_VERSION {
            return Err(Error::with_context(
                "Unsupported trusted-keys.toml version",
                &format!("expected {}, got {}", CONFIG_VERSION, config.version),
            ));
        }

        Ok(config)
    }

    /// Load trusted keys from the project-local `.ggen/trusted-keys.toml`.
    ///
    /// Falls back to an empty config if the file does not exist.
    pub fn load_from_project() -> Result<Self> {
        let path = Self::project_config_path()?;
        Self::load_from_path(&path)
    }

    /// Load trusted keys from the user-level config directory.
    ///
    /// On macOS: `~/Library/Application Support/ggen/trusted-keys.toml`
    /// On Linux: `~/.config/ggen/trusted-keys.toml`
    /// On Windows: `C:\Users\<user>\AppData\Roaming\ggen\trusted-keys.toml`
    pub fn load_from_user_dir() -> Result<Self> {
        let path = Self::user_config_path()?;
        Self::load_from_path(&path)
    }

    /// Save the configuration to a specific file path.
    pub fn save_to_path(&self, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                Error::with_context("Failed to create config directory", &e.to_string())
            })?;
        }

        let content = toml::to_string_pretty(self).map_err(|e| {
            Error::with_context("Failed to serialize trusted-keys.toml", &e.to_string())
        })?;

        std::fs::write(path, content).map_err(|e| {
            Error::with_context("Failed to write trusted-keys.toml", &e.to_string())
        })?;

        Ok(())
    }

    /// Get the project-local config path (`.ggen/trusted-keys.toml`).
    pub fn project_config_path() -> Result<PathBuf> {
        Ok(PathBuf::from(".ggen").join(TRUSTED_KEYS_FILENAME))
    }

    /// Get the user-level config path.
    pub fn user_config_path() -> Result<PathBuf> {
        let base =
            dirs::data_dir().ok_or_else(|| Error::new("Cannot determine user data directory"))?;
        Ok(base.join("ggen").join(TRUSTED_KEYS_FILENAME))
    }
}

/// PKI Manager for trusted key operations.
///
/// Provides methods for adding, removing, and looking up trusted keys,
/// as well as verifying signatures against the trust store.
pub struct PkiManager {
    /// The underlying configuration (keys are the source of truth).
    config: TrustedKeysConfig,
    /// In-memory index: fingerprint -> key index in config.keys.
    fingerprint_index: HashMap<String, usize>,
    /// In-memory index: name -> key index in config.keys.
    name_index: HashMap<String, usize>,
}

impl PkiManager {
    /// Create a new PKI manager from the given configuration.
    ///
    /// The manager builds in-memory indexes for fast lookups by name and fingerprint.
    pub fn new(config: TrustedKeysConfig) -> Self {
        let mut fingerprint_index = HashMap::new();
        let mut name_index = HashMap::new();

        for (i, key) in config.keys.iter().enumerate() {
            if let Some(fp) = &key.fingerprint {
                fingerprint_index.insert(fp.clone(), i);
            }
            name_index.insert(key.name.clone(), i);
        }

        Self {
            config,
            fingerprint_index,
            name_index,
        }
    }

    /// Create a PKI manager from the project-local config.
    pub fn from_project() -> Result<Self> {
        let config = TrustedKeysConfig::load_from_project()?;
        Ok(Self::new(config))
    }

    /// Create a PKI manager from the user-level config.
    pub fn from_user_dir() -> Result<Self> {
        let config = TrustedKeysConfig::load_from_user_dir()?;
        Ok(Self::new(config))
    }

    /// Add a trusted key to the manager.
    ///
    /// Returns an error if a key with the same name already exists.
    /// After adding, call [`save`](Self::save_to_project) or [`save_to_user_dir`](Self::save_to_user_dir) to persist.
    pub fn add_key(&mut self, entry: TrustedKeyEntry) -> Result<()> {
        if self.name_index.contains_key(&entry.name) {
            return Err(Error::with_context(
                "Trusted key already exists",
                &format!("key named '{}' is already in the trust store", entry.name),
            ));
        }

        // Check for duplicate public key (by fingerprint)
        if let Some(fp) = &entry.fingerprint {
            if self.fingerprint_index.contains_key(fp) {
                return Err(Error::with_context(
                    "Duplicate public key detected",
                    &format!("a key with fingerprint '{}' already exists", fp),
                ));
            }
        }

        let idx = self.config.keys.len();
        if let Some(fp) = &entry.fingerprint {
            self.fingerprint_index.insert(fp.clone(), idx);
        }
        self.name_index.insert(entry.name.clone(), idx);
        self.config.keys.push(entry);

        Ok(())
    }

    /// Remove a trusted key by name.
    ///
    /// Returns the removed entry, or an error if the name is not found.
    pub fn remove_key(&mut self, name: &str) -> Result<TrustedKeyEntry> {
        let idx = self
            .name_index
            .remove(name)
            .ok_or_else(|| Error::with_context("Trusted key not found", name))?;

        let entry = self.config.keys.remove(idx);

        // Rebuild indexes since indices shifted
        self.rebuild_indexes();

        Ok(entry)
    }

    /// Look up a trusted key by name.
    pub fn get_key(&self, name: &str) -> Option<&TrustedKeyEntry> {
        self.name_index
            .get(name)
            .and_then(|&idx| self.config.keys.get(idx))
    }

    /// Look up a trusted key by fingerprint.
    pub fn get_key_by_fingerprint(&self, fingerprint: &str) -> Option<&TrustedKeyEntry> {
        self.fingerprint_index
            .get(fingerprint)
            .and_then(|&idx| self.config.keys.get(idx))
    }

    /// Get all trusted key entries.
    pub fn all_keys(&self) -> &[TrustedKeyEntry] {
        &self.config.keys
    }

    /// Get all non-expired trusted keys.
    pub fn active_keys(&self) -> Vec<&TrustedKeyEntry> {
        self.config
            .keys
            .iter()
            .filter(|k| !k.is_expired())
            .collect()
    }

    /// Get the number of trusted keys.
    pub fn key_count(&self) -> usize {
        self.config.keys.len()
    }

    /// Verify a signature against a specific trusted key by name.
    ///
    /// # Arguments
    ///
    /// * `message` - The original message bytes that were signed
    /// * `signature_hex` - Hex-encoded Ed25519 signature
    /// * `key_name` - Name of the trusted key to verify against
    ///
    /// # Returns
    ///
    /// `Ok(true)` if the signature is valid, `Ok(false)` if invalid.
    pub fn verify_with_key(
        &self, message: &[u8], signature_hex: &str, key_name: &str,
    ) -> Result<bool> {
        let entry = self
            .get_key(key_name)
            .ok_or_else(|| Error::with_context("Trusted key not found", key_name))?;

        if entry.is_expired() {
            return Err(Error::with_context("Trusted key has expired", key_name));
        }

        verify_ed25519(message, signature_hex, &entry.public_key)
    }

    /// Verify a signature against all active trusted keys.
    ///
    /// Returns `Ok(Some(name))` with the name of the first trusted key that
    /// validates the signature, or `Ok(None)` if no key matches.
    pub fn verify_against_trusted(
        &self, message: &[u8], signature_hex: &str,
    ) -> Result<Option<String>> {
        for entry in self.active_keys() {
            if verify_ed25519(message, signature_hex, &entry.public_key)? {
                return Ok(Some(entry.name.clone()));
            }
        }
        Ok(None)
    }

    /// Verify a signature against trusted keys with a specific purpose filter.
    ///
    /// Only keys matching the given purpose are checked.
    pub fn verify_with_purpose(
        &self, message: &[u8], signature_hex: &str, purpose: &KeyPurpose,
    ) -> Result<Option<String>> {
        for entry in self.active_keys() {
            if entry.parsed_purpose() == *purpose {
                if verify_ed25519(message, signature_hex, &entry.public_key)? {
                    return Ok(Some(entry.name.clone()));
                }
            }
        }
        Ok(None)
    }

    /// Save the current state to the project-local `.ggen/trusted-keys.toml`.
    pub fn save_to_project(&self) -> Result<()> {
        let path = TrustedKeysConfig::project_config_path()?;
        self.config.save_to_path(&path)
    }

    /// Save the current state to the user-level config directory.
    pub fn save_to_user_dir(&self) -> Result<()> {
        let path = TrustedKeysConfig::user_config_path()?;
        self.config.save_to_path(&path)
    }

    /// Consume the manager and return the underlying configuration.
    pub fn into_config(self) -> TrustedKeysConfig {
        self.config
    }

    /// Rebuild in-memory indexes from the current config keys.
    fn rebuild_indexes(&mut self) {
        self.fingerprint_index.clear();
        self.name_index.clear();

        for (i, key) in self.config.keys.iter().enumerate() {
            if let Some(fp) = &key.fingerprint {
                self.fingerprint_index.insert(fp.clone(), i);
            }
            self.name_index.insert(key.name.clone(), i);
        }
    }
}

/// Verify an Ed25519 signature given a message, hex-encoded signature, and base64-encoded public key.
///
/// This is a standalone function that can be used without a `PkiManager`.
pub fn verify_ed25519(message: &[u8], signature_hex: &str, public_key_b64: &str) -> Result<bool> {
    let pk_bytes = general_purpose::STANDARD
        .decode(public_key_b64)
        .map_err(|e| Error::with_context("Failed to decode public key", &e.to_string()))?;

    let sig_bytes = hex::decode(signature_hex)
        .map_err(|e| Error::with_context("Failed to decode signature hex", &e.to_string()))?;

    let pk_array: [u8; 32] = pk_bytes.try_into()
        .map_err(|_| Error::new("Ed25519 public key must be 32 bytes"))?;
    let verifying_key = VerifyingKey::from_bytes(&pk_array)
        .map_err(|e| Error::with_context("Invalid Ed25519 public key", &e.to_string()))?;

    let signature = Signature::from_slice(&sig_bytes)
        .map_err(|_| Error::new("Invalid Ed25519 signature format"))?;

    match verifying_key.verify(message, &signature) {
        Ok(()) => Ok(true),
        Err(_) => Ok(false),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_dalek::{Signer, SigningKey};
    use rand::rngs::OsRng;

    /// Helper: generate a valid keypair and return (TrustedKeyEntry, SigningKey).
    fn generate_test_key(name: &str, purpose: &str) -> (TrustedKeyEntry, SigningKey) {
        let signing_key = SigningKey::generate(&mut OsRng);
        let verifying_key = signing_key.verifying_key();
        let pk_bytes = verifying_key.to_bytes();
        let pk_b64 = general_purpose::STANDARD.encode(pk_bytes);
        let entry = TrustedKeyEntry::new(name.to_string(), pk_b64, purpose.to_string());
        (entry, signing_key)
    }

    /// Helper: sign a message and return hex-encoded signature.
    fn sign_message(signing_key: &SigningKey, message: &[u8]) -> String {
        let signature = signing_key.sign(message);
        hex::encode(signature.to_bytes())
    }

    #[test]
    fn test_trusted_key_entry_creation() {
        let entry = TrustedKeyEntry::new(
            "test-key".to_string(),
            "dGVzdA==".to_string(),
            "receipt-verification".to_string(),
        );

        assert_eq!(entry.name, "test-key");
        assert_eq!(entry.public_key, "dGVzdA==");
        assert!(entry.fingerprint.is_some());
        assert!(entry.fingerprint.as_ref().unwrap().starts_with("sha256:"));
        assert!(!entry.is_expired());
        assert_eq!(entry.parsed_purpose(), KeyPurpose::ReceiptVerification);
    }

    #[test]
    fn test_trusted_key_entry_expiry() {
        let past = Utc::now() - chrono::Duration::days(1);
        let future = Utc::now() + chrono::Duration::days(30);

        let expired = TrustedKeyEntry::with_expiry(
            "expired-key".to_string(),
            "dGVzdA==".to_string(),
            "general".to_string(),
            past,
        );
        assert!(expired.is_expired());

        let valid = TrustedKeyEntry::with_expiry(
            "valid-key".to_string(),
            "dGVzdA==".to_string(),
            "general".to_string(),
            future,
        );
        assert!(!valid.is_expired());
    }

    #[test]
    fn test_fingerprint_deterministic() {
        let fp1 = TrustedKeyEntry::compute_fingerprint("abc123");
        let fp2 = TrustedKeyEntry::compute_fingerprint("abc123");
        assert_eq!(fp1, fp2);

        let fp3 = TrustedKeyEntry::compute_fingerprint("xyz789");
        assert_ne!(fp1, fp3);
    }

    #[test]
    fn test_trusted_keys_config_default() {
        let config = TrustedKeysConfig::new();
        assert_eq!(config.version, "1.0");
        assert!(config.keys.is_empty());
    }

    #[test]
    fn test_trusted_keys_config_save_and_load() {
        let dir = tempfile::tempdir().expect("Failed to create tempdir");
        let path = dir.path().join("trusted-keys.toml");

        let (entry, _) = generate_test_key("test-key", "receipt-verification");

        let mut config = TrustedKeysConfig::new();
        config.keys.push(entry);

        config.save_to_path(&path).expect("Save failed");
        assert!(path.exists());

        let loaded = TrustedKeysConfig::load_from_path(&path).expect("Load failed");
        assert_eq!(loaded.version, "1.0");
        assert_eq!(loaded.keys.len(), 1);
        assert_eq!(loaded.keys[0].name, "test-key");
    }

    #[test]
    fn test_pki_manager_add_and_lookup() {
        let (entry, _) = generate_test_key("my-key", "package-signing");
        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);

        manager.add_key(entry).expect("Add failed");
        assert_eq!(manager.key_count(), 1);

        let found = manager.get_key("my-key");
        assert!(found.is_some());
        assert_eq!(found.unwrap().purpose, "package-signing");

        let not_found = manager.get_key("nonexistent");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_pki_manager_duplicate_name_rejected() {
        let (entry1, _) = generate_test_key("same-name", "general");
        let (entry2, _) = generate_test_key("same-name", "general");

        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);

        manager.add_key(entry1).expect("First add should succeed");
        let result = manager.add_key(entry2);
        assert!(result.is_err());
    }

    #[test]
    fn test_pki_manager_duplicate_public_key_rejected() {
        let (entry1, _) = generate_test_key("key-a", "general");
        let mut entry2 = TrustedKeyEntry::new(
            "key-b".to_string(),
            entry1.public_key.clone(),
            "general".to_string(),
        );

        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);

        manager.add_key(entry1).expect("First add should succeed");
        let result = manager.add_key(entry2);
        assert!(result.is_err());
    }

    #[test]
    fn test_pki_manager_remove_key() {
        let (entry, _) = generate_test_key("remove-me", "general");
        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);

        manager.add_key(entry).expect("Add failed");
        assert_eq!(manager.key_count(), 1);

        let removed = manager.remove_key("remove-me").expect("Remove failed");
        assert_eq!(removed.name, "remove-me");
        assert_eq!(manager.key_count(), 0);
    }

    #[test]
    fn test_pki_manager_verify_signature() {
        let (entry, signing_key) = generate_test_key("signing-key", "receipt-verification");
        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);
        manager.add_key(entry).expect("Add failed");

        let message = b"important data to sign";
        let signature_hex = sign_message(&signing_key, message);

        // Verify with specific key
        let result = manager.verify_with_key(message, &signature_hex, "signing-key");
        assert!(result.is_ok());
        assert!(result.unwrap());

        // Verify against all trusted keys
        let trusted_result = manager.verify_against_trusted(message, &signature_hex);
        assert!(trusted_result.is_ok());
        assert_eq!(trusted_result.unwrap(), Some("signing-key".to_string()));
    }

    #[test]
    fn test_pki_manager_verify_wrong_signature() {
        let (entry, signing_key) = generate_test_key("signing-key", "receipt-verification");
        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);
        manager.add_key(entry).expect("Add failed");

        let message = b"important data to sign";
        let signature_hex = sign_message(&signing_key, message);

        // Wrong message
        let result = manager.verify_with_key(b"wrong message", &signature_hex, "signing-key");
        assert!(result.is_ok());
        assert!(!result.unwrap());
    }

    #[test]
    fn test_pki_manager_verify_with_purpose_filter() {
        let (entry1, sk1) = generate_test_key("receipt-key", "receipt-verification");
        let (entry2, _sk2) = generate_test_key("package-key", "package-signing");

        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);
        manager.add_key(entry1).expect("Add failed");
        manager.add_key(entry2).expect("Add failed");

        let message = b"test data";
        let sig_receipt = sign_message(&sk1, message);

        // Verify with purpose filter should only match the receipt key
        let result =
            manager.verify_with_purpose(message, &sig_receipt, &KeyPurpose::ReceiptVerification);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some("receipt-key".to_string()));

        // Package purpose should not match
        let result_pkg =
            manager.verify_with_purpose(message, &sig_receipt, &KeyPurpose::PackageSigning);
        assert!(result_pkg.is_ok());
        assert!(result_pkg.unwrap().is_none());
    }

    #[test]
    fn test_pki_manager_active_keys_excludes_expired() {
        let past = Utc::now() - chrono::Duration::days(1);
        let (entry, _) = generate_test_key("expired-key", "general");
        let expired_entry =
            TrustedKeyEntry::with_expiry(entry.name.clone(), entry.public_key, entry.purpose, past);

        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);
        manager.add_key(expired_entry).expect("Add failed");

        assert_eq!(manager.key_count(), 1);
        assert_eq!(manager.active_keys().len(), 0);
    }

    #[test]
    fn test_pki_manager_save_and_reload() {
        let dir = tempfile::tempdir().expect("Failed to create tempdir");
        let path = dir.path().join("trusted-keys.toml");

        let (entry, _) = generate_test_key("persistent-key", "template-signing");
        let config = TrustedKeysConfig::new();
        let mut manager = PkiManager::new(config);
        manager.add_key(entry).expect("Add failed");

        // Save
        manager.config.save_to_path(&path).expect("Save failed");

        // Reload
        let loaded_config = TrustedKeysConfig::load_from_path(&path).expect("Load failed");
        let manager2 = PkiManager::new(loaded_config);
        assert_eq!(manager2.key_count(), 1);
        assert!(manager2.get_key("persistent-key").is_some());
    }

    #[test]
    fn test_verify_ed25519_standalone() {
        let signing_key = SigningKey::generate(&mut OsRng);
        let verifying_key = signing_key.verifying_key();
        let pk_b64 = general_purpose::STANDARD.encode(verifying_key.to_bytes());

        let message = b"standalone test";
        let signature = signing_key.sign(message);
        let sig_hex = hex::encode(signature.to_bytes());

        let result = verify_ed25519(message, &sig_hex, &pk_b64);
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_verify_ed25519_invalid_inputs() {
        // Invalid base64
        let result = verify_ed25519(b"msg", "deadbeef", "not-valid-base64!!!");
        assert!(result.is_err());

        // Invalid hex signature
        let (_, signing_key) = generate_test_key("k", "g");
        let verifying_key = signing_key.verifying_key();
        let pk_b64 = general_purpose::STANDARD.encode(verifying_key.to_bytes());
        let result = verify_ed25519(b"msg", "zzzz", &pk_b64);
        assert!(result.is_err());
    }

    #[test]
    fn test_key_purpose_display() {
        assert_eq!(
            KeyPurpose::ReceiptVerification.to_string(),
            "receipt-verification"
        );
        assert_eq!(KeyPurpose::PackageSigning.to_string(), "package-signing");
        assert_eq!(KeyPurpose::TemplateSigning.to_string(), "template-signing");
        assert_eq!(KeyPurpose::General.to_string(), "general");
    }

    #[test]
    fn test_key_purpose_from_str() {
        assert_eq!(
            KeyPurpose::from_str_lossy("receipt-verification"),
            KeyPurpose::ReceiptVerification
        );
        assert_eq!(
            KeyPurpose::from_str_lossy("package-signing"),
            KeyPurpose::PackageSigning
        );
        assert_eq!(
            KeyPurpose::from_str_lossy("template-signing"),
            KeyPurpose::TemplateSigning
        );
        assert_eq!(KeyPurpose::from_str_lossy("unknown"), KeyPurpose::General);
        assert_eq!(KeyPurpose::from_str_lossy(""), KeyPurpose::General);
    }
}
