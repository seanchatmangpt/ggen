//! WORM Evidence Bundles
//!
//! Write-Once-Read-Many storage for regulatory compliance.
//! Once sealed, bundles cannot be modified - only verified.
//!
//! # Compliance Features
//!
//! - **Immutability**: Sealed bundles cannot be modified
//! - **Chain Integrity**: Bundles link to previous bundles for continuity
//! - **Retention Policies**: Configurable retention periods with legal hold
//! - **Cryptographic Verification**: Ed25519 signatures on all seals
//! - **Audit Trail**: Complete verification history
//!
//! # Supported Frameworks
//!
//! - SOX (Sarbanes-Oxley)
//! - GDPR (General Data Protection Regulation)
//! - HIPAA (Health Insurance Portability and Accountability Act)
//! - SEC Rule 17a-4 (Financial Records)

use crate::crypto::{hash_sha256, KeyPair, Signature};
use crate::error::{McpError, McpResult};
use crate::types::EvidenceBundle;
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// WORM-compliant bundle wrapper
///
/// Once an `EvidenceBundle` is sealed into a `WormBundle`, its contents
/// become immutable. Any attempt to modify the bundle will invalidate
/// the cryptographic seal.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WormBundle {
    /// The sealed bundle (immutable once sealed)
    bundle: EvidenceBundle,
    /// Seal metadata with cryptographic proof
    seal: BundleSeal,
    /// Retention policy for compliance
    retention: RetentionPolicy,
}

/// Cryptographic seal for a WORM bundle
///
/// Contains all metadata needed to verify bundle integrity
/// and chain continuity.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleSeal {
    /// Timestamp when the bundle was sealed (UTC)
    pub sealed_at: DateTime<Utc>,
    /// Identity of the sealer (e.g., "system", "auditor@company.com")
    pub sealed_by: String,
    /// SHA-256 hash of the sealed bundle contents
    pub seal_hash: String,
    /// Ed25519 signature over the seal hash
    pub signature: String,
    /// Hash of the previous bundle for chain continuity (None for genesis)
    pub prev_bundle_hash: Option<String>,
}

/// Retention policy for compliance requirements
///
/// Defines how long a bundle must be retained and what
/// happens when the retention period expires.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetentionPolicy {
    /// Minimum retention period in days
    pub min_retention_days: i64,
    /// Legal hold flag (cannot delete even after retention expires)
    pub legal_hold: bool,
    /// Regulatory framework (e.g., "SOX", "GDPR", "HIPAA", "SEC-17a-4")
    pub framework: String,
    /// Action to take when retention period expires
    pub on_expiry: ExpiryAction,
}

impl RetentionPolicy {
    /// Create a SOX-compliant retention policy (7 years)
    pub fn sox() -> Self {
        Self {
            min_retention_days: 7 * 365, // 7 years
            legal_hold: false,
            framework: "SOX".to_string(),
            on_expiry: ExpiryAction::Archive,
        }
    }

    /// Create a GDPR-compliant retention policy (configurable)
    pub fn gdpr(retention_days: i64) -> Self {
        Self {
            min_retention_days: retention_days,
            legal_hold: false,
            framework: "GDPR".to_string(),
            on_expiry: ExpiryAction::Delete, // Right to erasure
        }
    }

    /// Create a HIPAA-compliant retention policy (6 years)
    pub fn hipaa() -> Self {
        Self {
            min_retention_days: 6 * 365, // 6 years
            legal_hold: false,
            framework: "HIPAA".to_string(),
            on_expiry: ExpiryAction::Archive,
        }
    }

    /// Create an SEC 17a-4 compliant retention policy (6 years)
    pub fn sec_17a4() -> Self {
        Self {
            min_retention_days: 6 * 365, // 6 years
            legal_hold: false,
            framework: "SEC-17a-4".to_string(),
            on_expiry: ExpiryAction::Review,
        }
    }

    /// Set legal hold on the policy
    pub fn with_legal_hold(mut self) -> Self {
        self.legal_hold = true;
        self
    }

    /// Get retention duration
    pub fn retention_duration(&self) -> Duration {
        Duration::days(self.min_retention_days)
    }
}

/// Action to take when retention period expires
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExpiryAction {
    /// Move to cold/archive storage
    Archive,
    /// Delete the bundle (with audit trail)
    Delete,
    /// Flag for manual review
    Review,
}

impl WormBundle {
    /// Seal a bundle, making it immutable
    ///
    /// # Arguments
    ///
    /// * `bundle` - The evidence bundle to seal
    /// * `sealed_by` - Identity of the sealer
    /// * `retention` - Retention policy for compliance
    /// * `prev_bundle` - Optional reference to previous bundle for chain continuity
    /// * `keypair` - Cryptographic keypair for signing the seal
    ///
    /// # Returns
    ///
    /// A sealed WORM bundle that cannot be modified
    pub fn seal(
        bundle: EvidenceBundle,
        sealed_by: &str,
        retention: RetentionPolicy,
        prev_bundle: Option<&WormBundle>,
        keypair: &KeyPair,
    ) -> McpResult<Self> {
        let sealed_at = Utc::now();

        // Compute the seal hash from bundle contents
        let bundle_json = serde_json::to_string(&bundle)?;
        let seal_hash = hash_sha256(bundle_json.as_bytes());

        // Get previous bundle hash for chain continuity
        let prev_bundle_hash = prev_bundle.map(|pb| pb.seal.seal_hash.clone());

        // Create the data to sign (includes chain link)
        let sign_data = match &prev_bundle_hash {
            Some(prev) => format!(
                "WORM-SEAL|{}|{}|{}|{}",
                sealed_at.to_rfc3339(),
                sealed_by,
                seal_hash,
                prev
            ),
            None => format!(
                "WORM-SEAL|{}|{}|{}|GENESIS",
                sealed_at.to_rfc3339(),
                sealed_by,
                seal_hash
            ),
        };

        // Sign the seal
        let sig = keypair.sign(sign_data.as_bytes());
        let signature = sig.to_hex();

        let seal = BundleSeal {
            sealed_at,
            sealed_by: sealed_by.to_string(),
            seal_hash,
            signature,
            prev_bundle_hash,
        };

        Ok(Self {
            bundle,
            seal,
            retention,
        })
    }

    /// Verify bundle integrity
    ///
    /// Checks that:
    /// 1. The seal hash matches the bundle contents
    /// 2. The signature is valid
    ///
    /// # Arguments
    ///
    /// * `keypair` - Keypair containing the public key for verification
    ///
    /// # Returns
    ///
    /// `Ok(())` if verification succeeds, `Err` otherwise
    pub fn verify(&self, keypair: &KeyPair) -> McpResult<()> {
        // Recompute the seal hash from bundle contents
        let bundle_json = serde_json::to_string(&self.bundle)?;
        let computed_hash = hash_sha256(bundle_json.as_bytes());

        // Verify hash matches
        if computed_hash != self.seal.seal_hash {
            return Err(McpError::ChainError(format!(
                "Seal hash mismatch: expected {}, computed {}",
                self.seal.seal_hash, computed_hash
            )));
        }

        // Reconstruct the signed data
        let sign_data = match &self.seal.prev_bundle_hash {
            Some(prev) => format!(
                "WORM-SEAL|{}|{}|{}|{}",
                self.seal.sealed_at.to_rfc3339(),
                self.seal.sealed_by,
                self.seal.seal_hash,
                prev
            ),
            None => format!(
                "WORM-SEAL|{}|{}|{}|GENESIS",
                self.seal.sealed_at.to_rfc3339(),
                self.seal.sealed_by,
                self.seal.seal_hash
            ),
        };

        // Verify signature
        let sig_bytes = hex::decode(&self.seal.signature)
            .map_err(|e| McpError::InvalidSignature(e.to_string()))?;
        let signature = Signature::from_bytes(sig_bytes)?;

        keypair.verify(sign_data.as_bytes(), &signature)?;

        Ok(())
    }

    /// Verify chain integrity with previous bundle
    ///
    /// Checks that this bundle correctly links to the previous bundle
    /// in the chain.
    ///
    /// # Arguments
    ///
    /// * `prev` - The previous bundle in the chain
    ///
    /// # Returns
    ///
    /// `Ok(())` if the chain is valid, `Err` otherwise
    pub fn verify_chain(&self, prev: &WormBundle) -> McpResult<()> {
        match &self.seal.prev_bundle_hash {
            Some(prev_hash) => {
                if prev_hash != &prev.seal.seal_hash {
                    return Err(McpError::ChainError(format!(
                        "Chain broken: expected prev_hash {}, found {}",
                        prev.seal.seal_hash, prev_hash
                    )));
                }
                Ok(())
            }
            None => Err(McpError::ChainError(
                "Bundle has no previous hash (genesis bundle)".to_string(),
            )),
        }
    }

    /// Check if bundle has expired its retention period
    ///
    /// Note: A bundle on legal hold never expires.
    pub fn is_expired(&self) -> bool {
        if self.retention.legal_hold {
            return false;
        }

        let expiry_date = self.seal.sealed_at + self.retention.retention_duration();
        Utc::now() > expiry_date
    }

    /// Check if bundle can be deleted
    ///
    /// A bundle can only be deleted if:
    /// 1. It has expired its retention period
    /// 2. It is not on legal hold
    /// 3. The expiry action is `Delete`
    pub fn can_delete(&self) -> bool {
        self.is_expired()
            && !self.retention.legal_hold
            && self.retention.on_expiry == ExpiryAction::Delete
    }

    /// Get bundle contents (read-only)
    ///
    /// Returns an immutable reference to the sealed evidence bundle.
    pub fn contents(&self) -> &EvidenceBundle {
        &self.bundle
    }

    /// Get the seal metadata
    pub fn get_seal(&self) -> &BundleSeal {
        &self.seal
    }

    /// Get the retention policy
    pub fn retention(&self) -> &RetentionPolicy {
        &self.retention
    }

    /// Get the bundle ID
    pub fn bundle_id(&self) -> &str {
        &self.bundle.bundle_id
    }

    /// Get the seal hash (used for chaining)
    pub fn seal_hash(&self) -> &str {
        &self.seal.seal_hash
    }

    /// Get the expiry date
    pub fn expiry_date(&self) -> Option<DateTime<Utc>> {
        if self.retention.legal_hold {
            None // No expiry on legal hold
        } else {
            Some(self.seal.sealed_at + self.retention.retention_duration())
        }
    }

    /// Export bundle to file with integrity verification
    ///
    /// # Arguments
    ///
    /// * `path` - File path to export to
    ///
    /// # Returns
    ///
    /// `Ok(())` if export succeeds
    pub fn export(&self, path: &Path) -> McpResult<()> {
        let json = serde_json::to_string_pretty(self)?;
        fs::write(path, json)?;
        Ok(())
    }

    /// Import bundle from file with integrity verification
    ///
    /// # Arguments
    ///
    /// * `path` - File path to import from
    /// * `keypair` - Keypair for signature verification
    ///
    /// # Returns
    ///
    /// The verified WORM bundle
    pub fn import(path: &Path, keypair: &KeyPair) -> McpResult<Self> {
        let json = fs::read_to_string(path)?;
        let bundle: Self = serde_json::from_str(&json)?;

        // Verify integrity after import
        bundle.verify(keypair)?;

        Ok(bundle)
    }
}

/// WORM storage manager for managing multiple sealed bundles
///
/// Provides chain verification, expiry processing, and bundle management.
pub struct WormStorage {
    /// Stored bundles in chain order
    bundles: Vec<WormBundle>,
    /// Keypair for signing and verification
    keypair: KeyPair,
}

impl WormStorage {
    /// Create a new WORM storage manager
    ///
    /// # Arguments
    ///
    /// * `keypair` - Keypair for signing new bundles and verifying existing ones
    pub fn new(keypair: KeyPair) -> Self {
        Self {
            bundles: Vec::new(),
            keypair,
        }
    }

    /// Create WORM storage from an existing bundle chain
    ///
    /// # Arguments
    ///
    /// * `bundles` - Existing bundles to initialize with
    /// * `keypair` - Keypair for operations
    pub fn from_bundles(bundles: Vec<WormBundle>, keypair: KeyPair) -> Self {
        Self { bundles, keypair }
    }

    /// Seal and store a new bundle
    ///
    /// The new bundle is chained to the previous bundle (if any).
    ///
    /// # Arguments
    ///
    /// * `bundle` - Evidence bundle to seal
    /// * `retention` - Retention policy
    /// * `sealed_by` - Identity of the sealer
    ///
    /// # Returns
    ///
    /// The seal hash of the newly stored bundle
    pub fn store(
        &mut self,
        bundle: EvidenceBundle,
        retention: RetentionPolicy,
        sealed_by: &str,
    ) -> McpResult<String> {
        let prev_bundle = self.bundles.last();

        let worm_bundle =
            WormBundle::seal(bundle, sealed_by, retention, prev_bundle, &self.keypair)?;

        let seal_hash = worm_bundle.seal.seal_hash.clone();
        self.bundles.push(worm_bundle);

        Ok(seal_hash)
    }

    /// Get bundle by ID (read-only)
    pub fn get(&self, bundle_id: &str) -> Option<&WormBundle> {
        self.bundles
            .iter()
            .find(|b| b.bundle.bundle_id == bundle_id)
    }

    /// Get bundle by seal hash
    pub fn get_by_hash(&self, seal_hash: &str) -> Option<&WormBundle> {
        self.bundles
            .iter()
            .find(|b| b.seal.seal_hash == seal_hash)
    }

    /// List all bundles (read-only)
    pub fn list(&self) -> Vec<&WormBundle> {
        self.bundles.iter().collect()
    }

    /// Get the number of stored bundles
    pub fn len(&self) -> usize {
        self.bundles.len()
    }

    /// Check if storage is empty
    pub fn is_empty(&self) -> bool {
        self.bundles.is_empty()
    }

    /// Verify entire chain integrity
    ///
    /// Verifies:
    /// 1. Each bundle's individual integrity (hash + signature)
    /// 2. Chain links between consecutive bundles
    ///
    /// # Returns
    ///
    /// A `ChainVerification` report
    pub fn verify_chain(&self) -> McpResult<ChainVerification> {
        if self.bundles.is_empty() {
            return Ok(ChainVerification {
                valid: true,
                bundles_verified: 0,
                chain_length: 0,
                first_bundle: None,
                last_bundle: None,
                errors: vec![],
            });
        }

        let mut errors = Vec::new();
        let mut bundles_verified = 0;

        // Verify each bundle individually
        for (i, bundle) in self.bundles.iter().enumerate() {
            if let Err(e) = bundle.verify(&self.keypair) {
                errors.push(format!("Bundle {} ({}): {}", i, bundle.bundle_id(), e));
            } else {
                bundles_verified += 1;
            }
        }

        // Verify chain links
        for i in 1..self.bundles.len() {
            if let Err(e) = self.bundles[i].verify_chain(&self.bundles[i - 1]) {
                errors.push(format!("Chain link {} -> {}: {}", i - 1, i, e));
            }
        }

        // First bundle should have no previous hash (genesis)
        if !self.bundles.is_empty() && self.bundles[0].seal.prev_bundle_hash.is_some() {
            errors.push("First bundle has prev_bundle_hash but should be genesis".to_string());
        }

        let valid = errors.is_empty();
        let first_bundle = self.bundles.first().map(|b| b.seal.sealed_at);
        let last_bundle = self.bundles.last().map(|b| b.seal.sealed_at);

        Ok(ChainVerification {
            valid,
            bundles_verified,
            chain_length: self.bundles.len(),
            first_bundle,
            last_bundle,
            errors,
        })
    }

    /// Process expired bundles according to their expiry action
    ///
    /// This processes all bundles that have exceeded their retention period.
    /// Note: Bundles on legal hold are never processed.
    ///
    /// # Returns
    ///
    /// A list of results for each processed bundle
    pub fn process_expired(&mut self) -> Vec<ExpiryResult> {
        let mut results = Vec::new();

        // Collect expired bundle indices and their actions
        let expired: Vec<(usize, String, ExpiryAction)> = self
            .bundles
            .iter()
            .enumerate()
            .filter(|(_, b)| b.is_expired())
            .map(|(i, b)| (i, b.bundle_id().to_string(), b.retention.on_expiry))
            .collect();

        // Process in reverse order to maintain indices when removing
        for (idx, bundle_id, action) in expired.into_iter().rev() {
            let success = match action {
                ExpiryAction::Delete => {
                    // Remove from storage (only if not breaking chain)
                    // In a real system, this would archive to cold storage first
                    if idx == self.bundles.len() - 1 || idx == 0 {
                        // Only safe to delete from ends
                        self.bundles.remove(idx);
                        true
                    } else {
                        // Cannot delete middle of chain - mark for review instead
                        false
                    }
                }
                ExpiryAction::Archive => {
                    // In a real system, move to cold storage
                    // For now, just mark as processed
                    true
                }
                ExpiryAction::Review => {
                    // Flag for manual review - no automatic action
                    true
                }
            };

            results.push(ExpiryResult {
                bundle_id,
                action_taken: action,
                success,
            });
        }

        results
    }

    /// Export all bundles to a directory
    pub fn export_all(&self, dir: &Path) -> McpResult<()> {
        fs::create_dir_all(dir)?;

        for (i, bundle) in self.bundles.iter().enumerate() {
            let filename = format!("{:04}-{}.worm.json", i, bundle.bundle_id());
            let path = dir.join(filename);
            bundle.export(&path)?;
        }

        Ok(())
    }

    /// Import bundles from a directory
    pub fn import_all(dir: &Path, keypair: KeyPair) -> McpResult<Self> {
        let mut bundles = Vec::new();

        let mut entries: Vec<_> = fs::read_dir(dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "json")
                    .unwrap_or(false)
            })
            .collect();

        // Sort by filename to maintain order
        entries.sort_by_key(|e| e.file_name());

        for entry in entries {
            let bundle = WormBundle::import(&entry.path(), &keypair)?;
            bundles.push(bundle);
        }

        let storage = Self::from_bundles(bundles, keypair);

        // Verify chain integrity after import
        let verification = storage.verify_chain()?;
        if !verification.valid {
            return Err(McpError::ChainError(format!(
                "Imported chain has integrity errors: {:?}",
                verification.errors
            )));
        }

        Ok(storage)
    }
}

/// Result of chain verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChainVerification {
    /// Whether the entire chain is valid
    pub valid: bool,
    /// Number of bundles that passed individual verification
    pub bundles_verified: usize,
    /// Total chain length
    pub chain_length: usize,
    /// Timestamp of first bundle (if any)
    pub first_bundle: Option<DateTime<Utc>>,
    /// Timestamp of last bundle (if any)
    pub last_bundle: Option<DateTime<Utc>>,
    /// List of errors found during verification
    pub errors: Vec<String>,
}

/// Result of processing an expired bundle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpiryResult {
    /// Bundle ID that was processed
    pub bundle_id: String,
    /// Action that was taken
    pub action_taken: ExpiryAction,
    /// Whether the action succeeded
    pub success: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_bundle(family: &str) -> EvidenceBundle {
        EvidenceBundle::new_empty(family)
    }

    fn create_test_keypair() -> KeyPair {
        let seed = [42u8; 32];
        KeyPair::from_seed(&seed)
    }

    // ==========================================================================
    // WormBundle Tests
    // ==========================================================================

    #[test]
    fn test_seal_genesis_bundle() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test-family");
        let retention = RetentionPolicy::sox();

        // Act
        let worm = WormBundle::seal(bundle, "test-sealer", retention, None, &keypair).unwrap();

        // Assert
        assert!(worm.seal.prev_bundle_hash.is_none());
        assert_eq!(worm.seal.sealed_by, "test-sealer");
        assert!(!worm.seal.seal_hash.is_empty());
        assert!(!worm.seal.signature.is_empty());
    }

    #[test]
    fn test_seal_chained_bundle() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle1 = create_test_bundle("family-1");
        let bundle2 = create_test_bundle("family-2");
        let retention = RetentionPolicy::sox();

        // Act
        let worm1 =
            WormBundle::seal(bundle1, "sealer", retention.clone(), None, &keypair).unwrap();
        let worm2 =
            WormBundle::seal(bundle2, "sealer", retention, Some(&worm1), &keypair).unwrap();

        // Assert
        assert!(worm1.seal.prev_bundle_hash.is_none());
        assert_eq!(
            worm2.seal.prev_bundle_hash,
            Some(worm1.seal.seal_hash.clone())
        );
    }

    #[test]
    fn test_verify_valid_bundle() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        // Act
        let result = worm.verify(&keypair);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_verify_tampered_bundle_fails() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let mut worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        // Tamper with the seal hash
        worm.seal.seal_hash = "tampered_hash".to_string();

        // Act
        let result = worm.verify(&keypair);

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, McpError::ChainError(_)));
    }

    #[test]
    fn test_verify_chain_valid() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle1 = create_test_bundle("family-1");
        let bundle2 = create_test_bundle("family-2");
        let retention = RetentionPolicy::sox();

        let worm1 =
            WormBundle::seal(bundle1, "sealer", retention.clone(), None, &keypair).unwrap();
        let worm2 =
            WormBundle::seal(bundle2, "sealer", retention, Some(&worm1), &keypair).unwrap();

        // Act
        let result = worm2.verify_chain(&worm1);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_verify_chain_broken() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle1 = create_test_bundle("family-1");
        let bundle2 = create_test_bundle("family-2");
        let bundle3 = create_test_bundle("family-3");
        let retention = RetentionPolicy::sox();

        let worm1 =
            WormBundle::seal(bundle1, "sealer", retention.clone(), None, &keypair).unwrap();
        let _worm2 =
            WormBundle::seal(bundle2, "sealer", retention.clone(), Some(&worm1), &keypair).unwrap();
        let worm3 =
            WormBundle::seal(bundle3, "sealer", retention, Some(&worm1), &keypair).unwrap();

        // Act - worm3 should chain to worm2, not worm1
        // But we created worm3 chaining to worm1, so verify_chain with worm2 should fail
        let result = worm3.verify_chain(&_worm2);

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, McpError::ChainError(_)));
    }

    #[test]
    fn test_contents_immutable() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let original_id = bundle.bundle_id.clone();
        let worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        // Act
        let contents = worm.contents();

        // Assert - contents should match original
        assert_eq!(contents.bundle_id, original_id);
    }

    // ==========================================================================
    // RetentionPolicy Tests
    // ==========================================================================

    #[test]
    fn test_retention_policy_sox() {
        let policy = RetentionPolicy::sox();
        assert_eq!(policy.min_retention_days, 7 * 365);
        assert_eq!(policy.framework, "SOX");
        assert!(!policy.legal_hold);
    }

    #[test]
    fn test_retention_policy_gdpr() {
        let policy = RetentionPolicy::gdpr(365);
        assert_eq!(policy.min_retention_days, 365);
        assert_eq!(policy.framework, "GDPR");
        assert_eq!(policy.on_expiry, ExpiryAction::Delete);
    }

    #[test]
    fn test_retention_policy_hipaa() {
        let policy = RetentionPolicy::hipaa();
        assert_eq!(policy.min_retention_days, 6 * 365);
        assert_eq!(policy.framework, "HIPAA");
    }

    #[test]
    fn test_retention_policy_sec_17a4() {
        let policy = RetentionPolicy::sec_17a4();
        assert_eq!(policy.min_retention_days, 6 * 365);
        assert_eq!(policy.framework, "SEC-17a-4");
        assert_eq!(policy.on_expiry, ExpiryAction::Review);
    }

    #[test]
    fn test_retention_policy_with_legal_hold() {
        let policy = RetentionPolicy::sox().with_legal_hold();
        assert!(policy.legal_hold);
    }

    // ==========================================================================
    // Expiry Tests
    // ==========================================================================

    #[test]
    fn test_is_expired_fresh_bundle() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        // Act & Assert - Fresh bundle should not be expired
        assert!(!worm.is_expired());
    }

    #[test]
    fn test_is_expired_with_legal_hold() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let retention = RetentionPolicy {
            min_retention_days: -1, // Already expired
            legal_hold: true,
            framework: "TEST".to_string(),
            on_expiry: ExpiryAction::Delete,
        };
        let worm = WormBundle::seal(bundle, "sealer", retention, None, &keypair).unwrap();

        // Act & Assert - Legal hold prevents expiry
        assert!(!worm.is_expired());
    }

    #[test]
    fn test_can_delete_not_expired() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let retention = RetentionPolicy::gdpr(365);
        let worm = WormBundle::seal(bundle, "sealer", retention, None, &keypair).unwrap();

        // Act & Assert - Not expired, cannot delete
        assert!(!worm.can_delete());
    }

    #[test]
    fn test_can_delete_wrong_action() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let retention = RetentionPolicy {
            min_retention_days: -1, // Already expired
            legal_hold: false,
            framework: "TEST".to_string(),
            on_expiry: ExpiryAction::Archive, // Not delete
        };
        let worm = WormBundle::seal(bundle, "sealer", retention, None, &keypair).unwrap();

        // Act & Assert - Wrong expiry action, cannot delete
        assert!(!worm.can_delete());
    }

    #[test]
    fn test_expiry_date_calculation() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let retention = RetentionPolicy::sox();
        let worm = WormBundle::seal(bundle, "sealer", retention, None, &keypair).unwrap();

        // Act
        let expiry = worm.expiry_date();

        // Assert
        assert!(expiry.is_some());
        let expiry = expiry.unwrap();
        let expected = worm.seal.sealed_at + Duration::days(7 * 365);
        assert_eq!(expiry, expected);
    }

    #[test]
    fn test_expiry_date_with_legal_hold() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let retention = RetentionPolicy::sox().with_legal_hold();
        let worm = WormBundle::seal(bundle, "sealer", retention, None, &keypair).unwrap();

        // Act & Assert - No expiry date with legal hold
        assert!(worm.expiry_date().is_none());
    }

    // ==========================================================================
    // WormStorage Tests
    // ==========================================================================

    #[test]
    fn test_storage_store_first_bundle() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);
        let bundle = create_test_bundle("test");

        // Act
        let seal_hash = storage
            .store(bundle, RetentionPolicy::sox(), "test-sealer")
            .unwrap();

        // Assert
        assert_eq!(storage.len(), 1);
        assert!(!seal_hash.is_empty());
    }

    #[test]
    fn test_storage_store_multiple_bundles() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);

        // Act
        for i in 0..3 {
            let bundle = create_test_bundle(&format!("family-{}", i));
            storage
                .store(bundle, RetentionPolicy::sox(), "sealer")
                .unwrap();
        }

        // Assert
        assert_eq!(storage.len(), 3);
    }

    #[test]
    fn test_storage_get_by_id() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);
        let bundle = create_test_bundle("test");
        let bundle_id = bundle.bundle_id.clone();
        storage
            .store(bundle, RetentionPolicy::sox(), "sealer")
            .unwrap();

        // Act
        let retrieved = storage.get(&bundle_id);

        // Assert
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().bundle_id(), bundle_id);
    }

    #[test]
    fn test_storage_get_by_hash() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);
        let bundle = create_test_bundle("test");
        let seal_hash = storage
            .store(bundle, RetentionPolicy::sox(), "sealer")
            .unwrap();

        // Act
        let retrieved = storage.get_by_hash(&seal_hash);

        // Assert
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().seal_hash(), seal_hash);
    }

    #[test]
    fn test_storage_list() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);
        for i in 0..3 {
            let bundle = create_test_bundle(&format!("family-{}", i));
            storage
                .store(bundle, RetentionPolicy::sox(), "sealer")
                .unwrap();
        }

        // Act
        let list = storage.list();

        // Assert
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn test_storage_verify_chain_empty() {
        // Arrange
        let keypair = create_test_keypair();
        let storage = WormStorage::new(keypair);

        // Act
        let verification = storage.verify_chain().unwrap();

        // Assert
        assert!(verification.valid);
        assert_eq!(verification.chain_length, 0);
    }

    #[test]
    fn test_storage_verify_chain_valid() {
        // Arrange
        let keypair = create_test_keypair();
        let mut storage = WormStorage::new(keypair);
        for i in 0..5 {
            let bundle = create_test_bundle(&format!("family-{}", i));
            storage
                .store(bundle, RetentionPolicy::sox(), "sealer")
                .unwrap();
        }

        // Act
        let verification = storage.verify_chain().unwrap();

        // Assert
        assert!(verification.valid);
        assert_eq!(verification.bundles_verified, 5);
        assert_eq!(verification.chain_length, 5);
        assert!(verification.errors.is_empty());
    }

    #[test]
    fn test_storage_is_empty() {
        // Arrange
        let keypair = create_test_keypair();
        let storage = WormStorage::new(keypair);

        // Act & Assert
        assert!(storage.is_empty());
        assert_eq!(storage.len(), 0);
    }

    // ==========================================================================
    // Export/Import Tests
    // ==========================================================================

    #[test]
    fn test_bundle_export_import_roundtrip() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let original_id = bundle.bundle_id.clone();
        let worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join("test_worm_bundle.json");

        // Act
        worm.export(&path).unwrap();
        let imported = WormBundle::import(&path, &keypair).unwrap();

        // Assert
        assert_eq!(imported.bundle_id(), original_id);
        assert_eq!(imported.seal_hash(), worm.seal_hash());

        // Cleanup
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn test_import_tampered_bundle_fails() {
        // Arrange
        let keypair = create_test_keypair();
        let bundle = create_test_bundle("test");
        let worm =
            WormBundle::seal(bundle, "sealer", RetentionPolicy::sox(), None, &keypair).unwrap();

        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join("test_tampered_bundle.json");

        // Export and tamper
        worm.export(&path).unwrap();
        let mut json: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        json["seal"]["seal_hash"] = serde_json::json!("tampered");
        std::fs::write(&path, serde_json::to_string(&json).unwrap()).unwrap();

        // Act
        let result = WormBundle::import(&path, &keypair);

        // Assert
        assert!(result.is_err());

        // Cleanup
        let _ = std::fs::remove_file(&path);
    }

    // ==========================================================================
    // ChainVerification Tests
    // ==========================================================================

    #[test]
    fn test_chain_verification_serialization() {
        // Arrange
        let verification = ChainVerification {
            valid: true,
            bundles_verified: 5,
            chain_length: 5,
            first_bundle: Some(Utc::now()),
            last_bundle: Some(Utc::now()),
            errors: vec![],
        };

        // Act
        let json = serde_json::to_string(&verification).unwrap();
        let deserialized: ChainVerification = serde_json::from_str(&json).unwrap();

        // Assert
        assert_eq!(deserialized.valid, verification.valid);
        assert_eq!(deserialized.bundles_verified, verification.bundles_verified);
    }

    // ==========================================================================
    // ExpiryResult Tests
    // ==========================================================================

    #[test]
    fn test_expiry_result_serialization() {
        // Arrange
        let result = ExpiryResult {
            bundle_id: "test-bundle".to_string(),
            action_taken: ExpiryAction::Archive,
            success: true,
        };

        // Act
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: ExpiryResult = serde_json::from_str(&json).unwrap();

        // Assert
        assert_eq!(deserialized.bundle_id, result.bundle_id);
        assert_eq!(deserialized.action_taken, result.action_taken);
        assert_eq!(deserialized.success, result.success);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_seal_verify_roundtrip(
            family in "[a-z]{5,10}",
            sealer in "[a-z]{5,10}",
            seed in prop::array::uniform32(0u8..255)
        ) {
            let keypair = KeyPair::from_seed(&seed);
            let bundle = EvidenceBundle::new_empty(&family);
            let worm = WormBundle::seal(
                bundle,
                &sealer,
                RetentionPolicy::sox(),
                None,
                &keypair
            ).unwrap();

            prop_assert!(worm.verify(&keypair).is_ok());
        }

        #[test]
        fn prop_chain_links_correctly(
            families in prop::collection::vec("[a-z]{5,10}", 2..5),
            seed in prop::array::uniform32(0u8..255)
        ) {
            let keypair = KeyPair::from_seed(&seed);
            let mut storage = WormStorage::new(keypair);

            for family in families {
                let bundle = EvidenceBundle::new_empty(&family);
                storage.store(bundle, RetentionPolicy::sox(), "sealer").unwrap();
            }

            let verification = storage.verify_chain().unwrap();
            prop_assert!(verification.valid);
            prop_assert!(verification.errors.is_empty());
        }

        #[test]
        fn prop_retention_days_positive(days in 1i64..10000) {
            let policy = RetentionPolicy::gdpr(days);
            prop_assert_eq!(policy.min_retention_days, days);
            prop_assert!(policy.retention_duration().num_days() == days);
        }
    }
}
