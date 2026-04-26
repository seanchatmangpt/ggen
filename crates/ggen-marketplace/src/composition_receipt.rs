//! Composition receipts for governed pack operations.
//!
//! Extends ggen-receipt with pack-specific provenance tracking.
//! Implements CISO requirement for full chain of evidence.
//!
//! Supports receipt chaining for composite package tracking:
//! - Parent-child relationships (store parent ID in child)
//! - Chain traversal (walk up/down the receipt tree)
//! - Chain verification (validate all signatures)
//! - Anomaly detection (broken links, cycles, orphaned receipts)

use crate::error::Result;
use crate::trust::TrustTier;
use ggen_receipt::{Receipt, ReceiptChain};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Reference to an atomic pack in the composition.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AtomicPackRef {
    /// Pack identifier (e.g., "surface-mcp", "projection-rust")
    pub pack_id: String,

    /// Exact version used
    pub version: String,

    /// SHA-256 digest of pack contents
    pub digest: String,

    /// Ed25519 signature of the pack
    pub signature: String,

    /// Trust tier classification
    pub trust_tier: TrustTier,
}

/// Bundle expansion record (which bundles expanded to which atomic packs).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BundleExpansion {
    /// Bundle identifier (e.g., "mcp-rust")
    pub bundle_id: String,

    /// Atomic packs this bundle expanded to
    pub expanded_to: Vec<String>,
}

/// Graph fragment from loaded RDF ontology.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GraphFragment {
    /// Fragment identifier
    pub id: String,

    /// Number of triples in this fragment
    pub triple_count: usize,

    /// SHA-256 digest of the fragment
    pub digest: String,
}

/// SPARQL query executed during extraction.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SparqlQuery {
    /// Query name/identifier
    pub name: String,

    /// SPARQL query text
    pub query: String,

    /// Number of results returned
    pub result_count: usize,
}

/// Template reference used during emission.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateRef {
    /// Template identifier (pack_id:path)
    pub template_id: String,

    /// Template file path
    pub path: String,

    /// Number of times rendered
    pub render_count: usize,
}

/// Validator reference applied during validation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidatorRef {
    /// Validator name
    pub name: String,

    /// Validator version
    pub version: String,

    /// Pass/fail result
    pub passed: bool,
}

/// Policy reference enforced during composition.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PolicyRef {
    /// Policy identifier
    pub policy_id: String,

    /// Profile this policy belongs to
    pub profile: String,

    /// Policy rule that was enforced
    pub rule: String,
}

/// Conflict resolution record.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConflictResolution {
    /// Conflict type (ontology namespace, file path, etc.)
    pub conflict_type: String,

    /// Packs involved in the conflict
    pub packs: Vec<String>,

    /// Resolution applied (fail, prefer-first, merge, user-resolve)
    pub resolution: String,

    /// Explanation of the resolution
    pub explanation: String,
}

/// Output file hash reference.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputPath {
    /// File path
    pub path: String,

    /// SHA-256 digest of the file
    pub digest: String,

    /// Pack that generated this file (if any)
    pub generated_by: Option<String>,
}

/// Runtime profile used for composition.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeProfile {
    /// Profile identifier (e.g., "enterprise-strict", "regulated-finance")
    pub profile_id: String,

    /// Runtime constraints applied
    pub runtime_constraints: Vec<String>,

    /// Trust requirements
    pub trust_requirement: TrustTier,
}

/// Composition receipt for pack operations.
///
/// Records the full provenance of how a generated artifact came to exist.
/// Implements CISO requirement for defensible chain of evidence.
///
/// Supports chaining: each receipt can link to a parent receipt (for composite packages),
/// forming a tree of receipts. The parent_receipt_id is a SHA-256 hash (hex-encoded).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompositionReceipt {
    /// Unique receipt identifier (SHA-256 hash of the receipt)
    /// Optional during construction, computed and set during emission
    #[serde(skip_serializing_if = "Option::is_none")]
    pub receipt_id: Option<String>,

    /// Parent receipt ID (if this is a child receipt in a composition chain)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_receipt_id: Option<String>,

    /// Contributing atomic packs
    pub atomic_packs: Vec<AtomicPackRef>,

    /// Bundle aliases expanded
    pub bundle_aliases: Vec<BundleExpansion>,

    /// Exact versions and digests
    pub versions: HashMap<String, String>,

    /// Signatures and trust status
    pub signatures: Vec<SignatureRecord>,

    /// Ontology fragments loaded
    pub ontology_fragments: Vec<GraphFragment>,

    /// Queries executed
    pub queries_executed: Vec<SparqlQuery>,

    /// Templates rendered
    pub templates_rendered: Vec<TemplateRef>,

    /// Validators applied
    pub validators_applied: Vec<ValidatorRef>,

    /// Policies enforced
    pub policies_enforced: Vec<PolicyRef>,

    /// Conflicts encountered and resolutions
    pub conflicts: Vec<ConflictResolution>,

    /// Final ownership map
    pub ownership_map: HashMap<String, OwnershipRecord>,

    /// Emitted artifact hashes
    pub artifact_hashes: Vec<OutputPath>,

    /// Runtime/profile context
    pub runtime_context: RuntimeProfile,

    /// Underlying receipt chain for cryptographic verification
    pub receipt_chain: ReceiptChain,
}

/// Signature record for a pack.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignatureRecord {
    /// Pack identifier
    pub pack_id: String,

    /// Ed25519 public key (hex)
    pub public_key: String,

    /// Signature (hex)
    pub signature: String,

    /// Data checksum (SHA-256 hex)
    pub checksum: String,

    /// Signature verified
    pub verified: bool,
}

/// Ownership record for an artifact/field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipRecord {
    /// Target being owned (file path, namespace, field)
    pub target: String,

    /// Ownership class (exclusive, mergeable, overlay, forbidden-overlap)
    pub class: String,

    /// Pack that owns this target
    pub owner_pack: String,

    /// Merge strategy (if applicable)
    pub merge_strategy: Option<String>,
}

/// Compute SHA-256 hash of data and return as hex string.
fn sha2_digest(data: &str) -> String {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(data.as_bytes());
    hex::encode(hasher.finalize())
}

impl CompositionReceipt {
    /// Create a new composition receipt.
    #[must_use]
    pub fn new(runtime_context: RuntimeProfile) -> Self {
        Self {
            receipt_id: None,
            parent_receipt_id: None,
            atomic_packs: Vec::new(),
            bundle_aliases: Vec::new(),
            versions: HashMap::new(),
            signatures: Vec::new(),
            ontology_fragments: Vec::new(),
            queries_executed: Vec::new(),
            templates_rendered: Vec::new(),
            validators_applied: Vec::new(),
            policies_enforced: Vec::new(),
            conflicts: Vec::new(),
            ownership_map: HashMap::new(),
            artifact_hashes: Vec::new(),
            runtime_context,
            receipt_chain: ReceiptChain::new(),
        }
    }

    /// Add an atomic pack to the receipt.
    pub fn add_atomic_pack(&mut self, pack: AtomicPackRef) {
        self.versions
            .insert(pack.pack_id.clone(), pack.version.clone());
        self.atomic_packs.push(pack);
    }

    /// Add a bundle expansion to the receipt.
    pub fn add_bundle_expansion(&mut self, expansion: BundleExpansion) {
        self.bundle_aliases.push(expansion);
    }

    /// Add an ontology fragment to the receipt.
    pub fn add_ontology_fragment(&mut self, fragment: GraphFragment) {
        self.ontology_fragments.push(fragment);
    }

    /// Add a query execution record to the receipt.
    pub fn add_query(&mut self, query: SparqlQuery) {
        self.queries_executed.push(query);
    }

    /// Add a template rendering record to the receipt.
    pub fn add_template(&mut self, template: TemplateRef) {
        self.templates_rendered.push(template);
    }

    /// Add a validator record to the receipt.
    pub fn add_validator(&mut self, validator: ValidatorRef) {
        self.validators_applied.push(validator);
    }

    /// Add a policy enforcement record to the receipt.
    pub fn add_policy(&mut self, policy: PolicyRef) {
        self.policies_enforced.push(policy);
    }

    /// Add a conflict resolution to the receipt.
    pub fn add_conflict(&mut self, conflict: ConflictResolution) {
        self.conflicts.push(conflict);
    }

    /// Add an ownership record to the receipt.
    pub fn add_ownership(&mut self, target: String, record: OwnershipRecord) {
        self.ownership_map.insert(target, record);
    }

    /// Add an artifact hash to the receipt.
    pub fn add_artifact(&mut self, artifact: OutputPath) {
        self.artifact_hashes.push(artifact);
    }

    /// Append a receipt to the chain.
    ///
    /// # Errors
    ///
    /// Returns error if chain linkage is invalid.
    pub fn append_receipt(&mut self, receipt: Receipt) -> Result<()> {
        self.receipt_chain.append(receipt)?;
        Ok(())
    }

    /// Get the total number of atomic packs in the composition.
    #[must_use]
    pub fn pack_count(&self) -> usize {
        self.atomic_packs.len()
    }

    /// Get the total number of artifacts generated.
    #[must_use]
    pub fn artifact_count(&self) -> usize {
        self.artifact_hashes.len()
    }

    /// Check if all validators passed.
    #[must_use]
    pub fn all_validators_passed(&self) -> bool {
        self.validators_applied.iter().all(|v| v.passed)
    }

    /// Check if there were any conflicts.
    #[must_use]
    pub fn has_conflicts(&self) -> bool {
        !self.conflicts.is_empty()
    }

    /// Verify the receipt chain and validate composition integrity.
    ///
    /// Performs three levels of verification:
    /// 1. Cryptographic chain verification using ggen-receipt's ReceiptChain
    /// 2. Pack compatibility check (ensures all packs meet trust tier requirements)
    /// 3. Signature presence check (enterprise profiles require signatures)
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - The public key hex is invalid
    /// - Chain cryptographic verification fails
    /// - Any pack's trust tier does not meet the profile requirement
    /// - Required signatures are missing
    pub fn verify_chain(&self, public_key: &str) -> Result<()> {
        use crate::error::Error;
        use crate::trust::TrustTier;

        // 1. Cryptographic chain verification via ggen-receipt
        let key_bytes =
            hex::decode(public_key).map_err(|e| Error::SignatureVerificationFailed {
                reason: format!("Invalid public key hex: {}", e),
            })?;

        if key_bytes.len() != 32 {
            return Err(Error::SignatureVerificationFailed {
                reason: "Public key must be 32 bytes for Ed25519".to_string(),
            });
        }

        let mut key_array = [0u8; 32];
        key_array.copy_from_slice(&key_bytes);

        let verifying_key = ed25519_dalek::VerifyingKey::from_bytes(&key_array).map_err(|e| {
            Error::SignatureVerificationFailed {
                reason: format!("Invalid Ed25519 verifying key: {}", e),
            }
        })?;

        self.receipt_chain
            .verify(&verifying_key)
            .map_err(Error::from)?;

        // 2. Trust tier validation: every atomic pack must meet the profile requirement
        let required_tier = self.runtime_context.trust_requirement;
        for pack in &self.atomic_packs {
            if !pack.trust_tier.meets_requirement(required_tier) {
                return Err(Error::TrustTierCheckFailed {
                    reason: format!(
                        "Pack '{}' has trust tier {:?} but profile '{}' requires {:?}",
                        pack.pack_id,
                        pack.trust_tier,
                        self.runtime_context.profile_id,
                        required_tier
                    ),
                });
            }
        }

        // 3. Signature presence check for enterprise profiles
        if required_tier == TrustTier::EnterpriseCertified
            || required_tier == TrustTier::EnterpriseApproved
        {
            for pack in &self.atomic_packs {
                if pack.signature.is_empty() {
                    return Err(Error::SignatureVerificationFailed {
                        reason: format!(
                            "Enterprise profile '{}' requires signed packs, but '{}' has no signature",
                            self.runtime_context.profile_id, pack.pack_id
                        ),
                    });
                }
            }
        }

        tracing::debug!(
            "Composition receipt verified: {} packs, {} artifacts, profile '{}'",
            self.pack_count(),
            self.artifact_count(),
            self.runtime_context.profile_id
        );

        Ok(())
    }

    /// Chain this receipt to a parent receipt (link for composite package tracking).
    ///
    /// Sets the parent_receipt_id to the parent's receipt_id. This indicates that
    /// this composition is a child of another composition.
    ///
    /// # Arguments
    ///
    /// * `parent` - The parent composition receipt
    ///
    /// # Errors
    ///
    /// Returns error if the parent receipt doesn't have a receipt_id set.
    pub fn chain_parent(&mut self, parent: &CompositionReceipt) -> Result<()> {
        let parent_id = parent.receipt_id.as_ref().ok_or_else(|| {
            crate::error::Error::ValidationFailed {
                reason: "Parent receipt must have receipt_id set before chaining".to_string(),
            }
        })?;

        self.parent_receipt_id = Some(parent_id.clone());
        tracing::debug!(
            "Chained composition receipt to parent: {}",
            parent_id
        );

        Ok(())
    }

    /// Compute and set the receipt_id for this receipt.
    ///
    /// The receipt_id is a SHA-256 hash of the entire receipt JSON.
    /// Must be called before chaining this receipt as a parent.
    ///
    /// # Errors
    ///
    /// Returns error if the receipt cannot be serialized to JSON.
    pub fn compute_receipt_id(&mut self) -> Result<()> {
        // Temporarily clear the receipt_id for hashing
        let _ = self.receipt_id.take();

        let json = self.to_json()?;
        let hash = sha2_digest(&json);

        self.receipt_id = Some(hash.clone());
        tracing::debug!("Computed receipt_id: {}", hash);

        Ok(())
    }

    /// Get the parent receipt ID if this receipt is linked to a parent.
    #[must_use]
    pub fn get_parent_id(&self) -> Option<&str> {
        self.parent_receipt_id.as_deref()
    }

    /// Check if this receipt is a child (has a parent).
    #[must_use]
    pub fn is_child(&self) -> bool {
        self.parent_receipt_id.is_some()
    }

    /// Check if this receipt is a root (has no parent).
    #[must_use]
    pub fn is_root(&self) -> bool {
        self.parent_receipt_id.is_none()
    }

    /// Validate the chain starting from this receipt.
    ///
    /// Checks for:
    /// - Broken links (child with parent_id that cannot be resolved)
    /// - Cycles (receipt is its own ancestor)
    /// - Orphaned receipts (parent_id set but empty)
    ///
    /// Note: This is a local check only. To validate all chains in a repository,
    /// you need to provide a resolver function.
    ///
    /// # Arguments
    ///
    /// * `resolver` - Function to resolve parent receipt by ID
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - A cycle is detected in the chain
    /// - A parent receipt cannot be resolved
    /// - The chain exceeds reasonable depth (>100 levels)
    pub fn validate_chain<F>(&self, resolver: F) -> Result<()>
    where
        F: Fn(&str) -> Result<CompositionReceipt>,
    {
        // If this is a root receipt, no validation needed
        if self.is_root() {
            return Ok(());
        }

        let mut visited = HashSet::new();
        let mut current_id = self
            .parent_receipt_id
            .as_ref()
            .ok_or_else(|| {
                crate::error::Error::ValidationFailed {
                    reason: "Receipt marked as child but parent_receipt_id is None"
                        .to_string(),
                }
            })?
            .clone();

        const MAX_CHAIN_DEPTH: usize = 100;
        let mut depth = 0;

        loop {
            depth += 1;
            if depth > MAX_CHAIN_DEPTH {
                return Err(crate::error::Error::ValidationFailed {
                    reason: format!(
                        "Receipt chain exceeds maximum depth of {}",
                        MAX_CHAIN_DEPTH
                    ),
                });
            }

            // Check for cycles
            if visited.contains(&current_id) {
                return Err(crate::error::Error::ValidationFailed {
                    reason: format!("Cycle detected in receipt chain at: {}", current_id),
                });
            }

            visited.insert(current_id.clone());

            // Try to resolve the parent
            let parent = resolver(&current_id).map_err(|_| {
                crate::error::Error::ValidationFailed {
                    reason: format!("Cannot resolve parent receipt: {}", current_id),
                }
            })?;

            // If parent is a root, we've reached the end of the chain
            if parent.is_root() {
                break;
            }

            // Move to the next parent
            current_id = parent
                .parent_receipt_id
                .ok_or_else(|| {
                    crate::error::Error::ValidationFailed {
                        reason: "Parent receipt marked as child but parent_receipt_id is None"
                            .to_string(),
                    }
                })?;
        }

        tracing::debug!(
            "Receipt chain validated: {} levels, no cycles or broken links",
            depth
        );

        Ok(())
    }

    /// Get the full chain from this receipt to root (if chained).
    ///
    /// Returns a vector of receipt IDs from this receipt up to the root.
    /// The first element is this receipt's ID, the last is the root receipt ID.
    ///
    /// # Arguments
    ///
    /// * `resolver` - Function to resolve parent receipt by ID
    ///
    /// # Errors
    ///
    /// Returns error if a parent cannot be resolved or a cycle is detected.
    pub fn get_full_chain<F>(&self, resolver: F) -> Result<Vec<String>>
    where
        F: Fn(&str) -> Result<CompositionReceipt>,
    {
        let mut chain = vec![];

        // Add this receipt's ID if available
        if let Some(id) = &self.receipt_id {
            chain.push(id.clone());
        } else {
            return Err(crate::error::Error::ValidationFailed {
                reason: "Receipt must have receipt_id set to get chain".to_string(),
            });
        }

        // If this is a root, we're done
        if self.is_root() {
            return Ok(chain);
        }

        let mut current_id = self
            .parent_receipt_id
            .as_ref()
            .ok_or_else(|| {
                crate::error::Error::ValidationFailed {
                    reason: "Receipt marked as child but parent_receipt_id is None"
                        .to_string(),
                }
            })?
            .clone();

        const MAX_CHAIN_DEPTH: usize = 100;
        let mut depth = 0;

        loop {
            depth += 1;
            if depth > MAX_CHAIN_DEPTH {
                return Err(crate::error::Error::ValidationFailed {
                    reason: format!(
                        "Receipt chain exceeds maximum depth of {}",
                        MAX_CHAIN_DEPTH
                    ),
                });
            }

            // Check for cycles
            if chain.contains(&current_id) {
                return Err(crate::error::Error::ValidationFailed {
                    reason: format!("Cycle detected in receipt chain at: {}", current_id),
                });
            }

            chain.push(current_id.clone());

            // Resolve the parent
            let parent = resolver(&current_id).map_err(|_| {
                crate::error::Error::ValidationFailed {
                    reason: format!("Cannot resolve parent receipt: {}", current_id),
                }
            })?;

            // If parent is a root, we've reached the end
            if parent.is_root() {
                break;
            }

            // Move to next parent
            current_id = parent
                .parent_receipt_id
                .ok_or_else(|| {
                    crate::error::Error::ValidationFailed {
                        reason: "Parent receipt marked as child but parent_receipt_id is None"
                            .to_string(),
                    }
                })?;
        }

        Ok(chain)
    }

    /// Serialize the composition receipt to JSON.
    ///
    /// # Errors
    ///
    /// Returns error if serialization fails.
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string_pretty(self).map_err(Into::into)
    }

    /// Deserialize a composition receipt from JSON.
    ///
    /// # Errors
    ///
    /// Returns error if deserialization fails.
    pub fn from_json(json: &str) -> Result<Self> {
        serde_json::from_str(json).map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_composition_receipt_creation() {
        let profile = RuntimeProfile {
            profile_id: "enterprise-strict".to_string(),
            runtime_constraints: vec!["require-explicit-runtime".to_string()],
            trust_requirement: TrustTier::EnterpriseCertified,
        };

        let receipt = CompositionReceipt::new(profile);
        assert_eq!(receipt.pack_count(), 0);
        assert_eq!(receipt.artifact_count(), 0);
        assert!(receipt.all_validators_passed());
        assert!(!receipt.has_conflicts());
        assert!(receipt.receipt_id.is_none());
        assert!(receipt.parent_receipt_id.is_none());
    }

    #[test]
    fn test_add_atomic_pack() {
        let mut receipt = CompositionReceipt::new(RuntimeProfile {
            profile_id: "default".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        let pack = AtomicPackRef {
            pack_id: "surface-mcp".to_string(),
            version: "1.0.0".to_string(),
            digest: "abc123".to_string(),
            signature: "def456".to_string(),
            trust_tier: TrustTier::EnterpriseCertified,
        };

        receipt.add_atomic_pack(pack);
        assert_eq!(receipt.pack_count(), 1);
        assert_eq!(
            receipt.versions.get("surface-mcp"),
            Some(&"1.0.0".to_string())
        );
    }

    #[test]
    fn test_json_serialization() {
        let receipt = CompositionReceipt::new(RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        let json = receipt.to_json();
        assert!(json.is_ok());

        let deserialized = CompositionReceipt::from_json(&json.unwrap());
        assert!(deserialized.is_ok());
        assert_eq!(deserialized.unwrap().runtime_context.profile_id, "test");
    }

    #[test]
    fn test_compute_receipt_id() {
        let mut receipt = CompositionReceipt::new(RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        assert!(receipt.receipt_id.is_none());

        let result = receipt.compute_receipt_id();
        assert!(result.is_ok());
        assert!(receipt.receipt_id.is_some());

        // Receipt ID should be 64 hex characters (SHA-256)
        let id = receipt.receipt_id.unwrap();
        assert_eq!(id.len(), 64);

        // Computing again should produce the same ID
        let mut receipt2 = CompositionReceipt::new(RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        receipt2.compute_receipt_id().unwrap();
        assert_eq!(receipt.receipt_id, receipt2.receipt_id);
    }

    #[test]
    fn test_receipt_is_root_and_child() {
        let mut receipt = CompositionReceipt::new(RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        // Initially should be root
        assert!(receipt.is_root());
        assert!(!receipt.is_child());
        assert!(receipt.get_parent_id().is_none());

        // Set parent
        receipt.parent_receipt_id = Some("parent_hash".to_string());

        assert!(!receipt.is_root());
        assert!(receipt.is_child());
        assert_eq!(receipt.get_parent_id(), Some("parent_hash"));
    }

    #[test]
    fn test_chain_parent() {
        let mut root = CompositionReceipt::new(RuntimeProfile {
            profile_id: "root".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        root.compute_receipt_id().unwrap();

        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        // Child should chain to root
        let result = child.chain_parent(&root);
        assert!(result.is_ok());
        assert!(child.is_child());
        assert_eq!(
            child.get_parent_id(),
            root.receipt_id.as_deref()
        );
    }

    #[test]
    fn test_chain_parent_without_parent_id_fails() {
        let parent_without_id = CompositionReceipt::new(RuntimeProfile {
            profile_id: "parent".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        // Should fail because parent has no receipt_id
        let result = child.chain_parent(&parent_without_id);
        assert!(result.is_err());
    }

    #[test]
    fn test_multi_level_chain() {
        // Create a 3-level chain: root -> child -> grandchild
        let mut root = CompositionReceipt::new(RuntimeProfile {
            profile_id: "root".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        root.compute_receipt_id().unwrap();

        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        child.chain_parent(&root).unwrap();
        child.compute_receipt_id().unwrap();

        let mut grandchild = CompositionReceipt::new(RuntimeProfile {
            profile_id: "grandchild".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        grandchild.chain_parent(&child).unwrap();
        grandchild.compute_receipt_id().unwrap();

        // Verify relationships
        assert!(root.is_root());
        assert!(child.is_child());
        assert!(grandchild.is_child());

        assert_eq!(
            child.get_parent_id(),
            root.receipt_id.as_deref()
        );
        assert_eq!(
            grandchild.get_parent_id(),
            child.receipt_id.as_deref()
        );
    }

    #[test]
    fn test_validate_chain_on_root() {
        let root = CompositionReceipt::new(RuntimeProfile {
            profile_id: "root".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });

        // Root receipt should always validate
        let result = root.validate_chain(|_| {
            Err(crate::error::Error::ValidationFailed {
                reason: "Should not be called for root".to_string(),
            })
        });
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_chain_detects_broken_links() {
        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        child.parent_receipt_id = Some("nonexistent_parent".to_string());

        // Validation should fail because resolver cannot find parent
        let result = child.validate_chain(|_| {
            Err(crate::error::Error::ValidationFailed {
                reason: "Cannot resolve".to_string(),
            })
        });
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Cannot resolve"));
    }

    #[test]
    fn test_validate_chain_detects_cycles() {
        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        child.receipt_id = Some("parent_id".to_string());
        child.parent_receipt_id = Some("parent_id".to_string()); // Points to itself

        let result = child.validate_chain(|id| {
            // Return a receipt that points back, creating a cycle
            let mut parent = CompositionReceipt::new(RuntimeProfile {
                profile_id: "parent".to_string(),
                runtime_constraints: vec![],
                trust_requirement: TrustTier::Experimental,
            });
            parent.receipt_id = Some(id.to_string());
            parent.parent_receipt_id = Some("parent_id".to_string()); // Back to child
            Ok(parent)
        });

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Cycle"));
    }

    #[test]
    fn test_get_full_chain_without_receipt_id_fails() {
        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        child.parent_receipt_id = Some("parent_id".to_string());

        let result = child.get_full_chain(|_| {
            Err(crate::error::Error::ValidationFailed {
                reason: "Not called".to_string(),
            })
        });

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("receipt_id"));
    }

    #[test]
    fn test_get_full_chain_for_root() {
        let mut root = CompositionReceipt::new(RuntimeProfile {
            profile_id: "root".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        root.compute_receipt_id().unwrap();

        let result = root.get_full_chain(|_| {
            Err(crate::error::Error::ValidationFailed {
                reason: "Should not be called".to_string(),
            })
        });

        assert!(result.is_ok());
        let chain = result.unwrap();
        assert_eq!(chain.len(), 1);
        assert_eq!(chain[0], root.receipt_id.as_ref().unwrap());
    }

    #[test]
    fn test_get_full_chain_multi_level() {
        let mut root = CompositionReceipt::new(RuntimeProfile {
            profile_id: "root".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        root.compute_receipt_id().unwrap();
        let root_id = root.receipt_id.clone().unwrap();

        let mut child = CompositionReceipt::new(RuntimeProfile {
            profile_id: "child".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        child.chain_parent(&root).unwrap();
        child.compute_receipt_id().unwrap();
        let child_id = child.receipt_id.clone().unwrap();

        let mut grandchild = CompositionReceipt::new(RuntimeProfile {
            profile_id: "grandchild".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        grandchild.chain_parent(&child).unwrap();
        grandchild.compute_receipt_id().unwrap();

        // Create a resolver that can resolve both root and child
        let resolver = |id: &str| -> Result<CompositionReceipt> {
            if id == &child_id {
                Ok(child.clone())
            } else if id == &root_id {
                Ok(root.clone())
            } else {
                Err(crate::error::Error::ValidationFailed {
                    reason: format!("Unknown ID: {}", id),
                })
            }
        };

        let result = grandchild.get_full_chain(resolver);
        assert!(result.is_ok());

        let chain = result.unwrap();
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0], grandchild.receipt_id.as_ref().unwrap());
        assert_eq!(chain[1], child_id);
        assert_eq!(chain[2], root_id);
    }

    #[test]
    fn test_chain_exceeds_max_depth() {
        let mut deep = CompositionReceipt::new(RuntimeProfile {
            profile_id: "deep".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        });
        deep.receipt_id = Some("deep_id".to_string());
        deep.parent_receipt_id = Some("parent_id".to_string());

        // Create a resolver that always returns a parent with another parent
        let resolver = |_id: &str| -> Result<CompositionReceipt> {
            let mut parent = CompositionReceipt::new(RuntimeProfile {
                profile_id: "parent".to_string(),
                runtime_constraints: vec![],
                trust_requirement: TrustTier::Experimental,
            });
            parent.parent_receipt_id = Some("another_parent".to_string());
            Ok(parent)
        };

        let result = deep.get_full_chain(resolver);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("exceeds maximum depth"));
    }
}
