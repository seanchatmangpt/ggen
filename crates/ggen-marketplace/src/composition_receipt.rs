//! Composition receipts for governed pack operations.
//!
//! Extends ggen-receipt with pack-specific provenance tracking.
//! Implements CISO requirement for full chain of evidence.

use crate::trust::TrustTier;
use crate::error::Result;
use ggen_receipt::{Receipt, ReceiptChain};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompositionReceipt {
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

impl CompositionReceipt {
    /// Create a new composition receipt.
    #[must_use]
    pub fn new(runtime_context: RuntimeProfile) -> Self {
        Self {
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
        self.versions.insert(pack.pack_id.clone(), pack.version.clone());
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
        let key_bytes = hex::decode(public_key).map_err(|e| {
            Error::SignatureVerificationFailed {
                reason: format!("Invalid public key hex: {}", e),
            }
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
                        pack.pack_id, pack.trust_tier,
                        self.runtime_context.profile_id, required_tier
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
        assert_eq!(receipt.versions.get("surface-mcp"), Some(&"1.0.0".to_string()));
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
}
