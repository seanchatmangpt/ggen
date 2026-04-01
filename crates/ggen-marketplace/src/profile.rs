//! Profile-based policy overlays for enterprise governance.
//!
//! Implements Fortune 5 CISO requirements for runtime profiles with
//! policy overlays and trust requirements.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::policy::{Policy, PolicyEnforcer, PolicyId, PolicyReport, PolicyRule, PackContext};
use crate::trust::TrustTier;

/// Unique identifier for a profile.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProfileId(String);

impl ProfileId {
    /// Create a new profile ID.
    #[must_use]
    pub fn new(id: &str) -> Self {
        Self(id.to_string())
    }

    /// Get the inner string value.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Receipt specification for profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReceiptSpec {
    /// No receipt required.
    None,

    /// Unsigned receipt (digest only).
    DigestOnly,

    /// Signed receipt (Ed25519).
    Signed,

    /// Signed and chained receipt (with hash links).
    SignedAndChained,
}

/// Runtime constraint for profile.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct RuntimeConstraint {
    /// Allowed runtime types (empty = all allowed).
    pub allowed_runtimes: Vec<String>,

    /// Forbid default runtime values.
    pub forbid_defaults: bool,

    /// Require explicit runtime declaration.
    pub require_explicit: bool,

    /// Additional metadata.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, String>,
}

impl RuntimeConstraint {
    /// Create a new runtime constraint.
    #[must_use]
    pub fn new() -> Self {
        Self {
            allowed_runtimes: Vec::new(),
            forbid_defaults: false,
            require_explicit: false,
            metadata: HashMap::new(),
        }
    }

    /// Check if a runtime is allowed.
    #[must_use]
    pub fn allows_runtime(&self, runtime: &str) -> bool {
        self.allowed_runtimes.is_empty() || self.allowed_runtimes.contains(&runtime.to_string())
    }
}

impl Default for RuntimeConstraint {
    fn default() -> Self {
        Self::new()
    }
}

/// Enterprise profile for policy enforcement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Profile {
    /// Unique profile identifier.
    pub id: ProfileId,

    /// Human-readable profile name.
    pub name: String,

    /// Profile description.
    pub description: String,

    /// Policy overlays to enforce.
    pub policy_overlays: Vec<Policy>,

    /// Runtime constraints.
    pub runtime_constraints: Vec<RuntimeConstraint>,

    /// Minimum trust tier required.
    pub trust_requirements: TrustTier,

    /// Receipt requirements.
    pub receipt_requirements: ReceiptSpec,

    /// Additional metadata.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, String>,
}

impl Profile {
    /// Create a new profile.
    #[must_use]
    pub fn new(
        id: ProfileId,
        name: String,
        description: String,
        policy_overlays: Vec<Policy>,
        trust_requirements: TrustTier,
        receipt_requirements: ReceiptSpec,
    ) -> Self {
        Self {
            id,
            name,
            description,
            policy_overlays,
            runtime_constraints: Vec::new(),
            trust_requirements,
            receipt_requirements,
            metadata: HashMap::new(),
        }
    }

    /// Add runtime constraint to profile.
    #[must_use]
    pub fn with_runtime_constraint(mut self, constraint: RuntimeConstraint) -> Self {
        self.runtime_constraints.push(constraint);
        self
    }

    /// Add metadata to profile.
    #[must_use]
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Enforce policies against packs.
    ///
    /// # Errors
    ///
    /// Returns error if enforcement fails.
    pub fn enforce(&self, packs: &[PackContext]) -> Result<PolicyReport> {
        PolicyEnforcer::enforce(&self.policy_overlays, packs)
    }

    /// Check if a runtime is allowed by this profile.
    #[must_use]
    pub fn allows_runtime(&self, runtime: &str) -> bool {
        self.runtime_constraints
            .iter()
            .all(|c| c.allows_runtime(runtime))
    }

    /// Check if a trust tier meets requirements.
    #[must_use]
    pub fn meets_trust_requirement(&self, tier: TrustTier) -> bool {
        tier.meets_requirement(self.trust_requirements)
    }
}

/// Enterprise strict profile (no defaults, all explicit).
#[must_use]
pub fn enterprise_strict_profile() -> Profile {
    Profile::new(
        ProfileId::new("enterprise-strict"),
        "Enterprise Strict".to_string(),
        "Enterprise profile with no defaults, all values explicit".to_string(),
        vec![
            Policy::forbid_template_defaults(),
            Policy::forbid_inferred_capabilities(),
            Policy::require_signed_receipts(),
            Policy::require_explicit_runtime(),
        ],
        TrustTier::EnterpriseApproved,
        ReceiptSpec::Signed,
    )
    .with_runtime_constraint(RuntimeConstraint {
        allowed_runtimes: vec!["axum".to_string(), "stdio".to_string()],
        forbid_defaults: true,
        require_explicit: true,
        metadata: HashMap::new(),
    })
    .with_metadata("category".to_string(), "enterprise".to_string())
    .with_metadata("strictness".to_string(), "high".to_string())
}

/// Regulated finance profile (highest security).
#[must_use]
pub fn regulated_finance_profile() -> Profile {
    Profile::new(
        ProfileId::new("regulated-finance"),
        "Regulated Finance".to_string(),
        "Regulated finance profile with maximum security requirements".to_string(),
        vec![
            Policy::forbid_public_registry_in_regulated(),
            Policy::require_trust_tier(TrustTier::EnterpriseCertified),
            Policy::require_approved_runtime(vec!["axum".to_string()]),
            Policy::require_signed_receipts(),
            Policy::forbid_unlisted_packages(),
            Policy::require_signature_verification(),
            Policy::require_composition_receipts(),
            Policy::require_provenance_tracking(),
        ],
        TrustTier::EnterpriseCertified,
        ReceiptSpec::SignedAndChained,
    )
    .with_runtime_constraint(RuntimeConstraint {
        allowed_runtimes: vec!["axum".to_string()],
        forbid_defaults: true,
        require_explicit: true,
        metadata: HashMap::new(),
    })
    .with_metadata("category".to_string(), "regulated".to_string())
    .with_metadata("industry".to_string(), "finance".to_string())
    .with_metadata("compliance".to_string(), "SOC2,FedRAMP,HIPAA".to_string())
}

/// Development profile (relaxed requirements).
#[must_use]
pub fn development_profile() -> Profile {
    Profile::new(
        ProfileId::new("development"),
        "Development".to_string(),
        "Development profile with relaxed requirements".to_string(),
        vec![
            // No strict policies for development
        ],
        TrustTier::Experimental,
        ReceiptSpec::DigestOnly,
    )
    .with_runtime_constraint(RuntimeConstraint {
        allowed_runtimes: Vec::new(), // All runtimes allowed
        forbid_defaults: false,
        require_explicit: false,
        metadata: HashMap::new(),
    })
    .with_metadata("category".to_string(), "development".to_string())
    .with_metadata("strictness".to_string(), "low".to_string())
}

/// Get all predefined profiles.
#[must_use]
pub fn predefined_profiles() -> Vec<Profile> {
    vec![
        enterprise_strict_profile(),
        regulated_finance_profile(),
        development_profile(),
    ]
}

/// Get profile by ID.
///
/// # Errors
    ///
/// Returns error if profile not found.
pub fn get_profile(id: &str) -> Result<Profile> {
    predefined_profiles()
        .into_iter()
        .find(|p| p.id.as_str() == id)
        .ok_or_else(|| Error::PackageNotFound {
            package_id: id.to_string(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile_id() {
        let id = ProfileId::new("test-profile");
        assert_eq!(id.as_str(), "test-profile");
    }

    #[test]
    fn test_enterprise_strict_profile() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.id.as_str(), "enterprise-strict");
        assert_eq!(profile.policy_overlays.len(), 4);
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseApproved);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::Signed);
    }

    #[test]
    fn test_regulated_finance_profile() {
        let profile = regulated_finance_profile();
        assert_eq!(profile.id.as_str(), "regulated-finance");
        assert_eq!(profile.policy_overlays.len(), 8);
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseCertified);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::SignedAndChained);
    }

    #[test]
    fn test_development_profile() {
        let profile = development_profile();
        assert_eq!(profile.id.as_str(), "development");
        assert_eq!(profile.policy_overlays.len(), 0);
        assert_eq!(profile.trust_requirements, TrustTier::Experimental);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::DigestOnly);
    }

    #[test]
    fn test_profile_enforce() {
        let profile = enterprise_strict_profile();

        // Pack that violates policies
        let pack_bad = PackContext::new("test-pack".to_string())
            .with_template_defaults(true)
            .with_signed_receipts(false)
            .with_runtime(None);

        let report = profile.enforce(&[pack_bad]).unwrap();
        assert!(!report.passed);
        assert!(report.violation_count() > 0);

        // Pack that complies with policies
        let pack_good = PackContext::new("good-pack".to_string())
            .with_template_defaults(false)
            .with_signed_receipts(true)
            .with_runtime(Some("axum".to_string()));

        let report = profile.enforce(&[pack_good]).unwrap();
        assert!(report.passed);
    }

    #[test]
    fn test_profile_allows_runtime() {
        let profile = enterprise_strict_profile();

        assert!(profile.allows_runtime("axum"));
        assert!(profile.allows_runtime("stdio"));
        assert!(!profile.allows_runtime("unknown"));
    }

    #[test]
    fn test_profile_meets_trust_requirement() {
        let profile = regulated_finance_profile();

        assert!(profile.meets_trust_requirement(TrustTier::EnterpriseCertified));
        assert!(!profile.meets_trust_requirement(TrustTier::Experimental));
    }

    #[test]
    fn test_runtime_constraint() {
        let constraint = RuntimeConstraint {
            allowed_runtimes: vec!["axum".to_string()],
            forbid_defaults: true,
            require_explicit: true,
            metadata: HashMap::new(),
        };

        assert!(constraint.allows_runtime("axum"));
        assert!(!constraint.allows_runtime("stdio"));
    }

    #[test]
    fn test_get_profile() {
        let profile = get_profile("enterprise-strict").unwrap();
        assert_eq!(profile.id.as_str(), "enterprise-strict");

        let err = get_profile("unknown-profile");
        assert!(err.is_err());
    }

    #[test]
    fn test_predefined_profiles() {
        let profiles = predefined_profiles();
        assert_eq!(profiles.len(), 3);

        let ids: Vec<_> = profiles.iter().map(|p| p.id.as_str()).collect();
        assert!(ids.contains(&"enterprise-strict"));
        assert!(ids.contains(&"regulated-finance"));
        assert!(ids.contains(&"development"));
    }

    #[test]
    fn test_profile_with_metadata() {
        let profile = Profile::new(
            ProfileId::new("custom"),
            "Custom".to_string(),
            "Custom profile".to_string(),
            vec![],
            TrustTier::EnterpriseApproved,
            ReceiptSpec::Signed,
        )
        .with_metadata("key".to_string(), "value".to_string());

        assert_eq!(profile.metadata.get("key"), Some(&"value".to_string()));
    }
}
