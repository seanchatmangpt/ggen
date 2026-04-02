//! Profile-based policy overlays for enterprise governance.
//!
//! Implements Fortune 5 CISO requirements for runtime profiles with
//! policy overlays and trust requirements.
//!
//! Supports both built-in profiles and user-configurable profiles loaded
//! from `.ggen/profiles.toml`.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::{Error, Result};
use crate::policy::{PackContext, Policy, PolicyEnforcer, PolicyReport, PolicyRule};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum ReceiptSpec {
    /// No receipt required.
    #[default]
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
        id: ProfileId, name: String, description: String, policy_overlays: Vec<Policy>,
        trust_requirements: TrustTier, receipt_requirements: ReceiptSpec,
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
    /// # Errors
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

    /// Check if this profile forbids public registry packs.
    #[must_use]
    pub fn forbid_public_registry(&self) -> bool {
        let forbid_rule = PolicyRule::ForbidPublicRegistryInRegulated;
        self.policy_overlays
            .iter()
            .any(|policy| policy.contains_rule(&forbid_rule))
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
        vec![],
        TrustTier::Experimental,
        ReceiptSpec::DigestOnly,
    )
    .with_runtime_constraint(RuntimeConstraint {
        allowed_runtimes: Vec::new(),
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

/// Get profile by ID (built-in only).
/// # Errors
/// Returns error if profile not found.
pub fn get_profile(id: &str) -> Result<Profile> {
    predefined_profiles()
        .into_iter()
        .find(|p| p.id.as_str() == id)
        .ok_or_else(|| Error::PackageNotFound {
            package_id: id.to_string(),
        })
}

// ---------------------------------------------------------------------------
// User-configurable profiles (loaded from .ggen/profiles.toml)
// ---------------------------------------------------------------------------

/// Runtime constraint entry for TOML deserialization.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CustomRuntimeConstraint {
    #[serde(default)]
    pub allowed_runtimes: Vec<String>,
    #[serde(default)]
    pub forbid_defaults: bool,
    #[serde(default)]
    pub require_explicit: bool,
}

impl From<CustomRuntimeConstraint> for RuntimeConstraint {
    fn from(c: CustomRuntimeConstraint) -> Self {
        Self {
            allowed_runtimes: c.allowed_runtimes,
            forbid_defaults: c.forbid_defaults,
            require_explicit: c.require_explicit,
            metadata: HashMap::new(),
        }
    }
}

/// Policy rule identifier that maps to a known [`PolicyRule`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CustomPolicyRef {
    ForbidTemplateDefaults,
    ForbidInferredCapabilities,
    RequireSignedReceipts,
    RequireExplicitRuntime,
    ForbidPublicRegistryInRegulated,
    ForbidUnlistedPackages,
    RequireSignatureVerification,
    RequireSemanticVersioning,
    ForbidVersionDowngrade,
    RequireCompositionReceipts,
    RequireProvenanceTracking,
}

impl From<CustomPolicyRef> for PolicyRule {
    fn from(r: CustomPolicyRef) -> Self {
        match r {
            CustomPolicyRef::ForbidTemplateDefaults => PolicyRule::ForbidTemplateDefaults,
            CustomPolicyRef::ForbidInferredCapabilities => PolicyRule::ForbidInferredCapabilities,
            CustomPolicyRef::RequireSignedReceipts => PolicyRule::RequireSignedReceipts,
            CustomPolicyRef::RequireExplicitRuntime => PolicyRule::RequireExplicitRuntime,
            CustomPolicyRef::ForbidPublicRegistryInRegulated => {
                PolicyRule::ForbidPublicRegistryInRegulated
            }
            CustomPolicyRef::ForbidUnlistedPackages => PolicyRule::ForbidUnlistedPackages,
            CustomPolicyRef::RequireSignatureVerification => {
                PolicyRule::RequireSignatureVerification
            }
            CustomPolicyRef::RequireSemanticVersioning => PolicyRule::RequireSemanticVersioning,
            CustomPolicyRef::ForbidVersionDowngrade => PolicyRule::ForbidVersionDowngrade,
            CustomPolicyRef::RequireCompositionReceipts => PolicyRule::RequireCompositionReceipts,
            CustomPolicyRef::RequireProvenanceTracking => PolicyRule::RequireProvenanceTracking,
        }
    }
}

/// A single custom profile entry as it appears in `profiles.toml`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CustomProfileEntry {
    pub name: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub trust_tier: TrustTier,
    #[serde(default)]
    pub receipt_spec: ReceiptSpec,
    #[serde(default)]
    pub policies: Vec<CustomPolicyRef>,
    #[serde(default)]
    pub runtime_constraints: Vec<CustomRuntimeConstraint>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, String>,
}

impl CustomProfileEntry {
    /// Convert this TOML-friendly entry into a full [`Profile`].
    #[must_use]
    pub fn into_profile(self, section_key: &str) -> Profile {
        let policy_overlays: Vec<Policy> = self
            .policies
            .into_iter()
            .map(|r| {
                let rule: PolicyRule = r.into();
                Policy::new(
                    crate::policy::PolicyId::new(&format!("{rule:?}")),
                    rule.description(),
                    rule.description(),
                    vec![rule],
                )
            })
            .collect();

        let runtime_constraints: Vec<RuntimeConstraint> = self
            .runtime_constraints
            .into_iter()
            .map(Into::into)
            .collect();

        let mut profile = Profile::new(
            ProfileId::new(section_key),
            self.name,
            self.description,
            policy_overlays,
            self.trust_tier,
            self.receipt_spec,
        );

        profile.runtime_constraints = runtime_constraints;
        profile.metadata = self.metadata;
        profile
    }
}

/// Root structure for `.ggen/profiles.toml`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProfileConfig {
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub profiles: HashMap<String, CustomProfileEntry>,
}

/// Loads user-configurable profiles from `.ggen/profiles.toml`.
pub struct ProfileLoader;

impl ProfileLoader {
    /// Default path to the profiles configuration file.
    #[must_use]
    pub fn default_path(project_root: &Path) -> PathBuf {
        project_root.join(".ggen").join("profiles.toml")
    }

    /// Load profiles from the default path (`.ggen/profiles.toml`).
    /// Returns only the built-in profiles when the file does not exist.
    /// # Errors
    /// Returns an error if the file exists but cannot be read or parsed.
    pub fn load(project_root: &Path) -> Result<Vec<Profile>> {
        Self::load_from(&Self::default_path(project_root))
    }

    /// Load profiles from an explicit path.
    /// Returns only the built-in profiles when the file does not exist.
    /// # Errors
    /// Returns an error if the file exists but cannot be read or parsed.
    pub fn load_from(path: &Path) -> Result<Vec<Profile>> {
        let mut all_profiles = predefined_profiles();

        if !path.exists() {
            return Ok(all_profiles);
        }

        let content = std::fs::read_to_string(path)?;
        let config: ProfileConfig = toml::from_str(&content)?;

        for (section_key, entry) in config.profiles {
            if all_profiles.iter().any(|p| p.id.as_str() == section_key) {
                continue;
            }
            all_profiles.push(entry.into_profile(&section_key));
        }

        Ok(all_profiles)
    }

    /// Load a single profile by ID, searching custom then built-in.
    /// # Errors
    /// Returns an error if the profile is not found or config cannot be parsed.
    pub fn get_profile(project_root: &Path, id: &str) -> Result<Profile> {
        let profiles = Self::load(project_root)?;
        profiles
            .into_iter()
            .find(|p| p.id.as_str() == id)
            .ok_or_else(|| Error::PackageNotFound {
                package_id: id.to_string(),
            })
    }

    /// Parse a `ProfileConfig` from a string (useful for testing).
    /// # Errors
    /// Returns an error if the TOML cannot be parsed.
    pub fn parse_str(content: &str) -> Result<ProfileConfig> {
        let config: ProfileConfig = toml::from_str(content)?;
        Ok(config)
    }
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
        let pack_bad = PackContext::new("test-pack".to_string())
            .with_template_defaults(true)
            .with_signed_receipts(false)
            .with_runtime(None);
        let report = profile.enforce(&[pack_bad]).unwrap();
        assert!(!report.passed);
        assert!(report.violation_count() > 0);

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
        assert!(get_profile("unknown-profile").is_err());
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

    #[test]
    fn test_parse_custom_profile_config() {
        let toml = r#"
[profiles.my-team]
name = "My Team"
description = "Team-specific governance"
trust_tier = "enterprise_approved"
receipt_spec = "signed"
policies = ["require_signed_receipts", "forbid_template_defaults"]
"#;
        let config = ProfileLoader::parse_str(toml).unwrap();
        assert_eq!(config.profiles.len(), 1);
        let entry = config.profiles.get("my-team").unwrap();
        assert_eq!(entry.name, "My Team");
        assert_eq!(entry.trust_tier, TrustTier::EnterpriseApproved);
        assert_eq!(entry.receipt_spec, ReceiptSpec::Signed);
        assert_eq!(entry.policies.len(), 2);
    }

    #[test]
    fn test_custom_profile_entry_into_profile() {
        let entry = CustomProfileEntry {
            name: "Sandbox".to_string(),
            description: "Sandbox profile".to_string(),
            trust_tier: TrustTier::Experimental,
            receipt_spec: ReceiptSpec::DigestOnly,
            policies: vec![CustomPolicyRef::RequireSignedReceipts],
            runtime_constraints: vec![CustomRuntimeConstraint {
                allowed_runtimes: vec!["stdio".to_string()],
                forbid_defaults: false,
                require_explicit: false,
            }],
            metadata: HashMap::new(),
        };
        let profile = entry.into_profile("sandbox");
        assert_eq!(profile.id.as_str(), "sandbox");
        assert_eq!(profile.trust_requirements, TrustTier::Experimental);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::DigestOnly);
        assert_eq!(profile.policy_overlays.len(), 1);
        assert_eq!(profile.runtime_constraints.len(), 1);
    }

    #[test]
    fn test_load_from_missing_file_returns_builtins() {
        let dir = tempfile::tempdir().unwrap();
        let profiles = ProfileLoader::load(dir.path()).unwrap();
        assert_eq!(profiles.len(), 3);
    }

    #[test]
    fn test_load_from_existing_file_adds_custom_profiles() {
        let dir = tempfile::tempdir().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();
        let toml = r#"
[profiles.my-team]
name = "My Team"
description = "Team profile"
trust_tier = "community_reviewed"
receipt_spec = "digest_only"
policies = ["require_signed_receipts"]
"#;
        std::fs::write(ggen_dir.join("profiles.toml"), toml).unwrap();
        let profiles = ProfileLoader::load(dir.path()).unwrap();
        assert_eq!(profiles.len(), 4);
        let custom = profiles
            .iter()
            .find(|p| p.id.as_str() == "my-team")
            .unwrap();
        assert_eq!(custom.name, "My Team");
        assert_eq!(custom.trust_requirements, TrustTier::CommunityReviewed);
    }

    #[test]
    fn test_load_skips_collision_with_builtin_ids() {
        let dir = tempfile::tempdir().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();
        let toml = r#"
[profiles.enterprise-strict]
name = "Hacked"
description = "Should be ignored"
"#;
        std::fs::write(ggen_dir.join("profiles.toml"), toml).unwrap();
        let profiles = ProfileLoader::load(dir.path()).unwrap();
        assert_eq!(profiles.len(), 3);
        let builtin = profiles
            .iter()
            .find(|p| p.id.as_str() == "enterprise-strict")
            .unwrap();
        assert_eq!(builtin.name, "Enterprise Strict");
    }

    #[test]
    fn test_get_profile_finds_custom_then_builtin() {
        let dir = tempfile::tempdir().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();
        let toml = r#"
[profiles.my-custom]
name = "Custom"
description = "Custom profile"
trust_tier = "production_ready"
"#;
        std::fs::write(ggen_dir.join("profiles.toml"), toml).unwrap();
        let profile = ProfileLoader::get_profile(dir.path(), "my-custom").unwrap();
        assert_eq!(profile.trust_requirements, TrustTier::ProductionReady);
        let builtin = ProfileLoader::get_profile(dir.path(), "development").unwrap();
        assert_eq!(builtin.id.as_str(), "development");
    }

    #[test]
    fn test_custom_runtime_constraint_conversion() {
        let custom = CustomRuntimeConstraint {
            allowed_runtimes: vec!["axum".to_string()],
            forbid_defaults: true,
            require_explicit: true,
        };
        let constraint: RuntimeConstraint = custom.into();
        assert!(constraint.allows_runtime("axum"));
        assert!(!constraint.allows_runtime("wasi"));
    }

    #[test]
    fn test_custom_policy_ref_conversion() {
        let refs = vec![
            CustomPolicyRef::ForbidTemplateDefaults,
            CustomPolicyRef::RequireSignedReceipts,
        ];
        let rules: Vec<PolicyRule> = refs.into_iter().map(Into::into).collect();
        assert_eq!(rules.len(), 2);
        assert_eq!(rules[0], PolicyRule::ForbidTemplateDefaults);
    }

    #[test]
    fn test_default_path_construction() {
        let path = ProfileLoader::default_path(Path::new("/projects/my-app"));
        assert_eq!(path, PathBuf::from("/projects/my-app/.ggen/profiles.toml"));
    }

    #[test]
    fn test_empty_profiles_toml() {
        let config = ProfileLoader::parse_str("").unwrap();
        assert!(config.profiles.is_empty());
    }

    #[test]
    fn test_custom_profile_with_metadata() {
        let toml = r#"
[profiles.audit]
name = "Audit"
trust_tier = "enterprise_certified"

[profiles.audit.metadata]
department = "compliance"
"#;
        let config = ProfileLoader::parse_str(toml).unwrap();
        let entry = config.profiles.get("audit").unwrap();
        assert_eq!(entry.metadata.get("department").unwrap(), "compliance");
    }

    #[test]
    fn test_custom_profile_with_runtime_constraints() {
        let toml = r#"
[profiles.locked-down]
name = "Locked Down"
trust_tier = "enterprise_certified"

[[profiles.locked-down.runtime_constraints]]
allowed_runtimes = ["axum"]
forbid_defaults = true
require_explicit = true
"#;
        let config = ProfileLoader::parse_str(toml).unwrap();
        let entry = config.profiles.get("locked-down").unwrap();
        assert_eq!(entry.runtime_constraints.len(), 1);
        assert_eq!(entry.runtime_constraints[0].allowed_runtimes, vec!["axum"]);
    }
}
