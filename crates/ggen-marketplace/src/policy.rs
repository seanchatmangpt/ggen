//! Policy enforcement for governed marketplace.
//!
//! Implements Fortune 5 CISO requirements for enterprise policy enforcement
//! through pack-based policy system.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

use crate::error::{Error, Result};
use crate::ownership::OwnershipClass;
use crate::trust::TrustTier;

/// Unique identifier for a policy.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PolicyId(String);

impl PolicyId {
    /// Create a new policy ID.
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

impl fmt::Display for PolicyId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for PolicyId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// Policy rule for enforcing enterprise requirements.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PolicyRule {
    /// Forbid use of template defaults (all values must be explicit).
    ForbidTemplateDefaults,

    /// Forbid inferred capabilities (all must be declared).
    ForbidInferredCapabilities,

    /// Require all receipts to be cryptographically signed.
    RequireSignedReceipts,

    /// Require specific approved runtime types only.
    RequireApprovedRuntime(Vec<String>),

    /// Forbid public registry packs in regulated environments.
    ForbidPublicRegistryInRegulated,

    /// Require minimum trust tier for all packs.
    RequireTrustTier(TrustTier),

    /// Require specific ownership class for all targets.
    RequireOwnershipClass(OwnershipClass),

    /// Require explicit runtime declaration (no defaults).
    RequireExplicitRuntime,

    /// Forbid unlisted packages (must be in allowlist).
    ForbidUnlistedPackages,

    /// Require all packs to have signature verification.
    RequireSignatureVerification,

    /// Require semantic versioning for all packs.
    RequireSemanticVersioning,

    /// Forbid downgrade attacks (version must not decrease).
    ForbidVersionDowngrade,

    /// Require composition receipts for all builds.
    RequireCompositionReceipts,

    /// Require provenance tracking for all artifacts.
    RequireProvenanceTracking,

    /// Custom policy rule with SPARQL validation.
    CustomSparql {
        rule_name: String,
        sparql_query: String,
        expected_result: String,
    },

    /// Custom policy rule with shell validation.
    CustomShell {
        rule_name: String,
        shell_command: String,
        expected_exit_code: i32,
    },
}

impl PolicyRule {
    /// Get a human-readable description of this rule.
    #[must_use]
    pub fn description(&self) -> String {
        match self {
            Self::ForbidTemplateDefaults => {
                "All template values must be explicit; defaults are forbidden".to_string()
            }
            Self::ForbidInferredCapabilities => {
                "All capabilities must be explicitly declared; inference is forbidden".to_string()
            }
            Self::RequireSignedReceipts => {
                "All receipts must be cryptographically signed".to_string()
            }
            Self::RequireApprovedRuntime(runtimes) => {
                format!("Only approved runtimes allowed: {runtimes:?}")
            }
            Self::ForbidPublicRegistryInRegulated => {
                "Public registry packs are forbidden in regulated environments".to_string()
            }
            Self::RequireTrustTier(tier) => {
                format!("All packs must meet trust tier requirement: {tier:?}")
            }
            Self::RequireOwnershipClass(class) => {
                format!("All targets must use ownership class: {class:?}")
            }
            Self::RequireExplicitRuntime => {
                "Runtime must be explicitly declared; no defaults allowed".to_string()
            }
            Self::ForbidUnlistedPackages => {
                "Only allowlisted packages are permitted".to_string()
            }
            Self::RequireSignatureVerification => {
                "All packs must have verified signatures".to_string()
            }
            Self::RequireSemanticVersioning => {
                "All packs must use semantic versioning".to_string()
            }
            Self::ForbidVersionDowngrade => {
                "Version downgrade attacks are forbidden".to_string()
            }
            Self::RequireCompositionReceipts => {
                "All builds must generate composition receipts".to_string()
            }
            Self::RequireProvenanceTracking => {
                "All artifacts must track provenance".to_string()
            }
            Self::CustomSparql { rule_name, .. } => {
                format!("Custom SPARQL policy: {rule_name}")
            }
            Self::CustomShell { rule_name, .. } => {
                format!("Custom shell policy: {rule_name}")
            }
        }
    }
}

/// Policy pack containing rules for enforcement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Policy {
    /// Unique policy identifier.
    pub id: PolicyId,

    /// Human-readable policy name.
    pub name: String,

    /// Policy description.
    pub description: String,

    /// Rules to enforce.
    pub rules: Vec<PolicyRule>,

    /// Additional metadata.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, String>,
}

impl Policy {
    /// Create a new policy.
    #[must_use]
    pub fn new(id: PolicyId, name: String, description: String, rules: Vec<PolicyRule>) -> Self {
        Self {
            id,
            name,
            description,
            rules,
            metadata: HashMap::new(),
        }
    }

    /// Create a policy that forbids template defaults.
    #[must_use]
    pub fn forbid_template_defaults() -> Self {
        Self::new(
            PolicyId::new("forbid-template-defaults"),
            "Forbid Template Defaults".to_string(),
            "All template values must be explicit; defaults are forbidden".to_string(),
            vec![PolicyRule::ForbidTemplateDefaults],
        )
    }

    /// Create a policy that forbids inferred capabilities.
    #[must_use]
    pub fn forbid_inferred_capabilities() -> Self {
        Self::new(
            PolicyId::new("forbid-inferred-capabilities"),
            "Forbid Inferred Capabilities".to_string(),
            "All capabilities must be explicitly declared; inference is forbidden".to_string(),
            vec![PolicyRule::ForbidInferredCapabilities],
        )
    }

    /// Create a policy that requires signed receipts.
    #[must_use]
    pub fn require_signed_receipts() -> Self {
        Self::new(
            PolicyId::new("require-signed-receipts"),
            "Require Signed Receipts".to_string(),
            "All receipts must be cryptographically signed".to_string(),
            vec![PolicyRule::RequireSignedReceipts],
        )
    }

    /// Create a policy that requires explicit runtime.
    #[must_use]
    pub fn require_explicit_runtime() -> Self {
        Self::new(
            PolicyId::new("require-explicit-runtime"),
            "Require Explicit Runtime".to_string(),
            "Runtime must be explicitly declared; no defaults allowed".to_string(),
            vec![PolicyRule::RequireExplicitRuntime],
        )
    }

    /// Create a policy that forbids public registry in regulated environments.
    #[must_use]
    pub fn forbid_public_registry_in_regulated() -> Self {
        Self::new(
            PolicyId::new("forbid-public-registry-regulated"),
            "Forbid Public Registry in Regulated".to_string(),
            "Public registry packs are forbidden in regulated environments".to_string(),
            vec![PolicyRule::ForbidPublicRegistryInRegulated],
        )
    }

    /// Create a policy that requires a specific trust tier.
    #[must_use]
    pub fn require_trust_tier(tier: TrustTier) -> Self {
        Self::new(
            PolicyId::new(&format!("require-trust-tier-{:?}", tier)),
            format!("Require Trust Tier: {:?}", tier),
            format!("All packs must meet trust tier requirement: {:?}", tier),
            vec![PolicyRule::RequireTrustTier(tier)],
        )
    }

    /// Create a policy that requires approved runtimes.
    #[must_use]
    pub fn require_approved_runtime(runtimes: Vec<String>) -> Self {
        Self::new(
            PolicyId::new("require-approved-runtime"),
            "Require Approved Runtime".to_string(),
            format!("Only approved runtimes allowed: {:?}", runtimes),
            vec![PolicyRule::RequireApprovedRuntime(runtimes)],
        )
    }

    /// Create a policy that forbids unlisted packages.
    #[must_use]
    pub fn forbid_unlisted_packages() -> Self {
        Self::new(
            PolicyId::new("forbid-unlisted-packages"),
            "Forbid Unlisted Packages".to_string(),
            "Only allowlisted packages are permitted".to_string(),
            vec![PolicyRule::ForbidUnlistedPackages],
        )
    }

    /// Create a policy that requires signature verification.
    #[must_use]
    pub fn require_signature_verification() -> Self {
        Self::new(
            PolicyId::new("require-signature-verification"),
            "Require Signature Verification".to_string(),
            "All packs must have verified signatures".to_string(),
            vec![PolicyRule::RequireSignatureVerification],
        )
    }

    /// Create a policy that requires composition receipts.
    #[must_use]
    pub fn require_composition_receipts() -> Self {
        Self::new(
            PolicyId::new("require-composition-receipts"),
            "Require Composition Receipts".to_string(),
            "All builds must generate composition receipts".to_string(),
            vec![PolicyRule::RequireCompositionReceipts],
        )
    }

    /// Create a policy that requires provenance tracking.
    #[must_use]
    pub fn require_provenance_tracking() -> Self {
        Self::new(
            PolicyId::new("require-provenance-tracking"),
            "Require Provenance Tracking".to_string(),
            "All artifacts must track provenance".to_string(),
            vec![PolicyRule::RequireProvenanceTracking],
        )
    }

    /// Check if this policy contains a specific rule type.
    #[must_use]
    pub fn contains_rule(&self, rule: &PolicyRule) -> bool {
        self.rules.iter().any(|r| std::mem::discriminant(r) == std::mem::discriminant(rule))
    }
}

/// Policy violation report.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PolicyViolation {
    /// Policy that was violated.
    pub policy_id: PolicyId,

    /// Rule that was violated.
    pub rule: PolicyRule,

    /// Pack that caused the violation.
    pub pack_id: String,

    /// Human-readable violation description.
    pub description: String,

    /// Additional context.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub context: HashMap<String, String>,
}

impl PolicyViolation {
    /// Create a new policy violation.
    #[must_use]
    pub fn new(
        policy_id: PolicyId,
        rule: PolicyRule,
        pack_id: String,
        description: String,
    ) -> Self {
        Self {
            policy_id,
            rule,
            pack_id,
            description,
            context: HashMap::new(),
        }
    }

    /// Add context to the violation.
    #[must_use]
    pub fn with_context(mut self, key: String, value: String) -> Self {
        self.context.insert(key, value);
        self
    }
}

/// Policy enforcement report.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PolicyReport {
    /// Whether all policies passed.
    pub passed: bool,

    /// Policy violations found.
    pub violations: Vec<PolicyViolation>,

    /// Policies that were checked.
    pub policies_checked: Vec<PolicyId>,
}

impl PolicyReport {
    /// Create a new policy report.
    #[must_use]
    pub fn new(passed: bool, violations: Vec<PolicyViolation>, policies_checked: Vec<PolicyId>) -> Self {
        Self {
            passed,
            violations,
            policies_checked,
        }
    }

    /// Create a successful report (no violations).
    #[must_use]
    pub fn success(policies_checked: Vec<PolicyId>) -> Self {
        Self {
            passed: true,
            violations: Vec::new(),
            policies_checked,
        }
    }

    /// Create a failure report with violations.
    #[must_use]
    pub fn failure(
        violations: Vec<PolicyViolation>,
        policies_checked: Vec<PolicyId>,
    ) -> Self {
        Self {
            passed: false,
            violations,
            policies_checked,
        }
    }

    /// Get the number of violations.
    #[must_use]
    pub fn violation_count(&self) -> usize {
        self.violations.len()
    }

    /// Get violations by policy.
    #[must_use]
    pub fn violations_by_policy(&self, policy_id: &PolicyId) -> Vec<&PolicyViolation> {
        self.violations
            .iter()
            .filter(|v| &v.policy_id == policy_id)
            .collect()
    }
}

/// Policy enforcer for checking pack compliance.
pub struct PolicyEnforcer;

impl PolicyEnforcer {
    /// Create a new policy enforcer.
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    /// Enforce policies against a set of packs.
    ///
    /// # Errors
    ///
    /// Returns error if policy checking fails (not if violations are found).
    pub fn enforce(
        policies: &[Policy],
        packs: &[PackContext],
    ) -> Result<PolicyReport> {
        let mut violations = Vec::new();
        let policies_checked: Vec<PolicyId> = policies.iter().map(|p| p.id.clone()).collect();

        for policy in policies {
            for pack in packs {
                if let Some(pack_violations) = Self::check_policy(policy, pack)? {
                    violations.extend(pack_violations);
                }
            }
        }

        let passed = violations.is_empty();
        Ok(PolicyReport::new(passed, violations, policies_checked))
    }

    /// Check a single policy against a single pack.
    ///
    /// # Errors
    ///
    /// Returns error if policy checking fails.
    ///
    /// # Returns
    ///
    /// Returns Some(violations) if violations found, None if pack passes.
    fn check_policy(
        policy: &Policy,
        pack: &PackContext,
    ) -> Result<Option<Vec<PolicyViolation>>> {
        let mut violations = Vec::new();

        for rule in &policy.rules {
            if let Some(violation) = Self::check_rule(rule, pack, policy)? {
                violations.push(violation);
            }
        }

        Ok(if violations.is_empty() {
            None
        } else {
            Some(violations)
        })
    }

    /// Check a single rule against a pack.
    ///
    /// # Errors
    ///
    /// Returns error if rule checking fails.
    ///
    /// # Returns
    ///
    /// Returns Some(violation) if rule violated, None if rule passes.
    fn check_rule(
        rule: &PolicyRule,
        pack: &PackContext,
        policy: &Policy,
    ) -> Result<Option<PolicyViolation>> {
        let violated = match rule {
            PolicyRule::ForbidTemplateDefaults => {
                pack.has_template_defaults
            }
            PolicyRule::ForbidInferredCapabilities => {
                pack.has_inferred_capabilities
            }
            PolicyRule::RequireSignedReceipts => {
                !pack.has_signed_receipts
            }
            PolicyRule::RequireApprovedRuntime(runtimes) => {
                !pack.runtime.as_ref().is_some_and(|r| runtimes.contains(r))
            }
            PolicyRule::ForbidPublicRegistryInRegulated => {
                pack.is_regulated_environment && pack.uses_public_registry
            }
            PolicyRule::RequireTrustTier(required_tier) => {
                !pack.trust_tier.meets_requirement(*required_tier)
            }
            PolicyRule::RequireOwnershipClass(required_class) => {
                pack.ownership_classes.iter().all(|c| c != required_class)
            }
            PolicyRule::RequireExplicitRuntime => {
                pack.runtime.is_none()
            }
            PolicyRule::ForbidUnlistedPackages => {
                !pack.is_allowlisted
            }
            PolicyRule::RequireSignatureVerification => {
                !pack.has_signature_verification
            }
            PolicyRule::RequireSemanticVersioning => {
                !pack.uses_semver
            }
            PolicyRule::ForbidVersionDowngrade => {
                pack.is_version_downgrade
            }
            PolicyRule::RequireCompositionReceipts => {
                !pack.has_composition_receipt
            }
            PolicyRule::RequireProvenanceTracking => {
                !pack.has_provenance_tracking
            }
            PolicyRule::CustomSparql { .. } | PolicyRule::CustomShell { .. } => {
                // Custom rules require execution context
                return Err(Error::ValidationFailed {
                    reason: "Custom policy rules require execution context".to_string(),
                });
            }
        };

        Ok(if violated {
            Some(PolicyViolation::new(
                policy.id.clone(),
                rule.clone(),
                pack.id.clone(),
                format!(
                    "Policy '{}' violated by pack '{}': {}",
                    policy.name,
                    pack.id,
                    rule.description()
                ),
            ))
        } else {
            None
        })
    }
}

impl Default for PolicyEnforcer {
    fn default() -> Self {
        Self::new()
    }
}

/// Pack context for policy enforcement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackContext {
    /// Pack identifier.
    pub id: String,

    /// Whether pack uses template defaults.
    pub has_template_defaults: bool,

    /// Whether pack has inferred capabilities.
    pub has_inferred_capabilities: bool,

    /// Whether pack has signed receipts.
    pub has_signed_receipts: bool,

    /// Runtime type (if specified).
    pub runtime: Option<String>,

    /// Whether environment is regulated.
    pub is_regulated_environment: bool,

    /// Whether pack uses public registry.
    pub uses_public_registry: bool,

    /// Pack trust tier.
    pub trust_tier: TrustTier,

    /// Ownership classes used by pack.
    pub ownership_classes: Vec<OwnershipClass>,

    /// Whether pack is allowlisted.
    pub is_allowlisted: bool,

    /// Whether pack has signature verification.
    pub has_signature_verification: bool,

    /// Whether pack uses semantic versioning.
    pub uses_semver: bool,

    /// Whether this is a version downgrade.
    pub is_version_downgrade: bool,

    /// Whether pack has composition receipt.
    pub has_composition_receipt: bool,

    /// Whether pack has provenance tracking.
    pub has_provenance_tracking: bool,
}

impl PackContext {
    /// Create a new pack context.
    #[must_use]
    pub fn new(id: String) -> Self {
        Self {
            id,
            has_template_defaults: false,
            has_inferred_capabilities: false,
            has_signed_receipts: false,
            runtime: None,
            is_regulated_environment: false,
            uses_public_registry: false,
            trust_tier: TrustTier::Experimental,
            ownership_classes: Vec::new(),
            is_allowlisted: false,
            has_signature_verification: false,
            uses_semver: false,
            is_version_downgrade: false,
            has_composition_receipt: false,
            has_provenance_tracking: false,
        }
    }

    /// Set whether pack uses template defaults.
    #[must_use]
    pub fn with_template_defaults(mut self, value: bool) -> Self {
        self.has_template_defaults = value;
        self
    }

    /// Set whether pack has inferred capabilities.
    #[must_use]
    pub fn with_inferred_capabilities(mut self, value: bool) -> Self {
        self.has_inferred_capabilities = value;
        self
    }

    /// Set whether pack has signed receipts.
    #[must_use]
    pub fn with_signed_receipts(mut self, value: bool) -> Self {
        self.has_signed_receipts = value;
        self
    }

    /// Set runtime type.
    #[must_use]
    pub fn with_runtime(mut self, runtime: Option<String>) -> Self {
        self.runtime = runtime;
        self
    }

    /// Set whether environment is regulated.
    #[must_use]
    pub fn with_regulated_environment(mut self, value: bool) -> Self {
        self.is_regulated_environment = value;
        self
    }

    /// Set whether pack uses public registry.
    #[must_use]
    pub fn with_public_registry(mut self, value: bool) -> Self {
        self.uses_public_registry = value;
        self
    }

    /// Set trust tier.
    #[must_use]
    pub fn with_trust_tier(mut self, tier: TrustTier) -> Self {
        self.trust_tier = tier;
        self
    }

    /// Add ownership class.
    #[must_use]
    pub fn with_ownership_class(mut self, class: OwnershipClass) -> Self {
        self.ownership_classes.push(class);
        self
    }

    /// Set whether pack is allowlisted.
    #[must_use]
    pub fn with_allowlisted(mut self, value: bool) -> Self {
        self.is_allowlisted = value;
        self
    }

    /// Set whether pack has signature verification.
    #[must_use]
    pub fn with_signature_verification(mut self, value: bool) -> Self {
        self.has_signature_verification = value;
        self
    }

    /// Set whether pack uses semantic versioning.
    #[must_use]
    pub fn with_semver(mut self, value: bool) -> Self {
        self.uses_semver = value;
        self
    }

    /// Set whether this is a version downgrade.
    #[must_use]
    pub fn with_version_downgrade(mut self, value: bool) -> Self {
        self.is_version_downgrade = value;
        self
    }

    /// Set whether pack has composition receipt.
    #[must_use]
    pub fn with_composition_receipt(mut self, value: bool) -> Self {
        self.has_composition_receipt = value;
        self
    }

    /// Set whether pack has provenance tracking.
    #[must_use]
    pub fn with_provenance_tracking(mut self, value: bool) -> Self {
        self.has_provenance_tracking = value;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_policy_id() {
        let id = PolicyId::new("test-policy");
        assert_eq!(id.as_str(), "test-policy");
        assert_eq!(id.to_string(), "test-policy");
    }

    #[test]
    fn test_policy_forbid_template_defaults() {
        let policy = Policy::forbid_template_defaults();
        assert_eq!(policy.id.as_str(), "forbid-template-defaults");
        assert_eq!(policy.rules.len(), 1);
        assert!(policy.contains_rule(&PolicyRule::ForbidTemplateDefaults));
    }

    #[test]
    fn test_policy_require_signed_receipts() {
        let policy = Policy::require_signed_receipts();
        assert_eq!(policy.id.as_str(), "require-signed-receipts");
        assert_eq!(policy.rules.len(), 1);
        assert!(policy.contains_rule(&PolicyRule::RequireSignedReceipts));
    }

    #[test]
    fn test_policy_enforcer_no_violations() {
        let policy = Policy::require_signed_receipts();
        let pack = PackContext::new("test-pack".to_string())
            .with_signed_receipts(true);

        let report = PolicyEnforcer::enforce(&[policy], &[pack]).unwrap();
        assert!(report.passed);
        assert_eq!(report.violation_count(), 0);
    }

    #[test]
    fn test_policy_enforcer_with_violations() {
        let policy = Policy::require_signed_receipts();
        let pack = PackContext::new("test-pack".to_string())
            .with_signed_receipts(false);

        let report = PolicyEnforcer::enforce(&[policy], &[pack]).unwrap();
        assert!(!report.passed);
        assert_eq!(report.violation_count(), 1);
        assert_eq!(report.violations[0].pack_id, "test-pack");
    }

    #[test]
    fn test_policy_enforcer_trust_tier() {
        let policy = Policy::require_trust_tier(TrustTier::EnterpriseApproved);

        // Pack with insufficient trust tier
        let pack_low = PackContext::new("experimental-pack".to_string())
            .with_trust_tier(TrustTier::Experimental);

        let report = PolicyEnforcer::enforce(&[policy.clone()], &[pack_low]).unwrap();
        assert!(!report.passed);
        assert_eq!(report.violation_count(), 1);

        // Pack with sufficient trust tier
        let pack_high = PackContext::new("enterprise-pack".to_string())
            .with_trust_tier(TrustTier::EnterpriseCertified);

        let report = PolicyEnforcer::enforce(&[policy], &[pack_high]).unwrap();
        assert!(report.passed);
    }

    #[test]
    fn test_policy_violation_with_context() {
        let policy_id = PolicyId::new("test-policy");
        let rule = PolicyRule::ForbidTemplateDefaults;
        let violation = PolicyViolation::new(
            policy_id.clone(),
            rule,
            "test-pack".to_string(),
            "Template defaults found".to_string(),
        )
        .with_context("file".to_string(), "template.rs".to_string())
        .with_context("line".to_string(), "42".to_string());

        assert_eq!(violation.context.len(), 2);
        assert_eq!(violation.context.get("file"), Some(&"template.rs".to_string()));
        assert_eq!(violation.context.get("line"), Some(&"42".to_string()));
    }

    #[test]
    fn test_policy_report_violations_by_policy() {
        let policy1 = PolicyId::new("policy-1");
        let policy2 = PolicyId::new("policy-2");

        let violations = vec![
            PolicyViolation::new(
                policy1.clone(),
                PolicyRule::ForbidTemplateDefaults,
                "pack-1".to_string(),
                "Violation 1".to_string(),
            ),
            PolicyViolation::new(
                policy1.clone(),
                PolicyRule::RequireSignedReceipts,
                "pack-2".to_string(),
                "Violation 2".to_string(),
            ),
            PolicyViolation::new(
                policy2.clone(),
                PolicyRule::RequireExplicitRuntime,
                "pack-3".to_string(),
                "Violation 3".to_string(),
            ),
        ];

        let report = PolicyReport::failure(violations, vec![policy1.clone(), policy2.clone()]);

        assert_eq!(report.violations_by_policy(&policy1).len(), 2);
        assert_eq!(report.violations_by_policy(&policy2).len(), 1);
    }

    #[test]
    fn test_pack_context_builder() {
        let pack = PackContext::new("test-pack".to_string())
            .with_template_defaults(true)
            .with_signed_receipts(false)
            .with_runtime(Some("axum".to_string()))
            .with_trust_tier(TrustTier::EnterpriseCertified)
            .with_allowlisted(true);

        assert!(pack.has_template_defaults);
        assert!(!pack.has_signed_receipts);
        assert_eq!(pack.runtime, Some("axum".to_string()));
        assert_eq!(pack.trust_tier, TrustTier::EnterpriseCertified);
        assert!(pack.is_allowlisted);
    }
}
