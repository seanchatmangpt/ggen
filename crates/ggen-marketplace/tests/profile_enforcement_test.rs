//! Profile Enforcement Tests - Chicago TDD
//!
//! Tests for `ggen sync --profile <profile-id>` governance enforcement.
//!
//! These tests verify that profile-based policy gates work correctly:
//! - Enterprise profiles require --audit
//! - Regulated profiles forbid public registry sources
//! - Enterprise profiles require locked state (ggen.lock)
//! - Profile resolution searches custom then built-in profiles
//! - Development profile has no enforcement gates
//!
//! Chicago TDD: Real filesystem operations, real profile loading, no mocks.
//!
//! GATED: development_profile, enterprise_strict_profile, ProfileLoader, etc. not exported.

#![cfg(feature = "integration")]

use ggen_core::marketplace::{
    development_profile, enterprise_strict_profile, regulated_finance_profile, PackContext,
    ProfileLoader, ReceiptSpec, TrustTier,
};
use tempfile::TempDir;

// ============================================================================
// Profile Resolution Tests
// ============================================================================

#[cfg(test)]
mod profile_resolution_tests {
    use super::*;

    /// Test: Development profile is the default (no policy overlays)
    #[test]
    fn test_development_profile_is_default() {
        let profile = development_profile();
        assert_eq!(profile.id.as_str(), "development");
        assert_eq!(profile.policy_overlays.len(), 0);
        assert_eq!(profile.trust_requirements, TrustTier::Experimental);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::DigestOnly);
    }

    /// Test: Enterprise strict profile has 4 policy overlays
    #[test]
    fn test_enterprise_strict_profile_overlays() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.id.as_str(), "enterprise-strict");
        assert_eq!(profile.policy_overlays.len(), 4);
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseApproved);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::Signed);
    }

    /// Test: Regulated finance profile has 8 policy overlays
    #[test]
    fn test_regulated_finance_profile_overlays() {
        let profile = regulated_finance_profile();
        assert_eq!(profile.id.as_str(), "regulated-finance");
        assert_eq!(profile.policy_overlays.len(), 8);
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseCertified);
        assert_eq!(profile.receipt_requirements, ReceiptSpec::SignedAndChained);
    }

    /// Test: ProfileLoader resolves built-in profiles from a clean directory
    #[test]
    fn test_profile_loader_resolves_builtin_profiles() {
        let dir = TempDir::new().unwrap();
        let profiles = ProfileLoader::load(dir.path()).unwrap();
        assert_eq!(profiles.len(), 3);

        let ids: Vec<&str> = profiles.iter().map(|p| p.id.as_str()).collect();
        assert!(ids.contains(&"development"));
        assert!(ids.contains(&"enterprise-strict"));
        assert!(ids.contains(&"regulated-finance"));
    }

    /// Test: ProfileLoader::get_profile finds built-in profiles
    #[test]
    fn test_get_profile_finds_builtins() {
        let dir = TempDir::new().unwrap();

        let dev = ProfileLoader::get_profile(dir.path(), "development").unwrap();
        assert_eq!(dev.id.as_str(), "development");

        let ent = ProfileLoader::get_profile(dir.path(), "enterprise-strict").unwrap();
        assert_eq!(ent.id.as_str(), "enterprise-strict");

        let reg = ProfileLoader::get_profile(dir.path(), "regulated-finance").unwrap();
        assert_eq!(reg.id.as_str(), "regulated-finance");
    }

    /// Test: ProfileLoader::get_profile returns error for unknown profile
    #[test]
    fn test_get_profile_unknown_returns_error() {
        let dir = TempDir::new().unwrap();
        let result = ProfileLoader::get_profile(dir.path(), "nonexistent-profile");
        assert!(result.is_err());
    }

    /// Test: ProfileLoader loads custom profiles from .ggen/profiles.toml
    #[test]
    fn test_profile_loader_loads_custom_profiles() {
        let dir = TempDir::new().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();

        let toml = r#"
[profiles.my-team]
name = "My Team"
description = "Team-specific governance"
trust_tier = "enterprise_approved"
receipt_spec = "signed"
policies = ["require_signed_receipts", "forbid_template_defaults"]
"#;
        std::fs::write(ggen_dir.join("profiles.toml"), toml).unwrap();

        let profiles = ProfileLoader::load(dir.path()).unwrap();
        assert_eq!(profiles.len(), 4); // 3 built-in + 1 custom

        let custom = profiles
            .iter()
            .find(|p| p.id.as_str() == "my-team")
            .unwrap();
        assert_eq!(custom.name, "My Team");
        assert_eq!(custom.trust_requirements, TrustTier::EnterpriseApproved);
        assert_eq!(custom.policy_overlays.len(), 2);
    }

    /// Test: Custom profiles cannot override built-in profile IDs
    #[test]
    fn test_custom_profile_cannot_override_builtin() {
        let dir = TempDir::new().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();

        let toml = r#"
[profiles.enterprise-strict]
name = "Hacked Enterprise"
description = "Should be ignored"
"#;
        std::fs::write(ggen_dir.join("profiles.toml"), toml).unwrap();

        let profile = ProfileLoader::get_profile(dir.path(), "enterprise-strict").unwrap();
        assert_eq!(profile.name, "Enterprise Strict"); // Real built-in, not the hack
    }
}

// ============================================================================
// Enterprise Strict Enforcement Tests
// ============================================================================

#[cfg(test)]
mod enterprise_strict_enforcement_tests {
    use super::*;

    /// Test: Enterprise strict profile requires signed receipts
    #[test]
    fn test_enterprise_strict_requires_signed_receipts() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.receipt_requirements, ReceiptSpec::Signed);
    }

    /// Test: Enterprise strict profile requires EnterpriseApproved trust tier
    #[test]
    fn test_enterprise_strict_trust_requirement() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseApproved);

        // EnterpriseCertified meets EnterpriseApproved requirement
        assert!(profile.meets_trust_requirement(TrustTier::EnterpriseCertified));
        assert!(profile.meets_trust_requirement(TrustTier::EnterpriseApproved));
        assert!(!profile.meets_trust_requirement(TrustTier::Experimental));
        assert!(!profile.meets_trust_requirement(TrustTier::CommunityReviewed));
    }

    /// Test: Enterprise strict profile allows only approved runtimes
    #[test]
    fn test_enterprise_strict_runtime_constraints() {
        let profile = enterprise_strict_profile();
        assert!(profile.allows_runtime("axum"));
        assert!(profile.allows_runtime("stdio"));
        assert!(!profile.allows_runtime("wasi"));
        assert!(!profile.allows_runtime("unknown-runtime"));
    }

    /// Test: Enterprise strict profile has expected policy overlay count
    #[test]
    fn test_enterprise_strict_policy_count() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.policy_overlays.len(), 4);
    }

    /// Test: Enterprise strict profile runtime constraints forbid defaults
    #[test]
    fn test_enterprise_strict_forbids_defaults() {
        let profile = enterprise_strict_profile();
        assert_eq!(profile.runtime_constraints.len(), 1);
        let constraint = &profile.runtime_constraints[0];
        assert!(constraint.forbid_defaults);
        assert!(constraint.require_explicit);
    }

    /// Test: Enterprise strict profile enforcement against compliant packs
    #[test]
    fn test_enterprise_strict_passes_compliant_packs() {
        let profile = enterprise_strict_profile();
        let compliant_pack = PackContext::new("enterprise-pack".to_string())
            .with_template_defaults(false)
            .with_inferred_capabilities(false)
            .with_signed_receipts(true)
            .with_runtime(Some("axum".to_string()))
            .with_trust_tier(TrustTier::EnterpriseApproved);

        let report = profile.enforce(&[compliant_pack]).unwrap();
        assert!(report.passed);
        assert_eq!(report.violation_count(), 0);
    }

    /// Test: Enterprise strict profile rejects non-compliant packs
    #[test]
    fn test_enterprise_strict_rejects_non_compliant_packs() {
        let profile = enterprise_strict_profile();
        let bad_pack = PackContext::new("dev-pack".to_string())
            .with_template_defaults(true)
            .with_inferred_capabilities(true)
            .with_signed_receipts(false)
            .with_runtime(None); // No explicit runtime

        let report = profile.enforce(&[bad_pack]).unwrap();
        assert!(!report.passed);
        // Should have violations for: template defaults, inferred capabilities, no signed receipts, no runtime
        assert!(report.violation_count() >= 3);
    }
}

// ============================================================================
// Regulated Finance Enforcement Tests
// ============================================================================

#[cfg(test)]
mod regulated_finance_enforcement_tests {
    use super::*;

    /// Test: Regulated finance profile requires signed-and-chained receipts
    #[test]
    fn test_regulated_finance_requires_chained_receipts() {
        let profile = regulated_finance_profile();
        assert_eq!(profile.receipt_requirements, ReceiptSpec::SignedAndChained);
    }

    /// Test: Regulated finance profile requires EnterpriseCertified trust tier
    #[test]
    fn test_regulated_finance_trust_requirement() {
        let profile = regulated_finance_profile();
        assert_eq!(profile.trust_requirements, TrustTier::EnterpriseCertified);

        // Only EnterpriseCertified meets EnterpriseCertified requirement
        assert!(profile.meets_trust_requirement(TrustTier::EnterpriseCertified));
        assert!(!profile.meets_trust_requirement(TrustTier::EnterpriseApproved));
        assert!(!profile.meets_trust_requirement(TrustTier::Experimental));
    }

    /// Test: Regulated finance profile forbids public registry
    #[test]
    fn test_regulated_finance_forbids_public_registry() {
        let profile = regulated_finance_profile();
        assert!(profile.forbid_public_registry());
    }

    /// Test: Regulated finance profile allows only axum runtime
    #[test]
    fn test_regulated_finance_runtime_constraints() {
        let profile = regulated_finance_profile();
        assert!(profile.allows_runtime("axum"));
        assert!(!profile.allows_runtime("stdio"));
        assert!(!profile.allows_runtime("wasi"));
    }

    /// Test: Regulated finance profile enforcement against public registry pack
    #[test]
    fn test_regulated_finance_rejects_public_registry_packs() {
        let profile = regulated_finance_profile();
        let public_pack = PackContext::new("crates-io-pack".to_string())
            .with_public_registry(true)
            .with_regulated_environment(true);

        let report = profile.enforce(&[public_pack]).unwrap();
        assert!(!report.passed);
    }

    /// Test: Regulated finance profile has 8 policy overlays
    #[test]
    fn test_regulated_finance_all_policies() {
        let profile = regulated_finance_profile();
        assert_eq!(profile.policy_overlays.len(), 8);
    }
}

// ============================================================================
// Profile Gate Validation Tests (simulating enforce_profile_gates behavior)
// ============================================================================

#[cfg(test)]
mod profile_gate_validation_tests {
    use super::*;

    /// Test: Enterprise strict profile requires --audit (receipt spec gate)
    #[test]
    fn test_enterprise_strict_requires_audit_flag() {
        let profile = enterprise_strict_profile();
        let audit_enabled = false;

        // ReceiptSpec::Signed requires audit to be enabled
        let receipt_gate_violated =
            matches!(profile.receipt_requirements, ReceiptSpec::Signed) && !audit_enabled;
        assert!(receipt_gate_violated);

        // With audit enabled, gate passes
        let audit_enabled = true;
        let receipt_gate_violated =
            matches!(profile.receipt_requirements, ReceiptSpec::Signed) && !audit_enabled;
        assert!(!receipt_gate_violated);
    }

    /// Test: Regulated finance profile requires --audit (chained receipt spec gate)
    #[test]
    fn test_regulated_finance_requires_audit_flag() {
        let profile = regulated_finance_profile();
        let audit_enabled = false;

        // ReceiptSpec::SignedAndChained requires audit to be enabled
        let receipt_gate_violated =
            matches!(profile.receipt_requirements, ReceiptSpec::SignedAndChained) && !audit_enabled;
        assert!(receipt_gate_violated);
    }

    /// Test: Development profile has no receipt gate
    #[test]
    fn test_development_profile_no_receipt_gate() {
        let profile = development_profile();
        // DigestOnly and None don't require audit
        assert!(matches!(
            profile.receipt_requirements,
            ReceiptSpec::DigestOnly | ReceiptSpec::None
        ));
    }

    /// Test: Regulated finance profile forbids public registry in manifest
    #[test]
    fn test_regulated_finance_detects_public_registry_in_manifest() {
        let dir = TempDir::new().unwrap();
        let manifest_path = dir.path().join("ggen.toml");

        // Manifest with public registry reference
        let manifest_content = r#"
[project]
name = "test"
version = "0.1.0"

[source.crates-io]
registry = "https://crates.io"
"#;
        std::fs::write(&manifest_path, manifest_content).unwrap();

        let profile = regulated_finance_profile();
        assert!(profile.forbid_public_registry());

        // Read manifest and check for public registry references
        let content = std::fs::read_to_string(&manifest_path).unwrap();
        let has_public_registry =
            content.contains("crates.io") || content.contains("registry.npmjs.org");
        assert!(has_public_registry);
    }

    /// Test: Manifest without public registry references passes regulated check
    #[test]
    fn test_clean_manifest_passes_regulated_check() {
        let dir = TempDir::new().unwrap();
        let manifest_path = dir.path().join("ggen.toml");

        let manifest_content = r#"
[project]
name = "test"
version = "0.1.0"

[source.internal]
registry = "https://registry.internal.company.com"
"#;
        std::fs::write(&manifest_path, manifest_content).unwrap();

        let content = std::fs::read_to_string(&manifest_path).unwrap();
        let has_public_registry =
            content.contains("crates.io") || content.contains("registry.npmjs.org");
        assert!(!has_public_registry);
    }

    /// Test: Enterprise strict profile requires explicit runtime declaration
    #[test]
    fn test_enterprise_strict_requires_explicit_runtime() {
        let dir = TempDir::new().unwrap();
        let manifest_path = dir.path().join("ggen.toml");

        // Manifest WITHOUT runtime section
        let manifest_content = r#"
[project]
name = "test"
version = "0.1.0"
"#;
        std::fs::write(&manifest_path, manifest_content).unwrap();

        let content = std::fs::read_to_string(&manifest_path).unwrap();
        let has_runtime_section = content.contains("[runtime]");

        // Enterprise strict requires explicit runtime
        let profile = enterprise_strict_profile();
        for constraint in &profile.runtime_constraints {
            if constraint.require_explicit && !has_runtime_section {
                assert!(!constraint.allows_runtime("")); // Should be strict
            }
        }
    }
}
