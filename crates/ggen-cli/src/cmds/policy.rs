//! Policy Commands
//!
//! This module provides policy management commands wired to the marketplace layer.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// Re-export marketplace types for policy enforcement
pub use ggen_marketplace::marketplace::policy::{PackContext, PolicyReport};
pub use ggen_marketplace::marketplace::profile::{predefined_profiles, Profile, ProfileId};

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
pub struct ListOutput {
    pub profiles: Vec<ProfileSummary>,
    pub total: usize,
}

#[derive(Serialize)]
pub struct ProfileSummary {
    pub id: String,
    pub name: String,
    pub description: String,
    pub policy_count: usize,
    pub trust_requirement: String,
    pub receipt_requirement: String,
}

#[derive(Serialize)]
pub struct ValidateOutput {
    pub profile_id: String,
    pub passed: bool,
    pub violation_count: usize,
    pub policies_checked: usize,
    pub violations: Vec<ViolationSummary>,
}

#[derive(Serialize)]
pub struct ViolationSummary {
    pub policy_id: String,
    pub pack_id: String,
    pub description: String,
}

#[derive(Serialize)]
pub struct ShowOutput {
    pub profile_id: String,
    pub name: String,
    pub description: String,
    pub policies: Vec<PolicySummary>,
    pub trust_requirement: String,
    pub receipt_requirement: String,
    pub runtime_constraints: Vec<RuntimeConstraintSummary>,
}

#[derive(Serialize)]
pub struct PolicySummary {
    pub id: String,
    pub name: String,
    pub description: String,
    pub rule_count: usize,
}

#[derive(Serialize)]
pub struct RuntimeConstraintSummary {
    pub allowed_runtimes: Vec<String>,
    pub forbid_defaults: bool,
    pub require_explicit: bool,
}

// ============================================================================
// Domain Helper Functions
// ============================================================================

/// Load pack contexts from the current project's lockfile
///
/// This function reads `.ggen/packs.lock` and loads metadata for each
/// installed pack to create `PackContext` objects for policy validation.
///
/// # Errors
///
/// Returns error if:
/// - Lockfile doesn't exist
/// - Lockfile is invalid
/// - Pack metadata cannot be loaded
fn load_pack_contexts_from_project() -> crate::Result<Vec<PackContext>> {
    use ggen_marketplace::marketplace::metadata::{get_pack_cache_dir, load_pack_metadata};
    use ggen_marketplace::packs::lockfile::PackLockfile;
    use std::path::Path;

    let lockfile_path = Path::new(".ggen/packs.lock");
    if !lockfile_path.exists() {
        return Err(crate::utils::error::Error::new(
            "No project found. Please install packs first with 'ggen packs install <pack-id>'",
        ));
    }

    let lockfile = PackLockfile::from_file(lockfile_path).map_err(|e| {
        crate::utils::error::Error::new(&format!("Failed to load lockfile: {}", e))
    })?;

    let mut pack_contexts = Vec::new();
    for (pack_id, locked_pack) in &lockfile.packs {
        let package_id = ggen_marketplace::marketplace::models::PackageId::new(pack_id).map_err(|e| {
            crate::utils::error::Error::new(&format!("Invalid package ID {}: {}", pack_id, e))
        })?;

        let cache_dir = get_pack_cache_dir(&package_id, &locked_pack.version);

        let metadata = load_pack_metadata(&cache_dir).map_err(|e| {
            crate::utils::error::Error::new(&format!(
                "Failed to load metadata for pack {}: {}",
                pack_id, e
            ))
        })?;

        let (template_defaults, runtime) = load_pack_config_from_cache(&cache_dir);

        let pack_context = PackContext::new(pack_id.clone())
            .with_template_defaults(template_defaults)
            .with_signed_receipts(metadata.signature.is_some())
            .with_runtime(runtime)
            .with_trust_tier(metadata.trust_tier)
            .with_signature_verification(metadata.signature.is_some());

        pack_contexts.push(pack_context);
    }

    Ok(pack_contexts)
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Load template_defaults and runtime from pack.toml in the cache directory.
///
/// Reads the `[pack]` section for `use_defaults` and `runtime` fields.
/// Returns `(use_template_defaults, runtime)` tuple.
fn load_pack_config_from_cache(cache_dir: &std::path::Path) -> (bool, Option<String>) {
    use std::fs;

    let pack_toml_path = cache_dir.join("pack.toml");
    if !pack_toml_path.exists() {
        return (false, None);
    }

    let content = match fs::read_to_string(&pack_toml_path) {
        Ok(c) => c,
        Err(_) => return (false, None),
    };

    let value: toml::Value = match toml::from_str(&content) {
        Ok(v) => v,
        Err(_) => return (false, None),
    };

    let template_defaults = value
        .get("pack")
        .and_then(|p| p.get("use_defaults"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let runtime = value
        .get("pack")
        .and_then(|p| p.get("runtime"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    (template_defaults, runtime)
}

/// List all available policy profiles
#[verb]
pub fn list(verbose: bool) -> VerbResult<ListOutput> {
    let profiles = predefined_profiles();

    if verbose {
        log::info!("Available Policy Profiles:");
        for profile in &profiles {
            log::info!("  - {} ({})", profile.id.as_str(), profile.name);
            log::info!("    Description: {}", profile.description);
            log::info!("    Policies: {}", profile.policy_overlays.len());
            log::info!("    Trust Tier: {:?}", profile.trust_requirements);
            log::info!("    Receipt Spec: {:?}", profile.receipt_requirements);
        }
    }

    let profiles_summary: Vec<ProfileSummary> = profiles
        .iter()
        .map(|p| ProfileSummary {
            id: p.id.as_str().to_string(),
            name: p.name.clone(),
            description: p.description.clone(),
            policy_count: p.policy_overlays.len(),
            trust_requirement: format!("{:?}", p.trust_requirements),
            receipt_requirement: format!("{:?}", p.receipt_requirements),
        })
        .collect();

    Ok(ListOutput {
        profiles: profiles_summary,
        total: profiles.len(),
    })
}

/// Validate current project against a policy profile
#[verb]
pub fn validate(profile: String) -> VerbResult<ValidateOutput> {
    // Get the profile
    let profile_obj = ggen_marketplace::marketplace::profile::get_profile(&profile).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Profile not found: {}", e))
    })?;

    // Load pack contexts from project
    let pack_contexts = load_pack_contexts_from_project()
        .map_err(|e| clap_noun_verb::NounVerbError::argument_error(format!("{}", e)))?;

    // Enforce policy
    let report = profile_obj.enforce(&pack_contexts).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Policy enforcement failed: {}", e))
    })?;

    // Format violations
    let violations: Vec<ViolationSummary> = report
        .violations
        .iter()
        .map(|v| ViolationSummary {
            policy_id: v.policy_id.as_str().to_string(),
            pack_id: v.pack_id.clone(),
            description: v.description.clone(),
        })
        .collect();

    if report.passed {
        log::info!("✓ Profile '{}' validation passed", profile);
    } else {
        log::error!("✗ Profile '{}' validation failed", profile);
        log::error!("  Violations: {}", report.violation_count());
        for violation in &violations {
            log::error!("    - {}: {}", violation.pack_id, violation.description);
        }
    }

    Ok(ValidateOutput {
        profile_id: profile,
        passed: report.passed,
        violation_count: report.violation_count(),
        policies_checked: report.policies_checked.len(),
        violations,
    })
}

/// Show detailed profile information
#[verb]
pub fn show(profile_id: String) -> VerbResult<ShowOutput> {
    let profile = ggen_marketplace::marketplace::profile::get_profile(&profile_id).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Profile not found: {}", e))
    })?;

    log::info!("Profile: {} ({})", profile.id.as_str(), profile.name);
    log::info!("Description: {}", profile.description);
    log::info!("Policies ({}):", profile.policy_overlays.len());
    for policy in &profile.policy_overlays {
        log::info!("  - {} ({})", policy.id.as_str(), policy.name);
        log::info!("    {}", policy.description);
        log::info!("    Rules: {}", policy.rules.len());
    }
    log::info!("Trust Tier: {:?}", profile.trust_requirements);
    log::info!("Receipt Spec: {:?}", profile.receipt_requirements);
    log::info!(
        "Runtime Constraints ({}):",
        profile.runtime_constraints.len()
    );
    for (idx, constraint) in profile.runtime_constraints.iter().enumerate() {
        log::info!("  Constraint {}:", idx + 1);
        log::info!("    Allowed Runtimes: {:?}", constraint.allowed_runtimes);
        log::info!("    Forbid Defaults: {}", constraint.forbid_defaults);
        log::info!("    Require Explicit: {}", constraint.require_explicit);
    }

    let policies: Vec<PolicySummary> = profile
        .policy_overlays
        .iter()
        .map(|p| PolicySummary {
            id: p.id.as_str().to_string(),
            name: p.name.clone(),
            description: p.description.clone(),
            rule_count: p.rules.len(),
        })
        .collect();

    let runtime_constraints: Vec<RuntimeConstraintSummary> = profile
        .runtime_constraints
        .iter()
        .map(|c| RuntimeConstraintSummary {
            allowed_runtimes: c.allowed_runtimes.clone(),
            forbid_defaults: c.forbid_defaults,
            require_explicit: c.require_explicit,
        })
        .collect();

    Ok(ShowOutput {
        profile_id: profile.id.as_str().to_string(),
        name: profile.name,
        description: profile.description,
        policies,
        trust_requirement: format!("{:?}", profile.trust_requirements),
        receipt_requirement: format!("{:?}", profile.receipt_requirements),
        runtime_constraints,
    })
}

/// Check current environment against default profile
#[verb]
pub fn check() -> VerbResult<ValidateOutput> {
    // Use Production as the default profile
    let profile_obj =
        ggen_marketplace::marketplace::profile::get_profile("enterprise-strict").map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!(
                "Default profile not found: {}",
                e
            ))
        })?;

    // Load pack contexts from project
    let pack_contexts = load_pack_contexts_from_project()
        .map_err(|e| clap_noun_verb::NounVerbError::argument_error(format!("{}", e)))?;

    // Enforce policy
    let report = profile_obj.enforce(&pack_contexts).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Policy enforcement failed: {}", e))
    })?;

    // Format violations
    let violations: Vec<ViolationSummary> = report
        .violations
        .iter()
        .map(|v| ViolationSummary {
            policy_id: v.policy_id.as_str().to_string(),
            pack_id: v.pack_id.clone(),
            description: v.description.clone(),
        })
        .collect();

    if report.passed {
        log::info!(
            "✓ Current environment passes '{}' profile",
            profile_obj.name
        );
    } else {
        log::error!("✗ Current environment fails '{}' profile", profile_obj.name);
        log::error!("  Violations: {}", report.violation_count());
        for violation in &violations {
            log::error!("    - {}: {}", violation.pack_id, violation.description);
        }
    }

    Ok(ValidateOutput {
        profile_id: profile_obj.id.as_str().to_string(),
        passed: report.passed,
        violation_count: report.violation_count(),
        policies_checked: report.policies_checked.len(),
        violations,
    })
}
