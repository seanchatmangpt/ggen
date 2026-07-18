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

/// Outcome of [`load_pack_contexts_from_project`]: the packs that parsed
/// cleanly, plus any lockfile entries whose key is not a well-formed
/// `PackageId`.
///
/// `malformed` entries are `(pack_id, validation_error)` pairs. They are
/// deliberately *not* folded into a hard `Err` from the loader itself: one
/// legacy/corrupted lockfile entry (written by a write path that predates or
/// bypasses `PackageId` validation -- see `crates/ggen-cli/src/cmds/packs.rs`
/// and `crates/ggen-cli/src/cmds/capability.rs`) must not block every other
/// pack in the project from reaching a real policy verdict. Callers
/// (`check`/`validate`) still surface `malformed` as a typed refusal -- see
/// `run_policy_enforcement`.
struct LoadedPackContexts {
    contexts: Vec<PackContext>,
    malformed: Vec<(String, String)>,
}

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
///
/// A lockfile entry whose *key* fails `PackageId::new` (malformed pack
/// identifier) is not a load error: it is skipped from policy evaluation and
/// reported back via [`LoadedPackContexts::malformed`] instead, so the rest
/// of the project's packs still reach the policy engine.
fn load_pack_contexts_from_project() -> crate::Result<LoadedPackContexts> {
    use ggen_marketplace::marketplace::metadata::{get_pack_cache_dir, load_pack_metadata};
    use ggen_marketplace::marketplace::models::PackageId;
    use ggen_marketplace::packs::lockfile::PackLockfile;
    use std::path::Path;

    let lockfile_path = Path::new(".ggen/packs.lock");
    if !lockfile_path.exists() {
        return Err(crate::utils::error::Error::new(
            "No project found. Please install packs first with 'ggen packs install <pack-id>'",
        ));
    }

    let lockfile = PackLockfile::from_file(lockfile_path)
        .map_err(|e| crate::utils::error::Error::new(&format!("Failed to load lockfile: {}", e)))?;

    let mut pack_contexts = Vec::new();
    let mut malformed = Vec::new();
    for (pack_id, locked_pack) in &lockfile.packs {
        let package_id = match PackageId::new(pack_id) {
            Ok(id) => id,
            Err(e) => {
                // Pre-existing lockfile DATA that was never validated when
                // written (see the write-path fixes referenced above). Skip
                // it rather than aborting the whole command -- BUG-001's
                // failure mode was exactly this: check/validate crashed on
                // one bad identifier before compliance logic ran at all.
                log::warn!(
                    "Skipping malformed pack identifier '{}' from .ggen/packs.lock \
                     (excluded from policy evaluation): {}",
                    pack_id,
                    e
                );
                malformed.push((pack_id.clone(), e.to_string()));
                continue;
            }
        };

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

    Ok(LoadedPackContexts {
        contexts: pack_contexts,
        malformed,
    })
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

/// Resolve `profile_id`, load the project's pack contexts (tolerating -- but
/// surfacing -- any malformed lockfile identifiers), and run the real policy
/// engine (`Profile::enforce`) against the valid subset.
///
/// Behavior (Blocker B / BUG-001 fix):
/// - The policy engine always runs over whatever valid packs the lockfile
///   contains, even when other entries are malformed -- so a legacy/corrupted
///   identifier no longer prevents the rest of the project from reaching a
///   real pass/fail compliance verdict.
/// - Clean input with zero violations => `Ok` (process exit 0).
/// - Malformed lockfile identifiers => typed `ArgumentError` (bad
///   configuration data), not a crash.
/// - Real policy violations on otherwise-valid packs => typed
///   `ExecutionError` (a compliance refusal), not a silent exit-0 pass.
fn run_policy_enforcement(profile_id: String) -> VerbResult<ValidateOutput> {
    let profile_obj =
        ggen_marketplace::marketplace::profile::get_profile(&profile_id).map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!("Profile not found: {}", e))
        })?;

    // Load pack contexts from project (never aborts on a malformed identifier
    // alone -- see `load_pack_contexts_from_project`).
    let loaded = load_pack_contexts_from_project()
        .map_err(|e| clap_noun_verb::NounVerbError::argument_error(format!("{}", e)))?;

    // Enforce policy over the valid subset -- this is "reaching the policy
    // engine" even when malformed entries were skipped above.
    let report = profile_obj.enforce(&loaded.contexts).map_err(|e| {
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

    let mut problems: Vec<String> = Vec::new();

    if !loaded.malformed.is_empty() {
        log::error!(
            "✗ .ggen/packs.lock contains {} malformed pack identifier(s), excluded from policy evaluation:",
            loaded.malformed.len()
        );
        for (id, err) in &loaded.malformed {
            log::error!("    - {}: {}", id, err);
        }
        problems.push(format!(
            "{} malformed pack identifier(s) in .ggen/packs.lock were excluded from policy \
             evaluation: {}",
            loaded.malformed.len(),
            loaded
                .malformed
                .iter()
                .map(|(id, err)| format!("{} ({})", id, err))
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    if report.passed {
        log::info!("✓ Profile '{}' validation passed", profile_id);
    } else {
        log::error!("✗ Profile '{}' validation failed", profile_id);
        log::error!("  Violations: {}", report.violation_count());
        for violation in &violations {
            log::error!("    - {}: {}", violation.pack_id, violation.description);
        }
        problems.push(format!(
            "{} policy violation(s) against profile '{}': {}",
            report.violation_count(),
            profile_id,
            violations
                .iter()
                .map(|v| format!("{}: {}", v.pack_id, v.description))
                .collect::<Vec<_>>()
                .join("; ")
        ));
    }

    if !problems.is_empty() {
        let message = problems.join(" | ");
        // A genuine policy violation is an execution-time compliance refusal;
        // a purely-malformed lockfile with no real violations is bad input
        // data. Either way: typed, non-panicking, nonzero.
        return Err(if !report.passed {
            clap_noun_verb::NounVerbError::execution_error(message)
        } else {
            clap_noun_verb::NounVerbError::argument_error(message)
        });
    }

    Ok(ValidateOutput {
        profile_id,
        passed: true,
        violation_count: 0,
        policies_checked: report.policies_checked.len(),
        violations,
    })
}

/// Validate current project against a policy profile
#[verb]
pub fn validate(profile: String) -> VerbResult<ValidateOutput> {
    run_policy_enforcement(profile)
}

/// Show detailed profile information
#[verb]
pub fn show(profile_id: String) -> VerbResult<ShowOutput> {
    let profile =
        ggen_marketplace::marketplace::profile::get_profile(&profile_id).map_err(|e| {
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
    // Use enterprise-strict as the default/hardcoded profile.
    run_policy_enforcement("enterprise-strict".to_string())
}
