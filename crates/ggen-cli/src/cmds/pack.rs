//! Pack Commands (singular alias for `packs`)
//!
//! This module provides the `ggen pack` noun as an alias for `ggen packs`,
//! supporting the golden-path form: `ggen pack add <name>`.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use ggen_core::domain::packs::install::{install_pack, InstallInput};
use ggen_core::domain::packs::metadata::{list_packs, load_pack_metadata, show_pack};
use ggen_core::packs::lockfile::PackLockfile;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
pub struct AddOutput {
    pub pack_name: String,
    pub status: String,
    pub message: String,
}

#[derive(Serialize)]
pub struct RemoveOutput {
    pub pack_name: String,
    pub status: String,
    pub message: String,
}

#[derive(Serialize)]
pub struct ListOutput {
    pub packs: Vec<PackSummary>,
    pub total: usize,
}

#[derive(Serialize)]
pub struct PackSummary {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub category: String,
    pub package_count: usize,
    pub template_count: usize,
    pub production_ready: bool,
    pub registry_type: String,
}

#[derive(Serialize)]
pub struct ShowOutput {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub category: String,
    pub package_count: usize,
    pub packages: Vec<String>,
    pub dependencies: Vec<String>,
    pub registry_type: String,
}

#[derive(Serialize)]
pub struct SearchOutput {
    pub query: String,
    pub results: Vec<SearchResult>,
    pub total: usize,
}

#[derive(Serialize)]
pub struct SearchResult {
    pub pack_id: String,
    pub name: String,
    pub description: String,
    pub score: f64,
    pub registry_type: String,
}

#[derive(Serialize)]
pub struct InstallOutput {
    pub pack_id: String,
    pub pack_name: String,
    pub status: String,
    pub message: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Add (install) a pack by name
#[verb]
pub fn add(pack_name: String, force: Option<bool>) -> Result<AddOutput> {
    // Verify the pack exists before attempting installation
    if let Err(e) = load_pack_metadata(&pack_name) {
        return Ok(AddOutput {
            pack_name: pack_name.clone(),
            status: "not_found".to_string(),
            message: format!(
                "Pack '{}' not found in local registry: {}. \
                 Ensure marketplace/packs/{}.toml exists.",
                pack_name, e, pack_name
            ),
        });
    }

    // Run the real installation via the domain layer
    let input = InstallInput {
        pack_id: pack_name.clone(),
        target_dir: None,
        force: force.unwrap_or(false),
        dry_run: false,
    };

    let install_result = crate::runtime::block_on(install_pack(&input)).map_err(|e| {
        NounVerbError::execution_error(format!("Failed to install pack '{}': {}", pack_name, e))
    })?;
    let output = install_result.map_err(|e| {
        NounVerbError::execution_error(format!("Failed to install pack '{}': {}", pack_name, e))
    })?;

    // The install only reaches here on success. Emit a provenance receipt that
    // binds the real pack closure (id+version+digest + packages) and the durable
    // artifacts (install dir + lockfile). Emission is GATED on a non-empty
    // digest — a failed install never reaches this point and never gets a
    // receipt (no fail-open). A receipt failure is surfaced loudly, never
    // swallowed.
    let mut artifact_paths = vec![output.install_path.clone()];
    if let Some(lock) = &output.lockfile_path {
        artifact_paths.push(lock.clone());
    }
    let closure = crate::cmds::packs_receipt::PackInstallClosure {
        pack_id: &output.pack_id,
        pack_version: &output.pack_version,
        pack_digest: &output.digest,
        packages_installed: &output.packages_installed,
        artifact_paths: &artifact_paths,
    };
    let receipt_path = crate::cmds::packs_receipt::generate_pack_install_receipt(&closure)
        .map_err(|e| {
            NounVerbError::execution_error(format!(
                "Pack '{}' installed but receipt emission failed: {}",
                pack_name, e
            ))
        })?;

    Ok(AddOutput {
        pack_name: output.pack_id.clone(),
        status: "installed".to_string(),
        message: format!(
            "Pack '{}' ({}) installed successfully. {} package(s) recorded, {} template(s) available. Lockfile: .ggen/packs.lock. Receipt: {}",
            output.pack_name,
            output.pack_id,
            output.packages_installed.len(),
            output.templates_available.len(),
            receipt_path.display()
        ),
    })
}

/// Remove an installed pack
#[verb]
pub fn remove(pack_name: String) -> Result<RemoveOutput> {
    validate_pack_name(&pack_name)?;

    // Step 1: Resolve lock_path
    let lock_path = std::env::current_dir()
        .map_err(|e| {
            NounVerbError::execution_error(format!("Cannot resolve project directory: {}", e))
        })?
        .join(".ggen")
        .join("packs.lock");

    // Step 2: Check if lockfile exists
    if !lock_path.exists() {
        return Err(NounVerbError::execution_error(
            "No packs installed: .ggen/packs.lock not found",
        ));
    }

    // Step 3: Load lockfile
    let mut lockfile = PackLockfile::from_file(&lock_path)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to load lockfile: {}", e)))?;

    // Step 4: Check if pack exists in lockfile
    if lockfile.get_pack(&pack_name).is_none() {
        return Err(NounVerbError::execution_error(format!(
            "Pack '{}' is not installed",
            pack_name
        )));
    }

    // Step 5: Compute pack_dir and remove if exists
    let pack_dir = resolve_cache_dir()?.join(&pack_name);

    if pack_dir.exists() {
        std::fs::remove_dir_all(&pack_dir).map_err(|e| {
            NounVerbError::execution_error(format!("Failed to remove pack directory: {}", e))
        })?;
    }

    // Step 6: Remove from lockfile
    lockfile.remove_pack(&pack_name);

    // Step 7: Save lockfile
    lockfile.save(&lock_path).map_err(|e| {
        NounVerbError::execution_error(format!(
            "Failed to save lockfile (partial removal may have occurred): {}",
            e
        ))
    })?;

    Ok(RemoveOutput {
        pack_name: pack_name.clone(),
        status: "removed".to_string(),
        message: format!(
            "Pack '{}' removed successfully. \
             Run `ggen pack list` to see remaining installed packs.",
            pack_name
        ),
    })
}

/// List all available packs
#[verb]
pub fn list(verbose: Option<bool>, category: Option<String>) -> Result<ListOutput> {
    let packages = list_packs(None)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to list packs: {}", e)))?;

    let is_verbose = verbose.unwrap_or(false);
    let filtered_packages: Vec<_> = if let Some(cat) = category.as_ref() {
        packages
            .into_iter()
            .filter(|pkg| &pkg.category == cat)
            .collect()
    } else {
        packages
    };

    let total = filtered_packages.len();
    let default_category = category.unwrap_or_else(|| "marketplace".to_string());

    let packs: Vec<PackSummary> = filtered_packages
        .into_iter()
        .map(|pkg| {
            if is_verbose {
                log::debug!("  - {} (v{})", pkg.id, pkg.version);
            }

            PackSummary {
                id: pkg.id,
                name: pkg.name,
                description: pkg.description,
                version: pkg.version,
                category: default_category.clone(),
                package_count: 0,
                template_count: 0,
                production_ready: pkg.production_ready,
                registry_type: pkg.registry_type.unwrap_or_else(|| "local".to_string()),
            }
        })
        .collect();

    Ok(ListOutput { packs, total })
}

/// Show detailed pack information
#[verb]
pub fn show(pack_id: String) -> Result<ShowOutput> {
    let detail = show_pack(&pack_id).map_err(|e| {
        NounVerbError::execution_error(format!("Failed to get pack '{}': {}", pack_id, e))
    })?;

    let dependencies: Vec<String> = detail
        .dependencies
        .iter()
        .map(|d| format!("{} {}", d.pack_id, d.version))
        .collect();

    let package_count = detail.packages.len();
    let packages: Vec<String> = detail.packages.iter().map(|p| p.to_string()).collect();

    Ok(ShowOutput {
        id: detail.id,
        name: detail.name,
        description: detail.description,
        version: detail.version,
        category: "marketplace".to_string(),
        package_count,
        packages,
        dependencies,
        registry_type: detail.registry_type.unwrap_or_else(|| "local".to_string()),
    })
}

/// Search for packs
#[verb]
pub fn search(query: String, limit: Option<usize>) -> Result<SearchOutput> {
    let results = perform_search(&query, limit)?;
    let total = results.len();
    log::info!("Found {} result(s) for '{}'", total, query);

    Ok(SearchOutput {
        query,
        results,
        total,
    })
}

/// Run health check on installed packs and lockfile
#[verb]
pub fn doctor() -> Result<serde_json::Value> {
    use ggen_core::domain::utils::{execute_doctor, DoctorInput};

    let result = crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: true,
        check: Some("cache".to_string()),
        env: false,
    }))
    .map_err(|e| NounVerbError::execution_error(format!("Runtime error: {}", e)))?
    .map_err(|e| NounVerbError::execution_error(format!("Doctor execution failed: {}", e)))?;

    Ok(serde_json::to_value(result).unwrap_or(serde_json::Value::Null))
}

// ============================================================================
// Helper Functions
// ============================================================================

fn perform_search(query: &str, limit: Option<usize>) -> Result<Vec<SearchResult>> {
    let packages = list_packs(None)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to list packages: {}", e)))?;

    let query_lower = query.to_lowercase();
    let max = limit.unwrap_or(20);

    let mut scored: Vec<SearchResult> = packages
        .into_iter()
        .filter_map(|p| {
            let relevance = calculate_relevance(&p.name, &p.description, &p.id, &query_lower)?;
            Some(SearchResult {
                pack_id: p.id,
                name: p.name,
                description: p.description,
                score: relevance,
                registry_type: p.registry_type.unwrap_or_else(|| "local".to_string()),
            })
        })
        .collect();

    scored.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    scored.truncate(max);
    Ok(scored)
}

fn calculate_relevance(name: &str, desc: &str, id: &str, query: &str) -> Option<f64> {
    if name.to_lowercase().contains(query) {
        Some(1.0)
    } else if id.to_lowercase().contains(query) {
        Some(0.8)
    } else if desc.to_lowercase().contains(query) {
        Some(0.5)
    } else {
        None
    }
}

fn validate_pack_name(pack_name: &str) -> Result<()> {
    if pack_name.trim().is_empty() {
        return Err(NounVerbError::argument_error("Pack name must not be empty"));
    }
    let valid = pack_name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.');
    if !valid {
        return Err(NounVerbError::argument_error(
            "Pack name contains invalid characters. Use alphanumeric, hyphens, underscores only.",
        ));
    }
    Ok(())
}

fn resolve_cache_dir() -> Result<PathBuf> {
    std::env::var_os("GGEN_PACK_CACHE_DIR")
        .map(PathBuf::from)
        .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
        .ok_or_else(|| {
            NounVerbError::execution_error(
                "Cannot resolve pack cache: set HOME or GGEN_PACK_CACHE_DIR",
            )
        })
}
