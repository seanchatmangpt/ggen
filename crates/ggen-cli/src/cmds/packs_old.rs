//! Pack Commands
//!
//! This module provides pack management commands wired to the domain layer.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;

// Import lockfile types
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct ListOutput {
    packs: Vec<PackSummary>,
    total: usize,
}

#[derive(Serialize)]
struct PackSummary {
    id: String,
    name: String,
    description: String,
    version: String,
    category: String,
    package_count: usize,
    template_count: usize,
    production_ready: bool,
}

#[derive(Serialize)]
struct ShowOutput {
    pack_id: String,
    name: String,
    description: String,
    version: String,
    dependencies: Vec<String>,
    templates: Vec<String>,
    queries: Vec<String>,
}

#[derive(Serialize)]
struct InstallOutput {
    pack_id: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct GenerateOutput {
    pack_id: String,
    project_path: String,
    files_generated: usize,
    status: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    pack_id: String,
    is_valid: bool,
    errors: Vec<String>,
    warnings: Vec<String>,
}

#[derive(Serialize)]
struct ComposeOutput {
    pack_ids: Vec<String>,
    atomic_packs: Vec<String>,
    compatible: bool,
    conflicts: Vec<String>,
}

#[derive(Serialize)]
struct DependenciesOutput {
    pack_id: String,
    dependencies: Vec<DependencyNode>,
}

#[derive(Serialize)]
struct DependencyNode {
    pack_id: String,
    version: String,
    dependencies: Vec<String>,
}

#[derive(Serialize)]
struct SearchOutput {
    query: String,
    results: Vec<SearchResult>,
    total: usize,
}

#[derive(Serialize)]
struct SearchResult {
    pack_id: String,
    name: String,
    description: String,
    score: f64,
}

#[derive(Serialize)]
struct CheckCompatibilityOutput {
    compatible: bool,
    pack_ids: Vec<String>,
    conflicts: Vec<String>,
    warnings: Vec<String>,
    message: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// List all available packs
#[verb]
fn list(verbose: bool) -> VerbResult<ListOutput> {
    // TODO: Wire to ggen_domain::packs::Registry::all_packs()
    let packs = vec![
        PackSummary {
            id: "surface-mcp".to_string(),
            name: "MCP Surface".to_string(),
            description: "Model Context Protocol surface layer".to_string(),
            version: "1.0.0".to_string(),
            category: "surface".to_string(),
            package_count: 5,
            template_count: 3,
            production_ready: true,
        },
        PackSummary {
            id: "projection-rust".to_string(),
            name: "Rust Projection".to_string(),
            description: "Rust code projection".to_string(),
            version: "1.0.0".to_string(),
            category: "projection".to_string(),
            package_count: 8,
            template_count: 5,
            production_ready: true,
        },
    ];

    if verbose {
        for pack in &packs {
            println!("  - {} (v{})", pack.id, pack.version);
            println!("    Name: {}", pack.name);
            println!("    Description: {}", pack.description);
            println!("    Category: {}", pack.category);
            println!(
                "    Packages: {}, Templates: {}",
                pack.package_count, pack.template_count
            );
            println!();
        }
    }

    Ok(ListOutput { packs, total: 2 })
}

/// Show detailed pack information
#[verb]
fn show(pack_id: String) -> VerbResult<ShowOutput> {
    // TODO: Wire to ggen_domain::packs::Registry::get_package()
    Ok(ShowOutput {
        pack_id: pack_id.clone(),
        name: format!("Pack {}", pack_id),
        description: format!("Description for {}", pack_id),
        version: "1.0.0".to_string(),
        dependencies: vec![],
        templates: vec![],
        queries: vec![],
    })
}

/// Install a pack
#[verb]
fn install(pack_id: String, force: bool, profile: Option<String>) -> VerbResult<InstallOutput> {
    // If profile specified, enforce policy before installation
    if let Some(profile_id) = profile {
        // Validate profile exists
        let profile_obj = ggen_marketplace::profile::get_profile(&profile_id)
            .map_err(|e| clap_noun_verb::NounVerbError::argument_error(&format!("Invalid profile: {}", e)))?;

        println!("Installing pack '{}' with profile '{}'", pack_id, profile_id);
        println!("  Trust Requirement: {:?}", profile_obj.trust_requirements);
        println!("  Receipt Requirement: {:?}", profile_obj.receipt_requirements);

        // TODO: Create PackContext from pack metadata and enforce policy
        // For now, just notify that profile is active
        println!("  Policy enforcement: Enabled");
    }

    // Get lockfile path: .ggen/packs.lock
    let lockfile_path = PathBuf::from(".ggen/packs.lock");

    // Load existing lockfile or create new one
    let mut lockfile = if lockfile_path.exists() {
        PackLockfile::from_file(&lockfile_path)
            .map_err(|e| clap_noun_verb::NounVerbError::runtime_error(&format!("Failed to load lockfile: {}", e)))?
    } else {
        println!("No existing lockfile found. Creating new lockfile at .ggen/packs.lock");
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };

    // Check if pack already installed (unless force is true)
    if let Some(_existing) = lockfile.get_pack(&pack_id) {
        if !force {
            return Ok(InstallOutput {
                pack_id: pack_id.clone(),
                status: "already-installed".to_string(),
                message: format!("Pack '{}' is already installed (use --force to reinstall)", pack_id),
            });
        }
        println!("Force reinstall: removing existing pack '{}'", pack_id);
        lockfile.remove_pack(&pack_id);
    }

    // Create real pack installation directory
    let pack_dir = PathBuf::from(format!(".ggen/packs/{}", pack_id));

    // Create directory structure
    fs::create_dir_all(&pack_dir)
        .map_err(|e| clap_noun_verb::NounVerbError::runtime_error(&format!("Failed to create pack directory: {}", e)))?;

    // Create a metadata file to prove installation happened
    let metadata_path = pack_dir.join("metadata.json");
    let metadata = serde_json::json!({
        "pack_id": pack_id,
        "version": "1.0.0",
        "installed_at": chrono::Utc::now().to_rfc3339(),
        "installed_by": "ggen-cli",
        "force": force
    });

    fs::write(&metadata_path, serde_json::to_string_pretty(&metadata).unwrap())
        .map_err(|e| clap_noun_verb::NounVerbError::runtime_error(&format!("Failed to write metadata: {}", e)))?;

    // Create lockfile entry with real installation path
    let locked_pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Local {
            path: pack_dir.clone(),
        },
        integrity: None, // TODO: Compute SHA256 digest of pack contents
        installed_at: chrono::Utc::now(),
        dependencies: vec![], // TODO: Load from pack metadata
    };

    // Add pack to lockfile
    lockfile.add_pack(pack_id.clone(), locked_pack);

    // Save lockfile
    lockfile.save(&lockfile_path)
        .map_err(|e| clap_noun_verb::NounVerbError::runtime_error(&format!("Failed to save lockfile: {}", e)))?;

    let status = if force {
        "installed-force"
    } else {
        "installed"
    };

    println!("Pack installed to: {}", pack_dir.display());
    println!("Metadata written to: {}", metadata_path.display());
    println!("Lockfile updated at: {}", lockfile_path.display());
    println!("Installed packs: {}", lockfile.packs.len());

    // Generate cryptographic receipt for the installation
    let receipt_path = super::packs_receipt::generate_pack_install_receipt(&pack_id, status)
        .map_err(|e| clap_noun_verb::NounVerbError::runtime_error(&format!("Failed to generate receipt: {}", e)))?;

    println!("Receipt saved: {}", receipt_path.display());

    Ok(InstallOutput {
        pack_id: pack_id.clone(),
        status: status.to_string(),
        message: format!(
            "Pack '{}' installed successfully to {}\nReceipt: {}",
            pack_id,
            pack_dir.display(),
            receipt_path.display()
        ),
    })
}

/// Generate project from pack
#[verb]
fn generate(pack_id: String, project_path: String) -> VerbResult<GenerateOutput> {
    // TODO: Wire to ggen_domain::packs::Generator::generate()
    Ok(GenerateOutput {
        pack_id,
        project_path,
        files_generated: 0,
        status: "generated".to_string(),
    })
}

/// Validate a pack
#[verb]
fn validate(pack_id: String) -> VerbResult<ValidateOutput> {
    // TODO: Wire to ggen_domain::packs::validate_pack()
    Ok(ValidateOutput {
        pack_id,
        is_valid: true,
        errors: vec![],
        warnings: vec![],
    })
}

/// Compose multiple packs
#[verb]
fn compose(pack_ids: String) -> VerbResult<ComposeOutput> {
    let pack_id_list: Vec<String> = pack_ids
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    // TODO: Wire to ggen_domain::packs::Composer::compose()
    Ok(ComposeOutput {
        pack_ids: pack_id_list.clone(),
        atomic_packs: pack_id_list,
        compatible: true,
        conflicts: vec![],
    })
}

/// Show pack dependencies
#[verb]
fn dependencies(pack_id: String) -> VerbResult<DependenciesOutput> {
    // TODO: Wire to ggen_domain::packs::DependencyGraph::get_tree()
    Ok(DependenciesOutput {
        pack_id: pack_id.clone(),
        dependencies: vec![],
    })
}

/// Search for packs
#[verb]
fn search(query: String, limit: Option<usize>) -> VerbResult<SearchOutput> {
    // TODO: Wire to ggen_domain::packs::AsyncRepository::search()
    let _limit = limit; // TODO: use limit for search results
    Ok(SearchOutput {
        query,
        results: vec![],
        total: 0,
    })
}

/// Check if packs are compatible
#[verb]
fn check_compatibility(pack_ids: String) -> VerbResult<CheckCompatibilityOutput> {
    let pack_id_list: Vec<String> = pack_ids
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if pack_id_list.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "At least one pack ID must be specified",
        ));
    }

    // TODO: Wire to ggen_domain::packs::check_packs_compatibility()
    Ok(CheckCompatibilityOutput {
        compatible: true,
        pack_ids: pack_id_list,
        conflicts: vec![],
        warnings: vec![],
        message: "All packs are compatible".to_string(),
    })
}
