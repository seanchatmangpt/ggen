//! Pack CLI verbs — thin wrappers over domain logic.
//!
//! All pack operations delegate to `ggen_core::domain::packs::*`.

#![allow(clippy::unused_unit)]

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

use crate::runtime::block_on;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct PackAddOutput {
    pack_id: String,
    pack_name: String,
    status: String,
    message: String,
    digest: Option<String>,
    templates_available: Vec<String>,
    packages_installed: Vec<String>,
    install_path: String,
}

#[derive(Serialize)]
struct PackRemoveOutput {
    pack_id: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct PackListOutput {
    packs: Vec<PackListEntry>,
    total: usize,
}

#[derive(Serialize)]
struct PackListEntry {
    id: String,
    version: String,
    installed_at: String,
    digest: Option<String>,
    trust_tier: String,
    install_path: String,
}

#[derive(Serialize)]
struct PackShowOutput {
    id: String,
    version: String,
    installed_at: String,
    digest: Option<String>,
    registry_source: String,
    trust_tier: String,
    dependencies: Vec<String>,
    install_path: String,
    files: Vec<String>,
}

#[derive(Serialize)]
struct PackVerifyOutput {
    pack_id: String,
    valid: bool,
    message: String,
}

#[derive(Serialize)]
struct PackGraphOutput {
    nodes: Vec<GraphEntry>,
    edges: Vec<EdgeEntry>,
}

#[derive(Serialize)]
struct GraphEntry {
    id: String,
    label: String,
}

#[derive(Serialize)]
struct EdgeEntry {
    from: String,
    to: String,
}

#[derive(Serialize)]
struct PackUpdateOutput {
    pack_id: String,
    status: String,
    message: String,
    digest: Option<String>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Add a pack to the workspace
#[verb]
fn add(pack_name: String) -> VerbResult<PackAddOutput> {
    let input = ggen_core::domain::packs::install::InstallInput {
        pack_id: pack_name.clone(),
        target_dir: None,
        force: false,
        dry_run: false,
    };

    let result = block_on(ggen_core::domain::packs::install::install_pack(&input))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    println!("  Pack '{}' added", result.pack_id);
    if let Some(ref digest) = result.digest {
        println!("  Digest: {}", digest);
    }
    println!("  Install path: {}", result.install_path.display());

    Ok(PackAddOutput {
        pack_id: result.pack_id,
        pack_name: result.pack_name,
        status: "installed".to_string(),
        message: format!(
            "Installed {} ({} packages, {} templates, {} sparql queries)",
            result.pack_id,
            result.total_packages,
            result.templates_available.len(),
            result.sparql_queries,
        ),
        digest: result.digest,
        templates_available: result.templates_available,
        packages_installed: result.packages_installed,
        install_path: result.install_path.display().to_string(),
    })
}

/// Remove a pack from the workspace
#[verb]
fn remove(pack_name: String) -> VerbResult<PackRemoveOutput> {
    let lockfile_path = std::path::PathBuf::from(".ggen/packs.lock");

    let removed = ggen_core::domain::packs::lockfile::remove_pack(&lockfile_path, &pack_name)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    if removed {
        println!("  Pack '{}' removed", pack_name);
        Ok(PackRemoveOutput {
            pack_id: pack_name,
            status: "removed".to_string(),
            message: format!("Pack '{}' removed from lockfile", pack_name),
        })
    } else {
        Ok(PackRemoveOutput {
            pack_id: pack_name,
            status: "not-found".to_string(),
            message: format!("Pack '{}' not found in lockfile", pack_name),
        })
    }
}

/// List all installed packs
#[verb]
fn list() -> VerbResult<PackListOutput> {
    let lockfile_path = std::path::PathBuf::from(".ggen/packs.lock");

    let packs = ggen_core::domain::packs::lockfile::list_packs(&lockfile_path)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    for pack in &packs {
        println!("  {} @ {} ({})", pack.id, pack.version, pack.trust_tier);
    }

    let entries: Vec<PackListEntry> = packs
        .iter()
        .map(|p| PackListEntry {
            id: p.id.clone(),
            version: p.version.clone(),
            installed_at: p.installed_at.to_rfc3339(),
            digest: p.digest.as_ref().map(|d| d.prefixed()),
            trust_tier: p.trust_tier.clone(),
            install_path: p.install_path.clone(),
        })
        .collect();

    Ok(PackListOutput {
        total: entries.len(),
        packs: entries,
    })
}

/// Show detailed information about an installed pack
#[verb]
fn show(pack_id: String) -> VerbResult<PackShowOutput> {
    let lockfile_path = std::path::PathBuf::from(".ggen/packs.lock");

    let entry = ggen_core::domain::packs::lockfile::get_pack(&lockfile_path, &pack_id)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
        .ok_or_else(|| {
            clap_noun_verb::NounVerbError::execution_error(format!("Pack '{}' not found", pack_id))
        })?;

    println!("  Pack: {}", entry.id);
    println!("  Version: {}", entry.version);
    println!("  Installed: {}", entry.installed_at);
    if let Some(ref digest) = entry.digest {
        println!("  Digest: {}", digest.prefixed());
    }
    println!("  Source: {}", entry.registry_source);
    println!("  Trust: {}", entry.trust_tier);
    if !entry.dependencies.is_empty() {
        println!("  Dependencies: {}", entry.dependencies.join(", "));
    }
    if !entry.files.is_empty() {
        println!("  Files: {}", entry.files.join(", "));
    }

    Ok(PackShowOutput {
        id: entry.id,
        version: entry.version,
        installed_at: entry.installed_at.to_rfc3339(),
        digest: entry.digest.as_ref().map(|d| d.prefixed()),
        registry_source: entry.registry_source,
        trust_tier: entry.trust_tier,
        dependencies: entry.dependencies,
        install_path: entry.install_path,
        files: entry.files,
    })
}

/// Verify the integrity of an installed pack
#[verb]
fn verify(pack_id: String) -> VerbResult<PackVerifyOutput> {
    let lockfile_path = std::path::PathBuf::from(".ggen/packs.lock");
    let packs_dir = ggen_core::domain::packs::metadata::get_packs_dir()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let valid = ggen_core::domain::packs::lockfile::verify_pack_integrity(
        &lockfile_path,
        &pack_id,
        &packs_dir,
    )
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let message = if valid {
        format!("Pack '{}' integrity verified", pack_id)
    } else {
        format!("Pack '{}' integrity check FAILED", pack_id)
    };

    println!("  {}", message);

    Ok(PackVerifyOutput {
        pack_id,
        valid,
        message,
    })
}

/// Show dependency graph for installed packs
#[verb]
fn graph() -> VerbResult<PackGraphOutput> {
    let lockfile_path = std::path::PathBuf::from(".ggen/packs.lock");

    let packs = ggen_core::domain::packs::lockfile::list_packs(&lockfile_path)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let nodes: Vec<GraphEntry> = packs
        .iter()
        .map(|p| GraphEntry {
            id: p.id.clone(),
            label: format!("{}@{}", p.id, p.version),
        })
        .collect();

    let mut edges: Vec<EdgeEntry> = vec![];
    for pack in &packs {
        for dep in &pack.dependencies {
            edges.push(EdgeEntry {
                from: pack.id.clone(),
                to: dep.clone(),
            });
        }
    }

    println!("digraph packs {{");
    for node in &nodes {
        println!("  \"{}\" [label=\"{}\"]", node.id, node.label);
    }
    for edge in &edges {
        println!("  \"{}\" -> \"{}\"", edge.from, edge.to);
    }
    println!("}}");

    Ok(PackGraphOutput { nodes, edges })
}

/// Update an installed pack (reinstall with fresh digest)
#[verb]
fn update(pack_id: String) -> VerbResult<PackUpdateOutput> {
    let input = ggen_core::domain::packs::install::InstallInput {
        pack_id: pack_id.clone(),
        target_dir: None,
        force: true,
        dry_run: false,
    };

    let result = block_on(ggen_core::domain::packs::install::install_pack(&input))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    println!("  Pack '{}' updated", result.pack_id);
    if let Some(ref digest) = result.digest {
        println!("  New digest: {}", digest);
    }

    Ok(PackUpdateOutput {
        pack_id: result.pack_id,
        status: "updated".to_string(),
        message: format!("Pack '{}' updated to version {}", result.pack_id, result.pack_name),
        digest: result.digest,
    })
}
