//! Capability-first CLI for governed pack composition
//!
//! Canonical form:
//!   ggen capability enable mcp --projection rust --runtime axum --profile enterprise-strict
//!
//! Thin wrapper: all logic delegated to domain layer.

#![allow(clippy::unused_unit)]

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use crate::runtime::block_on;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct CapabilityEnableOutput {
    capability: String,
    projection: Option<String>,
    runtime: Option<String>,
    profile: Option<String>,
    atomic_packs: Vec<String>,
    install_commands: Vec<String>,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct CapabilityInspectOutput {
    capability: String,
    atomic_packs: Vec<String>,
}

#[derive(Serialize)]
struct CapabilityListOutput {
    capabilities: Vec<CapabilityInfo>,
    total: usize,
}

#[derive(Serialize)]
struct CapabilityInfo {
    id: String,
    name: String,
    description: String,
    category: String,
    atomic_packs: Vec<String>,
}

#[derive(Serialize)]
struct CapabilityGraphOutput {
    nodes: Vec<GraphNode>,
    edges: Vec<GraphEdge>,
}

#[derive(Serialize)]
struct GraphNode {
    id: String,
    label: String,
    node_type: String,
}

#[derive(Serialize)]
struct GraphEdge {
    from: String,
    to: String,
    label: String,
}

#[derive(Serialize)]
struct CapabilityTrustOutput {
    packs: Vec<TrustInfo>,
    total: usize,
}

#[derive(Serialize)]
struct TrustInfo {
    pack_id: String,
    trust_tier: String,
    digest: String,
}

#[derive(Serialize)]
struct CapabilityConflictsOutput {
    conflicts: Vec<ConflictInfo>,
    compatible: bool,
}

#[derive(Serialize)]
struct ConflictInfo {
    dimension: String,
    pack_a: String,
    pack_b: String,
    description: String,
    severity: String,
    resolution: Option<String>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Enable a capability with explicit parameters (canonical form)
#[verb]
fn enable(
    surface: String, projection: Option<String>, runtime: Option<String>, profile: Option<String>,
) -> VerbResult<CapabilityEnableOutput> {
    // Resolve capability to atomic packs via domain
    let atomic_packs = block_on(
        ggen_core::domain::packs::capability_registry::resolve_capability_to_packs(
            &surface,
            projection.as_deref(),
            runtime.as_deref(),
        ),
    )
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to resolve capability: {}", e)))?;

    let selected_profile = profile.clone().unwrap_or_else(|| "default".to_string());

    println!("Capability: {}", surface);
    if let Some(ref proj) = projection {
        println!("Projection: {}", proj);
    }
    if let Some(ref rt) = runtime {
        println!("Runtime: {}", rt);
    }
    println!("Profile: {}", selected_profile);
    println!("\nResolved atomic packs:");
    for pack in &atomic_packs {
        println!("  - {}", pack);
    }

    // Persist to lockfile via domain lockfile module
    let lockfile_path = PathBuf::from(".ggen/packs.lock");
    let ggen_dir = std::env::current_dir()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
        .join(".ggen");

    for pack_id in &atomic_packs {
        let entry = ggen_core::domain::packs::lockfile::InstalledPackEntry {
            id: pack_id.clone(),
            version: "1.0.0".to_string(),
            installed_at: chrono::Utc::now(),
            digest: None,
            registry_source: "registry".to_string(),
            trust_tier: "registry".to_string(),
            dependencies: vec![],
            install_path: format!(".ggen/packs/{}", pack_id),
            files: vec![],
        };

        ggen_core::domain::packs::lockfile::add_pack(&lockfile_path, &entry)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to update lockfile: {}", e)))?;
    }
    println!("\n  Lockfile updated: {}", lockfile_path.display());

    // Generate composition receipt via domain
    let receipt_path = ggen_core::domain::receipts::emit_composition_receipt(
        &surface,
        projection.as_deref(),
        runtime.as_deref(),
        profile.as_deref(),
        &atomic_packs,
        &ggen_dir,
    )
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to generate receipt: {}", e)))?;

    tracing::info!("Receipt generated: {}", receipt_path.display());

    let install_commands: Vec<String> = atomic_packs
        .iter()
        .map(|pack_id| format!("ggen packs add {}", pack_id))
        .collect();

    Ok(CapabilityEnableOutput {
        capability: surface,
        projection,
        runtime,
        profile: Some(selected_profile),
        atomic_packs,
        install_commands,
        status: "enabled".to_string(),
        message: format!(
            "Enabled {} with {} atomic packs; receipt at {}",
            surface,
            atomic_packs.len(),
            receipt_path.display()
        ),
    })
}

/// Disable a capability and remove its atomic packs
#[verb]
fn disable(capability: String) -> VerbResult<serde_json::Value> {
    let lockfile_path = PathBuf::from(".ggen/packs.lock");

    if !lockfile_path.exists() {
        return Ok(serde_json::json!({
            "capability": capability,
            "status": "disabled",
            "message": format!("Capability '{}' was not enabled (no lockfile found)", capability)
        }));
    }

    let atomic_packs = block_on(
        ggen_core::domain::packs::capability_registry::resolve_capability_to_packs(
            &capability, &None, &None,
        ),
    )
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to resolve capability: {}", e)))?;

    let mut removed_count = 0;
    for pack_id in &atomic_packs {
        if ggen_core::domain::packs::lockfile::remove_pack(&lockfile_path, pack_id)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to remove pack: {}", e)))?
        {
            removed_count += 1;
            println!("  Removed {}", pack_id);
        }
    }

    println!("\n  Lockfile updated: {}", lockfile_path.display());

    Ok(serde_json::json!({
        "capability": capability,
        "status": "disabled",
        "removed_packs": removed_count,
        "message": format!("Capability '{}' disabled (removed {} packs)", capability, removed_count)
    }))
}

/// Inspect a capability's atomic pack composition
#[verb]
fn inspect(capability: String) -> VerbResult<CapabilityInspectOutput> {
    let atomic_packs = block_on(
        ggen_core::domain::packs::capability_registry::resolve_capability_to_packs(
            &capability, &None, &None,
        ),
    )
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to resolve capability: {}", e)))?;

    println!("Inspecting capability: {}", capability);
    for pack in &atomic_packs {
        println!("  - {}", pack);
    }

    Ok(CapabilityInspectOutput {
        capability: capability.clone(),
        atomic_packs,
    })
}

/// List available capabilities
#[verb]
fn list(verbose: bool) -> VerbResult<CapabilityListOutput> {
    let registry = ggen_core::domain::packs::capability_resolve::CapabilityRegistry::load()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let defs = registry.list();

    if verbose {
        for def in defs {
            println!("  - {}: {}", def.id, def.name);
            println!("    Description: {}", def.description);
            println!("    Category: {}", def.category);
            println!("    Atomic packs: {}", def.atomic_packs.join(", "));
            println!();
        }
    }

    let capabilities: Vec<CapabilityInfo> = defs
        .iter()
        .map(|d| CapabilityInfo {
            id: d.id.clone(),
            name: d.name.clone(),
            description: d.description.clone(),
            category: d.category.clone(),
            atomic_packs: d.atomic_packs.clone(),
        })
        .collect();

    Ok(CapabilityListOutput {
        total: capabilities.len(),
        capabilities,
    })
}

/// Show resolved atomic pack graph before compile
#[verb]
fn graph(format: Option<String>) -> VerbResult<CapabilityGraphOutput> {
    let registry = ggen_core::domain::packs::capability_resolve::CapabilityRegistry::load()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let defs = registry.list();

    let nodes: Vec<GraphNode> = defs
        .iter()
        .map(|d| GraphNode {
            id: d.id.clone(),
            label: d.name.clone(),
            node_type: d.category.clone(),
        })
        .collect();

    let edges: Vec<GraphEdge> = vec![];

    if format.as_deref() == Some("dot") {
        println!("digraph capabilities {{");
        for node in &nodes {
            println!(
                "  \"{}\" [label=\"{}\", type=\"{}\"]",
                node.id, node.label, node.node_type
            );
        }
        println!("}}");
    }

    Ok(CapabilityGraphOutput { nodes, edges })
}

/// Show trust status for all packs
#[verb]
fn trust(verbose: bool) -> VerbResult<CapabilityTrustOutput> {
    let lockfile_path = PathBuf::from(".ggen/packs.lock");

    let packs = if lockfile_path.exists() {
        ggen_core::domain::packs::lockfile::list_packs(&lockfile_path)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    } else {
        vec![]
    };

    let trust_infos: Vec<TrustInfo> = packs
        .iter()
        .map(|p| TrustInfo {
            pack_id: p.id.clone(),
            trust_tier: p.trust_tier.clone(),
            digest: p
                .digest
                .as_ref()
                .map(|d| d.prefixed())
                .unwrap_or_else(|| "none".to_string()),
        })
        .collect();

    if verbose {
        for info in &trust_infos {
            println!("  - {}: {}", info.pack_id, info.trust_tier);
            println!("    Digest: {}", info.digest);
            println!();
        }
    }

    Ok(CapabilityTrustOutput {
        total: trust_infos.len(),
        packs: trust_infos,
    })
}

/// Detect and report conflicts
#[verb]
fn conflicts(verbose: bool) -> VerbResult<CapabilityConflictsOutput> {
    println!("Checking for conflicts across all capabilities...");

    let lockfile_path = PathBuf::from(".ggen/packs.lock");

    if !lockfile_path.exists() {
        println!("No lockfile found - no packs to check for conflicts");
        return Ok(CapabilityConflictsOutput {
            conflicts: vec![],
            compatible: true,
        });
    }

    let packs = ggen_core::domain::packs::lockfile::list_packs(&lockfile_path)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    if packs.is_empty() {
        println!("No packs installed - no conflicts possible");
        return Ok(CapabilityConflictsOutput {
            conflicts: vec![],
            compatible: true,
        });
    }

    // Check for dependency cycles
    let all_conflicts = detect_dependency_cycles(&packs)?;

    if verbose {
        if all_conflicts.is_empty() {
            println!("  No conflicts detected");
        } else {
            println!("\n  Conflicts detected: {}", all_conflicts.len());
            for c in &all_conflicts {
                println!("    [{}] {}: {} vs {}", c.severity, c.dimension, c.pack_a, c.pack_b);
            }
        }
    }

    let has_errors = all_conflicts.iter().any(|c| c.severity == "error");

    Ok(CapabilityConflictsOutput {
        conflicts: all_conflicts,
        compatible: !has_errors,
    })
}

/// Detect dependency cycles in the lockfile.
fn detect_dependency_cycles(
    packs: &[ggen_core::domain::packs::lockfile::InstalledPackEntry],
) -> VerbResult<Vec<ConflictInfo>> {
    let mut conflicts = vec![];
    let mut visited = std::collections::HashSet::new();
    let mut recursion_stack = std::collections::HashSet::new();

    for pack in packs {
        if has_cycle(pack, &packs, &mut visited, &mut recursion_stack) {
            conflicts.push(ConflictInfo {
                dimension: "dependency_cycle".to_string(),
                pack_a: pack.id.clone(),
                pack_b: pack.dependencies.first().cloned().unwrap_or_default(),
                description: format!("Circular dependency detected involving pack: {}", pack.id),
                severity: "error".to_string(),
                resolution: Some("Remove circular dependency by refactoring pack structure".to_string()),
            });
        }
    }

    Ok(conflicts)
}

fn has_cycle(
    pack: &ggen_core::domain::packs::lockfile::InstalledPackEntry,
    all_packs: &[ggen_core::domain::packs::lockfile::InstalledPackEntry],
    visited: &mut std::collections::HashSet<String>,
    recursion_stack: &mut std::collections::HashSet<String>,
) -> bool {
    if recursion_stack.contains(&pack.id) {
        return true;
    }
    if visited.contains(&pack.id) {
        return false;
    }

    visited.insert(pack.id.clone());
    recursion_stack.insert(pack.id.clone());

    for dep_id in &pack.dependencies {
        if let Some(dep) = all_packs.iter().find(|p| p.id == *dep_id) {
            if has_cycle(dep, all_packs, visited, recursion_stack) {
                return true;
            }
        }
    }

    recursion_stack.remove(&pack.id);
    false
}
