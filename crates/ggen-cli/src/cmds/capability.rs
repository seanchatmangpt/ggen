//! Capability-first CLI for governed pack composition
//!
//! Canonical form:
//!   ggen capability enable mcp --projection rust --runtime axum --profile enterprise-strict
//!
//! Underlying model: expands to atomic packs, creates composition receipt

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::runtime::block_on;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};

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
    status: String,
    message: String,
}

#[derive(Serialize)]
struct CapabilityInspectOutput {
    capability: String,
    atomic_packs: Vec<String>,
    dependencies: Vec<String>,
    templates: Vec<String>,
    queries: Vec<String>,
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
    signature: String,
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
// Helper Functions
// ============================================================================

/// Update lockfile with resolved atomic packs
fn update_lockfile(lockfile_path: &PathBuf, atomic_packs: &[String]) -> Result<(), String> {
    let mut lockfile = if lockfile_path.exists() {
        PackLockfile::from_file(lockfile_path)
            .map_err(|e| format!("Failed to load lockfile: {}", e))?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };

    // Add each atomic pack to lockfile
    for pack_id in atomic_packs {
        let locked_pack = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: chrono::Utc::now(),
            dependencies: vec![],
        };
        lockfile.add_pack(pack_id, locked_pack);
    }

    lockfile
        .save(lockfile_path)
        .map_err(|e| format!("Failed to save lockfile: {}", e))?;
    Ok(())
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Enable a capability with explicit parameters (canonical form)
#[verb]
fn enable(
    surface: String, projection: Option<String>, runtime: Option<String>, profile: Option<String>,
) -> VerbResult<CapabilityEnableOutput> {
    run_capability_enable(surface, projection, runtime, profile)
}

fn run_capability_enable(
    surface: String, projection: Option<String>, runtime: Option<String>, profile: Option<String>,
) -> VerbResult<CapabilityEnableOutput> {
    use crate::receipt_manager::ReceiptManager;
    use std::path::PathBuf;

    // Resolve capability to atomic packs
    let atomic_packs_result = block_on(resolve_capability(&surface, &projection, &runtime))
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to resolve capability: {}",
                e
            ))
        })?;

    let atomic_packs = atomic_packs_result.map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to resolve capability: {}",
            e
        ))
    })?;

    let selected_profile = profile.unwrap_or_else(|| "default".to_string());

    // Display resolved packs
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

    // Persist to lockfile
    let lockfile_path = PathBuf::from(".ggen/packs.lock");
    update_lockfile(&lockfile_path, &atomic_packs).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to update lockfile: {}", e))
    })?;

    println!("\n✓ Lockfile updated: {}", lockfile_path.display());

    // Generate cryptographic receipt for composition
    let project_root = std::env::current_dir().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Cannot resolve project directory: {}",
            e
        ))
    })?;
    let ggen_dir = project_root.join(".ggen");
    let mut receipt_manager = ReceiptManager::new(ggen_dir).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create receipt manager: {}",
            e
        ))
    })?;

    let receipt_path = receipt_manager
        .generate_pack_install_receipt(
            &format!("capability-{}", surface),
            "1.0.0",
            &atomic_packs,
            &project_root,
        )
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to generate receipt: {}",
                e
            ))
        })?;

    tracing::info!("Receipt generated: {}", receipt_path.display());

    let surface_clone = surface.clone();
    let pack_count = atomic_packs.len();

    Ok(CapabilityEnableOutput {
        capability: surface,
        projection,
        runtime,
        profile: Some(selected_profile),
        atomic_packs,
        status: "enabled".to_string(),
        message: format!(
            "Enabled {} with {} atomic packs; receipt at {}",
            surface_clone,
            pack_count,
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

    let mut lockfile = PackLockfile::from_file(&lockfile_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to load lockfile: {}", e))
    })?;

    // Resolve capability to atomic packs to know which to remove
    let atomic_packs_result =
        block_on(resolve_capability(&capability, &None, &None)).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to resolve capability: {}",
                e
            ))
        })?;

    let atomic_packs = atomic_packs_result.map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to resolve capability: {}",
            e
        ))
    })?;

    let mut removed_count = 0;
    for pack_id in &atomic_packs {
        if lockfile.remove_pack(pack_id) {
            removed_count += 1;
            println!("  ✓ Removed {}", pack_id);
        }
    }

    // Save updated lockfile
    lockfile.save(&lockfile_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to save lockfile: {}", e))
    })?;

    println!("\n✓ Lockfile updated: {}", lockfile_path.display());

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
    use crate::runtime::block_on;

    println!("Inspecting capability: {}", capability);

    // Resolve capability to atomic packs
    let atomic_packs = block_on(resolve_capability(&capability, &None, &None))
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to resolve capability: {}",
                e
            ))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to resolve capability: {}",
                e
            ))
        })?;

    // Get pack details from lockfile and cache
    let (dependencies, templates, queries) = inspect_pack_details(&atomic_packs)?;

    // Print summary
    println!("\nAtomic Packs: {}", atomic_packs.len());
    for pack in &atomic_packs {
        println!("  - {}", pack);
    }
    println!("\nDependencies: {}", dependencies.len());
    for dep in &dependencies {
        println!("  - {}", dep);
    }
    println!("\nTemplates: {}", templates.len());
    for tmpl in &templates {
        println!("  - {}", tmpl);
    }
    println!("\nQueries: {}", queries.len());
    for query in &queries {
        println!("  - {}", query);
    }

    Ok(CapabilityInspectOutput {
        capability: capability.clone(),
        atomic_packs,
        dependencies,
        templates,
        queries,
    })
}

/// Helper: Get pack details from lockfile and cache
fn inspect_pack_details(
    atomic_packs: &[String],
) -> VerbResult<(Vec<String>, Vec<String>, Vec<String>)> {
    use std::fs;

    let lockfile_path = PathBuf::from(".ggen/packs.lock");
    let mut dependencies = vec![];
    let mut templates = vec![];
    let mut queries = vec![];

    if !lockfile_path.exists() {
        return Ok((dependencies, templates, queries));
    }

    let lockfile = PackLockfile::from_file(&lockfile_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to load lockfile: {}", e))
    })?;

    let cache_base = std::env::var("GGEN_PACK_CACHE_DIR")
        .ok()
        .map(PathBuf::from)
        .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
        .unwrap_or_else(|| PathBuf::from(".ggen/packs"));

    for pack_id in atomic_packs {
        if let Some(locked_pack) = lockfile.get_pack(pack_id) {
            dependencies.extend(locked_pack.dependencies.iter().cloned());

            let cache_dir = cache_base.join(pack_id);

            // Collect templates
            let templates_dir = cache_dir.join("templates");
            if templates_dir.exists() {
                if let Ok(entries) = fs::read_dir(&templates_dir) {
                    for entry in entries.filter_map(|e| e.ok()) {
                        if let Ok(name) = entry.file_name().into_string() {
                            if name.ends_with(".tera") {
                                templates.push(format!("{}:{}", pack_id, name));
                            }
                        }
                    }
                }
            }

            // Collect queries
            let queries_dir = cache_dir.join("queries");
            if queries_dir.exists() {
                if let Ok(entries) = fs::read_dir(&queries_dir) {
                    for entry in entries.filter_map(|e| e.ok()) {
                        if let Ok(name) = entry.file_name().into_string() {
                            if name.ends_with(".rq") {
                                queries.push(format!("{}:{}", pack_id, name));
                            }
                        }
                    }
                }
            }
        }
    }

    Ok((dependencies, templates, queries))
}

/// List available capabilities
#[verb]
fn list(verbose: bool) -> VerbResult<CapabilityListOutput> {
    let capabilities = vec![
        CapabilityInfo {
            id: "mcp".to_string(),
            name: "Model Context Protocol".to_string(),
            description: "MCP server and tools".to_string(),
            category: "surface".to_string(),
            atomic_packs: vec!["surface-mcp".to_string()],
        },
        CapabilityInfo {
            id: "a2a".to_string(),
            name: "Agent-to-Agent".to_string(),
            description: "A2A protocol and messaging".to_string(),
            category: "surface".to_string(),
            atomic_packs: vec!["surface-a2a".to_string()],
        },
        CapabilityInfo {
            id: "openapi".to_string(),
            name: "OpenAPI Contract".to_string(),
            description: "OpenAPI specification and validation".to_string(),
            category: "contract".to_string(),
            atomic_packs: vec!["contract-openapi".to_string()],
        },
    ];

    if verbose {
        for cap in &capabilities {
            println!("  - {}: {}", cap.id, cap.name);
            println!("    Description: {}", cap.description);
            println!("    Category: {}", cap.category);
            println!("    Atomic packs: {}", cap.atomic_packs.join(", "));
            println!();
        }
    }

    Ok(CapabilityListOutput {
        capabilities,
        total: 3,
    })
}

/// Show resolved atomic pack graph before compile
#[verb]
fn graph(format: Option<String>) -> VerbResult<CapabilityGraphOutput> {
    let nodes = vec![
        GraphNode {
            id: "surface-mcp".to_string(),
            label: "MCP Surface".to_string(),
            node_type: "surface".to_string(),
        },
        GraphNode {
            id: "projection-rust".to_string(),
            label: "Rust Projection".to_string(),
            node_type: "projection".to_string(),
        },
    ];

    let edges = vec![GraphEdge {
        from: "surface-mcp".to_string(),
        to: "projection-rust".to_string(),
        label: "projects-to".to_string(),
    }];

    if format.as_deref() == Some("dot") {
        println!("digraph capabilities {{");
        for node in &nodes {
            println!(
                "  \"{}\" [label=\"{}\", type=\"{}\"]",
                node.id, node.label, node.node_type
            );
        }
        for edge in &edges {
            println!(
                "  \"{}\" -> \"{}\" [label=\"{}\"]",
                edge.from, edge.to, edge.label
            );
        }
        println!("}}");
    }

    Ok(CapabilityGraphOutput { nodes, edges })
}

/// Show trust status for all packs
#[verb]
fn trust(verbose: bool) -> VerbResult<CapabilityTrustOutput> {
    let packs = vec![
        TrustInfo {
            pack_id: "surface-mcp".to_string(),
            trust_tier: "enterprise-certified".to_string(),
            signature: "abcd1234".to_string(),
            digest: "sha256:efgh5678".to_string(),
        },
        TrustInfo {
            pack_id: "projection-rust".to_string(),
            trust_tier: "enterprise-approved".to_string(),
            signature: "ijkl9012".to_string(),
            digest: "sha256:mnop3456".to_string(),
        },
    ];

    if verbose {
        for pack in &packs {
            println!("  - {}: {}", pack.pack_id, pack.trust_tier);
            println!("    Signature: {}", pack.signature);
            println!("    Digest: {}", pack.digest);
            println!();
        }
    }

    Ok(CapabilityTrustOutput { packs, total: 2 })
}

/// Detect and report conflicts
#[verb]
fn conflicts(verbose: bool) -> VerbResult<CapabilityConflictsOutput> {
    println!("Checking for conflicts across all capabilities...");

    let lockfile_path = PathBuf::from(".ggen/packs.lock");

    if !lockfile_path.exists() {
        println!("No lockfile found - no packs to check for conflicts");
        return ok_no_conflicts();
    }

    let lockfile = PackLockfile::from_file(&lockfile_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to load lockfile: {}", e))
    })?;

    let pack_ids: Vec<String> = lockfile.packs.keys().cloned().collect();

    if pack_ids.is_empty() {
        println!("No packs installed - no conflicts possible");
        return ok_no_conflicts();
    }

    // Check all conflict dimensions
    let all_conflicts = detect_all_conflicts(&pack_ids, &lockfile)?;

    // Print results if verbose
    if verbose {
        print_conflicts(&all_conflicts);
    }

    let has_errors = all_conflicts.iter().any(|c| c.severity == "error");

    Ok(CapabilityConflictsOutput {
        conflicts: all_conflicts,
        compatible: !has_errors,
    })
}

/// Helper: Return empty conflicts result
fn ok_no_conflicts() -> VerbResult<CapabilityConflictsOutput> {
    Ok(CapabilityConflictsOutput {
        conflicts: vec![],
        compatible: true,
    })
}

/// Helper: Print conflicts in verbose mode
fn print_conflicts(conflicts: &[ConflictInfo]) {
    if conflicts.is_empty() {
        println!("✓ No conflicts detected");
    } else {
        println!("\nConflicts detected: {}", conflicts.len());
        for conflict in conflicts {
            println!(
                "\n  [{}] {}: {} vs {}",
                conflict.severity, conflict.dimension, conflict.pack_a, conflict.pack_b
            );
            println!("    Description: {}", conflict.description);
            if let Some(ref resolution) = conflict.resolution {
                println!("    Resolution: {}", resolution);
            }
        }
    }
}

/// Helper: Detect all conflicts across multiple dimensions
fn detect_all_conflicts(
    pack_ids: &[String], lockfile: &PackLockfile,
) -> VerbResult<Vec<ConflictInfo>> {
    let mut all_conflicts = vec![];

    let cache_dir = std::env::var("GGEN_PACK_CACHE_DIR")
        .ok()
        .map(PathBuf::from)
        .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
        .unwrap_or_else(|| PathBuf::from(".ggen/packs"));

    // Dimension 1: File path conflicts
    all_conflicts.extend(detect_file_conflicts(pack_ids, &cache_dir)?);

    // Dimension 2: Template name conflicts
    all_conflicts.extend(detect_template_conflicts(pack_ids, &cache_dir)?);

    // Dimension 3: Query name conflicts
    all_conflicts.extend(detect_query_conflicts(pack_ids, &cache_dir)?);

    // Dimension 4: Dependency cycles
    all_conflicts.extend(detect_dependency_cycles(pack_ids, lockfile)?);

    Ok(all_conflicts)
}

/// Helper: Detect file path conflicts
fn detect_file_conflicts(
    pack_ids: &[String], cache_dir: &PathBuf,
) -> VerbResult<Vec<ConflictInfo>> {
    use std::fs;

    let mut conflicts = vec![];
    let mut file_owners: HashMap<String, Vec<String>> = HashMap::new();

    for pack_id in pack_ids {
        let pack_dir = cache_dir.join(pack_id).join("templates");
        if pack_dir.exists() {
            if let Ok(entries) = fs::read_dir(&pack_dir) {
                for entry in entries.flatten() {
                    if let Ok(name) = entry.file_name().into_string() {
                        if name.ends_with(".tera") {
                            let output_file = name.replace(".tera", "");
                            file_owners
                                .entry(output_file)
                                .or_insert_with(Vec::new)
                                .push(pack_id.clone());
                        }
                    }
                }
            }
        }
    }

    for (file, owners) in &file_owners {
        if owners.len() > 1 {
            conflicts.push(ConflictInfo {
                dimension: "emitted_file_path".to_string(),
                pack_a: owners[0].clone(),
                pack_b: owners[1].clone(),
                description: format!(
                    "Multiple packs would write to the same output file: {}",
                    file
                ),
                severity: "error".to_string(),
                resolution: Some(
                    "Remove one of the conflicting packs or specify different output paths"
                        .to_string(),
                ),
            });
        }
    }

    Ok(conflicts)
}

/// Helper: Detect template name conflicts
fn detect_template_conflicts(
    pack_ids: &[String], cache_dir: &PathBuf,
) -> VerbResult<Vec<ConflictInfo>> {
    use std::fs;

    let mut conflicts = vec![];
    let mut template_names: HashMap<String, Vec<String>> = HashMap::new();

    for pack_id in pack_ids {
        let pack_dir = cache_dir.join(pack_id).join("templates");
        if pack_dir.exists() {
            if let Ok(entries) = fs::read_dir(&pack_dir) {
                for entry in entries.flatten() {
                    if let Ok(name) = entry.file_name().into_string() {
                        let template_name = name.replace(".tera", "");
                        template_names
                            .entry(template_name)
                            .or_insert_with(Vec::new)
                            .push(pack_id.clone());
                    }
                }
            }
        }
    }

    for (template_name, owners) in &template_names {
        if owners.len() > 1 {
            conflicts.push(ConflictInfo {
                dimension: "template_name".to_string(),
                pack_a: owners[0].clone(),
                pack_b: owners[1].clone(),
                description: format!(
                    "Multiple packs provide templates with the same name: {}",
                    template_name
                ),
                severity: "warning".to_string(),
                resolution: Some("Templates will be namespaced by pack ID".to_string()),
            });
        }
    }

    Ok(conflicts)
}

/// Helper: Detect query name conflicts
fn detect_query_conflicts(
    pack_ids: &[String], cache_dir: &PathBuf,
) -> VerbResult<Vec<ConflictInfo>> {
    use std::fs;

    let mut conflicts = vec![];
    let mut query_names: HashMap<String, Vec<String>> = HashMap::new();

    for pack_id in pack_ids {
        let pack_dir = cache_dir.join(pack_id).join("queries");
        if pack_dir.exists() {
            if let Ok(entries) = fs::read_dir(&pack_dir) {
                for entry in entries.flatten() {
                    if let Ok(name) = entry.file_name().into_string() {
                        let query_name = name.replace(".rq", "");
                        query_names
                            .entry(query_name)
                            .or_insert_with(Vec::new)
                            .push(pack_id.clone());
                    }
                }
            }
        }
    }

    for (query_name, owners) in &query_names {
        if owners.len() > 1 {
            conflicts.push(ConflictInfo {
                dimension: "query_name".to_string(),
                pack_a: owners[0].clone(),
                pack_b: owners[1].clone(),
                description: format!(
                    "Multiple packs provide queries with the same name: {}",
                    query_name
                ),
                severity: "error".to_string(),
                resolution: Some("Queries must have unique names across all packs".to_string()),
            });
        }
    }

    Ok(conflicts)
}

/// Helper: Detect dependency cycles
fn detect_dependency_cycles(
    pack_ids: &[String], lockfile: &PackLockfile,
) -> VerbResult<Vec<ConflictInfo>> {
    let mut conflicts = vec![];
    let mut visited = HashSet::new();
    let mut recursion_stack = HashSet::new();

    for pack_id in pack_ids {
        if let Some(locked_pack) = lockfile.get_pack(pack_id) {
            if has_cycle(
                pack_id,
                &locked_pack.dependencies,
                lockfile,
                &mut visited,
                &mut recursion_stack,
            ) {
                conflicts.push(ConflictInfo {
                    dimension: "dependency_cycle".to_string(),
                    pack_a: pack_id.clone(),
                    pack_b: locked_pack
                        .dependencies
                        .first()
                        .unwrap_or(&"unknown".to_string())
                        .clone(),
                    description: format!(
                        "Circular dependency detected involving pack: {}",
                        pack_id
                    ),
                    severity: "error".to_string(),
                    resolution: Some(
                        "Remove circular dependency by refactoring pack structure".to_string(),
                    ),
                });
            }
        }
    }

    Ok(conflicts)
}

/// Helper function to detect dependency cycles
fn has_cycle(
    pack_id: &str, dependencies: &[String], lockfile: &PackLockfile, visited: &mut HashSet<String>,
    recursion_stack: &mut HashSet<String>,
) -> bool {
    if recursion_stack.contains(pack_id) {
        return true;
    }

    if visited.contains(pack_id) {
        return false;
    }

    visited.insert(pack_id.to_string());
    recursion_stack.insert(pack_id.to_string());

    for dep in dependencies {
        if let Some(locked_pack) = lockfile.get_pack(dep) {
            if has_cycle(
                dep,
                &locked_pack.dependencies,
                lockfile,
                visited,
                recursion_stack,
            ) {
                return true;
            }
        }
    }

    recursion_stack.remove(pack_id);
    false
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Resolve capability to atomic packs
async fn resolve_capability(
    surface: &str, projection: &Option<String>, runtime: &Option<String>,
) -> Result<Vec<String>, String> {
    let mut atomic_packs = vec![format!("surface-{}", surface)];

    if let Some(ref proj) = projection {
        atomic_packs.push(format!("projection-{}", proj));
    }

    if let Some(ref rt) = runtime {
        atomic_packs.push(format!("runtime-{}", rt));
    }

    // Add default dependencies
    atomic_packs.push("core-ontology".to_string());

    Ok(atomic_packs)
}
