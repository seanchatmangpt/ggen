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
// Verb Functions
// ============================================================================

/// Enable a capability with explicit parameters (canonical form)
#[verb]
fn enable(
    surface: String,
    projection: Option<String>,
    runtime: Option<String>,
    profile: Option<String>,
) -> VerbResult<CapabilityEnableOutput> {
    // Resolve capability to atomic packs
    let atomic_packs_result = block_on(resolve_capability(&surface, &projection, &runtime))
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to resolve capability: {}", e))
        })?;

    let atomic_packs = atomic_packs_result.map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to resolve capability: {}", e))
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

    let surface_clone = surface.clone();
    let pack_count = atomic_packs.len();

    Ok(CapabilityEnableOutput {
        capability: surface,
        projection,
        runtime,
        profile: Some(selected_profile),
        atomic_packs,
        status: "enabled".to_string(),
        message: format!("Enabled {} with {} atomic packs", surface_clone, pack_count),
    })
}

/// Disable a capability and remove its atomic packs
#[verb]
fn disable(capability: String) -> VerbResult<serde_json::Value> {
    // TODO: Implement pack removal from lockfile
    Ok(serde_json::json!({
        "capability": capability,
        "status": "disabled",
        "message": format!("Capability '{}' disabled", capability)
    }))
}

/// Inspect a capability's atomic pack composition
#[verb]
fn inspect(capability: String) -> VerbResult<CapabilityInspectOutput> {
    // TODO: Load pack metadata and return detailed info
    let atomic_packs = vec![
        format!("surface-{}", capability),
        format!("projection-rust"),
    ];

    Ok(CapabilityInspectOutput {
        capability: capability.clone(),
        atomic_packs: atomic_packs.clone(),
        dependencies: vec![],
        templates: vec![],
        queries: vec![],
    })
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
            println!("  \"{}\" [label=\"{}\", type=\"{}\"]", node.id, node.label, node.node_type);
        }
        for edge in &edges {
            println!("  \"{}\" -> \"{}\" [label=\"{}\"]", edge.from, edge.to, edge.label);
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

    Ok(CapabilityTrustOutput {
        packs,
        total: 2,
    })
}

/// Detect and report conflicts
#[verb]
fn conflicts(verbose: bool) -> VerbResult<CapabilityConflictsOutput> {
    // TODO: Implement real multi-dimensional conflict detection
    let conflicts: Vec<ConflictInfo> = vec![];

    if verbose {
        if conflicts.is_empty() {
            println!("No conflicts detected");
        } else {
            for conflict in &conflicts {
                println!("  - {}: {} vs {}", conflict.dimension, conflict.pack_a, conflict.pack_b);
                println!("    Severity: {}", conflict.severity);
                println!("    Description: {}", conflict.description);
                if let Some(ref resolution) = conflict.resolution {
                    println!("    Resolution: {}", resolution);
                }
                println!();
            }
        }
    }

    Ok(CapabilityConflictsOutput {
        conflicts,
        compatible: true,
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Resolve capability to atomic packs
async fn resolve_capability(
    surface: &str,
    projection: &Option<String>,
    runtime: &Option<String>,
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
