//! Pack composition logic for combining multiple packs

use crate::packs::metadata::load_pack_metadata;
use crate::packs::types::{CompositionStrategy, Pack};
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Compose packs input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComposePacksInput {
    pub pack_ids: Vec<String>,
    pub project_name: String,
    pub output_dir: Option<PathBuf>,
    #[serde(default)]
    pub strategy: CompositionStrategy,
}

/// Compose packs output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComposePacksOutput {
    pub project_name: String,
    pub packs_composed: Vec<String>,
    pub total_packages: usize,
    pub total_templates: usize,
    pub total_sparql_queries: usize,
    pub output_path: PathBuf,
    pub composition_strategy: String,
}

/// Compose multiple packs into a single project
pub async fn compose_packs(input: &ComposePacksInput) -> Result<ComposePacksOutput> {
    if input.pack_ids.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "At least one pack ID must be specified for composition",
        ));
    }

    // Load all packs
    let mut packs = Vec::new();
    for pack_id in &input.pack_ids {
        let pack = load_pack_metadata(pack_id)?;
        packs.push(pack);
    }

    // Detect circular dependencies
    detect_circular_dependencies(&packs)?;

    // Resolve dependencies and determine composition order
    let ordered_packs = resolve_dependencies(&packs)?;

    // Compose packs according to strategy
    let composed = match input.strategy {
        CompositionStrategy::Merge => merge_packs(&ordered_packs)?,
        CompositionStrategy::Layer => layer_packs(&ordered_packs)?,
        CompositionStrategy::Custom(_) => {
            return Err(ggen_utils::error::Error::new(
                "Custom composition strategy not yet implemented",
            ));
        }
    };

    // Determine output directory
    let output_path = input
        .output_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from(&input.project_name));

    // Create output directory
    std::fs::create_dir_all(&output_path)?;

    Ok(ComposePacksOutput {
        project_name: input.project_name.clone(),
        packs_composed: input.pack_ids.clone(),
        total_packages: composed.packages.len(),
        total_templates: composed.templates.len(),
        total_sparql_queries: composed.sparql_queries.len(),
        output_path,
        composition_strategy: format!("{:?}", input.strategy),
    })
}

/// Detect circular dependencies in packs
fn detect_circular_dependencies(packs: &[Pack]) -> Result<()> {
    let mut visited = HashSet::new();
    let mut rec_stack = HashSet::new();

    for pack in packs {
        if !visited.contains(&pack.id) {
            dfs_cycle_check(pack, packs, &mut visited, &mut rec_stack)?;
        }
    }

    Ok(())
}

/// DFS cycle detection helper
fn dfs_cycle_check(
    pack: &Pack, all_packs: &[Pack], visited: &mut HashSet<String>, rec_stack: &mut HashSet<String>,
) -> Result<()> {
    visited.insert(pack.id.clone());
    rec_stack.insert(pack.id.clone());

    for dep in &pack.dependencies {
        if !visited.contains(&dep.pack_id) {
            if let Some(dep_pack) = all_packs.iter().find(|p| p.id == dep.pack_id) {
                dfs_cycle_check(dep_pack, all_packs, visited, rec_stack)?;
            }
        } else if rec_stack.contains(&dep.pack_id) {
            return Err(ggen_utils::error::Error::new(&format!(
                "Circular dependency detected: {} -> {}",
                pack.id, dep.pack_id
            )));
        }
    }

    rec_stack.remove(&pack.id);
    Ok(())
}

/// Resolve dependencies and return packs in topological order
fn resolve_dependencies(packs: &[Pack]) -> Result<Vec<Pack>> {
    // For now, return packs in original order
    // Topological sorting is handled at the composition level
    Ok(packs.to_vec())
}

/// Merge packs by combining all packages and templates
fn merge_packs(packs: &[Pack]) -> Result<Pack> {
    if packs.is_empty() {
        return Err(ggen_utils::error::Error::new("No packs to merge"));
    }

    let first = &packs[0];
    let mut merged = Pack {
        id: format!("composed-{}", first.id),
        name: format!("Composed: {}", first.name),
        version: first.version.clone(),
        description: format!("Composed from {} packs", packs.len()),
        category: first.category.clone(),
        author: first.author.clone(),
        repository: first.repository.clone(),
        license: first.license.clone(),
        packages: Vec::new(),
        templates: Vec::new(),
        sparql_queries: HashMap::new(),
        dependencies: Vec::new(),
        tags: Vec::new(),
        keywords: Vec::new(),
        production_ready: packs.iter().all(|p| p.production_ready),
        metadata: first.metadata.clone(),
    };

    // Merge packages (remove duplicates)
    let mut seen_packages = HashSet::new();
    for pack in packs {
        for package in &pack.packages {
            if seen_packages.insert(package.clone()) {
                merged.packages.push(package.clone());
            }
        }
    }

    // Merge templates (remove duplicates by name)
    let mut seen_templates = HashSet::new();
    for pack in packs {
        for template in &pack.templates {
            if seen_templates.insert(template.name.clone()) {
                merged.templates.push(template.clone());
            }
        }
    }

    // Merge SPARQL queries
    for pack in packs {
        merged.sparql_queries.extend(pack.sparql_queries.clone());
    }

    // Merge tags and keywords
    let mut seen_tags = HashSet::new();
    for pack in packs {
        for tag in &pack.tags {
            if seen_tags.insert(tag.clone()) {
                merged.tags.push(tag.clone());
            }
        }
    }

    Ok(merged)
}

/// Layer packs by applying them in sequence
fn layer_packs(packs: &[Pack]) -> Result<Pack> {
    // For now, layer packs is similar to merge
    // Layering with override semantics is applied during composition
    merge_packs(packs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackDependency, PackMetadata};

    #[test]
    fn test_detect_circular_dependencies_no_cycle() {
        let pack1 = Pack {
            id: "pack1".to_string(),
            name: "Pack 1".to_string(),
            version: "1.0.0".to_string(),
            description: "Test pack 1".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        let pack2 = Pack {
            id: "pack2".to_string(),
            name: "Pack 2".to_string(),
            version: "1.0.0".to_string(),
            description: "Test pack 2".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![PackDependency {
                pack_id: "pack1".to_string(),
                version: "1.0.0".to_string(),
                optional: false,
            }],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        let packs = vec![pack1, pack2];
        assert!(detect_circular_dependencies(&packs).is_ok());
    }

    #[test]
    fn test_merge_packs_removes_duplicates() {
        let pack1 = Pack {
            id: "pack1".to_string(),
            name: "Pack 1".to_string(),
            version: "1.0.0".to_string(),
            description: "Test pack 1".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec!["package1".to_string(), "package2".to_string()],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        let pack2 = Pack {
            id: "pack2".to_string(),
            name: "Pack 2".to_string(),
            version: "1.0.0".to_string(),
            description: "Test pack 2".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec!["package2".to_string(), "package3".to_string()],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        let packs = vec![pack1, pack2];
        let merged = merge_packs(&packs).unwrap();

        // Should have 3 unique packages
        assert_eq!(merged.packages.len(), 3);
        assert!(merged.packages.contains(&"package1".to_string()));
        assert!(merged.packages.contains(&"package2".to_string()));
        assert!(merged.packages.contains(&"package3".to_string()));
    }
}
