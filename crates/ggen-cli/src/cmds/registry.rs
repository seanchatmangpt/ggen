//! Registry Commands - clap-noun-verb auto-discovery
//!
//! This module provides CLI commands for interacting with the mcpp pack registry.
//! The registry exposes pack metadata from the marketplace/packs directory.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use mcpp_domain::packs::metadata::{list_packs, load_pack_metadata};
use serde::Serialize;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct SearchOutput {
    query: String,
    packs: Vec<RegistryEntry>,
    total: usize,
}

#[derive(Serialize)]
struct ListOutput {
    packs: Vec<RegistryEntry>,
    total: usize,
}

#[derive(Serialize)]
struct RegistryEntry {
    id: String,
    name: String,
    version: String,
    category: String,
    description: String,
    tags: Vec<String>,
    production_ready: bool,
}

#[derive(Serialize)]
struct InfoOutput {
    id: String,
    name: String,
    version: String,
    category: String,
    description: String,
    author: String,
    license: String,
    repository: String,
    tags: Vec<String>,
    packages: Vec<String>,
    production_ready: bool,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search registry packs by name, id, tag, or description
#[verb]
fn search(query: String) -> Result<SearchOutput> {
    let q = query.to_lowercase();
    let all_packs = list_packs(None).unwrap_or_default();
    let matched: Vec<RegistryEntry> = all_packs
        .into_iter()
        .filter(|p| {
            p.id.to_lowercase().contains(&q)
                || p.name.to_lowercase().contains(&q)
                || p.description.to_lowercase().contains(&q)
                || p.tags.iter().any(|t| t.to_lowercase().contains(&q))
        })
        .map(|p| RegistryEntry {
            id: p.id,
            name: p.name,
            version: p.version,
            category: p.category,
            description: p.description,
            tags: p.tags,
            production_ready: p.production_ready,
        })
        .collect();
    let total = matched.len();
    Ok(SearchOutput {
        query,
        packs: matched,
        total,
    })
}

/// List all packs in the registry
#[verb]
fn list() -> Result<ListOutput> {
    let all_packs = list_packs(None).unwrap_or_default();
    let entries: Vec<RegistryEntry> = all_packs
        .into_iter()
        .map(|p| RegistryEntry {
            id: p.id,
            name: p.name,
            version: p.version,
            category: p.category,
            description: p.description,
            tags: p.tags,
            production_ready: p.production_ready,
        })
        .collect();
    let total = entries.len();
    Ok(ListOutput {
        packs: entries,
        total,
    })
}

/// Show detailed information about a pack
#[verb]
fn info(pack_id: String) -> Result<InfoOutput> {
    match load_pack_metadata(&pack_id) {
        Ok(p) => Ok(InfoOutput {
            id: p.id,
            name: p.name,
            version: p.version,
            category: p.category,
            description: p.description,
            author: p.author.unwrap_or_default(),
            license: p.license.unwrap_or_default(),
            repository: p.repository.unwrap_or_default(),
            tags: p.tags,
            packages: p.packages,
            production_ready: p.production_ready,
        }),
        Err(e) => Err(NounVerbError::execution_error(e.to_string())),
    }
}
