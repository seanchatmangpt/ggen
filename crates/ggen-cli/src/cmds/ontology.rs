//! Ontology Commands - Embedded and Marketplace Ontology Management
//!
//! This module implements ontology management commands for the ggen CLI:
//! - `ggen ontology list --embedded` - List embedded ontologies
//! - `ggen ontology status <uri>` - Check ontology availability
//! - `ggen ontology info <uri>` - Display detailed metadata
//! - `ggen ontology search <domain>` - Search for domain ontologies (placeholder)

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_core::ontology::{CoreOntologyBundle, OntologyLoader};
use serde::Serialize;
use std::collections::BTreeMap;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

/// Output for the `ggen ontology list` command
#[derive(Debug, Clone, Serialize)]
pub struct OntologyListOutput {
    /// List of ontologies
    pub ontologies: Vec<OntologyListEntry>,
    /// Total count
    pub count: usize,
}

/// Single ontology entry for list output
#[derive(Debug, Clone, Serialize)]
pub struct OntologyListEntry {
    /// Ontology name
    pub name: String,
    /// Namespace URI
    pub namespace: String,
    /// Size in bytes
    pub size: usize,
}

/// Output for the `ggen ontology status` command
#[derive(Debug, Clone, Serialize)]
pub struct OntologyStatusOutput {
    /// URI being queried
    pub uri: String,
    /// Whether it's embedded in the core bundle
    pub embedded: bool,
    /// Location ("core-bundle", "filesystem", or "not-found")
    pub location: String,
    /// Size in bytes if found
    pub size: Option<usize>,
    /// Name if found
    pub name: Option<String>,
}

/// Output for the `ggen ontology info` command
#[derive(Debug, Clone, Serialize)]
pub struct OntologyInfoOutput {
    /// Ontology name
    pub name: String,
    /// Namespace URI
    pub namespace: String,
    /// Size in bytes
    pub size: usize,
    /// Whether embedded
    pub embedded: bool,
    /// Content hash if available
    pub hash: Option<String>,
    /// Metadata
    pub metadata: BTreeMap<String, String>,
}

/// Output for the `ggen ontology search` command
#[derive(Debug, Clone, Serialize)]
pub struct OntologySearchOutput {
    /// Search query
    pub query: String,
    /// Results found
    pub results: Vec<OntologySearchResult>,
    /// Total count
    pub count: usize,
    /// Message (for placeholders)
    pub message: Option<String>,
}

/// Single search result entry
#[derive(Debug, Clone, Serialize)]
pub struct OntologySearchResult {
    /// Package name
    pub name: String,
    /// Brief description
    pub description: String,
    /// Domain category
    pub domain: String,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// List all embedded ontologies in the core bundle.
///
/// Usage:
///   ggen ontology list
///   ggen ontology list --embedded
#[verb("ontology list", "root")]
pub fn list(
    #[arg(long, default_value = "true")] embedded: bool,
) -> VerbResult<OntologyListOutput> {
    if !embedded {
        return Ok(OntologyListOutput {
            ontologies: vec![],
            count: 0,
        });
    }

    let ontologies = CoreOntologyBundle::all();
    let entries: Vec<OntologyListEntry> = ontologies
        .iter()
        .map(|ont| OntologyListEntry {
            name: ont.name.to_string(),
            namespace: ont.namespace.to_string(),
            size: ont.size,
        })
        .collect();

    let count = entries.len();
    Ok(OntologyListOutput {
        ontologies: entries,
        count,
    })
}

/// Check the status and availability of an ontology.
///
/// Usage:
///   ggen ontology status http://www.w3.org/1999/02/22-rdf-syntax-ns#
///   ggen ontology status <uri>
#[verb("ontology status", "root")]
pub fn status(uri: String) -> VerbResult<OntologyStatusOutput> {
    let is_embedded = OntologyLoader::is_embedded(&uri);

    if is_embedded {
        // Get metadata from core bundle
        if let Some(metadata) = OntologyLoader::get_metadata(&uri) {
            return Ok(OntologyStatusOutput {
                uri: uri.clone(),
                embedded: true,
                location: "core-bundle".to_string(),
                size: Some(metadata.size),
                name: Some(metadata.name),
            });
        }
    }

    // Check if it exists on the filesystem
    if let Some(_content) = OntologyLoader::load_content(&uri, std::path::Path::new(".")) {
        return Ok(OntologyStatusOutput {
            uri: uri.clone(),
            embedded: false,
            location: "filesystem".to_string(),
            size: None,
            name: None,
        });
    }

    // Not found
    Ok(OntologyStatusOutput {
        uri: uri.clone(),
        embedded: false,
        location: "not-found".to_string(),
        size: None,
        name: None,
    })
}

/// Display detailed metadata for an ontology.
///
/// Usage:
///   ggen ontology info http://www.w3.org/1999/02/22-rdf-syntax-ns#
///   ggen ontology info <uri>
#[verb("ontology info", "root")]
pub fn info(uri: String) -> VerbResult<OntologyInfoOutput> {
    // Try to get metadata from core bundle
    if let Some(metadata) = OntologyLoader::get_metadata(&uri) {
        let mut metadata_map = BTreeMap::new();
        metadata_map.insert("content_length".to_string(), metadata.size.to_string());

        return Ok(OntologyInfoOutput {
            name: metadata.name,
            namespace: uri.clone(),
            size: metadata.size,
            embedded: true,
            hash: None, // Hash could be computed if needed
            metadata: metadata_map,
        });
    }

    // Not found
    let mut metadata_map = BTreeMap::new();
    metadata_map.insert("status".to_string(), "not found".to_string());

    Ok(OntologyInfoOutput {
        name: "unknown".to_string(),
        namespace: uri.clone(),
        size: 0,
        embedded: false,
        hash: None,
        metadata: metadata_map,
    })
}

/// Search for ontologies by domain or keyword.
///
/// Usage:
///   ggen ontology search financial
///   ggen ontology search healthcare
///   ggen ontology search <domain>
///
/// Note: This is a placeholder for marketplace integration.
#[verb("ontology search", "root")]
pub fn search(
    #[arg(trailing_var_arg = true)] domain: Vec<String>,
) -> VerbResult<OntologySearchOutput> {
    let query = domain.join(" ");

    // Placeholder: In Phase 4, this will query the marketplace
    let message = Some(format!(
        "Marketplace integration not yet available. Use 'ggen ontology list' to see embedded ontologies."
    ));

    Ok(OntologySearchOutput {
        query,
        results: vec![],
        count: 0,
        message,
    })
}
