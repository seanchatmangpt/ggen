//! Graph Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements RDF graph operations using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct LoadOutput {
    triples_loaded: usize,
    total_triples: usize,
    format: String,
    file_path: String,
}

#[derive(Serialize)]
struct QueryOutput {
    bindings: Vec<std::collections::HashMap<String, String>>,
    variables: Vec<String>,
    result_count: usize,
}

#[derive(Serialize)]
struct ExportOutput {
    output_path: String,
    format: String,
    triples_exported: usize,
    file_size_bytes: usize,
}

#[derive(Serialize)]
struct VisualizeOutput {
    nodes_rendered: usize,
    edges_rendered: usize,
    output_path: String,
    format: String,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Load RDF data into graph
#[verb]
fn load(file: String, format: Option<String>) -> Result<LoadOutput> {
    use ggen_domain::graph::{execute_load, LoadInput};

    let input = LoadInput {
        file: PathBuf::from(file),
        format,
        base_iri: None,
        merge: false,
    };

    let result = crate::runtime::block_on(async move {
        Ok(execute_load(input)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("Load failed: {}", e)))?)
    })
    .map_err(|e: ggen_utils::Error| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    .map_err(|e: ggen_utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(e.to_string())
    })?;

    Ok(LoadOutput {
        triples_loaded: result.triples_loaded,
        total_triples: result.total_triples,
        format: result.format,
        file_path: result.file_path,
    })
}

/// Query graph with SPARQL
#[verb]
fn query(
    sparql_query: String, graph_file: Option<String>, format: Option<String>,
) -> Result<QueryOutput> {
    use ggen_domain::graph::{execute_query, QueryInput};

    let input = QueryInput {
        query: sparql_query,
        graph_file: graph_file.map(PathBuf::from),
        format: format.unwrap_or_else(|| "json".to_string()),
    };

    let result = crate::runtime::block_on(async move {
        Ok(execute_query(input)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("Query failed: {}", e)))?)
    })
    .map_err(|e: ggen_utils::Error| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    .map_err(|e: ggen_utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(e.to_string())
    })?;

    Ok(QueryOutput {
        bindings: result.bindings,
        variables: result.variables,
        result_count: result.result_count,
    })
}

/// Export graph to file
#[verb]
fn export(input_file: String, output: String, format: String) -> Result<ExportOutput> {
    use ggen_domain::graph::{execute_export, ExportInput};

    let input_data = ExportInput {
        input: PathBuf::from(input_file),
        output: PathBuf::from(output),
        format,
        pretty: false,
    };

    let result = crate::runtime::block_on(async move {
        Ok(execute_export(input_data)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("Export failed: {}", e)))?)
    })
    .map_err(|e: ggen_utils::Error| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    .map_err(|e: ggen_utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(e.to_string())
    })?;

    Ok(ExportOutput {
        output_path: result.output_path,
        format: result.format,
        triples_exported: result.triples_exported,
        file_size_bytes: result.file_size_bytes,
    })
}

/// Visualize graph structure
#[verb]
fn visualize(input_file: String, format: Option<String>) -> Result<VisualizeOutput> {
    use ggen_domain::graph::{execute_visualize, VisualizeInput};

    let input_data = VisualizeInput {
        input: PathBuf::from(input_file),
        output: None,
        format: format.unwrap_or_else(|| "dot".to_string()),
        labels: false,
        max_depth: None,
        subject: None,
    };

    let result = crate::runtime::block_on(async move {
        Ok(execute_visualize(input_data)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("Visualize failed: {}", e)))?)
    })
    .map_err(|e: ggen_utils::Error| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    .map_err(|e: ggen_utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(e.to_string())
    })?;

    Ok(VisualizeOutput {
        nodes_rendered: result.nodes_rendered,
        edges_rendered: result.edges_rendered,
        output_path: result.output_path,
        format: result.format,
    })
}
