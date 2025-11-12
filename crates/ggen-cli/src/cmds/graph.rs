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
fn load(file: PathBuf, format: Option<String>) -> Result<LoadOutput> {
    use ggen_domain::graph::{execute_load, LoadInput};

    let input = LoadInput {
        file,
        format,
        base_iri: None,
        merge: false,
    };

    let result =
        crate::runtime::block_on(async move { execute_load(input).await }).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Load failed: {}", e))
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
    sparql_query: String, graph_file: Option<PathBuf>, format: Option<String>,
) -> Result<QueryOutput> {
    use ggen_domain::graph::{execute_query, QueryInput};

    let input = QueryInput {
        query: sparql_query,
        graph_file,
        format: format.unwrap_or_else(|| "json".to_string()),
    };

    let result =
        crate::runtime::block_on(async move { execute_query(input).await }).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Query failed: {}", e))
        })?;

    Ok(QueryOutput {
        bindings: result.bindings,
        variables: result.variables,
        result_count: result.result_count,
    })
}

/// Export graph to file
#[verb]
fn export(input_file: PathBuf, output: PathBuf, format: String) -> Result<ExportOutput> {
    use ggen_domain::graph::{execute_export, ExportInput};

    let input_data = ExportInput {
        input: input_file,
        output,
        format,
        pretty: false,
    };

    let result = crate::runtime::block_on(async move { execute_export(input_data).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Export failed: {}", e))
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
fn visualize(input_file: PathBuf, format: Option<String>) -> Result<VisualizeOutput> {
    use ggen_domain::graph::{execute_visualize, VisualizeInput};

    let input_data = VisualizeInput {
        input: input_file,
        output: None,
        format: format.unwrap_or_else(|| "dot".to_string()),
        labels: false,
        max_depth: None,
        subject: None,
    };

    let result = crate::runtime::block_on(async move { execute_visualize(input_data).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Visualize failed: {}", e))
        })?;

    Ok(VisualizeOutput {
        nodes_rendered: result.nodes_rendered,
        edges_rendered: result.edges_rendered,
        output_path: result.output_path,
        format: result.format,
    })
}
