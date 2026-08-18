//! `ggen_pack_query` — the machine-facing surface for querying the LOCAL
//! PACK REGISTRY via SPARQL: either one pack's own RDF facts (`pack_id`
//! given) or the union of every pack currently in the local registry
//! (`pack_id` omitted). Distinct from `ggen_query_preview`, which queries a
//! *project's own graph* (`ggen.toml`/ontology), not the marketplace/pack
//! registry -- these two tools never overlap in scope.
//!
//! Thin adapter around `ggen_marketplace::packs_registry::sparql_executor::
//! run_pack_query`, the single implementation shared with the `ggen pack
//! query` CLI verb (`crates/ggen-cli/src/cmds/pack.rs`).

use ggen_marketplace::packs_registry::sparql_executor::{run_pack_query, Value as PackValue};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};

#[derive(Debug, Deserialize, JsonSchema)]
pub struct PackQueryParams {
    /// SPARQL query text to execute against the pack registry.
    pub sparql: String,
    /// When given, scope the query to this single pack's RDF facts. When
    /// omitted, the query runs over the union of every pack currently in
    /// the local registry.
    #[serde(default)]
    pub pack_id: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PackQueryResult {
    pub ok: bool,
    /// `"pack:<id>"` when `pack_id` was given, else `"all-packs"`.
    pub scope: String,
    pub packs_queried: usize,
    pub columns: Vec<String>,
    pub rows: Vec<Vec<serde_json::Value>>,
    pub row_count: usize,
    pub execution_time_ms: u128,
}

/// Validate and execute `params.sparql` against the local pack registry, at
/// `params.pack_id`'s scope (single pack) or across every pack (`None`).
///
/// # Errors
/// - `ErrorCategory::InputTooLarge`... no, this tool imposes no size cap of
///   its own beyond the underlying store's; empty-query rejection is
///   `ErrorCategory::SyntaxError` instead, checked before any pack RDF is
///   loaded.
/// - `ErrorCategory::NotFound` when `pack_id` names a pack absent from the
///   local registry.
/// - `ErrorCategory::GraphLoadError` for a malformed SPARQL query or any
///   other pack-load/query failure surfaced by the shared executor.
pub fn pack_query(params: &PackQueryParams) -> Result<PackQueryResult, McpError> {
    if params.sparql.trim().is_empty() {
        return Err(McpError::new(
            ErrorCategory::SyntaxError,
            "sparql must not be empty",
        ));
    }

    let outcome = run_pack_query(&params.sparql, params.pack_id.as_deref()).map_err(|e| {
        let category = if params.pack_id.is_some() && e.to_string().contains("not found") {
            ErrorCategory::NotFound
        } else {
            ErrorCategory::GraphLoadError
        };
        McpError::new(category, e.to_string())
    })?;

    let rows: Vec<Vec<serde_json::Value>> = outcome
        .result
        .rows
        .iter()
        .map(|row| row.iter().map(pack_value_to_json).collect())
        .collect();

    Ok(PackQueryResult {
        ok: true,
        scope: outcome.scope,
        packs_queried: outcome.packs_queried,
        columns: outcome.result.columns,
        row_count: rows.len(),
        rows,
        execution_time_ms: outcome.result.execution_time.as_millis(),
    })
}

fn pack_value_to_json(value: &PackValue) -> serde_json::Value {
    match value {
        PackValue::String(s) => serde_json::Value::String(s.clone()),
        PackValue::Integer(i) => serde_json::Value::from(*i),
        PackValue::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        PackValue::Boolean(b) => serde_json::Value::Bool(*b),
        PackValue::Null => serde_json::Value::Null,
    }
}
