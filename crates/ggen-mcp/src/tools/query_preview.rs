//! `ggen_query_preview` — the flagship tool. Direct fix for the verified
//! failure that motivated this crate: a SPARQL query with a mandatory
//! (non-OPTIONAL) triple on a predicate used zero times in the graph
//! silently returned 0 of 113 expected rows, with no tool able to ask
//! "does my query return rows, and how many" before it was committed to a
//! template. See crates/ggen-mcp/README.md.

use std::collections::BTreeSet;
use std::path::Path;

use ggen_engine::graph::{EngineQueryResults, EngineValue, GraphEngine};
use ggen_graph::sparql::{check_sparql_syntax, sparql_kind, SparqlKind};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::limits::{MAX_QUERY_RESULT_ROWS, MAX_QUERY_TEXT_BYTES};
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct QueryPreviewParams {
    /// Project root directory (containing `ggen.toml`).
    pub root: String,
    /// SPARQL query text to execute against the project's loaded graph.
    pub sparql: String,
    /// Maximum rows to return in the response body (still subject to the
    /// server's hard `MAX_QUERY_RESULT_ROWS` ceiling). Defaults to that
    /// ceiling when omitted.
    #[serde(default)]
    pub max_rows: Option<usize>,
}

#[derive(Debug, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum QueryKind {
    Select,
    Ask,
    Construct,
    Describe,
    Unknown,
}

impl From<SparqlKind> for QueryKind {
    fn from(k: SparqlKind) -> Self {
        match k {
            SparqlKind::Select => Self::Select,
            SparqlKind::Ask => Self::Ask,
            SparqlKind::Construct => Self::Construct,
            SparqlKind::Describe => Self::Describe,
            SparqlKind::Unknown => Self::Unknown,
        }
    }
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct QueryPreviewResult {
    pub ok: bool,
    pub query_kind: QueryKind,
    /// The TRUE row count before any truncation is applied. Always present,
    /// always honest -- this is the field that would have caught the
    /// motivating incident (a mandatory triple pattern silently matching
    /// nothing).
    pub row_count: usize,
    /// `true` iff `row_count` exceeds what's actually returned in `rows`.
    pub truncated: bool,
    /// `rows.len()` -- may be less than `row_count` when `truncated`.
    pub returned_rows: usize,
    /// SELECT bindings, one JSON object per row. Empty for ASK/CONSTRUCT.
    pub rows: Vec<serde_json::Map<String, serde_json::Value>>,
    /// Present (`Some(true|false)`) only for ASK queries.
    pub boolean_result: Option<bool>,
    /// Present specifically so a zero-row SELECT still tells the caller
    /// something: an empty set here on a zero-row result means none of the
    /// query's own bound terms ever touched the graph, which is the direct
    /// signal the motivating incident needed. The two query kinds populate
    /// this with different precision, and callers should not assume
    /// predicate-position specificity for SELECT:
    /// - CONSTRUCT/DESCRIBE (graph results): strictly triple-predicate
    ///   IRIs (`t.predicate` only).
    /// - SELECT (solution bindings): any IRI-shaped bound value across ALL
    ///   projected variables, regardless of which triple-pattern position
    ///   that variable occupies -- `EngineRow` carries no positional
    ///   (subject/predicate/object) metadata, so a subject- or
    ///   object-bound IRI lands here too, not just predicate-bound ones.
    pub touched_predicates: BTreeSet<String>,
    pub elapsed_ms: u64,
}

/// Validate and execute `params.sparql` against `params.root`'s project
/// graph. Pure function: no MCP/rmcp types in the signature, so this is
/// independently testable and reusable by the wire-protocol adapter.
///
/// # Errors
/// - `ErrorCategory::InputTooLarge` if `sparql` exceeds
///   [`MAX_QUERY_TEXT_BYTES`], checked before anything else.
/// - `ErrorCategory::SyntaxError` if the query does not parse -- checked
///   before the project graph is loaded, so a malformed query never pays
///   graph-load cost.
/// - `ErrorCategory::PathTraversal` / `ErrorCategory::GraphLoadError` if
///   `root` or the project's ontology cannot be resolved/loaded.
///
/// A **zero-row SELECT is never an error** -- `Ok(QueryPreviewResult { ok:
/// true, row_count: 0, .. })`. Making that fact loud instead of silent is
/// this tool's entire reason to exist.
pub fn query_preview(params: &QueryPreviewParams) -> Result<QueryPreviewResult, McpError> {
    let started = std::time::Instant::now();

    if params.sparql.len() > MAX_QUERY_TEXT_BYTES {
        return Err(McpError::new(
            ErrorCategory::InputTooLarge,
            format!("sparql text exceeds {MAX_QUERY_TEXT_BYTES} bytes"),
        ));
    }

    // Syntax gate FIRST, always, before graph load -- a malformed query
    // must never be attempted against the graph.
    if let Err(parse_err) = check_sparql_syntax(&params.sparql) {
        return Err(McpError::new(ErrorCategory::SyntaxError, parse_err));
    }
    let kind = sparql_kind(&params.sparql);

    let root = resolve_root(&params.root)?;
    let graph = load_graph(&root)?;

    let results = graph
        .query(&params.sparql)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;

    let response = match results {
        EngineQueryResults::Boolean(b) => QueryPreviewResult {
            ok: true,
            query_kind: kind.into(),
            row_count: 0,
            truncated: false,
            returned_rows: 0,
            rows: Vec::new(),
            boolean_result: Some(b),
            touched_predicates: BTreeSet::new(),
            elapsed_ms: started.elapsed().as_millis() as u64,
        },
        EngineQueryResults::Solutions(all_rows) => {
            let row_count = all_rows.len();
            let cap = params
                .max_rows
                .unwrap_or(MAX_QUERY_RESULT_ROWS)
                .min(MAX_QUERY_RESULT_ROWS);
            let truncated = row_count > cap;
            let mut touched_predicates = BTreeSet::new();
            let rows: Vec<_> = all_rows
                .iter()
                .take(cap)
                .map(|row| {
                    let mut obj = serde_json::Map::new();
                    for (var, value) in row {
                        if let EngineValue::String(s) = value {
                            if s.starts_with("http://") || s.starts_with("https://") {
                                touched_predicates.insert(s.clone());
                            }
                        }
                        obj.insert(var.clone(), engine_value_to_json(value));
                    }
                    obj
                })
                .collect();
            QueryPreviewResult {
                ok: true,
                query_kind: kind.into(),
                row_count,
                truncated,
                returned_rows: rows.len(),
                rows,
                boolean_result: None,
                touched_predicates,
                elapsed_ms: started.elapsed().as_millis() as u64,
            }
        }
        EngineQueryResults::Graph(triples) => {
            let row_count = triples.len();
            let cap = params
                .max_rows
                .unwrap_or(MAX_QUERY_RESULT_ROWS)
                .min(MAX_QUERY_RESULT_ROWS);
            let truncated = row_count > cap;
            let touched_predicates: BTreeSet<String> = triples
                .iter()
                .take(cap)
                .map(|t| t.predicate.clone())
                .collect();
            let rows = triples
                .iter()
                .take(cap)
                .map(|t| {
                    let mut obj = serde_json::Map::new();
                    obj.insert(
                        "subject".into(),
                        serde_json::Value::String(t.subject.clone()),
                    );
                    obj.insert(
                        "predicate".into(),
                        serde_json::Value::String(t.predicate.clone()),
                    );
                    obj.insert(
                        "object".into(),
                        serde_json::Value::String(t.object_value.clone()),
                    );
                    obj
                })
                .collect();
            QueryPreviewResult {
                ok: true,
                query_kind: kind.into(),
                row_count,
                truncated,
                returned_rows: cap.min(row_count),
                rows,
                boolean_result: None,
                touched_predicates,
                elapsed_ms: started.elapsed().as_millis() as u64,
            }
        }
    };
    Ok(response)
}

fn engine_value_to_json(value: &EngineValue) -> serde_json::Value {
    match value {
        EngineValue::Bool(b) => serde_json::Value::Bool(*b),
        EngineValue::Int(i) => serde_json::Value::from(*i),
        EngineValue::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        EngineValue::String(s) => serde_json::Value::String(s.clone()),
    }
}

fn load_graph(root: &Path) -> Result<std::sync::Arc<dyn GraphEngine>, McpError> {
    ggen_engine::project_graph::load_for_query(root)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))
}
