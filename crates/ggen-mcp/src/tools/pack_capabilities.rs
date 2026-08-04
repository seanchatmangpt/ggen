//! `ggen_pack_capabilities` — read-only introspection over a single ggen
//! pack directory (`ontology.ttl` + optional `gates/*.rq` / `shapes.ttl`),
//! answering three questions an agent asks before consuming or extending an
//! unfamiliar pack:
//!
//! 1. What RDF classes/individuals does this pack's ontology actually
//!    declare? (`rdf:type` enumeration, live SPARQL, never a hand-maintained
//!    summary that can drift from the `.ttl`.)
//! 2. What admission gates does it ship, and what does each one check? Reuses
//!    `packs/lsp-max-pack/gates/*.rq`'s own `# MESSAGE: ...` first-line
//!    convention rather than inventing new gate metadata -- a pack author who
//!    already writes that comment gets this tool's summary for free.
//! 3. Does this pack use the CP6 "contract" predicate convention
//!    (`lsp:expectsBinding` / `lsp:producesShape` in lsp-max's own ontology,
//!    generalized here by LOCAL NAME so this tool is not lsp-max-specific)?
//!    Every other existing ggen pack does not use this convention, and the
//!    tool must say so plainly rather than erroring.
//!
//! Deliberately generic: no `lsp:` (or any other pack's) namespace IRI is
//! hardcoded anywhere in this file. Class enumeration matches by `rdf:type`
//! alone; contract-predicate matching is by the predicate IRI's LOCAL NAME
//! (the substring after its last `#` or `/`), which is the only part of a
//! predicate name a pack author picks independent of their own namespace
//! prefix. The default local names it looks for
//! (`DEFAULT_CONTRACT_PREDICATE_LOCAL_NAMES`) are documented below and
//! caller-overridable via `contract_predicate_local_names`.

use std::path::Path;

use ggen_engine::graph::{DeterministicGraph, EngineQueryResults, EngineValue, GraphEngine};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

/// Predicate local names this tool looks for by default when reporting
/// "contract" facts (CP6's convention in `~/lsp-max/ontology/lsp318.ttl`).
/// Not a hardcoded namespace -- matched against any predicate IRI ending in
/// `#<name>` or `/<name>`, regardless of which pack's prefix owns it.
const DEFAULT_CONTRACT_PREDICATE_LOCAL_NAMES: &[&str] = &["expectsBinding", "producesShape"];

#[derive(Debug, Deserialize, JsonSchema)]
pub struct PackCapabilitiesParams {
    /// Directory containing the pack's `ontology.ttl` (and, optionally, a
    /// `gates/` directory of `*.rq` files and/or a `shapes.ttl`). Typically
    /// a `packs/<name>-pack` directory, but this tool imposes no naming
    /// requirement -- any directory with an `ontology.ttl` works.
    pub pack_dir: String,
    /// Predicate local names to check for when reporting "contract" facts
    /// (see module doc). Defaults to
    /// `DEFAULT_CONTRACT_PREDICATE_LOCAL_NAMES` (`expectsBinding`,
    /// `producesShape`) when omitted -- CP6's own convention, generalized by
    /// local name so this tool never hardcodes a namespace.
    #[serde(default)]
    pub contract_predicate_local_names: Option<Vec<String>>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ClassSummary {
    /// Full class IRI as declared via `rdf:type` in the ontology.
    pub class_iri: String,
    /// Count of individuals asserted `a <class_iri>`.
    pub individual_count: usize,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct GateSummary {
    /// File name under `gates/` (e.g. `010_required.rq`).
    pub file_name: String,
    /// The gate's own `# MESSAGE: ...` first-line comment, verbatim minus
    /// the `# MESSAGE: ` prefix -- `packs/lsp-max-pack/gates/*.rq`'s
    /// established convention. `None` when the file's first line does not
    /// use this convention (reported honestly, not fabricated).
    pub message: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ContractPredicateSummary {
    /// The local name searched for (e.g. `"expectsBinding"`).
    pub local_name: String,
    /// Full predicate IRI(s) found in this ontology ending in
    /// `#<local_name>` or `/<local_name>`. Empty means this pack does not
    /// use the convention for this local name -- a normal, expected result
    /// for the large majority of existing ggen packs.
    pub matched_predicate_iris: Vec<String>,
    /// Number of distinct subjects carrying at least one of the matched
    /// predicates.
    pub subject_count: usize,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PackCapabilitiesResult {
    pub ok: bool,
    pub pack_dir: String,
    /// `false` when no `ontology.ttl` was found at all (a real gap, not an
    /// error -- some pack directories are templates-only). All other fields
    /// are empty/default in that case.
    pub has_ontology: bool,
    pub classes: Vec<ClassSummary>,
    /// `true` when a `gates/` directory exists with at least one `*.rq`
    /// file inside it.
    pub has_gates: bool,
    pub gates: Vec<GateSummary>,
    /// `true` when a `shapes.ttl` file exists directly under `pack_dir`.
    pub has_shapes_ttl: bool,
    pub contract_predicates: Vec<ContractPredicateSummary>,
}

/// Report class/individual, gate, and contract-predicate capabilities for
/// the pack at `params.pack_dir`.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `pack_dir`;
/// `ErrorCategory::GraphLoadError` if `ontology.ttl` exists but fails to
/// parse as Turtle or the query engine cannot be constructed;
/// `ErrorCategory::Internal` on a `gates/*.rq` read failure.
pub fn pack_capabilities(
    params: &PackCapabilitiesParams,
) -> Result<PackCapabilitiesResult, McpError> {
    let root = resolve_root(&params.pack_dir)?;

    let ontology_path = root.join("ontology.ttl");
    let has_ontology = ontology_path.is_file();

    let (classes, contract_predicates) = if has_ontology {
        let ttl = std::fs::read_to_string(&ontology_path).map_err(|e| {
            McpError::new(
                ErrorCategory::GraphLoadError,
                format!("{} unreadable: {e}", ontology_path.display()),
            )
        })?;
        let graph = DeterministicGraph::new().map_err(|e| {
            McpError::new(
                ErrorCategory::GraphLoadError,
                format!("failed to construct graph engine: {e}"),
            )
        })?;
        graph.insert_turtle(&ttl).map_err(|e| {
            McpError::new(
                ErrorCategory::GraphLoadError,
                format!("{} failed to parse as Turtle: {e}", ontology_path.display()),
            )
        })?;

        let classes = query_classes(&graph)?;

        let requested_names: Vec<String> = params
            .contract_predicate_local_names
            .clone()
            .unwrap_or_else(|| {
                DEFAULT_CONTRACT_PREDICATE_LOCAL_NAMES
                    .iter()
                    .map(|s| (*s).to_string())
                    .collect()
            });
        let contract_predicates = requested_names
            .into_iter()
            .map(|name| query_contract_predicate(&graph, &name))
            .collect::<Result<Vec<_>, _>>()?;

        (classes, contract_predicates)
    } else {
        (Vec::new(), Vec::new())
    };

    let gates_dir = root.join("gates");
    let (has_gates, gates) = read_gates(&gates_dir)?;

    let has_shapes_ttl = root.join("shapes.ttl").is_file();

    Ok(PackCapabilitiesResult {
        ok: true,
        pack_dir: root.display().to_string(),
        has_ontology,
        classes,
        has_gates,
        gates,
        has_shapes_ttl,
        contract_predicates,
    })
}

fn query_classes(graph: &DeterministicGraph) -> Result<Vec<ClassSummary>, McpError> {
    let sparql = r#"
        SELECT ?class (COUNT(?individual) AS ?n) WHERE {
            ?individual a ?class .
        }
        GROUP BY ?class
        ORDER BY ?class
    "#;
    let results = GraphEngine::query(graph, sparql)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;
    let EngineQueryResults::Solutions(rows) = results else {
        return Err(McpError::new(
            ErrorCategory::Internal,
            "class-enumeration query did not return solutions",
        ));
    };
    let mut classes: Vec<ClassSummary> = rows
        .iter()
        .filter_map(|row| {
            let class_iri = match row.get("class")? {
                EngineValue::String(s) => s.clone(),
                _ => return None,
            };
            let individual_count = match row.get("n") {
                Some(EngineValue::Int(n)) => usize::try_from(*n).unwrap_or(0),
                _ => 0,
            };
            Some(ClassSummary {
                class_iri,
                individual_count,
            })
        })
        .collect();
    classes.sort_by(|a, b| a.class_iri.cmp(&b.class_iri));
    Ok(classes)
}

/// Query for any predicate IRI ending in `#<local_name>` or `/<local_name>`,
/// generic across every pack's own namespace prefix (see module doc). Uses
/// two `STRENDS` disjuncts rather than a single regex so no external regex
/// engine dependency is introduced for a check this simple.
fn query_contract_predicate(
    graph: &DeterministicGraph, local_name: &str,
) -> Result<ContractPredicateSummary, McpError> {
    // `local_name` comes from either our own hardcoded default list or a
    // caller-supplied JSON string list -- never interpolated as SPARQL
    // syntax, only as a quoted string literal, so no injection surface here
    // beyond what any other string-literal-bound SPARQL query already has.
    let suffix_hash = format!("#{local_name}");
    let suffix_slash = format!("/{local_name}");
    let sparql = format!(
        r#"
        SELECT ?s ?p WHERE {{
            ?s ?p ?o .
            FILTER (STRENDS(STR(?p), {suffix_hash:?}) || STRENDS(STR(?p), {suffix_slash:?}))
        }}
        "#
    );
    let results = GraphEngine::query(graph, &sparql)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;
    let EngineQueryResults::Solutions(rows) = results else {
        return Err(McpError::new(
            ErrorCategory::Internal,
            "contract-predicate query did not return solutions",
        ));
    };
    let mut matched_predicate_iris = std::collections::BTreeSet::new();
    let mut subjects = std::collections::BTreeSet::new();
    for row in &rows {
        if let Some(EngineValue::String(p)) = row.get("p") {
            matched_predicate_iris.insert(p.clone());
        }
        if let Some(EngineValue::String(s)) = row.get("s") {
            subjects.insert(s.clone());
        }
    }
    Ok(ContractPredicateSummary {
        local_name: local_name.to_string(),
        matched_predicate_iris: matched_predicate_iris.into_iter().collect(),
        subject_count: subjects.len(),
    })
}

fn read_gates(gates_dir: &Path) -> Result<(bool, Vec<GateSummary>), McpError> {
    if !gates_dir.is_dir() {
        return Ok((false, Vec::new()));
    }
    let entries = std::fs::read_dir(gates_dir).map_err(|e| {
        McpError::new(
            ErrorCategory::Internal,
            format!("{} unreadable: {e}", gates_dir.display()),
        )
    })?;
    let mut gates = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| {
            McpError::new(
                ErrorCategory::Internal,
                format!("{} directory entry unreadable: {e}", gates_dir.display()),
            )
        })?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("rq") {
            continue;
        }
        let file_name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        let content = std::fs::read_to_string(&path).map_err(|e| {
            McpError::new(
                ErrorCategory::Internal,
                format!("{} unreadable: {e}", path.display()),
            )
        })?;
        let first_line = content.lines().next().unwrap_or("");
        let message = first_line
            .strip_prefix("# MESSAGE:")
            .map(|rest| rest.trim().to_string());
        gates.push(GateSummary { file_name, message });
    }
    gates.sort_by(|a, b| a.file_name.cmp(&b.file_name));
    let has_gates = !gates.is_empty();
    Ok((has_gates, gates))
}
