//! ARM 3: `ggen reverse templates` — discovered code → Tera template candidates.
//!
//! Conservative, explicitly heuristic v1: each `disco:DiscoveredService` →
//! one parameterized Tera `.tmpl` + a matching SELECT `.rq` + a
//! `bindings.json` contract. It enforces the *reverse* of GGEN-TPL-001 at
//! generation time: every template binding must be produced by the sibling
//! SELECT over the discovered graph (no orphan template variables). Each
//! candidate is additionally render-checked against the real graph before it
//! is written — correct-by-construction, fail-closed.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use heck::ToSnakeCase;
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use regex::Regex;
use serde::Serialize;

use crate::graph::Graph;
use crate::utils::error::{Error, Result};

use super::events::{activity, obj_type, ReverseEvent};
use super::receipt::ReverseReceipt;
use super::vocab;

/// The explicit, checkable binding contract written alongside each candidate.
#[derive(Debug, Clone, Serialize)]
pub struct BindingContract {
    /// Service the candidate was inferred from.
    pub service: String,
    /// Template file name.
    pub template: String,
    /// SELECT query file name.
    pub query: String,
    /// Top-level template bindings.
    pub bindings: Vec<String>,
    /// Per-field bindings consumed inside the `fields` loop.
    pub field_bindings: Vec<String>,
    /// Whether the forward emission rule should iterate (false: single struct).
    pub iterate: bool,
}

/// A single emitted template candidate.
#[derive(Debug, Clone)]
pub struct TemplateCandidate {
    /// Service name.
    pub service: String,
    /// Path to the `.tmpl`.
    pub template_path: PathBuf,
    /// Path to the `.select.rq`.
    pub query_path: PathBuf,
    /// Path to the `.bindings.json`.
    pub bindings_path: PathBuf,
    /// Top-level bindings declared by the candidate.
    pub bindings: Vec<String>,
}

/// Result of a `ggen reverse templates` run.
#[derive(Debug, Clone)]
pub struct ReverseTemplatesReport {
    /// Emitted candidates (one per discovered service).
    pub candidates: Vec<TemplateCandidate>,
    /// Path to the provenance receipt.
    pub receipt_path: PathBuf,
    /// Neutral OCEL events for the CLI to log.
    pub events: Vec<ReverseEvent>,
}

/// Infer Tera template candidates from a discovered authority graph.
///
/// Fails loudly if the graph is missing, contains no discovered services, or if
/// any candidate would have an orphan binding / fails to render against the
/// real graph.
pub fn infer_candidates(
    project_root: &Path,
    discovered_ttl: &Path,
    out_dir: &Path,
) -> Result<ReverseTemplatesReport> {
    if !discovered_ttl.exists() {
        return Err(Error::new(&format!(
            "discovered graph not found: {}",
            discovered_ttl.display()
        )));
    }
    let ttl = std::fs::read_to_string(discovered_ttl)
        .map_err(|e| Error::new(&format!("read discovered graph: {e}")))?;
    let graph = Graph::load_from_string(&ttl)?;

    let mut events = vec![ReverseEvent::new(
        activity::TEMPLATES_STARTED,
        "candidates",
        obj_type::TEMPLATE_CANDIDATE,
    )];

    let svc_query = format!(
        "PREFIX disco: <{ns}>\nSELECT ?name WHERE {{ ?svc a disco:DiscoveredService ; disco:serviceName ?name . }} ORDER BY ?name",
        ns = vocab::DISCO_NS
    );
    let svc_rows = select_rows(&graph, &svc_query)?;
    if svc_rows.is_empty() {
        return Err(Error::new(
            "no disco:DiscoveredService with disco:serviceName in the discovered graph",
        ));
    }

    std::fs::create_dir_all(out_dir).map_err(|e| Error::new(&format!("create out dir: {e}")))?;

    let mut receipt = ReverseReceipt::new("reverse-templates");
    let discovered_key = discovered_ttl
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("graph.ttl");
    receipt.add_input(&format!("discovered:{discovered_key}"), ttl.as_bytes());

    let mut candidates = Vec::new();
    for row in &svc_rows {
        let Some(name) = row.get("name") else {
            continue;
        };
        let snake = name.to_snake_case();
        let template = template_source();
        let query = query_source(name);

        // GGEN-TPL-001 dual: every template binding must be produced by SELECT.
        check_bindings(&template, &query)?;
        // Runtime proof: the query runs against the graph and the template renders.
        render_round_trip(&graph, &query, name)?;

        let template_file = format!("{snake}.rs.tmpl");
        let query_file = format!("{snake}.select.rq");
        let bindings_file = format!("{snake}.bindings.json");

        let contract = BindingContract {
            service: name.clone(),
            template: template_file.clone(),
            query: query_file.clone(),
            bindings: vec!["name".to_string(), "fields".to_string()],
            field_bindings: vec!["field_name".to_string(), "field_type".to_string()],
            iterate: false,
        };
        let contract_json = serde_json::to_string_pretty(&contract)
            .map_err(|e| Error::new(&format!("serialize bindings: {e}")))?;

        let template_path = out_dir.join(&template_file);
        let query_path = out_dir.join(&query_file);
        let bindings_path = out_dir.join(&bindings_file);
        std::fs::write(&template_path, template.as_bytes())
            .map_err(|e| Error::new(&format!("write template: {e}")))?;
        std::fs::write(&query_path, query.as_bytes())
            .map_err(|e| Error::new(&format!("write query: {e}")))?;
        std::fs::write(&bindings_path, contract_json.as_bytes())
            .map_err(|e| Error::new(&format!("write bindings: {e}")))?;

        receipt.add_output(&template_file, template.as_bytes());
        receipt.add_output(&query_file, query.as_bytes());
        receipt.add_output(&bindings_file, contract_json.as_bytes());

        events.push(
            ReverseEvent::new(activity::TEMPLATE_CANDIDATE, name, obj_type::TEMPLATE_CANDIDATE)
                .with_attr("template", template_file),
        );
        candidates.push(TemplateCandidate {
            service: name.clone(),
            template_path,
            query_path,
            bindings_path,
            bindings: contract.bindings,
        });
    }

    let receipt_path = project_root
        .join(".ggen")
        .join("receipts")
        .join(format!("reverse-templates-{}.json", receipt.operation_id));
    receipt.write_to(&receipt_path)?;

    events.push(
        ReverseEvent::new(
            activity::TEMPLATES_COMPLETED,
            "candidates",
            obj_type::TEMPLATE_CANDIDATE,
        )
        .with_attr("candidates", candidates.len().to_string()),
    );
    events.push(
        ReverseEvent::new(
            activity::RECEIPT_EMITTED,
            &receipt.operation_id,
            obj_type::RECEIPT,
        )
        .with_attr("path", relative_path(project_root, &receipt_path)),
    );

    Ok(ReverseTemplatesReport {
        candidates,
        receipt_path,
        events,
    })
}

/// The generic, parameterized template body (identical for every service; the
/// per-service binding happens in the sibling query).
fn template_source() -> String {
    r#"// Candidate template inferred by `ggen reverse templates` (heuristic, v1).
// Round-trips against the discovered authority graph: every binding below is
// produced by the sibling `.select.rq` (the reverse of GGEN-TPL-001).
pub struct {{ name }} {
{% for field in fields %}    pub {{ field.field_name }}: {{ field.field_type }},
{% endfor %}}
"#
    .to_string()
}

/// The SELECT query whose projected variables are exactly the template's
/// terminal bindings (`name`, `field_name`, `field_type`). Explicit projection
/// (no `SELECT *`) and `ORDER BY` keep it strict-mode clean.
fn query_source(name: &str) -> String {
    let template = r#"PREFIX disco: <DISCO_NS>
PREFIX code: <CODE_NS>
SELECT ?name ?field_name ?field_type
WHERE {
  ?svc a disco:DiscoveredService ;
       disco:serviceName ?name .
  OPTIONAL {
    ?svc code:hasField ?f .
    ?f code:fieldName ?field_name ;
       code:fieldType ?field_type .
  }
  FILTER(?name = "SERVICE_NAME")
}
ORDER BY ?name ?field_name
"#;
    template
        .replace("DISCO_NS", vocab::DISCO_NS)
        .replace("CODE_NS", vocab::CODE_NS)
        .replace("SERVICE_NAME", name)
}

/// The reverse GGEN-TPL-001 law: every terminal template binding must be a
/// variable the SELECT projects. Returns `Err` listing any orphan bindings.
pub fn check_bindings(template: &str, query: &str) -> Result<()> {
    let needed = template_terminals(template)?;
    let projected = query_projected(query)?;
    let orphans: Vec<String> = needed.difference(&projected).cloned().collect();
    if !orphans.is_empty() {
        return Err(Error::new(&format!(
            "template binding(s) not produced by SELECT (GGEN-TPL-001 dual): {}",
            orphans.join(", ")
        )));
    }
    Ok(())
}

/// Terminal (query-bound) variables a template consumes, resolving loop aliases
/// (`field.field_name` → `field_name`) and excluding loop collections.
fn template_terminals(template: &str) -> Result<BTreeSet<String>> {
    let for_re = Regex::new(r"\{%-?\s*for\s+(\w+)\s+in\s+([\w.]+)")
        .map_err(|e| Error::new(&format!("for-loop regex: {e}")))?;
    let mut loop_vars = BTreeSet::new();
    let mut collections = BTreeSet::new();
    for cap in for_re.captures_iter(template) {
        loop_vars.insert(cap[1].to_string());
        collections.insert(cap[2].to_string());
    }

    let expr_re = Regex::new(r"\{\{-?\s*([A-Za-z_][\w.]*)")
        .map_err(|e| Error::new(&format!("expr regex: {e}")))?;
    let mut needed = BTreeSet::new();
    for cap in expr_re.captures_iter(template) {
        let path = cap[1].to_string();
        if let Some((base, attr)) = path.split_once('.') {
            if loop_vars.contains(base) {
                needed.insert(attr.to_string());
            } else {
                needed.insert(base.to_string());
            }
        } else if !collections.contains(&path) && !loop_vars.contains(&path) {
            needed.insert(path);
        }
    }
    Ok(needed)
}

/// Variables projected by the SELECT clause.
fn query_projected(query: &str) -> Result<BTreeSet<String>> {
    let select_start = query
        .find("SELECT")
        .ok_or_else(|| Error::new("inferred query missing SELECT"))?;
    let where_off = query[select_start..]
        .find("WHERE")
        .map_or(query.len(), |i| select_start + i);
    let select_clause = &query[select_start..where_off];
    let var_re =
        Regex::new(r"\?(\w+)").map_err(|e| Error::new(&format!("select var regex: {e}")))?;
    let mut projected = BTreeSet::new();
    for cap in var_re.captures_iter(select_clause) {
        projected.insert(cap[1].to_string());
    }
    Ok(projected)
}

/// Execute the candidate query against the discovered graph and render the
/// template with the results — proving the candidate genuinely round-trips.
fn render_round_trip(graph: &Graph, query: &str, name: &str) -> Result<String> {
    let rows = select_rows(graph, query)?;
    if rows.is_empty() {
        return Err(Error::new(&format!(
            "inferred query returned no rows for service {name}"
        )));
    }
    let svc_name = rows
        .iter()
        .find_map(|r| r.get("name"))
        .cloned()
        .ok_or_else(|| Error::new("inferred query did not bind ?name"))?;

    let mut fields = Vec::new();
    for r in &rows {
        if let (Some(fname), Some(ftype)) = (r.get("field_name"), r.get("field_type")) {
            let mut m = BTreeMap::new();
            m.insert("field_name".to_string(), fname.clone());
            m.insert("field_type".to_string(), ftype.clone());
            fields.push(m);
        }
    }

    let mut ctx = tera::Context::new();
    ctx.insert("name", &svc_name);
    ctx.insert("fields", &fields);
    let rendered = tera::Tera::one_off(&template_source(), &ctx, false)
        .map_err(|e| Error::new(&format!("candidate template render failed: {e}")))?;
    if !rendered.contains(&svc_name) {
        return Err(Error::new(
            "rendered candidate does not contain the service name",
        ));
    }
    Ok(rendered)
}

/// Run a SELECT and return rows of clean (unquoted) variable → value strings.
pub fn select_rows(graph: &Graph, sparql: &str) -> Result<Vec<BTreeMap<String, String>>> {
    let results = graph.query(sparql)?;
    let mut rows = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let solution =
                solution.map_err(|e| Error::new(&format!("SPARQL solution error: {e}")))?;
            let mut row = BTreeMap::new();
            for (var, term) in solution.iter() {
                row.insert(
                    var.as_str().trim_start_matches('?').to_string(),
                    clean_term(term),
                );
            }
            rows.push(row);
        }
    } else {
        return Err(Error::new("inferred query did not return SELECT solutions"));
    }
    Ok(rows)
}

/// The lexical value of a term, without RDF quoting/angle brackets.
fn clean_term(term: &Term) -> String {
    match term {
        Term::Literal(lit) => lit.value().to_string(),
        Term::NamedNode(n) => n.as_str().to_string(),
        Term::BlankNode(b) => b.as_str().to_string(),
        Term::Triple(_) => term.to_string(),
    }
}

fn relative_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_bindings_accepts_aligned_template() {
        let t = template_source();
        let q = query_source("Foo");
        check_bindings(&t, &q).expect("aligned template/query must pass");
    }

    #[test]
    fn check_bindings_rejects_orphan_variable() {
        // A template var the query does not project must be rejected (teeth).
        let broken_template = "pub struct {{ name }} { {{ ghost }} }";
        let q = query_source("Foo"); // projects name, field_name, field_type — not ghost
        let result = check_bindings(broken_template, &q);
        assert!(result.is_err(), "orphan binding `ghost` must be rejected");
    }

    #[test]
    fn template_terminals_resolve_loop_aliases() {
        let terminals = template_terminals(&template_source()).expect("parse");
        assert!(terminals.contains("name"));
        assert!(terminals.contains("field_name"));
        assert!(terminals.contains("field_type"));
        // `fields` is a loop collection, not a query column.
        assert!(!terminals.contains("fields"));
    }
}
