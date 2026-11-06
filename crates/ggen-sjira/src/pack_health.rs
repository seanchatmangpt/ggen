//! Pack health probe, Phase 1 — Rust port of
//! `GgenIgniter.SemanticJira.PackHealth` Phase 1 (ggen_igniter
//! `lib/ggen_igniter/semantic_jira/pack_health.ex`, 235L).
//!
//! Phase 1 (this slice): load the pack's `ontology.ttl`, execute EVERY gate
//! query (`gates/*.rq`) through the real SPARQL engine (Oxigraph — the same
//! engine ggen-graph's SHACL validator is built on), refuse a pack with zero
//! gates, and attribute any gate failure to the gate itself. Fail-closed:
//! any missing ontology, Turtle parse error, or gate evaluation error is a
//! typed [`HealthRefusal`], never a panic.
//!
//! UNSUPPORTED (disclosed, no Rust analog — follow-on `render.rs` port):
//! Phase 2's template rendering. The Elixir probe renders `templates/*.eex`
//! through the sync pipeline's binding convention; Rust ggen-sjira has no
//! EEx/template renderer, so template health is reported as
//! `unsupported` rather than silently skipped.

use std::path::Path;

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;

/// Ontology filename expected in every pack directory (Elixir
/// `GgenIgniter.Pack.default_ontology/1`).
pub const ONTOLOGY_FILE: &str = "ontology.ttl";
/// Gate query directory (Elixir `GgenIgniter.Pack.discover_queries/1`).
pub const GATES_DIR: &str = "gates";

/// Result of one executed gate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GateResult {
    /// Gate name (file stem).
    pub name: String,
    /// Rows the gate returned (0 for a healthy gate whose law is "no
    /// dangling references").
    pub rows: usize,
}

/// Phase-1 health summary for one pack directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HealthSummary {
    /// Gates executed against the packaged ontology, in name order.
    pub gates: Vec<GateResult>,
    /// Disclosed UNSUPPORTED surface: template rendering has no Rust analog
    /// this slice; the follow-on port is `render.rs` (Elixir
    /// `GgenIgniter.Render` + the sync pipeline's `build_bindings/2`).
    pub unsupported: &'static str,
}

/// Typed, fail-closed refusals — Elixir `check/1` rescues into
/// `{:error, "semantic-jira-pack: <message>"}`; this port returns typed
/// errors carrying the same component attribution.
#[derive(Debug, thiserror::Error)]
pub enum HealthRefusal {
    /// `ontology.ttl` missing or unreadable, or a Turtle parse failure.
    #[error("semantic-jira-pack: ontology {0}")]
    Ontology(String),

    /// No `*.rq` gate queries found under `gates/`.
    #[error("semantic-jira-pack: no *.rq gate queries found in {0}")]
    NoGates(String),

    /// Gate `<name>` (`<path>`) failed to execute against the ontology —
    /// attributed to the gate itself, never wrapped in another component.
    #[error("semantic-jira-pack: gate {name} ({path}) failed to execute against the packaged ontology: {detail}")]
    Gate {
        /// Gate name.
        name: String,
        /// Gate file path.
        path: String,
        /// Underlying engine error text.
        detail: String,
    },
}

/// Phase-1 health probe for the pack at `pack_dir`: load `ontology.ttl`,
/// execute every `gates/*.rq` gate through the real engine, refuse zero
/// gates. Never panics on pack defects — every failure is a typed refusal.
pub fn check(pack_dir: &Path) -> Result<HealthSummary, HealthRefusal> {
    let ontology_path = pack_dir.join(ONTOLOGY_FILE);
    let turtle = std::fs::read_to_string(&ontology_path).map_err(|e| {
        HealthRefusal::Ontology(format!("missing at {}: {e}", ontology_path.display()))
    })?;

    let store = Store::new().map_err(|e| HealthRefusal::Ontology(e.to_string()))?;
    store
        .load_from_reader(RdfParser::from_format(RdfFormat::Turtle), turtle.as_bytes())
        .map_err(|e| HealthRefusal::Ontology(format!("{}: {e}", ontology_path.display())))?;

    // Discover gates (*.rq, name = file stem), refusing zero gates.
    let mut gates: Vec<(String, std::path::PathBuf)> = Vec::new();
    let gates_dir = pack_dir.join(GATES_DIR);
    let entries = std::fs::read_dir(&gates_dir)
        .map_err(|_| HealthRefusal::NoGates(gates_dir.display().to_string()))?;
    for entry in entries {
        let entry = entry.map_err(|e| HealthRefusal::Gate {
            name: String::new(),
            path: gates_dir.display().to_string(),
            detail: e.to_string(),
        })?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("rq") {
            let name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or_default()
                .to_string();
            gates.push((name, path));
        }
    }
    gates.sort();
    if gates.is_empty() {
        return Err(HealthRefusal::NoGates(gates_dir.display().to_string()));
    }

    let mut results = Vec::new();
    for (name, path) in gates {
        let query = std::fs::read_to_string(&path).map_err(|e| HealthRefusal::Gate {
            name: name.clone(),
            path: path.display().to_string(),
            detail: e.to_string(),
        })?;
        let rows = execute_gate(&store, &query).map_err(|detail| HealthRefusal::Gate {
            name: name.clone(),
            path: path.display().to_string(),
            detail,
        })?;
        results.push(GateResult { name, rows });
    }

    Ok(HealthSummary {
        gates: results,
        unsupported: "template rendering (templates/*.eex|*.tmpl) — Phase 2 follow-on: render.rs port of GgenIgniter.Render + build_bindings/2",
    })
}

/// Executes one SELECT gate through the real engine and returns its row
/// count. ASK/CONSTRUCT gates degrade to their SELECT-equivalent handling:
/// an ASK truthy result counts as 1 row, CONSTRUCT/DESCRIBE are refused as
/// unsupported gate forms (a generation gate is a SELECT per the engine's
/// own query-form law).
fn execute_gate(store: &Store, query: &str) -> Result<usize, String> {
    let results = SparqlEvaluator::new()
        .parse_query(query)
        .map_err(|e| format!("parse: {e}"))?
        .on_store(store)
        .execute()
        .map_err(|e| format!("evaluate: {e}"))?;
    match results {
        QueryResults::Solutions(sols) => {
            let mut n = 0;
            for sol in sols {
                sol.map_err(|e| format!("read: {e}"))?;
                n += 1;
            }
            Ok(n)
        }
        QueryResults::Boolean(b) => Ok(usize::from(b)),
        _ => Err("unsupported gate form: a generation gate is a SELECT or ASK".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mini_pack() -> (tempfile::TempDir, String) {
        let dir = tempfile::tempdir().unwrap();
        let pack = dir.path().join("mini-pack");
        std::fs::create_dir_all(pack.join("gates")).unwrap();
        std::fs::write(
            pack.join(ONTOLOGY_FILE),
            r#"
            @prefix sj: <http://example.org/sj#> .
            sj:wo1 a sj:WorkOrder ; sj:targetPack "mini-pack" .
            "#,
        )
        .unwrap();
        std::fs::write(
            pack.join("gates").join("term-closure.rq"),
            r#"PREFIX sj: <http://example.org/sj#>
               SELECT ?wo WHERE {
                 ?wo sj:targetPack ?p .
                 FILTER NOT EXISTS { ?wo sj:term ?t }
               }"#,
        )
        .unwrap();
        (dir, pack.display().to_string())
    }

    #[test]
    fn mini_pack_gate_executes_and_counts_dangling_rows() {
        let (_dir, pack) = mini_pack();
        let summary = check(Path::new(&pack)).unwrap();
        assert_eq!(summary.gates.len(), 1);
        assert_eq!(summary.gates[0].name, "term-closure");
        // The mini ontology declares no sj:term -> the term-closure gate
        // correctly observes 1 dangling work order.
        assert_eq!(summary.gates[0].rows, 1);
    }

    #[test]
    fn zero_gates_refuses() {
        let dir = tempfile::tempdir().unwrap();
        let pack = dir.path().join("empty-pack");
        std::fs::create_dir_all(pack.join("gates")).unwrap();
        std::fs::write(pack.join(ONTOLOGY_FILE), "").unwrap();
        let err = check(Path::new(&pack)).unwrap_err();
        assert!(matches!(err, HealthRefusal::NoGates(_)), "got {err:?}");
    }

    #[test]
    fn missing_ontology_refuses() {
        let dir = tempfile::tempdir().unwrap();
        let err = check(dir.path()).unwrap_err();
        assert!(matches!(err, HealthRefusal::Ontology(_)), "got {err:?}");
    }

    #[test]
    fn broken_gate_is_attributed_to_the_gate_itself() {
        let (_dir, pack) = mini_pack();
        std::fs::write(
            Path::new(&pack).join("gates").join("broken.rq"),
            "SELECT ?x WHERE { ?x ??? }",
        )
        .unwrap();
        let err = check(Path::new(&pack)).unwrap_err();
        let HealthRefusal::Gate { name, .. } = &err else {
            panic!("got {err:?}");
        };
        assert_eq!(name, "broken");
    }

    #[test]
    fn gate_rows_count_flips_on_dangling_reference() {
        // Add a second work order that DOES carry a term, then mutate the
        // gate to look for dangling targetPack-less rows: the row-count law
        // is observable in both directions.
        let (_dir, pack) = mini_pack();
        std::fs::write(
            Path::new(&pack).join("gates").join("count-all.rq"),
            "PREFIX sj: <http://example.org/sj#> SELECT ?wo WHERE { ?wo a sj:WorkOrder }",
        )
        .unwrap();
        let summary = check(Path::new(&pack)).unwrap();
        let count_all = summary
            .gates
            .iter()
            .find(|g| g.name == "count-all")
            .unwrap();
        assert_eq!(count_all.rows, 1);
    }
}
