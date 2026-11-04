//! SHACL admission court for ggen-sjira — Rust port of
//! `GgenIgniter.SemanticJira.Shacl` (ggen_igniter
//! `lib/ggen_igniter/semantic_jira/shacl.ex`, 736L).
//!
//! Reuse ruling (PG-2 Slice 5, ds-3): ggen-graph already ships a real
//! Oxigraph-backed SHACL validator (`ggen-graph/src/shacl.rs:74`
//! `validate_shacl(data, &[shape_graphs])`) — this module is an ADAPTER, not a
//! validator. It reads the pack's own shapes file from disk
//! (`shapes/work-order.shacl.ttl`), runs the real ggen-graph validator, and
//! maps its [`ggen_graph::ShaclViolation`] to the `{shape, focus, path}`
//! triple the Elixir court reports, with the typed refusal
//! [`ShaclRefusal::ShaclViolations`] mirroring Elixir's
//! `{:error, {:shacl_violations, violations}}`.
//!
//! Disclosed divergence: ggen-graph's `validate_shacl` evaluates the
//! `sh:minCount`-over-`sh:targetClass` subset (its own doc names the subset
//! ggen enforces at sync time). The Elixir court additionally supports
//! `sh:pattern`/`sh:datatype`/`sh:closed`/SPARQL constraints and fails closed
//! on unknown constructs; those are UNSUPPORTED here (no Rust analog behind
//! `validate_shacl`) — follow-on: extend ggen-graph's `validate_shacl` or add
//! a constraint walk in this adapter.

use std::path::Path;

use ggen_graph::ShaclViolation as GraphViolation;

/// The pack's own SHACL shape file path, relative to a pack directory —
/// `pack_shapes_path/0` (shacl.ex:92).
pub const PACK_SHAPES_PATH: &str = "shapes/work-order.shacl.ttl";

/// One SHACL constraint violation against a focus node — the
/// `{shape, focus, path}` triple the Elixir court reports.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Violation {
    /// The shape IRI that was violated.
    pub shape: String,
    /// The instance IRI that violated the shape.
    pub focus: String,
    /// The property path (IRI) the constraint applies to.
    pub path: String,
    /// Human-readable explanation (from the real validator).
    pub message: String,
}

/// Typed refusal — mirrors `{:error, {:shacl_violations, violations}}`
/// (shacl.ex `run/2`).
#[derive(Debug, thiserror::Error)]
pub enum ShaclRefusal {
    /// The data graph violates the pack's shapes; each violation names
    /// `{shape, focus, path}`.
    #[error("REFUSED:SHACL_VIOLATIONS {} violation(s): {}", .0.len(), format_violations(.0))]
    ShaclViolations(Vec<Violation>),

    /// The shapes file or the data could not be read/parsed.
    #[error("REFUSED:SHACL_IO {0}")]
    Io(String),
}

fn format_violations(vs: &[Violation]) -> String {
    vs.iter()
        .map(|v| format!("shape={} focus={} path={}", v.shape, v.focus, v.path))
        .collect::<Vec<_>>()
        .join("; ")
}

/// Gate-shaped entry (shacl.ex `run/2`): validates the Turtle data at
/// `data_path` against `<pack_dir>/shapes/work-order.shacl.ttl` through the
/// real ggen-graph validator.
///
/// Conformance is `Ok(vec![])`; any violation is
/// [`ShaclRefusal::ShaclViolations`]. Parse/IO failures are
/// [`ShaclRefusal::Io`].
pub fn run(pack_dir: &Path, data_path: &Path) -> Result<Vec<Violation>, ShaclRefusal> {
    validate_work_order(data_path, &pack_dir.join(PACK_SHAPES_PATH))
}

/// Validates `data_path` (Turtle) against the shapes file at `shapes_path`
/// via `ggen_graph::validate_shacl`. The `Violation` triple `{shape, focus,
/// path}` is the Elixir court's result mapping.
pub fn validate_work_order(
    data_path: &Path,
    shapes_path: &Path,
) -> Result<Vec<Violation>, ShaclRefusal> {
    let data = std::fs::read_to_string(data_path)
        .map_err(|e| ShaclRefusal::Io(format!("{}: {e}", data_path.display())))?;
    let shapes = std::fs::read_to_string(shapes_path)
        .map_err(|e| ShaclRefusal::Io(refusal_io(shapes_path, e)))?;

    let violations: Vec<GraphViolation> = ggen_graph::validate_shacl(&data, &[&shapes])
        .map_err(|e| ShaclRefusal::Io(format!("SHACL parse: {e}")))?;

    if violations.is_empty() {
        Ok(Vec::new())
    } else {
        Err(ShaclRefusal::ShaclViolations(
            violations
                .into_iter()
                .map(|v| Violation {
                    shape: v.shape,
                    focus: v.focus,
                    path: v.path,
                    message: v.message,
                })
                .collect(),
        ))
    }
}

fn refusal_io(p: &Path, e: std::io::Error) -> String {
    format!("{}: {e}", p.display())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shapes() -> String {
        r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix sj: <http://example.org/sj#> .
        sj:WorkOrderShape a sh:NodeShape ;
            sh:targetClass sj:WorkOrder ;
            sh:property [ sh:path sj:targetPack ; sh:minCount 1 ] .
        "#
        .to_string()
    }

    fn data(pack: &str) -> String {
        format!(
            r#"
            @prefix sj: <http://example.org/sj#> .
            <http://example.org/wo/1> a sj:WorkOrder ; sj:targetPack "{pack}" .
            "#
        )
    }

    #[test]
    fn conforming_work_order_has_no_violations() {
        let dir = tempfile::tempdir().unwrap();
        let shapes_path = dir.path().join("work-order.shacl.ttl");
        let data_path = dir.path().join("ontology.ttl");
        std::fs::write(&shapes_path, shapes()).unwrap();
        std::fs::write(&data_path, data("semantic-jira-pack")).unwrap();
        assert!(validate_work_order(&data_path, &shapes_path).unwrap().is_empty());
    }

    #[test]
    fn missing_target_pack_flips_the_court() {
        let dir = tempfile::tempdir().unwrap();
        let shapes_path = dir.path().join("work-work.shacl.ttl");
        let data_path = dir.path().join("ontology.ttl");
        std::fs::write(&shapes_path, shapes()).unwrap();
        std::fs::write(&data_path, "<http://example.org/wo/2> a <http://example.org/sj#WorkOrder> .").unwrap();
        let err = validate_work_order(&data_path, &shapes_path).unwrap_err();
        let ShaclRefusal::ShaclViolations(vs) = &err else {
            panic!("expected ShaclViolations, got {err:?}");
        };
        assert_eq!(vs.len(), 1);
        assert_eq!(vs[0].path, "http://example.org/sj#targetPack");
    }

    #[test]
    fn missing_shapes_file_is_an_io_refusal() {
        let dir = tempfile::tempdir().unwrap();
        let data_path = dir.path().join("ontology.ttl");
        std::fs::write(&data_path, "").unwrap();
        let err = validate_work_order(&data_path, &dir.path().join("nope.ttl")).unwrap_err();
        assert!(matches!(err, ShaclRefusal::Io(_)), "got {err:?}");
    }
}
