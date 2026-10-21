//! Typed reader for the `vc:` mutant-catalog graph.
//!
//! Reuses the ggen-graph SPARQL extraction pattern
//! ([`crate::graph::DeterministicGraph`] + engine-neutral
//! [`crate::graph::EngineQueryResults`]). Every required field is matched
//! in the graph pattern itself: a subject missing any required field
//! simply produces no row, and a catalog with zero or ambiguous rows is
//! [`crate::court::CourtError::CatalogMalformed`] — a required field is
//! never defaulted.

use oxigraph::model::{Term, Variable};
use oxigraph::sparql::{QueryResults, QuerySolution};

use crate::court::{CourtError, CourtResult};
use crate::graph::DeterministicGraph;

/// RDF namespace of the vacuity-court schema (L10).
pub const VC_NS: &str = "http://ggen.org/vacuity#";

/// One compiled mutant catalog: the identity of the mutation a court must
/// survive-or-refuse.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MutantCatalog {
    /// Gate this mutant evidences (`vc:gateId`, e.g. `KD-C7`).
    pub gate_id: String,
    /// Root-relative target file the shim patches (`vc:target`).
    pub target: String,
    /// Mutation operator kind (`vc:mutantKind` — L10 enum:
    /// `return-default`, `flip-condition`, `drop-guard`, ...).
    pub mutant_kind: String,
    /// The shim patch text (unified diff or full replacement marker,
    /// `vc:shimPatch`).
    pub shim_patch: String,
    /// Expected SHA-256 hex of the target *after* the shim lands
    /// (`vc:expectedPostHash`). The runner refuses unless the post-shim
    /// digest matches — the mutation cannot silently miss.
    pub expected_post_hash: String,
    /// Optional shim authoring timestamp, unix millis (`vc:shimTs`).
    pub shim_ts: Option<u64>,
    /// Optional witness-receipt timestamp, unix millis (`vc:witnessTs`).
    pub witness_ts: Option<u64>,
}

const CATALOG_QUERY: &str = r"
PREFIX vc: <http://ggen.org/vacuity#>
SELECT ?gate ?target ?kind ?patch ?hash ?shimTs ?witnessTs WHERE {
  ?m a vc:Mutant ;
     vc:gateId ?gate ;
     vc:target ?target ;
     vc:mutantKind ?kind ;
     vc:shimPatch ?patch ;
     vc:expectedPostHash ?hash .
  OPTIONAL { ?m vc:shimTs ?shimTs }
  OPTIONAL { ?m vc:witnessTs ?witnessTs }
}
";

/// Read and validate one mutant catalog from Turtle text.
///
/// # Errors
/// [`crate::court::CourtError::CatalogMalformed`] when the Turtle cannot
/// be parsed, zero rows match (a required field is missing somewhere), or
/// more than one mutant subject is present (ambiguous catalog).
pub fn parse_mutant_catalog(ttl: &str) -> CourtResult<MutantCatalog> {
    let graph = DeterministicGraph::new()
        .map_err(|e| CourtError::CatalogMalformed(format!("graph init failed: {e}")))?;
    graph.insert_turtle(ttl).map_err(|e| {
        CourtError::CatalogMalformed(format!("catalog is not parseable Turtle: {e}"))
    })?;

    let rows: Vec<QuerySolution> = match graph.query(CATALOG_QUERY) {
        Ok(QueryResults::Solutions(solutions)) => solutions
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|e| CourtError::CatalogMalformed(format!("solution iteration failed: {e}")))?,
        Ok(QueryResults::Boolean(_) | QueryResults::Graph(_)) => {
            return Err(CourtError::CatalogMalformed(
                "catalog query unexpectedly returned a non-SELECT shape".to_string(),
            ));
        }
        Err(e) => {
            return Err(CourtError::CatalogMalformed(format!(
                "catalog query failed: {e}"
            )));
        }
    };

    if rows.is_empty() {
        return Err(CourtError::CatalogMalformed(
            "zero vc:Mutant subjects with all required fields (gateId, target, mutantKind, \
             shimPatch, expectedPostHash) — a required field is missing and is never defaulted"
                .to_string(),
        ));
    }
    if rows.len() > 1 {
        return Err(CourtError::CatalogMalformed(format!(
            "{} vc:Mutant subjects found — a court compiles exactly one mutant; split the catalog",
            rows.len()
        )));
    }

    let row = &rows[0];
    let required = |name: &str| -> CourtResult<String> {
        match row.get(Variable::new_unchecked(name)) {
            Some(Term::Literal(l)) if !l.value().is_empty() => Ok(l.value().to_string()),
            Some(Term::Literal(_)) => Err(CourtError::CatalogMalformed(format!(
                "field `{name}` is present but empty — never defaulted"
            ))),
            other => Err(CourtError::CatalogMalformed(format!(
                "field `{name}` unbound or non-literal in solution row (got {other:?}) — \
                 never defaulted"
            ))),
        }
    };

    let gate_id = required("gate")?;
    let target = required("target")?;
    let mutant_kind = required("kind")?;
    let shim_patch = required("patch")?;
    let expected_post_hash = required("hash")?;

    let ts = |name: &str| -> CourtResult<Option<u64>> {
        match row.get(Variable::new_unchecked(name)) {
            None => Ok(None),
            Some(Term::Literal(l)) => l.value().parse::<u64>().map(Some).map_err(|_| {
                CourtError::CatalogMalformed(format!(
                    "field `{name}` must be an xsd:integer unix-millis timestamp, got `{}`",
                    l.value()
                ))
            }),
            Some(other) => Err(CourtError::CatalogMalformed(format!(
                "field `{name}` must be an xsd:integer unix-millis timestamp, got {other:?}"
            ))),
        }
    };
    let shim_ts = ts("shimTs")?;
    let witness_ts = ts("witnessTs")?;

    Ok(MutantCatalog {
        gate_id,
        target,
        mutant_kind,
        shim_patch,
        expected_post_hash,
        shim_ts,
        witness_ts,
    })
}

/// Read one mutant catalog from a Turtle file on disk.
///
/// # Errors
/// [`crate::court::CourtError::Io`] when the file cannot be read;
/// [`crate::court::CourtError::CatalogMalformed`] per
/// [`parse_mutant_catalog`].
pub fn read_mutant_catalog(path: &std::path::Path) -> CourtResult<MutantCatalog> {
    let ttl = std::fs::read_to_string(path).map_err(|e| CourtError::Io {
        path: path.display().to_string(),
        detail: e.to_string(),
    })?;
    parse_mutant_catalog(&ttl)
}

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)] // test-only, H05 class (a)

    use super::*;

    const WELL_FORMED: &str = r#"
        @prefix vc: <http://ggen.org/vacuity#> .
        <http://ggen.org/vacuity#mutant-kd-c7> a vc:Mutant ;
            vc:gateId "KD-C7" ;
            vc:target "lib/gate.ex" ;
            vc:mutantKind "flip-condition" ;
            vc:shimPatch "--- a/lib/gate.ex\n+++ b/lib/gate.ex\n" ;
            vc:expectedPostHash "abc123" ;
            vc:shimTs 1000 ;
            vc:witnessTs 2000 .
    "#;

    #[test]
    fn well_formed_catalog_parses_all_fields() {
        let catalog = parse_mutant_catalog(WELL_FORMED).expect("well-formed catalog");
        assert_eq!(catalog.gate_id, "KD-C7");
        assert_eq!(catalog.target, "lib/gate.ex");
        assert_eq!(catalog.mutant_kind, "flip-condition");
        assert_eq!(catalog.expected_post_hash, "abc123");
        assert_eq!(catalog.shim_ts, Some(1000));
        assert_eq!(catalog.witness_ts, Some(2000));
    }

    #[test]
    fn missing_required_field_is_malformed_never_defaulted() {
        let missing_hash = WELL_FORMED.replace(" vc:expectedPostHash \"abc123\" ;\n", "");
        let err = parse_mutant_catalog(&missing_hash).expect_err("must refuse");
        assert!(matches!(err, CourtError::CatalogMalformed(_)), "{err}");
    }

    #[test]
    fn unparseable_turtle_is_malformed() {
        let err = parse_mutant_catalog("this is not turtle @@@").expect_err("must refuse");
        assert!(matches!(err, CourtError::CatalogMalformed(_)), "{err}");
    }
}
