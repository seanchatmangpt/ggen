//! Phase 2: public-ontology alignment admission gate.
//!
//! The PR/FAQ substrate: a domain graph must *compose real public industry
//! vocabulary*, not invent it. This gate admits a domain graph only if every
//! term it uses from a vendored public namespace (PROV-O, SOSA, QUDT, …) is
//! actually **defined** in the vendored public ontologies under
//! `.specify/ontology/vendored/`. A reference to a fabricated public term (e.g.
//! `prov:Flibbertigibbet`) is reported as *unaligned* and refuses admission —
//! the same "no fabrication" discipline the reverse pipeline applies to code.

use std::collections::BTreeSet;
use std::path::Path;

use crate::graph::Graph;
use crate::utils::error::{Error, Result};

use super::templates::select_rows;
use super::vocab;

/// Public namespaces a domain graph may compose from. Each must be backed by a
/// vendored ontology under the public dir, or terms in it cannot be admitted.
pub const PUBLIC_NAMESPACES: &[&str] = &[
    vocab::PROV_NS,
    vocab::SOSA_NS,
    vocab::QUDT_NS,
    vocab::OCEL_NS,
    vocab::FIBO_NS,
];

/// Outcome of an alignment check.
#[derive(Debug, Clone)]
pub struct AlignmentReport {
    /// Public-namespace IRIs the domain uses that ARE defined in the vendored ontologies.
    pub admitted: Vec<String>,
    /// Public-namespace IRIs the domain uses that are NOT defined (fabricated / unaligned).
    pub unaligned: Vec<String>,
}

impl AlignmentReport {
    /// Whether the domain graph is fully aligned (composes only real public terms).
    pub fn is_aligned(&self) -> bool {
        self.unaligned.is_empty()
    }
}

/// Check that `domain_ttl` composes only public terms defined under `public_dir`.
///
/// Fails loudly if the domain graph is missing, the public dir is absent, or it
/// contains no vendored ontologies to admit against.
pub fn check_alignment(domain_ttl: &Path, public_dir: &Path) -> Result<AlignmentReport> {
    if !domain_ttl.exists() {
        return Err(Error::new(&format!(
            "domain graph not found: {}",
            domain_ttl.display()
        )));
    }
    if !public_dir.is_dir() {
        return Err(Error::new(&format!(
            "public ontology dir not found: {}",
            public_dir.display()
        )));
    }

    // Load all vendored public ontologies (sorted, deterministic) into one graph.
    let public_graph = Graph::new()?;
    let mut files: Vec<_> = std::fs::read_dir(public_dir)
        .map_err(|e| Error::new(&format!("read public dir: {e}")))?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|x| x.to_str()) == Some("ttl"))
        .collect();
    files.sort();
    for path in &files {
        let ttl = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("read {}: {e}", path.display())))?;
        public_graph.insert_turtle(&ttl)?;
    }
    if files.is_empty() {
        return Err(Error::new(
            "no public ontology .ttl files to admit against",
        ));
    }

    // A public term is "defined" iff it appears as a subject in the vendored set.
    let defined = collect_public_iris(&public_graph, true)?;

    let domain_str = std::fs::read_to_string(domain_ttl)
        .map_err(|e| Error::new(&format!("read domain graph: {e}")))?;
    let domain_graph = Graph::load_from_string(&domain_str)?;
    // The domain "uses" a public IRI in any position (subject/predicate/object).
    let used = collect_public_iris(&domain_graph, false)?;

    let unaligned: Vec<String> = used.difference(&defined).cloned().collect();
    let admitted: Vec<String> = used.intersection(&defined).cloned().collect();
    Ok(AlignmentReport { admitted, unaligned })
}

/// Collect public-namespace IRIs from `graph`. When `subjects_only`, only the
/// subject position counts (used to enumerate *defined* terms); otherwise all
/// three positions count (used to enumerate *used* terms).
fn collect_public_iris(graph: &Graph, subjects_only: bool) -> Result<BTreeSet<String>> {
    let sparql = if subjects_only {
        "SELECT DISTINCT ?s WHERE { ?s ?p ?o }"
    } else {
        "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
    };
    let rows = select_rows(graph, sparql)?;
    let keys: &[&str] = if subjects_only { &["s"] } else { &["s", "p", "o"] };
    let mut set = BTreeSet::new();
    for row in rows {
        for key in keys {
            if let Some(value) = row.get(*key) {
                if in_public_ns(value) {
                    set.insert(value.clone());
                }
            }
        }
    }
    Ok(set)
}

/// Whether `iri` is in one of the admitted public namespaces.
fn in_public_ns(iri: &str) -> bool {
    PUBLIC_NAMESPACES.iter().any(|ns| iri.starts_with(ns))
}
