//! SHACL validation reports for ggen membrane projections.
//!
//! Enforces structural, cryptographic, and pedigree constraints on membrane projections.

use std::path::PathBuf;

use crate::graph::Graph;
use crate::utils::error::{Error, Result};
use crate::validation::{SparqlValidator, ValidationResult};

const SHAPES_FILE: &str = "membrane-boundary-crossing.ttl";

/// Load the membrane SHACL shapes from the ontology layer.
///
/// Resolution order:
/// 1. `GGEN_SHAPES_DIR` env var + `/membrane-boundary-crossing.ttl`
/// 2. Relative path `.specify/shapes/membrane-boundary-crossing.ttl`
/// 3. Path relative to `CARGO_MANIFEST_DIR` (for tests): `../../.specify/shapes/membrane-boundary-crossing.ttl`
fn load_membrane_shapes() -> Result<String> {
    let candidates: Vec<PathBuf> = {
        let mut v = Vec::new();

        // 1. GGEN_SHAPES_DIR env var
        if let Ok(dir) = std::env::var("GGEN_SHAPES_DIR") {
            v.push(PathBuf::from(dir).join(SHAPES_FILE));
        }

        // 2. Relative to cwd
        v.push(PathBuf::from(".specify/shapes").join(SHAPES_FILE));

        // 3. Relative to CARGO_MANIFEST_DIR (for tests run from crate directory)
        if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            v.push(
                PathBuf::from(manifest_dir)
                    .join("../../.specify/shapes")
                    .join(SHAPES_FILE),
            );
        }

        v
    };

    for path in &candidates {
        if path.exists() {
            return std::fs::read_to_string(path).map_err(|e| {
                Error::new(&format!(
                    "Failed to read membrane SHACL shapes from {}: {}",
                    path.display(),
                    e
                ))
            });
        }
    }

    Err(Error::new(&format!(
        "membrane SHACL shapes file '{}' not found; searched: {}",
        SHAPES_FILE,
        candidates
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )))
}

/// Validator utility for running SHACL checks on projected membrane graphs
pub struct MembraneShaclValidator;

impl MembraneShaclValidator {
    /// Validate the projected membrane RDF graph against the membrane SHACL shapes
    pub fn validate(projected_graph: &Graph) -> Result<ValidationResult> {
        let shapes_ttl = load_membrane_shapes()?;

        let shapes_graph = Graph::new()?;
        shapes_graph.insert_turtle(&shapes_ttl)?;

        let validator = SparqlValidator::new();
        let report = validator
            .validate(projected_graph, &shapes_graph)
            .map_err(|e| Error::new(&format!("SHACL validation failed: {}", e)))?;

        Ok(report)
    }
}
