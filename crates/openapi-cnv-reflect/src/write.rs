//! Serialize a reflected [`oxigraph::store::Store`] to an `ontology.ttl` file.
//!
//! N-Triples is a strict subset of Turtle syntax (full IRIs, one triple per
//! line, no prefixes) -- `crates/ggen-engine/src/graph.rs`'s
//! `DeterministicGraph::insert_turtle` loads via `RdfFormat::Turtle`, which
//! accepts N-Triples input unchanged. Emitting N-Triples avoids needing to
//! track prefix declarations here at all.

use std::path::Path;

use oxigraph::io::RdfFormat;
use oxigraph::model::GraphNameRef;
use oxigraph::store::Store;

use crate::error::ReflectError;

/// Serialize every default-graph triple in `store` and write it to
/// `output_path` as N-Triples (valid Turtle).
///
/// # Errors
/// Returns [`ReflectError::Io`] if writing the destination file fails, or
/// [`ReflectError::Store`] if serialization itself fails.
pub fn write_ontology(store: &Store, output_path: &Path) -> Result<(), ReflectError> {
    let mut buffer = Vec::new();
    store
        .dump_graph_to_writer(GraphNameRef::DefaultGraph, RdfFormat::NTriples, &mut buffer)
        .map_err(|e| ReflectError::Store(e.to_string()))?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(output_path, buffer)?;
    Ok(())
}
