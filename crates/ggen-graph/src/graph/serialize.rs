//! Serialization helpers for writing RDF datasets to various formats.

use crate::GraphError;
use oxigraph::io::{RdfFormat, RdfSerializer};
use oxigraph::model::{GraphNameRef, Quad};
use std::io::Write;

/// Serializes a slice of quads into a writer using the specified format.
///
/// For triple formats (Turtle, NTriples, N3) this uses `dump_graph_to_writer`
/// targeting the default graph, because `dump_to_writer` requires a dataset
/// format (TriG, NQuads). Quads loaded from Turtle/NTriples always land in the
/// default graph, so no data is lost.
pub fn serialize_to_writer<W: Write>(
    quads: &[Quad], format: RdfFormat, mut writer: W,
) -> Result<(), GraphError> {
    let temp_store = oxigraph::store::Store::new().map_err(GraphError::Oxigraph)?;
    for quad in quads {
        temp_store.insert(quad).map_err(GraphError::Oxigraph)?;
    }
    if format.supports_datasets() {
        // Dataset formats: TriG, NQuads — dump everything including named graphs
        temp_store
            .dump_to_writer(RdfSerializer::from_format(format), &mut writer)
            .map_err(|e| GraphError::Serialization(e.to_string()))?;
    } else {
        // Triple formats: Turtle, NTriples, etc. — dump only the default graph
        temp_store
            .dump_graph_to_writer(
                GraphNameRef::DefaultGraph,
                RdfSerializer::from_format(format),
                &mut writer,
            )
            .map_err(|e| GraphError::Serialization(e.to_string()))?;
    }
    Ok(())
}

/// Serializes a slice of quads into a String using the specified format.
pub fn serialize_to_string(quads: &[Quad], format: RdfFormat) -> Result<String, GraphError> {
    let mut buffer = Vec::new();
    serialize_to_writer(quads, format, &mut buffer)?;
    String::from_utf8(buffer).map_err(|e| GraphError::Serialization(e.to_string()))
}
