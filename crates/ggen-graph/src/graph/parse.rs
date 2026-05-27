//! Parser helpers for reading RDF datasets from various formats.

use crate::GraphError;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Quad;
use std::io::Read;

/// Parse quads from a reader using the specified RDF format.
pub fn parse_from_reader<R: Read>(format: RdfFormat, reader: R) -> Result<Vec<Quad>, GraphError> {
    let temp_store = oxigraph::store::Store::new().map_err(GraphError::Oxigraph)?;
    temp_store
        .load_from_reader(RdfParser::from_format(format), reader)
        .map_err(|e| GraphError::Serialization(e.to_string()))?;

    let mut quads = Vec::new();
    for quad_res in temp_store.quads_for_pattern(None, None, None, None) {
        quads.push(quad_res.map_err(GraphError::Oxigraph)?);
    }
    Ok(quads)
}

/// Parse quads from a string slice in N-Quads format.
pub fn parse_nquads(content: &str) -> Result<Vec<Quad>, GraphError> {
    parse_from_reader(RdfFormat::NQuads, content.as_bytes())
}

/// Parse quads from a string slice in Turtle format.
pub fn parse_turtle(content: &str) -> Result<Vec<Quad>, GraphError> {
    parse_from_reader(RdfFormat::Turtle, content.as_bytes())
}

/// Parse quads from a string slice in N-Triples format.
pub fn parse_ntriples(content: &str) -> Result<Vec<Quad>, GraphError> {
    parse_from_reader(RdfFormat::NTriples, content.as_bytes())
}
