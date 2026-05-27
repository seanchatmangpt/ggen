//! RDF Quad representation and formatting.

use crate::GraphError;
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Quad, Term};

/// Formatting helper to get a canonical string representation of a Quad.
pub fn canonical_quad_string(quad: &Quad) -> String {
    // A stable representation of the quad is its standard string representation.
    // Oxigraph's Quad::to_string() writes it in N-Quads format.
    quad.to_string()
}

/// Parses an N-Quad string into an Oxigraph `Quad`.
///
/// # Errors
///
/// Returns `GraphError::Serialization` if parsing fails or does not yield exactly one quad.
pub fn parse_nquad(nquad: &str) -> Result<Quad, GraphError> {
    let trimmed = nquad.trim();
    let formatted = if trimmed.ends_with('.') {
        trimmed.to_string()
    } else {
        format!("{} .", trimmed)
    };

    let temp_store = oxigraph::store::Store::new().map_err(GraphError::Oxigraph)?;
    let cursor = std::io::Cursor::new(formatted.as_bytes());
    temp_store
        .load_from_reader(
            oxigraph::io::RdfParser::from_format(oxigraph::io::RdfFormat::NQuads),
            cursor,
        )
        .map_err(|e| GraphError::Serialization(e.to_string()))?;

    let mut quads = Vec::new();
    for quad_res in temp_store.quads_for_pattern(None, None, None, None) {
        quads.push(quad_res.map_err(GraphError::Oxigraph)?);
    }

    if quads.is_empty() {
        return Err(GraphError::Serialization(
            "No quads found in N-Quad string".to_string(),
        ));
    }
    if quads.len() > 1 {
        return Err(GraphError::Serialization(
            "Multiple quads found in N-Quad string".to_string(),
        ));
    }
    Ok(quads[0].clone())
}

/// Helper struct for constructing a Quad.
pub struct QuadBuilder {
    subject: Option<NamedOrBlankNode>,
    predicate: Option<NamedNode>,
    object: Option<Term>,
    graph_name: Option<GraphName>,
}

impl Default for QuadBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl QuadBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            subject: None,
            predicate: None,
            object: None,
            graph_name: None,
        }
    }

    /// Set the subject of the quad.
    pub fn subject(mut self, subject: NamedOrBlankNode) -> Self {
        self.subject = Some(subject);
        self
    }

    /// Set the predicate of the quad.
    pub fn predicate(mut self, predicate: NamedNode) -> Self {
        self.predicate = Some(predicate);
        self
    }

    /// Set the object of the quad.
    pub fn object(mut self, object: Term) -> Self {
        self.object = Some(object);
        self
    }

    /// Set the graph name of the quad.
    pub fn graph_name(mut self, graph_name: GraphName) -> Self {
        self.graph_name = Some(graph_name);
        self
    }

    /// Build the quad. Returns an error if subject, predicate or object are missing.
    pub fn build(self) -> Result<Quad, GraphError> {
        let s = self
            .subject
            .ok_or_else(|| GraphError::Serialization("Missing subject".to_string()))?;
        let p = self
            .predicate
            .ok_or_else(|| GraphError::Serialization("Missing predicate".to_string()))?;
        let o = self
            .object
            .ok_or_else(|| GraphError::Serialization("Missing object".to_string()))?;
        let g = self.graph_name.unwrap_or(GraphName::DefaultGraph);
        Ok(Quad::new(s, p, o, g))
    }
}
