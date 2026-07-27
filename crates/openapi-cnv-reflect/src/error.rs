//! Error type for the OpenAPI-to-`cnv:Cli` reflector.

use thiserror::Error;

/// Errors returned by [`crate::reflect::reflect`] and [`crate::write::write_ontology`].
#[derive(Debug, Error)]
pub enum ReflectError {
    /// The document's top-level shape isn't an OpenAPI 3.x object at all
    /// (missing `openapi`/`paths`, or not a JSON object).
    #[error("not a recognizable OpenAPI 3.x document: {0}")]
    NotOpenApi(String),
    /// No operation in the document fell inside the reflector's 80/20 slice
    /// (GET, scalar path/query parameters only) -- every operation was
    /// skipped, so there is nothing to reflect.
    #[error(
        "no reflectable operations found (only GET operations with scalar path/query \
         parameters are supported); see warnings for what was skipped"
    )]
    NothingReflected,
    /// Building the RDF store failed.
    #[error("failed to build RDF store: {0}")]
    Store(String),
    /// Writing the serialized ontology failed.
    #[error("failed to write ontology.ttl: {0}")]
    Io(#[from] std::io::Error),
}
