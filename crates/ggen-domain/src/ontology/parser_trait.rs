//! Ontology Parser Trait Abstraction
//!
//! This module defines a unified trait for parsing RDF/OWL ontologies,
//! allowing runtime selection between different parser implementations.

use ggen_utils::error::Result;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;

/// Triple represents an RDF triple (subject, predicate, object)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Triple {
    pub subject: String,
    pub predicate: String,
    pub object: String,
}

/// Class represents an extracted OWL/RDFS class definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub uri: String,
    pub label: Option<String>,
    pub comment: Option<String>,
    pub properties: Vec<Property>,
}

/// Property represents an OWL/RDFS property definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub uri: String,
    pub label: Option<String>,
    pub domain: Option<String>,
    pub range: Option<String>,
}

/// Parse statistics for telemetry
#[derive(Debug, Clone, Default)]
pub struct ParseStats {
    pub triples_parsed: usize,
    pub classes_extracted: usize,
    pub properties_extracted: usize,
    pub parse_duration_ms: u128,
}

/// Unified trait for ontology parsing
pub trait OntologyParser: Send + Sync {
    /// Parse RDF triples from a file
    fn parse_file(
        &self,
        path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>>;

    /// Parse RDF triples from a string
    fn parse_str(
        &self,
        content: &str,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>>;

    /// Extract OWL/RDFS classes from ontology file
    fn extract_classes(
        &self,
        path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Class>>> + Send + '_>>;

    /// Get parsing statistics
    fn stats(&self) -> ParseStats;
}
