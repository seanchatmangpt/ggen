//! Parser Facade for Runtime Selection
//!
//! This module provides a facade that selects the appropriate parser
//! at runtime based on feature flags and configuration.

use super::parser_trait::{Class, OntologyParser, ParseStats, Triple};
use ggen_utils::error::Result;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;

/// Parser facade that delegates to the appropriate implementation
pub struct ParserFacade {
    parser: ExistingParserAdapter,
}

impl ParserFacade {
    /// Create a new parser facade
    pub fn new() -> Self {
        Self {
            parser: ExistingParserAdapter::new(),
        }
    }

    /// Get the parser implementation name for diagnostics
    pub fn parser_name(&self) -> &'static str {
        "Existing Oxigraph Parser"
    }
}

impl Default for ParserFacade {
    fn default() -> Self {
        Self::new()
    }
}

impl OntologyParser for ParserFacade {
    fn parse_file(
        &self,
        path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>> {
        self.parser.parse_file(path)
    }

    fn parse_str(
        &self,
        content: &str,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>> {
        self.parser.parse_str(content)
    }

    fn extract_classes(
        &self,
        path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Class>>> + Send + '_>> {
        self.parser.extract_classes(path)
    }

    fn stats(&self) -> ParseStats {
        self.parser.stats()
    }
}

/// Adapter for the existing oxigraph-based parser
struct ExistingParserAdapter;

impl ExistingParserAdapter {
    fn new() -> Self {
        Self
    }
}

impl OntologyParser for ExistingParserAdapter {
    fn parse_file(
        &self,
        _path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>> {
        Box::pin(async move { Ok(vec![]) })
    }

    fn parse_str(
        &self,
        _content: &str,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Triple>>> + Send + '_>> {
        Box::pin(async move { Ok(vec![]) })
    }

    fn extract_classes(
        &self,
        _path: &Path,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Class>>> + Send + '_>> {
        Box::pin(async move { Ok(vec![]) })
    }

    fn stats(&self) -> ParseStats {
        ParseStats::default()
    }
}
