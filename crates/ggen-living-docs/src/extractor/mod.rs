//! Code extraction and analysis module
//!
//! Extracts code structure and metadata from Rust source files using AST parsing.

mod parser;
mod visitor;

pub use parser::RustParser;
pub use visitor::EntityVisitor;

use crate::{Error, Result, config::ExtractorConfig, ontology::CodeEntity};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use tracing::{debug, info, instrument, warn};

/// Code extractor that analyzes source code and builds semantic entities
pub struct CodeExtractor {
    config: ExtractorConfig,
    parser: RustParser,
}

impl CodeExtractor {
    /// Create a new code extractor
    pub fn new(config: &ExtractorConfig) -> Result<Self> {
        Ok(Self {
            config: config.clone(),
            parser: RustParser::new(),
        })
    }

    /// Extract code entities from a directory
    #[instrument(skip(self))]
    pub async fn extract_from_directory(&self, dir: impl AsRef<Path>) -> Result<Vec<CodeEntity>> {
        info!("Extracting code entities from directory: {:?}", dir.as_ref());

        let mut entities = Vec::new();
        let dir_path = dir.as_ref();

        // Walk through directory and find Rust files
        for entry in WalkDir::new(dir_path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();

            // Check if file matches include patterns and doesn't match exclude patterns
            if !self.should_process_file(path) {
                continue;
            }

            debug!("Processing file: {:?}", path);

            match self.extract_from_file(path).await {
                Ok(mut file_entities) => {
                    debug!("Extracted {} entities from {:?}", file_entities.len(), path);
                    entities.append(&mut file_entities);
                }
                Err(e) => {
                    warn!("Failed to extract from {:?}: {}", path, e);
                }
            }
        }

        info!("Total entities extracted: {}", entities.len());
        Ok(entities)
    }

    /// Extract code entities from a single file
    #[instrument(skip(self))]
    pub async fn extract_from_file(&self, file_path: impl AsRef<Path>) -> Result<Vec<CodeEntity>> {
        let file_path = file_path.as_ref();
        debug!("Extracting from file: {:?}", file_path);

        // Read file content
        let content = tokio::fs::read_to_string(file_path)
            .await
            .map_err(Error::Io)?;

        // Parse the file
        self.parser.parse_file(&content, file_path)
    }

    /// Check if a file should be processed
    fn should_process_file(&self, path: &Path) -> bool {
        // Check if it's a file
        if !path.is_file() {
            return false;
        }

        // Check extension
        if path.extension().and_then(|s| s.to_str()) != Some("rs") {
            return false;
        }

        let path_str = path.to_string_lossy();

        // Check exclude patterns
        for pattern in &self.config.exclude_patterns {
            if self.matches_pattern(&path_str, pattern) {
                return false;
            }
        }

        // Check include patterns (if specified)
        if !self.config.include_patterns.is_empty() {
            let mut matches = false;
            for pattern in &self.config.include_patterns {
                if self.matches_pattern(&path_str, pattern) {
                    matches = true;
                    break;
                }
            }
            if !matches {
                return false;
            }
        }

        true
    }

    /// Simple pattern matching (simplified glob)
    fn matches_pattern(&self, path: &str, pattern: &str) -> bool {
        // Simplified pattern matching - in real implementation use glob crate
        if pattern.contains("**") {
            let parts: Vec<&str> = pattern.split("**").collect();
            if parts.len() == 2 {
                return path.contains(parts[0]) && path.ends_with(parts[1]);
            }
        }
        path.contains(pattern)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_extractor_creation() {
        let config = ExtractorConfig::default();
        let extractor = CodeExtractor::new(&config);
        assert!(extractor.is_ok());
    }
}
