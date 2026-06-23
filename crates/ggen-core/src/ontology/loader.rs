//! Ontology Loader
//!
//! Unified interface for loading ontologies from either:
//! 1. Core bundle (embedded at compile time, zero-copy, always available)
//! 2. Marketplace (downloaded on-demand, cached locally)
//! 3. Local filesystem (for development and testing)

use crate::ontology::resolver::OntologyResolver;
use std::path::Path;

/// Ontology loading strategy with fallback chain
#[derive(Debug, Clone, Copy)]
pub struct OntologyLoader;

impl OntologyLoader {
    /// Load ontology content using fallback chain:
    /// 1. Check core bundle (embedded, zero-copy)
    /// 2. Check local filesystem
    /// 3. Fall back to marketplace resolver (download if needed)
    ///
    /// # Arguments
    ///
    /// * `uri` - Namespace URI (e.g., "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    /// * `base_path` - Base path for relative file lookups
    ///
    /// # Returns
    ///
    /// Ontology content as bytes, or None if not found/unavailable
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let rdf_content = OntologyLoader::load_content(
    ///     "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    ///     Path::new(".")
    /// )?;
    /// assert!(!rdf_content.is_empty());
    /// ```
    pub fn load_content(uri: &str, base_path: &Path) -> Option<Vec<u8>> {
        // Try core bundle first (embedded, zero-copy, always available)
        if let Some(ontology) = crate::ontology::CoreOntologyBundle::by_namespace(uri) {
            return Some(ontology.content.to_vec());
        }

        // Try core bundle by simple name (fallback for partial matches)
        // E.g., "owl" matches "http://www.w3.org/2002/07/owl#"
        if let Some(ontology) = crate::ontology::CoreOntologyBundle::by_name(uri) {
            return Some(ontology.content.to_vec());
        }

        // Try local filesystem resolver (for development)
        let resolved_paths = OntologyResolver::resolve(Path::new(uri), base_path);
        for path in resolved_paths {
            if path.exists() {
                if let Ok(content) = std::fs::read(&path) {
                    return Some(content);
                }
            }
        }

        // Future: Try marketplace resolver (download and cache)
        // This would integrate with ggen-marketplace package registry
        // TODO: Implement marketplace fallback
        // let package = marketplace::find_ontology_package(uri)?;
        // return marketplace::install_and_load(package);

        None
    }

    /// Get ontology metadata for a namespace URI
    ///
    /// Returns metadata about the ontology if available in core bundle
    pub fn get_metadata(uri: &str) -> Option<&'static crate::ontology::OntologyMetadata> {
        crate::ontology::CoreOntologyBundle::by_namespace(uri)
            .or_else(|| crate::ontology::CoreOntologyBundle::by_name(uri))
    }

    /// Check if ontology is available in core bundle (zero-copy access)
    ///
    /// This is useful for determining whether network access is required
    pub fn is_embedded(uri: &str) -> bool {
        crate::ontology::CoreOntologyBundle::by_namespace(uri).is_some()
            || crate::ontology::CoreOntologyBundle::by_name(uri).is_some()
    }

    /// List all embedded ontologies available without network access
    pub fn list_embedded() -> Vec<(&'static str, &'static str)> {
        crate::ontology::CoreOntologyBundle::available()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_core_ontology() {
        let content = OntologyLoader::load_content(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            Path::new("."),
        );
        assert!(content.is_some(), "RDF ontology should be available in core bundle");
        assert!(!content.unwrap().is_empty(), "Ontology content should not be empty");
    }

    #[test]
    fn test_is_embedded() {
        assert!(OntologyLoader::is_embedded(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        ));
        assert!(OntologyLoader::is_embedded("owl"));
    }

    #[test]
    fn test_get_metadata() {
        let meta = OntologyLoader::get_metadata("http://www.w3.org/2002/07/owl#");
        assert!(meta.is_some());

        if let Some(m) = meta {
            assert_eq!(m.name, "owl");
            assert!(!m.content.is_empty());
        }
    }

    #[test]
    fn test_list_embedded() {
        let embedded = OntologyLoader::list_embedded();
        assert!(!embedded.is_empty());

        // Verify all entries have valid URIs
        for (name, uri) in embedded {
            assert!(!name.is_empty(), "Name should not be empty");
            assert!(
                uri.starts_with("http") || uri.starts_with("https"),
                "URI should start with http/https"
            );
        }
    }

    #[test]
    fn test_nonexistent_ontology() {
        let content =
            OntologyLoader::load_content("http://example.com/nonexistent#", Path::new("."));
        // Should return None, not panic
        // In the future, this might try marketplace fallback
        let _ = content; // Suppress unused warning
    }
}
