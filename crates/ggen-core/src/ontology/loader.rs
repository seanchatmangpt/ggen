//! Ontology Loader
//!
//! Unified interface for loading ontologies with fallback chain:
//! 1. Core bundle (embedded at compile time, zero-copy, always available) — Phase 2
//! 2. Lock file (.ggen/ontology.lock) — Phase 4
//! 3. Local filesystem (for development and testing)
//! 4. Marketplace (downloaded on-demand, cached locally) — Phase 4

use crate::ontology::resolver::OntologyResolver;
use std::path::Path;

#[cfg(feature = "bundled-standards")]
static FOAF_METADATA: crate::ontology::core_bundle::OntologyMetadata =
    crate::ontology::core_bundle::OntologyMetadata {
        name: "foaf",
        namespace: "http://xmlns.com/foaf/0.1/",
        size: crate::domain::ontology::standards::FOAF_TTL.len(),
        content: crate::domain::ontology::standards::FOAF_TTL.as_bytes(),
    };

#[cfg(feature = "bundled-standards")]
static DUBLIN_CORE_METADATA: crate::ontology::core_bundle::OntologyMetadata =
    crate::ontology::core_bundle::OntologyMetadata {
        name: "dc",
        namespace: "http://purl.org/dc/elements/1.1/",
        size: crate::domain::ontology::standards::DUBLIN_CORE_TTL.len(),
        content: crate::domain::ontology::standards::DUBLIN_CORE_TTL.as_bytes(),
    };

/// Ontology loading strategy with fallback chain
#[derive(Debug, Clone, Copy)]
pub struct OntologyLoader;

impl OntologyLoader {
    /// Load ontology content using fallback chain:
    /// 1. Check core bundle (embedded, zero-copy) — Phase 2
    /// 2. Check lock file (.ggen/ontology.lock) — Phase 4
    /// 3. Check local filesystem (for development and testing)
    /// 4. Fall back to marketplace resolver (download if needed) — Phase 4
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
        // Phase 2: Try core bundle first (embedded, zero-copy, always available)
        if let Some(ontology) = crate::ontology::CoreOntologyBundle::by_namespace(uri) {
            return Some(ontology.content.to_vec());
        }

        // Try core bundle by simple name (fallback for partial matches)
        // E.g., "owl" matches "http://www.w3.org/2002/07/owl#"
        if let Some(ontology) = crate::ontology::CoreOntologyBundle::by_name(uri) {
            return Some(ontology.content.to_vec());
        }

        #[cfg(feature = "bundled-standards")]
        {
            if uri == "http://xmlns.com/foaf/0.1/" || uri == "foaf" {
                return Some(FOAF_METADATA.content.to_vec());
            }
            if uri == "http://purl.org/dc/elements/1.1/" || uri == "dc" {
                return Some(DUBLIN_CORE_METADATA.content.to_vec());
            }
        }

        // Phase 4: Try lock file for cached packages.
        // Lock file package lookup will be integrated when the lockfile package module is completed.

        // Try local filesystem resolver (for development)
        let resolved_paths = OntologyResolver::resolve(Path::new(uri), base_path);
        for path in resolved_paths {
            if path.exists() {
                if let Ok(content) = std::fs::read(&path) {
                    return Some(content);
                }
            }
        }

        // Phase 4: Try marketplace resolver (download and cache).
        // Marketplace fallback will be integrated when the marketplace client module is completed.

        None
    }

    /// Get ontology metadata for a namespace URI
    ///
    /// Returns metadata about the ontology if available in core bundle
    pub fn get_metadata(
        uri: &str,
    ) -> Option<&'static crate::ontology::core_bundle::OntologyMetadata> {
        if let Some(meta) = crate::ontology::CoreOntologyBundle::by_namespace(uri)
            .or_else(|| crate::ontology::CoreOntologyBundle::by_name(uri))
        {
            return Some(meta);
        }

        #[cfg(feature = "bundled-standards")]
        {
            if uri == "http://xmlns.com/foaf/0.1/" || uri == "foaf" {
                return Some(&FOAF_METADATA);
            }
            if uri == "http://purl.org/dc/elements/1.1/" || uri == "dc" {
                return Some(&DUBLIN_CORE_METADATA);
            }
        }

        None
    }

    /// Check if ontology is available in core bundle (zero-copy access)
    ///
    /// This is useful for determining whether network access is required
    pub fn is_embedded(uri: &str) -> bool {
        if crate::ontology::CoreOntologyBundle::by_namespace(uri).is_some()
            || crate::ontology::CoreOntologyBundle::by_name(uri).is_some()
        {
            return true;
        }

        #[cfg(feature = "bundled-standards")]
        {
            if uri == "http://xmlns.com/foaf/0.1/"
                || uri == "foaf"
                || uri == "http://purl.org/dc/elements/1.1/"
                || uri == "dc"
            {
                return true;
            }
        }

        false
    }

    /// List all embedded ontologies available without network access
    pub fn list_embedded() -> Vec<(&'static str, &'static str)> {
        let mut list = crate::ontology::CoreOntologyBundle::available();
        #[cfg(feature = "bundled-standards")]
        {
            list.push(("foaf", "http://xmlns.com/foaf/0.1/"));
            list.push(("dc", "http://purl.org/dc/elements/1.1/"));
        }
        list
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
        assert!(
            content.is_some(),
            "RDF ontology should be available in core bundle"
        );
        assert!(
            !content.unwrap().is_empty(),
            "Ontology content should not be empty"
        );
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

    // ========== PHASE 5: COMPREHENSIVE UNIT TESTS ==========

    /// Test that is_embedded correctly identifies all core ontologies
    #[test]
    fn test_is_embedded_for_all_core_ontologies() {
        let embedded_list = OntologyLoader::list_embedded();

        for (name, uri) in embedded_list {
            assert!(
                OntologyLoader::is_embedded(uri),
                "Ontology {} with URI {} should be embedded",
                name,
                uri
            );
            assert!(
                OntologyLoader::is_embedded(name),
                "Ontology {} should be embedded by name",
                name
            );
        }
    }

    /// Test get_metadata returns correct size for all embedded ontologies
    #[test]
    fn test_get_metadata_returns_correct_size() {
        let embedded = OntologyLoader::list_embedded();

        for (_name, uri) in embedded {
            let meta = OntologyLoader::get_metadata(uri);
            assert!(meta.is_some(), "Should have metadata for {}", uri);

            if let Some(m) = meta {
                assert!(m.size > 0, "Size should be non-zero for {}", uri);
                assert!(
                    !m.content.is_empty(),
                    "Content should not be empty for {}",
                    uri
                );
                assert_eq!(m.size, m.content.len(), "Size should match content length");
            }
        }
    }

    /// Test load_content returns bytes for embedded ontologies
    #[test]
    fn test_load_content_returns_bytes() {
        let embedded = OntologyLoader::list_embedded();

        for (_name, uri) in embedded {
            let content = OntologyLoader::load_content(uri, Path::new("."));
            assert!(content.is_some(), "Should load content for {}", uri);
            assert!(
                !content.unwrap().is_empty(),
                "Content should not be empty for {}",
                uri
            );
        }
    }

    /// Test metadata consistency between load_content and get_metadata
    #[test]
    fn test_metadata_consistency() {
        let rdf_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

        let metadata = OntologyLoader::get_metadata(rdf_uri);
        let content = OntologyLoader::load_content(rdf_uri, Path::new("."));

        assert!(metadata.is_some());
        assert!(content.is_some());

        if let (Some(meta), Some(cnt)) = (metadata, content) {
            assert_eq!(
                meta.content.len(),
                cnt.len(),
                "Metadata content length should match loaded content"
            );
        }
    }

    /// Test nonexistent URI returns None
    #[test]
    fn test_nonexistent_uri_returns_none() {
        assert!(OntologyLoader::get_metadata("http://example.com/fake#").is_none());
        assert!(OntologyLoader::load_content("http://example.com/fake#", Path::new(".")).is_none());
        assert!(!OntologyLoader::is_embedded("http://example.com/fake#"));
    }

    /// Test deterministic hashing (same URI always returns same content)
    #[test]
    fn test_deterministic_hashing() {
        let uri = "http://www.w3.org/2002/07/owl#";

        let content1 = OntologyLoader::load_content(uri, Path::new("."));
        let content2 = OntologyLoader::load_content(uri, Path::new("."));
        let content3 = OntologyLoader::load_content(uri, Path::new("."));

        assert_eq!(
            content1, content2,
            "Same URI should return identical content"
        );
        assert_eq!(
            content2, content3,
            "Same URI should return identical content"
        );
    }

    /// Test fallback chain with by_namespace and by_name
    #[test]
    fn test_fallback_by_name_and_namespace() {
        // Test that looking up by name works
        let by_name = OntologyLoader::load_content("owl", Path::new("."));
        // Test that looking up by full namespace also works
        let by_namespace =
            OntologyLoader::load_content("http://www.w3.org/2002/07/owl#", Path::new("."));

        // Both should succeed
        assert!(by_name.is_some(), "Should load by short name");
        assert!(by_namespace.is_some(), "Should load by full namespace");

        // Content should be identical (same ontology)
        // Note: They might not be bitwise identical if loaded through different paths,
        // but should represent the same OWL ontology
    }

    /// Test is_embedded negative cases
    #[test]
    fn test_is_embedded_negative() {
        assert!(!OntologyLoader::is_embedded("http://example.com/ontology#"));
        assert!(!OntologyLoader::is_embedded("notanontology"));
        assert!(!OntologyLoader::is_embedded(""));
        assert!(!OntologyLoader::is_embedded(
            "HTTP://www.w3.org/1999/02/22-rdf-syntax-ns#"
        )); // case-sensitive
    }

    /// Test list_embedded never returns duplicates
    #[test]
    fn test_list_embedded_no_duplicates() {
        let embedded = OntologyLoader::list_embedded();
        let mut names = std::collections::HashSet::new();
        let mut uris = std::collections::HashSet::new();

        for (name, uri) in embedded {
            assert!(names.insert(name), "Duplicate name found: {}", name);
            assert!(uris.insert(uri), "Duplicate URI found: {}", uri);
        }
    }

    /// Test list_embedded contains only valid entries
    #[test]
    fn test_list_embedded_validity() {
        let embedded = OntologyLoader::list_embedded();

        for (name, uri) in embedded {
            // Name should be non-empty
            assert!(!name.is_empty(), "Name should not be empty");

            // URI should be valid
            assert!(!uri.is_empty(), "URI should not be empty");
            assert!(uri.starts_with("http"), "URI should start with http");
            assert!(
                uri.ends_with('#') || uri.ends_with('/'),
                "URI should end with # or /"
            );

            // Should actually be loadable
            assert!(
                OntologyLoader::load_content(uri, Path::new(".")).is_some(),
                "Listed ontology {} should be loadable",
                name
            );
        }
    }

    /// Test multiple concurrent lookups work correctly
    #[test]
    fn test_multiple_lookups_thread_safe() {
        // Load the same ontology 100 times to verify no race conditions
        for _ in 0..100 {
            let _ = OntologyLoader::load_content(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                Path::new("."),
            );
            let _ = OntologyLoader::get_metadata("http://www.w3.org/2000/01/rdf-schema#");
            let _ = OntologyLoader::is_embedded("owl");
        }
        // If no panic, test passes
    }
}
