//! Ontology Loader
//!
//! Unified interface for loading ontologies with fallback chain:
//! 1. Core bundle (embedded at compile time, zero-copy, always available)
//! 2. Bundled standards (FOAF, Dublin Core — embedded at compile time)
//! 3. Local filesystem (for development and testing)
//!
//! Ported from `ggen-core/src/ontology/loader.rs` during the ggen-core retirement migration
//! (`specs/014-ggen-core-replacement`, Phase 3 "non-colliding noun re-points"):
//! `ggen-cli`'s `cmds/ontology.rs` is the sole caller left depending on ggen-core for this
//! capability.
//!
//! ## Deviation from the ggen-core original: FOAF/Dublin Core are unconditional, not
//! ## feature-gated
//!
//! The ggen-core original gates its `FOAF_METADATA`/`DUBLIN_CORE_METADATA` statics behind a
//! `bundled-standards` Cargo feature. Investigation confirmed that feature is *on by default*
//! in ggen-core's own `Cargo.toml` (`default = ["dx", "marketplace", "bundled-standards"]`)
//! and nothing in the real `ggen-cli` build disables default features on its `ggen-core` dep
//! — so `OntologyLoader::load_content/get_metadata/is_embedded/list_embedded` always reach the
//! FOAF/Dublin Core branches in the shipped binary today. Rather than inventing a new
//! `bundled-standards` feature in `ggen-marketplace` to replicate a toggle nothing actually
//! exercises differently, this port makes the FOAF/Dublin Core statics unconditional — this is
//! behavior-identical to the real build, just without the dead alternate path.
//!
//! The two data files this pulls in (`foaf.ttl`, `dublin-core-elements-1.1.ttl`) are copied
//! into `ggen-marketplace/ontologies/` at the equivalent relative path and embedded here via
//! `include_str!`/inline byte constants — see `standard_ontologies_data` below.

use crate::ontology_core::resolver::OntologyResolver;
use std::path::Path;

/// FOAF (Friend of a Friend) ontology, embedded at compile time.
/// Copied from `ggen-core/ontologies/foaf.ttl` (see module docs for why this port is
/// unconditional rather than feature-gated).
const FOAF_TTL: &str = include_str!("../../ontologies/foaf.ttl");

/// Dublin Core Elements 1.1 ontology, embedded at compile time.
/// Copied from `ggen-core/ontologies/dublin-core-elements-1.1.ttl`.
const DUBLIN_CORE_TTL: &str = include_str!("../../ontologies/dublin-core-elements-1.1.ttl");

static FOAF_METADATA: crate::ontology_core::core_bundle::OntologyMetadata =
    crate::ontology_core::core_bundle::OntologyMetadata {
        name: "foaf",
        namespace: "http://xmlns.com/foaf/0.1/",
        size: FOAF_TTL.len(),
        content: FOAF_TTL.as_bytes(),
    };

static DUBLIN_CORE_METADATA: crate::ontology_core::core_bundle::OntologyMetadata =
    crate::ontology_core::core_bundle::OntologyMetadata {
        name: "dc",
        namespace: "http://purl.org/dc/elements/1.1/",
        size: DUBLIN_CORE_TTL.len(),
        content: DUBLIN_CORE_TTL.as_bytes(),
    };

/// Ontology loading strategy with fallback chain
#[derive(Debug, Clone, Copy)]
pub struct OntologyLoader;

impl OntologyLoader {
    /// Load ontology content using fallback chain:
    /// 1. Check core bundle (embedded, zero-copy)
    /// 2. Check bundled standards (FOAF, Dublin Core — embedded)
    /// 3. Check local filesystem (for development and testing)
    ///
    /// # Arguments
    ///
    /// * `uri` - Namespace URI (e.g., "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    /// * `base_path` - Base path for relative file lookups
    ///
    /// # Returns
    ///
    /// Ontology content as bytes, or None if not found/unavailable
    pub fn load_content(uri: &str, base_path: &Path) -> Option<Vec<u8>> {
        // Try core bundle first (embedded, zero-copy, always available)
        if let Some(ontology) = crate::ontology_core::CoreOntologyBundle::by_namespace(uri) {
            return Some(ontology.content.to_vec());
        }

        // Try core bundle by simple name (fallback for partial matches)
        // E.g., "owl" matches "http://www.w3.org/2002/07/owl#"
        if let Some(ontology) = crate::ontology_core::CoreOntologyBundle::by_name(uri) {
            return Some(ontology.content.to_vec());
        }

        if uri == "http://xmlns.com/foaf/0.1/" || uri == "foaf" {
            return Some(FOAF_METADATA.content.to_vec());
        }
        if uri == "http://purl.org/dc/elements/1.1/" || uri == "dc" {
            return Some(DUBLIN_CORE_METADATA.content.to_vec());
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

        None
    }

    /// Get ontology metadata for a namespace URI
    ///
    /// Returns metadata about the ontology if available in the embedded bundle.
    pub fn get_metadata(
        uri: &str,
    ) -> Option<&'static crate::ontology_core::core_bundle::OntologyMetadata> {
        if let Some(meta) = crate::ontology_core::CoreOntologyBundle::by_namespace(uri)
            .or_else(|| crate::ontology_core::CoreOntologyBundle::by_name(uri))
        {
            return Some(meta);
        }

        if uri == "http://xmlns.com/foaf/0.1/" || uri == "foaf" {
            return Some(&FOAF_METADATA);
        }
        if uri == "http://purl.org/dc/elements/1.1/" || uri == "dc" {
            return Some(&DUBLIN_CORE_METADATA);
        }

        None
    }

    /// Check if ontology is available in the embedded bundle (zero-copy access)
    ///
    /// This is useful for determining whether network access is required
    pub fn is_embedded(uri: &str) -> bool {
        if crate::ontology_core::CoreOntologyBundle::by_namespace(uri).is_some()
            || crate::ontology_core::CoreOntologyBundle::by_name(uri).is_some()
        {
            return true;
        }

        if uri == "http://xmlns.com/foaf/0.1/"
            || uri == "foaf"
            || uri == "http://purl.org/dc/elements/1.1/"
            || uri == "dc"
        {
            return true;
        }

        false
    }

    /// List all embedded ontologies available without network access
    pub fn list_embedded() -> Vec<(&'static str, &'static str)> {
        let mut list = crate::ontology_core::CoreOntologyBundle::available();
        list.push(("foaf", "http://xmlns.com/foaf/0.1/"));
        list.push(("dc", "http://purl.org/dc/elements/1.1/"));
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
        let _ = content;
    }

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

    #[test]
    fn test_nonexistent_uri_returns_none() {
        assert!(OntologyLoader::get_metadata("http://example.com/fake#").is_none());
        assert!(OntologyLoader::load_content("http://example.com/fake#", Path::new(".")).is_none());
        assert!(!OntologyLoader::is_embedded("http://example.com/fake#"));
    }

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

    #[test]
    fn test_fallback_by_name_and_namespace() {
        let by_name = OntologyLoader::load_content("owl", Path::new("."));
        let by_namespace =
            OntologyLoader::load_content("http://www.w3.org/2002/07/owl#", Path::new("."));

        assert!(by_name.is_some(), "Should load by short name");
        assert!(by_namespace.is_some(), "Should load by full namespace");
    }

    #[test]
    fn test_is_embedded_negative() {
        assert!(!OntologyLoader::is_embedded("http://example.com/ontology#"));
        assert!(!OntologyLoader::is_embedded("notanontology"));
        assert!(!OntologyLoader::is_embedded(""));
        assert!(!OntologyLoader::is_embedded(
            "HTTP://www.w3.org/1999/02/22-rdf-syntax-ns#"
        ));
    }

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

    #[test]
    fn test_list_embedded_validity() {
        let embedded = OntologyLoader::list_embedded();

        for (name, uri) in embedded {
            assert!(!name.is_empty(), "Name should not be empty");

            assert!(!uri.is_empty(), "URI should not be empty");
            assert!(uri.starts_with("http"), "URI should start with http");
            assert!(
                uri.ends_with('#') || uri.ends_with('/'),
                "URI should end with # or /"
            );

            assert!(
                OntologyLoader::load_content(uri, Path::new(".")).is_some(),
                "Listed ontology {} should be loadable",
                name
            );
        }
    }

    #[test]
    fn test_multiple_lookups_thread_safe() {
        for _ in 0..100 {
            let _ = OntologyLoader::load_content(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                Path::new("."),
            );
            let _ = OntologyLoader::get_metadata("http://www.w3.org/2000/01/rdf-schema#");
            let _ = OntologyLoader::is_embedded("owl");
        }
    }
}
