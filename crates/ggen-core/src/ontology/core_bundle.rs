//! Core Ontology Bundle
//!
//! Embedded at compile time via build.rs, this module provides access to the
//! 12 W3C standard ontologies that form the foundation of the ggen pipeline.
//!
//! These ontologies are always available without network access, ensuring
//! that the core code generation pipeline (μ₁–μ₅) can function offline.

include!(concat!(env!("OUT_DIR"), "/ontologies.rs"));

/// Core ontology bundle API
#[derive(Debug, Clone, Copy)]
pub struct CoreOntologyBundle;

impl CoreOntologyBundle {
    /// Get all embedded core ontologies
    pub fn all() -> &'static [OntologyMetadata] {
        CORE_ONTOLOGIES
    }

    /// Find an ontology by its full namespace URI
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let rdf = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")?;
    /// assert_eq!(rdf.name, "rdf-syntax-ns");
    /// ```
    pub fn by_namespace(uri: &str) -> Option<&'static OntologyMetadata> {
        by_namespace(uri)
    }

    /// Find an ontology by name
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let rdf = CoreOntologyBundle::by_name("rdf-syntax-ns")?;
    /// assert_eq!(rdf.size, 1234); // approximate
    /// ```
    pub fn by_name(name: &str) -> Option<&'static OntologyMetadata> {
        by_name(name)
    }

    /// List all available core ontologies with their namespace URIs
    pub fn available() -> Vec<(&'static str, &'static str)> {
        available()
    }

    /// Get statistics about the embedded ontologies
    pub fn stats() -> OntologyStats {
        OntologyStats {
            count: stats::TOTAL_ONTOLOGIES,
            total_size_bytes: stats::TOTAL_SIZE_BYTES,
        }
    }
}

/// Statistics about embedded ontologies
#[derive(Debug, Clone, Copy)]
pub struct OntologyStats {
    pub count: usize,
    pub total_size_bytes: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_ontologies_available() {
        let ontologies = CoreOntologyBundle::all();
        assert!(!ontologies.is_empty(), "Core ontologies should be embedded");
    }

    #[test]
    fn test_lookup_by_namespace() {
        let rdf = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        assert!(rdf.is_some(), "RDF ontology should be available");

        if let Some(rdf_meta) = rdf {
            assert!(!rdf_meta.content.is_empty(), "Ontology content should not be empty");
        }
    }

    #[test]
    fn test_lookup_by_name() {
        let owl = CoreOntologyBundle::by_name("owl");
        assert!(owl.is_some(), "OWL ontology should be available");
    }

    #[test]
    fn test_available_list() {
        let available = CoreOntologyBundle::available();
        assert!(!available.is_empty(), "Should have at least one core ontology");

        // Each entry should have both name and namespace
        for (name, ns) in available {
            assert!(!name.is_empty(), "Name should not be empty");
            assert!(!ns.is_empty(), "Namespace should not be empty");
            assert!(ns.starts_with("http"), "Namespace should be a valid URI");
        }
    }

    #[test]
    fn test_stats() {
        let stats = CoreOntologyBundle::stats();
        assert!(stats.count > 0, "Should have at least one core ontology");
        assert!(stats.total_size_bytes > 0, "Total size should be greater than zero");
    }
}
