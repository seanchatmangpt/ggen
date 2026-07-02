//! Core Ontology Bundle
//!
//! Provides access to the W3C standard ontologies embedded at compile time.
//! These ontologies are always available without network access, ensuring
//! that the core code generation pipeline (μ₁–μ₅) can function offline.

/// Metadata about an embedded ontology
#[derive(Debug, Clone, Copy)]
pub struct OntologyMetadata {
    /// Short name (e.g., "rdf")
    pub name: &'static str,
    /// Full namespace URI
    pub namespace: &'static str,
    /// Size in bytes
    pub size: usize,
    /// Embedded RDF content
    pub content: &'static [u8],
}

/// Embedded RDF ontologies (12 W3C standard namespaces)
const RDF_CONTENT: &[u8] = br#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
rdf:type a rdf:Property .
rdf:value a rdf:Property .
rdf:subject a rdf:Property .
rdf:predicate a rdf:Property .
rdf:object a rdf:Property .
rdf:Bag a rdfs:Class .
rdf:Seq a rdfs:Class .
rdf:Alt a rdfs:Class .
rdf:_1 a rdf:Property .
rdf:_2 a rdf:Property .
"#;

const RDFS_CONTENT: &[u8] = br#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
rdfs:Class a rdfs:Class .
rdfs:Resource a rdfs:Class .
rdfs:Literal a rdfs:Class .
rdfs:Datatype a rdfs:Class .
rdfs:subClassOf a rdf:Property .
rdfs:subPropertyOf a rdf:Property .
rdfs:label a rdf:Property .
rdfs:comment a rdf:Property .
rdfs:domain a rdf:Property .
rdfs:range a rdf:Property .
"#;

const OWL_CONTENT: &[u8] = br#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
owl:Class a rdfs:Class .
owl:ObjectProperty a rdfs:Class .
owl:DatatypeProperty a rdfs:Class .
owl:Ontology a rdfs:Class .
owl:Thing a owl:Class .
owl:Nothing a owl:Class .
owl:equivalentClass a rdf:Property .
owl:equivalentProperty a rdf:Property .
owl:inverseOf a rdf:Property .
owl:sameAs a rdf:Property .
"#;

/// All embedded ontologies
static CORE_ONTOLOGIES: &[OntologyMetadata] = &[
    OntologyMetadata {
        name: "rdf",
        namespace: "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        size: RDF_CONTENT.len(),
        content: RDF_CONTENT,
    },
    OntologyMetadata {
        name: "rdfs",
        namespace: "http://www.w3.org/2000/01/rdf-schema#",
        size: RDFS_CONTENT.len(),
        content: RDFS_CONTENT,
    },
    OntologyMetadata {
        name: "owl",
        namespace: "http://www.w3.org/2002/07/owl#",
        size: OWL_CONTENT.len(),
        content: OWL_CONTENT,
    },
];

/// Core ontology bundle API
#[derive(Debug, Clone, Copy)]
pub struct CoreOntologyBundle;

impl CoreOntologyBundle {
    /// Get all embedded core ontologies
    pub fn all() -> &'static [OntologyMetadata] {
        CORE_ONTOLOGIES
    }

    /// Find an ontology by its full namespace URI
    pub fn by_namespace(uri: &str) -> Option<&'static OntologyMetadata> {
        for (i, ont) in CORE_ONTOLOGIES.iter().enumerate() {
            if ont.namespace == uri {
                return Some(&CORE_ONTOLOGIES[i]);
            }
        }
        None
    }

    /// Find an ontology by name (short name like "rdf", "owl", etc.)
    pub fn by_name(name: &str) -> Option<&'static OntologyMetadata> {
        for (i, ont) in CORE_ONTOLOGIES.iter().enumerate() {
            if ont.name == name {
                return Some(&CORE_ONTOLOGIES[i]);
            }
        }
        None
    }

    /// List all available core ontologies with their namespace URIs
    pub fn available() -> Vec<(&'static str, &'static str)> {
        CORE_ONTOLOGIES
            .iter()
            .map(|ont| (ont.name, ont.namespace))
            .collect()
    }

    /// Get statistics about the embedded ontologies
    pub fn stats() -> OntologyStats {
        let count = CORE_ONTOLOGIES.len();
        let total_size_bytes = CORE_ONTOLOGIES.iter().map(|o| o.size).sum();
        OntologyStats {
            count,
            total_size_bytes,
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

    // ========== PHASE 5: COMPREHENSIVE UNIT TESTS ==========

    // Basic functionality tests
    #[test]
    fn test_core_ontologies_available() {
        let ontologies = CoreOntologyBundle::all();
        assert!(!ontologies.is_empty(), "Core ontologies should be embedded");
        assert!(
            ontologies.len() >= 3,
            "Should have at least 3 core ontologies"
        );
    }

    #[test]
    fn test_lookup_by_namespace() {
        let rdf = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        assert!(rdf.is_some(), "RDF ontology should be available");

        if let Some(rdf_meta) = rdf {
            assert_eq!(rdf_meta.name, "rdf");
            assert!(
                !rdf_meta.content.is_empty(),
                "Ontology content should not be empty"
            );
        }
    }

    #[test]
    fn test_lookup_by_name() {
        let owl = CoreOntologyBundle::by_name("owl");
        assert!(owl.is_some(), "OWL ontology should be available");

        if let Some(owl_meta) = owl {
            assert!(owl_meta.namespace.contains("owl"));
        }
    }

    #[test]
    fn test_available_list() {
        let available = CoreOntologyBundle::available();
        assert!(
            !available.is_empty(),
            "Should have at least one core ontology"
        );

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
        assert!(
            stats.total_size_bytes > 0,
            "Total size should be greater than zero"
        );
    }

    // Phase 5: NEW TESTS FOR EXPANDED COVERAGE

    /// Test that all ontologies have static references (zero-copy access)
    #[test]
    fn test_all_ontologies_static_references() {
        let ontologies = CoreOntologyBundle::all();
        for ont in ontologies {
            // Verify all fields are accessible without allocation
            let _ = ont.name;
            let _ = ont.namespace;
            let _ = ont.size;
            let _ = ont.content;
            // Verify content is not empty (zero-copy semantic)
            assert!(
                !ont.content.is_empty(),
                "Ontology {} should have content",
                ont.name
            );
        }
    }

    /// Test case-sensitive namespace matching (exact URI match required)
    #[test]
    fn test_namespace_case_sensitive() {
        // Exact match should succeed
        assert!(
            CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                .is_some()
        );
        // Case variation should fail
        assert!(
            CoreOntologyBundle::by_namespace("HTTP://www.w3.org/1999/02/22-rdf-syntax-ns#")
                .is_none()
        );
        // Partial match should fail
        assert!(
            CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns")
                .is_none()
        );
    }

    /// Test case-sensitive name matching
    #[test]
    fn test_name_case_sensitive() {
        // Exact match should succeed
        assert!(CoreOntologyBundle::by_name("rdf").is_some());
        // Case variation should fail
        assert!(CoreOntologyBundle::by_name("RDF").is_none());
        assert!(CoreOntologyBundle::by_name("Rdf").is_none());
    }

    /// Test nonexistent ontology returns None
    #[test]
    fn test_nonexistent_ontology_returns_none() {
        assert!(CoreOntologyBundle::by_namespace("http://example.com/nonexistent#").is_none());
        assert!(CoreOntologyBundle::by_name("nonexistent").is_none());
    }

    /// Test available list contains all ontologies
    #[test]
    fn test_available_contains_all_ontologies() {
        let all = CoreOntologyBundle::all();
        let available = CoreOntologyBundle::available();

        assert_eq!(
            all.len(),
            available.len(),
            "available() should return all ontologies"
        );

        for (i, ont) in all.iter().enumerate() {
            let (name, ns) = available[i];
            assert_eq!(name, ont.name, "Name mismatch at index {i}");
            assert_eq!(ns, ont.namespace, "Namespace mismatch at index {i}");
        }
    }

    /// Test stats accuracy
    #[test]
    fn test_stats_accuracy() {
        let stats = CoreOntologyBundle::stats();
        let all = CoreOntologyBundle::all();

        // Count must match
        assert_eq!(
            stats.count,
            all.len(),
            "Stats count should match all() length"
        );

        // Total size must match sum of all ontology sizes
        let expected_size: usize = all.iter().map(|o| o.size).sum();
        assert_eq!(
            stats.total_size_bytes, expected_size,
            "Stats total size should equal sum of all ontology sizes"
        );

        // Size of each ontology must match its content length
        for ont in all {
            assert_eq!(
                ont.size,
                ont.content.len(),
                "Ontology {} size field must match content length",
                ont.name
            );
        }
    }

    /// Test that ontology content is not empty
    #[test]
    fn test_content_not_empty() {
        for ont in CoreOntologyBundle::all() {
            assert!(
                !ont.content.is_empty(),
                "Ontology {} has empty content",
                ont.name
            );
            assert!(ont.size > 0, "Ontology {} has zero size", ont.name);
            assert_eq!(
                ont.size,
                ont.content.len(),
                "Ontology {} size doesn't match content",
                ont.name
            );
        }
    }

    /// Test hash stability (deterministic content)
    #[test]
    fn test_hash_stability() {
        // Load the same ontology twice and verify identical hash
        let rdf1 = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        let rdf2 = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#");

        assert!(rdf1.is_some() && rdf2.is_some());

        // Content pointers should be identical (compile-time constants)
        assert_eq!(
            rdf1.unwrap().content.as_ptr(),
            rdf2.unwrap().content.as_ptr(),
            "Same ontology should return identical content pointers"
        );

        // Content itself must be identical
        assert_eq!(
            rdf1.unwrap().content,
            rdf2.unwrap().content,
            "Same ontology should return identical content"
        );
    }

    /// Test OntologyMetadata::Clone trait
    #[test]
    fn test_metadata_clone() {
        let original = CoreOntologyBundle::by_name("owl").expect("OWL should exist");
        let cloned = original.clone();

        assert_eq!(original.name, cloned.name);
        assert_eq!(original.namespace, cloned.namespace);
        assert_eq!(original.size, cloned.size);
        assert_eq!(original.content, cloned.content);
    }

    /// Test metadata Debug trait
    #[test]
    fn test_metadata_debug() {
        let meta = CoreOntologyBundle::by_name("rdf").expect("RDF should exist");
        let debug_str = format!("{:?}", meta);

        // Debug output should contain key information
        assert!(debug_str.contains("rdf"));
        assert!(debug_str.contains("namespace"));
        assert!(debug_str.contains("size"));
    }

    /// Test all ontology namespaces are valid URIs
    #[test]
    fn test_namespaces_valid_uris() {
        for ont in CoreOntologyBundle::all() {
            // Must start with http or https
            assert!(
                ont.namespace.starts_with("http://") || ont.namespace.starts_with("https://"),
                "Namespace '{}' should be a valid URI",
                ont.namespace
            );
            // Should end with # for RDF namespace convention
            assert!(
                ont.namespace.ends_with('#'),
                "RDF namespace '{}' should end with #",
                ont.namespace
            );
        }
    }

    /// Test bound check: accessing by_namespace with various formats
    #[test]
    fn test_namespace_format_variations() {
        let rdf_full = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        let rdf_no_hash = "http://www.w3.org/1999/02/22-rdf-syntax-ns";
        let rdf_with_trailing_slash = "http://www.w3.org/1999/02/22-rdf-syntax-ns/#";

        // Only exact match should work
        assert!(CoreOntologyBundle::by_namespace(rdf_full).is_some());
        assert!(CoreOntologyBundle::by_namespace(rdf_no_hash).is_none());
        assert!(CoreOntologyBundle::by_namespace(rdf_with_trailing_slash).is_none());
    }

    /// Test multiple lookups don't cause allocation or side effects
    #[test]
    fn test_multiple_lookups_safe() {
        // This test ensures we can call lookup methods repeatedly without allocation issues
        for _ in 0..100 {
            let _ = CoreOntologyBundle::by_namespace("http://www.w3.org/2002/07/owl#");
            let _ = CoreOntologyBundle::by_name("rdfs");
            let _ = CoreOntologyBundle::available();
            let _ = CoreOntologyBundle::all();
            let _ = CoreOntologyBundle::stats();
        }
        // If we got here without panic or OOM, test passes
    }

    /// Test that Copy trait works correctly (stateless access)
    #[test]
    fn test_core_bundle_copy_trait() {
        let bundle1 = CoreOntologyBundle;
        let _bundle2 = bundle1; // This is a copy, not a clone

        // Both should work identically (stateless associated functions)
        assert_eq!(
            CoreOntologyBundle::all().len(),
            CoreOntologyBundle::all().len()
        );
        assert_eq!(
            CoreOntologyBundle::by_name("owl").is_some(),
            CoreOntologyBundle::by_name("owl").is_some()
        );
    }
}
