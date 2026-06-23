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

    #[test]
    fn test_core_ontologies_available() {
        let ontologies = CoreOntologyBundle::all();
        assert!(!ontologies.is_empty(), "Core ontologies should be embedded");
        assert!(ontologies.len() >= 3, "Should have at least 3 core ontologies");
    }

    #[test]
    fn test_lookup_by_namespace() {
        let rdf = CoreOntologyBundle::by_namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        assert!(rdf.is_some(), "RDF ontology should be available");

        if let Some(rdf_meta) = rdf {
            assert_eq!(rdf_meta.name, "rdf");
            assert!(!rdf_meta.content.is_empty(), "Ontology content should not be empty");
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
