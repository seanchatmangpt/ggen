//! Ontology Pack Metadata and Core Data Structures
//!
//! Extension of GpackMetadata for ontology-specific packs. Ontology packs are
//! specialized template packs that focus on code generation from RDF/OWL ontologies.
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_core::ontology_pack::{OntologyPackMetadata, OntologyConfig, OntologyDefinition};
//!
//! let pack_metadata = OntologyPackMetadata {
//!     base: Default::default(),
//!     ontology: OntologyConfig {
//!         ontologies: vec![
//!             OntologyDefinition {
//!                 id: "schema.org".to_string(),
//!                 name: "Schema.org".to_string(),
//!                 version: "15.0".to_string(),
//!                 namespace: "https://schema.org/".to_string(),
//!                 file_path: "ontologies/schema-org.ttl".to_string(),
//!                 format: OntologyFormat::Turtle,
//!                 description: "Schema.org vocabulary".to_string(),
//!                 spec_url: Some("https://schema.org".to_string()),
//!                 classes_count: 792,
//!                 properties_count: 1453,
//!             }
//!         ],
//!         ..Default::default()
//!     },
//! };
//! ```

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Ontology pack metadata - extends GpackMetadata with ontology-specific fields
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyPackMetadata {
    /// Base gpack metadata (id, name, version, description, etc.)
    #[serde(flatten)]
    pub base: crate::gpack::GpackMetadata,

    /// Ontology-specific configuration
    #[serde(default)]
    pub ontology: OntologyConfig,
}

/// Ontology-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OntologyConfig {
    /// Ontologies included in this pack
    #[serde(default)]
    pub ontologies: Vec<OntologyDefinition>,

    /// Default namespace for generated code
    pub default_namespace: Option<String>,

    /// Code generation targets supported by this pack
    #[serde(default)]
    pub targets: Vec<CodeGenTarget>,

    /// Template organization (language-specific paths)
    #[serde(default)]
    pub template_paths: BTreeMap<String, String>,

    /// Ontology-specific prefixes
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
}

/// An ontology definition (SCHEMA.ORG, FOAF, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyDefinition {
    /// Ontology identifier (e.g., "schema.org", "foaf", "dublincore")
    pub id: String,

    /// Human-readable name
    pub name: String,

    /// Ontology version
    pub version: String,

    /// Namespace URI
    pub namespace: String,

    /// Path to ontology file within pack (TTL, RDF/XML, etc.)
    pub file_path: String,

    /// Format (turtle, rdfxml, ntriples, jsonld)
    pub format: OntologyFormat,

    /// Description
    pub description: String,

    /// Official specification URL
    pub spec_url: Option<String>,

    /// Classes count (populated after extraction)
    #[serde(default)]
    pub classes_count: usize,

    /// Properties count (populated after extraction)
    #[serde(default)]
    pub properties_count: usize,
}

/// Ontology file format
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum OntologyFormat {
    /// Turtle format (.ttl)
    Turtle,
    /// RDF/XML format (.rdf, .xml)
    RdfXml,
    /// N-Triples format (.nt)
    NTriples,
    /// JSON-LD format (.jsonld)
    JsonLd,
}

impl OntologyFormat {
    /// Get file extension for this format
    pub fn extension(&self) -> &str {
        match self {
            OntologyFormat::Turtle => "ttl",
            OntologyFormat::RdfXml => "rdf",
            OntologyFormat::NTriples => "nt",
            OntologyFormat::JsonLd => "jsonld",
        }
    }
}

/// Code generation target (language + features)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeGenTarget {
    /// Target language (typescript, rust, python, etc.)
    pub language: String,

    /// Features enabled for this target
    #[serde(default)]
    pub features: Vec<String>, // ["zod", "utilities", "graphql", etc.]

    /// Template path for this target (relative to pack root)
    pub template_path: String,

    /// Output file pattern (e.g., "{class_name}.ts", "types.ts")
    pub output_pattern: String,
}

/// Extracted ontology schema (intermediate representation)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OntologySchema {
    /// Schema namespace
    pub namespace: String,

    /// Classes extracted from ontology
    #[serde(default)]
    pub classes: Vec<OntologyClass>,

    /// Properties extracted from ontology
    #[serde(default)]
    pub properties: Vec<OntologyProperty>,

    /// Relationships between classes
    #[serde(default)]
    pub relationships: Vec<OntologyRelationship>,

    /// Prefixes used in ontology
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
}

/// Ontology class
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyClass {
    /// Class URI
    pub uri: String,

    /// Class name (extracted from URI)
    pub name: String,

    /// Label (rdfs:label)
    pub label: Option<String>,

    /// Comment (rdfs:comment)
    pub comment: Option<String>,

    /// Properties belonging to this class
    #[serde(default)]
    pub properties: Vec<String>, // URIs

    /// Superclasses
    #[serde(default)]
    pub super_classes: Vec<String>,

    /// Subclasses
    #[serde(default)]
    pub sub_classes: Vec<String>,
}

/// Ontology property
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyProperty {
    /// Property URI
    pub uri: String,

    /// Property name
    pub name: String,

    /// Label
    pub label: Option<String>,

    /// Comment
    pub comment: Option<String>,

    /// Domain (class this property applies to)
    pub domain: String,

    /// Range (value type or target class)
    pub range: PropertyRange,

    /// Cardinality constraints
    pub cardinality: Option<Cardinality>,

    /// Is this a functional property (single value)?
    #[serde(default)]
    pub is_functional: bool,
}

/// Property range (value type)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum PropertyRange {
    /// Primitive type (string, integer, boolean, etc.)
    Datatype {
        /// XSD datatype (xsd:string, xsd:integer, etc.)
        datatype: String
    },

    /// Reference to another class
    Reference {
        /// Class URI
        class_uri: String
    },

    /// Array of values
    Array {
        /// Item type
        item_type: Box<PropertyRange>
    },
}

/// Cardinality constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cardinality {
    /// Minimum occurrences (0 = optional, 1 = required)
    pub min: Option<usize>,

    /// Maximum occurrences (None = unbounded)
    pub max: Option<usize>,
}

/// Relationship between classes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyRelationship {
    /// Source class URI
    pub from: String,

    /// Target class URI
    pub to: String,

    /// Property URI that defines this relationship
    pub via_property: String,

    /// Relationship type
    pub relationship_type: RelationshipType,
}

/// Type of relationship between classes
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum RelationshipType {
    /// rdfs:subClassOf inheritance
    Inheritance,
    /// Composition (part-of relationship)
    Composition,
    /// General association
    Association,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ontology_format_extension() {
        assert_eq!(OntologyFormat::Turtle.extension(), "ttl");
        assert_eq!(OntologyFormat::RdfXml.extension(), "rdf");
        assert_eq!(OntologyFormat::NTriples.extension(), "nt");
        assert_eq!(OntologyFormat::JsonLd.extension(), "jsonld");
    }

    #[test]
    fn test_ontology_class_serialization() {
        let class = OntologyClass {
            uri: "https://schema.org/Product".to_string(),
            name: "Product".to_string(),
            label: Some("Product".to_string()),
            comment: Some("A manufactured product".to_string()),
            properties: vec!["https://schema.org/name".to_string()],
            super_classes: vec![],
            sub_classes: vec![],
        };

        let json = serde_json::to_string(&class).unwrap();
        let deserialized: OntologyClass = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.name, "Product");
        assert_eq!(deserialized.properties.len(), 1);
    }
}
