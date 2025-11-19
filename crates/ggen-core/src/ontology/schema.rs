//! Ontology Schema Representation
//!
//! Core types for representing RDF ontologies extracted from OWL/SKOS definitions.
//! These types serve as the semantic schema used throughout the platform generation pipeline.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Complete ontology schema extracted from RDF/OWL definitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologySchema {
    /// All classes (owl:Class) in the ontology
    pub classes: Vec<OntClass>,
    /// All properties (owl:ObjectProperty, owl:DatatypeProperty) in the ontology
    pub properties: Vec<OntProperty>,
    /// Relationships between classes (derived from properties)
    pub relationships: Vec<OntRelationship>,
    /// Ontology namespace (e.g., http://example.org/schema#)
    pub namespace: String,
    /// Ontology version (rdfs:comment or owl:versionInfo)
    pub version: String,
    /// Human-readable label for the ontology
    pub label: String,
    /// Description of the ontology purpose
    pub description: Option<String>,
    /// Metadata key-value pairs
    pub metadata: BTreeMap<String, String>,
}

/// Class definition extracted from owl:Class
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct OntClass {
    /// Full URI of the class (e.g., http://example.org/schema#Product)
    pub uri: String,
    /// Short name extracted from URI (e.g., "Product")
    pub name: String,
    /// Human-readable label (rdfs:label)
    pub label: String,
    /// Description of the class purpose (rdfs:comment)
    pub description: Option<String>,
    /// Parent classes (rdfs:subClassOf)
    pub parent_classes: Vec<String>,
    /// Property URIs that are applicable to this class
    pub properties: Vec<String>,
    /// Whether this is an abstract class
    pub is_abstract: bool,
    /// Additional OWL restrictions on this class
    pub restrictions: Vec<OwlRestriction>,
}

/// Property definition extracted from owl:ObjectProperty or owl:DatatypeProperty
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct OntProperty {
    /// Full URI of the property (e.g., http://example.org/schema#hasAuthor)
    pub uri: String,
    /// Short name extracted from URI (e.g., "hasAuthor")
    pub name: String,
    /// Human-readable label (rdfs:label)
    pub label: String,
    /// Description of the property (rdfs:comment)
    pub description: Option<String>,
    /// Domain classes (rdfs:domain) - classes that have this property
    pub domain: Vec<String>,
    /// Range of values this property can take
    pub range: PropertyRange,
    /// Cardinality constraints for this property
    pub cardinality: Cardinality,
    /// Whether this property is required
    pub required: bool,
    /// Whether this property is a functional property (at most one value)
    pub is_functional: bool,
    /// Whether this property is inverse-functional
    pub is_inverse_functional: bool,
    /// Inverse property URI if this is a bidirectional relationship
    pub inverse_of: Option<String>,
}

/// Type range of a property - what values it can hold
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum PropertyRange {
    /// String literal
    String,
    /// Integer literal
    Integer,
    /// Floating point literal
    Float,
    /// Boolean literal
    Boolean,
    /// Date/DateTime literal
    DateTime,
    /// Date literal (YYYY-MM-DD)
    Date,
    /// Time literal
    Time,
    /// IRI/Reference to another class (ObjectProperty)
    Reference(String),
    /// Custom/literal type (e.g., JSON, UUID)
    Literal(String),
    /// Enumeration with fixed values
    Enum(Vec<String>),
}

/// Cardinality constraints for a property
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Cardinality {
    /// Exactly one value (owl:cardinality 1)
    One,
    /// Zero or one value (owl:maxCardinality 1)
    ZeroOrOne,
    /// Zero or more values (default)
    Many,
    /// One or more values (owl:minCardinality 1)
    OneOrMore,
    /// Specific range (min, max)
    Range { min: u32, max: Option<u32> },
}

impl Cardinality {
    /// Get minimum cardinality
    pub fn min(&self) -> u32 {
        match self {
            Self::One => 1,
            Self::ZeroOrOne => 0,
            Self::Many => 0,
            Self::OneOrMore => 1,
            Self::Range { min, .. } => *min,
        }
    }

    /// Get maximum cardinality (None = unbounded)
    pub fn max(&self) -> Option<u32> {
        match self {
            Self::One => Some(1),
            Self::ZeroOrOne => Some(1),
            Self::Many => None,
            Self::OneOrMore => None,
            Self::Range { max, .. } => *max,
        }
    }

    /// Whether this property can have multiple values
    pub fn is_multi_valued(&self) -> bool {
        self.max().is_none() || self.max() > Some(1)
    }
}

/// OWL Restriction on a class (e.g., owl:someValuesFrom, owl:allValuesFrom)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum OwlRestriction {
    /// owl:someValuesFrom - at least one value must be from the specified class
    SomeValuesFrom(String), // class URI
    /// owl:allValuesFrom - all values must be from the specified class
    AllValuesFrom(String), // class URI
    /// owl:hasValue - property must have this exact value
    HasValue(String), // value
    /// owl:minCardinality - minimum number of values
    MinCardinality(u32),
    /// owl:maxCardinality - maximum number of values
    MaxCardinality(u32),
    /// owl:cardinality - exact number of values
    Cardinality(u32),
}

/// Relationship between two classes (derived from properties)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntRelationship {
    /// Source class URI
    pub from_class: String,
    /// Destination class URI
    pub to_class: String,
    /// Property URI that defines this relationship
    pub property: String,
    /// Type of relationship (one-to-one, one-to-many, many-to-many)
    pub relationship_type: RelationshipType,
    /// Whether this is a bidirectional relationship
    pub bidirectional: bool,
    /// Label for the relationship
    pub label: String,
}

/// Type of relationship between classes
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RelationshipType {
    /// One instance of source relates to one instance of target
    OneToOne,
    /// One instance of source relates to many instances of target
    OneToMany,
    /// Many instances of source relate to one instance of target
    ManyToOne,
    /// Many instances of source relate to many instances of target
    ManyToMany,
    /// Inheritance/subclass relationship
    Inheritance,
    /// Composition relationship
    Composition,
    /// Aggregation relationship
    Aggregation,
}

impl OntologySchema {
    /// Create a new empty ontology schema
    pub fn new(namespace: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            classes: Vec::new(),
            properties: Vec::new(),
            relationships: Vec::new(),
            namespace: namespace.into(),
            version: version.into(),
            label: String::new(),
            description: None,
            metadata: BTreeMap::new(),
        }
    }

    /// Find a class by name
    pub fn find_class(&self, name: &str) -> Option<&OntClass> {
        self.classes.iter().find(|c| c.name == name)
    }

    /// Find a class by URI
    pub fn find_class_by_uri(&self, uri: &str) -> Option<&OntClass> {
        self.classes.iter().find(|c| c.uri == uri)
    }

    /// Find a property by name
    pub fn find_property(&self, name: &str) -> Option<&OntProperty> {
        self.properties.iter().find(|p| p.name == name)
    }

    /// Find a property by URI
    pub fn find_property_by_uri(&self, uri: &str) -> Option<&OntProperty> {
        self.properties.iter().find(|p| p.uri == uri)
    }

    /// Get all properties applicable to a class
    pub fn properties_for_class(&self, class_uri: &str) -> Vec<&OntProperty> {
        self.properties
            .iter()
            .filter(|p| p.domain.contains(&class_uri.to_string()))
            .collect()
    }

    /// Get parent class chain for inheritance
    pub fn get_class_hierarchy(&self, class_uri: &str) -> Vec<String> {
        let mut hierarchy = vec![class_uri.to_string()];
        if let Some(class) = self.find_class_by_uri(class_uri) {
            for parent in &class.parent_classes {
                let parent_hierarchy = self.get_class_hierarchy(parent);
                hierarchy.extend(parent_hierarchy);
            }
        }
        hierarchy.sort();
        hierarchy.dedup();
        hierarchy
    }
}

impl PropertyRange {
    /// Convert PropertyRange to TypeScript type string
    pub fn to_typescript_type(&self) -> String {
        match self {
            Self::String => "string".to_string(),
            Self::Integer => "number".to_string(),
            Self::Float => "number".to_string(),
            Self::Boolean => "boolean".to_string(),
            Self::DateTime => "Date".to_string(),
            Self::Date => "string".to_string(), // ISO date string
            Self::Time => "string".to_string(),
            Self::Reference(class_uri) => {
                // Extract class name from URI
                class_uri
                    .split('#')
                    .next_back()
                    .unwrap_or("unknown")
                    .to_string()
            }
            Self::Literal(type_name) => type_name.clone(),
            Self::Enum(values) => format!("'{}'", values.join("' | '")),
        }
    }

    /// Convert PropertyRange to GraphQL type string
    pub fn to_graphql_type(&self, cardinality: &Cardinality) -> String {
        let base_type = match self {
            Self::String => "String".to_string(),
            Self::Integer => "Int".to_string(),
            Self::Float => "Float".to_string(),
            Self::Boolean => "Boolean".to_string(),
            Self::DateTime => "DateTime".to_string(),
            Self::Date => "Date".to_string(),
            Self::Time => "Time".to_string(),
            Self::Reference(class_uri) => class_uri
                .split('#')
                .next_back()
                .unwrap_or("Unknown")
                .to_string(),
            Self::Literal(type_name) => type_name.clone(),
            Self::Enum(values) => values[0].clone(),
        };

        match cardinality {
            Cardinality::Many | Cardinality::OneOrMore => format!("[{}]!", base_type),
            Cardinality::One => format!("{}!", base_type),
            Cardinality::ZeroOrOne => base_type,
            Cardinality::Range { .. } => {
                if cardinality.is_multi_valued() {
                    format!("[{}]!", base_type)
                } else {
                    format!("{}!", base_type)
                }
            }
        }
    }

    /// Convert PropertyRange to SQL type string
    pub fn to_sql_type(&self) -> String {
        match self {
            Self::String => "VARCHAR(255)".to_string(),
            Self::Integer => "INTEGER".to_string(),
            Self::Float => "DECIMAL(10,2)".to_string(),
            Self::Boolean => "BOOLEAN".to_string(),
            Self::DateTime => "TIMESTAMPTZ".to_string(),
            Self::Date => "DATE".to_string(),
            Self::Time => "TIME".to_string(),
            Self::Reference(_) => "UUID".to_string(),
            Self::Literal(type_name) => match type_name.as_str() {
                "json" | "JSON" => "JSONB".to_string(),
                "uuid" | "UUID" => "UUID".to_string(),
                _ => "VARCHAR(255)".to_string(),
            },
            Self::Enum(_) => "VARCHAR(50)".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardinality_bounds() {
        assert_eq!(Cardinality::One.min(), 1);
        assert_eq!(Cardinality::One.max(), Some(1));
        assert_eq!(Cardinality::Many.max(), None);
        assert!(!Cardinality::One.is_multi_valued());
        assert!(Cardinality::Many.is_multi_valued());
    }

    #[test]
    fn test_property_range_conversion() {
        assert_eq!(PropertyRange::String.to_typescript_type(), "string");
        assert_eq!(PropertyRange::Integer.to_typescript_type(), "number");
        assert_eq!(PropertyRange::DateTime.to_typescript_type(), "Date");
        assert_eq!(PropertyRange::String.to_sql_type(), "VARCHAR(255)");
        assert_eq!(PropertyRange::DateTime.to_sql_type(), "TIMESTAMPTZ");
    }

    #[test]
    fn test_ontology_schema() {
        let mut schema = OntologySchema::new("http://example.org/", "1.0.0");
        schema.classes.push(OntClass {
            uri: "http://example.org/#Product".to_string(),
            name: "Product".to_string(),
            label: "Product".to_string(),
            description: Some("A product in the catalog".to_string()),
            parent_classes: vec![],
            properties: vec!["http://example.org/#name".to_string()],
            is_abstract: false,
            restrictions: vec![],
        });

        assert!(schema.find_class("Product").is_some());
        assert!(schema.find_class("Unknown").is_none());
    }
}
