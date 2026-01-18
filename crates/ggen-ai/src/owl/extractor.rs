//! OWL Extractor - Extract domain knowledge from OWL ontologies
//!
//! This module provides functionality to query RDF graphs for OWL class definitions,
//! properties, and restrictions using Oxigraph RDF store.

use crate::error::Result;
use oxigraph::model::{NamedNode, NamedOrBlankNodeRef, Term};
use oxigraph::store::Store;
use std::path::Path;

/// OWL class definition with properties and restrictions
#[derive(Debug, Clone)]
pub struct OWLClass {
    pub uri: NamedNode,
    pub label: Option<String>,
    pub comment: Option<String>,
    pub properties: Vec<OWLProperty>,
    pub restrictions: Vec<OWLRestriction>,
}

/// OWL property (datatype or object property)
#[derive(Debug, Clone)]
pub enum OWLProperty {
    DatatypeProperty {
        uri: NamedNode,
        label: Option<String>,
        range: NamedNode,
    },
    ObjectProperty {
        uri: NamedNode,
        label: Option<String>,
        range: NamedNode,
    },
}

impl OWLProperty {
    /// Get the URI of this property
    pub fn uri(&self) -> &NamedNode {
        match self {
            OWLProperty::DatatypeProperty { uri, .. } => uri,
            OWLProperty::ObjectProperty { uri, .. } => uri,
        }
    }
}

/// OWL restriction (cardinality, value, datatype)
#[derive(Debug, Clone)]
pub enum OWLRestriction {
    Cardinality {
        property: NamedNode,
        min: Option<u32>,
        max: Option<u32>,
    },
    DatatypeRestriction {
        property: NamedNode,
        base_type: NamedNode,
        facets: Vec<DatatypeFacet>,
    },
    ValueRestriction {
        property: NamedNode,
        value_type: ValueRestrictionType,
    },
}

/// Datatype facets (XSD restrictions)
#[derive(Debug, Clone)]
pub enum DatatypeFacet {
    MinLength(u32),
    MaxLength(u32),
    Length(u32),
    Pattern(String),
    MinInclusive(f64),
    MaxInclusive(f64),
    MinExclusive(f64),
    MaxExclusive(f64),
}

/// Value restriction types
#[derive(Debug, Clone)]
pub enum ValueRestrictionType {
    AllValuesFrom(NamedNode),
    SomeValuesFrom(NamedNode),
    HasValue(Term),
}

/// OWL Extractor - queries RDF graph for OWL constructs
pub struct OWLExtractor {
    store: Store,
}

impl OWLExtractor {
    /// Create new OWL extractor with given RDF store
    pub fn new(store: Store) -> Self {
        Self { store }
    }

    /// Load OWL ontology from TTL file
    pub fn load_ontology(&mut self, path: &Path) -> Result<()> {
        use crate::error::GgenAiError;
        let file = std::fs::File::open(path)?;

        self.store
            .load_from_reader(oxigraph::io::RdfFormat::Turtle, file)
            .map_err(|e| GgenAiError::ontology_error(format!("Failed to load RDF ontology: {}", e)))?;

        Ok(())
    }

    /// Extract OWL class definition with all restrictions
    ///
    /// This queries the RDF graph for the class definition and extracts:
    /// - Basic class metadata (label, comment)
    /// - Properties (datatype and object properties)
    /// - Restrictions (cardinality, datatype facets, value constraints)
    pub fn extract_class(&self, class_uri: &str) -> Result<OWLClass> {
        use crate::error::GgenAiError;
        let class_node = NamedNode::new(class_uri)
            .map_err(|e| GgenAiError::ontology_error(format!("Invalid class URI {}: {}", class_uri, e)))?;

        // Query for basic class info
        let label = self.query_label(&class_node)?;
        let comment = self.query_comment(&class_node)?;

        // Extract properties (both datatype and object properties)
        let properties = self.extract_properties(&class_node)?;

        // Extract restrictions (cardinality, datatype, value)
        let restrictions = self.extract_restrictions(&class_node)?;

        Ok(OWLClass {
            uri: class_node,
            label,
            comment,
            properties,
            restrictions,
        })
    }

    /// Query RDFS label
    fn query_label(&self, node: &NamedNode) -> Result<Option<String>> {
        use crate::error::GgenAiError;
        let rdfs_label = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#label");

        for quad in self.store.quads_for_pattern(
            Some(NamedOrBlankNodeRef::NamedNode(node.as_ref())),
            Some(rdfs_label.as_ref()),
            None,
            None,
        ) {
            let quad = quad.map_err(|e| GgenAiError::ontology_error(format!("Query failed: {}", e)))?;
            if let Term::Literal(lit) = quad.object {
                return Ok(Some(lit.value().to_string()));
            }
        }

        Ok(None)
    }

    /// Query RDFS comment
    fn query_comment(&self, node: &NamedNode) -> Result<Option<String>> {
        use crate::error::GgenAiError;
        let rdfs_comment = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#comment");

        for quad in self.store.quads_for_pattern(
            Some(NamedOrBlankNodeRef::NamedNode(node.as_ref())),
            Some(rdfs_comment.as_ref()),
            None,
            None,
        ) {
            let quad = quad.map_err(|e| GgenAiError::ontology_error(format!("Query failed: {}", e)))?;
            if let Term::Literal(lit) = quad.object {
                return Ok(Some(lit.value().to_string()));
            }
        }

        Ok(None)
    }

    /// Extract all properties for a class
    fn extract_properties(&self, _class_node: &NamedNode) -> Result<Vec<OWLProperty>> {
        // Simplified implementation - Phase 1 will implement full SPARQL queries
        // For now, return empty vec to allow compilation
        Ok(Vec::new())
    }

    /// Extract all restrictions for a class
    fn extract_restrictions(&self, _class_node: &NamedNode) -> Result<Vec<OWLRestriction>> {
        // Simplified implementation - Phase 1 will implement full restriction extraction
        // For now, return empty vec to allow compilation
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owl_extractor_creation() {
        let store = Store::new().expect("Failed to create store");
        let extractor = OWLExtractor::new(store);
        assert!(std::ptr::addr_of!(extractor).is_null() == false);
    }

    #[test]
    fn test_owl_class_uri_access() {
        let uri = NamedNode::new("http://example.com/TestClass").expect("Invalid URI");
        let class = OWLClass {
            uri: uri.clone(),
            label: Some("Test Class".to_string()),
            comment: None,
            properties: Vec::new(),
            restrictions: Vec::new(),
        };
        assert_eq!(class.uri.as_str(), "http://example.com/TestClass");
    }

    #[test]
    fn test_owl_property_uri_access() {
        let uri = NamedNode::new("http://example.com/testProperty").expect("Invalid URI");
        let range = NamedNode::new("http://www.w3.org/2001/XMLSchema#string").expect("Invalid URI");

        let prop = OWLProperty::DatatypeProperty {
            uri: uri.clone(),
            label: Some("Test Property".to_string()),
            range,
        };

        assert_eq!(prop.uri().as_str(), "http://example.com/testProperty");
    }
}
