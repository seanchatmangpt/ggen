//! OWL (Web Ontology Language) processing module
//!
//! This module provides functionality to extract domain knowledge from OWL ontologies
//! and transform them into SHACL shapes for constraint validation.

pub mod extractor;
pub mod shacl_generator;

pub use extractor::{
    DatatypeFacet, OWLClass, OWLExtractor, OWLProperty, OWLRestriction, ValueRestrictionType,
};
pub use shacl_generator::{GeneratedShape, PropertyShape, SHACLGenerator};
