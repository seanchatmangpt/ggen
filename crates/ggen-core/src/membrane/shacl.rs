//! SHACL validation reports for ggen membrane projections.
//!
//! Enforces structural, cryptographic, and pedigree constraints on membrane projections.

use crate::utils::error::{Error, Result};
use crate::graph::Graph;
use crate::validation::{SparqlValidator, ValidationResult};

/// Shapes Graph definition in Turtle for membrane components, events, and provenance.
pub const MEMBRANE_SHACL_SHAPES: &str = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix mem: <http://ggen.org/membrane#> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix ocel: <http://www.ocel-standard.org/ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# 1. Shape enforcing constraints on Boundary Crossings
mem:BoundaryCrossingShape a sh:NodeShape ;
    sh:targetClass mem:BoundaryCrossing ;
    
    # Must have exactly one interface function name
    sh:property [
        sh:path mem:interfaceFn ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Each Boundary Crossing must have exactly one interface function name of type string."
    ] ;
    
    # Must have exactly one execution timestamp
    sh:property [
        sh:path prov:startedAtTime ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:dateTime ;
        sh:message "Each Boundary Crossing must have exactly one start timestamp of type dateTime."
    ] ;

    # Must be associated with at least one agent (e.g. ggen_membrane)
    sh:property [
        sh:path prov:wasAssociatedWith ;
        sh:minCount 1 ;
        sh:message "Boundary crossings must record the agent associated with execution."
    ] ;

    # Must use at least one entity (e.g., input payload or interchangeable part)
    sh:property [
        sh:path prov:used ;
        sh:minCount 1 ;
        sh:message "Boundary crossings must record what entities were used during execution."
    ] .

# 2. Shape enforcing constraints on Interchangeable Parts
mem:InterchangeablePartShape a sh:NodeShape ;
    sh:targetClass mem:InterchangeablePart ;

    # Must have a payload hash
    sh:property [
        sh:path mem:payloadHash ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Every interchangeable part must have a cryptographic payload hash."
    ] ;

    # Must implement at least one interface
    sh:property [
        sh:path mem:implementsInterface ;
        sh:minCount 1 ;
        sh:message "An interchangeable part must declare at least one implemented interface."
    ] .
"#;

/// Validator utility for running SHACL checks on projected membrane graphs
pub struct MembraneShaclValidator;

impl MembraneShaclValidator {
    /// Validate the projected membrane RDF graph against the membrane SHACL shapes
    pub fn validate(projected_graph: &Graph) -> Result<ValidationResult> {
        let shapes_graph = Graph::new()?;
        shapes_graph.insert_turtle(MEMBRANE_SHACL_SHAPES)?;

        let validator = SparqlValidator::new();
        let report = validator
            .validate(projected_graph, &shapes_graph)
            .map_err(|e| Error::new(&format!("SHACL validation failed: {}", e)))?;

        Ok(report)
    }
}
