//! RDF/Turtle Control Plane
//!
//! This module implements a fully semantic control plane using RDF triples
//! and SPARQL queries. All operations go through RDF - no JSON/SQL fallbacks.

pub mod control;
pub mod ontology;
pub mod sparql;
pub mod state_machine;
pub mod turtle_config;

pub use control::RdfControlPlane;
pub use ontology::{Ontology, GGEN_NS, MARKETPLACE_NS};
pub use sparql::{SparqlExecutor, SparqlQuery, SparqlQueryBuilder};
pub use state_machine::StateMachineExecutor;
pub use turtle_config::TurtleConfigLoader;
