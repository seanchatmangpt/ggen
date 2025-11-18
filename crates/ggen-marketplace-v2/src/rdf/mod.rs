//! RDF/Turtle Control Plane
//!
//! This module implements a fully semantic control plane using RDF triples
//! and SPARQL queries. All operations go through RDF - no JSON/SQL fallbacks.
//!
//! ## Architecture
//!
//! - **Ontology**: Complete RDF vocabulary with standard namespaces (`ontology.rs`)
//! - **POKA YOKE**: Type-safe RDF operations with compile-time guarantees (`poka_yoke.rs`)
//! - **SPARQL Queries**: All marketplace operations via SPARQL (`sparql_queries.rs`)
//! - **FMEA Mitigations**: Failure detection and auto-recovery (`fmea_mitigations.rs`)
//! - **Turtle Config**: Configuration loading from .ttl files (`turtle_config.rs`)
//! - **RDF Control**: Main control plane integration (`rdf_control.rs`)

pub mod control;
pub mod fmea_mitigations;
pub mod ontology;
pub mod poka_yoke;
pub mod rdf_control;
pub mod sparql;
pub mod sparql_queries;
pub mod state_machine;
pub mod turtle_config;

// Re-export from existing modules
pub use control::RdfControlPlane;
pub use ontology::{generate_prefixes, namespaces};
pub use sparql::{SparqlExecutor, SparqlQuery, SparqlQueryBuilder};
pub use state_machine::StateMachineExecutor;
pub use turtle_config::TurtleConfigLoader;

// Re-export from new modules
pub use fmea_mitigations::{FailureCategory, FailureMode, FmeaMitigationManager, MitigationResult};
pub use poka_yoke::{Literal, PokaYokeError, RdfGraph, ResourceId, Triple, ValidationConstraint};
pub use rdf_control::{ControlPlaneError, StateTransitionResult, ValidationResult};
pub use sparql_queries::{MarketplaceQueries, PackageSearchResult, SearchParams};
