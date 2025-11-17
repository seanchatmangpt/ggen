#![forbid(unsafe_code)]
#![deny(missing_docs)]
#![warn(clippy::all, clippy::pedantic)]

//! # ggen-marketplace-v2: Hyper-Advanced Marketplace System
//!
//! A ground-up rewrite of the ggen marketplace using cutting-edge Rust patterns:
//!
//! - **RDF-Backed Data Store** using oxigraph as semantic foundation
//! - **SPARQL Queries** for intelligent package discovery and search
//! - **Generic Associated Types (GATs)** for flexible trait definitions
//! - **Higher-Ranked Trait Bounds (HRTB)** for lifetime flexibility
//! - **Type-Level Programming** to prevent invalid states at compile time
//! - **Zero-Copy Semantics** via references and smart pointers
//! - **Advanced Async Patterns** with structured concurrency
//! - **Cryptographic Security** with Ed25519 signing
//! - **Comprehensive Observability** via tracing
//!
//! ## Data Model
//!
//! The marketplace uses RDF (Resource Description Framework) as its data model,
//! with oxigraph as the underlying triplestore. This allows:
//! - Semantic package relationships
//! - Flexible schema evolution
//! - SPARQL-based queries
//! - Version history as RDF facts

pub mod error;
pub mod models;
pub mod traits;
pub mod ontology;
pub mod registry;
pub mod registry_rdf;
pub mod search;
pub mod search_sparql;
pub mod install;
pub mod validation;
pub mod security;
pub mod metrics;
pub mod builders;
pub mod v3;

pub use error::{Error, Result};
pub use models::*;
pub use traits::*;
pub use registry::Registry;
pub use registry_rdf::RdfRegistry;
pub use search::SearchEngine;
pub use search_sparql::SparqlSearchEngine;
pub use install::Installer;
pub use validation::Validator;
pub use security::SignatureVerifier;
pub use metrics::MetricsCollector;
pub use v3::V3OptimizedRegistry;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        error::{Error, Result},
        models::{Package, PackageId, PackageVersion, PackageMetadata, Manifest},
        traits::{
            AsyncRepository, Queryable, Installable, Validatable, Signable, Observable,
        },
        registry::Registry,
        registry_rdf::RdfRegistry,
        search::SearchEngine,
        search_sparql::SparqlSearchEngine,
        install::Installer,
        validation::Validator,
        security::SignatureVerifier,
        metrics::MetricsCollector,
        v3::V3OptimizedRegistry,
    };
}
