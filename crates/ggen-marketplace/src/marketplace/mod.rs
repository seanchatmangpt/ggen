#![forbid(unsafe_code)]
#![allow(
    missing_docs,
    deprecated,
    dead_code,
    elided_lifetimes_in_paths,
    mismatched_lifetime_syntaxes,
    clippy::pedantic,
    clippy::all
)]

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

pub mod atomic;
pub mod builders;
pub mod bundle;
pub mod cache;
pub mod compatibility;
pub mod composition_receipt;
pub mod error;
pub mod install;
pub mod metadata;
pub mod metrics;
pub mod migration;
pub mod models;
pub mod network;
pub mod ontology;
pub mod ownership;
pub mod part_passport;
pub mod policy;
pub mod profile;
pub mod rdf;
pub mod rdf_mapper;
pub mod registry;
pub mod registry_rdf;
pub mod search;
pub mod search_sparql;
pub mod security;
pub mod traits;
pub mod trust;
pub mod v3;
pub mod validation;

pub use composition_receipt::CompositionReceipt;
pub use error::{Error, Result};
pub use install::Installer;
pub use metrics::MetricsCollector;
pub use migration::{Migrator, UpgradeEdge};
pub use models::*;
pub use network::{MarketplaceClient, PackageMetadata as NetworkPackageMetadata};
pub use part_passport::{
    CausalPolarity, ClockDiscipline, ConformityMark, HostProfile, InputEnvelope, IsolationClass,
    LifecyclePolicy, LifecycleState, NameplateMark, NonInterferenceProfile, OutputContract,
    PartIdentity, PartPassport, PassportBinding, PassportValidationReport, PassportViolation,
    PassportViolationCode, ProtocolRange, ResourceEnvelope, RetirementPolicy, SubstitutionReport,
    SubstitutionViolation, SubstitutionViolationCode, TemporalProfile, TimeoutSemantics,
    VerifierMark, VerifierStatus, CURRENT_PASSPORT_SCHEMA,
};
pub use registry::Registry;
pub use registry_rdf::RdfRegistry;
pub use search::SearchEngine;
pub use search_sparql::SparqlSearchEngine;
pub use security::MarketplaceVerifier;
pub use traits::*;
pub use v3::V3OptimizedRegistry;
pub use validation::Validator;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::marketplace::{
        composition_receipt::CompositionReceipt,
        error::{Error, Result},
        install::Installer,
        metrics::MetricsCollector,
        migration::{Migrator, UpgradeEdge},
        models::{Manifest, Package, PackageId, PackageMetadata, PackageVersion},
        network::{MarketplaceClient, PackageMetadata as NetworkPackageMetadata},
        part_passport::{
            CausalPolarity, ClockDiscipline, ConformityMark, HostProfile, InputEnvelope,
            IsolationClass, LifecyclePolicy, LifecycleState, NonInterferenceProfile,
            OutputContract, PartIdentity, PartPassport, PassportBinding, ProtocolRange,
            ResourceEnvelope, RetirementPolicy, TemporalProfile, TimeoutSemantics, VerifierMark,
            VerifierStatus, CURRENT_PASSPORT_SCHEMA,
        },
        registry::Registry,
        registry_rdf::RdfRegistry,
        search::SearchEngine,
        search_sparql::SparqlSearchEngine,
        security::MarketplaceVerifier,
        traits::{AsyncRepository, Installable, Observable, Queryable, Signable, Validatable},
        v3::V3OptimizedRegistry,
        validation::Validator,
    };
}
