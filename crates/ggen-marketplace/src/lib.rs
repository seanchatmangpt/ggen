#![forbid(unsafe_code)]
#![allow(
    missing_docs,
    deprecated,
    dead_code,
    elided_lifetimes_in_paths,
    mismatched_lifetime_syntaxes
)]
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
pub mod ontology;
pub mod ownership;
pub mod pki;
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

pub use atomic::{foundation_packs, AtomicPackCategory, AtomicPackClass, AtomicPackId};
pub use bundle::{Bundle, Bundles};
pub use cache::{CacheConfig, CacheStats, CachedPack, PackCache};
pub use compatibility::{
    CompatibilityChecker, CompatibilityDimension, CompatibilityReport, Conflict,
    ConflictResolution, ConflictSeverity,
};
pub use composition_receipt::{CompositionReceipt, SignatureRecord};
pub use error::{Error, Result};
pub use install::Installer;
pub use metrics::MetricsCollector;
pub use models::*;
pub use ownership::{
    execute_merge_strategy, MergeError, MergeResult, MergeStrategy, OwnershipClass,
    OwnershipDeclaration, OwnershipMap, OwnershipTarget,
};
pub use policy::{
    PackContext, Policy, PolicyEnforcer, PolicyId, PolicyReport, PolicyRule, PolicyViolation,
};
pub use profile::{
    CustomPolicyRef, CustomProfileEntry, CustomRuntimeConstraint, Profile, ProfileConfig,
    ProfileId, ProfileLoader, ReceiptSpec, RuntimeConstraint, development_profile,
    enterprise_strict_profile, get_profile, predefined_profiles,
};
pub use registry::Registry;
pub use registry_rdf::RdfRegistry;
pub use search::SearchEngine;
pub use search_sparql::SparqlSearchEngine;
pub use security::{MarketplaceSignature, MarketplaceVerifier, SignatureReceipt};
pub use traits::*;
pub use trust::{RegistryClass, TrustTier};
pub use v3::V3OptimizedRegistry;
pub use validation::Validator;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        cache::{CacheConfig, CacheStats, CachedPack, PackCache},
        composition_receipt::CompositionReceipt,
        error::{Error, Result},
        install::Installer,
        metrics::MetricsCollector,
        models::{Manifest, Package, PackageId, PackageMetadata, PackageVersion},
        registry::Registry,
        registry_rdf::RdfRegistry,
        search::SearchEngine,
        search_sparql::SparqlSearchEngine,
        security::{MarketplaceSignature, MarketplaceVerifier, SignatureReceipt},
        traits::{AsyncRepository, Installable, Observable, Queryable, Signable, Validatable},
        trust::{RegistryClass, TrustTier},
        v3::V3OptimizedRegistry,
        validation::Validator,
    };
}
