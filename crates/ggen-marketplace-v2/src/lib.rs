#![forbid(unsafe_code)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(
    missing_docs,
    deprecated,
    dead_code,
    elided_lifetimes_in_paths,
    mismatched_lifetime_syntaxes,
    // Pedantic lints to suppress
    clippy::unused_async,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::return_self_not_must_use,
    clippy::needless_pass_by_value,
    clippy::struct_excessive_bools,
    clippy::too_many_lines,
    clippy::cognitive_complexity,
    clippy::similar_names,
    clippy::match_same_arms,
    clippy::uninlined_format_args,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::format_push_string,
    clippy::doc_markdown,
    clippy::ignored_unit_patterns,
    clippy::unnecessary_cast,
    clippy::nonminimal_bool,
    clippy::manual_pattern_char_comparison,
    // Additional clippy lints from full pedantic
    clippy::await_holding_lock,
    clippy::bool_to_int_with_if,
    clippy::cast_possible_truncation,
    clippy::collapsible_str_replace,
    clippy::double_ended_iterator_last,
    clippy::inefficient_to_string,
    clippy::inherent_to_string,
    clippy::items_after_statements,
    clippy::manual_flatten,
    clippy::manual_range_contains,
    clippy::map_unwrap_or,
    clippy::match_wildcard_for_single_variants,
    clippy::needless_borrows_for_generic_args,
    clippy::needless_range_loop,
    clippy::new_without_default,
    clippy::redundant_closure_for_method_calls,
    clippy::semicolon_if_nothing_returned,
    clippy::single_char_add_str,
    clippy::single_match_else,
    clippy::type_complexity,
    clippy::unnecessary_literal_bound,
    clippy::unnecessary_wraps,
    clippy::unused_self,
    clippy::unwrap_or_default,
    clippy::useless_format,
    clippy::vec_init_then_push,
    clippy::match_like_matches_macro,
    clippy::needless_raw_string_hashes
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

pub mod builders;
pub mod error;
pub mod install;
pub mod metrics;
pub mod migration;
pub mod models;
pub mod ontology;
pub mod rdf;
pub mod rdf_mapper;
pub mod registry;
pub mod registry_rdf;
pub mod search;
pub mod search_sparql;
pub mod security;
pub mod traits;
pub mod v3;
pub mod validation;

pub use error::{Error, Result};
pub use install::Installer;
pub use metrics::MetricsCollector;
pub use models::*;
pub use registry::Registry;
pub use registry_rdf::RdfRegistry;
pub use search::SearchEngine;
pub use search_sparql::SparqlSearchEngine;
pub use security::SignatureVerifier;
pub use traits::*;
pub use v3::V3OptimizedRegistry;
pub use validation::Validator;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        error::{Error, Result},
        install::Installer,
        metrics::MetricsCollector,
        models::{Manifest, Package, PackageId, PackageMetadata, PackageVersion},
        registry::Registry,
        registry_rdf::RdfRegistry,
        search::SearchEngine,
        search_sparql::SparqlSearchEngine,
        security::SignatureVerifier,
        traits::{AsyncRepository, Installable, Observable, Queryable, Signable, Validatable},
        v3::V3OptimizedRegistry,
        validation::Validator,
    };
}
