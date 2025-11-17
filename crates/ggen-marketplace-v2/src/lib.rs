#![forbid(unsafe_code)]
#![deny(missing_docs)]
#![warn(clippy::all, clippy::pedantic)]

//! # ggen-marketplace-v2: Hyper-Advanced Marketplace System
//!
//! A ground-up rewrite of the ggen marketplace using cutting-edge Rust patterns:
//!
//! - **Generic Associated Types (GATs)** for flexible trait definitions
//! - **Higher-Ranked Trait Bounds (HRTB)** for lifetime flexibility
//! - **Type-Level Programming** to prevent invalid states at compile time
//! - **Zero-Copy Semantics** via references and smart pointers
//! - **Advanced Async Patterns** with structured concurrency
//! - **High-Performance Data Structures** for scalability
//! - **Cryptographic Security** with Ed25519 signing
//! - **Comprehensive Observability** via tracing

pub mod error;
pub mod models;
pub mod traits;
pub mod registry;
pub mod search;
pub mod install;
pub mod validation;
pub mod security;
pub mod metrics;
pub mod builders;

pub use error::{Error, Result};
pub use models::*;
pub use traits::*;
pub use registry::Registry;
pub use search::SearchEngine;
pub use install::Installer;
pub use validation::Validator;
pub use security::SignatureVerifier;
pub use metrics::MetricsCollector;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        error::{Error, Result},
        models::{Package, PackageId, PackageVersion, PackageMetadata, Manifest},
        traits::{
            AsyncRepository, Queryable, Installable, Validatable, Signable, Observable,
        },
        registry::Registry,
        search::SearchEngine,
        install::Installer,
        validation::Validator,
        security::SignatureVerifier,
        metrics::MetricsCollector,
    };
}
