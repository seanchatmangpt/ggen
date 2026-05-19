//! # ggen-marketplace - Package marketplace system
//!
//! Marketplace plugin providing package management, discovery, and installation for ggen.

pub mod marketplace;

pub use marketplace::{
    Manifest, Package, PackageId, QualityScore, RdfRegistry, SparqlSearchEngine,
};
