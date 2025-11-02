//! Domain layer - pure business logic without CLI concerns
//!
//! This module contains all domain logic organized by functional area.
//! Domain modules are testable, reusable, and independent of CLI presentation.

pub mod ai;
pub mod audit;
pub mod graph;
pub mod hook;
pub mod marketplace;
pub mod project;
pub mod rdf;
pub mod template;
pub mod utils;
