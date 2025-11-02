//! Domain layer - pure business logic without CLI concerns
//!
//! This module contains all domain logic organized by functional area.
//! Domain modules are testable, reusable, and independent of CLI presentation.

pub mod ai;
pub mod audit;
pub mod graph;
pub mod marketplace;
// Temporarily disabled until cmds/project is implemented
// pub mod project;
pub mod template;
pub mod utils;
