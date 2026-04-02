//! Comprehensive test suite for ggen packs Phase 2-3
//!
//! This module contains 100+ tests covering:
//! - Installation system (download, extraction, verification, rollback)
//! - SPARQL execution
//! - Template generation
//! - Dependency resolution
//! - Registry operations
//! - Cloud distribution
//!
//! All tests are mapped to FMEA failure modes to ensure comprehensive coverage.

#[cfg(test)]
mod unit {
    pub mod installation;
}

#[cfg(test)]
mod integration;

#[cfg(test)]
mod performance;

#[cfg(test)]
mod security;

// Re-export test utilities
pub use unit::installation::*;
