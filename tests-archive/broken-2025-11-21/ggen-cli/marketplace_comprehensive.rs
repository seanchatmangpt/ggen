//! Comprehensive marketplace test suite entry point
//!
//! This file serves as the test harness for all marketplace tests,
//! integrating unit, integration, performance, and security tests.
//!
//! Run with: `cargo test --test marketplace_comprehensive`

#[path = "marketplace/mod.rs"]
mod marketplace;

// Re-export test modules for convenient access
pub use marketplace::*;
