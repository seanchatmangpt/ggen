//! Common utilities for MCP A2A integration tests
//!
//! This module provides shared test utilities, fixtures, and assertions
//! for MCP A2A integration testing following Chicago TDD patterns.

pub mod fixtures;
pub mod assertions;

// Re-export commonly used items
pub use fixtures::*;
pub use assertions::*;
