//! Expert-Level Testing Patterns
//!
//! This module implements the 80/20 rule for testing - focusing on the test cases
//! that catch 80% of production bugs:
//!
//! - Error paths (not just happy path)
//! - Boundary conditions (not just normal values)
//! - Resource cleanup (not just normal execution)
//! - Concurrency (not just single-threaded)
//! - Real dependencies (not just mocks)
//!
//! See docs/testing/chicago-tdd-guide.md for complete testing patterns.
//!
//! ## Test Structure
//!
//! Tests are organized by pattern type and ordered by priority (highest ROI first):
//! - **Phase 1**: Error paths + boundary conditions (70% of bugs prevented)
//! - **Phase 2**: Resource management (prevent memory/file leaks)
//! - **Phase 3**: Concurrency (thread-safety guarantees)

pub mod error_paths;
pub mod boundaries;
pub mod resources;
pub mod concurrency;
