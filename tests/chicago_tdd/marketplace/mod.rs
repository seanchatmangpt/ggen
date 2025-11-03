//! Chicago TDD tests for marketplace commands
//!
//! Following Classicist School principles:
//! - Use REAL objects, not mocks
//! - Verify ACTUAL state changes
//! - Test with REAL file system (using TempDir)
//! - Minimal mocking - only external APIs

mod integration_tests;
mod search_tests;
mod domain_logic_tests;
mod p2p_integration;
