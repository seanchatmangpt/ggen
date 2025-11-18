//! Integration tests for marketplace CLI commands
//!
//! This module contains integration tests that verify:
//! - Complete command workflows
//! - Real CLI execution
//! - Output format validation
//! - Error handling and edge cases

// Existing tests
mod cli_commands_test;
mod edge_cases_test;

// V2 migration integration tests
mod backward_compat_test;
mod cross_backend_test;
mod v2_workflows_test;
