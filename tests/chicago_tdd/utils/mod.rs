//! Chicago TDD tests for utility subsystems
//!
//! These tests follow Chicago School TDD principles:
//! - Use REAL system operations (file I/O, process execution, etc.)
//! - Test actual behavior, not mocked interfaces
//! - Verify real outcomes with temporary resources
//! - No mocking except for external services

mod audit_security_tests;
mod ci_workflow_tests;
mod shell_completion_tests;
