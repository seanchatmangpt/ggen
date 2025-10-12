//! Integration tests for cleanroom components
//!
//! Tests components with real collaborators to verify end-to-end behavior.
//! These tests may require external dependencies and should be marked with
//! #[ignore] if dependencies are unavailable.

pub mod local_backend;
pub mod docker_backend;
pub mod scenario_execution;
pub mod determinism;
