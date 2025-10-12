//! Step definitions for Cleanroom BDD tests
//!
//! This module contains all the step definitions organized by functionality:
//! - `common_steps.rs` - Shared steps used across features
//! - `unified_execution_steps.rs` - Unified execution across backends
//! - `backend_autodetect_steps.rs` - Backend auto-detection and preference
//! - `config_precedence_steps.rs` - Configuration precedence and validation
//! - `scenario_dsl_steps.rs` - Scenario DSL for multi-step workflows
//! - `assertion_steps.rs` - Fluent assertions on output, JSON, and timing
//! - `snapshot_steps.rs` - Snapshots with redaction and stability
//! - `coverage_steps.rs` - Coverage artifacts collection and host merge
//! - `security_steps.rs` - Secure-by-default policy
//! - `service_steps.rs` - Side services with health gates
//! - `redaction_steps.rs` - Secret redaction in logs and artifacts
//! - `determinism_steps.rs` - Deterministic outputs with seeded RNG
//! - `error_steps.rs` - Structured errors with context
//! - `windows_steps.rs` - Windows support via local backend
//! - `concurrency_steps.rs` - Concurrent steps with isolation
//! - `api_stability_steps.rs` - Public API stability and docs
//! - `freeze_steps.rs` - Idempotent results across repeated runs
//! - `policy_typestate_steps.rs` - Typestate-enforced policy elevation
//! - `cli_integration_steps.rs` - Cargo integration via package.metadata
//! - `service_teardown_steps.rs` - Idempotent teardown of services
//! - `artifact_steps.rs` - Artifact capture, structure, and size limits
//! - `engine_matrix_steps.rs` - Matrix behavior across docker and podman
//! - `tracing_steps.rs` - Structured tracing for runs
//! - `skip_logic_steps.rs` - Skip semantics for missing engines
//! - `io_limits_steps.rs` - Output and resource limits
//! - `workdir_isolation_steps.rs` - Ephemeral workspace and read-only rootfs
//! - `time_budget_steps.rs` - Time budgets per step and scenario
//! - `json_report_steps.rs` - Machine-readable run reports

pub mod common_steps;
pub mod unified_execution_steps;
pub mod backend_autodetect_steps;
pub mod config_precedence_steps;
pub mod scenario_dsl_steps;
pub mod assertion_steps;
pub mod snapshot_steps;
pub mod coverage_steps;
pub mod security_steps;
pub mod service_steps;
pub mod redaction_steps;
pub mod determinism_steps;
pub mod error_steps;
pub mod windows_steps;
pub mod concurrency_steps;
pub mod api_stability_steps;
pub mod freeze_steps;
pub mod policy_typestate_steps;
pub mod cli_integration_steps;
pub mod service_teardown_steps;
pub mod artifact_steps;
pub mod engine_matrix_steps;
pub mod tracing_steps;
pub mod skip_logic_steps;
pub mod io_limits_steps;
pub mod workdir_isolation_steps;
pub mod time_budget_steps;
pub mod json_report_steps;
