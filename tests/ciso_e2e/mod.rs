//! CISO End-to-End Test Suite
//!
//! Tests that verify template authority, sync authority, receipt verification,
//! construct/AI workflows, and the complete governance workflow.
//!
//! All tests use real CLI invocations via `assert_cmd::Command::cargo_bin("ggen")`.
//! Chicago TDD: No mocks, no test doubles -- real commands, real output.
//!
//! ## Test Suites
//!
//! - `packs_discovery_workflow`: Pack listing, search, show, compatibility
//! - `capability_enable_workflow`: Capability list, inspect, graph, trust, conflicts
//! - `policy_validation_workflow`: Policy profiles, validation, compliance
//! - `sync_receipt_workflow`: Init, sync, receipt verification
//! - `lockfile_policy_workflow`: Lockfile determinism, policy enforcement, capability enable
//! - `enterprise_profile_workflow`: Enterprise governance, regulated finance, profile validation
//! - `fail_closed_workflow`: Fail-closed security, invalid inputs, missing resources
//! - `template_sync_workflow`: Template authority, init, sync, graph commands
//! - `receipt_verification_workflow`: Receipt generation, verify, chain verify, info
//! - `construct_and_ai_workflow`: Construct and AI command workflows
//! - `complete_governance_workflow`: Full multi-step governance workflows

pub mod helpers;

pub mod template_sync_workflow;
pub mod receipt_verification_workflow;
pub mod construct_and_ai_workflow;
pub mod complete_governance_workflow;

pub mod packs_discovery_workflow;
pub mod capability_enable_workflow;
pub mod policy_validation_workflow;
pub mod sync_receipt_workflow;
pub mod lockfile_policy_workflow;
pub mod enterprise_profile_workflow;
pub mod fail_closed_workflow;
