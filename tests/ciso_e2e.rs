//! CISO End-to-End Test Suite
//!
//! Tests that verify template authority, sync authority, receipt verification,
//! construct/AI workflows, and the complete governance workflow.
//!
//! All tests use real CLI invocations via `assert_cmd::Command::cargo_bin("ggen")`.
//! Chicago TDD: No mocks, no test doubles -- real commands, real output.
//!
//! NOTE: Gated behind `integration` feature — these tests require the full CLI
//! surface to be implemented (packs search, capability inspect, etc.).
#![cfg(feature = "integration")]

#[path = "ciso_e2e/helpers.rs"]
mod helpers;

#[path = "ciso_e2e/packs_discovery_workflow.rs"]
mod packs_discovery_workflow;

#[path = "ciso_e2e/capability_enable_workflow.rs"]
mod capability_enable_workflow;

#[path = "ciso_e2e/policy_validation_workflow.rs"]
mod policy_validation_workflow;

#[path = "ciso_e2e/sync_receipt_workflow.rs"]
mod sync_receipt_workflow;

#[path = "ciso_e2e/lockfile_policy_workflow.rs"]
mod lockfile_policy_workflow;

#[path = "ciso_e2e/enterprise_profile_workflow.rs"]
mod enterprise_profile_workflow;

#[path = "ciso_e2e/fail_closed_workflow.rs"]
mod fail_closed_workflow;

#[path = "ciso_e2e/template_sync_workflow.rs"]
mod template_sync_workflow;

#[path = "ciso_e2e/receipt_verification_workflow.rs"]
mod receipt_verification_workflow;

#[path = "ciso_e2e/construct_and_ai_workflow.rs"]
mod construct_and_ai_workflow;

#[path = "ciso_e2e/complete_governance_workflow.rs"]
mod complete_governance_workflow;
