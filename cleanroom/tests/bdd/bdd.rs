use cucumber::{WorldInit, World};
use std::fs;
use std::path::Path;

mod world;
mod steps;

use world::CleanroomWorld;

/// BDD test runner for Cleanroom
///
/// This module provides the main test runner that executes all BDD scenarios
/// using the Cucumber framework. Each test function corresponds to a feature file.

#[tokio::test]
async fn test_unified_execution() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/01_unified_execution.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_backend_autodetect() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/02_backend_autodetect.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_config_precedence() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/03_config_precedence.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_scenario_dsl() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/04_scenario_dsl.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_assertions() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/05_assertions.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_all_features() {
    CleanroomWorld::cucumber()
        .features(&[
            "tests/bdd/features/01_unified_execution.feature",
            "tests/bdd/features/02_backend_autodetect.feature",
            "tests/bdd/features/03_config_precedence.feature",
            "tests/bdd/features/04_scenario_dsl.feature",
            "tests/bdd/features/05_assertions.feature",
            "tests/bdd/features/06_snapshots.feature",
            "tests/bdd/features/07_coverage.feature",
            "tests/bdd/features/08_security_defaults.feature",
            "tests/bdd/features/09_services.feature",
            "tests/bdd/features/10_redaction.feature",
            "tests/bdd/features/11_determinism.feature",
            "tests/bdd/features/12_errors.feature",
            "tests/bdd/features/13_windows_local.feature",
            "tests/bdd/features/14_concurrency.feature",
            "tests/bdd/features/15_api_stability.feature",
            "tests/bdd/features/16_freeze_outputs.feature",
            "tests/bdd/features/17_policy_typestate.feature",
            "tests/bdd/features/18_cli_integration.feature",
            "tests/bdd/features/19_service_teardown.feature",
            "tests/bdd/features/20_artifact_contract.feature",
            "tests/bdd/features/21_engine_matrix.feature",
            "tests/bdd/features/22_tracing.feature",
            "tests/bdd/features/23_skip_logic.feature",
            "tests/bdd/features/24_io_limits.feature",
            "tests/bdd/features/25_workdir_isolation.feature",
            "tests/bdd/features/26_time_budget.feature",
            "tests/bdd/features/27_json_report.feature",
        ])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_unified_execution_verbose() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/01_unified_execution.feature"])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_backend_autodetect_verbose() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/02_backend_autodetect.feature"])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_config_precedence_verbose() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/03_config_precedence.feature"])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_scenario_dsl_verbose() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/04_scenario_dsl.feature"])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_assertions_verbose() {
    CleanroomWorld::cucumber()
        .features(&["tests/bdd/features/05_assertions.feature"])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_all_features_verbose() {
    CleanroomWorld::cucumber()
        .features(&[
            "tests/bdd/features/01_unified_execution.feature",
            "tests/bdd/features/02_backend_autodetect.feature",
            "tests/bdd/features/03_config_precedence.feature",
            "tests/bdd/features/04_scenario_dsl.feature",
            "tests/bdd/features/05_assertions.feature",
            "tests/bdd/features/06_snapshots.feature",
            "tests/bdd/features/07_coverage.feature",
            "tests/bdd/features/08_security_defaults.feature",
            "tests/bdd/features/09_services.feature",
            "tests/bdd/features/10_redaction.feature",
            "tests/bdd/features/11_determinism.feature",
            "tests/bdd/features/12_errors.feature",
            "tests/bdd/features/13_windows_local.feature",
            "tests/bdd/features/14_concurrency.feature",
            "tests/bdd/features/15_api_stability.feature",
            "tests/bdd/features/16_freeze_outputs.feature",
            "tests/bdd/features/17_policy_typestate.feature",
            "tests/bdd/features/18_cli_integration.feature",
            "tests/bdd/features/19_service_teardown.feature",
            "tests/bdd/features/20_artifact_contract.feature",
            "tests/bdd/features/21_engine_matrix.feature",
            "tests/bdd/features/22_tracing.feature",
            "tests/bdd/features/23_skip_logic.feature",
            "tests/bdd/features/24_io_limits.feature",
            "tests/bdd/features/25_workdir_isolation.feature",
            "tests/bdd/features/26_time_budget.feature",
            "tests/bdd/features/27_json_report.feature",
        ])
        .fail_on_skipped()
        .run_and_exit()
        .await;
}
