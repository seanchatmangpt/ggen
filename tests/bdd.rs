mod bdd {
    pub mod steps;
    pub mod world;
}

use bdd::world::GgenWorld;
use cucumber::World as _;

// Import step definitions to ensure they're registered
#[allow(unused_imports)]
use bdd::steps::*;

/// BDD Test Suite for ggen
///
/// This module contains all BDD tests that validate README claims and
/// the new noun-verb command structure (project, market, template, graph).
/// Each feature file corresponds to a functional area and ensures that
/// documented functionality works as described.

#[tokio::test]
async fn test_graph_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/graph.feature")
        .await;
}

#[tokio::test]
async fn test_market_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/market.feature")
        .await;
}

#[tokio::test]
async fn test_project_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/project.feature")
        .await;
}

#[tokio::test]
async fn test_template_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/template.feature")
        .await;
}

#[tokio::test]
async fn test_installation_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/installation.feature")
        .await;
}

#[tokio::test]
async fn test_quickstart_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/quickstart.feature")
        .await;
}

#[tokio::test]
async fn test_template_generation_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/template_generation.feature")
        .await;
}

#[tokio::test]
async fn test_marketplace_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/marketplace.feature")
        .await;
}

#[tokio::test]
async fn test_cli_commands_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/cli_commands.feature")
        .await;
}

#[tokio::test]
async fn test_determinism_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/determinism.feature")
        .await;
}

#[tokio::test]
async fn test_multi_language_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/multi_language.feature")
        .await;
}

#[tokio::test]
async fn test_rdf_sparql_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features/rdf_sparql.feature")
        .await;
}

// Can run all features at once
#[tokio::test]
#[ignore] // Use individual feature tests by default for better reporting
async fn test_all_features() {
    GgenWorld::cucumber()
        .fail_on_skipped()
        .run("tests/bdd/features")
        .await;
}
