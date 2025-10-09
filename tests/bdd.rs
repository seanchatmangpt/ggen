mod world;
mod steps;

use cucumber::{Cucumber, World};

use crate::world::RgenWorld;

/// BDD Test Suite for rgen
/// 
/// This module contains all BDD tests that validate README claims.
/// Each feature file corresponds to a section of the README and
/// ensures that documented functionality works as described.

#[tokio::main]
async fn main() {
    // Initialize the cucumber test runner
    let cucumber = Cucumber::new()
        .features(&["tests/bdd/features"])
        .steps(steps::common_steps::steps())
        .steps(steps::installation_steps::steps())
        .steps(steps::quickstart_steps::steps())
        .steps(steps::template_steps::steps())
        .steps(steps::marketplace_steps::steps())
        .steps(steps::cli_steps::steps())
        .steps(steps::determinism_steps::steps())
        .steps(steps::multilang_steps::steps())
        .steps(steps::rdf_steps::steps());

    // Run the tests
    cucumber.run_and_exit().await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_installation_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/installation.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::installation_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_quickstart_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/quickstart.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::quickstart_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_template_generation_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/template_generation.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::template_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_marketplace_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/marketplace.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::marketplace_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_cli_commands_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/cli_commands.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::cli_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_determinism_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/determinism.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::determinism_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_multi_language_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/multi_language.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::multilang_steps::steps());

        cucumber.run_and_exit().await;
    }

    #[tokio::test]
    async fn test_rdf_sparql_features() {
        let cucumber = Cucumber::new()
            .features(&["tests/bdd/features/rdf_sparql.feature"])
            .steps(steps::common_steps::steps())
            .steps(steps::rdf_steps::steps());

        cucumber.run_and_exit().await;
    }
}