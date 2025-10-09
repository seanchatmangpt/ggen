mod bdd {
    pub mod steps;
    pub mod world;
}

use bdd::world::RgenWorld;
use cucumber::World as _;

// Import step definitions to ensure they're registered
use bdd::steps::*;

/// BDD Test Suite for rgen
///
/// This module contains all BDD tests that validate README claims.
/// Each feature file corresponds to a section of the README and
/// ensures that documented functionality works as described.

#[tokio::main]
async fn main() {
    RgenWorld::cucumber().run("tests/bdd/features").await;
}

// Individual feature tests can be run using:
// cargo test --test bdd --features installation
// cargo test --test bdd --features quickstart
// etc.
