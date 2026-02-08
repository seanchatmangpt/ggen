//! Chicago TDD unit tests for ggen-yawl crate.
//!
//! This module contains comprehensive unit tests for all major components:
//! - loader_tests: Ontology loading from multiple RDF formats
//! - executor_tests: SPARQL CONSTRUCT query execution with topological sort
//! - renderer_tests: Template rendering with Tera
//! - yawl_xml_tests: YAWL XML generation and validation
//!
//! All tests follow the AAA pattern (Arrange/Act/Assert) with real collaborators,
//! using state-based verification over interaction verification.

mod loader_tests;
mod executor_tests;
mod renderer_tests;
mod yawl_xml_tests;

/// Re-export test fixtures for use in integration tests
pub use loader_tests::fixtures as loader_fixtures;
pub use executor_tests::fixtures as executor_fixtures;
pub use renderer_tests::fixtures as renderer_fixtures;
pub use yawl_xml_tests::fixtures as yawl_xml_fixtures;
