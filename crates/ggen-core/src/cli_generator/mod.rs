//! CLI Generator for 2026 best practices
//!
//! This module provides generators for creating CLI projects from RDF ontologies
//! following 2026 best practices. It generates complete workspace structures with
//! separate CLI and domain crates, enabling clean separation of concerns and stable
//! API contracts.
//!
//! ## Architecture
//!
//! The generator creates a workspace with two main crates:
//! - **CLI Layer**: Handles argument parsing, user interaction, and command dispatch
//! - **Domain Layer**: Contains business logic and domain functions
//!
//! This separation allows the CLI to evolve independently while maintaining stable
//! domain function contracts.
//!
//! ## Features
//!
//! - **Workspace Structure**: Generates proper Rust workspace with separate crates
//! - **Domain Function References**: CLI layer references stable domain functions
//! - **clap-noun-verb v3.3.0**: Modern CLI framework with noun-verb command structure
//! - **Hyper-Advanced DX**: Enhanced error messages, live preview, IDE hints
//! - **RDF Ontology Parsing**: Extracts CLI structure from RDF/Turtle files
//! - **Type Safety**: Strongly typed arguments, nouns, verbs, and validations
//!
//! ## Workflow
//!
//! 1. **Parse Ontology**: Extract CLI structure from RDF/Turtle file
//! 2. **Generate Domain Layer**: Create domain crate with function stubs
//! 3. **Generate CLI Layer**: Create CLI crate with clap-noun-verb integration
//! 4. **Generate Workspace**: Create workspace Cargo.toml and structure
//!
//! ## Examples
//!
//! ### Generating a CLI Project
//!
//! ```rust,no_run
//! use ggen_core::cli_generator::{OntologyParser, CliLayerGenerator, DomainLayerGenerator, WorkspaceGenerator};
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Parse RDF ontology
//! let project = OntologyParser::parse(Path::new("cli-schema.ttl"))?;
//!
//! // Generate workspace structure
//! let workspace_gen = WorkspaceGenerator::new(Path::new("templates"))?;
//! workspace_gen.generate(&project, Path::new("output"))?;
//!
//! // Generate domain layer
//! let domain_gen = DomainLayerGenerator::new(Path::new("templates"))?;
//! domain_gen.generate(&project, Path::new("output"))?;
//!
//! // Generate CLI layer
//! let cli_gen = CliLayerGenerator::new(Path::new("templates"))?;
//! cli_gen.generate(&project, Path::new("output"))?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Project Structure
//!
//! The generator creates a workspace with the following structure:
//!
//! ```text
//! my-cli/
//! ├── Cargo.toml          # Workspace manifest
//! ├── cli/
//! │   ├── Cargo.toml
//! │   └── src/
//! │       └── main.rs      # CLI entry point with clap-noun-verb
//! └── domain/
//!     ├── Cargo.toml
//!     └── src/
//!         └── lib.rs        # Domain functions
//! ```

pub mod cli_layer;
pub mod domain_layer;
pub mod dx;
pub mod ontology_parser;
pub mod types;
pub mod workspace;

pub use cli_layer::CliLayerGenerator;
pub use domain_layer::DomainLayerGenerator;
pub use ontology_parser::OntologyParser;
pub use types::{Argument, ArgumentType, CliProject, Dependency, Noun, Validation, Verb};
pub use workspace::WorkspaceGenerator;
