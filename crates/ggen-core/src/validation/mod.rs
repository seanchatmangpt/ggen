//! SHACL validation module for ggen sync poka-yoke
//!
//! This module provides SPARQL-based SHACL validation integrated into the ggen sync pipeline.
//! It prevents defective TTL ontologies from entering the code generation pipeline by validating
//! them against SHACL shapes at the ontology loading stage.
//!
//! ## Architecture
//!
//! - **Zero new dependencies**: Uses existing Oxigraph SPARQL engine
//! - **SHACL→SPARQL translation**: Converts SHACL shapes into SPARQL SELECT queries
//! - **Poka-yoke design**: Fail-fast validation gates prevent defects from propagating
//!
//! ## Constitution Compliance
//!
//! - ✓ Principle I: Modular structure within ggen-core (not separate crate)
//! - ✓ Principle V: Type-first thinking with strong enums
//! - ✓ Principle VII: Result<T,E> error handling (NO unwrap in production)
//! - ✓ Principle IX: Lean Six Sigma poka-yoke design
//!
//! ## Integration Points
//!
//! 1. **Pipeline::load_ontology()** - Validation gate AFTER loading, BEFORE inference
//! 2. **SyncExecutor::execute_validate_only()** - Enhanced `--validate-only` mode
//!
//! ## Example Usage
//!
//! ```rust,no_run
//! use ggen_core::validation::{SparqlValidator, ValidationResult};
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load ontology and shapes
//! let ontology = Graph::load_from_file("ontology.ttl")?;
//! let shapes = Graph::load_from_file("shapes.ttl")?;
//!
//! // Validate
//! let validator = SparqlValidator::new();
//! let result = validator.validate(&ontology, &shapes)?;
//!
//! if !result.passed {
//!     eprintln!("Validation failed with {} violations", result.violations.len());
//!     for violation in &result.violations {
//!         eprintln!("  - {}: {}", violation.focus_node, violation.message);
//!     }
//! }
//! # Ok(())
//! # }
//! ```

pub mod error;
pub mod shacl;
pub mod sparql_rules;
pub mod validator;
pub mod violation;
pub mod checks;
pub mod gate;
pub mod standard_ontologies;
pub mod preflight;

#[cfg(test)]
mod tests;

// Re-export public API
pub use error::{Result, ValidationError};
pub use shacl::{PropertyConstraint, ShaclShape, ShaclShapeSet, ShapeLoader};
pub use sparql_rules::{RuleExecutor, RuleSeverity, ValidationRule};
pub use validator::SparqlValidator;
pub use violation::{ConstraintType, Severity, ValidationResult, Violation};
pub use checks::{Check, CheckError, CompilationCheck, LintCheck, TestCheck, SecurityCheck};
pub use gate::{QualityGate, QualityGateResult, CheckResult};
pub use standard_ontologies::{
    OntologyScreeningConfig, StandardOntology, StandardOntologyValidator,
};
pub use preflight::{PreFlightValidator, PreFlightResult};
