//! # ggen-yawl: YAWL Workflow Generation from Industry Ontologies
//!
//! This crate generates YAWL (Yet Another Workflow Language) workflows from industry
//! ontologies (FIBO, HL7, ISO standards) using SPARQL CONSTRUCT queries and the ggen
//! five-stage pipeline (A = μ(O)).
//!
//! ## Architecture
//!
//! ```text
//! Industry Ontology (FIBO, HL7, etc.)
//!         ↓
//! SPARQL CONSTRUCT (6 transformation patterns)
//!         ↓
//! YAWL RDF (intermediate representation)
//!         ↓
//! Tera Templates (YAWL XML + Erlang)
//!         ↓
//! Executable Workflows
//! ```
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_yawl::{YawlGenerator, OntologyLoader};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load industry ontology
//! let ontology = OntologyLoader::load_from_file("fibo.ttl")?;
//!
//! // Generate YAWL workflow
//! let generator = YawlGenerator::new();
//! let yawl_xml = generator.generate(&ontology)?;
//!
//! // Write output
//! std::fs::write("output.yawl", yawl_xml)?;
//! # Ok(())
//! # }
//! ```

pub mod error;
pub mod ontology;
pub mod transform;
pub mod template;
pub mod codegen;

pub use error::{Error, Result};
pub use ontology::loader::OntologyLoader;
pub use transform::executor::ConstructExecutor;
pub use template::renderer::TemplateRenderer;
pub use codegen::yawl_xml::YawlXmlGenerator;

/// Main YAWL workflow generator orchestrating the full pipeline.
///
/// This struct coordinates the five-stage transformation:
/// - μ₁: Normalize (RDF validation, SHACL shapes)
/// - μ₂: Extract (SPARQL CONSTRUCT queries)
/// - μ₃: Emit (Tera template rendering)
/// - μ₄: Canonicalize (Deterministic formatting)
/// - μ₅: Receipt (Cryptographic proof generation)
#[derive(Debug, Clone)]
pub struct YawlGenerator {
    /// SPARQL CONSTRUCT query executor
    executor: ConstructExecutor,
    /// Template renderer for YAWL XML/Erlang
    renderer: TemplateRenderer,
    /// Whether to validate generated workflows
    validate_output: bool,
}

impl Default for YawlGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl YawlGenerator {
    /// Create a new YAWL generator with default configuration.
    pub fn new() -> Self {
        Self {
            executor: ConstructExecutor::new(),
            renderer: TemplateRenderer::new(),
            validate_output: true,
        }
    }

    /// Set whether to validate generated workflows.
    pub fn with_validation(mut self, validate: bool) -> Self {
        self.validate_output = validate;
        self
    }

    /// Generate YAWL XML from an industry ontology.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The ontology cannot be loaded
    /// - SPARQL queries fail
    /// - Template rendering fails
    /// - Validation fails (when enabled)
    pub fn generate(&self, ontology: &str) -> Result<String> {
        // μ₁: Load and normalize
        let graph = ontology::loader::load_ontology(ontology)?;

        // μ₂: Extract using CONSTRUCT queries
        let yawl_rdf = self.executor.execute_all(&graph)?;

        // μ₃: Emit using templates
        let xml = self.renderer.render_yawl_xml(&yawl_rdf)?;

        // μ₄: Canonicalize (format consistently)
        let canonical = codegen::yawl_xml::canonicalize(&xml)?;

        // μ₅: Validate if enabled
        if self.validate_output {
            codegen::yawl_xml::validate(&canonical)?;
        }

        Ok(canonical)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generator_creation() {
        let gen = YawlGenerator::new();
        assert!(gen.validate_output);
    }

    #[test]
    fn test_generator_with_validation_disabled() {
        let gen = YawlGenerator::new().with_validation(false);
        assert!(!gen.validate_output);
    }
}
