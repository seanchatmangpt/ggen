//! Pass: Projection pipeline stage
//!
//! A Pass represents a single stage in the projection pipeline μ₁...μₙ.
//! Passes must be idempotent and deterministic.

use crate::graph::Graph;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Duration;

/// Type of projection pass
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PassType {
    /// μ₁: Ontology → Ontology (CONSTRUCT rewrites)
    Normalization,
    /// μ₂: Ontology → Bindings (SELECT queries)
    Extraction,
    /// μ₃: Bindings → Files (Tera templates)
    Emission,
    /// μ₄: Files → Canonical Files (formatting)
    Canonicalization,
    /// μ₅: Files → Receipt (provenance binding)
    Receipt,
}

impl PassType {
    /// Get the standard order index for this pass type
    pub fn order_index(&self) -> u32 {
        match self {
            PassType::Normalization => 1,
            PassType::Extraction => 2,
            PassType::Emission => 3,
            PassType::Canonicalization => 4,
            PassType::Receipt => 5,
        }
    }

    /// Get the standard name for this pass type
    pub fn name(&self) -> &'static str {
        match self {
            PassType::Normalization => "μ₁:normalization",
            PassType::Extraction => "μ₂:extraction",
            PassType::Emission => "μ₃:emission",
            PassType::Canonicalization => "μ₄:canonicalization",
            PassType::Receipt => "μ₅:receipt",
        }
    }
}

/// Result of a pass execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassResult {
    /// Whether the pass succeeded
    pub success: bool,

    /// Error message if failed
    pub error: Option<String>,

    /// Triples added (for CONSTRUCT passes)
    pub triples_added: usize,

    /// Files generated (for emission passes)
    pub files_generated: Vec<PathBuf>,

    /// Execution duration
    pub duration: Duration,
}

impl PassResult {
    /// Create a successful pass result
    pub fn success() -> Self {
        Self {
            success: true,
            error: None,
            triples_added: 0,
            files_generated: Vec::new(),
            duration: Duration::ZERO,
        }
    }

    /// Create a failed pass result
    pub fn failure(error: impl Into<String>) -> Self {
        Self {
            success: false,
            error: Some(error.into()),
            triples_added: 0,
            files_generated: Vec::new(),
            duration: Duration::ZERO,
        }
    }

    /// Set triples added
    pub fn with_triples(mut self, count: usize) -> Self {
        self.triples_added = count;
        self
    }

    /// Set files generated
    pub fn with_files(mut self, files: Vec<PathBuf>) -> Self {
        self.files_generated = files;
        self
    }

    /// Set execution duration
    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.duration = duration;
        self
    }
}

/// Context passed to each pass during execution
pub struct PassContext<'a> {
    /// The RDF graph (ontology + derived triples)
    pub graph: &'a Graph,

    /// Base path for file operations
    pub base_path: PathBuf,

    /// Output directory for generated files
    pub output_dir: PathBuf,

    /// Template bindings from extraction
    pub bindings: BTreeMap<String, serde_json::Value>,

    /// Files generated so far
    pub generated_files: Vec<PathBuf>,

    /// Project name
    pub project_name: String,

    /// Project version
    pub project_version: String,
}

impl<'a> PassContext<'a> {
    /// Create a new pass context
    pub fn new(graph: &'a Graph, base_path: PathBuf, output_dir: PathBuf) -> Self {
        Self {
            graph,
            base_path,
            output_dir,
            bindings: BTreeMap::new(),
            generated_files: Vec::new(),
            project_name: String::new(),
            project_version: String::new(),
        }
    }

    /// Set project metadata
    pub fn with_project(mut self, name: String, version: String) -> Self {
        self.project_name = name;
        self.project_version = version;
        self
    }
}

/// Record of a pass execution for auditing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassExecution {
    /// Pass name
    pub name: String,

    /// Pass type
    pub pass_type: PassType,

    /// Order index
    pub order_index: u32,

    /// Execution duration in milliseconds
    pub duration_ms: u64,

    /// SHA-256 hash of the query executed (if applicable)
    pub query_hash: Option<String>,

    /// Number of triples produced (for CONSTRUCT)
    pub triples_produced: usize,

    /// Files generated (for emission)
    pub files_generated: Vec<PathBuf>,

    /// Whether the pass succeeded
    pub success: bool,

    /// Error message if failed
    pub error: Option<String>,
}

/// Trait for implementing a projection pass
///
/// All passes must guarantee:
/// - **Idempotence**: Running twice equals running once
/// - **Determinism**: Same inputs always produce same outputs
pub trait Pass: Send + Sync {
    /// Get the pass type
    fn pass_type(&self) -> PassType;

    /// Get the pass name
    fn name(&self) -> &str;

    /// Get the execution order index
    fn order_index(&self) -> u32 {
        self.pass_type().order_index()
    }

    /// Whether this pass is idempotent (μ∘μ = μ)
    fn is_idempotent(&self) -> bool {
        true // All v6 passes must be idempotent
    }

    /// Whether this pass is deterministic
    fn is_deterministic(&self) -> bool {
        true // All v6 passes must be deterministic
    }

    /// Execute the pass
    ///
    /// # Arguments
    /// * `ctx` - The pass context with graph and file state
    ///
    /// # Returns
    /// * `Ok(PassResult)` - Pass completed (may have succeeded or failed)
    /// * `Err(Error)` - Fatal error during execution
    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult>;

    /// Create an execution record for this pass
    fn create_execution_record(&self, result: &PassResult) -> PassExecution {
        PassExecution {
            name: self.name().to_string(),
            pass_type: self.pass_type(),
            order_index: self.order_index(),
            duration_ms: result.duration.as_millis() as u64,
            query_hash: None,
            triples_produced: result.triples_added,
            files_generated: result.files_generated.clone(),
            success: result.success,
            error: result.error.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pass_type_ordering() {
        assert!(PassType::Normalization.order_index() < PassType::Extraction.order_index());
        assert!(PassType::Extraction.order_index() < PassType::Emission.order_index());
        assert!(PassType::Emission.order_index() < PassType::Canonicalization.order_index());
        assert!(PassType::Canonicalization.order_index() < PassType::Receipt.order_index());
    }

    #[test]
    fn test_pass_type_names() {
        assert_eq!(PassType::Normalization.name(), "μ₁:normalization");
        assert_eq!(PassType::Extraction.name(), "μ₂:extraction");
        assert_eq!(PassType::Emission.name(), "μ₃:emission");
        assert_eq!(PassType::Canonicalization.name(), "μ₄:canonicalization");
        assert_eq!(PassType::Receipt.name(), "μ₅:receipt");
    }

    #[test]
    fn test_pass_result_success() {
        let result = PassResult::success()
            .with_triples(10)
            .with_duration(Duration::from_millis(100));

        assert!(result.success);
        assert!(result.error.is_none());
        assert_eq!(result.triples_added, 10);
    }

    #[test]
    fn test_pass_result_failure() {
        let result = PassResult::failure("Query failed");

        assert!(!result.success);
        assert_eq!(result.error, Some("Query failed".to_string()));
    }
}
