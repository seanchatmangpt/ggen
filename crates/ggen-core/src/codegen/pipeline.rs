//! Code generation pipeline
//!
//! Orchestrates the full generation flow:
//! 1. Load ontology from manifest
//! 2. Execute inference rules (CONSTRUCT queries)
//! 3. Execute generation rules (SELECT → Template → Code)
//! 4. Validate outputs
//! 5. Write files and audit trail

use crate::graph::Graph;
use crate::manifest::{GgenManifest, InferenceRule};
use ggen_utils::error::{Error, Result};
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Instant;

/// Tracks execution state through the generation pipeline
pub struct PipelineState {
    /// Loaded manifest
    pub manifest: GgenManifest,

    /// Domain ontology graph
    pub ontology_graph: Graph,

    /// Code graph (built by CONSTRUCT)
    pub code_graph: Graph,

    /// Inference rules executed
    pub executed_rules: Vec<ExecutedRule>,

    /// Generated files
    pub generated_files: Vec<GeneratedFile>,

    /// Validation results
    pub validation_results: Vec<ValidationResult>,

    /// Pipeline start time
    pub started_at: Instant,
}

/// Record of an executed rule
#[derive(Debug, Clone, Serialize)]
pub struct ExecutedRule {
    /// Rule name
    pub name: String,

    /// Rule type (inference or generation)
    pub rule_type: RuleType,

    /// Number of triples added to graph
    pub triples_added: usize,

    /// Execution duration in milliseconds
    pub duration_ms: u64,

    /// Hash of the query for audit
    pub query_hash: String,
}

/// Type of rule executed
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum RuleType {
    /// Inference rule (CONSTRUCT for graph enrichment)
    Inference,
    /// Generation rule (SELECT → Template)
    Generation,
}

/// Record of a generated file
#[derive(Debug, Clone, Serialize)]
pub struct GeneratedFile {
    /// Output file path
    pub path: PathBuf,

    /// SHA256 hash of content
    pub content_hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Name of the rule that generated this file
    pub source_rule: String,
}

/// Validation result for a single check
#[derive(Debug, Clone, Serialize)]
pub struct ValidationResult {
    /// Rule name that was checked
    pub rule_name: String,

    /// Whether validation passed
    pub passed: bool,

    /// Optional message (usually for failures)
    pub message: Option<String>,

    /// Severity of the validation
    pub severity: ValidationSeverity,
}

/// Severity of validation failure
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum ValidationSeverity {
    /// Fails generation
    Error,
    /// Logged but continues
    Warning,
}

/// The main generation pipeline
pub struct GenerationPipeline {
    /// Parsed manifest
    manifest: GgenManifest,

    /// Base path for resolving relative paths
    base_path: PathBuf,

    /// Ontology graph (loaded from manifest)
    ontology_graph: Option<Graph>,

    /// Code graph (built by CONSTRUCT rules)
    code_graph: Option<Graph>,

    /// Executed inference rules
    executed_rules: Vec<ExecutedRule>,

    /// Generated files
    generated_files: Vec<GeneratedFile>,

    /// Validation results
    validation_results: Vec<ValidationResult>,

    /// Pipeline start time
    started_at: Instant,
}

impl GenerationPipeline {
    /// Create a new generation pipeline from a manifest
    ///
    /// # Arguments
    /// * `manifest` - Parsed ggen.toml manifest
    /// * `base_path` - Base path for resolving relative paths
    pub fn new(manifest: GgenManifest, base_path: PathBuf) -> Self {
        Self {
            manifest,
            base_path,
            ontology_graph: None,
            code_graph: None,
            executed_rules: Vec::new(),
            generated_files: Vec::new(),
            validation_results: Vec::new(),
            started_at: Instant::now(),
        }
    }

    /// Load ontology from manifest configuration
    pub fn load_ontology(&mut self) -> Result<()> {
        let graph = Graph::new()?;

        // Load primary ontology source
        let source_path = self.base_path.join(&self.manifest.ontology.source);
        let content = std::fs::read_to_string(&source_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read ontology '{}': {}",
                source_path.display(),
                e
            ))
        })?;
        graph.insert_turtle(&content)?;

        // Load imports
        for import in &self.manifest.ontology.imports {
            let import_path = self.base_path.join(import);
            let import_content = std::fs::read_to_string(&import_path).map_err(|e| {
                Error::new(&format!(
                    "Failed to read ontology import '{}': {}",
                    import_path.display(),
                    e
                ))
            })?;
            graph.insert_turtle(&import_content)?;
        }

        self.ontology_graph = Some(graph);
        Ok(())
    }

    /// Execute all inference rules in order
    pub fn execute_inference_rules(&mut self) -> Result<Vec<ExecutedRule>> {
        let mut executed = Vec::new();

        // Sort rules by order
        let mut rules: Vec<_> = self.manifest.inference.rules.clone();
        rules.sort_by_key(|r| r.order);

        for rule in rules {
            let result = self.execute_inference_rule(&rule)?;
            executed.push(result);
        }

        self.executed_rules.extend(executed.clone());
        Ok(executed)
    }

    /// Execute a single inference rule
    fn execute_inference_rule(&mut self, rule: &InferenceRule) -> Result<ExecutedRule> {
        let start = Instant::now();

        // TODO: Implement actual CONSTRUCT execution via ConstructExecutor
        // For now, this is a stub that records the execution

        let duration = start.elapsed();
        let query_hash = format!("{:x}", sha2::Sha256::digest(rule.construct.as_bytes()));

        Ok(ExecutedRule {
            name: rule.name.clone(),
            rule_type: RuleType::Inference,
            triples_added: 0, // Will be populated by actual execution
            duration_ms: duration.as_millis() as u64,
            query_hash,
        })
    }

    /// Execute all generation rules
    pub fn execute_generation_rules(&mut self) -> Result<Vec<GeneratedFile>> {
        let generated = Vec::new();

        for rule in &self.manifest.generation.rules {
            // TODO: Implement actual generation
            // 1. Load query from QuerySource
            // 2. Execute SELECT query
            // 3. Load template from TemplateSource
            // 4. Render template with query results
            // 5. Write to output_file (with variable expansion)

            let _ = rule; // Placeholder to use variable
        }

        self.generated_files.extend(generated.clone());
        Ok(generated)
    }

    /// Run the complete pipeline
    ///
    /// # Returns
    /// * `Ok(PipelineState)` - Pipeline completed successfully
    /// * `Err(Error)` - Pipeline failed at some stage
    pub fn run(&mut self) -> Result<PipelineState> {
        // 1. Load ontology
        self.load_ontology()?;

        // 2. Execute inference rules
        self.execute_inference_rules()?;

        // 3. Execute generation rules
        self.execute_generation_rules()?;

        // 4. Build final state
        let state = PipelineState {
            manifest: self.manifest.clone(),
            ontology_graph: self.ontology_graph.take().unwrap_or_else(|| {
                Graph::new().expect("Failed to create fallback graph")
            }),
            code_graph: self.code_graph.take().unwrap_or_else(|| {
                Graph::new().expect("Failed to create fallback graph")
            }),
            executed_rules: self.executed_rules.clone(),
            generated_files: self.generated_files.clone(),
            validation_results: self.validation_results.clone(),
            started_at: self.started_at,
        };

        Ok(state)
    }

    /// Render a template with the given context
    pub fn render_template(
        _template: &crate::manifest::TemplateSource,
        _context: &tera::Context,
    ) -> Result<String> {
        // TODO: Implement template rendering using existing template.rs infrastructure
        Ok(String::new())
    }

    /// Expand output path pattern with variables
    pub fn expand_output_path(pattern: &str, context: &BTreeMap<String, String>) -> PathBuf {
        let mut result = pattern.to_string();
        for (key, value) in context {
            let placeholder = format!("{{{{{}}}}}", key);
            result = result.replace(&placeholder, value);
        }
        PathBuf::from(result)
    }
}

// Import sha2 for query hashing
use sha2::Digest;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_output_path() {
        let mut ctx = BTreeMap::new();
        ctx.insert("name".to_string(), "user".to_string());
        ctx.insert("module".to_string(), "models".to_string());

        let result = GenerationPipeline::expand_output_path("src/{{module}}/{{name}}.rs", &ctx);
        assert_eq!(result, PathBuf::from("src/models/user.rs"));
    }

    #[test]
    fn test_expand_output_path_no_vars() {
        let ctx = BTreeMap::new();
        let result = GenerationPipeline::expand_output_path("src/fixed.rs", &ctx);
        assert_eq!(result, PathBuf::from("src/fixed.rs"));
    }
}
