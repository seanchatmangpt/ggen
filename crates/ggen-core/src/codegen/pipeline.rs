//! Code generation pipeline
//!
//! Orchestrates the full generation flow:
//! 1. Load ontology from manifest
//! 2. Execute inference rules (CONSTRUCT queries)
//! 3. Execute generation rules (SELECT → Template → Code)
//! 4. Validate outputs
//! 5. Write files and audit trail

use crate::codegen::transaction::FileTransaction;
use crate::graph::{ConstructExecutor, Graph};
use crate::manifest::{GenerationRule, GgenManifest, InferenceRule};
use ggen_utils::error::{Error, Result};
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Instant;

// ============================================================================
// LLM Service Trait (Dependency Injection)
// ============================================================================

/// Trait for LLM-based code generation services.
///
/// This trait allows injecting LLM functionality from the CLI layer
/// (ggen-cli depends on ggen-ai, avoiding cyclic dependency with ggen-core).
///
/// # Architecture Note
/// ggen-core cannot depend on ggen-ai (would create cyclic dependency).
/// Implementations of this trait should be provided by ggen-cli or ggen-ai.
pub trait LlmService: Send + Sync {
    /// Generate skill implementation code using LLM.
    ///
    /// # Arguments
    /// * `skill_name` - Name of the skill to implement
    /// * `system_prompt` - Description of what the skill does
    /// * `implementation_hint` - Hint about how to implement it
    /// * `language` - Target programming language (rust, elixir, typescript, etc.)
    ///
    /// # Returns
    /// * `Ok(String)` - Generated implementation code
    /// * `Err(Box<dyn Error>)` - Generation failed
    fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>>;

    /// Clone the service for use in async contexts
    fn clone_box(&self) -> Box<dyn LlmService>;
}

// ============================================================================
// Global LLM Service Storage
// ============================================================================

/// Global LLM service slot for dependency injection from CLI layer.
///
/// This allows the CLI to set an LLM service that can be used by the codegen
/// pipeline without creating a cyclic dependency (ggen-core → ggen-ai).
///
/// # Thread Safety
/// Uses Arc<Mutex<>> for safe concurrent access from multiple threads.
///
/// # Example
/// ```rust
/// // In CLI layer (ggen-cli):
/// let service = Box::new(GroqLlmService::new(api_key));
/// set_llm_service(service);
///
/// // In codegen pipeline:
/// if let Some(service) = get_llm_service() {
///     let code = service.generate_skill_impl(...)?;
/// }
/// ```
static GLOBAL_LLM_SERVICE: once_cell::sync::Lazy<Arc<Mutex<Option<Box<dyn LlmService>>>>> =
    once_cell::sync::Lazy::new(|| Arc::new(Mutex::new(None)));

/// Set the global LLM service for code generation.
///
/// This function should be called from the CLI layer to inject an LLM service
/// implementation (e.g., from ggen-ai) into the codegen pipeline.
///
/// # Arguments
/// * `service` - Boxed LLM service implementation
///
/// # Example
/// ```rust
/// use ggen_core::codegen::pipeline::set_llm_service;
/// use ggen_ai::GroqLlmService;
///
/// let service = Box::new(GroqLlmService::new("api_key"));
/// set_llm_service(service);
/// ```
pub fn set_llm_service(service: Box<dyn LlmService>) {
    let mut svc = GLOBAL_LLM_SERVICE.lock().unwrap();
    *svc = Some(service);
}

/// Get the global LLM service for code generation.
///
/// Returns None if no service has been set via `set_llm_service()`.
///
/// # Returns
/// * `Some(Box<dyn LlmService>)` - Cloned LLM service if available
/// * `None` - No LLM service configured
///
/// # Example
/// ```rust
/// use ggen_core::codegen::pipeline::get_llm_service;
///
/// if let Some(service) = get_llm_service() {
///     let code = service.generate_skill_impl("my_skill", "desc", "hint", "rust")?;
/// } else {
///     // Fallback to TODO stubs
/// }
/// ```
pub fn get_llm_service() -> Option<Box<dyn LlmService>> {
    let svc = GLOBAL_LLM_SERVICE.lock().unwrap();
    svc.as_ref().map(|s| s.clone_box())
}

/// Default LLM service that returns TODO stubs (used when no LLM is configured).
#[derive(Debug, Clone)]
struct DefaultLlmService;

impl LlmService for DefaultLlmService {
    fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
        let stub = match language {
            "rust" => format!(
                "// TODO: Implement {} skill (Rust)\n// Description: {}\n// Hint: {}\n\
                 // Note: LLM auto-generation is not configured",
                skill_name, system_prompt, implementation_hint
            ),
            "elixir" => format!(
                "# TODO: Implement {} skill (Elixir)\n# Description: {}\n# Hint: {}\n\
                 # Note: LLM auto-generation is not configured",
                skill_name, system_prompt, implementation_hint
            ),
            "typescript" | "javascript" => format!(
                "// TODO: Implement {} skill (TypeScript/JavaScript)\n\
                 // Description: {}\n// Hint: {}\n\
                 // Note: LLM auto-generation is not configured",
                skill_name, system_prompt, implementation_hint
            ),
            _ => format!(
                "// TODO: Implement {} skill ({})\n// Description: {}\n// Hint: {}",
                skill_name, language, system_prompt, implementation_hint
            ),
        };

        Ok(stub)
    }

    fn clone_box(&self) -> Box<dyn LlmService> {
        Box::new(self.clone())
    }
}

/// Tracks execution state through the generation pipeline
pub struct PipelineState {
    /// Loaded manifest
    pub manifest: GgenManifest,

    /// Domain ontology graph
    pub ontology_graph: Graph,

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

    /// Executed inference rules
    executed_rules: Vec<ExecutedRule>,

    /// Generated files
    generated_files: Vec<GeneratedFile>,

    /// Validation results
    validation_results: Vec<ValidationResult>,

    /// Pipeline start time
    started_at: Instant,

    /// Force overwrite of protected files (bypasses poka-yoke protection)
    force_overwrite: bool,

    /// Optional CLI --output-dir override
    output_dir_override: Option<PathBuf>,

    /// Optional LLM service for auto-generating skill implementations
    llm_service: Option<Box<dyn LlmService>>,
}

/// Clean a SPARQL term string representation.
///
/// Converts oxigraph's term representation to plain values:
/// - IRIs: `<http://example.org/>` -> `http://example.org/`
/// - Literals: `"value"` or `"value"^^<xsd:string>` -> `value`
/// - Language-tagged: `"value"@en` -> `value`
fn clean_sparql_term(value: &str) -> String {
    if value.starts_with('<') && value.ends_with('>') {
        // IRI: strip angle brackets
        value[1..value.len() - 1].to_string()
    } else if let Some(without_prefix) = value.strip_prefix('"') {
        // Literal: strip quotes and optional datatype/language tag
        if let Some(quote_end) = without_prefix.find('"') {
            without_prefix[..quote_end].to_string()
        } else {
            value.to_string()
        }
    } else {
        value.to_string()
    }
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
            executed_rules: Vec::new(),
            generated_files: Vec::new(),
            validation_results: Vec::new(),
            started_at: Instant::now(),
            force_overwrite: false,
            output_dir_override: None,
            llm_service: None,
        }
    }

    /// Set LLM service for auto-generating skill implementations
    ///
    /// # Arguments
    /// * `service` - Optional boxed LLM service (None = use default TODO stubs)
    pub fn set_llm_service(&mut self, service: Option<Box<dyn LlmService>>) {
        self.llm_service = service;
    }

    /// Set force overwrite flag (bypasses protected_paths check)
    pub fn set_force_overwrite(&mut self, force: bool) {
        self.force_overwrite = force;
    }

    /// Set output directory override (from CLI --output-dir)
    pub fn set_output_dir(&mut self, output_dir: PathBuf) {
        self.output_dir_override = Some(output_dir);
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

    /// Execute a single inference rule (CONSTRUCT query with materialization)
    fn execute_inference_rule(&mut self, rule: &InferenceRule) -> Result<ExecutedRule> {
        let start = Instant::now();

        // Get the ontology graph (must be loaded)
        let graph = self
            .ontology_graph
            .as_ref()
            .ok_or_else(|| Error::new("Ontology graph not loaded. Call load_ontology() first."))?;

        // T019: Conditional execution - Check 'when' condition if present
        if let Some(ref when_query) = rule.when {
            if !self.evaluate_condition(when_query)? {
                // Condition failed - skip this rule
                return Ok(ExecutedRule {
                    name: rule.name.clone(),
                    rule_type: RuleType::Inference,
                    triples_added: 0, // Skipped
                    duration_ms: start.elapsed().as_millis() as u64,
                    query_hash: "skipped".to_string(),
                });
            }
        }

        // Create executor and run CONSTRUCT with materialization
        let executor = ConstructExecutor::new(graph);
        let triples_added = executor
            .execute_and_materialize(&rule.construct)
            .map_err(|e| Error::new(&format!("Inference rule '{}' failed: {}", rule.name, e)))?;

        let duration = start.elapsed();
        let query_hash = format!("{:x}", sha2::Sha256::digest(rule.construct.as_bytes()));

        Ok(ExecutedRule {
            name: rule.name.clone(),
            rule_type: RuleType::Inference,
            triples_added,
            duration_ms: duration.as_millis() as u64,
            query_hash,
        })
    }

    /// Evaluate a SPARQL ASK condition query
    ///
    /// Returns `true` if condition passes (ASK returns true), `false` otherwise.
    fn evaluate_condition(&self, ask_query: &str) -> Result<bool> {
        use oxigraph::sparql::QueryResults;

        let graph = self
            .ontology_graph
            .as_ref()
            .ok_or_else(|| Error::new("Ontology graph not loaded"))?;

        let results = graph
            .query(ask_query)
            .map_err(|e| Error::new(&format!("Condition query failed: {}", e)))?;

        match results {
            QueryResults::Boolean(result) => Ok(result),
            _ => Err(Error::new(
                "Condition query must be ASK query (got SELECT/CONSTRUCT)",
            )),
        }
    }

    /// Execute all generation rules (SELECT → Template → Code)
    pub fn execute_generation_rules(&mut self) -> Result<Vec<GeneratedFile>> {
        use crate::manifest::{GenerationMode, QuerySource, TemplateSource};
        use oxigraph::sparql::QueryResults;

        let mut generated = Vec::new();

        // Get the ontology graph (must be loaded)
        let graph = self
            .ontology_graph
            .as_ref()
            .ok_or_else(|| Error::new("Ontology graph not loaded. Call load_ontology() first."))?;

        // Clone rules to avoid borrow conflict
        let rules = self.manifest.generation.rules.clone();
        // Join output_dir with base_path to make it relative to manifest location
        let output_dir = if let Some(ref override_dir) = self.output_dir_override {
            self.base_path.join(override_dir)
        } else {
            self.base_path.join(&self.manifest.generation.output_dir)
        };

        // Create transaction for atomic file operations
        let mut transaction = FileTransaction::new()?;

        for rule in &rules {
            let start = Instant::now();

            // T019: Check WHEN condition if present (SPARQL ASK)
            if let Some(when_query) = &rule.when {
                if !self.evaluate_condition(when_query)? {
                    // Condition failed - skip this rule silently
                    continue;
                }
            }

            // 1. Load query from QuerySource
            let query = match &rule.query {
                QuerySource::File { file } => {
                    let query_path = self.base_path.join(file);
                    std::fs::read_to_string(&query_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read query file '{}': {}",
                            query_path.display(),
                            e
                        ))
                    })?
                }
                QuerySource::Inline { inline } => inline.clone(),
            };

            // 2. Execute SELECT query
            let results = graph.query(&query).map_err(|e| {
                Error::new(&format!(
                    "Generation rule '{}' query failed: {}",
                    rule.name, e
                ))
            })?;

            // Convert query results to rows for template rendering
            // Values are cleaned to remove RDF serialization syntax (quotes, XSD types, etc.)
            let rows = match results {
                QueryResults::Solutions(solutions) => {
                    let mut rows = Vec::new();
                    for solution in solutions {
                        let solution = solution
                            .map_err(|e| Error::new(&format!("SPARQL solution error: {}", e)))?;
                        let mut row = BTreeMap::new();
                        for (var, term) in solution.iter() {
                            // Strip leading '?' so templates use {{ var }} not {{ ?var }}
                            let key = var.to_string();
                            let clean_key = key.strip_prefix('?').unwrap_or(&key).to_string();
                            row.insert(clean_key, clean_sparql_term(&term.to_string()));
                        }
                        rows.push(row);
                    }
                    rows
                }
                _ => {
                    return Err(Error::new(&format!(
                        "Generation rule '{}' query must be SELECT (got CONSTRUCT/ASK)",
                        rule.name
                    )));
                }
            };

            // 3. Skip if empty and skip_empty is set
            if rows.is_empty() && rule.skip_empty {
                continue;
            }

            // 4. Load template from TemplateSource
            let (template_content, _template_source_info) = match &rule.template {
                TemplateSource::File { file } => {
                    let template_path = self.base_path.join(file);
                    let content = std::fs::read_to_string(&template_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read template file '{}': {}",
                            template_path.display(),
                            e
                        ))
                    })?;
                    (content, format!("file '{}'", template_path.display()))
                }
                TemplateSource::Inline { inline } => {
                    (inline.clone(), "inline template".to_string())
                }
            };

            // 5. For each row, render template and generate file
            let mut tera = tera::Tera::default();
            crate::register::register_all(&mut tera);
            tera.add_raw_template("generation_rule", &template_content)
                .map_err(|e| {
                    Error::new(&format!(
                        "Template parse error in rule '{}': {}",
                        rule.name, e
                    ))
                })?;

            for row in &rows {
                // Build context from row
                let mut context = tera::Context::new();

                for (key, value) in row {
                    // Strip leading '?' from SPARQL variable names
                    let clean_key = key.strip_prefix('?').unwrap_or(key);

                    // Values already cleaned during SPARQL result collection (line 357)
                    context.insert(clean_key, value.as_str());
                }

                // Also insert sparql_results for advanced templates
                let results_json = serde_json::json!(rows);
                context.insert("sparql_results", &results_json);

                // LLM Generation: Auto-generate skill implementations if enabled
                if self.manifest.generation.enable_llm {
                    // Look for skill-specific fields in SPARQL results
                    let skill_name = row
                        .get("?skill_name")
                        .or_else(|| row.get("skill_name"))
                        .map(|s| s.as_str())
                        .unwrap_or("");

                    let system_prompt = row
                        .get("?system_prompt")
                        .or_else(|| row.get("system_prompt"))
                        .or_else(|| row.get("?skill_description"))
                        .map(|s| s.as_str())
                        .unwrap_or("");

                    let implementation_hint = row
                        .get("?implementation_hint")
                        .or_else(|| row.get("implementation_hint"))
                        .map(|s| s.as_str())
                        .unwrap_or("Implement this skill");

                    // Detect language from output file extension or SPARQL results
                    let language = row
                        .get("?language")
                        .or_else(|| row.get("language"))
                        .or_else(|| row.get("?target_language"))
                        .map(|s| s.as_str())
                        .unwrap_or_else(|| {
                            // Fallback: detect from output file extension
                            let output_ext = rule.output_file.rsplit('.').next().unwrap_or("");
                            match output_ext {
                                "rs" => "rust",
                                "ex" | "exs" => "elixir",
                                "ts" => "typescript",
                                "js" => "javascript",
                                "go" => "go",
                                "java" => "java",
                                _ => "rust", // Default to Rust
                            }
                        });

                    // Generate skill implementation using injected LLM service
                    if !skill_name.is_empty() && !system_prompt.is_empty() {
                        match self.generate_skill_impl(
                            skill_name,
                            system_prompt,
                            implementation_hint,
                            language,
                        ) {
                            Ok(generated_code) => {
                                context.insert("generated_impl", &generated_code);
                            }
                            Err(e) => {
                                // LLM generation failed - log warning but don't block generation
                                eprintln!(
                                    "Warning: LLM generation failed for skill '{}': {}. Using TODO stub.",
                                    skill_name, e
                                );
                                context.insert(
                                    "generated_impl",
                                    &format!(
                                        "// TODO: Implement {} skill: {}\n// Hint: {}\n// Note: LLM generation failed",
                                        skill_name, system_prompt, implementation_hint
                                    ),
                                );
                            }
                        }
                    }
                }

                // Render template
                let rendered = tera.render("generation_rule", &context).map_err(|e| {
                    // Build comprehensive error context for debugging
                    let var_names: Vec<String> = row
                        .keys()
                        .map(|k| k.strip_prefix('?').unwrap_or(k).to_string())
                        .collect();

                    // Format row values for display (limit value length to prevent spam)
                    let row_values: Vec<String> = row
                        .iter()
                        .map(|(k, v)| {
                            let clean_key = k.strip_prefix('?').unwrap_or(k);
                            let display_value = if v.len() > 100 {
                                format!("{}...", &v[..100])
                            } else {
                                v.clone()
                            };
                            format!("{} = \"{}\"", clean_key, display_value)
                        })
                        .collect();

                    // Build error chain for debugging
                    let mut error_chain = format!("{}", e);
                    let mut source = std::error::Error::source(&e);
                    while let Some(cause) = source {
                        error_chain.push_str(&format!("\n  Caused by: {}", cause));
                        source = std::error::Error::source(cause);
                    }

                    Error::new(&format!(
                        "Failed to render template for rule '{}': {}\n\
                         Template source: {}\n\
                         Available variables: {}\n\
                         Row values:\n  {}",
                        rule.name,
                        error_chain,
                        _template_source_info,
                        var_names.join(", "),
                        row_values.join("\n  ")
                    ))
                })?;

                // Expand output path with Tera (supports filters like {{ name | lower }})
                let output_path_rendered =
                    tera.render_str(&rule.output_file, &context).map_err(|e| {
                        Error::new(&format!(
                            "Output path template error in rule '{}': {}",
                            rule.name, e
                        ))
                    })?;
                let full_output_path = output_dir.join(&output_path_rendered);

                // T015-T016: Check generation mode and apply merge logic
                let final_content = match rule.mode {
                    GenerationMode::Create => {
                        if full_output_path.exists() {
                            // Skip - file already exists
                            continue;
                        }
                        rendered.clone()
                    }
                    GenerationMode::Overwrite => rendered.clone(),
                    GenerationMode::Merge => {
                        // Merge mode: preserve manual sections
                        if full_output_path.exists() {
                            let existing =
                                std::fs::read_to_string(&full_output_path).map_err(|e| {
                                    Error::new(&format!(
                                        "Failed to read existing file for merge '{}': {}",
                                        full_output_path.display(),
                                        e
                                    ))
                                })?;
                            crate::codegen::merge::merge_sections(&rendered, &existing)?
                        } else {
                            // First time - wrap in markers
                            crate::codegen::merge::merge_sections(&rendered, "")?
                        }
                    }
                };

                // Validate generated output before writing
                Self::validate_generated_output(
                    &final_content,
                    full_output_path.as_path(),
                    &rule.name,
                )?;

                // Write file atomically with automatic rollback on failure
                // FileTransaction handles parent directory creation internally
                transaction.write_file(&full_output_path, &final_content)?;

                // Record generated file
                let content_hash = format!("{:x}", sha2::Sha256::digest(final_content.as_bytes()));
                generated.push(GeneratedFile {
                    path: full_output_path,
                    content_hash,
                    size_bytes: final_content.len(),
                    source_rule: rule.name.clone(),
                });
            }

            // Record rule execution
            let duration = start.elapsed();
            let query_hash = format!("{:x}", sha2::Sha256::digest(query.as_bytes()));
            self.executed_rules.push(ExecutedRule {
                name: rule.name.clone(),
                rule_type: RuleType::Generation,
                triples_added: 0, // Generation rules don't add triples
                duration_ms: duration.as_millis() as u64,
                query_hash,
            });
        }

        // Commit transaction - all files written successfully
        // If any error occurred above, transaction will auto-rollback on drop
        let _receipt = transaction.commit()?;

        self.generated_files.extend(generated.clone());
        Ok(generated)
    }

    /// Execute a single generation rule (for use with --rule filter)
    pub fn execute_generation_rule(&mut self, rule: &GenerationRule) -> Result<Vec<GeneratedFile>> {
        // Clone manifest and set rules to just this one
        let original_rules = std::mem::take(&mut self.manifest.generation.rules);
        self.manifest.generation.rules = vec![rule.clone()];

        let result = self.execute_generation_rules();

        // Restore original rules
        self.manifest.generation.rules = original_rules;

        result
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
            ontology_graph: self
                .ontology_graph
                .take()
                .ok_or_else(|| Error::new("Ontology graph not initialized"))?,
            executed_rules: self.executed_rules.clone(),
            generated_files: self.generated_files.clone(),
            validation_results: self.validation_results.clone(),
            started_at: self.started_at,
        };

        Ok(state)
    }

    /// Expand output path pattern with variables
    ///
    /// SPARQL variable names may have a `?` prefix which is stripped for template matching.
    /// The value is also cleaned (IRI angle brackets and literal quotes removed).
    pub fn expand_output_path(pattern: &str, context: &BTreeMap<String, String>) -> PathBuf {
        let mut result = pattern.to_string();
        for (key, value) in context {
            // Strip '?' prefix from SPARQL variable names
            let clean_key = key.strip_prefix('?').unwrap_or(key);

            // Clean value: strip IRI angle brackets or literal quotes
            let clean_value = if value.starts_with('<') && value.ends_with('>') {
                &value[1..value.len() - 1]
            } else if let Some(without_prefix) = value.strip_prefix('"') {
                if let Some(quote_end) = without_prefix.find('"') {
                    &without_prefix[..quote_end]
                } else {
                    value.as_str()
                }
            } else {
                value.as_str()
            };

            let placeholder = format!("{{{{{}}}}}", clean_key);
            result = result.replace(&placeholder, clean_value);
        }
        PathBuf::from(result)
    }

    /// Validate generated output before writing to filesystem
    ///
    /// Enforces HDOC validation requirements:
    /// - Content must not be empty
    /// - File size must be under 10MB
    /// - Path must not contain traversal patterns (../)
    ///
    /// # Arguments
    /// * `content` - The generated content to validate
    /// * `path` - The output path to validate
    /// * `rule_id` - The rule name for error reporting
    ///
    /// # Errors
    /// Returns descriptive errors with rule_id and path for HDOC framework
    fn validate_generated_output(content: &str, path: &Path, rule_id: &str) -> Result<()> {
        // Check 1: Content must not be empty
        if content.is_empty() {
            return Err(Error::new(&format!(
                "Validation failed for rule '{}': Generated content is empty for path '{}'",
                rule_id,
                path.display()
            )));
        }

        // Check 2: File size must be under 10MB (10 * 1024 * 1024 bytes)
        const MAX_SIZE_BYTES: usize = 10 * 1024 * 1024;
        let size_bytes = content.len();
        if size_bytes > MAX_SIZE_BYTES {
            return Err(Error::new(&format!(
                "Validation failed for rule '{}': Generated file size ({} bytes) exceeds 10MB limit for path '{}'",
                rule_id,
                size_bytes,
                path.display()
            )));
        }

        // Check 3: Path must not contain traversal patterns
        let path_str = path.to_string_lossy();
        if path_str.contains("../") || path_str.contains("..\\") {
            return Err(Error::new(&format!(
                "Validation failed for rule '{}': Path contains directory traversal pattern (..) in '{}'",
                rule_id,
                path.display()
            )));
        }

        Ok(())
    }

    /// Generate skill implementation using LLM
    ///
    /// This function uses an injected LLM service to generate skill implementations.
    /// If no service is injected or LLM is disabled in manifest, returns a TODO stub.
    ///
    /// # Arguments
    /// * `skill_name` - Name of the skill to implement
    /// * `system_prompt` - Description of what the skill does
    /// * `implementation_hint` - Hint about how to implement it
    /// * `language` - Target programming language
    ///
    /// # Returns
    /// * `Ok(String)` - Generated implementation code (or TODO stub if LLM unavailable)
    /// * `Err(Error)` - Generation failed critically
    ///
    /// # Architecture Note
    /// ggen-core cannot depend on ggen-ai (would create cyclic dependency).
    /// LLM service should be injected from CLI layer via set_llm_service().
    pub fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> Result<String> {
        // Check if LLM is enabled in manifest
        if !self.manifest.generation.enable_llm {
            // Return simple TODO stub if LLM is disabled
            return Ok(format!(
                "// TODO: Implement {} skill: {}\n// Hint: {}",
                skill_name, system_prompt, implementation_hint
            ));
        }

        // Use injected LLM service if available, otherwise use default (TODO stubs)
        let service = self
            .llm_service
            .as_ref()
            .map(|s| s.as_ref())
            .unwrap_or(&DefaultLlmService);

        // Call LLM service (may be real LLM or default TODO stub generator)
        service
            .generate_skill_impl(skill_name, system_prompt, implementation_hint, language)
            .map_err(|e| Error::new(&format!("LLM generation failed: {}", e)))
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

    #[test]
    fn test_validation_rejects_empty_output() {
        // Arrange
        let empty_content = "";
        let path = PathBuf::from("src/output.rs");
        let rule_id = "test_rule";

        // Act
        let result = GenerationPipeline::validate_generated_output(empty_content, &path, rule_id);

        // Assert
        assert!(
            result.is_err(),
            "Expected validation to fail for empty content"
        );
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("empty"),
            "Error message should mention empty content: {}",
            err_msg
        );
        assert!(
            err_msg.contains("test_rule"),
            "Error message should include rule_id: {}",
            err_msg
        );
        assert!(
            err_msg.contains("src/output.rs"),
            "Error message should include path: {}",
            err_msg
        );
    }

    #[test]
    fn test_validation_rejects_path_traversal() {
        // Arrange
        let valid_content = "fn main() {}";
        let traversal_path = PathBuf::from("../../../etc/passwd");
        let rule_id = "malicious_rule";

        // Act
        let result =
            GenerationPipeline::validate_generated_output(valid_content, &traversal_path, rule_id);

        // Assert
        assert!(
            result.is_err(),
            "Expected validation to fail for path traversal"
        );
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("traversal"),
            "Error message should mention path traversal: {}",
            err_msg
        );
        assert!(
            err_msg.contains("malicious_rule"),
            "Error message should include rule_id: {}",
            err_msg
        );
        assert!(
            err_msg.contains(".."),
            "Error message should mention the traversal pattern: {}",
            err_msg
        );
    }

    #[test]
    fn test_validation_accepts_valid_output() {
        // Arrange
        let valid_content = "pub struct User { id: u64 }";
        let path = PathBuf::from("src/models/user.rs");
        let rule_id = "generate_struct";

        // Act
        let result = GenerationPipeline::validate_generated_output(valid_content, &path, rule_id);

        // Assert
        assert!(
            result.is_ok(),
            "Expected validation to pass for valid content: {:?}",
            result
        );
    }

    #[test]
    fn test_validation_rejects_oversized_output() {
        // Arrange
        // Create content larger than 10MB
        let oversized_content = "x".repeat(11 * 1024 * 1024); // 11MB
        let path = PathBuf::from("src/huge.rs");
        let rule_id = "huge_generator";

        // Act
        let result =
            GenerationPipeline::validate_generated_output(&oversized_content, &path, rule_id);

        // Assert
        assert!(
            result.is_err(),
            "Expected validation to fail for oversized content"
        );
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("10MB"),
            "Error message should mention 10MB limit: {}",
            err_msg
        );
        assert!(
            err_msg.contains("huge_generator"),
            "Error message should include rule_id: {}",
            err_msg
        );
    }

    #[test]
    fn test_error_shows_available_variables() {
        // Arrange: Create a minimal test environment
        use std::collections::BTreeMap;
        use tera::Tera;

        let mut tera = Tera::default();
        // Template with undefined variable to trigger error
        let template = "Hello {{ undefined_var }}!";
        tera.add_raw_template("test_template", template).unwrap();

        // Create context with some variables (but not the one template needs)
        let mut context = tera::Context::new();
        context.insert("name", "Alice");
        context.insert("email", "alice@example.com");

        // Simulate row data for error message
        let mut row: BTreeMap<String, String> = BTreeMap::new();
        row.insert("?name".to_string(), "Alice".to_string());
        row.insert("?email".to_string(), "alice@example.com".to_string());

        // Act: Try to render and capture error
        let render_result = tera.render("test_template", &context).map_err(|e| {
            // Simulate the enhanced error message format from execute_generation_rules
            let var_names: Vec<String> = row
                .keys()
                .map(|k| k.strip_prefix('?').unwrap_or(k).to_string())
                .collect();

            let row_values: Vec<String> = row
                .iter()
                .map(|(k, v)| {
                    let clean_key = k.strip_prefix('?').unwrap_or(k);
                    let display_value = if v.len() > 100 {
                        format!("{}...", &v[..100])
                    } else {
                        v.clone()
                    };
                    format!("{} = \"{}\"", clean_key, display_value)
                })
                .collect();

            Error::new(&format!(
                "Failed to render template for rule 'test_rule': {}\n\
                 Template source: inline template\n\
                 Available variables: {}\n\
                 Row values:\n  {}",
                e,
                var_names.join(", "),
                row_values.join("\n  ")
            ))
        });

        // Assert: Verify error contains all expected context
        assert!(render_result.is_err(), "Expected template render to fail");
        let err_msg = render_result.unwrap_err().to_string();

        // Check error message includes available variable names (BTreeMap keeps sorted order)
        assert!(
            err_msg.contains("Available variables: email, name"),
            "Error should list available variables, got: {}",
            err_msg
        );

        // Check error message includes row values
        assert!(
            err_msg.contains("name = \"Alice\""),
            "Error should show row values, got: {}",
            err_msg
        );
        assert!(
            err_msg.contains("email = \"alice@example.com\""),
            "Error should show row values, got: {}",
            err_msg
        );

        // Check error message includes template source info
        assert!(
            err_msg.contains("Template source: inline template"),
            "Error should show template source, got: {}",
            err_msg
        );

        // Check error message includes rule name
        assert!(
            err_msg.contains("test_rule"),
            "Error should include rule name, got: {}",
            err_msg
        );
    }

    // ========================================================================
    // Global LLM Service Tests
    // ========================================================================

    #[test]
    fn test_set_and_get_llm_service() {
        // Arrange: Create a mock LLM service
        struct MockLlmService {
            skill_name: String,
        }

        impl LlmService for MockLlmService {
            fn generate_skill_impl(
                &self, skill_name: &str, _system_prompt: &str, _implementation_hint: &str,
                _language: &str,
            ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
                Ok(format!("// Mock implementation for {}", skill_name))
            }

            fn clone_box(&self) -> Box<dyn LlmService> {
                Box::new(MockLlmService {
                    skill_name: self.skill_name.clone(),
                })
            }
        }

        // Act: Set the global service
        let service = Box::new(MockLlmService {
            skill_name: "test_skill".to_string(),
        });
        set_llm_service(service);

        // Assert: Retrieve and verify
        let retrieved = get_llm_service();
        assert!(retrieved.is_some(), "LLM service should be set");

        let result = retrieved
            .unwrap()
            .generate_skill_impl("test_skill", "desc", "hint", "rust")
            .unwrap();
        assert!(
            result.contains("test_skill"),
            "Generated code should contain skill name"
        );
    }

    #[test]
    fn test_get_llm_service_returns_none_when_not_set() {
        // Arrange: Clear any existing service (by setting a new empty one)
        let mut svc = GLOBAL_LLM_SERVICE.lock().unwrap();
        *svc = None;
        drop(svc);

        // Act: Try to get service
        let retrieved = get_llm_service();

        // Assert: Should return None
        assert!(
            retrieved.is_none(),
            "LLM service should be None when not set"
        );
    }

    #[test]
    fn test_llm_service_clone_box() {
        // Arrange: Create a mock service with state
        struct CloneableLlmService {
            counter: std::sync::Arc<std::sync::atomic::AtomicU32>,
        }

        impl LlmService for CloneableLlmService {
            fn generate_skill_impl(
                &self, skill_name: &str, _system_prompt: &str, _implementation_hint: &str,
                _language: &str,
            ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
                Ok(format!("// Implementation {}", skill_name))
            }

            fn clone_box(&self) -> Box<dyn LlmService> {
                // Increment counter to verify clone was called
                self.counter
                    .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Box::new(CloneableLlmService {
                    counter: std::sync::Arc::clone(&self.counter),
                })
            }
        }

        // Act: Clone the service
        let counter = std::sync::Arc::new(std::sync::atomic::AtomicU32::new(0));
        let service1: Box<dyn LlmService> = Box::new(CloneableLlmService {
            counter: std::sync::Arc::clone(&counter),
        });
        let service2 = service1.clone_box();

        // Assert: Both services should work
        let result1 = service1
            .generate_skill_impl("skill1", "desc", "hint", "rust")
            .unwrap();
        let result2 = service2
            .generate_skill_impl("skill2", "desc", "hint", "rust")
            .unwrap();

        assert!(result1.contains("skill1"));
        assert!(result2.contains("skill2"));
        assert_eq!(
            counter.load(std::sync::atomic::Ordering::SeqCst),
            1,
            "clone_box should have been called once"
        );
    }

    #[test]
    fn test_default_llm_service_generates_todos() {
        // Arrange: Use DefaultLlmService
        let service = DefaultLlmService;

        // Act: Generate implementations for different languages
        let rust_impl = service
            .generate_skill_impl("my_skill", "Do something", "Use async", "rust")
            .unwrap();
        let elixir_impl = service
            .generate_skill_impl("my_skill", "Do something", "Use GenServer", "elixir")
            .unwrap();
        let ts_impl = service
            .generate_skill_impl("my_skill", "Do something", "Use async/await", "typescript")
            .unwrap();

        // Assert: Should all contain TODO markers (case-insensitive check)
        assert!(rust_impl.to_uppercase().contains("TODO"));
        assert!(rust_impl.to_uppercase().contains("RUST"));
        assert!(rust_impl.contains("my_skill"));

        assert!(elixir_impl.to_uppercase().contains("TODO"));
        assert!(elixir_impl.to_uppercase().contains("ELIXIR"));
        assert!(elixir_impl.contains("my_skill"));

        assert!(ts_impl.to_uppercase().contains("TODO"));
        assert!(ts_impl.to_uppercase().contains("TYPESCRIPT"));
        assert!(ts_impl.contains("my_skill"));
    }
}
