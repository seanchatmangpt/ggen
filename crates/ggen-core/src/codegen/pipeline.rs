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
use crate::template_types::Template;
use crate::utils::error::{Error, Result};
use rayon::prelude::*;
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

/// Type alias for the global LLM service storage (reduces type complexity)
type GlobalLlmService = Arc<Mutex<Option<Box<dyn LlmService>>>>;

/// Global LLM service slot for dependency injection from CLI layer.
///
/// This allows the CLI to set an LLM service that can be used by the codegen
/// pipeline without creating a cyclic dependency (ggen-core → ggen-ai).
///
/// # Thread Safety
/// Uses Arc<Mutex<>> for safe concurrent access from multiple threads.
///
/// # Example
/// ```ignore
/// // In CLI layer (ggen-cli):
/// let service = Box::new(GroqLlmService::new(api_key));
/// set_llm_service(service);
///
/// // In codegen pipeline:
/// if let Some(service) = get_llm_service() {
///     let code = service.generate_skill_impl(/* ... */)?;
/// }
/// ```
static GLOBAL_LLM_SERVICE: std::sync::LazyLock<GlobalLlmService> =
    std::sync::LazyLock::new(|| Arc::new(Mutex::new(None)));

/// Set the global LLM service for code generation.
///
/// This function should be called from the CLI layer to inject an LLM service
/// implementation (e.g., from ggen-ai) into the codegen pipeline.
///
/// # Arguments
/// * `service` - Boxed LLM service implementation
///
/// # Example
/// ```ignore
/// // Called from the CLI layer (ggen-ai cannot be a dependency of ggen-core).
/// use crate::codegen::pipeline::set_llm_service;
/// use ggen_ai::GroqLlmService;
///
/// let service = Box::new(GroqLlmService::new("api_key"));
/// set_llm_service(service);
/// ```
pub fn set_llm_service(service: Box<dyn LlmService>) {
    if let Ok(mut svc) = GLOBAL_LLM_SERVICE.lock() {
        *svc = Some(service);
    }
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
/// ```ignore
/// use crate::codegen::pipeline::get_llm_service;
///
/// if let Some(service) = get_llm_service() {
///     let code = service.generate_skill_impl("my_skill", "desc", "hint", "rust")?;
/// } else {
///     // Fallback to TemplateFallback stubs
/// }
/// ```
pub fn get_llm_service() -> Option<Box<dyn LlmService>> {
    let svc = GLOBAL_LLM_SERVICE.lock().ok()?;
    svc.as_ref().map(|s| s.clone_box())
}

/// Fallback service that generates stubs for manual implementation when no LLM is configured.
#[derive(Debug, Clone)]
struct TemplateFallbackService;

impl LlmService for TemplateFallbackService {
    fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
        let stub = match language {
            "rust" => format!(
                "// [ManualImplementation] Implement {} skill (Rust)\n// Description: {}\n// Hint: {}\n\
                 // Note: LLM auto-generation is not configured (TemplateFallback used)",
                skill_name, system_prompt, implementation_hint
            ),
            "elixir" => format!(
                "# [ManualImplementation] Implement {} skill (Elixir)\n# Description: {}\n# Hint: {}\n\
                 # Note: LLM auto-generation is not configured (TemplateFallback used)",
                skill_name, system_prompt, implementation_hint
            ),
            "typescript" | "javascript" => format!(
                "// [ManualImplementation] Implement {} skill (TypeScript/JavaScript)\n\
                 // Description: {}\n// Hint: {}\n\
                 // Note: LLM auto-generation is not configured (TemplateFallback used)",
                skill_name, system_prompt, implementation_hint
            ),
            _ => format!(
                "// [ManualImplementation] Implement {} skill ({})\n// Description: {}\n// Hint: {}\n\
                 // Note: TemplateFallback used",
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
    /// * `service` - Optional boxed LLM service (None = use default TemplateFallback stubs)
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
        // The ontology graph must be loaded before any inference can run, even when
        // the manifest declares no inference rules (mirrors execute_generation_rules).
        if self.ontology_graph.is_none() {
            return Err(Error::new(
                "Ontology graph not loaded. Call load_ontology() first.",
            ));
        }

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
    fn execute_inference_rule(&self, rule: &InferenceRule) -> Result<ExecutedRule> {
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

        // GGEN-INFER-001: warn when a CONSTRUCT rule adds zero new triples.
        // Identity queries (CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }) or
        // queries that match nothing produce zero output silently — this makes
        // misconfigured inference rules very hard to diagnose.
        if triples_added == 0 {
            if self.manifest.validation.strict_mode {
                return Err(Error::new(&format!(
                    "error[GGEN-INFER-001]: Inference rule '{}' added 0 triples\n  \
                     = strict_mode is enabled: identity/no-match CONSTRUCT rules are rejected\n  \
                     = help: Verify the query pattern against the loaded ontology\n  \
                     = help: If intentionally conditional, add a `when` clause",
                    rule.name
                )));
            }
            log::warn!(
                "warning[GGEN-INFER-001]: Inference rule '{}' added 0 triples\n  \
                 = CONSTRUCT query matched no triples in the current graph\n  \
                 = help: Verify the query pattern against the loaded ontology\n  \
                 = help: If intentionally conditional, add a `when` clause to suppress this warning",
                rule.name
            );
        }

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

    /// Prepend declarations for well-known RDF vocabulary prefixes (`rdf`, `rdfs`,
    /// `owl`, `xsd`) to a SPARQL query, but only for prefixes the query does not
    /// already declare. This lets generation and condition queries use standard
    /// vocabulary prefixes without redeclaring them, without risking a duplicate-
    /// prefix parse error when the author already declared one.
    fn with_well_known_prefixes(query: &str) -> String {
        const WELL_KNOWN: &[(&str, &str)] = &[
            ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
            ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
            ("owl", "http://www.w3.org/2002/07/owl#"),
            ("xsd", "http://www.w3.org/2001/XMLSchema#"),
        ];

        let mut prolog = String::new();
        for (pfx, iri) in WELL_KNOWN {
            // Skip if the query already declares this prefix (case-insensitive PREFIX keyword).
            let declares = query.lines().any(|l| {
                let lt = l.trim_start();
                lt.len() >= 6
                    && lt[..6].eq_ignore_ascii_case("prefix")
                    && lt[6..].trim_start().starts_with(&format!("{pfx}:"))
            });
            if !declares {
                prolog.push_str(&format!("PREFIX {pfx}: <{iri}>\n"));
            }
        }

        if prolog.is_empty() {
            query.to_string()
        } else {
            format!("{prolog}{query}")
        }
    }

    /// Evaluate a SPARQL ASK condition query
    fn evaluate_condition(&self, ask_query: &str) -> Result<bool> {
        let graph = self
            .ontology_graph
            .as_ref()
            .ok_or_else(|| Error::new("Ontology graph not loaded"))?;

        Self::evaluate_condition_static(graph, ask_query)
    }

    /// Static version of evaluate_condition for use in parallel contexts
    fn evaluate_condition_static(graph: &Graph, ask_query: &str) -> Result<bool> {
        use oxigraph::sparql::QueryResults;

        let ask_query = Self::with_well_known_prefixes(ask_query);
        let results = graph
            .query(&ask_query)
            .map_err(|e| Error::new(&format!("Condition query failed: {}", e)))?;

        match results {
            QueryResults::Boolean(result) => Ok(result),
            _ => Err(Error::new(
                "error[E0002]: Condition query must return boolean (ASK), not results\n  --> query used in WHEN condition\n  |\n  = help: Change SPARQL query from SELECT/CONSTRUCT to ASK:\n  =   ASK { ... }\n  = help: Conditions must return true/false, not result rows\n  = help: Example: ASK { ?x a :Type }",
            )),
        }
    }

    /// Inject the `generated_impl` template variable for LLM-backed skill generation.
    ///
    /// Reads skill metadata from a SPARQL result `row`, invokes the (possibly
    /// fallback) LLM service, and inserts the resulting code into `context`. On
    /// failure it inserts a deterministic stub instead of propagating the error,
    /// so code generation is never blocked by LLM unavailability. Used by both the
    /// static- and dynamic-output paths of `execute_generation_rules`.
    fn inject_generated_impl(
        &self, row: &BTreeMap<String, String>, rule: &crate::manifest::GenerationRule,
        context: &mut tera::Context,
    ) {
        Self::inject_generated_impl_static(
            row,
            rule,
            context,
            &self.manifest,
            self.llm_service.as_ref().map(|s| s.as_ref()),
        );
    }

    /// Static version of inject_generated_impl for use in parallel contexts
    fn inject_generated_impl_static(
        row: &BTreeMap<String, String>, rule: &crate::manifest::GenerationRule,
        context: &mut tera::Context, manifest: &GgenManifest, llm_service: Option<&dyn LlmService>,
    ) {
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

        // Generate skill implementation using injected LLM service (or fallback stub)
        if !skill_name.is_empty() && !system_prompt.is_empty() {
            // Check if LLM is enabled in manifest
            if !manifest.generation.enable_llm {
                context.insert(
                    "generated_impl",
                    &format!(
                        "// [ManualImplementation] Implement {} skill: {}\n// Hint: {}",
                        skill_name, system_prompt, implementation_hint
                    ),
                );
                return;
            }

            // Use injected LLM service if available, otherwise use default (TemplateFallback stubs)
            let service = llm_service.unwrap_or(&TemplateFallbackService);

            match service.generate_skill_impl(
                skill_name,
                system_prompt,
                implementation_hint,
                language,
            ) {
                Ok(generated_code) => {
                    context.insert("generated_impl", &generated_code);
                }
                Err(e) => {
                    eprintln!(
                        "Warning: LLM generation failed for skill '{}': {}. Using TemplateFallback stub.",
                        skill_name, e
                    );
                    context.insert(
                        "generated_impl",
                        &format!(
                            "// [ManualImplementation] Implement {} skill: {}\n// Hint: {}\n// Note: LLM generation failed (TemplateFallback used)",
                            skill_name, system_prompt, implementation_hint
                        ),
                    );
                }
            }
        }
    }

    /// Resolve a pack `output` key to an absolute directory path using the pack's `package.toml`.
    fn resolve_pack_output(&self, pack_name: &str, output_key: &str) -> std::path::PathBuf {
        Self::resolve_pack_output_static(&self.manifest, &self.base_path, pack_name, output_key)
    }

    /// Static version of resolve_pack_output for use in parallel contexts
    fn resolve_pack_output_static(
        manifest: &GgenManifest, base_path: &Path, pack_name: &str, output_key: &str,
    ) -> std::path::PathBuf {
        if let Some(pack_root) = manifest
            .packs
            .iter()
            .find(|p| p.name == pack_name)
            .and_then(|p| p.path.as_ref())
            .map(|p| base_path.join(p))
        {
            use crate::manifest::types::PackageToml;
            let pkg = PackageToml::load(&pack_root);
            let resolved = pkg.resolve_output_key(output_key);
            pack_root.join(resolved)
        } else {
            // Pack not found in manifest — return output_key literally so the caller
            // surfaces the missing-pack error from the surrounding lookup.
            std::path::PathBuf::from(output_key)
        }
    }

    /// Execute all generation rules (SELECT → Template → Code)
    pub fn execute_generation_rules(&mut self) -> Result<Vec<GeneratedFile>> {
        use crate::manifest::{GenerationMode, QuerySource, TemplateSource};
        use oxigraph::sparql::QueryResults;

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
        let transaction = Arc::new(FileTransaction::new()?);

        // Cache Tera base instance (already thread-safe via clone or immutable shared state)
        let mut tera_base = tera::Tera::default();
        crate::register::register_all(&mut tera_base);
        let tera_base = Arc::new(tera_base);

        // Arc-ify components for parallel execution
        let graph_arc = Arc::new(graph.clone());
        let manifest_arc = Arc::new(self.manifest.clone());
        let base_path_arc = Arc::new(self.base_path.clone());
        let output_dir_arc = Arc::new(output_dir);
        let llm_service_arc = self.llm_service.as_ref().map(|s| Arc::new(s.clone_box()));
        let no_unsafe = self.manifest.validation.no_unsafe;

        let results: Result<Vec<(Vec<GeneratedFile>, Vec<ExecutedRule>)>> = rules.par_iter().map(|rule| {
            let mut generated = Vec::new();
            let mut executed_rules = Vec::new();
            let start = Instant::now();

            // T019: Check WHEN condition if present (SPARQL ASK)
            if let Some(when_query) = &rule.when {
                if !Self::evaluate_condition_static(&graph_arc, when_query)? {
                    // Condition failed - skip this rule silently
                    return Ok((generated, executed_rules));
                }
            }

            // 1. Load query from QuerySource
            let query = match &rule.query {
                QuerySource::File { file } => {
                    let query_path = base_path_arc.join(file);
                    let content = std::fs::read_to_string(&query_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read query file '{}': {}",
                            query_path.display(),
                            e
                        ))
                    })?;
                    // VALUES data must be inline in ggen.toml, never in .rq files
                    if crate::manifest::validation::query_contains_values(&content) {
                        return Err(Error::new(&format!(
                            "error[E0010]: VALUES data must be inline in ggen.toml\n  --> rule: '{}'\n  --> file: {}\n  |\n  = Move VALUES blocks into ggen.toml: `query = {{ inline = \"SELECT ... WHERE {{ VALUES ... }}\" }}`",
                            rule.name,
                            query_path.display()
                        )));
                    }
                    content
                }
                QuerySource::Inline { inline } => inline.clone(),
                QuerySource::Pack { pack, output, file } => {
                    let pack_ref = manifest_arc
                        .packs
                        .iter()
                        .find(|p| p.name == *pack)
                        .ok_or_else(|| {
                            Error::new(&format!(
                                "Generation rule '{}': pack '{}' not declared in [[packs]]",
                                rule.name, pack
                            ))
                        })?;
                    let pack_root = pack_ref.path.as_ref().ok_or_else(|| {
                        Error::new(&format!(
                            "Generation rule '{}': pack '{}' has no local path (registry = '{}')",
                            rule.name, pack, pack_ref.registry
                        ))
                    })?;
                    let _ = pack_root;
                    let pack_dir = Self::resolve_pack_output_static(&manifest_arc, &base_path_arc, pack, output.as_str());
                    let query_path = pack_dir.join(file);
                    std::fs::read_to_string(&query_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read pack query file '{}': {}",
                            query_path.display(),
                            e
                        ))
                    })?
                }
            };

            // 2. Execute SELECT query (with well-known vocabulary prefixes available)
            let query_prefixed = Self::with_well_known_prefixes(&query);
            let results = graph_arc.query(&query_prefixed).map_err(|e| {
                Error::new(&format!(
                    "Generation rule '{}' query failed: {}",
                    rule.name, e
                ))
            })?;

            // Convert query results to rows for template rendering
            let rows = match results {
                QueryResults::Solutions(solutions) => {
                    let mut rows = Vec::new();
                    for solution in solutions {
                        let solution = solution
                            .map_err(|e| Error::new(&format!("SPARQL solution error: {}", e)))?;
                        let mut row = BTreeMap::new();
                        for (var, term) in solution.iter() {
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
                        "error[E0003]: Generation rules require SELECT queries (not CONSTRUCT/ASK)\n  --> rule: '{}'\n  |\n  = help: Change SPARQL query to SELECT to return result rows for template rendering\n  = help: Example: SELECT ?var WHERE {{ ... }}",
                        rule.name
                    )));
                }
            };

            // 3. Skip if empty and skip_empty is set
            if rows.is_empty() && rule.skip_empty {
                return Ok((generated, executed_rules));
            }

            // 4. Load template from TemplateSource
            let (template_content, template_source_info) = match &rule.template {
                TemplateSource::File { file } => {
                    let template_path = base_path_arc.join(file);
                    let content = std::fs::read_to_string(&template_path).map_err(|e| {
                        Error::new(&format!(
                            "error[E0008]: Failed to read template file\n  --> path: '{}'\n  |\n  = error: {}\n  = help: Check if file exists and is readable\n  = help: Verify template path in ggen.toml is relative to project root",
                            template_path.display(),
                            e
                        ))
                    })?;
                    (content, format!("file '{}'", template_path.display()))
                }
                TemplateSource::Inline { inline } => {
                    (inline.clone(), "inline template".to_string())
                }
                TemplateSource::Git { git, branch, path } => {
                    let temp_id = uuid::Uuid::new_v4();
                    let temp_dir = std::env::temp_dir().join(format!("ggen-git-{}", temp_id));
                    let mut cmd = std::process::Command::new("git");
                    cmd.arg("clone").arg("--depth").arg("1");
                    if let Some(b) = branch {
                        cmd.arg("--branch").arg(b);
                    }
                    cmd.arg(git).arg(&temp_dir);

                    let status = cmd
                        .status()
                        .map_err(|e| Error::new(&format!("Failed to execute git clone: {}", e)))?;

                    if !status.success() {
                        return Err(Error::new(&format!(
                            "Failed to clone git repository: {}",
                            git
                        )));
                    }

                    let template_path = temp_dir.join(path);
                    let content = std::fs::read_to_string(&template_path).map_err(|e| {
                        Error::new(&format!("Failed to read template file from git: {}", e))
                    })?;

                    // Clean up temp dir
                    let _ = std::fs::remove_dir_all(temp_dir);

                    (content, format!("git '{}'", git))
                }
                TemplateSource::Package {
                    package,
                    version,
                    path,
                } => {
                    let home = dirs::home_dir()
                        .ok_or_else(|| Error::new("Failed to determine home directory"))?;
                    let mut pack_dir = home.join(".ggen").join("packs").join(package);
                    if let Some(v) = version {
                        pack_dir = pack_dir.join(v);
                    } else {
                        pack_dir = pack_dir.join("latest");
                    }

                    let template_path = pack_dir.join(path);
                    let content = std::fs::read_to_string(&template_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read template file from package {}: {}",
                            package, e
                        ))
                    })?;

                    (content, format!("package '{}'", package))
                }
                TemplateSource::Pack { pack, output, file } => {
                    let pack_ref = manifest_arc
                        .packs
                        .iter()
                        .find(|p| p.name == *pack)
                        .ok_or_else(|| {
                            Error::new(&format!(
                                "Generation rule '{}': pack '{}' not declared in [[packs]]",
                                rule.name, pack
                            ))
                        })?;
                    let pack_root = pack_ref.path.as_ref().ok_or_else(|| {
                        Error::new(&format!(
                            "Generation rule '{}': pack '{}' has no local path (registry = '{}')",
                            rule.name, pack, pack_ref.registry
                        ))
                    })?;
                    let _ = pack_root;
                    let pack_dir = Self::resolve_pack_output_static(&manifest_arc, &base_path_arc, pack, output.as_str());
                    let template_path = pack_dir.join(file);
                    let content = std::fs::read_to_string(&template_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read template from pack '{}' output '{}' file '{}': {}",
                            pack,
                            output,
                            file.display(),
                            e
                        ))
                    })?;
                    (
                        content,
                        format!(
                            "pack '{}' output '{}' file '{}'",
                            pack,
                            output,
                            file.display()
                        ),
                    )
                }
            };

            // 5. Render template and generate file(s)
            let mut tera = (*tera_base).clone();

            // Parse template to strip frontmatter (if any)
            let parsed_template = Template::parse(&template_content).map_err(|e| {
                Error::new(&format!(
                    "Frontmatter parse error in rule '{}': {}",
                    rule.name, e
                ))
            })?;

            tera.add_raw_template("generation_rule", &parsed_template.body)
                .map_err(|e| {
                    Error::new(&format!(
                        "Template parse error in rule '{}': {}",
                        rule.name, e
                    ))
                })?;

            let is_static_output = !rule.output_file.contains("{{");

            // For static output paths with data, build one aggregate context and render once.
            if is_static_output && !rows.is_empty() {
                let results_json = serde_json::json!(rows);
                let mut context = tera::Context::new();
                context.insert("results", &results_json);
                context.insert("sparql_results", &results_json);
                context.insert("entities", &results_json);
                // Expose first-row scalars so single-row specs can use {{ name }} directly
                for (key, value) in &rows[0] {
                    let clean_key = key.strip_prefix('?').unwrap_or(key);
                    context.insert(clean_key, value.as_str());
                }

                Self::inject_generated_impl_static(
                    &rows[0],
                    rule,
                    &mut context,
                    &manifest_arc,
                    llm_service_arc.as_ref().map(|s| s.as_ref().as_ref()),
                );

                // BUG-002: render frontmatter through Tera so {{ }} vars in front.to are resolved
                let mut tpl_with_front = parsed_template.clone();
                if let Err(e) = tpl_with_front.render_frontmatter(&mut tera, &context) {
                    log::warn!("Frontmatter render error in rule '{}': {e}", rule.name);
                }
                let effective_output_file_static = tpl_with_front.front.to.as_deref()
                    .filter(|s| !s.is_empty())
                    .unwrap_or(&rule.output_file);

                let rendered = tera.render("generation_rule", &context).map_err(|e| {
                    Error::new(&format!(
                        "Failed to render template for rule '{}': {}",
                        rule.name, e
                    ))
                })?;

                let full_output_path = output_dir_arc.join(effective_output_file_static);

                let final_content = match rule.mode {
                    GenerationMode::Create => {
                        if full_output_path.exists() {
                            return Err(Error::new(&format!(
                                "error[E0011]: Output file already exists in 'Create' mode\n  --> rule: '{}', output: '{}'\n  |\n  = help: mode=Create requires that the file does not exist\n  = help: Use mode=Overwrite to replace the file, or mode=Merge to combine content",
                                rule.name,
                                full_output_path.display()
                            )));
                        }
                        rendered
                    }
                    GenerationMode::Overwrite => rendered,
                    GenerationMode::Merge => {
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
                            crate::codegen::merge::merge_sections(&rendered, "")?
                        }
                    }
                };

                Self::validate_generated_output(
                    &final_content,
                    full_output_path.as_path(),
                    &rule.name,
                )?;
                if no_unsafe {
                    Self::check_no_unsafe(&final_content, full_output_path.as_path(), &rule.name)?;
                }
                transaction.write_file(&full_output_path, &final_content)?;

                let content_hash = format!("{:x}", sha2::Sha256::digest(final_content.as_bytes()));
                generated.push(GeneratedFile {
                    path: full_output_path,
                    content_hash,
                    size_bytes: final_content.len(),
                    source_rule: rule.name.clone(),
                });

                let duration = start.elapsed();
                let query_hash = format!("{:x}", sha2::Sha256::digest(query.as_bytes()));
                executed_rules.push(ExecutedRule {
                    name: rule.name.clone(),
                    rule_type: RuleType::Generation,
                    triples_added: 0,
                    duration_ms: duration.as_millis() as u64,
                    query_hash,
                });
            } else {
                // Dynamic output paths: render once per row so each row can produce a distinct file
                for row in &rows {
                    // Build context from row
                    let mut context = tera::Context::new();

                    for (key, value) in row {
                        // Strip leading '?' from SPARQL variable names
                        let clean_key = key.strip_prefix('?').unwrap_or(key);
                        context.insert(clean_key, value.as_str());
                    }

                    // Also insert sparql_results and entities (full row list) for batch templates
                    let results_json = serde_json::json!(rows);
                    context.insert("results", &results_json);
                    context.insert("sparql_results", &results_json);
                    context.insert("entities", &results_json);

                    // Inject `generated_impl` (LLM result or stub) when skill fields exist.
                    Self::inject_generated_impl_static(
                        row,
                        rule,
                        &mut context,
                        &manifest_arc,
                        llm_service_arc.as_ref().map(|s| s.as_ref().as_ref()),
                    );

                    // BUG-002: render frontmatter per-row so {{ var }} in front.to resolves to row values
                    let mut row_tpl = parsed_template.clone();
                    if let Err(e) = row_tpl.render_frontmatter(&mut tera, &context) {
                        log::warn!("Frontmatter render error in rule '{}': {e}", rule.name);
                    }
                    let effective_output = row_tpl.front.to.as_deref()
                        .filter(|s| !s.is_empty())
                        .map(str::to_owned);

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
                            template_source_info,
                            var_names.join(", "),
                            row_values.join("\n  ")
                        ))
                    })?;

                    // Expand output path with Tera (supports filters like {{ name | lower }})
                    let output_path_rendered = if let Some(ref eff) = effective_output {
                        eff.clone()
                    } else {
                        tera.render_str(&rule.output_file, &context).map_err(|e| {
                            Error::new(&format!(
                                "Output path template error in rule '{}': {}",
                                rule.name, e
                            ))
                        })?
                    };
                    let full_output_path = output_dir_arc.join(&output_path_rendered);

                    // T015-T016: Check generation mode and apply merge logic
                    let final_content = match rule.mode {
                        GenerationMode::Create => {
                            if full_output_path.exists() {
                                return Err(Error::new(&format!(
                                    "error[E0011]: Output file already exists in 'Create' mode\n  --> rule: '{}', output: '{}'\n  |\n  = help: mode=Create requires that the file does not exist\n  = help: Use mode=Overwrite to replace the file, or mode=Merge to combine content",
                                    rule.name,
                                    full_output_path.display()
                                )));
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
                    if no_unsafe {
                        Self::check_no_unsafe(&final_content, full_output_path.as_path(), &rule.name)?;
                    }

                    // Write file atomically with automatic rollback on failure
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
                executed_rules.push(ExecutedRule {
                    name: rule.name.clone(),
                    rule_type: RuleType::Generation,
                    triples_added: 0,
                    duration_ms: duration.as_millis() as u64,
                    query_hash,
                });
            }

            Ok((generated, executed_rules))
        }).collect();

        let results = results?;
        let mut all_generated = Vec::new();
        for (gen, exec) in results {
            all_generated.extend(gen);
            self.executed_rules.extend(exec);
        }

        // Commit transaction - all files written successfully
        // We use Arc::try_unwrap to get the owned transaction back if possible,
        // or we just call commit on the Arc if we modify FileTransaction (but we didn't).
        // Since rayon join finished, we should be able to unwrap.
        let tx = Arc::try_unwrap(transaction)
            .map_err(|_| Error::new("Transaction still has multiple owners"))?;
        let _receipt = tx.commit()?;

        self.generated_files.extend(all_generated.clone());
        Ok(all_generated)
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

    /// Execute custom validation rules (`[[validation.rules]]`) against the
    /// inferred graph.
    ///
    /// Each manifest rule (`name`/`description`/`ask`/`severity`) is mapped onto
    /// the [`RuleExecutor`] in `crate::validation::sparql_rules` and evaluated
    /// against the current (post-inference) `ontology_graph`.
    ///
    /// ## ASK polarity (executor convention — honored, NOT inverted)
    /// The executor treats **ASK = true ⇒ VALID (pass)** and **ASK = false ⇒
    /// VIOLATION**. This matches the manifest field doc ("SPARQL ASK query
    /// (true = valid)"). Authors who want "a collision EXISTS is a failure" must
    /// therefore write the ASK as a well-formedness assertion, e.g.
    /// `ASK { FILTER NOT EXISTS { <collision pattern> } }` (true when no
    /// collision exists).
    ///
    /// An `Error`-severity violation returns `Err` (blocks generation, before any
    /// files are written). A `Warning`-severity violation is logged and recorded
    /// but does not block.
    pub fn execute_validation_rules(&mut self) -> Result<()> {
        use crate::manifest::types::ValidationSeverity as MSeverity;
        use crate::validation::sparql_rules::{
            RuleExecutor, RuleSeverity, ValidationRule as SparqlRule,
        };
        use crate::validation::violation::Severity as VSeverity;

        let manifest_rules = self.manifest.validation.rules.clone();
        if manifest_rules.is_empty() {
            return Ok(());
        }

        let graph = self
            .ontology_graph
            .as_ref()
            .ok_or_else(|| Error::new("Ontology graph not loaded. Call load_ontology() first."))?;

        // Adapter: manifest::types::ValidationRule -> sparql_rules::ValidationRule
        let sparql_rules: Vec<SparqlRule> = manifest_rules
            .iter()
            .map(|r| {
                let severity = match r.severity {
                    MSeverity::Error => RuleSeverity::Error,
                    MSeverity::Warning => RuleSeverity::Warning,
                };
                SparqlRule::new(
                    r.name.clone(),
                    r.ask.clone(),
                    severity,
                    r.description.clone(),
                )
            })
            .collect();

        let executor = RuleExecutor::new();
        let result = executor
            .execute(graph, &sparql_rules)
            .map_err(|e| Error::new(&format!("Validation rule execution failed: {}", e)))?;

        // Record per-rule outcomes and surface messages.
        let mut error_messages: Vec<String> = Vec::new();
        for v in &result.violations {
            let (sev, is_error) = match v.severity {
                VSeverity::Violation => (ValidationSeverity::Error, true),
                _ => (ValidationSeverity::Warning, false),
            };
            self.validation_results.push(ValidationResult {
                rule_name: v.focus_node.clone(),
                passed: false,
                message: Some(v.message.clone()),
                severity: sev,
            });
            if is_error {
                error_messages.push(format!("{}: {}", v.focus_node, v.message));
            } else {
                log::warn!(
                    "warning[GGEN-VALIDATION]: rule '{}' failed: {}",
                    v.focus_node,
                    v.message
                );
            }
        }

        if !error_messages.is_empty() {
            return Err(Error::new(&format!(
                "error[GGEN-VALIDATION]: {} custom validation rule(s) failed (Error severity):\n  - {}\n  = generation aborted before writing files",
                error_messages.len(),
                error_messages.join("\n  - ")
            )));
        }

        Ok(())
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

        // 2b. Execute custom validation rules (SPARQL ASK/SELECT) against the
        // inferred graph. An Error-severity violation aborts BEFORE any files
        // are written; a Warning is recorded but does not block.
        self.execute_validation_rules()?;

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
                "error[E0004]: Generated content is empty\n  --> rule: '{}', output: '{}'\n  |\n  = help: Check if:\n  =   1. SPARQL query returned results (test in separate SPARQL tool)\n  =   2. Template has content (not empty file)\n  =   3. Template variables match query result columns\n  = help: Use 'ggen validate --dry-run' to see query results",
                rule_id,
                path.display()
            )));
        }

        // Check 2: File size must be under 10MB (10 * 1024 * 1024 bytes)
        const MAX_SIZE_BYTES: usize = 10 * 1024 * 1024;
        let size_bytes = content.len();
        if size_bytes > MAX_SIZE_BYTES {
            return Err(Error::new(&format!(
                "error[E0005]: Generated file too large ({} bytes, limit: 10MB)\n  --> rule: '{}', output: '{}'\n  |\n  = help: Consider splitting into multiple smaller files\n  = help: Or adjust template to reduce output size\n  = help: Check for unexpected data duplication in SPARQL results",
                size_bytes,
                rule_id,
                path.display()
            )));
        }

        // Check 3: Path must not contain traversal patterns
        let path_str = path.to_string_lossy();
        if path_str.contains("../") || path_str.contains("..\\") {
            return Err(Error::new(&format!(
                "error[E0006]: Directory traversal pattern detected in output path\n  --> rule: '{}', path: '{}'\n  |\n  = help: Remove '../' or '..\\' from template output path\n  = help: Use relative paths from base directory without '..'\n  = security: Directory traversal is blocked for security reasons",
                rule_id,
                path.display()
            )));
        }

        Ok(())
    }

    /// Check generated content for unsafe blocks (enforces `no_unsafe = true` in [validation])
    fn check_no_unsafe(content: &str, path: &Path, rule_id: &str) -> Result<()> {
        // Detect `unsafe` keyword used as a block or impl qualifier.
        // Matches: `unsafe {`, `unsafe fn`, `unsafe impl`, `unsafe trait`
        // Does not fire on comments or string literals containing the word.
        let has_unsafe = content.lines().any(|line| {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('*') {
                return false;
            }
            let mut chars = trimmed.char_indices().peekable();
            while let Some((i, c)) = chars.next() {
                if c == '"' || c == '\'' {
                    // skip string/char literal content
                    for (_, ch) in chars.by_ref() {
                        if ch == c {
                            break;
                        }
                    }
                    continue;
                }
                let rest = &trimmed[i..];
                if let Some(after) = rest.strip_prefix("unsafe") {
                    if after.starts_with([' ', '\t', '{', '\n']) || after.is_empty() {
                        return true;
                    }
                }
            }
            false
        });

        if has_unsafe {
            return Err(Error::new(&format!(
                "error[E0012]: Generated code contains `unsafe` block\n  --> rule: '{}', output: '{}'\n  |\n  = `no_unsafe = true` is set in [validation]\n  = help: Remove unsafe code from the template or ontology data\n  = help: Or set `no_unsafe = false` in [validation] if unsafe is intentional",
                rule_id,
                path.display()
            )));
        }
        Ok(())
    }

    /// Generate skill implementation using LLM
    ///
    /// This function uses an injected LLM service to generate skill implementations.
    /// If no service is injected or LLM is disabled in manifest, returns a TemplateFallback stub.
    ///
    /// # Arguments
    /// * `skill_name` - Name of the skill to implement
    /// * `system_prompt` - Description of what the skill does
    /// * `implementation_hint` - Hint about how to implement it
    /// * `language` - Target programming language
    ///
    /// # Returns
    /// * `Ok(String)` - Generated implementation code (or TemplateFallback stub if LLM unavailable)
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
            // Return simple TemplateFallback stub if LLM is disabled
            return Ok(format!(
                "// [ManualImplementation] Implement {} skill: {}\n// Hint: {}",
                skill_name, system_prompt, implementation_hint
            ));
        }

        // Use injected LLM service if available, otherwise use default (TemplateFallback stubs)
        let service = self
            .llm_service
            .as_ref()
            .map(|s| s.as_ref())
            .unwrap_or(&TemplateFallbackService);

        // Call LLM service (may be real LLM or default TemplateFallback stub generator)
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

    #[ignore]
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
    fn test_template_fallback_service_generates_stubs() {
        // Arrange: Use TemplateFallbackService
        let service = TemplateFallbackService;

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

        // Assert: Should all contain manual implementation markers
        assert!(rust_impl.contains("[ManualImplementation]"));
        assert!(rust_impl.to_uppercase().contains("RUST"));
        assert!(rust_impl.contains("my_skill"));

        assert!(elixir_impl.contains("[ManualImplementation]"));
        assert!(elixir_impl.to_uppercase().contains("ELIXIR"));
        assert!(elixir_impl.contains("my_skill"));

        assert!(ts_impl.contains("[ManualImplementation]"));
        assert!(ts_impl.to_uppercase().contains("TYPESCRIPT"));
        assert!(ts_impl.contains("my_skill"));
    }
}
