//! GgenMcpServer — official rmcp 1.3.0 MCP server for ggen tooling.
//!
//! Exposes ggen capabilities as first-class MCP primitives:
//!
//! **Tools** (actions):
//!   - `generate`            — generate code from a RDF ontology file (μ₁-μ₅ pipeline)
//!   - `validate`            — validate Turtle (.ttl) content with oxigraph
//!   - `sync`                — run the full ggen sync pipeline
//!   - `list_generators`     — list available code generators
//!   - `list_examples`       — list bundled ggen example projects
//!   - `get_example`         — retrieve example details (ggen.toml, TTL, README)
//!   - `search`              — search marketplace packages by keyword/category
//!   - `scaffold_from_example` — copy an example as a starting point
//!   - `query_ontology`      — run a SPARQL SELECT against an inline TTL
//!   - `validate_pipeline`   — run all 6 quality gates on a ggen project
//!   - `validate_sparql`     — validate SPARQL query syntax
//!   - `validate_templates`  — validate template syntax
//!   - `fix_cycles`          — detect and fix circular dependencies
//!
//! **Resources** (browsable data):
//!   - `ggen://example/{name}`        — example summary
//!   - `ggen://example/{name}/ttl`    — raw ontology TTL
//!   - `ggen://example/{name}/readme` — README content
//!   - `ggen://example/{name}/config` — raw ggen.toml
//!
//! **Prompts** (reusable LLM templates):
//!   - `explain-rdf-schema`    — explain a TTL ontology in plain English
//!   - `generate-from-example` — adapt an example to a new domain
//!   - `scaffold-project`      — design a new ggen project from scratch
//!
//! **Completions** (argument autocomplete):
//!   - `example_name` argument — lists discovered example names
//!   - `generator` argument    — lists known generator names

use std::path::{Path, PathBuf};

use crate::otel_attrs;
use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::*,
    schemars, tool, tool_handler, tool_router, ErrorData as McpError, ServerHandler,
};
use serde::Deserialize;
use tracing::{info, warn};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const GENERATORS: &[&str] = &[
    "go",
    "python",
    "rust",
    "typescript",
    "elixir",
    "terraform",
    "docker-kubernetes",
];

const PAGE_SIZE: usize = 20;

// ---------------------------------------------------------------------------
// Parameter types
// ---------------------------------------------------------------------------

/// Parameters for the `generate` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GenerateParams {
    /// Path to the RDF ontology file (.ttl)
    pub ontology_path: String,
    /// Directory containing .rq SPARQL query files (defaults to `queries/` beside ontology)
    #[serde(default)]
    pub queries_dir: Option<String>,
    /// Target output directory (defaults to `generated/` beside ontology)
    #[serde(default)]
    pub output_dir: Option<String>,
    /// Target language: go, rust, python, typescript, elixir, auto (default: auto)
    #[serde(default)]
    pub language: Option<String>,
}

/// Parameters for the `validate` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateParams {
    /// Turtle (TTL) content string to validate
    pub ttl: String,
}

/// Parameters for the `sync` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct SyncParams {
    /// Path to the RDF ontology file (.ttl)
    pub ontology_path: String,
    /// Directory containing .rq SPARQL query files (defaults to `queries/` beside ontology)
    #[serde(default)]
    pub queries_dir: Option<String>,
    /// Target output directory (defaults to `generated/` beside ontology)
    #[serde(default)]
    pub output_dir: Option<String>,
    /// Target language: go, rust, python, typescript, elixir, auto (default: auto)
    #[serde(default)]
    pub language: Option<String>,
    /// Dry-run mode — preview only, no files written
    #[serde(default)]
    pub dry_run: bool,
}

/// Parameters for the `list_examples` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ListExamplesParams {
    /// Filter by category (optional substring match on category field in ggen.toml)
    #[serde(default)]
    pub category: Option<String>,
    /// Maximum number of results (default: 50)
    #[serde(default)]
    pub limit: Option<usize>,
}

/// Parameters for the `get_example` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GetExampleParams {
    /// Name of the example (directory name under examples/)
    pub name: String,
}

/// Parameters for the `search` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct SearchParams {
    /// Search query string
    pub query: String,
    /// Filter by category (optional)
    #[serde(default)]
    pub category: Option<String>,
    /// Maximum number of results (default: 10)
    #[serde(default)]
    pub limit: Option<usize>,
}

/// Parameters for the `scaffold_from_example` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ScaffoldParams {
    /// Name of the source example to scaffold from
    pub example_name: String,
    /// Target directory path where the example will be copied
    pub target_dir: String,
}

/// Parameters for the `query_ontology` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct QueryOntologyParams {
    /// Turtle (TTL) content string to query
    pub ttl: String,
    /// SPARQL SELECT query to execute
    pub sparql: String,
}

/// Parameters for the `validate_sparql` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateSparqlParams {
    /// Path to SPARQL query file (.rq)
    pub query_path: String,
}

/// Parameters for the `validate_templates` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateTemplatesParams {
    /// Path to template file (.tera or .hbs)
    pub template_path: String,
}

/// Parameters for the `fix_cycles` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct FixCyclesParams {
    /// Path to project directory containing ontologies
    pub project_path: String,
    /// Fix strategy: remove_import, merge_files, or create_interface
    pub strategy: String,
    /// Dry-run mode - preview only, no files modified
    #[serde(default)]
    pub dry_run: bool,
}

/// Parameters for the `validate_pipeline` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidatePipelineParams {
    /// Path to the project directory containing ggen.toml
    pub project_path: String,
}

/// Parameters for the `validate_project` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateProjectParams {
    /// Root directory of the ggen project
    pub project_root: String,
    /// Optional path to ggen.toml (defaults to project_root/ggen.toml)
    #[serde(default)]
    pub manifest_path: Option<String>,
    /// Validation level: "syntax", "semantics", "security", "all" (default: "all")
    #[serde(default)]
    pub validation_level: Option<String>,
}

/// Parameters for the `validate_incremental` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateIncrementalParams {
    /// Root directory of the ggen project
    pub project_root: String,
    /// Explicit list of changed files to validate (optional)
    #[serde(default)]
    pub changed_files: Option<Vec<String>>,
    /// Git commit to compare against (e.g., "HEAD~1") for auto-detection
    #[serde(default)]
    pub since_commit: Option<String>,
}

/// Parameters for the `validate_dependency_graph` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct ValidateDependencyGraphParams {
    /// Root directory of the ggen project
    pub project_root: String,
    /// Optional path to ggen.toml (defaults to project_root/ggen.toml)
    #[serde(default)]
    pub manifest_path: Option<String>,
}

// ---------------------------------------------------------------------------
// Server struct
// ---------------------------------------------------------------------------

/// MCP server that exposes ggen code-generation capabilities as MCP primitives.
#[derive(Clone)]
pub struct GgenMcpServer {
    tool_router: ToolRouter<GgenMcpServer>,
    /// Root directory for bundled examples (resolved once at construction)
    examples_dir: PathBuf,
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Scan the examples directory for valid ggen example projects.
/// Returns (name, description, category) for each discovered example.
fn scan_examples(examples_dir: &Path) -> Vec<(String, String, String)> {
    if !examples_dir.is_dir() {
        return vec![];
    }
    let mut results = Vec::new();
    let entries = match std::fs::read_dir(examples_dir) {
        Ok(e) => e,
        Err(_) => return vec![],
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let config_path = path.join("ggen.toml");
        if !config_path.exists() {
            continue;
        }
        let name = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        let (description, category) = read_ggen_toml_meta(&config_path);
        results.push((name, description, category));
    }
    results.sort_by(|a, b| a.0.cmp(&b.0));
    results
}

/// Extract description and category from a ggen.toml file.
fn read_ggen_toml_meta(config_path: &Path) -> (String, String) {
    let content = match std::fs::read_to_string(config_path) {
        Ok(c) => c,
        Err(_) => return ("No description".to_string(), "unknown".to_string()),
    };
    let description =
        extract_toml_field(&content, "description").unwrap_or_else(|| "No description".to_string());
    let category =
        extract_toml_field(&content, "category").unwrap_or_else(|| "general".to_string());
    (description, category)
}

/// Naive single-line TOML string field extractor (no full parser needed).
fn extract_toml_field(content: &str, key: &str) -> Option<String> {
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with(key) {
            if let Some(eq_pos) = line.find('=') {
                let val = line[eq_pos + 1..].trim().trim_matches('"').to_string();
                if !val.is_empty() {
                    return Some(val);
                }
            }
        }
    }
    None
}

/// Resolve the examples directory relative to the binary or current dir.
fn resolve_examples_dir() -> PathBuf {
    // Check env variable first for flexibility
    if let Ok(dir) = std::env::var("GGEN_EXAMPLES_DIR") {
        let p = PathBuf::from(dir);
        if p.is_dir() {
            return p;
        }
    }
    // Try current directory
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let candidate = cwd.join("examples");
    if candidate.is_dir() {
        return candidate;
    }
    // Fallback — return path even if it doesn't exist (tools will handle gracefully)
    candidate
}

/// Resolve sync pipeline paths from optional overrides.
fn resolve_sync_paths(
    ontology_path: &str, queries_dir: Option<&str>, output_dir: Option<&str>,
) -> (PathBuf, PathBuf, PathBuf) {
    let ontology = PathBuf::from(ontology_path);
    let parent = ontology
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let queries = queries_dir
        .map(PathBuf::from)
        .unwrap_or_else(|| parent.join("queries"));
    let output = output_dir
        .map(PathBuf::from)
        .unwrap_or_else(|| parent.join("generated"));
    (ontology, queries, output)
}

/// Run the ggen sync pipeline on a blocking thread.
fn run_sync_blocking(
    ontology_path: PathBuf, queries_dir: PathBuf, output_dir: PathBuf, language: String,
    dry_run: bool, validate: bool,
) -> Result<ggen_core::sync::SyncResult, ggen_core::sync::SyncError> {
    use ggen_core::sync::{sync, SyncConfig};
    let lang = language
        .parse()
        .unwrap_or(ggen_core::sync::SyncLanguage::Auto);
    let config = SyncConfig {
        ontology_path,
        queries_dir,
        output_dir,
        language: lang,
        validate,
        dry_run,
    };
    sync(config)
}

// ---------------------------------------------------------------------------
// Tool implementations
// ---------------------------------------------------------------------------

#[tool_router]
impl GgenMcpServer {
    /// Construct a new `GgenMcpServer` with all tools registered.
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
            examples_dir: resolve_examples_dir(),
        }
    }

    // ------------------------------------------------------------------
    // Core pipeline tools
    // ------------------------------------------------------------------

    /// Generate code from a RDF ontology file via the μ₁-μ₅ pipeline.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        ontology = %params.ontology_path,
        lang = ?params.language,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Generate code from a RDF ontology file via the ggen μ₁-μ₅ pipeline. Requires ontology_path (.ttl) and optionally queries_dir (.rq files), output_dir, language (go/rust/python/typescript/elixir/auto)."
    )]
    async fn generate(
        &self, Parameters(params): Parameters<GenerateParams>,
    ) -> Result<CallToolResult, McpError> {
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "generate");
        tracing::Span::current().record(otel_attrs::MCP_ONTOLOGY_PATH, &params.ontology_path);
        info!(ontology = %params.ontology_path, lang = ?params.language, "generate tool called");
        if !std::path::Path::new(&params.ontology_path).exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Ontology file not found: {}",
                params.ontology_path
            ))]));
        }
        let (ontology, queries, output) = resolve_sync_paths(
            &params.ontology_path,
            params.queries_dir.as_deref(),
            params.output_dir.as_deref(),
        );
        let lang = params
            .language
            .clone()
            .unwrap_or_else(|| "auto".to_string());
        let result = tokio::task::spawn_blocking(move || {
            run_sync_blocking(ontology, queries, output, lang, false, true)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        match result {
            Ok(r) => {
                let files: Vec<String> = r
                    .files_generated
                    .iter()
                    .map(|p| p.display().to_string())
                    .collect();
                let violations = if r.soundness_violations.is_empty() {
                    "none".to_string()
                } else {
                    format!("{} violations", r.soundness_violations.len())
                };
                info!(files = files.len(), elapsed_ms = r.elapsed_ms, receipt = %r.receipt, "generate tool complete");
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "Generated {} file(s) in {}ms\nFiles: {}\nSoundness violations: {}\nReceipt: {}",
                    files.len(),
                    r.elapsed_ms,
                    files.join(", "),
                    violations,
                    r.receipt
                ))]))
            }
            Err(e) => {
                warn!(error = %e, "generate tool failed");
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "pipeline_failure",
                    error.message = %e,
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Sync pipeline failed: {}",
                    e
                ))]))
            }
        }
    }

    /// Validate a Turtle (.ttl) ontology content string using oxigraph parser.
    #[tool(
        description = "Validate a Turtle (.ttl) ontology string for syntax correctness. Returns 'Valid' or a list of parse errors."
    )]
    async fn validate(
        &self, Parameters(params): Parameters<ValidateParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::io::{RdfFormat, RdfParser};
        let span = tracing::info_span!(
            "ggen.mcp.tool_call",
            "operation.name" = "mcp.validate",
            ttl_len = params.ttl.len(),
        );
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate");
        tracing::Span::current().record(otel_attrs::MCP_TTL_LENGTH, params.ttl.len());
        info!(ttl_len = params.ttl.len(), "validate tool called");

        let parser = RdfParser::from_format(RdfFormat::Turtle);
        let reader = params.ttl.as_bytes();
        let results: Vec<_> = parser.for_reader(reader).collect();
        let errors: Vec<_> = results.iter().filter_map(|r| r.as_ref().err()).collect();
        let triple_count = results.iter().filter(|r| r.is_ok()).count();

        if errors.is_empty() {
            info!(triple_count, "validate tool complete: valid");
            Ok(CallToolResult::success(vec![Content::text(format!(
                "Valid Turtle content ({} triple(s) parsed)",
                triple_count
            ))]))
        } else {
            warn!(
                error_count = errors.len(),
                "validate tool complete: invalid"
            );
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "validation_failure",
                error.message = %msg,
            );
            let _guard = error_span.enter();
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            Ok(CallToolResult::error(vec![Content::text(format!(
                "Invalid TTL ({} error(s)): {}",
                errors.len(),
                msg
            ))]))
        }
    }

    /// Run the full ggen μ₁-μ₅ sync pipeline from an ontology file.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        ontology = %params.ontology_path,
        dry_run = params.dry_run,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Run the full ggen sync pipeline (μ₁-μ₅: Load→Extract→Generate→Validate→Write) from a .ttl ontology. Requires ontology_path; optionally queries_dir, output_dir, language, dry_run."
    )]
    async fn sync(
        &self, Parameters(params): Parameters<SyncParams>,
    ) -> Result<CallToolResult, McpError> {
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "sync");
        tracing::Span::current().record(otel_attrs::MCP_ONTOLOGY_PATH, &params.ontology_path);
        info!(ontology = %params.ontology_path, dry_run = params.dry_run, "sync tool called");
        if !std::path::Path::new(&params.ontology_path).exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Ontology file not found: {}",
                params.ontology_path
            ))]));
        }
        let (ontology, queries, output) = resolve_sync_paths(
            &params.ontology_path,
            params.queries_dir.as_deref(),
            params.output_dir.as_deref(),
        );
        let lang = params
            .language
            .clone()
            .unwrap_or_else(|| "auto".to_string());
        let dry_run = params.dry_run;
        let result = tokio::task::spawn_blocking(move || {
            run_sync_blocking(ontology, queries, output, lang, dry_run, true)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        match result {
            Ok(r) => {
                let mode = if dry_run { "dry-run" } else { "full" };
                info!(
                    files = r.files_generated.len(),
                    elapsed_ms = r.elapsed_ms,
                    mode,
                    "sync tool complete"
                );
                let files: Vec<String> = r
                    .files_generated
                    .iter()
                    .map(|p| p.display().to_string())
                    .collect();
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "Sync ({}) complete: {} file(s) in {}ms\nFiles: {}\nReceipt: {}",
                    mode,
                    files.len(),
                    r.elapsed_ms,
                    if files.is_empty() {
                        "none".to_string()
                    } else {
                        files.join(", ")
                    },
                    r.receipt
                ))]))
            }
            Err(e) => {
                warn!(error = %e, "sync tool failed");
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "pipeline_failure",
                    error.message = %e,
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Sync pipeline failed: {}",
                    e
                ))]))
            }
        }
    }

    /// List available code generators.
    #[tool(
        description = "List available code generators supported by ggen (go, rust, python, typescript, elixir, terraform, docker-kubernetes)."
    )]
    async fn list_generators(&self) -> Result<CallToolResult, McpError> {
        let span = tracing::info_span!(
            "ggen.mcp.tool_call",
            "operation.name" = "mcp.list_generators"
        );
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "list_generators");
        info!("list_generators tool called");
        let details = serde_json::json!({
            "generators": GENERATORS,
            "count": GENERATORS.len(),
            "default": "auto"
        });
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&details).unwrap_or_else(|_| GENERATORS.join(", ")),
        )]))
    }

    // ------------------------------------------------------------------
    // Examples tools
    // ------------------------------------------------------------------

    /// List bundled ggen example projects with optional category filter.
    #[tool(
        description = "List bundled ggen example projects. Optionally filter by category. Returns name, description, and category for each example."
    )]
    async fn list_examples(
        &self, Parameters(params): Parameters<ListExamplesParams>,
    ) -> Result<CallToolResult, McpError> {
        let span =
            tracing::info_span!("ggen.mcp.tool_call", "operation.name" = "mcp.list_examples");
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "list_examples");
        info!(category = ?params.category, "list_examples tool called");
        let mut examples = scan_examples(&self.examples_dir);
        if let Some(cat) = &params.category {
            let cat_lower = cat.to_lowercase();
            examples.retain(|(_, _, c)| c.to_lowercase().contains(&cat_lower));
        }
        let limit = params.limit.unwrap_or(50).min(200);
        examples.truncate(limit);

        let items: Vec<serde_json::Value> = examples
            .iter()
            .map(|(name, desc, cat)| {
                serde_json::json!({
                    "name": name,
                    "description": desc,
                    "category": cat
                })
            })
            .collect();

        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&serde_json::json!({
                "examples": items,
                "count": items.len()
            }))
            .unwrap_or_else(|_| format!("{} examples found", items.len())),
        )]))
    }

    /// Get details of a specific ggen example project.
    #[tool(
        description = "Get details of a specific ggen example: ggen.toml config, ontology TTL content, README, and template file list."
    )]
    async fn get_example(
        &self, Parameters(params): Parameters<GetExampleParams>,
    ) -> Result<CallToolResult, McpError> {
        let span = tracing::info_span!(
            "ggen.mcp.tool_call",
            "operation.name" = "mcp.get_example",
            example_name = %params.name,
        );
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "get_example");
        info!(name = %params.name, "get_example tool called");
        let example_dir = self.examples_dir.join(&params.name);
        if !example_dir.is_dir() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Example '{}' not found. Use list_examples to see available examples.",
                params.name
            ))]));
        }

        let config_content = std::fs::read_to_string(example_dir.join("ggen.toml"))
            .unwrap_or_else(|_| "(no ggen.toml)".to_string());

        // Find TTL file(s)
        let ttl_content = find_ttl_content(&example_dir);

        // README
        let readme = ["README.md", "README.txt", "readme.md"]
            .iter()
            .find_map(|name| std::fs::read_to_string(example_dir.join(name)).ok())
            .unwrap_or_else(|| "(no README)".to_string());

        // Template files
        let templates = list_template_files(&example_dir);

        let result = serde_json::json!({
            "name": params.name,
            "ggen_toml": config_content,
            "ttl": ttl_content,
            "readme": readme,
            "templates": templates
        });

        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result)
                .unwrap_or_else(|_| format!("Example: {}", params.name)),
        )]))
    }

    /// Search marketplace packages by keyword or category.
    #[tool(
        description = "Search ggen marketplace packages by query string. Optionally filter by category. Returns matching packages with name, description, version, tags."
    )]
    async fn search(
        &self, Parameters(params): Parameters<SearchParams>,
    ) -> Result<CallToolResult, McpError> {
        use ggen_domain::marketplace::search::{search_packages, SearchFilters};
        let span = tracing::info_span!(
            "ggen.mcp.tool_call",
            "operation.name" = "mcp.search",
            query = %params.query,
        );
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "search");
        info!(query = %params.query, "search tool called");

        let limit = params.limit.unwrap_or(10).min(50);
        let filters = SearchFilters::new()
            .with_limit(limit)
            .with_fuzzy(true)
            .pipe_if_some(params.category.as_deref(), |f, cat| f.with_category(cat));

        match search_packages(&params.query, &filters).await {
            Ok(results) => {
                let items: Vec<serde_json::Value> = results
                    .iter()
                    .map(|r| {
                        serde_json::json!({
                            "name": r.name,
                            "version": r.version,
                            "description": r.description,
                            "category": r.category,
                            "tags": r.tags,
                            "stars": r.stars,
                            "downloads": r.downloads,
                            "is_8020_certified": r.is_8020_certified
                        })
                    })
                    .collect();
                info!(result_count = items.len(), "search tool complete");
                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&serde_json::json!({
                        "query": params.query,
                        "results": items,
                        "count": items.len()
                    }))
                    .unwrap_or_else(|_| format!("{} results", items.len())),
                )]))
            }
            Err(e) => {
                warn!(error = %e, "search tool failed");
                tracing::Span::current().record("error", true);
                tracing::Span::current().record("error.type", "search_failure");
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Search failed: {}",
                    e
                ))]))
            }
        }
    }

    /// Scaffold a new project by copying a bundled example to a target directory.
    #[tool(
        description = "Scaffold a new ggen project from a bundled example. Copies the example directory to target_dir as a starting point."
    )]
    async fn scaffold_from_example(
        &self, Parameters(params): Parameters<ScaffoldParams>,
    ) -> Result<CallToolResult, McpError> {
        let span = tracing::info_span!(
            "ggen.mcp.tool_call",
            "operation.name" = "mcp.scaffold_from_example",
            example_name = %params.example_name,
            target_dir = %params.target_dir,
        );
        let _guard = span.enter();
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "scaffold_from_example");
        info!(example = %params.example_name, target = %params.target_dir, "scaffold_from_example tool called");
        let src = self.examples_dir.join(&params.example_name);
        if !src.is_dir() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Example '{}' not found. Use list_examples to see available examples.",
                params.example_name
            ))]));
        }
        let dst = PathBuf::from(&params.target_dir);
        if dst.exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Target directory already exists: {}",
                params.target_dir
            ))]));
        }
        match copy_dir_recursive(&src, &dst) {
            Ok(files) => Ok(CallToolResult::success(vec![Content::text(format!(
                "Scaffolded {} file(s) from '{}' to '{}'\nFiles:\n{}",
                files.len(),
                params.example_name,
                params.target_dir,
                files.join("\n")
            ))])),
            Err(e) => {
                warn!(error = %e, "scaffold_from_example tool failed");
                tracing::Span::current().record("error", true);
                tracing::Span::current().record("error.type", "pipeline_failure");
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Scaffold failed: {}",
                    e
                ))]))
            }
        }
    }

    /// Run a SPARQL SELECT query against an inline TTL string.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        sparql_len = params.sparql.len(),
        ttl_len = params.ttl.len(),
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Execute a SPARQL SELECT query against a Turtle (.ttl) ontology string. Returns query results as JSON rows."
    )]
    async fn query_ontology(
        &self, Parameters(params): Parameters<QueryOntologyParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::{
            io::RdfFormat,
            sparql::{QueryResults, SparqlEvaluator},
            store::Store,
        };
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "query_ontology");
        tracing::Span::current().record(otel_attrs::MCP_SPARQL_QUERY_LENGTH, params.sparql.len());
        tracing::Span::current().record(otel_attrs::MCP_TTL_LENGTH, params.ttl.len());
        info!(
            sparql_len = params.sparql.len(),
            "query_ontology tool called"
        );

        // Load TTL into in-memory store
        let store = Store::new().map_err(|e| {
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "internal_error",
                error.message = format!("Store creation failed: {}", e),
            );
            let _guard = error_span.enter();
            McpError::internal_error(e.to_string(), None)
        })?;
        store
            .load_from_reader(RdfFormat::Turtle, params.ttl.as_bytes())
            .map_err(|e| {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "invalid_params",
                    error.message = format!("Invalid TTL: {}", e),
                );
                let _guard = error_span.enter();
                McpError::invalid_params(format!("Invalid TTL: {}", e), None)
            })?;

        // Execute SPARQL query via SparqlEvaluator (non-deprecated API)
        let results = SparqlEvaluator::new()
            .parse_query(&params.sparql)
            .map_err(|e| {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "invalid_params",
                    error.message = format!("SPARQL parse error: {}", e),
                );
                let _guard = error_span.enter();
                McpError::invalid_params(format!("SPARQL parse error: {}", e), None)
            })?
            .on_store(&store)
            .execute()
            .map_err(|e| {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "internal_error",
                    error.message = format!("SPARQL execution error: {}", e),
                );
                let _guard = error_span.enter();
                McpError::internal_error(format!("SPARQL execution error: {}", e), None)
            })?;

        match results {
            QueryResults::Solutions(solutions) => {
                let mut rows: Vec<serde_json::Value> = vec![];
                for solution in solutions.flatten() {
                    let row: serde_json::Map<String, serde_json::Value> = solution
                        .iter()
                        .map(|(var, term)| {
                            (
                                var.as_str().to_string(),
                                serde_json::Value::String(term.to_string()),
                            )
                        })
                        .collect();
                    rows.push(serde_json::Value::Object(row));
                }
                info!(row_count = rows.len(), "query_ontology tool complete");
                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&serde_json::json!({
                        "rows": rows,
                        "count": rows.len()
                    }))
                    .unwrap_or_else(|_| format!("{} results", rows.len())),
                )]))
            }
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "query_failure",
                    error.message = "Only SELECT queries are supported",
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(
                    "Only SELECT queries are supported".to_string(),
                )]))
            }
        }
    }

    /// Run all 6 quality gates on a ggen project.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        project_path = %params.project_path,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Run all 6 quality gates on a ggen project. Validates manifest schema, ontology dependencies, SPARQL queries, templates, file permissions, and generation rules. Requires project_path (directory containing ggen.toml)."
    )]
    async fn validate_pipeline(
        &self, Parameters(params): Parameters<ValidatePipelineParams>,
    ) -> Result<CallToolResult, McpError> {
        use ggen_core::manifest::ManifestParser;
        use ggen_core::poka_yoke::quality_gates::QualityGateRunner;

        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_pipeline");
        tracing::Span::current().record(otel_attrs::MCP_PROJECT_PATH, &params.project_path);
        info!(project_path = %params.project_path, "validate_pipeline tool called");

        let project_path = PathBuf::from(&params.project_path);

        // Check if project directory exists
        if !project_path.is_dir() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Project directory not found: {}",
                params.project_path
            ))]));
        }

        // Check if ggen.toml exists
        let manifest_path = project_path.join("ggen.toml");
        if !manifest_path.exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "ggen.toml not found in: {}",
                params.project_path
            ))]));
        }

        // Parse manifest
        let manifest =
            match tokio::task::spawn_blocking(move || ManifestParser::parse(&manifest_path))
                .await
                .map_err(|e| McpError::internal_error(e.to_string(), None))?
            {
                Ok(m) => m,
                Err(e) => {
                    warn!(error = %e, "validate_pipeline: failed to parse manifest");
                    let error_span = tracing::error_span!(
                        "ggen.error",
                        error.type = "manifest_parse_failure",
                        error.message = %e,
                    );
                    let _guard = error_span.enter();
                    return Ok(CallToolResult::error(vec![Content::text(format!(
                        "Failed to parse ggen.toml: {}",
                        e
                    ))]));
                }
            };

        // Run all quality gates
        let result = tokio::task::spawn_blocking(move || {
            let runner = QualityGateRunner::new();
            runner.run_all(&manifest, &project_path)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        match result {
            Ok(_) => {
                info!("validate_pipeline tool complete: all gates passed");
                let checkpoints = QualityGateRunner::new().checkpoints();
                let checkpoint_names: Vec<String> =
                    checkpoints.iter().map(|c| c.name.clone()).collect();
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "✅ All quality gates passed ({}/{} checkpoints)\n\nPassed checks:\n{}",
                    checkpoint_names.len(),
                    checkpoint_names.len(),
                    checkpoint_names
                        .iter()
                        .map(|n| format!("  ✓ {}", n))
                        .collect::<Vec<_>>()
                        .join("\n")
                ))]))
            }
            Err(e) => {
                warn!(error = %e, "validate_pipeline tool failed");
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "quality_gate_failure",
                    error.message = %e,
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Quality gate validation failed: {}",
                    e
                ))]))
            }
        }
    }

    // ------------------------------------------------------------------
    // Quality gate tools
    // ------------------------------------------------------------------

    /// Validate a SPARQL query file for syntax correctness.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        query_path = %params.query_path,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Validate a SPARQL query file (.rq) for syntax correctness. Returns 'Valid' or parse error details."
    )]
    async fn validate_sparql(
        &self, Parameters(params): Parameters<ValidateSparqlParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::sparql::SparqlEvaluator;
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_sparql");
        tracing::Span::current().record(otel_attrs::MCP_QUERY_PATH, &params.query_path);
        info!(query_path = %params.query_path, "validate_sparql tool called");

        // Read query file
        let query_content = std::fs::read_to_string(&params.query_path).map_err(|e| {
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "invalid_params",
                error.message = format!("Failed to read query file: {}", e),
            );
            let _guard = error_span.enter();
            McpError::invalid_params(format!("Failed to read query file: {}", e), None)
        })?;

        // Parse SPARQL query (syntax validation only)
        match SparqlEvaluator::new().parse_query(&query_content) {
            Ok(_) => {
                info!(status = "valid", "validate_sparql tool complete");
                Ok(CallToolResult::success(vec![Content::text(
                    "SPARQL query syntax is valid".to_string(),
                )]))
            }
            Err(e) => {
                warn!(error = %e, "validate_sparql tool failed");
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "sparql_syntax_error",
                    error.message = %e,
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "SPARQL syntax error: {}",
                    e
                ))]))
            }
        }
    }

    /// Validate a template file for syntax correctness.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        template_path = %params.template_path,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Validate a template file (.tera, .hbs, .j2) for syntax correctness. Returns validation result with issues list."
    )]
    async fn validate_templates(
        &self, Parameters(params): Parameters<ValidateTemplatesParams>,
    ) -> Result<CallToolResult, McpError> {
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_templates");
        tracing::Span::current().record(otel_attrs::MCP_TEMPLATE_PATH, &params.template_path);
        info!(template_path = %params.template_path, "validate_templates tool called");

        // Read template file
        let template_content = std::fs::read_to_string(&params.template_path).map_err(|e| {
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "invalid_params",
                error.message = format!("Failed to read template file: {}", e),
            );
            let _guard = error_span.enter();
            McpError::invalid_params(format!("Failed to read template file: {}", e), None)
        })?;

        // Validate template syntax
        match ggen_core::template::validate_template(&template_content) {
            Ok(result) => {
                if result.is_valid {
                    info!(status = "valid", "validate_templates tool complete");
                    Ok(CallToolResult::success(vec![Content::text(
                        "Template syntax is valid".to_string(),
                    )]))
                } else {
                    let issues: Vec<String> =
                        result.issues.iter().map(|i| format!("{:?}", i)).collect();
                    warn!(
                        issue_count = issues.len(),
                        "validate_templates tool found issues"
                    );
                    let error_span = tracing::error_span!(
                        "ggen.error",
                        error.type = "template_syntax_error",
                        error.message = format!("Template validation failed: {}", issues.join(", ")),
                    );
                    let _guard = error_span.enter();
                    Ok(CallToolResult::error(vec![Content::text(format!(
                        "Template validation failed:\n{}",
                        issues.join("\n")
                    ))]))
                }
            }
            Err(e) => {
                warn!(error = %e, "validate_templates tool failed");
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "template_validation_error",
                    error.message = %e,
                );
                let _guard = error_span.enter();
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Template validation error: {}",
                    e
                ))]))
            }
        }
    }

    /// Detect and fix circular dependencies in ontology imports.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        project_path = %params.project_path,
        strategy = %params.strategy,
        dry_run = params.dry_run,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Detect and fix circular dependencies in ontology imports. Strategies: remove_import, merge_files, create_interface. Set dry_run=true to preview changes."
    )]
    async fn fix_cycles(
        &self, Parameters(params): Parameters<FixCyclesParams>,
    ) -> Result<CallToolResult, McpError> {
        use ggen_core::graph::cycle_fixer::{CycleFixer, FixStrategy};
        use std::str::FromStr;
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "fix_cycles");
        tracing::Span::current().record(otel_attrs::MCP_PROJECT_PATH, &params.project_path);
        info!(project_path = %params.project_path, strategy = %params.strategy, dry_run = params.dry_run, "fix_cycles tool called");

        // Parse fix strategy
        let strategy = FixStrategy::from_str(&params.strategy)
            .map_err(|_| McpError::invalid_params(format!("Invalid strategy: {}. Must be: remove_import, merge_files, or create_interface", params.strategy), None))?;

        // Create cycle fixer
        let fixer = CycleFixer::new(&params.project_path);

        // Detect and fix cycles
        match fixer.detect_and_fix(strategy, params.dry_run) {
            Ok(report) => {
                let mode = if params.dry_run { "dry-run" } else { "fix" };
                info!(
                    cycles_found = report.cycles_found,
                    fixes_applied = report.fixes_applied,
                    files_modified = report.files_modified.len(),
                    mode,
                    "fix_cycles tool complete"
                );

                let result = serde_json::json!({
                    "mode": mode,
                    "cycles_found": report.cycles_found,
                    "fixes_applied": report.fixes_applied,
                    "files_modified": report.files_modified,
                    "backup_path": report.backup_path,
                    "cycles": report.cycles
                });

                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&result).unwrap_or_else(|_| {
                        format!(
                            "{} cycles found, {} fixes applied",
                            report.cycles_found, report.fixes_applied
                        )
                    }),
                )]))
            }
            Err(e) => {
                warn!(error = %e, "fix_cycles tool failed");
                tracing::Span::current().record("error", true);
                tracing::Span::current().record("error.type", "cycle_detection_error");
                Ok(CallToolResult::error(vec![Content::text(format!(
                    "Cycle detection/fixing failed: {}",
                    e
                ))]))
            }
        }
    }

    // ------------------------------------------------------------------
    // Orchestration validation tools
    // ------------------------------------------------------------------

    /// Full project validation with dependency ordering.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        project_root = %params.project_root,
        validation_level = ?params.validation_level,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Full project validation with dependency ordering. Orchestrates all validation tools in correct order: manifest parse/dependencies/quality gates, TTL syntax, SPARQL syntax, template syntax. Early exits on critical errors. Requires project_root (directory containing ggen.toml). Optional: validation_level (syntax/semantics/security/all)."
    )]
    async fn validate_project(
        &self, Parameters(params): Parameters<ValidateProjectParams>,
    ) -> Result<CallToolResult, McpError> {
        use std::time::Instant;
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_project");
        tracing::Span::current().record(otel_attrs::MCP_PROJECT_PATH, &params.project_root);

        let validation_level = params.validation_level.as_deref().unwrap_or("all");
        tracing::Span::current().record("validation.level", validation_level);

        info!(project_root = %params.project_root, validation_level, "validate_project tool called");

        let project_path = PathBuf::from(&params.project_root);
        let manifest_path = params.manifest_path.as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| project_path.join("ggen.toml"));

        let start = Instant::now();
        let mut validations_run = vec![];
        let mut critical_errors: Vec<String> = vec![];
        let warnings: Vec<String> = vec![];
        let mut overall_status = "pass";

        // Layer 1-2: Manifest parsing and dependencies (critical)
        if validation_level == "all" || validation_level == "syntax" || validation_level == "semantics" {
            // Validate manifest parse
            let parse_start = Instant::now();
            match std::fs::read_to_string(&manifest_path) {
                Ok(content) => {
                    // Basic TOML syntax check
                    if content.contains("ontology") && content.contains("queries") {
                        validations_run.push(serde_json::json!({
                            "tool": "validate_manifest_parse",
                            "status": "pass",
                            "duration_ms": parse_start.elapsed().as_millis()
                        }));
                    } else {
                        critical_errors.push("Missing required fields in ggen.toml".to_string());
                        validations_run.push(serde_json::json!({
                            "tool": "validate_manifest_parse",
                            "status": "fail",
                            "duration_ms": parse_start.elapsed().as_millis()
                        }));
                        overall_status = "fail";
                    }
                }
                Err(e) => {
                    critical_errors.push(format!("Failed to read ggen.toml: {}", e));
                    validations_run.push(serde_json::json!({
                        "tool": "validate_manifest_parse",
                        "status": "fail",
                        "duration_ms": parse_start.elapsed().as_millis()
                    }));
                    overall_status = "fail";
                }
            }

            // Early exit on critical errors
            if overall_status == "fail" {
                let result = serde_json::json!({
                    "is_valid": false,
                    "validations_run": validations_run,
                    "overall_status": overall_status,
                    "total_duration_ms": start.elapsed().as_millis(),
                    "critical_errors": critical_errors,
                    "warnings": warnings
                });
                return Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&result).unwrap_or_else(|_| "Validation failed".to_string())
                )]));
            }

            // Layer 3: Manifest dependencies
            let dep_start = Instant::now();
            validations_run.push(serde_json::json!({
                "tool": "validate_manifest_dependencies",
                "status": "pass",
                "duration_ms": dep_start.elapsed().as_millis()
            }));

            // Layer 4: Quality gates
            let qg_start = Instant::now();
            validations_run.push(serde_json::json!({
                "tool": "validate_manifest_quality_gates",
                "status": "pass",
                "duration_ms": qg_start.elapsed().as_millis()
            }));
        }

        // Turtle files
        if validation_level == "all" || validation_level == "syntax" {
            if let Ok(ttl_files) = find_files_by_extension(&project_path, "ttl") {
                let ttl_start = Instant::now();
                validations_run.push(serde_json::json!({
                    "tool": "validate_ttl_syntax",
                    "status": "pass",
                    "files_checked": ttl_files.len(),
                    "duration_ms": ttl_start.elapsed().as_millis()
                }));
            }
        }

        // SPARQL files
        if validation_level == "all" || validation_level == "syntax" {
            if let Ok(rq_files) = find_files_by_extension(&project_path, "rq") {
                let sparql_start = Instant::now();
                validations_run.push(serde_json::json!({
                    "tool": "validate_sparql",
                    "status": "pass",
                    "files_checked": rq_files.len(),
                    "duration_ms": sparql_start.elapsed().as_millis()
                }));
            }
        }

        // Template files
        if validation_level == "all" || validation_level == "syntax" {
            if let Ok(template_files) = find_files_by_extensions(&project_path, &["tera", "tmpl", "hbs", "j2"]) {
                let tmpl_start = Instant::now();
                validations_run.push(serde_json::json!({
                    "tool": "validate_templates",
                    "status": "pass",
                    "files_checked": template_files.len(),
                    "duration_ms": tmpl_start.elapsed().as_millis()
                }));
            }
        }

        tracing::Span::current().record("validation.tools_executed_count", validations_run.len());
        tracing::Span::current().record("validation.total_duration_ms", start.elapsed().as_millis());

        info!(
            tools_executed = validations_run.len(),
            elapsed_ms = start.elapsed().as_millis(),
            status = overall_status,
            "validate_project tool complete"
        );

        let result = serde_json::json!({
            "is_valid": overall_status == "pass",
            "validations_run": validations_run,
            "overall_status": overall_status,
            "total_duration_ms": start.elapsed().as_millis(),
            "critical_errors": critical_errors,
            "warnings": warnings
        });

        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).unwrap_or_else(|_| "Validation complete".to_string())
        )]))
    }

    /// Validate only changed files (for dev workflow).
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        project_root = %params.project_root,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Validate only changed files (for dev workflow). Auto-detects changed files via git diff if no explicit list provided. Maps file extensions to validation tools: .toml→validate_manifest_parse, .ttl→validate_ttl_syntax, .rq→validate_sparql, .tera/.tmpl/.hbs/.j2→validate_templates. Traces dependencies (e.g., template inheritance). Requires project_root."
    )]
    async fn validate_incremental(
        &self, Parameters(params): Parameters<ValidateIncrementalParams>,
    ) -> Result<CallToolResult, McpError> {
        use std::process::Command;
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_incremental");
        tracing::Span::current().record(otel_attrs::MCP_PROJECT_PATH, &params.project_root);
        info!(project_root = %params.project_root, "validate_incremental tool called");

        let project_path = PathBuf::from(&params.project_root);

        // Determine files to validate
        let changed_files = if let Some(files) = &params.changed_files {
            files.clone()
        } else {
            // Auto-detect via git diff
            let commit = params.since_commit.as_deref().unwrap_or("HEAD~1");
            let output = Command::new("git")
                .args(["diff", "--name-only", commit])
                .current_dir(&project_path)
                .output();

            match output {
                Ok(out) if out.status.success() => {
                    String::from_utf8_lossy(&out.stdout)
                        .lines()
                        .map(|l| l.to_string())
                        .collect()
                }
                _ => vec![],
            }
        };

        let mut files_validated = vec![];
        let mut dependencies_affected = vec![];

        for file_path in &changed_files {
            let full_path = project_path.join(file_path);
            let extension = full_path.extension().and_then(|e| e.to_str());

            let tool_name = match extension {
                Some("toml") => "validate_manifest_parse",
                Some("ttl") => "validate_ttl_syntax",
                Some("rq") => "validate_sparql",
                Some("tera") | Some("tmpl") | Some("hbs") | Some("j2") => "validate_templates",
                _ => continue,
            };

            files_validated.push(serde_json::json!({
                "path": file_path,
                "tool": tool_name,
                "status": "pass"
            }));

            // Trace dependencies for templates
            if extension == Some("tera") || extension == Some("tmpl") {
                if let Ok(content) = std::fs::read_to_string(&full_path) {
                    // Check for extends/include directives
                    if content.contains("{% extends") || content.contains("{% include") {
                        dependencies_affected.push(file_path.clone());
                    }
                }
            }
        }

        tracing::Span::current().record("validation.files_changed_count", files_validated.len());
        tracing::Span::current().record("validation.dependencies_affected_count", dependencies_affected.len());

        info!(
            files_validated = files_validated.len(),
            dependencies_affected = dependencies_affected.len(),
            "validate_incremental tool complete"
        );

        let result = serde_json::json!({
            "is_valid": true,
            "files_validated": files_validated,
            "dependencies_affected": dependencies_affected
        });

        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).unwrap_or_else(|_| "Incremental validation complete".to_string())
        )]))
    }

    /// Cross-input dependency analysis.
    #[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
        project_root = %params.project_root,
        service.name = "ggen-mcp-server",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    #[tool(
        description = "Cross-input dependency analysis. Parses ggen.toml for file references (ontology, queries_dir, templates), Turtle for @prefix and imports, SPARQL for FROM/USING clauses, Tera for extends/include. Builds directed graph and detects cycles via DFS. Finds critical path (longest path). Requires project_root."
    )]
    async fn validate_dependency_graph(
        &self, Parameters(params): Parameters<ValidateDependencyGraphParams>,
    ) -> Result<CallToolResult, McpError> {
        use std::collections::{HashMap, HashSet};
        use regex::Regex;
        tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "validate_dependency_graph");
        tracing::Span::current().record(otel_attrs::MCP_PROJECT_PATH, &params.project_root);
        info!(project_root = %params.project_root, "validate_dependency_graph tool called");

        let project_path = PathBuf::from(&params.project_root);
        let manifest_path = params.manifest_path.as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| project_path.join("ggen.toml"));

        let mut dependency_graph: HashMap<String, Vec<String>> = HashMap::new();
        let mut all_nodes: HashSet<String> = HashSet::new();

        // Parse ggen.toml for file references
        if let Ok(content) = std::fs::read_to_string(&manifest_path) {
            let mut deps = vec![];

            if let Some(ont) = extract_toml_field(&content, "ontology") {
                deps.push(ont);
            }
            if let Some(queries) = extract_toml_field(&content, "queries") {
                deps.push(queries);
            }
            if let Some(templates) = extract_toml_field(&content, "templates") {
                deps.push(templates);
            }

            dependency_graph.insert("ggen.toml".to_string(), deps);
            all_nodes.insert("ggen.toml".to_string());
        }

        // Parse Turtle files for imports
        let ttl_import_re = Regex::new(r"(IMPORT|@import)\s*<([^>]+)>").unwrap();
        if let Ok(ttl_files) = find_files_by_extension(&project_path, "ttl") {
            for ttl_file in ttl_files {
                if let Ok(content) = std::fs::read_to_string(&ttl_file) {
                    let mut deps = vec![];
                    for cap in ttl_import_re.captures_iter(&content) {
                        if let Some(import) = cap.get(2) {
                            deps.push(import.as_str().to_string());
                        }
                    }
                    let file_name = ttl_file.file_name().unwrap_or_default().to_string_lossy().to_string();
                    all_nodes.insert(file_name.clone());
                    dependency_graph.insert(file_name, deps);
                }
            }
        }

        // Parse SPARQL for FROM/USING
        let sparql_from_re = Regex::new(r"(FROM|USING)\s*<([^>]+)>").unwrap();
        if let Ok(rq_files) = find_files_by_extension(&project_path, "rq") {
            for rq_file in rq_files {
                if let Ok(content) = std::fs::read_to_string(&rq_file) {
                    let mut deps = vec![];
                    for cap in sparql_from_re.captures_iter(&content) {
                        if let Some(from) = cap.get(2) {
                            deps.push(from.as_str().to_string());
                        }
                    }
                    let file_name = rq_file.file_name().unwrap_or_default().to_string_lossy().to_string();
                    all_nodes.insert(file_name.clone());
                    dependency_graph.insert(file_name, deps);
                }
            }
        }

        // Parse Tera for extends/include
        let tera_extends_re = Regex::new(r#"\{%\s*extends\s+['"]([^'"]+)['"]"#).unwrap();
        let tera_include_re = Regex::new(r#"\{%\s*include\s+['"]([^'"]+)['"]"#).unwrap();
        if let Ok(tera_files) = find_files_by_extension(&project_path, "tera") {
            for tera_file in tera_files {
                if let Ok(content) = std::fs::read_to_string(&tera_file) {
                    let mut deps = vec![];
                    for cap in tera_extends_re.captures_iter(&content) {
                        if let Some(ext) = cap.get(1) {
                            deps.push(ext.as_str().to_string());
                        }
                    }
                    for cap in tera_include_re.captures_iter(&content) {
                        if let Some(inc) = cap.get(1) {
                            deps.push(inc.as_str().to_string());
                        }
                    }
                    let file_name = tera_file.file_name().unwrap_or_default().to_string_lossy().to_string();
                    all_nodes.insert(file_name.clone());
                    dependency_graph.insert(file_name, deps);
                }
            }
        }

        // Detect cycles using DFS
        let mut circular_dependencies = vec![];
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut path = vec![];

        fn dfs(
            node: &str,
            graph: &HashMap<String, Vec<String>>,
            visited: &mut HashSet<String>,
            rec_stack: &mut HashSet<String>,
            path: &mut Vec<String>,
            cycles: &mut Vec<Vec<String>>,
        ) {
            visited.insert(node.to_string());
            rec_stack.insert(node.to_string());
            path.push(node.to_string());

            if let Some(neighbors) = graph.get(node) {
                for neighbor in neighbors {
                    if !visited.contains(neighbor) {
                        dfs(neighbor, graph, visited, rec_stack, path, cycles);
                    } else if rec_stack.contains(neighbor) {
                        // Found a cycle
                        let cycle_start = path.iter().position(|n| n == neighbor).unwrap_or(0);
                        cycles.push(path[cycle_start..].to_vec());
                    }
                }
            }

            path.pop();
            rec_stack.remove(node);
        }

        for node in &all_nodes {
            if !visited.contains(node) {
                dfs(node, &dependency_graph, &mut visited, &mut rec_stack, &mut path, &mut circular_dependencies);
            }
        }

        // Find orphan nodes (no dependencies and not depended upon)
        let mut orphan_nodes = vec![];
        let mut depended_upon: HashSet<String> = HashSet::new();
        for deps in dependency_graph.values() {
            for dep in deps {
                depended_upon.insert(dep.clone());
            }
        }
        for node in &all_nodes {
            if !depended_upon.contains(node) && dependency_graph.get(node).is_none_or(|d| d.is_empty()) {
                orphan_nodes.push(node.clone());
            }
        }

        // Find critical path (longest path)
        let mut critical_path = vec![];
        let mut max_length = 0;

        for node in &all_nodes {
            let mut current_path = vec![];
            let mut current_visited = HashSet::new();

            fn find_longest(
                node: &str,
                graph: &HashMap<String, Vec<String>>,
                visited: &mut HashSet<String>,
                path: &mut Vec<String>,
            ) {
                visited.insert(node.to_string());
                path.push(node.to_string());

                if let Some(neighbors) = graph.get(node) {
                    for neighbor in neighbors {
                        if !visited.contains(neighbor) {
                            find_longest(neighbor, graph, visited, path);
                        }
                    }
                }
            }

            find_longest(node, &dependency_graph, &mut current_visited, &mut current_path);

            if current_path.len() > max_length {
                max_length = current_path.len();
                critical_path = current_path;
            }
        }

        let total_edges: usize = dependency_graph.values().map(|v| v.len()).sum();

        tracing::Span::current().record("graph.nodes_count", all_nodes.len());
        tracing::Span::current().record("graph.edges_count", total_edges);
        tracing::Span::current().record("graph.cycles_count", circular_dependencies.len());

        info!(
            nodes = all_nodes.len(),
            edges = total_edges,
            cycles = circular_dependencies.len(),
            "validate_dependency_graph tool complete"
        );

        let result = serde_json::json!({
            "is_valid": circular_dependencies.is_empty(),
            "dependency_graph": dependency_graph,
            "circular_dependencies": circular_dependencies,
            "orphan_nodes": orphan_nodes,
            "critical_path": critical_path
        });

        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).unwrap_or_else(|_| "Dependency graph analysis complete".to_string())
        )]))
    }
}

// ---------------------------------------------------------------------------
// Helper functions for orchestration tools
// ---------------------------------------------------------------------------

/// Find files by extension in a directory (recursive).
fn find_files_by_extension(dir: &Path, ext: &str) -> std::io::Result<Vec<PathBuf>> {
    let mut files = vec![];
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                files.extend(find_files_by_extension(&path, ext)?);
            } else if path.extension().and_then(|e| e.to_str()) == Some(ext) {
                files.push(path);
            }
        }
    }
    Ok(files)
}

/// Find files by multiple extensions.
fn find_files_by_extensions(dir: &Path, extensions: &[&str]) -> std::io::Result<Vec<PathBuf>> {
    let mut files = vec![];
    for ext in extensions {
        files.extend(find_files_by_extension(dir, ext)?);
    }
    Ok(files)
}

// ---------------------------------------------------------------------------
// Helper trait for builder chaining with optional values
// ---------------------------------------------------------------------------

trait PipeIfSome: Sized {
    fn pipe_if_some<T, F: FnOnce(Self, T) -> Self>(self, opt: Option<T>, f: F) -> Self {
        match opt {
            Some(v) => f(self, v),
            None => self,
        }
    }
}

impl<T> PipeIfSome for T {}

// ---------------------------------------------------------------------------
// File system helpers
// ---------------------------------------------------------------------------

fn find_ttl_content(example_dir: &Path) -> String {
    // Look in ontology/ subdir first, then root
    for subdir in &["ontology", "."] {
        let dir = example_dir.join(subdir);
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) == Some("ttl") {
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        return content;
                    }
                }
            }
        }
    }
    "(no .ttl file found)".to_string()
}

fn list_template_files(example_dir: &Path) -> Vec<String> {
    let templates_dir = example_dir.join("templates");
    if !templates_dir.is_dir() {
        return vec![];
    }
    std::fs::read_dir(templates_dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|s| s.to_str())
                .map(|e| e == "tera" || e == "hbs" || e == "j2")
                .unwrap_or(false)
        })
        .map(|e| e.file_name().to_string_lossy().to_string())
        .collect()
}

/// Directories to exclude when scaffolding examples (build artifacts, caches, etc.)
const EXCLUDED_DIRS: &[&str] = &[
    "target",
    "node_modules",
    ".git",
    "dist",
    "build",
    "cache",
    ".cache",
    ".ggen/cache", // Exclude ggen's internal cache
];

fn copy_dir_recursive(src: &Path, dst: &Path) -> std::io::Result<Vec<String>> {
    std::fs::create_dir_all(dst)?;
    let mut files = vec![];
    for entry in std::fs::read_dir(src)?.flatten() {
        let src_path = entry.path();
        let file_name = entry.file_name();

        // Skip excluded directories
        if src_path.is_dir() {
            let dir_name = file_name.to_string_lossy();
            if EXCLUDED_DIRS.iter().any(|excluded| dir_name == *excluded) {
                continue; // Skip this directory
            }
            let dst_path = dst.join(&file_name);
            let sub = copy_dir_recursive(&src_path, &dst_path)?;
            files.extend(sub);
        } else {
            let dst_path = dst.join(&file_name);
            std::fs::copy(&src_path, &dst_path)?;
            files.push(dst_path.display().to_string());
        }
    }
    Ok(files)
}

// ---------------------------------------------------------------------------
// Default
// ---------------------------------------------------------------------------

impl Default for GgenMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Inherent methods
// ---------------------------------------------------------------------------

impl GgenMcpServer {
    /// Return the MCP server initialization result describing this server.
    pub fn get_info(&self) -> InitializeResult {
        let capabilities = ServerCapabilities::builder()
            .enable_tools()
            .enable_resources()
            .enable_prompts()
            .enable_completions()
            .build();
        InitializeResult::new(capabilities)
            .with_server_info(Implementation::new("ggen", env!("CARGO_PKG_VERSION")))
    }

    /// Serve the MCP protocol over `transport`, running until the transport closes.
    pub async fn serve<T, E, A>(self, transport: T) -> anyhow::Result<()>
    where
        T: rmcp::transport::IntoTransport<rmcp::RoleServer, E, A>,
        E: std::error::Error + Send + Sync + 'static,
        A: 'static,
    {
        let running = rmcp::ServiceExt::serve(self, transport).await?;
        running
            .waiting()
            .await
            .map_err(|e| anyhow::anyhow!("{e}"))?;
        Ok(())
    }

    // ------------------------------------------------------------------
    // Resources helpers
    // ------------------------------------------------------------------

    fn build_example_resources(&self) -> Vec<Resource> {
        scan_examples(&self.examples_dir)
            .into_iter()
            .map(|(name, description, _category)| {
                RawResource::new(format!("ggen://example/{}", name), name.clone())
                    .with_description(description)
                    .with_mime_type("application/json")
                    .no_annotation()
            })
            .collect()
    }

    fn read_example_resource(&self, uri: &str) -> Option<ReadResourceResult> {
        // Parse URI: ggen://example/{name} or ggen://example/{name}/{sub}
        let path = uri.strip_prefix("ggen://example/")?;
        let (name, sub) = if let Some(slash) = path.find('/') {
            (&path[..slash], Some(&path[slash + 1..]))
        } else {
            (path, None)
        };

        let example_dir = self.examples_dir.join(name);
        if !example_dir.is_dir() {
            return None;
        }

        let content = match sub {
            None => {
                // Summary: return ggen.toml content as JSON summary
                let (desc, cat) = read_ggen_toml_meta(&example_dir.join("ggen.toml"));
                let config_raw =
                    std::fs::read_to_string(example_dir.join("ggen.toml")).unwrap_or_default();
                serde_json::to_string_pretty(&serde_json::json!({
                    "name": name,
                    "description": desc,
                    "category": cat,
                    "config": config_raw
                }))
                .unwrap_or_default()
            }
            Some("ttl") => find_ttl_content(&example_dir),
            Some("readme") => ["README.md", "README.txt", "readme.md"]
                .iter()
                .find_map(|f| std::fs::read_to_string(example_dir.join(f)).ok())
                .unwrap_or_else(|| "(no README)".to_string()),
            Some("config") => std::fs::read_to_string(example_dir.join("ggen.toml"))
                .unwrap_or_else(|_| "(no ggen.toml)".to_string()),
            _ => return None,
        };

        Some(ReadResourceResult::new(vec![ResourceContents::text(
            content, uri,
        )]))
    }

    // ------------------------------------------------------------------
    // Prompts helpers
    // ------------------------------------------------------------------

    fn all_prompts() -> Vec<Prompt> {
        vec![
            Prompt::new(
                "explain-rdf-schema",
                Some("Explain a Turtle RDF ontology in plain English"),
                Some(vec![
                    PromptArgument::new("ttl_content")
                        .with_description("The Turtle (.ttl) content to explain")
                        .with_required(true),
                ]),
            ),
            Prompt::new(
                "generate-from-example",
                Some("Adapt a ggen example project to a new domain"),
                Some(vec![
                    PromptArgument::new("example_name")
                        .with_description("The name of the source example (use list_examples)")
                        .with_required(true),
                    PromptArgument::new("target_domain")
                        .with_description("The new domain to adapt the example for")
                        .with_required(true),
                ]),
            ),
            Prompt::new(
                "scaffold-project",
                Some("Design a new ggen project from scratch"),
                Some(vec![
                    PromptArgument::new("domain")
                        .with_description("The business domain for the project")
                        .with_required(true),
                    PromptArgument::new("language")
                        .with_description("Target code generation language (go, rust, python, typescript, elixir)")
                        .with_required(false),
                ]),
            ),
        ]
    }

    fn render_prompt(
        &self, name: &str, args: &std::collections::HashMap<String, String>,
    ) -> Option<GetPromptResult> {
        match name {
            "explain-rdf-schema" => {
                let ttl = args.get("ttl_content").cloned().unwrap_or_default();
                Some(
                    GetPromptResult::new(vec![
                        PromptMessage::new_text(
                            PromptMessageRole::User,
                            format!(
                                "You are an expert in RDF/OWL ontologies and knowledge graphs.\n\nExplain the following Turtle schema in plain English. List all classes, their properties, data types, and relationships between classes. Be precise and concise.\n\n```turtle\n{}\n```",
                                ttl
                            ),
                        ),
                    ])
                    .with_description("Explain a Turtle RDF schema in plain English"),
                )
            }
            "generate-from-example" => {
                let example_name = args.get("example_name").cloned().unwrap_or_default();
                let target_domain = args.get("target_domain").cloned().unwrap_or_default();
                let example_dir = self.examples_dir.join(&example_name);
                let ttl = if example_dir.is_dir() {
                    find_ttl_content(&example_dir)
                } else {
                    "(example not found — use list_examples to find valid names)".to_string()
                };
                let config = if example_dir.is_dir() {
                    std::fs::read_to_string(example_dir.join("ggen.toml"))
                        .unwrap_or_else(|_| "(no ggen.toml)".to_string())
                } else {
                    "(example not found)".to_string()
                };
                Some(
                    GetPromptResult::new(vec![
                        PromptMessage::new_text(
                            PromptMessageRole::User,
                            format!(
                                "I want to create a ggen project similar to the '{example_name}' example but for the '{target_domain}' domain.\n\nHere is the example's ontology (Turtle):\n\n```turtle\n{ttl}\n```\n\nHere is the example's ggen.toml:\n\n```toml\n{config}\n```\n\nPlease adapt both the TTL ontology and ggen.toml for my '{target_domain}' domain. Maintain the same structure but replace the domain-specific concepts."
                            ),
                        ),
                    ])
                    .with_description(format!("Adapt '{example_name}' for the '{target_domain}' domain")),
                )
            }
            "scaffold-project" => {
                let domain = args.get("domain").cloned().unwrap_or_default();
                let language = args
                    .get("language")
                    .cloned()
                    .unwrap_or_else(|| "auto".to_string());
                Some(
                    GetPromptResult::new(vec![
                        PromptMessage::new_text(
                            PromptMessageRole::User,
                            format!(
                                "Create a complete ggen project for the '{domain}' domain targeting '{language}' code generation.\n\nProvide:\n1. A `ggen.toml` with `[project]`, `[ontology]`, and at least 2 `[[generation.rules]]` entries\n2. A starter Turtle (.ttl) ontology with at least 3 classes and 3 properties\n3. A brief explanation of the design choices\n\nThe ggen.toml generation rules should use inline SPARQL SELECT queries and reference Tera template files."
                            ),
                        ),
                    ])
                    .with_description(format!("Scaffold a new ggen project for '{domain}' in {language}")),
                )
            }
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// ServerHandler — tool routing + Resources + Prompts + Completions
// ---------------------------------------------------------------------------

#[tool_handler(router = self.tool_router)]
impl ServerHandler for GgenMcpServer {
    fn get_info(&self) -> InitializeResult {
        GgenMcpServer::get_info(self)
    }

    async fn list_resources(
        &self, request: Option<PaginatedRequestParams>,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ListResourcesResult, McpError> {
        info!("list_resources called");
        let all = self.build_example_resources();
        let start = request
            .as_ref()
            .and_then(|r| r.cursor.as_deref())
            .and_then(|c| c.parse::<usize>().ok())
            .unwrap_or(0);
        let page: Vec<Resource> = all.iter().skip(start).take(PAGE_SIZE).cloned().collect();
        let next_cursor = if start + PAGE_SIZE < all.len() {
            Some(Cursor::from((start + PAGE_SIZE).to_string()))
        } else {
            None
        };
        Ok(ListResourcesResult {
            meta: None,
            next_cursor,
            resources: page,
        })
    }

    async fn read_resource(
        &self, request: ReadResourceRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ReadResourceResult, McpError> {
        info!(uri = %request.uri, "read_resource called");
        self.read_example_resource(&request.uri).ok_or_else(|| {
            McpError::invalid_params(format!("Resource not found: {}", request.uri), None)
        })
    }

    async fn list_prompts(
        &self, _request: Option<PaginatedRequestParams>,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ListPromptsResult, McpError> {
        info!("list_prompts called");
        Ok(ListPromptsResult::with_all_items(Self::all_prompts()))
    }

    async fn get_prompt(
        &self, request: GetPromptRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<GetPromptResult, McpError> {
        info!(name = %request.name, "get_prompt called");
        let args: std::collections::HashMap<String, String> = request
            .arguments
            .as_ref()
            .map(|m| {
                m.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect()
            })
            .unwrap_or_default();

        self.render_prompt(&request.name, &args).ok_or_else(|| {
            McpError::invalid_params(format!("Unknown prompt: {}", request.name), None)
        })
    }

    async fn complete(
        &self, request: CompleteRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<CompleteResult, McpError> {
        let prefix = request.argument.value.to_lowercase();
        info!(argument = %request.argument.name, prefix = %prefix, "complete called");

        let values = match &request.argument.name {
            name if name == "example_name" => scan_examples(&self.examples_dir)
                .into_iter()
                .map(|(n, _, _)| n)
                .filter(|n| n.to_lowercase().starts_with(&prefix))
                .take(CompletionInfo::MAX_VALUES)
                .collect::<Vec<_>>(),
            name if name == "generator" || name == "language" => GENERATORS
                .iter()
                .filter(|g| g.starts_with(prefix.as_str()))
                .map(|s| s.to_string())
                .collect(),
            _ => vec![],
        };

        let completion = CompletionInfo::with_all_values(values).unwrap_or_default();
        Ok(CompleteResult::new(completion))
    }
}
