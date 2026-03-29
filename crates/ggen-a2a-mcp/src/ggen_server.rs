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

use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::*,
    schemars, tool, tool_handler, tool_router, ErrorData as McpError, ServerHandler,
};
use serde::Deserialize;

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
        let name = path.file_name().unwrap_or_default().to_string_lossy().to_string();
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
    let description = extract_toml_field(&content, "description")
        .unwrap_or_else(|| "No description".to_string());
    let category = extract_toml_field(&content, "category")
        .unwrap_or_else(|| "general".to_string());
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
    ontology_path: &str,
    queries_dir: Option<&str>,
    output_dir: Option<&str>,
) -> (PathBuf, PathBuf, PathBuf) {
    let ontology = PathBuf::from(ontology_path);
    let parent = ontology.parent().unwrap_or_else(|| Path::new(".")).to_path_buf();
    let queries = queries_dir.map(PathBuf::from).unwrap_or_else(|| parent.join("queries"));
    let output = output_dir.map(PathBuf::from).unwrap_or_else(|| parent.join("generated"));
    (ontology, queries, output)
}

/// Run the ggen sync pipeline on a blocking thread.
fn run_sync_blocking(
    ontology_path: PathBuf,
    queries_dir: PathBuf,
    output_dir: PathBuf,
    language: String,
    dry_run: bool,
    validate: bool,
) -> Result<ggen_core::sync::SyncResult, ggen_core::sync::SyncError> {
    use ggen_core::sync::{SyncConfig, sync};
    let lang = language.parse().unwrap_or(ggen_core::sync::SyncLanguage::Auto);
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
    #[tool(description = "Generate code from a RDF ontology file via the ggen μ₁-μ₅ pipeline. Requires ontology_path (.ttl) and optionally queries_dir (.rq files), output_dir, language (go/rust/python/typescript/elixir/auto).")]
    async fn generate(
        &self, Parameters(params): Parameters<GenerateParams>,
    ) -> Result<CallToolResult, McpError> {
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
        let lang = params.language.clone().unwrap_or_else(|| "auto".to_string());
        let result = tokio::task::spawn_blocking(move || {
            run_sync_blocking(ontology, queries, output, lang, false, true)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        match result {
            Ok(r) => {
                let files: Vec<String> =
                    r.files_generated.iter().map(|p| p.display().to_string()).collect();
                let violations = if r.soundness_violations.is_empty() {
                    "none".to_string()
                } else {
                    format!("{} violations", r.soundness_violations.len())
                };
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "Generated {} file(s) in {}ms\nFiles: {}\nSoundness violations: {}\nReceipt: {}",
                    files.len(),
                    r.elapsed_ms,
                    files.join(", "),
                    violations,
                    r.receipt
                ))]))
            }
            Err(e) => Ok(CallToolResult::error(vec![Content::text(format!(
                "Sync pipeline failed: {}",
                e
            ))])),
        }
    }

    /// Validate a Turtle (.ttl) ontology content string using oxigraph parser.
    #[tool(description = "Validate a Turtle (.ttl) ontology string for syntax correctness. Returns 'Valid' or a list of parse errors.")]
    async fn validate(
        &self, Parameters(params): Parameters<ValidateParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::io::{RdfFormat, RdfParser};

        let parser = RdfParser::from_format(RdfFormat::Turtle);
        let reader = params.ttl.as_bytes();
        let results: Vec<_> = parser.for_reader(reader).collect();
        let errors: Vec<_> = results.iter().filter_map(|r| r.as_ref().err()).collect();
        let triple_count = results.iter().filter(|r| r.is_ok()).count();

        if errors.is_empty() {
            Ok(CallToolResult::success(vec![Content::text(format!(
                "Valid Turtle content ({} triple(s) parsed)",
                triple_count
            ))]))
        } else {
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
    #[tool(description = "Run the full ggen sync pipeline (μ₁-μ₅: Load→Extract→Generate→Validate→Write) from a .ttl ontology. Requires ontology_path; optionally queries_dir, output_dir, language, dry_run.")]
    async fn sync(
        &self, Parameters(params): Parameters<SyncParams>,
    ) -> Result<CallToolResult, McpError> {
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
        let lang = params.language.clone().unwrap_or_else(|| "auto".to_string());
        let dry_run = params.dry_run;
        let result = tokio::task::spawn_blocking(move || {
            run_sync_blocking(ontology, queries, output, lang, dry_run, true)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        match result {
            Ok(r) => {
                let mode = if dry_run { "dry-run" } else { "full" };
                let files: Vec<String> =
                    r.files_generated.iter().map(|p| p.display().to_string()).collect();
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "Sync ({}) complete: {} file(s) in {}ms\nFiles: {}\nReceipt: {}",
                    mode,
                    files.len(),
                    r.elapsed_ms,
                    if files.is_empty() { "none".to_string() } else { files.join(", ") },
                    r.receipt
                ))]))
            }
            Err(e) => Ok(CallToolResult::error(vec![Content::text(format!(
                "Sync pipeline failed: {}",
                e
            ))])),
        }
    }

    /// List available code generators.
    #[tool(description = "List available code generators supported by ggen (go, rust, python, typescript, elixir, terraform, docker-kubernetes).")]
    async fn list_generators(&self) -> Result<CallToolResult, McpError> {
        let details = serde_json::json!({
            "generators": GENERATORS,
            "count": GENERATORS.len(),
            "default": "auto"
        });
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&details)
                .unwrap_or_else(|_| GENERATORS.join(", ")),
        )]))
    }

    // ------------------------------------------------------------------
    // Examples tools
    // ------------------------------------------------------------------

    /// List bundled ggen example projects with optional category filter.
    #[tool(description = "List bundled ggen example projects. Optionally filter by category. Returns name, description, and category for each example.")]
    async fn list_examples(
        &self, Parameters(params): Parameters<ListExamplesParams>,
    ) -> Result<CallToolResult, McpError> {
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
    #[tool(description = "Get details of a specific ggen example: ggen.toml config, ontology TTL content, README, and template file list.")]
    async fn get_example(
        &self, Parameters(params): Parameters<GetExampleParams>,
    ) -> Result<CallToolResult, McpError> {
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
    #[tool(description = "Search ggen marketplace packages by query string. Optionally filter by category. Returns matching packages with name, description, version, tags.")]
    async fn search(
        &self, Parameters(params): Parameters<SearchParams>,
    ) -> Result<CallToolResult, McpError> {
        use ggen_domain::marketplace::search::{SearchFilters, search_packages};

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
                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&serde_json::json!({
                        "query": params.query,
                        "results": items,
                        "count": items.len()
                    }))
                    .unwrap_or_else(|_| format!("{} results", items.len())),
                )]))
            }
            Err(e) => Ok(CallToolResult::error(vec![Content::text(format!(
                "Search failed: {}",
                e
            ))])),
        }
    }

    /// Scaffold a new project by copying a bundled example to a target directory.
    #[tool(description = "Scaffold a new ggen project from a bundled example. Copies the example directory to target_dir as a starting point.")]
    async fn scaffold_from_example(
        &self, Parameters(params): Parameters<ScaffoldParams>,
    ) -> Result<CallToolResult, McpError> {
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
            Err(e) => Ok(CallToolResult::error(vec![Content::text(format!(
                "Scaffold failed: {}",
                e
            ))])),
        }
    }

    /// Run a SPARQL SELECT query against an inline TTL string.
    #[tool(description = "Execute a SPARQL SELECT query against a Turtle (.ttl) ontology string. Returns query results as JSON rows.")]
    async fn query_ontology(
        &self, Parameters(params): Parameters<QueryOntologyParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::{
            io::RdfFormat,
            sparql::{QueryResults, SparqlEvaluator},
            store::Store,
        };

        // Load TTL into in-memory store
        let store = Store::new().map_err(|e| McpError::internal_error(e.to_string(), None))?;
        store
            .load_from_reader(RdfFormat::Turtle, params.ttl.as_bytes())
            .map_err(|e| McpError::invalid_params(format!("Invalid TTL: {}", e), None))?;

        // Execute SPARQL query via SparqlEvaluator (non-deprecated API)
        let results = SparqlEvaluator::new()
            .parse_query(&params.sparql)
            .map_err(|e| McpError::invalid_params(format!("SPARQL parse error: {}", e), None))?
            .on_store(&store)
            .execute()
            .map_err(|e| McpError::internal_error(format!("SPARQL execution error: {}", e), None))?;

        match results {
            QueryResults::Solutions(solutions) => {
                let mut rows: Vec<serde_json::Value> = vec![];
                for solution in solutions.flatten() {
                    let row: serde_json::Map<String, serde_json::Value> = solution
                        .iter()
                        .map(|(var, term)| {
                            (var.as_str().to_string(), serde_json::Value::String(term.to_string()))
                        })
                        .collect();
                    rows.push(serde_json::Value::Object(row));
                }
                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&serde_json::json!({
                        "rows": rows,
                        "count": rows.len()
                    }))
                    .unwrap_or_else(|_| format!("{} results", rows.len())),
                )]))
            }
            _ => Ok(CallToolResult::error(vec![Content::text(
                "Only SELECT queries are supported".to_string(),
            )])),
        }
    }
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

fn copy_dir_recursive(src: &Path, dst: &Path) -> std::io::Result<Vec<String>> {
    std::fs::create_dir_all(dst)?;
    let mut files = vec![];
    for entry in std::fs::read_dir(src)?.flatten() {
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if src_path.is_dir() {
            let sub = copy_dir_recursive(&src_path, &dst_path)?;
            files.extend(sub);
        } else {
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
                RawResource::new(
                    format!("ggen://example/{}", name),
                    name.clone(),
                )
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
                let config_raw = std::fs::read_to_string(example_dir.join("ggen.toml"))
                    .unwrap_or_default();
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

        Some(ReadResourceResult::new(vec![
            ResourceContents::text(content, uri),
        ]))
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
        &self,
        name: &str,
        args: &std::collections::HashMap<String, String>,
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
                let language = args.get("language").cloned().unwrap_or_else(|| "auto".to_string());
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
        &self,
        request: Option<PaginatedRequestParams>,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ListResourcesResult, McpError> {
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
        &self,
        request: ReadResourceRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ReadResourceResult, McpError> {
        self.read_example_resource(&request.uri).ok_or_else(|| {
            McpError::invalid_params(
                format!("Resource not found: {}", request.uri),
                None,
            )
        })
    }

    async fn list_prompts(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<ListPromptsResult, McpError> {
        Ok(ListPromptsResult::with_all_items(Self::all_prompts()))
    }

    async fn get_prompt(
        &self,
        request: GetPromptRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<GetPromptResult, McpError> {
        let args: std::collections::HashMap<String, String> = request
            .arguments
            .as_ref()
            .map(|m| {
                m.iter()
                    .filter_map(|(k, v)| {
                        v.as_str().map(|s| (k.clone(), s.to_string()))
                    })
                    .collect()
            })
            .unwrap_or_default();

        self.render_prompt(&request.name, &args).ok_or_else(|| {
            McpError::invalid_params(
                format!("Unknown prompt: {}", request.name),
                None,
            )
        })
    }

    async fn complete(
        &self,
        request: CompleteRequestParams,
        _context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> Result<CompleteResult, McpError> {
        let prefix = request.argument.value.to_lowercase();

        let values = match &request.argument.name {
            name if name == "example_name" => {
                scan_examples(&self.examples_dir)
                    .into_iter()
                    .map(|(n, _, _)| n)
                    .filter(|n| n.to_lowercase().starts_with(&prefix))
                    .take(CompletionInfo::MAX_VALUES)
                    .collect::<Vec<_>>()
            }
            name if name == "generator" || name == "language" => GENERATORS
                .iter()
                .filter(|g| g.starts_with(prefix.as_str()))
                .map(|s| s.to_string())
                .collect(),
            _ => vec![],
        };

        let completion = CompletionInfo::with_all_values(values)
            .unwrap_or_default();
        Ok(CompleteResult::new(completion))
    }
}
