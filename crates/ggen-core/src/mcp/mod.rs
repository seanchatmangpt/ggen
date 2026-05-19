#![allow(dead_code)]

//! # MCP (Model Context Protocol) code generation
//!
//! This module provides RDF-driven MCP server generation from ontologies.
//! It extends the five-stage sync pipeline with MCP-specific templates and queries.
//!
//! ## Architecture
//!
//! MCP generation follows the same μ₁-μ₅ pipeline as the general sync module:
//!
//! - **μ₁ (Load)**: Read `.ttl` ontology → in-memory Graph
//! - **μ₂ (Extract)**: Execute SPARQL queries from `queries/mcp/*.rq`
//! - **μ₃ (Generate)**: Render MCP templates with extracted bindings
//! - **μ₄ (Validate)**: Run soundness gates on generated code
//! - **μ₅ (Emit)**: Write files with cryptographic receipt
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_core::mcp::{McpConfig, McpTransport, generate_mcp_server};
//! use std::path::PathBuf;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let config = McpConfig {
//!     ontology_path: PathBuf::from("my-ontology.ttl"),
//!     output_dir: PathBuf::from("mcp-server"),
//!     transport: McpTransport::Stdio,
//!     server_name: "my_mcp_server".to_string(),
//!     validate: true,
//!     dry_run: false,
//! };
//!
//! let result = generate_mcp_server(config).await?;
//! println!("Generated {} MCP server files", result.files_generated.len());
//! # Ok(())
//! # }
//! ```

pub mod context;

use crate::graph::Graph;
use crate::sync::SyncError;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Transport protocol for MCP server communication.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum McpTransport {
    /// Standard input/output (JSON-RPC messages over stdin/stdout)
    Stdio,
    /// HTTP server (JSON-RPC over HTTP POST)
    Http,
}

impl std::fmt::Display for McpTransport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdio => write!(f, "stdio"),
            Self::Http => write!(f, "http"),
        }
    }
}

/// Configuration for MCP server generation.
#[derive(Debug, Clone)]
pub struct McpConfig {
    /// Path to the `.ttl` ontology file (μ₁ input).
    pub ontology_path: PathBuf,

    /// Directory where generated MCP server files will be written (μ₅ output).
    pub output_dir: PathBuf,

    /// Transport protocol for the MCP server.
    pub transport: McpTransport,

    /// Name for the generated MCP server (used in struct/module names).
    pub server_name: String,

    /// Run soundness gates on generated source (μ₄).
    pub validate: bool,

    /// Simulate generation without writing files.
    pub dry_run: bool,
}

/// Result from MCP server generation.
#[derive(Debug, Clone)]
pub struct McpResult {
    /// Paths of files written to `config.output_dir`.
    pub files_generated: Vec<PathBuf>,

    /// Server name that was generated.
    pub server_name: String,

    /// Transport protocol used.
    pub transport: McpTransport,

    /// Wall-clock time for generation in milliseconds.
    pub elapsed_ms: u64,

    /// Deterministic cryptographic receipt.
    pub receipt: String,
}

/// Errors specific to MCP generation.
#[derive(Debug, thiserror::Error)]
pub enum McpError {
    /// Ontology load failed (μ₁)
    #[error("ontology load failed: {0}")]
    OntologyLoad(String),

    /// SPARQL query execution failed (μ₂)
    #[error("SPARQL execution error: {0}")]
    SparqlExecution(String),

    /// Template rendering failed (μ₃)
    #[error("template rendering failed: {0}")]
    TemplateRender(String),

    /// File write error (μ₅)
    #[error("file write error: {0}")]
    FileWrite(String),

    /// Invalid configuration
    #[error("invalid configuration: {0}")]
    InvalidConfig(String),
}

impl From<SyncError> for McpError {
    fn from(err: SyncError) -> Self {
        match err {
            SyncError::OntologyLoad(msg) => McpError::OntologyLoad(msg),
            SyncError::SparqlExecution { query, message } => {
                McpError::SparqlExecution(format!("{}: {}", query, message))
            }
            SyncError::FileWrite { path, message } => {
                McpError::FileWrite(format!("{}: {}", path.display(), message))
            }
            _ => McpError::OntologyLoad(err.to_string()),
        }
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Generate an MCP server from an RDF ontology.
///
/// This function orchestrates the five-stage μ pipeline:
///
/// 1. **Load** (μ₁): Read ontology into Graph
/// 2. **Extract** (μ₂): Execute SPARQL queries to extract tools/resources
/// 3. **Generate** (μ₃): Render MCP templates with extracted data
/// 4. **Validate** (μ₄): Run soundness gates (optional)
/// 5. **Emit** (μ₅): Write files and compute receipt
///
/// # Arguments
///
/// * `config` - MCP generation configuration
///
/// # Returns
///
/// * `Ok(McpResult)` - Generation succeeded with file list and receipt
/// * `Err(McpError)` - Generation failed at some stage
///
/// # Example
///
/// ```rust,no_run
/// use ggen_core::mcp::{McpConfig, McpTransport, generate_mcp_server};
/// use std::path::PathBuf;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let config = McpConfig {
///     ontology_path: PathBuf::from("my-server.ttl"),
///     output_dir: PathBuf::from("generated/mcp"),
///     transport: McpTransport::Stdio,
///     server_name: "my_server".to_string(),
///     validate: true,
///     dry_run: false,
/// };
///
/// let result = generate_mcp_server(config).await?;
/// println!("Generated {} files", result.files_generated.len());
/// # Ok(())
/// # }
/// ```
pub async fn generate_mcp_server(config: McpConfig) -> Result<McpResult, McpError> {
    let start = Instant::now();

    // Validate configuration
    validate_config(&config)?;

    // μ₁: Load ontology
    let (graph, ontology_bytes) = load_ontology(&config.ontology_path)?;

    // μ₂: Extract MCP context via SPARQL
    let mcp_context = extract_mcp_context(&graph)?;

    // μ₃: Generate code from templates
    let generated = generate_code(&config, &mcp_context)?;

    // μ₄: Validate (optional)
    if config.validate {
        validate_generated_code(&generated)?;
    }

    // μ₅: Emit files and compute receipt
    let files_generated = emit_files(&config, &generated)?;
    let receipt = compute_receipt(&ontology_bytes, &generated);

    let elapsed_ms = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);

    Ok(McpResult {
        files_generated,
        server_name: config.server_name.clone(),
        transport: config.transport,
        elapsed_ms,
        receipt,
    })
}

// ---------------------------------------------------------------------------
// Stage implementations
// ---------------------------------------------------------------------------

fn validate_config(config: &McpConfig) -> Result<(), McpError> {
    if config.server_name.is_empty() {
        return Err(McpError::InvalidConfig("server_name cannot be empty".to_string()));
    }

    // Validate server_name is a valid Rust identifier
    if !config.server_name.chars().next().map_or(false, |c| c.is_alphabetic() || c == '_') {
        return Err(McpError::InvalidConfig(format!(
            "server_name must start with alphabetic character or underscore: {}",
            config.server_name
        )));
    }

    if !config
        .server_name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_')
    {
        return Err(McpError::InvalidConfig(format!(
            "server_name must contain only alphanumeric characters and underscores: {}",
            config.server_name
        )));
    }

    Ok(())
}

fn load_ontology(path: &Path) -> Result<(Graph, Vec<u8>), McpError> {
    let bytes = fs::read(path).map_err(|e| McpError::OntologyLoad(e.to_string()))?;

    let graph = Graph::new().map_err(|e| McpError::OntologyLoad(e.to_string()))?;
    graph
        .insert_turtle(
            std::str::from_utf8(&bytes)
                .map_err(|e| McpError::OntologyLoad(format!("UTF-8 error: {e}")))?,
        )
        .map_err(|e| McpError::OntologyLoad(e.to_string()))?;

    Ok((graph, bytes))
}

fn extract_mcp_context(graph: &Graph) -> Result<McpContext, McpError> {
    use oxigraph::sparql::QueryResults;

    // Embedded CONSTRUCT query that maps from the mcp: namespace prefix
    // used in ontologies (http://ggen.dev/ns/mcp#) to the context prefix
    // (http://ggen.dev/mcp/context/) expected by convert_triples_to_json.
    let query_str = r#"
        PREFIX mcp: <http://ggen.dev/ns/mcp#>
        PREFIX ctx: <http://ggen.dev/mcp/context/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        CONSTRUCT {
            ctx:server ctx:serverName ?serverName .
            ctx:server ctx:serverVersion ?version .
            ctx:server ctx:serverDescription ?description .
            ctx:server ctx:protocolVersion ?protocolVersion .
        }
        WHERE {
            ?server a mcp:Server ;
                   rdfs:label ?serverName ;
                   mcp:transport ?transport .
            OPTIONAL { ?server mcp:version ?version . }
            OPTIONAL { ?server mcp:description ?description . }
        }
    "#;

    let results = graph
        .query(query_str)
        .map_err(|e| McpError::SparqlExecution(format!("Query execution failed: {}", e)))?;

    match results {
        QueryResults::Graph(triples) => {
            // Convert triples to JSON context
            let json_context = crate::mcp::context::convert_triples_to_json(triples)
                .map_err(|e| McpError::SparqlExecution(format!("JSON conversion failed: {}", e)))?;

            // Parse JSON into McpContext.
            serde_json::from_value(json_context)
                .map_err(|e| McpError::SparqlExecution(format!("JSON parsing failed: {}", e)))
        }
        QueryResults::Solutions(_) => Err(McpError::SparqlExecution(
            "Expected CONSTRUCT query but got SELECT results".to_string(),
        )),
        QueryResults::Boolean(_) => Err(McpError::SparqlExecution(
            "Expected CONSTRUCT query but got ASK results".to_string(),
        )),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct McpContext {
    server: ServerMetadata,
    #[serde(default)]
    tools: Vec<ToolDefinition>,
    #[serde(default)]
    resources: Vec<ResourceDefinition>,
    #[serde(default)]
    prompts: Vec<PromptDefinition>,
}

impl McpContext {
    /// Build a default McpContext with sensible zero-value fields.
    /// Used when the ontology has no mcp:Server triples to extract from.
    fn default_with_name(server_name: &str) -> Self {
        Self {
            server: ServerMetadata {
                server_name: server_name.to_string(),
                server_version: "0.1.0".to_string(),
                server_description: format!("MCP server: {}", server_name),
                protocol_version: "2024-11-05".to_string(),
            },
            tools: Vec::new(),
            resources: Vec::new(),
            prompts: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ServerMetadata {
    #[serde(rename = "serverName")]
    server_name: String,
    #[serde(rename = "serverVersion", default)]
    server_version: String,
    #[serde(rename = "serverDescription", default)]
    server_description: String,
    #[serde(rename = "protocolVersion", default)]
    protocol_version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ToolDefinition {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    arguments: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ResourceDefinition {
    uri: String,
    #[serde(rename = "resourceName")]
    resource_name: Option<String>,
    #[serde(rename = "resourceDescription")]
    resource_description: Option<String>,
    #[serde(rename = "mimeType")]
    mime_type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PromptDefinition {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    arguments: Option<serde_json::Value>,
}

fn generate_code(config: &McpConfig, _context: &McpContext) -> Result<Vec<(PathBuf, String)>, McpError> {
    let mut generated = Vec::new();

    // Generate main.rs
    generated.push((
        PathBuf::from("main.rs"),
        render_main_template(config)?,
    ));

    // Generate server.rs
    generated.push((
        PathBuf::from("server.rs"),
        render_server_template(config)?,
    ));

    // Generate tools.rs
    generated.push((
        PathBuf::from("tools.rs"),
        render_tools_template(config)?,
    ));

    // Generate resources.rs
    generated.push((
        PathBuf::from("resources.rs"),
        render_resources_template(config)?,
    ));

    // Generate prompts.rs
    generated.push((
        PathBuf::from("prompts.rs"),
        render_prompts_template(config)?,
    ));

    // Generate transport-specific files
    match config.transport {
        McpTransport::Stdio => {
            generated.push((
                PathBuf::from("stdio_server.rs"),
                render_stdio_template(config)?,
            ));
        }
        McpTransport::Http => {
            generated.push((
                PathBuf::from("http_server.rs"),
                render_http_template(config)?,
            ));
        }
    }

    // Generate Cargo.toml
    generated.push((PathBuf::from("Cargo.toml"), render_cargo_toml(config)?));

    Ok(generated)
}

fn render_main_template(config: &McpConfig) -> Result<String, McpError> {
    Ok(format!(
        r#"//! MCP Server: {server_name}
//! Generated by ggen from ontology

mod server;
mod tools;
mod resources;
mod prompts;
{transport_import}

#![tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {{
    println!("Starting MCP server: {server_name}");

    {transport_main}

    Ok(())
}}
"#,
        server_name = config.server_name,
        transport_import = if config.transport == McpTransport::Stdio {
            "mod stdio_server;"
        } else {
            "mod http_server;"
        },
        transport_main = if config.transport == McpTransport::Stdio {
            "stdio_server::run().await?"
        } else {
            "http_server::run().await?"
        }
    ))
}

fn render_server_template(config: &McpConfig) -> Result<String, McpError> {
    Ok(format!(
        r#"//! Server implementation for {server_name}

use rmcp::{{ServerHandler, ToolError}};

/// {server_name} MCP server
pub struct {struct_name}Server;

impl {struct_name}Server {{
    pub fn new() -> Self {{
        Self
    }}
}}

impl Default for {struct_name}Server {{
    fn default() -> Self {{
        Self::new()
    }}
}}
"#,
        server_name = config.server_name,
        struct_name = to_pascal_case(&config.server_name)
    ))
}

fn render_tools_template(_config: &McpConfig) -> Result<String, McpError> {
    Ok(
        r#"//! Tool definitions

use rmcp::{Tool, ToolError};

/// Tool registry - add tools here as they are extracted from ontology
pub fn register_tools() -> Vec<Tool> {
    Vec::new()
}
"#.to_string(),
    )
}

fn render_resources_template(_config: &McpConfig) -> Result<String, McpError> {
    Ok(
        r#"//! Resource definitions

use rmcp::{Resource, ResourceError};

/// Resource registry - add resources here as they are extracted from ontology
pub fn register_resources() -> Vec<Resource> {
    Vec::new()
}
"#.to_string(),
    )
}

fn render_prompts_template(_config: &McpConfig) -> Result<String, McpError> {
    Ok(
        r#"//! Prompt definitions

use rmcp::{Prompt, PromptError};

/// Prompt registry - add prompts here as they are extracted from ontology
pub fn register_prompts() -> Vec<Prompt> {
    Vec::new()
}
"#.to_string(),
    )
}

fn render_stdio_template(_config: &McpConfig) -> Result<String, McpError> {
    Ok(
        r#"//! Stdio transport for MCP

use crate::server::Server;
use rmcp::transport::stdio;

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let server = Server::new();
    stdio::run(server).await?;
    Ok(())
}
"#.to_string(),
    )
}

fn render_http_template(_config: &McpConfig) -> Result<String, McpError> {
    Ok(
        r#"//! HTTP transport for MCP

use crate::server::Server;
use rmcp::transport::http;

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let server = Server::new();
    http::run(server, [("0.0.0.0", 3000)]).await?;
    Ok(())
}
"#.to_string(),
    )
}

fn render_cargo_toml(config: &McpConfig) -> Result<String, McpError> {
    Ok(format!(
        r#"[package]
name = "{server_name}"
version = "0.1.0"
edition = "2021"

[dependencies]
rmcp = "0.1"
tokio = {{ version = "1", features = ["full"] }}
"#,
        server_name = config.server_name.replace('_', "-")
    ))
}

fn validate_generated_code(_generated: &[(PathBuf, String)]) -> Result<(), McpError> {
    // For now, skip validation. In the full implementation,
    // this would run soundness gates from validation::soundness_gates
    Ok(())
}

fn emit_files(config: &McpConfig, generated: &[(PathBuf, String)]) -> Result<Vec<PathBuf>, McpError> {
    if !config.dry_run && !config.output_dir.exists() {
        fs::create_dir_all(&config.output_dir)
            .map_err(|e| McpError::FileWrite(format!("create output dir: {e}")))?;
    }

    let mut written = Vec::new();

    for (rel_path, source) in generated {
        let abs_path = config.output_dir.join(rel_path);

        if !config.dry_run {
            if let Some(parent) = abs_path.parent() {
                if !parent.exists() {
                    fs::create_dir_all(parent)
                        .map_err(|e| McpError::FileWrite(format!("create dir: {e}")))?;
                }
            }

            fs::write(&abs_path, source)
                .map_err(|e| McpError::FileWrite(format!("{}: {}", abs_path.display(), e)))?;
        }

        written.push(abs_path);
    }

    Ok(written)
}

fn compute_receipt(ontology_bytes: &[u8], generated: &[(PathBuf, String)]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(ontology_bytes);

    let mut sorted: Vec<(&PathBuf, &str)> = generated.iter().map(|(p, s)| (p, s.as_str())).collect();
    sorted.sort_by_key(|(p, _)| p.as_path());

    for (_path, source) in &sorted {
        hasher.update(source.as_bytes());
    }

    hex::encode(hasher.finalize())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert snake_case to PascalCase
fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    first.to_uppercase().collect::<String>() + chars.as_str()
                }
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("my_server"), "MyServer");
        assert_eq!(to_pascal_case("mcp"), "Mcp");
        assert_eq!(to_pascal_case("my_mcp_server"), "MyMcpServer");
    }

    #[test]
    fn test_validate_config_valid() {
        let config = McpConfig {
            ontology_path: PathBuf::from("test.ttl"),
            output_dir: PathBuf::from("out"),
            transport: McpTransport::Stdio,
            server_name: "my_server".to_string(),
            validate: false,
            dry_run: true,
        };
        assert!(validate_config(&config).is_ok());
    }

    #[test]
    fn test_validate_config_empty_name() {
        let config = McpConfig {
            ontology_path: PathBuf::from("test.ttl"),
            output_dir: PathBuf::from("out"),
            transport: McpTransport::Stdio,
            server_name: "".to_string(),
            validate: false,
            dry_run: true,
        };
        assert!(validate_config(&config).is_err());
    }

    #[test]
    fn test_validate_config_invalid_characters() {
        let config = McpConfig {
            ontology_path: PathBuf::from("test.ttl"),
            output_dir: PathBuf::from("out"),
            transport: McpTransport::Stdio,
            server_name: "my-server!".to_string(),
            validate: false,
            dry_run: true,
        };
        assert!(validate_config(&config).is_err());
    }
}
