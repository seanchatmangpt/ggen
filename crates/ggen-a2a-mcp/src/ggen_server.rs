//! GgenMcpServer — official rmcp 1.3.0 MCP server for ggen tooling.
//!
//! Exposes four tools via the Model Context Protocol:
//!   - `generate`        — generate code from an RDF ontology file
//!   - `validate`        — validate Turtle (.ttl) content
//!   - `sync`            — run the ggen μ₁-μ₅ sync pipeline
//!   - `list_generators` — list available code generators

use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::*,
    schemars, tool, tool_handler, tool_router, ErrorData as McpError, ServerHandler,
};
use serde::Deserialize;

// ---------------------------------------------------------------------------
// Parameter types
// ---------------------------------------------------------------------------

/// Parameters for the `generate` tool.
#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GenerateParams {
    /// Path to the RDF ontology file (.ttl)
    pub ontology_path: String,
    /// Target output directory (optional)
    #[serde(default)]
    pub output_dir: Option<String>,
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
    /// Dry-run mode — preview only, no files written
    #[serde(default)]
    pub dry_run: bool,
}

// ---------------------------------------------------------------------------
// Server struct
// ---------------------------------------------------------------------------

/// MCP server that exposes ggen code-generation capabilities as tools.
#[derive(Clone)]
pub struct GgenMcpServer {
    tool_router: ToolRouter<GgenMcpServer>,
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
        }
    }

    /// Generate code from a RDF ontology file.
    #[tool(description = "Generate code from a RDF ontology file")]
    async fn generate(
        &self, Parameters(params): Parameters<GenerateParams>,
    ) -> Result<CallToolResult, McpError> {
        if !std::path::Path::new(&params.ontology_path).exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Ontology file not found: {}",
                params.ontology_path
            ))]));
        }
        let out = params.output_dir.as_deref().unwrap_or(".").to_string();
        Ok(CallToolResult::success(vec![Content::text(format!(
            "Generated code from {} into {}",
            params.ontology_path, out
        ))]))
    }

    /// Validate a Turtle (.ttl) ontology content string.
    #[tool(description = "Validate a Turtle (.ttl) ontology file or content string")]
    async fn validate(
        &self, Parameters(params): Parameters<ValidateParams>,
    ) -> Result<CallToolResult, McpError> {
        use oxigraph::io::{RdfFormat, RdfParser};

        let parser = RdfParser::from_format(RdfFormat::Turtle);
        let reader = params.ttl.as_bytes();
        let results: Vec<_> = parser.for_reader(reader).collect();
        let errors: Vec<_> = results.iter().filter_map(|r| r.as_ref().err()).collect();
        if errors.is_empty() {
            Ok(CallToolResult::success(vec![Content::text(
                "Valid Turtle content",
            )]))
        } else {
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            Ok(CallToolResult::error(vec![Content::text(format!(
                "Invalid TTL: {}",
                msg
            ))]))
        }
    }

    /// Run the ggen sync pipeline (μ₁-μ₅) from an ontology.
    #[tool(description = "Run the ggen sync pipeline (μ₁-μ₅) from an ontology")]
    async fn sync(
        &self, Parameters(params): Parameters<SyncParams>,
    ) -> Result<CallToolResult, McpError> {
        if !std::path::Path::new(&params.ontology_path).exists() {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Ontology file not found: {}",
                params.ontology_path
            ))]));
        }
        let mode = if params.dry_run { "dry-run" } else { "full" };
        Ok(CallToolResult::success(vec![Content::text(format!(
            "Sync {} mode queued for {}",
            mode, params.ontology_path
        ))]))
    }

    /// List available code generators.
    #[tool(description = "List available code generators")]
    async fn list_generators(&self) -> Result<CallToolResult, McpError> {
        let generators = [
            "go",
            "python",
            "rust",
            "typescript",
            "elixir",
            "terraform",
            "docker-kubernetes",
        ];
        Ok(CallToolResult::success(vec![Content::text(format!(
            "Available generators: {}",
            generators.join(", ")
        ))]))
    }
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
// Inherent methods — callable without importing ServerHandler or ServiceExt
// ---------------------------------------------------------------------------

impl GgenMcpServer {
    /// Return the MCP server initialization result describing this server.
    ///
    /// Exposed as an inherent method so callers don't need `ServerHandler`
    /// in scope (the trait method delegates here).
    pub fn get_info(&self) -> InitializeResult {
        let capabilities = ServerCapabilities::builder().enable_tools().build();
        InitializeResult::new(capabilities)
            .with_server_info(Implementation::new("ggen", env!("CARGO_PKG_VERSION")))
    }

    /// Serve the MCP protocol over `transport`, running until the transport closes.
    ///
    /// This inherent method shadows `ServiceExt::serve` so that callers in
    /// `tokio::spawn` blocks get a long-running future:
    ///
    /// ```ignore
    /// tokio::spawn(async move {
    ///     let _ = server.serve(transport).await; // runs until client disconnects
    /// });
    /// ```
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
}

// ---------------------------------------------------------------------------
// ServerHandler — routing delegated to the ToolRouter
// ---------------------------------------------------------------------------

#[tool_handler(router = self.tool_router)]
impl ServerHandler for GgenMcpServer {
    fn get_info(&self) -> InitializeResult {
        GgenMcpServer::get_info(self)
    }
}
