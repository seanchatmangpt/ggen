//! `ggen-mcp` — an MCP server exposing ggen's SPARQL/frontmatter/diagnostic
//! introspection surface as tool calls.
//!
//! See `docs/2026-08-03-ggen-mcp-plan.md` (or the plan file this crate was
//! built from) for the full design. In one sentence: ggen's entire CLI is
//! whole-pipeline verbs with no way to ask "does my SPARQL query return
//! rows" or "which frontmatter keys exist" without running the full
//! generator — this crate is that missing introspection surface, as a
//! small, tool-first (not resources/sampling/tasks) MCP server.

pub mod error;
pub mod limits;
pub mod project_root;
pub mod tools;

use std::sync::Arc;

use rmcp::{model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler};

/// Build an rmcp `Tool` from a name, description, and a schemars-derived
/// input schema value. Pattern matches `ggen-lsp`'s own MCP server
/// (`crates/ggen-lsp/src/mcp/mod.rs`) exactly, not a new convention.
fn make_tool(
    name: &'static str, description: &'static str, schema: serde_json::Value,
    annotations: ToolAnnotations,
) -> Tool {
    let mut tool = Tool::new(name, description, Arc::new(schema.as_object().cloned().unwrap_or_default()));
    tool.annotations = Some(annotations);
    tool
}

fn read_only(title: &'static str) -> ToolAnnotations {
    ToolAnnotations::with_title(title).read_only(true).destructive(false)
}

#[allow(dead_code)] // wired up as write_apply lands (task #6)
fn destructive(title: &'static str) -> ToolAnnotations {
    ToolAnnotations::with_title(title).read_only(false).destructive(true)
}

/// The ggen-mcp server: nine tools (see plan), read-only except
/// `ggen_write_apply`, no resources/prompts/sampling/tasks in v1.
#[derive(Clone)]
pub struct GgenMcpServer {
    tools: Arc<Vec<Tool>>,
}

impl Default for GgenMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl GgenMcpServer {
    #[must_use]
    pub fn new() -> Self {
        let tools = vec![make_tool(
            "ggen_query_preview",
            "Execute an ad-hoc SPARQL query against a ggen project's loaded graph and \
             report the TRUE row count before any truncation. A zero-row SELECT is not \
             an error -- it is reported loudly (ok:true, row_count:0) so a mandatory \
             triple pattern that matches nothing is never silently mistaken for success.",
            serde_json::to_value(schemars::schema_for!(tools::query_preview::QueryPreviewParams))
                .unwrap_or_default(),
            read_only("Preview a SPARQL query"),
        )];
        Self { tools: Arc::new(tools) }
    }

    /// Serve over stdio. Bare invocation (`ggen-mcp` with no args) is the
    /// expected entry point -- MCP clients spawn the binary and speak
    /// JSON-RPC over its stdio immediately, so nothing may write to stdout
    /// before the transport takes it over (see `bin/ggen-mcp.rs`'s
    /// stderr-only tracing setup).
    ///
    /// # Errors
    /// Returns an error if the stdio transport fails to initialize or serve.
    pub async fn start_stdio() -> anyhow::Result<()> {
        use rmcp::ServiceExt;
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let running = Self::new().serve((stdin, stdout)).await?;
        running.waiting().await?;
        Ok(())
    }
}

impl ServerHandler for GgenMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_protocol_version(ProtocolVersion::V_2024_11_05)
            .with_server_info(Implementation::new("ggen-mcp", env!("CARGO_PKG_VERSION")))
            .with_instructions(
                "ggen-mcp: introspection tools for authoring ggen projects (SPARQL query \
                 preview, frontmatter schema, project diagnostics). All tools are \
                 read-only except ggen_write_apply. Call ggen_query_preview before \
                 relying on a SPARQL query in a template -- it reports the true row \
                 count, including zero, before you commit to using it.",
            )
    }

    fn list_tools(
        &self, _request: Option<PaginatedRequestParams>, _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListToolsResult, McpError>> + Send + '_ {
        std::future::ready(Ok(ListToolsResult {
            tools: (*self.tools).clone(),
            next_cursor: None,
            meta: None,
        }))
    }

    fn call_tool(
        &self, CallToolRequestParams { name, arguments, .. }: CallToolRequestParams,
        _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<CallToolResult, McpError>> + Send + '_ {
        std::future::ready((|| {
            match name.as_ref() {
                "ggen_query_preview" => Ok(dispatch_query_preview(arguments)),
                other => Err(McpError::invalid_params(format!("unknown tool: {other}"), None)),
            }
        })())
    }
}

fn dispatch_query_preview(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> CallToolResult {
    let params: tools::query_preview::QueryPreviewParams = match arguments {
        Some(a) => match serde_json::from_value(serde_json::Value::Object(a)) {
            Ok(p) => p,
            Err(e) => {
                return crate::error::McpError::new(
                    crate::error::ErrorCategory::Internal,
                    format!("invalid params: {e}"),
                )
                .into()
            }
        },
        None => {
            return crate::error::McpError::new(
                crate::error::ErrorCategory::Internal,
                "missing arguments",
            )
            .into()
        }
    };
    match tools::query_preview::query_preview(&params) {
        Ok(result) => CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).unwrap_or_default(),
        )]),
        Err(e) => e.into(),
    }
}
