use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{CallToolResult, Content, Implementation, InitializeResult, ServerCapabilities};
use rmcp::{tool, tool_handler, tool_router, ServerHandler, ServiceExt};

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct HelloParams {
    pub name: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ConstructParams {
    pub task_id: String,
    pub jtbd: String,
    pub avatar: String,
}

#[derive(Debug, Clone)]
pub struct GgenMcpServer {
    tool_router: rmcp::handler::server::router::tool::ToolRouter<Self>,
}

#[tool_router]
impl GgenMcpServer {
    /// Demo tool: returns a greeting. This is an honest demo — a greeting tool
    /// that returns a greeting performs exactly the work it advertises, so it is
    /// NOT an Oracle Gap. It exists to exercise the tool-router wiring.
    #[tool(description = "Say hello")]
    async fn hello(
        &self, Parameters(params): Parameters<HelloParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        Ok(CallToolResult::success(vec![Content::text(format!(
            "Hello, {}!",
            params.name
        ))]))
    }

    #[tool(
        name = "ggen.construct",
        description = "Construct a target artifact using ggen A2A pipeline"
    )]
    async fn construct(
        &self, Parameters(params): Parameters<ConstructParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        // Log the boundary crossing. This OCEL event is real evidence that the
        // tool was invoked, so it must be preserved even though construction is
        // not yet implemented.
        tracing::info!(
            "OCEL: {}",
            serde_json::json!({
                "event": "a2a.mcp.tool.invoked",
                "objects": { "task": params.task_id, "tool": "ggen.construct" }
            })
        );

        // WHY FAIL LOUD: This handler does NOT yet invoke the real μ₁–μ₅
        // pipeline. Previously it returned a hardcoded {"status":"success",
        // "message":"Artifact constructed successfully."} without constructing
        // anything — an Oracle Gap where advertised != delivered. Reporting
        // success for work that did not happen is worse than failing, because
        // callers (and provenance/receipts) would record a fabricated outcome.
        //
        // Until the pipeline is wired, the only honest response is an explicit
        // failure that directs the caller to the real construction path.
        //
        // TODO(pipeline): Wire this to `ggen_core::Pipeline` (the μ₁–μ₅ stages
        // exposed via `ggen sync`) so that construction actually loads the
        // ontology, generates, validates, and emits artifacts — then return
        // success only after artifacts are emitted and a receipt is produced.
        let diagnostic = serde_json::json!({
            "status": "unimplemented",
            "task_id": params.task_id,
            "jtbd": params.jtbd,
            "avatar": params.avatar,
            "tool": "ggen.construct",
            "hint": "Run `ggen sync` for the real μ₁–μ₅ construction pipeline."
        });

        Err(rmcp::model::ErrorData::internal_error(
            "ggen.construct is not implemented: this tool does not run the \
             μ₁–μ₅ pipeline and therefore cannot construct an artifact. Use \
             `ggen sync` for the real construction path. No artifact was \
             produced.",
            Some(diagnostic),
        ))
    }
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for GgenMcpServer {
    fn get_info(&self) -> InitializeResult {
        let capabilities = ServerCapabilities::builder().enable_tools().build();
        InitializeResult::new(capabilities)
            .with_server_info(Implementation::new("ggen-mcp", "0.1.0"))
    }
}

impl Default for GgenMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl GgenMcpServer {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

    pub async fn start_stdio() -> anyhow::Result<()> {
        let server = Self::new();
        let transport = rmcp::transport::stdio();
        if let Ok(svc) = server.serve(transport).await {
            let _ = svc.waiting().await;
        }
        Ok(())
    }
}
