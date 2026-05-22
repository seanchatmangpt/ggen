use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{CallToolResult, Content, Implementation, InitializeResult, ServerCapabilities};
use rmcp::{tool, tool_handler, tool_router, ServerHandler, ServiceExt};

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct HelloParams {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct GgenMcpServer {
    tool_router: rmcp::handler::server::router::tool::ToolRouter<Self>,
}

#[tool_router]
impl GgenMcpServer {
    #[tool(description = "Say hello")]
    async fn hello(
        &self, Parameters(params): Parameters<HelloParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        Ok(CallToolResult::success(vec![Content::text(format!(
            "Hello, {}!",
            params.name
        ))]))
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
