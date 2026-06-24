use std::{path::PathBuf, sync::Arc};
use rmcp::{
    tool, tool_handler, tool_router,
    handler::server::wrapper::Parameters,
    model::*,
    ServerHandler,
    ErrorData as McpError,
};
use schemars::JsonSchema;
use serde::Deserialize;
use tracing::info;
use crate::{
    dispatch::dispatch_bundle,
    state::DaemonState,
};

#[derive(Clone)]
pub struct GgenDaemonMcp {
    pub state: Arc<DaemonState>,
    pub working_dir: PathBuf,
    tool_router: rmcp::handler::server::router::tool::ToolRouter<Self>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct TriggerParams {
    /// Path to the spec manifest to sync
    pub manifest: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct LogParams {
    /// Maximum number of log entries to return (default 10, max 100)
    pub limit: Option<i64>,
}

#[tool_router]
impl GgenDaemonMcp {
    #[tool(description = "List scheduled jobs and recent run history from the daemon")]
    async fn daemon_status(
        &self,
    ) -> Result<CallToolResult, McpError> {
        let runs = self.state.recent_runs(20).await.map_err(|e| McpError::internal_error(
            e.to_string(),
            None,
        ))?;
        let body = serde_json::to_string_pretty(&runs).unwrap_or_else(|_| "[]".to_owned());
        Ok(CallToolResult::success(vec![Content::text(body)]))
    }

    #[tool(description = "Manually trigger a ggen sync for a spec manifest path")]
    async fn daemon_trigger(
        &self,
        Parameters(TriggerParams { manifest }): Parameters<TriggerParams>,
    ) -> Result<CallToolResult, McpError> {
        info!("MCP trigger: {}", manifest);
        let result = dispatch_bundle("mcp:manual", &manifest, &self.working_dir, &self.state)
            .await
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;
        let body = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "{}".to_owned());
        Ok(CallToolResult::success(vec![Content::text(body)]))
    }

    #[tool(description = "Return recent job run events from the daemon SQLite log")]
    async fn daemon_log(
        &self,
        Parameters(LogParams { limit }): Parameters<LogParams>,
    ) -> Result<CallToolResult, McpError> {
        let n = limit.unwrap_or(10).min(100);
        let runs = self.state.recent_runs(n).await.map_err(|e| McpError::internal_error(
            e.to_string(),
            None,
        ))?;
        let body = serde_json::to_string_pretty(&runs).unwrap_or_else(|_| "[]".to_owned());
        Ok(CallToolResult::success(vec![Content::text(body)]))
    }
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for GgenDaemonMcp {
    fn get_info(&self) -> InitializeResult {
        InitializeResult::new(ServerCapabilities::builder().enable_tools().build())
            .with_server_info(Implementation::new("ggen-daemon", env!("CARGO_PKG_VERSION")))
    }
}

impl GgenDaemonMcp {
    pub fn new(state: Arc<DaemonState>, working_dir: PathBuf) -> Self {
        Self {
            state,
            working_dir,
            tool_router: Self::tool_router(),
        }
    }
}
