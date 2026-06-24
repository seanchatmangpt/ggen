use std::{collections::HashSet, path::PathBuf, sync::Arc};
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
    catalog::{append_entry, load_catalog, RepoCatalogEntry},
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

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CatalogDiscoverParams {
    /// GitHub organization or user login to discover repos from
    pub owner: String,
    /// Optional GitHub personal access token for higher rate limits
    pub token: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CatalogSyncParams {
    /// GitHub organization or user login to sync repos from
    pub owner: String,
    /// Optional GitHub personal access token for higher rate limits
    pub token: Option<String>,
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
    async fn trigger_dispatch(
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

    #[tool(description = "Discover public GitHub repos for an owner and return them as a JSON list (read-only, does not modify catalog)")]
    async fn catalog_discover(
        &self,
        Parameters(CatalogDiscoverParams { owner, token }): Parameters<CatalogDiscoverParams>,
    ) -> Result<CallToolResult, McpError> {
        let owner_c = owner.clone();
        let token_c = token.clone();
        let repos = tokio::task::spawn_blocking(move || {
            crate::catalog_sync::discover_repos(&owner_c, token_c.as_deref())
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let repo_list: Vec<serde_json::Value> = repos.iter().map(|r| serde_json::json!({
            "name": r.name,
            "url": r.html_url,
            "language": r.language,
            "description": r.description,
        })).collect();

        let body = serde_json::to_string_pretty(&serde_json::json!({
            "owner": owner,
            "discovered": repos.len(),
            "repos": repo_list,
        })).unwrap_or_default();

        Ok(CallToolResult::success(vec![Content::text(body)]))
    }

    #[tool(description = "Sync GitHub repos for an owner into repos-catalog.ttl, appending repos not already present")]
    async fn catalog_sync(
        &self,
        Parameters(CatalogSyncParams { owner, token }): Parameters<CatalogSyncParams>,
    ) -> Result<CallToolResult, McpError> {
        let catalog_path = self.working_dir.join(".specify/specs/repos-catalog.ttl");
        let owner_c = owner.clone();
        let token_c = token.clone();

        let repos = tokio::task::spawn_blocking(move || {
            crate::catalog_sync::discover_repos(&owner_c, token_c.as_deref())
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let existing_names: HashSet<String> = if catalog_path.exists() {
            load_catalog(&catalog_path)
                .unwrap_or_default()
                .into_iter()
                .map(|e| e.name)
                .collect()
        } else {
            HashSet::new()
        };

        let mut added = 0usize;
        let mut skipped = 0usize;

        for repo in &repos {
            if existing_names.contains(&repo.name) {
                skipped += 1;
                continue;
            }
            let entry = RepoCatalogEntry {
                name: repo.name.clone(),
                github_url: repo.html_url.clone(),
                short_desc: repo.description.clone(),
                primary_language: repo.language.clone(),
            };
            append_entry(&catalog_path, &entry)
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;
            added += 1;
        }

        info!("catalog_sync: owner={} discovered={} added={} skipped={}", owner, repos.len(), added, skipped);

        let body = serde_json::to_string_pretty(&serde_json::json!({
            "owner": owner,
            "discovered": repos.len(),
            "added": added,
            "skipped": skipped,
            "catalog_path": catalog_path.display().to_string(),
        })).unwrap_or_default();

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
