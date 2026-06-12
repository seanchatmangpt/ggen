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
        use ggen_core::codegen::pipeline::GenerationPipeline;
        use ggen_core::manifest::ManifestParser;
        use std::path::PathBuf;

        // Log the boundary crossing.
        tracing::info!(
            "OCEL: {}",
            serde_json::json!({
                "event": "a2a.mcp.tool.invoked",
                "objects": { "task": params.task_id, "tool": "ggen.construct" }
            })
        );

        // 1. Locate and load the manifest from CWD
        let base_path = std::env::current_dir().map_err(|e| {
            rmcp::model::ErrorData::internal_error(format!("Failed to get current directory: {}", e), None)
        })?;

        let manifest_path = base_path.join("ggen.toml");
        if !manifest_path.exists() {
            return Err(rmcp::model::ErrorData::internal_error(
                "ggen.toml not found in current directory. `ggen.construct` requires a ggen project context.",
                Some(serde_json::json!({ "path": base_path })),
            ));
        }

        let manifest = ManifestParser::parse_and_validate(&manifest_path).map_err(|e| {
            rmcp::model::ErrorData::internal_error(format!("Failed to load manifest: {}", e), None)
        })?;

        // 2. Initialize and run the GenerationPipeline
        let mut pipeline = GenerationPipeline::new(manifest, base_path);
        
        // Connect LLM service if available in global slot (e.g. from ggen-cli)
        if let Some(svc) = ggen_core::codegen::pipeline::get_llm_service() {
            pipeline.set_llm_service(Some(svc));
        }

        let state = pipeline.run().map_err(|e| {
            rmcp::model::ErrorData::internal_error(format!("Generation pipeline failed: {}", e), None)
        })?;

        // 3. Formulate success response with artifact details
        let mut text = format!("Successfully constructed artifact for task {}.\n", params.task_id);
        text.push_str(&format!("JTBD: {}\n", params.jtbd));
        text.push_str(&format!("Generated {} files:\n", state.generated_files.len()));
        
        for file in &state.generated_files {
            text.push_str(&format!("- {} ({} bytes, hash: {})\n", 
                file.path.display(), 
                file.size_bytes, 
                &file.content_hash[..8]));
        }

        let result_data = serde_json::json!({
            "status": "success",
            "task_id": params.task_id,
            "files_count": state.generated_files.len(),
            "duration_ms": state.started_at.elapsed().as_millis()
        });

        let mut meta = rmcp::model::Meta::default();
        meta.insert("ggen_result".to_string(), result_data);

        Ok(CallToolResult::success(vec![Content::text(text)]).with_meta(Some(meta)))
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
