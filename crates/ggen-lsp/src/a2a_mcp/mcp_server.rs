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
        // Log the boundary crossing.
        tracing::info!(
            "OCEL: {}",
            serde_json::json!({
                "event": "a2a.mcp.tool.invoked",
                "objects": { "task": params.task_id, "tool": "ggen.construct" }
            })
        );

        // 1. Locate the project root (CWD) and confirm it is a ggen project.
        let base_path = std::env::current_dir().map_err(|e| {
            rmcp::model::ErrorData::internal_error(
                format!("Failed to get current directory: {}", e),
                None,
            )
        })?;

        let manifest_path = base_path.join("ggen.toml");
        if !manifest_path.exists() {
            return Err(rmcp::model::ErrorData::internal_error(
                "ggen.toml not found in current directory. `ggen.construct` requires a ggen project context.",
                Some(serde_json::json!({ "path": base_path })),
            ));
        }

        // 2. Run the ggen-engine sync pipeline (T049: replaces the retired
        // ggen-core ManifestParser::parse_and_validate + GenerationPipeline
        // chain). `ggen_engine::sync::sync` loads `ggen.toml` and the ontology
        // itself and handles BOTH the frontmatter-per-template schema and the
        // `[[generation.rules]]` declarative schema automatically (T070), so
        // the separate manifest-parse step is subsumed, not preserved.
        let report = ggen_engine::sync::sync(&base_path, ggen_engine::sync::SyncOptions::default())
            .map_err(|e| {
                rmcp::model::ErrorData::internal_error(
                    format!("Generation pipeline failed: {}", e),
                    None,
                )
            })?;

        // NOTE: no LLM-service hook here. The old handler connected an
        // optional `Box<dyn LlmService>` via `ggen_core::codegen::pipeline::
        // get_llm_service()`, but that free-function's only production setter
        // (the same-named `set_llm_service`) had zero callers anywhere in the
        // workspace outside ggen-core's own unit tests (confirmed via
        // workspace-wide grep) -- it died with ggen-cli's sync module removal
        // earlier in this migration. `ggen_engine::sync::sync` has no
        // equivalent LLM-injection point; there is nothing to preserve.

        // 3. Formulate success response with artifact details.
        let mut text = format!(
            "Successfully constructed artifact for task {}.\n",
            params.task_id
        );
        text.push_str(&format!("JTBD: {}\n", params.jtbd));
        text.push_str(&format!("Generated {} files:\n", report.written.len()));

        for path in &report.written {
            text.push_str(&format!("- {}\n", path.display()));
        }

        // Field-by-field disposition of the OLD `ggen_result` meta payload
        // against the new `SyncReport` (T049 combinatorial-maximalism pass):
        //   - "status": "success"        -> kept literally; reaching this line
        //                                    without an early return means sync succeeded.
        //   - "task_id"                  -> kept unchanged (request echo, not
        //                                    engine-derived).
        //   - "files_count"              -> mapped from `report.written.len()`
        //                                    (was `state.generated_files.len()`).
        //   - "duration_ms"              -> DROPPED. `SyncReport` carries no
        //                                    timing field and `sync()` does not
        //                                    expose a start instant; ggen-engine
        //                                    treats wall-clock timing as
        //                                    non-deterministic receipt content
        //                                    (see sync.rs's `ts_ns` note), so
        //                                    there is no equivalent to map to.
        //   - (new) "graph_hash_hex"     -> ADDED from `report.graph_hash_hex`,
        //                                    the closest ggen-engine equivalent
        //                                    to a content-identity hash for the
        //                                    run (the old handler had no
        //                                    per-file content hash at the
        //                                    top level to preserve either).
        let result_data = serde_json::json!({
            "status": "success",
            "task_id": params.task_id,
            "files_count": report.written.len(),
            "graph_hash_hex": report.graph_hash_hex,
        });

        let mut meta = rmcp::model::Meta::default();
        meta.insert("ggen_result".to_string(), result_data);

        Ok(CallToolResult::success(vec![Content::text(text)]).with_meta(Some(meta)))
    }

    // ── Pack + marketplace tools ────────────────────────────────────────────
    //
    // Each tool is a thin wrapper over a single pure result function in
    // `mcp_packs`; the same functions back the A2A `PackToolsAdapter`, so the
    // two transports cannot drift. Outputs are the structured, evidence-bearing
    // `ggen_marketplace::agent` contract; failures are typed `AgentError`s
    // carrying a `{kind, detail}` body the agent can branch on.

    #[tool(
        name = "ggen.packs.capabilities",
        description = "Describe the pack operations and capability surfaces available to agents."
    )]
    async fn packs_capabilities(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackCapabilitiesParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.capabilities", "-");
        let value = crate::a2a_mcp::mcp_packs::capabilities_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.search",
        description = "Relevance-rank packs in the local registry by a text query (name > id > description)."
    )]
    async fn packs_search(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackSearchParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.search", &params.query);
        let value = crate::a2a_mcp::mcp_packs::search_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.list",
        description = "List all packs in the local registry, optionally filtered by category."
    )]
    async fn packs_list(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackListParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked(
            "ggen.packs.list",
            params.category.as_deref().unwrap_or("-"),
        );
        let value = crate::a2a_mcp::mcp_packs::list_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.show",
        description = "Full detail for one pack: metadata, packages, templates, dependencies, and validation."
    )]
    async fn packs_show(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackShowParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.show", &params.pack_id);
        let value = crate::a2a_mcp::mcp_packs::show_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.resolve",
        description = "Resolve a capability surface (e.g. mcp, web) to concrete pack IDs, splitting resolved vs missing."
    )]
    async fn packs_resolve(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackResolveParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.resolve", &params.surface);
        let value = crate::a2a_mcp::mcp_packs::resolve_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.compatibility",
        description = "Check whether a set of packs can be composed without conflicts (overlapping packages or unloadable packs)."
    )]
    async fn packs_compatibility(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackCompatibilityParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.compatibility", "-");
        let value = crate::a2a_mcp::mcp_packs::compatibility_result(params)
            .await
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.status",
        description = "Report installed packs from the project lockfile (.ggen/packs.lock)."
    )]
    async fn packs_status(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackStatusParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked(
            "ggen.packs.status",
            params.root.as_deref().unwrap_or("."),
        );
        let value = crate::a2a_mcp::mcp_packs::status_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.verify",
        description = "Verify a provenance receipt against its signing key. Fail-closed: missing key or bad signature yields is_valid=false."
    )]
    async fn packs_verify(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackVerifyParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.verify", &params.receipt_path);
        let value = crate::a2a_mcp::mcp_packs::verify_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.install",
        description = "Install a pack: write the lockfile with a non-empty digest and emit a signed provenance receipt."
    )]
    async fn packs_install(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackInstallParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.install", &params.pack_id);
        let value = crate::a2a_mcp::mcp_packs::install_result(params)
            .await
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
    }

    #[tool(
        name = "ggen.packs.remove",
        description = "Remove a pack from the project lockfile. Fail-closed: an absent pack or missing lockfile errors."
    )]
    async fn packs_remove(
        &self, Parameters(params): Parameters<crate::a2a_mcp::mcp_packs::PackRemoveParams>,
    ) -> Result<CallToolResult, rmcp::model::ErrorData> {
        crate::a2a_mcp::mcp_packs::ocel_invoked("ggen.packs.remove", &params.pack_id);
        let value = crate::a2a_mcp::mcp_packs::remove_result(params)
            .map_err(crate::a2a_mcp::mcp_packs::mcp_err)?;
        Ok(crate::a2a_mcp::mcp_packs::mcp_ok(value))
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
