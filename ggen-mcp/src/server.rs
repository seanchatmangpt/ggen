use rmcp::{
    ErrorData, ServerHandler,
    service::{RequestContext, RoleServer},
    model::{
        CallToolRequestParam, CallToolResult, Content,
        InitializeRequestParam, InitializeResult, Implementation,
        ListToolsResult, PaginatedRequestParam, ProtocolVersion,
        ServerCapabilities, Tool, ToolsCapability
    },
};
use serde_json::{Map, Value};
use std::{borrow::Cow, collections::HashMap, sync::Arc};

use crate::error::{GgenMcpError, Result};
use crate::schema::*;
use crate::tools::{graph, hook, market, project, template};
// Simplified agent integration - focusing on core functionality

#[derive(Debug, Clone)]
pub struct ToolDef {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

pub struct GgenMcpServer {
    tools: HashMap<String, ToolDef>,
}

impl GgenMcpServer {
    pub fn new() -> Self {
        let mut tools = HashMap::new();

        // Project tools
        tools.insert(
            "project_gen".to_string(),
            ToolDef {
                name: "project_gen".to_string(),
                description: "Generate files from a template with variables".to_string(),
                input_schema: project_gen_schema(),
            },
        );

        tools.insert(
            "project_plan".to_string(),
            ToolDef {
                name: "project_plan".to_string(),
                description: "Create an execution plan without applying changes".to_string(),
                input_schema: project_plan_schema(),
            },
        );

        tools.insert(
            "project_apply".to_string(),
            ToolDef {
                name: "project_apply".to_string(),
                description: "Apply a previously created execution plan".to_string(),
                input_schema: project_apply_schema(),
            },
        );

        tools.insert(
            "project_diff".to_string(),
            ToolDef {
                name: "project_diff".to_string(),
                description: "Show differences between template output and existing files"
                    .to_string(),
                input_schema: project_diff_schema(),
            },
        );

        // Market tools
        tools.insert(
            "market_list".to_string(),
            ToolDef {
                name: "market_list".to_string(),
                description: "List available templates from the marketplace".to_string(),
                input_schema: market_list_schema(),
            },
        );

        tools.insert(
            "market_search".to_string(),
            ToolDef {
                name: "market_search".to_string(),
                description: "Search marketplace templates by query".to_string(),
                input_schema: market_search_schema(),
            },
        );

        tools.insert(
            "market_install".to_string(),
            ToolDef {
                name: "market_install".to_string(),
                description: "Install a template from the marketplace".to_string(),
                input_schema: market_install_schema(),
            },
        );

        tools.insert(
            "market_recommend".to_string(),
            ToolDef {
                name: "market_recommend".to_string(),
                description: "Get personalized package recommendations based on installed packages and categories".to_string(),
                input_schema: market_recommend_schema(),
            },
        );

        tools.insert(
            "market_info".to_string(),
            ToolDef {
                name: "market_info".to_string(),
                description: "Get detailed information about a specific package with examples and health metrics".to_string(),
                input_schema: market_info_schema(),
            },
        );

        tools.insert(
            "market_offline_search".to_string(),
            ToolDef {
                name: "market_offline_search".to_string(),
                description: "Search marketplace packages using cached offline data".to_string(),
                input_schema: market_offline_search_schema(),
            },
        );

        tools.insert(
            "market_cache_status".to_string(),
            ToolDef {
                name: "market_cache_status".to_string(),
                description: "Get status and statistics about the marketplace cache".to_string(),
                input_schema: market_cache_status_schema(),
            },
        );

        tools.insert(
            "market_sync".to_string(),
            ToolDef {
                name: "market_sync".to_string(),
                description: "Synchronize local cache with remote marketplace".to_string(),
                input_schema: market_sync_schema(),
            },
        );

        // Graph tools
        tools.insert(
            "graph_query".to_string(),
            ToolDef {
                name: "graph_query".to_string(),
                description: "Execute SPARQL query against RDF graph store".to_string(),
                input_schema: graph_query_schema(),
            },
        );

        tools.insert(
            "graph_load".to_string(),
            ToolDef {
                name: "graph_load".to_string(),
                description: "Load RDF data from file into graph store".to_string(),
                input_schema: graph_load_schema(),
            },
        );

        tools.insert(
            "graph_export".to_string(),
            ToolDef {
                name: "graph_export".to_string(),
                description: "Export RDF graph to file".to_string(),
                input_schema: graph_export_schema(),
            },
        );

        // Template tools
        tools.insert(
            "template_create".to_string(),
            ToolDef {
                name: "template_create".to_string(),
                description: "Create a new template".to_string(),
                input_schema: template_create_schema(),
            },
        );

        tools.insert(
            "template_validate".to_string(),
            ToolDef {
                name: "template_validate".to_string(),
                description: "Validate template syntax and structure".to_string(),
                input_schema: template_validate_schema(),
            },
        );

        // Hook tools
        tools.insert(
            "hook_register".to_string(),
            ToolDef {
                name: "hook_register".to_string(),
                description: "Register a lifecycle hook".to_string(),
                input_schema: hook_register_schema(),
            },
        );

        Self {
            tools,
        }
    }

    async fn execute_tool(&self, name: &str, params: Value) -> Result<Value> {
        match name {
            // Project tools
            "project_gen" => project::gen(params).await,
            "project_plan" => project::plan(params).await,
            "project_apply" => project::apply(params).await,
            "project_diff" => project::diff(params).await,

            // Market tools
            "market_list" => market::list(params).await,
            "market_search" => market::search(params).await,
            "market_install" => market::install(params).await,
            "market_recommend" => market::recommend(params).await,
            "market_info" => market::info(params).await,
            "market_offline_search" => market::offline_search(params).await,
            "market_cache_status" => market::cache_status(params).await,
            "market_sync" => market::sync(params).await,

            // Graph tools
            "graph_query" => graph::query(params).await,
            "graph_load" => graph::load(params).await,
            "graph_export" => graph::export(params).await,

            // Template tools
            "template_create" => template::create(params).await,
            "template_validate" => template::validate(params).await,

            // Hook tools
            "hook_register" => hook::register(params).await,

            _ => Err(GgenMcpError::InvalidParameter(format!(
                "Unknown tool: {}",
                name
            ))),
        }
    }
}

impl ServerHandler for GgenMcpServer {
    async fn initialize(
        &self,
        _params: InitializeRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<InitializeResult, ErrorData> {
        Ok(InitializeResult {
            protocol_version: ProtocolVersion::default(),
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability::default()),
                ..Default::default()
            },
            server_info: Implementation {
                name: "ggen-mcp".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
                title: None,
                website_url: None,
                icons: None,
            },
            instructions: None,
        })
    }

    async fn list_tools(
        &self,
        _pagination: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<ListToolsResult, ErrorData> {
        let tools = self
            .tools
            .values()
            .map(|tool_def| {
                // Convert Value to Map<String, Value>
                let schema_map = match &tool_def.input_schema {
                    Value::Object(map) => map.clone(),
                    _ => Map::new(),
                };

                Tool {
                    name: Cow::Owned(tool_def.name.clone()),
                    description: Some(Cow::Owned(tool_def.description.clone())),
                    input_schema: Arc::new(schema_map),
                    title: None,
                    output_schema: None,
                    annotations: None,
                    icons: None,
                }
            })
            .collect();

        Ok(ListToolsResult {
            tools,
            next_cursor: None,
        })
    }

    async fn call_tool(
        &self,
        params: CallToolRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<CallToolResult, ErrorData> {
        let args = Value::Object(params.arguments.unwrap_or_default());
        let result = self
            .execute_tool(&params.name, args)
            .await
            .map_err(|e| ErrorData::internal_error(e.to_string(), None))?;

        Ok(CallToolResult {
            content: vec![Content::text(
                serde_json::to_string_pretty(&result)
                    .map_err(|e| ErrorData::internal_error(e.to_string(), None))?,
            )],
            is_error: None,
            meta: None,
            structured_content: None,
        })
    }
}

impl Default for GgenMcpServer {
    fn default() -> Self {
        Self::new()
    }
}
