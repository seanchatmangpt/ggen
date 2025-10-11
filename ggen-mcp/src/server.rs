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
use crate::swarm;
use crate::tools::{graph, hook, market, project, template, ai};
// Ultrathink swarm integration for autonomous operations

#[derive(Debug, Clone)]
pub struct ToolDef {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

pub struct GgenMcpServer {
    tools: HashMap<String, ToolDef>,
    ultrathink_swarm: Option<Arc<swarm::ultrathink::UltrathinkSwarm>>,
}

impl GgenMcpServer {
    pub async fn new() -> Result<Self> {
        let mut tools = HashMap::new();

        // Initialize ultrathink swarm for WIP connectivity
        let ultrathink_config = swarm::ultrathink::UltrathinkConfig::default();
        let ultrathink_swarm = swarm::ultrathink::UltrathinkSwarm::new(ultrathink_config).await?;
        let ultrathink_swarm = Arc::new(ultrathink_swarm);

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

        // AI tools - Autonomous code generation
        tools.insert(
            "ai_generate_template".to_string(),
            ToolDef {
                name: "ai_generate_template".to_string(),
                description: "Generate a template from natural language description using AI".to_string(),
                input_schema: ai_generate_template_schema(),
            },
        );

        tools.insert(
            "ai_generate_sparql".to_string(),
            ToolDef {
                name: "ai_generate_sparql".to_string(),
                description: "Generate SPARQL query from natural language intent using AI".to_string(),
                input_schema: ai_generate_sparql_schema(),
            },
        );

        tools.insert(
            "ai_generate_ontology".to_string(),
            ToolDef {
                name: "ai_generate_ontology".to_string(),
                description: "Generate RDF ontology from domain description using AI".to_string(),
                input_schema: ai_generate_ontology_schema(),
            },
        );

        tools.insert(
            "ai_generate_project".to_string(),
            ToolDef {
                name: "ai_generate_project".to_string(),
                description: "Generate complete project structure from description using AI".to_string(),
                input_schema: ai_generate_project_schema(),
            },
        );

        tools.insert(
            "ai_extend_graph".to_string(),
            ToolDef {
                name: "ai_extend_graph".to_string(),
                description: "Extend existing RDF graph with new knowledge using AI".to_string(),
                input_schema: ai_extend_graph_schema(),
            },
        );

        tools.insert(
            "ai_validate_and_improve".to_string(),
            ToolDef {
                name: "ai_validate_and_improve".to_string(),
                description: "Validate and improve existing code or templates using AI".to_string(),
                input_schema: ai_validate_and_improve_schema(),
            },
        );

        tools.insert(
            "ai_list_providers".to_string(),
            ToolDef {
                name: "ai_list_providers".to_string(),
                description: "List available AI providers and their capabilities".to_string(),
                input_schema: ai_list_providers_schema(),
            },
        );

        Ok(Self {
            tools,
            ultrathink_swarm: Some(ultrathink_swarm),
        })
    }

    /// Get the number of registered tools
    pub fn tool_count(&self) -> usize {
        self.tools.len()
    }

    /// Check if a tool is registered
    pub fn has_tool(&self, name: &str) -> bool {
        self.tools.contains_key(name)
    }

    /// Get all registered tool names
    pub fn tool_names(&self) -> Vec<String> {
        self.tools.keys().cloned().collect()
    }

    pub async fn execute_tool(&self, name: &str, params: Value) -> Result<Value> {
        tracing::debug!("Routing tool '{}' to handler", name);

        let result = match name {
            // Project tools
            "project_gen" => {
                tracing::trace!("Executing project_gen");
                project::gen(params).await
            }
            "project_plan" => {
                tracing::trace!("Executing project_plan");
                project::plan(params).await
            }
            "project_apply" => {
                tracing::trace!("Executing project_apply");
                project::apply(params).await
            }
            "project_diff" => {
                tracing::trace!("Executing project_diff");
                project::diff(params).await
            }

            // Market tools
            "market_list" => {
                tracing::trace!("Executing market_list");
                market::list(params).await
            }
            "market_search" => {
                tracing::trace!("Executing market_search");
                market::search(params).await
            }
            "market_install" => {
                tracing::trace!("Executing market_install");
                market::install(params).await
            }
            "market_recommend" => {
                tracing::trace!("Executing market_recommend");
                market::recommend(params).await
            }
            "market_info" => {
                tracing::trace!("Executing market_info");
                market::info(params).await
            }
            "market_offline_search" => {
                tracing::trace!("Executing market_offline_search");
                market::offline_search(params).await
            }
            "market_cache_status" => {
                tracing::trace!("Executing market_cache_status");
                market::cache_status(params).await
            }
            "market_sync" => {
                tracing::trace!("Executing market_sync");
                market::sync(params).await
            }

            // Graph tools
            "graph_query" => {
                tracing::trace!("Executing graph_query");
                graph::query(params).await
            }
            "graph_load" => {
                tracing::trace!("Executing graph_load");
                graph::load(params).await
            }
            "graph_export" => {
                tracing::trace!("Executing graph_export");
                graph::export(params).await
            }

            // Template tools
            "template_create" => {
                tracing::trace!("Executing template_create");
                template::create(params).await
            }
            "template_validate" => {
                tracing::trace!("Executing template_validate");
                template::validate(params).await
            }

            // Hook tools
            "hook_register" => {
                tracing::trace!("Executing hook_register");
                hook::register(params).await
            }

            // AI tools - Autonomous code generation
            "ai_generate_template" => {
                tracing::trace!("Executing ai_generate_template");
                ai::generate_template(params).await
            }
            "ai_generate_sparql" => {
                tracing::trace!("Executing ai_generate_sparql");
                ai::generate_sparql(params).await
            }
            "ai_generate_ontology" => {
                tracing::trace!("Executing ai_generate_ontology");
                ai::generate_ontology(params).await
            }
            "ai_generate_project" => {
                tracing::trace!("Executing ai_generate_project");
                ai::generate_project(params).await
            }
            "ai_extend_graph" => {
                tracing::trace!("Executing ai_extend_graph");
                ai::extend_graph(params).await
            }
            "ai_validate_and_improve" => {
                tracing::trace!("Executing ai_validate_and_improve");
                ai::validate_and_improve(params).await
            }
            "ai_list_providers" => {
                tracing::trace!("Executing ai_list_providers");
                ai::list_providers(params).await
            }

            _ => {
                tracing::warn!("Unknown tool requested: {}", name);
                Err(GgenMcpError::InvalidParameter(format!(
                    "Unknown tool: {}",
                    name
                )))
            }
        };

        match &result {
            Ok(_) => tracing::debug!("Tool '{}' handler completed successfully", name),
            Err(e) => tracing::error!("Tool '{}' handler failed: {}", name, e),
        }

        result
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
        let tool_name = params.name.clone();
        tracing::info!("MCP call_tool invoked: {}", tool_name);
        tracing::debug!("Tool arguments: {:?}", params.arguments);

        let args = Value::Object(params.arguments.unwrap_or_default());

        // Execute tool with comprehensive error handling and logging
        let result = match self.execute_tool(&tool_name, args.clone()).await {
            Ok(value) => {
                tracing::info!("Tool '{}' executed successfully", tool_name);
                tracing::debug!("Tool '{}' result: {:?}", tool_name, value);
                value
            }
            Err(e) => {
                tracing::error!(
                    "Tool '{}' execution failed: {} (args: {:?})",
                    tool_name,
                    e,
                    args
                );
                // Return structured error response following MCP protocol
                return Err(ErrorData::invalid_params(
                    format!("Tool execution failed: {}", e),
                    None
                ));
            }
        };

        // Format response according to MCP protocol
        let formatted_response = serde_json::to_string_pretty(&result)
            .map_err(|e| {
                tracing::error!(
                    "Failed to serialize tool '{}' response: {}",
                    tool_name,
                    e
                );
                ErrorData::internal_error(
                    format!("Response serialization failed: {}", e),
                    None
                )
            })?;

        tracing::debug!("Tool '{}' response formatted: {} bytes", tool_name, formatted_response.len());

        Ok(CallToolResult {
            content: vec![Content::text(formatted_response)],
            is_error: Some(false),
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
