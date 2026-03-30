//! Integration test: render ALL MCP + A2A templates with mock data
//!
//! Validates that every template in /templates/ and /crates/ggen-core/templates/
//! produces non-empty output with realistic mock context data.
//!
//! Output is written to /tmp/ggen-mcp-a2a-test/ for manual inspection and
//! downstream compilation checks (cargo check, go vet, elixirc, javac, etc.).

use ggen_core::register::register_all;
use tera::{Context, Tera};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn create_test_tera() -> Tera {
    let mut tera = Tera::default();
    register_all(&mut tera);
    tera
}

/// Write rendered output to /tmp/ggen-mcp-a2a-test/{filename}
fn write_output(filename: &str, content: &str) {
    let dir = "/tmp/ggen-mcp-a2a-test";
    let _ = std::fs::create_dir_all(dir);
    let path = std::path::Path::new(dir).join(filename);
    std::fs::write(&path, content).unwrap_or_else(|e| {
        eprintln!("WARN: failed to write {}: {}", path.display(), e);
    });
}

/// Render a template string with the given context, return rendered output.
fn render_template_str(tera: &mut Tera, template: &str, ctx: &Context) -> Result<String, String> {
    tera.render_str(template, ctx).map_err(|e| {
        // Show full Tera error chain with source
        let mut msg = format!("{e}");
        // Walk the error chain for more details
        let mut source = std::error::Error::source(&e);
        while let Some(cause) = source {
            msg = format!("{msg}\n  Caused by: {cause}");
            source = std::error::Error::source(cause);
        }
        msg
    })
}

// ---------------------------------------------------------------------------
// Mock data builders
// ---------------------------------------------------------------------------

/// Standard MCP mock context with 2 tools, each having 2 parameters.
fn mcp_context() -> Context {
    let mut ctx = Context::new();
    ctx.insert("server_name", "order_processor");
    ctx.insert("server_version", "1.2.0");
    ctx.insert(
        "server_description",
        "Processes customer orders via the Model Context Protocol",
    );
    ctx.insert("transport_type", "stdio");

    let tools = serde_json::json!([
        {
            "name": "create_order",
            "description": "Create a new customer order",
            "parameters": [
                {
                    "name": "customer_id",
                    "description": "Unique customer identifier",
                    "rust_type": "String",
                    "go_type": "string",
                    "java_type": "String",
                    "ts_type": "string",
                    "elixir_type": "String.t()",
                    "json_type": "string",
                    "json_schema_type": "string",
                    "go_json_type": "string",
                    "java_json_type": "String",
                    "is_required": "true",
                    "default_value": "",
                    "enum_values": "",
                    "zod_type": "z.string",
                    "json_schema": ""
                },
                {
                    "name": "amount",
                    "description": "Order total in USD",
                    "rust_type": "f64",
                    "go_type": "float64",
                    "java_type": "double",
                    "ts_type": "number",
                    "elixir_type": "float()",
                    "json_type": "number",
                    "json_schema_type": "number",
                    "go_json_type": "number",
                    "java_json_type": "Double",
                    "is_required": "true",
                    "default_value": "",
                    "enum_values": "",
                    "zod_type": "z.number",
                    "json_schema": ""
                }
            ]
        },
        {
            "name": "get_order_status",
            "description": "Retrieve the status of an existing order",
            "parameters": [
                {
                    "name": "order_id",
                    "description": "Unique order identifier",
                    "rust_type": "String",
                    "go_type": "string",
                    "java_type": "String",
                    "ts_type": "string",
                    "elixir_type": "String.t()",
                    "json_type": "string",
                    "json_schema_type": "string",
                    "go_json_type": "string",
                    "java_json_type": "String",
                    "is_required": "true",
                    "default_value": "",
                    "enum_values": "",
                    "zod_type": "z.string",
                    "json_schema": ""
                },
                {
                    "name": "include_details",
                    "description": "Whether to include line items",
                    "rust_type": "bool",
                    "go_type": "bool",
                    "java_type": "Boolean",
                    "ts_type": "boolean",
                    "elixir_type": "boolean()",
                    "json_type": "boolean",
                    "json_schema_type": "boolean",
                    "go_json_type": "boolean",
                    "java_json_type": "Boolean",
                    "is_required": "false",
                    "default_value": "false",
                    "enum_values": "",
                    "zod_type": "z.boolean",
                    "json_schema": ""
                }
            ]
        }
    ]);
    ctx.insert("tools", &tools);
    ctx
}

/// Standard A2A mock context with 2 skills.
fn a2a_context() -> Context {
    let mut ctx = Context::new();
    ctx.insert("agent_name", "inventory_agent");
    ctx.insert("agent_version", "2.0.1");
    ctx.insert(
        "agent_description",
        "Manages warehouse inventory and stock levels across locations",
    );
    ctx.insert("agent_url", "http://localhost:8090");
    ctx.insert("provider_name", "ChatmanGPT");
    ctx.insert("provider_url", "https://chatmangpt.com");
    ctx.insert("agent_port", "8090");

    let skills = serde_json::json!([
        {
            "name": "check_stock",
            "description": "Check current stock level for a product SKU",
            "id": "check_stock",
            "tags": "[]string{\"inventory\", \"read\"}",
            "streaming": "false",
            "timeout_ms": "5000",
            "retry_policy": "exponential",
            "input_schema": "map[string]interface{}{}",
            "output_schema": "map[string]interface{}{}",
            "input_params": [
                {
                    "name": "sku",
                    "description": "Product SKU identifier",
                    "rust_type": "String",
                    "go_type": "string",
                    "java_type": "String",
                    "ts_type": "string",
                    "is_required": "true"
                },
                {
                    "name": "location",
                    "description": "Warehouse location code",
                    "rust_type": "String",
                    "go_type": "string",
                    "java_type": "String",
                    "ts_type": "string",
                    "is_required": "false"
                }
            ],
            "output_params": [
                {
                    "name": "quantity",
                    "description": "Current stock quantity",
                    "rust_type": "i64",
                    "go_type": "int64",
                    "java_type": "Long",
                    "ts_type": "number"
                }
            ]
        },
        {
            "name": "update_stock",
            "description": "Update stock level for a product SKU after shipment or receipt",
            "id": "update_stock",
            "tags": "[]string{\"inventory\", \"write\"}",
            "streaming": "false",
            "timeout_ms": "10000",
            "retry_policy": "none",
            "input_schema": "map[string]interface{}{}",
            "output_schema": "map[string]interface{}{}",
            "input_params": [
                {
                    "name": "sku",
                    "description": "Product SKU identifier",
                    "rust_type": "String",
                    "go_type": "string",
                    "java_type": "String",
                    "ts_type": "string",
                    "is_required": "true"
                },
                {
                    "name": "delta",
                    "description": "Quantity change (positive or negative)",
                    "rust_type": "i64",
                    "go_type": "int64",
                    "java_type": "Long",
                    "ts_type": "number",
                    "is_required": "true"
                }
            ],
            "output_params": [
                {
                    "name": "new_quantity",
                    "description": "Updated stock quantity",
                    "rust_type": "i64",
                    "go_type": "int64",
                    "java_type": "Long",
                    "ts_type": "number"
                },
                {
                    "name": "success",
                    "description": "Whether the update succeeded",
                    "rust_type": "bool",
                    "go_type": "bool",
                    "java_type": "Boolean",
                    "ts_type": "boolean"
                }
            ]
        }
    ]);
    ctx.insert("skills", &skills);
    ctx
}

/// Java-specific MCP context (has package_name).
fn mcp_java_context() -> Context {
    let mut ctx = mcp_context();
    ctx.insert("package_name", "com.ggen.mcp");
    ctx
}

/// Java-specific A2A context (has package_name).
fn a2a_java_context() -> Context {
    let mut ctx = a2a_context();
    ctx.insert("package_name", "com.ggen.a2a");
    ctx
}

/// Adapter template context: SPARQL-based MCP server.rs.tera
fn adapter_mcp_sparql_context() -> Context {
    let mut ctx = Context::new();
    ctx.insert("server_name", "board_report");
    ctx.insert("server_version", "0.5.0");
    ctx.insert("server_description", "Board intelligence reporting server");
    ctx.insert("transport_type", "stdio");

    let sparql_results = serde_json::json!([
        { "tool_name": "generate_report", "tool_description": "Generate a quarterly board intelligence report" },
        { "tool_name": "get_envelope", "tool_description": "Retrieve a specific governance envelope" },
        { "tool_name": "get_envelope", "tool_description": "Retrieve a specific governance envelope (duplicate row)" }
    ]);
    ctx.insert("sparql_results", &sparql_results);
    ctx
}

/// Adapter template context: SPARQL-based A2A agent.ex.tera
fn adapter_a2a_sparql_context() -> Context {
    let mut ctx = Context::new();
    ctx.insert("agent_name", "process_analyzer");
    ctx.insert("agent_version", "1.0.0");
    ctx.insert(
        "agent_description",
        "Analyzes business processes and detects bottlenecks",
    );
    ctx.insert("agent_url", "http://localhost:8090");
    ctx.insert("provider_name", "ChatmanGPT");

    let sparql_results = serde_json::json!([
        { "skill_name": "analyze_process", "skill_description": "Run process mining analysis on event logs", "skill_tags": "[\"mining\", \"analysis\"]", "streaming": "false", "timeout_ms": "30000", "retry_policy": "exponential" },
        { "skill_name": "detect_bottleneck", "skill_description": "Identify bottlenecks in a process model", "skill_tags": "[\"mining\", \"optimization\"]", "streaming": "false", "timeout_ms": "15000", "retry_policy": "none" }
    ]);
    ctx.insert("sparql_results", &sparql_results);
    ctx
}

// ---------------------------------------------------------------------------
// Template resolution helper
// ---------------------------------------------------------------------------

/// Resolve the ggen workspace root directory.
/// Tests run with CWD set by cargo, which may be the workspace root or the target dir.
fn workspace_root() -> std::path::PathBuf {
    // CARGO_MANIFEST_DIR = /Users/sac/ggen/crates/ggen-core
    // Workspace root = two levels up
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR must be set");
    let manifest = std::path::Path::new(&manifest_dir);
    // ggen-core is at crates/ggen-core, so workspace root is ../../
    let root = manifest
        .parent()
        .unwrap() // crates/
        .parent()
        .unwrap() // ggen/
        .to_path_buf();
    root
}

/// Load a template from the ggen repo root templates/ directory.
fn load_root_template(name: &str) -> String {
    let root = workspace_root();
    let candidates = [root.join("templates").join(name)];
    for path in &candidates {
        if let Ok(content) = std::fs::read_to_string(path) {
            return content;
        }
    }
    panic!(
        "Template not found: {name}. Tried: {:?}",
        candidates
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
    );
}

/// Load a template from crates/ggen-core/templates/
fn load_core_template(name: &str) -> String {
    let root = workspace_root();
    let candidates = [root
        .join("crates")
        .join("ggen-core")
        .join("templates")
        .join(name)];
    for path in &candidates {
        if let Ok(content) = std::fs::read_to_string(path) {
            return content;
        }
    }
    panic!(
        "Core template not found: {name}. Tried: {:?}",
        candidates
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
    );
}

// ---------------------------------------------------------------------------
// Render + assert helper
// ---------------------------------------------------------------------------

struct RenderResult {
    template_name: &'static str,
    output_file: &'static str,
    rendered: String,
}

fn render_and_save(
    tera: &mut Tera, template_name: &'static str, output_file: &'static str, template_str: &str,
    ctx: &Context,
) -> RenderResult {
    let rendered = render_template_str(tera, template_str, ctx)
        .unwrap_or_else(|e| panic!("FAILED to render {template_name}: {e}"));
    assert!(
        !rendered.trim().is_empty(),
        "{template_name} produced empty output"
    );
    write_output(output_file, &rendered);
    RenderResult {
        template_name,
        output_file,
        rendered,
    }
}

// ===========================================================================
// MCP Template Tests (root templates/)
// ===========================================================================

#[test]
fn test_render_mcp_rust() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-rust.tera");
    let ctx = mcp_context();
    let result = render_and_save(&mut tera, "mcp-rust.tera", "mcp_server.rs", &template, &ctx);

    // Verify key code elements
    assert!(result.rendered.contains("pub struct CreateOrderParams"));
    assert!(result.rendered.contains("pub struct GetOrderStatusParams"));
    assert!(result.rendered.contains("pub customer_id: String"));
    assert!(result.rendered.contains("pub amount: f64"));
    assert!(result.rendered.contains("#[tokio::main]"));
    assert!(result.rendered.contains("async fn create_order("));
    assert!(result.rendered.contains("async fn get_order_status("));
    assert!(result.rendered.contains("rmcp::transport::stdio()"));
    assert!(result.rendered.contains("#[tool_handler]"));
    assert!(result.rendered.contains("#[tool_router]"));
    assert!(result
        .rendered
        .contains("impl ServerHandler for OrderProcessor"));
    // Verify correct rmcp 1.3.0 Parameters pattern (NOT #[tool(aggr)])
    assert!(result.rendered.contains("Parameters(params): Parameters<CreateOrderParams>"));
    assert!(result.rendered.contains("Parameters(params): Parameters<GetOrderStatusParams>"));
    assert!(!result.rendered.contains("#[tool(aggr)]"), "Template must use Parameters wrapper, not #[tool(aggr)]");
    // Verify correct schemars import
    assert!(result.rendered.contains("schemars::JsonSchema"));
}

#[test]
fn test_render_mcp_typescript() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-typescript.tera");
    let ctx = mcp_context();
    let result = render_and_save(
        &mut tera,
        "mcp-typescript.tera",
        "mcp_server.ts",
        &template,
        &ctx,
    );

    assert!(result.rendered.contains("const server = new Server("));
    assert!(result.rendered.contains("handleCreateOrder("));
    assert!(result.rendered.contains("handleGetOrderStatus("));
    assert!(result.rendered.contains("StdioServerTransport"));
    assert!(result.rendered.contains("ListToolsRequestSchema"));
    assert!(result.rendered.contains("CallToolRequestSchema"));
}

#[test]
fn test_render_mcp_go() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-go.tera");
    let ctx = mcp_context();
    let result = render_and_save(&mut tera, "mcp-go.tera", "mcp_server.go", &template, &ctx);

    assert!(result.rendered.contains("package main"));
    assert!(result.rendered.contains("type MCPServer struct"));
    assert!(result.rendered.contains("func handleCreateOrder("));
    // Verify {% raw %} blocks preserved Go composite literals correctly
    assert!(result.rendered.contains("[]ToolContent{"));
    assert!(result.rendered.contains("Type: \"text\""));
}

#[test]
fn test_render_mcp_elixir() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-elixir.tera");
    let ctx = mcp_context();
    let result = render_and_save(
        &mut tera,
        "mcp-elixir.tera",
        "mcp_server.ex",
        &template,
        &ctx,
    );

    assert!(result
        .rendered
        .contains("defmodule OrderProcessor.McpServer do"));
    assert!(result.rendered.contains("use GenServer"));
    assert!(result.rendered.contains("@tools ["));
    assert!(result.rendered.contains("create_order"));
    assert!(result.rendered.contains("get_order_status"));
    assert!(result.rendered.contains("def start_link("));
    assert!(result.rendered.contains("def handle_call({:call_tool,"));

    // Also write as .ex file for elixirc check
    write_output("mcp_server_elixir.ex", &result.rendered);
}

#[test]
fn test_render_mcp_java() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-java.tera");
    let ctx = mcp_java_context();
    let result = render_and_save(
        &mut tera,
        "mcp-java.tera",
        "McpServer.java",
        &template,
        &ctx,
    );

    assert!(result.rendered.contains("package com.ggen.mcp;"));
    assert!(result
        .rendered
        .contains("public class OrderProcessorServer"));
    assert!(result.rendered.contains("record CreateOrderInput("));
    assert!(result.rendered.contains("record GetOrderStatusInput("));
    assert!(result
        .rendered
        .contains("public ToolResult handleCreateOrder("));
    assert!(result
        .rendered
        .contains("public ToolResult handleGetOrderStatus("));
    assert!(result
        .rendered
        .contains("public void run() throws IOException"));
}

// ===========================================================================
// A2A Template Tests (root templates/)
// ===========================================================================

#[test]
fn test_render_a2a_rust() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-rust.tera");
    let ctx = a2a_context();
    let result = render_and_save(&mut tera, "a2a-rust.tera", "a2a_agent.rs", &template, &ctx);

    assert!(result.rendered.contains("pub struct AgentCard"));
    assert!(result.rendered.contains("pub enum TaskStatus"));
    assert!(result.rendered.contains("pub struct CheckStockInput"));
    assert!(result.rendered.contains("pub struct UpdateStockInput"));
    assert!(result.rendered.contains("pub async fn handle_check_stock("));
    assert!(result
        .rendered
        .contains("pub async fn handle_update_stock("));
    assert!(result.rendered.contains("#[tokio::main]"));
    assert!(result.rendered.contains("axum::serve("));
    assert!(result.rendered.contains("mod tests"));
}

#[test]
fn test_render_a2a_typescript() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-typescript.tera");
    let ctx = a2a_context();
    let result = render_and_save(
        &mut tera,
        "a2a-typescript.tera",
        "a2a_agent.ts",
        &template,
        &ctx,
    );

    assert!(result.rendered.contains("const AGENT_CARD: AgentCard"));
    assert!(result.rendered.contains("handleCheckStock("));
    assert!(result.rendered.contains("handleUpdateStock("));
    assert!(result
        .rendered
        .contains("app.get(\"/.well-known/agent.json\""));
    assert!(result.rendered.contains("app.post(\"/a2a/message\""));
    assert!(result.rendered.contains("app.listen(PORT"));
}

#[test]
fn test_render_a2a_go() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-go.tera");
    let ctx = a2a_context();
    let result = render_and_save(&mut tera, "a2a-go.tera", "a2a_agent.go", &template, &ctx);

    assert!(result.rendered.contains("package main"));
    assert!(result.rendered.contains("type AgentCard struct"));
    assert!(result.rendered.contains("func handleCheckStock("));
    assert!(result.rendered.contains("func handleUpdateStock("));
}

#[test]
fn test_render_a2a_elixir() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-elixir.tera");
    let ctx = a2a_context();
    let result = render_and_save(
        &mut tera,
        "a2a-elixir.tera",
        "a2a_agent.ex",
        &template,
        &ctx,
    );

    assert!(result.rendered.contains("defmodule InventoryAgent do"));
    assert!(result.rendered.contains("use GenServer"));
    assert!(result.rendered.contains("def handle_check_stock("));
    assert!(result.rendered.contains("def handle_update_stock("));
    assert!(result.rendered.contains("def start_link("));
    assert!(result.rendered.contains("def send_message("));
    assert!(result
        .rendered
        .contains("defmodule InventoryAgent.Supervisor"));

    // Bug 1 fix: AgentTask (not Task) to avoid Elixir stdlib collision
    assert!(result.rendered.contains("defmodule AgentTask do"),
        "Bug 1: inner module must be AgentTask, not Task (stdlib collision)");
    assert!(!result.rendered.contains("defmodule Task do"),
        "Bug 1: must NOT contain defmodule Task (stdlib collision)");
    assert!(result.rendered.contains("AgentTask.new("),
        "Bug 1: must use AgentTask.new() not Task.new()");

    // Bug 2 fix: direct Task.Supervisor.start_child (no PartitionSupervisor tuple)
    assert!(!result.rendered.contains("PartitionSupervisor"),
        "Bug 2: must NOT use PartitionSupervisor tuple");
    assert!(result.rendered.contains("Task.Supervisor.start_child("),
        "Bug 2: must use direct Task.Supervisor.start_child");
    assert!(result.rendered.contains("Process.monitor(pid)"),
        "Bug 2: must monitor the spawned process pid");

    // Bug 3 fix: handle_info DOWN uses 5-tuple with guard
    assert!(result.rendered.contains("{:DOWN, _ref, :process, _pid, :normal}"),
        "Bug 3: handle_info DOWN must use 5-tuple for :normal");
    assert!(result.rendered.contains("when reason != :normal"),
        "Bug 3: abnormal exit clause must guard against :normal to avoid overlap");

    // Bug 4 fix: @default_timeout_ms is referenced in send_message
    assert!(result.rendered.contains("@default_timeout_ms"),
        "Bug 4: @default_timeout_ms must be declared");
    assert!(result.rendered.contains(", @default_timeout_ms)"),
        "Bug 4: send_message must use @default_timeout_ms for GenServer.call timeout");

    // Bug 5 fix: streaming uses boolean (false/true), not string
    assert!(!result.rendered.contains("streaming: \"false\""),
        "Bug 5: streaming must be boolean false, not string \"false\"");
    assert!(!result.rendered.contains("streaming: \"true\""),
        "Bug 5: streaming must be boolean true, not string \"true\"");

    // Also write as .ex file for elixirc check
    write_output("a2a_agent_elixir.ex", &result.rendered);
}

#[test]
fn test_render_a2a_java() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-java.tera");
    let ctx = a2a_java_context();
    let result = render_and_save(&mut tera, "a2a-java.tera", "A2AAgent.java", &template, &ctx);

    assert!(result.rendered.contains("package com.ggen.a2a;"));
    assert!(result.rendered.contains("@RestController"));
    assert!(result.rendered.contains("public class InventoryAgentAgent"));
    assert!(result.rendered.contains("record CheckStockInput("));
    assert!(result.rendered.contains("record UpdateStockInput("));
    assert!(result.rendered.contains("@SpringBootApplication"));
    assert!(result.rendered.contains("SpringApplication.run("));
}

// ===========================================================================
// Adapter Template Tests (crates/ggen-core/templates/)
// ===========================================================================
// NOTE: Adapter templates use Tera's {% set %} tag to build arrays from
// SPARQL results. This is NOT supported by Tera's render_str() one-off mode.
// These templates are designed to be used through the ggen pipeline, not
// directly via render_str. We test them by verifying they produce the expected
// error (missing context variables set by {% set %} blocks).

#[test]
fn test_adapter_mcp_server_rs_render() {
    let mut tera = create_test_tera();
    let template = load_core_template("mcp/server.rs.tera");
    let ctx = adapter_mcp_sparql_context();

    // Adapter templates now render successfully with SPARQL context data
    let result = render_and_save(&mut tera, "mcp/server.rs.tera", "adapter_mcp_server.rs", &template, &ctx);
    assert!(!result.rendered.is_empty());
    assert!(result.rendered.contains("fn main"));
}

#[test]
fn test_adapter_a2a_agent_ex_render() {
    let mut tera = create_test_tera();
    let template = load_core_template("a2a/agent.ex.tera");
    let ctx = adapter_a2a_sparql_context();

    // Adapter templates now render successfully with SPARQL context data
    let result = render_and_save(&mut tera, "a2a/agent.ex.tera", "adapter_a2a_agent.ex", &template, &ctx);
    assert!(!result.rendered.is_empty());
}

// ===========================================================================
// Edge case: empty tools/skills
// ===========================================================================

#[test]
fn test_render_mcp_rust_empty_tools() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-rust.tera");

    let mut ctx = mcp_context();
    let empty_tools: serde_json::Value = serde_json::json!([]);
    ctx.insert("tools", &empty_tools);

    let result = render_and_save(
        &mut tera,
        "mcp-rust.tera (empty tools)",
        "mcp_server_empty_tools.rs",
        &template,
        &ctx,
    );
    assert!(result.rendered.contains("pub struct OrderProcessor"));
    assert!(!result
        .rendered
        .contains("#[derive(Debug, Deserialize, Serialize, JsonSchema)]"));
    // Empty tools -> no Params structs
    assert!(!result.rendered.contains("Params"));
}

#[test]
fn test_render_a2a_rust_empty_skills() {
    let mut tera = create_test_tera();
    let template = load_root_template("a2a-rust.tera");

    let mut ctx = a2a_context();
    let empty_skills: serde_json::Value = serde_json::json!([]);
    ctx.insert("skills", &empty_skills);

    let result = render_and_save(
        &mut tera,
        "a2a-rust.tera (empty skills)",
        "a2a_agent_empty_skills.rs",
        &template,
        &ctx,
    );
    assert!(result.rendered.contains("pub struct AgentCard"));
    // Skills vec should be empty
    assert!(result.rendered.contains("skills: vec!["));
    assert!(result.rendered.contains("]"));
    // No skill-specific structs should be generated when skills array is empty
    assert!(!result.rendered.contains("CheckStockInput"));
    assert!(!result.rendered.contains("UpdateStockInput"));
}

// ===========================================================================
// Edge case: SSE transport
// ===========================================================================

#[test]
fn test_render_mcp_rust_http_transport() {
    let mut tera = create_test_tera();
    let template = load_root_template("mcp-rust.tera");

    let mut ctx = mcp_context();
    ctx.insert("transport_type", "http");

    let result = render_and_save(
        &mut tera,
        "mcp-rust.tera (http transport)",
        "mcp_server_http.rs",
        &template,
        &ctx,
    );
    // The root template doesn't differentiate transport in code (it's stdio-only)
    // but should still render
    assert!(!result.rendered.is_empty());
}

// NOTE: Adapter transport tests removed because adapter templates use
// {% set %} blocks which are not supported by render_str().

// ===========================================================================
// Summary: print first 20 lines of each rendered file
// ===========================================================================

#[test]
fn test_summary_print_rendered_outputs() {
    let dir = "/tmp/ggen-mcp-a2a-test";
    let mut entries: Vec<_> = std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file())
        .collect();
    entries.sort_by_key(|e| e.file_name());

    println!("\n========== RENDERED TEMPLATE SUMMARY ==========");
    println!("Output directory: {dir}\n");

    for entry in &entries {
        let name = entry.file_name().to_string_lossy().to_string();
        let content = std::fs::read_to_string(entry.path()).unwrap_or_default();
        let lines: Vec<&str> = content.lines().take(20).collect();
        println!(
            "--- {} ({} bytes, {} total lines) ---",
            name,
            content.len(),
            content.lines().count()
        );
        for line in &lines {
            println!("  {line}");
        }
        println!();
    }

    println!("Total files rendered: {}", entries.len());
    assert!(
        !entries.is_empty(),
        "Should have rendered at least one file"
    );
}
