# MCP + Rig Integration Architecture Specification

## Executive Summary

This specification defines a comprehensive ggen-based example for integrating Model Context Protocol (MCP) with the Rig AI framework in Rust. The system enables dynamic tool selection using RAG-based embeddings, multi-provider LLM support, and streaming chat interfaces.

## System Overview

### Purpose
Create a template-driven project generator that produces production-ready MCP + Rig integrations with:
- Multi-server MCP coordination
- Dynamic tool selection via vector embeddings
- Support for multiple LLM providers (Deepseek, Cohere, OpenAI)
- Beautiful streaming CLI interface
- Configuration-driven architecture

### Key Features
1. **MCP Tool Adaptation**: Convert MCP tools to Rig's `ToolDyn` interface
2. **Dynamic Tool Selection**: RAG-based tool discovery using embeddings and vector stores
3. **Multi-Server Support**: Manage multiple MCP servers simultaneously
4. **Streaming Chat**: Real-time responses with tool call visualization
5. **Configuration-Driven**: TOML-based configuration for servers and API keys

## Architecture Design

### 1. Component Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     CLI Application (main.rs)                │
│  - Configuration Loading                                     │
│  - LLM Client Initialization (Deepseek, Cohere)             │
│  - MCP Manager Setup                                         │
│  - Tool Embedding & Vector Store                            │
│  - Chat Agent Construction                                   │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ↓
┌─────────────────────────────────────────────────────────────┐
│              Configuration Module (config.rs)                │
│  - TOML Configuration Loading                                │
│  - API Key Management                                        │
│  - MCP Server Configuration (config/mcp.rs)                 │
│    * Stdio, SSE, Streamable transports                      │
│    * Multi-server management                                │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ↓
┌─────────────────────────────────────────────────────────────┐
│           MCP Adaptor Module (mcp_adaptor.rs)               │
│  - McpToolAdaptor: Converts MCP tools → Rig ToolDyn         │
│  - McpManager: Coordinates multiple MCP servers             │
│  - Tool listing and execution                               │
│  - Result conversion (CallToolResult → String)              │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ↓
┌─────────────────────────────────────────────────────────────┐
│                Chat Module (chat.rs)                         │
│  - Interactive CLI with colored output                       │
│  - Streaming response handling                              │
│  - Tool call visualization                                   │
│  - Error handling and logging                               │
└─────────────────────────────────────────────────────────────┘
```

### 2. Data Flow Architecture

```
User Input → CLI
           ↓
   Configuration Load (config.toml)
           ↓
   LLM Client Init (Deepseek/Cohere)
           ↓
   MCP Manager Start (stdio/sse/streamable)
           ↓
   Tool Discovery & Embedding
           ↓
   Vector Store Population
           ↓
   Chat Agent Construction
           ↓
   Interactive Chat Loop
           ↓
   Query → RAG Tool Selection → Tool Call → Stream Response
```

### 3. MCP Server Configuration Patterns

#### Transport Types
1. **Stdio**: Local process communication
2. **SSE (Server-Sent Events)**: HTTP-based streaming
3. **Streamable**: Bidirectional streaming

#### Configuration Example
```toml
[mcp]
[[mcp.server]]
name = "filesystem"
protocol = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/allowed/files"]

[[mcp.server]]
name = "brave-search"
protocol = "sse"
url = "http://localhost:3001/sse"

[[mcp.server]]
name = "postgres"
protocol = "streamable"
url = "http://localhost:3002/stream"
envs = { DATABASE_URL = "postgresql://..." }
```

### 4. Tool Adaptation Pattern

```rust
// MCP Tool → Rig ToolDyn Conversion
McpToolAdaptor {
    tool: McpTool,           // Original MCP tool definition
    server: ServerSink,       // Communication channel
}

impl RigTool for McpToolAdaptor {
    fn definition(&self) -> rig::tool::ToolDefinition {
        // Convert MCP schema to Rig definition
    }

    async fn call(&self, args: String) -> String {
        // Execute MCP tool and convert result
    }
}

impl ToolEmbeddingDyn for McpToolAdaptor {
    async fn embed(&self, client: &dyn EmbeddingModel) -> Vec<f32> {
        // Create embeddings for RAG-based selection
    }
}
```

## Module Specifications

### 1. Main Module (`main.rs`)

**Responsibilities:**
- Initialize tracing/logging with rolling file appender
- Load configuration from TOML
- Create LLM clients (Deepseek, Cohere) with fallback to env vars
- Start MCP manager with all configured servers
- Build tool embeddings using Cohere
- Populate in-memory vector store
- Construct chat agent with dynamic tool selection
- Launch interactive CLI chat

**Key Dependencies:**
```toml
rig-core = "0.15.1"
tokio = { version = "1", features = ["full"] }
rmcp = { workspace = true }
tracing = "0.1"
tracing-subscriber = "0.3"
tracing-appender = "0.2"
```

**Configuration Pattern:**
```rust
let config = Config::retrieve("config.toml").await?;
let mcp_manager = config.mcp.create_manager().await?;
let tool_set = mcp_adaptor::get_tool_set(mcp_manager.clone()).await?;

let embeddings = EmbeddingsBuilder::new(cohere_client.embedding_model("embed-english-v3.0"))
    .build_from_tools(tool_set.tools())
    .await?;

let vector_store = InMemoryVectorStore::from_embeddings(embeddings).await?;
let agent = completion_model.agent("gpt-4o")
    .dynamic_tools(max_tools, vector_store)
    .build();
```

### 2. Configuration Module (`config.rs` + `config/mcp.rs`)

**Responsibilities:**
- Load TOML configuration
- Parse MCP server configurations
- Manage API keys (Deepseek, Cohere)
- Start MCP servers with appropriate transports

**MCP Server Config Structure:**
```rust
pub struct McpServerConfig {
    name: String,
    transport: McpServerTransportConfig,
}

pub enum McpServerTransportConfig {
    Streamable { url: String },
    Sse { url: String },
    Stdio {
        command: String,
        args: Vec<String>,
        envs: HashMap<String, String>,
    },
}
```

**Manager Creation:**
```rust
impl McpConfig {
    pub async fn create_manager(&self) -> anyhow::Result<McpManager> {
        let mut clients = HashMap::new();
        let mut task_set = tokio::task::JoinSet::new();

        for server in &self.server {
            task_set.spawn(async move {
                let client = server.transport.start().await?;
                Ok((server.name.clone(), client))
            });
        }

        // Collect results
        for result in task_set.join_all().await {
            // Handle startup results
        }

        Ok(McpManager { clients })
    }
}
```

### 3. MCP Adaptor Module (`mcp_adaptor.rs`)

**Responsibilities:**
- Adapt MCP tools to Rig's ToolDyn interface
- Manage multiple MCP server connections
- List available tools from all servers
- Execute tool calls and convert results
- Generate embeddings for tool descriptions

**Key Components:**

```rust
pub struct McpToolAdaptor {
    tool: McpTool,
    server: ServerSink,
}

impl RigTool for McpToolAdaptor {
    fn definition(&self) -> ToolDefinition {
        ToolDefinition {
            name: self.tool.name.clone(),
            description: self.tool.description.clone().unwrap_or_default(),
            parameters: self.tool.input_schema.clone(),
        }
    }

    async fn call(&self, args: String) -> String {
        let params = serde_json::from_str(&args)?;
        let result = self.server.call_tool(params).await?;
        convert_mcp_call_tool_result_to_string(result)
    }
}

impl ToolEmbeddingDyn for McpToolAdaptor {
    async fn embed(&self, client: &dyn EmbeddingModel) -> Vec<f32> {
        let text = format!(
            "{}: {}",
            self.tool.name,
            self.tool.description.as_deref().unwrap_or("")
        );
        client.embed_text(&text).await
    }
}

pub struct McpManager {
    pub clients: HashMap<String, RunningService<RoleClient, ()>>,
}

impl McpManager {
    pub async fn get_all_tools(&self) -> ToolSet {
        let mut tools = Vec::new();
        for (name, client) in &self.clients {
            let server_tools = get_tool_set(client.server()).await?;
            tools.extend(server_tools);
        }
        ToolSet::from(tools)
    }
}
```

### 4. Chat Module (`chat.rs`)

**Responsibilities:**
- Interactive CLI with colored terminal output
- Stream agent responses in real-time
- Display tool calls with formatting
- Handle errors gracefully
- Maintain chat history

**Key Features:**
```rust
pub async fn cli_chatbot(agent: Agent) -> anyhow::Result<()> {
    println!("{}", "🤖 Chat Agent Ready!".bright_cyan().bold());

    loop {
        let input = read_user_input()?;

        if input.trim().eq_ignore_ascii_case("exit") {
            break;
        }

        let mut stream = agent.chat(&input).await?;

        while let Some(chunk) = stream.next().await {
            match chunk {
                ChatEvent::Response(text) => {
                    print!("{}", text.green());
                }
                ChatEvent::ToolCall(call) => {
                    println!("\n{}", format_tool_call(&call).yellow());
                }
                ChatEvent::Error(e) => {
                    eprintln!("{}", format!("Error: {}", e).red());
                }
            }
        }

        println!();
    }

    Ok(())
}
```

## Template Structure

### Directory Layout
```
/Users/sac/ggen/examples/mcp-rig-integration/
├── README.md                    # Comprehensive guide
├── templates/                   # ggen templates
│   ├── main-rs.tmpl            # Main application template
│   ├── chat-rs.tmpl            # Chat module template
│   ├── config-rs.tmpl          # Config module template
│   ├── config-mcp-rs.tmpl      # MCP config submodule template
│   ├── mcp-adaptor-rs.tmpl     # MCP adaptor template
│   ├── cargo-toml.tmpl         # Cargo.toml template
│   ├── config-toml.tmpl        # config.toml template
│   └── lib-rs.tmpl             # Optional lib.rs template
├── schemas/                     # Template schemas
│   ├── project-config.yaml     # Project configuration schema
│   └── mcp-server.yaml         # MCP server definition schema
├── examples/                    # Example configurations
│   ├── filesystem-server.toml  # Filesystem MCP example
│   ├── api-server.toml         # API MCP example
│   └── multi-server.toml       # Multi-server example
├── scripts/                     # Generation scripts
│   ├── generate-project.sh     # Main generation script
│   ├── add-mcp-server.sh       # Add MCP server script
│   └── test-integration.sh     # Integration testing
└── docs/                        # Documentation
    ├── ARCHITECTURE.md          # This document
    ├── TEMPLATES.md             # Template usage guide
    ├── MCP_SERVERS.md           # MCP server configuration
    └── CUSTOMIZATION.md         # Customization guide
```

## Template Variables

### Global Variables
```yaml
# Project metadata
project_name: "my-mcp-integration"
package_name: "my-mcp-integration"
version: "0.1.0"
authors: ["Your Name <email@example.com>"]

# LLM providers (optional, falls back to env vars)
deepseek_key: ""
cohere_key: ""
openai_key: ""

# Chat configuration
default_model: "gpt-4o"
max_dynamic_tools: 5
enable_streaming: true
enable_colored_output: true

# Logging
log_level: "info"
log_rotation: "daily"
log_directory: "logs"
```

### MCP Server Variables
```yaml
mcp_servers:
  - name: "filesystem"
    protocol: "stdio"
    command: "npx"
    args: ["-y", "@modelcontextprotocol/server-filesystem", "./allowed-files"]

  - name: "brave-search"
    protocol: "sse"
    url: "http://localhost:3001/sse"

  - name: "postgres"
    protocol: "streamable"
    url: "http://localhost:3002/stream"
    envs:
      DATABASE_URL: "postgresql://user:pass@localhost/db"
```

### Provider Configuration
```yaml
llm_providers:
  deepseek:
    enabled: true
    model: "deepseek-chat"
    api_key_env: "DEEPSEEK_API_KEY"

  cohere:
    enabled: true
    embedding_model: "embed-english-v3.0"
    api_key_env: "COHERE_API_KEY"

  openai:
    enabled: false
    model: "gpt-4o"
    api_key_env: "OPENAI_API_KEY"
```

## Template Implementation

### 1. Main Template (`main-rs.tmpl`)

```rust
{{#if use_tracing}}
use tracing_appender::rolling::{RollingFileAppender, Rotation};
{{/if}}
use rig::{
    client::{CompletionClient, ProviderClient},
    embeddings::EmbeddingsBuilder,
    providers::{{{providers}}},
    vector_store::in_memory_store::InMemoryVectorStore,
};

pub mod chat;
pub mod config;
pub mod mcp_adaptor;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    {{#if use_tracing}}
    // Logging setup
    let file_appender = RollingFileAppender::new(
        Rotation::{{log_rotation}},
        "{{log_directory}}",
        format!("{}.log", env!("CARGO_CRATE_NAME")),
    );

    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::{{log_level}}.into()),
        )
        .with_writer(file_appender)
        .with_file(false)
        .with_ansi(false)
        .init();
    {{/if}}

    // Load configuration
    let config = config::Config::retrieve("config.toml").await?;

    {{#each llm_providers}}
    {{#if this.enabled}}
    // {{@key}} client
    let {{@key}}_client = {
        if let Some(key) = config.{{@key}}_key {
            {{provider_type}}::Client::new(&key)
        } else {
            {{provider_type}}::Client::from_env()
        }
    };
    {{/if}}
    {{/each}}

    // Start MCP manager
    let mcp_manager = config.mcp.create_manager().await?;
    tracing::info!(
        "MCP Manager created, {} servers started",
        mcp_manager.clients.len()
    );

    // Get all tools from MCP servers
    let tool_set = mcp_manager.get_all_tools().await?;
    tracing::info!("Loaded {} tools from MCP servers", tool_set.len());

    {{#if enable_embeddings}}
    // Build tool embeddings
    let embeddings = EmbeddingsBuilder::new(
        cohere_client.embedding_model("{{cohere.embedding_model}}")
    )
    .build_from_tools(tool_set.tools())
    .await?;

    let vector_store = InMemoryVectorStore::from_embeddings(embeddings).await?;
    {{/if}}

    // Build completion model
    let completion_model = {{primary_provider}}_client
        .completion_model("{{default_model}}");

    // Construct agent with dynamic tools
    let agent = completion_model
        .agent("{{default_model}}")
        {{#if enable_embeddings}}
        .dynamic_tools({{max_dynamic_tools}}, vector_store)
        {{else}}
        .tools(tool_set)
        {{/if}}
        .build();

    // Start chat interface
    chat::cli_chatbot(agent).await?;

    Ok(())
}
```

### 2. Config Template (`config-rs.tmpl`)

```rust
use std::path::Path;
use serde::{Deserialize, Serialize};

pub mod mcp;

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    pub mcp: mcp::McpConfig,
    {{#each llm_providers}}
    {{#if this.enabled}}
    pub {{@key}}_key: Option<String>,
    {{/if}}
    {{/each}}
}

impl Config {
    pub async fn retrieve(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let content = tokio::fs::read_to_string(path).await?;
        let config: Self = toml::from_str(&content)?;
        Ok(config)
    }
}
```

### 3. MCP Config Template (`config-mcp-rs.tmpl`)

```rust
use std::{collections::HashMap, process::Stdio};
use rmcp::{RoleClient, ServiceExt, service::RunningService, transport::ConfigureCommandExt};
use serde::{Deserialize, Serialize};
use crate::mcp_adaptor::McpManager;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct McpServerConfig {
    name: String,
    #[serde(flatten)]
    transport: McpServerTransportConfig,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "protocol", rename_all = "lowercase")]
pub enum McpServerTransportConfig {
    Streamable { url: String },
    Sse { url: String },
    Stdio {
        command: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        envs: HashMap<String, String>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct McpConfig {
    server: Vec<McpServerConfig>,
}

impl McpConfig {
    pub async fn create_manager(&self) -> anyhow::Result<McpManager> {
        let mut clients = HashMap::new();
        let mut task_set = tokio::task::JoinSet::<anyhow::Result<_>>::new();

        for server in &self.server {
            let server = server.clone();
            task_set.spawn(async move {
                let client = match &server.transport {
                    McpServerTransportConfig::Stdio { command, args, envs } => {
                        let mut cmd = tokio::process::Command::new(command);
                        cmd.args(args)
                           .envs(envs)
                           .stdin(Stdio::piped())
                           .stdout(Stdio::piped())
                           .stderr(Stdio::inherit());

                        RoleClient::new()
                            .with_command_transport(cmd)
                            .start_service()
                            .await?
                    }
                    McpServerTransportConfig::Sse { url } => {
                        RoleClient::new()
                            .with_sse_transport(url)
                            .start_service()
                            .await?
                    }
                    McpServerTransportConfig::Streamable { url } => {
                        RoleClient::new()
                            .with_streamable_transport(url)
                            .start_service()
                            .await?
                    }
                };

                anyhow::Result::Ok((server.name.clone(), client))
            });
        }

        let start_up_result = task_set.join_all().await;
        for result in start_up_result {
            match result {
                Ok((name, client)) => {
                    tracing::info!("Started MCP server: {}", name);
                    clients.insert(name, client);
                }
                Err(e) => {
                    tracing::error!("Failed to start MCP server: {}", e);
                    return Err(e);
                }
            }
        }

        Ok(McpManager { clients })
    }
}
```

### 4. MCP Adaptor Template (`mcp-adaptor-rs.tmpl`)

```rust
use std::collections::HashMap;
use rig::tool::{ToolDyn as RigTool, ToolEmbeddingDyn, ToolSet};
use rmcp::{
    RoleClient,
    model::{CallToolRequestParam, CallToolResult, Tool as McpTool, TextContent},
    service::{RunningService, ServerSink},
};

pub struct McpToolAdaptor {
    tool: McpTool,
    server: ServerSink,
}

impl RigTool for McpToolAdaptor {
    fn definition(&self) -> rig::tool::ToolDefinition {
        rig::tool::ToolDefinition {
            name: self.tool.name.clone(),
            description: self.tool.description.clone().unwrap_or_default(),
            parameters: self.tool.input_schema.clone(),
        }
    }

    async fn call(&self, args: String) -> String {
        let params = match serde_json::from_str(&args) {
            Ok(p) => p,
            Err(e) => return format!("Error parsing arguments: {}", e),
        };

        let request = CallToolRequestParam {
            name: self.tool.name.clone(),
            arguments: Some(params),
        };

        match self.server.call_tool(request).await {
            Ok(result) => convert_mcp_call_tool_result_to_string(result),
            Err(e) => format!("Error calling tool: {}", e),
        }
    }
}

impl ToolEmbeddingDyn for McpToolAdaptor {
    async fn embed(&self, client: &dyn rig::embeddings::EmbeddingModel) -> Result<Vec<f32>, rig::embeddings::EmbeddingError> {
        let text = format!(
            "{}: {}",
            self.tool.name,
            self.tool.description.as_deref().unwrap_or("")
        );
        client.embed_text(&text).await
    }
}

pub struct McpManager {
    pub clients: HashMap<String, RunningService<RoleClient, ()>>,
}

impl McpManager {
    pub async fn get_all_tools(&self) -> anyhow::Result<ToolSet> {
        let mut all_tools: Vec<Box<dyn RigTool>> = Vec::new();

        for (server_name, client) in &self.clients {
            match get_tool_set(client.server()).await {
                Ok(tools) => {
                    tracing::info!("Loaded {} tools from {}", tools.len(), server_name);
                    all_tools.extend(tools.into_iter());
                }
                Err(e) => {
                    tracing::warn!("Failed to load tools from {}: {}", server_name, e);
                }
            }
        }

        Ok(ToolSet::from(all_tools))
    }
}

pub fn convert_mcp_call_tool_result_to_string(result: CallToolResult) -> String {
    result
        .content
        .into_iter()
        .filter_map(|content| match content {
            rmcp::model::Content::Text(TextContent { text, .. }) => Some(text),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub async fn get_tool_set(server: ServerSink) -> anyhow::Result<Vec<Box<dyn RigTool>>> {
    let tools_list = server.list_tools(None).await?;

    Ok(tools_list
        .tools
        .into_iter()
        .map(|tool| {
            Box::new(McpToolAdaptor {
                tool,
                server: server.clone(),
            }) as Box<dyn RigTool>
        })
        .collect())
}
```

### 5. Chat Template (`chat-rs.tmpl`)

```rust
use futures::StreamExt;
use rig::agent::Agent;
{{#if enable_colored_output}}
use colored::Colorize;
{{/if}}

pub async fn cli_chatbot(agent: Agent) -> anyhow::Result<()> {
    {{#if enable_colored_output}}
    println!("{}", "🤖 MCP + Rig Chat Agent Ready!".bright_cyan().bold());
    println!("{}", "Type 'exit' to quit, 'help' for commands".dimmed());
    {{else}}
    println!("🤖 MCP + Rig Chat Agent Ready!");
    println!("Type 'exit' to quit, 'help' for commands");
    {{/if}}
    println!();

    let mut history = Vec::new();

    loop {
        {{#if enable_colored_output}}
        print!("{}", "You: ".bright_blue().bold());
        {{else}}
        print!("You: ");
        {{/if}}
        std::io::Write::flush(&mut std::io::stdout())?;

        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        if input.eq_ignore_ascii_case("exit") {
            {{#if enable_colored_output}}
            println!("{}", "Goodbye! 👋".bright_green());
            {{else}}
            println!("Goodbye! 👋");
            {{/if}}
            break;
        }

        if input.eq_ignore_ascii_case("help") {
            print_help();
            continue;
        }

        history.push(input.to_string());

        {{#if enable_colored_output}}
        print!("{}", "Agent: ".bright_green().bold());
        {{else}}
        print!("Agent: ");
        {{/if}}

        {{#if enable_streaming}}
        let mut stream = agent.chat(input, &history).await?;

        while let Some(chunk) = stream.next().await {
            match chunk {
                Ok(rig::agent::ChatChunk::Response(text)) => {
                    print!("{}", text);
                    std::io::Write::flush(&mut std::io::stdout())?;
                }
                Ok(rig::agent::ChatChunk::ToolCall(call)) => {
                    {{#if enable_colored_output}}
                    println!("\n{}", format!("🔧 Calling tool: {}", call.name).yellow());
                    println!("{}", format!("   Arguments: {}", call.args).dimmed());
                    {{else}}
                    println!("\n🔧 Calling tool: {}", call.name);
                    println!("   Arguments: {}", call.args);
                    {{/if}}
                }
                Ok(rig::agent::ChatChunk::ToolResult(result)) => {
                    {{#if enable_colored_output}}
                    println!("{}", format!("✓ Result: {}", result).green());
                    {{else}}
                    println!("✓ Result: {}", result);
                    {{/if}}
                }
                Err(e) => {
                    {{#if enable_colored_output}}
                    eprintln!("{}", format!("Error: {}", e).red());
                    {{else}}
                    eprintln!("Error: {}", e);
                    {{/if}}
                }
            }
        }
        {{else}}
        let response = agent.chat(input, &history).await?;
        println!("{}", response);
        {{/if}}

        println!("\n");
    }

    Ok(())
}

fn print_help() {
    {{#if enable_colored_output}}
    println!("{}", "Available commands:".bright_yellow().bold());
    {{else}}
    println!("Available commands:");
    {{/if}}
    println!("  exit  - Quit the chat");
    println!("  help  - Show this help message");
    println!();
}
```

### 6. Cargo.toml Template (`cargo-toml.tmpl`)

```toml
[package]
name = "{{project_name}}"
version = "{{version}}"
edition = "2021"
authors = [{{#each authors}}"{{this}}"{{#unless @last}}, {{/unless}}{{/each}}]

[dependencies]
# Rig framework
rig-core = "0.15.1"

# Async runtime
tokio = { version = "1", features = ["full"] }

# MCP protocol
rmcp = { version = "0.1", features = [
    "client",
    "command-transport",
    "sse-client-transport",
    "streamable-client-transport",
] }

# Error handling
anyhow = "1.0"

# Serialization
serde = { version = "1", features = ["derive"] }
serde_json = "1"
toml = "0.9"

# Async utilities
futures = "0.3"

{{#if use_tracing}}
# Logging
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter", "std", "fmt"] }
tracing-appender = "0.2"
{{/if}}

{{#if enable_colored_output}}
# Terminal colors
colored = "2"
{{/if}}

{{#each llm_providers}}
{{#if this.enabled}}
# {{@key}} provider
{{provider_crate}} = "{{provider_version}}"
{{/if}}
{{/each}}
```

### 7. Config TOML Template (`config-toml.tmpl`)

```toml
{{#each llm_providers}}
{{#if this.enabled}}
# {{@key}} API key (optional, uses env var {{this.api_key_env}} if not set)
{{@key}}_key = "{{this.api_key}}"
{{/if}}
{{/each}}

[mcp]
{{#each mcp_servers}}
[[mcp.server]]
name = "{{this.name}}"
protocol = "{{this.protocol}}"
{{#if (eq this.protocol "stdio")}}
command = "{{this.command}}"
args = [{{#each this.args}}"{{this}}"{{#unless @last}}, {{/unless}}{{/each}}]
{{#if this.envs}}
[mcp.server.envs]
{{#each this.envs}}
{{@key}} = "{{this}}"
{{/each}}
{{/if}}
{{else if (eq this.protocol "sse")}}
url = "{{this.url}}"
{{else if (eq this.protocol "streamable")}}
url = "{{this.url}}"
{{#if this.envs}}
[mcp.server.envs]
{{#each this.envs}}
{{@key}} = "{{this}}"
{{/each}}
{{/if}}
{{/if}}

{{/each}}
```

## Generation Workflow

### 1. Project Generation Script

```bash
#!/bin/bash
# generate-project.sh

set -e

PROJECT_NAME="${1:-my-mcp-integration}"
CONFIG_FILE="${2:-config.yaml}"

echo "🚀 Generating MCP + Rig Integration: $PROJECT_NAME"

# Generate project structure
ggen project gen \
    --template mcp-rig-integration \
    --output "$PROJECT_NAME" \
    --vars-file "$CONFIG_FILE" \
    --force

# Navigate to project
cd "$PROJECT_NAME"

# Create necessary directories
mkdir -p logs
mkdir -p allowed-files

# Build project
echo "📦 Building project..."
cargo build

# Test configuration
echo "✅ Testing configuration..."
cargo run --bin test-config

echo "✨ Project generated successfully!"
echo ""
echo "Next steps:"
echo "  1. Configure MCP servers in config.toml"
echo "  2. Set API keys: export DEEPSEEK_API_KEY=... COHERE_API_KEY=..."
echo "  3. Run: cargo run"
```

### 2. Add MCP Server Script

```bash
#!/bin/bash
# add-mcp-server.sh

set -e

SERVER_NAME="$1"
PROTOCOL="$2"

if [ -z "$SERVER_NAME" ] || [ -z "$PROTOCOL" ]; then
    echo "Usage: $0 <server-name> <stdio|sse|streamable>"
    exit 1
fi

echo "📝 Adding MCP server: $SERVER_NAME ($PROTOCOL)"

case "$PROTOCOL" in
    stdio)
        read -p "Command: " COMMAND
        read -p "Args (space-separated): " ARGS
        ggen tools add-mcp-server \
            --name "$SERVER_NAME" \
            --protocol stdio \
            --command "$COMMAND" \
            --args "$ARGS"
        ;;
    sse|streamable)
        read -p "URL: " URL
        ggen tools add-mcp-server \
            --name "$SERVER_NAME" \
            --protocol "$PROTOCOL" \
            --url "$URL"
        ;;
    *)
        echo "Invalid protocol: $PROTOCOL"
        exit 1
        ;;
esac

echo "✅ Server added to config.toml"
```

## Testing & Validation

### Integration Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_config_loading() {
        let config = Config::retrieve("test-config.toml").await.unwrap();
        assert!(!config.mcp.server.is_empty());
    }

    #[tokio::test]
    async fn test_mcp_manager_startup() {
        let config = Config::retrieve("test-config.toml").await.unwrap();
        let manager = config.mcp.create_manager().await.unwrap();
        assert!(!manager.clients.is_empty());
    }

    #[tokio::test]
    async fn test_tool_discovery() {
        let manager = get_test_manager().await;
        let tools = manager.get_all_tools().await.unwrap();
        assert!(!tools.is_empty());
    }

    #[tokio::test]
    async fn test_tool_embedding() {
        let manager = get_test_manager().await;
        let tools = manager.get_all_tools().await.unwrap();
        let client = get_test_cohere_client();

        let embeddings = EmbeddingsBuilder::new(
            client.embedding_model("embed-english-v3.0")
        )
        .build_from_tools(tools.tools())
        .await
        .unwrap();

        assert!(!embeddings.is_empty());
    }
}
```

## Best Practices

### 1. Error Handling
- Use `anyhow::Result` for flexible error handling
- Log errors at appropriate levels (error, warn, info)
- Provide helpful error messages with context
- Handle MCP server startup failures gracefully

### 2. Configuration Management
- Support both explicit config and environment variables
- Validate configuration on startup
- Provide sensible defaults
- Document all configuration options

### 3. Performance Optimization
- Use async/await throughout
- Leverage parallel server startup with `JoinSet`
- Cache embeddings when possible
- Stream responses for better UX

### 4. Security
- Never commit API keys
- Validate MCP server inputs
- Use secure transports (HTTPS for SSE/Streamable)
- Sanitize user inputs before tool calls

### 5. Extensibility
- Support pluggable LLM providers
- Allow custom MCP transports
- Enable middleware for tool calls
- Support multiple vector stores

## Future Enhancements

1. **Advanced Features**
   - Multi-agent coordination
   - Tool result caching
   - Custom embedding models
   - Prompt templates

2. **Additional Transports**
   - WebSocket support
   - gRPC transport
   - Unix socket communication

3. **Monitoring & Observability**
   - Metrics collection (Prometheus)
   - Distributed tracing (Jaeger)
   - Performance dashboards
   - Tool usage analytics

4. **Developer Experience**
   - Hot-reload configuration
   - Interactive setup wizard
   - Visual tool builder
   - MCP server marketplace integration

## References

- [Rig Framework](https://rig.rs/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [MCP Rust SDK](https://github.com/modelcontextprotocol/rust-sdk)
- [Rig-MCP Example](https://github.com/RGGH/rig-mcp-server)
- [MCP + Rig Tutorial](https://dev.to/joshmo_dev/using-model-context-protocol-with-rig-m7o)

## Conclusion

This architecture provides a robust, scalable foundation for building MCP + Rig integrations using ggen templates. The modular design allows for easy customization while maintaining production-ready quality standards.

The template-driven approach enables rapid development of new MCP integrations while ensuring consistency and best practices across projects.
