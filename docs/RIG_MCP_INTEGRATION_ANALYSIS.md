# Rig-MCP Integration Analysis & Implementation Guide for ggen

**Date**: 2025-10-11
**Source**: https://github.com/modelcontextprotocol/rust-sdk/tree/main/examples/rig-integration
**Status**: ✅ Successfully cloned and built in `/Users/sac/ggen/vendors/rig-integration`

---

## 📋 Executive Summary

The **rig-integration** example demonstrates how to integrate the Model Context Protocol (MCP) with Rig, an AI agent framework for Rust. This analysis provides a comprehensive breakdown of the architecture and actionable recommendations for implementing similar patterns in ggen.

### Key Capabilities Demonstrated

1. **MCP Tool Adapter Pattern** - Wraps MCP tools for use with Rig agents
2. **Multi-Transport Support** - Stdio, SSE, and HTTP streaming transports
3. **Async Tool Execution** - Non-blocking tool calls with proper error handling
4. **Vector-Based Tool Selection** - Uses embeddings for intelligent tool routing
5. **CLI Chat Interface** - Streaming chat with tool call visualization

---

## 🏗️ Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Rig Agent (Deepseek)                      │
│  - Chat completion model                                     │
│  - Dynamic tool selection (vector-based)                     │
│  - Streaming responses                                       │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                 ToolSet (Rig Framework)                      │
│  - Vector store with tool embeddings (Cohere)               │
│  - Dynamic tool routing based on semantic similarity         │
│  - Tool definition management                                │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│              McpToolAdaptor (Adapter Layer)                  │
│  - Implements Rig's ToolDyn trait                           │
│  - Implements ToolEmbeddingDyn trait                        │
│  - Translates between Rig and MCP interfaces                │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                McpManager (MCP Client Pool)                  │
│  - Manages multiple MCP server connections                   │
│  - Handles server lifecycle (start/stop)                     │
│  - Aggregates tools from all servers                         │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│          MCP Servers (External Processes)                    │
│  ├─ Git Server (uvx mcp-server-git)                         │
│  ├─ Filesystem Server                                        │
│  ├─ Database Server                                          │
│  └─ Custom ggen Server                                       │
└─────────────────────────────────────────────────────────────┘
```

---

## 📦 Core Components Analysis

### 1. McpToolAdaptor (`src/mcp_adaptor.rs`)

**Purpose**: Adapts MCP tools to work with Rig's tool interface

**Key Traits Implemented**:

```rust
impl RigTool for McpToolAdaptor {
    fn name(&self) -> String
    fn definition(&self, _prompt: String) -> Pin<Box<dyn Future<...>>>
    fn call(&self, args: String) -> Pin<Box<dyn Future<...>>>
}

impl ToolEmbeddingDyn for McpToolAdaptor {
    fn context(&self) -> serde_json::Result<serde_json::Value>
    fn embedding_docs(&self) -> Vec<String>
}
```

**Pattern Analysis**:
- ✅ **Async-first design** - All operations return pinned futures
- ✅ **Error handling** - Proper conversion from MCP errors to Rig ToolError
- ✅ **Logging integration** - Uses tracing for observability
- ✅ **Schema translation** - Converts MCP JSON Schema to Rig's ToolDefinition

**Key Method: `call()`**
```rust
fn call(&self, args: String) -> Pin<Box<dyn Future<...>>> {
    let server = self.server.clone();
    Box::pin(async move {
        let call_mcp_tool_result = server
            .call_tool(CallToolRequestParam {
                name: self.tool.name.clone(),
                arguments: serde_json::from_str(&args)
                    .map_err(rig::tool::ToolError::JsonError)?,
            })
            .await
            .inspect(|result| tracing::info!(?result))
            .inspect_err(|error| tracing::error!(%error))
            .map_err(|e| rig::tool::ToolError::ToolCallError(Box::new(e)))?;

        Ok(convert_mcp_call_tool_result_to_string(call_mcp_tool_result))
    })
}
```

**Lessons for ggen**:
1. Use the adapter pattern to bridge between frameworks
2. Leverage Rust's trait system for clean interfaces
3. Implement proper async handling with pinned futures
4. Include comprehensive error mapping

---

### 2. McpManager (`src/mcp_adaptor.rs:77-122`)

**Purpose**: Manages multiple MCP server connections and aggregates their tools

**Core Functionality**:
```rust
pub struct McpManager {
    pub clients: HashMap<String, RunningService<RoleClient, ()>>,
}

impl McpManager {
    pub async fn get_tool_set(&self) -> anyhow::Result<ToolSet> {
        let mut tool_set = ToolSet::default();
        let mut task = tokio::task::JoinSet::<anyhow::Result<_>>::new();

        // Parallel tool fetching from all servers
        for client in self.clients.values() {
            let server = client.peer().clone();
            task.spawn(get_tool_set(server));
        }

        // Aggregate results
        let results = task.join_all().await;
        for result in results {
            match result {
                Ok(tools) => tool_set.add_tools(tools),
                Err(e) => tracing::error!(error = %e, "Failed to get tool set"),
            }
        }
        Ok(tool_set)
    }
}
```

**Pattern Analysis**:
- ✅ **Parallel execution** - Uses `JoinSet` for concurrent tool fetching
- ✅ **Error isolation** - Individual server failures don't break the entire system
- ✅ **Resource management** - Uses `HashMap` for efficient server lookup
- ✅ **Aggregation pattern** - Merges tools from multiple sources

**Lessons for ggen**:
1. Support multiple MCP servers simultaneously
2. Use parallel task execution for performance
3. Handle partial failures gracefully
4. Provide a unified tool interface across servers

---

### 3. Transport Configuration (`src/config/mcp.rs`)

**Purpose**: Flexible configuration for different MCP transport protocols

**Supported Transports**:

```rust
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "protocol", rename_all = "lowercase")]
pub enum McpServerTransportConfig {
    // HTTP streaming transport
    Streamable { url: String },

    // Server-Sent Events transport
    Sse { url: String },

    // Standard I/O (child process) transport
    Stdio {
        command: String,
        args: Vec<String>,
        envs: HashMap<String, String>,
    },
}
```

**Example Configuration** (`config.toml`):
```toml
[[mcp.server]]
name = "git"
protocol = "stdio"
command = "uvx"
args = ["mcp-server-git"]

[[mcp.server]]
name = "filesystem"
protocol = "sse"
url = "http://localhost:3000/sse"

[[mcp.server]]
name = "database"
protocol = "streamable"
url = "http://localhost:8080/mcp"
```

**Pattern Analysis**:
- ✅ **Enum-based configuration** - Type-safe transport selection
- ✅ **Serde tagging** - Clean TOML serialization with `#[serde(tag = "protocol")]`
- ✅ **Environment variables** - Stdio transport supports custom env vars
- ✅ **Async initialization** - All transports started concurrently

**Lessons for ggen**:
1. Support multiple transport protocols for flexibility
2. Use TOML for human-friendly configuration
3. Provide sane defaults while allowing customization
4. Initialize connections in parallel for fast startup

---

### 4. CLI Chat Interface (`src/chat.rs`)

**Purpose**: Interactive streaming chat with tool call visualization

**Key Features**:
```rust
pub async fn cli_chatbot<M>(chatbot: Agent<M>) -> anyhow::Result<()>
where
    M: CompletionModel,
{
    let mut chat_log = vec![];
    loop {
        // Read user input
        let mut input_buf = String::new();
        input.read_line(&mut input_buf).await?;

        // Stream agent response
        match chatbot.stream_chat(input, chat_log.clone()).await {
            Ok(mut response) => {
                while let Some(message) = response.next().await {
                    match message {
                        // Handle streaming text
                        Ok(AssistantContent::Text(text)) => {
                            message_buf.push_str(&text.text);
                            output_agent(text.text, &mut output).await?;
                        }
                        // Handle tool calls
                        Ok(AssistantContent::ToolCall(tool_call)) => {
                            let result = chatbot.tools.call(&name, arguments).await;
                            chat_log.push(Message::user(tool_call_result));
                        }
                        Err(error) => output_error(error, &mut output).await?,
                    }
                }
            }
        }
    }
}
```

**UI Elements**:
- 🟢 **User prompt**: `\x1b[32muser>\x1b[0m`
- 🤖 **Agent response**: `\x1b[1;34m🤖 Agent: \x1b[0m`
- 🛠️ **Tool call**: `\x1b[1;33m🛠 Tool Call: \x1b[0m`
- ❌ **Error**: `\x1b[1;31m❌ ERROR: \x1b[0m`

**Pattern Analysis**:
- ✅ **Streaming-first** - Uses async streams for real-time responses
- ✅ **ANSI colors** - Clean terminal output with color codes
- ✅ **Chat history** - Maintains conversation context
- ✅ **Tool transparency** - Shows tool calls to user

**Lessons for ggen**:
1. Implement streaming for better UX
2. Use ANSI colors for clear output formatting
3. Maintain chat history for context-aware responses
4. Make tool execution transparent to users

---

### 5. Main Application Flow (`src/main.rs`)

**Architecture**:

```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // 1. Setup logging
    let file_appender = RollingFileAppender::new(...);
    tracing_subscriber::fmt()...init();

    // 2. Load configuration
    let config = config::Config::retrieve("config.toml").await?;

    // 3. Initialize AI clients
    let openai_client = deepseek::Client::new(&key);
    let cohere_client = cohere::Client::new(&key);

    // 4. Start MCP servers and create manager
    let mcp_manager = config.mcp.create_manager().await?;

    // 5. Get all MCP tools
    let tool_set = mcp_manager.get_tool_set().await?;

    // 6. Create tool embeddings for semantic search
    let embedding_model = cohere_client.embedding_model(...);
    let embeddings = EmbeddingsBuilder::new(embedding_model.clone())
        .documents(tool_set.schemas()?)?
        .build()
        .await?;

    // 7. Create vector store for tool retrieval
    let store = InMemoryVectorStore::from_documents_with_id_f(embeddings, |f| {
        f.name.clone()
    });
    let index = store.index(embedding_model);

    // 8. Build agent with dynamic tool selection
    let agent = openai_client
        .agent(deepseek::DEEPSEEK_CHAT)
        .dynamic_tools(4, index, tool_set)
        .build();

    // 9. Start chat interface
    chat::cli_chatbot(agent).await?;

    Ok(())
}
```

**Flow Diagram**:
```
Config TOML → MCP Manager → Tool Set → Embeddings → Vector Store
                                                          ↓
User Input → Agent (Deepseek) ← Dynamic Tool Selection ←┘
     ↓
  Stream Response → Tool Call → MCP Server → Result → Continue
```

**Pattern Analysis**:
- ✅ **Vector-based tool routing** - Uses embeddings for intelligent tool selection
- ✅ **Multi-provider support** - Deepseek for chat, Cohere for embeddings
- ✅ **Lazy tool loading** - Tools retrieved only from active MCP servers
- ✅ **Clean separation** - Config → Setup → Execution phases

**Lessons for ggen**:
1. Use vector search for intelligent tool routing
2. Support multiple AI providers for different tasks
3. Implement clean phased initialization
4. Leverage embeddings for semantic tool discovery

---

## 🔧 Dependencies Analysis

### Core Dependencies

```toml
[dependencies]
# AI Framework
rig-core = "0.15.1"              # Rig AI agent framework

# MCP Client
rmcp = { version = "0.8", features = [
    "client",                     # MCP client role
    "transport-child-process",    # Stdio transport (uvx, npx)
    "transport-sse-client-reqwest", # SSE transport
    "transport-streamable-http-client-reqwest" # HTTP streaming
] }

# Async Runtime
tokio = { version = "1", features = ["full"] }
futures = "0.3"

# Serialization
serde = { version = "1", features = ["derive"] }
serde_json = "1"
toml = "0.9"

# Error Handling
anyhow = "1.0"

# Logging
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = [
    "env-filter", "std", "fmt"
] }
tracing-appender = "0.2"
```

**Dependency Graph**:
```
rig-integration
├── rig-core (AI agent framework)
├── rmcp (MCP client with transport support)
├── tokio (async runtime)
├── tracing (structured logging)
├── serde (serialization)
└── anyhow (error handling)
```

---

## 💡 Implementation Patterns for ggen

### Pattern 1: Adapter Layer for LLM Tool Integration

**Problem**: ggen needs to expose MCP tools to various LLM frameworks

**Solution**: Create a generic adapter trait similar to `McpToolAdaptor`

```rust
// ggen/ggen-mcp/src/adapter.rs
use crate::client::LlmClient;
use rmcp::model::Tool as McpTool;

pub struct GgenMcpAdapter {
    tool: McpTool,
    server: ServerSink,
    client: Arc<dyn LlmClient>, // ggen's LLM client
}

#[async_trait]
impl LlmTool for GgenMcpAdapter {
    async fn execute(&self, args: serde_json::Value) -> Result<String> {
        let result = self.server
            .call_tool(CallToolRequestParam {
                name: self.tool.name.clone(),
                arguments: args,
            })
            .await?;

        Ok(serde_json::to_string(&result)?)
    }

    fn schema(&self) -> serde_json::Value {
        self.tool.schema_as_json_value()
    }
}
```

**Benefits**:
- ✅ Framework-agnostic tool interface
- ✅ Reusable across different LLM providers
- ✅ Type-safe tool execution
- ✅ Clean error handling

---

### Pattern 2: Multi-Server MCP Manager

**Problem**: ggen needs to coordinate multiple MCP servers (git, filesystem, custom)

**Solution**: Implement `McpCoordinator` similar to `McpManager`

```rust
// ggen/ggen-mcp/src/coordinator.rs
pub struct McpCoordinator {
    servers: HashMap<String, RunningService<RoleClient, ()>>,
    config: CoordinatorConfig,
}

impl McpCoordinator {
    pub async fn new(config: CoordinatorConfig) -> Result<Self> {
        let mut servers = HashMap::new();
        let mut tasks = JoinSet::new();

        // Start all servers in parallel
        for server_config in config.servers {
            tasks.spawn(async move {
                Self::start_server(server_config).await
            });
        }

        // Collect results
        for result in tasks.join_all().await {
            match result {
                Ok((name, server)) => { servers.insert(name, server); }
                Err(e) => tracing::warn!("Failed to start server: {}", e),
            }
        }

        Ok(Self { servers, config })
    }

    pub async fn get_all_tools(&self) -> Result<Vec<McpTool>> {
        let mut all_tools = Vec::new();

        for (name, server) in &self.servers {
            match server.peer().list_all_tools().await {
                Ok(tools) => {
                    tracing::info!("Loaded {} tools from {}", tools.len(), name);
                    all_tools.extend(tools);
                }
                Err(e) => {
                    tracing::error!("Failed to get tools from {}: {}", name, e);
                }
            }
        }

        Ok(all_tools)
    }

    pub async fn execute_tool(&self, tool_name: &str, args: serde_json::Value) -> Result<String> {
        // Find which server provides this tool
        for server in self.servers.values() {
            let tools = server.peer().list_all_tools().await?;
            if tools.iter().any(|t| t.name == tool_name) {
                let result = server.peer().call_tool(CallToolRequestParam {
                    name: tool_name.to_string(),
                    arguments: args,
                }).await?;

                return Ok(serde_json::to_string(&result)?);
            }
        }

        Err(anyhow!("Tool {} not found in any server", tool_name))
    }
}
```

**Benefits**:
- ✅ Centralized server management
- ✅ Parallel server initialization
- ✅ Graceful partial failure handling
- ✅ Unified tool execution interface

---

### Pattern 3: Configuration-Driven Transport Selection

**Problem**: ggen needs flexible MCP server connection options

**Solution**: Use TOML configuration with transport-specific options

```rust
// ggen/ggen-mcp/src/config.rs
#[derive(Debug, Deserialize)]
pub struct McpServersConfig {
    pub servers: Vec<McpServerConfig>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct McpServerConfig {
    pub name: String,
    pub enabled: bool,
    #[serde(flatten)]
    pub transport: TransportConfig,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "protocol")]
pub enum TransportConfig {
    #[serde(rename = "stdio")]
    Stdio {
        command: String,
        args: Vec<String>,
        #[serde(default)]
        env: HashMap<String, String>,
    },
    #[serde(rename = "sse")]
    Sse { url: String },
    #[serde(rename = "http")]
    Http { url: String },
}

impl TransportConfig {
    pub async fn connect(&self) -> Result<RunningService<RoleClient, ()>> {
        match self {
            Self::Stdio { command, args, env } => {
                let transport = TokioChildProcess::new(
                    tokio::process::Command::new(command)
                        .args(args)
                        .envs(env)
                        .stderr(Stdio::null())
                )?;
                ().serve(transport).await
            }
            Self::Sse { url } => {
                let transport = SseClientTransport::start(url.clone()).await?;
                ().serve(transport).await
            }
            Self::Http { url } => {
                let transport = StreamableHttpClientTransport::from_uri(url.clone());
                ().serve(transport).await
            }
        }
    }
}
```

**Example ggen MCP config** (`ggen-mcp.toml`):
```toml
[[servers]]
name = "ggen-filesystem"
enabled = true
protocol = "stdio"
command = "npx"
args = ["@modelcontextprotocol/server-filesystem", "./templates"]

[[servers]]
name = "ggen-git"
enabled = true
protocol = "stdio"
command = "uvx"
args = ["mcp-server-git", "--repository", "."]

[[servers]]
name = "ggen-sparql"
enabled = true
protocol = "http"
url = "http://localhost:8080/mcp"

[[servers]]
name = "ggen-ultrathink"
enabled = false  # Optional server
protocol = "sse"
url = "http://localhost:3000/ultrathink/stream"
```

**Benefits**:
- ✅ Human-readable configuration
- ✅ Type-safe transport options
- ✅ Easy to enable/disable servers
- ✅ Supports all rmcp transport types

---

### Pattern 4: Tool Selection via Vector Embeddings

**Problem**: Large number of MCP tools requires intelligent routing

**Solution**: Use embeddings for semantic tool discovery

```rust
// ggen/ggen-ai/src/tool_router.rs
use crate::client::LlmClient;
use crate::generators::EmbeddingsGenerator;

pub struct SemanticToolRouter {
    embeddings_client: Arc<dyn LlmClient>,
    tool_embeddings: HashMap<String, Vec<f32>>,
    tools: Vec<McpTool>,
}

impl SemanticToolRouter {
    pub async fn new(
        tools: Vec<McpTool>,
        embeddings_client: Arc<dyn LlmClient>,
    ) -> Result<Self> {
        let mut tool_embeddings = HashMap::new();

        // Generate embeddings for each tool
        for tool in &tools {
            let doc = format!(
                "{}: {}",
                tool.name,
                tool.description.as_deref().unwrap_or("")
            );

            let embedding = embeddings_client
                .generate_embedding(&doc)
                .await?;

            tool_embeddings.insert(tool.name.clone(), embedding);
        }

        Ok(Self {
            embeddings_client,
            tool_embeddings,
            tools,
        })
    }

    pub async fn select_tools(
        &self,
        query: &str,
        top_k: usize,
    ) -> Result<Vec<McpTool>> {
        // Generate query embedding
        let query_embedding = self.embeddings_client
            .generate_embedding(query)
            .await?;

        // Calculate cosine similarity
        let mut similarities = self.tool_embeddings
            .iter()
            .map(|(name, tool_embedding)| {
                let similarity = cosine_similarity(&query_embedding, tool_embedding);
                (name.clone(), similarity)
            })
            .collect::<Vec<_>>();

        // Sort by similarity and take top_k
        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

        let selected_tools = similarities
            .into_iter()
            .take(top_k)
            .filter_map(|(name, _)| {
                self.tools.iter().find(|t| t.name == name).cloned()
            })
            .collect();

        Ok(selected_tools)
    }
}

fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    let dot_product: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let magnitude_a: f32 = a.iter().map(|x| x * x).sum::<f32>().sqrt();
    let magnitude_b: f32 = b.iter().map(|x| x * x).sum::<f32>().sqrt();
    dot_product / (magnitude_a * magnitude_b)
}
```

**Usage**:
```rust
// In ggen CLI
let router = SemanticToolRouter::new(mcp_tools, embeddings_client).await?;
let relevant_tools = router.select_tools("I need to modify git history", 3).await?;

// Pass only relevant tools to LLM
let response = llm_client.complete_with_tools(&prompt, relevant_tools).await?;
```

**Benefits**:
- ✅ Reduces LLM context window usage
- ✅ Improves tool selection accuracy
- ✅ Scales to large tool sets
- ✅ Language-agnostic semantic matching

---

## 🚀 Recommended Implementation Roadmap for ggen

### Phase 1: Foundation (Week 1-2)

**Goal**: Basic MCP integration with ggen-ai

1. **Create `ggen-mcp` crate** ✅ (already exists)
   ```bash
   cd ggen-mcp
   cargo add rmcp --features client,transport-child-process
   cargo add tokio --features full
   cargo add serde --features derive
   cargo add anyhow tracing
   ```

2. **Implement `McpCoordinator`**
   - Copy pattern from `McpManager`
   - Add configuration loading from `ggen-mcp.toml`
   - Support stdio transport (most common for MCP servers)

3. **Create basic adapter**
   - Implement `GgenMcpAdapter` for LLM tool integration
   - Connect to existing `LlmClient` trait

4. **Add tests**
   - Unit tests for coordinator
   - Integration tests with mock MCP server

**Deliverables**:
- ✅ `ggen-mcp` crate with MCP client support
- ✅ Configuration file (`ggen-mcp.toml`)
- ✅ Basic tool listing and execution
- ✅ Tests with 80%+ coverage

---

### Phase 2: LLM Integration (Week 3-4)

**Goal**: Connect MCP tools to ggen-ai's LLM clients

1. **Extend `LlmClient` trait**
   ```rust
   #[async_trait]
   pub trait LlmClient {
       async fn complete(&self, prompt: &str) -> Result<LlmResponse>;

       // NEW: Tool-aware completion
       async fn complete_with_tools(
           &self,
           prompt: &str,
           tools: Vec<ToolDefinition>,
       ) -> Result<ToolAwareResponse>;

       // NEW: Execute tool call
       async fn execute_tool_call(
           &self,
           tool_call: ToolCall,
       ) -> Result<ToolResult>;
   }
   ```

2. **Implement for OpenAI/Anthropic**
   - Use native tool calling APIs
   - Map MCP tool schemas to provider formats

3. **Add streaming support**
   - Stream tool calls and results
   - Provide progress indicators

**Deliverables**:
- ✅ Tool-aware LLM completions
- ✅ OpenAI function calling integration
- ✅ Anthropic tool use integration
- ✅ Streaming tool execution

---

### Phase 3: CLI Enhancement (Week 5)

**Goal**: Add MCP tool support to ggen CLI

1. **Add `ggen ai chat` command**
   ```bash
   ggen ai chat --mcp-config ggen-mcp.toml
   ```

2. **Implement streaming chat**
   - Copy patterns from `chat.rs`
   - Add tool call visualization
   - Show tool execution in real-time

3. **Add tool inspection commands**
   ```bash
   ggen mcp list-servers
   ggen mcp list-tools
   ggen mcp inspect-tool <tool-name>
   ggen mcp execute-tool <tool-name> <args>
   ```

**Deliverables**:
- ✅ Interactive chat with MCP tools
- ✅ Tool inspection CLI commands
- ✅ ANSI-colored output
- ✅ Comprehensive examples

---

### Phase 4: Advanced Features (Week 6+)

**Goal**: Production-ready MCP integration

1. **Semantic tool routing**
   - Implement `SemanticToolRouter`
   - Add embedding generation
   - Cache tool embeddings

2. **Multi-transport support**
   - Add SSE transport
   - Add HTTP streaming transport
   - Support transport fallbacks

3. **Tool sandboxing**
   - Add approval workflow for tool execution
   - Implement tool execution limits
   - Add audit logging

4. **Error recovery**
   - Retry failed tool calls
   - Graceful server reconnection
   - Partial failure handling

**Deliverables**:
- ✅ Vector-based tool selection
- ✅ All rmcp transport types supported
- ✅ Production-grade error handling
- ✅ Audit logging and security

---

## 📊 Comparison: ggen vs rig-integration

| Feature | rig-integration | ggen (Proposed) | Priority |
|---------|-----------------|------------------|----------|
| **MCP Client** | ✅ rmcp | ✅ rmcp | High |
| **Multi-Server** | ✅ Yes | ✅ Yes | High |
| **Stdio Transport** | ✅ Yes | ✅ Yes | High |
| **SSE Transport** | ✅ Yes | ⏭️ Future | Medium |
| **HTTP Transport** | ✅ Yes | ⏭️ Future | Medium |
| **Tool Adapter** | ✅ Rig-specific | ✅ LLM-agnostic | High |
| **Vector Routing** | ✅ Cohere embeddings | ✅ Multi-provider | High |
| **Streaming Chat** | ✅ Yes | ✅ Yes | High |
| **Config-Driven** | ✅ TOML | ✅ TOML | High |
| **CLI Commands** | ❌ No | ✅ Yes (inspect/execute) | High |
| **Template Generation** | ❌ No | ✅ Yes (core feature) | High |
| **SPARQL Integration** | ❌ No | ✅ Yes (core feature) | High |
| **Approval Workflow** | ❌ No | ⏭️ Future | Medium |
| **Audit Logging** | ⚠️ Basic tracing | ⏭️ Future | Medium |

---

## 🎯 Key Takeaways

### What Works Well in rig-integration

1. ✅ **Clean adapter pattern** - Easy to understand and extend
2. ✅ **Parallel server initialization** - Fast startup times
3. ✅ **Vector-based tool routing** - Intelligent tool selection
4. ✅ **Multiple transport support** - Flexible connectivity
5. ✅ **Comprehensive error handling** - Robust failure recovery

### What ggen Can Improve

1. ✅ **CLI-first design** - Add tool inspection commands
2. ✅ **Template integration** - Connect MCP tools to template generation
3. ✅ **SPARQL queries** - Use MCP tools in graph operations
4. ✅ **Approval workflows** - Add safety for tool execution
5. ✅ **Audit logging** - Track all tool calls for compliance

### Architecture Decisions for ggen

**Use rmcp as MCP client** ✅
- Mature, well-maintained
- Supports all transport types
- Good async/await integration

**Implement adapter layer** ✅
- Keep framework-agnostic
- Support multiple LLM providers
- Easy to test and mock

**Configuration-driven** ✅
- TOML for human readability
- Enable/disable servers easily
- Environment-specific configs

**Vector-based routing** ✅
- Scales to large tool sets
- Reduces LLM context usage
- Improves tool selection accuracy

---

## 📁 File Structure Recommendation

```
ggen/
├── ggen-mcp/
│   ├── src/
│   │   ├── lib.rs
│   │   ├── coordinator.rs      # McpCoordinator (server manager)
│   │   ├── adapter.rs           # GgenMcpAdapter (tool adapter)
│   │   ├── config.rs            # Configuration types
│   │   ├── transport.rs         # Transport implementations
│   │   ├── router.rs            # SemanticToolRouter
│   │   └── tools/
│   │       ├── mod.rs
│   │       ├── listing.rs       # Tool discovery
│   │       ├── execution.rs     # Tool execution
│   │       └── schema.rs        # Schema translation
│   ├── examples/
│   │   ├── basic_chat.rs
│   │   ├── tool_inspection.rs
│   │   └── multi_server.rs
│   └── tests/
│       ├── integration_tests.rs
│       └── mock_server.rs
├── ggen-ai/
│   ├── src/
│   │   ├── client.rs            # Add tool-aware methods
│   │   ├── tool_router.rs       # NEW: Semantic routing
│   │   └── providers/
│   │       ├── openai.rs        # Add function calling
│   │       └── anthropic.rs     # Add tool use
├── cli/
│   └── src/
│       └── cmds/
│           ├── ai/
│           │   └── chat.rs      # NEW: Streaming chat with tools
│           └── mcp/
│               ├── mod.rs       # NEW: MCP commands
│               ├── list.rs      # List servers/tools
│               ├── inspect.rs   # Inspect tool details
│               └── execute.rs   # Execute tool manually
└── ggen-mcp.toml               # Default MCP configuration
```

---

## 🧪 Testing Strategy

### Unit Tests
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_coordinator_initialization() {
        let config = McpServersConfig {
            servers: vec![
                McpServerConfig {
                    name: "test".to_string(),
                    enabled: true,
                    transport: TransportConfig::Stdio {
                        command: "echo".to_string(),
                        args: vec![],
                        env: HashMap::new(),
                    },
                },
            ],
        };

        let coordinator = McpCoordinator::new(config).await.unwrap();
        assert_eq!(coordinator.servers.len(), 1);
    }

    #[tokio::test]
    async fn test_tool_execution() {
        let adapter = create_test_adapter();
        let result = adapter.execute(json!({ "arg": "value" })).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_semantic_routing() {
        let router = create_test_router().await;
        let tools = router.select_tools("git commit", 3).await.unwrap();
        assert!(tools.iter().any(|t| t.name.contains("git")));
    }
}
```

### Integration Tests
```rust
#[tokio::test]
async fn test_end_to_end_tool_call() {
    // Start mock MCP server
    let mock_server = start_mock_mcp_server().await;

    // Create coordinator
    let coordinator = McpCoordinator::new(test_config()).await.unwrap();

    // Get tools
    let tools = coordinator.get_all_tools().await.unwrap();
    assert!(!tools.is_empty());

    // Execute tool
    let result = coordinator.execute_tool(
        "test_tool",
        json!({ "input": "test" })
    ).await.unwrap();

    assert!(result.contains("success"));
}
```

---

## 🔐 Security Considerations

### Tool Execution Approval
```rust
pub struct ApprovalWorkflow {
    mode: ApprovalMode,
}

pub enum ApprovalMode {
    Always,      // Require approval for every tool
    FirstUse,    // Approve once per tool per session
    Allowlist,   // Auto-approve allowlisted tools
    Never,       // Never ask (dangerous!)
}

impl ApprovalWorkflow {
    pub async fn approve_tool(&self, tool: &McpTool) -> Result<bool> {
        match self.mode {
            ApprovalMode::Always => self.prompt_user(tool).await,
            ApprovalMode::FirstUse => self.check_cache_or_prompt(tool).await,
            ApprovalMode::Allowlist => Ok(self.is_allowlisted(tool)),
            ApprovalMode::Never => Ok(true),
        }
    }
}
```

### Audit Logging
```rust
pub struct ToolAuditLog {
    path: PathBuf,
}

impl ToolAuditLog {
    pub async fn log_execution(
        &self,
        tool_name: &str,
        args: &serde_json::Value,
        result: &Result<String>,
        duration: Duration,
    ) -> Result<()> {
        let entry = json!({
            "timestamp": Utc::now().to_rfc3339(),
            "tool": tool_name,
            "args": args,
            "success": result.is_ok(),
            "duration_ms": duration.as_millis(),
            "error": result.as_ref().err().map(|e| e.to_string()),
        });

        // Append to log file
        self.append_entry(&entry).await
    }
}
```

---

## 📚 Resources & References

### Official Documentation
- **MCP Spec**: https://spec.modelcontextprotocol.io/
- **rmcp Crate**: https://docs.rs/rmcp/
- **Rig Framework**: https://rig.rs/

### Example Servers
- **Git Server**: https://github.com/modelcontextprotocol/servers/tree/main/src/git
- **Filesystem Server**: https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem
- **PostgreSQL Server**: https://github.com/modelcontextprotocol/servers/tree/main/src/postgres

### Related Projects
- **Claude Desktop MCP**: https://github.com/anthropics/anthropic-quickstarts/tree/main/mcp
- **OpenAI MCP Bridge**: https://github.com/openai/mcp-bridge

---

## ✅ Next Steps

1. **Immediate** (This Week)
   - ✅ Clone and study rig-integration example (DONE)
   - ✅ Document architecture and patterns (DONE)
   - 🔲 Create `ggen-mcp` crate structure
   - 🔲 Implement basic `McpCoordinator`

2. **Short-term** (Next 2 Weeks)
   - 🔲 Add MCP support to `ggen-ai`
   - 🔲 Implement tool-aware LLM completions
   - 🔲 Create `ggen ai chat` command
   - 🔲 Write comprehensive tests

3. **Medium-term** (Next Month)
   - 🔲 Add semantic tool routing
   - 🔲 Implement approval workflows
   - 🔲 Add audit logging
   - 🔲 Create example configurations

4. **Long-term** (Next Quarter)
   - 🔲 Support all rmcp transports
   - 🔲 Create custom ggen MCP servers
   - 🔲 Build tool marketplace
   - 🔲 Add enterprise features

---

**Conclusion**: The rig-integration example provides an excellent blueprint for integrating MCP into ggen. By following the adapter pattern, multi-server coordination, and vector-based tool routing, ggen can provide a production-ready MCP integration that leverages its existing AI capabilities while adding powerful extensibility through the Model Context Protocol.
