# API Reference: ggen-a2a-mcp

Primary MCP server implementation with 16 tools, adapters, and message routing.

## Structs

### GgenMcpServer

**Purpose**: Main MCP server that implements ServerHandler with all tools, resources, and prompts.

**Fields** (private):
- `tool_router: ToolRouter<GgenMcpServer>` - Routes tool calls to methods
- `examples_dir: PathBuf` - Path to bundled examples

**Constructor**:
```rust
pub fn new() -> Self
```

**Key Methods**:
- `serve<T, E, A>(self, transport: T)` - Serve over any RMCP transport
- `get_info()` - Returns MCP server info (tools, resources, prompts)

**Example**:
```rust
let server = GgenMcpServer::new();
server.serve(rmcp::transport::stdio()).await?;
```

---

### AgentToToolAdapter

**Purpose**: Converts A2A agent capabilities into LLM tool format (A2A → LLM).

**Fields**:
- `converter: Arc<A2aMessageConverter>` - Message format converter

**Key Methods**:
```rust
pub fn generate_tools(
    &self,
    agent_name: &str,
    capabilities: &[String]
) -> Vec<Tool>

pub fn tool_call_to_message(
    &self,
    call: &ToolCall,
    agent_id: &str
) -> ConvergedMessage

pub fn message_to_tool_response(
    &self,
    message: &ConvergedMessage
) -> ToolResponse
```

**Example**:
```rust
let adapter = AgentToToolAdapter::new(converter);
let tools = adapter.generate_tools("codegen", &["generate", "validate"]);
```

---

### ToolToAgentAdapter

**Purpose**: Converts LLM tools into A2A agent format (LLM → A2A).

**Fields**:
- `tools: HashMap<String, Tool>` - Registered tools
- `agent_name: String` - Agent identifier
- `agent_description: String` - Agent description

**Key Methods**:
```rust
pub fn add_tool(&mut self, tool: Tool)
pub fn agent_card(&self) -> A2aAgentCard
pub fn find_tool(&self, name: &str) -> Option<&Tool>
```

**Example**:
```rust
let mut adapter = ToolToAgentAdapter::new("codegen", "Code generation");
adapter.add_tool(generate_tool);
let card = adapter.agent_card();
```

---

### MessageRouter

**Purpose**: Routes ConvergedMessage to appropriate handler based on content type.

**Fields**:
- `factory: HandlerFactory` - Handler registry
- `default_handler: Option<Arc<dyn MessageHandler>>` - Fallback handler

**Key Methods**:
```rust
pub fn with_defaults() -> Self  // Pre-registers all 5 handlers
pub fn register<H>(&mut self, handler: H) where H: MessageHandler
pub fn route(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage>
```

**Example**:
```rust
let router = MessageRouter::with_defaults();
let result = router.route(message).await?;
```

---

### A2aLlmClient

**Purpose**: A2A-L bridge client that connects LLM providers via ggen-ai.

**Fields** (private):
- `llm_client: GenAiClient` - Underlying LLM client
- `adapter: ToolToAgentAdapter` - LLM to A2A adapter
- `health: Arc<RwLock<ConnectionHealth>>` - Connection status

**Key Methods**:
```rust
pub async fn process_message(&self, message: ConvergedMessage) -> ConvergedMessage
pub async fn call_tool(&self, call: ToolCall) -> ToolExecutionResult
pub async fn stream_response(&self, prompt: &str) -> Stream<StreamingChunk>
```

**Example**:
```rust
let client = A2aLlmClient::new(config)?;
let response = client.process_message(message).await?;
```

---

## Traits

### MessageHandler

**Purpose**: Trait for handling ConvergedMessage instances.

**Methods**:
```rust
async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage>
fn can_handle(&self, message: &ConvergedMessage) -> bool
fn priority(&self) -> HandlerPriority
```

**Implementors**:
- `TextContentHandler` - Handles Direct, Task, Query messages
- `FileContentHandler` - Handles file content with size validation
- `DataContentHandler` - Handles Query, Command, Sync messages
- `MultipartHandler` - Handles multipart content (recursive)
- `StreamHandler` - Handles streaming content

**Example**:
```rust
#[async_trait]
impl MessageHandler for MyHandler {
    async fn handle(&self, msg: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        // Handle message
        Ok(response)
    }

    fn can_handle(&self, msg: &ConvergedMessage) -> bool {
        matches!(msg.payload.content, UnifiedContent::Text { .. })
    }

    fn priority(&self) -> HandlerPriority {
        HandlerPriority::Normal
    }
}
```

---

## Enums

### UnifiedContent

**Purpose**: Tagged enum representing different content types in ConvergedMessage.

**Variants**:
```rust
pub enum UnifiedContent {
    Text { content: String, metadata: HashMap<String, String> },
    File { file: UnifiedFileContent, metadata: HashMap<String, String> },
    Data { data: Map<String, Value>, schema: Option<String> },
    Multipart { parts: Vec<UnifiedContent>, boundary: String },
    Stream { stream_id: String, chunk_size: usize, metadata: HashMap<String, String> },
}
```

**Usage**:
```rust
match message.payload.content {
    UnifiedContent::Text { content, .. } => {
        println!("Text: {}", content);
    }
    UnifiedContent::Data { data, .. } => {
        println!("Data: {:?}", data);
    }
    _ => {}
}
```

---

## Constants

### OTEL Attributes

**File**: `src/ggen_server.rs`

| Attribute | Value |
|-----------|-------|
| `llm.model` | Model identifier (e.g., "groq::openai/gpt-oss-20b") |
| `llm.prompt_tokens` | Input token count |
| `llm.completion_tokens` | Output token count |
| `llm.total_tokens` | Total tokens used |
| `mcp.tool.name` | Tool name (e.g., "generate") |
| `mcp.tool.duration_ms` | Tool execution time |

**Usage**:
```rust
use opentelemetry::trace::{TraceContextExt, Tracer};

let span = tracer.start("generate");
span.set_attribute("llm.model", "groq::gpt-oss-20b");
span.set_attribute("llm.total_tokens", total_tokens);
```

---

## See Also

- [GgenMcpServer Source](../../../../../crates/ggen-a2a-mcp/src/ggen_server.rs)
- [Handler Implementations](../../../../../crates/ggen-a2a-mcp/src/handlers.rs)
- [Adapter Implementation](../../../../../crates/ggen-a2a-mcp/src/adapter.rs)
