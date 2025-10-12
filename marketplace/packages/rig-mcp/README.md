# Rig + MCP Integration

**Exact code from [official MCP Rust SDK example](https://github.com/modelcontextprotocol/rust-sdk/tree/main/examples/rig-integration)**

This library provides the battle-tested integration between [Rig LLM framework](https://github.com/0xPlaygrounds/rig) and [Model Context Protocol (MCP)](https://modelcontextprotocol.io/).

## Features

- ✅ **20+ LLM Providers**: OpenAI, Anthropic, Cohere, Deepseek, Gemini, Ollama, etc.
- ✅ **Dynamic MCP Tool Loading**: Automatically discover and use MCP tools
- ✅ **Vector-Based Tool Selection**: Intelligent tool selection using embeddings
- ✅ **Multi-Transport Support**: stdio, SSE, and HTTP transports
- ✅ **Production Ready**: Extracted from official SDK example

## Installation

```bash
# Via ggen marketplace (coming soon)
ggen market install rig-mcp

# Or add to Cargo.toml
[dependencies]
rig-mcp-integration = { path = "../marketplace/packages/rig-mcp" }
# Or from crates.io (when published):
# rig-mcp-integration = "0.1.0"
```

## Usage (Exact Vendor Pattern)

This library exports the exact code from the vendor example. Use it exactly as shown:

```rust
use rig_mcp_integration::{Config, McpManager, get_tool_set};
use rig::{
    client::{CompletionClient, ProviderClient},
    embeddings::EmbeddingsBuilder,
    providers::{cohere, deepseek},
    vector_store::in_memory_store::InMemoryVectorStore,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // 1. Load configuration
    let config = Config::retrieve("config.toml").await?;

    // 2. Initialize providers
    let deepseek_client = {
        if let Some(key) = config.deepseek_key {
            deepseek::Client::new(&key)
        } else {
            deepseek::Client::from_env()
        }
    };
    let cohere_client = {
        if let Some(key) = config.cohere_key {
            cohere::Client::new(&key)
        } else {
            cohere::Client::from_env()
        }
    };

    // 3. Create MCP manager and load tools
    let mcp_manager = config.mcp.create_manager().await?;
    let tool_set = mcp_manager.get_tool_set().await?;

    // 4. Build embeddings for tool selection
    let embedding_model = cohere_client.embedding_model(
        cohere::EMBED_MULTILINGUAL_V3,
        "search_document"
    );
    let embeddings = EmbeddingsBuilder::new(embedding_model.clone())
        .documents(tool_set.schemas()?)?
        .build()
        .await?;

    // 5. Create vector store for intelligent tool selection
    let store = InMemoryVectorStore::from_documents_with_id_f(embeddings, |f| {
        tracing::info!("Indexed tool: {}", f.name);
        f.name.clone()
    });
    let index = store.index(embedding_model);

    // 6. Build agent with dynamic tools
    let agent = deepseek_client
        .agent(deepseek::DEEPSEEK_CHAT)
        .dynamic_tools(4, index, tool_set)  // Top 4 relevant tools selected
        .build();

    // 7. Use the agent
    let response = agent.prompt("What tools do I have?").await?;
    println!("{}", response);

    Ok(())
}
```

## Configuration

Create a `config.toml` file:

```toml
# API Keys (optional - uses environment variables if not set)
deepseek_key = "sk-..."
cohere_key = "..."

[mcp]
[[mcp.server]]
name = "filesystem"
protocol = "stdio"
command = "npx"
args = ["@modelcontextprotocol/server-filesystem", "/allowed/path"]

[[mcp.server]]
name = "git"
protocol = "stdio"
command = "uvx"
args = ["mcp-server-git"]
```

See [config.toml.example](config.toml.example) for all options.

## Supported Providers

- **OpenAI**: GPT-4, GPT-4 Turbo, GPT-3.5
- **Anthropic**: Claude 3 (Opus, Sonnet, Haiku)
- **Cohere**: Command, Command R/R+
- **Deepseek**: Deepseek Chat
- **Google**: Gemini Pro, Gemini Ultra
- **Ollama**: Local models
- **And 15+ more...**

## MCP Transport Types

### Stdio (Child Process)
```toml
[[mcp.server]]
protocol = "stdio"
command = "npx"
args = ["@modelcontextprotocol/server-github"]
```

### Server-Sent Events
```toml
[[mcp.server]]
protocol = "sse"
url = "http://localhost:3000/sse"
```

### HTTP Streamable
```toml
[[mcp.server]]
protocol = "streamable"
url = "http://localhost:3000/mcp"
```

## Features

### Dynamic Tool Loading
Tools from MCP servers are automatically discovered and made available to agents:
```rust
let tools = mcp_manager.get_tool_set().await?;
// All MCP tools now accessible
```

### Vector-Based Tool Selection
Uses embeddings for intelligent tool selection:
```rust
let agent = client
    .agent("gpt-4")
    .await?
    .dynamic_tools(4, index, tool_set)  // Top 4 relevant tools selected
    .build();
```

### Multi-Server Support
Connect to multiple MCP servers simultaneously:
```toml
[[mcp.server]]
name = "filesystem"
# ...

[[mcp.server]]
name = "github"
# ...

[[mcp.server]]
name = "database"
# ...
```

## Examples

See the [examples/](examples/) directory for:
- Basic usage
- Multi-provider setup
- Custom tool creation
- Streaming responses
- Error handling

## Architecture

```
┌─────────────────────────────┐
│   Your Rust Application     │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│    rig-mcp-integration      │
│  (This Library)             │
│                             │
│  ┌─────────────────────┐   │
│  │  RigMcpClient       │   │
│  └─────────┬───────────┘   │
│            │                │
│  ┌─────────▼───────────┐   │
│  │  McpManager         │   │
│  │  (Multiple Servers) │   │
│  └─────────┬───────────┘   │
└────────────┼───────────────┘
             │
    ┌────────┴────────┐
    │                 │
┌───▼────┐    ┌──────▼─────┐
│MCP     │    │MCP         │
│Server 1│    │Server 2... │
└────────┘    └────────────┘
```

## Testing

```bash
cargo test
```

## License

MIT

## Credits

- Based on [official MCP Rust SDK example](https://github.com/modelcontextprotocol/rust-sdk/tree/main/examples/rig-integration)
- Built with [Rig framework](https://github.com/0xPlaygrounds/rig)
- Uses [MCP Rust SDK](https://github.com/modelcontextprotocol/rust-sdk)
