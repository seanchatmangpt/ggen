# Rig MCP Integration

Production-ready Rig LLM framework + MCP protocol integration for ggen.

## Features

- **Multi-provider LLM support** (OpenAI, Anthropic, Cohere, Deepseek, Gemini, Ollama, 20+)
- **Dynamic MCP tool loading** with vector-based selection
- **Multi-transport MCP support** (stdio, SSE, HTTP)
- **Production-ready patterns** from official MCP Rust SDK
- **Embedding-based intelligent tool selection**
- **Async/streaming support**

## Installation

```bash
# Add to your project
ggen market add rig-mcp-integration

# Or install manually
cargo add rig-mcp-integration
```

## Quick Start

```rust
use rig_mcp_integration::prelude::*;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Load configuration
    let config = Config::from_file("config.toml")?;

    // Create client
    let mut client = RigMcpClient::new(config).await?;

    // Create agent with MCP tools
    let agent = client.agent("gpt-4").await?.build();

    // Send prompt with MCP tool context
    let response = agent.prompt("Analyze this codebase and suggest improvements").await?;

    println!("{}", response);

    Ok(())
}
```

## Configuration

Create a `config.toml`:

```toml
[providers.openai]
model = "gpt-4"
api_key = "your-openai-api-key"

[providers.anthropic]
model = "claude-3-sonnet"
api_key = "your-anthropic-api-key"

[embeddings]
provider = "openai"
model = "text-embedding-ada-002"
api_key = "your-openai-api-key"

[agent]
max_tokens = 4000
temperature = 0.7
```

## Supported Providers

| Provider | Models | Status |
|----------|--------|--------|
| OpenAI | GPT-4, GPT-3.5-turbo | ✅ |
| Anthropic | Claude-3, Claude-2 | ✅ |
| Cohere | Command, Command-R | ✅ |
| Ollama | Llama-2, Code Llama | ✅ |
| DeepSeek | DeepSeek Chat | ✅ |
| Gemini | Gemini Pro | ✅ |

## MCP Integration

The library automatically discovers and loads MCP tools from connected servers:

```rust
// MCP server configuration
let mcp_server = ServerConfig {
    name: "filesystem".to_string(),
    transport: Transport::Stdio {
        command: "node".to_string(),
        args: vec!["mcp-server-filesystem.js".to_string()],
    },
};

// Tools are automatically available in agent context
let response = agent.prompt("List files in the current directory").await?;
```

## Examples

Run the example:

```bash
# Build the package first
cargo build --release

# Run the example
cargo run --bin rig-mcp-example
```

## Testing

```bash
cargo test
cargo test --release
```

## Performance

- **Async-first** design for high concurrency
- **Connection pooling** for LLM providers
- **Intelligent tool selection** based on context
- **Streaming support** for real-time responses

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Run the full test suite
5. Submit a pull request

## License

MIT License - see LICENSE file for details.
