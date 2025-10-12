# MCP + Rig Integration Example

This example demonstrates how to integrate Model Context Protocol (MCP) servers with the Rig framework to create AI agents with dynamic tool capabilities.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Your Application                          │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐   │
│  │    main.rs   │  │   chat.rs    │  │  config.rs      │   │
│  │              │  │              │  │                 │   │
│  │  Initialize  │─▶│  CLI Chat    │  │  Load TOML      │   │
│  │  MCP Manager │  │  Streaming   │  │  Configuration  │   │
│  └──────────────┘  └──────────────┘  └─────────────────┘   │
│         │                                                    │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐   │
│  │            mcp_adaptor.rs                            │   │
│  │                                                      │   │
│  │  Adapts MCP Tools → Rig ToolDyn Interface           │   │
│  └──────────────────────────────────────────────────────┘   │
│         │                                                    │
└─────────┼────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────────┐
│                     MCP Manager                              │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐   │
│  │  Stdio MCP   │  │   SSE MCP    │  │ Streamable MCP  │   │
│  │   Servers    │  │   Servers    │  │    Servers      │   │
│  └──────────────┘  └──────────────┘  └─────────────────┘   │
└─────────────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────────┐
│               External MCP Servers                           │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐   │
│  │  Filesystem  │  │   Weather    │  │   Database      │   │
│  │    Tools     │  │    Tools     │  │     Tools       │   │
│  └──────────────┘  └──────────────┘  └─────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Key Components

### 1. **MCP Manager** (`main.rs`)
- Initializes MCP servers from configuration
- Manages server lifecycle (start/stop)
- Provides tool discovery interface

### 2. **MCP Adaptor** (`mcp_adaptor.rs`)
- Bridges MCP protocol with Rig's ToolDyn interface
- Handles JSON schema conversion
- Manages tool execution and error handling

### 3. **Chat Interface** (`chat.rs`)
- CLI chatbot with streaming responses
- Colored output for better readability
- Automatic tool call handling
- History management

### 4. **Configuration** (`config.rs`, `config.toml`)
- TOML-based configuration
- Support for multiple AI providers (OpenAI, DeepSeek, Cohere)
- MCP server definitions (stdio, SSE, streamable)

## MCP Server Types

### Stdio Servers
Run as child processes with stdin/stdout communication:
```toml
[[mcp_servers]]
name = "filesystem"
transport = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
```

### SSE Servers
HTTP-based servers with Server-Sent Events:
```toml
[[mcp_servers]]
name = "weather"
transport = "sse"
url = "http://localhost:3000/sse"
```

### Streamable Servers
Custom streamable transport:
```toml
[[mcp_servers]]
name = "custom"
transport = "streamable"
# Custom configuration
```

## Quick Start

### 1. Generate Project
```bash
./generate-project.sh my-mcp-agent
cd my-mcp-agent
```

### 2. Configure API Keys
Edit `config.toml`:
```toml
[providers.deepseek]
api_key = "your-api-key-here"
```

### 3. Add MCP Servers
```toml
[[mcp_servers]]
name = "filesystem"
transport = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
```

### 4. Run
```bash
cargo run
```

## Usage Examples

### Basic Chat
```
You: What files are in /tmp?
Assistant: [Uses filesystem MCP tool to list files]
```

### Multi-Step Reasoning
```
You: Create a file called notes.txt and write "Hello World" to it
Assistant: [Uses filesystem tools to create and write file]
```

### Tool Chaining
```
You: Get the weather for New York and save it to weather.txt
Assistant: [Uses weather MCP tool, then filesystem tool]
```

## Configuration Variables

Templates support these variables:

- `project_name`: Project name (default: "mcp-rig-agent")
- `deepseek_support`: Enable DeepSeek provider (default: true)
- `cohere_support`: Enable Cohere provider (default: false)
- `openai_support`: Enable OpenAI provider (default: false)
- `log_dir`: Directory for logs (default: "logs")
- `mcp_servers`: List of MCP server configurations

## Template Structure

```
templates/
├── main-rs.tmpl           # Application entry point
├── chat-rs.tmpl           # CLI chatbot implementation
├── config-rs.tmpl         # Configuration loader
├── config-mcp-rs.tmpl     # MCP server configuration
├── mcp-adaptor-rs.tmpl    # MCP → Rig adaptor
├── cargo-toml.tmpl        # Dependencies
└── config-toml.tmpl       # Runtime configuration
```

## Dependencies

- **rig-core**: AI agent framework
- **rmcp**: Model Context Protocol implementation
- **tokio**: Async runtime
- **serde**: Serialization/deserialization
- **colored**: Terminal output formatting
- **anyhow**: Error handling

## Advanced Features

### Custom Tool Definitions
Add custom tools by implementing the MCP protocol:
```rust
let tool = Tool {
    name: "my_tool".to_string(),
    description: "My custom tool".to_string(),
    input_schema: json!({
        "type": "object",
        "properties": {
            "input": {"type": "string"}
        }
    })
};
```

### Streaming Responses
The chat interface automatically handles streaming:
```rust
let mut stream = agent
    .chat(&prompt, session)
    .await?;

while let Some(chunk) = stream.next().await {
    print!("{}", chunk.content);
}
```

### Error Handling
Comprehensive error handling with context:
```rust
manager.initialize()
    .await
    .context("Failed to initialize MCP servers")?;
```

## Troubleshooting

### MCP Server Not Found
- Ensure the command is in your PATH
- Check server logs in the configured log directory
- Verify server configuration in config.toml

### API Key Issues
- Set environment variables: `DEEPSEEK_API_KEY`, etc.
- Or configure in config.toml
- Check provider-specific documentation

### Tool Execution Failures
- Check MCP server logs
- Verify tool parameter schemas
- Ensure proper permissions for file/system operations

## Examples

See `example-servers.toml` for pre-configured MCP server examples:
- Filesystem operations
- Weather data
- Time/date utilities
- Custom servers

## Resources

- [Model Context Protocol](https://modelcontextprotocol.io/)
- [Rig Framework](https://github.com/0xPlaygrounds/rig)
- [MCP Rust SDK](https://github.com/modelcontextprotocol/rust-sdk)
- [Available MCP Servers](https://github.com/modelcontextprotocol/servers)

## License

MIT
