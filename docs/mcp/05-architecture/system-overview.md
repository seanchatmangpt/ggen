# MCP System Architecture

## High-Level Overview

The ggen MCP implementation is a **three-layer bridge** that connects LLM clients to ggen's code generation core:

```
┌─────────────────────────────────────────────────────────────────┐
│                        LLM Client Layer                         │
│                    (Claude, ChatGPT, etc.)                     │
└─────────────────────────────┬───────────────────────────────────┘
                              │ JSON-RPC 2.0
┌─────────────────────────────▼───────────────────────────────────┐
│                       MCP Protocol Layer                        │
│              (Stdio/HTTP Transport + 16 Tools)                  │
└─────────────────────────────┬───────────────────────────────────┘
                              │ A2A Protocol
┌─────────────────────────────▼───────────────────────────────────┐
│                         Bridge Layer                            │
│        (AgentToToolAdapter ↔ MessageRouter ↔ ToolToAgent)      │
└─────────────────────────────┬───────────────────────────────────┘
                              │ A2A Messages
┌─────────────────────────────▼───────────────────────────────────┐
│                       Core Services Layer                       │
│           (Code Gen, Validation, SPARQL, YAWL)                 │
└─────────────────────────────────────────────────────────────────┘
```

## Components by Layer

### MCP Protocol Layer

| Component | File | Purpose |
|-----------|------|---------|
| GgenMcpServer | `ggen_server.rs` | Main MCP server with tool router |
| ToolRouter | `ggen_server.rs` | Routes tool calls to handlers |
| 16 Tools | `ggen_server.rs` | Core functionality (generate, validate, etc.) |
| 4 Resources | `ggen_server.rs` | Browseable example data |
| 3 Prompts | `ggen_server.rs` | LLM templates for workflows |
| Stdio Transport | `server.rs` | JSON-RPC over stdin/stdout |
| HTTP Transport | `server.rs` | JSON-RPC over HTTP |

### Bridge Layer

| Component | File | Purpose |
|-----------|------|---------|
| AgentToToolAdapter | `adapter.rs` | A2A → LLM format conversion |
| ToolToAgentAdapter | `adapter.rs` | LLM → A2A format conversion |
| MessageRouter | `handlers.rs` | Routes messages by content type |
| 5 Handlers | `handlers.rs` | Text, File, Data, Multipart, Stream |
| BatchProcessor | `handlers.rs` | Concurrent batch processing |
| A2aMessageConverter | `message.rs` | Message format conversion |
| A2aLlmClient | `client.rs` | A2A-L bridge with streaming |

### Core Services Layer

| Service | Crate | Purpose |
|---------|-------|---------|
| Code Generation | ggen-core | μ₁-μ₅ pipeline |
| Validation | ggen-jidoka | Quality gates |
| SPARQL Engine | ggen-ontology-core | Query execution |
| YAWL Bridge | ggen-a2a-mcp | Workflow integration |
| Registry | ggen-a2a-registry | Agent discovery |
| Transport | ggen-transport | Message passing |
| Integration | ggen-integration | 6-stage pipeline |

## Data Flow

### Request Flow (LLM → Core)

```
1. LLM Client sends JSON-RPC request
2. MCP Server parses request
3. ToolRouter routes to appropriate tool
4. ToolToAgentAdapter converts to A2A format
5. MessageRouter selects handler by content type
6. Handler processes request
7. Core service executes (generate/validate/etc.)
8. Response propagates back through adapters
9. MCP Server sends JSON-RPC response
10. LLM Client receives result
```

### Streaming Flow (Long Operations)

```
1. LLM Client calls tool with streaming enabled
2. StreamBuilder creates (StreamSender, MessageStream)
3. Core service processes in chunks
4. StreamSender sends progress updates
5. MessageStream delivers chunks to client
6. Stream closed when complete
```

## Message Types

### A2A Message Types

| Type | Direction | Purpose |
|------|-----------|---------|
| Request | Client → Server | Initiate operation |
| Response | Server → Client | Return result |
| Event | Server → Client | Asynchronous notification |
| Command | Client → Server | Execute command |
| Query | Client → Server | Request information |
| Notification | Either | Broadcast message |

### UnifiedContent Variants

| Variant | Description | Example |
|---------|-------------|---------|
| Text | Plain text content | "Hello, world!" |
| Data | Structured key-value | `{"key": "value"}` |
| File | File reference or bytes | `{"uri": "file:///path"}` |
| Multipart | Nested content parts | `{"parts": [...]}` |
| Stream | Chunked stream data | `{"stream_id": "abc123"}` |

## Transport Comparison

| Transport | Use Case | Pros | Cons |
|-----------|----------|------|------|
| Stdio | Local clients (Claude Desktop) | Simple, secure | Local only |
| HTTP | Remote clients | Network accessible | Requires auth |
| WebSocket | Real-time | Bidirectional | Complex setup |

## Security Model

### Layers of Security

1. **Firewall Stage**: Rate limiting, size validation, schema checks
2. **Origin Validation**: Allowed origins whitelist
3. **Message Validation**: Schema validation on all messages
4. **Session Management**: TTL-based expiration
5. **Health Checks**: Agent health monitoring

### Security Policies

| Policy | Description |
|--------|-------------|
| Rate Limiting | Max requests per time window |
| Size Limits | Max message size (default: 1MB) |
| Origin Allowlist | Whitelist of allowed sources |
| Session TTL | Auto-expire inactive sessions |
| Circuit Breaker | Fail fast on degraded agents |

## Observability

### OpenTelemetry Spans

| Span | Attributes |
|------|------------|
| `mcp.tool.call` | tool_name, duration_ms |
| `ggen.generate` | triples, files_generated |
| `ggen.validate` | errors, warnings |
| `a2a.message` | message_type, from_agent |

### Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `mcp_tools_total` | Counter | Total tool calls |
| `mcp_tool_duration` | Histogram | Tool execution time |
| `a2a_messages_total` | Counter | A2A messages sent |
| `a2a_message_errors` | Counter | Failed messages |

## Related Documentation

- [Component Details](./components/mcp-server.md)
- [Data Flow](./data-flow/request-lifecycle.md)
- [A2A Protocol](./protocols/mcp-a2a-bridge.md)
