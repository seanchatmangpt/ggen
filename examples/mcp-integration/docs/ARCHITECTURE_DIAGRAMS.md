# MCP + Rig Integration - Architecture Diagrams

## System Architecture Overview

### High-Level Component Diagram

```
┌──────────────────────────────────────────────────────────────────────┐
│                        User Interaction Layer                         │
│                                                                        │
│   ┌──────────────────────────────────────────────────────────────┐  │
│   │                    CLI Chat Interface                         │  │
│   │  • Colored output     • Streaming responses                  │  │
│   │  • Tool visualization • Error handling                       │  │
│   └─────────────────────┬────────────────────────────────────────┘  │
└─────────────────────────┼───────────────────────────────────────────┘
                          │
                          ↓
┌──────────────────────────────────────────────────────────────────────┐
│                       Application Core Layer                          │
│                                                                        │
│   ┌────────────────────────────────────────────────────────────┐    │
│   │                     Main Application                        │    │
│   │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  │    │
│   │  │ Config   │  │   LLM    │  │   MCP    │  │   Chat   │  │    │
│   │  │ Loading  │→ │  Clients │→ │ Manager  │→ │  Agent   │  │    │
│   │  └──────────┘  └──────────┘  └──────────┘  └──────────┘  │    │
│   └────────────────────────────────────────────────────────────┘    │
│                                                                        │
│   ┌────────────────────────────────────────────────────────────┐    │
│   │              Configuration Management                       │    │
│   │  • TOML parsing        • Provider config                   │    │
│   │  • MCP server setup    • Environment variables             │    │
│   └────────────────────────────────────────────────────────────┘    │
└─────────────────────────┬────────────────────────────────────────────┘
                          │
                          ↓
┌──────────────────────────────────────────────────────────────────────┐
│                      Integration Layer                                │
│                                                                        │
│   ┌─────────────────────────────────────────────────────────┐       │
│   │                  MCP Tool Adaptor                        │       │
│   │  ┌──────────────┐         ┌──────────────┐             │       │
│   │  │ McpTool      │    →    │ RigTool      │             │       │
│   │  │ (External)   │         │ (Internal)   │             │       │
│   │  └──────────────┘         └──────────────┘             │       │
│   │                                                          │       │
│   │  ┌──────────────────────────────────────────────┐      │       │
│   │  │ Tool Embedding for RAG                       │      │       │
│   │  │  Tool Name + Description → Vector → Search   │      │       │
│   │  └──────────────────────────────────────────────┘      │       │
│   └─────────────────────────────────────────────────────────┘       │
│                                                                        │
│   ┌─────────────────────────────────────────────────────────┐       │
│   │                  MCP Manager                             │       │
│   │  • Multi-server coordination                            │       │
│   │  • Concurrent startup                                   │       │
│   │  • Tool aggregation                                     │       │
│   └─────────────────────────────────────────────────────────┘       │
└─────────────────────────┬────────────────────────────────────────────┘
                          │
                          ↓
┌──────────────────────────────────────────────────────────────────────┐
│                      External Services Layer                          │
│                                                                        │
│   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │
│   │ MCP Server 1 │  │ MCP Server 2 │  │ MCP Server N │             │
│   │  (stdio)     │  │    (sse)     │  │(streamable)  │             │
│   └──────────────┘  └──────────────┘  └──────────────┘             │
│                                                                        │
│   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │
│   │  Deepseek    │  │   Cohere     │  │   OpenAI     │             │
│   │ Completion   │  │  Embeddings  │  │ Completion   │             │
│   └──────────────┘  └──────────────┘  └──────────────┘             │
└──────────────────────────────────────────────────────────────────────┘
```

## Data Flow Architecture

### Request Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                         User Query Flow                              │
└─────────────────────────────────────────────────────────────────────┘

User Input
    │
    ↓
┌───────────────┐
│  Chat Module  │  Receives input
└───────┬───────┘
        │
        ↓
┌───────────────┐
│     Agent     │  Process query
└───────┬───────┘
        │
        ├──────────────────────────────────┐
        │                                  │
        ↓                                  ↓
┌───────────────┐                  ┌──────────────┐
│ Vector Store  │                  │  LLM Model   │
│  RAG Search   │                  │  Generation  │
└───────┬───────┘                  └──────┬───────┘
        │                                  │
        │ Top-K Tools                      │ Response +
        │                                  │ Tool Calls
        ↓                                  │
┌───────────────┐                          │
│ Tool Executor │◄─────────────────────────┘
└───────┬───────┘
        │
        ├──────────┬──────────┬──────────┐
        ↓          ↓          ↓          ↓
    ┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐
    │ Server │ │ Server │ │ Server │ │ Server │
    │   1    │ │   2    │ │   3    │ │   N    │
    └────────┘ └────────┘ └────────┘ └────────┘
        │          │          │          │
        └──────────┴──────────┴──────────┘
                     │
                     ↓
            ┌─────────────────┐
            │  Tool Results   │
            │  Aggregation    │
            └────────┬────────┘
                     │
                     ↓
            ┌─────────────────┐
            │  Format & Send  │
            │  to User        │
            └─────────────────┘
```

### Initialization Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                      System Initialization                           │
└─────────────────────────────────────────────────────────────────────┘

main.rs starts
    │
    ├─→ Load config.toml ───────────────┐
    │                                    │
    ├─→ Init Logging ───────────────────┤
    │   • Rolling file appender          │
    │   • Level: INFO/DEBUG/TRACE        │
    │                                    │
    ├─→ Create LLM Clients ─────────────┤
    │   │                                │
    │   ├─→ Deepseek (completion)       │
    │   ├─→ Cohere (embedding)          │
    │   └─→ OpenAI (optional)           │
    │                                    │
    ├─→ Start MCP Manager ──────────────┤  ◄── Parallel
    │   │                                │
    │   ├─→ Server 1 (stdio)            │
    │   ├─→ Server 2 (sse)    ┌─────────┘
    │   └─→ Server 3 (stream) │
    │                          │
    ├─→ Discover Tools ────────┴────────┐
    │   • List from each server         │
    │   • Create adaptors               │
    │   • Build tool set                │
    │                                    │
    ├─→ Generate Embeddings ────────────┤
    │   • Embed tool descriptions       │
    │   • Build vector store            │
    │                                    │
    ├─→ Construct Agent ────────────────┤
    │   • Attach completion model       │
    │   • Configure dynamic tools       │
    │   • Set max tools                 │
    │                                    │
    └─→ Launch Chat Interface ──────────┘
        • Interactive CLI
        • Streaming enabled
        • Ready for queries
```

## Module Architecture

### Configuration Module

```
┌─────────────────────────────────────────────────────────────────────┐
│                      config.rs + config/mcp.rs                       │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────┐
│      Config Struct      │
│  ┌───────────────────┐  │
│  │ mcp: McpConfig    │  │
│  │ deepseek_key      │  │
│  │ cohere_key        │  │
│  │ openai_key        │  │
│  └───────────────────┘  │
└───────────┬─────────────┘
            │
            ↓
┌─────────────────────────────────────────────────┐
│              McpConfig                          │
│  ┌───────────────────────────────────────────┐ │
│  │  server: Vec<McpServerConfig>            │ │
│  │                                           │ │
│  │  ┌────────────────────────────────────┐  │ │
│  │  │  McpServerConfig                   │  │ │
│  │  │  • name: String                    │  │ │
│  │  │  • transport: TransportConfig      │  │ │
│  │  └────────────────────────────────────┘  │ │
│  └───────────────────────────────────────────┘ │
└───────────────────┬─────────────────────────────┘
                    │
                    ↓
┌─────────────────────────────────────────────────────┐
│          McpServerTransportConfig                   │
│  ┌─────────────────────────────────────────────┐   │
│  │  Stdio { command, args, envs }             │   │
│  │  Sse { url }                               │   │
│  │  Streamable { url, envs }                  │   │
│  └─────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

### MCP Adaptor Module

```
┌─────────────────────────────────────────────────────────────────────┐
│                        mcp_adaptor.rs                                │
└─────────────────────────────────────────────────────────────────────┘

┌──────────────────────────┐
│    McpToolAdaptor        │
│  ┌────────────────────┐  │
│  │ tool: McpTool      │  │
│  │ server: ServerSink │  │
│  └────────────────────┘  │
└───────────┬──────────────┘
            │
            ├──────────────────────────────────┐
            │                                  │
            ↓                                  ↓
┌─────────────────────────┐      ┌─────────────────────────┐
│    RigTool Trait        │      │  ToolEmbeddingDyn       │
│  ┌───────────────────┐  │      │  ┌───────────────────┐  │
│  │ definition()      │  │      │  │ embed()           │  │
│  │ → ToolDefinition  │  │      │  │ → Vec<f32>        │  │
│  │                   │  │      │  │                   │  │
│  │ call(args)        │  │      │  │ Uses Cohere       │  │
│  │ → String          │  │      │  │ embed-english-v3  │  │
│  └───────────────────┘  │      │  └───────────────────┘  │
└─────────────────────────┘      └─────────────────────────┘

┌──────────────────────────────────────────────────────────┐
│                    McpManager                            │
│  ┌────────────────────────────────────────────────────┐  │
│  │ clients: HashMap<String, RunningService>          │  │
│  │                                                    │  │
│  │ Methods:                                           │  │
│  │  • get_all_tools() → ToolSet                      │  │
│  │  • list_servers() → Vec<String>                   │  │
│  │  • get_server(name) → Option<ServerSink>          │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
```

### Chat Module

```
┌─────────────────────────────────────────────────────────────────────┐
│                           chat.rs                                    │
└─────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────┐
│      cli_chatbot(agent)               │
│  ┌─────────────────────────────────┐  │
│  │                                 │  │
│  │  Loop {                         │  │
│  │    • Read user input            │  │
│  │    • Check for commands         │  │
│  │    • Send to agent              │  │
│  │    • Stream response            │  │
│  │  }                              │  │
│  │                                 │  │
│  └─────────────────────────────────┘  │
└───────────────┬───────────────────────┘
                │
                ↓
┌────────────────────────────────────────┐
│      Response Streaming                │
│  ┌──────────────────────────────────┐  │
│  │  ChatEvent::Response             │  │
│  │    → Print green text            │  │
│  │                                  │  │
│  │  ChatEvent::ToolCall             │  │
│  │    → Print yellow tool info      │  │
│  │                                  │  │
│  │  ChatEvent::ToolResult           │  │
│  │    → Print green result          │  │
│  │                                  │  │
│  │  ChatEvent::Error                │  │
│  │    → Print red error             │  │
│  └──────────────────────────────────┘  │
└────────────────────────────────────────┘
```

## Sequence Diagrams

### Tool Discovery Sequence

```
User      Main      Config     McpMgr    McpServer   Adaptor    VectorStore
 │          │          │          │          │          │          │
 │ start ───┼─→ load ──┼─→ parse │          │          │          │
 │          │          │          │          │          │          │
 │          │←─────────┼─── config          │          │          │
 │          │          │          │          │          │          │
 │          ├──────────┼───────→ create     │          │          │
 │          │          │          │          │          │          │
 │          │          │          ├─────→ start         │          │
 │          │          │          │          │          │          │
 │          │          │          │←──────── ready      │          │
 │          │          │          │          │          │          │
 │          │          │          ├──────────┼─→ list   │          │
 │          │          │          │          │  tools   │          │
 │          │          │          │          │          │          │
 │          │          │          │←─────────┼── tools  │          │
 │          │          │          │          │          │          │
 │          │          │          ├──────────┼─────→ create        │
 │          │          │          │          │    adaptors         │
 │          │          │          │          │          │          │
 │          │          │          │          │          ├──────→ embed
 │          │          │          │          │          │          │
 │          │          │          │          │          │←───── vectors
 │          │          │          │          │          │          │
 │          │          │          │←─────────┼──────────┼── ready  │
 │          │          │          │          │          │          │
 │          │←─────────┼──────────┴── tools  │          │          │
 │          │          │          aggregated │          │          │
 │          │          │          │          │          │          │
 │←─────────┴────── agent ready   │          │          │          │
 │                   with tools    │          │          │          │
```

### Chat Query Sequence

```
User     Chat      Agent     Vector    Adaptor   Server    LLM
 │         │          │        Store      │         │        │
 │ input ──┼─→ send ──┼─→ embed          │         │        │
 │         │          │        query      │         │        │
 │         │          │          │        │         │        │
 │         │          │←──────── top-k    │         │        │
 │         │          │          tools    │         │        │
 │         │          │          │        │         │        │
 │         │          ├──────────┼────────┼────────→│        │
 │         │          │          │        │      generate    │
 │         │          │          │        │         │        │
 │         │          │←─────────┼────────┼─────────┴── response
 │         │          │          │     + tool_calls │        │
 │         │          │          │        │         │        │
 │         │          ├──────────┼─→ call │         │        │
 │         │          │          │   tool │         │        │
 │         │          │          │        │         │        │
 │         │          │          │        ├────→ execute     │
 │         │          │          │        │      tool        │
 │         │          │          │        │         │        │
 │         │          │          │        │←─────── result   │
 │         │          │          │        │         │        │
 │         │          │←─────────┼────────┴── result│        │
 │         │          │          │        aggregated│        │
 │         │          │          │        │         │        │
 │         │←────── stream       │        │         │        │
 │         │        response     │        │         │        │
 │         │        + results    │        │         │        │
 │         │          │          │        │         │        │
 │←────────┴── display           │        │         │        │
 │       formatted   │          │        │         │        │
```

## Deployment Architecture

### Local Development

```
┌─────────────────────────────────────────────────────────────┐
│                    Developer Machine                         │
│                                                              │
│  ┌────────────────────────────────────────────────────┐    │
│  │              MCP + Rig Application                  │    │
│  │  Port: N/A (CLI)                                    │    │
│  └────────┬───────────────────────┬───────────────────┘    │
│           │                       │                         │
│           ↓                       ↓                         │
│  ┌─────────────────┐    ┌─────────────────┐               │
│  │  MCP Servers    │    │   LLM APIs      │               │
│  │  • stdio (npx)  │    │  • Deepseek     │               │
│  │  • local port   │    │  • Cohere       │               │
│  └─────────────────┘    │  • OpenAI       │               │
│                          └────────┬────────┘               │
└─────────────────────────────────────┼──────────────────────┘
                                      │
                                      ↓
                              ┌───────────────┐
                              │   Internet    │
                              └───────────────┘
```

### Production Deployment

```
┌─────────────────────────────────────────────────────────────┐
│                    Cloud Environment                         │
│                                                              │
│  ┌────────────────────────────────────────────────────┐    │
│  │         Load Balancer (Optional)                   │    │
│  └────────────────┬───────────────────────────────────┘    │
│                   │                                         │
│           ┌───────┴────────┬─────────────┐                 │
│           ↓                ↓             ↓                 │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐       │
│  │ Instance 1  │  │ Instance 2  │  │ Instance N  │       │
│  │             │  │             │  │             │       │
│  │  App + MCP  │  │  App + MCP  │  │  App + MCP  │       │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘       │
│         │                │                 │               │
│         └────────────────┴─────────────────┘               │
│                          │                                 │
│                          ↓                                 │
│  ┌──────────────────────────────────────────────────┐    │
│  │           Shared Services                         │    │
│  │  ┌──────────────┐  ┌──────────────┐             │    │
│  │  │   Logs       │  │   Metrics    │             │    │
│  │  │ (CloudWatch) │  │ (Prometheus) │             │    │
│  │  └──────────────┘  └──────────────┘             │    │
│  └──────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Template Generation Flow

```
┌─────────────────────────────────────────────────────────────┐
│                   Template Processing                        │
└─────────────────────────────────────────────────────────────┘

User Config (YAML)
    │
    ↓
┌─────────────────┐
│ ggen CLI        │
│  project gen    │
└────────┬────────┘
         │
         ├─→ Load templates/
         │   • main-rs.tmpl
         │   • config-rs.tmpl
         │   • chat-rs.tmpl
         │   • mcp-adaptor-rs.tmpl
         │   • cargo-toml.tmpl
         │   • config-toml.tmpl
         │
         ├─→ Validate variables
         │   • Required fields
         │   • Type checking
         │   • Constraints
         │
         ├─→ Process templates
         │   • Handlebars rendering
         │   • Conditional blocks
         │   • Loops
         │
         └─→ Generate files
             │
             ↓
        Project Structure
        ├── src/
        │   ├── main.rs
        │   ├── chat.rs
        │   ├── config.rs
        │   ├── config/
        │   │   └── mcp.rs
        │   └── mcp_adaptor.rs
        ├── Cargo.toml
        ├── config.toml
        └── logs/
```

## Error Handling Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Error Propagation                         │
└─────────────────────────────────────────────────────────────┘

┌──────────────┐
│ Tool Failure │
└──────┬───────┘
       │
       ↓
┌──────────────────┐
│ Adaptor catches  │
│ Returns error    │
│ string           │
└──────┬───────────┘
       │
       ↓
┌──────────────────┐
│ Agent receives   │
│ error result     │
└──────┬───────────┘
       │
       ↓
┌──────────────────┐
│ LLM processes    │
│ error context    │
└──────┬───────────┘
       │
       ↓
┌──────────────────┐
│ Stream error     │
│ to user (red)    │
└──────────────────┘

Parallel path:
┌──────────────────┐
│ Log error with   │
│ full context     │
└──────────────────┘
```

## Scalability Patterns

### Horizontal Scaling

```
┌────────────────────────────────────────────────────────┐
│              Request Distribution                       │
└────────────────────────────────────────────────────────┘

           Incoming Requests
                  │
                  ↓
         ┌────────────────┐
         │ Load Balancer  │
         │  (Round Robin) │
         └────────┬───────┘
                  │
      ┌───────────┼───────────┐
      ↓           ↓           ↓
┌─────────┐ ┌─────────┐ ┌─────────┐
│ Worker  │ │ Worker  │ │ Worker  │
│    1    │ │    2    │ │    N    │
└────┬────┘ └────┬────┘ └────┬────┘
     │           │           │
     └───────────┼───────────┘
                 │
                 ↓
        ┌────────────────┐
        │ Shared Cache   │
        │ (Tool Results) │
        └────────────────┘
```

### Vertical Scaling

```
Single Instance Optimization

┌──────────────────────────────────┐
│     Async Task Parallelism       │
│                                  │
│  ┌────────┐  ┌────────┐         │
│  │ Tool 1 │  │ Tool 2 │  ...    │
│  │ (async)│  │ (async)│         │
│  └────────┘  └────────┘         │
│       │           │              │
│       └─────┬─────┘              │
│             ↓                    │
│      Tokio Runtime               │
│    (Thread Pool)                 │
└──────────────────────────────────┘
```

---

**Note**: All diagrams use ASCII art for maximum compatibility. For production documentation, consider converting to:
- Mermaid.js for interactive diagrams
- PlantUML for UML diagrams
- Draw.io for detailed architecture diagrams
