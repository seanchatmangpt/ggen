# GGen-MCP Module Dependency Diagram

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                          ggen-mcp crate                              │
│                                                                       │
│  ┌────────────┐         ┌──────────────┐        ┌────────────────┐ │
│  │  lib.rs    │────────►│  server.rs   │◄───────│  schema.rs     │ │
│  │  (root)    │         │  (MCP Server)│        │  (Types)       │ │
│  └──────┬─────┘         └──────────────┘        └────────────────┘ │
│         │                                                            │
│         │                                                            │
│         ├─────────────┬──────────────┬──────────────┬──────────────┤
│         │             │              │              │              │
│    ┌────▼────┐   ┌───▼────┐    ┌───▼───┐     ┌───▼────┐    ┌────▼───┐
│    │ error   │   │ agents │    │ swarm │     │ tools  │    │ utils  │
│    │         │   │        │    │       │     │        │    │        │
│    └─────────┘   └────┬───┘    └───┬───┘     └────────┘    └────────┘
│                       │            │                                 │
└───────────────────────┼────────────┼─────────────────────────────────┘
                        │            │
                        │            │
        ┌───────────────┴────┐   ┌──┴─────────────────┐
        │                    │   │                    │
        ▼                    │   ▼                    │
┌───────────────┐            │ ┌──────────────┐      │
│  Agent Trait  │            │ │  ultrathink  │      │
│  (async ops)  │            │ │  swarm       │      │
└───────────────┘            │ └──────────────┘      │
        ▲                    │                       │
        │                    │ ┌──────────────┐      │
        │                    │ │     wip      │      │
┌───────┴──────┐             │ │ integration  │      │
│  12 Agent    │             │ └──────────────┘      │
│  Impls       │             │                       │
├──────────────┤             │                       │
│ - coordinator│             │                       │
│ - validator  │             │                       │
│ - executor   │             │                       │
│ - monitor    │             └───────────────────────┘
│ - cache      │                     │
│ - security   │                     │
│ - metrics    │                     │
│ - health     │                     ▼
│ - recovery   │         ┌────────────────────────┐
│ - consensus  │         │      ggen-ai crate     │
│ - discovery  │         ├────────────────────────┤
│ - scheduler  │         │ ✓ LlmClient (trait)    │
└──────────────┘         │ ✓ LlmConfig            │
        │                │ ✓ OpenAIClient         │
        │                │ ✓ AnthropicClient      │
        │                │ ✓ OllamaClient         │
        ▼                │ ✓ MockClient           │
┌───────────────┐        │ ✓ TemplateGenerator    │
│  AgentInfo    │        │ ✓ SparqlGenerator      │
│  (data        │        │ ✓ OntologyGenerator    │
│   holder)     │        └────────────────────────┘
└───────────────┘
```

## Import Flow Diagram

```
┌──────────────┐
│ tools/ai.rs  │
└──────┬───────┘
       │
       │ imports LlmClient, OpenAIClient,
       │ AnthropicClient, OllamaClient, MockClient
       │
       ▼
┌─────────────────────┐
│   ggen-ai crate     │
│                     │
│ pub use client::    │
│   LlmClient         │
│                     │
│ pub use providers:: │
│   OpenAIClient      │
│   AnthropicClient   │
│   OllamaClient      │
│   MockClient        │
└─────────────────────┘

┌───────────────┐
│ swarm/mod.rs  │
└───────┬───────┘
        │
        │ use crate::agents::{AgentInfo, AgentConfig, ...}
        │ use crate::error::{McpError, Result}
        │
        ▼
┌────────────────┐          ┌──────────────┐
│ agents/mod.rs  │          │  error.rs    │
│                │          │              │
│ pub trait Agent│          │ pub enum     │
│ pub struct     │          │ GgenMcpError │
│   AgentInfo    │          │              │
│ pub type       │          │ pub type     │
│   AgentId      │          │ McpError =   │
│ pub type       │          │   GgenMcpError
│ ConsensusResult│          └──────────────┘
└────────────────┘
```

## Module Communication Pattern

```
    ┌─────────┐
    │ Client  │
    │ Request │
    └────┬────┘
         │
         │ JSON-RPC
         ▼
    ┌─────────────┐
    │ MCP Server  │
    │ (server.rs) │
    └──────┬──────┘
           │
           │ Routes to tool
           ▼
    ┌──────────────┐
    │ tools/ai.rs  │
    │ tools/*.rs   │
    └──────┬───────┘
           │
           ├───────────────┐
           │               │
    ┌──────▼──────┐  ┌────▼──────┐
    │  ggen-ai    │  │  agents   │
    │  LlmClient  │  │  (if      │
    │             │  │  needed)  │
    └─────────────┘  └───────────┘
```

## Type Relationship Diagram

```
┌──────────────────────────────────────────────────────────┐
│                    agents/mod.rs                         │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  pub trait Agent {                                       │
│      async fn initialize(&mut self);                     │
│      async fn start(&mut self);                          │
│      async fn handle_message(&mut self, ...);            │
│      ...                                                 │
│  }                                                       │
│                                                          │
│  ┌─────────────────────────────────────┐                │
│  │ Implemented by 12 agent types:      │                │
│  │                                     │                │
│  │ - LondonBddCoordinator             │                │
│  │ - ByzantineValidator               │                │
│  │ - TemplateExecutor                 │                │
│  │ - GraphMonitor                     │                │
│  │ - CacheManager                     │                │
│  │ - SecurityAgent                    │                │
│  │ - MetricsCollector                 │                │
│  │ - HealthMonitor                    │                │
│  │ - RecoveryAgent                    │                │
│  │ - ConsensusManager                 │                │
│  │ - ServiceDiscovery                 │                │
│  │ - TaskScheduler                    │                │
│  └─────────────────────────────────────┘                │
│                                                          │
│  pub struct AgentInfo {  // <-- Renamed from Agent      │
│      pub id: AgentId,                                    │
│      pub agent_type: AgentType,                          │
│      pub status: AgentStatus,                            │
│      pub capabilities: Vec<AgentCapability>,             │
│  }                                                       │
│                                                          │
│  pub type AgentId = Uuid;                                │
│  pub type ConsensusResult = bool;                        │
│                                                          │
│  pub enum AgentStatus { ... }                            │
│  pub enum AgentType { ... }                              │
│  pub enum AgentCapability { ... }                        │
└──────────────────────────────────────────────────────────┘
```

## Error Type Hierarchy

```
┌────────────────────────────────┐
│        error.rs                │
├────────────────────────────────┤
│                                │
│  pub enum GgenMcpError {       │
│      MissingParameter(String)  │
│      InvalidParameter(String)  │
│      ExecutionFailed(String)   │
│      RegistryError(String)     │
│      GraphError(String)        │
│      TemplateError(String)     │
│      SerializationError(String)│
│      Timeout(String)           │
│      GenerationFailed(String)  │
│      Core(GgenError)           │
│      Anyhow(AnyhowError)       │
│      Protocol(rmcp::Error)     │
│      NotFound(String)          │ // Added
│      ConnectionError(String)   │ // Added
│      NetworkError(String)      │ // Added
│      ValidationError(String)   │ // Added
│      Configuration(String)     │ // Added
│      Io(String)                │ // Added
│  }                             │
│                                │
│  pub type McpError =           │
│      GgenMcpError;  // Alias   │
│                                │
│  pub type Result<T> =          │
│      std::result::Result<      │
│          T, GgenMcpError       │
│      >;                        │
└────────────────────────────────┘
```

## Swarm Architecture

```
┌───────────────────────────────────────────────────────┐
│                   swarm/mod.rs                        │
├───────────────────────────────────────────────────────┤
│                                                       │
│  pub struct SwarmCoordinator {                        │
│      config: SwarmConfig,                             │
│      agents: Arc<RwLock<HashMap<Uuid, AgentInfo>>>, │
│      task_queue: Arc<RwLock<VecDeque<...>>>,         │
│      metrics: Arc<RwLock<SwarmMetrics>>,             │
│      event_tx: broadcast::Sender<SwarmEvent>,        │
│      channels: SwarmChannels,                        │
│  }                                                    │
│                                                       │
│  pub mod ultrathink;      // Local implementation    │
│  pub mod wip_integration; // Local implementation    │
│                                                       │
└───────────────────────────────────────────────────────┘
         │                          │
         │                          │
    ┌────▼──────────────┐      ┌───▼─────────────────┐
    │ ultrathink.rs     │      │ wip_integration.rs  │
    │                   │      │                     │
    │ UltrathinkSwarm   │      │ WipIntegration      │
    │ UltrathinkConfig  │      │   Manager           │
    │ UltrathinkAgent   │      │ WipClient           │
    │ ...               │      │ ConflictResolver    │
    └───────────────────┘      └─────────────────────┘
```

## Key Architectural Decisions

### 1. Separation of Concerns

- **ggen-mcp**: MCP server, agents, swarm coordination
- **ggen-ai**: LLM clients, generators, autonomous systems
- **ggen-core**: Graph operations, templates

### 2. Agent Architecture

- **Agent trait**: Async interface for all agents
- **AgentInfo struct**: Metadata and state storage
- **12 specialized implementations**: Each with specific responsibility

### 3. Import Strategy

```
✅ IMPORT from ggen-ai:
   - LlmClient (trait)
   - Provider implementations (OpenAIClient, AnthropicClient, etc.)
   - Generator implementations (TemplateGenerator, etc.)

❌ DO NOT import from ggen-ai:
   - Swarm types (different domain)
   - Agent types (ggen-mcp has its own)
```

### 4. Error Handling

```
GgenMcpError (main error type)
    ↓
McpError (type alias for compatibility)
    ↓
Result<T> (shorthand type alias)
```

## File Size Constraints

Following best practices:
- **agents/mod.rs**: ~350 lines (trait + types + registry)
- **swarm/mod.rs**: ~450 lines (coordinator + config)
- **swarm/ultrathink.rs**: ~1070 lines (complex swarm logic)
- **swarm/wip_integration.rs**: ~565 lines (WIP integration)
- **tools/ai.rs**: ~570 lines (AI tool implementations)

All files under 1100 lines, maintaining modularity.

## Testing Structure

```
tests/
├── integration_tests.rs      # MCP server integration
├── agent_tests.rs             # Agent coordination tests
├── swarm_tests.rs             # Swarm operation tests
├── tool_tests.rs              # Tool functionality tests
└── common/
    ├── fixtures.rs            # Test fixtures
    └── mod.rs                 # Test utilities
```

## Next Steps

1. ✅ Module structure fixed
2. ✅ Import paths corrected
3. ✅ Type conflicts resolved
4. ⏳ Verify compilation
5. ⏳ Run test suite
6. ⏳ Performance benchmarks
7. ⏳ Documentation review
