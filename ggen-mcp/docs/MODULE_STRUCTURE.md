# GGen-MCP Module Structure

## Overview

This document describes the corrected module structure for `ggen-mcp` after resolving import and type conflicts.

## Module Hierarchy

```
ggen-mcp/
├── lib.rs                  # Root module exports
├── error.rs                # Error types and Result alias
├── agents/                 # 12-agent architecture
│   ├── mod.rs             # Agent trait + AgentInfo struct
│   ├── coordinator.rs     # London-BDD Coordinator
│   ├── validator.rs       # Byzantine Validator
│   ├── consensus.rs       # Consensus Manager
│   └── ... (9 more agents)
├── swarm/                  # Swarm intelligence
│   ├── mod.rs             # Swarm coordinator
│   ├── ultrathink.rs      # Ultrathink swarm (local)
│   └── wip_integration.rs # WIP integration manager
├── tools/                  # MCP tool implementations
│   ├── mod.rs
│   ├── ai.rs              # AI-powered tools
│   ├── template.rs        # Template tools
│   └── ... (other tools)
└── server.rs               # MCP server implementation
```

## Key Type Definitions

### agents/mod.rs

**Exports:**
- `Agent` trait - Async interface for agent operations
- `AgentInfo` struct - Data holder for agent metadata
- `AgentId` type alias - `Uuid`
- `ConsensusResult` type alias - `bool`
- `AgentStatus`, `AgentType`, `AgentCapability` enums
- `AgentConfig` - Agent configuration struct

**Critical Fix:** Renamed `Agent` struct to `AgentInfo` to avoid conflict with `Agent` trait.

### error.rs

**Exports:**
- `GgenMcpError` enum - Main error type
- `McpError` type alias - For compatibility with swarm module
- `Result<T>` type alias - `std::result::Result<T, GgenMcpError>`

**Added Variants:**
- `Configuration(String)` - For configuration errors
- `Io(String)` - For IO errors

### swarm/mod.rs

**Uses:**
- `crate::agents::{AgentInfo, AgentCapability, AgentConfig, AgentRole, AgentStatus, AgentType}`
- `crate::error::{McpError, Result}`

**Local Modules:**
- `pub mod ultrathink` - Local ultrathink implementation
- `pub mod wip_integration` - WIP integration manager

**Note:** Uses `AgentInfo` struct for storing agent data, not the `Agent` trait.

## Import Strategy

### From ggen-ai

**DO Import:**
```rust
use ggen_ai::{
    LlmClient,           // Trait for LLM operations
    LlmConfig,           // Configuration
    OpenAIClient,        // OpenAI provider
    AnthropicClient,     // Anthropic provider
    OllamaClient,        // Ollama provider
    MockClient,          // Testing
    TemplateGenerator,   // Template generation
    SparqlGenerator,     // SPARQL generation
    OntologyGenerator,   // Ontology generation
};
```

**DO NOT Import:**
- `GenAiClient` - Does not exist (was incorrect import)
- `SwarmAgent` from ggen-ai - ggen-mcp has its own swarm implementation

### Local Definitions

**ggen-mcp defines locally:**
- 12-agent architecture (`Agent` trait and implementations)
- Swarm coordination (`SwarmCoordinator`, `SwarmConfig`)
- Ultrathink swarm (different from ggen-ai's ultrathink)
- WIP integration manager
- MCP server and tool implementations

## Architectural Decisions

### 1. Agent Trait vs Agent Struct Separation

**Problem:** Both a trait and struct named `Agent` caused conflicts.

**Solution:**
- Keep `Agent` as the trait (primary async interface)
- Rename struct to `AgentInfo` (data holder)
- Update all references in swarm modules

**Rationale:** The trait is the primary abstraction for agent behavior. The struct is just metadata storage.

### 2. LLM Client Imports

**Problem:** Code attempted to import non-existent `GenAiClient`.

**Solution:** Import `LlmClient` trait and concrete provider implementations (`OpenAIClient`, `AnthropicClient`, `OllamaClient`).

**Rationale:** ggen-ai exports provider-specific clients that implement the `LlmClient` trait.

### 3. Swarm Module Organization

**Problem:** Unclear whether to import swarm types from ggen-ai or define locally.

**Solution:**
- ggen-mcp defines its own `SwarmCoordinator` and swarm types
- ggen-ai's swarm is for template/code generation workflows
- ggen-mcp's swarm is for MCP agent coordination

**Rationale:** Different domains, different purposes. Separation of concerns.

### 4. Error Type Compatibility

**Problem:** Missing error variants (`Configuration`, `Io`) and compatibility with swarm module.

**Solution:**
- Added missing variants to `GgenMcpError`
- Created `McpError` type alias for swarm module compatibility
- Added helper methods (`not_found()`, `connection()`, etc.)

**Rationale:** Maintains clean error handling with compatibility across modules.

### 5. Cargo Dependencies

**Added:**
- `tokio-tungstenite = "0.23"` - WebSocket support for WIP integration
- `futures-util = "0.3"` - Future utilities
- `reqwest = { version = "0.12", features = ["json", "rustls-tls"] }` - HTTP client

**Rationale:** Required for WIP integration WebSocket connections and HTTP communication.

## Module Dependency Graph

```
┌─────────────┐
│   lib.rs    │  (Root module)
└──────┬──────┘
       │
       ├─────────────┬─────────────┬──────────────┬─────────────┐
       │             │             │              │             │
   ┌───▼───┐    ┌───▼───┐    ┌───▼────┐    ┌───▼────┐   ┌───▼────┐
   │ error │    │agents │    │ swarm  │    │ tools  │   │ server │
   └───┬───┘    └───┬───┘    └───┬────┘    └───┬────┘   └────────┘
       │            │            │              │
       │            ├── Agent trait             │
       │            ├── AgentInfo struct        │
       │            ├── 12 agent impls          │
       │            │                           │
       │            ▼                           │
       │        ┌───────────┐                  │
       │        │ consensus │                  │
       │        │ validator │                  │
       │        │ executor  │                  │
       │        │ ...       │                  │
       │        └───────────┘                  │
       │                                       │
       │         ┌─────────────────┐          │
       │         │  ultrathink.rs  │◄─────────┤
       │         └─────────────────┘          │
       │         ┌──────────────────┐         │
       │         │wip_integration.rs│◄────────┤
       │         └──────────────────┘         │
       │                                      │
       │                ┌────────────────────▼────┐
       └────────────────►   ggen-ai crate         │
                        │  (LlmClient, Providers) │
                        └─────────────────────────┘
```

## Import Rules

### Rule 1: Use crate:: prefix for sibling modules
```rust
// ✅ Correct
use crate::agents::{AgentInfo, AgentCapability};
use crate::swarm::ultrathink::UltrathinkSwarm;

// ❌ Wrong
use super::ultrathink::UltrathinkSwarm;  // Only works within same directory
```

### Rule 2: Import specific types, not wildcards
```rust
// ✅ Correct
use ggen_ai::{LlmClient, LlmConfig, OpenAIClient};

// ❌ Wrong (less clear)
use ggen_ai::*;
```

### Rule 3: Use type aliases for compatibility
```rust
// ✅ Correct
pub type McpError = GgenMcpError;  // In error.rs
use crate::error::McpError;         // In other modules

// This allows gradual migration without breaking existing code
```

## Testing Strategy

1. **Unit tests** - In each module file
2. **Integration tests** - In `tests/` directory
3. **Agent coordination tests** - Test agent trait implementations
4. **Swarm operation tests** - Test swarm coordinator functionality

## Future Improvements

1. **Agent Pool Management** - Implement Arc-based client pooling instead of recreating clients
2. **Better Error Context** - Add more context to error types with `thiserror` context
3. **Async Trait Performance** - Consider using `async-trait` alternatives for hot paths
4. **Module Documentation** - Add detailed rustdoc comments to all public interfaces

## Version History

- **2025-10-10** - Initial module restructuring
  - Fixed Agent trait/struct naming conflict
  - Corrected LlmClient imports from ggen-ai
  - Added missing error variants
  - Added missing Cargo dependencies
  - Documented architectural decisions
