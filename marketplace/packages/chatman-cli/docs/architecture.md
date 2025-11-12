# ChatMan CLI Architecture

## Overview

ChatMan CLI is built on a modular, production-grade architecture using Rust's async/await ecosystem with Tokio runtime.

## Core Components

### 1. Chat Manager (`ChatManager`)

The central orchestrator for conversation management:

- **Conversation State**: Maintains message history and context
- **Provider Integration**: Interfaces with AI providers (OpenAI, Anthropic, etc.)
- **Knowledge Hooks**: Integrates semantic understanding via RDF ontologies
- **History Management**: Automatic trimming based on configurable limits
- **Export**: Conversation export to multiple formats

### 2. Configuration (`Config`)

Runtime configuration with environment variable support:

```rust
pub struct Config {
    pub provider: String,      // AI provider
    pub model: String,         // Model name
    pub max_history: usize,    // History limit
    pub timeout_secs: u64,     // Request timeout
    pub retry_attempts: u32,   // Retry logic
    pub ontology_path: Option<PathBuf>,  // RDF ontology
}
```

### 3. Message System

Strongly-typed message handling:

- **Message**: Core message structure with role, content, timestamp
- **Role Types**: User, Assistant, System
- **Serialization**: JSON/YAML support for persistence

### 4. Knowledge Hooks

Semantic understanding layer:

- **RDF Ontology Integration**: Load and query knowledge graphs
- **SPARQL Queries**: Pattern matching and reasoning
- **Context Enhancement**: Enrich conversations with semantic data

## Architecture Diagram

```
┌─────────────────────────────────────────────────┐
│              CLI Entry Point                    │
│           (src/main.rs - clap)                  │
└───────────────────┬─────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────┐
│           ChatManager (Core Logic)              │
│  ┌──────────────┐  ┌──────────────┐            │
│  │  History     │  │  Config      │            │
│  │  Management  │  │  Management  │            │
│  └──────────────┘  └──────────────┘            │
└───────────┬────────────────┬────────────────────┘
            │                │
            ▼                ▼
┌───────────────────┐  ┌──────────────────┐
│  AI Provider      │  │  Knowledge Hooks │
│  Integration      │  │  (RDF/SPARQL)    │
│  (OpenAI, etc.)   │  │                  │
└───────────────────┘  └──────────────────┘
```

## Data Flow

1. **User Input** → CLI Parser (clap)
2. **Command Routing** → ChatManager
3. **Message Creation** → Message struct
4. **Context Enhancement** → Knowledge Hooks (optional)
5. **AI Provider Call** → Async HTTP request
6. **Response Processing** → Message parsing
7. **History Update** → Conversation state
8. **Output Rendering** → Colored terminal

## Async Architecture

Built on Tokio for high-performance async I/O:

- **Non-blocking AI calls**: Concurrent request handling
- **Streaming support**: Real-time response streaming (future)
- **Resource efficiency**: Minimal memory footprint
- **Scalability**: Handle multiple conversations simultaneously

## Error Handling

Production-grade error handling with `anyhow` and `thiserror`:

- **Context propagation**: Rich error context
- **Retry logic**: Configurable retry attempts
- **Graceful degradation**: Fallback mechanisms
- **User-friendly messages**: Clear error reporting

## Testing Strategy

Comprehensive test coverage:

- **Unit tests**: `src/lib.rs` (inline tests)
- **Integration tests**: `tests/integration_test.rs`
- **Example tests**: Runnable examples as documentation
- **Property-based tests**: Future enhancement

## Performance Characteristics

- **Memory**: O(max_history) for conversation storage
- **Latency**: Network-bound (AI provider response time)
- **Throughput**: Limited by API rate limits
- **Concurrency**: Tokio runtime handles 1000s of concurrent tasks

## Extension Points

1. **Custom AI Providers**: Implement provider trait
2. **Knowledge Hook Plugins**: Add custom SPARQL queries
3. **Export Formats**: Add new serialization formats
4. **CLI Commands**: Extend clap subcommands
5. **Middleware**: Pre/post message processing hooks
