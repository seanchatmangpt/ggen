# Rig Integration Re-enabling Guide

**Purpose**: Step-by-step guide to re-enable LLM functionality using Rig+MCP integration after the migration is complete.

**Status**: LLM integration currently disabled (2025-10-11)
**Target**: Rig 0.15.1 + rmcp 0.8 + MCP servers

---

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Architecture Changes](#architecture-changes)
4. [Step-by-Step Re-enabling](#step-by-step-re-enabling)
5. [File-by-File Implementation](#file-by-file-implementation)
6. [Testing Strategy](#testing-strategy)
7. [Rollback Procedure](#rollback-procedure)

---

## Overview

### What Was Disabled

All LLM integration code in the CLI has been commented out (NOT deleted) in **8 files**:

#### AI Commands (`/cli/src/cmds/ai/`):
1. `generate.rs` - Template generation
2. `graph.rs` - RDF graph generation
3. `sparql.rs` - SPARQL query generation
4. `project.rs` - Project scaffolding
5. `from_source.rs` - Source-to-template conversion
6. `frontmatter.rs` - Frontmatter generation

#### Other Commands:
7. `market/natural.rs` - Natural language marketplace search
8. `autonomous.rs` - Autonomous graph evolution

### Why Rig+MCP?

**Current Architecture** (ggen-ai):
```
User → CLI → GenAiClient → LLM Provider (direct API calls)
```

**New Architecture** (Rig+MCP):
```
User → CLI → Rig Agent → MCP Adapter → MCP Servers (tools)
                ↓
          Vector Store (semantic tool routing)
```

**Benefits**:
- **Semantic tool routing**: AI automatically finds the right tool for the task
- **Multi-server support**: Git, filesystem, template, and custom tools work together
- **Streaming responses**: Real-time feedback with integrated tool calls
- **Framework-agnostic**: Works with any LLM provider (DeepSeek, Claude, GPT)
- **Automatic tool execution**: MCP adapter handles tool calls transparently
- **Type-safe schemas**: JSON Schema automatically converted

---

## Prerequisites

### 1. Dependencies Already Added

The `vendors/rig-integration/` directory contains a working example from the official MCP Rust SDK:

```toml
# vendors/rig-integration/Cargo.toml
[dependencies]
rig-core = "0.15.1"
rmcp = { version = "0.8", features = [
    "client",
    "transport-child-process",
    "transport-sse-client-reqwest",
    "transport-streamable-http-client-reqwest"
] }
tokio = { version = "1", features = ["full"] }
anyhow = "1.0"
serde_json = "1"
```

### 2. Add to ggen Workspace

Add rig-integration to main workspace:

```toml
# /Users/sac/ggen/Cargo.toml
[workspace]
members = [
  "utils",
  "cli",
  "ggen-core",
  "ggen-mcp",
  "ggen-ai",
  "vendors/rig-integration",  # ADD THIS LINE
  "examples/frontmatter-cli",
  "examples/natural-market-search",
  "examples/ai-template-project",
]
```

### 3. Install MCP Servers

```bash
# Filesystem operations
npm install -g @modelcontextprotocol/server-filesystem

# Git operations
npm install -g @modelcontextprotocol/server-git

# Add custom ggen MCP server (if available)
# npm install -g @ggen/mcp-server
```

### 4. Create MCP Configuration

Create `ggen-mcp.toml`:

```toml
# MCP Server Configuration for ggen

[[mcp.servers]]
name = "filesystem"
protocol = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/templates"]
description = "File system operations for template management"

[[mcp.servers]]
name = "git"
protocol = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-git"]
description = "Git operations for version control"

# Optional: SSE-based server
[[mcp.servers]]
name = "template-tools"
protocol = "sse"
url = "http://localhost:3000/sse"
description = "Custom template generation tools"

# Optional: HTTP-based server
[[mcp.servers]]
name = "custom-tools"
protocol = "streamable"
url = "http://localhost:3001"
description = "Custom ggen tools"
```

---

## Architecture Changes

### Old Pattern (ggen-ai)

```rust
// 1. Create LLM client
let client: Arc<dyn LlmClient> = global_config.create_contextual_client()?;

// 2. Create generator
let generator = TemplateGenerator::with_client(client);

// 3. Generate (single API call)
let template = generator.generate_template(&description, examples).await?;

// 4. Output result
println!("{:?}", template);
```

### New Pattern (Rig+MCP)

```rust
// 1. Load MCP configuration
use rig_integration::config::Config;
let config = Config::retrieve("ggen-mcp.toml").await?;

// 2. Initialize MCP manager (parallel server startup)
let mcp_manager = config.mcp.create_manager().await?;
// Servers: filesystem, git, template-tools all start concurrently

// 3. Get all available MCP tools
let tool_set = mcp_manager.get_tool_set().await?;
// Aggregates tools from all servers using JoinSet

// 4. Create embedding model for semantic routing
use rig::providers::cohere;
let cohere_client = cohere::Client::from_env();
let embedding_model = cohere_client.embedding_model(
    cohere::EMBED_MULTILINGUAL_V3,
    "search_document"
);

// 5. Build embeddings for tool schemas
use rig::embeddings::EmbeddingsBuilder;
let embeddings = EmbeddingsBuilder::new(embedding_model.clone())
    .documents(tool_set.schemas()?)?  // Convert tools to documents
    .build().await?;

// 6. Create vector store for semantic tool routing
use rig::vector_store::in_memory_store::InMemoryVectorStore;
let store = InMemoryVectorStore::from_documents_with_id_f(
    embeddings,
    |tool| tool.name.clone()  // Use tool name as ID
);
let index = store.index(embedding_model);

// 7. Create Rig agent with dynamic tool selection
use rig::providers::deepseek;
let deepseek_client = deepseek::Client::from_env();
let agent = deepseek_client
    .agent(deepseek::DEEPSEEK_CHAT)
    .dynamic_tools(4, index, tool_set)  // Top-4 semantic routing
    .build();

// 8. Stream chat with automatic tool execution
let mut chat_log = Vec::new();
let mut response = agent.stream_chat(&description, chat_log.clone()).await?;

use std::io::{stdout, Write};
while let Some(message) = response.next().await {
    match message {
        Ok(AssistantContent::Text(text)) => {
            // Stream text response to user
            print!("{}", text.text);
            stdout().flush()?;
        }
        Ok(AssistantContent::ToolCall(tool_call)) => {
            // MCP adapter automatically executes tool via server
            let result = agent.tools.call(
                &tool_call.function.name,
                tool_call.function.arguments
            ).await?;
            // Result automatically fed back to agent
            chat_log.push(Message::user(result));
        }
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

---

## Step-by-Step Re-enabling

### Phase 1: Foundation (Week 1)

**Goal**: Set up Rig+MCP infrastructure without touching CLI commands yet.

#### Step 1.1: Create Integration Module

```bash
mkdir -p cli/src/rig_integration
touch cli/src/rig_integration/mod.rs
touch cli/src/rig_integration/mcp_manager.rs
touch cli/src/rig_integration/agent_builder.rs
```

**File**: `cli/src/rig_integration/mod.rs`
```rust
//! Rig+MCP integration layer for ggen

pub mod mcp_manager;
pub mod agent_builder;

pub use mcp_manager::GgenMcpManager;
pub use agent_builder::GgenAgentBuilder;
```

#### Step 1.2: Implement MCP Manager

**File**: `cli/src/rig_integration/mcp_manager.rs`
```rust
use rig::tool::ToolSet;
use rig_integration::config::Config;
use rig_integration::mcp_adaptor::McpManager;
use anyhow::Result;

/// Wrapper around rig-integration's McpManager for ggen
pub struct GgenMcpManager {
    inner: McpManager,
}

impl GgenMcpManager {
    /// Initialize from ggen-mcp.toml configuration
    pub async fn new() -> Result<Self> {
        let config = Config::retrieve("ggen-mcp.toml").await?;
        let inner = config.mcp.create_manager().await?;
        Ok(Self { inner })
    }

    /// Get all available tools from all MCP servers
    pub async fn get_tool_set(&self) -> Result<ToolSet> {
        self.inner.get_tool_set().await
    }
}
```

#### Step 1.3: Implement Agent Builder

**File**: `cli/src/rig_integration/agent_builder.rs`
```rust
use rig::agent::Agent;
use rig::completion::CompletionModel;
use rig::embeddings::EmbeddingsBuilder;
use rig::providers::{cohere, deepseek};
use rig::tool::ToolSet;
use rig::vector_store::in_memory_store::InMemoryVectorStore;
use anyhow::Result;

pub struct GgenAgentBuilder {
    tool_set: Option<ToolSet>,
    top_k_tools: usize,
}

impl GgenAgentBuilder {
    pub fn new() -> Self {
        Self {
            tool_set: None,
            top_k_tools: 4,
        }
    }

    pub fn with_tools(mut self, tool_set: ToolSet) -> Self {
        self.tool_set = Some(tool_set);
        self
    }

    pub fn with_top_k(mut self, k: usize) -> Self {
        self.top_k_tools = k;
        self
    }

    pub async fn build<M: CompletionModel>(
        self,
        model_name: &str,
    ) -> Result<Agent<M>> {
        // Create LLM client
        let deepseek_client = deepseek::Client::from_env();

        // If no tools provided, return simple agent
        let Some(tool_set) = self.tool_set else {
            return Ok(deepseek_client
                .agent(model_name)
                .build());
        };

        // Create embedding model for semantic routing
        let cohere_client = cohere::Client::from_env();
        let embedding_model = cohere_client.embedding_model(
            cohere::EMBED_MULTILINGUAL_V3,
            "search_document",
        );

        // Build embeddings for tool schemas
        let embeddings = EmbeddingsBuilder::new(embedding_model.clone())
            .documents(tool_set.schemas()?)?
            .build()
            .await?;

        // Create vector store
        let store = InMemoryVectorStore::from_documents_with_id_f(
            embeddings,
            |tool| tool.name.clone(),
        );
        let index = store.index(embedding_model);

        // Build agent with dynamic tool selection
        Ok(deepseek_client
            .agent(model_name)
            .dynamic_tools(self.top_k_tools, index, tool_set)
            .build())
    }
}
```

#### Step 1.4: Add Dependencies to CLI

**File**: `cli/Cargo.toml`
```toml
[dependencies]
# ... existing dependencies ...

# Rig+MCP integration
rig-core = "0.15.1"
rig-integration = { path = "../vendors/rig-integration" }
```

#### Step 1.5: Test Foundation

Create integration test:

**File**: `cli/tests/rig_integration_test.rs`
```rust
use ggen_cli::rig_integration::{GgenMcpManager, GgenAgentBuilder};

#[tokio::test]
#[ignore = "Requires MCP servers running"]
async fn test_mcp_manager_initialization() {
    let manager = GgenMcpManager::new().await.unwrap();
    let tool_set = manager.get_tool_set().await.unwrap();
    assert!(!tool_set.schemas().unwrap().is_empty());
}

#[tokio::test]
#[ignore = "Requires API keys"]
async fn test_agent_builder() {
    let manager = GgenMcpManager::new().await.unwrap();
    let tool_set = manager.get_tool_set().await.unwrap();

    let agent = GgenAgentBuilder::new()
        .with_tools(tool_set)
        .with_top_k(4)
        .build("deepseek-chat")
        .await
        .unwrap();

    // Agent should have tools available
    assert!(agent.tools.len() > 0);
}
```

Run tests:
```bash
# Without MCP servers (should pass)
cargo test --package ggen-cli-lib rig_integration

# With MCP servers (run ignored tests)
cargo test --package ggen-cli-lib rig_integration -- --ignored
```

### Phase 2: Single Command Re-enabling (Week 2)

**Goal**: Re-enable one command (`generate.rs`) as proof of concept.

#### Step 2.1: Uncomment and Refactor generate.rs

**File**: `cli/src/cmds/ai/generate.rs`

1. **Remove early return error**:
```rust
// DELETE THIS:
return Err(ggen_utils::error::Error::new(
    "LLM integration disabled for Rig migration. See comments in generate.rs"
));
```

2. **Uncomment the client creation section** and replace with Rig:
```rust
pub async fn run(args: &GenerateArgs) -> Result<()> {
    println!("🔧 Generating template with AI...");

    // NEW: Initialize Rig+MCP
    let mcp_manager = GgenMcpManager::new().await?;
    let tool_set = mcp_manager.get_tool_set().await?;

    let agent = GgenAgentBuilder::new()
        .with_tools(tool_set)
        .with_top_k(4)
        .build(deepseek::DEEPSEEK_CHAT)
        .await?;

    // Build prompt from description and examples
    let prompt = format!(
        "Generate a template for: {}\n\nRequirements:\n{}",
        args.description,
        args.examples.join("\n")
    );

    // Stream response
    let mut chat_log = Vec::new();
    let mut response = agent.stream_chat(&prompt, chat_log.clone()).await?;

    let mut generated_content = String::new();

    use std::io::{stdout, Write};
    while let Some(message) = response.next().await {
        match message {
            Ok(AssistantContent::Text(text)) => {
                print!("{}", text.text);
                stdout().flush()?;
                generated_content.push_str(&text.text);
            }
            Ok(AssistantContent::ToolCall(tool_call)) => {
                println!("\n🔧 Using tool: {}", tool_call.function.name);
                let result = agent.tools.call(
                    &tool_call.function.name,
                    tool_call.function.arguments
                ).await?;
                chat_log.push(Message::user(result));
            }
            Err(e) => eprintln!("Error: {}", e),
        }
    }

    // Parse generated content into template
    // (keep existing parsing logic)

    println!("\n✅ Template generated successfully!");

    if let Some(output_path) = &args.output {
        fs::write(output_path, generated_content)?;
        println!("📁 Saved to: {}", output_path);
    }

    Ok(())
}
```

#### Step 2.2: Test Single Command

```bash
# Test with mock (if mock support added)
cargo run -- ai generate "a simple web server" --mock

# Test with real MCP servers
cargo run -- ai generate "a simple web server" --output test.tmpl
```

#### Step 2.3: Validate and Document

1. Verify streaming output works
2. Verify tool calls execute correctly
3. Verify template generation is equivalent to old system
4. Document any differences in behavior

### Phase 3: Remaining AI Commands (Week 3)

Apply the same pattern to all AI commands:

1. **graph.rs**: RDF graph generation
2. **sparql.rs**: SPARQL query generation
3. **project.rs**: Project scaffolding
4. **from_source.rs**: Source analysis
5. **frontmatter.rs**: Frontmatter generation

For each file:
1. Remove early return error
2. Replace commented LLM client creation with Rig agent
3. Replace `generator.generate_*()` calls with `agent.stream_chat()`
4. Handle streaming responses
5. Test thoroughly

### Phase 4: Non-AI Commands (Week 4)

1. **market/natural.rs**: Natural language search
2. **autonomous.rs**: Graph evolution

These may require custom adaptations due to specialized workflows.

### Phase 5: Testing & Validation (Week 5)

1. Re-enable all disabled tests
2. Update test fixtures for Rig patterns
3. Add integration tests for MCP tooling
4. Performance benchmarking vs old system
5. Documentation updates

---

## File-by-File Implementation

### Template for Each File

```rust
// BEFORE (ggen-ai):
let client = global_config.create_contextual_client()?;
let generator = TemplateGenerator::with_client(client);
let result = generator.generate_template(&description, examples).await?;

// AFTER (Rig+MCP):
use crate::rig_integration::{GgenMcpManager, GgenAgentBuilder};

let mcp_manager = GgenMcpManager::new().await?;
let tool_set = mcp_manager.get_tool_set().await?;

let agent = GgenAgentBuilder::new()
    .with_tools(tool_set)
    .build(deepseek::DEEPSEEK_CHAT)
    .await?;

let mut response = agent.stream_chat(&description, vec![]).await?;

// Handle streaming response with tool calls
```

### Special Cases

#### autonomous.rs: Dual LLM Clients

Old code creates TWO clients (parser + validator):

```rust
let (parser_client, validator_client): (Arc<dyn LlmClient>, Arc<dyn LlmClient>)
```

**Rig Solution**: Create two separate agents with different system prompts:

```rust
let parser_agent = GgenAgentBuilder::new()
    .with_tools(tool_set.clone())
    .build("deepseek-chat")
    .await?;

let validator_agent = GgenAgentBuilder::new()
    .with_tools(tool_set)
    .build("deepseek-chat")
    .await?;

let engine = GraphEvolutionEngine::new_with_rig(
    parser_agent,
    validator_agent,
    config
)?;
```

---

## Testing Strategy

### Unit Tests

Re-enable by removing `#[cfg(feature = "rig-integration-enabled")]`:

**File**: `cli/src/cmds/market/natural.rs`
```rust
// BEFORE:
#[cfg(test)]
#[cfg(feature = "rig-integration-enabled")]  // REMOVE THIS LINE
mod tests {
    #[tokio::test]
    #[ignore = "LLM integration disabled"]  // CHANGE TO:
    #[ignore = "Requires MCP servers"]
    async fn test_natural_search_with_mock() {
        // Update test to use Rig agent instead of NaturalSearchGenerator
    }
}
```

### Integration Tests

Create comprehensive MCP integration tests:

**File**: `cli/tests/mcp_integration_test.rs`
```rust
use ggen_cli::rig_integration::GgenMcpManager;

#[tokio::test]
#[ignore = "Requires running MCP servers"]
async fn test_full_template_generation_workflow() {
    let manager = GgenMcpManager::new().await.unwrap();
    // Test complete workflow from description to template
}

#[tokio::test]
#[ignore = "Requires running MCP servers"]
async fn test_semantic_tool_routing() {
    // Verify that agent selects appropriate tools for task
}

#[tokio::test]
#[ignore = "Requires running MCP servers"]
async fn test_multi_server_coordination() {
    // Verify filesystem + git tools work together
}
```

### Performance Tests

Compare old vs new:

```rust
#[tokio::test]
#[ignore = "Expensive"]
async fn benchmark_template_generation() {
    // Measure: time, token usage, quality
}
```

---

## Rollback Procedure

If Rig integration fails or needs to be reverted:

### Quick Rollback (Emergency)

1. **Re-comment Rig code**, restore early returns:
```rust
// In each file, add back:
return Err(ggen_utils::error::Error::new(
    "LLM integration disabled for Rig migration. See comments"
));
```

2. **Uncomment ggen-ai code**:
```bash
# Script to batch uncomment
find cli/src/cmds -name "*.rs" -exec sed -i 's|^    /\*|    |g' {} \;
find cli/src/cmds -name "*.rs" -exec sed -i 's|^    \*/||g' {} \;
```

3. **Revert dependencies** in `cli/Cargo.toml`

### Staged Rollback

1. Keep Rig infrastructure but disable individual commands
2. Use feature flags to toggle between ggen-ai and Rig:

```toml
[features]
default = ["ggen-ai-backend"]
ggen-ai-backend = []
rig-backend = ["rig-core", "rig-integration"]
```

```rust
#[cfg(feature = "ggen-ai-backend")]
{
    // Old code
}

#[cfg(feature = "rig-backend")]
{
    // New code
}
```

---

## Verification Checklist

Before considering migration complete:

### Functionality
- [ ] All 8 CLI commands work with Rig
- [ ] Streaming output displays correctly
- [ ] Tool calls execute without errors
- [ ] Error handling graceful
- [ ] Mock mode works for testing

### Performance
- [ ] Response time comparable to old system
- [ ] Token usage not significantly increased
- [ ] Memory usage acceptable
- [ ] Concurrent operations stable

### Quality
- [ ] Generated templates equivalent quality
- [ ] All test suites pass
- [ ] Documentation updated
- [ ] Examples work

### Deployment
- [ ] MCP servers documented
- [ ] Configuration examples provided
- [ ] Migration guide complete
- [ ] Rollback procedure tested

---

## Common Issues & Solutions

### Issue 1: MCP Server Connection Failures

**Symptom**: `Failed to connect to MCP server`

**Solutions**:
1. Verify servers installed: `npm list -g | grep modelcontextprotocol`
2. Check server startup: `npx @modelcontextprotocol/server-filesystem --help`
3. Validate ggen-mcp.toml paths

### Issue 2: Tool Not Found

**Symptom**: `Tool 'xyz' not available in tool set`

**Solutions**:
1. Check tool_set schemas: `println!("{:#?}", tool_set.schemas())`
2. Verify MCP server exposes tool
3. Check vector index built correctly

### Issue 3: Poor Tool Routing

**Symptom**: Agent selects wrong tools

**Solutions**:
1. Increase top_k: `.with_top_k(8)`
2. Improve tool descriptions in MCP server
3. Use better embedding model
4. Add explicit tool hints in prompt

### Issue 4: Streaming Breaks

**Symptom**: Output stops mid-generation

**Solutions**:
1. Check timeout settings
2. Verify network stability
3. Add error recovery:
```rust
while let Some(message) = response.next().await {
    match message {
        Ok(content) => handle_content(content),
        Err(e) => {
            eprintln!("Stream error: {}", e);
            // Attempt recovery
            continue;
        }
    }
}
```

---

## Additional Resources

1. **Rig Documentation**: https://github.com/0xPlaygrounds/rig
2. **MCP Rust SDK**: https://github.com/modelcontextprotocol/rust-sdk
3. **Working Example**: `/Users/sac/ggen/vendors/rig-integration/`
4. **Integration Analysis**: `/Users/sac/ggen/docs/RIG_MCP_INTEGRATION_ANALYSIS.md`
5. **Official MCP Docs**: https://modelcontextprotocol.io/

---

## Support

For questions during re-enabling:

1. Check vendors/rig-integration/ for working patterns
2. Review RIG_MCP_INTEGRATION_ANALYSIS.md for architecture details
3. Test against minimal example before CLI integration
4. Use --mock flag during development to avoid API costs

---

**Last Updated**: 2025-10-11
**Status**: LLM integration disabled, awaiting Rig migration
**Next Steps**: Complete Phase 1 foundation before touching CLI commands
