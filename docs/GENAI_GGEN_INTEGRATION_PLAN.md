# genai + ggen Integration Plan

## Overview

This document describes how to integrate the `genai` library (v0.4) with ggen's existing AI infrastructure once the ggen-ai compilation errors are resolved.

## Current Status

### ✅ Completed
- genai dependency updated to v0.4 in Cargo.toml
- Three working genai/Ollama examples created:
  - `examples/genai_ollama_loop.rs` - Interactive conversation
  - `examples/genai_ollama_stream.rs` - Streaming responses
  - `examples/genai_multi_provider_compare.rs` - Multi-provider comparison
- Comprehensive integration guide: `GENAI_OLLAMA_INTEGRATION.md`
- `SparqlGenerator::with_ollama_qwen3_coder()` method available at `ggen-ai/src/generators/sparql.rs:34`

### 🔧 Blocked
- ggen-ai has 9 compilation errors preventing use of SPARQL generator
- Cannot demonstrate full integration until these are resolved

## Integration Architecture

### Pattern 1: genai as LlmClient Adapter

```rust
// Future implementation when ggen-ai compiles
use genai::chat::{ChatMessage, ChatRequest, ChatOptions};
use genai::Client as GenAiClient;
use ggen_ai::client::LlmClient;
use std::sync::Arc;

/// Adapter that implements ggen's LlmClient trait using genai
pub struct GenAiClientAdapter {
    genai_client: GenAiClient,
    model: String,
    options: ChatOptions,
}

impl GenAiClientAdapter {
    pub fn new_ollama_qwen3(model: &str) -> Self {
        Self {
            genai_client: GenAiClient::default(),
            model: model.to_string(),
            options: ChatOptions::default()
                .with_temperature(0.7)
                .with_max_tokens(2048),
        }
    }
}

#[async_trait]
impl LlmClient for GenAiClientAdapter {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_res = self.genai_client
            .exec_chat(&self.model, chat_req, Some(&self.options))
            .await
            .map_err(|e| GgenAiError::client_error(e.to_string()))?;

        Ok(LlmResponse {
            content: chat_res.first_text().unwrap_or("").to_string(),
            usage: chat_res.usage.map(|u| TokenUsage {
                prompt_tokens: u.prompt_tokens.unwrap_or(0),
                completion_tokens: u.completion_tokens.unwrap_or(0),
                total_tokens: u.total_tokens.unwrap_or(0),
            }),
        })
    }

    async fn complete_stream(&self, prompt: &str) -> Result<StreamResponse> {
        // Similar implementation for streaming
        // ...
    }
}
```

### Pattern 2: Direct genai Usage in SPARQL Generator

Once ggen-ai compiles, you can use the existing SPARQL generator with genai:

```rust
use ggen_ai::generators::SparqlGenerator;
use genai_adapter::GenAiClientAdapter;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create genai-backed client
    let client = Arc::new(GenAiClientAdapter::new_ollama_qwen3("qwen3-coder:30b"));

    // Use with existing SPARQL generator
    let generator = SparqlGenerator::with_ollama_qwen3_coder(client);

    let graph = Graph::new()?;
    let query = generator
        .generate_query(&graph, "Find all people with their email addresses")
        .await?;

    println!("Generated SPARQL:\n{}", query);

    Ok(())
}
```

### Pattern 3: MCP Server Integration

Extend ggen-mcp with genai-powered tools:

```rust
// In ggen-mcp/src/server.rs
use genai::Client as GenAiClient;
use genai::chat::{ChatMessage, ChatRequest};

impl GgenMcpServer {
    async fn tool_ai_chat(&self, params: Value) -> Result<Value> {
        let message = params.get("message")
            .and_then(|v| v.as_str())
            .ok_or("message required")?;

        let model = params.get("model")
            .and_then(|v| v.as_str())
            .unwrap_or("qwen3-coder:30b");

        let client = GenAiClient::default();
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system("You are a helpful coding assistant."),
            ChatMessage::user(message),
        ]);

        let response = client
            .exec_chat(model, chat_req, None)
            .await
            .map_err(|e| ErrorData::from(e.to_string()))?;

        Ok(json!({
            "response": response.first_text().unwrap_or("No response"),
            "model": model,
            "usage": response.usage,
        }))
    }
}

// Register in list_tools():
Tool {
    name: "ai_chat".to_string(),
    description: Some("Interactive AI chat using genai library".to_string()),
    input_schema: json!({
        "type": "object",
        "properties": {
            "message": {
                "type": "string",
                "description": "Message to send to AI"
            },
            "model": {
                "type": "string",
                "description": "Model to use (default: qwen3-coder:30b)"
            }
        },
        "required": ["message"]
    }),
}
```

## Benefits of Integration

### 1. Multi-Provider Support
- Switch between Ollama (qwen3-coder:30b), OpenAI, Anthropic, Gemini with same API
- No vendor lock-in
- Automatic API key detection

### 2. Enhanced Features
- Streaming responses for better UX
- Conversation history management
- Token usage tracking
- Error handling and retries

### 3. Ecosystem Compatibility
- genai is actively maintained (v0.4 released 2025-01)
- Large community and examples
- Works with minimal configuration

## Testing Plan

Once ggen-ai compiles, run these tests:

### Test 1: Standalone genai Examples
```bash
# These work NOW (independent of ggen-ai):
cargo run --example genai_ollama_loop
cargo run --example genai_ollama_stream
cargo run --example genai_multi_provider_compare
```

### Test 2: SPARQL Generation with genai
```bash
# Will work after ggen-ai compilation fix:
cargo run --example genai_sparql_generation
```

### Test 3: MCP Server with genai
```bash
# After adding ai_chat tool:
cd ggen-mcp
cargo build --release
./target/release/ggen-mcp
# Test with Claude Code using ai_chat tool
```

## Next Steps

### Immediate (When ggen-ai Compiles)
1. Create `GenAiClientAdapter` in `ggen-ai/src/adapters/genai.rs`
2. Add example: `examples/genai_sparql_generation.rs`
3. Update SPARQL generator tests to use genai
4. Document multi-provider SPARQL generation

### Short Term (1-2 weeks)
1. Add `ai_chat` tool to ggen-mcp server
2. Create examples for all MCP AI tools using genai
3. Add provider selection to CLI: `ggen ai --provider ollama generate-sparql "query"`
4. Performance benchmarks: genai vs direct API calls

### Long Term (1-2 months)
1. Replace all direct API calls with genai
2. Add provider configuration to ggen config file
3. Implement caching layer for repeated queries
4. Add cost tracking across providers

## Code Examples

### Example 1: SPARQL Generation (Future)
```rust
// examples/genai_sparql_generation.rs
use ggen_ai::generators::SparqlGenerator;
use genai_adapter::GenAiClientAdapter;
use ggen_core::Graph;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔮 SPARQL Generation with qwen3-coder:30b");

    // Initialize with Ollama
    let client = Arc::new(GenAiClientAdapter::new_ollama_qwen3("qwen3-coder:30b"));
    let generator = SparqlGenerator::with_ollama_qwen3_coder(client);

    // Load or create graph
    let graph = Graph::new()?;

    // Generate queries
    let queries = vec![
        "Find all people with their email addresses",
        "Get projects created in the last month",
        "List all resources of type Organization",
    ];

    for intent in queries {
        println!("\n📝 Intent: {}", intent);
        let query = generator.generate_query(&graph, intent).await?;
        println!("✅ Generated:\n{}", query);
    }

    Ok(())
}
```

### Example 2: Streaming SPARQL (Future)
```rust
// examples/genai_sparql_stream.rs
use futures::StreamExt;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = Arc::new(GenAiClientAdapter::new_ollama_qwen3("qwen3-coder:30b"));
    let generator = SparqlGenerator::with_ollama_qwen3_coder(client);

    let graph = Graph::new()?;
    let prefixes = [
        ("foaf", "http://xmlns.com/foaf/0.1/"),
        ("dc", "http://purl.org/dc/elements/1.1/"),
    ];

    let mut stream = generator
        .stream_sparql(&graph, "Find all people and their projects", &prefixes)
        .await?;

    print!("Generating: ");
    while let Some(chunk) = stream.next().await {
        print!("{}", chunk?);
        std::io::Write::flush(&mut std::io::stdout())?;
    }
    println!();

    Ok(())
}
```

## Configuration

Add provider configuration to `.ggen.toml`:

```toml
[ai]
default_provider = "ollama"

[ai.providers.ollama]
model = "qwen3-coder:30b"
base_url = "http://localhost:11434"
temperature = 0.7
max_tokens = 2048

[ai.providers.openai]
model = "gpt-4o-mini"
api_key_env = "OPENAI_API_KEY"
temperature = 0.7
max_tokens = 2048

[ai.providers.anthropic]
model = "claude-3-haiku-20240307"
api_key_env = "ANTHROPIC_API_KEY"
temperature = 0.7
max_tokens = 2048
```

## Conclusion

The genai library provides a perfect foundation for ggen's AI features:
- ✅ Single API for multiple providers
- ✅ Active development and maintenance
- ✅ Comprehensive examples and documentation
- ✅ Works with Ollama qwen3-coder:30b out of the box
- ✅ Streaming support for better UX
- ✅ No vendor lock-in

Once ggen-ai compilation is fixed (estimated 50-60 minutes), integration can proceed with the patterns outlined above.

---

**Status:** Ready for implementation pending ggen-ai compilation fix
**Priority:** High - Enables local AI with no API costs
**Effort:** 2-3 days for complete integration
