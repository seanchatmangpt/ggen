# Ollama qwen3-coder:30b Integration with genai

Complete guide for using Ollama's qwen3-coder:30b model with the genai library in the ggen ecosystem.

## 🎯 Overview

The `genai` library provides a unified API for multiple AI providers, including Ollama. This means you can use the same code structure to work with OpenAI, Anthropic, Gemini, and Ollama models.

**Key Benefits:**
- ✅ Single API for all providers
- ✅ Easy model switching
- ✅ Consistent error handling
- ✅ Built-in streaming support
- ✅ Conversation management
- ✅ Token usage tracking

## 📋 Prerequisites

### 1. Install Ollama
```bash
# macOS/Linux
curl -fsSL https://ollama.ai/install.sh | sh

# Or download from https://ollama.ai
```

### 2. Pull qwen3-coder:30b
```bash
ollama pull qwen3-coder:30b

# Verify it's available
ollama list
```

### 3. Start Ollama Server
```bash
# Usually starts automatically, but if not:
ollama serve

# Check status
curl http://localhost:11434/api/tags
```

### 4. Add genai to Your Project
```toml
[dependencies]
genai = "0.4"
tokio = { version = "1", features = ["full"] }
```

## 🚀 Quick Start

### Basic Loop Example

```rust
use genai::chat::{ChatMessage, ChatRequest, ChatOptions};
use genai::Client;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = Client::default();

    let chat_options = ChatOptions::default()
        .with_temperature(0.7)
        .with_max_tokens(2048);

    let mut conversation = vec![
        ChatMessage::system("You are a helpful Rust coding assistant."),
    ];

    loop {
        // Get user input
        let user_input = read_user_input()?;

        if user_input == "quit" {
            break;
        }

        // Add to conversation
        conversation.push(ChatMessage::user(user_input));

        // Create request
        let chat_req = ChatRequest::new(conversation.clone());

        // Call Ollama
        let chat_res = client
            .exec_chat("qwen3-coder:30b", chat_req, Some(&chat_options))
            .await?;

        // Get response
        if let Some(response) = chat_res.first_text() {
            println!("Assistant: {}", response);
            conversation.push(ChatMessage::assistant(response));
        }
    }

    Ok(())
}
```

## 📚 Examples

We've created three complete examples:

### 1. Interactive Loop ([examples/genai_ollama_loop.rs](../examples/genai_ollama_loop.rs))
```bash
cargo run --example genai_ollama_loop
```

Features:
- Interactive conversation loop
- Conversation history management
- Command handling (quit, clear)
- Token usage display
- Error handling with helpful messages

### 2. Streaming Responses ([examples/genai_ollama_stream.rs](../examples/genai_ollama_stream.rs))
```bash
cargo run --example genai_ollama_stream
```

Features:
- Real-time response streaming
- Progressive text display
- Same conversation management
- Better user experience for long responses

### 3. Multi-Provider Comparison ([examples/genai_multi_provider_compare.rs](../examples/genai_multi_provider_compare.rs))
```bash
cargo run --example genai_multi_provider_compare
```

Features:
- Compare responses from multiple providers
- Side-by-side performance metrics
- Automatic API key detection
- Skip unavailable providers

## 🔧 Configuration Options

### Chat Options

```rust
use genai::chat::ChatOptions;

let options = ChatOptions::default()
    .with_temperature(0.7)         // Creativity (0.0-2.0)
    .with_max_tokens(2048)          // Max response length
    .with_top_p(0.9)                // Nucleus sampling
    .with_capture_raw_body(true);   // Capture raw response

// Use with request
let chat_res = client
    .exec_chat(model, chat_req, Some(&options))
    .await?;
```

### System Prompts

```rust
let system_prompts = vec![
    // Code generation specialist
    "You are an expert Rust programmer. Provide idiomatic, safe code with explanations.",

    // Architecture expert
    "You are a software architect. Focus on design patterns, scalability, and maintainability.",

    // Teaching assistant
    "You are a patient teacher. Explain concepts step-by-step with examples.",

    // Code reviewer
    "You are a code reviewer. Identify issues, suggest improvements, and explain best practices.",
];
```

### Model Selection

```rust
// Ollama models (no API key needed)
const OLLAMA_MODELS: &[&str] = &[
    "qwen3-coder:30b",      // Best for code
    "codellama:70b",        // Alternative
    "deepseek-coder:33b",   // Another option
    "llama3:70b",           // General purpose
];

// Cloud models (API key required)
const CLOUD_MODELS: &[&str] = &[
    "gpt-4o",               // OpenAI
    "claude-3-opus",        // Anthropic
    "gemini-2.0-flash",     // Google
];
```

## 🎨 Use Cases

### 1. Code Generation
```rust
let chat_req = ChatRequest::new(vec![
    ChatMessage::system("You are an expert code generator."),
    ChatMessage::user("Create a Rust async HTTP client with retry logic"),
]);

let response = client
    .exec_chat("qwen3-coder:30b", chat_req, None)
    .await?;
```

### 2. Code Review
```rust
let code_to_review = r#"
    fn process_data(data: Vec<String>) -> Vec<String> {
        data.iter().map(|s| s.to_uppercase()).collect()
    }
"#;

let chat_req = ChatRequest::new(vec![
    ChatMessage::system("You are a code reviewer. Provide constructive feedback."),
    ChatMessage::user(format!("Review this Rust code:\n{}", code_to_review)),
]);
```

### 3. Documentation Generation
```rust
let chat_req = ChatRequest::new(vec![
    ChatMessage::system("You generate clear, comprehensive documentation."),
    ChatMessage::user("Document this function with examples and edge cases"),
]);
```

### 4. Bug Fixing
```rust
let chat_req = ChatRequest::new(vec![
    ChatMessage::system("You are a debugging expert."),
    ChatMessage::user("This code panics with 'index out of bounds'. Help me fix it."),
]);
```

### 5. Architecture Design
```rust
let chat_req = ChatRequest::new(vec![
    ChatMessage::system("You are a software architect."),
    ChatMessage::user("Design a scalable microservices architecture for an e-commerce platform"),
]);
```

## 🔄 Conversation Management

### Basic Conversation
```rust
let mut conversation = vec![
    ChatMessage::system("You are helpful assistant."),
];

// Add messages
conversation.push(ChatMessage::user("Hello"));
conversation.push(ChatMessage::assistant("Hi! How can I help?"));
conversation.push(ChatMessage::user("Tell me about Rust"));

// Use in request
let chat_req = ChatRequest::new(conversation.clone());
```

### Context Window Management
```rust
fn trim_conversation(conversation: &mut Vec<ChatMessage>, max_messages: usize) {
    if conversation.len() > max_messages {
        // Keep system message + recent messages
        let system_msg = conversation[0].clone();
        let recent: Vec<_> = conversation
            .iter()
            .skip(conversation.len() - max_messages + 1)
            .cloned()
            .collect();

        *conversation = vec![system_msg];
        conversation.extend(recent);
    }
}
```

### Conversation Branching
```rust
// Save conversation state
let checkpoint = conversation.clone();

// Try different approaches
conversation.push(ChatMessage::user("Approach 1"));
// ... get response ...

// Restore and try different approach
conversation = checkpoint.clone();
conversation.push(ChatMessage::user("Approach 2"));
// ... get response ...
```

## 📊 Token Usage and Monitoring

### Track Token Usage
```rust
let chat_res = client
    .exec_chat("qwen3-coder:30b", chat_req, None)
    .await?;

if let Some(usage) = chat_res.usage {
    println!("Input tokens: {}", usage.prompt_tokens.unwrap_or(0));
    println!("Output tokens: {}", usage.completion_tokens.unwrap_or(0));
    println!("Total tokens: {}", usage.total_tokens.unwrap_or(0));
}
```

### Cost Estimation (for cloud providers)
```rust
fn estimate_cost(usage: &Usage, model: &str) -> f64 {
    let (input_cost, output_cost) = match model {
        "gpt-4o" => (0.005, 0.015),           // per 1K tokens
        "claude-3-opus" => (0.015, 0.075),
        "gemini-2.0-flash" => (0.0, 0.0),     // Free tier
        _ => (0.0, 0.0),                       // Ollama is free
    };

    let input_tokens = usage.prompt_tokens.unwrap_or(0) as f64 / 1000.0;
    let output_tokens = usage.completion_tokens.unwrap_or(0) as f64 / 1000.0;

    (input_tokens * input_cost) + (output_tokens * output_cost)
}
```

## 🚨 Error Handling

### Comprehensive Error Handling
```rust
use genai::chat::ChatError;

match client.exec_chat(model, chat_req, None).await {
    Ok(response) => {
        // Handle success
        println!("Response: {:?}", response.first_text());
    }
    Err(e) => {
        // Handle different error types
        eprintln!("Error: {}", e);

        // Provide user-friendly messages
        if e.to_string().contains("connection") {
            eprintln!("💡 Check if Ollama is running: ollama serve");
        } else if e.to_string().contains("model") {
            eprintln!("💡 Pull the model: ollama pull qwen3-coder:30b");
        } else if e.to_string().contains("timeout") {
            eprintln!("💡 Request timed out. Try a smaller prompt or increase timeout.");
        }
    }
}
```

### Retry Logic
```rust
async fn chat_with_retry(
    client: &Client,
    model: &str,
    chat_req: ChatRequest,
    max_retries: u32,
) -> Result<ChatResponse, Box<dyn std::error::Error>> {
    let mut retries = 0;

    loop {
        match client.exec_chat(model, chat_req.clone(), None).await {
            Ok(response) => return Ok(response),
            Err(e) if retries < max_retries => {
                retries += 1;
                eprintln!("Retry {}/{}: {}", retries, max_retries, e);
                tokio::time::sleep(tokio::time::Duration::from_secs(2u64.pow(retries))).await;
            }
            Err(e) => return Err(e.into()),
        }
    }
}
```

## 🔐 Multi-Provider Support

### Dynamic Provider Selection
```rust
async fn select_best_available_model(client: &Client) -> String {
    let models = vec![
        ("qwen3-coder:30b", ""),              // Try Ollama first (free)
        ("gpt-4o-mini", "OPENAI_API_KEY"),
        ("claude-3-haiku", "ANTHROPIC_API_KEY"),
    ];

    for (model, env_key) in models {
        if env_key.is_empty() || std::env::var(env_key).is_ok() {
            // Test if model is accessible
            let test_req = ChatRequest::new(vec![
                ChatMessage::user("test"),
            ]);

            if client.exec_chat(model, test_req, None).await.is_ok() {
                return model.to_string();
            }
        }
    }

    panic!("No models available!");
}
```

## 📝 Best Practices

### 1. Model Selection
- **Ollama (qwen3-coder:30b)**: Best for privacy, offline use, no cost
- **GPT-4**: Best for complex reasoning
- **Claude**: Best for long contexts
- **Gemini**: Best for multimodal tasks

### 2. Temperature Settings
```rust
// Code generation (deterministic)
.with_temperature(0.2)

// Creative writing
.with_temperature(0.9)

// Balanced (default)
.with_temperature(0.7)
```

### 3. Token Limits
```rust
// Short responses (summaries, code snippets)
.with_max_tokens(512)

// Medium responses (explanations)
.with_max_tokens(1024)

// Long responses (full implementations)
.with_max_tokens(2048)

// Very long (documentation, tutorials)
.with_max_tokens(4096)
```

### 4. System Prompts
- Be specific about role and expertise
- Include formatting preferences
- Specify constraints (code style, length)
- Add examples if needed

## 🔗 Integration with ggen

### Current Status

**✅ Working Now:**
- Standalone genai examples (genai_ollama_loop, genai_ollama_stream, genai_multi_provider_compare)
- genai dependency updated to v0.4 in Cargo.toml
- `SparqlGenerator::with_ollama_qwen3_coder()` method added to ggen-ai

**🔧 Blocked Until ggen-ai Compiles:**
- Direct use of ggen's SPARQL generator with genai
- Full MCP server integration with AI tools
- Template generation with AI

**📋 See `/docs/GENAI_GGEN_INTEGRATION_PLAN.md` for:**
- Complete integration architecture
- Adapter pattern for LlmClient
- MCP server integration examples
- Testing plan
- Configuration guidelines

### Use with Minimal MCP Server (Works Now)
```rust
// Extend minimal-mcp-server with genai
use genai::Client;
use genai::chat::{ChatMessage, ChatRequest};

impl MinimalMcpServer {
    async fn tool_ai_complete(&self, params: Value) -> Result<Value, String> {
        let prompt = params.get("prompt")
            .and_then(|v| v.as_str())
            .ok_or("prompt required")?;

        let client = Client::default();
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let response = client
            .exec_chat("qwen3-coder:30b", chat_req, None)
            .await
            .map_err(|e| e.to_string())?;

        Ok(json!({
            "response": response.first_text().unwrap_or("No response"),
        }))
    }
}
```

### Future: SPARQL Generator with genai (After ggen-ai Fix)
```rust
// Will work once ggen-ai compiles
use ggen_ai::generators::SparqlGenerator;
use genai_adapter::GenAiClientAdapter;
use std::sync::Arc;

let client = Arc::new(GenAiClientAdapter::new_ollama_qwen3("qwen3-coder:30b"));
let generator = SparqlGenerator::with_ollama_qwen3_coder(client);

let query = generator
    .generate_query(&graph, "Find all people with email addresses")
    .await?;
```

## 🎯 Next Steps

1. **Try the examples:**
   ```bash
   cargo run --example genai_ollama_loop
   cargo run --example genai_ollama_stream
   cargo run --example genai_multi_provider_compare
   ```

2. **Read the genai docs:**
   - https://crates.io/crates/genai
   - https://github.com/jeremychone/rust-genai

3. **Fix ggen-ai compilation:**
   - Follow `/docs/INTEGRATION_STATUS_AND_NEXT_STEPS.md`
   - Enable full MCP integration with AI features

4. **Build your own tools:**
   - Extend minimal-mcp-server
   - Add genai-powered tools
   - Create custom workflows

---

**The genai library provides a perfect foundation for AI-powered code generation in the ggen ecosystem!** 🚀
