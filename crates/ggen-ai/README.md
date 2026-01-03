# ggen-ai

**AI-powered code generation capabilities for ggen** - Unified LLM integration using `rust-genai` for intelligent template generation, SPARQL queries, and RDF graph operations.

## 🚀 **NEW: v1.0.0 with rust-genai Integration**

**Major Update:** Complete migration from custom LLM clients to `rust-genai` for production-ready multi-provider AI integration.

## Features

- **🔧 Multi-provider LLM support**: OpenAI, Anthropic, Ollama via rust-genai
- **🤖 Intelligent template generation**: Natural language to ggen templates
- **🔍 SPARQL query generation**: Intent-based query construction from RDF graphs
- **📊 Ontology generation**: Domain descriptions to RDF/OWL schemas
- **🔄 Code refactoring**: AI-assisted code improvement suggestions
- **🎪 MCP server integration**: Model Context Protocol for AI tool integration
- **⚡ Production-ready**: Structured error handling, configuration management, and comprehensive testing

## Quick Start

### Installation

```bash
# Add to your Cargo.toml
[dependencies]
ggen-ai = "1.0"
dotenvy = "0.15"  # For environment configuration
tokio = { version = "1.0", features = ["full"] }
```

### Basic Setup

### Basic Usage

```rust
use ggen_ai::{LlmClient, TemplateGenerator, LlmConfig};
use ggen_ai::client::GenAiClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load environment configuration
    dotenvy::dotenv().ok();

    // Configure LLM client (supports OpenAI, Anthropic, Ollama)
    let config = LlmConfig {
        model: "gpt-4o".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    // Initialize unified LLM client
    let client = GenAiClient::new(config)?;
    let generator = TemplateGenerator::new(Box::new(client));

    // Generate template from description
    let template = generator.generate_template(
        "Generate a REST API controller for user management",
        vec!["Include CRUD operations", "Use TypeScript"]
    ).await?;

    println!("Generated template: {}", template);
    Ok(())
}
```

### CLI Usage

```bash
# Generate template using AI
ggen ai generate -d "Database model" --provider openai --model gpt-4o

# Generate SPARQL query from graph
ggen ai sparql -d "Find all users" -g schema.ttl --provider anthropic

# Generate RDF ontology
ggen ai graph -d "E-commerce ontology" -o products.ttl --provider ollama

# Start MCP server for AI tools
ggen ai server --provider openai --model gpt-4o
```

## API Reference

### Core Types

```rust
use ggen_ai::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use ggen_ai::client::GenAiClient;

// Configuration for all LLM providers
#[derive(Debug, Clone)]
pub struct LlmConfig {
    pub model: String,
    pub max_tokens: Option<u32>,
    pub temperature: Option<f32>,
    pub top_p: Option<f32>,
    pub stop: Option<Vec<String>>,
    pub extra: HashMap<String, Value>,
}

// Response from LLM completion
#[derive(Debug, Clone)]
pub struct LlmResponse {
    pub content: String,
    pub usage: Option<UsageStats>,
    pub model: String,
    pub finish_reason: Option<String>,
    pub extra: HashMap<String, Value>,
}

// Streaming chunk from LLM
#[derive(Debug, Clone)]
pub struct LlmChunk {
    pub content: String,
    pub model: String,
    pub finish_reason: Option<String>,
    pub usage: Option<UsageStats>,
    pub extra: HashMap<String, Value>,
}

// Usage statistics
#[derive(Debug, Clone)]
pub struct UsageStats {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}
```

### Template Generation

```rust
use ggen_ai::generators::TemplateGenerator;

// Create generator with LLM client
let generator = TemplateGenerator::new(Box::new(client));

// Generate REST API controller
let template = generator.generate_rest_controller(
    "User management API",
    "TypeScript",
    "Express"
).await?;

// Generate data model
let template = generator.generate_data_model(
    "User entity",
    "Rust"
).await?;

// Generate from natural language description
let template = generator.generate_template(
    "E-commerce system with payment processing",
    vec!["Include inventory management", "Add user registration"]
).await?;
```

### SPARQL Query Generation

```rust
use ggen_ai::generators::SparqlGenerator;
use ggen_core::Graph;

// Create generator with LLM client
let generator = SparqlGenerator::new(Box::new(client));

// Generate query from natural language intent
let query = generator.generate_query(
    &graph,
    "Find all users with email addresses"
).await?;

// Generate query with specific intent
let query = generator.generate_query_with_intent(
    &graph,
    "Find all people and their properties"
).await?;
```

### Ontology Generation

```rust
use ggen_ai::generators::OntologyGenerator;

// Create generator with LLM client
let generator = OntologyGenerator::new(Box::new(client));

// Generate ontology from domain description
let ontology = generator.generate_ontology(
    "E-commerce system",
    vec!["Include Product and Customer classes", "Add Order relationships"]
).await?;

// Generate domain-specific ontology
let ontology = generator.generate_domain_ontology(
    "Healthcare",
    vec!["Patient", "Doctor", "Appointment"],
    vec!["hasAppointment", "treats", "schedules"]
).await?;
```

### Code Refactoring

```rust
use ggen_ai::generators::RefactorAssistant;

// Create refactoring assistant with LLM client
let assistant = RefactorAssistant::new(Box::new(client));

// Suggest refactoring improvements
let suggestions = assistant.suggest_refactoring(
    &code,
    "TypeScript",
    vec!["performance", "readability"]
).await?;

// Get detailed suggestions with explanations
for suggestion in suggestions {
    println!("Suggestion: {}", suggestion.description);
    println!("Impact: {:?}", suggestion.impact);
    println!("Confidence: {:.2}", suggestion.confidence);
}
```

## MCP Tools

The ggen-ai MCP server provides the following tools for AI assistant integration:

### `ai_generate_template`
Generate ggen templates from natural language descriptions.

**Parameters:**
- `description` (string, required): Natural language description
- `examples` (array, optional): Example requirements or context
- `language` (string, optional): Target programming language
- `framework` (string, optional): Target framework

### `ai_generate_sparql`
Generate SPARQL queries from natural language intent and RDF graphs.

**Parameters:**
- `intent` (string, required): Natural language query description
- `graph` (string, required): RDF graph data in Turtle format

### `ai_generate_ontology`
Generate RDF/OWL ontologies from domain descriptions.

**Parameters:**
- `domain` (string, required): Domain description
- `requirements` (array, optional): Specific requirements or classes

### `ai_refactor_code`
Suggest code refactoring improvements using AI analysis.

**Parameters:**
- `code` (string, required): Code to analyze and refactor
- `language` (string, optional): Programming language for context

### `ai_explain_graph`
Explain RDF graph content in natural language.

**Parameters:**
- `graph` (string, required): RDF graph data in Turtle format
- `focus` (string, optional): Specific aspect to explain

### `ai_suggest_delta`
Suggest intelligent merge strategies for delta-driven projection.

**Parameters:**
- `baseline` (string, required): Baseline version
- `current` (string, required): Current generated version
- `manual` (string, optional): Manual modifications made

## Configuration

### Environment Variables

Configure LLM providers using environment variables:

```bash
# OpenAI Configuration
export OPENAI_API_KEY="sk-your-openai-key"
export OPENAI_BASE_URL="https://api.openai.com/v1"  # Optional custom endpoint
export OPENAI_DEFAULT_MODEL="gpt-4o"

# Anthropic Configuration
export ANTHROPIC_API_KEY="sk-ant-your-anthropic-key"
export ANTHROPIC_BASE_URL="https://api.anthropic.com/v1"
export ANTHROPIC_DEFAULT_MODEL="claude-haiku-4-5-20251001"

# Ollama Configuration (local models)
export OLLAMA_BASE_URL="http://localhost:11434"
export OLLAMA_DEFAULT_MODEL="ministral-3:3b"

# Global Configuration
export AI_DEFAULT_PROVIDER="openai"
export RUST_LOG="ggen_ai=info"
```

### Programmatic Configuration

```rust
use ggen_ai::{LlmConfig, GenAiClient};

// Configure for OpenAI
let config = LlmConfig {
    model: "gpt-4o".to_string(),
    max_tokens: Some(4096),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: std::collections::HashMap::new(),
};

// Create client
let client = GenAiClient::new(config)?;
let generator = TemplateGenerator::new(Box::new(client));
```

## Examples

### Complete Template Generation Workflow

```rust
use ggen_ai::{LlmClient, TemplateGenerator, LlmConfig, GenAiClient};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load environment configuration
    dotenvy::dotenv().ok();

    // Configure and initialize LLM client
    let config = LlmConfig {
        model: "gpt-4o".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    let client = GenAiClient::new(config)?;
    let generator = TemplateGenerator::new(Box::new(client));

    // Generate REST API controller
    let template = generator.generate_rest_controller(
        "User management API with authentication",
        "TypeScript",
        "Express"
    ).await?;

    // Save template
    std::fs::write("user-api.tmpl", template.content)?;

    println!("Template generated successfully!");
    Ok(())
}
```

### SPARQL Query Generation

```rust
use ggen_ai::generators::SparqlGenerator;
use ggen_core::Graph;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load graph
    let graph = Graph::new()?;
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:user1 a ex:Person ;
            ex:hasName "John Doe" ;
            ex:hasEmail "john@example.com" .
        ex:user2 a ex:Person ;
            ex:hasName "Jane Smith" ;
            ex:hasEmail "jane@example.com" .
    "#)?;
    
    // Generate query
    let generator = SparqlGenerator::new(Box::new(client));
    let query = generator.generate_query(
        &graph,
        "Find all people with email addresses"
    ).await?;
    
    println!("Generated query: {}", query);
    
    // Execute query
    let results = graph.query(&query)?;
    println!("Results: {:?}", results);
    
    Ok(())
}
```

## Testing

```bash
# Run unit tests
cargo test

# Run integration tests
cargo test --test integration

# Run with debug logging
RUST_LOG=ggen_ai=debug cargo test

# Test specific provider
OPENAI_API_KEY="test-key" cargo test test_openai_client
```

## Contributing

1. **Follow core team best practices** - Use `cargo make` commands, no direct `cargo` usage
2. **Add comprehensive tests** - Unit, integration, and property tests
3. **Update documentation** - Keep README and guides current
4. **Use structured error handling** - No `.unwrap()` or `.expect()` in library code

## Migration from v0.x

**Major Update:** ggen-ai v1.0.0 migrates from custom LLM clients to `rust-genai` for production-ready multi-provider support.

- **Breaking Changes:** Provider initialization now uses configuration objects
- **New Features:** Environment-based configuration, structured error handling
- **Migration Guide:** See [docs/ggen-ai-migration-guide.md](../docs/ggen-ai-migration-guide.md)

## License

MIT

