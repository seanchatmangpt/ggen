# ggen-ai

AI-powered code generation capabilities for ggen - LLM integration for intelligent template generation and graph operations.

## Features

- **Multi-provider LLM support**: OpenAI, Anthropic, Ollama
- **Intelligent template generation**: Natural language to ggen templates
- **SPARQL query generation**: Intent-based query construction
- **Ontology generation**: Domain descriptions to RDF/OWL
- **Code refactoring**: AI-assisted code improvement suggestions
- **MCP server integration**: Expose AI capabilities via Model Context Protocol

## Quick Start

### Installation

```bash
# Install dependencies
cargo build --release

# Set up environment variables
export OPENAI_API_KEY="your-openai-key"
# OR
export ANTHROPIC_API_KEY="your-anthropic-key"
# OR
export USE_OLLAMA=true
```

### Basic Usage

```rust
use ggen_ai::{LlmClient, TemplateGenerator};
use ggen_ai::providers::OpenAIClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize LLM client
    let client = OpenAIClient::new("your-api-key".to_string());
    
    // Generate template from description
    let generator = TemplateGenerator::new(Box::new(client));
    let template = generator.generate_template(
        "Generate a REST API controller for user management",
        vec!["Include CRUD operations", "Use TypeScript"]
    ).await?;
    
    println!("Generated template: {}", template);
    Ok(())
}
```

### MCP Server

```bash
# Start the MCP server
cargo run --bin ggen-ai-mcp

# Or with specific client
OPENAI_API_KEY="your-key" cargo run --bin ggen-ai-mcp
```

## API Reference

### Template Generation

```rust
use ggen_ai::generators::TemplateGenerator;

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

// Generate with custom requirements
let template = generator.generate_with_requirements(
    "E-commerce system",
    vec!["Include payment processing", "Add inventory management"],
    vec!["User registration", "Product catalog"],
    Some("TypeScript"),
    Some("Next.js")
).await?;
```

### SPARQL Query Generation

```rust
use ggen_ai::generators::SparqlGenerator;

// Generate query from intent
let query = generator.generate_query(
    &graph,
    "Find all users with email addresses"
).await?;

// Generate specific query types
let query = generator.generate_find_instances(
    &graph,
    "ex:Person"
).await?;

let query = generator.generate_find_properties(
    &graph,
    "ex:user1"
).await?;
```

### Ontology Generation

```rust
use ggen_ai::generators::OntologyGenerator;

// Generate ontology from domain
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
use ggen_ai::generators::{RefactorAssistant, RefactoringContext};

// Suggest refactoring improvements
let context = RefactoringContext::new("TypeScript".to_string())
    .with_framework("React".to_string())
    .with_focus_areas(vec!["performance".to_string(), "readability".to_string()]);

let suggestions = assistant.suggest_refactoring(&code, &context).await?;

// Apply refactoring with three-way merge
let refactored = assistant.apply_refactoring(
    &original_code,
    suggestions,
    MergeStrategy::GeneratedWins
).await?;
```

## MCP Tools

The ggen-ai MCP server provides the following tools:

### `ai_generate_template`
Generate ggen templates from natural language descriptions.

**Parameters:**
- `description` (string, required): Natural language description
- `examples` (array, optional): Example requirements
- `language` (string, optional): Target programming language
- `framework` (string, optional): Target framework

### `ai_generate_sparql`
Generate SPARQL queries from natural language intent.

**Parameters:**
- `intent` (string, required): Natural language description
- `graph` (string, required): RDF graph data in Turtle format

### `ai_generate_ontology`
Generate RDF/OWL ontologies from domain descriptions.

**Parameters:**
- `domain` (string, required): Domain description
- `requirements` (array, optional): Specific requirements

### `ai_refactor_code`
Suggest code refactoring improvements using AI.

**Parameters:**
- `code` (string, required): Code to refactor
- `language` (string, optional): Programming language

### `ai_explain_graph`
Explain RDF graph content in natural language.

**Parameters:**
- `graph` (string, required): RDF graph data in Turtle format
- `focus` (string, optional): What aspect to focus on

### `ai_suggest_delta`
Suggest intelligent merge strategies for delta-driven projection.

**Parameters:**
- `baseline` (string, required): Baseline version
- `current` (string, required): Current generated version
- `manual` (string, optional): Manual modifications

## Configuration

### Environment Variables

- `OPENAI_API_KEY`: OpenAI API key
- `ANTHROPIC_API_KEY`: Anthropic API key
- `USE_OLLAMA`: Set to "true" to use Ollama (default: false)
- `RUST_LOG`: Logging level (default: "ggen_ai=info")

### LLM Configuration

```rust
use ggen_ai::client::{LlmConfig, LlmClient};

let config = LlmConfig {
    model: "gpt-4".to_string(),
    max_tokens: Some(4096),
    temperature: Some(0.7),
    top_p: Some(1.0),
    stop: Some(vec!["```".to_string()]),
    extra: HashMap::new(),
};

let generator = TemplateGenerator::with_config(client, config);
```

## Examples

### Complete Template Generation Workflow

```rust
use ggen_ai::{LlmClient, TemplateGenerator};
use ggen_ai::providers::OpenAIClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize client
    let client = OpenAIClient::new("your-api-key".to_string());
    let generator = TemplateGenerator::new(Box::new(client));
    
    // Generate template
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

# Run with logging
RUST_LOG=debug cargo test
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

MIT
