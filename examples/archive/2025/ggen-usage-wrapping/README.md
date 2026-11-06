# ggen Library Usage & Wrapping Examples

This comprehensive guide demonstrates how to use **ggen-core** and **ggen-ai** as libraries in your Rust projects, including various wrapper implementations.

## 📚 Table of Contents

- [Overview](#overview)
- [Core Concepts](#core-concepts)
- [Library Usage Examples](#library-usage-examples)
- [Wrapper Examples](#wrapper-examples)
- [API Reference](#api-reference)
- [Best Practices](#best-practices)

## Overview

ggen is a powerful graph-aware code generation framework consisting of two main crates:

### ggen-core
Core generation engine providing:
- **Template Processing**: Tera-based templating with RDF/SPARQL integration
- **Graph Operations**: RDF/OWL graph manipulation and queries
- **Pipeline System**: Composable generation pipelines
- **Cache Management**: Intelligent caching for performance
- **Registry Integration**: Package and template registry access

### ggen-ai
AI-powered capabilities including:
- **Multi-provider LLM Support**: OpenAI, Anthropic, Ollama, Gemini, DeepSeek, xAI/Grok, Groq, Cohere
- **Template Generation**: Natural language to ggen templates
- **SPARQL Generation**: Intent-based query construction
- **Ontology Generation**: Domain descriptions to RDF/OWL
- **Code Refactoring**: AI-assisted code improvements
- **Agent System**: Autonomous code generation agents

## Core Concepts

### 1. Template System

Templates are the foundation of ggen's code generation:

```rust
use ggen_core::Template;

// Load from file
let template = Template::from_file("path/to/template.md")?;

// Load from string
let template = Template::from_str(content)?;

// Access template metadata
println!("Name: {}", template.metadata.name);
println!("Description: {}", template.metadata.description);
```

### 2. Generator

The Generator processes templates with context:

```rust
use ggen_core::{Generator, GenContext};
use std::collections::HashMap;

// Create context
let mut context = GenContext::new();
context.insert("name", "MyProject");
context.insert("version", "1.0.0");

// Create generator
let generator = Generator::new(vec![], HashMap::new())?;

// Generate code
let output = generator.generate(&template, &context).await?;
```

### 3. Pipeline System

Build complex generation workflows:

```rust
use ggen_core::{Pipeline, PipelineBuilder};

let pipeline = PipelineBuilder::new()
    .with_template("template1.md")
    .with_template("template2.md")
    .with_output_dir("./output")
    .build()?;

// Execute pipeline
pipeline.execute(&context).await?;
```

### 4. Graph Operations

Work with RDF/SPARQL graphs:

```rust
use ggen_core::Graph;

let mut graph = Graph::new()?;

// Add triples
graph.add_triple(
    "http://example.org/Person",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    "http://www.w3.org/2002/07/owl#Class"
)?;

// Query with SPARQL
let query = r#"
    SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
    }
"#;
let results = graph.query(query)?;
```

### 5. LLM Integration

Use AI capabilities:

```rust
use ggen_ai::{LlmClient, GenAiClient, TemplateGenerator};

// Create LLM client
let client = GenAiClient::new("gpt-4o", "your-api-key")?;

// Generate template from description
let generator = TemplateGenerator::new(Box::new(client));
let template = generator.generate_template(
    "Create a REST API controller for user management",
    vec!["Include CRUD operations", "Use TypeScript"]
).await?;
```

## Library Usage Examples

### Example 1: Basic Template Processing

```bash
cargo run --example basic-usage
```

Demonstrates:
- Loading templates from files and strings
- Creating generation context
- Rendering templates with variables
- Error handling

**File**: `examples/basic-usage.rs`

### Example 2: AI-Powered Template Generation

```bash
# Set up your API key
export OPENAI_API_KEY="your-key"
cargo run --example with-ai
```

Demonstrates:
- Configuring LLM clients
- Generating templates from natural language
- Validating generated templates
- Using different AI providers

**File**: `examples/with-ai.rs`

### Example 3: Custom Pipeline Creation

```bash
cargo run --example custom-pipeline
```

Demonstrates:
- Building multi-stage pipelines
- Chaining template transformations
- Pipeline error recovery
- Output organization

**File**: `examples/custom-pipeline.rs`

### Example 4: Batch Processing

```bash
cargo run --example batch-processor
```

Demonstrates:
- Processing multiple templates concurrently
- Parallel execution with Tokio
- Progress tracking
- Result aggregation

**File**: `examples/batch-processor.rs`

### Example 5: Graph Operations

```bash
cargo run --example graph-operations
```

Demonstrates:
- Creating and manipulating RDF graphs
- SPARQL query execution
- Graph serialization (Turtle, JSON-LD)
- Ontology integration

**File**: `examples/graph-operations.rs`

### Example 6: Template Validation

```bash
cargo run --example template-validation
```

Demonstrates:
- Validating template syntax
- Checking template metadata
- Ensuring template security
- Custom validation rules

**File**: `examples/template-validation.rs`

## Wrapper Examples

### 1. REST API Wrapper

Expose ggen functionality via HTTP endpoints:

```bash
cd wrappers/rest-api
cargo run
```

**Features**:
- RESTful API for template generation
- WebSocket streaming for real-time updates
- Authentication and rate limiting
- OpenAPI documentation

**Endpoints**:
- `POST /api/generate` - Generate code from template
- `POST /api/templates` - Create new template
- `GET /api/templates/:id` - Get template details
- `POST /api/ai/generate` - AI-powered generation

**File**: `wrappers/rest-api/src/main.rs`

### 2. Custom CLI Wrapper

Build custom command-line tools with ggen:

```bash
cd wrappers/custom-cli
cargo run -- generate --template mytemplate.md --output ./out
```

**Features**:
- Custom commands and subcommands
- Interactive mode with prompts
- Configuration file support
- Shell completion

**Commands**:
- `generate` - Generate code from templates
- `validate` - Validate templates
- `ai` - AI-powered operations
- `graph` - Graph operations

**File**: `wrappers/custom-cli/src/main.rs`

### 3. Plugin System

Extend ggen with custom plugins:

```bash
cd wrappers/plugin-system
cargo run
```

**Features**:
- Dynamic plugin loading
- Plugin lifecycle management
- Custom generator extensions
- Hook system for events

**Plugin Types**:
- **Template Plugins**: Custom template processors
- **Generator Plugins**: Custom generation logic
- **Validator Plugins**: Custom validation rules
- **Output Plugins**: Custom output formatters

**File**: `wrappers/plugin-system/src/main.rs`

## API Reference

### ggen-core Public API

#### Template
```rust
pub struct Template {
    pub metadata: TemplateMetadata,
    pub content: String,
}

impl Template {
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self>;
    pub fn from_str(content: &str) -> Result<Self>;
    pub fn render(&self, context: &GenContext) -> Result<String>;
}
```

#### Generator
```rust
pub struct Generator {
    // Internal fields
}

impl Generator {
    pub fn new(
        templates: Vec<String>,
        context: HashMap<String, String>
    ) -> Result<Self>;

    pub async fn generate(
        &self,
        template: &Template,
        context: &GenContext
    ) -> Result<String>;
}
```

#### Pipeline
```rust
pub struct Pipeline {
    // Internal fields
}

pub struct PipelineBuilder {
    // Internal fields
}

impl PipelineBuilder {
    pub fn new() -> Self;
    pub fn with_template(mut self, path: &str) -> Self;
    pub fn with_output_dir(mut self, dir: &str) -> Self;
    pub fn build(self) -> Result<Pipeline>;
}

impl Pipeline {
    pub async fn execute(&self, context: &GenContext) -> Result<()>;
}
```

#### Graph
```rust
pub struct Graph {
    // Internal fields
}

impl Graph {
    pub fn new() -> Result<Self>;
    pub fn add_triple(&mut self, s: &str, p: &str, o: &str) -> Result<()>;
    pub fn query(&self, sparql: &str) -> Result<Vec<QuerySolution>>;
    pub fn to_turtle(&self) -> Result<String>;
}
```

### ggen-ai Public API

#### LlmClient
```rust
#[async_trait]
pub trait LlmClient: Send + Sync {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    async fn complete_stream(&self, prompt: &str) -> Result<Pin<Box<dyn Stream<Item = Result<LlmChunk>>>>>;
}

pub struct GenAiClient {
    // Internal fields
}

impl GenAiClient {
    pub fn new(model: &str, api_key: &str) -> Result<Self>;
}
```

#### TemplateGenerator
```rust
pub struct TemplateGenerator {
    // Internal fields
}

impl TemplateGenerator {
    pub fn new(client: Box<dyn LlmClient>) -> Self;

    pub async fn generate_template(
        &self,
        description: &str,
        requirements: Vec<&str>
    ) -> Result<Template>;
}
```

#### TemplateValidator
```rust
pub struct TemplateValidator {
    // Internal fields
}

impl TemplateValidator {
    pub fn new(client: Box<dyn LlmClient>) -> Self;

    pub async fn validate(
        &self,
        template: &Template
    ) -> Result<Vec<ValidationIssue>>;
}
```

#### Agent System
```rust
pub struct Agent {
    // Internal fields
}

pub struct AgentRegistry {
    // Internal fields
}

impl AgentRegistry {
    pub fn new() -> Self;
    pub fn register(&mut self, agent: Agent) -> Result<()>;
    pub fn get(&self, name: &str) -> Option<&Agent>;
}
```

## Best Practices

### 1. Error Handling

Always use proper error handling:

```rust
use anyhow::{Context, Result};

pub async fn generate_safe(template_path: &str) -> Result<String> {
    let template = Template::from_file(template_path)
        .context("Failed to load template")?;

    let generator = Generator::new(vec![], HashMap::new())
        .context("Failed to create generator")?;

    let context = GenContext::new();

    generator.generate(&template, &context).await
        .context("Generation failed")
}
```

### 2. Async Patterns

Use Tokio for async operations:

```rust
use tokio::task;

// Parallel template processing
let tasks: Vec<_> = templates.iter()
    .map(|t| task::spawn(process_template(t.clone())))
    .collect();

let results = futures::future::join_all(tasks).await;
```

### 3. Resource Management

Properly manage resources:

```rust
use std::sync::Arc;

// Share expensive resources
let graph = Arc::new(Graph::new()?);
let graph_clone = Arc::clone(&graph);

tokio::spawn(async move {
    // Use graph_clone in spawned task
});
```

### 4. Configuration

Use environment variables and config files:

```rust
use std::env;

let api_key = env::var("OPENAI_API_KEY")
    .context("OPENAI_API_KEY not set")?;

let client = GenAiClient::new("gpt-4o", &api_key)?;
```

### 5. Testing

Write comprehensive tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::MockClient;

    #[tokio::test]
    async fn test_template_generation() {
        let mock = MockClient::with_response("mock template");
        let generator = TemplateGenerator::new(Box::new(mock));

        let result = generator.generate_template("test", vec![]).await;
        assert!(result.is_ok());
    }
}
```

## Building and Running

### Build All Examples

```bash
cargo build --examples
```

### Run Specific Example

```bash
cargo run --example basic-usage
```

### Build Wrappers

```bash
# Build REST API wrapper
cd wrappers/rest-api && cargo build --release

# Build CLI wrapper
cd wrappers/custom-cli && cargo build --release

# Build plugin system
cd wrappers/plugin-system && cargo build --release
```

## Environment Setup

Create a `.env` file in the project root:

```env
# LLM API Keys
OPENAI_API_KEY=your-openai-key
ANTHROPIC_API_KEY=your-anthropic-key

# Configuration
GGEN_CACHE_DIR=./cache
GGEN_OUTPUT_DIR=./output
RUST_LOG=info
```

## License

This example project is MIT licensed, same as ggen.

## Contributing

Contributions are welcome! Please see the main ggen repository for contribution guidelines.

## Additional Resources

- [ggen Documentation](https://github.com/seanchatmangpt/ggen)
- [ggen-core API Docs](../../ggen-core/README.md)
- [ggen-ai API Docs](../../ggen-ai/README.md)
- [Tera Template Documentation](https://keats.github.io/tera/)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
