# ggen Library API Reference

Complete API reference for ggen-core and ggen-ai libraries.

## Table of Contents

- [ggen-core API](#ggen-core-api)
- [ggen-ai API](#ggen-ai-api)
- [Type Definitions](#type-definitions)
- [Error Handling](#error-handling)

## ggen-core API

### Template

Core template structure with metadata and content.

```rust
pub struct Template {
    pub metadata: TemplateMetadata,
    pub content: String,
}
```

#### Methods

##### `from_file`
Load template from a file.

```rust
pub fn from_file(path: impl AsRef<Path>) -> Result<Self>
```

**Example:**
```rust
let template = Template::from_file("path/to/template.md")?;
```

##### `from_str`
Parse template from a string.

```rust
pub fn from_str(content: &str) -> Result<Self>
```

**Example:**
```rust
let template = Template::from_str(r#"
---
name: my-template
---
Content here
"#)?;
```

### TemplateMetadata

Template metadata from frontmatter.

```rust
pub struct TemplateMetadata {
    pub name: String,
    pub description: Option<String>,
    pub version: Option<String>,
    pub author: Option<String>,
    pub license: Option<String>,
    pub tags: Option<Vec<String>>,
}
```

### Generator

Code generation engine.

```rust
pub struct Generator {
    // Internal implementation
}
```

#### Methods

##### `new`
Create a new generator instance.

```rust
pub fn new(
    templates: Vec<String>,
    context: HashMap<String, String>
) -> Result<Self>
```

**Example:**
```rust
let generator = Generator::new(vec![], HashMap::new())?;
```

##### `generate`
Generate code from template and context.

```rust
pub async fn generate(
    &self,
    template: &Template,
    context: &GenContext
) -> Result<String>
```

**Example:**
```rust
let output = generator.generate(&template, &context).await?;
```

### GenContext

Generation context for template variables.

```rust
pub struct GenContext {
    // Internal implementation
}
```

#### Methods

##### `new`
Create a new empty context.

```rust
pub fn new() -> Self
```

##### `insert`
Add a variable to the context.

```rust
pub fn insert(&mut self, key: &str, value: &str)
```

**Example:**
```rust
let mut context = GenContext::new();
context.insert("name", "value");
```

### Pipeline

Multi-stage generation pipeline.

```rust
pub struct Pipeline {
    // Internal implementation
}
```

#### Methods

##### `execute`
Execute the pipeline with given context.

```rust
pub async fn execute(&self, context: &GenContext) -> Result<()>
```

### PipelineBuilder

Builder for creating pipelines.

```rust
pub struct PipelineBuilder {
    // Internal implementation
}
```

#### Methods

##### `new`
Create a new pipeline builder.

```rust
pub fn new() -> Self
```

##### `with_template`
Add a template to the pipeline.

```rust
pub fn with_template(mut self, path: &str) -> Self
```

##### `with_output_dir`
Set the output directory.

```rust
pub fn with_output_dir(mut self, dir: &str) -> Self
```

##### `build`
Build the pipeline.

```rust
pub fn build(self) -> Result<Pipeline>
```

**Example:**
```rust
let pipeline = PipelineBuilder::new()
    .with_template("template1.md")
    .with_template("template2.md")
    .with_output_dir("./output")
    .build()?;
```

### Graph

RDF graph for semantic operations.

```rust
pub struct Graph {
    // Internal implementation
}
```

#### Methods

##### `new`
Create a new empty graph.

```rust
pub fn new() -> Result<Self>
```

##### `add_triple`
Add a triple to the graph.

```rust
pub fn add_triple(&mut self, subject: &str, predicate: &str, object: &str) -> Result<()>
```

**Example:**
```rust
graph.add_triple(
    "http://example.org/subject",
    "http://example.org/predicate",
    "http://example.org/object"
)?;
```

##### `query`
Execute a SPARQL query.

```rust
pub fn query(&self, sparql: &str) -> Result<Vec<QuerySolution>>
```

**Example:**
```rust
let results = graph.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")?;
```

##### `to_turtle`
Serialize graph to Turtle format.

```rust
pub fn to_turtle(&self) -> Result<String>
```

### CacheManager

Template and generation cache.

```rust
pub struct CacheManager {
    // Internal implementation
}
```

### RegistryClient

Package registry client.

```rust
pub struct RegistryClient {
    // Internal implementation
}
```

## ggen-ai API

### LlmClient

Trait for LLM client implementations.

```rust
#[async_trait]
pub trait LlmClient: Send + Sync {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    async fn complete_stream(&self, prompt: &str) -> Result<Pin<Box<dyn Stream<Item = Result<LlmChunk>>>>>;
}
```

### GenAiClient

Multi-provider LLM client.

```rust
pub struct GenAiClient {
    // Internal implementation
}
```

#### Methods

##### `new`
Create client with model and API key.

```rust
pub fn new(model: &str, api_key: &str) -> Result<Self>
```

##### `with_config`
Create client with full configuration.

```rust
pub fn with_config(config: LlmConfig) -> Result<Self>
```

**Example:**
```rust
let config = LlmConfig {
    provider: LlmProvider::OpenAI,
    model: "gpt-4o".to_string(),
    api_key: std::env::var("OPENAI_API_KEY")?,
    temperature: Some(0.7),
    ..Default::default()
};

let client = GenAiClient::with_config(config)?;
```

### LlmConfig

LLM client configuration.

```rust
pub struct LlmConfig {
    pub provider: LlmProvider,
    pub model: String,
    pub api_key: String,
    pub temperature: Option<f32>,
    pub max_tokens: Option<u32>,
    pub top_p: Option<f32>,
    pub frequency_penalty: Option<f32>,
    pub presence_penalty: Option<f32>,
}
```

### LlmProvider

Supported LLM providers.

```rust
pub enum LlmProvider {
    OpenAI,
    Anthropic,
    Ollama,
    Gemini,
    DeepSeek,
    XAI,
    Groq,
    Cohere,
}
```

### TemplateGenerator

AI-powered template generation.

```rust
pub struct TemplateGenerator {
    // Internal implementation
}
```

#### Methods

##### `new`
Create generator with LLM client.

```rust
pub fn new(client: Box<dyn LlmClient>) -> Self
```

##### `generate_template`
Generate template from description.

```rust
pub async fn generate_template(
    &self,
    description: &str,
    requirements: Vec<&str>
) -> Result<Template>
```

**Example:**
```rust
let generator = TemplateGenerator::new(Box::new(client));

let template = generator.generate_template(
    "Create a REST API endpoint",
    vec!["Include validation", "Use TypeScript"]
).await?;
```

### TemplateValidator

AI-powered template validation.

```rust
pub struct TemplateValidator {
    // Internal implementation
}
```

#### Methods

##### `new`
Create validator with LLM client.

```rust
pub fn new(client: Box<dyn LlmClient>) -> Self
```

##### `validate`
Validate a template.

```rust
pub async fn validate(
    &self,
    template: &Template
) -> Result<Vec<ValidationIssue>>
```

### ValidationIssue

Template validation issue.

```rust
pub struct ValidationIssue {
    pub severity: Severity,
    pub message: String,
    pub line: Option<usize>,
    pub suggestion: Option<String>,
}

pub enum Severity {
    Error,
    Warning,
    Info,
}
```

### MockClient

Mock LLM client for testing.

```rust
pub struct MockClient {
    // Internal implementation
}
```

#### Methods

##### `with_response`
Create mock client with fixed response.

```rust
pub fn with_response(response: &str) -> Self
```

**Example:**
```rust
let mock = MockClient::with_response("Mock response");
let generator = TemplateGenerator::new(Box::new(mock));
```

### Agent

Autonomous code generation agent.

```rust
pub struct Agent {
    pub id: String,
    pub name: String,
    pub capabilities: Vec<String>,
    // Internal fields
}
```

### AgentRegistry

Registry for managing agents.

```rust
pub struct AgentRegistry {
    // Internal implementation
}
```

#### Methods

##### `new`
Create new agent registry.

```rust
pub fn new() -> Self
```

##### `register`
Register an agent.

```rust
pub fn register(&mut self, agent: Agent) -> Result<()>
```

##### `get`
Get agent by name.

```rust
pub fn get(&self, name: &str) -> Option<&Agent>
```

## Type Definitions

### LlmResponse

Response from LLM completion.

```rust
pub struct LlmResponse {
    pub content: String,
    pub model: String,
    pub usage: Option<UsageStats>,
}
```

### UsageStats

Token usage statistics.

```rust
pub struct UsageStats {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}
```

### LlmChunk

Streaming response chunk.

```rust
pub struct LlmChunk {
    pub content: String,
    pub is_final: bool,
}
```

## Error Handling

### Core Errors

```rust
use anyhow::{Context, Result};

// Using anyhow for error handling
let template = Template::from_file("template.md")
    .context("Failed to load template")?;

// Custom error types
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("Template error: {0}")]
    Template(String),

    #[error("Generation error: {0}")]
    Generation(String),
}
```

### AI Errors

```rust
use ggen_ai::{GgenAiError, Result};

// ggen-ai specific errors
match generator.generate_template(desc, reqs).await {
    Ok(template) => println!("Success: {}", template.metadata.name),
    Err(GgenAiError::ApiError(e)) => eprintln!("API error: {}", e),
    Err(GgenAiError::ValidationError(e)) => eprintln!("Validation error: {}", e),
    Err(e) => eprintln!("Other error: {}", e),
}
```

## Usage Patterns

### Pattern 1: Simple Generation

```rust
use ggen_core::{Template, Generator, GenContext};

#[tokio::main]
async fn main() -> Result<()> {
    let template = Template::from_file("template.md")?;
    let mut context = GenContext::new();
    context.insert("var", "value");

    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    println!("{}", output);
    Ok(())
}
```

### Pattern 2: AI Generation

```rust
use ggen_ai::{GenAiClient, LlmConfig, LlmProvider, TemplateGenerator};

#[tokio::main]
async fn main() -> Result<()> {
    let config = LlmConfig {
        provider: LlmProvider::OpenAI,
        model: "gpt-4o".to_string(),
        api_key: std::env::var("OPENAI_API_KEY")?,
        ..Default::default()
    };

    let client = GenAiClient::with_config(config)?;
    let generator = TemplateGenerator::new(Box::new(client));

    let template = generator.generate_template(
        "Create a CLI tool",
        vec!["Use clap", "Include subcommands"]
    ).await?;

    println!("{}", template.content);
    Ok(())
}
```

### Pattern 3: Pipeline

```rust
use ggen_core::PipelineBuilder;

#[tokio::main]
async fn main() -> Result<()> {
    let pipeline = PipelineBuilder::new()
        .with_template("core.md")
        .with_template("api.md")
        .with_template("tests.md")
        .with_output_dir("./output")
        .build()?;

    let context = GenContext::new();
    pipeline.execute(&context).await?;

    Ok(())
}
```

### Pattern 4: Graph Operations

```rust
use ggen_core::Graph;

#[tokio::main]
async fn main() -> Result<()> {
    let mut graph = Graph::new()?;

    graph.add_triple(
        "http://example.org/alice",
        "http://example.org/knows",
        "http://example.org/bob"
    )?;

    let results = graph.query(
        "SELECT ?s ?o WHERE { ?s <http://example.org/knows> ?o }"
    )?;

    for result in results {
        println!("{:?}", result);
    }

    Ok(())
}
```

## See Also

- [Usage Guide](USAGE_GUIDE.md)
- [Quick Start](../QUICKSTART.md)
- [Examples](../examples/)
- [Main README](../README.md)
