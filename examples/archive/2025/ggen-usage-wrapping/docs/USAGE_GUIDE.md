# ggen Library Usage Guide

Complete guide to using ggen-core and ggen-ai as libraries in your Rust projects.

## Table of Contents

- [Quick Start](#quick-start)
- [Core Library Usage](#core-library-usage)
- [AI Features](#ai-features)
- [Advanced Patterns](#advanced-patterns)
- [Wrapper Examples](#wrapper-examples)
- [Best Practices](#best-practices)

## Quick Start

### Adding Dependencies

```toml
[dependencies]
ggen-core = { path = "path/to/ggen-core", version = "1.0.0" }
ggen-ai = { path = "path/to/ggen-ai", version = "1.0.0" }
tokio = { version = "1.0", features = ["full"] }
anyhow = "1.0"
```

### Basic Usage

```rust
use ggen_core::{Template, Generator, GenContext};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Load template
    let template = Template::from_file("template.md")?;

    // Create context
    let mut context = GenContext::new();
    context.insert("name", "MyProject");

    // Generate
    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    println!("{}", output);
    Ok(())
}
```

## Core Library Usage

### 1. Template System

#### Loading Templates

```rust
use ggen_core::Template;

// From file
let template = Template::from_file("path/to/template.md")?;

// From string
let template = Template::from_str(r#"---
name: my-template
description: Example template
---
Hello {{ name }}!
"#)?;

// Access metadata
println!("Name: {}", template.metadata.name);
println!("Description: {:?}", template.metadata.description);
```

#### Template Structure

Templates use frontmatter for metadata:

```markdown
---
name: template-name
description: Template description
version: 1.0.0
author: Your Name
tags: [tag1, tag2]
---

# Template Content

{{ variable_name }}

{% if condition %}
  Conditional content
{% endif %}

{% for item in items %}
  - {{ item }}
{% endfor %}
```

### 2. Generator

#### Basic Generation

```rust
use ggen_core::{Generator, GenContext};
use std::collections::HashMap;

let generator = Generator::new(vec![], HashMap::new())?;

let mut context = GenContext::new();
context.insert("project_name", "awesome-app");
context.insert("version", "1.0.0");

let output = generator.generate(&template, &context).await?;
```

#### Batch Generation

```rust
use tokio::task;

let tasks: Vec<_> = templates
    .into_iter()
    .map(|template| {
        let ctx = context.clone();
        task::spawn(async move {
            generator.generate(&template, &ctx).await
        })
    })
    .collect();

let results = futures::future::join_all(tasks).await;
```

### 3. Pipeline System

#### Building Pipelines

```rust
use ggen_core::{Pipeline, PipelineBuilder};

let pipeline = PipelineBuilder::new()
    .with_template("template1.md")
    .with_template("template2.md")
    .with_output_dir("./output")
    .build()?;

pipeline.execute(&context).await?;
```

#### Custom Pipeline Stages

```rust
// Create multi-stage pipeline
let pipeline = PipelineBuilder::new()
    // Stage 1: Core files
    .with_template("core.md")
    // Stage 2: API layer
    .with_template("api.md")
    // Stage 3: Tests
    .with_template("tests.md")
    .with_output_dir("./project")
    .build()?;

// Execute with context
let mut context = GenContext::new();
context.insert("module", "user-service");

pipeline.execute(&context).await?;
```

### 4. Graph Operations

#### Creating Graphs

```rust
use ggen_core::Graph;

let mut graph = Graph::new()?;

// Add triples
graph.add_triple(
    "http://example.org/Person",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    "http://www.w3.org/2002/07/owl#Class"
)?;

graph.add_triple(
    "http://example.org/alice",
    "http://example.org/name",
    "\"Alice Smith\""
)?;
```

#### SPARQL Queries

```rust
let query = r#"
    PREFIX ex: <http://example.org/>
    SELECT ?person ?name WHERE {
        ?person ex:name ?name .
    }
"#;

let results = graph.query(query)?;

for solution in results {
    println!("{:?}", solution);
}
```

#### Graph Serialization

```rust
// Serialize to Turtle
let turtle = graph.to_turtle()?;
println!("{}", turtle);

// In future versions: JSON-LD, RDF/XML, N-Triples
```

## AI Features

### 1. LLM Client Configuration

#### Provider Setup

```rust
use ggen_ai::{GenAiClient, LlmConfig, LlmProvider};

// OpenAI
let config = LlmConfig {
    provider: LlmProvider::OpenAI,
    model: "gpt-4o".to_string(),
    api_key: std::env::var("OPENAI_API_KEY")?,
    temperature: Some(0.7),
    max_tokens: Some(2000),
    ..Default::default()
};

// Anthropic
let config = LlmConfig {
    provider: LlmProvider::Anthropic,
    model: "claude-3-5-sonnet-20241022".to_string(),
    api_key: std::env::var("ANTHROPIC_API_KEY")?,
    ..Default::default()
};

// Ollama (local)
let config = LlmConfig {
    provider: LlmProvider::Ollama,
    model: "qwen2.5-coder:7b".to_string(),
    api_key: String::new(), // Not needed for Ollama
    ..Default::default()
};

let client = GenAiClient::with_config(config)?;
```

### 2. Template Generation

```rust
use ggen_ai::TemplateGenerator;

let generator = TemplateGenerator::new(Box::new(client));

let template = generator.generate_template(
    "Create a REST API controller for user management",
    vec![
        "Include CRUD operations",
        "Use TypeScript",
        "Add input validation"
    ]
).await?;

println!("Generated template: {}", template.content);
```

### 3. Template Validation

```rust
use ggen_ai::TemplateValidator;

let validator = TemplateValidator::new(Box::new(client));

let issues = validator.validate(&template).await?;

for issue in issues {
    println!("Severity: {:?}", issue.severity);
    println!("Message: {}", issue.message);
}
```

### 4. Mock Client (Testing)

```rust
use ggen_ai::MockClient;

let mock = MockClient::with_response("Mock generated content");
let generator = TemplateGenerator::new(Box::new(mock));

let template = generator.generate_template("test", vec![]).await?;
// Returns mock response without API calls
```

### 5. Streaming Responses

```rust
use futures::StreamExt;

let mut stream = client.complete_stream("Generate code...").await?;

while let Some(chunk) = stream.next().await {
    match chunk {
        Ok(chunk) => print!("{}", chunk.content),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

## Advanced Patterns

### 1. Resource Sharing

```rust
use std::sync::Arc;
use tokio::task;

// Share expensive resources
let generator = Arc::new(Generator::new(vec![], HashMap::new())?);
let graph = Arc::new(Graph::new()?);

let gen_clone = Arc::clone(&generator);
let graph_clone = Arc::clone(&graph);

task::spawn(async move {
    // Use shared resources
    let result = gen_clone.generate(&template, &context).await;
});
```

### 2. Concurrent Processing

```rust
use tokio::sync::Semaphore;

// Limit concurrency
let semaphore = Arc::new(Semaphore::new(5));

let tasks: Vec<_> = templates
    .into_iter()
    .map(|template| {
        let sem = Arc::clone(&semaphore);
        task::spawn(async move {
            let _permit = sem.acquire().await.unwrap();
            // Process template
        })
    })
    .collect();
```

### 3. Error Handling

```rust
use anyhow::{Context, Result};

async fn safe_generate(template_path: &str) -> Result<String> {
    let template = Template::from_file(template_path)
        .context("Failed to load template")?;

    let generator = Generator::new(vec![], HashMap::new())
        .context("Failed to create generator")?;

    let context = GenContext::new();

    generator.generate(&template, &context).await
        .context("Generation failed")
}
```

### 4. Custom Extensions

```rust
// Custom template processor
struct CustomProcessor {
    generator: Generator,
}

impl CustomProcessor {
    pub fn new() -> Result<Self> {
        Ok(Self {
            generator: Generator::new(vec![], HashMap::new())?,
        })
    }

    pub async fn process_with_validation(
        &self,
        template: &Template,
        context: &GenContext,
    ) -> Result<String> {
        // Pre-processing
        self.validate_context(context)?;

        // Generate
        let output = self.generator.generate(template, context).await?;

        // Post-processing
        self.format_output(&output)
    }

    fn validate_context(&self, context: &GenContext) -> Result<()> {
        // Custom validation logic
        Ok(())
    }

    fn format_output(&self, output: &str) -> Result<String> {
        // Custom formatting
        Ok(output.to_string())
    }
}
```

## Wrapper Examples

### 1. REST API Wrapper

See `wrappers/rest-api/` for a complete Actix-web implementation:

```bash
cd wrappers/rest-api
cargo run
```

Endpoints:
- `POST /api/generate` - Generate from template
- `POST /api/templates` - Create template
- `GET /api/templates/{id}` - Get template
- `POST /api/ai/generate` - AI generation

### 2. Custom CLI Wrapper

See `wrappers/custom-cli/` for an enhanced CLI:

```bash
cd wrappers/custom-cli
cargo run -- generate --template template.md --output ./out
```

Features:
- Interactive mode
- AI-powered generation
- Template validation
- Project initialization

### 3. Plugin System

See `wrappers/plugin-system/` for extensible architecture:

```bash
cd wrappers/plugin-system
cargo run
```

## Best Practices

### 1. Configuration Management

```rust
use std::env;
use anyhow::Result;

struct AppConfig {
    openai_key: String,
    cache_dir: PathBuf,
}

impl AppConfig {
    fn from_env() -> Result<Self> {
        Ok(Self {
            openai_key: env::var("OPENAI_API_KEY")
                .context("OPENAI_API_KEY not set")?,
            cache_dir: PathBuf::from(
                env::var("CACHE_DIR").unwrap_or_else(|_| "./cache".to_string())
            ),
        })
    }
}
```

### 2. Async Patterns

```rust
// Use tokio for async operations
#[tokio::main]
async fn main() -> Result<()> {
    // Parallel execution
    let (result1, result2) = tokio::join!(
        task1(),
        task2()
    );

    Ok(())
}
```

### 3. Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::MockClient;

    #[tokio::test]
    async fn test_generation() {
        let mock = MockClient::with_response("test output");
        let gen = TemplateGenerator::new(Box::new(mock));

        let result = gen.generate_template("test", vec![]).await;
        assert!(result.is_ok());
    }
}
```

### 4. Performance Optimization

```rust
// Use caching
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

struct TemplateCache {
    cache: Arc<RwLock<HashMap<String, Template>>>,
}

impl TemplateCache {
    async fn get_or_load(&self, path: &str) -> Result<Template> {
        // Check cache
        {
            let cache = self.cache.read().await;
            if let Some(template) = cache.get(path) {
                return Ok(template.clone());
            }
        }

        // Load and cache
        let template = Template::from_file(path)?;
        let mut cache = self.cache.write().await;
        cache.insert(path.to_string(), template.clone());

        Ok(template)
    }
}
```

## Running Examples

### Build All Examples

```bash
cargo build --examples
```

### Run Specific Example

```bash
# Basic usage
cargo run --example basic-usage

# AI features
export OPENAI_API_KEY="your-key"
cargo run --example with-ai

# Custom pipeline
cargo run --example custom-pipeline

# Batch processing
cargo run --example batch-processor

# Graph operations
cargo run --example graph-operations

# Template validation
cargo run --example template-validation
```

### Build Wrappers

```bash
# REST API
cd wrappers/rest-api
cargo build --release

# CLI
cd wrappers/custom-cli
cargo build --release

# Plugin system
cd wrappers/plugin-system
cargo build --release
```

## Troubleshooting

### Common Issues

1. **API Key Not Found**
   ```bash
   export OPENAI_API_KEY="sk-..."
   # or
   export ANTHROPIC_API_KEY="sk-ant-..."
   ```

2. **Template Parse Error**
   - Ensure frontmatter is properly formatted
   - Check YAML syntax
   - Verify closing `---`

3. **Generation Failures**
   - Check context variables are provided
   - Validate template syntax
   - Review error messages

## Additional Resources

- [ggen-core Documentation](../../ggen-core/README.md)
- [ggen-ai Documentation](../../ggen-ai/README.md)
- [Tera Template Syntax](https://keats.github.io/tera/)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
