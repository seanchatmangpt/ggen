# Quick Start Guide - Using ggen as a Library

Get started with ggen-core and ggen-ai in under 5 minutes!

## Prerequisites

- Rust 1.75 or later
- (Optional) OpenAI or Anthropic API key for AI features

## Installation

### 1. Add to Your Project

```toml
# Cargo.toml
[dependencies]
ggen-core = { path = "path/to/ggen-core" }
ggen-ai = { path = "path/to/ggen-ai" }  # Optional, for AI features
tokio = { version = "1.0", features = ["full"] }
anyhow = "1.0"
```

### 2. Clone This Example

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen/examples/ggen-usage-wrapping
```

## Your First ggen Program

### Step 1: Create a Template

Create `my-template.md`:

```markdown
---
name: hello-world
description: A simple greeting template
version: 1.0.0
---
# Hello {{ name }}!

Welcome to {{ project }}. This is version {{ version }}.

{% if add_footer %}
---
Generated with ggen
{% endif %}
```

### Step 2: Write Code

Create `src/main.rs`:

```rust
use ggen_core::{Template, Generator, GenContext};
use std::collections::HashMap;
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Load template
    let template = Template::from_file("my-template.md")?;

    // Create context
    let mut context = GenContext::new();
    context.insert("name", "World");
    context.insert("project", "ggen");
    context.insert("version", "1.0.0");
    context.insert("add_footer", "true");

    // Generate
    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    println!("{}", output);
    Ok(())
}
```

### Step 3: Run

```bash
cargo run
```

**Output:**
```
# Hello World!

Welcome to ggen. This is version 1.0.0.

---
Generated with ggen
```

## AI-Powered Generation (Optional)

### Setup

```bash
export OPENAI_API_KEY="your-api-key"
# or
export ANTHROPIC_API_KEY="your-api-key"
```

### Code

```rust
use ggen_ai::{GenAiClient, LlmConfig, LlmProvider, TemplateGenerator};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Configure LLM
    let config = LlmConfig {
        provider: LlmProvider::OpenAI,
        model: "gpt-4o".to_string(),
        api_key: std::env::var("OPENAI_API_KEY")?,
        ..Default::default()
    };

    let client = GenAiClient::with_config(config)?;
    let generator = TemplateGenerator::new(Box::new(client));

    // Generate template from description
    let template = generator.generate_template(
        "Create a REST API endpoint for user registration",
        vec!["Include input validation", "Use TypeScript"]
    ).await?;

    println!("Generated template:\n{}", template.content);
    Ok(())
}
```

## Running the Examples

### Core Examples

```bash
# Basic usage
cargo run --example basic-usage

# Custom pipeline
cargo run --example custom-pipeline

# Batch processing
cargo run --example batch-processor

# Graph operations
cargo run --example graph-operations

# Template validation
cargo run --example template-validation
```

### AI Examples

```bash
export OPENAI_API_KEY="your-key"
cargo run --example with-ai
```

### All Examples at Once

```bash
./run-examples.sh
```

## Using the Wrappers

### 1. REST API Wrapper

Start the API server:

```bash
cd wrappers/rest-api
cargo run
```

Visit http://localhost:8080/swagger-ui/ for interactive API docs.

**Generate code via API:**

```bash
curl -X POST http://localhost:8080/api/generate \
  -H "Content-Type: application/json" \
  -d '{
    "template": "---\nname: test\n---\nHello {{ name }}!",
    "context": {"name": "API"},
    "is_template_id": false
  }'
```

### 2. Custom CLI Wrapper

Interactive mode:

```bash
cd wrappers/custom-cli
cargo run -- interactive
```

Command mode:

```bash
cargo run -- generate \
  --template ../../templates/sample.md \
  --output ./output \
  --var name=MyProject \
  --var version=1.0.0
```

AI generation:

```bash
export OPENAI_API_KEY="your-key"
cargo run -- ai \
  --description "Create a React component" \
  --requirements "Use TypeScript" \
  --requirements "Include props interface" \
  --output component.md
```

## Common Patterns

### 1. Load and Render Template

```rust
let template = Template::from_file("template.md")?;
let mut context = GenContext::new();
context.insert("var", "value");

let generator = Generator::new(vec![], HashMap::new())?;
let output = generator.generate(&template, &context).await?;
```

### 2. Parallel Processing

```rust
use tokio::task;
use futures::future::join_all;

let tasks: Vec<_> = templates.into_iter()
    .map(|t| task::spawn(async move {
        process_template(t).await
    }))
    .collect();

let results = join_all(tasks).await;
```

### 3. Pipeline Execution

```rust
use ggen_core::PipelineBuilder;

let pipeline = PipelineBuilder::new()
    .with_template("step1.md")
    .with_template("step2.md")
    .with_output_dir("./output")
    .build()?;

pipeline.execute(&context).await?;
```

### 4. Graph Queries

```rust
use ggen_core::Graph;

let mut graph = Graph::new()?;
graph.add_triple(subject, predicate, object)?;

let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
let results = graph.query(query)?;
```

### 5. AI Template Generation

```rust
use ggen_ai::{GenAiClient, TemplateGenerator};

let client = GenAiClient::new("gpt-4o", api_key)?;
let generator = TemplateGenerator::new(Box::new(client));

let template = generator.generate_template(
    "description",
    vec!["requirement1", "requirement2"]
).await?;
```

## Testing Your Integration

### Unit Tests

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

### Integration Tests

```rust
#[tokio::test]
async fn test_full_pipeline() {
    let template = Template::from_str(TEST_TEMPLATE)?;
    let context = GenContext::new();
    let generator = Generator::new(vec![], HashMap::new())?;

    let output = generator.generate(&template, &context).await?;
    assert!(output.contains("expected content"));
}
```

## Next Steps

1. **Explore Examples**: Check the `examples/` directory for more patterns
2. **Read API Docs**: See `docs/USAGE_GUIDE.md` for detailed API reference
3. **Build Wrappers**: Customize the wrappers in `wrappers/` for your needs
4. **Check Main README**: See `README.md` for complete API reference

## Troubleshooting

### Template Parse Error

**Error**: `Failed to parse template frontmatter`

**Solution**: Ensure your frontmatter is valid YAML:
```markdown
---
name: template-name
description: "Template description"
---
```

### API Key Not Found

**Error**: `No API key found`

**Solution**:
```bash
export OPENAI_API_KEY="sk-..."
# or
export ANTHROPIC_API_KEY="sk-ant-..."
```

### Generation Fails

**Error**: `Generation failed: variable not found`

**Solution**: Ensure all template variables are in context:
```rust
context.insert("required_var", "value");
```

## Resources

- [Full Usage Guide](docs/USAGE_GUIDE.md)
- [API Reference](README.md)
- [ggen-core Documentation](../../ggen-core/README.md)
- [ggen-ai Documentation](../../ggen-ai/README.md)

## Support

- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Documentation: https://github.com/seanchatmangpt/ggen

Happy coding with ggen! 🚀
