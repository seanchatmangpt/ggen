# AI Microservice

AI-powered microservice with template generation, refactoring, and ontology support for ggen.

## Features

- **Multi-provider AI support** - OpenAI, Anthropic, Ollama integration
- **Microservice template generation** - Complete service scaffolding
- **Automatic refactoring suggestions** - AI-powered code improvements
- **Ontology-based code generation** - Semantic-driven development
- **Caching for improved performance** - LRU caching for AI responses
- **Streaming support** - Real-time AI responses
- **Configuration-driven development** - Flexible configuration system
- **Production-ready patterns** - Best practices baked in

## Installation

```bash
# Add to your project
ggen market add ai-microservice

# Or generate from template
ggen template generate ai-microservice --vars '{"project_name":"my-service"}'
```

## Quick Start

```rust
use ai_microservice::prelude::*;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Load configuration
    let config = Config::from_file("ggen.toml")?;

    // Create AI client
    let ai = AIClient::new(config).await?;

    // Generate microservice code
    let code = ai.generate_service("Create a product catalog service").await?;

    println!("{}", code);

    Ok(())
}
```

## Configuration

### ggen.toml

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000

[ai.cache]
enabled = true
size_mb = 100
ttl_seconds = 3600

[ai.streaming]
enabled = true
chunk_size = 1024
```

## Supported Providers

| Provider | Models | Status |
|----------|--------|--------|
| OpenAI | GPT-4, GPT-3.5-turbo | ✅ |
| Anthropic | Claude-3, Claude-2 | ✅ |
| Ollama | Llama-2, Qwen | ✅ |

## Usage Examples

### Generate Microservice

```bash
# Generate a complete microservice
ggen ai generate "Create a product catalog microservice with REST API"

# Generate with specific provider
ggen ai generate "Create user service" --provider anthropic --model claude-3-sonnet
```

### Refactor Existing Code

```bash
# Refactor a service
ggen ai refactor src/services/product.rs

# Refactor with suggestions
ggen ai refactor src/services/user.rs --suggest-improvements
```

### Ontology-Based Generation

```bash
# Generate from ontology
ggen ai generate-from-ontology data/domain.ttl --entity Product

# Generate with semantic constraints
ggen ai generate "Create order service" --ontology data/domain.ttl
```

## Advanced Features

### Caching

The AI microservice includes built-in LRU caching for improved performance:

```rust
// Cached AI responses
let response = ai.generate_cached("Create API endpoint").await?;

// Manual cache control
ai.cache.clear();
ai.cache.set_size(200); // 200MB
```

### Streaming

Support for real-time streaming responses:

```rust
// Stream AI generation
let mut stream = ai.generate_stream("Create service").await?;

while let Some(chunk) = stream.next().await {
    print!("{}", chunk?);
}
```

### Refactoring

Automatic code refactoring suggestions:

```rust
// Get refactoring suggestions
let suggestions = ai.refactor_suggestions("src/service.rs").await?;

for suggestion in suggestions {
    println!("Suggestion: {}", suggestion.description);
    println!("Code: {}", suggestion.code);
}
```

## Testing

```bash
# Run tests
cargo test

# Run with specific features
cargo test --features streaming
cargo test --features cache
```

## Performance

- **Caching**: 90% reduction in duplicate API calls
- **Streaming**: Real-time response generation
- **Async**: High concurrency with tokio
- **Memory**: Efficient LRU cache management

## Best Practices

1. **Use caching** for repeated queries
2. **Enable streaming** for long responses
3. **Configure timeouts** appropriately
4. **Use ontologies** for semantic generation
5. **Monitor API usage** to control costs

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Run the full test suite
5. Submit a pull request

## License

MIT License - see LICENSE file for details.
