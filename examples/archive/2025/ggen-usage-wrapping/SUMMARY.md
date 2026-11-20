# ggen Library Usage & Wrapping - Complete Summary

## Overview

This comprehensive example project demonstrates how to use **ggen-core** and **ggen-ai** as libraries in your Rust applications, with complete working examples, wrappers, and documentation.

## Project Structure

```
ggen-usage-wrapping/
├── Cargo.toml                    # Main project configuration
├── README.md                     # Main documentation
├── QUICKSTART.md                 # Quick start guide
├── SUMMARY.md                    # This file
├── run-examples.sh               # Script to run all examples
│
├── examples/                     # Core library usage examples
│   ├── basic-usage.rs           # Template loading and rendering
│   ├── with-ai.rs               # AI-powered generation
│   ├── custom-pipeline.rs       # Multi-stage pipelines
│   ├── batch-processor.rs       # Parallel processing
│   ├── graph-operations.rs      # RDF/SPARQL operations
│   └── template-validation.rs   # Template validation
│
├── wrappers/                     # Example wrapper implementations
│   ├── rest-api/                # REST API wrapper
│   │   ├── Cargo.toml
│   │   └── src/main.rs         # Actix-web server
│   │
│   └── custom-cli/              # CLI wrapper
│       ├── Cargo.toml
│       └── src/main.rs         # Enhanced CLI tool
│
├── templates/                    # Sample templates
│   ├── sample-api.md            # REST API endpoint template
│   └── simple-hello.md          # Simple greeting template
│
└── docs/                         # Documentation
    ├── USAGE_GUIDE.md           # Comprehensive usage guide
    └── API_REFERENCE.md         # Complete API reference
```

## What's Included

### 📚 Core Examples (6 Examples)

1. **basic-usage.rs** - Fundamental operations
   - Loading templates from files and strings
   - Creating generation context
   - Rendering templates with variables
   - Error handling patterns

2. **with-ai.rs** - AI-powered features
   - Configuring LLM clients (OpenAI, Anthropic, Ollama)
   - Generating templates from natural language
   - Template validation with AI
   - Using mock clients for testing

3. **custom-pipeline.rs** - Advanced pipelines
   - Building multi-stage pipelines
   - Chaining template transformations
   - Conditional pipeline execution
   - Error handling and recovery

4. **batch-processor.rs** - Parallel processing
   - Processing multiple templates concurrently
   - Rate limiting with semaphores
   - Progress tracking
   - Result aggregation

5. **graph-operations.rs** - RDF/SPARQL
   - Creating and manipulating RDF graphs
   - SPARQL query execution
   - Graph serialization (Turtle)
   - Ontology integration

6. **template-validation.rs** - Validation
   - Syntax validation
   - Metadata validation
   - Security checks
   - AI-powered validation

### 🔧 Wrapper Implementations (2 Wrappers)

1. **REST API Wrapper** (`wrappers/rest-api/`)
   - Actix-web based HTTP server
   - RESTful endpoints for generation
   - OpenAPI/Swagger documentation
   - CORS support
   - Rate limiting
   - Error handling

   **Endpoints:**
   - `GET /api/health` - Health check
   - `POST /api/generate` - Generate from template
   - `POST /api/templates` - Create template
   - `GET /api/templates/{id}` - Get template
   - `GET /api/templates` - List templates
   - `POST /api/ai/generate` - AI generation

2. **Custom CLI Wrapper** (`wrappers/custom-cli/`)
   - Enhanced command-line interface
   - Interactive mode with prompts
   - AI-powered generation
   - Template validation
   - Project initialization
   - Colored output and progress bars

   **Commands:**
   - `generate` - Generate from template
   - `interactive` - Interactive mode
   - `ai` - AI-powered generation
   - `validate` - Validate template
   - `list` - List templates
   - `init` - Initialize project

### 📖 Documentation (4 Documents)

1. **README.md** - Main documentation
   - Overview and table of contents
   - Core concepts explanation
   - API reference summaries
   - Usage examples
   - Best practices

2. **QUICKSTART.md** - Quick start guide
   - 5-minute getting started
   - First program tutorial
   - Common patterns
   - Troubleshooting

3. **USAGE_GUIDE.md** - Comprehensive usage
   - Detailed API usage
   - Advanced patterns
   - Resource management
   - Testing strategies

4. **API_REFERENCE.md** - Complete API docs
   - ggen-core API reference
   - ggen-ai API reference
   - Type definitions
   - Error handling

### 🎯 Sample Templates (2 Templates)

1. **sample-api.md** - REST API endpoint
   - TypeScript/Express example
   - CRUD operations
   - Input validation
   - Error handling

2. **simple-hello.md** - Simple greeting
   - Basic variable substitution
   - Conditional rendering
   - Loop examples

## Key Features Demonstrated

### ggen-core Features

✅ **Template System**
- Loading from files and strings
- Frontmatter metadata parsing
- Tera template engine integration
- Variable substitution
- Conditional rendering
- Loops and filters

✅ **Generator**
- Context-based generation
- Async/await patterns
- Error handling
- Batch processing

✅ **Pipeline System**
- Multi-stage pipelines
- Template chaining
- Output organization
- Error recovery

✅ **Graph Operations**
- RDF triple management
- SPARQL queries
- Graph serialization
- Ontology support

### ggen-ai Features

✅ **LLM Integration**
- Multi-provider support (OpenAI, Anthropic, Ollama)
- Configuration management
- API key handling
- Model selection

✅ **Template Generation**
- Natural language to templates
- Requirement-based generation
- Context-aware generation

✅ **Validation**
- Syntax validation
- Metadata validation
- Security checks
- AI-powered validation

✅ **Mock Client**
- Testing without API calls
- Deterministic responses
- Development support

## Quick Start Commands

### Run All Examples
```bash
./run-examples.sh
```

### Run Individual Examples
```bash
# Core examples
cargo run --example basic-usage
cargo run --example with-ai
cargo run --example custom-pipeline
cargo run --example batch-processor
cargo run --example graph-operations
cargo run --example template-validation
```

### Run Wrappers
```bash
# REST API
cd wrappers/rest-api && cargo run

# Custom CLI
cd wrappers/custom-cli && cargo run -- --help
cd wrappers/custom-cli && cargo run -- interactive
```

## Prerequisites

- **Rust**: 1.75 or later
- **Optional**: API keys for AI features
  - OpenAI: `export OPENAI_API_KEY="sk-..."`
  - Anthropic: `export ANTHROPIC_API_KEY="sk-ant-..."`
  - Ollama: Local installation

## Learning Path

### Beginner
1. Start with `QUICKSTART.md`
2. Run `cargo run --example basic-usage`
3. Explore `examples/basic-usage.rs`
4. Try modifying `templates/simple-hello.md`

### Intermediate
1. Read `docs/USAGE_GUIDE.md`
2. Run `cargo run --example custom-pipeline`
3. Run `cargo run --example batch-processor`
4. Explore the REST API wrapper

### Advanced
1. Study `docs/API_REFERENCE.md`
2. Run `cargo run --example graph-operations`
3. Explore AI features with `with-ai.rs`
4. Build custom wrappers using examples as templates

## Use Cases Covered

### 1. Simple Code Generation
- Load template
- Add context variables
- Generate output
- **Example**: `basic-usage.rs`

### 2. AI-Powered Generation
- Configure LLM client
- Generate from description
- Validate templates
- **Example**: `with-ai.rs`

### 3. Complex Workflows
- Multi-stage pipelines
- Conditional execution
- Error handling
- **Example**: `custom-pipeline.rs`

### 4. High-Performance Processing
- Parallel execution
- Rate limiting
- Progress tracking
- **Example**: `batch-processor.rs`

### 5. Semantic Operations
- RDF graph manipulation
- SPARQL queries
- Ontology integration
- **Example**: `graph-operations.rs`

### 6. Quality Assurance
- Template validation
- Security checks
- Best practices
- **Example**: `template-validation.rs`

### 7. HTTP API Exposure
- RESTful endpoints
- OpenAPI docs
- CORS support
- **Example**: `rest-api` wrapper

### 8. CLI Tools
- Interactive mode
- Command processing
- User experience
- **Example**: `custom-cli` wrapper

## Integration Patterns

### Pattern 1: Direct Library Usage
```rust
use ggen_core::{Template, Generator, GenContext};
// Direct API calls
```

### Pattern 2: Wrapper Service
```rust
// Expose via HTTP API
// See: wrappers/rest-api/
```

### Pattern 3: Enhanced CLI
```rust
// Build custom CLI tools
// See: wrappers/custom-cli/
```

### Pattern 4: Plugin System
```rust
// Extend functionality
// Custom processors, validators, etc.
```

## Testing

All examples include unit tests:

```bash
# Run all tests
cargo test

# Run specific example tests
cargo test --example basic-usage
```

Mock clients provided for testing without API keys:
```rust
use ggen_ai::MockClient;

let mock = MockClient::with_response("test output");
```

## Performance Optimizations

- **Parallel Processing**: Tokio-based concurrency
- **Resource Sharing**: Arc for shared state
- **Rate Limiting**: Semaphore-based throttling
- **Caching**: Template and result caching
- **Streaming**: Async streaming for large outputs

## Error Handling

Comprehensive error handling demonstrated:
- `anyhow::Result` for application errors
- `anyhow::Context` for error context
- Custom error types with `thiserror`
- Graceful degradation
- Recovery strategies

## Best Practices Shown

1. ✅ Async/await patterns
2. ✅ Resource management (Arc, Mutex)
3. ✅ Error handling and recovery
4. ✅ Testing with mocks
5. ✅ Configuration management
6. ✅ Logging and tracing
7. ✅ Security considerations
8. ✅ API design patterns

## File Checksums

Total files created: **18**

- **6** example programs (`.rs`)
- **2** wrapper implementations
- **4** documentation files (`.md`)
- **2** sample templates
- **3** configuration files (`.toml`)
- **1** automation script (`.sh`)

## Next Steps

1. **Explore Examples**: Run each example to understand concepts
2. **Read Documentation**: Start with QUICKSTART.md
3. **Try Wrappers**: Experiment with REST API and CLI
4. **Build Your Own**: Use as template for your projects
5. **Contribute**: Share your wrappers and examples

## Support Resources

- **Main Documentation**: `README.md`
- **Quick Start**: `QUICKSTART.md`
- **Usage Guide**: `docs/USAGE_GUIDE.md`
- **API Reference**: `docs/API_REFERENCE.md`
- **ggen Repository**: https://github.com/seanchatmangpt/ggen
- **Issues**: https://github.com/seanchatmangpt/ggen/issues

## License

This example project follows the same license as ggen (MIT).

---

**Happy coding with ggen! 🚀**

For questions or contributions, please visit the main ggen repository.
