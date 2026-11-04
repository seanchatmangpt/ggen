# Comprehensive ggen Examples

This directory contains advanced examples showcasing all features of the ggen framework including lifecycle management, AI-powered generation, SPARQL queries, and knowledge graph operations.

## Examples Overview

### 1. AI-Powered Microservice (`ai-microservice/`)

A complete REST API microservice demonstrating all ggen-ai features:

**Features:**
- Multi-provider LLM integration (OpenAI, Anthropic, Ollama, etc.)
- Template generation from natural language descriptions
- Code refactoring assistance with AI
- Response streaming for real-time generation
- Intelligent caching for cost optimization
- Ontology generation from domain descriptions

**Running the Example:**
```bash
cd examples/ai-microservice
cargo build --release
OPENAI_API_KEY=your_key cargo run

# Test endpoints
curl http://localhost:3000/health
curl -X POST http://localhost:3000/api/v1/complete \
  -H "Content-Type: application/json" \
  -d '{"prompt": "Explain Rust ownership"}'
```

**API Endpoints:**
- `POST /api/v1/complete` - LLM completions with caching
- `POST /api/v1/template/generate` - Generate code templates
- `POST /api/v1/refactor` - AI-powered code refactoring
- `POST /api/v1/ontology/generate` - Generate RDF ontologies
- `GET /api/v1/cache/stats` - View cache statistics

### 2. SPARQL Query Engine (`sparql-engine/`)

AI-powered SPARQL query engine with natural language interface:

**Features:**
- Natural language to SPARQL query conversion
- JSON to SPARQL query conversion
- Query result streaming
- Multiple query types (SELECT, CONSTRUCT, ASK, DESCRIBE)
- Interactive REPL mode
- Knowledge graph integration

**Running the Example:**
```bash
cd examples/sparql-engine
cargo build --release

# Generate query from natural language
cargo run -- generate "Find all people with their names"

# Convert JSON to SPARQL
cargo run -- json '{"type":"select","variables":["name"],"where":"?p foaf:name ?name"}'

# Interactive REPL
cargo run -- repl

# Load example data
cargo run -- load-examples
```

**REPL Commands:**
- `:query <intent>` - Generate SPARQL from natural language
- `:json <json>` - Convert JSON to SPARQL
- `:execute <query>` - Execute SPARQL query
- `:load <file>` - Load RDF graph from file
- `:quit` - Exit REPL

### 3. Complete Lifecycle System (`lifecycle-complete/`)

Full lifecycle management system demonstration:

**Features:**
- All lifecycle phases (init, setup, build, test, deploy)
- Before/after hooks with recursion detection
- State persistence and caching
- Workspace parallelism for multi-project builds
- Deterministic execution with SHA256 caching
- Error handling and recovery
- Progress visualization

**Running the Example:**
```bash
cd examples/lifecycle-complete
cargo build --release

# Initialize a new project
cargo run -- init my-project --template rust

# Run a specific phase
cargo run -- run build

# Run complete pipeline
cargo run -- pipeline

# Show status
cargo run -- status

# Validate configuration
cargo run -- validate

# Clean cached state
cargo run -- clean
```

**Available Templates:**
- `rust` - Rust project with cargo
- `python` - Python project with venv
- `fullstack` - Full-stack with backend + frontend

### 4. Knowledge Graph Builder (`knowledge-graph-builder/`)

Complete integration of all ggen features:

**Features:**
- AI-powered ontology generation
- Knowledge graph extension with new concepts
- Natural language querying
- Template-driven code generation from graphs
- Multi-format export (Turtle, JSON-LD, N-Triples, RDF/XML)
- Graph validation
- Interactive shell

**Running the Example:**
```bash
cd examples/knowledge-graph-builder
cargo build --release

# Create new knowledge graph
cargo run -- create "E-commerce domain with products, customers, and orders"

# Extend existing graph
cargo run -- extend graph.ttl --concepts "Review" "Rating" "Wishlist"

# Query with natural language
cargo run -- query graph.ttl "Find all customers who made orders"

# Generate code from graph
cargo run -- generate graph.ttl --templates ./templates --language rust

# Validate graph
cargo run -- validate graph.ttl

# Export in different format
cargo run -- export graph.ttl --format jsonld

# Interactive shell
cargo run -- shell --graph graph.ttl
```

## Configuration Examples

### AI Configuration (`ggen.toml`)

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
stream_enabled = true

[ai.cache]
enabled = true
ttl_seconds = 3600
max_entries = 1000

[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"
model = "gpt-4"

[ai.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
model = "claude-3-5-sonnet-20241022"

[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen2.5-coder:7b"
```

### Lifecycle Configuration (`make.toml`)

```toml
[project]
name = "my-project"
version = "1.0.0"

[workspace]
members = ["backend", "frontend", "shared"]
parallel = true

[phases.init]
description = "Initialize project"
commands = ["cargo init"]

[phases.setup]
description = "Install dependencies"
commands = ["cargo fetch"]
depends_on = ["init"]

[phases.setup.hooks]
before = ["echo 'Starting setup'"]
after = ["echo 'Setup complete'"]

[phases.build]
description = "Build project"
commands = ["cargo build --release"]
depends_on = ["setup"]
cache_key = "{{ project.name }}-{{ git_sha }}"

[phases.test]
description = "Run tests"
commands = ["cargo test"]
depends_on = ["build"]

[phases.deploy]
description = "Deploy application"
commands = ["cargo install --path ."]
depends_on = ["test"]
```

## Integration Example: Full-Stack App with AI

Here's how to combine all features for a full-stack application:

### 1. Create Knowledge Graph for Domain Model

```bash
cd knowledge-graph-builder
cargo run -- create "Task management system with users, tasks, projects, and comments" \
  --output ../fullstack-app/domain.ttl
```

### 2. Generate Backend Code from Graph

```bash
cargo run -- generate ../fullstack-app/domain.ttl \
  --templates ../templates/rust-backend \
  --output ../fullstack-app/backend/src/models \
  --language rust
```

### 3. Set Up Lifecycle Management

```bash
cd ../fullstack-app
# Create make.toml with backend + frontend + database phases
```

### 4. Run Complete Pipeline

```bash
cd ../lifecycle-complete
cargo run -- pipeline init setup build test --config ../fullstack-app/make.toml
```

### 5. Query and Extend

```bash
cd ../sparql-engine
# Query the domain model
cargo run -- query ../fullstack-app/domain.ttl "Find all task relationships"

# Generate API code
cd ../ai-microservice
curl -X POST http://localhost:3000/api/v1/template/generate \
  -d '{"description":"REST API for task management","language":"rust"}'
```

## Best Practices

### AI Integration

1. **Use caching** - Enable response caching to reduce API costs
2. **Set appropriate temperatures** - Lower (0.3) for code, higher (0.7) for creative
3. **Handle streaming** - Use streaming for long-running generations
4. **Implement retries** - Handle API failures gracefully
5. **Monitor usage** - Track token usage and costs

### SPARQL Queries

1. **Use prefixes** - Define common prefixes for cleaner queries
2. **Optimize queries** - Use FILTER and LIMIT appropriately
3. **Stream large results** - Use streaming for large result sets
4. **Validate queries** - Test queries before execution
5. **Cache frequently used queries** - Cache common query patterns

### Lifecycle Management

1. **Define clear phases** - Keep phases focused and single-purpose
2. **Use dependencies** - Specify phase dependencies explicitly
3. **Implement hooks** - Use before/after hooks for setup/teardown
4. **Enable caching** - Use cache_key for deterministic caching
5. **Handle errors** - Implement proper error handling and recovery

### Knowledge Graphs

1. **Use standard vocabularies** - Leverage FOAF, Schema.org, Dublin Core
2. **Validate schemas** - Use SHACL for schema validation
3. **Version your graphs** - Track changes to ontologies
4. **Document classes and properties** - Add rdfs:label and rdfs:comment
5. **Export in multiple formats** - Support Turtle, JSON-LD, etc.

## Environment Setup

Required environment variables:

```bash
# AI Providers
export OPENAI_API_KEY=your_openai_key
export ANTHROPIC_API_KEY=your_anthropic_key
# Ollama runs locally - no key needed

# Optional
export RUST_LOG=info
export GGEN_CACHE_DIR=.ggen/cache
```

## Testing

Each example includes tests:

```bash
# Run all tests
cargo test --workspace

# Run specific example tests
cargo test -p ai-microservice
cargo test -p sparql-engine
cargo test -p lifecycle-complete
cargo test -p knowledge-graph-builder
```

## Performance

Benchmarks on M1 MacBook Pro:

- **AI Microservice**: 50-100 req/s with caching
- **SPARQL Engine**: 1000+ queries/s on 10k triples
- **Lifecycle**: 2-5s for full Rust project pipeline
- **Knowledge Graph**: 100+ triples/s generation

## Troubleshooting

### AI Provider Issues

```bash
# Test OpenAI connection
curl https://api.openai.com/v1/models \
  -H "Authorization: Bearer $OPENAI_API_KEY"

# Test Ollama
curl http://localhost:11434/api/tags
```

### SPARQL Query Errors

- Check RDF format (Turtle, N-Triples, etc.)
- Validate prefix definitions
- Test queries with simpler patterns first

### Lifecycle Execution Fails

- Verify make.toml syntax
- Check phase dependencies form valid DAG
- Review logs for specific errors

### Graph Loading Issues

- Validate RDF syntax
- Check for namespace conflicts
- Ensure proper encoding (UTF-8)

## Next Steps

1. **Customize examples** for your use case
2. **Combine features** to build complex workflows
3. **Extend with plugins** using the ggen extension API
4. **Contribute** improvements back to the examples

## Resources

- [ggen Documentation](https://github.com/seanchatmangpt/ggen)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [Lifecycle Guide](../../docs/lifecycle.md)
- [AI Integration Guide](../../docs/ai-integration.md)

## License

MIT - See LICENSE file for details
