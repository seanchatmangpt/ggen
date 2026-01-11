# Advanced Rust Project Usage Guide

This comprehensive example demonstrates all advanced features of ggen including lifecycle management, AI-powered generation, SPARQL/RDF integration, and template processing.

## Quick Start

### 1. Initialize Project
```bash
cd examples/advanced-rust-project
ggen lifecycle list
```

### 2. Generate Code
```bash
# Generate complete Rust service
ggen template generate templates/rust-service.tmpl \
    --var name="UserService" \
    --var description="User management service" \
    --var version="1.0.0" \
    --var author="ggen-examples"

# Generate API endpoint
ggen template generate templates/api-endpoint.tmpl \
    --var name="UserAPI" \
    --var method="GET" \
    --var path="/api/v1/users" \
    --var description="User API endpoint"

# Generate database schema
ggen template generate templates/database-schema.tmpl \
    --var name="UserSchema" \
    --var database="postgresql" \
    --var orm="sqlx"

# Generate documentation
ggen template generate templates/documentation.tmpl \
    --var name="UserDocs" \
    --var format="markdown" \
    --var style="github"
```

### 3. Run Lifecycle Phases
```bash
# Run single phase
ggen lifecycle run generate

# Run pipeline
ggen lifecycle pipeline setup generate build test

# Run with environment
ggen lifecycle run deploy --env production
```

### 4. AI-Powered Generation
```bash
# Generate templates using AI
ggen ai generate --description "Rust microservice with REST API"

# Generate SPARQL queries
ggen ai sparql --description "Find all API endpoints"

# Generate RDF graphs
ggen ai graph --description "E-commerce domain model"

# Generate frontmatter
ggen ai frontmatter --description "API documentation"
```

## Advanced Features

### Lifecycle Management

The `make.toml` file defines a complete workflow with phases, hooks, and environments:

```toml
[lifecycle.init]
description = "Initialize project structure"
commands = ["mkdir -p generated/{src,api,database,docs}"]

[lifecycle.generate]
description = "Generate code using AI and templates"
commands = ["ggen template generate templates/rust-service.tmpl"]
depends_on = ["setup"]

[lifecycle.build]
description = "Build the Rust project"
commands = ["cd generated && cargo build --release"]
depends_on = ["generate"]
```

**Key Features:**
- **Phases**: init, setup, generate, build, test, deploy
- **Dependencies**: Phase execution order
- **Hooks**: Pre/post execution commands
- **Environments**: Development, staging, production
- **Parallel Execution**: Concurrent phase execution
- **State Management**: Track execution progress

### AI-Powered Generation

Generate code using natural language descriptions:

```bash
# Generate complete service
ggen ai generate --description "Rust microservice with REST API, database integration, and authentication"

# Generate SPARQL queries
ggen ai sparql --description "Find all users with orders greater than $100"

# Generate RDF graphs
ggen ai graph --description "E-commerce domain model with products, users, and orders"

# Generate frontmatter
ggen ai frontmatter --description "Blog post about Rust async programming"
```

**Key Features:**
- **Natural Language**: Describe what you want in plain English
- **Multiple Providers**: OpenAI, Anthropic, Ollama, etc.
- **Mock Mode**: Test without API calls
- **Custom Models**: Override default models
- **Temperature Control**: Adjust creativity level

### SPARQL/RDF Integration

Query knowledge graphs and use results in templates:

```sparql
# Find all entities
SELECT ?entity WHERE { ?entity a ex:Entity }

# Find all API endpoints
SELECT ?endpoint WHERE { ?endpoint a ex:APIEndpoint }

# Find all database tables
SELECT ?table WHERE { ?table a ex:Table }
```

**Key Features:**
- **RDF Loading**: Load Turtle, RDF/XML, JSON-LD
- **SPARQL Queries**: Complex knowledge graph queries
- **Template Integration**: Use query results in templates
- **Caching**: Query result caching for performance
- **Prefix Support**: Automatic prefix resolution

### Template System

Advanced template features with comprehensive filters:

```tera
<!-- Case conversion filters -->
{{ name | camel }}      <!-- helloWorld -->
{{ name | pascal }}     <!-- HelloWorld -->
{{ name | snake }}      <!-- hello_world -->
{{ name | kebab }}      <!-- hello-world -->
{{ name | title }}      <!-- Hello World -->

<!-- String manipulation -->
{{ name | pluralize }}  <!-- users -->
{{ name | singularize }} <!-- user -->
{{ name | upper }}      <!-- HELLO -->
{{ name | lower }}      <!-- hello -->

<!-- SPARQL helpers -->
{{ sparql_count(results=sparql_results.entities) }}
{{ sparql_first(results=sparql_results.entities, column="name") }}
{{ sparql_values(results=sparql_results.entities, column="name") }}
```

**Key Features:**
- **20+ Filters**: Case conversion, string manipulation
- **SPARQL Helpers**: Query result processing
- **File Injection**: Modify existing files
- **Shell Hooks**: Execute commands
- **Variable Processing**: Flexible YAML variables

### Security Features

Comprehensive security measures:

```rust
// Path traversal protection
if !canonical_path.starts_with(&canonical_template_dir) {
    return Err(Error::new("Path traversal detected"));
}

// Shell injection prevention
if self.is_dangerous_command(command) {
    return Err(Error::new("Dangerous command blocked"));
}

// Input validation
if request.name.is_empty() {
    return Err(Error::Validation("Name is required"));
}
```

**Key Features:**
- **Path Traversal Protection**: Secure file operations
- **Shell Injection Prevention**: Command validation
- **Input Sanitization**: Safe template processing
- **Error Handling**: Comprehensive error management
- **Audit Logging**: Security event tracking

## Configuration

### ggen.toml
Project-specific configuration:

```toml
[project]
name = "advanced-rust-project"
version = "1.0.0"
description = "Advanced Rust project demonstrating all ggen features"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000

[rdf]
base_iri = "http://example.org/advanced-rust-project/"
default_format = "turtle"
cache_queries = true

[security]
validate_paths = true
block_shell_injection = true
require_confirmation = false
audit_operations = true
```

### make.toml
Lifecycle configuration:

```toml
[lifecycle.init]
description = "Initialize project structure"
commands = ["mkdir -p generated/{src,api,database,docs}"]

[lifecycle.generate]
description = "Generate code using AI and templates"
commands = ["ggen template generate templates/rust-service.tmpl"]
depends_on = ["setup"]

[hooks.before_generate]
commands = ["echo 'Starting code generation...'"]

[hooks.after_generate]
commands = ["echo 'Code generation completed'"]
```

## Scripts

### Build Script
```bash
# Full build process
./scripts/build/build.sh

# Clean only
./scripts/build/build.sh --clean

# Generate only
./scripts/build/build.sh --generate-only

# Build only
./scripts/build/build.sh --build-only
```

### Deploy Script
```bash
# Deploy to production
./scripts/deploy/deploy.sh

# Deploy to staging
GGEN_ENV=staging ./scripts/deploy/deploy.sh

# Rollback deployment
./scripts/deploy/deploy.sh --rollback

# Health check only
./scripts/deploy/deploy.sh --health-check
```

### Test Script
```bash
# Full test suite
./scripts/test/test.sh

# Unit tests only
./scripts/test/test.sh --unit-only

# Integration tests only
./scripts/test/test.sh --integration-only

# Coverage analysis only
./scripts/test/test.sh --coverage-only
```

## Examples

### Complete Service Generation
```bash
# Generate complete Rust service
ggen template generate templates/rust-service.tmpl \
    --var name="UserService" \
    --var description="User management service" \
    --var version="1.0.0" \
    --var author="ggen-examples"
```

### API Endpoint Generation
```bash
# Generate API endpoint
ggen template generate templates/api-endpoint.tmpl \
    --var name="UserAPI" \
    --var method="GET" \
    --var path="/api/v1/users" \
    --var description="User API endpoint"
```

### Database Schema Generation
```bash
# Generate database schema
ggen template generate templates/database-schema.tmpl \
    --var name="UserSchema" \
    --var database="postgresql" \
    --var orm="sqlx"
```

### Documentation Generation
```bash
# Generate documentation
ggen template generate templates/documentation.tmpl \
    --var name="UserDocs" \
    --var format="markdown" \
    --var style="github"
```

## Best Practices

### 1. Use Lifecycle Phases
- Define clear phases with dependencies
- Use hooks for pre/post execution
- Test with different environments
- Monitor execution state

### 2. Leverage AI Generation
- Use descriptive prompts
- Test with mock mode first
- Adjust temperature for creativity
- Validate generated code

### 3. Integrate RDF/SPARQL
- Design comprehensive domain models
- Use meaningful prefixes
- Cache query results
- Validate SPARQL syntax

### 4. Apply Security Measures
- Validate all inputs
- Use path traversal protection
- Block dangerous shell commands
- Enable audit logging

### 5. Test Thoroughly
- Run unit tests
- Test integration scenarios
- Validate E2E workflows
- Monitor performance

### 6. Document Everything
- Document templates
- Explain SPARQL queries
- Document lifecycle phases
- Include usage examples

## Troubleshooting

### Common Issues

1. **Template Generation Failed**
   - Check template syntax
   - Validate RDF data files
   - Check SPARQL query syntax
   - Verify variable names

2. **Lifecycle Phase Failed**
   - Check phase dependencies
   - Verify command syntax
   - Check environment variables
   - Review execution logs

3. **AI Generation Failed**
   - Check API key configuration
   - Verify model availability
   - Test with mock mode
   - Check network connectivity

4. **SPARQL Query Failed**
   - Validate SPARQL syntax
   - Check RDF data format
   - Verify prefix definitions
   - Test query in isolation

### Debug Mode

```bash
# Enable debug logging
RUST_LOG=debug ggen template generate templates/rust-service.tmpl

# Enable trace logging
RUST_LOG=trace ggen lifecycle run generate

# Verbose output
ggen template generate templates/rust-service.tmpl --verbose
```

### Log Files

- **Build Logs**: `logs/build.log`
- **Deployment Logs**: `logs/deployment.log`
- **Test Logs**: `logs/test.log`
- **Ggen Logs**: `.ggen/logs/ggen.log`

## Performance Optimization

### Build Performance
- Use `cargo make` for parallel builds
- Enable build caching
- Use incremental compilation
- Optimize dependency resolution

### Runtime Performance
- Enable query result caching
- Use connection pooling
- Implement request batching
- Monitor memory usage

### Template Performance
- Cache rendered templates
- Optimize SPARQL queries
- Use efficient filters
- Minimize file I/O

## Monitoring

### Health Checks
```bash
# Check service health
curl http://localhost:8080/health

# Check metrics
curl http://localhost:8080/metrics
```

### Logging
```bash
# View logs
tail -f logs/advanced-rust-project.log

# Filter logs
grep "ERROR" logs/advanced-rust-project.log
```

### Metrics
- Service uptime
- Request latency
- Error rates
- Resource usage

## Contributing

### Development Setup
1. Clone repository
2. Install dependencies
3. Run tests
4. Make changes
5. Submit pull request

### Code Style
- Follow Rust conventions
- Use `cargo fmt` for formatting
- Use `cargo clippy` for linting
- Write comprehensive tests

### Documentation
- Update README.md
- Add code comments
- Write API documentation
- Include usage examples

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Support

For support and questions:
- Check documentation
- Review examples
- Search issues
- Create new issue

---

*This example demonstrates the full power of ggen for advanced Rust project development.*
