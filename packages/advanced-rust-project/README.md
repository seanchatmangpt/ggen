# Advanced Rust Project Example

This example demonstrates all advanced features of ggen including:

- **Lifecycle Management**: Complete make.toml workflow with phases, hooks, and environments
- **AI-Powered Generation**: Templates generated using AI with RDF/SPARQL integration
- **SPARQL Queries**: Complex knowledge graph queries and data processing
- **Template Filters**: All text transformation filters and SPARQL helpers
- **File Injection**: Modify existing files with markers and line numbers
- **Shell Hooks**: Pre/post execution commands with security validation

## Project Structure

```
advanced-rust-project/
├── README.md
├── make.toml                 # Lifecycle configuration
├── ggen.toml                 # Project configuration
├── templates/                # AI-generated templates
│   ├── rust-service.tmpl     # Complete Rust service
│   ├── api-endpoint.tmpl     # API endpoint generation
│   ├── database-schema.tmpl  # Database schema from RDF
│   └── documentation.tmpl    # Auto-generated docs
├── data/                     # RDF knowledge graphs
│   ├── domain.ttl           # Domain model
│   ├── api-spec.ttl         # API specification
│   └── database.ttl         # Database schema
├── generated/               # Generated code output
└── scripts/                 # Build and deployment scripts
```

## Features Demonstrated

### 1. Lifecycle Management

Complete workflow automation with:
- **Phases**: init, setup, generate, build, test, deploy
- **Environments**: development, staging, production
- **Hooks**: Pre/post execution commands
- **State Management**: Track execution progress
- **Parallel Execution**: Concurrent phase execution

### 2. AI-Powered Generation

- **Template Generation**: AI creates templates from natural language
- **RDF Integration**: Knowledge graphs drive code generation
- **SPARQL Queries**: Complex data queries and transformations
- **Frontmatter Generation**: SEO-optimized metadata
- **Graph Generation**: RDF graphs from descriptions

### 3. Advanced Template Features

- **Text Filters**: 20+ case conversion filters
- **SPARQL Helpers**: Query result processing functions
- **File Injection**: Modify existing files with precision
- **Shell Hooks**: Secure command execution
- **Variable Processing**: Flexible YAML variable handling

### 4. Security Features

- **Path Traversal Protection**: Secure file operations
- **Shell Injection Prevention**: Command validation
- **Input Sanitization**: Safe template processing
- **Error Handling**: Comprehensive error management

## Usage

### Initialize Project
```bash
cd examples/advanced-rust-project
ggen lifecycle list
```

### Generate Code
```bash
# Generate complete Rust service
ggen template generate templates/rust-service.tmpl

# Generate API endpoints
ggen template generate templates/api-endpoint.tmpl

# Generate database schema
ggen template generate templates/database-schema.tmpl
```

### Run Lifecycle Phases
```bash
# Run single phase
ggen lifecycle run generate

# Run pipeline
ggen lifecycle pipeline setup generate build test

# Run with environment
ggen lifecycle run deploy --env production
```

### AI-Powered Generation
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

### SPARQL Integration
- Complex knowledge graph queries
- Data transformation and filtering
- Template variable population
- Real-time data processing

### Template Filters
- Case conversion (camel, snake, kebab, etc.)
- String manipulation (pluralize, singularize)
- SPARQL result processing
- Local name extraction

### File Operations
- Atomic file writes
- Backup creation
- Idempotent operations
- Conflict resolution

### Security
- Path traversal prevention
- Shell injection protection
- Input validation
- Secure error handling

## Configuration

### make.toml
Complete lifecycle configuration with phases, hooks, and environments.

### ggen.toml
Project-specific configuration for templates, data sources, and output.

### Environment Variables
- `GGEN_ENV`: Environment selection
- `GGEN_DRY_RUN`: Preview mode
- `GGEN_VERBOSE`: Detailed logging

## Examples

See individual template files for specific examples of:
- RDF/SPARQL integration
- AI-powered generation
- Complex template logic
- File injection patterns
- Security best practices

## Best Practices

1. **Use lifecycle phases** for complex workflows
2. **Leverage AI generation** for boilerplate code
3. **Integrate RDF/SPARQL** for data-driven generation
4. **Apply security measures** for all operations
5. **Test thoroughly** with different environments
6. **Document templates** with clear frontmatter
7. **Use version control** for generated code
8. **Monitor execution** with state tracking

This example serves as a comprehensive reference for advanced ggen usage patterns.
