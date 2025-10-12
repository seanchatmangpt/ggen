# AI-Powered Code Generation Example

This example demonstrates advanced AI-powered code generation using ggen's AI capabilities:

- **Template Generation**: AI creates templates from natural language descriptions
- **Code Generation**: AI generates Rust code with proper error handling and testing
- **SPARQL Integration**: AI generates SPARQL queries from domain descriptions
- **Documentation**: AI generates comprehensive documentation
- **Validation**: AI validates generated code for quality and correctness

## Features Demonstrated

### AI Template Generation
- Natural language to template conversion
- Variable extraction and type inference
- Frontmatter generation with proper metadata
- Template validation and improvement

### AI Code Generation
- Rust service generation with multiple frameworks
- Database schema generation
- API endpoint generation
- Test case generation
- Error handling implementation

### AI SPARQL Generation
- Natural language to SPARQL query conversion
- Domain model analysis
- Query optimization suggestions
- Result interpretation

### AI Documentation Generation
- API documentation generation
- Architecture documentation
- User guides and tutorials
- Code comments and examples

## Quick Start

```bash
# Generate a complete Rust web service
ggen ai generate -d "Create a REST API for managing books with CRUD operations, authentication, and search functionality" --validate

# Generate SPARQL queries for domain analysis
ggen ai sparql -d "Find all entities and their relationships in the domain model" -g data/domain.ttl

# Generate documentation
ggen ai generate -d "Create comprehensive API documentation with examples and usage guides" --validate

# Generate test cases
ggen ai generate -d "Generate comprehensive test cases for the book management API" --validate
```

## Project Structure

```
ai-code-generation/
├── README.md                 # This file
├── ggen.toml                 # ggen configuration
├── make.toml                 # Lifecycle configuration
├── templates/                # AI-generated templates
│   ├── rust-service.tmpl     # Rust service template
│   ├── database-schema.tmpl  # Database schema template
│   ├── api-docs.tmpl         # API documentation template
│   └── tests.tmpl            # Test cases template
├── data/                     # Domain models and queries
│   ├── domain.ttl            # RDF domain model
│   ├── queries.sparql        # Generated SPARQL queries
│   └── examples.json         # Example data
├── generated/                # AI-generated code
│   ├── services/             # Generated services
│   ├── schemas/              # Generated schemas
│   ├── docs/                 # Generated documentation
│   └── tests/                # Generated tests
└── examples/                 # Usage examples
    ├── book-service/         # Book management service
    ├── user-service/         # User management service
    └── inventory-service/    # Inventory management service
```

## AI Generation Examples

### 1. Service Generation
```bash
# Generate a complete book management service
ggen ai generate -d "
Create a Rust web service for managing a library catalog with the following features:
- Book CRUD operations (Create, Read, Update, Delete)
- Author management
- Category classification
- Search functionality with filters
- Pagination support
- Input validation
- Error handling with proper HTTP status codes
- Database integration with PostgreSQL
- Redis caching for performance
- Comprehensive logging
- Health check endpoint
- OpenAPI documentation
" --validate --max-iterations 3
```

### 2. SPARQL Query Generation
```bash
# Generate SPARQL queries for domain analysis
ggen ai sparql -d "
Analyze the book domain model and generate queries for:
- Finding all books by a specific author
- Listing books in a category with their details
- Finding books published in a date range
- Identifying books with low inventory
- Generating reports on book popularity
- Finding related books based on categories
" -g data/domain.ttl -o data/generated-queries.sparql
```

### 3. Documentation Generation
```bash
# Generate comprehensive documentation
ggen ai generate -d "
Create comprehensive documentation for the book management API including:
- API overview and architecture
- Endpoint documentation with examples
- Authentication and authorization
- Error handling and status codes
- Rate limiting and usage guidelines
- Integration examples
- Troubleshooting guide
- Performance optimization tips
" --validate
```

### 4. Test Generation
```bash
# Generate comprehensive test suite
ggen ai generate -d "
Generate a comprehensive test suite for the book management API including:
- Unit tests for all service methods
- Integration tests for API endpoints
- Database interaction tests
- Authentication and authorization tests
- Error handling tests
- Performance tests
- Load tests
- Mock data generation
" --validate
```

## Configuration

### ggen.toml
```toml
[ai]
provider = "ollama"
model = "qwen2.5-coder"
temperature = 0.7
max_tokens = 4000
timeout = 30

[ai.prompts]
system = "You are an expert Rust developer specializing in web services, databases, and API design. Generate production-ready code with proper error handling, testing, and documentation."
user_prefix = "Generate a Rust service with the following requirements:"

[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3

[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true

[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"
```

## Advanced Features

### Iterative Improvement
The AI system can iteratively improve generated code based on validation feedback:

```bash
# Generate with iterative improvement
ggen ai generate -d "Create a user authentication service" --validate --max-iterations 5
```

### Quality Validation
Generated code is automatically validated for:
- Compilation correctness
- Test coverage
- Code quality metrics
- Security best practices
- Performance optimization

### Template Customization
Templates can be customized for specific frameworks and patterns:

```bash
# Generate with specific framework
ggen ai generate -d "Create a microservice" --template rust-service.tmpl --vars framework=axum
```

## Best Practices

### 1. Clear Descriptions
Provide clear, detailed descriptions for better AI generation:
- Specify exact requirements
- Include technical constraints
- Mention preferred frameworks and patterns
- Describe expected behavior

### 2. Iterative Refinement
Use iterative generation to improve quality:
- Start with basic requirements
- Add validation feedback
- Refine based on results
- Validate final output

### 3. Validation and Testing
Always validate generated code:
- Check compilation
- Run tests
- Verify functionality
- Review security implications

### 4. Documentation
Generate comprehensive documentation:
- API documentation
- Usage examples
- Architecture diagrams
- Troubleshooting guides

This example serves as a comprehensive reference for AI-powered code generation with ggen.
