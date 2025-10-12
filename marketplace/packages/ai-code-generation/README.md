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

## AI Generation Examples - Marketplace-First Approach

### 1. **Search Marketplace** for existing AI patterns
```bash
# Search for AI and web service packages
ggen market search "ai service"
ggen market search "rust web service"
ggen market search "crud templates"
ggen market search "database integration"
```

### 2. **Install Required Packages**
```bash
# Install AI and template packages
ggen market add "ai-service-templates"         # AI-powered service generation
ggen market add "rust-axum-service"             # Axum web framework
ggen market add "crud-operations"               # CRUD operation templates
ggen market add "postgresql-database"           # Database integration
ggen market add "redis-cache"                   # Caching layer
ggen market add "openapi-documentation"         # API documentation
```

### 3. **Service Generation** using marketplace templates + AI
```bash
# Generate book management service using marketplace template
ggen template generate ai-service-templates:book-service.tmpl

# Enhance with AI for custom requirements
ggen ai generate -d "
Enhance the generated book service with these additional features:
- Advanced search with multiple filters
- Book recommendations based on reading history
- Integration with external book APIs
- Real-time inventory tracking
- Automated restocking alerts
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

## Key Benefits of Marketplace-First AI Development

### **Intelligent Code Reuse** - Don't reinvent the wheel
- Marketplace provides proven, production-ready AI patterns
- AI enhances existing templates rather than generating from scratch
- Combines human expertise (marketplace) with AI adaptability

### **Quality Assurance** - Production-ready from day one
- Marketplace templates include proper error handling, security, logging
- AI enhancements maintain code quality standards
- Comprehensive validation ensures generated code works

### **Rapid Iteration** - Focus on business logic, not boilerplate
- 80% of code comes from marketplace templates
- AI handles the remaining 20% of custom requirements
- Faster development cycles with higher quality

### **Consistency** - Standardized patterns across projects
- All AI-generated services follow same architectural patterns
- Uniform error handling and API design
- Easier maintenance and team collaboration

## Best Practices for AI + Marketplace Development

### 1. **Start with Marketplace Search**
```bash
# Always search first - someone may have already solved your problem
ggen market search "your specific requirement"
ggen market categories  # See what's available
```

### 2. **Use Marketplace Templates as Foundation**
```bash
# Install proven patterns
ggen market add "your-required-package"

# Generate from marketplace template
ggen template generate package-name:template.tmpl

# Then enhance with AI for custom needs
ggen ai generate "Add my specific business requirements"
```

### 3. **Leverage AI for Complex Logic**
```bash
# Use AI for:
# - Complex business rules
# - Custom algorithms
# - Integration requirements
# - Performance optimizations

# Don't use AI for:
# - Basic CRUD operations (use marketplace)
# - Standard web frameworks (use marketplace)
# - Common patterns (use marketplace)
```

### 4. **Validate Everything**
```bash
# Always validate generated code
ggen lifecycle run test
ggen graph validate data/domain.ttl
ggen template validate templates/*.tmpl
```

### 5. **Contribute Back to Marketplace**
```bash
# After successful customization, consider publishing improvements
ggen market publish "my-enhanced-pattern"
```

This example demonstrates how to combine ggen's marketplace (human expertise) with AI generation (adaptability) for optimal development speed and quality.
