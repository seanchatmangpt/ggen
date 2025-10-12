# Ggen Development Environment - Production CLI Tool

## 🚨 CRITICAL: GGEN MARKETPLACE & LIFECYCLE WORKFLOW

**ABSOLUTE RULES**:
1. **ALWAYS use ggen marketplace commands for package management**
2. **ALWAYS use ggen lifecycle commands for project workflows**
3. **NEVER use direct cargo commands - use ggen market and lifecycle**
4. **ALWAYS organize examples in examples/ subdirectory**

### ⚡ GOLDEN RULE: "1 COMMAND = ALL GGEN OPERATIONS"

**MANDATORY PATTERNS:**
- **ggen market search/add/list/update** for package management
- **ggen lifecycle run/list/show/pipeline** for project workflows
- **ggen template generate** for code generation
- **ggen ai generate** for AI-powered development
- **ggen graph query** for SPARQL operations

### 🎯 CRITICAL: Ggen Marketplace for Package Management

**Ggen marketplace is the PRIMARY way to manage packages:**
```bash
# ✅ CORRECT: Use ggen marketplace for all package operations
ggen market search "rust cli" --category templates
ggen market add "rig-mcp-integration"
ggen market list --installed
ggen market update "all"
ggen market info "rig-mcp"
```

**Available marketplace commands:**
- `ggen market search <query>` - Search marketplace packages
- `ggen market add <package>` - Install package from marketplace
- `ggen market remove <package>` - Remove installed package
- `ggen market list` - List all available/installed packages
- `ggen market update` - Update packages to latest versions
- `ggen market info <package>` - Show detailed package information
- `ggen market recommend` - Get personalized recommendations
- `ggen market offline` - Browse cached marketplace data
- `ggen market sync` - Synchronize with remote marketplace
- `ggen market publish` - Publish new package to marketplace

### 🎯 CRITICAL: Ggen Lifecycle for Project Management

**Ggen lifecycle is the PRIMARY way to manage project workflows:**
```bash
# ✅ CORRECT: Use ggen lifecycle for all project operations
ggen lifecycle list  # Show all available phases
ggen lifecycle run init  # Initialize project
ggen lifecycle run setup  # Install dependencies
ggen lifecycle run generate  # Generate code from templates
ggen lifecycle run build  # Build project
ggen lifecycle run test  # Run tests
ggen lifecycle run deploy  # Deploy to target environment
```

**Available lifecycle commands:**
- `ggen lifecycle list` - List all available phases
- `ggen lifecycle show <phase>` - Show details of specific phase
- `ggen lifecycle run <phase>` - Run single lifecycle phase
- `ggen lifecycle pipeline <phases>` - Run multiple phases in sequence

### 📁 File Organization Rules

**NEVER save to root folder. Use these directories:**
- `/src` - Source code files
- `/tests` - Test files
- `/docs` - Documentation and markdown files
- `/config` - Configuration files
- `/scripts` - Utility scripts
- `/examples` - Example code

## Project Overview

This project uses ggen - a language-agnostic, deterministic code projection CLI that turns ontologies + RDF-like metadata into reproducible code projections. It features a comprehensive marketplace and lifecycle system.

## Ggen Commands

### Core Commands
- `ggen --help` - Show all available commands
- `ggen version` - Show ggen version
- `ggen --list` - List all available subcommands

### Template Commands
- `ggen template list` - List available templates
- `ggen template generate <template>` - Generate code from template
- `ggen template validate <template>` - Validate template syntax

### AI Commands
- `ggen ai generate "<prompt>"` - Generate code using AI
- `ggen ai analyze <file>` - Analyze existing code
- `ggen ai optimize <file>` - Optimize code performance

### Graph/SPARQL Commands
- `ggen graph query "<sparql>"` - Execute SPARQL query
- `ggen graph validate <file>` - Validate RDF data
- `ggen graph info` - Show graph statistics

### Lifecycle Commands
- `ggen lifecycle list` - List all available phases
- `ggen lifecycle show <phase>` - Show phase details
- `ggen lifecycle run <phase>` - Run single phase
- `ggen lifecycle pipeline <phases>` - Run multiple phases

### Marketplace Commands
- `ggen market search "<query>"` - Search marketplace packages
- `ggen market add <package>` - Install package
- `ggen market list` - List installed packages
- `ggen market info <package>` - Show package details

## Ggen Workflow

1. **Initialize** (`ggen lifecycle run init`) - Set up project structure
2. **Setup** (`ggen lifecycle run setup`) - Install dependencies via marketplace
3. **Generate** (`ggen lifecycle run generate`) - Generate code from templates
4. **Build** (`ggen lifecycle run build`) - Build the project
5. **Test** (`ggen lifecycle run test`) - Run comprehensive tests
6. **Deploy** (`ggen lifecycle run deploy`) - Deploy to target environment

## Code Style & Best Practices

- **Modular Design**: Files under 500 lines
- **Environment Safety**: Never hardcode secrets
- **Test-First**: Write tests before implementation
- **Clean Architecture**: Separate concerns
- **Documentation**: Keep updated

### 🚨 Production Code Quality Rules

**NEVER use `.expect()` or `.unwrap()` in production code:**
- ❌ `.expect("error message")` - Panics on error (crashes the program)
- ❌ `.unwrap()` - Panics without explanation
- ✅ Use `?` operator with proper Result types
- ✅ Use `.map_err(|e| anyhow::Error::from(e))?` for error conversion
- ✅ Use `.unwrap_or_default()` or `.unwrap_or_else()` for safe defaults

**Exception:** `.expect()` is acceptable in:
- Test code (`#[cfg(test)]` or `tests/` directory)
- Example code (`examples/` directory)
- One-time initialization that should fail fast (rare cases)

**Why:** Production code must handle errors gracefully, not crash. Users running in containers, VMs, or with unexpected system states will experience crashes instead of helpful error messages.

**Example fixes:**
```rust
// ❌ BAD - Crashes in production
let time = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("System clock error")
    .as_millis();

// ✅ GOOD - Returns error that can be handled
let time = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .map(|d| d.as_millis())
    .map_err(|_| anyhow::anyhow!("System clock error"))?;
```

## 🚀 Ggen Marketplace Packages

### Available Package Categories
- `ai` - AI and LLM integration packages
- `templates` - Code generation templates
- `cli` - Command-line interface tools
- `web` - Web framework integrations
- `database` - Database and ORM packages
- `testing` - Testing and validation tools
- `security` - Security and authentication packages

### Featured Packages

#### Rig MCP Integration (`rig-mcp-integration`)
- **Description**: Production-ready Rig LLM framework + MCP protocol integration
- **Version**: 0.1.0
- **Category**: ai
- **Features**:
  - Multi-provider LLM support (OpenAI, Anthropic, Cohere, Deepseek, Gemini, Ollama, 20+)
  - Dynamic MCP tool loading with vector-based selection
  - Multi-transport MCP support (stdio, SSE, HTTP)
  - Production-ready patterns from official MCP Rust SDK
  - Embedding-based intelligent tool selection

#### CLI Templates (`noun-verb-cli`)
- **Description**: Complete CLI application templates with noun-verb architecture
- **Version**: 1.0.0
- **Category**: templates
- **Features**:
  - Full CRUD operations for entities
  - Clap-based command structure
  - Comprehensive error handling
  - Integration testing
  - Documentation generation

#### Web API Templates (`api-endpoint`)
- **Description**: REST API endpoint templates with OpenAPI documentation
- **Version**: 1.0.0
- **Category**: templates
- **Features**:
  - Axum-based HTTP handlers
  - Request/response validation
  - Error handling and status codes
  - OpenAPI specification generation
  - Rate limiting and security

### Package Installation

```bash
# Install specific package
ggen market add "rig-mcp-integration"

# Install template package
ggen market add "noun-verb-cli"

# List available packages
ggen market search "rust"

# Get package information
ggen market info "rig-mcp-integration"

# Update all packages
ggen market update
```

## 🎯 Ggen Tool Workflow

### Ggen Handles ALL DEVELOPMENT:
- **Marketplace**: Package discovery, installation, and management
- **Lifecycle**: Project initialization, dependency management, code generation
- **Templates**: Code generation from structured templates
- **AI Integration**: Intelligent code generation and analysis
- **Graph Operations**: SPARQL queries and RDF data processing
- **Testing**: Comprehensive test execution and validation
- **Deployment**: Multi-environment deployment management

### Core Development Workflow:
1. **Initialize Project** (`ggen lifecycle run init`)
2. **Install Dependencies** (`ggen market add <packages>`)
3. **Generate Code** (`ggen template generate <template>`)
4. **Run Tests** (`ggen lifecycle run test`)
5. **Deploy** (`ggen lifecycle run deploy`)

### Example Development Session:
```bash
# Initialize a new Rust project
ggen lifecycle run init

# Install required packages
ggen market add "rig-mcp-integration"
ggen market add "noun-verb-cli"

# Generate API endpoints
ggen template generate templates/api-endpoint.tmpl

# Generate database schema
ggen template generate templates/database-schema.tmpl

# Run comprehensive tests
ggen lifecycle run test

# Deploy to staging
ggen lifecycle run deploy --env staging
```

**KEY**: Ggen provides the complete development toolchain - marketplace, lifecycle, templates, AI, and deployment.

## 🚀 Quick Setup

### Install Ggen CLI
```bash
# Install ggen CLI (requires Rust)
cargo install ggen

# Or build from source
cargo build --release
cargo install --path .
```

### Initialize Project
```bash
# Initialize a new ggen project
ggen lifecycle run init

# Install core packages
ggen market add "rig-mcp-integration"
ggen market add "noun-verb-cli"

# Verify installation
ggen --version
ggen market list
ggen lifecycle list
```

### Example Project Creation
```bash
# Create a comprehensive Rust showcase project
cd examples/comprehensive-rust-showcase

# Run the complete lifecycle
ggen lifecycle run init
ggen lifecycle run setup
ggen lifecycle run generate
ggen lifecycle run build
ggen lifecycle run test
ggen lifecycle run deploy --env development

# Or run all phases in pipeline
ggen lifecycle pipeline "init setup generate build test deploy"
```

### Marketplace Operations
```bash
# Search for packages
ggen market search "rust cli"
ggen market search "web api" --category templates

# Install packages
ggen market add "rig-mcp-integration"
ggen market add "api-endpoint"

# List installed packages
ggen market list --installed

# Update packages
ggen market update
```

### Template Generation
```bash
# Generate code from templates
ggen template generate templates/rust-service.tmpl
ggen template generate templates/api-endpoint.tmpl
ggen template generate templates/database-schema.tmpl

# Generate with AI assistance
ggen ai generate "Create a user authentication service"
ggen ai analyze generated/src/services/user.rs
```

### Graph Operations
```bash
# Execute SPARQL queries
ggen graph query "SELECT ?entity ?label WHERE { ?entity a ex:Entity ; rdfs:label ?label }"

# Validate RDF data
ggen graph validate data/domain.ttl

# Show graph statistics
ggen graph info
```

## 🚀 Ggen Development Workflow

### The Correct Pattern:

1. **Initialize Project** - Use ggen lifecycle to set up project structure
2. **Install Dependencies** - Use ggen marketplace to add required packages
3. **Generate Code** - Use ggen templates and AI for code generation
4. **Test & Validate** - Use ggen lifecycle for comprehensive testing
5. **Deploy** - Use ggen lifecycle for multi-environment deployment

### Example Comprehensive Project Development:

```bash
# Single workflow for complete project creation
cd examples/comprehensive-rust-showcase

# Initialize project structure
ggen lifecycle run init

# Install AI and template packages
ggen market add "rig-mcp-integration"
ggen market add "noun-verb-cli"
ggen market add "api-endpoint"

# Generate all components
ggen template generate templates/rust-service.tmpl
ggen template generate templates/api-endpoint.tmpl
ggen template generate templates/database-schema.tmpl
ggen template generate templates/documentation.tmpl
ggen template generate templates/tests.tmpl
ggen template generate templates/deployment.tmpl

# Build and test everything
ggen lifecycle run build
ggen lifecycle run test

# Deploy to development
ggen lifecycle run deploy --env development

# Generate comprehensive documentation
ggen ai generate "Create comprehensive README for this project"
```

### Complete Development Session:

```bash
# Full development workflow in one session
[Comprehensive Project Development]:

  # Initialize project
  ggen lifecycle run init

  # Install all required packages
  ggen market add "rig-mcp-integration"
  ggen market add "api-endpoint"
  ggen market add "database-schema"

  # Generate core components
  ggen template generate templates/rust-service.tmpl
  ggen template generate templates/api-endpoint.tmpl
  ggen template generate templates/database-schema.tmpl

  # Add tests and deployment
  ggen template generate templates/tests.tmpl
  ggen template generate templates/deployment.tmpl

  # Build, test, and validate
  ggen lifecycle run build
  ggen lifecycle run test
  ggen lifecycle run security

  # Deploy to staging
  ggen lifecycle run deploy --env staging
```

## Performance & Quality

### Ggen Performance Features
- **Deterministic Outputs** - Same inputs always produce identical results
- **Parallel Execution** - Lifecycle phases run concurrently where possible
- **Caching** - Intelligent caching of expensive operations
- **Memory Safety** - Zero-cost abstractions with Rust type safety
- **Error Handling** - Comprehensive error handling with actionable messages

### Quality Assurance
- **Template Validation** - All templates validated before generation
- **RDF Graph Validation** - Semantic data validation with SHACL
- **SPARQL Query Optimization** - Query performance analysis and optimization
- **Security Scanning** - Automated vulnerability detection
- **Code Quality** - Clippy linting and formatting enforcement

## Advanced Features

### Multi-Environment Support
- **Development** - Fast iteration with debug features
- **Staging** - Pre-production testing and validation
- **Production** - Optimized for performance and security

### AI Integration
- **Multi-Provider Support** - OpenAI, Anthropic, Ollama, and more
- **Context-Aware Generation** - Uses project structure and RDF data
- **Natural Language Processing** - Convert natural language to SPARQL
- **Code Analysis** - Intelligent code review and optimization

### Graph-Driven Development
- **RDF Data Sources** - Structured domain modeling
- **SPARQL Queries** - Semantic data extraction and transformation
- **SHACL Validation** - Constraint validation for generated code
- **Provenance Tracking** - Complete audit trail of generation decisions

## Integration Tips

1. **Start with Lifecycle** - Always use `ggen lifecycle run init` to start projects
2. **Use Marketplace** - Install packages with `ggen market add` instead of manual dependency management
3. **Leverage Templates** - Generate code with `ggen template generate` for consistency
4. **AI Assistance** - Use `ggen ai generate` for complex requirements
5. **Graph Validation** - Use `ggen graph validate` to ensure data integrity
6. **Comprehensive Testing** - Use `ggen lifecycle run test` for full validation
7. **Multi-Environment Deployment** - Use `ggen lifecycle run deploy --env <env>`

## Support

### Documentation
- **CLI Reference**: `docs/cli.md` - Complete command reference
- **Marketplace Guide**: `docs/marketplace.md` - Package management guide
- **Lifecycle Guide**: `docs/lifecycle.md` - Project workflow guide
- **Template Guide**: `docs/templates.md` - Code generation guide

### Examples
- **Comprehensive Showcase**: `examples/comprehensive-rust-showcase/` - Complete project example
- **CLI Templates**: `examples/noun-verb-cli/` - CLI application templates
- **Web Templates**: `examples/api-endpoint/` - API endpoint templates

### Community
- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://seanchatmangpt.github.io/ggen/

---

**Remember: Ggen provides the complete development toolchain - marketplace, lifecycle, templates, AI, and deployment!**

# Ggen Development Guidelines

## Core Principles

1. **Use Ggen Toolchain** - Always use ggen marketplace, lifecycle, templates, and AI commands
2. **Deterministic Development** - Same inputs should always produce identical outputs
3. **Comprehensive Testing** - Every component must have tests that run from lifecycle
4. **Production Ready** - All generated code must be production-ready with proper error handling
5. **Security First** - Implement security hardening and vulnerability scanning
6. **Documentation** - Generate comprehensive documentation for all components

## Workflow Guidelines

1. **Initialize with Lifecycle** - Start every project with `ggen lifecycle run init`
2. **Package Management** - Use `ggen market add` for all dependencies
3. **Template Generation** - Use `ggen template generate` for consistent code structure
4. **AI Enhancement** - Use `ggen ai generate` for complex requirements
5. **Validation** - Use `ggen graph validate` and `ggen lifecycle run test`
6. **Deployment** - Use `ggen lifecycle run deploy` for all environments

## Code Quality Standards

1. **Error Handling** - Never use `.expect()` or `.unwrap()` in production code
2. **Type Safety** - Leverage Rust's type system for compile-time guarantees
3. **Performance** - Implement efficient algorithms and data structures
4. **Security** - Follow security best practices and vulnerability scanning
5. **Testing** - Maintain high test coverage with meaningful assertions
6. **Documentation** - Keep code comments and generated docs up to date

## Example Project Structure

All example projects should demonstrate the complete ggen workflow:

```bash
examples/comprehensive-rust-showcase/
├── ggen.toml              # Project configuration
├── make.toml              # Lifecycle configuration
├── data/                  # RDF/SPARQL data sources
│   ├── domain.ttl        # Domain model
│   ├── api-spec.ttl      # API specification
│   ├── database.ttl      # Database schema
│   └── queries.ttl       # SPARQL queries
├── templates/             # Code generation templates
│   ├── rust-service.tmpl # Complete service template
│   ├── api-endpoint.tmpl # API endpoint template
│   ├── database-schema.tmpl # Database schema template
│   ├── tests.tmpl        # Comprehensive test template
│   └── deployment.tmpl   # Deployment configuration template
├── generated/             # Generated code (created by lifecycle)
└── README.md              # Comprehensive project documentation
```

## Command Usage

Always use the correct ggen commands:

```bash
# ✅ CORRECT - Use ggen marketplace
ggen market search "rust cli"
ggen market add "rig-mcp-integration"

# ✅ CORRECT - Use ggen lifecycle
ggen lifecycle run init
ggen lifecycle run test
ggen lifecycle run deploy --env production

# ✅ CORRECT - Use ggen templates
ggen template generate templates/rust-service.tmpl

# ❌ WRONG - Never use direct cargo commands
# cargo add rig-mcp-integration
# cargo test
# cargo build
```

## Testing Requirements

Every example project must include:

1. **Unit Tests** - Test individual functions and methods
2. **Integration Tests** - Test component interactions
3. **End-to-End Tests** - Test complete workflows
4. **Performance Tests** - Validate performance requirements
5. **Security Tests** - Verify security hardening
6. **Lifecycle Tests** - Ensure all phases execute successfully

## Documentation Standards

All examples must include:

1. **Comprehensive README** - Complete project overview and usage
2. **API Documentation** - Generated OpenAPI specifications
3. **Architecture Documentation** - System design and data flow
4. **Deployment Guide** - Multi-environment deployment instructions
5. **Troubleshooting Guide** - Common issues and solutions
6. **Performance Metrics** - Benchmarks and optimization notes
