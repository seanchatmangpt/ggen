<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Autonomous System - User Guide](#ggen-autonomous-system---user-guide)
  - [Table of Contents](#table-of-contents)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
    - [Quick Start](#quick-start)
  - [CLI Command Reference](#cli-command-reference)
    - [Core Commands](#core-commands)
      - [`ggen ai project`](#ggen-ai-project)
      - [`ggen ai generate`](#ggen-ai-generate)
      - [`ggen ai graph`](#ggen-ai-graph)
      - [`ggen ai sparql`](#ggen-ai-sparql)
    - [Autonomous Agent Commands](#autonomous-agent-commands)
      - [`ggen-mcp`](#ggen-mcp)
    - [Configuration Commands](#configuration-commands)
      - [`ggen ai config`](#ggen-ai-config)
  - [Example Workflows](#example-workflows)
    - [Workflow 1: Autonomous Test-Driven Development](#workflow-1-autonomous-test-driven-development)
    - [Workflow 2: Knowledge Graph-Driven Architecture](#workflow-2-knowledge-graph-driven-architecture)
    - [Workflow 3: Distributed System with Fault Tolerance](#workflow-3-distributed-system-with-fault-tolerance)
    - [Workflow 4: Template Market Integration](#workflow-4-template-market-integration)
    - [Workflow 5: Continuous Autonomous Refactoring](#workflow-5-continuous-autonomous-refactoring)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues](#common-issues)
      - [Issue: "API key not found"](#issue-api-key-not-found)
      - [Issue: "MCP server connection failed"](#issue-mcp-server-connection-failed)
      - [Issue: "Validation failed after max iterations"](#issue-validation-failed-after-max-iterations)
      - [Issue: "Graph loading error"](#issue-graph-loading-error)
      - [Issue: "Agent timeout"](#issue-agent-timeout)
    - [Debug Mode](#debug-mode)
    - [Getting Help](#getting-help)
    - [Performance Optimization](#performance-optimization)
    - [Reporting Issues](#reporting-issues)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Autonomous System - User Guide

## Table of Contents
1. [Getting Started](#getting-started)
2. [CLI Command Reference](#cli-command-reference)
3. [Example Workflows](#example-workflows)
4. [Troubleshooting](#troubleshooting)

## Getting Started

### Prerequisites

- Rust 1.70 or higher
- Docker (optional, for distributed deployments)
- An AI provider API key (Anthropic, OpenAI, or Ollama)

### Installation

```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/ggen
cd ggen

# Build the project
cargo build --release

# Install globally
cargo install --path .

# Set up environment variables
export ANTHROPIC_API_KEY="your-api-key-here"
# or
export OPENAI_API_KEY="your-api-key-here"
```

### Quick Start

1. **Initialize a new project with autonomous capabilities:**

```bash
ggen ai project \
  --description "REST API for task management" \
  --name task-api \
  --ollama
```

2. **Start the MCP server for autonomous operations:**

```bash
ggen-mcp
```

3. **Generate code with autonomous validation:**

```bash
ggen ai generate \
  --description "User authentication module" \
  --validate \
  --max-iterations 3
```

## CLI Command Reference

### Core Commands

#### `ggen ai project`

Create a new project with AI-powered scaffolding.

```bash
ggen ai project [OPTIONS]

Options:
  -d, --description <DESC>    Project description
  -n, --name <NAME>          Project name
      --ollama               Use Ollama provider
      --validate             Enable autonomous validation
  -o, --output <PATH>        Output directory
```

**Example:**
```bash
ggen ai project \
  -d "Microservice for user management" \
  -n user-service \
  --validate
```

#### `ggen ai generate`

Generate templates and code with AI assistance.

```bash
ggen ai generate [OPTIONS]

Options:
  -d, --description <DESC>    What to generate
      --validate             Enable iterative validation
      --max-iterations <N>   Maximum validation iterations (default: 3)
  -o, --output <PATH>        Output file
      --format <FORMAT>      Output format (rust, python, typescript)
```

**Example:**
```bash
ggen ai generate \
  -d "Database migration script for user table" \
  --validate \
  --format sql \
  -o migrations/001_users.sql
```

#### `ggen ai graph`

Generate RDF ontologies and knowledge graphs.

```bash
ggen ai graph [OPTIONS]

Options:
  -d, --description <DESC>    Graph description
  -o, --output <FILE>        Output file (.ttl, .rdf, .jsonld)
      --verify               Create reference file for verification
      --format <FORMAT>      RDF format (turtle, rdfxml, jsonld)
```

**Example:**
```bash
ggen ai graph \
  -d "Ontology for software development workflow" \
  -o ontologies/sdlc.ttl \
  --verify
```

#### `ggen ai sparql`

Generate SPARQL queries from natural language.

```bash
ggen ai sparql [OPTIONS]

Options:
  -d, --description <DESC>    Query description
  -g, --graph <FILE>         Graph file to query
  -o, --output <FILE>        Output SPARQL file
      --explain              Include query explanation
```

**Example:**
```bash
ggen ai sparql \
  -d "Find all agents with fault tolerance capability" \
  -g ontologies/sdlc.ttl \
  -o queries/fault_tolerant_agents.sparql \
  --explain
```

### Autonomous Agent Commands

#### `ggen-mcp`

Start the Model Context Protocol server for autonomous operations.

```bash
ggen-mcp [OPTIONS]

Options:
      --port <PORT>          Server port (default: 3000)
      --host <HOST>          Server host (default: localhost)
      --config <FILE>        Configuration file
      --agents <N>           Number of agents (default: 12)
      --topology <TYPE>      Swarm topology (mesh, hierarchical, ring, star)
```

**Example:**
```bash
ggen-mcp \
  --port 8080 \
  --agents 12 \
  --topology mesh \
  --config config/autonomous.toml
```

### Configuration Commands

#### `ggen ai config`

Manage AI provider configurations.

```bash
ggen ai config [SUBCOMMAND]

Subcommands:
  list                 List all configurations
  set <KEY> <VALUE>    Set configuration value
  get <KEY>            Get configuration value
  validate             Validate current configuration
```

**Example:**
```bash
ggen ai config set provider anthropic
ggen ai config set model claude-3-opus-20240229
ggen ai config validate
```

## Example Workflows

### Workflow 1: Autonomous Test-Driven Development

Generate a complete feature with TDD using autonomous agents:

```bash
# Step 1: Generate BDD scenarios
ggen ai generate \
  -d "User login feature with OAuth2" \
  --format bdd \
  --validate \
  -o features/login.feature

# Step 2: Generate test implementation (London School TDD)
ggen ai generate \
  -d "Tests for login feature using mocks" \
  --format rust \
  --validate \
  -o tests/login_test.rs

# Step 3: Generate implementation
ggen ai generate \
  -d "Login implementation passing all tests" \
  --format rust \
  --validate \
  --max-iterations 5 \
  -o src/auth/login.rs

# Step 4: Verify with autonomous validation
ggen ai validate \
  --tests tests/login_test.rs \
  --implementation src/auth/login.rs
```

### Workflow 2: Knowledge Graph-Driven Architecture

Create a system architecture from knowledge graphs:

```bash
# Step 1: Define domain ontology
ggen ai graph \
  -d "E-commerce platform domain model" \
  --verify \
  -o ontologies/ecommerce.ttl

# Step 2: Generate architecture from ontology
ggen ai project \
  --description "Microservices from e-commerce ontology" \
  --graph ontologies/ecommerce.ttl \
  --validate \
  --name ecommerce-platform

# Step 3: Query and validate architecture
ggen ai sparql \
  -d "Find all service dependencies and interfaces" \
  -g ontologies/ecommerce.ttl \
  --explain \
  -o queries/service_dependencies.sparql
```

### Workflow 3: Distributed System with Fault Tolerance

Build a Byzantine fault-tolerant system:

```bash
# Step 1: Start MCP server with Byzantine configuration
ggen-mcp \
  --topology mesh \
  --agents 12 \
  --config config/byzantine.toml

# Step 2: Generate fault-tolerant service
ggen ai generate \
  -d "Distributed consensus service with Byzantine fault tolerance" \
  --validate \
  --format rust \
  -o src/consensus/byzantine.rs

# Step 3: Generate verification tests
ggen ai generate \
  -d "Property-based tests for Byzantine fault tolerance" \
  --format rust \
  --validate \
  -o tests/byzantine_properties.rs
```

### Workflow 4: Template Market Integration

Publish and use templates from the marketplace:

```bash
# Step 1: Generate a template
ggen ai generate \
  -d "REST API template with authentication" \
  --format template \
  --validate \
  -o templates/rest-api-auth.ggt

# Step 2: Publish to marketplace (via MCP)
curl -X POST http://localhost:8080/mcp/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "name": "publish_template",
    "arguments": {
      "template_path": "templates/rest-api-auth.ggt",
      "metadata": {
        "name": "REST API with Auth",
        "description": "Production-ready REST API template",
        "tags": ["rest", "auth", "api"]
      }
    }
  }'

# Step 3: Search and use marketplace templates
ggen market search --tags rest,auth
ggen market install rest-api-auth
ggen gen --template rest-api-auth --output my-api/
```

### Workflow 5: Continuous Autonomous Refactoring

Set up autonomous monitoring and refactoring:

```bash
# Step 1: Configure autonomous monitoring
cat > config/autonomous.toml <<EOF
[monitoring]
enabled = true
check_interval_seconds = 300
auto_refactor = true

[policies]
max_function_lines = 50
max_file_lines = 500
min_test_coverage = 80

[agents]
count = 12
roles = [
  "validator",
  "refactor",
  "security",
  "performance",
  "documentation"
]
EOF

# Step 2: Start autonomous monitoring
ggen-mcp --config config/autonomous.toml --daemon

# Step 3: View autonomous actions
ggen ai monitor --status
ggen ai audit --last 24h
```

## Troubleshooting

### Common Issues

#### Issue: "API key not found"

**Solution:**
```bash
# Set the appropriate API key
export ANTHROPIC_API_KEY="your-key-here"
# or
export OPENAI_API_KEY="your-key-here"

# Verify configuration
ggen ai config validate
```

#### Issue: "MCP server connection failed"

**Solution:**
```bash
# Check if server is running
ps aux | grep ggen-mcp

# Restart server
killall ggen-mcp
ggen-mcp --port 8080

# Test connection
curl http://localhost:8080/health
```

#### Issue: "Validation failed after max iterations"

**Solution:**
```bash
# Increase iteration count
ggen ai generate \
  -d "your description" \
  --validate \
  --max-iterations 10  # Increased from default 3

# Or disable validation for initial generation
ggen ai generate \
  -d "your description" \
  -o output.rs

# Then validate separately
ggen ai validate --file output.rs --fix
```

#### Issue: "Graph loading error"

**Solution:**
```bash
# Verify graph file format
ggen graph validate ontologies/your-graph.ttl

# Convert to different format if needed
ggen graph convert \
  --input ontologies/your-graph.rdf \
  --output ontologies/your-graph.ttl \
  --format turtle
```

#### Issue: "Agent timeout"

**Solution:**
```bash
# Increase timeout in configuration
cat > config/agents.toml <<EOF
[agents]
timeout_ms = 60000  # 60 seconds
retry_count = 5
EOF

# Use configuration
ggen-mcp --config config/agents.toml
```

### Debug Mode

Enable verbose logging for troubleshooting:

```bash
# Set debug logging level
export RUST_LOG=debug

# Run command with debug output
ggen ai generate -d "test" --validate

# View MCP server logs
ggen-mcp --log-level debug
```

### Getting Help

```bash
# Command-specific help
ggen ai --help
ggen ai generate --help

# MCP server help
ggen-mcp --help

# View system status
ggen ai status

# Check agent health
ggen ai agents --status
```

### Performance Optimization

```bash
# Enable caching for faster generation
export GGEN_CACHE_ENABLED=true
export GGEN_CACHE_DIR=~/.cache/ggen

# Use local model for faster iteration
ggen ai config set provider ollama
ggen ai config set model qwen2.5-coder

# Parallel agent execution
ggen-mcp --agents 24 --topology mesh
```

### Reporting Issues

When reporting issues, include:

1. **System information:**
```bash
ggen --version
rustc --version
uname -a
```

2. **Configuration:**
```bash
ggen ai config list
```

3. **Logs:**
```bash
# Last 100 lines of logs
ggen ai logs --tail 100
```

4. **Minimal reproduction:**
```bash
# Exact command that failed
ggen ai generate -d "example" --validate
```

Submit issues at: https://github.com/seanchatmangpt/ggen/issues
