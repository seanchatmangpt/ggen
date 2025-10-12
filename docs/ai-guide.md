<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen AI Guide - AI-Powered Code Generation](#ggen-ai-guide---ai-powered-code-generation)
  - [Overview](#overview)
  - [Quick Start](#quick-start)
  - [AI Commands](#ai-commands)
    - [Template Generation](#template-generation)
    - [SPARQL Query Generation](#sparql-query-generation)
    - [RDF Graph Generation](#rdf-graph-generation)
    - [Project Scaffolding](#project-scaffolding)
    - [Source File Analysis](#source-file-analysis)
  - [AI Models and Providers](#ai-models-and-providers)
    - [Supported Providers](#supported-providers)
    - [Configuration](#configuration)
      - [Environment Variables](#environment-variables)
      - [Ollama Setup](#ollama-setup)
    - [Model Selection](#model-selection)
  - [Configuration](#configuration-1)
    - [Global Configuration](#global-configuration)
    - [Provider-Specific Configuration](#provider-specific-configuration)
  - [Advanced Usage](#advanced-usage)
    - [Custom Prompts](#custom-prompts)
    - [Batch Generation](#batch-generation)
    - [Integration with Templates](#integration-with-templates)
  - [Best Practices](#best-practices)
    - [Prompt Engineering](#prompt-engineering)
    - [Quality Assurance](#quality-assurance)
    - [Performance](#performance)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues](#common-issues)
    - [Debug Mode](#debug-mode)
  - [Examples](#examples)
    - [Complete Workflow](#complete-workflow)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen AI Guide - AI-Powered Code Generation

**Version**: v1.2.0 | **Status**: ✅ Production Ready

**ggen-ai v1.2.0** introduces intelligent code generation using advanced LLMs with multi-provider support (OpenAI, Anthropic, Ollama).

## Overview

The AI module (`ggen-ai`) provides natural language interfaces for generating:
- **Templates** from descriptions
- **SPARQL queries** from intent
- **RDF graphs** from domain descriptions
- **Complete project structures** from requirements
- **Template extraction** from existing source files

## Quick Start

```bash
# Generate a template from natural language
ggen ai generate "A Rust REST API controller" --language rust --output api.tmpl

# Generate SPARQL queries from intent
ggen ai sparql "Find all users with admin role" --graph data.ttl

# Generate RDF ontologies from descriptions
ggen ai graph "Person management system" --output person.ttl

# Generate complete project structures
ggen ai project "E-commerce API" --name shop --language rust --output project/

# Extract templates from existing code
ggen ai from-source src/main.rs --language rust --output template.tmpl
```

## AI Commands

### Template Generation

Generate templates from natural language descriptions:

```bash
ggen ai generate <description> [OPTIONS]

Options:
  -d, --description <DESC>    Description of what to generate (required)
  -e, --examples <EXAMPLES>   Examples or requirements (can be used multiple times)
  -o, --output <FILE>         Output template file
  --model <MODEL>             AI model to use (default: qwen3-coder:30b)
  --temperature <FLOAT>       Sampling temperature (0.0-2.0)
  --max-tokens <INT>          Maximum tokens to generate
  --validate                  Enable iterative validation and improvement
  --max-iterations <INT>      Maximum iterations for validation (default: 3)
  --mock                      Use mock client for testing
```

**Examples:**
```bash
# Basic template generation
ggen ai generate "A simple CLI tool" -o cli.tmpl

# Template with examples and validation
ggen ai generate "REST API controller" \
  -e "Should follow REST conventions" \
  -e "Include CRUD operations" \
  -o api.tmpl --validate

# Python web application with custom model
ggen ai generate "FastAPI application" \
  -e "Use async/await patterns" \
  -e "Include CORS middleware" \
  --model gpt-4o \
  -o app.tmpl
```

### SPARQL Query Generation

Generate SPARQL queries from natural language intent:

```bash
ggen ai sparql <description> [OPTIONS]

Options:
  -d, --description <DESC>    Description of the SPARQL query to generate (required)
  -g, --graph <FILE>          RDF graph file for context (optional)
  -o, --output <FILE>         Output file path (optional)
  -f, --format <FORMAT>       Output format: sparql, json (default: sparql)
  --model <MODEL>             AI model to use
  --temperature <FLOAT>       Sampling temperature (0.0-2.0)
  --max-tokens <INT>          Maximum tokens to generate
  --mock                      Use mock client for testing
```

**Examples:**
```bash
# Generate basic SPARQL query
ggen ai sparql "Find all people" -o query.sparql

# Generate query with graph context
ggen ai sparql "Get user profiles with emails" \
  -g ontology.ttl -f json -o profiles.json

# Generate SPARQL with custom model
ggen ai sparql "Find all active projects" \
  --model gpt-4o --temperature 0.3 -o projects.sparql
```

### RDF Graph Generation

Generate RDF ontologies from domain descriptions:

```bash
ggen ai graph <description> [OPTIONS]

Options:
  -d, --description <DESC>    Description of the RDF graph to generate (required)
  --domain <DOMAIN>           Domain or context for the graph (optional)
  -b, --base-iri <IRI>        Base IRI for the ontology (optional)
  -o, --output <FILE>         Output file path (required)
  -f, --format <FORMAT>       Output format: turtle, rdf, jsonld, ntriples (default: turtle)
  --include-examples          Include example data instances
  --verify                    Verify the generated graph can be loaded
  --model <MODEL>             AI model to use
  --temperature <FLOAT>       Sampling temperature (0.0-2.0)
  --max-tokens <INT>          Maximum tokens to generate
  --mock                      Use mock client for testing
```

**Examples:**
```bash
# Generate basic domain ontology
ggen ai graph "Person management system with roles" \
  -o person.ttl --include-examples --verify

# E-commerce ontology with custom base IRI
ggen ai graph "Product catalog with categories and reviews" \
  -o catalog.ttl -b "http://ecommerce.example.org/" \
  --include-examples --verify

# Organization structure as JSON-LD
ggen ai graph "Organization structure" \
  -o org.jsonld -f jsonld --include-examples --verify
```

### Project Scaffolding

Generate complete project structures:

```bash
ggen ai project [OPTIONS]

Options:
  -d, --description <DESC>    Project description (required)
  -n, --name <NAME>           Project name (required)
  -l, --language <LANG>       Target programming language (default: rust)
  -f, --framework <FRAMEWORK> Target framework (optional)
  -o, --output <DIR>          Output directory (default: ./generated-project)
  --tests                     Include comprehensive test suite
  --docs                      Include documentation with examples
  --ci                        Include CI/CD configuration
  --publish                   Publish to marketplace after generation
  --openai                    Use OpenAI provider
  --anthropic                 Use Anthropic provider
  --ollama                    Use Ollama provider
  --mock                      Use mock client for testing
```

**Examples:**
```bash
# Rust web API project
ggen ai project -d "User management API" -n user-api -l rust \
  -f axum --tests --docs --ci -o user-api/

# Python CLI application
ggen ai project -d "Data processing tool" -n data-tool -l python \
  -o data-tool/

# Full-stack web application with OpenAI
ggen ai project -d "E-commerce platform" -n shop -l typescript \
  -f nextjs --tests --docs --ci --openai -o shop/
```

### Source File Analysis

Extract templates from existing source code:

```bash
ggen ai from-source <file> [OPTIONS]

Options:
  --language <LANG>       Source language
  --output <FILE>         Output template file
  --extract-variables     Extract variables from code
  --include-rdf          Include RDF metadata
```

**Examples:**
```bash
# Extract Rust template
ggen ai from-source src/main.rs --language rust --output main.tmpl

# Extract with variable analysis
ggen ai from-source lib/utils.js --language javascript \
  --extract-variables --output utils.tmpl
```

## AI Models and Providers

### Supported Providers

| Provider | Models | Access |
|----------|--------|--------|
| **OpenAI** | GPT-4o, GPT-3.5-turbo | API key required |
| **Anthropic** | Claude 3.5 Sonnet, Claude 3 Haiku | API key required |
| **Ollama** | Qwen3-coder:30b, Llama 2, Code Llama | Local installation |
| **Gemini** | Gemini Pro, Gemini Ultra | API key required |
| **Groq** | Mixtral 8x7B, Llama 2 70B | API key required |
| **Cohere** | Command R+, Aya | API key required |

### Configuration

#### Environment Variables

```bash
# OpenAI
export OPENAI_API_KEY="your-key-here"

# Anthropic
export ANTHROPIC_API_KEY="your-key-here"

# Gemini
export GEMINI_API_KEY="your-key-here"

# Groq
export GROQ_API_KEY="your-key-here"

# Cohere
export COHERE_API_KEY="your-key-here"
```

#### Ollama Setup

```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Pull models
ollama pull qwen3-coder:30b
ollama pull llama2:7b
ollama pull codellama:7b
```

### Model Selection

```bash
# Use specific model
ggen ai generate "API controller" --model gpt-4o --output api.tmpl

# Use local Ollama model
ggen ai generate "CLI tool" --model qwen3-coder:30b --output cli.tmpl

# List available models
ggen ai models
```

## Configuration

### Global Configuration

```bash
# Set default model
ggen config set ai.model qwen3-coder:30b

# Set default temperature
ggen config set ai.temperature 0.7

# Set default max tokens
ggen config set ai.max_tokens 4096
```

### Provider-Specific Configuration

```toml
# ~/.ggen/config.toml
[ai]
default_provider = "ollama"
default_model = "qwen3-coder:30b"

[ai.ollama]
endpoint = "http://localhost:11434"

[ai.openai]
api_key_env = "OPENAI_API_KEY"
model = "gpt-4o"

[ai.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
model = "claude-3-sonnet-20240229"
```

## Advanced Usage

### Custom Prompts

```bash
# Use custom prompt templates
ggen ai generate --prompt-template custom.tmpl "API description"

# Include additional context
ggen ai generate "Database model" --context "PostgreSQL, relationships"
```

### Batch Generation

```bash
# Generate multiple templates
cat descriptions.txt | xargs -I {} ggen ai generate {} --output {}.tmpl

# Generate project components
find specs/ -name "*.md" | xargs -I {} basename {} .md | \
  xargs -I {} ggen ai generate --file specs/{}.md --output templates/{}.tmpl
```

### Integration with Templates

```yaml
# templates/complex-api.tmpl
---
to: "src/{{module_name}}.rs"
vars:
  module_name: "{{name}}"
  description: "{{description}}"
rdf:
  - "graphs/api.ttl"
sparql:
  routes: "SELECT ?route WHERE { ?api api:route ?route }"
---

use axum::{Router, routing::{{method}}};

pub fn {{module_name}}_router() -> Router {
    Router::new()
    {{#each (sparql query="routes")}}
        .route("{{this.route}}", {{this.method}}.handler)
    {{/each}}
}
```

## Best Practices

### Prompt Engineering

1. **Be specific**: Include language, framework, and requirements
2. **Provide context**: Reference existing code or patterns
3. **Use examples**: Show expected output format
4. **Iterate**: Refine prompts based on results

### Quality Assurance

1. **Validate outputs**: Use `ggen ai validate` to check templates
2. **Test generation**: Verify generated code compiles/runs
3. **Review metadata**: Check RDF graphs for semantic correctness
4. **Version control**: Track template evolution

### Performance

1. **Use appropriate models**: Smaller models for simple tasks
2. **Batch operations**: Generate multiple items together
3. **Cache results**: Reuse successful generations
4. **Monitor costs**: Track API usage for paid providers

## Troubleshooting

### Common Issues

**Empty or invalid output:**
- Check model availability and API keys
- Verify prompt clarity and specificity
- Try different temperature settings

**Compilation errors:**
- Validate generated code manually
- Use `ggen ai validate` for template checking
- Check framework compatibility

**API rate limits:**
- Implement retry logic
- Use local models when possible
- Monitor usage quotas

### Debug Mode

```bash
# Enable debug logging
export RUST_LOG=ggen_ai=debug
ggen ai generate "test" --debug

# Save prompts for analysis
ggen ai generate "test" --save-prompts
```

## Examples

### Complete Workflow

```bash
# 1. Generate ontology
ggen ai graph "User management with roles and permissions" --output user.ttl

# 2. Generate API from ontology
ggen ai generate "REST API for user management" --graph user.ttl \
  --language rust --framework axum --output api.tmpl

# 3. Generate tests
ggen ai generate "Unit tests for user API" --language rust \
  --framework tokio-test --output tests.tmpl

# 4. Generate documentation
ggen ai generate "API documentation" --language markdown \
  --output README.md.tmpl

# 5. Generate project structure
ggen ai project "User management system" --name user-system \
  --language rust --framework axum --tests --docs --output user-system/
```

This demonstrates the complete AI-powered development workflow from ontology to deployed application.
