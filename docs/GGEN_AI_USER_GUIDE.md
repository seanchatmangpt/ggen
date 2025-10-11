# ggen-ai User Guide

## Overview

ggen-ai provides AI-powered code generation capabilities for ggen, enabling intelligent template generation, SPARQL query construction, and ontology creation using large language models.

## Architecture

### Noun-Verb Command Structure

Unlike traditional CLI tools, ggen uses a **noun-verb** command structure:

```
ggen <noun> <verb> [options]
```

### AI Integration

AI functionality is integrated into existing commands rather than being a separate top-level command:

- **Template Generation**: `ggen project gen --ai`
- **Template Validation**: `ggen project validate`
- **AI Models**: `ggen ai models`

## Quick Start

### 1. Prerequisites

**Ollama Setup:**
```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Start Ollama service
ollama serve

# Pull the qwen3-coder:30b model
ollama pull qwen3-coder:30b
```

**Verify Installation:**
```bash
# Check Ollama status
curl http://localhost:11434/api/version

# List available models
ollama list
```

### 2. Basic Usage

**Generate Template with AI:**
```bash
# Basic AI generation
ggen project gen "template.tmpl" --ai --ai-provider ollama

# With custom model
ggen project gen "template.tmpl" --ai --ai-provider ollama --ai-model qwen3-coder:30b

# With validation
ggen project gen "template.tmpl" --ai --validate --max-iterations 3
```

**Validate Templates:**
```bash
# Basic validation
ggen project validate template.tmpl

# Strict validation
ggen project validate template.tmpl --strict
```

**List AI Models:**
```bash
# All providers
ggen ai models

# Specific provider
ggen ai models --adapter ollama
```

### 3. Environment Configuration

**Ollama (Local):**
```bash
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
```

**OpenAI:**
```bash
export OPENAI_API_KEY=your-api-key
export OPENAI_BASE_URL=https://api.openai.com/v1
```

**Anthropic:**
```bash
export ANTHROPIC_API_KEY=your-api-key
export ANTHROPIC_BASE_URL=https://api.anthropic.com
```

## Command Reference

### `ggen project gen --ai`

AI-powered template generation with iterative validation.

**Options:**
- `--ai`: Enable AI-powered generation
- `--ai-provider <provider>`: AI provider (ollama, openai, anthropic) [default: ollama]
- `--ai-model <model>`: Specific model to use
- `--validate`: Enable iterative validation and improvement
- `--max-iterations <n>`: Maximum validation iterations [default: 3]

**Examples:**
```bash
# Simple AI generation
ggen project gen "api-template.tmpl" --ai

# With validation
ggen project gen "api-template.tmpl" --ai --validate

# With custom model and variables
ggen project gen "api-template.tmpl" --ai --ai-model qwen3-coder:30b --var name=MyAPI --var version=1.0.0
```

### `ggen project validate`

Template validation and quality assessment.

**Options:**
- `--strict`: Enable strict validation mode
- `--quality-threshold <n>`: Minimum quality score (0.0-1.0) [default: 0.7]

**Examples:**
```bash
# Basic validation
ggen project validate template.tmpl

# Strict validation
ggen project validate template.tmpl --strict

# Custom quality threshold
ggen project validate template.tmpl --quality-threshold 0.9
```

### `ggen ai models`

List available AI models across providers.

**Options:**
- `--adapter <provider>`: Filter by specific provider

**Examples:**
```bash
# All models
ggen ai models

# Ollama models only
ggen ai models --adapter ollama
```

## Validation & Quality Assessment

### Quality Metrics

ggen-ai evaluates templates across 6 dimensions:

1. **Completeness** (0.0-1.0): Template structure and required fields
2. **Correctness** (0.0-1.0): Syntax validity and semantic correctness
3. **Maintainability** (0.0-1.0): Code structure and documentation
4. **Performance** (0.0-1.0): Efficient patterns and anti-patterns
5. **Security** (0.0-1.0): Security vulnerability detection
6. **Readability** (0.0-1.0): Code clarity and documentation

### Validation Issues

Templates are checked for:
- YAML syntax errors
- Template engine compatibility
- RDF/SPARQL syntax validation
- Security vulnerabilities
- Performance anti-patterns
- Best practices compliance

## MCP Server Integration

### VS Code/Cursor Integration

**Setup:**
```bash
# Start MCP server with Ollama
USE_OLLAMA=true OLLAMA_MODEL=qwen3-coder:30b cargo run --bin ggen-ai-mcp
```

**VS Code Configuration:**
```json
{
  "mcp": {
    "servers": {
      "ggen-ai": {
        "command": "cargo",
        "args": ["run", "--bin", "ggen-ai-mcp"],
        "env": {
          "USE_OLLAMA": "true",
          "OLLAMA_MODEL": "qwen3-coder:30b"
        }
      }
    }
  }
}
```

## Advanced Configuration

### Custom Model Configuration

**Ollama:**
```bash
export OLLAMA_MODEL=qwen3-coder:30b
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_TIMEOUT=60
```

**OpenAI:**
```bash
export OPENAI_API_KEY=sk-your-key
export OPENAI_MODEL=gpt-4
export OPENAI_BASE_URL=https://api.openai.com/v1
```

### Quality Thresholds

**Environment Variables:**
```bash
export GGEN_AI_QUALITY_THRESHOLD=0.8
export GGEN_AI_MAX_ITERATIONS=5
export GGEN_AI_STRICT_MODE=true
```

## Troubleshooting

### Common Issues

**Ollama Not Running:**
```bash
# Start Ollama service
ollama serve

# Check status
curl http://localhost:11434/api/version
```

**Model Not Available:**
```bash
# Pull required model
ollama pull qwen3-coder:30b

# List available models
ollama list
```

**API Key Issues:**
```bash
# Verify API keys are set
echo $OPENAI_API_KEY
echo $ANTHROPIC_API_KEY

# Test connectivity
curl -H "Authorization: Bearer $OPENAI_API_KEY" https://api.openai.com/v1/models
```

## Examples

### Complete Workflow

```bash
# 1. Generate initial template
ggen project gen "api-template.tmpl" --ai --ai-provider ollama --var name=MyAPI

# 2. Validate and improve
ggen project validate MyAPI.rs

# 3. Use in project
ggen project gen "MyAPI.rs" --var name=MyApp --var port=8080
```

### AI-Enhanced Development

```bash
# Generate with multiple iterations for high quality
ggen project gen "complex-template.tmpl" --ai --validate --max-iterations 5

# Custom validation thresholds
ggen project validate template.tmpl --quality-threshold 0.9

# List available models
ggen ai models --adapter ollama
```

## Migration from Solo AI Command

**Old (Deprecated):**
```bash
ggen ai generate --description "API module"
ggen ai validate template.tmpl
```

**New (Recommended):**
```bash
ggen project gen "template.tmpl" --ai --description "API module"
ggen project validate template.tmpl
```

The new structure integrates AI capabilities into the established noun-verb command pattern, providing a more consistent and extensible interface.
