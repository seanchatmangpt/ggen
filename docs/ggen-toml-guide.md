<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen.toml User Guide](#ggentoml-user-guide)
  - [Introduction](#introduction)
  - [Quick Start](#quick-start)
    - [Creating Your First ggen.toml](#creating-your-first-ggentoml)
    - [Generate Code](#generate-code)
  - [Configuration Sections](#configuration-sections)
    - [1. Project Metadata](#1-project-metadata)
    - [2. Template Configuration](#2-template-configuration)
    - [3. AI Configuration](#3-ai-configuration)
    - [4. RDF and SPARQL Configuration](#4-rdf-and-sparql-configuration)
    - [5. Marketplace Integration](#5-marketplace-integration)
    - [6. Lifecycle Management](#6-lifecycle-management)
    - [7. Template Type-Specific Configuration](#7-template-type-specific-configuration)
    - [8. Security Configuration](#8-security-configuration)
    - [9. Performance Configuration](#9-performance-configuration)
    - [10. Logging Configuration](#10-logging-configuration)
    - [11. Feature Flags](#11-feature-flags)
  - [Common Workflows](#common-workflows)
    - [1. Simple Project Generation](#1-simple-project-generation)
    - [2. AI-Powered Microservice](#2-ai-powered-microservice)
    - [3. Knowledge Graph Code Generation](#3-knowledge-graph-code-generation)
    - [4. Marketplace Package Development](#4-marketplace-package-development)
  - [Environment Variables](#environment-variables)
  - [Best Practices](#best-practices)
    - [1. Version Control](#1-version-control)
    - [2. Security](#2-security)
    - [3. Performance](#3-performance)
    - [4. Organization](#4-organization)
    - [5. Testing](#5-testing)
  - [Migration from Other Tools](#migration-from-other-tools)
    - [From Cargo.toml](#from-cargotoml)
    - [From pyproject.toml](#from-pyprojecttoml)
  - [Troubleshooting](#troubleshooting)
    - [Configuration Not Found](#configuration-not-found)
    - [Invalid TOML Syntax](#invalid-toml-syntax)
    - [AI Provider Errors](#ai-provider-errors)
  - [Advanced Topics](#advanced-topics)
    - [Custom Template Engines](#custom-template-engines)
    - [Multi-Environment Configuration](#multi-environment-configuration)
    - [Configuration Inheritance](#configuration-inheritance)
  - [Reference](#reference)
  - [Getting Help](#getting-help)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen.toml User Guide

## Introduction

`ggen.toml` is the project configuration format for ggen, providing a declarative way to configure code generation, RDF/SPARQL integration, AI-powered features, marketplace packages, and lifecycle management. Think of it as combining the best aspects of `Cargo.toml`, `pyproject.toml`, and `package.json` tailored for knowledge-driven code generation.

## Quick Start

### Creating Your First ggen.toml

Create a `ggen.toml` file in your project root:

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "My knowledge-driven project"
author = "Your Name"

[templates]
source_dir = "templates"
output_dir = "generated"

[logging]
level = "info"
format = "json"
```

### Generate Code

```bash
# Initialize project structure
ggen project init

# Generate code from templates
ggen project generate

# Watch for changes and auto-regenerate
ggen project watch
```

## Configuration Sections

### 1. Project Metadata

Define your project's core information:

```toml
[project]
name = "comprehensive-rust-showcase"
version = "1.0.0"
description = "Comprehensive Rust project showcasing all ggen features"
author = "ggen-examples"
license = "MIT"
repository = "https://github.com/example/my-project"
```

**Fields:**
- `name` (required): Project identifier
- `version` (required): Semantic version (e.g., "1.0.0")
- `description` (required): Brief project description
- `author`: Project author
- `license`: License type (MIT, Apache-2.0, etc.)
- `repository`: Git repository URL

### 2. Template Configuration

Control how templates are discovered and rendered:

```toml
[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true
```

**Fields:**
- `source_dir`: Directory containing `.tmpl` template files
- `output_dir`: Where generated code is written
- `backup_enabled`: Create backups before overwriting
- `idempotent`: Ensure repeated generation produces same output

**Advanced patterns:**
```toml
[templates]
patterns = ["templates/**/*.tmpl", "custom/**/*.tera"]
includes = ["macros/**/*.tera"]
```

### 3. AI Configuration

Configure AI-powered code generation:

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout_seconds = 30
retry_attempts = 3
stream_enabled = true
```

**Supported providers:**
- `openai`: GPT-4, GPT-3.5
- `anthropic`: Claude 3.5 Sonnet, Claude 3 Opus
- `ollama`: Local models (qwen2.5-coder, codellama, etc.)

**Provider-specific configuration:**
```toml
[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"
organization_env = "OPENAI_ORG_ID"
model = "gpt-4"

[ai.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
model = "claude-3-5-sonnet-20241022"

[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen2.5-coder:7b"
```

**AI caching:**
```toml
[ai.cache]
enabled = true
ttl_seconds = 3600
max_entries = 1000
strategy = "lru"
```

**AI prompts:**
```toml
[ai.prompts]
system = "You are an expert Rust developer..."
user_prefix = "Generate a service with the following requirements:"
```

**AI validation:**
```toml
[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3

[ai.generation]
auto_improve = true
include_tests = true
include_docs = true
include_examples = true
```

### 4. RDF and SPARQL Configuration

Integrate semantic knowledge graphs:

```toml
[rdf]
base_uri = "https://example.com/my-project/"
prefixes = {
    ex = "https://example.com/my-project/",
    schema = "http://schema.org/",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
}

[graph]
base_iri = "http://example.org/my-project/"
cache_queries = true
query_timeout_seconds = 30

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true
```

### 5. Marketplace Integration

Configure package marketplace access:

```toml
[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"
offline_mode = false
```

### 6. Lifecycle Management

Define build phases and automation:

```toml
[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases]
default = ["init", "setup", "generate", "validate", "test"]
development = ["init", "setup", "generate", "validate", "test", "dev"]
production = ["generate", "validate", "test", "build", "deploy"]
```

### 7. Template Type-Specific Configuration

Configure code generation for specific tech stacks:

**Rust configuration:**
```toml
[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"
documentation = "comprehensive"
```

**Web framework configuration:**
```toml
[templates.web]
framework = "auto-detect"  # axum, actix-web, warp
database = "postgresql"
cache = "redis"
monitoring = "prometheus"
documentation = "openapi"
```

**Database configuration:**
```toml
[templates.database]
provider = "postgresql"
migrations = true
connection_pooling = true
transactions = true
backup = true
```

**API configuration:**
```toml
[templates.api]
versioning = "v1"
authentication = "jwt"
rate_limiting = true
cors = true
validation = true
documentation = "openapi"
```

### 8. Security Configuration

Control security features:

```toml
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
audit_operations = true
require_confirmation = false
```

### 9. Performance Configuration

Optimize execution:

```toml
[performance]
parallel_execution = true
max_workers = 16
cache_size = "1GB"
enable_profiling = true
memory_limit_mb = 1024
```

### 10. Logging Configuration

Configure logging output:

```toml
[logging]
level = "info"  # trace, debug, info, warn, error
format = "json"  # json, text, pretty
output = "file"  # file, stdout, stderr
file_path = ".ggen/logs/ggen.log"
rotation = "daily"  # hourly, daily, weekly
```

### 11. Feature Flags

Enable/disable features:

```toml
[features]
ai_generation = true
sparql_queries = true
lifecycle_management = true
template_validation = true
graph_analysis = true
documentation_generation = true
code_validation = true
test_generation = true
```

## Common Workflows

### 1. Simple Project Generation

**ggen.toml:**
```toml
[project]
name = "hello-world"
version = "0.1.0"
description = "Simple Hello World project"

[templates]
source_dir = "templates"
output_dir = "src"

[logging]
level = "info"
```

**Usage:**
```bash
ggen project init
ggen project generate
```

### 2. AI-Powered Microservice

**ggen.toml:**
```toml
[project]
name = "ai-microservice"
version = "0.1.0"

[ai]
provider = "ollama"
model = "qwen2.5-coder"
temperature = 0.7

[ai.prompts]
system = "You are an expert Rust developer..."

[templates.web]
framework = "axum"
database = "postgresql"

[templates.api]
authentication = "jwt"
documentation = "openapi"
```

### 3. Knowledge Graph Code Generation

**ggen.toml:**
```toml
[project]
name = "ontology-driven"
version = "1.0.0"

[rdf]
base_uri = "https://example.com/ontology/"
prefixes = { ex = "https://example.com/ontology/", schema = "http://schema.org/" }

[sparql]
timeout = 10
cache_enabled = true

[templates]
source_dir = "templates"
output_dir = "generated"
```

### 4. Marketplace Package Development

**ggen.toml:**
```toml
[project]
name = "my-marketplace-package"
version = "1.0.0"

[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"

[lifecycle]
enabled = true
config_file = "make.toml"

[lifecycle.phases]
publish = ["validate", "test", "build", "package", "upload"]
```

## Environment Variables

ggen.toml supports environment variable substitution:

```toml
[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"  # Reads from $OPENAI_API_KEY

[ai.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"  # Reads from $ANTHROPIC_API_KEY
```

## Best Practices

### 1. Version Control
- **Commit** `ggen.toml` to version control
- **Ignore** `.ggen/` directory (caches, state)
- **Document** custom configurations in README

### 2. Security
- Never hardcode API keys or secrets
- Use `api_key_env` to reference environment variables
- Enable `path_traversal_protection` and `shell_injection_protection`

### 3. Performance
- Enable caching for AI and SPARQL queries
- Use `parallel_execution` for large codebases
- Set appropriate `memory_limit_mb` for your system

### 4. Organization
- Group related templates in subdirectories
- Use consistent naming for template files
- Document template variables in README

### 5. Testing
- Use `dry_run` mode to preview generation
- Enable `backup_enabled` before overwriting files
- Test with `lifecycle.phases.development` before production

## Migration from Other Tools

### From Cargo.toml

**Cargo.toml:**
```toml
[package]
name = "my-crate"
version = "0.1.0"
authors = ["Me"]
```

**ggen.toml equivalent:**
```toml
[project]
name = "my-crate"
version = "0.1.0"
author = "Me"

[templates]
source_dir = "templates"
output_dir = "src"
```

### From pyproject.toml

**pyproject.toml:**
```toml
[project]
name = "my-package"
version = "0.1.0"
```

**ggen.toml equivalent:**
```toml
[project]
name = "my-package"
version = "0.1.0"

[templates]
source_dir = "templates"
output_dir = "src"
```

## Troubleshooting

### Configuration Not Found
```bash
# Ensure ggen.toml is in project root
ls -la ggen.toml

# Or use --config flag
ggen project generate --config ./path/to/ggen.toml
```

### Invalid TOML Syntax
```bash
# Validate TOML syntax
toml check ggen.toml

# Common issues:
# - Missing quotes around strings
# - Invalid key names (use underscores, not hyphens)
# - Unclosed brackets
```

### AI Provider Errors
```bash
# Check API key is set
echo $OPENAI_API_KEY

# Test connection
ggen ai test --provider openai
```

## Advanced Topics

### Custom Template Engines

ggen supports both Tera and custom template engines:

```toml
[templates]
engine = "tera"  # or "handlebars", "minijinja"
extensions = [".tmpl", ".tera", ".hbs"]
```

### Multi-Environment Configuration

Use separate configs for different environments:

```bash
# Development
ggen project generate --config ggen.dev.toml

# Production
ggen project generate --config ggen.prod.toml
```

### Configuration Inheritance

Override base configuration:

**ggen.base.toml:**
```toml
[project]
name = "base-project"

[logging]
level = "info"
```

**ggen.toml:**
```toml
# Inherit from base
include = "ggen.base.toml"

[project]
name = "my-project"  # Override

[logging]
level = "debug"  # Override
```

## Reference

For complete configuration reference, see:
- [ggen.toml Reference](./ggen-toml-reference.md)
- [Migration Guide](./ggen-toml-migration.md)
- [Examples](../examples/)

## Getting Help

- **Documentation**: https://github.com/seanchatmangpt/ggen/docs
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discord**: https://discord.gg/ggen (example)

---

**Next Steps:**
1. Create your first `ggen.toml`
2. Run `ggen project init`
3. Add templates to `templates/`
4. Run `ggen project generate`
5. Explore advanced features (AI, RDF, lifecycle)
