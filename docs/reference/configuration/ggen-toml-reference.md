<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen.toml Configuration Reference](#ggentoml-configuration-reference)
  - [Prerequisites](#prerequisites)
  - [Overview](#overview)
  - [Quick Example](#quick-example)
  - [Configuration Sections](#configuration-sections)
    - [`[project]` - Project Metadata](#project---project-metadata)
    - [`[ai]` - AI Provider Configuration](#ai---ai-provider-configuration)
    - [`[templates]` - Template Configuration](#templates---template-configuration)
    - [`[rdf]` - RDF Configuration](#rdf---rdf-configuration)
    - [`[sparql]` - SPARQL Configuration](#sparql---sparql-configuration)
    - [`[lifecycle]` - Lifecycle Hooks](#lifecycle---lifecycle-hooks)
    - [`[security]` - Security Configuration](#security---security-configuration)
    - [`[performance]` - Performance Tuning](#performance---performance-tuning)
    - [`[logging]` - Logging Configuration](#logging---logging-configuration)
  - [Environment-Specific Configuration](#environment-specific-configuration)
  - [Complete Example](#complete-example)
  - [Validation](#validation)
  - [Best Practices](#best-practices)
    - [1. Use Environment Variables for Secrets](#1-use-environment-variables-for-secrets)
    - [2. Use Environment-Specific Configs](#2-use-environment-specific-configs)
    - [3. Set Reasonable Limits](#3-set-reasonable-limits)
    - [4. Use Prefixes for Readability](#4-use-prefixes-for-readability)
  - [Troubleshooting](#troubleshooting)
    - [Configuration Not Found](#configuration-not-found)
    - [Invalid TOML Syntax](#invalid-toml-syntax)
    - [Environment Override Not Working](#environment-override-not-working)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen.toml Configuration Reference

Complete reference for ggen project configuration files.

## Prerequisites

**Required**:
- ggen v1.0.0 or higher installed
- Basic TOML syntax knowledge ([learn TOML](https://toml.io))
- Rust project or multi-language codebase

**Helpful**:
- Understanding of RDF/SPARQL concepts
- Familiarity with template-driven code generation
- Experience with environment-specific configurations

**File Path Conventions**:
- All paths in this document are **relative to project root** (where ggen.toml is located)
- Template paths: `./templates/` by default
- Output paths: `./generated/` by default
- RDF store: `./.ggen/rdf-store/` by default

## Overview

`ggen.toml` is the project-level configuration file that controls ggen's behavior. Place it in your project root directory.

**Location**: `./ggen.toml` (project root)

**Format**: TOML (Tom's Obvious, Minimal Language)

## Quick Example

```toml
[project]
name = "my-app"
version = "1.0.0"
description = "E-commerce ontology project"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true

[rdf]
base_uri = "https://example.com/ecommerce/"
default_format = "turtle"

[sparql]
timeout = 30
max_results = 1000
```

---

## Configuration Sections

### `[project]` - Project Metadata

Project identification and metadata.

**Fields**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | String | Yes | Project name |
| `version` | String | Yes | Semantic version (e.g., "1.0.0") |
| `description` | String | No | Project description |
| `authors` | Array[String] | No | Author names/emails |
| `license` | String | No | License identifier (e.g., "MIT") |
| `repository` | String | No | Git repository URL |

**Example**:

```toml
[project]
name = "ecommerce-ontology"
version = "2.1.0"
description = "Product catalog ontology with Schema.org integration"
authors = ["Jane Developer <jane@example.com>"]
license = "MIT"
repository = "https://github.com/example/ecommerce-ontology"
```

---

### `[ai]` - AI Provider Configuration

Configure AI providers for code generation and assistance.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `provider` | String | No | - | Provider: "openai", "anthropic", "ollama" |
| `model` | String | No | - | Model name (e.g., "gpt-4", "claude-3") |
| `api_key` | String | No | `$OPENAI_API_KEY` | API key (prefer env var) |
| `base_url` | String | No | - | Custom API endpoint |
| `temperature` | Float | No | 0.7 | Sampling temperature (0.0-2.0) |
| `max_tokens` | Integer | No | 2000 | Maximum response tokens |
| `timeout` | Integer | No | 60 | Request timeout (seconds) |

**Example - OpenAI**:

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 4000
timeout = 90
# API key from environment: $OPENAI_API_KEY
```

**Example - Anthropic**:

```toml
[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"
temperature = 0.5
max_tokens = 8000
# API key from environment: $ANTHROPIC_API_KEY
```

**Example - Ollama (Local)**:

```toml
[ai]
provider = "ollama"
model = "llama2"
base_url = "http://localhost:11434"
temperature = 0.8
```

**Security Note**: NEVER commit API keys to version control. Use environment variables:

```bash
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
```

---

### `[templates]` - Template Configuration

Configure template directories and generation behavior.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `directory` | String | No | "templates" | Template source directory |
| `output_directory` | String | No | "generated" | Output directory for generated code |
| `backup_enabled` | Boolean | No | true | Backup existing files before overwrite |
| `idempotent` | Boolean | No | true | Skip regeneration if output unchanged |
| `cache_directory` | String | No | ".ggen/cache" | Template cache directory |

**Example**:

```toml
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true
cache_directory = ".ggen/cache"
```

**Directory Structure**:

```
project/
├── ggen.toml
├── templates/           # Template source
│   ├── javascript/
│   └── rust/
└── src/generated/       # Generated output
    ├── models.js
    └── schemas.rs
```

---

### `[rdf]` - RDF Configuration

Configure RDF graph behavior and prefixes.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `base_uri` | String | No | - | Default base URI for RDF |
| `prefixes` | Table | No | {} | Namespace prefix mappings |
| `default_format` | String | No | "turtle" | Default RDF format |
| `cache_queries` | Boolean | No | true | Cache SPARQL query results |
| `store_path` | String | No | ".ggen/store" | RDF store persistence path |

**Supported Formats**: `turtle`, `rdf-xml`, `n-triples`, `n-quads`, `trig`

**Example**:

```toml
[rdf]
base_uri = "https://example.com/ecommerce/"
default_format = "turtle"
cache_queries = true
store_path = ".ggen/rdf-store"

[rdf.prefixes]
ex = "https://example.com/ecommerce/"
schema = "https://schema.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
owl = "http://www.w3.org/2002/07/owl#"
xsd = "http://www.w3.org/2001/XMLSchema#"
```

**Usage with SPARQL**:

```sparql
PREFIX ex: <https://example.com/ecommerce/>
PREFIX schema: <https://schema.org/>

SELECT ?product ?name
WHERE {
  ?product a ex:Product ;
           schema:name ?name .
}
```

---

### `[sparql]` - SPARQL Configuration

Configure SPARQL query execution.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `timeout` | Integer | No | 30 | Query timeout (seconds) |
| `max_results` | Integer | No | 1000 | Maximum result rows |
| `cache_enabled` | Boolean | No | true | Enable query result caching |
| `cache_ttl` | Integer | No | 3600 | Cache time-to-live (seconds) |

**Example**:

```toml
[sparql]
timeout = 60
max_results = 5000
cache_enabled = true
cache_ttl = 7200  # 2 hours
```

---

### `[lifecycle]` - Lifecycle Hooks

Configure project lifecycle hooks and automation.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `enabled` | Boolean | No | false | Enable lifecycle hooks |
| `config_file` | String | No | ".ggen/lifecycle.toml" | Lifecycle configuration |
| `cache_directory` | String | No | ".ggen/cache" | Cache directory |
| `state_file` | String | No | ".ggen/state.json" | State persistence file |

**Phases**: `pre_generate`, `post_generate`, `pre_load`, `post_load`

**Example**:

```toml
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases.pre_generate]
scripts = ["scripts/validate-ontology.sh"]

[lifecycle.phases.post_generate]
scripts = ["scripts/format-code.sh", "scripts/run-tests.sh"]
```

---

### `[security]` - Security Configuration

Configure security policies and validation.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `allowed_domains` | Array[String] | No | [] | Allowed URI domains for RDF loading |
| `deny_list` | Array[String] | No | [] | Blocked URIs or patterns |
| `max_file_size` | Integer | No | 104857600 | Max RDF file size (bytes, default 100MB) |
| `validate_ssl` | Boolean | No | true | Validate SSL certificates |

**Example**:

```toml
[security]
allowed_domains = ["schema.org", "dbpedia.org", "example.com"]
deny_list = ["file:///", "javascript:"]
max_file_size = 52428800  # 50MB
validate_ssl = true
```

---

### `[performance]` - Performance Tuning

Configure performance optimizations.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `parallel_generation` | Boolean | No | true | Generate templates in parallel |
| `max_workers` | Integer | No | - | Max parallel workers (default: CPU cores) |
| `cache_templates` | Boolean | No | true | Cache compiled templates |
| `incremental_build` | Boolean | No | true | Only regenerate changed templates |

**Example**:

```toml
[performance]
parallel_generation = true
max_workers = 8
cache_templates = true
incremental_build = true
```

---

### `[logging]` - Logging Configuration

Configure logging behavior.

**Fields**:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `level` | String | No | "info" | Log level: "trace", "debug", "info", "warn", "error" |
| `format` | String | No | "pretty" | Format: "json", "pretty", "compact" |
| `output` | String | No | "stderr" | Output: "stdout", "stderr", file path |

**Example**:

```toml
[logging]
level = "debug"
format = "pretty"
output = "stderr"
```

---

## Environment-Specific Configuration

Override settings for different environments.

**Syntax**: `[env.<ENVIRONMENT_NAME>]`

**Example**:

```toml
[project]
name = "my-app"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

# Development overrides
[env.development]
"ai.model" = "gpt-3.5-turbo"  # Cheaper model for dev
"ai.temperature" = 0.9        # More creative for experimentation
"logging.level" = "debug"     # Verbose logging

# Production overrides
[env.production]
"ai.temperature" = 0.3        # Deterministic outputs
"logging.level" = "warn"      # Quiet logging
"performance.max_workers" = 16
```

**Activate Environment**:

```bash
# Set environment
export GGEN_ENV=development
ggen ontology generate schema.json

# Or inline
GGEN_ENV=production ggen ontology generate schema.json
```

---

## Complete Example

```toml
# Project metadata
[project]
name = "ecommerce-schema"
version = "2.0.0"
description = "E-commerce product catalog with Schema.org integration"
authors = ["Dev Team <dev@example.com>"]
license = "MIT"

# AI configuration
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 4000
timeout = 90

# Template configuration
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true

# RDF configuration
[rdf]
base_uri = "https://example.com/ecommerce/"
default_format = "turtle"
cache_queries = true

[rdf.prefixes]
ex = "https://example.com/ecommerce/"
schema = "https://schema.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

# SPARQL configuration
[sparql]
timeout = 60
max_results = 5000
cache_enabled = true

# Security
[security]
allowed_domains = ["schema.org", "dbpedia.org"]
max_file_size = 52428800

# Performance
[performance]
parallel_generation = true
max_workers = 8

# Logging
[logging]
level = "info"
format = "pretty"

# Development environment
[env.development]
"ai.model" = "gpt-3.5-turbo"
"logging.level" = "debug"

# Production environment
[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
```

---

## Validation

Validate your configuration:

```bash
# Check configuration is valid
ggen config validate

# Show merged configuration (with environment overrides)
ggen config show

# Show specific section
ggen config show ai
```

---

## Best Practices

### 1. Use Environment Variables for Secrets

❌ **BAD**:
```toml
[ai]
api_key = "sk-1234567890abcdef"  # Committed to Git!
```

✅ **GOOD**:
```toml
[ai]
provider = "openai"
# API key from $OPENAI_API_KEY environment variable
```

### 2. Use Environment-Specific Configs

```toml
# Base configuration
[ai]
model = "gpt-4"

# Cheaper model for development
[env.development]
"ai.model" = "gpt-3.5-turbo"
```

### 3. Set Reasonable Limits

```toml
[security]
max_file_size = 52428800  # 50MB prevents accidents

[sparql]
timeout = 60              # Prevent runaway queries
max_results = 5000        # Limit result size
```

### 4. Use Prefixes for Readability

```toml
[rdf.prefixes]
ex = "https://example.com/very/long/namespace/uri/"
schema = "https://schema.org/"

# Now in SPARQL:
# PREFIX ex: <https://example.com/very/long/namespace/uri/>
# becomes just: ex:Product
```

---

## Troubleshooting

### Configuration Not Found

```
Error: Configuration file not found: ggen.toml
```

**Solution**: Create `ggen.toml` in project root or use `ggen init` to scaffold a project.

### Invalid TOML Syntax

```
Error: TOML parse error at line 15: expected '=', found ':'
```

**Solution**: Check TOML syntax. Common issues:
- Use `=` not `:` for assignment
- Quote strings: `name = "value"`
- Use `[section]` for tables

### Environment Override Not Working

**Check**:
1. Environment variable is set: `echo $GGEN_ENV`
2. Section name matches: `[env.development]` not `[env.dev]`
3. Path uses dots: `"ai.model"` not `"[ai].model"`

---

## Related Documentation

- **[gpack.toml Package Format](gpack-toml-reference.md)** - Template package metadata
- **[Common TOML Configurations](../how-to/configuration/common-toml-configs.md)** - Example configurations
- **[CLI Reference](../commands/complete-cli-reference.md)** - Command-line options

---

**Last Updated**: 2025-12-10
