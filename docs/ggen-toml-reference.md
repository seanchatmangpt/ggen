# ggen.toml Complete Reference

## Overview

This document provides a complete reference for all configuration options available in `ggen.toml`. Each section includes field descriptions, types, default values, and examples.

## Table of Contents

1. [Project Metadata](#project-metadata)
2. [Templates](#templates)
3. [AI Configuration](#ai-configuration)
4. [RDF and SPARQL](#rdf-and-sparql)
5. [Marketplace](#marketplace)
6. [Lifecycle](#lifecycle)
7. [Security](#security)
8. [Performance](#performance)
9. [Logging](#logging)
10. [Features](#features)

---

## Project Metadata

Defines core project information.

### Section: `[project]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | String | Yes | - | Project identifier (alphanumeric, underscores, hyphens) |
| `version` | String | Yes | - | Semantic version (e.g., "1.0.0") |
| `description` | String | Yes | - | Brief project description |
| `author` | String | No | "" | Project author or organization |
| `authors` | Array[String] | No | [] | List of authors |
| `license` | String | No | "MIT" | License identifier (SPDX format) |
| `repository` | String | No | "" | Git repository URL |
| `homepage` | String | No | "" | Project homepage URL |
| `documentation` | String | No | "" | Documentation URL |
| `keywords` | Array[String] | No | [] | Search keywords |
| `categories` | Array[String] | No | [] | Project categories |

### Example

```toml
[project]
name = "my-awesome-project"
version = "2.1.0"
description = "An awesome code generation project using knowledge graphs"
author = "Jane Developer <jane@example.com>"
authors = ["Jane Developer", "John Contributor"]
license = "MIT OR Apache-2.0"
repository = "https://github.com/example/my-project"
homepage = "https://my-project.example.com"
documentation = "https://docs.my-project.example.com"
keywords = ["code-generation", "rdf", "sparql", "templates"]
categories = ["development-tools", "command-line-utilities"]
```

---

## Templates

Controls template discovery, rendering, and output.

### Section: `[templates]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `source_dir` | String | No | "templates" | Directory containing template files |
| `output_dir` | String | No | "generated" | Output directory for generated code |
| `backup_enabled` | Boolean | No | false | Create backups before overwriting |
| `idempotent` | Boolean | No | true | Ensure repeated generation is deterministic |
| `patterns` | Array[String] | No | ["templates/**/*.tmpl"] | Glob patterns for template discovery |
| `includes` | Array[String] | No | [] | Additional Tera includes |
| `engine` | String | No | "tera" | Template engine (tera, handlebars, minijinja) |
| `extensions` | Array[String] | No | [".tmpl", ".tera"] | Template file extensions |

### Example

```toml
[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true
patterns = ["templates/**/*.tmpl", "custom/**/*.tera"]
includes = ["macros/**/*.tera"]
engine = "tera"
extensions = [".tmpl", ".tera", ".hbs"]
```

### Tech Stack-Specific Templates

#### Rust Templates: `[templates.rust]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `style` | String | "core-team" | Code style (core-team, idiomatic, custom) |
| `error_handling` | String | "thiserror" | Error handling (thiserror, anyhow, std) |
| `logging` | String | "tracing" | Logging framework (tracing, log, env_logger) |
| `async_runtime` | String | "tokio" | Async runtime (tokio, async-std, smol) |
| `testing` | String | "comprehensive" | Testing strategy (minimal, standard, comprehensive) |
| `documentation` | String | "comprehensive" | Documentation level (minimal, standard, comprehensive) |

```toml
[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"
documentation = "comprehensive"
```

#### Web Framework Templates: `[templates.web]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `framework` | String | "auto-detect" | Web framework (axum, actix-web, warp, rocket) |
| `database` | String | "postgresql" | Database (postgresql, mysql, sqlite, mongodb) |
| `cache` | String | "redis" | Caching layer (redis, memcached, none) |
| `monitoring` | String | "prometheus" | Monitoring (prometheus, grafana, datadog) |
| `documentation` | String | "openapi" | API docs (openapi, swagger, none) |

```toml
[templates.web]
framework = "axum"
database = "postgresql"
cache = "redis"
monitoring = "prometheus"
documentation = "openapi"
```

#### Database Templates: `[templates.database]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `provider` | String | "postgresql" | Database provider |
| `migrations` | Boolean | true | Enable migrations |
| `connection_pooling` | Boolean | true | Enable connection pooling |
| `transactions` | Boolean | true | Enable transactions |
| `backup` | Boolean | true | Enable backups |

```toml
[templates.database]
provider = "postgresql"
migrations = true
connection_pooling = true
transactions = true
backup = true
```

#### API Templates: `[templates.api]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `versioning` | String | "v1" | API versioning scheme |
| `authentication` | String | "jwt" | Auth method (jwt, oauth2, api-key) |
| `rate_limiting` | Boolean | true | Enable rate limiting |
| `cors` | Boolean | true | Enable CORS |
| `validation` | Boolean | true | Enable request validation |
| `documentation` | String | "openapi" | API documentation format |

```toml
[templates.api]
versioning = "v1"
authentication = "jwt"
rate_limiting = true
cors = true
validation = true
documentation = "openapi"
```

---

## AI Configuration

Configure AI-powered code generation features.

### Section: `[ai]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `provider` | String | No | "openai" | AI provider (openai, anthropic, ollama) |
| `model` | String | No | "gpt-4" | Model identifier |
| `temperature` | Float | No | 0.7 | Sampling temperature (0.0-2.0) |
| `max_tokens` | Integer | No | 2000 | Maximum tokens in response |
| `timeout_seconds` | Integer | No | 30 | Request timeout in seconds |
| `retry_attempts` | Integer | No | 3 | Number of retry attempts |
| `stream_enabled` | Boolean | No | false | Enable streaming responses |

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

### Provider-Specific Configuration

#### OpenAI: `[ai.providers.openai]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `api_key_env` | String | "OPENAI_API_KEY" | Environment variable for API key |
| `organization_env` | String | "OPENAI_ORG_ID" | Environment variable for org ID |
| `model` | String | "gpt-4" | Default model |
| `base_url` | String | "https://api.openai.com/v1" | API base URL |

```toml
[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"
organization_env = "OPENAI_ORG_ID"
model = "gpt-4"
```

#### Anthropic: `[ai.providers.anthropic]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `api_key_env` | String | "ANTHROPIC_API_KEY" | Environment variable for API key |
| `model` | String | "claude-3-5-sonnet-20241022" | Default model |
| `base_url` | String | "https://api.anthropic.com" | API base URL |

```toml
[ai.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
model = "claude-3-5-sonnet-20241022"
```

#### Ollama: `[ai.providers.ollama]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `base_url` | String | "http://localhost:11434" | Ollama server URL |
| `model` | String | "qwen2.5-coder:7b" | Local model name |

```toml
[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen2.5-coder:7b"
```

### AI Cache: `[ai.cache]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | Boolean | false | Enable response caching |
| `ttl_seconds` | Integer | 3600 | Cache entry TTL |
| `max_entries` | Integer | 1000 | Maximum cache entries |
| `strategy` | String | "lru" | Eviction strategy (lru, lfu, fifo) |

```toml
[ai.cache]
enabled = true
ttl_seconds = 3600
max_entries = 1000
strategy = "lru"
```

### AI Prompts: `[ai.prompts]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `system` | String | "" | System prompt template |
| `user_prefix` | String | "" | User message prefix |
| `assistant_prefix` | String | "" | Assistant response prefix |

```toml
[ai.prompts]
system = "You are an expert Rust developer specializing in web services."
user_prefix = "Generate code for:"
```

### AI Validation: `[ai.validation]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | Boolean | false | Enable output validation |
| `quality_threshold` | Float | 0.8 | Minimum quality score (0.0-1.0) |
| `max_iterations` | Integer | 3 | Maximum validation iterations |

```toml
[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3
```

### AI Generation: `[ai.generation]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `auto_improve` | Boolean | false | Automatically improve generated code |
| `include_tests` | Boolean | false | Generate tests |
| `include_docs` | Boolean | false | Generate documentation |
| `include_examples` | Boolean | false | Generate examples |

```toml
[ai.generation]
auto_improve = true
include_tests = true
include_docs = true
include_examples = true
```

---

## RDF and SPARQL

Configure semantic knowledge graph integration.

### Section: `[rdf]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `base_uri` | String | "" | Base URI for RDF resources |
| `prefixes` | Table | {} | Namespace prefix mappings |
| `patterns` | Array[String] | [] | Glob patterns for RDF file discovery |
| `inline` | Array[String] | [] | Inline RDF content (Turtle syntax) |

```toml
[rdf]
base_uri = "https://example.com/ontology/"
prefixes = {
    ex = "https://example.com/ontology/",
    schema = "http://schema.org/",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs = "http://www.w3.org/2000/01/rdf-schema#"
}
patterns = ["templates/**/graphs/*.ttl", "ontology/**/*.rdf"]
inline = ["@prefix ex: <https://example.com/> . ex:Foo a ex:Class ."]
```

### Section: `[graph]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `base_iri` | String | "" | Base IRI for graph operations |
| `cache_queries` | Boolean | true | Cache SPARQL query results |
| `query_timeout_seconds` | Integer | 30 | Query execution timeout |

```toml
[graph]
base_iri = "http://example.org/my-project/"
cache_queries = true
query_timeout_seconds = 30
```

### Section: `[sparql]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `timeout` | Integer | 10 | Query timeout in seconds |
| `max_results` | Integer | 1000 | Maximum result limit |
| `cache_enabled` | Boolean | true | Enable query caching |

```toml
[sparql]
timeout = 10
max_results = 1000
cache_enabled = true
```

---

## Marketplace

Configure package marketplace integration.

### Section: `[marketplace]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `registry_url` | String | "https://registry.ggen.dev" | Package registry URL |
| `cache_dir` | String | ".ggen/marketplace" | Local cache directory |
| `offline_mode` | Boolean | false | Use offline cache only |

```toml
[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"
offline_mode = false
```

---

## Lifecycle

Define build phases and task automation.

### Section: `[lifecycle]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | Boolean | false | Enable lifecycle management |
| `config_file` | String | "make.toml" | Lifecycle config file (cargo-make format) |
| `cache_directory` | String | ".ggen/cache" | Cache directory |
| `state_file` | String | ".ggen/state.json" | State persistence file |
| `makefile` | String | "" | Alternative makefile path |

```toml
[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"
```

### Lifecycle Phases: `[lifecycle.phases]`

Define custom phase sequences:

```toml
[lifecycle.phases]
default = ["init", "setup", "generate", "validate", "test"]
development = ["init", "setup", "generate", "validate", "test", "dev"]
production = ["generate", "validate", "test", "build", "deploy"]
ci = ["validate", "test", "build", "package"]
```

---

## Security

Configure security features and protections.

### Section: `[security]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `path_traversal_protection` | Boolean | true | Prevent path traversal attacks |
| `shell_injection_protection` | Boolean | true | Prevent shell injection |
| `template_sandboxing` | Boolean | true | Sandbox template execution |
| `validate_paths` | Boolean | true | Validate all file paths |
| `audit_operations` | Boolean | false | Log security-sensitive operations |
| `require_confirmation` | Boolean | false | Require user confirmation for writes |

```toml
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
audit_operations = true
require_confirmation = false
```

---

## Performance

Optimize execution performance.

### Section: `[performance]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `parallel_execution` | Boolean | false | Enable parallel processing |
| `max_workers` | Integer | 4 | Maximum worker threads |
| `cache_size` | String | "512MB" | Cache size limit |
| `enable_profiling` | Boolean | false | Enable performance profiling |
| `memory_limit_mb` | Integer | 512 | Memory limit in MB |

```toml
[performance]
parallel_execution = true
max_workers = 16
cache_size = "1GB"
enable_profiling = true
memory_limit_mb = 1024
```

---

## Logging

Configure logging output and behavior.

### Section: `[logging]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `level` | String | "info" | Log level (trace, debug, info, warn, error) |
| `format` | String | "text" | Log format (text, json, pretty) |
| `output` | String | "stdout" | Output destination (stdout, stderr, file) |
| `file_path` | String | "" | Log file path (if output=file) |
| `rotation` | String | "daily" | Log rotation (hourly, daily, weekly) |

```toml
[logging]
level = "info"
format = "json"
output = "file"
file_path = ".ggen/logs/ggen.log"
rotation = "daily"
```

---

## Features

Enable or disable features.

### Section: `[features]`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `ai_generation` | Boolean | false | AI-powered code generation |
| `sparql_queries` | Boolean | false | SPARQL query support |
| `lifecycle_management` | Boolean | false | Lifecycle automation |
| `template_validation` | Boolean | true | Validate templates before rendering |
| `graph_analysis` | Boolean | false | Knowledge graph analysis |
| `documentation_generation` | Boolean | false | Auto-generate documentation |
| `code_validation` | Boolean | true | Validate generated code |
| `test_generation` | Boolean | false | Generate tests |

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

---

## Complete Example

Here's a comprehensive `ggen.toml` showcasing all features:

```toml
# Project metadata
[project]
name = "comprehensive-example"
version = "1.0.0"
description = "Comprehensive ggen configuration example"
author = "Example Developer <dev@example.com>"
license = "MIT"
repository = "https://github.com/example/comprehensive"

# Template configuration
[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true

[templates.rust]
style = "core-team"
error_handling = "thiserror"
async_runtime = "tokio"

[templates.web]
framework = "axum"
database = "postgresql"
documentation = "openapi"

# AI configuration
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"

[ai.cache]
enabled = true
ttl_seconds = 3600

[ai.generation]
include_tests = true
include_docs = true

# RDF and SPARQL
[rdf]
base_uri = "https://example.com/ontology/"
prefixes = { ex = "https://example.com/ontology/", schema = "http://schema.org/" }

[sparql]
timeout = 10
cache_enabled = true

# Marketplace
[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"

# Lifecycle
[lifecycle]
enabled = true
config_file = "make.toml"

[lifecycle.phases]
default = ["init", "generate", "test"]
production = ["generate", "validate", "build", "deploy"]

# Security
[security]
path_traversal_protection = true
shell_injection_protection = true
audit_operations = true

# Performance
[performance]
parallel_execution = true
max_workers = 8
enable_profiling = true

# Logging
[logging]
level = "info"
format = "json"
output = "file"
file_path = ".ggen/logs/ggen.log"

# Features
[features]
ai_generation = true
sparql_queries = true
template_validation = true
code_validation = true
```

---

## Validation

Validate your `ggen.toml`:

```bash
# Check TOML syntax
toml check ggen.toml

# Validate against ggen schema
ggen config validate

# Show parsed configuration
ggen config show
```

## Schema Definition

For IDE support and validation, see the JSON Schema definition at:
`https://github.com/seanchatmangpt/ggen/schema/ggen.toml.schema.json`

---

For usage examples and tutorials, see [ggen.toml User Guide](./ggen-toml-guide.md).
