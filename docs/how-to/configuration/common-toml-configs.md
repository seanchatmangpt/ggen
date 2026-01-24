<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Common TOML Configuration Patterns](#common-toml-configuration-patterns)
  - [Quick Navigation](#quick-navigation)
  - [AI Provider Setup](#ai-provider-setup)
    - [OpenAI Configuration](#openai-configuration)
    - [Anthropic (Claude) Configuration](#anthropic-claude-configuration)
    - [Ollama (Local) Configuration](#ollama-local-configuration)
  - [Template Configuration](#template-configuration)
    - [JavaScript + Zod Project](#javascript--zod-project)
    - [Multi-Output Configuration](#multi-output-configuration)
  - [RDF & SPARQL Configuration](#rdf--sparql-configuration)
    - [Basic RDF Setup](#basic-rdf-setup)
    - [Schema.org Integration](#schemaorg-integration)
    - [SPARQL Performance Tuning](#sparql-performance-tuning)
  - [Environment-Specific Configuration](#environment-specific-configuration)
    - [Development vs Production](#development-vs-production)
    - [Testing Configuration](#testing-configuration)
  - [Multi-Language Projects](#multi-language-projects)
    - [JavaScript + Rust Project](#javascript--rust-project)
    - [Full-Stack Configuration](#full-stack-configuration)
  - [Performance Optimization](#performance-optimization)
    - [High-Performance Configuration](#high-performance-configuration)
    - [Low-Resource Configuration](#low-resource-configuration)
  - [Security Configuration](#security-configuration)
    - [Strict Security](#strict-security)
    - [Development Security](#development-security)
  - [Lifecycle Hooks](#lifecycle-hooks)
    - [Automated Workflow](#automated-workflow)
  - [Complete Examples](#complete-examples)
    - [Small JavaScript Project](#small-javascript-project)
    - [Large Enterprise Project](#large-enterprise-project)
  - [Troubleshooting](#troubleshooting)
    - [Configuration Not Loading](#configuration-not-loading)
    - [Environment Override Not Working](#environment-override-not-working)
    - [API Keys Not Found](#api-keys-not-found)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Common TOML Configuration Patterns

Practical examples of ggen.toml configurations for common use cases.

## Quick Navigation

- [AI Provider Setup](#ai-provider-setup)
- [Template Configuration](#template-configuration)
- [RDF & SPARQL](#rdf--sparql-configuration)
- [Environment-Specific Configs](#environment-specific-configuration)
- [Multi-Language Projects](#multi-language-projects)
- [Performance Optimization](#performance-optimization)

---

## AI Provider Setup

### OpenAI Configuration

**Basic Setup**:

```toml
[project]
name = "my-project"
version = "1.0.0"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout = 60

# API key from environment: $OPENAI_API_KEY
```

**With Custom Endpoint** (Azure OpenAI):

```toml
[ai]
provider = "openai"
model = "gpt-4"
base_url = "https://your-resource.openai.azure.com/"
api_key = "${AZURE_OPENAI_API_KEY}"  # From environment
temperature = 0.7
```

**Cost-Conscious Setup** (Development):

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

# Use cheaper model in development
[env.development]
"ai.model" = "gpt-3.5-turbo"
"ai.max_tokens" = 1000  # Reduce token usage
```

---

### Anthropic (Claude) Configuration

```toml
[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"
temperature = 0.5
max_tokens = 8000
timeout = 90

# API key from environment: $ANTHROPIC_API_KEY
```

**With Fallback Models**:

```toml
[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"

# Use cheaper model for dev
[env.development]
"ai.model" = "claude-3-haiku-20240307"

# Use most powerful for production
[env.production]
"ai.model" = "claude-3-opus-20240229"
"ai.temperature" = 0.3  # More deterministic
```

**⚠️ When NOT to Use Anthropic**:
- When working offline (requires internet connection)
- In CI/CD with no API access (use Ollama instead)
- For high-volume generation (watch API costs)
- When strict data privacy required (models run on Anthropic servers)

---

### Ollama (Local) Configuration

**Basic Local Setup**:

```toml
[ai]
provider = "ollama"
model = "llama2"
base_url = "http://localhost:11434"
temperature = 0.8
max_tokens = 4000
timeout = 120
```

**With Custom Model**:

```toml
[ai]
provider = "ollama"
model = "codellama:13b"  # Code-specialized model
base_url = "http://localhost:11434"
temperature = 0.7
```

**Remote Ollama Server**:

```toml
[ai]
provider = "ollama"
model = "llama2"
base_url = "http://ollama-server.local:11434"
timeout = 180  # Longer timeout for remote
```

**⚠️ When NOT to Use Ollama**:
- When you need cutting-edge AI models (Ollama models lag behind cloud)
- For production deployments requiring guaranteed uptime
- When local hardware is insufficient (requires 8-16GB RAM for decent models)
- For complex reasoning tasks (cloud models like GPT-4/Claude are superior)

---

## Template Configuration

### JavaScript + Zod Project

```toml
[project]
name = "ecommerce-models"
version = "1.0.0"
description = "E-commerce data models with Zod validation"

[templates]
directory = "templates"
backup_enabled = true
idempotent = true

# Template-specific settings
[templates.models]
enabled = true
output = "src/generated/models.js"

[templates.validators]
enabled = true
output = "src/generated/validators.js"

[templates.tests]
enabled = false  # Skip tests in dev
```

**With Watch Mode**:

```toml
[templates]
directory = "templates"
backup_enabled = true
idempotent = true

# Watch configuration
[templates.watch]
enabled = true
debounce = 500  # milliseconds
patterns = ["**/*.tmpl", "**/*.rdf"]
```

---

### Multi-Output Configuration

```toml
[templates]
directory = "templates"

# Multiple output directories
[templates.outputs]
javascript = "src/generated/js"
rust = "src/generated/rs"
python = "src/generated/py"
docs = "docs/generated"

backup_enabled = true
idempotent = true
```

---

## RDF & SPARQL Configuration

### Basic RDF Setup

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

---

### Schema.org Integration

```toml
[rdf]
base_uri = "https://example.com/catalog/"
default_format = "turtle"

[rdf.prefixes]
schema = "https://schema.org/"
ex = "https://example.com/catalog/"

[rdf.imports]
# Auto-import Schema.org definitions
schema_org = "https://schema.org/version/latest/schemaorg-current-https.jsonld"
```

---

### SPARQL Performance Tuning

```toml
[sparql]
timeout = 60
max_results = 5000
cache_enabled = true
cache_ttl = 7200  # 2 hours

# Performance settings
[sparql.performance]
parallel_queries = true
batch_size = 1000
```

**For Large Datasets**:

```toml
[sparql]
timeout = 300  # 5 minutes for complex queries
max_results = 50000
cache_enabled = true
cache_ttl = 86400  # 24 hours

[sparql.performance]
parallel_queries = true
batch_size = 5000
use_indexes = true
```

---

## Environment-Specific Configuration

### Development vs Production

```toml
[project]
name = "my-app"
version = "1.0.0"

# Base configuration
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

[logging]
level = "info"
format = "pretty"

[performance]
parallel_generation = true

# Development: fast iteration, verbose logging
[env.development]
"ai.model" = "gpt-3.5-turbo"      # Cheaper
"ai.temperature" = 0.9            # More creative
"logging.level" = "debug"         # Verbose
"performance.max_workers" = 4     # Limit CPU
"templates.backup_enabled" = false # Skip backups

# Staging: balance cost and quality
[env.staging]
"ai.model" = "gpt-4"
"ai.temperature" = 0.5
"logging.level" = "info"
"performance.max_workers" = 8

# Production: quality and determinism
[env.production]
"ai.model" = "gpt-4"
"ai.temperature" = 0.3           # Deterministic
"logging.level" = "warn"         # Quiet
"logging.format" = "json"        # Structured
"performance.max_workers" = 16   # Full power
"sparql.cache_enabled" = true    # Performance
```

**Usage**:

```bash
# Development (default)
ggen ontology generate schema.json

# Staging
GGEN_ENV=staging ggen ontology generate schema.json

# Production
GGEN_ENV=production ggen ontology generate schema.json
```

---

### Testing Configuration

```toml
[env.test]
"ai.provider" = "ollama"           # Local, no API costs
"ai.model" = "llama2"
"ai.base_url" = "http://localhost:11434"
"logging.level" = "error"          # Quiet tests
"templates.backup_enabled" = false # Speed up tests
"rdf.cache_queries" = false        # Fresh data
```

**Run tests**:

```bash
GGEN_ENV=test npm test
```

---

## Multi-Language Projects

### JavaScript + Rust Project

```toml
[project]
name = "multi-lang-schemas"
version = "1.0.0"

[templates]
directory = "templates"

# Multiple output directories
[templates.outputs]
javascript = "packages/js/src/generated"
rust = "crates/core/src/generated"

# Language-specific settings
[templates.javascript]
enabled = true
zod = true
jsdoc = true
tests = true

[templates.rust]
enabled = true
serde = true
derive_traits = ["Debug", "Clone", "PartialEq"]
tests = true
```

---

### Full-Stack Configuration

```toml
[project]
name = "fullstack-app"

[templates.outputs]
frontend = "frontend/src/generated"
backend = "backend/src/generated"
database = "database/migrations/generated"
docs = "docs/generated"

# Frontend (JavaScript + Zod)
[templates.frontend]
enabled = true
language = "javascript"
zod = true
react_components = true

# Backend (Rust)
[templates.backend]
enabled = true
language = "rust"
async_runtime = "tokio"
api_framework = "axum"

# Database (SQL)
[templates.database]
enabled = true
dialect = "postgresql"
migrations = true
```

---

## Performance Optimization

### High-Performance Configuration

```toml
[performance]
parallel_generation = true
max_workers = 16              # Use all CPU cores
cache_templates = true        # Cache compiled templates
incremental_build = true      # Only regenerate changed
memory_limit = 8589934592     # 8GB memory limit

[rdf]
cache_queries = true
store_path = "/dev/shm/ggen"  # Use RAM disk for speed

[sparql]
cache_enabled = true
cache_ttl = 86400             # Cache for 24 hours
parallel_queries = true
```

**⚠️ When NOT to Use High-Performance Config**:
- On systems with limited RAM (requires 8GB+)
- When disk I/O is bottleneck (RAM disk won't help)
- For small projects (overhead not worth it)
- When debugging (parallel execution hides errors)

---

### Low-Resource Configuration

```toml
[performance]
parallel_generation = false   # Sequential processing
max_workers = 1              # Minimal CPU
cache_templates = false      # Save memory
incremental_build = true     # Reduce work

[sparql]
timeout = 30
max_results = 1000           # Limit result size
cache_enabled = false        # Save memory
```

---

## Security Configuration

### Strict Security

```toml
[security]
# Only allow trusted domains
allowed_domains = [
    "schema.org",
    "dbpedia.org",
    "w3.org",
    "example.com"
]

# Block dangerous patterns
deny_list = [
    "file:///",
    "javascript:",
    "data:",
    "vbscript:"
]

# File size limits
max_file_size = 52428800      # 50MB
validate_ssl = true

# Strict validation
[validation]
strict_mode = true
fail_on_warning = true
```

---

### Development Security

```toml
[security]
# More permissive for dev
allowed_domains = ["*"]
max_file_size = 104857600     # 100MB
validate_ssl = false          # Allow self-signed

[validation]
strict_mode = false
fail_on_warning = false
```

---

## Lifecycle Hooks

### Automated Workflow

```toml
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"

# Pre-generation hooks
[lifecycle.phases.pre_generate]
scripts = [
    "scripts/validate-ontology.sh",
    "scripts/check-dependencies.sh"
]

# Post-generation hooks
[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-code.sh",
    "scripts/run-tests.sh",
    "scripts/generate-docs.sh"
]

# Error handling
[lifecycle.phases.on_error]
scripts = ["scripts/cleanup.sh"]
```

---

## Complete Examples

### Small JavaScript Project

```toml
[project]
name = "simple-models"
version = "1.0.0"

[ai]
provider = "openai"
model = "gpt-3.5-turbo"  # Cost-effective

[templates]
directory = "templates"

[rdf]
base_uri = "https://example.com/"
default_format = "turtle"

[rdf.prefixes]
ex = "https://example.com/"
schema = "https://schema.org/"
```

---

### Large Enterprise Project

```toml
[project]
name = "enterprise-platform"
version = "2.0.0"
description = "Multi-language enterprise data platform"
authors = ["Platform Team <platform@example.com>"]

# Production AI
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.3
max_tokens = 8000
timeout = 120

# Multi-language templates
[templates]
directory = "templates"
backup_enabled = true
idempotent = true

[templates.outputs]
javascript = "packages/js/src/generated"
typescript = "packages/ts/src/generated"
rust = "crates/models/src/generated"
python = "python/generated"
sql = "database/migrations"

# Enterprise RDF with multiple ontologies
[rdf]
base_uri = "https://platform.example.com/"
default_format = "turtle"
cache_queries = true
store_path = "/var/lib/ggen/rdf-store"

[rdf.prefixes]
platform = "https://platform.example.com/"
schema = "https://schema.org/"
foaf = "http://xmlns.com/foaf/0.1/"
dc = "http://purl.org/dc/elements/1.1/"

[rdf.imports]
schema_org = "https://schema.org/version/latest/schemaorg-current-https.jsonld"
foaf = "http://xmlns.com/foaf/spec/index.rdf"

# High-performance SPARQL
[sparql]
timeout = 300
max_results = 100000
cache_enabled = true
cache_ttl = 86400

# Security hardening
[security]
allowed_domains = ["schema.org", "xmlns.com", "purl.org", "platform.example.com"]
max_file_size = 104857600
validate_ssl = true

# Performance optimization
[performance]
parallel_generation = true
max_workers = 32
cache_templates = true
incremental_build = true

# Structured logging
[logging]
level = "info"
format = "json"
output = "/var/log/ggen/generation.log"

# Environment configs
[env.development]
"ai.model" = "gpt-3.5-turbo"
"logging.level" = "debug"
"logging.format" = "pretty"
"performance.max_workers" = 4

[env.staging]
"ai.temperature" = 0.5
"logging.level" = "info"

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"performance.max_workers" = 32
```

---

## Troubleshooting

### Configuration Not Loading

**Check**:
1. File is named `ggen.toml` (not `ggen.tml` or `ggen.yaml`)
2. File is in project root
3. TOML syntax is valid: `ggen config validate`

### Environment Override Not Working

**Check**:
1. `GGEN_ENV` is set: `echo $GGEN_ENV`
2. Section name matches: `[env.development]`
3. Key path uses dots: `"ai.model"` not `"[ai].model"`
4. Verify with: `ggen config show`

### API Keys Not Found

**Set environment variables**:

```bash
# OpenAI
export OPENAI_API_KEY="sk-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."

# Verify
ggen config show ai
```

---

## Related Documentation

- **[ggen.toml Reference](../reference/configuration/ggen-toml-reference.md)** - Complete configuration reference
- **[gpack.toml Reference](../reference/configuration/gpack-toml-reference.md)** - Package format reference
- **[Quick Start Tutorial](../../getting-started/quick-start.md)** - Get started quickly

---

**Last Updated**: 2025-12-10
