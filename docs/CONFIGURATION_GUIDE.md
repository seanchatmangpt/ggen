<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Configuration Guide](#ggen-configuration-guide)
  - [Overview](#overview)
  - [Configuration Sources & Precedence](#configuration-sources--precedence)
  - [Project Configuration: `ggen.toml`](#project-configuration-ggentoml)
    - [Project Metadata](#project-metadata)
    - [Template Configuration](#template-configuration)
    - [AI/LLM Configuration](#aillm-configuration)
    - [RDF & Graph Configuration](#rdf--graph-configuration)
    - [Lifecycle Configuration](#lifecycle-configuration)
    - [Security Configuration](#security-configuration)
    - [Performance Configuration](#performance-configuration)
    - [Logging Configuration](#logging-configuration)
    - [Environment-Specific Overrides](#environment-specific-overrides)
    - [Template-Specific Configuration](#template-specific-configuration)
    - [Data Sources & SPARQL Queries](#data-sources--sparql-queries)
    - [Build, Test & Deployment](#build-test--deployment)
  - [Lifecycle Configuration: `make.toml`](#lifecycle-configuration-maketoml)
  - [System & User Configuration Files](#system--user-configuration-files)
  - [Environment Variables](#environment-variables)
    - [LLM Provider Configuration](#llm-provider-configuration)
    - [GitHub Integration](#github-integration)
    - [Registry Configuration](#registry-configuration)
    - [Code Generation](#code-generation)
    - [Logging](#logging)
    - [Development & Testing](#development--testing)
  - [Complete Configuration Example](#complete-configuration-example)
  - [Configuration Examples](#configuration-examples)
    - [Basic Project](#basic-project)
    - [AI-Powered Project](#ai-powered-project)
    - [Multi-Language Project](#multi-language-project)
    - [Enterprise Configuration](#enterprise-configuration)
    - [Local Development (Ollama)](#local-development-ollama)
    - [Cloud Development (OpenAI)](#cloud-development-openai)
  - [Configuration Validation](#configuration-validation)
  - [Best Practices](#best-practices)
  - [Security Considerations](#security-considerations)
  - [Troubleshooting](#troubleshooting)
  - [Migration Guide](#migration-guide)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Configuration Guide

Comprehensive guide to configuring ggen projects, including `ggen.toml` project configuration, `make.toml` lifecycle configuration, environment variables, and system/user configuration files.

## Overview

ggen supports configuration through multiple sources with a clear precedence order. This guide documents all configuration options, from project-specific `ggen.toml` files to system-wide environment variables.

**Key Principles:**
- **Zero-cost abstractions** - Configuration is type-safe and validated at compile-time where possible
- **Deterministic** - Same configuration produces identical results
- **Hierarchical** - Configuration cascades from system → user → project → environment → CLI
- **Secure defaults** - Sensitive values (API keys) are never stored in files

## Configuration Sources & Precedence

Configuration is loaded and merged in the following order (later sources override earlier ones):

1. **System defaults** (embedded in binary, `config/defaults.toml`)
2. **System config** (`/etc/ggen/config.toml` on Linux, `/Library/Application Support/ggen/config.toml` on macOS)
3. **User config** (`~/.config/ggen/config.toml`)
4. **Project config** (`./ggen.toml` in project root)
5. **Environment variables** (e.g., `GGEN_LLM_PROVIDER`, `OLLAMA_MODEL`)
6. **CLI arguments** (highest precedence)

**Priority Order:**
```
CLI Args > Environment Variables > Project Config > User Config > System Config > Defaults
```

## Project Configuration: `ggen.toml`

The `ggen.toml` file in your project root is the primary way to configure project-specific settings. All sections are optional - ggen will use sensible defaults if a section is omitted.

### Project Metadata

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "My ggen project description"
author = "Your Name"
license = "MIT"
repository = "https://github.com/user/my-project"
```

### Template Configuration

```toml
[templates]
# Template source directory (relative to project root)
source_dir = "templates"

# Generated output directory
output_dir = "generated"

# Enable backups before overwriting files
backup_enabled = true

# Ensure idempotent generation (same inputs → same outputs)
idempotent = true

# Dry run mode (show what would be generated without writing)
dry_run = false
```

### AI/LLM Configuration

```toml
[ai]
# Provider: "openai", "anthropic", "ollama", or "mock"
provider = "ollama"

# Model name (provider-specific)
model = "qwen3-coder:30b"

# Generation parameters
temperature = 0.7      # 0.0 (deterministic) to 2.0 (creative)
max_tokens = 2048      # Maximum tokens to generate
timeout_seconds = 30   # Request timeout
retry_attempts = 3     # Number of retries on failure

[ai.prompts]
# Custom system prompt
system = "You are an expert Rust developer..."
user_prefix = "Generate code with the following requirements:"

[ai.validation]
# Enable quality validation
enabled = true
quality_threshold = 0.8    # Minimum quality score (0.0-1.0)
max_iterations = 3         # Max validation attempts
```

**Note:** API keys are **never** stored in `ggen.toml`. Use environment variables:
- `OPENAI_API_KEY` for OpenAI
- `ANTHROPIC_API_KEY` for Anthropic
- Ollama uses local server (no API key needed)

### RDF & Graph Configuration

```toml
[rdf]
# Base IRI for RDF resources
base_iri = "http://example.org/my-project/"

# Default RDF format: "turtle", "rdfxml", "jsonld", "ntriples"
default_format = "turtle"

# Cache SPARQL query results
cache_queries = true
query_timeout_seconds = 10

# RDF namespace prefixes
[rdf.prefixes]
ex = "http://example.org/my-project/"
schema = "http://schema.org/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

[graph]
# Enable graph caching
enable_caching = true
cache_size = 1000           # Max cached triples
cache_ttl_seconds = 3600     # Cache TTL
enable_tracing = true        # Enable graph operation tracing
```

### Lifecycle Configuration

```toml
[lifecycle]
# Enable lifecycle management
enabled = true

# Path to make.toml (lifecycle configuration)
config_file = "make.toml"

# Cache and state directories
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases]
# Define phase sequences
default = ["init", "setup", "build", "test"]
development = ["init", "setup", "build", "test", "dev"]
production = ["build", "test", "docker", "deploy"]
```

**Note:** Lifecycle phases are configured in `make.toml`, not `ggen.toml`. See [Lifecycle Configuration](#lifecycle-configuration-maketoml).

### Security Configuration

```toml
[security]
# Validate file paths to prevent traversal attacks
validate_paths = true

# Block shell injection in templates
block_shell_injection = true

# Require confirmation before destructive operations
require_confirmation = false

# Audit operations for security review
audit_operations = true

# Backup files before writing
backup_before_write = true
```

### Performance Configuration

```toml
[performance]
# Enable parallel template execution
parallel_execution = true
max_parallel_templates = 4

# Pipeline timeout
timeout_seconds = 300
retry_attempts = 3

# Resource limits
memory_limit_mb = 512
cpu_limit_percent = 80

# Profiling
enable_profiling = false
profile_output = ".ggen/profiles/"
```

### Logging Configuration

```toml
[logging]
# Log level: "trace", "debug", "info", "warn", "error"
level = "info"

# Log format: "pretty", "json", "compact"
format = "json"

# Log output: "stdout", "file", "both"
output = "file"

# File logging (if output = "file" or "both")
file_path = ".ggen/logs/ggen.log"
max_file_size = "10MB"
max_files = 5
```

### Environment-Specific Overrides

Override settings per environment:

```toml
[env.development]
ai.model = "gpt-3.5-turbo"
ai.temperature = 0.9
logging.level = "debug"
security.require_confirmation = false

[env.staging]
ai.model = "gpt-4"
ai.temperature = 0.5
logging.level = "info"
security.require_confirmation = true

[env.production]
ai.model = "gpt-4"
ai.temperature = 0.3
logging.level = "warn"
security.require_confirmation = true
security.audit_operations = true
```

Activate environment via `GGEN_ENVIRONMENT`:
```bash
export GGEN_ENVIRONMENT=production
```

### Template-Specific Configuration

Configure individual templates:

```toml
[templates.rust-service]
description = "Rust microservice template"
variables = { language = "rust", framework = "axum" }
output_pattern = "generated/src/services/{name}.rs"
backup_enabled = true

[templates.api-endpoint]
description = "REST API endpoint template"
variables = { method = "GET", path = "/api/v1" }
output_pattern = "generated/src/api/{name}.rs"
```

### Data Sources & SPARQL Queries

Define RDF data sources and SPARQL queries:

```toml
[data_sources]
domain_model = "data/domain.ttl"
api_specification = "data/api-spec.ttl"
database_schema = "data/database.ttl"

[queries]
find_endpoints = "SELECT ?endpoint WHERE { ?endpoint a <http://example.org/APIEndpoint> }"
find_entities = "SELECT ?entity WHERE { ?entity a <http://example.org/Entity> }"
find_relationships = "SELECT ?rel WHERE { ?rel a <http://example.org/Relationship> }"
```

### Build, Test & Deployment

```toml
[build]
target = "release"                    # "debug" or "release"
features = ["default"]                 # Cargo features
profile = "release"
parallel_jobs = 4

[test]
framework = "cargo"
parallel = true
timeout_seconds = 300
coverage_enabled = true
coverage_threshold = 80                # Minimum coverage %

[deployment]
strategy = "rolling"                   # "rolling", "blue-green", "canary"
health_check_path = "/health"
rollback_enabled = true
max_instances = 3
min_instances = 1

[monitoring]
metrics_enabled = true
tracing_enabled = true
health_checks_enabled = true
alerting_enabled = true

[backup]
enabled = true
strategy = "incremental"               # "full", "incremental"
retention_days = 30
compression = true
encryption = false

[cache]
enabled = true
strategy = "lru"                       # "lru", "fifo", "random"
max_size_mb = 100
ttl_seconds = 3600
persistent = true
```

## Lifecycle Configuration: `make.toml`

The `make.toml` file configures ggen's lifecycle management system (phases, hooks, pipelines). This is separate from `ggen.toml` which focuses on project and generation configuration.

**Key Sections:**

```toml
[lifecycle]
name = "my-project"
version = "1.0.0"

[phases.validate-safety]
description = "Validate code safety"
commands = ["./scripts/check-no-panic-points.sh"]
error_handling = "stop"
required = true
timeout = 60

[phases.build]
description = "Build release"
commands = ["cargo build --release --all-features"]
parallel = false
timeout = 600

[phases.test]
description = "Run tests"
commands = ["cargo test --all-features"]
parallel = false
timeout = 600

[hooks]
before_build = ["validate-safety", "lint"]
after_build = ["test"]
before_deploy = ["production-validate"]

[pipelines]
dev = ["lint", "build-debug", "test"]
production = ["validate-safety", "lint", "build", "test"]
```

See `make.toml` in the project root for a complete example, or refer to the [Lifecycle Documentation](lifecycle.md).

## System & User Configuration Files

### User Config: `~/.config/ggen/config.toml`

Store user-specific defaults:

```toml
[llm]
provider = "ollama"

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"

[logging]
level = "info"
format = "pretty"

[registry]
url = "https://seanchatmangpt.github.io/ggen/registry/"
```

### System Config: `/etc/ggen/config.toml` (Linux) or `/Library/Application Support/ggen/config.toml` (macOS)

Store system-wide defaults (requires root/admin access):

```toml
[logging]
level = "warn"

[security]
validate_paths = true
block_shell_injection = true
```

## Environment Variables

Environment variables override configuration files and have high precedence. Use them for:
- **API keys** (never in files)
- **Temporary overrides** (CI/CD, one-off runs)
- **Sensitive data** (credentials, tokens)

### LLM Provider Configuration

```bash
# Provider Selection
export GGEN_LLM_PROVIDER=ollama          # openai, anthropic, ollama, mock

# Ollama (Local AI)
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
export OLLAMA_TIMEOUT=30

# OpenAI
export OPENAI_API_KEY=sk-...             # REQUIRED for OpenAI
export OPENAI_BASE_URL=https://api.openai.com/v1
export OPENAI_MODEL=gpt-3.5-turbo
export OPENAI_ORGANIZATION=org-...       # Optional

# Anthropic
export ANTHROPIC_API_KEY=sk-ant-...      # REQUIRED for Anthropic
export ANTHROPIC_BASE_URL=https://api.anthropic.com
export ANTHROPIC_MODEL=claude-3-sonnet-20240229
```

### GitHub Integration

```bash
# GitHub API
export GITHUB_TOKEN=ghp_...              # REQUIRED for private repos
export GH_TOKEN=ghp_...                   # Alternative to GITHUB_TOKEN
export GGEN_GITHUB_BASE_URL=https://api.github.com
export GGEN_GITHUB_TIMEOUT=30

# GitHub Enterprise
export GGEN_GITHUB_BASE_URL=https://github.company.com/api/v3
```

### Registry Configuration

```bash
# Marketplace Registry
export GGEN_REGISTRY_URL=https://seanchatmangpt.github.io/ggen/registry/
export GGEN_REGISTRY_TIMEOUT=30

# Custom registry
export GGEN_REGISTRY_URL=https://registry.company.com/
```

### Code Generation

```bash
# Generation Parameters
export GGEN_GENERATION_TEMPERATURE=0.7   # 0.0-2.0 (creativity)
export GGEN_GENERATION_MAX_TOKENS=4096   # Max output length
export GGEN_GENERATION_TOP_P=0.9         # 0.0-1.0 (diversity)
export GGEN_GENERATION_STREAMING=false   # Enable streaming
```

### Logging

```bash
# Log Configuration
export GGEN_LOG_LEVEL=info               # trace, debug, info, warn, error
export RUST_LOG=ggen=debug                # Alternative (Rust standard)
export GGEN_LOG_FORMAT=pretty            # pretty, json, compact
export GGEN_LOG_FILE=/var/log/ggen.log   # Optional log file
```

### Development & Testing

```bash
# Test Mode (uses mock clients, no API calls)
export GGEN_TEST_MODE=1
export GGEN_ALLOW_LIVE_CALLS=0            # Disable live API calls in tests
export GGEN_DEV_MODE=1                    # Development mode
```

## Complete Configuration Example

Here's a complete `ggen.toml` example showing all available sections:

```toml
# My Project Configuration
[project]
name = "my-api-service"
version = "1.0.0"
description = "REST API service generated with ggen"
author = "John Doe"
license = "MIT"
repository = "https://github.com/user/my-api-service"

[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true

[ai]
provider = "ollama"
model = "qwen3-coder:30b"
temperature = 0.7
max_tokens = 2048
timeout_seconds = 30

[rdf]
base_iri = "http://example.org/my-api/"
default_format = "turtle"
cache_queries = true

[lifecycle]
enabled = true
config_file = "make.toml"

[security]
validate_paths = true
block_shell_injection = true

[logging]
level = "info"
format = "json"
output = "file"
file_path = ".ggen/logs/ggen.log"

[env.development]
ai.temperature = 0.9
logging.level = "debug"

[env.production]
ai.temperature = 0.3
logging.level = "warn"
security.audit_operations = true
```

## Configuration Examples

### Basic Project

Minimal configuration for a simple project:

```toml
[project]
name = "my-cli-tool"
version = "0.1.0"

[templates]
source_dir = "templates"
output_dir = "generated"
```

### AI-Powered Project

Project using AI generation:

```toml
[project]
name = "ai-powered-service"

[ai]
provider = "ollama"
model = "qwen3-coder:30b"
temperature = 0.7

[templates]
source_dir = "templates"
output_dir = "generated"
```

### Multi-Language Project

Project generating multiple languages:

```toml
[project]
name = "multi-lang-api"

[templates]
source_dir = "templates"
output_dir = "generated"

[templates.rust]
variables = { language = "rust", framework = "axum" }

[templates.typescript]
variables = { language = "typescript", framework = "express" }

[rdf]
base_iri = "http://api.example.org/"
```

### Enterprise Configuration

Enterprise setup with GitHub Enterprise and private registry:

```toml
[project]
name = "enterprise-tool"

[registry]
url = "https://internal-registry.company.com"
# Token via GGEN_REGISTRY_TOKEN env var

[security]
validate_paths = true
block_shell_injection = true
require_confirmation = true
audit_operations = true

[rdf]
allow_remote = false  # Security: no remote RDF

[logging]
level = "warn"
format = "json"
```

### Local Development (Ollama)

```toml
[ai]
provider = "ollama"
model = "qwen3-coder:30b"

[logging]
level = "debug"
format = "pretty"
```

Set via environment:
```bash
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
```

### Cloud Development (OpenAI)

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
```

Set API key via environment:
```bash
export OPENAI_API_KEY=sk-proj-...
```

## Configuration Validation

Validate your configuration:

```bash
# Validate project config (future)
ggen config validate

# Show effective configuration (future)
ggen config show

# Test configuration
ggen ai generate "test" --verbose
```

**Common Validation Issues:**

1. **Invalid TOML syntax** - Check brackets, quotes, commas
2. **Missing required fields** - Some sections require specific keys
3. **Invalid values** - Check ranges (temperature: 0.0-2.0, log level: valid enum)
4. **File not found** - Verify paths are relative to project root

## Best Practices

### Project Configuration

1. **Version control** - Commit `ggen.toml` to git (but not API keys)
2. **Semantic versioning** - Use semver for project versions
3. **Environment separation** - Use `[env.*]` sections for dev/staging/prod
4. **Documentation** - Add comments explaining non-obvious settings

### Security

1. **Never commit API keys** - Use environment variables only
2. **Restrict file permissions** - `chmod 600 ~/.config/ggen/config.toml`
3. **Validate paths** - Enable `validate_paths = true` in production
4. **Audit operations** - Enable `audit_operations = true` in production

### Performance

1. **Enable caching** - Set `cache_enabled = true` for repeated operations
2. **Parallel execution** - Enable `parallel_execution = true` for multi-template projects
3. **Resource limits** - Set appropriate `memory_limit_mb` and `cpu_limit_percent`

### Team Configuration

1. **Shared defaults** - Store team defaults in user config files
2. **Consistent structure** - Use consistent `ggen.toml` structure across projects
3. **Documentation** - Document team-specific configuration patterns

## Security Considerations

### ✅ DO:
- Store API keys in environment variables
- Use `.env` files (add to `.gitignore`)
- Use secret managers in production
- Restrict file permissions: `chmod 600 ~/.config/ggen/config.toml`
- Enable path validation in production

### ❌ DON'T:
- Commit API keys to git
- Store keys in TOML files
- Share keys in logs/screenshots
- Use production keys in development
- Disable security settings in production

## Troubleshooting

### Configuration Not Loading

**Issue:** Changes to `ggen.toml` aren't taking effect.

**Solutions:**
1. Check file is in project root (same directory as `Cargo.toml`)
2. Verify TOML syntax is valid
3. Check environment variables aren't overriding settings
4. Restart ggen process

### Environment Variables Not Working

**Issue:** Environment variables are ignored.

**Solutions:**
1. Verify variable name is correct (check [Environment Variables](#environment-variables))
2. Export variable in same shell session: `export GGEN_LLM_PROVIDER=ollama`
3. Check for typos in variable names
4. Ensure variable is exported (not just set locally)

### API Key Errors

**Issue:** `OPENAI_API_KEY environment variable not set`

**Solutions:**
1. Export API key: `export OPENAI_API_KEY=sk-...`
2. Verify key is correct (not expired)
3. Check key permissions (some keys have restricted access)
4. Use `.env` file with proper loading (if supported)

### Invalid Configuration Values

**Issue:** Configuration validation fails.

**Solutions:**
1. Check value ranges (temperature: 0.0-2.0)
2. Verify enum values (log level: trace|debug|info|warn|error)
3. Check path existence (relative to project root)
4. Review error messages for specific field issues

## Migration Guide

### From v1.x to v2.0+

**Breaking Changes:**
- Configuration structure has changed
- Some environment variable names changed
- New `[ai]` section replaces `[llm]` section (partial)

**Migration Steps:**

1. **Update `ggen.toml` structure:**
   ```toml
   # Old (v1.x)
   [llm]
   provider = "ollama"
   
   # New (v2.0+)
   [ai]
   provider = "ollama"
   ```

2. **Update environment variables:**
   ```bash
   # Old
   export LLM_PROVIDER=ollama
   
   # New
   export GGEN_LLM_PROVIDER=ollama
   ```

3. **Review new sections:**
   - `[lifecycle]` - New lifecycle management
   - `[security]` - Enhanced security options
   - `[performance]` - Performance tuning

See [Migration Guide](MIGRATION_V1_TO_V2.md) for complete details.

---

**Related Documentation:**
- [Lifecycle Documentation](lifecycle.md) - `make.toml` configuration
- [Template Guide](templates.md) - Template configuration
- [AI Integration Guide](ai-guide.md) - AI provider setup
- [RDF Integration Guide](rdf_metadata_guide.md) - RDF configuration

**Last Updated:** 2025-01-XX  
**Version:** 2.0.0


