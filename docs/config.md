<!-- START doctoc generated TOC please keep comment here to keep auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Configuration](#configuration)
  - [Overview](#overview)
  - [Configuration File Location](#configuration-file-location)
  - [Configuration Priority](#configuration-priority)
  - [Project Configuration (`ggen.toml`)](#project-configuration-ggentoml)
    - [Project Metadata](#project-metadata)
    - [AI Configuration](#ai-configuration)
      - [AI Providers](#ai-providers)
      - [AI Prompts](#ai-prompts)
      - [AI Validation](#ai-validation)
      - [AI Cache](#ai-cache)
    - [Template Configuration](#template-configuration)
    - [RDF and SPARQL Configuration](#rdf-and-sparql-configuration)
    - [Graph Configuration](#graph-configuration)
    - [Lifecycle Configuration](#lifecycle-configuration)
    - [Marketplace Configuration](#marketplace-configuration)
    - [Security Configuration](#security-configuration)
    - [Logging Configuration](#logging-configuration)
    - [Performance Configuration](#performance-configuration)
    - [Environment-Specific Overrides](#environment-specific-overrides)
    - [Template-Specific Configuration](#template-specific-configuration)
    - [Additional Sections](#additional-sections)
  - [Environment Variables](#environment-variables)
    - [Registry Configuration](#registry-configuration)
    - [Global Settings](#global-settings)
    - [Marketplace Settings](#marketplace-settings)
    - [AI Provider Settings](#ai-provider-settings)
  - [Variable Precedence](#variable-precedence)
  - [Configuration Examples](#configuration-examples)
    - [Minimal Configuration](#minimal-configuration)
    - [Basic Project](#basic-project)
    - [AI-Focused Project](#ai-focused-project)
    - [Comprehensive Project](#comprehensive-project)
    - [Enterprise Configuration](#enterprise-configuration)
  - [Configuration Validation](#configuration-validation)
  - [Best Practices](#best-practices)
    - [Project Configuration](#project-configuration-1)
    - [Security](#security)
    - [Performance](#performance)
    - [Team Collaboration](#team-collaboration)
  - [Reference](#reference)

<!-- END doctoc generated TOC please keep comment here to keep auto update -->

# Configuration

ggen supports comprehensive configuration through project files (`ggen.toml`) and environment variables, following core team best practices with typed configuration management.

## Overview

ggen uses a hierarchical configuration system that merges settings from multiple sources:

1. **Default configuration** (`config/defaults.toml`)
2. **System configuration** (`/etc/ggen/config.toml` - optional)
3. **User configuration** (`~/.config/ggen/config.toml` - optional)
4. **Project configuration** (`./ggen.toml` - recommended)
5. **Environment variables** (highest priority)
6. **CLI arguments** (highest priority)

## Configuration File Location

The primary configuration file for your project is `ggen.toml` in the project root directory. ggen automatically discovers this file when executing commands.

```bash
project-root/
├── ggen.toml          # Project configuration (recommended)
├── templates/         # Template files
├── generated/        # Generated output
└── .ggen/            # Cache and metadata
```

## Configuration Priority

Settings are resolved in this order (later values override earlier):

1. **CLI arguments** (`--option value`)
2. **Environment variables** (`GGEN_*`, `OPENAI_API_KEY`, etc.)
3. **Project config** (`./ggen.toml`)
4. **User config** (`~/.config/ggen/config.toml`)
5. **System config** (`/etc/ggen/config.toml`)
6. **Built-in defaults** (lowest priority)

## Project Configuration (`ggen.toml`)

### Project Metadata

Basic project information:

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "A project generated with ggen"
author = "John Doe"
license = "MIT"
repository = "https://github.com/user/my-project"
```

**Fields:**
- `name` (required): Project name
- `version` (optional): Project version (semver recommended)
- `description` (optional): Project description
- `author` (optional): Author name
- `license` (optional): License identifier
- `repository` (optional): Repository URL

### AI Configuration

Configure AI code generation and assistance:

```toml
[ai]
provider = "openai"          # openai, anthropic, ollama, mock
model = "gpt-4"              # Model name
temperature = 0.7           # 0.0-2.0 (creativity)
max_tokens = 2000            # Maximum tokens
timeout_seconds = 30         # Request timeout
retry_attempts = 3          # Retry attempts
stream_enabled = false      # Enable streaming
```

#### AI Providers

Configure specific provider settings:

```toml
[ai.providers.openai]
api_key_env = "OPENAI_API_KEY"    # Environment variable name
organization_env = "OPENAI_ORG_ID" # Optional org ID
model = "gpt-4"                    # Default model
base_url = "https://api.openai.com/v1"

[ai.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
model = "claude-3-5-sonnet-20241022"
base_url = "https://api.anthropic.com"

[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen2.5-coder:7b"
timeout = 30
```

**⚠️ Security Note:** Never store API keys in `ggen.toml`. Always use environment variables.

#### AI Prompts

Customize AI prompt templates:

```toml
[ai.prompts]
system = "You are an expert Rust developer specializing in microservices..."
user_prefix = "Generate a Rust service with the following requirements:"
```

#### AI Validation

Configure code validation and quality checks:

```toml
[ai.validation]
enabled = true
quality_threshold = 0.8      # Quality score threshold (0.0-1.0)
max_iterations = 3           # Maximum improvement iterations
```

#### AI Cache

Configure AI response caching:

```toml
[ai.cache]
enabled = true
ttl_seconds = 3600           # Cache time-to-live
max_entries = 1000           # Maximum cached entries
strategy = "lru"             # lru, fifo, random
```

### Template Configuration

Configure template processing and generation:

```toml
[templates]
source_dir = "templates"      # Template directory
output_dir = "generated"     # Output directory
backup_enabled = true        # Create backups before overwrite
idempotent = true            # Ensure idempotent generation
dry_run = false              # Preview without writing
```

**Template-Specific Configuration:**

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

### RDF and SPARQL Configuration

Configure RDF graph processing and SPARQL queries:

```toml
[rdf]
base_iri = "http://example.org/my-project/"
default_format = "turtle"    # turtle, rdfxml, jsonld
cache_queries = true         # Cache query results
query_timeout_seconds = 10   # Query timeout

# RDF prefixes
prefixes = {
    ex = "http://example.org/my-project/",
    schema = "http://schema.org/",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
}

[sparql]
timeout = 10                 # SPARQL query timeout
max_results = 1000          # Maximum query results
cache_enabled = true        # Enable query caching
```

**Data Sources:**

```toml
[data_sources]
domain_model = "data/domain.ttl"
api_specification = "data/api-spec.ttl"
database_schema = "data/database.ttl"
```

**SPARQL Query Definitions:**

```toml
[queries]
find_endpoints = "SELECT ?endpoint WHERE { ?endpoint a <http://example.org/APIEndpoint> }"
find_entities = "SELECT ?entity WHERE { ?entity a <http://example.org/Entity> }"
find_relationships = "SELECT ?rel WHERE { ?rel a <http://example.org/Relationship> }"
```

### Graph Configuration

Configure RDF graph processing:

```toml
[graph]
base_iri = "http://example.org/my-project/"
enable_caching = true        # Enable graph caching
cache_size = 1000           # Cache size (triples)
cache_ttl_seconds = 3600     # Cache TTL
enable_tracing = true       # Enable query tracing
allow_remote = false        # Allow remote RDF loading (security)
```

### Lifecycle Configuration

Configure project lifecycle management (integration with `make.toml`):

```toml
[lifecycle]
enabled = true
config_file = "make.toml"    # Lifecycle config file
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases]
default = ["init", "setup", "build", "test"]
development = ["init", "setup", "build", "test", "dev"]
production = ["build", "test", "docker", "deploy"]
```

### Marketplace Configuration

Configure marketplace and gpack management:

```toml
[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"
offline_mode = false

[registry]
url = "https://registry.ggen.io"  # Registry URL
token = "${GGEN_REGISTRY_TOKEN}" # Auth token (use env var)
cache_dir = ".ggen/cache"
cache_ttl = "1h"                # Cache TTL

[gpacks]
install_dir = ".ggen/gpacks/"  # Installation directory
update_policy = "compatible"   # compatible, latest, pinned
resolve_deps = true            # Resolve dependencies
lock_versions = true           # Lock versions
```

**Update Policies:**
- `compatible`: Update to latest compatible versions (semver `^`)
- `latest`: Update to latest versions regardless of compatibility
- `pinned`: Never update automatically, require manual updates

### Security Configuration

Configure security features:

```toml
[security]
validate_paths = true              # Validate file paths
block_shell_injection = true       # Block shell injection attempts
require_confirmation = false       # Require confirmation for operations
audit_operations = true            # Audit all operations
backup_before_write = true         # Backup before file writes
path_traversal_protection = true   # Protect against path traversal
template_sandboxing = true         # Sandbox template execution
```

### Logging Configuration

Configure logging behavior:

```toml
[logging]
level = "info"                     # trace, debug, info, warn, error
format = "json"                    # pretty, json, compact
output = "file"                    # stdout, file, both
file_path = ".ggen/logs/ggen.log" # Log file path
max_file_size = "10MB"             # Max log file size
max_files = 5                      # Max rotated log files
rotation = "daily"                 # daily, hourly, never
color = true                       # Enable ANSI colors (stdout only)
```

### Performance Configuration

Configure performance and resource limits:

```toml
[performance]
enable_profiling = false           # Enable performance profiling
profile_output = ".ggen/profiles/" # Profile output directory
memory_limit_mb = 512              # Memory limit (MB)
cpu_limit_percent = 80             # CPU limit (%)
parallel_execution = true          # Enable parallel execution
max_parallel_templates = 4         # Max parallel templates
max_workers = 16                   # Max worker threads
cache_size = "1GB"                 # Cache size limit
http_pool_size = 10                # HTTP connection pool size
retry_attempts = 3                # Request retry attempts
retry_backoff_ms = 1000            # Retry backoff (ms)
```

### Environment-Specific Overrides

Configure environment-specific settings:

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

Activate environment: `export GGEN_ENVIRONMENT=production`

### Template-Specific Configuration

Configure individual templates:

```toml
[templates.rust-service]
description = "Complete Rust microservice template"
variables = { language = "rust", framework = "axum" }
output_pattern = "generated/src/services/{name}.rs"
backup_enabled = true

[templates.api-endpoint]
description = "REST API endpoint template"
variables = { method = "GET", path = "/api/v1" }
output_pattern = "generated/src/api/{name}.rs"

[templates.database-schema]
description = "Database schema from RDF"
variables = { database = "postgresql", orm = "sqlx" }
output_pattern = "generated/src/database/{name}.rs"
```

### Additional Sections

Additional configuration sections supported:

```toml
# Pipeline configuration
[pipeline]
parallel_execution = true
max_parallel_templates = 4
timeout_seconds = 300
retry_attempts = 3

# Build configuration
[build]
target = "release"
features = ["default"]
profile = "release"
parallel_jobs = 4

# Test configuration
[test]
framework = "cargo"
parallel = true
timeout_seconds = 300
coverage_enabled = true
coverage_threshold = 80

# Deployment configuration
[deployment]
strategy = "rolling"
health_check_path = "/health"
rollback_enabled = true
max_instances = 3
min_instances = 1

# Monitoring configuration
[monitoring]
metrics_enabled = true
tracing_enabled = true
health_checks_enabled = true
alerting_enabled = true

# Backup configuration
[backup]
enabled = true
strategy = "incremental"
retention_days = 30
compression = true
encryption = false

# Cache configuration
[cache]
enabled = true
strategy = "lru"
max_size_mb = 100
ttl_seconds = 3600
persistent = true

# Feature flags
[features]
ai_generation = true
sparql_queries = true
lifecycle_management = true
template_validation = true
graph_analysis = true
documentation_generation = true
```

## Environment Variables

### Registry Configuration

```bash
# Marketplace registry
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"
export GGEN_REGISTRY_TOKEN="your-token"
export GGEN_REGISTRY_TIMEOUT=30

# Local development
export GGEN_REGISTRY_URL="file://$(pwd)/docs/registry/"
```

### Global Settings

```bash
# Configuration file location
export GGEN_CONFIG_FILE="custom.toml"

# Cache directory
export GGEN_CACHE_DIR="/tmp/ggen-cache"

# Environment selection
export GGEN_ENVIRONMENT="production"

# Debug and tracing
export GGEN_DEBUG=1
export GGEN_TRACE=1
export RUST_LOG=ggen=debug
```

### Marketplace Settings

```bash
# Registry settings
export GGEN_REGISTRY_URL="https://custom-registry.com"
export GGEN_REGISTRY_TOKEN="your-token"
export GGEN_NO_UPDATE=1  # Disable automatic updates
```

### AI Provider Settings

```bash
# Provider selection
export GGEN_LLM_PROVIDER="openai"  # openai, anthropic, ollama, mock

# OpenAI
export OPENAI_API_KEY="sk-..."
export OPENAI_MODEL="gpt-4"
export OPENAI_BASE_URL="https://api.openai.com/v1"
export OPENAI_ORGANIZATION="org-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."
export ANTHROPIC_MODEL="claude-3-5-sonnet-20241022"
export ANTHROPIC_BASE_URL="https://api.anthropic.com"

# Ollama
export OLLAMA_BASE_URL="http://localhost:11434"
export OLLAMA_MODEL="qwen2.5-coder:7b"
export OLLAMA_TIMEOUT=30

# Logging
export GGEN_LOG_LEVEL="info"
export GGEN_LOG_FORMAT="json"
```

## Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **CLI arguments** (`--var key=value`)
2. **Environment variables** (`GGEN_*`, `OPENAI_API_KEY`, etc.)
3. **Project config** (`ggen.toml`)
4. **User config** (`~/.config/ggen/config.toml`)
5. **System config** (`/etc/ggen/config.toml`)
6. **Built-in defaults** (`config/defaults.toml`)

**Note:** For template variables, the precedence is:
1. CLI `--var` arguments
2. Template frontmatter `vars:` section
3. Project `ggen.toml` `[vars]` section
4. Gpack `ggen.toml` variables
5. Environment variables

## Configuration Examples

### Minimal Configuration

```toml
# ggen.toml
[project]
name = "my-project"
version = "0.1.0"
```

### Basic Project

```toml
# ggen.toml
[project]
name = "my-cli-tool"
version = "0.1.0"
description = "A CLI tool generated with ggen"
author = "Jane Smith"
license = "MIT"

[vars]
author = "Jane Smith"
license = "MIT"
description = "A CLI tool generated with ggen"
```

### AI-Focused Project

```toml
# ggen.toml
[project]
name = "ai-microservice"
version = "0.1.0"
description = "AI-powered microservice"

[ai]
provider = "ollama"
model = "qwen2.5-coder:7b"
temperature = 0.7
max_tokens = 4000

[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen2.5-coder:7b"

[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
```

### Comprehensive Project

```toml
# ggen.toml - See examples/advanced-rust-project/ggen.toml
[project]
name = "advanced-rust-project"
version = "1.0.0"
description = "Advanced Rust project demonstrating all ggen features"
author = "ggen-examples"
license = "MIT"
repository = "https://github.com/example/advanced-rust-project"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout_seconds = 30
retry_attempts = 3

[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true

[rdf]
base_iri = "http://example.org/advanced-rust-project/"
default_format = "turtle"
cache_queries = true

[graph]
enable_caching = true
cache_size = 1000
cache_ttl_seconds = 3600

[security]
validate_paths = true
audit_operations = true
require_confirmation = false

[logging]
level = "info"
format = "json"
output = "file"
file_path = ".ggen/logs/ggen.log"

[performance]
enable_profiling = false
memory_limit_mb = 512

[env.development]
ai.model = "gpt-3.5-turbo"
logging.level = "debug"

[env.production]
ai.model = "gpt-4"
logging.level = "warn"
security.require_confirmation = true
```

### Enterprise Configuration

```toml
# ggen.toml
[project]
name = "enterprise-tool"
version = "1.0.0"

[registry]
url = "https://internal-registry.company.com"
token = "${GGEN_REGISTRY_TOKEN}"  # Use environment variable

[gpacks]
update_policy = "pinned"
resolve_deps = true
lock_versions = true

[graph]
allow_remote = false  # Security: no remote RDF

[security]
validate_paths = true
block_shell_injection = true
template_sandboxing = true
audit_operations = true
require_confirmation = true

[logging]
level = "warn"  # Minimal logging in production
format = "json"
output = "file"
file_path = "/var/log/ggen/ggen.log"

[performance]
memory_limit_mb = 1024
cpu_limit_percent = 80
```

## Configuration Validation

Validate your configuration:

```bash
# Validate project configuration (future)
ggen config validate

# Check configuration syntax (future)
ggen config check

# Show effective configuration (future)
ggen config show
```

**Common Configuration Issues:**
- **Invalid TOML syntax**: Check brackets `[]` and quotes `""`
- **Missing required fields**: Ensure `[project]` section with `name`
- **Invalid registry URL**: Verify registry accessibility
- **Permission issues**: Check file and directory permissions
- **Type mismatches**: Ensure numeric values are numbers, not strings

## Best Practices

### Project Configuration

1. **Use semantic versioning**: Follow [semver](https://semver.org/) for project versions
2. **Document custom variables**: Include descriptions for variables in `[vars]` section
3. **Version control**: Commit `ggen.toml` to version control (exclude secrets)
4. **Environment separation**: Use `[env.*]` sections for different environments
5. **Keep it minimal**: Only include sections you actually use

### Security

1. **Never store secrets**: Use environment variables for API keys, tokens, passwords
2. **Validate paths**: Enable `security.validate_paths` to prevent path traversal
3. **Restrict remote RDF**: Set `graph.allow_remote = false` unless needed
4. **Audit operations**: Enable `security.audit_operations` for production
5. **Template sandboxing**: Enable `security.template_sandboxing` for untrusted templates

### Performance

1. **Enable caching**: Use `cache.enabled = true` and appropriate TTLs
2. **Parallel execution**: Enable `performance.parallel_execution` for large projects
3. **Resource limits**: Set appropriate `memory_limit_mb` and `cpu_limit_percent`
4. **Profiling**: Use `performance.enable_profiling` for performance analysis

### Team Collaboration

1. **Shared templates**: Document template usage and requirements
2. **Configuration documentation**: Document custom configuration options
3. **Validation in CI/CD**: Validate configuration in continuous integration
4. **Consistent environments**: Ensure consistent configuration across dev/staging/prod
5. **Team standards**: Establish team-wide configuration templates

## Reference

**Related Documentation:**
- [Configuration Quick Reference](config_quick_reference.md) - Quick lookup for common settings
- [Configuration Migration Plan](config_migration_plan.md) - Migration guide and roadmap
- [Default Configuration](config/defaults.toml) - Complete default configuration reference
- [Templates Guide](templates.md) - Template system documentation
- [Marketplace Guide](MARKETPLACE.md) - Marketplace and gpack documentation

**Example Configurations:**
- [Advanced Rust Project](examples/advanced-rust-project/ggen.toml) - Comprehensive example
- [Microservices Architecture](examples/microservices-architecture/ggen.toml) - Microservices setup
- [AI Code Generation](examples/ai-code-generation/ggen.toml) - AI-focused configuration
- [AI Microservice](examples/ai-microservice/ggen.toml) - AI microservice example

---

**Last Updated:** 2025-01-XX  
**Version:** 2.0.0
