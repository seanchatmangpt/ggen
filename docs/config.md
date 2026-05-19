<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Configuration](#configuration)
  - [Configuration File Locations](#configuration-file-locations)
  - [Configuration Priority](#configuration-priority)
  - [Project Configuration: `ggen.toml`](#project-configuration-ggentoml)
    - [Basic Structure](#basic-structure)
    - [Project Metadata](#project-metadata)
    - [Template Variables](#template-variables)
    - [Template Configuration](#template-configuration)
    - [AI Configuration](#ai-configuration)
    - [RDF and Graph Configuration](#rdf-and-graph-configuration)
    - [SPARQL Configuration](#sparql-configuration)
    - [Lifecycle Configuration](#lifecycle-configuration)
    - [Marketplace Configuration](#marketplace-configuration)
    - [Registry Configuration](#registry-configuration)
    - [Security Configuration](#security-configuration)
    - [Performance Configuration](#performance-configuration)
    - [Logging Configuration](#logging-configuration)
    - [Path Configuration](#path-configuration)
    - [Determinism Configuration](#determinism-configuration)
    - [Data Sources and Queries](#data-sources-and-queries)
    - [Build Configuration](#build-configuration)
    - [Test Configuration](#test-configuration)
    - [Deployment Configuration](#deployment-configuration)
    - [Monitoring Configuration](#monitoring-configuration)
    - [Backup Configuration](#backup-configuration)
    - [Cache Configuration](#cache-configuration)
    - [Features Configuration](#features-configuration)
    - [Environment-Specific Configuration](#environment-specific-configuration)
  - [Environment Variables](#environment-variables)
    - [Registry and Marketplace](#registry-and-marketplace)
    - [Global Settings](#global-settings)
    - [LLM Providers](#llm-providers)
  - [Variable Precedence](#variable-precedence)
  - [Complete Example](#complete-example)
  - [Configuration Validation](#configuration-validation)
  - [Best Practices](#best-practices)
  - [Migration from Legacy Formats](#migration-from-legacy-formats)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Configuration

ggen supports comprehensive configuration through project files (`ggen.toml`) and environment variables, following core team best practices with typed configuration management.

## Configuration File Locations

Configuration files are loaded in priority order:

1. **Project config**: `./ggen.toml` (project root)
2. **User config**: `~/.config/ggen/config.toml` (user-specific defaults)
3. **System config**: `/etc/ggen/config.toml` (system-wide defaults)
4. **Built-in defaults**: Embedded in ggen binary

## Configuration Priority

Configuration values are resolved in this order (later values override earlier):

1. **Command-line arguments** (highest priority)
2. **Environment variables** (e.g., `GGEN_*`, `OLLAMA_*`, `OPENAI_*`)
3. **Project config** (`./ggen.toml`)
4. **User config** (`~/.config/ggen/config.toml`)
5. **System config** (`/etc/ggen/config.toml`)
6. **Built-in defaults** (lowest priority)

## Project Configuration: `ggen.toml`

The `ggen.toml` file is the primary configuration file for ggen projects. It supports extensive customization across all aspects of code generation, templates, AI integration, RDF processing, and lifecycle management.

### Basic Structure

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "My ggen project"
author = "John Doe"
license = "MIT"
repository = "https://github.com/example/my-project"
```

### Project Metadata

The `[project]` section defines basic project information:

```toml
[project]
name = "my-project"              # Project name (required)
version = "0.1.0"               # Semantic version (required)
description = "Project description"
author = "Author Name"          # Author or maintainer
authors = ["Author 1", "Author 2"]  # Multiple authors
license = "MIT"                 # License identifier
repository = "https://github.com/user/repo"  # Repository URL
```

### Template Variables

The `[vars]` section defines default variables available to all templates:

```toml
[vars]
# Default variables available to all templates
author = "John Doe"
license = "MIT"
year = "2024"
company = "Acme Corp"
project_url = "https://example.com"
```

These variables can be overridden in template frontmatter or CLI arguments.

### Template Configuration

#### Basic Template Settings

```toml
[templates]
source_dir = "templates"        # Template source directory
output_dir = "generated"        # Generated code output directory
backup_enabled = true           # Create backups before overwriting
idempotent = true                # Ensure idempotent generation
dry_run = false                  # Preview without writing files
```

#### Language-Specific Template Settings

```toml
[templates.rust]
style = "core-team"             # Code style: core-team, standard, custom
error_handling = "thiserror"     # Error handling: thiserror, anyhow, custom
logging = "tracing"              # Logging: tracing, log, custom
async_runtime = "tokio"          # Async runtime: tokio, async-std, custom
testing = "comprehensive"        # Testing: comprehensive, minimal, custom
documentation = "comprehensive"  # Documentation level
```

#### Template-Specific Configurations

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

### AI Configuration

#### Basic AI Settings

```toml
[ai]
provider = "openai"             # Provider: openai, anthropic, ollama, mock
model = "gpt-4"                 # Model name (varies by provider)
temperature = 0.7               # Creativity (0.0-2.0)
max_tokens = 2000               # Maximum output tokens
timeout_seconds = 30            # Request timeout
retry_attempts = 3              # Retry on failure
```

#### AI Prompts

```toml
[ai.prompts]
system = "You are an expert Rust developer. Generate production-ready code."
user_prefix = "Generate a Rust service with the following requirements:"
```

#### AI Validation

```toml
[ai.validation]
enabled = true                  # Enable code validation
quality_threshold = 0.8          # Quality score threshold (0.0-1.0)
max_iterations = 3              # Maximum validation iterations
```

#### AI Generation Options

```toml
[ai.generation]
auto_improve = true             # Automatically improve generated code
include_tests = true            # Generate test code
include_docs = true             # Generate documentation
include_examples = true         # Generate usage examples
```

### RDF and Graph Configuration

```toml
[rdf]
base_iri = "http://example.org/my-project/"  # Base IRI for RDF resources
default_format = "turtle"       # Default RDF format: turtle, n3, json-ld
cache_queries = true            # Cache SPARQL query results
query_timeout_seconds = 10      # SPARQL query timeout

[rdf.prefixes]
ex = "http://example.org/my-project/"
schema = "http://schema.org/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
```

#### Graph Configuration

```toml
[graph]
base_iri = "http://example.org/my-project/"  # Base IRI
allow_remote = false            # Require explicit opt-in for remote RDF
enable_caching = true           # Enable graph caching
cache_size = 1000               # Maximum cache entries
cache_ttl_seconds = 3600        # Cache time-to-live
enable_tracing = true           # Enable graph operation tracing
```

### SPARQL Configuration

```toml
[sparql]
timeout = 10                    # Query timeout in seconds
max_results = 1000              # Maximum query results
cache_enabled = true            # Enable query result caching
```

### Lifecycle Configuration

```toml
[lifecycle]
enabled = true                  # Enable lifecycle management
config_file = "make.toml"       # Lifecycle configuration file
cache_directory = ".ggen/cache" # Cache directory
state_file = ".ggen/state.json"  # State tracking file

[lifecycle.phases]
default = ["init", "setup", "build", "test"]
development = ["init", "setup", "build", "test", "dev"]
production = ["build", "test", "docker", "deploy"]
```

### Marketplace Configuration

```toml
[marketplace]
registry_url = "https://registry.ggen.dev"  # Marketplace registry URL
cache_dir = ".ggen/marketplace"              # Marketplace cache directory
offline_mode = false                         # Work offline without registry
```

### Registry Configuration

```toml
[registry]
url = "https://registry.ggen.io"  # Registry URL (default: GitHub Pages)
token = "${GGEN_REGISTRY_TOKEN}"   # Authentication token (from env var)
cache_dir = ".ggen/cache"           # Registry cache directory
cache_ttl = "1h"                     # Cache time-to-live

[gpacks]
install_dir = ".ggen/gpacks/"       # Gpack installation directory
update_policy = "compatible"        # Update policy: compatible, latest, pinned
resolve_deps = true                 # Resolve gpack dependencies
lock_versions = true                # Lock gpack versions
```

### Security Configuration

```toml
[security]
validate_paths = true            # Validate file paths for traversal
block_shell_injection = true     # Block shell injection in templates
require_confirmation = false     # Require confirmation for destructive ops
audit_operations = true          # Audit security-sensitive operations
backup_before_write = true        # Backup files before overwriting
path_traversal_protection = true # Enable path traversal protection
shell_injection_protection = true # Enable shell injection protection
template_sandboxing = true        # Sandbox template execution
```

### Performance Configuration

```toml
[performance]
parallel_execution = true        # Enable parallel template execution
max_parallel_templates = 4       # Maximum concurrent template operations
timeout_seconds = 300           # Operation timeout
retry_attempts = 3              # Retry attempts on failure
enable_profiling = false        # Enable performance profiling
profile_output = ".ggen/profiles/"  # Profiling output directory
memory_limit_mb = 512           # Memory limit in MB
cpu_limit_percent = 80          # CPU limit percentage
max_workers = 16                # Maximum worker threads
cache_size = "1GB"               # Cache size limit
```

### Logging Configuration

```toml
[logging]
level = "info"                  # Log level: trace, debug, info, warn, error
format = "json"                  # Format: pretty, json, compact
output = "file"                  # Output: stdout, file, both
file_path = ".ggen/logs/ggen.log"  # Log file path
max_file_size = "10MB"           # Maximum log file size
max_files = 5                    # Maximum log file rotation count
rotation = "daily"                # Rotation: daily, hourly, size-based
```

### Path Configuration

```toml
[paths]
templates = "templates/"         # Template directory
output = "generated/"            # Output directory
cache = ".ggen/"                 # Cache directory
```

### Determinism Configuration

```toml
[determinism]
sort = "name"                    # Default sort key for deterministic output
seed = "project"                 # Seed for reproducible generation
```

### Data Sources and Queries

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

### Build Configuration

```toml
[build]
target = "release"               # Build target: debug, release
features = ["default"]           # Enabled features
profile = "release"              # Build profile
parallel_jobs = 4                # Parallel build jobs
```

### Test Configuration

```toml
[test]
framework = "cargo"              # Test framework: cargo, custom
parallel = true                  # Run tests in parallel
timeout_seconds = 300            # Test timeout
coverage_enabled = true          # Enable coverage collection
coverage_threshold = 80          # Coverage threshold percentage
```

### Deployment Configuration

```toml
[deployment]
strategy = "rolling"            # Deployment strategy
health_check_path = "/health"    # Health check endpoint
rollback_enabled = true         # Enable automatic rollback
max_instances = 3                # Maximum instances
min_instances = 1                # Minimum instances
```

### Monitoring Configuration

```toml
[monitoring]
metrics_enabled = true           # Enable metrics collection
tracing_enabled = true           # Enable distributed tracing
health_checks_enabled = true     # Enable health checks
alerting_enabled = true          # Enable alerting
```

### Backup Configuration

```toml
[backup]
enabled = true                  # Enable automatic backups
strategy = "incremental"        # Backup strategy: incremental, full
retention_days = 30             # Backup retention period
compression = true              # Compress backups
encryption = false              # Encrypt backups
```

### Cache Configuration

```toml
[cache]
enabled = true                  # Enable caching
strategy = "lru"                 # Cache strategy: lru, fifo, custom
max_size_mb = 100                # Maximum cache size
ttl_seconds = 3600              # Cache time-to-live
persistent = true               # Persist cache to disk
```

### Features Configuration

```toml
[features]
ai_generation = true            # Enable AI code generation
sparql_queries = true            # Enable SPARQL queries
lifecycle_management = true      # Enable lifecycle management
template_validation = true       # Enable template validation
graph_analysis = true           # Enable graph analysis
documentation_generation = true  # Enable documentation generation
production_readiness = true      # Enable production readiness checks
code_validation = true          # Enable code validation
test_generation = true          # Enable test generation
```

### Environment-Specific Configuration

Override settings for specific environments:

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

## Environment Variables

### Registry and Marketplace

```bash
# Registry URL
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# Registry authentication
export GGEN_REGISTRY_TOKEN="your-token-here"

# Registry timeout
export GGEN_REGISTRY_TIMEOUT=30
```

### Global Settings

```bash
# Configuration file location
export GGEN_CONFIG_FILE="custom.toml"

# Cache directory
export GGEN_CACHE_DIR="/tmp/ggen-cache"

# Debug mode
export GGEN_DEBUG=1

# Trace mode
export GGEN_TRACE=1

# Log level
export GGEN_LOG_LEVEL=info  # trace, debug, info, warn, error

# Log format
export GGEN_LOG_FORMAT=pretty  # pretty, json, compact

# Test mode
export GGEN_TEST_MODE=1
```

### LLM Providers

```bash
# Provider selection
export GGEN_LLM_PROVIDER=ollama  # openai, anthropic, ollama, mock

# Ollama settings
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
export OLLAMA_TIMEOUT=30

# OpenAI settings
export OPENAI_API_KEY=sk-...
export OPENAI_BASE_URL=https://api.openai.com/v1
export OPENAI_MODEL=gpt-3.5-turbo

# Anthropic settings
export ANTHROPIC_API_KEY=sk-ant-...
export ANTHROPIC_BASE_URL=https://api.anthropic.com
export ANTHROPIC_MODEL=claude-3-sonnet-20240229
```

## Variable Precedence

Template variables are resolved in this order (later values override earlier):

1. **CLI arguments** (`--var key=value`)
2. **Template frontmatter** (`vars:` section)
3. **Gpack variables** (from gpack `ggen.toml`)
4. **Project configuration** (`ggen.toml` `[vars]` section)
5. **User config** (`~/.config/ggen/config.toml` `[vars]` section)
6. **System environment** (`$HOME`, `$USER`, etc.)
7. **Environment variables** (from `.env` files)

## Complete Example

```toml
# ggen.toml - Complete Configuration Example

[project]
name = "advanced-rust-project"
version = "1.0.0"
description = "Advanced Rust project demonstrating all ggen features"
author = "ggen-examples"
license = "MIT"
repository = "https://github.com/example/advanced-rust-project"

[vars]
author = "ggen-examples"
license = "MIT"
year = "2024"

[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true
dry_run = false

[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout_seconds = 30
retry_attempts = 3

[ai.prompts]
system = "You are an expert Rust developer specializing in microservices."

[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3

[rdf]
base_iri = "http://example.org/advanced-rust-project/"
default_format = "turtle"
cache_queries = true
query_timeout_seconds = 10

[graph]
base_iri = "http://example.org/advanced-rust-project/"
allow_remote = false
enable_caching = true
cache_size = 1000
cache_ttl_seconds = 3600
enable_tracing = true

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true

[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases]
default = ["init", "setup", "build", "test"]
development = ["init", "setup", "build", "test", "dev"]
production = ["build", "test", "docker", "deploy"]

[marketplace]
registry_url = "https://registry.ggen.dev"
cache_dir = ".ggen/marketplace"
offline_mode = false

[registry]
url = "https://registry.ggen.io"
token = "${GGEN_REGISTRY_TOKEN}"
cache_dir = ".ggen/cache"
cache_ttl = "1h"

[gpacks]
install_dir = ".ggen/gpacks/"
update_policy = "compatible"
resolve_deps = true
lock_versions = true

[security]
validate_paths = true
block_shell_injection = true
require_confirmation = false
audit_operations = true
backup_before_write = true

[performance]
parallel_execution = true
max_parallel_templates = 4
timeout_seconds = 300
retry_attempts = 3
enable_profiling = false
profile_output = ".ggen/profiles/"
memory_limit_mb = 512
cpu_limit_percent = 80

[logging]
level = "info"
format = "json"
output = "file"
file_path = ".ggen/logs/ggen.log"
max_file_size = "10MB"
max_files = 5

[paths]
templates = "templates/"
output = "generated/"
cache = ".ggen/"

[determinism]
sort = "name"
seed = "project"

[features]
ai_generation = true
sparql_queries = true
lifecycle_management = true
template_validation = true

[env.development]
ai.model = "gpt-3.5-turbo"
ai.temperature = 0.9
logging.level = "debug"

[env.production]
ai.model = "gpt-4"
ai.temperature = 0.3
logging.level = "warn"
security.require_confirmation = true
```

## Configuration Validation

Validate your configuration:

```bash
# Validate project configuration
ggen config validate

# Check configuration syntax
ggen config check

# Show effective configuration (merged from all sources)
ggen config show

# Show configuration with environment variables
ggen config show --env
```

## Best Practices

### Project Configuration

1. **Use semantic versioning**: Follow [semver](https://semver.org/) for project versions
2. **Document variables**: Include descriptions for custom variables in comments
3. **Version control**: Commit `ggen.toml` to version control (but not secrets)
4. **Environment separation**: Use `[env.*]` sections for environment-specific settings
5. **Start simple**: Begin with basic `[project]` and `[vars]` sections, add complexity as needed

### Security

1. **Never commit secrets**: Store API keys and tokens in environment variables
2. **Use token placeholders**: Reference env vars with `${VAR_NAME}` syntax
3. **Enable security features**: Turn on path validation and shell injection protection in production
4. **Audit operations**: Enable audit logging for production environments

### Performance

1. **Enable caching**: Use appropriate cache settings for your use case
2. **Parallel execution**: Enable parallel template processing for large projects
3. **Profile hot paths**: Use profiling to identify bottlenecks
4. **Set limits**: Configure memory and CPU limits to prevent resource exhaustion

### Team Configuration

1. **Shared templates**: Use consistent template configurations across team
2. **Documentation**: Document custom configuration options and their purposes
3. **Validation**: Validate configuration in CI/CD pipelines
4. **Consistency**: Ensure consistent configuration across development, staging, and production

## Migration from Legacy Formats

### From `.ggenrc.yaml`

If you have an existing `.ggenrc.yaml` file, migrate it to `ggen.toml`:

**Old format** (`.ggenrc.yaml`):
```yaml
vars:
  author: "John Doe"
  license: "MIT"

graph:
  allow_remote: false
  base: "http://example.org/"
```

**New format** (`ggen.toml`):
```toml
[vars]
author = "John Doe"
license = "MIT"

[graph]
allow_remote = false
base = "http://example.org/"
```

The `.ggenrc.yaml` format is still supported but deprecated. Migrate to `ggen.toml` for full feature support.