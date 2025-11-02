# ggen.toml Schema Reference

**Quick reference for `ggen.toml` project configuration file structure.**

> 📘 For complete documentation, see [Configuration Guide](./CONFIGURATION_GUIDE.md)

---

## Complete Schema

```toml
# ============================================================================
# Project Metadata
# ============================================================================
[project]
name = "my-project"           # Required: Project name
version = "1.0.0"              # Optional: Semantic version
description = "My project"     # Optional: Project description
author = "John Doe"            # Optional: Author name
license = "MIT"                # Optional: License identifier
repository = "https://github.com/user/project"  # Optional: Repository URL

# ============================================================================
# Template Variables
# ============================================================================
[vars]
# Default variables available to all templates
# Can be overridden via CLI: --var key=value
author = "John Doe"
license = "MIT"
year = "2024"
company = "My Company"

# ============================================================================
# AI Configuration
# ============================================================================
[ai]
provider = "ollama"            # ollama, openai, anthropic, mock, auto
model = "qwen3-coder:30b"      # Model identifier
temperature = 0.7              # 0.0-2.0 (creativity)
max_tokens = 2048              # Maximum output tokens
timeout = 30                   # Request timeout (seconds)
retry_attempts = 3            # Retry attempts

# Provider-specific settings
[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
timeout = 30

[ai.providers.openai]
base_url = "https://api.openai.com/v1"
model = "gpt-3.5-turbo"
organization = ""              # Optional org ID

[ai.providers.anthropic]
base_url = "https://api.anthropic.com"
model = "claude-3-sonnet-20240229"

# AI Prompts
[ai.prompts]
system = "You are an expert Rust developer..."
user_prefix = "Generate a Rust service with the following requirements:"

# AI Validation
[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3

# ============================================================================
# Template Configuration
# ============================================================================
[templates]
directory = "templates"         # Template source directory
output_directory = "generated"  # Generated code destination
backup_enabled = true           # Backup before overwriting
idempotent = true               # Ensure reproducible outputs
dry_run = false                 # Preview without writing

# Template-specific settings
[templates.rust]
style = "core-team"             # Rust style: core-team, idiomatic
error_handling = "thiserror"    # Error handling: thiserror, anyhow
logging = "tracing"             # Logging: tracing, log
async_runtime = "tokio"         # Async runtime: tokio, async-std
testing = "comprehensive"       # Testing level: minimal, standard, comprehensive

[templates.microservice]
framework = "auto-detect"       # auto-detect, axum, actix-web, warp, tonic
database = "postgresql"         # postgresql, mysql, sqlite, mongodb
cache = "redis"                 # redis, memcached
monitoring = "prometheus"        # prometheus, datadog
documentation = "openapi"        # openapi, graphql

# ============================================================================
# RDF Configuration
# ============================================================================
[rdf]
base_uri = "http://example.org/my-project/"  # Base IRI for RDF resources
allow_remote = false                         # Require explicit opt-in for remote RDF
default_format = "turtle"                    # turtle, json-ld, n-triples

# RDF Namespace Prefixes
[rdf.prefixes]
ex = "http://example.org/my-project/"
schema = "http://schema.org/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
owl = "http://www.w3.org/2002/07/owl#"

# ============================================================================
# SPARQL Configuration
# ============================================================================
[sparql]
timeout = 10                   # Query timeout (seconds)
max_results = 1000            # Maximum results per query
cache_enabled = true           # Cache query results
query_timeout_seconds = 10     # Alternative timeout format

# ============================================================================
# Graph Configuration
# ============================================================================
[graph]
enable_caching = true          # Enable graph caching
cache_size = 1000              # Cache size (number of entries)
cache_ttl_seconds = 3600       # Cache TTL (seconds)
enable_tracing = true          # Enable tracing

# ============================================================================
# Pipeline Configuration
# ============================================================================
[pipeline]
parallel_execution = true       # Enable parallel template generation
max_parallel_templates = 4      # Maximum parallel templates
timeout_seconds = 300           # Pipeline timeout
retry_attempts = 3             # Retry attempts

# ============================================================================
# Lifecycle Integration
# ============================================================================
[lifecycle]
enabled = true                 # Enable lifecycle system
config_file = "make.toml"      # Lifecycle config file
cache_directory = ".ggen/cache"  # Lifecycle cache directory
state_file = ".ggen/state.json"  # State tracking file

# Lifecycle Phase Definitions
[lifecycle.phases]
default = ["init", "setup", "build", "test"]
development = ["init", "setup", "build", "test", "dev"]
production = ["build", "test", "docker", "deploy"]

# ============================================================================
# Security Configuration
# ============================================================================
[security]
path_traversal_protection = true    # Prevent directory traversal
shell_injection_protection = true   # Sanitize shell commands
template_sandboxing = true           # Sandbox template execution
validate_paths = true                # Validate all file paths
block_shell_injection = true         # Block shell injection
require_confirmation = false          # Require confirmation for destructive operations
audit_operations = false              # Audit all operations
backup_before_write = true           # Backup before writing
validate_signatures = false           # Validate event signatures

# Security Keys (optional)
[security.keys]
public_key = "ed25519:..."            # Ed25519 public key for verification

# ============================================================================
# Performance Configuration
# ============================================================================
[performance]
parallel_execution = true       # Enable parallel execution
max_workers = 8                # Maximum parallel workers
cache_size = "512MB"           # Cache size (can use units: MB, GB)
enable_profiling = false       # Enable performance profiling
profile_output = ".ggen/profiles/"  # Profiling output directory
memory_limit_mb = 512          # Memory limit (MB)
cpu_limit_percent = 80         # CPU limit (percentage)

# ============================================================================
# Logging Configuration
# ============================================================================
[logging]
level = "info"                 # trace, debug, info, warn, error
format = "pretty"              # pretty, json, compact
output = "stdout"              # stdout, file, both
file = "logs/ggen.log"         # Log file path (if output=file or both)
max_file_size = "10MB"         # Max log file size
max_files = 5                  # Maximum log files (rotation)
rotation = "daily"              # daily, weekly, monthly
color = true                   # Enable ANSI colors

# ============================================================================
# Registry Configuration
# ============================================================================
[registry]
url = "https://seanchatmangpt.github.io/ggen/registry/"  # Registry URL
timeout_seconds = 30            # Request timeout
cache_ttl_hours = 24            # Cache TTL (hours)

# Mirror URLs (fallback registries)
[registry.mirrors]
mirror1 = "https://mirror1.example.com/registry/"
mirror2 = "https://mirror2.example.com/registry/"

# ============================================================================
# GitHub Integration
# ============================================================================
[github]
base_url = "https://api.github.com"      # GitHub API base URL
enterprise_base_url = ""                 # GitHub Enterprise URL (optional)
timeout_seconds = 30                      # Request timeout
user_agent = "ggen-cli"                   # User agent string

# ============================================================================
# File Watching
# ============================================================================
[watch]
default_poll_interval_ms = 1000   # File system polling interval (ms)
default_debounce_ms = 500         # Debounce time (ms)
recursive = false                  # Watch subdirectories
clear_screen = false               # Clear screen on change

# ============================================================================
# CI/CD Configuration
# ============================================================================
[ci.release]
default_timeout_seconds = 1800      # Release workflow timeout
workflow_timeout_seconds = 3600     # GitHub Actions workflow timeout
homebrew_timeout_seconds = 1800    # Homebrew publishing timeout

# ============================================================================
# Features
# ============================================================================
[features]
ai_generation = true           # Enable AI generation
sparql_queries = true          # Enable SPARQL queries
lifecycle_management = true    # Enable lifecycle management
template_validation = true     # Enable template validation
graph_analysis = true          # Enable graph analysis
documentation_generation = true # Enable documentation generation

# ============================================================================
# Environment-Specific Overrides
# ============================================================================
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

---

## Required Fields

Only one field is required:

- `[project]` → `name` (string) - Project name

All other fields are optional and have sensible defaults.

---

## Field Types

| Type | Description | Example |
|------|-------------|---------|
| `string` | Text value | `"my-project"` |
| `integer` | Whole number | `2048` |
| `float` | Decimal number | `0.7` |
| `boolean` | true/false | `true` |
| `array` | List of values | `["init", "setup", "build"]` |
| `table` | Nested configuration | `[ai.providers.ollama]` |
| `string (size)` | Size with units | `"512MB"`, `"1GB"` |
| `string (duration)` | Duration | `"7d"`, `"24h"`, `"3600s"` |

---

## Common Patterns

### Minimal Configuration

```toml
[project]
name = "my-project"
```

### Basic Project

```toml
[project]
name = "my-project"
version = "1.0.0"

[vars]
author = "John Doe"
license = "MIT"
```

### AI-Enabled Project

```toml
[project]
name = "ai-project"

[ai]
provider = "ollama"
model = "qwen3-coder:30b"
temperature = 0.7
```

### Full-Featured Project

See complete schema above or examples in [`examples/advanced-rust-project/ggen.toml`](../examples/advanced-rust-project/ggen.toml).

---

## Validation

### Check Syntax

```bash
# Validate TOML syntax
toml-lint ggen.toml

# Validate ggen.toml (ggen command)
ggen config validate
```

### Common Errors

**❌ Missing Project Name**
```toml
# Error: [project] section required with 'name' field
[project]
# name = "my-project"  # Missing!
```

**❌ Invalid Table Path**
```toml
# Error: Can't use [ai.providers.ollama.model] without parent table
[ai.providers.ollama.model]  # Wrong!
model = "qwen3-coder:30b"

# Correct:
[ai.providers.ollama]
model = "qwen3-coder:30b"
```

**❌ Type Mismatch**
```toml
# Error: temperature must be float, not string
[ai]
temperature = "0.7"  # Wrong! Should be: temperature = 0.7
```

---

## Examples

See real-world examples in:
- [`examples/advanced-rust-project/ggen.toml`](../examples/advanced-rust-project/ggen.toml)
- [`examples/microservices-architecture/ggen.toml`](../examples/microservices-architecture/ggen.toml)
- [`marketplace/packages/*/ggen.toml`](../marketplace/packages/)

---

## Related Documentation

- **[Configuration Guide](./CONFIGURATION_GUIDE.md)** - Complete configuration documentation
- **[Configuration Quick Reference](./config_quick_reference.md)** - Environment variables
- **[make.toml Schema](./make-toml-complete-example.md)** - Lifecycle configuration

---

*Last Updated: December 2025 | ggen v2.2.0*
