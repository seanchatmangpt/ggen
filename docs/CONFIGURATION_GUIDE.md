# ggen Configuration Guide

**Complete reference for configuring ggen projects and the ggen CLI itself.**

This guide consolidates all configuration documentation into a single, authoritative source. It covers project configuration (`ggen.toml`), lifecycle configuration (`make.toml`), environment variables, defaults, and best practices.

---

## Table of Contents

1. [Configuration Overview](#configuration-overview)
2. [Project Configuration (`ggen.toml`)](#project-configuration-ggentoml)
3. [Lifecycle Configuration (`make.toml`)](#lifecycle-configuration-maketoml)
4. [CLI Configuration](#cli-configuration)
5. [Environment Variables](#environment-variables)
6. [Configuration Precedence](#configuration-precedence)
7. [Configuration Examples](#configuration-examples)
8. [Best Practices](#best-practices)
9. [Reference](#reference)

---

## Configuration Overview

ggen uses a layered configuration system with clear precedence rules:

### Configuration Files

1. **`ggen.toml`** - Project-specific configuration (for ggen-generated projects)
   - Located in project root
   - Defines project metadata, AI settings, templates, RDF, and generation options
   - Used when generating code from templates

2. **`make.toml`** - Lifecycle configuration (for any project)
   - Located in project root
   - Defines lifecycle phases, hooks, and workflows
   - Universal project descriptor (like `package.json` but language-agnostic)

3. **`~/.config/ggen/config.toml`** - User configuration
   - User-specific defaults
   - Overrides system defaults

4. **`/etc/ggen/config.toml`** - System configuration (Linux)
   - System-wide defaults
   - Lowest priority (before built-in defaults)

5. **`config/defaults.toml`** - Built-in defaults (in ggen source)
   - Fallback defaults
   - No need to modify (reference only)

### Configuration Precedence (Highest to Lowest)

1. **Command-line arguments** (highest priority)
2. **Environment variables** (`GGEN_*`, `OLLAMA_*`, `OPENAI_*`, etc.)
3. **Project config** (`./ggen.toml`)
4. **User config** (`~/.config/ggen/config.toml`)
5. **System config** (`/etc/ggen/config.toml` or macOS/Windows equivalents)
6. **Built-in defaults** (`config/defaults.toml`)

---

## Project Configuration (`ggen.toml`)

The `ggen.toml` file is used in ggen-generated projects to configure code generation, AI integration, templates, RDF graphs, and more.

### Basic Structure

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "My ggen project"
author = "John Doe"
license = "MIT"
repository = "https://github.com/user/my-project"

[vars]
# Default variables available to all templates
author = "John Doe"
license = "MIT"
year = "2024"
company = "My Company"

[ai]
provider = "ollama"  # ollama, openai, anthropic, mock
model = "qwen3-coder:30b"
temperature = 0.7
max_tokens = 2048
timeout = 30

[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true
idempotent = true

[rdf]
base_uri = "http://example.org/my-project/"
prefixes = { 
    ex = "http://example.org/my-project/",
    schema = "http://schema.org/",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
}

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true

[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true

[performance]
parallel_execution = true
max_workers = 8
cache_size = "512MB"

[logging]
level = "info"
format = "pretty"  # pretty, json, compact
file = "logs/ggen.log"
```

### Section Reference

#### `[project]`

Project metadata used throughout generation.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Project name (used in templates) |
| `version` | string | No | Semantic version |
| `description` | string | No | Project description |
| `author` | string | No | Author name |
| `license` | string | No | License identifier |
| `repository` | string | No | Repository URL |

#### `[vars]`

Default variables available to all templates. These can be overridden via CLI flags (`--var key=value`).

#### `[ai]`

AI generation configuration.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `provider` | string | `"auto"` | LLM provider: `ollama`, `openai`, `anthropic`, `mock` |
| `model` | string | `"qwen3-coder:30b"` | Model identifier |
| `temperature` | float | `0.7` | Creativity (0.0-2.0) |
| `max_tokens` | integer | `2048` | Maximum output tokens |
| `timeout` | integer | `30` | Request timeout (seconds) |

**Provider-Specific Settings:**

```toml
[ai.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
timeout = 30

[ai.providers.openai]
base_url = "https://api.openai.com/v1"
model = "gpt-3.5-turbo"
organization = ""  # Optional org ID

[ai.providers.anthropic]
base_url = "https://api.anthropic.com"
model = "claude-3-sonnet-20240229"
```

**AI Prompts:**

```toml
[ai.prompts]
system = "You are an expert Rust developer. Generate production-ready code."
user_prefix = "Generate a Rust service with the following requirements:"

[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3
```

#### `[templates]`

Template generation settings.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `directory` | string | `"templates"` | Template source directory |
| `output_directory` | string | `"generated"` | Generated code destination |
| `backup_enabled` | bool | `true` | Backup existing files before overwriting |
| `idempotent` | bool | `true` | Ensure reproducible outputs |

**Template-Specific Configuration:**

```toml
[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"

[templates.microservice]
framework = "auto-detect"  # axum, actix-web, warp, tonic
database = "postgresql"
cache = "redis"
monitoring = "prometheus"
documentation = "openapi"
```

#### `[rdf]`

RDF graph configuration.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `base_uri` | string | - | Base IRI for RDF resources |
| `prefixes` | table | - | RDF namespace prefixes |

#### `[sparql]`

SPARQL query settings.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `timeout` | integer | `10` | Query timeout (seconds) |
| `max_results` | integer | `1000` | Maximum results per query |
| `cache_enabled` | bool | `true` | Cache query results |

#### `[lifecycle]`

Lifecycle management integration.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | bool | `true` | Enable lifecycle system |
| `config_file` | string | `"make.toml"` | Lifecycle config file |
| `cache_directory` | string | `".ggen/cache"` | Lifecycle cache directory |
| `state_file` | string | `".ggen/state.json"` | State tracking file |

#### `[security]`

Security settings.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `path_traversal_protection` | bool | `true` | Prevent directory traversal |
| `shell_injection_protection` | bool | `true` | Sanitize shell commands |
| `template_sandboxing` | bool | `true` | Sandbox template execution |

#### `[performance]`

Performance tuning.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `parallel_execution` | bool | `true` | Enable parallel template generation |
| `max_workers` | integer | `8` | Maximum parallel workers |
| `cache_size` | string | `"512MB"` | Cache size limit |

#### `[logging]`

Logging configuration.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `level` | string | `"info"` | Log level: `trace`, `debug`, `info`, `warn`, `error` |
| `format` | string | `"pretty"` | Format: `pretty`, `json`, `compact` |
| `file` | string | - | Optional log file path |

### Environment-Specific Overrides

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

---

## Lifecycle Configuration (`make.toml`)

The `make.toml` file defines lifecycle phases, hooks, and workflows. It's a universal project descriptor that works across all languages and frameworks.

### Basic Structure

```toml
[project]
name = "my-app"
version = "1.0.0"
description = "My application"
license = "MIT"
authors = ["Your Name <you@example.com>"]
repository = "https://github.com/user/my-app"

[lifecycle.init]
description = "Initialize project"
commands = [
    "echo 'Initializing...'",
    "npm install",
]
first_run_only = true

[lifecycle.setup]
description = "Install dependencies"
commands = [
    "npm install",
    "cargo fetch",
]
parallel = true

[lifecycle.dev]
description = "Start development server"
command = "npm run dev"
watch = true
port = 3000

[lifecycle.build]
description = "Build for production"
commands = [
    "npm run build",
    "cargo build --release",
]
outputs = ["dist/", "target/release/"]
cache = true

[lifecycle.test]
description = "Run tests"
commands = [
    "npm test",
    "cargo test",
]
parallel = true

[hooks]
before_all = "validate-env"
before_build = ["test", "lint"]
after_build = "analyze-bundle"
before_deploy = ["test:e2e", "check-migrations"]

[env.development]
NODE_ENV = "development"
RUST_LOG = "debug"
API_URL = "http://localhost:3000"

[env.production]
NODE_ENV = "production"
RUST_LOG = "info"
API_URL = "https://api.example.com"
```

### Lifecycle Phases

Standard phases available to all projects:

- **`init`** - Initialize project structure
- **`setup`** - Install dependencies
- **`dev`** - Development server (hot reload)
- **`build`** - Production build
- **`test`** - Run tests
- **`lint`** - Code quality checks
- **`format`** - Code formatting
- **`deploy`** - Deployment
- **`clean`** - Cleanup artifacts
- **`docs`** - Generate documentation

### Phase Configuration

#### Single Command

```toml
[lifecycle.dev]
description = "Start dev server"
command = "npm run dev"
watch = true
port = 3000
```

#### Multiple Commands

```toml
[lifecycle.build]
description = "Build for production"
commands = [
    "echo '🔨 Building...'",
    "npm run build",
    "cargo build --release",
    "echo '✅ Done!'"
]
outputs = ["dist/", "target/release/"]
cache = true
```

#### Parallel Execution

```toml
[lifecycle.test]
description = "Run tests"
parallel = true

[lifecycle.test.workspaces.frontend]
command = "npm test"

[lifecycle.test.workspaces.backend]
command = "cargo test"
```

#### Custom Phases

```toml
[lifecycle."generate:api"]
description = "Generate API endpoint"
commands = [
    "ggen gen api {{name}} --workspace backend",
    "ggen gen composable use{{name}} --workspace frontend",
]
```

### Hooks System

Hooks run before/after phases:

```toml
[hooks]
# Global hooks (run for all phases)
before_all = "validate-env"
after_all = "notify-complete"

# Phase-specific hooks
before_init = ["check-system-deps", "backup-config"]
after_init = ["setup-git", "setup-vscode"]
before_dev = ["check-ports", "ensure-db-running"]
after_dev = "cleanup-temp"
before_build = ["test", "lint", "sync-types"]
after_build = ["analyze-bundle", "optimize-assets"]
before_deploy = ["test:e2e", "check-migrations", "backup-db"]
after_deploy = ["smoke-test", "notify-slack"]

# Error handling
on_error = "rollback-last"
on_success = "celebrate"
```

### Workspaces (Multi-Language Projects)

```toml
[workspace.frontend]
path = "apps/web"
runtime = "node:20.10.0"
package_manager = "pnpm"
framework = "nuxt"
framework_version = "4.0.0"
language = "typescript"

[workspace.backend]
path = "apps/api"
runtime = "rust:1.75.0"
package_manager = "cargo"
framework = "axum"
framework_version = "0.7.0"
language = "rust"
```

### Environment Configuration

```toml
[env.development]
API_URL = "http://localhost:4000"
DATABASE_URL = "postgres://localhost/myapp_dev"
RUST_LOG = "debug"
NODE_ENV = "development"

[env.staging]
API_URL = "https://staging-api.example.com"
DATABASE_URL = "postgres://staging-db.example.com/myapp"
RUST_LOG = "info"
NODE_ENV = "production"

[env.production]
API_URL = "https://api.example.com"
DATABASE_URL = "postgres://prod-db.example.com/myapp"
RUST_LOG = "warn"
NODE_ENV = "production"
```

### State Management

```toml
[state]
file = ".ggen/state.json"
track_generated = true
track_migrations = true

cache_dir = ".ggen/cache"
cache_ttl = "7d"

# Idempotency
skip_if_exists = [
    "node_modules",
    "target",
    ".nuxt",
]

# Force re-run if these files change
force_rerun_if_changed = [
    "make.toml",
    "ggen.toml",
    "package.json",
    "Cargo.toml",
]
```

See [`make-toml-complete-example.md`](./make-toml-complete-example.md) for a complete, real-world example.

---

## CLI Configuration

### Configuration File Locations

**Linux:**
- System: `/etc/ggen/config.toml`
- User: `~/.config/ggen/config.toml` or `$XDG_CONFIG_HOME/ggen/config.toml`

**macOS:**
- System: `/Library/Application Support/ggen/config.toml`
- User: `~/Library/Application Support/ggen/config.toml` or `~/.config/ggen/config.toml`

**Windows:**
- System: `C:\ProgramData\ggen\config.toml`
- User: `%APPDATA%\ggen\config.toml` or `%USERPROFILE%\.config\ggen\config.toml`

### Default Configuration Reference

See [`config/defaults.toml`](../config/defaults.toml) for complete default values. Key sections include:

- `[llm]` - LLM provider configuration
- `[github]` - GitHub API integration
- `[registry]` - Package registry settings
- `[watch]` - File watching configuration
- `[ci]` - CI/CD settings
- `[generation]` - Code generation parameters
- `[security]` - Security settings
- `[logging]` - Logging configuration
- `[performance]` - Performance tuning
- `[dev]` - Development/test mode settings

---

## Environment Variables

Environment variables override configuration files and provide runtime configuration.

### LLM Providers

```bash
# Provider Selection
export GGEN_LLM_PROVIDER=ollama  # ollama, openai, anthropic, mock

# Ollama (Local AI)
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
export OLLAMA_TIMEOUT=30

# OpenAI
export OPENAI_API_KEY=sk-...  # REQUIRED for OpenAI
export OPENAI_BASE_URL=https://api.openai.com/v1
export OPENAI_MODEL=gpt-3.5-turbo
export OPENAI_ORGANIZATION=org-...  # Optional

# Anthropic
export ANTHROPIC_API_KEY=sk-ant-...  # REQUIRED for Anthropic
export ANTHROPIC_BASE_URL=https://api.anthropic.com
export ANTHROPIC_MODEL=claude-3-sonnet-20240229
```

### GitHub Integration

```bash
# GitHub API
export GITHUB_TOKEN=ghp_...  # REQUIRED for private repos
export GH_TOKEN=ghp_...  # Alternative to GITHUB_TOKEN
export GGEN_GITHUB_BASE_URL=https://api.github.com
export GGEN_GITHUB_TIMEOUT=30

# GitHub Enterprise
export GGEN_GITHUB_BASE_URL=https://github.company.com/api/v3
```

### Package Registry

```bash
# Registry Configuration
export GGEN_REGISTRY_URL=https://seanchatmangpt.github.io/ggen/registry/
export GGEN_REGISTRY_TIMEOUT=30
```

### Code Generation

```bash
# Generation Parameters
export GGEN_GENERATION_TEMPERATURE=0.7  # 0.0-2.0 (creativity)
export GGEN_GENERATION_MAX_TOKENS=4096  # Max output length
export GGEN_GENERATION_TOP_P=0.9  # 0.0-1.0 (diversity)
export GGEN_GENERATION_STREAMING=false  # Enable streaming
```

### Logging

```bash
# Log Configuration
export GGEN_LOG_LEVEL=info  # trace, debug, info, warn, error
export RUST_LOG=ggen=debug  # Alternative (Rust standard)
export GGEN_LOG_FORMAT=pretty  # pretty, json, compact
```

### Development/Testing

```bash
# Test Mode (uses mock clients, no API calls)
export GGEN_TEST_MODE=1
export GGEN_ALLOW_LIVE_CALLS=0  # Disable live API calls in tests
export GGEN_DEV_MODE=1  # Enable development mode
```

See [`config_quick_reference.md`](./config_quick_reference.md) for a complete environment variable reference.

---

## Configuration Precedence

Configuration is resolved in this order (later values override earlier):

1. **Command-line arguments** (highest priority)
   ```bash
   ggen template generate --var author="Jane Doe" --log-level debug
   ```

2. **Environment variables**
   ```bash
   export GGEN_LOG_LEVEL=debug
   ggen template generate
   ```

3. **Project config** (`./ggen.toml`)
   ```toml
   [logging]
   level = "info"
   ```

4. **User config** (`~/.config/ggen/config.toml`)
   ```toml
   [logging]
   level = "warn"
   ```

5. **System config** (`/etc/ggen/config.toml` or platform equivalent)
   ```toml
   [logging]
   level = "error"
   ```

6. **Built-in defaults** (`config/defaults.toml`)
   ```toml
   [logging]
   level = "info"
   ```

### Variable Precedence for Templates

For template variables:

1. **CLI arguments** (`--var key=value`)
2. **Environment variables** (`GGEN_VAR_*`)
3. **Project config** (`ggen.toml` `[vars]` section)
4. **Gpack variables** (from installed gpacks)
5. **Template frontmatter** (`vars:` section in template)
6. **Built-in defaults**

---

## Configuration Examples

### Basic Project

```toml
# ggen.toml
[project]
name = "my-cli-tool"
version = "0.1.0"

[vars]
author = "Jane Smith"
license = "MIT"
description = "A CLI tool generated with ggen"
```

### Multi-Language Project

```toml
# ggen.toml
[project]
name = "multi-lang-api"
version = "0.1.0"

[vars]
author = "Team Name"
license = "Apache-2.0"

[rdf]
base = "http://api.example.org/"
prefixes.api = "http://api.example.org/"

# make.toml
[project]
name = "multi-lang-api"

[workspace.frontend]
path = "apps/web"
runtime = "node:20"
framework = "next"

[workspace.backend]
path = "apps/api"
runtime = "rust:1.75"
framework = "axum"

[lifecycle.dev]
parallel = true

[lifecycle.dev.workspaces.frontend]
command = "npm run dev"

[lifecycle.dev.workspaces.backend]
command = "cargo watch -x run"
```

### Enterprise Configuration

```toml
# ggen.toml
[project]
name = "enterprise-tool"
version = "1.0.0"

[registry]
url = "https://internal-registry.company.com"
token = "${GGEN_REGISTRY_TOKEN}"  # From env var

[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true

[rdf]
allow_remote = false  # Security: no remote RDF

[env.production]
security.validate_signatures = true
security.audit_operations = true
logging.level = "warn"
```

### Advanced Rust Project

```toml
# ggen.toml
[project]
name = "advanced-rust-project"
version = "1.0.0"
description = "Advanced Rust project demonstrating all ggen features"
author = "ggen-examples"
license = "MIT"

[templates]
source_dir = "templates"
output_dir = "generated"
backup_enabled = true
idempotent = true

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout_seconds = 30

[rdf]
base_iri = "http://example.org/advanced-rust-project/"
default_format = "turtle"
cache_queries = true
query_timeout_seconds = 10

[graph]
enable_caching = true
cache_size = 1000
cache_ttl_seconds = 3600

[pipeline]
parallel_execution = true
max_parallel_templates = 4
timeout_seconds = 300

[security]
validate_paths = true
block_shell_injection = true
require_confirmation = false
audit_operations = true

[logging]
level = "info"
format = "json"
file = ".ggen/logs/ggen.log"

[performance]
enable_profiling = false
profile_output = ".ggen/profiles/"
memory_limit_mb = 512
```

See examples in [`examples/`](../examples/) for more complete examples.

---

## Best Practices

### Project Configuration (`ggen.toml`)

1. **Version Control** - Commit `ggen.toml` to version control
2. **Document Variables** - Include descriptions for custom variables
3. **Environment Separation** - Use `[env.*]` sections for different environments
4. **Security** - Never commit API keys or secrets; use environment variables
5. **Semantic Versioning** - Follow semver for project versions

### Lifecycle Configuration (`make.toml`)

1. **Standard Phases** - Use standard phase names (`init`, `setup`, `dev`, `build`, `test`, `deploy`)
2. **Idempotency** - Ensure phases can be run multiple times safely
3. **Documentation** - Include `description` for all phases
4. **Caching** - Use `cache = true` and `outputs` for build phases
5. **Parallel Execution** - Enable `parallel = true` for independent phases

### Environment Variables

1. **`.env` Files** - Use `.env` files for local development (add to `.gitignore`)
2. **Secret Managers** - Use secret managers in production (AWS Secrets Manager, HashiCorp Vault, etc.)
3. **No Hardcoding** - Never hardcode secrets in configuration files
4. **Validation** - Validate required environment variables at startup

### Security

1. **Path Traversal** - Always enable `path_traversal_protection = true`
2. **Shell Injection** - Enable `shell_injection_protection = true`
3. **Template Sandboxing** - Enable `template_sandboxing = true`
4. **API Keys** - Store API keys in environment variables, never in files
5. **File Permissions** - Restrict config file permissions: `chmod 600 ~/.config/ggen/config.toml`

### Team Configuration

1. **Shared Templates** - Use team-wide configuration templates
2. **Documentation** - Document custom configuration options
3. **CI/CD Validation** - Validate configuration in CI/CD pipelines
4. **Consistency** - Ensure consistent configuration across environments

---

## Reference

### Quick Reference Documents

- [`config_quick_reference.md`](./config_quick_reference.md) - Environment variables cheat sheet
- [`make-toml-complete-example.md`](./make-toml-complete-example.md) - Complete `make.toml` example
- [`LIFECYCLE_QUICK_REFERENCE.md`](./LIFECYCLE_QUICK_REFERENCE.md) - Lifecycle commands quick reference

### Configuration Files

- [`config/defaults.toml`](../config/defaults.toml) - Complete default configuration reference
- [`make.toml`](../make.toml) - ggen project lifecycle configuration (real example)

### Examples

- [`examples/advanced-rust-project/ggen.toml`](../examples/advanced-rust-project/ggen.toml) - Advanced Rust project
- [`examples/microservices-architecture/ggen.toml`](../examples/microservices-architecture/ggen.toml) - Microservices example
- [`marketplace/packages/*/ggen.toml`](../marketplace/packages/) - Marketplace package examples

### Related Documentation

- [`GGEN_TOML_SCHEMA.md`](./GGEN_TOML_SCHEMA.md) - **Schema reference** for `ggen.toml` structure
- [`config.md`](./config.md) - Legacy configuration documentation (see this guide for updated info)
- [`lifecycle.md`](./lifecycle.md) - Lifecycle system overview
- [`templates.md`](./templates.md) - Template development guide

---

## Validation

### Validate Configuration

```bash
# Validate project configuration
ggen config validate

# Check configuration syntax
ggen config check

# Show effective configuration
ggen config show
```

### Common Issues

**❌ Invalid TOML Syntax**
- Check brackets and quotes
- Use TOML validator: https://www.toml-lint.com/

**❌ Missing Required Fields**
- Ensure `[project]` section with at least `name` field

**❌ Invalid Registry URL**
- Verify registry accessibility
- Check network connectivity

**❌ Permission Issues**
- Check file and directory permissions
- Ensure read access to config files

---

*Last Updated: December 2025 | ggen v2.2.0*
