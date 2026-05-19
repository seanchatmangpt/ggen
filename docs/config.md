<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Configuration](#configuration)
  - [Quick Reference](#quick-reference)
  - [Configuration Overview](#configuration-overview)
  - [Quick Start](#quick-start)
    - [Project Configuration (`ggen.toml`)](#project-configuration-ggentoml)
    - [Environment Variables](#environment-variables)
    - [Lifecycle Configuration (`make.toml`)](#lifecycle-configuration-maketoml)
  - [Configuration Sources & Precedence](#configuration-sources--precedence)
  - [Common Configuration Patterns](#common-configuration-patterns)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Configuration

ggen supports configuration through project files, environment variables, and system/user configuration files. This document provides a quick reference - see [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for comprehensive documentation.

## Quick Reference

**Configuration Priority (highest to lowest):**
1. CLI arguments
2. Environment variables
3. Project config (`./ggen.toml`)
4. User config (`~/.config/ggen/config.toml`)
5. System config (`/etc/ggen/config.toml`)
6. Defaults (embedded)

## Configuration Overview

ggen uses a hierarchical configuration system that merges settings from multiple sources in a clear precedence order. Configuration can be set via:

- **Project files** (`ggen.toml` in project root)
- **Lifecycle files** (`make.toml` for lifecycle management)
- **Environment variables** (for sensitive data and overrides)
- **System/user config files** (for defaults)

## Quick Start

### Project Configuration (`ggen.toml`)

Create a `ggen.toml` file in your project root:

```toml
[project]
name = "my-project"
version = "1.0.0"

[templates]
source_dir = "templates"
output_dir = "generated"

[ai]
provider = "ollama"
model = "qwen3-coder:30b"

[rdf]
base_iri = "http://example.org/my-project/"
default_format = "turtle"
```

### Environment Variables

Set sensitive values via environment variables:

```bash
# LLM Provider (Ollama)
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b

# LLM Provider (OpenAI)
export OPENAI_API_KEY=sk-...
export OPENAI_MODEL=gpt-4

# Registry
export GGEN_REGISTRY_URL=https://seanchatmangpt.github.io/ggen/registry/

# Logging
export GGEN_LOG_LEVEL=info
```

### Lifecycle Configuration (`make.toml`)

Configure lifecycle phases in `make.toml`:

```toml
[lifecycle]
name = "my-project"
version = "1.0.0"

[phases.build]
description = "Build release"
commands = ["cargo build --release --all-features"]

[phases.test]
description = "Run tests"
commands = ["cargo test --all-features"]
```

## Configuration Sources & Precedence

Configuration is loaded in this order (later sources override earlier):

1. **System defaults** (embedded in binary)
2. **System config** (`/etc/ggen/config.toml` on Linux, `/Library/Application Support/ggen/config.toml` on macOS)
3. **User config** (`~/.config/ggen/config.toml`)
4. **Project config** (`./ggen.toml`)
5. **Environment variables** (e.g., `GGEN_LLM_PROVIDER`, `OLLAMA_MODEL`)
6. **CLI arguments** (highest precedence)

## Common Configuration Patterns

### Basic Project

```toml
[project]
name = "my-cli-tool"
version = "0.1.0"

[templates]
source_dir = "templates"
output_dir = "generated"
```

### AI-Powered Project

```toml
[project]
name = "ai-service"

[ai]
provider = "ollama"
model = "qwen3-coder:30b"
temperature = 0.7
```

### Multi-Language Project

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
```

### Enterprise Configuration

```toml
[project]
name = "enterprise-tool"

[registry]
url = "https://internal-registry.company.com"
# Token via GGEN_REGISTRY_TOKEN env var

[security]
validate_paths = true
block_shell_injection = true
audit_operations = true
```

See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for complete examples and detailed documentation.

## Related Documentation

- **[CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md)** - Comprehensive configuration documentation
  - Complete `ggen.toml` reference
  - All environment variables
  - `make.toml` lifecycle configuration
  - Security best practices
  - Troubleshooting guide

- **[config_quick_reference.md](config_quick_reference.md)** - Quick reference cheat sheet

- **[config_migration_plan.md](config_migration_plan.md)** - Migration guide from v1.x to v2.0+

- **[lifecycle.md](lifecycle.md)** - Lifecycle configuration (`make.toml`)

- **[templates.md](templates.md)** - Template configuration

- **[ai-guide.md](ai-guide.md)** - AI provider setup and configuration

- **[marketplace.md](marketplace.md)** - Marketplace and registry configuration
