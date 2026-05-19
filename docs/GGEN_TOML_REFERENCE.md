# ggen.toml Complete Reference

**Complete reference guide for the `ggen.toml` project configuration file.**

## Quick Links

- **[Full Configuration Guide](./config.md)** - Complete documentation with all sections
- **[Quick Reference](./config_quick_reference.md)** - Environment variables and common patterns
- **[Migration Guide](./config_migration_plan.md)** - Migrating from legacy formats

## Overview

`ggen.toml` is the primary configuration file for ggen projects. It supports extensive customization across:

- ✅ Project metadata and variables
- ✅ Template configuration (language-specific, template-specific)
- ✅ AI provider configuration (OpenAI, Anthropic, Ollama)
- ✅ RDF/SPARQL graph processing
- ✅ Lifecycle management integration
- ✅ Marketplace and registry settings
- ✅ Security, performance, and logging settings
- ✅ Environment-specific overrides

## Configuration File Locations

Configuration is loaded from multiple locations in priority order:

1. **Project config**: `./ggen.toml` (project root) - **Highest Priority**
2. **User config**: `~/.config/ggen/config.toml` (user defaults)
3. **System config**: `/etc/ggen/config.toml` (system-wide defaults)
4. **Built-in defaults**: Embedded in ggen binary - **Lowest Priority**

## All Supported Sections

### Core Sections

| Section | Purpose | Required |
|---------|---------|----------|
| `[project]` | Project metadata (name, version, author, etc.) | ✅ Yes |
| `[vars]` | Template variables available to all templates | No |
| `[templates]` | Template directory and output settings | No |
| `[templates.*]` | Language or template-specific settings | No |

### AI Configuration

| Section | Purpose | Required |
|---------|---------|----------|
| `[ai]` | AI provider and model settings | No |
| `[ai.prompts]` | System and user prompts | No |
| `[ai.validation]` | Code validation settings | No |
| `[ai.generation]` | Generation options | No |

### RDF and Graph Processing

| Section | Purpose | Required |
|---------|---------|----------|
| `[rdf]` | RDF base IRI and format settings | No |
| `[rdf.prefixes]` | RDF namespace prefixes | No |
| `[graph]` | Graph caching and tracing | No |
| `[sparql]` | SPARQL query settings | No |
| `[data_sources]` | RDF data source files | No |
| `[queries]` | SPARQL query definitions | No |

### Lifecycle and Marketplace

| Section | Purpose | Required |
|---------|---------|----------|
| `[lifecycle]` | Lifecycle management settings | No |
| `[lifecycle.phases]` | Environment-specific phase definitions | No |
| `[marketplace]` | Marketplace registry settings | No |
| `[registry]` | Registry URL and authentication | No |
| `[gpacks]` | Gpack installation and update policies | No |

### Operations

| Section | Purpose | Required |
|---------|---------|----------|
| `[security]` | Security settings (validation, sandboxing) | No |
| `[performance]` | Performance tuning (parallelism, limits) | No |
| `[logging]` | Logging level, format, and output | No |
| `[paths]` | Custom directory paths | No |
| `[determinism]` | Reproducibility settings | No |
| `[cache]` | Cache configuration | No |
| `[backup]` | Backup settings | No |

### Build and Deployment

| Section | Purpose | Required |
|---------|---------|----------|
| `[build]` | Build target and features | No |
| `[test]` | Test framework and settings | No |
| `[deployment]` | Deployment strategy and health checks | No |
| `[monitoring]` | Monitoring and observability | No |

### Features and Environment

| Section | Purpose | Required |
|---------|---------|----------|
| `[features]` | Enable/disable specific features | No |
| `[env.*]` | Environment-specific overrides | No |

## Minimal Example

```toml
[project]
name = "my-project"
version = "0.1.0"

[vars]
author = "John Doe"
license = "MIT"
```

## Comprehensive Example

See [config.md](./config.md#complete-example) for a complete example with all sections.

## Validation

Validate your `ggen.toml` configuration:

```bash
# Validate configuration
ggen config validate

# Check syntax
ggen config check

# Show effective configuration (merged from all sources)
ggen config show
```

## Best Practices

1. **Start Simple**: Begin with `[project]` and `[vars]`, add complexity as needed
2. **Version Control**: Commit `ggen.toml` but never commit secrets (use env vars)
3. **Environment Separation**: Use `[env.*]` sections for environment-specific settings
4. **Security First**: Enable security features in production (`[security]` section)
5. **Performance Tuning**: Configure parallelism and limits based on your system (`[performance]` section)

## Common Patterns

### Local Development with Ollama

```toml
[ai]
provider = "ollama"
model = "qwen3-coder:30b"
```

### Production with OpenAI

```toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.3

[security]
audit_operations = true
require_confirmation = true

[env.production]
logging.level = "warn"
```

### Multi-Language Project

```toml
[project]
name = "fullstack-app"

[templates.rust]
style = "core-team"
error_handling = "thiserror"

[templates.typescript]
framework = "nuxt"
style = "standard"
```

## Migration from Legacy Formats

### From `.ggenrc.yaml`

```yaml
# Old format (.ggenrc.yaml)
vars:
  author: "John Doe"
  license: "MIT"
```

```toml
# New format (ggen.toml)
[vars]
author = "John Doe"
license = "MIT"
```

The `.ggenrc.yaml` format is still supported but deprecated. Migrate to `ggen.toml` for full feature support.

## Related Documentation

- **[Configuration Guide](./config.md)** - Complete reference with all sections detailed
- **[Quick Reference](./config_quick_reference.md)** - Environment variables cheat sheet
- **[Default Config](../config/defaults.toml)** - Default values and examples
- **[Migration Plan](./config_migration_plan.md)** - Step-by-step migration guide

---

**Last Updated:** 2025-01-XX  
**Version:** 2.0


