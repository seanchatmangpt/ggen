# Configuration Documentation Index

Index of all configuration-related documentation for ggen.

## Primary Documentation

### [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) ⭐ **START HERE**

Comprehensive configuration guide covering:
- Complete `ggen.toml` project configuration reference
- All available configuration sections and options
- Environment variables documentation
- `make.toml` lifecycle configuration
- System and user configuration files
- Configuration examples and patterns
- Security best practices
- Troubleshooting guide
- Migration guide from v1.x to v2.0+

**Use this guide for:**
- Complete configuration reference
- Understanding all available options
- Setting up new projects
- Troubleshooting configuration issues

### [config.md](config.md)

Quick reference and overview of ggen configuration:
- Configuration priority order
- Quick start examples
- Common configuration patterns
- Links to detailed documentation

**Use this guide for:**
- Quick overview of configuration system
- Getting started quickly
- Finding related documentation

### [config_quick_reference.md](config_quick_reference.md)

Cheat sheet for common configuration tasks:
- Environment variables quick reference
- Common configuration patterns
- Platform-specific config locations
- Quick examples

**Use this guide for:**
- Quick lookup of environment variables
- Common configuration snippets
- Platform-specific information

## Related Documentation

### [config_migration_plan.md](config_migration_plan.md)

Migration guide from v1.x to v2.0+:
- Breaking changes
- Migration steps
- Configuration structure changes
- Backward compatibility notes

**Use this guide for:**
- Upgrading from v1.x
- Understanding breaking changes
- Migration planning

### [lifecycle.md](lifecycle.md)

Lifecycle configuration (`make.toml`):
- Phase configuration
- Hooks and pipelines
- Lifecycle management

**Use this guide for:**
- Configuring lifecycle phases
- Setting up hooks
- Pipeline configuration

### [templates.md](templates.md)

Template configuration:
- Template structure
- Template variables
- Template-specific settings

**Use this guide for:**
- Template development
- Template configuration
- Template variables

### [ai-guide.md](ai-guide.md)

AI provider setup and configuration:
- LLM provider setup (Ollama, OpenAI, Anthropic)
- AI generation parameters
- Provider-specific configuration

**Use this guide for:**
- Setting up AI providers
- Configuring generation parameters
- Troubleshooting AI issues

### [marketplace.md](marketplace.md)

Marketplace and registry configuration:
- Registry settings
- Gpack management
- Update policies

**Use this guide for:**
- Marketplace configuration
- Registry setup
- Gpack management

## Configuration Files Reference

### Project Configuration

**`ggen.toml`** - Project-specific configuration (in project root)
- Project metadata
- Template configuration
- AI/LLM settings
- RDF and graph configuration
- Security settings
- Performance tuning
- Environment-specific overrides

**`make.toml`** - Lifecycle configuration (in project root)
- Lifecycle phases
- Hooks
- Pipelines
- Phase dependencies

See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for complete `ggen.toml` reference.

### System Configuration Files

**`~/.config/ggen/config.toml`** - User-specific defaults
- User preferences
- Default LLM provider
- Default logging settings

**`/etc/ggen/config.toml`** (Linux) or **`/Library/Application Support/ggen/config.toml`** (macOS) - System-wide defaults
- System-wide settings
- Security defaults
- Global logging configuration

See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for system config details.

### Default Configuration

**`config/defaults.toml`** - Embedded default values
- All configuration options with defaults
- Environment variable mappings
- Default values reference

## Quick Navigation

### By Task

- **Setting up a new project:** [config.md](config.md) → Quick Start
- **Configuring AI providers:** [ai-guide.md](ai-guide.md)
- **Setting up lifecycle:** [lifecycle.md](lifecycle.md)
- **Troubleshooting config issues:** [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) → Troubleshooting
- **Understanding configuration precedence:** [config.md](config.md) → Configuration Sources & Precedence
- **Looking up environment variables:** [config_quick_reference.md](config_quick_reference.md)
- **Migrating from v1.x:** [config_migration_plan.md](config_migration_plan.md)

### By Configuration Type

- **Project configuration (`ggen.toml`):** [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) → Project Configuration
- **Lifecycle configuration (`make.toml`):** [lifecycle.md](lifecycle.md)
- **Environment variables:** [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) → Environment Variables
- **System/user config files:** [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) → System & User Configuration Files

## Configuration Priority

Configuration is loaded in this order (later sources override earlier):

1. **System defaults** (embedded in binary)
2. **System config** (`/etc/ggen/config.toml` on Linux)
3. **User config** (`~/.config/ggen/config.toml`)
4. **Project config** (`./ggen.toml`)
5. **Environment variables** (e.g., `GGEN_LLM_PROVIDER`)
6. **CLI arguments** (highest precedence)

See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for complete precedence details.

## Common Configuration Examples

### Basic Project

```toml
[project]
name = "my-project"
version = "1.0.0"

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
```

### Enterprise Configuration

```toml
[project]
name = "enterprise-tool"

[registry]
url = "https://internal-registry.company.com"

[security]
validate_paths = true
audit_operations = true
```

See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) for more examples.

## Support

- **Configuration issues:** See [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) → Troubleshooting
- **Migration help:** See [config_migration_plan.md](config_migration_plan.md)
- **AI setup help:** See [ai-guide.md](ai-guide.md)
- **Lifecycle help:** See [lifecycle.md](lifecycle.md)

---

**Last Updated:** 2025-01-XX  
**Version:** 2.0.0


