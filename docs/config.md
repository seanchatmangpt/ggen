<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Configuration](#configuration)
  - [Environment Variables](#environment-variables)
    - [GGEN_REGISTRY_URL](#ggen_registry_url)
  - [Project Configuration](#project-configuration)
    - [ggen.toml](#ggentoml)
    - [.ggenrc.yaml (Legacy)](#ggenrcyaml-legacy)
  - [Marketplace Configuration](#marketplace-configuration)
    - [Registry Settings](#registry-settings)
    - [Gpack Management](#gpack-management)
    - [Update Policies](#update-policies)
  - [Environment Variables](#environment-variables-1)
    - [Global Settings](#global-settings)
    - [Marketplace Settings](#marketplace-settings)
  - [Variable Precedence](#variable-precedence)
  - [Configuration Examples](#configuration-examples)
    - [Basic Project](#basic-project)
    - [Multi-Language Project](#multi-language-project)
    - [Enterprise Configuration](#enterprise-configuration)
  - [Configuration Validation](#configuration-validation)
    - [Validate Configuration](#validate-configuration)
    - [Configuration Errors](#configuration-errors)
  - [Best Practices](#best-practices)
    - [Project Configuration](#project-configuration-1)
    - [Marketplace Configuration](#marketplace-configuration-1)
    - [Team Configuration](#team-configuration)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Configuration

ggen supports configuration through project files and environment variables.

## Environment Variables

### GGEN_REGISTRY_URL

Controls the marketplace registry URL used for searching and installing gpacks.

**Default**: [seanchatmangpt.github.io/ggen/registry](https://seanchatmangpt.github.io/ggen/registry/) (GitHub Pages)

**Examples**:
```bash
# Use production marketplace (default)
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# Use local registry for development/testing
export GGEN_REGISTRY_URL="file:///path/to/local/registry/"

# Use custom registry
export GGEN_REGISTRY_URL="https://your-registry.com/"

# Use GitHub raw URLs (legacy)
export GGEN_REGISTRY_URL="https://raw.githubusercontent.com/seanchatmangpt/ggen/master/registry/"
```

**Local Development Setup**:
```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Use local registry
export GGEN_REGISTRY_URL="file://$(pwd)/docs/registry/"

# Test marketplace functionality
ggen search rust
ggen add io.ggen.rust.cli-subcommand
```

## Project Configuration

### ggen.toml

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "My ggen project"

[vars]
# Default variables available to all templates
author = "John Doe"
license = "MIT"
year = "2024"

[graph]
# RDF graph configuration
allow_remote = false  # Require explicit opt-in for remote RDF
base = "http://example.org/"
prefixes.ex = "http://example.org/"

[paths]
# Override default folder paths
templates = "templates/"
output = "generated/"
cache = ".ggen/"

[determinism]
# Determinism configuration
sort = "name"  # Default matrix sort key
seed = "project"  # Default seed for reproducibility
```

### .ggenrc.yaml (Legacy)

```yaml
vars:
  author: "John Doe"
  license: "MIT"
  year: "2024"

graph:
  allow_remote: false
  base: "http://example.org/"
  prefixes:
    ex: "http://example.org/"

paths:
  templates: "templates/"
  output: "generated/"
  cache: ".ggen/"

determinism:
  sort: "name"
  seed: "project"
```

## Marketplace Configuration

### Registry Settings

```toml
[registry]
# Registry URL (default: https://registry.ggen.io)
url = "https://registry.ggen.io"

# Authentication token for private registries
token = "your-token-here"

# Cache settings
cache_dir = ".ggen/cache"
cache_ttl = "1h"
```

### Gpack Management

```toml
[gpacks]
# Default gpack installation directory
install_dir = ".ggen/gpacks/"

# Update policy
update_policy = "compatible"  # compatible, latest, pinned

# Dependency resolution
resolve_deps = true
lock_versions = true
```

### Update Policies

- **compatible**: Update to latest compatible versions (^)
- **latest**: Update to latest versions regardless of compatibility
- **pinned**: Never update automatically, require manual updates

## Environment Variables

### Global Settings

```bash
# Override configuration file location
export GGEN_CONFIG_FILE="custom.toml"

# Override cache directory
export GGEN_CACHE_DIR="/tmp/ggen-cache"

# Enable debug output
export GGEN_DEBUG=1

# Enable trace output
export GGEN_TRACE=1
```

### Marketplace Settings

```bash
# Registry authentication
export GGEN_REGISTRY_TOKEN="your-token"

# Custom registry URL
export GGEN_REGISTRY_URL="https://custom-registry.com"

# Disable automatic updates
export GGEN_NO_UPDATE=1
```

## Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **Environment variables** (from `.env` files)
2. **System environment** (`$HOME`, `$USER`, etc.)
3. **Project configuration** (`ggen.toml` `[vars]` section)
4. **Gpack variables** (from gpack `ggen.toml`)
5. **Template frontmatter** (`vars:` section)
6. **CLI arguments** (`--var key=value`)

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
description = "Multi-language API"

[graph]
base = "http://api.example.org/"
prefixes.api = "http://api.example.org/"

[gpacks]
update_policy = "compatible"
```

### Enterprise Configuration

```toml
# ggen.toml
[project]
name = "enterprise-tool"
version = "1.0.0"

[registry]
url = "https://internal-registry.company.com"
token = "${GGEN_REGISTRY_TOKEN}"

[gpacks]
update_policy = "pinned"
resolve_deps = true
lock_versions = true

[graph]
allow_remote = false  # Security: no remote RDF
```

## Configuration Validation

### Validate Configuration

```bash
# Validate project configuration
ggen config validate

# Check configuration syntax
ggen config check

# Show effective configuration
ggen config show
```

### Configuration Errors

Common configuration issues:

- **Invalid TOML syntax**: Check brackets and quotes
- **Missing required fields**: Ensure project name and version
- **Invalid registry URL**: Verify registry accessibility
- **Permission issues**: Check file and directory permissions

## Best Practices

### Project Configuration

1. **Use semantic versioning**: Follow semver for project versions
2. **Document variables**: Include descriptions for custom variables
3. **Version control**: Commit configuration files to version control
4. **Environment separation**: Use different configs for dev/prod

### Marketplace Configuration

1. **Pin versions**: Use specific versions for production
2. **Update policies**: Choose appropriate update strategy
3. **Security**: Use tokens for private registries
4. **Caching**: Configure appropriate cache settings

### Team Configuration

1. **Shared configs**: Use team-wide configuration templates
2. **Documentation**: Document custom configuration options
3. **Validation**: Validate configuration in CI/CD
4. **Consistency**: Ensure consistent configuration across environments
