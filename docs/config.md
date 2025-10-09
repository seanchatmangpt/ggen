# Configuration

rgen supports configuration through project files and environment variables.

## Environment Variables

### GGEN_REGISTRY_URL

Controls the marketplace registry URL used for searching and installing rpacks.

**Default**: [seanchatmangpt.github.io/ggen](https://seanchatmangpt.github.io/ggen/) (GitHub Pages)

**Examples**:
```bash
# Use production marketplace (default)
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/"

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
export GGEN_REGISTRY_URL="file://$(pwd)/registry/"

# Test marketplace functionality
rgen search rust
rgen add io.ggen.rust.cli-subcommand
```

## Project Configuration

### ggen.toml

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "My rgen project"

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
cache = ".rgen/"

[determinism]
# Determinism configuration
sort = "name"  # Default matrix sort key
seed = "project"  # Default seed for reproducibility
```

### .rgenrc.yaml (Legacy)

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
  cache: ".rgen/"

determinism:
  sort: "name"
  seed: "project"
```

## Marketplace Configuration

### Registry Settings

```toml
[registry]
# Registry URL (default: https://registry.rgen.io)
url = "https://registry.rgen.io"

# Authentication token for private registries
token = "your-token-here"

# Cache settings
cache_dir = ".rgen/cache"
cache_ttl = "1h"
```

### Rpack Management

```toml
[rpacks]
# Default rpack installation directory
install_dir = ".rgen/rpacks/"

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
export RGEN_CONFIG_FILE="custom.toml"

# Override cache directory
export RGEN_CACHE_DIR="/tmp/rgen-cache"

# Enable debug output
export RGEN_DEBUG=1

# Enable trace output
export RGEN_TRACE=1
```

### Marketplace Settings

```bash
# Registry authentication
export RGEN_REGISTRY_TOKEN="your-token"

# Custom registry URL
export GGEN_REGISTRY_URL="https://custom-registry.com"

# Disable automatic updates
export RGEN_NO_UPDATE=1
```

## Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **Environment variables** (from `.env` files)
2. **System environment** (`$HOME`, `$USER`, etc.)
3. **Project configuration** (`ggen.toml` `[vars]` section)
4. **Rpack variables** (from rpack `ggen.toml`)
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
description = "A CLI tool generated with rgen"
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

[rpacks]
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
token = "${RGEN_REGISTRY_TOKEN}"

[rpacks]
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
rgen config validate

# Check configuration syntax
rgen config check

# Show effective configuration
rgen config show
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
