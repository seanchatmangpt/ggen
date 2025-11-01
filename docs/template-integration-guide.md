# Template System Integration Guide

## Overview

The ggen template system is now fully integrated with the CLI, marketplace, and lifecycle management. This guide shows how to use templates in your development workflow.

## Quick Start

### 1. Search for Templates in Marketplace

```bash
# Search for microservice templates
ggen market search "rust microservice"

# Filter by category
ggen market search --category "web-service"
```

### 2. Install Template Package

```bash
# Add template package from marketplace
ggen market add "rust-microservice-template"

# List installed packages
ggen market list
```

### 3. Generate from Template

```bash
# Basic generation
ggen template generate-tree \
  --template rust-microservice-template:service.yaml \
  --output ./my-service

# With variables
ggen template generate-tree \
  --template rust-microservice-template:service.yaml \
  --output ./my-service \
  --var service_name=user-service \
  --var port=8080 \
  --var db_enabled=true

# Interactive mode
ggen template generate-tree \
  --template rust-microservice-template:service.yaml \
  --output ./my-service \
  --interactive

# Dry run (preview)
ggen template generate-tree \
  --template rust-microservice-template:service.yaml \
  --dry-run
```

### 4. Integrate with Lifecycle

Add to your `make.toml`:

```toml
[project]
name = "my-service"
version = "1.0.0"

[[phases]]
name = "template-generate"
description = "Generate code from templates"
commands = [
    "ggen template generate-tree --template service.yaml --var service_name=${SERVICE_NAME}"
]

[[phases]]
name = "init"
description = "Initialize project"
depends_on = ["template-generate"]
commands = [
    "cargo init --lib",
    "cargo add tokio --features full"
]

[[phases]]
name = "build"
description = "Build project"
depends_on = ["init"]
commands = ["cargo build"]
```

Run lifecycle:

```bash
# Run template generation
ggen lifecycle run template-generate

# Run full pipeline
ggen lifecycle run init
```

## Complete Workflow Example

### Microservice Development

```bash
# 1. Search for microservice template
ggen market search "rust axum microservice"

# 2. Add template package
ggen market add "rust-axum-microservice"

# 3. Generate project structure
ggen template generate-tree \
  --template rust-axum-microservice:full-service.yaml \
  --output ./user-service \
  --var service_name=user-service \
  --var port=8080 \
  --var db_type=postgresql \
  --var auth_enabled=true

# 4. Navigate to generated project
cd user-service

# 5. Verify with lifecycle
ggen lifecycle validate

# 6. Build and test
ggen lifecycle run build
ggen lifecycle run test

# 7. Deploy
ggen lifecycle run deploy --env production
```

### Multi-Service Architecture

```bash
# Generate multiple services from template
for service in user-service order-service payment-service; do
  ggen template generate-tree \
    --template microservice.yaml \
    --output ./services/$service \
    --var service_name=$service \
    --var port=$(( 8080 + $RANDOM % 100 ))
done

# Generate gateway
ggen template generate-tree \
  --template api-gateway.yaml \
  --output ./gateway \
  --var services="user,order,payment"
```

## Template Configuration

Create `.ggen/template-config.toml`:

```toml
[search_paths]
paths = [
    "templates",
    ".ggen/templates",
    "~/.ggen/global-templates"
]

[default_variables]
author = "Your Name"
license = "MIT"
rust_edition = "2021"

[metadata_store]
path = ".ggen/metadata.ttl"

[generation]
auto_format = true
run_hooks = true
interactive = false
force_overwrite = false
validate_before_gen = true

[marketplace]
enabled = true
package_cache = ".ggen/packages"
auto_update = false
trusted_sources = ["ggen-official", "community-verified"]
```

## Advanced Features

### Custom Template Variables

```yaml
# template.yaml
metadata:
  name: "Custom Service"
  version: "1.0.0"
  variables:
    - name: service_name
      description: "Service name"
      required: true
    - name: port
      description: "HTTP port"
      default: "8080"
      pattern: "^[0-9]{4,5}$"
    - name: features
      description: "Enabled features"
      default: "auth,metrics,tracing"

files:
  - path: "src/main.rs"
    template: "main.rs.tmpl"
  - path: "Cargo.toml"
    template: "cargo.toml.tmpl"
```

### Post-Generation Hooks

Add hooks to `template.yaml`:

```yaml
post_hooks:
  - "cargo fmt"
  - "cargo clippy --fix --allow-dirty"
  - "git init"
  - "git add ."
  - "git commit -m 'Initial commit from template'"
```

Or in lifecycle `make.toml`:

```toml
[[phases]]
name = "template-generate"
commands = [
    "ggen template generate-tree --template service.yaml"
]

[phases.hooks]
after = [
    "cargo fmt",
    "cargo build --release",
    "cargo test"
]
```

### RDF Metadata

Templates automatically generate RDF metadata:

```bash
# View metadata
cat .ggen/metadata.ttl

# Query with ggen graph
ggen graph query \
  --pattern "?file a ggen:GeneratedFile" \
  --format table
```

## Marketplace Integration

### Publishing Templates

```bash
# Create template package
ggen market create-package \
  --name "my-template" \
  --category "web-service" \
  --templates templates/

# Publish to marketplace
ggen market publish my-template
```

### Template Package Structure

```
my-template-package/
├── package.toml          # Package metadata
├── templates/
│   ├── service.yaml      # Main template
│   ├── simple.yaml       # Variant
│   └── templates/        # Template files
│       ├── main.rs.tmpl
│       └── cargo.toml.tmpl
├── examples/             # Usage examples
│   └── basic-service.sh
└── README.md
```

## Lifecycle Integration Patterns

### Pattern 1: Template-First Development

```toml
# make.toml
[[phases]]
name = "scaffold"
commands = [
    "ggen template generate-tree --template base.yaml --interactive"
]

[[phases]]
name = "init"
depends_on = ["scaffold"]
commands = ["cargo init"]

[[phases]]
name = "build"
depends_on = ["init"]
commands = ["cargo build"]
```

### Pattern 2: Conditional Generation

```toml
[[phases]]
name = "setup"
commands = [
    '''
    if [ ! -f "src/main.rs" ]; then
        ggen template generate-tree --template main.yaml
    fi
    '''
]
```

### Pattern 3: Multi-Stage Generation

```toml
[[phases]]
name = "gen-models"
commands = [
    "ggen template generate-tree --template models.yaml --output src/models"
]

[[phases]]
name = "gen-api"
depends_on = ["gen-models"]
commands = [
    "ggen template generate-tree --template api.yaml --output src/api"
]

[[phases]]
name = "gen-tests"
depends_on = ["gen-models", "gen-api"]
commands = [
    "ggen template generate-tree --template tests.yaml --output tests"
]
```

## Best Practices

### 1. Use Marketplace for Common Patterns

```bash
# Don't reinvent the wheel
ggen market search "pattern you need"
ggen market add "existing-solution"
```

### 2. Version Your Templates

```yaml
# template.yaml
metadata:
  name: "My Template"
  version: "1.2.0"
  min_ggen_version: "1.2.0"
```

### 3. Validate Before Generation

```bash
# Always dry-run first
ggen template generate-tree --template complex.yaml --dry-run

# Then generate
ggen template generate-tree --template complex.yaml
```

### 4. Store Templates in Version Control

```bash
# Add to .gitignore
echo ".ggen/template-cache/" >> .gitignore
echo ".ggen/packages/" >> .gitignore

# Keep templates
git add templates/
git add .ggen/template-config.toml
```

### 5. Use Lifecycle for Automation

```bash
# Integrate with CI/CD
ggen lifecycle run template-generate
ggen lifecycle validate --env production
ggen lifecycle run deploy --env production
```

## Troubleshooting

### Template Not Found

```bash
# Check search paths
ggen template list

# Add custom search path
echo '[search_paths]' >> .ggen/template-config.toml
echo 'paths = ["custom/path"]' >> .ggen/template-config.toml
```

### Variable Errors

```bash
# Use interactive mode
ggen template generate-tree --template service.yaml --interactive

# Or provide all variables
ggen template generate-tree \
  --template service.yaml \
  --var name=value \
  --var key=value
```

### File Conflicts

```bash
# Use force to overwrite
ggen template generate-tree --template service.yaml --force

# Or choose different output
ggen template generate-tree --template service.yaml --output ./new-location
```

## Next Steps

- Explore [Template Creation Guide](./template-creation-guide.md)
- Learn about [RDF Metadata](./rdf-metadata-guide.md)
- See [Lifecycle Documentation](./lifecycle.md)
- Browse [Marketplace Guide](./marketplace.md)
