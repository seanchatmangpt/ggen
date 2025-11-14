# Marketplace User Guide

Complete guide to discovering, installing, and using packages from the ggen marketplace.

## 🚀 Quick Start (30 Seconds)

```bash
# Search for packages
ggen market search "api"

# Install a package
ggen market install "advanced-rust-api-8020"

# Use the package
cd advanced-rust-api-8020
ggen lifecycle run
```

## 🔍 Discovering Packages

### Search by Name

```bash
# Find all API packages
ggen market search "api"

# Find specific package
ggen market search "rust-cli"

# Case-insensitive search
ggen market search "GRAPHQL"
```

### Search by Category

```bash
# Search within category
ggen market search "query" --category templates
ggen market search "query" --category utilities
ggen market search "query" --category ai
```

## 📦 Package Information

Package information is available through search results. Use `ggen market search` to find packages and view their details.

## 💾 Installing Packages

### Basic Installation

```bash
# Install latest version
ggen market install "advanced-rust-api-8020"

# Install to specific directory
ggen market install "advanced-rust-api-8020" --target ./my-project

# Install specific version
ggen market install "advanced-rust-api-8020@0.1.0"
```

### Installation Options

```bash
# Dry run (preview changes)
ggen market install "package-name" --dry-run

# Force overwrite
ggen market install "package-name" --force

# Skip dependencies
ggen market install "package-name" --no-dependencies

# Quiet mode
ggen market install "package-name" --quiet
```

### Installing with Dependencies

```bash
# Install with all dependencies (default)
ggen market install "package-name"

# Skip dependencies
ggen market install "package-name" --no-dependencies

# Install only dependencies
ggen market install "package-name" --only-dependencies
```

## 📋 Managing Installed Packages

### List Installed Packages

```bash
# List all installed packages
ggen market list

# Detailed view
ggen market list --detailed

# JSON output
ggen market list --json
```


## 🎯 Using Installed Packages

### Template Packages

```bash
# Install template package
ggen market install "rust-api-template"

# Use template
cd rust-api-template
ggen template generate templates/api.hbs --output src/

# Or use lifecycle
ggen lifecycle run
```

### Utility Packages

```bash
# Install utility package
ggen market install "code-generator-utils"

# Use in your project
# Add to dependencies in your package.toml
[dependencies]
code-generator-utils = "0.1.0"
```

### AI Integration Packages

```bash
# Install AI package
ggen market install "llm-integration"

# Configure
export OPENAI_API_KEY="your-key"

# Use AI features
ggen ai generate "Create a REST endpoint for users"
```

### Framework Packages

```bash
# Install framework
ggen market install "microservice-framework"

# Initialize project
ggen project init --template microservice-framework

# Run lifecycle
ggen lifecycle run
```

## 🔧 Advanced Usage

### Working with Package Manifests

```bash
# View package manifest
cat ~/.ggen/packages/package-name/package.toml
```

### Package Dependencies

```bash
# View package dependencies - check package.toml file
cat ~/.ggen/packages/package-name/package.toml | grep dependencies
ggen market reverse-deps "package-name"
```

### Local Package Development

```bash
# Install from local path
ggen market install ./my-local-package --local

# Link for development
ggen market link ./my-local-package

# Unlink
ggen market unlink "my-local-package"
```

## 📊 Package Examples

### Example 1: REST API Project

```bash
# 1. Install API template
ggen market install "advanced-rust-api-8020"

# 2. Navigate to package
cd advanced-rust-api-8020

# 3. Initialize project
ggen lifecycle init

# 4. Configure environment
cp .env.example .env
# Edit .env with your settings

# 5. Run lifecycle
ggen lifecycle run

# 6. Generated API is ready!
cargo run
```

### Example 2: CLI Application

```bash
# 1. Install CLI template
ggen market install "rust-cli-template"

# 2. Generate project
cd rust-cli-template
ggen lifecycle run

# 3. Build and test
cargo build
cargo test

# 4. Run CLI
cargo run -- --help
```

### Example 3: GraphQL Server

```bash
# 1. Install GraphQL template
ggen market install "graphql-api-rust"

# 2. Configure
cd graphql-api-rust
ggen lifecycle init

# 3. Generate schema
ggen template generate templates/schema.hbs \
  --output src/schema.rs

# 4. Run server
cargo run
```

### Example 4: Microservices Architecture

```bash
# 1. Install microservices template
ggen market install "microservices-architecture"

# 2. Initialize architecture
cd microservices-architecture
ggen lifecycle init

# 3. Generate services
ggen lifecycle run --phases generate

# 4. Deploy
ggen lifecycle deploy --env development
```

## 🔍 Searching Tips

### Effective Search Queries

```bash
# Broad search
ggen market search "rust"

# Specific search
ggen market search "rust rest api authentication"

# By technology
ggen market search "axum postgres jwt"

# By use case
ggen market search "microservice production ready"
```

### Combining Filters

```bash
# Search with category filter
ggen market search "query" --category templates

# Limit results
ggen market search "api" --limit 5
```

## ⚙️ Configuration

### Set Default Target Directory

```bash
# Configure default installation path
ggen config set marketplace.default_target ./packages

# View current config
ggen config get marketplace.default_target
```

### Set Default Registry

```bash
# Use custom registry
ggen config set marketplace.registry_url \
  "https://my-registry.com/packages.toml"

# Reset to default
ggen config set marketplace.registry_url \
  "https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml"
```

### Package Cache

```bash
# Clear package cache
ggen market cache clear

# Update cache
ggen market cache update

# View cache location
ggen market cache path
```

## 🔒 Security Best Practices

### Verify Package Integrity

```bash
# Review before installation - use search to view package details
ggen market search "package-name"
```

### Safe Installation

```bash
# Always dry-run first
ggen market install "new-package" --dry-run

# Review what will be installed
ggen market deps "new-package"

# Install with verification
ggen market install "new-package" --verify
```

### Package Trust

- All packages are open source (visible on GitHub)
- Packages are reviewed before publication
- No telemetry or tracking
- MIT license (most packages)

## 🐛 Troubleshooting

### Package Not Found

```bash
# Update registry cache
ggen market cache update

# Search again
ggen market search "package-name"

# Check registry URL
ggen config get marketplace.registry_url
```

### Installation Fails

```bash
# Check disk space
df -h

# Verify permissions
ls -la ~/.ggen/packages/

# Try with --force
ggen market install "package-name" --force

# Check logs
ggen market install "package-name" --verbose
```

### Template Generation Fails

```bash
# Verify template syntax
ggen template validate template.hbs

# Check variables
ggen template list-vars template.hbs

# Use --verbose for details
ggen template generate template.hbs --verbose
```

### Lifecycle Errors

```bash
# Check make.toml
cat make.toml

# Run specific phase
ggen lifecycle run --phases init

# Verbose output
ggen lifecycle run --verbose
```

## 📈 Productivity Tips

### Create Aliases

```bash
# Add to ~/.bashrc or ~/.zshrc
alias gmi="ggen market install"
alias gms="ggen market search"
alias gml="ggen market list"
```

### Use Package Templates

```bash
# Create reusable project templates
ggen market install "base-template"

# Customize for your org
cd base-template
# Edit files...

# Save as local template
ggen template save . --name my-org-template
```

### Automate Common Tasks

```bash
# Install and run in one command
ggen market install "package" && \
cd package && \
ggen lifecycle run

# Note: Package update functionality is not yet implemented
```

## 🆘 Getting Help

### Documentation

```bash
# View help
ggen market --help

# Command-specific help
ggen market install --help
ggen market search --help
```

### Community Support

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://seanchatmangpt.github.io/ggen/

### Report Package Issues

```bash
# Report bug with package
gh issue create \
  --repo seanchatmangpt/ggen \
  --title "Issue with package-name" \
  --body "Description of issue"
```

## 📚 Next Steps

1. **Explore Packages**: `ggen market search ""` (empty query to see all)
2. **Try Examples**: Install and run sample packages
3. **Build Projects**: Use templates for your projects
4. **Contribute**: Publish your own packages

---

**Ready to explore?** Run `ggen market search` to discover available packages!
