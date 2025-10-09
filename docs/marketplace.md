# RGen Marketplace

The RGen marketplace provides a curated ecosystem of reusable code generation packs (rpacks) served via GitHub Pages with automated validation and deployment.

## Overview

The marketplace is hosted at [seanchatmangpt.github.io/ggen](https://seanchatmangpt.github.io/ggen/) (GitHub Pages) and provides:

- **Transparency**: All templates are open source and version controlled
- **Reliability**: GitHub Pages with CDN-backed infrastructure
- **Community**: Easy contribution via pull requests
- **Free**: No additional hosting costs
- **Automated**: CI/CD pipeline validates and publishes registry updates

## Usage

### Environment Configuration

The marketplace URL can be configured using the `GGEN_REGISTRY_URL` environment variable:

```bash
# Use GitHub Pages marketplace (default)
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# Use local registry for development/testing
export GGEN_REGISTRY_URL="file:///path/to/local/registry/"

# Use custom registry
export GGEN_REGISTRY_URL="https://your-registry.com/"
```

### Basic Commands

```bash
# Search for rpacks
rgen search rust cli

# Browse categories
rgen categories

# Install an rpack
rgen add io.rgen.rust.cli-subcommand

# List installed rpacks
rgen packs

# Update rpacks
rgen update

# Remove an rpack
rgen remove io.rgen.rust.cli-subcommand
```

### Using Installed Rpacks

```bash
# Generate code using installed rpack
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Show template information
rgen show io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```

## Registry Structure

The marketplace registry is defined in `registry/index.json` and automatically published to GitHub Pages:

```json
{
  "updated": "2024-12-19T00:00:00Z",
  "packs": {
    "io.ggen.rust.cli-subcommand": {
      "id": "io.ggen.rust.cli-subcommand",
      "name": "Rust CLI Subcommand",
      "description": "Generate clap subcommands for Rust CLI applications with proper error handling and testing",
      "tags": ["rust", "cli", "clap", "subcommand"],
      "keywords": ["rust", "cli", "clap", "command-line", "terminal"],
      "category": "rust",
      "author": "ggen-team",
      "latest_version": "0.1.0",
      "versions": {
        "0.1.0": {
          "version": "0.1.0",
          "git_url": "https://github.com/seanchatmangpt/ggen.git",
          "git_rev": "11ea0739a579165c33fde5fb4d5a347bed6f5c58",
          "sha256": "58db67ac8440401e"
        }
      },
      "license": "MIT",
      "homepage": "https://github.com/seanchatmangpt/ggen",
      "repository": "https://github.com/seanchatmangpt/ggen",
      "documentation": "https://github.com/seanchatmangpt/ggen/tree/main/templates/cli/subcommand"
    }
  }
}
```

## Adding New Rpacks

### 1. Create Rpack Structure

Create a new directory under `templates/` with the following structure:

```
templates/your-category/your-rpack/
├── rpack.toml          # Rpack manifest
├── rust.tmpl           # Template files
├── meta.yaml           # Metadata
└── graphs/             # RDF graphs (optional)
    ├── data.ttl
    └── shapes.ttl
```

### 2. Create Rpack Manifest

Create `rpack.toml` with proper metadata:

```toml
[rpack]
id = "io.ggen.your-category.your-rpack"
name = "Your Rpack Name"
version = "0.1.0"
description = "Description of what this rpack generates"
license = "MIT"
ggen_compat = ">=0.1.0"

[templates]
patterns = ["*.tmpl"]

[rdf]
patterns = ["graphs/*.ttl"]
```

### 3. Generate SHA256 Hash

Use the hash generation script:

```bash
# Generate hash for your rpack
cargo make registry-hash PACK_PATH=templates/your-category/your-rpack
```

### 4. Update Registry Index

Add your rpack to `registry/index.json`:

```json
{
  "id": "io.ggen.your-category.your-rpack",
  "name": "Your Rpack Name",
  "description": "Description of what this rpack generates",
  "tags": ["your", "category", "tags"],
  "keywords": ["keyword1", "keyword2"],
  "category": "your-category",
  "author": "your-name",
  "latest_version": "0.1.0",
  "versions": {
    "0.1.0": {
      "version": "0.1.0",
      "git_url": "https://github.com/seanchatmangpt/ggen.git",
      "git_rev": "current-commit-hash",
      "sha256": "generated-hash"
    }
  },
  "license": "MIT",
  "homepage": "https://github.com/seanchatmangpt/ggen",
  "repository": "https://github.com/seanchatmangpt/ggen"
}
```

### 5. Validate and Publish

Validate the registry and push to trigger automatic publishing:

```bash
# Validate registry
cargo make registry-validate

# Push changes (triggers GitHub Pages deployment)
git add registry/index.json
git commit -m "Add new rpack: io.ggen.your-category.your-rpack"
git push
```

## Hash Generation Process

The SHA256 hash ensures integrity and reproducibility. It's calculated using the same algorithm as the cache manager:

1. **Walk all files** in the rpack directory
2. **Hash file contents** in deterministic order
3. **Produce 64-character hex string**

Use the provided script for consistency:

```bash
./scripts/generate_registry_hashes templates/your-category/your-rpack
```

## Validation Requirements

All rpacks must pass validation before being published:

- **JSON Schema**: Valid registry structure
- **Git References**: Accessible URLs and valid revisions
- **SHA256 Hashes**: Valid hex format
- **Version Strings**: Semantic versioning compliance
- **Required Fields**: All mandatory metadata present

Run validation locally:

```bash
cargo make registry-validate
```

## Local Development and Testing

### Setting Up Local Registry

1. Clone the repository:
   ```bash
   git clone https://github.com/seanchatmangpt/rgen.git
   cd rgen
   ```

2. Set up local registry:
   ```bash
   export GGEN_REGISTRY_URL="file://$(pwd)/registry/"
   ```

3. Test marketplace functionality:
   ```bash
   # Test search
   rgen search rust

   # Test installation
   rgen add io.rgen.rust.cli-subcommand

   # Test generation
   rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test
   ```

### Running Tests

```bash
# Run marketplace tests
cargo test test_marketplace_local

# Run all tests
cargo test
```

## Adding New Rpacks

### 1. Create Template Structure

Organize your templates in the repository:

```
templates/
  your-category/
    your-template/
      rust.tmpl
      python.tmpl
      README.md
```

### 2. Update Registry Index

Add your rpack to `registry/index.json`:

```json
{
  "io.rgen.your-category.your-template": {
    "id": "io.rgen.your-category.your-template",
    "name": "Your Template Name",
    "description": "Description of what this template generates",
    "tags": ["tag1", "tag2"],
    "keywords": ["keyword1", "keyword2"],
    "category": "your-category",
    "author": "your-username",
    "latest_version": "0.1.0",
    "versions": {
      "0.1.0": {
        "version": "0.1.0",
        "git_url": "https://github.com/seanchatmangpt/rgen.git",
        "git_rev": "master",
        "sha256": "calculated_hash"
      }
    },
    "license": "MIT",
    "homepage": "https://github.com/seanchatmangpt/rgen",
    "repository": "https://github.com/seanchatmangpt/rgen"
  }
}
```

### 3. Template Requirements

Templates should include:

- **Frontmatter**: YAML header with `to`, `vars`, `rdf`, `sparql` fields
- **Documentation**: README explaining usage
- **Examples**: Sample generated output
- **Tests**: Validation that templates work correctly

### 4. Submit Pull Request

1. Fork the repository
2. Add your templates and update the registry
3. Test locally with `GGEN_REGISTRY_URL="file://$(pwd)/registry/"`
4. Submit a pull request with:
   - Description of the rpack
   - Example usage
   - Test results

## Troubleshooting

### Common Issues

**Registry not found**: Check that `GGEN_REGISTRY_URL` is set correctly and the URL is accessible.

**Template not found**: Verify the template path in the registry index matches the actual file location.

**Git clone fails**: Ensure the repository URL and revision are correct in the registry index.

**SHA256 mismatch**: The registry index may need to be updated with the correct hash.

### Debug Mode

Enable debug logging to troubleshoot issues:

```bash
export RUST_LOG=debug
rgen search rust
```

## Contributing

We welcome contributions to the marketplace! Please see the main repository's contributing guidelines for details on:

- Code style and standards
- Testing requirements
- Documentation expectations
- Review process

## License

All marketplace templates are licensed under the MIT License unless otherwise specified in the individual template files.