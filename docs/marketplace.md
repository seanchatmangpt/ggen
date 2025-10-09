# RGen Marketplace

The RGen marketplace provides a curated ecosystem of reusable code generation packs (rpacks) served directly from the GitHub repository.

## Overview

The marketplace is hosted at `https://github.com/seanchatmangpt/rgen` and serves rpacks via GitHub raw URLs. This approach provides:

- **Transparency**: All templates are open source and version controlled
- **Reliability**: GitHub's robust infrastructure
- **Community**: Easy contribution via pull requests
- **Free**: No additional hosting costs

## Usage

### Environment Configuration

The marketplace URL can be configured using the `RGEN_REGISTRY_URL` environment variable:

```bash
# Use GitHub marketplace (default)
export RGEN_REGISTRY_URL="https://raw.githubusercontent.com/seanchatmangpt/rgen/master/registry/"

# Use local registry for development/testing
export RGEN_REGISTRY_URL="file:///path/to/local/registry/"

# Use custom registry
export RGEN_REGISTRY_URL="https://your-registry.com/"
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

The marketplace registry is defined in `registry/index.json`:

```json
{
  "updated": "2024-01-01T00:00:00Z",
  "packs": {
    "io.rgen.rust.cli-subcommand": {
      "id": "io.rgen.rust.cli-subcommand",
      "name": "Rust CLI Subcommand",
      "description": "Generate clap subcommands for Rust CLI applications",
      "tags": ["rust", "cli", "clap", "subcommand"],
      "keywords": ["rust", "cli", "clap"],
      "category": "rust",
      "author": "rgen-team",
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
}
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
   export RGEN_REGISTRY_URL="file://$(pwd)/registry/"
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
3. Test locally with `RGEN_REGISTRY_URL="file://$(pwd)/registry/"`
4. Submit a pull request with:
   - Description of the rpack
   - Example usage
   - Test results

## Troubleshooting

### Common Issues

**Registry not found**: Check that `RGEN_REGISTRY_URL` is set correctly and the URL is accessible.

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