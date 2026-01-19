# ggen v0.2.0 Migration Guide

Complete guide for upgrading to ggen v0.2.0 from earlier versions.

## Overview

ggen v0.2.0 consolidates multiple version streams (3.3.0, 5.1.0, etc.) into a unified platform. While the core functionality remains compatible, there are several breaking changes and improvements to be aware of.

## Migration Paths

### From v0.1.0 to v0.2.0

This is the primary upgrade path. Follow the steps below.

### From v3.3.0 to v0.2.0

The v3.3.0 series is being consolidated into v0.2.0 as part of unified versioning. All features from v3.3.0 are preserved in v0.2.0.

## Pre-Upgrade Checklist

- [ ] Backup your current ggen installation
- [ ] Backup your projects and configurations
- [ ] Review breaking changes below
- [ ] Check for deprecated features in your projects
- [ ] Read this entire guide before upgrading

## Step-by-Step Upgrade

### 1. Backup Current Installation

```bash
# Backup ggen config and cache
cp -r ~/.ggen ~/.ggen.v0.1.0.backup
cp -r ~/.config/ggen ~/.config/ggen.v0.1.0.backup

# Backup your projects
cp -r ~/my-projects ~/my-projects.backup
```

### 2. Backup Projects Using Old Package Format

```bash
# Check for old marketplace packages
ggen marketplace list --json > packages_before.json

# Archive old packages
tar czf old_packages_backup.tar.gz ~/.ggen/packages/
```

### 3. Uninstall Previous Version

```bash
cargo uninstall ggen
```

### 4. Install v0.2.0

```bash
# Install from crates.io
cargo install ggen@0.2.0

# Or build from source
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen
git checkout v0.2.0
cargo install --path . --release
```

### 5. Verify Installation

```bash
ggen --version
# Expected: ggen 0.2.0

ggen --help
# Should show all commands
```

### 6. Run Migration Helper

```bash
ggen migrate --from 0.1.0 --to 0.2.0
# This will:
# - Convert old configuration files
# - Update marketplace packages
# - Migrate project files
# - Update templates
```

### 7. Update Project Files

For each of your projects:

```bash
cd my-project

# Update ggen.toml format
ggen update-manifest

# Check for deprecation warnings
ggen check --strict

# Update template references if needed
ggen templates update
```

### 8. Test Generated Code

```bash
# Regenerate code with new version
ggen generate

# Build and test
cargo build  # or: npm run build, etc.
cargo test   # or: npm test
```

## Breaking Changes

### 1. Package Management

**Old (v0.1.0):**
```bash
ggen install --package old-format-package
```

**New (v0.2.0):**
```bash
ggen marketplace install old-format-package
# Use unified marketplace registry
```

**Migration:**
```bash
ggen marketplace migrate-packages
# Automatically converts old packages to new format
```

### 2. Configuration Format

**Old (v0.1.0) - ggen.yaml:**
```yaml
generation:
  output_dir: ./src
  template: default
```

**New (v0.2.0) - ggen.toml:**
```toml
[generation]
output_dir = "./src"
template = "default"
deterministic = true  # New requirement
```

**Migration Steps:**
```bash
# Automatic conversion
ggen config convert --from yaml --to toml

# Or manual - backup and recreate
cp ggen.yaml ggen.yaml.bak
ggen init --regenerate-config
```

### 3. CLI Commands

**Removed/Changed Commands:**

| Old Command | New Command | Status |
|------------|------------|--------|
| `ggen gen` | `ggen generate` | Renamed |
| `ggen pkg list` | `ggen marketplace list` | Reorganized |
| `ggen template` | `ggen templates` (subcommand plural) | Renamed |
| `ggen cache clear` | `ggen cache reset` | Renamed |

**Migration:**
```bash
# Update scripts/CI/CD
# Old: ggen gen --project my-proj
# New: ggen generate --project my-proj

# Old: ggen pkg search rust
# New: ggen marketplace search --query rust

# Old: ggen cache clear
# New: ggen cache reset
```

### 4. Template Format

**Old (v0.1.0):**
- Simple Tera templates
- Limited variable substitution
- No inheritance support

**New (v0.2.0):**
- Enhanced Tera with scoping
- Full inheritance and includes
- Better error messages

**Migration:**
```bash
# Check template compatibility
ggen templates validate

# Automatically upgrade templates
ggen templates upgrade

# Review and test
ggen generate --verbose
```

### 5. Error Handling

**Old (v0.1.0):**
```
Error: Template rendering failed
```

**New (v0.2.0):**
```
Error [E001]: Template rendering failed at line 42, column 8
  Reason: Variable 'undefined_var' not found in scope
  Hint: Check ggen.toml for variable definitions
  File: templates/main.rs.tera
```

**Migration:**
No code changes needed, but error messages are now more helpful.

### 6. Marketplace Packages

**Old Format:**
```json
{
  "name": "my-package",
  "version": "1.0.0",
  "templates": ["template1.tera"]
}
```

**New Format:**
```json
{
  "name": "my-package",
  "version": "1.0.0",
  "spec_version": "0.2.0",
  "templates": ["template1.tera"],
  "ontologies": ["schema.ttl"],
  "validators": [],
  "metadata": {
    "author": "",
    "license": ""
  }
}
```

**Migration:**
```bash
# Automatic upgrade
ggen marketplace migrate-packages

# Manual if needed
ggen package create --from-old ./old-package
```

## Feature Additions

### 1. Ontology Support

**New in v0.2.0:**
```bash
# Define ontology
ggen ontology add ./data/schema.ttl

# Generate from ontology
ggen generate --ontology schema.ttl

# Query ontology
ggen ontology query --sparql "SELECT ?x WHERE ..."
```

### 2. Deterministic Generation

**New in v0.2.0:**
```toml
[generation]
deterministic = true  # Guarantees reproducible outputs
```

**Usage:**
```bash
# Same input always produces same output
ggen generate --seed 12345
```

### 3. AI Integration

**New in v0.2.0:**
```bash
# Configure AI provider
export GGEN_AI_PROVIDER=ollama
export GGEN_AI_MODEL=qwen:7b-code

# Use AI in templates
ggen generate --with-ai

# Hybrid generation (rules + AI)
ggen generate --hybrid-mode
```

## Updated Dependencies

### Major Updates

| Dependency | Old | New | Notes |
|------------|-----|-----|-------|
| tokio | 1.35 | 1.47 | Async runtime improvements |
| serde | 1.0 | 1.0 | No breaking changes |
| clap | 4.4 | 4.5 | Enhanced CLI parsing |
| oxigraph | - | 0.5.1 | New RDF support |

### New Dependencies

```toml
chicago-tdd-tools = "1.4.0"      # Testing framework
pqcrypto-mldsa = "0.1"           # Post-quantum crypto
testcontainers = "0.25"          # E2E testing
proptest = "1.8"                 # Property-based testing
```

### Migration Impact

Most dependencies are compatible. If you use ggen as a library:

```rust
// Old
use ggen::core::Generator;

// New (same interface, enhanced internals)
use ggen::core::Generator;

// New features available
use ggen::ontology::OntologyEngine;
```

## Configuration Migration

### Automatic Migration

```bash
ggen migrate --from 0.1.0 --to 0.2.0 --auto
```

This handles:
- Configuration file format conversion (YAML → TOML)
- Package registry updates
- Template format upgrades
- Deprecated setting removal

### Manual Migration Steps

**1. Update ggen.toml:**

```toml
# v0.1.0 format
[generation]
template = default
output = ./src

# v0.2.0 format
[generation]
template = "default"
output_dir = "./src"
deterministic = true
parallel_threads = "auto"
```

**2. Update environment variables:**

```bash
# Old format
export GGEN_TEMPLATE=default
export GGEN_OUTPUT=./src

# New format
export GGEN_GENERATION_TEMPLATE=default
export GGEN_GENERATION_OUTPUT_DIR=./src
```

**3. Update CI/CD pipelines:**

```yaml
# Old GitHub Actions
- run: ggen gen --project ${{ matrix.project }}

# New GitHub Actions
- run: ggen generate --project ${{ matrix.project }} --deterministic
```

## Verification Checklist

After migration, verify:

- [ ] `ggen --version` shows v0.2.0
- [ ] `ggen --help` shows all commands
- [ ] `ggen config show` displays correct configuration
- [ ] All projects generate successfully
- [ ] Generated code compiles without errors
- [ ] Generated code produces same output as before
- [ ] Tests pass with new generated code
- [ ] Marketplace packages are accessible
- [ ] AI features work (if configured)
- [ ] Ontology validation passes

## Rollback Procedure

If you need to rollback to the previous version:

```bash
# Restore from backup
rm -rf ~/.ggen
rm -rf ~/.config/ggen
cp -r ~/.ggen.v0.1.0.backup ~/.ggen
cp -r ~/.config/ggen.v0.1.0.backup ~/.config/ggen

# Uninstall v0.2.0
cargo uninstall ggen

# Reinstall previous version
cargo install ggen@0.1.0

# Restore project files from backup
cp -r ~/my-projects.backup/* ~/my-projects/
```

## Troubleshooting

### Issue: Migration fails with "Incompatible format"

```bash
# Solution: Run with verbose logging
RUST_LOG=debug ggen migrate --from 0.1.0 --to 0.2.0 --verbose

# Or manually convert
ggen config convert --from yaml --to toml
```

### Issue: Projects don't generate correctly

```bash
# Check for deprecated features
ggen check --strict

# Validate against new schema
ggen validate --schema 0.2.0

# View detailed error
ggen generate --verbose
```

### Issue: Old packages not working

```bash
# Verify package compatibility
ggen marketplace verify old-package

# Migrate package format
ggen marketplace migrate old-package

# Or reinstall from registry
ggen marketplace install old-package@latest
```

### Issue: Configuration not loaded after migration

```bash
# Verify config file location
ggen config show --path

# Check config syntax
ggen config validate

# Reset to defaults if corrupted
ggen config reset
```

## Performance Comparison

| Operation | v0.1.0 | v0.2.0 | Improvement |
|-----------|--------|--------|-------------|
| First Build | 18s | 15s | +20% |
| Incremental | 2.5s | 2s | +25% |
| Project Gen | 800ms | 600ms | +33% |
| Memory Usage | 120MB | 95MB | +26% |
| RDF Processing | 7s (1k triples) | 5s (1k triples) | +40% |

## Next Steps

1. **Review Documentation**: Check [INSTALLATION.md](INSTALLATION.md)
2. **Explore Features**: See [RELEASE-NOTES.md](RELEASE-NOTES.md)
3. **Update Projects**: Run `ggen migrate` on all projects
4. **Report Issues**: Open GitHub issues for any problems

## Support

- **Migration Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://github.com/seanchatmangpt/ggen/docs

---

**Questions?** Refer to specific sections above or open an issue on GitHub.
