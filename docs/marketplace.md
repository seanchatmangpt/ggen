# Rgen Marketplace (Rpacks)

The rgen marketplace system provides a frictionless ecosystem for discovering, installing, and using reusable code generation packs. This system enables teams to share and consume high-quality templates, macros, and queries while maintaining security, determinism, and version pinning.

## Consumer DX

### Search & Discovery

```bash
# Search for packs matching query
rgen search rust cli

# Get JSON output for tooling integration
rgen search --json rust cli > results.json

# Search output format:
# ID                                    LATEST     KIND       TAGS
# io.rgen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
```

### Installation & Management

```bash
# Install specific version
rgen add io.rgen.rust.cli-subcommand@0.2.0

# Install latest version
rgen add io.rgen.rust.cli-subcommand

# List installed packs with details
rgen packs

# Update all packs to latest compatible versions
rgen update

# Update specific pack
rgen update io.rgen.rust.cli-subcommand

# Remove pack and clean up
rgen remove io.rgen.rust.cli-subcommand
```

### Usage

```bash
# Use rpack template directly
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users

# Works with all existing rgen features
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl \
  --var name=Users --var description="User management" --dry

# Trace execution for debugging
RGEN_TRACE=1 rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users
```

## Publisher DX

### Pack Development

```bash
# Initialize new pack
rgen pack init

# Lint pack for publishing
rgen pack lint

# Run golden tests
rgen pack test

# Publish to registry (opens PR)
rgen pack publish
```

### Pack Structure

Rpacks follow a standard structure defined in `templates/rgen.toml`:

```toml
[rpack]
id = "io.rgen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.2.1"
description = "Generate clap subcommands"
license = "MIT"
rgen_compat = ">=0.2 <0.4"

[dependencies]
"io.rgen.macros.std" = "^0.2"

[templates]
entrypoints = ["cli/subcommand/rust.tmpl"]
includes   = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
files  = ["../graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]

[queries]
files = ["../queries/*.rq"]
aliases.component_by_name = "../queries/component_by_name.rq"

[shapes]
files = ["../shapes/*.ttl"]

[preset]
config = "../preset/rgen.toml"
vars   = { author = "Acme", license = "MIT" }
```

## Registry Architecture

### Index Structure

The registry uses a JSON index at `https://registry.rgen.dev/index.json`:

```json
{
  "packs": {
    "io.rgen.rust.cli-subcommand": {
      "id": "io.rgen.rust.cli-subcommand",
      "name": "Rust CLI subcommand",
      "description": "Generate clap subcommands",
      "tags": ["rust", "cli", "clap"],
      "latest_version": "0.2.1",
      "versions": {
        "0.2.1": {
          "version": "0.2.1",
          "git_url": "https://github.com/example/rpack.git",
          "git_rev": "abc123...",
          "manifest_url": "https://cdn.example.com/manifest.toml",
          "sha256": "def456..."
        }
      }
    }
  }
}
```

### Publishing Workflow

1. **Local Development**: Use `rgen pack lint` and `rgen pack test` locally
2. **PR Creation**: `rgen pack publish` opens a GitHub PR to the registry repo
3. **CI Validation**: Registry runs validation on the PR
4. **Merge & Deploy**: After merge, registry rebuilds and deploys updated index

### Validation Rules

- **Schema Compliance**: Manifest must match JSON schema
- **Semver Compliance**: Version must follow semantic versioning
- **Compatibility**: `rgen_compat` range must include current rgen version
- **Path Validation**: All referenced files and directories must exist
- **Query Resolution**: All aliased queries must resolve correctly
- **License Allowlist**: Only approved licenses permitted
- **Size Limits**: Packs must be under size thresholds
- **Security**: No install-time shell execution

## Loader Integration

### Automatic Wiring

When `rgen gen` runs, the loader automatically:

1. **Reads `rgen.lock`** → determines installed rpacks
2. **Merges `[preset]`** → project config and variables
3. **Adds template paths** → to Tera search roots
4. **Preloads RDF** → base/prefixes/files/inline into shared Graph
5. **Indexes queries** → exposes `sparql_named(name, var?)`
6. **Sets precedence** → project > locked rpacks > built-ins

### Trace Output

```bash
RGEN_TRACE=1 rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users

# Shows:
# === RGEN TRACE ===
# Resolved frontmatter:
# { ... merged from rpack preset ... }
# SPARQL prolog:
# @prefix ex: <http://example.org/> . @base <http://example.org/> .
# Target output path: src/cmds/users.rs
```

## Lockfile Management

### Structure

```toml
[[pack]]
id = "io.rgen.macros.std"
version = "0.2.1"
sha256 = "<tree-hash>"

[[pack]]
id = "io.rgen.rust.cli-subcommand"
version = "0.2.1"
sha256 = "<tree-hash>"
```

### Integrity Verification

- **SHA256 Tree Hash**: Verifies entire pack contents
- **Version Pinning**: Prevents accidental upgrades
- **Source Tracking**: Records where each pack came from
- **Atomic Updates**: Lockfile updated only after successful installation

## Security Model

### Trust & Safety

- **No Runtime Code**: Packs contain only templates, RDF, and queries
- **SHA256 Verification**: Registry index includes integrity hashes
- **Sandboxed Paths**: Pack content is read-only during generation
- **Shell Hook Opt-in**: Requires `--allow-sh` flag for shell execution
- **License Enforcement**: Registry validates against allowlist

### Network Controls

- **Registry Only**: No network access except to registry
- **Cached Index**: Registry index cached locally for performance
- **Offline Mode**: Works with installed packs without network

## Error Handling

### Actionable Messages

```bash
# Template not found across rpacks
Error: Template 'cli/subcommand/missing.tmpl' not found in any installed rpack
Suggestion: Available templates in io.rgen.rust.cli-subcommand:
  - cli/subcommand/rust.tmpl
  - cli/subcommand/python.tmpl

# Version compatibility issue
Error: Rpack 'io.rgen.old-pack' requires rgen >=0.1 <0.2, but current version is 0.3.0
Suggestion: Update rpack or use rgen update to find compatible version

# Missing query alias
Error: Query alias 'component_by_name' not found
Suggestion: Available aliases in io.rgen.macros.std: user_by_id, project_by_slug
```

## Examples

### Basic Workflow

```bash
# Find and install a pack
rgen search rust cli
rgen add io.rgen.rust.cli-subcommand@0.2.1

# Use immediately
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users

# See what's installed
rgen packs

# Update when new versions available
rgen update

# Remove when no longer needed
rgen remove io.rgen.rust.cli-subcommand
```

### Multi-Pack Composition

```bash
# Install multiple related packs
rgen add io.rgen.rust.cli-subcommand@0.2.1
rgen add io.rgen.macros.std@0.2.1

# Use templates from both packs
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users
rgen gen io.rgen.macros.std:macros/std.tmpl type=Result
```

### Publishing Workflow

```bash
# Initialize new pack
rgen pack init

# Edit templates/rgen.toml with pack metadata

# Test locally
rgen pack test

# Lint for publishing
rgen pack lint

# Publish (opens PR to registry)
rgen pack publish
```

## Benefits

### For Consumers
- **Zero Configuration**: Install and use immediately
- **Version Safety**: Pinned versions prevent breaking changes
- **Composability**: Mix and match packs from different publishers
- **Offline Support**: Works without network after installation

### For Publishers
- **Simple Publishing**: Git-based workflow with PR validation
- **Quality Assurance**: Automated linting and testing
- **Discoverability**: Rich search and tagging system
- **Version Management**: Semver-aware updates and compatibility

### For Teams
- **Shared Standards**: Consistent templates across projects
- **Reduced Duplication**: Reuse proven patterns
- **Easy Onboarding**: New team members can quickly adopt standards
- **Governance**: Registry provides quality control and security
