# Pack System: Quick Reference Guide

## System Overview Diagram

```
┌────────────────────────────────────────────────────────────────────┐
│                         PACKS SYSTEM                               │
│                   (Composition Layer)                              │
│                                                                    │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  USER COMMANDS (25 verbs)                                   │ │
│  │  Discovery: list, search, show                              │ │
│  │  Management: install, uninstall, update                     │ │
│  │  Generation: generate, compose, plan                        │ │
│  │  Validation: validate, lint, score                          │ │
│  │  Publishing: publish, create                                │ │
│  └──────────────────────────────────────────────────────────────┘ │
│                              ▼                                     │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  PACK DOMAIN (ggen-domain::pack)                           │ │
│  │  • PackRepository: Storage abstraction                      │ │
│  │  • PackGenerator: Project generation                        │ │
│  │  • PackComposer: Multi-pack orchestration                  │ │
│  │  • PackValidator: Quality and compatibility                │ │
│  └──────────────────────────────────────────────────────────────┘ │
│                              ▼                                     │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  MARKETPLACE INTEGRATION                                    │ │
│  │  • Template Resolution (marketplace packages)               │ │
│  │  • SPARQL Execution (render_with_rdf)                      │ │
│  │  • Quality Scoring (marketplace_scorer)                     │ │
│  │  • Template Rendering (ggen-core)                           │ │
│  └──────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
```

---

## Pack Structure

```
my-pack/
├── pack.toml                    # Pack manifest (REQUIRED)
├── README.md                    # Documentation (recommended)
├── CHANGELOG.md                 # Version history (recommended)
├── LICENSE                      # License file (required for publish)
├── templates/                   # Bundled templates (optional)
│   ├── template1.yaml
│   └── template2.yaml
├── queries/                     # SPARQL queries (optional)
│   ├── query1.sparql
│   └── query2.sparql
└── examples/                    # Usage examples (optional)
    └── example1.yaml
```

---

## pack.toml Quick Reference

```toml
# METADATA (required)
[metadata]
name = "my-pack"                 # Pack name (required)
version = "1.0.0"                # Semantic version (required)
title = "My Awesome Pack"        # Human-readable title
description = "Description"      # Pack description (required)
category = "startup"             # Category (required)
tags = ["rust", "web"]           # Search tags
license = "MIT"                  # SPDX license (required)
min_ggen_version = ">=3.0.0"     # Minimum ggen version

[metadata.author]
name = "John Doe"
email = "john@example.com"
url = "https://example.com"

# TEMPLATES (at least one required)
[[templates]]
alias = "backend"                # Optional alias
priority = 1                     # Execution order (lower = earlier)

[templates.source]
type = "marketplace"             # marketplace | local | remote | inline
package_id = "io.ggen.rust.api"
version = "^2.1.0"

[templates.config]
overwrite = false                # Overwrite existing files?
merge = false                    # Merge with existing?
skip_if_exists = true            # Skip if file exists?

[templates.variables]
service_name = "{{ project_name }}-api"
port = "{{ api_port }}"

[[templates.conditions]]          # Optional execution conditions
variable = "database_type"
operator = "equals"
value = "postgres"

# SPARQL QUERIES (optional)
[[queries]]
name = "service-discovery"
description = "Generate service config"
output_path = "config/services.yaml"
output_format = "yaml"
priority = 10

[queries.source]
type = "file"
path = "queries/service-discovery.sparql"

# VARIABLES
[[variables]]
name = "project_name"
description = "Project name"
type = "string"                  # string | integer | boolean | enum | list
required = true
prompt = "Enter project name:"

[variables.validation]
pattern = "^[a-z][a-z0-9-]*$"
min_length = 3
max_length = 50

[[variables]]
name = "api_port"
description = "API server port"
type = "integer"
required = false
default = "8080"

[variables.validation]
min = 1024
max = 65535

# DEPENDENCIES
[dependencies]
devops-pack = "^2.0.0"           # Semantic version constraint
monitoring-pack = "^1.0.0"

# HOOKS
[[hooks.pre_generation]]
name = "validate-env"
type = "builtin"
function = "format-code"

[[hooks.post_generation]]
name = "format-code"
type = "builtin"
function = "format-code"

[[hooks.post_generation]]
name = "install-deps"
type = "command"
command = "npm"
args = ["install"]
continue_on_error = true
timeout = 120

# EXAMPLES
[[examples]]
name = "basic"
description = "Basic usage"
command = "ggen pack generate my-pack --output my-app --var project_name=myapp"
expected_output = "Full project with backend and frontend"
estimated_time = "8s"

# EXCLUSIONS
excludes = [
    "*.tmp",
    ".git/",
    "node_modules/",
]
```

---

## CLI Command Cheat Sheet

### Discovery

```bash
# List all packs
ggen pack list

# List by category
ggen pack list --category startup

# List installed only
ggen pack list --installed

# Search packs
ggen pack search "microservice rust"

# Show pack details
ggen pack show startup-pack

# Show specific version
ggen pack show startup-pack --version 1.2.0

# Show with all details
ggen pack show startup-pack \
  --show-templates \
  --show-queries \
  --show-dependencies \
  --show-variables
```

---

### Management

```bash
# Install pack
ggen pack install startup-pack

# Install specific version
ggen pack install startup-pack --version 1.2.0

# Install from URL
ggen pack install --source https://example.com/pack.tar.gz

# Dry run install
ggen pack install startup-pack --dry-run

# Uninstall pack
ggen pack uninstall startup-pack

# Update pack
ggen pack update startup-pack

# Update all packs
ggen pack update

# Check for updates
ggen pack update --check

# Clean cache
ggen pack clean --cache --downloads
```

---

### Generation

```bash
# Generate from pack
ggen pack generate startup-pack --output my-app

# Generate with variables
ggen pack generate startup-pack --output my-app \
  --var project_name=MyApp \
  --var api_port=3000

# Generate from variables file
ggen pack generate startup-pack --output my-app \
  --vars-file vars.yaml

# Interactive generation
ggen pack generate startup-pack --output my-app --interactive

# Dry run (preview)
ggen pack generate startup-pack --output my-app --dry-run

# Generate specific template only
ggen pack generate startup-pack --output my-app \
  --template backend

# Skip hooks
ggen pack generate startup-pack --output my-app --skip-hooks

# Skip SPARQL queries
ggen pack generate startup-pack --output my-app --skip-queries

# Overwrite existing
ggen pack generate startup-pack --output my-app --overwrite
```

---

### Composition

```bash
# Compose multiple packs
ggen pack compose \
  --packs startup-pack,devops-pack,monitoring-pack \
  --output my-complete-app

# Compose from composition file
ggen pack compose \
  --composition-file composition.yaml \
  --output my-app

# Show composition plan (dry run)
ggen pack plan \
  --packs startup-pack,devops-pack \
  --composition-file composition.yaml

# Merge existing projects
ggen pack merge source-project/ target-project/ \
  --strategy smart

# Custom conflict resolution
ggen pack compose \
  --packs pack1,pack2 \
  --output my-app \
  --resolve-conflicts ask
```

---

### Validation

```bash
# Validate pack
ggen pack validate startup-pack

# Validate with strict mode
ggen pack validate startup-pack --strict

# Check compatibility
ggen pack validate startup-pack \
  --compatibility \
  --with devops-pack

# Validate all components
ggen pack validate startup-pack \
  --check-templates \
  --check-queries \
  --check-dependencies \
  --check-variables

# Lint pack
ggen pack lint ./my-pack/

# Lint with auto-fix
ggen pack lint ./my-pack/ --fix

# Quick health check
ggen pack check startup-pack

# Score pack quality
ggen pack score startup-pack

# Detailed score breakdown
ggen pack score startup-pack --verbose
```

---

### Publishing

```bash
# Create new pack (interactive)
ggen pack create

# Create with options
ggen pack create \
  --name my-pack \
  --category startup \
  --output ./my-pack

# Initialize pack in existing directory
ggen pack init

# Publish pack
ggen pack publish ./my-pack/

# Dry run publish
ggen pack publish ./my-pack/ --dry-run

# Publish to custom registry
ggen pack publish ./my-pack/ \
  --registry https://custom-registry.com
```

---

### Benchmarking

```bash
# Benchmark pack generation
ggen pack benchmark startup-pack

# Multiple iterations
ggen pack benchmark startup-pack --iterations 20

# Save results
ggen pack benchmark startup-pack \
  --output benchmark-results.json

# Compare packs
ggen pack benchmark startup-pack \
  --compare devops-pack
```

---

### Utility

```bash
# Show dependency tree
ggen pack tree startup-pack

# Limit tree depth
ggen pack tree startup-pack --depth 2

# Compare pack versions
ggen pack diff startup-pack 1.2.0 1.3.0

# Show only template changes
ggen pack diff startup-pack 1.2.0 1.3.0 --templates

# Export pack as bundle
ggen pack export startup-pack

# Export with dependencies
ggen pack export startup-pack --include-dependencies

# Import pack from bundle
ggen pack import startup-pack.tar.gz

# Verify checksum
ggen pack import startup-pack.tar.gz --verify
```

---

## composition.yaml Quick Reference

```yaml
name: my-complete-app
description: Full-stack application with DevOps

# Packs to compose
packs:
  - name: startup-pack
    version: "^1.2.0"
    order: 1
    enabled: true
    variables:
      project_name: MyApp
      api_port: 8080

  - name: devops-pack
    version: "^2.0.0"
    order: 2
    variables:
      ci_platform: github-actions

  - name: monitoring-pack
    version: "^1.0.0"
    order: 3
    variables:
      metrics_backend: prometheus

# Global variables (shared across all packs)
global_variables:
  organization: mycompany
  environment: production

# Conflict resolution
conflict_resolution:
  mode: merge  # ask | overwrite | skip | merge | fail

  rules:
    - pattern: "*.md"
      action: merge

    - pattern: "Dockerfile"
      action: overwrite
      priority_pack: devops-pack

    - pattern: "src/**/*.rs"
      action: ask

    - pattern: ".gitignore"
      action: merge
```

---

## Performance Quick Reference

| Operation | Target | Max |
|-----------|--------|-----|
| `pack list` | < 50ms | 100ms |
| `pack search` | < 200ms | 500ms |
| `pack show` | < 100ms | 250ms |
| `pack install` | < 5s | 15s |
| `pack generate` (1 template) | < 2s | 5s |
| `pack generate` (5 templates) | < 8s | 15s |
| `pack compose` (3 packs) | < 20s | 40s |
| `pack validate` | < 500ms | 1s |

---

## Common Patterns

### Pattern 1: Simple Single-Pack Generation

```bash
# 1. Search for pack
ggen pack search "web api"

# 2. View details
ggen pack show web-api-pack

# 3. Install
ggen pack install web-api-pack

# 4. Generate
ggen pack generate web-api-pack --output my-api \
  --var project_name=my-api \
  --var port=8080
```

---

### Pattern 2: Multi-Pack Composition

```bash
# 1. Create composition file
cat > composition.yaml <<EOF
name: my-app
packs:
  - name: backend-pack
    version: "^1.0.0"
    variables:
      service_name: my-service
  - name: frontend-pack
    version: "^2.0.0"
  - name: devops-pack
    version: "^1.0.0"
EOF

# 2. Preview composition
ggen pack plan --composition-file composition.yaml

# 3. Generate
ggen pack compose --composition-file composition.yaml --output my-app
```

---

### Pattern 3: Custom Pack Creation

```bash
# 1. Create pack structure
ggen pack create --name my-custom-pack --output ./my-pack

# 2. Edit pack.toml
vim ./my-pack/pack.toml

# 3. Add templates and queries
# (manually or via --template flag)

# 4. Validate
ggen pack validate ./my-pack/

# 5. Test locally
ggen pack generate ./my-pack/ --output test-project

# 6. Publish
ggen pack publish ./my-pack/
```

---

### Pattern 4: Pack Quality Improvement

```bash
# 1. Check current score
ggen pack score my-pack

# 2. Lint for issues
ggen pack lint ./my-pack/

# 3. Fix issues
ggen pack lint ./my-pack/ --fix

# 4. Add documentation and examples
# (edit pack.toml, add examples)

# 5. Re-validate
ggen pack validate ./my-pack/

# 6. Re-score (should be higher)
ggen pack score my-pack
```

---

## Troubleshooting

### Common Issues and Solutions

| Issue | Cause | Solution |
|-------|-------|----------|
| "Pack not found" | Not installed or wrong name | `ggen pack list --available` |
| "Dependency conflict" | Version constraints incompatible | Check `ggen pack tree`, pin versions |
| "Template rendering failed" | Missing variables | `ggen pack show --show-variables` |
| "File conflict" | Multiple packs write same file | Use `--resolve-conflicts` or composition rules |
| "Hook execution failed" | Missing command or permissions | Check `continue_on_error` flag |
| "SPARQL query failed" | RDF store unavailable | Use `--skip-queries` flag |
| "Validation errors" | Missing required fields | `ggen pack lint` for details |
| "Generation timeout" | Large pack or slow system | Increase timeout or simplify pack |

---

## Environment Variables

```bash
# Pack registry location
export GGEN_PACKS_DIR=~/.ggen/packs

# Cache directory
export GGEN_CACHE_DIR=~/.ggen/cache

# Default registry URL (future)
export GGEN_REGISTRY_URL=https://packs.ggen.io

# Telemetry (opt-in)
export GGEN_TELEMETRY=false

# Log level
export RUST_LOG=info  # trace, debug, info, warn, error
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Validation failure |
| 3 | Dependency resolution failure |
| 4 | Generation failure |
| 5 | Network error |
| 6 | Permission denied |
| 7 | File not found |
| 8 | Configuration error |

---

## Glossary

| Term | Definition |
|------|------------|
| **Pack** | Reusable bundle of templates, queries, and configuration |
| **Composition** | Combining multiple packs into single project |
| **Manifest** | pack.toml file defining pack metadata and structure |
| **Template** | File tree or single file template for code generation |
| **SPARQL Query** | Semantic query for RDF-based code generation |
| **Hook** | Pre/post generation script or command |
| **Variable** | Configurable parameter for pack generation |
| **Dependency** | Another pack required by this pack |
| **Registry** | Storage location for packs (local or remote) |
| **Maturity Score** | Quality score (0-100) based on multiple dimensions |

---

## Related Resources

- **Full Architecture**: See [00_INDEX.md](./00_INDEX.md)
- **CLI Reference**: See [02_PACK_VERBS.md](./02_PACK_VERBS.md)
- **Data Structures**: See [03_DATA_STRUCTURES.md](./03_DATA_STRUCTURES.md)
- **Examples**: See `/examples/packs/` directory

---

## Getting Help

```bash
# General help
ggen pack --help

# Verb-specific help
ggen pack generate --help
ggen pack compose --help
ggen pack validate --help

# Version info
ggen --version

# System info
ggen pack info --system
```

---

**Quick Reference Version**: 1.0
**Last Updated**: 2025-11-17
