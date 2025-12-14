<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen.toml Configuration: Ontology Integration](#ggentoml-configuration-ontology-integration)
  - [Table of Contents](#table-of-contents)
  - [Basic Configuration](#basic-configuration)
  - [Ontology Section](#ontology-section)
    - [Pack References](#pack-references)
    - [Version Constraints](#version-constraints)
    - [Namespace Filtering](#namespace-filtering)
  - [Composition Strategies](#composition-strategies)
    - [Union (Default)](#union-default)
    - [Intersection](#intersection)
    - [Priority](#priority)
    - [Custom](#custom)
  - [Code Generation Targets](#code-generation-targets)
    - [Language Support](#language-support)
    - [Features by Language](#features-by-language)
    - [Generation Hooks](#generation-hooks)
  - [Lock Files (ggen.lock)](#lock-files-ggenlock)
    - [Purpose](#purpose)
    - [Auto-Generated Lock File](#auto-generated-lock-file)
    - [Lock File Configuration](#lock-file-configuration)
    - [Using Lock Files](#using-lock-files)
  - [Hive Queen Orchestration](#hive-queen-orchestration)
    - [How It Works](#how-it-works)
  - [Examples](#examples)
    - [Example 1: Simple E-Commerce Setup](#example-1-simple-e-commerce-setup)
    - [Example 2: Multi-Vocabulary Composition](#example-2-multi-vocabulary-composition)
    - [Example 3: Advanced with Custom Templates and Hooks](#example-3-advanced-with-custom-templates-and-hooks)
    - [Example 4: Filtering Classes and Properties](#example-4-filtering-classes-and-properties)
  - [CLI Integration](#cli-integration)
    - [Generate from Configuration](#generate-from-configuration)
    - [Lock File Management](#lock-file-management)
    - [Export Environment Variables](#export-environment-variables)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)
    - [Conflict Errors](#conflict-errors)
    - [Lock File Staleness](#lock-file-staleness)
    - [Generation Hooks Failing](#generation-hooks-failing)
  - [Integration with CI/CD](#integration-with-cicd)
    - [GitHub Actions Example](#github-actions-example)
    - [GitLab CI Example](#gitlab-ci-example)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen.toml Configuration: Ontology Integration

Complete guide to configuring ontology packs, code generation targets, and lock files using ggen.toml.

## Table of Contents

1. [Basic Configuration](#basic-configuration)
2. [Ontology Section](#ontology-section)
3. [Composition Strategies](#composition-strategies)
4. [Code Generation Targets](#code-generation-targets)
5. [Lock Files (ggen.lock)](#lock-files-ggenlock)
6. [Hive Queen Orchestration](#hive-queen-orchestration)
7. [Examples](#examples)

## Basic Configuration

A minimal ggen.toml with ontology configuration:

```toml
[package]
name = "my-project"
version = "1.0.0"

[ontology]
packs = [
    { name = "schema-org", version = "^3.13.0" }
]
composition = "union"

[ontology.targets.typescript]
language = "typescript"
output_dir = "src/generated"
features = ["zod", "utilities"]

[ontology.targets.rust]
language = "rust"
output_dir = "src/generated"
features = ["serde", "validation"]

[ontology.lock]
file = "ggen.lock"
auto_update = true
enforce = false
```

## Ontology Section

### Pack References

Each pack reference specifies:

```toml
[[ontology.packs]]
name = "schema-org"          # Pack name (required)
version = "^3.13.0"         # Version constraint (required)
namespace = "https://schema.org/e-commerce"  # Optional: specific namespace
classes = ["Product", "Offer", "Organization"]  # Optional: filter classes
properties = ["name", "price", "description"]   # Optional: filter properties
source = "registry://..."   # Optional: custom source for private packs
```

### Version Constraints

Semantic versioning constraints:

```toml
# Exact version
version = "3.13.0"

# Compatible versions (caret)
version = "^3.13.0"  # >=3.13.0, <4.0.0

# Approximate versions (tilde)
version = "~3.13.0"  # >=3.13.0, <3.14.0

# Latest
version = "latest"

# Range
version = ">=3.0.0, <4.0.0"
```

### Namespace Filtering

Extract specific namespace from pack:

```toml
[[ontology.packs]]
name = "schema-org"
version = "^3.13.0"
namespace = "https://schema.org/e-commerce"
classes = ["Product", "Offer", "Organization"]
```

## Composition Strategies

### Union (Default)

Combines all classes and properties from all packs:

```toml
[ontology]
composition = "union"
```

Result: Schema-org + Dublin-core = all classes from both

### Intersection

Only includes classes/properties common to all packs:

```toml
[ontology]
composition = "intersection"
```

Result: Only overlapping classes included

### Priority

First pack takes precedence in conflicts:

```toml
[ontology]
composition = "priority"
```

Result: Schema-org definitions override Dublin-core

### Custom

Fine-grained conflict resolution:

```toml
[ontology]
composition = "custom"

[ontology.composition.rules]
"schema-org" = "use-first"
"dublin-core" = "use-second"
"foaf" = "merge"
```

## Code Generation Targets

### Language Support

Configured per target language:

```toml
[ontology.targets.typescript]
language = "typescript"
output_dir = "src/generated"
features = ["zod", "utilities", "graphql"]
template_path = "templates/custom"  # Optional

  [ontology.targets.typescript.hooks]
  pre_generate = "npm run clean"
  post_generate = "npm run format"
  validate = "npm run typecheck"

[ontology.targets.rust]
language = "rust"
output_dir = "src/generated"
features = ["serde", "validation", "sqlx"]

  [ontology.targets.rust.hooks]
  post_generate = "cargo fmt"
  validate = "cargo clippy"

[ontology.targets.python]
language = "python"
output_dir = "src/generated"
features = ["pydantic", "sqlalchemy"]

[ontology.targets.go]
language = "go"
output_dir = "internal/generated"
features = ["encoding/json", "validation"]
```

### Features by Language

**TypeScript**:
- `zod` - Runtime validation with Zod
- `utilities` - Helper functions
- `graphql` - GraphQL schema generation
- `openapi` - OpenAPI schema export

**Rust**:
- `serde` - Serialization/deserialization
- `validation` - Input validation
- `sqlx` - Database integration
- `async` - Async trait support

**Python**:
- `pydantic` - Data validation
- `sqlalchemy` - ORM integration
- `typing-extensions` - Advanced types
- `dataclass` - Use dataclasses instead of Pydantic

**Go**:
- `json` - JSON marshaling
- `validation` - Input validation
- `gorm` - ORM integration
- `grpc` - gRPC service definitions

### Generation Hooks

Run commands during code generation:

```toml
[ontology.targets.typescript.hooks]
pre_generate = "npm run clean && npm run lint"
post_generate = "npm run format && npm run typecheck"
validate = "npm test"
```

## Lock Files (ggen.lock)

### Purpose

Lock files freeze exact versions for reproducible builds across environments.

### Auto-Generated Lock File

```toml
[ontology.lock]
version = 1
generated_at = "2024-01-15T10:30:00Z"
generator_version = "3.2.0"

[[packages]]
name = "schema-org"
version = "3.13.0"
resolved = "registry://ggen-marketplace/schema-org@3.13.0"
integrity = "sha256-abc123def456ghi789jkl012mno345pqr678stu901vwx"
namespace = "https://schema.org/"
classes_count = 788
properties_count = 2500

  [packages.dependencies]
  # None for schema-org

[[packages]]
name = "dublin-core"
version = "1.11.0"
resolved = "registry://ggen-marketplace/dublin-core@1.11.0"
integrity = "sha256-xyz789abc456def123ghi456jkl789mno012pqr345stu"
namespace = "https://purl.org/dc/elements/1.1/"
classes_count = 35
properties_count = 50

  [packages.dependencies]
  # None for dublin-core

[composition]
strategy = "union"
total_classes = 823
total_properties = 2550
conflicts_resolved = 0
validation_status = "valid"

[hashes]
schema-org = "6a7f8b9c0d1e2f3a4b5c6d7e8f9a0b1c"
dublin-core = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6"
```

### Lock File Configuration

```toml
[ontology.lock]
file = "ggen.lock"      # Lock file path
auto_update = true      # Auto-update on pack changes
enforce = true          # Require lock file for reproducible builds
```

### Using Lock Files

In CI/CD environments:

```bash
# Ensure reproducible builds
ggen ontology generate --use-lock-file ggen.lock

# Or set as environment variable
export GGEN_LOCK_FILE=ggen.lock
ggen ontology generate
```

## Hive Queen Orchestration

Advanced multi-agent configuration management (automatic, no configuration needed):

```toml
[ontology]
# Hive Queen automatically manages:
# - Version resolution across packs
# - Conflict detection and resolution
# - Composition optimization
# - Validation and compatibility checking
# - Agent-based decision making
```

### How It Works

1. **Agent Spawning** - Hive Queen spawns agents based on complexity:
   - `Analyzer` - Analyzes configuration
   - `VersionResolver` - Resolves version conflicts
   - `ConflictDetector` - Detects compatibility issues
   - `Validator` - Validates composition safety
   - `Optimizer` - Recommends optimizations (complex configs)
   - `PerformanceManager` - Manages caching (very complex configs)

2. **Distributed Decision Making** - Agents vote on resolutions
3. **Consensus Building** - Majority agreement determines outcomes
4. **Orchestration** - Hive Queen coordinates all phases

## Examples

### Example 1: Simple E-Commerce Setup

```toml
[package]
name = "ecommerce-api"
version = "1.0.0"

[ontology]
packs = [
    { name = "schema-org", version = "^3.13.0" }
]
composition = "union"

[ontology.targets.typescript]
language = "typescript"
output_dir = "src/generated/types"
features = ["zod", "utilities"]

[ontology.targets.rust]
language = "rust"
output_dir = "src/generated"
features = ["serde"]

[ontology.lock]
file = "ggen.lock"
auto_update = true
enforce = true
```

### Example 2: Multi-Vocabulary Composition

```toml
[ontology]
packs = [
    { name = "schema-org", version = "^3.13.0", classes = ["Product", "Offer"] },
    { name = "dublin-core", version = "~1.11.0", classes = ["Creator", "Date"] },
    { name = "foaf", version = "^0.1.0" }
]
composition = "union"

[[ontology.targets]]
name = "typescript"
language = "typescript"
output_dir = "src/generated"
features = ["zod", "utilities", "graphql"]

[[ontology.targets]]
name = "python"
language = "python"
output_dir = "src/generated"
features = ["pydantic"]
```

### Example 3: Advanced with Custom Templates and Hooks

```toml
[ontology]
packs = [
    { name = "schema-org", version = "^3.13.0" },
    { name = "custom-internal", version = "1.0.0", source = "file:///internal/ontologies" }
]
composition = "priority"

[ontology.targets.typescript]
language = "typescript"
output_dir = "src/generated"
features = ["zod", "utilities"]
template_path = "./templates/custom-typescript"

  [ontology.targets.typescript.hooks]
  pre_generate = "npm run clean"
  post_generate = "npm run format && npm run build"
  validate = "npm test"

[ontology.targets.rust]
language = "rust"
output_dir = "src/generated"
features = ["serde", "validation"]
template_path = "./templates/custom-rust"

  [ontology.targets.rust.hooks]
  post_generate = "cargo fmt && cargo clippy"
  validate = "cargo test"

[ontology.lock]
file = "ggen.lock"
auto_update = true
enforce = true
```

### Example 4: Filtering Classes and Properties

```toml
[[ontology.packs]]
name = "schema-org"
version = "^3.13.0"
namespace = "https://schema.org/e-commerce"
classes = [
    "Product",
    "Offer",
    "Organization",
    "Person",
    "Brand",
    "AggregateRating",
    "ReviewRating"
]
properties = [
    "name",
    "description",
    "price",
    "priceCurrency",
    "availability",
    "ratingValue",
    "ratingCount"
]
```

## CLI Integration

### Generate from Configuration

```bash
# Use ggen.toml automatically
ggen ontology generate

# Specify custom config file
ggen ontology generate --config custom-ggen.toml

# Force regenerate all targets
ggen ontology generate --force

# Generate specific target only
ggen ontology generate --target typescript
```

### Lock File Management

```bash
# Create lock file
ggen ontology lock create

# Update lock file from current config
ggen ontology lock update

# Enforce lock file
GGEN_LOCK_ENFORCE=1 ggen ontology generate
```

### Export Environment Variables

```bash
# Export lock file as environment variables for CI/CD
eval $(ggen ontology lock export-env ggen.lock)

# Now GGEN_PACK_SCHEMA_ORG_VERSION and others are available
echo $GGEN_PACK_SCHEMA_ORG_VERSION  # 3.13.0
```

## Best Practices

1. **Always commit ggen.lock** - Ensures reproducible builds
2. **Use version constraints** - Not exact versions unless needed
3. **Organize targets logically** - Group by language or concern
4. **Use hooks for validation** - Ensure generated code quality
5. **Keep configs DRY** - Use TOML arrays for repeated sections
6. **Filter classes/properties** - Only include what you need
7. **Monitor Hive Queen logs** - Check agent decisions for complex configs

## Troubleshooting

### Conflict Errors

```bash
# Check conflicts detected
ggen ontology validate --verbose

# Use custom resolution rules
[ontology.composition.rules]
"package-a" = "use-first"
```

### Lock File Staleness

```bash
# Update lock file after changing ggen.toml
ggen ontology lock update
```

### Generation Hooks Failing

```toml
# Debug hooks
[ontology.targets.typescript.hooks]
post_generate = "set -x && npm run format"  # Enable bash debug mode
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
- name: Load Ontology Lock File
  run: |
    eval $(ggen ontology lock export-env ggen.lock)

- name: Generate Ontology Code
  run: ggen ontology generate

- name: Commit Generated Code
  if: github.event_name == 'push'
  run: |
    git add src/generated
    git commit -m "chore: regenerate types from ontology"
```

### GitLab CI Example

```yaml
generate-ontology:
  script:
    - ggen ontology generate --use-lock-file ggen.lock
  artifacts:
    paths:
      - src/generated
    reports:
      dotenv: .ontology.env
```
