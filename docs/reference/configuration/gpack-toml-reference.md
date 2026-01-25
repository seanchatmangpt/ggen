<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [gpack.toml Package Format Reference](#gpacktoml-package-format-reference)
  - [Prerequisites](#prerequisites)
  - [Overview](#overview)
  - [Quick Example](#quick-example)
  - [Package Sections](#package-sections)
    - [`[package]` - Package Metadata](#package---package-metadata)
    - [`[dependencies]` - Package Dependencies](#dependencies---package-dependencies)
    - [`[templates]` - Template Definitions](#templates---template-definitions)
    - [`[scripts]` - Automation Scripts](#scripts---automation-scripts)
    - [`[validation]` - Validation Rules](#validation---validation-rules)
    - [`[features]` - Package Features](#features---package-features)
    - [`[tags]` - Package Tags](#tags---package-tags)
    - [`[metadata]` - Additional Metadata](#metadata---additional-metadata)
  - [Complete Example - JavaScript + Zod Generator](#complete-example---javascript--zod-generator)
  - [Complete Example - Rust CLI Generator](#complete-example---rust-cli-generator)
  - [Package Lifecycle](#package-lifecycle)
    - [1. Package Creation](#1-package-creation)
    - [2. Package Validation](#2-package-validation)
    - [3. Package Installation](#3-package-installation)
    - [4. Package Usage](#4-package-usage)
  - [Best Practices](#best-practices)
    - [1. Version Semantically](#1-version-semantically)
    - [2. Document Templates](#2-document-templates)
    - [3. Declare All Dependencies](#3-declare-all-dependencies)
    - [4. Provide Validation](#4-provide-validation)
    - [5. Use Feature Flags](#5-use-feature-flags)
    - [6. Tag Appropriately](#6-tag-appropriately)
  - [Validation](#validation)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# gpack.toml Package Format Reference

Complete reference for ggen package metadata format.

## Prerequisites

**Required**:
- ggen v1.0.0 or higher installed
- Basic TOML syntax knowledge ([learn TOML](https://toml.io))
- Understanding of package management concepts (similar to npm, Cargo, pip)

**Helpful**:
- Template system experience (Jinja2, Liquid, Tera)
- Version constraints knowledge (semver)
- Package publishing workflow understanding

**File Path Conventions**:
- All paths in this document are **relative to package root** (where gpack.toml is located)
- Template files: `./templates/*.tmpl`
- Scripts: `./scripts/`
- Documentation: `./README.md`, `./docs/`

**Package Structure**:
```
my-package/
├── gpack.toml           # This file
├── README.md            # Package documentation
├── templates/           # Template files
│   ├── main.tmpl
│   └── helpers.tmpl
└── scripts/             # Build/test scripts
    ├── generate.js
    └── test.js
```

## Overview

`gpack.toml` defines package metadata for ggen templates and generators. It's similar to `package.json` (npm) or `Cargo.toml` (Rust).

**Location**: Root of template package directory

**Purpose**: Package metadata, dependencies, validation, scripts

## Quick Example

```toml
[package]
name = "javascript-zod-generator"
version = "1.0.0"
description = "Generate JavaScript + Zod schemas from RDF ontologies"
authors = ["ggen-ai <noreply@ggen.io>"]
license = "MIT"

[dependencies]
zod = "^3.22.0"

[templates]
"models.js.tmpl" = { description = "Zod schema definitions" }
"validators.js.tmpl" = { description = "Validation helpers" }

[scripts]
generate = "node scripts/generate.js"
test = "node scripts/test.js"

[validation]
required_files = ["templates/models.js.tmpl"]
min_ggen_version = "1.0.0"

[features]
typescript = false
zod = true
jsdoc = true

[tags]
stable = true
production-ready = true
javascript = true
```

---

## Package Sections

### `[package]` - Package Metadata

Core package identification and metadata.

**Fields**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | String | Yes | Package name (lowercase, hyphens) |
| `version` | String | Yes | Semantic version (e.g., "1.0.0") |
| `description` | String | Yes | One-line package description |
| `authors` | Array[String] | No | Author names/emails |
| `license` | String | No | License identifier (e.g., "MIT") |
| `repository` | String | No | Git repository URL |
| `homepage` | String | No | Package homepage URL |
| `keywords` | Array[String] | No | Search keywords |

**Example**:

```toml
[package]
name = "cli-noun-verb-generator"
version = "2.1.0"
description = "Generate production-ready noun-verb pattern CLIs"
authors = ["Jane Developer <jane@example.com>"]
license = "MIT"
repository = "https://github.com/example/cli-generator"
homepage = "https://example.com/cli-generator"
keywords = ["cli", "codegen", "rust", "clap"]
```

**Naming Conventions**:
- Lowercase only
- Use hyphens (not underscores)
- Descriptive: `javascript-zod-generator` not `jsgen`
- Suffix with purpose: `-generator`, `-template`, `-cli`

---

### `[dependencies]` - Package Dependencies

External dependencies required by generated code.

**Format**: `package = "version-spec"`

**Example - JavaScript**:

```toml
[dependencies]
zod = "^3.22.0"
"@types/node" = "^20.0.0"
```

**Example - Python**:

```toml
[dependencies]
pydantic = "^2.0.0"
rdflib = "^7.0.0"
```

**Example - Rust**:

```toml
[dependencies]
serde = "1.0"
serde_json = "1.0"
clap = { version = "4.0", features = ["derive"] }
```

**Version Specifications**:
- `"1.0.0"` - Exact version
- `"^1.0.0"` - Compatible (SemVer caret)
- `"~1.0.0"` - Patch updates only
- `">= 1.0.0"` - Minimum version

---

### `[templates]` - Template Definitions

Define template files and their metadata.

**Format**: `"path/to/template.ext.tmpl" = { description, output, ... }`

**Fields**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `description` | String | Yes | Template purpose |
| `output` | String | No | Output path (relative to project root) |
| `required` | Boolean | No | Template is required (default: true) |
| `variables` | Table | No | Required template variables |

**Example**:

```toml
[templates]
"scaffold/package.json.tmpl" = {
    description = "NPM package configuration",
    output = "package.json",
    required = true
}

"src/models.js.tmpl" = {
    description = "Zod schema definitions",
    output = "src/models.js"
}

"src/validators.js.tmpl" = {
    description = "Validation helper functions",
    output = "src/validators.js",
    variables = { schema_name = "string" }
}

"README.md.tmpl" = {
    description = "Package documentation",
    required = false
}
```

**Template Variables**:

```toml
[templates."src/models.js.tmpl"]
description = "Model definitions"

[templates."src/models.js.tmpl".variables]
schema_name = "string"
namespace = "string"
include_validators = "boolean"
```

---

### `[scripts]` - Automation Scripts

Define scripts for generation, testing, and validation.

**Format**: `script_name = "command"`

**Common Scripts**:

| Script | Purpose |
|--------|---------|
| `generate` | Main generation script |
| `test` | Run tests |
| `validate` | Validate generated code |
| `format` | Format generated code |
| `lint` | Lint generated code |
| `build` | Build generated code |

**Example**:

```toml
[scripts]
generate = "node scripts/generate-models.js"
test = "npm test"
validate = "zod-validator src/models.js"
format = "prettier --write src/**/*.js"
lint = "eslint src/**/*.js"
build = "npm run build"
```

**Usage**:

```bash
# Run script via ggen
ggen package run generate
ggen package run test
```

---

### `[validation]` - Validation Rules

Define validation requirements for the package.

**Fields**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `required_files` | Array[String] | No | Files that must exist |
| `min_ggen_version` | String | No | Minimum ggen version |
| `max_ggen_version` | String | No | Maximum ggen version |
| `required_features` | Array[String] | No | Required ggen features |
| `validators` | Array[String] | No | Custom validator scripts |

**Example**:

```toml
[validation]
required_files = [
    "templates/models.js.tmpl",
    "templates/validators.js.tmpl",
    "scripts/generate.js"
]

min_ggen_version = "1.0.0"
max_ggen_version = "2.0.0"

required_features = ["rdf", "sparql"]

validators = [
    "scripts/validate-templates.sh",
    "scripts/validate-output.sh"
]
```

**Custom Validators**:

```bash
#!/usr/bin/env bash
# scripts/validate-output.sh

# Check generated code is valid JavaScript
node -c src/models.js || exit 1

# Check Zod schemas parse
node scripts/validate-zod.js || exit 1

echo "✓ Validation passed"
```

---

### `[features]` - Package Features

Declare package capabilities and features.

**Common Features**:

| Feature | Description |
|---------|-------------|
| `typescript` | TypeScript support |
| `javascript` | JavaScript support |
| `zod` | Zod schema generation |
| `jsdoc` | JSDoc type annotations |
| `tests` | Test generation |
| `cli` | CLI generation |
| `api` | API generation |

**Example**:

```toml
[features]
# Language targets
typescript = false
javascript = true
python = false
rust = false

# Schema/validation
zod = true
jsdoc = true
pydantic = false

# Code types
models = true
validators = true
tests = true
cli = false
```

**Feature-Conditional Templates**:

```toml
[templates."src/models.ts.tmpl"]
description = "TypeScript models"
required_features = ["typescript"]

[templates."src/models.js.tmpl"]
description = "JavaScript models"
required_features = ["javascript"]
```

---

### `[tags]` - Package Tags

Boolean flags for package classification and filtering.

**Common Tags**:

| Tag | Meaning |
|-----|---------|
| `stable` | Production-ready |
| `experimental` | Experimental/beta |
| `deprecated` | Deprecated package |
| `production-ready` | Suitable for production |
| `typescript`, `javascript`, etc. | Language tags |

**Example**:

```toml
[tags]
stable = true
production-ready = true
experimental = false
deprecated = false

# Language tags
javascript = true
typescript = false
rust = false

# Feature tags
cli = true
api = false
full-stack = false
```

---

### `[metadata]` - Additional Metadata

Optional metadata for package management.

**Fields**:

| Field | Type | Description |
|-------|------|-------------|
| `category` | String | Package category |
| `icon` | String | Icon emoji or URL |
| `screenshots` | Array[String] | Screenshot URLs |
| `documentation` | String | Documentation URL |
| `changelog` | String | Changelog URL |

**Example**:

```toml
[metadata]
category = "code-generation"
icon = "⚙️"
documentation = "https://example.com/docs"
changelog = "https://example.com/changelog"
screenshots = [
    "https://example.com/screenshots/1.png",
    "https://example.com/screenshots/2.png"
]
```

---

## Complete Example - JavaScript + Zod Generator

```toml
# Package metadata
[package]
name = "javascript-zod-generator"
version = "1.2.0"
description = "Generate JavaScript + Zod schemas from RDF ontologies"
authors = ["ggen-ai <noreply@ggen.io>"]
license = "MIT"
repository = "https://github.com/ggen-ai/javascript-zod-generator"
keywords = ["javascript", "zod", "schema", "validation", "rdf"]

# Dependencies for generated code
[dependencies]
zod = "^3.22.0"

# Template definitions
[templates]
"scaffold/package.json.tmpl" = {
    description = "NPM package configuration",
    output = "package.json",
    required = true
}

"src/models.js.tmpl" = {
    description = "Zod schema definitions",
    output = "src/models.js",
    variables = { namespace = "string" }
}

"src/validators.js.tmpl" = {
    description = "Validation helper functions",
    output = "src/validators.js"
}

"tests/models.test.js.tmpl" = {
    description = "Model tests",
    output = "tests/models.test.js",
    required = false
}

"README.md.tmpl" = {
    description = "Package documentation",
    required = false
}

# Automation scripts
[scripts]
generate = "node scripts/generate-models.js"
test = "npm test"
validate = "node scripts/validate-schemas.js"
format = "prettier --write src/**/*.js"
lint = "eslint src/**/*.js"

# Validation rules
[validation]
required_files = [
    "templates/src/models.js.tmpl",
    "templates/src/validators.js.tmpl",
    "scripts/generate-models.js"
]

min_ggen_version = "1.0.0"

validators = ["scripts/validate-output.sh"]

# Package features
[features]
typescript = false
javascript = true
zod = true
jsdoc = true
tests = true
validators = true

# Package tags
[tags]
stable = true
production-ready = true
javascript = true
zod = true
schema-validation = true

# Additional metadata
[metadata]
category = "code-generation"
icon = "📦"
documentation = "https://ggen.dev/docs/generators/javascript-zod"
```

---

## Complete Example - Rust CLI Generator

```toml
[package]
name = "rust-cli-noun-verb"
version = "2.0.0"
description = "Generate Rust CLI with noun-verb pattern using clap"
authors = ["ggen-ai <noreply@ggen.io>"]
license = "MIT"
keywords = ["rust", "cli", "clap", "codegen"]

[dependencies]
clap = { version = "4.0", features = ["derive"] }
serde = "1.0"
serde_json = "1.0"
anyhow = "1.0"

[templates]
"scaffold/Cargo.toml.tmpl" = {
    description = "Cargo configuration",
    required = true
}

"src/main.rs.tmpl" = {
    description = "Main CLI entry point",
    required = true
}

"src/noun/mod.rs.tmpl" = {
    description = "Noun module with verb routing",
    variables = { noun_name = "string" }
}

"src/noun/verb.rs.tmpl" = {
    description = "Verb implementation",
    variables = { noun_name = "string", verb_name = "string" }
}

"tests/integration_test.rs.tmpl" = {
    description = "Integration tests",
    required = false
}

[scripts]
generate = "bash scripts/generate-cli.sh"
test = "cargo test"
build = "cargo build --release"
lint = "cargo clippy -- -D warnings"
format = "cargo fmt"

[validation]
required_files = [
    "templates/scaffold/Cargo.toml.tmpl",
    "templates/src/main.rs.tmpl",
    "scripts/generate-cli.sh"
]

min_ggen_version = "1.0.0"

[features]
rust = true
cli = true
clap = true
noun-verb = true

[tags]
stable = true
production-ready = true
rust = true
cli = true
```

---

## Package Lifecycle

### 1. Package Creation

```bash
# Create package structure
ggen package init javascript-zod-generator

# Edit gpack.toml
vim gpack.toml

# Add templates
mkdir -p templates/src
vim templates/src/models.js.tmpl
```

### 2. Package Validation

```bash
# Validate package structure
ggen package validate

# Check for missing required files
ggen package check

# Run custom validators
ggen package run validate
```

### 3. Package Installation

```bash
# Install from local directory
ggen package install ./javascript-zod-generator

# Install from Git repository
ggen package install https://github.com/example/pkg.git

# Install specific version
ggen package install javascript-zod-generator@1.2.0
```

### 4. Package Usage

```bash
# List installed packages
ggen package list

# Show package info
ggen package info javascript-zod-generator

# Generate using package
ggen generate --package javascript-zod-generator
```

---

## Best Practices

### 1. Version Semantically

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR**: Breaking changes (2.0.0)
- **MINOR**: New features (1.1.0)
- **PATCH**: Bug fixes (1.0.1)

### 2. Document Templates

```toml
[templates."src/models.js.tmpl"]
description = "Zod schema definitions - generates type-safe models"
# Not just: description = "Models"
```

### 3. Declare All Dependencies

```toml
[dependencies]
zod = "^3.22.0"
# Even if users might have it installed!
```

### 4. Provide Validation

```toml
[validation]
required_files = ["templates/critical-file.tmpl"]
validators = ["scripts/validate-output.sh"]
```

### 5. Use Feature Flags

```toml
[features]
typescript = false
javascript = true
# Makes capabilities discoverable
```

### 6. Tag Appropriately

```toml
[tags]
stable = true           # Only if battle-tested
production-ready = true # Only if safe for production
experimental = false    # Mark beta features
```

---

## Validation

```bash
# Validate package structure
ggen package validate

# Check missing files
ggen package check

# Run custom validators
ggen package run validate

# Show package metadata
ggen package show
```

---

## Related Documentation

- **[ggen.toml Reference](ggen-toml-reference.md)** - Project configuration
- **[Common TOML Configurations](../how-to/configuration/common-toml-configs.md)** - Example configs
- **[Template System](../../explanations/fundamentals/template-system.md)** - Template documentation

---

**Last Updated**: 2025-12-10
