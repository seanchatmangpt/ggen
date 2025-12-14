<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Migration Guide: From Other Config Formats to ggen.toml](#migration-guide-from-other-config-formats-to-ggentoml)
  - [Table of Contents](#table-of-contents)
  - [Migration from Cargo.toml (Rust)](#migration-from-cargotoml-rust)
    - [Basic Package Metadata](#basic-package-metadata)
    - [Workspace Configuration](#workspace-configuration)
    - [Features and Profiles](#features-and-profiles)
  - [Migration from pyproject.toml (Python)](#migration-from-pyprojecttoml-python)
    - [Basic Project Metadata](#basic-project-metadata)
    - [Poetry Configuration](#poetry-configuration)
  - [Migration from package.json (Node.js)](#migration-from-packagejson-nodejs)
    - [Basic Package Configuration](#basic-package-configuration)
  - [Hybrid Projects](#hybrid-projects)
    - [Rust + TypeScript (e.g., Tauri, WASM)](#rust--typescript-eg-tauri-wasm)
    - [Python + Rust (e.g., PyO3)](#python--rust-eg-pyo3)
  - [Common Patterns](#common-patterns)
    - [Pattern 1: Coexistence Strategy](#pattern-1-coexistence-strategy)
    - [Pattern 2: Template Organization](#pattern-2-template-organization)
    - [Pattern 3: Environment-Specific Configs](#pattern-3-environment-specific-configs)
    - [Pattern 4: Incremental Migration](#pattern-4-incremental-migration)
  - [Migration Checklist](#migration-checklist)
    - [From Cargo.toml](#from-cargotoml)
    - [From pyproject.toml](#from-pyprojecttoml)
    - [From package.json](#from-packagejson)
  - [Troubleshooting](#troubleshooting)
    - [Issue: Duplicate metadata in multiple files](#issue-duplicate-metadata-in-multiple-files)
    - [Issue: Conflicting configurations](#issue-conflicting-configurations)
    - [Issue: Complex workspace setup](#issue-complex-workspace-setup)
  - [Examples](#examples)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Migration Guide: From Other Config Formats to ggen.toml

This guide helps you migrate from other project configuration formats (Cargo.toml, pyproject.toml, package.json) to ggen.toml.

## Table of Contents

1. [Migration from Cargo.toml (Rust)](#migration-from-cargotoml-rust)
2. [Migration from pyproject.toml (Python)](#migration-from-pyprojecttoml-python)
3. [Migration from package.json (Node.js)](#migration-from-packagejson-nodejs)
4. [Hybrid Projects](#hybrid-projects)
5. [Common Patterns](#common-patterns)

---

## Migration from Cargo.toml (Rust)

### Basic Package Metadata

**Cargo.toml:**
```toml
[package]
name = "my-rust-crate"
version = "0.1.0"
authors = ["John Doe <john@example.com>"]
edition = "2021"
description = "A Rust crate for X"
license = "MIT"
repository = "https://github.com/user/my-crate"
keywords = ["cli", "tool"]
categories = ["command-line-utilities"]
```

**ggen.toml equivalent:**
```toml
[project]
name = "my-rust-crate"
version = "0.1.0"
author = "John Doe <john@example.com>"
description = "A Rust crate for X"
license = "MIT"
repository = "https://github.com/user/my-crate"
keywords = ["cli", "tool"]
categories = ["command-line-utilities"]

# Add ggen-specific template config
[templates]
source_dir = "templates"
output_dir = "src/generated"

[templates.rust]
style = "core-team"
error_handling = "thiserror"
async_runtime = "tokio"
```

### Workspace Configuration

**Cargo.toml (workspace):**
```toml
[workspace]
members = [
    "crates/core",
    "crates/cli",
    "crates/utils"
]

[workspace.dependencies]
tokio = { version = "1.0", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
```

**ggen.toml (workspace root):**
```toml
[project]
name = "my-workspace"
version = "1.0.0"
description = "Multi-crate workspace"

[templates]
source_dir = "templates"
output_dir = "generated"

# Reference workspace members
[workspace]
members = ["crates/core", "crates/cli", "crates/utils"]
```

**ggen.toml (member crate in `crates/core/`):**
```toml
[project]
name = "my-workspace-core"
version = "1.0.0"
description = "Core functionality"

[templates]
source_dir = "templates"
output_dir = "src"
```

### Features and Profiles

**Cargo.toml:**
```toml
[features]
default = ["std"]
std = []
nightly = []

[profile.release]
opt-level = 3
lto = true
```

**ggen.toml:**
```toml
[features]
# ggen features (not Cargo features)
ai_generation = true
sparql_queries = true
code_validation = true

# For Cargo features, keep Cargo.toml alongside ggen.toml
# ggen.toml focuses on code generation configuration
```

**Key Insight:** ggen.toml complements Cargo.toml rather than replacing it. Keep Cargo.toml for build configuration and dependencies, use ggen.toml for code generation.

---

## Migration from pyproject.toml (Python)

### Basic Project Metadata

**pyproject.toml:**
```toml
[project]
name = "my-python-package"
version = "0.1.0"
description = "A Python package"
authors = [
    {name = "Jane Doe", email = "jane@example.com"}
]
license = {text = "MIT"}
requires-python = ">=3.8"
dependencies = [
    "requests>=2.28.0",
    "pydantic>=2.0.0",
]

[project.optional-dependencies]
dev = ["pytest>=7.0.0", "black>=23.0.0"]
```

**ggen.toml:**
```toml
[project]
name = "my-python-package"
version = "0.1.0"
description = "A Python package"
author = "Jane Doe <jane@example.com>"
license = "MIT"

[templates]
source_dir = "templates"
output_dir = "src/generated"

# Python-specific template configuration
[templates.python]
min_version = "3.8"
style = "black"
type_checking = "mypy"
testing = "pytest"

[ai]
provider = "openai"
model = "gpt-4"

[ai.prompts]
system = "You are an expert Python developer following PEP 8 and type hints best practices."
```

**Key Insight:** Like Cargo.toml, ggen.toml complements pyproject.toml. Keep pyproject.toml for dependencies and build configuration.

### Poetry Configuration

**pyproject.toml (Poetry):**
```toml
[tool.poetry]
name = "my-package"
version = "0.1.0"
description = ""
authors = ["Author <email@example.com>"]

[tool.poetry.dependencies]
python = "^3.8"
requests = "^2.28"
```

**ggen.toml:**
```toml
[project]
name = "my-package"
version = "0.1.0"
author = "Author <email@example.com>"

[templates]
source_dir = "templates"
output_dir = "src/generated"

[ai]
provider = "openai"
model = "gpt-4"
```

---

## Migration from package.json (Node.js)

### Basic Package Configuration

**package.json:**
```json
{
  "name": "my-node-package",
  "version": "1.0.0",
  "description": "A Node.js package",
  "author": "Developer <dev@example.com>",
  "license": "MIT",
  "repository": {
    "type": "git",
    "url": "https://github.com/user/package"
  },
  "keywords": ["api", "rest"],
  "main": "dist/index.js",
  "scripts": {
    "build": "tsc",
    "test": "jest",
    "dev": "nodemon"
  },
  "dependencies": {
    "express": "^4.18.0"
  },
  "devDependencies": {
    "typescript": "^5.0.0"
  }
}
```

**ggen.toml:**
```toml
[project]
name = "my-node-package"
version = "1.0.0"
description = "A Node.js package"
author = "Developer <dev@example.com>"
license = "MIT"
repository = "https://github.com/user/package"
keywords = ["api", "rest"]

[templates]
source_dir = "templates"
output_dir = "src/generated"

[templates.typescript]
strict = true
target = "ES2020"
module = "commonjs"

[templates.web]
framework = "express"
documentation = "openapi"

[lifecycle]
enabled = true
config_file = "make.toml"

[lifecycle.phases]
build = ["generate", "tsc"]
test = ["jest"]
dev = ["generate", "nodemon"]
```

**Key Insight:** ggen.toml adds code generation capabilities to your Node.js project while keeping package.json for npm/yarn dependency management.

---

## Hybrid Projects

### Rust + TypeScript (e.g., Tauri, WASM)

**Project structure:**
```
my-project/
├── Cargo.toml          # Rust dependencies and build
├── package.json        # Node.js dependencies
├── ggen.toml           # Code generation config
├── templates/
│   ├── rust/           # Rust templates
│   └── typescript/     # TypeScript templates
└── src/
    ├── rust/           # Rust source
    └── typescript/     # TypeScript source
```

**ggen.toml:**
```toml
[project]
name = "hybrid-project"
version = "1.0.0"
description = "Rust + TypeScript hybrid project"

[templates]
source_dir = "templates"
output_dir = "src/generated"

[templates.rust]
style = "core-team"
async_runtime = "tokio"

[templates.typescript]
strict = true
target = "ES2020"

[lifecycle]
enabled = true

[lifecycle.phases]
build = ["generate-rust", "cargo-build", "generate-ts", "npm-build"]
```

### Python + Rust (e.g., PyO3)

**Project structure:**
```
my-project/
├── Cargo.toml          # Rust (PyO3) config
├── pyproject.toml      # Python config
├── ggen.toml           # Code generation
└── src/
    ├── lib.rs          # Rust bindings
    └── python/         # Python code
```

**ggen.toml:**
```toml
[project]
name = "python-rust-bindings"
version = "1.0.0"

[templates]
source_dir = "templates"
output_dir = "src/generated"

[templates.rust]
style = "pyo3"
features = ["python-bindings"]

[templates.python]
style = "black"
type_checking = "mypy"
```

---

## Common Patterns

### Pattern 1: Coexistence Strategy

**Best Practice:** Keep existing config files, add ggen.toml for code generation.

```
project/
├── Cargo.toml          # Dependencies, build, features
├── ggen.toml           # Code generation, templates, AI
├── make.toml           # Lifecycle tasks (cargo-make)
└── templates/          # Code generation templates
```

### Pattern 2: Template Organization

**ggen.toml:**
```toml
[templates]
source_dir = "templates"
output_dir = "generated"

# Organize templates by target language/framework
patterns = [
    "templates/rust/**/*.tmpl",
    "templates/typescript/**/*.tmpl",
    "templates/python/**/*.tmpl"
]
```

**Directory structure:**
```
templates/
├── rust/
│   ├── models.tmpl
│   ├── services.tmpl
│   └── api.tmpl
├── typescript/
│   ├── interfaces.tmpl
│   └── client.tmpl
└── python/
    ├── models.tmpl
    └── api.tmpl
```

### Pattern 3: Environment-Specific Configs

Create multiple ggen.toml files for different environments:

```
project/
├── ggen.toml            # Default config
├── ggen.dev.toml        # Development overrides
├── ggen.prod.toml       # Production overrides
└── ggen.test.toml       # Testing overrides
```

**Usage:**
```bash
# Development
ggen project generate --config ggen.dev.toml

# Production
ggen project generate --config ggen.prod.toml
```

### Pattern 4: Incremental Migration

Migrate in phases:

**Phase 1: Add ggen.toml with minimal config**
```toml
[project]
name = "my-project"
version = "1.0.0"

[templates]
source_dir = "templates"
output_dir = "generated"
```

**Phase 2: Add AI capabilities**
```toml
[ai]
provider = "openai"
model = "gpt-4"
```

**Phase 3: Add RDF/SPARQL**
```toml
[rdf]
base_uri = "https://example.com/ontology/"
prefixes = { ex = "https://example.com/ontology/" }
```

**Phase 4: Add lifecycle automation**
```toml
[lifecycle]
enabled = true
config_file = "make.toml"
```

---

## Migration Checklist

### From Cargo.toml

- [ ] Copy `[package]` metadata to `[project]`
- [ ] Keep Cargo.toml for dependencies and build config
- [ ] Add `[templates]` section for code generation
- [ ] Add `[templates.rust]` for Rust-specific templates
- [ ] Configure `[ai]` if using AI-powered generation
- [ ] Set up `[lifecycle]` for build automation
- [ ] Test: `ggen project generate`

### From pyproject.toml

- [ ] Copy `[project]` metadata to ggen.toml `[project]`
- [ ] Keep pyproject.toml for dependencies
- [ ] Add `[templates]` section
- [ ] Add `[templates.python]` for Python-specific config
- [ ] Configure `[ai]` if needed
- [ ] Test: `ggen project generate`

### From package.json

- [ ] Extract metadata to ggen.toml `[project]`
- [ ] Keep package.json for npm dependencies
- [ ] Add `[templates]` section
- [ ] Add `[templates.typescript]` or `[templates.javascript]`
- [ ] Map `scripts` to `[lifecycle.phases]`
- [ ] Test: `ggen project generate`

---

## Troubleshooting

### Issue: Duplicate metadata in multiple files

**Solution:** Establish single source of truth:
- **Build metadata**: Keep in original file (Cargo.toml, pyproject.toml, package.json)
- **Generation metadata**: Keep in ggen.toml
- **Sync if needed**: Use lifecycle tasks to copy version from Cargo.toml to ggen.toml

### Issue: Conflicting configurations

**Solution:** Clear separation of concerns:
```toml
# ggen.toml - code generation only
[templates]
source_dir = "templates"
output_dir = "generated"

# Cargo.toml - build and dependencies only
[package]
name = "my-crate"
version = "0.1.0"
```

### Issue: Complex workspace setup

**Solution:** Use workspace-aware ggen.toml:

**Root ggen.toml:**
```toml
[project]
name = "workspace-root"
version = "1.0.0"

[templates]
source_dir = "templates"
output_dir = "generated"

[workspace]
members = ["crates/*/ggen.toml"]
```

**Member ggen.toml:**
```toml
[project]
name = "workspace-member"
version = "1.0.0"

[templates]
source_dir = "../../templates"
output_dir = "src/generated"
```

---

## Examples

See the [examples/](../examples/) directory for complete migration examples:

- `examples/rust-migration/` - Cargo.toml → ggen.toml
- `examples/python-migration/` - pyproject.toml → ggen.toml
- `examples/nodejs-migration/` - package.json → ggen.toml
- `examples/hybrid-workspace/` - Multi-language workspace

---

## Next Steps

1. Read the [ggen.toml User Guide](./ggen-toml-guide.md)
2. Review the [Complete Reference](./ggen-toml-reference.md)
3. Try the [Simple Project Example](../examples/simple-project/)
4. Explore [Advanced Examples](../examples/)

---

For questions or issues, see:
- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Documentation**: https://github.com/seanchatmangpt/ggen/docs
