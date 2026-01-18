# clap-noun-verb Marketplace Integration Analysis

**Status**: Pre-specification analysis for marketplace support
**Date**: 2025-12-13
**Phase**: Strategic Planning

---

## Executive Summary

`clap-noun-verb` is a production-ready CLI framework (v5.3.2) that aligns with ggen's architecture principles. Making it a fully supported marketplace project requires:

1. **Package Structure** - Create `marketplace/packages/clap-noun-verb/`
2. **Integration Points** - Expose via ggen CLI and templates
3. **Documentation** - Marketplace-grade quickstart + examples
4. **Validation** - Constitution compliance, type-safety verification
5. **Distribution** - Crates.io + marketplace with version pinning

---

## Current State Assessment

### What We Have
✅ Published crate (v5.3.2 on crates.io)
✅ Comprehensive documentation (tutorial, reference, explanation)
✅ Phase 2 features (typer-like doc comment syntax)
✅ Type-safe macro system (#[noun], #[verb])
✅ Agent-ready (JSON output, introspection, MCP compatible)
✅ ggen integration (conventions, presets, templates exist)

### What's Missing for Marketplace
❌ Dedicated marketplace package structure
❌ Marketplace-specific documentation (quickstart, examples)
❌ Schema definitions (CLI ontology in RDF/TTL)
❌ Integration examples (ggen use cases)
❌ Marketplace installation support
❌ Version management strategy
❌ Testing templates for marketplace integration

---

## Architecture: How clap-noun-verb Fits ggen

### Design Alignment

```
ggen Crate Architecture:           clap-noun-verb Fits:
┌─────────────────────────┐        ┌─────────────────────────┐
│  ggen-cli (CLI layer)   │ ←────→ │  clap-noun-verb (CLI)   │
└──────────────┬──────────┘        └──────────────┬──────────┘
               │                                   │
        ┌──────▼──────────┐        ┌──────────────▼────────┐
        │ Domain Logic    │        │ Domain-Separated Apps │
        │ (RDF, ontology) │        │ (your logic here)     │
        └─────────────────┘        └───────────────────────┘
```

### Key Principles Alignment

| Principle | How clap-noun-verb Supports It |
|-----------|--------------------------------|
| **Architecture First** | Explicitly separates CLI validation from domain logic |
| **Type-First Thinking** | Compile-time validation via macros, zero-cost abstractions |
| **Deterministic Output** | JSON-first design, structured exit codes |
| **Domain Separation** | Thin CLI wrapper + pure Rust domain logic |
| **Agent-Ready** | JSON output, MCP-compatible introspection, stateless |
| **Zero-Cost Abstractions** | Generics & macros, no runtime overhead |

---

## Marketplace Package Structure

### Proposed Layout

```
marketplace/packages/clap-noun-verb/
├── README.md                          # Marketplace overview
├── USAGE.md                           # Quick integration guide
├── package.toml                       # Marketplace metadata
├── data/
│   ├── clap-noun-verb-schema.ttl      # CLI ontology (RDF/TTL)
│   └── cli-patterns.json              # Common CLI patterns
├── examples/
│   ├── 01-simple-calc/                # Math CLI app (5 min)
│   ├── 02-file-processor/             # File I/O app (15 min)
│   ├── 03-agent-api/                  # Agent-ready REST API (30 min)
│   ├── 04-multi-noun/                 # Multi-command app (45 min)
│   └── Cargo.toml                     # Shared example workspace
├── templates/
│   ├── cli-project.tmpl               # New project scaffold
│   ├── noun-template.tmpl             # New noun command template
│   └── verb-template.tmpl             # New verb command template
├── tools/
│   ├── cli-validator.rs               # Validate CLI structure
│   └── schema-generator.rs            # Generate RDF schemas from code
└── hooks/
    ├── validate.sh                    # Pre-commit CLI validation
    └── introspect.sh                  # Generate CLI documentation
```

### package.toml Format

```toml
[package]
name = "clap-noun-verb"
version = "5.3.2"
description = "Machine-grade CLI framework for AI agents and autonomous systems"
category = "framework"
tags = ["cli", "framework", "agent-ready", "macros", "type-safe"]
authors = ["Sean Chat Management"]
repository = "https://github.com/seanchatmangpt/clap-noun-verb"
documentation = "https://docs.rs/clap-noun-verb"
crates-io = "https://crates.io/crates/clap-noun-verb"
license = "MIT OR Apache-2.0"

[marketplace]
type = "framework"  # framework, template, tool, pattern
scope = "cli"       # cli, domain, integration, deployment
phase = "stable"    # alpha, beta, stable, deprecated
min-ggen-version = "4.0.0"

[dependencies]
clap-noun-verb = "5.3"
clap-noun-verb-macros = "5.3"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

[features]
# Phase 2: Typer-like syntax
advanced-syntax = ["clap-noun-verb/advanced-syntax"]
# MCP integration
mcp-compatible = ["clap-noun-verb/mcp-compatible"]

[examples]
simple-calc = { path = "examples/01-simple-calc" }
file-processor = { path = "examples/02-file-processor" }
agent-api = { path = "examples/03-agent-api" }
multi-noun = { path = "examples/04-multi-noun" }

[validation]
# Constitution alignment checks
type-coverage = "100"           # Percent type annotations
test-coverage = "80"            # Minimum test coverage
unwrap-count = "0"              # Zero unwrap in production code
clippy-lint = "strict"          # All lints must pass
doc-coverage = "100"            # All public APIs documented
```

---

## Integration Points: How ggen Uses clap-noun-verb

### 1. **ggen CLI Commands** (Primary)

```bash
# Generate a new clap-noun-verb project
ggen new --template clap-noun-verb my-cli

# Generate a new noun+verb pair
ggen generate --template marketplace/clap-noun-verb/noun-template my_feature

# Validate CLI structure against schema
ggen validate --schema marketplace/clap-noun-verb/cli-patterns.json src/main.rs

# Generate RDF schema from CLI code
ggen schema --from src/main.rs --output cli-ontology.ttl
```

### 2. **ggen Conventions** (Template Integration)

```rust
// crates/ggen-cli/src/conventions/clap_noun_verb_preset.rs
pub fn register_clap_noun_verb_preset() -> Preset {
    Preset {
        name: "clap-noun-verb-5.3",
        templates: vec![
            "cli-project",
            "noun-template",
            "verb-template"
        ],
        validators: vec![
            "cli-structure",
            "type-coverage",
            "doc-coverage"
        ],
        min_version: "5.3.0"
    }
}
```

### 3. **ggen Templates** (Code Generation)

```tera
{# marketplace/packages/clap-noun-verb/templates/cli-project.tmpl #}
[package]
name = "{{ project_name }}"
version = "0.1.0"
edition = "2021"

[dependencies]
clap-noun-verb = "{{ min_version }}"
clap-noun-verb-macros = "{{ min_version }}"
{% if features.mcp %}
mcp = "0.1"
{% endif %}

// src/main.rs
use clap_noun_verb::Result;
use clap_noun_verb_macros::noun;

#[noun("{{ primary_domain }}", "{{ description }}")]
fn main() -> Result<()> {
    clap_noun_verb::run()
}
```

### 4. **Ontology/Schema** (ggen RDF Integration)

```ttl
# marketplace/packages/clap-noun-verb/data/clap-noun-verb-schema.ttl
@prefix cv: <https://ggen.dev/clap-noun-verb/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

cv:NounCommand a rdfs:Class ;
    rdfs:label "Noun Command" ;
    rdfs:comment "Top-level command noun (e.g., 'file', 'user', 'api')" ;
    cv:hasMacro "noun" ;
    cv:minVersion "5.3.0" .

cv:VerbCommand a rdfs:Class ;
    rdfs:label "Verb Command" ;
    rdfs:comment "Subcommand verb (e.g., 'create', 'delete', 'list')" ;
    cv:hasMacro "verb" ;
    cv:parentClass cv:NounCommand .

cv:ArgumentGroup a rdfs:Class ;
    rdfs:label "Argument Group" ;
    rdfs:comment "Typer-style argument relationship" ;
    cv:phase "5.2.0" .

cv:DocCommentTag a rdfs:Class ;
    rdfs:label "Doc Comment Attribute" ;
    cv:examples "[group: name]", "[requires: arg]", "[conflicts: arg]", "[env: VAR]" .
```

---

## Documentation Strategy

### Marketplace README

**Focus**: How clap-noun-verb solves CLI problems for ggen users

```markdown
# clap-noun-verb in the ggen Marketplace

Machine-grade CLI framework designed for AI agents and autonomous systems.
Perfect for building production CLIs with type safety, domain separation, and agent integration.

## Why Use clap-noun-verb?

### 1. **Type-Safe CLIs**
No runtime surprises—all validation at compile time via macros.

### 2. **Domain-Separated Architecture**
CLI validates, domain computes. Tests focus on domain logic, not CLI parsing.

### 3. **Agent-Ready**
JSON output, introspection, MCP integration out of the box.

### 4. **Zero-Cost**
Generics + macros = no runtime overhead. Fast startup, small binary.

## Quick Integration

### Create a new clap-noun-verb CLI
```bash
ggen new --template clap-noun-verb calculator
cd calculator
cargo run -- calc add 2 3
# {"result": 5}
```

### Add to existing project
```toml
[dependencies]
clap-noun-verb = "5.3"
clap-noun-verb-macros = "5.3"
```

## Examples Included
1. **Simple Calculator** (5 min) - Basic noun + verb
2. **File Processor** (15 min) - File I/O, error handling
3. **Agent API** (30 min) - JSON RPC, introspection
4. **Multi-Noun App** (45 min) - Complex CLI with multiple domains
```

### USAGE.md

**Focus**: Step-by-step integration with ggen

```markdown
# Using clap-noun-verb with ggen

## Installation

```bash
ggen marketplace install --package clap-noun-verb
ggen marketplace install --package clap-noun-verb --version 5.3.2
```

## Creating Your First CLI

### 1. Generate project structure
```bash
ggen new --template clap-noun-verb my-app
cd my-app
```

### 2. Define your domain logic
Create `src/domain.rs` with pure Rust functions (no CLI code).

### 3. Wrap with clap-noun-verb
Use `#[noun]` and `#[verb]` macros in `src/main.rs`.

### 4. Build and test
```bash
cargo make check
cargo make test
ggen generate --schema src/main.rs --output schema.ttl
```

## CLI Validation

```bash
# Validate against marketplace schema
ggen validate \
  --schema marketplace/clap-noun-verb/cli-patterns.json \
  src/main.rs

# Check type coverage (must be 100%)
ggen analyze --type-coverage src/

# Generate documentation
ggen schema --from src/main.rs --output docs/cli.md
```

## Integration with ggen Projects

### Use in ggen-cli crates
```rust
// crates/my-feature/src/cli.rs
#[noun("myfeature", "My feature")]
#[verb("process")]
fn cmd_process(input: String, format: String) -> Result<Output> {
    // Delegate to domain logic immediately
    let result = crate::domain::process(&input, &format)?;
    Ok(Output { result })
}
```

### Schema generation
```bash
ggen schema --from crates/my-feature/src/cli.rs \
  --output ontologies/my-feature-cli.ttl
```
```

---

## Validation Strategy

### Constitution Compliance Checklist

```toml
[validation.constitution]
# I. Crate-First Architecture
crate-independence = true
circular-dependencies = false

# III. Chicago TDD (Zero Tolerance)
test-coverage-minimum = 80
unwrap-in-production = 0
expect-in-production = 0

# V. Type-First Thinking
type-coverage = 100
newtypes-for-ids = true
exhaustive-matching = true

# VI. Andon Signal Protocol
compiler-warnings = 0
clippy-lint-strict = true
deny-warnings = true

# VII. Error Handling
result-types-in-public-api = true
no-panics-in-production = true

# VIII. Concurrent Execution
message-batching = true
parallel-agent-support = true

# IX. Lean Six Sigma Quality
security-audit = "cargo audit clean"
documentation-complete = true
reproducible-builds = true
```

### Validator Script

```bash
#!/bin/bash
# marketplace/packages/clap-noun-verb/hooks/validate.sh

echo "=== clap-noun-verb Marketplace Validation ==="

# 1. Type coverage
echo "Checking type coverage..."
cargo check --all-targets 2>&1 | grep -E "cannot find|unresolved" && exit 1

# 2. Test coverage
echo "Checking test coverage..."
cargo make test >/dev/null || exit 1

# 3. Unwrap/expect in production
echo "Checking for unwraps in production code..."
grep -r "\.unwrap()" src/ --exclude-dir=tests && exit 1
grep -r "\.expect(" src/ --exclude-dir=tests && exit 1

# 4. Documentation
echo "Checking documentation..."
cargo doc --no-deps 2>&1 | grep "warning: missing" && exit 1

# 5. Clippy
echo "Checking lints..."
cargo clippy --all-targets -- -D warnings >/dev/null || exit 1

echo "✅ All validation passed"
```

---

## Version Management Strategy

### Semantic Versioning

```
clap-noun-verb releases:
  v5.3.2 (current stable)
  v5.2.0 (Phase 2: typer-like syntax)
  v5.1.0 (Phase 1: core macros)
  v5.0.0 (major rewrite)
```

### Marketplace Pinning

```toml
# marketplace/packages/clap-noun-verb/package.toml
[version-constraints]
min-version = "5.3.0"      # Oldest supported version
max-version = "5.*"        # Allow 5.x.y
latest-tested = "5.3.2"    # Last verified to work

[breaking-changes]
v5.0.0 = "Major rewrite: macro syntax changed"
v6.0.0 = "TODO: Future breaking changes"

[migration-guides]
"4.x -> 5.x" = "docs/migration/4-to-5.md"
"5.0 -> 5.2" = "docs/migration/5.0-to-5.2.md"
```

---

## Distribution Strategy

### Three Channels

1. **crates.io** (Primary)
   - Source of truth
   - Version: `clap-noun-verb = "5.3"`
   - Documentation: docs.rs

2. **ggen Marketplace** (Secondary)
   - Integration layer
   - Includes templates, examples, schemas
   - Validated against ggen conventions

3. **GitHub Releases** (Archive)
   - Source code + changelogs
   - Pre-built binaries (if applicable)

### Installation Paths

```bash
# Via Cargo.toml
[dependencies]
clap-noun-verb = "5.3"

# Via ggen marketplace
ggen marketplace install --package clap-noun-verb

# From GitHub
cargo install --git https://github.com/seanchatmangpt/clap-noun-verb
```

---

## Phase Implementation Plan

### Phase 1: Package Structure (Week 1)
- [ ] Create `marketplace/packages/clap-noun-verb/` directory
- [ ] Copy examples to `examples/`
- [ ] Create package.toml with metadata
- [ ] Write README.md and USAGE.md

### Phase 2: Schemas & Validation (Week 2)
- [ ] Create `clap-noun-verb-schema.ttl` (RDF ontology)
- [ ] Create `cli-patterns.json` (validation rules)
- [ ] Write `hooks/validate.sh` script
- [ ] Implement marketplace validation checks

### Phase 3: Templates & Code Generation (Week 3)
- [ ] Create Tera templates for projects, nouns, verbs
- [ ] Register in ggen conventions
- [ ] Implement `ggen new --template clap-noun-verb`
- [ ] Create integration examples

### Phase 4: Documentation & Examples (Week 4)
- [ ] Write marketplace-specific quickstart
- [ ] Create 4 progressive examples (5m, 15m, 30m, 45m)
- [ ] Generate ontology from code
- [ ] Create migration guides

### Phase 5: Testing & Validation (Week 5)
- [ ] Test marketplace installation
- [ ] Validate Constitution compliance
- [ ] Run full validation suite
- [ ] Create CI/CD workflow

---

## Success Metrics

| Metric | Target | Method |
|--------|--------|--------|
| **Type Coverage** | 100% | `cargo check --all-targets` |
| **Test Coverage** | 80%+ | `cargo make test` |
| **Documentation** | 100% | `cargo doc --no-deps` |
| **Lint Clean** | 0 warnings | `cargo clippy --all-targets -- -D warnings` |
| **Marketplace Install** | <5s | Time marketplace install command |
| **Example Runtime** | <30s total | Run all 4 examples |
| **Schema Valid** | 100% | Validate TTL against RDF spec |
| **Constitution Compliance** | 100% | All validation checks pass |

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|-----------|
| **Breaking changes in clap** | High | Pin major version, maintain compatibility layer |
| **Performance regression** | Medium | Benchmark examples, SLO monitoring |
| **Schema complexity** | Medium | Start simple, iterate based on usage |
| **Integration friction** | Medium | Clear documentation, automated validation |
| **Version conflicts** | Low | Use workspace resolution, Cargo.lock |

---

## References

- Official Repository: https://github.com/seanchatmangpt/clap-noun-verb
- Crates.io: https://crates.io/crates/clap-noun-verb
- Docs: https://docs.rs/clap-noun-verb
- Phase 2 Analysis: docs/phase2-analysis.md (in repo)

---

## Next Steps

1. **Approve structure** - Validate marketplace layout
2. **Create spec** - Write `/speckit.specify "clap-noun-verb marketplace integration"`
3. **Implement** - Build package structure, schemas, templates
4. **Validate** - Run full validation suite against Constitution
5. **Test** - Marketplace install, examples, integration
6. **Launch** - Publish to marketplace with announcement

**Ready to proceed with specification phase?**
