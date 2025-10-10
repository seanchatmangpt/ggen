# ggen Codebase Analysis: Patterns for Cookbook

**Date:** 2025-10-09
**Analyst:** CodebaseAnalyst Agent
**Purpose:** Identify key patterns, use cases, and best practices for ggen cookbook documentation

## Executive Summary

ggen is a **deterministic, language-agnostic code generation framework** that treats software artifacts as projections of knowledge graphs. Built in Rust, it combines:
- **Template processing** (Tera engine with YAML frontmatter)
- **RDF/SPARQL graph operations** (Oxigraph store)
- **Package management** (gpack registry system)
- **CLI tooling** (clap-based commands)

**Core Innovation:** Templates can embed RDF data and SPARQL queries to drive generation from semantic knowledge graphs.

---

## 1. PROJECT ARCHITECTURE

### Module Structure
```
ggen/
├── ggen-core/         # Core generation engine
│   ├── template.rs    # Frontmatter parsing & rendering
│   ├── graph.rs       # RDF store with caching
│   ├── pipeline.rs    # Rendering pipeline
│   ├── gpack.rs       # Package manifest handling
│   ├── registry.rs    # Marketplace client
│   ├── lockfile.rs    # Dependency locking
│   └── inject.rs      # Code injection utilities
├── cli/               # CLI commands
│   └── cmds/          # Individual command modules
├── utils/             # Shared utilities
└── tests/
    └── bdd/features/  # Cucumber BDD tests
```

### Key Design Patterns

**1. Pipeline Pattern**
- `PipelineBuilder` → `Pipeline` → `Plan` → `apply()`
- Immutable builder pattern for configuration
- Separation of rendering from execution

**2. Template Processing Flow**
```rust
Parse → RenderFrontmatter → ProcessGraph → RenderBody → Plan
```

**3. Graph-Aware Generation**
- RDF data loaded from files or inline Turtle
- SPARQL queries extract variables for templates
- Cached query results for performance

---

## 2. TEMPLATE SYSTEM PATTERNS

### Frontmatter Structure

**Basic Template:**
```yaml
---
to: "src/cmds/{{cmd}}.rs"
vars:
  cmd: "hello"
  summary: "Print a greeting"
---
pub fn {{cmd}}() {
    println!("{{summary}}");
}
```

**RDF-Enhanced Template:**
```yaml
---
to: "src/cmds/{{cmd}}.rs"
prefixes:
  cli: "urn:ggen:cli#"
base: "http://example.org/"
rdf:
  - "graphs/cli.ttl"
rdf_inline:
  - "@prefix cli: <urn:ggen:cli#> . cli:hello a cli:Command ."
sparql:
  commands: "SELECT ?cmd ?summary WHERE { ?cmd a cli:Command ; cli:summary ?summary }"
shape:
  - "graphs/shapes/cli.shacl.ttl"
determinism:
  seed: "cli-subcommand"
  sort_order: ["cmd", "summary"]
---
```

### Template Features

| Feature | Purpose | Example |
|---------|---------|---------|
| `to` | Output path (templated) | `src/{{name}}.rs` |
| `from` | Source file to use as body | `base-template.rs` |
| `vars` | Default variables | `{cmd: "hello"}` |
| `inject` | Code injection mode | `true` |
| `before/after` | Injection anchors | `"// IMPORTS"` |
| `skip_if` | Idempotency check | `"pub fn hello"` |
| `force` | Overwrite existing | `true` |
| `unless_exists` | Create only if missing | `true` |
| `sh_before/sh_after` | Shell hooks | `"cargo fmt"` |

### Injection Patterns

**Append to File:**
```yaml
---
to: "src/lib.rs"
inject: true
append: true
skip_if: "mod {{module_name}}"
---
pub mod {{module_name}};
```

**Insert at Specific Line:**
```yaml
---
to: "Cargo.toml"
inject: true
after: "[dependencies]"
skip_if: "serde"
---
serde = "1.0"
```

**Prepend to File:**
```yaml
---
to: "src/main.rs"
inject: true
prepend: true
skip_if: "use anyhow"
---
use anyhow::Result;
```

---

## 3. RDF/SPARQL PATTERNS

### Graph Loading Strategies

**1. External RDF Files:**
```yaml
rdf:
  - "graphs/schema.ttl"
  - "graphs/entities.ttl"
```

**2. Inline Turtle:**
```yaml
rdf_inline:
  - |
    @prefix ex: <http://example.org/> .
    ex:{{entity}} a ex:{{type}} ;
      ex:name "{{name}}" .
```

**3. Prefix Management:**
```yaml
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
base: "http://example.org/{{namespace}}/"
```

### SPARQL Query Patterns

**Variable Extraction:**
```yaml
sparql:
  entities: |
    PREFIX ex: <http://example.org/>
    SELECT ?name ?type WHERE {
      ?entity a ex:{{entity_type}} ;
        ex:name ?name ;
        ex:type ?type .
    }
```

**Count Aggregation:**
```yaml
sparql:
  count: |
    SELECT (COUNT(?item) AS ?total) WHERE {
      ?item a ex:{{type}} .
    }
```

**Conditional Generation:**
```yaml
sparql:
  has_tests: |
    ASK { ?module ex:hasTests true }
```

### Template Function Usage

**In-template SPARQL:**
```jinja2
{% set results = sparql(query="SELECT ?name WHERE { ?x ex:name ?name }") %}
{% for row in results %}
  pub struct {{row.name}} {}
{% endfor %}
```

**Extract Single Value:**
```jinja2
{% set name = sparql(query="SELECT ?name WHERE { ... }", var="name") %}
```

---

## 4. GPACK (PACKAGE) SYSTEM

### Manifest Structure (`gpack.toml`)

```toml
[gpack]
id = "io.ggen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.1.0"
description = "Generate clap subcommands"
license = "MIT"
ggen_compat = ">=0.1 <0.2"

[dependencies]
"io.ggen.macros.std" = "^0.1"

[templates]
patterns = ["cli/subcommand/*.tmpl"]
includes = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
patterns = ["templates/**/graphs/*.ttl"]

[queries]
patterns = ["../queries/*.rq"]
aliases.component_by_name = "../queries/component_by_name.rq"

[shapes]
patterns = ["../shapes/*.ttl"]

[preset]
vars = { author = "Acme", license = "MIT" }
```

### Convention-Based Discovery

**Default Patterns:**
- Templates: `templates/**/*.tmpl`, `templates/**/*.tera`
- RDF: `templates/**/graphs/*.ttl`
- Queries: `templates/**/queries/*.rq`
- Shapes: `templates/**/shapes/*.ttl`

---

## 5. CLI COMMAND PATTERNS

### Core Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen gen` | Generate from template | `ggen gen rust:main.tmpl -v name=app` |
| `ggen market search` | Search registry | `ggen market search rust cli` |
| `ggen market add` | Install gpack | `ggen market add io.ggen.rust.cli` |
| `ggen template lint` | Validate templates | `ggen template lint templates/` |
| `ggen graph export` | Export RDF data | `ggen graph export --format turtle` |
| `ggen completion` | Shell completions | `ggen completion bash` |

### Template Reference Formats

**Local Template:**
```bash
ggen gen templates/rust.tmpl -v name=hello
```

**Gpack Template:**
```bash
ggen gen io.ggen.rust.cli:cli/subcommand/rust.tmpl -v cmd=hello
```

**With Variables:**
```bash
ggen gen rust:main.tmpl \
  -v name=myapp \
  -v author="Sean Chatman" \
  -v version=0.1.0
```

---

## 6. DETERMINISM & REPRODUCIBILITY

### Deterministic Features

**1. Lockfile Management (`ggen.lock`):**
```toml
[[pack]]
id = "io.ggen.rust.cli"
version = "0.1.0"
git_url = "https://github.com/..."
git_rev = "abc123..."
sha256 = "deadbeef..."
```

**2. Seed-Based Generation:**
```yaml
determinism:
  seed: "my-template-v1"
  sort_order: ["name", "type"]
```

**3. SHA256 Verification:**
- All gpack downloads verified against registry SHA256
- Lockfile ensures reproducible builds

### Tracing & Observability

**Simple Tracing System:**
- Template processing metrics
- RDF loading statistics
- SPARQL query performance
- File operation tracking

---

## 7. MULTI-LANGUAGE SUPPORT

### Language-Specific Templates

**Rust:**
```yaml
---
to: "src/{{module}}.rs"
sh_after: "cargo fmt && cargo clippy"
---
```

**Python:**
```yaml
---
to: "{{package}}/{{module}}.py"
sh_after: "black {{to}} && isort {{to}}"
---
```

**Bash:**
```yaml
---
to: "scripts/{{name}}.sh"
sh_after: "chmod +x {{to}}"
---
#!/bin/bash
```

**TypeScript:**
```yaml
---
to: "src/{{name}}.ts"
sh_after: "prettier --write {{to}}"
---
```

### Tera Filters & Functions

**Built-in Filters:**
- `{{name | title}}` - Title case
- `{{name | upper}}` - Uppercase
- `{{name | lower}}` - Lowercase
- `{{name | snake_case}}` - Snake case
- `{{name | camel_case}}` - Camel case
- `{{name | pascal_case}}` - Pascal case

**Custom Functions:**
- `sparql()` - Execute SPARQL query
- `local()` - Generate local IRIs

---

## 8. TESTING PATTERNS

### BDD Test Structure

**Feature File Example:**
```gherkin
Feature: Template Generation
  Scenario: Basic template with frontmatter
    Given I have a template with:
      """
      ---
      to: src/{{name}}.rs
      ---
      pub fn {{name}}() {}
      """
    When I run "ggen gen test-template"
    Then the file "src/hello.rs" should exist
```

### Integration Test Patterns

**Template Validation:**
```rust
#[test]
fn test_template_render() -> Result<()> {
    let mut pipeline = Pipeline::new()?;
    let vars = BTreeMap::from([
        ("name".to_string(), "test".to_string())
    ]);
    let plan = pipeline.render_file(template_path, &vars, false)?;
    assert_eq!(plan.output_path, PathBuf::from("src/test.rs"));
    Ok(())
}
```

---

## 9. COMMON WORKFLOWS

### Workflow 1: CLI Subcommand Generation

**Goal:** Generate a new CLI subcommand with boilerplate

**Steps:**
1. Define RDF schema for CLI structure
2. Create template with SPARQL queries
3. Generate subcommand with metadata
4. Inject module registration

**Template Pattern:**
```yaml
---
to: "src/cmds/{{cmd}}.rs"
rdf_inline:
  - "@prefix cli: <urn:ggen:cli#> . cli:{{cmd}} a cli:Command ."
sparql:
  validate: "ASK { cli:{{cmd}} a cli:Command }"
---
use clap::Args;

#[derive(Args, Debug)]
pub struct {{cmd|title}}Args {
    // ...
}

pub fn run(args: &{{cmd|title}}Args) -> Result<()> {
    // ...
}
```

### Workflow 2: API Endpoint Generation

**Goal:** Generate REST API endpoints from OpenAPI-like RDF schema

**Steps:**
1. Define API schema in Turtle
2. Query endpoints with SPARQL
3. Generate handlers and routes
4. Inject into main router

### Workflow 3: Database Schema to Structs

**Goal:** Generate Rust structs from RDF database schema

**Steps:**
1. Load database schema as RDF
2. Query for tables/columns
3. Generate Rust structs with derives
4. Add validation annotations

---

## 10. KEY COOKBOOK RECIPES TO DOCUMENT

Based on this analysis, here are the **top 10 cookbook recipes** users would benefit from:

### 1. **Quick Start: Your First Template**
- Basic frontmatter structure
- Variable substitution
- Output path configuration

### 2. **Template Injection Patterns**
- Append/prepend to existing files
- Anchor-based injection (before/after)
- Idempotent generation with `skip_if`

### 3. **Working with RDF Graphs**
- Loading external Turtle files
- Inline RDF in frontmatter
- Prefix management and base IRIs

### 4. **SPARQL-Driven Generation**
- Variable extraction from queries
- Conditional generation with ASK
- Aggregation and filtering

### 5. **Creating a Gpack**
- Manifest structure (`gpack.toml`)
- Convention-based file discovery
- Publishing to marketplace

### 6. **Multi-Language Templates**
- Language-specific patterns
- Shell hook integration
- Formatter/linter automation

### 7. **Deterministic Builds**
- Lockfile usage
- Seed-based generation
- SHA256 verification

### 8. **CLI Scaffolding Recipe**
- End-to-end CLI subcommand generation
- Module registration patterns
- Integration testing

### 9. **Code Generation from OpenAPI**
- RDF mapping of OpenAPI schemas
- API endpoint generation
- Client SDK generation

### 10. **Database-First Development**
- Schema to struct generation
- Migration generation
- CRUD boilerplate

---

## 11. BEST PRACTICES IDENTIFIED

### Template Design
✅ **DO:**
- Use descriptive variable names
- Include default values in `vars`
- Document expected variables in comments
- Use `skip_if` for idempotency
- Normalize EOL for cross-platform compatibility

❌ **DON'T:**
- Hardcode paths or platform-specific values
- Skip validation with `force: true` unnecessarily
- Ignore shell hook exit codes
- Mix multiple concerns in one template

### RDF/SPARQL Usage
✅ **DO:**
- Define clear prefix namespaces
- Use SHACL shapes for validation
- Cache queries with semantic versioning
- Separate schema from instance data

❌ **DON'T:**
- Query in loops (use SPARQL aggregation)
- Store large binary data in RDF
- Skip graph clearing between renders

### Gpack Distribution
✅ **DO:**
- Version dependencies with semver
- Include comprehensive examples
- Document required variables
- Provide shape validation

❌ **DON'T:**
- Publish without testing
- Skip lockfile updates
- Use wildcard dependencies in production

---

## 12. ARCHITECTURAL INSIGHTS

### Performance Optimizations
1. **LRU Caching:** Query plans and results cached by epoch
2. **Parallel Graph Ops:** Thread-safe Arc-wrapped store
3. **Lazy Rendering:** Frontmatter rendered before body
4. **Deterministic Ordering:** Sorted file discovery

### Security Considerations
1. **SHA256 Verification:** All downloads validated
2. **No Code Execution in Templates:** Only data substitution
3. **Shell Hooks:** Explicit opt-in with sandboxing
4. **Registry HTTPS:** Enforced in production

### Extensibility Points
1. **Tera Custom Functions:** Register new template functions
2. **Custom Filters:** Add domain-specific transformations
3. **RDF Formats:** Support NTriples, RDF/XML, JSON-LD
4. **Shape Validation:** SHACL constraint checking

---

## CONCLUSION

ggen represents a **paradigm shift** from imperative code generation to **declarative, graph-aware generation**. Key innovations:

1. **Semantic Templates:** RDF/SPARQL elevates templates from string substitution to knowledge projection
2. **Deterministic by Design:** Lockfiles + SHA256 + seeds ensure reproducibility
3. **Language-Agnostic:** Tera templates work for any language
4. **Marketplace Ecosystem:** Gpack system enables template sharing

**Primary Use Cases:**
- **Scaffolding:** CLI tools, APIs, database schemas
- **Boilerplate Reduction:** DRY principle via template reuse
- **Knowledge-Driven:** Generate code from semantic models
- **Consistency Enforcement:** Templates as contracts

**Cookbook Priority:**
Focus on **practical recipes** that demonstrate the RDF/SPARQL advantage over traditional templating. Show how semantic graphs enable:
- Multi-target generation from single source
- Constraint-based validation
- Relationship-aware code generation

---

**Analysis Complete**
**Next Steps:** Use this analysis to structure cookbook chapters and select example projects.
