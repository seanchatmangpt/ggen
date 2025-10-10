# ggen Cookbook

A comprehensive guide to using ggen for graph-aware code generation with practical recipes and real-world examples.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Template Basics](#template-basics)
3. [RDF & SPARQL Integration](#rdf--sparql-integration)
4. [Injection Patterns](#injection-patterns)
5. [Deterministic Generation](#deterministic-generation)
6. [Marketplace & Gpacks](#marketplace--gpacks)
7. [Advanced Techniques](#advanced-techniques)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)

---

## Getting Started

### Recipe: Your First Template

**Problem**: Create a simple template that generates a Rust "Hello World" program.

**Solution**: Use basic frontmatter with variable substitution.

**Code Example**:
```yaml
---
to: src/main.rs
vars:
  name: "World"
---
fn main() {
    println!("Hello, {{name}}!");
}
```

**Usage**:
```bash
ggen gen templates/hello.tmpl --vars name=Alice
```

**Output** (`src/main.rs`):
```rust
fn main() {
    println!("Hello, Alice!");
}
```

**Explanation**:
1. `to:` specifies the output file path
2. `vars:` defines default template variables
3. `{{name}}` is replaced with the provided value
4. Command-line `--vars` override defaults

**Source Reference**: [templates/rust.tmpl:1-8](/Users/sac/ggen/templates/rust.tmpl)

---

### Recipe: Dynamic Output Paths

**Problem**: Generate files with paths based on template variables.

**Solution**: Use Tera templating in the `to:` field.

**Code Example**:
```yaml
---
to: "src/{{module}}/{{name}}.rs"
vars:
  module: "utils"
  name: "helpers"
---
// {{module}}/{{name}} module
pub fn {{name}}_function() {
    // Implementation
}
```

**Usage**:
```bash
ggen gen template.tmpl --vars module=auth name=token
```

**Output Path**: `src/auth/token.rs`

**Explanation**:
- The `to:` field is rendered as a Tera template
- Variables work in both path and body
- Directories are auto-created if needed

**See Also**: [Injection Patterns](#injection-patterns)

---

## Template Basics

### Recipe: Template Variables

**Problem**: Define reusable variables with defaults and overrides.

**Solution**: Use the `vars:` frontmatter section with command-line overrides.

**Code Example**:
```yaml
---
to: "{{output_dir}}/config.yaml"
vars:
  app_name: "MyApp"
  version: "1.0.0"
  environment: "development"
  output_dir: "config"
---
app:
  name: {{app_name}}
  version: {{version}}
  environment: {{environment}}
```

**Usage**:
```bash
# Use defaults
ggen gen config.tmpl

# Override specific vars
ggen gen config.tmpl --vars environment=production version=2.0.0
```

**Best Practices**:
- ✅ Provide sensible defaults in `vars:`
- ✅ Use descriptive variable names
- ✅ Document required vs optional vars in comments
- ❌ Don't hardcode environment-specific values

**Source Reference**: [template.rs:52-53](/Users/sac/ggen/ggen-core/src/template.rs)

---

### Recipe: Tera Filters and Functions

**Problem**: Transform variables during rendering.

**Solution**: Use Tera's built-in filters and ggen's custom functions.

**Code Example**:
```yaml
---
to: "src/{{name | lower}}.rs"
vars:
  name: "MyModule"
  author: "john doe"
---
//! {{name | title}} Module
//! Author: {{author | capitalize}}

pub struct {{name | capitalize}} {
    // {{name | upper}}_VERSION constant
}

impl {{name | capitalize}} {
    pub fn new() -> Self {
        Self {}
    }
}
```

**Available Filters**:
- `lower` - Convert to lowercase
- `upper` - Convert to uppercase
- `capitalize` - Capitalize first letter
- `title` - Title case
- `trim` - Remove whitespace
- `length` - Get string length

**Custom Functions**:
- `sparql(query="...", var="...")` - Execute SPARQL query
- `local(iri="...")` - Extract local name from IRI

**Source Reference**: [tera_env.rs](/Users/sac/ggen/ggen-core/src/tera_env.rs)

---

### Recipe: Multi-File Templates

**Problem**: Generate multiple related files from a single template.

**Solution**: Create separate templates and orchestrate with a gpack.

**Project Structure**:
```
my-gpack/
├── ggen.yaml           # Pack manifest
├── model.tmpl          # Data model
├── controller.tmpl     # API controller
└── tests.tmpl          # Test suite
```

**ggen.yaml**:
```yaml
name: "io.myorg.api-scaffold"
version: "1.0.0"
description: "Generate complete API module"
templates:
  - model.tmpl
  - controller.tmpl
  - tests.tmpl
```

**Usage**:
```bash
ggen add io.myorg.api-scaffold
ggen gen io.myorg.api-scaffold:model.tmpl --vars resource=User
ggen gen io.myorg.api-scaffold:controller.tmpl --vars resource=User
ggen gen io.myorg.api-scaffold:tests.tmpl --vars resource=User
```

**See Also**: [Marketplace & Gpacks](#marketplace--gpacks)

---

## RDF & SPARQL Integration

### Recipe: Embed Semantic Metadata

**Problem**: Enrich generated code with semantic knowledge graphs.

**Solution**: Use `rdf_inline:` to embed Turtle triples directly in templates.

**Code Example**:
```yaml
---
to: "src/models/{{name}}.rs"
vars:
  name: "User"
prefixes:
  ex: "http://example.org/"
  foaf: "http://xmlns.com/foaf/0.1/"
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
  - "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  - "ex:{{name}} a ex:Model ; foaf:name '{{name}}' ."
---
/// {{name}} model with semantic metadata
#[derive(Debug, Clone)]
pub struct {{name}} {
    pub id: u64,
    pub name: String,
}
```

**Explanation**:
1. `prefixes:` declares namespace prefixes
2. `rdf_inline:` adds triples to the internal RDF graph
3. Variables work inside RDF triples
4. Graph persists across template rendering

**Benefits**:
- Query relationships between generated artifacts
- Validate against SHACL shapes
- Generate documentation from ontology
- Enable semantic search

**Source Reference**: [template.rs:111-139](/Users/sac/ggen/ggen-core/src/template.rs)

---

### Recipe: Load External RDF Files

**Problem**: Share RDF knowledge across multiple templates.

**Solution**: Use `rdf:` to reference external Turtle files.

**Code Example**:
```yaml
---
to: "src/api/{{endpoint}}.rs"
vars:
  endpoint: "users"
prefixes:
  api: "http://api.example.org/"
rdf:
  - "graphs/api-schema.ttl"
  - "graphs/{{endpoint}}-data.ttl"
---
// Generated from RDF schema
```

**graphs/api-schema.ttl**:
```turtle
@prefix api: <http://api.example.org/> .
@prefix http: <http://www.w3.org/2011/http#> .

api:users a api:Endpoint ;
    http:methodName "GET" ;
    api:path "/api/users" .
```

**File Resolution**:
- Paths are relative to the template directory
- Variables work in file paths
- Files are loaded in order
- Supports `.ttl`, `.nt`, `.rdf` formats

**Source Reference**: [template.rs:123-139](/Users/sac/ggen/ggen-core/src/template.rs), [graph.rs:146-166](/Users/sac/ggen/ggen-core/src/graph.rs)

---

### Recipe: SPARQL Queries in Templates

**Problem**: Query RDF data and use results in generated code.

**Solution**: Use `sparql:` frontmatter with named queries.

**Code Example**:
```yaml
---
to: "src/routes.rs"
prefixes:
  api: "http://api.example.org/"
  http: "http://www.w3.org/2011/http#"
rdf_inline:
  - "@prefix api: <http://api.example.org/> ."
  - "api:users http:methodName 'GET' ."
  - "api:posts http:methodName 'POST' ."
sparql:
  endpoints: |
    SELECT ?endpoint ?method WHERE {
      ?endpoint http:methodName ?method
    }
---
use axum::Router;

pub fn api_routes() -> Router {
    Router::new()
{% for row in sparql(query="endpoints") %}
        .route("/api/{{local(iri=row.endpoint)}}", {{row.method | lower}}_handler)
{% endfor %}
}
```

**SPARQL Functions**:

1. **`sparql(query="name")`** - Execute named query, returns array of results
2. **`sparql(query="name", var="variable")`** - Extract single variable from first result
3. **`local(iri="...")`** - Get local name from full IRI

**Query Results**:
- Returns JSON array of solution bindings
- Use in Tera loops: `{% for row in sparql(...) %}`
- Access variables: `{{row.varname}}`

**Caching**:
- Queries are cached by content hash
- Cache invalidated on graph modifications
- Plan cache (100 entries) + result cache (1000 entries)

**Source Reference**: [pipeline.rs:206-259](/Users/sac/ggen/ggen-core/src/pipeline.rs), [graph.rs:168-210](/Users/sac/ggen/ggen-core/src/graph.rs)

---

### Recipe: Complex SPARQL Patterns

**Problem**: Generate code based on complex graph patterns.

**Solution**: Use advanced SPARQL features with aggregation and filtering.

**Code Example**:
```yaml
---
to: "src/generated/stats.rs"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdf:
  - "data/entities.ttl"
sparql:
  type_counts: |
    SELECT ?type (COUNT(?entity) as ?count) WHERE {
      ?entity a ?type
    }
    GROUP BY ?type
    ORDER BY DESC(?count)

  top_entities: |
    SELECT ?entity ?name WHERE {
      ?entity ex:name ?name ;
              ex:priority ?priority .
      FILTER(?priority > 80)
    }
    ORDER BY DESC(?priority)
    LIMIT 10
---
/// Auto-generated statistics from RDF graph

pub mod stats {
    // Type distribution
{% for row in sparql(query="type_counts") %}
    pub const {{local(iri=row.type) | upper}}_COUNT: u32 = {{row.count}};
{% endfor %}

    // High priority entities
    pub const TOP_ENTITIES: &[&str] = &[
{% for row in sparql(query="top_entities") %}
        "{{row.name}}",
{% endfor %}
    ];
}
```

**Advanced SPARQL Features**:
- `GROUP BY` - Aggregate results
- `FILTER` - Conditional filtering
- `ORDER BY` - Sort results
- `LIMIT` / `OFFSET` - Pagination
- Aggregate functions: `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`
- `OPTIONAL` - Left joins
- Property paths: `ex:knows+` (transitive)

**See Also**: [W3C SPARQL Spec](https://www.w3.org/TR/sparql11-query/)

---

## Injection Patterns

### Recipe: Append to Existing File

**Problem**: Add new code to an existing file without overwriting.

**Solution**: Use `inject: true` with `append` mode.

**Code Example**:
```yaml
---
to: "src/lib.rs"
inject: true
append: true
skip_if: "pub mod {{name}}"
vars:
  name: "new_module"
---
pub mod {{name}};
```

**Before** (`src/lib.rs`):
```rust
pub mod utils;
pub mod config;
```

**After** (`src/lib.rs`):
```rust
pub mod utils;
pub mod config;
pub mod new_module;
```

**Explanation**:
- `inject: true` enables injection mode
- `append: true` adds to end of file
- `skip_if:` prevents duplicate injections
- File is created if it doesn't exist

**Source Reference**: [pipeline.rs:306-370](/Users/sac/ggen/ggen-core/src/pipeline.rs), [inject.rs:70-105](/Users/sac/ggen/ggen-core/src/inject.rs)

---

### Recipe: Inject After Pattern

**Problem**: Insert code after a specific marker line.

**Solution**: Use `after:` to specify the pattern to match.

**Code Example**:
```yaml
---
to: "src/routes.rs"
inject: true
after: "// ROUTES"
skip_if: "route_{{endpoint}}"
vars:
  endpoint: "users"
  method: "GET"
---
    .route("/api/{{endpoint}}", {{method | lower}}(route_{{endpoint}}))
```

**Before**:
```rust
pub fn api_routes() -> Router {
    Router::new()
        // ROUTES
        .route("/api/health", get(health_check))
}
```

**After**:
```rust
pub fn api_routes() -> Router {
    Router::new()
        // ROUTES
        .route("/api/users", get(route_users))
        .route("/api/health", get(health_check))
}
```

**Pattern Matching**:
- Uses substring match (not regex)
- Finds first occurrence
- If pattern not found, appends to end
- Case-sensitive matching

**Source Reference**: [pipeline.rs:449-462](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Recipe: Inject Before Pattern

**Problem**: Insert code before a specific line.

**Solution**: Use `before:` to specify the marker.

**Code Example**:
```yaml
---
to: "Cargo.toml"
inject: true
before: "[dev-dependencies]"
skip_if: "{{crate}}"
vars:
  crate: "serde"
  version: "1.0"
---
{{crate}} = "{{version}}"
```

**Before**:
```toml
[dependencies]
anyhow = "1.0"

[dev-dependencies]
tempfile = "3.0"
```

**After**:
```toml
[dependencies]
anyhow = "1.0"
serde = "1.0"

[dev-dependencies]
tempfile = "3.0"
```

**Source Reference**: [pipeline.rs:436-448](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Recipe: Idempotent Injection

**Problem**: Ensure injection only happens once, even if run multiple times.

**Solution**: Use `idempotent: true` with automatic content detection.

**Code Example**:
```yaml
---
to: "src/exports.rs"
inject: true
append: true
idempotent: true
vars:
  module: "auth"
---
pub use crate::{{module}}::*;
```

**Behavior**:
- First run: Injects content
- Subsequent runs: Skips if exact content exists
- Uses substring matching
- Works across injection modes

**Manual Control**:
```yaml
skip_if: "pub use crate::{{module}}"  # More specific pattern
```

**Source Reference**: [pipeline.rs:333-339](/Users/sac/ggen/ggen-core/src/pipeline.rs), [inject.rs:84-92](/Users/sac/ggen/ggen-core/src/inject.rs)

---

### Recipe: Inject at Specific Line

**Problem**: Insert code at an exact line number.

**Solution**: Use `at_line:` with 1-based line numbers.

**Code Example**:
```yaml
---
to: "README.md"
inject: true
at_line: 5
---
## New Section

This content is inserted at line 5.
```

**Note**: Line numbers are 1-based (line 1 is first line)

**Use Cases**:
- Programmatic file editing
- Deterministic insertion
- Build scripts

**Caution**: ⚠️ Fragile if file changes. Prefer pattern matching.

**Source Reference**: [pipeline.rs:463-470](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Recipe: Prepend to File

**Problem**: Add content at the beginning of a file.

**Solution**: Use `prepend: true`.

**Code Example**:
```yaml
---
to: "src/lib.rs"
inject: true
prepend: true
skip_if: "#![warn(clippy"
---
#![warn(clippy::all)]
#![deny(unsafe_code)]
```

**Before**:
```rust
pub mod utils;
```

**After**:
```rust
#![warn(clippy::all)]
#![deny(unsafe_code)]
pub mod utils;
```

**Common Uses**:
- License headers
- Compiler attributes
- File-level documentation
- Linter directives

**Source Reference**: [pipeline.rs:423-428](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

## Deterministic Generation

### Recipe: Reproducible Output

**Problem**: Generate identical output across runs and machines.

**Solution**: Use `determinism:` with a fixed seed.

**Code Example**:
```yaml
---
to: "src/generated.rs"
vars:
  count: 10
determinism: 42
---
// Deterministically generated constants
{% for i in range(end=count) %}
pub const ITEM_{{i}}: &str = "value_{{i}}";
{% endfor %}
```

**Benefits**:
- ✅ Byte-identical output every time
- ✅ Git-friendly (no spurious diffs)
- ✅ Reproducible builds
- ✅ Easier code review

**Seed Types**:
```yaml
determinism: 42                    # Integer seed
determinism: "my-project"          # String seed (hashed)
determinism:
  seed: 42
  sort_order: ["name", "type"]     # Control iteration order
```

**What Gets Fixed**:
- Random number generation
- Map/set iteration order
- Hash-based collections
- Time-based generation (if using RNG)

**Source Reference**: [template.rs:64](/Users/sac/ggen/ggen-core/src/template.rs)

---

### Recipe: Deterministic SPARQL Results

**Problem**: SPARQL query results have non-deterministic order.

**Solution**: Use `ORDER BY` in queries or `determinism.sort_order`.

**Code Example**:
```yaml
---
to: "src/types.rs"
determinism:
  seed: "types-gen"
  sort_order: ["name", "priority"]
rdf:
  - "schema.ttl"
sparql:
  all_types: |
    SELECT ?name ?priority WHERE {
      ?type ex:name ?name ;
            ex:priority ?priority .
    }
    ORDER BY ?priority ?name
---
// Types generated in deterministic order
{% for row in sparql(query="all_types") %}
pub struct {{row.name}} { }
{% endfor %}
```

**Best Practices**:
- ✅ Always use `ORDER BY` in SPARQL queries
- ✅ Sort by stable properties (names, IDs)
- ✅ Use `determinism` seed for RNG operations
- ❌ Don't rely on insertion order

---

## Marketplace & Gpacks

### Recipe: Search for Templates

**Problem**: Find reusable templates in the marketplace.

**Solution**: Use `ggen search` and `ggen categories`.

**Commands**:
```bash
# Search by keyword
ggen search "rust"
ggen search "cli subcommand"

# Browse categories
ggen categories

# View specific category
ggen search --category "rust-backend"
```

**Search Results**:
```
📦 io.ggen.rust.cli-subcommand
   ⭐ 4.5 | 📥 1,234 | v1.2.0
   Generate Clap CLI subcommands with proper error handling

📦 io.ggen.rust.api-endpoint
   ⭐ 4.8 | 📥 892 | v2.0.1
   Axum API endpoint with validation and tests
```

**Source Reference**: [cli/src/cmds/search.rs](/Users/sac/ggen/cli/src/cmds/)

---

### Recipe: Install and Use Gpacks

**Problem**: Add a template pack to your project.

**Solution**: Use `ggen add` and reference templates by pack ID.

**Workflow**:
```bash
# Add gpack to project
ggen add io.ggen.rust.cli-subcommand

# List installed gpacks
ggen packs

# Use template from gpack
ggen gen io.ggen.rust.cli-subcommand:rust.tmpl \
  --vars cmd=hello summary="Print greeting"

# Update all gpacks
ggen update
```

**ggen.lock** (auto-generated):
```yaml
packages:
  io.ggen.rust.cli-subcommand:
    version: "1.2.0"
    sha256: "abc123..."
    url: "https://marketplace.ggen.io/..."
```

**Benefits**:
- 📦 Versioned dependencies
- 🔒 SHA256 integrity checks
- 🔄 Easy updates
- 🤝 Team sharing via `ggen.lock`

**Source Reference**: [cli/src/cmds/add.rs](/Users/sac/ggen/cli/src/cmds/add.rs), [lockfile.rs](/Users/sac/ggen/ggen-core/src/lockfile.rs)

---

### Recipe: Create Your Own Gpack

**Problem**: Share templates across projects or with team.

**Solution**: Create a gpack directory with manifest.

**Directory Structure**:
```
my-gpack/
├── ggen.yaml          # Pack manifest
├── README.md          # Documentation
├── cli-cmd.tmpl       # Template 1
├── api-route.tmpl     # Template 2
├── graphs/
│   └── schema.ttl     # Shared RDF data
└── examples/
    └── usage.md       # Usage examples
```

**ggen.yaml**:
```yaml
name: "io.myorg.backend-scaffold"
version: "1.0.0"
description: "Backend scaffolding templates"
author: "Your Name <email@example.com>"
license: "MIT"
repository: "https://github.com/myorg/ggen-backend"

templates:
  - name: "cli-cmd.tmpl"
    description: "CLI command with args"
  - name: "api-route.tmpl"
    description: "REST API route handler"

dependencies:
  io.ggen.rust.common: "^1.0"

keywords:
  - rust
  - backend
  - cli
  - api
```

**Publishing**:
```bash
# Test locally
ggen add ./my-gpack

# Publish to marketplace (future)
ggen publish
```

**Source Reference**: [gpack.rs](/Users/sac/ggen/ggen-core/src/gpack.rs)

---

## Advanced Techniques

### Recipe: Shell Hooks

**Problem**: Run commands before/after code generation.

**Solution**: Use `sh_before:` and `sh_after:` frontmatter.

**Code Example**:
```yaml
---
to: "src/generated/api.rs"
sh_before: "cargo fmt -- --check"
sh_after: "cargo fmt && cargo clippy"
vars:
  endpoint: "users"
---
// Generated API endpoint
pub mod {{endpoint}} {
    // Implementation
}
```

**Execution Flow**:
1. Run `sh_before` (fails if non-zero exit)
2. Generate file
3. Run `sh_after`

**Common Uses**:
```yaml
sh_before: "git diff --exit-code"  # Fail if uncommitted changes
sh_after: "cargo test"              # Verify tests pass
sh_after: "prettier --write"        # Format output
sh_after: "git add ."               # Stage generated files
```

**Alias**:
```yaml
sh: "cargo fmt"  # Shorthand for sh_before
```

**Security Note**: ⚠️ Commands run with current user permissions

**Source Reference**: [pipeline.rs:356-367,393-406](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Recipe: Conditional Generation

**Problem**: Generate different code based on variables.

**Solution**: Use Tera conditionals and SPARQL filters.

**Code Example**:
```yaml
---
to: "src/{{feature}}.rs"
vars:
  feature: "auth"
  enable_oauth: "true"
  enable_jwt: "false"
---
// {{feature | title}} Module

{% if enable_oauth == "true" %}
pub mod oauth {
    pub fn authenticate() { }
}
{% endif %}

{% if enable_jwt == "true" %}
pub mod jwt {
    pub fn verify_token() { }
}
{% endif %}

{% if enable_oauth != "true" and enable_jwt != "true" %}
compile_error!("At least one auth method must be enabled");
{% endif %}
```

**Tera Conditionals**:
```django
{% if condition %}...{% endif %}
{% if x %}...{% elif y %}...{% else %}...{% endif %}
{% if x and y %}...{% endif %}
{% if x or y %}...{% endif %}
{% if not x %}...{% endif %}
```

**SPARQL Filters**:
```yaml
sparql:
  active_features: |
    SELECT ?feature WHERE {
      ?feature a ex:Feature ;
               ex:enabled true .
    }
---
{% for row in sparql(query="active_features") %}
pub mod {{local(iri=row.feature)}} { }
{% endfor %}
```

---

### Recipe: Template Inheritance

**Problem**: Share common template structure across files.

**Solution**: Use `from:` to load external template bodies.

**Code Example**:

**base-module.tmpl**:
```rust
//! {{name}} module
//! Auto-generated by ggen

#[derive(Debug, Clone)]
pub struct {{name | capitalize}} {
    {{fields}}
}

impl {{name | capitalize}} {
    {{methods}}
}
```

**user-model.tmpl**:
```yaml
---
to: "src/models/user.rs"
from: "templates/base-module.tmpl"
vars:
  name: "user"
  fields: |
    pub id: u64,
    pub email: String
  methods: |
    pub fn new(id: u64, email: String) -> Self {
        Self { id, email }
    }
---
```

**Explanation**:
- `from:` loads external file as template body
- Frontmatter block in referencing template is empty
- All variables work in the loaded template
- Paths are relative to current template

**Source Reference**: [template.rs:156-169](/Users/sac/ggen/ggen-core/src/template.rs)

---

### Recipe: Backup Files

**Problem**: Preserve original files before injection.

**Solution**: Use `backup: true` in frontmatter.

**Code Example**:
```yaml
---
to: "important-config.yaml"
inject: true
append: true
backup: true
---
new_setting: value
```

**Behavior**:
- Creates `important-config.yaml.backup` before modification
- Overwrites backup if it exists
- Original file is modified
- Useful for manual rollback

**Alternatives**:
- Use version control (recommended)
- `--dry` flag to preview changes
- Custom `sh_before:` to create backups

**Source Reference**: [pipeline.rs:342-346](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Recipe: Multi-Language Templates

**Problem**: Generate the same logic in multiple programming languages.

**Solution**: Use RDF to model logic, generate per language.

**Schema** (`logic.ttl`):
```turtle
@prefix ex: <http://example.org/> .

ex:CalculateDiscount a ex:Function ;
    ex:param [ ex:name "amount" ; ex:type "number" ] ;
    ex:param [ ex:name "percent" ; ex:type "number" ] ;
    ex:returns "number" ;
    ex:logic "amount * (percent / 100)" .
```

**Rust Template**:
```yaml
---
to: "rust/discount.rs"
rdf: ["logic.ttl"]
sparql:
  functions: "SELECT ?fn ?logic WHERE { ?fn a ex:Function ; ex:logic ?logic }"
---
{% for row in sparql(query="functions") %}
pub fn {{local(iri=row.fn) | lower}}(amount: f64, percent: f64) -> f64 {
    {{row.logic}}
}
{% endfor %}
```

**Python Template**:
```yaml
---
to: "python/discount.py"
rdf: ["logic.ttl"]
sparql:
  functions: "SELECT ?fn ?logic WHERE { ?fn a ex:Function ; ex:logic ?logic }"
---
{% for row in sparql(query="functions") %}
def {{local(iri=row.fn) | lower}}(amount: float, percent: float) -> float:
    return {{row.logic}}
{% endfor %}
```

**Benefits**:
- Single source of truth
- Language-agnostic logic
- Validation via SHACL
- Cross-language testing

---

## Common Patterns

### Pattern: CLI Subcommand Generator

**Use Case**: Scaffold new CLI subcommands with boilerplate.

**Template** (`cli-subcommand.tmpl`):
```yaml
---
to: "src/cmds/{{cmd}}.rs"
inject: false
unless_exists: true
vars:
  cmd: "example"
  summary: "Example command"
rdf_inline:
  - "@prefix cli: <http://cli.example.org/> ."
  - "cli:{{cmd}} a cli:Command ; cli:summary '{{summary}}' ."
---
use clap::Args;
use anyhow::Result;

#[derive(Args, Debug)]
pub struct {{cmd | title}}Args {
    /// Add your arguments here
}

pub fn run(args: &{{cmd | title}}Args) -> Result<()> {
    println!("{{summary}}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{cmd}}_command() {
        // Add tests
    }
}
```

**mod.rs injection**:
```yaml
---
to: "src/cmds/mod.rs"
inject: true
after: "// COMMANDS"
skip_if: "pub mod {{cmd}}"
vars:
  cmd: "example"
---
pub mod {{cmd}};
```

**Usage**:
```bash
ggen gen cli-subcommand.tmpl --vars cmd=hello summary="Print greeting"
ggen gen mod-injection.tmpl --vars cmd=hello
```

**Real Example**: [examples/cli-subcommand/rust.tmpl:1-29](/Users/sac/ggen/examples/cli-subcommand/rust.tmpl)

---

### Pattern: REST API Endpoint

**Use Case**: Generate REST API route handlers with consistent structure.

**Template**:
```yaml
---
to: "src/api/{{resource | lower}}.rs"
vars:
  resource: "User"
  method: "GET"
rdf_inline:
  - "api:{{resource}} http:method '{{method}}' ."
---
use axum::{extract::Path, routing::{{method | lower}}, Router, Json};
use serde::{Deserialize, Serialize};
use anyhow::Result;

#[derive(Debug, Serialize, Deserialize)]
pub struct {{resource}} {
    pub id: u64,
    pub name: String,
}

pub async fn {{resource | lower}}_handler(
    Path(id): Path<u64>
) -> Result<Json<{{resource}}>, StatusCode> {
    // Implementation
    Ok(Json({{resource}} {
        id,
        name: "Example".into(),
    }))
}

pub fn {{resource | lower}}_routes() -> Router {
    Router::new()
        .route("/api/{{resource | lower}}/:id", {{method | lower}}({{resource | lower}}_handler))
}
```

**Real Example**: [examples/api-endpoint/rust.tmpl:1-30](/Users/sac/ggen/examples/api-endpoint/rust.tmpl)

---

### Pattern: Database Model + Migration

**Use Case**: Generate model and corresponding SQL migration.

**Model Template**:
```yaml
---
to: "src/models/{{table}}.rs"
vars:
  table: "users"
  fields:
    - { name: "id", type: "i64", primary: true }
    - { name: "email", type: "String" }
    - { name: "created_at", type: "DateTime<Utc>" }
---
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct {{table | title | singular}} {
{% for field in fields %}
    pub {{field.name}}: {{field.type}},
{% endfor %}
}
```

**Migration Template**:
```yaml
---
to: "migrations/{{timestamp}}_create_{{table}}.sql"
vars:
  table: "users"
  fields:
    - { name: "id", type: "BIGSERIAL", constraints: "PRIMARY KEY" }
    - { name: "email", type: "VARCHAR(255)", constraints: "UNIQUE NOT NULL" }
    - { name: "created_at", type: "TIMESTAMPTZ", constraints: "DEFAULT NOW()" }
---
-- Create {{table}} table
CREATE TABLE {{table}} (
{% for field in fields %}
    {{field.name}} {{field.type}} {{field.constraints}}{% if not loop.last %},{% endif %}
{% endfor %}
);

CREATE INDEX idx_{{table}}_email ON {{table}}(email);
```

---

### Pattern: Test Suite Generator

**Use Case**: Generate comprehensive test files for modules.

**Template**:
```yaml
---
to: "tests/{{module}}_test.rs"
vars:
  module: "auth"
  functions: ["login", "logout", "verify"]
---
use super::{{module}}::*;
use anyhow::Result;

#[cfg(test)]
mod {{module}}_tests {
    use super::*;

{% for func in functions %}
    #[test]
    fn test_{{func}}_success() -> Result<()> {
        // Arrange

        // Act
        let result = {{func}}(/* args */)?;

        // Assert
        assert!(result.is_ok());
        Ok(())
    }

    #[test]
    fn test_{{func}}_failure() -> Result<()> {
        // Test error cases
        Ok(())
    }
{% endfor %}

    #[test]
    fn test_{{module}}_integration() -> Result<()> {
        // Integration test
        Ok(())
    }
}
```

---

## Troubleshooting

### Problem: Template Not Found

**Symptom**: `Error: Template not found: my-template.tmpl`

**Solutions**:

1. **Check file path**:
```bash
ls -la templates/my-template.tmpl  # Verify file exists
ggen gen ./templates/my-template.tmpl  # Use relative path
```

2. **Use pack reference**:
```bash
ggen list  # Show installed gpacks
ggen gen io.ggen.rust:my-template.tmpl  # Full pack reference
```

3. **Check ggen.lock**:
```bash
cat ggen.lock  # Verify pack is installed
ggen update    # Update pack cache
```

---

### Problem: Variable Not Substituted

**Symptom**: Output contains literal `{{name}}` instead of value

**Causes**:

1. **Escaping issue** - Use `{{ "{{" }}name}}` for literal braces
2. **Variable not defined**:
```yaml
vars:
  module: "auth"  # Define in frontmatter
# Or
ggen gen template.tmpl --vars module=auth
```

3. **Typo in variable name** - Check spelling matches exactly

**Debug**:
```yaml
---
to: "debug.txt"
---
Vars: {{ __all__ | json_encode(pretty=true) }}
```

---

### Problem: SPARQL Query Returns Empty

**Symptom**: Template generates but SPARQL results are empty

**Debug Steps**:

1. **Check RDF loading**:
```yaml
sparql:
  count: "SELECT (COUNT(*) as ?cnt) WHERE { ?s ?p ?o }"
---
Triple count: {{ sparql(query="count", var="cnt") }}
```

2. **Verify prefixes**:
```yaml
prefixes:
  ex: "http://example.org/"  # Must match RDF
rdf_inline:
  - "@prefix ex: <http://example.org/> ."  # Same URI
  - "ex:alice ex:knows ex:bob ."
sparql:
  test: "SELECT ?s WHERE { ?s ex:knows ?o }"  # Use prefix
```

3. **Test query syntax**:
```bash
# Use SPARQL playground
curl -X POST https://sparql.example.org \
  -H "Content-Type: application/sparql-query" \
  --data "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
```

4. **Check query results**:
```yaml
---
{% set results = sparql(query="test") %}
Debug: {{ results | json_encode(pretty=true) }}
{% for row in results %}
  Row: {{ row | json_encode() }}
{% endfor %}
---
```

**Source Reference**: [graph.rs:212-225](/Users/sac/ggen/ggen-core/src/graph.rs)

---

### Problem: Injection Not Working

**Symptom**: File unchanged after injection template

**Checklist**:

1. **Verify `inject: true`**:
```yaml
inject: true  # Required for injection
append: true  # Or other injection mode
```

2. **Check `skip_if` pattern**:
```yaml
skip_if: "pub mod {{name}}"  # May be matching incorrectly
# Add debug:
sh_after: "cat {{to}}"  # View result
```

3. **File permissions**:
```bash
ls -la src/lib.rs  # Check write permissions
chmod 644 src/lib.rs
```

4. **Pattern not found** (for `before:`/`after:`):
```yaml
# If pattern missing, injection appends
after: "// EXACT TEXT TO MATCH"  # Must match exactly
```

5. **EOL issues**:
```yaml
# Windows CRLF vs Unix LF
sh_before: "dos2unix src/lib.rs"  # Normalize line endings
```

**Source Reference**: [pipeline.rs:295-304](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

### Problem: Determinism Not Working

**Symptom**: Output differs between runs despite `determinism` seed

**Causes**:

1. **Time-based generation**:
```yaml
# Bad
timestamp: "{{ now() }}"  # Changes every run

# Good
determinism: 42
timestamp: "{{ 1234567890 }}"  # Fixed value
```

2. **SPARQL result order**:
```yaml
# Add ORDER BY
sparql:
  items: "SELECT ?item WHERE { ?item a ex:Thing } ORDER BY ?item"
```

3. **External file changes**:
```yaml
rdf: ["data.ttl"]  # If data.ttl changes, output changes
```

4. **Non-deterministic code**:
```rust
// Template generates this:
use std::collections::HashMap;  // Non-deterministic iteration
// Use BTreeMap instead for determinism
```

**Verification**:
```bash
# Generate twice and compare
ggen gen template.tmpl -o out1/
ggen gen template.tmpl -o out2/
diff -r out1/ out2/  # Should be identical
```

---

### Problem: Graph Memory Issues

**Symptom**: Out of memory or slow performance with large RDF files

**Solutions**:

1. **Use file loading** instead of inline:
```yaml
# Slow
rdf_inline: |
  <1000+ lines of RDF>

# Fast
rdf: ["large-file.ttl"]
```

2. **Filter SPARQL results**:
```yaml
sparql:
  # Avoid SELECT *
  data: "SELECT ?needed WHERE { ?s ex:prop ?needed } LIMIT 100"
```

3. **Clear cache** between generations:
```bash
rm -rf .ggen/cache
```

4. **Split into multiple templates**:
```yaml
# template1.tmpl - loads partial RDF
# template2.tmpl - loads different partial RDF
```

**Cache Stats**:
- Plan cache: 100 queries
- Result cache: 1000 result sets
- LRU eviction

**Source Reference**: [graph.rs:59-71](/Users/sac/ggen/ggen-core/src/graph.rs)

---

### Problem: Shell Hook Fails

**Symptom**: `Error: Shell hook failed: <command>`

**Debug**:

1. **Check command**:
```bash
# Test manually
sh -c "your-command"
```

2. **View stderr**:
```yaml
sh_after: "cargo fmt 2>&1"  # Capture stderr
```

3. **Use absolute paths**:
```yaml
sh_after: "/usr/bin/cargo fmt"
```

4. **Check working directory**:
```yaml
sh_before: "pwd && ls -la"  # Verify location
```

5. **Tolerate failures** (advanced):
```yaml
sh_after: "cargo fmt || true"  # Ignore exit code
```

**Source Reference**: [pipeline.rs:481-509](/Users/sac/ggen/ggen-core/src/pipeline.rs)

---

## Appendix

### Frontmatter Reference

Complete list of frontmatter fields:

**Output Control**:
- `to:` - Output file path (supports Tera)
- `from:` - Load template body from external file
- `force:` - Overwrite existing files
- `unless_exists:` - Skip if file exists

**Injection**:
- `inject:` - Enable injection mode
- `append:` - Append to end of file
- `prepend:` - Prepend to start of file
- `before:` - Insert before pattern
- `after:` - Insert after pattern
- `at_line:` - Insert at line number
- `skip_if:` - Skip if pattern matches
- `idempotent:` - Skip if content exists

**RDF/SPARQL**:
- `base:` - Base IRI for RDF
- `prefixes:` - Namespace prefix mappings
- `rdf:` - External RDF file paths
- `rdf_inline:` - Inline Turtle triples
- `sparql:` - Named SPARQL queries

**Variables**:
- `vars:` - Template variable defaults

**Hooks**:
- `sh_before:` (alias: `sh:`) - Run before generation
- `sh_after:` - Run after generation

**Safety**:
- `backup:` - Create .backup file
- `eof_last:` - Ensure newline at end

**Advanced**:
- `determinism:` - Seed for reproducibility
- `shape:` - SHACL shapes for validation

**Source Reference**: [template.rs:10-65](/Users/sac/ggen/ggen-core/src/template.rs)

---

### Tera Filter Reference

**String Filters**:
- `lower` - Lowercase
- `upper` - Uppercase
- `capitalize` - Capitalize first letter
- `title` - Title case
- `trim` - Remove whitespace
- `truncate(len)` - Limit length
- `wordcount` - Count words
- `replace(from, to)` - Replace substring

**Array/Collection**:
- `length` - Get length
- `first` - First element
- `last` - Last element
- `join(sep)` - Join with separator
- `sort` - Sort array
- `reverse` - Reverse order
- `unique` - Remove duplicates

**Formatting**:
- `json_encode` - JSON stringify
- `urlencode` - URL encoding
- `escape` - HTML escape

**Custom (ggen)**:
- `sparql(query, var?)` - Execute SPARQL
- `local(iri)` - Extract local name from IRI

---

### Tera Control Structures

**Conditionals**:
```django
{% if condition %}...{% endif %}
{% if x %}...{% elif y %}...{% else %}...{% endif %}
```

**Loops**:
```django
{% for item in items %}
  {{ item }}
{% endfor %}

{% for i in range(end=10) %}
  {{ i }}
{% endfor %}

{% for key, value in map %}
  {{ key }}: {{ value }}
{% endfor %}
```

**Loop Variables**:
- `loop.index` - Current iteration (1-indexed)
- `loop.index0` - Current iteration (0-indexed)
- `loop.first` - True if first iteration
- `loop.last` - True if last iteration

**Macros**:
```django
{% macro input(name, type="text") %}
  <input name="{{ name }}" type="{{ type }}">
{% endmacro %}

{{ input(name="email", type="email") }}
```

**Set Variables**:
```django
{% set name = "value" %}
{% set result = sparql(query="test") %}
```

---

### Common SPARQL Patterns

**Count**:
```sparql
SELECT (COUNT(?item) AS ?count) WHERE {
  ?item a ex:Thing .
}
```

**Aggregation**:
```sparql
SELECT ?type (COUNT(?item) AS ?count) WHERE {
  ?item a ?type .
}
GROUP BY ?type
ORDER BY DESC(?count)
```

**Property Paths**:
```sparql
# Direct or transitive
?person foaf:knows+ ?friend .

# Zero or more
?person foaf:knows* ?connection .

# Inverse
?person ^foaf:knows ?follower .
```

**Optional Patterns**:
```sparql
SELECT ?person ?name ?email WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
}
```

**Filter**:
```sparql
SELECT ?person WHERE {
  ?person ex:age ?age .
  FILTER(?age >= 18)
}
```

**Bind**:
```sparql
SELECT ?person ?fullName WHERE {
  ?person foaf:firstName ?first ;
          foaf:lastName ?last .
  BIND(CONCAT(?first, " ", ?last) AS ?fullName)
}
```

---

### Performance Tips

**RDF Loading**:
- ✅ Use external files for large datasets
- ✅ Load common data once in shared templates
- ❌ Don't use `rdf_inline` for >100 triples

**SPARQL Queries**:
- ✅ Use `LIMIT` for large result sets
- ✅ Add `ORDER BY` for determinism
- ✅ Filter early in query (use `FILTER`)
- ❌ Avoid `SELECT *` unless needed

**Caching**:
- ✅ Reuse template variables
- ✅ Same queries get cached
- ❌ Don't dynamically generate query strings

**File Operations**:
- ✅ Use `unless_exists` to skip regeneration
- ✅ Batch multiple templates in scripts
- ❌ Don't generate unchanged files

**Template Design**:
- ✅ Keep templates focused (<200 lines)
- ✅ Use template inheritance via `from:`
- ✅ Modularize with multiple templates
- ❌ Don't create monolithic templates

---

## Further Resources

- **Official Docs**: https://seanchatmangpt.github.io/ggen/
- **GitHub**: https://github.com/seanchatmangpt/ggen
- **Tera Docs**: https://tera.netlify.app/
- **SPARQL Spec**: https://www.w3.org/TR/sparql11-query/
- **RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **Marketplace**: https://marketplace.ggen.io (future)

---

**Last Updated**: 2025-10-09
**ggen Version**: 0.2.4
**Cookbook Version**: 1.0.0
