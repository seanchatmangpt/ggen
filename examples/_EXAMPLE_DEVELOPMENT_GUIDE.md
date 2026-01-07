# ggen Example Development Guide

**Version**: 1.0
**Last Updated**: 2026-01-07
**Purpose**: Guide for reimplementing ggen examples using the holographic orchestration framework

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Shared Infrastructure](#shared-infrastructure)
3. [RDF-Driven Examples](#rdf-driven-examples)
4. [Specification Closure](#specification-closure)
5. [Template Development](#template-development)
6. [Quality Gates & Andon Signals](#quality-gates--andon-signals)
7. [Acceptance Criteria](#acceptance-criteria)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)

---

## Quick Start

### 1. Create Example Directory

```bash
mkdir -p /home/user/ggen/examples/my-example/{ontology,templates,generated,tests}
```

### 2. Copy Shared Files

```bash
# For RDF-driven examples
cp ../_shared_templates/model_base.tmpl ./templates/
cp ../_validation_rules.ttl ./

# For lifecycle management
cp ../_shared_templates/make.toml.template ./make.toml
```

### 3. Create Cargo.toml

```toml
[package]
name = "ggen-example-my-example"
version = "0.1.0"
edition = "2021"

[dependencies]
ggen-core = { path = "../../crates/ggen-core" }
tera = "1.20"
oxigraph = "0.5.1"
tokio = { version = "1.47", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

[dev-dependencies]
insta = "1.43"
proptest = "1.8"
```

### 4. Create Domain RDF

```turtle
# ontology/domain.ttl
@prefix ex: <http://ggen.example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Your domain model goes here
ex:MyModel a ex:Model ;
    rdfs:label "My Model" ;
    ex:hasField ex:MyField .

ex:MyField a ex:Field ;
    ex:fieldName "my_field" ;
    ex:fieldType "string" ;
    ex:fieldRequired true .
```

### 5. Create Generation Template

```tera
# templates/my-output.tmpl
# Your template code here
```

### 6. Run Validation & Generation

```bash
cargo make validate-spec     # Check SHACL closure
cargo make generate          # Generate code
cargo make test              # Verify output
```

---

## Shared Infrastructure

The following shared files are available in `_shared_templates/`:

### RDF Ontology

**File**: `../.specify/example-ontologies.ttl`

Provides base classes and properties:

- **Classes**: `Entity`, `Service`, `Model`, `Field`, `Endpoint`, `Job`, `Worker`
- **Properties**: `hasField`, `fieldName`, `fieldType`, `fieldRequired`, `hasEndpoint`, etc.
- **Example Definitions**: Complete User + API endpoint example

**Usage**: Import and extend:

```turtle
@prefix ex: <http://ggen.example.org/> .
@prefix exo: <http://ggen.example.org/ontology/> .

# Extend shared ontology
exo:MyModel a ex:Model ;
    rdfs:label "My Model" ;
    ex:hasField exo:MyField .
```

### Base Templates

1. **`model_base.tmpl`** - Generate data models (Rust/Python/TypeScript)
2. **`api_endpoint.tmpl`** - Generate REST API endpoints (Axum/FastAPI/Express)
3. **`cli_command.tmpl`** - Generate CLI commands (clap-noun-verb/Click/Yargs)
4. **`make.toml.template`** - Lifecycle configuration template

### Validation Rules

**File**: `_validation_rules.ttl`

SHACL shapes ensuring:

- Models have labels and fields
- Fields have names and types
- Endpoints have paths and methods
- Services have names, languages, and frameworks
- All required properties are present

---

## RDF-Driven Examples

For examples that generate code from RDF, follow this pattern:

### Step 1: Define Domain in RDF

```turtle
# ontology/api-spec.ttl
@prefix ex: <http://ggen.example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Models
ex:UserModel a ex:Model ;
    rdfs:label "User" ;
    ex:hasField ex:user_id, ex:user_email, ex:user_name .

ex:user_id a ex:Field ;
    ex:fieldName "id" ;
    ex:fieldType "uuid" ;
    ex:fieldRequired true .

ex:user_email a ex:Field ;
    ex:fieldName "email" ;
    ex:fieldType "string" ;
    ex:fieldRequired true ;
    ex:fieldValidation "email" .

ex:user_name a ex:Field ;
    ex:fieldName "name" ;
    ex:fieldType "string" ;
    ex:fieldRequired true ;
    ex:fieldValidation "max_length:255" .

# Endpoints
ex:ListUsersEndpoint a ex:Endpoint ;
    rdfs:label "List Users" ;
    ex:endpointPath "/users" ;
    ex:endpointMethod "GET" ;
    ex:endpointDescription "Retrieve list of all users" ;
    ex:endpointResponseModel ex:UserModel .

ex:GetUserEndpoint a ex:Endpoint ;
    rdfs:label "Get User" ;
    ex:endpointPath "/users/{id}" ;
    ex:endpointMethod "GET" ;
    ex:endpointDescription "Retrieve a specific user" ;
    ex:endpointResponseModel ex:UserModel .

# Services
ex:UserService a ex:Service ;
    rdfs:label "User Service" ;
    ex:serviceName "user-service" ;
    ex:serviceLanguage "rust" ;
    ex:serviceFramework "axum" ;
    ex:servicePort 3000 ;
    ex:hasEndpoint ex:ListUsersEndpoint, ex:GetUserEndpoint .
```

### Step 2: Create SPARQL Queries in Templates

```tera
# templates/api-models.tmpl
{% set models = sparql_query("""
SELECT ?modelName ?modelLabel
WHERE {
  ?model a ex:Model ;
    rdfs:label ?modelLabel .
  BIND(STRAFTER(STR(?model), "#") AS ?modelName)
}
""") %}

{% for model in models %}
// Model: {{ model.modelLabel }}
pub struct {{ model.modelName }} {
    {% set fields = sparql_query("""
    SELECT ?fieldName ?fieldType ?isRequired
    WHERE {
      <""" ~ model.model ~ """> ex:hasField ?field .
      ?field ex:fieldName ?fieldName ;
        ex:fieldType ?fieldType ;
        ex:fieldRequired ?isRequired .
    }
    """) %}
    {% for field in fields %}
    pub {{ field.fieldName }}: {{ field.fieldType }}, // required: {{ field.isRequired }}
    {% endfor %}
}
{% endfor %}
```

### Step 3: Validate Specification Closure

```bash
# Check RDF passes SHACL validation
cargo make validate-spec

# Output should be:
# ✓ Validation report conforms
# ✓ All models have fields
# ✓ All endpoints have methods and paths
```

### Step 4: Generate Code

```bash
cargo make generate

# Output: ./generated/
#   ├── models.rs
#   ├── api.rs
#   ├── handlers.rs
#   └── ...
```

### Step 5: Test Generated Code

```bash
cargo make test

# Output:
# running 15 tests
# test api_endpoint_list ... ok
# test api_endpoint_get ... ok
# ...
# test result: ok. 15 passed
```

---

## Specification Closure

**Definition**: All values needed by templates come from RDF. No hardcoded values.

### ✓ Good: Specification Closure Achieved

```rust
// Template has SPARQL query for every value
{% set services = sparql_query(query) %}
{% for service in services %}
pub const SERVICE_NAME: &str = "{{ service.name }}";
pub const SERVICE_PORT: u16 = {{ service.port }};
{% endfor %}
```

```turtle
# RDF defines all values
ex:UserService ex:serviceName "user-service" ;
    ex:servicePort 3000 .
```

**Result**: `cargo make generate` always produces identical output (deterministic)

### ❌ Bad: Specification Closure Violated

```rust
// Hardcoded value in template
pub const SERVICE_NAME: &str = "user-service";  // ← NOT from RDF
pub const SERVICE_PORT: u16 = 3000;             // ← NOT from RDF
```

**Problem**: If RDF changes, template doesn't. Outputs diverge (non-deterministic)

### Verification Checklist

- [ ] Every string in generated output comes from RDF
- [ ] Every number in generated output comes from RDF
- [ ] Template contains SPARQL queries for all dynamic values
- [ ] No `let x = "hardcoded"` in templates
- [ ] RDF file passes SHACL validation (`cargo make validate-spec`)

---

## Template Development

### 1. Template Syntax (Tera)

```tera
{% comment %} This is a comment {% endcomment %}

{% for item in items %}
  Item: {{ item.name }}
{% endfor %}

{% if condition %}
  This shows conditionally
{% endif %}

{% set variable = "value" %}
```

See [Tera documentation](https://tera.netlify.app) for full syntax.

### 2. SPARQL in Templates

**Use SPARQL to extract data from RDF:**

```tera
{% set models = sparql_query("""
SELECT ?modelName ?fieldCount
WHERE {
  ?model a ex:Model ;
    rdfs:label ?modelName .
  {
    SELECT ?model (COUNT(?field) AS ?fieldCount)
    WHERE {
      ?model ex:hasField ?field .
    }
    GROUP BY ?model
  }
}
ORDER BY DESC(?fieldCount)
""") %}

{% for model in models %}
- {{ model.modelName }}: {{ model.fieldCount }} fields
{% endfor %}
```

### 3. Multi-File Output

Template can generate multiple files:

```tera
{# models.rs #}
pub struct User { ... }

{# ===== FILE: handlers.rs ===== #}
pub async fn list_users() { ... }

{# ===== FILE: main.rs ===== #}
fn main() { ... }
```

Each `FILE:` comment creates a new output file.

### 4. Testing Templates

```rust
#[test]
fn test_user_model_generation() {
    let rdf = r#"
        @prefix ex: <http://ggen.example.org/> .
        ex:UserModel a ex:Model ;
            ex:hasField ex:user_id .
        ex:user_id a ex:Field ;
            ex:fieldName "id" ;
            ex:fieldType "uuid" .
    "#;

    let output = generate(rdf, "templates/models.tmpl");

    assert!(output.contains("pub struct UserModel {"));
    assert!(output.contains("pub id: uuid,"));
}
```

---

## Quality Gates & Andon Signals

### 🔴 RED STOP

**Blocks release. Must fix immediately.**

- `cargo check` fails (compilation error)
- `cargo test` fails (test failure)
- SHACL validation fails (specification closure broken)
- Generated code doesn't compile
- Clippy warnings present

**Action**: Stop work, investigate root cause, fix.

### 🟡 YELLOW CAUTION

**Investigate. May need fix.**

- Documentation incomplete
- Test coverage < 60%
- Performance regression (slower than baseline)
- Warnings in generated code

**Action**: Review, decide if fix needed.

### 🟢 GREEN GO

**Ready to ship.**

- ✓ `cargo make check` < 5s
- ✓ `cargo make test` < 30s
- ✓ SHACL validation passes
- ✓ Generated code compiles
- ✓ 0 clippy warnings
- ✓ Documentation complete

---

## Acceptance Criteria

Every example must satisfy:

### Code Quality

- [ ] `cargo make check` succeeds in < 5 seconds
- [ ] `cargo make fmt` leaves code unchanged (properly formatted)
- [ ] `cargo make lint` produces 0 clippy warnings
- [ ] `cargo make test` passes all tests in < 30 seconds
- [ ] Generated code compiles without warnings

### Documentation

- [ ] README.md explains example purpose
- [ ] README.md has step-by-step instructions
- [ ] README.md documents key patterns demonstrated
- [ ] Code has meaningful comments (not over-commented)
- [ ] Example runs start-to-finish without errors

### RDF Examples

- [ ] `domain.ttl` loads into Oxigraph
- [ ] SHACL validation passes (`cargo make validate-spec`)
- [ ] SPARQL queries return expected results
- [ ] Generated code matches domain model exactly
- [ ] Specification closure is 100% (no hardcoded values)

### Determinism & Reproducibility

- [ ] Same RDF + Templates = Always same output
- [ ] Output is bit-perfect reproducible
- [ ] No random/non-deterministic values in output
- [ ] Build is hermetic (no external dependencies)

### Test Coverage

- [ ] Unit tests: > 60% code coverage
- [ ] Integration tests: End-to-end workflow runs
- [ ] Snapshot tests: Golden outputs validated with `insta`

---

## Common Patterns

### Pattern 1: RDF-to-Rust-Struct

**What**: Generate Rust structs from RDF models

**RDF** (ontology/domain.ttl):

```turtle
ex:User a ex:Model ;
    rdfs:label "User" ;
    ex:hasField ex:id, ex:email, ex:name .

ex:id a ex:Field ;
    ex:fieldName "id" ;
    ex:fieldType "uuid" ;
    ex:fieldRequired true .

ex:email a ex:Field ;
    ex:fieldName "email" ;
    ex:fieldType "string" ;
    ex:fieldRequired true .

ex:name a ex:Field ;
    ex:fieldName "name" ;
    ex:fieldType "string" ;
    ex:fieldRequired false .
```

**Template** (templates/models.rs.tera):

```tera
{% set models = sparql_query("
SELECT ?modelName ?fieldName ?fieldType ?isRequired
WHERE {
  ?model a ex:Model ;
    rdfs:label ?modelName ;
    ex:hasField ?field .
  ?field ex:fieldName ?fieldName ;
    ex:fieldType ?fieldType ;
    ex:fieldRequired ?isRequired .
}
ORDER BY ?modelName ?fieldName
") %}

{% for model in models %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ model.modelName }} {
    {% for field in model.fields %}
    pub {{ field.fieldName }}: {{ field.fieldType }},
    {% endfor %}
}
{% endfor %}
```

**Output** (generated/models.rs):

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: uuid,
    pub email: string,
    pub name: string,
}
```

### Pattern 2: Lifecycle Multi-Phase

**What**: Execute project lifecycle phases (build → test → deploy)

**Configuration** (make.toml):

```toml
[tasks.init]
script = "mkdir -p src tests && touch src/main.rs"

[tasks.build]
command = "cargo"
args = ["build", "--release"]
dependencies = ["init"]

[tasks.test]
command = "cargo"
args = ["test", "--all"]
dependencies = ["build"]

[tasks.deploy]
script = "docker build -t myapp:latest . && docker push ..."
dependencies = ["test"]

[tasks.lifecycle]
dependencies = ["init", "build", "test", "deploy"]
```

**Execution**:

```bash
cargo make lifecycle
# Runs: init → build → test → deploy in order
```

### Pattern 3: AI-Assisted Template Generation

**What**: Use Ollama/LLM to generate templates from specifications

**RDF** (ontology/spec.ttl):

```turtle
ex:TemplateSpec a rdf:Class ;
    ex:templatePurpose "Generate Rust middleware" ;
    ex:templateLanguage "rust" ;
    ex:templateFramework "axum" .
```

**Prompt** (prompts/generate-template.txt):

```
Generate a Tera template for {{ purpose }}

Target language: {{ language }}
Framework: {{ framework }}

Requirements:
- Use Oxigraph SPARQL queries
- Support model generation
- Include error handling
- Use idiomatic {{ language }} patterns
```

**Usage**:

```bash
# 1. Read spec from RDF
SPEC=$(sparql-select spec.ttl "SELECT ?purpose ?language ?framework...")

# 2. Generate template with Ollama
ggen ai generate --spec "$SPEC" --template-prompt prompts/generate-template.txt

# 3. Output: templates/generated-middleware.tmpl
```

---

## Troubleshooting

### Issue: SHACL Validation Fails

**Error**: `Validation report does not conform`

**Solution**:

1. Run validation with details:
   ```bash
   cargo run --bin ggen-spec-validator -- validate --verbose domain.ttl
   ```

2. Check for missing required properties:
   - All Models must have `rdfs:label` and `ex:hasField`
   - All Fields must have `ex:fieldName` and `ex:fieldType`
   - All Endpoints must have `ex:endpointPath` and `ex:endpointMethod`

3. Validate RDF syntax:
   ```bash
   riot domain.ttl --check
   ```

### Issue: Template SPARQL Query Returns No Results

**Error**: `No data in generated output`

**Solution**:

1. Debug SPARQL query:
   ```bash
   sparql-query domain.ttl "
   SELECT ?model WHERE {
     ?model a ex:Model .
   }"
   ```

2. Check RDF prefix definitions match template
3. Verify property names match exactly (case-sensitive)
4. Use `?_describe` to inspect what exists:
   ```sparql
   SELECT ?property WHERE {
     <http://ggen.example.org/MyModel> ?property ?value .
   }
   ```

### Issue: Generated Code Doesn't Compile

**Error**: Rust compilation fails in generated code

**Solution**:

1. Check template output is valid code:
   ```bash
   cargo make generate
   cat generated/models.rs
   ```

2. Validate template syntax (Tera):
   - All `{% %}` blocks are closed
   - All `{{ }}` variables exist in context
   - No unclosed strings or comments

3. Test template in isolation:
   ```rust
   #[test]
   fn test_template() {
       let template = include_str!("../templates/models.rs.tera");
       let tera = Tera::one_off(template, &context, false);
       assert!(tera.is_ok());
   }
   ```

### Issue: Performance Degradation

**Error**: `cargo make test` takes > 30 seconds

**Solution**:

1. Profile with `cargo make bench`
2. Check for expensive SPARQL queries in templates
3. Cache SPARQL results instead of querying repeatedly
4. Run expensive tests only on CI (mark with `#[ignore]`)

---

## Examples Completion Checklist

Use this checklist for each example:

- [ ] Directory structure created
- [ ] `Cargo.toml` with dependencies
- [ ] `make.toml` with lifecycle tasks
- [ ] `README.md` with instructions
- [ ] `domain.ttl` (RDF) with all entities
- [ ] `validation_rules.ttl` (SHACL) passes
- [ ] `templates/*.tera` with SPARQL queries
- [ ] `tests/` with unit and integration tests
- [ ] `generated/` with sample outputs
- [ ] `cargo make check` passes (< 5s)
- [ ] `cargo make test` passes (< 30s)
- [ ] `cargo make lint` has 0 warnings
- [ ] Documentation complete
- [ ] Specification closure verified (100%)
- [ ] PR ready for review

---

## Next Steps

1. **Pick an example** from the Wave 1-4 list
2. **Copy this guide** to your example directory
3. **Follow Quick Start** section
4. **Build incrementally**: RDF → Templates → Tests → Docs
5. **Validate early**: Run `cargo make validate-spec` before generating
6. **Test often**: Run `cargo make test` after each change
7. **Check quality**: Ensure `cargo make pre-commit` passes

---

## Questions?

- See `/home/user/ggen/examples/_shared_templates/` for templates
- See `/home/user/ggen/.specify/example-ontologies.ttl` for RDF base
- See `/home/user/ggen/examples/_validation_rules.ttl` for constraints
- Check existing examples in `/home/user/ggen/examples/` for patterns

**Last Updated**: 2026-01-07
