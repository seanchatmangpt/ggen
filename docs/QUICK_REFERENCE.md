# ggen v6.0.1 - Quick Reference

**Last Updated:** 2026-03-31

> Specification-driven code generation from RDF ontologies | A = μ(O)

---

## 🚀 Common CLI Commands

### Core Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen sync` | Full μ₁-μ₅ pipeline (THE primary command) | `ggen sync --audit` |
| `ggen wizard` | Interactive project setup | `ggen wizard --profile c4-diagrams` |
| `ggen init` | Initialize new project scaffold | `ggen init --path my-project` |
| `ggen validate` | Validate ontology (SHACL/SPARQL) | `ggen validate schema.ttl` |

### Marketplace Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen marketplace search` | Search packages | `ggen marketplace search "workflow"` |
| `ggen marketplace install` | Install package | `ggen marketplace install io.ggen.yaml` |
| `ggen marketplace list` | List installed | `ggen marketplace list` |
| `ggen marketplace publish` | Publish package | `ggen marketplace publish --manifest` |

### MCP Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen mcp start-server` | Start MCP server | `ggen mcp start-server --transport stdio` |
| `ggen mcp list` | List tools | `ggen mcp list --verbose` |
| `ggen mcp test-tool` | Test MCP tool | `ggen mcp test-tool validate_pipeline` |

### Other Common Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen graph query` | SPARQL query | `ggen graph query "SELECT ?s WHERE { ?s a ?o }"` |
| `ggen template generate-rdf` | Generate from template | `ggen template generate-rdf --output .` |
| `ggen utils doctor` | System diagnostics | `ggen utils doctor` |

---

## 📝 ggen.toml Reference

### Minimal Configuration

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "Project description"

[ontology]
source = "schema/domain.ttl"
base_iri = "https://example.com/ontology#"

[generation]
output_dir = "./generated"
```

### Complete Configuration

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "Full-featured project"
authors = ["Your Name"]

[ontology]
source = "schema/domain.ttl"
imports = ["common.ttl"]
base_iri = "https://example.com/ontology#"

[ontology.prefixes]
ex = "https://example.com/ontology#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
xsd = "http://www.w3.org/2001/XMLSchema#"

# Phase 1: CONSTRUCT inference
[[inference.rules]]
name = "enrich-types"
description = "Add type metadata"
construct = """
PREFIX ex: <https://example.com/ontology#>
CONSTRUCT {
  ?s ex:inferredType ?type .
}
WHERE {
  ?s a ?type .
}
"""

# Phase 2: Generation rules
[[generation.rules]]
name = "generate-rust-struct"
description = "Generate Rust struct from ontology"
query = { inline = """
PREFIX ex: <https://example.com/ontology#>
SELECT ?struct_name ?field_name ?field_type
WHERE {
  ?s a ex:Struct ;
    ex:name ?struct_name ;
    ex:hasField ?field .
  ?field ex:name ?field_name ;
    ex:type ?field_type .
}
""" }
template = { file = "templates/struct.tera" }
output_file = "{{ struct_name }}.rs"
mode = "Overwrite"

[validation]
validate_syntax = true
no_unsafe = true
require_doc_comments = false
max_line_length = 100
```

### Configuration Sections

| Section | Purpose | Required |
|---------|---------|----------|
| `[project]` | Project metadata | Yes |
| `[ontology]` | RDF ontology source | Yes |
| `[ontology.prefixes]` | SPARQL prefix bindings | Optional |
| `[[inference.rules]]` | CONSTRUCT queries | Optional |
| `[[generation.rules]]` | SELECT + template mappings | Yes |
| `[validation]` | Code quality rules | Optional |

---

## 🎨 Template Syntax Reference (Tera)

### Basic Variables

```tera
{# Single variable #}
{{ variable_name }}

{# With default value #}
{{ variable_name | default(value="default") }}

{# Nested access #}
{{ object.field }}
{{ array[0] }}
```

### Conditionals

```tera
{# If/else #}
{% if has_feature %}
  Feature enabled
{% elif has_optional %}
  Optional feature
{% else %}
  No feature
{% endif %}

{# Check for presence #}
{% if variable_name is defined %}
  Variable exists
{% endif %}
```

### Loops

```tera
{# Iterate over array #}
{% for item in items %}
  - {{ item.name }}
{% endfor %}

{# Loop with index #}
{% for field in fields %}
  {{ loop.index }}: {{ field.name }}
{% endfor %}

{# Loop with special variables #}
{% for item in items %}
  {{ loop.index }}       {# 1-based index #}
  {{ loop.index0 }}      {# 0-based index #}
  {{ loop.first }}       {# true if first iteration #}
  {{ loop.last }}        {# true if last iteration #}
{% endfor %}
```

### Filters

```tera
{# Common filters #}
{{ name | upper }}              {# UPPERCASE #}
{{ name | lower }}              {# lowercase #}
{{ name | capitalize }}         {# Capitalize #}
{{ text | replace(from=" ", to="_") }}
{{ text | default(value="N/A") }}
{{ items | length }}            {# Array length #}
{{ items | first }}             {# First element #}
{{ items | last }}              {# Last element #}

{# Chaining filters #}
{{ name | trim | capitalize | replace(from=" ", to="-") }}
```

### Comments

```tera
{# Single-line comment #}

{#
  Multi-line comment
  Can span multiple lines
#}
```

### YAML Frontmatter

```tera
---
title: "{{ title }}"
description: "{{ description | default(value='No description') }}"
date: "{{ date | default(value='2026-03-31') }}
tags:
{% for tag in tags %}
  - {{ tag }}
{% endfor %}
---

{{ content }}
```

---

## 🔍 SPARQL Query Examples

### SELECT Queries

```sparql
# Basic SELECT
PREFIX ex: <https://example.com/ontology#>
SELECT ?subject ?predicate ?object
WHERE {
  ?subject ?predicate ?object .
}
LIMIT 10

# Filter by type
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <https://example.com/ontology#>
SELECT ?name ?description
WHERE {
  ?s a ex:Struct ;
    ex:name ?name ;
    rdfs:comment ?description .
}
ORDER BY ?name

# Optional fields
PREFIX ex: <https://example.com/ontology#>
SELECT ?struct_name ?has_builder
WHERE {
  ?s a ex:Struct ;
    ex:name ?struct_name .
  OPTIONAL { ?s ex:hasBuilder ?has_builder }
}
```

### CONSTRUCT Queries

```sparql
# Materialize inferred triples
PREFIX ex: <https://example.com/ontology#>
CONSTRUCT {
  ?s ex:inferredType ?type ;
    ex:hasFieldCount ?count .
}
WHERE {
  ?s a ?type ;
    ex:hasField ?field .
  ?s ex:hasField ?field2 .
  BIND(COUNT(?field) AS ?count)
}
GROUP BY ?s ?type
```

### ASK Queries

```sparql
# Boolean check
PREFIX ex: <https://example.com/ontology#>
ASK {
  ?s a ex:Struct ;
    ex:name "MyStruct" .
}
```

---

## 🔬 OpenTelemetry (OTEL) Verification

### Enable Tracing

```bash
# Set environment variables
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run tests with OTEL output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt
```

### Verify Spans

```bash
# Check for LLM completion spans
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt

# Check for model name
grep -E "llm\.model.*groq|llm\.model.*gpt" otel_output.txt

# Check for token counts
grep -E "llm\.(prompt_tokens|completion_tokens|total_tokens)" otel_output.txt

# Check for MCP tool spans
grep -E "mcp\.tool\.(call|response)" otel_output.txt

# Check for pipeline stages
grep -E "pipeline\.(load|extract|generate|validate|emit)" otel_output.txt
```

### Required Spans by Feature

| Feature | Required Spans | Required Attributes |
|---------|---------------|---------------------|
| **LLM Integration** | `llm.complete`, `llm.complete_stream` | `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens` |
| **MCP Tools** | `mcp.tool.call`, `mcp.tool.response` | `mcp.tool.name`, `mcp.tool.duration_ms` |
| **Pipeline Stages** | `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` | `pipeline.stage`, `pipeline.duration_ms` |
| **Quality Gates** | `quality_gate.validate`, `quality_gate.pass_fail` | `gate.name`, `gate.result` |

---

## 🛠️ Build & Test Commands

### Cargo Make (Preferred)

```bash
# Build commands
cargo make check          # Compilation check (<5s)
cargo make build          # Build debug binary
cargo make build-release  # Build release binary

# Test commands
cargo make test-unit      # Unit tests only (<16s)
cargo make test           # Full test suite (<30s)
cargo make test-mutation  # Mutation testing (≥60% score)

# Quality gates
cargo make lint           # Clippy + rustfmt (<60s)
cargo make pre-commit     # check → lint → test-unit (<2min)
cargo make slo-check      # Performance SLOs validation
cargo make audit          # Security vulnerabilities scan
```

### Direct Cargo (Only if cargo-make unavailable)

```bash
# Not recommended - use cargo make instead
cargo check
cargo test
cargo clippy
cargo fmt --all
```

---

## 🚨 Troubleshooting Checklist

### Build Errors

| Symptom | Cause | Solution |
|---------|-------|----------|
| `error[E0433]: failed to resolve` | Missing dependency or version mismatch | `cargo update` or check `Cargo.toml` |
| `error: could not compile` | Compiler error in code | Check error message, fix syntax/type errors |
| `linking with cc failed` | C compiler issues | Install Xcode Command Line Tools (macOS) or build-essential (Linux) |
| `timeout` error | Build taking too long | Increase timeout in `Makefile.toml` or optimize code |

### Test Failures

| Symptom | Cause | Solution |
|---------|-------|----------|
| `test ... FAILED` | Test assertion failed | Check test output, fix implementation |
| `panicked at 'assertion failed'` | Logic error in code | Debug with `RUST_BACKTRACE=1` |
| `No such file or directory` | Missing test fixture | Create fixture files or update test paths |
| Tests pass but OTEL spans missing | Mocks instead of real calls | Verify real API calls with `RUST_LOG=trace` |

### Runtime Errors

| Symptom | Cause | Solution |
|---------|-------|----------|
| `ERROR: Failed to load ontology` | Missing or invalid `.ttl` file | Check file path, validate RDF syntax |
| `Template error: Variable 'foo' not found` | Missing template variable | Add variable to ggen.toml or provide default |
| `SPARQL query error` | Invalid SPARQL syntax | Validate query with `ggen validate` |
| `Permission denied` | File system permissions | Check file ownership, use `chmod` if needed |

### MCP Server Issues

| Symptom | Cause | Solution |
|---------|-------|----------|
| `Failed to start MCP server` | Port in use or config error | Check `--transport` flag, verify config |
| `Tool 'xxx' not found` | Tool not registered | Run `ggen mcp list` to see available tools |
| `Invalid JSON arguments` | Malformed tool input | Validate JSON syntax |

---

## 📚 Common Workflows

### New Project Setup

```bash
# 1. Initialize project
ggen wizard --profile rust

# 2. Edit ontology
vim schema/domain.ttl

# 3. Create templates
mkdir -p templates
vim templates/struct.tera

# 4. Configure generation
vim ggen.toml

# 5. Generate code
ggen sync --audit

# 6. Validate output
cargo make check
cargo make test
```

### Feature Development (Chicago TDD)

```bash
# 1. Create RDF spec
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl

# 2. Write failing test (RED)
vim crates/ggen-core/tests/feature_test.rs
cargo make test-unit  # Verify fails

# 3. Implement feature (GREEN)
vim crates/ggen-core/src/feature.rs
cargo make test-unit  # Verify passes

# 4. Refactor (maintain GREEN)
cargo make pre-commit

# 5. OTEL validation (if LLM/external service)
RUST_LOG=trace cargo test -p ggen-cli-lib --test feature_e2e 2>&1 | grep llm
```

### Continuous Development

```bash
# Watch mode for live feedback
ggen sync --watch --verbose

# Validate without generating
ggen sync --validate-only

# Dry-run to preview changes
ggen sync --dry-run

# Force overwrite with audit
ggen sync --force --audit
```

---

## 🔑 Key Concepts

### μ Pipeline (μ₁-μ₅)

| Stage | Name | Purpose |
|-------|------|---------|
| μ₁ | CONSTRUCT | Normalize RDF ontology via SPARQL CONSTRUCT |
| μ₂ | SELECT | Extract bindings via SPARQL SELECT |
| μ₃ | Tera | Generate code from templates |
| μ₄ | Canonicalize | Format and organize (rustfmt, compilation) |
| μ₅ | Receipt | Generate cryptographic verification (Ed25519) |

### Chicago TDD

- ✅ Real collaborators (actual databases, filesystems, HTTP clients)
- ✅ State-based verification (assert on observable results)
- ✅ Empirical observation (tests verify actual system behavior)
- ✅ OTEL trace verification (prove real external calls)
- ❌ No mocks, no test doubles, no behavior verification

### Andon Signals

| Level | Pattern | Action |
|-------|---------|--------|
| 🔴 **CRITICAL** | `error[E...]` | HALT - Compiler errors |
| 🔴 **CRITICAL** | `test ... FAILED` | HALT - Test failures |
| 🟡 **HIGH** | `warning:` | STOP before release |
| 🟡 **HIGH** | Clippy errors | STOP before release |
| 🟢 **GREEN** | All checks pass | Proceed |

---

## 📖 Further Reading

- **Architecture:** `CLAUDE.md` - Full crate map and patterns
- **Rules:** `.claude/rules/` - Development workflow and conventions
- **Research:** `docs/research/` - LSP, OTEL, and best practices
- **Repository:** https://github.com/seanchatmangpt/ggen

---

**Version:** v6.0.1 | **Stack:** Rust 1.91.1 | Tokio | Oxigraph | Tera | Clap
