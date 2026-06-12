<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Troubleshooting Guide](#ggen-troubleshooting-guide)
  - [Quick Start](#quick-start)
  - [Table of Contents](#table-of-contents)
  - [Compilation Issues](#compilation-issues)
    - [Error: `error[E0433]: cannot find macro 'unwrap'`](#error-errore0433-cannot-find-macro-unwrap)
    - [Error: `error[E0599]: no method named 'query' found`](#error-errore0599-no-method-named-query-found)
    - [Error: `error: linking with 'cc' failed`](#error-error-linking-with-cc-failed)
    - [Error: `warning: unused import`](#error-warning-unused-import)
  - [SPARQL Query Issues](#sparql-query-issues)
    - [Error: `SPARQL parse error: Unexpected token`](#error-sparql-parse-error-unexpected-token)
    - [Error: `Query returned 0 results when 5 were expected`](#error-query-returned-0-results-when-5-were-expected)
    - [Error: `Cartesian product explosion` (query times out)](#error-cartesian-product-explosion-query-times-out)
  - [Template Issues](#template-issues)
    - [Error: `tera error: variable not found: 'foo'`](#error-tera-error-variable-not-found-foo)
    - [Error: `tera error: unexpected token`](#error-tera-error-unexpected-token)
    - [Error: `Error: variable not found in SPARQL row`](#error-error-variable-not-found-in-sparql-row)
  - [RDF Data Issues](#rdf-data-issues)
    - [Error: `Invalid RDF/Turtle syntax`](#error-invalid-rdfturtle-syntax)
    - [Error: `Blank node not unified`](#error-blank-node-not-unified)
    - [Error: `Namespace collision`](#error-namespace-collision)
  - [Performance Issues](#performance-issues)
    - [Symptom: Queries take >10 seconds](#symptom-queries-take-10-seconds)
    - [Symptom: Memory usage grows unbounded](#symptom-memory-usage-grows-unbounded)
    - [Symptom: Generation takes much longer than expected](#symptom-generation-takes-much-longer-than-expected)
  - [Configuration Issues](#configuration-issues)
    - [Error: `config file not found: ggen.toml`](#error-config-file-not-found-ggentoml)
    - [Error: `Invalid TOML syntax`](#error-invalid-toml-syntax)
    - [Error: `ontology not found` when running generation](#error-ontology-not-found-when-running-generation)
  - [Best Practices to Prevent Errors](#best-practices-to-prevent-errors)
    - [1. Use `cargo make` Commands](#1-use-cargo-make-commands)
    - [2. Validate RDF Data Before Use](#2-validate-rdf-data-before-use)
    - [3. Test SPARQL Queries in Isolation](#3-test-sparql-queries-in-isolation)
    - [4. Document Assumptions](#4-document-assumptions)
    - [5. Use Result<T,E> Everywhere](#5-use-resultte-everywhere)
  - [Getting Help](#getting-help)
    - [Check Documentation](#check-documentation)
    - [Debug Techniques](#debug-techniques)
    - [Report Issues](#report-issues)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Troubleshooting Guide

> Error recovery strategies, common mistakes, and poka-yoke defenses

## Quick Start

- **Compilation errors?** → [Compilation Issues](#compilation-issues)
- **SPARQL query failing?** → [SPARQL Query Issues](#sparql-query-issues)
- **Templates not rendering?** → [Template Issues](#template-issues)
- **RDF data problems?** → [RDF Data Issues](#rdf-data-issues)
- **Performance too slow?** → [Performance Issues](#performance-issues)

---

## Table of Contents

1. [Compilation Issues](#compilation-issues)
2. [SPARQL Query Issues](#sparql-query-issues)
3. [Template Issues](#template-issues)
4. [RDF Data Issues](#rdf-data-issues)
5. [Performance Issues](#performance-issues)
6. [Configuration Issues](#configuration-issues)
7. [Best Practices to Prevent Errors](#best-practices-to-prevent-errors)
8. [Getting Help](#getting-help)

---

## Compilation Issues

### Error: `error[E0433]: cannot find macro 'unwrap'`

**Cause**: Code uses `.unwrap()` in production (violates Constitution Principle VII)

**Fix**:
```rust
// ❌ WRONG
let value = some_result.unwrap();

// ✅ CORRECT
let value = some_result.map_err(|e| Error::new(&format!("Deserialization failed: {}", e)))?;
```

**Prevention**: Use clippy lint `#[deny(unwrap_in_code)]` in production modules

---

### Error: `error[E0599]: no method named 'query' found`

**Cause**: Using wrong Graph API method or incorrect import

**Fix**:
```rust
// ✅ Use Graph::query() for SELECT
let results = graph.query("SELECT ?var WHERE { ... }")?;

// ✅ Use Graph::construct() for CONSTRUCT
let triples = graph.construct("CONSTRUCT { ... } WHERE { ... }")?;

// ✅ Use Graph::query_cached() for cached results
let cached = graph.query_cached("SELECT ...")?;
```

**Related**: See [Graph Querying API](../GRAPH_QUERYING_API.md)

---

### Error: `error: linking with 'cc' failed`

**Cause**: Missing system libraries (usually graphdb or native dependencies)

**Fix**:
```bash
# macOS
brew install openssl pkg-config

# Ubuntu/Debian
sudo apt-get install libssl-dev pkg-config

# Fedora
sudo dnf install openssl-devel pkg-config
```

**Prevention**: Install build dependencies before `cargo build`

---

### Error: `warning: unused import`

**Cause**: Unused import in code (violates Principle IV)

**Fix**:
```rust
// ❌ WRONG
use crate::validation::SparqlValidator;
// ... SparqlValidator never used

// ✅ CORRECT
use crate::validation::SparqlValidator;
let validator = SparqlValidator::new();
```

**Prevention**: Use `cargo make lint` before committing

---

## SPARQL Query Issues

### Error: `SPARQL parse error: Unexpected token`

**Cause**: Invalid SPARQL syntax in query

**Common Issues**:
1. Missing `?` prefix on variables
2. Missing `{ }` braces on WHERE clause
3. Invalid URIs (not wrapped in `< >` or with prefix)
4. Missing FILTER after UNION

**Example Fix**:
```sparql
# ❌ WRONG
SELECT name WHERE {
  ?node dc:title name .
}

# ✅ CORRECT
PREFIX dc: <http://purl.org/dc/terms/>
SELECT ?name WHERE {
  ?node dc:title ?name .
}
```

---

### Error: `Query returned 0 results when 5 were expected`

**Cause**: RDF data doesn't match query assumptions

**Debug Strategy**:
1. Verify data exists: `SELECT ?s ?p ?o LIMIT 10`
2. Check namespace prefixes are correct
3. Use OPTIONAL to find missing data
4. Print actual triple store: `graph.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")`

**Example**:
```rust
// Step 1: Verify data exists
let check = graph.query("SELECT COUNT(?s) AS ?count WHERE { ?s ?p ?o }")?;
println!("Total triples: {}", /* count */);

// Step 2: Verify specific predicates
let pred_check = graph.query("SELECT DISTINCT ?p WHERE { ?s ?p ?o } LIMIT 100")?;
```

---

### Error: `Cartesian product explosion` (query times out)

**Cause**: Multiple independent OPTIONAL clauses creating exponential combinations

**Fix**: Use UNION instead of OPTIONAL
```sparql
# ❌ SLOW (Cartesian product)
SELECT ?x ?a ?b ?c WHERE {
  ?x a :Entity .
  OPTIONAL { ?x :propA ?a }
  OPTIONAL { ?x :propB ?b }
  OPTIONAL { ?x :propC ?c }
}

# ✅ FAST (UNION pattern)
SELECT ?x ?a ?b ?c WHERE {
  ?x a :Entity .
  { SELECT ?x ?a ?b ?c WHERE {
      ?x a :Entity .
      ?x :propA ?a .
      OPTIONAL { ?x :propB ?b . OPTIONAL { ?x :propC ?c } }
    }
    UNION
    { SELECT ?x ?a ?b ?c WHERE {
        ?x a :Entity .
        FILTER NOT EXISTS { ?x :propA ?a }
        OPTIONAL { ?x :propB ?b . OPTIONAL { ?x :propC ?c } }
      }
    }
  }
}
```

---

## Template Issues

### Error: `tera error: variable not found: 'foo'`

**Cause**: Template references variable that doesn't exist in context

**Fix**:
```rust
// ❌ WRONG: template uses `{{ bar }}` but we provide `foo`
let mut ctx = tera::Context::new();
ctx.insert("foo", &value);

// ✅ CORRECT: match template variable names
ctx.insert("bar", &value);
```

**Prevention**: Use `| default()` filter for optional variables:
```tera
{# Safe even if 'optional_var' missing #}
{% if optional_var %}{{ optional_var }}{% else %}fallback{% endif %}
```

---

### Error: `tera error: unexpected token`

**Cause**: Tera syntax error in template

**Common Issues**:
1. Missing `endif` for `if`
2. Missing `endfor` for `for`
3. Wrong filter syntax: `{{ var | filter }}` not `{{ var|filter }}`
4. Literal `{` or `}` needs escaping

**Fix**:
```tera
{# ❌ WRONG: missing endif #}
{% if condition %}
  content here

{# ✅ CORRECT: matched tags #}
{% if condition %}
  content here
{% endif %}

{# ❌ WRONG: unescaped braces in output #}
JSON: { "key": "{{ value }}" }

{# ✅ CORRECT: escaped braces #}
JSON: {% raw %}{ "key": "{{ value }}" }{% endraw %}
```

---

### Error: `Error: variable not found in SPARQL row`

**Cause**: SQL/SPARQL variable name mismatch between query and template

**Fix**:
```rust
// Query: SELECT ?nodeName ?nodeID WHERE { ... }
let results = graph.query("SELECT ?nodeName ?nodeID WHERE { ... }")?;

// Template must match: {{ nodeName }} and {{ nodeID }}
// ❌ WRONG: {{ node_name }} (snake_case)
// ✅ CORRECT: {{ nodeName }} (camelCase as declared in SELECT)
```

---

## RDF Data Issues

### Error: `Invalid RDF/Turtle syntax`

**Cause**: Malformed Turtle file

**Common Issues**:
1. Missing semicolon after property
2. Missing period at end of statement
3. Invalid URI syntax
4. Missing quotes for string literals

**Example Fix**:
```turtle
# ❌ WRONG
@prefix ex: <http://example.org/>
ex:alice a ex:Person
  ex:name "Alice"
  ex:age 30

# ✅ CORRECT
@prefix ex: <http://example.org/>
ex:alice a ex:Person ;
  ex:name "Alice" ;
  ex:age 30 .
```

---

### Error: `Blank node not unified`

**Cause**: Blank nodes `[ ... ]` create duplicate instances instead of shared references

**Fix**: Use named nodes instead:
```turtle
# ❌ CREATES DUPLICATES
ex:alice ex:knows [ ex:name "Bob" ] .
ex:alice ex:knows [ ex:name "Bob" ] .
# This creates TWO different "Bob" nodes!

# ✅ USE NAMED NODES
ex:bob ex:name "Bob" .
ex:alice ex:knows ex:bob .
ex:charlie ex:knows ex:bob .
# Now both Alice and Charlie know the SAME Bob
```

---

### Error: `Namespace collision`

**Cause**: Same prefix used for multiple namespaces

**Fix**:
```turtle
# ❌ WRONG: 'ex' used for multiple namespaces
@prefix ex: <http://example.org/>
@prefix ex: <http://example.com/>  # Oops!

# ✅ CORRECT: different prefixes
@prefix exorg: <http://example.org/>
@prefix excom: <http://example.com/>
```

---

## Performance Issues

### Symptom: Queries take >10 seconds

**Cause #1**: Cartesian product in OPTIONAL clauses
- **Fix**: See [Cartesian product explosion](#error-cartesian-product-explosion-query-times-out)

**Cause #2**: Missing index on frequently queried properties
- **Fix**: None in Oxigraph (in-memory), consider data organization instead

**Cause #3**: Loading too much data into graph
- **Fix**: Use SPARQL FILTER to reduce early, implement pagination

**Debug**:
```rust
use std::time::Instant;

let start = Instant::now();
let results = graph.query("SELECT ...")?;
let elapsed = start.elapsed();
eprintln!("Query took: {:?}", elapsed);
```

---

### Symptom: Memory usage grows unbounded

**Cause #1**: Caching too many query results
- **Fix**: Use `graph.query()` instead of `graph.query_cached()` for one-time queries

**Cause #2**: Loading entire dataset into memory
- **Fix**: Consider streaming or file-based processing

**Debug**:
```bash
# Monitor memory
/usr/bin/time -v cargo run --release -- your-command

# Profile with perf
cargo build --release
perf record ./target/release/ggen-cli ...
perf report
```

---

### Symptom: Generation takes much longer than expected

**Cause**: Inefficient CONSTRUCT queries or excessive template rendering

**Fix**:
```rust
// ✅ Use GROUP_CONCAT for multi-value properties
SELECT ?entity (GROUP_CONCAT(?val) AS ?values) WHERE {
  ?entity :property ?val .
}
GROUP BY ?entity

// ✅ Pre-filter with MINUS instead of OPTIONAL
SELECT ?node WHERE {
  ?node :classA .
  MINUS { ?node :skipMe }
}

// ✅ Limit scope with VALUES
SELECT ?x WHERE {
  VALUES ?class { :Type1 :Type2 :Type3 }
  ?x a ?class .
}
```

---

## Configuration Issues

### Error: `config file not found: ggen.toml`

**Cause**: Wrong working directory or missing file

**Fix**:
```bash
# Check current directory
ls ggen.toml  # Should exist

# Or specify full path
ggen-cli --config /path/to/ggen.toml render
```

---

### Error: `Invalid TOML syntax`

**Cause**: Malformed TOML configuration

**Common Issues**:
1. Missing `[[generation.rules]]` array syntax
2. Unquoted strings as values
3. Invalid TOML types

**Fix**:
```toml
# ❌ WRONG
generation {
  rules = {
    name = select_rule,  # Missing quotes
    output_file = "file.txt"
  }
}

# ✅ CORRECT
[[generation.rules]]
name = "select_rule"
output_file = "file.txt"

[[generation.rules]]
name = "construct_rule"
output_file = "graph.ttl"
```

---

### Error: `ontology not found` when running generation

**Cause**: Path in ggen.toml is incorrect

**Fix**:
```toml
[ontology]
# ❌ WRONG: relative to cwd, not config file
source = "ontology.ttl"

# ✅ CORRECT: relative to config file location
source = "ontology/my-ontology.ttl"

# ✅ CORRECT: absolute path
source = "/home/user/ggen/ontology.ttl"
```

---

## Best Practices to Prevent Errors

### 1. Use `cargo make` Commands

```bash
# Check compilation
cargo make check

# Run tests
cargo make test

# Lint code (catches many errors early)
cargo make lint

# Full validation
cargo make pre-commit
```

**Why**: Catches errors before they become bugs

---

### 2. Validate RDF Data Before Use

```rust
// Add validation at pipeline entry point
let graph = Graph::load_ttl(&data)?;

// Validate against SHACL shapes
let validator = SparqlValidator::new();
let result = validator.validate(&graph, &shapes)?;

if !result.passed {
    eprintln!("Validation errors:");
    for violation in &result.violations {
        eprintln!("  {}: {}", violation.focus_node, violation.message);
    }
    return Err(Error::new("RDF validation failed"));
}
```

**Why**: Detects bad data early (poka-yoke principle)

---

### 3. Test SPARQL Queries in Isolation

```rust
// Create minimal test data
let test_graph = Graph::new()?;
test_graph.insert_turtle(r#"
    @prefix ex: <http://example.org/>
    ex:test a ex:TestEntity ;
      ex:prop "value" .
"#)?;

// Test query
let results = test_graph.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")?;
assert!(!results.is_empty(), "Expected results, got none");
```

**Why**: Validates queries with known data first

---

### 4. Document Assumptions

```rust
/// Load ontology with required SHACL validation
///
/// # Assumptions
/// - `path` file contains valid Turtle RDF
/// - All required properties from SHACL schema are present
/// - No circular references in inverse properties
///
/// # Errors
/// - Returns Error if RDF syntax is invalid
/// - Returns Error if SHACL validation fails
pub fn load_with_validation(path: &str) -> Result<Graph> {
    // ...
}
```

**Why**: Prevents misuse and clarifies error handling

---

### 5. Use Result<T,E> Everywhere

```rust
// ❌ WRONG: Can panic
pub fn process_data(graph: &Graph) -> Vec<String> {
    graph.query("SELECT ...").unwrap()  // PANIC!
}

// ✅ CORRECT: Propagates error
pub fn process_data(graph: &Graph) -> Result<Vec<String>> {
    let results = graph.query("SELECT ...")?;
    Ok(results)
}
```

**Why**: Makes errors recoverable, not fatal

---

## Getting Help

### Check Documentation

1. **[SPARQL Reference Card](../SPARQL_REFERENCE_CARD.md)** - Common SPARQL patterns
2. **[Graph Querying API](../GRAPH_QUERYING_API.md)** - Graph methods
3. **[ggen.toml Reference](../GGEN_TOML_REFERENCE.md)** - Configuration options
4. **[Best Practices](../BEST_PRACTICES.md)** - Design guidance

### Debug Techniques

```bash
# 1. Enable debug logging
RUST_LOG=debug cargo make run -- your-command

# 2. Print RDF data
graph.print_triples()

# 3. Execute SPARQL directly
graph.query("SELECT * WHERE { ?s ?p ?o LIMIT 100")?

# 4. Check compilation errors carefully
cargo make check 2>&1 | head -50

# 5. Validate with cargo-edit
cargo add --dry-run your-dependency
```

### Report Issues

Include:
1. **Error message**: Full text from compiler or runtime
2. **Minimal reproduction**: Smallest code that causes error
3. **SPARQL query** (if applicable): Exact query that fails
4. **ggen version**: `ggen-cli --version`
5. **Environment**: OS, Rust version, architecture

---

## See Also

- [Build Corruption Recovery](./build-corruption-recovery.md)
- [Poka-Yoke Quick Reference](../poka-yoke-quick-reference.md)
- [CLAUDE.md - Constitution Principles](../CLAUDE.md)
- [Error Catalog](../ERROR_CATALOG_COMPRESSED.md)
