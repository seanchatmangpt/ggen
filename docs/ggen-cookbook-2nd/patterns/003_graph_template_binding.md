<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 003: GRAPH-TEMPLATE BINDING ***](#pattern-003-graph-template-binding-)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Result](#result)
  - [Graph Example](#graph-example)
  - [Template Example](#template-example)
  - [CLI Invocation](#cli-invocation)
  - [Expected Output](#expected-output)
  - [Verification](#verification)
  - [Advanced Binding Patterns](#advanced-binding-patterns)
    - [**Conditional Queries**](#conditional-queries)
    - [**Composed Queries**](#composed-queries)
    - [**Parameterized Queries**](#parameterized-queries)
  - [Anti-Patterns](#anti-patterns)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 003: GRAPH-TEMPLATE BINDING ***

## Context

Building on **Pattern 001: KNOWLEDGE-FIRST PROJECTION**, we have semantic graphs as our source of truth. However, templates need a way to declare their data dependencies and specify exactly what information they require from the knowledge graph. Without explicit binding, templates might receive irrelevant data or miss critical information.

## Problem

**How do you ensure templates receive exactly the data they need from knowledge graphs, while maintaining clear traceability and enabling template reuse across different domains?**

Without explicit graph-template binding:
- Templates receive arbitrary or irrelevant data
- No way to validate that templates have the data they need
- Difficult to reuse templates across different domains
- No clear traceability from template requirements to graph content
- Template authors must manually specify data extraction logic

## Forces

- Templates need specific data structures to render correctly
- Knowledge graphs contain rich semantic information
- Templates should be reusable across different domains
- Data extraction should be explicit and traceable
- Template validation should catch missing dependencies early
- SPARQL queries can extract precisely what's needed
- Template authors shouldn't need to understand SPARQL syntax

## Solution

**Therefore, bind templates to knowledge graphs through explicit SPARQL queries in template frontmatter.**

Every template declares its data needs via a `query` field in the frontmatter:

```yaml
---
name: "User Model Generator"
description: "Generate Rust structs from user domain model"
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
  }
  ORDER BY ?className ?propName
---
```

The query results become available in the template as `results`:

```handlebars
{{#each results}}
pub struct {{className}} {
    pub {{propName}}: {{propType}},
}
{{/each}}
```

## Result

You achieve:

- **Explicit Dependencies**: Templates clearly declare what data they need
- **Precise Extraction**: SPARQL queries extract exactly the required information
- **Template Validation**: Missing data is caught before template execution
- **Domain Reusability**: Same template works across different knowledge graphs
- **Traceability**: Clear mapping from template requirements to graph content
- **Composability**: Queries can be composed and extended

This pattern enables → **Pattern 004: NOUN-VERB CLI** (semantic operations)
This pattern enables → **Pattern 005: MULTI-LANGUAGE PROJECTION** (same query → multiple languages)
This pattern enables → **Pattern 006: LOCKFILE VERSIONING** (dependency tracking)

## Graph Example

**File**: `docs/examples/user_model.ttl`

```turtle
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "A system user with authentication" .

:name a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

:email a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:label "email" .

:alice a :User ;
    :name "Alice Johnson" ;
    :email "alice@example.com" .

:bob a :User ;
    :name "Bob Smith" ;
    :email "bob@example.com" .
```

## Template Example

**File**: `templates/rust_struct.tmpl`

```handlebars
---
name: "Rust Struct Generator"
description: "Generate Rust structs from semantic model"
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
  }
  ORDER BY ?className ?propName
---
// Generated from semantic model
{{#each results}}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{className}} {
    pub {{propName}}: {{propType}},
}

impl {{className}} {
    pub fn new({{propName}}: {{propType}}) -> Self {
        Self { {{propName}} }
    }
}
{{/each}}
```

## CLI Invocation

```bash
# Step 1: Load the knowledge graph
ggen graph load docs/examples/user_model.ttl

# Step 2: Verify graph contents
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Step 3: Generate from template using graph data
ggen template apply templates/rust_struct.tmpl > src/models/user.rs
```

## Expected Output

**File**: `src/models/user.rs`

```rust
// Generated from semantic model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
}

impl User {
    pub fn new(name: String, email: String) -> Self {
        Self { name, email }
    }
}
```

## Verification

```bash
# Verify the graph is loaded and queryable
ggen graph query "PREFIX : <http://example.org/> SELECT (COUNT(?user) as ?count) WHERE { ?user a :User }"

# Expected output:
# count: 2

# Verify template received projected data
ggen template apply templates/rust_struct.tmpl --dry-run --show-data

# Expected JSON projection:
# {
#   "results": [
#     {"className": "User", "propName": "name", "propType": "string"},
#     {"className": "User", "propName": "email", "propType": "string"}
#   ]
# }

# Verify generated code compiles
rustc --crate-type lib src/models/user.rs
```

## Advanced Binding Patterns

### **Conditional Queries**
Templates can include conditional logic in queries:

```yaml
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType ?isRequired
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
    OPTIONAL {
      ?prop :required true .
    }
    BIND(COALESCE(?isRequired, false) AS ?isRequired)
  }
  ORDER BY ?className ?propName
```

### **Composed Queries**
Queries can be composed from multiple sources:

```yaml
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType ?validationRule
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
    OPTIONAL {
      ?prop :validation ?validationRule .
    }
  }
  ORDER BY ?className ?propName
```

### **Parameterized Queries**
Queries can accept parameters from template variables:

```yaml
variables:
  - name: domain_prefix
    description: "Domain prefix for RDF URIs"
    type: string
    default: "http://example.org/"

query: |
  PREFIX : <{{domain_prefix}}>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
  }
  ORDER BY ?className ?propName
```

## Anti-Patterns

❌ **Implicit Data Access**: Templates accessing graph data without explicit queries
❌ **Hardcoded Queries**: Embedding SPARQL directly in template logic
❌ **No Validation**: Not validating that queries return expected data
❌ **Tight Coupling**: Templates tightly coupled to specific graph structures
❌ **No Reusability**: Templates that only work with specific domains

## Related Patterns

- **Pattern 001: KNOWLEDGE-FIRST PROJECTION** - Provides the semantic foundation
- **Pattern 002: DETERMINISTIC ENGINE** - Ensures reproducible query results
- **Pattern 004: NOUN-VERB CLI** - Semantic operations on graph entities
- **Pattern 005: MULTI-LANGUAGE PROJECTION** - Same query → multiple languages
- **Pattern 006: LOCKFILE VERSIONING** - Dependency tracking for queries
