<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 001: KNOWLEDGE-FIRST PROJECTION ***](#pattern-001-knowledge-first-projection-)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Template Example](#template-example)
  - [CLI Invocation](#cli-invocation)
  - [Expected Output](#expected-output)
  - [Verification](#verification)
  - [Anti-Patterns](#anti-patterns)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 001: KNOWLEDGE-FIRST PROJECTION ***

## Context

In traditional code generation systems, templates are populated with arbitrary data structures, leading to inconsistent outputs and difficult debugging. GGen establishes a foundational principle: **all generation must flow from a semantic knowledge graph**. This pattern is the cornerstone of the entire GGen architecture, enabling deterministic behavior, multi-language support, and the delta-driven projection system.

## Problem

**How do you ensure generated code is consistent, traceable, and semantically grounded when templates could accept arbitrary input data?**

Without a semantic foundation, code generators suffer from:
- Inconsistent variable naming and data types across templates
- No way to trace generated artifacts back to their semantic source
- Difficulty reasoning about what can be generated from available data
- Template parameters that drift from actual domain concepts
- No shared vocabulary between different generation targets

## Forces

- Templates need concrete data to render, but arbitrary data leads to chaos
- Different output languages (Rust, Python, TypeScript) need the same semantic concepts
- Developers expect to query and reason about available knowledge before generation
- Generated code should be traceable to specific domain entities and relationships
- Knowledge graphs provide rich semantics but aren't directly consumable by templates
- SPARQL queries can extract precisely what's needed but require graph data first

## Solution

**Therefore, establish the knowledge graph as the single source of truth for all generation.**

Before any template execution:

1. **Load semantic data** into an RDF graph (Turtle format)
2. **Query the graph** using SPARQL to extract projection data
3. **Project graph results** into template-friendly JSON structures
4. **Render templates** using projected data only

The knowledge graph becomes the authoritative representation of your domain. Templates never receive arbitrary data—only projections derived from semantic queries.

```
┌─────────────────────────────────────────────────────────────┐
│                    KNOWLEDGE-FIRST FLOW                      │
└─────────────────────────────────────────────────────────────┘

    Domain Model                Graph Store              Template
    ════════════                ═══════════              ════════
         │                           │                       │
         │  1. Load .ttl            │                       │
         ├──────────────────────────>│                       │
         │                           │                       │
         │                           │  2. SPARQL Query      │
         │                           │<──────────────────────│
         │                           │                       │
         │                           │  3. JSON Projection   │
         │                           ├──────────────────────>│
         │                           │                       │
         │                           │                       │  4. Render
         │                           │                       ├────────>
         │                           │                       │
    ┌────┴────┐              ┌───────┴────────┐      ┌──────┴──────┐
    │ domain: │              │ :User a :Person│      │ struct User │
    │  User   │  ========>   │ :name "Alice"  │ ───> │   name: str │
    │  name   │              │ :email "..."    │      │   email:str │
    └─────────┘              └─────────────────┘      └─────────────┘

## Result

You achieve:

- **Semantic Consistency**: All generated artifacts share the same domain vocabulary
- **Traceability**: Every generated element traces back to specific graph nodes
- **Query-Driven Design**: Templates declare their data needs via SPARQL
- **Multi-Target Generation**: Same graph projects to Rust, Python, TypeScript, etc.
- **Validation**: Invalid queries fail before template execution
- **Composability**: Queries can be composed and extended

This pattern enables → **Pattern 002: DETERMINISTIC ENGINE** (same input graph = same output)
This pattern enables → **Pattern 003: GRAPH-TEMPLATE BINDING** (SPARQL frontmatter)
This pattern enables → **Pattern 004: NOUN-VERB CLI** (semantic operations)

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
pub struct {{className}} {
    pub {{propName}}: {{propType}},
{{/each}}
}
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
pub struct User {
    pub name: String,
    pub email: String,
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

## Anti-Patterns

❌ **Direct Template Data**: Passing JSON directly to templates bypasses semantic grounding
❌ **Graph as Cache**: Loading graphs only for performance, not as source of truth
❌ **Template-Side Queries**: Embedding SPARQL in template logic instead of frontmatter
❌ **Multiple Sources**: Mixing graph data with configuration files or environment variables

## Related Patterns

- **Pattern 002: DETERMINISTIC ENGINE** - Ensures same graph → same output
- **Pattern 003: GRAPH-TEMPLATE BINDING** - How templates declare graph dependencies
- **Pattern 004: NOUN-VERB CLI** - Semantic operations on graph entities
- **Pattern 005: MULTI-LANGUAGE PROJECTION** - Same graph → multiple languages
