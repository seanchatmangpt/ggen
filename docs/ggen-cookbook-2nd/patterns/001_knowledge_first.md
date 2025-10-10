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
