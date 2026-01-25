<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Zero to Generated Code - Complete Walkthrough](#zero-to-generated-code---complete-walkthrough)
  - [Prerequisites](#prerequisites)
  - [Step 1: Generate Your Domain Ontology (2 min)](#step-1-generate-your-domain-ontology-2-min)
  - [Step 2: Generate Rust Code (2 min)](#step-2-generate-rust-code-2-min)
  - [Step 3: Generate TypeScript Code (1 min)](#step-3-generate-typescript-code-1-min)
  - [Step 4: Modify the Ontology (2 min)](#step-4-modify-the-ontology-2-min)
  - [Step 5: Regenerate Code (1 min)](#step-5-regenerate-code-1-min)
  - [Step 6: Generate Documentation (1 min)](#step-6-generate-documentation-1-min)
  - [Step 7: Integrate with Your Project (1 min)](#step-7-integrate-with-your-project-1-min)
  - [What You've Accomplished](#what-youve-accomplished)
  - [Advanced: Automate with Hooks](#advanced-automate-with-hooks)
  - [Next Steps](#next-steps)
    - [1. Customize Templates](#1-customize-templates)
    - [2. Understand RDF/SPARQL](#2-understand-rdfsparql)
    - [3. Advanced Workflows](#3-advanced-workflows)
    - [4. Production Setup](#4-production-setup)
  - [Troubleshooting](#troubleshooting)
    - ["ggen ai generate-ontology" fails](#ggen-ai-generate-ontology-fails)
    - [Generated code has syntax errors](#generated-code-has-syntax-errors)
    - [TypeScript types don't match Rust types](#typescript-types-dont-match-rust-types)
  - [Key Takeaways](#key-takeaways)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Zero to Generated Code - Complete Walkthrough

**Goal:** Generate production-ready Rust code from a domain description in 10 minutes.

**What you'll learn:**
- How to generate an ontology from natural language
- How to generate code in multiple languages
- How to modify the ontology and regenerate
- How to integrate with your project

**Time:** 10 minutes | **Difficulty:** Beginner

---

## Prerequisites

✅ ggen installed (verify: `ggen --version`)
✅ Rust installed (for generated code)
✅ 10 minutes of time

## Step 1: Generate Your Domain Ontology (2 min)

Start by describing your domain in plain English. We'll build an e-commerce API with products and orders.

```bash
# Generate ontology from natural language
ggen ai generate-ontology \
  --prompt "E-commerce domain: Product with name, price, inventory count; Order with user email, items list, total amount; User with name and email" \
  --output domain.ttl
```

**What happened:**
- AI analyzed your description
- Created RDF ontology in `domain.ttl`
- Ready for code generation

**Verify it worked:**
```bash
cat domain.ttl
```

You should see something like:
```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Product a rdfs:Class ;
  rdfs:label "Product" ;
  rdfs:comment "A product in the catalog" .

ex:name a rdf:Property ;
  rdfs:domain ex:Product ;
  rdfs:range xsd:string ;
  rdfs:label "name" .

ex:price a rdf:Property ;
  rdfs:domain ex:Product ;
  rdfs:range xsd:decimal ;
  rdfs:label "price" .

# ... more properties and classes ...
```

---

## Step 2: Generate Rust Code (2 min)

Now generate Rust models from the ontology:

```bash
# Generate Rust code
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/models.rs
```

**What happened:**
- ggen queried the RDF ontology with SPARQL
- Mapped RDF types to Rust types (xsd:string → String, xsd:decimal → f64)
- Generated Rust structs with proper derives

**Check the output:**
```bash
cat src/models.rs
```

You should see:
```rust
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub name: String,
    pub price: f64,
    pub inventory_count: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub user_email: String,
    pub items: Vec<Product>,
    pub total_amount: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
}
```

---

## Step 3: Generate TypeScript Code (1 min)

Generate the same types in TypeScript:

```bash
# Generate TypeScript interfaces
ggen template generate-rdf \
  --ontology domain.ttl \
  --template typescript-models \
  --output types.ts
```

**Check the output:**
```bash
cat types.ts
```

You should see:
```typescript
export interface Product {
  name: string;
  price: number;
  inventory_count: number;
}

export interface Order {
  user_email: string;
  items: Product[];
  total_amount: number;
}

export interface User {
  name: string;
  email: string;
}
```

**Key insight:** Same ontology, different languages. Types are perfectly synchronized.

---

## Step 4: Modify the Ontology (2 min)

Now add a new field to demonstrate the power of regeneration.

Edit `domain.ttl` and add a `rating` field to Product:

```bash
# Open in your editor
nano domain.ttl
# or
vim domain.ttl
```

Find the `ex:Product` class and add:

```turtle
ex:rating a rdf:Property ;
  rdfs:domain ex:Product ;
  rdfs:range xsd:decimal ;
  rdfs:label "rating" ;
  sh:minInclusive 0 ;
  sh:maxInclusive 5 .
```

Save the file.

---

## Step 5: Regenerate Code (1 min)

Regenerate both Rust and TypeScript:

```bash
# Regenerate Rust
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/models.rs

# Regenerate TypeScript
ggen template generate-rdf \
  --ontology domain.ttl \
  --template typescript-models \
  --output types.ts
```

**Verify the changes:**
```bash
# Rust
grep "pub rating" src/models.rs

# TypeScript
grep "rating:" types.ts
```

**Result:**
```rust
// In Rust
pub rating: f64,

// In TypeScript
rating: number;
```

**The magic:** One ontology change → Both languages updated automatically. No manual sync needed.

---

## Step 6: Generate Documentation (1 min)

Generate API documentation from the ontology:

```bash
# Generate markdown documentation
ggen template generate-rdf \
  --ontology domain.ttl \
  --template markdown-schema \
  --output SCHEMA.md
```

**View the documentation:**
```bash
cat SCHEMA.md
```

You'll see a formatted table describing all classes, properties, and constraints.

---

## Step 7: Integrate with Your Project (1 min)

Add the generated code to your Rust project:

```bash
# Create a new Rust project (if you don't have one)
cargo new --lib myapp
cd myapp

# Run all generation steps
ggen template generate-rdf \
  --ontology ../domain.ttl \
  --template rust-models \
  --output src/models.rs

ggen template generate-rdf \
  --ontology ../domain.ttl \
  --template rust-graphql-api \
  --output src/api.rs

# Format the generated code
cargo fmt

# Run tests
cargo test
```

---

## What You've Accomplished

✅ Generated RDF ontology from natural language
✅ Generated Rust code from the ontology
✅ Generated TypeScript code from the same ontology
✅ Modified the ontology
✅ Regenerated code in all languages (perfectly synchronized)
✅ Generated documentation
✅ Integrated into a real project

---

## Advanced: Automate with Hooks

Set up a Git hook to automatically regenerate code when ontology changes:

```bash
# Create a pre-commit hook
ggen hook create pre-commit \
  --name regenerate-on-ontology-change \
  --command "ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/models.rs && cargo fmt" \
  --timeout 120
```

Now when you commit changes to `domain.ttl`:
1. Code regenerates automatically
2. Rust formatter runs
3. Only then is the commit allowed

---

## Next Steps

### 1. Customize Templates
Learn how to create your own templates:
- [How to Create Templates](../how-to-guides/create-templates.md)

### 2. Understand RDF/SPARQL
Go deeper into the semantic foundation:
- [RDF/SPARQL Reference](../reference/rdf-sparql.md)
- [Ontology-Driven Development](../explanations/ontology-driven.md)

### 3. Advanced Workflows
Explore more complex scenarios:
- [Real-World Workflows](../tutorials/real-world-workflows.md)
- [Marketplace Templates](../tutorials/marketplace-workflow.md)

### 4. Production Setup
Prepare for production use:
- [Deploy to Production](../how-to-guides/deploy-production.md)
- [CI/CD Integration](../how-to-guides/cicd-workflows.md)

---

## Troubleshooting

### "ggen ai generate-ontology" fails

Make sure you have an API key configured:
```bash
ggen utils doctor
```

Or use a local model:
```bash
ggen ai generate-ontology \
  --prompt "your domain" \
  --model ollama:llama2 \
  --output domain.ttl
```

### Generated code has syntax errors

Check that the ontology is valid:
```bash
ggen graph load domain.ttl
ggen graph query --sparql "SELECT ?s WHERE {?s a rdfs:Class}"
```

If empty, the ontology wasn't loaded properly.

### TypeScript types don't match Rust types

Run both templates again to ensure they use the same ontology version:
```bash
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/models.rs
ggen template generate-rdf --ontology domain.ttl --template typescript-models --output types.ts
```

---

## Key Takeaways

1. **One Ontology, Many Languages:** Define your domain once, generate code in all languages
2. **Type Safety:** RDF types automatically map to language-specific types
3. **No Drift:** Change ontology → All code regenerates → Perfect sync
4. **Validation:** Constraints in ontology (sh:minInclusive, sh:pattern) become validation in code
5. **Automation:** Hooks can regenerate code automatically when ontology changes

---

## See Also

- [Quick Start Tutorial](getting-started.md) - 5-minute intro
- [Type Mapping Reference](../reference/type-mapping.md) - All type mappings
- [SPARQL Cookbook](../reference/sparql-cookbook.md) - Query patterns by task
- [Template Directives](../reference/template-directives.md) - Template metadata syntax
