# Ontology-to-Code Workflow

**Goal:** Master the complete ontology-driven development workflow from RDF to production code.

**What you'll learn:** How to create ontologies, extract data with SPARQL, and generate code across multiple languages.

## The Core Workflow

```
RDF Ontology (domain model)
         ↓
   SPARQL Queries (extract structure)
         ↓
  Templates (project to code)
         ↓
   Generated Code (Rust, TypeScript, Python...)
```

## Step 1: Create a Domain Ontology

Create `ecommerce.ttl`:

```turtle
@prefix ex: <http://example.org/ecommerce/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Customer account" .

ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "Product listing" .

ex:userName a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:productPrice a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .
```

## Step 2: Generate Rust Models

```bash
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template rust-models \
  --output src/models.rs
```

**Generated:**
```rust
pub struct User {
    pub id: Uuid,
    pub name: String,
}

pub struct Product {
    pub id: Uuid,
    pub price: f64,
}
```

## Step 3: Generate TypeScript Types

Same ontology, different language:

```bash
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template typescript-models \
  --output frontend/src/types/models.ts
```

**Generated:**
```typescript
export interface User {
  id: string;
  name: string;
}

export interface Product {
  id: string;
  price: number;
}
```

## Step 4: Evolve the Domain

Add a new field to `ecommerce.ttl`:

```turtle
ex:productSku a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "sku" .
```

Regenerate both languages:

```bash
# Regenerate Rust
ggen template generate-rdf --ontology ecommerce.ttl --template rust-models --output src/models.rs

# Regenerate TypeScript
ggen template generate-rdf --ontology ecommerce.ttl --template typescript-models --output frontend/src/types/models.ts
```

**Result:** Both `Product` structs now have `sku` fields. Zero manual edits!

## Understanding SPARQL Queries

Templates use SPARQL to extract data from the RDF graph:

```sparql
SELECT ?class ?property ?type
WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
    ?property rdfs:range ?type .
}
```

This query finds all classes, their properties, and property types.

## Key Concepts

1. **Ontology is source of truth:** All code derives from RDF
2. **SPARQL extracts structure:** Queries pull domain logic from graph
3. **Templates project to code:** Same ontology → any language
4. **Automatic synchronization:** Change ontology → regenerate → all code updates

## Advanced: Custom SPARQL Queries

Create custom queries for domain-specific logic:

```sparql
# Find all required properties (minCount = 1)
SELECT ?class ?property
WHERE {
    ?shape sh:targetClass ?class .
    ?shape sh:property [
        sh:path ?property ;
        sh:minCount 1
    ] .
}
```

Use in templates to generate validation code.

## Next Steps

- **Learn RDF/SPARQL:** [RDF/SPARQL Reference](../reference/rdf-sparql.md)
- **Create templates:** [Create Templates Guide](../how-to-guides/create-templates.md)
- **Understand architecture:** [Architecture Explanation](../explanations/architecture.md)

