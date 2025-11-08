# Quick Start: Your First Generation in 5 Minutes

**Goal:** Generate a Rust REST API from an RDF ontology in 5 minutes.

**What you'll learn:** The core ggen workflow: ontology → SPARQL queries → code generation across any language.

## The ggen Philosophy

Traditional generators copy templates. ggen **projects semantic knowledge** into code:

```
RDF Ontology (single source of truth)
         ↓
   SPARQL Queries (extract domain logic)
         ↓
  Code Generation (Rust, TypeScript, Python...)
```

Change the ontology → code automatically updates. **One ontology, unlimited projections.**

## Step 1: Create Your First Ontology (1 minute)

Let's model a simple REST API: Users, Products, Orders.

**Option A: AI-Powered (fastest)**
```bash
ggen ai generate-ontology \
  --prompt "E-commerce API: User (name, email), Product (title, price), Order (user, products, total)" \
  --output ecommerce.ttl
```

**Option B: Manual RDF (learn the fundamentals)**

Create `ecommerce.ttl`:
```turtle
@prefix ex: <http://example.org/ecommerce/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Define domain classes
ex:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Customer account" .

ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "Product listing" .

ex:Order a rdfs:Class ;
    rdfs:label "Order" ;
    rdfs:comment "Customer order" .

# Define properties
ex:userName a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:userEmail a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" .

ex:productTitle a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "title" .

ex:productPrice a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .

ex:orderUser a rdf:Property ;
    rdfs:domain ex:Order ;
    rdfs:range ex:User ;
    rdfs:label "user" .

ex:orderTotal a rdf:Property ;
    rdfs:domain ex:Order ;
    rdfs:range xsd:decimal ;
    rdfs:label "total" .
```

**Key insight:** This RDF ontology is your **single source of truth**. All code generates from here.

## Step 2: Generate Rust Models (1 minute)

Now project this ontology into Rust structs:

```bash
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template rust-models \
  --output src/
```

**Generated `src/models.rs`:**
```rust
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub id: Uuid,
    pub title: String,
    pub price: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub id: Uuid,
    pub user_id: Uuid,
    pub total: f64,
}
```

**What happened?**
1. ggen loaded `ecommerce.ttl` into an RDF graph
2. SPARQL queries extracted class definitions and properties
3. Templates rendered Rust structs with correct types (`xsd:string` → `String`, `xsd:decimal` → `f64`)

## Step 3: Generate REST API Endpoints (1 minute)

Same ontology, different projection—now generate API handlers:

```bash
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template rust-axum-api \
  --output src/
```

**Generated `src/api/users.rs`:**
```rust
use axum::{Json, extract::Path};
use uuid::Uuid;
use crate::models::User;

pub async fn get_user(Path(id): Path<Uuid>) -> Json<User> {
    // TODO: Fetch from database
    Json(User {
        id,
        name: "Example".to_string(),
        email: "user@example.com".to_string(),
    })
}

pub async fn list_users() -> Json<Vec<User>> {
    // TODO: Fetch from database
    Json(vec![])
}

pub async fn create_user(Json(user): Json<User>) -> Json<User> {
    // TODO: Save to database
    Json(user)
}
```

**Same ontology → different template → REST API code!**

## Step 4: Generate TypeScript Frontend (1 minute)

Let's prove the point: **one ontology, unlimited languages**.

```bash
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template typescript-models \
  --output frontend/src/types/
```

**Generated `frontend/src/types/models.ts`:**
```typescript
export interface User {
  id: string;
  name: string;
  email: string;
}

export interface Product {
  id: string;
  title: string;
  price: number;
}

export interface Order {
  id: string;
  userId: string;
  total: number;
}
```

**Key insight:** Rust, TypeScript, Python—all generated from the **same RDF ontology**. Update `ecommerce.ttl` once, regenerate all languages.

## Step 5: Evolve Your Domain (1 minute)

Business requirement: "Add product categories."

**Edit `ecommerce.ttl`** (add 5 lines):
```turtle
ex:Category a rdfs:Class ;
    rdfs:label "Category" ;
    rdfs:comment "Product category" .

ex:productCategory a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range ex:Category ;
    rdfs:label "category" .
```

**Regenerate everything:**
```bash
# Rust models
ggen template generate-rdf --ontology ecommerce.ttl --template rust-models --output src/

# Rust API
ggen template generate-rdf --ontology ecommerce.ttl --template rust-axum-api --output src/

# TypeScript types
ggen template generate-rdf --ontology ecommerce.ttl --template typescript-models --output frontend/src/types/
```

**Result:** All code now has `category` fields. Zero manual edits.

## What Just Happened?

You experienced the **ontology-driven workflow**:

1. **Single source of truth:** RDF ontology defines your domain
2. **SPARQL extraction:** Queries pull structured data from the graph
3. **Multi-language projection:** Same ontology → Rust, TypeScript, Python, GraphQL...
4. **Automatic sync:** Change ontology → regenerate → all code updates

This isn't template expansion—it's **semantic code generation**.

## Next Steps

### Learn the Template System
Understand how templates use SPARQL to extract ontology data: [Templates Guide](templates.md)

### Browse the Marketplace
Discover pre-built ontologies and templates: [Marketplace Guide](marketplace.md)

### Advanced Workflows
- **SHACL validation:** Ensure ontology consistency before generation
- **SPARQL customization:** Write custom queries for domain-specific logic
- **Multi-project sync:** Share one ontology across microservices

### Full Example Projects

```bash
# Microservices architecture
ggen project new my-microservices --type rust-microservices
cd my-microservices && cat README.md

# GraphQL API from ontology
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template rust-graphql-api \
  --output graphql/

# Python FastAPI + Pydantic models
ggen template generate-rdf \
  --ontology ecommerce.ttl \
  --template python-pydantic \
  --output models.py
```

## Common Patterns

### Pattern 1: Domain-First Development
```bash
# 1. Model domain in RDF (NOT code)
ggen ai generate-ontology --prompt "Healthcare FHIR Patient" --output domain.ttl

# 2. Generate all code layers
ggen template generate-rdf --ontology domain.ttl --template rust-models
ggen template generate-rdf --ontology domain.ttl --template rust-api
ggen template generate-rdf --ontology domain.ttl --template typescript-sdk

# 3. Evolve domain (add Patient.allergies)
# 4. Regenerate → code auto-updates
```

### Pattern 2: Marketplace Bootstrap
```bash
# Search for existing ontologies
ggen marketplace search "e-commerce"

# Install and extend
ggen marketplace install io.ggen.ontologies.ecommerce
ggen template generate-rdf \
  --ontology .ggen/ontologies/io.ggen.ontologies.ecommerce/schema.ttl \
  --template rust-models
```

### Pattern 3: Multi-Repo Sync
```bash
# Shared ontology repository
cd ontologies/
ggen ai generate-ontology --prompt "Shared domain model" --output shared.ttl

# Backend (Rust)
cd ../backend/
ggen template generate-rdf --ontology ../ontologies/shared.ttl --template rust-models

# Frontend (TypeScript)
cd ../frontend/
ggen template generate-rdf --ontology ../ontologies/shared.ttl --template typescript-models

# Mobile (Kotlin)
cd ../mobile/
ggen template generate-rdf --ontology ../ontologies/shared.ttl --template kotlin-models
```

**Key advantage:** Update `shared.ttl` once → regenerate all repos → guaranteed type safety across stack.

## Troubleshooting

### "Template not found"

```bash
# List available templates
ggen template list

# If template missing, install from marketplace
ggen marketplace search "rust-models"
ggen marketplace install io.ggen.templates.rust-models
```

### "SPARQL query failed"

```bash
# Validate ontology syntax
ggen graph validate ecommerce.ttl

# Inspect loaded graph
ggen graph query ecommerce.ttl --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

### "Invalid RDF syntax"

```bash
# Use AI to fix
ggen ai generate-ontology --prompt "Fix this RDF: $(cat broken.ttl)" --output fixed.ttl

# Or validate manually
ggen graph validate broken.ttl --verbose
```

---

**Congratulations!** You've mastered the ontology-driven workflow. Now explore [Templates](templates.md) to customize SPARQL queries and create your own projections.
