# How to Migrate Existing Code to ggen

## Overview

Migrating existing code to use ggen allows you to leverage ontology-driven development going forward. This guide helps you extract your domain model into an RDF ontology and regenerate code for multiple languages.

## Problem You're Solving

You have existing code with:
- Domain models spread across multiple files/languages
- Manual synchronization between language implementations
- Difficulty evolving the domain model
- High cost of maintaining consistency

By migrating to ggen, you'll have:
- A single source of truth (RDF ontology)
- Automated code generation for all languages
- Easy domain model evolution
- Zero drift between implementations

## Prerequisites

- Existing codebase with a clear domain model
- ggen installed and working
- Understanding of your domain entities and relationships
- Familiarity with [Getting Started Tutorial](../tutorials/getting-started.md)

## Assessment Phase

### Step 1: Map Your Domain Model

Document your current domain model. Create a simple document listing:

**Entities:**
- Product (name: string, price: decimal, sku: string)
- Order (id: uuid, customer: User, items: Product[], total: decimal)
- User (id: uuid, email: string, name: string)

**Relationships:**
- Order → User (customer)
- Order → Product[] (items)
- User → Order[] (orders)

**Constraints:**
- price >= 0.01
- email must be valid
- name is required

### Step 2: Identify Code Generation Candidates

Prioritize which components to generate:

**High Priority (Generate First):**
- Domain models (structs/classes/dataclasses)
- Type definitions and interfaces
- Serialization/deserialization code

**Medium Priority:**
- Repository/DAO interfaces
- API request/response models
- Validation schemas

**Lower Priority:**
- Business logic (generate scaffolds only)
- Complex algorithmic code
- Performance-critical code

## Extraction Phase

### Step 3: Create Your RDF Ontology

Convert your domain model to RDF. Use AI to generate the initial ontology:

```bash
ggen ai generate-ontology \
  --prompt "E-commerce domain: Product (name, price, sku), User (email, name), Order (items, customer, total)" \
  --output domain.ttl
```

Review the generated `domain.ttl`:

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Product a rdfs:Class ;
    rdfs:label "Product" .

ex:productName a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .
```

### Step 4: Refine the Ontology

Add constraints that match your existing code:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    sh:property [
        sh:path ex:price ;
        sh:minInclusive 0.01 ;
        sh:minCount 1 ;
    ] ;
    sh:property [
        sh:path ex:productName ;
        sh:minLength 1 ;
        sh:minCount 1 ;
    ] .
```

## Code Generation Phase

### Step 5: Generate Models from Ontology

Generate code for your primary language:

```bash
# For Rust
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/models.rs

# For TypeScript
ggen template generate-rdf \
  --ontology domain.ttl \
  --template typescript-models \
  --output src/models.ts

# For Python
ggen template generate-rdf \
  --ontology domain.ttl \
  --template python-pydantic \
  --output models.py
```

### Step 6: Compare with Existing Code

Compare generated code with your existing models:

```bash
# Compare Rust models
diff src/models.rs src/models_old.rs

# Compare TypeScript models
diff src/models.ts src/models_old.ts
```

Address any differences:
1. **Missing fields:** Add to ontology, regenerate
2. **Extra fields:** Remove from ontology, regenerate
3. **Type differences:** Update type mappings in ontology
4. **Validation differences:** Add SHACL constraints

### Step 7: Migrate Supporting Code

For code that's not automatically generated, create adapters:

**Example: Serialization (Rust)**
```rust
// Old code: Custom serde implementation
impl Serialize for Product { ... }

// New approach: Use generated models + derive
#[derive(Serialize, Deserialize)]
pub struct Product {
    pub name: String,
    pub price: Decimal,
}
```

**Example: Validation (Python)**
```python
# Old code: Manual validation
def validate_product(product):
    if not product.name or len(product.name) == 0:
        raise ValueError("name is required")
    if product.price <= 0:
        raise ValueError("price must be > 0")

# New approach: Use Pydantic (generated)
from models import Product
product = Product(name="Widget", price=9.99)  # Auto-validated
```

## Polyglot Generation Phase

### Step 8: Generate Code for Other Languages

Now that your ontology is solid, generate for all your target languages:

```bash
# Rust
ggen template generate-rdf --ontology domain.ttl --template rust-models

# TypeScript/JavaScript
ggen template generate-rdf --ontology domain.ttl --template typescript-models

# Python
ggen template generate-rdf --ontology domain.ttl --template python-pydantic

# Go
ggen template generate-rdf --ontology domain.ttl --template go-models

# Java
ggen template generate-rdf --ontology domain.ttl --template java-models
```

All language implementations now share the exact same domain model definition.

## Testing Phase

### Step 9: Validate Generated Code

#### Unit Tests

Test that generated models work correctly:

**Rust:**
```rust
#[test]
fn test_product_creation() {
    let product = Product {
        id: Uuid::new_v4(),
        name: "Widget".to_string(),
        price: Decimal::new(999, 2),  // 9.99
    };
    assert_eq!(product.name, "Widget");
}
```

**TypeScript:**
```typescript
test('Product creation', () => {
    const product: Product = {
        id: generateUuid(),
        name: "Widget",
        price: 9.99,
    };
    expect(product.name).toBe("Widget");
});
```

#### Integration Tests

Test that your existing business logic still works with generated models:

```bash
# Run existing test suite with generated models
cargo test
npm test
pytest
```

### Step 10: Round-Trip Serialization

Verify data can be serialized and deserialized correctly:

**Test data flow:**
1. Create object in one language
2. Serialize to JSON
3. Deserialize in another language
4. Verify all fields match

## Adoption Phase

### Step 11: Establish Regeneration Workflow

Create a standard workflow for future changes:

```bash
#!/bin/bash
# regenerate.sh

set -e

echo "Step 1: Validate ontology..."
ggen graph validate --ontology domain.ttl --shapes domain-shapes.ttl

echo "Step 2: Generate models..."
for template in rust-models typescript-models python-pydantic go-models; do
    ggen template generate-rdf --ontology domain.ttl --template $template
done

echo "Step 3: Run tests..."
cargo test
npm test
pytest

echo "All done! Models are in sync."
```

Run regularly:
```bash
chmod +x regenerate.sh
./regenerate.sh
```

### Step 12: Set Up Automation

Create a pre-commit hook to validate before commits:

```bash
ggen hook create pre-commit --name validate-and-regenerate
```

Set up CI/CD to regenerate on domain model changes:

```yaml
# .github/workflows/regenerate.yml
name: Regenerate Code

on:
  push:
    paths:
      - 'domain.ttl'
      - '.github/workflows/regenerate.yml'

jobs:
  regenerate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install ggen
        run: cargo install ggen
      - name: Regenerate code
        run: ./regenerate.sh
      - name: Create PR
        uses: peter-evans/create-pull-request@v4
```

## Gradual Migration Strategy

If migrating a large codebase, do it incrementally:

### Phase 1: Non-Critical Models (Week 1)
- Generate utility/helper models
- Test in isolated components
- Build confidence in process

### Phase 2: Core Domain Models (Week 2-3)
- Generate main business entities
- Update repositories and services
- Verify tests pass

### Phase 3: API Models (Week 4)
- Generate request/response models
- Update API handlers
- Full integration testing

### Phase 4: Complete Adoption (Week 5+)
- Generate all remaining models
- Remove manual code
- Establish continuous generation

## Rollback Plan

If issues arise during migration:

1. **Keep original code** in a separate branch
2. **Run both in parallel** during transition
3. **Compare outputs** between old and new
4. **Revert to original** if needed:
   ```bash
   git checkout original-main -- src/models.*
   ```

## Troubleshooting

### Generated Code Doesn't Match Existing Types

**Problem:** Generated `String` but existing code expects `&str`

**Solution:** Update template or adjust type mappings in ontology

### Tests Fail After Generation

**Problem:** Tests expect certain internal implementation details

**Solution:**
1. Update tests to be more decoupled
2. Use public API rather than internal structure
3. Add adapter layer if needed

### Performance Regression

**Problem:** Generated code is slower than original

**Solution:**
1. Profile both implementations
2. Optimize hot paths in template
3. Generate inline vs. separate module

## Success Metrics

After migration, you should have:
- ✅ All domain models in one ontology
- ✅ Code generated for 2+ languages
- ✅ No manual sync between implementations
- ✅ CI/CD validates ontology on changes
- ✅ New features require only ontology updates
- ✅ Tests pass across all languages

## See Also

- [Getting Started Tutorial](../tutorials/getting-started.md) - Basic ggen workflow
- [Ontology-to-Code Tutorial](../tutorials/ontology-to-code.md) - Complete workflow
- [Use RDF Ontologies Guide](use-rdf-ontologies.md) - Working with ontologies
- [How to Validate Ontologies](validate-ontologies-shacl.md) - Ensuring ontology quality
