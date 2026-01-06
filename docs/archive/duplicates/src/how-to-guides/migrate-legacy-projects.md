<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Migrate Legacy Projects to Ontology-Driven Development](#migrate-legacy-projects-to-ontology-driven-development)
  - [Migration Strategy](#migration-strategy)
    - [Phase 1: Extract (Weeks 1-2)](#phase-1-extract-weeks-1-2)
    - [Phase 2: Validate (Weeks 2-3)](#phase-2-validate-weeks-2-3)
    - [Phase 3: Parallel Implementation (Weeks 3-4)](#phase-3-parallel-implementation-weeks-3-4)
    - [Phase 4: Gradual Cutover (Weeks 4+)](#phase-4-gradual-cutover-weeks-4)
  - [Real-World Example: E-Commerce Migration](#real-world-example-e-commerce-migration)
    - [Before: Multiple Type Definitions](#before-multiple-type-definitions)
    - [After: Single Ontology](#after-single-ontology)
  - [Handling Legacy Quirks](#handling-legacy-quirks)
    - [Problem: Custom Validation Logic](#problem-custom-validation-logic)
    - [Problem: Custom Constructors](#problem-custom-constructors)
  - [Migration Checklist](#migration-checklist)
  - [Timeline Expectations](#timeline-expectations)
  - [When to Migrate](#when-to-migrate)
  - [Common Pitfalls](#common-pitfalls)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Migrate Legacy Projects to Ontology-Driven Development

Convert existing projects to use ggen's ontology-driven approach incrementally.

## Migration Strategy

The 80/20 approach: Start with your core domain model and migrate incrementally.

### Phase 1: Extract (Weeks 1-2)

**Goal**: Define your domain ontology from existing code

1. **Identify core entities**:
   ```bash
   # List all classes in your codebase
   find src -name "*.ts" -exec grep "^export class\|^export interface" {} \;

   # In Rust
   grep -r "^pub struct\|^pub enum" src/
   ```

2. **Create ontology skeleton**:
   ```turtle
   @prefix domain: <http://example.org/domain/> .
   @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

   # Start with your core entities
   domain:User a rdfs:Class ;
     rdfs:label "User" .

   domain:Product a rdfs:Class ;
     rdfs:label "Product" .
   ```

3. **Extract relationships**:
   ```turtle
   domain:name a rdf:Property ;
     rdfs:domain domain:User, domain:Product ;
     rdfs:range <http://www.w3.org/2001/XMLSchema#string> .
   ```

### Phase 2: Validate (Weeks 2-3)

**Goal**: Ensure ontology matches reality

```bash
# 1. Validate syntax
ggen ontology validate domain.ttl

# 2. Generate code from ontology
ggen ontology generate domain.ttl \
  --language typescript \
  --output generated-models.ts

# 3. Compare with existing code
diff existing-models.ts generated-models.ts

# 4. Refine ontology until they match
```

### Phase 3: Parallel Implementation (Weeks 3-4)

**Goal**: Run both systems side-by-side

```typescript
// Step 1: Import both old and new
import { User as OldUser } from './models/user.old';
import { User as NewUser } from './generated/models';

// Step 2: Create adapter
function migrateUser(oldUser: OldUser): NewUser {
  return {
    id: oldUser.id,
    email: oldUser.email,
    username: oldUser.username,
    isActive: oldUser.active,
    createdAt: new Date(oldUser.created_at),
  };
}

// Step 3: Use adapter in code
const newUser = migrateUser(oldUserData);
```

### Phase 4: Gradual Cutover (Weeks 4+)

**Goal**: Replace old code incrementally

```bash
# 1. Update one module at a time
git checkout -b feat/migrate-user-module

# 2. Replace old imports with new
# OLD: import { User } from './models/user.old';
# NEW: import { User } from './generated/models';

# 3. Run tests
npm test

# 4. Review and merge
git push origin feat/migrate-user-module
```

## Real-World Example: E-Commerce Migration

### Before: Multiple Type Definitions

**TypeScript** (`product.ts`):
```typescript
export interface Product {
  id: string;
  name: string;
  price: number;
  inStock: boolean;
}
```

**Python** (`models.py`):
```python
class Product:
    def __init__(self, id: str, name: str, price: float, in_stock: bool):
        self.id = id
        self.name = name
        self.price = price
        self.in_stock = in_stock
```

**Rust** (`models.rs`):
```rust
pub struct Product {
    pub id: String,
    pub name: String,
    pub price: f64,
    pub in_stock: bool,
}
```

**Problem**: Duplicate definitions, manual synchronization, type drift

### After: Single Ontology

Create `ecommerce.ttl`:

```turtle
@prefix shop: <http://example.org/ecommerce/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

shop:Product a rdfs:Class ;
  rdfs:label "Product" ;
  rdfs:comment "An e-commerce product" .

shop:id a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:string .

shop:name a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:string .

shop:price a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:decimal .

shop:inStock a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:boolean .
```

**Regenerate all languages**:

```bash
# Generate TypeScript
ggen ontology generate ecommerce.ttl \
  --language typescript \
  --output ts/models.ts

# Generate Python
ggen ontology generate ecommerce.ttl \
  --language python \
  --output python/models.py

# Generate Rust
ggen ontology generate ecommerce.ttl \
  --language rust \
  --output rust/src/models.rs
```

**Result**: Single source of truth, no drift, all in sync

## Handling Legacy Quirks

### Problem: Custom Validation Logic

**Legacy code**:
```typescript
export function isValidProduct(product: Product): boolean {
  return product.price > 0 && product.name.length >= 3;
}
```

**Solution**: Preserve in adapters

```typescript
import { Product } from './generated/models';

export function isValidProduct(product: Product): boolean {
  return product.price > 0 && product.name.length >= 3;
}
```

### Problem: Custom Constructors

**Legacy code**:
```typescript
export class Product {
  constructor(data: Partial<Product>) {
    Object.assign(this, {
      inStock: true,  // Default
      ...data
    });
  }
}
```

**Solution**: Wrap generated code

```typescript
import { Product as GeneratedProduct } from './generated/models';

export class Product extends GeneratedProduct {
  constructor(data: Partial<Product>) {
    super(data);
    if (this.inStock === undefined) {
      this.inStock = true;  // Default
    }
  }
}
```

## Migration Checklist

- [ ] **Extract**: Identify all entities and relationships
- [ ] **Create**: Define ontology in Turtle format
- [ ] **Validate**: Check syntax and completeness
- [ ] **Generate**: Create code in all target languages
- [ ] **Compare**: Verify generated code matches legacy code
- [ ] **Test**: Run full test suite with generated code
- [ ] **Parallel**: Run both old and new simultaneously
- [ ] **Adapt**: Create adapters for special logic
- [ ] **Migrate**: Module-by-module replacement
- [ ] **Verify**: All tests pass post-migration
- [ ] **Cleanup**: Remove old code and files

## Timeline Expectations

| Phase | Duration | Activity |
|-------|----------|----------|
| Extract | 1-2 weeks | Identify entities, create ontology |
| Validate | 1 week | Compare with existing code, refine |
| Parallel | 1 week | Run both systems, create adapters |
| Cutover | 2-4 weeks | Gradually replace old code |
| Cleanup | 1 week | Remove legacy code, finalize |

**Total**: 2-3 months for typical project

## When to Migrate

**Good candidates**:
- ✅ Projects with multiple implementations
- ✅ Polyglot codebases
- ✅ Projects with frequent model changes
- ✅ Large teams with synchronization challenges

**Not good candidates**:
- ❌ Single-language, single-implementation projects
- ❌ Projects nearing end-of-life
- ❌ Very small projects (< 10 entities)

## Common Pitfalls

**1. Try to migrate everything at once**
- ❌ Risky, hard to debug
- ✅ Migrate module by module

**2. Ignore existing code patterns**
- ❌ Generated code doesn't match style
- ✅ Customize generation templates first

**3. Skip testing during migration**
- ❌ Bugs sneak through
- ✅ Run full test suite at each step

**4. Forget to update documentation**
- ❌ Team confused about model source
- ✅ Document ontology as source of truth

## Summary

You now know how to:
- ✅ Extract ontologies from existing code
- ✅ Validate and refine ontologies
- ✅ Migrate incrementally and safely
- ✅ Handle special cases and legacy code
- ✅ Run parallel systems during transition

Your legacy project is ready for modern, type-safe development!
