<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Limitations and Anti-Patterns](#limitations-and-anti-patterns)
  - [When NOT to Use ggen](#when-not-to-use-ggen)
    - [Scenario 1: Simple Single-Service App](#scenario-1-simple-single-service-app)
    - [Scenario 2: Highly Dynamic Schema](#scenario-2-highly-dynamic-schema)
    - [Scenario 3: Zero Code Generation Tolerance](#scenario-3-zero-code-generation-tolerance)
    - [Scenario 4: Performance-Sensitive Path](#scenario-4-performance-sensitive-path)
  - [Technical Limitations](#technical-limitations)
    - [Limitation 1: RDF Learning Curve](#limitation-1-rdf-learning-curve)
    - [Limitation 2: Namespace Management](#limitation-2-namespace-management)
    - [Limitation 3: Large Ontologies Get Unwieldy](#limitation-3-large-ontologies-get-unwieldy)
    - [Limitation 4: Circular Dependencies](#limitation-4-circular-dependencies)
    - [Limitation 5: Polyglot Type Mismatches](#limitation-5-polyglot-type-mismatches)
  - [Practical Limitations](#practical-limitations)
    - [Limitation 1: Generated Code Bloat](#limitation-1-generated-code-bloat)
    - [Limitation 2: Version Lock-in](#limitation-2-version-lock-in)
    - [Limitation 3: Language Feature Coverage](#limitation-3-language-feature-coverage)
    - [Limitation 4: Build Integration Complexity](#limitation-4-build-integration-complexity)
  - [When Limitations Are Acceptable](#when-limitations-are-acceptable)
    - [Use ggen when:](#use-ggen-when)
    - [Don't use ggen when:](#dont-use-ggen-when)
  - [Troubleshooting Common Issues](#troubleshooting-common-issues)
    - [Issue: "Generated code doesn't compile"](#issue-generated-code-doesnt-compile)
    - [Issue: "Ontology is too large"](#issue-ontology-is-too-large)
    - [Issue: "Teams have different type expectations"](#issue-teams-have-different-type-expectations)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Limitations and Anti-Patterns

Understanding what ggen does well and what it doesn't.

## When NOT to Use ggen

### Scenario 1: Simple Single-Service App

**Characteristics**:
- One programming language
- Simple CRUD operations (< 20 entities)
- Single database
- Unlikely to grow

**Why ggen is overkill**: You're solving a problem you don't have.
- Overhead of learning RDF/SPARQL not justified
- Manual type definitions are simpler
- No synchronization burden (one language)

**Better choice**: Generate types locally or use schema-to-code tools in your language

### Scenario 2: Highly Dynamic Schema

**Characteristics**:
- Schema changes weekly
- Experiments constantly
- No stabilized domain model
- Fluid requirements

**Why ggen is difficult**:
- Requires stable ontology to benefit from
- Constant regeneration adds friction
- Types change too fast to be useful

**Better choice**: Dynamic/schemaless database (MongoDB), runtime schema discovery

### Scenario 3: Zero Code Generation Tolerance

**Characteristics**:
- Team distrust of generated code
- Preference for hand-written code
- Audit/compliance requires human review
- Performance-critical sections

**Why ggen won't work**:
- Generated code must be trusted
- Team must understand and accept auto-generation
- Some generated code unavoidable

**Better choice**: Build core manually, use codegen only for scaffolding

### Scenario 4: Performance-Sensitive Path

**Characteristics**:
- Microsecond-level latency requirements
- Generated code overhead unacceptable
- Hand-tuned assembly/SIMD needed
- Real-time systems

**Why ggen struggles**:
- Generated code adds abstraction layers
- Generics/polymorphism may have overhead
- Can't optimize for micro-level performance

**Better choice**: Hand-write critical paths, use ggen for non-critical parts

## Technical Limitations

### Limitation 1: RDF Learning Curve

**Problem**: SPARQL and RDF concepts take time to learn

**Examples of confusion**:
- Why are there three ways to define properties?
- What's the difference between rdf:type and rdfs:type?
- How do I query relationships bidirectionally?

**Cost**: 1-2 weeks training for new developers

**Mitigation**:
- Hire experienced RDF developers
- Invest in team training
- Keep ontologies simple initially
- Document naming conventions

### Limitation 2: Namespace Management

**Problem**: Managing RDF namespaces is complex

**Example**:
```turtle
@prefix app: <http://example.org/app/> .
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .

# Which namespace does this belong in?
ex:User vs app:User vs schema:User
```

**Mitigation**:
- Define namespace policy upfront
- Document conventions
- Use consistent prefix names
- Validate namespace usage

### Limitation 3: Large Ontologies Get Unwieldy

**Problem**: Ontologies > 1000 classes become hard to manage

**Issues**:
- File becomes huge (10MB+)
- Query performance degrades
- Difficult to navigate
- Change conflicts in version control

**Mitigation**:
- Split into modules
- Use SPARQL views for subgraphs
- Generate from multiple ontologies
- Regular refactoring

### Limitation 4: Circular Dependencies

**Problem**: RDF allows (and doesn't prevent) circular references

```turtle
app:User rdfs:domain app:Order .
app:Order rdfs:domain app:User .
```

**Consequence**: Can create uncompilable code in languages that forbid cycles

**Mitigation**:
- Run validation: `ggen ontology validate --check-cycles`
- Break cycles in generated code with references/pointers
- Document expected cycle handling

### Limitation 5: Polyglot Type Mismatches

**Problem**: Some types don't map perfectly across languages

**Example**:
```turtle
xsd:integer range is 64-bit
```

Maps to:
- Rust: i64 ✅ (64-bit)
- Python: int ✅ (arbitrary precision)
- JavaScript: number ⚠️ (53-bit safe integer)

**Result**: Silent overflow on large integers in JavaScript

**Mitigation**:
- Use BigInt wrapper for large integers
- Document type limitations
- Test with boundary values
- Use type annotations

## Practical Limitations

### Limitation 1: Generated Code Bloat

**Problem**: Generated code can be 2-5x larger than hand-written

**Example**: User model

```typescript
// Hand-written (clean)
export interface User {
  id: string;
  email: string;
}

// Generated (with validation, serialization, docs)
/**
 * A user account
 * Generated from ontology
 */
export interface User {
  /** User ID */
  id: string;
  /** Email address */
  email: string;
}

export class UserModel implements User {
  constructor(data: Partial<User>) { /* ... */ }
  validate(): boolean { /* ... */ }
  toJSON(): string { /* ... */ }
  static fromJSON(json: string): User { /* ... */ }
  // + more methods
}
```

**Cost**: Larger bundle size, more memory

**Mitigation**:
- Tree-shake unused code
- Generate only what you need
- Use incremental compilation
- Profile bundle size

### Limitation 2: Version Lock-in

**Problem**: Changing generated code breaks regeneration

**Scenario**:
```typescript
// Generated code
export interface User {
  id: string;
}

// You modify:
export interface User {
  id: string;
  customField: boolean;  // Added manually
}

// Next generation:
export interface User {
  id: string;
}
// ERROR: Where did customField go?
```

**Solution**: Don't edit generated code. Extend instead:
```typescript
// Generated
export interface User { id: string; }

// Your code
export interface UserWithCustomField extends User {
  customField: boolean;
}
```

**Mitigation**:
- Mark generated code clearly
- Use non-editable files
- Extend, don't modify
- Regenerate regularly

### Limitation 3: Language Feature Coverage

**Problem**: Not all language features can be generated

**Missing features**:
- Generic constraints
- Async/await patterns
- Error handling strategies
- Performance optimizations

**Mitigation**:
- Generate interfaces, implement logic manually
- Use traits/type classes for extensibility
- Document expected manual additions
- Provide extension points

### Limitation 4: Build Integration Complexity

**Problem**: Build systems need to know about code generation

**Challenges**:
- Ordering: Generate before compile
- Caching: When to regenerate?
- Incremental builds: Only changed classes?
- CI/CD: Extra build step

**Mitigation**:
- Document build integration
- Provide build tool plugins
- Make caching explicit
- Automate with make/gradle/cargo-make

## When Limitations Are Acceptable

### Use ggen when:

1. **Multi-language requirement is driving force**
   - Cost of synchronization > learning curve

2. **Domain model is stable**
   - Ontology won't change hourly

3. **Team is willing to adopt semantic thinking**
   - RDF learning curve acceptable

4. **Type safety is high priority**
   - Benefit of consistency outweighs generated code cost

5. **Scale is significant**
   - 50+ developers, 100+ entities, 5+ languages

6. **Long-term project**
   - ROI justifies investment period

### Don't use ggen when:

- ❌ Single language, single database
- ❌ Schema changes constantly
- ❌ Team refuses generated code
- ❌ Microsecond latency critical
- ❌ Project duration < 3 months
- ❌ Domain model unclear/evolving

## Troubleshooting Common Issues

### Issue: "Generated code doesn't compile"

**Causes**:
- Invalid type mappings for target language
- Missing language-specific features
- Circular dependencies

**Fix**:
```bash
ggen ontology validate --check-compilation
ggen ontology validate --check-cycles
```

### Issue: "Ontology is too large"

**Causes**:
- > 1000 classes in single file
- Too many properties per class
- No logical separation

**Fix**:
```bash
# Split into modules
ggen ontology generate user-domain.ttl --output user-models.ts
ggen ontology generate product-domain.ttl --output product-models.ts

# Or use SPARQL for subsets
ggen packs sparql --query "SELECT * WHERE { ?c domain:database. }"
```

### Issue: "Teams have different type expectations"

**Causes**:
- No documented naming conventions
- Ambiguous ontology design
- Missing type constraints

**Fix**:
- Document naming policy
- Add comments in ontology
- Run validation with custom rules
- Review generated code as team

## Summary

ggen excels at:
- ✅ Multi-language synchronization
- ✅ Large, stable domain models
- ✅ Type-safe code generation
- ✅ Enforcing consistency

ggen struggles with:
- ❌ Simple projects (overkill)
- ❌ Highly dynamic schemas
- ❌ Teams that distrust generation
- ❌ Microsecond performance requirements

**Success depends on**: Right problem, right team, right expectations.

Choose ggen when its benefits (consistency, scale, safety) outweigh its costs (learning curve, complexity, abstraction overhead).
