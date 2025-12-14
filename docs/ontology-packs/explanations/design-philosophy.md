<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Explanation: Design Philosophy](#explanation-design-philosophy)
  - [1. Universal Standards Over Proprietary Formats](#1-universal-standards-over-proprietary-formats)
  - [2. Composition Over Hardcoding](#2-composition-over-hardcoding)
  - [3. Templates Enable Infinite Extensibility](#3-templates-enable-infinite-extensibility)
  - [4. Packs as a Social Construct](#4-packs-as-a-social-construct)
  - [5. Marketplace Reuse Over Reimplementation](#5-marketplace-reuse-over-reimplementation)
  - [6. Progressive Disclosure of Complexity](#6-progressive-disclosure-of-complexity)
  - [7. DRY (Don't Repeat Yourself) Across Dimensions](#7-dry-dont-repeat-yourself-across-dimensions)
  - [8. Deterministic Output](#8-deterministic-output)
  - [9. Observe-Understand-Improve Cycle](#9-observe-understand-improve-cycle)
  - [10. Principle of Least Surprise](#10-principle-of-least-surprise)
  - [Summary Table](#summary-table)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Explanation: Design Philosophy

Core principles guiding the ontology-as-packs architecture.

## 1. Universal Standards Over Proprietary Formats

**Principle**: Use W3C standards (RDF, OWL, SPARQL) instead of custom formats.

**Why**:
- Broad ecosystem support (Protégé, GraphDB, Virtuoso, etc.)
- Integration with existing semantic web tools
- Tools and libraries exist for querying/reasoning
- Community can extend without vendor lock-in

**Example**:
- ✗ Custom JSON format for vocabulary
- ✓ RDF Turtle (W3C standard, human-readable)

```turtle
@prefix schema: <https://schema.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

schema:Product a rdfs:Class ;
  rdfs:label "Product" ;
  rdfs:comment "A physical product in a store" .

schema:name a rdfs:Property ;
  rdfs:domain schema:Product ;
  rdfs:range rdfs:Literal .
```

## 2. Composition Over Hardcoding

**Principle**: Build systems to combine things, not duplicate logic.

**Why**:
- One merge algorithm supports unlimited ontology combinations
- No need to pre-compute all combinations
- Community contributions compose naturally
- Reduces maintenance burden exponentially

**Example**:
- ✗ Maintain: schema-org, schema-org-with-dublin-core, schema-org-with-foaf, schema-org-with-both, ...
- ✓ Compose: Install three packs, merge as needed

```bash
# Instead of 6 hardcoded combinations:
# Install 1 pack
ggen ontology install schema-org

# Merge with others as needed
ggen ontology compose --packs schema-org,dublin-core
ggen ontology compose --packs schema-org,foaf
ggen ontology compose --packs schema-org,dublin-core,foaf
```

## 3. Templates Enable Infinite Extensibility

**Principle**: Define data once (ontology), generate code in unlimited ways.

**Why**:
- Add language without modifying ontology
- Community contributes templates
- Domain-specific code generation
- No hardcoding language-specific logic

**Example**:
- ✗ Hardcode TypeScript generation in Rust:
  ```rust
  fn generate_typescript(schema: &OntologySchema) {
      // 500 lines of TypeScript-specific logic
  }
  fn generate_rust(schema: &OntologySchema) {
      // 500 lines of Rust-specific logic
  }
  fn generate_python(schema: &OntologySchema) {
      // 500 lines of Python-specific logic
  }
  ```

- ✓ Use templates (same in all languages):
  ```typescript
  // Same 50 lines regardless of target language
  render_template("templates/{{ language }}/types.tera", schema)
  ```

## 4. Packs as a Social Construct

**Principle**: Ontology packs are social contracts between communities.

**Why**:
- Shared vocabulary → shared understanding
- Version numbers → predictable upgrades
- Lock files → reproducible builds
- Registry → discoverability

**Example - E-Commerce Ecosystem**:
```
Amazon, eBay, Shopify, Etsy
         ↓ (all use)
   schema-org (v3.13.0)
         ↓
Shared understanding of Product, Offer, Seller
         ↓
Zero integration friction
```

**2024 Alternative** (Real, actual problem):
```
Amazon uses ProductV12
eBay uses ShopItem
Shopify uses Item
Etsy uses GoodListing
    ↓
Integration failures
Support tickets: 300+
Maintenance: 80% debugging incompatibilities
```

## 5. Marketplace Reuse Over Reimplementation

**Principle**: Share solutions through the marketplace, not documentation.

**Why**:
- Consuming packs is 10 seconds
- Reading docs and building is 10 days
- Versioning prevents breaking changes
- Community incentive to build quality

**Example**:
- ✗ Doc: "Here's how to do e-commerce types" (10KB)
  → User must parse and implement
  → Likely has bugs or misses features
  → Different from other implementations

- ✓ Pack: `ggen ontology install ecommerce-ontology`
  → Exactly what everyone else uses
  → Maintained by domain experts
  → Automatically updated

## 6. Progressive Disclosure of Complexity

**Principle**: Simple use case = simple command. Complex use case = more options.

**Simple**: Just want types from Schema.org?
```bash
ggen ontology install schema-org
ggen ontology generate --schema schema-org.ttl --language typescript
```

**Medium**: Need to filter classes?
```bash
ggen ontology extract \
  --pack schema-org \
  --classes "Product,Offer,Organization" \
  --output subset.ttl
```

**Advanced**: Need custom composition with merge strategies?
```bash
ggen ontology compose \
  --packs "schema-org::Product,Offer" \
  --packs "dublin-core::*" \
  --merge-strategy priority \
  --output combined.ttl
```

**Expert**: Building domain-specific pack with templates?
```bash
# Create gpack.toml + ontology.ttl + templates/
ggen pack publish my-ontology --visibility public
```

## 7. DRY (Don't Repeat Yourself) Across Dimensions

**Principle**: Every dimension (language, ontology, feature) should be independent.

Avoid matrices:
- ✗ 12 languages × 5 ontologies × 4 features = 240 combinations to maintain
- ✓ Each dimension independent; combine as needed

**Dimensions**:

| Dimension | Values | Mechanism |
|-----------|--------|-----------|
| Ontology | Schema.org, Dublin Core, FOAF, ... | Packs (install any) |
| Language | TypeScript, Rust, Python, Go, ... | Templates (any language) |
| Feature | Zod, Serde, Pydantic, ... | Template options |
| Composition | Union, Intersection, Priority | Merge strategies |

**Cartesian Product**: Any combination works because no hardcoding.

## 8. Deterministic Output

**Principle**: Same input always produces same output.

**Why**:
- Reproducible builds
- Mergeable git history
- No "random" code generation surprises
- Safe to commit generated code

**Implementation**:
- Sort all iterators (BTreeMap not HashMap)
- Use stable output ordering
- Deterministic template rendering
- Version-locked dependencies

## 9. Observe-Understand-Improve Cycle

**Principle**: Gather feedback from usage, improve based on real patterns.

**Feedback Loop**:
1. **Observe**: Users publish packs to registry
2. **Understand**: Popular packs, common patterns, pain points
3. **Improve**: Update core system based on learnings

**Example**:
- Observe: Many users create ecommerce packs
- Understand: This is a common pattern
- Improve: Publish curated ecommerce-ontology in registry

## 10. Principle of Least Surprise

**Principle**: Behavior should match user expectations.

**Examples**:
- `ggen pack` commands work same as `npm`
- `ontology-lock.toml` same structure as `package-lock.json`
- Version conflicts use semver like npm
- Search/install/update feel familiar

**Not**:
- Custom versioning scheme
- Surprising CLI behavior
- Incompatible with industry conventions

## Summary Table

| Principle | Benefit | Mechanism |
|-----------|---------|-----------|
| Universal Standards | Ecosystem compatibility | Use RDF/OWL/SPARQL |
| Composition | No hardcoding | Merge strategies |
| Templates | Infinite extensibility | Template-driven generation |
| Packs as Social | Community alignment | Marketplace infrastructure |
| Marketplace Reuse | 80% less work | Share solutions |
| Progressive Disclosure | Accessible + powerful | Simple-to-advanced commands |
| DRY across dimensions | Linear complexity | Independent concerns |
| Deterministic Output | Reproducible builds | Sorting + versioning |
| Observe-Understand-Improve | Continuous evolution | Feedback loops |
| Principle of Least Surprise | Intuitive to use | Follow conventions |

These principles enable the system to scale from hobby projects to enterprise standardization.
