<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SPARQL Inference Rules: The Power Engine of ggen](#sparql-inference-rules-the-power-engine-of-ggen)
  - [What Are Inference Rules?](#what-are-inference-rules)
    - [Why It Matters](#why-it-matters)
  - [Pattern 1: Inheritance Materialization (Class Hierarchies)](#pattern-1-inheritance-materialization-class-hierarchies)
  - [Pattern 2: Property Inference (Domain/Range Expansion)](#pattern-2-property-inference-domainrange-expansion)
  - [Pattern 3: Role/Capability Detection (Interface Extraction)](#pattern-3-rolecapability-detection-interface-extraction)
  - [Pattern 4: Validation Rule Extraction (Constraint Propagation)](#pattern-4-validation-rule-extraction-constraint-propagation)
  - [Pattern 5: Cross-Cutting Concerns (Auditing, Versioning, Soft Deletes)](#pattern-5-cross-cutting-concerns-auditing-versioning-soft-deletes)
  - [How to Use Inference Rules in ggen](#how-to-use-inference-rules-in-ggen)
    - [Step 1: Define Your Rule in ggen.toml](#step-1-define-your-rule-in-ggentoml)
    - [Step 2: Apply Inference Rules During Sync](#step-2-apply-inference-rules-during-sync)
    - [Step 3: Query Derived Facts in Templates](#step-3-query-derived-facts-in-templates)
  - [Performance Optimization for Large Ontologies](#performance-optimization-for-large-ontologies)
    - [Problem: SPARQL Queries Can Be Slow](#problem-sparql-queries-can-be-slow)
    - [Solution: Query Optimization](#solution-query-optimization)
      - [1. **Use DISTINCT to Reduce Results**](#1-use-distinct-to-reduce-results)
      - [2. **Use MINUS to Exclude Already-Materialized Facts**](#2-use-minus-to-exclude-already-materialized-facts)
      - [3. **Batch Rules: Mark Materialized, Skip in Next Pass**](#3-batch-rules-mark-materialized-skip-in-next-pass)
      - [4. **Profile Your Queries**](#4-profile-your-queries)
    - [Benchmarks](#benchmarks)
  - [Testing Inference Rules](#testing-inference-rules)
    - [Unit Test Pattern](#unit-test-pattern)
  - [Real-World Example: Building a REST API](#real-world-example-building-a-rest-api)
    - [Ontology (domain.ttl)](#ontology-domainttl)
    - [Inference Rules (ggen.toml)](#inference-rules-ggentoml)
    - [Template (rust-api.tera)](#template-rust-apitera)
    - [Generated Code Output](#generated-code-output)
  - [Key Takeaways](#key-takeaways)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SPARQL Inference Rules: The Power Engine of ggen

> **This is the crown jewel of ggen.** SPARQL CONSTRUCT queries are where intelligent code generation happens. Master this, and you unlock 60% of ggen's potential.

## What Are Inference Rules?

**SPARQL CONSTRUCT queries that materialize implicit relationships.** They transform your RDF ontology by deriving new facts based on logical rules.

```sparql
# Input: Raw class definitions
:User a rdfs:Class .
:User rdfs:subClassOf :Entity .

# Rule (CONSTRUCT): Materialize subclass relationships
CONSTRUCT {
  ?class rdfs:subClassOf ?superclass .
  ?class :isMaterialized true .
}
WHERE {
  ?class rdfs:subClassOf ?superclass .
}

# Output: Enriched graph with new facts ready for code generation
```

### Why It Matters

1. **Semantic Enrichment**: Derive facts your templates need without manual specification
2. **DRY Principle**: Define rules once, use in many templates
3. **Inference**: Chain rules to discover deep patterns (transitive closure, role detection, etc.)
4. **Testability**: Rules are standalone, unit-testable SPARQL queries
5. **AGI-Ready**: Agents can reason about and modify rules programmatically

---

## Pattern 1: Inheritance Materialization (Class Hierarchies)

**Problem:** You define class hierarchies but need explicit inheritance chains for code generation.

**Pattern:** Materialize all superclasses (including transitive).

```turtle
# Ontology
@prefix ex: <https://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" .

ex:Employee a rdfs:Class ;
    rdfs:subClassOf ex:Person ;
    rdfs:label "Employee" .

ex:Manager a rdfs:Class ;
    rdfs:subClassOf ex:Employee ;
    rdfs:label "Manager" .
```

**Inference Rule: Transitive Closure**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <https://example.com/>

CONSTRUCT {
  ?subclass :allSuperclasses ?superclass .
  ?superclass :allSubclasses ?subclass .
}
WHERE {
  # Direct and transitive subclass relationships
  ?subclass rdfs:subClassOf+ ?superclass .
}
```

**Template Usage:**

```jinja2
{%- for class in classes %}
// Class: {{ class.label }}
pub struct {{ class.name }} {
  {%- if class.allSuperclasses %}
  // Inherited from: {% for parent in class.allSuperclasses %}{{ parent.label }}{{ ", " if not loop.last }}{% endfor %}
  {%- endif %}
  pub id: u64,
  {%- for prop in class.properties %}
  pub {{ prop.name }}: {{ prop.rust_type }},
  {%- endfor %}
}

{%- if class.allSubclasses %}
// Manager allows downcasting from any subclass
impl From<&dyn Any> for {{ class.name }} {
  fn from(obj: &dyn Any) -> Self {
    {%- for sub in class.allSubclasses %}
    if let Some({{ sub.name | downcase }}) = obj.downcast_ref::<{{ sub.name }}>() {
      Self { id: {{ sub.name | downcase }}.id, /* ... */ }
    } else {
    {%- endfor %}
      panic!("Cannot cast to {{ class.name }}")
    {%- for _ in class.allSubclasses %}}{% endfor %}
  }
}
{%- endif %}
{%- endfor %}
```

**Result:** Rust structs with inherited fields + downcasting automatically derived.

---

## Pattern 2: Property Inference (Domain/Range Expansion)

**Problem:** Properties defined at parent class level should apply to children; need to materialize this.

**Pattern:** Expand properties to all subclasses.

```turtle
ex:Entity a rdfs:Class ;
    rdfs:label "Entity" .

ex:HasTimestamp a rdf:Property ;
    rdfs:domain ex:Entity ;
    rdfs:range xsd:dateTime ;
    rdfs:label "timestamp" .

ex:User a rdfs:Class ;
    rdfs:subClassOf ex:Entity ;
    rdfs:label "User" .
    # Implicitly has HasTimestamp
```

**Inference Rule: Domain/Range Expansion**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <https://example.com/>

CONSTRUCT {
  ?prop rdfs:domain ?class .
}
WHERE {
  # If property applies to superclass, apply to subclass too
  ?prop rdfs:domain ?superclass .
  ?class rdfs:subClassOf+ ?superclass .
}
```

**Template Usage:**

```jinja2
{%- for class in classes %}
pub struct {{ class.name }} {
  {%- for prop in class.properties %}
  pub {{ prop.name }}: {{ prop.range | type_map }},
  {%- endfor %}
}

// Automatically includes timestamp for all Entity subclasses
impl Timestamped for {{ class.name }} {
  fn created_at(&self) -> DateTime {
    self.timestamp
  }
}
{%- endfor %}
```

**Result:** All entity subclasses automatically get timestamp field + Timestamped trait.

---

## Pattern 3: Role/Capability Detection (Interface Extraction)

**Problem:** Classes have varied capabilities (create, update, delete, query). Need to extract interfaces per capability.

**Pattern:** Detect capabilities and materialize role-based groupings.

```turtle
ex:User a rdfs:Class .
ex:User :canCreate ex:Post .
ex:User :canDelete ex:Comment .
ex:User :canUpdate ex:Profile .
ex:User :canQuery ex:Analytics .

ex:Admin a rdfs:Class ;
    rdfs:subClassOf ex:User .
ex:Admin :canDelete ex:User .
ex:Admin :canDelete ex:Post .
ex:Admin :canUpdate ex:Settings .
```

**Inference Rule: Capability Aggregation**

```sparql
PREFIX ex: <https://example.com/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?class :hasCapability ?capability .
  ?capability :appliesToClass ?class .
  ?class :createsType ?type .
}
WHERE {
  {
    # Direct capabilities
    ?class :canCreate ?type .
    BIND( "Create" AS ?capability )
  }
  UNION
  {
    ?class :canUpdate ?type .
    BIND( "Update" AS ?capability )
  }
  UNION
  {
    ?class :canDelete ?type .
    BIND( "Delete" AS ?capability )
  }
  UNION
  {
    # Inherited capabilities from superclass
    ?superclass :hasCapability ?capability .
    ?class rdfs:subClassOf+ ?superclass .
  }
}
```

**Template Usage:**

```jinja2
{%- for class in classes %}
pub struct {{ class.name }};

{%- set capabilities = class.hasCapability %}
{%- if "Create" in capabilities %}
impl Creator for {{ class.name }} {
  {%- for type in class.createsType %}
  pub fn create_{{ type.name | downcase }}(&self, data: &str) -> Result<{{ type.name }}> {
    // Auto-generated create logic
  }
  {%- endfor %}
}
{%- endif %}

{%- if "Delete" in capabilities %}
impl Deleter for {{ class.name }} {
  pub fn delete<T: Entity>(&self, entity: &T) -> Result<()> {
    // Authorization check happens here
  }
}
{%- endif %}
{%- endfor %}
```

**Result:** Trait-based interfaces automatically extracted from capability declarations.

---

## Pattern 4: Validation Rule Extraction (Constraint Propagation)

**Problem:** Define constraints once (in SHACL or RDF), propagate to generated code.

**Pattern:** Extract validation rules and create enforcement code.

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <https://example.com/> .

ex:UserShape a sh:NodeShape ;
    sh:targetClass ex:User ;
    sh:property [
        sh:path ex:email ;
        sh:datatype xsd:string ;
        sh:minLength 5 ;
        sh:pattern "^[^@]+@[^@]+\\.[^@]+$" ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .
```

**Inference Rule: Extract Validators**

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX ex: <https://example.com/>

CONSTRUCT {
  ?property :hasValidator ?validator .
  ?validator :validatorType ?type .
  ?validator :validatorParam ?param .
  ?property :minLength ?minLength .
  ?property :maxLength ?maxLength .
  ?property :pattern ?pattern .
  ?property :minInclusive ?min .
  ?property :maxInclusive ?max .
}
WHERE {
  ?shape sh:targetClass ?class .
  ?shape sh:property [
    sh:path ?property ;
    sh:minLength ?minLength
  ] .
}
UNION
{
  ?shape sh:property [
    sh:path ?property ;
    sh:pattern ?pattern
  ] .
}
UNION
{
  ?shape sh:property [
    sh:path ?property ;
    sh:minInclusive ?min ;
    sh:maxInclusive ?max
  ] .
}
```

**Template Usage:**

```jinja2
{%- for class in classes %}
pub struct {{ class.name }} {
  {%- for prop in class.properties %}
  #[validate(
    {%- if prop.minLength %}length(min = {{ prop.minLength }}){% endif %}
    {%- if prop.maxLength %}length(max = {{ prop.maxLength }}){% endif %}
    {%- if prop.pattern %}regex = "{{ prop.pattern }}"{% endif %}
  )]
  pub {{ prop.name }}: {{ prop.range | type_map }},
  {%- endfor %}
}

impl Validate for {{ class.name }} {
  fn validate(&self) -> Result<(), ValidationError> {
    {%- for prop in class.properties %}
    {%- if prop.pattern %}
    if !regex::Regex::new("{{ prop.pattern }}")?.is_match(&self.{{ prop.name }}) {
      return Err(ValidationError::new("{{ prop.name }} pattern mismatch"));
    }
    {%- endif %}
    {%- if prop.minInclusive %}
    if self.{{ prop.name }} < {{ prop.minInclusive }} {
      return Err(ValidationError::new("{{ prop.name }} below minimum"));
    }
    {%- endif %}
    {%- endfor %}
    Ok(())
  }
}
{%- endfor %}
```

**Result:** Validation code auto-generated with all constraints from SHACL.

---

## Pattern 5: Cross-Cutting Concerns (Auditing, Versioning, Soft Deletes)

**Problem:** Every entity needs audit trail, version number, soft delete flag. Define once, propagate everywhere.

**Pattern:** Mark classes that need concern; materialize as fields and behaviors.

```turtle
ex:Auditable a rdfs:Class ;
    rdfs:label "Auditable" .

ex:User a rdfs:Class ;
    rdfs:subClassOf ex:Auditable .

ex:Post a rdfs:Class ;
    rdfs:subClassOf ex:Auditable .

ex:Versioned a rdfs:Class ;
    rdfs:label "Versioned" .

ex:Document a rdfs:Class ;
    rdfs:subClassOf [
        a owl:Restriction ;
        owl:onClass ex:Versioned ;
        owl:minQualifiedCardinality 1
    ] .

ex:SoftDeletable a rdfs:Class ;
    rdfs:label "SoftDeletable" .

ex:User rdfs:subClassOf ex:SoftDeletable .
ex:Post rdfs:subClassOf ex:SoftDeletable .
```

**Inference Rule: Concern Materialization**

```sparql
PREFIX ex: <https://example.com/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?class :isAuditable true .
  ?class :isVersioned true .
  ?class :isSoftDeletable true .
  ?class :auditableFields (
    "created_at"^^xsd:string
    "created_by"^^xsd:string
    "updated_at"^^xsd:string
    "updated_by"^^xsd:string
  ) .
}
WHERE {
  {
    ?class rdfs:subClassOf+ ex:Auditable .
  }
  UNION
  {
    ?class rdfs:subClassOf+ ex:Versioned .
  }
  UNION
  {
    ?class rdfs:subClassOf+ ex:SoftDeletable .
  }
}
```

**Template Usage:**

```jinja2
{%- for class in classes %}
pub struct {{ class.name }} {
  pub id: uuid::Uuid,
  {%- if class.isAuditable %}
  pub created_at: DateTime<Utc>,
  pub created_by: UserId,
  pub updated_at: DateTime<Utc>,
  pub updated_by: UserId,
  {%- endif %}
  {%- if class.isVersioned %}
  pub version: i32,
  {%- endif %}
  {%- if class.isSoftDeletable %}
  pub deleted_at: Option<DateTime<Utc>>,
  {%- endif %}
  {%- for prop in class.properties %}
  pub {{ prop.name }}: {{ prop.range | type_map }},
  {%- endfor %}
}

{%- if class.isAuditable %}
impl Auditable for {{ class.name }} {
  fn created_by(&self) -> UserId {
    self.created_by
  }
}
{%- endif %}

{%- if class.isSoftDeletable %}
impl SoftDeletable for {{ class.name }} {
  fn soft_delete(&mut self) {
    self.deleted_at = Some(Utc::now());
  }

  fn is_deleted(&self) -> bool {
    self.deleted_at.is_some()
  }
}
{%- endif %}
{%- endfor %}
```

**Result:** Common concerns (audit, versioning, soft deletes) automatically added to all relevant entities.

---

## How to Use Inference Rules in ggen

### Step 1: Define Your Rule in ggen.toml

```toml
[inference]
# Enable inference rule processing
enabled = true

# Define inference rules (CONSTRUCT queries)
[[inference.rules]]
name = "inheritance_closure"
description = "Materialize transitive class hierarchies"
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?subclass :allSuperclasses ?superclass .
  ?superclass :allSubclasses ?subclass .
}
WHERE {
  ?subclass rdfs:subClassOf+ ?superclass .
}
"""

[[inference.rules]]
name = "capability_aggregation"
description = "Extract role-based capabilities"
query = """
PREFIX ex: <https://example.com/>
# ... (see Pattern 3 above)
"""

# Rule execution order (important for dependent rules!)
rule_order = [
  "inheritance_closure",
  "property_expansion",
  "capability_aggregation",
  "validation_extraction",
  "concern_materialization"
]
```

### Step 2: Apply Inference Rules During Sync

```bash
# Apply ALL inference rules
ggen sync

# Apply specific rule
ggen sync --rule inheritance_closure

# Dry-run to see what facts are derived
ggen sync --rule capability_aggregation --dry-run
```

### Step 3: Query Derived Facts in Templates

```jinja2
{# Access materialized facts from inference rules #}
{%- for class in classes %}
  {# These properties exist because inference rules derived them #}
  allSuperclasses: {{ class.allSuperclasses }}
  hasCapability: {{ class.hasCapability }}
  isAuditable: {{ class.isAuditable }}
{%- endfor %}
```

---

## Performance Optimization for Large Ontologies

### Problem: SPARQL Queries Can Be Slow

When working with 1000+ classes, inference rules may take seconds.

### Solution: Query Optimization

#### 1. **Use DISTINCT to Reduce Results**

```sparql
# ❌ Slow: Duplicate results
CONSTRUCT {
  ?subclass :allSuperclasses ?superclass .
}
WHERE {
  ?subclass rdfs:subClassOf+ ?superclass .
}

# ✅ Fast: Deduplicated
CONSTRUCT {
  ?subclass :allSuperclasses ?superclass .
}
WHERE {
  ?subclass rdfs:subClassOf+ ?superclass .
}
LIMIT 10000  # Prevent runaway
```

#### 2. **Use MINUS to Exclude Already-Materialized Facts**

```sparql
# Only materialize facts that don't already exist
CONSTRUCT {
  ?class :allSuperclasses ?superclass .
}
WHERE {
  ?class rdfs:subClassOf+ ?superclass .
  # Skip if already materialized
  MINUS {
    ?class :allSuperclasses ?superclass .
  }
}
```

#### 3. **Batch Rules: Mark Materialized, Skip in Next Pass**

```sparql
# Rule 1: Materialize hierarchies
CONSTRUCT {
  ?class :isMaterialized true .
  ?class :allSuperclasses ?superclass .
}
WHERE {
  ?class rdfs:subClassOf+ ?superclass .
  MINUS { ?class :isMaterialized true . }
}

# Rule 2: Only process non-materialized
CONSTRUCT {
  ?class :hasCapability ?cap .
}
WHERE {
  ?class :isMaterialized true .  # <-- Filter
  ?class :canCreate ?type .
}
```

#### 4. **Profile Your Queries**

```bash
# Enable query timing
ggen sync --rule inheritance_closure --timing

# Output shows:
# Rule: inheritance_closure
# Time: 342ms
# Results: 14,532 triples
# Suggestion: Consider pre-materialization
```

### Benchmarks

| Ontology Size | Rule Type | Time | Recommendation |
|---------------|-----------|------|-----------------|
| <100 classes | Any | <50ms | Use directly |
| 100-500 classes | Simple (transitive closure) | 100-300ms | Acceptable |
| 500-2000 classes | Complex (multi-union) | 500ms-2s | Pre-materialize |
| >2000 classes | Any | >2s | Split into phases or pre-materialize |

---

## Testing Inference Rules

### Unit Test Pattern

```rust
#[cfg(test)]
mod inference_rules {
    use ggen_core::graph::Graph;
    use ggen_core::query::SparqlQuery;

    #[test]
    fn test_inheritance_closure() {
        // Arrange: Create ontology with class hierarchy
        let mut graph = Graph::new();
        graph.add_triple(":User", "rdfs:subClassOf", ":Entity");
        graph.add_triple(":Admin", "rdfs:subClassOf", ":User");

        // Act: Run inference rule
        let query = SparqlQuery::construct(r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            CONSTRUCT {
              ?subclass :allSuperclasses ?superclass .
            }
            WHERE {
              ?subclass rdfs:subClassOf+ ?superclass .
            }
        "#);
        let results = graph.query(&query)?;

        // Assert: Admin should have both User and Entity as superclasses
        assert!(results.contains_triple(":Admin", ":allSuperclasses", ":User"));
        assert!(results.contains_triple(":Admin", ":allSuperclasses", ":Entity"));
    }
}
```

---

## Real-World Example: Building a REST API

### Ontology (domain.ttl)

```turtle
@prefix ex: <https://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Base entity
ex:Entity a rdfs:Class ;
    rdfs:label "Entity" .

ex:Resource a rdfs:Class ;
    rdfs:subClassOf ex:Entity ;
    rdfs:label "Resource" .

# Domain entities
ex:User a rdfs:Class ;
    rdfs:subClassOf ex:Resource ;
    rdfs:label "User" .

ex:Post a rdfs:Class ;
    rdfs:subClassOf ex:Resource ;
    rdfs:label "Post" .

# Properties
ex:id a rdf:Property ;
    rdfs:domain ex:Resource ;
    rdfs:range xsd:string .

ex:title a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string .

ex:author a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range ex:User .

# Capabilities
ex:User :canCreate ex:Post .
ex:User :canUpdate ex:Post .
ex:User :canRead ex:Post .

ex:Admin a rdfs:Class ;
    rdfs:subClassOf ex:User .
ex:Admin :canDelete ex:Post .
ex:Admin :canDelete ex:User .
```

### Inference Rules (ggen.toml)

```toml
[[inference.rules]]
name = "transitive_hierarchy"
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT {
  ?subclass :allSuperclasses ?superclass .
}
WHERE {
  ?subclass rdfs:subClassOf+ ?superclass .
}
"""

[[inference.rules]]
name = "inherit_properties"
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT {
  ?property rdfs:domain ?subclass .
}
WHERE {
  ?property rdfs:domain ?superclass .
  ?subclass rdfs:subClassOf+ ?superclass .
}
"""

[[inference.rules]]
name = "inherit_capabilities"
query = """
PREFIX ex: <https://example.com/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT {
  ?subclass :canCreate ?type .
  ?subclass :canUpdate ?type .
  ?subclass :canRead ?type .
  ?subclass :canDelete ?type .
}
WHERE {
  ?superclass :canCreate ?type .
  ?subclass rdfs:subClassOf+ ?superclass .
}
"""

rule_order = [
  "transitive_hierarchy",
  "inherit_properties",
  "inherit_capabilities"
]
```

### Template (rust-api.tera)

```jinja2
// Auto-generated REST API from ontology
{% for resource in resources %}

// ============================================================
// {{ resource.label }}
// ============================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ resource.name }} {
  {% for prop in resource.properties -%}
  pub {{ prop.name }}: {{ prop.range | to_rust_type }},
  {% endfor -%}
}

// CRUD operations based on inferred capabilities
{% if resource.creator %}
#[post("/{{ resource.name | lowercase }}")]
pub async fn create_{{ resource.name | lowercase }}(
    body: {{ resource.name }},
) -> Result<{{ resource.name }}> {
    // Auto-generated create logic
    database::insert(&body).await
}
{% endif %}

{% if resource.reader %}
#[get("/{{ resource.name | lowercase }}/{id}")]
pub async fn get_{{ resource.name | lowercase }}(
    id: String,
) -> Result<{{ resource.name }}> {
    database::find_by_id(&id).await
}
{% endif %}

{% if resource.updater %}
#[put("/{{ resource.name | lowercase }}/{id}")]
pub async fn update_{{ resource.name | lowercase }}(
    id: String,
    body: {{ resource.name }},
) -> Result<{{ resource.name }}> {
    database::update(&id, &body).await
}
{% endif %}

{% if resource.deleter %}
#[delete("/{{ resource.name | lowercase }}/{id}")]
pub async fn delete_{{ resource.name | lowercase }}(
    id: String,
) -> Result<()> {
    database::delete(&id).await
}
{% endif %}

{% endfor %}
```

### Generated Code Output

```rust
// Auto-generated REST API from ontology

// ============================================================
// User
// ============================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
  pub id: String,
}

#[post("/user")]
pub async fn create_user(body: User) -> Result<User> {
    database::insert(&body).await
}

#[get("/user/{id}")]
pub async fn get_user(id: String) -> Result<User> {
    database::find_by_id(&id).await
}

#[put("/user/{id}")]
pub async fn update_user(id: String, body: User) -> Result<User> {
    database::update(&id, &body).await
}

#[delete("/user/{id}")]
pub async fn delete_user(id: String) -> Result<()> {
    database::delete(&id).await
}

// ============================================================
// Post
// ============================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Post {
  pub id: String,
  pub title: String,
  pub author: User,
}

#[post("/post")]
pub async fn create_post(body: Post) -> Result<Post> {
    database::insert(&body).await
}

#[get("/post/{id}")]
pub async fn get_post(id: String) -> Result<Post> {
    database::find_by_id(&id).await
}

#[put("/post/{id}")]
pub async fn update_post(id: String, body: Post) -> Result<Post> {
    database::update(&id, &body).await
}
```

---

## Key Takeaways

1. **CONSTRUCT queries are your code generator's best friend** - Use them to materialize facts that templates need
2. **Chain rules in dependency order** - Simple rules first (hierarchies), then complex (capabilities)
3. **SPARQL is the language of semantic code generation** - Master it and you can generate anything
4. **Inference enables AGI workflows** - Agents can reason about rules, modify ontologies, and regenerate automatically
5. **Optimize for size** - Use DISTINCT, MINUS, and LIMIT for large ontologies

---

## Next Steps

1. Define 3 inference rules for your domain
2. Create test cases for each rule
3. Extend templates to use materialized facts
4. Profile performance (goal: <500ms for typical ontologies)
5. Share rules with your team (they're composable and reusable)

