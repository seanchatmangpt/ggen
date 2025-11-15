# Maturity Progression: From Simple to Enterprise

**Goal:** Understand ggen's maturity model and scale your projects progressively from 1-10 files to 10,000+ files.

**What you'll learn:** The 5-level maturity progression, when to level up, and patterns for each tier.

**Time:** 90 minutes | **Difficulty:** Intermediate

---

## Maturity Model Overview

ggen uses a 5-level maturity model mapping to project complexity and file count. Each level introduces new capabilities and patterns you'll need to master.

```
Level 1: Simple        (1-10 files)        ↓ Single template, basic ontology
Level 2: Small        (10-100 files)      ↓ Multiple templates, structured RDF
Level 3: Medium       (100-1K files)      ↓ Complex ontology, lifecycle integration
Level 4: Large        (1K-10K files)      ↓ Distributed ontologies, CI/CD pipelines
Level 5: Enterprise   (10K+ files)        ↑ Federated ontologies, multi-tenant, distributed
```

Each level builds on previous patterns. Attempting to jump levels without mastering foundations leads to maintenance nightmares.

---

## Level 1: Simple Projects (1-10 Files)

### Characteristics
- **Single ontology** - One domain model
- **Single template** - One code generation pattern
- **Manual deployment** - Direct execution
- **Monolithic structure** - Everything in one place
- **Time to generate** - <1 second

### When You're Ready
- You have a domain model
- You want to generate 1-5 core files
- You don't need complex orchestration

### Example: Blog Engine Models

**Ontology:** `blog.ttl`
```turtle
@prefix ex: <http://example.com/blog/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class ; rdfs:label "User" .
ex:Post a rdfs:Class ; rdfs:label "Post" .
ex:author a rdf:Property ; rdfs:domain ex:Post ; rdfs:range ex:User .
```

**Template:** `models.rs.tmpl`
```tera
{% query "queries/all-classes.rq" as classes %}

{% for class in classes %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} { }
{% endfor %}
```

**Generation:**
```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template . \
  --output src/
```

**Output:** 2-3 files (models.rs, possibly lib.rs)

### Patterns to Master
1. **Basic SPARQL queries** - SELECT, WHERE, basic patterns
2. **Tera templating** - Loops, conditionals, variables
3. **Type mapping** - XSD to Rust/TS/Python

### Key Files
- Single `.ttl` ontology file
- 1-2 template files
- Simple query files

### Validation Checklist
- ✅ Ontology is valid Turtle
- ✅ All required classes have labels
- ✅ All properties define domain and range
- ✅ Generated code compiles
- ✅ Generated code is idiomatic

---

## Level 2: Small Projects (10-100 Files)

### New Capabilities

#### 1. Multiple Templates
Generate different file types from same ontology:

```
templates/
├── models.rs.tmpl      → src/models.rs
├── api.rs.tmpl         → src/api.rs
├── db.sql.tmpl         → migrations/schema.sql
└── queries/
    ├── models.rq
    ├── relationships.rq
    └── aggregations.rq
```

#### 2. Structured Ontology
Organize RDF into logical sections:

```turtle
# Core domain model
@prefix domain: <http://example.com/domain/> .
domain:User a rdfs:Class .
domain:Post a rdfs:Class .

# API layer
@prefix api: <http://example.com/api/> .
api:GetUserEndpoint a rdfs:Class .
api:CreatePostEndpoint a rdfs:Class .

# Validation constraints
@prefix sh: <http://www.w3.org/ns/shacl#> .
[ sh:targetClass domain:User ;
  sh:property [ sh:path domain:email ; sh:minLength 5 ] ] .
```

#### 3. Template Variations
Use frontmatter variables for customization:

```yaml
---
to: "{{ output_dir }}/{{ filename }}"
vars:
  derive:
    default: ["Debug", "Clone", "Serialize"]
  visibility:
    default: "pub"
---
```

### When You're Ready
- Generating 10-50 files
- Multiple file types (code, config, docs)
- Need to customize generation per use case
- Team collaboration starting

### Example Project Structure

```
blog-app/
├── .ggen/
│   ├── ontologies/
│   │   ├── domain.ttl          # Core models
│   │   ├── api.ttl             # API endpoints
│   │   └── validation.ttl      # SHACL constraints
│   └── templates/
│       ├── rust/
│       │   ├── models.tmpl
│       │   ├── handlers.tmpl
│       │   └── db.tmpl
│       └── typescript/
│           ├── client.tmpl
│           └── types.tmpl
├── Cargo.toml
├── src/                        # Generated code
│   ├── models.rs
│   ├── handlers.rs
│   └── db.rs
└── web/                        # Generated frontend
    ├── api-client.ts
    └── types.ts
```

### Patterns to Master
1. **Multiple templates** - Generate different outputs
2. **SHACL validation** - Define constraints in RDF
3. **Template parameters** - Customize per use case
4. **File organization** - Logical project structure
5. **Multi-language generation** - Rust + TypeScript from one ontology

### New Queries You'll Need
```sparql
# Find required properties
SELECT ?class ?prop WHERE {
    ?shape sh:targetClass ?class ;
           sh:property [ sh:path ?prop ; sh:minCount 1 ] .
}

# Find validatable fields
SELECT ?class ?prop ?minLength WHERE {
    ?shape sh:targetClass ?class ;
           sh:property [ sh:path ?prop ; sh:minLength ?minLength ] .
}

# Extract relationships
SELECT ?source ?target WHERE {
    ?prop rdfs:domain ?source ; rdfs:range ?target .
    FILTER (?source != ?target)
}
```

### Key Learnings
- **Ontology becomes critical** - It's now your single source of truth
- **Template reusability** - Write once, generate to multiple languages
- **Variable management** - Frontmatter enables customization
- **Query complexity** - Advanced SPARQL patterns needed

---

## Level 3: Medium Projects (100-1K Files)

### New Capabilities

#### 1. Lifecycle Integration
Add hooks for pre/post generation steps:

```toml
[lifecycle]
phases = ["pre-generate", "generate", "validate", "post-generate"]

[[lifecycle.hooks]]
phase = "pre-generate"
script = "scripts/validate-ontology.sh"

[[lifecycle.hooks]]
phase = "post-generate"
script = "scripts/format-code.sh"
```

#### 2. Multi-Ontology Projects
Merge and compose multiple ontologies:

```bash
# Load from multiple sources
ggen graph query \
  --ontology domain.ttl \
  --ontology api.ttl \
  --ontology validation.ttl
```

#### 3. Incremental Generation
Use frozen sections to preserve manual code:

```rust
// Generated code
#[derive(Debug)]
pub struct User {
    pub id: String,
    pub name: String,
}

// FROZEN: Manual implementation
impl User {
    pub fn validate(&self) -> Result<()> {
        // Custom validation logic - preserved across regenerations
        if self.name.is_empty() {
            return Err("Name required");
        }
        Ok(())
    }
}
// FROZEN END
```

#### 4. CI/CD Integration
Automatically regenerate when ontology changes:

```yaml
name: Regen on Ontology Change
on:
  push:
    paths:
      - '.ggen/ontologies/**'

jobs:
  regenerate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Regenerate code
        run: ggen template generate-rdf --ontology .ggen/ontologies/domain.ttl
      - name: Commit changes
        run: |
          git add src/
          git commit -m "chore: regenerate from ontology"
          git push
```

### When You're Ready
- Generating 100-500 files
- Lifecycle needs (setup, cleanup, validation)
- Multiple teams working on different ontologies
- Automated code generation workflows
- Mixed generated and manual code

### Project Structure

```
monorepo/
├── .ggen/
│   ├── ontologies/
│   │   ├── shared.ttl          # Shared domain
│   │   ├── product.ttl         # Product bounded context
│   │   ├── orders.ttl          # Orders bounded context
│   │   └── validation.ttl      # Shared validation
│   └── templates/
│       ├── shared/             # Shared templates
│       ├── backend/            # Backend code gen
│       └── frontend/           # Frontend code gen
├── services/
│   ├── product-service/
│   │   ├── src/               # Generated code
│   │   └── hooks/             # Lifecycle hooks
│   └── order-service/
│       ├── src/               # Generated code
│       └── hooks/             # Lifecycle hooks
├── hooks/
│   ├── pre-generate.sh        # Ontology validation
│   ├── post-generate.sh       # Code formatting
│   └── validate.sh            # Generated code validation
└── ggen.lock                  # Lock file (like Cargo.lock)
```

### Patterns to Master
1. **Lifecycle hooks** - Pre/post generation automation
2. **Multi-ontology composition** - Shared + specific ontologies
3. **Frozen sections** - Protect manual code from regeneration
4. **CI/CD automation** - Trigger regeneration on changes
5. **Bounded contexts** - Organize large systems
6. **Workspace patterns** - Monorepo management

### Advanced Queries

```sparql
# Find all classes in multiple ontologies
SELECT DISTINCT ?class ?ontology WHERE {
    ?class a rdfs:Class .
    GRAPH ?graph { ?class ?p ?o }
    BIND(STRAFTER(STR(?graph), "#") as ?ontology)
}

# Find conflicts between ontologies
SELECT ?class ?prop1 ?prop2 WHERE {
    GRAPH ?g1 { ?class ?p1 ?prop1 }
    GRAPH ?g2 { ?class ?p2 ?prop2 }
    FILTER (?g1 != ?g2 && ?p1 = ?p2 && ?prop1 != ?prop2)
}
```

### Key Learnings
- **Automation saves time** - Hooks eliminate manual steps
- **Composition is power** - Multiple ontologies cover complex domains
- **Frozen sections enable iteration** - Don't lose manual work
- **CI/CD keeps in sync** - Ontology changes propagate automatically

---

## Level 4: Large Projects (1K-10K Files)

### New Capabilities

#### 1. Distributed Ontologies
Organize by domain with federation:

```
ontologies/
├── federation.ttl             # Links all domains
├── domains/
│   ├── accounting/            # Accounting domain
│   │   ├── core.ttl
│   │   ├── reports.ttl
│   │   └── integration.ttl
│   ├── inventory/             # Inventory domain
│   │   ├── core.ttl
│   │   ├── warehouse.ttl
│   │   └── integration.ttl
│   └── shared/                # Shared across domains
│       ├── types.ttl
│       ├── validation.ttl
│       └── events.ttl
```

#### 2. Template Composition
Build complex templates from simpler ones:

```yaml
---
# Parent template can include others
includes:
  - "common/validation.tera"
  - "common/serialization.tera"
  - "common/error-handling.tera"

to: "{{ service }}/src/models.rs"
---
```

#### 3. Performance Optimization
Use caching and parallel generation:

```bash
# Parallel generation across templates
ggen template generate-rdf \
  --ontology domain.ttl \
  --parallel 8 \
  --cache ~/.ggen/cache
```

#### 4. Quality Gates
Validate generated code before committing:

```bash
# Run validation suite
ggen template test \
  --template . \
  --ontology domain.ttl \
  --checks compilability \
  --checks idiomatic \
  --checks security
```

### When You're Ready
- Generating 1,000+ files
- Multiple teams, multiple domains
- Performance is a concern
- Need sophisticated validation
- Enterprise governance requirements

### Architecture Pattern: Domain-Driven Design

```
services/
├── accounting-service/
│   ├── src/
│   │   ├── models/       # Generated from accounting.ttl
│   │   ├── handlers/     # Generated from api.ttl
│   │   └── events/       # Generated from events.ttl
│   └── ggen/
│       ├── hooks/
│       └── config.toml
├── inventory-service/
│   ├── src/
│   │   ├── models/       # Generated from inventory.ttl
│   │   ├── handlers/     # Generated from api.ttl
│   │   └── warehouse/    # Generated from warehouse.ttl
│   └── ggen/
│       ├── hooks/
│       └── config.toml
└── shared/
    ├── ontologies/       # Federation point
    └── templates/        # Shared generation patterns
```

### Patterns to Master
1. **Domain federation** - Link independent ontologies
2. **Template composition** - Reuse template blocks
3. **Parallel generation** - Speed up large builds
4. **Quality validation** - Automated checks
5. **Performance tuning** - Cache strategies
6. **Multi-service coordination** - Cross-service consistency

### Advanced Configuration

```toml
[generation]
parallel_workers = 8
cache_dir = "~/.ggen/cache"
cache_ttl_hours = 24

[validation]
require_compilability = true
require_idiomatic = true
security_scan_level = "strict"

[performance]
max_files_per_template = 500
batch_size = 100
timeout_seconds = 300

[monitoring]
enable_metrics = true
log_level = "debug"
```

### Key Learnings
- **Scale requires organization** - Clear domain boundaries
- **Performance matters** - Caching and parallelism essential
- **Quality gates prevent issues** - Validate early
- **Monitoring enables optimization** - Metrics guide improvements

---

## Level 5: Enterprise (10K+ Files)

### New Capabilities

#### 1. Federated Ontologies
Multi-organization, multi-tenant support:

```
federation/
├── core.ttl                # Base types for entire enterprise
├── departments/
│   ├── sales/
│   │   └── ontology.ttl
│   ├── engineering/
│   │   └── ontology.ttl
│   └── marketing/
│       └── ontology.ttl
└── tenants/
    ├── customer-a/
    │   └── overrides.ttl
    └── customer-b/
        └── overrides.ttl
```

#### 2. Distributed Processing
Generate code across multiple workers:

```bash
# Distribute generation across cluster
ggen generate \
  --ontology federation.ttl \
  --distributed \
  --nodes node1.example.com,node2.example.com,node3.example.com \
  --replication 3
```

#### 3. Advanced Governance
Audit and approval workflows for generated code:

```yaml
governance:
  require_human_approval: true
  approval_roles: [architect, tech-lead]
  audit_trail: true
  rollback_enabled: true

  security:
    scan_generated_code: true
    require_security_review: true
    vulnerability_threshold: "critical"
```

#### 4. Multi-Tenant Code Generation
Generate per-customer variations:

```sparql
# Query with tenant context
SELECT ?entity ?property ?tenant_override
WHERE {
    ?entity a rdfs:Class .
    ?entity ?property ?baseValue .

    OPTIONAL {
        GRAPH <tenant://acme> {
            ?entity ?property ?tenant_override .
        }
    }

    BIND(COALESCE(?tenant_override, ?baseValue) as ?finalValue)
}
```

#### 5. Real-Time Regeneration
Watch and regenerate as ontology changes:

```bash
# Watch mode - regenerates on ontology changes
ggen generate \
  --ontology federation.ttl \
  --watch \
  --rebuild-on-change
```

### When You're Ready
- 10,000+ generated files
- Multiple organizations/teams
- Distributed infrastructure
- Governance and compliance requirements
- Multi-tenant product
- Real-time generation needs

### Enterprise Architecture

```
ggen-enterprise/
├── federation/
│   ├── core.ttl                # Enterprise baseline
│   ├── policies.ttl            # Governance rules
│   └── integration.ttl         # System integrations
├── tenants/
│   ├── customer-a/             # Customer A customizations
│   │   ├── ontology.ttl
│   │   └── overrides.ttl
│   └── customer-b/             # Customer B customizations
│       ├── ontology.ttl
│       └── overrides.ttl
├── distributed/
│   ├── node-1/                 # Distribution node
│   ├── node-2/                 # Distribution node
│   └── node-3/                 # Distribution node
├── governance/
│   ├── audit.log               # Audit trail
│   ├── approvals/              # Approval workflows
│   └── security/               # Security scans
└── services/
    ├── service-a/              # Generated for customer A
    ├── service-b/              # Generated for customer B
    └── service-shared/         # Shared infrastructure
```

### Patterns to Master
1. **Federation architecture** - Org-wide ontology management
2. **Distributed generation** - Multi-node processing
3. **Governance workflows** - Approval and audit
4. **Multi-tenancy** - Per-customer variations
5. **Real-time adaptation** - Watch-mode regeneration
6. **Compliance** - Audit trails and security
7. **Performance at scale** - Optimization for massive codebases

### Enterprise Best Practices

```toml
[enterprise]
federation_enabled = true
distributed_enabled = true
multi_tenant = true
governance_enabled = true

[security]
require_code_review = true
require_security_scan = true
require_audit_trail = true
encryption_key_rotation_days = 90

[compliance]
gdpr_compliant = true
hipaa_compliant = true
soc2_audit_trail = true

[performance]
cache_distributed = true
cache_replication = 3
parallel_workers = 64
batch_size = 1000
```

### Key Learnings
- **Federation enables scale** - Organization-wide patterns
- **Governance is non-negotiable** - Compliance requires process
- **Distribution is essential** - Cannot generate 10K+ files single-machine
- **Real-time matters** - Business agility requires rapid feedback
- **Security and audit** - Enterprise requirements

---

## Progression Checklist

### Before Moving to Next Level
- ✅ Current level fully mastered
- ✅ All patterns documented in team wiki
- ✅ All developers understand level concepts
- ✅ Automated tests for generation
- ✅ Performance benchmarks established

### Level 1 → Level 2
- ✅ Can write multiple templates
- ✅ Can structure complex ontologies
- ✅ Can use SHACL for validation
- ✅ Can generate to multiple languages

### Level 2 → Level 3
- ✅ Can manage lifecycle hooks
- ✅ Can merge ontologies
- ✅ Can use frozen sections effectively
- ✅ Can set up CI/CD automation

### Level 3 → Level 4
- ✅ Can design domain-driven ontologies
- ✅ Can compose templates
- ✅ Can optimize for performance
- ✅ Can validate generated code

### Level 4 → Level 5
- ✅ Can architect enterprise federation
- ✅ Can implement governance workflows
- ✅ Can manage multi-tenant generation
- ✅ Can operate distributed systems
- ✅ Can maintain audit trails

---

## Anti-Patterns to Avoid

### Level 1 Mistakes
- ❌ Ontology missing labels (breaks generation)
- ❌ Hardcoding values in templates (not reusable)
- ❌ No type mapping (generates unusable types)

### Level 2 Mistakes
- ❌ Unorganized ontology (hard to maintain)
- ❌ Template duplication (maintenance nightmare)
- ❌ Ignoring SHACL constraints (incomplete validation)

### Level 3 Mistakes
- ❌ Unchecked frozen sections (code rot)
- ❌ Missing CI/CD validation (silent failures)
- ❌ No test coverage (regressions creep in)

### Level 4 Mistakes
- ❌ Inadequate performance testing (surprises at scale)
- ❌ Unclear domain boundaries (coupling)
- ❌ Missing quality gates (poor generated code)

### Level 5 Mistakes
- ❌ Inadequate governance (compliance failures)
- ❌ Untracked changes (audit violations)
- ❌ Single point of failure (distributed without redundancy)

---

## Getting Help

- **Level 1-2 Questions** → [SPARQL Cookbook](../reference/sparql-cookbook.md)
- **Level 2-3 Questions** → [Testing Strategy](testing-strategy.md)
- **Level 3-4 Questions** → [Architecture Deep Dive](../explanations/architecture-deep-dive.md)
- **Level 4-5 Questions** → Contact ggen core team for enterprise consulting

---

## Example Projects by Level

### Level 1
- [simple blog model](../../examples/basic-template-generation/)
- [single ontology generation](../../examples/demo-project/)

### Level 2
- [full stack app](../../examples/full-stack-app/)
- [microservices](../../examples/microservices-architecture/)

### Level 3
- [monorepo example](../../examples/rust-monorepo/)
- [lifecycle complete](../../examples/lifecycle-complete/)

### Level 4
- [advanced pipeline](../../examples/advanced-pipeline/)
- [comprehensive showcase](../../examples/comprehensive-rust-showcase/)

### Level 5
- [maturity matrix showcase](../../examples/maturity-matrix-showcase/)
- [enterprise federation patterns](contact core team)

---

## Next Steps

1. **Find your current level** - Where are you generating files now?
2. **Master all level patterns** - Complete checklist before advancing
3. **Read advanced docs** - As you approach next level
4. **Run example projects** - See patterns in action
5. **Contribute back** - Share what you learn with community
