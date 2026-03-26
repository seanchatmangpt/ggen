# YAWL Codegen Architecture Guide

Comprehensive technical documentation of ggen-yawl's architecture, design patterns, and transformation pipeline.

**Version**: 0.1.0
**Last Updated**: 2026-03-26

## Table of Contents

1. [System Overview](#system-overview)
2. [The Five-Stage Pipeline](#the-five-stage-pipeline)
3. [Transformation Patterns](#transformation-patterns)
4. [Rule<Q,T> Composition Pattern](#ruleqt-composition-pattern)
5. [Data Flow](#data-flow)
6. [Design Decisions](#design-decisions)
7. [Module Architecture](#module-architecture)

---

## System Overview

### Design Equation

```
A = μ(O)

where:
  A = Artifact (generated code/workflow)
  μ = Transformation operator (5-stage pipeline)
  O = Ontology (RDF source)
```

### Core Concept

ggen-yawl transforms **industry ontologies** (FIBO, HL7, ISO standards) into **executable YAWL workflows** and **Spring Boot applications** through a deterministic five-stage pipeline.

### Architecture Diagram

```
┌──────────────────────────────────────────────────────────────────────────┐
│                         YAWL Codegen Architecture                        │
└──────────────────────────────────────────────────────────────────────────┘

    Industry Ontology (RDF/Turtle/XML)
              │
              ▼ [μ₁ Normalize]
    ┌─────────────────────────────┐
    │ Load & Validate             │
    │ - RDF parsing               │
    │ - SHACL shape validation    │
    │ - Dependency resolution     │
    └─────────────────────────────┘
              │
              ▼ [μ₂ Extract]
    ┌─────────────────────────────┐
    │ SPARQL CONSTRUCT Queries    │
    │ - 6 transformation patterns │
    │ - YAWL intermediate RDF     │
    │ - Property mapping          │
    └─────────────────────────────┘
              │
              ▼ [μ₃ Emit]
    ┌─────────────────────────────┐
    │ Template Rendering (Tera)   │
    │ - YAWL XML generation       │
    │ - Erlang code generation    │
    │ - Java code generation      │
    └─────────────────────────────┘
              │
              ▼ [μ₄ Canonicalize]
    ┌─────────────────────────────┐
    │ Deterministic Formatting    │
    │ - Normalize spacing/order   │
    │ - Content hashing           │
    │ - Reproducibility check     │
    └─────────────────────────────┘
              │
              ▼ [μ₅ Receipt]
    ┌─────────────────────────────┐
    │ Validation & Auditing       │
    │ - Schema validation         │
    │ - Cryptographic proof       │
    │ - Audit trail generation    │
    └─────────────────────────────┘
              │
              ▼
    ┌──────────────────────────────────────────────┐
    │ Generated Artifacts                          │
    ├──────────────────────────────────────────────┤
    │ YAWL XML      │ Java Code (Rules 3-10)      │
    │ Erlang Module │ Hibernate Mappings          │
    │ Reports       │ Jackson Serializers         │
    └──────────────────────────────────────────────┘
```

---

## The Five-Stage Pipeline

### Stage μ₁: Normalize

**Purpose**: Load and validate the industry ontology.

**Inputs**:
- RDF/Turtle/XML files
- Ontology IRI and base namespace

**Processing**:
1. Parse RDF in multiple formats (Turtle, RDF/XML, N-Triples, N-Quads, TriG)
2. Build in-memory RDF graph (using Oxigraph)
3. Apply SHACL shape validation
4. Resolve owl:imports dependencies
5. Flatten imports (optional)

**Outputs**:
- Validated RDF graph
- Error reports (if validation fails)

**Validation Rules**:
- All classes must have `rdfs:label`
- Object properties must have domain/range
- Functional properties must be marked

**Performance SLO**: <5 seconds for 1,000+ triples

### Stage μ₂: Extract

**Purpose**: Execute SPARQL CONSTRUCT queries to extract domain-specific patterns.

**Inputs**:
- Validated RDF graph from μ₁
- 6 pattern-specific CONSTRUCT queries (see [Transformation Patterns](#transformation-patterns))

**Processing**:
1. Execute each CONSTRUCT query in dependency order
2. Merge results into intermediate RDF graph
3. Apply inference rules (optional)
4. Topological sort for deterministic ordering
5. Generate unique identifiers (hash-based)

**Outputs**:
- YAWL intermediate RDF graph
- Pattern bindings (task ids, flows, conditions)

**Queries Executed** (in order):
1. Extract tasks from classes
2. Extract flows from properties
3. Extract split/join from cardinality
4. Extract conditions from rules
5. Extract multiple instances
6. Extract composite tasks

**Performance SLO**: <2 seconds for all queries

### Stage μ₃: Emit

**Purpose**: Render templates to generate code artifacts.

**Inputs**:
- YAWL intermediate RDF graph from μ₂
- Tera template files
- Configuration (package names, output paths)

**Processing**:
1. Convert RDF graph to template context
2. Render each template (YAWL XML, Java, Erlang, HBM)
3. Apply custom filters and transformations
4. Generate multiple files per entity
5. Create directory structure

**Outputs**:
- YAWL XML workflow definition
- Java source files (8 types: entity, repository, DTO, controller, service, enum, serializer)
- Erlang source files (optional)
- Hibernate HBM XML mappings
- Jackson serializers

**Templates Used**:
- `workflow.yawl.tera` - YAWL XML specification
- `entity.java.tera` - JPA Entity
- `repository.java.tera` - Spring Data Repository
- `dto.java.tera` - Data Transfer Object
- `controller.java.tera` - REST Controller
- `service.java.tera` - Business Service
- `enum.java.tera` - Java Enum
- `hbm_mapping.xml.tera` - Hibernate Mapping
- `serializer.java.tera` - Jackson Serializer

**Performance SLO**: <1 second for all templates

### Stage μ₄: Canonicalize

**Purpose**: Ensure deterministic, reproducible output.

**Inputs**:
- Generated artifacts from μ₃

**Processing**:
1. Normalize whitespace (consistent indentation)
2. Sort elements deterministically (attributes alphabetically)
3. Normalize line endings (LF)
4. Calculate content hash (SHA-256)
5. Verify reproducibility by regenerating and comparing

**Outputs**:
- Canonical artifacts
- Content hashes for each file
- Determinism validation report

**Benefits**:
- Identical input always produces identical output
- Enables content-addressed storage
- Supports incremental updates (detect changes by hash)
- Reproducible builds

### Stage μ₅: Receipt

**Purpose**: Generate cryptographic proof and audit trail.

**Inputs**:
- Canonical artifacts from μ₄
- Hash values

**Processing**:
1. Generate receipt JSON with:
   - Timestamp (ISO8601)
   - Input ontology hash
   - Output artifact hashes
   - Stage completion times
   - Version information
2. Create audit log entries
3. Optionally sign with private key (if configured)

**Outputs**:
- `receipt.json` with cryptographic proof
- Audit log entries
- Error report (if any stage failed)

**Receipt Structure**:

```json
{
  "version": "0.1.0",
  "timestamp": "2026-03-26T14:30:00Z",
  "stage_1_normalize": {
    "status": "success",
    "duration_ms": 1200,
    "triples_loaded": 2450
  },
  "stage_2_extract": {
    "status": "success",
    "duration_ms": 800,
    "patterns_found": 12
  },
  "stage_3_emit": {
    "status": "success",
    "duration_ms": 450,
    "files_generated": 28
  },
  "stage_4_canonicalize": {
    "status": "success",
    "duration_ms": 100
  },
  "stage_5_receipt": {
    "status": "success",
    "duration_ms": 50
  },
  "artifacts": {
    "yawl_xml": {
      "path": ".ggen/yawl/workflow.yawl.xml",
      "hash": "sha256:abc123...",
      "size_bytes": 4521
    },
    "java_entity": {
      "path": "src/main/java/com/example/yawl/entity/YWorkItem.java",
      "hash": "sha256:def456...",
      "size_bytes": 1823
    }
  },
  "total_duration_ms": 2600
}
```

---

## Transformation Patterns

The YAWL codegen implements **6 transformation patterns** that map ontology constructs to YAWL workflow elements.

### Pattern 1: Class → Task

**Description**: Maps OWL classes to YAWL atomic tasks.

**SPARQL Query**:

```sparql
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX yawl: <http://unrdf.org/yawl#>

CONSTRUCT {
  ?task a yawl:AtomicTask ;
    yawl:taskId ?taskId ;
    yawl:taskName ?name ;
    yawl:isAutomatic false .
}
WHERE {
  ?class a owl:Class ;
    rdfs:label ?name .
  FILTER(?class != owl:Thing)
  BIND(SHA256(CONCAT(STR(?class), ?name)) AS ?taskId)
  BIND(IRI(CONCAT(STR(yawl:), "task/", ?taskId)) AS ?task)
}
```

**Example**:

**Input Ontology**:
```turtle
fibo:LoanApplication a owl:Class ;
    rdfs:label "Loan Application" .

fibo:CreditCheck a owl:Class ;
    rdfs:label "Credit Check" .
```

**Output YAWL**:
```xml
<task id="task-LoanApplication" name="Loan Application">
  <split type="XOR"/>
  <join type="XOR"/>
</task>

<task id="task-CreditCheck" name="Credit Check">
  <split type="XOR"/>
  <join type="XOR"/>
</task>
```

### Pattern 2: Object Property → Flow

**Description**: Maps RDF object properties to YAWL control flows.

**SPARQL Query**:

```sparql
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX yawl: <http://unrdf.org/yawl#>

CONSTRUCT {
  ?flow a yawl:Flow ;
    yawl:source ?sourceTaskId ;
    yawl:target ?targetTaskId ;
    yawl:isDefault true .
}
WHERE {
  ?property a owl:ObjectProperty ;
    rdfs:domain ?domainClass ;
    rdfs:range ?rangeClass .
  BIND(SHA256(CONCAT(STR(?domainClass))) AS ?sourceTaskId)
  BIND(SHA256(CONCAT(STR(?rangeClass))) AS ?targetTaskId)
  BIND(IRI(CONCAT(STR(yawl:), "flow/", ?sourceTaskId, "-", ?targetTaskId)) AS ?flow)
}
```

**Example**:

**Input Ontology**:
```turtle
fibo:hasNextStep a owl:ObjectProperty ;
    rdfs:domain fibo:LoanApplication ;
    rdfs:range fibo:CreditCheck .
```

**Output YAWL**:
```xml
<flow source="task-LoanApplication" target="task-CreditCheck"/>
```

### Pattern 3: Cardinality → Split/Join Type

**Description**: Maps property cardinality to split/join types.

**Rules**:
- `rdfs:maxCardinality 1` → XOR (exclusive choice)
- `rdfs:minCardinality >1` → AND (parallel split)
- No cardinality → OR (multiple choice)

**Example**:

**Input Ontology**:
```turtle
fibo:requiresApproval a owl:ObjectProperty ;
    rdfs:domain fibo:LoanApplication ;
    owl:maxCardinality 1 .  # XOR split

fibo:requiresCheck a owl:ObjectProperty ;
    rdfs:domain fibo:LoanApplication ;
    owl:minCardinality 2 .  # AND split
```

**Output YAWL**:
```xml
<!-- XOR split for exclusive choice -->
<task id="approval-check">
  <split type="XOR"/>
</task>

<!-- AND split for parallel branches -->
<task id="credit-check">
  <split type="AND"/>
</task>
```

### Pattern 4: Conditional Rules → Predicates

**Description**: Maps rule conditions to YAWL flow predicates.

**Example**:

**Input Ontology**:
```turtle
fibo:LoanApplication fibo:condition "amount > 100000"^^xsd:string .
fibo:condition rdfs:label "Condition" .
```

**Output YAWL**:
```xml
<flow source="input" target="highValueReview">
  <predicate>/amount > 100000</predicate>
</flow>
<flow source="input" target="standardProcessing">
  <predicate>/amount <= 100000</predicate>
</flow>
```

### Pattern 5: Multiple Instances

**Description**: Maps collection properties to multiple instance patterns.

**Example**:

**Input Ontology**:
```turtle
fibo:Documents a owl:Class ;
    rdf:type owl:Class ;
    rdfs:label "Documents" .

fibo:collectDocuments a owl:ObjectProperty ;
    rdfs:domain fibo:LoanApplication ;
    rdfs:range fibo:Documents ;
    fibo:isCollection true .
```

**Output YAWL**:
```xml
<task id="collectDocuments" name="Collect Documents">
  <multiInstanceLoopCharacteristics isSequential="false">
    <loopCardinality>/documents.length</loopCardinality>
  </multiInstanceLoopCharacteristics>
</task>
```

### Pattern 6: Composite Task

**Description**: Maps nested class hierarchies to composite/sub-workflow tasks.

**Example**:

**Input Ontology**:
```turtle
fibo:LoanApplication a owl:Class .

fibo:ApplicationReview a owl:Class ;
    rdfs:subClassOf fibo:LoanApplication ;
    fibo:isComposite true .
```

**Output YAWL**:
```xml
<task id="applicationReview" name="Application Review">
  <decomposition id="ApplicationReviewNet"/>
</task>

<net id="ApplicationReviewNet" name="Application Review">
  <!-- Nested subnet definition -->
</net>
```

---

## Rule<Q,T> Composition Pattern

ggen-yawl uses a **composable rule pattern** for code generation.

### Generic Signature

```rust
pub trait Rule<Q: Queryable, T: Renderable> {
    fn execute(&self) -> Result<Vec<GeneratedFile>>;
}

impl<Q: Queryable, T: Renderable> Rule<Q, T> {
    fn query(&self) -> &Q { ... }
    fn template(&self) -> &T { ... }
}
```

### Pattern Flow

```
Rule<Q, T>
  ├─ Queryable Q
  │   ├─ Execute SPARQL
  │   ├─ Extract bindings
  │   └─ Return HashMap<String, String>
  │
  ├─ Renderable T
  │   ├─ Receive bindings
  │   ├─ Render with Tera
  │   └─ Return generated code
  │
  └─ execute()
      ├─ q.execute() → bindings
      ├─ t.render(bindings) → code
      └─ return GeneratedFile
```

### Rules 3-10 Implementation

Each rule follows the pattern:

| Rule | Queryable | Renderable | Output |
|------|-----------|-----------|--------|
| 3 | `JpaEntityQuery` | `JpaEntityTemplate` | Java entity class |
| 4 | `RepositoryQuery` | `RepositoryTemplate` | Spring repository interface |
| 5 | `DtoQuery` | `DtoTemplate` | Lombok DTO |
| 6 | `ControllerQuery` | `ControllerTemplate` | REST controller |
| 7 | `EnumQuery` | `EnumTemplate` | Java enum |
| 8 | `ServiceQuery` | `ServiceTemplate` | Spring service |
| 9 | `HbmMappingQuery` | `HbmMappingTemplate` | Hibernate mapping XML |
| 10 | `JacksonSerializerQuery` | `JacksonSerializerTemplate` | Jackson serializer |

### Example: Rule 3 (JPA Entity)

```rust
pub struct JpaEntityRule {
    query: JpaEntityQuery,
    template: JpaEntityTemplate,
}

impl Rule<JpaEntityQuery, JpaEntityTemplate> for JpaEntityRule {
    fn execute(&self) -> Result<Vec<GeneratedFile>> {
        // 1. Execute SPARQL query
        let bindings = self.query.execute()?;

        // 2. For each entity class
        let mut files = Vec::new();
        for binding in bindings {
            // 3. Render entity Java code
            let code = self.template.render(&binding)?;

            // 4. Create GeneratedFile
            let file = GeneratedFile {
                path: format!("entity/{}.java", binding["className"]),
                content: code,
            };
            files.push(file);
        }

        Ok(files)
    }
}
```

---

## Data Flow

### Complete End-to-End Flow

```
User Input
  │
  ├─ Ontology File (TTL/XML)
  ├─ Configuration (--package, --output-dir)
  └─ Flags (--validate, --watch)
  │
  ▼
┌────────────────────────────────────────┐
│ 1. Load Phase                          │
│  - Parse ontology (μ₁)                 │
│  - Validate RDF syntax                 │
│  - Build in-memory graph               │
└────────────────────────────────────────┘
  │
  ▼
┌────────────────────────────────────────┐
│ 2. Transform Phase                     │
│  - Execute 6 CONSTRUCT queries (μ₂)    │
│  - Build YAWL RDF graph                │
│  - Extract patterns                    │
└────────────────────────────────────────┘
  │
  ▼
┌────────────────────────────────────────┐
│ 3. Code Generation Phase               │
│  - Build context (μ₃)                  │
│  - Render YAWL XML template            │
│  - Execute 10 Java rules               │
│  - Generate 8 file types               │
└────────────────────────────────────────┘
  │
  ▼
┌────────────────────────────────────────┐
│ 4. Canonicalization Phase              │
│  - Normalize formatting (μ₄)           │
│  - Calculate hashes                    │
│  - Verify reproducibility              │
└────────────────────────────────────────┘
  │
  ▼
┌────────────────────────────────────────┐
│ 5. Validation & Receipt Phase          │
│  - Validate syntax (μ₅)                │
│  - Generate receipt.json               │
│  - Create audit trail                  │
└────────────────────────────────────────┘
  │
  ▼
Generated Artifacts
  ├─ .ggen/yawl/workflow.yawl.xml
  ├─ src/main/java/com/example/yawl/
  │   ├─ entity/*.java
  │   ├─ repository/*.java
  │   ├─ dto/*.java
  │   ├─ controller/*.java
  │   ├─ service/*.java
  │   ├─ enums/*.java
  │   ├─ hbm/*.xml
  │   └─ serializer/*.java
  └─ receipt.json
```

### Context Data Structure

The TemplateContext carries data through stages μ₃-μ₅:

```rust
pub struct TemplateContext {
    pub workflow_name: String,
    pub description: String,
    pub version: String,
    pub tasks: Vec<TaskContext>,
    pub flows: Vec<FlowContext>,
    pub input_condition: Option<ConditionContext>,
    pub output_condition: Option<ConditionContext>,
    pub variables: Vec<VariableContext>,
}
```

---

## Design Decisions

### Decision 1: Why Five Stages?

**Problem**: Transformation is complex and error-prone.

**Solution**: Five-stage pipeline separates concerns:

| Stage | Concern | Benefit |
|-------|---------|---------|
| μ₁ | Validation | Catch errors early |
| μ₂ | Pattern extraction | Reusable transformations |
| μ₃ | Code generation | Template-driven flexibility |
| μ₄ | Determinism | Reproducible builds |
| μ₅ | Auditability | Cryptographic proof |

### Decision 2: SPARQL CONSTRUCT for Extraction

**Alternative Considered**: Direct graph traversal in Rust.

**Why SPARQL CONSTRUCT**:
- Declarative (easier to understand transformations)
- Testable independently
- Reusable across projects
- Supports inference rules
- Industry standard

### Decision 3: Tera Templates for Code Gen

**Alternative Considered**: Procedural code generation.

**Why Tera**:
- Separates logic from presentation
- Non-developers can customize templates
- Handles complex control flow (loops, conditionals)
- Built-in filters and escaping
- Mature and well-documented

### Decision 4: Rule<Q,T> Composition

**Alternative Considered**: Single monolithic generator.

**Why Composition**:
- Each rule is independently testable
- Rules can be composed into workflows
- Easy to add new rules (Rule 11+)
- Parallel rule execution possible
- Clear separation of concerns

### Decision 5: Content-Based Hashing

**Alternative Considered**: Timestamp-based versioning.

**Why Content Hashing**:
- Detects actual changes (not just time)
- Enables incremental updates
- Supports content-addressed storage
- Deterministic and reproducible
- Works in version control (same hash = same content)

---

## Module Architecture

### Crate Organization

```
ggen-yawl/
├── src/
│   ├── lib.rs                 # Root: YawlGenerator
│   ├── error.rs               # Error types
│   ├── ontology/
│   │   ├── loader.rs          # OntologyLoader (μ₁)
│   │   └── mod.rs             # Module re-exports
│   ├── codegen/
│   │   ├── yawl_xml.rs        # YAWL XML rendering
│   │   ├── java_rules.rs      # Java code rules (μ₃)
│   │   └── rules/
│   │       ├── jpa_entity.rs  # Rule 3
│   │       ├── repositories.rs # Rule 4
│   │       ├── dtos.rs         # Rule 5
│   │       ├── controllers.rs  # Rule 6
│   │       ├── enums.rs        # Rule 7
│   │       ├── services.rs     # Rule 8
│   │       ├── hbm_mappings.rs # Rule 9
│   │       └── jackson_serializers.rs # Rule 10
│   ├── transform/
│   │   ├── executor.rs        # ConstructExecutor (μ₂)
│   │   └── mod.rs             # Module
│   ├── template/
│   │   ├── context.rs         # TemplateContext
│   │   ├── renderer.rs        # TemplateRenderer (μ₃)
│   │   └── mod.rs             # Module
│   └── a2a/                   # Agent-to-Agent utilities
│       ├── converter.rs
│       ├── state.rs
│       └── mod.rs
├── templates/
│   ├── workflow.yawl.tera
│   ├── entity.java.tera
│   ├── repository.java.tera
│   ├── dto.java.tera
│   ├── controller.java.tera
│   ├── service.java.tera
│   ├── enum.java.tera
│   ├── hbm_mapping.xml.tera
│   └── serializer.java.tera
├── queries/                   # SPARQL queries (μ₂)
│   ├── 01-extract-tasks.rq
│   ├── 02-extract-flows.rq
│   ├── 03-cardinality-splitjoin.rq
│   ├── 04-rules-to-conditions.rq
│   ├── 05-multiple-instance.rq
│   └── 06-composite-task.rq
└── tests/
    ├── integration_generated_java_test.rs
    ├── ontology_real_data_test.rs
    ├── phase4_e2e_validation.rs
    └── ...
```

### Dependency Graph

```
YawlGenerator (lib.rs)
  │
  ├─ OntologyLoader (ontology/) → μ₁
  │   └─ Oxigraph (RDF parsing)
  │
  ├─ ConstructExecutor (transform/) → μ₂
  │   └─ Oxigraph (SPARQL)
  │
  ├─ TemplateRenderer (template/) → μ₃
  │   └─ Tera (template rendering)
  │
  ├─ YawlXmlGenerator (codegen/yawl_xml.rs) → μ₃+μ₄
  │   └─ Serde (XML serialization)
  │
  ├─ Rules 3-10 (codegen/rules/) → μ₃
  │   └─ Tera + Serde
  │
  └─ Receipt Generator (implicit in main flow) → μ₅
      └─ SHA-256 hashing
```

### Key Type Definitions

```rust
// From ggen-codegen (generic framework)
pub trait Queryable {
    fn execute(&self) -> Result<Vec<HashMap<String, String>>>;
}

pub trait Renderable {
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String>;
}

pub struct GeneratedFile {
    pub path: PathBuf,
    pub content: String,
}

// From ggen-yawl (domain-specific)
pub struct YawlGenerator {
    executor: ConstructExecutor,
    renderer: TemplateRenderer,
    validate_output: bool,
}

pub struct TemplateContext {
    pub workflow_name: String,
    pub tasks: Vec<TaskContext>,
    pub flows: Vec<FlowContext>,
    // ... more fields
}
```

---

## Performance Characteristics

### Stage Timings (SLO Targets)

| Stage | Target | Notes |
|-------|--------|-------|
| μ₁ Normalize | <5s | Includes parsing and validation |
| μ₂ Extract | <2s | All 6 CONSTRUCT queries |
| μ₃ Emit | <1s | Template rendering |
| μ₄ Canonicalize | <500ms | Formatting and hashing |
| μ₅ Receipt | <50ms | Audit trail generation |
| **Total** | **<10s** | End-to-end generation |

### Scaling Characteristics

- **Linear**: Ontology size (N triples) → O(N) time
- **Linear**: Number of entities → O(N) files generated
- **Constant**: Number of stages (always 5)
- **Constant**: Number of rules (currently 10, can extend to N)

### Memory Usage

- **Ontology**: ~100KB per 1,000 triples (in-memory RDF graph)
- **Generated code**: ~50KB per entity (9 files per entity)
- **Total**: <100MB for typical projects

---

**For Implementation Details**, see [YAWL_CODEGEN_API.md](./YAWL_CODEGEN_API.md).

**For Rule Specifications**, see `docs/rules/` directory.
