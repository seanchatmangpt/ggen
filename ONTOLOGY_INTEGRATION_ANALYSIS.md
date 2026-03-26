# YAWL Ontology Integration Analysis

**Date:** 2026-03-26
**Status:** Discovery & Planning Phase
**Target:** Replace mock data with real YAWL ontology in Rules 3-8

---

## Executive Summary

Found **6 YAWL ontology files** (5,199 lines total) in `/Users/sac/yawlv6/.claude/ggen/`. Current code generation rules (3-8) use hardcoded mock data. This analysis documents the real ontology structure and provides an implementation roadmap to integrate actual SPARQL queries.

---

## Ontology Files Discovered

| File | Format | Size | Domain Coverage | Status |
|------|--------|------|-----------------|--------|
| `yawl-domain.ttl` | Turtle | 3,256 lines | **63 HBM entities, fields, relationships** | **PRIMARY** |
| `yawl-workflow.ttl` | Turtle | 291 lines | **Core YAWL abstractions** (Spec, Net, Task, Condition, Flow) | Primary |
| `yawl-code.ttl` | Turtle | 637 lines | **31 Java packages + 90+ classes** | Secondary |
| `yawl-patterns.ttl` | Turtle | 456 lines | **43 WCP patterns** (joins, splits) | Secondary |
| `yawl-modules.ttl` | Turtle | 328 lines | **Maven modules, services, build config** | Secondary |
| `primitives.ttl` | Turtle | 231 lines | Claude Code framework (not YAWL-specific) | Reference |

**Location:** `/Users/sac/yawlv6/.claude/ggen/`

---

## Detailed Ontology Analysis

### 1. yawl-domain.ttl (PRIMARY)

**Purpose:** Auto-generated from 63 Hibernate HBM XML files via `hbm_to_ttl.py`

**Structure:**
```
- yawl:Entity (root class for persistent entities)
  - Properties: className, tableName, packageName, sourceFile
  - Relations: hasIdField, hasField, hasManyToOne, hasOneToOne, hasSet, hasMap

- yawl:Field (entity fields)
  - Properties: fieldName, columnName, fieldType, isPrimaryKey, generatorClass

- yawl:ManyToOneRel, yawl:OneToOneRel (relationships)

- yawl:SetCollection, yawl:MapCollection (complex types)

- yawl:ComponentMapping (JPA component/embeddable)

- hbm:* namespace (Hibernate-specific: cascade, lazy, batchSize, notNull, etc.)
```

**Real Data Examples:**
- `yawl:YExternalClient` - User authentication entity (63 lines)
- `yawl:CostDriver` - Cost tracking with collections (100+ lines)
- **63 total entities** (YTask, YCondition, YFlow, YWorkItem, YCase, etc.)

**Rules Affected:**
- Rule 3: Entity → JPA @Entity + annotations
- Rule 4: Entity → Spring Repository interfaces
- Rule 5: Entity → DTOs (Cardinality split/join patterns)

**SPARQL Query Template:**
```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?entity ?className ?tableName ?package WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:tableName ?tableName ;
          yawl:packageName ?package .
  FILTER (REGEX(?className, "Task|Condition|Work"))
}
LIMIT 20
```

---

### 2. yawl-workflow.ttl (PRIMARY)

**Purpose:** Core YAWL workflow domain ontology

**Classes Defined:**
- `yawl:YSpecification` - Workflow spec container (versions 2.0-2.2.x)
- `yawl:Net` - Workflow net (Petri net with YAWL semantics)
- `yawl:Task`, `yawl:AtomicTask`, `yawl:CompositeTask`
- `yawl:MultiInstanceTask` (with thresholds, join/AND/OR)
- `yawl:Condition`, `yawl:InputCondition`, `yawl:OutputCondition`, `yawl:InternalCondition`
- `yawl:Flow` (directed edges with optional predicates)
- `yawl:Decomposition`, `yawl:NetDecomposition`, `yawl:ExternalServiceDecomposition`

**Key Properties:**
- `yawl:implementedBy` - Maps to Java class
- `yawl:relatesTo` - Cross-reference other elements
- `rdfs:comment` - Semantic documentation

**Rules Affected:**
- Rule 6: Rules → Condition decomposition
- Rule 8: Composite Task patterns

**SPARQL Query Template:**
```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?task ?label ?type WHERE {
  ?task a ?type ;
        rdfs:label ?label .
  ?type rdfs:subClassOf yawl:Task .
}
```

---

### 3. yawl-code.ttl (SECONDARY)

**Purpose:** Java package/class structure (31 packages, 90+ classes)

**Package Hierarchy:**
- `org.yawlfoundation.yawl.elements` - **Domain model (primary)**
  - `.elements.state` - YIdentifier, YMarking, YSetOfMarkings
  - `.elements.data` - YParameter, YVariable, YAttributeMap
  - `.elements.pattern` - **Sealed hierarchy** (Java 26): WorkflowPattern, JoinPattern, SplitPattern
  - `.elements.predicate` - PredicateEvaluator, PredicateEvaluatorFactory
  - `.elements.milestone` - MilestoneEvaluator
  - `.elements.e2wfoj` - E2WFOJNet (reduction graph analysis)

- `org.yawlfoundation.yawl.engine` - Runtime engine
- `org.yawlfoundation.yawl.runtime` - Work items, identifiers
- `org.yawlfoundation.yawl.authentication` - YExternalClient
- `org.yawlfoundation.yawl.persistence` - Hibernate integration
- ...24 more packages

**Classes:**
- `yawl-code:JavaPackage` - Maven module/package
- `yawl-code:JavaClass` - Standard class
- `yawl-code:JavaInterface`, `yawl-code:SealedInterface` - Interface types

**Rules Affected:**
- Rule 3: Class structure validation
- Rule 8: Service/controller package mapping

---

### 4. yawl-patterns.ttl (SECONDARY)

**Purpose:** 43 WCP (Workflow Control Patterns) implementation status

**Structure:**
- `yawl-wcp:WorkflowControlPattern` - Base class
  - `yawl-wcp:JoinPattern` - Input synchronization
  - `yawl-wcp:SplitPattern` - Output branching

**All 43 WCP Patterns Defined:**
- WCP-1: Sequence
- WCP-2: Parallel Split (AND)
- WCP-3: Synchronization (AND join)
- WCP-4: Exclusive Choice (XOR split)
- WCP-5: Simple Merge (XOR join)
- WCP-6: Multi-Choice (OR split)
- WCP-7: Synchronizing Merge (Discriminator join)
- ...WCP-43 (cancellation, recursion, etc.)

**Properties per pattern:**
- `yawl-wcp:patternId` - Numeric ID (1-43)
- `yawl-wcp:category` - Classification (Sequence, Parallelism, Synchronization, Choice, etc.)
- `yawl-wcp:implementationStatus` - "Implemented" | "Partially" | "Not Implemented"
- `yawl-wcp:implementedBy` - Java class (e.g., `yawl:AndSplit`, `yawl:XorJoin`)

**Rules Affected:**
- Rule 7: Multiple Instance patterns (enum generation)

---

### 5. yawl-modules.ttl (SECONDARY)

**Purpose:** Maven multi-module project structure

**Modules:**
```
yawl-parent (v2026.1.0, Java 26, GraalVM)
├── yawl-core (JAR: engine + domain, 1949+ tests, Hibernate 7 + JPA 3.2)
│   └── generators (code gen pipeline)
├── yawl-webapp (WAR: control panel, JSP)
└── services (9 external services)
    ├── Resource Service
    ├── Worklet Service
    ├── Mail Service
    └── ... (6 more)
```

**Properties per module:**
- `yawl-mod:artifactId`
- `yawl-mod:version`
- `yawl-mod:javaVersion`
- `yawl-mod:buildTool` (mvnd)
- `yawl-mod:buildFlags` (compiler.fork, skips)
- `yawl-mod:persistenceFramework`
- `yawl-mod:database`
- `yawl-mod:testCount`

**Rules Affected:**
- Rule 3-8: Module/package context
- Build configuration inference

---

## Current Mock Data Issues

### Location: Rules 3-8 Implementation

| Rule | Current Issue | Expected Real Data |
|------|---------------|-------------------|
| **3** | Hardcoded `"entity_0"`, `"Entity 0"` task context | 63 real entities from yawl-domain.ttl |
| **4** | Synthetic flows: `"start"`, `"repo_0"`, `"end"` | Real task relationships, foreign keys |
| **5** | Mock DTO generation: `"dto_0"`, `"dto_1"` | Entity cardinality (1:1, 1:N, M:N), split/join |
| **6** | Hardcoded conditions: `"cond_0"`, `"status=='active_0'"` | Workflow rules, predicates from ontology |
| **7** | Enum values: `"VALUE_1"`, `"VALUE_2"` (arbitrary) | Real WCP patterns (AndSplit, XorJoin, etc.) |
| **8** | Synthetic services (fixed pattern) | Real Spring Service patterns, @Transactional |

**Benchmark file:** `/Users/sac/ggen/.claude/worktrees/yawl-codegen/crates/ggen-yawl/benches/yawl_rules_bench.rs`

**Test file:** `/Users/sac/ggen/.claude/worktrees/yawl-codegen/crates/ggen-yawl/tests/integration_generated_java_test.rs`

---

## Implementation Roadmap

### Phase 1: Ontology Loader Integration ✓ (ALREADY EXISTS)

**Status:** `OntologyLoader` exists in `crates/ggen-yawl/src/ontology/loader.rs`

Features:
- Multi-format support (Turtle, RDF/XML, N-Triples, N-Quads, TriG)
- Automatic format detection from file extension
- Load from file or string
- Returns `ggen_core::Graph` (Oxigraph store)

**Code:**
```rust
pub struct OntologyLoader;
pub enum OntologyFormat {
    Turtle, RdfXml, NTriples, NQuads, Trig
}
impl OntologyLoader {
    pub fn load_from_file(path: &Path) -> Result<Graph>
    pub fn load_from_str(content: &str, format: OntologyFormat) -> Result<Graph>
}
```

### Phase 2: SPARQL Integration (TO DO)

**Goal:** Execute real SPARQL queries against loaded ontology

**Steps:**
1. Load all 6 TTL files into a single Graph (or separate graphs per domain)
2. For each Rule (3-8):
   - Define SPARQL SELECT/ASK query
   - Execute query using `Graph::query()`
   - Map results to rule-specific context structures
   - Verify query returns real data

**Example Query (Rule 3 - Entities):**
```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?entity ?className ?tableName ?package WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:tableName ?tableName ;
          yawl:packageName ?package .
}
LIMIT 20
```

### Phase 3: Rule Implementations Update (TO DO)

**Affected files:**
- `crates/ggen-yawl/benches/yawl_rules_bench.rs` - Replace mock generators
- `crates/ggen-yawl/tests/integration_generated_java_test.rs` - Update test fixtures
- `crates/ggen-yawl/src/codegen/rules/hbm_mappings.rs` - Integrate ontology queries
- `crates/ggen-yawl/src/codegen/rules/jackson_serializers.rs` - Real enum generation

### Phase 4: Validation Testing (TO DO)

**Verification:**
- [ ] All 6 TTL files load without errors
- [ ] SPARQL queries execute and return real data
- [ ] Generated code compiles (javac validation)
- [ ] Rule output is deterministic (reproducible)
- [ ] No mock data in final output

---

## SPARQL Query Template Library

### Rule 3: Entities → JPA @Entity

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
SELECT ?entity ?className ?tableName ?package ?hasFields WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:tableName ?tableName ;
          yawl:packageName ?package .
  OPTIONAL { ?entity yawl:hasField ?field }
  BIND(BOUND(?field) AS ?hasFields)
}
LIMIT 63
```

### Rule 4: Entities → Spring Repository

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
SELECT ?entity ?className ?idField ?keyType WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:hasIdField ?idField .
  ?idField yawl:fieldType ?keyType .
}
```

### Rule 5: Entities → DTOs (Cardinality)

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
SELECT ?entity ?className
       (COUNT(DISTINCT ?m2o) AS ?manyToOneCount)
       (COUNT(DISTINCT ?o2o) AS ?oneToOneCount)
       (COUNT(DISTINCT ?set) AS ?setCount) WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className .
  OPTIONAL { ?entity yawl:hasManyToOne ?m2o }
  OPTIONAL { ?entity yawl:hasOneToOne ?o2o }
  OPTIONAL { ?entity yawl:hasSet ?set }
}
GROUP BY ?entity ?className
```

### Rule 6: Rules → Conditions

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
SELECT ?condition ?label ?type WHERE {
  ?condition a ?type ;
             rdfs:label ?label .
  ?type rdfs:subClassOf yawl:Condition .
}
```

### Rule 7: WCP Patterns → Enums

```sparql
PREFIX yawl-wcp: <https://yawlfoundation.org/patterns#>
SELECT ?pattern ?label ?patternId ?implementedBy WHERE {
  ?pattern a yawl-wcp:WorkflowControlPattern ;
           rdfs:label ?label ;
           yawl-wcp:patternId ?patternId ;
           yawl-wcp:implementedBy ?implementedBy .
}
ORDER BY ?patternId
```

### Rule 8: Composite Tasks → Services

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
SELECT ?task ?taskLabel ?decomposition WHERE {
  ?task a yawl:CompositeTask ;
        rdfs:label ?taskLabel ;
        yawl:isDecomposedBy ?decomposition .
  ?decomposition a yawl:NetDecomposition .
}
```

---

## Expected Benefits

1. **Real Data Integration:** 63 actual entities instead of 8 hardcoded mock entities
2. **Reproducible Generation:** SPARQL queries produce deterministic output
3. **Cross-Rule Consistency:** Entities in Rule 3 drive consistent Repository names in Rule 4
4. **Validation:** Generated code will match actual YAWL class structure
5. **Maintainability:** TTL is source of truth; updates propagate automatically

---

## Files to Modify

### 1. benches/yawl_rules_bench.rs
- Replace `generate_rule3_entities()` → Load from yawl-domain.ttl
- Replace `generate_rule4_repositories()` → Query entity relationships
- Replace `generate_rule5_dtos()` → Query cardinality from ontology
- Replace `generate_rule6_controllers()` → Query condition types
- Replace `generate_rule7_enums()` → Query WCP patterns from yawl-patterns.ttl
- Replace `generate_rule8_services()` → Query task decompositions

### 2. tests/integration_generated_java_test.rs
- Load real ontology files
- Verify generated code uses real entity/pattern names
- Validate Java syntax against real structure

### 3. src/codegen/rules/hbm_mappings.rs
- Update `HbmMappingQuery` to use real SPARQL results
- Verify foreign key mappings from ontology

### 4. src/codegen/rules/jackson_serializers.rs
- Update enum generation to use real WCP patterns
- Ensure all 43 patterns are represented (if applicable)

---

## Dependencies

Already available:
- `oxigraph::store::Store` - SPARQL endpoint
- `oxigraph::sparql::QueryResults` - Query execution
- `ggen_core::Graph` - RDF graph wrapper

No additional dependencies needed.

---

## Next Steps

1. **Create test module** that loads all 6 TTL files
2. **Implement SPARQL query builders** for each Rule
3. **Execute queries** and validate result cardinality
4. **Update benchmark generators** to use real data
5. **Run integration tests** with real ontology
6. **Verify generated Java code** compiles and validates

---

**Prepared by:** Claude Code Analysis
**Ready for Implementation:** YES
**Risk Level:** LOW (non-breaking, additive)
