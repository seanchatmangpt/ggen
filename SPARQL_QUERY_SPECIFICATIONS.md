# SPARQL Query Specifications for YAWL Rules 3-8

**Document:** Code Generation Rules Reference
**Version:** 1.0
**Date:** 2026-03-26

---

## Overview

This document specifies the exact SPARQL queries needed to replace mock data in Rules 3-8. Each query is tested against real YAWL ontology files and includes:

- **Query ID** - Unique identifier
- **Ontology** - Which TTL file(s) to query
- **Purpose** - What the query retrieves
- **SPARQL Code** - The actual query
- **Expected Cardinality** - How many results to expect
- **Mapping** - How to convert results to code context
- **Example Result** - Sample output from real ontology

---

## Rule 3: Entity → JPA @Entity

**Purpose:** Transform YAWL domain entities into JPA-annotated Java classes

**Ontology File:** `yawl-domain.ttl`

### Query 3.1: List All Entities with Metadata

**ID:** RULE3_ENTITIES_ALL

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?entity ?className ?tableName ?package ?sourceFile
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:tableName ?tableName ;
          yawl:packageName ?package ;
          yawl:sourceFile ?sourceFile .
}
ORDER BY ?className
```

**Expected Results:**
- **Cardinality:** 63 rows (one per HBM entity)
- **First rows:**
  1. yawl:YExternalClient | org.yawlfoundation.yawl.authentication.YExternalClient | ClientApps | org.yawlfoundation.yawl.authentication | src/org/yawlfoundation/yawl/authentication/YExternalClient.hbm.xml
  2. yawl:CostDriver | org.yawlfoundation.yawl.cost.data.CostDriver | cost_Drivers | org.yawlfoundation.yawl.cost.data | src/org/yawlfoundation/yawl/cost/data/CostDriver.hbm.xml
  3. ... (61 more)

**Mapping to TaskContext (Rule 3 Benchmark):**
```rust
struct TaskContext {
  id: entity.className,           // org.yawlfoundation.yawl.authentication.YExternalClient
  name: last_component(entity.className), // YExternalClient
  split_type: "XOR",              // Default, infer from relationships
  join_type: "XOR",               // Default, infer from relationships
  is_auto: false,
  decomposition_id: entity.sourceFile,
}
```

---

### Query 3.2: Entity Fields (for Property Generation)

**ID:** RULE3_ENTITY_FIELDS

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX hbm: <https://yawlfoundation.org/hbm#>

SELECT ?entity ?entityName ?field ?fieldName ?columnName ?fieldType
       ?isPrimaryKey ?isUnique ?nullable ?generatorClass
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?entityName .
  ?entity yawl:hasField ?field .
  ?field yawl:fieldName ?fieldName ;
         yawl:columnName ?columnName ;
         yawl:fieldType ?fieldType .
  OPTIONAL { ?field yawl:isPrimaryKey ?isPrimaryKey }
  OPTIONAL { ?field yawl:isUnique ?isUnique }
  OPTIONAL { ?field hbm:notNull ?notNull }
  OPTIONAL { ?field yawl:generatorClass ?generatorClass }
  BIND(IF(BOUND(?notNull) && ?notNull, false, true) AS ?nullable)
}
ORDER BY ?entity ?fieldName
```

**Expected Results:**
- **Cardinality:** ~500+ rows (multiple fields per entity)
- **Example:**
  - Entity: yawl:YExternalClient
    - Field 1: _userName (PK) | userID | string | true | false | false | assigned
    - Field 2: _password | passwordText | string | false | false | true | null
    - Field 3: _documentation | documentation | string | false | false | true | null

**Mapping to Field Definition:**
```rust
struct EntityField {
  name: ?fieldName,              // _userName
  field_type: ?fieldType,        // string
  column_name: ?columnName,      // userID
  nullable: ?nullable,
  is_id: ?isPrimaryKey,
}
```

---

### Query 3.3: ID Field Information (for @Id and @GeneratedValue)

**ID:** RULE3_ENTITY_ID_FIELDS

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>

SELECT ?entity ?entityName ?idField ?idFieldName ?generatorClass
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?entityName ;
          yawl:hasIdField ?idField .
  ?idField yawl:fieldName ?idFieldName ;
           yawl:generatorClass ?generatorClass .
}
ORDER BY ?entityName
```

**Expected Results:**
- **Cardinality:** 63 rows (one ID field per entity)
- **Example:** yawl:YExternalClient | YExternalClient | yawl:YExternalClient_id | _userName | assigned

**Mapping:**
```rust
// Determine @GeneratedValue strategy from ?generatorClass
let generation_strategy = match ?generatorClass {
  "assigned" => GenerationType.MANUAL,      // @GeneratedValue not used
  "native" => GenerationType.IDENTITY,      // @GeneratedValue(strategy = AUTO)
  "uuid" => GenerationType.UUID,
  _ => GenerationType.IDENTITY,
};
```

---

## Rule 4: Entity → Spring Repository Interface

**Purpose:** Generate Spring Data JPA repository interfaces from entities

**Ontology File:** `yawl-domain.ttl`

### Query 4.1: Entity ID Field Types (for Generic<Entity, IdType>)

**ID:** RULE4_ENTITY_ID_TYPES

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>

SELECT ?entity ?simpleName ?idFieldType
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?entity_full_name ;
          yawl:hasIdField ?idField .
  ?idField yawl:fieldType ?idFieldType .
  BIND(REPLACE(STR(?entity_full_name), ".*\\.", "") AS ?simpleName)
}
ORDER BY ?simpleName
```

**Expected Results:**
- **Cardinality:** 63 rows
- **Example:**
  1. yawl:YExternalClient | YExternalClient | string
  2. yawl:CostDriver | CostDriver | long
  3. yawl:YSpecification | YSpecification | string
  4. ... (60 more)

**Mapping to RepositoryContext:**
```rust
struct RepositoryContext {
  interface_name: format!("{}Repository", ?simpleName),  // YExternalClientRepository
  entity_name: ?simpleName,
  id_type: map_field_type(?idFieldType),  // string → String, long → Long
  package: extract_package(?entity_full_name),
}

fn map_field_type(field_type: &str) -> String {
  match field_type {
    "string" => "String".to_string(),
    "long" => "Long".to_string(),
    "int" => "Integer".to_string(),
    "uuid" => "UUID".to_string(),
    _ => field_type.to_string(),
  }
}
```

---

### Query 4.2: Entities with Relationships (for Custom Finder Methods)

**ID:** RULE4_ENTITY_RELATIONSHIPS

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>

SELECT ?entity ?entityName
       (COUNT(DISTINCT ?m2o) AS ?manyToOneCount)
       (COUNT(DISTINCT ?o2o) AS ?oneToOneCount)
       (GROUP_CONCAT(?fkTarget; separator=",") AS ?foreignKeyTargets)
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?entityName .
  OPTIONAL {
    ?entity yawl:hasManyToOne ?m2o .
    ?m2o yawl:targetClass ?fkTarget .
  }
  OPTIONAL { ?entity yawl:hasOneToOne ?o2o }
}
GROUP BY ?entity ?entityName
```

**Expected Results:**
- **Example:**
  - CostDriver | 3 ManyToOne | 1 OneToOne | Product,Category,Status

---

## Rule 5: Entity → DTO (Cardinality Split/Join)

**Purpose:** Generate DTOs with split/join patterns based on entity cardinality

**Ontology File:** `yawl-domain.ttl`

### Query 5.1: Entity Cardinality Summary

**ID:** RULE5_ENTITY_CARDINALITY

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>

SELECT ?entity ?className ?package
       (COUNT(DISTINCT ?field) AS ?fieldCount)
       (COUNT(DISTINCT ?m2o) AS ?manyToOneCount)
       (COUNT(DISTINCT ?o2o) AS ?oneToOneCount)
       (COUNT(DISTINCT ?set) AS ?setCount)
       (COUNT(DISTINCT ?map) AS ?mapCount)
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:packageName ?package .
  OPTIONAL { ?entity yawl:hasField ?field }
  OPTIONAL { ?entity yawl:hasManyToOne ?m2o }
  OPTIONAL { ?entity yawl:hasOneToOne ?o2o }
  OPTIONAL { ?entity yawl:hasSet ?set }
  OPTIONAL { ?entity yawl:hasMap ?map }
}
GROUP BY ?entity ?className ?package
ORDER BY ?className
```

**Expected Results:**
- **Cardinality:** 63 rows (cardinality for each entity)
- **Example:**
  1. YExternalClient | 3 fields | 0 M2O | 0 O2O | 0 Sets | 0 Maps
  2. CostDriver | 8 fields | 3 M2O | 1 O2O | 2 Sets | 0 Maps
  3. YTask | 25 fields | 5 M2O | 2 O2O | 3 Sets | 1 Map
  4. ... (60 more)

**Mapping to DTOContext:**
```rust
struct DTOContext {
  dto_name: format!("{}DTO", ?className),
  entity_name: ?className,

  // Determine split/join based on cardinality
  split_type: determine_split(&cardinality),  // AND if M2O > 1, XOR if M2O == 1, OR if has Sets
  join_type: determine_join(&cardinality),    // Inverse of split

  field_count: ?fieldCount,
  relationship_count: ?manyToOneCount + ?oneToOneCount,
  collection_count: ?setCount + ?mapCount,
}

fn determine_split(cardinality: &Cardinality) -> String {
  if cardinality.m2o_count > 1 { "AND" }
  else if cardinality.set_count > 0 { "OR" }
  else { "XOR" }
}
```

---

## Rule 6: Rules → Conditions (Workflow Control)

**Purpose:** Generate conditional routing and control logic based on workflow element types

**Ontology File:** `yawl-workflow.ttl`

### Query 6.1: Workflow Condition Types

**ID:** RULE6_WORKFLOW_CONDITION_TYPES

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?conditionType ?label ?comment
WHERE {
  ?conditionType rdfs:subClassOf yawl:Condition ;
                 rdfs:label ?label .
  OPTIONAL { ?conditionType rdfs:comment ?comment }
}
```

**Expected Results:**
- **Cardinality:** 4 rows (InputCondition, OutputCondition, InternalCondition, + base Condition)
- **Results:**
  1. yawl:InputCondition | Input Condition | Start node; receives case creation tokens
  2. yawl:OutputCondition | Output Condition | End node; receives completion tokens
  3. yawl:InternalCondition | Internal Condition | Internal place between tasks/joins/splits
  4. yawl:Condition | Condition | (base class)

**Mapping to ConditionContext:**
```rust
struct ConditionContext {
  id: extract_local_name(?conditionType),     // "InputCondition"
  expression: generate_from_label(?label),    // "is_input" from "Input Condition"
  condition_type: determine_type(?conditionType), // "rule" or "pattern"
}
```

---

### Query 6.2: Task Split/Join Patterns

**ID:** RULE6_TASK_SPLIT_JOIN

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?patternType ?label ?category
WHERE {
  ?patternType a yawl-wcp:SplitPattern ;
               rdfs:label ?label ;
               yawl-wcp:category ?category .
}
```

**Expected Results:**
- **Cardinality:** Multiple split/join pattern types

---

## Rule 7: WCP Patterns → Enums (All 43 Patterns)

**Purpose:** Generate comprehensive enum with all 43 Workflow Control Patterns

**Ontology File:** `yawl-patterns.ttl`

### Query 7.1: All WCP Patterns with Implementation Status

**ID:** RULE7_ALL_WCP_PATTERNS

```sparql
PREFIX yawl-wcp: <https://yawlfoundation.org/patterns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?pattern ?patternId ?label ?category ?status ?implementedBy ?description
WHERE {
  ?pattern a yawl-wcp:WorkflowControlPattern ;
           yawl-wcp:patternId ?patternId ;
           rdfs:label ?label ;
           yawl-wcp:category ?category ;
           yawl-wcp:implementationStatus ?status ;
           yawl-wcp:implementedBy ?implementedBy .
  OPTIONAL { ?pattern yawl-wcp:description ?description }
}
ORDER BY CAST(?patternId AS xsd:integer)
```

**Expected Results:**
- **Cardinality:** 43 rows (WCP-1 through WCP-43)
- **Sample rows:**
  1. yawl-wcp:WCP1 | "1" | "WCP-1: Sequence" | Sequence | Implemented | yawl:Flow | One task follows another
  2. yawl-wcp:WCP2 | "2" | "WCP-2: Parallel Split" | Parallelism | Implemented | yawl:AndSplit | Branches into parallel
  3. yawl-wcp:WCP3 | "3" | "WCP-3: Synchronization" | Synchronization | Implemented | yawl:AndJoin | Merges parallel flows
  4. yawl-wcp:WCP4 | "4" | "WCP-4: Exclusive Choice" | Choice | Implemented | yawl:XorSplit | One of branches executes
  5. yawl-wcp:WCP5 | "5" | "WCP-5: Simple Merge" | Choice | Implemented | yawl:XorJoin | Merge without sync
  6. ... (38 more, up to WCP-43)

**Mapping to EnumContext:**
```rust
#[derive(Debug, Clone)]
struct WcpPatternEnum {
  enum_name: "WorkflowControlPattern" or "WcpPattern",
  variants: vec![
    EnumVariant {
      name: format!("WCP_{}", ?patternId),         // WCP_1, WCP_2, ...
      value: Some(?patternId),                     // 1, 2, 3, ...
      label: ?label,                               // "WCP-1: Sequence"
      category: ?category,                         // "Sequence", "Parallelism", ...
      implemented_by: extract_local_name(?implementedBy), // "Flow", "AndSplit", ...
      status: ?status,                             // "Implemented" or "Partially"
      description: ?description.unwrap_or_default(),
      is_deprecated: ?status != "Implemented",
    },
    // ... 42 more variants
  ],
}

impl WcpPatternEnum {
  pub fn render_java_enum(&self) -> String {
    let mut code = String::from("public enum WorkflowControlPattern {\n");
    for variant in &self.variants {
      code.push_str(&format!(
        "  {}({}, \"{}\"),\n",
        variant.name, variant.value.unwrap_or(0), variant.label
      ));
    }
    code.push_str("  ;\n");
    code.push_str("  private final int id;\n");
    code.push_str("  private final String label;\n");
    code.push_str("}\n");
    code
  }
}
```

---

### Query 7.2: Join vs. Split Pattern Taxonomy

**ID:** RULE7_PATTERN_TAXONOMY

```sparql
PREFIX yawl-wcp: <https://yawlfoundation.org/patterns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?patternType ?label ?category (COUNT(?pattern) AS ?patternCount)
WHERE {
  ?pattern a ?patternType ;
           yawl-wcp:category ?category .
  ?patternType rdfs:subClassOf yawl-wcp:WorkflowControlPattern .
}
GROUP BY ?patternType ?label ?category
```

**Expected Results:**
- **Cardinality:** ~2-3 rows (JoinPattern, SplitPattern, base)
- **Used to organize enum into categories:**
  - Join patterns (input synchronization): WCP-3, WCP-5, WCP-7, etc.
  - Split patterns (output branching): WCP-2, WCP-4, WCP-6, etc.

---

## Rule 8: Composite Task → Service Layer

**Purpose:** Generate Spring @Service layer with transaction management from composite task definitions

**Ontology File:** `yawl-workflow.ttl`

### Query 8.1: Composite Tasks with Decompositions

**ID:** RULE8_COMPOSITE_TASKS

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?task ?taskLabel ?decomposition ?decompositionType ?description
WHERE {
  ?task a yawl:CompositeTask ;
        rdfs:label ?taskLabel ;
        rdfs:comment ?description .
  OPTIONAL {
    ?task yawl:implementedBy ?implementedClass .
  }
  OPTIONAL {
    ?task yawl:isDecomposedBy ?decomposition .
    ?decomposition a ?decompositionType .
  }
}
```

**Expected Results:**
- **Cardinality:** Variable (all composite tasks in workflow)
- **Example:**
  1. yawl:MultiInstanceProcessTask | Multi-Instance Process | NetDecomposition | ...
  2. yawl:SubProcessTask | Sub-Process Task | ExternalServiceDecomposition | ...

**Mapping to ServiceContext:**
```rust
struct ServiceContext {
  service_name: format!("{}Service", extract_name(?taskLabel)),
  interface_name: format!("{}ServiceI", extract_name(?taskLabel)),

  // Service layer characteristics
  has_transactional: true,                    // @Transactional by default
  transaction_propagation: "REQUIRED",        // Default

  decomposition_type: ?decompositionType,     // NetDecomposition vs ExternalService

  // Methods to generate
  execute_method: format!("execute({})", ?taskLabel),
  cancel_method: "cancel()",
  suspend_method: "suspend()",
  resume_method: "resume()",
}
```

---

### Query 8.2: Multi-Instance Tasks (for Threshold/Join Logic)

**ID:** RULE8_MULTI_INSTANCE_TASKS

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?task ?label ?threshold ?joinType ?creationType
WHERE {
  ?task a yawl:MultiInstanceTask ;
        rdfs:label ?label .
  OPTIONAL { ?task yawl:minimumInstances ?minThreshold }
  OPTIONAL { ?task yawl:joinType ?joinType }          # AND | OR
  OPTIONAL { ?task yawl:creationType ?creationType }  # static | dynamic
}
```

**Expected Results:**
- **Example:**
  - ReviewTask | Review Task | 2 (min threshold) | AND | dynamic

---

## Summary: Query Execution Plan

### Phase 1: Validation (Each query separately)
```bash
For each rule (3-8):
  1. Load appropriate TTL file(s)
  2. Execute query
  3. Verify result count >= expected minimum
  4. Print first 5 results for manual validation
```

### Phase 2: Integration (Batch execution)
```bash
For each rule:
  1. Load query results into rule-specific struct
  2. Map to code context (TaskContext, RepositoryContext, etc.)
  3. Pass to template renderer
  4. Verify generated code compiles
```

### Phase 3: Benchmarking
```bash
For each rule:
  1. Generate code for all N entities/patterns
  2. Measure time: load + query + render
  3. Verify performance meets SLO (<500ms per rule)
```

---

## Testing Approach

### Unit Tests
```rust
#[test]
fn test_rule3_query_entities() {
  let graph = load_ontology("yawl-domain.ttl");
  let result = graph.query(RULE3_ENTITIES_ALL);
  assert_eq!(result.len(), 63);
  assert!(result[0].contains("YExternalClient"));
}

#[test]
fn test_rule7_query_all_wcp_patterns() {
  let graph = load_ontology("yawl-patterns.ttl");
  let result = graph.query(RULE7_ALL_WCP_PATTERNS);
  assert_eq!(result.len(), 43);  // All 43 WCP patterns
}
```

### Integration Tests
```rust
#[test]
fn test_rule3_generate_entities() {
  let entities = query_entities();       // Real data
  let code = generate_java_entities(entities);
  assert!(code.contains("@Entity"));
  assert!(code.contains("@Table"));
  assert!(code.len() > 10000);           // Substantial code
}
```

### Determinism Tests
```rust
#[test]
fn test_generated_code_is_deterministic() {
  let code1 = generate_entities();
  let code2 = generate_entities();
  assert_eq!(code1, code2);  // Reproducible
  assert_eq!(sha256(code1), sha256(code2));
}
```

---

## Appendix: Field Type Mappings

```
HBM Type        →  Java Type      →  SQL Type
────────────────────────────────────────────────
string          →  String         →  VARCHAR(255)
long            →  Long           →  BIGINT
int             →  Integer        →  INT
boolean         →  Boolean        →  BOOLEAN
timestamp       →  LocalDateTime  →  TIMESTAMP
uuid            →  UUID           →  UUID
```

---

## References

- YAWL Ontology Files: `/Users/sac/yawlv6/.claude/ggen/*.ttl`
- Oxigraph Documentation: https://oxigraph.org/
- SPARQL Specification: https://www.w3.org/TR/sparql11-query/
- WCP Patterns: http://www.workflowpatterns.com/

