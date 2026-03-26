# YAWL Ontology Files Reference

**Location:** `/Users/sac/yawlv6/.claude/ggen/`
**Updated:** 2026-03-26

This document provides a complete inventory and structure guide for all YAWL ontology files.

---

## File Inventory

```
/Users/sac/yawlv6/.claude/ggen/
├── yawl-domain.ttl         (3,256 lines, 110 KB)  ← PRIMARY
├── yawl-workflow.ttl       (  291 lines,  13 KB)  ← PRIMARY
├── yawl-patterns.ttl       (  456 lines,  22 KB)  ← PRIMARY
├── yawl-code.ttl           (  637 lines,  39 KB)  [SECONDARY]
├── yawl-modules.ttl        (  328 lines,  15 KB)  [SECONDARY]
└── primitives.ttl          (  231 lines,  11 KB)  [REFERENCE]

TOTAL: 5,199 lines, ~210 KB
```

---

## 1. yawl-domain.ttl (PRIMARY)

### Purpose
Auto-generated from 63 Hibernate HBM XML mapping files via `hbm_to_ttl.py` script.
Represents the persistent domain model of YAWL (database schema).

### Namespaces
```turtle
@prefix yawl:  <https://yawlfoundation.org/ontology#> .
@prefix hbm:   <https://yawlfoundation.org/hbm#> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
```

### Top-Level Classes (OWL)

```
yawl:Entity                 (root class for all persistent entities)
  ├─ 63 entity instances (YExternalClient, CostDriver, YTask, etc.)

yawl:Field                  (entity field/property)
yawl:ManyToOneRel           (many-to-one relationship)
yawl:OneToOneRel            (one-to-one relationship)
yawl:SetCollection          (java.util.Set<T>)
yawl:MapCollection          (java.util.Map<K,V>)
yawl:ComponentMapping       (JPA @Embeddable component)
```

### Entity Properties

```
yawl:Entity properties:
  - yawl:className          (String) Full Java class name
  - yawl:tableName          (String) Database table name
  - yawl:packageName        (String) Java package
  - yawl:sourceFile         (String) Path to HBM XML
  - yawl:hasIdField         (ObjectProperty) → yawl:Field
  - yawl:hasField           (ObjectProperty) → yawl:Field (0..*)
  - yawl:hasManyToOne       (ObjectProperty) → yawl:ManyToOneRel (0..*)
  - yawl:hasOneToOne        (ObjectProperty) → yawl:OneToOneRel (0..*)
  - yawl:hasSet             (ObjectProperty) → yawl:SetCollection (0..*)
  - yawl:hasMap             (ObjectProperty) → yawl:MapCollection (0..*)
  - yawl:hasComponent       (ObjectProperty) → yawl:ComponentMapping (0..*)
```

### Field Properties

```
yawl:Field properties:
  - yawl:fieldName          (String) Java field name
  - yawl:columnName         (String) Database column name
  - yawl:fieldType          (String) Java type (string, long, int, boolean, etc.)
  - yawl:isPrimaryKey       (Boolean) Is this the primary key?
  - yawl:generatorClass     (String) ID generation (assigned, native, uuid, etc.)
  - yawl:isUnique           (Boolean) Is column UNIQUE?
  - hbm:access              (String) Field access method (field, property)
  - hbm:notNull             (Boolean) NOT NULL constraint
```

### Entity Examples

```turtle
# Example 1: Simple entity
yawl:YExternalClient a yawl:Entity ;
    yawl:className "org.yawlfoundation.yawl.authentication.YExternalClient" ;
    yawl:tableName "ClientApps" ;
    yawl:packageName "org.yawlfoundation.yawl.authentication" ;
    yawl:sourceFile "src/org/yawlfoundation/yawl/authentication/YExternalClient.hbm.xml" ;
    yawl:hasIdField yawl:YExternalClient_id ;
    yawl:hasField yawl:YExternalClient_f0 ;
    yawl:hasField yawl:YExternalClient_f1 .

yawl:YExternalClient_id a yawl:Field ;
    yawl:fieldName "_userName" ;
    yawl:columnName "userID" ;
    yawl:fieldType "string" ;
    yawl:isPrimaryKey true ;
    yawl:generatorClass "assigned" ;
    hbm:access "field" .

yawl:YExternalClient_f0 a yawl:Field ;
    yawl:fieldName "_password" ;
    yawl:columnName "passwordText" ;
    yawl:fieldType "string" ;
    hbm:access "field" .

# Example 2: Complex entity with relationships
yawl:CostDriver a yawl:Entity ;
    yawl:className "org.yawlfoundation.yawl.cost.data.CostDriver" ;
    yawl:tableName "cost_Drivers" ;
    yawl:packageName "org.yawlfoundation.yawl.cost.data" ;
    hbm:lazy "false" ;
    yawl:hasIdField yawl:CostDriver_id ;
    yawl:hasField yawl:CostDriver_f0 ;
    yawl:hasSet yawl:CostDriver_set0 ;
    yawl:hasSet yawl:CostDriver_set1 ;
    yawl:hasComponent yawl:CostDriver_comp0 ;
    yawl:hasComponent yawl:CostDriver_comp1 .
```

### Generation Command
```bash
python3 /Users/sac/yawlv6/.claude/ggen/hbm_to_ttl.py
# Regenerates yawl-domain.ttl from 63 HBM XML files
```

### Rules Using This Ontology
- **Rule 3:** Entity → JPA @Entity
- **Rule 4:** Entity → Spring Repository
- **Rule 5:** Entity → DTO (with cardinality split/join)

### Sample Query
```sparql
SELECT ?entity ?className ?tableName WHERE {
  ?entity a yawl:Entity ;
    yawl:className ?className ;
    yawl:tableName ?tableName .
}
LIMIT 10
```

---

## 2. yawl-workflow.ttl (PRIMARY)

### Purpose
Core YAWL workflow domain ontology.
Represents the workflow execution model (Petri nets with YAWL semantics).

### Namespaces
```turtle
@prefix yawl:  <https://yawlfoundation.org/ontology#> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
```

### Top-Level Classes

```
yawl:YSpecification         (Workflow specification container)

yawl:Net                    (Petri net with YAWL control-flow semantics)
  └─ yawl:Decomposition (abstract)
     ├─ yawl:NetDecomposition
     └─ yawl:ExternalServiceDecomposition

yawl:Task                   (abstract base)
  ├─ yawl:AtomicTask
  ├─ yawl:CompositeTask
  └─ yawl:MultiInstanceTask

yawl:Condition              (Petri net place)
  ├─ yawl:InputCondition   (start place)
  ├─ yawl:OutputCondition  (end place)
  └─ yawl:InternalCondition (internal place)

yawl:Flow                   (Petri net edge/arc)

yawl:Version                (Specification version)
```

### Class Hierarchies

```
yawl:Task
  rdfs:comment "Abstract YTask: node in workflow net"

  yawl:AtomicTask
    rdfs:subClassOf yawl:Task
    rdfs:comment "YAtomicTask: leaf task with no internal net"

  yawl:CompositeTask
    rdfs:subClassOf yawl:Task
    rdfs:comment "YCompositeTask: has embedded Net decomposition"

  yawl:MultiInstanceTask
    rdfs:comment "YTask with YMultiInstanceAttributes"
    rdfs:comment "Spawns multiple work items; join: AND/OR"
```

### Key Properties

```
yawl:YSpecification:
  - yawl:id         (String)
  - yawl:version    (String, e.g., "2.2")
  - yawl:hasNet     (ObjectProperty) → yawl:Net

yawl:Net:
  - yawl:name       (String)
  - yawl:relatesTo  (ObjectProperty) → yawl:Task, yawl:Condition, yawl:Flow

yawl:Task:
  - yawl:label      (String)
  - yawl:implementedBy (ObjectProperty) → Java class

yawl:Condition:
  - yawl:label      (String)
  - rdfs:comment    (String)

yawl:Flow:
  - yawl:source     (ObjectProperty) → yawl:Task | yawl:Condition
  - yawl:target     (ObjectProperty) → yawl:Task | yawl:Condition
  - yawl:predicate  (String) Optional XPath predicate for conditional routing
```

### Examples

```turtle
# YSpecification
yawl:LoanApplicationSpec a yawl:YSpecification ;
    yawl:id "LoanApplication" ;
    yawl:version "2.2" ;
    yawl:hasNet yawl:LoanNet .

# Net
yawl:LoanNet a yawl:Net ;
    rdfs:label "Loan Application Net" ;
    yawl:relatesTo yawl:SubmitTask ;
    yawl:relatesTo yawl:ApproveTask ;
    yawl:relatesTo yawl:CompleteCondition .

# Condition types
yawl:InputCondition_0 a yawl:InputCondition ;
    rdfs:label "Start" .

yawl:OutputCondition_0 a yawl:OutputCondition ;
    rdfs:label "End" .

yawl:CheckCondition a yawl:InternalCondition ;
    rdfs:label "Approval Check" ;
    rdfs:comment "Internal place for routing logic" .

# Task - Atomic
yawl:SubmitTask a yawl:AtomicTask ;
    rdfs:label "Submit Application" ;
    yawl:implementedBy "org.example.workflow.SubmitTaskService" .

# Task - Composite
yawl:ApprovalProcess a yawl:CompositeTask ;
    rdfs:label "Approval Process" ;
    yawl:isDecomposedBy yawl:ApprovalNet ;
    rdfs:comment "Invokes sub-process for approval workflow" .

# Task - Multi-Instance
yawl:ReviewByMultiple a yawl:MultiInstanceTask ;
    rdfs:label "Review by Panel" ;
    yawl:minimumInstances "2" ;
    yawl:joinType "AND" ;
    yawl:creationType "dynamic" ;
    rdfs:comment "Spawn one task per reviewer, join when all complete" .

# Flow with predicate
yawl:ApprovalFlow a yawl:Flow ;
    yawl:source yawl:ApproveTask ;
    yawl:target yawl:CheckCondition ;
    yawl:predicate "status='approved'" ;
    yawl:isDefault false .

# Flow without predicate (default)
yawl:DefaultFlow a yawl:Flow ;
    yawl:source yawl:CheckCondition ;
    yawl:target yawl:OutputCondition_0 ;
    yawl:isDefault true .
```

### Rules Using This Ontology
- **Rule 6:** Rules → Conditions (workflow control)
- **Rule 8:** Composite Task → Service layer

### Sample Queries

```sparql
# All task types
SELECT ?taskType ?label WHERE {
  ?taskType rdfs:subClassOf yawl:Task ;
            rdfs:label ?label .
}

# All condition types
SELECT ?condition ?label WHERE {
  ?condition rdfs:subClassOf yawl:Condition ;
             rdfs:label ?label .
}

# Composite tasks and their decompositions
SELECT ?task ?label ?decomposition WHERE {
  ?task a yawl:CompositeTask ;
        rdfs:label ?label ;
        yawl:isDecomposedBy ?decomposition .
}
```

---

## 3. yawl-patterns.ttl (PRIMARY)

### Purpose
Represents all 43 Workflow Control Patterns (WCP) and their implementation in YAWL.

### Namespaces
```turtle
@prefix yawl:      <https://yawlfoundation.org/ontology#> .
@prefix yawl-wcp:  <https://yawlfoundation.org/patterns#> .
@prefix rdfs:      <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:       <http://www.w3.org/2002/07/owl#> .
@prefix xsd:       <http://www.w3.org/2001/XMLSchema#> .
```

### Classes

```
yawl-wcp:WorkflowControlPattern     (base class for all 43 patterns)
  ├─ yawl-wcp:JoinPattern           (13 input synchronization patterns)
  └─ yawl-wcp:SplitPattern          (15 output branching patterns)
```

### Pattern Structure

Each pattern is an RDF resource with:

```
?pattern a yawl-wcp:WorkflowControlPattern ;
    yawl-wcp:patternId        "1"..:"43" ;
    rdfs:label                "WCP-1: Sequence" ;
    yawl-wcp:category         "Sequence" | "Parallelism" | "Synchronization" | "Choice" | ... ;
    yawl-wcp:description      "Textual description of pattern" ;
    yawl-wcp:implementationStatus "Implemented" | "Partially" | "Not Implemented" ;
    yawl-wcp:implementedBy    yawl:AndSplit | yawl:XorJoin | ... ;
    rdfs:comment              "Semantic meaning and implications" .
```

### All 43 WCP Patterns

**WCP-1 to WCP-7: Basic Control Flow**

| ID | Pattern | Category | Implemented By |
|----|---------|----------|-----------------|
| 1 | Sequence | Sequence | Flow |
| 2 | Parallel Split | Parallelism | AndSplit |
| 3 | Synchronization | Synchronization | AndJoin |
| 4 | Exclusive Choice | Choice | XorSplit |
| 5 | Simple Merge | Choice | XorJoin |
| 6 | Multi-Choice | Choice | OrSplit |
| 7 | Synchronizing Merge | Choice | DiscriminatorJoin |

**WCP-8 to WCP-15: Advanced Control Flow**

| ID | Pattern | Category | Implemented By |
|----|---------|----------|-----------------|
| 8 | Multiple Merge | Parallelism | OrJoin |
| 9 | Structured Loop | Iteration | ... |
| 10 | Arbitrary Cycles | Iteration | ... |
| 11 | Implicit Termination | Termination | ... |
| 12 | Multiple Instances without Synchronization | Multiple Instance | ... |
| 13 | Multiple Instances with a priori Design-Time Knowledge | Multiple Instance | ... |
| 14 | Multiple Instances with a priori Runtime Knowledge | Multiple Instance | ... |
| 15 | Multiple Instances without a priori Runtime Knowledge | Multiple Instance | ... |

**WCP-16 to WCP-43: Advanced/Specialized Patterns**

| ID | Pattern | Category |
|----|---------|----------|
| 16 | Deferred Choice | Choice Deferral |
| 17 | Interleaved Parallel Routing | Advanced Synchronization |
| 18 | Milestone | Milestone |
| 19 | Cancel Task | Cancellation |
| 20 | Cancel Case | Cancellation |
| 21 | Structured Non-Sequential Flows | Transitions |
| 22 | Acyclic Synchronizing Merge | Synchronization |
| 23 | Cyclic Synchronizing Merge | Synchronization |
| 24 | Receive Task | Messaging |
| 25 | Send Task | Messaging |
| ... | ... | ... |
| 43 | ... (recursion, dynamic process creation, etc.) | ... |

### Pattern Examples

```turtle
# WCP-1: Sequence
yawl-wcp:WCP1 a yawl-wcp:WorkflowControlPattern ;
    rdfs:label "WCP-1: Sequence" ;
    yawl-wcp:patternId "1" ;
    yawl-wcp:category "Sequence" ;
    yawl-wcp:description "One task follows another in strict order" ;
    yawl-wcp:implementationStatus "Implemented" ;
    yawl-wcp:implementedBy yawl:Flow .

# WCP-2: Parallel Split
yawl-wcp:WCP2 a yawl-wcp:WorkflowControlPattern ;
    rdfs:label "WCP-2: Parallel Split" ;
    yawl-wcp:patternId "2" ;
    yawl-wcp:category "Parallelism" ;
    yawl-wcp:description "One flow branches into multiple parallel flows" ;
    yawl-wcp:implementationStatus "Implemented" ;
    yawl-wcp:implementedBy yawl:AndSplit .

# WCP-4: Exclusive Choice
yawl-wcp:WCP4 a yawl-wcp:WorkflowControlPattern ;
    rdfs:label "WCP-4: Exclusive Choice" ;
    yawl-wcp:patternId "4" ;
    yawl-wcp:category "Choice" ;
    yawl-wcp:description "One of several branches executes based on condition" ;
    yawl-wcp:implementationStatus "Implemented" ;
    yawl-wcp:implementedBy yawl:XorSplit .

# WCP-3: Synchronization
yawl-wcp:WCP3 a yawl-wcp:WorkflowControlPattern ;
    rdfs:label "WCP-3: Synchronization" ;
    yawl-wcp:patternId "3" ;
    yawl-wcp:category "Synchronization" ;
    yawl-wcp:description "Multiple flows merge into one, waiting for all inputs" ;
    yawl-wcp:implementationStatus "Implemented" ;
    yawl-wcp:implementedBy yawl:AndJoin .
```

### Rules Using This Ontology
- **Rule 7:** WCP Patterns → Enums (all 43 patterns)

### Sample Query

```sparql
SELECT ?patternId ?label ?category ?implementedBy WHERE {
  ?pattern a yawl-wcp:WorkflowControlPattern ;
    yawl-wcp:patternId ?patternId ;
    rdfs:label ?label ;
    yawl-wcp:category ?category ;
    yawl-wcp:implementedBy ?implementedBy .
}
ORDER BY ?patternId
```

---

## 4. yawl-code.ttl (SECONDARY)

### Purpose
Maps the YAWL Java codebase structure (31 packages, 90+ classes).

### Key Classes

```
yawl-code:JavaPackage       (Maven package under org.yawlfoundation.yawl)
yawl-code:JavaClass         (Java class file)
yawl-code:JavaInterface     (Standard interface)
yawl-code:SealedInterface   (Java 26 sealed interface)
```

### Package Hierarchy

**Top-level packages:**
```
org.yawlfoundation.yawl.elements          (Domain model - core)
  ├─ .elements.state                      (Token representation)
  ├─ .elements.data                       (Data binding)
  ├─ .elements.pattern                    (WCP patterns - sealed hierarchy)
  ├─ .elements.predicate                  (XPath evaluation)
  ├─ .elements.milestone                  (Milestone evaluation)
  └─ .elements.e2wfoj                     (Reduction graph analysis)

org.yawlfoundation.yawl.engine            (Runtime engine)
org.yawlfoundation.yawl.runtime           (Work items, execution state)
org.yawlfoundation.yawl.persistence       (Hibernate integration)
org.yawlfoundation.yawl.authentication    (User/client management)
... (25 more packages)
```

### Usage
- Referenced by Rule 3 (entity package validation)
- Referenced by Rule 8 (service/controller package mapping)

---

## 5. yawl-modules.ttl (SECONDARY)

### Purpose
Describes the Maven multi-module project structure of YAWL v6.

### Top-Level Modules

```
yawl-parent (v2026.1.0, Java 26, GraalVM)
├── yawl-core
│   ├── JPA entities
│   ├── Hibernage mappings
│   └── Domain model
├── yawl-webapp
│   ├── Control panel
│   └── JSP UI
└── services
    ├── Resource Service
    ├── Worklet Service
    ├── Mail Service
    ├── ... (6 more)
```

### Usage
- Build configuration reference
- Dependency information
- Java version requirements (Java 26)
- Test count validation

---

## 6. primitives.ttl (REFERENCE)

### Purpose
Claude Code framework ontology (not YAWL-specific).
Defines first-class primitives in Claude Code: Hooks, Settings, SlashCommands, MCP Servers.

### Not used by YAWL rules (reference only)

---

## Quick Access Commands

```bash
# View all files
ls -lh /Users/sac/yawlv6/.claude/ggen/

# Count lines per file
wc -l /Users/sac/yawlv6/.claude/ggen/*.ttl

# View yawl-domain (first 100 lines)
head -100 /Users/sac/yawlv6/.claude/ggen/yawl-domain.ttl

# Count entities in yawl-domain
grep "a yawl:Entity" /Users/sac/yawlv6/.claude/ggen/yawl-domain.ttl | wc -l
# Expected: 63

# Count WCP patterns
grep "a yawl-wcp:WorkflowControlPattern" /Users/sac/yawlv6/.claude/ggen/yawl-patterns.ttl | wc -l
# Expected: 43

# Validate Turtle syntax (if rapper installed)
rapper -i turtle /Users/sac/yawlv6/.claude/ggen/yawl-domain.ttl
```

---

## Integration Checklist

- [ ] **File Access:** All 6 files readable at `/Users/sac/yawlv6/.claude/ggen/`
- [ ] **Format Validation:** All files are valid Turtle (TTL)
- [ ] **Content Verification:**
  - [ ] yawl-domain.ttl contains 63 yawl:Entity instances
  - [ ] yawl-workflow.ttl contains Task/Condition/Flow classes
  - [ ] yawl-patterns.ttl contains 43 WCP patterns
- [ ] **Namespace Resolution:** All namespaces properly defined with @prefix
- [ ] **RDF Parser Compatibility:** Loads via oxigraph without errors

---

## File Update Instructions

### If yawl-domain.ttl changes (HBM files updated):
```bash
cd /Users/sac/yawlv6/.claude/ggen/
python3 hbm_to_ttl.py
# Regenerates yawl-domain.ttl from HBM sources
```

### If YAWL structure changes:
```bash
# Update corresponding TTL files in:
# /Users/sac/yawlv6/.claude/ggen/
# Then re-run ggen to update code generation
ggen sync
```

---

**Last Updated:** 2026-03-26
**Verified:** All files exist and are accessible
**Status:** Ready for integration into Rules 3-8

