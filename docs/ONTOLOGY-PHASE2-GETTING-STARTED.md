<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Phase 2 Implementation: Getting Started Guide](#phase-2-implementation-getting-started-guide)
  - [Overview (2 minutes)](#overview-2-minutes)
  - [Documentation Reading Order](#documentation-reading-order)
  - [Quick Start (5 minutes)](#quick-start-5-minutes)
    - [1. Read the HIPAA Example](#1-read-the-hipaa-example)
    - [2. Understand the Type System](#2-understand-the-type-system)
    - [3. Understand the Pipeline](#3-understand-the-pipeline)
  - [Phase 2 Scope (What to Build)](#phase-2-scope-what-to-build)
    - [File: `crates/ggen-ontology-core/src/integration.rs`](#file-cratesggen-ontology-coresrcintegrationrs)
    - [File: `crates/ggen-cli/src/cmds/ontology.rs`](#file-cratesggen-clisrccmdsontologyrs)
    - [Files: Phase 3 Preparation](#files-phase-3-preparation)
  - [Task Breakdown (Quick View)](#task-breakdown-quick-view)
  - [Development Checklist](#development-checklist)
    - [Before You Start](#before-you-start)
    - [Day 1 (Type System)](#day-1-type-system)
    - [Day 2-3 (Domain Parsing)](#day-2-3-domain-parsing)
    - [Day 4-5 (Entity Mapping)](#day-4-5-entity-mapping)
    - [Day 6-7 (Query Generation)](#day-6-7-query-generation)
    - [Day 8 (Pipeline Integration)](#day-8-pipeline-integration)
    - [Day 9-10 (CLI + Testing)](#day-9-10-cli--testing)
  - [Code Generation Pattern](#code-generation-pattern)
    - [Type Definition Example](#type-definition-example)
    - [Method Example](#method-example)
    - [Test Example (Chicago TDD)](#test-example-chicago-tdd)
  - [Testing Requirements](#testing-requirements)
    - [Types of Tests](#types-of-tests)
    - [Test Coverage Target](#test-coverage-target)
  - [CLI Examples](#cli-examples)
    - [Run Mapping](#run-mapping)
    - [Validate Domain](#validate-domain)
  - [Success Indicators](#success-indicators)
    - [By End of Week 3](#by-end-of-week-3)
    - [By End of Week 4](#by-end-of-week-4)
  - [Performance Targets](#performance-targets)
  - [Common Pitfalls & Solutions](#common-pitfalls--solutions)
    - [Problem: Determinism Not Guaranteed](#problem-determinism-not-guaranteed)
    - [Problem: Entity Mapping Confidence Too Low](#problem-entity-mapping-confidence-too-low)
    - [Problem: SPARQL Queries Too Complex](#problem-sparql-queries-too-complex)
    - [Problem: Performance Degradation Over Time](#problem-performance-degradation-over-time)
  - [Getting Help](#getting-help)
    - [If Stuck on Type System](#if-stuck-on-type-system)
    - [If Stuck on Parser](#if-stuck-on-parser)
    - [If Stuck on Entity Mapping](#if-stuck-on-entity-mapping)
    - [If Stuck on Query Generation](#if-stuck-on-query-generation)
    - [If Stuck on CLI](#if-stuck-on-cli)
    - [If Tests Not Passing](#if-tests-not-passing)
  - [Next Steps After Phase 2](#next-steps-after-phase-2)
    - [Immediate (Phase 3 Kickoff)](#immediate-phase-3-kickoff)
    - [Timeline](#timeline)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Phase 2 Implementation: Getting Started Guide

**Quick Reference**: How to begin Phase 2 work immediately

---

## Overview (2 minutes)

Phase 2 transforms customer domain YAML into ontology-mapped entities and SPARQL queries:

```
domain.yaml → Parser → Entities → EntityMapper → Mappings → QueryGen → queries.json
```

**Duration**: 2 weeks (10 business days)
**Effort**: 23 tasks across 2 developers
**Output**: MappingResult JSON ready for Phase 3

---

## Documentation Reading Order

**Time: 10-15 minutes to understand scope**

1. **Start here**: `ONTOLOGY-PHASE2-SUMMARY.md` (5 min)
   - Executive summary
   - Key metrics and deliverables
   - Success criteria

2. **Then read**: `ONTOLOGY-PHASE2-IMPLEMENTATION.md` (10 min)
   - Detailed type definitions
   - Core method pseudocode
   - Example outputs

3. **Architecture**: `ONTOLOGY-PHASE2-INTEGRATION-DIAGRAM.md` (5 min)
   - Data flow diagrams
   - Component interactions
   - Type relationships

4. **Examples**: `ONTOLOGY-PHASE2-CODE-EXAMPLES.md` (10 min)
   - HIPAA domain YAML
   - Rust code examples
   - CLI usage
   - Test examples

5. **Planning**: `ONTOLOGY-PHASE2-WEEK-BY-WEEK.md` (5 min)
   - 23 task breakdown
   - Week 3 & 4 schedule
   - Parallel execution strategy

---

## Quick Start (5 minutes)

### 1. Read the HIPAA Example

```bash
cat docs/examples/domain-hipaa.yaml
```

**What it shows**: 8 entities (policies, controls, services, classifications)

### 2. Understand the Type System

Key types to implement:

```rust
// Input
Entity { id, name, entity_type, attributes, tags, relationships }

// Processing
OntologyMapping { entity_id, primary_match, confidence, rationale }

// Output
SparqlQuery { id, query, purpose, applicable_entities }
MappingResult { parsed_entities, mappings, queries, overall_confidence }
```

### 3. Understand the Pipeline

```rust
// Pseudocode
let entities = parse_domain_description(yaml)?;
let mappings = map_entities_to_ontology(entities)?;
let queries = generate_queries_for_mappings(mappings)?;
let result = MappingResult { entities, mappings, queries, ... };
```

---

## Phase 2 Scope (What to Build)

### File: `crates/ggen-ontology-core/src/integration.rs`

**Implement 4 methods**:

1. `parse_domain_description(yaml: &str) -> Result<Vec<Entity>>`
   - Parse YAML into Entity structs
   - Time: 2-3 hours including tests

2. `map_entities_to_ontology(entities: Vec<Entity>) -> Result<Vec<OntologyMapping>>`
   - Route to EntityMapper methods
   - Extract confidence scores
   - Time: 2-3 hours including tests

3. `generate_queries_for_mappings(mappings: Vec<OntologyMapping>) -> Result<Vec<SparqlQuery>>`
   - Create SPARQL queries for each mapping
   - Generate deterministic IDs
   - Time: 2-3 hours including tests

4. `execute_mapping_pipeline(yaml: &str) -> Result<MappingResult>`
   - Orchestrate all steps
   - Aggregate results
   - Time: 1-2 hours including tests

**Total**: ~8-10 hours of implementation (+ 10-15 hours of testing)

### File: `crates/ggen-cli/src/cmds/ontology.rs`

**Implement 2 CLI commands**:

1. `ggen ontology map --input domain.yaml --format json --output result.json`
   - Load YAML file
   - Execute pipeline
   - Output JSON/YAML

2. `ggen ontology validate --input domain.yaml`
   - Parse and report entity count
   - Validate structure

**Total**: 2-3 hours

### Files: Phase 3 Preparation

1. `crates/ggen-ontology-core/src/provider_mapper.rs`
   - ProviderMapper trait
   - Placeholder implementations

2. `crates/ggen-ontology-core/src/receipt_generator.rs`
   - ComplianceReceipt struct
   - SHA256 hashing

**Total**: 2-3 hours (ready for Phase 3)

---

## Task Breakdown (Quick View)

**Week 3 (5 days)**:
- Day 1-2: Type system + domain parsing (Tasks 1-4)
- Day 3-4: Entity mapping (Tasks 5-8)
- Day 5: Query generation (Tasks 9-11)

**Week 4 (5 days)**:
- Day 1-2: Pipeline + CLI integration (Tasks 12-15)
- Day 3: Documentation + receipts (Tasks 16-18)
- Day 4-5: Testing + validation (Tasks 19-23)

**Total**: 23 tasks

---

## Development Checklist

### Before You Start

- [ ] Read ONTOLOGY-PHASE2-SUMMARY.md
- [ ] Read ONTOLOGY-PHASE2-IMPLEMENTATION.md
- [ ] Review HIPAA example: `docs/examples/domain-hipaa.yaml`
- [ ] Understand Chicago TDD testing requirements
- [ ] Confirm Rust environment: `cargo make check` works
- [ ] Review CLAUDE.md SPARC + Chicago TDD patterns

### Day 1 (Type System)

- [ ] Create `crates/ggen-ontology-core/src/integration.rs`
- [ ] Define Entity, EntityType, EntityRelationship structs
- [ ] Define OntologyMapping, SparqlQuery, MappingResult structs
- [ ] Implement Serialize/Deserialize traits
- [ ] Run `cargo make check` - no errors
- [ ] Write 4 unit tests for type creation

### Day 2-3 (Domain Parsing)

- [ ] Implement `parse_domain_description()` method
- [ ] Handle YAML parsing with error context
- [ ] Validate required fields
- [ ] Ensure deterministic entity ordering
- [ ] Write 4 unit tests for parsing
- [ ] Run `cargo make test` - all pass
- [ ] Test determinism: parse same YAML 10x, verify identical output

### Day 4-5 (Entity Mapping)

- [ ] Implement `map_entities_to_ontology()` method
- [ ] Route entities to EntityMapper methods
- [ ] Extract primary match and confidence
- [ ] Create OntologyMapping structs
- [ ] Ensure deterministic sorting
- [ ] Write 5 integration tests for mapping
- [ ] Verify determinism (10 runs → identical output)

### Day 6-7 (Query Generation)

- [ ] Implement `generate_queries_for_mappings()` method
- [ ] Generate enumeration queries
- [ ] Generate compliance queries (policies)
- [ ] Generate verification queries (controls)
- [ ] Ensure deterministic query IDs
- [ ] Write 6 validation tests
- [ ] Verify determinism

### Day 8 (Pipeline Integration)

- [ ] Implement `execute_mapping_pipeline()` orchestration
- [ ] Chain all steps together
- [ ] Calculate overall confidence
- [ ] Create provider mapping placeholders
- [ ] Write 3 end-to-end tests
- [ ] Measure performance: should be <3 seconds

### Day 9-10 (CLI + Testing)

- [ ] Implement CLI commands (map, validate)
- [ ] Create comprehensive test suite (20+ tests)
- [ ] Run Andon signal checks
- [ ] Performance benchmarking
- [ ] Documentation review
- [ ] Final smoke testing

---

## Code Generation Pattern

### Type Definition Example

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entity {
    pub id: String,
    pub name: String,
    pub entity_type: EntityType,
    pub attributes: std::collections::HashMap<String, String>,
    pub tags: Vec<String>,
    pub relationships: Vec<EntityRelationship>,
    pub source_yaml: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EntityType {
    Policy,
    Control,
    Service,
    Classification,
    Jurisdiction,
    Role,
    Requirement,
    Artifact,
    Unknown,
}
```

### Method Example

```rust
impl EnterpriseDomainMapper {
    pub fn parse_domain_description(yaml: &str) -> Result<Vec<Entity>> {
        // 1. Parse YAML
        let value: serde_yaml::Value = serde_yaml::from_str(yaml)?;

        // 2. Extract entities section
        let entities_section = value.get("entities")
            .ok_or_else(|| OntologyError::validation(vec![
                "Missing 'entities' section".to_string()
            ]))?;

        // 3. Create Entity structs
        let mut entities = Vec::new();
        for (entity_id, entity_value) in entities_section.as_mapping().unwrap().iter() {
            // ... parse entity fields ...
            entities.push(Entity { /* ... */ });
        }

        // 4. Sort deterministically
        entities.sort_by_key(|e| (format!("{:?}", e.entity_type), e.id.clone()));

        Ok(entities)
    }
}
```

### Test Example (Chicago TDD)

```rust
#[test]
fn test_parse_domain_creates_entities_deterministically() {
    // Arrange
    let domain = r#"
entities:
  policy:
    name: "Test Policy"
    type: "Policy"
    tags: ["test"]
"#;

    // Act
    let result1 = EnterpriseDomainMapper::parse_domain_description(domain);
    let result2 = EnterpriseDomainMapper::parse_domain_description(domain);

    // Assert: Observable behavior - identical results
    assert_eq!(result1.is_ok(), result2.is_ok());
    if let (Ok(entities1), Ok(entities2)) = (result1, result2) {
        assert_eq!(entities1.len(), entities2.len());
        assert_eq!(
            serde_json::to_string(&entities1).unwrap(),
            serde_json::to_string(&entities2).unwrap()
        );
    }
}
```

---

## Testing Requirements

### Types of Tests

1. **Unit Tests** (30 tests)
   - Test individual methods
   - Mock dependencies
   - Fast execution (<100ms each)

2. **Integration Tests** (30 tests)
   - Test complete flows
   - Use real EntityMapper + SparqlGenerator
   - Verify end-to-end behavior

3. **Determinism Tests** (5 tests)
   - Run same input 100x
   - Verify identical output
   - Check JSON serialization

4. **Error Tests** (10 tests)
   - Invalid YAML
   - Missing fields
   - Edge cases

### Test Coverage Target

- >85% of Phase 2 code paths
- All public APIs tested
- Error paths tested
- Mutation testing: >80% kill rate

---

## CLI Examples

### Run Mapping

```bash
# Basic usage
ggen ontology map --input domain.yaml

# Save to file
ggen ontology map \
  --input domain.yaml \
  --format json \
  --output result.json

# With provider skeleton
ggen ontology map \
  --input domain.yaml \
  --format json \
  --output result.json \
  --with-provider-skeleton
```

### Validate Domain

```bash
ggen ontology validate --input domain.yaml
```

**Expected output**:
```
Domain validation passed: 8 entities found
  - hipaa_privacy_policy (Policy)
  - phi_classification (Classification)
  - encryption_at_rest_control (Control)
  - ...
```

---

## Success Indicators

### By End of Week 3

- [ ] Domain parser working (parses HIPAA example)
- [ ] Entity mapping working (maps 8 HIPAA entities)
- [ ] Query generation working (generates 12+ queries)
- [ ] Determinism verified (same input → identical output)
- [ ] 30+ tests passing

### By End of Week 4

- [ ] CLI commands working
- [ ] 60+ tests passing (>85% coverage)
- [ ] Andon signals clear (check, test, lint, slo-check)
- [ ] Performance benchmarks met (<3s pipeline)
- [ ] Documentation complete
- [ ] Ready for Phase 3 handoff

---

## Performance Targets

| Component | Target | Measure |
|-----------|--------|---------|
| Parsing | <500ms | Time to parse domain.yaml |
| Mapping | <1s | Time to map 8 entities |
| Query gen | <1s | Time to generate queries |
| Full pipeline | <3s | End-to-end time |
| Test suite | <30s | All 60+ tests |
| Determinism | 100% | Same JSON output 100x |

---

## Common Pitfalls & Solutions

### Problem: Determinism Not Guaranteed

**Solution**:
- Sort consistently at each step (by ID, not random)
- Avoid system time in mappings (use timestamp only at end)
- Use deterministic SPARQL generation (formatted strings, not templates)

### Problem: Entity Mapping Confidence Too Low

**Solution**:
- Use EntityMapper scoring as baseline
- Add context-aware boosting (tags, attributes)
- Include alternative matches with lower scores
- Document rationale for each mapping

### Problem: SPARQL Queries Too Complex

**Solution**:
- Keep queries focused on single entity type
- Use simple SELECT + WHERE patterns
- Avoid complex JOINs
- Generate separate queries for different concerns

### Problem: Performance Degradation Over Time

**Solution**:
- Profile hot paths early
- Cache SPARQL query strings
- Batch entity processing if needed
- Use efficient sorting algorithms

---

## Getting Help

### If Stuck on Type System

→ See `ONTOLOGY-PHASE2-IMPLEMENTATION.md` Section 1
→ Review Phase 1 existing types in `entity_mapper.rs`

### If Stuck on Parser

→ See `ONTOLOGY-PHASE2-CODE-EXAMPLES.md` Example 2
→ Review HIPAA YAML structure
→ Test with simple YAML first, add complexity

### If Stuck on Entity Mapping

→ See EntityMapper methods (already exist in Phase 1)
→ Test routing to each matcher
→ Verify confidence scores

### If Stuck on Query Generation

→ See SparqlGenerator methods (already exist in Phase 1)
→ Build simple queries first
→ Verify SPARQL syntax

### If Stuck on CLI

→ Look at existing CLI command patterns in `ggen-cli`
→ Use clap derive macros
→ Test with simple input first

### If Tests Not Passing

→ Follow Chicago TDD: Arrange-Act-Assert
→ Test observable behavior, not implementation
→ Use real collaborators (not mocks)
→ Debug with `RUST_LOG=debug cargo make test`

---

## Next Steps After Phase 2

### Immediate (Phase 3 Kickoff)

1. Hand off MappingResult JSON to Phase 3
2. Phase 3 team implements AWS/GCP/Azure ProviderMapper
3. Phase 3 executes SPARQL queries against customer RDF
4. Phase 3 generates provider-specific proposals

### Timeline

- Week 5-6: Phase 3 implementation
- Week 7: Provider-specific proposals generated
- Week 8: Compliance receipts and final testing

---

## Summary

**Phase 2 is a focused, well-scoped 2-week effort**:
- Clear deliverables (23 tasks)
- Parallel execution ready
- Comprehensive testing strategy
- Determinism guaranteed
- Ready to feed Phase 3

**Start with**: ONTOLOGY-PHASE2-SUMMARY.md
**Then read**: ONTOLOGY-PHASE2-IMPLEMENTATION.md
**Code reference**: ONTOLOGY-PHASE2-CODE-EXAMPLES.md
**Task tracking**: ONTOLOGY-PHASE2-WEEK-BY-WEEK.md

Ready to build? Start Day 1 with type definitions and domain parsing.

