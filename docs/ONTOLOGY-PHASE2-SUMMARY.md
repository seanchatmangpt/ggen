# Phase 2 Implementation Plan Summary

**Duration**: Weeks 3-4 (10 business days)
**Status**: Ready for execution
**Last Updated**: 2024-01-19

---

## Executive Summary

Phase 2 transforms customer domain descriptions (YAML) into ontology-mapped entities and deterministic SPARQL queries. This integration layer connects Phase 1 (ontology core) with Phase 3 (provider-specific proposals).

**Key Metrics**:
- Entity mapping coverage: >90% of standard ontology classes
- Pipeline performance: <3 seconds end-to-end
- Determinism: 100% (same input → identical output)
- Test coverage: >85% of code paths
- Confidence scoring: 0.0-1.0 per entity mapping

---

## Deliverables (23 Tasks Over 2 Weeks)

### Week 3: Core Infrastructure (12 Tasks)

#### Day 1-2: Type System & Domain Parsing (4 Tasks)
1. Define integration layer type structures
2. Implement `parse_domain_description()` method
3. Create 4 unit tests for domain parsing
4. Review and refine type system

#### Day 3-4: Entity-to-Ontology Mapping (4 Tasks)
5. Implement `map_entities_to_ontology()` method
6. Create 5 integration tests for entity mapping
7. Verify determinism (100 runs → identical output)
8. Add support for complex entity matching

#### Day 5: SPARQL Query Generation (4 Tasks)
9. Implement `generate_queries_for_mappings()` method
10. Create 6 SPARQL query validation tests
11. Verify query generation determinism
12. (Buffer for issues or refinement)

### Week 4: Integration & Validation (11 Tasks)

#### Day 1-2: Pipeline & CLI (4 Tasks)
13. Implement `execute_mapping_pipeline()` orchestration
14. Add `ggen ontology map` CLI command
15. Add `ggen ontology validate` CLI command
16. Implement ProviderMapper trait interface (Phase 3 prep)

#### Day 3: Documentation & Receipts (3 Tasks)
17. Implement ComplianceReceipt structure
18. Create comprehensive example documentation
19. Update CLAUDE.md with Phase 2 patterns

#### Day 4-5: Testing & Validation (4 Tasks)
20. Create comprehensive test suite (20+ tests)
21. Run Andon signal checks (compiler, tests, lint)
22. Performance benchmarking
23. Phase 2 completion summary

---

## File Structure

### Documentation Created

```
docs/
├── ONTOLOGY-PHASE2-IMPLEMENTATION.md    (This file - detailed spec)
├── ONTOLOGY-PHASE2-WEEK-BY-WEEK.md     (Task breakdown, 23 tasks)
├── ONTOLOGY-PHASE2-INTEGRATION-DIAGRAM.md (Architecture & data flow)
├── ONTOLOGY-PHASE2-CODE-EXAMPLES.md    (HIPAA example, code snippets)
├── ONTOLOGY-PHASE2-SUMMARY.md          (This summary)
└── examples/
    └── domain-hipaa.yaml                (Complete HIPAA example domain)
```

### Code to be Created

```
crates/ggen-ontology-core/src/
├── integration.rs          (EnterpriseDomainMapper + types)
├── provider_mapper.rs      (ProviderMapper trait - Phase 3 prep)
└── receipt_generator.rs    (ComplianceReceipt - Phase 3 prep)

crates/ggen-ontology-core/tests/
├── mapping_integration.rs  (20+ integration tests)
└── behavior_verification.rs (Chicago TDD tests)

crates/ggen-cli/src/cmds/
└── ontology.rs             (CLI commands: map, validate)

crates/ggen-ontology-core/examples/
└── hipaa_mapping.rs        (Executable HIPAA example)

crates/ggen-ontology-core/benches/
└── mapping_bench.rs        (Performance benchmarks)
```

---

## Architecture Highlights

### Data Flow Pipeline

```
Customer YAML Domain
         ↓
   Domain Parser
         ↓
  Parsed Entities (Vec<Entity>)
         ↓
   Entity Mapper (5 methods)
         ↓
  Ontology Mappings (Vec<OntologyMapping>)
         ↓
 SPARQL Query Generator
         ↓
   SPARQL Queries (Vec<SparqlQuery>)
         ↓
    Aggregation Layer
         ↓
  MappingResult (JSON/YAML)
         ↓
   CLI Output / Phase 3 Input
```

### Type System

**Inputs**:
- `Entity`: Customer domain entity with attributes and relationships
- `EntityType`: Policy, Control, Service, Classification, etc.

**Processing**:
- `OntologyMatch`: Ontology class with confidence score
- `OntologyMapping`: Entity mapped to ontology classes

**Outputs**:
- `SparqlQuery`: Deterministic SPARQL query with metadata
- `MappingResult`: Complete aggregated result
- `ProviderMappings`: Placeholder for Phase 3 provider routing

### Determinism Guarantees

1. **YAML Parsing**: Alphabetically sorted keys, consistent ordering
2. **Entity Mapping**: EntityMapper methods are deterministic
3. **Query Generation**: Deterministic query IDs (idx_type_entityid)
4. **Sorting**: Consistent at each step (by entity_id, query_id)
5. **Timestamp**: Captured once at end (not per-entity)

### Confidence Scoring

- **Per-entity**: 0.0-1.0 (from primary ontology match)
- **Aggregate**: Average of all entity confidences
- **Deterministic**: Same entity always gets same score
- **Traceable**: Rationale explains scoring decision

---

## Test Coverage Plan

### Unit Tests (30 tests)

- Domain parsing: 4 tests
- Entity mapping: 5 tests
- Query generation: 6 tests
- Error handling: 5 tests
- Type system: 3 tests
- Determinism: 3 tests
- Edge cases: 4 tests

### Integration Tests (30 tests)

- HIPAA scenario: 1 test
- Determinism (100 runs): 1 test
- End-to-end pipeline: 5 tests
- Multi-entity scenarios: 10 tests
- CLI integration: 5 tests
- Provider mapping (Phase 3 prep): 3 tests
- Performance SLO: 4 tests

### Coverage Target: >85% of Phase 2 code paths

---

## Performance SLOs

| Component | Target | Verification |
|-----------|--------|--------------|
| Domain parsing | <500ms | `cargo make bench` |
| Entity mapping | <1s | `cargo make bench` |
| Query generation | <1s | `cargo make bench` |
| Full pipeline | <3s | `cargo make bench` |
| Determinism | 100% match | Test suite |
| Ontology coverage | >90% | Matcher tests |

---

## Andon Signals (Quality Gates)

**Before marking Phase 2 complete**:

1. `cargo make check` - No compiler errors/warnings
2. `cargo make test` - All tests pass (60+)
3. `cargo make lint` - No clippy warnings
4. `cargo make slo-check` - Performance within limits
5. `cargo make audit` - No security vulnerabilities

---

## Success Criteria

1. **Functionality**: Complete domain YAML → ontology mappings → SPARQL queries
2. **Determinism**: Same domain generates identical mappings/queries (byte-for-byte JSON)
3. **Coverage**: Map >90% of standard ontology entities
4. **Performance**: Complete pipeline in <3 seconds
5. **Quality**: 60+ tests with >85% code coverage
6. **Documentation**: Comprehensive examples and architecture docs
7. **Code Quality**: All Andon signals clear

---

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Entity-ontology ambiguity | Confidence scoring + alternative match tracking |
| SPARQL query complexity | Keep queries simple, focused on entity type |
| YAML parsing failures | Validate structure early, helpful error messages |
| Performance degradation | Profile hot paths, cache queries, optimize sorting |
| Determinism violation | Consistent sorting at each step, no system time |
| Phase 3 interface issues | Placeholder interfaces, documented contract |

---

## Transition to Phase 3

**Phase 2 Output**:
- `parsed_entities`: Validated customer domain entities
- `mappings`: Entity-to-ontology mappings with confidence
- `queries`: SPARQL queries for compliance verification
- `provider_mappings`: Placeholder routing info

**Phase 3 Receives**:
- MappingResult JSON containing all Phase 2 output
- Implements AWS/GCP/Azure ProviderMapper
- Executes SPARQL queries against customer RDF
- Maps results to provider operations
- Generates provider-specific proposals
- Creates compliance receipts

**Key Assumption**: Phase 2 output is deterministic and reproducible, enabling Phase 3 to build on stable foundation.

---

## Implementation Notes

### Chicago TDD Required

- **State-based testing**: Verify observable outputs/state changes
- **Real collaborators**: Use actual EntityMapper, SparqlGenerator
- **AAA pattern**: Arrange-Act-Assert for all tests
- **Behavior focus**: Tests verify what code does, not implementation

### Type-First Design

- Use Rust types to encode invariants
- Confidence scores are typed (f32 with range 0.0-1.0)
- OntologyMatch cannot be created without all fields
- Result<T, OntologyError> for all fallible operations

### Zero-Cost Abstractions

- Generic sorting for consistent ordering
- No unnecessary allocations
- Deterministic algorithms (no random seeds)
- Cache query strings for reuse

### Cargo Make Requirements

- **NEVER use direct cargo commands** (use `cargo make` wrapper)
- All builds have timeout wrappers
- Hooks for pre/post operations
- Deterministic build artifacts

---

## Parallel Execution Strategy

### Week 3 Parallelization

- **Day 1-2**: Type system (1 dev) + Domain parsing (1 dev in parallel)
- **Day 3-4**: Entity mapping (1 dev) + Mapping tests (1 dev in parallel)
- **Day 5**: Query generation (1 dev) + Query tests (1 dev in parallel)

### Week 4 Parallelization

- **Day 1-2**: Pipeline orchestration (1 dev) + CLI commands (1 dev) + Provider interface (1 dev)
- **Day 3**: Receipt generator (1 dev) + Documentation (1 dev in parallel)
- **Day 4-5**: Test suite (1 dev) + Andon signals (1 dev) + Smoke testing (1 dev in parallel)

**Minimum Required**: 1 developer full-time
**Optimal**: 2-3 developers with clear task ownership

---

## Daily Checkpoints

### Week 3

- **Mon**: Types + parsing kickoff (50% done)
- **Tue**: Domain parsing complete + entity mapping starts
- **Wed**: Entity mapping halfway through (50% complete)
- **Thu**: Entity mapping complete + query gen starts
- **Fri**: All core components complete

### Week 4

- **Mon**: Pipeline orchestration + CLI integration
- **Tue**: CLI commands complete + provider interface done
- **Wed**: Receipt generator + documentation 80% complete
- **Thu**: All tests passing, Andon signals clear
- **Fri**: Smoke testing complete, Phase 2 summary ready

---

## Supporting Documents

1. **ONTOLOGY-PHASE2-IMPLEMENTATION.md** - Detailed implementation spec with pseudocode
2. **ONTOLOGY-PHASE2-WEEK-BY-WEEK.md** - Task breakdown (23 tasks)
3. **ONTOLOGY-PHASE2-INTEGRATION-DIAGRAM.md** - Architecture diagrams
4. **ONTOLOGY-PHASE2-CODE-EXAMPLES.md** - HIPAA example, tests, CLI usage

---

## Getting Started

1. **Read**: Start with ONTOLOGY-PHASE2-IMPLEMENTATION.md (detailed spec)
2. **Plan**: Review ONTOLOGY-PHASE2-WEEK-BY-WEEK.md (task breakdown)
3. **Architecture**: Study ONTOLOGY-PHASE2-INTEGRATION-DIAGRAM.md (visual flow)
4. **Examples**: Run through ONTOLOGY-PHASE2-CODE-EXAMPLES.md (HIPAA scenario)
5. **Execute**: Follow week-by-week plan with parallel task execution

---

## Questions & Clarifications

**Q: What if entity mapping confidence is <0.5?**
A: Include lower-scoring alternatives in OntologyMapping.ontology_matches. Phase 3 can investigate ambiguity or request manual clarification from customer.

**Q: How do we handle entities that don't match any ontology class?**
A: Return a generic match with confidence 0.50-0.60 and rationale explaining why. Phase 3 can flag for manual review.

**Q: Can we parallelize query generation?**
A: Yes, generate queries in parallel per entity, then sort deterministically. Verify sorting produces identical results.

**Q: What if YAML has circular entity relationships?**
A: Allow relationships to point anywhere. Phase 3 will handle circular reference detection during proposal generation.

**Q: How does Phase 2 connect to provider proposals?**
A: MappingResult.provider_mappings contains placeholder AWS/GCP/Azure operation names. Phase 3 replaces placeholders with real operations based on ProviderMapper implementations.

---

## Conclusion

Phase 2 is a well-defined, focused 2-week effort to build the mapping integration layer. Clear specifications, comprehensive testing strategy, and determinism guarantees provide a solid foundation for Phase 3 provider-specific proposals.

The architecture is clean:
- **Input**: Customer domain YAML
- **Processing**: Entity mapping + SPARQL generation
- **Output**: Deterministic MappingResult JSON
- **Next**: Phase 3 fan-out to AWS/GCP/Azure

Ready to execute.

