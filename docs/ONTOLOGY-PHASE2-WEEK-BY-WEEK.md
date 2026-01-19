# ggen Phase 2: Week-by-Week Task Breakdown

**Duration**: Weeks 3-4 (10 business days)

## Week 3 Schedule

### Day 1-2: EnterpriseDomainMapper Core Implementation

**Tasks**:

1. **Task 1: Define type structures for integration layer**
   - Create Entity, EntityType, EntityRelationship structs
   - Create OntologyMapping, SparqlQuery, MappingResult types
   - Implement Serialize/Deserialize for JSON/YAML output
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Acceptance: Types compile, derive macros work
   - Estimated: 1 hour
   - Verification: `cargo make check` passes

2. **Task 2: Implement parse_domain_description method**
   - Parse YAML domain description into Entity vec
   - Validate entity structure (required fields)
   - Extract attributes, tags, relationships
   - Ensure deterministic ordering
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Tests: 4 unit tests (valid YAML, invalid YAML, missing fields, edge cases)
   - Acceptance: Parse any valid domain YAML consistently
   - Estimated: 2 hours
   - Verification: `cargo make test` passes

3. **Task 3: Create unit tests for domain parsing**
   - Test valid domain YAML parsing
   - Test invalid YAML error handling
   - Test missing required fields error
   - Test complex entity relationships
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Coverage: >90% of parse function paths
   - Acceptance: All 4 tests pass consistently
   - Estimated: 1.5 hours
   - Verification: `cargo make test-unit` shows 4/4 passing

4. **Task 4: Review and refine type system for Phase 2-3 transition**
   - Ensure OntologyMapping supports provider routing
   - Verify ProviderMappings struct works with JSON serialization
   - Check that all types support serde_json
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Acceptance: Types serialize/deserialize without errors
   - Estimated: 1 hour
   - Verification: `cargo make check` passes

---

### Day 3-4: Entity-to-Ontology Mapping Pipeline

**Tasks**:

5. **Task 5: Implement map_entities_to_ontology method**
   - Route entities to appropriate EntityMapper methods
   - Extract primary match and calculate confidence
   - Generate mapping rationale strings
   - Create OntologyMapping structs
   - Ensure deterministic sorting
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Tests: 5 unit tests (policy mapping, classification, service, control, error)
   - Acceptance: Map all entity types correctly
   - Estimated: 2.5 hours
   - Verification: `cargo make test` passes

6. **Task 6: Create integration tests for entity mapping**
   - Test policy entity mapping to :PrivacyPolicy
   - Test classification entity mapping to :ConfidentialData
   - Test control entity mapping to :EncryptionControl
   - Test service entity mapping to :ComputeService
   - Test error case: entity with no matches
   - Location: `crates/ggen-ontology-core/tests/mapping_integration.rs`
   - Coverage: >85% of map_entities method paths
   - Acceptance: All 5 tests pass consistently
   - Estimated: 2 hours
   - Verification: `cargo make test` shows integration tests passing

7. **Task 7: Implement determinism verification**
   - Run same entity mapping 10x in tests
   - Assert all results are identical
   - Verify sort order is consistent
   - Location: `crates/ggen-ontology-core/tests/mapping_integration.rs`
   - Acceptance: Determinism test passes
   - Estimated: 1 hour
   - Verification: `cargo make test | grep determinism`

8. **Task 8: Add support for complex entity matching**
   - Handle entities with multiple attributes
   - Parse availability percentage from service requirements
   - Support tag-based entity classification hints
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Acceptance: Complex entities map correctly with hints
   - Estimated: 1.5 hours
   - Verification: `cargo make test` passes

---

### Day 5: SPARQL Query Generation & Validation

**Tasks**:

9. **Task 9: Implement generate_queries_for_mappings method**
   - Generate entity enumeration queries
   - Generate compliance queries for policies
   - Generate control verification queries
   - Ensure deterministic query IDs
   - Location: `crates/ggen-ontology-core/src/integration.rs`
   - Tests: 6 unit tests (enum queries, compliance queries, control queries, ordering, edge cases)
   - Acceptance: Generate >1 query per entity minimum
   - Estimated: 2.5 hours
   - Verification: `cargo make test` passes

10. **Task 10: Create SPARQL query validation tests**
    - Validate generated SPARQL syntax
    - Verify queries have all required fields (id, query, purpose)
    - Test query ordering consistency
    - Verify applicable_entities not empty
    - Location: `crates/ggen-ontology-core/tests/mapping_integration.rs`
    - Coverage: >90% of query generation paths
    - Acceptance: All queries valid SPARQL syntax
    - Estimated: 1.5 hours
    - Verification: `cargo make test` passes

11. **Task 11: Verify determinism of query generation**
    - Run same mapping 5x
    - Assert identical queries generated
    - Check query IDs are deterministic
    - Location: `crates/ggen-ontology-core/tests/mapping_integration.rs`
    - Acceptance: Query determinism test passes
    - Estimated: 1 hour
    - Verification: `cargo make test | grep "query.*determinism"`

---

## Week 4 Schedule

### Day 1-2: Integration Pipeline & CLI

**Tasks**:

12. **Task 12: Implement execute_mapping_pipeline orchestration method**
    - Chain parse → map → generate queries
    - Calculate overall confidence score
    - Create provider mapping placeholders
    - Create MappingResult aggregation
    - Location: `crates/ggen-ontology-core/src/integration.rs`
    - Tests: 3 integration tests (successful pipeline, error propagation, empty domain)
    - Acceptance: End-to-end pipeline executes in <3 seconds
    - Estimated: 1.5 hours
    - Verification: `cargo make test` passes, timing verified

13. **Task 13: Add CLI command: ggen ontology map**
    - Implement OntologyCommand enum with Map subcommand
    - Load YAML from file
    - Execute pipeline
    - Output JSON/YAML format
    - Location: `crates/ggen-cli/src/cmds/ontology.rs`
    - Acceptance: CLI command accepts input file, outputs result
    - Estimated: 1.5 hours
    - Verification: `ggen ontology map --input test.yaml --format json` works

14. **Task 14: Add CLI command: ggen ontology validate**
    - Implement Validate subcommand
    - Parse YAML and report entity count
    - Provide helpful error messages for invalid domains
    - Location: `crates/ggen-cli/src/cmds/ontology.rs`
    - Tests: 2 tests (valid domain, invalid domain)
    - Acceptance: CLI validates domain syntax correctly
    - Estimated: 1 hour
    - Verification: `ggen ontology validate --input test.yaml` works

15. **Task 15: Implement provider mapping placeholder interface**
    - Create ProviderMapper trait
    - Define AwsOperation, GcpOperation, AzureOperation structs
    - Add placeholder implementations
    - Document Phase 3 implementation plan
    - Location: `crates/ggen-ontology-core/src/provider_mapper.rs`
    - Acceptance: Trait compiles, placeholder runs without errors
    - Estimated: 1 hour
    - Verification: `cargo make check` passes

---

### Day 3: Receipt Generator & Documentation

**Tasks**:

16. **Task 16: Implement ComplianceReceipt structure**
    - Create ComplianceReceipt and ProofEntry structs
    - Implement SHA256 hashing of mappings/queries
    - Create proof chain from entities
    - Add placeholder for Ed25519 signature
    - Location: `crates/ggen-ontology-core/src/receipt_generator.rs`
    - Tests: 2 unit tests (receipt generation, proof chain creation)
    - Acceptance: Receipt generates without errors, hashes are deterministic
    - Estimated: 1.5 hours
    - Verification: `cargo make test` passes

17. **Task 17: Create comprehensive example documentation**
    - Write HIPAA domain YAML example
    - Document expected output JSON
    - Show CLI commands with examples
    - Explain mapping decisions and rationale
    - Location: `docs/examples/domain-hipaa.yaml`, `docs/ONTOLOGY-PHASE2-EXAMPLES.md`
    - Acceptance: Documentation is clear, example is runnable
    - Estimated: 1 hour
    - Verification: Manual review

18. **Task 18: Update CLAUDE.md with Phase 2 patterns**
    - Document integration layer API
    - Add Chicago TDD testing examples
    - Document determinism verification patterns
    - Add common error scenarios and fixes
    - Location: `CLAUDE.md` (Phase 2 section)
    - Acceptance: Documentation matches implemented API
    - Estimated: 1.5 hours
    - Verification: Manual review

---

### Day 4-5: Testing, Validation & Andon Signals

**Tasks**:

19. **Task 19: Create comprehensive test suite (Phase 2)**
    - HIPAA scenario test (end-to-end mapping)
    - Determinism test (100 runs, identical output)
    - Error path tests (invalid YAML, missing fields)
    - Performance tests (parse <500ms, map <1s, query gen <1s)
    - Location: `crates/ggen-ontology-core/tests/mapping_integration.rs`
    - Coverage Target: >85% of all Phase 2 code paths
    - Tests: 20+ new tests
    - Acceptance: All tests pass, coverage >85%
    - Estimated: 3 hours
    - Verification: `cargo make test` passes, coverage report

20. **Task 20: Run Andon signal checks (compiler, tests, lint)**
    - `cargo make check` - No compiler errors
    - `cargo make test` - All tests pass
    - `cargo make lint` - No clippy warnings
    - `cargo make slo-check` - Performance within limits
    - Location: All crates involved
    - Acceptance: All signals clear
    - Estimated: 1 hour
    - Verification: All cargo make commands pass

21. **Task 21: Performance benchmarking**
    - Benchmark domain parsing (target: <500ms)
    - Benchmark entity mapping (target: <1s)
    - Benchmark query generation (target: <1s)
    - Benchmark full pipeline (target: <3s)
    - Location: `crates/ggen-ontology-core/benches/mapping_bench.rs`
    - Acceptance: All benchmarks meet SLOs
    - Estimated: 1.5 hours
    - Verification: `cargo make bench` output

22. **Task 22: Final integration and smoke testing**
    - End-to-end CLI testing with example domains
    - Cross-crate integration verification
    - JSON/YAML output format validation
    - Error message quality check
    - Location: Manual testing + CLI tests
    - Acceptance: All scenarios work, error messages helpful
    - Estimated: 1.5 hours
    - Verification: Manual smoke test script

23. **Task 23: Create Phase 2 completion summary**
    - Document deliverables completed
    - Create Phase 2→Phase 3 transition plan
    - Update project roadmap
    - Document known limitations and future work
    - Location: `docs/ONTOLOGY-PHASE2-COMPLETION.md`
    - Acceptance: Summary accurate, readable
    - Estimated: 1 hour
    - Verification: Manual review

---

## Success Metrics

| Metric | Target | Verification |
|--------|--------|--------------|
| Domain parsing | <500ms | `cargo make bench` |
| Entity mapping | <1s | `cargo make bench` |
| Query generation | <1s | `cargo make bench` |
| Full pipeline | <3s | `cargo make bench` |
| Determinism | 100% match | `mapping_determinism.rs` tests |
| Ontology coverage | >90% | Entity matcher tests |
| Test coverage | >85% | Coverage report |
| Compiler errors | 0 | `cargo make check` |
| Test failures | 0 | `cargo make test` |
| Clippy warnings | 0 | `cargo make lint` |

---

## Parallel Execution Strategy

### Week 3 Parallelization

**Day 1-2**: Type system design (can be done sequentially with domain parsing)
- Task 1: Types (1 dev)
- Task 2-3: Domain parsing (1 dev) - depends on Task 1
- Task 4: Type review (1 dev) - parallel with Task 2-3

**Day 3-4**: Entity mapping (independent from domain parsing)
- Task 5: Mapping implementation (1 dev)
- Task 6-7: Mapping tests (1 dev) - parallel with Task 5
- Task 8: Complex matching (1 dev) - parallel with Task 5-6

**Day 5**: Query generation (depends on mapping completion)
- Task 9: Query generation (1 dev)
- Task 10-11: Query validation (1 dev) - parallel with Task 9

### Week 4 Parallelization

**Day 1-2**: Pipeline + CLI
- Task 12: Pipeline orchestration (1 dev)
- Task 13-14: CLI commands (1 dev) - depends on Task 12 completion
- Task 15: Provider interface (1 dev) - independent

**Day 3**: Documentation + Receipt generator
- Task 16: Receipt generator (1 dev)
- Task 17-18: Documentation (1 dev) - independent

**Day 4-5**: Testing + Validation
- Task 19: Test suite (1 dev)
- Task 20-21: Andon signals + benchmarking (1 dev) - can run parallel
- Task 22-23: Smoke testing + summary (1 dev) - depends on other tasks

---

## Risk Mitigation

| Risk | Mitigation | Owner |
|------|-----------|-------|
| Domain parsing fails on edge cases | Create comprehensive parser test suite early | Day 1-2 |
| Entity mapping ambiguity | Build confidence scoring + alternative matches | Day 3-4 |
| Query generation complexity | Keep queries simple, focused on single entity type | Day 5 |
| Determinism violations | Sort consistently at each step, avoid time dependencies | Day 1-5 |
| Performance degradation | Profile early, cache query strings, optimize hot paths | Week 4 |
| CLI integration issues | Test CLI early against working pipeline | Day 1-2 Week 4 |

---

## Daily Stand-up Checkpoints

### Week 3

**Monday (Day 1)**:
- Types defined and compiling
- Domain parsing 50% complete

**Tuesday (Day 2)**:
- Domain parsing complete and tested
- Entity mapping implementation started

**Wednesday (Day 3)**:
- Entity mapping complete and tested
- Query generation 50% complete

**Thursday (Day 4)**:
- Query generation complete and tested
- Determinism verified for all components

**Friday (Day 5)**:
- All Phase 2 core components complete
- Performance benchmarks baseline established

### Week 4

**Monday (Day 1)**:
- Pipeline orchestration complete
- CLI commands 50% implemented

**Tuesday (Day 2)**:
- CLI commands complete
- Provider interface placeholder complete

**Wednesday (Day 3)**:
- Receipt generator complete
- Documentation 80% complete

**Thursday (Day 4)**:
- All tests passing (>60 tests)
- Andon signals clear (check, test, lint)

**Friday (Day 5)**:
- Smoke testing complete
- Phase 2 summary complete
- Ready for Phase 3 kickoff

