# Module Test Checklist - Detailed Coverage Map

**Purpose**: Track test coverage for each source file
**Usage**: Check ✅ when module reaches 95% coverage
**Last Updated**: 2025-11-18

---

## 🚨 P0: Compilation Blockers (Fix Immediately)

### ggen-marketplace
- [ ] **Fix Send trait violations** in `registry_rdf.rs:165, 196`
- [ ] **Update deprecated API** - Replace `Store::query()` with `SparqlEvaluator`
- [ ] **Verify compilation** - `cargo build --lib` succeeds
- [ ] **Run existing tests** - `cargo test --lib` passes

---

## 🔴 P1: Critical Modules (Days 1-3, Target: 95%+)

### lifecycle/ (26 files)

#### ✅ Already Tested
- [x] `behavior_tests.rs` - 695 LOC (test file)
- [x] `integration_test.rs` - 758 LOC (test file)
- [x] `poka_yoke_tests.rs` - 158 LOC (test file)
- [x] `poka_yoke_runtime_tests.rs` - 221 LOC (test file)

#### ❌ Needs Tests (CRITICAL)

**`optimization.rs` (433 LOC) - 0% → 95%**
- [ ] ContainerPool
  - [ ] `new()` - initialization
  - [ ] `get_container()` - pool management
  - [ ] `return_container()` - resource reuse
  - [ ] Concurrent access (10 threads)
  - [ ] Pool exhaustion handling
  - [ ] Container health checks
- [ ] DependencyCache
  - [ ] `new()` - initialization
  - [ ] `get()` - cache hits
  - [ ] `insert()` - cache updates
  - [ ] `invalidate()` - cache clearing
  - [ ] LRU eviction
  - [ ] Thread-safe access
- [ ] ParallelOrchestrator
  - [ ] `new()` - setup
  - [ ] `run_parallel()` - parallel execution
  - [ ] Dependency ordering
  - [ ] Error propagation
  - [ ] Resource limits
  - [ ] Graceful degradation
- [ ] PipelineProfiler
  - [ ] `start_profiling()` - begin tracking
  - [ ] `record_stage()` - stage timing
  - [ ] `finish_profiling()` - report generation
  - [ ] Nested stage tracking
  - [ ] Memory profiling
  - [ ] Export to flamegraph

**`production.rs` (1,089 LOC) - <10% → 95%**
- [ ] ReadinessTracker
  - [ ] `new()` - initialization
  - [ ] `add_requirement()` - add checks
  - [ ] `check_readiness()` - validation
  - [ ] `generate_report()` - reporting
  - [ ] Category-based filtering
  - [ ] Severity levels
  - [ ] Custom validators
- [ ] PlaceholderProcessor
  - [ ] `process_file()` - placeholder detection
  - [ ] `replace_placeholders()` - substitution
  - [ ] `validate_placeholders()` - validation
  - [ ] Nested placeholders
  - [ ] Missing placeholder errors
  - [ ] Custom placeholder formats
- [ ] ReadinessReport
  - [ ] `new()` - creation
  - [ ] `add_issue()` - issue tracking
  - [ ] `is_ready()` - status check
  - [ ] `to_markdown()` - formatting
  - [ ] `to_json()` - JSON export
  - [ ] Filtering by category/severity
- [ ] Placeholder patterns
  - [ ] TODO detection
  - [ ] FIXME detection
  - [ ] XXX detection
  - [ ] Custom patterns
  - [ ] Regex-based detection

**`state_validation.rs` (215 LOC) - 0% → 95%**
- [ ] ValidatedLifecycleState
  - [ ] `validate()` - state validation
  - [ ] `new()` - creation with checks
  - [ ] Invalid state rejection
  - [ ] State transition rules
  - [ ] Validation errors
- [ ] StateValidationError
  - [ ] Error types
  - [ ] Error messages
  - [ ] Error context

**`template_phase.rs` (169 LOC) - 0% → 95%**
- [ ] execute_template_phase
  - [ ] Basic execution
  - [ ] Template variables
  - [ ] Output verification
  - [ ] Error handling
- [ ] register_template_phase
  - [ ] Phase registration
  - [ ] Hook integration
  - [ ] Configuration

---

### ontology/ (9 files)

**`sigma_runtime.rs` (709 LOC) - 0% → 95%**
- [ ] SigmaRuntime
  - [ ] `new()` - initialization
  - [ ] `create_snapshot()` - snapshot creation
  - [ ] `apply_overlay()` - overlay application
  - [ ] `commit_snapshot()` - commit operation
  - [ ] `rollback_snapshot()` - rollback
  - [ ] Concurrent snapshots
  - [ ] Snapshot garbage collection
- [ ] SigmaSnapshot
  - [ ] `new()` - creation
  - [ ] `id()` - ID generation
  - [ ] `metadata()` - metadata access
  - [ ] `parent()` - parent tracking
  - [ ] Snapshot chain validation
- [ ] SigmaOverlay
  - [ ] `new()` - creation
  - [ ] `apply_to()` - application
  - [ ] `validate()` - validation
  - [ ] Overlay conflicts
  - [ ] Merge strategies
- [ ] SigmaReceipt
  - [ ] `new()` - generation
  - [ ] `verify()` - verification
  - [ ] Cryptographic signatures
  - [ ] Tamper detection

**`control_loop.rs` (390 LOC) - 0% → 95%**
- [ ] AutonomousControlLoop
  - [ ] `new()` - initialization
  - [ ] `observe()` - observation intake
  - [ ] `analyze()` - analysis phase
  - [ ] `decide()` - decision making
  - [ ] `act()` - action execution
  - [ ] `run()` - full loop
  - [ ] Loop termination
- [ ] Observation
  - [ ] `new()` - creation
  - [ ] `source()` - source tracking
  - [ ] `timestamp()` - timing
  - [ ] `data()` - data access
- [ ] IterationTelemetry
  - [ ] Metric collection
  - [ ] Performance tracking
  - [ ] Loop statistics

**`constitution.rs` (305 LOC) - 0% → 95%**
- [ ] Constitution
  - [ ] `new()` - creation
  - [ ] `add_invariant()` - add rule
  - [ ] `validate()` - full validation
  - [ ] `check_invariant()` - single check
  - [ ] Invariant ordering
  - [ ] Validation caching
- [ ] Invariant checks
  - [ ] TypeSoundnessCheck
  - [ ] ImmutabilityCheck
  - [ ] NoRetrocausationCheck
  - [ ] ProjectionDeterminismCheck
  - [ ] SLOPreservationCheck
  - [ ] AtomicPromotionCheck
  - [ ] GuardSoundnessCheck
- [ ] InvariantResult
  - [ ] Success cases
  - [ ] Failure cases
  - [ ] Evidence tracking

**`validators.rs` (512 LOC) - 0% → 95%**
- [ ] StaticValidator
  - [ ] `validate()` - static analysis
  - [ ] Type checking
  - [ ] Syntax validation
  - [ ] Schema conformance
- [ ] DynamicValidator
  - [ ] `validate()` - runtime checks
  - [ ] Constraint validation
  - [ ] Business rules
  - [ ] Performance bounds
- [ ] PerformanceValidator
  - [ ] `validate()` - performance checks
  - [ ] SLO compliance
  - [ ] Resource limits
  - [ ] Throughput validation
- [ ] CompositeValidator
  - [ ] Multi-validator orchestration
  - [ ] Validation ordering
  - [ ] Early termination
  - [ ] Error aggregation

**`pattern_miner.rs` (601 LOC) - 0% → 95%**
- [ ] PatternMiner
  - [ ] `new()` - initialization
  - [ ] `mine_patterns()` - pattern detection
  - [ ] `classify_pattern()` - classification
  - [ ] `store_pattern()` - storage
  - [ ] Pattern frequency tracking
  - [ ] Pattern evolution
- [ ] Pattern types
  - [ ] Structural patterns
  - [ ] Behavioral patterns
  - [ ] Performance patterns
  - [ ] Anti-patterns
- [ ] Pattern detection
  - [ ] Frequency analysis
  - [ ] Co-occurrence analysis
  - [ ] Temporal patterns
  - [ ] Spatial patterns

**`delta_proposer.rs` (374 LOC) - 0% → 95%**
- [ ] DeltaSigmaProposer
  - [ ] `new()` - initialization
  - [ ] `propose_delta()` - proposal generation
  - [ ] `validate_proposal()` - validation
  - [ ] `apply_proposal()` - application
- [ ] ProposedChange
  - [ ] Change validation
  - [ ] Impact analysis
  - [ ] Rollback generation

**`promotion.rs` (236 LOC) - 0% → 95%**
- [ ] AtomicSnapshotPromoter
  - [ ] `promote()` - snapshot promotion
  - [ ] `validate_promotion()` - checks
  - [ ] `rollback_promotion()` - reversal
  - [ ] Atomic guarantees
  - [ ] Promotion metrics

**`e2e_example.rs` (342 LOC) - 0% → 95%**
- [ ] End-to-end scenarios
  - [ ] Complete workflow
  - [ ] Error scenarios
  - [ ] Performance scenarios
  - [ ] Integration points

---

## 🟡 P2: High Priority (Days 4-6, Target: 90%+)

### graph/ (10 files)

#### ✅ Already Tested
- [x] `core_fs_tests.rs` - 178 LOC
- [x] `export_tests.rs` - 178 LOC
- [x] `store_tests.rs` - 186 LOC

#### ❌ Needs Additional Tests

**`core.rs` (818 LOC) - ~30% → 90%**
- [ ] Graph::new() - initialization edge cases
- [ ] Graph::query() - complex SPARQL
- [ ] Graph::insert_turtle() - malformed input
- [ ] Graph::insert_ntriples() - edge cases
- [ ] Concurrent modifications (10 threads)
- [ ] Large graph handling (1M+ triples)
- [ ] Error recovery scenarios
- [ ] Memory management
- [ ] Cache coherence

**`query.rs` (226 LOC) - 0% → 90%**
- [ ] SPARQL parser
  - [ ] SELECT queries
  - [ ] CONSTRUCT queries
  - [ ] ASK queries
  - [ ] DESCRIBE queries
  - [ ] Malformed queries
- [ ] Query optimization
  - [ ] Join reordering
  - [ ] Filter pushdown
  - [ ] Index usage
- [ ] Result caching
  - [ ] Cache hits
  - [ ] Cache invalidation
  - [ ] Cache eviction

**`update.rs` (349 LOC) - 0% → 90%**
- [ ] INSERT DATA
  - [ ] Single triple
  - [ ] Multiple triples
  - [ ] Named graphs
  - [ ] Blank nodes
- [ ] DELETE WHERE
  - [ ] Simple patterns
  - [ ] Complex patterns
  - [ ] Optional clauses
- [ ] Transaction handling
  - [ ] Commit
  - [ ] Rollback
  - [ ] Nested transactions
  - [ ] Concurrent updates

**`export.rs` (420 LOC) - ~20% → 90%**
- [ ] Additional export formats
- [ ] Large graph exports
- [ ] Streaming exports
- [ ] Error handling

**`store.rs` (189 LOC) - ~30% → 90%**
- [ ] Store operations
- [ ] Persistence
- [ ] Backup/restore

**`types.rs` (193 LOC) - ~40% → 90%**
- [ ] Type conversions
- [ ] Edge cases

---

### templates/ (7 files)

**`file_tree_generator.rs` (753 LOC) - ~20% → 90%**
- [ ] Nested directories (10+ levels)
- [ ] File permissions
  - [ ] Executable files
  - [ ] Read-only files
  - [ ] Special permissions
- [ ] Symlinks
  - [ ] Absolute symlinks
  - [ ] Relative symlinks
  - [ ] Broken symlinks
  - [ ] Circular symlinks
- [ ] Large trees (1000+ files)
- [ ] Concurrent generation
- [ ] Error recovery
- [ ] Platform-specific paths (Windows, Unix)

**`format.rs` (718 LOC) - 0% → 90%**
- [ ] TemplateFormat parsing
  - [ ] JSON format
  - [ ] YAML format
  - [ ] TOML format
  - [ ] Custom formats
- [ ] Format validation
  - [ ] Schema validation
  - [ ] Syntax checking
  - [ ] Required fields
- [ ] Format conversion
  - [ ] JSON ↔ YAML
  - [ ] YAML ↔ TOML
  - [ ] Lossy conversions
  - [ ] Encoding handling

**`business_logic.rs` (450 LOC) - 0% → 90%**
- [ ] Rule execution
  - [ ] Conditional rules
  - [ ] Validation rules
  - [ ] Transformation rules
- [ ] Rule composition
  - [ ] Rule chaining
  - [ ] Rule conflicts
  - [ ] Rule priorities
- [ ] Error handling
  - [ ] Validation failures
  - [ ] Execution errors
  - [ ] Rollback scenarios

**`context.rs` (672 LOC) - ~25% → 90%**
- [ ] Context building
- [ ] Variable resolution
- [ ] Context inheritance

**`generator.rs` (518 LOC) - ~30% → 90%**
- [ ] Template generation
- [ ] Output validation

**`frozen.rs` (501 LOC) - 0% → 90%**
- [ ] Immutable templates
- [ ] Frozen state validation

---

### rdf/ (5 files)

**`schema.rs` (721 LOC) - ~15% → 90%**
- [ ] Schema validation
  - [ ] Class hierarchy
  - [ ] Property domains/ranges
  - [ ] Cardinality constraints
  - [ ] Value constraints
- [ ] SHACL constraints
  - [ ] Node shapes
  - [ ] Property shapes
  - [ ] Logical constraints
  - [ ] Validation reports
- [ ] Namespace management
  - [ ] Prefix registration
  - [ ] URI resolution
  - [ ] Namespace conflicts

**`template_metadata.rs` (716 LOC) - <10% → 90%**
- [ ] Metadata extraction
  - [ ] Template variables
  - [ ] Dependencies
  - [ ] Version information
  - [ ] Author information
- [ ] Version comparison
  - [ ] Semantic versioning
  - [ ] Version ranges
  - [ ] Compatibility checks
- [ ] Dependency resolution
  - [ ] Transitive dependencies
  - [ ] Circular dependencies
  - [ ] Version conflicts
  - [ ] Optional dependencies

**`validation.rs` (668 LOC) - <10% → 90%**
- [ ] ValidationReport generation
  - [ ] Error collection
  - [ ] Warning collection
  - [ ] Info messages
- [ ] Multi-level validation
  - [ ] Syntax level
  - [ ] Semantic level
  - [ ] Business logic level
- [ ] Error aggregation
  - [ ] Error grouping
  - [ ] Error prioritization
  - [ ] Error formatting

---

### marketplace (ggen-marketplace)

**After fixing compilation**, test:

**`rdf_mapper.rs` (644 LOC) - 0% → 90%**
- [ ] RDF to Package mapping
- [ ] Package to RDF mapping
- [ ] Dependency resolution
- [ ] Version handling

**`registry_rdf.rs` (270 LOC) - 0% → 90%**
- [ ] Registry queries (AFTER Send fix)
- [ ] Package retrieval
- [ ] Search operations
- [ ] Batch operations

**`search_sparql.rs` (215 LOC) - 0% → 90%**
- [ ] Search query construction
- [ ] Result ranking
- [ ] Faceted search
- [ ] Full-text search

**`v3.rs` (308 LOC) - 0% → 90%**
- [ ] V3 API features
- [ ] Backward compatibility
- [ ] Migration scenarios

---

## 🟢 P3: Medium Priority (Days 7-9, Target: 85%+)

### Core Utilities

**`generator.rs` (705 LOC) - ~20% → 85%**
- [ ] Error handling paths
- [ ] Streaming generation
- [ ] Resource cleanup
- [ ] Timeout handling

**`pipeline.rs` (908 LOC) - ~25% → 85%**
- [ ] Pipeline builder
- [ ] Stage execution
- [ ] Error propagation
- [ ] Stage dependencies

**`register.rs` (906 LOC) - ~30% → 85%**
- [ ] Tera filter registration
  - [ ] All builtin filters
  - [ ] Custom filters
  - [ ] Filter errors
- [ ] Tera function registration
  - [ ] All builtin functions
  - [ ] Custom functions
  - [ ] Function errors
- [ ] Template rendering
  - [ ] Variable substitution
  - [ ] Nested templates
  - [ ] Include mechanism

**`registry.rs` (980 LOC) - ~15% → 85%**
- [ ] Registry client
  - [ ] Authentication
  - [ ] API calls
  - [ ] Rate limiting
  - [ ] Retry logic
- [ ] Search operations
  - [ ] Text search
  - [ ] Tag search
  - [ ] Version filters
- [ ] Cache management
  - [ ] Local cache
  - [ ] Cache invalidation
  - [ ] Cache updates

**`resolver.rs` (789 LOC) - ~10% → 85%**
- [ ] Template resolution
  - [ ] Local templates
  - [ ] Remote templates
  - [ ] Pack templates
  - [ ] Git templates
- [ ] Dependency resolution
  - [ ] Version resolution
  - [ ] Conflict resolution
  - [ ] Circular detection

**`snapshot.rs` (686 LOC) - ~20% → 85%**
- [ ] Snapshot creation
- [ ] Snapshot diff
- [ ] Region management
- [ ] Snapshot restoration

**`delta.rs` (916 LOC) - ~15% → 85%**
- [ ] Delta generation
- [ ] Impact analysis
- [ ] Delta application
- [ ] Conflict detection

**`merge.rs` (849 LOC) - ~10% → 85%**
- [ ] Three-way merge
- [ ] Conflict resolution strategies
  - [ ] Ours strategy
  - [ ] Theirs strategy
  - [ ] Union strategy
  - [ ] Custom strategies
- [ ] Region-aware merging
- [ ] Merge conflict reporting

**`lockfile.rs` (451 LOC) - ~40% → 85%**
- [ ] Concurrent lockfile updates
- [ ] Lock corruption recovery
- [ ] Version migration

**`cache.rs` (612 LOC) - ~25% → 85%**
- [ ] Cache eviction policies
- [ ] Cache statistics
- [ ] Cache warming

**`github.rs` (489 LOC) - ~5% → 85%**
- [ ] GitHub API integration
  - [ ] Repository operations
  - [ ] Release management
  - [ ] Workflow runs
  - [ ] Pages deployment
- [ ] Error handling
- [ ] Rate limiting
- [ ] Authentication

**`gpack.rs` (559 LOC) - ~30% → 85%**
- [ ] Manifest validation
- [ ] File discovery
- [ ] Pack building

**`inject.rs` (422 LOC) - ~15% → 85%**
- [ ] File injection
- [ ] Content merging
- [ ] Error recovery

**`preprocessor.rs` (467 LOC) - ~10% → 85%**
- [ ] Preprocessing stages
- [ ] Macro expansion
- [ ] Variable substitution

---

### CLI Module (ggen-cli)

**Command Testing**:
- [ ] `ggen new` - Project creation
- [ ] `ggen pack` - Pack operations
- [ ] `ggen install` - Pack installation
- [ ] `ggen search` - Template search
- [ ] `ggen list` - List installed packs
- [ ] `ggen update` - Update packs
- [ ] `ggen remove` - Remove packs
- [ ] `ggen info` - Show pack info
- [ ] `ggen lifecycle` - Lifecycle commands

**Output Testing**:
- [ ] JSON output format
- [ ] Table output format
- [ ] Quiet mode
- [ ] Verbose mode
- [ ] Color handling

**Error Testing**:
- [ ] Invalid commands
- [ ] Missing arguments
- [ ] Configuration errors
- [ ] Permission errors

---

## ⚪ P4: Specialized Tests (Days 10-12, Target: 70-90%)

### Performance Tests (250 tests)

**Graph Operations**:
- [ ] Query performance (1K, 10K, 100K triples)
- [ ] Insert performance (bulk operations)
- [ ] Update performance (batch updates)
- [ ] Export performance (large graphs)

**Template Generation**:
- [ ] Small templates (<10 files)
- [ ] Medium templates (10-100 files)
- [ ] Large templates (100-1000 files)
- [ ] Nested templates (10+ levels)

**Parallel Execution**:
- [ ] 2 concurrent operations
- [ ] 10 concurrent operations
- [ ] 100 concurrent operations
- [ ] Resource contention

**Memory Profiling**:
- [ ] Memory usage baseline
- [ ] Memory leaks detection
- [ ] Large dataset handling
- [ ] Garbage collection impact

**Benchmarks**:
- [ ] Critical path benchmarks
- [ ] Regression detection
- [ ] Optimization validation

---

### Security Tests (170 tests)

**Input Validation**:
- [ ] SQL injection attempts
- [ ] XSS attempts
- [ ] Path traversal attempts
- [ ] Command injection
- [ ] LDAP injection
- [ ] XML injection

**Authentication**:
- [ ] Token validation
- [ ] Token expiration
- [ ] Token revocation
- [ ] Session management

**Authorization**:
- [ ] Permission checks
- [ ] Role-based access
- [ ] Resource ownership
- [ ] Privilege escalation

**Cryptography**:
- [ ] PQC signing
- [ ] PQC verification
- [ ] Hash function usage
- [ ] Secure random generation
- [ ] Key management

**File Handling**:
- [ ] Path sanitization
- [ ] File upload validation
- [ ] Symlink attacks
- [ ] Directory traversal

**Dependencies**:
- [ ] Known vulnerability scanning
- [ ] Outdated dependency detection
- [ ] License compliance

---

## 📈 Coverage Tracking

### Overall Targets
```
Current:  47% (63,956 test LOC / 135,823 source LOC)
Target:   95% (131,031 test LOC / 135,823 source LOC)
Gap:      +1,565 tests (~67,000 LOC)
```

### By Priority
```
P0: Compilation fixes (blocker)
P1: 95%+ coverage (critical modules)
P2: 90%+ coverage (high priority)
P3: 85%+ coverage (medium priority)
P4: 70-90% coverage (specialized)
```

### Daily Progress Log
```
Day 1:  ☐ Fix compilation
Day 2:  ☐ 300 tests (lifecycle/ontology)
Day 3:  ☐ Reach 60% overall
Day 6:  ☐ 700 tests (graph/templates/rdf/marketplace)
Day 9:  ☐ Reach 85% overall
Day 12: ☐ 1,565 tests, 95% overall
```

---

## 🎯 Completion Criteria

For each module to be marked complete (✅):

1. **Coverage**: Minimum coverage target met
2. **Quality**: All tests follow templates/patterns
3. **Documentation**: Test purpose and scenarios documented
4. **Performance**: Tests complete in reasonable time
5. **Reliability**: No flaky tests
6. **Maintainability**: Clear, readable test code

---

**Usage**: Mark checkbox when module reaches target coverage. Run `cargo tarpaulin` to verify.
