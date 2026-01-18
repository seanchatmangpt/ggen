# ggen v5.1.0 - Customized Skills Framework

**Source**: Adapted from `~/ggen-spec-kit/CLAUDE.md` (13 base skills)
**Project**: ggen - Language-agnostic deterministic code generation CLI
**Status**: v5.1.0 GA Production Release (013-ga-production-release)
**Date**: 2025-12-21

---

## Skills Customized for ggen v5.1.0

### Core ggen Skills (7 - Primary Focus)

#### 1. **sync-executor** ⭐ (PRIMARY)
- **Triggers**: `ggen sync`, audit trail, force flag, merge mode, watch mode, conditional execution
- **Purpose**: Execute sync pipeline with all CLI features
- **ggen Context**: Implements SyncExecutor, generation rules, file writing
- **Related To**: Feature #013 Phase 1-2 implementation
- **Test Focus**: Chicago TDD with AAA pattern verifying observable state changes

#### 2. **audit-trail-writer**
- **Triggers**: `--audit` flag, JSON serialization, execution metadata
- **Purpose**: Record deterministic execution history
- **ggen Context**: Implements AuditTrail struct, timestamp tracking, file hashing
- **Related To**: Feature #013 Task 1.1
- **Test Focus**: audit.json creation, structure validation, content hashing

#### 3. **merge-mode-handler**
- **Triggers**: Merge markers, git-style conflict detection, manual code preservation
- **Purpose**: Implement three-way merge with git markers
- **ggen Context**: Regex-based marker detection, manual section extraction, content injection
- **Related To**: Feature #013 Task 2.1
- **Test Focus**: Marker preservation, generated section updates, malformed handling

#### 4. **watch-mode-monitor**
- **Triggers**: `--watch` flag, file changes, debounce, event queue
- **Purpose**: Monitor files and auto-regenerate with debouncing
- **ggen Context**: notify crate integration, 300ms debounce, bounded queue (10 items)
- **Related To**: Feature #013 Task 2.2
- **Test Focus**: Change detection, debounce timing, queue management

#### 5. **conditional-executor**
- **Triggers**: `when` clause, SPARQL ASK queries, conditional rule execution
- **Purpose**: Data-driven rule filtering based on ontology state
- **ggen Context**: Execute ASK query pre-rule, skip if false, log decisions
- **Related To**: Feature #013 Task 2.3
- **Test Focus**: Query evaluation, rule skipping, missing when field handling

#### 6. **validation-pipeline**
- **Triggers**: SHACL shapes, SPARQL validation rules, severity levels
- **Purpose**: Two-stage validation (pre-generation SHACL, post-generation SPARQL)
- **ggen Context**: Shape execution, rule post-processing, fail-fast on severity=error
- **Related To**: Feature #013 Task 4.1-4.2
- **Test Focus**: Shape violations, rule execution, severity handling

#### 7. **performance-benchmarker**
- **Triggers**: SLO verification, 100-rule manifests, 10,000-triple ontologies
- **Purpose**: Benchmark sync pipeline against performance targets
- **ggen Context**: 90th percentile ≤5s, 95th percentile ≤10s targets
- **Related To**: Feature #013 Task 4.4
- **Test Focus**: Benchmark scenarios, regression detection, optimization validation

---

### Supporting Skills (6 - Quality & Architecture)

#### 8. **rdf-sync-validator** (RDF Domain)
- **Triggers**: Turtle syntax, SHACL shapes, RDF validation pre-sync
- **Purpose**: Validate RDF/TTL syntax before generation
- **ggen Context**: Validates feature.ttl, ontologies, manifest SHACL
- **Related To**: Feature spec validation, plan validation

#### 9. **rust-executor** (Implementation Domain)
- **Triggers**: Cargo make, compilation, test execution
- **Purpose**: Execute Rust build, test, lint commands with proper timeouts
- **ggen Context**: `cargo make check`, `cargo make test`, `cargo make lint`
- **Related To**: All phases - CI/CD pipeline integration

#### 10. **chicago-tdd-implementer** (Testing Domain)
- **Triggers**: Unit tests, integration tests, behavior verification
- **Purpose**: Implement tests using AAA (Arrange-Act-Assert) pattern
- **ggen Context**: Verify observable state, file writes, audit trail, merge operations
- **Related To**: Feature #013 Phase 3 (comprehensive testing)

#### 11. **architecture-validator** (QA Domain)
- **Triggers**: Three-tier compliance, layer boundaries, ggen architecture
- **Purpose**: Validate crate structure (codegen, cli, core, domain)
- **ggen Context**: Verify SyncExecutor placement, feature organization, module structure
- **Related To**: Production hardening goals

#### 12. **specification-writer** (RDF Domain)
- **Triggers**: RDF/Turtle specifications, feature.ttl, plan.ttl, tasks.ttl
- **Purpose**: Create/modify RDF-based specifications
- **ggen Context**: Constitutional equation `spec.md = μ(feature.ttl)`
- **Related To**: Feature #013 specification and planning work

#### 13. **documentation-generator** (Content Domain)
- **Triggers**: Markdown generation, docs from RDF, help text, examples
- **Purpose**: Generate user and developer documentation
- **ggen Context**: CLI help text, sync guide, merge mode docs, validation guide
- **Related To**: Feature #013 Task 5.1-5.2

---

## Skill Grouping by ggen Context

### Phase 1: Critical Fixes Skills (P0)
- **audit-trail-writer** - Write audit.json ✓
- **sync-executor** - Basic sync with flags ✓
- **rust-executor** - Build & test ✓
- **chicago-tdd-implementer** - Test scaffold ✓

### Phase 2: Missing Features Skills (P1)
- **merge-mode-handler** - Git-style merge ✓
- **watch-mode-monitor** - File monitoring ✓
- **conditional-executor** - When clauses ✓
- **chicago-tdd-implementer** - Feature tests ✓

### Phase 3: Testing Skills (P1)
- **chicago-tdd-implementer** - Comprehensive test suite ✓
- **rust-executor** - Test execution & coverage ✓
- **performance-benchmarker** - SLO benchmarks ✓

### Phase 4: Hardening Skills (P2)
- **validation-pipeline** - SHACL + SPARQL validation ✓
- **rdf-sync-validator** - RDF validation ✓
- **architecture-validator** - Layer compliance ✓
- **performance-benchmarker** - Performance optimization ✓

### Phase 5: Release Skills (P3)
- **documentation-generator** - User docs & help text ✓
- **specification-writer** - Update specs ✓
- **rust-executor** - Final testing & release ✓

---

## Skill Activation Triggers for ggen v5.1.0

| Trigger Pattern | Activated Skills | Context |
|-----------------|-----------------|---------|
| `--audit` flag, audit.json | `audit-trail-writer`, `sync-executor` | Task 1.1 |
| `--force` flag | `sync-executor` | Task 1.2 |
| `<<<<<<< GENERATED` marker | `merge-mode-handler`, `chicago-tdd-implementer` | Task 2.1 |
| `--watch` flag, file changes | `watch-mode-monitor`, `rust-executor` | Task 2.2 |
| `when = "ASK"` clause | `conditional-executor`, `rdf-sync-validator` | Task 2.3 |
| SHACL, SPARQL validation | `validation-pipeline`, `rdf-sync-validator` | Task 4.1-4.2 |
| Performance SLO | `performance-benchmarker`, `rust-executor` | Task 4.4 |
| Documentation, help text | `documentation-generator`, `specification-writer` | Task 5.1-5.2 |
| Code review, architecture | `architecture-validator`, `chicago-tdd-implementer` | All phases |
| Compilation, tests | `rust-executor`, `chicago-tdd-implementer` | All phases |

---

## Constitutional Equation Support

**Equation**: `spec.md = μ(feature.ttl)` for ggen v5.1.0

**Transformation Pipeline**:

1. **Input**: `feature.ttl` (RDF specification)
   - Skills: `specification-writer`, `rdf-sync-validator`

2. **Normalize**: Validate RDF syntax & SHACL shapes
   - Skill: `rdf-sync-validator`

3. **Extract**: Execute SPARQL queries, analyze requirements
   - Skills: `specification-writer`, `conditional-executor` (ASK evaluation)

4. **Emit**: Render Tera templates to markdown
   - Skill: `documentation-generator`

5. **Canonicalize**: Format output with consistent styling
   - Skill: `documentation-generator`

6. **Receipt**: SHA256 hash proof of specification
   - Skill: `rust-executor` (verify hash)

7. **Output**: `spec.md` (markdown artifact)
   - Skills: `architecture-validator`, `chicago-tdd-implementer`

---

## Implementation SLOs with Skills

| SLO | Verification Skill | Target |
|----|-------------------|--------|
| First build | `rust-executor` | ≤15s |
| Incremental build | `rust-executor` | ≤2s |
| RDF processing | `rdf-sync-validator` | ≤5s (1k+ triples) |
| Sync execution | `sync-executor`, `performance-benchmarker` | ≤5s (100 rules) |
| Test suite | `rust-executor`, `chicago-tdd-implementer` | 100% pass |
| Code coverage | `chicago-tdd-implementer` | ≥95% |
| Help examples | `documentation-generator` | 100% executable |

---

## Three-Tier Architecture Validation (ggen)

```
Commands Layer (CLI)
├── Skills: architecture-validator, documentation-generator
├── Responsibilities: Parse args, format output
└── Validates: No subprocess calls in this layer

Operations Layer (Business Logic)
├── Skills: sync-executor, merge-mode-handler, conditional-executor
├── Responsibilities: Pure logic (audit trail, merge, validation)
└── Validates: No file I/O or subprocess in this layer

Runtime Layer (Subprocess + I/O)
├── Skills: rust-executor, performance-benchmarker
├── Responsibilities: File I/O, subprocess execution
└── Validates: No imports from commands/ops in this layer
```

---

## Andon Signal Monitoring (ggen)

**RED Signals** (STOP - Fix Immediately):
- `error[E...]` from rustc compilation
- `test ... FAILED` from `cargo make test`
- Coverage < 95% for codegen/
- Performance > SLOs (90th > 5s, 95th > 10s)

**YELLOW Signals** (Investigate Before Release):
- `warning:` from rustc
- Clippy lint violations
- Flaky tests (intermittent failures)
- Unused code paths

**GREEN Signals** (Continue):
- Clean compilation
- All tests passing
- Coverage ≥ 95%
- Performance within SLOs

---

## Quick Reference: Skill Selection for Tasks

### For Task 1.1: Audit Trail
→ Use: `audit-trail-writer`, `sync-executor`, `chicago-tdd-implementer`

### For Task 1.2: Force Flag
→ Use: `sync-executor`, `rust-executor`, `chicago-tdd-implementer`

### For Task 1.3: Test Scaffold
→ Use: `chicago-tdd-implementer`, `rust-executor`

### For Task 2.1: Merge Mode
→ Use: `merge-mode-handler`, `chicago-tdd-implementer`, `rust-executor`

### For Task 2.2: Watch Mode
→ Use: `watch-mode-monitor`, `rust-executor`, `chicago-tdd-implementer`

### For Task 2.3: Conditional Execution
→ Use: `conditional-executor`, `rdf-sync-validator`, `chicago-tdd-implementer`

### For Phase 3: Comprehensive Testing
→ Use: `chicago-tdd-implementer`, `rust-executor`, `performance-benchmarker`

### For Task 4.1-4.2: Validation Pipeline
→ Use: `validation-pipeline`, `rdf-sync-validator`, `chicago-tdd-implementer`

### For Task 4.4: Performance
→ Use: `performance-benchmarker`, `rust-executor`

### For Task 5.1-5.2: Documentation & Release
→ Use: `documentation-generator`, `specification-writer`, `rust-executor`

---

## Skill Implementation Checklist

- [ ] **sync-executor**: Modify SyncExecutor, integrate audit/merge/watch/conditional
- [ ] **audit-trail-writer**: Create audit module with JSON serialization
- [ ] **merge-mode-handler**: Implement regex-based merge with markers
- [ ] **watch-mode-monitor**: Add notify crate integration with debounce
- [ ] **conditional-executor**: Implement SPARQL ASK pre-execution checks
- [ ] **validation-pipeline**: Add SHACL pre-gen and SPARQL post-gen validation
- [ ] **performance-benchmarker**: Create benches/ with SLO verification
- [ ] **chicago-tdd-implementer**: Write comprehensive AAA-pattern tests (95%+ coverage)
- [ ] **rust-executor**: Verify cargo make integration (check, test, lint)
- [ ] **rdf-sync-validator**: Validate feature.ttl before implementation
- [ ] **architecture-validator**: Verify three-tier layer boundaries
- [ ] **documentation-generator**: Create sync guide, merge docs, validation guide
- [ ] **specification-writer**: Maintain feature.ttl and plan.ttl

---

## Integration with Feature #013

**Feature**: v5.1.0 GA Production Release
**Branch**: `013-ga-production-release`
**RDF Source**: `.specify/specs/013-ga-production-release/feature.ttl` and `plan.ttl`

**Skill Execution Order**:
1. `specification-writer` + `rdf-sync-validator` → Validate specifications
2. `sync-executor` + `chicago-tdd-implementer` → Implement features (Phase 1-2)
3. `chicago-tdd-implementer` + `rust-executor` → Comprehensive testing (Phase 3)
4. `validation-pipeline` + `performance-benchmarker` → Hardening (Phase 4)
5. `documentation-generator` + `specification-writer` → Release (Phase 5)
6. `architecture-validator` + `rust-executor` → Final validation

---

**Last Updated**: 2025-12-21
**Maintainer**: Claude Code v2026 Edition
**Reference**: Feature #013 Implementation Plan
**Status**: Skills defined and ready for Phase 1 implementation
