# Marketplace Gpack - Implementation Planning Report

**Report Date**: 2025-12-21 16:30 UTC
**Phase**: Architecture & Implementation Planning (Phase 1)
**Status**: ✅ COMPLETE - READY FOR TASK BREAKDOWN
**Branch**: `014-marketplace-gpack`

---

## Overview

The comprehensive implementation plan for the ggen marketplace gpack retrofit has been defined using RDF-first architecture. This plan bridges the specification (what to build) with executable tasks (how to build it).

**Key Outcome**: Clear, detailed architecture and implementation strategy for 10-agent parallel execution.

---

## Architecture Summary

### Architecture Pattern
**Layered Architecture with Command Pattern**

The implementation is organized across 4 major layers:

1. **Domain Layer** (`crates/ggen-marketplace/src/gpack/`)
   - Core gpack format and manifest handling
   - Crates.io API client integration
   - Dependency resolver engine
   - Validation framework
   - Multi-layer caching system

2. **CLI Layer** (`crates/ggen-cli/src/commands/marketplace/`)
   - User-facing commands (publish, install, search, list, update)
   - Command parsing and execution
   - User-friendly error messaging
   - Integration with existing ggen CLI

3. **Test Layer** (`crates/ggen-core/tests/ + crates/ggen-marketplace/tests/`)
   - Chicago TDD patterns (AAA: Arrange-Act-Assert)
   - Unit, integration, and e2e tests
   - Cross-platform validation
   - Performance benchmarks

4. **Documentation Layer** (`docs/features/marketplace-gpack.md`)
   - User guides and API references
   - Migration guide for 84 packages
   - Troubleshooting and FAQ
   - Code examples

---

## Technology Stack (13 Technologies)

### Core Technologies (Existing)
- **Rust 1.75+** (Edition 2021) - Type-safe implementation language
- **Oxigraph 0.3+** - RDF triplestore for semantic search
- **Tera 1.19+** - Template generation for Cargo.toml
- **Tracing + OpenTelemetry** - Structured logging and distributed tracing
- **Ed25519 Signatures** - Package authentication (from marketplace v5.0.2)

### Integration Technologies (New)
- **Crates.io HTTP API** - Standard Rust registry for publish/download
- **Tokio 1.35+** - Async runtime for concurrent operations
- **Reqwest 0.11+** - HTTP client for crates.io API
- **Serde 1.0+** - Serialization (JSON/TOML/YAML)

### Supporting Technologies
- **SHA2 0.10+** - Cryptographic hashing for integrity
- **Tempfile 3.8+** - Safe temporary directory management
- **YAML** - Manifest format (human-readable)

### Technology Decisions

| Decision | Choice | Rationale | Alternatives |
|----------|--------|-----------|--------------|
| **Format** | YAML + TOML | YAML readable, TOML for crates.io | JSON only, TOML only |
| **Registry** | Crates.io native | Standard, 100% reach, zero maintenance | Private registry, GitHub releases |
| **Resolver** | Lock-file based | Deterministic reproducibility | Semver ranges, latest versions |
| **Validation** | Mandatory FMEA | Prevents unsafe packages by default | Optional, warning-only |
| **Search** | SPARQL + metadata | Semantic queries, quality metrics | Full-text only, crates.io search |
| **Caching** | Multi-layer | Offline support, performance | No cache, package cache only |

---

## Project Structure (4 Components, ~30 files)

### 1. Core Domain (`crates/ggen-marketplace/src/gpack/`)
**~2000-2500 LOC across 15 new files**

```
crates/ggen-marketplace/src/gpack/
├── format.rs              (GpackManifest definition, validation)
├── manifest.rs            (YAML/TOML serialization, conversion)
├── crates_client.rs       (crates.io HTTP API integration)
├── resolver.rs            (Dependency resolution, version constraints)
├── validator.rs           (FMEA validation, guard application)
├── cache.rs               (Multi-layer caching, invalidation)
├── error.rs               (Error types and messages)
├── lockfile.rs            (ggen.lock format and generation)
├── search.rs              (SPARQL queries for discovery)
├── quality.rs             (Quality tier computation)
├── mod.rs                 (Module organization)
└── tests/                 (Unit tests for domain logic)
```

**Responsibilities**:
- Define gpack manifest format (YAML/TOML)
- Serialize/deserialize package metadata
- Integrate with crates.io API (async, concurrent)
- Resolve dependencies deterministically
- Validate FMEA and apply poka-yoke guards
- Cache metadata and packages efficiently
- Query marketplace via SPARQL
- Compute quality tiers

### 2. CLI Commands (`crates/ggen-cli/src/commands/marketplace/`)
**~800-1000 LOC across 5 files**

```
crates/ggen-cli/src/commands/marketplace/
├── publish.rs             (publish cmd: manifest → gpack → crates.io)
├── install.rs             (install cmd: download, validate, install)
├── search.rs              (search cmd: SPARQL queries, results display)
├── list.rs                (list cmd: show installed packages)
├── update.rs              (update cmd: regenerate lock file)
└── mod.rs                 (Command routing)
```

**Responsibilities**:
- Parse CLI arguments
- Call domain layer functions
- Format and display results
- Handle user errors gracefully
- Integrate with ggen sync pipeline

### 3. Test Suite (`crates/ggen-core/tests/` + `crates/ggen-marketplace/tests/`)
**~3000-3500 LOC across 8 files**

```
Tests structure:
├── gpack_format_tests.rs           (Format validation, schema)
├── crates_integration_tests.rs     (crates.io API mocking)
├── resolver_tests.rs               (Dependency resolution, conflicts)
├── validation_tests.rs             (FMEA, guards, safety)
├── e2e_publish_tests.rs            (Full publish workflow)
├── e2e_install_tests.rs            (Full install workflow)
├── e2e_search_tests.rs             (SPARQL queries, performance)
└── cross_platform_tests.rs         (Determinism: SHA256 matching)
```

**Test Approach**: Chicago TDD
- Tests verify behavior, not implementation
- Use real objects (no mocks) where possible
- AAA pattern (Arrange-Act-Assert)
- 80%+ code coverage target

### 4. Documentation (`docs/features/marketplace-gpack.md`)
**~1500-2000 LOC across 4 files**

```
docs/features/
├── marketplace-gpack.md            (Main feature guide)
├── manifest-reference.md           (Gpack manifest schema)
├── migration-guide.md              (How to convert 84 packages)
└── api-reference.md                (CLI and API docs)
```

---

## Implementation Phases (8 Phases, 168-216 hours)

### Phase 0: Foundation (Days 1-4, 24-32 hours)
**Goal**: Set up infrastructure, dependencies, test framework

**Deliverables**:
- Crate structure created (gpack module in ggen-marketplace)
- All 13 technologies integrated in Cargo.toml
- Test infrastructure operational (cargo test, cargo make test-unit)
- Pre-commit hooks configured

**Quality Gate**:
```bash
cargo make check       # Compilation clean
cargo make test-unit   # Tests passing
cargo make lint        # Clippy clean
```

### Phase 1a: Gpack Format (Days 5-8, 24-32 hours)
**Goal**: Define gpack manifest format and serialization

**Deliverables**:
- GpackManifest struct defined and documented
- YAML ↔ TOML serialization working
- Schema validation (name pattern, version, fields)
- Unit tests for format (10+ scenarios)

**Acceptance**: Manifest serialization round-trips without data loss

### Phase 1b: Publish (Days 9-12, 24-32 hours)
**Goal**: Integrate with crates.io, implement publish command

**Deliverables**:
- CratesClient struct (async HTTP client)
- CLI: `ggen marketplace publish --package PKG`
- Authentication and token handling
- Integration tests: Can publish to test registry

**Acceptance**: Test package published and searchable in 30 seconds

### Phase 2a: Resolver (Days 13-16, 24-32 hours)
**Goal**: Implement dependency resolution and lock files

**Deliverables**:
- Resolver engine (version constraint satisfaction)
- LockFile generation and parsing
- Cache system (packages + metadata)
- Determinism validation (SHA256 matching)

**Acceptance**: Lock file reproducible across Linux/macOS/Windows

### Phase 2b: Validation (Days 17-20, 16-24 hours)
**Goal**: FMEA validation and poka-yoke guard application

**Deliverables**:
- FMEA validation integration
- Guard application during installation
- CLI: `ggen marketplace install PKG`
- Installation audit trail

**Acceptance**: Critical FMEA failures block installation (unless --force-fmea)

### Phase 3a: Search (Days 21-24, 24-32 hours)
**Goal**: SPARQL search and quality indicators

**Deliverables**:
- SPARQL queries for flexible search
- Crates.io metadata indexing
- Quality tier computation
- CLI: `ggen marketplace search TERM`

**Acceptance**: 100 concurrent searches return <1 second

### Phase 3b: Recommendations (Days 25-27, 16-24 hours)
**Goal**: Recommendation engine and offline support

**Deliverables**:
- Recommendation algorithm (quality-based ranking)
- Offline installation from cache
- Cache invalidation strategy

**Acceptance**: Offline installation works with appropriate warnings

### Phase 4: Release (Days 28-35, 40-56 hours)
**Goal**: Convert 84 packages, comprehensive testing, release

**Deliverables**:
- Conversion tool for 84 marketplace packages
- E2E test suite (cross-platform)
- Complete documentation with examples
- Release notes and migration guide

**Acceptance**:
- All 84 packages published to crates.io
- All 7 success criteria met (SC-001 through SC-007)
- 80%+ test coverage
- Zero breaking changes verified

---

## 6 Architectural Decisions

### AD-001: Gpack Format (YAML + TOML Hybrid)
**Decision**: Primary format is YAML (flexible, readable), auto-generate TOML for crates.io

**Rationale**:
- YAML allows complex structures (multiple dependencies, metadata)
- TOML is crates.io standard (required for publishing)
- Conversion is deterministic (tested)

**Verification**: Round-trip conversion tests, no data loss

### AD-002: Registry Integration (Crates.io Native)
**Decision**: Use crates.io public API as primary distribution channel

**Rationale**:
- Standard Rust ecosystem (100% audience reach)
- Zero maintenance cost (crates.io managed)
- Proven pattern (all Rust packages use it)

**Verification**: Publish 10 test packages, verify searchable

### AD-003: Dependency Resolution (Lock-File Based)
**Decision**: Pin exact versions in ggen.lock, committed to git

**Rationale**:
- Enables byte-identical installs across systems
- Proven pattern from Cargo, npm, pip
- Supports reproducible builds

**Verification**: SHA256 hashes match Linux == macOS == Windows

### AD-004: FMEA Validation (Mandatory with Override)
**Decision**: Block installation of packages with critical FMEA failures unless --force-fmea

**Rationale**:
- Safety by default (prevents unsafe packages)
- Explicit override (power users can override)
- Audit trail of all decisions

**Verification**: Installation blocked for critical failures, --force-fmea allows override

### AD-005: Search Engine (SPARQL Queries)
**Decision**: Use SPARQL on Oxigraph RDF index for flexible search

**Rationale**:
- Enables semantic queries (beyond full-text)
- Quality metrics and category filtering
- Already integrated in ggen-marketplace

**Verification**: 100 concurrent queries <1 second latency

### AD-006: Caching Strategy (Multi-Layer)
**Decision**: Cache both metadata (search) and packages (install) separately

**Rationale**:
- Metadata cache enables offline search
- Package cache enables offline install
- Both needed for complete offline support

**Verification**: Offline functionality works with appropriate warnings

---

## 5 Critical Dependencies

✅ **All available, no blockers**

1. **Existing Marketplace v5.0.2**: Already integrated, proven architecture
2. **FMEA Validation (spec v006)**: Already defined, controls available
3. **Poka-Yoke Guards (spec v006)**: Already defined, ready to integrate
4. **Crates.io API**: Public, stable, documented

---

## 5 Key Risks & Mitigation

### R-001: Determinism Breaks Across Platforms (Medium Probability, Critical Severity)

**Risk**: SHA256 hashes differ between Linux/macOS/Windows

**Mitigation**:
- Use sorted keys for all collections (BTreeMap, not HashMap)
- Test across all platforms in CI
- Lock Rust toolchain version
- Document platform-specific gotchas

**Verification**: Cross-platform hash matching test

### R-002: FMEA Reports Missing or Invalid (Medium Probability, High Severity)

**Risk**: Installation proceeds without FMEA validation

**Mitigation**:
- Make FMEA reference required in manifest
- Default to blocking if FMEA missing/invalid
- Provide clear --force-fmea override message
- Log all FMEA decisions to audit trail

**Verification**: Test with missing FMEA, verify blocking

### R-003: Crates.io Downtime (Low Probability, High Severity)

**Risk**: Users cannot install packages during outage

**Mitigation**:
- Cache packages and metadata locally
- Implement offline mode with clear messaging
- Provide fallback registry option (for future)
- Clear warnings about using cached versions

**Verification**: Network failure simulation test

### R-004: Unresolvable Version Conflicts (Medium Probability, Medium Severity)

**Risk**: Installation fails without clear resolution path

**Mitigation**:
- Sophisticated constraint solver
- Detailed error messages showing conflicts
- Suggest package upgrades or alternatives
- Document common patterns

**Verification**: Complex dependency scenario tests

### R-005: Backward Compatibility Broken (Low Probability, Critical Severity)

**Risk**: Existing marketplace workflows stop working

**Mitigation**:
- Parallel operation (old + new during transition)
- Comprehensive integration test suite
- Feature flags for gradual rollout
- Easy rollback to v5.0.2

**Verification**: All existing commands work unchanged

---

## Data Models (5 Core Types)

### GpackManifest
Package metadata in gpack format (YAML/TOML)
```yaml
crate_name: my-pkg-gpack
version: 1.0.0
description: "A marketplace package"
dependencies:
  other-pkg-gpack: ">=1.0,<2.0"
fmea_reference: "https://crates.io/crates/my-pkg-gpack/0.1.0/docs"
quality_tier: gold
homepage: "https://github.com/user/my-pkg"
documentation: "https://docs.rs/my-pkg-gpack"
```

### CratesIndexEntry
Cached metadata from crates.io for search and discovery
- Indexed in Oxigraph RDF
- Updated lazily on-demand
- 1-hour cache lifetime

### InstallationMetadata
Record of installation with validation results
- Stored in ggen audit trail JSON
- Includes FMEA status, guards applied, deps resolved
- Used for reproducibility verification

### FmeaValidationReport
FMEA validation from spec v006
- Fetched from crates.io metadata
- Blocking if critical failures (RPN >= 200)
- Override via --force-fmea

### GgenLockFile
Deterministic lock file (ggen.lock)
```yaml
format_version: "1.0"
packages:
  other-pkg-gpack: "1.0.5"
checksums:
  other-pkg-gpack: "sha256:..."
generated_at: "2025-12-21T16:30:00Z"
rust_toolchain: "1.75"
```

---

## Constitutional Requirements (5 Principles)

All implementation must comply with ggen constitution:

1. **100% Type Safety** (CONST-001)
   - All types explicit, no untyped code
   - Rust compiler enforces, clippy validates

2. **Result<T,E> Error Handling** (CONST-002)
   - All fallible operations return Result
   - No unwrap/expect in production code

3. **Deterministic Output** (CONST-003)
   - Same inputs → byte-identical outputs
   - Verified via SHA256 hashing

4. **Chicago School TDD** (CONST-004)
   - Tests verify behavior (not implementation)
   - AAA pattern, 80%+ coverage

5. **Documentation-First** (CONST-005)
   - 100% of public APIs documented
   - Examples compile, messages actionable

---

## Quality Gates (5 Checkpoints)

### Gate 0: Foundation Setup
**Phase**: Foundation (Phase 0)
**Criteria**:
- Crate structure builds cleanly
- Dependencies integrated
- Test infrastructure working
- Pre-commit hooks functional

**Command**: `cargo make check && cargo make test-unit`
**Blocking**: Yes

### Gate 1: Gpack Format & Publish
**Phase**: Critical (Phases 1a-1b)
**Criteria**:
- Manifest format defined
- YAML ↔ TOML serialization works
- Test package publishes to crates.io
- Package searchable within 30s

**Command**: `cargo make test && integration tests`
**Blocking**: Yes

### Gate 2: Installation & Validation
**Phase**: High (Phases 2a-2b)
**Criteria**:
- Package installs from crates.io
- Dependency resolution works
- Lock file reproducible
- FMEA validation blocks critical failures

**Command**: `cargo make test && integration tests`
**Blocking**: Yes

### Gate 3: Search & Discovery
**Phase**: Medium (Phase 3a)
**Criteria**:
- SPARQL queries return <1s
- Quality indicators accurate
- 100 concurrent queries handled
- Metadata cached efficiently

**Command**: Load test script, benchmarks
**Blocking**: No (advisory)

### Gate 4: Release Readiness
**Phase**: Polish (Phase 4)
**Criteria**:
- All 84 packages migrated
- All acceptance scenarios pass
- All 7 success criteria met
- 80%+ test coverage achieved
- Zero breaking changes verified
- Documentation complete

**Command**: `cargo make ci && full test suite`
**Blocking**: Yes

---

## Estimated Effort & Timeline

### Total Effort: 168-216 hours (4-5.5 weeks)

**Breakdown**:
- Phase 0 (Foundation): 24-32 hours (3-4 days)
- Phase 1a (Format): 24-32 hours (3-4 days)
- Phase 1b (Publish): 24-32 hours (3-4 days)
- Phase 2a (Resolver): 24-32 hours (3-4 days)
- Phase 2b (Validation): 16-24 hours (2-3 days)
- Phase 3a (Search): 24-32 hours (3-4 days)
- Phase 3b (Recommendations): 16-24 hours (2-3 days)
- Phase 4 (Release): 40-56 hours (5-7 days)

### Code Estimates:
- Domain layer: 2000-2500 LOC
- CLI layer: 800-1000 LOC
- Test suite: 3000-3500 LOC
- Documentation: 1500-2000 LOC
- **Total**: 7300-9000 LOC

### Critical Path: 2-3 weeks minimum (sequential)
- Foundation → Phase 1b → Phase 2b blocks all later phases
- Phases 3a/3b can parallelize after Phase 2b

### Optimal Team Size: 10 parallel agents (following v5.2.0 pattern)
- Agent 1: Foundation (Phase 0)
- Agent 2: Gpack Format (Phase 1a)
- Agent 3: Publish (Phase 1b)
- Agent 4: Resolver (Phase 2a)
- Agent 5: Validation (Phase 2b)
- Agent 6: Search (Phase 3a)
- Agent 7: Recommendations (Phase 3b)
- Agent 8: E2E Tests (Phase 4)
- Agent 9: Documentation (Phase 4)
- Agent 10: Package Migration (Phase 4)

---

## Success Criteria Mapping

All 7 success criteria from specification are measurable:

| Criterion | Target | Verification | Effort |
|-----------|--------|---------------|--------|
| SC-001: Backward Compatibility | 100% of 84 packages | Migration test | Phase 4 |
| SC-002: Publish Latency | ≤30 seconds | Benchmark 3 packages | Phase 1b |
| SC-003: Install Performance | ≤30 seconds (5-10MB) | Benchmark 10 packages | Phase 4 |
| SC-004: Search Latency | ≤1 second | Load test 100 queries | Phase 3a |
| SC-005: FMEA Coverage | 100% of installs | Audit trail analysis | Phase 2b |
| SC-006: Zero Breaking Changes | All CLI workflows | Integration tests | Phase 4 |
| SC-007: Determinism | SHA256 match across OS | Cross-platform test | Phase 4 |

---

## Next Step: Task Breakdown

**Ready for**: `/speckit.tasks`

The planning phase defines:
- ✅ Architecture pattern (Layered + Command)
- ✅ 13 technology choices (all justified)
- ✅ 4-component project structure (~30 files)
- ✅ 8 implementation phases (168-216 hours)
- ✅ 6 architectural decisions (with rationale)
- ✅ 5 domain models (with schemas)
- ✅ 5 constitutional requirements (compliance)
- ✅ 5 quality gates (validation checkpoints)
- ✅ 5 risks + mitigation strategies

**Task breakdown will create**:
- 40-50 executable implementation tasks
- Dependencies and parallelization groups
- File paths and code sections
- Test coverage requirements
- Documentation needs

---

## Summary

The implementation plan provides a complete, detailed, executable roadmap for the ggen marketplace gpack retrofit:

✅ **Architecture Clear**: Layered design with 4 components
✅ **Technology Selected**: 13 technologies justified and alternatives considered
✅ **Phases Defined**: 8 phases with dependencies and quality gates
✅ **Risks Identified**: 5 key risks with mitigation strategies
✅ **Success Metrics**: All 7 criteria mapped to phases
✅ **Effort Estimated**: 168-216 hours (4-5.5 weeks)
✅ **Team Optimized**: 10 parallel agents for maximum throughput

The plan bridges specification (what) with execution (how), enabling clear task breakdown and parallel agent coordination.

---

**Source**: RDF Implementation Plan (`specs/014-marketplace-gpack/ontology/plan.ttl`)
**Status**: ✅ COMPLETE - READY FOR TASK BREAKDOWN
**Quality**: Architecture-level completeness
**Date**: 2025-12-21
