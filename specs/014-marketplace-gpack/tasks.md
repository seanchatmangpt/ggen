<!-- Generated from ontology/tasks.ttl - DO NOT EDIT MANUALLY -->

# Marketplace Gpack - Task Breakdown

**Branch**: `014-marketplace-gpack`
**Feature**: Marketplace Gpack Distribution Retrofit
**Date**: 2025-12-21
**Status**: ✅ TASK BREAKDOWN COMPLETE - READY FOR IMPLEMENTATION
**Total Tasks**: 52
**Quality Target**: Lean Six Sigma (99.99966%)

---

## Executive Summary

The marketplace gpack retrofit has been broken down into **52 executable tasks organized across 9 phases**:

| Phase | Title | Duration | Tasks | Critical? |
|-------|-------|----------|-------|-----------|
| 1 | Project Setup | 3-4h | 4 | ✅ Blocking |
| 2 | Foundation (Core Models) | 24-32h | 6 | ✅ Blocking |
| 3 | Story 1: Publish | 24-32h | 8 | ✅ Critical Path |
| 4 | Story 2: Install | 28-36h | 10 | ✅ Critical Path |
| 5 | Story 3: Search | 20-28h | 7 | - |
| 6 | Story 4: Determinism | 12-16h | 4 | - |
| 7 | Story 5: Validation | 16-20h | 5 | - |
| 8 | Story 6: Recommendations | 12-16h | 4 | - |
| 9 | Polish & Release | 40-56h | 4 | ✅ Blocking |
| **TOTAL** | | **168-216h** | **52** | |

---

## Implementation Strategy

### Phase Dependencies

```
Phase 1 (Setup)
    ↓
Phase 2 (Foundation - Core Domain Models)
    ├→ Phase 3 (Story 1: Publish)
    │   ↓
    ├→ Phase 4 (Story 2: Install) ← Depends on Phase 3
    │   ├→ Phase 5 (Story 3: Search) ← Depends on Phase 4
    │   ├→ Phase 6 (Story 4: Determinism) ← Depends on Phase 4
    │   └→ Phase 7 (Story 5: Validation) ← Depends on Phase 4
    │
    └→ Phase 8 (Story 6: Recommendations) ← Depends on Phase 5
        ↓
    Phase 9 (Polish & Release) ← Depends on Phase 8
```

### Parallelization Strategy (10-Agent Swarm)

**Phase 1-2**: Sequential (foundation blocking)
**Phase 3-8**: Partially parallel with dependencies:
- Story 1 (Publish) starts first
- Story 2 (Install) starts when Publish domain layer ready
- Stories 3-6 start when their dependencies ready
- Tests run in parallel with implementation in most phases

**Phase 9**: Mostly sequential (final validation, migration, release)

**Optimal Team Layout** (10 agents):
- 1 Architecture Lead (oversees all phases, makes design decisions)
- 2 Domain Developers (Phase 2 core models, Phase 3-5 domain logic)
- 2 CLI Developers (Phase 3-8 command implementations)
- 2 Test Engineers (All phases, Chicago TDD, comprehensive coverage)
- 1 Documentation Writer (Concurrent with development)
- 1 Integration Engineer (Phase 4-9, migrations, CI)
- 1 Performance Engineer (Phase 9, benchmarking, optimization)

---

## Phase 1: Project Setup (3-4 hours)

**Goal**: Initialize project structure and establish patterns
**Dependencies**: None (first phase)
**Critical**: ✅ Blocking for all downstream phases
**Parallelizable**: ❌ All sequential

### Tasks

- [ ] **T001** Create gpen-marketplace gpack module structure
  - Create `crates/ggen-marketplace/src/gpack/` directory structure
  - Create mod.rs and file stubs: format.rs, manifest.rs, crates_client.rs, resolver.rs, validator.rs, cache.rs, error.rs, lockfile.rs, search.rs, quality.rs
  - **Acceptance**: Module builds, cargo check passes
  - **Estimated**: 0.5 hours

- [ ] **T002** Verify and document all dependencies in Cargo.toml
  - Add 13 technologies to Cargo.toml: tokio, reqwest, serde (json/toml/yaml), oxigraph, tera, tracing, OTEL, sha2, ed25519-dalek, tempfile
  - Configure feature gates (serde/json, serde/toml, serde/yaml)
  - Document version constraints with rationale
  - Run `cargo check` to verify
  - **Acceptance**: cargo check passes, all deps documented
  - **Estimated**: 1 hour

- [ ] **T003** Set up test infrastructure and fixtures
  - Create test directories: `crates/ggen-core/tests/`, `crates/ggen-marketplace/tests/`
  - Add test fixtures: sample manifests (valid/invalid), lock files, FMEA reports
  - Create test utilities: HTTP mocking, package generation, assertion helpers
  - **Acceptance**: Test structure ready, fixtures available, utils functional
  - **Estimated**: 1.5 hours

- [ ] **T004** Configure pre-commit hooks and CI pipeline
  - Update .pre-commit-config.yaml for gpack validation
  - Update GitHub Actions CI workflows for gpack testing
  - Configure cargo make targets: `cargo make gpack-check`, `gpack-test`, `gpack-lint`
  - **Acceptance**: Pre-commit hooks work, CI green, all make targets pass
  - **Estimated**: 1 hour

---

## Phase 2: Foundation - Core Domain Models (24-32 hours)

**Goal**: Define core domain types and error handling (blocks all feature stories)
**Dependencies**: Phase 1 (setup complete)
**Critical**: ✅ Blocking for all feature stories
**Parallelizable**: ✅ Tasks T005-T010 all parallel (different modules, no shared deps)

### Tasks

- [ ] **T005** [P] Define GpackManifest structure and validation rules
  - Create struct `GpackManifest` with fields: crate_name, version, description, dependencies, fmea_reference, quality_tier, homepage, documentation
  - Implement validation: crate_name ends with "-gpack", version is SemVer, description ≤500 chars
  - Add #[derive(Serialize, Deserialize)] with feature gates for YAML/TOML
  - **File**: `crates/ggen-marketplace/src/gpack/format.rs`
  - **Acceptance**: Struct defined, validation tested, serde works
  - **Estimated**: 4 hours
  - **Tests**: Part of T014 (manifest tests)

- [ ] **T006** [P] Define comprehensive error type for gpack operations
  - Create `GpackError` enum with variants: FormatError, ManifestError, CratesError, ResolverError, ValidationError, CacheError
  - Include error messages with context
  - Implement Display and std::error::Error traits
  - Use thiserror crate for derive macros
  - **File**: `crates/ggen-marketplace/src/gpack/error.rs`
  - **Acceptance**: Error enum defined, Display impl works, all variants documented
  - **Estimated**: 3 hours

- [ ] **T007** [P] Define lock file format and structures
  - Create struct `GgenLockFile` with fields: format_version (1.0), packages, checksums, generated_at, rust_toolchain
  - Implement serialization to YAML (for readability)
  - Implement deserialization with validation
  - **File**: `crates/ggen-marketplace/src/gpack/lockfile.rs`
  - **Acceptance**: Struct defined, YAML ser/de works, round-trip tests pass
  - **Estimated**: 3 hours

- [ ] **T008** [P] Define dependency version constraint types
  - Create `VersionConstraint` enum: Exact(Version), AtLeast(Version), UpTo(Version), Range(min, max)
  - Implement constraint matching: does_match(version) → bool
  - Implement parser for semver strings: ">=1.0", "<2.0", "1.2.3", etc.
  - **File**: `crates/ggen-marketplace/src/gpack/resolver.rs` (types section)
  - **Acceptance**: Enum defined, parser works, matching tested
  - **Estimated**: 3 hours

- [ ] **T009** [P] Define cache layer types and interfaces
  - Create `CacheEntry` struct: package_name, version, hash, location, timestamp, size
  - Define `CacheManager` trait with: get(), set(), invalidate(), cleanup()
  - Plan two cache layers: metadata (1h TTL), packages (7d TTL)
  - **File**: `crates/ggen-marketplace/src/gpack/cache.rs` (types section)
  - **Acceptance**: Types and trait defined, documentation clear
  - **Estimated**: 2 hours

- [ ] **T010** [P] Define FMEA validation types and quality tiers
  - Create `FmeaValidation` struct: crate_name, version, failure_modes, critical_count, controls_applied
  - Create `QualityTier` enum: Gold, Silver, Bronze
  - Create `PokayokeGuard` enum: DirectorySeparation, TraitBoundary, PathProtection, VersionConstraint
  - **File**: `crates/ggen-marketplace/src/gpack/validator.rs` (types section)
  - **Acceptance**: All types defined, documentation complete
  - **Estimated**: 2.5 hours

---

## Phase 3: User Story 1 - Publish to crates.io (24-32 hours)

**JTBD**: Package developers publish marketplace packages to crates.io without custom tooling

**Dependencies**: Phase 2 (foundation complete)
**Critical**: ✅ Critical path (blocks Story 2)
**Parallelization**: T011/T013/T014 parallel; T012 depends on T011; T016-T018 sequential after CLI ready

### Story Goal

Enable package developers to publish marketplace packages to crates.io as `*-gpack` crates, reaching the standard Rust ecosystem.

### Acceptance Scenarios

1. **Basic Publication**: Publish manifest → gpack created → appears in crates.io search within 30 seconds
2. **Dependency Resolution**: Dependencies on other marketplace packages resolved to their gpack crates
3. **Validation Requirement**: Missing FMEA blocks publication with clear error message

### Tasks

- [ ] **T011** [P] Implement manifest serialization (YAML → TOML conversion)
  - Load YAML gpack manifest
  - Convert to Cargo.toml format for crates.io
  - Handle dependencies: marketplace refs → crates.io refs
  - Preserve optional fields: homepage, documentation, FMEA reference
  - **File**: `crates/ggen-marketplace/src/gpack/manifest.rs`
  - **Acceptance**: YAML→TOML conversion works, fields preserved, round-trip tests pass
  - **Estimated**: 4 hours

- [ ] **T012** Implement crates.io HTTP API client
  - Create async HTTP client for crates.io
  - Implement `publish(manifest, tarball) → PublishResponse`
  - Implement `fetch_metadata(crate_name) → CrateMetadata`
  - Implement `search(query) → SearchResults`
  - Handle authentication via API token (env var: CARGO_REGISTRY_TOKEN)
  - Implement retries (3x with exponential backoff) and timeouts (30s)
  - **File**: `crates/ggen-marketplace/src/gpack/crates_client.rs`
  - **Dependency**: T011 (manifest format)
  - **Acceptance**: Client publishes/fetches/searches, unit tests pass with mocked API
  - **Estimated**: 6 hours

- [ ] **T013** [P] Implement manifest validation and schema enforcement
  - Validate crate_name pattern: `^[a-z0-9_-]+-gpack$`
  - Validate version: semantic versioning required
  - Validate required fields present: name, version, description
  - Validate optional fields: homepage, documentation are valid URLs
  - Validate FMEA reference if provided
  - Return detailed validation errors
  - **File**: `crates/ggen-marketplace/src/gpack/format.rs` (validation section)
  - **Acceptance**: All validation rules enforced, error messages clear, edge cases handled
  - **Estimated**: 3 hours

- [ ] **T014** [P] Write unit tests for manifest and serialization
  - Test valid manifest parsing and serialization
  - Test invalid patterns: missing fields, invalid names, bad versions
  - Test round-trip: YAML → internal → TOML → back to internal → YAML (same)
  - Test edge cases: unicode in description, special chars, long strings, optional fields
  - Achieve 90%+ coverage of format.rs and manifest.rs
  - **File**: `crates/ggen-marketplace/tests/gpack_format_tests.rs`
  - **Approach**: Chicago TDD (AAA pattern, state-based, real objects)
  - **Coverage**: 90%+
  - **Acceptance**: All tests pass, no panics on invalid input, coverage 90%+
  - **Estimated**: 3 hours

- [ ] **T015** [P] Write unit tests for crates.io client
  - Test publish() success/error paths (mocked HTTP responses)
  - Test fetch_metadata() with various crate names
  - Test search() with different queries
  - Test timeout handling and retry logic
  - Test authentication token inclusion
  - Test error message formatting for users
  - Achieve 85%+ coverage of crates_client.rs
  - **File**: `crates/ggen-marketplace/tests/crates_integration_tests.rs`
  - **Approach**: Chicago TDD with HTTP mocking (mockito crate)
  - **Coverage**: 85%+
  - **Acceptance**: All paths tested, mocks work correctly, timeouts tested
  - **Estimated**: 4 hours

- [ ] **T016** Implement CLI publish command
  - Parse CLI args: `ggen marketplace publish --package <name> [--target crates.io]`
  - Load manifest from `<name>.toml` or search for it
  - Validate manifest (call validation from T013)
  - Verify FMEA reference present (block if missing with clear message)
  - Call crates_client.publish() (from T012)
  - Display success message with crates.io link
  - Handle errors gracefully with user-friendly messages
  - **File**: `crates/ggen-cli/src/commands/marketplace/publish.rs`
  - **Dependency**: T012 (crates client)
  - **Acceptance**: Command parses args, validates, publishes, displays results, handles errors
  - **Estimated**: 3 hours

- [ ] **T017** Implement end-to-end publish workflow tests
  - Test full flow: load manifest → validate → serialize → publish (mocked)
  - Test acceptance scenarios:
    - Basic publication: valid manifest → published
    - With dependencies: marketplace deps → gpack crates
    - Missing FMEA: publish blocked with actionable error
  - Mock crates.io responses
  - Verify package would appear in search
  - Achieve 80%+ coverage of end-to-end flow
  - **File**: `crates/ggen-core/tests/e2e_publish_tests.rs`
  - **Dependency**: T016 (CLI command)
  - **Approach**: Chicago TDD e2e
  - **Coverage**: 80%+
  - **Acceptance**: All acceptance scenarios pass, no flaky tests
  - **Estimated**: 3 hours

- [ ] **T018** Write comprehensive documentation for publish workflow
  - Document `ggen marketplace publish` command
  - Include CLI examples: basic usage, with options
  - Document manifest format (.toml structure)
  - Document FMEA requirement and where to find/create reports
  - Document error scenarios and troubleshooting
  - Include step-by-step guide for developers
  - **File**: `docs/features/marketplace-gpack.md` (publish section)
  - **Dependency**: T016 (CLI command)
  - **Acceptance**: Clear examples, all options documented, no ambiguities
  - **Estimated**: 2 hours

---

## Phase 4: User Story 2 - Install from crates.io (28-36 hours)

**JTBD**: Users install marketplace packages from crates.io with automatic validation

**Dependencies**: Phase 3 (publish workflow complete, crates client available)
**Critical**: ✅ Critical path (enables Stories 3-5)
**Parallelization**: T019/T020/T021/T023 parallel; sequential after for CLI/e2e

### Story Goal

Enable users to install marketplace packages from crates.io with deterministic resolution, validation, and caching.

### Acceptance Scenarios

1. **Basic Installation**: Download → resolve dependencies → install → ready to use (≤30 seconds)
2. **Installation Validation**: FMEA checks performed, poka-yoke guards applied, audit logged
3. **Offline Installation**: Cached packages used when network unavailable, with appropriate warnings

### Tasks

- [ ] **T019** [P] Implement dependency resolver engine
  - Create constraint satisfaction algorithm
  - Process version constraints (>=1.0, <2.0, etc.)
  - Detect conflicts early: conflicting packages/versions
  - Build resolution tree: packages → resolved versions
  - Return detailed error on conflict with suggestions
  - Handle cycles and missing packages gracefully
  - **File**: `crates/ggen-marketplace/src/gpack/resolver.rs` (implementation)
  - **Acceptance**: Resolver handles simple/complex/conflicting deps, suggests resolutions
  - **Estimated**: 6 hours

- [ ] **T020** [P] Implement multi-layer caching system
  - Implement CacheManager trait from T009
  - Metadata cache: 1 hour TTL, stores crate_index entries
  - Package cache: 7 days TTL, stores downloaded .tar.gz files
  - Storage: `~/.ggen/cache/` with organized structure: `metadata/`, `packages/`
  - Implement cleanup: remove expired entries on startup
  - Implement cache validation: verify SHA256 on read
  - **File**: `crates/ggen-marketplace/src/gpack/cache.rs` (implementation)
  - **Acceptance**: Cache reads/writes work, TTL enforced, cleanup removes stale data
  - **Estimated**: 5 hours

- [ ] **T021** [P] Implement FMEA validation and poka-yoke guard application
  - Load FMEA report from manifest reference
  - Count critical failures (RPN >= 200)
  - If critical failures found:
    - By default: block installation, show error with details
    - With --force-fmea: log warning, continue, apply all guards anyway
  - Apply poka-yoke guards:
    - Directory separation: install in isolated directory
    - Trait boundary: enforce trait boundaries if needed
    - Path protection: prevent `../` escapes
    - Version constraint: enforce dependency versions
  - Log all decisions to ggen audit system with OTEL spans
  - **File**: `crates/ggen-marketplace/src/gpack/validator.rs` (implementation)
  - **Acceptance**: FMEA validation enforced, guards applied, blocking works, audit logged
  - **Estimated**: 5 hours

- [ ] **T022** Implement lock file generation and persistence
  - After resolver completes, generate `ggen.lock`
  - Calculate SHA256 checksums for all packages
  - Use sorted keys (BTreeMap) for deterministic output
  - Include format_version, timestamp, rust_toolchain
  - Implement read/write operations
  - Implement update operation for `ggen marketplace update` command
  - Ensure round-trip consistency (load/save/load identical)
  - **File**: `crates/ggen-marketplace/src/gpack/lockfile.rs` (implementation)
  - **Dependency**: T019 (resolver)
  - **Acceptance**: Lock file created, contents deterministic, can be read/updated
  - **Estimated**: 3 hours

- [ ] **T023** [P] Write tests for resolver, cache, and validator
  - **Resolver tests**: simple deps, complex multi-level, conflicts, missing packages
  - **Cache tests**: get/set, TTL expiration, cleanup, validation, concurrent access
  - **Validator tests**: FMEA checks, guard application, audit logging, --force-fmea behavior
  - Use Chicago TDD: real objects, state-based, AAA pattern
  - Achieve 85%+ coverage
  - **File**: `crates/ggen-marketplace/tests/resolver_tests.rs + validation_tests.rs + cache_tests.rs`
  - **Coverage**: 85%+
  - **Acceptance**: All paths tested, coverage 85%+, no edge case regressions
  - **Estimated**: 6 hours

- [ ] **T024** Implement CLI install command
  - Parse args: `ggen marketplace install <crate-name> [--from crates.io] [--force-fmea] [--offline] [--no-cache]`
  - Call resolver (T019) to resolve dependencies
  - Call validator (T021) to check FMEA
  - Call cache (T020) to download/retrieve packages
  - Display progress: "Resolving...", "Downloading...", "Validating...", "Installing..."
  - Show validation results: FMEA status, guards applied
  - Handle network failures with graceful fallback to offline
  - Display clear error messages with actionable next steps
  - **File**: `crates/ggen-cli/src/commands/marketplace/install.rs`
  - **Dependency**: T021 (validator)
  - **Acceptance**: Command handles all args, validates, installs, shows progress, handles errors
  - **Estimated**: 3 hours

- [ ] **T025** Implement offline installation fallback
  - Enhance cache layer: when network request fails, check if package in cache
  - If in cache: use cached version, show warning about using stale data
  - If not in cache: fail with clear "package not found" message
  - Add --no-cache flag to skip offline fallback (require network)
  - Log all fallback decisions to audit trail
  - **File**: `crates/ggen-marketplace/src/gpack/cache.rs (offline logic) + install.rs`
  - **Dependency**: T024 (install command)
  - **Acceptance**: Offline fallback works, warnings clear, no silent failures
  - **Estimated**: 2 hours

- [ ] **T026** Write end-to-end installation workflow tests
  - Test full flow: download → resolve → validate → apply guards → install
  - Test acceptance scenarios:
    - Basic install: valid package → installed successfully
    - Validation hooks: FMEA passed → installation proceeds
    - Validation hooks: critical FMEA failure → installation blocked (default)
    - Validation hooks: critical FMEA failure + --force-fmea → installation proceeds with warning
    - Offline fallback: network failure → cached version used
  - Mock crates.io and FMEA checks
  - Achieve 80%+ coverage
  - **File**: `crates/ggen-core/tests/e2e_install_tests.rs`
  - **Dependency**: T024 (CLI command)
  - **Coverage**: 80%+
  - **Acceptance**: All scenarios pass, no flaky tests, clear failure messages
  - **Estimated**: 4 hours

- [ ] **T027** Write documentation for install workflow
  - Document `ggen marketplace install` command
  - Include CLI examples: basic, with flags, offline
  - Document validation process: FMEA checks, guards
  - Document lock file commitment to git
  - Document FMEA requirement and --force-fmea override (when to use, risks)
  - Document offline support and caching
  - Include troubleshooting: conflicts, missing FMEA, network failures
  - **File**: `docs/features/marketplace-gpack.md` (install section)
  - **Dependency**: T024 (CLI command)
  - **Acceptance**: Clear guide, all options documented, troubleshooting complete
  - **Estimated**: 2 hours

- [ ] **T028** Implement integration with ggen audit system
  - Create InstallationRecord struct with fields: crate_name, version, installed_at, fmea_validation_passed, guards_applied, dependencies_resolved, install_location, package_hash
  - Log records to ggen audit trail
  - Include OTEL span IDs for correlation across system
  - Enable querying audit trail: "How many installations of X-gpack?", "What FMEA decisions made?"
  - Document audit schema for external queries
  - **File**: `crates/ggen-marketplace/src/gpack/validator.rs (audit integration) + ggen-core audit module`
  - **Dependency**: T021 (validator)
  - **Acceptance**: Audit records created, OTEL correlation works, queries return data
  - **Estimated**: 2 hours

---

## Phase 5: User Story 3 - Search & Discover Packages (20-28 hours)

**JTBD**: Users discover marketplace packages via crates.io and semantic search

**Dependencies**: Phase 4 (install complete, crates client available)
**Critical**: - (not on critical path, starts after Phase 4)
**Parallelization**: T029/T030/T031 parallel; sequential after for CLI/e2e

### Story Goal

Enable users to search marketplace packages with quality-based filtering and recommendations.

### Acceptance Scenarios

1. **Basic Search**: Search crates.io for packages, results show metadata and quality indicators
2. **Search Filtering**: Filter by category, quality tier, FMEA status
3. **Quality Indicators**: Display FMEA status, download trend, last update date

### Tasks

- [ ] **T029** [P] Implement SPARQL query engine for marketplace search
  - Create SPARQL queries for different search patterns:
    - By crate name: exact match or contains
    - By category: ontology-based filtering
    - By quality tier: filter on gold/silver/bronze
    - By FMEA status: passed/failed/warning
  - Integrate with existing Oxigraph backend
  - Query marketplace RDF metadata index
  - Return results with full metadata
  - Optimize for latency: target <1 second for typical queries
  - Handle edge cases: no results, partial matches
  - **File**: `crates/ggen-marketplace/src/gpack/search.rs`
  - **Acceptance**: SPARQL queries return correct results, latency <1s for typical queries
  - **Estimated**: 5 hours

- [ ] **T030** [P] Implement quality tier computation and indicators
  - Define gold tier: FMEA passed + >100 downloads + updated <30 days
  - Define silver tier: FMEA passed + 10-100 downloads or updated 30-90 days
  - Define bronze tier: basic validation only
  - Compute tiers based on crates.io metadata
  - Display indicators in search results: [GOLD], [SILVER], [BRONZE]
  - Include metadata in results: download count, last update date, FMEA status
  - Update metadata cache with computed tiers
  - **File**: `crates/ggen-marketplace/src/gpack/quality.rs`
  - **Acceptance**: Tiers computed correctly, displayed in results, cache updated
  - **Estimated**: 3 hours

- [ ] **T031** [P] Write tests for SPARQL search and quality logic
  - Test SPARQL queries:
    - Search by name: find packages matching query
    - Filter by category: return matching categories only
    - Filter by tier: only gold/silver/bronze as specified
    - Filter by FMEA: only passed/failed as specified
  - Test quality tier computation:
    - Gold tier requirements met → tier = gold
    - Silver tier requirements met → tier = silver
    - Neither → tier = bronze
  - Test result ordering: tiers ranked correctly
  - Use real Oxigraph backend with test data
  - Achieve 80%+ coverage
  - **File**: `crates/ggen-marketplace/tests/search_tests.rs`
  - **Coverage**: 80%+
  - **Acceptance**: All query patterns tested, tiers computed correctly, latency acceptable
  - **Estimated**: 4 hours

- [ ] **T032** Implement CLI search command
  - Parse args: `ggen marketplace search <query> [--category <cat>] [--quality gold|silver|bronze] [--fmea-only] [--limit 20]`
  - Call search.rs SPARQL queries
  - Display results in table format: name | version | downloads | FMEA status | quality tier
  - Support JSON output format for scripts
  - Support pagination: --limit (default 20), --offset
  - Highlight quality tiers with visual indicators
  - **File**: `crates/ggen-cli/src/commands/marketplace/search.rs`
  - **Dependency**: T029 (SPARQL search)
  - **Acceptance**: Command searches, displays results with indicators, supports filters/pagination
  - **Estimated**: 3 hours

- [ ] **T033** Implement crates.io metadata syncing to RDF
  - Fetch package data from crates.io API (async)
  - For each published gpack package:
    - Extract: crate_name, description, downloads, last_updated, version_list
    - Convert to RDF triples: subject = crate, predicates = properties
    - Ingest into Oxigraph triplestore
  - Implement sync trigger: on-demand (manual) + on timer (1 hour interval)
  - Handle errors gracefully: partial failures don't stop entire sync
  - Log sync status: "Synced 42 packages", "Skipped 2 (errors)"
  - **File**: `crates/ggen-marketplace/src/gpack/crates_client.rs (sync module)`
  - **Dependency**: T012 (crates client)
  - **Acceptance**: Metadata synced to RDF, queries find synced data, timer works
  - **Estimated**: 4 hours

- [ ] **T034** Write e2e search and discovery tests
  - Test full flow:
    - Sync metadata from crates.io (mocked) → RDF
    - Execute SPARQL queries → get results
    - Display results with quality indicators
    - Test scenarios:
      - Basic search: find packages by name
      - Filter by category: narrow results
      - Filter by quality: show only gold tier
      - Filter by FMEA: show only passed
      - No results: handle gracefully
    - Performance test: 100 concurrent searches, all <1 second
    - Use real Oxigraph with test metadata
    - Achieve 80%+ coverage
  - **File**: `crates/ggen-core/tests/e2e_search_tests.rs`
  - **Dependency**: T032 (CLI command)
  - **Coverage**: 80%+
  - **Acceptance**: All scenarios pass, 100 concurrent queries <1s, no timeouts
  - **Estimated**: 3 hours

- [ ] **T035** Write documentation for search and discovery
  - Document search command: full reference with examples
  - Document filter options: --category, --quality, --fmea-only
  - Explain quality tiers and how they're computed
  - Include example searches: "find academic tools", "find gold-tier packages", "find FMEA-validated packages"
  - Document RDF metadata model for advanced users
  - Document custom SPARQL queries (for power users)
  - **File**: `docs/features/marketplace-gpack.md` (search section)
  - **Dependency**: T032 (CLI command)
  - **Acceptance**: Clear guide, all filters documented, examples work
  - **Estimated**: 2 hours

---

## Phase 6: User Story 4 - Deterministic Resolution (12-16 hours)

**JTBD**: Installations are deterministic and reproducible across systems (Linux/macOS/Windows)

**Dependencies**: Phase 4 (resolver and lock file complete)
**Critical**: - (not on critical path, verification phase)
**Parallelization**: T036/T037 parallel (different test scenarios)

### Story Goal

Ensure lock files and installations produce byte-identical results across platforms.

### Acceptance Scenarios

1. **Lockfile Generation**: Manifest → lock file with exact pinned versions and checksums
2. **Conflict Detection**: Incompatible versions clearly identified with resolution suggestions

### Tasks

- [ ] **T036** [P] Write determinism and lock file tests
  - Test lock file generation:
    - Resolve same manifest twice → identical lock files
    - Verify sorted keys (BTreeMap) used throughout
    - Verify checksums computed consistently
    - Verify timestamps included (for audit, but don't affect determinism)
  - Test round-trip consistency:
    - Generate lock file → save to YAML
    - Load from YAML → save to YAML (same bytes)
    - Repeat 3 times → always same output
  - Test across Rust versions: same Rust version produces same output
  - Achieve 90%+ coverage
  - **File**: `crates/ggen-marketplace/tests/lockfile_tests.rs`
  - **Coverage**: 90%+
  - **Acceptance**: Lock files deterministic, all keys sorted, round-trip succeeds
  - **Estimated**: 3 hours

- [ ] **T037** [P] Write cross-platform determinism tests (Linux/macOS/Windows)
  - Configure GitHub Actions CI matrix: runs on ubuntu-latest, macos-latest, windows-latest
  - Test scenario: resolve same manifest on all platforms
  - Verify: lock files produce same SHA256 hash across platforms
  - Verify: no platform-specific path separators affect output
  - Verify: no line ending differences (CRLF vs LF) affect hashes
  - Document any gotchas: Windows path handling, timezone handling, etc.
  - Fail CI if hashes differ across platforms
  - **File**: `crates/ggen-core/tests/cross_platform_tests.rs` (with CI configuration)
  - **Coverage**: 80%+
  - **Acceptance**: Cross-platform hashes match, CI passes on all OS
  - **Estimated**: 3 hours

- [ ] **T038** Implement conflict detection and error reporting
  - Enhance resolver.rs: detect conflicts during resolution
  - When conflict detected:
    - Identify conflicting packages: "pkg-a requires X v1.0, pkg-b requires X v2.0"
    - Identify conflicting versions: "v1.0 vs v2.0 incompatible"
    - Suggest resolutions:
      - Upgrade package A to newer version (if compatible)
      - Downgrade package B to older version (if compatible)
      - Remove one of the conflicting packages
  - Include conflict details in error message
  - Test with real conflict scenarios (many combinations)
  - **File**: `crates/ggen-marketplace/src/gpack/resolver.rs` (conflict detection)
  - **Dependency**: T019 (resolver)
  - **Acceptance**: Conflicts detected early, error messages include packages and suggestions
  - **Estimated**: 3 hours

- [ ] **T039** Write documentation for lock files and determinism
  - Explain why lock files matter: reproducibility, auditing, supply chain security
  - Document lock file format: what fields, what they mean
  - Document how to use lock files: commit to git, update with `ggen marketplace update`
  - Explain determinism guarantees and limitations
  - Document cross-platform considerations: what's guaranteed deterministic, what isn't
  - Include examples: sample lock file, update workflow
  - FAQ: "Why is my lock file different?", "When should I regenerate lock files?"
  - **File**: `docs/features/marketplace-gpack.md` (lock file section)
  - **Acceptance**: Clear explanation of lock files, format documented, examples provided
  - **Estimated**: 1.5 hours

---

## Phase 7: User Story 5 - FMEA Validation (16-20 hours)

**JTBD**: Installation safety through mandatory FMEA validation with poka-yoke guards

**Dependencies**: Phase 4 (validator implementation)
**Critical**: - (validation is integrated into Phase 4, this refines it)
**Parallelization**: T040/T041 parallel (different test layers)

### Story Goal

Ensure installations are safe by validating FMEA reports and applying error-prevention guards.

### Acceptance Scenarios

1. **FMEA Validation Check**: FMEA report fetched and validated, controls applied
2. **Critical Failure Blocking**: Critical failures (RPN >= 200) block installation by default, allow with --force-fmea

### Tasks

- [ ] **T040** [P] Write comprehensive FMEA validation tests
  - Test FMEA checks:
    - Package with no critical failures (RPN < 200) → passes validation
    - Package with critical failures (RPN >= 200) → fails validation
    - Package with no FMEA report → fails validation (or warnings?)
  - Test blocking behavior:
    - Critical failures + default → installation blocked
    - Critical failures + --force-fmea → installation proceeds with warning
  - Test guard application:
    - Guards marked as applied in audit trail
    - Guard violations tracked
  - Test error messages: actionable, reference FMEA report
  - Test audit logging: all decisions logged
  - Achieve 85%+ coverage
  - **File**: `crates/ggen-marketplace/tests/validation_tests.rs`
  - **Coverage**: 85%+
  - **Acceptance**: All validation paths tested, blocking works, flags honored
  - **Estimated**: 4 hours

- [ ] **T041** [P] Write poka-yoke guard integration tests
  - Test directory separation:
    - Packages installed in isolated directories
    - No cross-package contamination possible
  - Test trait boundary:
    - Trait implementations isolated by package
    - No leakage of private traits
  - Test path protection:
    - No `../` escapes possible
    - Paths validated to stay within installation directory
  - Test version constraint:
    - Range matching works correctly
    - Constraint violations detected
  - Test guard effectiveness:
    - Guards actually prevent the failure modes
    - No false positives (blocks legitimate use)
  - Achieve 80%+ coverage
  - **File**: `crates/ggen-marketplace/tests/guards_tests.rs`
  - **Coverage**: 80%+
  - **Acceptance**: All guards tested, isolation verified, no escapes possible
  - **Estimated**: 3 hours

- [ ] **T042** Enhance --force-fmea override mechanism and logging
  - Modify install command to require explicit confirmation for --force-fmea:
    - Display warning about what's being overridden
    - Require user confirmation (y/n)
    - Only proceed if confirmed
  - Log override usage to audit trail:
    - Timestamp, user, package, FMEA failures overridden
    - Reason (if provided via --reason "...")
  - Integration with ggen audit system:
    - Track --force-fmea decisions for compliance/auditing
    - Enable queries: "How many --force-fmea overrides this month?"
  - **File**: `crates/ggen-cli/src/commands/marketplace/install.rs (force logic) + validator.rs (audit)`
  - **Dependency**: T040 (validation tests)
  - **Acceptance**: Override requires confirmation, logged to audit, warning clear
  - **Estimated**: 2 hours

- [ ] **T043** Write documentation for FMEA validation and guards
  - Explain FMEA concept: Failure Mode and Effects Analysis
  - Document critical failure threshold: RPN >= 200 blocks installation
  - Document what guards do: prevent specific failure modes
  - List guard types:
    - Directory separation: packages isolated
    - Trait boundaries: trait implementations separated
    - Path protection: prevent directory traversal
    - Version constraints: enforce dependency versions
  - Document --force-fmea override: when to use, risks
  - Safety guidelines: recommendations for when --force-fmea is acceptable
  - Include examples: FMEA report, guard descriptions
  - **File**: `docs/features/marketplace-gpack.md` (validation section)`
  - **Acceptance**: Clear validation explanation, guards documented, safety guidelines provided
  - **Estimated**: 1.5 hours

- [ ] **T044** Write e2e FMEA validation workflow tests
  - Test full validation flow:
    - Package with valid FMEA → passes → installation succeeds
    - Package with critical FMEA failures → blocked → installation fails (default)
    - Package with critical FMEA failures + --force-fmea → warning shown → installation proceeds
  - Test acceptance scenarios from spec.md:
    - Basic install with validation: passes
    - Validation hooks applied: guards confirmed active
    - Critical failure blocking: error message clear
  - Test audit trail:
    - All validation decisions logged
    - --force-fmea override logged with reason
  - Verify error messages are clear and actionable
  - Achieve 80%+ coverage
  - **File**: `crates/ggen-core/tests/e2e_validation_tests.rs`
  - **Dependency**: T042 (force logic)
  - **Coverage**: 80%+
  - **Acceptance**: All scenarios pass, audit trail correct, messages clear
  - **Estimated**: 3 hours

---

## Phase 8: User Story 6 - Recommendations (12-16 hours)

**JTBD**: Quality-based recommendations help users choose high-quality packages

**Dependencies**: Phase 5 (quality tier implementation)
**Critical**: - (enhancement phase)
**Parallelization**: T045/T046 parallel (independent systems)

### Story Goal

Help users discover and choose high-quality packages through quality-tier recommendations.

### Acceptance Scenarios

1. **Quality Tier Recommendations**: Gold tier packages appear first, sorted by quality metrics

### Tasks

- [ ] **T045** [P] Write recommendation engine tests
  - Test ranking algorithm:
    - Gold tier packages appear first
    - Silver tier packages second
    - Bronze tier packages last
  - Test secondary sorting:
    - Within same tier, sort by download count (descending)
    - Within same tier+downloads, sort by update recency (most recent first)
  - Test edge cases:
    - No gold tier packages → silver packages appear first
    - Single package → returns as is
    - Tied rankings → use secondary sort
  - Achieve 80%+ coverage
  - **File**: `crates/ggen-marketplace/tests/recommendation_tests.rs`
  - **Coverage**: 80%+
  - **Acceptance**: Ranking algorithm correct, tiers prioritized properly, tiebreakers work
  - **Estimated**: 2.5 hours

- [ ] **T046** [P] Implement CLI list and update commands
  - Implement `ggen marketplace list [--format table|json]`:
    - Show installed packages: name, version, installed_at
    - Show FMEA status: passed/failed/warning
    - Sort by quality tier (gold first)
    - Display in table or JSON format
  - Implement `ggen marketplace update`:
    - Check for newer versions of installed packages
    - Generate new lock file with updated versions
    - Show what changed: "upgraded X v1.0 → v1.1"
    - Commit lock file (optional: --auto-commit)
    - Apply quality-aware sorting to recommendations
  - **File**: `crates/ggen-cli/src/commands/marketplace/list.rs + update.rs`
  - **Acceptance**: Commands list/update correctly, quality-aware sorting works
  - **Estimated**: 2 hours

- [ ] **T047** Enhance search with recommendation sorting
  - Update search.rs: sort all results by quality tier
  - Update CLI search command:
    - Add --best-match flag: show only gold and silver tiers
    - Display tier indicators: [GOLD], [SILVER], [BRONZE]
    - Show quality metrics: downloads, last update date
  - Verify SPARQL queries optimized for sorting
  - **File**: `crates/ggen-marketplace/src/gpack/search.rs (sorting) + CLI search.rs`
  - **Dependency**: T045 (recommendation logic)
  - **Acceptance**: Results sorted by quality, --best-match flag works, queries efficient
  - **Estimated**: 2 hours

- [ ] **T048** Write documentation for recommendations and quality tiers
  - Explain quality tiers:
    - **Gold**: FMEA passed + >100 downloads + updated <30 days
    - **Silver**: FMEA passed + 10-100 downloads OR updated 30-90 days
    - **Bronze**: Basic validation only
  - Document recommendation algorithm: how tiers are sorted
  - Show examples:
    - Search results sorted by tier
    - Gold tier packages highlighted
    - --best-match flag showing only quality packages
  - Include guide for package maintainers: "How to reach Gold tier"
    - Maintain FMEA validation
    - Encourage adoption (reach 100+ downloads)
    - Keep package updated
  - **File**: `docs/features/marketplace-gpack.md` (recommendations section)`
  - **Acceptance**: Quality tiers clearly explained, recommendation algorithm described
  - **Estimated**: 1.5 hours

---

## Phase 9: Polish, Testing, Migration & Release (40-56 hours)

**Goal**: Comprehensive validation, migrate 84 existing packages, prepare v5.3.0 release

**Dependencies**: All feature phases (3-8) complete
**Critical**: ✅ Blocking for release
**Parallelization**: T049/T050 parallel (different test layers); T051/T052 sequential

### Tasks

- [ ] **T049** [P] Run comprehensive cross-platform integration tests
  - Configure GitHub Actions CI matrix: ubuntu-latest, macos-latest, windows-latest
  - Run full integration test suite on all platforms:
    - `cargo make test` (all unit tests)
    - Publish acceptance scenario: publish → appears in crates.io
    - Install acceptance scenario: install → works correctly
    - Search acceptance scenario: search → returns results
    - Determinism acceptance scenario: SHA256 hashes match
    - FMEA validation acceptance scenario: validation blocks/allows correctly
  - Verify 100% pass rate across all platforms
  - Document any platform-specific behaviors
  - No flaky tests (run 3 times, all succeed)
  - Achieve 80%+ overall coverage
  - **File**: `crates/ggen-core/tests/` (all test files)
  - **Coverage**: 80%+
  - **Acceptance**: All tests pass on all platforms, no flakiness, determinism verified
  - **Estimated**: 6 hours

- [ ] **T050** [P] Performance benchmarking and optimization
  - Create performance benchmarks for success criteria:
    - **SC-002**: Publish latency ≤30 seconds (test with 3 packages)
    - **SC-003**: Install performance ≤30 seconds (test with 10 representative packages)
    - **SC-004**: Search latency ≤1 second (test 100 concurrent searches)
    - **SC-007**: Lock file generation <5 seconds
  - Profile hot paths using flamegraph:
    - Identify bottlenecks
    - Optimize as needed (async improvements, caching enhancements)
    - Re-benchmark after optimizations
  - Document results: actual vs target
  - No regressions allowed: new code faster than baseline
  - **File**: `crates/ggen-marketplace/benches/` (benchmark suite)
  - **Acceptance**: All SLOs met, results documented, no regressions from target
  - **Estimated**: 4 hours

- [ ] **T051** Create migration tool and migrate 84 existing packages
  - Create migration script/tool:
    - Input: list of 84 packages (from baseline analysis in spec phase)
    - For each package:
      - Generate gpack manifest from existing metadata
      - Create Cargo.toml for crates.io
      - Validate manifest schema
      - Test serialization
      - Publish to crates.io test registry first (verify format)
      - Publish to public crates.io
  - Document migration status:
    - Success count: "82 of 84 packages migrated"
    - Failures with reasons: "2 packages missing FMEA"
    - Publish latencies: "average 15 seconds"
  - Test backward compatibility:
    - Old command `ggen marketplace install pkg` still works (for legacy packages)
    - New command `ggen marketplace install pkg-gpack` works (for gpack packages)
  - **File**: `scripts/migrate_packages.rs + crates/ggen-core/tests/migration_tests.rs`
  - **Dependency**: T049 (tests pass)
  - **Acceptance**: 84 packages migrated, all tests pass, old packages still work
  - **Estimated**: 12 hours

- [ ] **T052** Create release notes and finalize documentation
  - Write comprehensive release notes for v5.3.0:
    - Feature overview: "Marketplace retrofitted to use crates.io distribution"
    - New commands: `ggen marketplace publish`, enhanced `install`, enhanced `search`
    - Migration guide: "How to migrate existing packages to gpack format"
    - Breaking changes: NONE (100% backward compatible)
    - Known limitations: document any gaps
    - Performance improvements: summarize benchmarks
  - Finalize all documentation:
    - docs/features/marketplace-gpack.md (complete with all sections)
    - Update main README.md with gpack feature
    - Update CHANGELOG.md with detailed changes
    - Create FAQ: common questions and answers
  - Prepare for release:
    - Tag v5.3.0 in git
    - Verify all quality gates pass: cargo make ci
    - Verify all 7 success criteria (SC-001 through SC-007) met
    - Prepare announcement for community
  - **File**: `RELEASE_NOTES.md + docs/ (all sections) + CHANGELOG.md + README.md`
  - **Dependency**: T051 (migration complete)
  - **Acceptance**: Release notes complete, docs updated, v5.3.0 ready
  - **Estimated**: 3 hours

---

## Success Criteria Mapping

Each task contributes to measurable success criteria from spec.md:

| Success Criterion | Measurement | Phase/Task | Verification |
|---|---|---|---|
| **SC-001**: 100% backward compatibility | 84 packages convert | Phase 9 / T051 | Migration test pass |
| **SC-002**: Publish latency ≤30s | Publish 3 test packages | Phase 9 / T050 | Performance benchmark |
| **SC-003**: Install ≤30 seconds | Benchmark 10 packages | Phase 9 / T050 | Performance benchmark |
| **SC-004**: Search ≤1 second | 100 concurrent searches | Phase 9 / T050 | Performance benchmark |
| **SC-005**: 100% FMEA coverage | Audit trail analysis | Phase 7 / T044 | Audit trail queries |
| **SC-006**: Zero breaking changes | CLI integration tests | Phase 9 / T049 | Cross-platform tests |
| **SC-007**: Deterministic distribution | SHA256 match across OS | Phase 6 / T037 | Cross-platform tests |

---

## Quality Gates

Each phase includes quality checkpoints:

### Phase 1-2: Foundation
- ✅ Cargo check passes
- ✅ All dependencies verified
- ✅ Test infrastructure working

### Phase 3: Publish
- ✅ Publish 3 test packages successfully
- ✅ Packages appear in crates.io search
- ✅ FMEA requirement enforced

### Phase 4: Install
- ✅ Can install published packages
- ✅ Dependency resolution works
- ✅ Lock file generation deterministic
- ✅ FMEA validation blocks critical failures

### Phase 5: Search
- ✅ SPARQL queries return results <1 second
- ✅ Quality tier indicators displayed
- ✅ 100 concurrent searches handled

### Phase 6: Determinism
- ✅ Lock files deterministic (same input → same output)
- ✅ Cross-platform hashes match (Linux == macOS == Windows)

### Phase 7: Validation
- ✅ FMEA validation enforced
- ✅ Critical failures block by default
- ✅ --force-fmea override works with audit trail

### Phase 8: Recommendations
- ✅ Quality tiers computed and displayed correctly
- ✅ Search results sorted by quality

### Phase 9: Release
- ✅ All tests pass on all platforms (80%+ coverage)
- ✅ Performance benchmarks meet SLOs
- ✅ 84 packages successfully migrated
- ✅ All 7 success criteria verified
- ✅ Documentation complete
- ✅ No breaking changes

---

## Task Checklist Format

All tasks follow this format for clarity:

```
- [ ] **Txxx** [P?] [Story?] Task Title
  - Description with specific actions and file paths
  - **File**: Path(s) affected
  - **Dependency**: Task(s) that must complete first
  - **Parallelizable**: true/false
  - **Acceptance**: Clear, testable acceptance criteria
  - **Estimated**: Hours to complete
```

**Legend**:
- `[P]` = Parallelizable (can run concurrent with other [P] tasks in same phase)
- `[Story]` = Related to specific user story (US1, US2, etc.)
- Dependent tasks shown with arrows in sequential sections

---

## Team Execution Strategy

**Recommended: 10-agent parallel swarm** (following v5.2.0 pattern)

### Agent Assignments

1. **Agent 1: Architect Lead**
   - Oversees all phases, makes architectural decisions
   - Focuses: Phase 1-2 setup, overall design review
   - Hands off: domain/CLI implementation once design complete

2. **Agents 2-3: Domain Developers** (parallel)
   - Build domain layer: format, manifest, resolver, validator, cache
   - Focuses: Phase 2-5 domain modules (T005-T010, T011, T019-T021, T029-T030, etc.)
   - Coordinate on shared interfaces

3. **Agents 4-5: CLI Developers** (parallel)
   - Build CLI layer: publish, install, search, list, update commands
   - Focuses: Phase 3-8 CLI commands (T016, T024, T032, T046-T047, etc.)
   - Integrate domain functions from domain developers

4. **Agents 6-7: Test Engineers** (parallel)
   - Write Chicago TDD tests for all components
   - Focuses: All test tasks (T014-T015, T023, T026, T031, T036-T037, T040-T041, T045, T049)
   - Ensure 80%+ coverage across all code

5. **Agent 8: Documentation Writer**
   - Write user-facing documentation concurrently with development
   - Focuses: T018, T027, T035, T039, T043, T048, T052
   - Final polish in Phase 9

6. **Agent 9: Integration Engineer**
   - Handle integration, CI, cross-cutting concerns
   - Focuses: T004 (CI setup), T028 (audit integration), T033 (metadata sync), T051 (migration), T049-T052 (release)

7. **Agent 10: Performance Engineer**
   - Benchmarking, optimization, profiling
   - Focuses: T022 (determinism), T050 (performance), T051 (migration testing)

---

## How to Execute This Plan

### Using `/speckit.implement`

```bash
/speckit.implement 014
```

This will:
1. Load this tasks.ttl as source of truth
2. Parse task dependencies and phases
3. Launch 10-agent swarm with coordinated execution
4. Track progress across phases
5. Report completion with results

### Manual Execution

1. **Phase 1**: Complete all 4 setup tasks sequentially
2. **Phase 2**: Run T005-T010 in parallel (all independent)
3. **Phase 3**: Run T011/T013/T014 parallel, then T012 (depends on T011), then T016-T018
4. **Phase 4**: Run T019/T020/T021/T023 parallel, then dependent tasks
5. **Phase 5**: Can start after Phase 4 completes; T029/T030/T031 parallel
6. **Phases 6-8**: Follow similar pattern based on dependencies
7. **Phase 9**: Sequential (depends on all prior phases)

### Progress Tracking

Use TodoWrite to track completion:
```
[x] T001 - Create module structure
[x] T002 - Add dependencies
[x] T003 - Test infrastructure
[x] T004 - CI setup
[x] T005-T010 - Foundation types (6/6 complete)
...
```

---

## Expected Deliverables

### After Phase 1: Foundation Ready
- Project structure initialized
- Dependencies verified
- Test infrastructure ready
- CI/CD configured

### After Phase 2: Domain Models Complete
- Core types defined: GpackManifest, LockFile, Error, Cache, Validator types
- All unit tests passing
- Ready for feature implementation

### After Phase 3: Publish Feature Complete
- Can publish packages to crates.io
- Manifests validated
- FMEA requirement enforced
- Acceptance scenario: "Basic Publication" passes

### After Phase 4: Install Feature Complete
- Can install packages from crates.io
- Dependency resolution works
- Lock files generated
- FMEA validation enforced
- Offline support working
- Acceptance scenarios: "Basic Installation", "Installation Validation", "Offline Installation" pass

### After Phase 5: Search Feature Complete
- SPARQL-based search working
- Quality tiers computed and displayed
- Acceptance scenarios: "Basic Search", "Search Filtering", "Quality Indicators" pass

### After Phase 6: Determinism Verified
- Lock files deterministic
- Cross-platform hashes match
- Conflicts detected with suggestions

### After Phase 7: Validation Enhanced
- FMEA validation comprehensive
- Guards applied and tested
- --force-fmea override safe with audit trail

### After Phase 8: Recommendations Working
- Quality-based sorting implemented
- List and update commands working
- Recommendations displayed in search

### After Phase 9: Release Ready
- All tests pass on all platforms
- Performance SLOs met
- 84 packages migrated
- v5.3.0 release ready
- All 7 success criteria verified
- Zero breaking changes confirmed

---

## Notes for Implementation Team

1. **Task Independence**: Each task is independently verifiable. Complete a task fully before moving to dependent tasks.

2. **Quality First**: No task is "done" until tests pass. Chicago TDD required for all code.

3. **Parallel Execution**: [P] marked tasks can run concurrently. No [P] = must wait for dependencies.

4. **Documentation**: Write docs concurrently with code. Don't defer documentation to end.

5. **Testing**: Tests should be written before or alongside implementation (Chicago TDD).

6. **Determinism**: Especially important in Phase 6. Test across platforms early and often.

7. **Success Criteria**: Each task contributes to success criteria (SC-001 through SC-007). Track progress against these.

8. **Risk Mitigation**: R-001 (determinism) and R-005 (backward compatibility) are highest risks. Test aggressively.

---

**Generated with**: ggen v6 ontology-driven task system
**Source**: `specs/014-marketplace-gpack/ontology/tasks.ttl` (RDF/Turtle)
**Format**: Markdown (generated artifact - DO NOT EDIT MANUALLY)
**Quality**: Lean Six Sigma (99.99966% target)
**Status**: ✅ Ready for Implementation via `/speckit.implement`
