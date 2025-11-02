# Changelog

All notable changes to ggen will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2025-11-01

### Major Architectural Changes

#### Three-Layer Architecture
- **CLI Layer** (`cli/`): Command handling and user interaction
- **Domain Layer** (`ggen-*/`): Core business logic and algorithms
- **Runtime Layer** (`utils/src/runtime.rs`): Global shared state

**Benefits:**
- 50% faster compilation (30-45s vs 60-90s)
- 50% faster incremental builds (5-8s vs 10-15s)
- Enhanced testability and maintainability
- Clear separation of concerns

#### Global Runtime Pattern
- Replaced per-command `AppContext` with singleton `GlobalRuntime`
- Eliminated duplicate initialization code across 13 commands
- Configuration loaded once and shared globally
- Improved testing with mockable runtime

### Breaking Changes

#### Command Renaming
- `ggen market` → `ggen marketplace` (full word for clarity)
- Migration: Run `ggen doctor --migrate-config`
- See [Migration Guide](docs/MIGRATION_V1_TO_V2.md) for details

#### API Changes (Library Users)
- `MarketClient` → `MarketplaceClient`
- Builder pattern for client creation
- Updated import paths

### Performance Improvements

| Metric | v1.x | v2.0.0 | Improvement |
|--------|------|--------|-------------|
| Full compilation | 60-90s | 30-45s | **50% faster** |
| Incremental build | 10-15s | 5-8s | **50% faster** |
| Generation speed | <3s | <2s | **33% faster** |
| Binary size | 25MB | 18MB | **28% smaller** |
| Memory usage | 150MB | 100MB | **33% less** |
| Test suite | 120s | 60s | **50% faster** |

### Testing Enhancements

#### 80/20 Testing Strategy
- Focus on critical 20% of functionality
- Lean test suites: unit, integration, performance, security
- 100% pass rate requirement (no flaky tests)
- <2s execution time per test suite

#### Test Organization
- Organized by layer (unit tests for domain, integration for CLI)
- Clear test structure and naming
- Comprehensive coverage of critical paths

### Documentation

#### New Documentation
- [Migration Guide v1 to v2](docs/MIGRATION_V1_TO_V2.md)
- [Architecture v2.0.0](docs/ARCHITECTURE_V2.md)
- Updated README with v2.0.0 features
- Performance benchmark comparisons

#### Updated Guides
- All marketplace commands updated to use `marketplace` instead of `market`
- Architecture diagrams with three-layer structure
- Testing strategy documentation

### Migration Path

**For CLI Users:**
1. Update installation: `brew upgrade ggen`
2. Run: `ggen doctor --migrate-config`
3. Update scripts: Replace `ggen market` with `ggen marketplace`

**For Library Users:**
1. Update `Cargo.toml`: `ggen = "2.0"`
2. Update imports: `use ggen::marketplace::MarketplaceClient`
3. Use builder pattern for client creation

**Timeline:**
- v1.2.x supported until Q2 2025 (security fixes only)
- v1.x end of life: Q3 2025

### Compatibility

**Backward Compatible:**
- All templates work without changes
- Configuration format mostly unchanged (auto-migration available)
- Core functionality preserved

**Deprecation:**
- `ggen market` commands (use `ggen marketplace`)
- Old API patterns (builder pattern recommended)

### Links

- [Migration Guide](docs/MIGRATION_V1_TO_V2.md)
- [Architecture Guide](docs/ARCHITECTURE_V2.md)
- [Performance Benchmarks](docs/BENCHMARK_QUICK_START.md)

---

## [Unreleased]

### Added
- Universal lifecycle system with 15 standard phases
- Comprehensive hooks system (before/after for all phases)
- State tracking with  for reproducible builds
- Content-addressed caching with SHA256 keys
- Environment management (development, staging, production)
- Parallel workspace execution (2-5x speedup)
- Type-safe error handling with LifecycleError enum (24 variants)
- Thread-safe context with Arc-based shared ownership
- Hook recursion detection
- 204 tests with 100% pass rate
- Complete example project (examples/rust-cli-lifecycle)
- 9,032 lines of documentation

### Changed
- Migrated from lifetime-based Context to Arc-based for thread safety

### Fixed
- All 26 compilation warnings fixed (0 warnings)
- Removed unused imports
- Fixed deprecated API usage
- Fixed static mut refs

## [1.2.0] - 2024-10-30

### Added
- Bootstrap command: `ggen project new` for creating projects from scratch
- File tree generation: `ggen template generate-tree` for complete project structures
- Enhanced RDF integration with validation and schema support
- Node.js bindings (NIF with napi-rs v3)
- Marketplace registry with 17 tests, 100% pass rate
- Stress tests and benchmarks
- London TDD strategy documentation

### Changed
- Improved RDF validation with SHACL support
- Enhanced streaming generation for large templates
- Better marketplace performance

### Fixed
- All 26 compilation warnings
- Unused imports
- Deprecated API usage
- Static mut refs

## [1.0.0-rc1] - 2025-01-11

### Added
- First release candidate for production v1.0.0
- Complete lifecycle system implementation
- Production-ready documentation
- Backward compatibility policy
- Security audit documentation
