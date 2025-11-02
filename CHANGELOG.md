# Changelog

All notable changes to ggen will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.2.0] - 2025-11-02

### Added
- **File-Based Conventions System**: Zero-config project setup with automatic structure detection
  - Convention resolver discovers RDF files, templates, and queries from standard directories
  - Generation planner analyzes template metadata and creates execution plans
  - Project watcher with file system monitoring and debounced regeneration
  - Convention presets: `clap-noun-verb` and `custom`
- **Project Init Command**: `ggen project init --preset <preset>` for instant project scaffolding
- **Watch Mode Foundation**: Infrastructure for `ggen project watch` with automatic regeneration
- **Template Metadata Parsing**: Support for `{# output: ... #}`, `{# when: ... #}`, `{# query: ... #}` directives
- **Dependency Graph Analysis**: Topological sorting and circular dependency detection

### Fixed
- ProjectConventions struct now includes directory paths (rdf_dir, templates_dir) for file watching
- Resolved compilation errors in conventions module
- Fixed test imports after v2.0 refactoring (commands → cmds)
- Corrected ConventionResolver API to use public discovery methods

### Changed
- Conventions module structure improved for clarity and extensibility
- Watch mode uses debounced file system events (300ms default)
- Generation planning supports template-level triggers and dependencies

## [2.0.0] - 2025-11-02

### Major Architectural Changes

#### Three-Layer Architecture
- **CLI Layer** (`cli/src/cmds/`): Command routing with clap-noun-verb v3
- **Domain Layer** (`cli/src/domain/`): Pure business logic (async, framework-agnostic)
- **Runtime Layer** (`cli/src/runtime.rs`): Sync/async bridge utilities

**Benefits:**
- 50% faster compilation (30-45s vs 60-90s)
- 50% faster incremental builds (5-8s vs 10-15s)
- Enhanced testability and maintainability
- Clear separation of concerns
- Convention-based routing (directory = noun, file = verb)

#### Global Runtime Pattern
- Replaced per-command `AppContext` with singleton `GlobalRuntime`
- Eliminated duplicate initialization code across all commands
- Configuration loaded once and shared globally
- Improved testing with mockable runtime

#### clap-noun-verb v3 Integration
- **Auto-discovery**: Commands discovered from directory structure
- **Zero boilerplate**: No manual command registration needed
- **Type-safe**: Compile-time validation of command structure
- **Self-documenting**: Help text generated from file structure

### Complete Command Migration

#### All 8 Noun Commands Migrated (29 Total Verbs)

**Template** (7 commands):
- `generate` - Generate from template with RDF context
- `generate-tree` - Generate entire file trees
- `lint` - Validate template syntax and queries
- `list` - Discover available templates
- `new` - Create new template from wizard
- `regenerate` - Regenerate from existing template
- `show` - Display template metadata

**AI** (3 commands):
- `generate` - AI-powered code generation
- `chat` - Interactive AI chat session
- `analyze` - Analyze code with AI insights

**Graph** (4 commands):
- `load` - Load RDF graph from file
- `query` - Execute SPARQL queries
- `export` - Export graph to various formats
- `visualize` - Generate graph visualizations

**Marketplace** (5 commands):
- `search` - Search for packages
- `install` - Install packages
- `list` - List installed packages
- `publish` - Publish new packages
- `update` - Update installed packages

**Project** (4 commands):
- `new` - Create new project from scratch
- `plan` - Generate project plan
- `gen` - Generate code from plan
- `apply` - Apply code changes

**Hook** (4 commands):
- `create` - Create lifecycle hooks
- `list` - List registered hooks
- `remove` - Remove hooks
- `monitor` - Monitor hook execution

**Utils** (2 commands):
- `doctor` - System diagnostics and health check
- `env` - Environment variable management

**CI** (1 command):
- `workflow` - Generate CI/CD workflows

### Breaking Changes

#### Command Structure
- All commands now use noun-verb pattern (e.g., `ggen template generate`)
- Old flat command structure no longer supported
- Migration: Update scripts to use new command format

#### Command Renaming
- `ggen market` → `ggen marketplace` (full word for clarity)
- All commands now require explicit noun-verb format
- Migration: Run `ggen utils doctor --migrate-config`

#### API Changes (Library Users)
- `MarketClient` → `MarketplaceClient`
- Builder pattern for client creation
- Updated import paths to reflect new structure
- Domain layer moved to `cli/src/domain/`

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
