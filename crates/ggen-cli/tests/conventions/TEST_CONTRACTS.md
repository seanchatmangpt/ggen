# Convention-Based Routing Test Contracts
## London TDD Implementation Guide for ggen v2.2.0

**Status**: 🔴 RED PHASE - Tests written, awaiting implementation

## Overview

This document defines the **contracts** that implementation agents must fulfill to make the convention-based routing tests pass. Following London TDD methodology, we've written tests with mocks that specify the **interactions** and **collaborations** between components.

## Test Suite Structure

```
cli/tests/conventions/
├── fixtures.rs          # Mock definitions and test data
├── resolver_tests.rs    # ConventionResolver contract tests (11 tests)
├── planner_tests.rs     # GenerationPlanner contract tests (14 tests)
├── integration_tests.rs # End-to-end workflow tests (10 tests)
└── TEST_CONTRACTS.md    # This document
```

## Component Contracts

### 1. ConventionResolver (11 tests)

**Purpose**: Discover and resolve files following ggen conventions.

**Required Trait**:
```rust
trait ConventionResolver {
    fn discover_rdf_files(&self, config: &ConventionConfig) -> Result<Vec<PathBuf>>;
    fn discover_templates(&self, config: &ConventionConfig) -> Result<Vec<PathBuf>>;
    fn discover_queries(&self, config: &ConventionConfig) -> Result<Vec<PathBuf>>;
    fn resolve_output_directory(&self, config: &ConventionConfig) -> Result<PathBuf>;
    fn load_config_overrides(&self, project_root: &Path) -> Result<Option<ConventionConfig>>;
}
```

**Behaviors to Implement**:

1. **Alphabetical Ordering** (`test_discover_rdf_files_alphabetical_order`)
   - Sort discovered RDF files alphabetically
   - Expected: `[config.ttl, posts.ttl, users.ttl]`

2. **Numbered Ordering** (`test_discover_rdf_files_numbered_ordering`)
   - Respect numeric prefixes (01-, 05-, 10-)
   - Expected: `[01-schema.ttl, 05-config.ttl, 10-users.ttl]`

3. **Recursive Discovery** (`test_discover_templates_nested_structure`)
   - Find templates in nested directories
   - Only match `.hbs` extensions
   - Return sorted alphabetically

4. **Query Discovery** (`test_discover_queries_by_name`)
   - Find `.sparql` files
   - Ignore non-query files (README.md, etc.)

5. **Output Directory Resolution** (`test_resolve_output_directory_convention`)
   - Default to `src/` directory
   - Validate directory exists and is writable

6. **Config Override Loading** (`test_override_conventions_from_dotggen`)
   - Parse `.ggen` TOML file
   - Override default conventions
   - Support `[conventions]` section with:
     - `rdf_dir`
     - `templates_dir`
     - `queries_dir`
     - `output_dir`
     - `preset`

7. **Empty Directory Handling** (`test_empty_directories_handled`)
   - Return empty vec, not error
   - Don't fail on missing directories

8. **Extension Filtering** (`test_invalid_file_extensions_ignored`)
   - Only include valid extensions (.ttl, .hbs, .sparql)
   - Ignore backup files, READMEs, etc.

9. **Symlink Resolution** (`test_discover_handles_symlinks`)
   - Canonicalize paths
   - Follow symlinks correctly

10. **Case-Insensitive Extensions** (`test_case_insensitive_extension_matching`)
    - Match .ttl, .TTL, .Ttl equally

**Mock Interactions Expected**:
- `MockFileSystem::is_dir()` - Check if path is directory
- `MockFileSystem::read_dir()` - List directory contents
- `MockFileSystem::is_file()` - Check if path is file
- `MockFileSystem::read_to_string()` - Read config files
- `MockFileSystem::exists()` - Verify path exists
- `MockFileSystem::canonicalize()` - Resolve symlinks

### 2. GenerationPlanner (14 tests)

**Purpose**: Build execution plans for template generation.

**Required Trait**:
```rust
trait PlannerService {
    fn build_generation_plan(&self, config: &ConventionConfig) -> Result<GenerationPlan>;
    fn resolve_template_dependencies(&self, templates: &[TemplateMetadata]) -> Result<Vec<TemplateMetadata>>;
    fn parse_template_metadata(&self, path: &Path) -> Result<TemplateMetadata>;
    fn match_when_triggers(&self, template: &TemplateMetadata, rdf_files: &[PathBuf]) -> Vec<PathBuf>;
    fn link_queries(&self, template: &TemplateMetadata, queries: &[PathBuf]) -> Option<PathBuf>;
    fn detect_circular_dependencies(&self, templates: &[TemplateMetadata]) -> Result<()>;
}
```

**Behaviors to Implement**:

1. **Plan Building** (`test_build_generation_plan_from_conventions`)
   - Coordinate resolver outputs
   - Build structured GenerationPlan
   - Map RDF files to outputs

2. **Dependency Resolution** (`test_resolve_template_dependencies`)
   - Topologically sort templates
   - Respect `depends_on` metadata
   - Base templates before dependents

3. **Frontmatter Parsing** (`test_template_metadata_parsing`)
   - Parse YAML frontmatter
   - Extract: name, mode, when, output, query, depends_on
   - Separate metadata from template body

4. **ForEach vs Once** (`test_foreach_vs_once_generation`)
   - `mode: foreach` → one output per RDF file
   - `mode: once` → single output
   - Handle both modes correctly

5. **When Trigger Matching** (`test_when_trigger_file_matching`)
   - Match glob patterns (`**/*user*.ttl`)
   - Filter RDF files by pattern
   - Return matched files only

6. **Query Linking** (`test_query_linking_by_name`)
   - Link `query: get_users` to `queries/get_users.sparql`
   - Match by filename stem

7. **Incremental Planning** (`test_incremental_planning`)
   - Add new templates to existing plan
   - Don't regenerate unchanged outputs

8. **Circular Dependency Detection** (`test_circular_dependency_detection`)
   - Detect cycles: A → B → A
   - Return error with cycle path

9. **Parallel Planning** (`test_parallel_generation_planning`)
   - Identify independent templates
   - Mark parallelizable tasks

10. **Output Path Interpolation** (`test_output_path_interpolation`)
    - Replace `{{filename}}` with RDF filename
    - Replace `{{name}}` with extracted name
    - Support nested interpolation

11. **Preset Overrides** (`test_preset_convention_override`)
    - Apply preset conventions
    - Support `clap-noun-verb` preset

12. **Conditional Skip** (`test_conditional_generation_skip`)
    - Skip templates when `when` doesn't match
    - No error, just skip

**Mock Interactions Expected**:
- `MockTemplateEngine::parse_frontmatter()` - Extract metadata
- `MockGenerationPlanner::build_plan()` - Orchestrate planning
- `MockGenerationPlanner::resolve_dependencies()` - Sort dependencies

### 3. Integration Tests (10 tests)

**Purpose**: End-to-end workflows with real filesystem.

**Scenarios to Implement**:

1. **Zero-Config Generation** (`test_zero_config_project_generation`)
   - Auto-discover standard layout
   - No `.ggen` config required
   - Generate outputs correctly

2. **Clap-Noun-Verb Preset** (`test_convention_preset_clap_noun_verb`)
   - Apply preset conventions
   - Handle CLI command structure
   - Generate `src/commands/user_create.rs`

3. **Complete Auto-Discovery** (`test_auto_discovery_complete_workflow`)
   - Discover all files
   - Respect ordering (01-, 02-)
   - Process nested templates
   - Link queries by name
   - Execute full pipeline

4. **Metadata to Output** (`test_template_metadata_to_output`)
   - Parse frontmatter
   - Match when triggers
   - Link queries
   - Interpolate output paths
   - Generate at correct location

5. **Incremental Caching** (`test_incremental_generation_caching`)
   - Skip unchanged files
   - Compare timestamps
   - Cache hit → no regeneration

6. **Config Precedence** (`test_convention_override_precedence`)
   - CLI args > .ggen > defaults
   - Merge configs correctly

7. **Error: Missing Query** (`test_error_handling_missing_query`)
   - Helpful error messages
   - Suggest available queries

8. **Parallel Execution** (`test_parallel_template_execution`)
   - Execute independent templates concurrently
   - Use rayon for parallelism

## Data Structures

### ConventionConfig
```rust
pub struct ConventionConfig {
    pub rdf_dir: PathBuf,
    pub templates_dir: PathBuf,
    pub queries_dir: PathBuf,
    pub output_dir: PathBuf,
    pub preset: Option<String>,
}
```

### TemplateMetadata
```rust
pub struct TemplateMetadata {
    pub name: String,
    pub path: PathBuf,
    pub mode: GenerationMode,
    pub when_trigger: Option<String>,
    pub output_path: Option<String>,
    pub query: Option<String>,
    pub dependencies: Vec<String>,
}
```

### GenerationMode
```rust
pub enum GenerationMode {
    ForEach,  // Generate once per RDF file
    Once,     // Generate single output
}
```

### GenerationPlan
```rust
pub struct GenerationPlan {
    pub templates: Vec<TemplateMetadata>,
    pub rdf_files: Vec<PathBuf>,
    pub output_mappings: Vec<(PathBuf, PathBuf)>,
}
```

## Implementation Location

Create implementation files at:
- `/Users/sac/ggen/cli/src/conventions/resolver.rs`
- `/Users/sac/ggen/cli/src/conventions/planner.rs`
- `/Users/sac/ggen/cli/src/conventions/generator.rs`

## Mock Filesystem Abstraction

To pass tests, implement a filesystem trait:

```rust
pub trait FileSystem {
    fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>>;
    fn read_to_string(&self, path: &Path) -> Result<String>;
    fn exists(&self, path: &Path) -> bool;
    fn is_file(&self, path: &Path) -> bool;
    fn is_dir(&self, path: &Path) -> bool;
    fn canonicalize(&self, path: &Path) -> Result<PathBuf>;
}

// Production implementation
pub struct RealFileSystem;

// Test implementation uses MockFileSystem from fixtures.rs
```

## Test Execution Order

1. **Phase 1**: Implement ConventionResolver
   - Run: `cargo test conventions::resolver_tests`
   - Goal: 11/11 tests passing

2. **Phase 2**: Implement GenerationPlanner
   - Run: `cargo test conventions::planner_tests`
   - Goal: 14/14 tests passing

3. **Phase 3**: Integration
   - Run: `cargo test conventions::integration_tests`
   - Goal: 10/10 tests passing

## Running Tests

```bash
# All convention tests
cargo test --package ggen-cli-lib conventions

# Specific suite
cargo test --package ggen-cli-lib conventions::resolver_tests
cargo test --package ggen-cli-lib conventions::planner_tests
cargo test --package ggen-cli-lib conventions::integration_tests

# Single test
cargo test --package ggen-cli-lib test_discover_rdf_files_alphabetical_order
```

## Success Criteria

- ✅ All 35 tests compile without errors
- ✅ All tests are in RED phase (failing as expected)
- ✅ Mock interactions clearly define contracts
- ✅ Implementation agents can reference this doc
- ✅ Tests guide implementation design

## Next Steps for Implementation Agents

1. **Create module structure**:
   ```bash
   mkdir -p /Users/sac/ggen/cli/src/conventions
   touch /Users/sac/ggen/cli/src/conventions/{mod.rs,resolver.rs,planner.rs,generator.rs}
   ```

2. **Implement FileSystem trait** in `resolver.rs`

3. **Implement ConventionResolver** with all 11 behaviors

4. **Implement PlannerService** with all 14 behaviors

5. **Wire up integration** in `generator.rs`

6. **Run tests incrementally** after each implementation

7. **Refactor to GREEN** once all tests pass

## London TDD Benefits

- ✅ **Design by contract**: Mocks define interfaces
- ✅ **Fast feedback**: Tests run without filesystem I/O
- ✅ **Clear expectations**: Each test documents one behavior
- ✅ **Implementation guidance**: Tests show how components collaborate
- ✅ **Refactoring safety**: Tests verify interactions, not implementation

---

**Generated by**: London TDD Swarm Agent
**Date**: 2025-11-02
**Target Version**: ggen v2.2.0
**Status**: 🔴 RED - Awaiting implementation
