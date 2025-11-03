# ggen v3.0.0 - Complete Functionality Audit & Porting Plan

## Purpose

This document provides a complete inventory of all functionality in the current ggen codebase, its location, dependencies, and rationale for porting to the v3.0.0 crates pattern structure. This audit must be completed before any refactoring begins.

---

## Current Structure Overview

### Workspace Members (v2.4.0)

```
Root:
├── cli/                    # CLI binary and command definitions
├── domain/                 # Minimal domain crate (only hooks)
├── ggen-core/              # Core infrastructure (templates, RDF, pipeline)
├── ggen-ai/                # AI/LLM integration
├── ggen-marketplace/       # Package marketplace
├── utils/                  # Shared utilities
└── node/                   # Node.js bindings
```

### Target Structure (v3.0.0)

```
Root:
└── crates/
    ├── ggen-cli/           # CLI presentation layer (from cli/)
    ├── ggen-domain/        # Domain logic layer (NEW - from cli/src/domain/)
    ├── ggen-core/          # Infrastructure (from ggen-core/)
    ├── ggen-ai/            # AI integration (from ggen-ai/)
    ├── ggen-marketplace/   # Marketplace (from ggen-marketplace/)
    ├── ggen-utils/         # Shared utilities (from utils/)
    └── ggen-node/          # Node.js bindings (from node/)
```

---

## Part 1: CLI Crate Analysis (`cli/`)

### Current Location: `cli/`

### What Exists

#### 1. CLI Command Layer (`cli/src/cmds/`) - **9 files, 8 nouns**

**Purpose**: Command-line argument parsing and routing using clap-noun-verb v3.3.0

**Files**:
- `mod.rs` - Main CLI router, defines `Cli` struct with 8 noun commands
- `ai.rs` - AI noun (analyze, chat, generate)
- `graph.rs` - Graph noun (load, query, export, visualize)
- `hook.rs` - Hook noun (create, list, remove, monitor)
- `marketplace.rs` - Marketplace noun (search, install, list, publish, update)
- `project.rs` - Project noun (new, gen, apply, plan, init, generate, watch)
- `template.rs` - Template noun (generate, generate-tree, lint, list, new, regenerate, show)
- `utils.rs` - Utils noun (doctor, env)
- `ci.rs` - CI noun (workflow)

**Functionality**:
- Parses command-line arguments using `clap::Args`
- Routes to domain layer via `crate::domain::*`
- Synchronous wrappers that bridge to async domain functions
- Uses `crate::runtime::execute()` for async/sync bridge
- Auto-discovery via clap-noun-verb macros

**Dependencies**:
- `clap`, `clap-noun-verb`, `clap-noun-verb-macros`
- `ggen-utils` (for error types)
- `crate::domain` (internal domain layer - **THIS NEEDS TO CHANGE**)

**Porting Rationale**:
- Must stay in CLI layer (presentation)
- Should NOT depend on internal domain - must depend on `ggen-domain` crate
- Needs runtime bridge for async/sync

**Destination**: `crates/ggen-cli/src/cmds/`

---

#### 2. Domain Logic Layer (`cli/src/domain/`) - **53 files, 10 modules**

**Purpose**: Pure business logic, testable without CLI concerns

**Modules**:

**2.1. AI Domain (`cli/src/domain/ai/`)** - 3 files
- `mod.rs` - Module exports
- `analyze.rs` - AI code analysis logic
- `generate.rs` - AI code generation logic

**Functionality**:
- `analyze_code()` - Analyzes code with AI insights
- `generate_code()` - Generates code from prompts
- Uses `ggen-ai` crate for LLM integration

**Dependencies**:
- `ggen-ai` (for LLM client)
- `ggen-utils` (for error types)
- `tokio` (async runtime)
- `serde` (serialization)

**Porting Rationale**:
- Pure business logic - no CLI dependencies
- Currently mixed in `cli/` crate - violates separation
- Must be extracted to standalone `ggen-domain` crate
- CLI layer should only call via stable interface

**Destination**: `crates/ggen-domain/src/ai/`

---

**2.2. Graph Domain (`cli/src/domain/graph/`)** - 4 files
- `mod.rs` - Module exports
- `load.rs` - RDF graph loading
- `query.rs` - SPARQL query execution
- `export.rs` - Graph export functionality
- `visualize.rs` - Graph visualization

**Functionality**:
- `load_rdf()` - Loads RDF/TTL files into graph
- `execute_sparql()` - Executes SPARQL queries
- `export_graph()` - Exports graph to various formats
- `visualize_graph()` - Generates graph visualizations

**Dependencies**:
- `ggen-core::Graph` - Uses ggen-core infrastructure
- `ggen-utils` - Error types
- `oxigraph` - RDF processing (via ggen-core)

**Porting Rationale**:
- Business logic that uses infrastructure
- No CLI dependencies
- Uses ggen-core (infrastructure layer) - correct dependency
- Should be in domain crate, not CLI crate

**Destination**: `crates/ggen-domain/src/graph/`

---

**2.3. Marketplace Domain (`cli/src/domain/marketplace/`)** - 10 files
- `mod.rs` - Module exports
- `search.rs` - Package search logic
- `install.rs` - Package installation logic
- `publish.rs` - Package publishing logic
- `list.rs` - Package listing logic
- `update.rs` - Package update logic
- `registry.rs` - Registry management
- `p2p.rs` - P2P marketplace functionality
- `p2p_state.rs` - P2P state management
- Backup files: `install.rs.bak`, `search.rs.bak`, `update.rs.bak2`

**Functionality**:
- `search_packages()` - Search marketplace for packages
- `install_package()` - Install packages with dependency resolution
- `publish_package()` - Publish packages to registry
- `list_packages()` - List installed/available packages
- `update_package()` - Update packages to latest versions
- P2P marketplace integration

**Dependencies**:
- `ggen-marketplace` - Marketplace backend
- `ggen-utils` - Error types
- `tokio` - Async runtime

**Porting Rationale**:
- Business logic for marketplace operations
- Uses ggen-marketplace infrastructure - correct
- No CLI dependencies
- Currently in CLI crate - needs extraction

**Destination**: `crates/ggen-domain/src/marketplace/`

---

**2.4. Template Domain (`cli/src/domain/template/`)** - 10 files
- `mod.rs` - Module exports
- `generate.rs` - Template generation
- `generate_tree.rs` - File tree generation
- `generate_rdf.rs` - RDF-based generation
- `render_with_rdf.rs` - Template rendering with RDF context
- `lint.rs` - Template linting
- `list.rs` - Template listing
- `new.rs` - Template creation
- `regenerate.rs` - Template regeneration
- `show.rs` - Template metadata display

**Functionality**:
- `generate_template()` - Generates files from templates
- `generate_tree()` - Generates entire file trees
- `render_with_rdf()` - Renders templates with RDF context
- `lint_template()` - Validates template syntax
- `list_templates()` - Discovers and lists templates
- `create_template()` - Creates new templates

**Dependencies**:
- `ggen-core::Template`, `ggen-core::Generator`, `ggen-core::Graph`
- `ggen-utils` - Error types
- `tera` - Template engine (via ggen-core)

**Porting Rationale**:
- Core business logic for template operations
- Uses ggen-core infrastructure correctly
- No CLI dependencies
- Must be extracted from CLI crate

**Destination**: `crates/ggen-domain/src/template/`

---

**2.5. Project Domain (`cli/src/domain/project/`)** - 8 files
- `mod.rs` - Module exports
- `new.rs` - Project creation
- `gen.rs` - Project generation
- `plan.rs` - Generation plan creation
- `apply.rs` - Plan application
- `init.rs` - Project initialization
- `build.rs` - Project build operations
- Backup files: `init.rs.bak`, `build.rs.bak`, `mod.rs.bak`

**Functionality**:
- `create_project()` - Creates new projects from templates
- `generate_project()` - Generates project from template + RDF
- `create_plan()` - Creates generation plans
- `apply_plan()` - Applies generation plans
- `init_project()` - Initializes projects with conventions
- `build_project()` - Builds projects

**Dependencies**:
- `ggen-core::project_generator` - Uses project generator
- `ggen-utils` - Error types

**Porting Rationale**:
- Business logic for project operations
- Uses ggen-core infrastructure
- No CLI dependencies
- Needs extraction to domain crate

**Destination**: `crates/ggen-domain/src/project/`

---

**2.6. Hook Domain (`cli/src/domain/hook/`)** - 5 files
- `mod.rs` - Module exports
- `create.rs` - Hook creation
- `list.rs` - Hook listing
- `remove.rs` - Hook removal
- `monitor.rs` - Hook monitoring

**Functionality**:
- Hook management for file system events
- Integration with existing `domain/` crate

**Dependencies**:
- `domain::hook` (external domain crate)
- `ggen-utils` - Error types

**Porting Rationale**:
- Note: There's also a `domain/` crate at root with hooks
- Need to decide: merge or keep separate?
- Currently hooks exist in both places
- **DECISION NEEDED**: Consolidate hooks into single domain crate

**Destination**: `crates/ggen-domain/src/hook/` (consolidate with `domain/src/hook/`)

---

**2.7. Utils Domain (`cli/src/domain/utils/`)** - 3 files
- `mod.rs` - Module exports
- `doctor.rs` - System diagnostics
- `env.rs` - Environment management

**Functionality**:
- `SystemChecker` - Diagnoses system configuration
- `EnvironmentManager` - Manages ggen environment variables

**Dependencies**:
- `ggen-utils` - Error types
- No infrastructure dependencies

**Porting Rationale**:
- Pure utility business logic
- No CLI dependencies
- Simple extraction candidate

**Destination**: `crates/ggen-domain/src/utils/`

---

**2.8. RDF Domain (`cli/src/domain/rdf/`)** - 4 files
- `mod.rs` - Module exports
- `metadata.rs` - RDF metadata extraction
- `schema.rs` - RDF schema management
- `validation.rs` - RDF validation
- `schema.ttl` - Schema definition

**Functionality**:
- RDF metadata operations
- Schema validation
- Note: Some overlap with `ggen-core::rdf`

**Dependencies**:
- `ggen-core::rdf` (potential overlap)
- `ggen-utils` - Error types

**Porting Rationale**:
- **DECISION NEEDED**: Overlap with ggen-core::rdf
- May need to consolidate or clearly separate concerns
- Domain vs infrastructure boundaries need clarification

**Destination**: `crates/ggen-domain/src/rdf/` OR merge with `ggen-core::rdf`

---

**2.9. Audit Domain (`cli/src/domain/audit/`)** - 2 files
- `mod.rs` - Module exports
- `security.rs` - Security auditing

**Functionality**:
- Security auditing operations

**Dependencies**:
- `ggen-utils` - Error types

**Porting Rationale**:
- Pure business logic
- Simple extraction

**Destination**: `crates/ggen-domain/src/audit/`

---

**2.10. CI Domain (`cli/src/domain/ci/`)** - 2 files
- `mod.rs` - Module exports
- `workflow.rs` - CI workflow management

**Functionality**:
- CI workflow operations

**Dependencies**:
- `ggen-utils` - Error types

**Porting Rationale**:
- Pure business logic
- Simple extraction

**Destination**: `crates/ggen-domain/src/ci/`

---

**2.11. Shell Domain (`cli/src/domain/shell/`)** - 2 files
- `mod.rs` - Module exports
- `completion.rs` - Shell completion generation

**Functionality**:
- Shell completion script generation

**Dependencies**:
- `ggen-utils` - Error types

**Porting Rationale**:
- Pure business logic
- Simple extraction

**Destination**: `crates/ggen-domain/src/shell/`

---

#### 3. Runtime Utilities (`cli/src/runtime.rs`, `cli/src/runtime_helper.rs`)

**Purpose**: Async/sync bridge for CLI commands

**Functionality**:
- `execute()` - Executes async futures in sync context
- `block_on()` - Blocks on async futures with generic return types

**Dependencies**:
- `tokio` - Runtime creation
- `ggen-utils` - Error types

**Porting Rationale**:
- Needed by CLI layer to bridge to async domain
- Must stay in CLI crate (presentation concern)

**Destination**: `crates/ggen-cli/src/runtime.rs`

---

#### 4. Conventions (`cli/src/conventions/`)

**Purpose**: File-based routing conventions

**Functionality**:
- Template discovery
- File-based routing
- Preset configurations

**Dependencies**:
- Various

**Porting Rationale**:
- **DECISION NEEDED**: Is this CLI-specific or domain logic?
- May belong in domain if it's business logic
- May belong in CLI if it's presentation/routing

**Destination**: TBD - needs analysis

---

### CLI Crate Dependencies

```
ggen-cli depends on:
├── clap, clap-noun-verb, clap-noun-verb-macros (CLI parsing)
├── ggen-utils (error types)
├── ggen-core (infrastructure)
├── ggen-ai (AI integration)
├── ggen-marketplace (marketplace backend)
└── crate::domain (INTERNAL - needs to become ggen-domain)
```

---

## Part 2: Existing Domain Crate Analysis (`domain/`)

### Current Location: `domain/`

### What Exists

**Hook Module Only** (`domain/src/hook/`) - 5 files
- Same as `cli/src/domain/hook/`
- Duplicate functionality!

**Porting Rationale**:
- **DECISION NEEDED**: Merge with `cli/src/domain/hook/`
- Single source of truth for hooks
- Eliminate duplication

**Destination**: `crates/ggen-domain/src/hook/` (consolidate both)

---

## Part 3: Core Infrastructure (`ggen-core/`)

### Current Location: `ggen-core/`

### What Exists

#### Public Modules (30 exports)

1. **Template System**:
   - `template.rs` - Template abstraction
   - `templates/` - Template processing, file trees, frozen sections
   - `template_cache.rs` - Template caching
   - `tera_env.rs` - Tera environment setup

2. **RDF/Graph Processing**:
   - `graph.rs` - Graph operations
   - `rdf/` - RDF schema, metadata, validation
   - `oxigraph` integration

3. **Generation Pipeline**:
   - `generator.rs` - High-level generation
   - `pipeline.rs` - Pipeline orchestration
   - `streaming_generator.rs` - Streaming generation
   - `preprocessor.rs` - Template preprocessing

4. **Project Generation**:
   - `project_generator/` - Project scaffolding
   - `cli_generator/` - **NEW** CLI project generator (v3.3.0, crates pattern)

5. **Marketplace/Registry**:
   - `registry.rs` - Package registry
   - `resolver.rs` - Template resolution
   - `register.rs` - Template registration

6. **Utilities**:
   - `cache.rs` - Caching utilities
   - `delta.rs` - Change detection
   - `inject.rs` - File injection
   - `lockfile.rs` - Lockfile management
   - `merge.rs` - Three-way merging
   - `snapshot.rs` - File snapshots
   - `pqc.rs` - Post-quantum cryptography
   - `gpack.rs` - Package format
   - `github.rs` - GitHub integration
   - `telemetry.rs` - Observability
   - `lifecycle/` - Project lifecycle management
   - `config/` - Configuration management

**Porting Rationale**:
- Infrastructure layer - correct location
- No changes needed except path update
- Used by both domain and CLI layers

**Destination**: `crates/ggen-core/` (just move, no structural changes)

**Dependencies**:
- `ggen-utils`
- Various external crates (tera, oxigraph, etc.)

---

## Part 4: AI Integration (`ggen-ai/`)

### Current Location: `ggen-ai/`

### What Exists

- LLM client integration
- Multi-provider support (OpenAI, Anthropic, Ollama)
- Response caching
- Template generation
- SPARQL query generation
- RDF generation from CLI generator

**Porting Rationale**:
- Infrastructure/utility crate
- Just needs path update
- No structural changes

**Destination**: `crates/ggen-ai/`

**Dependencies**:
- `ggen-core`
- `ggen-utils`
- `genai` (multi-provider client)

---

## Part 5: Marketplace (`ggen-marketplace/`)

### Current Location: `ggen-marketplace/`

### What Exists

- Package registry backend
- Search engine (Tantivy)
- P2P marketplace (feature-flagged)
- Crypto verification
- Storage backends (filesystem, memory)

**Porting Rationale**:
- Infrastructure crate
- Just needs path update
- No structural changes

**Destination**: `crates/ggen-marketplace/`

**Dependencies**:
- Various external crates
- Minimal dependencies on other ggen crates

---

## Part 6: Utilities (`utils/`)

### Current Location: `utils/`

### What Exists

- Error types (`error.rs`)
- Configuration (`app_config.rs`)
- Logging (`logger.rs`)
- Types (`types.rs`)

**Porting Rationale**:
- Shared utilities
- Used by all other crates
- Just needs path update

**Destination**: `crates/ggen-utils/`

---

## Part 7: Node.js Bindings (`node/`)

### Current Location: `node/`

### What Exists

- Rust FFI bindings for Node.js
- `run_for_node()` function

**Porting Rationale**:
- External integration
- Just needs path update

**Destination**: `crates/ggen-node/`

---

## Part 8: Root `src/` Directory

### Current Location: `src/`

### What Exists

- `main.rs` - Entry point
- `lib.rs` - Library exports
- `agents/` - Agent system
- `core.rs` - Core functionality (legacy?)
- `p2p/` - P2P implementation (disabled)
- `mock_registry.rs` - Mock registry

**Porting Rationale**:
- **DECISION NEEDED**: What belongs here?
- `main.rs` should probably move to `crates/ggen-cli/src/main.rs`
- `lib.rs` exports may need redistribution
- Agents may belong in domain or separate crate
- P2P may belong in marketplace

**Destination**: TBD - needs analysis

---

## Summary: What Needs to be Ported

### Phase 1: Simple Moves (No Structural Changes)

1. **ggen-core/** → `crates/ggen-core/`
   - Why: Infrastructure, already correctly structured
   - Impact: Low - just update paths

2. **ggen-ai/** → `crates/ggen-ai/`
   - Why: Infrastructure, already correctly structured
   - Impact: Low - just update paths

3. **ggen-marketplace/** → `crates/ggen-marketplace/`
   - Why: Infrastructure, already correctly structured
   - Impact: Low - just update paths

4. **utils/** → `crates/ggen-utils/`
   - Why: Shared utilities, just update paths
   - Impact: Low - just update paths

5. **node/** → `crates/ggen-node/`
   - Why: External bindings, just update paths
   - Impact: Low - just update paths

---

### Phase 2: CLI Extraction (Structural Changes)

6. **cli/src/cmds/** → `crates/ggen-cli/src/cmds/`
   - Why: CLI presentation layer, must stay in CLI crate
   - Impact: Medium - update imports from `crate::domain` to `ggen_domain::`

7. **cli/src/runtime.rs** → `crates/ggen-cli/src/runtime.rs`
   - Why: Async/sync bridge, CLI concern
   - Impact: Low - just move

8. **cli/src/conventions/** → `crates/ggen-cli/src/conventions/` OR `crates/ggen-domain/src/conventions/`
   - Why: **NEEDS DECISION** - CLI or domain concern?
   - Impact: Medium - depends on decision

---

### Phase 3: Domain Extraction (Major Structural Changes)

9. **cli/src/domain/** → `crates/ggen-domain/src/`
   - Why: Pure business logic, must be separate from CLI
   - Impact: **HIGH** - 53 files, all imports need updating

**Breakdown**:
- `ai/` → `crates/ggen-domain/src/ai/` (3 files)
- `graph/` → `crates/ggen-domain/src/graph/` (4 files)
- `marketplace/` → `crates/ggen-domain/src/marketplace/` (10 files)
- `template/` → `crates/ggen-domain/src/template/` (10 files)
- `project/` → `crates/ggen-domain/src/project/` (8 files)
- `hook/` → `crates/ggen-domain/src/hook/` (5 files - consolidate with `domain/src/hook/`)
- `utils/` → `crates/ggen-domain/src/utils/` (3 files)
- `rdf/` → `crates/ggen-domain/src/rdf/` (4 files - **DECISION NEEDED**: overlap with ggen-core::rdf)
- `audit/` → `crates/ggen-domain/src/audit/` (2 files)
- `ci/` → `crates/ggen-domain/src/ci/` (2 files)
- `shell/` → `crates/ggen-domain/src/shell/` (2 files)

10. **domain/src/hook/** → Merge into `crates/ggen-domain/src/hook/`
    - Why: Eliminate duplication
    - Impact: Medium - need to merge implementations

---

### Phase 4: Decisions Needed Before Porting

**DECISION 1: Root `src/` directory**
- What should happen to `src/main.rs`?
- Should it move to `crates/ggen-cli/src/main.rs`?
- What about `src/lib.rs` exports?
- What about `src/agents/`, `src/p2p/`, `src/core.rs`?

**DECISION 2: Conventions module**
- Is `cli/src/conventions/` CLI-specific or domain logic?
- Should it move to domain or stay in CLI?

**DECISION 3: RDF overlap**
- `cli/src/domain/rdf/` vs `ggen-core/src/rdf/`
- Consolidate or separate?
- Clear boundaries needed

**DECISION 4: Hook duplication**
- `cli/src/domain/hook/` vs `domain/src/hook/`
- Merge into single implementation
- Choose best implementation or merge both

**DECISION 5: Main entry point**
- Where does `main.rs` live?
- Root `src/main.rs` vs `crates/ggen-cli/src/main.rs`
- Binary definition in Cargo.toml

---

## Dependency Graph Analysis

### Current Dependencies

```
cli
├── depends on: ggen-core, ggen-ai, ggen-marketplace, ggen-utils, domain
└── contains: cmds/, domain/, runtime/, conventions/

domain
├── depends on: minimal (just anyhow, serde)
└── contains: hook/

ggen-core
├── depends on: ggen-utils
└── infrastructure for all

ggen-ai
├── depends on: ggen-core, ggen-utils
└── AI integration

ggen-marketplace
├── depends on: minimal external deps
└── Marketplace backend
```

### Target Dependencies (v3.0.0)

```
crates/ggen-cli
├── depends on: ggen-domain, ggen-core, ggen-ai, ggen-marketplace, ggen-utils
└── contains: cmds/, runtime/, conventions/ (TBD)

crates/ggen-domain
├── depends on: ggen-core, ggen-ai, ggen-marketplace, ggen-utils
└── contains: ai/, graph/, marketplace/, template/, project/, hook/, utils/, rdf/, audit/, ci/, shell/

crates/ggen-core
├── depends on: ggen-utils
└── (unchanged)

crates/ggen-ai
├── depends on: ggen-core, ggen-utils
└── (unchanged)

crates/ggen-marketplace
├── depends on: minimal external deps
└── (unchanged)
```

**Critical Constraint**: `ggen-domain` must have **ZERO** dependencies on `ggen-cli`. This ensures clean separation.

---

## File Count Summary

### What's Being Moved

- **CLI commands**: 9 files → `crates/ggen-cli/src/cmds/`
- **Domain logic**: 53 files → `crates/ggen-domain/src/`
- **Runtime utilities**: 2 files → `crates/ggen-cli/src/`
- **Conventions**: ~9 files → TBD
- **Infrastructure crates**: ~200+ files → `crates/*/` (just path updates)

### Total Files to Port: ~270+ files

---

## Import Changes Required

### Before (v2.4.0)

```rust
// In CLI commands
use crate::domain::project;

// In domain modules
use ggen_core::Graph;
use ggen_marketplace::Registry;
```

### After (v3.0.0)

```rust
// In CLI commands
use ggen_domain::project;

// In domain modules (unchanged)
use ggen_core::Graph;
use ggen_marketplace::Registry;
```

**Estimated import changes**: ~50-100 import statements need updating

---

## Why This Refactor is Needed

1. **Separation of Concerns**: Domain logic mixed in CLI crate violates clean architecture
2. **Testability**: Domain logic should be testable without CLI dependencies
3. **Reusability**: Domain logic should be usable by other interfaces (web API, library mode)
4. **Consistency**: Apply same crates pattern we generate for CLI projects to ggen itself
5. **Maintainability**: Clear boundaries between layers make refactoring easier
6. **Type Safety**: Compile-time guarantees that domain has no CLI dependencies

---

## Risks & Mitigations

### Risk 1: Breaking Changes
- **Mitigation**: Comprehensive test suite, migration guide

### Risk 2: Circular Dependencies
- **Mitigation**: Dependency graph analysis, `ggen-domain` has zero CLI deps

### Risk 3: Lost Git History
- **Mitigation**: Use `git mv` for file moves, preserve history

### Risk 4: Import Errors During Migration
- **Mitigation**: Incremental approach, test after each module move

### Risk 5: Unclear Boundaries
- **Mitigation**: Make decisions before porting (see Phase 4 decisions)

---

## Next Steps

1. **Review this audit** - Confirm understanding
2. **Make decisions** - Resolve Phase 4 decision points
3. **Create detailed migration plan** - Step-by-step with validation
4. **Execute incrementally** - One module at a time
5. **Test continuously** - After each migration step

---

**Document Status**: Complete functionality audit ready for review
**Next Action**: User review and decision-making on Phase 4 items

