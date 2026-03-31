# ggen-cli Analysis: Last 10 Commits

**Generated:** 2026-03-31
**Scope:** crates/ggen-cli/ directory
**Commits Analyzed:** 605a91b9 through a8b877a5 (10 commits)
**Files Changed:** 69 unique files

## Executive Summary

The last 10 commits focused on:
1. **Clippy cleanup and formatting fixes** (2 commits)
2. **Major feature sprint**: LLM bridge, MCP quality tools, A2A agents (1 commit)
3. **Test gating**: 500+ broken tests behind integration feature flag (1 commit)
4. **MCP server integration**: rmcp 1.3.0 stdio transport (1 commit)
5. **Graph command improvements**: SPARQL variable prefix stripping (1 commit)
6. **Platform regeneration**: ggen sync pipeline with 6 generators (1 commit)
7. **Quality gate validation**: All definitions of done passing (1 commit)
8. **Phase3 implementation**: JPA entities with Queryable/Renderable (1 commit)

**Key Finding:** The CLI is in active development with MCP/A2A integration as the primary focus. Several commands are stubbed or simplified.

---

## File-by-File Analysis

### 1. src/main.rs

**Purpose:** CLI entry point - thin delegation to `ggen_cli_lib`

**Key Elements:**
- Tokio async runtime
- Simple error handling with exit code 1 on failure
- Delegates to `ggen_cli_lib::cli_match()`

**Dependencies:** `ggen_cli_lib`, `tokio`

**Status:** ✅ Complete

---

### 2. src/cmds/mod.rs

**Purpose:** Command router module using clap-noun-verb v3.4.0 auto-discovery

**Key Types:**
- `run_cli()` - Entry point for clap-noun-verb auto-discovery

**Commands Registered:**
- **Core:** `git_hooks`, `init`, `sync`, `wizard`
- **Full Feature Set:** `ai`, `construct`, `graph`, `hook`, `marketplace`, `mcp`, `ontology`, `packs`, `paper`, `project`, `template`, `utils`, `workflow`, `yawl`
- **Disabled:** `self_play` (TODO: Implement self_play module)

**Removed Commands (v5.0):**
- `ggen generate` → Use `ggen sync`
- `ggen validate` → Use `ggen sync --validate-only`
- `ggen template *` → Use `ggen sync`
- `ggen project *` → Add back in v5.1+
- `ggen graph *` → Add back in v5.1+
- `ggen ontology *` → Add back in v5.1+
- `ggen marketplace *` → Add back in v5.1+
- `ggen ai *` → Add back in v5.1+

**Status:** ✅ Complete but some commands stubbed

---

### 3. src/cmds/sync.rs

**Purpose:** The ONLY command in ggen v5 - unified code synchronization pipeline

**Key Types:**
- `SyncOutput` - CLI output wrapper around `SyncResult`
- `SyncedFile` - Individual file sync result
- `sync()` verb function - Main entry point

**Key Functions:**
- `sync()` - Execute μ₁-μ₅ A2A-RS pipeline
- `run_low_level_pipeline()` - Ontology-first pipeline (bypasses ggen.toml)
- `inject_llm_if_enabled()` - Inject GroqLlmBridge if manifest has `enable_llm = true`
- `build_sync_options()` - Build SyncOptions from CLI args

**A2A-RS μ Pipeline:**
- μ₁ (CONSTRUCT): Normalize RDF ontology
- μ₂ (SELECT): Extract bindings for modules
- μ₃ (Tera): Generate Rust code
- μ₄ (Canonicalize): Format and organize
- μ₅ (Receipt): Generate cryptographic verification

**Dependencies:**
- `ggen_core::codegen` (SyncExecutor, SyncOptions, SyncResult)
- `ggen_core::sync` (low-level sync)
- `crate::llm_bridge::GroqLlmBridge`
- `clap_noun_verb` (verb macros)

**TODO/FIXME:**
- Line 480: "Falling back to TODO stubs" (LLM integration incomplete)
- Line 539: "Add timeout_ms field to SyncOptions if needed"

**Status:** 🟡 Core implemented, LLM integration partial

---

### 4. src/cmds/init.rs

**Purpose:** Initialize new ggen project with atomic file operations

**Key Types:**
- `InitOutput` - CLI output with transaction info
- `TransactionInfo` - Atomic operation tracking

**Key Functions:**
- `init()` verb - Thin CLI layer
- `perform_init()` - Atomic initialization with FileTransaction

**Features:**
- **BIG BANG 80/20 Screening Gate:**
  1. Real user data (CSV/JSON, not promised)
  2. Found existing standard ontology (schema.org, FOAF, Dublin Core, SKOS)
  3. Clear problem articulation (one sentence)
  4. Market signal (email list, beta users, commitment)
  5. Speed validation plan (10 users in 48 hours)

- **Atomic Initialization:**
  - FileTransaction for rollback on failure
  - Pre-flight validation (disk space, permissions)
  - Preserves existing .gitignore and README.md

**Hardcoded Files:**
- `ggen.toml` - Project configuration
- `schema/domain.ttl` - Schema.org example ontology
- `Makefile` - Build targets
- `templates/example.txt.tera` - Example template
- `scripts/startup.sh` - BIG BANG 80/20 screening script

**Dependencies:**
- `ggen_core::codegen::FileTransaction`
- `ggen_core::validation::PreFlightValidator`
- `tempfile` (tests)

**Status:** ✅ Complete and production-ready

---

### 5. src/cmds/wizard.rs

**Purpose:** Interactive project bootstrap with deterministic factory scaffold

**Key Types:**
- `WizardProfile` - 8 profiles (receipts-first, c4-diagrams, openapi-contracts, infra-k8s-gcp, lnctrl-output-contracts, ln-ctrl, mcp-a2a, custom)
- `ProjectMetadata` - Project name, version, description, license, authors
- `WizardConfig` - Full wizard configuration
- `WizardOutput` - Result with files/directories created

**Key Functions:**
- `wizard()` verb - Interactive CLI entry point
- `perform_wizard()` - Scaffold generation with FileTransaction
- `generate_scaffold()` - Profile-specific file generation
- `generate_ln_ctrl_*()` - LN_CTRL-specific generators
- `generate_mcp_a2a_configs()` - MCP/A2A config files

**Profiles:**
1. **receipts-first** (default): World manifest, receipt schemas, audit trail
2. **c4-diagrams**: C4 L1-L4 Mermaid diagram generation
3. **openapi-contracts**: OpenAPI spec generation
4. **infra-k8s-gcp**: Kubernetes + GCP infrastructure manifests
5. **lnctrl-output-contracts**: LN_CTRL output contract schemas
6. **ln-ctrl**: Full LN_CTRL profile with λn execution traces
7. **mcp-a2a**: MCP + A2A configuration
8. **custom**: Custom configuration (advanced)

**Generated Files (per profile):**
- `ggen.toml` - Project configuration
- `.specify/specs/project.ttl` - Project metadata
- `.specify/ontologies/*.ttl` - RDF ontologies
- `sparql/*.rq` - SPARQL queries
- `templates/*.tera` - Tera templates
- `.mcp.json` - MCP server config (mcp-a2a profile)
- `a2a.toml` - A2A agent config (mcp-a2a profile)

**Dependencies:**
- `ggen_core::codegen::FileTransaction`
- `serde` (Serialize)
- `tempfile` (tests)

**Status:** ✅ Complete with 8 profiles

---

### 6. src/cmds/mcp.rs

**Purpose:** MCP (Model Context Protocol) commands for server management and tool bridging

**Key Types:**
- `McpToolInfo` - Tool metadata with input schema
- `ToolExecutionResult` - Execution result with duration
- `McpBackendManager` - Backend for MCP tools and agents

**Key Functions:**
- Core tools: `agent-list`, `agent-start`, `agent-status`, `workflow-start`
- Backend management: `list_tools()`, `bridge_agent()`, `test_tool()`
- Schema management: `get_tool_status()`, `get_schemas()`

**Dependencies:**
- `ggen_ai` (GenAiClient, LlmClient, LlmConfig)
- `ggen_domain::mcp_config` (MCP/A2A config loading)
- `tokio::sync::RwLock` (async state)

**Status:** 🟡 Backend structure present, incomplete integration

---

### 7. src/cmds/ontology.rs

**Purpose:** Simple ontology commands (generate, validate, init)

**Key Types:**
- `GenerateOutput` - Language, files generated, output directory
- `ValidateOutput` - Validation results with warnings/errors
- `InitOutput` - Project initialization result

**Key Functions:**
- `generate()` - Generate code from ontology schema (STUB)
- `validate()` - Validate ontology schema quality (STUB)
- `init()` - Initialize ontology project (STUB)

**Status:** 🔴 All commands are stubs returning zero values

---

### 8. src/cmds/project.rs

**Purpose:** Project commands for scaffolding, planning, and generation

**Key Types:**
- `NewOutput` - Project creation result
- `PlanOutput` - Plan generation result
- `GenOutput` - Code generation result
- `ApplyOutput` - Plan application result
- `InitOutput` - Project initialization result
- `GenerateOutput` - Template processing result
- `WatchOutput` - File watching result
- `OperationSummary` - Individual operation summary

**Key Functions:**
- `new()` - Create new project from scratch
- `plan()` - Generate project plan from template
- `gen()` - Generate code from template
- `apply()` - Apply plan to filesystem
- `init()` - Initialize project from preset
- `generate()` - Process templates
- `watch()` - Watch for file changes

**Dependencies:**
- `ggen_domain::project` (new, plan, gen modules)

**Status:** 🟡 Bridge functions implemented, domain logic incomplete

---

### 9. src/commands/paas/mod.rs

**Purpose:** PaaS submodule management with noun-verb semantic routing

**Key Types:**
- `PaasCommand` - Main command with verbose/spec_dir/output_dir flags
- `PaasAction` - 9 subcommands (Init, Update, Validate, Sync, Deploy, Status, Logs, Describe, Explain)

**Nouns:**
- submodule
- artifact
- specification
- environment

**Verbs:**
- init, update, validate, sync, deploy, status, logs, describe, explain

**Key Functions:**
- `execute()` - Route noun-verb pairs to handlers

**Handlers:**
- `handlers::init` - Initialize submodule
- `handlers::update` - Update submodule(s)
- `handlers::validate` - Validate specification closure
- `handlers::sync` - Sync specs with code
- `handlers::deploy` - Deploy artifacts
- `handlers::status` - Show deployment status
- `handlers::logs` - Stream operation logs
- `handlers::describe` - Describe artifact/spec
- `handlers::explain` - Explain artifact origin

**Dependencies:**
- `clap` (Args, Subcommand)
- `errors`, `handlers`, `validators` submodules

**Status:** 🟡 Command structure complete, handlers incomplete

---

### 10. src/conventions/planner.rs

**Purpose:** Generation planner for creating task execution plans

**Key Types:**
- `TemplateMetadata` - Parsed from {# ... #} comments
- `GenerationTask` - Single generation task
- `GenerationPlan` - Complete plan with tasks
- `GenerationPlanner` - Plans code generation

**Key Functions:**
- `plan()` - Create generation plan by analyzing templates
- `parse_template_metadata()` - Parse {# output:, when:, query:, foreach: #}
- `resolve_dependencies()` - Resolve file dependencies
- `check_circular_dependencies()` - DFS-based cycle detection
- `topological_sort()` - Sort tasks by dependencies

**Dependencies:**
- `super::ProjectConventions`
- `std::collections::{HashMap, HashSet}`
- `tempfile` (tests)

**Status:** ✅ Complete with tests

---

## Test Files Analysis

### tests/wizard_ln_ctrl_test.rs

**Purpose:** End-to-end tests for ln_ctrl wizard profile with determinism validation

**Test Philosophy:** Chicago TDD (state-based testing with real objects)
- No mocks - uses real filesystem, real ggen binary, real Node.js execution
- Verifies observable state changes and side effects

**Test Coverage:**
1. `ggen wizard --profile ln_ctrl` creates all expected files
2. `ggen sync` runs successfully
3. `node world.verify.mjs` validates all artifacts
4. Second `ggen sync` produces byte-identical output (determinism)
5. All schemas validate their respective golden examples
6. SHACL validation passes (if implemented)

**Key Test Helpers:**
- `setup_test_project()` - Temp directory creation
- `run_wizard()` - Execute ggen wizard
- `run_sync()` - Execute ggen sync
- `run_world_verifier()` - Execute Node.js verifier
- `compute_file_hash()` - SHA-256 hashing for determinism

**Status:** ✅ Test structure complete, implementation pending

---

## What Needs To Be Finished

### High Priority (Blocking Features)

1. **LLM Integration** (src/cmds/sync.rs)
   - Line 480: "Falling back to TODO stubs"
   - GroqLlmBridge injection is partial
   - Need to verify OTEL spans for llm.complete operations
   - **Evidence Required:** `RUST_LOG=trace,ggen_ai=trace` output showing actual LLM API calls

2. **Ontology Commands** (src/cmds/ontology.rs)
   - All commands are stubs returning zero values
   - `generate()`, `validate()`, `init()` need implementation
   - Depends on `ggen_domain::ontology`

3. **Project Commands Domain Logic** (src/cmds/project.rs)
   - Bridge functions implemented
   - Domain logic in `ggen_domain::project` incomplete
   - `new()`, `plan()`, `gen()`, `apply()`, `init()`, `generate()`, `watch()` all stubbed

4. **PaaS Handlers** (src/commands/paas/)
   - Command routing complete
   - Handler implementations incomplete:
     - `handlers::init`
     - `handlers::update`
     - `handlers::validate`
     - `handlers::sync`
     - `handlers::deploy`
     - `handlers::status`
     - `handlers::logs`
     - `handlers::describe`
     - `handlers::explain`

### Medium Priority (Feature Completeness)

5. **Self-Play Module** (src/cmds/mod.rs:47)
   - Commented out: `// pub mod self_play; // TODO: Implement self_play module`
   - Needs to be added back in v5.1+

6. **MCP Backend Integration** (src/cmds/mcp.rs)
   - Backend structure present
   - Core tools defined (agent-list, agent-start, agent-status, workflow-start)
   - Agent bridging incomplete
   - Tool execution incomplete

7. **Timeout Support** (src/cmds/sync.rs:539)
   - Comment: "Add timeout_ms field to SyncOptions if needed"
   - `--timeout` flag exists but not implemented

8. **Environment Variables** (src/cmds/utils.rs:105)
   - Comment: "TODO: Fix compilation issue with environment variables"

### Low Priority (Nice to Have)

9. **Test Coverage**
   - Integration tests exist but many are gated behind `integration` feature
   - 500+ tests were gated in commit 58a02684
   - Need to ungate and fix failing tests

10. **Documentation**
    - Generated code needs inline documentation
    - Command help text could be improved
    - Examples for all command combinations

---

## Dependencies and Relationships

### Internal Dependencies

```
ggen-cli (CLI layer)
├── ggen_cli_lib (library)
│   ├── ggen_core (domain logic, codegen, validation)
│   ├── ggen_domain (project, mcp_config)
│   ├── ggen_ai (LLM integration)
│   └── ggen_utils (error types)
├── clap-noun-verb (CLI framework)
├── tokio (async runtime)
└── serde (serialization)
```

### External Dependencies

- **clap-noun-verb v3.4.0**: Auto-discovery of verb functions
- **tokio**: Async runtime for all commands
- **serde/serde_json**: Serialization for JSON output
- **tempfile**: Test isolation
- **assert_cmd**: Integration testing
- **predicates**: Command output assertions

---

## Architecture Patterns

### Three-Layer Pattern (sync.rs)

```
Layer 3 (CLI): Input validation, output formatting, thin routing
Layer 2 (Integration): Async execution, error handling
Layer 1 (Domain): Pure generation logic from ggen_core::codegen
```

### Atomic Operations (init.rs, wizard.rs)

```
FileTransaction:
1. Pre-flight checks
2. Create transaction
3. Write files via transaction
4. Commit (point of no return)
5. Automatic rollback on error
```

### Noun-Verb Routing (paas/mod.rs)

```
PaasCommand:
- Nouns: submodule, artifact, specification, environment
- Verbs: init, update, validate, sync, deploy, status, logs, describe, explain
- Routing: match PaasAction → handler
```

---

## Definitions of Done

### Per Feature

**sync command:**
- ✅ Compiles without errors
- ✅ μ₁-μ₅ pipeline implemented
- ✅ Low-level pipeline (--queries flag) working
- 🟡 LLM integration complete (OTEL spans required)
- ✅ Atomic file operations
- ✅ Error handling with exit codes

**init command:**
- ✅ Compiles without errors
- ✅ Atomic initialization with FileTransaction
- ✅ BIG BANG 80/20 screening gate
- ✅ Pre-flight validation
- ✅ Preserves user files (.gitignore, README.md)
- ✅ Git hooks integration
- ✅ Comprehensive tests

**wizard command:**
- ✅ Compiles without errors
- ✅ 8 profiles implemented
- ✅ Interactive and non-interactive modes
- ✅ Profile-specific scaffold generation
- ✅ LN_CTRL profile with λn execution traces
- ✅ MCP/A2A profile with config generation
- ✅ Comprehensive tests

**MCP commands:**
- 🟡 Backend structure complete
- 🔴 Tool execution incomplete
- 🔴 Agent bridging incomplete
- 🔴 OTEL validation pending

**Ontology commands:**
- 🔴 All commands stubbed
- 🔴 Zero implementation

**Project commands:**
- 🟡 Bridge functions complete
- 🔴 Domain logic incomplete
- 🔴 All commands stubbed

**PaaS commands:**
- 🟡 Noun-verb routing complete
- 🔴 Handler implementations incomplete

---

## Recommendations

### Immediate Actions

1. **Complete LLM Integration** (sync.rs)
   - Implement GroqLlmBridge integration
   - Add OTEL spans for llm.complete
   - Verify with `RUST_LOG=trace,ggen_ai=trace`
   - Remove "Falling back to TODO stubs" message

2. **Implement Ontology Commands** (ontology.rs)
   - Start with `generate()` - use ggen_core::codegen
   - Then `validate()` - use ggen_core::validation
   - Finally `init()` - reuse init.rs patterns

3. **Complete Project Domain Logic** (project.rs)
   - Implement `ggen_domain::project::new`
   - Implement `ggen_domain::project::plan`
   - Implement `ggen_domain::project::gen`
   - Other commands depend on these

4. **Implement PaaS Handlers** (paas/)
   - Start with `handlers::validate` (highest value)
   - Then `handlers::sync` (core workflow)
   - Then `handlers::init`, `handlers::update`
   - Finally `handlers::deploy`, `handlers::status`, etc.

### Medium-Term Actions

5. **Un gate Integration Tests**
   - Fix 500+ tests behind `integration` feature
   - Prioritize tests for core commands (sync, init, wizard)
   - Use Chicago TDD patterns (real collaborators, no mocks)

6. **Add Timeout Support** (sync.rs)
   - Add `timeout_ms` field to `SyncOptions`
   - Implement timeout logic in `SyncExecutor`
   - Add tests for timeout behavior

7. **Implement Self-Play Module** (mod.rs)
   - Create `src/cmds/self_play.rs`
   - Add to module exports
   - Document in help text

### Long-Term Actions

8. **Improve MCP Integration** (mcp.rs)
   - Complete agent bridging
   - Implement tool execution
   - Add OTEL spans for MCP operations
   - Add comprehensive tests

9. **Fix Environment Variables** (utils.rs)
   - Resolve compilation issue
   - Add tests for env var handling

10. **Documentation**
    - Add examples for all command combinations
    - Document error codes and recovery procedures
    - Create architecture diagrams

---

## Test Strategy

### Chicago TDD Compliance

**Current State:**
- ✅ init.rs: Real FileTransaction, no mocks
- ✅ wizard.rs: Real filesystem, tempfile
- 🟡 sync.rs: Real ggen_core, but LLM integration incomplete
- ✅ planner.rs: Real template parsing, no mocks
- 🔴 ontology.rs: All stubs, no real tests
- 🔴 project.rs: Bridge functions tested, domain logic stubbed
- 🔴 mcp.rs: Backend structure only, no integration tests
- 🔴 paas/: Routing tested, handlers not implemented

**Required Actions:**
1. Convert all stubs to real implementations
2. Replace any remaining mocks with real collaborators
3. Add OTEL span verification for LLM/external services
4. Ensure all tests verify observable state, not mock interactions

---

## Conclusion

The ggen CLI is in active development with a solid architectural foundation. The core commands (sync, init, wizard) are production-ready with comprehensive atomic operations and error handling. However, several feature areas are stubbed or incomplete:

**Strong Areas:**
- ✅ Core sync pipeline (μ₁-μ₅ A2A-RS)
- ✅ Atomic file operations (FileTransaction)
- ✅ Wizard profiles (8 profiles with deterministic output)
- ✅ BIG BANG 80/20 screening gate (prevents Seth-like patterns)
- ✅ Chicago TDD test structure (real collaborators, no mocks)

**Weak Areas:**
- 🔴 LLM integration incomplete (TODO stubs, no OTEL spans)
- 🔴 Ontology commands stubbed
- 🔴 Project domain logic incomplete
- 🔴 PaaS handlers not implemented
- 🔴 500+ tests gated behind feature flag

**Next Steps:**
1. Complete LLM integration with OTEL validation
2. Implement ontology commands
3. Complete project domain logic
4. Implement PaaS handlers
5. Un gate and fix integration tests

The codebase demonstrates strong architectural patterns (three-layer, atomic operations, noun-verb routing) but needs implementation work in feature areas outside the core sync/init/wizard commands.
