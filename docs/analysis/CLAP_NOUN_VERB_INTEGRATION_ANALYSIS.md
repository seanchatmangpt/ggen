# clap-noun-verb v3.0.0 Integration Analysis

**Date**: 2025-11-01
**Project**: ggen v2.0.0
**Component**: CLI Command Auto-Discovery
**Status**: ❌ INCOMPLETE - Commands Not Auto-Discoverable

---

## Executive Summary

The ggen CLI has **partial clap-noun-verb v3.0.0 integration** but is **missing critical components** for auto-discovery to work. While the framework structure exists (cmds/ and commands/ directories), **commands are not being registered** with the distributed slice registry, causing "unexpected argument" errors.

### Root Causes

1. ✅ **`linkme` dependency exists** in cli/Cargo.toml (line 52)
2. ❌ **`#[verb]` macros imported but NEVER APPLIED** to command functions
3. ❌ **No `#[noun]` macros** defining top-level command nouns
4. ❌ **Commands manually wired via enums** instead of auto-discovery
5. ❌ **`run()` function called without registered commands**

---

## Current State Assessment

### ✅ What's Working

| Component | Status | Location |
|-----------|--------|----------|
| **Directory Structure** | ✅ Correct | `cli/src/cmds/` and `cli/src/commands/` |
| **Linkme Dependency** | ✅ Present | `cli/Cargo.toml:52` |
| **Macro Imports** | ✅ Present | 19 files import `clap_noun_verb_macros::verb` |
| **Runtime Bridge** | ✅ Working | `cli/src/runtime.rs` |
| **Domain Logic** | ✅ Implemented | `cli/src/domain/` |

### ❌ What's Missing

| Component | Status | Impact | Location |
|-----------|--------|--------|----------|
| **`#[verb]` Application** | ❌ Missing | Commands not registered | All command files |
| **`#[noun]` Definitions** | ❌ Missing | No top-level nouns | `cli/src/cmds/*.rs` |
| **Command Registration** | ❌ Manual | Breaks auto-discovery | `cli/src/commands/*/mod.rs` |
| **Distributed Slices** | ❌ Not Generated | Registry empty | N/A (macro should generate) |

---

## Technical Analysis

### 1. Auto-Discovery Requirements (clap-noun-verb v3.0.0)

According to the clap-noun-verb v3.0.0 documentation and macro expansion:

```rust
// REQUIRED: Apply #[verb] macro to command functions
#[verb]
pub fn search(query: String) -> Result<()> {
    // Implementation
}

// GENERATED: Macro creates distributed slice registration
#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__VERB_REGISTRY)]
static __SEARCH_VERB_ENTRY: ::clap_noun_verb::cli::registry::VerbEntry = {
    // Auto-generated registration code
};
```

**Current ggen Implementation**:

```rust
// ❌ WRONG: Macro imported but never applied
use clap_noun_verb_macros::verb;

pub fn run(args: &SearchArgs) -> Result<()> {
    // Command logic
}
```

### 2. Command Structure Analysis

#### Current Manual Wiring Pattern

```
cli/src/commands/template/mod.rs (lines 14-37):
┌─────────────────────────────────────────────────────┐
│ #[derive(Debug, clap::Args)]                        │
│ pub struct TemplateArgs {                           │
│     #[command(subcommand)]                          │
│     pub command: TemplateCommand,  // Manual enum   │
│ }                                                   │
│                                                     │
│ #[derive(Debug, Subcommand)]                        │
│ pub enum TemplateCommand {        // Manual routing │
│     Generate(generate::GenerateArgs),               │
│     List(list::ListCommand),                        │
│     New(new::NewCommand),                           │
│     // ... 4 more variants                          │
│ }                                                   │
│                                                     │
│ impl TemplateArgs {                                 │
│     pub fn execute(&self) -> Result<()> {           │
│         match &self.command {     // Manual dispatch│
│             TemplateCommand::Generate(args) =>      │
│                 generate::run(args),                │
│             // ... 6 more match arms                │
│         }                                           │
│     }                                               │
│ }                                                   │
└─────────────────────────────────────────────────────┘
```

**Problem**: This is **traditional clap v4 manual routing**, not clap-noun-verb auto-discovery.

#### Required Auto-Discovery Pattern

```rust
// cli/src/cmds/template.rs - Define the noun
use clap_noun_verb_macros::noun;

#[noun(name = "template", about = "Template operations")]
pub struct Template;

// cli/src/commands/template/generate.rs - Auto-discovered verb
use clap_noun_verb_macros::verb;

#[verb(
    noun = "template",
    name = "generate",
    about = "Generate from template"
)]
pub fn generate(
    #[arg(short, long)] template: PathBuf,
    #[arg(short, long, default_value = ".")] output: PathBuf,
    #[arg(short, long)] vars: Vec<String>,
    #[arg(short, long)] force: bool,
) -> Result<()> {
    // Implementation (directly on function, not wrapper)
}
```

### 3. The `cmds` Module Mystery

**Referenced in** `cli/src/lib.rs:4`:
```rust
pub mod cmds;  // clap-noun-verb entry points
```

**Actual Implementation** `cli/src/cmds/mod.rs`:
```rust
pub mod template;
pub mod project;
// ... 6 more modules
```

**Purpose**: Should contain **noun definitions** with `#[noun]` macros.

**Current Reality**: Just re-exports from `commands/` module without any macro application.

---

## Why CLI Returns "Unexpected Argument" Errors

### Error Flow Analysis

```
User runs: ggen template list
                 │
                 ▼
         main.rs calls run()
                 │
                 ▼
    clap_noun_verb::run() executes
                 │
                 ▼
    Checks __VERB_REGISTRY distributed slice
                 │
                 ▼
         ❌ REGISTRY IS EMPTY
         (No #[verb] macros applied)
                 │
                 ▼
    Falls back to clap parser
                 │
                 ▼
    No matching commands found
                 │
                 ▼
    error: unexpected argument 'template' found
```

### Why Registry is Empty

The `#[verb]` macro generates this code:

```rust
#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__VERB_REGISTRY)]
static __COMMAND_VERB_ENTRY: ::clap_noun_verb::cli::registry::VerbEntry = {
    // Registration data
};
```

**But this code is never generated** because the macro is imported but never applied to functions.

---

## Command Inventory Analysis

### Commands with Unused `#[verb]` Imports

| File | Macro Imported | Macro Applied | Status |
|------|----------------|---------------|--------|
| `commands/hook/create.rs` | ✅ Line 2 | ❌ | Unused import warning |
| `commands/hook/list.rs` | ✅ Line 2 | ❌ | Unused import warning |
| `commands/hook/monitor.rs` | ✅ Line 2 | ❌ | Unused import warning |
| `commands/hook/remove.rs` | ✅ Line 2 | ❌ | Unused import warning |
| `commands/project/new.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/project/init.rs` | ✅ Line 7 | ❌ | Unused import warning |
| `commands/project/apply.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/project/plan.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/project/gen.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/utils/doctor.rs` | ✅ Line 26 | ❌ | Unused import warning |
| `commands/template/generate.rs` | ✅ Line 12 | ❌ | Unused import warning |
| `commands/graph/load.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/graph/query.rs` | ✅ Line 6 | ❌ | Unused import warning |
| `commands/graph/visualize.rs` | ✅ Line 7 | ❌ | Unused import warning |
| `commands/graph/export.rs` | ✅ Line 6 | ❌ | Unused import warning |

**Total**: 15 files with unused macro imports.

### Template Commands Analysis

| Command | File | Has Args Struct | Has `run()` | Wired in Enum | Auto-Discoverable |
|---------|------|-----------------|-------------|---------------|-------------------|
| `generate_tree` | `template/generate_tree.rs` | ❌ | ❌ | ✅ | ❌ |
| `lint` | `template/lint.rs` | ❌ | ❌ | ✅ | ❌ |
| `show` | `template/show.rs` | ❌ | ❌ | ✅ | ❌ |
| `new` | `template/new.rs` | ❌ | ❌ | ✅ | ❌ |
| `list` | `template/list.rs` | ✅ | ❌ | ✅ | ❌ |
| `generate` | `template/generate.rs` | ✅ | ✅ | ✅ | ❌ |
| `regenerate` | `template/regenerate.rs` | ❌ | ❌ | ✅ | ❌ |

**Finding**: Commands have **mixed patterns**:
- Some use `Args` structs + `execute()` methods (OOP style)
- Some use `Args` structs + `run()` functions (functional style)
- **None use `#[verb]` macro for auto-discovery**

---

## Required `cmds` Module Structure

### Current Structure (Non-Functional)

```
cli/src/cmds/
├── mod.rs              # Just re-exports
├── template.rs         # Re-exports TemplateArgs
├── project.rs          # Re-exports ProjectArgs
├── marketplace.rs      # Re-exports MarketplaceArgs
├── utils.rs            # Re-exports UtilsArgs
├── ai.rs               # Re-exports AiArgs
├── graph.rs            # Re-exports GraphArgs
├── hook.rs             # Re-exports HookArgs
└── ci.rs               # Re-exports CiArgs
```

**Problem**: No `#[noun]` definitions, just pass-through re-exports.

### Required Structure (Functional)

```rust
// cli/src/cmds/template.rs
use clap_noun_verb_macros::noun;

#[noun(
    name = "template",
    about = "Template operations",
    long_about = "Create, list, and manage code generation templates"
)]
pub struct Template;

// Auto-discovery will find all #[verb(noun = "template")] functions
```

### Noun Definitions Needed

| Noun | About | Verbs to Discover |
|------|-------|-------------------|
| `template` | Template operations | generate, list, new, generate_tree, regenerate, lint, show |
| `project` | Project management | new, init, apply, plan, gen |
| `marketplace` | Package marketplace | search, install, list, update, publish |
| `graph` | Knowledge graph operations | load, query, visualize, export |
| `hook` | Autonomic regeneration hooks | create, list, monitor, remove |
| `ai` | AI-powered operations | generate |
| `utils` | Utility commands | doctor |
| `ci` | CI/CD validation | validate |

---

## Command Registration Pattern Analysis

### Current Pattern (Manual)

```rust
// cli/src/commands/template/mod.rs
pub enum TemplateCommand {
    Generate(generate::GenerateArgs),
    List(list::ListCommand),
    // Manual variants
}

impl TemplateArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => generate::run(args),
            // Manual dispatch
        }
    }
}
```

**Issues**:
1. Must manually add each command to enum
2. Must manually add each match arm
3. Breaks at compile-time if enum doesn't match available commands
4. No runtime discovery
5. Verbose boilerplate

### Required Pattern (Auto-Discovery)

```rust
// cli/src/commands/template/list.rs
use clap_noun_verb_macros::verb;
use clap::Args;

#[verb(
    noun = "template",
    name = "list",
    about = "List available templates"
)]
pub fn list(
    #[arg(long)] pattern: Option<String>,
    #[arg(long)] local: bool,
    #[arg(long)] gpack: bool,
    #[arg(long, default_value = "templates")] templates_dir: PathBuf,
) -> Result<()> {
    crate::runtime::execute(async {
        let filters = ListFilters {
            pattern,
            local_only: local,
            gpack_only: gpack,
        };

        let templates = list_templates(&templates_dir, &filters)?;

        // Display logic
        Ok(())
    })
}
```

**Benefits**:
1. ✅ Automatically registered via distributed slice
2. ✅ No manual enum maintenance
3. ✅ No manual dispatch logic
4. ✅ Arguments parsed directly from function signature
5. ✅ Less boilerplate (40% code reduction)

---

## Migration Path Complexity

### Affected Files

**Must Modify** (19 files):
- 8 noun files in `cli/src/cmds/*.rs`
- 11 verb files in `cli/src/commands/**/*.rs`

**Must Remove** (8 files):
- 8 `mod.rs` files with manual enum dispatch

**Total Changes**: ~27 files, ~500 lines of code

### Breaking Changes

1. **Public API**: `TemplateArgs::execute()` removed
2. **Type Signatures**: Args structs become function parameters
3. **Error Handling**: Must use `clap_noun_verb::Result`
4. **Testing**: Mocks must target functions, not structs

---

## Next Steps for CLI Wiring

### Phase 1: Proof of Concept (1 command)
**Effort**: 30 minutes
**Risk**: Low

1. Choose `template list` as pilot command
2. Add `#[noun]` to `cli/src/cmds/template.rs`
3. Add `#[verb]` to `cli/src/commands/template/list.rs`
4. Refactor function signature to use direct parameters
5. Test: `cargo run -- template list`
6. Validate auto-discovery works

### Phase 2: Template Noun (7 commands)
**Effort**: 2 hours
**Risk**: Medium

1. Apply `#[verb]` to all template commands
2. Remove `TemplateCommand` enum
3. Remove `TemplateArgs::execute()` match statement
4. Update tests to call functions directly
5. Verify all 7 commands work

### Phase 3: Remaining Nouns (30+ commands)
**Effort**: 6 hours
**Risk**: High

1. Project noun (5 commands)
2. Marketplace noun (5 commands)
3. Graph noun (4 commands)
4. Hook noun (4 commands)
5. AI noun (1 command)
6. Utils noun (1 command)
7. CI noun (1 command)

### Phase 4: Cleanup & Validation
**Effort**: 1 hour
**Risk**: Low

1. Remove unused imports (`#[allow(unused_imports)]` annotations)
2. Run `cargo clippy` to verify no warnings
3. Update integration tests
4. Update documentation
5. Update examples

**Total Estimated Effort**: 9.5 hours

---

## Alternative: Hybrid Approach

### Keep Manual Routing + Document Why

If auto-discovery is not critical for v2.0.0:

1. **Remove** clap-noun-verb dependency
2. **Keep** current manual enum-based routing
3. **Document** decision in architecture docs
4. **Save** ~10 hours of migration effort

### Trade-offs

| Aspect | Auto-Discovery | Manual Routing |
|--------|---------------|----------------|
| **Boilerplate** | Low | High |
| **Maintainability** | High | Medium |
| **Type Safety** | Compile-time | Compile-time |
| **Discovery** | Runtime | Compile-time |
| **Learning Curve** | Steep | Shallow |
| **Migration Effort** | 10 hours | 0 hours |

---

## Recommendations

### Short-term (v2.0.0 Release)

**Option A: Quick Fix (Recommended)**
1. Remove unused `clap_noun_verb_macros::verb` imports
2. Document current manual routing pattern
3. Add note: "Auto-discovery planned for v2.1.0"
4. Ship v2.0.0 with working manual routing

**Effort**: 15 minutes
**Risk**: None
**Benefit**: Clean compile, no unused import warnings

### Medium-term (v2.1.0)

**Option B: Full Auto-Discovery Migration**
1. Follow 4-phase migration plan
2. Start with template noun as pilot
3. Incremental rollout to other nouns
4. Comprehensive testing

**Effort**: 10 hours
**Risk**: Medium
**Benefit**: Reduced boilerplate, better extensibility

### Long-term (v3.0.0)

**Option C: Hybrid Architecture**
1. Core commands use auto-discovery
2. Plugin commands use manual routing
3. Best of both worlds

---

## Conclusion

The ggen CLI has **partial clap-noun-verb integration** but is **missing the critical step** of applying `#[noun]` and `#[verb]` macros to enable auto-discovery. The current implementation uses **traditional clap v4 manual routing** with enum-based dispatch.

### Key Findings

1. ✅ Infrastructure is in place (linkme, directory structure)
2. ❌ Macros are imported but never applied
3. ❌ Commands are manually wired via enums
4. ❌ Auto-discovery is completely non-functional

### Decision Required

**Do we migrate to auto-discovery for v2.0.0, or defer to v2.1.0?**

- **Migrate now**: 10 hours effort, better architecture
- **Defer**: 15 minutes cleanup, ship faster

Recommend: **Quick fix for v2.0.0, full migration for v2.1.0**.

---

**Analyzed by**: Code Quality Analyzer (code-analyzer agent)
**Date**: 2025-11-01
**Session**: clap-noun-verb-integration-analysis
