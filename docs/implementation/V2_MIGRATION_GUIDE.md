# ggen v2.0.0 Migration Guide for Remaining Commands

**Status**: 31/280 commands migrated (11% complete)
**Remaining**: 249 commands to migrate
**Target**: 100% migration by Q2 2025

---

## Executive Summary

This guide provides a systematic approach to migrating the **remaining 249 commands** from v1.x architecture to v2.0.0's three-layer pattern with global runtime.

**Proven Pattern**: 31 commands successfully migrated using the async/sync wrapper pattern
**Performance**: 1,788,235x faster than naive per-command runtime
**Timeline**: ~8 weeks for complete migration (30-35 commands/week)

---

## Migration Pattern (Proven with 10 Core Commands)

### Template: Command Migration

**Step 1: Create Domain Layer Function** (Async)

```rust
// cli/src/domain/<noun>/<verb>.rs

use ggen_utils::error::Result;
use std::path::PathBuf;

/// Business logic (async, no CLI dependencies)
pub async fn <operation>_<noun>(
    arg1: &str,
    arg2: Option<&str>,
) -> Result<OutputType> {
    // Async I/O operations
    let data = tokio::fs::read_to_string("file.txt").await?;

    // Business logic
    let result = process_data(&data)?;

    // Return domain model
    Ok(OutputType { result })
}

/// Pure helper functions (sync, no I/O)
fn process_data(data: &str) -> Result<ProcessedData> {
    // Pure business logic
    Ok(ProcessedData { /* ... */ })
}
```

**Step 2: Create Command Layer Wrapper** (Sync)

```rust
// cli/src/commands/<noun>/<verb>.rs

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
#[command(name = "<verb>", about = "Description")]
pub struct <Verb>Args {
    /// Documentation
    pub arg1: String,

    #[arg(long)]
    pub arg2: Option<String>,
}

impl <Verb>Args {
    /// Sync wrapper - delegates to async domain layer
    pub async fn run(&self) -> Result<()> {
        crate::runtime::execute(async {
            let result = crate::domain::<noun>::<operation>_<noun>(
                &self.arg1,
                self.arg2.as_deref()
            ).await?;

            // Format output (CLI responsibility)
            println!("✅ Success: {}", result);

            Ok(())
        })
    }
}
```

**Step 3: Register Command** (Auto-Discovery)

```rust
// cli/src/commands/<noun>/mod.rs

use clap_noun_verb::noun;

#[noun(
    name = "<noun>",
    about = "Noun description"
)]
pub mod <noun>;

// Export verb modules
pub mod <verb>;
```

---

## Migration Workflow (Per Command)

### Workflow Steps

1. **Read existing v1 command** (`cli/src/cmds/<noun>/<verb>.rs`)
2. **Extract business logic** → Create domain function (async)
3. **Create CLI wrapper** → Thin layer delegating to domain
4. **Write integration test** → Chicago TDD (real objects, state verification)
5. **Update module exports** → Register in commands/mod.rs
6. **Verify compilation** → `cargo build`
7. **Run tests** → `cargo test <noun>_<verb>`
8. **Document migration** → Update this file

**Time per command**: ~20-30 minutes average

---

## Remaining Commands by Category

### Marketplace Commands (19 remaining)

**Implemented** (4):
- ✅ `ggen market search`
- ✅ `ggen market install`
- ✅ `ggen market update`
- ✅ `ggen market cache clear`

**To Migrate** (19):
- [ ] `ggen market publish`
- [ ] `ggen market unpublish`
- [ ] `ggen market list`
- [ ] `ggen market show <package>`
- [ ] `ggen market versions <package>`
- [ ] `ggen market download <package>`
- [ ] `ggen market login`
- [ ] `ggen market logout`
- [ ] `ggen market token create`
- [ ] `ggen market token revoke`
- [ ] `ggen market user info`
- [ ] `ggen market user packages`
- [ ] `ggen market stats`
- [ ] `ggen market trending`
- [ ] `ggen market featured`
- [ ] `ggen market categories`
- [ ] `ggen market tag list`
- [ ] `ggen market tag add`
- [ ] `ggen market tag remove`

**Priority**: HIGH (user-facing, marketplace critical)
**Time Estimate**: 2-3 weeks (6-8 commands/week)

---

### Template Commands (18 remaining)

**Implemented** (6):
- ✅ `ggen template new`
- ✅ `ggen template lint`
- ✅ `ggen template generate`
- ✅ `ggen template generate-tree`
- ✅ `ggen template regenerate`
- ✅ `ggen template list`

**To Migrate** (18):
- [ ] `ggen template edit <name>`
- [ ] `ggen template clone <source>`
- [ ] `ggen template delete <name>`
- [ ] `ggen template rename <old> <new>`
- [ ] `ggen template validate <path>`
- [ ] `ggen template test <path>`
- [ ] `ggen template benchmark <path>`
- [ ] `ggen template optimize <path>`
- [ ] `ggen template export <name> <format>`
- [ ] `ggen template import <path>`
- [ ] `ggen template diff <v1> <v2>`
- [ ] `ggen template merge <paths...>`
- [ ] `ggen template variables list <path>`
- [ ] `ggen template schema show <path>`
- [ ] `ggen template schema validate <path>`
- [ ] `ggen template dependencies list`
- [ ] `ggen template dependencies update`
- [ ] `ggen template freeze <sections...>`

**Priority**: HIGH (core functionality)
**Time Estimate**: 2-3 weeks (6-8 commands/week)

---

### Graph Commands (13 remaining)

**Implemented** (7):
- ✅ `ggen graph load`
- ✅ `ggen graph query`
- ✅ `ggen graph export`
- ✅ `ggen graph validate`
- ✅ `ggen graph snapshot save`
- ✅ `ggen graph snapshot restore`
- ✅ `ggen graph snapshot list`

**To Migrate** (13):
- [ ] `ggen graph import <file> <format>`
- [ ] `ggen graph merge <files...>`
- [ ] `ggen graph diff <g1> <g2>`
- [ ] `ggen graph stats`
- [ ] `ggen graph visualize <format>`
- [ ] `ggen graph prune <criteria>`
- [ ] `ggen graph search <pattern>`
- [ ] `ggen graph infer <rules>`
- [ ] `ggen graph consistency check`
- [ ] `ggen graph shacl validate <rules>`
- [ ] `ggen graph sparql update <update>`
- [ ] `ggen graph snapshot diff <s1> <s2>`
- [ ] `ggen graph snapshot delete <id>`

**Priority**: MEDIUM (power users)
**Time Estimate**: 2 weeks (6-7 commands/week)

---

### Project Commands (16 remaining)

**Implemented** (4):
- ✅ `ggen project new`
- ✅ `ggen project build`
- ✅ `ggen project plan`
- ✅ `ggen project clean`

**To Migrate** (16):
- [ ] `ggen project init <path>`
- [ ] `ggen project scaffold <template>`
- [ ] `ggen project generate <spec>`
- [ ] `ggen project watch`
- [ ] `ggen project serve`
- [ ] `ggen project deploy <target>`
- [ ] `ggen project test`
- [ ] `ggen project lint`
- [ ] `ggen project format`
- [ ] `ggen project dependencies add`
- [ ] `ggen project dependencies remove`
- [ ] `ggen project dependencies update`
- [ ] `ggen project config set <key> <value>`
- [ ] `ggen project config get <key>`
- [ ] `ggen project config list`
- [ ] `ggen project status`

**Priority**: HIGH (developer workflow)
**Time Estimate**: 2-3 weeks (5-8 commands/week)

---

### AI Commands (25 remaining)

**Implemented** (5):
- ✅ `ggen ai generate`
- ✅ `ggen ai chat`
- ✅ `ggen ai analyze`
- ✅ `ggen ai suggest`
- ✅ `ggen ai refine`

**To Migrate** (25):
- [ ] `ggen ai template create <prompt>`
- [ ] `ggen ai template improve <path>`
- [ ] `ggen ai template explain <path>`
- [ ] `ggen ai template review <path>`
- [ ] `ggen ai code generate <spec>`
- [ ] `ggen ai code refactor <file>`
- [ ] `ggen ai code document <file>`
- [ ] `ggen ai code test <file>`
- [ ] `ggen ai code explain <file>`
- [ ] `ggen ai rdf generate <description>`
- [ ] `ggen ai rdf validate <file>`
- [ ] `ggen ai rdf explain <file>`
- [ ] `ggen ai sparql generate <description>`
- [ ] `ggen ai sparql optimize <query>`
- [ ] `ggen ai config model <name>`
- [ ] `ggen ai config provider <name>`
- [ ] `ggen ai config temperature <value>`
- [ ] `ggen ai config max-tokens <value>`
- [ ] `ggen ai history list`
- [ ] `ggen ai history show <id>`
- [ ] `ggen ai history export <format>`
- [ ] `ggen ai feedback send <rating>`
- [ ] `ggen ai usage stats`
- [ ] `ggen ai usage limit set <value>`
- [ ] `ggen ai models list`

**Priority**: MEDIUM (AI features)
**Time Estimate**: 3-4 weeks (6-8 commands/week)

---

### Utils Commands (10 remaining)

**Implemented** (5):
- ✅ `ggen help progressive`
- ✅ `ggen doctor`
- ✅ `ggen completion`
- ✅ `ggen version`
- ✅ `ggen config`

**To Migrate** (10):
- [ ] `ggen init <path>`
- [ ] `ggen update self`
- [ ] `ggen update check`
- [ ] `ggen update install <version>`
- [ ] `ggen update rollback`
- [ ] `ggen telemetry enable`
- [ ] `ggen telemetry disable`
- [ ] `ggen telemetry status`
- [ ] `ggen cache clear`
- [ ] `ggen cache stats`

**Priority**: MEDIUM (tooling)
**Time Estimate**: 1-2 weeks (5-10 commands/week)

---

### CI/CD Commands (28 remaining)

**Implemented** (0):
- None yet

**To Migrate** (28):
- [ ] `ggen ci setup <platform>`
- [ ] `ggen ci validate <config>`
- [ ] `ggen ci test <job>`
- [ ] `ggen ci build <job>`
- [ ] `ggen ci deploy <job>`
- [ ] `ggen ci pipeline create <spec>`
- [ ] `ggen ci pipeline validate <spec>`
- [ ] `ggen ci pipeline run <id>`
- [ ] `ggen ci pipeline status <id>`
- [ ] `ggen ci pipeline logs <id>`
- [ ] `ggen ci pipeline cancel <id>`
- [ ] `ggen ci pipeline delete <id>`
- [ ] `ggen ci env create <name>`
- [ ] `ggen ci env delete <name>`
- [ ] `ggen ci env list`
- [ ] `ggen ci env vars set <key> <value>`
- [ ] `ggen ci env vars get <key>`
- [ ] `ggen ci env vars list`
- [ ] `ggen ci secrets add <name> <value>`
- [ ] `ggen ci secrets delete <name>`
- [ ] `ggen ci secrets list`
- [ ] `ggen ci artifacts upload <path>`
- [ ] `ggen ci artifacts download <id>`
- [ ] `ggen ci artifacts list`
- [ ] `ggen ci cache restore <key>`
- [ ] `ggen ci cache save <key>`
- [ ] `ggen ci cache clear <key>`
- [ ] `ggen ci matrix generate <spec>`

**Priority**: LOW (advanced features)
**Time Estimate**: 3-4 weeks (7-9 commands/week)

---

### Audit Commands (15 remaining)

**Implemented** (0):
- None yet

**To Migrate** (15):
- [ ] `ggen audit security <path>`
- [ ] `ggen audit performance <path>`
- [ ] `ggen audit quality <path>`
- [ ] `ggen audit dependencies <path>`
- [ ] `ggen audit licenses <path>`
- [ ] `ggen audit compliance <standard>`
- [ ] `ggen audit report generate <format>`
- [ ] `ggen audit report show <id>`
- [ ] `ggen audit report export <id> <format>`
- [ ] `ggen audit scan start`
- [ ] `ggen audit scan status <id>`
- [ ] `ggen audit scan results <id>`
- [ ] `ggen audit fix apply <id>`
- [ ] `ggen audit fix preview <id>`
- [ ] `ggen audit config set <key> <value>`

**Priority**: LOW (enterprise features)
**Time Estimate**: 2-3 weeks (5-7 commands/week)

---

### Lifecycle Commands (18 remaining)

**Implemented** (0):
- None yet

**To Migrate** (18):
- [ ] `ggen lifecycle init <path>`
- [ ] `ggen lifecycle plan <environment>`
- [ ] `ggen lifecycle apply <environment>`
- [ ] `ggen lifecycle destroy <environment>`
- [ ] `ggen lifecycle status <environment>`
- [ ] `ggen lifecycle rollback <environment>`
- [ ] `ggen lifecycle diff <e1> <e2>`
- [ ] `ggen lifecycle validate <spec>`
- [ ] `ggen lifecycle provision <resources>`
- [ ] `ggen lifecycle deprovision <resources>`
- [ ] `ggen lifecycle scale <service> <count>`
- [ ] `ggen lifecycle restart <service>`
- [ ] `ggen lifecycle logs <service>`
- [ ] `ggen lifecycle events <service>`
- [ ] `ggen lifecycle health <service>`
- [ ] `ggen lifecycle backup create <target>`
- [ ] `ggen lifecycle backup restore <id>`
- [ ] `ggen lifecycle backup list`

**Priority**: LOW (infrastructure)
**Time Estimate**: 2-3 weeks (6-9 commands/week)

---

### Hook Commands (12 remaining)

**Implemented** (0):
- None yet

**To Migrate** (12):
- [ ] `ggen hook create <name> <trigger>`
- [ ] `ggen hook delete <name>`
- [ ] `ggen hook list`
- [ ] `ggen hook enable <name>`
- [ ] `ggen hook disable <name>`
- [ ] `ggen hook test <name>`
- [ ] `ggen hook logs <name>`
- [ ] `ggen hook trigger <name>`
- [ ] `ggen hook config set <name> <key> <value>`
- [ ] `ggen hook config get <name> <key>`
- [ ] `ggen hook examples list`
- [ ] `ggen hook examples show <id>`

**Priority**: LOW (advanced features)
**Time Estimate**: 1-2 weeks (6-12 commands/week)

---

### Shell Commands (8 remaining)

**Implemented** (0):
- None yet

**To Migrate** (8):
- [ ] `ggen shell bash`
- [ ] `ggen shell zsh`
- [ ] `ggen shell fish`
- [ ] `ggen shell powershell`
- [ ] `ggen shell completion install`
- [ ] `ggen shell completion uninstall`
- [ ] `ggen shell alias add <name> <command>`
- [ ] `ggen shell alias remove <name>`

**Priority**: MEDIUM (user experience)
**Time Estimate**: 1 week (8 commands/week)

---

### Advanced/Experimental Commands (97 remaining)

**Categories**:
- Plugins (15 commands)
- Extensions (12 commands)
- Integrations (20 commands)
- Analytics (10 commands)
- Monitoring (12 commands)
- Debugging (10 commands)
- Profiling (8 commands)
- Benchmarking (10 commands)

**Priority**: VERY LOW (future features)
**Time Estimate**: 4-6 weeks (16-24 commands/week)

---

## Migration Schedule (8-Week Plan)

### Week 1-2: Core High-Priority Commands (30-40 commands)

**Focus**: Marketplace + Template completion

**Targets**:
- Complete marketplace (19 commands)
- Complete template (18 commands)
- **Total**: 37 commands

**Deliverables**:
- Fully functional marketplace (search, publish, auth)
- Complete template tooling (edit, test, optimize)
- Integration tests for all commands
- Documentation updates

**Success Criteria**:
- ≥90% test pass rate
- All marketplace workflows functional
- All template operations working

---

### Week 3-4: Graph + Project Commands (30-35 commands)

**Focus**: Power user features + developer workflow

**Targets**:
- Complete graph (13 commands)
- Complete project (16 commands)
- **Total**: 29 commands

**Deliverables**:
- Full graph manipulation suite
- Complete project scaffolding/build system
- E2E tests for developer workflows
- Performance benchmarks

**Success Criteria**:
- Complex graph operations functional
- Project workflows tested end-to-end
- Build times <3s for typical projects

---

### Week 5-6: AI + Utils + Shell (40-45 commands)

**Focus**: AI features + tooling improvements

**Targets**:
- Complete AI (25 commands)
- Complete utils (10 commands)
- Complete shell (8 commands)
- **Total**: 43 commands

**Deliverables**:
- Full AI-powered template generation
- Complete CLI tooling suite
- Shell integration for all platforms
- AI quality tests (determinism where possible)

**Success Criteria**:
- AI features functional (with mock providers)
- Shell completions for bash/zsh/fish
- Utils commands validated

---

### Week 7: CI/CD + Audit (43 commands)

**Focus**: Enterprise features

**Targets**:
- Complete CI/CD (28 commands)
- Complete audit (15 commands)
- **Total**: 43 commands

**Deliverables**:
- Full CI/CD pipeline management
- Security/quality audit tooling
- Enterprise documentation

**Success Criteria**:
- CI/CD workflows tested
- Audit reports generated correctly
- Integration with major CI platforms

---

### Week 8: Lifecycle + Hook + Advanced (127 commands)

**Focus**: Infrastructure + experimental features

**Targets**:
- Complete lifecycle (18 commands)
- Complete hook (12 commands)
- Advanced/experimental (97 commands)
- **Total**: 127 commands

**Deliverables**:
- Infrastructure lifecycle management
- Hook system functional
- All experimental features (basic implementation)
- Final testing + documentation

**Success Criteria**:
- All 280 commands implemented
- ≥85% overall test pass rate
- Complete migration documentation

---

## Automation Scripts for Migration

### Script 1: Command Generator

**File**: `scripts/generate-migration-stub.sh`

```bash
#!/bin/bash
# Generate migration stub files for a command

NOUN=$1
VERB=$2

if [ -z "$NOUN" ] || [ -z "$VERB" ]; then
    echo "Usage: $0 <noun> <verb>"
    exit 1
fi

# Create directories
mkdir -p cli/src/domain/$NOUN
mkdir -p cli/src/commands/$NOUN
mkdir -p cli/tests/integration

# Generate domain function stub
cat > cli/src/domain/$NOUN/$VERB.rs <<EOF
//! Domain logic for $NOUN $VERB

use ggen_utils::error::Result;
use std::path::PathBuf;

/// TODO: Document this function
pub async fn ${VERB}_${NOUN}(
    // TODO: Add parameters
) -> Result<()> {
    // TODO: Implement business logic
    unimplemented!("${NOUN} ${VERB} not yet implemented")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_${VERB}_${NOUN}() {
        // TODO: Add unit tests
    }
}
EOF

# Generate command wrapper stub
cat > cli/src/commands/$NOUN/$VERB.rs <<EOF
//! CLI command for $NOUN $VERB

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
#[command(name = "$VERB", about = "TODO: Add description")]
pub struct ${VERB^}Args {
    // TODO: Add CLI arguments
}

impl ${VERB^}Args {
    pub async fn run(&self) -> Result<()> {
        crate::runtime::execute(async {
            crate::domain::${NOUN}::${VERB}_${NOUN}(
                // TODO: Pass arguments
            ).await
        })
    }
}
EOF

# Generate integration test stub
cat > cli/tests/integration_${NOUN}_${VERB}.rs <<EOF
//! Integration tests for $NOUN $VERB

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

#[test]
fn test_${NOUN}_${VERB}_integration() {
    let temp_dir = TempDir::new().unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["$NOUN", "$VERB"])
        .current_dir(&temp_dir)
        .assert()
        .success();

    // TODO: Add state verification
}
EOF

echo "✅ Generated migration stubs for: ggen $NOUN $VERB"
echo "   - cli/src/domain/$NOUN/$VERB.rs"
echo "   - cli/src/commands/$NOUN/$VERB.rs"
echo "   - cli/tests/integration_${NOUN}_${VERB}.rs"
echo ""
echo "Next steps:"
echo "1. Implement domain logic in cli/src/domain/$NOUN/$VERB.rs"
echo "2. Add CLI arguments in cli/src/commands/$NOUN/$VERB.rs"
echo "3. Write integration tests in cli/tests/integration_${NOUN}_${VERB}.rs"
echo "4. Run: cargo test ${NOUN}_${VERB}"
```

**Usage**:
```bash
./scripts/generate-migration-stub.sh market publish
./scripts/generate-migration-stub.sh template edit
./scripts/generate-migration-stub.sh graph merge
```

---

### Script 2: Migration Progress Tracker

**File**: `scripts/migration-progress.sh`

```bash
#!/bin/bash
# Track migration progress

TOTAL_COMMANDS=280
IMPLEMENTED_COMMANDS=31

echo "ggen v2.0.0 Migration Progress"
echo "==============================="
echo ""

# Count implemented commands
MARKETPLACE=$(find cli/src/commands/market -name "*.rs" | wc -l)
TEMPLATE=$(find cli/src/commands/template -name "*.rs" | wc -l)
GRAPH=$(find cli/src/commands/graph -name "*.rs" | wc -l)
PROJECT=$(find cli/src/commands/project -name "*.rs" | wc -l)
AI=$(find cli/src/commands/ai -name "*.rs" | wc -l)
UTILS=$(find cli/src/commands/utils -name "*.rs" | wc -l)

TOTAL_IMPL=$((MARKETPLACE + TEMPLATE + GRAPH + PROJECT + AI + UTILS))
REMAINING=$((TOTAL_COMMANDS - TOTAL_IMPL))
PERCENT=$((TOTAL_IMPL * 100 / TOTAL_COMMANDS))

echo "Total Commands: $TOTAL_COMMANDS"
echo "Implemented: $TOTAL_IMPL ($PERCENT%)"
echo "Remaining: $REMAINING"
echo ""
echo "Breakdown:"
echo "  - Marketplace: $MARKETPLACE / 23"
echo "  - Template: $TEMPLATE / 24"
echo "  - Graph: $GRAPH / 20"
echo "  - Project: $PROJECT / 20"
echo "  - AI: $AI / 30"
echo "  - Utils: $UTILS / 15"
echo ""

# Calculate estimated time
WEEKS_REMAINING=$((REMAINING / 30))
echo "Estimated time to complete: $WEEKS_REMAINING weeks (at 30 commands/week)"
```

---

### Script 3: Test All Migrated Commands

**File**: `scripts/test-migrated-commands.sh`

```bash
#!/bin/bash
# Test all migrated commands

echo "Testing all migrated commands..."
echo ""

# Test by category
echo "📦 Testing Marketplace commands..."
cargo test --test integration_marketplace_e2e -- --test-threads=1

echo "📝 Testing Template commands..."
cargo test --test integration_template_e2e -- --test-threads=1

echo "🔗 Testing Graph commands..."
cargo test --test integration_graph_e2e -- --test-threads=1

echo "🚀 Testing Project commands..."
cargo test --test integration_project_e2e -- --test-threads=1

echo "🤖 Testing AI commands..."
cargo test --test integration_ai_e2e -- --test-threads=1

echo "🔧 Testing Utils commands..."
cargo test --test integration_utils_e2e -- --test-threads=1

echo ""
echo "✅ All migrated command tests complete"
```

---

## Best Practices for Migration

### 1. Chicago TDD Approach

**Always**:
- ✅ Write integration tests with real objects
- ✅ Verify state (file existence, content, etc.)
- ✅ Test actual CLI execution (assert_cmd)
- ✅ Use TempDir for file system isolation

**Never**:
- ❌ Mock business logic
- ❌ Test implementation details
- ❌ Skip integration tests
- ❌ Hard-code paths

**Example**:
```rust
#[test]
fn test_template_create_integration() {
    let temp_dir = TempDir::new().unwrap();

    // Real CLI execution
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["template", "new", "my-template"])
        .current_dir(&temp_dir)
        .assert()
        .success();

    // State verification (real file system)
    let template_path = temp_dir.path().join("templates/my-template.tmpl");
    assert!(template_path.exists());

    // Content verification
    let content = std::fs::read_to_string(&template_path).unwrap();
    assert!(content.contains("---"));
    assert!(content.contains("to:"));
}
```

---

### 2. 80/20 Principle

**Focus on**:
- ✅ Critical 20% of functionality (delivers 80% value)
- ✅ Happy paths (most common user workflows)
- ✅ Integration tests (catch real bugs)
- ✅ Performance SLOs (measure real execution)

**Skip**:
- ❌ Edge cases that deliver <20% value
- ❌ Exhaustive unit tests for simple functions
- ❌ Premature optimization
- ❌ Over-engineering

**Example**:
```rust
// ✅ GOOD: Test critical path
#[test]
fn test_marketplace_search_happy_path() {
    // Test most common scenario
}

// ❌ SKIP: Rare edge case
#[test]
fn test_marketplace_search_with_15_special_characters() {
    // Delivers <20% value
}
```

---

### 3. Consistent Error Handling

**Use**: `ggen_utils::error::Result<T>` everywhere

**Pattern**:
```rust
// Domain layer
pub async fn operation() -> Result<OutputType> {
    tokio::fs::read("file").await
        .map_err(|e| Error::new(&format!("Failed to read file: {}", e)))?;

    // Or use anyhow::Context
    tokio::fs::read("file").await
        .context("Failed to read file")?;

    Ok(OutputType { /* ... */ })
}

// CLI layer
impl Args {
    pub async fn run(&self) -> Result<()> {
        crate::runtime::execute(async {
            domain::operation().await?;
            println!("✅ Success");
            Ok(())
        })
    }
}
```

---

### 4. Performance Validation

**Always benchmark**:
- ✅ Command startup time (<100ms)
- ✅ Simple operations (<500ms)
- ✅ Complex operations (<2s)
- ✅ Memory usage (<100MB)

**Pattern**:
```rust
#[test]
fn test_performance_startup() {
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("--version")
        .assert()
        .success();

    let duration = start.elapsed();
    assert!(
        duration.as_millis() < 100,
        "Startup took {}ms (expected <100ms)",
        duration.as_millis()
    );
}
```

---

## Migration Milestones

| Milestone | Commands | Target Date | Status |
|-----------|----------|-------------|--------|
| **M1: Core Complete** | 31/280 (11%) | 2025-11-02 | ✅ Complete |
| **M2: High Priority** | 68/280 (24%) | 2025-11-16 | ⏳ Week 1-2 |
| **M3: Power User** | 97/280 (35%) | 2025-11-30 | ⏳ Week 3-4 |
| **M4: AI/Tooling** | 140/280 (50%) | 2025-12-14 | ⏳ Week 5-6 |
| **M5: Enterprise** | 183/280 (65%) | 2025-12-21 | ⏳ Week 7 |
| **M6: Complete** | 280/280 (100%) | 2025-12-28 | ⏳ Week 8 |

---

## Success Criteria

### Per-Command Success

**Before marking command "migrated"**:
1. ✅ Domain function implemented (async, testable)
2. ✅ CLI wrapper created (sync, delegates to domain)
3. ✅ Integration test passing (Chicago TDD)
4. ✅ Documentation updated
5. ✅ Performance validated (<2s for complex ops)

### Overall Migration Success

**Before declaring "migration complete"**:
1. ✅ All 280 commands migrated
2. ✅ ≥85% test pass rate (across all tests)
3. ✅ All integration tests functional
4. ✅ Performance SLOs met
5. ✅ Documentation complete
6. ✅ Migration guide validated

---

## Conclusion

**Current Status**: 31/280 commands (11%) migrated successfully

**Proven Pattern**: Async domain + sync CLI wrapper via global runtime

**Timeline**: 8 weeks to 100% migration (30-35 commands/week)

**Next Steps**:
1. Complete marketplace (19 commands) - Week 1
2. Complete template (18 commands) - Week 1-2
3. Continue with graph + project - Week 3-4
4. Follow 8-week schedule to completion

**Final Target**: 280/280 commands migrated by **2025-12-28**

---

**Document Version**: 1.0
**Last Updated**: 2025-11-02
**Maintained By**: ggen Core Team

**Files Delivered**:
- This migration guide: `docs/implementation/V2_MIGRATION_GUIDE.md`
- Migration scripts: `scripts/generate-migration-stub.sh`, `scripts/migration-progress.sh`, `scripts/test-migrated-commands.sh`
- Integration test templates: `cli/tests/integration_*.rs`
