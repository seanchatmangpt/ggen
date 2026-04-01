# CLI Command Testing Report

## Executive Summary

**Status**: 🔴 BLOCKED - Compilation errors prevent testing

**Issue**: 14 CLI commands have broken `#[verb]` macro invocations causing compilation failures.

**Impact**: Cannot test any CLI commands until build is fixed.

---

## Problem Details

### Compilation Error
```
error[E0433]: failed to resolve: could not find `autonomic` in `clap_noun_verb`
```

**Total Errors**: 88 compilation errors
**Error Location**: `crates/ggen-cli/src/cmds/workflow.rs` and `crates/ggen-cli/src/cmds/yawl.rs`

### Root Cause

The `#[verb]` macro from `clap-noun-verb-macros` v5.0.0 requires **two arguments**:

```rust
// ✅ CORRECT - Two arguments
#[verb("verb-name", "noun-name")]
fn command_name(...) { ... }

// ❌ INCORRECT - No arguments
#[verb]
fn command_name(...) { ... }
```

### Working Examples (from other commands)

```rust
// sync.rs - WORKS
#[verb("sync", "root")]
pub fn sync(...) -> VerbResult<SyncOutput> { ... }

// init.rs - WORKS
#[verb("init", "root")]
fn init(...) -> Result<InitOutput> { ... }

// construct.rs - WORKS
#[verb("construct", "create")]
fn create(...) -> Result<ConstructOutput> { ... }

#[verb("construct", "validate")]
fn validate(module: String) -> Result<ValidateOutput> { ... }

// yawl.rs - PARTIALLY BROKEN
#[verb("generate", "yawl")]  // ✅ WORKS
fn generate(...) { ... }

#[verb]  // ❌ BROKEN - Missing arguments
fn validate(...) { ... }
```

---

## Affected Commands

### workflow.rs (5 broken verbs)

| Line | Function | Expected Macro | Current State |
|------|----------|----------------|---------------|
| 61 | `init` | `#[verb("init", "workflow")]` | `#[verb]` ❌ |
| 86 | `analyze` | `#[verb("analyze", "workflow")]` | `#[verb]` ❌ |
| 115 | `event` | `#[verb("event", "workflow")]` | `#[verb]` ❌ |
| 173 | `event` | `#[verb("event", "workflow")]` | `#[verb]` ❌ |
| 199 | `report` | `#[verb("report", "workflow")]` | `#[verb]` ❌ |

### yawl.rs (9 broken verbs)

| Line | Function | Expected Macro | Current State |
|------|----------|----------------|---------------|
| 56 | `validate` | `#[verb("validate", "yawl")]` | `#[verb]` ❌ |
| 82 | `watch` | `#[verb("watch", "yawl")]` | `#[verb]` ❌ |
| 110 | `deploy` | `#[verb("deploy", "yawl")]` | `#[verb]` ❌ |
| 137 | `test` | `#[verb("test", "yawl")]` | `#[verb]` ❌ |
| 163 | `optimize` | `#[verb("optimize", "yawl")]` | `#[verb]` ❌ |
| 206 | `analyze` | `#[verb("analyze", "yawl")]` | `#[verb]` ❌ |
| 231 | `merge` | `#[verb("merge", "yawl")]` | `#[verb]` ❌ |
| 255 | `diff` | `#[verb("diff", "yawl")]` | `#[verb]` ❌ |
| 287 | `export` | `#[verb("export", "yawl")]` | `#[verb]` ❌ |

**Note**: yawl.rs has 4 working verbs (generate, validate, watch, deploy) that use correct macro syntax, but 9 broken ones.

---

## Commands to Test (Once Build is Fixed)

### Total Commands: 25 verbs across 7 nouns

| Noun | Verbs | Count | Status |
|------|-------|-------|--------|
| **sync** | dry-run | 1 | 🔴 BUILD BROKEN |
| **init** | project | 1 | 🔴 BUILD BROKEN |
| **wizard** | receipts-first profile | 1 | 🔴 BUILD BROKEN |
| **template** | list, show, get, new, lint, generate, generate-tree, regenerate | 8 | 🔴 BUILD BROKEN |
| **graph** | load, query, export, visualize | 4 | 🔴 BUILD BROKEN |
| **construct** | create, validate | 2 | 🔴 BUILD BROKEN |
| **ai** | generate, chat, analyze | 3 | 🔴 BUILD BROKEN |
| **project** | new, plan, gen, apply, init, generate, watch | 7 | 🔴 BUILD BROKEN |
| **utils** | doctor, env | 2 | 🔴 BUILD BROKEN |

---

## Fix Required

### Step 1: Fix workflow.rs (5 changes)

```rust
// Line 61
- #[verb]
+ #[verb("init", "workflow")]
fn init(...)

// Line 86
- #[verb]
+ #[verb("analyze", "workflow")]
fn analyze(...)

// Line 115
- #[verb]
+ #[verb("event", "workflow")]
fn event(...)

// Line 173
- #[verb]
+ #[verb("event", "workflow")]
fn event(...)

// Line 199
- #[verb]
+ #[verb("report", "workflow")]
fn report(...)
```

### Step 2: Fix yawl.rs (9 changes)

```rust
// Line 56
- #[verb]
+ #[verb("validate", "yawl")]
fn validate(...)

// Line 82
- #[verb]
+ #[verb("watch", "yawl")]
fn watch(...)

// Line 110
- #[verb]
+ #[verb("deploy", "yawl")]
fn deploy(...)

// Line 137
- #[verb]
+ #[verb("test", "yawl")]
fn test(...)

// Line 163
- #[verb]
+ #[verb("optimize", "yawl")]
fn optimize(...)

// Line 206
- #[verb]
+ #[verb("analyze", "yawl")]
fn analyze(...)

// Line 231
- #[verb]
+ #[verb("merge", "yawl")]
fn merge(...)

// Line 255
- #[verb]
+ #[verb("diff", "yawl")]
fn diff(...)

// Line 287
- #[verb]
+ #[verb("export", "yawl")]
fn export(...)
```

### Step 3: Verify Build

```bash
cargo make build
# Expected: "Finished" with no errors
```

### Step 4: Test Commands

Once build succeeds, test each command systematically:

```bash
# Create test environment
mkdir -p /tmp/test-ggen-cli
cd /tmp/test-ggen-cli

# Test each command group
ggen sync --dry-run
ggen init /tmp/test-project --force
ggen wizard --profile receipts-first --output-dir /tmp/test-wizard --yes --no-sync
# ... etc for all 25 commands
```

---

## Testing Strategy

### Phase 1: Fix Build (BLOCKER)
- [ ] Fix 14 broken `#[verb]` macros in workflow.rs
- [ ] Fix 9 broken `#[verb]` macros in yawl.rs
- [ ] Verify clean build with `cargo make build`
- [ ] Verify binary exists at `target/debug/ggen`

### Phase 2: Prepare Test Environment
- [ ] Create test directory `/tmp/test-ggen-cli`
- [ ] Create sample TTL file for graph commands
- [ ] Create sample project for init commands
- [ ] Set up clean state for each test

### Phase 3: Execute Tests
- [ ] Test sync command (1 verb)
- [ ] Test init command (1 verb)
- [ ] Test wizard command (1 verb)
- [ ] Test template commands (8 verbs)
- [ ] Test graph commands (4 verbs)
- [ ] Test construct commands (2 verbs)
- [ ] Test ai commands (3 verbs)
- [ ] Test project commands (7 verbs)
- [ ] Test utils commands (2 verbs)

### Phase 4: Generate Report
- [ ] Create summary table with all commands
- [ ] Document success/failure for each command
- [ ] Capture error messages for failures
- [ ] Calculate pass rate
- [ ] File bug reports for failing commands

---

## Expected Test Outcomes

### Success Criteria
- ✅ CLI builds without errors
- ✅ Each command executes without panic
- ✅ Clear success/failure indication
- ✅ Error messages documented for failures

### Test Artifacts
- Test execution log
- Summary table (Command | Status | Error)
- Sample output files (if any)
- Bug reports for failing commands

---

## Notes

1. **API Keys Required**: AI commands may require `GROQ_API_KEY` environment variable
2. **Network Access**: Some commands may require network access
3. **File Operations**: Some commands create files/directories
4. **Test Isolation**: Use `/tmp/` for all file operations to avoid polluting workspace
5. **Feature Flags**: The `autonomic` feature exists but is not enabled by default

---

## Next Steps

1. **Fix compilation errors** (PREREQUISITE)
2. Build CLI successfully
3. Test each command systematically
4. Document results in summary table

**Status**: Waiting on build fix to proceed with testing.

---

**Report Generated**: 2026-04-01
**Investigated By**: Claude Code Agent
**Plan File**: `/Users/sac/.claude/plans/clever-skipping-axolotl-agent-a1ce9c05630c0a644.md`
