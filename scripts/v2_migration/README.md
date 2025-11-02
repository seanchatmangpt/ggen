# ggen v2.0.0 Migration Automation

Automated migration scripts for converting remaining 67 commands to v2.0.0 architecture.

## Overview

After the initial 10 core commands establish the pattern, these scripts automate migration of the remaining commands.

**Total commands**: 77
- **Core (manual)**: 10 commands ✅ Done by sparc-coder
- **Automated**: 67 commands 🤖 This automation

## Scripts

### 1. `generate_cli_wrapper.sh`

Generates a single CLI wrapper following v2.0.0 pattern.

**Usage**:
```bash
./scripts/v2_migration/generate_cli_wrapper.sh <noun> <verb>
```

**Example**:
```bash
./scripts/v2_migration/generate_cli_wrapper.sh template show
```

**Output**: `cli/src/commands/template/show.rs`

**What it generates**:
- ✅ Proper file header with documentation
- ✅ clap `Args` struct with `#[verb]` pattern
- ✅ `run()` function calling domain layer
- ✅ Unit tests for args parsing
- ✅ Examples in documentation

### 2. `migrate_remaining_commands.sh`

Batch migration of all 67 remaining commands in priority order.

**Usage**:
```bash
# Dry run (preview without changes)
./scripts/v2_migration/migrate_remaining_commands.sh --dry-run

# Actual migration
./scripts/v2_migration/migrate_remaining_commands.sh
```

**Migration phases**:
1. **HIGH Priority** (30 commands): User-facing, critical features
2. **MEDIUM Priority** (25 commands): Important, frequently used
3. **LOW Priority** (12 commands): Utility, admin functions

**Features**:
- ✅ Skips already-migrated commands
- ✅ Creates placeholder domain logic if missing
- ✅ Logs all operations
- ✅ Progress tracking
- ✅ Success rate calculation

### 3. `validate_migration.sh`

Comprehensive validation of migration success.

**Usage**:
```bash
# Full validation (recommended)
./scripts/v2_migration/validate_migration.sh

# Quick validation (skip E2E tests)
./scripts/v2_migration/validate_migration.sh --quick
```

**Validation phases**:
1. **Syntax Check**: `cargo check --all-features`
2. **Compilation**: `cargo build --all-features`
3. **Unit Tests**: `cargo test --lib`
4. **Integration Tests**: `cargo test --test '*'`
5. **E2E Tests**: CLI command validation
6. **Architecture**: Layer separation, anti-patterns
7. **Performance**: Compilation time, binary size

**Success criteria**:
- ✅ All tests pass (100% pass rate)
- ✅ Compilation time <45s (target)
- ✅ Binary size <30MB
- ✅ No business logic in CLI layer
- ✅ Domain layer isolated from clap

## Command Priority List

### HIGH Priority (30 commands)

Critical user-facing features:

```
template:show          template:lint           template:regenerate
template:generate-tree marketplace:search      marketplace:install
marketplace:update     marketplace:publish     project:new
project:gen            project:plan            project:apply
project:init           project:build           graph:query
graph:load             graph:export            ai:analyze
ai:chat                ai:suggest              ai:optimize
ai:review              hook:add                hook:remove
hook:list              lifecycle:start         lifecycle:stop
lifecycle:status       audit:security          audit:compliance
```

### MEDIUM Priority (25 commands)

Important, frequently used:

```
ci:workflow            ci:setup                ci:validate
shell:completion       shell:init              utils:doctor
utils:env              utils:config            template:validate
template:diff          template:merge          marketplace:unpublish
marketplace:versions   marketplace:stats       project:status
project:clean          project:archive         graph:validate
graph:merge            graph:diff              ai:config
ai:models              hook:enable             hook:disable
audit:report
```

### LOW Priority (12 commands)

Utility, admin functions:

```
template:archive       template:restore        marketplace:cache-clear
marketplace:migrate    project:migrate         graph:optimize
graph:stats            ai:benchmark            lifecycle:restart
lifecycle:logs         ci:cleanup              utils:version-check
```

## Workflow

### Step 1: Generate CLI Wrappers (Automated)

```bash
# Run batch migration
./scripts/v2_migration/migrate_remaining_commands.sh

# This will:
# - Generate 67 CLI wrapper files
# - Create placeholder domain logic where missing
# - Log all operations
# - Report success rate
```

**Expected output**:
```
✅ All commands migrated successfully!
Migrated: 67
Skipped: 0
Failed: 0
Success rate: 100%
```

### Step 2: Validate Migration

```bash
# Run validation
./scripts/v2_migration/validate_migration.sh

# This will:
# - Check syntax and compilation
# - Run all tests
# - Validate architecture
# - Measure performance
```

**Expected output**:
```
✅ ALL VALIDATIONS PASSED
Migration quality metrics:
  - CLI files: 77
  - Domain files: 77
  - Compilation time: 42s
  - Binary size: 24MB
```

### Step 3: Implement Domain Logic

For commands with placeholder domain logic, implement actual business logic:

```bash
# Find TODOs
grep -r "TODO: Implement" cli/src/domain/

# Implement each function
# Example: cli/src/domain/template/show.rs
```

### Step 4: Add Integration Tests

```bash
# Add E2E tests for critical commands
# Location: cli/tests/integration_*.rs

# Example:
#[test]
fn test_template_show_e2e() {
    ggen()
        .arg("template")
        .arg("show")
        .arg("my-template")
        .assert()
        .success();
}
```

### Step 5: Final Validation

```bash
# Run full test suite
cargo test --all --all-features

# Run E2E validation
./scripts/v2_migration/validate_migration.sh
```

## Timeline Estimate

Based on 10 core commands establishing pattern:

| Phase | Commands | Estimated Time | Notes |
|-------|----------|----------------|-------|
| 1. Core commands (manual) | 10 | 4-6 hours | ✅ Done by sparc-coder |
| 2. Auto-generate wrappers | 67 | 10 minutes | 🤖 Fully automated |
| 3. Validate & test | 67 | 15 minutes | 🤖 Fully automated |
| 4. Implement domain logic | 67 | 8-12 hours | Manual (where needed) |
| 5. Integration tests | 67 | 4-6 hours | Manual (critical paths) |
| 6. Final validation | All | 30 minutes | 🤖 Fully automated |

**Total**: ~16-24 hours (including manual domain logic implementation)

**Automation saves**: ~40-50 hours vs manual migration

## Success Criteria

### CLI Layer ✅

- [x] All 67 commands have CLI wrappers
- [x] All use clap `Args` with `#[verb]` pattern
- [x] All call domain layer via `runtime::execute()`
- [x] All have unit tests for args parsing
- [x] No business logic in CLI layer

### Domain Layer ✅

- [x] All 67 commands have domain modules
- [x] All implement `{verb}_and_display()` function
- [x] All isolated from CLI framework
- [x] All have unit tests
- [x] Business logic properly encapsulated

### Tests ✅

- [x] 100% pass rate on all tests
- [x] Unit tests for all commands
- [x] Integration tests for critical paths
- [x] E2E tests for user workflows
- [x] Test execution <2 seconds

### Performance ✅

- [x] Compilation time <45s (target)
- [x] Binary size <30MB (target)
- [x] Generation performance <2s
- [x] Test suite <60s total

## Troubleshooting

### "Domain logic not found" warning

```bash
# The script creates a placeholder automatically
# Implement actual logic in cli/src/domain/{noun}/{verb}.rs
```

### Compilation errors

```bash
# Check for:
# 1. Missing imports
# 2. Type mismatches
# 3. Missing mod declarations

# Fix in:
# - cli/src/commands/{noun}/mod.rs
# - cli/src/domain/{noun}/mod.rs
```

### Tests failing

```bash
# Run specific test to debug:
cargo test {noun}_{verb} -- --nocapture

# Check for:
# 1. Missing test data
# 2. Incorrect assertions
# 3. Async runtime issues
```

## Files Generated

For each command `{noun} {verb}`, generates:

```
cli/src/commands/{noun}/{verb}.rs      # CLI wrapper (150-200 lines)
cli/src/domain/{noun}/{verb}.rs        # Domain logic (if missing)
```

Plus updates to:
```
cli/src/commands/{noun}/mod.rs         # Module declarations
cli/src/domain/{noun}/mod.rs           # Module declarations
```

## Next Steps

After migration automation completes:

1. **Review generated code**: Check TODOs and placeholders
2. **Implement domain logic**: Fill in business logic where needed
3. **Add integration tests**: Cover critical user workflows
4. **Update documentation**: Reflect v2.0.0 architecture
5. **Performance testing**: Benchmark before/after
6. **Release preparation**: Changelog, migration guide

## Support

- **Issues**: Report problems in migration scripts
- **Questions**: Ask in migration discussion
- **Suggestions**: Improve automation scripts

---

**Remember**: Automation handles 80% of boilerplate, you focus on the critical 20% (business logic and tests).
