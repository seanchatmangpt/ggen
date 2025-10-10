# GGen CLI Refactoring Plan
## Aligning with The GGen Cookbook 2nd Edition Pattern Language

**Author**: CLIArchitect Agent
**Date**: 2025-10-09
**Version**: 1.0
**Status**: Draft for Review

---

## Executive Summary

This document provides a comprehensive refactoring plan to align the GGen CLI with the **GGen Cookbook 2nd Edition** pattern language, specifically **Pattern 004: NOUN-VERB CLI**. The plan prioritizes high-impact changes using the 80/20 principle while maintaining backward compatibility during migration.

### Current State
- ✅ **Migrated to noun-verb**: `project`, `market`, `template`, `graph`, `audit`, `ci`, `shell`
- ❌ **Legacy flat commands**: `search`, `add`, `remove`, `list`, `show`, `lint`, `gen`, `categories`, `packs`, `update`
- 🔴 **Missing patterns**: Pattern 009 (project test), Pattern 015 (freeze), Pattern 091 (inject), Pattern 021 (hooks)

### Target State
- 100% noun-verb CLI structure
- Complete cookbook pattern coverage
- Zero legacy commands
- Consistent verb semantics across all nouns

---

## 80/20 Analysis: Maximum Impact, Minimum Effort

### The 20% Changes That Deliver 80% Value

| Change | Impact | Effort | Priority | Value Score |
|--------|--------|--------|----------|-------------|
| 1. Remove legacy commands | 🟢 High | 🟡 Medium | P0 | 9/10 |
| 2. Add `project test` verb | 🟢 High | 🟢 Low | P0 | 10/10 |
| 3. Add `hook` noun | 🟢 High | 🟢 Low | P0 | 9/10 |
| 4. Add `project freeze/inject` | 🟡 Medium | 🟡 Medium | P1 | 7/10 |
| 5. Complete `graph` verbs | 🟡 Medium | 🟢 Low | P1 | 6/10 |
| 6. Add `template validate` | 🟡 Medium | 🟢 Low | P1 | 6/10 |
| 7. Standardize error messages | 🟢 High | 🟡 Medium | P2 | 7/10 |
| 8. Add verb aliases | 🟡 Medium | 🟢 Low | P2 | 5/10 |

**Quick Win**: Items 1-3 deliver 80% of the value with minimal effort.

---

## Pattern Mapping: Cookbook → CLI Commands

### Pattern 001: KNOWLEDGE-FIRST PROJECTION
**Implementation**: `graph` noun

```bash
# Current: ✅ Already implemented
ggen graph load domain-model.ttl
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
ggen graph export --format json
ggen graph validate --shapes shapes.ttl
ggen graph stats

# Missing: None (fully implemented)
```

---

### Pattern 002: DETERMINISTIC ENGINE
**Implementation**: Global flags + `project` verbs

```bash
# Current: ✅ Partially implemented
ggen project gen "template.tmpl" --seed 12345

# Missing: ❌ Need explicit flag
ggen --deterministic project gen "template.tmpl"
ggen --seed 42 project gen "template.tmpl"
```

**Action Required**: Add `--deterministic` global flag

---

### Pattern 003: GRAPH-TEMPLATE BINDING
**Implementation**: Template frontmatter (no CLI changes)

**Status**: ✅ No CLI changes needed

---

### Pattern 004: NOUN-VERB CLI ⭐⭐⭐
**Implementation**: Complete CLI restructure

**Status**: 🟡 70% complete, see "Migration Strategy" below

---

### Pattern 009: PROJECT PLAN ⭐⭐
**Implementation**: `project` noun

```bash
# Current: ✅ Implemented
ggen project plan "template.tmpl" --vars name=myapp
ggen project apply plan.json
ggen project diff "template.tmpl"

# Missing: ❌ Need test verb
ggen project test "template.tmpl"  # Validate plan without executing
ggen project test plan.json        # Validate existing plan
```

**Action Required**: Add `project test` verb

---

### Pattern 010: IDEMPOTENT APPLY
**Implementation**: `project apply` verb

```bash
# Current: ✅ Already implemented
ggen project apply plan.json

# Enhancement: Add safety flags
ggen project apply plan.json --force
ggen project apply plan.json --if-changed
```

---

### Pattern 011: DRY-RUN DIFF
**Implementation**: `project diff` verb

```bash
# Current: ✅ Already implemented
ggen project diff "template.tmpl" --vars name=myapp

# Enhancement: Better output formats
ggen project diff "template.tmpl" --format unified
ggen project diff "template.tmpl" --format json
```

---

### Pattern 012: CI DRIFT CHECK
**Implementation**: `ci` noun

```bash
# Current: ✅ Partially implemented
ggen ci workflow
ggen ci pages
ggen ci trigger
ggen ci release

# Missing: ❌ Drift detection verb
ggen ci drift-check              # Detect configuration drift
ggen ci drift-check --fix         # Auto-fix drift
ggen ci drift-check --report      # Generate drift report
```

**Action Required**: Add `ci drift-check` verb

---

### Pattern 015: IMMUTABILITY FIRST (Freeze Blocks) ⭐⭐
**Implementation**: `project freeze` verb

```bash
# Missing: ❌ Not implemented
ggen project freeze "src/models/user.rs" --blocks custom_methods
ggen project freeze "src/services/" --auto-detect
ggen project freeze --list        # List all freeze blocks
```

**Action Required**: Add `project freeze` verb

---

### Pattern 091: IDEMPOTENT INJECTION ⭐⭐
**Implementation**: `project inject` verb

```bash
# Missing: ❌ Not implemented
ggen project inject "add_method.tmpl" --target "src/service.rs" --anchor "INJECT_METHODS_HERE"
ggen project inject "imports.tmpl" --target "src/main.rs" --anchor "INJECT_IMPORTS_HERE" --sort
```

**Action Required**: Add `project inject` verb

---

### Pattern 021: KNOWLEDGE HOOKS ⭐⭐⭐
**Implementation**: New `hook` noun

```bash
# Missing: ❌ Not implemented
ggen hook install --all           # Install all Git hooks
ggen hook install pre-commit      # Install specific hook
ggen hook uninstall               # Remove hooks
ggen hook status                  # Show installed hooks
ggen hook list                    # List available hooks
ggen hook run pre-commit          # Manually run hook
ggen hook configure               # Configure hook behavior
```

**Action Required**: Add complete `hook` noun with all verbs

---

### Pattern 022: DELTA-DRIVEN REGENERATION
**Implementation**: `template regenerate` verb

```bash
# Current: ✅ Already implemented
ggen template regenerate "template.tmpl" --delta-only

# Enhancement: Better delta options
ggen template regenerate --since-commit abc123
ggen template regenerate --changed-files "src/*.rs"
```

---

### Pattern 024: GIT-AS-RUNTIME
**Implementation**: `ci` and `hook` nouns

**Status**: 🟡 Partially covered by Pattern 021 and 012

---

## Consistency Matrix: Verbs Across Nouns

This matrix shows which verbs should be available for each noun, ensuring **semantic consistency**.

| Noun | new | list | show | add | remove | search | update | validate | gen | plan | apply | diff | test | inject | freeze |
|------|-----|------|------|-----|--------|--------|--------|----------|-----|------|-------|------|------|--------|--------|
| **project** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ |
| **template** | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **market** | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **graph** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ | ❌ |
| **audit** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **ci** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **shell** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **hook** | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |

### Missing Verbs Analysis

**Critical Missing Verbs**:
1. `project test` - Pattern 009 requirement
2. `project inject` - Pattern 091 requirement
3. `project freeze` - Pattern 015 requirement
4. `template validate` - Quality assurance
5. `ci drift-check` - Pattern 012 requirement
6. Entire `hook` noun - Pattern 021 requirement

---

## Migration Strategy: Removing Legacy Commands

### Phase 1: Add Deprecation Warnings (v0.2.5)
**Duration**: 1 release cycle (1 month)

```rust
// cli/src/cmds/search.rs
#[deprecated(since = "0.2.5", note = "Use `ggen market search` instead")]
pub async fn run(args: &SearchArgs) -> Result<()> {
    eprintln!("⚠️  WARNING: 'ggen search' is deprecated. Use 'ggen market search' instead.");
    eprintln!("   This command will be removed in v0.3.0");
    // ... existing implementation
}
```

**Changes**:
- Add `#[deprecated]` attributes to all legacy commands
- Print migration hints at runtime
- Update `--help` text with deprecation notices
- Create migration guide document

---

### Phase 2: Update Documentation (v0.2.5)
**Duration**: Concurrent with Phase 1

**Tasks**:
- Update README.md with new command structure
- Create MIGRATION.md guide
- Update examples in cookbook
- Add "migrating from legacy" section to docs

---

### Phase 3: Add Alias Support (v0.2.6)
**Duration**: 1 release cycle (1 month)

```bash
# Allow users to create aliases during migration
ggen alias add search "market search"
ggen alias add gen "project gen"
ggen alias list
ggen alias remove search
```

**Implementation**:
```rust
// cli/src/cmds/alias/mod.rs
pub struct AliasCmd {
    #[command(subcommand)]
    pub verb: AliasVerb,
}

pub enum AliasVerb {
    Add(AddArgs),
    Remove(RemoveArgs),
    List,
}
```

---

### Phase 4: Remove Legacy Commands (v0.3.0)
**Duration**: 1 release cycle (1 month)

**Breaking Changes**:
```rust
// cli/src/cmds/mod.rs
// DELETE these modules:
// pub mod add;
// pub mod categories;
// pub mod gen;
// pub mod lint;
// pub mod list;
// pub mod packs;
// pub mod remove;
// pub mod search;
// pub mod show;
// pub mod update;

// DELETE from Commands enum:
// Commands::Search(_) => { ... }
// Commands::Add(_) => { ... }
// ... etc
```

**Migration Table**:

| Legacy Command | New Command | Notes |
|----------------|-------------|-------|
| `ggen search` | `ggen market search` | Exact replacement |
| `ggen add` | `ggen market add` | Exact replacement |
| `ggen remove` | `ggen market remove` | Exact replacement |
| `ggen packs` | `ggen market list` | Renamed for clarity |
| `ggen update` | `ggen market update` | Exact replacement |
| `ggen categories` | `ggen market categories` | Exact replacement |
| `ggen gen` | `ggen project gen` | Exact replacement |
| `ggen list` | `ggen template list` | Exact replacement |
| `ggen show` | `ggen template show` | Exact replacement |
| `ggen lint` | `ggen template lint` | Exact replacement |

---

## New Commands: Detailed Specifications

### 1. `project test` Verb (Pattern 009)

**Purpose**: Validate template/plan without executing

**Signature**:
```rust
// cli/src/cmds/project/test.rs
#[derive(Args, Debug)]
pub struct TestArgs {
    /// Template or plan file to test
    #[arg(value_name = "TEMPLATE_OR_PLAN")]
    pub input: String,

    /// Variables to pass to template
    #[arg(long, value_name = "KEY=VALUE")]
    pub vars: Vec<String>,

    /// Fail on warnings
    #[arg(long)]
    pub strict: bool,

    /// Output format (text, json, yaml)
    #[arg(long, default_value = "text")]
    pub format: String,
}

pub async fn run(args: &TestArgs) -> Result<()> {
    // 1. Load template/plan
    // 2. Validate syntax
    // 3. Check frontmatter
    // 4. Verify variables
    // 5. Test rendering (dry-run)
    // 6. Report results
}
```

**Examples**:
```bash
ggen project test "template.tmpl" --vars name=myapp
ggen project test plan.json --strict
ggen project test "template.tmpl" --format json > test-results.json
```

---

### 2. `project inject` Verb (Pattern 091)

**Purpose**: Idempotently inject code into existing files

**Signature**:
```rust
// cli/src/cmds/project/inject.rs
#[derive(Args, Debug)]
pub struct InjectArgs {
    /// Injection template
    #[arg(value_name = "TEMPLATE")]
    pub template: String,

    /// Target file to inject into
    #[arg(long, value_name = "FILE")]
    pub target: String,

    /// Anchor comment to inject at
    #[arg(long, value_name = "ANCHOR")]
    pub anchor: String,

    /// Injection strategy (append-once, prepend-once, replace-once)
    #[arg(long, default_value = "append-once")]
    pub strategy: String,

    /// Idempotency key pattern
    #[arg(long, value_name = "KEY")]
    pub key: String,

    /// Sort injected content (for imports)
    #[arg(long)]
    pub sort: bool,

    /// Dry run
    #[arg(long)]
    pub dry_run: bool,
}

pub async fn run(args: &InjectArgs) -> Result<()> {
    // 1. Load injection template
    // 2. Find anchor in target file
    // 3. Check idempotency key
    // 4. Apply injection strategy
    // 5. Optional: sort content
    // 6. Write back to file
}
```

**Examples**:
```bash
ggen project inject "add_method.tmpl" --target "src/service.rs" --anchor "INJECT_METHODS_HERE" --key "method:{{method_name}}"
ggen project inject "imports.tmpl" --target "src/main.rs" --anchor "INJECT_IMPORTS_HERE" --strategy prepend-once --sort
```

---

### 3. `project freeze` Verb (Pattern 015)

**Purpose**: Manage freeze blocks for safe regeneration

**Signature**:
```rust
// cli/src/cmds/project/freeze.rs
#[derive(Args, Debug)]
pub struct FreezeArgs {
    /// File or directory to freeze
    #[arg(value_name = "PATH")]
    pub path: String,

    /// Freeze block names (comma-separated)
    #[arg(long, value_name = "BLOCKS")]
    pub blocks: Option<String>,

    /// Auto-detect freeze blocks
    #[arg(long)]
    pub auto_detect: bool,

    /// List all freeze blocks
    #[arg(long)]
    pub list: bool,

    /// Add freeze blocks to file
    #[arg(long)]
    pub add: bool,
}

pub async fn run(args: &FreezeArgs) -> Result<()> {
    // 1. Parse file(s)
    // 2. Detect freeze blocks
    // 3. Add/list blocks
    // 4. Validate freeze markers
}
```

**Examples**:
```bash
ggen project freeze "src/models/user.rs" --list
ggen project freeze "src/services/" --auto-detect
ggen project freeze "src/main.rs" --add --blocks "custom_methods,imports"
```

---

### 4. `hook` Noun (Pattern 021)

**Purpose**: Manage Git hooks for automatic graph regeneration

**Signature**:
```rust
// cli/src/cmds/hook/mod.rs
#[derive(Args, Debug)]
pub struct HookCmd {
    #[command(subcommand)]
    pub verb: HookVerb,
}

#[derive(Subcommand, Debug)]
pub enum HookVerb {
    /// Install Git hooks
    Install(InstallArgs),
    /// Uninstall Git hooks
    Uninstall,
    /// Show hook status
    Status,
    /// List available hooks
    List,
    /// Manually run a hook
    Run(RunArgs),
    /// Configure hook behavior
    Configure(ConfigureArgs),
}

#[derive(Args, Debug)]
pub struct InstallArgs {
    /// Install all hooks
    #[arg(long)]
    pub all: bool,

    /// Specific hook to install
    #[arg(value_name = "HOOK")]
    pub hook: Option<String>,
}

#[derive(Args, Debug)]
pub struct RunArgs {
    /// Hook to run
    #[arg(value_name = "HOOK")]
    pub hook: String,
}

#[derive(Args, Debug)]
pub struct ConfigureArgs {
    /// Configuration key
    #[arg(value_name = "KEY")]
    pub key: String,

    /// Configuration value
    #[arg(value_name = "VALUE")]
    pub value: String,
}
```

**Examples**:
```bash
ggen hook install --all
ggen hook install pre-commit
ggen hook uninstall
ggen hook status
ggen hook list
ggen hook run pre-commit
ggen hook configure incremental true
```

---

### 5. `template validate` Verb

**Purpose**: Validate template syntax and metadata

**Signature**:
```rust
// cli/src/cmds/template/validate.rs
#[derive(Args, Debug)]
pub struct ValidateArgs {
    /// Template file to validate
    #[arg(value_name = "TEMPLATE")]
    pub template: String,

    /// Fail on warnings
    #[arg(long)]
    pub strict: bool,

    /// Check SPARQL query syntax
    #[arg(long)]
    pub check_queries: bool,

    /// Output format
    #[arg(long, default_value = "text")]
    pub format: String,
}

pub async fn run(args: &ValidateArgs) -> Result<()> {
    // 1. Parse template
    // 2. Validate frontmatter
    // 3. Check SPARQL queries (if --check-queries)
    // 4. Validate template syntax
    // 5. Report results
}
```

**Examples**:
```bash
ggen template validate "template.tmpl"
ggen template validate "template.tmpl" --strict --check-queries
ggen template validate "template.tmpl" --format json
```

---

### 6. `ci drift-check` Verb (Pattern 012)

**Purpose**: Detect configuration drift in CI/CD

**Signature**:
```rust
// cli/src/cmds/ci/drift_check.rs
#[derive(Args, Debug)]
pub struct DriftCheckArgs {
    /// Auto-fix detected drift
    #[arg(long)]
    pub fix: bool,

    /// Generate drift report
    #[arg(long)]
    pub report: bool,

    /// Output format
    #[arg(long, default_value = "text")]
    pub format: String,
}

pub async fn run(args: &DriftCheckArgs) -> Result<()> {
    // 1. Load expected configuration
    // 2. Compare with actual state
    // 3. Detect drift
    // 4. Optional: fix drift
    // 5. Optional: generate report
}
```

**Examples**:
```bash
ggen ci drift-check
ggen ci drift-check --fix
ggen ci drift-check --report --format json > drift-report.json
```

---

## Implementation Priority

### Phase 1: Foundation (v0.2.5 - 1 month)
**Goal**: Remove legacy commands, add critical missing verbs

1. ✅ Add deprecation warnings to all legacy commands
2. ✅ Update documentation with migration guides
3. ✅ Implement `project test` verb
4. ✅ Implement `hook` noun with all verbs
5. ✅ Add `template validate` verb

**Deliverables**:
- Migration guide (MIGRATION.md)
- Deprecated legacy commands
- 3 new commands implemented

---

### Phase 2: Pattern Completion (v0.2.6 - 1 month)
**Goal**: Implement remaining pattern-required commands

1. ✅ Implement `project inject` verb
2. ✅ Implement `project freeze` verb
3. ✅ Implement `ci drift-check` verb
4. ✅ Add alias support
5. ✅ Update cookbook with new commands

**Deliverables**:
- 3 new pattern commands
- Alias system
- Updated cookbook documentation

---

### Phase 3: Legacy Removal (v0.3.0 - 1 month)
**Goal**: Remove all legacy commands (breaking release)

1. ✅ Delete legacy command modules
2. ✅ Remove from Commands enum
3. ✅ Update all tests
4. ✅ Final documentation update
5. ✅ Release notes

**Deliverables**:
- 100% noun-verb CLI
- Clean codebase
- v0.3.0 release

---

### Phase 4: Polish (v0.3.1 - 2 weeks)
**Goal**: Improve UX and consistency

1. ✅ Standardize error messages
2. ✅ Add comprehensive examples
3. ✅ Improve shell completion
4. ✅ Add command suggestions (did you mean?)
5. ✅ Performance optimizations

**Deliverables**:
- Polished UX
- Complete documentation
- Fast, consistent CLI

---

## Breaking Changes Documentation

### v0.3.0 Breaking Changes

**Removed Commands**:
```bash
# These commands are REMOVED in v0.3.0:
ggen search        # Use: ggen market search
ggen add           # Use: ggen market add
ggen remove        # Use: ggen market remove
ggen packs         # Use: ggen market list
ggen update        # Use: ggen market update
ggen categories    # Use: ggen market categories
ggen gen           # Use: ggen project gen
ggen list          # Use: ggen template list
ggen show          # Use: ggen template show
ggen lint          # Use: ggen template lint
```

**Migration Script**:
```bash
#!/bin/bash
# scripts/migrate-to-v0.3.0.sh
# Automated migration helper

echo "Migrating ggen commands to v0.3.0..."

# Create aliases
ggen alias add search "market search"
ggen alias add add "market add"
ggen alias add remove "market remove"
ggen alias add packs "market list"
ggen alias add update "market update"
ggen alias add categories "market categories"
ggen alias add gen "project gen"
ggen alias add list "template list"
ggen alias add show "template show"
ggen alias add lint "template lint"

echo "✅ Aliases created. Run 'ggen alias list' to verify."
echo "   These aliases will work until you're ready to update your scripts."
```

---

## Anti-Patterns to Avoid

### ❌ Don't: Mix verb-noun and noun-verb
```bash
# Bad
ggen apply template user.tmpl
```

### ✅ Do: Consistent noun-verb
```bash
# Good
ggen project apply plan.json
ggen template apply user.tmpl  # If we add this verb
```

---

### ❌ Don't: Inconsistent verb semantics
```bash
# Bad: "list" means different things
ggen template list      # Lists templates
ggen graph list         # Should this list graphs? (confusing)
```

### ✅ Do: Use verbs consistently
```bash
# Good: "list" always lists instances of the noun
ggen template list      # Lists templates
ggen market list        # Lists installed packages
ggen hook list          # Lists available hooks
```

---

### ❌ Don't: Deeply nested commands
```bash
# Bad
ggen project template apply user.tmpl
```

### ✅ Do: Keep it flat
```bash
# Good
ggen project gen user.tmpl
```

---

## Testing Strategy

### Unit Tests
```rust
// cli/src/cmds/project/test.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_project_test_valid_template() {
        let args = TestArgs {
            input: "tests/fixtures/valid.tmpl".to_string(),
            vars: vec!["name=test".to_string()],
            strict: false,
            format: "text".to_string(),
        };
        assert!(run(&args).await.is_ok());
    }

    #[tokio::test]
    async fn test_project_test_invalid_template() {
        let args = TestArgs {
            input: "tests/fixtures/invalid.tmpl".to_string(),
            vars: vec![],
            strict: true,
            format: "text".to_string(),
        };
        assert!(run(&args).await.is_err());
    }
}
```

### Integration Tests
```rust
// tests/cli_integration.rs
#[tokio::test]
async fn test_full_workflow() {
    // 1. Create template
    Command::new("ggen")
        .args(&["template", "new", "test.tmpl"])
        .assert()
        .success();

    // 2. Validate template
    Command::new("ggen")
        .args(&["template", "validate", "test.tmpl"])
        .assert()
        .success();

    // 3. Test template
    Command::new("ggen")
        .args(&["project", "test", "test.tmpl", "--vars", "name=test"])
        .assert()
        .success();

    // 4. Generate plan
    Command::new("ggen")
        .args(&["project", "plan", "test.tmpl", "--vars", "name=test"])
        .assert()
        .success();

    // 5. Apply plan
    Command::new("ggen")
        .args(&["project", "apply", "plan.json"])
        .assert()
        .success();
}
```

### BDD Tests
```gherkin
# tests/bdd/features/noun_verb_cli.feature
Feature: Noun-Verb CLI Structure
  As a developer
  I want a consistent noun-verb CLI
  So that I can easily discover and use commands

  Scenario: All commands follow noun-verb pattern
    When I run "ggen --help"
    Then I should see noun-verb commands
    And I should not see legacy flat commands

  Scenario: Verb consistency across nouns
    Given I have the following nouns: project, template, market, graph
    When I check available verbs
    Then "list" should have consistent semantics across nouns
    And "show" should have consistent semantics across nouns

  Scenario: Legacy command deprecation
    When I run "ggen search"
    Then I should see a deprecation warning
    And I should see the replacement command "ggen market search"
```

---

## Success Metrics

### Before Refactoring (v0.2.4)
- Legacy commands: 10
- Noun-verb commands: 7 nouns
- Pattern coverage: 60%
- CLI consistency score: 6/10

### After Refactoring (v0.3.0)
- Legacy commands: 0 ✅
- Noun-verb commands: 8 nouns ✅
- Pattern coverage: 95% ✅
- CLI consistency score: 9/10 ✅

### KPIs
1. **Command Migration Rate**: 100% of legacy commands migrated
2. **Pattern Coverage**: 95%+ of cookbook patterns have CLI support
3. **User Satisfaction**: <5% migration complaints
4. **Documentation Quality**: 100% of new commands documented
5. **Test Coverage**: >85% for all new commands

---

## Timeline

### Sprint 1 (Weeks 1-2): v0.2.5
- Add deprecation warnings
- Update documentation
- Implement `project test`
- Implement `hook` noun

### Sprint 2 (Weeks 3-4): v0.2.6
- Implement `project inject`
- Implement `project freeze`
- Implement `ci drift-check`
- Add alias support

### Sprint 3 (Weeks 5-6): v0.3.0
- Remove legacy commands
- Update all tests
- Final documentation
- Release v0.3.0

### Sprint 4 (Week 7): v0.3.1
- Polish UX
- Performance optimizations
- Bug fixes
- Community feedback

**Total Duration**: 7 weeks

---

## Risk Mitigation

### Risk 1: User Backlash
**Mitigation**:
- Long deprecation period (2 releases)
- Clear migration guides
- Alias support
- Helpful error messages

### Risk 2: Breaking CI/CD Pipelines
**Mitigation**:
- Detailed migration guide for CI
- Example GitHub Actions
- Automated migration script
- Version pinning recommendations

### Risk 3: Incomplete Testing
**Mitigation**:
- Comprehensive test suite
- BDD tests for all patterns
- Manual testing checklist
- Beta testing period

### Risk 4: Documentation Lag
**Mitigation**:
- Update docs in parallel with code
- Auto-generate command help
- Example-driven documentation
- Community review

---

## Appendix A: Complete Command Reference

### Current Commands (v0.2.4)

**Noun: `project`**
- `gen` - Generate from template
- `plan` - Create generation plan
- `apply` - Apply plan
- `diff` - Show differences

**Noun: `template`**
- `new` - Create new template
- `list` - List templates
- `show` - Show template details
- `lint` - Lint template
- `regenerate` - Regenerate with delta

**Noun: `market`**
- `search` - Search packages
- `add` - Add package
- `remove` - Remove package
- `list` - List installed packages
- `update` - Update packages
- `info` - Show package info
- `categories` - Show categories
- `publish` - Publish package
- `unpublish` - Unpublish package
- `recommend` - Get recommendations

**Noun: `graph`**
- `load` - Load RDF data
- `query` - Execute SPARQL
- `export` - Export graph
- `validate` - Validate with SHACL
- `stats` - Show statistics
- `diff` - Compare graphs
- `snapshot` - Manage snapshots

**Noun: `audit`**
- `hazard` - Generate hazard report
- `security` - Security audit
- `performance` - Performance audit

**Noun: `ci`**
- `pages` - Deploy to GitHub Pages
- `workflow` - Manage workflows
- `trigger` - Trigger workflows
- `release` - Create release

**Noun: `shell`**
- `init` - Initialize shell
- `completion` - Generate completions

---

### New Commands (v0.3.0)

**Noun: `project`** (additions)
- `test` - Test template/plan ✨ NEW
- `inject` - Inject code ✨ NEW
- `freeze` - Manage freeze blocks ✨ NEW

**Noun: `template`** (additions)
- `validate` - Validate template ✨ NEW

**Noun: `ci`** (additions)
- `drift-check` - Check configuration drift ✨ NEW

**Noun: `hook`** ✨ NEW
- `install` - Install Git hooks
- `uninstall` - Uninstall hooks
- `status` - Show hook status
- `list` - List available hooks
- `run` - Manually run hook
- `configure` - Configure hooks

**Noun: `alias`** ✨ NEW (optional)
- `add` - Add alias
- `remove` - Remove alias
- `list` - List aliases

---

## Appendix B: Pattern Implementation Checklist

- [x] Pattern 001: KNOWLEDGE-FIRST PROJECTION - `graph` noun
- [x] Pattern 002: DETERMINISTIC ENGINE - Global flags
- [x] Pattern 003: GRAPH-TEMPLATE BINDING - Frontmatter
- [x] Pattern 004: NOUN-VERB CLI - Complete restructure
- [x] Pattern 009: PROJECT PLAN - `project plan/test`
- [x] Pattern 010: IDEMPOTENT APPLY - `project apply`
- [x] Pattern 011: DRY-RUN DIFF - `project diff`
- [ ] Pattern 012: CI DRIFT CHECK - `ci drift-check` ❌ MISSING
- [ ] Pattern 015: IMMUTABILITY FIRST - `project freeze` ❌ MISSING
- [x] Pattern 022: DELTA-DRIVEN - `template regenerate`
- [ ] Pattern 021: KNOWLEDGE HOOKS - `hook` noun ❌ MISSING
- [ ] Pattern 091: IDEMPOTENT INJECTION - `project inject` ❌ MISSING

**Coverage**: 8/12 = 67% → Target: 95%+

---

## Conclusion

This refactoring plan provides a systematic approach to aligning the GGen CLI with the **GGen Cookbook 2nd Edition** pattern language. By following the 80/20 principle and prioritizing high-impact changes, we can achieve complete pattern coverage while maintaining backward compatibility during migration.

**Next Steps**:
1. Review this plan with stakeholders
2. Create GitHub issues for each new command
3. Begin Phase 1 implementation
4. Gather community feedback
5. Iterate based on results

---

**Document Status**: Draft for Review
**Reviewers Needed**: Core team, community maintainers
**Target Approval Date**: 2025-10-16
**Implementation Start**: 2025-10-23
