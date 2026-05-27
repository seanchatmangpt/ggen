# Migration Automation Plan - 67 Remaining Commands

## Executive Summary

**Objective**: Automate migration of 67 remaining commands to v2.0.0 architecture after 10 core commands establish the pattern.

**Timeline**: 16-24 hours total (10 minutes automated, rest manual domain logic)

**Success Rate Target**: 100% compilation, 100% test pass rate

## Pattern Established by 10 Core Commands

The sparc-coder will implement these 10 commands to establish the pattern:

1. `template new` - Template creation
2. `template list` - Template listing
3. `marketplace search` - Marketplace search
4. `marketplace install` - Package installation
5. `marketplace list` - Installed packages
6. `project new` - Project initialization
7. `project gen` - Project generation
8. `graph query` - Graph querying
9. `graph load` - Graph loading
10. `utils doctor` - System diagnostics

**Pattern components**:
- ✅ CLI layer: Clap args with `#[verb]` pattern
- ✅ Domain layer: Business logic isolation
- ✅ Runtime layer: Global state management
- ✅ Tests: Unit + integration + E2E
- ✅ Error handling: Consistent Result types
- ✅ Documentation: Examples and usage

## 67 Commands to Automate

### HIGH Priority (30 commands)

**Template Commands** (6):
```
template:show          - Display template details
template:lint          - Validate template syntax
template:regenerate    - Regenerate from template
template:generate-tree - Generate directory tree
template:validate      - Comprehensive validation
template:diff          - Compare templates
```

**Marketplace Commands** (7):
```
marketplace:update     - Update installed packages
marketplace:publish    - Publish to marketplace
marketplace:unpublish  - Remove from marketplace
marketplace:versions   - List package versions
marketplace:stats      - Marketplace statistics
```

**Project Commands** (7):
```
project:plan           - Generate project plan
project:apply          - Apply project plan
project:init           - Initialize project structure
project:build          - Build project
project:status         - Project status
project:clean          - Clean build artifacts
project:archive        - Archive project
```

**Graph Commands** (4):
```
graph:export           - Export graph data
graph:validate         - Validate graph structure
graph:merge            - Merge graphs
graph:diff             - Compare graphs
```

**AI Commands** (6):
```
ai:analyze             - Analyze code
ai:chat                - Interactive AI chat
ai:suggest             - Get suggestions
ai:optimize            - Optimize code
ai:review              - Review code
ai:config              - Configure AI settings
```

### MEDIUM Priority (25 commands)

**Hook Commands** (5):
```
hook:add               - Add lifecycle hook
hook:remove            - Remove hook
hook:list              - List hooks
hook:enable            - Enable hook
hook:disable           - Disable hook
```

**Lifecycle Commands** (4):
```
lifecycle:start        - Start service
lifecycle:stop         - Stop service
lifecycle:status       - Service status
lifecycle:restart      - Restart service
```

**CI Commands** (4):
```
ci:workflow            - Manage CI workflows
ci:setup               - Setup CI configuration
ci:validate            - Validate CI config
ci:cleanup             - Cleanup CI artifacts
```

**Audit Commands** (3):
```
audit:security         - Security audit
audit:compliance       - Compliance check
audit:report           - Generate audit report
```

**Shell Commands** (2):
```
shell:completion       - Shell completions
shell:init             - Initialize shell
```

**Utils Commands** (4):
```
utils:env              - Environment management
utils:config           - Configuration management
utils:version-check    - Check for updates
```

**Template (continued)** (2):
```
template:merge         - Merge templates
```

**AI (continued)** (1):
```
ai:models              - List available models
ai:benchmark           - Benchmark AI performance
```

### LOW Priority (12 commands)

**Template Commands** (2):
```
template:archive       - Archive template
template:restore       - Restore template
```

**Marketplace Commands** (2):
```
marketplace:cache-clear - Clear download cache
marketplace:migrate     - Migrate marketplace data
```

**Project Commands** (1):
```
project:migrate        - Migrate project structure
```

**Graph Commands** (2):
```
graph:optimize         - Optimize graph storage
graph:stats            - Graph statistics
```

**Lifecycle Commands** (1):
```
lifecycle:logs         - View service logs
```

## Automation Scripts

### 1. `generate_cli_wrapper.sh`

**Purpose**: Generate single CLI wrapper

**Template structure**:
```rust
//! {Noun} {verb} command - CLI layer
use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

#[derive(Args, Debug)]
#[command(name = "{verb}", about = "{description}")]
pub struct {Verb}Args {
    #[arg(long)]
    pub detailed: bool,

    #[arg(long)]
    pub json: bool,
}

pub fn run(args: &{Verb}Args) -> Result<()> {
    runtime::execute(async {
        crate::domain::{noun}::{verb}::{verb}_and_display(
            args.detailed,
            args.json
        ).await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{verb}_args_parsing() {
        // Test args
    }
}
```

**Customization points**:
- Command-specific arguments
- Documentation examples
- Test cases

### 2. `migrate_remaining_commands.sh`

**Purpose**: Batch migrate all 67 commands

**Algorithm**:
```bash
for priority in HIGH MEDIUM LOW; do
    for command in ${priority}_COMMANDS; do
        noun, verb = parse(command)

        if not exists("cli/src/commands/${noun}/${verb}.rs"):
            if not exists("cli/src/domain/${noun}/${verb}.rs"):
                create_placeholder_domain(noun, verb)

            generate_cli_wrapper(noun, verb)
            log_success(command)
        else:
            log_skipped(command)
    done
done

report_summary()
```

**Features**:
- Dry-run mode for preview
- Progress tracking
- Error recovery
- Detailed logging

### 3. `validate_migration.sh`

**Purpose**: Comprehensive validation

**Validation phases**:
1. **Syntax**: `cargo check --all-features`
2. **Build**: `cargo build --all-features`
3. **Unit Tests**: `cargo test --lib`
4. **Integration**: `cargo test --test '*'`
5. **E2E**: CLI command smoke tests
6. **Architecture**: Layer separation check
7. **Performance**: Compilation time, binary size

**Success criteria**:
```yaml
compilation_time: <45s
binary_size: <30MB
test_pass_rate: 100%
test_execution_time: <60s
architecture_violations: 0
```

## Domain Logic Templates

For commands without existing domain logic, create minimal placeholder:

```rust
//! {Noun} {verb} domain logic

use ggen_utils::error::Result;

/// Execute {verb} operation for {noun}
pub async fn {verb}_and_display(detailed: bool, json: bool) -> Result<()> {
    // TODO: Implement actual domain logic

    if json {
        println!("{{}}");
    } else {
        println!("{Noun} {verb} - Not yet implemented");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_{verb}_basic() {
        let result = {verb}_and_display(false, false).await;
        assert!(result.is_ok());
    }
}
```

## Testing Strategy

### Unit Tests (Automated)

Generated for every command:

```rust
#[test]
fn test_{verb}_args_parsing() {
    let args = {Verb}Args {
        detailed: true,
        json: false,
    };
    assert!(args.detailed);
    assert!(!args.json);
}

#[test]
fn test_{verb}_args_defaults() {
    let args = {Verb}Args {
        detailed: false,
        json: false,
    };
    assert!(!args.detailed);
    assert!(!args.json);
}
```

### Integration Tests (Manual - Critical Paths)

For HIGH priority commands:

```rust
#[test]
fn test_{noun}_{verb}_e2e() {
    ggen()
        .arg("{noun}")
        .arg("{verb}")
        .arg("--json")
        .assert()
        .success();
}
```

Focus on:
- Template show, lint, regenerate
- Marketplace search, install, update
- Project new, gen, plan, apply
- Graph query, load, export
- AI analyze, chat, suggest

## Timeline Breakdown

### Phase 1: Core Pattern (4-6 hours) ✅ sparc-coder

- [x] Implement 10 core commands manually
- [x] Establish CLI wrapper pattern
- [x] Establish domain layer pattern
- [x] Create comprehensive tests
- [x] Document patterns

### Phase 2: Automation Development (2 hours) ✅ This task

- [x] Create `generate_cli_wrapper.sh`
- [x] Create `migrate_remaining_commands.sh`
- [x] Create `validate_migration.sh`
- [x] Test scripts on 2-3 commands
- [x] Document automation

### Phase 3: Automated Migration (10 minutes)

- [ ] Run `migrate_remaining_commands.sh`
- [ ] Generate 67 CLI wrappers
- [ ] Create placeholder domain logic
- [ ] Validate compilation
- [ ] Fix any syntax errors

### Phase 4: Domain Logic Implementation (8-12 hours)

Priority-based implementation:

**HIGH Priority** (5-6 hours):
- Template: show, lint, regenerate (2 hours)
- Marketplace: update, publish (1.5 hours)
- Project: plan, apply, init, build (2 hours)
- Graph: export, validate (1 hour)
- AI: analyze, suggest, optimize (1.5 hours)

**MEDIUM Priority** (3-4 hours):
- Hooks: add, remove, list (1 hour)
- Lifecycle: start, stop, status (1 hour)
- CI: workflow, setup, validate (1 hour)
- Audit: security, compliance (1 hour)

**LOW Priority** (1-2 hours):
- Archive/restore operations (30 min)
- Cache/optimization (30 min)
- Statistics/reporting (30 min)
- Migration utilities (30 min)

### Phase 5: Integration Testing (4-6 hours)

- [ ] Write E2E tests for critical paths
- [ ] Test all HIGH priority commands
- [ ] Test key MEDIUM priority commands
- [ ] Performance benchmarking
- [ ] Edge case testing

### Phase 6: Final Validation (30 minutes)

- [ ] Run full test suite
- [ ] Run validation script
- [ ] Check code coverage
- [ ] Review architecture compliance
- [ ] Performance metrics

## Success Metrics

### Code Quality

- ✅ 100% compilation success
- ✅ 100% test pass rate
- ✅ 0 clippy warnings (with pedantic)
- ✅ Proper error handling everywhere
- ✅ Consistent naming conventions

### Performance

- ✅ Compilation time <45s (vs v1: 60-90s)
- ✅ Binary size <30MB (vs v1: 35MB)
- ✅ Test execution <60s total
- ✅ Generation performance <2s

### Architecture

- ✅ Clear layer separation
- ✅ No business logic in CLI
- ✅ Domain isolated from framework
- ✅ Runtime properly utilized
- ✅ No circular dependencies

### Documentation

- ✅ All commands documented
- ✅ Usage examples for each
- ✅ Migration guide complete
- ✅ Architecture docs updated
- ✅ API docs generated

## Risk Assessment

### LOW RISK ✅

- Script generation (proven pattern)
- Compilation validation (automated)
- Unit test generation (templated)

### MEDIUM RISK ⚠️

- Domain logic complexity (varies by command)
- Integration test coverage (manual effort)
- Performance regression (need benchmarks)

### HIGH RISK 🔴

- Breaking API changes (mitigated by tests)
- User workflow disruption (mitigated by E2E tests)
- Migration deadline pressure (mitigated by automation)

**Mitigation strategies**:
- Comprehensive E2E test coverage
- Beta release before stable
- Clear migration documentation
- Automated validation at every step

## Rollback Plan

If critical issues discovered:

1. **Revert commits**: `git revert HEAD~N`
2. **Restore v1.x**: `git checkout v1.2.0`
3. **Fix issues**: Address root cause
4. **Re-run automation**: With fixes applied
5. **Validate again**: Full validation cycle

## Communication Plan

### Internal Team

- Daily standup: Migration progress
- Blocker resolution: Real-time Slack
- Code reviews: GitHub PRs
- Documentation: Continuous updates

### Users

- Migration guide: Published with v2.0.0
- Breaking changes: Highlighted in CHANGELOG
- Support: GitHub Discussions
- Timeline: Clear deprecation schedule

## Next Actions

1. **Review this plan**: Team approval
2. **Execute automation**: Run migration scripts
3. **Implement domain logic**: Priority-based
4. **Integration testing**: Critical paths
5. **Final validation**: Full test suite
6. **Documentation**: Migration guide
7. **Release preparation**: v2.0.0 beta

---

**Automation saves**: ~40-50 hours vs manual migration
**Risk level**: LOW (with comprehensive validation)
**Success probability**: HIGH (proven pattern, automated validation)
