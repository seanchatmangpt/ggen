# ggen v2 Migration Phases: Command Rollout Plan

**Created**: 2025-11-01
**Status**: DRAFT
**Current State**: 36 commands, 39 domain files
**Target**: Full v2 migration with 67+ total commands

---

## Executive Summary

**Current Blockers** (from Agent 12 validation):
- ❌ Build fails (8 compilation errors)
- ❌ Security vulnerability (RUSTSEC-2025-0111)
- ❌ Marketplace commands are stubs (0/5 working)

**Migration Strategy**: Phased rollout with 80/20 focus
- **v2.0.0 MVP**: Fix blockers + core commands (20 commands)
- **v2.1.0**: AI + Graph + Shell features (25 commands)
- **v2.2.0**: Complete migration + deprecate v1 (67+ commands)

**Timeline**:
- v2.0.0 MVP: 2 weeks (fix blockers + stabilize)
- v2.1.0: 4 weeks (major features)
- v2.2.0: 6 weeks (complete migration)

---

## Phase Analysis: Critical Path

### Current State Audit (2025-11-01)

**Commands Layer** (`cli/src/commands/`):
```
✅ Template (7 files): generate, list, show, lint, generate_tree, mod
✅ Marketplace (5 files): search, install, publish, list, update
✅ Project (3 files): init, build, mod
✅ Graph (5 files): load, query, visualize, export, mod
✅ AI (2 files): gen, mod
✅ CI (2 files): validate, mod
✅ Hook (2 files): apply, mod
✅ Utils (2 files): doctor, mod
```

**Domain Layer** (`cli/src/domain/`):
```
✅ Template (9 files): generate, list, show, lint, generate_tree, regenerate, etc.
✅ Marketplace (6 files): search, install, publish, list, update, mod
✅ Project (3 files): init, build, mod
✅ Graph (5 files): load, query, visualize, export, mod
✅ AI (4 files): gen, analyze, plan, mod
✅ Audit (1 file): security
✅ Utils (4 files): doctor, env, completion, mod
```

**Key Findings**:
1. **36 commands exist** (commands + domain layers)
2. **0 TODO/FIXME markers** (clean implementation)
3. **Marketplace is stubs** (critical blocker)
4. **Missing module files** (ai.rs, project.rs, utils.rs in commands/)

---

## v2.0.0 MVP: Production-Ready Foundation

**Goal**: Ship working, secure, stable foundation
**Timeline**: 2 weeks
**Focus**: Fix blockers + stabilize core (80/20)

### Critical Blockers (MUST FIX)

#### 1. Build Failures (2 hours)
**Root Cause**: Missing module files in `cli/src/commands/`

**Fix**:
```rust
// Missing files to create:
cli/src/commands/ai.rs       // Re-export ai::*
cli/src/commands/project.rs  // Re-export project::*
cli/src/commands/utils.rs    // Re-export utils::*
```

**Test**: `cargo build --release` succeeds

#### 2. Security Vulnerability (4 hours)
**Root Cause**: `tokio-tar 0.3.1` via `testcontainers`

**Fix Options**:
1. **Workaround**: Disable tar feature in testcontainers
2. **Fork**: Vendor patched tokio-tar
3. **Replace**: Use alternative tar library

**Test**: `cargo audit` shows 0 critical vulnerabilities

#### 3. Marketplace Stubs (6 hours)
**Root Cause**: Agent 1-5 created empty stubs

**Fix**: Implement minimal working versions
```rust
// v2.0.0 MVP implementations:
marketplace::search()   // Query registry API
marketplace::install()  // Download + validate template
marketplace::list()     // Show installed templates
marketplace::publish()  // Upload to registry (auth required)
marketplace::update()   // Update installed template
```

**Test**: All 5 commands work end-to-end

### v2.0.0 MVP Command Set (20 commands)

**Core Template Commands** (7 - WORKING):
- `ggen generate <template>` - Generate from template
- `ggen template list` - List available templates
- `ggen template show <name>` - Show template details
- `ggen template lint <file>` - Validate template syntax
- `ggen template generate-tree` - Create template tree
- `ggen template regenerate` - Regenerate with new data
- `ggen template new` - Create new template

**Core Marketplace Commands** (5 - NEEDS FIX):
- `ggen marketplace search <query>` - Search registry
- `ggen marketplace install <name>` - Install template
- `ggen marketplace list` - List installed
- `ggen marketplace publish` - Publish template
- `ggen marketplace update <name>` - Update template

**Core Project Commands** (3 - WORKING):
- `ggen project init` - Initialize project
- `ggen project build` - Build project
- `ggen project new` - Create new project

**Core Utils Commands** (5 - WORKING):
- `ggen doctor` - Health check
- `ggen completion <shell>` - Shell completions
- `ggen env` - Show environment
- `ggen hook apply` - Apply Git hooks
- `ggen ci validate` - CI validation

**Total**: 20 commands (15 working + 5 to fix)

### v2.0.0 MVP Acceptance Criteria

**Build & Security**:
- ✅ `cargo build --release` succeeds
- ✅ `cargo audit` shows 0 critical vulnerabilities
- ✅ `cargo test --workspace` passes 80%+
- ✅ Binary size < 20MB

**Functionality**:
- ✅ All 20 commands work end-to-end
- ✅ Marketplace search/install/list work
- ✅ Template generation works
- ✅ Project init/build work

**Quality**:
- ✅ 0 clippy errors
- ✅ Documentation updated
- ✅ Migration guide accurate
- ✅ Release notes complete

**Performance**:
- ✅ Startup < 100ms
- ✅ Generation < 2s
- ✅ Compilation < 45s

---

## v2.1.0: AI, Graph, and Shell Features

**Goal**: Add advanced features for power users
**Timeline**: 4 weeks
**Focus**: AI code generation, graph workflows, shell integration

### AI Commands (8 new commands)

**AI Generation** (4 commands):
- `ggen ai gen <prompt>` - Generate code from prompt
- `ggen ai analyze <file>` - Analyze code quality
- `ggen ai plan <feature>` - Plan implementation
- `ggen ai refactor <file>` - Suggest refactorings

**AI Templates** (4 commands):
- `ggen ai template create <prompt>` - Create template from prompt
- `ggen ai template optimize <file>` - Optimize template
- `ggen ai template validate <file>` - AI-powered validation
- `ggen ai template suggest` - Suggest improvements

**Implementation**:
- Domain: `cli/src/domain/ai/` (4 files exist)
- Commands: `cli/src/commands/ai/` (2 files exist, add 6 more)
- Runtime: Use OpenAI/Anthropic APIs
- Caching: Local cache for prompts

**Dependencies**:
```toml
[dependencies]
openai-api-rs = "5.0"        # OpenAI integration
anthropic-sdk = "0.3"         # Claude integration
langchain-rust = "0.2"        # Chain workflows
```

### Graph Commands (9 new commands)

**Graph Operations** (5 commands):
- `ggen graph load <file>` - Load graph from file
- `ggen graph query <cypher>` - Query graph (Cypher syntax)
- `ggen graph visualize` - Visualize graph (GraphViz)
- `ggen graph export <format>` - Export (JSON/GraphML/DOT)
- `ggen graph merge <graphs...>` - Merge multiple graphs

**Graph Workflows** (4 commands):
- `ggen graph workflow create` - Create workflow
- `ggen graph workflow run <name>` - Execute workflow
- `ggen graph workflow list` - List workflows
- `ggen graph workflow validate` - Validate workflow

**Implementation**:
- Domain: `cli/src/domain/graph/` (5 files exist)
- Commands: `cli/src/commands/graph/` (5 files exist, add 4 more)
- Backend: Neo4j or in-memory graph
- Format: RDF + custom extensions

**Dependencies**:
```toml
[dependencies]
petgraph = "0.6"             # Graph algorithms
graphviz-rust = "0.7"        # Visualization
neo4rs = "0.7"               # Neo4j driver (optional)
```

### Shell Commands (8 new commands)

**Shell Integration** (4 commands):
- `ggen shell init` - Initialize shell integration
- `ggen shell aliases` - Generate shell aliases
- `ggen shell functions` - Generate shell functions
- `ggen shell completions <shell>` - Advanced completions

**Environment Management** (4 commands):
- `ggen env set <key> <value>` - Set environment variable
- `ggen env get <key>` - Get environment variable
- `ggen env list` - List all variables
- `ggen env export <file>` - Export environment

**Implementation**:
- Domain: `cli/src/domain/shell/` (create new)
- Commands: `cli/src/commands/shell/` (create new)
- Shells: bash, zsh, fish, PowerShell
- Config: `~/.config/ggen/shell/`

**Dependencies**:
```toml
[dependencies]
clap_complete = "4.4"        # Shell completions
shellexpand = "3.1"          # Shell expansion
```

### v2.1.0 Total: 25 new commands

**Breakdown**:
- AI: 8 commands
- Graph: 9 commands
- Shell: 8 commands
- **Total v2.1.0**: 45 commands (20 from v2.0.0 + 25 new)

### v2.1.0 Acceptance Criteria

**Functionality**:
- ✅ AI generation works with OpenAI/Anthropic
- ✅ Graph workflows execute correctly
- ✅ Shell integration works on bash/zsh/fish
- ✅ All 45 commands work end-to-end

**Performance**:
- ✅ AI generation < 5s (with caching)
- ✅ Graph queries < 1s (10k nodes)
- ✅ Shell init < 500ms

**Quality**:
- ✅ Test coverage 80%+
- ✅ Documentation complete
- ✅ Examples for all features

---

## v2.2.0: Complete Migration + Feature Parity

**Goal**: 100% v1 feature parity + deprecate v1
**Timeline**: 6 weeks
**Focus**: Remaining commands + v1 compatibility

### Remaining Commands (22+ new commands)

**Advanced Template Features** (5 commands):
- `ggen template validate --schema <file>` - Schema validation
- `ggen template test <file>` - Test template
- `ggen template benchmark <file>` - Benchmark generation
- `ggen template migrate <v1-file>` - Migrate v1 to v2
- `ggen template diff <file1> <file2>` - Compare templates

**Marketplace Advanced** (5 commands):
- `ggen marketplace sync` - Sync local with registry
- `ggen marketplace verify <name>` - Verify integrity
- `ggen marketplace stats` - Usage statistics
- `ggen marketplace author <name>` - Show author info
- `ggen marketplace trending` - Show trending templates

**Project Management** (6 commands):
- `ggen project status` - Show project status
- `ggen project test` - Run project tests
- `ggen project deploy` - Deploy project
- `ggen project watch` - Watch for changes
- `ggen project clean` - Clean build artifacts
- `ggen project dependencies` - Manage dependencies

**CI/CD Integration** (6 commands):
- `ggen ci test` - Run CI tests
- `ggen ci lint` - Run CI linters
- `ggen ci build` - CI build
- `ggen ci deploy` - CI deployment
- `ggen ci rollback` - Rollback deployment
- `ggen ci status` - CI pipeline status

**Total New**: 22 commands

### v1 Compatibility Layer

**Goal**: 100% backward compatibility for v1 users

**Strategy**:
1. **Command Aliases**: Map v1 commands to v2
   ```bash
   ggen market search -> ggen marketplace search
   ggen project gen -> ggen generate
   ```

2. **Config Migration**: Auto-migrate v1 config
   ```rust
   fn migrate_v1_config(old: &Path) -> Result<Config> {
       // Read v1 config
       // Transform to v2 format
       // Backup old config
       // Write new config
   }
   ```

3. **Template Format**: Support v1 templates
   ```rust
   fn detect_template_version(file: &Path) -> Version {
       // Parse template header
       // Return v1 or v2
   }
   ```

4. **Deprecation Warnings**: Warn on v1 usage
   ```bash
   $ ggen market search
   ⚠️  Warning: 'ggen market' is deprecated. Use 'ggen marketplace' instead.
   ```

### v2.2.0 Total: 67+ commands

**Breakdown**:
- v2.0.0 MVP: 20 commands
- v2.1.0 additions: 25 commands
- v2.2.0 additions: 22 commands
- **Total v2.2.0**: 67 commands

### v2.2.0 Acceptance Criteria

**Completeness**:
- ✅ 100% v1 feature parity
- ✅ All 67+ commands working
- ✅ v1 compatibility layer working
- ✅ Migration guide complete

**Quality**:
- ✅ Test coverage 85%+
- ✅ Performance benchmarks met
- ✅ Security audit passed
- ✅ Documentation 100% complete

**Release Readiness**:
- ✅ v1 deprecation announcement
- ✅ Migration tools ready
- ✅ Support documentation ready
- ✅ Community feedback incorporated

---

## Automation Opportunities

### 1. Command Generation Automation

**Template-Based Generation**:
```rust
// Generate boilerplate for new commands
fn generate_command(
    name: &str,
    domain: &str,
    args: &[Arg],
) -> Result<(CommandFile, DomainFile, TestFile)> {
    // Read template
    // Substitute variables
    // Generate 3 files
}
```

**Usage**:
```bash
ggen internal gen-command \
    --name "marketplace verify" \
    --domain marketplace \
    --args "name:String required:true"
```

**Benefits**:
- Consistent structure
- 80% less boilerplate
- Auto-generated tests

### 2. Test Generation Automation

**Auto-Generate Test Suites**:
```rust
fn generate_tests(command: &Command) -> TestSuite {
    // Unit tests (args parsing)
    // Integration tests (end-to-end)
    // Performance tests (benchmarks)
    // Security tests (fuzzing)
}
```

**Benefits**:
- 100% test coverage baseline
- Consistent test structure
- Catch regressions early

### 3. Documentation Generation

**Auto-Generate Docs from Code**:
```rust
fn generate_docs(commands: &[Command]) -> Markdown {
    // Extract doc comments
    // Generate CLI examples
    // Create man pages
    // Build website docs
}
```

**Benefits**:
- Always up-to-date docs
- No manual sync needed
- Auto-generated examples

### 4. CI/CD Automation

**Continuous Integration Checks**:
```yaml
# .github/workflows/v2-migration.yml
name: v2 Migration CI

on: [push, pull_request]

jobs:
  check-completeness:
    runs-on: ubuntu-latest
    steps:
      - name: Count Commands
        run: |
          EXPECTED=67
          ACTUAL=$(find cli/src/commands -name "*.rs" | wc -l)
          if [ $ACTUAL -lt $EXPECTED ]; then
            echo "❌ Only $ACTUAL/$EXPECTED commands implemented"
            exit 1
          fi

      - name: Check Test Coverage
        run: |
          cargo tarpaulin --out Xml
          COVERAGE=$(grep line-rate coverage.xml | head -1 | grep -oP '\d+\.\d+')
          if [ "$COVERAGE" < "0.80" ]; then
            echo "❌ Coverage $COVERAGE < 80%"
            exit 1
          fi
```

---

## Risk Assessment

### High-Risk Areas

#### 1. Marketplace Implementation
**Risk**: API changes break existing workflows
**Mitigation**:
- Versioned API endpoints
- Backward compatibility layer
- Comprehensive integration tests

#### 2. Security Vulnerabilities
**Risk**: New dependencies introduce CVEs
**Mitigation**:
- `cargo audit` in CI
- Minimal dependency tree
- Security review before release

#### 3. Performance Regression
**Risk**: v2 slower than v1
**Mitigation**:
- Continuous benchmarking
- Performance budgets
- Optimization phase before release

#### 4. Breaking Changes
**Risk**: Users cannot migrate easily
**Mitigation**:
- v1 compatibility layer
- Auto-migration tools
- Clear migration guide

### Medium-Risk Areas

#### 1. AI API Costs
**Risk**: OpenAI/Anthropic costs too high
**Mitigation**:
- Local caching
- Rate limiting
- Optional feature (disable by default)

#### 2. Graph Database Dependency
**Risk**: Neo4j adds complexity
**Mitigation**:
- Optional Neo4j support
- In-memory graph default
- Pluggable backends

#### 3. Shell Compatibility
**Risk**: Fish/PowerShell edge cases
**Mitigation**:
- Thorough testing on all shells
- Graceful fallbacks
- Community beta testing

### Low-Risk Areas

#### 1. Documentation
**Risk**: Docs outdated
**Mitigation**: Auto-generated from code

#### 2. Test Coverage
**Risk**: Insufficient tests
**Mitigation**: 80/20 strategy + automation

---

## Success Metrics

### v2.0.0 MVP
| Metric | Target | Measurement |
|--------|--------|-------------|
| Build Success | 100% | `cargo build --release` |
| Test Pass Rate | 80%+ | `cargo test --workspace` |
| Security Vulns | 0 critical | `cargo audit` |
| Startup Time | <100ms | Benchmarks |
| Binary Size | <20MB | Release binary |

### v2.1.0
| Metric | Target | Measurement |
|--------|--------|-------------|
| Commands Working | 45/45 | Manual testing |
| AI Generation Time | <5s | Benchmarks |
| Graph Query Time | <1s | Benchmarks (10k nodes) |
| Test Coverage | 80%+ | `cargo tarpaulin` |

### v2.2.0
| Metric | Target | Measurement |
|--------|--------|-------------|
| Commands Working | 67+/67+ | Automated E2E tests |
| v1 Compatibility | 100% | Migration tests |
| Test Coverage | 85%+ | `cargo tarpaulin` |
| Performance vs v1 | ≥100% | Comparative benchmarks |

---

## Timeline Summary

```
Week 1-2:  v2.0.0 MVP (fix blockers + stabilize)
  ├─ Fix build failures
  ├─ Fix security vulnerability
  ├─ Implement marketplace stubs
  └─ Release v2.0.0 (20 commands)

Week 3-6:  v2.1.0 (AI + Graph + Shell)
  ├─ Implement AI commands (8)
  ├─ Implement Graph commands (9)
  ├─ Implement Shell commands (8)
  └─ Release v2.1.0 (45 commands)

Week 7-12: v2.2.0 (Complete migration)
  ├─ Implement remaining commands (22)
  ├─ Build v1 compatibility layer
  ├─ Complete documentation
  └─ Release v2.2.0 (67+ commands)
```

**Total Timeline**: 12 weeks (3 months)

---

## Next Steps

### Immediate (This Week)
1. ✅ Fix build failures (2 hours)
2. ✅ Fix security vulnerability (4 hours)
3. ✅ Implement marketplace stubs (6 hours)
4. ✅ Run integration tests (2 hours)
5. ✅ Update documentation (2 hours)

**Total**: 16 hours (2 developer days)

### Short-Term (Next 2 Weeks)
1. ✅ Release v2.0.0 MVP (20 commands)
2. ✅ Community beta testing
3. ✅ Fix critical bugs
4. ✅ Performance optimization
5. ✅ Start v2.1.0 planning

### Medium-Term (Next 6 Weeks)
1. ✅ Implement AI commands
2. ✅ Implement Graph commands
3. ✅ Implement Shell commands
4. ✅ Release v2.1.0 (45 commands)
5. ✅ Start v2.2.0 planning

### Long-Term (Next 12 Weeks)
1. ✅ Complete all 67+ commands
2. ✅ v1 compatibility layer
3. ✅ Release v2.2.0 (complete)
4. ✅ Deprecate v1
5. ✅ Community celebration

---

## Conclusion

**Current State**: 36 commands, build broken, marketplace stubs

**Target State**: 67+ commands, production-ready, v1 compatible

**Strategy**: Phased rollout (MVP → Features → Complete)

**Timeline**: 12 weeks (3 months)

**Risk**: Medium (manageable with automation)

**Recommendation**: **Proceed with phased approach**

**First Priority**: Fix v2.0.0 blockers (16 hours)

---

**Document Version**: 1.0
**Last Updated**: 2025-11-01
**Owner**: ggen Core Team
**Status**: Draft (awaiting approval)
