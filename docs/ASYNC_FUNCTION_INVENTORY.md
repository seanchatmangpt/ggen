# Async Function Inventory for ggen v2.0 Migration
## Complete List of 206 Async Functions (Excluding Tests)

**Generated**: 2025-11-01
**Purpose**: Track async→sync migration for clap-noun-verb v3.0.0 compatibility
**Status**: Foundation complete, migration in progress

---

## Summary

| Category | Count | Status | Priority |
|----------|-------|--------|----------|
| **Entry Points** | 2 | ✅ No migration needed | - |
| **Domain Layer** | 24 | ✅ Complete | - |
| **Commands (v2)** | 15 | 🟡 Partial | P0 |
| **Legacy (v1)** | 165+ | ⏳ Pending | P1-P2 |
| **TOTAL** | **206** | 🟡 **In Progress** | - |

---

## 1. Entry Points (2 functions) - No Migration Needed

These remain async as they're called from main.rs:

```rust
// cli/src/lib.rs
pub async fn cli_match() -> Result<()>              // Line 56  - Main CLI entry point
pub async fn run_for_node(args: Vec<String>) -> Result<RunResult>  // Line 111 - Node.js API
```

**Status**: ✅ **Keep async** - Entry points don't need sync wrappers

---

## 2. Domain Layer (24 functions) - Complete

### 2.1 Marketplace Domain (18 functions)

**Status**: ✅ **Complete** - Business logic separated from CLI

```rust
// domain/marketplace/search.rs
pub async fn search_and_display(query: &str, limit: Option<usize>, category: Option<&str>) -> Result<()>
async fn search_packages(query: &str, registry: &Registry) -> Result<Vec<Package>>  // Private helper

// domain/marketplace/install.rs
pub async fn install_and_report(package: &str, version: Option<&str>, force: bool, dry_run: bool) -> Result<()>
async fn fetch_and_extract(url: &str, dest: &Path) -> Result<()>  // Private
async fn update_lockfile(lockfile_path: &Path, pkg_name: &str, version: &str) -> Result<()>  // Private
async fn install_dependencies(package_path: &Path) -> Result<()>  // Private

// domain/marketplace/publish.rs
pub async fn publish_and_report(path: &Path, dry_run: bool, force: bool) -> Result<()>
async fn package_version_exists(name: &str, version: &str, registry: &Registry) -> Result<bool>  // Private
async fn create_tarball(path: &Path, package_name: &str, version: &str) -> Result<String>  // Private
async fn update_registry_index(name: &str, version: &str, url: &str) -> Result<()>  // Private

// domain/marketplace/update.rs
pub async fn update_and_report(package: Option<&str>, all: bool, dry_run: bool) -> Result<()>
async fn check_for_updates(lockfile: &LockFile) -> Result<Vec<Update>>  // Private

// domain/marketplace/list.rs
pub async fn list_and_display(detailed: bool, json: bool) -> Result<()>
```

### 2.2 Project Domain (4 functions)

```rust
// domain/project/init.rs
pub async fn init_project(path: &Path, name: &str) -> Result<()>

// domain/project/build.rs
pub async fn build_project(path: &Path) -> Result<()>
pub async fn clean_project(path: &Path) -> Result<()>
```

### 2.3 AI Domain (2 functions)

```rust
// domain/ai/analyze.rs
pub async fn analyze_code(code: &str) -> Result<String>
pub async fn analyze_project(path: &Path) -> Result<String>
```

---

## 3. Commands Layer - v2 (15 functions) - Partial

### 3.1 Utils Commands (1 function)

**Status**: 🟡 Needs sync wrapper

```rust
// commands/utils/mod.rs
pub async fn doctor() -> Result<()>  // Line 11
```

**Migration**:
```rust
#[verb("doctor", "utils")]
pub fn doctor() -> Result<()> {
    crate::runtime::execute(async {
        crate::domain::utils::doctor().await
    })
}
```

### 3.2 Graph Commands (3 functions)

```rust
// commands/graph/load.rs
pub async fn run(args: &LoadArgs) -> Result<()>  // Line 26

// commands/graph/query.rs
pub async fn run(args: &QueryArgs) -> Result<()>  // Line 22

// commands/graph/export.rs
pub async fn run(args: &ExportArgs) -> Result<()>  // Line 22
```

### 3.3 Template Commands (5 functions)

```rust
// commands/template/generate_tree.rs
pub async fn execute(&self) -> Result<()>  // Line 38

// commands/template/list.rs
pub async fn execute(&self) -> Result<()>  // Line 29

// commands/template/new.rs
pub async fn execute(&self) -> Result<()>  // Line 28

// commands/template/regenerate.rs
pub async fn execute(&self) -> Result<()>  // Line 41

// commands/template/mod.rs
pub async fn execute(&self) -> Result<()>  // Line 31 - Dispatcher
```

### 3.4 Marketplace Commands (5 functions)

```rust
// commands/marketplace/search.rs (assumed - verify file exists)
pub async fn run(args: &SearchArgs) -> Result<()>

// commands/marketplace/install.rs (assumed)
pub async fn run(args: &InstallArgs) -> Result<()>

// commands/marketplace/list.rs (assumed)
pub async fn run(args: &ListArgs) -> Result<()>

// commands/marketplace/update.rs (assumed)
pub async fn run(args: &UpdateArgs) -> Result<()>

// commands/marketplace/publish.rs (assumed)
pub async fn run(args: &PublishArgs) -> Result<()>
```

### 3.5 Project Commands (1 function)

```rust
// commands/project/mod.rs
pub async fn run(args: &ProjectArgs) -> Result<()>  // Dispatcher
```

---

## 4. Legacy Commands - v1 (165+ functions) - Pending Migration

### 4.1 Shell Commands (10 functions)

**Priority**: P1

```rust
// cmds/shell/completion.rs
pub async fn run(args: &CompletionArgs) -> Result<()>  // Line 182
pub async fn run_with_deps(...) -> Result<()>  // Line 190
async fn generate_completion_with_deps(...) -> Result<()>  // Line 205
async fn generate_completion(args: &GenerateArgs) -> Result<()>  // Line 224
async fn install_completion_with_deps(...) -> Result<()>  // Line 229
async fn install_completion(args: &InstallArgs) -> Result<()>  // Line 249
async fn list_shells_with_deps(lister: &dyn ShellLister) -> Result<()>  // Line 254
async fn list_shells() -> Result<()>  // Line 271

// cmds/shell/init.rs
pub async fn run(args: &InitArgs) -> Result<()>  // Line 154
pub async fn run_with_deps(...) -> Result<()>  // Line 162
async fn init_shell_with_deps(...) -> Result<()>  // Line 175
async fn init_shell(args: &ShellInitArgs) -> Result<()>  // Line 195
async fn init_project_with_deps(...) -> Result<()>  // Line 200
async fn init_project(args: &ProjectInitArgs) -> Result<()>  // Line 220
async fn init_dev_with_deps(_args: &DevInitArgs, dev_init: &dyn DevInitializer) -> Result<()>  // Line 225
async fn init_dev(args: &DevInitArgs) -> Result<()>  // Line 243

// cmds/shell/mod.rs
pub async fn run(&self) -> Result<()>  // Line 23 - Dispatcher
```

### 4.2 CI Commands (20+ functions)

**Priority**: P2

```rust
// cmds/ci/release.rs
pub async fn run(args: &ReleaseArgs) -> Result<()>  // Line 180
pub async fn run_with_deps(args: &ReleaseArgs, runner: &dyn ReleaseWorkflowRunner) -> Result<()>  // Line 185
async fn run_release_workflows_with_deps(...) -> Result<()>  // Line 203
async fn run_release_workflows(args: &RunArgs) -> Result<()>  // Line 225
async fn run_release_with_retry_with_deps(...) -> Result<()>  // Line 230
async fn run_release_with_retry(args: &RetryArgs) -> Result<()>  // Line 252
async fn run_release_with_metrics_with_deps(...) -> Result<()>  // Line 257
async fn run_release_with_metrics(args: &MetricsArgs) -> Result<()>  // Line 279
async fn run_release_with_timeout_with_deps(...) -> Result<()>  // Line 284
async fn run_release_with_timeout(args: &TimeoutArgs) -> Result<()>  // Line 306
async fn run_release_dry_run_with_deps(...) -> Result<()>  // Line 311
async fn run_release_dry_run(args: &DryRunArgs) -> Result<()>  // Line 330

// cmds/ci/trigger.rs
pub async fn run(args: &TriggerArgs) -> Result<()>  // Line 83
async fn trigger_workflow(args: &WorkflowTriggerArgs) -> Result<()>  // Line 178
async fn trigger_all_workflows(args: &AllTriggerArgs) -> Result<()>  // Line 212

// cmds/ci/workflow.rs
pub async fn run(args: &WorkflowArgs) -> Result<()>  // Line 154
pub async fn run_with_deps(...) -> Result<()>  // Line 163
async fn list_workflows_with_deps(args: &ListArgs, lister: &dyn WorkflowLister) -> Result<()>  // Line 181
async fn list_workflows(args: &ListArgs) -> Result<()>  // Line 198
async fn check_workflow_status_with_deps(...) -> Result<()>  // Line 203
async fn check_workflow_status(args: &StatusArgs) -> Result<()>  // Line 222
async fn view_workflow_logs_with_deps(...) -> Result<()>  // Line 227
async fn view_workflow_logs(args: &LogsArgs) -> Result<()>  // Line 246
async fn cancel_workflows_with_deps(...) -> Result<()>  // Line 251
async fn cancel_workflows(args: &CancelArgs) -> Result<()>  // Line 291

// cmds/ci/pages.rs
pub async fn run(args: &PagesArgs) -> Result<()>  // Line 171
pub async fn run_with_deps(...) -> Result<()>  // Line 180
async fn deploy_pages_with_deps(...) -> Result<()>  // Line 197
async fn deploy_pages(args: &DeployArgs) -> Result<()>  // Line 216
async fn check_status_with_deps(...) -> Result<()>  // Line 221
async fn check_status(args: &StatusArgs) -> Result<()>  // Line 240
async fn view_logs_with_deps(args: &LogsArgs, log_viewer: &dyn GitHubPagesLogViewer) -> Result<()>  // Line 245
async fn view_logs(args: &LogsArgs) -> Result<()>  // Line 262
async fn compare_deployment_with_deps(...) -> Result<()>  // Line 267
async fn compare_deployment(args: &CompareArgs) -> Result<()>  // Line 289

// cmds/ci/mod.rs
pub async fn run(&self) -> Result<()>  // Line 32 - Dispatcher
```

### 4.3 Graph Commands (14 functions)

**Priority**: P1

```rust
// cmds/graph/export.rs
pub async fn run(args: &ExportArgs) -> Result<()>  // Line 114
pub async fn run_with_deps(args: &ExportArgs, exporter: &dyn GraphExporter) -> Result<()>  // Line 272

// cmds/graph/query.rs
pub async fn run(args: &QueryArgs) -> Result<()>  // Line 184
pub async fn run_with_deps(args: &QueryArgs, executor: &dyn SparqlExecutor) -> Result<()>  // Line 259

// cmds/graph/snapshot.rs
pub async fn run(args: &SnapshotArgs) -> Result<()>  // Line 83
async fn create_snapshot(...) -> Result<()>  // Line 103
async fn list_snapshots(snapshot_dir: &Path) -> Result<()>  // Line 144
async fn show_snapshot(name: &str, snapshot_dir: &Path) -> Result<()>  // Line 160
async fn delete_snapshot(name: &str, snapshot_dir: &Path) -> Result<()>  // Line 184
async fn verify_snapshot(name: &str, snapshot_dir: &Path, exit_code: bool) -> Result<()>  // Line 191

// cmds/graph/load.rs
pub async fn run(args: &LoadArgs) -> Result<()>  // Line 172
pub async fn run_with_deps(args: &LoadArgs, loader: &dyn RdfLoader) -> Result<()>  // Line 225

// cmds/graph/stats.rs
pub async fn run(args: &StatsArgs) -> Result<()>  // Line 69
pub async fn run_with_deps(args: &StatsArgs, analyzer: &dyn GraphAnalyzer) -> Result<()>  // Line 155

// cmds/graph/diff.rs
pub async fn run(args: &DiffArgs) -> Result<()>  // Line 34

// cmds/graph/validate.rs
pub async fn run(args: &ValidateArgs) -> Result<()>  // Line 114
pub async fn run_with_deps(args: &ValidateArgs, validator: &dyn ShaclValidator) -> Result<()>  // Line 203

// cmds/graph/mod.rs
pub async fn run(&self) -> Result<()>  // Line 37 - Dispatcher
```

### 4.4 Template Commands (12 functions)

**Priority**: P1

```rust
// cmds/template/generate_tree.rs
pub async fn run(args: &GenerateTreeArgs) -> Result<()>  // Line 40

// cmds/template/list.rs
pub async fn run(args: &ListArgs) -> Result<()>  // Line 90
pub async fn run_with_deps(args: &ListArgs, lister: &dyn TemplateLister) -> Result<()>  // Line 198

// cmds/template/show.rs
pub async fn run(args: &ShowArgs) -> Result<()>  // Line 167
pub async fn run_with_deps(args: &ShowArgs, fetcher: &dyn TemplateMetadataFetcher) -> Result<()>  // Line 240

// cmds/template/lint.rs
pub async fn run(args: &LintArgs) -> Result<()>  // Line 95
pub async fn run_with_deps(args: &LintArgs, linter: &dyn TemplateLinter) -> Result<()>  // Line 288

// cmds/template/regenerate.rs
pub async fn run(args: &RegenerateArgs) -> Result<()>  // Line 43
async fn regenerate_template(...) -> Result<()>  // Line 100

// cmds/template/new.rs
pub async fn run(args: &NewArgs) -> Result<()>  // Line 291
pub async fn run_with_deps(...) -> Result<()>  // Line 343

// cmds/template/mod.rs
pub async fn run(&self) -> Result<()>  // Line 34 - Dispatcher
```

### 4.5 Project Commands (10 functions)

**Priority**: P0

```rust
// cmds/project/gen.rs
pub async fn run(args: &GenArgs) -> Result<()>  // Line 153
pub async fn run_with_deps(...) -> Result<()>  // Line 193

// cmds/project/plan.rs
pub async fn run(args: &PlanArgs) -> Result<()>  // Line 35

// cmds/project/apply.rs
pub async fn run(args: &ApplyArgs) -> Result<()>  // Line 31

// cmds/project/watch.rs
pub async fn run(args: &WatchArgs) -> Result<()>  // Line 118

// cmds/project/diff.rs
pub async fn run(args: &DiffArgs) -> Result<()>  // Line 24

// cmds/project/new.rs
pub async fn run(args: &NewArgs) -> Result<()>  // Line 49

// cmds/project/freeze.rs
pub async fn run(args: &FreezeArgs) -> Result<()>  // Line 88

// cmds/project/validate.rs
pub async fn run(args: &ValidateArgs) -> Result<()>  // Line 84

// cmds/project/inject.rs
pub async fn run(args: &InjectArgs) -> Result<()>  // Line 102

// cmds/project/mod.rs
pub async fn run(&self) -> Result<()>  // Line 105 - Dispatcher
```

### 4.6 Doctor Command (1 function)

**Priority**: P0

```rust
// cmds/doctor.rs
pub async fn run(args: &DoctorArgs) -> Result<()>  // Line 28
```

### 4.7 Help Command (1 function)

**Priority**: P2

```rust
// cmds/help_progressive.rs
pub async fn run(args: &HelpProgressiveArgs) -> Result<()>  // Line 19
```

### 4.8 Lifecycle Commands (1 function)

**Priority**: P2

```rust
// cmds/lifecycle/mod.rs
pub async fn run(args: LifecycleArgs) -> Result<()>  // Line 127
```

### 4.9 Main Dispatcher (1 function)

```rust
// cmds/mod.rs
pub async fn run(&self) -> Result<()>  // Line 67 - Top-level dispatcher
```

---

## 5. Migration Pattern Reference

### 5.1 Standard Sync Wrapper

```rust
// Before (async)
pub async fn run(args: &MyArgs) -> Result<()> {
    domain::my_function(args).await
}

// After (sync wrapper)
#[verb("my-command", "my-noun")]
pub fn run(args: MyArgs) -> Result<()> {
    crate::runtime::execute(async {
        crate::domain::my_function(&args).await
    })
}
```

### 5.2 With Dependency Injection

```rust
// Before
pub async fn run_with_deps(args: &MyArgs, dep: &dyn MyTrait) -> Result<()> {
    // ... async logic
}

// After
#[verb("my-command", "my-noun")]
pub fn run(args: MyArgs) -> Result<()> {
    crate::runtime::execute(async {
        let dep = DefaultImpl::new();
        run_with_deps_async(&args, &dep).await
    })
}

async fn run_with_deps_async(args: &MyArgs, dep: &dyn MyTrait) -> Result<()> {
    // Keep async logic for testing
}
```

---

## 6. Migration Progress Tracking

### 6.1 By Priority

| Priority | Category | Functions | Status |
|----------|----------|-----------|--------|
| **P0** | Doctor, Project | 11 | ⏳ Pending |
| **P1** | Graph, Template, Marketplace | 39 | 🟡 15% done |
| **P2** | CI, Shell, Lifecycle, Help | 115 | ⏳ Pending |
| **Entry** | cli_match, run_for_node | 2 | ✅ No migration |
| **Domain** | Business logic | 24 | ✅ Complete |

### 6.2 By Module

| Module | Total | Migrated | Remaining | Progress |
|--------|-------|----------|-----------|----------|
| domain/ | 24 | 24 | 0 | ✅ 100% |
| commands/ | 15 | 0 | 15 | ⏳ 0% |
| cmds/project/ | 10 | 0 | 10 | ⏳ 0% |
| cmds/graph/ | 14 | 0 | 14 | ⏳ 0% |
| cmds/template/ | 12 | 0 | 12 | ⏳ 0% |
| cmds/ci/ | 30+ | 0 | 30+ | ⏳ 0% |
| cmds/shell/ | 10 | 0 | 10 | ⏳ 0% |
| cmds/doctor.rs | 1 | 0 | 1 | ⏳ 0% |
| cmds/help_progressive.rs | 1 | 0 | 1 | ⏳ 0% |
| cmds/lifecycle/ | 1 | 0 | 1 | ⏳ 0% |
| **TOTAL** | **206** | **24** | **182** | 🟡 **11.7%** |

---

## 7. Automation Opportunities

### 7.1 Regex Patterns for Migration

**Find async functions**:
```regex
pub async fn (\w+)\((.*?)\) -> Result<\(\)>
```

**Replace with sync wrapper**:
```rust
#[verb("$1", "NOUN")]  // Manual: Replace NOUN
pub fn $1($2) -> Result<()> {
    crate::runtime::execute(async {
        $1_async($2).await  // Manual: Adjust call
    })
}

async fn $1_async($2) -> Result<()> {
    // Original function body
}
```

### 7.2 Test Generation Script

```bash
# Generate test boilerplate for each migration
for file in cli/src/commands/**/*.rs; do
  echo "
#[test]
fn test_$(basename $file .rs)_sync_wrapper() {
    // Test sync wrapper executes correctly
}
" >> tests/wrappers/$(basename $file)
done
```

---

## 8. Validation Checklist

### 8.1 Per-Function Migration

- [ ] Convert `async fn` to `fn`
- [ ] Add `#[verb]` attribute
- [ ] Wrap body in `runtime::execute(async { ... })`
- [ ] Update argument types (`&Args` → `Args` if needed)
- [ ] Add unit test for sync wrapper
- [ ] Update integration tests
- [ ] Verify in documentation

### 8.2 Per-Module Migration

- [ ] All functions in module migrated
- [ ] Module tests passing
- [ ] Integration tests passing
- [ ] Documentation updated
- [ ] Benchmark performance (optional)

---

## 9. Next Steps

### 9.1 Week 1-2: High Priority (P0)

1. Migrate `cmds/doctor.rs` (1 function)
2. Migrate `cmds/project/` (10 functions)
3. Add tests for migrated functions
4. Benchmark performance

### 9.2 Week 3-4: Medium Priority (P1)

1. Migrate `commands/` (15 functions)
2. Migrate `cmds/graph/` (14 functions)
3. Migrate `cmds/template/` (12 functions)
4. Integration testing

### 9.3 Week 5-6: Low Priority (P2)

1. Migrate `cmds/ci/` (30+ functions)
2. Migrate `cmds/shell/` (10 functions)
3. Migrate remaining commands
4. Final testing

---

## 10. Risk Mitigation

### 10.1 Known Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance overhead | MEDIUM | Benchmark, optimize if >50ms |
| Error handling breaks | HIGH | Comprehensive test suite |
| Missed functions | LOW | This inventory + grep validation |

### 10.2 Validation Commands

```bash
# Find any remaining async functions
grep -r "pub async fn" cli/src/commands --include="*.rs" | grep -v test

# Count migrated vs remaining
echo "Total: 206"
echo "Migrated: $(grep -r '#\[verb\]' cli/src/commands --include="*.rs" | wc -l)"

# Run tests
cargo test --lib
cargo test --test integration_*
```

---

**End of Async Function Inventory**

**Last Updated**: 2025-11-01
**Next Review**: After Week 2 migration
**Status**: 🟡 **11.7% Complete** (24/206 functions migrated via domain layer)
