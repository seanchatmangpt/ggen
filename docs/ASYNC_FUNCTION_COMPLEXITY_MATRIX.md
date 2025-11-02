# Async Function Complexity Matrix - ggen v2.0

## Quick Reference for Refactoring

**Total Async Functions**: 203 (non-test)
**Total Effort**: 509 hours (can be parallelized)

---

## Simple Functions (145 total - 71%)

**Characteristics**: 5-30 lines, 1-3 await points, straightforward logic
**Effort per function**: 1-2 hours
**Total effort**: ~217 hours

### Domain Layer (25 functions)

| File | Function | Lines | Await Points | Priority |
|------|----------|-------|--------------|----------|
| `domain/ai/analyze.rs` | `analyze_code` | 9 | 1 | High |
| `domain/ai/analyze.rs` | `analyze_project` | 11 | 2 | High |
| `domain/project/init.rs` | `init_project` | 15 | 3 | High |
| `domain/project/build.rs` | `build_project` | 14 | 2 | High |
| `domain/project/build.rs` | `clean_project` | 8 | 1 | Medium |
| `domain/marketplace/list.rs` | `list_and_display` | 28 | 2 | High |
| `domain/marketplace/update.rs` | `check_for_updates` | 25 | 3 | Medium |

### Commands Layer (120 functions)

| Module | Count | Priority | Notes |
|--------|-------|----------|-------|
| `cmds/hook/` | 18 | High | Core workflow functionality |
| `cmds/template/` | 22 | High | Template operations |
| `cmds/project/` | 25 | High | Project scaffolding |
| `cmds/ai/` | 15 | Medium | AI integration |
| `cmds/graph/` | 20 | Medium | RDF operations |
| `cmds/market/` | 15 | Medium | Marketplace |
| `cmds/audit/` | 5 | Low | Auditing tools |

---

## Medium Functions (48 total - 23%)

**Characteristics**: 30-100 lines, 3-8 await points, complex error handling
**Effort per function**: 3-5 hours
**Total effort**: ~192 hours

### High-Priority Files

| File | Function | Lines | Complexity | Dependencies |
|------|----------|-------|------------|--------------|
| `domain/marketplace/search.rs` | `search_and_display` | 82 | Medium | tokio::fs, serde_json |
| `domain/marketplace/install.rs` | `install_and_report` | 74 | High | tokio::fs, tar extraction |
| `domain/marketplace/publish.rs` | `publish_and_report` | 85 | High | tokio::fs, registry |
| `domain/marketplace/update.rs` | `update_and_report` | 68 | Medium | lockfile parsing |
| `cmds/hook/validate.rs` | `validate_hook_configuration` | 58 | Medium | toml parsing |
| `cmds/hook/run.rs` | `run` | 52 | Medium | process execution |
| `cmds/hook/create.rs` | `run` | 92 | High | file creation |
| `cmds/graph/validate.rs` | `run_with_deps` | 65 | Medium | SHACL validation |
| `cmds/graph/query.rs` | `run_with_deps` | 77 | Medium | SPARQL execution |
| `cmds/graph/export.rs` | `run_with_deps` | 72 | Medium | format conversion |

### Refactoring Pattern

```rust
// BEFORE (async - 74 lines)
pub async fn install_and_report(
    package: &str,
    target: Option<&str>,
    force: bool,
    include_deps: bool,
    dry_run: bool,
) -> Result<()> {
    let (pkg_name, version) = parse_package_spec(package)?;
    let target_dir = determine_target(target)?;

    // Multiple async operations
    tokio::fs::create_dir_all(&target_dir).await?;
    fetch_and_extract(&pkg_name, version.as_deref(), &registry_path, &install_path).await?;
    update_lockfile(&lockfile_path, &pkg_name, version.as_deref().unwrap_or("latest")).await?;

    if include_deps {
        install_dependencies(&install_path).await?;
    }

    Ok(())
}

// AFTER (sync wrapper in cmds/, async stays in domain/)
// In cmds/market/install.rs
pub fn run(args: &InstallArgs) -> Result<()> {
    crate::runtime::execute(async {
        domain::marketplace::install::install_and_report(
            &args.package,
            args.target.as_deref(),
            args.force,
            args.include_deps,
            args.dry_run,
        ).await
    })
}

// In domain/marketplace/install.rs (stays async - unchanged)
pub async fn install_and_report(...) -> Result<()> {
    // Existing implementation
}
```

---

## Complex Functions (10 total - 5%)

**Characteristics**: 100+ lines, 10+ await points, orchestration logic
**Effort per function**: 8-12 hours
**Total effort**: ~100 hours

### Critical Files

| File | Lines | Async Funcs | Complexity | Refactor Strategy |
|------|-------|-------------|------------|-------------------|
| `cmds/ci/release.rs` | 786 | 19 | Very High | Split into 3 modules |
| `cmds/ci/workflow.rs` | 601 | 17 | High | Extract workflow parsing |
| `cmds/ci/pages.rs` | 585 | 16 | High | Separate deployment logic |
| `cmds/shell/init.rs` | 489 | 13 | Medium | Split shell types |
| `cmds/shell/completion.rs` | 432 | 14 | Medium | Extract completion gen |
| `cmds/lifecycle/mod.rs` | 656 | 10+ | High | Extract lifecycle stages |
| `cmds/market/cache.rs` | 457 | 8 | Medium | Separate cache ops |
| `cmds/market/offline.rs` | 472 | 7 | Medium | Extract sync logic |
| `cmds/template/lint.rs` | 433 | 6 | Medium | Modularize linters |
| `cmds/template/new.rs` | 439 | 5 | Medium | Extract generators |

### Example: ci/release.rs Refactoring

**Current**: Single 786-line file with 19 async functions

**Proposed Structure**:
```
cmds/ci/release/
├── mod.rs           # Entry point (50 lines)
├── runner.rs        # Core execution (200 lines)
├── retry.rs         # Retry logic (150 lines)
├── metrics.rs       # Metrics collection (150 lines)
├── timeout.rs       # Timeout handling (100 lines)
└── dry_run.rs       # Dry run simulation (136 lines)
```

**Refactoring Steps**:
1. Extract retry logic to `retry.rs` (3h)
2. Extract metrics to `metrics.rs` (3h)
3. Extract timeout to `timeout.rs` (2h)
4. Extract dry-run to `dry_run.rs` (2h)
5. Simplify mod.rs to orchestration (2h)
**Total**: 12 hours

---

## Async/Sync Conversion Checklist

### For Each Function

- [ ] Identify if function does I/O (keep async in domain)
- [ ] Check if called from command layer (needs sync wrapper)
- [ ] Verify error handling (no unwrap/expect)
- [ ] Count await points (complexity indicator)
- [ ] Check for spawn/spawn_blocking (review needed)
- [ ] Test async behavior is preserved
- [ ] Update documentation

### Validation

```bash
# Check no async in commands layer
rg "pub async fn run" cli/src/cmds/ && echo "FAIL: Found async run()" || echo "PASS"

# Check runtime::execute usage
rg "runtime::execute" cli/src/cmds/ | wc -l  # Should be 74+

# Check unwrap count
rg "\.unwrap\(\)" cli/src/cmds cli/src/domain --no-tests | wc -l  # Should be 0
```

---

## Priority Matrix

### High Priority (Complete First - 5 weeks)
- All command run() functions (74 files)
- Marketplace domain functions (search, install, publish)
- Hook operations (validate, run, create)
- Template operations (generate, lint, new)

### Medium Priority (Week 6-8)
- Graph operations (query, export, validate)
- CI/CD operations (release, workflow, pages)
- Project operations (gen, plan, apply)

### Low Priority (Week 9-10)
- Audit operations
- Shell operations (completion can stay async longer)
- Lifecycle operations (complex, can defer)

---

## Automated Refactoring Script Template

```bash
#!/bin/bash
# refactor-async-to-sync.sh

FILE=$1
FUNCTION=$2

# 1. Find async function in cmds/
LOCATION=$(rg -l "pub async fn $FUNCTION" cli/src/cmds/)

if [ -z "$LOCATION" ]; then
    echo "Function $FUNCTION not found in cmds/"
    exit 1
fi

# 2. Replace async fn with sync fn + runtime::execute
sed -i '' "s/pub async fn $FUNCTION/pub fn $FUNCTION/g" $LOCATION

# 3. Wrap body with runtime::execute
# (Manual step - too complex for sed)

echo "Refactored $FUNCTION in $LOCATION"
echo "⚠️  Manual: Wrap function body with crate::runtime::execute(async { ... })"
```

---

## Tracking Progress

Create a tracking spreadsheet:

| Module | Total Funcs | Converted | Tests Pass | Status |
|--------|-------------|-----------|------------|--------|
| hook/ | 18 | 0 | - | ⬜ Not Started |
| template/ | 22 | 0 | - | ⬜ Not Started |
| project/ | 25 | 0 | - | ⬜ Not Started |
| market/ | 15 | 0 | - | ⬜ Not Started |
| graph/ | 20 | 0 | - | ⬜ Not Started |
| ci/ | 52 | 0 | - | ⬜ Not Started |
| ai/ | 15 | 0 | - | ⬜ Not Started |
| audit/ | 5 | 0 | - | ⬜ Not Started |

Legend:
- ⬜ Not Started
- 🟨 In Progress
- ✅ Complete
- ❌ Blocked

---

## Key Insights

1. **74 command files** need sync wrappers (critical path)
2. **Domain layer stays async** (correct design - no changes)
3. **Test functions stay async** (tokio::test is fine)
4. **Runtime bridge pattern** is simple and effective
5. **Can parallelize** refactoring across modules
6. **Automated testing** will catch regressions

**Total Parallelizable Work**: 509 hours
**With 3 developers**: ~170 hours each = 4-5 weeks
**With 6 developers**: ~85 hours each = 2-3 weeks

---

**Generated**: 2025-11-01
**Next Update**: After Phase 1 (Week 2)
