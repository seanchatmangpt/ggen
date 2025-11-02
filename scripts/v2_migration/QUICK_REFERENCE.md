# Migration Automation - Quick Reference

## TL;DR

```bash
# Step 1: Generate all CLI wrappers (10 minutes)
./scripts/v2_migration/migrate_remaining_commands.sh

# Step 2: Validate migration (5 minutes)
./scripts/v2_migration/validate_migration.sh

# Step 3: Implement domain logic (manual, 8-12 hours)
# Find TODOs: grep -r "TODO: Implement" cli/src/domain/

# Step 4: Final validation
./scripts/v2_migration/validate_migration.sh
cargo test --all --all-features
```

## Commands

### Generate Single Wrapper

```bash
./scripts/v2_migration/generate_cli_wrapper.sh <noun> <verb>

# Example
./scripts/v2_migration/generate_cli_wrapper.sh template show
```

### Batch Migration (67 commands)

```bash
# Preview (dry run)
./scripts/v2_migration/migrate_remaining_commands.sh --dry-run

# Execute
./scripts/v2_migration/migrate_remaining_commands.sh
```

### Validation

```bash
# Full validation (recommended)
./scripts/v2_migration/validate_migration.sh

# Quick validation (skip E2E)
./scripts/v2_migration/validate_migration.sh --quick
```

## What Gets Generated

For each `{noun} {verb}` command:

```
cli/src/commands/{noun}/{verb}.rs    ← CLI wrapper (150-200 lines)
cli/src/domain/{noun}/{verb}.rs      ← Domain logic (if missing)
```

## File Structure

```rust
// CLI Layer (cli/src/commands/{noun}/{verb}.rs)
#[derive(Args, Debug)]
pub struct {Verb}Args {
    #[arg(long)]
    pub detailed: bool,
}

pub fn run(args: &{Verb}Args) -> Result<()> {
    runtime::execute(async {
        domain::{noun}::{verb}::{verb}_and_display(args).await
    })
}
```

```rust
// Domain Layer (cli/src/domain/{noun}/{verb}.rs)
pub async fn {verb}_and_display(detailed: bool, json: bool) -> Result<()> {
    // TODO: Implement business logic
    Ok(())
}
```

## Priority Order

1. **HIGH** (30 commands): User-facing, critical
2. **MEDIUM** (25 commands): Important, frequent
3. **LOW** (12 commands): Utility, admin

See `MIGRATION_PLAN.md` for full list.

## Success Criteria

- ✅ All 67 commands have CLI wrappers
- ✅ All compile without errors
- ✅ All tests pass (100% pass rate)
- ✅ Compilation time <45s
- ✅ Binary size <30MB
- ✅ No business logic in CLI layer
- ✅ Domain layer isolated from clap

## Common Issues

### "Domain logic not found"

```bash
# Script creates placeholder automatically
# Implement at: cli/src/domain/{noun}/{verb}.rs
```

### Compilation errors

```bash
# Check module declarations
# cli/src/commands/{noun}/mod.rs
# cli/src/domain/{noun}/mod.rs

# Add: pub mod {verb};
```

### Test failures

```bash
# Run specific test
cargo test {noun}_{verb} -- --nocapture

# Check async runtime setup
# Ensure #[tokio::test] for async tests
```

## Timeline

| Phase | Duration | Type |
|-------|----------|------|
| Generate wrappers | 10 min | 🤖 Automated |
| Validate | 5 min | 🤖 Automated |
| Implement domain logic | 8-12 hrs | 👤 Manual |
| Integration tests | 4-6 hrs | 👤 Manual |
| Final validation | 30 min | 🤖 Automated |

**Total**: ~16-24 hours

## Validation Checklist

```bash
# 1. Syntax check
cargo check --all-features

# 2. Build
cargo build --all-features

# 3. Unit tests
cargo test --lib

# 4. Integration tests
cargo test --test '*'

# 5. E2E validation
./scripts/v2_migration/validate_migration.sh

# 6. Performance check
time cargo build --release
ls -lh target/release/ggen
```

## Next Steps After Automation

1. Review generated files for TODOs
2. Implement domain logic (priority order)
3. Add integration tests (critical paths)
4. Update documentation
5. Run final validation
6. Prepare release notes

## Documentation

- **Full plan**: `MIGRATION_PLAN.md`
- **README**: `README.md`
- **User guide**: `docs/MIGRATION_V1_TO_V2.md`

## Support

- Issues: GitHub Issues
- Questions: GitHub Discussions
- Code review: GitHub PRs

---

**Remember**: Automation handles 80% boilerplate, you focus on 20% critical business logic!
