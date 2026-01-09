# v6.0 Migration Quick Start

**5-Minute Guide** to migrating from ggen v5.1.0 to v6.0.0

---

## Am I Affected?

### ✅ You're Good (No Action Needed)

You **do not** need to change anything if you:
- Only use `ggen` CLI commands (`ggen sync`, `ggen ai`, etc.)
- Only use Tera templates and SPARQL queries
- Only edit `ggen.toml` configuration files
- Do NOT import `ggen-core` types in Rust code

**Just upgrade and continue:**
```bash
cargo install ggen-cli@6.0.0
ggen sync  # Works exactly as before
```

---

### ⚠️ You Need to Act (Import Changes Required)

You **must** update imports if you:
- Use `ggen-core` as a library dependency
- Import types like `ProtectedPath`, `PathProtectionError` in Rust code
- Have custom code that uses `use ggen_core::types::{...}`

**Time required**: 5-10 minutes (automated script available)

---

## Quick Migration (3 Steps)

### Step 1: Run Migration Script (2 minutes)

```bash
cd /path/to/your/project
./scripts/migrate_to_v6.sh
```

The script will:
- ✅ Backup your code
- ✅ Update import paths automatically
- ✅ Verify compilation
- ✅ Show summary of changes

### Step 2: Review Changes (2 minutes)

```bash
git diff
```

Look for changes like:
```diff
- use ggen_core::types::ProtectedPath;
+ use ggen_core::protection::ProtectedPath;
```

### Step 3: Test and Commit (1 minute)

```bash
cargo make test
git commit -am "chore: migrate to ggen v6.0.0"
```

**Done!** 🎉

---

## Manual Migration (If Script Fails)

### Find and Replace (All Files)

Use your editor's find-and-replace:

| Find | Replace |
|------|---------|
| `use ggen_core::types::ProtectedPath` | `use ggen_core::protection::ProtectedPath` |
| `use ggen_core::types::PathError` | `use ggen_core::protection::PathError` |
| `use ggen_core::types::PathProtectionError` | `use ggen_core::protection::PathProtectionError` |
| `use ggen_core::types::PathProtector` | `use ggen_core::protection::PathProtector` |
| `use ggen_core::types::GlobPattern` | `use ggen_core::protection::GlobPattern` |

### Grouped Imports

**Before**:
```rust
use ggen_core::types::{
    ProtectedPath,
    PathProtectionError,
    EnterpriseConfig  // This stays in types
};
```

**After**:
```rust
use ggen_core::protection::{ProtectedPath, PathProtectionError};
use ggen_core::types::EnterpriseConfig;  // Enterprise types stay
```

### Verify Compilation

```bash
cargo check
```

Fix any remaining errors following the import table above.

---

## What Changed?

### Import Path Changes

| Type | Old Location (v5.1.0) | New Location (v6.0.0) |
|------|----------------------|----------------------|
| `ProtectedPath` | `ggen_core::types` | `ggen_core::protection` |
| `PathError` | `ggen_core::types` | `ggen_core::protection` |
| `PathProtectionError` | `ggen_core::types` | `ggen_core::protection` |
| `PathProtector` | `ggen_core::types` | `ggen_core::protection` |
| `GlobPattern` | `ggen_core::types` | `ggen_core::protection` |

### New Modules

| Module | Purpose | Example |
|--------|---------|---------|
| `ggen_core::protection` | Path safety and validation | `use ggen_core::protection::ProtectedPath;` |
| `ggen_core::signals` | Quality gate signals | `use ggen_core::signals::AndonSignal;` |

### What Did NOT Change

- ✅ Function signatures
- ✅ Trait definitions
- ✅ Struct field names
- ✅ Template syntax
- ✅ SPARQL syntax
- ✅ CLI command syntax
- ✅ Configuration format

---

## Common Errors and Fixes

### Error 1: Type Not Found in `types`

```
error[E0432]: unresolved import `ggen_core::types::ProtectedPath`
  --> src/main.rs:5:5
   |
5  | use ggen_core::types::ProtectedPath;
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

**Fix**:
```rust
// Change this line:
use ggen_core::types::ProtectedPath;

// To this:
use ggen_core::protection::ProtectedPath;
```

### Error 2: Cannot Find `AndonSignal`

```
error[E0433]: failed to resolve: could not find `AndonSignal` in `ggen_core`
```

**Fix** (This is a NEW type):
```rust
// Add this import:
use ggen_core::signals::AndonSignal;
```

### Error 3: Build Time Increased

**Expected**: v6.0.0 adds 7 new crates, increasing build time by ~20-30%

**Fix**:
```bash
# Use incremental builds
cargo make check        # <5s
cargo test -p ggen-core # Test specific crate

# Enable sparse registry
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse
```

---

## Testing Your Migration

### Minimal Testing (2 minutes)

```bash
cargo check  # Verify compilation
cargo test   # Run test suite
```

### Full Testing (5 minutes)

```bash
cargo make check      # Compilation
cargo make test       # All tests
cargo make lint       # Code quality
cargo make pre-commit # Complete validation
```

### Integration Testing (Optional)

```bash
cargo test --test '*'  # All integration tests
cargo bench           # Performance benchmarks (optional)
```

---

## Rollback Plan

If something goes wrong:

### Option 1: Restore from Backup

```bash
# The migration script creates a backup
rm -rf crates/
cp -r .ggen_migration_backup_TIMESTAMP/crates/ .
```

### Option 2: Downgrade to v5.1.0

```bash
cargo install ggen-cli@5.1.0
# Your code will work with v5.1.0
```

### Option 3: Git Reset

```bash
git reset --hard HEAD  # Discard all changes
git clean -fd          # Remove untracked files
```

---

## Performance Impact

### Build Times

| Scenario | v5.1.0 | v6.0.0 | Change |
|----------|--------|--------|--------|
| Full workspace build | 30-45s | 40-60s | +20-30% |
| Incremental build | 5-8s | 5-10s | Minimal |
| `cargo make check` | <5s | <5s | None |

### Why the Increase?

7 new crates added:
- `ggen-ai` (AI orchestration)
- `knhk-etl` (ETL pipelines)
- `knhk-hot` (C FFI optimization)
- `knhk-connectors` (Kafka, HTTP, etc.)
- `knhk-lockchain` (Merkle receipts)
- `knhk-otel` (OpenTelemetry)
- `knhk-orchestrator` (Integration bridge)

### Mitigation

**Use incremental targets**:
```bash
cargo make check        # Fast feedback
cargo test -p ggen-core # Specific crate only
```

**Disable features** (if not needed):
```toml
[dependencies]
ggen-core = { version = "6.0", default-features = false }
```

---

## CI/CD Updates

### GitHub Actions

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      # Option 1: Use migration script
      - name: Migrate to v6.0.0
        run: ./scripts/migrate_to_v6.sh

      # Option 2: Already migrated, just test
      - name: Check compilation
        run: cargo make check

      - name: Run tests
        run: cargo make test
```

### GitLab CI

```yaml
# .gitlab-ci.yml
test:
  image: rust:latest
  script:
    - ./scripts/migrate_to_v6.sh  # If needed
    - cargo make check
    - cargo make test
```

---

## Support Levels

| Version | Status | Support |
|---------|--------|---------|
| **v6.0.0** | Current | ✅ Full support (all features + bug fixes) |
| **v5.1.0** | Maintenance | ⚠️ Security fixes only (3 months) |
| **v5.0.x** | End of Life | ❌ No support |

---

## Resources

### Documentation
- **Breaking Changes**: [BREAKING_CHANGES_V6.md](../BREAKING_CHANGES_V6.md) - Complete reference
- **Migration FAQ**: [V6_MIGRATION_FAQ.md](V6_MIGRATION_FAQ.md) - 30+ Q&A
- **Changelog**: [CHANGELOG.md](../CHANGELOG.md) - Full version history

### Tools
- **Migration Script**: `./scripts/migrate_to_v6.sh` - Automated import updates
- **Backup**: Auto-created at `.ggen_migration_backup_TIMESTAMP/`

### Community
- **GitHub Discussions**: [Ask questions](https://github.com/seanchatmangpt/ggen/discussions)
- **GitHub Issues**: [Report bugs](https://github.com/seanchatmangpt/ggen/issues)
- **API Docs**: [docs.rs/ggen-core](https://docs.rs/ggen-core)

---

## Summary Checklist

Before you start:
- [ ] Read this guide (5 minutes)
- [ ] Backup your code (automatic with script)

Migration:
- [ ] Run `./scripts/migrate_to_v6.sh`
- [ ] Review changes: `git diff`
- [ ] Test: `cargo make test`

Completion:
- [ ] Commit changes
- [ ] Update CI/CD (if needed)
- [ ] Deploy

**Estimated Total Time**: 10-15 minutes

---

**Need Help?** Open a [GitHub Discussion](https://github.com/seanchatmangpt/ggen/discussions) with your specific issue.

**Last Updated**: 2026-01-09
**ggen Version**: 6.0.0
