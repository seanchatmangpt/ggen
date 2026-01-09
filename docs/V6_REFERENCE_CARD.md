# ggen v6.0 Migration Reference Card

**Quick reference for v5.1.0 → v6.0.0 migration**

---

## Import Changes (Copy-Paste Ready)

### Before (v5.1.0) → After (v6.0.0)

```rust
// ❌ REMOVE (v5.1.0)
use ggen_core::types::{ProtectedPath, PathProtectionError};

// ✅ ADD (v6.0.0)
use ggen_core::protection::{ProtectedPath, PathProtectionError};
```

---

## Complete Import Map

| Type | v5.1.0 | v6.0.0 |
|------|--------|--------|
| `ProtectedPath` | `::types::` | `::protection::` |
| `PathError` | `::types::` | `::protection::` |
| `PathProtectionError` | `::types::` | `::protection::` |
| `PathProtector` | `::types::` | `::protection::` |
| `GlobPattern` | `::types::` | `::protection::` |
| `GeneratorPathGuard` | N/A | `::protection::` (NEW) |
| `AndonSignal` | N/A | `::signals::` (NEW) |
| `AndonContext` | N/A | `::signals::` (NEW) |

---

## One-Command Migration

### Automated (Recommended)

```bash
./scripts/migrate_to_v6.sh
```

### Manual (Linux)

```bash
find crates/ -name "*.rs" -type f -exec sed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +
```

### Manual (macOS)

```bash
# Requires: brew install gnu-sed
find crates/ -name "*.rs" -type f -exec gsed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +
```

---

## Common Errors

### Error 1: Type not found in `types`

```
error: unresolved import `ggen_core::types::ProtectedPath`
```

**Fix**: Change `::types::` to `::protection::`

### Error 2: Cannot find `AndonSignal`

```
error: could not find `AndonSignal` in `ggen_core`
```

**Fix**: Add `use ggen_core::signals::AndonSignal;` (NEW in v6)

### Error 3: Build time increased

**Expected behavior**: +20-30% due to 7 new crates

**Fix**: Use `cargo make check` (<5s) instead of `cargo build`

---

## Validation Commands

```bash
cargo make check      # Fast compilation check (<5s)
cargo make test       # Full test suite
cargo make lint       # Code quality
cargo make pre-commit # Complete validation
```

---

## What Did NOT Change

✅ Function signatures
✅ Trait definitions
✅ Struct fields
✅ Template syntax (.tera files)
✅ SPARQL syntax (.rq files)
✅ Configuration (ggen.toml)
✅ CLI commands

---

## New Features (Optional)

```bash
# Infrastructure generation
ggen paas generate-k8s schema/

# AI-powered generation
ggen ai generate -d "REST API"

# Quality gates
# Automatic in all commands
```

---

## Support Timeline

| Version | Support Type | Duration |
|---------|-------------|----------|
| v6.0.0 | Full | Current |
| v5.1.0 | Security fixes | 3 months |
| v5.0.x | None | EOL |

---

## Emergency Rollback

```bash
# Option 1: Restore backup
cp -r .ggen_migration_backup_*/crates/ .

# Option 2: Downgrade
cargo install ggen-cli@5.1.0

# Option 3: Git reset
git reset --hard HEAD
```

---

## Resources

| Resource | Location |
|----------|----------|
| Full Guide | [BREAKING_CHANGES_V6.md](../BREAKING_CHANGES_V6.md) |
| FAQ | [V6_MIGRATION_FAQ.md](V6_MIGRATION_FAQ.md) |
| Quick Start | [V6_MIGRATION_QUICK_START.md](V6_MIGRATION_QUICK_START.md) |
| Script | `./scripts/migrate_to_v6.sh` |
| Changelog | [CHANGELOG.md](../CHANGELOG.md) |
| API Docs | [docs.rs/ggen-core](https://docs.rs/ggen-core) |

---

## Decision Tree

```
Do you use `ggen` CLI only?
├─ YES → Just upgrade: `cargo install ggen-cli@6.0.0` ✅
└─ NO → Do you import `ggen_core::types` in Rust code?
    ├─ YES → Run migration script → Test → Commit ⚠️
    └─ NO → Just upgrade ✅
```

---

**Print this page and keep it handy during migration!**

**Last Updated**: 2026-01-09 | **Version**: 6.0.0
