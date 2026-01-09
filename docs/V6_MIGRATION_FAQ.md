# v6.0 Migration FAQ

## Quick Reference

| Question | Answer |
|----------|--------|
| Do CLI users need to change anything? | **No** - All commands work identically |
| Do library users need to change imports? | **Yes** - Update import paths (see below) |
| Will my templates break? | **No** - Template syntax unchanged |
| Will my SPARQL queries break? | **No** - Query syntax unchanged |
| Will my CI/CD pipelines break? | **Maybe** - Only if you use `ggen-core` types directly |
| How long is v5.1.0 supported? | 3 months after v6.0.0 release (security fixes only) |

---

## For CLI Users

### Q: I only use `ggen` CLI commands. Do I need to do anything?

**A**: No! Simply upgrade and continue using ggen as normal.

```bash
# Upgrade
cargo install ggen-cli@6.0.0

# All your existing commands work
ggen sync
ggen ai generate -d "API for user management"
ggen paas generate-k8s schema/
```

### Q: Will my `ggen.toml` configuration file still work?

**A**: Yes. The configuration format is 100% unchanged. No modifications needed.

### Q: Will my Tera templates still work?

**A**: Yes. Template syntax is unchanged. All your `.tera` files work as-is.

### Q: Will my SPARQL queries still work?

**A**: Yes. SPARQL syntax is unchanged. All your `.rq` files work as-is.

### Q: What are the new features I can try?

**A**:
- `ggen paas` commands for infrastructure generation (Kubernetes, Terraform, Docker)
- Enhanced `ggen ai` commands with multi-provider support
- Quality gates and Andon signals for better error prevention

---

## For Library Users

### Q: I use `ggen-core` as a library. What do I need to change?

**A**: You must update import paths for types that moved from `types` to `protection` and `signals` modules.

**Affected types**:
- `ProtectedPath`
- `PathError`
- `PathProtectionError`
- `PathProtector`
- `GlobPattern`

**Migration**:
```rust
// OLD (v5.1.0)
use ggen_core::types::{ProtectedPath, PathProtectionError};

// NEW (v6.0.0)
use ggen_core::protection::{ProtectedPath, PathProtectionError};
```

### Q: Is there an automated migration tool?

**A**: Yes! Run this script in your project root:

```bash
./scripts/migrate_to_v6.sh
```

Or manually:

```bash
# macOS (requires gnu-sed)
find crates/ -name "*.rs" -type f -exec gsed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +

# Linux
find crates/ -name "*.rs" -type f -exec sed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +

# Verify
cargo check
```

### Q: Can I still use root re-exports like `use ggen_core::ProtectedPath;`?

**A**: Yes! Root re-exports still work for backward compatibility. However, we recommend explicit module imports for better IDE support and clarity.

```rust
// Both work in v6.0.0
use ggen_core::ProtectedPath;            // ✅ Root re-export (legacy)
use ggen_core::protection::ProtectedPath; // ✅ Explicit import (recommended)
```

### Q: Will I get deprecation warnings?

**A**: No. There are no deprecation warnings. The upgrade is clean.

### Q: Did any function signatures change?

**A**: No. All public API function signatures remain unchanged.

### Q: Did any traits change?

**A**: No. All trait definitions remain unchanged.

---

## For CI/CD Pipelines

### Q: Will my GitHub Actions / GitLab CI break?

**A**: Only if you:
1. Use `ggen-core` as a library AND
2. Import types from `ggen_core::types` module

If you only use CLI commands (`ggen sync`, etc.), your pipelines will continue to work.

### Q: How do I update my CI pipeline?

**A**: Update your Rust code following the import migration guide, then commit:

```yaml
# .github/workflows/ci.yml
- name: Migrate to v6.0.0
  run: ./scripts/migrate_to_v6.sh

- name: Verify migration
  run: cargo check

- name: Run tests
  run: cargo make test
```

---

## Build and Performance

### Q: Why did my build time increase?

**A**: v6.0.0 adds 7 new workspace crates (AI, ETL, orchestration, etc.). This increases full workspace build time by ~20-30%.

**Mitigation**:
```bash
# Use incremental builds during development
cargo make check        # <5s
cargo test -p ggen-core # Test specific crate

# Full build for CI only
cargo make pre-commit   # <2min
```

### Q: Can I disable new features to reduce build time?

**A**: Yes! Use feature flags:

```toml
# Cargo.toml - Minimal build
[dependencies]
ggen-core = { version = "6.0", default-features = false }
```

### Q: Will runtime performance change?

**A**: No. Code generation performance is unchanged. Only build times are affected.

---

## Common Errors

### Error: "cannot find type `ProtectedPath` in crate `ggen_core::types`"

**Fix**:
```rust
// Change:
use ggen_core::types::ProtectedPath;

// To:
use ggen_core::protection::ProtectedPath;
```

### Error: "unresolved import `ggen_core::types::PathProtectionError`"

**Fix**:
```rust
use ggen_core::protection::PathProtectionError;
```

### Error: "cannot find `AndonSignal` in crate `ggen_core`"

**Fix**:
```rust
// This is a NEW type in v6.0.0
use ggen_core::signals::AndonSignal;
```

### Error: Build takes much longer than before

**Fix**:
```bash
# Use sparse registry
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse

# Build incrementally
cargo make check  # Fast feedback
```

---

## Rollback and Support

### Q: Can I roll back to v5.1.0 if something goes wrong?

**A**: Yes. Downgrade at any time:

```bash
cargo install ggen-cli@5.1.0
```

Your code will continue to work with v5.1.0.

### Q: How long will v5.1.0 be supported?

**A**: 3 months after v6.0.0 release for security fixes. No new features.

| Support Type | v5.1.0 Timeline |
|--------------|-----------------|
| Full support | Until v6.0.0 release |
| Security fixes only | 3 months post-v6.0.0 |
| End of life | Month 4 post-v6.0.0 |

### Q: What if I find a bug in v6.0.0?

**A**: Please [file an issue](https://github.com/seanchatmangpt/ggen/issues) with:
1. Your version: `ggen --version`
2. Error message (full output)
3. Minimal reproduction steps
4. What you expected to happen

We prioritize migration-related bugs.

---

## Advanced Topics

### Q: I use wildcard imports like `use ggen_core::types::*;`. What happens?

**A**: Wildcard imports from `types` will still include enterprise types (FMEA, EnterpriseConfig, etc.) but NOT path protection types.

**Fix**:
```rust
// OLD (incomplete in v6.0.0)
use ggen_core::types::*;

// NEW (complete)
use ggen_core::types::*;
use ggen_core::protection::*;
use ggen_core::signals::*;
```

**Recommendation**: Avoid wildcard imports. Use explicit imports for better IDE support.

### Q: I maintain a library that depends on `ggen-core`. How do I handle the transition?

**A**: Use version ranges and document the breakage:

```toml
# Your library's Cargo.toml
[dependencies]
ggen-core = "6.0"  # v6 only

# OR support both versions
ggen-core = ">= 5.1, < 7"
```

Document in your CHANGELOG:
```markdown
## [Your Version]
### Breaking Changes
- Updated ggen-core to v6.0.0
- Users must update imports: `ggen_core::types::ProtectedPath` → `ggen_core::protection::ProtectedPath`
```

### Q: What if I'm using a custom type that wraps `ProtectedPath`?

**A**: Update your imports and re-export:

```rust
// Your library code
use ggen_core::protection::ProtectedPath;  // Updated import

pub struct MyCustomPath(ProtectedPath);

// Users of your library
use your_lib::MyCustomPath;  // No change needed for users
```

---

## Testing Your Migration

### Q: How do I verify my migration before deploying?

**A**: Run the complete test suite:

```bash
# 1. Check compilation
cargo make check

# 2. Run all tests
cargo make test

# 3. Lint checks
cargo make lint

# 4. Full pre-commit validation
cargo make pre-commit

# 5. Integration tests (if applicable)
cargo test --test '*'
```

### Q: Can I test v6.0.0 in a separate branch?

**A**: Yes! Recommended workflow:

```bash
# Create migration branch
git checkout -b migrate-to-v6

# Run migration
./scripts/migrate_to_v6.sh

# Review changes
git diff

# Test thoroughly
cargo make test

# If successful, merge to main
git checkout main
git merge migrate-to-v6
```

---

## Getting Help

### Q: Where can I ask questions about migration?

**A**:
- **GitHub Discussions**: [Ask questions](https://github.com/seanchatmangpt/ggen/discussions)
- **GitHub Issues**: [Report bugs](https://github.com/seanchatmangpt/ggen/issues)
- **API Docs**: [docs.rs/ggen-core](https://docs.rs/ggen-core)

### Q: I'm stuck. Can I get 1-on-1 help?

**A**: Open a [GitHub Discussion](https://github.com/seanchatmangpt/ggen/discussions) with:
1. Your `ggen --version` output
2. Full error message
3. What you've tried
4. Minimal code sample

Community and maintainers will assist.

---

## Summary Checklist

Use this checklist to ensure smooth migration:

### CLI Users
- [ ] Upgrade to v6.0.0: `cargo install ggen-cli@6.0.0`
- [ ] Verify commands work: `ggen sync`
- [ ] (Optional) Try new features: `ggen paas`, `ggen ai`

### Library Users
- [ ] Read import migration guide
- [ ] Run migration script: `./scripts/migrate_to_v6.sh`
- [ ] Verify compilation: `cargo check`
- [ ] Run tests: `cargo make test`
- [ ] Review and commit changes

### CI/CD
- [ ] Update pipelines if using `ggen-core` types
- [ ] Test pipeline on migration branch
- [ ] Update documentation
- [ ] Deploy after successful tests

### All Users
- [ ] Review [BREAKING_CHANGES_V6.md](../BREAKING_CHANGES_V6.md)
- [ ] Check [CHANGELOG.md](../CHANGELOG.md) for full details
- [ ] Bookmark [Migration FAQ](../docs/V6_MIGRATION_FAQ.md) (this document)

---

**Last Updated**: 2026-01-09
**ggen Version**: 6.0.0
**Support**: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
