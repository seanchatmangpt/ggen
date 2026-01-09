# Breaking Changes in v6.0

**Version 6.0.0** introduces architectural improvements and new capabilities while maintaining backward compatibility for most users. This section documents what changed and how to migrate.

## For CLI Users: No Breaking Changes ✅

**Good news**: All v5.1.0 commands work identically in v6.0.0.

- All existing `ggen sync` workflows continue to work
- Configuration format (`ggen.toml`) unchanged
- Template syntax unchanged
- SPARQL query syntax unchanged
- No command-line flag changes

**What's New** (Optional):
- New `ggen paas` commands for infrastructure generation (opt-in feature flag)
- Enhanced AI capabilities via `ggen ai` commands
- Additional quality gates and validation hooks

**Migration**: None required. Simply upgrade and continue.

```bash
# Upgrade to v6.0.0
cargo install ggen-cli@6.0.0

# All your existing commands work
ggen sync
ggen ai generate -d "your description"
```

---

## For Library Users: Import Path Changes Required ⚠️

If you use `ggen-core` as a library dependency, you **must** update import paths.

### What Changed

Path protection and signal types were reorganized into dedicated modules for better discoverability and maintainability.

**Before (v5.1.0)**:
```rust
// ❌ REMOVED - No longer valid in v6.0.0
use ggen_core::types::{ProtectedPath, PathProtectionError};
use ggen_core::types::PathProtectionConfig;
```

**After (v6.0.0)**:
```rust
// ✅ NEW - Required import locations in v6.0.0
use ggen_core::protection::{ProtectedPath, PathProtectionError, PathProtector};
use ggen_core::signals::{AndonSignal, AndonContext};
```

### Complete Import Migration Table

| Type | v5.1.0 Location | v6.0.0 Location | Status |
|------|----------------|-----------------|--------|
| `ProtectedPath` | `ggen_core::types` | `ggen_core::protection` | **MOVED** |
| `PathError` | `ggen_core::types` | `ggen_core::protection` | **MOVED** |
| `PathProtectionError` | `ggen_core::types` | `ggen_core::protection` | **MOVED** |
| `PathProtector` | `ggen_core::types` | `ggen_core::protection` | **MOVED** |
| `GlobPattern` | `ggen_core::types` | `ggen_core::protection` | **MOVED** |
| `GeneratorPathGuard` | N/A | `ggen_core::protection` | **NEW** |
| `AndonSignal` | N/A | `ggen_core::signals` | **NEW** |
| `AndonContext` | N/A | `ggen_core::signals` | **NEW** |

**Note**: Types are still re-exported from `ggen_core` root for convenience, but explicit module imports are recommended.

### Automated Migration Script

Run this command in your project root to automatically fix import paths:

```bash
# macOS/BSD (requires gnu-sed: brew install gnu-sed)
find crates/ -name "*.rs" -type f -exec gsed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +

# Linux
find crates/ -name "*.rs" -type f -exec sed -i \
  's/use ggen_core::types::\(ProtectedPath\|PathError\|PathProtectionError\|PathProtector\|GlobPattern\)/use ggen_core::protection::\1/g' {} +

# Verify changes
cargo check
```

**Manual Alternative**:
1. Search for `use ggen_core::types::{ProtectedPath` in your codebase
2. Replace with `use ggen_core::protection::{ProtectedPath`
3. Run `cargo check` to verify compilation
4. Fix any remaining import errors following the table above

---

## For Workspace Builds: New Crates Added

**v6.0.0** introduces 7 new workspace crates for enhanced capabilities:

### New Crates
1. **ggen-ai** - AI orchestration and multi-provider LLM integration
2. **knhk-etl** - Extract-Transform-Load pipeline for data processing
3. **knhk-hot** - C FFI hot-path optimization for performance-critical code
4. **knhk-connectors** - Connector registry (Kafka, HTTP, gRPC, etc.)
5. **knhk-lockchain** - Merkle-linked receipt storage for audit trails
6. **knhk-otel** - OpenTelemetry integration for observability
7. **knhk-orchestrator** - Integration bridge (ETL → KGC-4D → Workflow)

### Build Impact

- **Full workspace build time**: +20-30% increase (due to 7 additional crates)
  - v5.1.0: ~30-45s
  - v6.0.0: ~40-60s
- **Incremental builds**: No significant impact (<2s difference)
- **Binary size**: +15% (additional AI and orchestration capabilities)

### Why the Increase?

These new systems enable:
- Multi-provider AI integration (OpenAI, Anthropic, Groq, Ollama, Cohere)
- ETL pipelines for RDF data transformation
- Provenance tracking with Merkle-linked receipts
- Production-grade observability with OpenTelemetry

### Mitigation Strategies

**Option 1**: Use incremental build targets (recommended for development)
```bash
# Fast feedback during development (<5s)
cargo make check

# Test only what you're working on
cargo test -p ggen-core

# Full validation before commit
cargo make pre-commit
```

**Option 2**: Disable optional features (reduce dependency graph)
```toml
# Cargo.toml - Disable AI features if not needed
[dependencies]
ggen-core = { version = "6.0", default-features = false }
```

**Option 3**: Use sparse registry for faster dependency resolution
```bash
# Enable sparse registry (Rust 1.68+)
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse
cargo build
```

---

## For Projects Using ggen-core Directly

### What Did NOT Change ✅

- **Function signatures**: All public APIs remain unchanged
- **Trait definitions**: No trait changes or new requirements
- **Data structures**: Public struct fields unchanged
- **Template format**: Tera template syntax unchanged
- **SPARQL queries**: Query syntax and semantics unchanged
- **RDF specification format**: Turtle (.ttl) syntax unchanged
- **Configuration**: `ggen.toml` format unchanged
- **CLI commands**: All command syntax unchanged
- **Generation pipeline**: Core pipeline logic unchanged

### What DID Change

**Module Organization Only**:
- Path protection moved to dedicated `protection` module
- Signals system introduced in new `signals` module
- Improved discoverability through logical grouping

**New Capabilities** (Non-Breaking):
- `AndonSignal` - Quality gate signals (NEW, opt-in)
- `AndonContext` - Context for andon signals (NEW, opt-in)
- `GeneratorPathGuard` - Enhanced path protection (NEW, opt-in)

**Backward Compatibility**:
- All v5.1.0 types still exported from `ggen_core` root
- No deprecation warnings (clean upgrade path)
- No runtime behavior changes

---

## Common Errors and Solutions

### Error 1: "cannot find type `ProtectedPath` in crate `ggen_core::types`"

**Cause**: Import path changed in v6.0.0

**Solution**:
```rust
// Change this:
use ggen_core::types::ProtectedPath;

// To this:
use ggen_core::protection::ProtectedPath;

// Or use root re-export:
use ggen_core::ProtectedPath;  // Still works
```

### Error 2: "unresolved import `ggen_core::types::PathProtectionError`"

**Cause**: Type moved to `protection` module

**Solution**:
```rust
use ggen_core::protection::PathProtectionError;
```

### Error 3: Build time increased significantly

**Cause**: 7 new workspace crates added

**Solution**:
```bash
# Use incremental targets during development
cargo make check        # <5s
cargo make test-unit    # <16s

# Full build only for CI/pre-commit
cargo make pre-commit   # <2min
```

### Error 4: "cannot find `AndonSignal` in crate `ggen_core`"

**Cause**: New type in v6.0.0 (not a breaking change, but may appear in updated examples)

**Solution**:
```rust
// Add new import for signals
use ggen_core::signals::{AndonSignal, AndonContext};
```

---

## Migration Timeline

| Phase | Timeframe | Action | Support |
|-------|-----------|--------|---------|
| **Phase 1**: Pre-Release | Now | Review breaking changes | Full support |
| **Phase 2**: v6.0.0 Release | Week 1 | Update imports, test builds | Full support |
| **Phase 3**: Transition | Weeks 2-8 | Complete migration | Full support |
| **Phase 4**: v5 Sunset | Month 3+ | v5.1.0 security fixes only | Limited support |

**Recommendation**: Migrate during Phase 2 (first week after release) to benefit from full community support and early bug fixes.

---

## Troubleshooting FAQ

**Q: Do I need to change my templates?**
A: No. All Tera templates work unchanged.

**Q: Do I need to change my SPARQL queries?**
A: No. SPARQL syntax is unchanged.

**Q: Do I need to change my `ggen.toml` configuration?**
A: No. Configuration format is unchanged.

**Q: Will my CI/CD pipelines break?**
A: No, if you only use CLI commands. Yes, if you import `ggen-core` types directly (follow import migration guide above).

**Q: Can I stay on v5.1.0?**
A: Yes. v5.1.0 will receive security fixes for 3 months after v6.0.0 release.

**Q: What if I encounter breaking changes not listed here?**
A: Please [file an issue](https://github.com/seanchatmangpt/ggen/issues) immediately. We maintain strict backward compatibility and any unlisted breaking change is considered a bug.

**Q: How do I test my migration before committing?**
A: Run the full test suite:
```bash
cargo make check      # Fast compilation check
cargo make test       # Full test suite
cargo make lint       # Style and quality checks
cargo make pre-commit # Complete validation
```

---

## Support Resources

- **Migration Guide**: This section
- **Changelog**: [CHANGELOG.md](CHANGELOG.md) - Complete version history
- **GitHub Issues**: [Report problems](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [Ask questions](https://github.com/seanchatmangpt/ggen/discussions)
- **API Documentation**: [docs.rs/ggen-core](https://docs.rs/ggen-core) - Updated API reference

**Need Help?** Open a [GitHub Discussion](https://github.com/seanchatmangpt/ggen/discussions) with:
1. Your current version (`ggen --version`)
2. Error message (full output)
3. Minimal reproduction steps
4. What you've tried so far
