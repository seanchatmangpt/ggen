# Documentation Mura Inventory

**Generated**: $(date)
**Purpose**: Identify files that haven't been updated to follow documentation and doctest standards

## Summary Statistics

- **Total Public APIs**: 1,148
- **Total Documentation Lines**: 11,751
- **Total Doctests**: 325
- **Non-runnable Doctests (no_run)**: 244 (75%)
- **Ignored Doctests**: 18 (6%)
- **Runnable Doctests**: 63 (19%)

## Documentation Coverage

- **Documentation Coverage**: ~73% (estimated based on doc lines vs public APIs)
- **Doctest Coverage**: ~28% (325 doctests / 1,148 public APIs)
- **Target Coverage**: 100% documentation, 100% doctests for public APIs

## Categories of Inconsistencies

### 1. Files Without Module-Level Documentation

**Issue**: Files with public APIs but no `//!` module-level documentation.

**Affected Files** (Library Code Only):
- `crates/ggen-core/src/lifecycle/hooks.rs`
- `crates/ggen-core/src/lifecycle/template_phase.rs`
- `crates/ggen-ai/src/config/global.rs`
- `crates/ggen-ai/src/providers/adapter.rs`
- `crates/ggen-ai/src/ultrathink/core.rs`
- `crates/ggen-ai/src/ultrathink/mod.rs`
- `crates/ggen-ai/src/cli.rs`
- `crates/ggen-cli/src/runtime.rs`
- `crates/ggen-cli/src/cmds/mod.rs`
- `crates/ggen-cli/src/conventions/presets/mod.rs`
- `crates/ggen-node/src/lib.rs`
- `crates/ggen-domain/src/marketplace/publish.rs`
- `crates/ggen-domain/src/marketplace/validate.rs`
- `crates/ggen-domain/src/graph/export.rs`

**Action Required**: Add module-level `//!` documentation following standard format.

### 2. Public Types Without Documentation

**Issue**: Public structs and enums without `///` documentation.

**Affected Files** (Library Code Only):
- Multiple structs in example files (lower priority)
- Need to audit main library code for undocumented types

**Action Required**: Add `///` documentation for all public types.

### 3. Documentation Without Doctests

**Issue**: Files with documentation but no doctest examples.

**Affected Files** (Library Code Only):
- `crates/ggen-core/src/lifecycle/hooks.rs`
- `crates/ggen-core/src/lifecycle/template_phase.rs`
- `crates/ggen-ai/src/config/global.rs`
- `crates/ggen-ai/src/providers/adapter.rs`
- `crates/ggen-ai/src/ultrathink/core.rs`
- `crates/ggen-ai/src/ultrathink/mod.rs`
- `crates/ggen-ai/src/cli.rs`
- `crates/ggen-cli/src/runtime.rs`
- `crates/ggen-cli/src/cmds/mod.rs`
- `crates/ggen-cli/src/conventions/presets/mod.rs`
- `crates/ggen-node/src/lib.rs`
- `crates/ggen-domain/src/marketplace/publish.rs`
- `crates/ggen-domain/src/marketplace/validate.rs`
- `crates/ggen-domain/src/graph/export.rs`

**Action Required**: Add doctest examples following standard format.

### 4. Doctest Format Inconsistencies

**Issue**: Doctests that use `Result` types but don't follow the standard `# fn main() -> anyhow::Result<()> {` pattern.

**Affected Files**:
- `crates/ggen-core/src/registry.rs`
- `crates/ggen-core/src/lifecycle/state_machine.rs`
- `crates/ggen-core/src/lifecycle/poka_yoke.rs`
- `crates/ggen-core/src/lifecycle/state_validation.rs`
- `crates/ggen-core/src/lifecycle/model.rs`
- `crates/ggen-core/src/cli_generator/dx.rs`
- `crates/ggen-core/src/project_generator/mod.rs`
- `crates/ggen-core/src/cleanroom/policy.rs`
- `crates/ggen-core/src/cleanroom/forensics.rs`
- `crates/ggen-core/src/cleanroom/mod.rs`
- `crates/ggen-core/src/rdf/schema.rs`
- `crates/ggen-utils/src/logger.rs`
- `crates/ggen-utils/src/types.rs`
- `crates/ggen-utils/src/error.rs`
- `crates/ggen-utils/src/lib.rs`
- `crates/ggen-utils/src/alert.rs`
- `crates/ggen-utils/src/app_config.rs`
- `crates/ggen-marketplace/src/template_search.rs`
- `crates/ggen-marketplace/src/cache/mod.rs`
- `crates/ggen-marketplace/src/traits/registry.rs`

**Action Required**: Update doctests to use standard `# fn main() -> anyhow::Result<()> {` pattern with `# Ok(())` at end.

### 5. Missing Error Case Examples

**Issue**: Fallible operations documented but missing error case examples in doctests.

**Action Required**: Audit all `Result`-returning functions and add error case examples.

## Priority Ranking

### High Priority (Core Library Code)
1. **Doctest Format Inconsistencies** (20 files) - Easy fixes, high impact
2. **Documentation Without Doctests** (14 files) - Core functionality needs examples
3. **Files Without Module-Level Documentation** (14 files) - Missing architecture overview

### Medium Priority
4. **Public Types Without Documentation** - Need to audit and document
5. **Missing Error Case Examples** - Need to audit all Result-returning functions

### Low Priority
6. **Example Files** - Lower priority, but should still follow standards

## Next Steps

1. **Immediate**: Fix doctest format inconsistencies (20 files)
2. **Short-term**: Add doctests to documented files without examples (14 files)
3. **Medium-term**: Add module-level documentation (14 files)
4. **Long-term**: Complete documentation audit for all public APIs

## Measurement Commands

```bash
# Run analysis
./scripts/analyze-documentation-mura.sh

# Verify doctests compile
cargo test --doc

# Verify documentation builds
cargo make docs

# Count undocumented APIs
grep -r "^pub fn\|^pub struct\|^pub enum\|^pub trait" crates/ --include="*.rs" | wc -l
grep -r "^///\|^//!" crates/ --include="*.rs" | wc -l
```

## Reference Implementation

See `.cursor/commands/eliminate-mura.md` for complete documentation standards and doctest patterns.

