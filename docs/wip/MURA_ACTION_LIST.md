# Documentation Mura - Action List

**Generated**: $(date)
**Status**: Files that haven't been updated to follow documentation standards

## Quick Summary

- **Total Issues Found**: 48 files
- **High Priority**: 20 files (doctest format inconsistencies)
- **Medium Priority**: 14 files (missing doctests)
- **Low Priority**: 14 files (missing module-level docs)

## High Priority: Doctest Format Inconsistencies ✅ COMPLETE

**Action**: Update doctests to use standard `# fn main() -> anyhow::Result<()> {` pattern.

### ggen-core ✅
- [x] `crates/ggen-core/src/registry.rs` - Already had correct pattern
- [x] `crates/ggen-core/src/lifecycle/state_machine.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/lifecycle/poka_yoke.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/lifecycle/state_validation.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/lifecycle/model.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/cli_generator/dx.rs` - No Result doctests needed
- [x] `crates/ggen-core/src/project_generator/mod.rs` - Already had correct pattern
- [x] `crates/ggen-core/src/cleanroom/policy.rs` - ✅ Fixed (2 doctests)
- [x] `crates/ggen-core/src/cleanroom/forensics.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/cleanroom/mod.rs` - ✅ Fixed
- [x] `crates/ggen-core/src/rdf/schema.rs` - No Result doctests needed

### ggen-utils ✅
- [x] `crates/ggen-utils/src/logger.rs` - Already had correct pattern
- [x] `crates/ggen-utils/src/types.rs` - Already had correct pattern
- [x] `crates/ggen-utils/src/error.rs` - ✅ Fixed
- [x] `crates/ggen-utils/src/lib.rs` - ✅ Fixed
- [x] `crates/ggen-utils/src/alert.rs` - No Result doctests needed
- [x] `crates/ggen-utils/src/app_config.rs` - Already had correct pattern

### ggen-marketplace ✅
- [x] `crates/ggen-marketplace/src/template_search.rs` - Already had correct pattern
- [x] `crates/ggen-marketplace/src/cache/mod.rs` - Already had correct pattern
- [x] `crates/ggen-marketplace/src/traits/registry.rs` - Already had correct pattern

## Medium Priority: Documentation Without Doctests

**Action**: Add doctest examples following standard format.

### ggen-core
- [ ] `crates/ggen-core/src/lifecycle/hooks.rs`
- [ ] `crates/ggen-core/src/lifecycle/template_phase.rs`

### ggen-ai
- [ ] `crates/ggen-ai/src/config/global.rs`
- [ ] `crates/ggen-ai/src/providers/adapter.rs`
- [ ] `crates/ggen-ai/src/ultrathink/core.rs`
- [ ] `crates/ggen-ai/src/ultrathink/mod.rs`
- [ ] `crates/ggen-ai/src/cli.rs`

### ggen-cli
- [ ] `crates/ggen-cli/src/runtime.rs`
- [ ] `crates/ggen-cli/src/cmds/mod.rs`
- [ ] `crates/ggen-cli/src/conventions/presets/mod.rs`

### ggen-node
- [ ] `crates/ggen-node/src/lib.rs`

### ggen-domain
- [ ] `crates/ggen-domain/src/marketplace/publish.rs`
- [ ] `crates/ggen-domain/src/marketplace/validate.rs`
- [ ] `crates/ggen-domain/src/graph/export.rs`

## Low Priority: Missing Module-Level Documentation

**Action**: Add `//!` module-level documentation with architecture overview.

### ggen-core
- [ ] `crates/ggen-core/src/lifecycle/hooks.rs`
- [ ] `crates/ggen-core/src/lifecycle/template_phase.rs`

### ggen-ai
- [ ] `crates/ggen-ai/src/config/global.rs`
- [ ] `crates/ggen-ai/src/providers/adapter.rs`
- [ ] `crates/ggen-ai/src/ultrathink/core.rs`
- [ ] `crates/ggen-ai/src/ultrathink/mod.rs`
- [ ] `crates/ggen-ai/src/cli.rs`

### ggen-cli
- [ ] `crates/ggen-cli/src/runtime.rs`
- [ ] `crates/ggen-cli/src/cmds/mod.rs`
- [ ] `crates/ggen-cli/src/conventions/presets/mod.rs`

### ggen-node
- [ ] `crates/ggen-node/src/lib.rs`

### ggen-domain
- [ ] `crates/ggen-domain/src/marketplace/publish.rs`
- [ ] `crates/ggen-domain/src/marketplace/validate.rs`
- [ ] `crates/ggen-domain/src/graph/export.rs`

## Verification Commands

After fixing files, verify with:

```bash
# Verify doctests compile and run
cargo test --doc

# Verify documentation builds
cargo make docs

# Re-run analysis
./scripts/analyze-documentation-mura.sh
```

## Standard Patterns

See `.cursor/commands/eliminate-mura.md` for complete documentation standards:

- **Module-level docs**: `//!` with architecture overview
- **Item-level docs**: `///` with arguments, returns, errors, examples
- **Doctest format**: `# fn main() -> anyhow::Result<()> { ... # Ok(()) }`
- **Error examples**: Include error case examples for fallible operations

