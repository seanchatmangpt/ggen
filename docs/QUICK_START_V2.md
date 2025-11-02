# ggen v2.0 Quick Start Guide

## What Changed?

**TLDR:** v2.0 adds async architecture foundations. **No breaking changes!** Your existing code works unchanged.

## New Architecture

```
CLI (sync) → Domain (async) → Runtime (services)
   30 LOC      200 LOC          existing code
```

## For Template Authors

### Using RDF Frontmatter? ⚠️

Your templates still work, but you'll see deprecation warnings:

```yaml
---
rdf_inline: "ex:Data ..."  # ⚠️ Deprecated in v2.0, removed in v2.1
sparql: "SELECT ..."       # ⚠️ Deprecated in v2.0, removed in v2.1
---
```

**Migration (before v2.1 - Feb 2026):**

```bash
# Option 1: Use ggen graph commands
ggen graph load data.ttl
ggen graph query "SELECT ..." > results.json
ggen template generate my-template --var results=@results.json

# Option 2: Pre-processor script
# See docs/v2-rdf-deprecation-plan.md
```

### Not Using RDF? ✅

Zero changes needed! Templates work exactly as before.

## For Developers

### Writing New Commands

Use the three-layer pattern:

**1. CLI Wrapper** (`cli/src/cmds/<noun>/<verb>.rs`):
```rust
use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct MyArgs {
    pub input: String,
}

pub async fn run(args: &MyArgs) -> Result<()> {
    let result = crate::domain::my_operation(&args.input).await?;
    println!("✓ {}", result);
    Ok(())
}
```

**2. Domain Logic** (`cli/src/domain/<noun>/<operation>.rs`):
```rust
use ggen_utils::error::Result;

pub async fn my_operation(input: &str) -> Result<String> {
    // Business logic here
    Ok(format!("Processed: {}", input))
}
```

**3. Tests** (`cli/tests/domain/<noun>/<operation>_tests.rs`):
```rust
#[tokio::test]
async fn test_my_operation() {
    let result = my_operation("test").await;
    assert!(result.is_ok());
}
```

See `docs/v2-command-migration-template.md` for complete patterns.

## Documentation

- **Migration Template**: `docs/v2-command-migration-template.md`
  - 3 command patterns with complete examples
  - Testing strategies
  - 77-command migration roadmap

- **RDF Deprecation**: `docs/v2-rdf-deprecation-plan.md`
  - Timeline and migration paths
  - Impact analysis
  - Migration script examples

- **Implementation Guide**: `docs/v2-implementation-guide.md`
  - Complete architecture overview
  - Best practices
  - Common pitfalls

- **Summary**: `docs/v2-sparc-coder-implementation-summary.md`
  - Complete implementation details
  - Statistics and metrics
  - Next steps

## Timeline

- **v2.0 (Nov 2025)** - Current
  - ✅ Async foundations
  - ⚠️ RDF deprecation warnings
  - ✅ 20 core commands migrated
  - ✅ No breaking changes

- **v2.1 (Feb 2026)**
  - ❌ RDF frontmatter removed (BREAKING)
  - ✅ All 77 commands migrated
  - ⚠️ Old `commands/` module deprecated

- **v2.2 (May 2026)**
  - ❌ Old `commands/` module removed
  - ✅ Clean architecture
  - 🚀 Performance optimizations

## Build Status

```bash
$ cargo build --package ggen-cli-lib
    Finished dev [unoptimized + debuginfo] target(s)
```

✅ Compiles successfully
✅ All existing tests pass
✅ Zero new errors

## Examples

### Existing Command (No Changes)
```rust
// This still works!
use ggen_cli::commands::template::GenerateArgs;
```

### New Command (v2.0 Pattern)
```rust
// New pattern
use ggen_cli::cmds::template::TemplateCmd;
use ggen_cli::domain::template;
```

## Need Help?

1. **Templates & RDF**: See `docs/v2-rdf-deprecation-plan.md`
2. **New Commands**: See `docs/v2-command-migration-template.md`
3. **Architecture**: See `docs/v2-implementation-guide.md`
4. **Complete Details**: See `docs/v2-sparc-coder-implementation-summary.md`

## Questions?

- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Documentation: `docs/v2-*.md`
- Examples: `cli/src/cmds/shell/completion.rs`

---

**Bottom Line:**
- ✅ Your code works unchanged
- ⚠️ Migrate RDF templates before v2.1 (Feb 2026)
- 📚 Complete docs provided
- 🎯 Clear migration path
