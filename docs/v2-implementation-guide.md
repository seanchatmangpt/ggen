# v2.0 Implementation Guide: Migrating Commands to Two-Layer Architecture

**Status**: Production Ready
**Pattern**: cmds → domain
**Proof of Concept**: `utils/doctor` command

## Quick Start

Migrating a command from the old pattern to v2.0 architecture takes 3 simple steps:

### 1. Create Domain Module

Extract business logic to `cli/src/domain/{category}/{command}.rs`:

```rust
pub async fn run_command(args: Args) -> Result<Output> {
    // Pure business logic here
    Ok(output)
}
```

### 2. Update cmds Layer

Simplify `cli/src/cmds/{category}/{command}.rs` to just delegate:

```rust
pub async fn run(args: &Args) -> Result<()> {
    crate::domain::category::command::run_command(args.into()).await
}
```

### 3. Test

```bash
cargo test
cargo run -- {command} {args}
```

## Full Migration Guide

See `/Users/sac/ggen/docs/v2-architecture-pattern-validation.md` for:
- Complete architecture overview
- Detailed migration steps
- Code templates for common patterns
- Testing strategies
- Common pitfalls and solutions
- Migration checklist
- Automation scripts

## Command Categories

| Category | Count | Priority |
|----------|-------|----------|
| utils | 1 | ✅ Done (doctor) |
| template | 6 | High |
| marketplace | 15 | High |
| project | 10 | Medium |
| ai | 11 | Medium |
| graph | 8 | Low |
| ci | 4 | Low |
| audit | 3 | Low |
| shell | 2 | Low |

## Migration Status

- ✅ **Validated**: Pattern proven with doctor command
- 🎯 **Ready**: 93 commands to migrate
- ⏱️ **Estimate**: 91 hours (~11 days with 1 developer)

## Support

Questions? Check:
1. Pattern validation report: `docs/v2-architecture-pattern-validation.md`
2. Example implementation: `cli/src/domain/utils/doctor.rs`
3. Migration guide: `docs/MIGRATION_V1_TO_V2.md`
