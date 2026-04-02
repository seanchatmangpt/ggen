# ggen v2.0.0 Deprecation Plan

**Status**: Active (v2.0.0 - v2.2.0)
**Last Updated**: 2025-11-01

## Executive Summary

This document outlines the deprecation strategy for transitioning from ggen v1.x architecture to the new v2.0.0 three-layer architecture (CLI → Domain → Runtime). The plan follows a phased approach with clear migration paths and deprecation timelines.

## Deprecation Timeline

```
v2.0.0 (Current)         v2.1.0 (Feb 2026)      v2.2.0 (May 2026)
    │                         │                       │
    ├─ Deprecation warnings   ├─ Remove old code     ├─ Clean slate
    ├─ New architecture       ├─ Migration complete   └─ Full v2 API
    └─ Both systems work      └─ Only new system
```

### Phase 1: v2.0.0 (Current - November 2025)
**Status**: Active deprecation warnings, both systems functional

- ✅ New three-layer architecture active (`cmds/`, `domain/`, runtime)
- ⚠️ Old `commands/` directory marked deprecated
- ⚠️ Deprecation warnings added to old modules
- 📝 Migration guide published
- 🔄 Both old and new systems work side-by-side

### Phase 2: v2.1.0 (February 2026)
**Status**: Planned removal

- 🗑️ Remove `cli/src/commands/` directory entirely
- 🗑️ Remove old domain logic embedded in commands
- ✅ All functionality migrated to new architecture
- 📝 Breaking change notices in release notes
- 🔄 Final migration assistance tools

### Phase 3: v2.2.0 (May 2026)
**Status**: Clean slate

- ✅ Fully migrated to v2 architecture
- ✅ No legacy code remains
- ✅ Documentation updated to reflect only v2 patterns
- 🎉 Performance improvements from cleanup

## Deprecated Components

### 1. Old Command Structure (`cli/src/commands/`)

**Deprecated In**: v2.0.0
**Removal Date**: v2.1.0 (February 2026)
**Replacement**: `cli/src/cmds/` with domain layer separation

#### Affected Files:
```
cli/src/commands/
├── ai/
│   ├── mod.rs (DEPRECATED)
│   └── generate.rs (DEPRECATED)
├── template/
│   ├── mod.rs (DEPRECATED)
│   └── generate.rs (DEPRECATED)
├── project/
│   ├── mod.rs (DEPRECATED)
│   └── gen.rs (DEPRECATED)
├── marketplace/
│   ├── mod.rs (DEPRECATED)
│   └── search.rs (DEPRECATED)
└── utils/
    └── mod.rs (DEPRECATED - partially)
```

#### Migration Path:
```rust
// OLD (v1.x - DEPRECATED)
use ggen_cli::commands::template::GenerateArgs;

// NEW (v2.0.0+)
use ggen_cli::cmds::template::TemplateCmd;
```

**Why Deprecated**:
- Violates single responsibility principle (CLI + domain logic mixed)
- Makes testing difficult (can't test business logic without CLI)
- Increases compilation time (tightly coupled modules)
- Prevents code reuse (business logic tied to CLI presentation)

### 2. Embedded Domain Logic

**Deprecated In**: v2.0.0
**Removal Date**: v2.1.0
**Replacement**: `cli/src/domain/` layer

#### Pattern:
```rust
// OLD (DEPRECATED) - Domain logic in command
pub async fn run(args: &Args) -> Result<()> {
    // CLI presentation + business logic mixed
    let result = do_business_logic()?;
    println!("Result: {}", result);
    Ok(())
}

// NEW (v2.0.0+) - Separated concerns
// In cli/src/cmds/foo.rs
pub async fn run(&self) -> Result<()> {
    let result = domain::foo::execute(self.into())?;
    self.present(result);
    Ok(())
}

// In cli/src/domain/foo.rs
pub fn execute(params: Params) -> Result<Output> {
    // Pure business logic, testable
    Ok(Output { ... })
}
```

### 3. TODO Comments and Placeholders

**Deprecated In**: v2.0.0
**Removal Date**: v2.1.0
**Replacement**: Proper error handling and implementation

#### Current TODO Items:
```rust
// cli/src/cmds/hook/remove.rs
// TODO: Check if hook exists → Implement validation
// TODO: Implement actual confirmation prompt → Add confirmation
// TODO: Implement actual hook removal → Full implementation

// cli/src/cmds/hook/list.rs
// TODO: Load hooks from .ggen/hooks/ directory → Implement loading

// cli/src/cmds/hook/create.rs
// TODO: Implement actual hook installation → Full implementation

// cli/src/cmds/hook/run.rs
// TODO: Implement actual hook execution → Full implementation
```

**Migration**: All TODOs will be implemented or removed by v2.1.0.

### 4. Unsafe Error Handling

**Deprecated In**: v2.0.0
**Removal Date**: v2.1.0
**Replacement**: Proper Result/Option handling

#### Current Issues:
- 2 `.expect()` calls in cmds/ (down from 15 in v1.x)
- 30 `unwrap()` calls in cmds/ (target: 0)

**Migration**:
```rust
// OLD (DEPRECATED)
let value = some_option.unwrap();  // Panics on None

// NEW (v2.0.0+)
let value = some_option.ok_or_else(|| Error::MissingValue)?;
```

## Migration Assistance

### For Users

**No Action Required**: v2.0.0 is fully backward compatible. Deprecation warnings will guide you to new patterns.

```bash
# Old commands still work with deprecation warnings
ggen template generate  # Warning: Use 'ggen template new' instead

# New commands (recommended)
ggen template new
```

### For Contributors

**Action Required**: Follow new architecture patterns for all new code.

#### New Module Pattern:
```rust
// 1. CLI Layer (cli/src/cmds/foo/mod.rs)
pub struct FooCmd {
    #[arg(long)]
    pub bar: String,
}

impl FooCmd {
    pub async fn run(&self) -> Result<()> {
        let params = self.into();
        let result = domain::foo::execute(params).await?;
        self.present(result);
        Ok(())
    }
}

// 2. Domain Layer (cli/src/domain/foo/mod.rs)
pub fn execute(params: Params) -> Result<Output> {
    // Pure business logic
    Ok(Output { ... })
}

// 3. Runtime Layer (uses ggen-core, ggen-ai, etc.)
// No direct implementation - use existing runtime crates
```

#### Testing Pattern:
```rust
// Unit tests - domain layer (fast, isolated)
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_business_logic() {
        let result = execute(Params::default()).unwrap();
        assert_eq!(result.status, "success");
    }
}

// Integration tests - CLI layer (slower, full stack)
#[test]
fn test_cli_integration() {
    let cmd = FooCmd { bar: "test".into() };
    let result = cmd.run().await.unwrap();
    assert!(result.is_ok());
}
```

## Cleanup Checklist

### v2.0.0 (Current)
- [x] Create deprecation plan
- [x] Add deprecation warnings to old modules
- [x] Document new architecture
- [x] Publish migration guide
- [x] Ensure both systems work

### v2.1.0 (February 2026)
- [ ] Remove `cli/src/commands/` directory
- [ ] Remove all TODO comments (implement or delete)
- [ ] Replace all `unwrap()` with proper error handling
- [ ] Remove all `.expect()` calls
- [ ] Update all tests to new architecture
- [ ] Remove deprecation warnings (code is gone)
- [ ] Final migration assistance tools
- [ ] Update CHANGELOG.md

### v2.2.0 (May 2026)
- [ ] Clean slate validation
- [ ] Performance audit (should be faster without legacy code)
- [ ] Documentation audit (remove v1 references)
- [ ] Celebrate clean architecture!

## Breaking Changes

### v2.1.0
**Module Removals**:
- `ggen_cli::commands::*` → Use `ggen_cli::cmds::*`
- Embedded domain logic → Use `ggen_cli::domain::*`

**Command Changes**: None (CLI interface remains the same)

### v2.2.0
**No Breaking Changes**: Just cleanup and performance improvements.

## Performance Impact

### Expected Improvements (v2.1.0)
- **Build Time**: -10% (remove dead code, cleaner dependencies)
- **Binary Size**: -5% (less code to compile)
- **Test Time**: -15% (better test isolation)
- **Memory Usage**: -5% (fewer allocations)

## Communication Plan

### v2.0.0 Release (Current)
- [x] Deprecation warnings in code
- [x] Migration guide in docs
- [x] Announcement in README
- [x] GitHub release notes

### v2.1.0 Pre-Release (January 2026)
- [ ] Blog post: "Preparing for v2.1.0"
- [ ] Email to contributors
- [ ] Deprecation removal timeline
- [ ] Final migration checklist

### v2.1.0 Release (February 2026)
- [ ] Breaking change announcement
- [ ] Updated migration guide
- [ ] Performance benchmarks
- [ ] "What's New" blog post

### v2.2.0 Release (May 2026)
- [ ] Clean architecture celebration
- [ ] Performance comparison blog
- [ ] Architecture deep dive

## FAQ

**Q: Will my existing code break in v2.0.0?**
A: No. v2.0.0 is fully backward compatible with deprecation warnings.

**Q: When should I migrate to the new architecture?**
A: Immediately for new code. Existing code can wait until v2.1.0 (February 2026).

**Q: What if I'm still using old patterns in v2.1.0?**
A: Your code will not compile. Migrate before v2.1.0 or stay on v2.0.x.

**Q: Can I help with the migration?**
A: Yes! See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

**Q: What's the priority for cleanup?**
A: Focus on the 20% of code that causes 80% of confusion:
   1. Remove `commands/` directory stubs
   2. Implement all TODOs in `cmds/hook/`
   3. Replace unwrap() in error paths
   4. Update tests for new architecture

## References

- [Architecture v2.0.0 Documentation](../ARCHITECTURE_V2.md)
- [Migration Guide v1 to v2](../MIGRATION_V1_TO_V2.md)
- [Three-Layer Architecture Pattern](../patterns/three-layer-architecture.md)
- [Testing Strategy](../testing/README.md)
- [Contributing Guidelines](../../CONTRIBUTING.md)

## Contact

Questions? Open an issue or discussion:
- Issues: https://github.com/seanchatmangpt/ggen/issues
- Discussions: https://github.com/seanchatmangpt/ggen/discussions

---

**Last Review**: 2025-11-01
**Next Review**: 2026-01-01 (v2.1.0 prep)
**Status**: Active Deprecation (v2.0.0)
