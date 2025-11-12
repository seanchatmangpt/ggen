# Type Safety Coding Standards

## Purpose

This document defines type safety standards to prevent compilation errors and ensure type-safe code patterns are followed consistently.

## Builder Pattern Usage

### Pattern: UnvalidatedPackage → ValidatedPackage → Package

When using the `Package::builder()` pattern:

```rust
// ✅ CORRECT: Follow validation chain
let unvalidated = Package::builder(id, version)
    .title("My Package")
    .description("Description")
    .build()?;

let validated = unvalidated.validate()?;
let package = validated.package().clone();

// Use package for operations
registry.publish(package).await?;
```

```rust
// ❌ WRONG: Skipping validation step
let package = Package::builder(id, version)
    .title("My Package")
    .build()?;

registry.publish(package).await?; // Type mismatch!
```

### Why This Pattern Exists

**Poka-yoke (Mistake-Proofing)**: The builder pattern intentionally returns `UnvalidatedPackage` to force validation before use. This prevents:
- Publishing invalid packages
- Installing broken packages
- Using packages without required fields

### Required Steps

1. **Build**: `builder.build()` → `UnvalidatedPackage`
2. **Validate**: `unvalidated.validate()` → `ValidatedPackage`
3. **Extract**: `validated.package().clone()` → `Package`
4. **Use**: Use `Package` for operations

## Type Safety Checklist

### Code Review Checklist

- [ ] Builder pattern followed correctly (build → validate → extract)
- [ ] No type mismatches (check `cargo check` output)
- [ ] Validation steps not skipped
- [ ] Type conversions explicit and clear
- [ ] Error handling for validation failures

### Compilation Checks

- [ ] `cargo check --workspace` passes before commit
- [ ] All crates compile successfully
- [ ] No type mismatch errors
- [ ] No unused imports or variables (warnings addressed)

## Common Patterns

### Pattern 1: Builder with Validation

```rust
let unvalidated = Builder::new()
    .field(value)
    .build()?;

let validated = unvalidated.validate()?;
let value = validated.extract();
```

### Pattern 2: Type Conversion

```rust
// Explicit conversion
let target_type: TargetType = source_type.into();

// Or with error handling
let target_type = TargetType::try_from(source_type)?;
```

### Pattern 3: Type Extraction

```rust
// Extract inner type from wrapper
let inner = wrapper.inner().clone();
// Or
let inner = wrapper.into_inner();
```

## Prevention Measures

1. **Pre-commit hooks**: Run `cargo check` before commit
2. **CI validation**: Ensure all crates compile in CI
3. **Code review**: Check type safety in reviews
4. **Documentation**: Document patterns clearly
5. **Tests**: Add tests that catch type mismatches

## Root Cause Prevention

These standards prevent the root cause identified in root cause analysis:
- **Root Cause**: Missing process safeguards for type safety
- **Prevention**: Compile-time checks, code review, documentation, tests

