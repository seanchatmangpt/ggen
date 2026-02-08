# Chicago TDD Methodology

## Core Principles

**Chicago School**: State-based testing with real collaborators, behavior verification, observable outputs.

## AAA Pattern (MANDATORY)

```rust
#[test]
fn test_user_discount_calculation() {
    // ARRANGE: Setup test state
    let user = User {
        purchases: 10,
        member_since: date(2020, 1, 1),
    };
    let service = UserService::new();

    // ACT: Execute behavior
    let discount = service.calculate_discount(&user);

    // ASSERT: Verify observable output
    assert_eq!(discount, 0.1);
}
```

## Test Types

### Unit Tests (< 150s)
```rust
// Test single functions/methods
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid_input() {
        let result = parse("valid");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_invalid_input() {
        let result = parse("invalid");
        assert!(result.is_err());
    }
}
```

### Integration Tests (< 30s)
```rust
// tests/integration_test.rs
// Test components working together with real dependencies
#[test]
fn test_full_pipeline() {
    let store = RdfStore::new();
    store.load_ontology("test.ttl").unwrap();

    let result = store.query("SELECT ?s WHERE { ?s ?p ?o }");
    assert!(result.is_ok());
}
```

### Property Tests
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_roundtrip(s in ".*") {
        let encoded = encode(&s);
        let decoded = decode(&encoded)?;
        prop_assert_eq!(s, decoded);
    }
}
```

### Snapshot Tests
```rust
use insta::assert_snapshot;

#[test]
fn test_render_output() {
    let output = render_template("user.html", &context);
    assert_snapshot!(output);
}
```

## Requirements

✅ All public APIs tested
✅ Error paths + edge cases (80%+ coverage)
✅ Tests verify observable outputs/state changes
✅ NEVER claim completion without running tests
✅ AAA pattern: Arrange/Act/Assert
✅ Real collaborators (not mocks unless necessary)

## 80/20 Focus

Focus testing effort on:
- Error paths and edge cases
- Resource cleanup
- Concurrency
- Real dependencies
- Determinism (RNG_SEED=42)

## Anti-Patterns

❌ Meaningless tests (no verification)
❌ Testing implementation details
❌ Excessive mocking
❌ Tests that don't fail when behavior breaks
❌ Claiming code works without test evidence

## TDD Cycle

1. **RED**: Write failing test
2. **GREEN**: Implement minimal code to pass
3. **REFACTOR**: Improve while maintaining green

```bash
cargo make test-unit  # Verify fails (RED)
# Implement code
cargo make test-unit  # Verify passes (GREEN)
cargo make pre-commit # Refactor (maintain GREEN)
```
