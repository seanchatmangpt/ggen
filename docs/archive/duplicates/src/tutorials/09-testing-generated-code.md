<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Generated Code: Chicago TDD for Deterministic Outputs](#testing-generated-code-chicago-tdd-for-deterministic-outputs)
  - [Goal](#goal)
  - [Prerequisites](#prerequisites)
  - [Understanding Chicago TDD](#understanding-chicago-tdd)
  - [Part 1: Rust Tests](#part-1-rust-tests)
  - [Part 2: TypeScript Tests](#part-2-typescript-tests)
  - [Part 3: Python Tests](#part-3-python-tests)
  - [Part 4: Integration Tests](#part-4-integration-tests)
  - [Best Practices](#best-practices)
  - [Test Coverage Goals](#test-coverage-goals)
  - [Next Steps](#next-steps)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Generated Code: Chicago TDD for Deterministic Outputs

Learn how to test generated code using state-based testing to verify that your code generation pipeline produces correct, consistent results.

## Goal

Create comprehensive tests for generated code that verify correctness without being brittle or implementation-dependent.

## Prerequisites

- ggen installed
- Completed [Your First CLI Command](05-first-cli-command.md)
- Familiarity with your language's testing framework
- Completed [Multi-Language Project](07-multi-language-project.md)

## Understanding Chicago TDD

**Chicago School TDD** focuses on:
- **State verification**: Test observable outputs, not implementation
- **Real collaborators**: Use actual objects, minimize mocks
- **Behavior testing**: Verify what code does, not how it does it
- **AAA Pattern**: Arrange → Act → Assert

For generated code, this means:
- ✅ Test serialization/deserialization
- ✅ Test validation logic
- ✅ Test type safety
- ✅ Don't test internal implementation details

## Part 1: Rust Tests

Create `rust/tests/models.rs`:

```rust
#[cfg(test)]
mod tests {
    use domain::models::User;
    use chrono::Utc;

    // State-based test: Verify User constructor sets expected state
    #[test]
    fn user_new_sets_correct_state() {
        // Arrange
        let id = "u123".to_string();
        let email = "alice@example.com".to_string();
        let username = "alice".to_string();

        // Act
        let user = User::new(id.clone(), email.clone(), username.clone());

        // Assert - verify observable state
        assert_eq!(user.id, id);
        assert_eq!(user.email, email);
        assert_eq!(user.username, username);
        assert!(user.is_active);
        assert!(user.created_at <= Utc::now());
    }

    // Behavior test: Verify validation logic
    #[test]
    fn user_validation_fails_with_empty_username() {
        // Arrange
        let user = User::new(
            "u123".to_string(),
            "alice@example.com".to_string(),
            "".to_string(),  // Invalid: empty
        );

        // Act & Assert
        assert!(!user.is_valid());
    }

    #[test]
    fn user_validation_fails_with_short_username() {
        // Arrange
        let user = User::new(
            "u123".to_string(),
            "alice@example.com".to_string(),
            "ab".to_string(),  // Invalid: too short
        );

        // Act & Assert
        assert!(!user.is_valid());
    }

    #[test]
    fn user_validation_succeeds_with_valid_data() {
        // Arrange
        let user = User::new(
            "u123".to_string(),
            "alice@example.com".to_string(),
            "alice".to_string(),
        );

        // Act & Assert
        assert!(user.is_valid());
    }

    // Serialization test: Verify round-trip consistency
    #[test]
    fn user_serializes_and_deserializes_correctly() {
        // Arrange
        let original = User::new(
            "u123".to_string(),
            "alice@example.com".to_string(),
            "alice".to_string(),
        );

        // Act - serialize to JSON and back
        let json = serde_json::to_string(&original).unwrap();
        let deserialized: User = serde_json::from_str(&json).unwrap();

        // Assert - verify observable state unchanged
        assert_eq!(original.id, deserialized.id);
        assert_eq!(original.email, deserialized.email);
        assert_eq!(original.username, deserialized.username);
        assert_eq!(original.is_active, deserialized.is_active);
    }
}

// Test that generated types compile with type safety
#[test]
fn type_safety_enforced() {
    // This code won't compile if User is missing fields
    let _user = User::new(
        "u123".to_string(),
        "alice@example.com".to_string(),
        "alice".to_string(),
    );

    // This would cause compile error:
    // user.id = 123;  // Error: id is String, not i32
}
```

Run Rust tests:

```bash
cd rust
cargo test --lib

# Output:
# running 5 tests
# test tests::user_new_sets_correct_state ... ok
# test tests::user_validation_fails_with_empty_username ... ok
# test tests::user_validation_fails_with_short_username ... ok
# test tests::user_validation_succeeds_with_valid_data ... ok
# test tests::user_serializes_and_deserializes_correctly ... ok
```

## Part 2: TypeScript Tests

Create `typescript/tests/models.test.ts`:

```typescript
import { User, UserModel } from '../src/models';

describe('UserModel', () => {
  // State-based test: Verify constructor sets expected state
  test('constructor sets correct state', () => {
    // Arrange
    const data: Partial<User> = {
      id: 'u123',
      email: 'alice@example.com',
      username: 'alice',
    };

    // Act
    const user = new UserModel(data);

    // Assert
    expect(user.id).toBe('u123');
    expect(user.email).toBe('alice@example.com');
    expect(user.username).toBe('alice');
    expect(user.isActive).toBe(true);
    expect(user.createdAt).toBeInstanceOf(Date);
  });

  // Behavior test: Validate username
  test('validation fails with empty username', () => {
    const user = new UserModel({
      id: 'u123',
      email: 'alice@example.com',
      username: '',
    });

    expect(user.isValid()).toBe(false);
  });

  test('validation fails with short username', () => {
    const user = new UserModel({
      id: 'u123',
      email: 'alice@example.com',
      username: 'ab',
    });

    expect(user.isValid()).toBe(false);
  });

  test('validation succeeds with valid data', () => {
    const user = new UserModel({
      id: 'u123',
      email: 'alice@example.com',
      username: 'alice',
    });

    expect(user.isValid()).toBe(true);
  });

  // Serialization test
  test('serializes and deserializes correctly', () => {
    // Arrange
    const original = new UserModel({
      id: 'u123',
      email: 'alice@example.com',
      username: 'alice',
    });

    // Act
    const json = JSON.stringify(original);
    const deserialized = JSON.parse(json) as User;

    // Assert
    expect(deserialized.id).toBe(original.id);
    expect(deserialized.email).toBe(original.email);
    expect(deserialized.username).toBe(original.username);
  });

  // Type safety tests
  test('type system prevents invalid assignments', () => {
    const user = new UserModel({
      id: 'u123',
      email: 'alice@example.com',
      username: 'alice',
    });

    // This won't compile:
    // user.id = 123;  // Type error: number is not assignable to string

    // This will:
    user.id = 'u456';
  });
});
```

Run TypeScript tests:

```bash
cd typescript
npm test

# Output:
# PASS  tests/models.test.ts
#   UserModel
#     ✓ constructor sets correct state (3ms)
#     ✓ validation fails with empty username (1ms)
#     ✓ validation fails with short username (1ms)
#     ✓ validation succeeds with valid data (1ms)
#     ✓ serializes and deserializes correctly (2ms)
```

## Part 3: Python Tests

Create `python/tests/test_models.py`:

```python
import json
from datetime import datetime

import pytest

from models import User


class TestUser:
    """Chicago TDD tests for User model."""

    def test_constructor_sets_correct_state(self):
        """State-based test: Verify constructor state."""
        # Arrange
        user_data = {
            "id": "u123",
            "email": "alice@example.com",
            "username": "alice",
        }

        # Act
        user = User(**user_data)

        # Assert - verify observable state
        assert user.id == "u123"
        assert user.email == "alice@example.com"
        assert user.username == "alice"
        assert user.is_active is True
        assert isinstance(user.created_at, datetime)

    def test_validation_fails_with_empty_username(self):
        """Behavior test: Validate username field."""
        # Arrange
        with pytest.raises(ValueError):
            User(
                id="u123",
                email="alice@example.com",
                username="",  # Invalid
            )

    def test_validation_fails_with_short_username(self):
        """Behavior test: Validate username length."""
        with pytest.raises(ValueError):
            User(
                id="u123",
                email="alice@example.com",
                username="ab",  # Too short
            )

    def test_validation_succeeds_with_valid_data(self):
        """Behavior test: Accept valid data."""
        # Act
        user = User(
            id="u123",
            email="alice@example.com",
            username="alice",
        )

        # Assert
        assert user.is_valid()

    def test_serializes_and_deserializes_correctly(self):
        """Verify round-trip serialization."""
        # Arrange
        original = User(
            id="u123",
            email="alice@example.com",
            username="alice",
        )

        # Act - serialize to JSON and back
        json_str = original.model_dump_json()
        deserialized = User.model_validate_json(json_str)

        # Assert - verify observable state unchanged
        assert deserialized.id == original.id
        assert deserialized.email == original.email
        assert deserialized.username == original.username
        assert deserialized.is_active == original.is_active

    def test_invalid_email_raises_error(self):
        """Test email validation."""
        with pytest.raises(ValueError):
            User(
                id="u123",
                email="invalid-email",
                username="alice",
            )
```

Run Python tests:

```bash
cd python
pip install pytest
pytest -v

# Output:
# tests/test_models.py::TestUser::test_constructor_sets_correct_state PASSED
# tests/test_models.py::TestUser::test_validation_fails_with_empty_username PASSED
# tests/test_models.py::TestUser::test_validation_fails_with_short_username PASSED
# tests/test_models.py::TestUser::test_validation_succeeds_with_valid_data PASSED
# tests/test_models.py::TestUser::test_serializes_and_deserializes_correctly PASSED
```

## Part 4: Integration Tests

Create a cross-language integration test that verifies all languages handle the same data:

Create `scripts/test-integration.sh`:

```bash
#!/bin/bash
set -e

echo "Running cross-language integration tests..."

# Test data
USER_JSON='{
  "id": "u999",
  "email": "integration@example.com",
  "username": "integration",
  "isActive": true,
  "createdAt": "2024-01-01T00:00:00Z"
}'

# Test Rust
echo "Testing Rust..."
cd rust
cargo test --test models -- --ignored
cd ..

# Test TypeScript
echo "Testing TypeScript..."
cd typescript
npm test
cd ..

# Test Python
echo "Testing Python..."
cd python
pytest -v
cd ..

echo "✓ All integration tests passed"
```

## Best Practices

**1. Test observable behavior, not implementation**:
```rust
// ✅ Good - tests observable state
assert_eq!(user.is_active, true);

// ❌ Bad - tests internal implementation
assert_eq!(user.fields.get("is_active"), Some(Value::Bool(true)));
```

**2. Use AAA pattern consistently**:
```
Arrange -> Act -> Assert

Setup test data -> Call function -> Verify results
```

**3. Test both happy path and error cases**:
```rust
// ✅ Happy path
assert!(valid_user.is_valid());

// ✅ Error case
assert!(!invalid_user.is_valid());
```

**4. Test serialization round-trips**:
```
Object → JSON → Object → Verify same state
```

## Test Coverage Goals

For generated code:

- ✅ **100% of public APIs**: Every exported type and method
- ✅ **100% of validation logic**: All validation rules tested
- ✅ **100% serialization**: Both directions (to/from JSON)
- ✅ **Type system verification**: Compile-time type safety

## Next Steps

- Integrate tests into CI/CD ([CI/CD Integration](08-ci-cd-integration.md))
- Add property-based testing for advanced scenarios
- Test with real data from your domain
- [Advanced testing patterns](../how-to-guides/testing-strategy.md)

## Summary

You've learned:
- ✅ Chicago TDD principles for generated code
- ✅ State-based testing approach
- ✅ Testing across Rust, TypeScript, and Python
- ✅ Serialization round-trip testing
- ✅ Type safety verification
- ✅ Integration testing across languages

Your generated code is now fully tested and verified!
