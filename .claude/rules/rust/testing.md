---
auto_load: false
category: rust
priority: critical
version: 6.0.0
---

# 🧪 Chicago TDD (MANDATORY)

## Principles
- State-based verification
- Real collaborators (not mocks)
- Real execution (not test doubles)
- AAA pattern: Arrange/Act/Assert

## FORBIDDEN London TDD Patterns

The following patterns are **NOT ALLOWED** in this project:

### ❌ Mocks and Test Doubles
```rust
// FORBIDDEN:
use mockall::mock!;

mock! {
    pub HttpClient {}
    impl HttpClient for MockHttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

// FORBIDDEN:
#[mockall::automock]
trait HttpClient {
    fn get(&self, url: &str) -> Result<String>;
}
```

**Why:** Mocks test mock behavior, not real system behavior. Use real HTTP clients instead.

### ❌ Behavior Verification
```rust
// FORBIDDEN:
let mock_client = MockHttpClient::new();
mock_client.expect_get()
    .with(eq("https://example.com"))
    .times(1)
    .returning(Ok("response".to_string()));

// FORBIDDEN:
assert_eq!(mock_client.call_count("get"), 1);
```

**Why:** Tests verify mock interactions, not actual state. Use real HTTP calls and assert on response content.

### ❌ Test Doubles for "Isolation"
```rust
// FORBIDDEN:
struct InMemoryStorage { /* ... */ } // Fake that simulates real storage

// FORBIDDEN:
struct FakeDatabase { /* ... */ } // Test double that avoids real DB
```

**Why:** Test doubles hide integration bugs. Use real databases (SQLite, PostgreSQL via testcontainers).

### ✅ ACCEPTABLE: Real Collaborators
```rust
// ACCEPTABLE: Real HTTP client
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);

// ACCEPTABLE: Real database
let pool = SqlitePool::connect(":memory:").await?;
sqlx::query("INSERT INTO users (name) VALUES (?)")
    .bind("Alice")
    .execute(&pool)
    .await?;

// ACCEPTABLE: Real filesystem
let temp_dir = TempDir::new()?;
std::fs::write(temp_dir.path().join("test.txt"), "content")?;
assert!(temp_dir.path().join("test.txt").exists());
```

### ❌ `tempfile::TempDir` for Real File I/O (NOT London TDD)

**Note:** `tempfile::TempDir` is ACCEPTABLE when used for real filesystem operations (real I/O, not isolation). This is Chicago TDD, not London TDD.

```rust
// ACCEPTABLE: Real file I/O in temp directory
let temp_dir = TempDir::new()?;
let file_path = temp_dir.path().join("test.json");
std::fs::write(&file_path, r#"{"key": "value"}"#)?;
let content = std::fs::read_to_string(&file_path)?;
assert_eq!(content, r#"{"key": "value"}"#);
```

## Test Types
| Type | Timeout | Framework |
|------|---------|-----------|
| Unit | <150s | Standard Rust |
| Integration | <30s | Workspace tests |
| BDD | - | Cucumber |
| Property | - | proptest |
| Snapshot | - | insta |
| Security | - | Custom |
| Determinism | - | RNG_SEED=42 |
| Performance | - | Criterion |

## Requirements (Definition of Done)
- ✅ All public APIs tested
- ✅ Error paths + edge cases (80%+ coverage)
- ✅ Tests verify observable outputs/state changes
- ✅ NEVER claim completion without running tests
- ✅ AAA pattern enforced
- ✅ No meaningless tests

## Commands
```bash
cargo make test-unit     # Fast (<16s)
cargo make test          # Full (<30s)
cargo make slo-check     # Performance validation
```

## 80/20 Focus Areas
- Error paths and resource cleanup
- Concurrency edge cases
- Real dependencies integration
- Deterministic behavior verification
