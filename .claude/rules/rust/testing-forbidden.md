---
auto_load: false
category: rust
priority: critical
version: 6.0.0
---

# ❌ FORBIDDEN: London TDD Patterns

This project uses **Chicago TDD ONLY**. The following patterns are **FORBIDDEN**.

## Forbidden Patterns

### 1. Mockall and Auto-Mocks

```rust
// ❌ FORBIDDEN
use mockall::mock!;

mock! {
    pub HttpClient {}
    impl HttpClient for MockHttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

// ❌ FORBIDDEN
#[mockall::automock]
trait HttpClient {
    fn get(&self, url: &str) -> Result<String>;
}
```

**Use instead:** Real `reqwest::Client` with actual HTTP calls.

### 2. Behavior Verification

```rust
// ❌ FORBIDDEN
let mock_client = MockHttpClient::new();
mock_client.expect_get()
    .with(eq("https://example.com"))
    .times(1)
    .returning(Ok("response".to_string()));

// ❌ FORBIDDEN
assert_eq!(mock_client.call_count("get"), 1);
```

**Use instead:** Assert on actual response content, not mock interactions.

### 3. Test Doubles (InMemoryStorage, FakeDatabase, etc.)

```rust
// ❌ FORBIDDEN (when used to avoid real dependencies)
struct InMemoryStorage { /* ... */ }
struct FakeDatabase { /* ... */ }
```

**Use instead:** Real SQLite (`:memory:`), real PostgreSQL via testcontainers.

### 4. Dependency Injection for Testability

```rust
// ❌ FORBIDDEN (traits used as mocks)
trait HttpClient {
    fn get(&self, url: &str) -> Result<String>;
}

fn process_data<T: HttpClient>(client: &T) -> Result<String> {
    // Function designed for mock injection
}

// ❌ FORBIDDEN (mock injection)
#[cfg(test)]
fn process_data_test() {
    let mock = MockHttpClient::new();
    process_data(&mock); // Pass mock instead of real client
}
```

**Use instead:** Real collaborator passed directly, no trait abstraction for mocking.

## Allowed Patterns

### ✅ Real HTTP Client

```rust
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);
```

### ✅ Real Database (SQLite, PostgreSQL)

```rust
let pool = SqlitePool::connect(":memory:").await?;
sqlx::query("INSERT INTO users (name) VALUES (?)")
    .bind("Alice")
    .execute(&pool)
    .await?;
```

### ✅ Real Filesystem (TempDir for real I/O)

```rust
let temp_dir = TempDir::new()?;
std::fs::write(temp_dir.path().join("test.txt"), "content")?;
assert!(temp_dir.path().join("test.txt").exists());
```

### ✅ Real LLM API Calls (with OTEL verification)

```rust
let client = GenAiClient::new(config)?;
let response = client.complete("Generate code").await?;
assert!(!response.content.is_empty());
// Verify OTEL spans: llm.complete, llm.model, llm.total_tokens
```

## What to Do With Existing London TDD Tests

Existing London TDD tests should either:

1. **Convert to Chicago TDD** - Replace mocks with real collaborators
2. **Delete** - If the test only verifies mock wiring (not real behavior)
3. **Archive** - Move to `tests-archive/london_tdd_legacy/` with DEPRECATED notice

## Enforcement

- CI will fail if new London TDD patterns are introduced
- Code review will reject PRs that add mocks/test doubles
- `cargo make lint` should include checks for forbidden patterns
