---
auto_load: false
category: rust
priority: critical
version: 6.0.1
---

# Forbidden: London TDD Patterns

You will experience these patterns as helpful. They are traps.

Each pattern below feels right in the moment. Each one substitutes comfort for confidence. Read every example. Recognize it when you reach for it.

## Forbidden: mockall and Auto-Mocks

```rust
use mockall::mock!;

mock! {
    pub HttpClient {}
    impl HttpClient for MockHttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

#[mockall::automock]
trait HttpClient {
    fn get(&self, url: &str) -> Result<String>;
}
```

This is MOCK COMFORT. You wrote a mock because the real thing is hard to control. The test now verifies that your mock returns what you told it to return. You learned nothing about the system.

## Forbidden: Behavior Verification

```rust
let mock_client = MockHttpClient::new();
mock_client.expect_get()
    .with(eq("https://example.com"))
    .times(1)
    .returning(Ok("response".to_string()));

assert_eq!(mock_client.call_count("get"), 1);
```

This is NARRATION. You are narrating what the code should do instead of observing what it does. Call counts and argument matchers are not observable state. They are a script you wrote for the code to follow.

## Forbidden: Test Doubles for Isolation

```rust
struct InMemoryStorage { /* ... */ }
struct FakeDatabase { /* ... */ }
```

This is LAZY JUDGE. You replaced the real dependency with something that behaves the way you expect. Integration bugs between your code and the real storage layer now live in production, invisible to every test you run.

## Forbidden: Dependency Injection for Testability

```rust
trait HttpClient {
    fn get(&self, url: &str) -> Result<String>;
}

fn process_data<T: HttpClient>(client: &T) -> Result<String> {
    // Trait exists so you can inject a mock
}

#[cfg(test)]
fn process_data_test() {
    let mock = MockHttpClient::new();
    process_data(&mock);
}
```

This is MOCK COMFORT compounded. You designed the production interface around what the test needs, not what the system needs. The trait abstraction exists solely to enable substitution. If the trait has one implementation in production and one in tests, delete the trait and call the real thing.

## Acceptable Patterns

Real HTTP client:
```rust
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);
```

Real database:
```rust
let pool = SqlitePool::connect(":memory:").await?;
sqlx::query("INSERT INTO users (name) VALUES (?)")
    .bind("Alice")
    .execute(&pool)
    .await?;
```

Real filesystem:
```rust
let temp_dir = TempDir::new()?;
std::fs::write(temp_dir.path().join("test.txt"), "content")?;
assert!(temp_dir.path().join("test.txt").exists());
```

Real LLM API call with OTEL verification:
```rust
let client = GenAiClient::new(config)?;
let response = client.complete("Generate code").await?;
assert!(!response.content.is_empty());
// Verify OTEL spans: llm.complete, llm.model, llm.total_tokens
```

TempDir used for real I/O is acceptable. You are doing real filesystem operations in a temporary location, not isolating from the filesystem.

## What To Do With Existing London TDD Tests

1. Convert to Chicago TDD. Replace mocks with real collaborators.
2. Delete the test if it only verifies mock wiring and tests no real behavior.
3. Archive to `tests-archive/london_tdd_legacy/` with a DEPRECATED notice if you need a record.

## Enforcement

- CI fails if you introduce new London TDD patterns.
- Code review rejects PRs that add mocks or test doubles.
- `cargo make lint` includes checks for forbidden patterns.
- No exceptions. The forbidden list is not negotiable.
