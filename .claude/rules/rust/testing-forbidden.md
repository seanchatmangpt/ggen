---
auto_load: false
category: rust
priority: critical
version: 6.0.0
---

# ❌ FORBIDDEN: London TDD Patterns

Chicago TDD ONLY. Forbidden: `mockall::mock!`/`#[mockall::automock]` (use real `reqwest::Client`);
behavior verification (`.expect_get().times(1)` — assert real content instead); test doubles
like `InMemoryStorage`/`FakeDatabase` avoiding real deps (use real SQLite/PostgreSQL); trait
abstraction purely for mocking (pass the real collaborator directly).

```rust
let response = reqwest::Client::new().get(url).await?;   // real HTTP
let pool = SqlitePool::connect(":memory:").await?;        // real DB
let temp_dir = TempDir::new()?; std::fs::write(...)?;      // real filesystem
// real LLM call, verify OTEL: llm.complete, llm.model, llm.total_tokens
```

Existing London tests: convert to Chicago, delete (if only mock-wiring), or archive to
`tests-archive/london_tdd_legacy/`. Enforcement: CI fails on new London patterns; code review
rejects mocks/doubles.
