# ADR-0003: Testing Strategy (Chicago TDD)

**Date:** 2024-01-01  
**Status:** Accepted

## Context

The boilerplate must choose a testing style. The two dominant schools are:

- **London TDD** — test doubles (mocks) replace all collaborators; assertions verify mock
  interactions (`.expect_x().times(1)`).
- **Chicago TDD** — real collaborators are used wherever feasible; assertions verify
  observable state changes.

Rust's async ecosystem, strict ownership model, and zero-cost abstractions make real-collaborator
testing practical even for I/O-heavy code. SQLite `:memory:` initialises in microseconds.
`axum::test` exercises the full request/response stack without a live socket. `tempfile::TempDir`
provides real filesystem I/O with automatic cleanup. There is no performance argument for mocks.

## Decision

Adopt Chicago TDD exclusively across all crates.

- **No `mockall`**, no `#[automock]`, no `MockXxx` structs.
- **No behavior verification** (`.expect_*()`, `.times()`, `.with(eq(...))`).
- **No test doubles** that simulate real collaborators (in-memory fakes, stub clients).
- **State-based assertions only** — assert on the result, the database row, the file, the HTTP
  response; never on whether a method was called.

Concrete substitutions:

| London TDD (forbidden) | Chicago TDD (required) |
|------------------------|------------------------|
| `MockRepository` | `SqliteRepository` with `SqlitePool::connect(":memory:")` |
| `MockHttpClient` | `reqwest::Client` or `axum::test::TestClient` |
| `FakeFileSystem` | `tempfile::TempDir` with `std::fs` |
| `.expect_save().times(1)` | `assert_eq!(repo.find(id).await?, entity)` |

Test types in use:

- **Unit tests** (`#[cfg(test)]` modules inside source files) with real collaborators for
  pure-logic functions and value objects.
- **Integration tests** (`tests/integration/`) crossing crate boundaries, exercising the full
  hexagonal stack from HTTP handler to SQLite and back.
- **Property-based tests** (`proptest`) for value objects and parsing logic where exhaustive
  coverage matters more than specific scenarios.

## Rationale

Mocks test mock behavior, not system behavior. A `MockRepository` that returns a hard-coded entity
proves the service called `save()`; it does not prove the entity is durable. Real collaborators
prove both.

Rust's type system and borrow checker already prevent the class of bugs mocks are most effective
at catching — null dereferences, use-after-free, incorrect interface usage. The marginal protection
from mocking at the unit level is low. The marginal cost (mockall dependency, verbose setup,
behavior expectations that drift from implementation) is real.

SQLite `:memory:` is as fast as a mock in practice. Schema migrations run in the same process with
no disk I/O. Queries execute in microseconds. The performance argument for mocks does not apply.

## Alternatives Considered

### Option A: London TDD (mockall)

All collaborators replaced with generated mocks via `mockall`. Rejected: adds `mockall` as a
dependency, produces verbose setup code, hides integration bugs by design, and requires
behavior expectations that couple tests to implementation details.

### Option B: Hybrid (mocks for external, real for internal)

Mock HTTP and external APIs; use real SQLite for persistence. Rejected: the boundary between
"external" and "internal" is unclear and shifts over time, leading to inconsistent test patterns
across crates. The same practical constraints that make SQLite `:memory:` fast make `axum::test`
equally lightweight.

### Option C: No automated tests

Obviously rejected.

## Consequences

### Positive

- Tests prove real behavior, not mock wiring.
- Integration bugs surface in the test suite, not in production.
- Test code is simpler — no mock setup, no behavior expectations, no drift between mock and
  implementation.
- Adding a new `CoreError` variant or changing a repository signature produces a compiler error
  in tests immediately, not a silent mock mismatch.

### Negative

- Tests require a Tokio runtime (`#[tokio::test]`), adding a line of boilerplate per async test.
- SQLite `:memory:` setup (schema migration) adds a few milliseconds per test; pure unit tests
  with no I/O are marginally faster. In practice the full suite remains well under 30 seconds.

### Neutral

- `tempfile::TempDir` is real filesystem I/O, not a test double. Its use is encouraged, not
  a London TDD exception.
