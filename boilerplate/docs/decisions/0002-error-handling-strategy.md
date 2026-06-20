# ADR-0002: Error Handling Strategy

**Date:** 2024-01-01  
**Status:** Accepted

## Context

Rust error handling has two dominant styles:
- **`thiserror`** — typed enums, good for library/domain boundaries
- **`anyhow`** — boxed dynamic errors, good for application/infrastructure code

We need a strategy that:
1. Preserves typed error information at the domain boundary (for HTTP mapping).
2. Doesn't require boilerplate for infrastructure code (DB open, file read).
3. Maps errors correctly to HTTP status codes without `match` duplication.

## Decision

Two-layer strategy:

1. **Domain layer** (`core`, `domain`): `CoreError` typed enum via `thiserror`.
   - `NotFound` → 404
   - `Validation` → 422
   - `Conflict` → 409
   - `Internal(anyhow::Error)` → 500
   - `pub type Result<T> = std::result::Result<T, CoreError>`

2. **Infrastructure layer** (`sqlite`, `config`): `anyhow::Result` for IO/driver errors,
   converted to `CoreError::Internal` at the port boundary.

3. **HTTP boundary** (`service`): `ApiError(CoreError)` implements `IntoResponse`, matching
   on `CoreError` variants to produce the correct status code + JSON body.

## Consequences

### Positive

- HTTP layer has exhaustive mapping: adding a `CoreError` variant forces a compiler error
  at the HTTP boundary until it's handled.
- Infrastructure code stays concise: `?` on `sqlx::Error` works without a custom `From`.
- Domain code never depends on HTTP types.

### Negative

- Infrastructure errors lose their original type when wrapped in `CoreError::Internal`.
  Structured logging is the only way to recover details at runtime.
- Cross-crate conversions need explicit `From` or `map_err` at each boundary.

### Neutral

- `anyhow::Error` is in `CoreError::Internal` — mixing two error philosophies in one enum
  is a deliberate trade-off, not an oversight.

## Alternatives Considered

### Option A: `anyhow` everywhere

Simple. Rejected: HTTP layer cannot pattern-match on error type to produce correct status
codes. All errors become 500.

### Option B: Single `thiserror` enum with all variants

Maximum type safety. Rejected: infrastructure variants (e.g., `SqlxError(sqlx::Error)`)
bleed into the domain layer, creating a direct dependency on `sqlx` in `core`.

### Option C: Separate error type per crate

Maximum isolation. Rejected: callers need `N` conversion paths and `N` `match` arms;
HTTP mapping becomes fragile as crates evolve.
