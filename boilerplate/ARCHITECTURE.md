# Architecture

## Crate Topology

```
src/              ← CLI binary + thin lib (init_tracing, arg parsing)
crates/
  core/           ← shared types: Error, Result<T>, Id<T>, pagination
  domain/         ← hex-arch center: entities, ports (traits), value objects
  service/        ← inbound adapter: Axum HTTP routes, middleware
  config/         ← layered config: defaults → .env → env vars
  sqlite/         ← outbound adapter: sqlx SQLite repository impls
  mcp-server/     ← inbound adapter: JSON-RPC 2.0 MCP server
```

### Dependency Rules (enforced by crate boundaries)

```
src → [core, bp-config]          (slim binary, no domain knowledge)
service → [core, domain, bp-config]
sqlite  → [core, domain]
mcp-server → [core, domain]
domain → [core]
bp-config → []                   (no internal dependencies)
core → []                        (leaf — nothing internal)
```

Cycles are impossible: the crate graph is a DAG.

## Data Flow

```
HTTP request
  → service::router (Axum)
  → service::handlers (deserialize, call domain port)
  → domain::ports::ItemService
  → sqlite::SqliteItemRepository (implements ItemRepository port)
  → SQLite database
  → back up the chain as Result<T, CoreError>
  → service::ApiError (maps CoreError → HTTP status)
  → JSON response
```

## Hexagonal Architecture

```
                    ┌─────────────────────────────┐
HTTP ───────────────►  service (inbound adapter)  │
                    │         │                   │
MCP  ───────────────►  mcp-server                │
                    │         │                   │
                    │   ┌─────▼──────┐            │
                    │   │   domain   │ ← ports    │
                    │   │  (center)  │   (traits) │
                    │   └─────┬──────┘            │
                    │         │                   │
                    │   sqlite (outbound adapter) │
                    └─────────┼───────────────────┘
                              │
                           SQLite DB
```

## Error Handling Strategy

Two-layer strategy documented in ADR-0002:

- **`core::CoreError`** — domain-level typed errors (`NotFound`, `Validation`, `Conflict`, `Internal`)
- **`anyhow::Error`** — infrastructure/IO errors wrapped at the boundary

HTTP boundary: `service::ApiError` implements `IntoResponse`, mapping `CoreError` variants to
appropriate HTTP status codes without leaking internal details.

## Config Precedence

```
hardcoded defaults
  → .env file (via dotenvy, optional)
  → environment variables
  → CLI flags (highest priority)
```

## Observability

Structured logging via `tracing` + `tracing-subscriber`. JSON format in production,
pretty-printed in development (detected via `RUST_LOG` / verbosity flags).

## Testing Strategy

- **Unit tests**: co-located in `src/` files with `#[cfg(test)]`
- **Integration tests**: under `tests/integration/` — real SQLite (`:memory:`), no mocks
- **Property tests**: under `tests/property/` — proptest for value objects, pagination math
- **Benchmarks**: under `tests/benches/` — Criterion
- **Fuzz targets**: under `tests/fuzz/` — cargo-fuzz entry points

No test doubles. Real collaborators (SQLite `:memory:`, real HTTP client, real tokio runtime).

## Splitting vs. Merging Crates

Merge a crate back into its consumer when:
- It has exactly one consumer
- It has no tests of its own
- It would never be published independently

Split a crate out when:
- Two consumers need it (avoids duplication)
- It has a distinct conceptual boundary
- It has its own test suite

## ADRs

| # | Decision |
|---|----------|
| [0001](docs/decisions/0001-hexagonal-architecture.md) | Hexagonal architecture |
| [0002](docs/decisions/0002-error-handling-strategy.md) | Error handling strategy |

## HTTP Request Flow

```
Client → Axum Router → Handler → ItemService → ItemRepository
                ↑                                      ↓
         X-Request-Id                           SQLite Pool
         (middleware)                                  ↓
                ↓                              rows → Item
         ApiError                                      ↓
         (JSON body)                          Ok(Item) / Err(CoreError)
                                                       ↓
                                              Ok(Json) / ApiError → HTTP 4xx/5xx
```

### Request lifecycle

1. Request arrives at Axum router
2. `add_request_id` middleware assigns `X-Request-Id` header
3. Handler extracts path/query params
4. Service validates domain rules, calls repository
5. Repository executes SQL, maps rows to domain entities
6. `ApiError` converts `CoreError` to HTTP status + JSON body
7. Response includes `X-Request-Id` for correlation

## Error Propagation

| Layer | Error Type | Mapped To |
|-------|-----------|-----------|
| Repository | `CoreError::NotFound` | `404 Not Found` |
| Repository | `sqlx::Error` | `CoreError::Internal` → `500` |
| Domain | `CoreError::Validation` | `400 Bad Request` |
| Domain | `CoreError::Conflict` | `409 Conflict` |
| Service | `CoreError::Internal` | `500 Internal Server Error` |

JSON error body shape:
```json
{"error": "not_found", "message": "Item abc not found"}
```

## Crate Dependency Graph (compile order)

```
bp-core        (no workspace deps)
bp-config      → bp-core
domain         → bp-core
service        → domain, bp-core
bp-sqlite      → domain, bp-core
mcp-server     (no workspace deps)
cargo-project  → bp-config, bp-core
```

Rule: no crate may depend on `service`, `bp-sqlite`, or `mcp-server` — they are leaves.
