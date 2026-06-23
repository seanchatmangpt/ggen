# ADR-0001: Hexagonal Architecture

**Date:** 2024-01-01  
**Status:** Accepted

## Context

We need a crate layout that:
1. Makes domain logic testable without spinning up HTTP servers or real databases.
2. Lets us swap infrastructure (e.g., PostgreSQL for SQLite, REST for gRPC) without touching domain code.
3. Keeps dependency cycles impossible by construction.

## Decision

Adopt the hexagonal architecture (ports and adapters) with dedicated crates per layer:

- `crates/core` — shared types, no external dependencies
- `crates/domain` — entities, ports (traits), value objects; depends only on `core`
- `crates/service` — inbound HTTP adapter; depends on `domain`
- `crates/sqlite` — outbound repository adapter; depends on `domain`
- `crates/mcp-server` — inbound MCP adapter; depends on `domain`
- `crates/config` — configuration; no internal dependencies

## Consequences

### Positive

- Domain logic tested with `#[cfg(test)]` in-process; no HTTP or DB needed.
- Infrastructure replaced by implementing a new adapter crate.
- Dependency graph is a DAG — cycles impossible.
- Each crate compiles independently; faster incremental builds.

### Negative

- More `Cargo.toml` files to maintain.
- Cross-crate refactors touch multiple manifests.
- Trait objects or generics required to cross the port boundary.

### Neutral

- All port-crossing requires `async_trait` (or Rust 1.75+ `async fn in trait`).

## Alternatives Considered

### Option A: Single-crate monolith

Simpler initially, but domain logic becomes entangled with HTTP/DB types. Tests require full
infrastructure. Rejected: coupling cost compounds over time.

### Option B: Two crates (lib + bin)

Common Rust pattern (`my-crate` + `my-crate-cli`). Insufficient separation: HTTP and SQLite
types still share the same compilation unit. Rejected: doesn't prevent coupling.

### Option C: Feature flags to gate adapters

No separate crates; feature flags enable adapters in one crate. Rejected: feature
combinations explode, and dependency graph cannot be enforced by the compiler.
