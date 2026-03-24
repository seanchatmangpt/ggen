# ggen-backpressure

Backpressure and admission control (λ ≤ μ) for ggen v6.0.0.

## Overview

Implements work-in-progress limits, rate limiting, and kanban-style pull-based flow control to prevent system overload. Enforces the fundamental queueing theory constraint that arrival rate (λ) must not exceed service rate (μ).

## Components

### WIPToken (`token.rs`)
- Finite capacity tokens for limiting concurrent work
- RAII-based: dropping a token releases capacity
- Strict enforcement via semaphores
- Metadata tracking (ID, acquisition time)

### RateLimiter (`limiter.rs`)
- Token bucket algorithm for rate limiting
- Dual enforcement: rate limits + WIP limits
- Implements `AdmissionController` trait
- Configurable burst capacity

### KanbanBoard (`kanban.rs`)
- Pull-only workflow (no push allowed)
- Per-stage WIP limits
- Five stages: Backlog → Ready → InProgress → Review → Done
- Strict capacity enforcement

### AdmissionController (`lib.rs`)
- Common trait for admission control
- Async acquire with backpressure
- Non-blocking try_acquire
- Utilization metrics

## Usage

```rust
use ggen_backpressure::{RateLimiter, RateLimiterConfig, AdmissionController};

let config = RateLimiterConfig {
    max_rps: 100.0,
    max_concurrent: 10,
    burst_size: 20,
};

let limiter = RateLimiter::new(config);

// Acquire admission (waits if at capacity)
let token = limiter.acquire().await?;
// ... do work ...
drop(token); // Releases capacity

// Try without blocking
if let Some(token) = limiter.try_acquire()? {
    // ... do work ...
}
```

## Test Coverage

- 18 unit tests
- 9 integration tests under load
- 100% pass rate

## Performance

- Zero-copy token management
- Lock-free atomic operations where possible
- Async-native with Tokio

## Guarantees

1. **Finite capacity**: No more than N concurrent operations
2. **Rate limiting**: Enforces maximum requests/second
3. **Pull-only flow**: Work cannot be pushed, only pulled
4. **No deadlocks**: Non-blocking try_acquire always available
5. **Fair admission**: FIFO ordering via semaphore
