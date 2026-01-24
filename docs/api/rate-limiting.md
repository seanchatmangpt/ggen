# Production-Ready Rate Limiting

## Overview

The ggen-api rate limiting middleware provides enterprise-grade request throttling with:

- **Token bucket algorithm** for burst and sustained rate control
- **Per-IP and per-API-key limiting** with automatic client identification
- **Redis backend** for distributed multi-instance deployments
- **Graceful degradation** with in-memory fallback
- **Configurable limits** for different tiers and routes
- **Comprehensive observability** via tracing

## Architecture

### Token Bucket Algorithm

The rate limiter uses a token bucket algorithm that provides:

1. **Burst capacity**: Allows short bursts up to `burst_size`
2. **Sustained rate**: Refills tokens at `requests_per_second`
3. **Fair queueing**: Time-based token refill prevents starvation
4. **Predictable behavior**: Deterministic rate calculation

### Backend Storage

#### In-Memory Backend
- Single-instance deployments
- Zero external dependencies
- Millisecond-precision token refill
- Thread-safe with RwLock

#### Redis Backend
- Multi-instance distributed deployments
- Atomic Lua script execution
- Connection pooling (bb8)
- TTL-based automatic cleanup
- Automatic fallback to in-memory on Redis failure

## Configuration

```rust
use ggen_api::middleware::{RateLimitConfig, RateLimitBackend};
use std::time::Duration;

let config = RateLimitConfig {
    requests_per_second: 100,  // Sustained rate
    burst_size: 200,            // Burst capacity
    backend: RateLimitBackend::Redis {
        url: "redis://localhost:6379".to_string(),
    },
    ttl: Duration::from_secs(60), // Data retention
};
```

### Configuration Options

| Field | Type | Description | Default |
|-------|------|-------------|---------|
| `requests_per_second` | `u32` | Sustained request rate (tokens/sec) | 10 |
| `burst_size` | `u32` | Maximum burst capacity | 20 |
| `backend` | `RateLimitBackend` | Storage backend | InMemory |
| `ttl` | `Duration` | Data time-to-live | 60s |

## Usage Patterns

### Pattern 1: Single Tier (Middleware)

```rust
use axum::{routing::get, Router};
use ggen_api::middleware::{rate_limit_middleware, RateLimiter, RateLimitConfig};
use std::sync::Arc;

let config = RateLimitConfig::default();
let limiter = Arc::new(RateLimiter::new(config).await?);

let app = Router::new()
    .route("/api/data", get(handler))
    .layer(axum::middleware::from_fn_with_state(
        limiter.clone(),
        rate_limit_middleware,
    ))
    .with_state(limiter);
```

### Pattern 2: Multi-Tier Limits

```rust
// Free tier: 10 req/s
let free_limiter = Arc::new(RateLimiter::new(RateLimitConfig {
    requests_per_second: 10,
    burst_size: 15,
    ..Default::default()
}).await?);

// Premium tier: 100 req/s
let premium_limiter = Arc::new(RateLimiter::new(RateLimitConfig {
    requests_per_second: 100,
    burst_size: 150,
    backend: RateLimitBackend::Redis {
        url: "redis://localhost:6379".to_string(),
    },
    ..Default::default()
}).await?);

let app = Router::new()
    .nest("/api/free", free_routes(free_limiter))
    .nest("/api/premium", premium_routes(premium_limiter));
```

### Pattern 3: Programmatic Checking

```rust
let limiter = RateLimiter::new(config).await?;

// Check before processing
match limiter.check_limit("api:user-key-123").await {
    Ok(()) => {
        // Process request
    }
    Err(RateLimitError::TooManyRequests { retry_after }) => {
        // Return 429 with Retry-After header
    }
    Err(e) => {
        // Handle backend errors
    }
}
```

### Pattern 4: Graceful Degradation

```rust
let config = RateLimitConfig {
    backend: RateLimitBackend::Redis {
        url: std::env::var("REDIS_URL")
            .unwrap_or_else(|_| "redis://localhost:6379".to_string()),
    },
    ..Default::default()
};

let limiter = match RateLimiter::new(config.clone()).await {
    Ok(limiter) => limiter,
    Err(e) => {
        warn!("Redis unavailable, using in-memory: {}", e);
        RateLimiter::new(RateLimitConfig {
            backend: RateLimitBackend::InMemory,
            ..config
        }).await?
    }
};

// Limiter automatically falls back to in-memory if Redis fails during operation
```

## Client Identification

The middleware automatically extracts client identifiers in this priority order:

1. **API Key** (Header: `X-API-Key`) → `api:{key}`
2. **IP Address** (from socket) → `ip:{address}`
3. **Anonymous** (fallback) → `anonymous`

Example:
```bash
# Per-API-key limiting
curl -H "X-API-Key: user-123" https://api.example.com/data

# Per-IP limiting (no API key)
curl https://api.example.com/data
```

## Response Headers

When rate limited (429 status), the response includes:

```
HTTP/1.1 429 Too Many Requests
Retry-After: 3
X-RateLimit-Limit: 0
X-RateLimit-Remaining: 0

Rate limit exceeded. Retry after 3 seconds
```

| Header | Description |
|--------|-------------|
| `Retry-After` | Seconds until next token available |
| `X-RateLimit-Limit` | Current limit (0 when exhausted) |
| `X-RateLimit-Remaining` | Tokens remaining (0 when exhausted) |

## Error Handling

### Error Types

```rust
pub enum RateLimitError {
    TooManyRequests { retry_after: u64 },
    BackendError(String),
    InvalidConfig(String),
}
```

### Error Responses

| Error | HTTP Status | Description |
|-------|-------------|-------------|
| `TooManyRequests` | 429 | Rate limit exceeded |
| `BackendError` | 500 | Backend (Redis) failure |
| `InvalidConfig` | 500 | Configuration error |

## Performance Characteristics

### In-Memory Backend
- Latency: <1ms (RwLock acquisition + HashMap lookup)
- Memory: ~100 bytes per active client
- Scalability: Thousands of clients per instance
- Accuracy: Millisecond-precision token refill

### Redis Backend
- Latency: 1-5ms (network + Lua script execution)
- Memory: Redis memory (hash with 2 fields per client)
- Scalability: Millions of clients across instances
- Accuracy: Floating-point second precision
- TTL cleanup: Automatic via Redis EXPIRE

## Testing

### Unit Tests (Chicago TDD)

All tests follow AAA (Arrange-Act-Assert) pattern with real collaborators:

```bash
cargo test -p ggen-api --lib rate_limit
```

Test coverage:
- Token bucket burst handling
- Token refill over time
- Independent limits per key
- Client ID extraction (API key, IP, anonymous)
- Retry-after calculation
- Concurrent request handling
- Error responses and headers

### Integration Tests

```bash
cargo test -p ggen-api --test rate_limit_integration_test
```

Integration test coverage:
- Axum middleware integration
- Multi-tier routing
- Header extraction
- Rate limit enforcement
- Burst and sustained rates

## Security Considerations

1. **DDoS Protection**: Configurable burst limits prevent request flooding
2. **Resource Exhaustion**: TTL-based cleanup prevents memory leaks
3. **Fair Queueing**: Time-based refill prevents monopolization
4. **Anonymous Limits**: All traffic without API keys shares `anonymous` limit

## Observability

The rate limiter emits structured logs via `tracing`:

```rust
// Debug level
debug!("Checking rate limit for key: {}", key);
debug!("Rate limit check passed for key: {}", key);

// Warning level
warn!("Rate limit exceeded for key: {}, retry after: {}s", key, retry_after);
warn!("Primary backend failed, falling back to in-memory: {}", error);

// Error level
error!("Rate limit backend error: {}", error);
error!("Fallback backend also failed: {}", error);
```

## Production Deployment

### Recommended Configuration

```rust
// API server (distributed)
RateLimitConfig {
    requests_per_second: 100,
    burst_size: 200,
    backend: RateLimitBackend::Redis {
        url: std::env::var("REDIS_URL")?,
    },
    ttl: Duration::from_secs(60),
}

// Single instance (dev/staging)
RateLimitConfig {
    requests_per_second: 50,
    burst_size: 100,
    backend: RateLimitBackend::InMemory,
    ttl: Duration::from_secs(60),
}
```

### Redis Setup

```bash
# Docker
docker run -d -p 6379:6379 redis:7-alpine

# Kubernetes
kubectl apply -f kubernetes/redis-deployment.yaml
```

### Monitoring

Monitor these metrics:
- Rate limit rejections (429 responses)
- Backend errors (Redis failures)
- Fallback activations (in-memory degradation)
- Retry-after distribution (histogram)

## Migration Guide

### From Legacy Rate Limiter

```diff
- use ggen_api::middleware::RateLimiter;
+ use ggen_api::middleware::{RateLimiter, RateLimitConfig, RateLimitBackend};

- let limiter = RateLimiter::new(max_requests_per_minute);
+ let config = RateLimitConfig {
+     requests_per_second: max_requests_per_minute / 60,
+     burst_size: max_requests_per_minute,
+     backend: RateLimitBackend::InMemory,
+     ttl: Duration::from_secs(60),
+ };
+ let limiter = RateLimiter::new(config).await?;

- limiter.check_rate_limit(ip).await?;
+ limiter.check_limit(&format!("ip:{}", ip)).await?;
```

## References

- [Token Bucket Algorithm](https://en.wikipedia.org/wiki/Token_bucket)
- [Redis Lua Scripting](https://redis.io/docs/manual/programmability/eval-intro/)
- [Axum Middleware](https://docs.rs/axum/latest/axum/middleware/)
- [ggen-api examples](/crates/ggen-api/examples/rate_limit_usage.rs)
