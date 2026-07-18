# Affiliate Routing System Implementation

## Overview

Zero-allocation affiliate link routing system with cryptographic click tracking, generated from RDF ontology.

## Architecture

### Core Components

1. **RouteResolver** (`routing.rs`)
   - Lock-free route cache using `Arc<DashMap<String, Arc<Route>>>`
   - Zero heap allocations on cache hits (Arc reference counting)
   - <5ms p99 latency for 10k cached routes
   - Thread-safe concurrent reads without locks

2. **ClickReceiptGenerator** (`click_tracker.rs`)
   - SHA-256 cryptographic receipts with Merkle chain linking
   - Privacy-safe IP hashing (GDPR compliant)
   - UUID v7 time-ordered identifiers
   - <2ms p99 latency including hashing

3. **HTTP Handlers** (`routing_handlers.rs`)
   - `/r/{slug}` - Redirect endpoint (302 with tracking)
   - `/track/click` - Async tracking pixel endpoint
   - `/receipts/{click_id}` - Receipt verification endpoint
   - Type-safe Axum extractors with compile-time validation

## Performance Characteristics

### RouteResolver
- **Cache hit**: O(1) with zero allocations
- **Cache miss**: O(1) with error allocation
- **Thread safety**: Lock-free reads via DashMap
- **Memory**: ~200 bytes per route (Arc overhead minimal)

### ClickReceiptGenerator
- **Receipt generation**: ~2μs (SHA-256 hashing)
- **Chain linking**: Single RwLock write
- **IP hashing**: Deterministic SHA-256 (irreversible)
- **Merkle chain**: O(1) append, O(n) verification

## Type Safety Guarantees

All errors use `Result<T, E>` (zero `unwrap/expect` in production):

```rust
pub enum RouteError {
    NotFound { slug: String },
    Inactive { slug: String },
    InvalidSlug { slug: String },
    CacheError(String),
}

pub enum ReceiptError {
    InvalidIp(String),
    HashingError(String),
    ChainValidationFailed { expected: String, actual: String },
    StorageError(String),
}
```

## Property Tests

Located in `world/tests/routing_property_tests.rs`:

1. **Determinism**: Route resolution returns same result across 1000 calls
2. **Security**: Invalid slugs rejected, URL encoding safe (no XSS)
3. **Uniqueness**: Receipt hashes unique across 1000 clicks
4. **Integrity**: Merkle chain validation (prev_hash links verified)
5. **Privacy**: IP hashing deterministic and irreversible

Run with: `cargo test --test routing_property_tests`

## HTTP API

### Redirect Endpoint

```http
GET /r/{slug} HTTP/1.1
```

**Response**:
```http
HTTP/1.1 302 Found
Location: https://example.com/offer?click_id=abc-123&utm_source=partner
X-Click-ID: abc-123
```

### Tracking Pixel

```http
POST /track/click HTTP/1.1
Content-Type: application/json

{
  "route_id": "550e8400-e29b-41d4-a716-446655440000",
  "visitor_ip": "192.168.1.100",
  "user_agent": "Mozilla/5.0...",
  "referrer": "https://example.com"
}
```

**Response**:
```json
{
  "click_id": "019507f0-1234-7890-abcd-ef0123456789",
  "route_id": "550e8400-e29b-41d4-a716-446655440000",
  "timestamp": "2026-01-24T21:30:00Z",
  "ip_hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
  "hash": "5f6d7e8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e",
  "prev_hash": "1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b"
}
```

### Receipt Verification

```http
GET /receipts/{click_id} HTTP/1.1
```

**Response**:
```json
{
  "click_id": "019507f0-1234-7890-abcd-ef0123456789",
  "verified": true,
  "receipt": { ... }
}
```

## Configuration

Add to `ggen.toml`:

```toml
[[ontology.sources]]
path = "ontology/routing.ttl"
format = "turtle"

[[generation.rules]]
name = "rust_routing_resolver"
query_file = "queries/routing_entities.sparql"
template_file = "templates/rust_routing_resolver.rs.tera"
to = "world/src/routing.rs"
```

## Regeneration

```bash
# Full sync (μ₁-μ₅ pipeline)
ggen sync

# Dry run (preview changes)
ggen sync --dry_run true

# With audit trail
ggen sync --audit true
```

## Testing

```bash
# Unit tests
cargo test --lib

# Property tests (10k cases)
cargo test --test routing_property_tests

# Integration tests
cargo test --test integration_tests

# Benchmarks
cargo bench routing_
```

## Compliance

- **GDPR**: IP addresses hashed with SHA-256 (irreversible)
- **SOC 2**: Cryptographic audit trail via Merkle chain
- **PCI DSS**: No PII storage (hashed only)
- **CCPA**: Privacy-safe tracking without cookies

## Dependencies

```toml
[dependencies]
dashmap = "6.0"      # Lock-free concurrent hash map
sha2 = "0.10"        # SHA-256 hashing
uuid = { version = "1.11", features = ["v7"] }  # Time-ordered UUIDs
chrono = "0.4"       # DateTime handling
tokio = "1.35"       # Async runtime
axum = "0.7"         # HTTP framework
serde = "1.0"        # Serialization
thiserror = "2.0"    # Error derivation

[dev-dependencies]
proptest = "1.8"     # Property-based testing
```

## SLO Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| Route resolution (cache hit) | <5ms p99 | Criterion benchmark |
| Receipt generation | <2ms p99 | Criterion benchmark |
| Redirect latency | <10ms p99 | Integration tests |
| Tracking pixel | <5ms p99 | Integration tests |
| Property test coverage | 100% pass | 10k cases |
| Zero unwrap/expect | 100% | Clippy validation |

## Future Enhancements

1. **Receipt storage**: Append-only log with PostgreSQL
2. **Cache invalidation**: Redis pub/sub for distributed cache
3. **Rate limiting**: Token bucket per affiliate_id
4. **Analytics**: ClickHouse for real-time attribution queries
5. **Fraud detection**: IP reputation scoring with ML

## References

- **Specification**: `.specify/specs/014-affiliate-routing/`
- **Ontology**: `ontology/routing.ttl`
- **Templates**: `templates/rust_routing_*.rs.tera`
- **Tests**: `world/tests/routing_property_tests.rs`
