# Specification: Affiliate Link Routing & Click Tracking

**Feature ID**: 014
**Version**: 1.0.0
**Priority**: P1
**Status**: Implementation Ready

## Summary

Zero-allocation affiliate link routing system with cryptographic click receipts for FactoryPaaS revenue attribution.

## Files

- `feature.ttl` - User stories and acceptance scenarios (RDF source of truth)
- `entities.ttl` - Domain model entities (AffiliateLinkRoute, ClickReceipt)
- `plan.ttl` - Architecture decisions and implementation tasks

## User Stories

### US-001: Route Resolution with Caching (P1)
**As a** FactoryPaaS operator
**I want** affiliate links resolved in <5ms with zero allocations on hot path
**So that** redirect latency is imperceptible to users

**Acceptance Criteria**:
- Route cache lookup completes in <5ms for 1M sequential lookups
- Zero heap allocations after cache warmup (verified via valgrind)
- 404 response for non-existent routes with error receipt

### US-002: Click Tracking with Receipts (P1)
**As a** FactoryPaaS auditor
**I need** cryptographic proof of every click
**So that** revenue attribution is verifiable and dispute-free

**Acceptance Criteria**:
- Receipt includes SHA-256 hash, timestamp (ISO 8601), IP hash
- Receipts form Merkle chain (prev_hash links to previous)
- Receipt verification endpoint returns cryptographic proof

### US-003: HTTP Redirect Handler (P2)
**As a** FactoryPaaS publisher
**I want** transparent click tracking via 302 redirects
**So that** users experience no friction while clicks are attributed

**Acceptance Criteria**:
- 302 redirect with X-Click-ID header
- Tracking pixel <1KB, <5ms latency
- No cookies required (privacy-safe)

### US-004: Property-Based Testing (P2)
**As a** ggen maintainer
**I need** property tests verifying routing invariants
**So that** edge cases are caught before production

**Acceptance Criteria**:
- 10k test cases covering determinism, URL safety, hash uniqueness
- Merkle chain integrity verified across 100 receipts
- Zero test failures or panics

## Domain Model

### Entities

**AffiliateLinkRoute**
- `id: Uuid` - UUID v7 for time-ordered identifiers
- `source_path: String` - Short slug (e.g., "promo-123")
- `destination_url: String` - Target URL for redirect
- `affiliate_id: Uuid` - Publisher identifier
- `tracking_params: Option<HashMap>` - Additional query params
- `created_at: DateTime` - ISO 8601 timestamp
- `active: bool` - Route enabled flag

**ClickReceipt** (Value Object)
- `click_id: Uuid` - UUID v7 unique identifier
- `route_id: Uuid` - Reference to route
- `timestamp: DateTime` - ISO 8601 UTC timestamp
- `ip_hash: String` - SHA-256 hex (privacy-safe)
- `user_agent: Option<String>` - User-Agent header
- `referrer: Option<String>` - HTTP Referer
- `hash: String` - SHA-256 of receipt content
- `prev_hash: Option<String>` - Merkle chain link

### Commands

**ResolveRoute**
- Resolve route by slug, generate click receipt
- HTTP: `GET /r/{slug}`

**TrackClick**
- Record click event with cryptographic receipt
- HTTP: `POST /track/click`

### Events

**RouteResolved**
- Emitted when route successfully resolved

**ClickTracked**
- Emitted when click receipt generated

## Architecture Decisions

### AD-001: Arc<DashMap> for Zero-Alloc Cache
**Decision**: Use `Arc<DashMap<String, Arc<Route>>>` for route cache
**Rationale**: Lock-free reads, zero clones on cache hits (Arc reference counting)
**Tradeoff**: ~5% overhead vs unsafe lock-free, but safety guarantees

### AD-002: Merkle-Linked Receipt Chain
**Decision**: Each receipt links to previous via `prev_hash`
**Rationale**: Tamper-evident audit trail, verifiable by third parties
**Tradeoff**: Sequential writes (contention), but strongest audit guarantees

### AD-003: UUID v7 Time-Ordered IDs
**Decision**: Use UUID v7 for all identifiers
**Rationale**: Chronological ordering, better DB performance, global uniqueness
**Tradeoff**: Recent RFC (9562), requires uuid crate 1.11+

### AD-004: SHA-256 IP Hashing
**Decision**: Hash IPs with SHA-256 before storage
**Rationale**: GDPR compliance, irreversible, duplicate detection
**Tradeoff**: Cannot reverse to IP for debugging

### AD-005: Axum HTTP Framework
**Decision**: Use Axum for type-safe HTTP handlers
**Rationale**: Zero-cost extractors, excellent ergonomics, Tokio integration
**Tradeoff**: Newer framework (less mature)

## Implementation Tasks

1. **RouteResolver** (4h) - DashMap cache, resolve() method
2. **ClickReceiptGenerator** (3h) - SHA-256 hashing, Merkle chain
3. **HTTP Handlers** (3h) - Axum routes for redirect/tracking
4. **Property Tests** (2h) - Determinism, safety, uniqueness tests
5. **Integration Tests** (3h) - End-to-end flow, concurrent load

**Total Estimate**: 15 hours

## Success Criteria

- ✅ Route resolution <5ms p99 (10k cached routes)
- ✅ Receipt generation <2ms p99 (including SHA-256)
- ✅ Property tests 100% pass (10k cases)
- ✅ Zero unwrap/expect in production code
- ✅ Integration tests pass with 1k concurrent requests

## Dependencies

- `dashmap = "6.0"` - Lock-free concurrent map
- `sha2 = "0.10"` - SHA-256 hashing
- `uuid = { version = "1.11", features = ["v7"] }` - Time-ordered UUIDs
- `chrono = "0.4"` - DateTime handling
- `axum = "0.7"` - HTTP framework
- `proptest = "1.8"` - Property testing

## References

- **Ontology**: `examples/rust-attribution-context/ontology/routing.ttl`
- **Templates**: `examples/rust-attribution-context/templates/rust_routing_*.rs.tera`
- **Documentation**: `examples/rust-attribution-context/ROUTING_IMPLEMENTATION.md`
- **SPARQL Queries**: `examples/rust-attribution-context/queries/routing*.sparql`

## Validation

```bash
# Validate specification
cargo make speckit-validate

# Generate code from ontology
cd examples/rust-attribution-context
ggen sync

# Run tests
cargo test --lib
cargo test --test routing_property_tests

# Verify SLOs
cargo bench routing_
```

## Changelog

- **2026-01-24**: Initial specification created
- **2026-01-24**: Ontology, templates, and queries implemented
- **2026-01-24**: Property tests and documentation added
