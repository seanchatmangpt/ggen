# Affiliate Routing Implementation Todos

## Phase 1: Core Implementation (COMPLETE)

- [x] Create RDF ontology for routing domain (routing.ttl)
- [x] Define AffiliateLinkRoute entity with fields
- [x] Define ClickReceipt value object with Merkle chain
- [x] Create SPARQL queries for route extraction
- [x] Create Tera template for RouteResolver
- [x] Create Tera template for ClickReceiptGenerator
- [x] Create Tera template for HTTP handlers
- [x] Create property tests template
- [x] Update ggen.toml with routing generation rules
- [x] Create routing implementation documentation

## Phase 2: Code Generation & Validation (IN PROGRESS)

- [ ] Run `ggen sync` to generate Rust code from ontology
- [ ] Verify generated routing.rs compiles without errors
- [ ] Verify generated click_tracker.rs compiles without errors
- [ ] Verify generated routing_handlers.rs compiles without errors
- [ ] Run `cargo make check` to verify zero compiler errors
- [ ] Run `cargo make lint` to verify zero clippy warnings
- [ ] Fix any compilation errors (type mismatches, missing imports)
- [ ] Add necessary dependencies to world/Cargo.toml
- [ ] Verify all Result<T,E> error paths are handled

## Phase 3: Testing (PENDING)

- [ ] Run property tests: `cargo test --test routing_property_tests`
- [ ] Verify determinism property (1000 calls same result)
- [ ] Verify URL safety property (no XSS vectors)
- [ ] Verify receipt uniqueness property (1000 unique hashes)
- [ ] Verify Merkle chain integrity property
- [ ] Verify IP hashing determinism and irreversibility
- [ ] Run integration tests with testcontainers
- [ ] Test concurrent load (1k requests/sec)
- [ ] Verify zero race conditions under load
- [ ] Fix any failing tests (update ontology if needed)

## Phase 4: Performance Validation (PENDING)

- [ ] Run criterion benchmarks for route resolution
- [ ] Verify <5ms p99 latency with 10k cached routes
- [ ] Run benchmarks for receipt generation
- [ ] Verify <2ms p99 latency including SHA-256
- [ ] Verify zero heap allocations on cache hits (valgrind)
- [ ] Profile with flamegraph to identify hot paths
- [ ] Optimize any bottlenecks found
- [ ] Re-run benchmarks to verify improvements
- [ ] Document performance characteristics

## Phase 5: Integration (PENDING)

- [ ] Wire routing handlers into main application router
- [ ] Add RouteResolver and ClickReceiptGenerator to AppState
- [ ] Implement receipt storage backend (PostgreSQL)
- [ ] Add cache warming on application startup
- [ ] Implement receipt verification endpoint fully
- [ ] Add metrics collection (Prometheus)
- [ ] Add structured logging (tracing)
- [ ] Add health check for route cache status
- [ ] Test end-to-end redirect flow manually
- [ ] Test tracking pixel endpoint manually

## Phase 6: Security & Compliance (PENDING)

- [ ] Verify IP hashing implementation (SHA-256)
- [ ] Audit for GDPR compliance (no PII storage)
- [ ] Verify no SQL injection vectors (parameterized queries)
- [ ] Verify no XSS vectors in URL encoding
- [ ] Add rate limiting per affiliate_id
- [ ] Add request validation middleware
- [ ] Add CORS configuration for tracking pixel
- [ ] Add HTTPS enforcement in production
- [ ] Document security assumptions
- [ ] Create threat model document

## Phase 7: Documentation & Examples (PENDING)

- [ ] Create example route configuration (seed data)
- [ ] Create curl examples for all endpoints
- [ ] Create Postman collection for API testing
- [ ] Document error codes and recovery strategies
- [ ] Create runbook for operators
- [ ] Create monitoring dashboard spec
- [ ] Document SLO targets and measurement
- [ ] Create example integration (TypeScript client)
- [ ] Add API documentation (OpenAPI spec)
- [ ] Create video demo of routing system

## Phase 8: Production Readiness (PENDING)

- [ ] Run `cargo make ci` full validation
- [ ] Run `cargo make audit` security scan
- [ ] Run `cargo make slo-check` performance validation
- [ ] Verify all Andon signals cleared (no errors/warnings)
- [ ] Create Terraform infrastructure config
- [ ] Create Docker image with multi-stage build
- [ ] Create Kubernetes deployment manifests
- [ ] Set up monitoring alerts (Prometheus/Grafana)
- [ ] Create rollback plan
- [ ] Conduct load testing (sustained 10k req/s)

## Notes

- **RDF-First**: Ontology (routing.ttl) is source of truth
- **Zero Unwrap**: All production code uses Result<T,E>
- **Chicago TDD**: Property tests verify invariants
- **Performance**: Zero-alloc hot path with Arc<DashMap>
- **Privacy**: SHA-256 IP hashing for GDPR compliance
- **Audit Trail**: Merkle-linked receipts for tamper evidence

## Success Criteria

- ✅ All property tests pass (10k cases)
- ✅ Zero compiler errors/warnings
- ✅ <5ms p99 route resolution
- ✅ <2ms p99 receipt generation
- ✅ Zero heap allocations on cache hits
- ✅ 100% GDPR compliant (hashed IPs only)
- ✅ Cryptographic receipts with Merkle chain
- ✅ Type-safe error handling throughout
