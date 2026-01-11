# Marketplace V2 Migration Architecture

## Overview

This directory contains comprehensive architecture documentation for migrating from marketplace-v1 (tantivy-based) to marketplace-v2 (RDF/SPARQL-based) with zero breaking changes and gradual adoption.

## Quick Links

- **[Executive Summary](00-executive-summary.md)** - Start here for high-level overview
- **[Feature Gates Strategy](01-feature-gates.md)** - Conditional compilation and build configuration
- **[Adapter Pattern Design](02-adapter-pattern.md)** - Unified backend interface and adapters
- **[Data Model Bridging](03-data-model-bridging.md)** - V1 ↔ V2 data conversion
- **[Migration Phases](04-migration-phases.md)** - 6-phase rollout timeline
- **[Code Organization](05-code-organization.md)** - File structure and module hierarchy
- **[Error Handling Strategy](06-error-handling.md)** - Unified errors and fallback logic
- **[Performance Strategy](07-performance-strategy.md)** - SLOs, optimization, and monitoring
- **[Testing Strategy](08-testing-strategy.md)** - Comprehensive test coverage plan
- **[Deployment & Rollout](09-deployment-rollout.md)** - Production deployment procedures

## Key Design Principles

### 1. Zero Breaking Changes

All existing marketplace-v1 functionality works unchanged:
- Same CLI commands
- Same API surface
- Same data formats (backward compatible)
- All existing tests pass

### 2. Gradual Migration

6-phase rollout over 5 weeks minimizes risk:
- **Week 1**: Feature gates + adapter layer (infrastructure)
- **Week 2**: Search migration (10% → 25% → 50% → 100% V2)
- **Week 3**: Registry/installation migration (same gradual approach)
- **Week 4**: Publishing with Ed25519 signing (new feature)
- **Week 5**: Documentation, cutover, monitoring

### 3. Automatic Fallback

V2 failures automatically fall back to V1:
- Circuit breaker pattern
- Retry with exponential backoff
- Transparent to users
- Metrics and monitoring

### 4. Feature Gates

Conditional compilation enables flexible deployment:
- `marketplace-v1` - V1 backend only (current default)
- `marketplace-v2` - V2 backend only (future default)
- `marketplace-parallel` - Both backends for A/B testing

### 5. Adapter Pattern

Unified `MarketplaceBackend` trait abstracts backend differences:
- `V1Adapter` - Wraps existing tantivy backend
- `V2Adapter` - Wraps new RDF/SPARQL backend
- `DualBackendAdapter` - Routes traffic between V1 and V2

## Architecture at a Glance

```
┌─────────────────────────────────────────────────────────┐
│                    CLI Commands                         │
│         search | install | publish | list               │
└──────────────────────────┬──────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────┐
│              MarketplaceBackend Trait                   │
│                  (Unified API)                          │
└──────────────────────────┬──────────────────────────────┘
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
        ▼                  ▼                  ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ V1Adapter   │    │ V2Adapter   │    │DualAdapter  │
│ (Tantivy)   │    │(RDF/SPARQL) │    │(Both A/B)   │
└─────────────┘    └─────────────┘    └─────────────┘
```

## Key Technical Decisions

| Decision | Chosen Approach | Rationale |
|----------|----------------|-----------|
| Backend abstraction | Adapter pattern | Gradual migration, A/B testing, fallback |
| Feature selection | Cargo feature flags | Compile-time optimization, clear dependencies |
| V2 data store | RDF (Oxigraph) | Semantic relationships, graph-aware |
| V2 query language | SPARQL | Powerful queries, flexible schema |
| Signing algorithm | Ed25519 | Fast, small signatures, widely trusted |
| Error handling | Automatic fallback | Zero user impact, high availability |
| Migration strategy | 6 phases, gradual | Low risk, quick rollback |

## Migration Timeline

### Week 1: Infrastructure (Low Risk)

**Phase 1: Feature Gates (Days 1-2)**
- Add Cargo feature flags
- Set up conditional compilation
- CI tests all combinations

**Phase 2: Adapter Layer (Days 3-7)**
- Implement `MarketplaceBackend` trait
- Create V1Adapter, V2Adapter, DualBackendAdapter
- All tests passing

**Risk**: Low
**User Impact**: None
**Rollback**: Immediate (compilation flags)

### Week 2: Search Migration (Medium Risk)

**Phase 3: A/B Testing Search (Days 8-14)**
- Day 8: 10% V2 traffic
- Day 10: 25% V2 traffic
- Day 12: 50% V2 traffic
- Day 14: 100% V2 (with V1 fallback)

**Risk**: Medium
**User Impact**: Minimal (improved search quality)
**Rollback**: <1 hour (config change)

### Week 3: Installation Migration (Medium Risk)

**Phase 4: Registry/Installation (Days 15-21)**
- Deploy V2 registry
- Gradual rollout: 10% → 50% → 100%
- Dependency resolution validation

**Risk**: Medium
**User Impact**: Minimal (faster installations)
**Rollback**: <1 hour (config change)

### Week 4: New Features (High Risk)

**Phase 5: Publishing with Signing (Days 22-28)**
- Deploy Ed25519 signing infrastructure
- Enable opt-in package signing
- Key generation and management

**Risk**: High (new feature)
**User Impact**: New capabilities (signing)
**Rollback**: N/A (feature is additive, opt-in)

### Week 5: Cutover (Low Risk)

**Phase 6: Documentation & Cutover (Days 29-35)**
- Complete user and publisher documentation
- Increase V2 traffic to 100%
- V2 becomes default
- V1 available as fallback

**Risk**: Low (proven in prior phases)
**User Impact**: None (transparent)
**Rollback**: <1 hour (config change)

## Service Level Objectives (SLOs)

### Performance Targets

| Operation | V1 Baseline | V2 Target | Measurement |
|-----------|-------------|-----------|-------------|
| Search (p95) | 120ms | <100ms | Response time |
| Install (p95) | 3s | <5s | End-to-end time |
| Publish (p95) | 2s | <3s | End-to-end time |
| Signature verification | N/A | <10ms | Crypto operation |

### Reliability Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| Error rate | <1% | Production errors / total operations |
| Fallback rate | <5% | V2→V1 fallbacks / V2 operations |
| Availability | >99.9% | Uptime (with fallback) |

## Test Coverage

| Test Type | Count | Coverage |
|-----------|-------|----------|
| Unit tests | 400 | 50% of tests |
| Component tests | 200 | 30% of tests |
| Integration tests | 80 | 15% of tests |
| E2E tests | 20 | 5% of tests |
| **Total** | **700** | **≥85% code coverage** |

## Success Criteria

### Pre-Launch

- [ ] All 700+ tests passing
- [ ] Feature flag coverage: 100%
- [ ] Backward compatibility: 100%
- [ ] Performance benchmarks: V2 ≥ V1
- [ ] Security audit: Passed

### Post-Launch (Week 1)

- [ ] Error rate increase: <1%
- [ ] Fallback rate: <5%
- [ ] Search latency (p95): <100ms
- [ ] User complaints: <10

### Post-Launch (Month 1)

- [ ] Signing adoption: >10%
- [ ] Performance SLOs: 100% met
- [ ] Rollback events: 0
- [ ] Documentation: 100% complete

## Resource Requirements

### Development Team

- **Phase 1-2**: 2 engineers, 2 weeks
- **Phase 3-4**: 3 engineers, 2 weeks
- **Phase 5-6**: 2 engineers, 2 weeks
- **Total**: 3 engineers (average), 5 weeks, 89 person-days

### Infrastructure

- RDF Store (Oxigraph): $200/month ongoing
- Monitoring (OpenTelemetry + Grafana): $250/month ongoing
- Additional compute (A/B testing): $300/month temporary
- **Total**: $450/month ongoing, $750/month during migration

## Risk Assessment

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| V2 performance regression | High | Medium | Benchmarking, gradual rollout, fallback |
| Data corruption | Critical | Low | Backups, dry-run testing, rollback plan |
| Breaking changes | Critical | Low | Adapter maintains V1 API, 700+ tests |
| Security vulnerability | Critical | Low | Security audit, battle-tested crypto |
| High fallback rate | Medium | Medium | Load testing, monitoring, alerts |

## Rollback Procedures

### Level 1: Configuration Rollback (<1 hour)

```bash
# Revert to V1 backend
cat > config.yaml <<EOF
marketplace:
  backend: v1
EOF
./deploy-config.sh config.yaml
```

### Level 2: Code Rollback (<2 hours)

```bash
git checkout release/marketplace-v1-stable
cargo build --release --no-default-features --features marketplace-v1
./deploy-production.sh
```

### Level 3: Data Rollback (<4 hours)

```bash
./scripts/restore-v1-data.sh --from-backup latest
./scripts/verify-data-integrity.sh
./scripts/resume-service.sh
```

## Documentation Index

### For Architects

1. **[Executive Summary](00-executive-summary.md)** - High-level overview with diagrams
2. **[Feature Gates](01-feature-gates.md)** - Conditional compilation strategy
3. **[Adapter Pattern](02-adapter-pattern.md)** - Backend abstraction design
4. **[Data Model](03-data-model-bridging.md)** - V1 ↔ V2 conversion
5. **[Code Organization](05-code-organization.md)** - File structure and modules

### For Developers

6. **[Error Handling](06-error-handling.md)** - Errors, fallback, retry logic
7. **[Testing Strategy](08-testing-strategy.md)** - Test pyramid and coverage
8. **[Performance](07-performance-strategy.md)** - Optimization and benchmarks

### For DevOps

9. **[Migration Phases](04-migration-phases.md)** - Rollout timeline and phases
10. **[Deployment](09-deployment-rollout.md)** - Production deployment procedures

## Next Steps

1. **Review this architecture** with stakeholders
2. **Approve migration plan** and timeline
3. **Set up project board** for tracking
4. **Kick off Phase 1** (feature gates)

## FAQ

### Why not just replace V1 with V2?

Direct replacement is high-risk. The adapter pattern enables:
- Gradual migration with A/B testing
- Automatic fallback on V2 failures
- Zero breaking changes
- Easy rollback

### Why RDF instead of SQL?

RDF provides:
- Semantic relationships between packages
- Flexible schema evolution (no migrations)
- SPARQL's powerful query capabilities
- Alignment with ggen's graph-aware philosophy

### What happens if V2 fails in production?

Automatic fallback to V1:
1. DualBackendAdapter detects V2 failure
2. Automatically retries on V1 backend
3. Logs fallback event for monitoring
4. User sees no interruption

### Can we skip A/B testing?

Not recommended. A/B testing provides:
- Real-world performance validation
- Search quality comparison
- Risk mitigation before full rollout
- Confidence in V2 readiness

### How do we handle existing packages?

Existing packages work unchanged:
- V1 packages installable via V2 backend
- Automatic conversion V1 → unified → V2
- No re-publishing required
- Opt-in for signing (not required)

### What if we need to rollback?

Three levels of rollback:
- **Level 1** (<1 hour): Config change to revert to V1
- **Level 2** (<2 hours): Code deployment of V1 stable
- **Level 3** (<4 hours): Data restore from backup

## Contact

- **Architecture questions**: [Create GitHub issue]
- **Implementation help**: [Slack #marketplace-v2]
- **Deployment support**: [On-call rotation]

---

**Last Updated**: 2025-11-18
**Version**: 1.0
**Status**: Architecture Complete, Awaiting Approval
