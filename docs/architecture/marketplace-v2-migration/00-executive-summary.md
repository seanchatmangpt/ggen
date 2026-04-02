<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace V2 Migration - Executive Summary](#marketplace-v2-migration---executive-summary)
  - [Overview](#overview)
  - [Key Design Principles](#key-design-principles)
  - [Architecture Diagrams](#architecture-diagrams)
    - [High-Level Architecture](#high-level-architecture)
    - [Feature Gate Architecture](#feature-gate-architecture)
    - [Data Flow Architecture](#data-flow-architecture)
    - [Migration Phases Timeline](#migration-phases-timeline)
    - [Error Handling and Fallback Flow](#error-handling-and-fallback-flow)
  - [Key Technical Decisions](#key-technical-decisions)
    - [1. Adapter Pattern (Not Direct V2 Replacement)](#1-adapter-pattern-not-direct-v2-replacement)
    - [2. Feature Gates (Not Runtime-Only Selection)](#2-feature-gates-not-runtime-only-selection)
    - [3. RDF/SPARQL (Not SQL or NoSQL)](#3-rdfsparql-not-sql-or-nosql)
    - [4. Ed25519 Signing (Not RSA or Other Algorithms)](#4-ed25519-signing-not-rsa-or-other-algorithms)
  - [Risk Assessment](#risk-assessment)
    - [High-Risk Areas](#high-risk-areas)
    - [Mitigation Strategies](#mitigation-strategies)
  - [Success Metrics](#success-metrics)
    - [Pre-Launch Validation](#pre-launch-validation)
    - [Post-Launch Metrics (Week 1)](#post-launch-metrics-week-1)
    - [Post-Launch Metrics (Month 1)](#post-launch-metrics-month-1)
  - [Resource Requirements](#resource-requirements)
    - [Development Time](#development-time)
    - [Infrastructure Requirements](#infrastructure-requirements)
  - [Key Deliverables](#key-deliverables)
    - [Code Artifacts](#code-artifacts)
    - [Documentation](#documentation)
    - [Testing Artifacts](#testing-artifacts)
  - [Next Steps](#next-steps)
    - [Immediate (This Week)](#immediate-this-week)
    - [Short-Term (Week 1)](#short-term-week-1)
    - [Medium-Term (Weeks 2-4)](#medium-term-weeks-2-4)
    - [Long-Term (Week 5+)](#long-term-week-5)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace V2 Migration - Executive Summary

## Overview

This document provides a comprehensive architecture for migrating from marketplace-v1 (tantivy-based) to marketplace-v2 (RDF/SPARQL-based) with **zero breaking changes** and **gradual adoption**.

## Key Design Principles

1. **Backward Compatibility**: All existing functionality works unchanged
2. **Gradual Migration**: 6-phase rollout over 5 weeks minimizes risk
3. **Automatic Fallback**: V2 failures automatically fall back to V1
4. **Feature Gates**: Conditional compilation enables flexible deployment
5. **Zero Downtime**: Adapter pattern enables hot-swapping backends

## Architecture Diagrams

### High-Level Architecture

```mermaid
flowchart TD
    subgraph CLI["CLI Layer (ggen-cli)"]
        SEARCH["search"]
        INSTALL["install"]
        PUBLISH["publish"]
        LIST["list"]
    end

    subgraph ADAPTER["Adapter Layer (ggen-domain/marketplace)"]
        TRAIT["MarketplaceBackend Trait<br/>(Unified API)"]
        V1ADAPTER["V1Adapter"]
        V2ADAPTER["V2Adapter"]
        DUALADAPTER["DualAdapter"]
    end

    subgraph BACKENDS["Backend Storage"]
        V1["Marketplace V1<br/>(Tantivy)"]
        V2["Marketplace V2<br/>(RDF/SPARQL)"]
        BOTH["Both (A/B Testing)"]
    end

    SEARCH --> TRAIT
    INSTALL --> TRAIT
    PUBLISH --> TRAIT
    LIST --> TRAIT

    TRAIT --> V1ADAPTER
    TRAIT --> V2ADAPTER
    TRAIT --> DUALADAPTER

    V1ADAPTER --> V1
    V2ADAPTER --> V2
    DUALADAPTER --> BOTH

    style CLI fill:#e1f5ff
    style ADAPTER fill:#fff4e6
    style BACKENDS fill:#c8e6c9
```

### Feature Gate Architecture

```mermaid
flowchart LR
    subgraph FEATURES["Cargo Features"]
        DEFAULT["default = [marketplace-v1]"]
        V1FEAT["marketplace-v1<br/>= [ggen-marketplace]"]
        V2FEAT["marketplace-v2<br/>= [ggen-marketplace-v2,<br/>rdf-backend,<br/>crypto-signing]"]
        PARALLEL["marketplace-parallel<br/>= [marketplace-v1,<br/>marketplace-v2,<br/>dual-backend]"]
    end

    subgraph BUILDS["Build Configurations"]
        V1ONLY["V1 Only Backend"]
        V2ONLY["V2 Only Backend"]
        DUAL["V1 + V2 (Dual)"]
    end

    DEFAULT --> V1ONLY
    V1FEAT --> V1ONLY
    V2FEAT --> V2ONLY
    PARALLEL --> DUAL

    style FEATURES fill:#e1f5ff
    style BUILDS fill:#c8e6c9
```

### Data Flow Architecture

```mermaid
sequenceDiagram
    participant User as User Request
    participant CLI as CLI (marketplace.rs)
    participant Selector as Backend Selector
    participant V1Adapter as V1 Adapter
    participant V2Adapter as V2 Adapter
    participant Tantivy as Tantivy Index
    participant Oxigraph as Oxigraph RDF Store
    participant Converter as UnifiedPackage Converter

    User->>CLI: search "rust framework"
    CLI->>CLI: Parse arguments
    CLI->>Selector: Create backend from config

    Selector->>Selector: Check feature flags
    Selector->>Selector: Read config (v1/v2/ab-test)

    alt V1 Backend
        Selector->>V1Adapter: Create V1 adapter
        V1Adapter->>V1Adapter: Convert to tantivy query
        V1Adapter->>Tantivy: Search
        Tantivy-->>V1Adapter: Tantivy results
        V1Adapter->>Converter: Convert to UnifiedPackage
    else V2 Backend
        Selector->>V2Adapter: Create V2 adapter
        V2Adapter->>V2Adapter: Convert to SPARQL query
        V2Adapter->>Oxigraph: SPARQL query
        Oxigraph-->>V2Adapter: RDF results
        V2Adapter->>Converter: Convert to UnifiedPackage
    end

    Converter-->>CLI: UnifiedPackage results
    CLI-->>User: SearchResults (JSON)
```

### Migration Phases Timeline

```mermaid
gantt
    title Marketplace V2 Migration Timeline (5 Weeks)
    dateFormat  YYYY-MM-DD
    section Week 1: Feature Gates + Adapter
    Feature Gates (Day 1-2)           :w1g1, 2026-04-01, 2d
    Adapter Layer (Day 3-7)           :w1g2, after w1g1, 5d

    section Week 2: Search Migration
    10% V2 traffic (Day 8)            :w2g1, 2026-04-08, 1d
    25% V2 traffic (Day 10)           :w2g2, after w2g1, 2d
    50% V2 traffic (Day 12)           :w2g3, after w2g2, 2d
    100% V2 with fallback (Day 14)    :w2g4, after w2g3, 2d

    section Week 3: Registry/Installation
    Deploy V2 registry (Day 15-16)    :w3g1, 2026-04-15, 2d
    10% V2 installation (Day 17)      :w3g2, after w3g1, 1d
    50% V2 installation (Day 19)      :w3g3, after w3g2, 2d
    100% V2 with fallback (Day 21)    :w3g4, after w3g3, 2d

    section Week 4: Publishing + Signing
    Deploy Ed25519 signing (Day 22-23):w4g1, 2026-04-22, 2d
    Enable signing opt-in (Day 24)    :w4g2, after w4g1, 1d
    Monitor adoption (Day 25-27)      :w4g3, after w4g2, 3d

    section Week 5: Documentation & Cutover
    Complete documentation (Day 29-30) :w5g1, 2026-04-29, 2d
    75% V2 traffic (Day 31)           :w5g2, after w5g1, 1d
    100% V2 traffic (Day 32)          :w5g3, after w5g2, 1d
    Final validation (Day 33)         :w5g4, after w5g3, 1d
    Announce V2 default (Day 34)      :w5g5, after w5g4, 1d
    Post-launch monitoring (Day 35)   :w5g6, after w5g5, 1d
```

### Error Handling and Fallback Flow

```mermaid
flowchart TD
    START["User Operation<br/>(search/install/etc)"]
    ADAPTER["DualBackendAdapter<br/>Strategy: V2WithFallback"]
    V2EXEC["Execute on V2 Backend"]

    START --> ADAPTER
    ADAPTER --> V2EXEC

    V2EXEC --> V2SUCCESS{V2 Success?}
    V2SUCCESS -->|Yes| V2RETURN["Return V2 Results"]
    V2SUCCESS -->|No| V2FAIL["Log V2 Failure<br/>Increment fallback_count"]

    V2FAIL --> V1EXEC["Execute on V1 Backend<br/>(Automatic Fallback)"]
    V1EXEC --> V1SUCCESS{V1 Success?}
    V1SUCCESS -->|Yes| V1RETURN["Return V1 Results"]
    V1SUCCESS -->|No| ERROR["Return Error<br/>(Both failed)"]

    style V2RETURN fill:#c8e6c9
    style V1RETURN fill:#fff4e6
    style ERROR fill:#ffcdd2
```

## Key Technical Decisions

### 1. Adapter Pattern (Not Direct V2 Replacement)

**Decision**: Use adapter layer instead of direct V2 replacement

**Rationale**:
- Enables gradual migration with zero breaking changes
- Allows A/B testing and performance comparison
- Provides automatic fallback on V2 failures
- Keeps V1 code stable during migration

**Trade-offs**:
- Adds ~5% adapter overhead
- More complex code structure
- Requires maintaining both backends temporarily

### 2. Feature Gates (Not Runtime-Only Selection)

**Decision**: Use Cargo feature flags for backend selection

**Rationale**:
- Compile-time optimization (smaller binaries)
- No runtime overhead for single-backend builds
- Clear dependency management
- Easier to test all combinations in CI

**Trade-offs**:
- Multiple build targets
- CI must test all feature combinations
- Can't switch backends without rebuild (in single-backend mode)

### 3. RDF/SPARQL (Not SQL or NoSQL)

**Decision**: Use RDF (Oxigraph) for V2 backend

**Rationale**:
- Semantic relationships between packages
- Flexible schema evolution
- SPARQL provides powerful query capabilities
- Aligns with ggen's graph-aware philosophy

**Trade-offs**:
- Steeper learning curve than SQL
- Less mature ecosystem than PostgreSQL/MongoDB
- Performance tuning requires SPARQL expertise

### 4. Ed25519 Signing (Not RSA or Other Algorithms)

**Decision**: Use Ed25519 for package signing

**Rationale**:
- Fast signing and verification
- Small signature size (64 bytes)
- Widely trusted (used by GitHub, SSH, etc.)
- Resistant to side-channel attacks

**Trade-offs**:
- Less ubiquitous than RSA
- Requires new key infrastructure
- No hardware support (unlike RSA)

## Risk Assessment

### High-Risk Areas

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| V2 performance regression | High | Medium | Extensive benchmarking, gradual rollout, automatic fallback |
| Data corruption during migration | Critical | Low | Backup before migration, dry-run testing, rollback plan |
| Breaking changes to existing users | Critical | Low | Adapter layer maintains V1 API, comprehensive testing |
| Security vulnerability in signing | Critical | Low | Security audit, use battle-tested libraries (ed25519-dalek) |
| High fallback rate in production | Medium | Medium | Load testing, canary deployment, monitoring |

### Mitigation Strategies

1. **Comprehensive Testing**: 700+ tests across unit, component, integration, E2E
2. **Gradual Rollout**: 6 phases over 5 weeks with 10% → 25% → 50% → 100% traffic
3. **Automatic Fallback**: V2 failures automatically fall back to V1 (99% success rate)
4. **Performance Monitoring**: Real-time dashboards, alerts, SLO tracking
5. **Rollback Plan**: <1 hour config rollback, <2 hour code rollback, <4 hour data rollback

## Success Metrics

### Pre-Launch Validation

| Metric | Target | Status |
|--------|--------|--------|
| Test coverage | ≥85% | TBD |
| Feature flag coverage | 100% | TBD |
| Backward compatibility | 100% | TBD |
| Performance benchmarks | V2 ≥ V1 | TBD |
| Security audit | Pass | TBD |

### Post-Launch Metrics (Week 1)

| Metric | Target | Actual |
|--------|--------|--------|
| Error rate increase | <1% | TBD |
| Fallback rate | <5% | TBD |
| Search latency (p95) | <100ms | TBD |
| Installation latency (p95) | <5s | TBD |
| User complaints | <10 | TBD |

### Post-Launch Metrics (Month 1)

| Metric | Target | Actual |
|--------|--------|--------|
| Signing adoption | >10% | TBD |
| Performance SLOs met | 100% | TBD |
| Rollback events | 0 | TBD |
| Documentation completeness | 100% | TBD |

## Resource Requirements

### Development Time

| Phase | Duration | Team Size | Total Person-Days |
|-------|----------|-----------|-------------------|
| Phase 1: Feature Gates | 2 days | 2 engineers | 4 |
| Phase 2: Adapter Layer | 5 days | 3 engineers | 15 |
| Phase 3: Search Migration | 7 days | 3 engineers | 21 |
| Phase 4: Registry/Installation | 7 days | 3 engineers | 21 |
| Phase 5: Publishing/Signing | 7 days | 2 engineers | 14 |
| Phase 6: Documentation/Cutover | 7 days | 2 engineers | 14 |
| **Total** | **35 days (5 weeks)** | **3 engineers (avg)** | **89 person-days** |

### Infrastructure Requirements

| Component | Purpose | Estimated Cost |
|-----------|---------|----------------|
| RDF Store (Oxigraph) | V2 package storage | $200/month |
| OpenTelemetry Collector | Monitoring | $100/month |
| Grafana Cloud | Dashboards and alerts | $150/month |
| Additional compute (dual backend) | A/B testing period | $300/month (temporary) |
| **Total (ongoing)** | | **$450/month** |
| **Total (migration period)** | | **$750/month** |

## Key Deliverables

### Code Artifacts

1. **Feature Gates** (ggen-core/Cargo.toml)
   - Conditional compilation structure
   - Feature flag definitions

2. **Adapter Layer** (ggen-domain/src/marketplace)
   - `MarketplaceBackend` trait
   - `V1Adapter`, `V2Adapter`, `DualBackendAdapter`
   - Conversion functions (v1 ↔ unified ↔ v2)

3. **V2 Backend** (ggen-marketplace-v2)
   - RDF registry (`RdfRegistry`)
   - SPARQL search engine (`SparqlSearchEngine`)
   - Ed25519 signing (`Ed25519Signer`)

4. **CLI Integration** (ggen-cli/src/cmds/marketplace.rs)
   - Backend selection logic
   - Configuration file support
   - Migration utilities

### Documentation

1. **User Guide**
   - How to use new V2 features
   - Migration guide for package publishers
   - Troubleshooting guide

2. **Architecture Documentation** (this folder)
   - 10 comprehensive architecture documents
   - Migration phases and timeline
   - Testing strategy
   - Deployment and rollout plan

3. **API Documentation**
   - `MarketplaceBackend` trait reference
   - Configuration options
   - Ed25519 signing workflow

### Testing Artifacts

1. **Test Suite**
   - 700+ tests (unit, component, integration, E2E)
   - Feature flag test matrix
   - Performance benchmarks
   - Load testing scripts

2. **CI/CD Pipeline**
   - Test all feature combinations
   - Automated benchmarking
   - Deployment automation
   - Rollback automation

## Next Steps

### Immediate (This Week)

1. Review and approve architecture
2. Set up project tracking (GitHub project board)
3. Schedule kickoff meeting
4. Assign Phase 1 tasks (feature gates)

### Short-Term (Week 1)

1. Implement feature gates (Phase 1)
2. Create `MarketplaceBackend` trait
3. Implement `V1Adapter`
4. Set up CI for feature flag testing

### Medium-Term (Weeks 2-4)

1. Implement V2 backend (RDF, SPARQL, crypto)
2. Build adapter layer
3. Conduct A/B testing for search
4. Migrate registry and installation

### Long-Term (Week 5+)

1. Enable publishing with signing
2. Complete documentation
3. Full production cutover
4. Monitor and optimize

## Conclusion

This architecture enables safe, gradual migration from marketplace-v1 to marketplace-v2 with:

✅ **Zero breaking changes** - All existing functionality preserved
✅ **Low risk** - 6-phase rollout with automatic fallback
✅ **High confidence** - 700+ tests, extensive monitoring
✅ **Clear timeline** - 5 weeks with defined milestones
✅ **Quick recovery** - <1 hour rollback capability
✅ **Future-proof** - RDF/SPARQL foundation for advanced features

The migration strategy balances innovation (new RDF backend, cryptographic signing) with stability (backward compatibility, gradual rollout, automatic fallback), ensuring a smooth transition for all stakeholders.

---

**Documentation Index**:
1. [Feature Gates Strategy](01-feature-gates.md)
2. [Adapter Pattern Design](02-adapter-pattern.md)
3. [Data Model Bridging](03-data-model-bridging.md)
4. [Migration Phases](04-migration-phases.md)
5. [Code Organization](05-code-organization.md)
6. [Error Handling Strategy](06-error-handling.md)
7. [Performance Strategy](07-performance-strategy.md)
8. [Testing Strategy](08-testing-strategy.md)
9. [Deployment and Rollout](09-deployment-rollout.md)
