<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Gradual Migration Path - 6-Phase Rollout](#gradual-migration-path---6-phase-rollout)
  - [Overview](#overview)
  - [Timeline Overview](#timeline-overview)
  - [Phase 1: Feature Gates (Days 1-2)](#phase-1-feature-gates-days-1-2)
    - [Objectives](#objectives)
    - [Tasks](#tasks)
    - [Validation](#validation)
    - [Success Criteria](#success-criteria)
    - [Rollback Plan](#rollback-plan)
  - [Phase 2: Adapter Layer (Days 3-7)](#phase-2-adapter-layer-days-3-7)
    - [Objectives](#objectives-1)
    - [Tasks](#tasks-1)
    - [Validation](#validation-1)
    - [Success Criteria](#success-criteria-1)
    - [Rollback Plan](#rollback-plan-1)
  - [Phase 3: Search Migration (Days 8-14)](#phase-3-search-migration-days-8-14)
    - [Objectives](#objectives-2)
    - [Tasks](#tasks-2)
    - [Validation](#validation-2)
    - [Success Criteria](#success-criteria-2)
    - [Rollback Plan](#rollback-plan-2)
  - [Phase 4: Registry/Installation Migration (Days 15-21)](#phase-4-registryinstallation-migration-days-15-21)
    - [Objectives](#objectives-3)
    - [Tasks](#tasks-3)
    - [Validation](#validation-3)
    - [Success Criteria](#success-criteria-3)
    - [Rollback Plan](#rollback-plan-3)
  - [Phase 5: Publishing with Signing (Days 22-28)](#phase-5-publishing-with-signing-days-22-28)
    - [Objectives](#objectives-4)
    - [Tasks](#tasks-4)
    - [Validation](#validation-4)
    - [Success Criteria](#success-criteria-4)
    - [Rollback Plan](#rollback-plan-4)
  - [Phase 6: Documentation & Cutover (Days 29-35)](#phase-6-documentation--cutover-days-29-35)
    - [Objectives](#objectives-5)
    - [Tasks](#tasks-5)
    - [Validation](#validation-5)
    - [Success Criteria](#success-criteria-5)
    - [Rollback Plan](#rollback-plan-5)
  - [Risk Assessment Matrix](#risk-assessment-matrix)
  - [Monitoring & Metrics](#monitoring--metrics)
    - [Key Metrics to Track](#key-metrics-to-track)
    - [Monitoring Dashboard](#monitoring-dashboard)
  - [Communication Plan](#communication-plan)
    - [Stakeholders](#stakeholders)
    - [Milestones](#milestones)
  - [Success Criteria Summary](#success-criteria-summary)
  - [Post-Migration](#post-migration)
    - [V1 Deprecation Timeline](#v1-deprecation-timeline)
    - [Continuous Improvement](#continuous-improvement)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Gradual Migration Path - 6-Phase Rollout

## Overview

The migration from marketplace-v1 to marketplace-v2 follows a 6-phase approach designed for zero downtime, zero breaking changes, and gradual risk mitigation.

## Timeline Overview

| Phase | Duration | Risk Level | User Impact | Rollback Time |
|-------|----------|------------|-------------|---------------|
| Phase 1: Feature Gates | Week 1 (Days 1-2) | Low | None | Immediate |
| Phase 2: Adapter Layer | Week 1 (Days 3-7) | Low | None | <1 hour |
| Phase 3: Search Migration | Week 2 (Days 8-14) | Medium | Minimal | <1 hour |
| Phase 4: Registry/Installation | Week 3 (Days 15-21) | Medium | Minimal | <1 hour |
| Phase 5: Publishing with Signing | Week 4 (Days 22-28) | High | New features | <4 hours |
| Phase 6: Documentation & Cutover | Week 5 (Days 29-35) | Low | None | N/A |

## Phase 1: Feature Gates (Days 1-2)

### Objectives
- Add feature flags to Cargo.toml
- Implement conditional compilation structure
- Zero functional changes (infrastructure only)

### Tasks

**Day 1: Cargo.toml Setup**
```bash
# Update workspace Cargo.toml
- Add ggen-marketplace to workspace members
- Define workspace dependencies for v2

# Update ggen-core/Cargo.toml
- Add feature flags: marketplace-v1, marketplace-v2, marketplace-parallel
- Make ggen-marketplace optional
- Add ggen-marketplace as optional dependency

# Update ggen-cli/Cargo.toml
- Inherit feature flags from ggen-core
```

**Day 2: Conditional Compilation**
```rust
// Create module structure
ggen-core/src/marketplace/mod.rs
  - #[cfg(feature = "marketplace-v1")] pub mod v1;
  - #[cfg(feature = "marketplace-v2")] pub mod v2;
  - #[cfg(feature = "marketplace-parallel")] pub mod adapter;
```

### Validation
```bash
# Test all feature combinations
cargo build --no-default-features --features marketplace-v1
cargo build --no-default-features --features marketplace-v2
cargo build --features marketplace-parallel
cargo test --all-features

# Verify default behavior unchanged
cargo test
```

### Success Criteria
- ✅ All feature combinations compile
- ✅ Default build uses marketplace-v1
- ✅ All existing tests pass
- ✅ CI green across all feature flag combinations
- ✅ Zero user-facing changes

### Rollback Plan
- Revert Cargo.toml changes (1 commit rollback)
- No runtime impact

---

## Phase 2: Adapter Layer (Days 3-7)

### Objectives
- Implement `MarketplaceBackend` trait
- Create V1Adapter and V2Adapter
- Build DualBackendAdapter for A/B testing
- All functionality routed through adapters

### Tasks

**Day 3-4: Trait and V1 Adapter**
```rust
// ggen-domain/src/marketplace/backend.rs
- Define MarketplaceBackend trait
- Define unified data models (SearchQuery, SearchResults, etc.)

// ggen-domain/src/marketplace/v1_adapter.rs
- Implement V1Adapter (wraps existing ggen-marketplace)
- Implement all MarketplaceBackend methods
- Add conversion helpers (v1 ↔ unified)
```

**Day 5-6: V2 Adapter**
```rust
// ggen-domain/src/marketplace/v2_adapter.rs
- Implement V2Adapter (wraps ggen-marketplace)
- Implement all MarketplaceBackend methods
- Add RDF conversion helpers (v2 ↔ unified)
```

**Day 7: Dual Backend Adapter**
```rust
// ggen-domain/src/marketplace/dual_adapter.rs
- Implement DualBackendAdapter
- Support BackendStrategy enum (V1Only, V2Only, V2WithFallback, ABTest, Compare)
- Add metrics collection
- Add fallback logic
```

### Validation
```bash
# Unit tests for each adapter
cargo test v1_adapter
cargo test v2_adapter
cargo test dual_adapter

# Integration tests
cargo test --features marketplace-parallel marketplace_adapter_integration

# Verify v1 adapter passes all existing v1 tests
cargo test --no-default-features --features marketplace-v1
```

### Success Criteria
- ✅ V1Adapter passes all existing marketplace tests
- ✅ V2Adapter implements all methods (even if stubbed)
- ✅ DualBackendAdapter supports all strategies
- ✅ CLI code can use adapters transparently
- ✅ Metrics collection working
- ✅ Fallback logic tested

### Rollback Plan
- Disable marketplace-parallel feature
- CLI falls back to direct v1 usage
- Rollback time: <1 hour (config change)

---

## Phase 3: Search Migration (Days 8-14)

### Objectives
- Migrate search subsystem to V2 (RDF/SPARQL)
- A/B test V1 vs V2 search results
- Validate performance and accuracy

### Tasks

**Day 8-9: V2 Search Implementation**
```rust
// ggen-marketplace/src/search.rs
- Implement SparqlSearchEngine
- Build SPARQL query templates
- Add result ranking algorithm
- Optimize for <100ms lookup
```

**Day 10-11: A/B Testing Setup**
```yaml
# config.yaml
marketplace:
  backend: ab-test
  v2_percentage: 10  # Start with 10% traffic
  enable_ab_testing: true
  log_backend_selection: true
```

**Day 12-13: Monitoring and Tuning**
- Monitor metrics: latency, error rate, result quality
- Compare V1 vs V2 search results
- Tune SPARQL queries for performance
- Gradually increase v2_percentage: 10% → 25% → 50%

**Day 14: Validation**
- Full regression testing
- Performance benchmarks
- User acceptance testing

### Validation
```bash
# Performance benchmarks
cargo bench marketplace_search --features marketplace-parallel

# A/B test comparison
ggen marketplace search --query "rust web framework" --backend v1
ggen marketplace search --query "rust web framework" --backend v2

# Stress testing
cargo test marketplace_stress_suite --features marketplace-parallel
```

### Success Criteria
- ✅ V2 search latency <100ms (p95)
- ✅ V2 search results accuracy ≥ V1 (manual validation)
- ✅ A/B test shows <5% error rate difference
- ✅ No performance regression at 50% v2 traffic
- ✅ Automatic fallback working (if V2 fails, use V1)

### Rollback Plan
- Set v2_percentage to 0 in config
- All traffic routes to V1
- Rollback time: <1 hour (config change + restart)

---

## Phase 4: Registry/Installation Migration (Days 15-21)

### Objectives
- Migrate package installation to V2 (RDF registry)
- Enable optional cryptographic verification
- Maintain backward compatibility with V1 packages

### Tasks

**Day 15-16: V2 Registry Implementation**
```rust
// ggen-marketplace/src/registry.rs
- Implement RdfRegistry
- Support RDF-based package storage
- Implement installation logic
- Add Ed25519 signature verification (optional)
```

**Day 17-18: Installation Adapter**
```rust
// ggen-domain/src/marketplace/install_adapter.rs
- Implement unified installation interface
- Support both V1 and V2 package formats
- Add signature verification for V2 packages
- Maintain V1 installation paths for backward compatibility
```

**Day 19-20: Integration Testing**
- Test installation of V1 packages via V2 backend
- Test installation of V2 packages (with/without signatures)
- Test dependency resolution across V1/V2 packages
- Test rollback and error handling

**Day 21: Gradual Rollout**
```yaml
# config.yaml
marketplace:
  backend: ab-test
  v2_percentage: 25  # Conservative start
  fallback_on_error: true
```

### Validation
```bash
# Installation tests
ggen marketplace install hello-world --backend v1
ggen marketplace install hello-world --backend v2

# Signature verification tests (V2 only)
ggen marketplace install signed-package --backend v2 --verify-signature

# Dependency resolution tests
ggen marketplace install complex-package --backend v2

# Stress testing
cargo test marketplace_install_e2e --features marketplace-parallel
```

### Success Criteria
- ✅ V2 installation works for all V1 packages
- ✅ V2 installation works for V2 packages (with signatures)
- ✅ Dependency resolution works across V1/V2 packages
- ✅ Installation paths backward compatible
- ✅ Cryptographic verification working (when enabled)
- ✅ Error rate <1% in production

### Rollback Plan
- Set v2_percentage to 0 for installation operations
- Continue using V2 for search (Phase 3 remains active)
- Rollback time: <1 hour

---

## Phase 5: Publishing with Signing (Days 22-28)

### Objectives
- Enable package publishing with Ed25519 signatures
- Build key management tooling
- Document publishing workflow

### Tasks

**Day 22-23: Ed25519 Signing Implementation**
```rust
// ggen-marketplace/src/crypto.rs
- Implement Ed25519Signer
- Add key generation utilities
- Add signature verification
- Implement key storage (secure)
```

**Day 24-25: Publishing Adapter**
```rust
// ggen-domain/src/marketplace/publish_adapter.rs
- Implement unified publishing interface
- Support V1 publishing (no signing)
- Support V2 publishing (with Ed25519 signing)
- Add manifest validation
```

**Day 26-27: CLI Integration**
```bash
# Key generation
ggen marketplace keygen --output ~/.ggen/keys/signing.key

# Package signing
ggen marketplace sign --package gpack.yaml --key ~/.ggen/keys/signing.key

# Publishing with signature
ggen marketplace publish --sign --key ~/.ggen/keys/signing.key
```

**Day 28: Documentation**
- Write publishing guide
- Document key management best practices
- Create migration guide for publishers
- Add security documentation

### Validation
```bash
# Key generation tests
ggen marketplace keygen --output test.key
ggen marketplace verify-key test.key

# Signing tests
ggen marketplace sign --package test-package/gpack.yaml --key test.key

# Publishing tests
ggen marketplace publish --sign --key test.key --dry-run
ggen marketplace publish --sign --key test.key

# Verification tests
ggen marketplace install signed-package --verify-signature
```

### Success Criteria
- ✅ Ed25519 signing working
- ✅ Key generation secure and user-friendly
- ✅ Publishing workflow documented
- ✅ Backward compatibility (unsigned packages still work)
- ✅ Signature verification working
- ✅ Security audit passed

### Rollback Plan
- Publishing with signatures is opt-in
- V1 publishing still available
- No rollback needed (feature is additive)

---

## Phase 6: Documentation & Cutover (Days 29-35)

### Objectives
- Complete documentation
- Increase V2 traffic to 100%
- Deprecate V1 (but keep available)
- Announce V2 as default

### Tasks

**Day 29-30: Documentation**
```markdown
- User guide for new features
- Migration guide for package publishers
- Performance comparison (V1 vs V2)
- Breaking changes documentation (should be none)
- Security documentation (Ed25519 signing)
```

**Day 31-32: Gradual Traffic Increase**
```yaml
# Day 31: config.yaml
marketplace:
  backend: ab-test
  v2_percentage: 75

# Day 32: config.yaml
marketplace:
  backend: v2  # Full cutover to V2
  fallback_on_error: true  # Keep V1 as fallback
```

**Day 33: Final Validation**
- Full regression testing
- Performance benchmarks
- User acceptance testing
- Security audit
- Load testing

**Day 34: Announce V2 as Default**
```bash
# Update default features
[features]
default = ["marketplace-v2", "dx"]  # V2 is now default
marketplace-v1 = ["ggen-marketplace"]  # V1 available but deprecated
```

**Day 35: Monitoring**
- Monitor production metrics
- Track error rates
- Track performance
- Collect user feedback

### Validation
```bash
# Final regression suite
cargo test --all-features

# Performance benchmarks
cargo bench marketplace_performance

# Load testing
cargo test marketplace_stress_suite --release

# Security audit
cargo audit
```

### Success Criteria
- ✅ Documentation complete
- ✅ V2 is default backend
- ✅ V1 still available (deprecated)
- ✅ All tests passing
- ✅ Performance SLOs met (<100ms search, <200ms install)
- ✅ Zero breaking changes
- ✅ User feedback positive

### Rollback Plan
- Change default feature back to marketplace-v1
- Set backend: v1 in config
- Rollback time: <1 hour

---

## Risk Assessment Matrix

| Phase | Risk Area | Impact | Likelihood | Mitigation |
|-------|-----------|--------|------------|------------|
| 1 | Compilation failures | Low | Low | CI tests all feature combinations |
| 1 | Build time increase | Low | Medium | Benchmark CI build times |
| 2 | Adapter bugs | Medium | Medium | Comprehensive adapter tests |
| 2 | Performance overhead | Low | Low | Benchmark adapter overhead |
| 3 | Search result quality | High | Medium | A/B testing with gradual rollout |
| 3 | Performance regression | High | Medium | Performance benchmarks, SLO monitoring |
| 4 | Installation failures | High | Medium | Fallback to V1, extensive testing |
| 4 | Dependency resolution bugs | High | Low | Comprehensive integration tests |
| 5 | Cryptographic vulnerabilities | Critical | Low | Security audit, use battle-tested libraries |
| 5 | Key management issues | High | Medium | Secure key storage, clear documentation |
| 6 | Production incidents | High | Low | Gradual rollout, monitoring, quick rollback |

## Monitoring & Metrics

### Key Metrics to Track

**Performance Metrics**
- Search latency (p50, p95, p99)
- Installation time
- Backend selection time (adapter overhead)
- Fallback frequency (V2 → V1)

**Quality Metrics**
- Search result accuracy (manual validation)
- Installation success rate
- Signature verification success rate
- Error rate by backend

**Business Metrics**
- V2 adoption rate (% of operations using V2)
- User feedback sentiment
- Package publication rate (with/without signatures)

### Monitoring Dashboard

```yaml
# Metrics to expose via OpenTelemetry
marketplace.search.latency{backend=v1|v2}
marketplace.search.error_rate{backend=v1|v2}
marketplace.install.latency{backend=v1|v2}
marketplace.install.success_rate{backend=v1|v2}
marketplace.publish.signature_rate
marketplace.adapter.fallback_count
marketplace.backend.selection{backend=v1|v2}
```

## Communication Plan

### Stakeholders

**Developers (Package Publishers)**
- Email announcement before Phase 5
- Migration guide for signing keys
- Webinar on new features

**Users (Package Consumers)**
- Blog post announcing V2
- Changelog updates
- Documentation on new capabilities

**Internal Team**
- Daily standups during rollout
- Incident response plan
- Rollback playbook

### Milestones

- **Day 1**: Feature gates merged
- **Day 7**: Adapter layer complete
- **Day 14**: Search migration complete (50% traffic on V2)
- **Day 21**: Installation migration complete (25% traffic on V2)
- **Day 28**: Publishing with signing available
- **Day 35**: V2 is default, V1 deprecated

## Success Criteria Summary

| Criterion | Target | Measurement |
|-----------|--------|-------------|
| Zero breaking changes | 100% | All existing tests pass |
| Performance SLO: Search | <100ms (p95) | Benchmark |
| Performance SLO: Install | <200ms (p95) | Benchmark |
| Error rate | <1% | Production metrics |
| Fallback success rate | >99% | V2 failures don't break workflows |
| A/B test accuracy | ≥V1 quality | Manual validation |
| Security audit | Pass | External security review |
| Documentation coverage | 100% | All features documented |
| User satisfaction | >90% positive | Feedback survey |

## Post-Migration

### V1 Deprecation Timeline
- **Week 5**: V2 is default, V1 available but deprecated
- **Month 3**: Remove V1 from default features
- **Month 6**: V1 removed entirely (breaking change in major version)

### Continuous Improvement
- Collect user feedback
- Optimize SPARQL queries based on usage patterns
- Expand RDF ontology based on needs
- Add more cryptographic features (multi-sig, hardware keys)
