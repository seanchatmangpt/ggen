# Deployment and Rollout Plan

## Overview

This document defines the production deployment strategy, rollout timeline, monitoring approach, and rollback procedures for the marketplace v2 migration.

## Pre-Deployment Checklist

### Code Readiness

- [ ] All feature gates implemented and tested
- [ ] Adapter layer complete with 100% test coverage
- [ ] V2 backend fully implemented (RDF, SPARQL, crypto)
- [ ] Data model conversion tested bidirectionally
- [ ] Error handling with automatic fallback working
- [ ] Performance benchmarks meet SLOs
- [ ] Security audit passed (Ed25519 signing)
- [ ] Documentation complete (user guide, migration guide, API docs)

### Infrastructure Readiness

- [ ] RDF store (Oxigraph) deployed and configured
- [ ] OpenTelemetry collector configured
- [ ] Grafana dashboards created
- [ ] Alert rules configured (latency, error rate, fallback rate)
- [ ] Backup and restore procedures tested
- [ ] Capacity planning completed
- [ ] Load testing completed at 2x expected traffic

### Operational Readiness

- [ ] Runbook created (deployment, rollback, troubleshooting)
- [ ] On-call rotation scheduled
- [ ] Incident response plan documented
- [ ] Communication plan ready (users, stakeholders, team)
- [ ] Rollback plan tested
- [ ] Feature flag controls accessible

## Deployment Timeline

### Week 1: Phase 1-2 (Feature Gates + Adapter Layer)

#### Day 1: Feature Gates Deployment

**Morning (9 AM)**
```bash
# 1. Deploy feature gates to staging
git checkout release/marketplace-v2-phase1
cargo build --release
./deploy-staging.sh

# 2. Run smoke tests on staging
cargo test --features marketplace-v1
cargo test --features marketplace-v2
cargo test --features marketplace-parallel

# 3. Verify all feature combinations
./scripts/test-all-features.sh
```

**Afternoon (2 PM)**
```bash
# 4. Deploy to production (canary)
./deploy-canary.sh  # 5% of servers

# 5. Monitor for 2 hours
./scripts/monitor-canary.sh --duration 2h

# 6. Full production rollout (if canary successful)
./deploy-production.sh
```

**Success Criteria:**
- All builds successful across feature combinations
- Zero test failures
- No performance regression
- No user-facing changes

#### Day 2-7: Adapter Layer Deployment

**Day 2-3: Deploy V1 Adapter**
```bash
# Deploy V1 adapter (wraps existing marketplace)
git checkout release/marketplace-v2-phase2a

# Deploy to staging
./deploy-staging.sh

# Run adapter tests
cargo test v1_adapter --features marketplace-parallel

# Deploy to production (gradual rollout)
./deploy-production-gradual.sh --percentage 25 --interval 1h
```

**Day 4-5: Deploy V2 Adapter**
```bash
# Deploy V2 adapter (RDF backend)
git checkout release/marketplace-v2-phase2b

# Deploy V2 infrastructure (RDF store)
kubectl apply -f k8s/rdf-store.yaml

# Deploy adapter code
./deploy-production-gradual.sh --percentage 25 --interval 1h
```

**Day 6-7: Deploy Dual Backend Adapter**
```bash
# Deploy dual backend adapter
git checkout release/marketplace-v2-phase2c

# Configure for V1-only initially
cat > config.yaml <<EOF
marketplace:
  backend: v1
  fallback:
    enabled: true
EOF

# Deploy to production
./deploy-production.sh
```

**Success Criteria:**
- All adapters pass integration tests
- V1 adapter passes all existing marketplace tests
- Performance overhead <5%
- Zero user-facing changes

### Week 2: Phase 3 (Search Migration)

#### Day 8: A/B Test Setup (10% V2 Traffic)

**Morning**
```yaml
# Update config.yaml
marketplace:
  backend: ab-test
  ab_testing:
    enabled: true
    v2_percentage: 10
    log_backend_selection: true
    compare_results: true
```

```bash
# Deploy configuration
./deploy-config.sh config.yaml

# Monitor A/B test metrics
./scripts/monitor-ab-test.sh --duration 4h
```

**Afternoon**
```bash
# Analyze results
python scripts/analyze-ab-test.py

# Compare search quality
python scripts/compare-search-quality.py --backend v1 --backend v2
```

#### Day 10: Increase to 25% V2 Traffic

```yaml
marketplace:
  ab_testing:
    v2_percentage: 25
```

#### Day 12: Increase to 50% V2 Traffic

```yaml
marketplace:
  ab_testing:
    v2_percentage: 50
```

#### Day 14: Full Search Migration (100% V2 with V1 Fallback)

```yaml
marketplace:
  backend: v2
  fallback:
    enabled: true
    fallback_to_v1: true
```

**Success Criteria:**
- V2 search latency <100ms (p95)
- Search result quality ≥ V1
- Error rate <1%
- Fallback rate <5%
- No increase in user complaints

### Week 3: Phase 4 (Registry/Installation Migration)

#### Day 15-16: Deploy V2 Registry

```bash
# Deploy V2 registry infrastructure
kubectl apply -f k8s/v2-registry.yaml

# Seed V2 registry with V1 data
./scripts/migrate-packages-v1-to-v2.sh --dry-run
./scripts/migrate-packages-v1-to-v2.sh

# Verify migration
./scripts/verify-migration.sh
```

#### Day 17: A/B Test Installation (10% V2)

```yaml
marketplace:
  backend: ab-test
  operations:
    search: v2  # Keep search on V2
    install:
      ab_testing:
        v2_percentage: 10
```

#### Day 19: Increase to 50% V2 Installation

```yaml
marketplace:
  operations:
    install:
      ab_testing:
        v2_percentage: 50
```

#### Day 21: Full Installation Migration

```yaml
marketplace:
  backend: v2  # All operations on V2
  fallback:
    enabled: true
```

**Success Criteria:**
- Installation success rate ≥99%
- Dependency resolution working
- V2 installation time <5s (p95)
- Backward compatibility with V1 packages

### Week 4: Phase 5 (Publishing with Signing)

#### Day 22-23: Deploy Cryptographic Signing

```bash
# Deploy Ed25519 signing infrastructure
kubectl apply -f k8s/crypto-signing.yaml

# Generate and distribute signing keys
./scripts/setup-signing-keys.sh

# Test signing and verification
./scripts/test-signing.sh
```

#### Day 24: Enable Publishing with Signing (Opt-In)

```yaml
marketplace:
  publishing:
    signing:
      enabled: true
      required: false  # Optional initially
```

**Communication:**
```bash
# Send email to package publishers
./scripts/send-publisher-announcement.sh

# Publish blog post
./scripts/publish-blog-post.sh "marketplace-v2-signing"

# Update documentation
./scripts/update-docs.sh
```

#### Day 27: Monitor Adoption

```bash
# Monitor signing adoption
./scripts/monitor-signing-adoption.sh --duration 24h

# Generate adoption report
python scripts/generate-adoption-report.py
```

**Success Criteria:**
- Signing workflow documented
- ≥10% of new packages signed
- Signature verification working
- Key management secure

### Week 5: Phase 6 (Documentation & Cutover)

#### Day 29-30: Complete Documentation

```bash
# Generate comprehensive docs
./scripts/generate-docs.sh

# Publish user guide
./scripts/publish-user-guide.sh

# Publish migration guide
./scripts/publish-migration-guide.sh

# Update API documentation
./scripts/update-api-docs.sh
```

#### Day 31: Increase V2 Traffic to 75%

```yaml
marketplace:
  backend: v2
  # Keep fallback enabled
```

#### Day 32: Increase V2 Traffic to 100%

```yaml
marketplace:
  backend: v2
  fallback:
    enabled: true  # V1 still available as fallback
```

#### Day 33: Final Validation

```bash
# Full regression testing
cargo test --all-features

# Performance benchmarks
cargo bench

# Security audit
./scripts/security-audit.sh

# Load testing
./scripts/load-test.sh --duration 1h --rps 1000
```

#### Day 34: Announce V2 as Default

```bash
# Update default features in Cargo.toml
sed -i 's/marketplace-v1/marketplace-v2/g' Cargo.toml

# Publish announcement
./scripts/publish-announcement.sh "marketplace-v2-default"

# Send notification to users
./scripts/notify-users.sh
```

#### Day 35: Post-Launch Monitoring

```bash
# Monitor for 24 hours
./scripts/monitor-production.sh --duration 24h

# Generate post-launch report
python scripts/generate-post-launch-report.py

# Conduct retrospective
./scripts/schedule-retrospective.sh
```

**Success Criteria:**
- V2 is production-ready and default
- V1 available as fallback
- All documentation complete
- User feedback positive
- SLOs met

## Monitoring Strategy

### Real-Time Dashboards

#### Dashboard 1: Backend Health

```yaml
panels:
  - title: "Backend Selection Distribution"
    query: "rate(marketplace_backend_selection_total[5m])"
    alert:
      condition: "v2_percentage < 90%"
      severity: warning

  - title: "Fallback Rate"
    query: "rate(marketplace_fallback_count[5m])"
    alert:
      condition: "fallback_rate > 0.05"  # >5% fallbacks
      severity: critical

  - title: "Error Rate by Backend"
    query: "rate(marketplace_errors_total[5m])"
    alert:
      condition: "error_rate > 0.01"  # >1% errors
      severity: critical
```

#### Dashboard 2: Performance Metrics

```yaml
panels:
  - title: "Search Latency (p95)"
    query: "histogram_quantile(0.95, marketplace_search_latency)"
    alert:
      condition: "p95 > 100"  # >100ms
      severity: warning

  - title: "Installation Latency (p95)"
    query: "histogram_quantile(0.95, marketplace_install_latency)"
    alert:
      condition: "p95 > 5000"  # >5s
      severity: warning

  - title: "Throughput (req/s)"
    query: "rate(marketplace_requests_total[1m])"
```

#### Dashboard 3: Business Metrics

```yaml
panels:
  - title: "Packages Published"
    query: "increase(marketplace_publish_count[1h])"

  - title: "Packages Installed"
    query: "increase(marketplace_install_count[1h])"

  - title: "Signing Adoption Rate"
    query: "marketplace_signed_packages / marketplace_total_packages"
```

### Alert Rules

```yaml
# High-Priority Alerts

- alert: HighFallbackRate
  expr: rate(marketplace_fallback_count[5m]) > 0.10
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "High V2→V1 fallback rate (>10%)"
    action: "Investigate V2 backend failures"

- alert: HighErrorRate
  expr: rate(marketplace_errors_total[5m]) > 0.02
  for: 3m
  labels:
    severity: critical
  annotations:
    summary: "High marketplace error rate (>2%)"
    action: "Check error logs and consider rollback"

- alert: SearchLatencyHigh
  expr: histogram_quantile(0.95, marketplace_search_latency) > 150
  for: 10m
  labels:
    severity: warning
  annotations:
    summary: "Search latency p95 >150ms"
    action: "Check SPARQL query performance"

- alert: RDFStoreUnhealthy
  expr: up{job="rdf-store"} == 0
  for: 1m
  labels:
    severity: critical
  annotations:
    summary: "RDF store is down"
    action: "Restart RDF store and activate V1 fallback"
```

## Rollback Procedures

### Rollback Triggers

Initiate rollback if:
- Error rate >5% for >10 minutes
- Fallback rate >20% for >15 minutes
- Search latency p95 >200ms for >15 minutes
- Critical security vulnerability discovered
- Data corruption detected

### Rollback Plan

#### Level 1: Configuration Rollback (<1 hour)

```bash
# Revert to V1 backend
cat > config.yaml <<EOF
marketplace:
  backend: v1
EOF

./deploy-config.sh config.yaml

# Monitor for 15 minutes
./scripts/monitor-rollback.sh --duration 15m

# Verify service restored
./scripts/verify-service-health.sh
```

#### Level 2: Code Rollback (<2 hours)

```bash
# Revert to previous release
git checkout release/marketplace-v1-stable

# Build and deploy
cargo build --release --no-default-features --features marketplace-v1
./deploy-production.sh

# Verify deployment
cargo test --features marketplace-v1
```

#### Level 3: Data Rollback (<4 hours)

```bash
# Restore V1 data from backup
./scripts/restore-v1-data.sh --from-backup latest

# Verify data integrity
./scripts/verify-data-integrity.sh

# Resume service
./scripts/resume-service.sh
```

## Communication Plan

### Stakeholder Communication

#### Users
- **Pre-Launch (1 week before)**: Email announcement of new features
- **Launch Day**: Blog post, changelog, documentation
- **Post-Launch (1 week after)**: Survey for feedback

#### Package Publishers
- **Pre-Launch (2 weeks before)**: Migration guide, signing tutorial
- **Launch Day**: Webinar on new features
- **Post-Launch (ongoing)**: Office hours for support

#### Internal Team
- **Daily**: Standup during rollout
- **Weekly**: Retrospective after each phase
- **Incidents**: Immediate Slack notification

### Incident Communication Template

```markdown
## Incident: [Brief Description]

**Status**: Investigating / Mitigating / Resolved
**Severity**: P0 (Critical) / P1 (High) / P2 (Medium)
**Started**: [Timestamp]
**Affected Users**: [Percentage/Count]

### Impact
[Description of user impact]

### Current Status
[What we're doing right now]

### Timeline
- [Timestamp]: Incident detected
- [Timestamp]: Investigation started
- [Timestamp]: Mitigation applied
- [Timestamp]: Incident resolved

### Next Steps
[What we'll do next]

### Point of Contact
[Name, Slack handle]
```

## Post-Deployment Activities

### Week 1 Post-Launch
- Daily monitoring of key metrics
- Collect and triage user feedback
- Address high-priority bugs
- Tune performance based on real usage

### Month 1 Post-Launch
- Publish performance report (V1 vs V2 comparison)
- Analyze signing adoption
- Optimize based on usage patterns
- Plan for V1 deprecation

### Month 3 Post-Launch
- Remove V1 from default features (opt-in only)
- Announce V1 deprecation timeline
- Encourage migration to V2 signing
- Plan for V1 removal

### Month 6 Post-Launch
- Remove V1 entirely (major version bump)
- V2 is sole backend
- Archive V1 code for historical reference

## Success Metrics

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Zero downtime deployment | 100% | TBD | |
| Error rate increase | <1% | TBD | |
| Performance regression | 0% | TBD | |
| Fallback rate | <5% | TBD | |
| User complaints | <10 | TBD | |
| Signing adoption (1 month) | >10% | TBD | |
| Documentation completeness | 100% | TBD | |
| Rollback count | 0 | TBD | |

## Lessons Learned Template

```markdown
## Marketplace V2 Migration - Lessons Learned

### What Went Well
- [Success 1]
- [Success 2]

### What Could Be Improved
- [Area 1]
- [Area 2]

### Action Items
- [ ] [Action 1] - Owner: [Name] - Due: [Date]
- [ ] [Action 2] - Owner: [Name] - Due: [Date]

### Metrics
- Deployment duration: [X hours]
- Issues encountered: [N]
- User impact: [Description]
- Rollback events: [N]

### Recommendations for Future Migrations
- [Recommendation 1]
- [Recommendation 2]
```

## Conclusion

This deployment and rollout plan ensures:
- **Zero downtime** through gradual rollout and fallback mechanisms
- **Low risk** through extensive testing and monitoring
- **Transparency** through clear communication
- **Quick recovery** through well-defined rollback procedures
- **Continuous improvement** through post-launch analysis and lessons learned
