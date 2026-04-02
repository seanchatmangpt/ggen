<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace V2 Deployment Guide](#marketplace-v2-deployment-guide)
  - [Table of Contents](#table-of-contents)
  - [Pre-Deployment Checklist](#pre-deployment-checklist)
    - [Critical Tasks (Must Complete)](#critical-tasks-must-complete)
    - [Optional Tasks (Recommended)](#optional-tasks-recommended)
  - [Deployment Procedures](#deployment-procedures)
    - [Phase 1: Internal Testing (Week 1)](#phase-1-internal-testing-week-1)
    - [Phase 2: Opt-In Beta (Week 2-3)](#phase-2-opt-in-beta-week-2-3)
    - [Phase 3: A/B Testing (Week 4-5)](#phase-3-ab-testing-week-4-5)
    - [Phase 4: Full Migration (Week 6)](#phase-4-full-migration-week-6)
    - [Phase 5: Deprecation (Week 8+)](#phase-5-deprecation-week-8)
  - [Rollback Procedures](#rollback-procedures)
    - [Emergency Rollback (5 Minutes)](#emergency-rollback-5-minutes)
    - [Planned Rollback (1 Hour)](#planned-rollback-1-hour)
    - [Feature Flag Rollback (Instant)](#feature-flag-rollback-instant)
  - [Monitoring & Validation](#monitoring--validation)
    - [Key Metrics to Monitor](#key-metrics-to-monitor)
    - [Prometheus Queries](#prometheus-queries)
    - [Grafana Dashboard](#grafana-dashboard)
    - [Alert Rules](#alert-rules)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues](#common-issues)
      - [1. Slow Search Queries](#1-slow-search-queries)
      - [2. Ed25519 Signature Verification Failures](#2-ed25519-signature-verification-failures)
      - [3. RDF Store Errors](#3-rdf-store-errors)
      - [4. High Memory Usage](#4-high-memory-usage)
  - [Emergency Contacts](#emergency-contacts)
    - [On-Call Rotation](#on-call-rotation)
    - [Escalation Path](#escalation-path)
    - [Communication Channels](#communication-channels)
  - [Post-Deployment Checklist](#post-deployment-checklist)
    - [Immediate (Day 1)](#immediate-day-1)
    - [Short-Term (Week 1)](#short-term-week-1)
    - [Long-Term (Month 1)](#long-term-month-1)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace V2 Deployment Guide

**Version:** 1.0.0
**Date:** 2025-11-18
**Status:** Production Ready

---

## Table of Contents

1. [Pre-Deployment Checklist](#pre-deployment-checklist)
2. [Deployment Procedures](#deployment-procedures)
3. [Rollback Procedures](#rollback-procedures)
4. [Monitoring & Validation](#monitoring--validation)
5. [Troubleshooting](#troubleshooting)
6. [Emergency Contacts](#emergency-contacts)

---

## Pre-Deployment Checklist

### Critical Tasks (Must Complete)

- [ ] **Fix Clippy Warnings**
  ```bash
  cd /Users/sac/ggen
  cargo fix --package ggen-marketplace-v2 --allow-dirty
  cargo clippy --package ggen-marketplace-v2 -- -D warnings
  ```

- [ ] **Run Security Audit**
  ```bash
  cargo install cargo-audit
  cargo audit --deny warnings
  # Review and patch any findings
  ```

- [ ] **Validate All Tests**
  ```bash
  # V2 tests (must be 100%)
  cargo test --package ggen-marketplace-v2 --lib

  # V1 tests (fix 2 failures)
  cargo test --package ggen-marketplace --lib

  # Integration tests
  cargo test --test marketplace_package_e2e
  ```

- [ ] **Build All Feature Combinations**
  ```bash
  cargo build --release --features marketplace-v1
  cargo build --release --features marketplace-v2
  cargo build --release --features marketplace-parallel
  ```

- [ ] **Backup Production Data**
  ```bash
  DATE=$(date +%Y%m%d_%H%M%S)
  tar -czf ggen_backup_$DATE.tar.gz ~/.cache/ggen ~/.config/ggen
  mv ggen_backup_$DATE.tar.gz ~/backups/
  ```

- [ ] **Create Rollback Binary**
  ```bash
  cargo build --release --features marketplace-v1
  cp target/release/ggen ~/backups/ggen_v1_stable
  chmod +x ~/backups/ggen_v1_stable
  ```

### Optional Tasks (Recommended)

- [ ] **Performance Baseline**
  ```bash
  cargo bench --package ggen-cli -- marketplace
  # Save results for comparison
  ```

- [ ] **Load Testing**
  ```bash
  # Run concurrent search tests
  ./scripts/load_test_marketplace.sh
  ```

- [ ] **Documentation Review**
  ```bash
  cargo doc --no-deps --open
  # Verify all docs are up to date
  ```

---

## Deployment Procedures

### Phase 1: Internal Testing (Week 1)

**Objective:** Validate v2 in controlled environment

**Steps:**

1. **Deploy to Test Environment**
   ```bash
   # Build with v2 enabled
   cargo build --release --features marketplace-v2

   # Install to test location
   cargo install --path . --features marketplace-v2 --root ~/test_install

   # Set up test PATH
   export PATH="$HOME/test_install/bin:$PATH"
   ```

2. **Run Smoke Tests**
   ```bash
   # Basic functionality
   ggen marketplace search --query "rust"
   ggen marketplace list
   ggen marketplace install hello-world --target /tmp/test

   # Verify output
   cat /tmp/test/hello-world/README.md
   ```

3. **Validate Performance**
   ```bash
   # Search latency
   time ggen marketplace search --query "test"  # Should be <200ms

   # List latency
   time ggen marketplace list                    # Should be <100ms

   # Cache warming test
   ggen marketplace search --query "popular"
   time ggen marketplace search --query "popular"  # Should be faster
   ```

4. **Check Metrics**
   ```bash
   # Enable debug logging
   export RUST_LOG=ggen_marketplace_v2=debug

   # Run operations and check logs
   ggen marketplace search --query "test" 2>&1 | grep -i cache
   ggen marketplace search --query "test" 2>&1 | grep -i latency
   ```

**Success Criteria:**
- ✅ All smoke tests pass
- ✅ Performance within SLOs (<200ms search, <100ms list)
- ✅ Cache hit rate >80%
- ✅ Zero crashes or panics
- ✅ Error messages user-friendly

**If Failures:** Fix issues before proceeding to Phase 2

---

### Phase 2: Opt-In Beta (Week 2-3)

**Objective:** Early adopter testing with real workloads

**Steps:**

1. **Announce Beta Program**
   ```markdown
   # In GitHub Discussions / Slack

   🚀 Marketplace V2 Beta Now Available!

   Try the new RDF-backed marketplace with:
   - Faster searches (<200ms)
   - Better caching (85-95% hit rate)
   - Enhanced security (Ed25519 signatures)

   To opt-in:
   cargo install ggen --features marketplace-v2

   Feedback: github.com/your-org/ggen/discussions/v2-beta
   ```

2. **Monitor Beta Users**
   ```bash
   # Set up telemetry dashboard
   # Track:
   # - Error rates
   # - Performance metrics
   # - User feedback
   # - Edge cases
   ```

3. **Collect Feedback**
   ```bash
   # Create feedback form
   # Questions:
   # - Did searches feel faster?
   # - Any errors encountered?
   # - Feature requests?
   # - Would you recommend v2?
   ```

**Success Criteria:**
- ✅ <1% error rate increase vs v1
- ✅ >90% user satisfaction
- ✅ No critical bugs reported
- ✅ Performance equal or better than v1

**If Failures:** Iterate on fixes, extend beta period

---

### Phase 3: A/B Testing (Week 4-5)

**Objective:** Validate v2 at scale with production traffic

**Steps:**

1. **Deploy Parallel Mode**
   ```bash
   cargo build --release --features marketplace-parallel
   cargo install --path . --features marketplace-parallel
   ```

2. **Configure Traffic Split**
   ```rust
   // In marketplace.rs (pseudo-code)
   fn route_request() -> Backend {
       let rand = random::<f32>();
       if rand < 0.5 {
           Backend::V1  // 50% traffic
       } else {
           Backend::V2  // 50% traffic
       }
   }
   ```

3. **Compare Metrics**
   ```bash
   # Collect metrics for both backends
   # Compare:
   # - Latency (p50, p95, p99)
   # - Error rates
   # - Cache hit rates
   # - User satisfaction
   ```

4. **Analyze Results**
   ```sql
   -- Example Prometheus queries

   -- Latency comparison
   histogram_quantile(0.95,
     sum(rate(marketplace_search_latency_bucket[5m])) by (backend, le)
   )

   -- Error rate comparison
   sum(rate(marketplace_errors_total[5m])) by (backend)

   -- Cache hit rate
   avg(marketplace_cache_hit_rate) by (backend)
   ```

**Success Criteria:**
- ✅ V2 latency within 10% of V1 (ideally better)
- ✅ V2 error rate ≤ V1 error rate
- ✅ V2 cache hit rate ≥ 80%
- ✅ No user complaints about v2

**If Failures:** Adjust traffic split, fix issues, re-test

---

### Phase 4: Full Migration (Week 6)

**Objective:** Make v2 the default for all users

**Steps:**

1. **Update Default Feature**
   ```toml
   # In Cargo.toml
   [features]
   default = ["marketplace-v2"]  # Changed from v1
   ```

2. **Deploy Production Build**
   ```bash
   cargo build --release
   cargo install --path .

   # Verify default is v2
   ggen marketplace search --query "test"
   ```

3. **Announce Migration**
   ```markdown
   # Release Notes

   ## ggen v3.2.1 - Marketplace V2 Default

   🎉 Marketplace V2 is now the default backend!

   **What's New:**
   - 2x faster searches (avg <100ms)
   - Enhanced RDF-backed storage
   - Ed25519 cryptographic signatures
   - Smart caching (85-95% hit rate)

   **Rollback:**
   If you experience issues:
   cargo install ggen --features marketplace-v1

   **Report Issues:**
   github.com/your-org/ggen/issues
   ```

4. **Monitor Production**
   ```bash
   # Watch for:
   # - Error rate spikes
   # - Performance degradation
   # - User complaints
   # - System resource usage

   # Set up alerts
   # - Error rate > 1%
   # - P95 latency > 200ms
   # - Cache hit rate < 80%
   ```

**Success Criteria:**
- ✅ Smooth migration with no outages
- ✅ Performance stable or improved
- ✅ <0.5% rollback rate
- ✅ Positive user feedback

**If Failures:** Execute rollback procedure immediately

---

### Phase 5: Deprecation (Week 8+)

**Objective:** Sunset v1 backend after v2 stability proven

**Steps:**

1. **Announce Deprecation**
   ```markdown
   # Deprecation Notice

   Marketplace V1 will be deprecated in 2 months (YYYY-MM-DD)

   **Action Required:**
   Migrate to v2 (default in latest version)

   **Timeline:**
   - Today: V1 deprecated, V2 recommended
   - +1 month: V1 soft deprecation (warnings)
   - +2 months: V1 removed from codebase
   ```

2. **Add Deprecation Warnings**
   ```rust
   #[deprecated(
       since = "3.2.1",
       note = "Use marketplace-v2 feature instead"
   )]
   pub mod marketplace_v1;
   ```

3. **Remove V1 Code (After 2 Months)**
   ```bash
   git rm crates/ggen-marketplace/src/backend/old_*.rs
   # Update Cargo.toml to remove v1 feature
   cargo test --all
   ```

---

## Rollback Procedures

### Emergency Rollback (5 Minutes)

**When to Rollback:**
- Critical bugs in production
- Error rate >5%
- P95 latency >500ms
- Data corruption detected

**Steps:**

1. **Instant Binary Rollback**
   ```bash
   # Use pre-built v1 binary
   cp ~/backups/ggen_v1_stable ~/.cargo/bin/ggen
   chmod +x ~/.cargo/bin/ggen

   # Verify v1 active
   ggen --version
   ggen marketplace search --query "test"
   ```

2. **Notify Team**
   ```bash
   # Send alert to #incidents channel
   # Subject: URGENT - Marketplace V2 Rollback Initiated
   # Reason: [specific issue]
   # ETA: Investigating, v1 restored
   ```

3. **Restore Data (If Needed)**
   ```bash
   # Only if data corruption detected
   DATE="20251118_143000"  # Use latest backup
   tar -xzf ~/backups/ggen_backup_$DATE.tar.gz -C ~/
   ```

4. **Validate Rollback**
   ```bash
   ggen marketplace search --query "test"
   ggen marketplace list
   # Should work as before
   ```

### Planned Rollback (1 Hour)

**When to Use:**
- A/B test shows v2 underperforming
- Beta feedback is negative
- Need more time for fixes

**Steps:**

1. **Rebuild with V1**
   ```bash
   cargo build --release --features marketplace-v1
   cargo install --path . --features marketplace-v1
   ```

2. **Update Documentation**
   ```bash
   # Update README
   # Mark v2 as "experimental"
   # Set expectations for re-release
   ```

3. **Communicate to Users**
   ```markdown
   # Status Update

   We're rolling back to marketplace v1 to address:
   - [Issue 1]
   - [Issue 2]

   Expected timeline for v2 re-release: [date]
   ```

### Feature Flag Rollback (Instant)

**Advantages:**
- No rebuild needed
- No downtime
- Can test both backends simultaneously

**Steps:**

1. **Switch Feature Flag**
   ```toml
   # Cargo.toml
   [features]
   default = ["marketplace-v1"]  # Revert from v2
   ```

2. **Rebuild (If Needed)**
   ```bash
   cargo build --release
   cargo install --path .
   ```

---

## Monitoring & Validation

### Key Metrics to Monitor

**Performance SLOs:**
```
Search Latency (p95):  <200ms  ✅
Lookup Latency (p95):  <100ms  ✅
Cache Hit Rate:        >80%    ✅
Error Rate:            <1%     ✅
Availability:          >99.9%  ✅
```

### Prometheus Queries

```promql
# Search latency p95
histogram_quantile(0.95,
  rate(marketplace_search_latency_bucket[5m])
)

# Error rate
sum(rate(marketplace_errors_total[5m]))
/
sum(rate(marketplace_requests_total[5m]))

# Cache hit rate
sum(rate(marketplace_cache_hits[5m]))
/
sum(rate(marketplace_cache_requests[5m]))

# Availability
avg_over_time(up{job="marketplace"}[5m])
```

### Grafana Dashboard

**Panels:**
1. Search Latency (line graph)
   - p50, p95, p99
   - Target line at 200ms

2. Cache Performance (gauge)
   - Hit rate percentage
   - Target >80%

3. Error Rate (bar chart)
   - Errors per minute
   - Threshold alert at 1%

4. Request Volume (area chart)
   - Requests per second
   - By endpoint

### Alert Rules

```yaml
groups:
  - name: marketplace_slos
    interval: 30s
    rules:
      - alert: HighSearchLatency
        expr: histogram_quantile(0.95, rate(marketplace_search_latency_bucket[5m])) > 200
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Search latency above SLO"

      - alert: LowCacheHitRate
        expr: marketplace_cache_hit_rate < 0.80
        for: 10m
        labels:
          severity: info

      - alert: HighErrorRate
        expr: rate(marketplace_errors_total[5m]) > 0.01
        for: 2m
        labels:
          severity: critical
```

---

## Troubleshooting

### Common Issues

#### 1. Slow Search Queries

**Symptoms:**
- Search takes >200ms
- User complaints about performance

**Diagnosis:**
```bash
# Check cache hit rate
ggen marketplace search --query "test" --verbose
# Look for "cache: HIT" vs "cache: MISS"

# Check RDF store performance
export RUST_LOG=ggen_marketplace_v2::search=debug
ggen marketplace search --query "test"
```

**Solutions:**
```bash
# Increase cache size
export GGEN_CACHE_SIZE=20000  # Default: 10000

# Rebuild FST index
ggen marketplace rebuild-index

# Clear stale cache entries
ggen marketplace cache clear --older-than 1h
```

---

#### 2. Ed25519 Signature Verification Failures

**Symptoms:**
- "Invalid signature" errors
- Package install failures

**Diagnosis:**
```bash
# Verify key format
ggen marketplace verify --package hello-world --verbose

# Check signature length
hexdump -C signature.bin | head
# Should be exactly 64 bytes
```

**Solutions:**
```bash
# Regenerate package signature
ggen marketplace sign --package hello-world --key private.pem

# Verify public key format
openssl pkey -pubin -in public.pem -text
# Should show "ED25519 Public-Key"
```

---

#### 3. RDF Store Errors

**Symptoms:**
- "RDF operation failed" errors
- Corrupted package metadata

**Diagnosis:**
```bash
# Check RDF store integrity
export RUST_LOG=oxigraph=debug
ggen marketplace list --verbose

# Verify disk space
df -h ~/.cache/ggen
```

**Solutions:**
```bash
# Rebuild RDF store
ggen marketplace rebuild-db

# Clear corrupted data
rm -rf ~/.cache/ggen/marketplace.db
ggen marketplace init

# Restore from backup
cp ~/backups/marketplace.db.backup ~/.cache/ggen/marketplace.db
```

---

#### 4. High Memory Usage

**Symptoms:**
- OOM errors
- System slowdown

**Diagnosis:**
```bash
# Check memory usage
top -pid $(pgrep ggen)

# Profile memory
valgrind --tool=massif ggen marketplace search --query "test"
```

**Solutions:**
```bash
# Reduce cache sizes
export GGEN_PACKAGE_CACHE=5000     # Default: 10000
export GGEN_SEARCH_CACHE=2000      # Default: 5000

# Clear cache manually
ggen marketplace cache clear

# Limit RDF store size
export GGEN_RDF_MAX_SIZE=100M
```

---

## Emergency Contacts

### On-Call Rotation

| Role | Primary | Backup |
|------|---------|--------|
| **Marketplace Owner** | @username1 | @username2 |
| **Backend Engineer** | @username3 | @username4 |
| **DevOps** | @username5 | @username6 |
| **Security** | @username7 | @username8 |

### Escalation Path

1. **L1 - On-Call Engineer** (0-15 min)
   - Attempt standard fixes
   - Check runbook
   - Gather diagnostics

2. **L2 - Marketplace Owner** (15-30 min)
   - Decide rollback vs fix
   - Coordinate team response
   - Communicate to users

3. **L3 - Engineering Leadership** (30+ min)
   - Major incident coordination
   - External communication
   - Post-mortem planning

### Communication Channels

- **Slack:** #marketplace-incidents
- **PagerDuty:** marketplace-v2-oncall
- **Status Page:** status.yourdomain.com
- **Email:** marketplace-team@yourdomain.com

---

## Post-Deployment Checklist

### Immediate (Day 1)

- [ ] Verify all smoke tests pass
- [ ] Check error rates (<1%)
- [ ] Monitor performance metrics
- [ ] Review user feedback
- [ ] No critical alerts triggered

### Short-Term (Week 1)

- [ ] Analyze A/B test results
- [ ] Review cache hit rates
- [ ] Check system resource usage
- [ ] Collect user satisfaction data
- [ ] Document lessons learned

### Long-Term (Month 1)

- [ ] Deprecate v1 feature
- [ ] Update documentation
- [ ] Archive old code
- [ ] Plan future enhancements
- [ ] Conduct retrospective

---

**Document Version:** 1.0.0
**Last Updated:** 2025-11-18
**Owner:** Marketplace Team
**Review Date:** 2025-12-18
