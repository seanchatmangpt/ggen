# ggen v2.5.0 - Production Deployment Checklist

**Quick Reference Guide for DevOps/SRE Teams**

---

## 🚨 PRE-FLIGHT CHECK - DO NOT DEPLOY WITHOUT THESE

### BLOCKERS (Must Fix Before Production)

- [ ] ❌ **BLOCKER**: Upgrade `ring` to 0.17.12+ (RUSTSEC-2025-0009 - CRITICAL CVE)
- [ ] ❌ **BLOCKER**: Upgrade `wasmtime` to 34.0.2+ (RUSTSEC-2025-0046 - MEDIUM CVE)
- [ ] ❌ **BLOCKER**: Run `cargo audit` and verify ZERO critical vulnerabilities
- [ ] ❌ **BLOCKER**: Document rollback procedure (test it!)

**DO NOT PROCEED if any BLOCKER is unchecked.**

---

## ✅ PHASE 1: STAGING DEPLOYMENT (Safe to proceed now)

### Environment Setup
- [ ] Deploy v2.5.0 to staging environment
- [ ] Configure OpenTelemetry endpoint: `https://otel-collector.internal:4317`
- [ ] Set environment variables:
  ```bash
  export RUST_LOG=info
  export OTEL_SERVICE_NAME=ggen
  export OTEL_SERVICE_VERSION=2.5.0
  ```
- [ ] Verify Docker build:
  ```bash
  docker build -t ggen:2.5.0 -f docker/Dockerfile .
  docker run ggen:2.5.0 utils doctor
  ```

### Functional Validation (32 Commands)
Run these commands and verify output:

**Critical Path (10 commands)**:
- [ ] `ggen template list` → Should return template catalog
- [ ] `ggen project new test-app --type rust-cli` → Creates project
- [ ] `ggen project gen --template service.tmpl` → Generates code
- [ ] `ggen marketplace list` → `{"packages":[],"total":0}` (no panic!)
- [ ] `ggen marketplace search "rust web"` → Search results
- [ ] `ggen hook list` → `{"hooks":[],"total":0}` (no panic!)
- [ ] `ggen utils doctor` → `{"checks_passed":3,"overall_status":"healthy"}`
- [ ] `ggen ai generate "Create handler"` → AI generation works
- [ ] `ggen graph query "SELECT * {?s ?p ?o}"` → SPARQL works
- [ ] `ggen utils env list` → Environment variables

**Extended Validation (22 additional commands)**:
- [ ] All `ai` commands (3): generate, chat, analyze
- [ ] All `graph` commands (4): load, query, export, visualize
- [ ] All `hook` commands (4): create, list, remove, monitor
- [ ] All `marketplace` commands (4): search, install, list, publish
- [ ] All `project` commands (7): new, plan, gen, apply, init, generate, watch
- [ ] All `template` commands (8): show, new, list, lint, generate, regenerate, tree

**Expected Results**:
- ✅ 32/32 commands functional (no panics)
- ✅ Sub-30ms startup time
- ✅ <200ms health check latency

### Performance Testing
- [ ] Run benchmark suite:
  ```bash
  cargo bench --no-fail-fast
  ```
- [ ] Verify p95 latency <500ms for all operations
- [ ] Load test: 100 concurrent requests over 60 seconds
  ```bash
  for i in {1..100}; do
    ggen utils doctor &
  done
  wait
  ```

### Security Validation
- [ ] Run security audit:
  ```bash
  cargo audit
  # Expected: 2 vulnerabilities (ring, wasmtime) - KNOWN
  # Expected: 11 unmaintained warnings - ACCEPTABLE FOR STAGING
  ```
- [ ] Verify no secrets in environment:
  ```bash
  env | grep -i "secret\|password\|token\|key"
  ```
- [ ] Check Docker image vulnerabilities:
  ```bash
  trivy image ggen:2.5.0
  # Or: grype ggen:2.5.0
  ```

### Observability Setup
- [ ] Verify OpenTelemetry spans in monitoring dashboard
- [ ] Check log aggregation (JSON format):
  ```bash
  ggen utils doctor 2>&1 | jq .
  ```
- [ ] Configure alerts:
  - Error rate >0.1% for 5 minutes
  - p95 latency >1000ms for 5 minutes
  - Pod restart count >3 in 1 hour

---

## ⚠️ PHASE 2: CANARY DEPLOYMENT (Only after security patches)

**CRITICAL**: Do NOT proceed until v2.5.1 with security patches is released.

### Pre-Canary Checklist
- [ ] ✅ Security patches applied (v2.5.1+)
- [ ] ✅ Staging validation passed (100% tests)
- [ ] ✅ Rollback procedure documented and tested
- [ ] ✅ Monitoring dashboards configured
- [ ] ✅ On-call rotation scheduled

### Canary Deployment (5% Traffic)
- [ ] Deploy to 5% of production pods
- [ ] Monitor for 48 hours:
  - [ ] Error rate compared to baseline
  - [ ] Latency p50/p95/p99 compared to baseline
  - [ ] Memory usage trend
  - [ ] CPU usage trend
- [ ] Compare against v2.4.0 metrics:
  ```sql
  # Example Prometheus query
  rate(http_request_errors_total{version="2.5.1"}[5m])
  vs
  rate(http_request_errors_total{version="2.4.0"}[5m])
  ```

### Canary Success Criteria
- [ ] Error rate ≤ baseline + 0.1%
- [ ] p95 latency ≤ baseline + 50ms
- [ ] Zero critical alerts triggered
- [ ] No user-reported bugs related to runtime

**If ANY criterion fails → ROLLBACK immediately**

---

## 🚀 PHASE 3: GRADUAL ROLLOUT (Weeks 5-8)

### Week 5: 25% Traffic
- [ ] Increase to 25% of production pods
- [ ] Monitor for 7 days
- [ ] Verify all success criteria met

### Week 6: 50% Traffic
- [ ] Increase to 50% of production pods
- [ ] Monitor for 7 days
- [ ] Run weekly security scan: `cargo audit`

### Week 7: 75% Traffic
- [ ] Increase to 75% of production pods
- [ ] Monitor for 7 days
- [ ] Prepare to decommission v2.4.0

### Week 8: 100% Traffic
- [ ] Deploy to 100% of production
- [ ] Monitor for 14 days
- [ ] Decommission v2.4.0 after validation
- [ ] Update baseline metrics

---

## 🔄 ROLLBACK PROCEDURE

### Automatic Rollback Triggers
Monitor these conditions - if ANY occur, auto-rollback:

- [ ] Error rate >1% sustained for 5 minutes
- [ ] p95 latency >2000ms sustained for 5 minutes
- [ ] Memory usage >90% for 10 minutes
- [ ] Pod crash loop (>3 restarts in 1 hour)

### Manual Rollback Steps
```bash
# 1. Stop new deployments
kubectl rollout pause deployment/ggen

# 2. Rollback to previous version
kubectl rollout undo deployment/ggen

# 3. Verify rollback
kubectl rollout status deployment/ggen
ggen utils doctor  # Should show v2.4.0 behavior

# 4. Document incident
# - Root cause
# - Affected systems
# - Resolution timeline
# - Post-mortem scheduled
```

### Rollback Validation
- [ ] Verify v2.4.0 running: `kubectl get pods -l app=ggen -o yaml | grep image:`
- [ ] Run health check: `ggen utils doctor`
- [ ] Check error rates return to baseline
- [ ] Notify stakeholders of rollback

---

## 📊 MONITORING DASHBOARDS

### Required Metrics

**SLIs (Service Level Indicators)**:
- [ ] Availability: `(successful_requests / total_requests) * 100`
- [ ] Latency: `histogram_quantile(0.95, request_duration_seconds)`
- [ ] Error Rate: `(error_responses / total_requests) * 100`

**SLOs (Service Level Objectives)**:
- [ ] 99.9% availability (43 min downtime/month)
- [ ] p95 latency <500ms
- [ ] Error rate <0.1%

**Alerts**:
- [ ] Critical: Error rate >1% for 5 min
- [ ] Warning: Error rate >0.5% for 10 min
- [ ] Critical: p95 latency >1000ms for 5 min
- [ ] Warning: p95 latency >750ms for 10 min

### Sample Grafana Queries
```promql
# Error rate
rate(ggen_command_errors_total[5m]) / rate(ggen_command_total[5m])

# Command duration p95
histogram_quantile(0.95,
  rate(ggen_command_duration_seconds_bucket[5m])
)

# Active deployments by version
count(kube_pod_info{app="ggen"}) by (version)
```

---

## 🔐 SECURITY CHECKLIST

### Container Security
- [ ] Run Trivy scan: `trivy image ggen:2.5.0`
- [ ] Verify no critical vulnerabilities
- [ ] Check for outdated base images
- [ ] Verify non-root user in Dockerfile

### Dependency Security
- [ ] Run cargo-audit weekly
- [ ] Monitor RustSec advisory database
- [ ] Track unmaintained dependencies (11 currently)
- [ ] Plan upgrades for high-priority CVEs (ring, wasmtime)

### Runtime Security
- [ ] Enable Pod Security Policies/Standards
- [ ] Configure network policies (egress/ingress)
- [ ] Enable audit logging
- [ ] Configure resource limits:
  ```yaml
  resources:
    requests:
      memory: "128Mi"
      cpu: "100m"
    limits:
      memory: "512Mi"
      cpu: "500m"
  ```

---

## 📝 DOCUMENTATION UPDATES

### Required Documentation
- [ ] Update internal runbook with v2.5.0 specifics
- [ ] Document new OpenTelemetry configuration
- [ ] Update disaster recovery plan
- [ ] Document security patch timeline (v2.5.1)
- [ ] Create troubleshooting guide for common issues

### Known Issues to Document
- [ ] Help flag wraps in error message (cosmetic, P1)
- [ ] `utils doctor --fix` placeholder (not implemented)
- [ ] `project watch` uses blocking implementation

---

## 🎯 SUCCESS CRITERIA (Final Gate)

### Before Declaring "Production Ready"
- [ ] ✅ 100% of production traffic on v2.5.1+ (with security patches)
- [ ] ✅ 30 days uptime without critical incidents
- [ ] ✅ SLOs met consistently (99.9% availability, <500ms p95)
- [ ] ✅ Zero critical CVEs in dependencies
- [ ] ✅ Monitoring/alerting validated
- [ ] ✅ Rollback procedure tested successfully
- [ ] ✅ Incident response plan documented

---

## 🚨 EMERGENCY CONTACTS

### Escalation Path

**Tier 1 - Automated Alerts**:
- PagerDuty/OpsGenie integration
- Response: Immediate (on-call engineer)

**Tier 2 - Engineering Support**:
- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Response SLA: <2 hours
- Resolution SLA: <48 hours

**Tier 3 - Critical Security**:
- Email: security@ggen.io (hypothetical)
- Response SLA: <30 minutes
- Emergency patch: <24 hours

---

## 📅 TIMELINE REFERENCE

| Phase | Duration | Start Date | Key Deliverable |
|-------|----------|------------|-----------------|
| Security Patch Development | 2 weeks | Nov 8, 2025 | v2.5.1-rc1 |
| Staging Validation | 1 week | Nov 22, 2025 | Test results |
| Release v2.5.1 | 1 week | Nov 29, 2025 | Production release |
| Canary Deployment | 2 weeks | Dec 6, 2025 | 5% traffic |
| Gradual Rollout | 4 weeks | Dec 20, 2025 | 100% traffic |
| Full Validation | 2 weeks | Jan 17, 2026 | Production ready |

**Total Timeline**: ~10 weeks from today

---

## ✅ FINAL SIGN-OFF

**Before Production Deployment, Get Approval From**:
- [ ] Engineering Lead (code quality validation)
- [ ] Security Team (CVE resolution confirmed)
- [ ] Operations Lead (infrastructure ready)
- [ ] Product Owner (business requirements met)
- [ ] CTO/VP Engineering (final production approval)

**Deployment Authorized By**: ___________________ Date: ___________

**Rollback Authority**: On-call SRE (no approval needed for emergency rollback)

---

**Document Version**: 1.0
**Last Updated**: November 7, 2025
**Next Review**: Post v2.5.1 release (December 2025)
**Owner**: Production Validation Agent (Claude Code)

---

*This checklist follows Fortune 500 production deployment best practices including phased rollouts, automated rollback, and comprehensive monitoring.*
