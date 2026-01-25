# Deployment Runbook for TPS Systems

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: Release engineers, DevOps engineers
**Deployment Strategy**: Canary → Staging → Production (Blue-Green)

---

## Pre-Deployment Checklist (24 hours before)

- [ ] Code review completed (2+ reviewers)
- [ ] All tests passing (unit, integration, e2e)
- [ ] No known bugs or regressions
- [ ] Performance benchmarks run (no regressions)
- [ ] Deployment plan documented
- [ ] Rollback plan documented
- [ ] Team notified of deployment window
- [ ] Customer communication ready (if necessary)
- [ ] On-call engineer available during deployment
- [ ] Monitoring dashboard ready to watch

---

## Deployment Strategy: Canary + Staging + Production

### Phase 1: Canary Deployment (10% of traffic)

**Duration**: 10-15 minutes
**Risk Level**: Low (only 10% of users affected)

**Steps**:

1. **Trigger Canary** (0-2 min)
   ```
   kubectl set image deployment/api api=api:v2.1.0
   kubectl patch deployment/api -p "{\"spec\":{\"replicas\":2}}"
   # Now: 2 old + 2 new = 50/50 traffic split
   Wait for pods to be ready
   ```

2. **Validate Canary** (2-10 min)
   - Check dashboard: Error rate still < 0.1%?
   - Check traces: Latency still < SLO?
   - Check logs: Any errors?
   - Check metrics: Request rate normal?
   - Health checks: All green?

   **Automated checks**:
   ```bash
   # Run smoke tests against canary
   make test-canary

   # Check metrics
   - Error rate < 0.1%?
   - Latency p99 < SLO?
   - Throughput normal?
   ```

3. **Decision** (10 min)
   - ✅ All good: Proceed to staging
   - ❌ Problem: Rollback to old version, investigate

   **Rollback (if needed)**:
   ```bash
   kubectl set image deployment/api api=api:v2.0.0
   # Revert to old version
   ```

---

### Phase 2: Staging Deployment (50% of traffic)

**Duration**: 15-20 minutes
**Risk Level**: Medium (half of users might be affected)

**Steps**:

1. **Increase Canary to Staging** (0-2 min)
   ```
   kubectl patch deployment/api -p "{\"spec\":{\"replicas\":4}}"
   # Now: 4 old + 4 new = 50/50 traffic split
   Wait for pods to be ready
   ```

2. **Extended Validation** (2-15 min)
   - Run full test suite (not just smoke tests)
   - Monitor for 10-15 minutes
   - Check for performance regressions
   - Check database queries (any slow queries?)
   - Check external dependencies (all responding?)

   **Extended checks**:
   ```bash
   # Run full integration tests
   make test-integration

   # Check performance
   - p50 latency: Expected?
   - p99 latency: Expected?
   - Throughput: Expected?
   - Error rate: Expected?

   # Check resource usage
   - CPU: Normal?
   - Memory: Normal?
   - Disk: Normal?
   ```

3. **Decision** (15 min)
   - ✅ All good: Proceed to production
   - ⚠️ Degradation: Investigate, maybe revert
   - ❌ Problem: Rollback immediately

---

### Phase 3: Production Deployment (100% of traffic)

**Duration**: 10-15 minutes
**Risk Level**: High (all users affected)

**Steps**:

1. **Complete Rollout** (0-5 min)
   ```
   kubectl patch deployment/api -p "{\"spec\":{\"replicas\":8}}"
   # Now: 8 new instances (old instances scaled down)
   Wait for all pods to be ready
   ```

2. **Final Validation** (5-10 min)
   - All pods healthy?
   - Error rate < 0.1%?
   - Latency p99 < SLO?
   - Throughput = expected?
   - All metrics green?

3. **Cleanup** (10-15 min)
   - Scale down old version to 0
   - Clean up intermediate replicas
   - Verify no residual resources

---

## Deployment Monitoring

### During Deployment

**Dashboard Views**:
1. Error rate: Should stay < 0.1%
2. Latency: p99 should stay < SLO
3. Throughput: Should match pre-deployment
4. Queue depth: Should be stable
5. Worker utilization: Should be normal
6. Resource usage: CPU, memory, disk normal?

**Alert Thresholds** (auto-rollback if triggered):
```
If during deployment:
- Error rate > 1.0% for 2 minutes → Rollback
- Latency p99 > 3x baseline for 2 minutes → Rollback
- Circuit breaker open for 2+ minutes → Rollback
- OOM (out of memory) error → Rollback
```

### Rollback Procedure

```bash
# If anything looks wrong, rollback immediately
kubectl set image deployment/api api=api:v2.0.0

# Monitor rollback
- Should take 2-3 minutes
- Error rate should drop
- Latency should return to normal

# After rollback:
- Investigation: Why did it fail?
- Staging test: Why didn't staging catch it?
- Fix: What code change is needed?
```

---

## Deployment Decision Tree

```
Ready to deploy?

┌─ Pre-deployment checks passed?
│  ├─ No → Fix issues, run tests again
│  └─ Yes → Proceed to canary
│
├─ Canary metrics good?
│  ├─ No → Investigate issue
│  │        ├─ Code bug? → Rollback, fix, re-deploy
│  │        ├─ Staging issue? → Run different test scenario
│  │        └─ Unknown → Keep canary, investigate longer
│  └─ Yes → Proceed to staging
│
├─ Staging metrics good?
│  ├─ No → Investigate issue
│  │        ├─ Performance degradation? → Optimize, re-deploy
│  │        ├─ New bug found? → Rollback, fix, re-deploy
│  │        └─ Intermittent issue? → Extended staging (20 min)
│  └─ Yes → Proceed to production
│
├─ Production metrics good?
│  ├─ No → Rollback immediately (auto-rollback triggered)
│  └─ Yes → Deployment complete
│
└─ Monitor for 1 hour post-deployment
   ├─ All metrics normal?
   │  └─ Yes → Declare success, stand down
   └─ Issues found?
      └─ Rollback if serious, investigate if minor
```

---

## Deployment Checklist Template

**Date**: ___________
**Version**: _________
**Engineer**: ________

### Pre-Deployment (24h before)
- [ ] Code review completed
- [ ] Tests passing
- [ ] Performance benchmarks: ________
- [ ] Team notified
- [ ] On-call available
- [ ] Rollback plan documented: ________

### Canary (10% traffic)
- [ ] Deployment triggered at ___:___
- [ ] Pods ready: ____ pods, status: ______
- [ ] Error rate: _____ (target: < 0.1%)
- [ ] Latency p99: _____ (target: < ____ms)
- [ ] Throughput: _____ req/sec (target: _____)
- [ ] Health check: ✅ All green? Y/N
- [ ] Decision: ☐ Proceed ☐ Rollback

### Staging (50% traffic)
- [ ] Staging started at ___:___
- [ ] Pods ready: ____ pods, status: ______
- [ ] Error rate: _____ (target: < 0.1%)
- [ ] Latency p99: _____ (target: < ____ms)
- [ ] Throughput: _____ req/sec
- [ ] Resource usage: CPU ___%, Mem ___%, Disk ___% (all < 80%?)
- [ ] Test results: Passed? Y/N
- [ ] Decision: ☐ Proceed ☐ Rollback

### Production (100% traffic)
- [ ] Production started at ___:___
- [ ] Pods ready: ____ pods, status: ______
- [ ] Error rate: _____ (target: < 0.1%)
- [ ] Latency p99: _____ (target: < ____ms)
- [ ] Throughput: _____ req/sec
- [ ] Resource usage: CPU ___%, Mem ___%, Disk ___% (all < 80%?)
- [ ] Deployment complete at ___:___
- [ ] Deployment successful: Y/N

### Post-Deployment
- [ ] Monitoring for 1 hour
- [ ] No issues found: Y/N
- [ ] If issues, describe: _________________
- [ ] Stand down at ___:___

### Lessons Learned
- [ ] What went well: __________________
- [ ] What to improve: __________________
- [ ] Action items: __________________

---

## Emergency Deployment (Hotfix)

**When to Use**: Critical bug found in production, need immediate fix

**Differences from Normal Deployment**:
- Skip some staging (go straight to canary or production)
- Shorter deployment window
- Higher risk, but critical issue requires it

### Emergency Deployment Procedure

1. **Assess Criticality** (immediately)
   - Is service completely broken? → Emergency
   - Is service degraded? → Might not be emergency
   - Is there a workaround? → Use workaround first

2. **Fix Code** (5-10 minutes)
   - Make minimal fix (not refactoring)
   - Tests must pass (at least unit tests)
   - Code review: If possible, get review. If not possible, acknowledge risk.

3. **Build & Test** (5 minutes)
   - Build new image
   - Run smoke tests
   - Verify no obvious issues

4. **Canary Deployment** (5 minutes)
   - Deploy to 10% of traffic
   - Monitor for 2-5 minutes (shorter window)
   - Watch error rate and latency closely

5. **Full Rollout** (5 minutes)
   - If canary good, roll out to 100%
   - If canary bad, rollback immediately

6. **Post-Deployment** (1-2 hours)
   - Monitor closely (might have missed something)
   - Plan proper fix for next release
   - Schedule code review if skipped

---

## Database Migrations During Deployment

**Risk**: Database migration goes wrong, data loss possible

**Strategy**: Decouple deployment from migration

1. **Pre-deployment** (1-24 hours before)
   - Run migration in staging (verify it works)
   - Back up production database
   - Test rollback of migration (can you undo it?)

2. **Migration Window** (before or after deployment)
   - Option A: Migrate before deployment (old code + new schema)
   - Option B: Deploy new code, then migrate (new code + new schema)
   - Wait for migration to complete and verify

3. **Deployment** (after migration verified)
   - Deploy new code
   - New code expects new schema (or is backward-compatible)

4. **Rollback Plan**
   - If migration fails: Rollback migration, keep old code
   - If code fails: Rollback code, keep new schema
   - If both fail: Major incident, escalate

---

## Safe Defaults for Deployment

**Never Deploy When**:
- [ ] Tests not passing
- [ ] Code not reviewed
- [ ] On-call engineer not available
- [ ] Monitoring not ready
- [ ] Rollback plan not documented
- [ ] No permission from team lead

**Always Do**:
- [ ] Notify team before deployment
- [ ] Have monitoring dashboard open
- [ ] Have rollback command ready to run
- [ ] Document what you're deploying
- [ ] Have on-call available during deployment
- [ ] Monitor for 30-60 minutes after deployment

---

**End of Deployment Runbook**
