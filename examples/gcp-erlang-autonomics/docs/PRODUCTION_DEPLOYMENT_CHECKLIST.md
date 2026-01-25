# Production Deployment Checklist

## Erlang Autonomics BEAM on GCP - Pre-Launch Validation

**Deployment Date**: _________
**Environment**: Production
**Approver**: _________

---

## Phase 1: Pre-Deployment Verification (Week -1)

- [ ] **1.1 GCP Account & Project**
  - [ ] Project created and billing enabled
  - [ ] Required APIs enabled (Container, Pub/Sub, Monitoring, Logging)
  - [ ] Service account created with minimal permissions
  - [ ] Keys rotated and securely stored

- [ ] **1.2 Kubernetes Cluster**
  - [ ] GKE cluster provisioned (3+ nodes, n1-standard-2 or higher)
  - [ ] Network policies enabled
  - [ ] Workload identity enabled
  - [ ] Pod security policies enforced

- [ ] **1.3 Monitoring & Logging**
  - [ ] Cloud Logging enabled for GKE
  - [ ] Cloud Monitoring configured
  - [ ] Alerting rules defined (high CPU, memory, error rate)
  - [ ] PagerDuty/Slack integration tested

- [ ] **1.4 BEAM Release**
  - [ ] Release built and tested locally
  - [ ] Container image built and scanned for vulnerabilities
  - [ ] Image pushed to GCR with version tags
  - [ ] Release notes prepared and reviewed

- [ ] **1.5 Configuration**
  - [ ] sys.config reviewed and tuned for production
  - [ ] vm.args optimized for cloud environment
  - [ ] GCP credentials secured in Secret Manager
  - [ ] TLS certificates provisioned and valid

---

## Phase 2: Staging Deployment (Week 0, Day 1)

- [ ] **2.1 Deploy to Staging**
  - [ ] Apply K8s manifests to staging cluster
  - [ ] Verify StatefulSet creation and pod startup
  - [ ] Confirm all 3 pods reach Running state
  - [ ] Check logs for errors

- [ ] **2.2 Cluster Health**
  - [ ] Verify BEAM clustering (all nodes connected)
  - [ ] Check inter-node communication
  - [ ] Verify headless service DNS resolution
  - [ ] Test EPMD on port 4369

- [ ] **2.3 Governor Initialization**
  - [ ] Cost Guard governor starts and initializes
  - [ ] Rollback Guard governor starts and initializes
  - [ ] Backlog Valve governor starts and initializes
  - [ ] Receipt ledger accepting entries

- [ ] **2.4 Signal Ingestion**
  - [ ] Pub/Sub subscription configured
  - [ ] Cloud Function or Cloud Run handler pushing test signals
  - [ ] Signals routing to correct governors
  - [ ] Receipt chain verified end-to-end

- [ ] **2.5 Load Testing**
  - [ ] 100 concurrent tenants created
  - [ ] 1,000 signals/second ingested
  - [ ] p99 response time < 100ms
  - [ ] Governor state transitions verified
  - [ ] Memory stable (no unbounded growth)

- [ ] **2.6 Failover Testing**
  - [ ] Kill one pod, observe cluster recovery
  - [ ] Kill two pods, verify quorum holds
  - [ ] Add new pod, verify automatic clustering
  - [ ] Network partition simulation

---

## Phase 3: Production Deployment (Week 0, Day 2-3)

- [ ] **3.1 Pre-Deployment Checklist**
  - [ ] All staging tests passed
  - [ ] Performance SLOs met (startup < 5s, response < 100ms)
  - [ ] Security audit completed
  - [ ] Change control ticket approved
  - [ ] Rollback plan documented

- [ ] **3.2 Apply to Production**
  - [ ] Deploy to production cluster (dry-run first)
  - [ ] Monitor rollout progress
  - [ ] Verify all pods reach Running state
  - [ ] Confirm cluster formation

- [ ] **3.3 Production Validation**
  - [ ] Health endpoints responding
  - [ ] Metrics flowing to Cloud Monitoring
  - [ ] Logs flowing to Cloud Logging
  - [ ] Pub/Sub signal ingestion working
  - [ ] Receipt chain verified

- [ ] **3.4 Customer Signal**
  - [ ] Real customer signals flowing
  - [ ] Governor FSMs processing signals
  - [ ] Actions executing (throttle, rollback, etc.)
  - [ ] Receipts persisting to storage

- [ ] **3.5 Monitoring Setup**
  - [ ] Dashboards created in Cloud Monitoring
  - [ ] Alerts routing to on-call
  - [ ] Runbooks linked in alerts
  - [ ] SLO dashboards active

---

## Phase 4: Post-Deployment (Week 1)

- [ ] **4.1 Stability Monitoring**
  - [ ] Monitor error rates (target: < 0.1%)
  - [ ] Monitor p99 latency (target: < 200ms)
  - [ ] Monitor memory (target: stable at 400-600MB)
  - [ ] Monitor CPU (target: < 70% p95)

- [ ] **4.2 Incident Response**
  - [ ] No critical incidents in first 24h
  - [ ] On-call playbooks tested
  - [ ] Escalation procedures validated
  - [ ] Communication channels working

- [ ] **4.3 Documentation**
  - [ ] Runbooks accessible to team
  - [ ] Troubleshooting guide reviewed
  - [ ] Architecture diagrams updated
  - [ ] Knowledge transfer completed

- [ ] **4.4 Optimization**
  - [ ] Identify any performance issues
  - [ ] Tune JVM parameters if needed
  - [ ] Optimize Pub/Sub batch sizes
  - [ ] Review and optimize hot paths

---

## Risk Mitigation

### High-Risk Items

| Risk | Mitigation | Owner | Status |
|------|-----------|-------|--------|
| Clustering fails | Manual node restart procedure documented | DevOps | ✓ |
| Receipt ledger corruption | Backup to GCS every 10 minutes | DevOps | ✓ |
| Memory leak | Auto-restart daily at 2 AM | DevOps | ✓ |
| Pub/Sub delay | Implement exponential backoff + DLQ | Platform | ✓ |
| TLS cert expiry | Auto-renewal 30 days before expiry | Security | ✓ |

### Rollback Plan

If critical issues occur:

1. **Immediate (< 5 min)**:
   - Scale StatefulSet to 0 replicas
   - Remove load balancer routing
   - Notify stakeholders

2. **Analysis (5-30 min)**:
   - Gather logs and metrics
   - Identify root cause
   - Determine fix strategy

3. **Rollback (30-60 min)**:
   - Revert image to previous known-good version
   - Re-enable load balancer routing
   - Monitor for stability

4. **Post-Incident (24 h)**:
   - Conduct blameless postmortem
   - Document findings
   - Implement preventative measures

---

## Sign-Off

**Deployment Sponsor**: _________________________ Date: _________

**Platform Lead**: _________________________ Date: _________

**Security Lead**: _________________________ Date: _________

**Operations Lead**: _________________________ Date: _________

---

## Post-Deployment Notes

```
[Deployment Date]: [Start Time]
[Issues Encountered]:
[Resolution Time]:
[Final Status]: LIVE / ROLLED BACK
[Approvers]:
```

---

## Contact & Escalation

- **On-Call**: [Phone/Slack]
- **Platform Team Slack**: #erlang-autonomics
- **Incident Commander**: [Contact]
- **Escalation Path**: Platform Lead → VP Engineering → CEO
