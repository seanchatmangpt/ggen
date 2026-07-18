# Marketplace Operations Runbook

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Production-Ready
**Audience**: DevOps, SREs, On-Call Engineers

---

## Table of Contents

1. [Overview & Responsibilities](#overview--responsibilities)
2. [Health Monitoring Dashboard](#health-monitoring-dashboard)
3. [Alert Thresholds & Escalation](#alert-thresholds--escalation)
4. [On-Call Troubleshooting Guide](#on-call-troubleshooting-guide)
5. [Common Incidents & Recovery](#common-incidents--recovery)
6. [Governor Health Checks](#governor-health-checks)
7. [Operational Procedures](#operational-procedures)
8. [Metrics Reference](#metrics-reference)

---

## Overview & Responsibilities

### Mission-Critical Governors

The Marketplace Orchestrator coordinates 8 finite state machines (FSMs):

| Governor | Purpose | SLO | Owner |
|----------|---------|-----|-------|
| **Entitlement** | SKU activation/revocation, feature access control | 99.95% | @marketplace-team |
| **Billing** | Payment processing, invoicing, revenue recognition | 99.99% | @payments-team |
| **Product Catalog** | SKU definitions, pricing models, features | 99.9% | @product-ops |
| **Subscription** | Lifecycle (trial → active → renewal → upgrade → cancel) | 99.95% | @operations-team |
| **Customer Account** | Profile, payment methods, communication preferences | 99.9% | @customer-success |
| **Quota/SLA** | Resource limits, usage tracking, throttling | 99.95% | @platform-ops |
| **Compliance** | KYC/AML verification, fraud detection, data retention | 99.99% | @compliance-team |
| **Multi-Tenant** | Isolation verification, cross-tenant risk assessment | 99.95% | @infrastructure-team |

### On-Call Rotation

**Escalation Path**:
```
Tier 1 (SRE On-Call)
  ↓ (Page on-call if not resolved in 5 min)
Tier 2 (Engineering Manager)
  ↓ (Page on-call if not resolved in 15 min)
Tier 3 (Director of Engineering)
  ↓ (Page on-call if not resolved in 30 min)
Tier 4 (VP Engineering - Business-Critical only)
```

---

## Health Monitoring Dashboard

### Real-Time Dashboard

**URL**: `https://console.cloud.google.com/monitoring/dashboards/custom/marketplace-governors`

**Key Metrics**:

```
┌─────────────────────────────────────────────────────────┐
│ MARKETPLACE ORCHESTRATOR - HEALTH DASHBOARD             │
├─────────────────────────────────────────────────────────┤
│                                                         │
│ System Status: GREEN ✓                                  │
│ Uptime: 99.97% (52 minutes downtime in past 30 days)   │
│                                                         │
│ Orchestrator State:  Idle (accepting events)            │
│ Event Queue Depth:   127/1000 (12.7%)                   │
│ Avg Decision Time:   423ms (SLO: <500ms)                │
│ Error Rate:         0.03% (SLO: <0.1%)                  │
│                                                         │
├─────────────────────────────────────────────────────────┤
│ GOVERNOR STATUS                                         │
├─────────────────────────────────────────────────────────┤
│ ✓ Entitlement:      HEALTHY (1248 active transitions)  │
│ ✓ Billing:          HEALTHY (Batch 847 success/847)    │
│ ✓ Product Catalog:  HEALTHY (218 SKUs monitored)       │
│ ✓ Subscription:     HEALTHY (12,487 active)            │
│ ✓ Customer Account: HEALTHY (User data consistent)     │
│ ✓ Quota/SLA:        HEALTHY (98.2% quota available)    │
│ ✓ Compliance:       HEALTHY (0 violations detected)     │
│ ⚠ Multi-Tenant:     CAUTION (Tenant-008: 87% CPU)      │
│                                                         │
├─────────────────────────────────────────────────────────┤
│ TOP ALERTS (Past 24 Hours)                              │
├─────────────────────────────────────────────────────────┤
│ 1. Multi-Tenant: Tenant-008 CPU at 87% (HIGH)          │
│    → Action: Rebalance workload (see troubleshooting)   │
│ 2. Billing: Stripe API rate limit warning (MEDIUM)     │
│    → Action: Monitor payment queue, may need scaling    │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Key Metrics by Governor

#### Entitlement Governor
```
entitlement_fsm_transitions_total
  → Label: from_state, to_state, success
  → Threshold: >10 failures/hour → ALERT

entitlement_pending_approval_duration
  → Histogram: [0, 10, 24, 48, 72] hours
  → Threshold: P99 >72h → Escalate to manager

entitlement_activation_latency
  → Histogram: [ms]
  → SLO: P99 <5s
  → Threshold: >10s → Page on-call
```

#### Billing Governor
```
billing_payment_success_rate
  → Gauge: 0-100%
  → SLO: >99%
  → Threshold: <98% for 10 min → CRITICAL ALERT

billing_retry_depth
  → Counter: Retry1, Retry2, Retry3, CollectionAgency
  → Threshold: Retry3 >10/hour → Investigate fraud

billing_invoice_generation_time
  → Histogram: [ms]
  → SLO: P99 <2s
  → Threshold: >5s → Escalate to billing team

billing_total_revenue_processed
  → Gauge: USD
  → Threshold: Sudden drop >20% → Investigate payment processor
```

#### Product Catalog Governor
```
product_state_count
  → Label: state (Draft, Published, Featured, Deprecated, Archived)
  → Threshold: Unexpected state distribution → Investigate

product_validation_failures
  → Counter: SKU schema, pricing, feature conflicts
  → Threshold: >5 failures/hour → Page on-call

product_update_propagation_time
  → Histogram: [ms]
  → SLO: P99 <10s
  → Threshold: >30s → Investigate cache invalidation
```

#### Subscription Governor
```
subscription_lifecycle_transitions
  → Label: from_state, to_state, customer_tier
  → Threshold: Unusual patterns → Investigate churn

subscription_proration_accuracy
  → Gauge: 0-100%
  → SLO: 100% (cryptographic verification)
  → Threshold: <100% → CRITICAL: Billing issue

subscription_renewal_success_rate
  → Gauge: 0-100%
  → SLO: >98%
  → Threshold: <97% for 1 hour → Page billing team
```

#### Quota/SLA Governor
```
quota_utilization_by_tenant
  → Label: tenant_id, tier, resource_type
  → Threshold: >90% for 5 min → Warn customer

quota_throttling_events
  → Counter: tenant_id, duration_ms
  → Threshold: >100 events/hour → Investigate spike

sla_violation_count
  → Counter: customer_id, slo_target, actual_value
  → SLO: Zero SLA violations
  → Threshold: Any violation → Page on-call + notify customer
```

#### Compliance Governor
```
compliance_framework_status
  → Label: framework (GDPR, HIPAA, SOC2, PCI-DSS, FedRAMP)
  → Threshold: Any non-compliant → CRITICAL ALERT

kyc_aml_verification_time
  → Histogram: [ms]
  → SLO: <30s
  → Threshold: >60s → Investigate verification service

fraud_detection_alerts
  → Counter: risk_category (payment_anomaly, account_takeover, etc)
  → Threshold: >5 alerts/hour → Page security team

data_retention_expiration
  → Gauge: days_until_retention_expiration
  → Threshold: <30 days → Initiate retention review
```

#### Multi-Tenant Governor
```
tenant_resource_contention_events
  → Counter: cause (noisy_neighbor, cascade_prevention)
  → Threshold: >3 events/hour → Investigate

cascade_prevention_activations
  → Counter: severity (degradation, emergency_shutdown)
  → SLO: <1 per day
  → Threshold: >2 per day → Investigate capacity

fair_share_allocation_accuracy
  → Gauge: 0-100%
  → SLO: 100% (deterministic allocation)
  → Threshold: <100% → CRITICAL: Billing fairness issue

noisy_neighbor_detection_accuracy
  → Gauge: true_positive_rate, false_positive_rate
  → SLO: TP >95%, FP <1%
  → Threshold: Outside SLO → Retrain detection model
```

---

## Alert Thresholds & Escalation

### Critical Alerts (Page immediately)

**Alert #1: Orchestrator Unresponsive**
```
Condition: No heartbeat for 60 seconds
Threshold: CRITICAL
Escalation: Immediate page to Tier 1
Recovery: Restart orchestrator container
Evidence: Cloud Logs, Container metrics
```

**Alert #2: Governor Crash Loop**
```
Condition: Governor state = ERROR for >2 min
Threshold: CRITICAL
Escalation: Page Tier 2 (Engineering Manager)
Recovery: Manual governor restart + root cause analysis
Evidence: Governor logs, state machine trace
```

**Alert #3: Payment Processing Failure**
```
Condition: billing_payment_success_rate < 95% for 10 min
Threshold: CRITICAL ($$$)
Escalation: Page payments team + business ops
Recovery: Failover to backup payment processor
Evidence: Stripe webhook logs, reconciliation report
```

**Alert #4: Data Consistency Violation**
```
Condition: Idempotency check fails OR state mismatch detected
Threshold: CRITICAL (Data integrity)
Escalation: Page Tier 3 + Database team
Recovery: Database consistency check + replay transactions
Evidence: Audit trail, event logs, database checksums
```

**Alert #5: Security Event**
```
Condition: Unauthorized access attempt OR compliance violation detected
Threshold: CRITICAL (Security)
Escalation: Page security team + Tier 2
Recovery: Investigate + block suspicious access
Evidence: IAM logs, compliance audit trail
```

### High Alerts (Page within 5 min if not resolved)

**Alert #H1: Decision Latency SLO Miss**
```
Condition: decision_latency_p99 > 5 sec for 5 min
Threshold: HIGH
Escalation: Page Tier 1 (non-urgent, business impact low)
Recovery: Investigate bottleneck, scale if needed
Evidence: Cloud Trace, profiler output
Investigate: [See section 5.2]
```

**Alert #H2: Event Queue Backing Up**
```
Condition: orchestrator_event_queue_depth > 80% for 5 min
Threshold: HIGH
Escalation: Page Tier 1
Recovery: Increase orchestrator replicas
Evidence: Pub/Sub metrics, event processing rate
```

**Alert #H3: Governor Response Timeout**
```
Condition: governor_coordination_timeout > 3 sec for 10 min
Threshold: HIGH
Escalation: Page Tier 1
Recovery: Investigate slow governor, check logs
Evidence: Governor logs, coordination metrics
```

**Alert #H4: Retry Depth Escalation**
```
Condition: billing_retry_3_count > 50 in 1 hour
Threshold: HIGH
Escalation: Page payments team
Recovery: Investigate payment failures, contact processor
Evidence: Payment logs, processor responses
```

### Medium Alerts (Email owner, no page)

**Alert #M1: CPU/Memory Utilization**
```
Condition: CPU > 75% for 15 min OR Memory > 80% for 15 min
Threshold: MEDIUM
Escalation: Email DevOps team
Recovery: Scale up resources if sustained
Evidence: Cloud Monitoring, container metrics
```

**Alert #M2: Error Rate Spike**
```
Condition: error_rate > 0.5% for 10 min
Threshold: MEDIUM
Escalation: Email dev team
Recovery: Investigate error cause, deploy fix if needed
Evidence: Application logs, error aggregation tool
```

**Alert #M3: Cost Forecast Overage**
```
Condition: monthly_cost_forecast > 110% of budget
Threshold: MEDIUM
Escalation: Email finance + engineering leads
Recovery: Investigate cost driver, optimize if needed
Evidence: GCP Billing, cost analysis reports
```

---

## On-Call Troubleshooting Guide

### Decision Tree: System Not Responding

```
Is the Marketplace Orchestrator container running?
├─ NO → Check Cloud Run service
│  ├─ Service in STOPPED state?
│  │  └─ START the service (see section 6.1)
│  ├─ Service crashing?
│  │  └─ Check logs, deploy previous stable version (see section 6.2)
│  └─ Service in unknown state?
│     └─ Force restart (see section 6.3)
│
└─ YES → Check if it's accepting events
   ├─ Event queue backed up (>90%)?
   │  ├─ Check if governors are responsive
   │  │  └─ If any governor unresponsive, restart it (see section 5.2)
   │  └─ Scale up orchestrator replicas (see section 6.4)
   │
   └─ Receiving events but not processing?
      ├─ Check orchestrator logs for errors
      │  └─ If state machine corruption, initiate recovery (see section 7.2)
      └─ Check database connectivity
         └─ If database unavailable, failover to replica (see section 6.5)
```

### Decision Tree: Payment Processing Failures

```
Payment success rate dropped below 95%?
├─ Check Stripe API status
│  ├─ Stripe is DOWN
│  │  └─ Enable fallback payment processor (see section 7.1)
│  └─ Stripe is UP
│     └─ Check Stripe API logs
│        ├─ Rate limiting errors?
│        │  └─ Increase API throttle allowance (see section 6.6)
│        ├─ Card processing errors?
│        │  └─ Check for fraud filter activation (see section 7.3)
│        └─ Network/timeout errors?
│           └─ Investigate connectivity, enable circuit breaker (see section 6.7)
│
└─ Check our side
   ├─ Payment request validation failing?
   │  └─ Investigate schema changes, review error logs
   ├─ Idempotency key collisions?
   │  └─ Check for duplicate event processing (see section 5.3)
   └─ Database transaction rollbacks?
      └─ Check database logs for locks/deadlocks (see section 7.4)
```

### Decision Tree: Noisy Neighbor Detected

```
Multi-Tenant Governor detected resource contention?
├─ Identify the noisy tenant
│  ├─ Retrieve metrics:
│  │  ggen audit-export --governor multi-tenant --format json > /tmp/mt-metrics.json
│  └─ Parse output to find top CPU/memory/network consumer
│
├─ Assess impact
│  ├─ Estimated impact on other tenants?
│  │  └─ If <5%, monitor and allow self-healing
│  ├─ Estimated impact >5% and sustained >5 min?
│  │  └─ Initiate graceful degradation (see section 7.5)
│  └─ Impact >10% or cascading?
│     └─ Emergency shutdown (see section 7.6)
│
└─ Resolution options
   ├─ 1. Auto-rebalancing (preferred)
   │     → Monitor metrics, should improve in <2 min
   ├─ 2. Graceful degradation
   │     → Reduce noisy tenant to 50% quota, monitor
   ├─ 3. Emergency shutdown + manual intervention
   │     → Restart noisy tenant in isolated environment
   └─ 4. Escalate to customer
       → Contact noisy tenant, request to reduce usage
```

---

## Common Incidents & Recovery

### Incident #1: Governor State Corruption

**Symptoms**:
- Governor stuck in invalid state
- State transition validation failures
- Idempotency errors on valid requests

**Root Cause**:
- Database transaction rollback without state machine update
- Concurrent event processing with race condition
- Corrupted receipt ledger

**Recovery Steps**:
```bash
# 1. Identify corrupted governor
ggen audit-export --governor <name> --status corrupted

# 2. Export audit trail
ggen audit-export --governor <name> --format json > /tmp/audit-trail.json

# 3. Identify last valid state
cat /tmp/audit-trail.json | jq '.entries[] | select(.status == "success") | .to_state' | tail -1

# 4. Manually reset governor to last valid state
ggen governor-reset --governor <name> --to-state <state>

# 5. Replay events from recovery point
ggen event-replay --from-audit-id <last-valid-id> --governor <name>

# 6. Verify state consistency
ggen governor-verify --governor <name> --audit-trail /tmp/audit-trail.json

# 7. Monitor for errors
watch -n 5 'ggen audit-export --governor <name> | tail -20'
```

**Prevention**:
- Run `cargo make test` before deployment (state machine tests)
- Enable state machine validation in production (slight performance cost)
- Regular state consistency checks: `ggen audit-verify --frequency hourly`

---

### Incident #2: Billing Reconciliation Failure

**Symptoms**:
- Payment marked as received but not reconciled
- Invoice-to-payment mismatch
- Customer refund requests

**Root Cause**:
- Payment webhook delayed or lost
- Idempotency key not recorded
- Concurrent payment from different methods

**Recovery Steps**:
```bash
# 1. List unreconciled invoices
gcloud sql query \
  --database=marketplace \
  "SELECT invoice_id, amount, customer_id, state FROM billing WHERE state = 'payment_received' AND created < NOW() - INTERVAL '1 day'"

# 2. Check payment processor for proof
ggen billing-reconcile --invoice-id <id> --check-processor stripe

# 3. If payment confirmed by processor but not recorded
ggen billing-reconcile --invoice-id <id> --force-reconcile

# 4. If payment lost, issue refund
ggen billing-issue-refund --invoice-id <id> --reason "duplicate_charge"

# 5. Verify reconciliation
ggen billing-verify --invoice-id <id> --audit-trail

# 6. Notify accounting team
ggen audit-export --control PCI-DSS \
  --start-date $(date -d '1 day ago' +%Y-%m-%d) \
  --end-date $(date +%Y-%m-%d) > /tmp/billing-audit.json
```

**Prevention**:
- Idempotency keys recorded before payment attempt
- Webhook retry logic with exponential backoff
- Daily reconciliation job: `ggen billing-reconcile --frequency daily --auto-recovery true`

---

### Incident #3: Compliance Violation Detected

**Symptoms**:
- `compliance_framework_status` shows non-compliant
- Data retention deadline approaching
- Fraud detection threshold exceeded

**Root Cause**:
- Data retention policy not executed
- KYC/AML verification expired
- New regulatory requirement not implemented

**Recovery Steps**:
```bash
# 1. Identify which framework is non-compliant
ggen audit-export --control governance | jq '.frameworks[] | select(.compliant == false)'

# 2. Check specific requirements
ggen audit-verify --control GDPR --scope data-retention

# 3. For data retention violations
ggen data-retention-execute --framework GDPR --dry-run true

# 4. If safe, execute retention policy
ggen data-retention-execute --framework GDPR --execute true --log-audit-trail

# 5. For verification failures
ggen verification-batch --type kyc-aml --filter expired --retry true

# 6. Update compliance status
ggen compliance-audit --full-scan true --framework <name>

# 7. Export compliance report
ggen audit-export --control <framework> --format pdf > /tmp/compliance-report.pdf
```

**Prevention**:
- Compliance calendar: `ggen compliance-calendar --export csv`
- Automated verification expirations checks
- Quarterly compliance audits: `ggen audit-verify --frequency quarterly`

---

### Incident #4: Cascading Failure in Multi-Tenant System

**Symptoms**:
- Multiple tenants experiencing service degradation
- Noisy neighbor consumes >50% resources
- Error rate climbing exponentially

**Root Cause**:
- Single noisy tenant's workload causing CPU/memory spike
- Insufficient bulkheads/isolation between tenants
- Feedback loop: slow response → retry surge → more slowness

**Recovery Steps**:
```bash
# 1. Activate cascade prevention immediately
ggen multi-tenant-governor --activate-cascade-prevention

# 2. Identify noisy tenant
ggen audit-export --governor multi-tenant --metrics resource-usage \
  | sort -k3 -rn | head -1

# 3. Apply circuit breaker to noisy tenant
ggen multi-tenant-governor --circuit-breaker enable \
  --tenant-id <id> \
  --error-threshold 50 \
  --timeout-ms 1000

# 4. Reduce noisy tenant to baseline quota
ggen quota-sla-governor --set-quota \
  --tenant-id <id> \
  --quota-type cpu \
  --value 25 \
  --unit percent

# 5. Monitor recovery
watch -n 2 'ggen audit-export --governor multi-tenant | grep -A 5 "tenant_<id>"'

# 6. Once stabilized, gradually restore quota
ggen quota-sla-governor --set-quota \
  --tenant-id <id> \
  --quota-type cpu \
  --value 50 \
  --unit percent

# 7. Disable circuit breaker when stable
ggen multi-tenant-governor --circuit-breaker disable --tenant-id <id>
```

**Prevention**:
- Resource quotas per tenant tier: 40% enterprise, 35% professional, 25% starter
- Circuit breakers enabled by default for noisy neighbor detection
- Regular load testing to identify contention patterns

---

## Governor Health Checks

### Health Check #1: Entitlement Governor

```bash
# Command
ggen governor-verify --governor entitlement --check all

# Expected output
✓ State machine valid (9 states, 18 transitions)
✓ No stuck states (max 72h approval timeout)
✓ Audit trail consistent (12,847 entries)
✓ Invariants satisfied (active entitlements > 0, no impossible states)
✓ Performance: P99 < 5s (last 100 transitions avg 423ms)

# If fails, investigate
ggen governor-diagnose --governor entitlement --detail verbose
```

### Health Check #2: Billing Governor

```bash
# Command
ggen governor-verify --governor billing --check all

# Expected output
✓ State machine valid (10 states, 20 transitions)
✓ No stuck invoices (invoices in payment_pending > 7 days escalated)
✓ Idempotency keys recorded (100% payment attempt coverage)
✓ Reconciliation complete (payment_received → archived latency < 24h)
✓ Performance: Payment processing < 5s (P99)
✓ Retry logic functional (test payment failure recovery)

# If fails
ggen billing-verify --detail verbose --include payment-processor-logs
```

### Health Check #3: Product Catalog Governor

```bash
# Command
ggen governor-verify --governor product-catalog --check all

# Expected output
✓ SKU schema valid (218 SKUs conform to schema)
✓ No orphaned references (all feature IDs resolve)
✓ Pricing consistency (no contradictory price points)
✓ Update propagation working (caches invalidated properly)
✓ Performance: < 10s P99 for catalog updates

# If fails
ggen product-catalog-verify --detail verbose --validate-schema true
```

### Health Check #4: Multi-Tenant Governor

```bash
# Command
ggen governor-verify --governor multi-tenant --check all

# Expected output
✓ Tenant isolation enforced (0 cross-tenant data access)
✓ Fair-share allocation accurate (allocation matches tier weights)
✓ Noisy neighbor detection operational (detect >75% CPU usage)
✓ Cascade prevention functional (test circuit breaker)
✓ No resource leaks (tenant cleanup working)

# If fails
ggen multi-tenant-verify --detail verbose --include resource-audit
```

---

## Operational Procedures

### Procedure #1: Scaling the Orchestrator

**When to scale**:
- Event queue depth > 80% capacity
- Decision latency P99 > 5 sec
- CPU utilization > 75% sustained

**How to scale**:
```bash
# Check current replicas
gcloud run services describe marketplace-orchestrator \
  --format='value(spec.template.spec.containers[0].resources.limits)'

# Increase replicas (from 5 to 10)
gcloud run services update marketplace-orchestrator \
  --min-instances 10 \
  --max-instances 20

# Monitor scaling
watch -n 5 'gcloud run metrics list \
  --service marketplace-orchestrator'

# Once stable, update deployment
kubectl patch deployment marketplace-orchestrator \
  -p '{"spec":{"replicas":10}}'
```

### Procedure #2: Database Failover

**When to failover**:
- Primary database connection errors
- Replication lag > 5 sec
- Primary shows signs of data corruption

**How to failover**:
```bash
# Check replica status
gcloud sql instances describe marketplace-replica --format='value(state)'

# Initiate failover
gcloud sql instances failover marketplace-db \
  --backup-location us-central1

# Verify failover succeeded
gcloud sql instances describe marketplace-db --format='value(currentDiskSize,settings.replicationConfiguration.kind)'

# Update connection string (usually automatic via Cloud SQL Proxy)
ggen config update --database-connection-string $NEW_CONNECTION_STRING

# Verify all governors can connect
ggen governor-health-check --all --database-check true
```

### Procedure #3: Emergency Shutdown

**When to execute**:
- Cascading failure detected AND cascade prevention failed
- Data corruption affecting critical systems
- Security breach requiring immediate containment

**How to execute**:
```bash
# 1. Graceful shutdown (30s drain period)
gcloud run services update marketplace-orchestrator \
  --timeout 30s --no-allow-unauthenticated

# 2. Wait for in-flight events to complete
sleep 30

# 3. Stop all governors
for gov in entitlement billing product-catalog subscription customer-account quota-sla compliance multi-tenant; do
  ggen governor-shutdown --governor $gov --graceful true
done

# 4. Drain event queue to cold storage
ggen event-export --queue marketplace-events \
  --output gs://backup-bucket/events-$(date +%Y%m%d-%H%M%S).ndjson

# 5. Archive database state
gcloud sql backups create \
  --instance=marketplace-db \
  --description="Emergency shutdown backup"

# 6. Announce maintenance
ggen customer-notify --broadcast true \
  --message "Scheduled maintenance, service resuming in 1 hour"

# 7. Once issue resolved, restart
gcloud run services update marketplace-orchestrator --allow-unauthenticated
```

---

## Metrics Reference

### Core Metrics

| Metric | Type | Unit | Source | SLO |
|--------|------|------|--------|-----|
| `orchestrator_state` | Gauge | enum | orchestrator logs | healthy |
| `orchestrator_event_queue_depth` | Gauge | count | Pub/Sub | <80% |
| `orchestrator_decision_latency` | Histogram | ms | orchestrator logs | P99 <5s |
| `orchestrator_error_rate` | Gauge | % | app logs | <0.1% |
| `governor_transition_latency` | Histogram | ms | governor logs | P99 <2s |
| `governor_error_count` | Counter | count | governor logs | minimize |
| `billing_payment_success_rate` | Gauge | % | billing logs | >99% |
| `quota_utilization` | Gauge | % | quota governor | <90% |
| `compliance_violations` | Counter | count | compliance logs | zero |
| `tenant_isolation_violations` | Counter | count | audit logs | zero |

### Query Examples

```sql
-- Check decision latency SLO
SELECT
  PERCENTILE_CONT(latency_ms, 0.50) as p50,
  PERCENTILE_CONT(latency_ms, 0.95) as p95,
  PERCENTILE_CONT(latency_ms, 0.99) as p99
FROM orchestrator_metrics
WHERE timestamp >= CURRENT_TIMESTAMP - INTERVAL 1 hour
GROUP BY TIMESTAMP_TRUNC(timestamp, MINUTE);

-- Payment success rate by processor
SELECT
  payment_processor,
  COUNT(CASE WHEN success THEN 1 END) / COUNT(*) as success_rate,
  COUNT(*) as total_attempts
FROM billing_events
WHERE timestamp >= CURRENT_TIMESTAMP - INTERVAL 24 hour
GROUP BY payment_processor;

-- Noisy neighbor detection
SELECT
  tenant_id,
  MAX(cpu_usage) as peak_cpu,
  MAX(memory_usage) as peak_memory,
  SUM(CASE WHEN cascade_prevention_activated THEN 1 ELSE 0 END) as cascade_count
FROM multi_tenant_metrics
WHERE timestamp >= CURRENT_TIMESTAMP - INTERVAL 24 hour
GROUP BY tenant_id
ORDER BY peak_cpu DESC;

-- Compliance violations by framework
SELECT
  framework,
  violation_type,
  COUNT(*) as violation_count,
  MAX(severity) as max_severity
FROM compliance_violations
WHERE timestamp >= CURRENT_TIMESTAMP - INTERVAL 30 day
GROUP BY framework, violation_type
ORDER BY violation_count DESC;
```

---

**Last Updated**: January 2026
**Next Review Date**: April 2026
**Owner**: @marketplace-operations
**Slack Channel**: #marketplace-oncall
