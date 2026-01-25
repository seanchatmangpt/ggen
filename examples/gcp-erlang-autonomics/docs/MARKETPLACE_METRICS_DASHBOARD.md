# Marketplace Metrics Dashboard & Observability

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Observability Guide
**Audience**: DevOps, SREs, Product, Engineering

---

## Table of Contents

1. [Key Metrics by Governor](#key-metrics-by-governor)
2. [SLO Targets](#slo-targets)
3. [Alert Conditions](#alert-conditions)
4. [Grafana Dashboard JSON](#grafana-dashboard-json)
5. [Cloud Monitoring Queries](#cloud-monitoring-queries)
6. [Log Query Examples](#log-query-examples)
7. [Metrics Correlation](#metrics-correlation)

---

## Key Metrics by Governor

### Entitlement Governor Metrics

```
Metric: entitlement_state_transitions_total
  Type: Counter
  Label: from_state, to_state, success
  SLO: Zero failures
  Query: SELECT COUNT(*) FROM entitlement_transitions WHERE success=true
  Alert: > 10 failures per hour

Metric: entitlement_pending_approval_duration
  Type: Histogram
  Buckets: [1h, 6h, 12h, 24h, 48h, 72h]
  SLO: P99 < 24h
  Query: SELECT PERCENTILE_CONT(duration_seconds / 3600, 0.99) FROM entitlement_timings
  Alert: P99 > 48h (escalation deadline missed)

Metric: entitlement_timeout_escalations_total
  Type: Counter
  Label: timeout_type (24h_approval, 72h_suspension, etc)
  SLO: Minimize (should be rare)
  Query: SELECT COUNT(*) FROM entitlement_events WHERE event='EscalateToManager'
  Alert: > 5 per day (process broken)

Metric: entitlement_feature_access_violations
  Type: Counter
  Label: feature_id, violation_type (unauthorized_access, incorrect_state)
  SLO: Zero violations
  Query: SELECT COUNT(*) FROM entitlement_violations WHERE resolved=false
  Alert: Any violation (critical)

Metric: entitlement_active_count
  Type: Gauge
  Label: customer_tier (Enterprise, Professional, Starter)
  SLO: Trends match sales forecast
  Query: SELECT COUNT(DISTINCT customer_id) FROM entitlements WHERE state='active'
  Chart: Stacked bar by tier
```

### Billing Governor Metrics

```
Metric: billing_payment_success_rate
  Type: Gauge (%)
  Label: payment_processor (stripe, backup_processor)
  SLO: > 99%
  Query: SELECT COUNT(successful) / COUNT(*) FROM payments WHERE created >= NOW() - INTERVAL 1 hour
  Alert: < 98% for 10 min (page on-call)
         < 95% for 1 min (critical)

Metric: billing_invoice_count_monthly
  Type: Counter (monthly)
  Label: customer_tier, currency
  SLO: Matches active subscription count
  Query: SELECT COUNT(*) FROM invoices WHERE created >= NOW() - INTERVAL 1 month
  Chart: Stacked bar by tier

Metric: billing_revenue_processed
  Type: Counter (USD)
  Label: currency, payment_processor
  SLO: Matches financial records
  Query: SELECT SUM(amount) FROM payments WHERE created >= NOW() - INTERVAL 1 day
  Alert: Sudden drop > 20% (investigate)

Metric: billing_retry_depth_distribution
  Type: Counter
  Label: retry_level (1, 2, 3, collection_agency)
  SLO: Minimize retry_3 and collection_agency
  Query: SELECT retry_level, COUNT(*) FROM payment_retries GROUP BY retry_level
  Alert: collection_agency > 50 per day (fraud investigation)

Metric: billing_failed_payment_recovery_rate
  Type: Gauge (%)
  SLO: > 70% (recovered via retries)
  Query: SELECT COUNT(recovered=true) / COUNT(*) FROM payment_failures WHERE retry_applicable=true
  Chart: Time series (weekly trend)

Metric: billing_payment_processing_latency
  Type: Histogram (seconds)
  Buckets: [100ms, 500ms, 1s, 2s, 5s, 10s]
  SLO: P99 < 5s
  Query: SELECT PERCENTILE_CONT(processing_time_seconds, 0.99) FROM billing_events
  Alert: P99 > 10s (Stripe API slow)

Metric: billing_idempotency_cache_hits
  Type: Counter
  Label: hit (true/false), transaction_type
  SLO: > 5% hit rate (duplicate detection working)
  Query: SELECT hit, COUNT(*) FROM idempotency_cache GROUP BY hit
  Alert: Hit rate dropping (cache misconfigured)
```

### Subscription Governor Metrics

```
Metric: subscription_state_distribution
  Type: Gauge (count)
  Label: state (trial, active, renewal_pending, paused, cancelled, expired)
  SLO: Trends match churn model
  Query: SELECT state, COUNT(*) FROM subscriptions GROUP BY state
  Chart: Stacked bar

Metric: subscription_lifecycle_transitions
  Type: Counter
  Label: from_state, to_state, churn_signal (true/false)
  SLO: Churn rate < 5% monthly
  Query: SELECT COUNT(*) FROM subscription_transitions WHERE from_state='active' AND to_state='cancelled'
  Alert: Churn rate > 5% for 1 week (investigation)

Metric: subscription_trial_conversion_rate
  Type: Gauge (%)
  SLO: > 20% (customer-specific)
  Query: SELECT COUNT(converted) / COUNT(*) FROM trials WHERE created >= NOW() - INTERVAL 30 days
  Chart: Weekly trend with confidence interval

Metric: subscription_renewal_success_rate
  Type: Gauge (%)
  SLO: > 98%
  Query: SELECT COUNT(success=true) / COUNT(*) FROM renewals WHERE created >= NOW() - INTERVAL 30 days
  Alert: < 97% for 1 hour (page billing team)

Metric: subscription_proration_accuracy
  Type: Gauge (%)
  SLO: 100% (cryptographic verification)
  Query: SELECT COUNT(accurate=true) / COUNT(*) FROM prorations WHERE verified=true
  Alert: < 100% (data integrity issue)

Metric: subscription_pause_utilization
  Type: Gauge (%)
  SLO: < 2% of subscriptions paused
  Query: SELECT COUNT(*) FROM subscriptions WHERE state='paused'
  Chart: Count over time
```

### Quota & SLA Governor Metrics

```
Metric: quota_utilization_by_customer
  Type: Gauge (%)
  Label: customer_id, customer_tier, resource_type (api_calls, cpu, memory)
  SLO: Average < 60%, Max < 90%
  Query: SELECT customer_id, resource_type, utilization FROM quota_metrics ORDER BY utilization DESC
  Alert: Any customer > 90% (warn customer, page ops if > 100%)

Metric: quota_throttling_events
  Type: Counter
  Label: customer_id, resource_type, duration_seconds
  SLO: < 100 events per hour (system healthy)
  Query: SELECT COUNT(*) FROM throttle_events WHERE created >= NOW() - INTERVAL 1 hour
  Alert: > 200 per hour (investigate bottleneck)

Metric: quota_fair_share_accuracy
  Type: Gauge (%)
  SLO: 100% (allocation matches tier weights)
  Query: SELECT accuracy FROM fair_share_verification WHERE verified=true
  Chart: Hourly verification results

Metric: sla_uptime_achievement
  Type: Gauge (%)
  Label: customer_tier, service_component
  SLO: Enterprise 99.95%, Professional 99.9%, Starter 99.0%
  Query: SELECT customer_tier, uptime_percent FROM sla_metrics
  Alert: Below SLO for > 1 hour (notify customer, page ops)

Metric: sla_latency_achievement
  Type: Gauge (%)
  Label: customer_tier, percentile (p50, p95, p99)
  SLO: Enterprise p99 <200ms, Prof p99 <500ms, Starter p99 <2s
  Query: SELECT customer_tier, PERCENTILE_CONT(latency_ms, 0.99) FROM request_latencies
  Alert: Below SLO (investigate performance)

Metric: sla_violation_count
  Type: Counter
  Label: customer_id, sla_type (uptime, latency, availability)
  SLO: Zero violations
  Query: SELECT COUNT(*) FROM sla_violations
  Alert: Any violation (notify customer immediately)
```

### Compliance Governor Metrics

```
Metric: compliance_framework_status
  Type: Gauge (enum)
  Label: framework (GDPR, HIPAA, SOC2, PCI-DSS, FedRAMP)
  Values: compliant=1, non_compliant=0
  SLO: All frameworks compliant
  Query: SELECT framework, status FROM compliance_status
  Alert: Non-compliant framework (critical)

Metric: kyc_aml_verification_rate
  Type: Gauge (%)
  SLO: > 99%
  Query: SELECT COUNT(verified=true) / COUNT(*) FROM kyc_verifications
  Alert: < 99% (verification service issue)

Metric: kyc_aml_verification_time
  Type: Histogram (seconds)
  Buckets: [5s, 10s, 20s, 30s, 60s]
  SLO: P99 < 30s
  Query: SELECT PERCENTILE_CONT(time_seconds, 0.99) FROM kyc_timings
  Alert: P99 > 60s (third-party service slow)

Metric: fraud_detection_alerts
  Type: Counter
  Label: risk_category (payment_anomaly, account_takeover, velocity_check)
  SLO: Minimize false positives
  Query: SELECT risk_category, COUNT(*) FROM fraud_alerts WHERE created >= NOW() - INTERVAL 24 hours
  Alert: > 50 alerts per day (review detection model)

Metric: data_retention_expiration_days
  Type: Gauge (days until expiration)
  SLO: > 30 days (never let retention expire by accident)
  Query: SELECT MIN(expiration_date - NOW()) / 86400 FROM retention_schedules
  Alert: < 30 days (initiate retention review)

Metric: compliance_violations_open
  Type: Gauge (count)
  Label: framework, severity (critical, high, medium)
  SLO: Zero critical, < 5 high
  Query: SELECT severity, COUNT(*) FROM compliance_violations WHERE resolved=false
  Alert: Any critical violation (immediate action)

Metric: audit_trail_integrity_verified
  Type: Gauge (%)
  SLO: 100% (hash chain unbroken)
  Query: SELECT COUNT(valid=true) / COUNT(*) FROM audit_trail_verification
  Alert: < 100% (data tampering detected)
```

### Multi-Tenant Governor Metrics

```
Metric: tenant_isolation_verified
  Type: Gauge (%)
  SLO: 100% (no cross-tenant data leakage)
  Query: SELECT COUNT(isolated=true) / COUNT(*) FROM isolation_tests
  Alert: < 100% (security violation)

Metric: tenant_resource_contention_events
  Type: Counter
  Label: cause (noisy_neighbor, cascade_prevention), severity
  SLO: < 1 per day (healthy system)
  Query: SELECT cause, COUNT(*) FROM contention_events WHERE created >= NOW() - INTERVAL 24 hours
  Alert: > 3 per day (investigate capacity)

Metric: noisy_neighbor_detection_accuracy
  Type: Gauge (%)
  Label: metric_type (cpu_usage, memory_usage, network_bandwidth)
  SLO: True Positive Rate > 95%, False Positive Rate < 1%
  Query: SELECT metric_type, tp_rate, fp_rate FROM detection_accuracy
  Alert: Outside SLO (retrain model)

Metric: fair_share_allocation_accuracy
  Type: Gauge (%)
  SLO: 100% (allocation matches tier weights 40/35/25)
  Query: SELECT accuracy FROM fair_share_allocation_verify
  Alert: < 100% (billing fairness issue)

Metric: cascade_prevention_activations
  Type: Counter
  Label: severity (degradation, emergency_shutdown)
  SLO: < 1 per day
  Query: SELECT severity, COUNT(*) FROM cascade_events WHERE created >= NOW() - INTERVAL 24 hours
  Alert: > 2 per day (investigate system limits)

Metric: tenant_cpu_usage_percentile
  Type: Gauge (%)
  Label: percentile (p50, p75, p90, p99), resource (cpu, memory, network)
  SLO: P90 < 70%, P99 < 90%
  Query: SELECT resource, PERCENTILE_CONT(usage_percent, 0.90) FROM tenant_metrics
  Chart: Multiple time series

Metric: load_balancing_effectiveness
  Type: Gauge (%)
  SLO: Reduce peak tenant usage by > 30%
  Query: SELECT effectiveness FROM load_balancing_metrics
  Chart: Hourly effectiveness trend
```

### Orchestrator Metrics

```
Metric: orchestrator_state
  Type: Gauge (enum)
  Values: initializing=0, idle=1, processing=2, error=3
  SLO: idle (healthy)
  Query: SELECT state, COUNT(*) FROM orchestrator_state_history
  Alert: error state for > 1 min (critical)

Metric: orchestrator_event_queue_depth
  Type: Gauge (count)
  SLO: < 80% capacity (1000 max)
  Query: SELECT depth FROM orchestrator_queue_metrics
  Alert: > 800 events (scale orchestrator)

Metric: orchestrator_decision_latency
  Type: Histogram (milliseconds)
  Buckets: [100ms, 250ms, 500ms, 1s, 2s, 5s]
  SLO: P99 < 500ms
  Query: SELECT PERCENTILE_CONT(latency_ms, 0.99) FROM orchestrator_decisions
  Alert: P99 > 1s (investigate bottleneck)

Metric: orchestrator_event_processing_rate
  Type: Gauge (events/sec)
  SLO: > 100 events/sec (capacity)
  Query: SELECT rate FROM event_processing_metrics
  Chart: Rolling average (5-min window)

Metric: orchestrator_error_rate
  Type: Gauge (%)
  SLO: < 0.1%
  Query: SELECT COUNT(error=true) / COUNT(*) FROM orchestrator_events
  Alert: > 1% (critical)

Metric: governor_response_time_by_type
  Type: Histogram (ms)
  Label: governor_type (entitlement, billing, etc)
  SLO: All < 2s (P99)
  Query: SELECT governor_type, PERCENTILE_CONT(response_time_ms, 0.99) FROM governor_metrics
  Chart: Box plot by governor type
```

---

## SLO Targets

### System-Level SLOs

```yaml
SLO Targets (Annual):
  Availability:
    Target: 99.95% uptime
    Meaning: 52 minutes downtime allowed per year
    Measurement: Successful orchestrator heartbeat
    Alert: 0 heartbeats for 60 seconds

  Decision Latency:
    Target: P99 < 500ms
    Meaning: 99% of decisions within 500ms
    Measurement: Time from event arrival to decision
    Alert: P99 > 1s for 5 min

  Error Rate:
    Target: < 0.1%
    Meaning: 999 of 1000 events processed successfully
    Measurement: Failed event processing / total events
    Alert: > 1% for 10 min

  Data Consistency:
    Target: 100%
    Meaning: Zero data corruption incidents
    Measurement: Audit trail hash chain verification
    Alert: Hash chain broken (critical)

  Feature Availability:
    Target: 99.9%
    Meaning: Features available 99.9% of billing month
    Measurement: Feature-specific uptime
    Alert: Below SLO (notify customer)

  Payment Processing:
    Target: 99%
    Meaning: 99% of payment attempts succeed
    Measurement: Successful payments / total attempts
    Alert: < 98% for 10 min (page payments team)

  Compliance:
    Target: 100%
    Meaning: Zero compliance violations
    Measurement: Violation count
    Alert: Any violation (critical)
```

### Per-Customer-Tier SLOs

```
Enterprise Tier:
  Uptime: 99.95%
  Latency P99: <200ms
  Decision throughput: > 1000 req/sec
  Data retention: 3 years
  Support: 24/7 phone support

Professional Tier:
  Uptime: 99.9%
  Latency P99: <500ms
  Decision throughput: > 500 req/sec
  Data retention: 2 years
  Support: Business hours email + chat

Starter Tier:
  Uptime: 99.0%
  Latency P99: <2000ms
  Decision throughput: > 100 req/sec
  Data retention: 1 year
  Support: Community + email (best-effort)
```

---

## Alert Conditions

### Critical Alerts (Page Immediately)

```yaml
Critical Alerts:
  - Name: Orchestrator Down
    Condition: No heartbeat for 60 sec
    Action: Page Tier 1 immediately

  - Name: Governor Crash Loop
    Condition: Governor error state for > 2 min
    Action: Page Tier 2 (Engineering Manager)

  - Name: Payment Processing Failure
    Condition: Payment success rate < 95% for 10 min
    Action: Page Payments Team + Business Ops

  - Name: Data Consistency Violation
    Condition: Idempotency check fails OR audit trail hash broken
    Action: Page Tier 3 + Database Team

  - Name: Compliance Violation Detected
    Condition: Any GDPR/HIPAA/SOC2 violation
    Action: Page Security Team + Compliance Officer

  - Name: Security Event
    Condition: Unauthorized access attempt OR injection attack detected
    Action: Page Security Team immediately

  - Name: SLA Violation
    Condition: Any customer SLO breach
    Action: Page relevant team + notify customer
```

### High Alerts (Page within 5 min if not resolved)

```yaml
High Alerts:
  - Name: Decision Latency SLO Miss
    Condition: P99 > 5 sec for 5 min
    Action: Page Tier 1

  - Name: Event Queue Backing Up
    Condition: Queue depth > 80% for 5 min
    Action: Page Tier 1 (may need scaling)

  - Name: Governor Response Timeout
    Condition: Governor coordination timeout > 3 sec for 10 min
    Action: Page Tier 1

  - Name: Payment Retry Escalation
    Condition: Retry-3 count > 50/hour
    Action: Page Payments Team

  - Name: Noisy Neighbor Detected
    Condition: Single tenant > 75% CPU for 5 min
    Action: Page Platform Ops (for rebalancing)

  - Name: Fraud Alert Volume
    Condition: > 50 fraud alerts/hour
    Action: Page Security Team
```

### Medium Alerts (Email, No Page)

```yaml
Medium Alerts:
  - Name: CPU/Memory High
    Condition: CPU > 75% for 15 min OR Memory > 80% for 15 min
    Action: Email DevOps, may scale

  - Name: Error Rate Spike
    Condition: Error rate > 0.5% for 10 min
    Action: Email dev team, investigate

  - Name: Cost Forecast Overage
    Condition: Monthly cost forecast > 110% of budget
    Action: Email Finance + Engineering Leads

  - Name: Dependency Deprecation Warning
    Condition: Crate version approaching deprecation
    Action: Email Tech Lead (upgrade planning)

  - Name: Certificate Expiration Soon
    Condition: < 30 days to TLS cert expiration
    Action: Email Ops team (renewal planning)
```

---

## Grafana Dashboard JSON

### Dashboard: Marketplace Governors Overview

```json
{
  "dashboard": {
    "title": "Marketplace Governors - System Overview",
    "panels": [
      {
        "title": "Orchestrator Status",
        "type": "stat",
        "targets": [
          {
            "expr": "orchestrator_state{state='idle'}"
          }
        ],
        "thresholds": {
          "values": [0, 1],
          "colors": ["red", "green"]
        }
      },
      {
        "title": "Decision Latency (P99)",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.99, rate(orchestrator_decision_latency_ms_bucket[5m]))"
          }
        ],
        "alert": {
          "name": "High Decision Latency",
          "condition": "query(A, '5m', 'last') > 500"
        }
      },
      {
        "title": "Error Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(orchestrator_errors_total[5m]) / rate(orchestrator_events_total[5m])"
          }
        ],
        "thresholds": [0.001, 0.01]
      },
      {
        "title": "Event Queue Depth",
        "type": "graph",
        "targets": [
          {
            "expr": "orchestrator_event_queue_depth / 1000 * 100"
          }
        ],
        "alert": {
          "name": "Queue Backing Up",
          "condition": "query(A, '5m', 'last') > 80"
        }
      },
      {
        "title": "Governor Response Times",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.99, rate(governor_response_time_ms_bucket[5m]))"
          }
        ],
        "groupBy": ["governor_type"]
      },
      {
        "title": "Payment Success Rate",
        "type": "gauge",
        "targets": [
          {
            "expr": "rate(billing_payments_successful[1h]) / rate(billing_payments_total[1h])"
          }
        ],
        "thresholds": [0.98, 0.99, 1.0],
        "colors": ["red", "yellow", "green"]
      },
      {
        "title": "Subscription State Distribution",
        "type": "piechart",
        "targets": [
          {
            "expr": "count(subscription_state)",
            "legend": "{{ state }}"
          }
        ]
      },
      {
        "title": "Noisy Neighbor Detection",
        "type": "heatmap",
        "targets": [
          {
            "expr": "tenant_cpu_usage_percentile"
          }
        ]
      },
      {
        "title": "Compliance Status",
        "type": "table",
        "targets": [
          {
            "expr": "compliance_framework_status"
          }
        ],
        "columns": ["framework", "status", "last_verified"]
      },
      {
        "title": "SLA Violations (Last 24h)",
        "type": "table",
        "targets": [
          {
            "expr": "increase(sla_violations[24h])"
          }
        ],
        "columns": ["customer_id", "sla_type", "violation_count"]
      }
    ]
  }
}
```

---

## Cloud Monitoring Queries

### Query: Payment Processing SLO Status

```sql
-- Check if payment processing meets SLO (99%)
SELECT
  CURRENT_TIMESTAMP() AS timestamp,
  COUNT(CASE WHEN success THEN 1 END) / COUNT(*) AS success_rate,
  COUNT(*) AS total_attempts,
  CASE
    WHEN COUNT(CASE WHEN success THEN 1 END) / COUNT(*) >= 0.99 THEN 'PASS'
    ELSE 'FAIL'
  END AS slo_status
FROM `project.dataset.billing_payments`
WHERE created_at >= CURRENT_TIMESTAMP() - INTERVAL 1 HOUR
  AND payment_processor IN ('stripe', 'backup_processor');
```

### Query: Entitlement Timeout Analysis

```sql
-- Identify entitlements stuck in pending_approval > 24h
SELECT
  entitlement_id,
  customer_id,
  created_at,
  DATETIME_DIFF(CURRENT_TIMESTAMP(), created_at, HOUR) AS hours_pending,
  CASE
    WHEN DATETIME_DIFF(CURRENT_TIMESTAMP(), created_at, HOUR) > 24 THEN 'ESCALATE'
    WHEN DATETIME_DIFF(CURRENT_TIMESTAMP(), created_at, HOUR) > 12 THEN 'WARN'
    ELSE 'OK'
  END AS escalation_status
FROM `project.dataset.entitlements`
WHERE state = 'pending_approval'
ORDER BY created_at;
```

### Query: Quota Utilization by Customer

```sql
-- Show quota utilization by customer tier
SELECT
  customer_id,
  customer_tier,
  resource_type,
  ROUND(current_usage / allocated_quota * 100, 2) AS utilization_percent,
  allocated_quota,
  current_usage,
  CASE
    WHEN current_usage / allocated_quota > 0.9 THEN 'CRITICAL'
    WHEN current_usage / allocated_quota > 0.8 THEN 'HIGH'
    WHEN current_usage / allocated_quota > 0.6 THEN 'MEDIUM'
    ELSE 'LOW'
  END AS status
FROM `project.dataset.quota_metrics`
WHERE timestamp >= CURRENT_TIMESTAMP() - INTERVAL 1 HOUR
ORDER BY utilization_percent DESC;
```

### Query: Multi-Tenant Fairness Audit

```sql
-- Verify fair-share allocation is accurate
SELECT
  tenant_tier,
  SUM(CASE WHEN cpu_usage > 0 THEN cpu_usage ELSE 0 END) /
    SUM(SUM(CASE WHEN cpu_usage > 0 THEN cpu_usage ELSE 0 END)) OVER () AS actual_allocation,
  CASE
    WHEN tenant_tier = 'Enterprise' THEN 0.40
    WHEN tenant_tier = 'Professional' THEN 0.35
    WHEN tenant_tier = 'Starter' THEN 0.25
  END AS expected_allocation,
  CASE
    WHEN ABS(
      SUM(CASE WHEN cpu_usage > 0 THEN cpu_usage ELSE 0 END) /
        SUM(SUM(CASE WHEN cpu_usage > 0 THEN cpu_usage ELSE 0 END)) OVER () -
      CASE
        WHEN tenant_tier = 'Enterprise' THEN 0.40
        WHEN tenant_tier = 'Professional' THEN 0.35
        WHEN tenant_tier = 'Starter' THEN 0.25
      END
    ) < 0.05 THEN 'FAIR'
    ELSE 'UNFAIR'
  END AS fairness_status
FROM `project.dataset.multi_tenant_metrics`
WHERE timestamp >= CURRENT_TIMESTAMP() - INTERVAL 1 HOUR
GROUP BY tenant_tier;
```

### Query: Compliance Violation Audit

```sql
-- List all unresolved compliance violations
SELECT
  violation_id,
  framework,
  violation_type,
  severity,
  detected_at,
  DATETIME_DIFF(CURRENT_TIMESTAMP(), detected_at, HOUR) AS hours_open,
  remediation_deadline,
  remediation_status,
  owner_email
FROM `project.dataset.compliance_violations`
WHERE resolved_at IS NULL
ORDER BY severity DESC, detected_at;
```

---

## Log Query Examples

### Example 1: Troubleshoot Payment Processing Failure

```bash
# Find what happened during payment failure window
gcloud logging read \
  'resource.type="cloud_run_revision"
   AND resource.labels.service_name="marketplace-orchestrator"
   AND jsonPayload.event_type="PaymentFailed"
   AND timestamp >= "2026-01-25T14:30:00Z"
   AND timestamp <= "2026-01-25T14:40:00Z"' \
  --format json | jq '.[] | {timestamp, governor: .jsonPayload.governor, error_code: .jsonPayload.error_code, customer_id: .jsonPayload.customer_id}'
```

### Example 2: Identify Noisy Neighbor

```bash
# Find tenant consuming most CPU in past hour
gcloud logging read \
  'resource.type="cloud_run_revision"
   AND jsonPayload.event_type="MetricsSnapshot"
   AND timestamp >= "2026-01-25T14:00:00Z"' \
  --format json | \
  jq '.[] | .jsonPayload.tenants[] | {tenant_id, cpu_usage, memory_usage}' | \
  jq -s 'sort_by(-.cpu_usage) | .[0:5]'
```

### Example 3: Entitlement Approval Delays

```bash
# Find entitlements stuck in pending_approval
gcloud logging read \
  'resource.type="cloud_run_revision"
   AND jsonPayload.state="pending_approval"
   AND jsonPayload.hours_elapsed >= 24' \
  --format json | jq '.[] | {entitlement_id: .jsonPayload.entitlement_id, hours_elapsed: .jsonPayload.hours_elapsed, customer_id: .jsonPayload.customer_id}' | jq -s 'length'
```

### Example 4: Fraud Detection Activity

```bash
# Find fraud alerts triggered today
gcloud logging read \
  'resource.type="cloud_run_revision"
   AND jsonPayload.event_type="FraudAlertTriggered"
   AND timestamp >= "2026-01-25T00:00:00Z"' \
  --format json | \
  jq '.[] | {timestamp, risk_score: .jsonPayload.risk_score, customer_id: .jsonPayload.customer_id, indicators: .jsonPayload.fraud_indicators}' | \
  jq -s 'sort_by(-.risk_score) | .[0:20]'
```

---

## Metrics Correlation

### Correlation: Payment Failure → Entitlement Suspension

```
Timeline:
  14:25:00 - billing_payment_failed event (Stripe declined)
  14:25:01 - entitlement_suspend event triggered (via orchestrator)
  14:25:02 - entitlement_state changes to suspended
  14:25:03 - quota_throttled event (customer throttled after suspension)
  14:25:04 - customer_notification event (email sent)

Query to verify correlation:
SELECT
  MAX(CASE WHEN jsonPayload.event_type='PaymentFailed' THEN timestamp END) AS payment_fail_time,
  MAX(CASE WHEN jsonPayload.event_type='EntitlementSuspend' THEN timestamp END) AS suspend_time,
  MAX(CASE WHEN jsonPayload.event_type='EntitlementSuspend' THEN timestamp END) -
    MAX(CASE WHEN jsonPayload.event_type='PaymentFailed' THEN timestamp END) AS latency_ms
FROM `project.dataset.orchestrator_logs`
WHERE customer_id = 'cust_abc'
GROUP BY customer_id;
```

### Correlation: Noisy Neighbor → Cascade Prevention

```
Timeline:
  09:15:00 - tenant_resource_contention_event (Tenant-005: 87% CPU)
  09:15:01 - load_balancing_started event
  09:15:30 - load_balancing_failed event (no improvement)
  09:15:31 - cascade_prevention_activated event
  09:15:32 - cascade_prevention metrics show improvement
  09:25:00 - recovery_started event
  09:30:00 - system_healthy event

Query to verify correlation:
SELECT
  DATETIME_DIFF(
    MAX(CASE WHEN cascade_prevention_activated THEN timestamp END),
    MAX(CASE WHEN resource_contention_detected THEN timestamp END),
    SECOND
  ) AS time_to_activate_cascade_prevention,
  DATETIME_DIFF(
    MAX(CASE WHEN system_healthy THEN timestamp END),
    MAX(CASE WHEN cascade_prevention_activated THEN timestamp END),
    SECOND
  ) AS recovery_duration_seconds
FROM `project.dataset.multi_tenant_logs`
WHERE tenant_id = 'tenant_005'
  AND timestamp >= '2026-01-25 09:00:00';
```

---

**Last Updated**: January 2026
**Next Review**: April 2026
**Owner**: @observability-team
**Grafana URL**: https://grafana.company.com/d/marketplace-overview
