<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-004: Canary Deployment Strategy with KEDA](#adr-004-canary-deployment-strategy-with-keda)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Canary Benefits](#canary-benefits)
  - [Architecture](#architecture)
    - [Traffic Splitting with Istio](#traffic-splitting-with-istio)
    - [KEDA Event-Driven Scaling](#keda-event-driven-scaling)
  - [Promotion Criteria](#promotion-criteria)
    - [Automated Promotion Logic](#automated-promotion-logic)
    - [Promotion Stages](#promotion-stages)
  - [Implementation](#implementation)
    - [Flagger for Automated Canary](#flagger-for-automated-canary)
    - [Rollback Trigger](#rollback-trigger)
  - [Monitoring Strategy](#monitoring-strategy)
    - [Metrics to Compare (Stable vs Canary)](#metrics-to-compare-stable-vs-canary)
    - [Dashboard Setup](#dashboard-setup)
  - [Rollback Procedure](#rollback-procedure)
  - [Testing Before Canary](#testing-before-canary)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Cost Impact](#cost-impact)
  - [SLOs for Canary Deployments](#slos-for-canary-deployments)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-004: Canary Deployment Strategy with KEDA

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Rolling out new service versions safely with automated rollback
**Deciders:** Release Engineering Team

## Problem Statement

New deployments carry risk of:
- Breaking changes affecting all users
- Performance degradation
- Data corruption or loss
- Cascading failures in dependent services

Need: Gradual rollout, metrics-based validation, automatic rollback.

## Decision

**Progressive delivery using:**
- Canary deployments (10% → 50% → 100% traffic)
- Kubernetes Deployment rolling updates
- KEDA for event-driven scaling during canary
- Prometheus metrics for automatic promotion/rollback
- Istio VirtualService for traffic splitting

## Rationale

### Canary Benefits
1. **Risk Mitigation:**
   - Detect issues before full rollout
   - Limit blast radius (only 10% affected initially)
   - Automatic rollback on error rate spike
   - Safe rollout windows (off-peak hours)

2. **Observability:**
   - Compare metrics between stable and canary
   - Error rate, latency, resource usage deltas
   - Real user testing at scale
   - Business metrics validation

3. **Automation:**
   - No manual intervention needed
   - Policy-driven promotion criteria
   - Event-driven scaling with KEDA
   - GitOps-based configuration

## Architecture

### Traffic Splitting with Istio

```yaml
# Step 1: Deploy new version alongside existing
apiVersion: apps/v1
kind: Deployment
metadata:
  name: governor-v2
spec:
  replicas: 1  # Start with 1 replica
  selector:
    matchLabels:
      app: governor
      version: v2
  template:
    spec:
      containers:
      - name: governor
        image: gcr.io/ggen-project/governor:v2.0.0

---
# Step 2: Route 10% traffic to canary using Istio
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: governor
spec:
  hosts:
  - governor
  http:
  - route:
    - destination:
        host: governor
        subset: stable
      weight: 90
    - destination:
        host: governor
        subset: canary
      weight: 10
---
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: governor
spec:
  host: governor.svc.cluster.local
  subsets:
  - name: stable
    labels:
      version: v1
  - name: canary
    labels:
      version: v2
```

### KEDA Event-Driven Scaling

```yaml
# Scale based on Kafka lag during deployment
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: governor-canary-scaler
spec:
  scaleTargetRef:
    name: governor-v2
  minReplicaCount: 1
  maxReplicaCount: 10
  triggers:
  - type: kafka
    metadata:
      brokers: "kafka:9092"
      consumerGroup: governor-canary
      topic: governor-requests
      lagThreshold: "100"
```

## Promotion Criteria

### Automated Promotion Logic

```yaml
# Prometheus rules for canary promotion
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: canary-promotion
spec:
  groups:
  - name: canary
    rules:
    # Promote if canary error rate < stable error rate for 5 minutes
    - alert: CanaryPromotionCondition1
      expr: |
        (
          rate(istio_requests_total{response_code=~"5.."}[5m])
          offset 5m
        ) <
        (
          rate(istio_requests_total{response_code=~"5.."}[5m])
        )
      for: 5m
      labels:
        action: promote

    # Rollback if error rate > 10% for 2 minutes
    - alert: CanaryErrorRateSpike
      expr: |
        rate(istio_requests_total{response_code=~"5.."}[2m]) > 0.1
      for: 2m
      labels:
        action: rollback
```

### Promotion Stages

| Stage | Traffic % | Duration | Success Criteria |
|-------|-----------|----------|-----------------|
| Canary | 10% | 10 minutes | Error rate < 5%, latency p99 < 500ms |
| Early Adopters | 25% | 15 minutes | Same + no OOM/CPU throttling |
| Gradual | 50% | 20 minutes | Same + business metrics OK |
| Full | 100% | N/A | All systems nominal |

## Implementation

### Flagger for Automated Canary

Using [Flagger](https://flagger.app/) for automatic promotion:

```yaml
apiVersion: flagger.app/v1beta1
kind: Canary
metadata:
  name: governor
  namespace: tai-system
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: governor
  service:
    port: 50051
    targetPort: 50051
  analysis:
    interval: 1m
    threshold: 5
    maxWeight: 50
    stepWeight: 10
  metrics:
  - name: error-rate
    thresholdRange:
      max: 5
    interval: 1m
  - name: latency
    thresholdRange:
      max: 500
    interval: 1m
  - name: request-count
    thresholdRange:
      min: 100
    interval: 1m
  webhooks:
  - name: load-test
    url: http://flagger-loadtester/
    metadata:
      type: bash
      cmd: "curl http://governor:50051"
```

### Rollback Trigger

```yaml
# Automatic rollback on critical metric spike
apiVersion: flagger.app/v1beta1
kind: Alert
metadata:
  name: governor-canary-alert
spec:
  providerRef:
    name: slack
  eventSeverity: error
  eventAction: rollback
  description: "Rollback canary due to high error rate"
  match:
  - alertname: CanaryErrorRateSpike
```

## Monitoring Strategy

### Metrics to Compare (Stable vs Canary)

```prometheus
# Error rate
histogram_quantile(0.99, rate(istio_request_duration_ms_bucket[5m]))

# Request rate
rate(istio_requests_total[5m])

# Latency (p50, p95, p99)
histogram_quantile(0.99, rate(request_duration_bucket[5m]))

# Resource usage
container_memory_usage_bytes
rate(container_cpu_usage_seconds_total[5m])

# Business metrics
payment_success_rate
average_transaction_value
user_engagement_score
```

### Dashboard Setup

Create Grafana dashboard showing:
1. Side-by-side metrics (stable vs canary)
2. Traffic weight allocation over time
3. Promotion status and criteria
4. Alert threshold violations
5. Rollback history and reasons

## Rollback Procedure

Automatic rollback when:
- Error rate > 5% for 2 minutes
- Latency p99 > 500ms for 3 minutes
- OOM or crash loop detected
- Manual override requested

Manual rollback:
```bash
# Scale down canary
kubectl scale deployment governor-v2 --replicas=0 -n tai-system

# Route 100% traffic back to stable
kubectl patch virtualservice governor -p \
  '{"spec":{"http":[{"route":[{"destination":{"host":"governor","subset":"stable"},"weight":100}]}]}}'
```

## Testing Before Canary

1. **Unit Tests:** 100% pass rate
2. **Integration Tests:** Full test suite
3. **Load Tests:** 2x peak traffic for 5 minutes
4. **Chaos Tests:** Kill pods, inject latency
5. **Security Tests:** OWASP Top 10, SAST scan

## Consequences

### Positive
- Safe deployments with automatic validation
- Limits blast radius of defects
- Data-driven promotion decisions
- Reduced rollback/recovery time
- Improved reliability and confidence

### Negative
- Deployment takes longer (staged rollout)
- Complex monitoring setup required
- Requires reliable metrics and alerting
- False positives trigger unnecessary rollbacks
- Operational complexity

## Cost Impact

- Temporary 2x resource usage during canary (extra pods)
- Istio proxy overhead (~50MB per pod)
- Prometheus/Flagger infrastructure (~5 pod resources)
- Estimated: +20% compute cost during rollout windows

## SLOs for Canary Deployments

- **Promotion Time:** <60 minutes from canary to full traffic
- **Rollback Time:** <5 minutes after alert trigger
- **False Positive Rate:** <5% (unnecessary rollbacks)
- **Promotion Success Rate:** >95% (minimal human intervention)

## References
- [Flagger Documentation](https://flagger.app/)
- [Istio Traffic Management](https://istio.io/latest/docs/concepts/traffic-management/)
- [Progressive Delivery](https://progressivedelivery.com/)
- Helm chart: helm/tai-chart/flagger-values.yaml
