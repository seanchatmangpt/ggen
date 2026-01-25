<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-012: Kubernetes Autoscaling with HPA and KEDA](#adr-012-kubernetes-autoscaling-with-hpa-and-keda)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [HPA (Resource-Based)](#hpa-resource-based)
    - [KEDA (Event-Driven)](#keda-event-driven)
    - [VPA (Vertical)](#vpa-vertical)
  - [Implementation](#implementation)
    - [HPA Configuration](#hpa-configuration)
    - [KEDA Configuration (Event-Driven)](#keda-configuration-event-driven)
    - [KEDA with Pub/Sub](#keda-with-pubsub)
    - [Vertical Pod Autoscaler (VPA)](#vertical-pod-autoscaler-vpa)
    - [Custom Metrics](#custom-metrics)
    - [Pod Disruption Budget (PDB)](#pod-disruption-budget-pdb)
  - [Scaling Policies by Service](#scaling-policies-by-service)
  - [Monitoring Autoscaling](#monitoring-autoscaling)
    - [Alerts](#alerts)
  - [Cost Optimization](#cost-optimization)
  - [Testing Autoscaling](#testing-autoscaling)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [SLOs for Scaling](#slos-for-scaling)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-012: Kubernetes Autoscaling with HPA and KEDA

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Automatically scaling services based on demand and events
**Deciders:** Platform Engineering Team

## Problem Statement

TAI services experience variable load:
- Peaks during business hours (10x baseline)
- Events cause sudden spikes (policy enforcement, signal bursts)
- Manual scaling causes delayed response
- Over-provisioning wastes costs

Need: Automatic, fast scaling based on metrics and events.

## Decision

**Dual-layer autoscaling:**
1. **HPA (Horizontal Pod Autoscaler):** CPU/memory-based scaling
2. **KEDA (Kubernetes Event-Driven Autoscaling):** Event-based scaling (Kafka lag, queue depth)
3. **VPA (Vertical Pod Autoscaler):** Right-sizing resources

## Rationale

### HPA (Resource-Based)
- Simple, widely supported
- CPU and memory metrics
- Predictable scaling behavior
- Built-in to Kubernetes

### KEDA (Event-Driven)
- Scales based on workload (Kafka lag, queue depth)
- Reacts to actual load, not just resource usage
- Prevents queue buildup
- Supports 50+ scalers

### VPA (Vertical)
- Recommends right-sized resources
- Reduces wasted capacity
- Works alongside HPA

## Implementation

### HPA Configuration

```yaml
# Horizontal Pod Autoscaler for Governor
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: governor-hpa
  namespace: tai-system
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: governor
  minReplicas: 3
  maxReplicas: 50

  # Scale up aggressively, down conservatively
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
      - type: Pods
        value: 2
        periodSeconds: 60
      selectPolicy: Min

    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 15
      - type: Pods
        value: 4
        periodSeconds: 15
      selectPolicy: Max

  metrics:
  # CPU-based scaling
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  # Memory-based scaling
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  # Custom metric: request latency
  - type: Pods
    pods:
      metric:
        name: http_request_duration_seconds
      target:
        type: AverageValue
        averageValue: "0.5"
```

### KEDA Configuration (Event-Driven)

```yaml
# KEDA ScaledObject: Scale based on Kafka lag
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: coordinator-keda
  namespace: tai-system
spec:
  scaleTargetRef:
    name: coordinator
  minReplicaCount: 2
  maxReplicaCount: 100

  # Pause scaling if pod needs to restart
  pausedReplicaCount: 1
  fallback:
    failureThreshold: 3
    replicas: 10

  triggers:
  # Kafka topic lag
  - type: kafka
    metadata:
      bootstrapServers: kafka:9092
      consumerGroup: coordinator
      topic: tai-signals
      lagThreshold: "100"        # Scale when lag > 100
      offsetResetPolicy: "latest"

  # Custom metric from Prometheus
  - type: prometheus
    metadata:
      serverAddress: http://prometheus:9090
      metricName: custom_queue_depth
      query: |
        sum(rate(tai_queue_depth_seconds_total[1m]))
      threshold: "1000"

  # External HTTP metric
  - type: external
    metadata:
      scalerAddress: "custom-scaler:6379"
      metricName: "custom_queue_length"
      metricStatPeriod: "30"
      metricStatConsumption: "raw"
    parameters:
      endpoint: "scale"
      key: "coordinator"
```

### KEDA with Pub/Sub

```yaml
# Scale based on Google Cloud Pub/Sub subscription backlog
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: scheduler-pubsub
  namespace: tai-system
spec:
  scaleTargetRef:
    name: scheduler
  minReplicaCount: 2
  maxReplicaCount: 50

  triggers:
  - type: gcp-stackdriver
    metadata:
      filter: 'resource.type="pubsub_subscription" AND resource.labels.subscription_id="tai-tasks"'
      metricName: "pubsub.googleapis.com|subscription|num_undelivered_messages"
      threshold: "1000"
      projectId: "ggen-project"
```

### Vertical Pod Autoscaler (VPA)

```yaml
# VPA for right-sizing
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: governor-vpa
  namespace: tai-system
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: governor
  updatePolicy:
    updateMode: "Auto"  # Auto apply recommendations
    minReplicas: 2      # Don't scale below this
  resourcePolicy:
    containerPolicies:
    - containerName: governor
      minAllowed:
        cpu: 100m
        memory: 128Mi
      maxAllowed:
        cpu: 4
        memory: 4Gi
      controlledResources:
      - cpu
      - memory
```

### Custom Metrics

```rust
// Emit custom metrics for scaling
use prometheus::{Counter, Gauge};

lazy_static::lazy_static! {
    pub static ref QUEUE_DEPTH: Gauge = Gauge::new(
        "tai_queue_depth",
        "Number of items in processing queue"
    ).unwrap();

    pub static ref REQUEST_LATENCY: Gauge = Gauge::new(
        "tai_request_latency_p99_seconds",
        "99th percentile request latency"
    ).unwrap();
}

pub async fn process_signals(queue: Arc<Queue>) {
    loop {
        let depth = queue.len();
        QUEUE_DEPTH.set(depth as f64);

        // Scale trigger: queue depth > 100
        if depth > 100 {
            tracing::warn!("Queue depth high: {}", depth);
        }

        // Process item
        if let Some(signal) = queue.pop().await {
            let start = Instant::now();
            process_signal(signal).await;
            let latency = start.elapsed();
            REQUEST_LATENCY.set(latency.as_secs_f64());
        }
    }
}
```

### Pod Disruption Budget (PDB)

```yaml
# Ensure minimum replicas during scale-down
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: governor-pdb
  namespace: tai-system
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: governor
```

## Scaling Policies by Service

| Service | Min Replicas | Max Replicas | Scale-Up | Scale-Down | Trigger |
|---------|-------------|-------------|----------|-----------|---------|
| Governor | 3 | 50 | +100% every 15s | -50% every 60s | CPU >70% |
| Coordinator | 2 | 100 | +100% every 15s | -50% every 60s | Kafka lag >100 |
| Scheduler | 2 | 50 | +100% every 15s | -50% every 60s | Queue >500 |

## Monitoring Autoscaling

```prometheus
# HPA metrics
karpenter_pods_allocated{namespace="tai-system",pod_name_prefix="governor"} 15
karpenter_pods_allocatable{namespace="tai-system"} 50

# KEDA metrics
keda_scaler_active{scaledObject="coordinator-keda"} 1
keda_scaler_metrics_value{scaledObject="coordinator-keda",scalerIndex="0"} 45.2

# Scale events
tai_scale_up_events_total{service="coordinator"} 23
tai_scale_down_events_total{service="coordinator"} 12
```

### Alerts

```yaml
- name: HighScalingActivity
  expr: rate(tai_scale_up_events_total[5m]) > 1
  for: 5m
  severity: warning
  annotations:
    summary: "Frequent scaling events suggest underlying issue"

- name: MaxReplicasReached
  expr: karpenter_pods_allocated == karpenter_pods_allocatable
  for: 1m
  severity: critical
  annotations:
    summary: "Service at maximum replicas, may need higher limit"
```

## Cost Optimization

```yaml
# Use preemptible/spot instances for flexible workloads
apiVersion: apps/v1
kind: Deployment
metadata:
  name: coordinator
spec:
  template:
    spec:
      affinity:
        nodeAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            preference:
              matchExpressions:
              - key: cloud.google.com/gke-preemptible
                operator: In
                values:
                - "true"
```

## Testing Autoscaling

```bash
# Load test to trigger scaling
kubectl run -it --rm load-generator --image=busybox /bin/sh

# Inside pod:
while sleep 0.01; do wget -q -O- http://governor:50051; done

# Monitor scaling
watch kubectl get hpa governor-hpa -n tai-system
watch kubectl get pods -n tai-system -l app=governor

# Expected behavior:
# - Pods scale from 3 to 50 over 1 minute
# - CPU utilization drops below 70%
# - Scaling stabilizes
# - After load stops, scale back down over 5 minutes
```

## Consequences

### Positive
- Automatic response to load
- Cost reduction (scale down in quiet periods)
- Better resource utilization
- No manual intervention
- Handles traffic spikes gracefully

### Negative
- Scaling takes time (1-5 minutes)
- Cold starts slow first requests
- Configuration complexity
- Metrics collection overhead
- Scaling thrashing possible (needs tuning)

## SLOs for Scaling

- **Scale-up latency:** <30 seconds from metric change to new pod ready
- **Scale-down latency:** <5 minutes from metric improvement
- **Accuracy:** ±10% of target utilization
- **False positive rate:** <5% (unnecessary scaling)

## References
- [Kubernetes HPA](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/)
- [KEDA Documentation](https://keda.sh/docs/)
- [Vertical Pod Autoscaler](https://github.com/kubernetes/autoscaler/tree/master/vertical-pod-autoscaler)
- [Google Cloud Autoscaling](https://cloud.google.com/kubernetes-engine/docs/concepts/horizontalpodautoscaler)
