<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI Kubernetes Integration Guide](#tai-kubernetes-integration-guide)
  - [Overview](#overview)
  - [Architecture](#architecture)
    - [Deployment Topology](#deployment-topology)
  - [Components](#components)
    - [1. Helm Chart (`helm/tai-chart/`)](#1-helm-chart-helmtai-chart)
    - [2. KEDA Autoscaling](#2-keda-autoscaling)
      - [CPU and Memory Scaling](#cpu-and-memory-scaling)
      - [Kafka Topic Lag Scaling](#kafka-topic-lag-scaling)
      - [Google Cloud Pub/Sub Queue Depth](#google-cloud-pubsub-queue-depth)
      - [Custom Metrics](#custom-metrics)
    - [3. Istio Service Mesh](#3-istio-service-mesh)
    - [4. Network Policies](#4-network-policies)
    - [5. RBAC (Role-Based Access Control)](#5-rbac-role-based-access-control)
  - [Deployment Guide](#deployment-guide)
    - [Prerequisites](#prerequisites)
    - [Step 1: Prepare Values](#step-1-prepare-values)
    - [Step 2: Deploy TAI](#step-2-deploy-tai)
    - [Step 3: Verify Deployment](#step-3-verify-deployment)
    - [Step 4: Configure Autoscaling (KEDA)](#step-4-configure-autoscaling-keda)
    - [Step 5: Test Canary Deployment](#step-5-test-canary-deployment)
  - [Monitoring](#monitoring)
    - [Prometheus Metrics](#prometheus-metrics)
    - [Istio Metrics](#istio-metrics)
    - [KEDA Metrics](#keda-metrics)
  - [Troubleshooting](#troubleshooting)
    - [Pods not scaling up](#pods-not-scaling-up)
    - [Istio traffic not routing](#istio-traffic-not-routing)
    - [Network policies blocking traffic](#network-policies-blocking-traffic)
  - [Performance Tuning](#performance-tuning)
    - [CPU/Memory Limits](#cpumemory-limits)
    - [Connection Pool](#connection-pool)
    - [Cooldown Periods](#cooldown-periods)
  - [Security Best Practices](#security-best-practices)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI Kubernetes Integration Guide

## Overview

TAI (Trading AI) system provides production-grade Kubernetes deployment infrastructure with:
- **Helm chart** for reproducible, declarative deployments
- **KEDA autoscaling** for intelligent scaling based on metrics and events
- **Istio service mesh** for advanced traffic management
- **Network policies** for security and isolation
- **RBAC** for fine-grained access control

## Architecture

### Deployment Topology

```
┌─────────────────────────────────────────────────────────────┐
│                    Kubernetes Cluster                       │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────────────────────────────────────────────┐   │
│  │          TAI-API Deployment (3-10 replicas)          │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐           │   │
│  │  │ Pod 1    │  │ Pod 2    │  │ Pod 3    │           │   │
│  │  │ 8080     │  │ 8080     │  │ 8080     │           │   │
│  │  └──────────┘  └──────────┘  └──────────┘           │   │
│  └──────────────────────────────────────────────────────┘   │
│           │                                                  │
│  ┌────────┴────────────────────────────────────────────┐   │
│  │        Istio VirtualService (Canary/Blue-Green)    │   │
│  │                                                      │   │
│  │  - Route 90% to stable (v1)                         │   │
│  │  - Route 10% to canary (v2-canary)                  │   │
│  └─────────────────────────────────────────────────────┘   │
│                   │                                          │
│  ┌────────────────┴──────────────────────────────────────┐  │
│  │  KEDA ScaledObject (CPU + Kafka Event Scaling)      │  │
│  │  - Min: 3 replicas                                   │  │
│  │  - Max: 10 replicas                                  │  │
│  │  - Scale up: CPU > 70% or Kafka lag > 1000 msgs    │  │
│  │  - Scale down: stability window 5min                │  │
│  └────────────────┬──────────────────────────────────────┘  │
│                   │                                          │
│  ┌────────────────┴──────────────────────────────────────┐  │
│  │     Network Policy (Ingress/Egress)                 │  │
│  │     - Allow ingress from nginx-ingress controller   │  │
│  │     - Allow egress to cache and DNS only            │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
│  ┌────────────────────────────────────────────────────────┐  │
│  │         TAI-Cache StatefulSet (Redis)               │  │
│  │         - 3 replicas with persistent storage        │  │
│  │         - 10Gi per replica                          │  │
│  │         - Anti-affinity: different nodes            │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
│  ┌────────────────────────────────────────────────────────┐  │
│  │      TAI-Worker Deployment (2-20 replicas)          │  │
│  │      - Event-driven by Kafka topic lag              │  │
│  │      - Processes background jobs                    │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Components

### 1. Helm Chart (`helm/tai-chart/`)

Helm is the package manager for Kubernetes. Our TAI chart provides:

**Chart Structure:**
```
helm/tai-chart/
├── Chart.yaml                 # Chart metadata
├── values.yaml               # Default configuration values
└── templates/
    ├── deployment.yaml       # API and worker deployments
    ├── service.yaml          # ClusterIP and headless services
    ├── configmap.yaml        # Configuration data
    ├── statefulset.yaml      # Redis cache with persistent storage
    ├── serviceaccount.yaml   # RBAC service account
    └── NOTES.txt             # Post-install instructions
```

**Deployment:**
```bash
# Install TAI with defaults
helm install tai ./helm/tai-chart

# Install with custom values
helm install tai ./helm/tai-chart \
  --set api.replicaCount=5 \
  --set api.image.tag=v1.2.3

# Upgrade existing deployment
helm upgrade tai ./helm/tai-chart \
  --values custom-values.yaml

# Uninstall
helm uninstall tai
```

**Key Values:**
- `api.replicaCount`: Initial number of API pod replicas (KEDA can scale this)
- `api.image.tag`: Container image version
- `cache.replicaCount`: Number of Redis replicas
- `istio.enabled`: Enable Istio traffic management
- `keda.enabled`: Enable KEDA autoscaling
- `networkPolicy.enabled`: Enable network isolation

### 2. KEDA Autoscaling

KEDA (Kubernetes Event Driven Autoscaling) enables intelligent scaling based on metrics and external events.

**Supported Metric Types:**

#### CPU and Memory Scaling
```yaml
metrics:
  - type: cpu
    target_utilization_percentage: 70
  - type: memory
    target_utilization_percentage: 80
```

#### Kafka Topic Lag Scaling
When processing messages from Kafka, scale based on consumer lag:
```yaml
metrics:
  - type: kafka
    brokers: ["kafka:9092"]
    topic: tai-events
    consumer_group: tai-workers
    lagThreshold: 1000  # Scale up at 1000 messages behind
```

#### Google Cloud Pub/Sub Queue Depth
```yaml
metrics:
  - type: pubsub
    project_id: ggen-project
    subscription: tai-events
    target_queue_length: 100  # Scale up at 100 messages per replica
```

#### Custom Metrics
Using Prometheus or other metrics sources:
```yaml
metrics:
  - type: custom
    metric_name: http_requests_per_second
    target_value: "1000"
```

**Scaling Behavior:**

```
CPU Usage Over Time:
└──┬────┬────┬────┬────┬────┬────┬────┬────┬────┐
   │    │    │    │    │    │    │    │    │    │
   ├────┘ 40%│    │    │    │    │    │    │    │
   │         ├────┘ 65% │    │    │    │    │    │
   │         │          ├────┘ 75% │ (SCALE UP!) │
   │         │          │          ├────┬────────┘
   │         │          │          │ 72%│ new capacity
   │         │          │          ├────┘ 40%
   │         │          │          │

Min: 3 replicas
Max: 10 replicas
Target CPU: 70%
Scale-up: immediate (0s stabilization)
Scale-down: wait 300s before scaling down
```

**Configuration in values.yaml:**
```yaml
keda:
  enabled: true
  scaledObjects:
    - name: tai-api-scaler
      target: tai-api
      kind: Deployment
      minReplicas: 3          # Never scale below 3
      maxReplicas: 10         # Never scale above 10
      metrics:
        - type: cpu
          target: 70
        - type: memory
          target: 80
```

### 3. Istio Service Mesh

Istio provides advanced traffic management, security, and observability.

**VirtualService (Routing)**

Controls how traffic is routed to services:

```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: tai-vs
  namespace: default
spec:
  hosts:
    - tai-api.example.com
  gateways:
    - tai-gateway
  http:
    - match:
        - uri:
            prefix: /api/v1
      route:
        - destination:
            host: tai-api
            subset: stable
          weight: 90          # 90% of traffic
        - destination:
            host: tai-api
            subset: canary
          weight: 10          # 10% canary traffic
      timeout: 30s
      retries:
        attempts: 3
        perTryTimeout: 10s
```

**DestinationRule (Load Balancing)**

Defines how to load balance traffic to instances:

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: tai-dr
  namespace: default
spec:
  host: tai-api
  trafficPolicy:
    loadBalancer:
      simple: ROUND_ROBIN
    connectionPool:
      tcp:
        maxConnections: 100
      http:
        http1MaxPendingRequests: 2048
        http2MaxRequests: 1000
  subsets:
    - name: stable
      labels:
        version: v1
    - name: canary
      labels:
        version: v2-canary
```

**Gateway (Ingress)**

Exposes services outside the cluster:

```yaml
apiVersion: networking.istio.io/v1beta1
kind: Gateway
metadata:
  name: tai-gateway
  namespace: default
spec:
  selector:
    istio: ingressgateway
  servers:
    - port:
        number: 80
        name: http
        protocol: HTTP
      hosts:
        - "tai-api.example.com"
    - port:
        number: 443
        name: https
        protocol: HTTPS
      tls:
        mode: SIMPLE
        credentialName: tai-api-tls
      hosts:
        - "tai-api.example.com"
```

**Traffic Patterns:**

1. **Canary Deployment (10% → 100%)**
   - Route 10% to v2-canary while monitoring metrics
   - If error rate is low, gradually increase to 50% → 100%
   - If errors spike, traffic stays at 10% or rolls back

2. **Blue-Green Deployment**
   - Route 100% to blue (stable v1)
   - Deploy green (v2) and verify
   - Switch 100% to green atomically
   - Keep blue as quick rollback

3. **A/B Testing**
   - Route based on header: `user-id % 2 == 0` → A, else → B
   - Measure conversion rates and business metrics
   - Winner becomes standard, loser deprecated

**PeerAuthentication (mTLS)**

Enforces mutual TLS between services:

```yaml
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
  namespace: default
spec:
  mtls:
    mode: STRICT  # All traffic must be mTLS
```

### 4. Network Policies

Kubernetes NetworkPolicy restricts pod-to-pod traffic (network segmentation).

**TAI Network Policy:**

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: tai-api-netpol
  namespace: default
spec:
  podSelector:
    matchLabels:
      app: tai-api
  policyTypes:
    - Ingress
    - Egress
  ingress:
    # Allow traffic from nginx ingress controller only
    - from:
        - podSelector:
            matchLabels:
              app: nginx-ingress
      ports:
        - protocol: TCP
          port: 8080
  egress:
    # Allow to cache (Redis)
    - to:
        - podSelector:
            matchLabels:
              app: tai-cache
      ports:
        - protocol: TCP
          port: 6379
    # Allow to DNS
    - to:
        - namespaceSelector:
            matchLabels:
              name: kube-system
      ports:
        - protocol: UDP
          port: 53
```

**Default Deny Pattern:**

```yaml
# Deny all ingress traffic by default
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-ingress
  namespace: default
spec:
  podSelector: {}
  policyTypes:
    - Ingress

# Deny all egress traffic by default
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-egress
  namespace: default
spec:
  podSelector: {}
  policyTypes:
    - Egress
```

### 5. RBAC (Role-Based Access Control)

Controls which service accounts can perform which actions.

**TAI Role:**
```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: tai-reader
  namespace: default
rules:
  - apiGroups: [""]
    resources: ["pods", "services", "configmaps"]
    verbs: ["get", "list", "watch"]
  - apiGroups: ["apps"]
    resources: ["deployments", "statefulsets"]
    verbs: ["get", "list", "watch"]
```

**Service Account Binding:**
```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: tai-reader-binding
  namespace: default
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: tai-reader
subjects:
  - kind: ServiceAccount
    name: tai
    namespace: default
```

## Deployment Guide

### Prerequisites

1. **Kubernetes Cluster**
   - GKE, EKS, or local minikube
   - kubectl configured

2. **Helm 3**
   ```bash
   curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash
   ```

3. **KEDA (optional, for autoscaling)**
   ```bash
   helm repo add kedacore https://kedacore.github.io/charts
   helm install keda kedacore/keda --namespace keda --create-namespace
   ```

4. **Istio (optional, for service mesh)**
   ```bash
   curl -sL https://istio.io/downloadIstio | sh -
   cd istio-*
   ./bin/istioctl install --set profile=demo -y
   ```

### Step 1: Prepare Values

Create `custom-values.yaml`:

```yaml
namespace: default

api:
  replicaCount: 3
  image:
    tag: "v1.0.0"

keda:
  enabled: true

istio:
  enabled: true
  peerAuthentication:
    enabled: true
    mode: STRICT

networkPolicy:
  enabled: true
```

### Step 2: Deploy TAI

```bash
# Add TAI repo (if distributed)
helm repo add tai https://charts.ggen.io
helm repo update

# Install
helm install tai tai/tai-chart \
  --namespace default \
  --create-namespace \
  -f custom-values.yaml

# Verify
kubectl get deployments
kubectl get statefulsets
kubectl get scaledobjects
kubectl get vs
kubectl get dr
```

### Step 3: Verify Deployment

```bash
# Check pods are running
kubectl get pods -l app=tai-api
kubectl logs -l app=tai-api -f

# Check KEDA scaler status
kubectl describe scaledobject tai-api-scaler

# Check Istio resources
kubectl get vs
kubectl get dr
kubectl get gateway

# Test connectivity
POD=$(kubectl get pods -l app=tai-api -o jsonpath='{.items[0].metadata.name}')
kubectl exec -it $POD -- curl localhost:8080/health
```

### Step 4: Configure Autoscaling (KEDA)

Monitor scaling in real-time:

```bash
# Watch replica count
kubectl get deployments -w

# Check KEDA metrics
kubectl describe scaledobject tai-api-scaler

# Check HPA status (if using)
kubectl describe hpa
```

### Step 5: Test Canary Deployment

```bash
# Deploy canary version
helm upgrade tai ./helm/tai-chart \
  --set api.image.tag=v1.1.0-canary

# Check Istio routing
kubectl describe vs tai-vs

# Monitor error rate
kubectl logs -l app=tai-api,version=v2-canary -f

# Promote or rollback
helm upgrade tai ./helm/tai-chart \
  --set api.image.tag=v1.1.0  # Promote v1.1.0 to stable
```

## Monitoring

### Prometheus Metrics

Pods expose metrics on `/metrics` port:

```
# In values.yaml
api:
  port: 8080

# ServiceMonitor (if using Prometheus Operator)
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: tai-api
spec:
  selector:
    matchLabels:
      app: tai-api
  endpoints:
    - port: http
      interval: 30s
      path: /metrics
```

### Istio Metrics

Istio automatically exports:
- Request rate (requests/sec)
- Error rate (%)
- Latency (p50, p95, p99)

```bash
# Install kiali dashboard
kubectl apply -f https://raw.githubusercontent.com/istio/istio/release-1.17/samples/addons/kiali.yaml

# View dashboard
kubectl port-forward svc/kiali -n istio-system 20000:20000
# Open http://localhost:20000
```

### KEDA Metrics

Monitor scaling decisions:

```bash
# Check KEDA scaler status
kubectl describe scaledobject tai-api-scaler

# Check active replicas vs target
kubectl get hpa

# KEDA logs
kubectl logs -n keda deployment/keda-operator -f
```

## Troubleshooting

### Pods not scaling up

1. **Check KEDA is installed**
   ```bash
   kubectl get deployment keda-operator -n keda
   ```

2. **Check ScaledObject status**
   ```bash
   kubectl describe scaledobject tai-api-scaler
   ```

3. **Check metrics server**
   ```bash
   kubectl get deployment metrics-server -n kube-system
   ```

4. **Check pod logs**
   ```bash
   kubectl logs -l app=tai-api --tail=50
   ```

### Istio traffic not routing

1. **Check VirtualService**
   ```bash
   kubectl describe vs tai-vs
   kubectl get vs -o yaml
   ```

2. **Check DestinationRule**
   ```bash
   kubectl describe dr tai-dr
   ```

3. **Check Gateway**
   ```bash
   kubectl get gateway
   kubectl describe gateway tai-gateway
   ```

4. **Check Istio sidecar injection**
   ```bash
   kubectl get pods -o jsonpath='{.items[*].spec.containers[*].name}'
   # Should contain "istio-proxy"
   ```

### Network policies blocking traffic

1. **Check policies are applied**
   ```bash
   kubectl get networkpolicies
   ```

2. **Test connectivity**
   ```bash
   kubectl exec -it <api-pod> -- curl <cache-pod-ip>:6379
   ```

3. **Disable policies temporarily** (for testing)
   ```bash
   kubectl delete networkpolicies --all
   ```

## Performance Tuning

### CPU/Memory Limits

Adjust in `values.yaml`:

```yaml
api:
  resources:
    requests:
      cpu: 500m        # Increase for CPU-bound workloads
      memory: 512Mi    # Increase for memory-bound workloads
    limits:
      cpu: 2000m
      memory: 2Gi
```

### Connection Pool

Adjust in Istio DestinationRule:

```yaml
spec:
  trafficPolicy:
    connectionPool:
      tcp:
        maxConnections: 1000      # Increase for high concurrency
      http:
        http1MaxPendingRequests: 2048
        http2MaxRequests: 10000
```

### Cooldown Periods

In KEDA ScaledObject:

```yaml
spec:
  cooldownPeriod: 300    # Wait 5min before scale-down
  fallback:
    replicas: 3          # Use 3 if metrics unavailable
```

## Security Best Practices

1. **Enable mTLS**
   ```bash
   istioctl install --set profile=production
   ```

2. **Enable NetworkPolicy**
   - Always specify ingress/egress rules
   - Default-deny pattern
   - Regular audits

3. **RBAC Least Privilege**
   - Only grant needed permissions
   - Use namespace-level roles
   - Review quarterly

4. **Pod Security Policy**
   - Disable privileged containers
   - Enforce read-only root filesystem
   - Drop all capabilities

5. **Secrets Management**
   - Use external secret manager (Vault, GCP Secret Manager)
   - Rotate regularly
   - Never commit to git

## References

- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [Helm Documentation](https://helm.sh/docs/)
- [KEDA Documentation](https://keda.sh/)
- [Istio Documentation](https://istio.io/latest/docs/)
- [NetworkPolicy Recipes](https://github.com/ahmetb/kubernetes-network-policy-recipes)
