<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Operator Quick-Start Guide](#tps-operator-quick-start-guide)
  - [Prerequisites Check (1 minute)](#prerequisites-check-1-minute)
  - [5-Minute Deployment](#5-minute-deployment)
    - [Step 1: Create Namespace (30 seconds)](#step-1-create-namespace-30-seconds)
    - [Step 2: Create Storage (1 minute)](#step-2-create-storage-1-minute)
    - [Step 3: Deploy TPS System (2 minutes)](#step-3-deploy-tps-system-2-minutes)
    - [Step 4: Verify System is Healthy (30 seconds)](#step-4-verify-system-is-healthy-30-seconds)
    - [Step 5: Access Dashboards (30 seconds)](#step-5-access-dashboards-30-seconds)
  - [Send Your First Signal (1 minute)](#send-your-first-signal-1-minute)
  - [Verify Observability (1 minute)](#verify-observability-1-minute)
    - [Check Metrics in Prometheus](#check-metrics-in-prometheus)
    - [Check Traces in Jaeger](#check-traces-in-jaeger)
    - [Check Logs in Loki/Grafana](#check-logs-in-lokigrafana)
  - [Production Readiness Checklist](#production-readiness-checklist)
  - [Common Issues During Quick-Start](#common-issues-during-quick-start)
    - [Issue: Pod stuck in Pending](#issue-pod-stuck-in-pending)
    - [Issue: Pod in CrashLoopBackOff](#issue-pod-in-crashloopbackoff)
    - [Issue: API endpoint returns 503 Service Unavailable](#issue-api-endpoint-returns-503-service-unavailable)
    - [Issue: Grafana shows "No Data"](#issue-grafana-shows-no-data)
  - [Next Steps](#next-steps)
  - [Cleanup (When Done)](#cleanup-when-done)
  - [Success Summary](#success-summary)
  - [Support](#support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Operator Quick-Start Guide

**Version**: 1.0
**Duration**: 5 minutes to production
**Audience**: SRE/DevOps engineers (basic infrastructure familiarity)
**Goal**: Get TPS system running on GKE from zero to hello-world

---

## Prerequisites Check (1 minute)

Before you start, verify you have:
- GKE cluster with 3+ nodes (`kubectl get nodes`)
- `kubectl` configured (`kubectl cluster-info`)
- Docker (can pull images)
- `helm` installed (version 3.0+)
- Internet access (download images from registry)

**If you're missing any:** Stop here and set up GKE first. Skip this guide until you're ready.

---

## 5-Minute Deployment

### Step 1: Create Namespace (30 seconds)

```bash
# Create isolated namespace for TPS system
kubectl create namespace tps-system
kubectl label namespace tps-system tier=system

# Verify it exists
kubectl get namespace tps-system
```

**What this does**: Creates an isolated environment so TPS system won't interfere with other workloads.

---

### Step 2: Create Storage (1 minute)

```bash
# Create persistent volumes for Prometheus and Grafana
cat > /tmp/pv.yaml << 'EOF'
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: prometheus-storage
  namespace: tps-system
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 20Gi
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: grafana-storage
  namespace: tps-system
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 5Gi
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: loki-storage
  namespace: tps-system
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
EOF

kubectl apply -f /tmp/pv.yaml

# Wait for PVCs to be Bound
kubectl wait --for=condition=Bound pvc/prometheus-storage -n tps-system --timeout=60s
kubectl wait --for=condition=Bound pvc/grafana-storage -n tps-system --timeout=60s
kubectl wait --for=condition=Bound pvc/loki-storage -n tps-system --timeout=60s

# Verify all are bound
kubectl get pvc -n tps-system
# Should show: prometheus-storage, grafana-storage, loki-storage all Bound
```

**What this does**: Allocates persistent storage so data survives pod restarts.

---

### Step 3: Deploy TPS System (2 minutes)

```bash
# Add helm repository (if not already added)
helm repo add tps https://charts.tps-reference.io
helm repo update

# Deploy TPS system with default configuration
helm install tps-release tps/tps-reference \
  --namespace tps-system \
  --set replicas=2 \
  --set prometheus.storage.size=20Gi \
  --set grafana.storage.size=5Gi \
  --set loki.storage.size=10Gi \
  --wait --timeout=5m

# Verify deployment
kubectl rollout status deployment/tps-system -n tps-system --timeout=3m
kubectl get pods -n tps-system
# Should show: tps-system-*, prometheus-*, grafana-*, loki-*, jaeger-*
```

**What this does**: Deploys the entire TPS system stack including:
- TPS application (2 replicas)
- Prometheus (metrics collection)
- Grafana (visualization)
- Loki (log aggregation)
- Jaeger (distributed tracing)

---

### Step 4: Verify System is Healthy (30 seconds)

```bash
# Check all pods are running
kubectl get pods -n tps-system -o wide
# All pods should show STATUS: Running

# Check services
kubectl get svc -n tps-system
# Should show: tps-api, prometheus, grafana, jaeger, loki

# Test API health
kubectl exec -it deployment/tps-system -n tps-system -- \
  curl -s http://localhost:8080/health | jq .
# Should show: {"status":"healthy","components":[...]}
```

**Success criteria**:
- All pods in Running state
- API returns `{"status":"healthy"}`
- No pods in Error or CrashLoopBackOff state

---

### Step 5: Access Dashboards (30 seconds)

```bash
# Forward Grafana port
kubectl port-forward -n tps-system svc/grafana 3000:3000 &

# Forward Jaeger port
kubectl port-forward -n tps-system svc/jaeger 16686:16686 &

# Open in browser
# Grafana: http://localhost:3000 (default user: admin, password: admin)
# Jaeger: http://localhost:16686
```

**What you should see in Grafana**:
- TPS Overview dashboard shows system health
- Queue depth graph (should start at 0)
- Worker utilization (should be 0-5% at startup)
- Circuit breaker status (should be Closed)

---

## Send Your First Signal (1 minute)

```bash
# Port-forward API
kubectl port-forward -n tps-system svc/tps-api 8080:8080 &

# Send test signal (from another terminal)
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{
    "signal_type": "execute",
    "payload": {
      "task": "hello-world",
      "priority": "normal"
    }
  }' | jq .

# Response should be:
# {
#   "signal_id": "sig_abc123...",
#   "status": "accepted",
#   "timestamp": "2026-01-25T14:30:00Z"
# }

# Check signal status
curl http://localhost:8080/signal/sig_abc123.../status | jq .
# Status should be: "completed" (if worker processed it immediately)
```

**What's happening**:
1. Signal sent to API
2. API queues signal in Kanban queue
3. Worker picks up signal
4. Jidoka checks if downstream is OK
5. Signal processes successfully
6. Metrics recorded, trace created
7. Result visible in Grafana

---

## Verify Observability (1 minute)

### Check Metrics in Prometheus

```bash
# Forward Prometheus
kubectl port-forward -n tps-system svc/prometheus 9090:9090 &

# Open http://localhost:9090/graph
# Run these queries:
# - tps_queue_depth  → should show value 0 or 1
# - tps_signal_latency_p99 → should show < 100ms
# - tps_circuit_breaker_state → should show 0 (Closed)
```

### Check Traces in Jaeger

```bash
# Open http://localhost:16686
# Find your service: "tps-system"
# Click trace from a few seconds ago
# Should show full trace: API → Queue → Worker → Jidoka → Completion
```

### Check Logs in Loki/Grafana

```bash
# In Grafana, go to "Explore"
# Select "Loki" data source
# Run: {namespace="tps-system"} | json
# Should show structured logs with level, component, message
```

---

## Production Readiness Checklist

After deployment, verify:

- [ ] All pods in Running state (`kubectl get pods -n tps-system`)
- [ ] No restart loops (`kubectl describe pod <pod> -n tps-system`)
- [ ] API healthy (`curl http://localhost:8080/health`)
- [ ] Metrics flowing into Prometheus (check dashboard)
- [ ] Traces appearing in Jaeger (check service dropdown)
- [ ] Logs appearing in Loki (check Explore view)
- [ ] Can send signal and see it in metrics (curl POST /signal)
- [ ] Circuit breaker is Closed (not in Error state)
- [ ] Queue depth is stable (not growing indefinitely)

---

## Common Issues During Quick-Start

### Issue: Pod stuck in Pending

**Symptom**: `kubectl get pods -n tps-system` shows `Pending` status

**Solution**:
```bash
# Check what's wrong
kubectl describe pod <pod-name> -n tps-system
# Look for "Events" section at bottom

# Most common causes:
# 1. PVC not Bound: Check "kubectl get pvc -n tps-system"
# 2. Node not ready: Check "kubectl get nodes"
# 3. Resource quota exceeded: Check "kubectl describe quota -n tps-system"

# Fix PVC issue (most common)
kubectl delete pvc --all -n tps-system
# Then re-apply PVC YAML from Step 2
```

### Issue: Pod in CrashLoopBackOff

**Symptom**: Pod keeps restarting (`0/1` in STATUS column)

**Solution**:
```bash
# Check logs
kubectl logs <pod-name> -n tps-system
# Look for configuration errors or missing dependencies

# Common causes:
# 1. Missing database: Check if PostgreSQL is running
# 2. Invalid config: Check helm values in deployment
# 3. Out of memory: Check node resources "kubectl top nodes"

# Fix by checking events
kubectl describe pod <pod-name> -n tps-system | tail -30
```

### Issue: API endpoint returns 503 Service Unavailable

**Symptom**: `curl http://localhost:8080/health` returns 503

**Solution**:
```bash
# Check if pods are actually running
kubectl get pods -n tps-system | grep tps-api

# If pods are running, check component health
kubectl logs -l app=tps-system -n tps-system | tail -50
# Look for error messages about downstream services

# Restart just the TPS system pods
kubectl rollout restart deployment/tps-system -n tps-system
```

### Issue: Grafana shows "No Data"

**Symptom**: Dashboard panels are empty or show "N/A"

**Solution**:
```bash
# Check if Prometheus is collecting metrics
# Go to http://localhost:9090/targets
# All targets should be "Up" (green)

# If "Down", check Prometheus logs
kubectl logs -l app=prometheus -n tps-system | tail -30

# Rescrape by restarting Prometheus
kubectl rollout restart statefulset/prometheus -n tps-system

# Wait 60 seconds for data to arrive
sleep 60
# Refresh Grafana dashboard
```

---

## Next Steps

Congratulations! Your TPS system is running. Now:

1. **Understand the dashboards** → Read [05-grafana-dashboard-guide.md](05-grafana-dashboard-guide.md)
2. **Load test the system** → Run `kubectl exec deployment/tps-system -n tps-system -- /opt/load-test normal`
3. **Read the handbook** → [handbook-tps-operations.md](handbook-tps-operations.md) for detailed operations
4. **Try hands-on labs** → [02-hands-on-labs.md](02-hands-on-labs.md) to learn each TPS principle

---

## Cleanup (When Done)

```bash
# Uninstall entire TPS system
helm uninstall tps-release -n tps-system

# Delete namespace (will delete all resources in it)
kubectl delete namespace tps-system

# Verify cleanup
kubectl get namespace tps-system  # Should return "Not Found"
```

---

## Success Summary

You just completed a production-grade TPS system deployment in ~5 minutes:

✓ 5 Kubernetes services running
✓ Metrics collection (Prometheus)
✓ Visualization (Grafana)
✓ Log aggregation (Loki)
✓ Distributed tracing (Jaeger)
✓ API responding to signals
✓ Full observability stack

**Time spent**: ~5 minutes
**Lines of config**: ~30
**Success rate**: Should be 100% if GKE cluster is ready

---

## Support

- **Troubleshooting**: See [03-troubleshooting-decision-trees.md](03-troubleshooting-decision-trees.md)
- **Dashboard guide**: See [05-grafana-dashboard-guide.md](05-grafana-dashboard-guide.md)
- **Full operations handbook**: See [handbook-tps-operations.md](handbook-tps-operations.md)
- **Configuration tuning**: See [07-configuration-tuning-guide.md](07-configuration-tuning-guide.md)

---

**Status**: Production-Ready
**Last Updated**: January 2026
**Tested On**: GKE 1.25+ with n1-standard-4 nodes
