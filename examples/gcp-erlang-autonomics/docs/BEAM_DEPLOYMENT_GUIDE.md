# BEAM Erlang Autonomics Deployment Guide

## Complete Guide to Deploying Erlang Autonomics on GCP

**Status**: Production-Ready | **Version**: 1.0.0 | **Last Updated**: 2026-01-25

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Prerequisites](#prerequisites)
3. [Local Development Setup](#local-development-setup)
4. [BEAM Release Build](#beam-release-build)
5. [GKE Deployment](#gke-deployment)
6. [BEAM Clustering](#beam-clustering)
7. [Hot Code Reloading](#hot-code-reloading)
8. [Operations & Monitoring](#operations--monitoring)
9. [Troubleshooting](#troubleshooting)

---

## Architecture Overview

### Component Stack

```
┌─────────────────────────────────────────────────────────────────┐
│ GCP Cloud Run / GKE (Kubernetes)                                │
├─────────────────────────────────────────────────────────────────┤
│ BEAM Erlang Runtime (1.0.0)                                     │
├─────────────────────────────────────────────────────────────────┤
│ Autonomic Governors (gen_statem FSMs)                           │
│ ├─ cost_guard_governor      (Cost Circuit Breaker)             │
│ ├─ rollback_guard_governor  (Deploy Rollback Guard)            │
│ └─ backlog_valve_governor   (Backlog Pressure Valve)           │
├─────────────────────────────────────────────────────────────────┤
│ Infrastructure Layer                                             │
│ ├─ Receipt Ledger (cryptographic audit trail)                  │
│ ├─ Tenant Registry (multi-tenancy isolation)                   │
│ ├─ Signal Normalizer (Pub/Sub ingestion)                       │
│ ├─ HTTP Server (Cloud Run endpoint)                            │
│ └─ Clustering Manager (distributed consensus)                 │
├─────────────────────────────────────────────────────────────────┤
│ External Services                                               │
│ ├─ GCP Pub/Sub (telemetry signals)                             │
│ ├─ Cloud Monitoring (metrics export)                           │
│ ├─ Cloud Logging (structured audit logs)                       │
│ └─ Cloud Storage (receipt persistence)                         │
└─────────────────────────────────────────────────────────────────┘
```

### BEAM Clustering Topology

- **Headless Service**: `erlang-autonomics-headless:4369` (EPMD + distribution)
- **StatefulSet**: 3-10 replicas (min 3 for quorum)
- **Distribution Ports**: 9000-9100 (40 per node)
- **Pod Network**: Mesh network (no NAT)

---

## Prerequisites

### System Requirements

- **Erlang/OTP**: 24.0 or later
- **Reltool/Relx**: Latest (for release packaging)
- **Docker**: 20.10+ (for container builds)
- **kubectl**: 1.20+ (for GKE deployment)
- **gcloud CLI**: Latest (for GCP authentication)

### GCP Setup

```bash
# Set project
export PROJECT_ID=ggen-autonomics
gcloud config set project $PROJECT_ID

# Enable required APIs
gcloud services enable container.googleapis.com
gcloud services enable pubsub.googleapis.com
gcloud services enable monitoring.googleapis.com
gcloud services enable logging.googleapis.com

# Create GKE cluster
gcloud container clusters create erlang-autonomics \
  --zone us-central1-c \
  --num-nodes 3 \
  --machine-type n1-standard-2 \
  --enable-stackdriver-kubernetes \
  --enable-ip-alias \
  --addons HttpLoadBalancing \
  --workload-pool=$PROJECT_ID.svc.id.goog

# Get credentials
gcloud container clusters get-credentials erlang-autonomics \
  --zone us-central1-c
```

### GCP Service Account

```bash
# Create service account
gcloud iam service-accounts create erlang-autonomics \
  --display-name="Erlang Autonomics BEAM"

# Grant permissions
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member=serviceAccount:erlang-autonomics@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/pubsub.subscriber

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member=serviceAccount:erlang-autonomics@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/monitoring.metricWriter

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member=serviceAccount:erlang-autonomics@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/logging.logWriter

# Create key
gcloud iam service-accounts keys create /tmp/sa-key.json \
  --iam-account=erlang-autonomics@$PROJECT_ID.iam.gserviceaccount.com
```

---

## Local Development Setup

### Build Release Locally

```bash
cd /home/user/ggen/examples/gcp-erlang-autonomics

# Clean previous builds
rm -rf _build

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Build release
rebar3 release

# Verify release
ls -la _build/default/rel/erlang-autonomics/
```

### Start Local BEAM Node

```bash
# Extract release
RELEASE_DIR=_build/default/rel/erlang-autonomics

# Start in foreground (development)
$RELEASE_DIR/bin/erlang-autonomics console

# Or start in background
$RELEASE_DIR/bin/erlang-autonomics start

# Connect to remote shell
$RELEASE_DIR/bin/erlang-autonomics remote_console

# Check status
$RELEASE_DIR/bin/erlang-autonomics status

# Stop
$RELEASE_DIR/bin/erlang-autonomics stop
```

### Test Local Governor

```erlang
%% In remote_console
1> cost_guard_governor:start_link(#{tenant_id => <<"test-tenant-1">>}).
{ok, <0.123.0>}

2> cost_guard_governor:call(governor, {signal, billing_spike, #{usd_per_hour => 100}}).
{warn, billing_spike}

3> receipt_ledger:last_hash(<<"test-tenant-1">>).
<<16#abc123...>>
```

---

## BEAM Release Build

### Reltool Configuration

See `priv/reltool.config` for complete release specification:

- **Applications Included**: kernel, stdlib, sasl, crypto, ssl, inets
- **Governor SKUs**: cost_guard, rollback_guard, backlog_valve
- **Embedded Profile**: Production-optimized (stripped .beam files)
- **Release Name**: erlang-autonomics (1.0.0)

### Build Release

```bash
# Build tarball for distribution
rebar3 release tar

# Output: _build/default/rel/erlang-autonomics-1.0.0.tar.gz

# Verify tarball
tar tzf _build/default/rel/erlang-autonomics-1.0.0.tar.gz | head -20
```

### Docker Container Build

```dockerfile
FROM otp:25-slim

WORKDIR /srv

# Copy release
COPY _build/default/rel/erlang-autonomics-1.0.0.tar.gz .
RUN tar xzf erlang-autonomics-1.0.0.tar.gz

# Copy configuration
COPY priv/vm.args erlang-autonomics/etc/
COPY priv/sys.config erlang-autonomics/etc/

# Health check
HEALTHCHECK --interval=10s --timeout=5s --start-period=30s --retries=3 \
  CMD erlang-autonomics/bin/erlang-autonomics ping || exit 1

EXPOSE 4369 8080 8888 9000-9100

ENTRYPOINT ["erlang-autonomics/bin/erlang-autonomics"]
CMD ["foreground"]
```

### Push to GCR

```bash
# Build and push to Google Container Registry
docker build -t erlang-autonomics:1.0.0 .
docker tag erlang-autonomics:1.0.0 gcr.io/$PROJECT_ID/erlang-autonomics:1.0.0
docker push gcr.io/$PROJECT_ID/erlang-autonomics:1.0.0
```

---

## GKE Deployment

### Deploy to Kubernetes

```bash
# Create namespace and deploy
kubectl apply -f k8s/beam-statefulset.yaml

# Verify StatefulSet
kubectl get statefulset -n autonomic-system
kubectl describe statefulset erlang-autonomics-cluster -n autonomic-system

# Wait for pods to be ready
kubectl wait --for=condition=ready pod \
  -l app=erlang-autonomics \
  -n autonomic-system \
  --timeout=300s

# Check pod status
kubectl get pods -n autonomic-system -o wide

# View logs
kubectl logs -n autonomic-system erlang-autonomics-cluster-0
kubectl logs -n autonomic-system erlang-autonomics-cluster-1
```

### Verify Deployment

```bash
# Port forward to local machine
kubectl port-forward -n autonomic-system svc/erlang-autonomics-http 8080:8080 &

# Test health endpoint
curl http://localhost:8080/health
# Expected: {"status":"ok","sku":"..."}

# Check metrics
curl http://localhost:8080/metrics
```

---

## BEAM Clustering

### Automatic Clustering

BEAM clustering is automatic via:

1. **DNS Resolution**: Kubernetes DNS resolves `erlang-autonomics-headless.autonomic-system.svc.cluster.local`
2. **EPMD (Port 4369)**: Erlang Port Mapper Daemon (auto-started by BEAM)
3. **Distribution**: BEAM nodes discover and connect on ports 9000-9100

### Manual Node Management

```erlang
%% In remote_console of node-0

1> erlang:nodes().
['erlang-autonomics-cluster-1@erlang-autonomics-headless.autonomic-system.svc.cluster.local',
 'erlang-autonomics-cluster-2@erlang-autonomics-headless.autonomic-system.svc.cluster.local']

2> net_kernel:start(['erlang-autonomics-cluster-0@erlang-autonomics-headless.autonomic-system.svc.cluster.local', longnames]).
{ok, <0.25.0>}

3> rpc:call('erlang-autonomics-cluster-1@...', erlang, statistics, [processes]).
{ok, {40, 4096}}
```

### Check Clustering Health

```bash
# From any pod
kubectl exec -it erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console

# In remote_console
erlang:nodes().            % List connected nodes
net_kernel:nodes_info().   % Connection info
erlang:get_env(kernel, inet_dist_listen_min).  % Distribution ports
```

### Cluster Monitoring

```bash
# Watch cluster status
watch -n 2 'kubectl exec -q erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console -c "erlang:nodes()."'
```

---

## Hot Code Reloading

### Zero-Downtime Upgrade

1. **Prepare new release**:
   ```bash
   rebar3 release
   tar xzf _build/default/rel/erlang-autonomics-1.0.0.tar.gz -C /tmp/new-release/
   ```

2. **Trigger upgrade**:
   ```erlang
   release_handler:upgrade().
   {ok, "1.0.0"}
   ```

3. **Upgrade automatically rolls back on error**:
   ```erlang
   release_handler:downgrade().
   {ok, "0.9.0"}
   ```

### Production Rolling Update

```bash
# kubectl handles rolling updates automatically
kubectl set image statefulset/erlang-autonomics-cluster \
  erlang-autonomics=gcr.io/$PROJECT_ID/erlang-autonomics:1.1.0 \
  -n autonomic-system

# Monitor rollout
kubectl rollout status statefulset/erlang-autonomics-cluster -n autonomic-system

# Watch for in-flight requests draining
kubectl logs -f -n autonomic-system erlang-autonomics-cluster-0
```

---

## Operations & Monitoring

### Application Health

```bash
# Health probe (liveness)
curl http://localhost:8080/health

# Readiness probe (in-cluster)
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  curl -s http://localhost:8888/ready

# Metrics (Prometheus format)
curl http://localhost:9090/metrics
```

### Monitor via Cloud Monitoring

```bash
# View metrics in console
gcloud monitoring metrics-descriptors list \
  --filter='metric.type:erlang*'

# Query via gcloud
gcloud monitoring time-series list \
  --filter='metric.type="erlang_autonomics.governor_transitions"'
```

### View Logs

```bash
# Structured logging to Cloud Logging
gcloud logging read "resource.type=k8s_container AND resource.labels.pod_name=erlang-autonomics-cluster-0" \
  --limit 50

# Follow logs
kubectl logs -f -n autonomic-system erlang-autonomics-cluster-0
```

---

## Troubleshooting

### Pods Not Starting

```bash
# Check pod events
kubectl describe pod erlang-autonomics-cluster-0 -n autonomic-system

# Check resource availability
kubectl describe nodes

# Verify image pull
kubectl get events -n autonomic-system --sort-by='.lastTimestamp'
```

### Clustering Issues

```bash
# Check if nodes can reach each other
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  ping erlang-autonomics-headless.autonomic-system.svc.cluster.local

# Check EPMD
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  netstat -tlnp | grep 4369

# Force node reconnection
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console << 'EOF'
net_kernel:connect_node('erlang-autonomics-cluster-1@erlang-autonomics-headless.autonomic-system.svc.cluster.local').
EOF
```

### High Memory Usage

```bash
# Check memory distribution
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console << 'EOF'
erlang:memory().
EOF

# Trigger garbage collection
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console << 'EOF'
erlang:garbage_collect().
EOF
```

### Message Queue Backlog

```bash
# Check process message queue
kubectl exec erlang-autonomics-cluster-0 -n autonomic-system -- \
  erlang-autonomics/bin/erlang-autonomics remote_console << 'EOF'
[{Pid, erlang:process_info(Pid, message_queue_len)} || Pid <- erlang:processes(), erlang:process_info(Pid, message_queue_len) > 1000].
EOF
```

---

## Scale Management

### Horizontal Scaling

```bash
# Manual scale to 5 nodes
kubectl scale statefulset erlang-autonomics-cluster \
  --replicas=5 \
  -n autonomic-system

# Monitor scaling
kubectl get statefulset -n autonomic-system -w
```

### Vertical Scaling

```bash
# Update resource requests/limits in StatefulSet
kubectl patch statefulset erlang-autonomics-cluster -n autonomic-system --type='json' \
  -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/resources/requests/memory", "value":"1Gi"}]'

# Rolling restart to apply
kubectl delete pod erlang-autonomics-cluster-0 -n autonomic-system
```

---

## Maintenance

### Backup Receipt Ledger

```bash
# Export receipts from all nodes
for i in 0 1 2; do
  kubectl exec erlang-autonomics-cluster-$i -n autonomic-system -- \
    receipt_ledger:export_to_gcs(<<"erlang-autonomics-receipts">>) &
done
wait
```

### Update Configuration

```bash
# Update sys.config in ConfigMap
kubectl edit configmap erlang-autonomics-sys-config -n autonomic-system

# Rolling restart to apply
kubectl rollout restart statefulset/erlang-autonomics-cluster -n autonomic-system
```

---

## Next Steps

- [BEAM Clustering Advanced](BEAM_CLUSTERING_ADVANCED.md)
- [Performance Tuning](PERFORMANCE_TUNING.md)
- [Security Hardening](SECURITY_HARDENING.md)
- [Disaster Recovery](DISASTER_RECOVERY.md)

---

**Questions?** → See [Troubleshooting Guide](TROUBLESHOOTING.md) or [FAQ](FAQ.md)
