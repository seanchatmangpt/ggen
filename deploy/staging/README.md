# ggen v6.0.0 Staging Deployment

Production-grade Docker Compose deployment for ggen clustering, event store management, and metrics collection.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Staging Cluster (3 nodes)                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ ggen-node-1  │  │ ggen-node-2  │  │ ggen-node-3  │      │
│  │  :8080       │  │  :8081       │  │  :8082       │      │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘      │
│         │                  │                  │              │
│         └──────────────────┼──────────────────┘              │
│                            │                                 │
│  Firestore Emulator        │  Prometheus                     │
│  (Event Store) :8081       │  (Metrics)  :9090              │
│         │                  │                                 │
└────────────────────────────┼─────────────────────────────────┘
                             │
                    ┌────────┴─────────┐
                    │  172.25.0.0/16   │
                    │   ggen-staging   │
                    └──────────────────┘
```

## Quick Start (5 Steps)

### 1. Prerequisites
```bash
docker --version        # Docker 20.10+
docker-compose --version  # Docker Compose 2.0+
```

### 2. Clone/Navigate to Project
```bash
cd /Users/sac/ggen
```

### 3. Start Deployment
```bash
./deploy/staging/start.sh
```

The script will:
- ✓ Build multi-stage Docker image
- ✓ Start 3-node cluster
- ✓ Wait for all health checks
- ✓ Verify network connectivity
- ✓ Display dashboard URLs

### 4. Verify Cluster Status
```bash
# View running containers
docker-compose -f deploy/staging/docker-compose.yml ps

# Expected output (all "healthy"):
# NAME              STATUS
# ggen-node-1       healthy
# ggen-node-2       healthy
# ggen-node-3       healthy
# firestore-emulator healthy
# prometheus        healthy
```

### 5. Access Dashboards
```bash
# Prometheus metrics
open http://localhost:9090

# ggen node health
curl http://localhost:8080/health
curl http://localhost:8081/health
curl http://localhost:8082/health
```

---

## How to Monitor

### Prometheus Dashboard
```
http://localhost:9090
```

**Key Metrics:**
- `ggen_request_duration_seconds` - Request latency per node
- `ggen_errors_total` - Error counts
- `ggen_active_generations` - Concurrent operations
- `firestore_operation_latency` - Event store performance

**Example Queries:**
```promql
# Request rate per node
rate(ggen_request_duration_seconds_count[1m])

# Error rate
rate(ggen_errors_total[5m])

# Cluster health (all nodes responding)
count(up{job=~"ggen-node.*"} == 1)
```

### Logs Streaming
```bash
# All services
docker-compose -f deploy/staging/docker-compose.yml logs -f

# Specific node
docker logs -f ggen-node-1

# Firestore emulator
docker logs -f firestore-emulator

# Prometheus
docker logs -f prometheus
```

### Health Endpoints
```bash
# Node-1 health (port 8080)
curl -s http://localhost:8080/health | jq .

# Node-2 health (port 8081)
curl -s http://localhost:8081/health | jq .

# Node-3 health (port 8082)
curl -s http://localhost:8082/health | jq .
```

### Metrics Endpoint
```bash
# Prometheus format metrics (port 8080)
curl -s http://localhost:8080/metrics
```

---

## How to Kill a Node (Chaos Testing)

### Immediate Termination
```bash
# Kill node-1 (container stops immediately)
docker-compose -f deploy/staging/docker-compose.yml kill ggen-node-1

# Verify it's down
docker-compose -f deploy/staging/docker-compose.yml ps ggen-node-1
# Status should show: exited

# Resurrect it
docker-compose -f deploy/staging/docker-compose.yml up -d ggen-node-1
```

### Graceful Pause (Simulate Network Partition)
```bash
# Pause node-2 (all processes frozen)
docker pause ggen-node-2

# Watch other nodes detect failure (check logs)
docker logs -f ggen-node-1 | grep -i "node-2\|unreachable"

# Resume
docker unpause ggen-node-2
```

### Network Isolation (Chaos - Simulate Latency)
```bash
# Add 2000ms latency to node-3
docker exec ggen-node-3 sh -c 'tc qdisc add dev eth0 root netem delay 2000ms'

# Verify latency
docker exec ggen-node-3 ping -c 1 ggen-node-1

# Remove latency
docker exec ggen-node-3 sh -c 'tc qdisc del dev eth0 root'
```

### Cascading Failure (Resilience Test)
```bash
# Kill node-1 (primary leader)
docker-compose -f deploy/staging/docker-compose.yml kill ggen-node-1

# Watch logs - node-2 should detect failure
docker logs -f ggen-node-2

# Wait 30s, then kill node-2
sleep 30
docker-compose -f deploy/staging/docker-compose.yml kill ggen-node-2

# Node-3 should handle all traffic
docker logs -f ggen-node-3

# Resurrect both
docker-compose -f deploy/staging/docker-compose.yml up -d ggen-node-1 ggen-node-2
```

### Container Memory Pressure
```bash
# Stress node-1 memory to 500MB max
docker update --memory=500m ggen-node-1

# Remove limit
docker update --memory=-1 ggen-node-1
```

### Observe Behavior
```bash
# Watch Prometheus scrape failures in real-time
docker logs -f prometheus | grep -i "error\|timeout"

# Check alerting rules triggered
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].rules[] | select(.state=="firing")'
```

---

## Management Commands

### View Cluster Status
```bash
# Detailed status
docker-compose -f deploy/staging/docker-compose.yml ps

# Show container statistics
docker stats ggen-node-1 ggen-node-2 ggen-node-3

# Show network details
docker network inspect ggen-staging
```

### View Logs
```bash
# Follow all services
docker-compose -f deploy/staging/docker-compose.yml logs -f

# Last 100 lines of node-1
docker logs --tail 100 ggen-node-1

# Last 1 hour
docker logs --since 1h ggen-node-1

# Grep for errors
docker logs ggen-node-1 | grep -i error
```

### Environment Variables
Each node has these environment variables set in `docker-compose.yml`:

```bash
GGEN_NODE_ID=ggen-node-1          # Node identifier
GGEN_CLUSTER_NAME=staging-cluster # Cluster name
GGEN_LOG_LEVEL=info               # Logging level
GGEN_METRICS_ENABLED=true         # Prometheus metrics
GGEN_FIRESTORE_EMULATOR_HOST=firestore-emulator:8081
RUST_LOG=ggen=info,ggen_core=debug
```

To change a variable, update `docker-compose.yml` then:
```bash
docker-compose -f deploy/staging/docker-compose.yml up -d
```

### Persistent Data
- `node-*-data/` volumes store node-local data
- `prometheus-data/` stores time-series metrics
- Remove all with: `docker-compose down -v`

---

## Troubleshooting

### Containers Won't Start
```bash
# Check logs for startup errors
docker logs ggen-node-1

# Common issues:
# - Port already in use: lsof -i :8080
# - Disk space: docker system df
# - Memory: docker stats

# Nuclear option (reset everything)
docker-compose -f deploy/staging/docker-compose.yml down -v --remove-orphans
./deploy/staging/start.sh
```

### Health Check Failures
```bash
# Manually test health endpoint
docker exec ggen-node-1 curl -s http://localhost:8080/health

# If it fails, check service logs
docker logs ggen-node-1 | tail -20

# Restart individual service
docker-compose -f deploy/staging/docker-compose.yml restart ggen-node-1
```

### Prometheus Can't Scrape Nodes
```bash
# Check network connectivity from prometheus container
docker exec prometheus curl -s http://ggen-node-1:8080/metrics | head

# If it fails, verify docker network
docker network inspect ggen-staging

# Check prometheus config
docker exec prometheus cat /etc/prometheus/prometheus.yml
```

### Firestore Emulator Issues
```bash
# Restart emulator
docker-compose -f deploy/staging/docker-compose.yml restart firestore-emulator

# Check port binding
netstat -an | grep 8081

# Reset emulator data
docker-compose -f deploy/staging/docker-compose.yml down -v
docker-compose -f deploy/staging/docker-compose.yml up -d firestore-emulator
```

---

## Shutdown & Cleanup

### Graceful Shutdown (Keep Data)
```bash
docker-compose -f deploy/staging/docker-compose.yml down
```

Data persists in volumes; restart with `up -d` to resume.

### Full Cleanup (Remove Everything)
```bash
docker-compose -f deploy/staging/docker-compose.yml down -v
```

All volumes deleted; next startup is fresh.

### Remove Only Containers (Keep Volumes)
```bash
docker-compose -f deploy/staging/docker-compose.yml down
```

### Remove Dangling Resources
```bash
docker system prune -a --volumes
```

---

## Files Overview

| File | Purpose |
|------|---------|
| `Dockerfile` | Multi-stage build: builder → minimal runtime (200 lines) |
| `docker-compose.yml` | 3-node cluster + Firestore + Prometheus |
| `prometheus.yml` | Metrics scrape config for all nodes |
| `start.sh` | Automated deployment with health checks (300+ lines) |
| `README.md` | This file |

---

## Performance Notes

### Build Time
- First build: ~180s (heavy Rust compilation)
- Cached builds: ~5s (Docker layer cache)
- Use `--no-cache` flag to rebuild: `docker-compose build --no-cache`

### Runtime
- Single node: ~50MB memory
- Full cluster: ~200MB memory + Firestore/Prometheus
- CPU: Minimal during idle (< 5% per node)

### Network
- Inter-node latency: <1ms (localhost bridge)
- Prometheus scrape: 10s interval per node
- Firestore emulator: In-memory, ~1ms operations

---

## Production Considerations

This staging deployment is **production-adjacent** but designed for testing:

**What's Production-Ready:**
- ✓ Multi-stage Docker build (minimal attack surface)
- ✓ Non-root user execution
- ✓ Health checks on all services
- ✓ Prometheus metrics integration
- ✓ Logging to stderr/stdout
- ✓ Clean shutdown handling

**What's Staging-Only:**
- ✗ Firestore emulator (use Cloud Firestore in prod)
- ✗ Single Prometheus instance (use Managed Service in prod)
- ✗ No Kubernetes (this is docker-compose)
- ✗ No persistent storage backend (volumes are ephemeral)

For production, replace:
1. Firestore emulator → Cloud Firestore
2. Local Prometheus → GCP Cloud Monitoring
3. Docker Compose → Kubernetes (use `crates/tai-k8s`)
4. Add TLS/mTLS (use `crates/tai-security`)

---

## Support & Debugging

### View Full Deployment Log
```bash
tail -f deploy/staging/deployment.log
```

### Check Docker Daemon Health
```bash
docker system info
docker system df    # Disk usage
```

### Validate docker-compose.yml
```bash
docker-compose -f deploy/staging/docker-compose.yml config
```

### Test Individual Services
```bash
# Just Firestore
docker-compose -f deploy/staging/docker-compose.yml up firestore-emulator

# Just Prometheus
docker-compose -f deploy/staging/docker-compose.yml up prometheus

# Just one ggen node
docker-compose -f deploy/staging/docker-compose.yml up ggen-node-1
```

---

**Last Updated:** 2026-03-24
**ggen Version:** 6.0.0
**Maintainer:** Sean Chatman
