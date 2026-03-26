# Staging Deployment Checklist

## Files Created ✓

- [x] **Dockerfile** (63 lines)
  - Multi-stage build (builder → runtime)
  - Alpine 3.19 base (minimal footprint)
  - Health check on port 8080
  - Default command: `health-dashboard`
  - Non-root user (ggen:ggen)

- [x] **docker-compose.yml** (150 lines)
  - 3-node ggen cluster
  - Firestore emulator (event store)
  - Prometheus (metrics)
  - Bridge network (172.25.0.0/16)
  - Health checks on all services
  - Volume persistence

- [x] **prometheus.yml** (65 lines)
  - Scrape config for 3 nodes
  - Firestore emulator scraping
  - 10-15s intervals
  - Instance labels

- [x] **start.sh** (275 lines)
  - 10-step deployment script
  - Full health checks
  - Colored logging output
  - Cleanup on error
  - Exit codes
  - Quick reference guide

- [x] **README.md** (446 lines)
  - Quick start (5 steps)
  - How to monitor
  - How to kill nodes (chaos)
  - Troubleshooting guide
  - Management commands
  - Production considerations

## Validation ✓

- [x] docker-compose.yml is syntactically valid
- [x] Dockerfile is syntactically valid
- [x] start.sh has valid bash syntax
- [x] All files are in `/Users/sac/ggen/deploy/staging/`
- [x] start.sh is executable
- [x] No circular dependencies in compose file
- [x] All required volumes defined
- [x] All required networks defined
- [x] Health checks defined on all services

## Deployment Ready ✓

The staging deployment is ready for testing. To start:

```bash
cd /Users/sac/ggen
./deploy/staging/start.sh
```

Expected output:
- Build Docker image
- Start 3-node cluster
- Wait for Firestore health check
- Wait for Prometheus health check
- Wait for all 3 nodes health checks
- Display success message + quick reference

Access points after deployment:
- Prometheus: http://localhost:9090
- Node-1: http://localhost:8080/health
- Node-2: http://localhost:8081/health
- Node-3: http://localhost:8082/health

## Architecture Summary

```
┌─────────────────────────────────────────────────────────────┐
│         ggen Staging Cluster (docker-compose)               │
├──────────────────┬──────────────────┬──────────────────────┤
│  ggen-node-1     │  ggen-node-2     │  ggen-node-3         │
│  :8080           │  :8081           │  :8082               │
│  [healthy]       │  [healthy]       │  [healthy]           │
└──────────────────┴──────────────────┴──────────────────────┘
                           ↓
              ┌────────────┬────────────┐
              │            │            │
         [Firestore]  [Prometheus]   [Bridge]
         :8081        :9090         172.25.0.0/16
         [healthy]    [healthy]
```

## Test Scenarios

### Scenario 1: Basic Startup
```bash
./deploy/staging/start.sh
```
Expected: All 5 services healthy in ~60-90s

### Scenario 2: Kill Node
```bash
docker-compose -f deploy/staging/docker-compose.yml kill ggen-node-1
```
Expected: Node-1 stops, cluster continues with nodes 2-3

### Scenario 3: Resurrect Node
```bash
docker-compose -f deploy/staging/docker-compose.yml up -d ggen-node-1
```
Expected: Node-1 rejoins cluster

### Scenario 4: View Metrics
```bash
curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | .labels'
```
Expected: All 3 nodes + Firestore + Prometheus listed

### Scenario 5: Logs
```bash
docker-compose -f deploy/staging/docker-compose.yml logs -f ggen-node-1
```
Expected: Real-time logs from node-1

## Performance Expectations

| Metric | Expected |
|--------|----------|
| Build time (first) | ~180s |
| Build time (cached) | ~5s |
| Startup time (all healthy) | ~60-90s |
| Memory per node | ~50MB |
| Total memory (full cluster) | ~200MB |
| Inter-node latency | <1ms |
| Prometheus scrape interval | 10-15s |

## Files Located At

```
/Users/sac/ggen/
└── deploy/
    └── staging/
        ├── Dockerfile          (63 lines)
        ├── docker-compose.yml  (150 lines)
        ├── prometheus.yml      (65 lines)
        ├── start.sh           (275 lines, executable)
        └── README.md          (446 lines)
```

## Status

**Ready for production-adjacent staging testing**

All files created and validated. No errors or warnings. Deployment is ready to launch with `./deploy/staging/start.sh`.

---

**Created:** 2026-03-24
**ggen Version:** 6.0.0
