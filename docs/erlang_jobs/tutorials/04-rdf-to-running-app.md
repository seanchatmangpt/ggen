# Tutorial 4: End-to-End: From RDF Spec to Running Erlang App

**Learning Objective:** Master the complete workflow from RDF ontology design to deployed Erlang application with monitoring.

**Prerequisites:** Completed Tutorials 1-3

**Outcome:** A production-ready Erlang job queue deployed with Docker, monitored with Prometheus and Grafana.

**Time:** ~60 minutes

---

## The Complete ggen Workflow

This tutorial demonstrates the full **holographic factory** metaphor:

```
RDF Ontology (O)  →  μ (5-stage pipeline)  →  Erlang Code (A)
   (substrate)         (transformation)           (projection)
```

**Core equation:** $A = \mu(O)$

Where μ is the deterministic five-stage pipeline:
- **μ₁ (Normalize):** Validate RDF, SHACL conformance
- **μ₂ (Extract):** SPARQL queries, OWL inference
- **μ₃ (Emit):** Tera template rendering
- **μ₄ (Canonicalize):** Deterministic formatting
- **μ₅ (Receipt):** Cryptographic proof generation

---

## Step 1: Design Complete RDF Ontology

Create a comprehensive RDF specification in `.specify/specs/001-job-queue/complete.ttl`:

```turtle
@prefix jobs: <http://ggen.dev/ontology/jobs#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

###############################################################################
# DOMAIN ONTOLOGY: Job Queue System
###############################################################################

jobs:JobQueueSystem a owl:Class ;
    rdfs:label "Job Queue System" ;
    rdfs:comment "Complete production-ready job queue with workers, scheduler, metrics" ;
    jobs:hasComponent jobs:JobQueue ;
    jobs:hasComponent jobs:WorkerPool ;
    jobs:hasComponent jobs:Scheduler ;
    jobs:hasComponent jobs:MetricsCollector .

###############################################################################
# QUEUE COMPONENT
###############################################################################

jobs:JobQueue a rdfs:Class ;
    rdfs:label "Job Queue" ;
    rdfs:comment "Priority-based job queue with NATS backend" ;
    jobs:hasProperty jobs:priority ;
    jobs:hasProperty jobs:domain ;
    jobs:hasProperty jobs:maxRetries ;
    jobs:hasProperty jobs:retryBackoffMs ;
    jobs:hasBackend jobs:NATS ;
    jobs:hasFallbackBackend jobs:RabbitMQ .

jobs:NATS a jobs:Backend ;
    rdfs:label "NATS Backend" ;
    jobs:url "nats://localhost:4222" ;
    jobs:timeout 5000 .

jobs:RabbitMQ a jobs:Backend ;
    rdfs:label "RabbitMQ Fallback Backend" ;
    jobs:url "amqp://guest:guest@localhost:5672" ;
    jobs:durability "persistent" .

###############################################################################
# WORKER POOL COMPONENT
###############################################################################

jobs:WorkerPool a rdfs:Class ;
    rdfs:label "Worker Pool" ;
    jobs:poolSize 10 ;
    jobs:maxOverflow 5 ;
    jobs:pullSize 5 ;
    jobs:processTimeout 30000 ;
    jobs:circuitBreakerThreshold 5 ;
    jobs:circuitBreakerWindowMs 10000 ;
    jobs:circuitBreakerRecoveryMs 30000 .

###############################################################################
# SCHEDULER COMPONENT
###############################################################################

jobs:Scheduler a rdfs:Class ;
    rdfs:label "Job Scheduler" ;
    rdfs:comment "Periodic and delayed job scheduling" ;
    jobs:checkIntervalMs 1000 ;
    jobs:maxScheduledJobs 1000 .

###############################################################################
# METRICS COMPONENT
###############################################################################

jobs:MetricsCollector a rdfs:Class ;
    rdfs:label "Metrics Collector" ;
    rdfs:comment "Prometheus metrics for monitoring" ;
    jobs:hasMetric jobs:JobsPublishedCounter ;
    jobs:hasMetric jobs:JobsProcessedCounter ;
    jobs:hasMetric jobs:JobsFailedCounter ;
    jobs:hasMetric jobs:ProcessingDurationHistogram ;
    jobs:hasMetric jobs:CircuitBreakerStatusGauge .

jobs:JobsPublishedCounter a jobs:Counter ;
    rdfs:label "jobs_published_total" ;
    jobs:help "Total jobs published to queue" .

jobs:JobsProcessedCounter a jobs:Counter ;
    rdfs:label "jobs_processed_total" ;
    jobs:help "Total jobs processed successfully" .

jobs:JobsFailedCounter a jobs:Counter ;
    rdfs:label "jobs_failed_total" ;
    jobs:help "Total jobs that failed processing" .

jobs:ProcessingDurationHistogram a jobs:Histogram ;
    rdfs:label "job_processing_duration_ms" ;
    jobs:help "Job processing duration in milliseconds" ;
    jobs:buckets (10 50 100 500 1000 5000 10000) .

jobs:CircuitBreakerStatusGauge a jobs:Gauge ;
    rdfs:label "circuit_breaker_open" ;
    jobs:help "Circuit breaker status (0=closed, 1=open)" .

###############################################################################
# SHACL VALIDATION
###############################################################################

jobs:JobQueueShape a sh:NodeShape ;
    sh:targetClass jobs:JobQueue ;
    sh:property [
        sh:path jobs:priority ;
        sh:in (jobs:high jobs:normal jobs:low) ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path jobs:domain ;
        sh:datatype xsd:string ;
        sh:minCount 1
    ] ;
    sh:property [
        sh:path jobs:maxRetries ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 10
    ] .

jobs:WorkerPoolShape a sh:NodeShape ;
    sh:targetClass jobs:WorkerPool ;
    sh:property [
        sh:path jobs:poolSize ;
        sh:datatype xsd:integer ;
        sh:minInclusive 1 ;
        sh:maxInclusive 100
    ] ;
    sh:property [
        sh:path jobs:maxOverflow ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 50
    ] .
```

**This RDF encodes:**
- Complete system architecture (queue + workers + scheduler + metrics)
- Configuration defaults (pool size, retries, timeouts)
- Backend infrastructure (NATS primary, RabbitMQ fallback)
- Prometheus metrics definitions
- SHACL validation rules

---

## Step 2: Run Full Generation Pipeline

Execute ggen sync with audit trail:

```bash
ggen sync --audit true

# Expected output:
# [μ₁] Normalizing RDF ontology... ✓ (68ms)
#      - Loaded 156 triples
#      - SHACL validation passed (2 shapes)
#      - Dependency resolution complete
#
# [μ₂] Extracting entities via SPARQL... ✓ (42ms)
#      - Extracted 1 JobQueue
#      - Extracted 1 WorkerPool
#      - Extracted 1 Scheduler
#      - Extracted 5 Metrics
#      - OWL inference materialized 23 implicit triples
#
# [μ₃] Rendering templates (Tera)... ✓ (134ms)
#      - Rendered 12 Erlang modules
#      - Rendered 8 test suites
#      - Rendered 3 config files
#      - Rendered 2 Docker files
#
# [μ₄] Canonicalizing outputs... ✓ (28ms)
#      - Formatted 12 .erl files with erlang formatter
#      - Validated syntax (0 errors, 0 warnings)
#      - Content hashing complete (SHA-256)
#
# [μ₅] Generating receipt... ✓ (12ms)
#      - Execution ID: exec-1738155234-abc123
#      - Manifest hash: sha256:4f3a2b1c...
#      - Ontology hash: sha256:9e8d7c6b...
#      - Generated 23 files (12 code, 8 tests, 3 config)
#      - Audit trail: .ggen/audit/2026-01-29.json
#      - Receipt: .ggen/receipts/exec-1738155234-abc123.json
#
# Generation complete in 284ms.
```

---

## Step 3: Examine Generated Receipt

The receipt provides cryptographic proof of generation. View it:

```bash
cat .ggen/receipts/exec-1738155234-abc123.json | jq
```

```json
{
  "execution_id": "exec-1738155234-abc123",
  "timestamp": "2026-01-29T10:45:34Z",
  "ggen_version": "6.0.0",
  "manifest_hash": "sha256:4f3a2b1c8d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1",
  "ontology_hash": "sha256:9e8d7c6b5a4f3e2d1c0b9a8f7e6d5c4b3a2f1e0d9c8b7a6f5e4d3c2b1a0f9e8",
  "files_generated": [
    {
      "path": "src/my_erlang_jobs_app.erl",
      "hash": "sha256:1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a",
      "size_bytes": 4523
    },
    {
      "path": "src/job_queue.erl",
      "hash": "sha256:2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b",
      "size_bytes": 12847
    }
    // ... 21 more files
  ],
  "sparql_queries_executed": 8,
  "inference_rules_applied": 15,
  "generation_rules_executed": 23,
  "timings": {
    "normalize_ms": 68,
    "extract_ms": 42,
    "emit_ms": 134,
    "canonicalize_ms": 28,
    "receipt_ms": 12,
    "total_ms": 284
  }
}
```

**This receipt proves:**
- Exact RDF ontology used (SHA-256 hash)
- Every file generated (paths + hashes)
- Generation is reproducible (same ontology → same hashes)
- Audit trail for compliance

**Deterministic guarantee:** Run `ggen sync` again with same ontology → identical hashes.

---

## Step 4: Build and Test

Compile and run comprehensive tests:

```bash
# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run all tests (unit + integration + stress)
rebar3 ct

# Expected output:
# Testing my_erlang_jobs.job_queue_SUITE: Starting
# Testing my_erlang_jobs.job_queue_SUITE: test_publish_subscribe ... ok
# Testing my_erlang_jobs.job_queue_SUITE: test_metrics ... ok
# Testing my_erlang_jobs.job_queue_SUITE: test_nats_fallback_to_rabbitmq ... ok
#
# Testing my_erlang_jobs.job_worker_SUITE: Starting
# Testing my_erlang_jobs.job_worker_SUITE: test_worker_crash_and_restart ... ok
# Testing my_erlang_jobs.job_worker_SUITE: test_circuit_breaker_opens ... ok
# Testing my_erlang_jobs.job_worker_SUITE: test_pool_overflow ... ok
#
# Testing my_erlang_jobs.job_retry_SUITE: Starting
# ... (all retry tests)
#
# Testing my_erlang_jobs.stress_test_SUITE: Starting
# Testing my_erlang_jobs.stress_test_SUITE: test_10000_jobs_concurrent ... ok (8234ms)
#
# All 18 tests passed in 42.3 seconds.
```

---

## Step 5: Create Docker Deployment

The generated code includes Dockerfiles. Examine `Dockerfile`:

```dockerfile
FROM erlang:26-alpine AS builder

# Copy application source
WORKDIR /app
COPY . /app

# Fetch deps and build release
RUN rebar3 get-deps
RUN rebar3 as prod release

# Production image
FROM alpine:3.18

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/my_erlang_jobs /app

# Expose Prometheus metrics port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD /app/bin/my_erlang_jobs ping || exit 1

# Run application
CMD ["/app/bin/my_erlang_jobs", "foreground"]
```

Build and run:

```bash
# Build Docker image
docker build -t my-erlang-jobs:latest .

# Run with docker-compose (includes NATS, Prometheus, Grafana)
docker-compose up -d

# Expected services:
# - my_erlang_jobs: Your application
# - nats: NATS messaging backend
# - rabbitmq: RabbitMQ fallback backend
# - prometheus: Metrics collection
# - grafana: Visualization dashboard
```

---

## Step 6: Verify Deployment with Prometheus

Access Prometheus metrics:

```bash
# Prometheus UI
open http://localhost:9090

# Query metrics:
# - jobs_published_total
# - jobs_processed_total
# - job_processing_duration_ms
# - circuit_breaker_open

# Example PromQL query:
rate(jobs_processed_total[1m])  # Jobs per second

histogram_quantile(0.99, job_processing_duration_ms_bucket)  # p99 latency
```

---

## Step 7: Visualize with Grafana

The generated code includes Grafana dashboards:

```bash
# Grafana UI (admin/admin)
open http://localhost:3000

# Pre-configured dashboards:
# 1. Job Queue Overview
#    - Jobs published, processed, failed (counters)
#    - Processing latency (p50, p95, p99)
#    - Worker pool utilization
#
# 2. Worker Health
#    - Circuit breaker status per worker
#    - Crash rate and restart count
#    - Worker pull rate
#
# 3. System Performance
#    - BEAM VM metrics (memory, processes, schedulers)
#    - NATS message rate
#    - Queue depth over time
```

---

## Step 8: Load Test with Stress Suite

Run the generated stress tests:

```bash
rebar3 ct --suite stress_test_SUITE

# Test cases:
# - test_10000_jobs_concurrent: 10,000 jobs across 50 workers
# - test_burst_load: 1,000 jobs/second for 60 seconds
# - test_circuit_breaker_under_load: Force failures and verify recovery
# - test_worker_crash_recovery: Kill workers during processing
```

---

## Step 9: Update RDF and Regenerate

Now demonstrate deterministic regeneration. Edit `.specify/specs/001-job-queue/complete.ttl`:

```turtle
# Change pool size from 10 to 20
jobs:WorkerPool a rdfs:Class ;
    jobs:poolSize 20 ;  # Changed from 10
    jobs:maxOverflow 10 ;  # Changed from 5
```

Regenerate:

```bash
ggen sync --audit true --dry_run false

# Expected output:
# [μ₁] Normalizing RDF ontology... ✓ (71ms)
#      - Detected changes: jobs:poolSize, jobs:maxOverflow
#
# [μ₂] Extracting entities via SPARQL... ✓ (45ms)
#
# [μ₃] Rendering templates (Tera)... ✓ (138ms)
#      - Updated 3 files:
#        src/job_worker_pool.erl (pool_size: 10 → 20)
#        config/sys.config (max_overflow: 5 → 10)
#        test/job_worker_pool_SUITE.erl (assertions updated)
#
# [μ₄] Canonicalizing outputs... ✓ (30ms)
#
# [μ₅] Generating receipt... ✓ (13ms)
#      - Receipt: .ggen/receipts/exec-1738155456-def456.json
#
# Generation complete in 297ms.
```

**Key observation:** Only 3 files changed. ggen detected the delta and updated only affected files.

---

## Step 10: Verify Determinism

Run ggen sync again without changing RDF:

```bash
ggen sync --audit true

# Expected output:
# [μ₁] Normalizing RDF ontology... ✓ (69ms)
#      - No changes detected
#
# [μ₂] Extracting entities via SPARQL... ✓ (43ms)
#
# [μ₃] Rendering templates (Tera)... ✓ (135ms)
#      - 0 files changed (all hashes match previous generation)
#
# [μ₄] Canonicalizing outputs... ✓ (29ms)
#
# [μ₅] Generating receipt... ✓ (12ms)
#      - Receipt: .ggen/receipts/exec-1738155567-ghi789.json
#      - All file hashes match previous receipt (exec-1738155456-def456.json)
#
# Generation complete in 288ms.
# No changes detected. All outputs are deterministic.
```

**This proves determinism:** Same RDF → same code → same hashes, always.

---

## Congratulations!

You've completed the **full ggen workflow**:
- ✅ Designed comprehensive RDF ontology
- ✅ Generated Erlang application via μ₁-μ₅ pipeline
- ✅ Built, tested, and deployed with Docker
- ✅ Monitored with Prometheus and Grafana
- ✅ Verified deterministic generation
- ✅ Updated RDF and regenerated (delta detection)

---

## The Holographic Factory in Action

You've seen the **core equation** in practice:

```
A = μ(O)
```

Where:
- **A (Artifact):** Erlang application code
- **μ (Transformation):** Five-stage deterministic pipeline
- **O (Ontology):** RDF specification

**Key insights:**
1. **RDF is the source of truth:** All code is a projection
2. **Deterministic:** Same ontology → same code (proven by hashes)
3. **Auditable:** Receipts provide cryptographic proof
4. **Composable:** Change ontology, regenerate, deploy
5. **Reproducible:** Version control RDF, not generated code

---

## What's Next?

You've mastered the basics. Explore advanced topics:

**How-To Guides:**
- [How to Add a Custom Job Backend](../howto/01-custom-job-backend.md)
- [How to Implement Rate Limiting](../howto/02-rate-limiting.md)
- [How to Customize Generated Templates](../howto/05-customize-templates.md)

**Explanation Articles:**
- [Why Use RDF for Code Generation](../explanation/01-why-rdf.md)
- [Deterministic Generation Benefits](../explanation/05-deterministic-generation.md)

**Reference Documentation:**
- [RDF Ontology Reference](../reference/02-rdf-ontology.md)
- [CLI Command Reference](../reference/05-cli-reference.md)

---

## Summary

You've learned:
- Complete RDF ontology design for production systems
- Five-stage generation pipeline (μ₁-μ₅) in depth
- Cryptographic receipts and audit trails
- Docker deployment with monitoring
- Deterministic generation verification
- Delta detection and incremental updates

**You're now ready to build production-ready Erlang applications with ggen!**

---

**Generated by ggen v6.0.0 | 2026-01-29**
