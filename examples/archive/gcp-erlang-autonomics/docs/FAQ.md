# FAQ: Frequently Asked Questions

Common questions and solutions for GCP Erlang Autonomics.

---

## Table of Contents

1. [General](#general)
2. [Architecture & Design](#architecture--design)
3. [SKUs & Governance](#skus--governance)
4. [Deployment & Operations](#deployment--operations)
5. [Troubleshooting](#troubleshooting)
6. [Cost Management](#cost-management)
7. [Security & Compliance](#security--compliance)
8. [Advanced Topics](#advanced-topics)

---

## General

### Q: What is "autonomic governance"?

**A**: Autonomic governance means cloud infrastructure that self-heals without human intervention. The system:
1. **Monitors** cloud metrics (billing, error rates, latency)
2. **Analyzes** anomalies (detects threshold violations)
3. **Plans** corrective actions (scale, rollback, throttle)
4. **Executes** actions automatically (via GCP APIs)
5. **Logs** every action (cryptographic receipts for audit)

Example: If your cloud bill spikes 50% above baseline, the system automatically throttles scaling to reduce costs—without waking up an engineer at 2 AM.

### Q: What's the difference between this and traditional autoscaling?

**A**: Traditional autoscaling (Cloud Run, GKE) responds to load. Autonomic governance responds to *any* anomaly: costs, errors, resource deadlock, traffic patterns. It's broader, more semantic, and creates immutable audit trails.

| Feature | Cloud Run Autoscaling | Autonomic Governance |
|---------|----------------------|----------------------|
| **Triggers** | Load only | Multiple signals (cost, error, etc) |
| **Actions** | Scale only | Scale, rollback, throttle, alert |
| **FSM** | No | Yes (stateful decision logic) |
| **Audit Trail** | Limited | Cryptographic receipts (immutable) |
| **Customizable** | Hard | Easy (RDF ontology) |

### Q: Can I use this without GCP?

**A**: The architecture is cloud-agnostic, but current implementation targets GCP (Cloud Run, Pub/Sub, Firestore, BigQuery). Porting to AWS/Azure would require:
- Swap Cloud Run → Lambda/App Service
- Swap Pub/Sub → SNS/SQS or Event Grid
- Swap Firestore → DynamoDB/Cosmos DB
- Swap BigQuery → Athena/Synapse

Community contributions welcome!

### Q: Is this production-ready?

**A**: Yes, if you:
- Follow the [GCP_SETUP.md](GCP_SETUP.md) security guidelines (IAM, Secret Manager)
- Enable monitoring + alerting
- Have runbooks for common failure modes
- Test thoroughly in staging before prod

Not recommended if you:
- Need sub-100ms latency (we target <10s for signal→receipt)
- Require on-premises only (GCP is mandatory)
- Can't afford cloud infrastructure costs

---

## Architecture & Design

### Q: Why is it called "Erlang" autonomics?

**A**: Erlang (the programming language) pioneered "Let It Crash" and autonomous recovery. While we don't use Erlang here, we borrowed the philosophy:
1. **Crash gracefully**: Signal processing tolerates failures via Pub/Sub retries
2. **Supervisor trees**: FSM governors supervise cloud services
3. **Hot code reloading**: Update RDF ontologies without redeploying services
4. **Distribution**: Services are independent microservices on Cloud Run

### Q: What is a "receipt" and why do I need one?

**A**: A receipt is cryptographic proof that an action executed:
```json
{
  "execution_id": "exec-uuid-9999",
  "action": "scale_cloud_run",
  "result": "success",
  "receipt_hash": "sha256:abc123...",
  "timestamp": "2026-01-25T10:30:47Z"
}
```

**Why**:
- **Compliance**: Audit trail for regulations (SOC2, HIPAA, PCI-DSS)
- **Forensics**: Understand what happened when incidents occur
- **Tamper detection**: SHA256 prevents receipt forgery
- **Accountability**: Prove the system acted correctly

### Q: How does MAPE-K relate to this?

**A**: MAPE-K (Monitor-Analyze-Plan-Execute-Knowledge) is an autonomic computing loop. We map it to services:
- **Monitor**: Cloud Monitoring, Cloud Logging → Pub/Sub
- **Analyze**: Signal Ingest Service (dedup, normalize, validate)
- **Plan**: Governor Service (FSM state transitions, SPARQL queries)
- **Execute**: Actuator Service (GCP API calls)
- **Knowledge**: RDF ontologies + Receipt Ledger (BigQuery)

### Q: Why RDF ontologies instead of YAML/JSON?

**A**: RDF enables:
1. **Semantic reasoning** (SPARQL queries derive implicit rules)
2. **Reusability** (ontologies compose, no copy-paste)
3. **Type safety** (SHACL validation pre-flight)
4. **Determinism** (same ontology → identical output always)

YAML/JSON are great for configuration, but they're flat and static. RDF is a knowledge graph.

---

## SKUs & Governance

### Q: How do I add a custom SKU?

**A**: Edit `.specify/ontologies/skus.ttl` to define a new governor:

```turtle
@prefix autonomic: <http://ggen.io/autonomic#> .
@prefix sku: <http://ggen.io/sku#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# New SKU: Database Replication Lag Monitor
autonomic:DatabaseLagGovernor a sku:SKU ;
  rdfs:label "Database Replication Lag Monitor" ;
  sku:tier "P2" ;

  # Signal: replication lag threshold exceeded
  autonomic:triggeredBy [
    a autonomic:Signal ;
    rdfs:label "ReplicationLagSpike" ;
    autonomic:threshold 30000 ;  # 30 seconds (milliseconds)
  ] ;

  # Action: scale read replicas
  autonomic:executes [
    a autonomic:Action ;
    rdfs:label "ScaleReadReplicas" ;
    autonomic:minReplicas 2 ;
    autonomic:maxReplicas 10 ;
  ] ;

  # Cost tier
  sku:costPerMonth "150.00" ;
  sku:minBillingUnit "1h" ;
  .
```

Then regenerate:
```bash
ggen sync --audit true
```

The system automatically picks up the new SKU without code changes!

### Q: What happens if all governors refuse an action?

**A**: This shouldn't happen if FSM logic is sound, but if it does:

1. **Trace the signal** → Does it match a known signal type?
2. **Check preconditions** → Are FSM invariants satisfied?
3. **Review SPARQL queries** → Do they evaluate correctly?
4. **Check manual override** → Can you trigger action manually?

Example: All governors refuse to scale because the service doesn't exist.
```bash
# Verify service exists
gcloud run services describe api --region=us-central1

# If not, create it
gcloud run deploy api --image=gcr.io/my-project/api --region=us-central1
```

### Q: Can I run multiple SKUs simultaneously?

**A**: Yes! Multiple governors (FSMs) can run independently:

```
Cost Circuit Breaker (gov-001) → monitors billing
Deploy Rollback Guard (gov-002) → monitors error rate
Traffic Spike Mitigator (gov-003) → monitors latency
```

Each maintains its own state in Firestore. They coordinate via:
- **Pub/Sub**: Different topics per SKU
- **BigQuery**: Unified receipt ledger
- **RDF**: Shared ontology for consistency

**Note**: If two SKUs want to scale the same service, action ordering matters. Use SKU priorities in ontology:

```turtle
sku:costCircuitBreaker sku:priority "P1" .
sku:trafficSpikeMitigator sku:priority "P2" .
```

---

## Deployment & Operations

### Q: How do I deploy this to production?

**A**: Follow [GCP_SETUP.md](GCP_SETUP.md) exactly. Key steps:

1. **Create GCP project** with billing enabled
2. **Enable required APIs** (Cloud Run, Pub/Sub, Firestore, BigQuery)
3. **Create service account** with minimal IAM roles
4. **Build Docker image** and push to Artifact Registry
5. **Deploy 4 Cloud Run services** (Signal Ingest, Governor, Actuator, Receipt Ledger)
6. **Configure Pub/Sub** topics and subscriptions
7. **Create BigQuery dataset** with receipt_ledger table
8. **Enable monitoring** + alerting
9. **Test end-to-end** before traffic

Estimated time: 30-60 minutes (first time). 5 minutes (subsequent).

### Q: How do I update a SKU without downtime?

**A**: Edit `.specify/ontologies/skus.ttl`, regenerate, and redeploy Cloud Run services:

```bash
# 1. Update ontology
vim .specify/ontologies/skus.ttl

# 2. Regenerate
ggen sync --audit true

# 3. Build new image
docker build -t gcr.io/$GCP_PROJECT/autonomic:v2 .

# 4. Push image
docker push gcr.io/$GCP_PROJECT/autonomic:v2

# 5. Update Cloud Run services (no downtime, gradual rollout)
gcloud run deploy governor-service \
  --image gcr.io/$GCP_PROJECT/autonomic:v2 \
  --no-traffic-split false  # Gradual traffic shift

# 6. Monitor new version
gcloud run logs read governor-service --limit=50
```

The old governor instances finish processing current signals, then graceful shutdown occurs.

### Q: Can I test SKUs without affecting production?

**A**: Yes, use staging environment:

```bash
# Create separate GCP project for staging
export STAGING_PROJECT="my-project-staging"

# Deploy staging infrastructure
gcloud config set project $STAGING_PROJECT
# ... follow GCP_SETUP.md with `-staging` suffix ...

# Send test signals to staging Pub/Sub
gcloud pubsub topics publish cloud-events-topic-staging \
  --message='{"signal":"test","value":200}'

# Monitor staging logs
gcloud run logs read governor-service --region=us-central1

# Verify receipts in staging BigQuery
bq query --project_id=$STAGING_PROJECT \
  'SELECT * FROM autonomic.receipt_ledger ORDER BY timestamp DESC LIMIT 10'
```

### Q: How do I monitor the autonomic system itself?

**A**: Cloud Monitoring dashboards track:

**Service Health**:
- Cloud Run error rate (should be <1%)
- Cloud Run cold start time (should be <1s)
- Cloud Run request latency (p95 <5s)

**Signal Processing**:
- Pub/Sub message age (should be <10s)
- Dead-letter queue size (should be 0)

**Action Execution**:
- Action success rate (should be >99%)
- Action duration (p95 <10s)

**Receipt Ledger**:
- BigQuery insert latency (should be <1s)
- GCS archive write latency (should be <2s)

Set up in [GCP_SETUP.md § Monitoring & Observability](#monitoring--observability).

---

## Troubleshooting

### Q: Signal processing is slow (>30s latency)

**A**: Root cause analysis:

```bash
# 1. Check individual service latencies
gcloud run logs read signal-ingest-service --limit=100 | grep "duration"
gcloud run logs read governor-service --limit=100 | grep "duration"
gcloud run logs read actuator-service --limit=100 | grep "duration"

# 2. Check Pub/Sub lag
gcloud pubsub subscriptions describe signal-ingest-sub
# Look for "ackExpirationPeriod" and "messageRetentionDuration"

# 3. Check BigQuery insert time
bq show --job=true [JOB_ID]

# 4. Profile bottleneck (usually GCP API calls):
# - Cloud Run → GCP APIs are slowest
# - Check service permissions
# - Check quotas: gcloud compute project-info describe --project=$GCP_PROJECT
```

### Q: Receipts are not appearing in BigQuery

**A**: Checklist:

```bash
# 1. Verify table exists
bq show autonomic.receipt_ledger

# 2. Check for errors in Receipt Ledger service logs
gcloud run logs read receipt-ledger-service --limit=50 | grep -i error

# 3. Verify service account permissions
gcloud projects get-iam-policy $GCP_PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount=autonomic-governor@*"
# Should have roles/bigquery.dataEditor

# 4. Test manual insert
bq insert autonomic.receipt_ledger << 'EOF'
{
  "execution_id": "manual-test",
  "timestamp": "2026-01-25T10:30:00Z",
  "action": "test",
  "result": "success",
  "content_hash": "test",
  "receipt_hash": "test",
  "metadata": "{}"
}
EOF

# 5. Check for quota exhaustion
# https://console.cloud.google.com/bigquery/quotas

# 6. Check for network issues
gcloud logging read "resource.type=cloud_function AND jsonPayload.error=~'.*bigquery.*'" --limit=10
```

### Q: Cloud Run service is hitting timeout

**A**: Solutions:

```bash
# 1. Check current timeout (default 300s)
gcloud run services describe governor-service --format='value(spec.timeoutSeconds)'

# 2. Increase timeout
gcloud run services update governor-service --timeout=600

# 3. Optimize code:
#    - Cache SPARQL query results (Redis)
#    - Reduce RDF ontology size (split into multiple files)
#    - Use connection pooling for GCP APIs

# 4. Scale horizontally instead (increase min_instances)
gcloud run services update governor-service --min-instances=2 --max-instances=10
```

### Q: Pub/Sub messages are ending up in dead-letter queue

**A**: Solutions:

```bash
# 1. Check what's in DLQ
gcloud pubsub subscriptions pull dlq-sub --limit=10

# 2. Examine error message
gcloud pubsub subscriptions pull dlq-sub --auto-ack --format=json | jq '.attributes'

# 3. Fix root cause:
#    - Invalid JSON schema? → Update schema
#    - Missing field? → Check signal source
#    - Unknown signal type? → Add to ontology
#    - Permission issue? → Grant IAM roles

# 4. Replay message from DLQ
gcloud pubsub topics publish [TOPIC] \
  --message='[MESSAGE_FROM_DLQ]'
```

---

## Cost Management

### Q: How much will this cost?

**A**: Rough estimates (monthly, us-central1):

| Service | Usage | Cost |
|---------|-------|------|
| Cloud Run | 4 services × 2-5 instances, 1 million requests | $100-300 |
| Pub/Sub | 100k messages/day | $50-150 |
| Firestore | 100k operations/day | $50-150 |
| BigQuery | 1GB analyzed (30 queries/day) | $25-100 |
| Memorystore | 1GB Redis | $30-50 |
| Cloud Storage | 100GB archived (1 year retention) | $10-20 |
| **Total** | | **$265-770** |

**Cost optimization**:
1. **Right-size instances** (start with 512Mi memory)
2. **Use Cloud Run min-instances=0** when possible
3. **Archive old receipts** (>1 year) to cheaper Cold Storage
4. **Batch BigQuery queries** (cheaper than many small queries)
5. **Use regional Redis** (cheaper than multi-region)

### Q: Can I control costs with a SKU?

**A**: Yes! Add cost constraints to ontology:

```turtle
autonomic:costCircuitBreaker
  sku:maxDailySpend "500"  # USD
  sku:maxMonthlySpend "10000"  # USD
  sku:escalationPriority "P1"  # Alert if near limit
  .
```

The governor will throttle service when spending approaches limit.

---

## Security & Compliance

### Q: Is my data secure?

**A**: Yes, if you follow these:

1. **Encrypt in transit**: All Pub/Sub, Cloud Run, BigQuery use HTTPS/TLS
2. **Encrypt at rest**: Enable GCP Cloud KMS for encryption keys
3. **Service account**: Use minimal IAM roles (never give Owner role)
4. **Secrets**: Store in Secret Manager, never in code/config
5. **VPC**: Optional: Use VPC Service Controls to restrict network access

See [GCP_SETUP.md § IAM & Security](#iam--security).

### Q: How do I verify receipts weren't tampered with?

**A**: Check SHA256 hashes:

```bash
# Query receipt from BigQuery
bq query --format=json \
  'SELECT execution_id, receipt_hash, content_hash FROM autonomic.receipt_ledger LIMIT 1' | jq '.[0]'

# Retrieve archived JSON from GCS
gsutil cat gs://autonomic-audit-trail/2026/01/25/exec-uuid-9999.json > receipt.json

# Compute hash
sha256sum receipt.json

# Compare: if hashes match, receipt is authentic
```

### Q: What regulations does this support?

**A**: With proper setup, supports:
- **SOC2**: Audit trails, access logging, incident response
- **HIPAA**: Encryption, access control, audit logs (set HIPAA flag in BigQuery)
- **PCI-DSS**: Immutable logs, change tracking
- **GDPR**: Data retention policies (TTL), deletion audit trail

Contact your compliance team—this is not legal advice!

### Q: How do I delete a receipt?

**A**: **Don't**. Receipts are immutable by design (append-only BigQuery). If you must:

1. Delete from BigQuery (creates audit trail)
2. Delete from GCS archive (immutable, requires service account key)
3. Document reason in compliance system

This is a red flag for auditors—only do if legally required.

---

## Advanced Topics

### Q: Can I use custom ML models for anomaly detection?

**A**: Yes! Extend Governor FSM:

```rust
// In governor-service/src/lib.rs
pub async fn evaluate_with_ml(
    signal: &Signal,
    ml_model: &Model,  // Custom ML model
) -> Result<TransitionDecision> {
    // Get baseline from BigQuery historical data
    let historical = query_historical_data(&signal).await?;

    // Run ML model on historical + current
    let anomaly_score = ml_model.predict(&historical, &signal)?;

    // Use anomaly score in FSM guard condition
    if anomaly_score > 0.8 {
        Ok(TransitionDecision::Trigger {
            from_state: "Nominal",
            to_state: "Alert",
        })
    } else {
        Ok(TransitionDecision::NoTransition)
    }
}
```

Then update `.specify/ontologies/erlang-autonomics.ttl` to include ML-based rules.

### Q: Can I run this on GKE instead of Cloud Run?

**A**: Yes, use Kubernetes manifests from `generated/deployment-gke.yaml`:

```bash
# Create namespace
kubectl create namespace autonomic-system

# Deploy manifests
kubectl apply -f generated/deployment-gke.yaml

# Verify
kubectl get pods -n autonomic-system
```

Benefits:
- More control over resource allocation
- Lower cost for sustained workloads
- Custom networking/storage

Drawbacks:
- Must manage cluster upgrades
- More operational complexity
- Longer cold starts

### Q: How do I integrate with existing monitoring (Datadog, New Relic)?

**A**: Receipt Ledger publishes to BigQuery, which can export to any system:

```bash
# BigQuery can export to:
# - Cloud Logging (forward to any SIEM)
# - Cloud Monitoring (integration with Datadog, New Relic)
# - Pub/Sub (custom consumer)
# - Cloud Storage (daily snapshots)

# Example: Export to Datadog
# Set up BigQuery → Pub/Sub → Datadog agent
```

Or expose metrics via OpenTelemetry:

```rust
// Add to Cloud Run service
use opentelemetry_api::{global, metrics::*};

let meter = global::meter("autonomic-governor");
let counter = meter.u64_counter("actions_executed").build();

counter.add(1, &[]);  // Increments when action executes
```

### Q: Can I run this in multiple regions?

**A**: Yes, architecture supports multi-region:

```bash
# Deploy in us-central1
export GCP_REGION=us-central1
# ... follow GCP_SETUP.md ...

# Deploy in europe-west1
export GCP_REGION=europe-west1
# ... follow GCP_SETUP.md ...

# Unified receipt ledger in BigQuery (multi-region dataset)
bq mk --dataset --location=US autonomic-global
bq cp autonomic.receipt_ledger autonomic-global.receipt_ledger-us-central1
bq cp autonomic.receipt_ledger autonomic-global.receipt_ledger-europe-west1
```

Trade-offs:
- **Advantage**: Disaster recovery, reduced latency
- **Disadvantage**: More GCP projects, higher costs, state synchronization

---

## Getting Help

**Still have questions?**

1. **Read [QUICKSTART.md](QUICKSTART.md)** — 5-minute setup guide
2. **Check [ARCHITECTURE.md](ARCHITECTURE.md)** — Deep design docs
3. **Review [API_REFERENCE.md](API_REFERENCE.md)** — Service endpoints
4. **Inspect generated `.specify/specs/*.ttl`** — Real examples
5. **Run examples**: `cargo run --example cost_circuit_breaker_example`
6. **Check logs**: `gcloud run logs read [SERVICE] --limit=100`
7. **Search [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)**
8. **Ask in [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)**

---

**Last Updated**: January 2026 | **Document Version**: 1.0 | **Status**: Production-Ready ✓
