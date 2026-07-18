# GCP Infrastructure Setup

Complete guide to setting up GCP infrastructure for GCP Erlang Autonomics.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [GCP Project Setup](#gcp-project-setup)
3. [Cloud Run Deployment](#cloud-run-deployment)
4. [Pub/Sub Configuration](#pubsub-configuration)
5. [Firestore Setup](#firestore-setup)
6. [BigQuery Configuration](#bigquery-configuration)
7. [Memorystore (Redis)](#memorystore-redis)
8. [Cloud Storage (Archive)](#cloud-storage-archive)
9. [IAM & Security](#iam--security)
10. [Monitoring & Observability](#monitoring--observability)
11. [Testing Infrastructure](#testing-infrastructure)
12. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Local Tools

```bash
# Verify gcloud CLI (v445.0.0+)
gcloud --version

# Install kubectl (for GKE optional deployment)
gcloud components install kubectl

# Verify Docker
docker --version

# Verify Rust
rustc --version
```

### GCP Requirements

- Active GCP project with billing enabled
- Owner or Editor IAM role (for setup)
- APIs enabled:
  - Cloud Run API
  - Pub/Sub API
  - Firestore API
  - BigQuery API
  - Memorystore API
  - Cloud Storage API

### Cost Estimate (Monthly)

| Service | Estimate | Notes |
|---------|----------|-------|
| Cloud Run | $100-500 | 4 services, 2-10 instances each |
| Pub/Sub | $50-200 | ~100k messages/day |
| Firestore | $50-150 | ~100k operations/day |
| BigQuery | $25-100 | 1TB analyzed queries |
| Memorystore | $30-50 | 1GB Redis instance |
| Cloud Storage | $10-20 | Archive retention (1 year) |
| **Total** | **$265-1,020** | Highly variable |

Enable billing alerts in [Google Cloud Console](https://console.cloud.google.com/billing):
1. Select project
2. Budgets & alerts
3. Create alert at $500/month

---

## GCP Project Setup

### Step 1: Enable Required APIs

```bash
# Set project ID
export GCP_PROJECT_ID="your-gcp-project-id"
gcloud config set project $GCP_PROJECT_ID

# Enable APIs
gcloud services enable \
  run.googleapis.com \
  pubsub.googleapis.com \
  firestore.googleapis.com \
  bigquery.googleapis.com \
  redis.googleapis.com \
  storage.googleapis.com \
  artifactregistry.googleapis.com \
  cloudtrace.googleapis.com \
  logging.googleapis.com \
  monitoring.googleapis.com

# Verify APIs enabled
gcloud services list --enabled | grep -E "run|pubsub|firestore|bigquery|redis"
```

### Step 2: Set Default Region

```bash
# Set default region
export GCP_REGION="us-central1"
gcloud config set compute/region $GCP_REGION

# List available regions
gcloud compute regions list
```

### Step 3: Create Service Account

```bash
# Create service account
gcloud iam service-accounts create autonomic-governor \
  --display-name="Autonomic Governor Service Account" \
  --description="Service account for GCP Erlang Autonomics"

export SA_EMAIL="autonomic-governor@${GCP_PROJECT_ID}.iam.gserviceaccount.com"

# Verify creation
gcloud iam service-accounts list --filter="email:autonomic-governor@"
```

---

## Cloud Run Deployment

### Step 1: Create Artifact Registry Repository

```bash
# Create repository
gcloud artifacts repositories create autonomic \
  --repository-format=docker \
  --location=$GCP_REGION \
  --description="Docker images for autonomic system"

# Configure Docker for Artifact Registry
gcloud auth configure-docker ${GCP_REGION}-docker.pkg.dev

# Verify
gcloud artifacts repositories list
```

### Step 2: Build and Push Images

```bash
# Build image
docker build -t ${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1 .

# Push to Artifact Registry
docker push ${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1

# List images
gcloud artifacts docker images list ${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic
```

### Step 3: Deploy Services

#### Signal Ingest Service

```bash
gcloud run deploy signal-ingest-service \
  --image=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1 \
  --platform=managed \
  --region=$GCP_REGION \
  --memory=512Mi \
  --cpu=1 \
  --max-instances=10 \
  --min-instances=2 \
  --service-account=$SA_EMAIL \
  --set-env-vars="SERVICE_NAME=signal-ingest" \
  --no-allow-unauthenticated \
  --timeout=60 \
  --concurrency=80

# Get service URL
SIGNAL_INGEST_URL=$(gcloud run services describe signal-ingest-service \
  --platform=managed --region=$GCP_REGION --format='value(status.url)')
echo "Signal Ingest URL: $SIGNAL_INGEST_URL"
```

#### Governor Service

```bash
gcloud run deploy governor-service \
  --image=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1 \
  --platform=managed \
  --region=$GCP_REGION \
  --memory=512Mi \
  --cpu=1 \
  --max-instances=5 \
  --min-instances=1 \
  --service-account=$SA_EMAIL \
  --set-env-vars="SERVICE_NAME=governor" \
  --no-allow-unauthenticated \
  --timeout=60 \
  --concurrency=80

GOVERNOR_URL=$(gcloud run services describe governor-service \
  --platform=managed --region=$GCP_REGION --format='value(status.url)')
echo "Governor URL: $GOVERNOR_URL"
```

#### Actuator Service

```bash
gcloud run deploy actuator-service \
  --image=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1 \
  --platform=managed \
  --region=$GCP_REGION \
  --memory=512Mi \
  --cpu=1 \
  --max-instances=5 \
  --min-instances=1 \
  --service-account=$SA_EMAIL \
  --set-env-vars="SERVICE_NAME=actuator" \
  --no-allow-unauthenticated \
  --timeout=60 \
  --concurrency=80

ACTUATOR_URL=$(gcloud run services describe actuator-service \
  --platform=managed --region=$GCP_REGION --format='value(status.url)')
echo "Actuator URL: $ACTUATOR_URL"
```

#### Receipt Ledger Service

```bash
gcloud run deploy receipt-ledger-service \
  --image=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/autonomic/governor:v1 \
  --platform=managed \
  --region=$GCP_REGION \
  --memory=512Mi \
  --cpu=1 \
  --max-instances=2 \
  --min-instances=1 \
  --service-account=$SA_EMAIL \
  --set-env-vars="SERVICE_NAME=receipt-ledger" \
  --no-allow-unauthenticated \
  --timeout=60 \
  --concurrency=80

RECEIPT_URL=$(gcloud run services describe receipt-ledger-service \
  --platform=managed --region=$GCP_REGION --format='value(status.url)')
echo "Receipt Ledger URL: $RECEIPT_URL"
```

### Step 4: Verify Deployments

```bash
# List all Cloud Run services
gcloud run services list --platform=managed --region=$GCP_REGION

# Test service health
for service in signal-ingest-service governor-service actuator-service receipt-ledger-service; do
  url=$(gcloud run services describe $service \
    --platform=managed --region=$GCP_REGION --format='value(status.url)')

  echo "Testing $service..."
  curl -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
    $url/health
done
```

---

## Pub/Sub Configuration

### Step 1: Create Topics

```bash
# Cloud Events input
gcloud pubsub topics create cloud-events-topic \
  --message-retention-duration=7d \
  --labels=system=autonomic

# Validated signals (ingest → governor)
gcloud pubsub topics create validated-signals \
  --message-retention-duration=7d \
  --labels=system=autonomic

# Action plans (governor → actuator)
gcloud pubsub topics create action-plans \
  --message-retention-duration=7d \
  --labels=system=autonomic

# Action results (actuator → ledger)
gcloud pubsub topics create action-results \
  --message-retention-duration=7d \
  --labels=system=autonomic

# Dead-letter queue
gcloud pubsub topics create dead-letter-queue \
  --message-retention-duration=7d \
  --labels=system=autonomic

# List topics
gcloud pubsub topics list --filter="labels.system=autonomic"
```

### Step 2: Create Subscriptions

```bash
# Signal Ingest subscription (cloud-events-topic)
gcloud pubsub subscriptions create signal-ingest-sub \
  --topic=cloud-events-topic \
  --push-endpoint=$SIGNAL_INGEST_URL/signals \
  --push-auth-service-account=$SA_EMAIL \
  --ack-deadline=60 \
  --message-retention-duration=7d

# Governor subscription (validated-signals)
gcloud pubsub subscriptions create governor-sub \
  --topic=validated-signals \
  --push-endpoint=$GOVERNOR_URL/signals \
  --push-auth-service-account=$SA_EMAIL \
  --ack-deadline=60 \
  --message-retention-duration=7d

# Actuator subscription (action-plans)
gcloud pubsub subscriptions create actuator-sub \
  --topic=action-plans \
  --push-endpoint=$ACTUATOR_URL/actions \
  --push-auth-service-account=$SA_EMAIL \
  --ack-deadline=60 \
  --message-retention-duration=7d

# Receipt Ledger subscription (action-results)
gcloud pubsub subscriptions create ledger-sub \
  --topic=action-results \
  --push-endpoint=$RECEIPT_URL/receipts \
  --push-auth-service-account=$SA_EMAIL \
  --ack-deadline=60 \
  --message-retention-duration=7d

# Dead-letter subscription (manual review)
gcloud pubsub subscriptions create dlq-sub \
  --topic=dead-letter-queue \
  --message-retention-duration=7d

# List subscriptions
gcloud pubsub subscriptions list
```

### Step 3: Test Pub/Sub Flow

```bash
# Publish test signal
gcloud pubsub topics publish cloud-events-topic \
  --message='{"source":"test","type":"billing.threshold.exceeded","data":{"metric_value":160,"baseline":100}}'

# Check Signal Ingest logs
gcloud run logs read signal-ingest-service \
  --region=$GCP_REGION \
  --limit=20

# Check for messages in subscriptions
gcloud pubsub subscriptions pull validated-signals --auto-ack --limit=1
gcloud pubsub subscriptions pull action-plans --auto-ack --limit=1
gcloud pubsub subscriptions pull action-results --auto-ack --limit=1
```

---

## Firestore Setup

### Step 1: Create Database

```bash
# Create Firestore database in native mode
gcloud firestore databases create \
  --location=$GCP_REGION \
  --type=firestore-native

# Verify creation
gcloud firestore databases list
```

### Step 2: Create Collections and Indexes

```bash
# Create collections via Firestore console or CLI
# Collections:
#   - governor_state
#   - signal_history
#   - action_history

# Create composite index for efficiency
gcloud firestore indexes create \
  --collection=governor_state \
  --fields=sku:Ascending,timestamp:Descending

gcloud firestore indexes create \
  --collection=signal_history \
  --fields=governor_id:Ascending,timestamp:Descending

gcloud firestore indexes create \
  --collection=action_history \
  --fields=governor_id:Ascending,timestamp:Descending,status:Ascending

# List indexes
gcloud firestore indexes list
```

### Step 3: Set TTL Policies

```bash
# TTL for signal history (30 days)
# Document: signal_history/{doc}
# Field: expires_at (timestamp)

# TTL for action history (60 days)
# Document: action_history/{doc}
# Field: expires_at (timestamp)

# In Firestore console:
# 1. Go to Firestore > Indexes
# 2. Go to TTL Policies tab
# 3. Create TTL for collections
#    - Collection: signal_history
#    - Field: expires_at
#    - Duration: 30 days
```

---

## BigQuery Configuration

### Step 1: Create Dataset

```bash
# Create dataset
bq mk \
  --dataset \
  --location=$GCP_REGION \
  --description="Autonomic system audit trail" \
  autonomic

# Verify
bq ls --dataset-id=$GCP_PROJECT_ID
```

### Step 2: Create Tables

```bash
# Create receipt_ledger table
bq mk --table \
  autonomic.receipt_ledger \
  schema.json

# Or use SQL
bq query --use_legacy_sql=false \
'
CREATE TABLE autonomic.receipt_ledger (
  execution_id STRING NOT NULL,
  timestamp TIMESTAMP NOT NULL,
  signal_id STRING,
  action_id STRING,
  action STRING NOT NULL,
  service STRING,
  change_summary STRING,
  result STRING NOT NULL,
  error_message STRING,
  duration_ms INT64,
  content_hash STRING NOT NULL,
  receipt_hash STRING NOT NULL,
  previous_receipt_hash STRING,
  audit_trail_path STRING,
  verification_status STRING,
  tags ARRAY<STRING>,
  metadata JSON NOT NULL
)
PARTITION BY DATE(timestamp)
CLUSTER BY action, service
'

# Verify table
bq show autonomic.receipt_ledger
bq show --schema --format=prettyjson autonomic.receipt_ledger
```

### Step 3: Create Views for Analytics

```bash
# Success rate view
bq query --use_legacy_sql=false \
'
CREATE OR REPLACE VIEW autonomic.action_success_rate AS
SELECT
  DATE(timestamp) as date,
  action,
  COUNT(*) as total_actions,
  COUNTIF(result = "success") as successful_actions,
  ROUND(COUNTIF(result = "success") / COUNT(*) * 100, 2) as success_rate
FROM autonomic.receipt_ledger
GROUP BY date, action
ORDER BY date DESC, action
'

# Action duration view
bq query --use_legacy_sql=false \
'
CREATE OR REPLACE VIEW autonomic.action_performance AS
SELECT
  action,
  service,
  PERCENTILE_CONT(duration_ms, 0.5) OVER (PARTITION BY action) as median_duration_ms,
  PERCENTILE_CONT(duration_ms, 0.95) OVER (PARTITION BY action) as p95_duration_ms,
  PERCENTILE_CONT(duration_ms, 0.99) OVER (PARTITION BY action) as p99_duration_ms,
  MAX(duration_ms) as max_duration_ms
FROM autonomic.receipt_ledger
'

# Verify views
bq ls --dataset-id=autonomic
```

---

## Memorystore (Redis)

### Step 1: Create Redis Instance

```bash
# Create Redis instance
gcloud redis instances create autonomic-cache \
  --size=1 \
  --region=$GCP_REGION \
  --redis-version=7.0 \
  --tier=basic

# Get connection details
REDIS_HOST=$(gcloud redis instances describe autonomic-cache \
  --region=$GCP_REGION --format='value(host)')
REDIS_PORT=$(gcloud redis instances describe autonomic-cache \
  --region=$GCP_REGION --format='value(port)')

echo "Redis Host: $REDIS_HOST"
echo "Redis Port: $REDIS_PORT"

# Verify
gcloud redis instances list --region=$GCP_REGION
```

### Step 2: Configure in Services

```bash
# Store credentials in Secret Manager
echo -n "$REDIS_HOST:$REDIS_PORT" | gcloud secrets create redis-connection --data-file=-

# Update Cloud Run services with Redis connection
for service in signal-ingest-service governor-service actuator-service receipt-ledger-service; do
  gcloud run services update $service \
    --region=$GCP_REGION \
    --set-secrets=REDIS_CONNECTION=redis-connection:latest
done
```

---

## Cloud Storage (Archive)

### Step 1: Create Bucket

```bash
# Create bucket for audit trail archive
gsutil mb -l $GCP_REGION \
  -b on \
  --default-storage-class=STANDARD \
  gs://autonomic-audit-trail-${GCP_PROJECT_ID}/

# Enable versioning
gsutil versioning set on gs://autonomic-audit-trail-${GCP_PROJECT_ID}/

# Set lifecycle policy (keep 1 year)
cat > lifecycle.json << EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "Delete"},
        "condition": {"age": 365}
      }
    ]
  }
}
EOF

gsutil lifecycle set lifecycle.json gs://autonomic-audit-trail-${GCP_PROJECT_ID}/

# List buckets
gsutil ls
```

### Step 2: Configure Permissions

```bash
# Grant service account permission to write to bucket
gsutil iam ch \
  serviceAccount:$SA_EMAIL:objectCreator,objectViewer \
  gs://autonomic-audit-trail-${GCP_PROJECT_ID}/
```

---

## IAM & Security

### Step 1: Grant Required Roles

```bash
# Cloud Run invoker
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/run.invoker

# Pub/Sub subscriber
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/pubsub.subscriber

# Pub/Sub publisher
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/pubsub.publisher

# Firestore user
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/datastore.user

# BigQuery data editor
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/bigquery.dataEditor

# Redis client
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/redis.client

# Cloud Storage object creator
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/storage.objectCreator

# Compute OS Login (for GKE optional)
gcloud projects add-iam-policy-binding $GCP_PROJECT_ID \
  --member=serviceAccount:$SA_EMAIL \
  --role=roles/compute.osLogin

# View applied roles
gcloud projects get-iam-policy $GCP_PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount=$SA_EMAIL"
```

### Step 2: Create Service Account Keys

```bash
# Create key for local development (if needed)
gcloud iam service-accounts keys create autonomic-governor-key.json \
  --iam-account=$SA_EMAIL

# Set environment variable
export GOOGLE_APPLICATION_CREDENTIALS=$(pwd)/autonomic-governor-key.json

# SECURITY WARNING: Never commit this key to git
echo "autonomic-governor-key.json" >> .gitignore
```

### Step 3: Secret Manager Setup

```bash
# Enable Secret Manager API
gcloud services enable secretmanager.googleapis.com

# Create secrets for sensitive data
gcloud secrets create database-password \
  --replication-policy="automatic"

gcloud secrets create api-key \
  --replication-policy="automatic"

gcloud secrets create oauth-token \
  --replication-policy="automatic"

# Grant service account access to secrets
for secret in database-password api-key oauth-token; do
  gcloud secrets add-iam-policy-binding $secret \
    --member=serviceAccount:$SA_EMAIL \
    --role=roles/secretmanager.secretAccessor
done

# List secrets
gcloud secrets list
```

---

## Monitoring & Observability

### Step 1: Create Dashboard

```bash
# Create monitoring dashboard
gcloud monitoring dashboards create --config-from-file=- << EOF
{
  "displayName": "Autonomic System Dashboard",
  "mosaicLayout": {
    "columns": 12,
    "tiles": [
      {
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Signal Throughput (per minute)",
          "xyChart": {
            "dataSets": [{
              "timeSeriesQuery": {
                "timeSeriesFilter": {
                  "filter": "metric.type=\"run.googleapis.com/request_count\" resource.type=\"cloud_run_revision\" resource.label.service_name=\"signal-ingest-service\"",
                  "aggregation": {
                    "alignmentPeriod": "60s",
                    "perSeriesAligner": "ALIGN_RATE"
                  }
                }
              }
            }]
          }
        }
      }
    ]
  }
}
EOF

# View dashboard
gcloud monitoring dashboards list
```

### Step 2: Create Alerts

```bash
# Alert: Cloud Run error rate > 1%
gcloud alpha monitoring policies create \
  --notification-channels=[CHANNEL_ID] \
  --display-name="Cloud Run Error Rate Alert" \
  --condition-display-name="Error rate > 1%" \
  --condition-threshold-value=0.01 \
  --condition-threshold-comparison=COMPARISON_GT

# Alert: BigQuery insert failures
# (Set up in Cloud Console > Monitoring > Alerting Policies)

# List alerting policies
gcloud alpha monitoring policies list
```

### Step 3: Enable Cloud Logging

```bash
# Logs are automatically sent to Cloud Logging
# View logs via CLI:
gcloud logging read \
  "resource.type=cloud_run_revision AND resource.labels.service_name=signal-ingest-service" \
  --limit=50 \
  --format=json

# Or view in Cloud Console:
# https://console.cloud.google.com/logs/query
```

---

## Testing Infrastructure

### Step 1: Test Signal Flow

```bash
# Publish test signal
gcloud pubsub topics publish cloud-events-topic \
  --message='{"source":"test","type":"cost_spike","data":{"metric_value":160,"baseline":100}}'

# Monitor logs
for service in signal-ingest governor actuator receipt-ledger; do
  echo "=== $service ==="
  gcloud run logs read ${service}-service --region=$GCP_REGION --limit=10
done
```

### Step 2: Test BigQuery Queries

```bash
# Recent receipts
bq query --use_legacy_sql=false \
'
SELECT execution_id, timestamp, action, result, duration_ms
FROM autonomic.receipt_ledger
ORDER BY timestamp DESC
LIMIT 10
'

# Success rate by action
bq query --use_legacy_sql=false \
'
SELECT
  action,
  COUNT(*) as total,
  COUNTIF(result = "success") as successful,
  ROUND(COUNTIF(result = "success") / COUNT(*) * 100, 2) as success_rate
FROM autonomic.receipt_ledger
WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 24 HOUR)
GROUP BY action
ORDER BY success_rate DESC
'
```

### Step 3: Load Testing

```bash
# Use Cloud Load Testing or Apache JMeter
# Example: Send 100 signals per second for 5 minutes

for i in {1..300}; do
  gcloud pubsub topics publish cloud-events-topic \
    --message="{\"source\":\"load-test\",\"type\":\"cost_spike\",\"value\":$((150 + RANDOM % 50))}" &

  if [ $((i % 100)) -eq 0 ]; then
    wait
  fi
done
```

---

## Troubleshooting

### Problem: Cloud Run service timeout

```bash
# Increase timeout (default 300s)
gcloud run services update signal-ingest-service \
  --region=$GCP_REGION \
  --timeout=600

# Check logs for slow operations
gcloud run logs read signal-ingest-service \
  --region=$GCP_REGION \
  --limit=50 | grep "duration\|latency"
```

### Problem: Pub/Sub message not delivered

```bash
# Check subscription status
gcloud pubsub subscriptions describe signal-ingest-sub

# Check dead-letter queue
gcloud pubsub subscriptions pull dlq-sub --limit=10 --auto-ack

# Re-deliver: Seek to timestamp
gcloud pubsub subscriptions seek signal-ingest-sub \
  --time=$(date -u -d '5 minutes ago' +%Y-%m-%dT%H:%M:%SZ)
```

### Problem: BigQuery insert failures

```bash
# Check BigQuery job history
bq ls -j -a -n 50

# Check specific job
bq show -j [JOB_ID]

# Test insert manually
bq insert autonomic.receipt_ledger << EOF
{"execution_id":"test-001","timestamp":"2026-01-25T10:30:00Z","action":"test","result":"success","content_hash":"test","receipt_hash":"test","metadata":"{}"}
EOF
```

### Problem: Redis connection timeout

```bash
# Check Redis instance
gcloud redis instances describe autonomic-cache --region=$GCP_REGION

# Test connection from Cloud Run
# Add health check to Cloud Run service:
# GET /health -> connects to Redis and returns status
```

---

## Cleanup

To destroy all infrastructure and avoid charges:

```bash
# Delete Cloud Run services
for service in signal-ingest-service governor-service actuator-service receipt-ledger-service; do
  gcloud run services delete $service --region=$GCP_REGION --quiet
done

# Delete Pub/Sub topics and subscriptions
for topic in cloud-events-topic validated-signals action-plans action-results dead-letter-queue; do
  gcloud pubsub topics delete $topic --quiet
done

# Delete Firestore database
gcloud firestore databases delete --quiet

# Delete BigQuery dataset
bq rm -r --dataset --quiet autonomic

# Delete Redis instance
gcloud redis instances delete autonomic-cache --region=$GCP_REGION --quiet

# Delete Cloud Storage bucket
gsutil -m rm -r gs://autonomic-audit-trail-${GCP_PROJECT_ID}/

# Delete Artifact Registry repository
gcloud artifacts repositories delete autonomic --location=$GCP_REGION --quiet

# Delete service account (optional)
gcloud iam service-accounts delete $SA_EMAIL --quiet
```

---

**Last Updated**: January 2026 | **GCP Region**: us-central1 | **Status**: Production-Ready ✓
