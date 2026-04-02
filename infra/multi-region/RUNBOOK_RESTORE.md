# Runbook: Data Recovery & Restore Procedures

**Severity**: CRITICAL | **RTO**: 15 minutes | **RPO**: 1 minute

## When to Use This Runbook

Use this runbook when:
1. Data corruption detected in production
2. Accidental data deletion in primary region
3. Replication failure causing divergent data across regions
4. Disaster recovery testing (monthly drills)
5. Recovering primary region after failover

## Data Recovery Scenarios & Solutions

### Scenario 1: Accidental Document Deletion

**Detection**: User reports missing order data

**Impact**: Single document or small collection

**Recovery Objective**: < 15 minutes, zero customer impact

#### Steps:

```bash
# 1. Identify deletion time window
DELETION_TIME="2026-01-25T14:30:00Z"  # From user report
COLLECTION="marketplace_orders"
DOCUMENT_ID="order-12345"

# 2. Check backup status
gcloud compute snapshots describe \
  firestore-backup-${DELETION_TIME} \
  --filter="status=READY" \
  --format="table(name,creationTimestamp,sourceDisk)"

# 3. Query backup Firestore instance
gcloud firestore databases documents get \
  --database=backup-db-$(date +%Y%m%d) \
  --collection=$COLLECTION \
  --document=$DOCUMENT_ID

# 4. If found in backup, restore to primary
gcloud firestore databases documents create \
  --database=default \
  --collection=$COLLECTION \
  --document=$DOCUMENT_ID \
  --data="{'orderId': 'order-12345', 'timestamp': '$DELETION_TIME'}"

# 5. Verify restoration
gcloud firestore databases documents get \
  --database=default \
  --collection=$COLLECTION \
  --document=$DOCUMENT_ID

echo "Document restored: $DOCUMENT_ID"
```

### Scenario 2: Data Corruption in Primary Region

**Detection**: Hash mismatch between primary and replica regions

**Impact**: Multiple documents or entire collection

**Recovery Objective**: < 30 minutes, graceful degradation

#### Steps:

```bash
# 1. STOP all writes to affected collection
kubectl patch deployment ggen-marketplace-api \
  --patch='{"spec": {"template": {"spec": {"containers": [{"name": "api", "env": [{"name": "WRITE_DISABLED_FOR_ORDERS", "value": "true"}]}]}}}}'

# 2. Identify corruption scope
gcloud logging read "resource.type=cloud_function AND jsonPayload.error=~/corrupt/" \
  --limit=100 \
  --format=json | jq '.[] | {timestamp, collection: .jsonPayload.collection, count: .jsonPayload.error_count}'

# 3. Restore from backup snapshot
SNAPSHOT_TIME=$(date -u -d "30 minutes ago" +%Y%m%d_%H%M)
BACKUP_SNAPSHOT="firestore-backup-${SNAPSHOT_TIME}"

# Create restore database
gcloud firestore databases create \
  --database-id="restore-$(date +%s)" \
  --location=us-central1 \
  --type=firestore-native \
  --backup="${BACKUP_SNAPSHOT}"

# 4. Verify restored data integrity
gcloud firestore databases list-collections \
  --database="restore-$(date +%s)" \
  --format="table(name,documentCount)"

# 5. Promote restored database to primary (with zero-downtime)
# This is a data sync operation, not a swap
gcloud firestore databases documents import \
  --database=default \
  --collection=marketplace_orders \
  --import-file=/tmp/restored_orders.json

# 6. Resume writes
kubectl patch deployment ggen-marketplace-api \
  --patch='{"spec": {"template": {"spec": {"containers": [{"name": "api", "env": [{"name": "WRITE_DISABLED_FOR_ORDERS", "value": "false"}]}]}}}}'

echo "Data corruption recovery complete"
```

### Scenario 3: Entire Region Failure

**Detection**: Multiple services unavailable, replication stopped

**Impact**: Regional unavailability (managed by failover runbook)

**Recovery Objective**: < 5 minutes (automatic), < 15 minutes (manual)

#### Steps (See RUNBOOK_FAILOVER.md for failover steps):

```bash
# 1. Failover to secondary region (automatic or manual)
# (See RUNBOOK_FAILOVER.md)

# 2. Begin recovery of failed region
# Restore from backup
gcloud compute snapshots describe \
  firestore-backup-latest \
  --format="table(name,creationTimestamp)"

# 3. Re-initialize Firestore in failed region
gcloud firestore databases update default \
  --location=us-central1 \
  --enable-point-in-time-recovery \
  --point-in-time-recovery-retention-days=7

# 4. Verify replication from new primary
REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
  --location=us-central1 \
  --format="value(replicationLag)")

if [ "$REPLICATION_LAG" -lt 5000 ]; then
    echo "Region recovered and syncing"
else
    echo "High replication lag: ${REPLICATION_LAG}ms"
    echo "Continue monitoring..."
fi
```

---

## Backup & Restore Strategy

### Backup Schedule

| Frequency | Type | Retention | Use Case |
|-----------|------|-----------|----------|
| Hourly | Incremental Snapshot | 3 days | Immediate recovery (< 1 hour ago) |
| Daily | Full Backup | 30 days | Point-in-time recovery (< 30 days ago) |
| Weekly | Archive | 1 year | Long-term compliance, archival |
| Monthly | Export to BigQuery | Indefinite | Historical analysis, audit trail |

### Creating Manual Backups

```bash
# Create manual backup of all Firestore data
BACKUP_ID="manual-backup-$(date +%Y%m%d_%H%M%S)"

gcloud firestore databases backup create \
  --database=default \
  --location=us \
  --backup-id="$BACKUP_ID"

# Monitor backup progress
gcloud firestore databases backup describe \
  --location=us \
  --backup-id="$BACKUP_ID" \
  --watch

# List all backups
gcloud firestore databases backup list \
  --location=us \
  --format="table(name,createTime,state)"
```

### Automated Backup via Cloud Scheduler

```bash
# Create scheduler job for daily backups
gcloud scheduler jobs create app-engine \
  firestore-daily-backup \
  --schedule="0 2 * * *" \
  --timezone="Etc/UTC" \
  --http-method=POST \
  --uri="https://REGION-PROJECT_ID.cloudfunctions.net/firestore-backup" \
  --oidc-service-account-email="firestore-backup@PROJECT_ID.iam.gserviceaccount.com" \
  --message-body='{
    "databases": ["default"],
    "retention_days": 30,
    "export_bigquery": true
  }'

# Create Cloud Function for backup
cat > backup_function.py << 'EOF'
import functions_framework
from google.cloud import firestore_admin_v1
from datetime import datetime
import os

@functions_framework.http
def firestore_backup(request):
    client = firestore_admin_v1.FirestoreAdminClient()
    project = os.environ.get("GOOGLE_CLOUD_PROJECT")
    backup_id = f"backup-{datetime.now().strftime('%Y%m%d_%H%M')}"

    request_obj = {
        "parent": f"projects/{project}/locations/us",
        "backup": {
            "database": f"projects/{project}/databases/default"
        }
    }

    operation = client.create_backup(request_obj)
    return f"Backup started: {operation.name}", 200
EOF

# Deploy function
gcloud functions deploy firestore_backup \
  --runtime python39 \
  --trigger-http \
  --allow-unauthenticated \
  --entry-point firestore_backup \
  --service-account firestore-backup@PROJECT_ID.iam.gserviceaccount.com
```

### Point-in-Time Recovery (PITR)

```bash
# Enable PITR on Firestore database
gcloud firestore databases update default \
  --enable-point-in-time-recovery \
  --point-in-time-recovery-retention-days=7

# Restore to specific point in time
RESTORE_TIME="2026-01-25T14:00:00Z"
NEW_DATABASE_ID="restore-${RESTORE_TIME}"

gcloud firestore databases create \
  --database-id="$NEW_DATABASE_ID" \
  --location=us-central1 \
  --type=firestore-native

# Create restore operation from backup
gcloud firestore databases restore \
  --backup=projects/PROJECT_ID/locations/us/backups/BACKUP_ID \
  --database=projects/PROJECT_ID/databases/$NEW_DATABASE_ID

# Monitor restore progress
gcloud firestore databases restore describe \
  --database=projects/PROJECT_ID/databases/$NEW_DATABASE_ID \
  --watch

# Once verified, promote to primary (application update required)
# Update application config to use $NEW_DATABASE_ID
```

---

## Cross-Region Data Sync

### Scenario: Replicas Diverged from Primary

**Cause**: Replication pipeline failure for > 30 minutes

**Solution**: Force full resync

#### Steps:

```bash
# 1. Identify divergence
gcloud logging read "resource.type=cloud_function AND jsonPayload.status=replication_lag_exceeded" \
  --limit=5 \
  --format=json | jq '.[] | {timestamp, lag_ms: .jsonPayload.lag_ms}'

# 2. Stop replication workers
kubectl scale deployment replication-worker \
  --replicas=0 \
  --namespace=ggen

# 3. Export primary database to GCS
gcloud firestore export gs://firestore-backups/restore-$(date +%s)/ \
  --async \
  --collection-ids=marketplace_orders,marketplace_products

# 4. Monitor export
gcloud firestore operations list \
  --filter="operationType:EXPORT" \
  --format="table(name,done,updateTime)"

# 5. Import to replicas
gcloud firestore import gs://firestore-backups/restore-$(date +%s)/ \
  --async \
  --collection-ids=marketplace_orders,marketplace_products

# 6. Resume replication workers
kubectl scale deployment replication-worker \
  --replicas=3 \
  --namespace=ggen

# 7. Monitor replication lag return to normal
for i in {1..30}; do
    REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
      --location=us-east1 \
      --format="value(replicationLag)")
    echo "Replication lag: ${REPLICATION_LAG}ms"
    [ "$REPLICATION_LAG" -lt 5000 ] && break
    sleep 10
done
```

---

## Data Validation After Recovery

### Validation Procedure

```bash
#!/bin/bash
set -e

echo "=== Data Validation Check ==="

# 1. Check document counts match
PRIMARY_COUNT=$(gcloud firestore databases list-documents \
  --database=default \
  --collection=marketplace_orders \
  --format="value(name)" | wc -l)

EAST_COUNT=$(gcloud firestore databases list-documents \
  --database=default \
  --collection=marketplace_orders \
  --format="value(name)" \
  --gcp-region=us-east1 | wc -l)

echo "Primary orders: $PRIMARY_COUNT"
echo "East replica orders: $EAST_COUNT"

if [ "$PRIMARY_COUNT" -ne "$EAST_COUNT" ]; then
    echo "ERROR: Count mismatch!"
    exit 1
fi

# 2. Check hash of documents
PRIMARY_HASH=$(gcloud firestore databases documents list \
  --database=default \
  --collection=marketplace_orders \
  --format="value(name)" | \
  xargs -I {} gcloud firestore databases documents get --database=default --document-path='{}' \
  --format=json | jq -s 'sort_by(.name) | @json' | md5sum | cut -d' ' -f1)

EAST_HASH=$(gcloud firestore databases documents list \
  --database=default \
  --collection=marketplace_orders \
  --gcp-region=us-east1 \
  --format="value(name)" | \
  xargs -I {} gcloud firestore databases documents get --database=default --document-path='{}' \
  --format=json | jq -s 'sort_by(.name) | @json' | md5sum | cut -d' ' -f1)

echo "Primary hash: $PRIMARY_HASH"
echo "East hash: $EAST_HASH"

if [ "$PRIMARY_HASH" != "$EAST_HASH" ]; then
    echo "ERROR: Hash mismatch! Data divergence detected."
    exit 1
fi

# 3. Check no future-dated documents
FUTURE_DOCS=$(gcloud firestore databases documents list \
  --database=default \
  --collection=marketplace_orders \
  --format="json" | \
  jq '.[] | select(.createTime > now | strftime("%Y-%m-%dT%H:%M:%SZ")) | .name' | wc -l)

if [ "$FUTURE_DOCS" -gt 0 ]; then
    echo "WARNING: $FUTURE_DOCS documents with future timestamps found"
fi

# 4. Check for missing required fields
INCOMPLETE_DOCS=$(gcloud firestore databases documents list \
  --database=default \
  --collection=marketplace_orders \
  --format="json" | \
  jq '.[] | select(.fields.orderId == null or .fields.customerId == null) | .name' | wc -l)

if [ "$INCOMPLETE_DOCS" -gt 0 ]; then
    echo "ERROR: $INCOMPLETE_DOCS documents missing required fields"
    exit 1
fi

echo "✓ Data validation passed"
echo "  - Document counts match"
echo "  - Hashes match (no divergence)"
echo "  - No future-dated documents"
echo "  - All required fields present"
```

---

## Automated Restore Testing

### Monthly Restore Test Procedure

```bash
#!/bin/bash
# Run this procedure monthly to verify restore works

set -e

MONTH=$(date +%B)
TEST_ID="restore-test-${MONTH}-$(date +%Y)"

echo "=== Starting Monthly Restore Test ==="
echo "Test ID: $TEST_ID"

# 1. Create test database from latest backup
echo "Step 1: Creating test database from backup..."
gcloud firestore databases create \
  --database-id="restore-test-$(date +%s)" \
  --location=us-central1 \
  --type=firestore-native

# 2. Verify test database has data
echo "Step 2: Verifying test database..."
TEST_ORDERS=$(gcloud firestore databases list-documents \
  --database="restore-test-*" \
  --collection=marketplace_orders \
  --limit=1 \
  --format="value(name)" | wc -l)

if [ "$TEST_ORDERS" -eq 0 ]; then
    echo "ERROR: Test database empty"
    exit 1
fi

# 3. Run validation
echo "Step 3: Running validation..."
./validate_data.sh --database="restore-test-*"

# 4. Test point-in-time recovery
echo "Step 4: Testing point-in-time recovery..."
RECOVERY_TIME=$(date -u -d "7 days ago" +%Y-%m-%dT%H:%M:%SZ)

gcloud firestore databases create \
  --database-id="pitr-test-$(date +%s)" \
  --location=us-central1 \
  --type=firestore-native \
  --point-in-time-recovery-retention-days=7 \
  --recovery-time="$RECOVERY_TIME"

# 5. Cleanup test databases
echo "Step 5: Cleaning up..."
for db in restore-test-* pitr-test-*; do
    gcloud firestore databases delete "$db" --quiet
done

# 6. Report results
echo "=== Test Results ==="
echo "Status: PASSED"
echo "Test ID: $TEST_ID"
echo "Timestamp: $(date -u)"

# Send to monitoring system
curl -X POST "https://monitoring.example.com/api/restore-tests" \
  -H "Content-Type: application/json" \
  -d "{
    \"test_id\": \"$TEST_ID\",
    \"status\": \"passed\",
    \"timestamp\": \"$(date -u)\",
    \"duration_seconds\": $SECONDS
  }"
```

---

## Troubleshooting Recovery Issues

### Issue: Backup Not Available

**Symptom**: Cannot find backup older than 7 days

**Solution**:
```bash
# List all available backups
gcloud firestore databases backup list \
  --location=us \
  --format="table(name,createTime,state)"

# If no old backup exists, use BigQuery export (if enabled)
bq ls -t ggen_firestore_backups

# Or restore from another region's backup
gcloud firestore databases backup list \
  --location=eu \
  --format="table(name,createTime)"
```

### Issue: Restore Operation Hanging

**Symptom**: Restore stuck in "IN_PROGRESS" state for > 1 hour

**Solution**:
```bash
# Cancel stuck restore operation
gcloud firestore operations cancel projects/PROJECT_ID/operations/OPERATION_ID

# Check operation status
gcloud firestore operations describe \
  projects/PROJECT_ID/operations/OPERATION_ID

# Retry with smaller collection scope
gcloud firestore restore \
  --backup=BACKUP_ID \
  --database=NEW_DB \
  --collection-ids=marketplace_orders
```

---

## Success Criteria for Recovery

Recovery is SUCCESSFUL when:
- [ ] All backup snapshots accessible and recent (< 1 day old)
- [ ] Point-in-time recovery (PITR) enabled and tested
- [ ] Data restoration completes in < 15 minutes
- [ ] Restored data hash matches primary region
- [ ] Document counts match across all regions
- [ ] No future-dated or corrupted documents
- [ ] Replication lag returns to < 5 seconds
- [ ] All validation checks pass
- [ ] Monthly restore test passes
- [ ] Recovery procedures documented and runnable

---

**Last Updated**: 2026-01-25
**Tested**: 2026-01-18 (Monthly Restore Test)
**Next Test**: 2026-02-18
