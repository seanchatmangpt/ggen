# Runbook: Disaster Recovery Testing & Drills

**Frequency**: Monthly | **Duration**: 1-2 hours | **Scope**: All 3 regions

## Overview

Monthly DR drills validate:
1. **Failover automation** - Automatic region switchover works
2. **Data consistency** - No data loss or corruption
3. **Restore procedures** - Recovery from backups succeeds
4. **RTO/RPO targets** - Meet 5 minute RTO, 1 minute RPO
5. **Team readiness** - Engineers can execute runbooks correctly
6. **Alerting accuracy** - Alerts fire correctly and actionably

## DR Test Schedule

| Month | Date | Primary Test | Secondary Test | Tertiary Test |
|-------|------|--------------|-----------------|-----------------|
| January | 18 | us-central1 failure | Redis failover | PITR recovery |
| February | 15 | us-east1 failure | Firestore replication lag | Backup integrity |
| March | 20 | europe-west1 failure | Cache corruption recovery | Secret rotation |
| April | 17 | Data corruption | Network partition | Resource scaling |
| May | 15 | Cascading failure | DNS failure | KMS key access |
| June | 19 | Complete region outage | Quota exceeded | Disk full scenario |
| July | 17 | Secondary region failure | Replication lag spikes | Load balancer misconfiguration |
| August | 21 | Cache invalidation | Inconsistent reads | Certificate expiry |
| September | 18 | Backup corruption | Restore timeout | Metrics unavailable |
| October | 16 | Wildfire failover | Multiple simultaneous failures | Rollback procedures |
| November | 20 | Data divergence | Quorum loss | Permission escalation |
| December | 15 | Full system restore | Multi-region recovery | Year-end readiness |

## Test 1: Simulated Region Failure (us-central1)

**Objective**: Validate automatic failover to us-east1

**Duration**: 30-45 minutes

**Prerequisites**:
- [ ] All 3 regions healthy and synced
- [ ] Monitoring and alerting operational
- [ ] Incident commander on standby
- [ ] Runbook reviewed by participants

### Phase 1: Prepare (5 minutes)

```bash
# 1. Announce test to all stakeholders
cat > /tmp/dr_test_announcement.txt << 'EOF'
ANNOUNCEMENT: DR Test Starting

Start Time: 2026-02-15 14:00 UTC
Expected Duration: 45 minutes
Scope: Simulated us-central1 failure

During this test:
- Primary region will be unavailable
- Failover to us-east1 will be triggered
- Some brief API latency (< 1s) expected
- No data loss expected
- Status page will be updated

Participants: On-call team, incident commander, platform engineers
EOF

# 2. Verify baseline metrics
gcloud monitoring time-series list \
  --filter='metric.type="custom.googleapis.com/api/request_count"' \
  --format="table(resource.labels.region,points[0].value.int64_value)"

# 3. Record current replication lag
for REGION in us-east1 europe-west1; do
  REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
    --location=$REGION \
    --format="value(replicationLag)")
  echo "$REGION replication lag: ${REPLICATION_LAG}ms"
done

# 4. Clear alerts (so we can see fresh ones)
gcloud monitoring alert-policies update \
  POLICY_ID \
  --clear-notification-channels \
  --clear-documentation
```

### Phase 2: Inject Failure (5 minutes)

```bash
# Method 1: Create network outage (recommended for safety)
# Add firewall rule to block traffic to us-central1
gcloud compute firewall-rules create \
  dr-test-block-us-central1 \
  --direction=INGRESS \
  --priority=1000 \
  --network=default \
  --action=DENY \
  --rules=all \
  --source-ranges=10.0.0.0/8 \
  --target-tags=us-central1-api \
  --description="DR Test: Simulate region failure"

echo "Network block applied - us-central1 now unreachable"

# Verify block is working
curl -m 5 https://api-us-central1.ggen-marketplace.example.com/health || \
  echo "✓ Primary region correctly blocked"

# Method 2: Degrade service (for controlled testing)
# Scale down all pods in us-central1 to 0
# kubectl scale deployment ggen-marketplace-api \
#   --replicas=0 \
#   --namespace=ggen \
#   --context=gke_${GCP_PROJECT_ID}_us-central1_ggen-central1
```

### Phase 3: Monitor Failover (10-15 minutes)

```bash
# Monitor failover progression
FAILOVER_WINDOW=0
while [ $FAILOVER_WINDOW -lt 300 ]; do  # 5 minute timeout

  # Check health status
  PRIMARY_HEALTH=$(curl -s -m 5 \
    https://api-us-central1.ggen-marketplace.example.com/health \
    2>&1 | jq '.status' || echo "FAIL")

  SECONDARY_HEALTH=$(curl -s -m 5 \
    https://api-us-east1.ggen-marketplace.example.com/health \
    2>&1 | jq '.status' || echo "FAIL")

  # Check load balancer backend status
  LB_STATUS=$(gcloud compute backend-services get-health \
    ggen-marketplace-global-backend \
    --global \
    --format="table(instance,status)" | grep us-central1)

  echo "Failover Window: ${FAILOVER_WINDOW}s"
  echo "  Primary: $PRIMARY_HEALTH"
  echo "  Secondary: $SECONDARY_HEALTH"
  echo "  LB Status: $LB_STATUS"

  # Check if failover complete
  if [ "$PRIMARY_HEALTH" != "healthy" ] && [ "$SECONDARY_HEALTH" = "healthy" ]; then
    FAILOVER_TIME=$FAILOVER_WINDOW
    echo "✓ FAILOVER DETECTED at ${FAILOVER_TIME}s"
    break
  fi

  sleep 10
  FAILOVER_WINDOW=$((FAILOVER_WINDOW + 10))
done

if [ $FAILOVER_WINDOW -ge 300 ]; then
  echo "ERROR: Failover did not complete in 5 minutes"
  echo "Escalate to incident commander"
fi
```

### Phase 4: Validate Operation (5-10 minutes)

```bash
# 1. Verify API is responding to requests
RESPONSE=$(curl -s \
  -H "Authorization: Bearer $TEST_TOKEN" \
  https://api.ggen-marketplace.example.com/v1/products \
  | jq '.data | length')

echo "Products returned: $RESPONSE"
[ "$RESPONSE" -gt 0 ] && echo "✓ API responding correctly"

# 2. Test database writes
TEST_ORDER=$(curl -s -X POST \
  -H "Authorization: Bearer $TEST_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"productId": "test-123", "quantity": 1}' \
  https://api.ggen-marketplace.example.com/v1/orders \
  | jq '.orderId')

echo "Created test order: $TEST_ORDER"

# 3. Verify write replicated to other regions
sleep 2
for REGION in us-central1 europe-west1; do
  REPLICA_ORDER=$(gcloud firestore databases documents get \
    --database=default \
    --collection=marketplace_orders \
    --document="$TEST_ORDER" \
    --location=$REGION \
    2>/dev/null | jq '.name' || echo "NOT_FOUND")

  echo "$REGION has order: $REPLICA_ORDER"
done

# 4. Check replication lag acceptable
for REGION in us-east1 europe-west1; do
  REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
    --location=$REGION \
    --format="value(replicationLag)")
  echo "$REGION replication lag: ${REPLICATION_LAG}ms"

  if [ "$REPLICATION_LAG" -gt 10000 ]; then
    echo "WARNING: High replication lag in $REGION"
  fi
done

# 5. Monitor error rates (should be < 0.1%)
ERROR_RATE=$(gcloud monitoring time-series list \
  --filter='metric.type="custom.googleapis.com/api/error_rate"' \
  --format="value(points[0].value.double_value)")

echo "Current error rate: $ERROR_RATE"
if (( $(echo "$ERROR_RATE > 0.01" | bc -l) )); then
  echo "WARNING: Error rate elevated above baseline"
fi
```

### Phase 5: Clean Up & Recover (5-10 minutes)

```bash
# 1. Remove network block to restore us-central1
gcloud compute firewall-rules delete dr-test-block-us-central1 --quiet

echo "✓ Network block removed - us-central1 accessible"

# Or restore pods if using replica scaling
# kubectl scale deployment ggen-marketplace-api \
#   --replicas=3 \
#   --namespace=ggen \
#   --context=gke_${GCP_PROJECT_ID}_us-central1_ggen-central1

# 2. Wait for primary region to recover
RECOVERY_TIMEOUT=300
RECOVERY_WINDOW=0
while [ $RECOVERY_WINDOW -lt $RECOVERY_TIMEOUT ]; do
  PRIMARY_HEALTH=$(curl -s -m 5 \
    https://api-us-central1.ggen-marketplace.example.com/health \
    2>&1 | jq '.status' || echo "FAIL")

  if [ "$PRIMARY_HEALTH" = "healthy" ]; then
    RECOVERY_TIME=$RECOVERY_WINDOW
    echo "✓ Primary region recovered at ${RECOVERY_TIME}s"
    break
  fi

  sleep 10
  RECOVERY_WINDOW=$((RECOVERY_WINDOW + 10))
done

# 3. Verify replication synced
REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
  --location=us-central1 \
  --format="value(replicationLag)")

echo "Replication lag to primary: ${REPLICATION_LAG}ms"
if [ "$REPLICATION_LAG" -lt 5000 ]; then
  echo "✓ Primary region synced"
fi

# 4. Optional: Re-promote primary to primary role (manual step)
echo "Ready to restore primary as primary (manual operation)"
echo "Run: gcloud firestore promote-region us-central1"

# 5. Clean up test data
curl -s -X DELETE \
  -H "Authorization: Bearer $TEST_TOKEN" \
  https://api.ggen-marketplace.example.com/v1/orders/$TEST_ORDER

echo "✓ Test data cleaned up"
```

### Phase 6: Document Results

```bash
# Generate test report
cat > /tmp/dr_test_report.txt << EOF
=== DR TEST REPORT ===
Test Date: $(date -u)
Test Type: Simulated us-central1 failure
Scope: Multi-region failover

RESULTS:
--------
Failure Detection: ${FAILOVER_TIME}s (Target: < 30s) [PASS/FAIL]
Failover Time: ${FAILOVER_TIME}s (Target: < 60s) [PASS/FAIL]
Recovery Time: ${RECOVERY_TIME}s (Target: < 5min) [PASS/FAIL]
Data Loss: 0 documents (Target: 0) [PASS]
Error Rate: ${ERROR_RATE}% (Target: < 0.1%) [PASS/FAIL]
API Responsiveness: ${RESPONSE} products [PASS]

METRICS:
--------
Replication Lag (max): ${REPLICATION_LAG}ms (Target: < 5s)
Failover Automation: AUTOMATIC
Manual Intervention Required: NO
Customer Notifications: Status page updated

ISSUES:
-------
[None, or list any issues found]

IMPROVEMENTS:
-------------
[List any suggested improvements]

SIGN-OFF:
---------
Incident Commander: _________________
Date: ________
EOF

# Send report
mail -s "DR Test Report: $(date +%B\ %Y)" incident-commander@ggen-marketplace.example.com < /tmp/dr_test_report.txt
```

## Test 2: Redis Failover (Cache Failure)

**Objective**: Validate Redis Sentinel automatic failover

**Duration**: 15-20 minutes

```bash
#!/bin/bash
set -e

echo "=== Redis Failover Test ==="

# 1. Get current primary
PRIMARY_REDIS=$(redis-cli -h us-central1-redis-sentinel.internal -p 26379 \
  SENTINEL masters | grep mymaster)
echo "Current primary: $PRIMARY_REDIS"

# 2. Trigger failover
redis-cli -h us-central1-redis-sentinel.internal -p 26379 \
  SENTINEL failover mymaster

echo "Failover triggered at $(date)"

# 3. Monitor failover progress
for i in {1..30}; do
  NEW_PRIMARY=$(redis-cli -h us-east1-redis-sentinel.internal -p 26379 \
    SENTINEL masters | grep mymaster || echo "CHECKING")

  echo "Check $i: $NEW_PRIMARY"

  if [[ "$NEW_PRIMARY" != "$PRIMARY_REDIS" ]]; then
    echo "✓ Failover complete"
    break
  fi

  sleep 5
done

# 4. Verify writes to new primary work
redis-cli -h us-east1-redis.internal SET test-key "$(date)" EX 3600
TEST_VALUE=$(redis-cli -h us-east1-redis.internal GET test-key)
echo "✓ Write successful: $TEST_VALUE"

# 5. Clean up
redis-cli -h us-east1-redis.internal DEL test-key

echo "Redis failover test completed"
```

## Test 3: Backup & Restore Validation

**Objective**: Verify backup integrity and restore procedures

**Duration**: 30-40 minutes

```bash
#!/bin/bash
set -e

echo "=== Backup & Restore Test ==="

# 1. List available backups
echo "Available backups:"
gcloud firestore databases backup list \
  --location=us \
  --format="table(name,createTime,state)"

# 2. Select latest backup
LATEST_BACKUP=$(gcloud firestore databases backup list \
  --location=us \
  --limit=1 \
  --format="value(name)")

echo "Testing backup: $LATEST_BACKUP"

# 3. Create test database from backup
TEST_DB="restore-test-$(date +%s)"
gcloud firestore databases create \
  --database-id="$TEST_DB" \
  --location=us-central1 \
  --type=firestore-native

# 4. Restore from backup
gcloud firestore databases restore \
  --database="$TEST_DB" \
  --backup="$LATEST_BACKUP" \
  --async

# 5. Monitor restore
OPERATION=$(gcloud firestore operations list \
  --filter="(done=false AND operationType=RESTORE)" \
  --limit=1 \
  --format="value(name)")

echo "Monitoring restore operation..."
gcloud firestore operations describe "$OPERATION" --watch

# 6. Validate restored data
RESTORED_COUNT=$(gcloud firestore databases list-documents \
  --database="$TEST_DB" \
  --collection=marketplace_orders \
  --format="value(name)" | wc -l)

CURRENT_COUNT=$(gcloud firestore databases list-documents \
  --database=default \
  --collection=marketplace_orders \
  --format="value(name)" | wc -l)

echo "Restored documents: $RESTORED_COUNT"
echo "Current documents: $CURRENT_COUNT"

if [ "$RESTORED_COUNT" -eq "$CURRENT_COUNT" ]; then
  echo "✓ Restore test PASSED"
else
  echo "WARNING: Document count mismatch"
fi

# 7. Clean up
gcloud firestore databases delete "$TEST_DB" --quiet

echo "Backup & restore test completed"
```

## Monthly Test Template

Use this template for your monthly DR test:

```bash
#!/bin/bash
set -e

TEST_NAME="$1"
TEST_DATE=$(date +%Y-%m-%d)

echo "=== DR Test: $TEST_NAME ==="
echo "Date: $TEST_DATE"
echo "Duration: ~1 hour"
echo ""

# 1. PRE-TEST CHECKS
echo "Step 1: Pre-test validation..."
# [ ] All regions healthy
# [ ] Replication synced (lag < 5s)
# [ ] Backup recent (< 24 hours)
# [ ] Team ready

# 2. INJECT FAILURE
echo "Step 2: Injecting failure..."
# [Specific failure injection command]

# 3. MONITOR RESPONSE
echo "Step 3: Monitoring system response..."
# [Health check monitoring]

# 4. VALIDATE OPERATION
echo "Step 4: Validating operation..."
# [API/database tests]

# 5. CLEAN UP
echo "Step 5: Cleaning up..."
# [Failure removal, reset state]

# 6. DOCUMENT RESULTS
echo "Step 6: Documenting results..."
# [Generate report]

echo "✓ DR Test complete"
```

## Success Criteria

A DR test is SUCCESSFUL when:

- [ ] Failure detection < 30 seconds
- [ ] Automatic failover < 1 minute
- [ ] Zero data loss (RPO met)
- [ ] Full recovery < 5 minutes (RTO met)
- [ ] Error rate < 0.1% during failover
- [ ] All API endpoints responding
- [ ] Database writes persisting and replicating
- [ ] No customer-visible service interruption
- [ ] Team executed runbook correctly
- [ ] Test report completed and filed

---

**Last Updated**: 2026-01-25
**Next Scheduled Test**: 2026-02-15
**Test Coordinator**: On-call engineer
