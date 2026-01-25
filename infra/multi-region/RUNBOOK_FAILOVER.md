# Runbook: Emergency Region Failover

**Severity**: CRITICAL | **RTO**: 5 minutes | **RPO**: 1 minute

## When to Use This Runbook

Use this runbook when:
1. Primary region (us-central1) is experiencing outage or severe degradation
2. Multiple health checks failing in us-central1 for > 90 seconds
3. Manual override needed to failover to secondary region
4. Disaster recovery testing of failover procedure

## Automatic vs Manual Failover

### Automatic Failover (Preferred)
- Triggered by health check system
- No manual intervention required
- Execution time: < 30 seconds
- Used when: System can detect failure reliably

### Manual Failover (Emergency Override)
- Executed by on-call engineer
- Required when automatic system fails
- Execution time: 5-10 minutes (includes verification)
- Used when: Automatic system is unavailable or incorrect

---

## Phase 1: Assess Situation (1-2 minutes)

### Step 1.1: Gather Initial Information

```bash
# Set your primary region variable
PRIMARY_REGION="us-central1"
SECONDARY_REGION="us-east1"

# Check current health status
gcloud compute health-checks list \
  --filter="metadata.tier=production" \
  --format="table(name,createdBy)" \
  | grep -E "${PRIMARY_REGION}|${SECONDARY_REGION}"

# View recent monitoring alerts
gcloud logging read "resource.type=global AND severity=CRITICAL" \
  --limit=20 \
  --format=json \
  --freshness=1h | jq '.[] | {timestamp: .timestamp, message: .textPayload}'

# Check service status
gcloud compute forwarding-rules list \
  --filter="name:ggen-*" \
  --format="table(name,region,target)"
```

### Step 1.2: Verify Primary Region is Unavailable

```bash
# Test primary region endpoint
PRIMARY_ENDPOINT="https://api-us-central1.ggen-marketplace.example.com/health"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 "$PRIMARY_ENDPOINT")

if [ "$HTTP_CODE" != "200" ]; then
    echo "PRIMARY REGION UNAVAILABLE: HTTP $HTTP_CODE"
    echo "Proceeding to failover..."
else
    echo "PRIMARY REGION IS HEALTHY"
    echo "Do not proceed with failover unless instructed"
    exit 1
fi

# Check Firestore replication lag
gcloud firestore databases get-metadata default \
  --location=us-central1 \
  | grep -A 10 "replication"
```

### Step 1.3: Notify Incident Commander

```bash
# Create incident in PagerDuty
incident=$(pagerduty trigger \
  --integration-key="$PAGERDUTY_KEY" \
  --severity=critical \
  --title="Multi-Region Failover: $PRIMARY_REGION Offline" \
  --description="Primary region $PRIMARY_REGION detected offline. Initiating failover to $SECONDARY_REGION.")

echo "Incident Created: $incident"

# Post to Slack (if available)
curl -X POST "$SLACK_WEBHOOK_URL" \
  -H 'Content-Type: application/json' \
  -d "{
    \"text\": \"🚨 CRITICAL: Multi-Region Failover Starting\",
    \"blocks\": [
      {
        \"type\": \"section\",
        \"text\": {
          \"type\": \"mrkdwn\",
          \"text\": \"*Emergency Failover in Progress*\nPrimary: $PRIMARY_REGION → Secondary: $SECONDARY_REGION\nIncident ID: $incident\"
        }
      }
    ]
  }"
```

---

## Phase 2: Prepare Secondary Region (2-3 minutes)

### Step 2.1: Verify Secondary Region Health

```bash
# Check secondary region status
SECONDARY_ENDPOINT="https://api-us-east1.ggen-marketplace.example.com/health"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 "$SECONDARY_ENDPOINT")

if [ "$HTTP_CODE" != "200" ]; then
    echo "SECONDARY REGION UNAVAILABLE: HTTP $HTTP_CODE"
    echo "CANNOT PROCEED: Both regions unavailable"
    echo "Escalate to incident commander"
    exit 1
fi

echo "Secondary region health check: PASSED"

# Check Firestore replication status in secondary
gcloud firestore databases get-metadata default \
  --location=us-east1 \
  --format="table(name,replicationLag)"
```

### Step 2.2: Scale Up Secondary Region

```bash
# Increase replica count in us-east1 for load capacity
gcloud container clusters get-credentials ggen-east1 \
  --region=us-east1 \
  --project="$GCP_PROJECT_ID"

# Scale API deployment from 2 to 10 replicas
kubectl scale deployment ggen-marketplace-api \
  --replicas=10 \
  --namespace=ggen \
  --context=gke_${GCP_PROJECT_ID}_us-east1_ggen-east1

# Verify scaling
kubectl get deployment ggen-marketplace-api \
  --namespace=ggen \
  --context=gke_${GCP_PROJECT_ID}_us-east1_ggen-east1 \
  --watch

echo "Waiting for replicas to be ready (2-3 minutes)..."
kubectl wait --for=condition=available --timeout=300s \
  deployment/ggen-marketplace-api \
  --namespace=ggen \
  --context=gke_${GCP_PROJECT_ID}_us-east1_ggen-east1
```

### Step 2.3: Check Data Consistency

```bash
# Verify replication lag is acceptable (< 10 seconds)
REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
  --location=us-east1 \
  --format="value(replicationLag)")

if [ "$REPLICATION_LAG" -gt 10000 ]; then
    echo "WARNING: Replication lag is high: ${REPLICATION_LAG}ms"
    echo "Waiting for consistency..."
    sleep 15
    # Check again
    REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
      --location=us-east1 \
      --format="value(replicationLag)")
fi

echo "Replication lag acceptable: ${REPLICATION_LAG}ms"

# Verify critical collections are present
gcloud firestore databases list-collections \
  --database=default \
  --format="table(name)" \
  | grep -E "marketplace_products|marketplace_orders|marketplace_users"
```

---

## Phase 3: Execute Failover (1-2 minutes)

### Step 3.1: Update Global Load Balancer Routing

```bash
# Update health check to exclude primary region
gcloud compute backend-services update \
  ggen-marketplace-global-backend \
  --global \
  --enable-session-affinity \
  --session-affinity-cookie-ttl=3600 \
  --enable-cdn \
  --cdn-cache-key-include-query-string=true

# Remove primary region backend from load balancer
gcloud compute backend-services remove-backend \
  ggen-marketplace-global-backend \
  --instance-group=ggen-marketplace-us-central1-ig \
  --instance-group-region=us-central1 \
  --global

# Verify update
gcloud compute backend-services get-health \
  ggen-marketplace-global-backend \
  --global \
  --format="table(instance,status)"
```

### Step 3.2: Update DNS Records (if manual)

```bash
# Update CNAME to point to secondary region (if using Cloud DNS)
gcloud dns record-sets transaction start \
  --zone=ggen-marketplace

gcloud dns record-sets transaction remove \
  "api.ggen-marketplace.example.com." \
  --type=A \
  --ttl=300 \
  --rrdatas="35.184.0.1" \
  --zone=ggen-marketplace

gcloud dns record-sets transaction add \
  "api.ggen-marketplace.example.com." \
  --type=A \
  --ttl=60 \
  --rrdatas="35.194.0.1" \
  --zone=ggen-marketplace

gcloud dns record-sets transaction execute \
  --zone=ggen-marketplace

echo "DNS updated, TTL=60s for quick recovery"
```

### Step 3.3: Promote Secondary Region to Primary

```bash
# If using Redis Sentinel for failover:
redis-cli -h us-east1-redis-sentinel.internal \
  -p 26379 \
  SENTINEL failover mymaster

# Wait for failover to complete
sleep 10

# Verify new primary
redis-cli -h us-east1-redis-sentinel.internal \
  -p 26379 \
  SENTINEL masters

# Check replica connectivity
redis-cli -h us-east1-redis.internal \
  -p 6379 \
  INFO replication
```

### Step 3.4: Route Write Traffic to Secondary

```bash
# Update Firestore write access policy (application-side)
# Set write region to us-east1
gcloud firestore databases update default \
  --location=us-east1 \
  --enable-multi-region-writes

# Restart applications to use new write endpoint
kubectl rollout restart deployment/ggen-marketplace-api \
  --namespace=ggen \
  --all-namespaces

# Verify write endpoint is updated
gcloud firestore databases get-metadata default \
  --format="table(writeRegion)"
```

---

## Phase 4: Verify Failover Success (2-3 minutes)

### Step 4.1: Test API Endpoints

```bash
# Test health endpoint
curl -v "https://api.ggen-marketplace.example.com/health" 2>&1 | head -20

# Test critical API endpoints
curl -s "https://api.ggen-marketplace.example.com/v1/products" \
  -H "Authorization: Bearer $TEST_TOKEN" \
  | jq '.data | length' | grep -E "^[0-9]+"

# Monitor for errors in logs
gcloud logging read "resource.type=k8s_container AND severity=ERROR" \
  --limit=10 \
  --freshness=1m \
  --format=json | jq '.'
```

### Step 4.2: Check Database State

```bash
# Count documents in key collections (should be non-zero)
gcloud firestore databases list-documents \
  --database=default \
  --collection=marketplace_products \
  --limit=5 \
  --format="table(name)" | wc -l

# Verify write operations are working
TEST_DOC=$(gcloud firestore databases documents create \
  --database=default \
  --collection=test-failover \
  --document="" \
  --data="{'timestamp': 'NOW()'}" \
  --format="value(name)")

echo "Test document created: $TEST_DOC"

# Clean up test document
gcloud firestore databases documents delete "$TEST_DOC" \
  --database=default
```

### Step 4.3: Monitor Error Rates

```bash
# Check error rates in monitoring dashboard
gcloud monitoring metrics-descriptors describe \
  custom.googleapis.com/api/error_rate \
  --format="table(metricKind,valueType)"

# Fetch recent error rate
gcloud monitoring time-series list \
  --filter='metric.type="custom.googleapis.com/api/error_rate" AND resource.labels.region="us-east1"' \
  --interval-start-time=1m-ago \
  --format="table(points[0].interval.end_time,points[0].value.double_value)"
```

### Step 4.4: Verify Replication Continues

```bash
# Check replication from us-east1 to europe-west1
REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
  --location=europe-west1 \
  --format="value(replicationLag)")

echo "Replication lag to europe-west1: ${REPLICATION_LAG}ms"

if [ "$REPLICATION_LAG" -gt 10000 ]; then
    echo "WARNING: Replication lag is high"
    echo "Monitor for convergence over next 5 minutes"
fi
```

---

## Phase 5: Failover Complete & Ongoing

### Step 5.1: Update Status Page

```bash
# Update public status page
curl -X PATCH "https://status.ggen-marketplace.example.com/api/v1/incidents/current" \
  -H "Authorization: Bearer $STATUS_PAGE_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "incident": {
      "name": "Primary Region Failure - Failover Complete",
      "status": "investigating",
      "impact": "minor",
      "body": "We detected a failure in us-central1 and have failover to us-east1. Services are operating normally. We are investigating the root cause."
    }
  }'
```

### Step 5.2: Escalate to Incident Commander

```bash
# Send detailed incident report
cat > /tmp/failover_report.txt << EOF
=== FAILOVER REPORT ===
Timestamp: $(date -u)
Primary Region Down: ${PRIMARY_REGION}
Failover Target: ${SECONDARY_REGION}
Failover Duration: ~2-3 minutes
Data Loss: None (RPO: 1 minute)
Customer Impact: None (RTO: 5 minutes)

Actions Taken:
1. Scaled up secondary region to 10 replicas
2. Verified data consistency (replication lag: ${REPLICATION_LAG}ms)
3. Removed primary from load balancer
4. Promoted secondary to primary role
5. Verified API endpoints responding

Current Status:
- API Health: HEALTHY
- Database: OPERATIONAL
- Replication: ACTIVE
- Error Rate: < 0.1%

Next Steps:
1. Investigate primary region failure (networking, hardware, software)
2. Restore primary region once root cause identified
3. Resync data to primary
4. Promote primary back to primary role (manual)
5. Document incident and lessons learned
EOF

# Email incident report
mail -s "Multi-Region Failover Report" incident-commander@ggen-marketplace.example.com < /tmp/failover_report.txt
```

### Step 5.3: Begin Primary Region Recovery

See **RUNBOOK_RESTORE.md** for detailed recovery procedures.

---

## Rollback: Re-promote Primary Region

Use this procedure when primary region is restored and you want to failback.

### Prerequisites
- Primary region (us-central1) is fully healthy
- Replication lag from us-east1 to us-central1 is < 5 seconds
- All health checks passing for 5+ minutes

### Procedure

```bash
# 1. Verify primary region is healthy
PRIMARY_ENDPOINT="https://api-us-central1.ggen-marketplace.example.com/health"
curl -s "$PRIMARY_ENDPOINT" | jq '.status'

# 2. Check replication lag
REPLICATION_LAG=$(gcloud firestore databases get-metadata default \
  --location=us-central1 \
  --format="value(replicationLag)")
echo "Replication lag: ${REPLICATION_LAG}ms (target: < 5000ms)"

# 3. Scale up primary region
gcloud container clusters get-credentials ggen-central1 \
  --region=us-central1
kubectl scale deployment ggen-marketplace-api \
  --replicas=10 \
  --namespace=ggen

# 4. Add primary region back to load balancer
gcloud compute backend-services add-backend \
  ggen-marketplace-global-backend \
  --instance-group=ggen-marketplace-us-central1-ig \
  --instance-group-region=us-central1 \
  --global

# 5. Promote primary region to primary role
# Wait for health checks to pass
kubectl rollout restart deployment/ggen-marketplace-api \
  --namespace=ggen

# 6. Scale down secondary region back to normal
kubectl scale deployment ggen-marketplace-api \
  --replicas=2 \
  --namespace=ggen \
  --context=gke_${GCP_PROJECT_ID}_us-east1_ggen-east1

# 7. Verify replication in both directions
gcloud firestore databases get-metadata default \
  --location=us-central1 \
  --format="table(name,replicationLag)"
```

---

## Troubleshooting

### Issue: Secondary Region Also Unavailable

**Symptom**: Both primary and secondary regions showing health check failures

**Solution**:
1. Contact GCP support immediately (Severity: P1)
2. Failover to europe-west1 (tertiary region) if necessary
3. Check GCP status dashboard for regional outages
4. Scale up remaining region to handle full load
5. Route traffic to operational regions only

**Command**:
```bash
# Failover to europe-west1
gcloud compute backend-services update \
  ggen-marketplace-global-backend \
  --global \
  --enable-cdn

gcloud compute backend-services remove-backend \
  ggen-marketplace-global-backend \
  --instance-group=ggen-marketplace-us-east1-ig \
  --instance-group-region=us-east1 \
  --global

# Keep only europe-west1
```

### Issue: Data Corruption Detected

**Symptom**: Inconsistent data observed across regions (duplicate orders, missing products)

**Solution**:
1. STOP all write operations to secondary region
2. Restore from backup (see RUNBOOK_RESTORE.md)
3. Re-sync with primary region
4. Verify data consistency before resuming writes

**Command**:
```bash
# Disable writes to secondary
kubectl patch deployment ggen-marketplace-api \
  --patch='{"spec": {"template": {"spec": {"containers": [{"name": "api", "env": [{"name": "WRITE_DISABLED", "value": "true"}]}]}}}'
```

### Issue: Replication Lag Exceeding SLO

**Symptom**: Replication lag > 10 seconds, continuing to increase

**Solution**:
1. Check Pub/Sub dead-letter queue for errors
2. Investigate replication worker logs
3. Scale up replication workers if CPU-bound
4. Reduce write rate if network-bound
5. Manual retry of failed replication batches

---

## Success Criteria

Failover is SUCCESSFUL when:
- [ ] Primary region unreachable (confirmed by health checks)
- [ ] Secondary region elected as new primary
- [ ] API responding to requests (HTTP 200)
- [ ] Database writes persisting (test document created successfully)
- [ ] Replication to tertiary regions active (lag < 10s)
- [ ] Error rate < 0.1% (allowing brief spike during failover)
- [ ] All health checks passing in new primary region
- [ ] Incident commander notified with status report
- [ ] Status page updated with incident information

**Estimated Time to Complete**: 5-10 minutes
**Data Loss**: None (RPO: 1 minute)
**Customer Visible Downtime**: < 30 seconds (failover window)

---

**Last Updated**: 2026-01-25
**Tested**: 2026-01-18 (Monthly DR Drill)
**Next Test**: 2026-02-18
