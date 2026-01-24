# FactoryPaaS Operations Guide

**Audience**: Platform Operators, DevOps Engineers, SREs
**Version**: 1.0.0
**Last Updated**: 2026-01-24

---

## 📋 Table of Contents

1. [Daily Operations](#daily-operations)
2. [Publishing Content](#publishing-content)
3. [Revenue Tracking](#revenue-tracking)
4. [Reading Receipts](#reading-receipts)
5. [Monitoring & Alerting](#monitoring--alerting)
6. [Troubleshooting](#troubleshooting)
7. [Maintenance](#maintenance)
8. [Disaster Recovery](#disaster-recovery)

---

## 🔄 Daily Operations

### Morning Checks (5 Minutes)

```bash
# 1. Check system status
./world/run/status

# Expected output:
# ✓ API server: RUNNING (PID: 12345)
# ✓ Database: HEALTHY (connections: 3/20)
# ✓ Receipts ledger: ACCESSIBLE (1,247 receipts)
# ✓ Monitoring: ACTIVE (12 metrics)

# 2. Review overnight metrics
gcloud monitoring dashboards list --project=$GCP_PROJECT_ID

# 3. Check for alerts
gcloud alpha monitoring policies list --filter="enabled=true"

# 4. Verify receipts integrity
cd world && cargo make verify-receipts
```

### Health Check Endpoints

| Endpoint | Purpose | Expected Response |
|----------|---------|-------------------|
| `/health` | Liveness probe | `{"status": "ok"}` |
| `/metrics` | Prometheus metrics | Text format, 47 metrics |
| `/receipts/count` | Receipt count | `{"count": 1247}` |

---

## 📝 Publishing Content

### Manual Publishing (AI-Assisted)

```bash
# Generate AI-optimized affiliate content
curl -X POST https://api.factorypaas.example.com/api/v1/publish \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "topic": "Best Password Managers 2026",
    "offers": ["offer-1password-789", "offer-bitwarden-012"],
    "seo_keywords": ["password manager", "security", "encryption"],
    "auto_publish": true,
    "target_word_count": 2000
  }'
```

**Response**:
```json
{
  "page_id": "550e8400-e29b-41d4-a716-446655440000",
  "url": "https://content.factorypaas.example.com/best-password-managers-2026",
  "status": "published",
  "seo_score": 92,
  "estimated_clicks_month": 8500,
  "receipt_id": "rcpt_abc123xyz"
}
```

### Batch Publishing (CSV Import)

```bash
# Upload CSV with topics
curl -X POST https://api.factorypaas.example.com/api/v1/publish/batch \
  -H "Authorization: Bearer $API_KEY" \
  -F "file=@topics.csv" \
  -F "auto_publish=true"
```

**CSV format**:
```csv
topic,offers,seo_keywords,target_word_count
Best VPN 2026,offer-vpn-123|offer-vpn-456,vpn|privacy|security,2500
Top Antivirus Software,offer-av-789|offer-av-012,antivirus|malware,1800
```

### Publishing Workflow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Topic Input (manual or CSV)                             │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ 2. AI Content Generation (GPT-4)                           │
│    - SEO keyword optimization                               │
│    - Offer matching and integration                         │
│    - Image generation (DALL-E 3)                            │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ 3. SEO Scoring (0-100)                                      │
│    - Keyword density: ✓                                     │
│    - Readability: ✓                                         │
│    - Internal links: ✓                                      │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ 4. Auto-Publish (or manual review)                         │
│    - CDN deployment                                         │
│    - Sitemap update                                         │
│    - Social media posting                                   │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ 5. Receipt Generation                                       │
│    - Cryptographic signature                                │
│    - Append to receipts ledger                              │
└─────────────────────────────────────────────────────────────┘
```

---

## 💰 Revenue Tracking

### Dashboard Access

```bash
# Open monitoring dashboard
gcloud monitoring dashboards describe factorypaas-revenue \
  --project=$GCP_PROJECT_ID
```

**Key Metrics**:
- **Total Clicks**: Real-time click count
- **Attributed Conversions**: Clicks leading to sales
- **Revenue**: Total revenue (all publishers)
- **Payout Liability**: Amount owed to publishers

### Publisher Revenue Report

```bash
# Get publisher revenue for current month
curl -X GET "https://api.factorypaas.example.com/api/v1/publishers/$PUBLISHER_ID/revenue?month=2026-01" \
  -H "Authorization: Bearer $API_KEY"
```

**Response**:
```json
{
  "publisher_id": "pub-456",
  "period": "2026-01",
  "clicks": 12500,
  "attributed_clicks": 487,
  "conversions": 43,
  "gross_revenue": "$2,150.00",
  "platform_fee": "$645.00",
  "net_payout": "$1,505.00",
  "receipt_hashes": ["sha256:abc...", "sha256:def..."]
}
```

### Processing Payouts

```bash
# Trigger monthly payout calculation
curl -X POST https://api.factorypaas.example.com/api/v1/payouts/calculate \
  -H "Authorization: Bearer $ADMIN_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "period_start": "2026-01-01T00:00:00Z",
    "period_end": "2026-01-31T23:59:59Z",
    "payout_method": "stripe"
  }'
```

**What happens**:
1. Query all attributed conversions in period
2. Calculate publisher payouts (70% revenue share)
3. Generate payout receipts (cryptographically signed)
4. Initiate Stripe transfers
5. Send payout confirmation emails

---

## 📜 Reading Receipts

### Receipt Structure

Every operation produces a cryptographic receipt:

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "type": "ClickRecorded",
  "timestamp": "2026-01-24T12:34:56Z",
  "data": {
    "click_id": "click-123",
    "publisher_id": "pub-456",
    "offer_id": "off-789",
    "ip_hash": "sha256:abc123...",
    "user_agent": "Mozilla/5.0..."
  },
  "signature": "ed25519:def456...",
  "prev_receipt": "550e8400-e29b-41d4-a716-446655440001"
}
```

### Querying Receipts

```bash
# Get recent receipts
curl -X GET "https://api.factorypaas.example.com/api/v1/receipts?limit=10" \
  -H "Authorization: Bearer $API_KEY"

# Get receipts for specific publisher
curl -X GET "https://api.factorypaas.example.com/api/v1/receipts?publisher_id=pub-456" \
  -H "Authorization: Bearer $API_KEY"

# Verify receipt signature
curl -X POST https://api.factorypaas.example.com/api/v1/receipts/verify \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"receipt_id": "550e8400-e29b-41d4-a716-446655440000"}'
```

### Receipt Integrity Check

```bash
# Verify entire receipt chain (Merkle tree)
cd world && cargo make verify-receipts
```

**Expected output**:
```
✓ Verified 1,247 receipts
✓ No signature failures
✓ No hash mismatches
✓ Chain integrity: VALID
✓ Merkle root: sha256:xyz789...
```

---

## 📊 Monitoring & Alerting

### Key Performance Indicators

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| API Latency (p99) | <100ms | >200ms |
| Click Tracking Latency | <50ms | >100ms |
| Error Rate | <0.1% | >1% |
| API Uptime | >99.9% | <99.5% |
| Database Connections | <80% pool | >90% pool |
| Receipt Write Failures | 0 | >0 |

### Alert Channels

```yaml
# Configured in Terraform (world/infra/main.tf)
notification_channels:
  - type: email
    email: ops@factorypaas.example.com
  - type: pagerduty
    integration_key: $PAGERDUTY_KEY
  - type: slack
    webhook_url: $SLACK_WEBHOOK
```

### Custom Dashboards

```bash
# Create custom dashboard
gcloud monitoring dashboards create --config-from-file=dashboard.json
```

**Example dashboard.json**:
```json
{
  "displayName": "Publisher Revenue Dashboard",
  "mosaicLayout": {
    "columns": 12,
    "tiles": [
      {
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Click Rate (last 24h)",
          "xyChart": {
            "dataSets": [{
              "timeSeriesQuery": {
                "timeSeriesFilter": {
                  "filter": "metric.type=\"custom.googleapis.com/factorypaas/clicks\""
                }
              }
            }]
          }
        }
      }
    ]
  }
}
```

---

## 🔧 Troubleshooting

### Issue: API Not Responding

**Symptoms**: `/health` endpoint returns 5xx or times out

**Diagnosis**:
```bash
# Check VM status
gcloud compute instances describe factorypaas-api-vm \
  --zone=$GCP_ZONE

# Check service logs
gcloud logging read "resource.type=gce_instance AND resource.labels.instance_id=INSTANCE_ID" \
  --limit=50 \
  --format=json
```

**Resolution**:
1. Check if VM is running: `gcloud compute instances start factorypaas-api-vm`
2. Check if service is running: `systemctl status factorypaas`
3. Restart service: `systemctl restart factorypaas`
4. If still failing, redeploy: `./world/run/up`

### Issue: Clicks Not Tracking

**Symptoms**: No new receipts generated for clicks

**Diagnosis**:
```bash
# Check receipt bucket permissions
gsutil iam get gs://factorypaas-receipts-prod

# Check recent receipts
gsutil ls -l gs://factorypaas-receipts-prod/ | tail -10

# Check API logs for errors
gcloud logging read "resource.type=gce_instance AND severity>=ERROR" --limit=20
```

**Resolution**:
1. Verify service account has `roles/storage.objectCreator`
2. Check receipt signing key is set: `echo $RECEIPTS_SIGNING_KEY`
3. Verify database connection: `psql $DATABASE_URL -c "SELECT 1;"`

### Issue: Database Connection Pool Exhausted

**Symptoms**: API returns "connection pool exhausted" errors

**Diagnosis**:
```bash
# Check active connections
psql $DATABASE_URL -c "SELECT count(*) FROM pg_stat_activity WHERE state = 'active';"

# Check long-running queries
psql $DATABASE_URL -c "SELECT pid, now() - query_start AS duration, query FROM pg_stat_activity WHERE state = 'active' AND now() - query_start > interval '5 seconds';"
```

**Resolution**:
1. Increase pool size in `.env`: `DATABASE_POOL_SIZE=30`
2. Restart API server: `systemctl restart factorypaas`
3. If problem persists, scale database: edit `ontology/infra.ttl`, set `db_tier = "db-g1-small"`, run `ggen sync`, `terraform apply`

### Issue: Payout Calculation Incorrect

**Symptoms**: Publisher disputes payout amount

**Diagnosis**:
```bash
# Get publisher's receipts for period
curl -X GET "https://api.factorypaas.example.com/api/v1/receipts?publisher_id=$PUB_ID&start=$START&end=$END" \
  -H "Authorization: Bearer $API_KEY" \
  | jq '.receipts | map(select(.type == "AttributionComputed")) | length'

# Recalculate manually
curl -X POST https://api.factorypaas.example.com/api/v1/payouts/recalculate \
  -H "Authorization: Bearer $ADMIN_API_KEY" \
  -d "{\"publisher_id\": \"$PUB_ID\", \"period\": \"2026-01\"}"
```

**Resolution**:
1. Verify attribution window policy in `ontology/policies.ttl`
2. Check for duplicate receipts: `SELECT click_id, COUNT(*) FROM receipts GROUP BY click_id HAVING COUNT(*) > 1;`
3. If policy changed mid-period, regenerate with correct policy

---

## 🛠️ Maintenance

### Routine Maintenance Tasks

| Task | Frequency | Command |
|------|-----------|---------|
| Database backup | Daily (automated) | `gcloud sql export sql factorypaas-postgres gs://backups/$(date +%Y%m%d).sql` |
| Receipt archive | Weekly | `gsutil -m cp -r gs://factorypaas-receipts-prod gs://factorypaas-receipts-archive/$(date +%Y%W)` |
| Log rotation | Daily (automated) | Handled by Cloud Logging |
| Certificate renewal | Quarterly (automated) | Handled by Cloud Load Balancer |

### Database Maintenance

```bash
# Run vacuum analyze (monthly)
psql $DATABASE_URL -c "VACUUM ANALYZE;"

# Check database size
psql $DATABASE_URL -c "SELECT pg_size_pretty(pg_database_size('attribution_db'));"

# Reindex slow queries (quarterly)
psql $DATABASE_URL -c "REINDEX TABLE clicks;"
```

### Infrastructure Updates

```bash
# Update to new VM machine type
vim ontology/infra.ttl  # Change vm_machine_type
ggen sync
cd world/infra && terraform apply

# Update database tier
vim ontology/infra.ttl  # Change db_tier
ggen sync
cd world/infra && terraform apply  # Triggers rolling restart
```

---

## 🚨 Disaster Recovery

### Backup Strategy

**Automated Backups**:
- **Database**: Daily at 03:00 UTC, 7-day retention
- **Receipts**: Versioned Cloud Storage, 7-year retention
- **Infrastructure State**: Terraform state in GCS bucket

### Recovery Procedures

#### Scenario 1: Database Corruption

```bash
# 1. Identify last good backup
gcloud sql backups list --instance=factorypaas-postgres

# 2. Restore from backup
gcloud sql backups restore $BACKUP_ID \
  --backup-instance=factorypaas-postgres \
  --backup-project=$GCP_PROJECT_ID

# 3. Verify data integrity
psql $DATABASE_URL -c "SELECT COUNT(*) FROM clicks;"

# 4. Resume traffic
gcloud compute instance-groups managed set-autoscaling factorypaas-group --max-num-replicas=3
```

**RTO**: 30 minutes
**RPO**: <24 hours

#### Scenario 2: Receipt Ledger Data Loss

```bash
# Receipts are versioned and immutable
# Restore from archive bucket
gsutil -m cp -r gs://factorypaas-receipts-archive/$(date +%Y%W) gs://factorypaas-receipts-prod

# Verify integrity
cd world && cargo make verify-receipts
```

**RTO**: 15 minutes
**RPO**: 0 (append-only, no data loss)

#### Scenario 3: Complete Regional Outage

```bash
# Failover to secondary region (if configured)
gcloud compute instance-groups managed set-target-size factorypaas-group-us-east1 --size=3

# Update DNS to point to failover region
gcloud dns record-sets transaction start --zone=factorypaas-zone
gcloud dns record-sets transaction add $FAILOVER_IP --name=api.factorypaas.example.com --ttl=300 --type=A --zone=factorypaas-zone
gcloud dns record-sets transaction execute --zone=factorypaas-zone
```

**RTO**: 60 minutes (manual failover)
**RPO**: <5 minutes (async replication)

---

## 📞 Support Escalation

### Escalation Path

1. **Level 1**: DevOps Engineer (normal business hours)
   - Restart services
   - Check logs
   - Basic troubleshooting

2. **Level 2**: SRE On-Call (24/7 via PagerDuty)
   - Database issues
   - Infrastructure failures
   - Scaling decisions

3. **Level 3**: Platform Architect (major incidents)
   - Ontology changes
   - Architecture decisions
   - Disaster recovery

### Incident Response

```bash
# Declare incident
curl -X POST https://api.pagerduty.com/incidents \
  -H "Authorization: Token token=$PAGERDUTY_TOKEN" \
  -d '{
    "incident": {
      "type": "incident",
      "title": "FactoryPaaS API Degradation",
      "service": {"id": "$SERVICE_ID", "type": "service_reference"},
      "urgency": "high"
    }
  }'
```

---

## 📋 Runbooks

Quick links to detailed runbooks:

- [Database Failover](runbooks/database-failover.md)
- [Scaling Events](runbooks/scaling.md)
- [Certificate Renewal](runbooks/certificate-renewal.md)
- [Backup Restoration](runbooks/backup-restore.md)

---

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Maintained by**: FactoryPaaS Operations Team
**Contact**: ops@factorypaas.example.com
