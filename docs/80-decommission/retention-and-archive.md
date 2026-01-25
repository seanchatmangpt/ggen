<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Retention and Archive Policy](#retention-and-archive-policy)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Data Retention Policy](#data-retention-policy)
    - [Receipts (7 Years)](#receipts-7-years)
    - [Logs (2 Years)](#logs-2-years)
    - [Configs (7 Years)](#configs-7-years)
    - [Transient Data (7 Days)](#transient-data-7-days)
  - [Archive Strategy](#archive-strategy)
    - [Archive Bucket Structure](#archive-bucket-structure)
    - [Archive Process](#archive-process)
    - [Archive Verification](#archive-verification)
  - [Compliance Framework](#compliance-framework)
    - [NIST SP 800-53 AU-11 (Audit Information Protection)](#nist-sp-800-53-au-11-audit-information-protection)
    - [SOX (Sarbanes-Oxley) Section 302](#sox-sarbanes-oxley-section-302)
    - [GDPR Article 17 (Right to Be Forgotten)](#gdpr-article-17-right-to-be-forgotten)
    - [California Consumer Privacy Act (CCPA)](#california-consumer-privacy-act-ccpa)
  - [Access Controls](#access-controls)
    - [Authentication](#authentication)
    - [Audit Logging](#audit-logging)
  - [Retrieval Process](#retrieval-process)
    - [Scenario 1: Customer Requests Their Data (GDPR)](#scenario-1-customer-requests-their-data-gdpr)
    - [Scenario 2: Regulatory Audit (SOX)](#scenario-2-regulatory-audit-sox)
    - [Scenario 3: Incident Investigation (Ops)](#scenario-3-incident-investigation-ops)
  - [Deletion Process](#deletion-process)
    - [Automatic Deletion (After Retention Expires)](#automatic-deletion-after-retention-expires)
    - [Manual Deletion (With Approval)](#manual-deletion-with-approval)
  - [Examples](#examples)
    - [Example 1: Archive Size Estimation](#example-1-archive-size-estimation)
    - [Example 2: Retrieval Time Estimate](#example-2-retrieval-time-estimate)
  - [Receipt Contract](#receipt-contract)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Retention and Archive Policy

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

## Table of Contents
1. [Overview](#overview)
2. [Data Retention Policy](#data-retention-policy)
3. [Archive Strategy](#archive-strategy)
4. [Compliance Framework](#compliance-framework)
5. [Access Controls](#access-controls)
6. [Retrieval Process](#retrieval-process)
7. [Deletion Process](#deletion-process)
8. [Examples](#examples)

---

## Overview

**Core Principle**: Minimize data retention while maximizing compliance with regulations (NIST, SOX, GDPR).

**Data Classification**:
| Data Type | Retention | Storage | Access | Purpose |
|-----------|-----------|---------|--------|---------|
| **Receipts** | 7 years | GCS Archive | Audit queries | Legal compliance (SOX AU-11) |
| **Logs** | 2 years | Cloud Logging | Ops debugging | Incident investigation |
| **Configs** | 7 years | GCS Archive | Legal discovery | Regulatory audit |
| **Transient** | 7 days | Firestore | Ops querying | Signal/action queue cleanup |

---

## Data Retention Policy

### Receipts (7 Years)

**What**: All receipts (signal_received, action_completed, permission_denied, etc.).

**Where**:
- **Firestore**: 365 days (TTL + soft delete)
- **GCS Archive**: 7 years (Object Lock)
- **Cloud Logging**: 7 years (retention policy)

**Why**: Sarbanes-Oxley (SOX) audit trail retention requirement (AU-11).

**Timeline**:
```
Day 1: Receipt created in Firestore
  ↓
Day 365: TTL expires, Firestore auto-deletes
  (but still in GCS Archive, untouched)
  ↓
Day 2555: GCS Object Lock retention expires
  Deletion possible (manual, with approval)
  ↓
Day 2920: Auto-delete from GCS (if not on legal hold)
```

**Firestore TTL Configuration**:
```json
{
  "fields": {
    "ttl_timestamp": {
      "fieldValue": "serverTimestamp"
    }
  }
}
```

**Firestore Lifecycle Rule**:
```
Collection: receipts-{sku_id}
TTL Field: expire_at (RFC3339 timestamp)
Behavior: Documents deleted 24 hours after expire_at

Example: Receipt created 2026-01-25
  expire_at = 2027-01-25T00:00:00Z
  Auto-delete scheduled for 2027-01-25T00:00:00Z + 24h
```

**GCS Object Lock Configuration**:
```
Bucket: gs://ggen-archive-prod/
Object Retention:
  Mode: GOVERNANCE
  Duration: 7 years (2555 days)
  Unlock: Possible with legal approval
  Delete: Blocked until retention expires

Example: Archive created 2026-01-25
  Retention Expires: 2033-01-25T23:59:59Z
  Delete Allowed: On or after 2033-01-26
```

### Logs (2 Years)

**What**: Cloud Logging entries (API access, errors, service metrics).

**Where**:
- **Cloud Logging**: 2 years (default retention)
- **GCS Archive**: (optional) older logs backed up as JSONL

**Why**: Operational debugging, incident investigation.

**Timeline**:
```
Day 1: Log entry written
  ↓
Day 730: Cloud Logging auto-deletes
  (can be archived to GCS before deletion)
```

**Log Retention Policy**:
```
gcloud logging buckets update _Default \
  --retention-days=730
```

**Log Query Example** (Firestore access logs):
```
resource.type="cloud_firestore_database"
resource.labels.database_id="(default)"
protoPayload.methodName="google.firestore.v1.Firestore.Write"
protoPayload.request.writes[0].document.name:~"receipts-.*"
```

### Configs (7 Years)

**What**: Customer configurations, entitlements, billing info.

**Where**:
- **Firestore**: 365 days (TTL + soft delete)
- **GCS Archive**: 7 years (Object Lock)

**Why**: Regulatory audit, customer disputes, billing reconciliation.

**Timeline**: Same as Receipts (7 years total).

**Firestore Collections**:
```
- configs-{sku_id}
- entitlements-{account_id}
- billing-{account_id}
```

**GCS Archive Format**:
```json
{
  "type": "config",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "config": {
    "name": "my-instance",
    "region": "us-central1",
    "machine_type": "n1-standard-4"
  },
  "archived_ts": "2026-01-25T14:32:00Z"
}
```

### Transient Data (7 Days)

**What**: Signal/action queues, temporary processing state.

**Where**:
- **Firestore**: 7 days (TTL + soft delete)
- **Pub/Sub**: Auto-acked after processing
- **GCS**: Not archived

**Why**: Operational efficiency, don't clutter storage.

**Timeline**:
```
Day 1: Signal queued
  ↓
Day 7: TTL expires, auto-deleted
  ↓
After deletion: No retrieval possible (not archived)
```

**Firestore TTL Configuration**:
```
Collection: signal_queue-{sku_id}
TTL Field: expire_at
Value: now + 7 days

Collection: action_queue-{sku_id}
TTL Field: expire_at
Value: now + 7 days
```

---

## Archive Strategy

### Archive Bucket Structure

```
gs://ggen-archive-prod/
├── YYYY-MM-DD/
│   ├── {sku_id}/
│   │   ├── manifest.json
│   │   ├── receipts.jsonl.gz
│   │   ├── configs.json.gz
│   │   ├── audit_log.jsonl.gz
│   │   └── checksums.txt
│   └── {sku_id}/
│       └── [similar]
├── 2026-01-25/
│   ├── 550e8400-e29b-41d4-a716-446655440000/
│   │   ├── manifest.json
│   │   ├── receipts.jsonl.gz  (~1GB per SKU)
│   │   ├── configs.json.gz    (~10MB per SKU)
│   │   └── audit_log.jsonl.gz (~500MB per SKU)
│   └── [more SKUs...]
└── 2026-01-26/
    └── [more SKUs...]

Total Estimated Size:
  - 1,000 SKUs decommissioned per year
  - ~1.5 GB per SKU
  - Total: 1.5 TB per year
  - 7-year retention: 10.5 TB
```

### Archive Process

**Trigger**: decommission_export_started receipt

**Steps**:
```
1. Open transaction: START TRANSACTION
2. Query all receipts for {sku_id}
3. Stream to GCS: gs://ggen-archive-prod/{date}/{sku_id}/receipts.jsonl.gz
   - Compress: gzip
   - Format: JSONL (one per line)
   - Compute SHA-256 while streaming
4. Query all configs for {sku_id}
5. Stream to GCS: gs://ggen-archive-prod/{date}/{sku_id}/configs.json.gz
6. Query Cloud Logging for {sku_id}
7. Stream to GCS: gs://ggen-archive-prod/{date}/{sku_id}/audit_log.jsonl.gz
8. Generate manifest.json (sizes, checksums, record counts)
9. Create checksums.txt (list all files + SHA-256)
10. Enable Object Lock on all files (7-year retention)
11. Test readability: Download one file, decompress, verify structure
12. Commit transaction
13. Emit: decommission_export_complete receipt
```

**Pseudo-code**:
```python
async def archive_sku(sku_id: str, date: str) -> Receipt:
    gcs_path = f"gs://ggen-archive-prod/{date}/{sku_id}/"

    # 1. Archive receipts
    receipts_count = 0
    receipts_hash = hashlib.sha256()
    with gcs.open(f"{gcs_path}receipts.jsonl.gz", "wb") as f:
        with gzip.GzipFile(fileobj=f, mode="wb") as gz:
            async for receipt in firestore.collection(f"receipts-{sku_id}").stream():
                line = json.dumps(receipt.to_dict()) + "\n"
                gz.write(line.encode())
                receipts_hash.update(line.encode())
                receipts_count += 1

    # 2. Archive configs
    configs = firestore.collection(f"configs-{sku_id}").document("config").get()
    with gcs.open(f"{gcs_path}configs.json.gz", "wb") as f:
        with gzip.GzipFile(fileobj=f, mode="wb") as gz:
            gz.write(json.dumps(configs.to_dict()).encode())

    # 3. Create manifest
    manifest = {
        "sku_id": sku_id,
        "export_ts": now_rfc3339(),
        "files": [
            {
                "name": "receipts.jsonl.gz",
                "size_bytes": gcs.stat(f"{gcs_path}receipts.jsonl.gz").size,
                "checksum_sha256": receipts_hash.hexdigest(),
                "record_count": receipts_count
            },
            # ... more files
        ],
        "retention_expiration_ts": (now + timedelta(days=2555)).isoformat() + "Z"
    }

    with gcs.open(f"{gcs_path}manifest.json", "wb") as f:
        f.write(json.dumps(manifest, indent=2).encode())

    # 4. Verify readability
    test_file = gcs.open(f"{gcs_path}receipts.jsonl.gz", "rb")
    with gzip.GzipFile(fileobj=test_file) as gz:
        for i, line in enumerate(gz):
            if i == 0:  # Test parse first line
                receipt = json.loads(line.decode())
                assert receipt["kind"] is not None
        assert i + 1 == receipts_count

    # 5. Enable Object Lock
    gcs.set_object_retention(
        f"{gcs_path}receipts.jsonl.gz",
        retention_mode="GOVERNANCE",
        retention_period_days=2555
    )

    # 6. Emit receipt
    return emit_receipt("decommission_export_complete", {
        "export_id": uuid4(),
        "files_archived": len(manifest["files"]),
        "total_size_bytes": sum(f["size_bytes"] for f in manifest["files"]),
        "archive_readable": True
    })
```

### Archive Verification

**Automated (after export)**:
```
1. Check manifest.json exists
2. Check all files listed in manifest exist in GCS
3. Download one file (smallest, for speed)
4. Decompress and parse
5. Verify record count matches manifest
6. Verify SHA-256 matches manifest
7. Check Object Lock is enabled
```

**Manual (on demand)**:
```bash
# List archive for a SKU
gsutil ls gs://ggen-archive-prod/2026-01-25/550e8400-e29b-41d4-a716-446655440000/

# Download and verify checksum
gsutil cp gs://ggen-archive-prod/2026-01-25/550e8400-e29b-41d4-a716-446655440000/receipts.jsonl.gz /tmp/
gsutil hash -m gs://ggen-archive-prod/2026-01-25/550e8400-e29b-41d4-a716-446655440000/receipts.jsonl.gz

# Decompress and inspect
gunzip /tmp/receipts.jsonl.gz
head -1 /tmp/receipts.jsonl | jq .

# Count records
wc -l /tmp/receipts.jsonl

# Verify manifest
gsutil cat gs://ggen-archive-prod/2026-01-25/550e8400-e29b-41d4-a716-446655440000/manifest.json | jq .
```

---

## Compliance Framework

### NIST SP 800-53 AU-11 (Audit Information Protection)

**Requirement**: "The information system protects audit information and audit tools from unauthorized access, modification, and deletion."

**ggen Implementation**:
- ✅ GCS Object Lock (GOVERNANCE mode, cannot delete during retention period)
- ✅ Service account authentication (only ggen-archive-reader can read)
- ✅ Cloud Audit Logs (track all GCS accesses)
- ✅ Checksums (SHA-256 per file, detect tampering)
- ✅ Chain-of-custody receipts (cryptographic proof of integrity)

### SOX (Sarbanes-Oxley) Section 302

**Requirement**: "The certifying officers shall maintain, or cause to be maintained, internal controls."

**ggen Implementation**:
- ✅ Receipt audit trail (every action receipted)
- ✅ 7-year retention (minimum SOX requirement)
- ✅ Immutable archive (Object Lock prevents tampering)
- ✅ Chain verification (hash chain proves integrity)

### GDPR Article 17 (Right to Be Forgotten)

**Requirement**: "The data subject shall have the right to obtain from the controller the erasure of personal data concerning them."

**ggen Implementation**:
- ✅ Expedited deletion (decommission_forgotten phase)
- ✅ Verification (SELECT * from Firestore, should return 0)
- ✅ GCS cleanup (after 90 days right-to-know window)

### California Consumer Privacy Act (CCPA)

**Requirement**: "Consumer has right to know, delete, opt-out of sale."

**ggen Implementation**:
- ✅ Right to know (retrieve from GCS archive if <7 years old)
- ✅ Right to delete (expedited decommission)
- ✅ Opt-out (entitlement cancellation)

---

## Access Controls

### Authentication

**Service Account** (for automated archive access):
```
Service Account: ggen-archive-reader@{project}.iam.gserviceaccount.com
Permissions:
  - storage.buckets.list
  - storage.objects.list
  - storage.objects.get (read-only)
  - storage.objects.getMetadata
  - logs.entries.list (for Cloud Logging export)

No permissions:
  - storage.objects.delete
  - storage.objects.create
  - storage.objects.update
```

**Human Access** (for manual retrieval):
```
Process:
  1. Requestor: "I need to access {sku_id} archive"
  2. Compliance team: Verify business justification
  3. If approved: Grant temporary read access (24h max)
  4. Requestor: Download data (all accesses logged)
  5. After 24h: Revoke access automatically
  6. Compliance: Audit log of who accessed what, when
```

**Role-Based Access Control (RBAC)**:
```
Role: ggen-compliance-auditor
  Permissions:
    - List GCS buckets
    - List GCS objects
    - Read GCS objects (for compliance audits)
    - Query Cloud Audit Logs
  Conditions:
    - Valid corporate email
    - Signed compliance agreement
    - Max 24-hour access window
    - All reads logged to Cloud Audit Logs

Role: ggen-ops-engineer
  Permissions:
    - Manage GCS Object Lock policies
    - Delete expired objects (after retention expires)
    - Monitor GCS bucket health
  Conditions:
    - Multi-factor authentication required
    - Require approval for deletions >100GB
    - All deletions logged to Cloud Audit Logs
```

### Audit Logging

**Cloud Audit Logs**:
```
Every GCS access logged with:
  - requestor: service account or user
  - action: list, get, delete
  - resource: gs://ggen-archive-prod/...
  - timestamp: RFC3339
  - status: success or failure
  - ip_address: requestor's IP
  - user_agent: API client
  - error_message: if failed

Query Example:
  gcloud logging read \
    'resource.type=gcs_bucket AND
     resource.labels.bucket_name=ggen-archive-prod AND
     protoPayload.methodName="storage.objects.get"' \
    --limit 100
```

---

## Retrieval Process

### Scenario 1: Customer Requests Their Data (GDPR)

**Process**:
```
Customer: "Send me my data from {sku_id}"
  ↓
Legal Team: Verify GDPR applicability + identity
  ↓
Compliance: Query GCS archive for {sku_id}
  → If found: "Archive exists, generating delivery package..."
  → If not found: "Archive not found, checking Firestore..."
  ↓
Download from GCS: gs://ggen-archive-prod/{date}/{sku_id}/receipts.jsonl.gz
  ↓
Decrypt (if encrypted): Use KMS key
  ↓
Generate package:
  - receipts.jsonl
  - configs.json
  - README.txt (explain formats)
  ↓
Upload to secure download link
  ↓
Send link to customer (valid for 7 days)
  ↓
After 7 days: Delete from download server
  ↓
Compliance: Record data subject access request (DSAR log)
```

### Scenario 2: Regulatory Audit (SOX)

**Process**:
```
Auditor: "Need receipts for {sku_id} for years 2020-2023"
  ↓
Compliance: Request scope verification
  → Auditor provides audit mandate + dates
  ↓
Compliance: Query GCS for matching archives
  → gs://ggen-archive-prod/2020-*/*/receipts.jsonl.gz
  → gs://ggen-archive-prod/2021-*/*/receipts.jsonl.gz
  → gs://ggen-archive-prod/2022-*/*/receipts.jsonl.gz
  → gs://ggen-archive-prod/2023-*/*/receipts.jsonl.gz
  ↓
Grant auditor temporary read access (90 days)
  ↓
Auditor downloads, queries, analyzes
  ↓
Auditor: "Audit complete"
  ↓
Compliance: Revoke access, log to audit log
```

### Scenario 3: Incident Investigation (Ops)

**Process**:
```
Ops Team: "Issue with {sku_id}, need to debug action failures"
  ↓
Query: Check if {sku_id} still in Firestore (if recent)
  → Yes: Query Firestore directly (fast)
  → No: Proceed to GCS
  ↓
Query: Find matching date ranges
  → If within 365 days: In Firestore (TTL not expired)
  → If > 365 days: In GCS only
  ↓
Retrieve from GCS:
  gsutil cp gs://ggen-archive-prod/{date}/{sku_id}/receipts.jsonl.gz /tmp/
  gunzip /tmp/receipts.jsonl.gz
  grep "action_failed" /tmp/receipts.jsonl | head -20
  ↓
Analyze failures:
  - Error code: "permission_denied"
  - Frequency: 5 failures in 1 hour
  - Timestamp: 2026-01-25T14:30:00Z
  ↓
Ops: "Found the bug, implementing fix"
```

---

## Deletion Process

### Automatic Deletion (After Retention Expires)

**Trigger**: Retention expiration_ts reached (7 years after archival).

**Process**:
```
Batch Job: Daily, 2:00 AM UTC
  ↓
For each object in gs://ggen-archive-prod/:
  1. Check Object Lock retention_expiration_ts
  2. If expiration_ts <= now:
     - Check legal_hold flag (in Firestore decommission record)
     - If legal_hold == false:
       - Delete object from GCS
       - Log deletion to Cloud Audit Logs
       - Emit: object_deleted receipt
     - Else:
       - Skip (preserve for legal hold)
  3. Else:
     - Skip (retention still active)
  ↓
Report: "Deleted 234 objects, total 500GB"
```

**Pseudo-code**:
```python
async def auto_delete_expired_archives():
    bucket = gcs.bucket("ggen-archive-prod")

    for blob in bucket.list_blobs():
        # 1. Get retention info
        retention_time = blob.retention_expiration_time
        if retention_time and retention_time <= datetime.now(timezone.utc):
            # 2. Check legal hold
            sku_id = extract_sku_from_path(blob.name)
            decommission_rec = firestore.collection("decommissions") \
                .document(sku_id).get()

            if decommission_rec.get("legal_hold") == False:
                # 3. Delete
                blob.delete()
                logging.info(f"Deleted {blob.name}")
                emit_receipt("object_deleted", {
                    "object": blob.name,
                    "size_bytes": blob.size
                })
```

### Manual Deletion (With Approval)

**When**: Early deletion needed (e.g., compliance violation, customer request).

**Process**:
```
Requestor: "Need to delete {sku_id} archive before 7 years"
  ↓
Compliance: Verify justification
  → Legal requirement? (GDPR expedited deletion)
  → Data integrity violation? (corrupted archive)
  → Customer request? (with written consent)
  ↓
If approved:
  - Compliance signs off in ticket
  - Ops runs deletion script (requires approval in Terraform)
  ↓
Deletion:
  gcloud storage rm gs://ggen-archive-prod/{date}/{sku_id}/ \
    --recursive \
    --object-retention=OVERRIDE
  ↓
Verification:
  gsutil ls gs://ggen-archive-prod/{date}/{sku_id}/
  # Should return: "CommandException: No URLs matched"
  ↓
Compliance: Log deletion to audit trail
  Emit: manual_deletion_approved receipt
```

---

## Examples

### Example 1: Archive Size Estimation

**Input**:
```
SKU: 550e8400-e29b-41d4-a716-446655440000
Account: 650e8400-e29b-41d4-a716-446655440001
Duration: 3 years active
Signal frequency: 100 signals/day (on average)
Action frequency: 50 actions/day
```

**Calculation**:
```
Receipts per day: 150 (100 signals + 50 actions)
Receipts per year: 150 * 365 = 54,750
Receipts over 3 years: 54,750 * 3 = 164,250

Avg receipt size: ~1 KB
Total receipt size: 164,250 KB ≈ 160 MB
Compressed (gzip): 160 MB / 4 ≈ 40 MB

Configs: ~5 MB

Audit logs: ~500 MB (Cloud Logging exports)

Total archive: 40 + 5 + 500 ≈ 545 MB ≈ 0.5 GB

GCS storage cost (7-year retention):
  0.5 GB * 7 years * $0.02/GB/month * 12 months/year
  = 0.5 * 7 * 0.02 * 12
  = $8.40 per SKU per 7 years
```

### Example 2: Retrieval Time Estimate

**Scenario**: Download 3-year archive for one SKU.

**Timeline**:
```
GCS list + stat: 5 seconds (verify manifest)
Download (via gsutil): 2 minutes (at 5 MB/s)
Decompress: 30 seconds (gzip -d)
Verify checksums: 15 seconds (sha256sum)
Total: ~3 minutes
```

**Script**:
```bash
#!/bin/bash
SKU_ID="550e8400-e29b-41d4-a716-446655440000"
DATE="2026-01-25"
DEST="/data/archive-retrieval/"

mkdir -p $DEST
cd $DEST

# Download manifest
gsutil cp gs://ggen-archive-prod/$DATE/$SKU_ID/manifest.json .

# Download all files
gsutil -m cp gs://ggen-archive-prod/$DATE/$SKU_ID/*.gz .

# Verify checksums (from manifest)
gsutil hash -m gs://ggen-archive-prod/$DATE/$SKU_ID/*.gz

# Decompress all
gunzip *.gz

# Verify record counts
echo "Receipt count: $(wc -l < receipts.jsonl)"
echo "Archive retrieval complete"
```

---

## Receipt Contract

**Every archive MUST**:
- ✅ Have manifest.json with metadata and checksums
- ✅ Have Object Lock enabled (GOVERNANCE mode, 7-year retention)
- ✅ Be readable (verified before proceeding)
- ✅ Have SHA-256 checksums (detect tampering)
- ✅ Have record counts (verify completeness)
- ✅ Be queryable via Cloud Logging (audit trail)
- ✅ Be deletable only after retention expires OR legal approval

---

## Definition of Done

- [x] Data retention policy documented (receipts 7y, logs 2y, configs 7y, transient 7d)
- [x] GCS archive structure and Object Lock explained
- [x] Archive process with pseudo-code provided
- [x] Compliance framework (NIST AU-11, SOX, GDPR, CCPA) documented
- [x] Access controls (authentication, RBAC, audit logging) detailed
- [x] Retrieval process for 3 scenarios (GDPR, audit, investigation)
- [x] Deletion process (automatic + manual) explained
- [x] Size estimation and cost calculation example
- [x] Glossary references included

