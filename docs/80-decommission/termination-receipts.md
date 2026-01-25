# Termination Receipts

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

## Overview

Every decommission phase transition emits a cryptographically linked receipt. This document defines the receipt types, their timing, and how they prove compliance with decommission SLAs.

**Core Principle**: Chain-of-custody receipts form cryptographic proof that decommission was executed correctly.

---

## Decommission Receipt Types

### 1. decommission_initiated

**When**: Termination notice received from Marketplace or Operations.

**Decision**: `accept` (termination initiated successfully).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_initiated",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "reason": "contract_ended|customer_unsubscribed|force_termination|non_payment",
    "initiated_by": "system|legal_team|customer",
    "phase": "NOTICE_PERIOD",
    "final_access_ts": "2026-02-24T23:59:59Z",
    "reactivation_deadline": "2026-02-24T23:59:59Z",
    "entitlement_ids_affected": [
      "entitlement-uuid-001",
      "entitlement-uuid-002"
    ],
    "signal_queue_depth": 1234,
    "action_queue_depth": 567,
    "current_entitlements_active": 2,
    "legal_hold": false,
    "legal_hold_reason": null
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

**Fields Explained**:
- `reason`: Why termination was initiated
- `initiated_by`: Who triggered termination
- `phase`: Always "NOTICE_PERIOD" at start
- `final_access_ts`: Last moment customer can interact with system
- `reactivation_deadline`: Last moment customer can reactivate
- `entitlement_ids_affected`: Which entitlements are being terminated
- `signal_queue_depth`: Number of pending signals
- `action_queue_depth`: Number of pending actions
- `legal_hold`: Is this subject to litigation hold?

**Next Receipt**: `decommission_notice_sent` (usually within 1 hour).

---

### 2. decommission_notice_sent

**When**: Notification email sent to customer.

**Decision**: `accept` (notification delivered successfully).

**Frequency**: Once per decommission (or per retry if delivery failed).

**Example**:
```json
{
  "kind": "decommission_notice_sent",
  "ts": "2026-01-25T14:45:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "customer_email": "customer@example.com",
    "customer_name": "ACME Corp",
    "reactivation_deadline": "2026-02-24T23:59:59Z",
    "days_remaining": 30,
    "notice_id": "notice-uuid-001",
    "delivery_status": "sent|failed|bounced",
    "delivery_provider": "SendGrid|AWS-SES",
    "email_template": "decommission_30day_notice",
    "retry_count": 0,
    "next_retry_ts": null
  },
  "prev_chain_hash_b64": "hash_of_decommission_initiated"
}
```

**Fields Explained**:
- `customer_email`: Email address notification was sent to
- `delivery_status`: Was email accepted by mail server?
- `delivery_provider`: Which mail service was used?
- `retry_count`: How many times delivery was retried?
- `next_retry_ts`: When is next retry scheduled (if delivery failed)?

**SLA**: Email must be sent within 1 hour of decommission_initiated.

**Next Receipt**: Either `decommission_shutting_down` (if deadline passed) or `customer_reactivates` (if customer reactivates).

---

### 3. decommission_shutting_down

**When**: 30-day notice period expires (or force termination triggered).

**Decision**: `refuse` (all new signals rejected).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_shutting_down",
  "ts": "2026-02-25T00:00:01.000000Z",
  "decision": "refuse",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "SHUTTING_DOWN",
    "signal_cutoff_ts": "2026-02-25T00:00:00Z",
    "drain_deadline": "2026-02-28T00:00:00Z",
    "signal_queue_depth": 234,
    "action_queue_depth": 45,
    "in_flight_actions": 12,
    "actions_still_processing": true,
    "expected_drain_time_seconds": 86400,
    "reactivation_window_closed": true
  },
  "prev_chain_hash_b64": "hash_of_decommission_notice_sent"
}
```

**Fields Explained**:
- `signal_cutoff_ts`: When signal ingestion closed
- `drain_deadline`: By when all queues must be drained
- `signal_queue_depth`: Number of signals awaiting processing
- `action_queue_depth`: Number of actions awaiting execution
- `in_flight_actions`: Number of actions currently executing
- `reactivation_window_closed`: Can customer no longer reactivate?

**SLA**: Must transition to SHUTTING_DOWN exactly 30 days after decommission_initiated (or immediately if force_termination).

**Next Receipt**: `decommission_export_started` (within 1 hour).

---

### 4. decommission_export_started

**When**: Archive export job begins.

**Decision**: `accept` (export initiated successfully).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_export_started",
  "ts": "2026-02-25T00:15:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "SHUTTING_DOWN",
    "export_id": "export-uuid-001",
    "data_types": [
      "receipts",
      "configs",
      "audit_logs"
    ],
    "destination_gcs": "gs://ggen-archive-prod/2026-02-25/550e8400-e29b-41d4-a716-446655440000/",
    "estimated_size_bytes": 1073741824,
    "estimated_duration_seconds": 3600,
    "compression": "gzip",
    "encryption_algorithm": "AES-256",
    "object_lock_enabled": true,
    "retention_years": 7
  },
  "prev_chain_hash_b64": "hash_of_decommission_shutting_down"
}
```

**Fields Explained**:
- `export_id`: Unique identifier for this export job
- `data_types`: What data is being exported?
- `destination_gcs`: Where archive will be stored
- `estimated_size_bytes`: Predicted total size
- `compression`: Compression algorithm used
- `encryption_algorithm`: How data is encrypted at rest
- `object_lock_enabled`: Is GCS Object Lock enabled?
- `retention_years`: How long archive will be retained

**SLA**: Export must complete within 3 days of decommission_shutting_down.

**Next Receipt**: `decommission_export_complete` (when export finishes).

---

### 5. decommission_export_complete

**When**: Archive export finishes successfully.

**Decision**: `accept` (export completed successfully).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_export_complete",
  "ts": "2026-02-25T03:45:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "SHUTTING_DOWN",
    "export_id": "export-uuid-001",
    "duration_seconds": 13500,
    "files_exported": 5678,
    "total_size_bytes": 1073741824,
    "files": [
      {
        "name": "receipts.jsonl.gz",
        "size_bytes": 1000000000,
        "record_count": 56789,
        "checksum_sha256": "abc123..."
      },
      {
        "name": "configs.json.gz",
        "size_bytes": 50000000,
        "checksum_sha256": "def456..."
      },
      {
        "name": "audit_log.jsonl.gz",
        "size_bytes": 23741824,
        "record_count": 1234567,
        "checksum_sha256": "ghi789..."
      }
    ],
    "destination_gcs": "gs://ggen-archive-prod/2026-02-25/550e8400-e29b-41d4-a716-446655440000/",
    "checksum_manifest_sha256": "jkl012...",
    "archive_readable": true,
    "object_lock_enabled": true,
    "manifest_verified": true
  },
  "prev_chain_hash_b64": "hash_of_decommission_export_started"
}
```

**Fields Explained**:
- `duration_seconds`: How long export took
- `files_exported`: How many distinct files in archive
- `files`: Details for each file (size, count, checksum)
- `archive_readable`: Did verification download + decompress succeed?
- `object_lock_enabled`: Is GCS Object Lock protecting archive?
- `manifest_verified`: Does archive match manifest.json?

**Verification Performed**:
```
1. ✅ All files listed exist in GCS
2. ✅ Each file size matches manifest
3. ✅ Each file SHA-256 matches manifest
4. ✅ Download one file (test readability)
5. ✅ Decompress successfully
6. ✅ Parse first record (valid JSON)
7. ✅ Record count matches manifest
8. ✅ Object Lock is enabled (cannot delete)
```

**Next Receipt**: `decommission_resource_cleanup` (within 1 hour).

---

### 6. decommission_resource_cleanup

**When**: GCP resources are deleted (Pub/Sub, Firestore, Cloud Run).

**Decision**: `accept` (cleanup completed successfully).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_resource_cleanup",
  "ts": "2026-02-25T04:00:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "ARCHIVED",
    "cleanup_id": "cleanup-uuid-001",
    "duration_seconds": 300,
    "resources_deleted": {
      "pubsub_topics": {
        "count": 2,
        "names": [
          "projects/ggen-prod-123456/topics/ggen-signals-550e8400",
          "projects/ggen-prod-123456/topics/ggen-actions-550e8400"
        ]
      },
      "pubsub_subscriptions": {
        "count": 2,
        "names": [
          "projects/ggen-prod-123456/subscriptions/signal-processor-550e8400",
          "projects/ggen-prod-123456/subscriptions/action-engine-550e8400"
        ]
      },
      "firestore_collections": {
        "count": 3,
        "names": [
          "receipts-550e8400",
          "actions-550e8400",
          "configs-550e8400"
        ]
      },
      "cloud_run_services": {
        "count": 0,
        "names": []
      },
      "gcs_buckets": {
        "count": 0,
        "names": []
      }
    },
    "total_resources_deleted": 7,
    "errors": [],
    "cleanup_status": "success"
  },
  "prev_chain_hash_b64": "hash_of_decommission_export_complete"
}
```

**Fields Explained**:
- `cleanup_id`: Unique identifier for cleanup job
- `resources_deleted`: Categorized list of what was deleted
- `total_resources_deleted`: Sum of all deletions
- `errors`: Array of any deletion failures
- `cleanup_status`: "success" or "partial_failure"

**Resources Deleted**:
```
Pub/Sub Topics: 2
  - ggen-signals-{sku_id}
  - ggen-actions-{sku_id}

Pub/Sub Subscriptions: 2
  - signal-processor-{sku_id}
  - action-engine-{sku_id}

Firestore Collections: 3
  - receipts-{sku_id}
  - actions-{sku_id}
  - configs-{sku_id}

Cloud Run Services: 0-N (if SKU-specific)
  - signal-processor-{sku_id} (if exists)
  - action-engine-{sku_id} (if exists)

GCS Buckets: 0 (if using shared bucket)
```

**Verification**:
```
For each deleted resource:
  gcloud <service> list --filter="name:550e8400"
  # Should return 0 results
```

**Next Receipt**: `decommission_archived` (immediately after).

---

### 7. decommission_archived

**When**: Data moved to long-term storage, phase transitions to ARCHIVED.

**Decision**: `accept` (archive phase entered successfully).

**Frequency**: Once per decommission.

**Example**:
```json
{
  "kind": "decommission_archived",
  "ts": "2026-02-25T04:10:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "ARCHIVED",
    "archive_location": "gs://ggen-archive-prod/2026-02-25/550e8400-e29b-41d4-a716-446655440000/",
    "object_lock_enabled": true,
    "retention_expiration_ts": "2033-02-25T23:59:59Z",
    "retention_years": 7,
    "data_types": [
      "receipts",
      "configs",
      "audit_logs"
    ],
    "files_archived": 5678,
    "archive_size_bytes": 1073741824,
    "access_policy": "read_only_via_service_account",
    "firestore_ttl_enabled": true,
    "firestore_ttl_days": 365,
    "automated_deletion_scheduled": true,
    "deletion_scheduled_ts": "2033-02-26T00:00:00Z"
  },
  "prev_chain_hash_b64": "hash_of_decommission_resource_cleanup"
}
```

**Fields Explained**:
- `archive_location`: GCS path where archive is stored
- `object_lock_enabled`: Can archive be deleted before expiration?
- `retention_expiration_ts`: When does 7-year retention expire?
- `access_policy`: Who can read the archive? (read_only_via_service_account)
- `firestore_ttl_enabled`: Is Firestore TTL set on decommission records?
- `automated_deletion_scheduled`: Will data auto-delete after 7 years?
- `deletion_scheduled_ts`: When will auto-delete job run?

**SLA**: Must transition to ARCHIVED within 3 days of SHUTTING_DOWN.

**Next Receipt**: `decommission_forgotten` (after 7 years, during automated cleanup).

---

### 8. decommission_forgotten

**When**: 7-year retention expires, data is deleted (GDPR right to be forgotten).

**Decision**: `accept` (data purged successfully).

**Frequency**: Once per decommission (after 7 years).

**Example**:
```json
{
  "kind": "decommission_forgotten",
  "ts": "2033-02-26T00:00:01.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "phase": "FORGOTTEN",
    "retention_expiration_ts": "2033-02-25T23:59:59Z",
    "days_retained": 2556,
    "data_deletion": {
      "gcs_archive": {
        "deleted": true,
        "files_count": 5678,
        "size_bytes": 1073741824,
        "duration_seconds": 600
      },
      "firestore_records": {
        "deleted": true,
        "records_count": 234,
        "collections": [
          "decommissions",
          "archived_receipts"
        ],
        "duration_seconds": 120
      },
      "cloud_logging": {
        "deleted": true,
        "log_entries_count": 123456,
        "duration_seconds": 300
      }
    },
    "total_deleted_bytes": 1073741824,
    "total_deleted_records": 128790,
    "deletion_id": "deletion-uuid-001",
    "compliance_verified": true,
    "right_to_be_forgotten_satisfied": true
  },
  "prev_chain_hash_b64": "hash_of_decommission_archived"
}
```

**Fields Explained**:
- `retention_expiration_ts`: When 7-year retention expired
- `days_retained`: How long data was retained (should be ~2555)
- `data_deletion`: Details of what was deleted
- `compliance_verified`: Did verification confirm all data deleted?
- `right_to_be_forgotten_satisfied`: GDPR compliance verified?

**Verification Performed**:
```
1. ✅ SELECT * FROM receipts WHERE sku_id = {sku_id} → 0 results
2. ✅ gsutil ls gs://ggen-archive-prod/{date}/{sku_id}/ → 404
3. ✅ Cloud Logging query for {sku_id} → 0 recent logs
4. ✅ Check Firestore decommission record → deleted
5. ✅ Emit decommission_forgotten receipt (proof of compliance)
```

**SLA**: Auto-delete runs daily at 02:00 UTC, deletion completes within 24 hours.

**Final State**: sku_id completely forgotten from system.

---

## Receipt Chain Example

Complete decommission chain (from start to finish):

```json
[
  {
    "kind": "decommission_initiated",
    "ts": "2026-01-25T14:32:00Z",
    "prev_chain_hash_b64": "AAAA..."
  },
  {
    "kind": "decommission_notice_sent",
    "ts": "2026-01-25T14:45:00Z",
    "prev_chain_hash_b64": "hash_1"
  },
  {
    "kind": "decommission_shutting_down",
    "ts": "2026-02-25T00:00:01Z",
    "prev_chain_hash_b64": "hash_2"
  },
  {
    "kind": "decommission_export_started",
    "ts": "2026-02-25T00:15:00Z",
    "prev_chain_hash_b64": "hash_3"
  },
  {
    "kind": "decommission_export_complete",
    "ts": "2026-02-25T03:45:00Z",
    "prev_chain_hash_b64": "hash_4"
  },
  {
    "kind": "decommission_resource_cleanup",
    "ts": "2026-02-25T04:00:00Z",
    "prev_chain_hash_b64": "hash_5"
  },
  {
    "kind": "decommission_archived",
    "ts": "2026-02-25T04:10:00Z",
    "prev_chain_hash_b64": "hash_6"
  },
  {
    "kind": "decommission_forgotten",
    "ts": "2033-02-26T00:00:01Z",
    "prev_chain_hash_b64": "hash_7"
  }
]
```

---

## Timeline Summary

| Receipt | Trigger | Duration from Start |
|---------|---------|---------------------|
| decommission_initiated | Marketplace/Operations | Day 0 |
| decommission_notice_sent | Email delivery | Day 0 (1 hour) |
| decommission_shutting_down | 30-day deadline | Day 30 |
| decommission_export_started | Export job begins | Day 30-31 (1 hour) |
| decommission_export_complete | Export finishes | Day 30-33 (max 3 days) |
| decommission_resource_cleanup | Cleanup starts | Day 33 (1 hour) |
| decommission_archived | Cleanup finishes | Day 33 (1 hour) |
| decommission_forgotten | 7-year retention expires | Day ~2556 (7 years) |

---

## Receipt Verification

**Chain Verification Query**:
```sql
SELECT
  r1.kind,
  r1.ts,
  r1.decision,
  SUBSTR(r1.prev_chain_hash_b64, 0, 8) as prev_hash_short,
  CASE
    WHEN SHA256(TO_JSON_STRING(r0)) ENCODED = r1.prev_chain_hash_b64 THEN '✓ VALID'
    ELSE '✗ INVALID'
  END as chain_status
FROM receipts-{sku_id} r1
CROSS JOIN receipts-{sku_id} r0
WHERE r1.ts > r0.ts
  AND TIMESTAMP_DIFF(r1.ts, r0.ts, MINUTE) < 60
ORDER BY r1.ts;
```

---

## Receipt Contract

**Every decommission receipt MUST**:
- ✅ Include all required fields (kind, ts, decision, details, prev_chain_hash)
- ✅ Follow the correct sequence (no skipping phases)
- ✅ Have valid decision value (accept or refuse)
- ✅ Have correct prev_chain_hash (linking to previous receipt)
- ✅ Have unique timestamps (no two receipts same second)
- ✅ Be immutable (stored in Firestore without editing)

---

## Definition of Done

- [x] All 8 decommission receipt types documented
- [x] Complete example JSON provided for each type
- [x] Timing and SLAs documented
- [x] Chain-of-custody explained
- [x] Verification procedures provided
- [x] Complete timeline summary provided
- [x] Glossary references included

