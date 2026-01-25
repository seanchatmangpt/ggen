# Receipt Schema (CANONICAL)

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

> **CRITICAL**: This is the CANONICAL receipt schema. All agents and systems MUST reference this document. No paraphrasing, no duplication.

## Table of Contents
1. [Base Receipt Structure](#base-receipt-structure)
2. [Receipt Types Catalog](#receipt-types-catalog)
3. [JSON Schema](#json-schema)
4. [Decision Encoding](#decision-encoding)
5. [Chain of Custody](#chain-of-custody)
6. [Examples](#examples)

---

## Base Receipt Structure

Every receipt emitted by ggen follows this base structure:

```json
{
  "kind": "receipt_type",
  "ts": "2026-01-25T14:32:17.123456Z",
  "decision": "accept|refuse|unknown",
  "project_id": "gcp-project-id",
  "repo": "github.com/org/repo",
  "branch": "main",
  "sku_id": "sku-uuid-or-none",
  "account_id": "account-uuid-or-none",
  "details": { /* type-specific details */ },
  "prev_chain_hash_b64": "base64-encoded-sha256-of-previous-receipt"
}
```

### Base Fields Explained

| Field | Type | Required | Purpose |
|-------|------|----------|---------|
| `kind` | string | YES | Receipt type (e.g., "signal_received", "action_completed") |
| `ts` | RFC3339 | YES | Timestamp in UTC (ISO 8601 + nanoseconds) |
| `decision` | enum | YES | One of: "accept" (action allowed), "refuse" (action denied), "unknown" (unable to decide) |
| `project_id` | string | YES | GCP project ID (e.g., "ggen-prod-123456") |
| `repo` | string | YES | GitHub repository (full path) |
| `branch` | string | YES | Git branch name |
| `sku_id` | string | NO | SKU identifier (UUID format) |
| `account_id` | string | NO | Account identifier (UUID format) |
| `details` | object | YES | Type-specific payload (see individual receipt types) |
| `prev_chain_hash_b64` | string | YES | Base64-encoded SHA-256 of previous receipt (for chain verification) |

---

## Receipt Types Catalog

### Action Receipts

#### action_attempted
**When**: Action is initiated (signal → action queue entry).
**Decision**: always "accept" (receipt only on successful queue entry).

```json
{
  "kind": "action_attempted",
  "decision": "accept",
  "details": {
    "action_id": "action-uuid",
    "action_type": "launch|update|terminate|delete",
    "signal_id": "signal-uuid",
    "entitlement_id": "entitlement-uuid",
    "queue_position": 1234,
    "retry_count": 0,
    "deadline_ts": "2026-01-25T14:35:17.123456Z"
  }
}
```

#### action_completed
**When**: Action reaches terminal state (success).
**Decision**: "accept".

```json
{
  "kind": "action_completed",
  "decision": "accept",
  "details": {
    "action_id": "action-uuid",
    "action_type": "launch|update|terminate|delete",
    "duration_ms": 1234,
    "output": { /* action-specific output */ },
    "state_before": "pending",
    "state_after": "succeeded"
  }
}
```

#### action_failed
**When**: Action reaches terminal state (error).
**Decision**: "refuse" (refusal to allow retry).

```json
{
  "kind": "action_failed",
  "decision": "refuse",
  "details": {
    "action_id": "action-uuid",
    "action_type": "launch|update|terminate|delete",
    "duration_ms": 1234,
    "error_code": "action_timeout|permission_denied|resource_exhausted",
    "error_message": "Human-readable error",
    "retry_count": 0,
    "next_action": "dead_letter|retry|manual_intervention"
  }
}
```

#### action_timeout
**When**: Action exceeds deadline.
**Decision**: "refuse".

```json
{
  "kind": "action_timeout",
  "decision": "refuse",
  "details": {
    "action_id": "action-uuid",
    "action_type": "launch|update|terminate|delete",
    "deadline_ts": "2026-01-25T14:35:17.123456Z",
    "elapsed_ms": 180000,
    "state": "in_flight",
    "remediation": "dead_letter|retry_with_backoff"
  }
}
```

### Decommission Receipts

#### decommission_initiated
**When**: Termination notice received.
**Decision**: "accept".

```json
{
  "kind": "decommission_initiated",
  "decision": "accept",
  "details": {
    "reason": "contract_ended|customer_unsubscribed|force_termination|legal_hold",
    "initiated_by": "system|legal_team|customer",
    "final_access_ts": "2026-02-24T23:59:59Z",
    "phase": "NOTICE_PERIOD",
    "entitlement_ids_affected": ["ent-uuid-1", "ent-uuid-2"],
    "signal_queue_depth": 1234
  }
}
```

#### decommission_notice_sent
**When**: Customer notification email sent.
**Decision**: "accept".

```json
{
  "kind": "decommission_notice_sent",
  "decision": "accept",
  "details": {
    "customer_email": "customer@example.com",
    "reactivation_deadline": "2026-02-24T23:59:59Z",
    "days_remaining": 30,
    "notice_id": "notice-uuid",
    "delivery_status": "sent|failed"
  }
}
```

#### decommission_shutting_down
**When**: Signal intake closes, drain phase begins.
**Decision**: "refuse" (all new signals rejected).

```json
{
  "kind": "decommission_shutting_down",
  "decision": "refuse",
  "details": {
    "phase": "SHUTTING_DOWN",
    "signal_queue_depth": 234,
    "action_queue_depth": 45,
    "signal_cutoff_ts": "2026-01-25T14:32:17.123456Z",
    "drain_deadline": "2026-01-28T14:32:17.123456Z"
  }
}
```

#### decommission_export_started
**When**: Archive export begins.
**Decision**: "accept".

```json
{
  "kind": "decommission_export_started",
  "decision": "accept",
  "details": {
    "phase": "SHUTTING_DOWN",
    "export_id": "export-uuid",
    "data_types": ["receipts", "configs", "audit_logs", "entitlements"],
    "estimated_size_bytes": 1073741824,
    "destination_gcs": "gs://backup-bucket/decommission/2026-01-25/"
  }
}
```

#### decommission_export_complete
**When**: Archive export finishes successfully.
**Decision**: "accept".

```json
{
  "kind": "decommission_export_complete",
  "decision": "accept",
  "details": {
    "phase": "SHUTTING_DOWN",
    "export_id": "export-uuid",
    "files_exported": 5678,
    "total_size_bytes": 1073741824,
    "duration_ms": 120000,
    "destination_gcs": "gs://backup-bucket/decommission/2026-01-25/",
    "checksum_sha256": "abc123...",
    "archive_readable": true
  }
}
```

#### decommission_resource_cleanup
**When**: GCP resources deleted (Pub/Sub topics, Firestore collections, Cloud Run services).
**Decision**: "accept".

```json
{
  "kind": "decommission_resource_cleanup",
  "decision": "accept",
  "details": {
    "phase": "ARCHIVED",
    "resources_deleted": {
      "pubsub_topics": 5,
      "firestore_collections": 8,
      "cloud_run_services": 2,
      "gcs_buckets": 0
    },
    "cleanup_id": "cleanup-uuid",
    "duration_ms": 30000,
    "errors": []
  }
}
```

#### decommission_archived
**When**: Data moved to long-term storage (Object Lock enabled).
**Decision**: "accept".

```json
{
  "kind": "decommission_archived",
  "decision": "accept",
  "details": {
    "phase": "ARCHIVED",
    "archive_bucket": "gs://ggen-archive-prod/2026-01-25/sku-uuid/",
    "object_lock_enabled": true,
    "retention_years": 7,
    "data_types": ["receipts", "configs"],
    "files_archived": 5678,
    "archive_size_bytes": 1073741824,
    "access_policy": "read_only_via_service_account"
  }
}
```

#### decommission_forgotten
**When**: All traces removed (GDPR right to be forgotten).
**Decision**: "accept".

```json
{
  "kind": "decommission_forgotten",
  "decision": "accept",
  "details": {
    "phase": "FORGOTTEN",
    "retention_expired_ts": "2027-01-25T14:32:17.123456Z",
    "data_deleted": {
      "configs": 123,
      "logs": 4567,
      "transient_data": 89012
    },
    "deletion_id": "deletion-uuid",
    "retention_policy_followed": true,
    "compliance_verified": true
  }
}
```

### Entitlement Receipts

#### entitlement_active
**When**: Marketplace webhook confirms entitlement activation.
**Decision**: "accept".

```json
{
  "kind": "entitlement_active",
  "decision": "accept",
  "details": {
    "entitlement_id": "entitlement-uuid",
    "sku_id": "sku-uuid",
    "account_id": "account-uuid",
    "activation_ts": "2026-01-25T14:32:17.123456Z",
    "expiration_ts": "2027-01-25T14:32:17.123456Z",
    "auto_renew": true,
    "webhook_source": "gcp.marketplace",
    "billing_cycle": "annual"
  }
}
```

#### entitlement_cancelled
**When**: Marketplace webhook confirms entitlement cancellation.
**Decision**: "refuse".

```json
{
  "kind": "entitlement_cancelled",
  "decision": "refuse",
  "details": {
    "entitlement_id": "entitlement-uuid",
    "sku_id": "sku-uuid",
    "account_id": "account-uuid",
    "cancellation_ts": "2026-01-25T14:32:17.123456Z",
    "reason": "customer_request|non_payment|contract_end",
    "webhook_source": "gcp.marketplace"
  }
}
```

### Health & Readiness Receipts

#### health_check_passed
**When**: GET /health succeeds.
**Decision**: "accept".

```json
{
  "kind": "health_check_passed",
  "decision": "accept",
  "details": {
    "check_id": "health-uuid",
    "dependencies": {
      "firestore": "healthy",
      "pubsub": "healthy",
      "gcs": "healthy"
    },
    "uptime_seconds": 86400,
    "response_time_ms": 45
  }
}
```

#### health_check_failed
**When**: GET /health fails (at least one dependency down).
**Decision**: "unknown".

```json
{
  "kind": "health_check_failed",
  "decision": "unknown",
  "details": {
    "check_id": "health-uuid",
    "dependencies": {
      "firestore": "healthy",
      "pubsub": "unhealthy",
      "gcs": "healthy"
    },
    "failing_component": "pubsub",
    "error_message": "connection timeout"
  }
}
```

### Incident Receipts

#### incident_detected
**When**: Invariant violation detected.
**Decision**: "refuse".

```json
{
  "kind": "incident_detected",
  "decision": "refuse",
  "details": {
    "incident_id": "incident-uuid",
    "severity": "critical|high|medium",
    "type": "data_corruption|unauthorized_access|quota_exceeded|rate_limit|cascade_failure",
    "description": "Human-readable description",
    "affected_entities": ["sku-uuid-1", "account-uuid-2"],
    "action_taken": "signal_blocked|action_cancelled|auto_remediation_triggered",
    "requires_manual_intervention": false
  }
}
```

#### incident_resolved
**When**: Invariant violation cleared.
**Decision**: "accept".

```json
{
  "kind": "incident_resolved",
  "decision": "accept",
  "details": {
    "incident_id": "incident-uuid",
    "resolution_ts": "2026-01-25T14:35:17.123456Z",
    "time_to_resolution_seconds": 300,
    "root_cause": "cascade_failure_in_pubsub",
    "remediation": "auto_recovery_via_retry_backoff"
  }
}
```

### Invariant & Policy Receipts

#### invariant_violation
**When**: Signature mismatch, chain hash mismatch, or state machine violation.
**Decision**: "refuse".

```json
{
  "kind": "invariant_violation",
  "decision": "refuse",
  "details": {
    "invariant_id": "inv-signature|inv-chain|inv-state",
    "violation_type": "signature_mismatch|chain_hash_mismatch|invalid_state_transition",
    "expected": "expected_value",
    "actual": "actual_value",
    "remediation": "signal_rejected|action_reverted"
  }
}
```

#### invariant_check_passed
**When**: All invariant checks pass (post-action verification).
**Decision**: "accept".

```json
{
  "kind": "invariant_check_passed",
  "decision": "accept",
  "details": {
    "check_id": "invariant-check-uuid",
    "checks_passed": ["signature_valid", "chain_integrity", "state_consistency"],
    "total_checks": 3,
    "duration_ms": 12
  }
}
```

#### policy_loaded
**When**: Policy configuration loaded into memory.
**Decision**: "accept".

```json
{
  "kind": "policy_loaded",
  "decision": "accept",
  "details": {
    "policy_id": "policy-uuid",
    "policy_version": "1.2.3",
    "rules_loaded": 456,
    "load_time_ms": 234,
    "source": "firestore|gcs|local_config"
  }
}
```

#### policy_load_failed
**When**: Policy load fails.
**Decision**: "refuse".

```json
{
  "kind": "policy_load_failed",
  "decision": "refuse",
  "details": {
    "policy_id": "policy-uuid",
    "error_code": "not_found|syntax_error|permission_denied",
    "error_message": "Policy file not found in GCS",
    "fallback_used": "previous_cached_version"
  }
}
```

### Permission Receipts

#### permission_granted
**When**: Authorization check succeeds.
**Decision**: "accept".

```json
{
  "kind": "permission_granted",
  "decision": "accept",
  "details": {
    "principal": "service-account@project.iam.gserviceaccount.com",
    "resource": "sku-uuid",
    "action": "read|write|delete|admin",
    "check_duration_ms": 5
  }
}
```

#### permission_denied
**When**: Authorization check fails (principal not authorized).
**Decision**: "refuse".

```json
{
  "kind": "permission_denied",
  "decision": "refuse",
  "details": {
    "principal": "attacker@external.com",
    "resource": "sku-uuid-secret",
    "action": "read",
    "reason": "not_authenticated|insufficient_role|tenant_mismatch"
  }
}
```

### Quota & Limit Receipts

#### quota_exceeded
**When**: Rate limit or quota exceeded.
**Decision**: "refuse".

```json
{
  "kind": "quota_exceeded",
  "decision": "refuse",
  "details": {
    "quota_type": "rate_limit|signal_count|action_count|storage",
    "limit": 1000,
    "current_value": 1001,
    "reset_ts": "2026-01-26T00:00:00Z",
    "response_code": 429,
    "retry_after_seconds": 3600
  }
}
```

#### quota_reset
**When**: Quota counter resets (daily/hourly).
**Decision**: "accept".

```json
{
  "kind": "quota_reset",
  "decision": "accept",
  "details": {
    "quota_type": "rate_limit",
    "window": "hourly|daily|monthly",
    "reset_ts": "2026-01-26T00:00:00Z",
    "previous_usage": 456,
    "new_limit": 1000
  }
}
```

### Signal Receipts

#### signal_received
**When**: POST /signal/{sku_id}/{tenant_id} accepted.
**Decision**: "accept".

```json
{
  "kind": "signal_received",
  "decision": "accept",
  "details": {
    "signal_id": "signal-uuid",
    "signal_type": "launch|update|terminate|delete",
    "source": "webhook|api|direct",
    "signature_algorithm": "hmac-sha256",
    "signature_valid": true,
    "queue_position": 1234,
    "estimated_processing_time_seconds": 60
  }
}
```

#### signal_storm_detected
**When**: Rapid request rate detected.
**Decision**: "refuse".

```json
{
  "kind": "signal_storm_detected",
  "decision": "refuse",
  "details": {
    "detection_ts": "2026-01-25T14:32:17.123456Z",
    "requests_in_window": 5000,
    "window_duration_seconds": 60,
    "threshold": 1000,
    "source_ip": "192.0.2.1",
    "action_taken": "ip_blocked|rate_limit_applied|alert_sent"
  }
}
```

### Generic Refusal

#### refusal
**When**: Generic refusal (catch-all for any denial).
**Decision**: "refuse".

```json
{
  "kind": "refusal",
  "decision": "refuse",
  "details": {
    "reason": "invalid_request|internal_error|service_unavailable",
    "error_code": "400|500|503",
    "error_message": "Human-readable error",
    "correlation_id": "corr-uuid"
  }
}
```

---

## JSON Schema

Complete JSON Schema for receipt validation:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://ggen.dev/schemas/receipt-v6.0.0.json",
  "title": "ggen Receipt Schema",
  "type": "object",
  "required": ["kind", "ts", "decision", "project_id", "repo", "branch", "details", "prev_chain_hash_b64"],
  "properties": {
    "kind": {
      "type": "string",
      "enum": [
        "action_attempted", "action_completed", "action_failed", "action_timeout",
        "decommission_initiated", "decommission_notice_sent", "decommission_shutting_down",
        "decommission_export_started", "decommission_export_complete", "decommission_resource_cleanup",
        "decommission_archived", "decommission_forgotten",
        "entitlement_active", "entitlement_cancelled",
        "health_check_passed", "health_check_failed",
        "incident_detected", "incident_resolved",
        "invariant_violation", "invariant_check_passed",
        "policy_loaded", "policy_load_failed",
        "permission_granted", "permission_denied",
        "quota_exceeded", "quota_reset",
        "signal_received", "signal_storm_detected",
        "refusal"
      ]
    },
    "ts": {
      "type": "string",
      "format": "date-time",
      "description": "RFC 3339 timestamp with nanosecond precision"
    },
    "decision": {
      "type": "string",
      "enum": ["accept", "refuse", "unknown"],
      "description": "Outcome of receipt event"
    },
    "project_id": {
      "type": "string",
      "pattern": "^[a-z0-9\\-]{6,30}$"
    },
    "repo": {
      "type": "string",
      "pattern": "^github\\.com/[a-zA-Z0-9\\-]+/[a-zA-Z0-9\\-_.]+$"
    },
    "branch": {
      "type": "string",
      "pattern": "^[a-zA-Z0-9\\-_./]+$"
    },
    "sku_id": {
      "type": ["string", "null"],
      "pattern": "^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$"
    },
    "account_id": {
      "type": ["string", "null"],
      "pattern": "^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$"
    },
    "details": {
      "type": "object",
      "description": "Type-specific payload (varies by kind)"
    },
    "prev_chain_hash_b64": {
      "type": "string",
      "pattern": "^[A-Za-z0-9+/]+=*$",
      "description": "Base64-encoded SHA-256 of previous receipt"
    }
  },
  "additionalProperties": false
}
```

---

## Decision Encoding

### Decision States

| Decision | Meaning | HTTP Status | Action |
|----------|---------|-------------|--------|
| `accept` | Action allowed, operation succeeded | 200, 201, 202 | Proceed with action |
| `refuse` | Action denied, operation failed | 400, 403, 429, 500 | Reject request, emit receipt |
| `unknown` | Unable to determine (dependency down) | 503 | Retry with backoff |

### Signal Flow with Decisions

```
Signal Input
   ↓
[Validation] → {refuse: invalid_signature} OR proceed
   ↓
[Authorization] → {refuse: permission_denied} OR proceed
   ↓
[Quota Check] → {refuse: quota_exceeded} OR proceed
   ↓
[Entitlement Check] → {refuse: entitlement_cancelled} OR proceed
   ↓
[Action Queue] → {accept: action_attempted} + queued
   ↓
[Processing] → {accept: action_completed} OR {refuse: action_failed}
   ↓
[Receipt Emission] → Write to Firestore + Pub/Sub
```

---

## Chain of Custody

Every receipt includes `prev_chain_hash_b64` to form cryptographic chain:

```
Receipt 1 (ts: 14:32:00)
  prev_chain_hash_b64: "zero_hash"
  content_hash: SHA256("action_attempted + details") = "hash1"
   ↓
Receipt 2 (ts: 14:32:05)
  prev_chain_hash_b64: "hash1"
  content_hash: SHA256("action_completed + details") = "hash2"
   ↓
Receipt 3 (ts: 14:32:10)
  prev_chain_hash_b64: "hash2"
  content_hash: SHA256("signal_received + details") = "hash3"
   ↓
... (chain continues)
```

**Chain Verification Algorithm**:
```python
def verify_chain(receipts: list[Receipt]) -> bool:
    for i, receipt in enumerate(receipts):
        if i == 0:
            assert receipt.prev_chain_hash_b64 == "AAAAAAA..." # genesis hash
        else:
            prev_hash = sha256(json.dumps(receipts[i-1]))
            assert receipt.prev_chain_hash_b64 == b64encode(prev_hash)
    return True
```

---

## Examples

### Complete Signal Flow with Receipts

**Scenario**: Customer launches new SKU instance.

```json
[
  {
    "kind": "signal_received",
    "ts": "2026-01-25T14:32:00.000000Z",
    "decision": "accept",
    "project_id": "ggen-prod-123456",
    "repo": "github.com/org/ggen",
    "branch": "main",
    "sku_id": "550e8400-e29b-41d4-a716-446655440000",
    "account_id": "650e8400-e29b-41d4-a716-446655440001",
    "details": {
      "signal_id": "signal-uuid-001",
      "signal_type": "launch",
      "source": "webhook",
      "signature_valid": true,
      "queue_position": 1234
    },
    "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
  },
  {
    "kind": "action_attempted",
    "ts": "2026-01-25T14:32:02.100000Z",
    "decision": "accept",
    "project_id": "ggen-prod-123456",
    "repo": "github.com/org/ggen",
    "branch": "main",
    "sku_id": "550e8400-e29b-41d4-a716-446655440000",
    "account_id": "650e8400-e29b-41d4-a716-446655440001",
    "details": {
      "action_id": "action-uuid-001",
      "action_type": "launch",
      "signal_id": "signal-uuid-001",
      "queue_position": 1234,
      "deadline_ts": "2026-01-25T14:35:02.100000Z"
    },
    "prev_chain_hash_b64": "hash_of_receipt_1"
  },
  {
    "kind": "action_completed",
    "ts": "2026-01-25T14:32:45.500000Z",
    "decision": "accept",
    "project_id": "ggen-prod-123456",
    "repo": "github.com/org/ggen",
    "branch": "main",
    "sku_id": "550e8400-e29b-41d4-a716-446655440000",
    "account_id": "650e8400-e29b-41d4-a716-446655440001",
    "details": {
      "action_id": "action-uuid-001",
      "action_type": "launch",
      "duration_ms": 43400,
      "output": {
        "instance_id": "instance-uuid-001",
        "public_ip": "203.0.113.45"
      },
      "state_after": "succeeded"
    },
    "prev_chain_hash_b64": "hash_of_receipt_2"
  }
]
```

---

## Receipt Contract

**Every receipt MUST be**:
- ✅ Valid JSON (parseable)
- ✅ Properly timestamped (RFC 3339 UTC)
- ✅ Cryptographically hashable (deterministic JSON serialization)
- ✅ Chain-linked (prev_chain_hash_b64 set correctly)
- ✅ Immutable in Firestore (no subsequent edits)
- ✅ Queryable by `kind`, `ts`, `decision`, `sku_id`, `account_id`
- ✅ Exportable to GCS (for audit trails, GDPR requests)

---

## Definition of Done

- [x] All 28+ receipt types documented with schema and examples
- [x] JSON Schema provided for validation
- [x] Chain-of-custody algorithm explained
- [x] Decision encoding clarified (accept/refuse/unknown)
- [x] Complete flow example provided
- [x] Glossary references included

