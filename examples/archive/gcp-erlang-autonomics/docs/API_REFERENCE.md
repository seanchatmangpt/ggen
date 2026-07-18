# API Reference

Complete service API documentation for GCP Erlang Autonomics.

---

## Table of Contents

1. [Signal Ingest API](#signal-ingest-api)
2. [Governor API](#governor-api)
3. [Actuator API](#actuator-api)
4. [Receipt Ledger API](#receipt-ledger-api)
5. [Domain Types](#domain-types)
6. [Error Handling](#error-handling)
7. [Examples](#examples)

---

## Signal Ingest API

The Signal Ingest service normalizes, deduplicates, and validates cloud events.

### Service Endpoint

```
https://signal-ingest-service-[hash].run.app
```

### API: normalize()

**Purpose**: Normalize a cloud event to internal signal format.

**Endpoint**: `POST /signals/normalize`

**Request**

```json
{
  "source": "logging.googleapis.com",
  "type": "billing.threshold.exceeded",
  "data": {
    "resource_id": "projects/123456",
    "metric_name": "cloudapis.googleapis.com/compute",
    "metric_value": 156,
    "baseline": 100,
    "unit": "percent"
  },
  "timestamp": "2026-01-25T10:30:45Z",
  "trace_id": "trace-uuid-1234"
}
```

**Response (Success)**

```json
{
  "signal_id": "sig-uuid-5678",
  "signal_type": "cost_spike",
  "normalized_value": 156.0,
  "baseline": 100.0,
  "threshold": 150.0,
  "triggered_sku": "cost-circuit-breaker-p1",
  "expected_action": "scale_down",
  "confidence": 0.95,
  "received_at": "2026-01-25T10:30:45.123Z",
  "source_trace_id": "trace-uuid-1234",
  "status": "success"
}
```

**Response (Error)**

```json
{
  "error": "validation_failed",
  "message": "Field 'metric_value' is required",
  "field": "data.metric_value",
  "status": "error"
}
```

**Error Codes**

| Code | HTTP | Description |
|------|------|-------------|
| `validation_failed` | 400 | Missing required field or invalid type |
| `unknown_signal_type` | 400 | Signal type not recognized |
| `parse_error` | 400 | JSON parsing failed |
| `internal_error` | 500 | Server error |

### API: deduplicate()

**Purpose**: Check if signal is a duplicate and skip if so.

**Endpoint**: `POST /signals/deduplicate`

**Request**

```json
{
  "signal_id": "sig-uuid-5678",
  "signal_type": "cost_spike",
  "resource_id": "projects/123456"
}
```

**Response (Not Duplicate)**

```json
{
  "is_duplicate": false,
  "cache_key": "cost_spike::projects/123456",
  "ttl_seconds": 300,
  "status": "success"
}
```

**Response (Duplicate)**

```json
{
  "is_duplicate": true,
  "first_seen_at": "2026-01-25T10:30:40Z",
  "duplicate_count": 3,
  "cache_key": "cost_spike::projects/123456",
  "ttl_seconds": 260,
  "status": "success"
}
```

### API: validate()

**Purpose**: Validate signal against JSON schema and domain constraints.

**Endpoint**: `POST /signals/validate`

**Request**

```json
{
  "signal": {
    "signal_type": "cost_spike",
    "normalized_value": 156.0,
    "baseline": 100.0,
    "threshold": 150.0
  }
}
```

**Response (Valid)**

```json
{
  "valid": true,
  "violations": [],
  "status": "success"
}
```

**Response (Invalid)**

```json
{
  "valid": false,
  "violations": [
    {
      "field": "normalized_value",
      "constraint": "range",
      "message": "Value must be > 0",
      "expected": "> 0",
      "actual": -5.0
    }
  ],
  "status": "error"
}
```

### API: enrich()

**Purpose**: Enrich signal with ontology-based knowledge (SKU, expected actions).

**Endpoint**: `POST /signals/enrich`

**Request**

```json
{
  "signal_type": "cost_spike",
  "normalized_value": 156.0,
  "baseline": 100.0
}
```

**Response**

```json
{
  "signal_type": "cost_spike",
  "triggered_skus": [
    {
      "sku_name": "cost-circuit-breaker-p1",
      "sku_tier": "P1",
      "confidence": 0.95,
      "expected_action": "scale_down",
      "preconditions": [
        "current_instances > 2",
        "billing_api_accessible"
      ],
      "postconditions": [
        "max_instances == 2",
        "cost_reduced_within_1h"
      ]
    }
  ],
  "inferred_rules": [
    {
      "rule": "cost_spike → throttle",
      "source": "erlang-autonomics.ttl",
      "confidence": 0.98
    }
  ],
  "status": "success"
}
```

---

## Governor API

The Governor service evaluates FSM state transitions and plans corrective actions.

### Service Endpoint

```
https://governor-service-[hash].run.app
```

### API: get_state()

**Purpose**: Retrieve current FSM state for a governor instance.

**Endpoint**: `GET /governor/{governor_id}/state`

**Response**

```json
{
  "governor_id": "gov-cost-breaker-001",
  "sku": "cost-circuit-breaker-p1",
  "current_state": "Nominal",
  "state_entered_at": "2026-01-25T09:00:00Z",
  "time_in_state_seconds": 5400,
  "signal_history": [
    {
      "signal_type": "cost_spike",
      "value": 120,
      "received_at": "2026-01-25T10:00:00Z"
    },
    {
      "signal_type": "cost_spike",
      "value": 160,
      "received_at": "2026-01-25T10:30:45Z"
    }
  ],
  "last_action": {
    "action_id": "act-uuid-5678",
    "action_type": "scale_down",
    "executed_at": "2026-01-25T10:30:47Z",
    "result": "success"
  },
  "status": "success"
}
```

### API: evaluate()

**Purpose**: Evaluate FSM transition rules given current state and signal.

**Endpoint**: `POST /governor/{governor_id}/evaluate`

**Request**

```json
{
  "signal": {
    "signal_id": "sig-uuid-5678",
    "signal_type": "cost_spike",
    "normalized_value": 156.0,
    "baseline": 100.0,
    "threshold": 150.0
  }
}
```

**Response**

```json
{
  "governor_id": "gov-cost-breaker-001",
  "signal_id": "sig-uuid-5678",
  "current_state": "Nominal",
  "applicable_transitions": [
    {
      "from_state": "Nominal",
      "to_state": "Alert",
      "trigger": "cost_spike",
      "guard_condition": "value > threshold",
      "guard_result": true,
      "priority": 1,
      "applicable": true
    }
  ],
  "recommended_transition": {
    "from_state": "Nominal",
    "to_state": "Alert",
    "confidence": 0.99,
    "reasoning": "Cost threshold exceeded: 156% > 150%"
  },
  "status": "success"
}
```

### API: transition()

**Purpose**: Execute FSM state transition and plan action.

**Endpoint**: `POST /governor/{governor_id}/transition`

**Request**

```json
{
  "signal": {
    "signal_id": "sig-uuid-5678",
    "signal_type": "cost_spike",
    "normalized_value": 156.0
  },
  "target_state": "Alert"
}
```

**Response**

```json
{
  "governor_id": "gov-cost-breaker-001",
  "signal_id": "sig-uuid-5678",
  "state_before": "Nominal",
  "state_after": "Alert",
  "transition_timestamp": "2026-01-25T10:30:46Z",
  "planned_actions": [
    {
      "action_id": "act-uuid-5678",
      "action_type": "scale_down",
      "service": "api",
      "parameters": {
        "min_instances": 1,
        "max_instances": 2
      },
      "reason": "Cost spike detected: throttle scaling",
      "preconditions_met": true
    }
  ],
  "state_updated_at": "2026-01-25T10:30:46.123Z",
  "status": "success"
}
```

### API: check_invariant()

**Purpose**: Verify FSM invariants are maintained.

**Endpoint**: `GET /governor/{governor_id}/invariants`

**Response (All Valid)**

```json
{
  "governor_id": "gov-cost-breaker-001",
  "invariants_checked": 8,
  "invariants_valid": 8,
  "invariants_violated": 0,
  "checks": [
    {
      "invariant": "current_state_is_valid",
      "valid": true,
      "message": "Current state 'Alert' is a valid state"
    },
    {
      "invariant": "state_transition_allowed",
      "valid": true,
      "message": "Transition Nominal→Alert is allowed"
    },
    {
      "invariant": "no_orphaned_signals",
      "valid": true,
      "message": "All signals in history have resolution"
    },
    {
      "invariant": "time_monotonic",
      "valid": true,
      "message": "All timestamps are monotonically increasing"
    }
  ],
  "status": "success"
}
```

**Response (Violation Detected)**

```json
{
  "governor_id": "gov-cost-breaker-001",
  "invariants_checked": 8,
  "invariants_valid": 7,
  "invariants_violated": 1,
  "violations": [
    {
      "invariant": "state_timeout",
      "valid": false,
      "message": "State 'Alert' has been active for 2 hours (max: 1 hour)",
      "severity": "warning"
    }
  ],
  "status": "error"
}
```

---

## Actuator API

The Actuator service executes GCP API calls and verifies outcomes.

### Service Endpoint

```
https://actuator-service-[hash].run.app
```

### API: execute()

**Purpose**: Execute an action plan (GCP API call).

**Endpoint**: `POST /actions/execute`

**Request**

```json
{
  "action": {
    "action_id": "act-uuid-5678",
    "action_type": "scale_cloud_run",
    "service": "api",
    "parameters": {
      "min_instances": 1,
      "max_instances": 2
    },
    "preconditions": [
      "service_exists",
      "current_instances > 2"
    ]
  }
}
```

**Response (Success)**

```json
{
  "action_id": "act-uuid-5678",
  "status": "success",
  "execution": {
    "service": "api",
    "action_type": "scale_cloud_run",
    "gcp_operation": "UpdateService",
    "gcp_request_id": "req-uuid-9999",
    "gcp_response_code": 200,
    "started_at": "2026-01-25T10:30:47Z",
    "completed_at": "2026-01-25T10:30:49Z",
    "duration_ms": 2100
  },
  "verification": {
    "verified": true,
    "check_type": "GET_service",
    "expected": {
      "max_instances": 2
    },
    "actual": {
      "max_instances": 2
    },
    "matches": true
  },
  "outcome": {
    "service": "api",
    "change_before": {
      "max_instances": 10,
      "current_instances": 8
    },
    "change_after": {
      "max_instances": 2,
      "current_instances": 2
    }
  }
}
```

**Response (Failure)**

```json
{
  "action_id": "act-uuid-5678",
  "status": "failure",
  "error": {
    "code": "PERMISSION_DENIED",
    "message": "Service account lacks compute.instances.get permission",
    "details": "Required IAM role: roles/compute.osLogin"
  },
  "execution": {
    "service": "api",
    "action_type": "scale_cloud_run",
    "attempts": 3,
    "last_attempt_at": "2026-01-25T10:30:49.500Z",
    "total_duration_ms": 6300
  },
  "recovery_suggestion": "Grant service account the following roles: roles/run.admin, roles/compute.osLogin"
}
```

### API: rollback()

**Purpose**: Rollback service to previous revision.

**Endpoint**: `POST /actions/rollback`

**Request**

```json
{
  "action": {
    "action_id": "act-uuid-9999",
    "action_type": "rollback_cloud_run",
    "service": "api",
    "parameters": {
      "target_revision": "rev-5"
    }
  }
}
```

**Response**

```json
{
  "action_id": "act-uuid-9999",
  "status": "success",
  "execution": {
    "service": "api",
    "current_revision_before": "rev-6",
    "target_revision": "rev-5",
    "traffic_routing": "100%->rev-5",
    "started_at": "2026-01-25T10:30:47Z",
    "completed_at": "2026-01-25T10:30:49Z"
  },
  "verification": {
    "verified": true,
    "revision_active": "rev-5",
    "revision_deleted": "rev-6",
    "error_rate_after_rollback": 0.8,
    "error_rate_before_rollback": 8.5,
    "latency_after_rollback_ms": 125,
    "latency_before_rollback_ms": 850
  },
  "zero_downtime_verified": true
}
```

### API: verify()

**Purpose**: Verify action outcome by checking actual GCP state.

**Endpoint**: `POST /actions/{action_id}/verify`

**Request**

```json
{
  "expected_state": {
    "max_instances": 2,
    "min_instances": 1
  },
  "timeout_seconds": 30
}
```

**Response**

```json
{
  "action_id": "act-uuid-5678",
  "verified": true,
  "verification_timestamp": "2026-01-25T10:30:50Z",
  "checks": [
    {
      "check": "max_instances == 2",
      "expected": 2,
      "actual": 2,
      "match": true
    },
    {
      "check": "min_instances == 1",
      "expected": 1,
      "actual": 1,
      "match": true
    }
  ],
  "all_checks_passed": true
}
```

---

## Receipt Ledger API

The Receipt Ledger service creates and verifies cryptographic audit trails.

### Service Endpoint

```
https://receipt-ledger-service-[hash].run.app
```

### API: emit()

**Purpose**: Create and store a cryptographic receipt.

**Endpoint**: `POST /receipts/emit`

**Request**

```json
{
  "action_result": {
    "action_id": "act-uuid-5678",
    "action_type": "scale_cloud_run",
    "status": "success",
    "service": "api",
    "change": {
      "before": { "max_instances": 10 },
      "after": { "max_instances": 2 }
    },
    "executed_at": "2026-01-25T10:30:47Z",
    "duration_ms": 2100
  },
  "audit_context": {
    "operator": "autonomic-governor",
    "reason": "Cost circuit breaker activated",
    "tags": ["cost-control", "auto-healing"]
  }
}
```

**Response**

```json
{
  "receipt": {
    "execution_id": "exec-uuid-9999",
    "timestamp": "2026-01-25T10:30:50.123Z",
    "action": "scale_cloud_run",
    "result": "success",
    "service": "api",
    "change_summary": "max_instances: 10 → 2",
    "duration_ms": 2100
  },
  "cryptography": {
    "content_hash": "sha256:d1b2c3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0",
    "receipt_hash": "sha256:abc123def456ghi789jkl012mno345pqr678stu901",
    "previous_receipt_hash": "sha256:999888777666555444333222111000aaabbbcccddd",
    "chain_valid": true
  },
  "storage": {
    "bigquery_inserted": true,
    "bigquery_table": "autonomic.receipt_ledger",
    "gcs_archived": true,
    "gcs_path": "gs://autonomic-audit-trail/2026/01/25/exec-uuid-9999.json",
    "gcs_crc32c": "abcd1234"
  },
  "status": "success"
}
```

### API: verify_chain()

**Purpose**: Verify receipt authenticity and detect tampering.

**Endpoint**: `POST /receipts/{execution_id}/verify`

**Request**

```json
{
  "execution_id": "exec-uuid-9999"
}
```

**Response (Valid)**

```json
{
  "execution_id": "exec-uuid-9999",
  "verified": true,
  "verification_checks": [
    {
      "check": "receipt_hash_matches",
      "status": "pass",
      "expected_hash": "sha256:abc123...",
      "computed_hash": "sha256:abc123...",
      "match": true
    },
    {
      "check": "chain_continuity",
      "status": "pass",
      "previous_receipt": "exec-uuid-9998",
      "chain_valid": true
    },
    {
      "check": "gcs_archive_integrity",
      "status": "pass",
      "crc32c_match": true
    }
  ],
  "integrity": {
    "tamper_detected": false,
    "archive_original_hash": "sha256:d1b2c3e4...",
    "current_hash": "sha256:d1b2c3e4...",
    "hashes_match": true
  },
  "status": "success"
}
```

**Response (Tampering Detected)**

```json
{
  "execution_id": "exec-uuid-9999",
  "verified": false,
  "verification_checks": [
    {
      "check": "receipt_hash_matches",
      "status": "fail",
      "expected_hash": "sha256:abc123...",
      "computed_hash": "sha256:xyz789...",
      "match": false
    }
  ],
  "integrity": {
    "tamper_detected": true,
    "archive_original_hash": "sha256:d1b2c3e4...",
    "current_hash": "sha256:999999...",
    "hashes_match": false,
    "alert": "RECEIPT MODIFIED: Hash mismatch detected"
  },
  "status": "error",
  "recommendations": [
    "Investigate receipt modification",
    "Check access logs for unauthorized changes",
    "Verify service account permissions"
  ]
}
```

### API: query_ledger()

**Purpose**: Query receipt ledger with filters.

**Endpoint**: `GET /receipts/ledger?filters`

**Query Parameters**

```
action=scale_cloud_run              # Filter by action type
service=api                          # Filter by service
result=success|failure               # Filter by result
start_time=2026-01-25T00:00:00Z     # Time range
end_time=2026-01-25T23:59:59Z
limit=100                            # Max results
offset=0                             # Pagination
```

**Response**

```json
{
  "query": {
    "filters": {
      "action": "scale_cloud_run",
      "service": "api",
      "result": "success",
      "time_range": "2026-01-25"
    },
    "limit": 100,
    "offset": 0
  },
  "results": {
    "total_count": 42,
    "returned_count": 20,
    "receipts": [
      {
        "execution_id": "exec-uuid-1001",
        "timestamp": "2026-01-25T10:30:50.123Z",
        "action": "scale_cloud_run",
        "result": "success",
        "service": "api",
        "change_summary": "max_instances: 10 → 2",
        "receipt_hash": "sha256:abc123...",
        "verified": true
      }
    ]
  },
  "status": "success"
}
```

---

## Domain Types

### Signal Type

```rust
pub struct Signal {
    pub signal_id: String,                      // Unique ID
    pub signal_type: SignalType,                // cost_spike, error_rate_spike, etc.
    pub normalized_value: f64,                  // Normalized metric value
    pub baseline: f64,                          // Baseline for comparison
    pub threshold: f64,                         // Alert threshold
    pub triggered_sku: Option<String>,          // Which SKU was triggered
    pub expected_action: Option<String>,        // Expected action type
    pub confidence: f64,                        // 0.0-1.0 confidence score
    pub received_at: DateTime<Utc>,             // When signal was received
    pub source_trace_id: String,                // Distributed trace ID
}

pub enum SignalType {
    CostSpike,
    ErrorRateSpike,
    DeploymentFailure,
    ResourceDeadlock,
    TrafficSpike,
    ConnectionPoolExhaustion,
    // ... more signal types
}
```

### Action Type

```rust
pub struct Action {
    pub action_id: String,                      // Unique ID
    pub action_type: ActionType,                // scale, rollback, etc.
    pub service: String,                        // Target service
    pub parameters: serde_json::Value,          // Action-specific params
    pub preconditions: Vec<String>,             // Must be true before execute
    pub postconditions: Vec<String>,            // Should be true after execute
    pub reason: String,                         // Why this action
    pub triggered_by_signal: String,            // Which signal triggered
}

pub enum ActionType {
    ScaleCloudRun,
    ScaleGke,
    RollbackCloudRun,
    RollbackGke,
    RateLimitTraffic,
    TriggerGarbageCollection,
    EnableCdnCaching,
    // ... more action types
}
```

### FSM State

```rust
pub struct FsmState {
    pub governor_id: String,
    pub current_state: State,
    pub state_entered_at: DateTime<Utc>,
    pub signal_history: Vec<Signal>,
    pub action_history: Vec<Action>,
    pub last_transition_reason: String,
}

pub enum State {
    Nominal,
    Alert,
    Throttled,
    Recovery,
    Degraded,
    Critical,
    RollingBack,
    Stable,
    // ... more states
}
```

### Receipt

```rust
pub struct Receipt {
    pub execution_id: String,                   // Unique ID
    pub timestamp: DateTime<Utc>,               // When executed
    pub action: String,                         // Action type
    pub result: ActionResult,                   // success|failure
    pub service: Option<String>,                // Affected service
    pub change_summary: String,                 // What changed
    pub duration_ms: u64,                       // Execution time
    pub content_hash: String,                   // SHA256(action_result)
    pub receipt_hash: String,                   // SHA256(receipt)
    pub previous_receipt_hash: Option<String>,  // Chain link
    pub audit_trail_path: String,               // GCS path
}

pub enum ActionResult {
    Success(SuccessDetails),
    Failure(FailureDetails),
}
```

---

## Error Handling

All API responses include error details:

### Error Response Format

```json
{
  "error": "error_code",
  "message": "Human-readable error description",
  "details": {
    "field": "optional field name",
    "constraint": "constraint that failed",
    "expected": "what we expected",
    "actual": "what we got"
  },
  "trace_id": "trace-uuid-1234",
  "status": "error"
}
```

### Common Error Codes

| Code | HTTP | Description | Recovery |
|------|------|-------------|----------|
| `validation_failed` | 400 | Input validation failed | Check request format |
| `unknown_signal_type` | 400 | Signal type not recognized | Use valid signal type |
| `service_not_found` | 404 | GCP service not found | Verify service exists |
| `permission_denied` | 403 | Service account lacks IAM role | Grant required role |
| `resource_exhausted` | 429 | Rate limit exceeded | Implement backoff |
| `deadline_exceeded` | 504 | Request timed out | Retry with longer timeout |
| `internal_error` | 500 | Server error | Check logs, retry |
| `unavailable` | 503 | Service temporarily unavailable | Retry with exponential backoff |

### Retry Strategy

```
Max retries: 3
Backoff: exponential
  Attempt 1: 1 second
  Attempt 2: 2 seconds
  Attempt 3: 4 seconds

Retryable errors: 429, 500, 503, 504
Non-retryable: 400, 403, 404

Timeout per request: 30 seconds
```

---

## Examples

### Complete Signal → Action → Receipt Flow

```bash
# 1. Publish billing spike to Cloud Events
curl -X POST \
  -H "Content-Type: application/json" \
  https://signal-ingest-service.run.app/signals/normalize \
  -d '{
    "source": "logging.googleapis.com",
    "type": "billing.threshold.exceeded",
    "data": {
      "resource_id": "projects/my-project",
      "metric_value": 160,
      "baseline": 100,
      "unit": "percent"
    }
  }'

# Response:
# {
#   "signal_id": "sig-uuid-1234",
#   "signal_type": "cost_spike",
#   "triggered_sku": "cost-circuit-breaker-p1"
# }

# 2. Governor evaluates state transition
curl -X POST \
  https://governor-service.run.app/governor/gov-001/transition \
  -d '{
    "signal": {
      "signal_id": "sig-uuid-1234",
      "signal_type": "cost_spike",
      "normalized_value": 160
    }
  }'

# Response:
# {
#   "state_before": "Nominal",
#   "state_after": "Alert",
#   "planned_actions": [{
#     "action_type": "scale_down",
#     "service": "api",
#     "parameters": { "max_instances": 2 }
#   }]
# }

# 3. Actuator executes action
curl -X POST \
  https://actuator-service.run.app/actions/execute \
  -d '{
    "action": {
      "action_type": "scale_cloud_run",
      "service": "api",
      "parameters": { "max_instances": 2 }
    }
  }'

# Response:
# {
#   "status": "success",
#   "execution": { "gcp_response_code": 200 }
# }

# 4. Receipt Ledger creates proof
curl -X POST \
  https://receipt-ledger-service.run.app/receipts/emit \
  -d '{
    "action_result": {
      "action_type": "scale_cloud_run",
      "status": "success",
      "service": "api"
    }
  }'

# Response:
# {
#   "receipt": {
#     "execution_id": "exec-uuid-5678",
#     "receipt_hash": "sha256:abc123..."
#   }
# }

# 5. Query receipt ledger
curl -X GET \
  'https://receipt-ledger-service.run.app/receipts/ledger?service=api&action=scale_cloud_run'

# Response shows audit trail of all actions taken
```

---

**Last Updated**: January 2026 | **API Version**: 1.0 | **Status**: Production-Ready ✓
