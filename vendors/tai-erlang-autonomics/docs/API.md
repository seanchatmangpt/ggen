# TAI Autonomics API Reference

**Status**: Production Ready
**Phase**: Phase 1 (MCP-only support)
**Last Updated**: 2026-01-26

---

## Overview

TAI Autonomics exposes functionality through:
1. **HTTP endpoints** - Health checks, event ingestion
2. **MCP tools** - Programmatic support via Model Context Protocol

---

## HTTP Endpoints

### `/health`

System health check endpoint.

**Method**: `GET`
**Authentication**: None required
**Rate Limit**: Unlimited (for monitoring)

**Response** (200 OK):
```json
{
  "status": "ok",
  "node": "tai@container-id",
  "uptime_ms": 45000,
  "timestamp": "2026-01-26T14:30:45.123Z",
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "ok"
  }
}
```

**Example**:
```bash
curl http://localhost:8080/health
```

**Use Case**: Kubernetes liveness/readiness probes, monitoring dashboards, pre-flight checks.

---

### `/marketplace`

Marketplace event ingestion endpoint.

**Method**: `POST`
**Authentication**: API key (if configured; default: none in Phase 1)
**Content-Type**: `application/json`
**Rate Limit**: 100 req/s per tenant

**Request**:
```json
{
  "event": "sku_inquiry|sku_activate|sku_upgrade|feature_request",
  "sku_id": "autonomic-v1",
  "tenant_id": "acme-corp",
  "data": {
    "feature": "report-gen",
    "tier": "professional",
    "reason": "customer_requested"
  }
}
```

**Response** (200 OK):
```json
{
  "status": "accepted",
  "event_id": "evt-uuid",
  "receipt": {
    "id": "receipt-uuid",
    "timestamp": "2026-01-26T14:30:45.123Z",
    "status": "success",
    "message": "Event received and queued for processing"
  }
}
```

**Response** (422 Unprocessable Entity - Refused):
```json
{
  "status": "refused",
  "reason": "entitlement_gate_failed",
  "message": "Tenant quota exceeded for SKU 'autonomic-v1'",
  "next_step": "Upgrade tier or contact sales@taiautonomics.io",
  "receipt": {
    "id": "receipt-uuid",
    "timestamp": "2026-01-26T14:30:45.123Z",
    "status": "refused",
    "metadata": {
      "tenant_quota": "10",
      "tenant_usage": "10",
      "gate_failed": "quota_gate"
    }
  }
}
```

**Example**:
```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "event": "feature_request",
    "sku_id": "autonomic-v1",
    "tenant_id": "acme-corp",
    "data": {
      "feature": "report-gen",
      "reason": "customer_requested"
    }
  }'
```

**Use Case**: Marketplace submissions, feature activation requests, SKU management.

---

### `/pubsub`

GCP Pub/Sub push endpoint (webhook receiver).

**Method**: `POST`
**Authentication**: GCP Cloud Pub/Sub authentication
**Content-Type**: `application/json`
**URL**: Automatically configured by Terraform (deployed only in Cloud Run)

**Request** (Pub/Sub wrapper):
```json
{
  "message": {
    "data": "base64-encoded-event-json",
    "messageId": "msg-uuid",
    "publishTime": "2026-01-26T14:30:45.123Z"
  },
  "subscription": "projects/your-project/subscriptions/tai-events"
}
```

**Decoded event data**:
```json
{
  "event": "autonomic_signal",
  "signal_type": "quota_approaching|capacity_alert|failure_detected",
  "tenant_id": "acme-corp",
  "severity": "info|warning|error"
}
```

**Response** (200 OK):
```json
{
  "status": "ack",
  "message_id": "msg-uuid",
  "receipt": {
    "id": "receipt-uuid",
    "timestamp": "2026-01-26T14:30:45.123Z",
    "status": "success"
  }
}
```

**Example** (publish to topic):
```bash
gcloud pubsub topics publish tai-events \
  --message '{"event":"autonomic_signal","signal_type":"capacity_alert"}'

# Service will receive via /pubsub endpoint
```

**Use Case**: Autonomous signal propagation, event-driven remediation, multi-region coordination.

---

## MCP Tools

TAI Autonomics exposes four MCP tools for programmatic support.

### Tool Registration

All tools are registered with the MCP server on startup:

```erlang
taiea_mcp_server:register_tools([
  {taiea_tool_health, "taiea.health.check"},
  {taiea_tool_entitlement, "taiea.entitlement.apply_event"},
  {taiea_tool_receipts, "taiea.receipts.verify_chain"},
  {taiea_tool_support, "taiea.support.model"}
])
```

---

### `taiea.health.check`

Check system health and dependencies.

**Input**:
```json
{}
```

**Output** (Success):
```json
{
  "status": "ok",
  "node": "tai@localhost",
  "uptime_ms": 12345,
  "timestamp": "2026-01-26T14:30:45.123Z",
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "ok"
  }
}
```

**Output** (Error):
```json
{
  "status": "error",
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "error: connection_refused"
  },
  "message": "One or more health checks failed"
}
```

**Receipt**: Always generated (success or error)

**Use Case**: Pre-flight validation, readiness checks, dependency verification.

**Example via curl** (if MCP server exposes HTTP):
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.health.check",
    "input": {}
  }'
```

---

### `taiea.entitlement.apply_event`

Apply entitlement state changes.

**Input**:
```json
{
  "tenant_id": "acme-corp",
  "event_type": "feature_activate|tier_upgrade|quota_increase",
  "event_data": {
    "feature": "report-gen",
    "new_tier": "professional",
    "new_quota": 50,
    "reason": "customer_requested"
  }
}
```

**Output** (Success - Feature Activated):
```json
{
  "decision": "approved",
  "tenant_id": "acme-corp",
  "event_type": "feature_activate",
  "feature": "report-gen",
  "new_state": "active",
  "previous_state": "inactive",
  "timestamp": "2026-01-26T14:30:45.123Z",
  "gates_passed": [
    "entitlement_gate",
    "quota_gate",
    "security_gate"
  ],
  "action_duration_ms": 89
}
```

**Output** (Refused - Gate Failed):
```json
{
  "decision": "refused",
  "tenant_id": "acme-corp",
  "event_type": "feature_activate",
  "feature": "report-gen",
  "reason": "quota_gate_failed",
  "message": "Tenant quota exceeded for SKU 'autonomic-v1'",
  "next_step": "Upgrade tier or contact sales@taiautonomics.io",
  "gate_failed": "quota_gate",
  "timestamp": "2026-01-26T14:30:45.123Z",
  "metadata": {
    "tenant_quota": "10",
    "tenant_usage": "10",
    "required_quota": "15"
  }
}
```

**Receipt**: Always generated (approved, refused, or error)

**Use Case**: Feature activation, tier upgrades, quota adjustments, entitlement validation.

**Example**:
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.entitlement.apply_event",
    "input": {
      "tenant_id": "acme-corp",
      "event_type": "feature_activate",
      "event_data": {
        "feature": "report-gen",
        "reason": "customer_requested"
      }
    }
  }'
```

---

### `taiea.receipts.verify_chain`

Verify receipt chain integrity and export for audit.

**Input**:
```json
{
  "tenant_id": "acme-corp",
  "receipt_id": "receipt-uuid-optional"
}
```

If `receipt_id` is omitted, returns all receipts for tenant.

**Output** (Success - Single Receipt):
```json
{
  "receipt_id": "550e8400-e29b-41d4-a716-446655440000",
  "tenant_id": "acme-corp",
  "timestamp": "2026-01-26T14:30:45.123Z",
  "status": "success",
  "event": "feature_activation",
  "tool": "taiea.entitlement.apply_event",
  "message": "Feature 'report-gen' activated",
  "verification_status": "valid",
  "chain_valid": true,
  "hash": "sha256:abc123def456...",
  "previous_hash": "sha256:xyz789...",
  "next_hash": "sha256:def456...",
  "signature": "ed25519:...",
  "metadata": {
    "feature": "report-gen",
    "gates_passed": ["entitlement", "quota", "security"],
    "action_duration_ms": 89
  }
}
```

**Output** (Success - Chain Export):
```json
{
  "tenant_id": "acme-corp",
  "total_receipts": 42,
  "time_range": {
    "start": "2026-01-01T00:00:00Z",
    "end": "2026-01-26T14:30:45Z"
  },
  "chain_status": "valid",
  "receipts": [
    { "receipt_id": "...", "timestamp": "...", "event": "...", ... },
    { "receipt_id": "...", "timestamp": "...", "event": "...", ... }
  ],
  "export_format": "json",
  "can_export_csv": true
}
```

**Output** (Error - Receipt Not Found):
```json
{
  "status": "error",
  "receipt_id": "invalid-uuid",
  "tenant_id": "acme-corp",
  "message": "Receipt not found",
  "verification_status": "not_found"
}
```

**Receipt**: Always generated (verification success, error, etc.)

**Use Case**: Audit trail verification, compliance exports, receipt chain validation.

**Example - Single Receipt**:
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.receipts.verify_chain",
    "input": {
      "tenant_id": "acme-corp",
      "receipt_id": "550e8400-e29b-41d4-a716-446655440000"
    }
  }'
```

**Example - Chain Export (All Receipts)**:
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.receipts.verify_chain",
    "input": {
      "tenant_id": "acme-corp"
    }
  }'
```

---

### `taiea.support.model`

Retrieve support model configuration and SLA details.

**Input**:
```json
{}
```

**Output**:
```json
{
  "support_model": "mcp_only_tcps",
  "phase": "phase_1_evaluation",
  "version": "1.0",
  "current_tier": "evaluation",
  "timestamp": "2026-01-26T14:30:45.123Z",
  "tiers": [
    {
      "name": "evaluation",
      "response_time": "best_effort",
      "uptime_sla": "none",
      "features": [
        "mcp_tools",
        "receipt_generation",
        "stdout_logging"
      ],
      "available_in_phase": "phase_1"
    },
    {
      "name": "standard",
      "response_time": "2h",
      "uptime_sla": "99.5%",
      "features": [
        "mcp_tools",
        "receipt_generation",
        "firestore_persistence",
        "ops_team"
      ],
      "available_in_phase": "phase_2",
      "estimated_launch": "2026-04-01"
    }
  ],
  "capabilities": {
    "deterministic_remediation": true,
    "stop_line_safety": true,
    "receipt_verification": true,
    "multi_tenant_billing": false,
    "custom_recipes": false,
    "human_engineering": false
  },
  "slo": {
    "phase": "phase_1",
    "health_check_latency_ms": 50,
    "entitlement_decision_ms": 100,
    "receipt_verification_ms": 150
  },
  "phase_2_estimated": "2026-04-01",
  "contact": {
    "sales": "sales@taiautonomics.io",
    "support_method": "mcp_tools_only"
  }
}
```

**Receipt**: Always generated (success)

**Use Case**: Support model reference, SLA verification, tier lookup, roadmap information.

**Example**:
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.support.model",
    "input": {}
  }'
```

---

## Request/Response Schemas

### Health Check Response
```json
{
  "status": "ok|error",
  "node": "string (Erlang node identifier)",
  "uptime_ms": "integer (milliseconds)",
  "timestamp": "ISO-8601 timestamp",
  "checks": {
    "[system]": "ok|error"
  }
}
```

### Entitlement Event Request
```json
{
  "tenant_id": "string (required)",
  "event_type": "feature_activate|tier_upgrade|quota_increase",
  "event_data": {
    "feature": "string (optional)",
    "new_tier": "string (optional)",
    "new_quota": "integer (optional)",
    "reason": "string (optional)"
  }
}
```

### Entitlement Event Response
```json
{
  "decision": "approved|refused|error",
  "tenant_id": "string",
  "event_type": "string",
  "new_state": "string (if approved)",
  "reason": "string (if refused)",
  "next_step": "string (if refused)",
  "gates_passed": ["string"],
  "gate_failed": "string (if refused)",
  "timestamp": "ISO-8601 timestamp",
  "metadata": { }
}
```

### Receipt Schema
```json
{
  "id": "UUID",
  "timestamp": "ISO-8601 timestamp",
  "event": "string (event name)",
  "status": "success|error|refused",
  "tool": "string (tool name)",
  "message": "string (human-readable)",
  "metadata": {
    "tenant_id": "string (optional)",
    "receipt_chain_hash": "string (SHA-256)",
    "action_duration_ms": "integer",
    "gates_passed": ["string"],
    "gate_failed": "string (optional)"
  }
}
```

---

## Error Handling

### HTTP Status Codes
- **200 OK**: Request succeeded; receipt generated
- **400 Bad Request**: Malformed input (missing required fields)
- **401 Unauthorized**: Authentication failed (Phase 2+)
- **422 Unprocessable Entity**: Request valid but refused by gate (safe refusal)
- **500 Internal Server Error**: System error; receipt with error status
- **503 Service Unavailable**: Dependencies offline; check `/health`

### Error Response Format
```json
{
  "status": "error",
  "message": "Human-readable error message",
  "error_code": "string (error_code)",
  "details": { },
  "receipt": {
    "id": "receipt-uuid",
    "timestamp": "ISO-8601 timestamp",
    "status": "error"
  }
}
```

### Common Error Codes
- `bad_request` - Malformed JSON or missing required field
- `auth_required` - Authentication required (Phase 2+)
- `quota_exceeded` - Tenant quota exceeded
- `gate_failed` - Safety gate rejected request
- `dependency_error` - Service dependency unavailable
- `internal_error` - System error

---

## Rate Limiting (Phase 1)

No rate limiting in Phase 1. Rate limits planned for Phase 2:

| Endpoint | Limit | Window |
|----------|-------|--------|
| `/health` | Unlimited | - |
| `/marketplace` | 100 req/s | Per tenant |
| `/pubsub` | Unlimited | - (GCP managed) |
| MCP tools | 100 req/s | Per tenant |

---

## Curl Examples

### Health Check
```bash
curl -v http://localhost:8080/health
```

### Marketplace Event (Feature Activation)
```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "event": "feature_request",
    "sku_id": "autonomic-v1",
    "tenant_id": "acme-corp",
    "data": {
      "feature": "report-gen",
      "reason": "customer_requested"
    }
  }' | jq .
```

### Marketplace Event (Tier Upgrade)
```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "event": "sku_upgrade",
    "sku_id": "autonomic-v1",
    "tenant_id": "acme-corp",
    "data": {
      "new_tier": "professional",
      "reason": "customer_requested"
    }
  }' | jq .
```

### MCP Tool: Health Check (via HTTP)
```bash
# Requires MCP server running on port 3000
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.health.check",
    "input": {}
  }' | jq .
```

### MCP Tool: Entitlement Event (via HTTP)
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.entitlement.apply_event",
    "input": {
      "tenant_id": "acme-corp",
      "event_type": "feature_activate",
      "event_data": {
        "feature": "report-gen",
        "reason": "customer_requested"
      }
    }
  }' | jq .
```

### MCP Tool: Receipt Verification
```bash
curl -X POST http://localhost:3000/mcp/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "taiea.receipts.verify_chain",
    "input": {
      "tenant_id": "acme-corp"
    }
  }' | jq .
```

---

## Document Control

| Field | Value |
|-------|-------|
| **Version** | 1.0 |
| **Audience** | Developers, integrators, API users |
| **Last Review** | 2026-01-26 |
| **Next Review** | 2026-04-01 (Phase 2 update) |
