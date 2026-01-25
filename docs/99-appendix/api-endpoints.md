# API Endpoints (CANONICAL)

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

> **CRITICAL**: This is the CANONICAL API reference. All agents and systems MUST reference this document. No paraphrasing, no duplication.

## Table of Contents
1. [Overview](#overview)
2. [Authentication](#authentication)
3. [Endpoints](#endpoints)
4. [Request/Response Schemas](#requestresponse-schemas)
5. [Error Handling](#error-handling)
6. [Examples](#examples)

---

## Overview

ggen provides 4 main HTTP endpoints:
1. **Signal Ingestion** - Receive customer signals (launch, update, terminate, delete)
2. **Entitlement Webhook** - Receive GCP Marketplace entitlement events
3. **Health Check** - Service availability probe
4. **Readiness Probe** - Kubernetes readiness check

**Base URL**: `https://ggen.example.com/api/v1`

**Timeout**: All requests must complete within 30 seconds (client-side enforced).

---

## Authentication

### Webhook Signature Verification (HMAC-SHA256)

**Header**: `X-Ggen-Signature`

**Algorithm**:
```
signature = HMAC-SHA256(request_body, customer_secret_key)
signature_b64 = base64(signature)
header_value = "sha256=" + signature_b64
```

**Example** (Python):
```python
import hmac
import hashlib
import base64

body = b'{"signal_type": "launch"}'
secret = b'customer-secret-key'
signature = hmac.new(secret, body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()
header = f"sha256={signature_b64}"
```

### Service Account Authentication (GCP Internal)

For internal GCP services (not customers):
```
Authorization: Bearer <GCP_ID_TOKEN>
```

---

## Endpoints

### 1. Signal Ingestion

#### POST /signal/{sku_id}/{tenant_id}

**Description**: Ingest customer signal (launch, update, terminate, delete).

**Path Parameters**:
| Name | Type | Required | Format |
|------|------|----------|--------|
| `sku_id` | string | YES | UUID |
| `tenant_id` | string | YES | UUID |

**Request Headers**:
```
Content-Type: application/json
X-Ggen-Signature: sha256=<HMAC-SHA256_BASE64>
X-Ggen-Request-ID: <unique-request-id>  # optional, for idempotency
```

**Request Body Schema**:
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "required": ["signal_type", "entitlement_id"],
  "properties": {
    "signal_type": {
      "type": "string",
      "enum": ["launch", "update", "terminate", "delete"],
      "description": "Operation type"
    },
    "entitlement_id": {
      "type": "string",
      "format": "uuid",
      "description": "GCP Marketplace entitlement ID"
    },
    "parameters": {
      "type": "object",
      "description": "SKU-specific parameters (varies by product)",
      "additionalProperties": true
    },
    "idempotency_key": {
      "type": "string",
      "description": "Idempotency key (optional, prevents duplicate processing)"
    }
  }
}
```

**Example Request**:
```bash
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=abcd1234..." \
  -d '{
    "signal_type": "launch",
    "entitlement_id": "entitlement-uuid-001",
    "parameters": {
      "instance_name": "my-instance-prod",
      "region": "us-central1",
      "machine_type": "n1-standard-4"
    }
  }'
```

**Response Codes**:

| Code | Meaning | Body |
|------|---------|------|
| 202 | Accepted (queued for processing) | Receipt JSON |
| 400 | Bad Request (invalid JSON, missing fields) | Error receipt |
| 401 | Unauthorized (invalid/missing signature) | Error receipt |
| 403 | Forbidden (tenant mismatch) | Error receipt |
| 429 | Rate Limited (quota exceeded) | Error receipt |
| 500 | Internal Server Error | Error receipt |
| 503 | Service Unavailable | Error receipt |

**Response Body** (202 Accepted):
```json
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
    "queue_position": 1234,
    "estimated_processing_time_seconds": 60
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

**Response Body** (400/401/403/429/500):
```json
{
  "kind": "refusal",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "refuse",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "error_code": 400,
    "error_message": "Invalid JSON: missing 'signal_type' field",
    "correlation_id": "corr-uuid-001"
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

---

### 2. Entitlement Webhook

#### POST /entitlement/webhook

**Description**: Receive GCP Marketplace entitlement activation/cancellation events.

**Request Headers**:
```
Content-Type: application/json
X-Gcp-Saas-Entitlement-Id: <entitlement-id>
X-Gcp-Saas-Event-Time: <RFC3339-timestamp>
X-Gcp-Saas-Event-Id: <event-id>
```

**Request Body Schema** (GCP Marketplace format):
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "required": ["entitlement_id", "state"],
  "properties": {
    "entitlement_id": {
      "type": "string",
      "description": "GCP entitlement ID"
    },
    "state": {
      "type": "string",
      "enum": ["ENTITLEMENT_ACTIVE", "ENTITLEMENT_CANCELLED"],
      "description": "Entitlement state change"
    },
    "account_id": {
      "type": "string",
      "description": "Customer's GCP project ID"
    },
    "sku_id": {
      "type": "string",
      "description": "Product SKU identifier"
    },
    "activation_time": {
      "type": "string",
      "format": "date-time",
      "description": "Activation timestamp"
    },
    "expiration_time": {
      "type": "string",
      "format": "date-time",
      "description": "Expiration timestamp"
    }
  }
}
```

**Example Request**:
```bash
curl -X POST https://ggen.example.com/api/v1/entitlement/webhook \
  -H "Content-Type: application/json" \
  -H "X-Gcp-Saas-Entitlement-Id: entitlement-uuid-001" \
  -d '{
    "entitlement_id": "entitlement-uuid-001",
    "state": "ENTITLEMENT_ACTIVE",
    "account_id": "650e8400-e29b-41d4-a716-446655440001",
    "sku_id": "550e8400-e29b-41d4-a716-446655440000",
    "activation_time": "2026-01-25T14:32:00Z",
    "expiration_time": "2027-01-25T14:32:00Z"
  }'
```

**Response Codes**:

| Code | Meaning | Body |
|------|---------|------|
| 200 | OK (webhook processed) | Receipt JSON |
| 400 | Bad Request | Error receipt |
| 500 | Internal Server Error | Error receipt |

**Response Body** (200 OK):
```json
{
  "kind": "entitlement_active",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "sku_id": "550e8400-e29b-41d4-a716-446655440000",
  "account_id": "650e8400-e29b-41d4-a716-446655440001",
  "details": {
    "entitlement_id": "entitlement-uuid-001",
    "activation_ts": "2026-01-25T14:32:00Z",
    "expiration_ts": "2027-01-25T14:32:00Z"
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

---

### 3. Health Check

#### GET /health

**Description**: Check service health and dependency status.

**Request Headers**:
```
Accept: application/json
```

**Example Request**:
```bash
curl https://ggen.example.com/api/v1/health
```

**Response Codes**:

| Code | Meaning |
|------|---------|
| 200 | Healthy (all dependencies OK) |
| 503 | Service Unavailable (dependency down) |

**Response Body** (200 OK):
```json
{
  "kind": "health_check_passed",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "accept",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "details": {
    "dependencies": {
      "firestore": {
        "status": "healthy",
        "latency_ms": 12
      },
      "pubsub": {
        "status": "healthy",
        "latency_ms": 15
      },
      "gcs": {
        "status": "healthy",
        "latency_ms": 8
      }
    },
    "uptime_seconds": 86400,
    "response_time_ms": 45
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

**Response Body** (503 Service Unavailable):
```json
{
  "kind": "health_check_failed",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "unknown",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "details": {
    "dependencies": {
      "firestore": {
        "status": "healthy",
        "latency_ms": 12
      },
      "pubsub": {
        "status": "unhealthy",
        "error": "connection timeout"
      },
      "gcs": {
        "status": "healthy",
        "latency_ms": 8
      }
    },
    "failing_component": "pubsub"
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

---

### 4. Readiness Probe

#### GET /readiness

**Description**: Kubernetes readiness check (simpler than health check, faster).

**Request Headers**:
```
Accept: application/json
```

**Example Request**:
```bash
curl https://ggen.example.com/api/v1/readiness
```

**Response Codes**:

| Code | Meaning |
|------|---------|
| 200 | Ready (can accept requests) |
| 503 | Not Ready |

**Response Body** (200 OK):
```json
{
  "status": "ready",
  "ts": "2026-01-25T14:32:00.000000Z"
}
```

---

## Request/Response Schemas

### Signal Request Schema (OpenAPI 3.1.0)

```yaml
openapi: 3.1.0
info:
  title: ggen Signal API
  version: 6.0.0
paths:
  /signal/{sku_id}/{tenant_id}:
    post:
      summary: Ingest customer signal
      parameters:
        - name: sku_id
          in: path
          required: true
          schema:
            type: string
            format: uuid
        - name: tenant_id
          in: path
          required: true
          schema:
            type: string
            format: uuid
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/Signal'
      responses:
        '202':
          description: Signal accepted
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Receipt'
        '400':
          description: Bad Request
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ErrorReceipt'
        '401':
          description: Unauthorized
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ErrorReceipt'
components:
  schemas:
    Signal:
      type: object
      required:
        - signal_type
        - entitlement_id
      properties:
        signal_type:
          type: string
          enum: [launch, update, terminate, delete]
        entitlement_id:
          type: string
          format: uuid
        parameters:
          type: object
          additionalProperties: true
    Receipt:
      type: object
      required:
        - kind
        - ts
        - decision
        - project_id
        - repo
        - branch
        - details
      properties:
        kind:
          type: string
        ts:
          type: string
          format: date-time
        decision:
          type: string
          enum: [accept, refuse, unknown]
        project_id:
          type: string
        repo:
          type: string
        branch:
          type: string
        details:
          type: object
    ErrorReceipt:
      type: object
      required:
        - kind
        - ts
        - decision
        - details
      properties:
        kind:
          type: string
          const: refusal
        ts:
          type: string
          format: date-time
        decision:
          type: string
          const: refuse
        details:
          type: object
          properties:
            error_code:
              type: integer
            error_message:
              type: string
```

---

## Error Handling

### Error Receipt Structure

All errors return a `refusal` receipt with HTTP status code:

```json
{
  "kind": "refusal",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "refuse",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "details": {
    "error_code": 400,
    "error_message": "Invalid JSON: missing 'signal_type' field",
    "correlation_id": "corr-uuid-001"
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

### Common Error Codes

| Code | Reason | Remediation |
|------|--------|-------------|
| 400 | Bad Request (malformed JSON, missing fields) | Fix request body, retry |
| 401 | Unauthorized (invalid signature) | Verify HMAC-SHA256 signature, check secret key |
| 403 | Forbidden (tenant mismatch) | Verify sku_id and tenant_id match webhook request |
| 429 | Too Many Requests (rate limit exceeded) | Wait `Retry-After` seconds, retry |
| 500 | Internal Server Error (bug in ggen) | Check logs, contact support |
| 503 | Service Unavailable (dependency down) | Retry with exponential backoff (max 5 minutes) |

### Retry Strategy

**Recommended backoff**:
```
attempt_1: immediate retry (500, 503 only)
attempt_2: wait 1 second
attempt_3: wait 2 seconds
attempt_4: wait 4 seconds
attempt_5: wait 8 seconds
max_total_time: 5 minutes
```

---

## Examples

### Example 1: Successful Signal Ingestion

**Request**:
```bash
#!/bin/bash

SECRET="customer-secret-key"
BODY='{"signal_type": "launch", "entitlement_id": "ent-uuid-001", "parameters": {"region": "us-central1"}}'

SIGNATURE=$(echo -n "$BODY" | openssl dgst -sha256 -hmac "$SECRET" -binary | base64)

curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=$SIGNATURE" \
  -d "$BODY"
```

**Response**:
```json
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
    "queue_position": 1234
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

### Example 2: Signature Verification Failure

**Request** (with wrong signature):
```bash
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=invalid_signature" \
  -d '{"signal_type": "launch"}'
```

**Response** (401 Unauthorized):
```json
{
  "kind": "refusal",
  "ts": "2026-01-25T14:32:00.000000Z",
  "decision": "refuse",
  "project_id": "ggen-prod-123456",
  "repo": "github.com/org/ggen",
  "branch": "main",
  "details": {
    "error_code": 401,
    "error_message": "Invalid signature: HMAC verification failed",
    "correlation_id": "corr-uuid-002"
  },
  "prev_chain_hash_b64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
}
```

---

## Receipt Contract

**Every API response MUST**:
- ✅ Be valid JSON
- ✅ Include all required receipt fields (`kind`, `ts`, `decision`, `details`)
- ✅ Use correct HTTP status code (202 for accept, 400/403/429/500 for refuse)
- ✅ Have proper CORS headers (for cross-origin requests)
- ✅ Include `X-Request-ID` header (for tracing)

---

## Definition of Done

- [x] All 4 endpoints documented with method, path, parameters
- [x] Request/response schemas provided (JSON Schema + OpenAPI)
- [x] Authentication (HMAC-SHA256) explained with examples
- [x] All HTTP status codes documented with examples
- [x] Error handling and retry strategy documented
- [x] Complete curl examples provided for each endpoint
- [x] Glossary references included

