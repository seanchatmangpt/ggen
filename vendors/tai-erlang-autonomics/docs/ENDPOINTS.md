# API Endpoints

Complete API reference for TAI Erlang Autonomics HTTP server.

## Overview

The service exposes three primary endpoints for health checks, pub/sub event processing, and marketplace entitlement management. All endpoints use JSON for request/response bodies.

## Base URL

```
http://localhost:8080
https://[cloud-run-service-url]
```

## Authentication

- **Local Development**: No authentication required
- **GCP Production**: OAuth 2.0 via Cloud Run authenticated services
- **Custom API Keys**: Configure via environment variables (optional)

---

## GET /health

Health check endpoint. Returns 200 when all dependencies are ready.

### Request

```bash
curl -X GET http://localhost:8080/health
```

### Response (200 OK)

```json
{
  "status": "ok",
  "timestamp": 1704067200,
  "version": "1.0.0",
  "dependencies": {
    "supervisors": "ok",
    "firestore": "ok",
    "pubsub": "ok"
  }
}
```

### Response (503 Service Unavailable)

```json
{
  "status": "unavailable",
  "timestamp": 1704067200,
  "reason": "supervisor governance_sup not running",
  "dependencies": {
    "supervisors": "error",
    "firestore": "ok",
    "pubsub": "ok"
  }
}
```

### Status Codes

- **200 OK**: All dependencies healthy, ready to accept traffic
- **503 Service Unavailable**: Dependencies not ready (startup, degradation)

### Use Cases

- **Cloud Run Health Probes**: Liveness and readiness checks
- **Load Balancer Checks**: Upstream routing decisions
- **Monitoring**: Health status tracking
- **Startup Validation**: Verify deployment readiness

---

## POST /pubsub

Pub/Sub push handler. Accepts Google Cloud Pub/Sub push envelopes and processes autonomic signals.

### Request Headers

```
Content-Type: application/json
User-Agent: Google-Cloud-Pub/Sub-Push
```

### Request Body

```json
{
  "message": {
    "messageId": "12345678901234567",
    "publishTime": "2024-01-01T12:00:00.000Z",
    "data": "eyJ0ZW5hbnRfaWQiOiAidGVuYW50LTEyMyIsICJhY3Rpb24iOiAiZ3JhbnQifQ==",
    "attributes": {
      "key": "value"
    }
  },
  "subscription": "projects/my-project/subscriptions/erlang-autonomics-signals-sub"
}
```

**Note**: `data` field is base64-encoded JSON:

```json
{
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "metadata": {
    "source": "marketplace",
    "request_id": "req-789"
  }
}
```

### Response (200 OK)

```json
{
  "id": "receipt-abc123def456",
  "type": "transition",
  "timestamp": 1704067200,
  "message_id": "12345678901234567",
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "state_from": "unentitled",
  "state_to": "entitled",
  "hash": "sha256:abcd1234...",
  "prev_hash": "sha256:xyz789...",
  "chain_hash": "sha256:chain123...",
  "metadata": {}
}
```

### Response (400 Bad Request)

```json
{
  "id": "receipt-err123",
  "type": "refusal",
  "timestamp": 1704067200,
  "reason": "invalid_message_format",
  "detail": "Missing required field: tenant_id",
  "hash": "sha256:err456..."
}
```

### Response (503 Service Unavailable)

```json
{
  "id": "receipt-err456",
  "type": "refusal",
  "timestamp": 1704067200,
  "reason": "service_unavailable",
  "detail": "Governor not ready",
  "hash": "sha256:err789..."
}
```

### Status Codes

- **200 OK**: Message processed successfully, receipt emitted
- **400 Bad Request**: Invalid message format, missing fields
- **401 Unauthorized**: Authentication failed
- **409 Conflict**: Idempotent conflict (message already processed)
- **422 Unprocessable Entity**: Business logic validation failed
- **503 Service Unavailable**: Service temporarily unavailable
- **504 Gateway Timeout**: Processing timeout

### Error Codes

| Code | Reason | Description |
|------|--------|-------------|
| `invalid_message_format` | Message format invalid | Missing or malformed JSON fields |
| `missing_field` | Required field missing | Specify missing field in detail |
| `invalid_tenant_id` | Tenant ID invalid | Tenant ID format or length incorrect |
| `invalid_entitlement_id` | Entitlement ID invalid | Entitlement ID format or length incorrect |
| `unknown_action` | Action not recognized | Action must be: grant, revoke, suspend |
| `duplicate_message` | Message already processed | Same messageId received twice (idempotent) |
| `governor_not_ready` | Governor not initialized | Try again later |
| `firestore_error` | Firestore write failed | Backend storage error |
| `service_unavailable` | Service not ready | Dependencies unavailable |

### Rate Limiting

- **Requests per second**: 100 (per service instance)
- **Burst capacity**: 200
- **Response Header**: `X-RateLimit-Remaining`

### Idempotency

Pub/Sub push subscriptions use message ID for deduplication:

```
If messageId matches previously processed message:
  → Return 200 with original receipt
  → Don't re-process state transitions
  → Ensure exactly-once semantics
```

### Processing Model

1. **Validation**: Check message format and required fields
2. **Deduplication**: Check if messageId already processed
3. **Governor State Check**: Verify governor is ready
4. **State Transition**: Process autonomic signal
5. **Receipt Emission**: Store receipt in ledger
6. **Response**: Return receipt to caller

---

## POST /marketplace

Marketplace entitlement handler. Accepts entitlement events and manages entitlement lifecycle.

### Request Headers

```
Content-Type: application/json
Authorization: Bearer [jwt-token]
X-Signature: [hmac-signature]
X-Request-ID: [unique-request-id]
```

### Request Body

```json
{
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "effective_at": "2024-01-01T12:00:00Z",
  "expires_at": "2024-12-31T23:59:59Z",
  "metadata": {
    "order_id": "order-789",
    "plan": "enterprise",
    "request_id": "req-abc123"
  },
  "signature": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Unique tenant identifier (max 128 chars) |
| `entitlement_id` | string | Yes | Unique entitlement identifier (max 128 chars) |
| `action` | enum | Yes | One of: `grant`, `revoke`, `suspend`, `resume` |
| `effective_at` | ISO8601 | No | When action becomes effective (default: now) |
| `expires_at` | ISO8601 | No | When entitlement expires (default: never) |
| `metadata` | object | No | Additional context (order ID, plan, etc.) |
| `signature` | string | No | JWT or HMAC signature for verification |

### Response (200 OK)

```json
{
  "id": "receipt-xyz789abc",
  "type": "transition",
  "timestamp": 1704067200,
  "request_id": "req-abc123",
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "state_from": "unentitled",
  "state_to": "entitled",
  "effective_at": 1704067200,
  "expires_at": 1735689599,
  "hash": "sha256:def456...",
  "prev_hash": "sha256:abc123...",
  "chain_hash": "sha256:chain456...",
  "metadata": {
    "order_id": "order-789",
    "plan": "enterprise"
  }
}
```

### Response (422 Unprocessable Entity)

```json
{
  "id": "receipt-err789",
  "type": "refusal",
  "timestamp": 1704067200,
  "request_id": "req-abc123",
  "reason": "entitlement_already_active",
  "detail": "Cannot grant already active entitlement",
  "hash": "sha256:err111..."
}
```

### Response (401 Unauthorized)

```json
{
  "id": "receipt-err999",
  "type": "refusal",
  "timestamp": 1704067200,
  "reason": "signature_invalid",
  "detail": "JWT signature verification failed",
  "hash": "sha256:err222..."
}
```

### Status Codes

- **200 OK**: Entitlement action processed successfully
- **400 Bad Request**: Invalid request format
- **401 Unauthorized**: Signature verification failed
- **409 Conflict**: Idempotent conflict (X-Request-ID duplicate)
- **422 Unprocessable Entity**: Business logic validation failed
- **503 Service Unavailable**: Service temporarily unavailable
- **504 Gateway Timeout**: Processing timeout

### Supported Actions

| Action | From States | To State | Description |
|--------|------------|----------|-------------|
| `grant` | unentitled, suspended | entitled | Grant or reactivate entitlement |
| `revoke` | entitled, suspended | revoked | Permanently remove entitlement |
| `suspend` | entitled | suspended | Temporarily disable entitlement |
| `resume` | suspended | entitled | Reactivate suspended entitlement |

### Idempotency

Use `X-Request-ID` header for idempotent requests:

```
If X-Request-ID matches previously processed request:
  → Return 200 with original receipt
  → Don't re-process state transitions
  → Ensure exactly-once semantics
```

### Signature Verification

When `verify_signatures` is enabled (CONFIG.md):

```bash
# JWT signature format
HEADER.PAYLOAD.SIGNATURE

# HMAC signature format (X-Signature header)
HMAC-SHA256(request_body, secret_key)
```

### Time Window

- **Effective Time**: Action applies at specified time
- **Expiration Time**: Entitlement becomes inactive at expiration
- **Time Zone**: Always UTC (ISO8601 format)

---

## Error Response Format

All error responses follow consistent format:

```json
{
  "id": "receipt-id",
  "type": "refusal",
  "timestamp": 1704067200,
  "reason": "error_code",
  "detail": "Human-readable error message",
  "request_id": "optional-request-id",
  "hash": "sha256:hash...",
  "metadata": {}
}
```

### Common HTTP Status Codes

| Code | Meaning | When to Retry |
|------|---------|---------------|
| 200 | Success | N/A |
| 400 | Bad Request | No |
| 401 | Unauthorized | No |
| 409 | Conflict | No (idempotent) |
| 422 | Unprocessable | Maybe (business logic) |
| 429 | Rate Limited | Yes (exponential backoff) |
| 503 | Unavailable | Yes (exponential backoff) |
| 504 | Timeout | Yes (exponential backoff) |

---

## Response Headers

All responses include:

```
Content-Type: application/json
X-Receipt-ID: [receipt-id]
X-Request-ID: [request-id]
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 99
X-RateLimit-Reset: 1704067260
Cache-Control: no-cache, no-store, must-revalidate
```

---

## Receipt Hash Verification

Each receipt includes cryptographic hashes for verification:

```json
{
  "id": "receipt-123",
  "hash": "sha256:abc123...",
  "prev_hash": "sha256:xyz789...",
  "chain_hash": "sha256:chain456..."
}
```

**Hash Chain Verification**:

```erlang
% Verify current hash
CurrentHash = sha256(receipt_json),

% Verify chain hash
ChainHash = sha256(<<PrevHash/binary, CurrentHash/binary>>),

% Verify consistency
ChainHash =:= Receipt.chain_hash
```

---

## Rate Limiting

Implemented per Cloud Run instance:

- **Burst**: 200 requests
- **Sustained**: 100 requests/second
- **Retry-After**: Returned in response headers

**Backoff Strategy**:

```
Attempt 1: Immediate
Attempt 2: Wait 1s + random(0-1s)
Attempt 3: Wait 2s + random(0-2s)
Attempt 4: Wait 4s + random(0-4s)
...
Max Wait: 32 seconds
```

---

## Monitoring

### Metrics

Each endpoint exposes Prometheus metrics:

```
tai_http_requests_total{endpoint, method, status}
tai_http_request_duration_seconds{endpoint}
tai_http_request_size_bytes{endpoint}
tai_http_response_size_bytes{endpoint}
```

### Logging

Structured JSON logging for all requests:

```json
{
  "timestamp": "2024-01-01T12:00:00Z",
  "level": "info",
  "endpoint": "/pubsub",
  "method": "POST",
  "status": 200,
  "duration_ms": 45,
  "request_id": "req-123",
  "receipt_id": "receipt-123",
  "tenant_id": "tenant-123"
}
```

---

## Examples

### Example 1: Pub/Sub Grant Signal

**Request**:
```bash
curl -X POST http://localhost:8080/pubsub \
  -H "Content-Type: application/json" \
  -d '{
    "message": {
      "messageId": "msg-001",
      "publishTime": "2024-01-01T12:00:00Z",
      "data": "eyJ0ZW5hbnRfaWQiOiAidGVuYW50LWFiYyIsICJhY3Rpb24iOiAiZ3JhbnQifQ=="
    }
  }'
```

**Response**:
```json
{
  "id": "receipt-001",
  "type": "transition",
  "timestamp": 1704067200,
  "action": "grant",
  "state_to": "entitled"
}
```

### Example 2: Marketplace Entitlement Grant

**Request**:
```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -H "X-Request-ID: req-abc123" \
  -d '{
    "tenant_id": "tenant-xyz",
    "entitlement_id": "ent-123",
    "action": "grant",
    "expires_at": "2024-12-31T23:59:59Z",
    "metadata": {
      "plan": "enterprise"
    }
  }'
```

**Response**:
```json
{
  "id": "receipt-002",
  "type": "transition",
  "tenant_id": "tenant-xyz",
  "action": "grant",
  "state_to": "entitled",
  "expires_at": 1735689599
}
```

### Example 3: Error Response

**Request**:
```bash
curl -X POST http://localhost:8080/pubsub \
  -H "Content-Type: application/json" \
  -d '{"message": {"data": "invalid"}}'
```

**Response** (400):
```json
{
  "id": "receipt-err001",
  "type": "refusal",
  "reason": "invalid_message_format",
  "detail": "Missing required field: messageId"
}
```

---

## Webhook Reliability

For Pub/Sub push subscriptions:

1. **At-Least-Once Delivery**: Message may be delivered multiple times
2. **Idempotent Processing**: Use `messageId` for deduplication
3. **ACK Timeout**: Service has 120 seconds to respond with 200/201/204
4. **Retry Policy**: Exponential backoff with jitter

---

## Upgrade & Deprecation

### API Versioning

Current version: **v1.0.0**

Future versions will be supported via:
- Accept header versioning: `Accept: application/vnd.tai.v1+json`
- URL versioning: `/api/v1/...` (future)

### Deprecation Timeline

Deprecated features will be announced with minimum 6-month notice.

---

## Support

For API support:
- Issues: github.com/seanchatmangpt/ggen/issues
- Documentation: https://github.com/seanchatmangpt/ggen/tree/master/tai-erlang-autonomics
