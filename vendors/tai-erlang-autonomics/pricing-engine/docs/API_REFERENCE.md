# Pricing Engine API Reference

## Overview

The TAI Autonomic Pricing Engine provides deterministic value calculation and billing for outcome-based pricing models. All values are cryptographically hashed for auditability and immutability.

**Base URL**: `https://pricing-api.autonomic.services/api`
**API Version**: 1.0.0
**Authentication**: OAuth 2.0 Bearer Token

---

## Fundamental Concepts

### Value Calculation Formula
```
V = f(metrics)  - Deterministic value calculation from metrics
P = α × V       - Price formula where α is customer-specific coefficient
```

### Merkle Chain
Each value calculation links to the previous via cryptographic hash, creating an immutable chain for audit trails.

### Receipt
Tamper-proof record of a value calculation with HMAC signature and merkle proof.

---

## Authentication

All requests require a Bearer token in the `Authorization` header:

```http
Authorization: Bearer <your_api_token>
```

**Token Requirements**:
- 256-bit randomly generated secret
- Rotated every 90 days
- Scoped to customer account

---

## Error Handling

### Standard Error Response
```json
{
  "error": "error_code",
  "message": "Human-readable error description",
  "details": {
    "field": "additional context"
  },
  "timestamp": 1704067200000,
  "request_id": "req_xyz789"
}
```

### Error Codes

| Code | HTTP | Description |
|------|------|-------------|
| `validation_failed` | 400 | Input validation error |
| `invalid_metrics` | 400 | Metric values out of bounds |
| `customer_not_found` | 404 | Customer does not exist |
| `receipt_not_found` | 404 | Receipt not found |
| `unauthorized` | 401 | Authentication failed |
| `forbidden` | 403 | Insufficient permissions |
| `rate_limit_exceeded` | 429 | Too many requests |
| `internal_error` | 500 | Server error |

---

## Endpoints

### 1. Calculate Value

Calculate value for a customer based on current metrics.

**Endpoint**: `POST /value/calculate`

**Request**:
```json
{
  "customer_id": "cust_abc123",
  "metrics": {
    "throughput": 150.5,
    "latency": 45.2,
    "availability": 99.95
  },
  "aggregation_method": "weighted_sum",
  "pricing_config": {
    "alpha_coefficient": 100.0,
    "min_price": 10.0,
    "max_price": 5000.0
  }
}
```

**Response** (200 OK):
```json
{
  "receipt_id": "receipt_xyz789",
  "customer_id": "cust_abc123",
  "calculated_value": 125.4,
  "billed_price": 12540.00,
  "timestamp": 1704067200000,
  "receipt_hash": "a1b2c3d4...",
  "previous_hash": "z9y8x7w6...",
  "metrics_snapshot": {
    "throughput": 150.5,
    "latency": 45.2,
    "availability": 99.95
  },
  "calculation_metadata": {
    "aggregation_method": "weighted_sum",
    "normalized_values": {
      "throughput": 0.745,
      "latency": 0.823,
      "availability": 0.999
    }
  }
}
```

**Error Responses**:
- `400 Bad Request` - Invalid metrics or config
- `401 Unauthorized` - Invalid authentication
- `429 Too Many Requests` - Rate limited

---

### 2. Get Current Value

Retrieve the most recent value calculation for a customer.

**Endpoint**: `GET /value/current?customer_id=<id>`

**Response** (200 OK):
```json
{
  "customer_id": "cust_abc123",
  "calculated_value": 125.4,
  "billed_price": 12540.00,
  "timestamp": 1704067200000,
  "period": "2026-01-01T00:00:00Z",
  "status": "completed",
  "metrics": {
    "throughput": 150.5,
    "latency": 45.2,
    "availability": 99.95
  }
}
```

---

### 3. Get Value History

Retrieve historical value calculations with pagination.

**Endpoint**: `GET /value/history?customer_id=<id>&limit=100&offset=0`

**Query Parameters**:
- `customer_id` (required): Customer identifier
- `limit` (optional): Max records to return (1-1000, default 100)
- `offset` (optional): Pagination offset (default 0)
- `start_time` (optional): Unix timestamp in milliseconds
- `end_time` (optional): Unix timestamp in milliseconds

**Response** (200 OK):
```json
{
  "customer_id": "cust_abc123",
  "total_records": 487,
  "limit": 100,
  "offset": 0,
  "has_more": true,
  "records": [
    {
      "timestamp": 1704067200000,
      "calculated_value": 125.4,
      "billed_price": 12540.00,
      "receipt_hash": "a1b2c3d4...",
      "status": "completed"
    }
  ]
}
```

---

### 4. Get Receipt

Retrieve specific receipt with full verification details.

**Endpoint**: `GET /receipt/{receipt_id}`

**Response** (200 OK):
```json
{
  "receipt_id": "receipt_xyz789",
  "customer_id": "cust_abc123",
  "calculated_value": 125.4,
  "billed_price": 12540.00,
  "timestamp": 1704067200000,
  "period_start": 1704067200000,
  "period_end": 1704153600000,
  "value_hash": "a1b2c3d4...",
  "signature": "e5f6g7h8...",
  "verified": true,
  "verification_timestamp": 1704153700000,
  "invoice_id": "inv_abc123",
  "payment_status": "paid",
  "merkle_proof": [
    "hash1...",
    "hash2...",
    "hash3..."
  ]
}
```

---

### 5. Verify Receipt

Verify receipt authenticity and chain integrity.

**Endpoint**: `POST /receipt/{receipt_id}/verify`

**Request**:
```json
{
  "customer_id": "cust_abc123",
  "hmac_key": "base64_encoded_key"
}
```

**Response** (200 OK):
```json
{
  "receipt_id": "receipt_xyz789",
  "verified": true,
  "verification_status": "signature_valid_chain_valid",
  "timestamp": 1704153700000,
  "merkle_chain_intact": true,
  "previous_receipt": "receipt_xyz788",
  "next_receipt": "receipt_xyz790",
  "forensics": {
    "signature_algorithm": "HMAC-SHA256",
    "hash_algorithm": "SHA-256",
    "chain_depth": 487
  }
}
```

**Error Responses**:
- `404 Not Found` - Receipt not found
- `400 Bad Request` - Verification failed

---

### 6. Get Invoice

Retrieve invoice with receipt references and payment status.

**Endpoint**: `GET /billing/invoice/{invoice_id}`

**Response** (200 OK):
```json
{
  "invoice_id": "inv_abc123",
  "customer_id": "cust_abc123",
  "billing_period": "2026-01",
  "start_date": "2026-01-01T00:00:00Z",
  "end_date": "2026-01-31T23:59:59Z",
  "total_value": 3754.2,
  "total_billed": 375420.00,
  "currency": "USD",
  "receipts": [
    "receipt_xyz789",
    "receipt_xyz790",
    "receipt_xyz791"
  ],
  "payment_status": "paid",
  "payment_date": "2026-02-01T10:30:00Z",
  "stripe_charge_id": "ch_xyz123",
  "due_date": "2026-02-05T00:00:00Z"
}
```

---

### 7. List Billing Cycles

Get billing cycles with summaries.

**Endpoint**: `GET /billing/cycles?customer_id=<id>&start_time=<ts>&end_time=<ts>`

**Response** (200 OK):
```json
{
  "customer_id": "cust_abc123",
  "cycles": [
    {
      "cycle_id": "cycle_2026_01",
      "period": "2026-01",
      "start_date": "2026-01-01T00:00:00Z",
      "end_date": "2026-01-31T23:59:59Z",
      "total_value": 3754.2,
      "total_billed": 375420.00,
      "invoice_id": "inv_abc123",
      "payment_status": "paid",
      "receipt_count": 31
    }
  ],
  "total_billed_all_time": 1125420.00
}
```

---

### 8. Export Audit Trail

Export customer's complete audit trail for compliance.

**Endpoint**: `GET /audit/export?customer_id=<id>&format=json&start_time=<ts>&end_time=<ts>`

**Query Parameters**:
- `customer_id` (required): Customer identifier
- `format` (optional): `json` or `csv` (default `json`)
- `start_time` (optional): Unix timestamp in milliseconds
- `end_time` (optional): Unix timestamp in milliseconds

**Response** (200 OK, application/json):
```json
{
  "customer_id": "cust_abc123",
  "exported_at": 1704153700000,
  "format": "json",
  "period": {
    "start": "2025-01-01T00:00:00Z",
    "end": "2026-01-25T00:00:00Z"
  },
  "entries": [
    {
      "timestamp": 1704067200000,
      "action": "value_calculated",
      "calculated_value": 125.4,
      "billed_price": 12540.00,
      "receipt_hash": "a1b2c3d4...",
      "metrics": {
        "throughput": 150.5,
        "latency": 45.2
      }
    }
  ],
  "total_entries": 487,
  "hash_chain_verified": true
}
```

---

### 9. Get Analytics

Get value analytics and trends.

**Endpoint**: `GET /analytics?customer_id=<id>&metric=calculated_value&period=30d`

**Query Parameters**:
- `customer_id` (required): Customer identifier
- `metric` (optional): `calculated_value`, `billed_price`, etc.
- `period` (optional): `7d`, `30d`, `90d`, `1y` (default `30d`)

**Response** (200 OK):
```json
{
  "customer_id": "cust_abc123",
  "metric": "calculated_value",
  "period": "30d",
  "statistics": {
    "average": 125.4,
    "median": 120.8,
    "min": 45.2,
    "max": 450.1,
    "stddev": 89.3,
    "percentile_95": 320.5
  },
  "trend": {
    "direction": "increasing",
    "month_over_month_change_percent": 12.5,
    "projected_next_period": 141.0
  },
  "samples": 30
}
```

---

## Request/Response Formats

### Metric Schema

Metrics must be numeric and within acceptable bounds:

```json
{
  "throughput": 150.5,        // operations per second (0 to 1,000,000)
  "latency": 45.2,             // milliseconds (0 to 10,000)
  "availability": 99.95,       // percentage (0 to 100)
  "error_rate": 0.05,          // percentage (0 to 100)
  "custom_metric": 500.0       // custom units (0 to 1,000,000,000)
}
```

### Pricing Config Schema

```json
{
  "alpha_coefficient": 100.0,      // price multiplier
  "min_price": 10.0,               // floor price in USD
  "max_price": 10000.0,            // ceiling price in USD
  "aggregation_method": "weighted_sum",  // or "multiplicative"
  "metric_weights": {
    "throughput": 0.5,
    "availability": 0.3,
    "latency": 0.2
  }
}
```

---

## Rate Limiting

- **Tier 1 (Free)**: 100 requests/minute
- **Tier 2 (Professional)**: 1,000 requests/minute
- **Tier 3 (Enterprise)**: Unlimited

Rate limit headers:
```http
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 998
X-RateLimit-Reset: 1704153700
```

---

## Security Best Practices

1. **Never log sensitive data**: API keys, HMAC keys, signatures
2. **Use HTTPS**: All endpoints require TLS 1.2+
3. **Rotate credentials**: Change API keys every 90 days
4. **Validate signatures**: Always verify HMAC signatures on receipts
5. **Implement circuit breakers**: Handle API errors gracefully
6. **Monitor for anomalies**: Track unusual value patterns

---

## Implementation Examples

### Calculate Value (Python)
```python
import requests
import json

headers = {
    "Authorization": "Bearer YOUR_API_TOKEN",
    "Content-Type": "application/json"
}

payload = {
    "customer_id": "cust_abc123",
    "metrics": {
        "throughput": 150.5,
        "latency": 45.2,
        "availability": 99.95
    },
    "aggregation_method": "weighted_sum",
    "pricing_config": {
        "alpha_coefficient": 100.0,
        "min_price": 10.0,
        "max_price": 5000.0
    }
}

response = requests.post(
    "https://pricing-api.autonomic.services/api/value/calculate",
    json=payload,
    headers=headers
)

result = response.json()
print(f"Calculated Value: {result['calculated_value']}")
print(f"Billed Price: {result['billed_price']}")
print(f"Receipt ID: {result['receipt_id']}")
```

### Verify Receipt (JavaScript)
```javascript
const fetch = require('node-fetch');
const crypto = require('crypto');

async function verifyReceipt(receiptId, customerId, hmacKey) {
    const headers = {
        'Authorization': `Bearer ${process.env.API_TOKEN}`,
        'Content-Type': 'application/json'
    };

    const payload = {
        customer_id: customerId,
        hmac_key: hmacKey
    };

    const response = await fetch(
        `https://pricing-api.autonomic.services/api/receipt/${receiptId}/verify`,
        {
            method: 'POST',
            headers,
            body: JSON.stringify(payload)
        }
    );

    const result = await response.json();
    console.log(`Receipt Verified: ${result.verified}`);
    console.log(`Chain Intact: ${result.merkle_chain_intact}`);
    return result;
}
```

---

## Webhooks

### Payment Status Updates

**Event**: `payment.completed`

```json
{
  "event_id": "evt_xyz789",
  "timestamp": 1704153700000,
  "event_type": "payment.completed",
  "data": {
    "invoice_id": "inv_abc123",
    "customer_id": "cust_abc123",
    "amount_paid": 375420.00,
    "currency": "USD",
    "stripe_charge_id": "ch_xyz123"
  }
}
```

---

## Support

- **Documentation**: https://docs.autonomic.services/pricing
- **Status Page**: https://status.autonomic.services
- **Support Email**: pricing-support@autonomic.services
- **Slack Community**: #pricing-engine
