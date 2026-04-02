# Audit Trail Format Specification

## Overview

The pricing engine maintains an immutable, cryptographically signed audit trail for regulatory compliance. Every action is recorded with:
- Timestamp (ISO 8601)
- Action type
- Customer context
- Digital signature (Ed25519)
- Hash chain for integrity

## Entry Structure

### JSON Format

```json
{
  "entry_id": "audit_20260125_0001_abc123",
  "timestamp": "2026-01-25T12:34:56.789Z",
  "timestamp_unix_ms": 1704067496789,
  "action": "value_calculated",
  "customer_id": "cust_abc123",
  "actor": {
    "type": "system",
    "id": "pricing-engine-v1.0.0"
  },
  "data": {
    "metrics": {
      "throughput": 150.5,
      "availability": 99.95,
      "latency": 45.2
    },
    "calculated_value": 125.4,
    "billed_price": 12540.00,
    "receipt_hash": "a1b2c3d4e5f6...",
    "aggregation_method": "weighted_sum"
  },
  "result": "success",
  "error": null,
  "signature": {
    "algorithm": "Ed25519",
    "public_key": "ed25519_pk_...",
    "signature_value": "sig_abc123...",
    "signed_fields": [
      "timestamp",
      "action",
      "customer_id",
      "data",
      "result"
    ]
  },
  "chain": {
    "previous_entry_hash": "hash_xyz789...",
    "current_entry_hash": "hash_abc123...",
    "chain_length": 12547
  },
  "metadata": {
    "ip_address": "192.0.2.100",
    "user_agent": "pricing-api/1.0.0",
    "request_id": "req_xyz789",
    "session_id": "sess_abc123"
  }
}
```

## Action Types

### value_calculated
**Triggered**: When value is calculated for a customer

```json
{
  "action": "value_calculated",
  "data": {
    "metrics": {/* metric values */},
    "calculated_value": 125.4,
    "billed_price": 12540.00,
    "receipt_hash": "...",
    "aggregation_method": "weighted_sum",
    "weights": {
      "throughput": 0.5,
      "availability": 0.3,
      "latency": 0.2
    }
  }
}
```

### receipt_generated
**Triggered**: When receipt is created with cryptographic proof

```json
{
  "action": "receipt_generated",
  "data": {
    "receipt_id": "receipt_xyz789",
    "value_hash": "a1b2c3d4...",
    "hmac_signature": "e5f6g7h8...",
    "previous_receipt": "receipt_xyz788",
    "merkle_chain_valid": true
  }
}
```

### receipt_verified
**Triggered**: When customer or auditor verifies receipt

```json
{
  "action": "receipt_verified",
  "data": {
    "receipt_id": "receipt_xyz789",
    "verification_method": "signature_check",
    "verification_result": "valid",
    "chain_integrity": "intact",
    "chain_depth": 487
  }
}
```

### invoice_created
**Triggered**: When billing invoice is created

```json
{
  "action": "invoice_created",
  "data": {
    "invoice_id": "inv_abc123",
    "billing_period": "2026-01",
    "total_value": 3754.2,
    "total_billed": 375420.00,
    "currency": "USD",
    "receipt_count": 31,
    "period_start": "2026-01-01T00:00:00Z",
    "period_end": "2026-01-31T23:59:59Z"
  }
}
```

### payment_initiated
**Triggered**: When payment attempt starts

```json
{
  "action": "payment_initiated",
  "data": {
    "invoice_id": "inv_abc123",
    "amount": 375420.00,
    "currency": "USD",
    "stripe_payment_intent": "pi_xyz789",
    "payment_method_last4": "4242"
  }
}
```

### payment_succeeded
**Triggered**: When payment completes successfully

```json
{
  "action": "payment_succeeded",
  "data": {
    "invoice_id": "inv_abc123",
    "payment_intent": "pi_xyz789",
    "charge_id": "ch_xyz123",
    "amount_received": 375420,
    "fee": 12345,
    "net_amount": 363075,
    "settlement_date": "2026-02-01"
  }
}
```

### payment_failed
**Triggered**: When payment attempt fails

```json
{
  "action": "payment_failed",
  "data": {
    "invoice_id": "inv_abc123",
    "payment_intent": "pi_xyz789",
    "error_code": "card_declined",
    "error_message": "Your card was declined",
    "retry_count": 1,
    "next_retry_at": "2026-02-02T00:00:00Z"
  }
}
```

### refund_issued
**Triggered**: When refund is processed

```json
{
  "action": "refund_issued",
  "data": {
    "refund_id": "ref_xyz789",
    "original_charge": "ch_xyz123",
    "amount_refunded": 375420,
    "reason": "customer_request",
    "status": "succeeded",
    "receipt_date": "2026-02-15"
  }
}
```

### anomaly_detected
**Triggered**: When security anomaly is detected

```json
{
  "action": "anomaly_detected",
  "data": {
    "anomaly_type": "spike_detected",
    "previous_value": 25.4,
    "current_value": 254.0,
    "change_percent": 900.0,
    "threshold": 500.0,
    "severity": "warning",
    "investigation_status": "pending"
  }
}
```

### config_changed
**Triggered**: When pricing configuration is modified

```json
{
  "action": "config_changed",
  "data": {
    "field": "alpha_coefficient",
    "old_value": 100.0,
    "new_value": 150.0,
    "changed_by": "admin_user_123",
    "change_reason": "pricing_adjustment_q1_2026"
  }
}
```

### audit_trail_exported
**Triggered**: When customer requests audit trail export

```json
{
  "action": "audit_trail_exported",
  "data": {
    "export_id": "export_xyz789",
    "format": "json",
    "requested_by": "cust_abc123",
    "export_reason": "compliance_audit",
    "entry_count": 487,
    "date_range": {
      "start": "2025-01-01T00:00:00Z",
      "end": "2026-01-25T00:00:00Z"
    }
  }
}
```

## Signature Format

### Ed25519 Digital Signature

```json
{
  "signature": {
    "algorithm": "Ed25519",
    "public_key": "ed25519_pk_abcdef123456...",
    "signature_value": "sig_abcdef123456...",
    "signature_version": "1.0",
    "signed_fields": [
      "timestamp",
      "action",
      "customer_id",
      "data",
      "result"
    ]
  }
}
```

**Verification Process**:
1. Extract fields in order specified by `signed_fields`
2. Create canonical JSON form (sorted keys, no whitespace)
3. Verify signature using public key

### Canonical Form Example

```json
{"action":"value_calculated","customer_id":"cust_abc123","data":{"aggregation_method":"weighted_sum","billed_price":12540.00,"calculated_value":125.4,"metrics":{"availability":99.95,"latency":45.2,"throughput":150.5},"receipt_hash":"a1b2c3d4e5f6..."},"result":"success","timestamp":"2026-01-25T12:34:56.789Z"}
```

## Hash Chain Integrity

### Merkle Chain Structure

```
Entry N
├── previous_entry_hash: SHA256(Entry N-1)
├── payload_hash: SHA256(canonical_form)
├── signature: Ed25519(payload_hash)
└── current_entry_hash: SHA256(payload_hash + signature)
    ↓
Entry N+1
├── previous_entry_hash: SHA256(Entry N)
└── ...
```

### Chain Verification

```python
def verify_chain(entry, previous_entry):
    # 1. Verify signature
    payload = canonical_json(entry)
    assert verify_signature(payload, entry.signature)

    # 2. Verify hash chain
    previous_hash = sha256(canonical_json(previous_entry))
    assert entry.chain.previous_entry_hash == previous_hash

    # 3. Verify payload integrity
    assert entry.chain.current_entry_hash == sha256(payload)

    return True
```

## Compliance & Retention

### Retention Policy
- **Regulatory**: 7 years (SOX, GDPR)
- **Default**: Unlimited (append-only ledger)
- **Archive**: Moved to cold storage after 2 years

### Deletion Policy
- **Never delete**: GDPR right to erasure requires special handling
- **Redaction**: Customer name/email can be redacted
- **Anonymization**: Replace customer_id with hash for historical analysis

### Access Control
- **Read**: Only data owner and authorized auditors
- **Export**: Customer can request full audit trail
- **Compliance**: Auditors can request filtered trails

## Export Formats

### JSON Export
```bash
curl https://pricing-api/api/audit/export?customer_id=cust_abc123&format=json \
  -H "Authorization: Bearer $TOKEN" | gzip > audit_trail.json.gz
```

### CSV Export
```csv
timestamp,action,customer_id,data_json,result,error,signature_valid,chain_valid
2026-01-25T12:34:56.789Z,value_calculated,cust_abc123,"{...}",success,,true,true
2026-01-25T12:35:00.123Z,receipt_generated,cust_abc123,"{...}",success,,true,true
```

### Compliance Format (for auditors)
```bash
curl https://pricing-api/api/audit/export/compliance \
  -H "Authorization: Bearer $AUDITOR_TOKEN" \
  -d '{
    "organization_id": "org_xyz",
    "audit_reason": "annual_compliance",
    "start_date": "2025-01-01",
    "end_date": "2026-01-25"
  }' > compliance_audit.tar.gz
```

## Security Considerations

### Timestamp Integrity
- Use NTP-synchronized system clock
- Include nanosecond precision
- Verify no clock jumps in chain

### Key Management
- Ed25519 signing key stored in HSM/KMS
- Rotate keys yearly
- Maintain key history for old entries

### Audit Log Security
- Write-once storage (immutable)
- Encrypt at rest (AES-256-GCM)
- Encrypt in transit (TLS 1.3)
- Access logs monitored

### Chain Continuity
- Daily chain integrity checks
- Automated alerts on chain breaks
- Monthly cryptographic verification
- Quarterly third-party audit

## Example Complete Entry

```json
{
  "entry_id": "audit_20260125_123456_f8a9b1c2",
  "timestamp": "2026-01-25T12:34:56.789Z",
  "timestamp_unix_ms": 1704067496789,
  "action": "value_calculated",
  "customer_id": "cust_acme_corp_001",
  "actor": {
    "type": "system",
    "id": "pricing-engine-v1.0.0",
    "version": "1.0.0"
  },
  "data": {
    "metrics": {
      "throughput": 150.5,
      "availability": 99.95,
      "latency": 45.2,
      "error_rate": 0.05
    },
    "calculated_value": 125.4,
    "billed_price": 12540.00,
    "currency": "USD",
    "receipt_hash": "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6",
    "aggregation_method": "weighted_sum",
    "weights": {
      "throughput": 0.5,
      "availability": 0.3,
      "latency": 0.15,
      "error_rate": 0.05
    },
    "pricing_config": {
      "alpha_coefficient": 100.0,
      "min_price": 10.0,
      "max_price": 5000.0
    }
  },
  "result": "success",
  "error": null,
  "signature": {
    "algorithm": "Ed25519",
    "public_key": "ed25519_pk_f8a9b1c2d3e4f5g6h7i8j9k0l1m2n3o4",
    "signature_value": "sig_a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6",
    "signature_version": "1.0",
    "signed_fields": [
      "timestamp",
      "action",
      "customer_id",
      "data",
      "result"
    ]
  },
  "chain": {
    "previous_entry_hash": "prev_a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6",
    "current_entry_hash": "curr_f8a9b1c2d3e4f5g6h7i8j9k0l1m2n3o4",
    "chain_length": 12547,
    "chain_verified": true,
    "verification_timestamp": "2026-01-25T12:34:57.000Z"
  },
  "metadata": {
    "ip_address": "192.0.2.100",
    "user_agent": "pricing-api/1.0.0",
    "request_id": "req_f8a9b1c2d3e4f5g6h7i8j9k0l1m2n3o4",
    "session_id": "sess_a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6",
    "datacenter": "us-central1",
    "region": "us"
  }
}
```

## Compliance Verification

### SOX Compliance
- ✅ Immutable audit trail
- ✅ Digital signatures
- ✅ No edit/delete capability
- ✅ 7-year retention
- ✅ Independent verification

### GDPR Compliance
- ✅ Right to access (export audit)
- ✅ Data minimization (only needed data)
- ✅ Retention policies
- ✅ Secure deletion (redaction)

### PCI DSS Compliance
- ✅ Access controls (authentication)
- ✅ Encryption (TLS + at-rest)
- ✅ Monitoring (alerts on anomalies)
- ✅ Testing (quarterly verification)
