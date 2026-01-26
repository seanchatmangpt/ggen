# Receipt Schema and Hash Chain

Comprehensive guide to TAI Erlang Autonomics receipt system for audit trails and cryptographic verification.

## Overview

Receipts are immutable records of every state transition and action in the system. They form a cryptographic hash chain for verification and audit trail purposes.

---

## Receipt Types

### 1. Transition Receipts

Emitted when a state machine transitions between states.

```json
{
  "id": "receipt-trans-001",
  "type": "transition",
  "timestamp": 1704067200,
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "state_from": "unentitled",
  "state_to": "entitled",
  "prev_hash": "sha256:abc123def456...",
  "hash": "sha256:xyz789abc123...",
  "chain_hash": "sha256:chain001...",
  "metadata": {
    "message_id": "msg-001",
    "request_id": "req-001"
  }
}
```

**When Emitted**:
- Governor state changes (unentitled → entitled → suspended → etc.)
- Autonomic action transitions
- Entitlement lifecycle changes

### 2. Refusal Receipts

Emitted when a request is rejected (business logic, validation, or service issues).

```json
{
  "id": "receipt-ref-001",
  "type": "refusal",
  "timestamp": 1704067200,
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "reason": "entitlement_already_active",
  "detail": "Cannot grant already active entitlement",
  "prev_hash": "sha256:chain001...",
  "hash": "sha256:xyz789abc123...",
  "chain_hash": "sha256:chain002...",
  "metadata": {
    "request_id": "req-002",
    "error_code": "ERR_DUPLICATE_GRANT"
  }
}
```

**When Emitted**:
- Business logic violations (invalid state transition)
- Validation failures (malformed input)
- Duplicate requests (idempotent conflicts)
- Service unavailability

### 3. Action Attempt Receipts

Emitted when an action is attempted by the bounded executor.

```json
{
  "id": "receipt-act-001",
  "type": "action_attempt",
  "timestamp": 1704067200,
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "action_id": "act-001",
  "action_type": "provision_resource",
  "prev_hash": "sha256:chain002...",
  "hash": "sha256:abc123xyz789...",
  "chain_hash": "sha256:chain003...",
  "metadata": {
    "worker_pid": "<0.123.0>",
    "pool_queue_depth": 5
  }
}
```

**When Emitted**:
- Action executor accepts a task
- Task enters bounded executor queue
- Worker begins processing

### 4. Action Result Receipts

Emitted when an action completes (success, timeout, or error).

```json
{
  "id": "receipt-res-001",
  "type": "action_result",
  "timestamp": 1704067200,
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant",
  "action_id": "act-001",
  "action_type": "provision_resource",
  "result": "success",
  "duration_ms": 245,
  "prev_hash": "sha256:chain003...",
  "hash": "sha256:def456ghi789...",
  "chain_hash": "sha256:chain004...",
  "metadata": {
    "resource_id": "res-123",
    "retries": 0
  }
}
```

**Result Values**:
- `success`: Action completed successfully
- `timeout`: Action exceeded time limit
- `error`: Action failed with error
- `partial`: Some sub-tasks succeeded

---

## Complete Receipt Structure

```json
{
  "id": "receipt-unique-id",
  "type": "transition|refusal|action_attempt|action_result",
  "timestamp": 1704067200,
  "tenant_id": "tenant-123",
  "entitlement_id": "ent-456",
  "action": "grant|revoke|suspend|resume",
  "state_from": "unentitled",
  "state_to": "entitled",
  "action_id": "act-001",
  "action_type": "provision_resource",
  "result": "success",
  "reason": "optional-error-reason",
  "detail": "Human-readable detail",
  "effective_at": 1704067200,
  "expires_at": 1735689599,
  "duration_ms": 245,
  "prev_hash": "sha256:abc123...",
  "hash": "sha256:xyz789...",
  "chain_hash": "sha256:chain...",
  "metadata": {
    "message_id": "msg-001",
    "request_id": "req-001",
    "correlation_id": "corr-001",
    "custom_field": "value"
  }
}
```

### Field Definitions

| Field | Type | Presence | Description |
|-------|------|----------|-------------|
| `id` | string | Always | Unique receipt ID (generated from hash) |
| `type` | enum | Always | Receipt type |
| `timestamp` | uint64 | Always | Unix timestamp (seconds) |
| `tenant_id` | string | Always | Tenant identifier |
| `entitlement_id` | string | Always | Entitlement identifier |
| `action` | string | Always | Action performed |
| `state_from` | string | Conditional | Previous state (transition only) |
| `state_to` | string | Conditional | New state (transition only) |
| `action_id` | string | Action receipts | Unique action identifier |
| `action_type` | string | Action receipts | Type of action executed |
| `result` | string | Result only | Success/timeout/error/partial |
| `reason` | string | Refusal only | Error reason code |
| `detail` | string | Error cases | Human-readable error detail |
| `effective_at` | uint64 | Optional | When action becomes effective |
| `expires_at` | uint64 | Optional | When action expires |
| `duration_ms` | uint64 | Action results | Processing time in milliseconds |
| `prev_hash` | string | Always | Hash of previous receipt |
| `hash` | string | Always | SHA256 of current receipt JSON |
| `chain_hash` | string | Always | SHA256(prev_hash \|\| hash) |
| `metadata` | object | Always | Custom context fields |

---

## Cryptographic Hash Chain

Each receipt is linked to the previous receipt via a cryptographic hash chain.

### Hash Chain Structure

```
Receipt 1:
  prev_hash = "0000000000000000000000000000000000000000000000000000000000000000" (genesis)
  hash = SHA256(receipt_1_json)
  chain_hash = SHA256("0000...0000" || hash)

Receipt 2:
  prev_hash = SHA256(receipt_1_json)
  hash = SHA256(receipt_2_json)
  chain_hash = SHA256(prev_hash || hash)

Receipt 3:
  prev_hash = SHA256(receipt_2_json)
  hash = SHA256(receipt_3_json)
  chain_hash = SHA256(prev_hash || hash)
```

### Hash Computation

**Receipt Hash** (current state):

```erlang
JsonBinary = jiffy:encode(Receipt#{
  hash => undefined,
  chain_hash => undefined
}),
Hash = crypto:hash(sha256, JsonBinary),
HexHash = "sha256:" ++ binary_to_hex(Hash).
```

**Chain Hash** (verification link):

```erlang
ChainHash = crypto:hash(sha256, <<PrevHash/binary, CurrentHash/binary>>),
HexChainHash = "sha256:" ++ binary_to_hex(ChainHash).
```

### Verification Algorithm

```erlang
verify_receipt_chain(Receipt, PreviousReceipt) ->
  % 1. Verify current receipt hash
  {ok, ComputedHash} = compute_receipt_hash(Receipt),
  ComputedHash = Receipt.hash,

  % 2. Verify chain hash
  {ok, ComputedChainHash} = compute_chain_hash(
    PreviousReceipt.hash,
    Receipt.hash
  ),
  ComputedChainHash = Receipt.chain_hash,

  % 3. Verify prev_hash matches previous receipt
  Receipt.prev_hash = PreviousReceipt.hash,

  ok.
```

### Hash Properties

- **Deterministic**: Same input always produces same hash
- **Avalanche Effect**: Smallest change in input changes entire hash
- **Collision Resistant**: Infeasible to find two inputs with same hash
- **One-Way**: Cannot reverse hash to original input
- **Audit Trail**: Each receipt contains proof of previous receipt

---

## Receipt Ledger

### Storage Backend Options

1. **ETS** (Erlang Term Storage)
   - In-memory storage
   - Fast local access
   - Data lost on restart
   - Use: Development, testing

2. **Firestore** (GCP Production)
   - Cloud-hosted database
   - Durable persistence
   - ACID compliance
   - Use: Production GCP deployment

3. **BigQuery** (Analytics)
   - Time-series analysis
   - Historical queries
   - Long-term retention
   - Use: Analytics and reporting

### Ledger Schema (Firestore)

```
Collection: receipts
Document ID: {receipt_id}

Fields:
  id: string
  type: string
  timestamp: timestamp
  tenant_id: string
  entitlement_id: string
  action: string
  state_from: string (indexed)
  state_to: string (indexed)
  reason: string
  detail: string
  hash: string (indexed)
  prev_hash: string
  chain_hash: string (indexed)
  metadata: map
  created_at: timestamp (server-timestamp)
  _ttl: timestamp (7-day retention)

Indexes:
  (tenant_id, timestamp)
  (entitlement_id, timestamp)
  (hash)
  (chain_hash)
  (state_from, state_to)
```

---

## Query Examples

### Find Entitlement History

```erlang
% Get all receipts for a tenant's entitlement
Query = firestore:query(receipts)
  .where('entitlement_id', '==', 'ent-456')
  .order_by('timestamp', 'asc')
  .build(),

Receipts = firestore:execute(Query).
```

### Verify Receipt Chain

```erlang
% Verify hash chain integrity for a receipts series
verify_chain([]) -> ok;
verify_chain([Receipt | Rest]) ->
  case verify_receipt_chain(Receipt, PreviousReceipt) of
    ok -> verify_chain(Rest);
    {error, Reason} -> {error, Reason}
  end.
```

### Find State Transitions

```erlang
% Get all transitions from one state to another
Query = firestore:query(receipts)
  .where('state_from', '==', 'unentitled')
  .where('state_to', '==', 'entitled')
  .order_by('timestamp', 'desc')
  .build(),

Transitions = firestore:execute(Query).
```

### Find Failures

```erlang
% Find all refusal receipts in time window
Query = firestore:query(receipts)
  .where('type', '==', 'refusal')
  .where('timestamp', '>=', StartTime)
  .where('timestamp', '<=', EndTime)
  .build(),

Refusals = firestore:execute(Query).
```

---

## Receipt Examples

### Example 1: Successful Grant Transition

**Pub/Sub Signal**:
```json
{
  "tenant_id": "acme-corp",
  "entitlement_id": "ent-ai-pro-001",
  "action": "grant"
}
```

**Receipt Chain**:

```json
[
  {
    "id": "receipt-grant-001",
    "type": "transition",
    "timestamp": 1704067200,
    "tenant_id": "acme-corp",
    "entitlement_id": "ent-ai-pro-001",
    "action": "grant",
    "state_from": "unentitled",
    "state_to": "entitled",
    "prev_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "hash": "sha256:abc123...",
    "chain_hash": "sha256:chain001...",
    "metadata": {
      "message_id": "msg-001",
      "request_id": "req-grant-001"
    }
  }
]
```

### Example 2: Duplicate Request (Idempotent)

**First Request**:
```json
{
  "message_id": "msg-001",
  "tenant_id": "acme-corp",
  "action": "grant"
}
```

**Receipt 1** (success):
```json
{
  "id": "receipt-001",
  "type": "transition",
  "state_to": "entitled",
  "hash": "sha256:abc123..."
}
```

**Second Request** (same message_id):
```json
{
  "message_id": "msg-001",
  "tenant_id": "acme-corp",
  "action": "grant"
}
```

**Receipt 2** (refusal - idempotent):
```json
{
  "id": "receipt-002",
  "type": "refusal",
  "reason": "duplicate_message",
  "detail": "Message already processed as receipt-001",
  "prev_hash": "sha256:abc123...",
  "hash": "sha256:xyz789...",
  "chain_hash": "sha256:chain002..."
}
```

### Example 3: Complex Multi-Step Action

**Action Sequence**:

1. Marketplace request received:
```json
{
  "id": "receipt-req-001",
  "type": "transition",
  "action": "grant",
  "state_to": "entitled"
}
```

2. Action attempt:
```json
{
  "id": "receipt-act-001",
  "type": "action_attempt",
  "action_type": "provision_resources"
}
```

3. Action result:
```json
{
  "id": "receipt-res-001",
  "type": "action_result",
  "action_type": "provision_resources",
  "result": "success",
  "duration_ms": 245
}
```

---

## Receipt Retention

### Retention Policy

- **ETS Backend**: 24 hours (in-memory buffer)
- **Firestore**: 7 days (default TTL)
- **BigQuery**: 1 year (historical analytics)
- **Compliance**: 7 years (regulatory requirement)

### Archival

Receipts older than 7 days are:
1. Exported to BigQuery for analytics
2. Moved to cold storage (Cloud Storage)
3. Optionally deleted from hot storage

### Compliance Holds

For audit/compliance, add retention hold:

```bash
gcloud firestore documents update receipts/receipt-001 \
  --update _retention_hold=true
```

---

## Receipt Versioning

### Schema Versions

Current: **v1.0.0**

```
v1.0.0: Initial release
  - Basic receipt types
  - SHA256 hash chain
  - ETS and Firestore backends

Future v2.0.0:
  - Additional receipt types
  - Optional encryption
  - Enhanced metadata
```

### Backward Compatibility

New receipt fields are always optional. Existing code continues working with older receipt formats.

---

## Security Considerations

### Hash Chain Integrity

✓ **Protected Against**: Data modification, reordering, deletion
✗ **Not Protected Against**: Receipt insertion (requires cryptographic proof), deletion from middle

### Non-Repudiation

Each receipt includes:
- Unique ID (immutable)
- Cryptographic hash (tamper-evident)
- Timestamp (ordered)
- Full audit trail (chain linkage)

### Hash Collision Resistance

Uses SHA256 (NIST FIPS 180-4):
- Output: 256 bits (32 bytes)
- Collision probability: < 2^-128 with proper implementation
- Recommendation: Migrate to SHA3 or BLAKE3 for post-quantum

---

## Monitoring & Analytics

### Key Metrics

```erlang
% Receipt emission rate
receipt_rate = receipts_per_second

% Receipt types distribution
receipt_types = {
  transition: 85%,
  refusal: 10%,
  action_attempt: 3%,
  action_result: 2%
}

% State transition patterns
common_transitions = [
  {unentitled, entitled, 78%},
  {entitled, suspended, 12%},
  {suspended, entitled, 8%},
  {entitled, revoked, 2%}
]

% Error reasons
top_refusals = [
  {duplicate_message, 45%},
  {invalid_format, 30%},
  {state_error, 15%},
  {other, 10%}
]
```

### Dashboards

Create Grafana/Data Studio dashboards:

```
1. Receipt Volume Dashboard
   - Receipts per minute
   - Receipts by type
   - Receipts by tenant

2. Hash Chain Health Dashboard
   - Hash verification success rate
   - Chain integrity
   - Verification latency

3. Error Tracking Dashboard
   - Refusal reasons
   - Error rates by type
   - Error trends

4. Performance Dashboard
   - Receipt emission latency
   - Storage write latency
   - Query performance
```

---

## Troubleshooting

### Hash Mismatch

**Problem**: `Hash verification failed`

**Diagnosis**:
```erlang
% Get receipt and compute hash
{ok, Receipt} = receipt_store:get(ReceiptId),
{ok, ComputedHash} = compute_receipt_hash(Receipt),

case ComputedHash =:= Receipt.hash of
  true -> io:format("Hash matches~n");
  false -> io:format("Hash mismatch!~n")
end.
```

**Solution**:
- Verify JSON encoding consistency
- Check for field reordering
- Validate Unicode handling

### Chain Broken

**Problem**: `Previous receipt hash doesn't match`

**Diagnosis**:
```erlang
% Verify chain continuity
verify_prev_hash(Receipt, PrevReceipt) ->
  case Receipt.prev_hash =:= PrevReceipt.hash of
    true -> ok;
    false -> {error, chain_broken}
  end.
```

**Solution**:
- Check receipt ordering
- Verify no receipts were deleted
- Rebuild index if corrupted

### Missing Receipts

**Problem**: `Gaps in receipt sequence`

**Diagnosis**:
```erlang
% Find missing receipts
find_gaps(Receipts) ->
  find_gaps(Receipts, []).

find_gaps([Receipt1, Receipt2 | Rest], Gaps) ->
  case Receipt2.prev_hash =:= Receipt1.hash of
    true -> find_gaps([Receipt2 | Rest], Gaps);
    false -> find_gaps([Receipt2 | Rest], [Receipt1 | Gaps])
  end;
find_gaps(_, Gaps) -> Gaps.
```

**Solution**:
- Check ledger backend health
- Verify network connectivity
- Review error logs

---

## References

- Receipt Ledger Backend: CONFIG.md
- API Endpoints: ENDPOINTS.md
- Cryptography: https://csrc.nist.gov/publications/detail/fips/180/4
- Firebase Documentation: https://firebase.google.com/docs/firestore
