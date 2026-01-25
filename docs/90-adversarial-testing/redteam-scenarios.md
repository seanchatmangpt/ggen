# Red Team Scenarios

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

## Overview

**Purpose**: Prove through attacker simulation that system recovers from all known attack patterns (zero humans needed).

**Approach**: Red-team scenarios based on OWASP Top 10, CWE Top 25, threat models.

**Success Criteria**:
- ✅ All attack scenarios fail (attacker cannot succeed)
- ✅ All attacks emit receipts (no silent failures)
- ✅ System recovers automatically (no manual intervention needed)
- ✅ Attacker detection logged (audit trail of attack attempts)

---

## Scenario 1: Signature Verification Bypass

**Objective**: Forge webhook signature (HMAC key compromise).

**Attacker Goal**: Launch expensive SKU without valid entitlement.

**Attack Path**:

```
Attacker: "I have customer's secret key (leaked from GitHub)"
  ↓
Attacker: "I'll sign my own webhook with stolen key"
  ↓
Attacker: POST /signal/expensive-sku/attacker-tenant
  Body: {"signal_type": "launch", "entitlement_id": "premium-ent"}
  Signature: hmac-sha256(body, leaked_key)
  ↓
ggen System:
  1. Verify signature ✓ (signature is valid, matches stolen key)
  2. Check entitlement: premium-ent belongs to attacker? NO
  3. Emit: permission_denied receipt
  4. Response: 403 Forbidden
  ↓
Result: ATTACK FAILED
  - Expensive SKU not launched
  - Receipt emitted (audit trail)
  - Attacker detected (IP logged)
```

**Expected Receipt**:

```json
{
  "kind": "permission_denied",
  "ts": "2026-01-25T14:32:00Z",
  "decision": "refuse",
  "details": {
    "principal": "stolen-key-signature",
    "resource": "expensive-sku-uuid",
    "reason": "entitlement_not_found",
    "error_message": "Entitlement premium-ent does not belong to account"
  }
}
```

**Why This Works**:
- ✅ Signature verification is at HTTP layer (fast)
- ✅ Entitlement verification is at authorization layer (catches forgery)
- ✅ Two-layer defense (signature + entitlement ownership)
- ✅ Attack detected and logged

**Mitigation**:
1. Customer rotates API key (in Marketplace console)
2. ggen invalidates old key (automatic)
3. Attacker's subsequent requests fail with 401

---

## Scenario 2: Entitlement Escalation

**Objective**: Change SKU ID in request to expensive one (privilege escalation).

**Attacker Goal**: Launch "premium" SKU when only "basic" is entitled.

**Attack Path**:

```
Attacker: "I'm entitled to basic-sku, but I want premium-sku"
  ↓
Attacker: "I'll just change the sku_id in the URL"
  ↓
Attacker: POST /signal/premium-sku/attacker-tenant
  Body: {"signal_type": "launch", "entitlement_id": "basic-ent-123"}
  Signature: hmac-sha256(body, my_key)
  ↓
ggen System:
  1. Verify signature ✓ (signature is valid)
  2. Verify path parameter sku_id == "premium-sku"
  3. Verify entitlement basic-ent-123
  4. Check: basic-ent-123 entitles access to "premium-sku"? NO
  5. Emit: entitlement_not_found receipt
  6. Response: 403 Forbidden
  ↓
Result: ATTACK FAILED
  - Premium SKU not launched
  - Attacker trying to access out-of-scope SKU detected
  - Audit log shows escalation attempt
```

**Expected Receipt**:

```json
{
  "kind": "refusal",
  "ts": "2026-01-25T14:32:00Z",
  "decision": "refuse",
  "details": {
    "error_code": 403,
    "error_message": "Entitlement basic-ent-123 does not grant access to premium-sku",
    "sku_id_requested": "premium-sku",
    "sku_id_entitled": "basic-sku",
    "incident_type": "entitlement_escalation_attempt"
  }
}
```

**Why This Works**:
- ✅ Path parameter (sku_id) is part of authorization check
- ✅ Entitlement scope verified against requested SKU
- ✅ Mismatch detected and refused
- ✅ Escalation attempt logged with severity

**Mitigation**:
1. Alert ops team (escalation attempt detected)
2. Rate limit customer if pattern continues (quota tightening)
3. Manual review of customer's usage patterns

---

## Scenario 3: Permission Elevation

**Objective**: Perform admin actions without admin role (privilege escalation).

**Attacker Goal**: Delete another customer's SKU instance.

**Attack Path**:

```
Attacker: "I'm customer A, but I want to delete customer B's instance"
  ↓
Attacker: "I'll send a delete signal for customer B's SKU"
  ↓
Attacker: POST /signal/customer-b-sku/customer-b-tenant
  Body: {"signal_type": "delete", "entitlement_id": "attacker-ent-123"}
  Signature: hmac-sha256(body, attacker_key)
  ↓
ggen System:
  1. Verify signature ✓ (signature is valid for attacker's key)
  2. Parse path: sku_id=customer-b-sku, tenant_id=customer-b-tenant
  3. Check: Does attacker_key correspond to customer B? NO
  4. Emit: permission_denied receipt
  5. Response: 403 Forbidden
  ↓
Result: ATTACK FAILED
  - Customer B's SKU not deleted
  - Cross-tenant access attempt detected
  - Attacker's identity verified (cross-check failed)
```

**Expected Receipt**:

```json
{
  "kind": "permission_denied",
  "ts": "2026-01-25T14:32:00Z",
  "decision": "refuse",
  "details": {
    "principal": "attacker-account",
    "resource": "customer-b-sku",
    "action": "delete",
    "reason": "tenant_mismatch",
    "attacker_tenant": "attacker-tenant",
    "requested_tenant": "customer-b-tenant",
    "incident_severity": "critical",
    "incident_type": "cross_tenant_access_attempt"
  }
}
```

**Why This Works**:
- ✅ Tenant ID verified from request path
- ✅ Principal (API key) mapped to tenant
- ✅ Tenant mismatch detected (cross-tenant access blocked)
- ✅ Critical incident severity (cross-tenant is CRITICAL)

**Mitigation**:
1. Immediate alert to security team (critical incident)
2. Account investigation (why was this attempted?)
3. Possible account suspension if pattern continues

---

## Scenario 4: Quota Cheating

**Objective**: Exhaust quota to launch DoS attack (or skip charges).

**Attacker Goal**: Send 10,000 requests per minute (exceed rate limit) to overload system.

**Attack Path**:

```
Attacker: "I'll send many requests rapidly to crash the system"
  ↓
Attacker: for i in 1..10000:
  POST /signal/sku/tenant
  Body: {"signal_type": "launch", "entitlement_id": f"ent-{i}"}
  Signature: valid
  ↓
Request 1-1000:
  ggen System:
    1. Verify signature ✓
    2. Check quota: Used 1/1000, Allowed 1000 ✓
    3. Emit: signal_received (accept)
    4. Response: 202 Accepted
  ↓
Request 1001-2000:
  ggen System:
    1. Verify signature ✓
    2. Check quota: Used 1000/1000, EXHAUSTED
    3. Emit: quota_exceeded receipt
    4. Response: 429 Too Many Requests
    5. Retry-After: 3600 (1 hour)
  ↓
Request 2001-10000:
  ggen System:
    1. Check quota: STILL EXHAUSTED
    2. Emit: quota_exceeded receipt (every request)
    3. Response: 429 (every request)
  ↓
Result: ATTACK FAILED
  - Quota enforced, requests rejected at 1001st request
  - No system overload (requests rejected immediately)
  - Attacker gets 429 responses, cannot bypass
  - DOS attempt detected and logged
```

**Expected Receipts**:

```json
// Request 1000
{
  "kind": "signal_received",
  "decision": "accept",
  "details": {
    "signal_id": "signal-uuid-1000",
    "queue_position": 1000
  }
}

// Request 1001
{
  "kind": "quota_exceeded",
  "decision": "refuse",
  "details": {
    "quota_type": "rate_limit",
    "limit": 1000,
    "current_value": 1001,
    "reset_ts": "2026-01-26T00:00:00Z",
    "response_code": 429,
    "retry_after_seconds": 3600
  }
}
```

**Why This Works**:
- ✅ Quota check happens early (before queueing action)
- ✅ Requests rejected immediately (no system load)
- ✅ Quota enforced per hour/day (rate limiting)
- ✅ Response time stays <100ms even under attack

**Monitoring**:
```
Alert: "signal_storm_detected"
  - Source IP: 192.0.2.1
  - Request rate: 10,000/minute
  - Quota exceeded at: request #1001
  - Action taken: IP blocked for 1 hour
```

**Mitigation**:
1. Auto-block IP (if pattern continues)
2. Alert ops team
3. Manual review of account (suspicious activity)

---

## Scenario 5: Data Corruption Attack

**Objective**: Tamper with receipt data in Firestore (corrupt audit trail).

**Attacker Goal**: Hide evidence of expensive actions (edit receipt to show "deny" instead of "accept").

**Attack Path**:

```
Attacker: "I'll hack Firestore and modify receipts"
  ↓
Attacker: Directly edit Firestore document
  Original: {"kind": "action_completed", "decision": "accept", ...}
  Modified: {"kind": "action_completed", "decision": "refuse", ...}
  ↓
ggen Chain Verification:
  1. Load all receipts for SKU
  2. For each receipt i:
     prev_hash = receipt[i].prev_chain_hash_b64
     computed_hash = SHA256(receipt[i-1])
     if prev_hash != computed_hash: CORRUPTION DETECTED
  ↓
Result: CORRUPTION DETECTED
  - Receipt i-1 was modified, hash chain broken
  - Emit: invariant_violation receipt
  - Response: 500 Internal Server Error
  - Firestore backup restored (from immutable GCS archive)
  ↓
Result: ATTACK FAILED
  - Tampering detected immediately
  - Hash chain integrity proven broken
  - Audit trail restored from archive
```

**Expected Receipt**:

```json
{
  "kind": "invariant_violation",
  "ts": "2026-01-25T14:32:00Z",
  "decision": "refuse",
  "details": {
    "invariant_id": "inv-chain",
    "violation_type": "chain_hash_mismatch",
    "receipt_index": 5,
    "expected_hash": "abc123...",
    "actual_hash": "xyz789...",
    "incident_type": "data_tampering_detected",
    "incident_severity": "critical",
    "remediation": "archive_restore_initiated"
  }
}
```

**Why This Works**:
- ✅ Hash chain is cryptographic proof (SHA-256, cannot forge)
- ✅ Chain verification is automatic (on every query)
- ✅ Archive is immutable (GCS Object Lock, cannot modify)
- ✅ Backup restore is automated

**Recovery**:
```
1. Detect corruption: Hash chain breaks
2. Restore: Copy receipts from GCS archive
3. Verify: Re-run hash chain verification
4. Alert: Security incident triggered
5. Investigate: Who accessed Firestore? When? What did they change?
```

---

## Scenario 6: Entitlement Forging

**Objective**: Create fake entitlement record (bypass marketplace webhook).

**Attacker Goal**: Activate premium entitlement without paying.

**Attack Path**:

```
Attacker: "I'll create a fake entitlement in Firestore"
  ↓
Attacker: INSERT INTO firestore.entitlements
  {
    "entitlement_id": "fake-ent-xyz",
    "sku_id": "premium-sku",
    "account_id": "attacker-account",
    "state": "ACTIVE",
    "activation_ts": now
  }
  ↓
ggen System:
  1. Customer sends signal for premium-sku
  2. Signal processor queries entitlements
  3. Find fake-ent-xyz ✓
  4. Check: Is entitlement from Marketplace? NO
     (Compare: db.creation_ts vs marketplace.webhook_ts)
  5. If mismatch: FORGED_ENTITLEMENT
  6. Emit: incident_detected receipt
  7. Response: 403 Forbidden
  ↓
Result: ATTACK FAILED
  - Forged entitlement detected (source mismatch)
  - Only Marketplace webhooks create legitimate entitlements
  - Fake entitlement rejected
```

**Expected Receipt**:

```json
{
  "kind": "incident_detected",
  "ts": "2026-01-25T14:32:00Z",
  "decision": "refuse",
  "details": {
    "incident_id": "incident-uuid",
    "severity": "critical",
    "type": "entitlement_forgery_attempt",
    "description": "Entitlement fake-ent-xyz lacks valid Marketplace webhook proof",
    "affected_entities": ["premium-sku", "attacker-account"],
    "action_taken": "signal_blocked",
    "requires_manual_intervention": false,
    "forensics": {
      "entitlement_created_at": "2026-01-25T14:30:00Z",
      "first_usage_attempt": "2026-01-25T14:32:00Z",
      "no_webhook_proof": true
    }
  }
}
```

**Why This Works**:
- ✅ Only Marketplace webhooks are trusted source (immutable audit trail)
- ✅ Direct Firestore inserts are not trusted (could be attacker)
- ✅ Source verification is automatic (webhook_id field)
- ✅ Forgery attempt is logged with forensics

**Prevention**:
```
Entitlement Validation Rules:
  1. entitlement.source == "gcp.marketplace.webhook"
  2. entitlement.webhook_id != null
  3. entitlement.webhook_id matches GCP marketplace records
  4. entitlement.creation_ts comes from Marketplace event
```

---

## Scenario 7: Permission Denial Attack (Cascading Failure)

**Objective**: Trigger cascading failures by creating bad state (e.g., deleted entitlement mid-action).

**Attacker Goal**: Crash system or cause undefined behavior.

**Attack Path**:

```
Attacker: "I'll delete an entitlement while action is in-flight"
  ↓
Step 1: Attacker sends launch signal
  POST /signal/sku/tenant
  → Response: 202 Accepted
  → Signal queued for processing
  ↓
Step 2: Attacker (via stolen DB access) deletes entitlement
  DELETE FROM firestore.entitlements WHERE id = "ent-123"
  ↓
Step 3: Action processor tries to execute
  1. Load entitlement: Not found (was deleted)
  2. Check: entitlement null? YES
  3. Handle null gracefully:
     - DO NOT crash
     - DO NOT ignore
     - Emit: action_failed receipt (decision: refuse)
     - Mark action as: error_state
  4. Response: No crash, system stable
  ↓
Result: ATTACK FAILED (Graceful Degradation)
  - No unhandled exception
  - No cascading failures
  - Receipt emitted (no silent failures)
  - System recovers automatically
```

**Expected Receipt**:

```json
{
  "kind": "action_failed",
  "ts": "2026-01-25T14:32:45Z",
  "decision": "refuse",
  "details": {
    "action_id": "action-uuid-001",
    "error_code": "entitlement_not_found",
    "error_message": "Entitlement was deleted during processing",
    "state": "in_flight",
    "remediation": "dead_letter",
    "incident_type": "concurrent_state_change_detected"
  }
}
```

**Why This Works**:
- ✅ Null checks at every boundary (defensive programming)
- ✅ Errors handled explicitly (no uncaught exceptions)
- ✅ Receipts emitted for all paths (no silent failures)
- ✅ System stable after error (no cascading failures)

**Safety Mechanisms**:
```
1. Null Checks: if entitlement == null → error receipt
2. Timeout Guards: if action > 180s → timeout receipt
3. Resource Limits: if memory > 100MB → throttle receipt
4. State Validation: if state invalid → invariant_violation receipt
5. Circuit Breaker: if errors > threshold → circuit_open receipt
```

---

## Scenario 8: Man-in-the-Middle (MITM) Attack

**Objective**: Intercept and modify webhook (if TLS is weak).

**Attacker Goal**: Change signal parameters (e.g., machine_type from small to large).

**Attack Path**:

```
Attacker: "I'm on the same network, I can intercept webhooks"
  ↓
Original Request (from Customer):
  POST /signal/sku/tenant
  Body: {"signal_type": "launch", "parameters": {"machine_type": "small"}}
  Signature: sha256=abc123...
  ↓
Attacker MITM: Intercept and modify
  Body (modified): {"signal_type": "launch", "parameters": {"machine_type": "large"}}
  Signature (still): sha256=abc123... (attacker cannot change without key)
  ↓
ggen System:
  1. Receive modified body
  2. Verify signature: sha256(modified_body) != abc123...
     (Signature only matches original body)
  3. HMAC verification FAILS
  4. Emit: permission_denied receipt
  5. Response: 401 Unauthorized
  ↓
Result: ATTACK FAILED
  - MITM detected (signature mismatch)
  - Modified body rejected
  - Original body was signed, tampering impossible
  - TLS is secondary defense (HMAC is primary)
```

**Why This Works**:
- ✅ HMAC signature is content-authenticated (detects any tampering)
- ✅ TLS provides transport security (MITM attacks fail at TLS level)
- ✅ Defense-in-depth: TLS + HMAC (both must be broken to succeed)

**Transport Security**:
```
1. TLS 1.3+ required (all connections)
2. Certificate pinning (prevent SSL stripping)
3. HSTS header: "Strict-Transport-Security: max-age=31536000"
4. HMAC signature: "X-Ggen-Signature: sha256=..."
5. Signature verification: Must match body exactly
```

---

## Red Team Summary

| Scenario | Attack Type | Attacker Goal | Result | Root Defense |
|----------|-------------|---------------|--------|--------------|
| 1 | Auth Bypass | Forge signature | FAILED | Entitlement verification |
| 2 | Escalation | Access expensive SKU | FAILED | Entitlement scope check |
| 3 | Permission Elevation | Delete other customer's SKU | FAILED | Tenant isolation |
| 4 | DoS/Quota Bypass | Exhaust quota | FAILED | Rate limiting |
| 5 | Data Corruption | Edit receipts | FAILED | Hash chain integrity |
| 6 | Forgery | Create fake entitlement | FAILED | Webhook source verification |
| 7 | Cascading Failure | Delete mid-action | FAILED | Graceful error handling |
| 8 | MITM | Modify webhook | FAILED | HMAC content authentication |

---

## Incident Classification

**For each attack scenario**:
- ✅ Attack detected (automated)
- ✅ Receipt emitted (audit trail)
- ✅ Severity assigned (CRITICAL if cross-tenant)
- ✅ Alert sent (to ops/security)
- ✅ Recovery automatic (no manual intervention)
- ✅ Forensics logged (who, when, what, how detected)

**Severity Levels**:
- 🔴 CRITICAL: Cross-tenant access, data tampering
- 🟠 HIGH: Escalation attempts, quota cheating
- 🟡 MEDIUM: Failed auth, invalid parameters
- 🟢 LOW: Malformed requests

---

## Receipt Contract

**Every red team attack MUST**:
- ✅ Be detected (no silent failures)
- ✅ Emit receipt (audit trail)
- ✅ Be refused (decision: refuse)
- ✅ Recover automatically (no manual intervention)
- ✅ Be logged with forensics (audit trail for investigation)

---

## Definition of Done

- [x] 8 complete red team scenarios documented
- [x] Attack path flow explained (step-by-step)
- [x] Expected receipt provided for each scenario
- [x] Root defense mechanism explained
- [x] Why it works documented (security rationale)
- [x] Incident classification provided
- [x] All attacks fail (proof of system resilience)
- [x] Glossary references included

