<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Refusal Modes: System Explicit Denials](#refusal-modes-system-explicit-denials)
  - [Executive Summary](#executive-summary)
  - [Refusal Principle](#refusal-principle)
  - [Refusal 1: Missing Required Field](#refusal-1-missing-required-field)
    - [Scenario](#scenario)
    - [HTTP Response](#http-response)
    - [Receipt](#receipt)
    - [Remediation](#remediation)
    - [Code](#code)
  - [Refusal 2: Invalid Signature](#refusal-2-invalid-signature)
    - [Scenario](#scenario-1)
    - [HTTP Response](#http-response-1)
    - [Receipt](#receipt-1)
    - [Security Alert](#security-alert)
    - [Remediation](#remediation-1)
    - [Code](#code-1)
  - [Refusal 3: Entitlement Not Active](#refusal-3-entitlement-not-active)
    - [Scenario](#scenario-2)
    - [HTTP Response](#http-response-2)
    - [Receipt](#receipt-2)
    - [Remediation](#remediation-2)
    - [Code](#code-2)
  - [Refusal 4: Permission Denied](#refusal-4-permission-denied)
    - [Scenario](#scenario-3)
    - [HTTP Response](#http-response-3)
    - [Receipt](#receipt-3)
    - [Remediation](#remediation-3)
    - [Code](#code-3)
  - [Refusal 5: Policy Violation](#refusal-5-policy-violation)
    - [Scenario](#scenario-4)
    - [HTTP Response](#http-response-4)
    - [Receipt](#receipt-4)
    - [Remediation](#remediation-4)
    - [Code](#code-4)
  - [Refusal 6: Quota Exceeded](#refusal-6-quota-exceeded)
    - [Scenario](#scenario-5)
    - [HTTP Response](#http-response-5)
    - [Receipt](#receipt-5)
    - [Remediation](#remediation-5)
    - [Code](#code-5)
  - [Refusal 7: Firestore Unavailable](#refusal-7-firestore-unavailable)
    - [Scenario](#scenario-6)
    - [HTTP Response](#http-response-6)
    - [Receipt](#receipt-6)
    - [Fallback Mechanism](#fallback-mechanism)
    - [Governor State](#governor-state)
    - [Receipt (If Firestore Recovers)](#receipt-if-firestore-recovers)
    - [Remediation](#remediation-6)
  - [Refusal 8: Invalid State Transition](#refusal-8-invalid-state-transition)
    - [Scenario](#scenario-7)
    - [HTTP Response](#http-response-7)
    - [Receipt](#receipt-7)
    - [Behavior](#behavior)
    - [Remediation](#remediation-7)
  - [Refusal 9: Signal Storm Throttle](#refusal-9-signal-storm-throttle)
    - [Scenario](#scenario-8)
    - [HTTP Response](#http-response-8)
    - [Receipt](#receipt-8)
    - [Throttling Behavior](#throttling-behavior)
    - [Recovery](#recovery)
  - [Refusal 10: Action Timeout](#refusal-10-action-timeout)
    - [Scenario](#scenario-9)
    - [HTTP Response](#http-response-9)
    - [Receipt (action_attempted, then action_timeout)](#receipt-action_attempted-then-action_timeout)
    - [Governor State After Timeout](#governor-state-after-timeout)
    - [Remediation](#remediation-8)
  - [Refusal Summary Table](#refusal-summary-table)
  - [Receipt Anatomy (All Refusals)](#receipt-anatomy-all-refusals)
  - [Escalation Paths](#escalation-paths)
    - [Tier 1: Customer Self-Service](#tier-1-customer-self-service)
    - [Tier 2: System Operator](#tier-2-system-operator)
    - [Tier 3: Engineering Investigation](#tier-3-engineering-investigation)
  - [Definition of Done](#definition-of-done)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Refusal Modes: System Explicit Denials

**Version**: 1.0.0 (Production-Ready)
**Last Updated**: 2026-01-25
**Owner**: Autonomics Constitution Lead

---

## Executive Summary

**No humans support** — the Governor is explicit about what it will and will not do. This document catalogs all 10 ways the system **refuses** (explicitly denies) requests, with the exact receipt emitted and remediation path.

---

## Refusal Principle

**No Silent Failures**: Every refused request generates a receipt. Operators know exactly why their signals/actions were rejected.

**Refusal ≠ Error**: A refusal is a decision (policy enforced), not a bug (something broke). Refusals are expected and logged intentionally.

---

## Refusal 1: Missing Required Field

### Scenario

Signal or action request missing a required field (e.g., `source`, `severity`, `type`).

### HTTP Response

```
400 Bad Request
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "missing_required_field",
  "context": {
    "request_type": "signal",
    "missing_fields": [
      {
        "field": "source",
        "required": true,
        "provided": false
      }
    ],
    "http_code": 400,
    "validation_errors": [
      {
        "field": "source",
        "error": "missing"
      }
    ]
  }
}
```

### Remediation

```
Developer adds missing field to request:
  POST /signal/acme-catalog-v1/customer-123
  {
    "source": "monitoring",        ← ADD THIS
    "type": "cpu_utilization",
    "timestamp": "2026-01-25T14:32:15.123Z",
    "severity": "MEDIUM"
  }
```

### Code

```erlang
validate_required_fields(Signal) ->
    Required = [source, type, timestamp, severity],
    Missing = lists:filter(fun(Field) ->
        not maps:is_key(Field, Signal)
    end, Required),
    case Missing of
        [] -> {ok, Signal};
        _ -> {refuse, 400, missing_required_field, Missing}
    end.
```

---

## Refusal 2: Invalid Signature

### Scenario

Webhook signature (HMAC-SHA256) doesn't match. Request may be spoofed or corrupted.

### HTTP Response

```
403 Forbidden
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signature_invalid",
  "context": {
    "request_type": "signal",
    "signature_algorithm": "sha256",
    "expected_signature": "sha256=abcdef123456...",
    "provided_signature": "sha256=fedcba654321...",
    "http_code": 403,
    "security_action": "log_and_reject"
  }
}
```

### Security Alert

```erlang
emit_security_alert(signature_invalid, #{
    webhook_id => Header['X-Webhook-ID'],
    source_ip => Req:remote_addr(),
    timestamp => rfc3339:now(),
    severity => 'HIGH'
}).
```

### Remediation

```
1. Verify webhook signing secret is correct (same on both sides)
2. Verify payload is NOT modified after signing
3. Verify signature algorithm is sha256 (not md5, sha1, etc.)
4. Regenerate webhook signing key if compromised
```

### Code

```erlang
verify_signature(Req, Secret) ->
    ProvidedSig = proplists:get_value('X-Webhook-Signature', Req:headers()),
    Body = Req:body(),
    Timestamp = proplists:get_value('X-Webhook-Timestamp', Req:headers()),
    ContentToSign = [Body, Timestamp],
    ExpectedSig = "sha256=" ++ hex:encode(
        crypto:mac(hmac, sha256, Secret, ContentToSign)
    ),
    case constant_time_compare(ProvidedSig, ExpectedSig) of
        true -> {ok, verified};
        false -> {refuse, 403, signature_invalid, #{
            expected => ExpectedSig,
            provided => ProvidedSig
        }}
    end.
```

---

## Refusal 3: Entitlement Not Active

### Scenario

Customer's entitlement is INACTIVE or EXPIRED. No actions allowed until billing restored.

### HTTP Response

```
200 OK (but status="refuse")
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "entitlement_not_active",
  "context": {
    "request_type": "signal|action",
    "entitlement_status": "INACTIVE",
    "entitlement_expires": "2026-01-24T00:00:00Z",
    "days_overdue": 1,
    "http_code": 200,
    "remediation": "renew_entitlement",
    "customer_action": "purchase_plan_or_renew_subscription"
  }
}
```

### Remediation

```
Customer action:
  1. Log into GCP Marketplace account
  2. Find "acme-catalog-v1" product
  3. Renew subscription or upgrade plan
  4. Entitlement transitions to ACTIVE

Governor behavior:
  On next signal/action: check entitlement_status
  If ACTIVE: process normally
  Pending signals/actions: requeued automatically
```

### Code

```erlang
check_entitlement(TenantId) ->
    case fetch_entitlement(TenantId) of
        {ok, #{status := 'ACTIVE'}} ->
            {ok, active};
        {ok, #{status := Status}} when Status == 'INACTIVE'; Status == 'EXPIRED' ->
            {refuse, 200, entitlement_not_active, #{
                status => Status,
                remediation => purchase
            }};
        {error, Reason} ->
            {degrade, Reason}  % Entitlement service unavailable
    end.
```

---

## Refusal 4: Permission Denied

### Scenario

Principal lacks required IAM permission for action (least privilege enforced).

### HTTP Response

```
200 OK (but status="refuse")
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "permission_denied",
  "context": {
    "request_type": "action",
    "action_type": "scale_up_cloud_run",
    "required_permission": "run.services.update",
    "principal": "service-account-governance@acme-project.iam.gserviceaccount.com",
    "has_permission": false,
    "http_code": 200,
    "remediation": "grant_iam_role",
    "operator_action": "gcloud iam service-accounts add-iam-policy-binding ... --role roles/run.admin"
  }
}
```

### Remediation

```
Operator grants permission:
  gcloud iam service-accounts add-iam-policy-binding \
    service-account-governance@acme-project.iam.gserviceaccount.com \
    --role roles/run.admin \
    --member=serviceAccount:service-account-governance@acme-project.iam.gserviceaccount.com

Governor behavior:
  On next signal/action: re-check permission
  If granted: process normally
  Pending actions: requeued automatically
```

### Code

```erlang
check_iam_permission(Action, TenantId) ->
    RequiredPerm = permission_for_action(Action#action.action_type),
    Principal = get_service_account(TenantId),
    case iam:has_permission(Principal, RequiredPerm) of
        {ok, true} ->
            {ok, authorized};
        {ok, false} ->
            {refuse, 200, permission_denied, #{
                action => Action#action.action_type,
                required_permission => RequiredPerm,
                principal => Principal,
                remediation => grant_role
            }};
        {error, _Reason} ->
            {degrade, iam_unavailable}  % IAM service down
    end.
```

---

## Refusal 5: Policy Violation

### Scenario

Action not in approved policy, or signal-action pair doesn't match policy.

### HTTP Response

```
200 OK (but status="refuse")
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "policy_violation",
  "context": {
    "request_type": "action",
    "action_type": "scale_up_cloud_run",
    "policy_id": "policy-scale-up-on-cpu-gt-75",
    "policy_version": 5,
    "violation_reason": "action_not_in_policy|signal_action_mismatch|bounds_exceeded",
    "http_code": 200,
    "remediation": "update_policy",
    "operator_action": "edit policy in ggen ontology, deploy new version"
  }
}
```

### Remediation

```
Scenario 1: Action not in policy
  Edit .specify/specs/policy.ttl
  Add action to policy:
    :policy-scale-up-on-cpu-gt-75
      :allows :scale_up_cloud_run .
  Deploy: ggen sync --audit true
  Governor loads new policy (v6)
  Next signal: action allowed

Scenario 2: Signal-action mismatch
  Edit policy to map signal → action:
    :cpu_high :remediation :scale_up_cloud_run .
  Deploy new policy
  Governor revalidates pending signals
```

### Code

```erlang
check_policy_compliance(Action, PolicyId) ->
    case policy:action_allowed(Action#action.action_type, PolicyId) of
        true ->
            {ok, compliant};
        false ->
            {refuse, 200, policy_violation, #{
                action => Action#action.action_type,
                policy_id => PolicyId,
                reason => action_not_in_policy,
                remediation => update_policy
            }}
    end.
```

---

## Refusal 6: Quota Exceeded

### Scenario

Customer reached monthly action quota (e.g., 500 actions).

### HTTP Response

```
200 OK (but status="refuse")
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "quota_exceeded",
  "context": {
    "request_type": "action",
    "quota_type": "actions_per_month",
    "quota_remaining": 0,
    "quota_limit": 500,
    "period_start": "2026-01-01T00:00:00Z",
    "period_end": "2026-02-01T00:00:00Z",
    "days_until_reset": 7,
    "http_code": 200,
    "remediation": "upgrade_plan|wait_for_reset",
    "customer_action": "upgrade_plan_or_wait_until_2026-02-01"
  }
}
```

### Remediation

```
Option 1: Upgrade to higher quota tier
  Customer: purchase Professional plan (5000 actions/month)
  Governor: load new entitlement with higher quota
  Pending actions: requeued

Option 2: Wait for monthly reset
  Quota resets on 1st of month (UTC)
  Governor: reset quota_remaining = quota_limit
  Pending actions: requeued
```

### Code

```erlang
check_quota(TenantId) ->
    case fetch_quota_status(TenantId) of
        {ok, #{remaining := Remaining}} when Remaining > 0 ->
            {ok, available};
        {ok, #{remaining := 0, reset_date := ResetDate}} ->
            {refuse, 200, quota_exceeded, #{
                remaining => 0,
                reset_date => ResetDate,
                remediation => upgrade_or_wait
            }};
        {error, Reason} ->
            {degrade, Reason}
    end.
```

---

## Refusal 7: Firestore Unavailable

### Scenario

Cannot emit receipt to ledger → cannot proceed (jidoka principle: fail safe).

### HTTP Response

```
500 Internal Server Error
```

### Receipt

```
(No receipt emitted — Firestore unavailable)
```

### Fallback Mechanism

```erlang
emit_receipt_with_fallback(ReceiptType, Data) ->
    case firestore:append_receipt(ReceiptType, Data) of
        {ok, receipt_id} ->
            {ok, receipt_emitted};
        {error, timeout} ->
            emit_receipt_to_fallback_log(ReceiptType, Data),
            {degrade, firestore_unavailable};
        {error, _Reason} ->
            emit_receipt_to_fallback_log(ReceiptType, Data),
            {degrade, firestore_error}
    end.

emit_receipt_to_fallback_log(ReceiptType, Data) ->
    %% Log receipt to local file (for manual recovery)
    FallbackPath = "/var/log/governor/receipts_fallback.jsonl",
    Receipt = format_receipt(ReceiptType, Data),
    file:write_file(FallbackPath, jsx:encode(Receipt) ++ "\n", [append]).
```

### Governor State

```
Firestore unavailable detected:
  ├─ Emit degraded_mode receipt (if possible)
  ├─ Transition to degraded state
  ├─ Refuse all signals/actions (safe mode)
  ├─ Log to fallback
  └─ Retry Firestore health check every 10 seconds
```

### Receipt (If Firestore Recovers)

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "error",
  "reason": "health_check_failed",
  "context": {
    "component": "firestore",
    "failure_type": "connection_timeout|unavailable",
    "error_message": "Firestore: deadline exceeded after 5 retries",
    "http_code": 500,
    "recovery_action": "degrade_to_safe_mode",
    "retry_interval_seconds": 10
  }
}
```

### Remediation

```
Ops team:
  1. Investigate Firestore service status
  2. Check network connectivity to Firestore
  3. Verify service account credentials
  4. Once recovered: Governor detects on health check (60s interval)
  5. Transition: degraded → stable

Governor behavior:
  All signals/actions refused while degraded
  "service temporarily unavailable, retry later"
```

---

## Refusal 8: Invalid State Transition

### Scenario

FSM receives event invalid for current state (e.g., signal during boot).

### HTTP Response

```
200 OK (but status="refuse")
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "invalid_state_transition",
  "context": {
    "current_state": "boot",
    "event_type": "signal",
    "event_content": "cpu_high",
    "invalid_transition": "boot → signal_received",
    "http_code": 200,
    "valid_transitions_from_boot": ["entitlement_active", "timeout"],
    "remediation": "wait_for_entitlement_or_retry"
  }
}
```

### Behavior

```erlang
handle_event(cast, {signal, _Signal}, boot, Data) ->
    %% Signal during boot is invalid
    emit_receipt(invalid_state_transition, Data#data{
        current_state => boot,
        event => signal,
        reason => awaiting_entitlement
    }),
    {keep_state_and_data, [{postpone, true}]}.  % Requeue after boot complete
```

### Remediation

```
Automatic:
  Governor awaits entitlement_active receipt
  Once received: transition to stable
  Postponed signals: requeued automatically

Manual:
  If stuck in boot >30s: timeout triggers degradation
  Operator: check entitlement service, restart Governor if needed
```

---

## Refusal 9: Signal Storm Throttle

### Scenario

Signal rate exceeds 100/minute (throttle engaged to prevent resource exhaustion).

### HTTP Response

```
429 Too Many Requests
```

### Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signal_storm_throttle",
  "context": {
    "request_type": "signal",
    "current_rate": 147,
    "rate_limit": 100,
    "period_seconds": 60,
    "buffer_length": 987,
    "buffer_max": 1000,
    "http_code": 429,
    "retry_after_seconds": 30,
    "action": "buffer_signal_in_queue"
  }
}
```

### Throttling Behavior

```erlang
handle_signal_storm(Signal, TenantId, Data) ->
    Rate = count_signals_per_minute(TenantId),
    case Rate > 100 of
        true ->
            %% Storm: buffer signal
            NewBuffer = queue:in(Signal, Data#data.signal_buffer),
            emit_receipt(signal_storm_throttle, Data#data{ signal_buffer = NewBuffer }),
            {refuse, 429, signal_storm_throttle, NewBuffer};
        false ->
            %% Normal rate: process signal
            {ok, process_signal(Signal, Data)}
    end.
```

### Recovery

```
Automatic:
  Signal rate drops below 100/min
  Governor drains buffer (FIFO)
  Process buffered signals normally
  Emit signal_received receipts for each

Manual drain (if needed):
  Operator: governess:drain_signal_buffer(SkuId, TenantId)
```

---

## Refusal 10: Action Timeout

### Scenario

Action pending >500ms without response (force escalation).

### HTTP Response

```
200 OK (receipt emitted before timeout)
```

### Receipt (action_attempted, then action_timeout)

```json
[
  {
    "receipt_id": "uuid-v4-1",
    "reason": "action_attempted",
    "context": {
      "action_id": "action-uuid-123",
      "action_type": "scale_up_cloud_run"
    }
  },
  {
    "receipt_id": "uuid-v4-2",
    "reason": "action_timeout",
    "context": {
      "action_id": "action-uuid-123",
      "action_type": "scale_up_cloud_run",
      "timeout_ms": 500,
      "http_code": 200,
      "escalation_action": "rollback_issued",
      "next_state": "degraded"
    }
  }
]
```

### Governor State After Timeout

```erlang
handle_event(state_timeout, action_timeout, intervening, Data) ->
    %% Action pending >500ms: escalate
    emit_receipt(action_timeout, Data),
    emit_rollback_action(Data),
    {next_state, degraded, Data#data{ in_flight_action = undefined }}.
```

### Remediation

```
Automatic:
  Governor issues rollback action immediately
  Transitions to degraded state
  Pending signals/actions: refused until recovery

Manual:
  Operator: investigate what happened to the action
  Options:
    1. Verify action actually completed (idempotency key check)
    2. Retry action manually
    3. Restart Governor process
```

---

## Refusal Summary Table

| Mode | Trigger | HTTP | Status | Reason | Remediation |
|------|---------|------|--------|--------|-------------|
| 1 | Missing field | 400 | refuse | `missing_required_field` | Add field |
| 2 | Bad signature | 403 | refuse | `signature_invalid` | Verify secret |
| 3 | No entitlement | 200 | refuse | `entitlement_not_active` | Renew plan |
| 4 | No permission | 200 | refuse | `permission_denied` | Grant role |
| 5 | Not in policy | 200 | refuse | `policy_violation` | Update policy |
| 6 | Quota full | 200 | refuse | `quota_exceeded` | Upgrade/wait |
| 7 | Firestore down | 500 | error | `health_check_failed` | Repair Firestore |
| 8 | Invalid state | 200 | refuse | `invalid_state_transition` | Wait/retry |
| 9 | Rate > 100/min | 429 | refuse | `signal_storm_throttle` | Wait/buffer |
| 10 | Action >500ms | 200 | refuse | `action_timeout` | Investigate |

---

## Receipt Anatomy (All Refusals)

Every refusal receipt has this structure:

```json
{
  "receipt_id": "uuid-v4",                      // Unique identifier
  "timestamp": "2026-01-25T14:32:15.234Z",     // ISO 8601 UTC
  "sku_id": "acme-catalog-v1",                  // Marketplace SKU
  "tenant_id": "customer-123",                  // Customer ID
  "status": "refuse|error",                     // refuse (policy), error (system failure)
  "reason": "missing_required_field|...",       // Specific refusal mode
  "context": {
    "http_code": 400|403|429|500,
    "remediation": "human_action_required",
    "customer_action": "description",
    "operator_action": "description",
    ...
  }
}
```

---

## Escalation Paths

### Tier 1: Customer Self-Service
- Refusals 3, 5, 6 → Customer can resolve independently
- Examples: Renew entitlement, upgrade plan, update policy

### Tier 2: System Operator
- Refusals 2, 4, 8, 9 → Operator action required
- Examples: Investigate signature, grant IAM role, check state

### Tier 3: Engineering Investigation
- Refusals 7, 10 → System-level failure
- Examples: Firestore unavailable, action timeout

---

## Definition of Done

- [ ] All 10 refusal modes documented (trigger, HTTP, receipt, remediation)
- [ ] Refusal 1: Missing required field (400 Bad Request)
- [ ] Refusal 2: Invalid signature (403 Forbidden, security alert)
- [ ] Refusal 3: Entitlement not active (200 OK, billing integrity)
- [ ] Refusal 4: Permission denied (200 OK, least privilege)
- [ ] Refusal 5: Policy violation (200 OK, policy enforcement)
- [ ] Refusal 6: Quota exceeded (200 OK, quota enforcement)
- [ ] Refusal 7: Firestore unavailable (500 error, jidoka fail-safe)
- [ ] Refusal 8: Invalid state transition (200 OK, FSM validation)
- [ ] Refusal 9: Signal storm throttle (429 Too Many Requests)
- [ ] Refusal 10: Action timeout (200 OK, escalation)
- [ ] Receipt anatomy documented (all fields)
- [ ] Summary table complete (all 10 modes)
- [ ] Escalation paths documented (3 tiers)
- [ ] Code examples for each refusal (Erlang pseudocode)
- [ ] Cross-reference: governor-contract.md for FSM transitions
- [ ] Cross-reference: signal-contracts.md for refusals 1-2, 9
- [ ] Cross-reference: action-contracts.md for refusals 4-5, 10
- [ ] Cross-reference: invariants.md for refusals 3, 4, 5, 6, 7

---

## References

- **governor-contract.md** — Receipt types, FSM states
- **signal-contracts.md** — Signal validation, HTTP contract
- **action-contracts.md** — Action verification, authorization
- **invariants.md** — Invariant checks that trigger refusals
- **RFC 7807** — Problem Details for HTTP APIs (error format)
- **HMAC-SHA256**: https://tools.ietf.org/html/rfc4868

---

**Last Updated**: 2026-01-25 | **Status**: Production-Ready
