# System Invariants: Checkable Constraints

**Version**: 1.0.0 (Production-Ready)
**Last Updated**: 2026-01-25
**Owner**: Autonomics Constitution Lead

---

## Executive Summary

Invariants are **logical constraints that must hold in every state**. If an invariant fails, the system emits a receipt and refuses actions until the invariant is restored. All invariants are **checkable** (not aspirational) — they're verified at runtime.

---

## Invariant 1: Entitlement Active Required

### Statement

**If signal reception or action execution occurs, entitlement must be ACTIVE (billing integrity).**

### Checkable Condition

```erlang
check_invariant(entitlement_active_required, Data) ->
    case Data#data.entitlement_status of
        'ACTIVE' -> {ok, Data};
        'INACTIVE' -> {error, entitlement_inactive};
        'EXPIRED' -> {error, entitlement_expired}
    end.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Signal received | YES | Refuse signal (receipt: `policy_violation`) |
| Action attempted | YES | Refuse action (receipt: `policy_violation`) |
| Entitlement update | YES | Update status, re-check all invariants |
| State transition | YES | Transition only if invariant passes |

### State Transitions Affected

```
stable → intervening:    Check invariant BEFORE transition
boot → stable:          Entitlement status must be 'ACTIVE'
```

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "policy_violation",
  "context": {
    "invariant_violated": "entitlement_active_required",
    "entitlement_status": "INACTIVE",
    "action_attempted": "scale_up_cloud_run",
    "remediation": "renew entitlement or purchase plan"
  }
}
```

### Recovery Path

```erlang
%% Operator purchases/renews entitlement
governess:update_entitlement(SkuId, TenantId, 'ACTIVE', ExpiryDate).

%% Governor checks invariant on next event
%% If invariant passes: transition to stable
%% Pending signals/actions are requeued
```

---

## Invariant 2: Permission Required for Every Action

### Statement

**Before any action execution, principal must have required IAM permission.**

### Checkable Condition

```erlang
check_invariant(permission_required, Data) ->
    case Data#data.in_flight_action of
        undefined ->
            {ok, Data};  % No action in flight
        Action ->
            RequiredPermission = permission_for_action(Action#action.action_type),
            Principal = get_service_account(Data#data.tenant_id),
            case iam:has_permission(Principal, RequiredPermission) of
                {ok, true} ->
                    {ok, Data};
                {ok, false} ->
                    {error, {permission_denied, RequiredPermission, Principal}};
                {error, IamError} ->
                    %% IAM unreachable: degrade
                    {error, {iam_unreachable, IamError}}
            end
    end.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Action submission | YES | Refuse action (receipt: `permission_denied`) |
| Periodic invariant check | YES | If permission revoked, escalate to refusing state |
| IAM permission grant | YES | Update cached permission, retry action |

### Permission Matrix

| Action Type | Required Permission | Service |
|-------------|-------------------|---------|
| `scale_up_cloud_run` | `run.services.update` | Cloud Run |
| `scale_down_cloud_run` | `run.services.update` | Cloud Run |
| `pause_cloud_run` | `run.services.update` | Cloud Run |
| `resume_cloud_run` | `run.services.update` | Cloud Run |
| `revoke_permission` | `iam.roles.update` | IAM |
| `grant_permission` | `iam.roles.update` | IAM |
| `suspend_billing` | `billing.budgets.update` | Billing |
| `resume_billing` | `billing.budgets.update` | Billing |

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "permission_denied",
  "context": {
    "invariant_violated": "permission_required",
    "action_type": "scale_up_cloud_run",
    "required_permission": "run.services.update",
    "principal": "service-account-governance@acme-project.iam.gserviceaccount.com",
    "has_permission": false,
    "remediation": "grant run.services.update to service account"
  }
}
```

### Recovery Path

```erlang
%% Operator grants permission
iam:grant_role(Principal, <<"run.services.update">>).

%% Governor detects permission change
%% Re-checks invariant on next signal/action
%% If passes: retry queued actions
```

---

## Invariant 3: Quota Not Exceeded

### Statement

**Action quota per customer (per month) must not be exhausted.**

### Checkable Condition

```erlang
check_invariant(quota_not_exceeded, Data) ->
    Quota = Data#data.quota_remaining,
    case Quota > 0 of
        true ->
            {ok, Data};
        false ->
            {error, quota_exhausted}
    end.

decrement_quota(Data) ->
    NewQuota = Data#data.quota_remaining - 1,
    Data#data{ quota_remaining = NewQuota }.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Action attempted | YES | Refuse (receipt: `quota_exceeded`) |
| Decrement quota | YES | Decrement after action submitted |
| Monthly reset | YES | Reset to limit (e.g., 1000) on 1st of month |

### Quota Policies

| Customer Type | Monthly Quota | Reset |
|---------------|--------------|-------|
| Free | 50 | 1st of month |
| Starter | 500 | 1st of month |
| Professional | 5000 | 1st of month |
| Enterprise | Unlimited | N/A |

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "quota_exceeded",
  "context": {
    "invariant_violated": "quota_not_exceeded",
    "quota_remaining": 0,
    "quota_limit": 500,
    "period": "monthly",
    "reset_date": "2026-02-01T00:00:00Z",
    "remediation": "upgrade plan or wait for monthly reset"
  }
}
```

### Recovery Path

```erlang
%% Customer upgrades plan
billing:upgrade_plan(TenantId, <<"professional">>).

%% Governor loads new entitlement
%% New quota limit: 5000
%% Pending actions requeued

%% OR: Monthly reset
%% Every 1st of month: set quota_remaining = quota_limit
scheduler:every_month({first, day}, fun reset_quotas/1).
```

---

## Invariant 4: Receipt Ledger Available

### Statement

**If Firestore (receipt ledger) is unreachable, system must degrade to safe mode.**

### Checkable Condition

```erlang
check_invariant(ledger_available, Data) ->
    case firestore_health_check() of
        {ok, pong} ->
            {ok, Data};
        {error, timeout} ->
            {error, ledger_timeout};
        {error, unavailable} ->
            {error, ledger_unavailable}
    end.

firestore_health_check() ->
    Timeout = 1000,  % 1 second
    case catch firestore:ping(Timeout) of
        {ok, pong} -> {ok, pong};
        {error, deadline_exceeded} -> {error, timeout};
        {error, Reason} -> {error, Reason}
    end.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Every 60 seconds | YES | Degrade if unavailable |
| Receipt emission | YES | Degrade if Firestore unreachable |
| State transition | YES | Degrade if ledger unavailable |

### Health Check Interval

```erlang
init(Data) ->
    %% Start periodic health check
    {ok, boot, Data, [{state_timeout, 60000, health_check}]}.

handle_event(state_timeout, health_check, State, Data) ->
    case check_invariant(ledger_available, Data) of
        {ok, NewData} ->
            {keep_state, NewData, [{state_timeout, 60000, health_check}]};
        {error, Reason} ->
            emit_receipt(health_check_failed, Data#data{ reason = Reason }),
            {next_state, degraded, Data#data{ policy_id = undefined }}
    end.
```

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "error",
  "reason": "health_check_failed",
  "context": {
    "invariant_violated": "ledger_available",
    "component": "firestore",
    "failure_type": "timeout|unavailable",
    "error_message": "Firestore: deadline exceeded after 5 retries",
    "recovery_action": "degrade_to_safe_mode",
    "retry_interval_seconds": 10
  }
}
```

### Recovery Path

```erlang
%% Firestore service recovers
%% Governor detects recovery on next health check (60s interval)
%% Transition: degraded → stable

%% Or: Manual recovery trigger
governess:health_check_forced(SkuId, TenantId).
```

---

## Invariant 5: Policy Pack Loaded

### Statement

**Governor must have current policy loaded from ontology (required before any action).**

### Checkable Condition

```erlang
check_invariant(policy_loaded, Data) ->
    case Data#data.policy_id of
        undefined ->
            {error, policy_not_loaded};
        PolicyId ->
            case policy:get_by_id(PolicyId) of
                {ok, Policy} ->
                    {ok, Data#data{ policy = Policy }};
                {error, not_found} ->
                    {error, policy_not_found};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Transition to intervening | YES | Refuse action (degrade if policy unavailable) |
| Policy update signal | YES | Reload and check invariant |
| Periodic check | YES | Verify policy is current |

### Policy Load Sequence

```erlang
boot_sequence(Data) ->
    case load_policy(Data#data.sku_id) of
        {ok, Policy} ->
            emit_receipt(policy_loaded, Data#data{ policy_id = Policy#policy.id }),
            {next_state, stable, Data#data{ policy_id = Policy#policy.id }};
        {error, Reason} ->
            emit_receipt(policy_load_failed, Data),
            {next_state, degraded, Data}
    end.

load_policy(SkuId) ->
    case catch ontology:load_policy(SkuId, 300) of  % 300ms timeout
        {ok, Policy} ->
            {ok, Policy};
        {error, timeout} ->
            {error, policy_load_timeout};
        {error, Reason} ->
            {error, Reason}
    end.
```

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "error",
  "reason": "policy_load_failed",
  "context": {
    "invariant_violated": "policy_loaded",
    "sku_id": "acme-catalog-v1",
    "failure_type": "timeout|not_found|syntax_error",
    "error_message": "Ontology unavailable: connection timeout",
    "recovery_action": "degrade_to_safe_mode",
    "retry_interval_seconds": 10
  }
}
```

### Recovery Path

```erlang
%% Ontology service recovers
%% Governor detects and reloads policy

%% Or: Manual policy update
governess:reload_policy(SkuId, TenantId).
```

---

## Invariant 6: Signal Storm Prevention

### Statement

**If signal rate exceeds 100/minute from same tenant, throttle to prevent resource exhaustion.**

### Checkable Condition

```erlang
check_invariant(signal_storm_prevention, Data) ->
    Rate = count_signals_per_minute(Data#data.tenant_id),
    case Rate > 100 of
        true ->
            {error, {signal_storm_detected, Rate}};
        false ->
            {ok, Data}
    end.

count_signals_per_minute(TenantId) ->
    Now = erlang:monotonic_time(microsecond),
    OneMinuteAgo = Now - 60_000_000,
    firestore:count_signals(TenantId, OneMinuteAgo, Now).
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Signal received | YES | Throttle (429 response, buffer in queue) |
| Every 10 seconds | YES | Re-check rate, drain buffer if rate dropped |

### Throttling Behavior

```erlang
handle_signal(Signal, TenantId, Data) ->
    case check_invariant(signal_storm_prevention, Data) of
        {ok, _} ->
            %% Normal rate: process signal
            process_signal(Signal, Data);
        {error, {signal_storm_detected, Rate}} ->
            %% Storm detected: buffer signal
            emit_receipt(signal_storm_detected, Data#data{ rate = Rate }),
            buffer_signal(Signal, TenantId, Data)
    end.

buffer_signal(Signal, TenantId, Data) ->
    MaxBuffer = 1000,
    CurrentBuffer = get_signal_buffer(TenantId),
    case queue:len(CurrentBuffer) < MaxBuffer of
        true ->
            NewBuffer = queue:in(Signal, CurrentBuffer),
            store_signal_buffer(TenantId, NewBuffer),
            {ok, queued};
        false ->
            %% Buffer full: drop oldest
            {_Dropped, NewBuffer} = queue:out(CurrentBuffer),
            NewBuffer2 = queue:in(Signal, NewBuffer),
            store_signal_buffer(TenantId, NewBuffer2),
            {ok, queued_with_drop}
    end.
```

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signal_storm_detected",
  "context": {
    "invariant_violated": "signal_storm_prevention",
    "current_rate": 147,
    "limit": 100,
    "period_seconds": 60,
    "buffer_length": 987,
    "buffer_max": 1000,
    "action": "throttle_signals",
    "retry_after_seconds": 30
  }
}
```

### Recovery Path

```erlang
%% Rate drops below 100/min
%% Governor drains buffer on next check
drain_signal_buffer(TenantId) ->
    Rate = count_signals_per_minute(TenantId),
    case Rate =< 100 of
        true ->
            CurrentBuffer = get_signal_buffer(TenantId),
            process_buffer_signals(CurrentBuffer, TenantId);
        false ->
            %% Still in storm: retry later
            {postpone, drain_buffer_task}
    end.
```

---

## Invariant 7: Action Concurrency Bounded

### Statement

**Maximum 1 action in flight at any time (prevents race conditions, ensures fairness).**

### Checkable Condition

```erlang
check_invariant(action_concurrency_bounded, Data) ->
    InFlight = Data#data.in_flight_action,
    case InFlight of
        undefined ->
            {ok, Data};  % Action slot available
        _ ->
            {error, {action_in_flight, InFlight#action.id}}
    end.
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Action attempted | YES | Queue action (receipt: `concurrency_limited`) |
| Action completed | YES | Dequeue next action from FIFO queue |

### Queue Management

```erlang
handle_event(cast, {issue_action, Action}, intervening, Data) ->
    %% Already in intervening: action in flight
    NewQueue = queue:in(Action, Data#data.action_queue),
    emit_receipt(concurrency_limited, Data#data{ action_queue = NewQueue }),
    {keep_state, Data#data{ action_queue = NewQueue }};

handle_event(info, {action_result, _Result}, intervening, Data) ->
    %% Action completed: dequeue next
    case queue:out(Data#data.action_queue) of
        {empty, _} ->
            %% No queued actions: return to stable
            {next_state, stable, Data#data{ in_flight_action = undefined }};
        {{value, NextAction}, NewQueue} ->
            %% Queued action available: submit
            NewData = Data#data{
                in_flight_action = NextAction,
                action_queue = NewQueue
            },
            {keep_state, NewData, [{next_event, cast, {submit_action_async, NextAction}}]}
    end.
```

### Receipt on Violation (Concurrency Limited)

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "concurrency_limited",
  "context": {
    "invariant_checked": "action_concurrency_bounded",
    "action_type": "scale_up_cloud_run",
    "queue_length": 7,
    "in_flight_action_id": "action-uuid-999",
    "in_flight_action_type": "scale_down_cloud_run",
    "queued_at": "2026-01-25T14:32:15.456Z"
  }
}
```

### Fairness Guarantee

```
Action 1: submitted at T0, queued
Action 2: submitted at T0+100ms, queued
Action 3: submitted at T0+200ms, queued

When Action 0 completes:
  Action 1 dequeued first (FIFO)
  Actions 2, 3 remain queued

Guarantee: No action starves (FIFO order enforced)
```

---

## Invariant 8: Deterministic Behavior

### Statement

**Same input must produce same output (reproducible governance).**

### Checkable Condition

```erlang
check_invariant(deterministic_behavior, Receipt) ->
    ExpectedHash = compute_receipt_hash(Receipt),
    case ExpectedHash == Receipt#receipt.hash of
        true ->
            {ok, Receipt};
        false ->
            {error, {nondeterminism_detected, ExpectedHash, Receipt#receipt.hash}}
    end.

compute_receipt_hash(Receipt) ->
    Content = [
        Receipt#receipt.receipt_id,
        Receipt#receipt.sku_id,
        Receipt#receipt.tenant_id,
        Receipt#receipt.reason,
        maps:to_list(Receipt#receipt.context)  % Sorted by key
    ],
    crypto:hash(sha256, term_to_binary(Content)).
```

### When Checked

| Event | Check? | Action on Failure |
|-------|--------|-------------------|
| Receipt emission | YES | Log critical error, escalate |
| Replay verification | YES | Verify hash matches expected |

### Determinism Requirements

```erlang
%% 1. Same policy → same remediation action
Signal1 = #{type => cpu_high, value => 80.5},
Signal2 = #{type => cpu_high, value => 80.5},
Policy = policy_scale_up,

Action1 = lookup_remediation(Signal1, Policy),
Action2 = lookup_remediation(Signal2, Policy),
Action1 == Action2.  % MUST be true

%% 2. Same action → same service call
Action = #{type => scale_up_cloud_run, delta => 3},
Headers1 = build_headers(Action),
Headers2 = build_headers(Action),
Headers1 == Headers2.  % MUST be true

%% 3. Same signal sequence → same receipt sequence
Signals = [signal1, signal2, signal3],
Receipts1 = process_signals(Signals),
Receipts2 = process_signals(Signals),
Receipts1 == Receipts2.  % MUST be true
```

### Receipt on Violation

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "error",
  "reason": "nondeterminism_detected",
  "context": {
    "invariant_violated": "deterministic_behavior",
    "expected_hash": "sha256:abcdef123456...",
    "actual_hash": "sha256:fedcba654321...",
    "receipt_id": "receipt-uuid-123",
    "severity": "CRITICAL",
    "remediation": "manual_investigation_required"
  }
}
```

### Recovery Path

```erlang
%% Operator investigates hash mismatch
%% Identifies non-deterministic component:
%%   - RNG seed not fixed
%%   - System clock variance
%%   - Floating-point rounding
%%   - Hash collision
%%
%% Fix root cause + retry
%% Verify determinism restored
```

---

## Invariant Checking Summary Table

| Invariant | Condition | Check Trigger | Failure Action | Recovery |
|-----------|-----------|---------------|----------------|----------|
| 1. Entitlement | status == 'ACTIVE' | Signal/action | Refuse | Renew entitlement |
| 2. Permission | has_permission(principal) | Action | Refuse | Grant permission |
| 3. Quota | quota_remaining > 0 | Action | Refuse | Upgrade/reset |
| 4. Ledger | Firestore ping OK | Every 60s | Degrade | Firestore recovery |
| 5. Policy | policy_id != undefined | Before action | Degrade | Load policy |
| 6. Storm | rate <= 100/min | Every signal | Throttle | Rate drops |
| 7. Concurrency | in_flight_action == undefined | Action | Queue | Previous action ends |
| 8. Determinism | hash matches expected | Receipt | Log critical | Manual fix |

---

## Definition of Done

- [ ] All 8 invariants documented with clear statements
- [ ] Each invariant has checkable condition (Erlang pseudocode)
- [ ] Each invariant has "when checked" table (triggers)
- [ ] Each invariant has receipt schema (on violation)
- [ ] Each invariant has recovery path (how to restore)
- [ ] Invariant 1: Entitlement check logic + billing integrity
- [ ] Invariant 2: Permission matrix (8+ actions) + IAM check
- [ ] Invariant 3: Quota policies (4 customer types) + monthly reset
- [ ] Invariant 4: Health check interval (60s) + timeout handling
- [ ] Invariant 5: Policy load sequence + timeout (300ms)
- [ ] Invariant 6: Storm detection (100/min) + buffer management (max 1000)
- [ ] Invariant 7: Concurrency (max 1 action) + FIFO queue + fairness guarantee
- [ ] Invariant 8: Determinism hash algorithm + requirements
- [ ] Invariant checking summary table (all 8 rows complete)
- [ ] Cross-reference: governor-contract.md for invariants section
- [ ] Cross-reference: refusal-modes.md for refusal scenarios

---

## References

- **governor-contract.md** — Invariant checks in FSM, receipt structure
- **signal-contracts.md** — Invariant 6 (signal storm prevention)
- **action-contracts.md** — Invariant 2 (permission required), 3 (quota)
- **refusal-modes.md** — Refusals related to invariant violations
- **Erlang/OTP**: https://erlang.org/doc/
- **Firestore Health Check**: https://cloud.google.com/firestore/docs/reference/rest

---

**Last Updated**: 2026-01-25 | **Status**: Production-Ready
