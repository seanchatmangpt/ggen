# Action Contracts: Execution & Verification

**Version**: 1.0.0 (Production-Ready)
**Last Updated**: 2026-01-25
**Owner**: Autonomics Constitution Lead

---

## Executive Summary

This document specifies how the Governor issues and executes actions to control cloud infrastructure. Actions are **verified, authorized, and receipted** — every action execution is audited with explicit success or failure.

---

## Action Types & Specifications

### Supported Actions (Cloud + Platform Operations)

| Action ID | Service | Operation | Permission | Rollback |
|-----------|---------|-----------|-----------|----------|
| `scale_up_cloud_run` | Cloud Run | Increase replicas | `run.services.update` | `scale_down_cloud_run` |
| `scale_down_cloud_run` | Cloud Run | Decrease replicas | `run.services.update` | `scale_up_cloud_run` |
| `pause_cloud_run` | Cloud Run | Pause service | `run.services.update` | `resume_cloud_run` |
| `resume_cloud_run` | Cloud Run | Resume service | `run.services.update` | `pause_cloud_run` |
| `suspend_billing` | Cloud Billing | Suspend project | `billing.budgets.update` | `resume_billing` |
| `resume_billing` | Cloud Billing | Resume project | `billing.budgets.update` | `suspend_billing` |
| `revoke_permission` | IAM | Revoke role from principal | `iam.roles.update` | `grant_permission` |
| `grant_permission` | IAM | Grant role to principal | `iam.roles.update` | `revoke_permission` |
| `scale_gke_cluster` | GKE | Scale node pool | `container.nodePools.update` | Inverse scale |
| `enforce_cost_cap` | Cost Control | Cap daily spend | `monitoring.alertPolicies.create` | `disable_cost_cap` |

---

## Action Execution Contract

### Action Object Structure

```json
{
  "action_id": "uuid-v4",
  "action_type": "scale_up_cloud_run",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "target": {
    "service": "production-catalog-service",
    "region": "us-central1",
    "current_replicas": 5,
    "delta_replicas": 3,
    "max_replicas": 20
  },
  "created_at": "2026-01-25T14:32:15.123Z",
  "initiated_by": "governor",
  "policy_id": "policy-scale-up-on-cpu-gt-75",
  "signal_id": "signal-uuid-123"
}
```

### Action Lifecycle States

```
CREATED → VERIFIED → AUTHORIZED → SUBMITTED → [IN_FLIGHT] → COMPLETED
                                                                ├─ SUCCESS
                                                                ├─ TIMEOUT
                                                                └─ FAILED

    If fails at any point:
    ├─ FAILED_VERIFICATION → REFUSE_RECEIPT
    ├─ FAILED_AUTHORIZATION → PERMISSION_DENIED_RECEIPT
    ├─ FAILED_SUBMISSION → ACTION_FAILED_RECEIPT
    └─ ROLLBACK_ISSUED
```

### Execution Flow (Pseudocode)

```erlang
execute_action(Action, Data) ->
    %% 1. VERIFY: Check action is valid and policy-compliant
    case verify_action(Action, Data) of
        {error, Reason} ->
            emit_receipt(action_verification_failed, #{ reason => Reason }),
            {refuse, Data};
        {ok, VerifiedAction} ->
            %% 2. AUTHORIZE: Check IAM permission
            case authorize_action(VerifiedAction, Data#data.tenant_id) of
                {error, permission_denied} ->
                    emit_receipt(permission_denied, #{ action => VerifiedAction }),
                    {refuse, Data};
                {error, Reason} ->
                    emit_receipt(authorization_error, #{ reason => Reason }),
                    {degrade, Data};  % IAM unavailable → degrade
                {ok, Authorized} ->
                    %% 3. CHECK BOUNDS: Verify action won't violate constraints
                    case check_action_bounds(Authorized, Data) of
                        {error, Reason} ->
                            emit_receipt(bounds_check_failed, #{ reason => Reason }),
                            {refuse, Data};
                        {ok, _} ->
                            %% 4. SUBMIT: Issue action to service
                            case submit_action_to_service(Authorized) of
                                {ok, result} ->
                                    emit_receipt(action_succeeded, #{ action => Authorized }),
                                    {ok, Data};
                                {error, Reason} ->
                                    emit_receipt(action_failed, #{ reason => Reason }),
                                    %% Issue rollback
                                    issue_rollback(Authorized, Data),
                                    {error, Data}
                            end
                    end
            end
    end.
```

---

## Action Verification

### Verification 1: Action Is In Policy

**Check**: Action type must be in current approved policy.

```erlang
verify_action(Action, Data) ->
    ActionType = Action#action.action_type,
    PolicyId = Data#data.policy_id,
    case policy:action_allowed(ActionType, PolicyId) of
        true ->
            {ok, Action};
        false ->
            {error, action_not_in_policy}
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "policy_violation",
  "context": {
    "action_type": "scale_up_cloud_run",
    "policy_id": "policy-scale-up-on-cpu-gt-75",
    "reason": "action_not_in_policy"
  }
}
```

### Verification 2: Target Is Valid

**Check**: Target service/resource exists and is accessible.

```erlang
verify_target(Target) ->
    case cloud_run:describe_service(Target#target.service, Target#target.region) of
        {ok, ServiceDesc} ->
            %% Verify constraints (replicas, etc.)
            case check_service_constraints(ServiceDesc, Target) of
                {error, Reason} -> {error, Reason};
                {ok, _} -> {ok, Target}
            end;
        {error, not_found} ->
            {error, target_service_not_found};
        {error, Reason} ->
            {error, Reason}
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "policy_violation",
  "context": {
    "action_type": "scale_up_cloud_run",
    "target": "production-catalog-service",
    "reason": "target_service_not_found"
  }
}
```

### Verification 3: Signal-Action Relationship

**Check**: Action is appropriate for signal type (prevents mismatched remediation).

```erlang
verify_signal_action_match(Signal, Action) ->
    SignalType = Signal#signal.type,
    ActionType = Action#action.action_type,
    case policy:action_for_signal(SignalType) of
        {ok, ExpectedAction} ->
            case ActionType == ExpectedAction of
                true -> {ok, Action};
                false -> {error, signal_action_mismatch}
            end;
        not_found ->
            {ok, Action}  % No expected action, allow any policy-compliant action
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "policy_violation",
  "context": {
    "signal_type": "cpu_utilization",
    "action_type": "suspend_billing",
    "reason": "signal_action_mismatch"
  }
}
```

---

## Action Authorization (IAM)

### Authorization Check

**Check**: Principal has required IAM permission for action.

```erlang
authorize_action(Action, TenantId) ->
    ActionType = Action#action.action_type,
    RequiredPermission = permission_for_action(ActionType),
    Principal = get_service_account(TenantId),
    case iam:has_permission(Principal, RequiredPermission) of
        {ok, true} ->
            {ok, Action};
        {ok, false} ->
            {error, permission_denied};
        {error, Reason} ->
            {error, {iam_check_failed, Reason}}
    end.

permission_for_action('scale_up_cloud_run') ->
    {service, <<"run.services.update">>};
permission_for_action('scale_down_cloud_run') ->
    {service, <<"run.services.update">>};
permission_for_action('revoke_permission') ->
    {service, <<"iam.roles.update">>};
permission_for_action(_ActionType) ->
    {service, <<"compute.admin">>}. % Conservative default
```

### Receipt on Authorization Failure

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "permission_denied",
  "context": {
    "action_type": "scale_up_cloud_run",
    "required_permission": "run.services.update",
    "principal": "service-account-governance@acme-project.iam.gserviceaccount.com",
    "has_permission": false
  }
}
```

---

## Bounds Checking

### Bound 1: Replica Limits (Scale Actions)

**Check**: New replica count doesn't exceed max_replicas.

```erlang
check_replica_bounds(Action, ServiceDesc) ->
    MaxReplicas = ServiceDesc#service.max_replicas,
    CurrentReplicas = ServiceDesc#service.current_replicas,
    DeltaReplicas = Action#action.target#target.delta_replicas,
    NewReplicas = CurrentReplicas + DeltaReplicas,
    case NewReplicas =< MaxReplicas of
        true ->
            {ok, Action#action{ target = Action#action.target#target{ new_replicas = NewReplicas }}};
        false ->
            {error, {would_exceed_max_replicas, MaxReplicas}}
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "bounds_exceeded",
  "context": {
    "action_type": "scale_up_cloud_run",
    "max_replicas": 20,
    "current_replicas": 5,
    "delta": 20,
    "would_be": 25,
    "reason": "would_exceed_max_replicas"
  }
}
```

### Bound 2: Quota Check

**Check**: Action won't exceed customer's action quota.

```erlang
check_quota(Action, Data) ->
    Quota = Data#data.quota_remaining,
    case Quota > 0 of
        true ->
            {ok, Data#data{ quota_remaining = Quota - 1 }};
        false ->
            {error, quota_exceeded}
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "quota_exceeded",
  "context": {
    "action_type": "scale_up_cloud_run",
    "quota_remaining": 0,
    "quota_limit": 1000,
    "period": "monthly"
  }
}
```

### Bound 3: Cost Projection

**Check**: Action won't push customer over budget.

```erlang
check_cost_bounds(Action, Data) ->
    EstimatedCost = estimate_action_cost(Action),
    CurrentMonthCost = Data#data.current_month_cost,
    BudgetLimit = Data#data.budget_limit,
    ProjectedCost = CurrentMonthCost + EstimatedCost,
    case ProjectedCost =< BudgetLimit of
        true ->
            {ok, Action};
        false ->
            {error, would_exceed_budget}
    end.
```

**Receipt on Failure**:
```json
{
  "status": "refuse",
  "reason": "would_exceed_budget",
  "context": {
    "estimated_action_cost": 250.00,
    "current_month_cost": 44800.00,
    "budget_limit": 45000.00,
    "projected_cost": 45050.00
  }
}
```

---

## Action Submission SLO

### Submission Timeline

```
T0: action_attempted receipt emitted (async start)
T0+ΔT: HTTP request submitted to service
T0+ΔT': Service responds (status code, body)
T0+500ms: HARD TIMEOUT (if no response, fail)
  ├─ If response received: parse + emit action_succeeded or action_failed
  └─ If timeout: emit action_timeout + issue rollback
```

### SLO Bounds

| Operation | SLO | Timeout | Action on Timeout |
|-----------|-----|---------|-------------------|
| Signal validation | <100ms | N/A | N/A |
| Action verification | <200ms | <200ms | Refuse |
| IAM authorization | <200ms | <200ms | Degrade (IAM unreachable) |
| Action submission | <500ms | 500ms | Rollback + escalate |
| Receipt emission | <50ms | <50ms | Degrade (Firestore unreachable) |

### Submission Code (Non-Blocking)

```erlang
submit_action_to_service(Action) ->
    %% Spawn non-blocking HTTP call
    spawn_link(fun() ->
        case http_request(Action) of
            {ok, StatusCode, Headers, Body} ->
                handle_action_response(Action, StatusCode, Body);
            {error, timeout} ->
                governor:action_result({error, timeout}, Action);
            {error, Reason} ->
                governor:action_result({error, Reason}, Action)
        end
    end).

http_request(Action) ->
    Url = action_endpoint(Action#action.action_type),
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer ", (get_access_token())/binary>>}
    ],
    Payload = action_to_json(Action),
    http_client:post(Url, Payload, Headers, [{timeout, 500}]).
```

---

## Rollback Contract

### Rollback Trigger

Action rollback is issued immediately if:
1. Action submission fails (HTTP error)
2. Action times out (>500ms)
3. Action completes but signal not restored to baseline

### Rollback Specification

```erlang
issue_rollback(Action, Data) ->
    RollbackAction = #{
        action_id => uuid:v4(),
        action_type => rollback_of(Action#action.action_type),
        target => Action#action.target,
        original_action_id => Action#action.id,
        reason => get_rollback_reason(Action),
        created_at => rfc3339:now()
    },
    emit_receipt(action_rollback_issued, Data#data{ rollback_action = RollbackAction }),
    submit_action_to_service(RollbackAction).

rollback_of('scale_up_cloud_run') -> 'scale_down_cloud_run';
rollback_of('scale_down_cloud_run') -> 'scale_up_cloud_run';
rollback_of('pause_cloud_run') -> 'resume_cloud_run';
rollback_of('suspend_billing') -> 'resume_billing';
rollback_of('revoke_permission') -> 'grant_permission';
rollback_of(ActionType) -> error({no_rollback_for, ActionType}).
```

### Rollback Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "action_rollback_issued",
  "context": {
    "original_action_id": "action-uuid-123",
    "original_action_type": "scale_up_cloud_run",
    "rollback_action_type": "scale_down_cloud_run",
    "rollback_reason": "action_timeout|action_failed|baseline_not_restored",
    "rollback_action_id": "rollback-uuid-456"
  }
}
```

---

## Bounded Concurrency

### Constraint

**Maximum 1 action in flight at any time per Governor instance.**

```erlang
-record(data, {
    ...
    in_flight_action :: action() | undefined,
    action_queue :: queue:queue(action()),
    ...
}).
```

### Queue Discipline (FIFO)

```erlang
handle_event(cast, {issue_action, Action}, intervening, Data) ->
    %% Already in intervening (action in flight): queue new action
    NewQueue = queue:in(Action, Data#data.action_queue),
    emit_receipt(concurrency_limited, Data#data{ action_queue = NewQueue }),
    {keep_state, Data#data{ action_queue = NewQueue }};

handle_event(info, {action_result, _Result}, intervening, Data) ->
    %% Action completed: dequeue next action if any
    case queue:out(Data#data.action_queue) of
        {empty, _} ->
            %% No queued actions: return to stable
            {next_state, stable, Data#data{ in_flight_action = undefined }};
        {{value, NextAction}, NewQueue} ->
            %% Queued action available: submit it
            NewData = Data#data{
                in_flight_action = NextAction,
                action_queue = NewQueue
            },
            {keep_state, NewData, [{next_event, cast, {submit_action_async, NextAction}}]}
    end.
```

### Concurrency Limited Receipt

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "concurrency_limited",
  "context": {
    "action_type": "scale_up_cloud_run",
    "queue_length": 5,
    "in_flight_action_type": "scale_down_cloud_run",
    "in_flight_action_id": "action-uuid-999"
  }
}
```

---

## Failure Modes

### Failure 1: Action Verification Failed

**When**: Action not in policy or target doesn't exist.

**Receipt**: `policy_violation` + reason: `action_not_in_policy` or `target_service_not_found`

**Flow**:
```erlang
%% Do NOT submit action
%% Emit refuse receipt
%% Return to signal-originating state
```

### Failure 2: Permission Denied

**When**: Principal lacks IAM permission.

**Receipt**: `permission_denied` + required_permission, principal, has_permission=false

**Flow**:
```erlang
%% Do NOT submit action
%% Emit refuse receipt
%% Stay in current state (signal still pending)
%% Operator must grant permission + policy update
```

### Failure 3: Bounds Exceeded

**When**: Action would violate replica limit, quota, or budget.

**Receipt**: `bounds_exceeded` with specific constraint violated

**Flow**:
```erlang
%% Do NOT submit action
%% Emit refuse receipt
%% Try alternative remediation (lower delta, etc.)
```

### Failure 4: Action HTTP Error

**When**: Service returns non-200 status.

**Receipt**: `action_failed` + service_response_code, error_message, retry_countdown

**Flow**:
```erlang
%% Emit action_failed receipt
%% If retries > 0: retry (bounded by max_retries=3)
%% If retries = 0: emit rollback action
%% Transition to warning state
```

### Failure 5: Action Timeout

**When**: Action pending >500ms.

**Receipt**: `action_timeout` + original_action_id

**Flow**:
```erlang
%% Emit action_timeout receipt
%% Issue rollback action immediately
%% Transition to degraded state
%% Require manual intervention
```

### Failure 6: Rollback Failure

**When**: Rollback action also fails.

**Receipt**: `action_rollback_failed` + original_reason, rollback_reason

**Flow**:
```erlang
%% Log critical error
%% Escalate to ops team (manual intervention required)
%% Emit receipt with severity=CRITICAL
```

---

## Idempotency & Replay

### Idempotency Key

Each action includes idempotency key to prevent duplicate execution:

```json
{
  "action_id": "uuid-v4",
  "idempotency_key": "scale_up:production-catalog-service:2026-01-25T14:32:15.123Z",
  ...
}
```

### Idempotent Service Submission

```erlang
submit_action_to_service(Action) ->
    Url = action_endpoint(Action#action.action_type),
    Headers = [
        {<<"Idempotency-Key">>, Action#action.idempotency_key}
    ],
    http_client:post(Url, action_to_json(Action), Headers, [{timeout, 500}]).
```

**Service Guarantee**: If same idempotency_key submitted twice, service must:
1. Return same result (success or failure)
2. Not execute action twice

This ensures Governor can safely retry without double-execution.

---

## Definition of Done

- [ ] Action types documented (10+ actions with service, permission, rollback)
- [ ] Action object structure documented (JSON schema)
- [ ] Execution flow documented (5-step verification → authorization → bounds → submit → receipt)
- [ ] Verification contract defined (action in policy, target valid, signal-action match)
- [ ] Authorization contract defined (IAM permission check per action type)
- [ ] Bounds checking defined (replica limits, quota, cost projection)
- [ ] Action submission SLO documented (<500ms HARD TIMEOUT)
- [ ] Rollback contract documented (trigger conditions, rollback_of mapping, receipt)
- [ ] Bounded concurrency documented (max 1 in flight, FIFO queue, queue receipt)
- [ ] 6 failure modes documented (verification, permission, bounds, HTTP, timeout, rollback)
- [ ] Idempotency key strategy documented (idempotency_key field, service dedup)
- [ ] Cross-reference: governor-contract.md for action_attempted, action_succeeded, action_failed receipts
- [ ] Cross-reference: refusal-modes.md for refusal modes 4-5 (permission denied, policy violation)

---

## References

- **governor-contract.md** — Receipt structure, action state diagram
- **invariants.md** — Invariant 2 (permission required), 3 (quota not exceeded)
- **refusal-modes.md** — Refusal modes 4-5 (permission denied, policy violation)
- **signal-contracts.md** — Signal-to-action mapping
- **Cloud Run API**: https://cloud.google.com/run/docs/reference/rest
- **IAM Permissions**: https://cloud.google.com/iam/docs/understanding-permissions

---

**Last Updated**: 2026-01-25 | **Status**: Production-Ready
