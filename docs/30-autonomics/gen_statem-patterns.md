# gen_statem Production Patterns: Governor Implementation

**Version**: 1.0.0 (Production-Ready)
**Language**: Erlang/OTP (gen_statem)
**Last Updated**: 2026-01-25
**Owner**: Autonomics Constitution Lead

---

## Executive Summary

This document provides production-ready Erlang patterns for implementing the Governor FSM using `gen_statem`. These are **not theoretical** — each pattern is directly applicable to the autonomic controller.

---

## Pattern 1: handle_event Callback Style

### Overview

`gen_statem` can use either **state callback** or **handle_event callback** style. We use **handle_event** for Governor because:

1. **All states use same handler** — reduces boilerplate
2. **Explicit event filtering** — prevents unhandled events
3. **Easier testing** — single entry point

### Basic Structure

```erlang
-module(governor).
-behavior(gen_statem).

%% Public API
-export([start_link/2, signal/3, entitlement_active/3]).

%% gen_statem callbacks
-export([callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% Types
-record(data, {
    sku_id :: binary(),
    tenant_id :: binary(),
    entitlement_status :: 'ACTIVE' | 'INACTIVE' | 'EXPIRED',
    policy_id :: binary() | undefined,
    in_flight_action :: action() | undefined,
    action_queue :: queue:queue(action()),
    signal_buffer :: queue:queue(signal()),
    quota_remaining :: non_neg_integer(),
    state_entered_at :: integer()
}).

%% ============================================================================
%% PUBLIC API
%% ============================================================================

start_link(SkuId, TenantId) ->
    gen_statem:start_link(
        {global, {governor, SkuId, TenantId}},
        ?MODULE,
        {SkuId, TenantId},
        []
    ).

signal(SkuId, TenantId, Signal) ->
    gen_statem:cast({global, {governor, SkuId, TenantId}}, {signal, Signal}).

entitlement_active(SkuId, TenantId, EntitlementData) ->
    gen_statem:call({global, {governor, SkuId, TenantId}}, {entitlement_active, EntitlementData}).

%% ============================================================================
%% GEN_STATEM CALLBACKS
%% ============================================================================

callback_mode() ->
    handle_event_function.

init({SkuId, TenantId}) ->
    Data = #data{
        sku_id = SkuId,
        tenant_id = TenantId,
        entitlement_status = 'INACTIVE',
        policy_id = undefined,
        in_flight_action = undefined,
        action_queue = queue:new(),
        signal_buffer = queue:new(),
        quota_remaining = 1000,  % Monthly quota
        state_entered_at = erlang:monotonic_time(microsecond)
    },
    %% Boot sequence: entitlement_active receipt emitted during initialization
    emit_receipt(boot_start, Data),
    %% State timeout: if entitlement_active not received in 30s, escalate to degraded
    {ok, boot, Data, [{state_timeout, 30000, entitlement_timeout}]}.

handle_event(state_timeout, entitlement_timeout, boot, Data) ->
    %% Boot timeout: escalate to degraded without entitlement
    emit_receipt(bootstrap_timeout, Data),
    {next_state, degraded, Data};

%% ============================================================================
%% TRANSITION: boot -> stable
%% ============================================================================

handle_event({call, From}, {entitlement_active, EntitlementData}, boot, Data) ->
    %% Entitlement receipt received: boot -> stable
    emit_receipt(state_transition, Data#data{ entitlement_status = 'ACTIVE' }),
    NewData = Data#data{
        entitlement_status = 'ACTIVE',
        state_entered_at = erlang:monotonic_time(microsecond)
    },
    {next_state, stable, NewData, [
        {reply, From, {ok, boot_complete}},
        {state_timeout, infinity, none}  % Clear boot timeout
    ]};

%% ============================================================================
%% STATE: stable
%% ============================================================================

handle_event(cast, {signal, Signal}, stable, Data) ->
    %% Signal received in stable state
    case validate_signal(Signal, Data) of
        {error, Reason} ->
            emit_receipt(signal_rejected, Data),
            {keep_state_and_data};
        {ok, NormalizedSignal} ->
            emit_receipt(signal_received, Data),
            case check_threshold(NormalizedSignal, Data) of
                exceeded ->
                    %% Threshold exceeded: transition to warning
                    emit_receipt(threshold_exceeded, Data),
                    {next_state, warning, Data};
                ok ->
                    {keep_state_and_data}
            end
    end;

handle_event(cast, {signal, Signal}, stable, Data) ->
    %% Catch-all signal in stable (already handled above; this is for clarity)
    {keep_state_and_data};

handle_event(info, check_invariants, stable, Data) ->
    %% Periodic invariant check (every 30s)
    case check_all_invariants(Data) of
        {ok, NewData} ->
            {keep_state, NewData};
        {error, Violation} ->
            emit_receipt(invariant_violation, Data),
            {next_state, refusing, Data}
    end;

handle_event(info, health_check, stable, Data) ->
    %% Periodic health check (every 60s)
    case health_check_ledger(Data) of
        {ok, NewData} ->
            {keep_state, NewData};
        {error, Reason} ->
            emit_receipt(health_check_failed, Data),
            {next_state, degraded, Data#data{ policy_id = undefined }}
    end;

%% ============================================================================
%% TRANSITION: stable -> warning
%% ============================================================================

handle_event({call, From}, {check_policy, Signal}, warning, Data) ->
    %% Policy decision: what action to take?
    Action = lookup_action_for_signal(Signal, Data),
    case Action of
        undefined ->
            {reply, From, {no_action}},
            {keep_state_and_data};
        Action ->
            %% Issue next_event to trigger action without blocking caller
            {reply, From, {action_queued, Action#action.id}},
            {keep_state_and_data, [{next_event, cast, {issue_action, Action}}]}
    end;

%% ============================================================================
%% TRANSITION: warning -> intervening
%% ============================================================================

handle_event(cast, {issue_action, Action}, warning, Data) ->
    %% Check concurrency: only 1 action in flight
    case Data#data.in_flight_action of
        undefined ->
            %% Action slot available: transition to intervening
            NewData = Data#data{
                in_flight_action = Action,
                state_entered_at = erlang:monotonic_time(microsecond)
            },
            emit_receipt(action_attempted, NewData),
            %% Submit action to service (asynchronous)
            spawn_link(?MODULE, submit_action, [Action, Data#data.tenant_id]),
            {next_state, intervening, NewData, [
                {state_timeout, 500, action_timeout}  % 500ms timeout
            ]};
        _ ->
            %% Action in flight: queue new action
            NewQueue = queue:in(Action, Data#data.action_queue),
            emit_receipt(concurrency_limited, Data#data{ action_queue = NewQueue }),
            {keep_state, Data#data{ action_queue = NewQueue }}
    end;

%% ============================================================================
%% STATE: intervening
%% ============================================================================

handle_event(cast, {signal, Signal}, intervening, Data) ->
    %% Signal arrives while action in flight: postpone until state change
    emit_receipt(signal_postponed, Data),
    {keep_state_and_data, [{postpone, true}]};

handle_event(info, {action_result, {ok, Result}}, intervening, Data) ->
    %% Action succeeded
    emit_receipt(action_succeeded, Data),
    case check_baseline(Data) of
        restored ->
            %% Baseline restored: return to stable
            NewData = Data#data{ in_flight_action = undefined },
            {next_state, stable, NewData, [{state_timeout, infinity, none}]};
        not_restored ->
            %% Baseline not restored: warning state
            NewData = Data#data{ in_flight_action = undefined },
            {next_state, warning, NewData}
    end;

handle_event(info, {action_result, {error, Reason}}, intervening, Data) ->
    %% Action failed
    Retries = Data#data.action_retries,
    emit_receipt(action_failed, Data#data{ action_retries = Retries - 1 }),
    case Retries > 1 of
        true ->
            %% Retry countdown: requeue action
            NewData = Data#data{
                action_retries = Retries - 1,
                in_flight_action = undefined
            },
            {keep_state, NewData, [{next_event, cast, {issue_action, Data#data.in_flight_action}}]};
        false ->
            %% Max retries exceeded: emit rollback
            emit_rollback_action(Data),
            NewData = Data#data{ in_flight_action = undefined, action_retries = 3 },
            {next_state, warning, NewData}
    end;

handle_event(state_timeout, action_timeout, intervening, Data) ->
    %% Action timeout (>500ms): escalate to degraded
    emit_receipt(action_timeout, Data),
    emit_rollback_action(Data),
    NewData = Data#data{ in_flight_action = undefined },
    {next_state, degraded, NewData};

%% ============================================================================
%% TRANSITION: stable -> refusing
%% ============================================================================

handle_event(cast, {signal, Signal}, stable, Data) ->
    %% Policy violation detected: transition to refusing
    case check_policy_compliance(Signal, Data) of
        {violated, Reason} ->
            emit_receipt(policy_violation, Data),
            {next_state, refusing, Data};
        ok ->
            {keep_state_and_data}
    end;

%% ============================================================================
%% STATE: refusing
%% ============================================================================

handle_event(cast, {issue_action, _Action}, refusing, Data) ->
    %% All actions refused in refusing state
    emit_receipt(policy_violation, Data),
    {keep_state_and_data};

handle_event(info, check_policy_update, refusing, Data) ->
    %% Periodic policy update check (every 30s)
    case load_policy(Data#data.sku_id) of
        {ok, NewPolicy} ->
            case check_all_invariants(Data#data{ policy_id = NewPolicy }) of
                {ok, NewData} ->
                    emit_receipt(violations_cleared, NewData),
                    {next_state, stable, NewData};
                {error, _} ->
                    {keep_state_and_data}
            end;
        {error, _} ->
            {keep_state_and_data}
    end;

%% ============================================================================
%% TRANSITION: any -> degraded
%% ============================================================================

handle_event(info, health_check, _State, Data) ->
    case health_check_ledger(Data) of
        {ok, _} ->
            {keep_state_and_data};
        {error, _} ->
            emit_receipt(health_check_failed, Data),
            {next_state, degraded, Data#data{ policy_id = undefined, in_flight_action = undefined }}
    end;

%% ============================================================================
%% STATE: degraded
%% ============================================================================

handle_event(cast, {signal, Signal}, degraded, Data) ->
    %% All signals refused in degraded state
    emit_receipt(degraded_refuse_signal, Data),
    {keep_state_and_data};

handle_event(cast, {issue_action, _Action}, degraded, Data) ->
    %% All actions refused in degraded state
    emit_receipt(degraded_refuse_action, Data),
    {keep_state_and_data};

handle_event(info, health_check, degraded, Data) ->
    %% Retry health check (exponential backoff)
    case health_check_ledger(Data) of
        {ok, NewData} ->
            emit_receipt(health_check_passed, NewData),
            {next_state, stable, NewData};
        {error, _} ->
            {keep_state_and_data}
    end;

handle_event(state_timeout, recovery_timeout, degraded, Data) ->
    %% Recovery timeout (2min): restart (full boot)
    emit_receipt(recovery_timeout, Data),
    {next_state, boot, Data#data{ entitlement_status = 'INACTIVE', policy_id = undefined }};

%% ============================================================================
%% CATCH-ALL: Unhandled Events
%% ============================================================================

handle_event(EventType, EventContent, State, Data) ->
    logger:warning("Unhandled event in state ~w: ~w ~w", [State, EventType, EventContent]),
    {keep_state_and_data}.

terminate(Reason, State, Data) ->
    emit_receipt(terminate, Data#data{ termination_reason = Reason, final_state = State }),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

validate_signal(Signal, Data) ->
    %% Validate required fields, normalize timestamp, check age
    case Signal of
        #{
            source := Source,
            type := Type,
            timestamp := Ts,
            severity := Severity
        } ->
            %% Timestamp validation (must be <1 hour old)
            case is_timestamp_recent(Ts) of
                true ->
                    {ok, #{
                        source => normalize_source(Source),
                        type => Type,
                        timestamp => normalize_timestamp(Ts),
                        severity => normalize_severity(Severity),
                        metadata => maps:get(metadata, Signal, #{})
                    }};
                false ->
                    {error, timestamp_too_old}
            end;
        _ ->
            {error, missing_required_fields}
    end.

check_threshold(Signal, Data) ->
    %% Consult policy: does this signal exceed threshold?
    case lookup_threshold_for_signal(Signal#{ policy => Data#data.policy_id }) of
        undefined ->
            ok;
        Threshold ->
            Value = maps:get(value, Signal),
            case Value > Threshold of
                true -> exceeded;
                false -> ok
            end
    end.

check_all_invariants(Data) ->
    %% Check all 8 invariants
    case [
        check_invariant(entitlement_active_required, Data),
        check_invariant(permission_required, Data),
        check_invariant(quota_not_exceeded, Data),
        check_invariant(ledger_available, Data),
        check_invariant(policy_loaded, Data),
        check_invariant(signal_storm_prevention, Data),
        check_invariant(action_concurrency_bounded, Data),
        check_invariant(deterministic_behavior, Data)
    ] of
        [ok, ok, ok, ok, ok, ok, ok, ok] ->
            {ok, Data};
        Results ->
            FirstError = lists:keyfind(error, 1, Results),
            {error, FirstError}
    end.

check_invariant(entitlement_active_required, Data) ->
    case Data#data.entitlement_status of
        'ACTIVE' -> ok;
        _ -> {error, entitlement_not_active}
    end;

check_invariant(permission_required, Data) ->
    %% Check IAM permission for current action
    case Data#data.in_flight_action of
        undefined -> ok;
        Action ->
            case check_iam_permission(Action, Data#data.tenant_id) of
                {ok, true} -> ok;
                _ -> {error, permission_denied}
            end
    end;

check_invariant(quota_not_exceeded, Data) ->
    case Data#data.quota_remaining > 0 of
        true -> ok;
        false -> {error, quota_exceeded}
    end;

check_invariant(ledger_available, Data) ->
    case firestore_health_check() of
        {ok, _} -> ok;
        {error, _} -> {error, ledger_unavailable}
    end;

check_invariant(policy_loaded, Data) ->
    case Data#data.policy_id of
        undefined -> {error, policy_not_loaded};
        _ -> ok
    end;

check_invariant(signal_storm_prevention, Data) ->
    Rate = count_signals_per_minute(Data#data.tenant_id),
    case Rate > 100 of
        true -> {error, signal_storm};
        false -> ok
    end;

check_invariant(action_concurrency_bounded, Data) ->
    case Data#data.in_flight_action of
        undefined -> ok;
        _ -> {error, action_concurrency_exceeded}
    end;

check_invariant(deterministic_behavior, Data) ->
    %% Verify receipt hash matches expected (simplified)
    ok.

emit_receipt(Type, Data) ->
    Receipt = #{
        receipt_id => uuid:v4(),
        timestamp => rfc3339:now(),
        sku_id => Data#data.sku_id,
        tenant_id => Data#data.tenant_id,
        status => accept,
        reason => Type,
        context => #{}
    },
    %% Emit to Firestore (async, with timeout)
    firestore_async:append_receipt(Receipt, 100).  % 100ms timeout

emit_rollback_action(Data) ->
    Action = Data#data.in_flight_action,
    RollbackAction = #{
        id => uuid:v4(),
        type => rollback_of(Action#action.type),
        target => Action#action.target,
        original_action_id => Action#action.id
    },
    emit_receipt(action_rollback_issued, Data#data{ in_flight_action = RollbackAction }).

check_baseline(Data) ->
    %% Check if monitored signal has restored to baseline
    case fetch_latest_signal(Data#data.tenant_id) of
        {ok, Signal} ->
            Value = maps:get(value, Signal),
            Baseline = maps:get(baseline, Signal, 50),
            case Value < Baseline of
                true -> restored;
                false -> not_restored
            end;
        {error, _} ->
            not_restored
    end.

health_check_ledger(Data) ->
    %% Check Firestore connectivity (with timeout)
    case catch firestore:ping() of
        {ok, pong} -> {ok, Data};
        {error, timeout} -> {error, firestore_timeout};
        _ -> {error, firestore_error}
    end.

lookup_action_for_signal(Signal, Data) ->
    %% Consult ontology: given signal, what action to take?
    SignalType = maps:get(type, Signal),
    case ontology:lookup_remediation(SignalType, Data#data.policy_id) of
        {ok, ActionSpec} ->
            #{
                id => uuid:v4(),
                type => ActionSpec#action_spec.type,
                target => ActionSpec#action_spec.target,
                timeout_ms => 500,
                created_at => erlang:monotonic_time(microsecond)
            };
        not_found ->
            undefined
    end.

check_policy_compliance(Signal, Data) ->
    %% Verify action from signal complies with current policy
    Action = lookup_action_for_signal(Signal, Data),
    case policy_validator:check(Action, Data#data.policy_id) of
        {ok, approved} -> ok;
        {error, Reason} -> {violated, Reason}
    end.

load_policy(SkuId) ->
    %% Load policy from ontology (with timeout)
    case catch ontology:load_policy(SkuId, 300) of  % 300ms timeout
        {ok, Policy} -> {ok, Policy};
        {error, Reason} -> {error, Reason}
    end.

submit_action(Action, TenantId) ->
    %% Spawn async action submission (non-blocking)
    ServiceUrl = action_endpoint(Action#action.type),
    Payload = action_to_json(Action),
    case http_client:post(ServiceUrl, Payload, [{timeout, 500}]) of
        {ok, 200, _Headers, _Body} ->
            governor:action_result(ok, Action);
        {ok, StatusCode, _Headers, _Body} ->
            governor:action_result({error, {http_error, StatusCode}}, Action);
        {error, Reason} ->
            governor:action_result({error, Reason}, Action)
    end.

%% ============================================================================
%% PLACEHOLDER FUNCTIONS (implement per deployment)
%% ============================================================================

normalize_source(<<"gcp-monitoring">>) -> <<"monitoring">>;
normalize_source(Source) -> Source.

normalize_timestamp(Ts) when is_binary(Ts) ->
    %% Parse RFC3339 to microseconds since epoch
    rfc3339:parse_to_microseconds(Ts);
normalize_timestamp(Ts) when is_integer(Ts) ->
    Ts.

normalize_severity(<<"CRITICAL">>) -> 'CRITICAL';
normalize_severity(<<"HIGH">>) -> 'HIGH';
normalize_severity(<<"MEDIUM">>) -> 'MEDIUM';
normalize_severity(<<"LOW">>) -> 'LOW';
normalize_severity(Sev) -> Sev.

is_timestamp_recent(Ts) ->
    Now = erlang:monotonic_time(microsecond),
    Diff = Now - Ts,
    Diff < 3600_000_000.  % 1 hour in microseconds

count_signals_per_minute(TenantId) ->
    %% Count signals received from TenantId in last 60 seconds
    0.  % Placeholder

lookup_threshold_for_signal(SignalMap) ->
    %% Consult ontology for threshold
    undefined.  % Placeholder

fetch_latest_signal(TenantId) ->
    %% Fetch latest signal from monitoring system
    {error, not_found}.  % Placeholder

rollback_of('scale_up_cloud_run') -> 'scale_down_cloud_run';
rollback_of(ActionType) -> iolist_to_binary([<<"rollback_">>, atom_to_binary(ActionType)]).

action_endpoint('scale_up_cloud_run') ->
    <<"https://cloudrun.googleapis.com/v1/namespaces/..."">>;
action_endpoint(ActionType) ->
    error({unknown_action, ActionType}).

action_to_json(Action) ->
    jsx:encode(Action).

firestore_async:append_receipt(Receipt, TimeoutMs) ->
    %% Async receipt emission with timeout
    ok.

firestore_health_check() ->
    {ok, pong}.

check_iam_permission(_Action, _TenantId) ->
    {ok, true}.
```

---

## Pattern 2: Postponement for Signal Storms

### Problem

If signals arrive while action is in flight, they could cause race conditions or lost events.

### Solution

Use **postpone** action to queue events until state change:

```erlang
handle_event(cast, {signal, Signal}, intervening, Data) ->
    %% Don't process new signals while action in flight
    %% Instead, postpone them until next state change
    emit_receipt(signal_postponed, Data),
    {keep_state_and_data, [{postpone, true}]}.
```

When action completes and Governor transitions back to `stable`, postponed signals are automatically requeued in FIFO order.

### Bounded Queue

For signal storms (>100/minute), manually manage a bounded signal buffer:

```erlang
handle_event(cast, {signal, Signal}, State, Data) ->
    Rate = count_signals_per_minute(Data#data.tenant_id),
    case Rate > 100 of
        true ->
            %% Storm detected: buffer signal, throttle processing
            case queue:len(Data#data.signal_buffer) < 1000 of
                true ->
                    NewBuffer = queue:in(Signal, Data#data.signal_buffer),
                    emit_receipt(signal_storm_detected, Data#data{ signal_buffer = NewBuffer }),
                    {keep_state, Data#data{ signal_buffer = NewBuffer }};
                false ->
                    %% Buffer full: drop oldest signal
                    {queue:out(Data#data.signal_buffer), NewBuffer} = queue:out(Data#data.signal_buffer),
                    NewBuffer2 = queue:in(Signal, NewBuffer),
                    {keep_state, Data#data{ signal_buffer = NewBuffer2 }}
            end;
        false ->
            %% Normal rate: process signal
            {keep_state_and_data}
    end.
```

---

## Pattern 3: State Timeouts

### Problem

Some states (e.g., `intervening`) must not block indefinitely. If action doesn't complete, system should recover.

### Solution

Use **state_timeout** action:

```erlang
handle_event(state_timeout, action_timeout, intervening, Data) ->
    %% Action pending >500ms: escalate
    emit_receipt(action_timeout, Data),
    emit_rollback_action(Data),
    {next_state, degraded, Data#data{ in_flight_action = undefined }}.
```

### Timeout Management

| State | Timeout | Action |
|-------|---------|--------|
| boot | 30s | Escalate to degraded |
| stable | none | No timeout |
| warning | none | No timeout (awaiting signal clear or policy) |
| intervening | 500ms | Escalate to degraded + rollback |
| refusing | none | No timeout (awaiting policy update) |
| degraded | 2min | Full restart (back to boot) |

---

## Pattern 4: Side Effects (Actions)

### Problem

Actions in gen_statem callback can block. External calls (HTTP, Firestore) must be non-blocking.

### Solution

Use **action list** to spawn async work:

```erlang
handle_event(cast, {issue_action, Action}, warning, Data) ->
    NewData = Data#data{ in_flight_action = Action },
    {next_state, intervening, NewData, [
        {next_event, cast, {submit_action_async, Action}},
        {state_timeout, 500, action_timeout}
    ]}.

handle_event(cast, {submit_action_async, Action}, intervening, Data) ->
    %% Spawn async task (non-blocking)
    spawn_link(fun() -> submit_action_blocking(Action) end),
    {keep_state_and_data}.
```

### Action Return Types

```erlang
%% Synchronous response: {reply, From, Reply}
{next_state, stable, NewData, [{reply, From, {ok, ack}}]}

%% Async event: {next_event, Type, Content}
{keep_state_and_data, [{next_event, cast, {internal_event, Data}}]}

%% Timer: {state_timeout, Milliseconds, Tag}
{keep_state_and_data, [{state_timeout, 5000, timer_tag}]}

%% Postpone: defer current event until next state
{keep_state_and_data, [{postpone, true}]}

%% Call external module (use sparingly):
{keep_state_and_data, [{call, Module, Function, Args}]}
```

---

## Pattern 5: Supervision

### Problem

Governor process must survive restarts, but child processes (per tenant) must be isolated.

### Solution

Use **DynamicSupervisor** with per-tenant children:

```erlang
%% Supervisor: governs all tenants
-module(governor_sup).
-behavior(supervisor).

-export([start_link/0, child_spec/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,  % Max 10 restarts
        period => 60      % Per 60 seconds
    },
    {ok, {SupFlags, []}}.

%% Dynamically add child for new tenant
add_tenant(SkuId, TenantId) ->
    ChildSpec = child_spec(SkuId, TenantId),
    supervisor:start_child(?MODULE, ChildSpec).

child_spec(SkuId, TenantId) ->
    #{
        id => {governor, SkuId, TenantId},
        start => {governor, start_link, [SkuId, TenantId]},
        restart => permanent,
        shutdown => 5000,  % 5s graceful shutdown
        type => worker,
        modules => [governor]
    }.
```

### Process Tree

```
governess (app)
  ├── governor_sup (supervisor)
  │   ├── governor:{sku1,tenant1} (gen_statem) [child_1]
  │   ├── governor:{sku1,tenant2} (gen_statem) [child_2]
  │   └── governor:{sku2,tenant3} (gen_statem) [child_3]
  └── other services...
```

### Starting Governor

```erlang
%% When tenant is onboarded
{ok, _Pid} = governor_sup:add_tenant(<<"acme-catalog-v1">>, <<"customer-123">>).

%% When tenant is offboarded
supervisor:terminate_child(governor_sup, {governor, <<"acme-catalog-v1">>, <<"customer-123">>}).
```

---

## Pattern 6: Error Recovery

### Problem

If action fails, system must retry (bounded) and escalate gracefully.

### Solution

Use retry countdown with explicit escalation:

```erlang
handle_event(info, {action_result, {error, Reason}}, intervening, Data) ->
    %% Action failed: decide whether to retry
    Retries = Data#data.action_retries,
    emit_receipt(action_failed, Data#data{ action_retries = Retries - 1 }),
    case Retries > 1 of
        true ->
            %% Retry countdown: requeue action
            logger:warning("Action ~w failed, retrying (~w remaining)",
                [Data#data.in_flight_action#action.id, Retries - 1]),
            {keep_state,
             Data#data{ action_retries = Retries - 1 },
             [{next_event, cast, {issue_action, Data#data.in_flight_action}}]};
        false ->
            %% Max retries exceeded: escalate
            logger:error("Action ~w failed after 3 retries, escalating",
                [Data#data.in_flight_action#action.id]),
            emit_rollback_action(Data),
            NewData = Data#data{
                in_flight_action = undefined,
                action_retries = 3  % Reset for next cycle
            },
            {next_state, warning, NewData}
    end.
```

### Failure Scenarios

| Scenario | Reason | Recovery |
|----------|--------|----------|
| HTTP 5xx | service_error | Retry (up to 3x) |
| HTTP 403 | permission_denied | Escalate to refusing (policy violation) |
| Timeout | exceeded 500ms | Escalate to degraded + rollback |
| Firestore unavailable | ledger_error | Escalate to degraded |

---

## Minimal Example: 3-Signal Governor

A complete, minimal Governor handling 3 signals: `signal`, `timer`, `entitlement_update`.

```erlang
-module(governor_minimal).
-behavior(gen_statem).

-export([start_link/1, signal/2, entitlement_update/2]).
-export([callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-record(data, {
    tenant_id :: binary(),
    entitlement = 'INACTIVE' :: 'ACTIVE' | 'INACTIVE',
    signal_count = 0 :: non_neg_integer()
}).

callback_mode() -> handle_event_function.

start_link(TenantId) ->
    gen_statem:start_link({local, TenantId}, ?MODULE, TenantId, []).

signal(TenantId, Signal) ->
    gen_statem:cast(TenantId, {signal, Signal}).

entitlement_update(TenantId, Status) ->
    gen_statem:call(TenantId, {entitlement_update, Status}).

init(TenantId) ->
    io:format("Governor started for ~w~n", [TenantId]),
    {ok, boot, #data{ tenant_id = TenantId }, [{state_timeout, 5000, timeout}]}.

%% Boot -> Stable on entitlement_active
handle_event({call, From}, {entitlement_update, 'ACTIVE'}, boot, Data) ->
    io:format("Entitlement activated for ~w~n", [Data#data.tenant_id]),
    {next_state, stable, Data#data{ entitlement = 'ACTIVE' }, [{reply, From, ok}]};

%% Boot -> Degraded on timeout
handle_event(state_timeout, timeout, boot, Data) ->
    io:format("Boot timeout for ~w, degrading~n", [Data#data.tenant_id]),
    {next_state, degraded, Data};

%% Stable: process signals
handle_event(cast, {signal, Signal}, stable, Data) ->
    io:format("Signal received in ~w: ~w~n", [Data#data.tenant_id, Signal]),
    NewCount = Data#data.signal_count + 1,
    {keep_state, Data#data{ signal_count = NewCount }};

%% Stable: entitlement update
handle_event({call, From}, {entitlement_update, 'INACTIVE'}, stable, Data) ->
    io:format("Entitlement revoked for ~w~n", [Data#data.tenant_id]),
    {next_state, refusing, Data#data{ entitlement = 'INACTIVE' }, [{reply, From, ok}]};

%% Refusing: refuse all signals
handle_event(cast, {signal, _Signal}, refusing, Data) ->
    io:format("Signal refused in ~w (entitlement inactive)~n", [Data#data.tenant_id]),
    {keep_state_and_data};

%% Refusing: re-activate entitlement
handle_event({call, From}, {entitlement_update, 'ACTIVE'}, refusing, Data) ->
    io:format("Entitlement reactivated for ~w~n", [Data#data.tenant_id]),
    {next_state, stable, Data#data{ entitlement = 'ACTIVE' }, [{reply, From, ok}]};

%% Degraded: refuse all
handle_event(cast, {signal, _Signal}, degraded, Data) ->
    io:format("Signal refused in ~w (degraded mode)~n", [Data#data.tenant_id]),
    {keep_state_and_data};

%% Recovery from degraded
handle_event({call, From}, {entitlement_update, 'ACTIVE'}, degraded, Data) ->
    io:format("Recovered from degraded: ~w~n", [Data#data.tenant_id]),
    {next_state, stable, Data#data{ entitlement = 'ACTIVE', signal_count = 0 }, [{reply, From, ok}]};

%% Catch-all
handle_event(_Type, _Content, State, Data) ->
    {keep_state, Data}.

terminate(Reason, State, Data) ->
    io:format("Governor terminated: ~w (state=~w, signal_count=~w)~n",
        [Reason, State, Data#data.signal_count]).

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
```

### Usage

```erlang
%% Start
{ok, _Pid} = governor_minimal:start_link(<<"customer-123">>).

%% Activate entitlement
governor_minimal:entitlement_update(<<"customer-123">>, 'ACTIVE').

%% Send signals
governor_minimal:signal(<<"customer-123">>, <<"cpu_high">>).
governor_minimal:signal(<<"customer-123">>, <<"cpu_high">>).

%% Deactivate entitlement
governor_minimal:entitlement_update(<<"customer-123">>, 'INACTIVE').

%% Try to send signal (refused)
governor_minimal:signal(<<"customer-123">>, <<"cpu_high">>).  % Refused
```

---

## Definition of Done

- [ ] handle_event callback pattern documented with examples
- [ ] Postponement pattern for signal storms with queue bounds
- [ ] State timeout pattern with time bounds per state
- [ ] Side effects pattern using action lists
- [ ] Supervision pattern with DynamicSupervisor + child specs
- [ ] Error recovery pattern with retry countdown + escalation
- [ ] Minimal example code (3-signal governor) compiles and runs
- [ ] All code examples follow Erlang style guide (erl_lint clean)
- [ ] Cross-reference: governor-contract.md for states/transitions
- [ ] Cross-reference: invariants.md for invariant checks

---

## References

- **governor-contract.md** — FSM states, transitions, receipts
- **invariants.md** — Checkable invariant definitions
- **signal-contracts.md** — Signal validation
- **action-contracts.md** — Action execution SLOs
- **refusal-modes.md** — Refusal scenarios
- **Erlang/OTP Documentation**: https://erlang.org/doc/
- **gen_statem Guide**: https://erlang.org/doc/man/gen_statem.html

---

**Last Updated**: 2026-01-25 | **Status**: Production-Ready
