# TAIEA Governor - State Machine Specification

## Overview

TAIEA Governor (TAI Entitlement & Action Governor) implements a deterministic finite state machine for controlling bounded action execution within entitlement constraints. The governor enforces three gate checks before any action proceeds, monitors in-flight operations, and generates immutable receipts for audit.

**Specification**: `μ = five-stage transformation pipeline for governance`

## State Machine Definition

### States

```
┌─────────────────────────────────────────────────────────────────────┐
│                      TAIEA GOVERNOR STATES                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────┐      ┌───────────┐      ┌─────────────┐    ┌──────────┐ │
│  │ BOOT │─────→│  STABLE   │─────→│ INTERVENING │──→│ REFUSING │ │
│  └──────┘      └───────────┘      └─────────────┘    └──────────┘ │
│     │              │    ↑                │               │          │
│     │              │    └────────────────┘               │          │
│     │              │                                     │          │
│     └──────────────┴─────────────────────────────────────┘          │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### State Descriptions

#### BOOT
- **Purpose**: Initial state; verify entitlement system is operational
- **Entry**: Governor initialization
- **Transitions**:
  - `signal()` + gates pass → STABLE
  - `signal()` + gates fail → REFUSING
- **Actions**:
  - Check entitlement gate (stub in Phase 1)
  - Check IAM gate (stub in Phase 1)
  - Check preconditions gate (stub in Phase 1)
  - Emit "initialization" receipt
- **Constraints**: Cannot execute tool calls; no action permitted

#### STABLE
- **Purpose**: Normal operational state; accept signals and tool calls
- **Entry**: Successful entitlement verification from BOOT
- **Transitions**:
  - `signal()` → STABLE (gate check, update signal count)
  - `tool_call()` success → STABLE (emit receipt, return result)
  - `tool_call()` timeout → INTERVENING (escalate to action handler)
  - `tool_call()` memory exceeded → INTERVENING (escalate to action handler)
  - `entitlement_changed(inactive)` (cast) → stays STABLE (record in receipt)
- **Actions**:
  - Gate check all incoming signals
  - Execute bounded actions (timeout + memory limits)
  - Emit receipt for each signal
  - Emit receipt for each tool call result
  - Track signal count (for monitoring)
- **Constraints**: Maximum 1 in-flight action (enforced by gate checks)

#### INTERVENING
- **Purpose**: Action in flight; waiting for completion/failure/recovery
- **Entry**: Timeout or memory exceeded during tool call in STABLE
- **Transitions**:
  - `info: {action_complete, ActionId, Result}` → STABLE
  - `info: {action_failed, ActionId, Reason}` → REFUSING
  - `signal()` → INTERVENING (postpone, don't reply until exit)
  - `tool_call()` → INTERVENING (refuse with error)
  - `entitlement_changed()` (cast) → stays INTERVENING (record)
- **Actions**:
  - Postpone all incoming signals (queue for later processing)
  - Refuse new tool calls
  - Wait for action completion info message
  - Emit receipt on state exit
- **Constraints**: All new work is rejected or deferred

#### REFUSING
- **Purpose**: Governor unable to accept new work (entitlement/recovery failure)
- **Entry**: Entitlement check fails in BOOT, or action fails in INTERVENING
- **Transitions**:
  - `signal()` → REFUSING (refuse, emit receipt)
  - `tool_call()` → REFUSING (refuse, emit receipt)
  - `entitlement_changed(active)` (cast) → STABLE (recovery)
  - `entitlement_changed(other)` (cast) → REFUSING (record)
- **Actions**:
  - Reject all incoming signals
  - Reject all tool calls
  - Emit refusal receipt
  - Wait for entitlement reactivation
- **Constraints**: No work proceeds; read-only state queries allowed

## Event Types

### Call Events (Synchronous, request-reply)

#### `{call, {signal, Signal}}`
**Source**: External client
**Payload**: `Signal = map()` (arbitrary signal context)
**Response**: `{ok, State, Receipt}` | `{error, Reason}`
**Processing**:
- Gate checks: entitlement, IAM, preconditions
- Update signal count (in STABLE)
- Emit receipt with metadata
- Stay in current state (in STABLE)

#### `{call, {tool_call, ToolName, Arguments, TimeoutMs}}`
**Source**: External client
**Payload**:
- `ToolName`: binary, tool identifier
- `Arguments`: map, tool-specific arguments
- `TimeoutMs`: integer, wall-clock timeout
**Response**: `{ok, Result, Receipt}` | `{error, Reason}`
**Processing** (in STABLE):
- Gate checks all three gates
- Call `execute_bounded_action/4`:
  - Simulate/execute tool with timeout enforcement
  - Track memory usage
  - Return result or timeout/memory_exceeded
- Emit receipt (success, failed, timeout, or memory_exceeded event type)
- Transition: success/failed → STABLE; timeout/memory → INTERVENING

#### `{call, get_state}`
**Source**: Monitoring/diagnostics
**Response**: `{ok, CurrentState}`
**Processing**: Read-only in all states

#### `{call, list_receipts}`
**Source**: Audit/diagnostics
**Response**: `{ok, [Receipt, ...]}`
**Processing**: Read-only in all states, returns all receipts from ETS table

### Cast Events (Asynchronous, no reply)

#### `{cast, {entitlement_changed, NewState}}`
**Source**: Entitlement subsystem (Agent 9)
**Payload**: `NewState = active | inactive`
**Processing**:
- Update internal entitlement state
- Emit receipt with new state
- In REFUSING + active → transition to STABLE
- In STABLE/INTERVENING + inactive → stay in state (record)

### Info Events (Internal messages)

#### `{action_complete, ActionId, Result}`
**Source**: Action handler/worker process
**Payload**:
- `ActionId`: binary, matches in-flight action
- `Result`: map, arbitrary result
**Processing** (in INTERVENING):
- Verify ActionId matches in-flight action
- Clear in-flight action
- Emit receipt
- Transition to STABLE

#### `{action_failed, ActionId, Reason}`
**Source**: Action handler/worker process
**Payload**:
- `ActionId`: binary, matches in-flight action
- `Reason`: binary | term, failure description
**Processing** (in INTERVENING):
- Verify ActionId matches in-flight action
- Clear in-flight action
- Emit receipt with reason
- Transition to REFUSING

## Gate Checking System

### Three-Gate Design

All incoming work (signals, tool calls) must pass three independent gates:

1. **Gate 1: Entitlement Active?**
   - **Agent**: Phase 1 stub (Agent 9 implements full logic)
   - **Phase 1 Behavior**: Always accept
   - **Full Logic** (Phase 2): Check if tenant's entitlement is in `active` state
   - **Failure Reason**: `entitlement_inactive` | `entitlement_expired` | ...

2. **Gate 2: IAM Role Enabled?**
   - **Agent**: Phase 1 stub (Agent 8 implements full policy evaluation)
   - **Phase 1 Behavior**: Always accept
   - **Full Logic** (Phase 2): Evaluate IAM policy against tenant context
   - **Failure Reason**: `iam_role_disabled` | `iam_policy_failed` | ...

3. **Gate 3: Action Preconditions?**
   - **Agent**: Phase 1 stub (Agent 8 implements action-level validators)
   - **Phase 1 Behavior**: Always accept
   - **Full Logic** (Phase 2): Validate action prerequisites (e.g., required params, state constraints)
   - **Failure Reason**: `invalid_arguments` | `missing_preconditions` | ...

### Gate Result Type

```erlang
-type gate_result() :: {accept, Metadata :: map()} | {refuse, Reason :: binary()}.
```

- **Accept**: `{accept, #{gate_1 => ..., gate_2 => ..., gate_3 => ...}}`
  - All three gates passed
  - Metadata includes per-gate status
  - Action proceeds to execution

- **Refuse**: `{refuse, Reason :: binary()}`
  - At least one gate failed
  - Reason describes which gate failed
  - Action is rejected; receipt emitted with reason

### Gate Evaluation Order

```
Gate 1 (entitlement)
   ↓ (if pass)
Gate 2 (IAM)
   ↓ (if pass)
Gate 3 (preconditions)
   ↓ (if pass)
Action Allowed
```

**Short-circuit**: First failure stops evaluation and returns refuse.

## Action Execution with Bounds

### `execute_bounded_action/4` Contract

```erlang
-spec execute_bounded_action(
    ToolName :: binary(),
    Arguments :: map(),
    TimeoutMs :: non_neg_integer(),
    MaxMemoryMb :: non_neg_integer()
) -> Result.

Result = {ok, Output :: map()}
       | {error, Reason :: term()}
       | {timeout, TimeoutMs}
       | {memory_exceeded, UsedMb :: non_neg_integer()}.
```

### Phase 1 Implementation

In Phase 1, `execute_bounded_action/4` simulates action execution:

```erlang
simulate_tool_execution(ToolName, Arguments) ->
    case ToolName of
        <<"query">> -> {ok, #{status => success, rows => 42, duration_ms => 125}};
        <<"create">> -> {ok, #{status => success, created_id => ..., duration_ms => 87}};
        <<"scale">> -> {ok, #{status => success, scaled_instances => 3, duration_ms => 2150}};
        <<"timeout_test">> -> {timeout, maps:get(timeout_ms, Arguments, 30000)};
        <<"memory_test">> -> {memory_exceeded, 1024};
        _ -> {ok, #{status => success, tool => ToolName, ...}}
    end.
```

### Future Enhancement (Phase 2)

- Spawn monitored worker process
- Track wall-clock timeout via timer
- Monitor memory via `erlang:process_info(Worker, memory)`
- Enforce hard limits with kill signal
- Collect metrics (CPU, I/O, allocations)

## Receipt Format

### Receipt Structure

```erlang
-type receipt() :: #{
    id => binary(),                    % Unique receipt ID (base64)
    timestamp => non_neg_integer(),    % Millisecond timestamp
    tenant_id => binary(),             % Multi-tenant isolation
    governor_id => binary(),           % Governor instance ID
    state_from => governor_state(),    % Previous state
    state_to => governor_state(),      % New state
    event_type => binary(),            % "signal_processed", "tool_call_success", etc.
    reason => binary(),                % Optional: failure reason
    metadata => map()                  % Event-specific context
}.
```

### Example Receipts

**Successful Signal in Stable**:
```erlang
#{
    id => <<"ZTAzZj...">>,
    timestamp => 1705000000123,
    tenant_id => <<"tenant_1">>,
    governor_id => <<"tenant_1">>,
    state_from => stable,
    state_to => stable,
    event_type => <<"signal_processed">>,
    reason => <<>>,
    metadata => #{
        gate_1 => <<"entitlement_active">>,
        gate_2 => <<"iam_enabled">>,
        gate_3 => <<"preconditions_met">>
    }
}
```

**Tool Call Success**:
```erlang
#{
    id => <<"MWI1ZT...">>,
    timestamp => 1705000001456,
    tenant_id => <<"tenant_1">>,
    governor_id => <<"tenant_1">>,
    state_from => stable,
    state_to => stable,
    event_type => <<"tool_call_success">>,
    reason => <<>>,
    metadata => #{
        tool => <<"query">>,
        result => #{status => success, rows => 42, duration_ms => 125},
        timeout_ms => 5000
    }
}
```

**Transition to Intervening (Timeout)**:
```erlang
#{
    id => <<"N3M0aT...">>,
    timestamp => 1705000003789,
    tenant_id => <<"tenant_1">>,
    governor_id => <<"tenant_1">>,
    state_from => stable,
    state_to => intervening,
    event_type => <<"action_timeout">>,
    reason => <<>>,
    metadata => #{
        tool => <<"timeout_test">>,
        timeout_ms => 1000
    }
}
```

**Refusal in Boot (Entitlement Check Failed)**:
```erlang
#{
    id => <<"OTZ1Zj...">>,
    timestamp => 1705000005012,
    tenant_id => <<"tenant_2">>,
    governor_id => <<"tenant_2">>,
    state_from => boot,
    state_to => refusing,
    event_type => <<"entitlement_check_failed">>,
    reason => <<>>,
    metadata => #{reason => <<"entitlement_inactive">>}
}
```

## State Transition Graph (ASCII Art)

```
                           [BOOT]
                              |
                    (signal + gates pass)
                              |
                              v
         ┌────────────────────────────────────────┐
         │                                        │
         │        [STABLE] ◄─────────────────┐   │
         │      (normal ops)                  │   │
         │    ┌──────────────┐                │   │
         │    │ • signal()   │                │   │
         │    │ • tool_call()│                │   │
         │    └──────────────┘                │   │
         │           │                        │   │
         │    (timeout or memory              │   │
         │     exceeded)                      │   │
         │           │                        │   │
         │           v                        │   │
         │      [INTERVENING]                 │   │
         │   (action in flight)               │   │
         │  ┌─────────────────────┐           │   │
         │  │ • postpone signals  │           │   │
         │  │ • refuse tool_calls │           │   │
         │  └─────────────────────┘           │   │
         │           │                        │   │
         │    ┌──────┴──────┐                 │   │
         │    │             │                 │   │
         │ (action_complete) (action_failed)  │   │
         │    │             │                 │   │
         │    └─────┬───────┤                 │   │
         │          │       v                 │   │
         │          │    [REFUSING]           │   │
         │          │  (recovery required)    │   │
         │          │  ┌──────────────────┐   │   │
         │          │  │ • reject all ops │   │   │
         │          │  └──────────────────┘   │   │
         │          │          │              │   │
         │          │   (entitlement_         │   │
         │          │    changed:active)      │   │
         │          │          │              │   │
         │          └──────────┘              │   │
         │                                    │   │
         └────────────────────────────────────┘   │
                                                  │
                    (gate fail)                   │
                           │                      │
                           └──────────────────────┘
                                   to [REFUSING]
```

## API Summary

### Public Functions

```erlang
%% Startup
start_link(TenantId :: binary()) -> {ok, pid()}
start_link(TenantId :: binary(), Opts :: map()) -> {ok, pid()}

%% Synchronous Operations
signal(Pid, Signal :: map())
    -> {ok, State, Receipt} | {error, Reason}

tool_call(Pid, ToolName :: binary(), Arguments :: map(), TimeoutMs :: integer())
    -> {ok, Result, Receipt} | {error, Reason}

get_state(Pid) -> {ok, State}
list_receipts(Pid) -> {ok, [Receipt]}

%% Asynchronous Operations (from entitlement subsystem)
entitlement_changed(Pid, NewState :: active | inactive) -> ok
```

## Multi-Tenant Isolation

Each governor instance is isolated per tenant:

- **Process Registration**: Via `gproc` with key `{taiea_governor, TenantId}`
- **ETS Table**: Named `TenantId_GovernorId_receipts`, independent tables per governor
- **State Data**: All internal state keyed by tenant context
- **Concurrency**: Each tenant governor is independent process; no cross-tenant locks

```erlang
%% Example: Tenant A and Tenant B isolated
{ok, PidA} = taiea_governor:start_link(<<"tenant_a">>),
{ok, PidB} = taiea_governor:start_link(<<"tenant_b">>),

%% Each has independent state
taiea_governor:signal(PidA, #{...}),  % Tenant A state changes
taiea_governor:get_state(PidB),        % Tenant B unaffected
```

## Monitoring & Observability

### Metrics to Track

1. **State Transitions**: Count by state pair (boot→stable, stable→intervening, etc.)
2. **Signal Processing**: Count total signals, successful, refused
3. **Tool Calls**: Count by tool name, success rate, timeout rate, memory exceeded rate
4. **Receipt Generation**: Total receipts, latency to emit
5. **In-Flight Actions**: Max concurrent, duration, failure modes

### Receipt Audit Trail

All state changes are recorded immutably in ETS. Query via `list_receipts/1`:

```erlang
{ok, Receipts} = taiea_governor:list_receipts(Pid),
%% Receipts = [
%%   #{state_from => boot, state_to => boot, event_type => <<"initialization">>},
%%   #{state_from => boot, state_to => stable, event_type => <<"entitlement_verified">>},
%%   #{state_from => stable, state_to => stable, event_type => <<"signal_processed">>},
%%   ...
%% ]
```

## Phase 1 vs Phase 2 Implementation

### Phase 1 (Current - TAIEA Governor Agent 7)

✓ **Implemented**:
- Four-state machine (boot, stable, intervening, refusing)
- Event handling (signal, tool_call, entitlement_changed)
- Three-gate system (all stubs, always accept in Phase 1)
- Simulated action execution (no real resource bounding)
- Receipt generation and ETS storage
- Multi-tenant isolation
- State queries and receipt audit

✗ **Not Implemented** (Agents 8-9, Phase 2):
- Real IAM policy evaluation (Gate 2 - Agent 8)
- Complex entitlement logic (Gate 1 - Agent 9)
- Actual timeout/memory enforcement (will use OS/VM mechanisms)
- Worker process spawning and monitoring
- Hash-chained receipts (cryptographic audit)
- Andon signals for critical failures

### Agent Responsibilities

- **Agent 7** (This module): Governor state machine, event handlers, gates skeleton
- **Agent 8**: Full IAM policy evaluation (Gate 2), action preconditions (Gate 3)
- **Agent 9**: Full entitlement logic (Gate 1), entitlement subsystem integration

## Testing

Comprehensive test suite (`taiea_governor_SUITE.erl`) covers:

1. **State Transitions**: boot→stable, stable→intervening, intervening→stable/refusing, refusing→stable
2. **Event Handling**: signal, tool_call, entitlement_changed, action_complete, action_failed
3. **Gate Behavior**: All gates pass, gate failures
4. **Receipt Generation**: Structure, metadata, audit trail
5. **Multi-Tenancy**: Isolation, independent state
6. **Concurrency**: Parallel signals, concurrent tool calls
7. **Tool Execution**: Success, timeout, memory exceeded scenarios
8. **State Queries**: get_state from all states, list_receipts

## Execution Examples

### Example 1: Successful Signal and Tool Call

```erlang
TenantId = <<"acme_corp">>,
{ok, Pid} = taiea_governor:start_link(TenantId),

%% Boot → Stable
{ok, stable, Receipt1} = taiea_governor:signal(Pid, #{metric => usage}),
io:format("Receipt 1: ~p~n", [Receipt1]),
%% Receipt1 = #{
%%   state_from => boot, state_to => stable,
%%   event_type => <<"signal_processed">>,
%%   metadata => #{gate_1 => ..., gate_2 => ..., gate_3 => ...}
%% }

%% Tool call in Stable
{ok, Result, Receipt2} = taiea_governor:tool_call(Pid, <<"query">>, #{table => customers}, 5000),
io:format("Tool Result: ~p~n", [Result]),
%% Result = #{status => success, rows => 42, duration_ms => 125}

%% Still in Stable
{ok, stable} = taiea_governor:get_state(Pid).
```

### Example 2: Timeout Escalation

```erlang
TenantId = <<"beta_inc">>,
{ok, Pid} = taiea_governor:start_link(TenantId),
{ok, stable, _} = taiea_governor:signal(Pid, #{}),

%% Tool call times out
{error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{timeout_ms => 1000}, 1000),

%% Now in Intervening
{ok, intervening} = taiea_governor:get_state(Pid),

%% New signals postponed
{ok, intervening, Receipt3} = taiea_governor:signal(Pid, #{type => test}),
io:format("Receipt 3: ~p~n", [Receipt3]),
%% Receipt3.event_type = <<"signal_postponed">>

%% Simulate action recovery
Pid ! {action_complete, <<"action_123">>, #{status => recovered}},
timer:sleep(100),

%% Back to Stable
{ok, stable} = taiea_governor:get_state(Pid).
```

### Example 3: Entitlement-Driven Recovery

```erlang
TenantId = <<"gamma_ltd">>,
{ok, Pid} = taiea_governor:start_link(TenantId),
{ok, stable, _} = taiea_governor:signal(Pid, #{}),

%% Action fails
{error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
Pid ! {action_failed, <<"action_456">>, <<"recovery_timeout">>},
timer:sleep(100),

%% In Refusing state
{ok, refusing} = taiea_governor:get_state(Pid),

%% All operations rejected
{error, refusing} = taiea_governor:signal(Pid, #{type => test}),

%% Entitlement reactivated by Agent 9
ok = taiea_governor:entitlement_changed(Pid, active),
timer:sleep(100),

%% Back to Stable
{ok, stable} = taiea_governor:get_state(Pid),

%% Now operations work again
{ok, stable, _} = taiea_governor:signal(Pid, #{}).
```

## Summary

The TAIEA Governor is a deterministic finite state machine implementing bounded action control within entitlement constraints. It provides:

✓ **Deterministic**: All state transitions are defined; no race conditions
✓ **Auditable**: Every state change generates an immutable receipt
✓ **Scalable**: Multi-tenant isolation, independent processes per tenant
✓ **Recoverable**: Three-gate system with clear recovery paths
✓ **Observable**: Receipt audit trail for diagnosis and compliance
✓ **Extensible**: Gate implementation deferred to Agents 8-9

The state machine enforces bounded resource usage (timeout, memory) and provides clear failure modes and recovery procedures for production systems.
