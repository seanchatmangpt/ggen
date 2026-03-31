# TAIEA Governor - Architecture Overview

## System Context

```
┌─────────────────────────────────────────────────────────────┐
│                    TAI AUTONOMIC SYSTEM                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │  Entitlement │  │     IAM      │  │  Action Handlers│  │
│  │  (Agent 9)   │  │  (Agent 8)   │  │   (Agents...)   │  │
│  └──────┬───────┘  └──────┬───────┘  └────────┬────────┘  │
│         │                 │                     │           │
│         ├─────────────────┼─────────────────────┤           │
│         │                 │                     │           │
│  ┌──────▼─────────────────▼─────────────────────▼───────┐  │
│  │                                                       │  │
│  │          TAIEA GOVERNOR (Agent 7)                    │  │
│  │  ┌─────────────────────────────────────────────┐    │  │
│  │  │ State Machine                               │    │  │
│  │  │ • boot → stable → intervening ↔ refusing    │    │  │
│  │  │ • Event handlers (signal, tool_call, ...)  │    │  │
│  │  │ • Gate evaluation (3-gate system)           │    │  │
│  │  │ • Action execution with bounds              │    │  │
│  │  │ • Receipt generation and audit              │    │  │
│  │  └─────────────────────────────────────────────┘    │  │
│  │                                                       │  │
│  └────────────────────────────────────────────────────┘  │
│         ▲                                ▲                 │
│         │                                │                 │
│  ┌──────┴──────────────┐        ┌───────┴───────────┐    │
│  │   External Clients  │        │  Monitoring/      │    │
│  │  (signal, tool_call)│        │  Audit (receipts) │    │
│  └─────────────────────┘        └───────────────────┘    │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Module Architecture

### Core Components

```
taiea_governor.erl
├── State Machine (gen_statem)
│   ├── boot/3
│   ├── stable/3
│   ├── intervening/3
│   └── refusing/3
│
├── Gate System
│   ├── check_gates/2 (orchestrator)
│   ├── gate_1_entitlement_active/1 (stub → Agent 9)
│   ├── gate_2_iam_enabled/2 (stub → Agent 8)
│   └── gate_3_action_preconditions/2 (stub → Agent 8)
│
├── Action Execution
│   ├── execute_bounded_action/4
│   └── simulate_tool_execution/2 (Phase 1)
│
├── Receipt Management
│   ├── emit_receipt/5 (write)
│   ├── get_all_receipts/1 (read)
│   └── receipt_table_name/2 (table naming)
│
└── Utilities
    ├── generate_receipt_id/0
    ├── generate_action_id/0
    └── timestamp/0
```

## Data Structures

### Governor State Record

```erlang
-record(taiea_data, {
    tenant_id :: binary(),                    % Multi-tenant key
    governor_id :: binary(),                  % Instance identifier
    receipt_table :: atom(),                  % ETS table name
    entitlement_state :: active | inactive,   % Cached from Agent 9
    in_flight_action :: undefined | {...},    % {ActionId, Pid, StartTime}
    action_timeout_ms :: non_neg_integer(),   % Default timeout
    max_memory_mb :: non_neg_integer(),       % Memory limit
    created_at :: non_neg_integer(),          % Creation timestamp
    last_transition :: non_neg_integer(),     % Last state change
    signal_count :: non_neg_integer(),        % Metrics
    receipt_count :: non_neg_integer()        % Metrics
}).
```

### Receipt Structure

```erlang
-type receipt() :: #{
    id => binary(),                    % Unique ID (base64)
    timestamp => non_neg_integer(),    % Millisecond epoch
    tenant_id => binary(),             % Isolation key
    governor_id => binary(),           % Governor instance
    state_from => governor_state(),    % Previous state
    state_to => governor_state(),      % New state
    event_type => binary(),            % <<"signal_processed">>, ...
    reason => binary(),                % Optional: failure reason
    metadata => map()                  % Event context
}.
```

### Gate Result

```erlang
-type gate_result() :: {accept, Metadata :: map()}
                     | {refuse, Reason :: binary()}.

%% Example accept result
{accept, #{
    gate_1 => <<"entitlement_active">>,
    gate_2 => <<"iam_enabled">>,
    gate_3 => <<"preconditions_met">>
}}

%% Example refuse result
{refuse, <<"iam_policy_denied">>}
```

## State Transition Logic

### Boot State Handler

```erlang
boot(enter, _OldState, Data) ->
    {keep_state, Data};

boot({call, From}, {signal, Signal}, Data) ->
    case check_gates(Data#taiea_data.tenant_id, Signal) of
        {accept, Metadata} ->
            NewData = Data#taiea_data{entitlement_state = active},
            Receipt = emit_receipt(NewData, boot, stable, ...),
            {next_state, stable, NewData, [{reply, From, {ok, stable, Receipt}}]};
        {refuse, Reason} ->
            _Receipt = emit_receipt(Data, boot, refusing, ...),
            {next_state, refusing, Data, [{reply, From, {error, Reason}}]}
    end.
```

### Stable State Handler (Simplified)

```erlang
stable({call, From}, {signal, Signal}, Data) ->
    case check_gates(Data#taiea_data.tenant_id, Signal) of
        {accept, Metadata} ->
            NewData = Data#taiea_data{signal_count = Data#..+1},
            Receipt = emit_receipt(NewData, stable, stable, ...),
            {keep_state, NewData, [{reply, From, {ok, stable, Receipt}}]};
        {refuse, Reason} ->
            _Receipt = emit_receipt(Data, stable, stable, ...),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

stable({call, From}, {tool_call, ToolName, Arguments, TimeoutMs}, Data) ->
    case check_gates(Data#taiea_data.tenant_id, #{tool => ToolName, ...}) of
        {accept, _} ->
            case execute_bounded_action(ToolName, Arguments, TimeoutMs, ...) of
                {ok, Result} ->
                    Metadata = #{tool => ToolName, result => Result, ...},
                    Receipt = emit_receipt(Data, stable, stable, ...),
                    {keep_state, Data, [{reply, From, {ok, Result, Receipt}}]};

                {timeout, _} ->
                    _Receipt = emit_receipt(Data, stable, intervening, ...),
                    NewData = Data#taiea_data{in_flight_action = {...}},
                    {next_state, intervening, NewData, [{reply, From, {error, timeout}}]};

                {memory_exceeded, Used} ->
                    _Receipt = emit_receipt(Data, stable, intervening, ...),
                    NewData = Data#taiea_data{in_flight_action = {...}},
                    {next_state, intervening, NewData, [{reply, From, {error, memory_exceeded}}]}
            end;

        {refuse, Reason} ->
            _Receipt = emit_receipt(Data, stable, stable, ...),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end.
```

### Intervening State Handler

```erlang
intervening({call, From}, {signal, Signal}, Data) ->
    %% Postpone all signals while action in flight
    Receipt = emit_receipt(Data, intervening, intervening, <<"signal_postponed">>),
    {keep_state, Data, [{reply, From, {ok, intervening, Receipt}}]};

intervening(info, {action_complete, ActionId, Result}, Data) ->
    case Data#taiea_data.in_flight_action of
        {ActionId, _Pid, _StartTime} ->
            NewData = Data#taiea_data{in_flight_action = undefined},
            emit_receipt(NewData, intervening, stable, ...),
            {next_state, stable, NewData};
        _ ->
            {keep_state, Data}
    end;

intervening(info, {action_failed, ActionId, Reason}, Data) ->
    case Data#taiea_data.in_flight_action of
        {ActionId, _Pid, _StartTime} ->
            NewData = Data#taiea_data{in_flight_action = undefined},
            emit_receipt(NewData, intervening, refusing, ...),
            {next_state, refusing, NewData};
        _ ->
            {keep_state, Data}
    end.
```

### Refusing State Handler

```erlang
refusing({call, From}, {signal, _Signal}, Data) ->
    _Receipt = emit_receipt(Data, refusing, refusing, <<"signal_refused">>),
    {keep_state, Data, [{reply, From, {error, refusing}}]};

refusing(cast, {entitlement_changed, active}, Data) ->
    %% Recovery path: entitlement reactivated
    NewData = Data#taiea_data{entitlement_state = active},
    emit_receipt(NewData, refusing, stable, ...),
    {next_state, stable, NewData}.
```

## Gate Evaluation Pipeline

```
┌─────────────────────────────────────┐
│   check_gates(TenantId, Context)    │
└──────────────┬──────────────────────┘
               │
               ▼
        ┌──────────────┐
        │  Gate 1      │
        │ Entitlement  │
        │ (Agent 9)    │
        └──┬───────┬──┘
           │       │
      pass │       │ fail
           │       ▼
           │    {refuse, Reason}
           │       │
           │       └──────────┐
           │                  │
           ▼                  │
        ┌──────────────┐     │
        │  Gate 2      │     │
        │     IAM      │     │
        │  (Agent 8)   │     │
        └──┬───────┬──┘     │
           │       │        │
      pass │       │ fail   │
           │       ▼        │
           │    {refuse, Reason}
           │       │        │
           │       └────┐   │
           │            │   │
           ▼            │   │
        ┌──────────────┐│   │
        │  Gate 3      ││   │
        │Preconditions ││   │
        │  (Agent 8)   ││   │
        └──┬───────┬──┘│   │
           │       │   │   │
      pass │       │   │   │
           │       │   │   │
           ▼       ▼   ▼   ▼
        {accept,  {refuse, Reason}
         Metadata}
```

## Receipt Generation Flow

```
Every state change triggers receipt generation:

┌─────────────────┐
│  State Change   │ ← Signal, Tool Call, Action Complete, etc.
└────────┬────────┘
         │
         ▼
    ┌─────────────────────────────────────┐
    │  emit_receipt(Data, From, To, Type) │
    └────────┬────────────────────────────┘
             │
             ├─ Generate unique ID
             ├─ Capture timestamp
             ├─ Record state transition
             ├─ Include event metadata
             │
             ▼
    ┌─────────────────────────────┐
    │  Insert into ETS table      │
    │  ets:insert(Table, Receipt) │
    └────────┬────────────────────┘
             │
             ▼
    ┌─────────────────────────────┐
    │  Return Receipt to caller   │
    └─────────────────────────────┘
             │
             ▼
    ┌─────────────────────────────────┐
    │  Query via list_receipts/1      │
    │  ets:match_object(Table, ...)   │
    │  Returns full audit trail       │
    └─────────────────────────────────┘
```

## Multi-Tenant Isolation

```
┌────────────────────────────────────────────┐
│         Global Process Registry (gproc)    │
├────────────────────────────────────────────┤
│                                            │
│  {taiea_governor, <<"tenant_a">>} ─────┐  │
│  {taiea_governor, <<"tenant_b">>} ──┐  │  │
│  {taiea_governor, <<"tenant_c">>} ──┼─┼──┐  │
│                                      │ │  │  │
└──────────────────────────────────────┼─┼──┼──┘
                                       │ │  │
           ┌───────────────────────────┘ │  │
           │                             │  │
           ▼                             ▼  ▼
    ┌─────────────────┐         ┌─────────────────┐
    │ Governor PID A  │         │ Governor PID B  │
    │                 │         │                 │
    │ State: stable   │         │ State: boot     │
    │ Receipts: [R1]  │         │ Receipts: [R0]  │
    │                 │         │                 │
    │ tenant_a_abb_   │         │ tenant_b_abb_   │
    │ receipts (ETS)  │         │ receipts (ETS)  │
    └─────────────────┘         └─────────────────┘
```

## Action Execution Lifecycle

```
Caller requests tool_call
        │
        ▼
    ┌───────────────────────────┐
    │ In Stable State?          │ ─── No ──→ {error, State}
    └────────┬──────────────────┘
             │ Yes
             ▼
    ┌─────────────────────────┐
    │ Gates Pass?             │ ─── No ──→ {error, Reason}
    └────────┬────────────────┘
             │ Yes
             ▼
    ┌────────────────────────────────────────┐
    │ execute_bounded_action/4               │
    │ ├─ TimeoutMs enforced (simulated)      │
    │ └─ MaxMemoryMb tracked (simulated)     │
    └────────┬─────────────────────────────┐
             │                             │
       success                             failure/timeout/memory
             │                             │
             ▼                             ▼
    ┌──────────────────────┐      ┌────────────────────────┐
    │ {ok, Result}         │      │ {error|timeout|memory} │
    └──┬───────────────────┘      └──┬─────────────────────┘
       │                             │
       ├─ Emit "tool_call_success"   ├─ Emit "action_timeout"
       │   receipt                   │   or "memory_exceeded"
       │ Stay in Stable              │   receipt
       │                             │ Move to Intervening
       │                             │ Set in_flight_action
       │                             │
       └──────────────┬──────────────┘
                      │
                      ▼
           Return {ok|error, Receipt}
```

## Scalability Considerations

### Per-Tenant Process Isolation
- Each tenant gets independent governor process
- No cross-tenant locking or contention
- Scales horizontally with tenant count

### ETS-Based Receipt Storage
- In-memory table per governor
- Lock-free reads with write_concurrency
- Suitable for Phase 1; Agent 9 adds Firestore persistence

### Timeout Enforcement
- Phase 1: Timeout tracked but not enforced
- Phase 2: Will use OS-level timeout mechanisms

### Memory Tracking
- Phase 1: Memory limits checked but not enforced
- Phase 2: Will use VM memory monitoring

## Error Handling Strategy

### Gate Failures
```erlang
Gate 1 fails: {refuse, <<"entitlement_inactive">>}
Gate 2 fails: {refuse, <<"iam_policy_denied">>}
Gate 3 fails: {refuse, <<"missing_preconditions">>}

Response: {error, Reason} to caller
```

### Action Failures
```erlang
Timeout:        {error, timeout} → Move to Intervening
Memory Exceeded: {error, memory_exceeded} → Move to Intervening
Execution Error: {error, {Class, Reason}} → Move to Intervening
```

### Recovery Paths
```erlang
Intervening:
  ├─ action_complete → Stable (recovery success)
  └─ action_failed → Refusing (recovery failure)

Refusing:
  └─ entitlement_changed(active) → Stable (recovery)
```

## Performance Profile (Phase 1)

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| signal/2 | O(1) | Gate checks + receipt write |
| tool_call/4 | O(1) | Simulated execution |
| get_state/1 | O(1) | State query |
| list_receipts/1 | O(n) | n = receipt count |
| Gate evaluation | O(1) | 3 sequential checks |
| Receipt storage | O(1) | ETS insert |

## Integration Checklist for Agents 8-9

### Agent 8 (IAM Policy)
- [ ] Implement `gate_2_iam_enabled(TenantId, ActionContext)`
- [ ] Return `{accept, Metadata}` or `{refuse, Reason}`
- [ ] Integrate with existing IAM systems

### Agent 9 (Entitlement Manager)
- [ ] Implement `gate_1_entitlement_active(TenantId)`
- [ ] Provide `entitlement_changed/2` notification mechanism
- [ ] Implement Firestore persistence for receipts
- [ ] Add entitlement state tracking

## Summary

The TAIEA Governor is a deterministic, multi-tenant state machine that:

✓ Enforces entitlement-based access control via three-gate system
✓ Manages action execution with bounded resource constraints
✓ Generates immutable audit receipts for compliance
✓ Supports concurrent operations without locking
✓ Isolates tenant state per process
✓ Provides clear recovery paths for failures
✓ Extensible design for Agent 8-9 integration

The architecture is designed for production deployment with Phase 2 enhancements for real resource enforcement and cryptographic verification.
