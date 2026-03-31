# TAIEA Governor - Quick Start Guide

## 5-Minute Setup

### 1. Start Erlang Shell

```bash
cd /Users/sac/ggen/tai-erlang-autonomics
rebar3 shell
```

### 2. Start a Governor

```erlang
%% Start governor for tenant
{ok, Pid} = taiea_governor:start_link(<<"demo_tenant">>).

%% Check initial state (boot)
{ok, boot} = taiea_governor:get_state(Pid).

%% All output should be {ok, X} indicating success
```

### 3. Signal and Transition to Stable

```erlang
%% Send signal from boot → transitions to stable
{ok, stable, Receipt1} = taiea_governor:signal(Pid, #{type => startup}).

%% Receipt shows boot → stable transition
io:format("~p~n", [Receipt1]).

%% Verify now in stable state
{ok, stable} = taiea_governor:get_state(Pid).
```

### 4. Execute Tool Call

```erlang
%% Simple query tool call
{ok, Result, Receipt2} = taiea_governor:tool_call(Pid, <<"query">>, #{}, 5000).

%% Result includes status and metadata
io:format("Result: ~p~n", [Result]).
%% Result: #{status => success, rows => 42, duration_ms => 125}

%% Receipt documents tool execution
io:format("Receipt: ~p~n", [Receipt2]).
```

### 5. Query Audit Trail

```erlang
%% Get all receipts (state change history)
{ok, Receipts} = taiea_governor:list_receipts(Pid).

%% Print receipt count and types
io:format("Total Receipts: ~w~n", [length(Receipts)]),

%% Print each receipt's state transition
lists:foreach(fun(#{state_from := From, state_to := To, event_type := Type}) ->
    io:format("  ~p → ~p: ~s~n", [From, To, Type])
end, Receipts).

%% Output:
% Total Receipts: 4
%   boot → boot: <<"initialization">>
%   boot → stable: <<"entitlement_verified">>
%   stable → stable: <<"signal_processed">>
%   stable → stable: <<"tool_call_success">>
```

## Testing Timeout Handling

```erlang
%% Start fresh governor
{ok, Pid2} = taiea_governor:start_link(<<"timeout_demo">>).
{ok, stable, _} = taiea_governor:signal(Pid2, #{}).

%% Trigger timeout scenario
{error, timeout} = taiea_governor:tool_call(Pid2, <<"timeout_test">>, #{}, 1000).

%% Governor moves to intervening state
{ok, intervening} = taiea_governor:get_state(Pid2).

%% Simulate action recovery
Pid2 ! {action_complete, <<"action_123">>, #{recovered => true}},
timer:sleep(100),

%% Back to stable
{ok, stable} = taiea_governor:get_state(Pid2).
```

## Multi-Tenant Isolation Demo

```erlang
%% Create two independent governors
{ok, PidA} = taiea_governor:start_link(<<"tenant_a">>),
{ok, PidB} = taiea_governor:start_link(<<"tenant_b">>),

%% Move only PidA to stable
{ok, stable, _} = taiea_governor:signal(PidA, #{}),

%% Verify isolation
{ok, stable} = taiea_governor:get_state(PidA),
{ok, boot} = taiea_governor:get_state(PidB).  % PidB still in boot!

%% Receipts are isolated
{ok, ReceiptsA} = taiea_governor:list_receipts(PidA),  % Has 2 receipts
{ok, ReceiptsB} = taiea_governor:list_receipts(PidB),  % Has 1 receipt

io:format("Tenant A Receipts: ~w~n", [length(ReceiptsA)]),
io:format("Tenant B Receipts: ~w~n", [length(ReceiptsB)]).
```

## Key Behaviors to Explore

### 1. Boot State Only Accepts Gates
```erlang
{ok, Pid} = taiea_governor:start_link(<<"test">>),

%% Only signal works in boot; tool calls not in this demo
{ok, stable, _} = taiea_governor:signal(Pid, #{}).
```

### 2. Stable State Processes Everything
```erlang
{ok, Pid} = taiea_governor:start_link(<<"test">>),
{ok, stable, _} = taiea_governor:signal(Pid, #{}),

%% Signals processed
{ok, stable, _} = taiea_governor:signal(Pid, #{type => a}),
{ok, stable, _} = taiea_governor:signal(Pid, #{type => b}),

%% Tool calls executed
{ok, #{status := success}, _} = taiea_governor:tool_call(Pid, <<"create">>, #{}, 5000),
{ok, #{status := success}, _} = taiea_governor:tool_call(Pid, <<"scale">>, #{}, 5000).
```

### 3. Timeout Escalates to Intervening
```erlang
{ok, Pid} = taiea_governor:start_link(<<"test">>),
{ok, stable, _} = taiea_governor:signal(Pid, #{}),

{error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
{ok, intervening} = taiea_governor:get_state(Pid),

%% While intervening, new signals postponed
{ok, intervening, Receipt} = taiea_governor:signal(Pid, #{type => test}),
#{event_type := <<"signal_postponed">>} = Receipt.
```

### 4. Entitlement Changes Drive Transitions
```erlang
{ok, Pid} = taiea_governor:start_link(<<"test">>),
{ok, stable, _} = taiea_governor:signal(Pid, #{}),

%% Trigger failure
{error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
Pid ! {action_failed, <<"action">>, <<"failed">>},
timer:sleep(50),

{ok, refusing} = taiea_governor:get_state(Pid),

%% Entitlement reactivation recovers
ok = taiea_governor:entitlement_changed(Pid, active),
timer:sleep(50),

{ok, stable} = taiea_governor:get_state(Pid).
```

## Viewing Receipt Details

```erlang
{ok, Pid} = taiea_governor:start_link(<<"inspect">>),
{ok, stable, R1} = taiea_governor:signal(Pid, #{}),

%% Inspect receipt structure
maps:keys(R1).  % [id, timestamp, tenant_id, governor_id, state_from, state_to, event_type, reason, metadata]

%% Check specific fields
io:format("ID: ~s~n", [maps:get(id, R1)]),
io:format("State: ~p → ~p~n", [
    maps:get(state_from, R1),
    maps:get(state_to, R1)
]),
io:format("Metadata: ~p~n", [maps:get(metadata, R1)]).
```

## Troubleshooting in Shell

### Receipt table already exists
```erlang
%% If you get an error about table existing, the governor is still running
%% Terminate it and start fresh
{ok, PidOld} = gproc:where({n, l, {taiea_governor, <<"test">>}}),
exit(PidOld, kill),
timer:sleep(100),
{ok, PidNew} = taiea_governor:start_link(<<"test">>).
```

### Governor not responding
```erlang
%% Check if it's registered
gproc:where({n, l, {taiea_governor, <<"my_tenant">>}}).

%% If undefined, start it
{ok, Pid} = taiea_governor:start_link(<<"my_tenant">>).

%% Check process is alive
erlang:is_process_alive(Pid).
```

## Summary of Key States

| State | Input | Output | Next State |
|-------|-------|--------|-----------|
| **boot** | signal() | {ok, stable, Receipt} | stable |
| **stable** | signal() | {ok, stable, Receipt} | stable |
| **stable** | tool_call() success | {ok, Result, Receipt} | stable |
| **stable** | tool_call() timeout | {error, timeout} | intervening |
| **intervening** | action_complete info | transitions internally | stable |
| **intervening** | action_failed info | transitions internally | refusing |
| **refusing** | entitlement_changed(active) | transitions internally | stable |

## Next Steps

1. **Run Comprehensive Tests**: `rebar3 ct --suite=taiea_governor_SUITE`
2. **Read Full Spec**: See `TAIEA_GOVERNOR_STATE_GRAPH.md`
3. **Read Implementation Guide**: See `AGENT_7_IMPLEMENTATION_GUIDE.md`
4. **Integrate with IAM (Agent 8)**: Implement Gate 2 logic
5. **Integrate with Entitlements (Agent 9)**: Implement Gate 1 logic

## Example: Full Workflow

```erlang
%% 1. Start governor
{ok, G} = taiea_governor:start_link(<<"workflow_demo">>).

%% 2. Verify boot state
boot = element(2, taiea_governor:get_state(G)).

%% 3. Signal to stable
stable = element(2, element(1, taiea_governor:signal(G, #{}))).

%% 4. Execute queries
{ok, R1, _} = taiea_governor:tool_call(G, <<"query">>, #{table => <<"users">>}, 5000),
{ok, R2, _} = taiea_governor:tool_call(G, <<"query">>, #{table => <<"orders">>}, 5000),

%% 5. Check audit trail
{ok, Receipts} = taiea_governor:list_receipts(G),
io:format("Generated ~w audit receipts~n", [length(Receipts)]),

%% 6. Verify state unchanged
stable = element(2, taiea_governor:get_state(G)).

%% Done!
io:format("Workflow complete!~n").
```

Enjoy exploring the TAIEA Governor!
