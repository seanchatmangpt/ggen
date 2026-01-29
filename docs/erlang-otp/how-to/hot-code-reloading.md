# How to Hot Code Reloading

**Problem**: Need to deploy updates without stopping the system or losing state.

**Solution**: Use OTP's hot code reloading capabilities with releases and code change callbacks.

## When to Use This Guide

- ✅ Deploy updates to production without downtime
- ✅ Maintain long-running state through upgrades
- ✅ Fix bugs in running system
- ✅ Add features without service interruption
- ✅ Upgrade telecom/critical systems

## Quick Solutions

### 1. Basic Module Reloading (Development)

**Problem**: Need to reload changed module during development.

**Solution**: Use `c(module)` or `l(module)` in the shell.

```erlang
% In rebar3 shell, after editing code
c(my_module).
%% {ok, my_module}

% Or reload module
l(my_module).
%% {module, my_module}

% Reload all changed modules
[c(M) || M <- [module1, module2, module3]].
```

**Limitation**: This only works for stateless code. gen_server state is lost.

### 2. Code Change Callbacks (Stateful Reload)

**Problem**: Need to update gen_server code without losing state.

**Solution**: Implement `code_change/3` callback.

```erlang
-module(stateful_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, code_change/3]).

% Version 1.0.0 - Old state structure
-record(state_v1, {
    counter = 0 :: integer()
}).

% Version 2.0.0 - New state structure with timestamp
-record(state_v2, {
    counter = 0 :: integer(),
    last_update :: integer()  % NEW FIELD
}).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    {ok, #state_v2{counter = 0, last_update = erlang:system_time(second)}}.

handle_call(increment, _From, State) ->
    NewState = State#state_v2{
        counter = State#state_v2.counter + 1,
        last_update = erlang:system_time(second)
    },
    {reply, ok, NewState};

handle_call(get_counter, _From, State) ->
    {reply, State#state_v2.counter, State}.

%% Critical: Migrate state from old version to new version
code_change(_OldVsn, #state_v1{counter = Counter}, _Extra) ->
    % Upgrade v1 -> v2
    NewState = #state_v2{
        counter = Counter,
        last_update = erlang:system_time(second)
    },
    {ok, NewState};

code_change(_OldVsn, State, _Extra) ->
    % No state migration needed
    {ok, State}.
```

**Test it**:

```erlang
% Start server with v1
Pid = stateful_server:start_link().

% Set some state
stateful_server:increment(Pid).
stateful_server:increment(Pid).

% Hot reload to v2
sys:suspend(Pid).
c(stateful_server).
sys:change_code(Pid, stateful_server, undefined, []).
sys:resume(Pid).

% State preserved + new field added!
stateful_server:get_counter(Pid).
%% 2
```

### 3. Release Upgrades with relx

**Problem**: Need structured upgrade process for production.

**Solution**: Use rebar3 releases with appup files.

**Step 1: Configure release in rebar.config**:

```erlang
{relx, [
    {release, {myapp, "2.0.0"},
     [myapp, sasl]},

    {dev_mode, false},
    {include_erts, true},

    {extended_start_script, true}
]}.
```

**Step 2: Create appup file** (`src/myapp.appup`):

```erlang
%% Upgrade from 1.0.0 to 2.0.0
{"2.0.0",
 [
  {"1.0.0", [
      {load_module, stateful_server},  % Load new code
      {update, stateful_server, {advanced, []}}  % Trigger code_change
  ]}
 ],
 [
  {"1.0.0", [
      {update, stateful_server, {advanced, []}},
      {load_module, stateful_server}
  ]}
 ]
}.
```

**Step 3: Build and install upgrade**:

```bash
# Build version 1.0.0
rebar3 release

# Update version to 2.0.0 in .app.src
# Make code changes
# Create appup file

# Build version 2.0.0
rebar3 release

# Generate upgrade package
rebar3 relup
rebar3 tar

# Install upgrade (system keeps running!)
./_build/default/rel/myapp/bin/myapp upgrade "2.0.0"
```

### 4. Upgrade Instructions Reference

**appup directives**:

```erlang
{"2.0.0",
 [
  {"1.0.0", [
      %% Load new module without running processes
      {load_module, simple_module},

      %% Update running gen_server (calls code_change)
      {update, my_server, {advanced, []}},

      %% Update supervisor (restart children with new code)
      {update, my_sup, supervisor},

      %% Add new module
      {add_module, new_module},

      %% Remove old module
      {delete_module, old_module},

      %% Add new process to supervisor
      {add_process, new_worker, [my_sup]},

      %% Remove process from supervisor
      {remove_process, old_worker}
  ]}
 ],
 [
  %% Downgrade instructions (reverse order)
  {"1.0.0", [
      {remove_process, new_worker},
      {add_process, old_worker, [my_sup]},
      {delete_module, new_module},
      {update, my_sup, supervisor},
      {update, my_server, {advanced, []}},
      {load_module, simple_module}
  ]}
 ]
}.
```

## Advanced Patterns

### Versioned State Migration

Handle multiple upgrade paths:

```erlang
% State version 1
-record(state_v1, {data}).

% State version 2
-record(state_v2, {data, cache}).

% State version 3
-record(state_v3, {data, cache, timestamp}).

code_change("1.0.0", #state_v1{data = Data}, _Extra) ->
    % v1 -> v3
    {ok, #state_v3{
        data = Data,
        cache = #{},
        timestamp = erlang:system_time(second)
    }};

code_change("2.0.0", #state_v2{data = Data, cache = Cache}, _Extra) ->
    % v2 -> v3
    {ok, #state_v3{
        data = Data,
        cache = Cache,
        timestamp = erlang:system_time(second)
    }};

code_change(_OldVsn, State, _Extra) ->
    % Already v3 or unknown version
    {ok, State}.
```

### Graceful State Migration with Compatibility Layer

Support both old and new formats during transition:

```erlang
-module(compatible_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, code_change/3]).

% Support both state formats
-type state() :: state_v1() | state_v2().

-record(state_v1, {data :: map()}).
-record(state_v2, {data :: map(), version = 2 :: integer()}).

init([]) ->
    {ok, #state_v2{data = #{}}}.

handle_call(get_data, _From, State) ->
    Data = get_data_compatible(State),
    {reply, Data, State}.

% Compatibility layer
get_data_compatible(#state_v1{data = Data}) -> Data;
get_data_compatible(#state_v2{data = Data}) -> Data.

code_change(_OldVsn, #state_v1{data = Data}, _Extra) ->
    {ok, #state_v2{data = Data, version = 2}};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### Rolling Upgrades in Clusters

Upgrade nodes one at a time without downtime:

```erlang
% upgrade_coordinator.erl
-module(upgrade_coordinator).
-export([rolling_upgrade/1]).

rolling_upgrade(Version) ->
    Nodes = nodes(),
    lists:foreach(
        fun(Node) ->
            % Upgrade one node
            io:format("Upgrading ~p to ~s~n", [Node, Version]),
            rpc:call(Node, release_handler, install_release, [Version]),

            % Wait for node to stabilize
            timer:sleep(5000),

            % Verify node health
            case rpc:call(Node, health_check, verify, []) of
                ok ->
                    io:format("~p upgraded successfully~n", [Node]);
                {error, Reason} ->
                    % Rollback
                    io:format("~p upgrade failed: ~p, rolling back~n", [Node, Reason]),
                    rpc:call(Node, release_handler, reboot_old_release, [])
            end
        end,
        Nodes
    ).
```

## Production Upgrade Checklist

Before deploying hot code upgrade:

- ✅ Implement `code_change/3` for stateful processes
- ✅ Create and test appup file
- ✅ Test upgrade in staging environment
- ✅ Test downgrade (rollback) path
- ✅ Verify state migration logic
- ✅ Check all modules are accounted for
- ✅ Monitor during upgrade (observer, metrics)
- ✅ Have rollback plan ready
- ✅ Document upgrade procedure
- ✅ Alert team before production upgrade

## Testing Upgrades

```erlang
% Test code_change callback
test_upgrade_v1_to_v2() ->
    % Start with v1 state
    OldState = #state_v1{counter = 42},

    % Simulate code change
    {ok, NewState} = stateful_server:code_change("1.0.0", OldState, []),

    % Verify migration
    ?assertMatch(#state_v2{counter = 42}, NewState),
    ?assert(is_integer(NewState#state_v2.last_update)).
```

## Common Patterns

### Database Schema Migration

```erlang
code_change("1.0.0", State, _Extra) ->
    % Migrate database schema
    ok = db:run_migration("add_timestamp_column"),

    % Update state to reflect new schema
    NewState = State#state{db_version = 2},
    {ok, NewState}.
```

### ETS Table Migration

```erlang
code_change("1.0.0", State, _Extra) ->
    % Create new ETS table with updated structure
    NewTable = ets:new(data_v2, [set, public]),

    % Copy and transform data
    ets:foldl(
        fun({Key, OldValue}, Acc) ->
            NewValue = transform(OldValue),
            ets:insert(NewTable, {Key, NewValue}),
            Acc
        end,
        ok,
        State#state.table
    ),

    % Delete old table
    ets:delete(State#state.table),

    NewState = State#state{table = NewTable},
    {ok, NewState}.
```

## Debugging Hot Upgrades

### Check loaded modules

```erlang
% List loaded modules
code:all_loaded().

% Check module version
code:is_loaded(mymodule).
%% {file, "/path/to/mymodule.beam"}

% Get module info
mymodule:module_info(attributes).
```

### Monitor upgrade process

```erlang
% Check release handler status
release_handler:which_releases().
%% [{"myapp","2.0.0",["kernel","stdlib","sasl"],"permanent"},
%%  {"myapp","1.0.0",["kernel","stdlib","sasl"],"old"}]

% Check for pending upgrades
release_handler:check_install_release("2.0.0").
```

### Observer for live monitoring

```erlang
observer:start().
% Applications tab shows loaded versions
% Processes tab shows process state
```

## Limitations and Gotcalls

**❌ Two-version limit**:
```
% Erlang only keeps 2 versions of a module in memory
% Can't have 3 versions running simultaneously
```

**❌ Function signature changes**:
```erlang
% Changing function signatures requires careful coordination
% Old processes calling new code can crash
```

**✅ Solution: Use wrapper functions**:
```erlang
% Old API (keep for compatibility)
old_function(Arg1) ->
    new_function(Arg1, default_value).

% New API
new_function(Arg1, Arg2) ->
    % Implementation
    ok.
```

**❌ Supervisor child spec changes**:
```erlang
% Can't hot upgrade supervisor child specs directly
% Must restart supervisor (loses child state)
```

**✅ Solution: Use dynamic children**:
```erlang
% Add/remove children dynamically
supervisor:start_child(MySup, ChildSpec).
supervisor:terminate_child(MySup, ChildId).
supervisor:delete_child(MySup, ChildId).
```

## Related Guides

- [Building Supervision Trees](../tutorials/03-supervision-trees.md) - Foundation
- [Handle Process Crashes](handle-process-crashes.md) - Recovery patterns
- [gen_server API](../reference/gen-server-api.md) - code_change callback

## References

- [OTP Design Principles - Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
- [appup Documentation](https://www.erlang.org/doc/man/appup.html)
- [relx Documentation](https://github.com/erlware/relx)

---

**When to use**: Production deployments, zero-downtime updates
**Difficulty**: Advanced
**See also**: Release handling, appup, relx, code_change
