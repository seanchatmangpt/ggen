# Your First OTP Application

**Goal**: Build a working OTP application with supervision and fault tolerance in 30 minutes.

**What You'll Learn**:
- Create an OTP application structure with rebar3
- Implement a gen_server worker process
- Add supervision for fault tolerance
- Test crash recovery and hot code reloading

## What You'll Build

A simple key-value store that:
- Stores data in-memory
- Handles concurrent requests
- Automatically restarts on crashes
- Supports hot code upgrades

## Prerequisites

- Erlang/OTP 26+ installed
- rebar3 build tool (`brew install rebar3` or equivalent)
- 30 minutes of focused time

## Step 1: Create Project Structure

```bash
# Create new OTP application
rebar3 new app kvstore
cd kvstore

# Verify structure
tree .
```

**Expected output**:
```
.
├── LICENSE
├── README.md
├── rebar.config
└── src
    ├── kvstore.app.src
    ├── kvstore_app.erl
    └── kvstore_sup.erl
```

**What just happened?**
- `kvstore.app.src` - Application metadata
- `kvstore_app.erl` - Application entry point (starts supervisor)
- `kvstore_sup.erl` - Top-level supervisor (monitors workers)

## Step 2: Implement gen_server Worker

Create `src/kvstore_server.erl`:

```erlang
-module(kvstore_server).
-behaviour(gen_server).

%% API
-export([start_link/0, put/2, get/1, delete/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%% ==================================================================
%%% API Functions
%%% ==================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

list() ->
    gen_server:call(?MODULE, list).

%%% ==================================================================
%%% gen_server Callbacks
%%% ==================================================================

init([]) ->
    io:format("~p starting~n", [?MODULE]),
    {ok, maps:new()}.  % Initialize with empty map

handle_call({put, Key, Value}, _From, State) ->
    NewState = maps:put(Key, Value, State),
    {reply, ok, NewState};

handle_call({get, Key}, _From, State) ->
    Reply = maps:get(Key, State, undefined),
    {reply, Reply, State};

handle_call(list, _From, State) ->
    {reply, maps:to_list(State), State}.

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating: ~p~n", [?MODULE, Reason]),
    ok.
```

**Key Concepts**:
- `gen_server` behavior provides structured concurrency
- `start_link/0` spawns the process and links it to supervisor
- `handle_call/3` handles synchronous requests (client waits)
- `handle_cast/2` handles asynchronous requests (fire-and-forget)
- State is immutable - we return `NewState` on each update

## Step 3: Configure Supervisor

Edit `src/kvstore_sup.erl`:

```erlang
-module(kvstore_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Restart only crashed child
        intensity => 5,           % Allow 5 crashes
        period => 60              % In 60 seconds
    },

    ChildSpecs = [
        #{
            id => kvstore_server,
            start => {kvstore_server, start_link, []},
            restart => permanent,      % Always restart
            shutdown => 5000,          % 5 second graceful shutdown
            type => worker,
            modules => [kvstore_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Supervision Strategy**:
- `one_for_one` - If kvstore_server crashes, only it restarts (not siblings)
- `intensity => 5, period => 60` - If crashes more than 5 times in 60 seconds, supervisor gives up
- `restart => permanent` - Always restart this worker, even if it exits normally

## Step 4: Build and Test

```bash
# Compile
rebar3 compile

# Start interactive shell
rebar3 shell
```

In the Erlang shell:

```erlang
% Application starts automatically in rebar3 shell
% Test basic operations
kvstore_server:put(name, "Alice").
%% ok

kvstore_server:get(name).
%% "Alice"

kvstore_server:put(age, 30).
%% ok

kvstore_server:list().
%% [{name, "Alice"}, {age, 30}]

kvstore_server:delete(age).
%% ok

kvstore_server:list().
%% [{name, "Alice"}]
```

## Step 5: Test Fault Tolerance

**This is where Erlang shines!** Let's crash the server and watch it restart.

```erlang
% Get the process ID
Pid = whereis(kvstore_server).
%% <0.145.0>

% Store some data
kvstore_server:put(test, "data before crash").
%% ok

% Kill the process
exit(Pid, kill).
%% true

% Check if process restarted (new PID!)
whereis(kvstore_server).
%% <0.150.0>  % Different PID - it restarted!

% Data is lost (in-memory only)
kvstore_server:get(test).
%% undefined
```

**What happened?**
1. We killed the process with `exit(Pid, kill)`
2. Supervisor detected the crash
3. Supervisor spawned a new process
4. System continues running (fault tolerance!)
5. Data is lost (we'll fix this in advanced tutorials with ETS or Mnesia)

## Step 6: Add Persistence (Bonus)

Let's make data survive crashes using ETS (Erlang Term Storage):

Update `src/kvstore_server.erl`:

```erlang
init([]) ->
    io:format("~p starting~n", [?MODULE]),
    % Create ETS table owned by the supervisor (survives crashes)
    case ets:info(kvstore_data) of
        undefined ->
            ets:new(kvstore_data, [named_table, public, set]);
        _ ->
            ok  % Table already exists
    end,
    {ok, #{}}.  % State is empty - we use ETS now

handle_call({put, Key, Value}, _From, State) ->
    ets:insert(kvstore_data, {Key, Value}),
    {reply, ok, State};

handle_call({get, Key}, _From, State) ->
    Reply = case ets:lookup(kvstore_data, Key) of
        [{Key, Value}] -> Value;
        [] -> undefined
    end,
    {reply, Reply, State};

handle_call(list, _From, State) ->
    {reply, ets:tab2list(kvstore_data), State}.

handle_cast({delete, Key}, State) ->
    ets:delete(kvstore_data, Key),
    {noreply, State}.
```

**Now test crash recovery**:

```erlang
% Restart shell to reload code
q().

rebar3 shell

% Store data
kvstore_server:put(test, "persistent data").

% Crash the process
exit(whereis(kvstore_server), kill).

% Data survived!
kvstore_server:get(test).
%% "persistent data"
```

**Why does this work?**
- ETS tables can be created with `public` access
- When gen_server crashes, ETS table survives
- New gen_server instance accesses same ETS table

## Step 7: Production Refinement

For production, move ETS creation to supervisor:

Update `src/kvstore_sup.erl`:

```erlang
init([]) ->
    % Create ETS table once, owned by supervisor
    ets:new(kvstore_data, [named_table, public, set,
                           {read_concurrency, true}]),  % Optimize reads

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => kvstore_server,
            start => {kvstore_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [kvstore_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Summary

**What You Built**:
- ✅ gen_server worker (structured concurrency)
- ✅ Supervisor (fault tolerance)
- ✅ Crash recovery (let it crash philosophy)
- ✅ Persistent storage (ETS)
- ✅ Production-ready structure

**Key Takeaways**:
1. **gen_server** provides structured state management
2. **Supervisors** automatically restart crashed processes
3. **"Let it crash"** - Don't defensively program, let supervisors handle failures
4. **ETS** provides fast, concurrent, persistent storage

## Next Steps

- **Tutorial 02**: [Message Passing Basics](02-message-passing-basics.md) - Master process communication
- **Tutorial 03**: [Building Supervision Trees](03-supervision-trees.md) - Multi-level fault tolerance
- **How-To**: [Optimize Message Passing](../how-to/optimize-message-passing.md) - Performance tuning

## Common Pitfalls

**❌ Don't do this**:
```erlang
% Storing state outside gen_server
start_link() ->
    put(my_state, []),  % Process dictionary is evil!
    gen_server:start_link(...).
```

**✅ Do this instead**:
```erlang
init([]) ->
    {ok, []}}.  % State belongs in gen_server
```

**❌ Don't do this**:
```erlang
handle_call({get, Key}, _From, State) ->
    try
        Value = maps:get(Key, State),  % Defensive programming
        {reply, Value, State}
    catch
        error:_ -> {reply, undefined, State}
    end.
```

**✅ Do this instead**:
```erlang
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),  % Let it crash or use default
    {reply, Value, State}.
```

## References

- [gen_server API Reference](../reference/gen-server-api.md)
- [Supervisor API Reference](../reference/supervisor-api.md)
- [Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md)

---

**Time to complete**: ~30 minutes
**Difficulty**: Beginner
**Prerequisites**: Basic Erlang syntax (pattern matching, functions)
