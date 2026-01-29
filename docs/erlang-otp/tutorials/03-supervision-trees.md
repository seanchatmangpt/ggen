# Building Supervision Trees

**Goal**: Implement fault-tolerant architectures using supervision trees in 60 minutes.

**What You'll Learn**:
- Design multi-level supervision hierarchies
- Choose appropriate restart strategies
- Implement error kernels
- Build production-grade fault tolerance

## What You'll Build

A telecom-style three-tier system:
- Database connection pool (critical, must never fail)
- Worker pool (100 processes handling requests)
- Supervisor tree with proper isolation

```
                  [Application Supervisor]
                           |
          +----------------+----------------+
          |                                 |
    [DB Supervisor]                 [Worker Supervisor]
          |                                 |
     [DB Pool]                      [Worker Pool]
    (5 connections)                (100 processes)
```

## Prerequisites

- Completed [Your First OTP Application](01-first-otp-app.md)
- Completed [Message Passing Basics](02-message-passing-basics.md)
- Understanding of gen_server
- 60 minutes of focused time

## The "Let It Crash" Philosophy

**Joe Armstrong's key insight**: Don't write defensive code. Let processes crash and supervisors restart them.

**Why?**
1. **Simpler code** - No complex error handling
2. **Fresh state** - Restart clears corrupted state
3. **Isolation** - One crash doesn't affect others
4. **Proven** - Ericsson AXD301 switch achieved 99.9999999% uptime (31ms downtime per year)

See [Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md) for deeper explanation.

## Step 1: Understand Restart Strategies

### one_for_one
```
[Sup]--+-- [Worker A]  ✗ crashes → only A restarts
       |
       +-- [Worker B]  ✓ continues running
       |
       +-- [Worker C]  ✓ continues running
```

**Use when**: Workers are independent (web request handlers, database connections)

### one_for_all
```
[Sup]--+-- [Worker A]  ✗ crashes → ALL restart
       |
       +-- [Worker B]  ✗ restarted
       |
       +-- [Worker C]  ✗ restarted
```

**Use when**: Workers depend on each other (TCP acceptor + connection handler)

### rest_for_one
```
[Sup]--+-- [Worker A]  ✓ continues running
       |
       +-- [Worker B]  ✗ crashes → B and all AFTER restart
       |
       +-- [Worker C]  ✗ restarted (comes after B)
```

**Use when**: Sequential dependencies (database → cache → web server)

### simple_one_for_one (deprecated in OTP 24+)
Use `#{strategy => one_for_one}` with dynamic children instead.

## Step 2: Create Database Layer

Create `src/db_connection.erl`:

```erlang
-module(db_connection).
-behaviour(gen_server).

-export([start_link/1, query/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {
    conn_id :: integer(),
    connected = false :: boolean(),
    queries = 0 :: integer()
}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link(ConnId) ->
    gen_server:start_link(?MODULE, [ConnId], []).

query(Pid, SQL) ->
    gen_server:call(Pid, {query, SQL}).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([ConnId]) ->
    io:format("DB Connection ~p starting~n", [ConnId]),
    % Simulate connection
    timer:sleep(100),
    {ok, #state{conn_id = ConnId, connected = true}}.

handle_call({query, SQL}, _From, State = #state{connected = true}) ->
    % Simulate query
    timer:sleep(10),
    NewState = State#state{queries = State#state.queries + 1},

    % Randomly crash to demonstrate fault tolerance
    case rand:uniform(100) of
        1 -> error({db_error, "Connection lost"});
        _ -> ok
    end,

    Reply = {ok, "Result for: " ++ SQL},
    {reply, Reply, NewState};

handle_call({query, _SQL}, _From, State = #state{connected = false}) ->
    {reply, {error, not_connected}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("DB Connection ~p terminating: ~p (served ~p queries)~n",
              [State#state.conn_id, Reason, State#state.queries]),
    ok.
```

## Step 3: Create Database Supervisor

Create `src/db_sup.erl`:

```erlang
-module(db_sup).
-behaviour(supervisor).

-export([start_link/0, get_connection/0]).
-export([init/1]).

-define(POOL_SIZE, 5).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_connection() ->
    % Simple round-robin selection
    Children = supervisor:which_children(?MODULE),
    {_, Pid, _, _} = lists:nth(rand:uniform(length(Children)), Children),
    Pid.

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent connections
        intensity => 10,          % Allow 10 crashes
        period => 60,             % In 60 seconds
        auto_shutdown => never    % Never auto-shutdown
    },

    % Create pool of database connections
    ChildSpecs = [
        #{
            id => {db_connection, N},
            start => {db_connection, start_link, [N]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [db_connection]
        }
        || N <- lists:seq(1, ?POOL_SIZE)
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Step 4: Create Worker Layer

Create `src/request_worker.erl`:

```erlang
-module(request_worker).
-behaviour(gen_server).

-export([start_link/1, process_request/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {
    worker_id :: integer(),
    requests_processed = 0 :: integer()
}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

process_request(Pid, Request) ->
    gen_server:call(Pid, {process, Request}, 10000).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([WorkerId]) ->
    {ok, #state{worker_id = WorkerId}}.

handle_call({process, Request}, _From, State) ->
    % Get database connection from pool
    DBConn = db_sup:get_connection(),

    % Execute query
    Result = case db_connection:query(DBConn, Request) of
        {ok, Data} ->
            {ok, "Processed: " ++ Data};
        {error, Reason} ->
            {error, Reason}
    end,

    NewState = State#state{
        requests_processed = State#state.requests_processed + 1
    },

    {reply, Result, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("Worker ~p terminating: ~p (processed ~p requests)~n",
              [State#state.worker_id, Reason, State#state.requests_processed]),
    ok.
```

## Step 5: Create Worker Supervisor

Create `src/worker_sup.erl`:

```erlang
-module(worker_sup).
-behaviour(supervisor).

-export([start_link/0, get_worker/0]).
-export([init/1]).

-define(POOL_SIZE, 100).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_worker() ->
    % Simple round-robin selection
    Children = supervisor:which_children(?MODULE),
    {_, Pid, _, _} = lists:nth(rand:uniform(length(Children)), Children),
    Pid.

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent workers
        intensity => 100,         % High tolerance (workers crash often)
        period => 60,
        auto_shutdown => never
    },

    % Create pool of workers
    ChildSpecs = [
        #{
            id => {request_worker, N},
            start => {request_worker, start_link, [N]},
            restart => transient,     % Only restart if abnormal exit
            shutdown => 5000,
            type => worker,
            modules => [request_worker]
        }
        || N <- lists:seq(1, ?POOL_SIZE)
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Step 6: Create Top-Level Supervisor

Create `src/telecom_sup.erl`:

```erlang
-module(telecom_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % DB and Workers are independent
        intensity => 5,           % Low tolerance (these are critical)
        period => 60,
        auto_shutdown => never
    },

    ChildSpecs = [
        % Database layer (started first)
        #{
            id => db_sup,
            start => {db_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Wait for all DB connections
            type => supervisor,
            modules => [db_sup]
        },

        % Worker layer (started second)
        #{
            id => worker_sup,
            start => {worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Wait for all workers
            type => supervisor,
            modules => [worker_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Step 7: Configure Application

Create `src/telecom_app.erl`:

```erlang
-module(telecom_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    telecom_sup:start_link().

stop(_State) ->
    ok.
```

Update `src/telecom.app.src`:

```erlang
{application, telecom,
 [{description, "Three-tier telecom system"},
  {vsn, "1.0.0"},
  {registered, [telecom_sup, db_sup, worker_sup]},
  {mod, {telecom_app, []}},
  {applications, [kernel, stdlib]},
  {env, []},
  {modules, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
```

## Step 8: Test Fault Tolerance

```bash
rebar3 shell
```

```erlang
% System starts automatically
% Check supervision tree
supervisor:which_children(telecom_sup).
%% [{worker_sup,<0.150.0>,supervisor,[worker_sup]},
%%  {db_sup,<0.149.0>,supervisor,[db_sup]}]

supervisor:which_children(db_sup).
%% [{{db_connection,5},<0.154.0>,worker,[db_connection]},
%%  {{db_connection,4},<0.153.0>,worker,[db_connection]},
%%  ...]

supervisor:which_children(worker_sup).
%% [{{request_worker,100},<0.254.0>,worker,[request_worker]},
%%  {{request_worker,99},<0.253.0>,worker,[request_worker]},
%%  ...]

% Process request
Worker = worker_sup:get_worker().
request_worker:process_request(Worker, "SELECT * FROM users").
%% {ok, "Processed: Result for: SELECT * FROM users"}

% Simulate load (some will crash due to random DB failures)
lists:foreach(
    fun(_) ->
        W = worker_sup:get_worker(),
        spawn(fun() ->
            request_worker:process_request(W, "SELECT * FROM data")
        end)
    end,
    lists:seq(1, 1000)
).

% Check how many processes restarted
observer:start().  % Visual supervision tree
```

## Step 9: Advanced Patterns

### Error Kernel Pattern

Keep critical state in stable supervisors, let complex logic crash.

```erlang
[Root Supervisor]
    |
    +-- [Error Kernel: ETS table with critical data]  % NEVER crashes
    |
    +-- [Complex Logic Supervisor]
            |
            +-- [Parser Worker]     % Crashes often
            +-- [Validator Worker]  % Crashes often
            +-- [Processor Worker]  % Crashes often
```

### Escalation Pattern

If a supervisor's intensity is exceeded, it crashes itself and escalates to parent.

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,   % Allow 3 crashes
        period => 10      % In 10 seconds (tight window!)
    },
    % If exceeded, this supervisor crashes
    % Parent supervisor restarts entire subtree
    {ok, {SupFlags, ChildSpecs}}.
```

## Summary

**What You Built**:
- ✅ Three-tier supervision tree
- ✅ Database connection pool (5 connections)
- ✅ Worker pool (100 processes)
- ✅ Fault isolation between layers
- ✅ Automatic crash recovery

**Key Takeaways**:
1. **Supervision trees** isolate failures
2. **Restart strategies** define failure propagation
3. **Error kernels** protect critical state
4. **Escalation** handles catastrophic failures

**Design Principles**:
- **one_for_one** for independent workers
- **Low intensity** for critical supervisors
- **High intensity** for worker pools
- **permanent restart** for infrastructure
- **transient restart** for request handlers

## Next Steps

- **How-To**: [Handle Process Crashes](../how-to/handle-process-crashes.md) - Advanced crash handling
- **Explanation**: [Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md) - Why this works
- **Reference**: [Supervisor API](../reference/supervisor-api.md) - Complete API

## Common Patterns

### Database Connection Pool
```erlang
#{strategy => one_for_one,  % Independent connections
  intensity => 10,          % Tolerate connection failures
  period => 60}
```

### Worker Pool
```erlang
#{strategy => one_for_one,  % Independent workers
  intensity => 100,         % High tolerance
  period => 60}
```

### Dependent Services
```erlang
#{strategy => rest_for_one,  % Sequential dependencies
  intensity => 5,
  period => 60}

ChildSpecs = [
    DatabaseSpec,   % Started first
    CacheSpec,      % Depends on database
    WebServerSpec   % Depends on cache
]
```

## Production Considerations

### Monitoring
```erlang
% Use observer for live supervision tree
observer:start().

% Or programmatically
supervisor:count_children(MySup).
%% [{specs,100},{active,100},{supervisors,0},{workers,100}]
```

### Logging
```erlang
% In terminate/2 callback
terminate(Reason, State) ->
    logger:warning("Worker crashed: ~p", [Reason]),
    ok.
```

### Metrics
```erlang
% Track restarts
init([]) ->
    ets:new(restart_counter, [named_table, public]),
    ets:insert(restart_counter, {restarts, 0}),
    {ok, State}.
```

## References

- [Supervisor API Reference](../reference/supervisor-api.md)
- [gen_server API Reference](../reference/gen-server-api.md)
- [Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md)

---

**Time to complete**: ~60 minutes
**Difficulty**: Intermediate
**Prerequisites**: [Tutorial 01](01-first-otp-app.md), [Tutorial 02](02-message-passing-basics.md)
