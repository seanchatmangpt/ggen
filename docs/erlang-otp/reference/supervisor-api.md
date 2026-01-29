# Supervisor Behavior Reference

Complete API reference for the `supervisor` behavior - the foundation of fault tolerance in OTP.

## Overview

**Supervisors** monitor and restart child processes according to restart strategies, providing:
- Automatic crash recovery
- Process lifecycle management
- Hierarchical fault isolation
- Dynamic child management

## Restart Strategies

### one_for_one

```
[Supervisor]
    |
    +-- [Child A]  ✗ crashes → only A restarts
    +-- [Child B]  ✓ continues
    +-- [Child C]  ✓ continues
```

**Use when**: Children are independent.

**Example**: Connection pool, request handler pool.

### one_for_all

```
[Supervisor]
    |
    +-- [Child A]  ✗ crashes → ALL restart
    +-- [Child B]  ✗ restarted
    +-- [Child C]  ✗ restarted
```

**Use when**: Children depend on each other.

**Example**: TCP acceptor and connection handler.

### rest_for_one

```
[Supervisor]
    |
    +-- [Child A]  ✓ continues
    +-- [Child B]  ✗ crashes → B and all AFTER restart
    +-- [Child C]  ✗ restarted
```

**Use when**: Sequential dependencies (start order matters).

**Example**: Database → Cache → Web Server.

### simple_one_for_one (deprecated OTP 24+)

Use `one_for_one` with dynamic children instead.

## Module Callbacks

### init(Args) → {ok, {SupFlags, ChildSpecs}}

**Description**: Initialize supervisor configuration.

**Parameters**:
- `Args :: term()` - Arguments from `start_link`

**Returns**:
- `{ok, {SupFlags, ChildSpecs}}` - Configuration
- `ignore` - Don't start

**SupFlags** (map format, OTP 18+):

```erlang
#{
    strategy => one_for_one | one_for_all | rest_for_one,
    intensity => non_neg_integer(),  % Max restarts
    period => pos_integer(),         % In N seconds
    auto_shutdown => never | any_significant | all_significant  % OTP 24+
}
```

**ChildSpec** (map format, OTP 18+):

```erlang
#{
    id => term(),                    % Unique child identifier
    start => {M, F, A},             % Start function
    restart => permanent | transient | temporary,
    shutdown => brutal_kill | infinity | pos_integer(),
    type => worker | supervisor,
    modules => [Module] | dynamic
}
```

**Examples**:

```erlang
% Basic supervisor
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => worker1,
            start => {my_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [my_worker]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

% Connection pool
init([PoolSize]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => {connection, N},
            start => {db_connection, start_link, [N]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
        || N <- lists:seq(1, PoolSize)
    ],

    {ok, {SupFlags, ChildSpecs}}.

% Nested supervisors
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => db_sup,
            start => {db_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Wait for all children
            type => supervisor
        },
        #{
            id => worker_sup,
            start => {worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

% Rest-for-one with dependencies
init([]) ->
    SupFlags = #{
        strategy => rest_for_one,  % Start order matters!
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{id => database, start => {db, start_link, []}},
        #{id => cache, start => {cache, start_link, []}},      % Depends on database
        #{id => web_server, start => {web, start_link, []}}    % Depends on cache
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## SupFlags Details

### strategy

**Values**:
- `one_for_one` - Restart only crashed child
- `one_for_all` - Restart all children
- `rest_for_one` - Restart crashed child and all started after it

### intensity and period

Controls restart storm prevention:

```erlang
#{
    intensity => 5,  % Allow 5 restarts
    period => 60     % In 60 seconds
}
```

If intensity exceeded, **supervisor crashes itself** (escalation).

**Guidelines**:
- **Low intensity (3-5)** for critical infrastructure
- **High intensity (50-100)** for worker pools
- **Period typically 60 seconds**

### auto_shutdown (OTP 24+)

Controls supervisor self-shutdown:

```erlang
#{
    auto_shutdown => never  % Default: never self-shutdown
}

#{
    auto_shutdown => any_significant  % Shutdown if ANY significant child terminates
}

#{
    auto_shutdown => all_significant  % Shutdown if ALL significant children terminate
}
```

Mark child as significant:

```erlang
#{
    id => worker,
    start => {worker, start_link, []},
    significant => true  % This child is significant
}
```

## ChildSpec Details

### id

Unique identifier. Used for `terminate_child/2`, `delete_child/2`, etc.

```erlang
id => my_worker
id => {worker, 1}  % Useful for pools
```

### start

Module-Function-Arguments tuple:

```erlang
start => {my_worker, start_link, []}
start => {my_worker, start_link, [Port, Options]}
```

**Must return**: `{ok, Pid}` or `{ok, Pid, Info}`

### restart

**Values**:
- `permanent` - Always restart (infrastructure)
- `transient` - Restart only if abnormal exit (workers)
- `temporary` - Never restart (one-shot tasks)

```erlang
% Always restart
restart => permanent

% Only restart on crashes, not normal exits
restart => transient

% Never restart
restart => temporary
```

### shutdown

Time to wait for graceful shutdown before killing:

```erlang
shutdown => 5000           % 5 seconds
shutdown => infinity       % Wait forever (supervisors only)
shutdown => brutal_kill    % Kill immediately
```

**Shutdown sequence**:
1. Supervisor sends exit signal to child
2. If `trap_exit = true`, child's `terminate/2` is called
3. Wait `shutdown` milliseconds
4. If still alive, kill with `exit(Pid, kill)`

**Guidelines**:
- **5000ms** for workers (default)
- **infinity** for supervisors (wait for grandchildren)
- **brutal_kill** for stateless workers

### type

```erlang
type => worker      % Regular process
type => supervisor  % Nested supervisor
```

**Important**: Use `supervisor` for nested supervisors to set `shutdown => infinity`.

### modules

List of modules implementing the child (for hot code upgrades):

```erlang
modules => [my_worker]
modules => [my_gen_server]
modules => dynamic  % For gen_event or complex behaviours
```

## API Functions

### Starting Supervisors

#### start_link(Module, Args) → Result

```erlang
supervisor:start_link(my_sup, []).
%% {ok, Pid}

supervisor:start_link({local, my_sup}, my_sup, []).
%% {ok, Pid}

supervisor:start_link({global, my_sup}, my_sup, []).
%% {ok, Pid}
```

### Dynamic Child Management

#### start_child(SupRef, ChildSpec) → Result

Add and start child dynamically:

```erlang
ChildSpec = #{
    id => worker1,
    start => {my_worker, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker
},

supervisor:start_child(my_sup, ChildSpec).
%% {ok, Pid}
%% {ok, Pid, Info}
%% {error, already_present}
%% {error, {already_started, Pid}}
%% {error, Reason}
```

#### terminate_child(SupRef, Id) → Result

Stop child without removing spec:

```erlang
supervisor:terminate_child(my_sup, worker1).
%% ok
%% {error, not_found}
```

#### delete_child(SupRef, Id) → Result

Remove child spec (must be terminated first):

```erlang
supervisor:terminate_child(my_sup, worker1),
supervisor:delete_child(my_sup, worker1).
%% ok
%% {error, running}
%% {error, not_found}
```

#### restart_child(SupRef, Id) → Result

Restart terminated child:

```erlang
supervisor:restart_child(my_sup, worker1).
%% {ok, Pid}
%% {ok, Pid, Info}
%% {error, running}
%% {error, not_found}
```

### Inspection

#### which_children(SupRef) → [{Id, Child, Type, Modules}]

List all children:

```erlang
supervisor:which_children(my_sup).
%% [{worker1, <0.123.0>, worker, [my_worker]},
%%  {worker2, undefined, worker, [my_worker]},  % Not running
%%  {sup1, <0.125.0>, supervisor, [my_sup]}]
```

#### count_children(SupRef) → PropList

Count children by status:

```erlang
supervisor:count_children(my_sup).
%% [{specs, 100},      % Total child specs
%%  {active, 98},      % Running children
%%  {supervisors, 2},  % Nested supervisors
%%  {workers, 96}]     % Workers
```

#### get_childspec(SupRef, Id) → {ok, ChildSpec} | {error, not_found}

Get child specification:

```erlang
supervisor:get_childspec(my_sup, worker1).
%% {ok, #{id => worker1, start => ...}}
```

## Common Patterns

### Worker Pool

```erlang
init([PoolSize]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => PoolSize,  % High tolerance
        period => 60
    },

    ChildSpecs = [
        #{
            id => {worker, N},
            start => {worker, start_link, [N]},
            restart => transient,  % Only restart on crashes
            shutdown => 5000,
            type => worker
        }
        || N <- lists:seq(1, PoolSize)
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### Database + Worker Architecture

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        % Database layer (started first)
        #{
            id => db_sup,
            start => {db_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        },

        % Worker layer (started second)
        #{
            id => worker_sup,
            start => {worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### Dependent Services (rest_for_one)

```erlang
init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 3,
        period => 60
    },

    ChildSpecs = [
        #{id => database, start => {db, start_link, []}},
        #{id => cache, start => {cache, start_link, []}},
        #{id => api, start => {api, start_link, []}}
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### Dynamic Worker Pool

```erlang
% Supervisor with no initial children
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 60
    },

    {ok, {SupFlags, []}}.

% Add workers dynamically
add_worker(SupPid, WorkerId) ->
    ChildSpec = #{
        id => {worker, WorkerId},
        start => {worker, start_link, [WorkerId]},
        restart => transient
    },
    supervisor:start_child(SupPid, ChildSpec).
```

## Escalation Pattern

If supervisor's intensity is exceeded, it crashes and escalates to parent:

```erlang
[Root Supervisor]
    |
    +-- [Middle Supervisor]  % intensity => 3, period => 10
            |
            +-- [Worker A]
            +-- [Worker B]
            +-- [Worker C]
```

If Worker A crashes 4 times in 10 seconds:
1. Middle supervisor's intensity exceeded
2. Middle supervisor crashes itself
3. Root supervisor restarts entire Middle supervisor subtree
4. Fresh start with clean state

## Significant Children (OTP 24+)

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60,
        auto_shutdown => any_significant
    },

    ChildSpecs = [
        #{
            id => critical_worker,
            start => {worker, start_link, []},
            significant => true  % Supervisor shuts down if this exits
        },
        #{
            id => helper_worker,
            start => {helper, start_link, []},
            significant => false  % Normal child
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Legacy Tuple Format

OTP 17 and earlier used tuple format (still supported):

```erlang
init([]) ->
    SupFlags = {one_for_one, 5, 60},

    ChildSpecs = [
        {worker1,
         {my_worker, start_link, []},
         permanent,
         5000,
         worker,
         [my_worker]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Prefer map format** for new code (clearer, more flexible).

## Testing Supervisors

```erlang
% Test child restart
test_child_restart() ->
    {ok, SupPid} = my_sup:start_link(),

    % Get child PID
    [{worker1, WorkerPid, _, _}] = supervisor:which_children(SupPid),

    % Kill worker
    exit(WorkerPid, kill),
    timer:sleep(100),

    % Verify restart
    [{worker1, NewPid, _, _}] = supervisor:which_children(SupPid),
    ?assert(is_pid(NewPid)),
    ?assert(WorkerPid =/= NewPid).

% Test intensity limit
test_intensity_exceeded() ->
    {ok, SupPid} = my_sup:start_link(),
    MonRef = monitor(process, SupPid),

    % Crash worker repeatedly
    lists:foreach(
        fun(_) ->
            [{worker, Pid, _, _}] = supervisor:which_children(SupPid),
            exit(Pid, kill),
            timer:sleep(100)
        end,
        lists:seq(1, 10)
    ),

    % Supervisor should have crashed
    receive
        {'DOWN', MonRef, process, SupPid, _} -> ok
    after 1000 ->
        error(supervisor_should_have_crashed)
    end.
```

## See Also

- [Tutorial: Building Supervision Trees](../tutorials/03-supervision-trees.md)
- [How-To: Handle Process Crashes](../how-to/handle-process-crashes.md)
- [gen_server API Reference](gen-server-api.md)
- [Explanation: Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md)

---

**Official Docs**: [supervisor - Erlang](https://www.erlang.org/doc/man/supervisor.html)
