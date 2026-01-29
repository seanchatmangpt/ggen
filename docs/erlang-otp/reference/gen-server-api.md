# gen_server Behavior Reference

Complete API reference for the `gen_server` behavior - the foundation of stateful processes in OTP.

## Overview

**gen_server** provides a standardized pattern for server processes with:
- Synchronous calls (`call`) and asynchronous casts (`cast`)
- State management across messages
- Supervision integration
- Hot code upgrades via `code_change/3`
- Systematic error handling

## Module Callbacks

All callbacks are optional except `init/1`. Implement only what you need.

---

### init(Args) → Result

**Description**: Initialize server state.

**Called when**: Process starts via `start_link/3` or `start/3`.

**Parameters**:
- `Args :: term()` - Arguments passed to start function

**Returns**:
- `{ok, State}` - Success with initial state
- `{ok, State, Timeout}` - Success with timeout (milliseconds or `hibernate`)
- `{ok, State, {continue, Continue}}` - Success with continuation
- `{stop, Reason}` - Initialization failed, don't start
- `ignore` - Don't start (supervisor won't restart)

**Examples**:

```erlang
% Simple initialization
init([]) ->
    {ok, #state{}}.

% Initialize with arguments
init([Port, Options]) ->
    case gen_tcp:listen(Port, Options) of
        {ok, Socket} ->
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

% Initialize with timeout
init([]) ->
    % Timeout after 5 seconds of inactivity
    {ok, #state{}, 5000}.

% Initialize with continuation
init([]) ->
    % Defer expensive initialization
    {ok, #state{}, {continue, load_data}}.

% Fail initialization
init([InvalidConfig]) ->
    {stop, {invalid_config, InvalidConfig}}.

% Ignore (used for conditional startup)
init([disabled]) ->
    ignore.
```

---

### handle_call(Request, From, State) → Result

**Description**: Handle synchronous messages. Client blocks until reply.

**Called when**: Client calls `gen_server:call/2,3`.

**Parameters**:
- `Request :: term()` - Message sent by caller
- `From :: {pid(), Tag}` - Caller identity (use for delayed replies)
- `State :: term()` - Current state

**Returns**:
- `{reply, Reply, NewState}` - Send reply, update state
- `{reply, Reply, NewState, Timeout | hibernate | {continue, Continue}}` - Reply with action
- `{noreply, NewState}` - Don't reply yet (use `gen_server:reply/2` later)
- `{noreply, NewState, Timeout | hibernate | {continue, Continue}}` - No reply with action
- `{stop, Reason, Reply, NewState}` - Reply and stop
- `{stop, Reason, NewState}` - Stop without reply

**Examples**:

```erlang
% Basic request-reply
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State#state.data, undefined),
    {reply, Value, State}.

% Update state
handle_call({put, Key, Value}, _From, State) ->
    NewData = maps:put(Key, Value, State#state.data),
    NewState = State#state{data = NewData},
    {reply, ok, NewState}.

% Reply with timeout
handle_call(long_operation, _From, State) ->
    Result = expensive_computation(),
    {reply, Result, State, 30000}.  % 30 second timeout

% Reply with continuation
handle_call(initialize, _From, State) ->
    {reply, ok, State, {continue, load_cache}}.

% Delayed reply (reply later)
handle_call(async_request, From, State) ->
    spawn(fun() ->
        Result = slow_operation(),
        gen_server:reply(From, Result)
    end),
    {noreply, State}.

% Conditional stop
handle_call(shutdown_if_empty, _From, State) ->
    case maps:size(State#state.data) of
        0 ->
            {stop, normal, ok, State};
        _ ->
            {reply, {error, not_empty}, State}
    end.

% Error handling
handle_call(risky_operation, _From, State) ->
    case perform_operation() of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```

---

### handle_cast(Request, State) → Result

**Description**: Handle asynchronous messages. No reply sent to caller.

**Called when**: Client calls `gen_server:cast/2`.

**Parameters**:
- `Request :: term()` - Message sent by caller
- `State :: term()` - Current state

**Returns**:
- `{noreply, NewState}` - Update state
- `{noreply, NewState, Timeout | hibernate | {continue, Continue}}` - Update with action
- `{stop, Reason, NewState}` - Stop the server

**Examples**:

```erlang
% Fire-and-forget update
handle_cast({update, Key, Value}, State) ->
    NewData = maps:put(Key, Value, State#state.data),
    {noreply, State#state{data = NewData}}.

% Async notification
handle_cast({notify, Event}, State) ->
    lists:foreach(
        fun(Subscriber) -> Subscriber ! Event end,
        State#state.subscribers
    ),
    {noreply, State}.

% Graceful shutdown
handle_cast(shutdown, State) ->
    {stop, normal, State}.

% With timeout
handle_cast(reset, State) ->
    {noreply, #state{}, 5000}.  % Reset and timeout

% With continuation
handle_cast(refresh, State) ->
    {noreply, State, {continue, reload_data}}.
```

---

### handle_info(Info, State) → Result

**Description**: Handle all other messages (not calls or casts).

**Called when**: Process receives non-gen_server messages.

**Parameters**:
- `Info :: term()` - Raw message received
- `State :: term()` - Current state

**Returns**: Same as `handle_cast/2`

**Examples**:

```erlang
% Timer expiration
handle_info(timeout, State) ->
    % Called when timeout expires
    {stop, normal, State}.

% Custom timeout message
handle_info({timeout, Ref, refresh}, State) when Ref == State#state.timer_ref ->
    NewState = refresh_data(State),
    NewRef = erlang:start_timer(60000, self(), refresh),
    {noreply, NewState#state{timer_ref = NewRef}}.

% Process monitoring
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    NewState = handle_process_down(Pid, Reason, State),
    {noreply, NewState}.

% ETS messages
handle_info({ETS_transfer, TableId, FromPid, _Data}, State) ->
    {noreply, State#state{table = TableId}}.

% Socket messages (inet:setopts([{active, once}]))
handle_info({tcp, Socket, Data}, State) ->
    handle_data(Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    {stop, {shutdown, tcp_closed}, State}.

% Catch-all (important for robustness!)
handle_info(_Info, State) ->
    {noreply, State}.
```

---

### handle_continue(Continue, State) → Result

**Description**: Handle continuation after `init/1` or other callbacks.

**Called when**: Callback returns `{continue, Continue}`.

**Parameters**:
- `Continue :: term()` - Continuation token
- `State :: term()` - Current state

**Returns**: Same as `handle_cast/2`

**Examples**:

```erlang
% Defer expensive initialization
init([]) ->
    {ok, #state{}, {continue, load_data}}.

handle_continue(load_data, State) ->
    Data = expensive_load_from_database(),
    {noreply, State#state{data = Data}}.

% Chain continuations
handle_continue(step1, State) ->
    NewState = perform_step1(State),
    {noreply, NewState, {continue, step2}}.

handle_continue(step2, State) ->
    FinalState = perform_step2(State),
    {noreply, FinalState}.
```

---

### terminate(Reason, State) → ok

**Description**: Cleanup before process terminates.

**Called when**: Process is about to exit normally or abnormally.

**Parameters**:
- `Reason :: term()` - Termination reason
- `State :: term()` - Final state

**Returns**: Return value is ignored

**Examples**:

```erlang
terminate(Reason, State) ->
    % Close connections
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,

    % Delete ETS tables
    case State#state.table of
        undefined -> ok;
        Table -> ets:delete(Table)
    end,

    % Log termination
    logger:info("Server terminating: ~p", [Reason]),

    ok.

% Distinguish termination reasons
terminate(normal, _State) ->
    % Normal shutdown
    ok;
terminate(shutdown, _State) ->
    % Supervisor shutdown
    ok;
terminate({shutdown, _Reason}, _State) ->
    % Supervisor shutdown with reason
    ok;
terminate(_Reason, State) ->
    % Abnormal termination - log details
    logger:error("Abnormal termination: ~p, state: ~p", [_Reason, State]),
    ok.
```

**Important**: `terminate/2` is NOT guaranteed to be called! It's only called for:
- Normal exits (`{stop, Reason, State}`)
- Supervisor shutdown
- `sys:terminate/2,3`

It's NOT called for:
- `exit(Pid, kill)` - brutal kill
- Untrapped exits (default)

**To guarantee cleanup, trap exits**:

```erlang
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
```

---

### code_change(OldVsn, State, Extra) → {ok, NewState}

**Description**: Migrate state during hot code reload.

**Called when**: Release upgrade triggers code change.

**Parameters**:
- `OldVsn :: term() | {down, term()}` - Old version or `{down, Version}` for downgrade
- `State :: term()` - Old state
- `Extra :: term()` - Extra data from appup file

**Returns**:
- `{ok, NewState}` - New state
- `{error, Reason}` - Migration failed

**Examples**:

```erlang
% Upgrade state structure
code_change(_OldVsn, #state_v1{data = Data}, _Extra) ->
    NewState = #state_v2{
        data = Data,
        cache = #{},  % New field
        timestamp = erlang:system_time(second)
    },
    {ok, NewState};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.  % Already new version

% Handle multiple versions
code_change("1.0.0", OldState, _Extra) ->
    {ok, migrate_1_to_2(OldState)};

code_change("2.0.0", OldState, _Extra) ->
    {ok, migrate_2_to_3(OldState)};

code_change({down, "2.0.0"}, NewState, _Extra) ->
    {ok, downgrade_2_to_1(NewState)}.

% Migrate with validation
code_change(_OldVsn, OldState, _Extra) ->
    case validate_state(OldState) of
        ok ->
            {ok, transform_state(OldState)};
        {error, Reason} ->
            {error, {migration_failed, Reason}}
    end.
```

See [Hot Code Reloading](../how-to/hot-code-reloading.md) for details.

---

## API Functions

### Starting Servers

#### start_link(Module, Args, Options) → Result

Start server linked to calling process.

```erlang
gen_server:start_link(?MODULE, [], []).
%% {ok, Pid}

gen_server:start_link({local, my_server}, ?MODULE, [Port], []).
%% {ok, Pid}

gen_server:start_link({global, my_server}, ?MODULE, [], []).
%% {ok, Pid}

gen_server:start_link({via, Registry, Name}, ?MODULE, [], []).
%% {ok, Pid}
```

**Options**:
- `{timeout, Timeout}` - Max time for `init/1` (default 5000ms)
- `{spawn_opt, SpawnOpts}` - Process spawn options
- `{debug, Dbgs}` - sys debug options

#### start(Module, Args, Options) → Result

Start server not linked to calling process.

```erlang
gen_server:start(?MODULE, [], []).
%% {ok, Pid}
```

### Calling Servers

#### call(ServerRef, Request) → Reply

Synchronous call with 5 second timeout.

```erlang
gen_server:call(my_server, {get, key}).
%% {ok, value}
```

#### call(ServerRef, Request, Timeout) → Reply

Synchronous call with custom timeout.

```erlang
gen_server:call(my_server, expensive_operation, 60000).
%% {ok, result}

gen_server:call(my_server, operation, infinity).
%% No timeout
```

**Throws**: `{timeout, {gen_server, call, [ServerRef, Request, Timeout]}}` if timeout.

#### cast(ServerRef, Request) → ok

Asynchronous cast (always returns `ok` immediately).

```erlang
gen_server:cast(my_server, {update, key, value}).
%% ok
```

#### reply(From, Reply) → ok

Send delayed reply.

```erlang
handle_call(async_operation, From, State) ->
    spawn(fun() ->
        Result = slow_work(),
        gen_server:reply(From, Result)
    end),
    {noreply, State}.
```

### Management

#### stop(ServerRef) → ok

Stop server gracefully.

```erlang
gen_server:stop(my_server).
%% ok
```

#### stop(ServerRef, Reason, Timeout) → ok

Stop with custom reason and timeout.

```erlang
gen_server:stop(my_server, shutdown, 10000).
%% ok
```

## ServerRef Formats

```erlang
% Local registration
Pid
RegisteredName

% Global registration
{global, GlobalName}

% Via registration (custom registry)
{via, Module, Name}
```

## Common Patterns

### Request-Reply
```erlang
handle_call({get, Key}, _From, State) ->
    {reply, maps:get(Key, State#state.data), State}.
```

### Fire-and-Forget
```erlang
handle_cast({set, Key, Value}, State) ->
    {noreply, State#state{data = maps:put(Key, Value, State#state.data)}}.
```

### Delayed Reply
```erlang
handle_call(slow_op, From, State) ->
    spawn(fun() -> gen_server:reply(From, work()) end),
    {noreply, State}.
```

### Timeout
```erlang
handle_call(request, _From, State) ->
    {reply, ok, State, 5000}.

handle_info(timeout, State) ->
    {stop, normal, State}.
```

### Hibernation
```erlang
handle_call(rarely_used, _From, State) ->
    {reply, ok, State, hibernate}.  % Minimize memory
```

## See Also

- [Tutorial: Your First OTP Application](../tutorials/01-first-otp-app.md)
- [How-To: Handle Process Crashes](../how-to/handle-process-crashes.md)
- [How-To: Hot Code Reloading](../how-to/hot-code-reloading.md)
- [Supervisor API Reference](supervisor-api.md)

---

**Official Docs**: [gen_server - Erlang](https://www.erlang.org/doc/man/gen_server.html)
