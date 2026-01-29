# gen_statem Behavior Reference

Complete API reference for the `gen_statem` behavior - advanced state machines in OTP.

## Overview

**gen_statem** (gen_erical state machine) provides:
- Explicit state transitions
- State-specific event handling
- State enter/exit actions
- Event postponement and insertion
- Replaces deprecated `gen_fsm`

**When to use**:
- Complex workflows with multiple states
- Protocol implementations (HTTP, WebSocket, SMTP)
- Connection state management
- Game logic with distinct phases

**When to use gen_server instead**:
- Simple request-reply pattern
- State is just data (no state transitions)
- Linear processing flow

## Callback Modes

### state_functions

Each state is a separate function.

```erlang
callback_mode() -> state_functions.

idle(EventType, EventContent, Data) ->
    % Handle events in idle state
    {next_state, connecting, Data}.

connecting(EventType, EventContent, Data) ->
    % Handle events in connecting state
    {next_state, connected, Data}.
```

**Use when**: Few states, clear separation.

### handle_event_function

Single function handles all states.

```erlang
callback_mode() -> handle_event_function.

handle_event(EventType, EventContent, State, Data) ->
    case State of
        idle -> handle_idle(EventContent, Data);
        connecting -> handle_connecting(EventContent, Data);
        connected -> handle_connected(EventContent, Data)
    end.
```

**Use when**: Many states, shared logic across states.

### state_functions with state_enter

Callbacks called on state entry:

```erlang
callback_mode() -> [state_functions, state_enter].

idle(enter, _OldState, Data) ->
    % Called when entering idle state
    {keep_state, Data};
idle(EventType, EventContent, Data) ->
    % Handle events in idle
    {next_state, connecting, Data}.
```

## Module Callbacks

### callback_mode() → Mode

**Required**: Define callback mode.

```erlang
% State functions
callback_mode() -> state_functions.

% Single handler
callback_mode() -> handle_event_function.

% State functions with enter callbacks
callback_mode() -> [state_functions, state_enter].
```

---

### init(Args) → Result

Initialize state machine.

**Returns**:
- `{ok, State, Data}` - Start in State with Data
- `{ok, State, Data, Actions}` - Start with actions
- `{stop, Reason}` - Initialization failed
- `ignore` - Don't start

```erlang
% Simple initialization
init([]) ->
    {ok, idle, #data{}}.

% Initialize with timeout
init([Timeout]) ->
    Actions = [{state_timeout, Timeout, connect}],
    {ok, idle, #data{timeout = Timeout}, Actions}.

% Initialize with side effects
init([Host, Port]) ->
    Actions = [{next_event, internal, {connect, Host, Port}}],
    {ok, connecting, #data{}, Actions}.
```

---

### StateName(EventType, EventContent, Data) → Result

**Mode**: `state_functions`

Handle events in specific state.

**EventType**:
- `{call, From}` - gen_statem:call
- `cast` - gen_statem:cast
- `info` - Regular Erlang message
- `internal` - Internal event
- `timeout` - Generic timeout
- `state_timeout` - State-specific timeout
- `{timeout, Name}` - Named timeout
- `enter` - State entry (if state_enter enabled)

**Returns**:
- `{next_state, NextState, NewData}` - Transition to NextState
- `{next_state, NextState, NewData, Actions}` - Transition with actions
- `{keep_state, NewData}` - Stay in current state
- `{keep_state, NewData, Actions}` - Stay with actions
- `{keep_state_and_data, Actions}` - No change
- `{repeat_state, NewData, Actions}` - Re-enter current state
- `{stop, Reason}` - Stop state machine
- `{stop, Reason, NewData}` - Stop with final data
- `{stop_and_reply, Reason, Replies}` - Stop and send replies

**Examples**:

```erlang
callback_mode() -> [state_functions, state_enter].

%% Idle state
idle(enter, _OldState, Data) ->
    % Entry action
    io:format("Entering idle state~n"),
    {keep_state, Data};

idle({call, From}, connect, Data) ->
    % Start connecting
    {next_state, connecting, Data, [{reply, From, ok}]};

idle(cast, reset, Data) ->
    % Stay in idle, reset data
    {keep_state, #data{}};

idle(info, _Msg, Data) ->
    % Ignore unexpected messages
    {keep_state, Data}.

%% Connecting state
connecting(enter, _OldState, Data) ->
    % Start connection attempt
    {ok, Socket} = gen_tcp:connect(Data#data.host, Data#data.port, []),
    NewData = Data#data{socket = Socket},
    {keep_state, NewData, [{state_timeout, 5000, connection_timeout}]};

connecting(state_timeout, connection_timeout, Data) ->
    % Connection timed out
    {next_state, idle, Data};

connecting(info, {tcp, Socket, Data}, StateData) when Socket == StateData#data.socket ->
    % Connection established
    {next_state, connected, StateData};

connecting(info, {tcp_closed, Socket}, StateData) when Socket == StateData#data.socket ->
    % Connection failed
    {next_state, idle, StateData}.

%% Connected state
connected(enter, _OldState, Data) ->
    io:format("Connected!~n"),
    {keep_state, Data};

connected({call, From}, {send, Message}, Data) ->
    gen_tcp:send(Data#data.socket, Message),
    {keep_state, Data, [{reply, From, ok}]};

connected(info, {tcp_closed, _Socket}, Data) ->
    % Disconnected
    {next_state, idle, Data};

connected(cast, disconnect, Data) ->
    gen_tcp:close(Data#data.socket),
    {next_state, idle, Data}.
```

---

### handle_event(EventType, EventContent, State, Data) → Result

**Mode**: `handle_event_function`

Single handler for all states.

```erlang
callback_mode() -> handle_event_function.

handle_event({call, From}, connect, idle, Data) ->
    {next_state, connecting, Data, [{reply, From, ok}]};

handle_event({call, From}, connect, _State, Data) ->
    % Already connecting/connected
    {keep_state, Data, [{reply, From, {error, already_connected}}]};

handle_event(cast, reset, _State, _Data) ->
    % Reset from any state
    {next_state, idle, #data{}};

handle_event(info, {tcp, Socket, Msg}, connected, Data) when Socket == Data#data.socket ->
    handle_message(Msg, Data),
    {keep_state, Data};

handle_event(EventType, EventContent, State, Data) ->
    % Unexpected event
    logger:warning("Unexpected event: ~p in state ~p", [EventContent, State]),
    {keep_state, Data}.
```

---

### terminate(Reason, State, Data) → ok

Cleanup before termination.

```erlang
terminate(_Reason, _State, Data) ->
    % Close socket
    case Data#data.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    ok.
```

---

### code_change(OldVsn, OldState, OldData, Extra) → {ok, NewState, NewData}

Migrate state during hot code reload.

```erlang
code_change(_OldVsn, State, #data_v1{counter = C}, _Extra) ->
    % Upgrade data structure
    NewData = #data_v2{counter = C, timestamp = erlang:system_time()},
    {ok, State, NewData};

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
```

## Actions

Actions modify state machine behavior:

### Replies

```erlang
{reply, From, Reply}
{reply, From, Reply}  % Multiple replies possible
```

### Timeouts

```erlang
{timeout, Time, EventContent}              % Generic timeout
{state_timeout, Time, EventContent}        % Cancel on state change
{{timeout, Name}, Time, EventContent}      % Named timeout

% Time values:
infinity    % No timeout
0           % Immediate
5000        % 5 seconds (milliseconds)
```

### Event Management

```erlang
{next_event, EventType, EventContent}  % Insert event to process next
postpone                                % Postpone current event
{postpone, Boolean}                     % Conditional postpone
```

### Hibernation

```erlang
hibernate  % Minimize memory (garbage collect)
```

## API Functions

### Starting

```erlang
gen_statem:start_link(?MODULE, [], []).
%% {ok, Pid}

gen_statem:start_link({local, my_statem}, ?MODULE, [], []).
%% {ok, Pid}
```

### Calling

```erlang
% Synchronous call
gen_statem:call(Pid, connect).
%% ok

gen_statem:call(Pid, {send, "data"}, 10000).
%% ok

% Asynchronous cast
gen_statem:cast(Pid, disconnect).
%% ok
```

### Management

```erlang
% Stop
gen_statem:stop(Pid).
%% ok

% Send any event
gen_statem:cast(Pid, Event).
Pid ! Event.
```

## Complete Example: TCP Client

```erlang
-module(tcp_client).
-behaviour(gen_statem).

-export([start_link/2, connect/1, send/2, disconnect/1, stop/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, connecting/3, connected/3]).

-record(data, {
    host :: string(),
    port :: integer(),
    socket :: gen_tcp:socket() | undefined,
    connect_timeout = 5000 :: integer()
}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link(Host, Port) ->
    gen_statem:start_link(?MODULE, [Host, Port], []).

connect(Pid) ->
    gen_statem:call(Pid, connect).

send(Pid, Message) ->
    gen_statem:call(Pid, {send, Message}).

disconnect(Pid) ->
    gen_statem:cast(Pid, disconnect).

stop(Pid) ->
    gen_statem:stop(Pid).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

callback_mode() -> [state_functions, state_enter].

init([Host, Port]) ->
    Data = #data{host = Host, port = Port},
    {ok, idle, Data}.

%%% ==================================================================
%%% State: idle
%%% ==================================================================

idle(enter, _OldState, _Data) ->
    keep_state_and_data;

idle({call, From}, connect, Data) ->
    % Trigger connection
    Actions = [
        {reply, From, ok},
        {next_event, internal, do_connect}
    ],
    {next_state, connecting, Data, Actions};

idle(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, idle, Data).

%%% ==================================================================
%%% State: connecting
%%% ==================================================================

connecting(enter, _OldState, Data) ->
    % Start connection attempt
    case gen_tcp:connect(Data#data.host, Data#data.port,
                        [binary, {active, true}, {packet, line}]) of
        {ok, Socket} ->
            NewData = Data#data{socket = Socket},
            {next_state, connected, NewData};
        {error, _Reason} ->
            % Retry after timeout
            Actions = [{state_timeout, Data#data.connect_timeout, retry}],
            {keep_state_and_data, Actions}
    end;

connecting(state_timeout, retry, Data) ->
    % Retry connection
    {repeat_state, Data};

connecting(cast, disconnect, _Data) ->
    % User canceled
    {next_state, idle, #data{}};

connecting(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, connecting, Data).

%%% ==================================================================
%%% State: connected
%%% ==================================================================

connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected({call, From}, {send, Message}, Data) ->
    case gen_tcp:send(Data#data.socket, Message) of
        ok ->
            {keep_state, Data, [{reply, From, ok}]};
        {error, Reason} ->
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

connected(info, {tcp, Socket, ReceivedData}, Data) when Socket == Data#data.socket ->
    % Handle received data
    io:format("Received: ~p~n", [ReceivedData]),
    {keep_state, Data};

connected(info, {tcp_closed, Socket}, Data) when Socket == Data#data.socket ->
    % Connection closed
    {next_state, idle, Data#data{socket = undefined}};

connected(cast, disconnect, Data) ->
    gen_tcp:close(Data#data.socket),
    {next_state, idle, Data#data{socket = undefined}};

connected(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, connected, Data).

%%% ==================================================================
%%% Common handlers
%%% ==================================================================

handle_common({call, From}, get_state, State, Data) ->
    {keep_state, Data, [{reply, From, State}]};

handle_common(EventType, EventContent, State, Data) ->
    logger:warning("Unexpected event ~p:~p in state ~p", [EventType, EventContent, State]),
    {keep_state, Data}.

%%% ==================================================================
%%% Termination
%%% ==================================================================

terminate(_Reason, _State, Data) ->
    case Data#data.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
```

**Usage**:

```erlang
{ok, Pid} = tcp_client:start_link("localhost", 8080).
tcp_client:connect(Pid).
tcp_client:send(Pid, "Hello\n").
tcp_client:disconnect(Pid).
```

## Advanced Patterns

### Event Postponement

```erlang
% Postpone events until ready
connecting({call, From}, {send, _Msg}, Data) ->
    % Not ready to send, postpone until connected
    {keep_state, Data, [postpone, {reply, From, {error, not_connected}}]};

connected(enter, _OldState, Data) ->
    % Now process postponed events
    keep_state_and_data.
```

### Named Timeouts

```erlang
idle({call, From}, start, Data) ->
    Actions = [
        {{timeout, heartbeat}, 1000, send_heartbeat},
        {{timeout, cleanup}, 60000, cleanup},
        {reply, From, ok}
    ],
    {next_state, running, Data, Actions};

running({timeout, heartbeat}, send_heartbeat, Data) ->
    send_heartbeat(),
    Actions = [{{timeout, heartbeat}, 1000, send_heartbeat}],
    {keep_state, Data, Actions};

running({timeout, cleanup}, cleanup, Data) ->
    cleanup_old_data(),
    Actions = [{{timeout, cleanup}, 60000, cleanup}],
    {keep_state, Data, Actions}.
```

### State Entry/Exit Actions

```erlang
callback_mode() -> [state_functions, state_enter].

state_a(enter, OldState, Data) ->
    io:format("Entering state_a from ~p~n", [OldState]),
    Actions = [{state_timeout, 5000, timeout}],
    {keep_state, Data, Actions};

state_a(exit, NextState, Data) ->
    io:format("Exiting state_a to ~p~n", [NextState]),
    cleanup_state_a(),
    keep_state_and_data.
```

## See Also

- [Tutorial: Message Passing Basics](../tutorials/02-message-passing-basics.md)
- [How-To: Optimize Message Passing](../how-to/optimize-message-passing.md)
- [gen_server API Reference](gen-server-api.md)

---

**Official Docs**: [gen_statem - Erlang](https://www.erlang.org/doc/man/gen_statem.html)
