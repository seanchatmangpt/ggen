# Message Passing Basics

**Goal**: Master Erlang's message passing patterns and build asynchronous systems in 45 minutes.

**What You'll Learn**:
- Send and receive messages between processes
- Pattern matching in receive blocks
- Selective receive and mailbox management
- Timeout handling
- Process links and monitors

## What You'll Build

A chat room system demonstrating:
- Asynchronous message broadcasting
- Process registration and discovery
- Selective message reception
- Timeout-based cleanup

## Prerequisites

- Completed [Your First OTP Application](01-first-otp-app.md)
- Understanding of pattern matching
- 45 minutes of focused time

## Core Concepts

### The Actor Model in 3 Lines

```erlang
% 1. Every process has a mailbox (unlimited queue)
% 2. Send messages asynchronously (never blocks)
% 3. Receive messages selectively (pattern matching)
```

**Key insight**: There is NO shared memory. Processes communicate only via message passing.

## Step 1: Basic Send and Receive

Create `src/basic_messenger.erl`:

```erlang
-module(basic_messenger).
-export([start/0, loop/0, send_message/2]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
        {From, Message} ->
            io:format("Received from ~p: ~p~n", [From, Message]),
            From ! {self(), "Message received!"},
            loop();  % Recurse to continue receiving
        stop ->
            io:format("Stopping messenger~n"),
            ok
    end.

send_message(Pid, Message) ->
    Pid ! {self(), Message}.
```

**Test it**:

```erlang
% Compile and start shell
rebar3 shell

% Start messenger process
Pid = basic_messenger:start().
%% <0.145.0>

% Send message
basic_messenger:send_message(Pid, "Hello").
%% Received from <0.142.0>: "Hello"
%% {<0.145.0>, "Message received!"}

% Receive the reply
flush().
%% Shell got {<0.145.0>, "Message received!"}
%% ok

% Stop messenger
Pid ! stop.
%% Stopping messenger
%% stop
```

**What's happening?**
1. `spawn/3` creates new process running `loop/0`
2. `!` sends message to process mailbox (asynchronous)
3. `receive` blocks until matching message arrives
4. Pattern matching selects which message to process
5. Recursion keeps process alive

## Step 2: Selective Receive

Messages don't have to be processed in order:

```erlang
-module(selective_receiver).
-export([start/0, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
        {priority, Message} ->
            io:format("PRIORITY: ~p~n", [Message]),
            loop();
        {normal, Message} ->
            io:format("Normal: ~p~n", [Message]),
            loop();
        shutdown ->
            io:format("Shutting down~n")
    end.
```

**Test selective receive**:

```erlang
Pid = selective_receiver:start().

% Send normal message first
Pid ! {normal, "First message"}.

% Send priority message second
Pid ! {priority, "Urgent!"}.

% Priority is processed first!
%% PRIORITY: "Urgent!"
%% Normal: "First message"
```

**Key insight**: `receive` scans the mailbox from oldest to newest, but only processes the first message that matches any pattern. Unmatched messages stay in the mailbox.

## Step 3: Timeouts

Prevent infinite blocking with timeouts:

```erlang
-module(timeout_example).
-export([wait_for_message/1]).

wait_for_message(Timeout) ->
    receive
        {data, Value} ->
            {ok, Value}
    after Timeout ->
        {error, timeout}
    end.
```

**Test it**:

```erlang
% Timeout after 1 second
timeout_example:wait_for_message(1000).
%% {error, timeout}

% Send message before timeout
Pid = self().
spawn(fun() ->
    timer:sleep(500),
    Pid ! {data, "In time!"}
end).
timeout_example:wait_for_message(1000).
%% {ok, "In time!"}
```

## Step 4: Build a Chat Room

Now let's combine concepts into a practical system.

Create `src/chat_room.erl`:

```erlang
-module(chat_room).
-export([start/0, join/2, leave/2, send_message/2, room_loop/1]).

%%% ==================================================================
%%% API
%%% ==================================================================

start() ->
    spawn(?MODULE, room_loop, [[]]).

join(RoomPid, UserName) ->
    RoomPid ! {join, self(), UserName}.

leave(RoomPid, UserName) ->
    RoomPid ! {leave, self(), UserName}.

send_message(RoomPid, Message) ->
    RoomPid ! {message, self(), Message}.

%%% ==================================================================
%%% Internal
%%% ==================================================================

room_loop(Users) ->
    receive
        {join, Pid, UserName} ->
            io:format("~s joined the room~n", [UserName]),
            NewUsers = [{Pid, UserName} | Users],
            broadcast(NewUsers, {system, UserName ++ " joined"}),
            room_loop(NewUsers);

        {leave, Pid, UserName} ->
            io:format("~s left the room~n", [UserName]),
            NewUsers = lists:keydelete(Pid, 1, Users),
            broadcast(NewUsers, {system, UserName ++ " left"}),
            room_loop(NewUsers);

        {message, FromPid, Message} ->
            case lists:keyfind(FromPid, 1, Users) of
                {FromPid, UserName} ->
                    broadcast(Users, {message, UserName, Message}),
                    room_loop(Users);
                false ->
                    io:format("Message from unknown user~n"),
                    room_loop(Users)
            end;

        shutdown ->
            broadcast(Users, {system, "Room shutting down"}),
            ok
    end.

broadcast(Users, Message) ->
    lists:foreach(
        fun({Pid, _UserName}) ->
            Pid ! Message
        end,
        Users
    ).
```

Create `src/chat_user.erl`:

```erlang
-module(chat_user).
-export([start/1, user_loop/1]).

start(UserName) ->
    spawn(?MODULE, user_loop, [UserName]).

user_loop(UserName) ->
    receive
        {system, Message} ->
            io:format("[SYSTEM] ~s~n", [Message]),
            user_loop(UserName);

        {message, From, Message} ->
            io:format("[~s] ~s~n", [From, Message]),
            user_loop(UserName);

        shutdown ->
            io:format("~s shutting down~n", [UserName]),
            ok
    end.
```

## Step 5: Test Chat Room

```erlang
% Start room
Room = chat_room:start().

% Create users
Alice = chat_user:start("Alice").
Bob = chat_user:start("Bob").
Carol = chat_user:start("Carol").

% Users join
chat_room:join(Room, "Alice").
chat_room:join(Room, "Bob").
chat_room:join(Room, "Carol").
%% Alice joined the room
%% Bob joined the room
%% Carol joined the room
%% [SYSTEM] Alice joined
%% [SYSTEM] Bob joined
%% [SYSTEM] Carol joined

% Send messages
chat_room:send_message(Room, "Hello everyone!").
%% [Alice] Hello everyone!
%% [Alice] Hello everyone!
%% [Alice] Hello everyone!

% User leaves
chat_room:leave(Room, "Bob").
%% Bob left the room
%% [SYSTEM] Bob left
%% [SYSTEM] Bob left

% Send another message
chat_room:send_message(Room, "Bob is gone").
%% [Alice] Bob is gone
%% [Alice] Bob is gone

% Shutdown
Room ! shutdown.
%% [SYSTEM] Room shutting down
%% [SYSTEM] Room shutting down
```

## Step 6: Process Links and Monitors

**Problem**: What if a user process crashes? Room should be notified.

### Links (Bidirectional)

```erlang
% If either process dies, the other dies too
Pid = spawn_link(fun() -> loop() end).
```

### Monitors (Unidirectional)

```erlang
-module(monitored_chat_room).
-export([start/0, join/2, room_loop/1]).

start() ->
    spawn(?MODULE, room_loop, [[]]).

join(RoomPid, UserName) ->
    RoomPid ! {join, self(), UserName}.

room_loop(Users) ->
    receive
        {join, Pid, UserName} ->
            MonitorRef = monitor(process, Pid),
            NewUsers = [{Pid, MonitorRef, UserName} | Users],
            io:format("~s joined (monitored)~n", [UserName]),
            room_loop(NewUsers);

        {'DOWN', MonitorRef, process, Pid, Reason} ->
            case lists:keyfind(MonitorRef, 2, Users) of
                {Pid, MonitorRef, UserName} ->
                    io:format("~s crashed: ~p~n", [UserName, Reason]),
                    NewUsers = lists:keydelete(Pid, 1, Users),
                    room_loop(NewUsers);
                false ->
                    room_loop(Users)
            end;

        shutdown ->
            ok
    end.
```

**Test crash handling**:

```erlang
Room = monitored_chat_room:start().

% Create user that will crash
BadUser = spawn(fun() ->
    monitored_chat_room:join(Room, "BadUser"),
    timer:sleep(1000),
    1 / 0  % Crash!
end).

%% BadUser joined (monitored)
%% BadUser crashed: {badarith,[{erlang,'/',[1,0],[]},...]}
```

**Links vs Monitors**:

| Feature | Links | Monitors |
|---------|-------|----------|
| Direction | Bidirectional | Unidirectional |
| Default action | Crash both | Send message |
| Use case | Fail together | Track failures |
| Example | gen_server + supervisor | Process registry |

## Step 7: Message Patterns Cheatsheet

### Fire-and-Forget (Cast)
```erlang
Pid ! {update, Data}.  % Don't wait for reply
```

### Request-Reply (Call)
```erlang
Pid ! {self(), get_data},
receive
    {Pid, Reply} -> Reply
after 5000 ->
    {error, timeout}
end.
```

### Broadcast
```erlang
lists:foreach(fun(P) -> P ! Message end, Subscribers).
```

### Selective Receive
```erlang
receive
    {priority, Msg} -> handle_priority(Msg);
    {normal, Msg} -> handle_normal(Msg)
end.
```

### Flush Mailbox
```erlang
flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.
```

## Summary

**What You Built**:
- ✅ Basic message send/receive
- ✅ Selective receive with pattern matching
- ✅ Timeout handling
- ✅ Chat room with broadcast
- ✅ Process monitoring for fault detection

**Key Takeaways**:
1. **Asynchronous** - `!` never blocks
2. **Selective** - Pattern match messages in any order
3. **Isolated** - No shared memory, only messages
4. **Monitored** - Detect failures without crashing

**Performance Notes**:
- Mailbox is unbounded (can grow indefinitely)
- Selective receive scans mailbox linearly (O(n))
- For high-throughput, avoid selective receive or use ETS
- See [Optimize Message Passing](../how-to/optimize-message-passing.md)

## Next Steps

- **Tutorial 03**: [Building Supervision Trees](03-supervision-trees.md) - Fault tolerance at scale
- **How-To**: [Optimize Message Passing](../how-to/optimize-message-passing.md) - Performance tuning
- **Explanation**: [Actor Model and Concurrency](../explanation/actor-model-concurrency.md) - Theory

## Common Pitfalls

**❌ Forgetting to recurse**:
```erlang
loop() ->
    receive
        Message -> handle(Message)
        % Oops! Process exits after one message
    end.
```

**✅ Always recurse**:
```erlang
loop() ->
    receive
        Message -> handle(Message),
                  loop()  % Keep receiving
    end.
```

**❌ Blocking forever**:
```erlang
receive
    {specific_tag, Data} -> Data
    % What if message never arrives?
end.
```

**✅ Always use timeouts**:
```erlang
receive
    {specific_tag, Data} -> Data
after 5000 ->
    {error, timeout}
end.
```

**❌ Growing mailbox**:
```erlang
% Selective receive with unmatched messages
loop() ->
    receive
        {expected, Data} -> handle(Data)
        % Other messages accumulate in mailbox!
    end,
    loop().
```

**✅ Catch-all pattern**:
```erlang
loop() ->
    receive
        {expected, Data} -> handle(Data);
        Other -> io:format("Unexpected: ~p~n", [Other])
    end,
    loop().
```

## References

- [gen_server API Reference](../reference/gen-server-api.md) - Structured message passing
- [Actor Model Explanation](../explanation/actor-model-concurrency.md) - Theoretical foundation

---

**Time to complete**: ~45 minutes
**Difficulty**: Beginner
**Prerequisites**: Basic Erlang syntax, [Tutorial 01](01-first-otp-app.md)
