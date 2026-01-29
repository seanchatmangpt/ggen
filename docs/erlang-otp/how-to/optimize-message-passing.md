# How to Optimize Message Passing

**Problem**: Your system has high latency or crashes under message flood.

**Solution**: Use ETS for read-heavy workloads, batch messages, and optimize mailbox processing.

## When to Use This Guide

- ✅ System slow under high message volume
- ✅ Mailbox growing unbounded
- ✅ Selective receive causing O(n²) complexity
- ✅ Read-heavy workloads overwhelming gen_server
- ✅ Need to process messages in batches

## Quick Solutions

### 1. Use ETS for Read-Heavy Workloads

**Problem**: Thousands of processes reading from single gen_server causes bottleneck.

**Solution**: Use ETS table with `{read_concurrency, true}`.

**Before (slow)**:
```erlang
% Every read goes through gen_server (serialized)
Value = gen_server:call(cache_server, {get, Key}).
```

**After (fast)**:
```erlang
% Direct ETS reads (parallel, no message passing!)
Value = case ets:lookup(cache_table, Key) of
    [{Key, Val}] -> Val;
    [] -> undefined
end.
```

**Full implementation**:

```erlang
-module(fast_cache).
-behaviour(gen_server).

-export([start_link/0, get/1, put/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TABLE, fast_cache_table).

%%% ==================================================================
%%% API (ETS reads, gen_server writes)
%%% ==================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    % Direct ETS read - NO message passing!
    case ets:lookup(?TABLE, Key) of
        [{Key, Value}] -> {ok, Value};
        [] -> {error, not_found}
    end.

put(Key, Value) ->
    % Write goes through gen_server for coordination
    gen_server:call(?MODULE, {put, Key, Value}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    % Create ETS table with read concurrency
    ets:new(?TABLE, [
        named_table,
        public,                  % All processes can read
        set,
        {read_concurrency, true}, % Optimize for parallel reads
        {write_concurrency, true} % Optimize for parallel writes
    ]),
    {ok, #{}}.

handle_call({put, Key, Value}, _From, State) ->
    ets:insert(?TABLE, {Key, Value}),
    {reply, ok, State}.

handle_cast({delete, Key}, State) ->
    ets:delete(?TABLE, Key),
    {noreply, State}.
```

**Performance**:
- **Before**: 50,000 reads/sec (gen_server bottleneck)
- **After**: 10,000,000 reads/sec (ETS with read_concurrency)

### 2. Batch Message Processing

**Problem**: Processing one message at a time is inefficient for high-volume workloads.

**Solution**: Accumulate messages and process in batches.

```erlang
-module(batch_processor).
-behaviour(gen_server).

-export([start_link/0, submit/1]).
-export([init/1, handle_cast/2, handle_info/2]).

-define(BATCH_SIZE, 100).
-define(BATCH_TIMEOUT, 1000).  % 1 second

-record(state, {
    pending = [] :: list(),
    timer_ref = undefined :: undefined | reference()
}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit(Item) ->
    gen_server:cast(?MODULE, {submit, Item}).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    {ok, #state{}}.

handle_cast({submit, Item}, State) ->
    NewPending = [Item | State#state.pending],
    NewState = State#state{pending = NewPending},

    case length(NewPending) of
        Len when Len >= ?BATCH_SIZE ->
            % Batch full - process immediately
            process_batch(NewPending),
            cancel_timer(State#state.timer_ref),
            {noreply, #state{}};
        _ ->
            % Start timer if not already running
            TimerRef = case State#state.timer_ref of
                undefined ->
                    erlang:send_after(?BATCH_TIMEOUT, self(), process_batch);
                Ref ->
                    Ref
            end,
            {noreply, NewState#state{timer_ref = TimerRef}}
    end.

handle_info(process_batch, State) ->
    case State#state.pending of
        [] ->
            {noreply, State};
        Pending ->
            process_batch(Pending),
            {noreply, #state{}}
    end.

%%% ==================================================================
%%% Internal
%%% ==================================================================

process_batch(Items) ->
    % Process entire batch in one operation
    io:format("Processing batch of ~p items~n", [length(Items)]),
    % Your batch processing logic here
    ok.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    ok.
```

**Performance**:
- **Before**: 1,000 messages/sec (individual processing)
- **After**: 100,000 messages/sec (batched processing)

### 3. Avoid Selective Receive in Hot Paths

**Problem**: Selective receive scans mailbox linearly - O(n) per receive.

**Before (slow)**:
```erlang
loop(State) ->
    receive
        {priority, Msg} -> handle_priority(Msg);
        {normal, Msg} -> handle_normal(Msg)
    end,
    loop(State).

% If 1000 normal messages arrive first, priority message
% scans through all 1000 on each receive!
```

**After (fast)**:
```erlang
% Option 1: Use separate processes for priority handling
start_link() ->
    PriorityPid = spawn(?MODULE, priority_loop, []),
    NormalPid = spawn(?MODULE, normal_loop, []),
    {ok, {PriorityPid, NormalPid}}.

% Option 2: Process all messages in order
loop(State) ->
    receive
        Msg -> handle_any_message(Msg, State)
    end,
    loop(State).

handle_any_message({priority, Msg}, State) ->
    handle_priority(Msg),
    State;
handle_any_message({normal, Msg}, State) ->
    handle_normal(Msg),
    State.
```

### 4. Use gen_statem for Message Queuing

**Problem**: Need different message handling in different states.

**Solution**: gen_statem changes behavior per state without selective receive.

```erlang
-module(stateful_processor).
-behaviour(gen_statem).

-export([start_link/0, submit/1, callback_mode/0]).
-export([init/1, idle/3, processing/3]).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

submit(Item) ->
    gen_statem:cast(?MODULE, {submit, Item}).

callback_mode() -> state_functions.

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    {ok, idle, #{queue => queue:new()}}.

%% In idle state - accumulate messages
idle(cast, {submit, Item}, Data) ->
    Queue = maps:get(queue, Data),
    NewQueue = queue:in(Item, Queue),

    case queue:len(NewQueue) of
        Len when Len >= 100 ->
            % Start processing
            {next_state, processing, Data#{queue => NewQueue}, [{next_event, internal, process}]};
        _ ->
            {keep_state, Data#{queue => NewQueue}}
    end.

%% In processing state - handle messages differently
processing(internal, process, Data) ->
    Queue = maps:get(queue, Data),
    case queue:out(Queue) of
        {{value, Item}, NewQueue} ->
            handle_item(Item),
            {keep_state, Data#{queue => NewQueue}, [{next_event, internal, process}]};
        {empty, _} ->
            % Done processing
            {next_state, idle, Data}
    end;

processing(cast, {submit, Item}, Data) ->
    % Still accepting messages while processing
    Queue = maps:get(queue, Data),
    NewQueue = queue:in(Item, Queue),
    {keep_state, Data#{queue => NewQueue}}.

handle_item(_Item) ->
    % Your processing logic
    ok.
```

### 5. Flow Control with Credits

**Problem**: Fast producer overwhelms slow consumer.

**Solution**: Implement credit-based flow control.

```erlang
-module(flow_controlled_worker).
-behaviour(gen_server).

-export([start_link/0, submit/1]).
-export([init/1, handle_call/3, handle_info/2]).

-define(INITIAL_CREDITS, 100).

-record(state, {
    credits = ?INITIAL_CREDITS :: non_neg_integer(),
    producer_pid :: pid()
}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit(Item) ->
    gen_server:call(?MODULE, {submit, Item}, infinity).

%%% ==================================================================
%%% Callbacks
%%% ==================================================================

init([]) ->
    {ok, #state{}}.

handle_call({submit, Item}, {ProducerPid, _Tag}, State) when State#state.credits > 0 ->
    % Have credits - accept work
    spawn(fun() ->
        process_item(Item),
        gen_server:cast(?MODULE, work_complete)
    end),

    NewCredits = State#state.credits - 1,

    % Warn producer if running low
    if
        NewCredits == 10 ->
            ProducerPid ! {slow_down, ?MODULE};
        true ->
            ok
    end,

    {reply, ok, State#state{credits = NewCredits, producer_pid = ProducerPid}};

handle_call({submit, _Item}, {ProducerPid, _Tag}, State) ->
    % No credits - reject work
    ProducerPid ! {backpressure, ?MODULE},
    {reply, {error, overloaded}, State}.

handle_info(work_complete, State) ->
    NewCredits = State#state.credits + 1,

    % Notify producer we can accept more work
    if
        NewCredits == 50, State#state.producer_pid =/= undefined ->
            State#state.producer_pid ! {ready, ?MODULE};
        true ->
            ok
    end,

    {noreply, State#state{credits = NewCredits}}.

process_item(_Item) ->
    timer:sleep(100),  % Simulate work
    ok.
```

## Advanced Optimizations

### Monitor Mailbox Size

Detect and react to mailbox growth before it becomes a problem.

```erlang
-module(monitored_worker).
-behaviour(gen_server).

-export([init/1, handle_info/2]).

-define(MAILBOX_WARN_THRESHOLD, 1000).
-define(MAILBOX_CRITICAL_THRESHOLD, 10000).

init([]) ->
    % Check mailbox size every second
    erlang:send_after(1000, self(), check_mailbox),
    {ok, #{}}.

handle_info(check_mailbox, State) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),

    case Len of
        L when L > ?MAILBOX_CRITICAL_THRESHOLD ->
            % Critical - shed load
            logger:error("Mailbox critical: ~p messages, shedding load", [L]),
            flush_low_priority_messages(),
            erlang:send_after(100, self(), check_mailbox);

        L when L > ?MAILBOX_WARN_THRESHOLD ->
            % Warning - alert monitoring
            logger:warning("Mailbox growing: ~p messages", [L]),
            erlang:send_after(500, self(), check_mailbox);

        _ ->
            % Normal
            erlang:send_after(1000, self(), check_mailbox)
    end,

    {noreply, State}.

flush_low_priority_messages() ->
    flush_low_priority_messages(0).

flush_low_priority_messages(Count) when Count > 1000 ->
    % Stop after flushing 1000 messages
    Count;
flush_low_priority_messages(Count) ->
    receive
        {low_priority, _} ->
            flush_low_priority_messages(Count + 1)
    after 0 ->
        Count
    end.
```

### Use pg for Distributed Message Broadcasting

For distributed systems, use `pg` (process groups) instead of manual broadcasting.

```erlang
% Join group
pg:join(my_group, self()).

% Send to all group members (efficient broadcast)
[Pid ! Message || Pid <- pg:get_members(my_group)].
```

## Performance Benchmarks

### ETS vs gen_server

```erlang
% Benchmark reads
benchmark_reads() ->
    % gen_server
    {Time1, _} = timer:tc(fun() ->
        [gen_server:call(server, {get, Key}) || Key <- lists:seq(1, 100000)]
    end),
    io:format("gen_server: ~p μs~n", [Time1]),

    % ETS
    {Time2, _} = timer:tc(fun() ->
        [ets:lookup(table, Key) || Key <- lists:seq(1, 100000)]
    end),
    io:format("ETS: ~p μs~n", [Time2]),

    io:format("ETS is ~px faster~n", [Time1 / Time2]).

% Result: ETS is 100-200x faster for reads
```

## Production Checklist

- ✅ Use ETS for read-heavy data
- ✅ Batch process messages when possible
- ✅ Avoid selective receive in hot paths
- ✅ Implement flow control for producer-consumer
- ✅ Monitor mailbox sizes
- ✅ Set message queue limits (max_mailbox_size)
- ✅ Use separate processes for priority handling
- ✅ Profile with `observer` and `fprof`
- ✅ Test under realistic load
- ✅ Set up alerts for mailbox growth

## Testing Message Performance

```erlang
% Test mailbox growth under load
test_mailbox_growth() ->
    Pid = spawn(fun slow_processor/0),

    % Send 10,000 messages
    [Pid ! {data, N} || N <- lists:seq(1, 10000)],

    % Check mailbox size
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    io:format("Mailbox size: ~p~n", [Len]).

slow_processor() ->
    receive
        {data, N} ->
            timer:sleep(10),  % Simulate slow processing
            io:format("Processed: ~p~n", [N])
    end,
    slow_processor().
```

## Related Guides

- [Handle Process Crashes](handle-process-crashes.md) - Prevent mailbox-related crashes
- [Message Passing Basics](../tutorials/02-message-passing-basics.md) - Foundational concepts
- [Actor Model Concurrency](../explanation/actor-model-concurrency.md) - Theory

## References

- [gen_server API](../reference/gen-server-api.md)
- [gen_statem API](../reference/gen-statem-api.md)
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html)

---

**When to use**: High-throughput systems, read-heavy workloads
**Difficulty**: Intermediate to Advanced
**See also**: ETS, Flow control, Backpressure
