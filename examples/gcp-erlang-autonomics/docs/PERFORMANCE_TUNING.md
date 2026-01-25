# Erlang Performance Tuning and Profiling

## Table of Contents
1. [Message Queue Optimization](#message-queue-optimization)
2. [Memory Profiling](#memory-profiling)
3. [CPU Hotspots and eprof](#cpu-hotspots-and-eprof)
4. [Latency Analysis with fprof](#latency-analysis-with-fprof)
5. [Benchmarking FSM Operations](#benchmarking-fsm-operations)
6. [Production Performance Optimization](#production-performance-optimization)

---

## Message Queue Optimization

### Understanding Message Queues

Each Erlang process has a **mailbox** (message queue):

```
Process A
├─ Mailbox (FIFO queue)
│   ├─ {operation, data1}
│   ├─ {operation, data2}
│   ├─ {operation, data3}
│   └─ ... (potentially thousands)
└─ Processes messages one at a time
   (slower if queue is long)
```

**Problem**: Large message queues cause:
- High latency (message waits in queue)
- Memory bloat (each message in memory)
- Potential crashes if unlimited

### Measure Queue Depth

```erlang
% Check message queue for specific process
(erlang@localhost)1> process_info(Pid, message_queue_len).
{message_queue_len, 1250}  % 1250 messages waiting!

% Find processes with large queues
find_overloaded_processes() ->
    Processes = processes(),
    OverloadedPids = lists:filter(fun(Pid) ->
        case process_info(Pid, message_queue_len) of
            {message_queue_len, Len} when Len > 1000 ->
                true;
            _ -> false
        end
    end, Processes),

    lists:foreach(fun(Pid) ->
        {message_queue_len, Len} = process_info(Pid, message_queue_len),
        {_MFA, Info} = process_info(Pid, current_function),
        io:format("PID ~p: ~p messages, ~p~n", [Pid, Len, Info])
    end, OverloadedPids).
```

### Strategy 1: Reduce Inbound Rate

```erlang
% In FSM, rate-limit incoming operations
-record(rate_limiter, {
    max_queuelen = 100,
    check_interval = 100  % Check every 100ms
}).

handle_event({call, From}, {operation, Data}, State, FsmData) ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > 100 ->
            % Queue is full - reject with backpressure signal
            {keep_state_and_data,
             [{reply, From, {error, {queue_full, Len}}}]};

        _ ->
            % Queue has space - process normally
            {ok, Result} = execute_operation(Data, FsmData),
            {keep_state_and_data,
             [{reply, From, {ok, Result}}]}
    end.
```

### Strategy 2: Batch Processing

Instead of processing messages one at a time:

```erlang
% Collect batch of messages before processing
batch_process_loop(Fsm) ->
    % Collect up to 100 messages
    Batch = collect_batch(100, 1000),  % Max 100, timeout 1sec

    % Process batch together
    lists:foreach(fun(Message) ->
        process_single_message(Fsm, Message)
    end, Batch),

    batch_process_loop(Fsm).

collect_batch(Max, TimeoutMs) ->
    collect_batch(Max, TimeoutMs, []).

collect_batch(0, _Timeout, Acc) ->
    lists:reverse(Acc);

collect_batch(N, TimeoutMs, Acc) ->
    Deadline = erlang:system_time(millisecond) + TimeoutMs,
    TimeRemaining = Deadline - erlang:system_time(millisecond),

    if TimeRemaining < 1 ->
        lists:reverse(Acc);
    true ->
        receive
            Message ->
                collect_batch(N - 1, TimeoutMs, [Message | Acc])
        after TimeRemaining ->
            lists:reverse(Acc)
        end
    end.
```

### Strategy 3: Load Balancing Across Processes

Spread load across multiple FSM instances:

```erlang
-module(load_balanced_fsm_pool).

% Create pool of FSM instances to distribute load
create_pool(TenantId, PoolSize) ->
    Pids = [
        begin
            {ok, Pid} = entitlement_governor:start_link(
                TenantId, <<TenantId/binary, "_", (integer_to_binary(I))/binary>>),
            Pid
        end || I <- lists:seq(1, PoolSize)
    ],
    Pids.

% Route to least-loaded FSM in pool
route_operation(Pids, Operation) ->
    % Find FSM with smallest message queue
    {SelectedPid, _} = lists:min(
        [{Pid, queue_len(Pid)} || Pid <- Pids],
        fun({_P1, Q1}, {_P2, Q2}) -> Q1 < Q2 end),

    % Send operation to selected FSM
    gen_statem:call(SelectedPid, Operation).

queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len;
        undefined -> infinity
    end.
```

### Strategy 4: Priority Queues

Process high-priority messages first:

```erlang
% Two-queue design: high-priority and normal
-record(fsm_data, {
    high_priority_queue = [],
    normal_queue = [],
    processing_high = true  % Alternate between queues
}).

handle_event({call, From}, {operation, Priority, Data}, State, FsmData) ->
    NewFsmData = case Priority of
        high ->
            FsmData#fsm_data{
                high_priority_queue = [
                    {call, From, Data} | FsmData#fsm_data.high_priority_queue
                ]
            };
        normal ->
            FsmData#fsm_data{
                normal_queue = [
                    {call, From, Data} | FsmData#fsm_data.normal_queue
                ]
            }
    end,
    {keep_state, NewFsmData}.

% Process next message from appropriate queue
process_next_message(FsmData) ->
    case FsmData#fsm_data.processing_high of
        true ->
            % Try high-priority first
            case FsmData#fsm_data.high_priority_queue of
                [Message | Rest] ->
                    NewFsmData = FsmData#fsm_data{
                        high_priority_queue = Rest
                    },
                    {ok, NewFsmData, Message};

                [] ->
                    % No high-priority, switch to normal
                    process_next_message(
                        FsmData#fsm_data{processing_high = false})
            end;

        false ->
            % Process normal queue, but check high periodically
            case FsmData#fsm_data.normal_queue of
                [Message | Rest] ->
                    NewFsmData = FsmData#fsm_data{
                        normal_queue = Rest,
                        processing_high = true  % Check high next time
                    },
                    {ok, NewFsmData, Message};

                [] ->
                    {empty, FsmData}
            end
    end.
```

---

## Memory Profiling

### Identify Memory Leaks

```erlang
% Basic memory check
memory_usage_report() ->
    {ok, {memory, Stats}} = erlang:memory(),

    Total = proplists:get_value(total, Stats),
    Processes = proplists:get_value(processes, Stats),
    ProcessesUsed = proplists:get_value(processes_used, Stats),
    System = proplists:get_value(system, Stats),
    Atom = proplists:get_value(atom, Stats),
    Binary = proplists:get_value(binary, Stats),
    Code = proplists:get_value(code, Stats),
    Ets = proplists:get_value(ets, Stats),

    io:format("=== Memory Report ===~n", []),
    io:format("Total:          ~10s MB~n", [fmt_bytes(Total)]),
    io:format("  Processes:    ~10s MB (used: ~s MB)~n",
        [fmt_bytes(Processes), fmt_bytes(ProcessesUsed)]),
    io:format("  System:       ~10s MB~n", [fmt_bytes(System)]),
    io:format("  Atom:         ~10s MB~n", [fmt_bytes(Atom)]),
    io:format("  Binary:       ~10s MB~n", [fmt_bytes(Binary)]),
    io:format("  Code:         ~10s MB~n", [fmt_bytes(Code)]),
    io:format("  ETS:          ~10s MB~n", [fmt_bytes(Ets)]).

fmt_bytes(Bytes) ->
    io_lib:format("~.1f", [Bytes / 1024 / 1024]).

% Track memory over time to detect leaks
-record(memory_sample, {
    timestamp = 0,
    total = 0,
    processes = 0,
    binary = 0,
    ets = 0
}).

enable_memory_tracking() ->
    spawn_link(fun() -> memory_tracking_loop([]) end).

memory_tracking_loop(Samples) ->
    timer:sleep(60000),  % Sample every minute

    {ok, {memory, Stats}} = erlang:memory(),
    Sample = #memory_sample{
        timestamp = erlang:system_time(millisecond),
        total = proplists:get_value(total, Stats),
        processes = proplists:get_value(processes, Stats),
        binary = proplists:get_value(binary, Stats),
        ets = proplists:get_value(ets, Stats)
    },

    NewSamples = [Sample | Samples],

    % Every 10 samples (10 minutes), check for leaks
    case length(NewSamples) >= 10 of
        true ->
            check_for_memory_leak(NewSamples),
            memory_tracking_loop([Sample]);  % Keep only recent

        false ->
            memory_tracking_loop(NewSamples)
    end.

check_for_memory_leak(Samples) ->
    % Get first and last samples
    [Recent | _] = Samples,
    [Old | _] = lists:reverse(Samples),

    OldTotal = Old#memory_sample.total,
    NewTotal = Recent#memory_sample.total,
    GrowthPercent = ((NewTotal - OldTotal) / OldTotal) * 100,

    case GrowthPercent > 20.0 of  % > 20% growth
        true ->
            io:format("WARNING: Memory grew ~.1f% over 10 minutes~n", [GrowthPercent]),

            % Find which component grew
            case (Recent#memory_sample.processes > Old#memory_sample.processes) of
                true -> io:format("  → Process heaps growing~n", []);
                false -> ok
            end,

            case (Recent#memory_sample.binary > Old#memory_sample.binary) of
                true -> io:format("  → Binary data growing~n", []);
                false -> ok
            end,

            case (Recent#memory_sample.ets > Old#memory_sample.ets) of
                true -> io:format("  → ETS tables growing~n", []);
                false -> ok
            end;

        false ->
            io:format("Memory stable: ~.1f% growth over 10 minutes~n",
                [GrowthPercent])
    end.
```

### Per-Process Memory Analysis

```erlang
% Find processes using most memory
top_processes(Count) ->
    Pids = processes(),
    MemoryByPid = lists:map(fun(Pid) ->
        {memory, Memory} = process_info(Pid, memory),
        {Pid, Memory, process_info(Pid, registered_name)}
    end, Pids),

    % Sort by memory descending
    Sorted = lists:sort(fun({_P1, M1, _}, {_P2, M2, _}) ->
        M1 > M2
    end, MemoryByPid),

    % Print top N
    TopN = lists:sublist(Sorted, Count),
    io:format("Top ~p processes by memory:~n", [Count]),
    lists:foreach(fun({Pid, Memory, Name}) ->
        NameStr = case Name of
            {registered_name, RegName} -> atom_to_list(RegName);
            undefined -> "unnamed"
        end,
        io:format("  ~p: ~p MB (~s)~n",
            [Pid, Memory div 1024 div 1024, NameStr])
    end, TopN).

% Analyze process heap fragmentation
analyze_process_heap(Pid) ->
    case process_info(Pid, [memory, heap_size, total_heap_size]) of
        [{memory, M}, {heap_size, HS}, {total_heap_size, THS}] ->
            Fragmentation = ((THS - HS) / THS) * 100,
            io:format("Process ~p:~n", [Pid]),
            io:format("  Memory: ~p bytes~n", [M]),
            io:format("  Heap size: ~p bytes~n", [HS]),
            io:format("  Total heap: ~p bytes~n", [THS]),
            io:format("  Fragmentation: ~.1f%~n", [Fragmentation]);

        undefined ->
            io:format("Process ~p not found~n", [Pid])
    end.
```

### ETS Table Profiling

```erlang
% Analyze all ETS tables
ets_memory_report() ->
    Tables = ets:all(),
    io:format("ETS Tables Memory Report:~n~n", []),

    TableStats = lists:map(fun(Table) ->
        Size = ets:info(Table, size),
        Memory = ets:info(Table, memory),
        Type = ets:info(Table, type),
        Name = ets:info(Table, name),
        {Table, Name, Type, Size, Memory}
    end, Tables),

    % Sort by memory usage
    Sorted = lists:sort(fun({_T1, _N1, _Ty1, _S1, M1},
                             {_T2, _N2, _Ty2, _S2, M2}) ->
        M1 > M2
    end, TableStats),

    TotalMemory = lists:foldl(fun({_, _, _, _, M}, Acc) ->
        Acc + M
    end, 0, Sorted),

    % Print report
    lists:foreach(fun({_Table, Name, Type, Size, Memory}) ->
        Percent = (Memory / TotalMemory) * 100,
        io:format("~20s [~p]:  ~10p entries,  ~8p MB  (~.1f%)~n",
            [atom_to_list(Name), Type, Size,
             Memory div 1024 div 1024, Percent])
    end, Sorted),

    io:format("~nTotal ETS memory: ~p MB~n~n", [TotalMemory div 1024 div 1024]).

% Identify ETS table issues
audit_ets_tables() ->
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
        Size = ets:info(Table, size),
        MaxSize = ets:info(Table, max_size),
        Memory = ets:info(Table, memory),
        MemoryPerEntry = Memory / max(1, Size),

        io:format("~n~p:~n", [Table]),
        io:format("  Entries: ~p~n", [Size]),
        io:format("  Memory: ~p MB~n", [Memory div 1024 div 1024]),
        io:format("  Per-entry: ~p bytes~n", [MemoryPerEntry]),

        % Warnings
        case MaxSize of
            undefined -> ok;
            _ ->
                Utilization = (Size / MaxSize) * 100,
                case Utilization > 90 of
                    true ->
                        io:format("  ⚠️  WARNING: ~.1f% full (max: ~p)~n",
                            [Utilization, MaxSize]);
                    false ->
                        ok
                end
        end
    end, Tables).
```

---

## CPU Hotspots and eprof

### Basic eprof Usage

**eprof**: Erlang profiler measuring CPU time per function

```erlang
% Start profiling
(erlang@localhost)1> eprof:start().
profiling started at ~p

% Run operation with profiling active
(erlang@localhost)2> eprof:profile([], entitlement_governor, grant_entitlement,
    [Pid, CustomerId, #{}]).

% Stop profiling and get report
(erlang@localhost)3> eprof:stop().

% Print profiling results
(erlang@localhost)4> eprof:dump().

% Output: list of functions sorted by CPU time
```

### Profile Specific Module

```erlang
profile_module(Module) ->
    eprof:start(),

    % Profile all calls in module
    eprof:profile([], Module),

    % Run some test operations
    test_operations(),

    % Stop and analyze
    eprof:stop(),
    eprof:analyze(total).  % Total time
```

### Profile FSM State Transitions

```erlang
% Measure performance of each state handler
profile_fsm_transitions() ->
    eprof:start(),

    % Profile just the FSM module
    eprof:profile([entitlement_governor]),

    % Trigger many state transitions
    {ok, Pid} = entitlement_governor:start_link(<<"test">>, <<"ent1">>),

    % Grant entitlements (unentitled -> pending_review)
    [entitlement_governor:grant_entitlement(Pid, <<"cust_", (integer_to_binary(I))/binary>>, #{})
     || I <- lists:seq(1, 1000)],

    eprof:stop(),

    % Print results (sorted by time)
    eprof:analyze(procs).  % Per-process analysis
```

### Identify Hotspots

```erlang
find_cpu_hotspots() ->
    eprof:start(),
    eprof:profile(all),  % Profile everything

    % Run system for a bit
    long_running_test(),

    eprof:stop(),

    % Get sorted list of slowest functions
    Results = eprof:analyze(),

    % Filter to top 10
    Top10 = lists:sublist(Results, 10),

    io:format("Top CPU hotspots:~n"),
    lists:foreach(fun({_Calls, Time, {Module, Function, Arity}}) ->
        io:format("  ~p:~p/~p: ~p ms~n",
            [Module, Function, Arity, Time div 1000])
    end, Top10).
```

---

## Latency Analysis with fprof

**fprof**: Function profiler showing call traces and latency

### Basic fprof Usage

```erlang
% Start function profiler
(erlang@localhost)1> fprof:start().

% Profile specific call
(erlang@localhost)2> fprof:apply(entitlement_governor, grant_entitlement,
    [Pid, CustomerId, #{}]).

% Analyze results
(erlang@localhost)3> fprof:profile().
fprof results...

% Print detailed results
(erlang@localhost)4> fprof:analyse([{dest, user}]).  % To console
% or
(erlang@localhost)4> fprof:analyse().  % To file
```

### Measure State Transition Latency

```erlang
measure_transition_latency(Pid, Operation) ->
    fprof:start(),

    % Measure specific operation
    StartTime = erlang:system_time(microsecond),
    {ok, Result} = gen_statem:call(Pid, Operation),
    EndTime = erlang:system_time(microsecond),

    Latency = EndTime - StartTime,

    fprof:profile(),

    io:format("Operation: ~p~n", [Operation]),
    io:format("Result: ~p~n", [Result]),
    io:format("Latency: ~p microseconds (~p ms)~n",
        [Latency, Latency div 1000]),

    % Get call tree
    fprof:analyse([{dest, user}]).
```

### Identify Slow Functions

```erlang
profile_and_find_slow(Threshold) ->
    fprof:start(),

    % Run test operations
    run_benchmark_operations(),

    fprof:profile(),

    % Analyze all functions
    fprof:analyse([{sort, own}]),  % Sort by own time

    % Print functions slower than threshold
    fprof_get_results(),
    Results = ets:tab2list(fprof_data),

    SlowFunctions = [
        R || R <- Results,
             element(1, R) > Threshold  % Own time > threshold
    ],

    io:format("Functions taking > ~p microseconds:~n", [Threshold]),
    lists:foreach(fun({Time, {Module, Function, Arity}}) ->
        io:format("  ~p:~p/~p: ~p ms~n",
            [Module, Function, Arity, Time div 1000])
    end, SlowFunctions).
```

---

## Benchmarking FSM Operations

### Simple Benchmark

```erlang
-module(fsm_benchmark).

% Benchmark FSM operation performance
benchmark_grant_entitlement(Count) ->
    {ok, Pid} = entitlement_governor:start_link(
        <<"bench_tenant">>,
        <<"bench_ent1">>
    ),

    StartTime = erlang:system_time(microsecond),

    % Run Count operations
    Results = [
        begin
            gen_statem:call(Pid, {grant_entitlement,
                <<"cust_", (integer_to_binary(I))/binary>>, #{}})
        end || I <- lists:seq(1, Count)
    ],

    EndTime = erlang:system_time(microsecond),
    TotalTime = EndTime - StartTime,
    AvgTime = TotalTime / Count,

    io:format("=== Grant Entitlement Benchmark ===~n", []),
    io:format("Operations: ~p~n", [Count]),
    io:format("Total time: ~p ms~n", [TotalTime div 1000]),
    io:format("Average latency: ~p microseconds~n", [AvgTime]),
    io:format("Throughput: ~p ops/sec~n",
        [Count / (TotalTime / 1_000_000)]),

    {ok, Results}.

% Benchmark state transition performance
benchmark_state_transitions(TransitionSequence, Iterations) ->
    {ok, Pid} = entitlement_governor:start_link(
        <<"bench_tenant">>,
        <<"bench_ent2">>
    ),

    StartTime = erlang:system_time(microsecond),

    % Run transition sequence Iterations times
    [
        begin
            % Execute full transition sequence
            lists:foreach(fun(Operation) ->
                catch gen_statem:call(Pid, Operation, 5000)
            end, TransitionSequence)
        end || _ <- lists:seq(1, Iterations)
    ],

    EndTime = erlang:system_time(microsecond),
    TotalTime = EndTime - StartTime,

    TransitionsCount = length(TransitionSequence) * Iterations,
    AvgPerTransition = TotalTime / TransitionsCount,

    io:format("=== State Transition Benchmark ===~n", []),
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("Transitions per iteration: ~p~n", [length(TransitionSequence)]),
    io:format("Total transitions: ~p~n", [TransitionsCount]),
    io:format("Total time: ~p ms~n", [TotalTime div 1000]),
    io:format("Average per transition: ~p microseconds~n",
        [AvgPerTransition]).

% Benchmark ETS access performance
benchmark_ets_operations(TableName, OperationCount) ->
    ets:new(TableName, [set, public, named_table]),

    % Benchmark write
    WriteStartTime = erlang:system_time(microsecond),
    [ets:insert(TableName, {Key, <<"value_", (integer_to_binary(Key))/binary>>})
     || Key <- lists:seq(1, OperationCount)],
    WriteEndTime = erlang:system_time(microsecond),

    WriteTotalTime = WriteEndTime - WriteStartTime,
    WriteAvgTime = WriteTotalTime / OperationCount,

    % Benchmark read
    ReadStartTime = erlang:system_time(microsecond),
    [ets:lookup(TableName, Key) || Key <- lists:seq(1, OperationCount)],
    ReadEndTime = erlang:system_time(microsecond),

    ReadTotalTime = ReadEndTime - ReadStartTime,
    ReadAvgTime = ReadTotalTime / OperationCount,

    io:format("=== ETS Benchmark ===~n", []),
    io:format("Operations: ~p~n", [OperationCount]),
    io:format("Write avg: ~p microseconds (~p writes/sec)~n",
        [WriteAvgTime, OperationCount / (WriteTotalTime / 1_000_000)]),
    io:format("Read avg: ~p microseconds (~p reads/sec)~n",
        [ReadAvgTime, OperationCount / (ReadTotalTime / 1_000_000)]),

    ets:delete(TableName).
```

### Comparative Benchmarks

```erlang
% Compare two implementations
compare_implementations(OldImpl, NewImpl, TestCount) ->
    io:format("=== Comparing ~p vs ~p ===~n", [OldImpl, NewImpl]),

    % Benchmark old
    {ok, OldPid} = OldImpl:start_link(<<"old">>, <<"test1">>),
    {OldTime, OldResult} = timer:tc(fun() ->
        run_test_sequence(OldPid, TestCount)
    end),

    % Benchmark new
    {ok, NewPid} = NewImpl:start_link(<<"new">>, <<"test2">>),
    {NewTime, NewResult} = timer:tc(fun() ->
        run_test_sequence(NewPid, TestCount)
    end),

    % Print comparison
    Improvement = ((OldTime - NewTime) / OldTime) * 100,

    io:format("Old implementation: ~p microseconds (~p ms)~n",
        [OldTime, OldTime div 1000]),
    io:format("New implementation: ~p microseconds (~p ms)~n",
        [NewTime, NewTime div 1000]),

    case Improvement > 0 of
        true ->
            io:format("✓ New is ~.1f% faster~n", [Improvement]);
        false ->
            io:format("✗ New is ~.1f% slower~n", [abs(Improvement)])
    end.
```

---

## Production Performance Optimization

### Strategy 1: Caching Hot Paths

```erlang
% Cache frequently accessed data
-record(entitlement_cache, {
    id = <<>>,
    state = unentitled,
    customer_id = <<>>,
    cached_at = 0
}).

% Check cache first before expensive lookup
get_entitlement_state(EntitlementId) ->
    case ets:lookup(entitlement_cache, EntitlementId) of
        [{cache, Cache}] ->
            % Cache hit
            case erlang:system_time(millisecond) - Cache#entitlement_cache.cached_at < 5000 of
                true ->
                    % Cache is fresh
                    {ok, Cache#entitlement_cache.state};
                false ->
                    % Cache expired, fetch fresh
                    {ok, State} = fetch_from_fsm(EntitlementId),
                    update_cache(EntitlementId, State),
                    {ok, State}
            end;

        [] ->
            % Cache miss
            {ok, State} = fetch_from_fsm(EntitlementId),
            update_cache(EntitlementId, State),
            {ok, State}
    end.

update_cache(EntitlementId, State) ->
    ets:insert(entitlement_cache, {cache,
        #entitlement_cache{
            id = EntitlementId,
            state = State,
            cached_at = erlang:system_time(millisecond)
        }
    }).
```

### Strategy 2: Async Processing

```erlang
% Move slow operations outside critical path
handle_event({call, From}, {grant_entitlement, CustomerId, Metadata}, unentitled, Data) ->
    % Immediately transition state
    NewData = Data#entitlement_data{customer_id = CustomerId},
    emit_receipt(NewData, unentitled, pending_review, <<"grant_requested">>, Metadata),

    % Do expensive operations asynchronously
    spawn_link(fun() ->
        % This might take 100ms
        send_confirmation_email(CustomerId, Metadata),
        notify_external_systems(Data#entitlement_data.tenant_id, CustomerId)
    end),

    % Reply immediately to caller
    {next_state, pending_review, NewData, [{reply, From, {ok, pending_review}}]}.
```

### Strategy 3: Connection Pooling

```erlang
% Reuse connections to reduce overhead
-module(cloud_connection_pool).

create_pool(PoolSize) ->
    Connections = [
        {ok, Conn} = cloud_client:connect()
        || _ <- lists:seq(1, PoolSize)
    ],
    ets:new(connection_pool, [queue, public, named_table]),
    lists:foreach(fun(Conn) ->
        ets:insert(connection_pool, Conn)
    end, Connections).

get_connection() ->
    case ets:first(connection_pool) of
        '$end_of_table' ->
            % No available connections, create new
            {ok, NewConn} = cloud_client:connect(),
            {ok, NewConn};

        Key ->
            [{Key, Conn}] = ets:lookup(connection_pool, Key),
            ets:delete(connection_pool, Key),
            {ok, Conn}
    end.

release_connection(Conn) ->
    ets:insert(connection_pool, {make_ref(), Conn}).
```

### Strategy 4: Lazy Evaluation

```erlang
% Don't compute values until needed
-record(lazy_value, {
    thunk :: fun(() -> term()),
    cached :: term() | undefined
}).

force(LazyValue) ->
    case LazyValue#lazy_value.cached of
        undefined ->
            % Compute on first access
            Result = (LazyValue#lazy_value.thunk)(),
            LazyValue#lazy_value{cached = Result};
        Cached ->
            Cached
    end.

% Usage:
lazy_load_customer_data(CustomerId) ->
    #lazy_value{
        thunk = fun() -> fetch_customer_from_db(CustomerId) end,
        cached = undefined
    }.

% Later:
Data = lazy_load_customer_data(<<"cust_123">>),
% ... pass around without fetching ...
ActualData = force(Data),  % Only fetch when needed
```

### Performance SLOs and Monitoring

```erlang
-record(slo_target, {
    operation = grant_entitlement,
    latency_p99_ms = 50,    % 99th percentile
    latency_p95_ms = 30,
    throughput_ops_sec = 1000,
    error_rate_percent = 0.1
}).

% Track performance metrics
monitor_slo_compliance() ->
    Targets = [
        #slo_target{operation = grant_entitlement, latency_p99_ms = 50},
        #slo_target{operation = revoke_entitlement, latency_p99_ms = 50},
        #slo_target{operation = get_state, latency_p99_ms = 10}
    ],

    spawn_link(fun() -> slo_monitor_loop(Targets, []) end).

slo_monitor_loop(Targets, LatencySamples) ->
    timer:sleep(60000),  % Check every minute

    % Analyze collected latencies
    lists:foreach(fun(Target) ->
        OperationLatencies = [
            L || {Op, L} <- LatencySamples,
                 Op =:= Target#slo_target.operation
        ],

        case length(OperationLatencies) > 0 of
            true ->
                Sorted = lists:sort(OperationLatencies),
                P95Idx = erlang:max(1, length(Sorted) * 95 div 100),
                P99Idx = erlang:max(1, length(Sorted) * 99 div 100),

                P95 = lists:nth(P95Idx, Sorted),
                P99 = lists:nth(P99Idx, Sorted),

                case P99 > Target#slo_target.latency_p99_ms * 1000 of
                    true ->
                        io:format("SLO VIOLATION: ~p p99 latency ~p us (target: ~p ms)~n",
                            [Target#slo_target.operation, P99,
                             Target#slo_target.latency_p99_ms]);
                    false ->
                        ok
                end;

            false ->
                ok
        end
    end, Targets),

    slo_monitor_loop(Targets, []).

% Record latency samples
record_operation_latency(Operation, LatencyMicroseconds) ->
    % This runs in operation path, keep it fast
    ets:insert(latency_samples,
        {Operation, erlang:system_time(millisecond), LatencyMicroseconds}).
```

---

**Last Updated**: January 2026
**OTP Version**: OTP 25+
**Status**: Production-Ready
