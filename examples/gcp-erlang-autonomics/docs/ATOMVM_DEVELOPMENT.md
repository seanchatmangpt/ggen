# AtomVM Development Guide

## Table of Contents
1. [AtomVM vs BEAM Differences](#atomvm-vs-beam-differences)
2. [Memory Constraints (256MB Devices)](#memory-constraints-256mb-devices)
3. [Offline Operation and Cache Management](#offline-operation-and-cache-management)
4. [Sync with Cloud](#sync-with-cloud)
5. [Debugging Edge Issues](#debugging-edge-issues)
6. [Production Edge Patterns](#production-edge-patterns)

---

## AtomVM vs BEAM Differences

### What is AtomVM?

**AtomVM**: Lightweight Erlang/Elixir virtual machine for IoT/embedded devices

```
BEAM (Erlang on server)          AtomVM (Erlang on edge)
├─ Full OTP                      ├─ Minimal OTP
├─ 500MB+ footprint              ├─ 2-10MB footprint
├─ Full GC (multiple strategies) ├─ Simple GC
├─ All modules in preloaded      ├─ Selective modules
├─ Network clustering            ├─ Single device or bridge
└─ Production apps               └─ Lightweight governors
```

### Key Differences

| Feature | BEAM | AtomVM |
|---------|------|--------|
| **Memory footprint** | 500MB+ | 2-10MB |
| **Startup time** | 1-2s | 100-200ms |
| **Supported BIFs** | ~1000 | ~200 |
| **Hot code reload** | Yes | No |
| **Supervisor trees** | Full | Basic |
| **OTP applications** | Yes | No |
| **Module preloading** | Yes | Selective |
| **Garbage collection** | Multiple strategies | Mark-and-sweep |
| **Networking** | Full (TCP, UDP, clustering) | Basic (HTTP, CoAP) |
| **Disk I/O** | Full filesystem | Limited |
| **Concurrency** | Unlimited processes | ~1000 processes |
| **Target platforms** | Servers, clouds | IoT, ESP32, ARM |

### Unsupported BEAM Features in AtomVM

```erlang
% ❌ NOT available in AtomVM:
% - mnesia (distributed database)
% - gen_server (use pattern matching in spawn)
% - gen_statem (use simple state matching)
% - Distributed Erlang (clustering)
% - Hot code reloading
% - Many string/binary operations
% - Advanced metaprogramming

% ✅ Available in AtomVM:
% - Processes via spawn/1
% - Message passing
% - Basic supervisor (simple pattern)
% - ETS (single-node)
% - File I/O (basic)
% - Sockets (basic TCP/UDP)
% - JSON parsing
% - Cryptography (minimal)
```

---

## Memory Constraints (256MB Devices)

### Typical Device Profile

```
ESP32 (common edge device):
├─ RAM: 256-520MB
├─ Flash: 4-16MB for program
├─ AtomVM: ~3MB
├─ Available for apps: ~250MB (after OS/kernel)
└─ Typical allocation:
   ├─ Compiled BEAM modules: 1-5MB
   ├─ Process heaps: 50-100MB
   ├─ ETS tables: 50-100MB
   └─ Buffers/caches: 50MB
```

### Memory Tracking

```erlang
% Get memory statistics
memory_stats() ->
    {ok, {memory, [
        {total, Total},
        {processes, ProcessesHeap},
        {processes_used, ProcessesUsed},
        {system, SystemMemory},
        {atom, AtomMemory},
        {binary, BinaryMemory},
        {code, CodeMemory},
        {ets, ETSMemory}
    ]}} = erlang:memory(),

    io:format("Total: ~p MB~n", [Total div 1024 div 1024]),
    io:format("Processes: ~p MB (used: ~p MB)~n",
        [ProcessesHeap div 1024 div 1024,
         ProcessesUsed div 1024 div 1024]),
    io:format("System: ~p MB~n", [SystemMemory div 1024 div 1024]),
    io:format("Binary: ~p MB~n", [BinaryMemory div 1024 div 1024]),
    io:format("Code: ~p MB~n", [CodeMemory div 1024 div 1024]),
    io:format("ETS: ~p MB~n", [ETSMemory div 1024 div 1024]).
```

### Optimize for Memory

#### 1. Minimize Process Heaps

```erlang
% ❌ BAD: Large data structures in process memory
-record(entitlement, {
    id = <<>>,
    customer_id = <<>>,
    feature_key = <<>>,
    large_metadata = #{}  % Could be 100KB+
}).

% ✅ GOOD: Store large data in ETS, reference by ID
-record(entitlement, {
    id = <<>>,
    customer_id = <<>>,
    feature_key = <<>>,
    metadata_ref = undefined  % Points to ETS table
}).

store_metadata(EntitlementId, Metadata) ->
    ets:insert(metadata_cache, {EntitlementId, Metadata}).

get_metadata(EntitlementId) ->
    case ets:lookup(metadata_cache, EntitlementId) of
        [{_, Metadata}] -> {ok, Metadata};
        [] -> {error, not_found}
    end.
```

#### 2. Use Binary Instead of String

```erlang
% ❌ BAD: String uses more memory (list of chars)
Reason = "entitlement_suspended_due_to_fraud_investigation",  % ~50 bytes

% ✅ GOOD: Binary is memory-efficient
Reason = <<"entitlement_suspended_due_to_fraud_investigation">>,  % ~25 bytes

% String conversion only when needed (display/logging)
format_reason(BinaryReason) ->
    binary_to_list(BinaryReason).
```

#### 3. Cache Size Limiting

```erlang
-define(MAX_RECEIPT_CACHE, 1000).
-define(MAX_RECEIPT_SIZE_BYTES, 50_000_000).  % 50MB max

init_receipt_table(TenantId) ->
    TableName = receipt_cache_table(TenantId),
    ets:new(TableName, [set, public, named_table]),
    % Limit table growth
    ets:setopts(TableName, [{max_size, ?MAX_RECEIPT_SIZE_BYTES}]),
    TableName.

% When cache gets full, evict oldest
manage_receipt_cache(TableName) ->
    Size = ets:info(TableName, size),
    case Size > ?MAX_RECEIPT_CACHE of
        true ->
            % Delete oldest 10% of receipts
            DeleteCount = Size div 10,
            delete_oldest_receipts(TableName, DeleteCount);
        false ->
            ok
    end.

delete_oldest_receipts(TableName, Count) ->
    % Get all receipts sorted by timestamp
    AllReceipts = ets:match_object(TableName, {'$1', '_'}),
    Sorted = lists:sort(fun({_ID1, TS1}, {_ID2, TS2}) ->
        TS1 < TS2  % Oldest first
    end, AllReceipts),

    % Delete oldest Count items
    ToDelete = lists:sublist(Sorted, Count),
    lists:foreach(fun({ID, _}) ->
        ets:delete(TableName, ID)
    end, ToDelete).
```

#### 4. Compact Data Structures

```erlang
% ❌ BAD: Verbose state record
-record(entitlement_state, {
    tenant_id = <<>>,
    entitlement_id = <<>>,
    customer_id = <<>>,
    feature_key = <<>>,
    created_at = 0,
    expires_at = infinity,
    state = unentitled,
    receipt_table = undefined,
    last_transition = 0
}).

% ✅ GOOD: Compact representation (use tuple)
% Instead of record, use:
% {entitlement, TenantId, EntitlementId, CustomerId, FeatureKey,
%  CreatedAt, ExpiresAt, State, ReceiptTable, LastTransition}

% Or use tuple with selective fields only needed in memory
-record(entitlement_state_compact, {
    id = <<>>,           % {tenant_id, entitlement_id}
    customer_id = <<>>,  % Reference only, full data in ETS
    state = unentitled,
    receipt_table = undefined
}).
```

#### 5. Garbage Collection Tuning

```erlang
% For AtomVM, GC is simple but can be triggered manually
collect_garbage() ->
    % Force garbage collection
    erlang:garbage_collect(),
    io:format("Garbage collection triggered~n", []).

% Monitor heap size
check_process_memory(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Memory} ->
            io:format("Process ~p uses ~p bytes~n", [Pid, Memory]);
        undefined ->
            io:format("Process ~p is dead~n", [Pid])
    end.
```

---

## Offline Operation and Cache Management

### Device Offline State Management

```erlang
-module(offline_governor).

% Simple state machine for offline operation
-record(offline_state, {
    device_id = <<>>,
    is_online = false,
    offline_queue = [],      % Queue of pending operations
    cache = #{},             % Local cache of cloud state
    last_sync_time = 0,
    next_sync_time = 0
}).

% Spawn offline governor
start_link(DeviceId) ->
    spawn_link(fun() -> init_offline(DeviceId) end).

init_offline(DeviceId) ->
    State = #offline_state{
        device_id = DeviceId,
        is_online = check_connectivity(),
        cache = load_local_cache(DeviceId)
    },
    loop(State).

% Main loop
loop(State) ->
    case State#offline_state.is_online of
        true ->
            % Online: process immediately
            loop_online(State);
        false ->
            % Offline: queue locally
            loop_offline(State)
    end.

% Online mode: process immediately + sync queue
loop_online(State) ->
    ReceiveTimeout = 100,  % Check connectivity periodically
    receive
        {operation, Operation} ->
            % Process immediately
            {ok, Result} = execute_operation(Operation, State#offline_state.cache),
            % Also queue for sync
            NewQueue = State#offline_state.offline_queue ++
                [{operation, Operation, Result, timestamp()}],
            loop(State#offline_state{offline_queue = NewQueue});

        {sync, _} ->
            % Sync all pending operations to cloud
            case sync_all_to_cloud(State#offline_state.offline_queue) of
                {ok, _} ->
                    % Clear queue after sync
                    NewState = State#offline_state{
                        offline_queue = [],
                        last_sync_time = timestamp(),
                        next_sync_time = timestamp() + 300000  % 5min
                    },
                    loop(NewState);
                {error, _} ->
                    % Retry sync later
                    loop(State#offline_state{
                        next_sync_time = timestamp() + 30000  % 30s
                    })
            end;

        {check_connectivity, _} ->
            case check_connectivity() of
                true ->
                    % Still online
                    loop(State);
                false ->
                    % Lost connectivity
                    loop(State#offline_state{is_online = false})
            end
    after ReceiveTimeout ->
        % Check connectivity periodically
        IsOnline = check_connectivity(),
        NewState = State#offline_state{is_online = IsOnline},

        % Auto-trigger sync if online and queue has items
        case {IsOnline, NewState#offline_state.offline_queue} of
            {true, [_|_]} ->
                % Has pending items, sync soon
                case erlang:system_time(millisecond) >= NewState#offline_state.next_sync_time of
                    true ->
                        self() ! {sync, auto},
                        loop(NewState);
                    false ->
                        loop(NewState)
                end;
            _ ->
                loop(NewState)
        end
    end.

% Offline mode: only queue locally
loop_offline(State) ->
    ReceiveTimeout = 5000,  % Check connectivity less frequently
    receive
        {operation, Operation} ->
            % Queue for later
            NewQueue = State#offline_state.offline_queue ++
                [{operation, Operation, pending, timestamp()}],
            % Update cache locally
            {ok, NewCache} = update_cache(Operation, State#offline_state.cache),
            loop(State#offline_state{
                offline_queue = NewQueue,
                cache = NewCache
            });

        {check_connectivity, _} ->
            case check_connectivity() of
                true ->
                    % Regained connectivity
                    self() ! {sync, auto},
                    loop(State#offline_state{is_online = true});
                false ->
                    loop(State)
            end
    after ReceiveTimeout ->
        % Periodically check if we're back online
        case check_connectivity() of
            true ->
                self() ! {sync, auto},
                loop(State#offline_state{is_online = true});
            false ->
                loop(State)
        end
    end.

% Sync all pending operations to cloud
sync_all_to_cloud(Queue) ->
    case httpc:request(post,
        {"https://cloud.example.com/sync", [],
         "application/json",
         json:encode(Queue)},
        [{connect_timeout, 5000}],
        [{body_format, binary}]) of

        {ok, {Status, _Headers, Body}} when Status >= 200, Status < 300 ->
            {ok, json:decode(Body)};

        {ok, {Status, _Headers, _}} ->
            {error, {http_error, Status}};

        {error, Reason} ->
            {error, Reason}
    end.

% Load cache from local storage
load_local_cache(DeviceId) ->
    CacheFile = "/data/cache_" ++ binary_to_list(DeviceId) ++ ".json",
    case file:read_file(CacheFile) of
        {ok, Data} ->
            json:decode(Data);
        {error, enoent} ->
            #{}  % Empty cache
    end.

% Save cache to local storage
save_local_cache(DeviceId, Cache) ->
    CacheFile = "/data/cache_" ++ binary_to_list(DeviceId) ++ ".json",
    file:write_file(CacheFile, json:encode(Cache)).

check_connectivity() ->
    % Ping a known IP (e.g., Google DNS)
    case httpc:request(get,
        {"http://8.8.8.8", []},
        [{connect_timeout, 2000}],
        [{body_format, binary}]) of

        {ok, _} -> true;
        {error, _} -> false
    end.

execute_operation(Operation, Cache) ->
    % Execute operation against local cache
    % Return result immediately
    {ok, cache_operation(Operation, Cache)}.

update_cache(Operation, Cache) ->
    % Update local cache based on operation
    {ok, cache_operation(Operation, Cache)}.

cache_operation({grant_entitlement, EntId, CustomerId}, Cache) ->
    Cache#{EntId => #{customer_id => CustomerId, state => pending}};
cache_operation({revoke_entitlement, EntId}, Cache) ->
    maps:update_with(EntId, fun(V) -> V#{state => revoked} end, Cache);
cache_operation(_, Cache) ->
    Cache.

timestamp() ->
    erlang:system_time(millisecond).
```

### Conflict Resolution During Sync

```erlang
resolve_conflicts(LocalChange, CloudVersion) ->
    % Last-write-wins strategy (most common)
    LocalTimestamp = maps:get(timestamp, LocalChange),
    CloudTimestamp = maps:get(timestamp, CloudVersion),

    case LocalTimestamp > CloudTimestamp of
        true ->
            % Local is newer - keep local
            {local, LocalChange};
        false ->
            % Cloud is newer - use cloud
            {cloud, CloudVersion}
    end.

% Or timestamp-free strategy: assume cloud is source of truth
resolve_conflicts_cloud_wins(_LocalChange, CloudVersion) ->
    {cloud, CloudVersion}.

% Or custom business logic
resolve_conflicts_custom(LocalChange, CloudVersion, Domain) ->
    case {Domain, maps:get(entitlement_state, LocalChange),
          maps:get(entitlement_state, CloudVersion)} of

        % If cloud is archived and local is still active,
        % respect cloud's archival
        {entitlement, _LocalState, archived} ->
            {cloud, CloudVersion};

        % If local has billing but cloud doesn't,
        % keep local (higher authority)
        {billing, billing_processed, _CloudState} ->
            {local, LocalChange};

        % Default: last-write-wins
        {_, _, _} ->
            resolve_conflicts(LocalChange, CloudVersion)
    end.
```

---

## Sync with Cloud

### Bidirectional Sync Protocol

```erlang
-module(cloud_sync).

% Initiate sync with cloud
sync_with_cloud(DeviceId) ->
    % 1. Get local version hash
    LocalHash = compute_local_hash(),

    % 2. Get cloud version hash
    {ok, CloudHash} = fetch_cloud_hash(DeviceId),

    % 3. Compare
    case LocalHash =:= CloudHash of
        true ->
            % In sync
            {ok, synced};

        false ->
            % Out of sync - get diffs
            {ok, CloudDiff} = fetch_cloud_diff(DeviceId, LocalHash),
            LocalDiff = compute_local_diff(CloudHash),

            % 4. Merge diffs
            {ok, MergedState} = merge_diffs(LocalDiff, CloudDiff),

            % 5. Persist merged state
            persist_merged_state(MergedState),

            % 6. Confirm sync
            confirm_sync(DeviceId),

            {ok, synced}
    end.

% Compute hash of local state (receipts, entitlements, etc.)
compute_local_hash() ->
    % Hash all receipts and state from local storage
    Receipts = load_all_receipts(),
    EntitlementStates = load_all_entitlements(),

    StateBlob = term_to_binary({Receipts, EntitlementStates}),
    crypto:hash(sha256, StateBlob).

% Fetch diff from cloud
fetch_cloud_diff(DeviceId, LocalHash) ->
    QueryParams = <<"?local_hash=", (base64:encode(LocalHash))/binary>>,
    Url = <<"https://cloud/device/", DeviceId/binary, "/diff", QueryParams/binary>>,

    case httpc:request(get, {Url, []}, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {200, _Headers, Body}} ->
            {ok, json:decode(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

% Push local changes to cloud
push_local_changes(DeviceId, Changes) ->
    Url = "https://cloud/device/" ++ binary_to_list(DeviceId) ++ "/changes",
    Body = json:encode(Changes),

    case httpc:request(post,
        {Url, [],
         "application/json",
         Body},
        [{timeout, 5000}],
        [{body_format, binary}]) of

        {ok, {200, _Headers, Response}} ->
            {ok, json:decode(Response)};
        {ok, {409, _Headers, _}} ->
            % Conflict detected
            {error, conflict};
        {error, Reason} ->
            {error, Reason}
    end.

% Merge cloud diffs with local changes
merge_diffs(LocalDiff, CloudDiff) ->
    % Group by entity
    LocalByEntity = group_by_entity(LocalDiff),
    CloudByEntity = group_by_entity(CloudDiff),

    % For each entity, resolve conflicts
    MergedEntities = maps:fold(fun(EntityId, LocalChange, Acc) ->
        CloudChange = maps:get(EntityId, CloudByEntity, undefined),
        case CloudChange of
            undefined ->
                % Only in local - keep it
                Acc#{EntityId => LocalChange};
            _ ->
                % In both - resolve conflict
                {WinnerSource, WinnerState} = resolve_conflict(
                    LocalChange, CloudChange),
                io:format("Conflict on ~p: ~p wins~n",
                    [EntityId, WinnerSource]),
                Acc#{EntityId => WinnerState}
        end
    end, #{}, LocalByEntity),

    % Add cloud-only entities
    FinalMerged = maps:fold(fun(EntityId, CloudChange, Acc) ->
        case maps:is_key(EntityId, MergedEntities) of
            true -> Acc;  % Already processed
            false -> Acc#{EntityId => CloudChange}  % Cloud-only, add it
        end
    end, MergedEntities, CloudByEntity),

    {ok, FinalMerged}.

group_by_entity(Changes) ->
    % Changes = [{entitlement_id, #{...}}, ...]
    lists:foldl(fun({EntityId, State}, Acc) ->
        Acc#{EntityId => State}
    end, #{}, Changes).

resolve_conflict(LocalChange, CloudChange) ->
    LocalTS = maps:get(timestamp, LocalChange, 0),
    CloudTS = maps:get(timestamp, CloudChange, 0),

    case LocalTS > CloudTS of
        true -> {local, LocalChange};
        false -> {cloud, CloudChange}
    end.

persist_merged_state(MergedState) ->
    % Write all merged entities back to local storage
    maps:foreach(fun(EntityId, EntityState) ->
        store_entity(EntityId, EntityState)
    end, MergedState).

confirm_sync(DeviceId) ->
    % Tell cloud we've synced
    Url = "https://cloud/device/" ++ binary_to_list(DeviceId) ++ "/sync-confirmed",
    httpc:request(post, {Url, [], "application/json", "{}"}, [], []).
```

### Incremental Sync (for bandwidth-constrained devices)

```erlang
% Instead of full state sync, only sync changes
incremental_sync(DeviceId) ->
    % 1. Get timestamp of last sync
    LastSyncTime = get_last_sync_timestamp(DeviceId),

    % 2. Get all changes since then
    LocalChanges = get_changes_since(LastSyncTime),

    % 3. Send changes to cloud
    case push_changes_to_cloud(DeviceId, LocalChanges) of
        {ok, CloudChanges} ->
            % 4. Apply cloud changes locally
            lists:foreach(fun(Change) ->
                apply_cloud_change(Change)
            end, CloudChanges),

            % 5. Update sync timestamp
            update_last_sync_timestamp(DeviceId, timestamp()),
            {ok, synced};

        {error, conflict} ->
            % Full sync needed on conflict
            sync_with_cloud(DeviceId)
    end.

get_changes_since(LastSyncTime) ->
    % Query local receipt log for changes after timestamp
    Query = "SELECT * FROM receipt_log WHERE timestamp > ?",
    case sql_query(Query, [LastSyncTime]) of
        {ok, Rows} -> Rows;
        {error, _} -> []
    end.

push_changes_to_cloud(DeviceId, Changes) ->
    % Compress for bandwidth
    Compressed = zlib:compress(term_to_binary(Changes)),
    Size = byte_size(Compressed),

    io:format("Syncing ~p bytes (~p changes)~n", [Size, length(Changes)]),

    % Push to cloud
    Url = "https://cloud/device/" ++ binary_to_list(DeviceId) ++ "/changes",
    httpc:request(post,
        {Url, [{"Content-Encoding", "gzip"}],
         "application/octet-stream",
         Compressed},
        [{timeout, 30000}],
        [{body_format, binary}]).
```

---

## Debugging Edge Issues

### Remote Observation

```erlang
% Connect to AtomVM device via Erlang distribution (if available)
% Otherwise use logging to observe behavior

% 1. Enable comprehensive logging
-define(LOG_FILE, "/data/atomvm.log").

log(Level, Format, Args) ->
    Timestamp = timestamp_string(),
    Message = io_lib:format(Format, Args),
    LogLine = io_lib:format("~s [~p] ~s~n", [Timestamp, Level, Message]),

    % Write to file
    file:write_file(?LOG_FILE, LogLine, [append]),

    % Also print to console if debug mode
    case get(debug_mode) of
        true -> io:format(LogLine);
        _ -> ok
    end.

% Usage in FSM
handle_operation(Operation, State, Data) ->
    log(info, "Processing operation: ~p in state: ~p~n",
        [Operation, State]),

    case process_operation(Operation, State, Data) of
        {ok, NewState} ->
            log(info, "Operation succeeded, transitioning to: ~p~n",
                [NewState]),
            {ok, NewState};

        {error, Reason} ->
            log(error, "Operation failed: ~p~n", [Reason]),
            {error, Reason}
    end.
```

### Memory Debugging

```erlang
% Monitor memory usage over time
-module(memory_monitor).

% Log memory stats every 60 seconds
monitor_memory() ->
    Timer = erlang:send_interval(60000, self(), {log_memory}),
    monitor_loop(Timer).

monitor_loop(TimerId) ->
    receive
        {log_memory} ->
            {ok, {memory, Stats}} = erlang:memory(),
            Total = proplists:get_value(total, Stats),
            Processes = proplists:get_value(processes, Stats),
            System = proplists:get_value(system, Stats),
            Binary = proplists:get_value(binary, Stats),

            log(info,
                "Memory - Total: ~pMB, Processes: ~pMB, System: ~pMB, Binary: ~pMB~n",
                [Total div 1024 div 1024,
                 Processes div 1024 div 1024,
                 System div 1024 div 1024,
                 Binary div 1024 div 1024]),

            % Check for memory leaks (memory growing)
            check_memory_leak(Total),

            monitor_loop(TimerId);

        stop ->
            erlang:cancel_timer(TimerId)
    end.

check_memory_leak(CurrentMemory) ->
    case get(previous_memory) of
        undefined ->
            put(previous_memory, CurrentMemory);

        PrevMemory ->
            GrowthPercent = ((CurrentMemory - PrevMemory) / PrevMemory) * 100,

            case GrowthPercent > 10.0 of  % > 10% growth
                true ->
                    log(warning,
                        "Possible memory leak: grew by ~.1f%~n",
                        [GrowthPercent]),
                    trigger_gc();
                false ->
                    ok
            end,

            put(previous_memory, CurrentMemory)
    end.

trigger_gc() ->
    erlang:garbage_collect(),
    log(info, "Garbage collection triggered~n", []).
```

### Battery Usage Optimization

```erlang
% Reduce power consumption on battery devices
optimize_for_battery() ->
    % 1. Reduce polling frequency
    put(poll_interval, 30000),  % 30s instead of 5s

    % 2. Batch operations
    put(batch_operations, true),

    % 3. Reduce logging detail
    put(log_level, warning),  % Only warnings and errors

    % 4. Disable non-essential features
    put(enable_metrics, false),
    put(enable_detailed_tracing, false).

% Increase sync interval to save bandwidth
set_sync_interval_battery(DeviceId) ->
    % On battery: sync every 5 minutes instead of 1 minute
    SyncInterval = 300000,  % 5 minutes
    update_sync_schedule(DeviceId, SyncInterval).
```

### Common AtomVM Issues

#### 1. Out of Memory (OOM)

```erlang
% Symptom: Process crashes or system freezes

% Root causes:
% - Large ETS tables
% - Unbound queue growth
% - Memory leaks in C code (BIFs)

% Debug:
ets:all(),  % List all tables
ets:info(TableName),  % Check size of specific table

% Fix:
% - Implement cache eviction (FIFO or LRU)
% - Limit message queue length
% - Monitor growth with memory_monitor
```

#### 2. Watchdog Timer Resets

```erlang
% Symptom: Device resets every N seconds

% Possible causes:
% - Infinite loop blocking event loop
% - Large blocking file I/O
% - Crashed process not restarted

% Debug:
% Add timestamps to log to find when reset occurs

% Fix:
% - Break up long operations into smaller chunks
% - Use non-blocking I/O where possible
% - Ensure supervisor restarts crashed processes
```

#### 3. Network Timeouts

```erlang
% Symptom: Cloud sync fails frequently

% Debug:
log_network_stats() ->
    % Log when send/recv fails
    log(info, "Network send: ~p bytes~n", [SentBytes]),
    log(info, "Network recv: ~p bytes~n", [RecvBytes]),
    log(info, "Network errors: ~p~n", [ErrorCount]).

% Fix:
% - Increase timeout values for slow networks
% - Implement retry with exponential backoff
% - Use incremental sync (smaller packets)
% - Compress data before sending
```

---

## Production Edge Patterns

### Pattern 1: Self-Healing Device

```erlang
-module(self_healing_device).

% Device that detects and recovers from common failures

-record(device_state, {
    device_id = <<>>,
    health_status = healthy,
    last_crash_time = 0,
    crash_count = 0,
    recovery_attempts = 0
}).

start_link(DeviceId) ->
    spawn_link(fun() -> init_device(DeviceId) end).

init_device(DeviceId) ->
    State = #device_state{device_id = DeviceId},
    % Start health monitor
    spawn_link(fun() -> health_monitor_loop(DeviceId) end),
    % Start auto-recovery
    spawn_link(fun() -> recovery_loop(State) end),
    loop(State).

health_monitor_loop(DeviceId) ->
    % Check every 10 seconds
    timer:sleep(10000),

    % Check critical services
    Services = [cloud_sync, entitlement_governor, billing_governor],
    DownServices = check_services(Services),

    case DownServices of
        [] ->
            % All healthy
            health_monitor_loop(DeviceId);

        _ ->
            % Some services down
            log(warning, "Services down: ~p~n", [DownServices]),
            self() ! {trigger_recovery, DownServices},
            health_monitor_loop(DeviceId)
    end.

recovery_loop(State) ->
    receive
        {trigger_recovery, DownServices} ->
            log(info, "Initiating recovery for: ~p~n", [DownServices]),

            % Try recovery
            case attempt_recovery(DownServices, State) of
                {ok, NewState} ->
                    log(info, "Recovery successful~n", []),
                    recovery_loop(NewState);

                {error, _} ->
                    % Recovery failed - increment counter
                    NewState = State#device_state{
                        recovery_attempts = State#device_state.recovery_attempts + 1
                    },

                    case NewState#device_state.recovery_attempts > 3 of
                        true ->
                            % Give up and reset device
                            log(critical, "Recovery failed 3 times, performing hard reset~n", []),
                            perform_hard_reset();
                        false ->
                            % Retry later
                            timer:sleep(30000),
                            recovery_loop(NewState)
                    end
            end
    end.

check_services(Services) ->
    % Check if each service is responding
    lists:filter(fun(Service) ->
        case is_service_alive(Service) of
            true -> false;  % Service alive, not in list
            false -> true   % Service dead, include in list
        end
    end, Services).

attempt_recovery(Services, State) ->
    % 1. Try restart
    case restart_services(Services) of
        {ok, _} -> {ok, State};

        {error, _} ->
            % 2. Try clear cache
            case clear_device_cache() of
                ok ->
                    % Restart again
                    restart_services(Services);

                {error, _} ->
                    % 3. Try reload config
                    case reload_device_config() of
                        ok ->
                            restart_services(Services);

                        {error, E} ->
                            {error, E}
                    end
            end
    end.

perform_hard_reset() ->
    log(critical, "Performing hard reset~n", []),
    % Clear all local state
    file:delete("/data/cache.json"),
    file:delete("/data/state.json"),
    % Restart
    erlang:halt().
```

### Pattern 2: Battery-Aware Operation

```erlang
-module(battery_aware_governor).

% Adapt behavior based on battery level

-record(battery_state, {
    device_id = <<>>,
    battery_percent = 100,
    is_charging = false,
    power_mode = normal  % normal | low_power | critical
}).

init_battery_monitor(DeviceId) ->
    spawn_link(fun() ->
        check_battery_loop(#battery_state{device_id = DeviceId})
    end).

check_battery_loop(State) ->
    % Check battery every 60 seconds
    timer:sleep(60000),

    BatteryPercent = read_battery_percent(),
    IsCharging = read_charging_status(),

    NewMode = case {BatteryPercent, IsCharging} of
        (_, true) -> normal;           % Charging
        (P, _) when P > 50 -> normal;  % > 50%
        (P, _) when P > 20 -> low_power; % 20-50%
        (P, _) when P > 5  -> critical;  % 5-20%
        _                  -> shutdown    % Critical low
    end,

    case {NewMode, State#battery_state.power_mode} of
        {NewMode, NewMode} ->
            % No change
            check_battery_loop(State#battery_state{battery_percent = BatteryPercent});

        {shutdown, _} ->
            % Critical - shut down gracefully
            log(critical, "Battery critical (~p%), shutting down~n",
                [BatteryPercent]),
            shutdown_gracefully();

        {critical, _} ->
            % Only sync every 10 minutes, no polling
            log(warning, "Battery critical (~p%), switching to critical mode~n",
                [BatteryPercent]),
            apply_power_mode(critical),
            check_battery_loop(State#battery_state{
                battery_percent = BatteryPercent,
                power_mode = critical
            });

        {low_power, _} ->
            % Reduce polling, longer sync intervals
            log(warning, "Battery low (~p%), switching to low power mode~n",
                [BatteryPercent]),
            apply_power_mode(low_power),
            check_battery_loop(State#battery_state{
                battery_percent = BatteryPercent,
                power_mode = low_power
            });

        {normal, _} ->
            % Back to normal
            log(info, "Battery level OK (~p%), normal mode~n",
                [BatteryPercent]),
            apply_power_mode(normal),
            check_battery_loop(State#battery_state{
                battery_percent = BatteryPercent,
                power_mode = normal,
                is_charging = IsCharging
            })
    end.

apply_power_mode(normal) ->
    put(poll_interval, 5000),      % Poll every 5s
    put(sync_interval, 60000),     % Sync every 1min
    put(metrics_enabled, true);

apply_power_mode(low_power) ->
    put(poll_interval, 30000),     % Poll every 30s
    put(sync_interval, 300000),    % Sync every 5min
    put(metrics_enabled, false);

apply_power_mode(critical) ->
    put(poll_interval, 120000),    % Poll every 2min
    put(sync_interval, 600000),    % Sync every 10min
    put(metrics_enabled, false);

apply_power_mode(_) ->
    ok.

shutdown_gracefully() ->
    % Give time to sync critical data
    case sync_critical_data(5000) of
        {ok, _} -> log(info, "Critical data synced, shutting down~n", []);
        {error, _} -> log(warning, "Sync timeout, shutting down anyway~n", [])
    end,
    erlang:halt().

sync_critical_data(TimeoutMs) ->
    % Push unsync'd receipts and entitlement changes
    Deadline = erlang:system_time(millisecond) + TimeoutMs,

    case push_all_pending_changes(Deadline) of
        {ok, _} -> {ok, synced};
        {error, E} -> {error, E}
    end.
```

---

**Last Updated**: January 2026
**AtomVM Version**: 0.6.0+
**Status**: Production-Ready
