%%%-------------------------------------------------------------------
%%% @doc
%%% Controlled Fault Injection
%%%
%%% Injects specific, reproducible faults for testing error handling:
%%% - Database timeouts
%%% - Network errors
%%% - Disk failures
%%% - Resource exhaustion
%%% - Invalid inputs
%%%
%%% More controlled than chaos_monkey - good for regression testing.
%%% @end
%%%-------------------------------------------------------------------
-module(fault_injection).

-export([
    inject/2,
    inject_with_verification/3,
    verify_recovery/2,
    get_fault_types/0
]).

-record(fault_result, {
    fault_type :: atom(),
    injection_status :: ok | {error, term()},
    recovery_status :: ok | {error, term()},
    recovery_time_ms :: non_neg_integer()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Inject specific fault
inject(FaultType, TargetOrConfig) ->
    io:format("~n=== Injecting Fault: ~p ===~n", [FaultType]),

    StartTime = erlang:monotonic_time(millisecond),
    Result = do_inject(FaultType, TargetOrConfig),
    EndTime = erlang:monotonic_time(millisecond),

    Duration = EndTime - StartTime,

    case Result of
        {ok, Details} ->
            io:format("✅ Fault injected successfully in ~pms: ~p~n", [Duration, Details]);
        {error, Reason} ->
            io:format("❌ Fault injection failed: ~p~n", [Reason])
    end,

    Result.

%% @doc Inject fault and verify recovery
inject_with_verification(FaultType, Target, RecoveryTimeoutMs) ->
    io:format("~n=== Fault Injection with Verification ===~n"),
    io:format("Fault Type: ~p~n", [FaultType]),
    io:format("Recovery Timeout: ~pms~n~n", [RecoveryTimeoutMs]),

    %% Inject fault
    InjectionStart = erlang:monotonic_time(millisecond),
    InjectionResult = do_inject(FaultType, Target),
    InjectionEnd = erlang:monotonic_time(millisecond),

    case InjectionResult of
        {ok, _Details} ->
            io:format("✅ Fault injected in ~pms~n", [InjectionEnd - InjectionStart]),

            %% Verify recovery
            RecoveryStart = erlang:monotonic_time(millisecond),
            RecoveryResult = verify_recovery(FaultType, RecoveryTimeoutMs),
            RecoveryEnd = erlang:monotonic_time(millisecond),

            RecoveryTime = RecoveryEnd - RecoveryStart,

            FaultResult = #fault_result{
                fault_type = FaultType,
                injection_status = ok,
                recovery_status = RecoveryResult,
                recovery_time_ms = RecoveryTime
            },

            report_fault_result(FaultResult),
            {ok, FaultResult};
        {error, Reason} ->
            io:format("❌ Fault injection failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Verify system recovered from fault
verify_recovery(FaultType, TimeoutMs) ->
    io:format("~nVerifying recovery from ~p...~n", [FaultType]),

    %% Wait for recovery
    timer:sleep(min(TimeoutMs, 5000)),

    %% Check system health
    Health = check_system_health(),

    case Health of
        healthy ->
            io:format("✅ System recovered successfully~n"),
            ok;
        {unhealthy, Reason} ->
            io:format("❌ System NOT recovered: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Get available fault types
get_fault_types() ->
    [
        supervisor_crash,
        worker_crash,
        database_timeout,
        network_error,
        disk_full,
        memory_exhausted,
        port_exhaustion,
        deadlock,
        message_queue_overflow,
        invalid_input,
        corrupt_state
    ].

%%%===================================================================
%%% Fault Injection Implementations
%%%===================================================================

do_inject(supervisor_crash, SupName) ->
    io:format("Injecting supervisor crash for: ~p~n", [SupName]),

    case whereis(SupName) of
        undefined ->
            {error, supervisor_not_found};
        Pid ->
            OldPid = Pid,
            exit(Pid, kill),

            %% Wait for restart
            timer:sleep(100),

            case whereis(SupName) of
                undefined ->
                    {error, not_restarted};
                NewPid when NewPid =/= OldPid ->
                    {ok, #{old_pid => OldPid, new_pid => NewPid}};
                _ ->
                    {error, same_pid}
            end
    end;

do_inject(worker_crash, WorkerSpec) ->
    SupName = maps:get(supervisor, WorkerSpec, call_router_sup),
    WorkerIndex = maps:get(index, WorkerSpec, 1),

    io:format("Injecting worker crash: supervisor=~p, index=~p~n", [SupName, WorkerIndex]),

    Workers = supervisor:which_children(SupName),
    case length(Workers) >= WorkerIndex of
        true ->
            {_Id, Pid, _Type, _Modules} = lists:nth(WorkerIndex, Workers),
            exit(Pid, kill),

            %% Verify restart
            timer:sleep(100),
            NewWorkers = supervisor:which_children(SupName),

            case length(NewWorkers) >= WorkerIndex of
                true ->
                    {ok, #{workers_before => length(Workers), workers_after => length(NewWorkers)}};
                false ->
                    {error, worker_not_restarted}
            end;
        false ->
            {error, worker_index_out_of_range}
    end;

do_inject(database_timeout, _Config) ->
    io:format("Injecting database timeout...~n"),

    %% Note: In production, you'd use meck or similar to mock db calls
    %% For demonstration, we'll simulate a slow operation

    spawn(fun() ->
        %% Simulate slow database query
        timer:sleep(10000)
    end),

    {ok, #{timeout_ms => 10000}};

do_inject(network_error, _Config) ->
    io:format("Injecting network error...~n"),

    %% Simulate network failure by disconnecting from random node
    case nodes() of
        [] ->
            {error, no_cluster};
        Nodes ->
            TargetNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
            erlang:disconnect_node(TargetNode),
            {ok, #{disconnected_node => TargetNode}}
    end;

do_inject(disk_full, _Config) ->
    io:format("Injecting disk full error...~n"),

    %% Note: In production, use meck to mock file operations
    %% For demonstration:

    TestFile = "/tmp/disk_full_test_" ++ integer_to_list(erlang:unique_integer([positive])),

    %% Try to write large file
    try
        {ok, FD} = file:open(TestFile, [write, binary]),
        %% Write 1MB
        ok = file:write(FD, crypto:strong_rand_bytes(1024 * 1024)),
        file:close(FD),
        file:delete(TestFile),
        {ok, #{test_file => TestFile}}
    catch
        _:Reason ->
            {error, Reason}
    end;

do_inject(memory_exhausted, Config) ->
    SizeMB = maps:get(size_mb, Config, 500),

    io:format("Injecting memory exhaustion: ~pMB~n", [SizeMB]),

    try
        %% Allocate large amount of memory
        _BigData = [crypto:strong_rand_bytes(1024 * 1024) || _ <- lists:seq(1, SizeMB)],

        MemInfo = erlang:memory(),
        TotalMem = proplists:get_value(total, MemInfo),

        {ok, #{allocated_mb => SizeMB, total_memory_mb => TotalMem div 1024 div 1024}}
    catch
        _:Reason ->
            {error, Reason}
    end;

do_inject(port_exhaustion, _Config) ->
    io:format("Injecting port exhaustion...~n"),

    %% Open many ports
    try
        Ports = [erlang:open_port({spawn, "cat"}, []) || _ <- lists:seq(1, 100)],

        %% Close them
        [erlang:port_close(P) || P <- Ports],

        {ok, #{ports_opened => length(Ports)}}
    catch
        _:Reason ->
            {error, Reason}
    end;

do_inject(message_queue_overflow, Config) ->
    TargetPid = maps:get(target_pid, Config, whereis(call_router_server)),
    NumMessages = maps:get(num_messages, Config, 100000),

    io:format("Injecting message queue overflow: ~p messages to ~p~n", [NumMessages, TargetPid]),

    case is_process_alive(TargetPid) of
        true ->
            %% Flood message queue
            [TargetPid ! {overflow_test, N} || N <- lists:seq(1, NumMessages)],

            %% Check queue length
            {message_queue_len, QueueLen} = process_info(TargetPid, message_queue_len),

            {ok, #{messages_sent => NumMessages, queue_length => QueueLen}};
        false ->
            {error, process_not_alive}
    end;

do_inject(invalid_input, _Config) ->
    io:format("Injecting invalid input...~n"),

    %% Send various invalid inputs
    InvalidInputs = [
        undefined,
        <<>>,
        "",
        #{},
        [],
        {invalid, tuple},
        self(),
        make_ref()
    ],

    Results = [test_invalid_input(Input) || Input <- InvalidInputs],

    NumRejected = length([R || {ok, rejected} <- Results]),
    NumAccepted = length([R || {ok, accepted} <- Results]),

    case NumAccepted of
        0 ->
            {ok, #{total_tests => length(InvalidInputs), rejected => NumRejected}};
        _ ->
            {error, #{accepted_invalid_input => NumAccepted}}
    end;

do_inject(corrupt_state, Config) ->
    TargetPid = maps:get(target_pid, Config, whereis(call_router_server)),

    io:format("Injecting corrupt state to ~p...~n", [TargetPid]),

    %% Send message with corrupt state
    TargetPid ! {corrupt_state, #{invalid => data}},

    timer:sleep(100),

    %% Verify process survived
    case is_process_alive(TargetPid) of
        true ->
            {ok, #{process_alive => true}};
        false ->
            {error, process_crashed}
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

test_invalid_input(Input) ->
    try
        _ = call_router_server:route_call(Input, <<"+15551234567">>),
        {ok, accepted}
    catch
        _:_ ->
            {ok, rejected}
    end.

check_system_health() ->
    %% Check critical processes
    CriticalProcesses = [call_router_server, call_router_sup],

    AllAlive = lists:all(
        fun(Name) ->
            case whereis(Name) of
                undefined -> false;
                Pid -> is_process_alive(Pid)
            end
        end,
        CriticalProcesses
    ),

    case AllAlive of
        true ->
            healthy;
        false ->
            DeadProcesses = [P || P <- CriticalProcesses, whereis(P) =:= undefined],
            {unhealthy, {dead_processes, DeadProcesses}}
    end.

report_fault_result(Result) ->
    io:format("~n=== Fault Injection Results ===~n"),
    io:format("Fault Type: ~p~n", [Result#fault_result.fault_type]),

    case Result#fault_result.injection_status of
        ok ->
            io:format("Injection: ✅ Success~n");
        {error, Reason} ->
            io:format("Injection: ❌ Failed (~p)~n", [Reason])
    end,

    case Result#fault_result.recovery_status of
        ok ->
            io:format("Recovery: ✅ Success (~pms)~n", [Result#fault_result.recovery_time_ms]);
        {error, Reason} ->
            io:format("Recovery: ❌ Failed (~p)~n", [Reason])
    end,

    io:format("~n").
