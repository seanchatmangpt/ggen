%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos Monkey - Chaos Engineering for Erlang/OTP
%%%
%%% Randomly injects failures to test system resilience:
%%% - Process kills
%%% - Network partitions
%%% - Message floods
%%% - Memory pressure
%%% - Supervisor failures
%%%
%%% Validates that the system recovers gracefully from failures.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_monkey).

-export([
    run/1,
    simulate_failures/1,
    get_scenarios/0
]).

-record(chaos_config, {
    target_supervisor :: atom(),
    scenarios :: [{atom(), float()}],  %% {scenario_name, probability}
    duration_sec :: pos_integer(),
    report_interval_sec :: pos_integer(),
    failure_interval_ms :: pos_integer()
}).

-record(chaos_stats, {
    total_failures :: non_neg_integer(),
    scenario_counts :: map(),
    recovery_times :: map(),
    unrecovered_failures :: [term()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run chaos monkey with configuration
run(Config) when is_map(Config) ->
    ChaosConfig = parse_config(Config),

    io:format("~n🐵 === CHAOS MONKEY ACTIVATED === 🐵~n~n"),
    io:format("Target Supervisor: ~p~n", [ChaosConfig#chaos_config.target_supervisor]),
    io:format("Duration: ~p seconds~n", [ChaosConfig#chaos_config.duration_sec]),
    io:format("Failure Interval: ~p ms~n", [ChaosConfig#chaos_config.failure_interval_ms]),
    io:format("~n"),
    io:format("Scenarios:~n"),
    [io:format("  - ~p (~.1f%)~n", [S, P * 100])
     || {S, P} <- ChaosConfig#chaos_config.scenarios],
    io:format("~n"),

    %% Run simulation
    Result = simulate_failures(ChaosConfig),

    io:format("~n🐵 === CHAOS MONKEY COMPLETE === 🐵~n~n"),

    Result.

%% @doc Simulate random failures
simulate_failures(Config) ->
    EndTime = erlang:system_time(second) + Config#chaos_config.duration_sec,
    InitStats = init_stats(),

    simulate_loop(EndTime, Config, InitStats).

%% @doc Get available chaos scenarios
get_scenarios() ->
    [
        kill_random_worker,
        kill_supervisor,
        network_partition,
        message_flood,
        memory_pressure,
        cpu_spike,
        slow_response,
        corrupt_state
    ].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_config(Config) ->
    DefaultScenarios = [
        {kill_random_worker, 0.30},
        {network_partition, 0.20},
        {message_flood, 0.20},
        {memory_pressure, 0.15},
        {kill_supervisor, 0.10},
        {cpu_spike, 0.05}
    ],

    #chaos_config{
        target_supervisor = maps:get(target_supervisor, Config, call_router_sup),
        scenarios = maps:get(scenarios, Config, DefaultScenarios),
        duration_sec = maps:get(duration_sec, Config, 300),
        report_interval_sec = maps:get(report_interval_sec, Config, 30),
        failure_interval_ms = maps:get(failure_interval_ms, Config, 1000)
    }.

init_stats() ->
    #chaos_stats{
        total_failures = 0,
        scenario_counts = #{},
        recovery_times = #{},
        unrecovered_failures = []
    }.

simulate_loop(EndTime, Config, Stats) ->
    Now = erlang:system_time(second),

    case Now < EndTime of
        true ->
            %% Select and execute random scenario
            Scenario = select_scenario(Config#chaos_config.scenarios),

            io:format("[~s] Executing: ~p~n", [format_timestamp(), Scenario]),

            StartTime = erlang:monotonic_time(millisecond),
            Result = execute_scenario(Scenario, Config),
            EndTime = erlang:monotonic_time(millisecond),

            RecoveryTime = EndTime - StartTime,

            %% Update statistics
            NewStats = update_stats(Stats, Scenario, Result, RecoveryTime),

            %% Periodic reporting
            case Now rem Config#chaos_config.report_interval_sec of
                0 -> report_stats(NewStats);
                _ -> ok
            end,

            %% Wait before next failure
            timer:sleep(Config#chaos_config.failure_interval_ms),

            simulate_loop(EndTime, Config, NewStats);
        false ->
            report_final_stats(Stats),
            {ok, Stats}
    end.

select_scenario(Scenarios) ->
    Random = rand:uniform(),
    select_scenario(Scenarios, Random, 0.0).

select_scenario([{Scenario, _Prob}], _Random, _Acc) ->
    %% Last scenario (fallback)
    Scenario;
select_scenario([{Scenario, Prob} | Rest], Random, Acc) ->
    NewAcc = Acc + Prob,
    case Random =< NewAcc of
        true -> Scenario;
        false -> select_scenario(Rest, Random, NewAcc)
    end.

execute_scenario(kill_random_worker, Config) ->
    case get_random_worker(Config#chaos_config.target_supervisor) of
        {ok, Pid} ->
            io:format("  💀 Killing worker: ~p~n", [Pid]),
            exit(Pid, kill),

            %% Verify supervisor restarted it
            timer:sleep(500),
            case verify_worker_restarted(Config#chaos_config.target_supervisor) of
                true ->
                    io:format("  ✅ Worker restarted by supervisor~n"),
                    {ok, restarted};
                false ->
                    io:format("  ❌ Worker NOT restarted!~n"),
                    {error, not_restarted}
            end;
        {error, Reason} ->
            io:format("  ⚠️  No workers available: ~p~n", [Reason]),
            {error, Reason}
    end;

execute_scenario(kill_supervisor, Config) ->
    SupName = Config#chaos_config.target_supervisor,
    case whereis(SupName) of
        undefined ->
            io:format("  ⚠️  Supervisor not found~n"),
            {error, not_found};
        Pid ->
            io:format("  💀 Killing supervisor: ~p~n", [Pid]),
            exit(Pid, kill),

            %% Verify application supervisor restarted it
            timer:sleep(1000),
            case whereis(SupName) of
                undefined ->
                    io:format("  ❌ Supervisor NOT restarted!~n"),
                    {error, not_restarted};
                NewPid when NewPid =/= Pid ->
                    io:format("  ✅ Supervisor restarted~n"),
                    {ok, restarted};
                _ ->
                    io:format("  ❌ Same supervisor PID~n"),
                    {error, same_pid}
            end
    end;

execute_scenario(network_partition, _Config) ->
    Nodes = nodes(),
    case Nodes of
        [] ->
            io:format("  ⚠️  No cluster nodes~n"),
            {error, no_cluster};
        _ ->
            TargetNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
            io:format("  🌐 Partitioning from node: ~p~n", [TargetNode]),

            erlang:disconnect_node(TargetNode),
            timer:sleep(5000),

            net_kernel:connect_node(TargetNode),
            timer:sleep(1000),

            case lists:member(TargetNode, nodes()) of
                true ->
                    io:format("  ✅ Network partition healed~n"),
                    {ok, healed};
                false ->
                    io:format("  ❌ Failed to reconnect~n"),
                    {error, reconnect_failed}
            end
    end;

execute_scenario(message_flood, Config) ->
    io:format("  🌊 Flooding messages...~n"),

    Workers = get_all_workers(Config#chaos_config.target_supervisor),
    NumMessages = 10000,

    %% Send flood
    [begin
         [Pid ! {flood, N} || N <- lists:seq(1, NumMessages)]
     end || {_, Pid, _, _} <- Workers],

    timer:sleep(2000),

    %% Check survivors
    AliveCount = length([P || {_, P, _, _} <- Workers, is_process_alive(P)]),
    TotalCount = length(Workers),

    case AliveCount == TotalCount of
        true ->
            io:format("  ✅ All workers survived flood~n"),
            {ok, survived};
        false ->
            io:format("  ❌ Workers died: ~p/~p alive~n", [AliveCount, TotalCount]),
            {error, {workers_died, TotalCount - AliveCount}}
    end;

execute_scenario(memory_pressure, _Config) ->
    io:format("  💾 Creating memory pressure...~n"),

    %% Allocate 1GB in 100MB chunks
    try
        _BigData = [crypto:strong_rand_bytes(100 * 1024 * 1024) || _ <- lists:seq(1, 10)],
        timer:sleep(3000),
        erlang:garbage_collect(),

        MemInfo = erlang:memory(),
        TotalMem = proplists:get_value(total, MemInfo),
        io:format("  ✅ Memory test complete. Total: ~p MB~n", [TotalMem div 1024 div 1024]),
        {ok, completed}
    catch
        _:Reason ->
            io:format("  ⚠️  Memory allocation failed: ~p~n", [Reason]),
            {error, Reason}
    end;

execute_scenario(cpu_spike, _Config) ->
    io:format("  🔥 Creating CPU spike...~n"),

    %% Spawn CPU-intensive processes
    Workers = [spawn(fun cpu_intensive_work/0) || _ <- lists:seq(1, erlang:system_info(schedulers))],

    timer:sleep(3000),

    %% Terminate workers
    [exit(W, kill) || W <- Workers],

    io:format("  ✅ CPU spike complete~n"),
    {ok, completed}.

cpu_intensive_work() ->
    %% Busy loop for 3 seconds
    EndTime = erlang:system_time(millisecond) + 3000,
    cpu_loop(EndTime).

cpu_loop(EndTime) ->
    Now = erlang:system_time(millisecond),
    case Now < EndTime of
        true ->
            _ = math:pow(rand:uniform(), 2),
            cpu_loop(EndTime);
        false ->
            ok
    end.

get_random_worker(SupName) ->
    Workers = get_all_workers(SupName),
    case Workers of
        [] ->
            {error, no_workers};
        _ ->
            {_, Pid, _, _} = lists:nth(rand:uniform(length(Workers)), Workers),
            {ok, Pid}
    end.

get_all_workers(SupName) ->
    case whereis(SupName) of
        undefined ->
            [];
        _Pid ->
            supervisor:which_children(SupName)
    end.

verify_worker_restarted(SupName) ->
    Workers = get_all_workers(SupName),
    length(Workers) > 0.

update_stats(Stats, Scenario, Result, RecoveryTime) ->
    ScenarioCounts = maps:update_with(
        Scenario,
        fun(Count) -> Count + 1 end,
        1,
        Stats#chaos_stats.scenario_counts
    ),

    RecoveryTimes = maps:update_with(
        Scenario,
        fun(Times) -> [RecoveryTime | Times] end,
        [RecoveryTime],
        Stats#chaos_stats.recovery_times
    ),

    UnrecoveredFailures = case Result of
        {error, Reason} ->
            [{Scenario, Reason} | Stats#chaos_stats.unrecovered_failures];
        _ ->
            Stats#chaos_stats.unrecovered_failures
    end,

    Stats#chaos_stats{
        total_failures = Stats#chaos_stats.total_failures + 1,
        scenario_counts = ScenarioCounts,
        recovery_times = RecoveryTimes,
        unrecovered_failures = UnrecoveredFailures
    }.

report_stats(Stats) ->
    io:format("~n--- Chaos Stats ---~n"),
    io:format("Total failures: ~p~n", [Stats#chaos_stats.total_failures]),
    io:format("Scenario breakdown:~n"),
    maps:foreach(
        fun(Scenario, Count) ->
            io:format("  ~p: ~p~n", [Scenario, Count])
        end,
        Stats#chaos_stats.scenario_counts
    ),
    io:format("~n").

report_final_stats(Stats) ->
    io:format("~n=== Final Chaos Statistics ===~n~n"),
    io:format("Total Failures: ~p~n", [Stats#chaos_stats.total_failures]),
    io:format("~n"),

    io:format("Scenario Breakdown:~n"),
    maps:foreach(
        fun(Scenario, Count) ->
            io:format("  ~-25s ~p~n", [atom_to_list(Scenario), Count])
        end,
        Stats#chaos_stats.scenario_counts
    ),
    io:format("~n"),

    io:format("Average Recovery Times (ms):~n"),
    maps:foreach(
        fun(Scenario, Times) ->
            Avg = lists:sum(Times) / length(Times),
            io:format("  ~-25s ~.2f~n", [atom_to_list(Scenario), Avg])
        end,
        Stats#chaos_stats.recovery_times
    ),
    io:format("~n"),

    NumUnrecovered = length(Stats#chaos_stats.unrecovered_failures),
    io:format("Unrecovered Failures: ~p~n", [NumUnrecovered]),
    case NumUnrecovered > 0 of
        true ->
            io:format("~n"),
            [io:format("  - ~p: ~p~n", [Scenario, Reason])
             || {Scenario, Reason} <- Stats#chaos_stats.unrecovered_failures];
        false ->
            ok
    end,
    io:format("~n").

format_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Y, M, D, H, Min, S]).
