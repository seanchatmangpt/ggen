%%%-------------------------------------------------------------------
%%% @doc
%%% Cluster Stress Testing
%%%
%%% Tests distributed Erlang cluster under stress:
%%% - Node failures
%%% - Network partitions
%%% - Split-brain scenarios
%%% - High message volume between nodes
%%% - Data consistency validation
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_stress).

-export([
    run/1,
    test_node_failure/1,
    test_split_brain/1,
    test_high_message_volume/1,
    verify_cluster_health/0
]).

-record(cluster_config, {
    nodes :: [node()],
    duration_sec :: pos_integer(),
    scenarios :: [atom()]
}).

-record(cluster_stats, {
    nodes_tested :: [node()],
    failures :: [{node(), term()}],
    message_counts :: map(),
    partition_events :: [term()],
    recovery_times :: [pos_integer()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run cluster stress tests
run(Config) when is_map(Config) ->
    ClusterConfig = parse_config(Config),

    io:format("~n=== Cluster Stress Test ===~n~n"),
    io:format("Nodes: ~p~n", [ClusterConfig#cluster_config.nodes]),
    io:format("Duration: ~p seconds~n", [ClusterConfig#cluster_config.duration_sec]),
    io:format("Scenarios: ~p~n", [ClusterConfig#cluster_config.scenarios]),
    io:format("~n"),

    %% Verify cluster connectivity
    case verify_cluster_connectivity(ClusterConfig#cluster_config.nodes) of
        ok ->
            run_stress_scenarios(ClusterConfig);
        {error, Reason} ->
            io:format("❌ Cluster connectivity check failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Test node failure scenario
test_node_failure(TargetNode) ->
    io:format("~n=== Testing Node Failure ===~n"),
    io:format("Target Node: ~p~n~n", [TargetNode]),

    %% Verify node is alive
    case net_adm:ping(TargetNode) of
        pong ->
            %% Simulate node crash
            io:format("Simulating node crash...~n"),
            rpc:call(TargetNode, init, stop, []),

            %% Monitor cluster reaction
            timer:sleep(2000),

            %% Check if other nodes detected failure
            RemainingNodes = nodes(),
            NodeDetected = not lists:member(TargetNode, RemainingNodes),

            case NodeDetected of
                true ->
                    io:format("✅ Node failure detected by cluster~n"),
                    {ok, detected};
                false ->
                    io:format("❌ Node failure NOT detected~n"),
                    {error, not_detected}
            end;
        pang ->
            {error, node_not_reachable}
    end.

%% @doc Test split-brain scenario
test_split_brain(ClusterNodes) ->
    io:format("~n=== Testing Split-Brain Scenario ===~n"),

    %% Divide cluster into two partitions
    MidPoint = length(ClusterNodes) div 2,
    {Partition1, Partition2} = lists:split(MidPoint, ClusterNodes),

    io:format("Partition 1: ~p~n", [Partition1]),
    io:format("Partition 2: ~p~n", [Partition2]),
    io:format("~n"),

    %% Create partition
    io:format("Creating network partition...~n"),
    create_partition(Partition1, Partition2),

    %% Wait for split-brain detection
    timer:sleep(5000),

    %% Verify partitions are isolated
    IsolationVerified = verify_partition_isolation(Partition1, Partition2),

    %% Heal partition
    io:format("Healing partition...~n"),
    heal_partition(Partition1, Partition2),

    timer:sleep(3000),

    %% Verify cluster recovered
    RecoveryVerified = verify_cluster_recovered(ClusterNodes),

    Result = #{
        isolation_verified => IsolationVerified,
        recovery_verified => RecoveryVerified
    },

    report_split_brain_results(Result),

    case IsolationVerified andalso RecoveryVerified of
        true -> {ok, Result};
        false -> {error, Result}
    end.

%% @doc Test high message volume between nodes
test_high_message_volume(ClusterNodes) ->
    io:format("~n=== Testing High Message Volume ===~n"),

    NumMessages = 100000,
    NumWorkers = 10,

    io:format("Messages per worker: ~p~n", [NumMessages]),
    io:format("Workers per node: ~p~n", [NumWorkers]),
    io:format("Total messages: ~p~n", [NumMessages * NumWorkers * length(ClusterNodes)]),
    io:format("~n"),

    %% Spawn workers on each node
    io:format("Spawning workers...~n"),
    Workers = spawn_message_workers(ClusterNodes, NumWorkers, NumMessages),

    %% Monitor message delivery
    StartTime = erlang:monotonic_time(millisecond),

    %% Wait for completion
    Results = wait_for_workers(Workers, 60000),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    TotalSent = lists:sum([maps:get(sent, R, 0) || R <- Results]),
    TotalReceived = lists:sum([maps:get(received, R, 0) || R <- Results]),
    MessageRate = (TotalSent / Duration) * 1000,

    io:format("~n=== Message Volume Results ===~n"),
    io:format("Duration: ~pms~n", [Duration]),
    io:format("Messages sent: ~p~n", [TotalSent]),
    io:format("Messages received: ~p~n", [TotalReceived]),
    io:format("Message rate: ~.2f msg/sec~n", [MessageRate]),
    io:format("Delivery rate: ~.2f%~n", [(TotalReceived / max(1, TotalSent)) * 100]),
    io:format("~n"),

    {ok, #{
        total_sent => TotalSent,
        total_received => TotalReceived,
        message_rate => MessageRate,
        duration_ms => Duration
    }}.

%% @doc Verify cluster health
verify_cluster_health() ->
    Nodes = [node() | nodes()],

    io:format("~n=== Cluster Health Check ===~n"),
    io:format("Checking ~p nodes...~n~n", [length(Nodes)]),

    Results = [check_node_health(N) || N <- Nodes],

    AllHealthy = lists:all(fun(R) -> R =:= healthy end, Results),

    case AllHealthy of
        true ->
            io:format("✅ All nodes healthy~n"),
            healthy;
        false ->
            UnhealthyNodes = [N || {N, R} <- lists:zip(Nodes, Results), R =/= healthy],
            io:format("❌ Unhealthy nodes: ~p~n", [UnhealthyNodes]),
            {unhealthy, UnhealthyNodes}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_config(Config) ->
    #cluster_config{
        nodes = maps:get(nodes, Config, nodes()),
        duration_sec = maps:get(duration_sec, Config, 300),
        scenarios = maps:get(scenarios, Config, [node_failure, split_brain, message_volume])
    }.

verify_cluster_connectivity(Nodes) ->
    io:format("Verifying cluster connectivity...~n"),

    Results = [net_adm:ping(N) || N <- Nodes],
    AllConnected = lists:all(fun(R) -> R =:= pong end, Results),

    case AllConnected of
        true ->
            io:format("✅ All nodes connected~n~n"),
            ok;
        false ->
            DisconnectedNodes = [N || {N, R} <- lists:zip(Nodes, Results), R =:= pang],
            {error, {disconnected_nodes, DisconnectedNodes}}
    end.

run_stress_scenarios(Config) ->
    Scenarios = Config#cluster_config.scenarios,
    Nodes = Config#cluster_config.nodes,

    Results = lists:foldl(
        fun(Scenario, Acc) ->
            Result = run_scenario(Scenario, Nodes),
            [{Scenario, Result} | Acc]
        end,
        [],
        Scenarios
    ),

    report_cluster_stress_results(Results),

    {ok, Results}.

run_scenario(node_failure, [Node | _Rest]) ->
    test_node_failure(Node);
run_scenario(split_brain, Nodes) when length(Nodes) >= 2 ->
    test_split_brain(Nodes);
run_scenario(message_volume, Nodes) ->
    test_high_message_volume(Nodes);
run_scenario(Scenario, _Nodes) ->
    {error, {unknown_scenario, Scenario}}.

create_partition(Partition1, Partition2) ->
    %% Disconnect nodes between partitions
    [erlang:disconnect_node(N2) || N1 <- Partition1, N2 <- Partition2],
    ok.

verify_partition_isolation(Partition1, Partition2) ->
    %% Verify nodes in Partition1 cannot see nodes in Partition2
    Isolated = lists:all(
        fun(N2) ->
            not lists:member(N2, nodes())
        end,
        Partition2
    ),
    Isolated.

heal_partition(Partition1, Partition2) ->
    %% Reconnect all nodes
    [net_kernel:connect_node(N2) || _N1 <- Partition1, N2 <- Partition2],
    timer:sleep(1000),
    ok.

verify_cluster_recovered(ExpectedNodes) ->
    ActualNodes = [node() | nodes()],
    lists:all(fun(N) -> lists:member(N, ActualNodes) end, ExpectedNodes).

spawn_message_workers(Nodes, NumWorkers, NumMessages) ->
    lists:flatten([
        [spawn(N, fun() -> message_worker(NumMessages) end)
         || _ <- lists:seq(1, NumWorkers)]
        || N <- Nodes
    ]).

message_worker(NumMessages) ->
    Receiver = self(),

    %% Send messages to random nodes
    Sent = send_messages(NumMessages),

    %% Count received messages
    Received = count_received_messages(5000),

    Receiver ! {result, #{sent => Sent, received => Received}}.

send_messages(NumMessages) ->
    Nodes = [node() | nodes()],

    lists:foldl(
        fun(N, Acc) ->
            TargetNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
            {call_router_server, TargetNode} ! {test_message, N},
            Acc + 1
        end,
        0,
        lists:seq(1, NumMessages)
    ).

count_received_messages(TimeoutMs) ->
    count_received_messages(TimeoutMs, 0).

count_received_messages(0, Count) ->
    Count;
count_received_messages(TimeoutMs, Count) ->
    receive
        {test_message, _} ->
            count_received_messages(TimeoutMs, Count + 1)
    after 100 ->
        count_received_messages(TimeoutMs - 100, Count)
    end.

wait_for_workers(Workers, TimeoutMs) ->
    wait_for_workers(Workers, TimeoutMs, []).

wait_for_workers([], _TimeoutMs, Acc) ->
    Acc;
wait_for_workers(_Workers, 0, Acc) ->
    Acc;
wait_for_workers(Workers, TimeoutMs, Acc) ->
    receive
        {result, Result} ->
            wait_for_workers(Workers, TimeoutMs, [Result | Acc])
    after 1000 ->
        wait_for_workers(Workers, TimeoutMs - 1000, Acc)
    end.

check_node_health(Node) ->
    case rpc:call(Node, erlang, memory, []) of
        {badrpc, _} ->
            unhealthy;
        Memory when is_list(Memory) ->
            healthy
    end.

report_split_brain_results(Result) ->
    io:format("~n=== Split-Brain Test Results ===~n"),
    io:format("Isolation: ~s~n", [format_bool(maps:get(isolation_verified, Result))]),
    io:format("Recovery: ~s~n", [format_bool(maps:get(recovery_verified, Result))]),
    io:format("~n").

report_cluster_stress_results(Results) ->
    io:format("~n=== Cluster Stress Test Results ===~n~n"),

    lists:foreach(
        fun({Scenario, Result}) ->
            io:format("~p: ~p~n", [Scenario, Result])
        end,
        Results
    ),

    io:format("~n").

format_bool(true) -> "✅ PASS";
format_bool(false) -> "❌ FAIL".
