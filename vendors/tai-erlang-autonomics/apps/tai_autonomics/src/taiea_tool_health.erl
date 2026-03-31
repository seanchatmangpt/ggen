%%%-------------------------------------------------------------------
%%% @doc Health Check Tool - System health status monitoring
%%%
%%% Tool: taiea.health.check
%%% Input: {} (empty object)
%%% Output: {ok, HealthStatus, Receipt}
%%%
%%% Queries system health indicators:
%%% - Node availability and status
%%% - Memory usage and limits
%%% - Process count and health
%%% - Governor availability (entitlement, billing, compliance)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_health).
-compile({no_auto_import, [is_process_alive/1]}).

%% API
-export([handle/1]).

%% Types
-type health_status() :: healthy | degraded | critical.
-type tool_response() :: map().
-type receipt() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle health check tool request
-spec handle(map()) -> {ok, tool_response(), receipt()} | {error, term()}.
handle(_Input) ->
    try
        %% Collect health metrics
        NodeHealth = check_node_health(),
        MemoryHealth = check_memory_health(),
        ProcessHealth = check_process_health(),
        GovernorHealth = check_governor_health(),

        %% Aggregate status
        AggregateStatus = aggregate_health(
            NodeHealth,
            MemoryHealth,
            ProcessHealth,
            GovernorHealth
        ),

        %% Build response
        Response = #{
            status => AggregateStatus,
            checks => #{
                node => NodeHealth,
                memory => MemoryHealth,
                processes => ProcessHealth,
                governors => GovernorHealth
            },
            timestamp => timestamp(),
            node => node(),
            uptime_ms => erlang:system_time(millisecond) - element(1, erlang:statistics(wall_clock))
        },

        %% Emit receipt
        ToolReceipt = emit_tool_receipt(AggregateStatus, Response),

        {ok, Response, ToolReceipt}
    catch
        Class:Reason ->
            ErrorReceipt = emit_error_receipt(Class, Reason),
            {error, {health_check_failed, Class, Reason, ErrorReceipt}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Check node health
-spec check_node_health() -> map().
check_node_health() ->
    #{
        status => healthy,
        node => node(),
        connected_nodes => erlang:nodes(connected),
        visible_nodes => erlang:nodes(visible),
        hidden_nodes => erlang:nodes(hidden)
    }.

%% @private Check memory health
-spec check_memory_health() -> map().
check_memory_health() ->
    MemInfo = erlang:memory(),
    Total = proplists:get_value(total, MemInfo, 0),
    Used = proplists:get_value(used, MemInfo, 0),
    ProcessesMemory = proplists:get_value(processes, MemInfo, 0),

    UsagePercent = case Total of
        0 -> 0;
        _ -> (Used / Total) * 100
    end,

    Status = case UsagePercent of
        P when P > 90 -> critical;
        P when P > 70 -> degraded;
        _ -> healthy
    end,

    #{
        status => Status,
        total_bytes => Total,
        used_bytes => Used,
        processes_bytes => ProcessesMemory,
        usage_percent => UsagePercent
    }.

%% @private Check process health
-spec check_process_health() -> map().
check_process_health() ->
    ProcCount = erlang:system_info(process_count),
    MaxProcs = erlang:system_info(process_limit),

    UsagePercent = (ProcCount / MaxProcs) * 100,

    Status = case UsagePercent of
        P when P > 90 -> critical;
        P when P > 70 -> degraded;
        _ -> healthy
    end,

    #{
        status => Status,
        current_count => ProcCount,
        max_count => MaxProcs,
        usage_percent => UsagePercent
    }.

%% @private Check governor availability
-spec check_governor_health() -> map().
check_governor_health() ->
    Governors = [
        {entitlement_governor, <<"entitlement">>},
        {billing_governor, <<"billing">>},
        {compliance_audit_governor, <<"compliance">>},
        {customer_account_governor, <<"customer_account">>}
    ],

    GovernorStatuses = lists:map(
        fun({_ModuleName, GovernorName}) ->
            check_governor_process(GovernorName)
        end,
        Governors
    ),

    AllHealthy = lists:all(fun(#{status := S}) -> S =:= healthy end, GovernorStatuses),
    OverallStatus = case AllHealthy of
        true -> healthy;
        false -> degraded
    end,

    #{
        status => OverallStatus,
        governors => GovernorStatuses
    }.

%% @private Check if a governor process is running
-spec check_governor_process(binary()) -> map().
check_governor_process(GovernorName) ->
    Status = case whereis(binary_to_atom(GovernorName, utf8)) of
        undefined -> unavailable;
        Pid ->
            case erlang:is_process_alive(Pid) of
                true -> healthy;
                false -> dead
            end
    end,

    #{
        name => GovernorName,
        status => Status
    }.

%% @private Aggregate health statuses
-spec aggregate_health(map(), map(), map(), map()) -> health_status().
aggregate_health(Node, Memory, Processes, Governors) ->
    Statuses = [
        maps:get(status, Node, unknown),
        maps:get(status, Memory, unknown),
        maps:get(status, Processes, unknown),
        maps:get(status, Governors, unknown)
    ],

    case lists:member(critical, Statuses) of
        true -> critical;
        false ->
            case lists:member(degraded, Statuses) of
                true -> degraded;
                false -> healthy
            end
    end.

%% @private Emit receipt for successful health check
-spec emit_tool_receipt(health_status(), tool_response()) -> receipt().
emit_tool_receipt(Status, _Response) ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.health.check">>,
        event => <<"health_check_completed">>,
        status => Status,
        message => <<
            "Health check completed with status: ",
            (atom_to_binary(Status, utf8))/binary
        >>,
        metadata => #{
            check_duration_ms => 1,
            node => node()
        }
    }.

%% @private Emit receipt for error during health check
-spec emit_error_receipt(atom(), term()) -> receipt().
emit_error_receipt(ErrorClass, ErrorReason) ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.health.check">>,
        event => <<"health_check_failed">>,
        status => error,
        error => #{
            class => atom_to_binary(ErrorClass, utf8),
            reason => format_error(ErrorReason)
        },
        message => <<"Health check failed">>,
        metadata => #{
            node => node()
        }
    }.

%% @private Format error for receipt
-spec format_error(term()) -> binary().
format_error(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% @private Generate unique receipt ID
-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @private Get current timestamp in milliseconds
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).
