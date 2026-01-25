%%%-------------------------------------------------------------------
%%% @doc Jidoka Metrics & Observability Module
%%%
%%% Provides comprehensive metrics collection and export functionality
%%% for the Jidoka system. Integrates with monitoring systems like
%%% Prometheus, Grafana, and OpenTelemetry.
%%%
%%% Metrics exposed:
%%% - Circuit breaker state transitions
%%% - Rate limiter token availability and rejections
%%% - Worker pool utilization and exhaustion
%%% - Request latency and throughput
%%% - System health indicators
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_metrics).

%% Metrics API
-export([
    collect_metrics/0,
    export_prometheus/0,
    health_status/0,
    alert_on_degradation/0
]).

%% Internal state
-record(metrics, {
    timestamp :: integer(),
    circuit_breaker_state :: atom(),
    circuit_breaker_failures :: integer(),
    rate_limiter_tokens :: float(),
    rate_limiter_accepted :: integer(),
    rate_limiter_rejected :: integer(),
    worker_pool_size :: integer(),
    worker_pool_available :: integer(),
    worker_pool_utilization :: float()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Collect current metrics from all Jidoka components.
%% Returns a record with all system metrics.
-spec collect_metrics() -> #metrics{} | {error, term()}.
collect_metrics() ->
    try
        {CBState, CBFailures, _} = jidoka_circuit_breaker:status(),
        {Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
        {available, Available} = jidoka_worker_pool:status(),
        PoolSize = 10,  % Could be parameterized

        Utilization = (1 - (Available / PoolSize)) * 100,

        #metrics{
            timestamp = erlang:system_time(millisecond),
            circuit_breaker_state = CBState,
            circuit_breaker_failures = CBFailures,
            rate_limiter_tokens = Tokens,
            rate_limiter_accepted = Accepted,
            rate_limiter_rejected = Rejected,
            worker_pool_size = PoolSize,
            worker_pool_available = Available,
            worker_pool_utilization = Utilization
        }
    catch
        _:Error ->
            logger:error("Failed to collect metrics: ~p", [Error]),
            {error, Error}
    end.

%% @doc Export metrics in Prometheus text format.
%% Returns a string formatted for Prometheus scrape endpoint.
-spec export_prometheus() -> binary().
export_prometheus() ->
    Metrics = collect_metrics(),
    case Metrics of
        {error, _} ->
            <<"# ERROR: Failed to collect metrics\n">>;
        #metrics{} ->
            format_prometheus(Metrics)
    end.

%% @doc Get overall system health status.
%% Returns: {Status, Details}
%% Status: healthy | degraded | critical
-spec health_status() -> {atom(), map()}.
health_status() ->
    Metrics = collect_metrics(),
    case Metrics of
        {error, Reason} ->
            {critical, #{error => Reason, timestamp => erlang:system_time(millisecond)}};
        #metrics{} ->
            evaluate_health(Metrics)
    end.

%% @doc Alert if system health is degraded.
%% Logs appropriate alerts based on thresholds.
-spec alert_on_degradation() -> ok.
alert_on_degradation() ->
    {Status, Details} = health_status(),
    case Status of
        healthy ->
            logger:debug("System healthy: ~p", [Details]),
            ok;
        degraded ->
            logger:warning("System degraded: ~p", [Details]),
            ok;
        critical ->
            logger:error("System critical: ~p", [Details]),
            ok
    end.

%%%===================================================================
%%% Private Functions
%%%===================================================================

%% @private Evaluate health status based on metrics.
evaluate_health(#metrics{
    circuit_breaker_state = CBState,
    worker_pool_utilization = Utilization,
    rate_limiter_rejected = Rejected,
    rate_limiter_accepted = Accepted
}) ->
    %% Determine status based on multiple factors
    Status = case {CBState, Utilization, Rejected, Accepted} of
        %% Critical: Circuit open AND high utilization
        {open, U, _, _} when U > 90 ->
            critical;
        %% Critical: Circuit open
        {open, _, _, _} ->
            critical;
        %% Degraded: High utilization OR high rejection rate
        {_, U, _, _} when U > 85 ->
            degraded;
        {_, _, R, A} when (R + A) > 0, (R / (R + A)) > 0.15 ->
            degraded;
        %% Healthy: All metrics normal
        _ ->
            healthy
    end,

    Details = #{
        circuit_breaker_state => CBState,
        worker_pool_utilization_percent => round(Utilization),
        rejection_rate => case Rejected + Accepted of
            0 -> 0.0;
            Total -> (Rejected / Total) * 100
        end,
        timestamp => erlang:system_time(millisecond)
    },

    {Status, Details}.

%% @private Format metrics in Prometheus text format.
format_prometheus(#metrics{
    circuit_breaker_state = CBState,
    circuit_breaker_failures = CBFailures,
    rate_limiter_tokens = Tokens,
    rate_limiter_accepted = Accepted,
    rate_limiter_rejected = Rejected,
    worker_pool_size = PoolSize,
    worker_pool_available = Available,
    worker_pool_utilization = Utilization,
    timestamp = Timestamp
}) ->
    Lines = [
        <<"# HELP circuit_breaker_state Jidoka circuit breaker state (0=closed, 1=open, 2=half_open)">>,
        <<"# TYPE circuit_breaker_state gauge">>,
        io_lib:format("circuit_breaker_state ~p~n", [state_to_int(CBState)]),

        <<"# HELP circuit_breaker_failures_in_window Number of failures in current window">>,
        <<"# TYPE circuit_breaker_failures_in_window gauge">>,
        io_lib:format("circuit_breaker_failures_in_window ~p~n", [CBFailures]),

        <<"# HELP rate_limiter_tokens Current tokens in bucket">>,
        <<"# TYPE rate_limiter_tokens gauge">>,
        io_lib:format("rate_limiter_tokens ~.1f~n", [Tokens]),

        <<"# HELP rate_limiter_accepted_total Cumulative accepted requests">>,
        <<"# TYPE rate_limiter_accepted_total counter">>,
        io_lib:format("rate_limiter_accepted_total ~p~n", [Accepted]),

        <<"# HELP rate_limiter_rejected_total Cumulative rejected requests">>,
        <<"# TYPE rate_limiter_rejected_total counter">>,
        io_lib:format("rate_limiter_rejected_total ~p~n", [Rejected]),

        <<"# HELP worker_pool_size Total worker pool size">>,
        <<"# TYPE worker_pool_size gauge">>,
        io_lib:format("worker_pool_size ~p~n", [PoolSize]),

        <<"# HELP worker_pool_available Available workers">>,
        <<"# TYPE worker_pool_available gauge">>,
        io_lib:format("worker_pool_available ~p~n", [Available]),

        <<"# HELP worker_pool_utilization_percent Worker pool utilization percentage">>,
        <<"# TYPE worker_pool_utilization_percent gauge">>,
        io_lib:format("worker_pool_utilization_percent ~.1f~n", [Utilization]),

        <<"# HELP jidoka_metrics_timestamp_ms Timestamp of metrics collection">>,
        <<"# TYPE jidoka_metrics_timestamp_ms gauge">>,
        io_lib:format("jidoka_metrics_timestamp_ms ~p~n", [Timestamp])
    ],

    iolist_to_binary([L || L <- Lines]).

%% @private Convert circuit breaker state to integer for Prometheus.
state_to_int(closed) -> 0;
state_to_int(open) -> 1;
state_to_int(half_open) -> 2;
state_to_int(unknown) -> -1.
