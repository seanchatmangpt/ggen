%%%-------------------------------------------------------------------
%%% @doc TPS Jidoka Application - Startup Module
%%%
%%% This module handles application startup and shutdown.
%%% It starts the Jidoka supervisor tree when the application starts.
%%% @end
%%%-------------------------------------------------------------------

-module(tps_jidoka_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application Callbacks
%%%===================================================================

%% @doc Application start callback.
%% Starts the Jidoka supervisor with configuration from app.src.
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    logger:info("Starting TPS Jidoka application"),

    %% Load configuration from app.src
    Config = get_config(),

    case jidoka_supervisor:start_link(Config) of
        {ok, Pid} ->
            logger:info("TPS Jidoka application started successfully"),
            {ok, Pid};
        {error, Reason} = Error ->
            logger:error("Failed to start TPS Jidoka: ~p", [Reason]),
            Error
    end.

%% @doc Application stop callback.
%% Cleanup when application stops.
-spec stop(State :: term()) -> ok.
stop(_State) ->
    logger:info("Stopping TPS Jidoka application"),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Load Jidoka configuration from app.src environment.
get_config() ->
    {ok, Env} = application:get_env(tps_jidoka),

    %% Extract sub-configs
    CircuitBreakerConfig = proplists:get_value(circuit_breaker, Env, []),
    RateLimiterConfig = proplists:get_value(rate_limiter, Env, []),
    WorkerPoolConfig = proplists:get_value(worker_pool, Env, []),

    %% Build config map
    #{
        pool_size => proplists:get_value(pool_size, WorkerPoolConfig, 10),
        circuit_threshold => proplists:get_value(threshold, CircuitBreakerConfig, 5),
        window_ms => proplists:get_value(window_ms, CircuitBreakerConfig, 10000),
        rate_limit => proplists:get_value(rate_limit, RateLimiterConfig, 1000)
    }.
