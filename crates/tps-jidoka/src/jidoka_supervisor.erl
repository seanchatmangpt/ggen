%%%-------------------------------------------------------------------
%%% @doc Jidoka Supervisor Tree - Stop-the-Line Autonomation
%%%
%%% Implements production-grade Jidoka (autonomation with human touch) using
%%% Erlang supervisor trees. When a defect is detected, the system stops
%%% processing that specific request (fail-fast) rather than propagating the
%%% error through the entire system.
%%%
%%% Architecture:
%%% - jidoka_supervisor (top-level, one_for_one strategy)
%%%   ├─ jidoka_worker_pool_sup (circuit breaker + worker pool)
%%%   ├─ jidoka_circuit_breaker (state machine for circuit breaker)
%%%   └─ jidoka_rate_limiter (token bucket for flow control)
%%%
%%% Jidoka Principles:
%%% 1. Stop the line when a defect is detected (fail-fast)
%%% 2. Don't amplify errors (don't retry, don't queue)
%%% 3. Isolate failures (one worker crash doesn't kill supervisor)
%%% 4. Observable (log every state transition)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
-export([get_status/0, get_pool_status/0, reset_circuit_breaker/0]).

%% Supervisor callbacks
-export([init/1]).

%% Internal state
-define(SERVER, ?MODULE).
-define(POOL_NAME, jidoka_worker_pool).
-define(POOL_SIZE, 10).
-define(CIRCUIT_BREAKER_THRESHOLD, 5).
-define(CIRCUIT_BREAKER_WINDOW_MS, 10000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the Jidoka supervisor with default configuration.
%% Starts the full supervision tree:
%% - Worker pool with configurable size
%% - Circuit breaker with configurable threshold
%% - Rate limiter with token bucket algorithm
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the Jidoka supervisor with custom configuration.
%% Config is a proplist:
%% - {pool_size, integer()} - Number of worker processes (default: 10)
%% - {circuit_threshold, integer()} - Failures to trigger open (default: 5)
%% - {window_ms, integer()} - Time window for failure count (default: 10000)
%% - {rate_limit, integer()} - Requests per second (default: 1000)
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, Config) of
        {ok, Pid} ->
            logger:info(
                "Jidoka supervisor started: ~p, pool_size=~p",
                [Pid, get_pool_size(Config)]
            ),
            {ok, Pid};
        {error, Reason} ->
            logger:error("Failed to start Jidoka supervisor: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Get current status of the Jidoka system.
%% Returns a map with:
%% - supervisor_pid: The supervisor process ID
%% - circuit_breaker_state: closed | open | half_open
%% - active_workers: Number of currently active workers
%% - failed_requests: Number of failures in current window
-spec get_status() -> map().
get_status() ->
    case whereis(?SERVER) of
        undefined ->
            logger:warning("Jidoka supervisor not running"),
            #{error => not_started};
        Pid ->
            CircuitState =
                case whereis(jidoka_circuit_breaker) of
                    undefined -> unknown;
                    CBPid -> jidoka_circuit_breaker:get_state(CBPid)
                end,
            #{
                supervisor_pid => Pid,
                circuit_breaker_state => CircuitState,
                active_workers => get_active_workers(),
                timestamp => erlang:system_time(millisecond)
            }
    end.

%% @doc Get detailed pool status.
%% Returns:
%% - pool_size: Configured pool size
%% - available_workers: Workers ready to accept work
%% - queue_depth: Pending requests in queue
%% - utilization: Percentage of pool in use
-spec get_pool_status() -> map().
get_pool_status() ->
    case whereis(?POOL_NAME) of
        undefined ->
            logger:warning("Worker pool not running"),
            #{error => pool_not_started};
        _PoolPid ->
            {available, Available} = poolboy:status(?POOL_NAME),
            PoolSize = get_pool_size([]),
            Utilization = round((1 - (Available / PoolSize)) * 100),
            #{
                pool_size => PoolSize,
                available_workers => Available,
                in_use => PoolSize - Available,
                utilization_percent => Utilization,
                timestamp => erlang:system_time(millisecond)
            }
    end.

%% @doc Reset the circuit breaker to closed state.
%% Used for operational recovery after addressing root causes.
-spec reset_circuit_breaker() -> ok | {error, term()}.
reset_circuit_breaker() ->
    case whereis(jidoka_circuit_breaker) of
        undefined ->
            logger:warning("Circuit breaker not running"),
            {error, not_started};
        CBPid ->
            logger:warning("Resetting Jidoka circuit breaker"),
            jidoka_circuit_breaker:reset(CBPid)
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @private
%% @doc Initialize the supervisor with child specifications.
%% Strategy: one_for_one
%% - If a child crashes, only that child is restarted
%% - This prevents cascading failures (Jidoka isolation principle)
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Config) ->
    logger:info("Initializing Jidoka supervisor tree with config: ~p", [Config]),

    PoolSize = proplists:get_value(pool_size, Config, ?POOL_SIZE),
    Threshold = proplists:get_value(circuit_threshold, Config, ?CIRCUIT_BREAKER_THRESHOLD),
    WindowMs = proplists:get_value(window_ms, Config, ?CIRCUIT_BREAKER_WINDOW_MS),
    RateLimit = proplists:get_value(rate_limit, Config, 1000),

    %% Supervisor flags: one_for_one strategy
    %% Intensity: max 3 restarts in 60 seconds (prevent restart loops)
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 60
    },

    %% Child specifications
    ChildSpecs = [
        %% Circuit breaker (monitors failure rate)
        #{
            id => jidoka_circuit_breaker,
            start => {jidoka_circuit_breaker, start_link, [#{
                threshold => Threshold,
                window_ms => WindowMs
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [jidoka_circuit_breaker]
        },

        %% Rate limiter (token bucket algorithm)
        #{
            id => jidoka_rate_limiter,
            start => {jidoka_rate_limiter, start_link, [#{
                rate_limit => RateLimit,
                burst_size => RateLimit div 2
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [jidoka_rate_limiter]
        },

        %% Worker pool (request handlers)
        #{
            id => jidoka_worker_pool_sup,
            start => {jidoka_worker_pool, start_link, [#{
                pool_name => ?POOL_NAME,
                pool_size => PoolSize,
                max_overflow => 0
            }]},
            restart => permanent,
            shutdown => 10000,
            type => supervisor,
            modules => [jidoka_worker_pool]
        }
    ],

    logger:info(
        "Jidoka supervisor initialized with pool_size=~p, threshold=~p, rate_limit=~p/s",
        [PoolSize, Threshold, RateLimit]
    ),

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Private Functions
%%%===================================================================

%% @private Get pool size from config or default.
get_pool_size(Config) ->
    proplists:get_value(pool_size, Config, ?POOL_SIZE).

%% @private Get count of active workers (workers in use).
get_active_workers() ->
    case whereis(?POOL_NAME) of
        undefined -> 0;
        _PoolPid ->
            {available, Available} = poolboy:status(?POOL_NAME),
            PoolSize = ?POOL_SIZE,
            PoolSize - Available
    end.

%%%===================================================================
%%% Internal API (for testing and monitoring)
%%%===================================================================

%% @doc Internal: Get all child PIDs (used by tests).
%% Returns: {CircuitBreakerPid, RateLimiterPid, WorkerPoolPid}
-spec get_child_pids() -> {pid(), pid(), pid()} | {error, term()}.
get_child_pids() ->
    case whereis(?SERVER) of
        undefined -> {error, not_started};
        _SupPid ->
            CB = whereis(jidoka_circuit_breaker),
            RL = whereis(jidoka_rate_limiter),
            WP = whereis(?POOL_NAME),
            case {CB, RL, WP} of
                {undefined, _, _} -> {error, circuit_breaker_not_started};
                {_, undefined, _} -> {error, rate_limiter_not_started};
                {_, _, undefined} -> {error, worker_pool_not_started};
                {CBPid, RLPid, WPPid} -> {CBPid, RLPid, WPPid}
            end
    end.
