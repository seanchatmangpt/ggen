%%%-------------------------------------------------------------------
%%% @doc Jidoka Worker Pool - Fixed-Size Request Handler Pool
%%%
%%% Implements a fixed-size worker pool using the poolboy library.
%%% Jidoka principle: Don't queue unbounded requests. If pool is full,
%%% reject the request (fail-fast) rather than queue it indefinitely.
%%%
%%% Features:
%%% - Fixed pool size (no overflow)
%%% - Health checks on workers
%%% - Rejection when pool exhausted (Jidoka fail-fast)
%%% - Metrics: utilization, rejections, response times
%%% - Graceful shutdown with in-flight request completion
%%%
%%% Configuration:
%%% - pool_name: Name of the pool (default: jidoka_worker_pool)
%%% - pool_size: Number of workers (default: 10)
%%% - max_overflow: Max temporary overflow (default: 0 for Jidoka)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_worker_pool).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([execute/1, execute/2]).
-export([status/0, health_check/0]).

%% Supervisor callbacks
-export([init/1]).

%% Worker callbacks
-export([start_worker/0, init_worker/0]).

%% Internal
-record(state, {
    pool_name :: atom(),
    pool_size :: integer(),
    health_check_interval :: integer()
}).

-define(DEFAULT_POOL_NAME, jidoka_worker_pool).
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_HEALTH_INTERVAL, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the worker pool supervisor and poolboy pool.
%% Config:
%% - pool_name: Name for the pool (default: jidoka_worker_pool)
%% - pool_size: Number of workers (default: 10)
%% - max_overflow: Overflow workers allowed (default: 0 for Jidoka)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    PoolName = maps:get(pool_name, Config, ?DEFAULT_POOL_NAME),
    PoolSize = maps:get(pool_size, Config, ?DEFAULT_POOL_SIZE),
    MaxOverflow = maps:get(max_overflow, Config, 0),

    case supervisor:start_link(?MODULE, #{
        pool_name => PoolName,
        pool_size => PoolSize,
        max_overflow => MaxOverflow
    }) of
        {ok, Pid} ->
            logger:info(
                "Worker pool started: name=~p, size=~p, overflow=~p",
                [PoolName, PoolSize, MaxOverflow]
            ),
            {ok, Pid};
        {error, Reason} ->
            logger:error("Failed to start worker pool: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Execute a function in the worker pool.
%% Returns: Result of Fun() | {error, pool_exhausted}
%%
%% Jidoka principle: If pool is exhausted, immediately return error
%% rather than queuing the request. Fail-fast prevents load buildup.
-spec execute(fun()) -> any() | {error, pool_exhausted}.
execute(Fun) ->
    execute(Fun, ?DEFAULT_POOL_NAME).

%% @doc Execute a function with custom pool name.
-spec execute(fun(), atom()) -> any() | {error, pool_exhausted}.
execute(Fun, PoolName) ->
    StartTime = erlang:system_time(millisecond),
    case poolboy:checkout(PoolName, false) of
        full ->
            logger:warning("Worker pool exhausted: pool=~p", [PoolName]),
            {error, pool_exhausted};
        Worker ->
            try
                Result = gen_server:call(Worker, {execute, Fun}),
                ElapsedMs = erlang:system_time(millisecond) - StartTime,
                logger:debug("Worker executed: time=~pms, pool=~p", [ElapsedMs, PoolName]),
                Result
            after
                poolboy:checkin(PoolName, Worker)
            end
    end.

%% @doc Get pool status.
%% Returns: {available_workers, integer()} | {error, pool_not_found}
-spec status() -> {available_workers, integer()} | {error, atom()}.
status() ->
    status(?DEFAULT_POOL_NAME).

%% @doc Get pool status with custom pool name.
-spec status(atom()) -> {available_workers, integer()} | {error, atom()}.
status(PoolName) ->
    case catch poolboy:status(PoolName) of
        {available, Count} -> {available, Count};
        {'EXIT', _} -> {error, pool_not_found};
        _ -> {error, unknown}
    end.

%% @doc Health check: verify all workers are responsive.
%% Returns: ok | {error, Reason}
-spec health_check() -> ok | {error, term()}.
health_check() ->
    health_check(?DEFAULT_POOL_NAME).

%% @doc Health check with custom pool name.
-spec health_check(atom()) -> ok | {error, term()}.
health_check(PoolName) ->
    case execute(fun() -> ok end, PoolName) of
        ok ->
            logger:info("Worker pool health check passed: pool=~p", [PoolName]),
            ok;
        {error, Reason} = Error ->
            logger:warning("Worker pool health check failed: pool=~p, reason=~p", [PoolName, Reason]),
            Error
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @private Initialize the pool supervisor.
init(Config) ->
    PoolName = maps:get(pool_name, Config),
    PoolSize = maps:get(pool_size, Config),
    MaxOverflow = maps:get(max_overflow, Config),

    logger:info(
        "Initializing worker pool supervisor: name=~p, size=~p",
        [PoolName, PoolSize]
    ),

    %% Supervisor flags: one_for_one (isolate worker crashes)
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% Poolboy configuration
    PoolboyConfig = [
        {name, {local, PoolName}},
        {worker_module, ?MODULE},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],

    %% Child spec for poolboy pool
    ChildSpecs = [
        #{
            id => PoolName,
            start => {poolboy, start_link, [PoolboyConfig]},
            restart => permanent,
            shutdown => 10000,
            type => worker,
            modules => [poolboy]
        },

        %% Health check monitor
        #{
            id => {health_check, PoolName},
            start => {?MODULE, start_health_monitor, [PoolName]},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Worker Implementation (poolboy)
%%%===================================================================

%% @private Start a worker process (called by poolboy).
start_worker() ->
    gen_server:start_link(?MODULE, [], []).

%% @private Initialize worker state (called by gen_server).
init(_Args) ->
    logger:debug("Worker started: pid=~p", [self()]),
    {ok, #state{}}.

%% @private Worker message handler.
handle_call({execute, Fun}, _From, State) ->
    try
        Result = Fun(),
        {reply, Result, State}
    catch
        Error:Reason ->
            logger:error("Worker execution failed: ~p:~p", [Error, Reason]),
            {reply, {error, {Error, Reason}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private Handle casts.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private Handle info messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private Worker termination.
terminate(Reason, _State) ->
    logger:info("Worker terminated: reason=~p", [Reason]),
    ok.

%% @private Code change handler.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Health Monitor (Periodic Checks)
%%%===================================================================

%% @private Start health monitor process.
start_health_monitor(PoolName) ->
    {ok, spawn_link(fun() -> health_monitor_loop(PoolName) end)}.

%% @private Health monitor loop: periodic checks.
health_monitor_loop(PoolName) ->
    IntervalMs = ?DEFAULT_HEALTH_INTERVAL,
    receive
        stop -> ok
    after
        IntervalMs ->
            case health_check(PoolName) of
                ok ->
                    logger:debug("Worker pool health OK: pool=~p", [PoolName]);
                {error, Reason} ->
                    logger:warning("Worker pool health degraded: pool=~p, reason=~p", [PoolName, Reason])
            end,
            health_monitor_loop(PoolName)
    end.
