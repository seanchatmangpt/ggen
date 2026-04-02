%%%-------------------------------------------------------------------
%%% @doc Jidoka Health Checker - Periodic System Health Verification
%%%
%%% Runs periodic health checks on all Jidoka components to ensure
%%% the system is operating within acceptable parameters. Provides
%%% automated alerts and recovery recommendations.
%%%
%%% Features:
%%% - Periodic liveness checks (every 30s)
%%% - Health grade: A (excellent) | B (good) | C (fair) | F (failing)
%%% - Automated alerts for degradation
%%% - Recovery recommendations
%%% - Metrics persistence (health history)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_health).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([get_health/0, get_history/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%% Health record
-record(health_check, {
    timestamp :: integer(),
    grade :: atom(),  % A | B | C | F
    score :: float(), % 0-100
    issues :: list(),
    recommendations :: list()
}).

-define(CHECK_INTERVAL_MS, 30000).  % 30 seconds
-define(HISTORY_SIZE, 144).         % Keep 12 hours of history (5min intervals)

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the health checker server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the health checker.
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Get current health status.
%% Returns: {Grade, Score, Issues, Recommendations}
-spec get_health() -> {atom(), float(), list(), list()} | {error, term()}.
get_health() ->
    try
        gen_server:call(?MODULE, get_health)
    catch
        _:_ -> {error, health_checker_not_running}
    end.

%% @doc Get health history.
%% Returns: List of health checks with timestamps
-spec get_history() -> list().
get_history() ->
    try
        gen_server:call(?MODULE, get_history)
    catch
        _:_ -> {error, health_checker_not_running}
    end.

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% @private Initialize health checker.
init([]) ->
    logger:info("Jidoka health checker started"),
    State = #{
        history => queue:new(),
        last_check => undefined
    },
    %% Schedule first check
    schedule_check(),
    {ok, State}.

%% @private Handle calls.
handle_call(get_health, _From, State) ->
    LastCheck = maps:get(last_check, State),
    case LastCheck of
        undefined ->
            {reply, {error, not_checked_yet}, State};
        #health_check{grade = Grade, score = Score, issues = Issues, recommendations = Recs} ->
            {reply, {Grade, Score, Issues, Recs}, State}
    end;

handle_call(get_history, _From, State) ->
    History = maps:get(history, State),
    HistoryList = queue:to_list(History),
    {reply, HistoryList, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private Handle casts.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private Handle info messages.
handle_info({check_health, _Ref}, State) ->
    %% Perform health check
    Check = perform_health_check(),

    %% Add to history (keep only recent checks)
    History = maps:get(history, State),
    NewHistory = case queue:len(History) >= ?HISTORY_SIZE of
        true ->
            {_, Q} = queue:out(History),
            queue:in(Check, Q);
        false ->
            queue:in(Check, History)
    end,

    %% Update last check
    NewState = State#{
        last_check => Check,
        history => NewHistory
    },

    %% Log if degradation detected
    case Check#health_check.grade of
        A when A =:= a ->
            logger:debug("Health: A (excellent), score ~.1f", [Check#health_check.score]);
        B when B =:= b ->
            logger:info("Health: B (good), score ~.1f", [Check#health_check.score]);
        C when C =:= c ->
            logger:warning("Health: C (fair), score ~.1f, issues: ~p",
                [Check#health_check.score, Check#health_check.issues]);
        F when F =:= f ->
            logger:error("Health: F (failing), score ~.1f, issues: ~p",
                [Check#health_check.score, Check#health_check.issues])
    end,

    %% Schedule next check
    schedule_check(),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private Cleanup on termination.
terminate(_Reason, _State) ->
    logger:info("Jidoka health checker stopped"),
    ok.

%% @private Code change handler.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Private Functions
%%%===================================================================

%% @private Schedule next health check.
schedule_check() ->
    Ref = erlang:send_after(?CHECK_INTERVAL_MS, self(), {check_health, erlang:make_ref()}),
    Ref.

%% @private Perform comprehensive health check.
perform_health_check() ->
    Timestamp = erlang:system_time(millisecond),

    %% Collect metrics
    {CBState, CBFailures, _} = jidoka_circuit_breaker:status(),
    {Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
    {available, Available} = jidoka_worker_pool:status(),
    PoolSize = 10,

    %% Calculate scores
    CBScore = score_circuit_breaker(CBState, CBFailures),
    RLScore = score_rate_limiter(Tokens, Accepted, Rejected),
    WPScore = score_worker_pool(Available, PoolSize),

    %% Overall score (weighted average)
    OverallScore = (CBScore + RLScore + WPScore) / 3.0,

    %% Determine grade
    Grade = case OverallScore of
        S when S >= 90 -> a;
        S when S >= 80 -> b;
        S when S >= 70 -> c;
        _ -> f
    end,

    %% Collect issues
    Issues = collect_issues(CBState, CBFailures, Tokens, Accepted, Rejected, Available, PoolSize),

    %% Generate recommendations
    Recommendations = generate_recommendations(Issues),

    #health_check{
        timestamp = Timestamp,
        grade = Grade,
        score = OverallScore,
        issues = Issues,
        recommendations = Recommendations
    }.

%% @private Score circuit breaker health (0-100).
score_circuit_breaker(closed, 0) -> 100.0;
score_circuit_breaker(closed, Failures) when Failures < 2 -> 95.0;
score_circuit_breaker(closed, _Failures) -> 80.0;
score_circuit_breaker(half_open, _) -> 60.0;
score_circuit_breaker(open, _) -> 0.0.

%% @private Score rate limiter health (0-100).
score_rate_limiter(Tokens, Accepted, Rejected) ->
    Total = Accepted + Rejected,
    case Total of
        0 -> 100.0;
        _ ->
            RejectionRate = (Rejected / Total) * 100,
            TokenScore = min(100.0, Tokens),
            CapacityScore = case RejectionRate of
                R when R < 5 -> 100.0;
                R when R < 15 -> 80.0;
                R when R < 25 -> 60.0;
                _ -> 40.0
            end,
            (TokenScore + CapacityScore) / 2.0
    end.

%% @private Score worker pool health (0-100).
score_worker_pool(Available, PoolSize) ->
    Utilization = 1.0 - (Available / PoolSize),
    case Utilization of
        U when U < 0.5 -> 100.0;
        U when U < 0.75 -> 95.0;
        U when U < 0.85 -> 80.0;
        U when U < 0.95 -> 60.0;
        _ -> 30.0
    end.

%% @private Collect issues found during health check.
collect_issues(CBState, CBFailures, Tokens, Accepted, Rejected, Available, PoolSize) ->
    Issues = [],

    %% Circuit breaker issues
    Issues1 = case CBState of
        open -> [circuit_breaker_open | Issues];
        _ -> Issues
    end,

    Issues2 = case CBFailures >= 3 of
        true -> [circuit_breaker_high_failure_rate | Issues1];
        false -> Issues1
    end,

    %% Rate limiter issues
    Total = Accepted + Rejected,
    RejectionRate = case Total of
        0 -> 0;
        _ -> Rejected / Total
    end,

    Issues3 = case RejectionRate > 0.25 of
        true -> [rate_limiter_high_rejection | Issues2];
        false -> Issues2
    end,

    Issues4 = case Tokens < 10 of
        true -> [rate_limiter_low_tokens | Issues3];
        false -> Issues3
    end,

    %% Worker pool issues
    Utilization = 1.0 - (Available / PoolSize),

    Issues5 = case Utilization > 0.95 of
        true -> [worker_pool_near_exhaustion | Issues4];
        false -> Issues4
    end,

    Issues6 = case Utilization > 0.85 of
        true -> [worker_pool_high_utilization | Issues5];
        false -> Issues5
    end,

    Issues6.

%% @private Generate recommendations based on issues.
generate_recommendations(Issues) ->
    Recommendations = [],

    %% Circuit breaker recommendations
    Recs1 = case lists:member(circuit_breaker_open, Issues) of
        true -> [
            "Circuit breaker is open - external service may be failing",
            "Investigate external service health and logs",
            "Reset circuit breaker after fixing root cause"
        ] ++ Recommendations;
        false -> Recommendations
    end,

    %% Rate limiter recommendations
    Recs2 = case lists:member(rate_limiter_high_rejection, Issues) of
        true -> [
            "Rate limiter is rejecting many requests",
            "Consider increasing rate_limit configuration",
            "Investigate if load is legitimate or due to retries"
        ] ++ Recs1;
        false -> Recs1
    end,

    %% Worker pool recommendations
    Recs3 = case lists:member(worker_pool_near_exhaustion, Issues) of
        true -> [
            "Worker pool is nearly exhausted",
            "Increase pool_size configuration immediately",
            "Investigate slow worker execution or deadlocks"
        ] ++ Recs2;
        false -> Recs2
    end,

    Recs4 = case lists:member(worker_pool_high_utilization, Issues) of
        true -> [
            "Worker pool utilization is high (>85%)",
            "Monitor closely and plan for scaling",
            "Consider optimizing worker execution time"
        ] ++ Recs3;
        false -> Recs3
    end,

    Recs4.
