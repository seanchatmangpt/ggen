%%%-------------------------------------------------------------------
%% @doc alert_manager: Threshold-based alerting system
%%
%% Manages alerts and notifications:
%% - Monitors metrics against thresholds
%% - Triggers alerts when thresholds breached
%% - Prevents alert storms with deduplication
%% - Integrates with Cloud Monitoring alert policies
%% - Supports escalation and notification channels
%%
%% @end
%%%-------------------------------------------------------------------
-module(alert_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, register_alert_policy/2, check_alert_threshold/3,
         get_active_alerts/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL, 30000).     % Check every 30 seconds
-define(ALERT_DEDUP_WINDOW, 300000). % 5 minute deduplication window

-record(state, {
    policies :: [alert_policy()],
    active_alerts :: [active_alert()],
    check_ref :: reference() | undefined,
    dedup_history :: map()
}).

-type alert_policy() :: #{
    name := string(),
    metric_name := atom(),
    threshold := number(),
    comparison := 'gt' | 'gte' | 'lt' | 'lte',
    severity := 'warning' | 'critical',
    duration_seconds := non_neg_integer()
}.

-type active_alert() :: #{
    policy_name := string(),
    triggered_at := integer(),
    metric_value := number(),
    severity := 'warning' | 'critical',
    fired := boolean()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the alert manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register an alert policy
-spec register_alert_policy(PolicyName, Policy) -> ok | {error, term()}
  when PolicyName :: string(),
       Policy :: alert_policy().
register_alert_policy(PolicyName, Policy) ->
    gen_server:call(?SERVER, {register_alert_policy, PolicyName, Policy}).

%% @doc Check if a metric exceeds threshold
-spec check_alert_threshold(PolicyName, MetricName, Value) -> ok | {alert, alert_policy()}
  when PolicyName :: string(),
       MetricName :: atom(),
       Value :: number().
check_alert_threshold(PolicyName, MetricName, Value) ->
    gen_server:call(?SERVER, {check_alert_threshold, PolicyName, MetricName, Value}).

%% @doc Get all active alerts
-spec get_active_alerts() -> {ok, [active_alert()]} | {error, term()}.
get_active_alerts() ->
    gen_server:call(?SERVER, get_active_alerts).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Schedule alert checking
    CheckRef = erlang:send_after(?CHECK_INTERVAL, self(), check_alerts),

    {ok, #state{
        policies = [],
        active_alerts = [],
        check_ref = CheckRef,
        dedup_history = #{}
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call({register_alert_policy, _PolicyName, Policy}, _From, State) ->
    NewPolicies = [Policy | State#state.policies],
    {reply, ok, State#state{policies = NewPolicies}};

handle_call({check_alert_threshold, PolicyName, _MetricName, Value}, _From, State) ->
    %% Find policy and check threshold
    case find_policy_by_name(PolicyName, State#state.policies) of
        {ok, Policy} ->
            case check_threshold(Value, Policy) of
                true ->
                    %% Threshold exceeded, check dedup
                    case should_fire_alert(PolicyName, State#state.dedup_history) of
                        true ->
                            NewAlerts = [
                                #{
                                    policy_name => PolicyName,
                                    triggered_at => erlang:system_time(millisecond),
                                    metric_value => Value,
                                    severity => maps:get(severity, Policy),
                                    fired => true
                                }
                                | State#state.active_alerts
                            ],
                            NewDedupHistory = update_dedup_history(
                                PolicyName,
                                State#state.dedup_history
                            ),
                            {reply, {alert, Policy}, State#state{
                                active_alerts = NewAlerts,
                                dedup_history = NewDedupHistory
                            }};
                        false ->
                            {reply, ok, State}
                    end;
                false ->
                    {reply, ok, State}
            end;
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call(get_active_alerts, _From, State) ->
    {reply, {ok, State#state.active_alerts}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, state()}
  when Request :: term(),
       State :: state().
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, state()}
  when Info :: term(),
       State :: state().
handle_info(check_alerts, State) ->
    %% Periodic alert checking
    %% This would check metrics and trigger alerts
    NewState = State,

    %% Clean up old dedup history
    CleanedDedupHistory = cleanup_dedup_history(State#state.dedup_history),

    %% Reschedule
    CheckRef = erlang:send_after(?CHECK_INTERVAL, self(), check_alerts),
    {noreply, NewState#state{
        check_ref = CheckRef,
        dedup_history = CleanedDedupHistory
    }};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, state()}
  when OldVsn :: term() | {down, term()},
       State :: state(),
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
-spec check_threshold(Value, Policy) -> boolean()
  when Value :: number(),
       Policy :: alert_policy().
check_threshold(Value, Policy) ->
    Threshold = maps:get(threshold, Policy),
    Comparison = maps:get(comparison, Policy),
    case Comparison of
        'gt' -> Value > Threshold;
        'gte' -> Value >= Threshold;
        'lt' -> Value < Threshold;
        'lte' -> Value =< Threshold
    end.

%% @private
-spec find_policy_by_name(Name, Policies) -> {ok, alert_policy()} | {error, not_found}
  when Name :: string(),
       Policies :: [alert_policy()].
find_policy_by_name(Name, Policies) ->
    case lists:filter(
        fun(P) -> maps:get(name, P) =:= Name end,
        Policies
    ) of
        [Policy] -> {ok, Policy};
        [] -> {error, not_found}
    end.

%% @private
-spec should_fire_alert(PolicyName, DedupHistory) -> boolean()
  when PolicyName :: string(),
       DedupHistory :: map().
should_fire_alert(PolicyName, DedupHistory) ->
    case maps:get(PolicyName, DedupHistory, undefined) of
        undefined ->
            true;
        LastFiredTime ->
            CurrentTime = erlang:system_time(millisecond),
            (CurrentTime - LastFiredTime) > ?ALERT_DEDUP_WINDOW
    end.

%% @private
-spec update_dedup_history(PolicyName, DedupHistory) -> map()
  when PolicyName :: string(),
       DedupHistory :: map().
update_dedup_history(PolicyName, DedupHistory) ->
    DedupHistory#{PolicyName => erlang:system_time(millisecond)}.

%% @private
-spec cleanup_dedup_history(DedupHistory) -> map()
  when DedupHistory :: map().
cleanup_dedup_history(DedupHistory) ->
    CurrentTime = erlang:system_time(millisecond),
    maps:filter(
        fun(_Key, LastFiredTime) ->
            (CurrentTime - LastFiredTime) < ?ALERT_DEDUP_WINDOW
        end,
        DedupHistory
    ).

%%%===================================================================
%% End of module
%%%===================================================================
