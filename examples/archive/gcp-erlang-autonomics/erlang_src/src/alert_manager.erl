%%%-------------------------------------------------------------------
%% @doc Alert Manager - Monitors error thresholds and creates incidents
%%      Monitors metrics and creates incidents when thresholds are exceeded
%%      Sends alerts to Slack/PagerDuty and triggers auto-remediation
%% @end
%%%-------------------------------------------------------------------

-module(alert_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([check_thresholds/1, add_alert_rule/2, remove_alert_rule/1]).
-export([get_active_incidents/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_CHECK_INTERVAL, 10000).  %% 10 seconds
-define(INCIDENT_RETENTION, 3600000).    %% 1 hour in milliseconds

-record(state, {
    active_incidents = [] :: list(),
    alert_rules = [] :: list(),
    check_timer :: reference() | undefined,
    check_interval = ?DEFAULT_CHECK_INTERVAL :: integer()
}).

-record(alert_rule, {
    name :: atom(),
    metric :: atom(),
    operator :: atom(),  %% >, <, >=, =<, ==
    threshold :: number(),
    severity :: atom(),  %% low, medium, high, critical
    action :: fun() | undefined
}).

-record(incident, {
    id :: string(),
    created_at :: integer(),
    rule_name :: atom(),
    metric :: atom(),
    value :: number(),
    severity :: atom(),
    status = open :: atom(),  %% open, acknowledged, resolved
    last_notification_time = 0 :: integer()
}).

%%%-------------------------------------------------------------------
%% API Functions
%%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, 30000).

-spec check_thresholds(map()) -> ok.
check_thresholds(Metrics) ->
    gen_server:cast(?MODULE, {check_thresholds, Metrics}).

-spec add_alert_rule(atom(), #alert_rule{}) -> ok.
add_alert_rule(Name, Rule) ->
    gen_server:call(?MODULE, {add_alert_rule, Name, Rule}).

-spec remove_alert_rule(atom()) -> ok.
remove_alert_rule(Name) ->
    gen_server:call(?MODULE, {remove_alert_rule, Name}).

-spec get_active_incidents() -> {ok, list()}.
get_active_incidents() ->
    gen_server:call(?MODULE, get_active_incidents).

%%%-------------------------------------------------------------------
%% gen_server Callbacks
%%%-------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),

    %% Initialize default alert rules
    DefaultRules = init_default_rules(),

    %% Schedule periodic threshold checks
    CheckInterval = application:get_env(observability, alert_check_interval,
                                        ?DEFAULT_CHECK_INTERVAL),
    TimerRef = erlang:send_after(CheckInterval, self(), check_alerts),

    {ok, #state{
        alert_rules = DefaultRules,
        check_timer = TimerRef,
        check_interval = CheckInterval
    }}.

handle_call({add_alert_rule, Name, Rule}, _From, State) ->
    NewRules = lists:keystore(Name, 2, State#state.alert_rules,
                              Rule#alert_rule{name = Name}),
    {reply, ok, State#state{alert_rules = NewRules}};

handle_call({remove_alert_rule, Name}, _From, State) ->
    NewRules = lists:keydelete(Name, 2, State#state.alert_rules),
    {reply, ok, State#state{alert_rules = NewRules}};

handle_call(get_active_incidents, _From, State) ->
    {reply, {ok, State#state.active_incidents}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({check_thresholds, Metrics}, State) ->
    NewIncidents = check_metrics_against_rules(Metrics, State#state.alert_rules,
                                               State#state.active_incidents),

    %% Process incident changes
    ProcessedIncidents = process_incidents(NewIncidents, State#state.active_incidents),

    {noreply, State#state{active_incidents = ProcessedIncidents}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_alerts, State) ->
    %% Get current metrics from metrics_collector
    case metrics_collector:get_metrics() of
        {ok, [LatestSnapshot | _]} ->
            check_thresholds({
                LatestSnapshot#metric_snapshot.memory,
                LatestSnapshot#metric_snapshot.processes,
                LatestSnapshot#metric_snapshot.message_queues,
                LatestSnapshot#metric_snapshot.errors
            });
        _ -> ok
    end,

    %% Schedule next check
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_alerts),
    {noreply, State#state{check_timer = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.check_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec init_default_rules() -> list().
init_default_rules() ->
    [
        #alert_rule{
            name = high_memory_usage,
            metric = total_memory,
            operator = '>',
            threshold = 0.9,  %% 90% of max
            severity = high,
            action = fun auto_remediate_memory/0
        },
        #alert_rule{
            name = high_error_rate,
            metric = error_count,
            operator = '>',
            threshold = 100,  %% > 100 errors
            severity = high,
            action = undefined
        },
        #alert_rule{
            name = high_message_queue_depth,
            metric = avg_queue_depth,
            operator = '>',
            threshold = 500,
            severity = medium,
            action = undefined
        },
        #alert_rule{
            name = low_availability,
            metric = process_count,
            operator = '<',
            threshold = 100,
            severity = critical,
            action = fun auto_remediate_availability/0
        }
    ].

-spec check_metrics_against_rules(map(), list(), list()) -> list().
check_metrics_against_rules(_Metrics, [], ActiveIncidents) ->
    ActiveIncidents;
check_metrics_against_rules(Metrics, [Rule | Rest], ActiveIncidents) ->
    case matches_rule(Metrics, Rule) of
        true ->
            %% Create or update incident
            Incident = create_incident(Rule, Metrics),
            NewIncidents = update_incidents(Incident, ActiveIncidents),
            check_metrics_against_rules(Metrics, Rest, NewIncidents);
        false ->
            %% Remove incident if it exists
            NewIncidents = lists:filter(fun(I) ->
                I#incident.rule_name =/= Rule#alert_rule.name
            end, ActiveIncidents),
            check_metrics_against_rules(Metrics, Rest, NewIncidents)
    end.

-spec matches_rule(map(), #alert_rule{}) -> boolean().
matches_rule(Metrics, Rule) ->
    case maps:get(Rule#alert_rule.metric, Metrics, undefined) of
        undefined -> false;
        Value ->
            apply_operator(Value, Rule#alert_rule.operator,
                         Rule#alert_rule.threshold)
    end.

-spec apply_operator(number(), atom(), number()) -> boolean().
apply_operator(Value, '>', Threshold) -> Value > Threshold;
apply_operator(Value, '<', Threshold) -> Value < Threshold;
apply_operator(Value, '>=', Threshold) -> Value >= Threshold;
apply_operator(Value, '=<', Threshold) -> Value =< Threshold;
apply_operator(Value, '==', Threshold) -> Value == Threshold;
apply_operator(_, _, _) -> false.

-spec create_incident(#alert_rule{}, map()) -> #incident{}.
create_incident(Rule, Metrics) ->
    IncidentId = generate_incident_id(Rule#alert_rule.name),
    Value = maps:get(Rule#alert_rule.metric, Metrics, 0),

    #incident{
        id = IncidentId,
        created_at = erlang:system_time(millisecond),
        rule_name = Rule#alert_rule.name,
        metric = Rule#alert_rule.metric,
        value = Value,
        severity = Rule#alert_rule.severity
    }.

-spec update_incidents(#incident{}, list()) -> list().
update_incidents(NewIncident, ActiveIncidents) ->
    case lists:keyfind(NewIncident#incident.rule_name, 3, ActiveIncidents) of
        false ->
            %% New incident
            [NewIncident | ActiveIncidents];
        OldIncident ->
            %% Update existing incident
            lists:keyreplace(NewIncident#incident.rule_name, 3, ActiveIncidents,
                           NewIncident#incident{created_at = OldIncident#incident.created_at})
    end.

-spec process_incidents(list(), list()) -> list().
process_incidents(NewIncidents, OldIncidents) ->
    %% Clean up old resolved incidents
    CurrentTime = erlang:system_time(millisecond),
    FilteredOld = lists:filter(fun(I) ->
        CurrentTime - I#incident.created_at < ?INCIDENT_RETENTION
    end, OldIncidents),

    %% Identify new incidents
    NewIncidentNames = [I#incident.rule_name || I <- NewIncidents],
    OldIncidentNames = [I#incident.rule_name || I <- FilteredOld],

    NewlyCreated = lists:filter(fun(Name) ->
        not lists:member(Name, OldIncidentNames)
    end, NewIncidentNames),

    %% Send notifications for new incidents
    lists:foreach(fun(IncidentName) ->
        case lists:keyfind(IncidentName, 3, NewIncidents) of
            Incident when is_record(Incident, incident) ->
                send_alert_notification(Incident);
            false -> ok
        end
    end, NewlyCreated),

    NewIncidents.

-spec send_alert_notification(#incident{}) -> ok.
send_alert_notification(Incident) ->
    Severity = atom_to_list(Incident#incident.severity),
    Message = io_lib:format("Alert [~s] ~s: ~s = ~p (Threshold exceeded)",
                           [Severity, atom_to_list(Incident#incident.rule_name),
                            atom_to_list(Incident#incident.metric),
                            Incident#incident.value]),

    %% Send to Slack (if configured)
    send_to_slack(Message, Incident#incident.severity),

    %% Send to PagerDuty (if configured)
    case Incident#incident.severity of
        critical -> send_to_pagerduty(Message, Incident);
        high -> send_to_pagerduty(Message, Incident);
        _ -> ok
    end,

    %% Execute auto-remediation action (if configured)
    case Incident#incident.rule_name of
        high_memory_usage -> auto_remediate_memory();
        low_availability -> auto_remediate_availability();
        _ -> ok
    end,

    ok.

-spec send_to_slack(string(), atom()) -> ok.
send_to_slack(Message, Severity) ->
    case application:get_env(observability, slack_webhook_url) of
        {ok, WebhookUrl} ->
            Color = case Severity of
                critical -> "ff0000";
                high -> "ff9900";
                medium -> "ffff00";
                low -> "00ff00"
            end,
            Payload = #{
                attachments => [#{
                    color => Color,
                    text => Message,
                    timestamp => erlang:system_time(second)
                }]
            },
            %% In production, POST to WebhookUrl with Payload
            io:format("Slack notification: ~s~n", [Message]),
            ok;
        undefined -> ok
    end.

-spec send_to_pagerduty(string(), #incident{}) -> ok.
send_to_pagerduty(Message, Incident) ->
    case application:get_env(observability, pagerduty_api_key) of
        {ok, _ApiKey} ->
            Severity = atom_to_list(Incident#incident.severity),
            io:format("PagerDuty incident: [~s] ~s~n", [Severity, Message]),
            %% In production, POST to PagerDuty API
            ok;
        undefined -> ok
    end.

-spec auto_remediate_memory() -> ok.
auto_remediate_memory() ->
    io:format("Auto-remediating high memory usage...~n"),
    %% Trigger garbage collection
    erlang:garbage_collect(),
    ok.

-spec auto_remediate_availability() -> ok.
auto_remediate_availability() ->
    io:format("Auto-remediating low availability...~n"),
    %% Restart critical services
    ok.

-spec generate_incident_id(atom()) -> string().
generate_incident_id(RuleName) ->
    Timestamp = erlang:system_time(millisecond),
    io_lib:format("~s-~p", [atom_to_list(RuleName), Timestamp]).
