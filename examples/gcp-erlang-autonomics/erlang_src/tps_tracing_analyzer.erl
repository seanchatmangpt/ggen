%%%-------------------------------------------------------------------
%% @doc tps_tracing_analyzer: Value stream mapping through trace analysis
%%
%% Analyzes collected traces to identify waste (Lean Manufacturing):
%% - Transport: moving data between processes (latency)
%% - Processing: doing actual work (latency)
%% - Waiting: queueing for resources (queue latency)
%% - Defects: failed requests (error spans)
%%
%% Calculates:
%% - Total latency breakdown by component
%% - Which component is slowest (bottleneck)
%% - Is this request slower than normal? (anomaly detection)
%%
%% Value stream mapping identifies:
%% - Value-added time: actual useful work
%% - Non-value-added time: waiting, transport, defects
%% - Ratio helps identify improvement priorities
%%
%% @end
%%%-------------------------------------------------------------------
-module(tps_tracing_analyzer).
-behaviour(gen_server).

%% API
-export([start_link/0, analyze_trace/1, analyze_trace_batch/1,
         get_component_latencies/0, get_bottleneck/0,
         detect_anomaly/1, get_waste_report/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ANALYSIS_RETENTION, 86400000).  % 24 hours in milliseconds
-define(PERCENTILE_95, 0.95).           % P95 for anomaly detection

-record(state, {
    analyses_table :: ets:tid(),
    latencies_table :: ets:tid(),
    cleanup_ref :: reference() | undefined
}).

-record(trace_analysis, {
    trace_id :: binary(),
    timestamp :: integer(),
    total_latency :: integer(),
    component_latencies :: #{string() => integer()},
    waste_breakdown :: #{atom() => integer()},
    error_count :: integer(),
    is_anomaly :: boolean()
}).

-type trace_id() :: binary().
-type component_name() :: string().
-type waste_type() :: transport | processing | waiting | defect.
-type waste_report() :: #{
    total_latency => integer(),
    value_added_time => integer(),
    non_value_added_time => integer(),
    ratio => float(),
    waste_breakdown => #{waste_type() => integer()},
    bottleneck_component => string(),
    anomaly_detected => boolean()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the trace analyzer
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Analyze a single trace for value stream mapping
%% Returns waste breakdown and anomaly detection
-spec analyze_trace(TraceData) -> {ok, waste_report()} | {error, term()}
  when TraceData :: #{
    trace_id := trace_id(),
    spans := [map()],
    expected_latency => integer()
}.
analyze_trace(TraceData) ->
    gen_server:call(?SERVER, {analyze_trace, TraceData}).

%% @doc Analyze a batch of traces
%% Returns aggregated analysis
-spec analyze_trace_batch(Traces) -> {ok, [waste_report()]} | {error, term()}
  when Traces :: [map()].
analyze_trace_batch(Traces) ->
    gen_server:call(?SERVER, {analyze_batch, Traces}).

%% @doc Get average latencies by component
%% Returns {component_name, average_latency_us, percentile_95}
-spec get_component_latencies() -> {ok, [tuple()]} | {error, term()}.
get_component_latencies() ->
    gen_server:call(?SERVER, get_component_latencies).

%% @doc Get bottleneck component (slowest on average)
-spec get_bottleneck() -> {ok, {ComponentName, AvgLatency}} | {error, term()}
  when ComponentName :: string(),
       AvgLatency :: integer().
get_bottleneck() ->
    gen_server:call(?SERVER, get_bottleneck).

%% @doc Detect anomaly: is this trace slower than normal?
%% Uses P95 latency as threshold
-spec detect_anomaly(Trace) -> {ok, boolean(), Reason} | {error, term()}
  when Trace :: map(),
       Reason :: string().
detect_anomaly(Trace) ->
    gen_server:call(?SERVER, {detect_anomaly, Trace}).

%% @doc Get waste report for recent traces
%% Aggregates waste across all analyzed traces
-spec get_waste_report() -> {ok, waste_report()} | {error, term()}.
get_waste_report() ->
    gen_server:call(?SERVER, get_waste_report).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Create ETS tables
    AnalysesTable = ets:new(tps_analyses, [
        {keypos, 2},  % trace_id is key
        public,
        {write_concurrency, true}
    ]),

    LatenciesTable = ets:new(tps_latencies, [
        {keypos, 1},  % component_name is key
        public,
        {write_concurrency, true}
    ]),

    %% Schedule periodic cleanup
    CleanupRef = erlang:send_after(?ANALYSIS_RETENTION, self(), cleanup_old_analyses),

    {ok, #state{
        analyses_table = AnalysesTable,
        latencies_table = LatenciesTable,
        cleanup_ref = CleanupRef
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call({analyze_trace, TraceData}, _From, State) ->
    TraceId = maps:get(trace_id, TraceData),
    Spans = maps:get(spans, TraceData, []),
    ExpectedLatency = maps:get(expected_latency, TraceData, undefined),

    Report = do_analyze_trace(Spans, ExpectedLatency, State),

    %% Store analysis
    Analysis = #trace_analysis{
        trace_id = TraceId,
        timestamp = erlang:system_time(microsecond),
        total_latency = maps:get(total_latency, Report),
        component_latencies = maps:get(component_breakdown, Report, #{}),
        waste_breakdown = maps:get(waste_breakdown, Report, #{}),
        error_count = maps:get(error_count, Report, 0),
        is_anomaly = maps:get(anomaly_detected, Report, false)
    },
    ets:insert(State#state.analyses_table, Analysis),

    %% Update component latencies
    update_component_latencies(Report, State#state.latencies_table),

    {reply, {ok, Report}, State};

handle_call({analyze_batch, Traces}, _From, State) ->
    Reports = lists:map(fun(Trace) ->
        Spans = maps:get(spans, Trace, []),
        ExpectedLatency = maps:get(expected_latency, Trace, undefined),
        do_analyze_trace(Spans, ExpectedLatency, State)
    end, Traces),

    {reply, {ok, Reports}, State};

handle_call(get_component_latencies, _From, State) ->
    Latencies = ets:tab2list(State#state.latencies_table),
    FormattedLatencies = lists:map(fun({Component, AvgLatency, P95}) ->
        {Component, AvgLatency, P95}
    end, Latencies),
    {reply, {ok, FormattedLatencies}, State};

handle_call(get_bottleneck, _From, State) ->
    Latencies = ets:tab2list(State#state.latencies_table),
    case Latencies of
        [] ->
            {reply, {error, no_latencies_recorded}, State};
        _ ->
            %% Find component with highest average latency
            Bottleneck = lists:max_by(fun({_C1, L1, _P1}, {_C2, L2, _P2}) ->
                L1 >= L2
            end, Latencies),
            {Component, AvgLatency, _} = Bottleneck,
            {reply, {ok, {Component, AvgLatency}}, State}
    end;

handle_call({detect_anomaly, Trace}, _From, State) ->
    Spans = maps:get(spans, Trace, []),
    TraceLatency = calculate_total_latency(Spans),

    %% Get P95 latency from history
    P95Latency = get_p95_latency(State#state.latencies_table),

    case P95Latency of
        undefined ->
            {reply, {ok, false, "No baseline data"}, State};
        _ ->
            %% Anomaly if latency > P95 * 1.5
            IsAnomaly = TraceLatency > (P95Latency * 1.5),
            Reason = case IsAnomaly of
                true ->
                    io_lib:format("Latency ~p us exceeds P95 threshold ~p us",
                                [TraceLatency, round(P95Latency * 1.5)]);
                false ->
                    "Within normal latency range"
            end,
            {reply, {ok, IsAnomaly, Reason}, State}
    end;

handle_call(get_waste_report, _From, State) ->
    Analyses = ets:tab2list(State#state.analyses_table),
    Report = aggregate_waste_report(Analyses),
    {reply, {ok, Report}, State};

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
handle_info(cleanup_old_analyses, State) ->
    %% Remove analyses older than 24 hours
    CurrentTime = erlang:system_time(microsecond),
    Threshold = CurrentTime - (?ANALYSIS_RETENTION * 1000),

    %% Find and delete old records
    MatchSpec = [{
        #trace_analysis{timestamp = '$1', _ = '_'},
        [{'<', '$1', Threshold}],
        [true]
    }],
    ets:select_delete(State#state.analyses_table, MatchSpec),

    %% Reschedule cleanup
    CleanupRef = erlang:send_after(?ANALYSIS_RETENTION, self(), cleanup_old_analyses),

    {noreply, State#state{cleanup_ref = CleanupRef}};

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
%% Internal Analysis Functions
%%%===================================================================

%% @private Main trace analysis function
%% Calculates waste breakdown and detects anomalies
-spec do_analyze_trace(Spans, ExpectedLatency, State) -> waste_report()
  when Spans :: [map()],
       ExpectedLatency :: integer() | undefined,
       State :: state().
do_analyze_trace(Spans, ExpectedLatency, State) ->
    TotalLatency = calculate_total_latency(Spans),
    ComponentLatencies = calculate_component_latencies(Spans),
    WasteBreakdown = calculate_waste_breakdown(Spans),
    ErrorCount = count_errors(Spans),

    %% Detect anomaly
    P95Latency = get_p95_latency(State#state.latencies_table),
    IsAnomaly = case P95Latency of
        undefined -> false;
        _ -> TotalLatency > (P95Latency * 1.5)
    end,

    %% Calculate value-added vs non-value-added time
    ProcessingTime = maps:get(processing, WasteBreakdown, 0),
    ValueAddedTime = ProcessingTime,
    NonValueAddedTime = TotalLatency - ValueAddedTime,
    Ratio = case TotalLatency of
        0 -> 0.0;
        _ -> ValueAddedTime / TotalLatency
    end,

    %% Find bottleneck
    BottleneckComponent = case ComponentLatencies of
        {} -> "unknown";
        _ -> find_bottleneck_component(ComponentLatencies)
    end,

    #{
        trace_id => maps:get(trace_id, hd(Spans), undefined),
        total_latency => TotalLatency,
        component_breakdown => ComponentLatencies,
        value_added_time => ValueAddedTime,
        non_value_added_time => NonValueAddedTime,
        ratio => Ratio,
        waste_breakdown => WasteBreakdown,
        error_count => ErrorCount,
        bottleneck_component => BottleneckComponent,
        anomaly_detected => IsAnomaly,
        expected_latency => ExpectedLatency
    }.

%% @private Calculate total trace latency
%% From earliest start to latest end
-spec calculate_total_latency(Spans) -> integer()
  when Spans :: [map()].
calculate_total_latency([]) ->
    0;
calculate_total_latency(Spans) ->
    StartTimes = [maps:get(start_time, S, erlang:system_time(microsecond)) || S <- Spans],
    EndTimes = [maps:get(end_time, S, erlang:system_time(microsecond)) || S <- Spans],

    MinStart = lists:min(StartTimes),
    MaxEnd = lists:max(EndTimes),

    max(1, MaxEnd - MinStart).

%% @private Calculate latency per component
-spec calculate_component_latencies(Spans) -> #{string() => integer()}.
calculate_component_latencies(Spans) ->
    ComponentMap = maps:new(),
    lists:foldl(fun(Span, Acc) ->
        Component = maps:get(component, Span, "unknown"),
        Latency = maps:get(latency, Span, 0),
        ExistingLatency = maps:get(Component, Acc, 0),
        Acc#{Component => ExistingLatency + Latency}
    end, ComponentMap, Spans).

%% @private Calculate waste breakdown (Lean Manufacturing)
%% Transport, Processing, Waiting, Defects
-spec calculate_waste_breakdown(Spans) -> #{atom() => integer()}.
calculate_waste_breakdown(Spans) ->
    lists:foldl(fun(Span, Acc) ->
        Component = maps:get(component, Span, "unknown"),
        Latency = maps:get(latency, Span, 0),
        Status = maps:get(status, Span, ok),

        %% Classify waste type by component
        WasteType = case Component of
            "kanban" -> waiting;           % Queue waiting
            "heijunka" -> waiting;          % Resource pool waiting
            "action" -> processing;         % Actual work
            "jidoka" -> transport;          % Circuit check (network latency)
            _ -> transport
        end,

        %% Handle errors
        FinalWasteType = case Status of
            error -> defect;
            _ -> WasteType
        end,

        CurrentValue = maps:get(FinalWasteType, Acc, 0),
        Acc#{FinalWasteType => CurrentValue + Latency}
    end, #{}, Spans).

%% @private Count error spans in trace
-spec count_errors(Spans) -> integer().
count_errors(Spans) ->
    length([S || S <- Spans, maps:get(status, S, ok) == error]).

%% @private Find bottleneck (slowest component)
-spec find_bottleneck_component(ComponentLatencies) -> string()
  when ComponentLatencies :: #{string() => integer()}.
find_bottleneck_component(ComponentLatencies) ->
    case maps:to_list(ComponentLatencies) of
        [] -> "unknown";
        List ->
            {Component, _Latency} = lists:max_by(fun({_C1, L1}, {_C2, L2}) ->
                L1 >= L2
            end, List),
            Component
    end.

%% @private Update component latency statistics
-spec update_component_latencies(Report, LatenciesTable) -> ok
  when Report :: waste_report(),
       LatenciesTable :: ets:tid().
update_component_latencies(Report, LatenciesTable) ->
    ComponentLatencies = maps:get(component_breakdown, Report, #{}),
    maps:foreach(fun(Component, Latency) ->
        case ets:lookup(LatenciesTable, Component) of
            [{Component, AvgLatency, P95}] ->
                %% Update running average
                NewAvg = (AvgLatency + Latency) / 2,
                NewP95 = max(Latency, P95),  % Simplified P95
                ets:insert(LatenciesTable, {Component, NewAvg, NewP95});
            [] ->
                ets:insert(LatenciesTable, {Component, Latency, Latency})
        end
    end, ComponentLatencies),
    ok.

%% @private Get P95 latency from recorded data
-spec get_p95_latency(LatenciesTable) -> integer() | undefined
  when LatenciesTable :: ets:tid().
get_p95_latency(LatenciesTable) ->
    case ets:tab2list(LatenciesTable) of
        [] ->
            undefined;
        Latencies ->
            P95Values = [P95 || {_Component, _Avg, P95} <- Latencies],
            case P95Values of
                [] -> undefined;
                _ -> lists:max(P95Values)
            end
    end.

%% @private Aggregate waste report across all analyses
-spec aggregate_waste_report(Analyses) -> waste_report()
  when Analyses :: [#trace_analysis{}].
aggregate_waste_report([]) ->
    #{
        total_latency => 0,
        value_added_time => 0,
        non_value_added_time => 0,
        ratio => 0.0,
        waste_breakdown => #{},
        bottleneck_component => "unknown",
        anomaly_detected => false
    };
aggregate_waste_report(Analyses) ->
    %% Aggregate across analyses
    TotalLatencies = [A#trace_analysis.total_latency || A <- Analyses],
    AvgLatency = case TotalLatencies of
        [] -> 0;
        _ -> round(lists:sum(TotalLatencies) / length(TotalLatencies))
    end,

    %% Aggregate waste
    AllWaste = lists:foldl(fun(A, Acc) ->
        maps:fold(fun(Type, Value, InnerAcc) ->
            Current = maps:get(Type, InnerAcc, 0),
            InnerAcc#{Type => Current + Value}
        end, Acc, A#trace_analysis.waste_breakdown)
    end, #{}, Analyses),

    %% Aggregate component latencies
    AllComponents = lists:foldl(fun(A, Acc) ->
        maps:fold(fun(Component, Latency, InnerAcc) ->
            Current = maps:get(Component, InnerAcc, 0),
            InnerAcc#{Component => Current + Latency}
        end, Acc, A#trace_analysis.component_latencies)
    end, #{}, Analyses),

    BottleneckComponent = find_bottleneck_component(AllComponents),
    AnomalyCount = length([A || A <- Analyses, A#trace_analysis.is_anomaly]),

    #{
        total_latency => AvgLatency,
        value_added_time => maps:get(processing, AllWaste, 0),
        non_value_added_time => maps:get(waiting, AllWaste, 0) + maps:get(transport, AllWaste, 0),
        ratio => case AvgLatency of
            0 -> 0.0;
            _ -> maps:get(processing, AllWaste, 0) / AvgLatency
        end,
        waste_breakdown => AllWaste,
        bottleneck_component => BottleneckComponent,
        anomaly_detected => AnomalyCount > 0
    }.

%%%===================================================================
%% End of module
%%%===================================================================
