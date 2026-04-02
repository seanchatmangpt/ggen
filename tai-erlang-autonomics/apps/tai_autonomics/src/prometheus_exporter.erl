%%%-------------------------------------------------------------------
%% @doc Prometheus Exporter - Export metrics in Prometheus format
%%      Integrates with Google Cloud Monitoring via Prometheus scrape
%%      Custom metrics: requests, errors, latency, memory, governors
%% @end
%%%-------------------------------------------------------------------

-module(prometheus_exporter).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, get_metrics/0, register_metric/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(COLLECTION_INTERVAL, 10000).  % 10 seconds

-record(state, {
    timer_ref :: reference() | undefined,
    metrics = #{} :: map(),
    collectors = [] :: list()
}).

%%%===================================================================
%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Get current metrics in Prometheus text format
-spec get_metrics() -> binary().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics_text).

%% @doc Register a metric collector function
-spec register_metric(Name :: binary(), Collector :: fun()) -> ok.
register_metric(Name, Collector) ->
    gen_server:call(?MODULE, {register_metric, Name, Collector}).

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, ScrapeInterval} = application:get_env(
        tai_autonomics,
        metrics_scrape_interval,
        ?COLLECTION_INTERVAL
    ),
    TimerRef = erlang:send_after(ScrapeInterval, self(), collect_metrics),

    State = #state{
        timer_ref = TimerRef,
        collectors = [
            {<<"http_requests_total">>, fun collect_request_metrics/0},
            {<<"http_request_duration_seconds">>, fun collect_latency_metrics/0},
            {<<"errors_total">>, fun collect_error_metrics/0},
            {<<"memory_bytes">>, fun collect_memory_metrics/0},
            {<<"governor_state_transitions_total">>, fun collect_governor_metrics/0},
            {<<"receipts_total">>, fun collect_receipt_metrics/0}
        ]
    },

    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_metrics_text, _From, State) ->
    MetricsText = render_metrics(State#state.collectors),
    {reply, MetricsText, State};

handle_call({register_metric, Name, Collector}, _From, State) ->
    NewCollectors = lists:keystore(Name, 1, State#state.collectors, {Name, Collector}),
    {reply, ok, State#state{collectors = NewCollectors}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    % Collect metrics and possibly export to remote system
    _Metrics = render_metrics(State#state.collectors),

    % Re-schedule collection
    TimerRef = erlang:send_after(?COLLECTION_INTERVAL, self(), collect_metrics),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Metric Collectors
%%%===================================================================

%% @private Collect HTTP request metrics
collect_request_metrics() ->
    {ok, History} = metrics_collector:get_metrics(),
    EndpointMetrics = group_by_endpoint(History),
    maps:fold(
        fun(Endpoint, Count, Acc) ->
            [
                format_metric(
                    <<"http_requests_total">>,
                    Count,
                    #{endpoint => Endpoint}
                )
                | Acc
            ]
        end,
        [],
        EndpointMetrics
    ).

%% @private Collect latency metrics (p50, p99)
collect_latency_metrics() ->
    {ok, History} = metrics_collector:get_metrics(),
    calculate_percentiles(History).

%% @private Collect error count metrics
collect_error_metrics() ->
    {ok, History} = metrics_collector:get_metrics(),
    ErrorCount = count_errors(History),
    [format_metric(<<"errors_total">>, ErrorCount, #{})].

%% @private Collect memory metrics
collect_memory_metrics() ->
    {memory, MemInfo} = erlang:process_info(self(), memory),
    [
        format_metric(<<"memory_bytes">>, MemInfo, #{type => <<"total">>}),
        format_metric(
            <<"memory_bytes">>,
            erlang:memory(heap_used),
            #{type => <<"heap_used">>}
        ),
        format_metric(
            <<"memory_bytes">>,
            erlang:memory(processes_used),
            #{type => <<"processes_used">>}
        )
    ].

%% @private Collect governor state transition metrics
collect_governor_metrics() ->
    case ets:lookup(tai_metrics, governor_transitions) of
        [{_, Transitions}] ->
            maps:fold(
                fun({Governor, FromState, ToState}, Count, Acc) ->
                    [
                        format_metric(
                            <<"governor_state_transitions_total">>,
                            Count,
                            #{
                                governor => Governor,
                                from_state => FromState,
                                to_state => ToState
                            }
                        )
                        | Acc
                    ]
                end,
                [],
                Transitions
            );
        [] -> []
    end.

%% @private Collect receipt metrics
collect_receipt_metrics() ->
    case ets:lookup(tai_metrics, receipts_generated) of
        [{_, Receipts}] ->
            maps:fold(
                fun({Type, Reason}, Count, Acc) ->
                    [
                        format_metric(
                            <<"receipts_total">>,
                            Count,
                            #{receipt_type => Type, reason => Reason}
                        )
                        | Acc
                    ]
                end,
                [],
                Receipts
            );
        [] -> []
    end.

%%%===================================================================
%% Rendering
%%%===================================================================

%% @private Render all metrics in Prometheus text format
-spec render_metrics(Collectors :: list()) -> binary().
render_metrics(Collectors) ->
    Lines = lists:flatmap(
        fun({_Name, Collector}) ->
            try
                Collector()
            catch
                _:_ -> []
            end
        end,
        Collectors
    ),
    erlang:iolist_to_binary(
        [Line ++ "\n" || Line <- Lines]
    ).

%% @private Format a metric in Prometheus text format
-spec format_metric(Name :: binary(), Value :: number(), Labels :: map()) -> string().
format_metric(Name, Value, Labels) ->
    LabelStr = format_labels(Labels),
    BinName = erlang:atom_to_binary(Name, utf8),
    case LabelStr of
        [] -> erlang:binary_to_list(BinName) ++ " " ++ format_value(Value);
        _ -> erlang:binary_to_list(BinName) ++ "{" ++ LabelStr ++ "} " ++ format_value(Value)
    end.

%% @private Format Prometheus labels
-spec format_labels(Labels :: map()) -> string().
format_labels(Labels) ->
    LabelPairs = maps:fold(
        fun(Key, Value, Acc) ->
            Pair = erlang:binary_to_list(Key) ++
                   "=\"" ++
                   erlang:binary_to_list(to_binary(Value)) ++
                   "\"",
            [Pair | Acc]
        end,
        [],
        Labels
    ),
    string:join(lists:reverse(LabelPairs), ",").

%% @private Format numeric value
-spec format_value(Value :: number()) -> string().
format_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]);
format_value(Value) when is_integer(Value) ->
    integer_to_list(Value).

%% @private Convert term to binary
-spec to_binary(Term :: term()) -> binary().
to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
to_binary(Term) -> erlang:term_to_binary(Term).

%%%===================================================================
%% Helpers
%%%===================================================================

%% @private Group metrics by endpoint
-spec group_by_endpoint(History :: list()) -> map().
group_by_endpoint(History) ->
    lists:foldl(
        fun(Snapshot, Acc) ->
            case maps:get(requests, Snapshot, #{}) of
                Requests when is_map(Requests) ->
                    maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, Acc, Requests);
                _ -> Acc
            end
        end,
        #{},
        History
    ).

%% @private Calculate percentile latencies
-spec calculate_percentiles(History :: list()) -> list().
calculate_percentiles(History) ->
    AllLatencies = lists:flatmap(
        fun(Snapshot) ->
            maps:get(latencies, Snapshot, [])
        end,
        History
    ),
    case AllLatencies of
        [] -> [];
        _ ->
            Sorted = lists:sort(AllLatencies),
            P50 = percentile(Sorted, 0.5),
            P99 = percentile(Sorted, 0.99),
            [
                format_metric(<<"http_request_duration_seconds">>, P50/1000, #{quantile => <<"0.5">>}),
                format_metric(<<"http_request_duration_seconds">>, P99/1000, #{quantile => <<"0.99">>})
            ]
    end.

%% @private Calculate percentile value
-spec percentile(Sorted :: list(), Quantile :: float()) -> number().
percentile(Sorted, Quantile) ->
    Index = max(1, round(Quantile * length(Sorted))),
    lists:nth(Index, Sorted).

%% @private Count errors in history
-spec count_errors(History :: list()) -> integer().
count_errors(History) ->
    lists:foldl(
        fun(Snapshot, Acc) ->
            case maps:get(errors, Snapshot, #{}) of
                Errors when is_map(Errors) ->
                    Acc + maps:fold(fun(_K, V, Sum) -> Sum + V end, 0, Errors);
                _ -> Acc
            end
        end,
        0,
        History
    ).
