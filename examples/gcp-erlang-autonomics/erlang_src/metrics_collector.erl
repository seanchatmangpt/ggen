%%%-------------------------------------------------------------------
%% @doc metrics_collector: Metrics collection and aggregation
%%
%% Manages system metrics:
%% - Governor decision counts and timings
%% - Receipt generation rates
%% - Error rates and types
%% - Throughput metrics
%% - Exports to Cloud Monitoring (Stackdriver)
%%
%% @end
%%%-------------------------------------------------------------------
-module(metrics_collector).
-behaviour(gen_server).

%% API
-export([start_link/0, record_metric/2, record_metric/3, get_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EXPORT_INTERVAL, 60000).  % Export every 60 seconds
-define(METRICS_TABLE, ggen_metrics).

-record(state, {
    metrics_table :: ets:tid(),
    export_ref :: reference() | undefined
}).

-type metric_name() :: atom().
-type metric_value() :: number().
-type metric_labels() :: #{string() => string()}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the metrics collector
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Record a metric with a value
-spec record_metric(MetricName, Value) -> ok
  when MetricName :: metric_name(),
       Value :: metric_value().
record_metric(MetricName, Value) ->
    record_metric(MetricName, Value, #{}).

%% @doc Record a metric with value and labels
-spec record_metric(MetricName, Value, Labels) -> ok
  when MetricName :: metric_name(),
       Value :: metric_value(),
       Labels :: metric_labels().
record_metric(MetricName, Value, Labels) ->
    gen_server:cast(?SERVER, {record_metric, MetricName, Value, Labels}).

%% @doc Get all collected metrics
-spec get_metrics() -> {ok, Metrics} | {error, term()}
  when Metrics :: [term()].
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Create ETS table for metrics
    TableId = ets:new(?METRICS_TABLE, [
        {keypos, 1},
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    %% Schedule periodic export
    ExportRef = erlang:send_after(?EXPORT_INTERVAL, self(), export_metrics),

    {ok, #state{
        metrics_table = TableId,
        export_ref = ExportRef
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call(get_metrics, _From, State) ->
    Metrics = ets:tab2list(State#state.metrics_table),
    {reply, {ok, Metrics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, state()}
  when Request :: term(),
       State :: state().
handle_cast({record_metric, MetricName, Value, Labels}, State) ->
    %% Store metric in ETS
    Key = {MetricName, Labels},
    ets:insert(State#state.metrics_table, {Key, Value, erlang:system_time()}),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, state()}
  when Info :: term(),
       State :: state().
handle_info(export_metrics, State) ->
    %% Export metrics to Cloud Monitoring
    Metrics = ets:tab2list(State#state.metrics_table),
    case export_to_cloud_monitoring(Metrics) of
        ok ->
            %% Clear exported metrics
            ets:delete_all_objects(State#state.metrics_table);
        {error, _Reason} ->
            %% Keep metrics for next export attempt
            ok
    end,

    %% Reschedule export
    ExportRef = erlang:send_after(?EXPORT_INTERVAL, self(), export_metrics),
    {noreply, State#state{export_ref = ExportRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, State) ->
    %% Final export before shutdown
    Metrics = ets:tab2list(State#state.metrics_table),
    _ = export_to_cloud_monitoring(Metrics),
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
-spec export_to_cloud_monitoring(Metrics) -> ok | {error, term()}
  when Metrics :: [term()].
export_to_cloud_monitoring([]) ->
    ok;
export_to_cloud_monitoring(Metrics) ->
    %% Convert metrics to Cloud Monitoring format
    CloudMetrics = lists:map(fun metric_to_cloud_format/1, Metrics),

    %% Send to Google Cloud Monitoring API
    case send_to_monitoring_api(CloudMetrics) of
        {ok, _Response} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @private
-spec metric_to_cloud_format(Metric) -> term()
  when Metric :: term().
metric_to_cloud_format({{MetricName, Labels}, Value, Timestamp}) ->
    #{
        metric_name => atom_to_string(MetricName),
        labels => Labels,
        value => Value,
        timestamp => Timestamp
    };
metric_to_cloud_format(Metric) ->
    Metric.

%% @private
-spec atom_to_string(Atom) -> string()
  when Atom :: atom().
atom_to_string(Atom) ->
    atom_to_list(Atom).

%% @private
-spec send_to_monitoring_api(Metrics) -> {ok, term()} | {error, term()}
  when Metrics :: [term()].
send_to_monitoring_api(_Metrics) ->
    %% Placeholder for actual Cloud Monitoring API call
    %% This would integrate with Google Cloud Monitoring client library
    {ok, #{status => <<"success">>}}.

%%%===================================================================
%% End of module
%%%===================================================================
