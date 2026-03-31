%%%-------------------------------------------------------------------
%% @doc Metrics Collector - Collects BEAM cluster metrics
%%      Publishes to GCP Cloud Monitoring every 10 seconds
%%      Tracks governor state transitions, message queues, memory, errors
%% @end
%%%-------------------------------------------------------------------

-module(metrics_collector).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([collect_metrics/0, get_metrics/0, register_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal
-export([collect_memory_metrics/0, collect_process_metrics/0,
         collect_message_queue_metrics/0, collect_error_metrics/0]).

-define(COLLECTION_INTERVAL, 10000).  %% 10 seconds
-define(GCP_METRICS_ENDPOINT, "https://monitoring.googleapis.com/v3/projects").

-record(state, {
    timer_ref :: reference() | undefined,
    metrics_history = [] :: list(),
    max_history = 1000 :: integer(),
    handlers = [] :: list(),
    error_count = 0 :: integer(),
    last_collection_time = 0 :: integer()
}).

-record(metric_snapshot, {
    timestamp :: integer(),
    node :: atom(),
    memory :: map(),
    processes :: map(),
    message_queues :: map(),
    governors :: map(),
    errors :: map()
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

-spec collect_metrics() -> {ok, map()} | {error, term()}.
collect_metrics() ->
    gen_server:call(?MODULE, collect_now, 30000).

-spec get_metrics() -> {ok, list()} | {error, term()}.
get_metrics() ->
    gen_server:call(?MODULE, get_history, 30000).

-spec register_handler(fun()) -> ok.
register_handler(Handler) when is_function(Handler) ->
    gen_server:call(?MODULE, {register_handler, Handler}).

%%%-------------------------------------------------------------------
%% gen_server Callbacks
%%%-------------------------------------------------------------------

-spec init(Args) -> {ok, State}
  when Args :: [],
       State :: #state{}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, ScrapeInterval} = application:get_env(observability,
                                               metrics_scrape_interval,
                                               ?COLLECTION_INTERVAL),
    TimerRef = erlang:send_after(ScrapeInterval, self(), collect),

    {ok, #state{
        timer_ref = TimerRef,
        metrics_history = [],
        max_history = 1000,
        handlers = []
    }}.

-spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
  when Request :: collect_now | get_history | {register_handler, fun()} | stop,
       From :: {pid(), term()},
       State :: #state{},
       Reply :: term(),
       Reason :: normal.
handle_call(collect_now, _From, State) ->
    Snapshot = perform_collection(State),
    {reply, {ok, Snapshot}, State};

handle_call(get_history, _From, State) ->
    {reply, {ok, State#state.metrics_history}, State};

handle_call({register_handler, Handler}, _From, State) ->
    NewHandlers = [Handler | State#state.handlers],
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(Msg, State) -> {noreply, State}
  when Msg :: term(),
       State :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, State}
  when Info :: term(),
       State :: #state{}.
handle_info(collect, State) ->
    %% Perform metrics collection
    Snapshot = perform_collection(State),

    %% Update history
    History = [Snapshot | State#state.metrics_history],
    TrimmedHistory = lists:sublist(History, State#state.max_history),

    %% Notify handlers
    notify_handlers(Snapshot, State#state.handlers),

    %% Publish to GCP
    publish_to_gcp(Snapshot),

    %% Schedule next collection
    {ok, ScrapeInterval} = application:get_env(observability,
                                               metrics_scrape_interval,
                                               ?COLLECTION_INTERVAL),
    NewTimerRef = erlang:send_after(ScrapeInterval, self(), collect),

    NewState = State#state{
        timer_ref = NewTimerRef,
        metrics_history = TrimmedHistory,
        last_collection_time = erlang:system_time(millisecond)
    },
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok
  when Reason :: term(),
       State :: #state{}.
terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

-spec code_change(OldVsn, State, Extra) -> {ok, State}
  when OldVsn :: term(),
       State :: #state{},
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec perform_collection(#state{}) -> #metric_snapshot{}.
perform_collection(_State) ->
    Timestamp = erlang:system_time(millisecond),
    Node = node(),

    MemoryMetrics = collect_memory_metrics(),
    ProcessMetrics = collect_process_metrics(),
    MessageQueueMetrics = collect_message_queue_metrics(),
    GovernorMetrics = collect_governor_metrics(),
    ErrorMetrics = collect_error_metrics(),

    #metric_snapshot{
        timestamp = Timestamp,
        node = Node,
        memory = MemoryMetrics,
        processes = ProcessMetrics,
        message_queues = MessageQueueMetrics,
        governors = GovernorMetrics,
        errors = ErrorMetrics
    }.

-spec collect_memory_metrics() -> map().
collect_memory_metrics() ->
    MemInfo = erlang:memory(),
    #{
        total => proplists:get_value(total, MemInfo, 0),
        processes => proplists:get_value(processes, MemInfo, 0),
        system => proplists:get_value(system, MemInfo, 0),
        atom => proplists:get_value(atom, MemInfo, 0),
        binary => proplists:get_value(binary, MemInfo, 0),
        code => proplists:get_value(code, MemInfo, 0),
        ets => proplists:get_value(ets, MemInfo, 0),
        maximum => erlang:system_info(max_heap_size)
    }.

-spec collect_process_metrics() -> map().
collect_process_metrics() ->
    ProcessCount = erlang:system_info(process_count),
    RunQueue = erlang:statistics(run_queue),
    {_, UptimeSec} = erlang:statistics(wallclock),

    #{
        count => ProcessCount,
        limit => erlang:system_info(process_limit),
        run_queue => RunQueue,
        uptime_seconds => UptimeSec div 1000,
        scheduler_count => erlang:system_info(schedulers),
        online_schedulers => erlang:system_info(schedulers_online)
    }.

-spec collect_message_queue_metrics() -> map().
collect_message_queue_metrics() ->
    AllProcesses = erlang:processes(),
    QueueDepths = lists:map(fun(Pid) ->
        {status, Pid, _, [_, _, _, _, Status]} = sys:get_status(Pid),
        case proplists:get_value(message_queue_len, Status, 0) of
            Len when is_integer(Len) -> Len;
            _ -> 0
        end
    end, AllProcesses),

    TotalQueueDepth = lists:sum(QueueDepths),
    AvgQueueDepth = case length(QueueDepths) of
        0 -> 0;
        Count -> TotalQueueDepth div Count
    end,
    MaxQueueDepth = case QueueDepths of
        [] -> 0;
        _ -> lists:max(QueueDepths)
    end,

    #{
        total_depth => TotalQueueDepth,
        average_depth => AvgQueueDepth,
        max_depth => MaxQueueDepth,
        process_count => length(QueueDepths),
        high_load_processes => count_high_load_processes(QueueDepths, 100)
    }.

-spec collect_governor_metrics() -> map().
collect_governor_metrics() ->
    %% Collect metrics from governor modules if they exist
    case whereis(billing_governor) of
        undefined -> #{billing => not_running};
        BillingPid ->
            BillingState = try sys:get_status(BillingPid) of
                {status, _, _, [_, _, _, _, State]} -> State
            catch _:_ -> []
            end,
            #{billing => #{
                pid => BillingPid,
                state => proplists:get_value(state, BillingState, unknown),
                cost_accumulated => proplists:get_value(cost_accumulated, BillingState, 0.0)
            }}
    end.

-spec collect_error_metrics() -> map().
collect_error_metrics() ->
    %% Collect error statistics from error_logger
    case catch error_logger:get_stat() of
        {_, {ErrorCount, WarningCount}} ->
            #{
                errors => ErrorCount,
                warnings => WarningCount,
                total => ErrorCount + WarningCount
            };
        _ ->
            #{errors => 0, warnings => 0, total => 0}
    end.

-spec count_high_load_processes(list(), integer()) -> integer().
count_high_load_processes(QueueDepths, Threshold) ->
    length([D || D <- QueueDepths, D > Threshold]).

-spec notify_handlers(#metric_snapshot{}, list()) -> ok.
notify_handlers(Snapshot, Handlers) ->
    lists:foreach(fun(Handler) ->
        try Handler(Snapshot)
        catch _:_ -> ok
        end
    end, Handlers).

-spec publish_to_gcp(#metric_snapshot{}) -> ok.
publish_to_gcp(Snapshot) ->
    %% Publish metrics to GCP Cloud Monitoring
    case application:get_env(observability, gcp_project_id) of
        {ok, ProjectId} ->
            publish_memory_metrics(ProjectId, Snapshot),
            publish_process_metrics(ProjectId, Snapshot),
            publish_queue_metrics(ProjectId, Snapshot),
            publish_governor_metrics(ProjectId, Snapshot),
            publish_error_metrics(ProjectId, Snapshot),
            ok;
        undefined ->
            ok  %% GCP not configured
    end.

-spec publish_memory_metrics(string(), #metric_snapshot{}) -> ok.
publish_memory_metrics(_ProjectId, Snapshot) ->
    MemMetrics = Snapshot#metric_snapshot.memory,
    MetricData = #{
        metric => "beam.memory.total",
        value => maps:get(total, MemMetrics, 0),
        labels => #{
            node => atom_to_list(Snapshot#metric_snapshot.node)
        }
    },
    publish_metric(MetricData).

-spec publish_process_metrics(string(), #metric_snapshot{}) -> ok.
publish_process_metrics(_ProjectId, Snapshot) ->
    ProcMetrics = Snapshot#metric_snapshot.processes,
    PublishProcessCount = #{
        metric => "beam.processes.count",
        value => maps:get(count, ProcMetrics, 0),
        labels => #{node => atom_to_list(Snapshot#metric_snapshot.node)}
    },
    publish_metric(PublishProcessCount).

-spec publish_queue_metrics(string(), #metric_snapshot{}) -> ok.
publish_queue_metrics(_ProjectId, Snapshot) ->
    QueueMetrics = Snapshot#metric_snapshot.message_queues,
    PublishQueueDepth = #{
        metric => "beam.message_queue.average_depth",
        value => maps:get(average_depth, QueueMetrics, 0),
        labels => #{node => atom_to_list(Snapshot#metric_snapshot.node)}
    },
    publish_metric(PublishQueueDepth).

-spec publish_governor_metrics(string(), #metric_snapshot{}) -> ok.
publish_governor_metrics(_ProjectId, Snapshot) ->
    GovernorMetrics = Snapshot#metric_snapshot.governors,
    case maps:get(billing, GovernorMetrics, undefined) of
        #{cost_accumulated := Cost} ->
            MetricData = #{
                metric => "beam.billing_governor.cost_accumulated",
                value => Cost,
                labels => #{node => atom_to_list(Snapshot#metric_snapshot.node)}
            },
            publish_metric(MetricData);
        _ -> ok
    end.

-spec publish_error_metrics(string(), #metric_snapshot{}) -> ok.
publish_error_metrics(_ProjectId, Snapshot) ->
    ErrorMetrics = Snapshot#metric_snapshot.errors,
    ErrorCount = maps:get(errors, ErrorMetrics, 0),
    MetricData = #{
        metric => "beam.errors.total",
        value => ErrorCount,
        labels => #{node => atom_to_list(Snapshot#metric_snapshot.node)}
    },
    publish_metric(MetricData).

-spec publish_metric(map()) -> ok.
publish_metric(MetricData) ->
    %% In production, this would call GCP Cloud Monitoring API
    %% For now, just log it
    io:format("Metric: ~p~n", [MetricData]),
    ok.
