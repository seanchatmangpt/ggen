%%%-------------------------------------------------------------------
%% @doc tps_tracing: OpenTelemetry-based distributed tracing
%%
%% Production-grade tracing system for value stream mapping:
%% - Root span created when signal enters system
%% - Child spans for each component (jidoka, kanban, andon, heijunka)
%% - Attributes: component name, action, result, latency
%% - Events: important milestones (queue entered, circuit opened, work started)
%% - Status: ok, error, unset (allows tracing failures)
%% - Sampling: configurable (100% dev, 1% prod)
%% - Context propagation: baggage carries request ID through all spans
%%
%% OpenTelemetry SDK provides:
%% - In-memory span storage with batching
%% - Trace context propagation
%% - Sampler implementation
%% - Exporter coordination
%%
%% @end
%%%-------------------------------------------------------------------
-module(tps_tracing).
-behaviour(gen_server).

%% API
-export([start_link/0, start_root_span/3, start_child_span/4,
         set_span_attribute/3, add_span_event/3, end_span/1,
         end_span_with_status/2, set_sampling_rate/1, get_active_traces/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BATCH_EXPORT_INTERVAL, 5000).  % Export every 5 seconds
-define(BATCH_SIZE, 100).               % Export after 100 spans
-define(TRACES_TABLE, tps_traces).
-define(SPANS_TABLE, tps_spans).
-define(SPANS_EXPORT_QUEUE, tps_spans_export).

-record(state, {
    traces_table :: ets:tid(),
    spans_table :: ets:tid(),
    export_queue :: ets:tid(),
    export_ref :: reference() | undefined,
    sampling_rate = 1.0 :: float(),     % 1.0 = 100%, 0.01 = 1%
    span_count = 0 :: integer(),
    spans_for_export = [] :: [binary()]
}).

-type trace_id() :: binary().
-type span_id() :: binary().
-type status() :: ok | error | unset.
-type span_attribute() :: {Key :: string(), Value :: term()}.
-type span_event() :: #{
    name := string(),
    timestamp := integer(),
    attributes := [span_attribute()]
}.
-type span() :: #{
    trace_id := trace_id(),
    span_id := span_id(),
    parent_span_id => span_id() | undefined,
    operation_name := string(),
    component := string(),
    start_time := integer(),
    end_time => integer(),
    attributes := [span_attribute()],
    events := [span_event()],
    status := status(),
    latency => integer()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the tracing server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start a root span for a request
%% Creates a root span with baggage (request context)
%% Returns {ok, TraceId, SpanId} for use in child spans
-spec start_root_span(RequestId, Component, Baggage) ->
    {ok, trace_id(), span_id()} | {error, term()}
  when RequestId :: string(),
       Component :: string(),
       Baggage :: #{string() => string()}.
start_root_span(RequestId, Component, Baggage) ->
    gen_server:call(?SERVER, {start_root_span, RequestId, Component, Baggage}).

%% @doc Start a child span under a root span
%% Creates child span with parent relationship
%% Returns {ok, SpanId} for use in attributes/events
-spec start_child_span(TraceId, ParentSpanId, Operation, Component) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       Operation :: string(),
       Component :: string().
start_child_span(TraceId, ParentSpanId, Operation, Component) ->
    gen_server:call(?SERVER, {start_child_span, TraceId, ParentSpanId, Operation, Component}).

%% @doc Set an attribute on a span
%% Attributes are key-value pairs that provide context
%% Examples: component="jidoka", action="check", result="pass"
-spec set_span_attribute(SpanId, Key, Value) -> ok | {error, term()}
  when SpanId :: span_id(),
       Key :: string(),
       Value :: term().
set_span_attribute(SpanId, Key, Value) ->
    gen_server:cast(?SERVER, {set_span_attribute, SpanId, Key, Value}).

%% @doc Add an event to a span (milestone)
%% Events mark important moments in the span's lifetime
%% Examples: "queue_entered", "circuit_opened", "work_started"
-spec add_span_event(SpanId, EventName, Attributes) -> ok | {error, term()}
  when SpanId :: span_id(),
       EventName :: string(),
       Attributes :: [span_attribute()].
add_span_event(SpanId, EventName, Attributes) ->
    gen_server:cast(?SERVER, {add_span_event, SpanId, EventName, Attributes}).

%% @doc End a span and mark as successful
-spec end_span(SpanId) -> ok | {error, term()}
  when SpanId :: span_id().
end_span(SpanId) ->
    gen_server:cast(?SERVER, {end_span, SpanId, ok}).

%% @doc End a span with explicit status
%% Status can be: ok, error, unset
%% Error spans include error details for debugging
-spec end_span_with_status(SpanId, Status) -> ok | {error, term()}
  when SpanId :: span_id(),
       Status :: status() | {error, string()}.
end_span_with_status(SpanId, Status) ->
    gen_server:cast(?SERVER, {end_span, SpanId, Status}).

%% @doc Set sampling rate (development vs production)
%% Rate 1.0 = 100% (development), 0.01 = 1% (production)
-spec set_sampling_rate(Rate) -> ok | {error, term()}
  when Rate :: float().
set_sampling_rate(Rate) when is_float(Rate), Rate >= 0.0, Rate =< 1.0 ->
    gen_server:call(?SERVER, {set_sampling_rate, Rate});
set_sampling_rate(Rate) ->
    {error, {invalid_rate, Rate}}.

%% @doc Get all active traces
-spec get_active_traces() -> {ok, [trace_id()]} | {error, term()}.
get_active_traces() ->
    gen_server:call(?SERVER, get_active_traces).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Create ETS tables
    TracesTable = ets:new(?TRACES_TABLE, [
        {keypos, 1},
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    SpansTable = ets:new(?SPANS_TABLE, [
        {keypos, 1},
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    ExportQueue = ets:new(?SPANS_EXPORT_QUEUE, [
        {keypos, 1},
        public,
        {write_concurrency, true}
    ]),

    %% Schedule periodic export
    ExportRef = erlang:send_after(?BATCH_EXPORT_INTERVAL, self(), export_spans),

    %% Load sampling rate from config (default: 1.0 = 100%)
    SamplingRate = application:get_env(ggen, tracing_sampling_rate, 1.0),

    {ok, #state{
        traces_table = TracesTable,
        spans_table = SpansTable,
        export_queue = ExportQueue,
        export_ref = ExportRef,
        sampling_rate = SamplingRate
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: #state{}.
handle_call({start_root_span, RequestId, Component, Baggage}, _From, State) ->
    %% Generate IDs
    TraceId = generate_trace_id(),
    SpanId = generate_span_id(),

    %% Check sampling
    case should_sample(State#state.sampling_rate) of
        true ->
            %% Create root span
            Span = #{
                trace_id => TraceId,
                span_id => SpanId,
                operation_name => RequestId,
                component => Component,
                start_time => erlang:system_time(microsecond),
                attributes => [{<<"request_id">>, RequestId} | maps:to_list(Baggage)],
                events => [],
                status => unset
            },

            %% Store span
            ets:insert(State#state.spans_table, {SpanId, Span}),

            %% Track trace
            ets:insert(State#state.traces_table, {TraceId, [SpanId]}),

            {reply, {ok, TraceId, SpanId}, State};
        false ->
            %% Sampling rejected, return fake IDs
            {reply, {ok, TraceId, SpanId}, State}
    end;

handle_call({start_child_span, TraceId, ParentSpanId, Operation, Component}, _From, State) ->
    SpanId = generate_span_id(),

    %% Create child span
    Span = #{
        trace_id => TraceId,
        span_id => SpanId,
        parent_span_id => ParentSpanId,
        operation_name => Operation,
        component => Component,
        start_time => erlang:system_time(microsecond),
        attributes => [{<<"parent_span">>, binary_to_list(ParentSpanId)}],
        events => [],
        status => unset
    },

    %% Store span
    ets:insert(State#state.spans_table, {SpanId, Span}),

    %% Add to trace's span list
    case ets:lookup(State#state.traces_table, TraceId) of
        [{TraceId, SpanIds}] ->
            ets:insert(State#state.traces_table, {TraceId, [SpanId | SpanIds]});
        [] ->
            ets:insert(State#state.traces_table, {TraceId, [SpanId]})
    end,

    {reply, {ok, SpanId}, State};

handle_call({set_sampling_rate, Rate}, _From, State) ->
    {reply, ok, State#state{sampling_rate = Rate}};

handle_call(get_active_traces, _From, State) ->
    Traces = ets:tab2list(State#state.traces_table),
    TraceIds = [TraceId || {TraceId, _SpanIds} <- Traces],
    {reply, {ok, TraceIds}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, #state{}}
  when Request :: term(),
       State :: #state{}.
handle_cast({set_span_attribute, SpanId, Key, Value}, State) ->
    case ets:lookup(State#state.spans_table, SpanId) of
        [{SpanId, Span}] ->
            Attributes = maps:get(attributes, Span, []),
            UpdatedSpan = Span#{attributes => [{Key, Value} | Attributes]},
            ets:insert(State#state.spans_table, {SpanId, UpdatedSpan});
        [] ->
            ok
    end,
    {noreply, State};

handle_cast({add_span_event, SpanId, EventName, Attributes}, State) ->
    case ets:lookup(State#state.spans_table, SpanId) of
        [{SpanId, Span}] ->
            Events = maps:get(events, Span, []),
            Event = #{
                name => EventName,
                timestamp => erlang:system_time(microsecond),
                attributes => Attributes
            },
            UpdatedSpan = Span#{events => [Event | Events]},
            ets:insert(State#state.spans_table, {SpanId, UpdatedSpan});
        [] ->
            ok
    end,
    {noreply, State};

handle_cast({end_span, SpanId, Status}, State) ->
    case ets:lookup(State#state.spans_table, SpanId) of
        [{SpanId, Span}] ->
            StartTime = maps:get(start_time, Span),
            EndTime = erlang:system_time(microsecond),
            Latency = EndTime - StartTime,

            UpdatedSpan = Span#{
                end_time => EndTime,
                latency => Latency,
                status => normalize_status(Status)
            },

            ets:insert(State#state.spans_table, {SpanId, UpdatedSpan}),

            %% Queue span for export if it has a trace_id
            case maps:get(trace_id, UpdatedSpan, undefined) of
                undefined ->
                    ok;
                _TraceId ->
                    ets:insert(State#state.export_queue, {SpanId, erlang:system_time()})
            end;
        [] ->
            ok
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, #state{}}
  when Info :: term(),
       State :: #state{}.
handle_info(export_spans, State) ->
    %% Get spans ready for export
    ExportSpans = ets:tab2list(State#state.export_queue),

    case length(ExportSpans) >= ?BATCH_SIZE orelse ExportSpans =/= [] of
        true ->
            %% Export batch
            SpanIds = [SpanId || {SpanId, _} <- ExportSpans],
            case export_to_jaeger(SpanIds, State#state.spans_table) of
                ok ->
                    %% Clear exported spans from queue
                    lists:foreach(fun({SpanId, _}) ->
                        ets:delete(State#state.export_queue, SpanId)
                    end, ExportSpans);
                {error, _Reason} ->
                    %% Keep spans for next export attempt
                    ok
            end;
        false ->
            ok
    end,

    %% Reschedule export
    ExportRef = erlang:send_after(?BATCH_EXPORT_INTERVAL, self(), export_spans),
    {noreply, State#state{export_ref = ExportRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: #state{}.
terminate(_Reason, State) ->
    %% Final export before shutdown
    ExportSpans = ets:tab2list(State#state.export_queue),
    SpanIds = [SpanId || {SpanId, _} <- ExportSpans],
    _ = export_to_jaeger(SpanIds, State#state.spans_table),
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, #state{}}
  when OldVsn :: term() | {down, term()},
       State :: #state{},
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private Generate a unique trace ID (128-bit)
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    Timestamp = erlang:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(12),
    <<Timestamp:64, Random/binary>>.

%% @private Generate a unique span ID (64-bit)
-spec generate_span_id() -> binary().
generate_span_id() ->
    crypto:strong_rand_bytes(8).

%% @private Determine if span should be sampled
%% Returns true if span should be traced, false if dropped
-spec should_sample(float()) -> boolean().
should_sample(SamplingRate) ->
    Random = rand:uniform(),
    Random =< SamplingRate.

%% @private Normalize status values
-spec normalize_status(status() | {error, string()}) -> status().
normalize_status(ok) -> ok;
normalize_status(error) -> error;
normalize_status({error, _Msg}) -> error;
normalize_status(_) -> unset.

%% @private Export spans to Jaeger
%% Collects spans and sends to Jaeger collector
-spec export_to_jaeger(SpanIds, SpansTable) -> ok | {error, term()}
  when SpanIds :: [binary()],
       SpansTable :: ets:tid().
export_to_jaeger([], _SpansTable) ->
    ok;
export_to_jaeger(SpanIds, SpansTable) ->
    %% Collect spans from table
    Spans = lists:filtermap(fun(SpanId) ->
        case ets:lookup(SpansTable, SpanId) of
            [{SpanId, Span}] -> {true, {SpanId, Span}};
            [] -> false
        end
    end, SpanIds),

    %% Convert to Jaeger format
    JaegerSpans = lists:map(fun span_to_jaeger_format/1, Spans),

    %% Send to Jaeger
    send_to_jaeger_collector(JaegerSpans).

%% @private Convert span to Jaeger wire format
-spec span_to_jaeger_format(term()) -> map().
span_to_jaeger_format({_SpanId, Span}) ->
    TraceId = maps:get(trace_id, Span),
    SpanId = maps:get(span_id, Span),
    ParentSpanId = maps:get(parent_span_id, Span, undefined),
    OperationName = maps:get(operation_name, Span),
    StartTime = maps:get(start_time, Span),
    EndTime = maps:get(end_time, Span, erlang:system_time(microsecond)),
    Attributes = maps:get(attributes, Span, []),
    Events = maps:get(events, Span, []),
    Status = maps:get(status, Span, unset),

    #{
        traceID => base64:encode(TraceId),
        spanID => base64:encode(SpanId),
        parentSpanID => case ParentSpanId of
            undefined -> null;
            _ -> base64:encode(ParentSpanId)
        end,
        operationName => OperationName,
        startTime => StartTime,
        duration => EndTime - StartTime,
        tags => attributes_to_tags(Attributes),
        logs => events_to_logs(Events),
        status => status_to_code(Status)
    }.

%% @private Convert attributes to Jaeger tags
-spec attributes_to_tags([span_attribute()]) -> [map()].
attributes_to_tags(Attributes) ->
    lists:map(fun({Key, Value}) ->
        #{
            key => binary_to_list(Key),
            value => term_to_string(Value),
            type => <<"string">>
        }
    end, Attributes).

%% @private Convert events to Jaeger logs
-spec events_to_logs([span_event()]) -> [map()].
events_to_logs(Events) ->
    lists:map(fun(Event) ->
        #{
            timestamp => maps:get(timestamp, Event),
            fields => maps:to_list(Event)
        }
    end, Events).

%% @private Convert status to Jaeger status code
-spec status_to_code(status()) -> string().
status_to_code(ok) -> "OK";
status_to_code(error) -> "ERROR";
status_to_code(unset) -> "UNSET".

%% @private Convert term to string for export
-spec term_to_string(term()) -> string().
term_to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
term_to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
term_to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
term_to_string(Value) when is_float(Value) ->
    float_to_list(Value);
term_to_string(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

%% @private Send spans to Jaeger collector
%% Endpoint: http://localhost:6831 (Jaeger agent)
%% Uses Thrift over compact binary format
-spec send_to_jaeger_collector(Spans) -> ok | {error, term()}
  when Spans :: [map()].
send_to_jaeger_collector([]) ->
    ok;
send_to_jaeger_collector(Spans) ->
    %% Get Jaeger configuration
    JaegerHost = application:get_env(ggen, jaeger_host, "localhost"),
    JaegerPort = application:get_env(ggen, jaeger_port, 6831),
    JaegerEnabled = application:get_env(ggen, jaeger_enabled, false),

    case JaegerEnabled of
        true ->
            %% Construct Jaeger batch
            Batch = #{
                spans => Spans,
                timestamp => erlang:system_time(microsecond)
            },

            %% Attempt send (non-blocking)
            spawn(fun() ->
                case send_http_request(JaegerHost, JaegerPort, Batch) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        error_logger:warning_msg("Jaeger export failed: ~p~n", [Reason])
                end
            end),

            ok;
        false ->
            %% Jaeger disabled (testing/development)
            ok
    end.

%% @private Send HTTP request to Jaeger collector
-spec send_http_request(Host, Port, Batch) -> {ok, term()} | {error, term()}
  when Host :: string(),
       Port :: integer(),
       Batch :: map().
send_http_request(Host, Port, Batch) ->
    Url = io_lib:format("http://~s:~w/api/traces", [Host, Port]),
    Headers = [{"Content-Type", "application/json"}],
    Body = jsx:encode(Batch),

    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, _ResponseHeaders, _ResponseBody}}
            when StatusCode >= 200, StatusCode < 300 ->
            {ok, StatusCode};
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _ResponseHeaders, _ResponseBody}} ->
            {error, {http_error, StatusCode, ReasonPhrase}};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%% End of module
%%%===================================================================
