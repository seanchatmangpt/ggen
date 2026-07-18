%%%-------------------------------------------------------------------
%% @doc trace_handler: Distributed tracing integration
%%
%% Manages distributed tracing:
%% - Trace span creation and completion
%% - Parent-child span relationships
%% - Trace context propagation
%% - Export to Cloud Trace (Stackdriver)
%% - Latency tracking for governance decisions
%%
%% @end
%%%-------------------------------------------------------------------
-module(trace_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, start_span/2, end_span/1, set_span_attribute/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EXPORT_INTERVAL, 30000).  % Export every 30 seconds
-define(TRACES_TABLE, ggen_traces).
-define(SPANS_TABLE, ggen_spans).

-record(state, {
    traces_table :: ets:tid(),
    spans_table :: ets:tid(),
    export_ref :: reference() | undefined
}).

-type trace_id() :: binary().
-type span_id() :: binary().
-type span() :: #{
    trace_id := trace_id(),
    span_id := span_id(),
    parent_span_id => span_id() | undefined,
    operation_name := string(),
    start_time := integer(),
    end_time => integer(),
    attributes := map(),
    status := pending | success | error
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the trace handler
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start a new span
-spec start_span(OperationName, ParentSpanId) -> {ok, SpanId} | {error, term()}
  when OperationName :: string(),
       ParentSpanId :: span_id() | undefined,
       SpanId :: span_id().
start_span(OperationName, ParentSpanId) ->
    gen_server:call(?SERVER, {start_span, OperationName, ParentSpanId}).

%% @doc End a span and mark it as complete
-spec end_span(SpanId) -> ok | {error, term()}
  when SpanId :: span_id().
end_span(SpanId) ->
    gen_server:cast(?SERVER, {end_span, SpanId}).

%% @doc Set an attribute on a span
-spec set_span_attribute(SpanId, Key, Value) -> ok | {error, term()}
  when SpanId :: span_id(),
       Key :: string(),
       Value :: term().
set_span_attribute(SpanId, Key, Value) ->
    gen_server:cast(?SERVER, {set_span_attribute, SpanId, Key, Value}).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Create ETS tables for traces and spans
    TracesTable = ets:new(?TRACES_TABLE, [
        {keypos, 1},
        public,
        {write_concurrency, true}
    ]),

    SpansTable = ets:new(?SPANS_TABLE, [
        {keypos, 1},
        public,
        {write_concurrency, true}
    ]),

    %% Schedule periodic export
    ExportRef = erlang:send_after(?EXPORT_INTERVAL, self(), export_traces),

    {ok, #state{
        traces_table = TracesTable,
        spans_table = SpansTable,
        export_ref = ExportRef
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call({start_span, OperationName, ParentSpanId}, _From, State) ->
    %% Generate unique IDs
    TraceId = generate_trace_id(),
    SpanId = generate_span_id(),

    %% Create span
    Span = #{
        trace_id => TraceId,
        span_id => SpanId,
        parent_span_id => ParentSpanId,
        operation_name => OperationName,
        start_time => erlang:system_time(microsecond),
        attributes => #{},
        status => pending
    },

    %% Store span
    ets:insert(State#state.spans_table, {SpanId, Span}),

    {reply, {ok, SpanId}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, state()}
  when Request :: term(),
       State :: state().
handle_cast({end_span, SpanId}, State) ->
    %% Update span with end time and mark as complete
    case ets:lookup(State#state.spans_table, SpanId) of
        [{SpanId, Span}] ->
            UpdatedSpan = Span#{
                end_time => erlang:system_time(microsecond),
                status => success
            },
            ets:insert(State#state.spans_table, {SpanId, UpdatedSpan});
        [] ->
            ok
    end,
    {noreply, State};

handle_cast({set_span_attribute, SpanId, Key, Value}, State) ->
    %% Add attribute to span
    case ets:lookup(State#state.spans_table, SpanId) of
        [{SpanId, Span}] ->
            Attributes = maps:get(attributes, Span, #{}),
            UpdatedSpan = Span#{attributes => Attributes#{Key => Value}},
            ets:insert(State#state.spans_table, {SpanId, UpdatedSpan});
        [] ->
            ok
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, state()}
  when Info :: term(),
       State :: state().
handle_info(export_traces, State) ->
    %% Export traces to Cloud Trace
    Spans = ets:tab2list(State#state.spans_table),
    case export_to_cloud_trace(Spans) of
        ok ->
            %% Clear exported spans
            ets:delete_all_objects(State#state.spans_table);
        {error, _Reason} ->
            %% Keep spans for next export attempt
            ok
    end,

    %% Reschedule export
    ExportRef = erlang:send_after(?EXPORT_INTERVAL, self(), export_traces),
    {noreply, State#state{export_ref = ExportRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, State) ->
    %% Final export before shutdown
    Spans = ets:tab2list(State#state.spans_table),
    _ = export_to_cloud_trace(Spans),
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
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    crypto:strong_rand_bytes(16).

%% @private
-spec generate_span_id() -> binary().
generate_span_id() ->
    crypto:strong_rand_bytes(8).

%% @private
-spec export_to_cloud_trace(Spans) -> ok | {error, term()}
  when Spans :: [term()].
export_to_cloud_trace([]) ->
    ok;
export_to_cloud_trace(Spans) ->
    %% Convert spans to Cloud Trace format
    CloudSpans = lists:map(fun span_to_cloud_format/1, Spans),

    %% Send to Google Cloud Trace API
    case send_to_trace_api(CloudSpans) of
        {ok, _Response} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @private
-spec span_to_cloud_format(Span) -> term()
  when Span :: {binary(), span()}.
span_to_cloud_format({_SpanId, Span}) ->
    #{
        trace_id => base64:encode(maps:get(trace_id, Span)),
        span_id => base64:encode(maps:get(span_id, Span)),
        parent_span_id => maps:get(parent_span_id, Span, undefined),
        display_name => maps:get(operation_name, Span),
        start_time => maps:get(start_time, Span),
        end_time => maps:get(end_time, Span, erlang:system_time(microsecond)),
        attributes => maps:get(attributes, Span, #{})
    };
span_to_cloud_format(Span) ->
    Span.

%% @private
-spec send_to_trace_api(Spans) -> {ok, term()} | {error, term()}
  when Spans :: [term()].
send_to_trace_api(_Spans) ->
    %% Placeholder for actual Cloud Trace API call
    %% This would integrate with Google Cloud Trace client library
    {ok, #{status => <<"success">>}}.

%%%===================================================================
%% End of module
%%%===================================================================
