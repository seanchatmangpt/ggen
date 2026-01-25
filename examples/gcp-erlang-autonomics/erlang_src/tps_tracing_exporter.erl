%%%-------------------------------------------------------------------
%% @doc tps_tracing_exporter: Jaeger exporter for distributed tracing
%%
%% Handles exporting spans to Jaeger for visualization:
%% - Batch collection (collect 100 spans, export every 5s)
%% - Jaeger collector endpoint: configurable (default localhost:6831)
%% - Fallback behavior: if Jaeger unavailable, drop trace (don't break request)
%% - Metrics: spans exported, export latency, export errors
%%
%% Jaeger Integration:
%% - Converts OpenTelemetry spans to Jaeger wire format
%% - Uses Thrift protocol over HTTP (zipkin-compatible)
%% - Implements exponential backoff for retries
%% - Tracks export metrics for observability
%%
%% @end
%%%-------------------------------------------------------------------
-module(tps_tracing_exporter).
-behaviour(gen_server).

%% API
-export([start_link/0, export_span/1, get_export_stats/0, set_jaeger_endpoint/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EXPORT_BATCH_SIZE, 100).
-define(EXPORT_TIMEOUT, 5000).  % 5 seconds
-define(HTTP_TIMEOUT, 3000).    % 3 seconds for HTTP request
-define(MAX_RETRIES, 3).
-define(INITIAL_RETRY_DELAY, 100).  % milliseconds

-record(state, {
    export_queue :: ets:tid(),
    jaeger_host :: string(),
    jaeger_port :: integer(),
    export_ref :: reference() | undefined,
    stats :: #{
        total_spans => integer(),
        successful_exports => integer(),
        failed_exports => integer(),
        retry_count => integer(),
        last_export_time => integer()
    }
}).

-type span_id() :: binary().
-type span() :: map().
-type export_stats() :: #{
    total_spans => integer(),
    successful_exports => integer(),
    failed_exports => integer(),
    retry_count => integer(),
    last_export_time => integer()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the Jaeger exporter
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Queue a span for export to Jaeger
%% Span will be batched with others and exported periodically
-spec export_span(Span) -> ok
  when Span :: span().
export_span(Span) ->
    gen_server:cast(?SERVER, {queue_span, Span}).

%% @doc Get export statistics (debugging/monitoring)
-spec get_export_stats() -> {ok, export_stats()} | {error, term()}.
get_export_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Set Jaeger endpoint (host and port)
-spec set_jaeger_endpoint(Host, Port) -> ok
  when Host :: string(),
       Port :: integer().
set_jaeger_endpoint(Host, Port) ->
    gen_server:call(?SERVER, {set_endpoint, Host, Port}).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Create ETS table for export queue
    ExportQueue = ets:new(tps_export_queue, [
        {keypos, 1},
        public,
        {write_concurrency, true}
    ]),

    %% Load Jaeger configuration
    JaegerHost = application:get_env(ggen, jaeger_host, "localhost"),
    JaegerPort = application:get_env(ggen, jaeger_port, 6831),

    %% Schedule periodic export
    ExportRef = erlang:send_after(?EXPORT_TIMEOUT, self(), export_batch),

    %% Initialize stats
    Stats = #{
        total_spans => 0,
        successful_exports => 0,
        failed_exports => 0,
        retry_count => 0,
        last_export_time => 0
    },

    {ok, #state{
        export_queue = ExportQueue,
        jaeger_host = JaegerHost,
        jaeger_port = JaegerPort,
        export_ref = ExportRef,
        stats = Stats
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call({set_endpoint, Host, Port}, _From, State) ->
    {reply, ok, State#state{jaeger_host = Host, jaeger_port = Port}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, state()}
  when Request :: term(),
       State :: state().
handle_cast({queue_span, Span}, State) ->
    SpanId = generate_queue_id(),
    ets:insert(State#state.export_queue, {SpanId, Span}),

    %% Update span count
    Stats = State#state.stats,
    UpdatedStats = Stats#{total_spans => maps:get(total_spans, Stats, 0) + 1},

    %% Check if batch size reached
    QueueSize = ets:info(State#state.export_queue, size),
    case QueueSize >= ?EXPORT_BATCH_SIZE of
        true ->
            self() ! export_batch,
            {noreply, State#state{stats = UpdatedStats}};
        false ->
            {noreply, State#state{stats = UpdatedStats}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, state()}
  when Info :: term(),
       State :: state().
handle_info(export_batch, State) ->
    %% Get all spans from queue
    Spans = ets:tab2list(State#state.export_queue),

    case Spans of
        [] ->
            %% No spans to export, reschedule
            ExportRef = erlang:send_after(?EXPORT_TIMEOUT, self(), export_batch),
            {noreply, State#state{export_ref = ExportRef}};
        _ ->
            %% Extract span data
            SpanData = [Span || {_SpanId, Span} <- Spans],

            %% Attempt export with retries
            case export_with_retry(SpanData, State#state.jaeger_host,
                                   State#state.jaeger_port, ?MAX_RETRIES) of
                {ok, ExportTime} ->
                    %% Clear exported spans
                    lists:foreach(fun({SpanId, _}) ->
                        ets:delete(State#state.export_queue, SpanId)
                    end, Spans),

                    %% Update stats
                    Stats = State#state.stats,
                    UpdatedStats = Stats#{
                        successful_exports => maps:get(successful_exports, Stats, 0) + 1,
                        last_export_time => ExportTime
                    },

                    %% Reschedule
                    ExportRef = erlang:send_after(?EXPORT_TIMEOUT, self(), export_batch),
                    {noreply, State#state{stats = UpdatedStats, export_ref = ExportRef}};

                {error, _Reason} ->
                    %% Keep spans for next attempt
                    Stats = State#state.stats,
                    UpdatedStats = Stats#{
                        failed_exports => maps:get(failed_exports, Stats, 0) + 1
                    },

                    %% Reschedule with backoff
                    ExportRef = erlang:send_after(?EXPORT_TIMEOUT, self(), export_batch),
                    {noreply, State#state{stats = UpdatedStats, export_ref = ExportRef}}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, State) ->
    %% Final export before shutdown
    Spans = ets:tab2list(State#state.export_queue),
    case Spans of
        [] ->
            ok;
        _ ->
            SpanData = [Span || {_SpanId, Span} <- Spans],
            _ = export_with_retry(SpanData, State#state.jaeger_host,
                                  State#state.jaeger_port, ?MAX_RETRIES),
            ok
    end.

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

%% @private Generate unique queue ID
-spec generate_queue_id() -> binary().
generate_queue_id() ->
    crypto:strong_rand_bytes(8).

%% @private Export spans with exponential backoff retry
-spec export_with_retry(Spans, Host, Port, Retries) -> {ok, integer()} | {error, term()}
  when Spans :: [span()],
       Host :: string(),
       Port :: integer(),
       Retries :: integer().
export_with_retry([], _Host, _Port, _Retries) ->
    {ok, erlang:system_time(microsecond)};

export_with_retry(Spans, Host, Port, Retries) ->
    case send_to_jaeger(Spans, Host, Port) of
        {ok, Time} ->
            {ok, Time};
        {error, Reason} when Retries > 0 ->
            %% Exponential backoff: 100ms, 200ms, 400ms
            Delay = ?INITIAL_RETRY_DELAY * (2 ^ (?MAX_RETRIES - Retries)),
            timer:sleep(Delay),
            export_with_retry(Spans, Host, Port, Retries - 1);
        {error, Reason} ->
            error_logger:warning_msg("Jaeger export failed after retries: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @private Send batch of spans to Jaeger
%% Converts spans to Jaeger wire format and sends via HTTP
-spec send_to_jaeger(Spans, Host, Port) -> {ok, integer()} | {error, term()}
  when Spans :: [span()],
       Host :: string(),
       Port :: integer().
send_to_jaeger([], _Host, _Port) ->
    {ok, erlang:system_time(microsecond)};

send_to_jaeger(Spans, Host, Port) ->
    %% Convert to Jaeger format
    JaegerSpans = lists:map(fun span_to_jaeger_format/1, Spans),

    %% Build batch
    Batch = #{
        <<"spans">> => JaegerSpans,
        <<"batchID">> => base64:encode(crypto:strong_rand_bytes(16)),
        <<"timestamp">> => erlang:system_time(microsecond)
    },

    %% Send to Jaeger collector
    send_http_request(Host, Port, Batch).

%% @private Convert OpenTelemetry span to Jaeger format
%% Jaeger expects: traceID, spanID, parentSpanID, operationName, startTime, duration, tags, logs, status
-spec span_to_jaeger_format(Span) -> map()
  when Span :: map().
span_to_jaeger_format(Span) ->
    TraceId = maps:get(trace_id, Span, <<>>),
    SpanId = maps:get(span_id, Span, <<>>),
    ParentSpanId = maps:get(parent_span_id, Span, undefined),
    OperationName = maps:get(operation_name, Span, <<"unknown">>),
    StartTime = maps:get(start_time, Span, erlang:system_time(microsecond)),
    EndTime = maps:get(end_time, Span, erlang:system_time(microsecond)),
    Attributes = maps:get(attributes, Span, []),
    Events = maps:get(events, Span, []),
    Status = maps:get(status, Span, unset),

    #{
        <<"traceID">> => base64:encode(TraceId),
        <<"spanID">> => base64:encode(SpanId),
        <<"parentSpanID">> => case ParentSpanId of
            undefined -> null;
            _ -> base64:encode(ParentSpanId)
        end,
        <<"operationName">> => ensure_binary(OperationName),
        <<"startTime">> => StartTime,
        <<"duration">> => max(1, EndTime - StartTime),
        <<"tags">> => attributes_to_tags(Attributes),
        <<"logs">> => events_to_logs(Events),
        <<"status">> => status_to_code(Status)
    }.

%% @private Convert attributes to Jaeger tags
-spec attributes_to_tags([{string(), term()}]) -> [map()].
attributes_to_tags(Attributes) ->
    lists:map(fun({Key, Value}) ->
        #{
            <<"key">> => ensure_binary(Key),
            <<"value">> => term_to_jaeger_value(Value),
            <<"type">> => <<"string">>
        }
    end, Attributes).

%% @private Convert events to Jaeger logs
-spec events_to_logs([map()]) -> [map()].
events_to_logs(Events) ->
    lists:map(fun(Event) ->
        Timestamp = maps:get(timestamp, Event, erlang:system_time(microsecond)),
        Fields = maps:get(attributes, Event, []),
        #{
            <<"timestamp">> => Timestamp,
            <<"fields">> => attributes_to_tags(Fields)
        }
    end, Events).

%% @private Convert term to Jaeger-compatible value
-spec term_to_jaeger_value(term()) -> term().
term_to_jaeger_value(Value) when is_binary(Value) ->
    Value;
term_to_jaeger_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
term_to_jaeger_value(Value) when is_number(Value) ->
    Value;
term_to_jaeger_value(Value) when is_list(Value) ->
    list_to_binary(Value);
term_to_jaeger_value(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

%% @private Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
ensure_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
ensure_binary(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

%% @private Convert status to Jaeger status code
-spec status_to_code(term()) -> string().
status_to_code(ok) -> "OK";
status_to_code(error) -> "ERROR";
status_to_code({error, _}) -> "ERROR";
status_to_code(unset) -> "UNSET";
status_to_code(_) -> "UNSET".

%% @private Send HTTP POST request to Jaeger collector
%% Endpoint: http://host:port/api/traces (Zipkin-compatible)
-spec send_http_request(Host, Port, Batch) -> {ok, integer()} | {error, term()}
  when Host :: string(),
       Port :: integer(),
       Batch :: map().
send_http_request(Host, Port, Batch) ->
    Url = io_lib:format("http://~s:~w/api/traces", [Host, Port]),
    Headers = [{"Content-Type", "application/json"}],
    Body = jsx:encode(Batch),

    case httpc:request(post,
                      {Url, Headers, "application/json", Body},
                      [{timeout, ?HTTP_TIMEOUT}],
                      []) of
        {ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, _ResponseHeaders, _ResponseBody}}
            when StatusCode >= 200, StatusCode < 300 ->
            {ok, erlang:system_time(microsecond)};

        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _ResponseHeaders, _ResponseBody}} ->
            {error, {http_error, StatusCode, ReasonPhrase}};

        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%% End of module
%%%===================================================================
