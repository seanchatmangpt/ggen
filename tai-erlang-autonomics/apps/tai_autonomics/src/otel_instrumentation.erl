%%%-------------------------------------------------------------------
%% @doc OpenTelemetry Instrumentation - Distributed tracing & metrics
%%      Emits traces to Google Cloud Trace via gRPC
%%      Records span events for governor transitions and receipts
%%      Tracks span attributes: status, error details, business metrics
%% @end
%%%-------------------------------------------------------------------

-module(otel_instrumentation).

-export([
    start_span/2, start_span/3,
    end_span/1, end_span/2,
    add_event/3,
    record_attribute/3,
    record_exception/2,
    with_span/2
]).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start a new trace span
-spec start_span(OperationName :: binary(), Attributes :: map()) ->
    {ok, SpanRef :: reference()} | {error, term()}.
start_span(OperationName, Attributes) ->
    start_span(OperationName, Attributes, #{}).

%% @doc Start a span with trace context
-spec start_span(
    OperationName :: binary(),
    Attributes :: map(),
    TraceContext :: map()
) -> {ok, SpanRef :: reference()} | {error, term()}.
start_span(OperationName, Attributes, TraceContext) ->
    try
        %% Get current span as parent
        Parent = case maps:get(parent_span_id, TraceContext, undefined) of
            undefined -> otel_tracer:current_span();
            ParentId -> ParentId
        end,

        %% Create span with attributes
        Span = otel_tracer:start_span(OperationName, #{
            attributes => maps:merge(
                Attributes,
                #{
                    <<"service.name">> => <<"tai-autonomics">>,
                    <<"service.version">> => <<"1.0.0">>,
                    <<"service.instance.id">> => node_id(),
                    <<"deployment.environment">> => deployment_env()
                }
            )
        }),

        {ok, Span}
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc End a span with optional status
-spec end_span(SpanRef :: reference()) -> ok.
end_span(SpanRef) ->
    end_span(SpanRef, ok).

-spec end_span(SpanRef :: reference(), Status :: ok | {error, Reason :: term()}) -> ok.
end_span(SpanRef, Status) ->
    try
        case Status of
            ok ->
                otel_span:set_attribute(
                    SpanRef,
                    <<"otel.status_code">>,
                    <<"OK">>
                );
            {error, Reason} ->
                otel_span:set_attribute(
                    SpanRef,
                    <<"otel.status_code">>,
                    <<"ERROR">>
                ),
                otel_span:set_attribute(
                    SpanRef,
                    <<"error.type">>,
                    format_error(Reason)
                ),
                otel_span:record_exception(SpanRef, Reason, #{})
        end,
        otel_span:end_span(SpanRef),
        ok
    catch
        _:_ -> ok
    end.

%% @doc Add event to current span
-spec add_event(SpanRef :: reference(), EventName :: binary(), Attributes :: map()) -> ok.
add_event(SpanRef, EventName, Attributes) ->
    try
        otel_span:add_event(SpanRef, EventName, Attributes),
        ok
    catch
        _:_ -> ok
    end.

%% @doc Record attribute on span
-spec record_attribute(SpanRef :: reference(), Key :: atom() | binary(), Value :: term()) -> ok.
record_attribute(SpanRef, Key, Value) ->
    try
        KeyBin = case Key of
            K when is_atom(K) -> atom_to_binary(K, utf8);
            K when is_binary(K) -> K
        end,
        otel_span:set_attribute(SpanRef, KeyBin, format_attribute(Value)),
        ok
    catch
        _:_ -> ok
    end.

%% @doc Record exception on span
-spec record_exception(SpanRef :: reference(), Exception :: term()) -> ok.
record_exception(SpanRef, Exception) ->
    try
        otel_span:record_exception(SpanRef, Exception, #{})
    catch
        _:_ -> ok
    end.

%% @doc Execute function within a span context
-spec with_span(SpanName :: binary(), Fun :: fun(() -> Result)) -> Result
  when Result :: term().
with_span(SpanName, Fun) ->
    {ok, SpanRef} = start_span(SpanName, #{}),
    try
        Result = Fun(),
        end_span(SpanRef, ok),
        Result
    catch
        Error:Reason:Stack ->
            end_span(SpanRef, {error, {Error, Reason}}),
            erlang:raise(Error, Reason, Stack)
    end.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @private Get node ID (Erlang node name or container ID)
-spec node_id() -> binary().
node_id() ->
    case os:getenv("HOSTNAME") of
        false -> erlang:atom_to_binary(node(), utf8);
        Hostname -> erlang:iolist_to_binary(Hostname)
    end.

%% @private Get deployment environment
-spec deployment_env() -> binary().
deployment_env() ->
    case os:getenv("ENVIRONMENT") of
        false -> <<"production">>;
        Env -> erlang:iolist_to_binary(Env)
    end.

%% @private Format error term for span attribute
-spec format_error(Reason :: term()) -> binary().
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error({Error, _Details}) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
format_error({error, Reason}) ->
    format_error(Reason);
format_error(Reason) ->
    erlang:iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private Format term as span attribute value
-spec format_attribute(Value :: term()) -> term().
format_attribute(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
format_attribute(Value) when is_binary(Value) ->
    Value;
format_attribute(Value) when is_integer(Value) ->
    Value;
format_attribute(Value) when is_float(Value) ->
    Value;
format_attribute(Value) when is_list(Value) ->
    try
        erlang:iolist_to_binary(Value)
    catch
        _:_ -> erlang:iolist_to_binary(io_lib:format("~p", [Value]))
    end;
format_attribute(Value) when is_map(Value) ->
    jsx:encode(Value);
format_attribute(Value) ->
    erlang:iolist_to_binary(io_lib:format("~p", [Value])).

%%%===================================================================
%% Hooks for Governor Events
%%%===================================================================

%% @doc Record governor state transition
-spec record_governor_transition(
    Governor :: atom(),
    FromState :: atom(),
    ToState :: atom(),
    Reason :: term()
) -> ok.
record_governor_transition(Governor, FromState, ToState, Reason) ->
    otel_instrumentation:with_span(
        <<"governor_transition">>,
        fun() ->
            {ok, Span} = otel_instrumentation:start_span(
                <<"governor_transition">>,
                #{
                    <<"governor">> => atom_to_binary(Governor, utf8),
                    <<"from_state">> => atom_to_binary(FromState, utf8),
                    <<"to_state">> => atom_to_binary(ToState, utf8)
                }
            ),

            %% Add reason as event
            otel_instrumentation:add_event(
                Span,
                <<"transition_reason">>,
                #{reason => format_attribute(Reason)}
            ),

            otel_instrumentation:end_span(Span, ok)
        end
    ).

%% @doc Record receipt generation
-spec record_receipt(
    ReceiptType :: atom(),
    ReceiptData :: map()
) -> ok.
record_receipt(ReceiptType, ReceiptData) ->
    EventName = erlang:atom_to_binary(ReceiptType, utf8),
    otel_instrumentation:with_span(
        <<"receipt_generated">>,
        fun() ->
            {ok, Span} = otel_instrumentation:start_span(
                <<"receipt_generated">>,
                #{
                    <<"receipt_type">> => EventName,
                    <<"receipt_id">> => maps:get(receipt_id, ReceiptData, <<>>)
                }
            ),

            otel_instrumentation:add_event(
                Span,
                <<"receipt_details">>,
                ReceiptData
            ),

            otel_instrumentation:end_span(Span, ok)
        end
    ).

%% @doc Record HTTP request span
-spec record_http_request(
    Method :: binary(),
    Path :: binary(),
    StatusCode :: integer(),
    DurationMs :: integer()
) -> ok.
record_http_request(Method, Path, StatusCode, DurationMs) ->
    Status = case StatusCode of
        Code when Code >= 200, Code < 300 -> ok;
        Code when Code >= 400, Code < 500 -> ok;  % Client errors still OK span
        _ -> {error, server_error}
    end,

    otel_instrumentation:with_span(
        <<"http_request">>,
        fun() ->
            {ok, Span} = otel_instrumentation:start_span(
                <<"http_request">>,
                #{
                    <<"http.method">> => Method,
                    <<"http.url">> => Path,
                    <<"http.status_code">> => StatusCode,
                    <<"http.duration_ms">> => DurationMs
                }
            ),

            otel_instrumentation:end_span(Span, Status)
        end
    ).
