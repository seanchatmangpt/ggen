%%%-------------------------------------------------------------------
%% @doc Structured Logger - JSON-formatted structured logging
%%      All logs emitted as JSON for machine parsing and BigQuery ingest
%%      Supports request tracing with OpenTelemetry span IDs
%% @end
%%%-------------------------------------------------------------------

-module(structured_logger).

-compile({no_auto_import,[error/3]}).

-export([
    info/2, info/3,
    warning/2, warning/3,
    error/2, error/3,
    debug/2, debug/3,
    audit/3,
    emit_receipt/2,
    emit_refusal/3
]).

-define(LOG_LEVEL_DEBUG, debug).
-define(LOG_LEVEL_INFO, info).
-define(LOG_LEVEL_WARNING, warning).
-define(LOG_LEVEL_ERROR, error).
-define(LOG_LEVEL_CRITICAL, critical).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Log info message with structured data
-spec info(Message :: binary(), Data :: map()) -> ok.
info(Message, Data) ->
    info(Message, Data, #{}).

%% @doc Log info with trace context
-spec info(Message :: binary(), Data :: map(), TraceContext :: map()) -> ok.
info(Message, Data, TraceContext) ->
    emit_log(info, Message, Data, TraceContext).

%% @doc Log warning message
-spec warning(Message :: binary(), Data :: map()) -> ok.
warning(Message, Data) ->
    warning(Message, Data, #{}).

-spec warning(Message :: binary(), Data :: map(), TraceContext :: map()) -> ok.
warning(Message, Data, TraceContext) ->
    emit_log(warning, Message, Data, TraceContext).

%% @doc Log error message
-spec error(Message :: binary(), Data :: map()) -> ok.
error(Message, Data) ->
    error(Message, Data, #{}).

-spec error(Message :: binary(), Data :: map(), TraceContext :: map()) -> ok.
error(Message, Data, TraceContext) ->
    emit_log(error, Message, Data, TraceContext).

%% @doc Log debug message (development only)
-spec debug(Message :: binary(), Data :: map()) -> ok.
debug(Message, Data) ->
    debug(Message, Data, #{}).

-spec debug(Message :: binary(), Data :: map(), TraceContext :: map()) -> ok.
debug(Message, Data, TraceContext) ->
    case application:get_env(tai_autonomics, enable_debug_logs, false) of
        true -> emit_log(debug, Message, Data, TraceContext);
        false -> ok
    end.

%% @doc Audit log for compliance & governance events
-spec audit(EventType :: atom(), Reason :: binary(), Details :: map()) -> ok.
audit(EventType, Reason, Details) ->
    EventData = Details#{
        event_type => atom_to_binary(EventType, utf8),
        reason => Reason,
        timestamp => current_timestamp_ms()
    },
    emit_log(info, <<"audit_event">>, EventData, #{
        labels => #{audit => <<"true">>}
    }).

%% @doc Emit acceptance receipt with logging
-spec emit_receipt(ReceiptType :: atom(), Receipt :: map()) -> ok.
emit_receipt(ReceiptType, Receipt) ->
    EventData = Receipt#{
        event_type => <<"receipt_generated">>,
        receipt_type => atom_to_binary(ReceiptType, utf8),
        timestamp => current_timestamp_ms()
    },
    emit_log(info, <<"receipt_generated">>, EventData, #{
        labels => #{event_type => <<"receipt">>}
    }).

%% @doc Emit refusal receipt with error tracking
-spec emit_refusal(Reason :: atom(), Message :: binary(), Details :: map()) -> ok.
emit_refusal(Reason, Message, Details) ->
    EventData = Details#{
        event_type => <<"refusal">>,
        reason => atom_to_binary(Reason, utf8),
        message => Message,
        timestamp => current_timestamp_ms()
    },
    emit_log(warning, <<"refusal">>, EventData, #{
        labels => #{
            event_type => <<"refusal">>,
            reason => atom_to_binary(Reason, utf8)
        }
    }).

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @private
%% @doc Emit structured log to Cloud Logging
-spec emit_log(
    Level :: atom(),
    Message :: binary(),
    Data :: map(),
    Context :: map()
) -> ok.
emit_log(Level, Message, Data, Context) ->
    Timestamp = current_timestamp_iso8601(),
    TraceId = get_trace_id(Context),
    SpanId = get_span_id(Context),

    LogEntry = #{
        timestamp => Timestamp,
        severity => severity_to_string(Level),
        message => Message,
        jsonPayload => Data,
        labels => maps:get(labels, Context, #{}),
        resource => resource_metadata(),
        traceId => TraceId,
        spanId => SpanId
    },

    % Emit to Cloud Logging via stderr (Fluentd/Cloud Run integration)
    % Cloud Run automatically captures stderr JSON and sends to Cloud Logging
    case os:getenv("GOOGLE_CLOUD_PROJECT") of
        false ->
            % Local development: pretty print
            logger:log(Level, "~s~n", [jsx:encode(LogEntry)]);
        _ ->
            % Cloud Run: structured JSON to stderr
            io:fwrite(standard_error, "~s~n", [jsx:encode(LogEntry)])
    end,

    % Track metrics based on log level
    case Level of
        error -> metrics_collector:increment_error_count();
        warning -> metrics_collector:increment_warning_count();
        _ -> ok
    end.

%% @private Get current timestamp in milliseconds
-spec current_timestamp_ms() -> integer().
current_timestamp_ms() ->
    erlang:system_time(millisecond).

%% @private Get current timestamp in ISO 8601 format
-spec current_timestamp_iso8601() -> binary().
current_timestamp_iso8601() ->
    Now = erlang:system_time(microsecond),
    Rfc3339 = calendar:system_time_to_rfc3339(Now, [{unit, microsecond}]),
    erlang:iolist_to_binary(Rfc3339).

%% @private Get trace ID from context or current trace
-spec get_trace_id(Context :: map()) -> binary().
get_trace_id(#{trace_id := TraceId}) -> TraceId;
get_trace_id(_) ->
    case otel_tracer:current_span() of
        {_, SpanContext} ->
            opentelemetry:trace_id_to_string(SpanContext);
        _ -> <<>>
    end.

%% @private Get span ID from context
-spec get_span_id(Context :: map()) -> binary().
get_span_id(#{span_id := SpanId}) -> SpanId;
get_span_id(_) ->
    case otel_tracer:current_span() of
        {_, SpanContext} ->
            opentelemetry:span_id_to_string(SpanContext);
        _ -> <<>>
    end.

%% @private Convert severity atom to string
-spec severity_to_string(atom()) -> binary().
severity_to_string(debug) -> <<"DEBUG">>;
severity_to_string(info) -> <<"INFO">>;
severity_to_string(warning) -> <<"WARNING">>;
severity_to_string(error) -> <<"ERROR">>;
severity_to_string(critical) -> <<"CRITICAL">>;
severity_to_string(Level) -> atom_to_binary(Level, utf8).

%% @private Get resource metadata for Cloud Run
-spec resource_metadata() -> map().
resource_metadata() ->
    #{
        type => <<"cloud_run_revision">>,
        labels => #{
            project_id => erlang:iolist_to_binary(os:getenv("GOOGLE_CLOUD_PROJECT", "")),
            service_name => <<"tai-autonomics">>,
            revision_name => erlang:iolist_to_binary(os:getenv("CLOUD_RUN_REVISION", "")),
            region => erlang:iolist_to_binary(os:getenv("CLOUD_RUN_REGION", ""))
        }
    }.
