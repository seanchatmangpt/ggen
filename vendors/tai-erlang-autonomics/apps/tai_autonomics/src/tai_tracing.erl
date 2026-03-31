%%%-------------------------------------------------------------------
%% @doc tai_tracing: OpenTelemetry tracing
%%
%% Creates spans for request processing, governor transitions, and actions.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_tracing).

%% API
-export([start_span/2, end_span/1, set_attribute/3]).

%% -include_lib("opentelemetry/include/otel_tracer.hrl").

%%%===================================================================
%% API
%%%===================================================================

-spec start_span(Name, Attributes) -> SpanCtx
  when Name :: binary(),
       Attributes :: [{Key, Value}],
       Key :: binary(),
       Value :: binary() | integer() | float(),
       SpanCtx :: opentelemetry:span_ctx().
start_span(Name, Attributes) ->
    %% Get tracer for this application
    Tracer = opentelemetry:get_tracer(?MODULE),
    
    %% Convert attributes to OpenTelemetry format
    OtelAttributes = lists:map(fun({Key, Value}) ->
        KeyStr = binary_to_list(Key),
        case Value of
            V when is_binary(V) ->
                {KeyStr, V};
            V when is_integer(V) ->
                {KeyStr, V};
            V when is_float(V) ->
                {KeyStr, V};
            _ ->
                {KeyStr, erlang:term_to_binary(Value)}
        end
    end, Attributes),
    
    %% Start span
    SpanName = binary_to_list(Name),
    SpanCtx = opentelemetry:start_span(Tracer, SpanName, #{attributes => OtelAttributes}),
    SpanCtx.

-spec end_span(SpanCtx) -> ok
  when SpanCtx :: opentelemetry:span_ctx().
end_span(SpanCtx) ->
    %% End span
    opentelemetry:end_span(SpanCtx),
    ok.

-spec set_attribute(SpanCtx, Key, Value) -> ok
  when SpanCtx :: opentelemetry:span_ctx(),
       Key :: binary(),
       Value :: binary() | integer() | float().
set_attribute(SpanCtx, Key, Value) ->
    %% Set attribute on span
    KeyStr = binary_to_list(Key),
    OtelValue = case Value of
        V when is_binary(V) -> V;
        V when is_integer(V) -> V;
        V when is_float(V) -> V;
        _ -> erlang:term_to_binary(Value)
    end,
    opentelemetry:set_attribute(SpanCtx, KeyStr, OtelValue),
    ok.
