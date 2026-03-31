%%%-------------------------------------------------------------------
%% @doc tps_span_builder: Helper module for creating spans in TPS components
%%
%% Provides specialized span creators for each TPS component:
%% - Jidoka (circuit breaker): circuit_breaker_check (<5ms)
%% - Kanban (queue): queue_pull, queue_acknowledge
%% - Andon (alerts): log_event
%% - Heijunka (leveling): pool_acquire
%% - Action execute: action_execute (with result/error)
%%
%% Each span builder:
%% - Creates child spans with proper component context
%% - Measures latency automatically
%% - Captures component-specific attributes
%% - Records events for state changes
%% - Contributes to latency histogram
%%
%% @end
%%%-------------------------------------------------------------------
-module(tps_span_builder).

%% Jidoka (Circuit Breaker) spans
-export([jidoka_circuit_check_span/4, jidoka_circuit_check_end/3]).

%% Kanban (Queue) spans
-export([kanban_queue_pull_span/4, kanban_queue_pull_end/4,
         kanban_queue_ack_span/4, kanban_queue_ack_end/3]).

%% Andon (Alert) spans
-export([andon_log_event_span/4, andon_log_event_end/4]).

%% Heijunka (Leveling) spans
-export([heijunka_pool_acquire_span/4, heijunka_pool_acquire_end/4]).

%% Action Execute spans
-export([action_execute_span/4, action_execute_end/4]).

%% Utility functions
-export([with_timing/2, measure_latency/2]).

-type trace_id() :: binary().
-type span_id() :: binary().
-type component() :: string().

%%%===================================================================
%% Jidoka (Circuit Breaker) Span Builders
%%%===================================================================

%% @doc Create a circuit breaker check span
%% Used when jidoka circuit is checked for state
%% Attributes: circuit_name, threshold, state (open/closed/half_open)
%% Target latency: <5ms
-spec jidoka_circuit_check_span(TraceId, ParentSpanId, CircuitName, State) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       CircuitName :: string(),
       State :: open | closed | half_open.
jidoka_circuit_check_span(TraceId, ParentSpanId, CircuitName, State) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      "circuit_breaker_check", "jidoka") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "circuit_name", CircuitName),
            ok = tps_tracing:set_span_attribute(SpanId, "circuit_state", atom_to_list(State)),
            ok = tps_tracing:add_span_event(SpanId, "circuit_state_checked", [
                {"circuit_name", CircuitName},
                {"state", atom_to_list(State)},
                {"timestamp", erlang:system_time(microsecond)}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End circuit breaker check span with result
%% Result: pass (circuit allows traffic) or block (circuit rejects)
-spec jidoka_circuit_check_end(SpanId, Result, ErrorMessage) -> ok
  when SpanId :: span_id(),
       Result :: pass | block,
       ErrorMessage :: string() | undefined.
jidoka_circuit_check_end(SpanId, Result, ErrorMessage) ->
    ok = tps_tracing:set_span_attribute(SpanId, "result", atom_to_list(Result)),
    case ErrorMessage of
        undefined ->
            ok = tps_tracing:end_span(SpanId);
        Error ->
            ok = tps_tracing:set_span_attribute(SpanId, "error", Error),
            ok = tps_tracing:end_span_with_status(SpanId, error)
    end,
    ok.

%%%===================================================================
%% Kanban (Queue) Span Builders
%%%===================================================================

%% @doc Create a queue pull span
%% Used when work item is pulled from queue
%% Attributes: queue_depth, priority, work_item_id
-spec kanban_queue_pull_span(TraceId, ParentSpanId, QueueName, QueueDepth) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       QueueName :: string(),
       QueueDepth :: integer().
kanban_queue_pull_span(TraceId, ParentSpanId, QueueName, QueueDepth) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      "queue_pull", "kanban") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "queue_name", QueueName),
            ok = tps_tracing:set_span_attribute(SpanId, "queue_depth", QueueDepth),
            ok = tps_tracing:add_span_event(SpanId, "queue_pull_initiated", [
                {"queue_name", QueueName},
                {"queue_depth", integer_to_list(QueueDepth)},
                {"timestamp", erlang:system_time(microsecond)}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End queue pull span
%% Measures queue latency and records work item
-spec kanban_queue_pull_end(SpanId, WorkItemId, Priority, WaitTime) -> ok
  when SpanId :: span_id(),
       WorkItemId :: string(),
       Priority :: string(),
       WaitTime :: integer().  % microseconds
kanban_queue_pull_end(SpanId, WorkItemId, Priority, WaitTime) ->
    ok = tps_tracing:set_span_attribute(SpanId, "work_item_id", WorkItemId),
    ok = tps_tracing:set_span_attribute(SpanId, "priority", Priority),
    ok = tps_tracing:set_span_attribute(SpanId, "queue_wait_time_us", WaitTime),
    ok = tps_tracing:add_span_event(SpanId, "work_item_acquired", [
        {"work_item_id", WorkItemId},
        {"priority", Priority},
        {"wait_time_us", integer_to_list(WaitTime)}
    ]),
    ok = tps_tracing:end_span(SpanId),
    ok.

%% @doc Create a queue acknowledge span
%% Used when work item is acknowledged/completed
-spec kanban_queue_ack_span(TraceId, ParentSpanId, WorkItemId, QueueName) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       WorkItemId :: string(),
       QueueName :: string().
kanban_queue_ack_span(TraceId, ParentSpanId, WorkItemId, QueueName) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      "queue_acknowledge", "kanban") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "work_item_id", WorkItemId),
            ok = tps_tracing:set_span_attribute(SpanId, "queue_name", QueueName),
            ok = tps_tracing:add_span_event(SpanId, "queue_ack_initiated", [
                {"work_item_id", WorkItemId},
                {"queue_name", QueueName}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End queue acknowledge span
-spec kanban_queue_ack_end(SpanId, Status, ErrorMessage) -> ok
  when SpanId :: span_id(),
       Status :: success | failure,
       ErrorMessage :: string() | undefined.
kanban_queue_ack_end(SpanId, Status, ErrorMessage) ->
    ok = tps_tracing:set_span_attribute(SpanId, "ack_status", atom_to_list(Status)),
    case ErrorMessage of
        undefined ->
            ok = tps_tracing:end_span(SpanId);
        Error ->
            ok = tps_tracing:set_span_attribute(SpanId, "error", Error),
            ok = tps_tracing:end_span_with_status(SpanId, error)
    end,
    ok.

%%%===================================================================
%% Andon (Alert) Span Builders
%%%===================================================================

%% @doc Create an Andon event logging span
%% Used when alerts/anomalies are logged
%% Attributes: event_type, severity_level
-spec andon_log_event_span(TraceId, ParentSpanId, EventType, Severity) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       EventType :: string(),
       Severity :: critical | high | medium | low.
andon_log_event_span(TraceId, ParentSpanId, EventType, Severity) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      "log_event", "andon") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "event_type", EventType),
            ok = tps_tracing:set_span_attribute(SpanId, "severity", atom_to_list(Severity)),
            ok = tps_tracing:add_span_event(SpanId, "alert_triggered", [
                {"event_type", EventType},
                {"severity", atom_to_list(Severity)},
                {"timestamp", erlang:system_time(microsecond)}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End Andon event span
%% Records whether alert was acknowledged/handled
-spec andon_log_event_end(SpanId, Acknowledged, Handler, Message) -> ok
  when SpanId :: span_id(),
       Acknowledged :: boolean(),
       Handler :: string(),
       Message :: string().
andon_log_event_end(SpanId, Acknowledged, Handler, Message) ->
    ok = tps_tracing:set_span_attribute(SpanId, "acknowledged", Acknowledged),
    ok = tps_tracing:set_span_attribute(SpanId, "handler", Handler),
    ok = tps_tracing:set_span_attribute(SpanId, "message", Message),
    ok = tps_tracing:add_span_event(SpanId, "alert_handled", [
        {"handler", Handler},
        {"acknowledged", atom_to_list(Acknowledged)}
    ]),
    ok = tps_tracing:end_span(SpanId),
    ok.

%%%===================================================================
%% Heijunka (Leveling/Pool) Span Builders
%%%===================================================================

%% @doc Create a resource pool acquire span
%% Used when waiting for pool resource (thread, connection)
%% Attributes: pool_name, pool_utilization
-spec heijunka_pool_acquire_span(TraceId, ParentSpanId, PoolName, Utilization) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       PoolName :: string(),
       Utilization :: float().  % 0.0-1.0
heijunka_pool_acquire_span(TraceId, ParentSpanId, PoolName, Utilization) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      "pool_acquire", "heijunka") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "pool_name", PoolName),
            ok = tps_tracing:set_span_attribute(SpanId, "pool_utilization", Utilization),
            ok = tps_tracing:add_span_event(SpanId, "pool_acquire_requested", [
                {"pool_name", PoolName},
                {"utilization", float_to_list(Utilization)}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End pool acquire span
%% Records acquired resource and wait time
-spec heijunka_pool_acquire_end(SpanId, ResourceId, AcquireTime, Success) -> ok
  when SpanId :: span_id(),
       ResourceId :: string(),
       AcquireTime :: integer(),  % microseconds
       Success :: boolean().
heijunka_pool_acquire_end(SpanId, ResourceId, AcquireTime, Success) ->
    ok = tps_tracing:set_span_attribute(SpanId, "resource_id", ResourceId),
    ok = tps_tracing:set_span_attribute(SpanId, "acquire_time_us", AcquireTime),
    ok = tps_tracing:set_span_attribute(SpanId, "acquire_success", Success),
    ok = tps_tracing:add_span_event(SpanId, "pool_resource_acquired", [
        {"resource_id", ResourceId},
        {"acquire_time_us", integer_to_list(AcquireTime)},
        {"success", atom_to_list(Success)}
    ]),
    ok = tps_tracing:end_span(SpanId),
    ok.

%%%===================================================================
%% Action Execute Span Builders
%%%===================================================================

%% @doc Create an action execution span
%% Used for top-level work item execution
%% Tracks entire action including component interactions
-spec action_execute_span(TraceId, ParentSpanId, ActionName, ActionType) ->
    {ok, span_id()} | {error, term()}
  when TraceId :: trace_id(),
       ParentSpanId :: span_id(),
       ActionName :: string(),
       ActionType :: string().
action_execute_span(TraceId, ParentSpanId, ActionName, ActionType) ->
    case tps_tracing:start_child_span(TraceId, ParentSpanId,
                                      ActionName, "action") of
        {ok, SpanId} ->
            ok = tps_tracing:set_span_attribute(SpanId, "action_type", ActionType),
            ok = tps_tracing:set_span_attribute(SpanId, "action_name", ActionName),
            ok = tps_tracing:add_span_event(SpanId, "action_started", [
                {"action_name", ActionName},
                {"action_type", ActionType},
                {"start_time", erlang:system_time(microsecond)}
            ]),
            {ok, SpanId};
        Error ->
            Error
    end.

%% @doc End action execution span
%% Records result and captures any errors
-spec action_execute_end(SpanId, Result, ErrorMessage, Metadata) -> ok
  when SpanId :: span_id(),
       Result :: success | failure,
       ErrorMessage :: string() | undefined,
       Metadata :: #{string() => term()}.
action_execute_end(SpanId, Result, ErrorMessage, Metadata) ->
    ok = tps_tracing:set_span_attribute(SpanId, "action_result", atom_to_list(Result)),

    %% Add metadata attributes
    lists:foreach(fun({Key, Value}) ->
        ok = tps_tracing:set_span_attribute(SpanId, Key, Value)
    end, maps:to_list(Metadata)),

    %% Handle error
    case ErrorMessage of
        undefined ->
            ok = tps_tracing:add_span_event(SpanId, "action_completed", [
                {"result", atom_to_list(Result)},
                {"timestamp", erlang:system_time(microsecond)}
            ]),
            ok = tps_tracing:end_span(SpanId);
        Error ->
            ok = tps_tracing:set_span_attribute(SpanId, "error", Error),
            ok = tps_tracing:add_span_event(SpanId, "action_failed", [
                {"error", Error},
                {"timestamp", erlang:system_time(microsecond)}
            ]),
            ok = tps_tracing:end_span_with_status(SpanId, {error, Error})
    end,
    ok.

%%%===================================================================
%% Utility Functions
%%%===================================================================

%% @doc Execute function with timing and create span
%% Used for measuring component performance
-spec with_timing(Fun, SpanId) -> {Result, Latency}
  when Fun :: fun(() -> Result),
       SpanId :: span_id(),
       Result :: term(),
       Latency :: integer().  % microseconds
with_timing(Fun, SpanId) ->
    StartTime = erlang:system_time(microsecond),
    Result = Fun(),
    EndTime = erlang:system_time(microsecond),
    Latency = EndTime - StartTime,
    {Result, Latency}.

%% @doc Measure latency of an operation
%% Returns {Result, Latency}
-spec measure_latency(Fun, Label) -> {Result, Latency}
  when Fun :: fun(() -> Result),
       Label :: string(),
       Result :: term(),
       Latency :: integer().
measure_latency(Fun, Label) ->
    StartTime = erlang:system_time(microsecond),
    Result = Fun(),
    EndTime = erlang:system_time(microsecond),
    Latency = EndTime - StartTime,

    %% Log latency measurement
    error_logger:info_msg("Latency measurement [~s]: ~p us~n", [Label, Latency]),

    {Result, Latency}.

%%%===================================================================
%% End of module
%%%===================================================================
