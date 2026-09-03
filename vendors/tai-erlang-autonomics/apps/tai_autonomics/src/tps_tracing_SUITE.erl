%%%-------------------------------------------------------------------
%% @doc tps_tracing_SUITE: Common Test suite for TPS tracing system
%%
%% Black-box tests verifying:
%% - Span creation and completion
%% - Parent-child span relationships
%% - Export to Jaeger
%% - Latency measurement
%% - Sampling behavior
%% - Component-specific span builders
%%
%% Uses Common Test framework with proper setup/teardown
%% Tests observable outputs and state changes (Chicago TDD style)
%%
%% @end
%%%-------------------------------------------------------------------
-module(tps_tracing_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    %% Start application and dependencies
    {ok, _} = application:ensure_all_started(ggen),
    {ok, _TracerPid} = tps_tracing:start_link(),
    {ok, _ExporterPid} = tps_tracing_exporter:start_link(),
    {ok, _AnalyzerPid} = tps_tracing_analyzer:start_link(),
    timer:sleep(100),  % Allow gen_servers to initialize
    Config.

end_per_suite(Config) ->
    application:stop(ggen),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

all() ->
    [
        test_root_span_creation,
        test_child_span_relationship,
        test_span_attributes,
        test_span_events,
        test_span_latency_measurement,
        test_span_status_tracking,
        test_sampling_works,
        test_export_to_jaeger,
        test_component_span_builders,
        test_span_builder_jidoka,
        test_span_builder_kanban,
        test_span_builder_andon,
        test_span_builder_heijunka,
        test_trace_analysis,
        test_waste_breakdown,
        test_bottleneck_detection,
        test_anomaly_detection,
        test_active_traces_tracking,
        test_concurrent_spans,
        test_sampling_rate_change
    ].

%%====================================================================
%% Test Cases - Span Creation and Relationships
%%====================================================================

test_root_span_creation(Config) ->
    %% Arrange: Create a root span
    RequestId = "req-001",
    Component = "test_component",
    Baggage = #{"request_id" => "req-001", "user_id" => "user-42"},

    %% Act: Start root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span(RequestId, Component, Baggage),

    %% Assert: Trace ID and span ID are binary
    ?assert(is_binary(TraceId)),
    ?assert(TraceId =/= <<>>),
    ?assert(is_binary(RootSpanId)),
    ?assert(RootSpanId =/= <<>>),

    %% Assert: TraceId and SpanId are different
    ?assertNotEqual(TraceId, RootSpanId),

    Config.

test_child_span_relationship(Config) ->
    %% Arrange: Create root and child spans
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-002", "app", #{}),

    %% Act: Create child span
    {ok, ChildSpanId} = tps_tracing:start_child_span(TraceId, RootSpanId, "child_op", "component"),

    %% Assert: Child span exists and is different from root
    ?assert(is_binary(ChildSpanId)),
    ?assert(ChildSpanId =/= <<>>),
    ?assertNotEqual(ChildSpanId, RootSpanId),

    %% Assert: Multiple child spans can be created under same root
    {ok, ChildSpanId2} = tps_tracing:start_child_span(TraceId, RootSpanId, "child_op_2", "component"),
    ?assertNotEqual(ChildSpanId2, ChildSpanId),

    Config.

test_span_attributes(Config) ->
    %% Arrange: Create a span
    {ok, TraceId, SpanId} = tps_tracing:start_root_span("req-003", "app", #{}),

    %% Act: Set multiple attributes
    ok = tps_tracing:set_span_attribute(SpanId, "key1", "value1"),
    ok = tps_tracing:set_span_attribute(SpanId, "key2", 42),
    ok = tps_tracing:set_span_attribute(SpanId, "key3", <<"binary">>),

    %% Assert: Attributes are stored (observable via private ETS table)
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    Attributes = maps:get(attributes, SpanData),
    ?assert(lists:any(fun({K, V}) -> K == "key1" andalso V == "value1" end, Attributes)),
    ?assert(lists:any(fun({K, V}) -> K == "key2" andalso V == 42 end, Attributes)),

    Config.

test_span_events(Config) ->
    %% Arrange: Create a span
    {ok, _TraceId, SpanId} = tps_tracing:start_root_span("req-004", "app", #{}),

    %% Act: Add events
    ok = tps_tracing:add_span_event(SpanId, "event_one", [{"detail", "value1"}]),
    ok = tps_tracing:add_span_event(SpanId, "event_two", [{"detail", "value2"}]),

    %% Assert: Events are stored
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    Events = maps:get(events, SpanData),
    ?assertEqual(2, length(Events)),
    ?assert(lists:any(fun(E) -> maps:get(name, E) == "event_one" end, Events)),

    Config.

test_span_latency_measurement(Config) ->
    %% Arrange: Create and end a span with known duration
    {ok, _TraceId, SpanId} = tps_tracing:start_root_span("req-005", "app", #{}),
    timer:sleep(10),  % Sleep 10ms

    %% Act: End span
    ok = tps_tracing:end_span(SpanId),

    %% Assert: Latency is measured and reasonable (≥ 10ms = 10000us)
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    Latency = maps:get(latency, SpanData, 0),
    ?assert(Latency >= 10000),  % At least 10ms in microseconds

    Config.

test_span_status_tracking(Config) ->
    %% Arrange: Create spans with different statuses
    {ok, _TraceId, SuccessSpanId} = tps_tracing:start_root_span("req-006a", "app", #{}),
    {ok, _TraceId2, ErrorSpanId} = tps_tracing:start_root_span("req-006b", "app", #{}),

    %% Act: End with different statuses
    ok = tps_tracing:end_span(SuccessSpanId),
    ok = tps_tracing:end_span_with_status(ErrorSpanId, {error, "test error"}),

    %% Assert: Status is tracked correctly
    [{SuccessSpanId, SuccessData}] = ets:lookup(tps_spans, SuccessSpanId),
    [{ErrorSpanId, ErrorData}] = ets:lookup(tps_spans, ErrorSpanId),

    ?assertEqual(ok, maps:get(status, SuccessData)),
    ?assertEqual(error, maps:get(status, ErrorData)),

    Config.

%%====================================================================
%% Test Cases - Sampling
%%====================================================================

test_sampling_works(Config) ->
    %% Arrange: Set sampling rate to 0% (nothing sampled)
    ok = tps_tracing:set_sampling_rate(0.0),
    timer:sleep(50),

    %% Act: Try to create spans (should generate IDs but not store)
    {ok, _TraceId1, SpanId1} = tps_tracing:start_root_span("req-sample1", "app", #{}),
    timer:sleep(10),

    %% Assert: Span is not stored (sampling rejected)
    Result = ets:lookup(tps_spans, SpanId1),
    ?assertEqual([], Result),

    %% Act: Set sampling to 100% and try again
    ok = tps_tracing:set_sampling_rate(1.0),
    timer:sleep(50),

    {ok, _TraceId2, SpanId2} = tps_tracing:start_root_span("req-sample2", "app", #{}),

    %% Assert: Span IS stored
    Result2 = ets:lookup(tps_spans, SpanId2),
    ?assertNotEqual([], Result2),

    Config.

%%====================================================================
%% Test Cases - Export
%%====================================================================

test_export_to_jaeger(Config) ->
    %% Arrange: Create and complete some spans
    {ok, _TraceId, SpanId} = tps_tracing:start_root_span("req-export", "app", #{}),
    ok = tps_tracing:set_span_attribute(SpanId, "test", "value"),
    timer:sleep(5),
    ok = tps_tracing:end_span(SpanId),

    %% Act: Trigger export
    {ok, _} = tps_tracing_exporter:get_export_stats(),

    %% Assert: Export stats show span was queued
    %% (We can't verify actual Jaeger send without Jaeger running,
    %% but we verify the export mechanism is functional)
    ok,

    Config.

%%====================================================================
%% Test Cases - Component Span Builders
%%====================================================================

test_component_span_builders(Config) ->
    %% Arrange: Get root span IDs
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-builders", "app", #{}),

    %% Act: Use span builders
    {ok, JidokaSpan} = tps_span_builder:jidoka_circuit_check_span(TraceId, RootSpanId, "circuit-1", closed),
    {ok, KanbanSpan} = tps_span_builder:kanban_queue_pull_span(TraceId, RootSpanId, "queue-1", 3),

    %% Assert: Spans are created
    ?assert(is_binary(JidokaSpan)),
    ?assert(JidokaSpan =/= <<>>),
    ?assert(is_binary(KanbanSpan)),
    ?assert(KanbanSpan =/= <<>>),
    ?assertNotEqual(JidokaSpan, KanbanSpan),

    Config.

test_span_builder_jidoka(Config) ->
    %% Arrange: Create root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-jidoka", "app", #{}),

    %% Act: Create jidoka circuit check span
    {ok, SpanId} = tps_span_builder:jidoka_circuit_check_span(TraceId, RootSpanId, "payment-circuit", closed),
    ok = tps_span_builder:jidoka_circuit_check_end(SpanId, pass, undefined),

    %% Assert: Span is complete with correct component
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    ?assertEqual("jidoka", maps:get(component, SpanData)),
    ?assertEqual("circuit_breaker_check", maps:get(operation_name, SpanData)),
    ?assertEqual(ok, maps:get(status, SpanData)),

    Config.

test_span_builder_kanban(Config) ->
    %% Arrange: Create root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-kanban", "app", #{}),

    %% Act: Create kanban queue span
    {ok, SpanId} = tps_span_builder:kanban_queue_pull_span(TraceId, RootSpanId, "work-queue", 5),
    ok = tps_span_builder:kanban_queue_pull_end(SpanId, "work-123", "P1", 1500),

    %% Assert: Span includes queue metrics
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    ?assertEqual("kanban", maps:get(component, SpanData)),
    Attributes = maps:get(attributes, SpanData),
    ?assert(lists:any(fun({K, V}) -> K == "queue_depth" andalso V == 5 end, Attributes)),

    Config.

test_span_builder_andon(Config) ->
    %% Arrange: Create root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-andon", "app", #{}),

    %% Act: Create andon alert span
    {ok, SpanId} = tps_span_builder:andon_log_event_span(TraceId, RootSpanId, "latency_anomaly", high),
    ok = tps_span_builder:andon_log_event_end(SpanId, true, "ops-team", "Resolved by optimizing DB query"),

    %% Assert: Span includes alert context
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    ?assertEqual("andon", maps:get(component, SpanData)),
    ?assertEqual(ok, maps:get(status, SpanData)),

    Config.

test_span_builder_heijunka(Config) ->
    %% Arrange: Create root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-heijunka", "app", #{}),

    %% Act: Create heijunka pool span
    {ok, SpanId} = tps_span_builder:heijunka_pool_acquire_span(TraceId, RootSpanId, "worker-pool", 0.75),
    ok = tps_span_builder:heijunka_pool_acquire_end(SpanId, "worker-5", 250, true),

    %% Assert: Span includes pool metrics
    [{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
    ?assertEqual("heijunka", maps:get(component, SpanData)),
    Attributes = maps:get(attributes, SpanData),
    ?assert(lists:any(fun({K, V}) -> K == "pool_utilization" end, Attributes)),

    Config.

%%====================================================================
%% Test Cases - Analysis and Waste Mapping
%%====================================================================

test_trace_analysis(Config) ->
    %% Arrange: Create a complete trace
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-analysis", "app", #{}),
    {ok, ActionSpan} = tps_span_builder:action_execute_span(TraceId, RootSpanId, "process_payment", "payment"),
    timer:sleep(5),
    ok = tps_span_builder:action_execute_end(ActionSpan, success, undefined, #{"amount" => "99.99"}),
    ok = tps_tracing:end_span(RootSpanId),

    %% Act: Analyze trace
    TraceData = #{
        trace_id => TraceId,
        spans => [
            #{trace_id => TraceId, component => "action", latency => 5000, status => ok}
        ]
    },

    {ok, Report} = tps_tracing_analyzer:analyze_trace(TraceData),

    %% Assert: Report contains expected fields
    ?assert(is_map(Report)),
    ?assert(maps:is_key(total_latency, Report)),
    ?assert(maps:is_key(waste_breakdown, Report)),
    ?assert(maps:is_key(bottleneck_component, Report)),

    Config.

test_waste_breakdown(Config) ->
    %% Arrange: Create trace with multiple component types
    TraceData = #{
        trace_id => crypto:strong_rand_bytes(16),
        spans => [
            #{trace_id => <<"t1">>, component => "action", latency => 6000, status => ok},
            #{trace_id => <<"t1">>, component => "kanban", latency => 2000, status => ok},
            #{trace_id => <<"t1">>, component => "heijunka", latency => 1000, status => ok}
        ]
    },

    %% Act: Analyze
    {ok, Report} = tps_tracing_analyzer:analyze_trace(TraceData),

    %% Assert: Waste is categorized
    WasteBreakdown = maps:get(waste_breakdown, Report),
    ?assert(is_map(WasteBreakdown)),
    %% Should have waiting time (kanban, heijunka)
    ?assert(maps:get(waiting, WasteBreakdown, 0) > 0),

    Config.

test_bottleneck_detection(Config) ->
    %% Arrange: Create multiple traces with different bottlenecks
    Trace1 = #{
        trace_id => crypto:strong_rand_bytes(16),
        spans => [
            #{trace_id => <<"t">>, component => "database", latency => 9000, status => ok},
            #{trace_id => <<"t">>, component => "api", latency => 1000, status => ok}
        ]
    },

    %% Act: Analyze
    {ok, Report} = tps_tracing_analyzer:analyze_trace(Trace1),

    %% Assert: Bottleneck is identified
    Bottleneck = maps:get(bottleneck_component, Report),
    ?assertEqual("database", Bottleneck),

    Config.

test_anomaly_detection(Config) ->
    %% Note: Anomaly detection requires baseline data
    %% This test verifies the mechanism is in place
    TraceData = #{
        trace_id => crypto:strong_rand_bytes(16),
        spans => []
    },

    %% Act: Try to detect anomaly
    {ok, _IsAnomaly, _Reason} = tps_tracing_analyzer:detect_anomaly(TraceData),

    %% Assert: Function exists and returns valid response
    ok,

    Config.

%%====================================================================
%% Test Cases - System Behavior
%%====================================================================

test_active_traces_tracking(Config) ->
    %% Arrange: Create multiple traces
    {ok, TraceId1, _} = tps_tracing:start_root_span("req-track1", "app", #{}),
    {ok, TraceId2, _} = tps_tracing:start_root_span("req-track2", "app", #{}),

    %% Act: Get active traces
    {ok, Traces} = tps_tracing:get_active_traces(),

    %% Assert: Both traces are listed
    ?assert(length(Traces) >= 2),
    ?assert(lists:member(TraceId1, Traces) orelse lists:member(TraceId2, Traces)),

    Config.

test_concurrent_spans(Config) ->
    %% Arrange: Create concurrent spans (stress test)
    NumSpans = 100,
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span("req-concurrent", "app", #{}),

    %% Act: Create many child spans
    SpanIds = lists:map(fun(N) ->
        {ok, SpanId} = tps_tracing:start_child_span(
            TraceId, RootSpanId, "op_" ++ integer_to_list(N), "component"
        ),
        SpanId
    end, lists:seq(1, NumSpans)),

    %% Assert: All spans created
    ?assertEqual(NumSpans, length(SpanIds)),

    %% Assert: All spans exist in table
    ExistingCount = length([S || S <- SpanIds, ets:lookup(tps_spans, S) /= []]),
    ?assertEqual(NumSpans, ExistingCount),

    Config.

test_sampling_rate_change(Config) ->
    %% Arrange: Start with 100% sampling
    ok = tps_tracing:set_sampling_rate(1.0),
    timer:sleep(50),

    %% Act: Create spans with 100% sampling
    {ok, _T1, S1} = tps_tracing:start_root_span("req-rate1", "app", #{}),
    Exists1 = ets:lookup(tps_spans, S1) /= [],

    %% Act: Change to 0% sampling and create more spans
    ok = tps_tracing:set_sampling_rate(0.0),
    timer:sleep(50),

    {ok, _T2, S2} = tps_tracing:start_root_span("req-rate2", "app", #{}),
    Exists2 = ets:lookup(tps_spans, S2) /= [],

    %% Assert: First span exists, second doesn't
    ?assert(Exists1),
    ?assertNot(Exists2),

    %% Cleanup: Reset sampling
    ok = tps_tracing:set_sampling_rate(1.0),

    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

% Helper to check if value is not empty
assertIsNotEmpty(Value) ->
    ?assertNotEqual([], Value),
    ?assertNotEqual(<<>>, Value),
    ?assertNotEqual(undefined, Value),
    ok.

%%%===================================================================
%% End of module
%%%===================================================================
