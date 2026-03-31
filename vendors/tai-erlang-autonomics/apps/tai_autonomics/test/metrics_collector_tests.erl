%%%-------------------------------------------------------------------
%% @doc Unit tests for metrics_collector module
%% @end
%%%-------------------------------------------------------------------

-module(metrics_collector_tests).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%% Test Setup/Teardown
%%%-------------------------------------------------------------------

setup() ->
    application:start(observability),
    ok.

teardown(_) ->
    application:stop(observability),
    ok.

%%%-------------------------------------------------------------------
%% Test Cases
%%%-------------------------------------------------------------------

%% Test that metrics_collector starts and stops successfully
start_stop_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Pid} = metrics_collector:start_link(),
                        ?assert(is_pid(Pid)),
                        ok = metrics_collector:stop()
                    end
                )
            ]
        end
    }.

%% Test that collect_metrics returns a valid snapshot
collect_metrics_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Metrics} = metrics_collector:collect_metrics(),
                        ?assert(is_map(Metrics)),
                        ?assertNotEqual(undefined, maps:get(memory, Metrics, undefined)),
                        ?assertNotEqual(undefined, maps:get(processes, Metrics, undefined))
                    end
                )
            ]
        end
    }.

%% Test that memory metrics are collected correctly
memory_metrics_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        MemMetrics = metrics_collector:collect_memory_metrics(),
                        ?assert(is_map(MemMetrics)),
                        Total = maps:get(total, MemMetrics, 0),
                        ?assert(Total > 0)
                    end
                )
            ]
        end
    }.

%% Test that process metrics are collected correctly
process_metrics_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        ProcMetrics = metrics_collector:collect_process_metrics(),
                        ?assert(is_map(ProcMetrics)),
                        Count = maps:get(count, ProcMetrics, 0),
                        ?assert(Count > 0)
                    end
                )
            ]
        end
    }.

%% Test that message queue metrics are collected correctly
message_queue_metrics_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        QueueMetrics = metrics_collector:collect_message_queue_metrics(),
                        ?assert(is_map(QueueMetrics)),
                        ?assert(maps:is_key(total_depth, QueueMetrics))
                    end
                )
            ]
        end
    }.

%% Test that error metrics are collected correctly
error_metrics_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        ErrorMetrics = metrics_collector:collect_error_metrics(),
                        ?assert(is_map(ErrorMetrics)),
                        ?assert(maps:is_key(errors, ErrorMetrics))
                    end
                )
            ]
        end
    }.

%% Test handler registration
register_handler_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Handler = fun(_Snapshot) -> ok end,
                        ok = metrics_collector:register_handler(Handler),
                        % Handler should be registered successfully
                        ?assert(true)
                    end
                )
            ]
        end
    }.

%% Test metrics history retrieval
metrics_history_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, History} = metrics_collector:get_metrics(),
                        ?assert(is_list(History))
                    end
                )
            ]
        end
    }.
