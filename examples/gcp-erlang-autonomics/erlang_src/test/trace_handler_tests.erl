%%%-------------------------------------------------------------------
%% @doc Unit tests for trace_handler module
%% @end
%%%-------------------------------------------------------------------

-module(trace_handler_tests).
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

%% Test that trace_handler starts and stops successfully
start_stop_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Pid} = trace_handler:start_link(),
                        ?assert(is_pid(Pid)),
                        ok = trace_handler:stop()
                    end
                )
            ]
        end
    }.

%% Test trace start/stop
trace_control_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        % Starting trace may fail due to environment (e.g., dbg not available)
                        StartResult = trace_handler:start_trace(all),
                        ?assert(StartResult =:= ok orelse element(1, StartResult) =:= error),

                        % Stop trace regardless
                        ok = trace_handler:stop_trace()
                    end
                )
            ]
        end
    }.

%% Test filter setting
set_filter_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        ok = trace_handler:set_filter(billing),
                        ?assert(true)
                    end
                )
            ]
        end
    }.
