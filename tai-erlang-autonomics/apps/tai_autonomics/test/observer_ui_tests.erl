%%%-------------------------------------------------------------------
%% @doc Unit tests for observer_ui module
%% @end
%%%-------------------------------------------------------------------

-module(observer_ui_tests).
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

%% Test that observer_ui starts and stops successfully
start_stop_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Pid} = observer_ui:start_link(),
                        ?assert(is_pid(Pid)),
                        ok = observer_ui:stop()
                    end
                )
            ]
        end
    }.

%% Test observer status retrieval
get_status_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Status} = observer_ui:get_observer_status(),
                        ?assert(is_map(Status)),
                        ?assert(maps:is_key(system_info, Status))
                    end
                )
            ]
        end
    }.

%% Test observer start/stop
observer_control_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        % Test starting observer (may fail in headless environment, which is OK)
                        StartResult = observer_ui:start_observer(),
                        % Should be ok or error, not crash
                        ?assert(StartResult =:= ok orelse element(1, StartResult) =:= error)
                    end
                )
            ]
        end
    }.

%% Test dump export
export_dump_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        DumpFile = "/tmp/test_dump.bin",
                        ok = observer_ui:export_observer_dump(DumpFile),
                        %% Verify file was created
                        ?assert(filelib:is_file(DumpFile)),
                        %% Clean up
                        file:delete(DumpFile)
                    end
                )
            ]
        end
    }.
