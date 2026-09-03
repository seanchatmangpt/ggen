%%%-------------------------------------------------------------------
%% @doc Unit tests for profiler module
%% @end
%%%-------------------------------------------------------------------

-module(profiler_tests).
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

%% Test that profiler starts and stops successfully
start_stop_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Pid} = profiler:start_link(),
                        ?assert(is_pid(Pid)),
                        ok = profiler:stop()
                    end
                )
            ]
        end
    }.

%% Test profiler status retrieval
get_status_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Status} = profiler:get_profile_status(),
                        ?assert(is_map(Status)),
                        ?assert(maps:is_key(cpu_profiling, Status))
                    end
                )
            ]
        end
    }.

%% Test CPU profiling control
cpu_profiling_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Result1 = profiler:start_cpu_profile(),
                        ?assert(Result1 =:= ok orelse element(1, Result1) =:= error),

                        % Stop profiling if it started
                        case Result1 of
                            ok -> profiler:stop_cpu_profile();
                            _ -> ok
                        end
                    end
                )
            ]
        end
    }.

%% Test memory profiling control
memory_profiling_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Result1 = profiler:start_memory_profile(),
                        ?assert(Result1 =:= ok),

                        Result2 = profiler:stop_memory_profile(),
                        ?assert(Result2 =:= ok)
                    end
                )
            ]
        end
    }.
