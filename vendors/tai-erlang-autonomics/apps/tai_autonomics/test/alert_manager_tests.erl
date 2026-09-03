%%%-------------------------------------------------------------------
%% @doc Unit tests for alert_manager module
%% @end
%%%-------------------------------------------------------------------

-module(alert_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include alert_rule record definition
-record(alert_rule, {
    name :: atom(),
    metric :: atom(),
    operator :: atom(),
    threshold :: number(),
    severity :: atom(),
    action :: fun() | undefined
}).

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

%% Test that alert_manager starts and stops successfully
start_stop_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Pid} = alert_manager:start_link(),
                        ?assert(is_pid(Pid)),
                        ok = alert_manager:stop()
                    end
                )
            ]
        end
    }.

%% Test that active incidents can be retrieved
get_active_incidents_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        {ok, Incidents} = alert_manager:get_active_incidents(),
                        ?assert(is_list(Incidents))
                    end
                )
            ]
        end
    }.

%% Test alert rule addition
add_alert_rule_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Rule = #alert_rule{
                            name = test_rule,
                            metric = test_metric,
                            operator = '>',
                            threshold = 100,
                            severity = high
                        },
                        ok = alert_manager:add_alert_rule(test_rule, Rule),
                        ?assert(true)
                    end
                )
            ]
        end
    }.

%% Test alert rule removal
remove_alert_rule_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Rule = #alert_rule{
                            name = test_rule,
                            metric = test_metric,
                            operator = '>',
                            threshold = 100,
                            severity = high
                        },
                        ok = alert_manager:add_alert_rule(test_rule, Rule),
                        ok = alert_manager:remove_alert_rule(test_rule),
                        ?assert(true)
                    end
                )
            ]
        end
    }.

%% Test threshold checking with metrics
check_thresholds_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(ok) ->
            [
                ?_test(
                    begin
                        Metrics = #{
                            total_memory => 1000000,
                            process_count => 50,
                            error_count => 5
                        },
                        ok = alert_manager:check_thresholds(Metrics),
                        ?assert(true)
                    end
                )
            ]
        end
    }.
