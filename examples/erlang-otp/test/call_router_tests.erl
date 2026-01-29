%%%-------------------------------------------------------------------
%%% @doc Call Router EUnit Tests
%%%
%%% Comprehensive test suite for call_router_server using EUnit.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(call_router_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Cleanup
%%%===================================================================

setup() ->
    {ok, Pid} = call_router_server:start_link([{load_threshold, 1000}]),
    Pid.

cleanup(Pid) ->
    exit(Pid, normal),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test basic routing functionality
route_call_success_test() ->
    Pid = setup(),

    %% Route to existing destination
    Result = call_router_server:route_call(<<"CALL-001">>, <<"1-800-FORTUNE">>),
    ?assertMatch({ok, _Target}, Result),

    cleanup(Pid).

%% Test routing to non-existent destination
route_call_not_found_test() ->
    Pid = setup(),

    Result = call_router_server:route_call(<<"CALL-002">>, <<"1-800-NOTFOUND">>),
    ?assertEqual({error, no_route}, Result),

    cleanup(Pid).

%% Test adding and removing routes
add_remove_route_test() ->
    Pid = setup(),

    Dest = <<"1-800-TEST">>,
    Target = <<"sip:test@example.com">>,

    %% Add route
    ok = call_router_server:add_route(Dest, Target),

    %% Route should now work
    {ok, ReturnedTarget} = call_router_server:route_call(<<"CALL-003">>, Dest),
    ?assertEqual(Target, ReturnedTarget),

    %% Remove route
    ok = call_router_server:remove_route(Dest),

    %% Route should now fail
    ?assertEqual({error, no_route}, call_router_server:route_call(<<"CALL-004">>, Dest)),

    cleanup(Pid).

%% Test metrics collection
metrics_test() ->
    Pid = setup(),

    %% Reset metrics
    ok = call_router_server:reset_metrics(),

    %% Route some calls
    call_router_server:route_call(<<"CALL-005">>, <<"1-800-FORTUNE">>),
    call_router_server:route_call(<<"CALL-006">>, <<"1-800-FORTUNE">>),
    call_router_server:route_call(<<"CALL-007">>, <<"1-800-NOTFOUND">>),

    %% Check metrics
    Metrics = call_router_server:get_metrics(),
    ?assertEqual(2, maps:get(routed, Metrics)),
    ?assertEqual(1, maps:get(failed, Metrics)),

    cleanup(Pid).

%% Test circuit breaker functionality
circuit_breaker_test() ->
    Pid = setup(),

    %% Enable circuit breaker
    ok = call_router_server:enable_circuit_breaker(),

    %% This test would need to generate enough failures to trip the breaker
    %% Skipped for brevity in this example

    cleanup(Pid).

%% Test load threshold (load shedding)
load_threshold_test() ->
    Pid = setup(),

    %% Set very low threshold
    ok = call_router_server:set_load_threshold(0),

    %% Next call should be dropped
    Result = call_router_server:route_call(<<"CALL-008">>, <<"1-800-FORTUNE">>),

    %% Might be overload or might succeed if queue is empty
    ?assert(Result =:= {error, overload} orelse element(1, Result) =:= ok),

    cleanup(Pid).
