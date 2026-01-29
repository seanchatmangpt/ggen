%% ============================================================================
%% HTTP Server Unit Tests
%% ============================================================================
-module(http_server_test).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Setup/Teardown
%% ============================================================================

setup() ->
    %% Start the supervisor (which starts the server)
    {ok, Pid} = http_sup:start_link(8080),
    Pid.

teardown(Pid) ->
    %% Stop the supervisor
    exit(Pid, normal).

%% ============================================================================
%% Tests
%% ============================================================================

http_server_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_server_starts/1,
      fun test_handle_get_request/1,
      fun test_handle_post_request/1,
      fun test_stats_tracking/1,
      fun test_404_handling/1
     ]}.

%% Test: Server starts successfully
test_server_starts(_Pid) ->
    fun() ->
        %% Server should be registered
        ?assert(whereis(http_server) =/= undefined)
    end.

%% Test: GET requests work
test_handle_get_request(_Pid) ->
    fun() ->
        %% Test root path
        {Status, Body} = http_server:handle_request('GET', "/"),
        ?assertEqual(200, Status),
        ?assertMatch(#{<<"content">> := _}, Body),

        %% Test health endpoint
        {Status2, Body2} = http_server:handle_request('GET', "/health"),
        ?assertEqual(200, Status2),
        ?assertEqual(#{<<"status">> => <<"ok">>}, Body2)
    end.

%% Test: POST requests work
test_handle_post_request(_Pid) ->
    fun() ->
        {Status, Body} = http_server:handle_request('POST', "/api/data"),
        ?assertEqual(201, Status),
        ?assertEqual(#{<<"created">> => true}, Body)
    end.

%% Test: Statistics are tracked
test_stats_tracking(_Pid) ->
    fun() ->
        %% Make some requests
        http_server:handle_request('GET', "/"),
        http_server:handle_request('GET', "/health"),
        http_server:handle_request('POST', "/api/data"),

        %% Check stats
        Stats = http_server:get_stats(),
        ?assertMatch(#{requests := Count} when Count >= 3, Stats),
        ?assertMatch(#{port := 8080}, Stats)
    end.

%% Test: 404 for unknown paths
test_404_handling(_Pid) ->
    fun() ->
        {Status, Body} = http_server:handle_request('GET', "/unknown"),
        ?assertEqual(404, Status),
        ?assertEqual(#{<<"error">> => <<"Not Found">>}, Body)
    end.

%% ============================================================================
%% Performance Tests (basic)
%% ============================================================================

throughput_test() ->
    %% Start server
    {ok, Pid} = http_sup:start_link(8080),

    %% Make 1000 requests
    Start = erlang:system_time(millisecond),
    [http_server:handle_request('GET', "/") || _ <- lists:seq(1, 1000)],
    End = erlang:system_time(millisecond),

    Duration = End - Start,
    Throughput = 1000 / (Duration / 1000),

    %% Should handle at least 1000 req/sec
    ?assert(Throughput > 1000),

    %% Cleanup
    exit(Pid, normal).

%% ============================================================================
%% Crash Recovery Tests
%% ============================================================================

crash_recovery_test() ->
    %% Start supervisor
    {ok, SupPid} = http_sup:start_link(8080),

    %% Get initial server pid
    ServerPid1 = whereis(http_server),
    ?assert(ServerPid1 =/= undefined),

    %% Kill the server (simulate crash)
    exit(ServerPid1, kill),

    %% Wait for restart
    timer:sleep(100),

    %% Server should be restarted with new pid
    ServerPid2 = whereis(http_server),
    ?assert(ServerPid2 =/= undefined),
    ?assert(ServerPid1 =/= ServerPid2),

    %% Server should still work
    {Status, _} = http_server:handle_request('GET', "/health"),
    ?assertEqual(200, Status),

    %% Cleanup
    exit(SupPid, normal).

%% ============================================================================
%% Test Report
%% ============================================================================
%% Run with: rebar3 eunit
%%
%% Expected output:
%% - All tests pass
%% - Throughput > 1000 req/sec
%% - Crash recovery works
%% ============================================================================
