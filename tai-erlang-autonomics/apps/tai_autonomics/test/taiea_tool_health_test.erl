%%%-------------------------------------------------------------------
%% @doc TAIEA Health Tool - Unit Test Suite
%%
%% Chicago School TDD Pattern:
%% - State-based testing (actual tool handler)
%% - Real collaborators (actual system calls)
%% - AAA pattern (Arrange, Act, Assert)
%%
%% Test Cases:
%% 1. Valid input returns healthy status
%% 2. Response includes all health metrics
%% 3. Receipt is generated with correct structure
%% 4. Deterministic output (same input → same output type)
%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_health_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%% Test Suite Definition
%%%===================================================================

taiea_tool_health_test_() ->
    [
        {"valid_input_returns_healthy_status", fun test_valid_input_returns_healthy_status/0},
        {"response_includes_all_health_metrics", fun test_response_includes_all_health_metrics/0},
        {"receipt_has_correct_structure", fun test_receipt_has_correct_structure/0},
        {"deterministic_output_same_input_same_output", fun test_deterministic_output_same_input_same_output/0}
    ].

%%%===================================================================
%% Test Cases
%%%===================================================================

%% Arrange: Health tool with empty input
%% Act: Call handle/1
%% Assert: Returns {ok, Response, Receipt}
test_valid_input_returns_healthy_status() ->
    Input = #{},

    {ok, Response, Receipt} = taiea_tool_health:handle(Input),

    %% Response should have status field
    ?assert(maps:is_key(status, Response)),
    Status = maps:get(status, Response),
    ?assert(lists:member(Status, [
        <<"healthy">>,
        <<"degraded">>,
        <<"critical">>
    ])),

    %% Receipt should have status
    ?assert(maps:is_key(status, Receipt)),
    ?assert(is_binary(maps:get(status, Receipt))),

    ok.

%% Arrange: Health tool with empty input
%% Act: Call handle/1 and examine response
%% Assert: Response includes timestamp, node, uptime
test_response_includes_all_health_metrics() ->
    Input = #{},

    {ok, Response, _Receipt} = taiea_tool_health:handle(Input),

    %% Response should have required fields
    ?assert(maps:is_key(timestamp, Response)),
    ?assert(maps:is_key(node, Response)),

    %% Response should have checks field
    ?assert(maps:is_key(checks, Response)),
    Checks = maps:get(checks, Response),
    ?assert(is_map(Checks)),

    %% Checks should include health categories
    ?assert(maps:is_key(node, Checks)),
    ?assert(maps:is_key(memory, Checks)),
    ?assert(maps:is_key(processes, Checks)),
    ?assert(maps:is_key(governors, Checks)),

    ok.

%% Arrange: Health tool with empty input
%% Act: Call handle/1 and examine receipt
%% Assert: Receipt has id, timestamp, event, status, tool, metadata
test_receipt_has_correct_structure() ->
    Input = #{},

    {ok, _Response, Receipt} = taiea_tool_health:handle(Input),

    %% Receipt should have required fields
    ?assert(maps:is_key(id, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(event, Receipt)),
    ?assert(maps:is_key(status, Receipt)),
    ?assert(maps:is_key(tool, Receipt)),
    ?assert(maps:is_key(message, Receipt)),
    ?assert(maps:is_key(metadata, Receipt)),

    %% Verify types
    ?assert(is_binary(maps:get(id, Receipt))),
    ?assert(is_integer(maps:get(timestamp, Receipt))),
    ?assert(is_binary(maps:get(event, Receipt))),
    ?assert(is_binary(maps:get(status, Receipt))),
    ?assert(is_binary(maps:get(tool, Receipt))),
    ?assert(maps:get(tool, Receipt) =:= <<"taiea.health.check">>),

    %% Metadata should be a map
    Metadata = maps:get(metadata, Receipt),
    ?assert(is_map(Metadata)),

    ok.

%% Arrange: Health tool with same input twice
%% Act: Call handle/1 twice
%% Assert: Both returns have same structure and status type
test_deterministic_output_same_input_same_output() ->
    Input = #{},

    {ok, Response1, Receipt1} = taiea_tool_health:handle(Input),
    {ok, Response2, Receipt2} = taiea_tool_health:handle(Input),

    %% Response structures should be the same
    ?assertEqual(
        maps:keys(Response1),
        maps:keys(Response2)
    ),

    %% Status type should be the same
    Status1 = maps:get(status, Response1),
    Status2 = maps:get(status, Response2),
    ?assertEqual(Status1, Status2),

    %% Receipt structures should be the same
    ?assertEqual(
        maps:keys(Receipt1),
        maps:keys(Receipt2)
    ),

    %% Tool name should be consistent
    ?assertEqual(
        maps:get(tool, Receipt1),
        maps:get(tool, Receipt2)
    ),

    ok.

%%%===================================================================
%% End of tests
%%%===================================================================
