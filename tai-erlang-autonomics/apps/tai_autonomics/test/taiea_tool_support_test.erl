%%%-------------------------------------------------------------------
%% @doc TAIEA Support Tool - Unit Test Suite
%%
%% Chicago School TDD Pattern:
%% - State-based testing (actual tool handler)
%% - Real collaborators (actual support model configuration)
%% - AAA pattern (Arrange, Act, Assert)
%%
%% Test Cases:
%% 1. Valid input returns support model
%% 2. Response includes all tiers
%% 3. Receipt generated with support metadata
%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_support_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%% Test Suite Definition
%%%===================================================================

taiea_tool_support_test_() ->
    [
        {"valid_input_returns_support_model", fun test_valid_input_returns_support_model/0},
        {"response_includes_all_support_tiers", fun test_response_includes_all_support_tiers/0},
        {"receipt_generated_with_support_metadata", fun test_receipt_generated_with_support_metadata/0}
    ].

%%%===================================================================
%% Test Cases
%%%===================================================================

%% Arrange: Support tool with empty input
%% Act: Call handle/1
%% Assert: Returns {ok, Response, Receipt} with support model
test_valid_input_returns_support_model() ->
    Input = #{},

    {ok, Response, Receipt} = taiea_tool_support:handle(Input),

    %% Response should have support model type
    ?assert(maps:is_key(support_model, Response)),
    ?assert(is_binary(maps:get(support_model, Response))),

    %% Receipt should indicate success
    ?assert(maps:is_key(status, Receipt)),
    Status = maps:get(status, Receipt),
    ?assert(lists:member(Status, [
        <<"success">>,
        <<"ok">>
    ])),

    ok.

%% Arrange: Support tool with empty input
%% Act: Call handle/1
%% Assert: Response includes all support tiers
test_response_includes_all_support_tiers() ->
    Input = #{},

    {ok, Response, _Receipt} = taiea_tool_support:handle(Input),

    %% Response should have tiers list
    ?assert(maps:is_key(tiers, Response)),
    Tiers = maps:get(tiers, Response),
    ?assert(is_list(Tiers)),

    %% Should have at least 3 tiers
    ?assert(length(Tiers) >= 3),

    %% Each tier should be a map with name and features
    lists:foreach(fun(Tier) ->
        ?assert(maps:is_key(name, Tier)),
        ?assert(maps:is_key(features, Tier)),
        ?assert(is_binary(maps:get(name, Tier))),
        ?assert(is_list(maps:get(features, Tier)))
    end, Tiers),

    ok.

%% Arrange: Support tool with empty input
%% Act: Call handle/1
%% Assert: Receipt has correct structure and metadata
test_receipt_generated_with_support_metadata() ->
    Input = #{},

    {ok, _Response, Receipt} = taiea_tool_support:handle(Input),

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

    %% Tool name should be correct
    ?assertEqual(
        maps:get(tool, Receipt),
        <<"taiea.support.model">>
    ),

    %% Metadata should be a map
    Metadata = maps:get(metadata, Receipt),
    ?assert(is_map(Metadata)),

    ok.

%%%===================================================================
%% End of tests
%%%===================================================================
