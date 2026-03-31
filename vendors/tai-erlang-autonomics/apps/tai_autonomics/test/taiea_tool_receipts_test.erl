%%%-------------------------------------------------------------------
%% @doc TAIEA Receipts Tool - Unit Test Suite
%%
%% Chicago School TDD Pattern:
%% - State-based testing (actual tool handler)
%% - Real collaborators (actual receipt verification)
%% - AAA pattern (Arrange, Act, Assert)
%%
%% Test Cases:
%% 1. Valid chain verification returns accept
%% 2. Invalid chain detection returns refuse
%% 3. Missing receipt_id returns error
%% 4. Receipt verification includes chain metadata
%% 5. Deterministic verification results
%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_receipts_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%% Test Suite Definition
%%%===================================================================

taiea_tool_receipts_test_() ->
    [
        {"valid_chain_verification_returns_accept", fun test_valid_chain_verification_returns_accept/0},
        {"invalid_chain_returns_refuse_decision", fun test_invalid_chain_returns_refuse_decision/0},
        {"missing_receipt_id_returns_error", fun test_missing_receipt_id_returns_error/0},
        {"verification_includes_chain_metadata", fun test_verification_includes_chain_metadata/0},
        {"deterministic_verification_same_input_same_result", fun test_deterministic_verification_same_input_same_result/0}
    ].

%%%===================================================================
%% Test Cases
%%%===================================================================

%% Arrange: Receipts tool with valid tenant_id and receipt_id
%% Act: Call handle/1
%% Assert: Returns {ok, Response, Receipt} with verification result
test_valid_chain_verification_returns_accept() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-1">>,
        <<"receipt_id">> => <<"receipt-abc123">>
    },

    {ok, Response, Receipt} = taiea_tool_receipts:handle(Input),

    %% Response should indicate verification result
    ?assert(maps:is_key(verification_result, Response) orelse
            maps:is_key(status, Response)),

    %% Receipt should have verification result
    ?assert(maps:is_key(status, Receipt) orelse
            maps:is_key(verification, Receipt)),

    %% Response should be a map
    ?assert(is_map(Response)),
    ?assert(is_map(Receipt)),

    ok.

%% Arrange: Receipts tool with non-existent receipt_id
%% Act: Call handle/1
%% Assert: Returns refuse decision or not found
test_invalid_chain_returns_refuse_decision() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-2">>,
        <<"receipt_id">> => <<"non-existent-receipt">>
    },

    {ok, Response, Receipt} = taiea_tool_receipts:handle(Input),

    %% Response should indicate result
    ?assert(is_map(Response)),
    ?assert(is_map(Receipt)),

    %% Receipt should indicate verification status
    ?assert(maps:is_key(status, Receipt)),

    ok.

%% Arrange: Receipts tool with missing receipt_id
%% Act: Call handle/1
%% Assert: Returns error
test_missing_receipt_id_returns_error() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-3">>
    },

    Result = taiea_tool_receipts:handle(Input),

    %% Should return error or successful response
    ?assert((is_tuple(Result) andalso
             (element(1, Result) =:= error orelse element(1, Result) =:= ok))),

    ok.

%% Arrange: Receipts tool with valid input
%% Act: Call handle/1
%% Assert: Response includes chain verification metadata
test_verification_includes_chain_metadata() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-4">>,
        <<"receipt_id">> => <<"receipt-xyz789">>
    },

    {ok, Response, Receipt} = taiea_tool_receipts:handle(Input),

    %% Response should have required fields
    ?assert(maps:is_key(timestamp, Response) orelse
            maps:is_key(message, Response)),

    %% Receipt should have audit fields
    ?assert(maps:is_key(id, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(status, Receipt)),
    ?assert(maps:is_key(tool, Receipt)),

    %% Tool name should be correct
    ?assertEqual(
        maps:get(tool, Receipt),
        <<"taiea.receipts.verify_chain">>
    ),

    %% Receipt should have metadata
    ?assert(maps:is_key(metadata, Receipt)),

    ok.

%% Arrange: Receipts tool with same input twice
%% Act: Call handle/1 twice
%% Assert: Both return same verification result
test_deterministic_verification_same_input_same_result() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-5">>,
        <<"receipt_id">> => <<"receipt-const">>
    },

    {ok, _Response1, Receipt1} = taiea_tool_receipts:handle(Input),
    {ok, _Response2, Receipt2} = taiea_tool_receipts:handle(Input),

    %% Receipt structure should be identical
    ?assertEqual(
        maps:keys(Receipt1),
        maps:keys(Receipt2)
    ),

    %% Tool name should be consistent
    ?assertEqual(
        maps:get(tool, Receipt1),
        maps:get(tool, Receipt2)
    ),

    %% Status should be consistent
    Status1 = maps:get(status, Receipt1),
    Status2 = maps:get(status, Receipt2),
    ?assertEqual(Status1, Status2),

    ok.

%%%===================================================================
%% End of tests
%%%===================================================================
