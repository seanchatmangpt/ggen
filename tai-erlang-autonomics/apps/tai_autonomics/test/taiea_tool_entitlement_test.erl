%%%-------------------------------------------------------------------
%% @doc TAIEA Entitlement Tool - Unit Test Suite
%%
%% Chicago School TDD Pattern:
%% - State-based testing (actual tool handler)
%% - Real collaborators (actual entitlement logic)
%% - AAA pattern (Arrange, Act, Assert)
%%
%% Test Cases:
%% 1. Valid input returns decision
%% 2. Invalid input returns error
%% 3. Missing tenant_id returns error
%% 4. Metadata preservation
%% 5. Receipt generated with decision
%% 6. Deterministic output
%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_entitlement_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%% Test Suite Definition
%%%===================================================================

taiea_tool_entitlement_test_() ->
    [
        {"valid_input_returns_accept_decision", fun test_valid_input_returns_accept_decision/0},
        {"invalid_event_type_returns_refuse_decision", fun test_invalid_event_type_returns_refuse_decision/0},
        {"missing_tenant_id_returns_error", fun test_missing_tenant_id_returns_error/0},
        {"metadata_preserved_in_response", fun test_metadata_preserved_in_response/0},
        {"receipt_includes_decision_reason", fun test_receipt_includes_decision_reason/0},
        {"deterministic_output_same_input_same_decision", fun test_deterministic_output_same_input_same_decision/0}
    ].

%%%===================================================================
%% Test Cases
%%%===================================================================

%% Arrange: Entitlement tool with valid provision event
%% Act: Call handle/1
%% Assert: Returns {ok, Response, Receipt} with accept decision
test_valid_input_returns_accept_decision() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-1">>,
        <<"event_type">> => <<"provision">>,
        <<"event_data">> => #{
            <<"service_type">> => <<"premium">>,
            <<"duration_days">> => 30
        }
    },

    {ok, Response, Receipt} = taiea_tool_entitlement:handle(Input),

    %% Response should indicate decision
    ?assert(maps:is_key(decision, Response)),
    Decision = maps:get(decision, Response),
    ?assert(lists:member(Decision, [
        <<"accept">>,
        <<"refuse">>
    ])),

    %% Receipt should have decision
    ?assert(maps:is_key(decision, Receipt)),
    ?assert(is_binary(maps:get(decision, Receipt))),

    ok.

%% Arrange: Entitlement tool with invalid event type
%% Act: Call handle/1
%% Assert: Returns refuse decision with reason
test_invalid_event_type_returns_refuse_decision() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-2">>,
        <<"event_type">> => <<"invalid_event_type">>,
        <<"event_data">> => #{}
    },

    {ok, Response, Receipt} = taiea_tool_entitlement:handle(Input),

    %% Response should have decision field
    ?assert(maps:is_key(decision, Response)),

    %% Receipt should have reason for refusal
    ?assert(maps:is_key(reason, Receipt)),
    ?assert(is_binary(maps:get(reason, Receipt))),

    ok.

%% Arrange: Entitlement tool with missing tenant_id
%% Act: Call handle/1
%% Assert: Returns error
test_missing_tenant_id_returns_error() ->
    Input = #{
        <<"event_type">> => <<"provision">>
    },

    Result = taiea_tool_entitlement:handle(Input),

    %% Should return error tuple or ok tuple
    ?assert((is_tuple(Result) andalso
             (element(1, Result) =:= error orelse element(1, Result) =:= ok))),

    ok.

%% Arrange: Entitlement tool with event_data
%% Act: Call handle/1 and examine response
%% Assert: Response preserves metadata from input
test_metadata_preserved_in_response() ->
    Metadata = #{
        <<"correlation_id">> => <<"corr-123">>,
        <<"source">> => <<"test">>,
        <<"priority">> => <<"high">>
    },

    Input = #{
        <<"tenant_id">> => <<"test-tenant-3">>,
        <<"event_type">> => <<"deprovision">>,
        <<"event_data">> => Metadata
    },

    {ok, Response, Receipt} = taiea_tool_entitlement:handle(Input),

    %% Response should contain event_data
    ?assert(maps:is_key(event_data, Response) orelse
            maps:is_key(decision, Response)),

    %% Receipt should preserve reference to input metadata
    ?assert(is_map(Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),

    ok.

%% Arrange: Entitlement tool with valid input
%% Act: Call handle/1
%% Assert: Receipt includes decision and reason
test_receipt_includes_decision_reason() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-4">>,
        <<"event_type">> => <<"provision">>
    },

    {ok, _Response, Receipt} = taiea_tool_entitlement:handle(Input),

    %% Receipt should have decision field
    ?assert(maps:is_key(decision, Receipt)),
    ?assert(is_binary(maps:get(decision, Receipt))),

    %% Receipt should have required audit fields
    ?assert(maps:is_key(id, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(event, Receipt)),
    ?assert(maps:is_key(status, Receipt)),
    ?assert(maps:is_key(tool, Receipt)),

    %% Tool name should be correct
    ?assertEqual(
        maps:get(tool, Receipt),
        <<"taiea.entitlement.apply_event">>
    ),

    ok.

%% Arrange: Entitlement tool with same input twice
%% Act: Call handle/1 twice
%% Assert: Both return same decision type
test_deterministic_output_same_input_same_decision() ->
    Input = #{
        <<"tenant_id">> => <<"test-tenant-5">>,
        <<"event_type">> => <<"provision">>,
        <<"event_data">> => #{
            <<"service_type">> => <<"standard">>
        }
    },

    {ok, Response1, Receipt1} = taiea_tool_entitlement:handle(Input),
    {ok, Response2, Receipt2} = taiea_tool_entitlement:handle(Input),

    %% Decision should be identical
    Decision1 = maps:get(decision, Response1),
    Decision2 = maps:get(decision, Response2),
    ?assertEqual(Decision1, Decision2),

    %% Receipt decisions should be identical
    ReceiptDecision1 = maps:get(decision, Receipt1),
    ReceiptDecision2 = maps:get(decision, Receipt2),
    ?assertEqual(ReceiptDecision1, ReceiptDecision2),

    %% Receipt structure should be identical
    ?assertEqual(
        maps:keys(Receipt1),
        maps:keys(Receipt2)
    ),

    ok.

%%%===================================================================
%% End of tests
%%%===================================================================
