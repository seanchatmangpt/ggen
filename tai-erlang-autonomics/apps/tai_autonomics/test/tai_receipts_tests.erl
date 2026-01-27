%%%-------------------------------------------------------------------
%% @doc Unit tests for tai_receipts module
%%
%% Tests receipt generation, deterministic hashing, and JSON encoding.
%% Tests cover Phase 1 receipt types: health_check, entitlement_event,
%% receipt_verify, support_query, http_request, mcp_tool_call.
%%
%% @end
%%%-------------------------------------------------------------------

-module(tai_receipts_tests).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%% Test Setup/Teardown
%%%-------------------------------------------------------------------

setup() ->
    %% Start ETS tables for receipt storage
    catch ets:delete(tai_receipts_store),
    catch ets:delete(tai_receipts_chain),
    ets:new(tai_receipts_store, [set, named_table, public]),
    ets:new(tai_receipts_chain, [set, named_table, public]),
    application:start(inets),
    ok.

teardown(_) ->
    catch ets:delete(tai_receipts_store),
    catch ets:delete(tai_receipts_chain),
    application:stop(inets),
    ok.

%%%-------------------------------------------------------------------
%% Test Cases: Receipt Creation
%%%-------------------------------------------------------------------

%% Test creating a transition receipt
create_transition_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant-123">>,
                            <<"entitlement-456">>,
                            <<"activate">>,
                            active,
                            #{reason => <<"User requested activation">>}
                        ),
                        ?assert(is_map(Receipt)),
                        ?assertEqual(<<"transition">>, maps:get(type, Receipt)),
                        ?assertEqual(<<"tenant-123">>, maps:get(tenant_id, Receipt)),
                        ?assertEqual(<<"entitlement-456">>, maps:get(entitlement_id, Receipt)),
                        ?assertEqual(<<"active">>, maps:get(state_to, Receipt)),
                        ?assert(is_binary(maps:get(hash, Receipt))),
                        ?assert(is_binary(maps:get(id, Receipt)))
                    end
                )
            ]
        end
    }.

%% Test creating a refusal receipt
create_refusal_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = tai_receipts:create_refusal(quota_exceeded),
                        ?assert(is_map(Receipt)),
                        ?assertEqual(<<"refusal">>, maps:get(type, Receipt)),
                        ?assert(is_binary(maps:get(reason, Receipt))),
                        ?assert(is_binary(maps:get(hash, Receipt))),
                        ?assert(is_binary(maps:get(id, Receipt)))
                    end
                )
            ]
        end
    }.

%% Test creating an action receipt (attempt)
create_action_attempt_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = tai_receipts:create_action_receipt(
                            attempt,
                            <<"tenant-123">>,
                            <<"action-789">>,
                            #{status => <<"in_progress">>}
                        ),
                        ?assert(is_map(Receipt)),
                        ?assertEqual(<<"action_attempt">>, maps:get(type, Receipt)),
                        ?assertEqual(<<"tenant-123">>, maps:get(tenant_id, Receipt)),
                        ?assertEqual(<<"action-789">>, maps:get(action_id, Receipt)),
                        ?assert(is_binary(maps:get(hash, Receipt)))
                    end
                )
            ]
        end
    }.

%% Test creating an action receipt (result)
create_action_result_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = tai_receipts:create_action_receipt(
                            result,
                            <<"tenant-123">>,
                            <<"action-789">>,
                            #{status => <<"completed">>, duration_ms => 1250}
                        ),
                        ?assert(is_map(Receipt)),
                        ?assertEqual(<<"action_result">>, maps:get(type, Receipt)),
                        ?assertEqual(<<"tenant-123">>, maps:get(tenant_id, Receipt)),
                        ?assertEqual(<<"action-789">>, maps:get(action_id, Receipt)),
                        ?assert(is_binary(maps:get(hash, Receipt)))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Deterministic Hashing
%%%-------------------------------------------------------------------

%% Test that same receipt produces same hash (determinism)
deterministic_hash_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt1 = #{
                            id => <<"test-receipt-1">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>,
                            decision => accept
                        },
                        Hash1 = tai_receipts:calculate_hash(Receipt1),
                        Hash2 = tai_receipts:calculate_hash(Receipt1),
                        ?assertEqual(Hash1, Hash2)
                    end
                )
            ]
        end
    }.

%% Test that different receipts produce different hashes
different_hash_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt1 = #{
                            id => <<"test-receipt-1">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>
                        },
                        Receipt2 = #{
                            id => <<"test-receipt-2">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>
                        },
                        Hash1 = tai_receipts:calculate_hash(Receipt1),
                        Hash2 = tai_receipts:calculate_hash(Receipt2),
                        ?assertNotEqual(Hash1, Hash2)
                    end
                )
            ]
        end
    }.

%% Test hash is SHA-256 (64 hex chars)
hash_format_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"test-receipt">>,
                            type => <<"health_check">>,
                            timestamp => 1000
                        },
                        Hash = tai_receipts:calculate_hash(Receipt),
                        ?assert(is_binary(Hash)),
                        ?assertEqual(32, byte_size(Hash))  %% SHA-256 is 32 bytes
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: JSON Encoding
%%%-------------------------------------------------------------------

%% Test converting receipt to JSON
to_json_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"test-123">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"http">>
                        },
                        Json = tai_receipts:to_json(Receipt),
                        ?assert(is_binary(Json)),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"test-123">>, maps:get(<<"id">>, DecodedJson)),
                        ?assertEqual(<<"health_check">>, maps:get(<<"type">>, DecodedJson)),
                        ?assertEqual(1000, maps:get(<<"timestamp">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test JSON encoding preserves complex metadata
json_metadata_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Metadata = #{
                            <<"request_id">> => <<"req-123">>,
                            <<"user_agent">> => <<"Mozilla/5.0">>,
                            <<"headers">> => #{<<"Content-Type">> => <<"application/json">>}
                        },
                        Receipt = #{
                            id => <<"test-123">>,
                            type => <<"http_request">>,
                            timestamp => 1000,
                            metadata => Metadata
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        DecodedMetadata = maps:get(<<"metadata">>, DecodedJson),
                        ?assertEqual(<<"req-123">>, maps:get(<<"request_id">>, DecodedMetadata)),
                        ?assertEqual(<<"Mozilla/5.0">>, maps:get(<<"user_agent">>, DecodedMetadata))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Receipt Emission
%%%-------------------------------------------------------------------

%% Test emitting receipt to stdout
emit_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"test-123">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        Result = tai_receipts:emit(Receipt),
                        ?assertEqual(ok, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Receipt Storage and Retrieval
%%%-------------------------------------------------------------------

%% Test storing a receipt and retrieving it
store_and_retrieve_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"test-123">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        ok = tai_receipts:store_receipt(Receipt),
                        {ok, RetrievedReceipt} = tai_receipts:get_receipt(<<"test-123">>),
                        ?assertEqual(Receipt, RetrievedReceipt)
                    end
                )
            ]
        end
    }.

%% Test retrieving non-existent receipt returns error
get_nonexistent_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Result = tai_receipts:get_receipt(<<"nonexistent-123">>),
                        ?assertEqual({error, not_found}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Chain Hashing and Verification
%%%-------------------------------------------------------------------

%% Test chain hash is different from receipt hash
chain_hash_differs_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        PrevHash = crypto:hash(sha256, <<"previous">>),
                        Receipt = #{
                            id => <<"test-123">>,
                            type => <<"health_check">>,
                            timestamp => 1000
                        },
                        CurrentHash = tai_receipts:calculate_hash(Receipt),
                        ChainHash = tai_receipts:calculate_chain_hash(PrevHash, CurrentHash),
                        ?assertNotEqual(CurrentHash, ChainHash)
                    end
                )
            ]
        end
    }.

%% Test verify_chain with valid chain
verify_valid_chain_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt1 = #{
                            id => <<"test-1">>,
                            type => <<"health_check">>,
                            timestamp => 1000
                        },
                        Hash1 = tai_receipts:calculate_hash(Receipt1),
                        ChainHash1 = tai_receipts:calculate_chain_hash(<<>>, Hash1),
                        ReceiptWithChain1 = Receipt1#{hash => Hash1, chain_hash => ChainHash1},

                        Receipt2 = #{
                            id => <<"test-2">>,
                            type => <<"health_check">>,
                            timestamp => 2000
                        },
                        Hash2 = tai_receipts:calculate_hash(Receipt2),
                        ChainHash2 = tai_receipts:calculate_chain_hash(ChainHash1, Hash2),
                        ReceiptWithChain2 = Receipt2#{hash => Hash2, chain_hash => ChainHash2},

                        Result = tai_receipts:verify_chain([ReceiptWithChain1, ReceiptWithChain2]),
                        ?assertEqual({ok, valid}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Phase 1 Receipt Types
%%%-------------------------------------------------------------------

%% Test health_check receipt type
health_check_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"health-check-1">>,
                            type => <<"health_check">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"http">>,
                            metadata => #{
                                status => <<"healthy">>,
                                response_time_ms => 45
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"health_check">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test entitlement_event receipt type
entitlement_event_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"entitlement-event-1">>,
                            type => <<"entitlement_event">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"mcp">>,
                            metadata => #{
                                entitlement_id => <<"ent-456">>,
                                event => <<"activated">>,
                                sku_id => <<"sku-789">>
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"entitlement_event">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test receipt_verify receipt type
receipt_verify_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"receipt-verify-1">>,
                            type => <<"receipt_verify">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"http">>,
                            metadata => #{
                                receipt_id => <<"test-123">>,
                                verification_status => <<"valid">>,
                                chain_depth => 5
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"receipt_verify">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test support_query receipt type
support_query_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"support-query-1">>,
                            type => <<"support_query">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"partial">>,
                            reason => <<"Rate limited">>,
                            source => <<"http">>,
                            metadata => #{
                                query_id => <<"query-123">>,
                                category => <<"billing">>
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"support_query">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test http_request receipt type
http_request_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"http-request-1">>,
                            type => <<"http_request">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"http">>,
                            metadata => #{
                                method => <<"POST">>,
                                path => <<"/api/v1/entitlements">>,
                                status_code => 200,
                                response_time_ms => 125
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"http_request">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%% Test mcp_tool_call receipt type
mcp_tool_call_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        Receipt = #{
                            id => <<"mcp-tool-call-1">>,
                            type => <<"mcp_tool_call">>,
                            timestamp => erlang:system_time(millisecond),
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>,
                            source => <<"mcp">>,
                            metadata => #{
                                tool_id => <<"tool-123">>,
                                tool_name => <<"create-entitlement">>,
                                input_hash => <<"abc123">>,
                                result_code => 0
                            }
                        },
                        Json = tai_receipts:to_json(Receipt),
                        DecodedJson = jsx:decode(Json, [return_maps]),
                        ?assertEqual(<<"mcp_tool_call">>, maps:get(<<"type">>, DecodedJson))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases: Sample Receipts (Fixtures)
%%%-------------------------------------------------------------------

%% Test creating multiple different receipt types and storing them
multiple_receipt_types_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Create 6 different Phase 1 receipt types
                        HealthCheck = #{
                            id => <<"health-1">>,
                            type => <<"health_check">>,
                            timestamp => 1000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        EntitlementEvent = #{
                            id => <<"entitlement-1">>,
                            type => <<"entitlement_event">>,
                            timestamp => 2000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        ReceiptVerify = #{
                            id => <<"verify-1">>,
                            type => <<"receipt_verify">>,
                            timestamp => 3000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        SupportQuery = #{
                            id => <<"support-1">>,
                            type => <<"support_query">>,
                            timestamp => 4000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"partial">>
                        },
                        HttpRequest = #{
                            id => <<"http-1">>,
                            type => <<"http_request">>,
                            timestamp => 5000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },
                        McpToolCall = #{
                            id => <<"mcp-1">>,
                            type => <<"mcp_tool_call">>,
                            timestamp => 6000,
                            tenant_id => <<"tenant-123">>,
                            decision => <<"accept">>
                        },

                        %% Store all
                        tai_receipts:store_receipt(HealthCheck),
                        tai_receipts:store_receipt(EntitlementEvent),
                        tai_receipts:store_receipt(ReceiptVerify),
                        tai_receipts:store_receipt(SupportQuery),
                        tai_receipts:store_receipt(HttpRequest),
                        tai_receipts:store_receipt(McpToolCall),

                        %% Verify all can be retrieved
                        {ok, _} = tai_receipts:get_receipt(<<"health-1">>),
                        {ok, _} = tai_receipts:get_receipt(<<"entitlement-1">>),
                        {ok, _} = tai_receipts:get_receipt(<<"verify-1">>),
                        {ok, _} = tai_receipts:get_receipt(<<"support-1">>),
                        {ok, _} = tai_receipts:get_receipt(<<"http-1">>),
                        {ok, _} = tai_receipts:get_receipt(<<"mcp-1">>),

                        ?assert(true)
                    end
                )
            ]
        end
    }.

