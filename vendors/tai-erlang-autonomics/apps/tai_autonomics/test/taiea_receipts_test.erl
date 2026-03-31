%%%-------------------------------------------------------------------
%% @doc Unit tests for tai_receipts module
%%
%% Chicago TDD pattern (Arrange/Act/Assert):
%% - Arrange: Set up test state (ETS tables, fixtures)
%% - Act: Call receipt creation/verification functions
%% - Assert: Verify receipt structure, hashing, determinism
%%
%% Test Coverage:
%%   - Receipt creation with all field types
%%   - JSON serialization round-trip
%%   - Deterministic hashing (same input → same hash)
%%   - Receipt type variants (transition, refusal, action)
%%   - Metadata preservation
%%   - Datetime handling
%%   - Hash chain verification
%%   - Stdout emission (logging)
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_receipts_test).
-include_lib("eunit/include/eunit.hrl").
-include("tai_autonomics.hrl").

%%%-------------------------------------------------------------------
%% Test Setup/Teardown
%%%-------------------------------------------------------------------

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    %% Initialize ETS tables for receipts store
    case ets:info(tai_receipts_store) of
        undefined ->
            ets:new(tai_receipts_store, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(tai_receipts_store)
    end,

    %% Initialize ETS for hash chain
    case ets:info(tai_receipts_chain) of
        undefined ->
            ets:new(tai_receipts_chain, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(tai_receipts_chain)
    end,

    ok.

teardown(_) ->
    %% Cleanup ETS tables
    catch ets:delete_all_objects(tai_receipts_store),
    catch ets:delete_all_objects(tai_receipts_chain),
    ok.

%%%-------------------------------------------------------------------
%% Test Cases - Transition Receipt Creation
%%%-------------------------------------------------------------------

%% Test: Create transition receipt with all fields populated
transition_receipt_basic_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-123">>,
                        EntitlementId = <<"ent-456">>,
                        Action = <<"activate">>,
                        NewState = active,
                        Metadata = #{
                            <<"reason">> => <<"user_request">>,
                            <<"user_id">> => <<"user-789">>
                        },

                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            TenantId, EntitlementId, Action, NewState, Metadata
                        ),

                        %% Assert - Receipt structure
                        ReceiptId = maps:get(id, Receipt),
                        ?assert(is_binary(ReceiptId)),
                        ?assertEqual(32, byte_size(ReceiptId)),
                        ?assertEqual(?RECEIPT_TYPE_TRANSITION, maps:get(type, Receipt)),
                        ?assertEqual(TenantId, maps:get(tenant_id, Receipt)),
                        ?assertEqual(EntitlementId, maps:get(entitlement_id, Receipt)),
                        ?assertEqual(Action, maps:get(action, Receipt)),
                        ?assertEqual(<<"active">>, maps:get(state_to, Receipt)),
                        ?assertEqual(Metadata, maps:get(metadata, Receipt)),

                        %% Assert - Hash fields present
                        ?assert(is_binary(maps:get(hash, Receipt))),
                        ?assert(is_binary(maps:get(chain_hash, Receipt))),

                        %% Assert - Timestamp present and reasonable
                        Timestamp = maps:get(timestamp, Receipt),
                        ?assert(is_integer(Timestamp)),
                        ?assert(Timestamp > 0),

                        %% Assert - Previous hash stored
                        PrevHash = maps:get(prev_hash, Receipt),
                        ?assert(is_binary(PrevHash))
                    end
                )
            ]
        end
    }.

%% Test: Deterministic hashing (same input → same hash)
deterministic_hash_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-det">>,
                        EntitlementId = <<"ent-det">>,
                        Action = <<"provision">>,
                        State = active,
                        Metadata = #{<<"key">> => <<"value">>},

                        %% Act - Create two receipts with same inputs
                        Receipt1 = tai_receipts:create_transition_receipt(
                            TenantId, EntitlementId, Action, State, Metadata
                        ),
                        Receipt2 = tai_receipts:create_transition_receipt(
                            TenantId, EntitlementId, Action, State, Metadata
                        ),

                        %% Assert - Hashes should be identical (inputs same)
                        Hash1 = maps:get(hash, Receipt1),
                        Hash2 = maps:get(hash, Receipt2),
                        ?assertEqual(Hash1, Hash2),
                        ?assertEqual(32, byte_size(Hash1))
                    end
                )
            ]
        end
    }.

%% Test: Receipt metadata preservation
metadata_preservation_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Metadata = #{
                            <<"string_field">> => <<"test_value">>,
                            <<"int_field">> => 42,
                            <<"float_field">> => 3.14,
                            <<"bool_field">> => true,
                            <<"nested">> => #{
                                <<"deep_key">> => <<"deep_value">>
                            }
                        },

                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            stable,
                            Metadata
                        ),

                        %% Assert - Metadata unchanged
                        StoredMetadata = maps:get(metadata, Receipt),
                        ?assertEqual(Metadata, StoredMetadata)
                    end
                )
            ]
        end
    }.

%% Test: Timestamp handling
timestamp_handling_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Before = erlang:system_time(millisecond),

                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            stable,
                            #{}
                        ),

                        %% Assert
                        After = erlang:system_time(millisecond),
                        Timestamp = maps:get(timestamp, Receipt),
                        ?assert(Timestamp >= Before),
                        ?assert(Timestamp =< After)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Refusal Receipt
%%%-------------------------------------------------------------------

%% Test: Create refusal receipt with atom reason
refusal_receipt_atom_reason_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Reason = entitlement_inactive,

                        %% Act
                        Receipt = tai_receipts:create_refusal(Reason),

                        %% Assert
                        ?assertEqual(?RECEIPT_TYPE_REFUSAL, maps:get(type, Receipt)),
                        ?assertEqual(<<"entitlement_inactive">>, maps:get(reason, Receipt)),
                        ?assert(is_binary(maps:get(hash, Receipt))),
                        ?assert(is_integer(maps:get(timestamp, Receipt)))
                    end
                )
            ]
        end
    }.

%% Test: Create refusal receipt with tuple reason
refusal_receipt_tuple_reason_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Reason = {insufficient_quota, #{
                            <<"available">> => 10,
                            <<"requested">> => 20
                        }},

                        %% Act
                        Receipt = tai_receipts:create_refusal(Reason),

                        %% Assert
                        ReasonStr = maps:get(reason, Receipt),
                        ?assert(is_binary(ReasonStr)),
                        ?assert(byte_size(ReasonStr) > 0),
                        ?assert(string:str(binary_to_list(ReasonStr), "insufficient_quota") > 0)
                    end
                )
            ]
        end
    }.

%% Test: Refusal receipt ID generation
refusal_receipt_unique_ids_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange & Act
                        Receipt1 = tai_receipts:create_refusal(test_reason_1),
                        Receipt2 = tai_receipts:create_refusal(test_reason_2),

                        %% Assert - Each receipt has unique ID
                        Id1 = maps:get(id, Receipt1),
                        Id2 = maps:get(id, Receipt2),
                        ?assertNotEqual(Id1, Id2)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Action Receipt
%%%-------------------------------------------------------------------

%% Test: Create action attempt receipt
action_attempt_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-action">>,
                        ActionId = <<"action-999">>,
                        Result = #{
                            <<"status">> => <<"pending">>,
                            <<"started_at">> => erlang:system_time(millisecond)
                        },

                        %% Act
                        Receipt = tai_receipts:create_action_receipt(attempt, TenantId, ActionId, Result),

                        %% Assert
                        ?assertEqual(?RECEIPT_TYPE_ACTION_ATTEMPT, maps:get(type, Receipt)),
                        ?assertEqual(TenantId, maps:get(tenant_id, Receipt)),
                        ?assertEqual(ActionId, maps:get(action_id, Receipt)),
                        ?assertEqual(Result, maps:get(result, Receipt))
                    end
                )
            ]
        end
    }.

%% Test: Create action result receipt
action_result_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-result">>,
                        ActionId = <<"action-888">>,
                        Result = #{
                            <<"status">> => <<"completed">>,
                            <<"duration_ms">> => 1500,
                            <<"outcome">> => <<"success">>
                        },

                        %% Act
                        Receipt = tai_receipts:create_action_receipt(result, TenantId, ActionId, Result),

                        %% Assert
                        ?assertEqual(?RECEIPT_TYPE_ACTION_RESULT, maps:get(type, Receipt)),
                        ?assertEqual(TenantId, maps:get(tenant_id, Receipt)),
                        ?assertEqual(ActionId, maps:get(action_id, Receipt)),
                        ?assertEqual(Result, maps:get(result, Receipt))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Storage and Retrieval
%%%-------------------------------------------------------------------

%% Test: Store and retrieve receipt
store_and_retrieve_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            active,
                            #{}
                        ),
                        ReceiptId = maps:get(id, Receipt),

                        %% Act
                        ok = tai_receipts:store_receipt(Receipt),
                        RetrievedResult = tai_receipts:get_receipt(ReceiptId),

                        %% Assert
                        {ok, Retrieved} = RetrievedResult,
                        ?assertEqual(ReceiptId, maps:get(id, Retrieved)),
                        ?assertEqual(maps:get(hash, Receipt), maps:get(hash, Retrieved))
                    end
                )
            ]
        end
    }.

%% Test: Retrieve non-existent receipt
retrieve_missing_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        FakeId = <<"nonexistent-id-1234567890123456">>,

                        %% Act
                        Result = tai_receipts:get_receipt(FakeId),

                        %% Assert
                        ?assertEqual({error, not_found}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Multiple receipts storage
multiple_receipts_storage_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Receipt1 = tai_receipts:create_transition_receipt(
                            <<"tenant-1">>, <<"ent-1">>, <<"action-1">>, active, #{}
                        ),
                        Receipt2 = tai_receipts:create_transition_receipt(
                            <<"tenant-1">>, <<"ent-2">>, <<"action-2">>, warning, #{}
                        ),
                        Receipt3 = tai_receipts:create_refusal(quota_exceeded),

                        %% Act
                        ok = tai_receipts:store_receipt(Receipt1),
                        ok = tai_receipts:store_receipt(Receipt2),
                        ok = tai_receipts:store_receipt(Receipt3),

                        %% Assert - All retrievable
                        {ok, _} = tai_receipts:get_receipt(maps:get(id, Receipt1)),
                        {ok, _} = tai_receipts:get_receipt(maps:get(id, Receipt2)),
                        {ok, _} = tai_receipts:get_receipt(maps:get(id, Receipt3))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Hash Chain Verification
%%%-------------------------------------------------------------------

%% Test: Verify valid hash chain
verify_valid_chain_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange - Create a sequence of transition receipts
                        TenantId = <<"tenant-chain">>,
                        Receipt1 = tai_receipts:create_transition_receipt(
                            TenantId, <<"ent-1">>, <<"action">>, boot, #{}
                        ),

                        %% Simulate storing chain hash for next receipt
                        ChainHash1 = maps:get(chain_hash, Receipt1),
                        ets:insert(tai_receipts_chain, {TenantId, ChainHash1}),

                        Receipt2 = tai_receipts:create_transition_receipt(
                            TenantId, <<"ent-2">>, <<"action">>, active, #{}
                        ),

                        %% Act - Verify chain
                        Result = tai_receipts:verify_chain([Receipt1, Receipt2]),

                        %% Assert
                        ?assertEqual({ok, valid}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Empty receipt list verification
verify_empty_chain_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Act
                        Result = tai_receipts:verify_chain([]),

                        %% Assert
                        ?assertEqual({ok, valid}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - JSON Serialization Round-trip
%%%-------------------------------------------------------------------

%% Test: Receipt JSON serialization and deserialization
json_serialization_roundtrip_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        OriginalReceipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            active,
                            #{<<"key">> => <<"value">>}
                        ),

                        %% Act - Serialize to JSON and back
                        Json = jsx:encode(OriginalReceipt),
                        Deserialized = jsx:decode(Json, [return_maps]),

                        %% Assert - Key fields preserved
                        ?assertEqual(maps:get(id, OriginalReceipt), maps:get(<<"id">>, Deserialized)),
                        ?assertEqual(maps:get(type, OriginalReceipt), maps:get(<<"type">>, Deserialized)),
                        ?assertEqual(maps:get(tenant_id, OriginalReceipt), maps:get(<<"tenant_id">>, Deserialized))
                    end
                )
            ]
        end
    }.

%% Test: Complex metadata JSON serialization
complex_metadata_json_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        ComplexMetadata = #{
                            <<"user_id">> => <<"user-123">>,
                            <<"tags">> => [<<"tag1">>, <<"tag2">>, <<"tag3">>],
                            <<"context">> => #{
                                <<"request_id">> => <<"req-999">>,
                                <<"ip_address">> => <<"192.168.1.1">>
                            },
                            <<"timings">> => #{
                                <<"started_at">> => 1000,
                                <<"ended_at">> => 2000
                            }
                        },

                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            stable,
                            ComplexMetadata
                        ),

                        %% Act
                        Json = jsx:encode(Receipt),
                        Decoded = jsx:decode(Json, [return_maps]),

                        %% Assert
                        DecodedMetadata = maps:get(<<"metadata">>, Decoded),
                        ?assertEqual(<<"user-123">>, maps:get(<<"user_id">>, DecodedMetadata)),
                        ?assertEqual(3, length(maps:get(<<"tags">>, DecodedMetadata)))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Edge Cases and Error Scenarios
%%%-------------------------------------------------------------------

%% Test: Empty metadata
empty_metadata_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            stable,
                            #{}
                        ),

                        %% Assert
                        ?assertEqual(#{}, maps:get(metadata, Receipt))
                    end
                )
            ]
        end
    }.

%% Test: Unicode in receipt fields
unicode_in_fields_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        UnicodeAction = <<"激活"/utf8>>,
                        UnicodeMetadata = #{
                            <<"reason">> => <<"用户请求"/utf8>>,
                            <<"region">> => <<"中国"/utf8>>
                        },

                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            UnicodeAction,
                            active,
                            UnicodeMetadata
                        ),

                        %% Assert
                        ?assertEqual(UnicodeAction, maps:get(action, Receipt)),
                        ?assertEqual(UnicodeMetadata, maps:get(metadata, Receipt))
                    end
                )
            ]
        end
    }.

%% Test: Very long metadata
large_metadata_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange - Create large metadata
                        LargeValue = binary:copy(<<"x">>, 10000),
                        LargeMetadata = #{
                            <<"large_field">> => LargeValue,
                            <<"count">> => 100
                        },

                        %% Act
                        Receipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>,
                            <<"ent">>,
                            <<"action">>,
                            active,
                            LargeMetadata
                        ),

                        %% Assert
                        ?assertEqual(LargeMetadata, maps:get(metadata, Receipt)),
                        ?assert(byte_size(maps:get(hash, Receipt)) > 0)
                    end
                )
            ]
        end
    }.

%% Test: Receipt with different state atoms
state_variants_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        States = [boot, stable, warning, intervening, refusing],

                        %% Act & Assert
                        lists:foreach(fun(State) ->
                            Receipt = tai_receipts:create_transition_receipt(
                                <<"tenant">>,
                                <<"ent">>,
                                <<"action">>,
                                State,
                                #{}
                            ),
                            StateStr = atom_to_binary(State, utf8),
                            ?assertEqual(StateStr, maps:get(state_to, Receipt))
                        end, States)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Hash Consistency
%%%-------------------------------------------------------------------

%% Test: Same receipt content produces same hash
hash_consistency_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Tenant = <<"tenant-hash">>,
                        Ent = <<"ent-hash">>,
                        Action = <<"verify">>,
                        State = active,
                        Meta = #{<<"test">> => <<"data">>},

                        %% Act - Create multiple receipts with same content
                        Receipts = [
                            tai_receipts:create_transition_receipt(Tenant, Ent, Action, State, Meta),
                            tai_receipts:create_transition_receipt(Tenant, Ent, Action, State, Meta),
                            tai_receipts:create_transition_receipt(Tenant, Ent, Action, State, Meta)
                        ],

                        %% Assert - All have same hash
                        Hashes = [maps:get(hash, R) || R <- Receipts],
                        [Hash1, Hash2, Hash3] = Hashes,
                        ?assertEqual(Hash1, Hash2),
                        ?assertEqual(Hash2, Hash3),
                        ?assertEqual(32, byte_size(Hash1))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Receipt Types Validation
%%%-------------------------------------------------------------------

%% Test: All receipt types are properly set
all_receipt_types_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Transition Receipt
                        TransitionReceipt = tai_receipts:create_transition_receipt(
                            <<"tenant">>, <<"ent">>, <<"action">>, active, #{}
                        ),
                        ?assertEqual(?RECEIPT_TYPE_TRANSITION, maps:get(type, TransitionReceipt)),

                        %% Refusal Receipt
                        RefusalReceipt = tai_receipts:create_refusal(test_reason),
                        ?assertEqual(?RECEIPT_TYPE_REFUSAL, maps:get(type, RefusalReceipt)),

                        %% Action Attempt Receipt
                        AttemptReceipt = tai_receipts:create_action_receipt(
                            attempt, <<"tenant">>, <<"action-123">>, #{}
                        ),
                        ?assertEqual(?RECEIPT_TYPE_ACTION_ATTEMPT, maps:get(type, AttemptReceipt)),

                        %% Action Result Receipt
                        ResultReceipt = tai_receipts:create_action_receipt(
                            result, <<"tenant">>, <<"action-123">>, #{}
                        ),
                        ?assertEqual(?RECEIPT_TYPE_ACTION_RESULT, maps:get(type, ResultReceipt))
                    end
                )
            ]
        end
    }.
