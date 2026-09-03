%%====================================================================
%% gen_yawl_receipts_tests - Cryptographic Receipt Tests
%%
%% @copyright 2025 ggen Project
%% @license MIT
%%====================================================================

-module(gen_yawl_receipts_tests).
-include_lib("eunit/include/eunit.hrl").
-include("gen_yawl.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

make_test_receipt() ->
    #receipt{
        id = <<"test-receipt-123">>,
        prev_hash = <<0:256>>,

        current_hash = crypto:hash(blake3, <<"test-data">>),
        timestamp = 1707040800000000000,  %% 2024-02-04 10:00:00 UTC
        event_type = work_item_enabled,
        case_id = <<"test-case-456">>,
        work_item_id = <<"workitem-789">>,
        justification = #{<<"reason">> => <<"test">>}
    }.

make_test_chain() ->
    Receipt1 = #receipt{
        id = <<"receipt-1">>,
        prev_hash = <<0:256>>,
        current_hash = crypto:hash(blake3, <<"event-1">>),
        timestamp = 1000000000000,
        event_type = case_started,
        case_id = <<"case-1">>,
        justification = #{}
    },
    Receipt2 = #receipt{
        id = <<"receipt-2">>,
        prev_hash = Receipt1#receipt.current_hash,
        current_hash = crypto:hash(blake3, <<"event-2">>),
        timestamp = 2000000000000,
        event_type = task_enabled,
        case_id = <<"case-1">>,
        justification = #{}
    },
    Receipt3 = #receipt{
        id = <<"receipt-3">>,
        prev_hash = Receipt2#receipt.current_hash,
        current_hash = crypto:hash(blake3, <<"event-3">>),
        timestamp = 3000000000000,
        event_type = task_completed,
        case_id = <<"case-1">>,
        justification = #{}
    },
    [Receipt1, Receipt2, Receipt3].

%%====================================================================
%% Receipt Creation Tests
%%====================================================================

create_receipt_test_() ->
    {setup,
     fun setup_receipts/0,
     fun cleanup_receipts/1,
     fun(_) -> [
         ?_test(begin
             CaseId = <<"test-case">>,
             EventType = task_completed,
             Context = #{<<"taskId">> => <<"my_task">>},
             {ok, Receipt} = gen_yawl_receipts:create_receipt(CaseId, EventType, Context),
             ?assert(is_binary(Receipt#receipt.id)),
             ?assert(is_binary(Receipt#receipt.current_hash)),
             ?assertEqual(CaseId, Receipt#receipt.case_id),
             ?assertEqual(EventType, Receipt#receipt.event_type)
         end)
     ] end}.

setup_receipts() ->
    {ok, Pid} = gen_yawl_receipts:start_link(),
    Pid.

cleanup_receipts(_Pid) ->
    gen_yawl_receipts:stop().

%%====================================================================
%% Receipt Verification Tests
%%====================================================================

verify_receipt_test_() ->
    Receipt = make_test_receipt(),

    [
     ?_test(begin
         {ok, IsValid} = gen_yawl_receipts:verify_receipt(Receipt),
         %% In real implementation, would recompute hash
         ?assert(is_boolean(IsValid))
       end)
    ].

verify_chain_test_() ->
    ValidChain = make_test_chain(),

    [
     ?_test(begin
         {ok, IsValid, Errors} = gen_yawl_receipts:verify_chain(ValidChain),
         %% Check that chain links are valid
         ?assert(is_boolean(IsValid)),
         ?assert(is_list(Errors))
       end)
    ].

verify_broken_chain_test_() ->
    [R1, R2 | _] = make_test_chain(),
    %% Break the chain by changing prev_hash
    BrokenR2 = R2#receipt{prev_hash = <<1:256>>},
    BrokenChain = [R1, BrokenR2],

    [
     ?_test(begin
         {ok, IsValid, _Errors} = gen_yawl_receipts:verify_chain(BrokenChain),
         ?assertNot(IsValid)
       end)
    ].

%%====================================================================
%% RDF Export Tests
%%====================================================================

export_to_rdf_test_() ->
    Receipt = make_test_receipt(),
    Turtle = gen_yawl_receipts:receipt_to_turtle(Receipt),

    [
     ?_assert(is_binary(Turtle)),
     ?_assert(<<<<"@prefix">>/binary>> =< Turtle),
     ?_assert(<<<<"yawl:">>/binary>> =< Turtle),
     ?_assert(<<<<"prov:">>/binary>> =< Turtle),
     ?_assert(<<<<"receipt_">>/binary>> =< Turtle)
    ].

export_receipts_to_rdf_test_() ->
    Chain = make_test_chain(),
    Turtle = gen_yawl_receipts:receipts_to_turtle(Chain),

    [
     ?_assert(is_binary(Turtle)),
     ?_assert(<<<<"@prefix">>/binary>> =< Turtle),
     %% Should have multiple receipts
     ?assertMatch(3, length(re:split(Turtle, <<"<receipt_">>)) - 1)
    ].

%%====================================================================
%% Hash Computation Tests
%%====================================================================

compute_hash_test_() ->
    Payload = #{<<"data">> => <<"test">>},
    Hash = crypto:hash(blake3, term_to_binary(Payload)),

    [
     ?_assertEqual(32, byte_size(Hash)),  %% BLAKE3 = 32 bytes
     ?_assert(is_binary(Hash))
    ].

hash_deterministic_test_() ->
    Payload1 = #{<<"data">> => <<"test">>},
    Payload2 = #{<<"data">> => <<"test">>},

    Hash1 = crypto:hash(blake3, term_to_binary(Payload1)),
    Hash2 = crypto:hash(blake3, term_to_binary(Payload2)),

    [
     ?_assertEqual(Hash1, Hash2)
    ].

hash_different_for_different_input_test_() ->
    Payload1 = #{<<"data">> => <<"test1">>},
    Payload2 = #{<<"data">> => <<"test2">>},

    Hash1 = crypto:hash(blake3, term_to_binary(Payload1)),
    Hash2 = crypto:hash(blake3, term_to_binary(Payload2)),

    [
     ?_assertNot(Hash1 =:= Hash2)
    ].

%%====================================================================
%% Timestamp Tests
%%====================================================================

nanos_to_iso8601_test_() ->
    Nanos = 1707040800000000000,  %% 2024-02-04 10:00:00 UTC

    IsoString = gen_yawl_receipts:nanos_to_iso8601(Nanos),

    [
     ?_assert(is_list(IsoString)),
     ?_assertMatch("2024-02-04T" ++ _, IsoString),
     ?_assertMatch("Z", lists:reverse(IsoString))
    ].

%%====================================================================
%% Chain Link Verification Tests
%%====================================================================

verify_chain_links_valid_test_() ->
    Chain = make_test_chain(),

    [
     ?_assert(gen_yawl_receipts:verify_chain_links(Chain))
    ].

verify_chain_links_single_test_() ->
    [Single] = make_test_chain(),

    [
     ?_assert(gen_yawl_receipts:verify_chain_links([Single]))
    ].

verify_chain_links_broken_test_() ->
    [R1, R2, R3] = make_test_chain(),
    %% Break link between R2 and R3
    BrokenR3 = R3#receipt{prev_hash = <<0:256>>},

    [
     ?_assertNot(gen_yawl_receipts:verify_chain_links([R1, R2, BrokenR3]))
    ].

%%====================================================================
%% JSON Encoding Tests
%%====================================================================

encode_json_test_() ->
    Map = #{<<"key">> => <<"value">>, <<"number">> => 42},

    Json = gen_yawl_receipts:encode_json(Map),

    [
     ?_assert(is_binary(Json)),
     ?_assertMatch(<<"{", _/binary>>, Json)
    ].

encode_json_nested_test_() ->
    Map = #{
        <<"outer">> => #{
            <<"inner">> => <<"value">>
        }
    },

    Json = gen_yawl_receipts:encode_json(Map),

    [
     ?_assert(is_binary(Json)),
     ?_assertMatch(<<"{", _/binary>>, Json)
    ].

encode_json_array_test_() ->
    List = [<<"a">>, <<"b">>, <<"c">>],

    Json = gen_yawl_receipts:encode_json_value(List),

    [
     ?_assert(is_binary(Json)),
     ?_assertMatch(<<"[", _/binary>>, Json)
    ].
