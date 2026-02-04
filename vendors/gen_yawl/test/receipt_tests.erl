%%====================================================================
%% receipt_tests - Cryptographic Receipt Tests
%%====================================================================
%% @doc Tests for receipt generation, verification, and chain integrity.
%% Receipts provide cryptographic proof of workflow execution.

-module(receipt_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/gen_yawl.hrl").

%%====================================================================
%% Receipt Creation Tests
%%====================================================================

receipt_creation_test_() ->
    [
        {"Create receipt from event",
         fun() ->
            Event = task_completed,
            Context = #{
                case_id => <<"case-123">>,
                task_id => task_a,
                timestamp => 1234567890
            },

            Receipt = gen_yawl_receipts:create(Event, Context, undefined),

            ?assert(is_record(Receipt, receipt)),
            ?assert(is_binary(Receipt#receipt.id)),
            ?assert(is_binary(Receipt#receipt.current_hash)),
            ?assertEqual(<<>>, Receipt#receipt.prev_hash),
            ?assertEqual(task_completed, Receipt#receipt.event_type),
            ?assertEqual(<<"case-123">>, Receipt#receipt.case_id)
         end},
        {"Create receipt with previous receipt",
         fun() ->
            %% First receipt
            R1 = gen_yawl_receipts:create(
                event_1,
                #{case_id => <<"case-1">>},
                undefined
            ),

            %% Second receipt linked to first
            R2 = gen_yawl_receipts:create(
                event_2,
                #{case_id => <<"case-1">>},
                R1
            ),

            ?assertEqual(R1#receipt.current_hash, R2#receipt.prev_hash),
            ?assertNotEqual(R1#receipt.current_hash, R2#receipt.current_hash)
         end},
        {"Receipt hash is deterministic",
         fun() ->
            Context = #{case_id => <<"case-1">>, data => <<"test">>},

            R1 = gen_yawl_receipts:create(test, Context, undefined),
            R2 = gen_yawl_receipts:create(test, Context, undefined),

            ?assertEqual(R1#receipt.current_hash, R2#receipt.current_hash)
         end},
        {"Receipt hash differs for different events",
         fun() ->
            Context = #{case_id => <<"case-1">>},

            R1 = gen_yawl_receipts:create(event_a, Context, undefined),
            R2 = gen_yawl_receipts:create(event_b, Context, undefined),

            ?assertNotEqual(R1#receipt.current_hash, R2#receipt.current_hash)
         end}
    ].

%%====================================================================
%% Receipt Chain Tests
%%====================================================================

receipt_chain_test_() ->
    [
        {"Chain of receipts forms correctly",
         fun() ->
            %% Create chain of 5 receipts
            Receipts = lists:foldl(
                fun(I, Acc) ->
                    Prev = case Acc of
                        [] -> undefined;
                        _ -> hd(Acc)
                    end,
                    [gen_yawl_receipts:create(
                        I,
                        #{case_id => <<"chain-test">>, index => I},
                        Prev
                    ) | Acc]
                end,
                [],
                lists:seq(1, 5)
            ),

            ?assertEqual(5, length(Receipts)),

            %% Verify chain links
            [R5, R4, R3, R2, R1] = Receipts,
            ?assertEqual(R1#receipt.current_hash, R2#receipt.prev_hash),
            ?assertEqual(R2#receipt.current_hash, R3#receipt.prev_hash),
            ?assertEqual(R3#receipt.current_hash, R4#receipt.prev_hash),
            ?assertEqual(R4#receipt.current_hash, R5#receipt.prev_hash)
         end},
        {"Verify valid chain",
         fun() ->
            %% Create valid chain
            R1 = gen_yawl_receipts:create(e1, #{case_id => <<"c1">>}, undefined),
            R2 = gen_yawl_receipts:create(e2, #{case_id => <<"c1">>}, R1),
            R3 = gen_yawl_receipts:create(e3, #{case_id => <<"c1">>}, R2),

            {ok, IsValid, _} = gen_yawl_receipts:verify_chain([R1, R2, R3]),
            ?assert(IsValid)
         end},
        {"Detect broken chain link",
         fun() ->
            R1 = gen_yawl_receipts:create(e1, #{case_id => <<"c1">>}, undefined),
            R2 = gen_yawl_receipts:create(e2, #{case_id => <<"c1">>}, R1),
            %% R3 has wrong prev_hash
            R3 = #receipt{
                id = <<"fake">>,
                prev_hash = <<"wrong-hash">>,
                current_hash = crypto:hash(blake3, <<"fake">>),
                timestamp = os:system_time(nanosecond),
                event_type = e3,
                case_id = <<"c1">>,
                work_item_id = undefined,
                justification = #{}
            },

            {ok, IsValid, Errors} = gen_yawl_receipts:verify_chain([R1, R2, R3]),
            ?assertNot(IsValid),
            ?assert(length(Errors) > 0)
         end}
    ].

%%====================================================================
%% Receipt Persistence Tests
%%====================================================================

receipt_persistence_test_() ->
    {foreach,
     fun() ->
        gen_yawl_receipts:start_link(),
        ok
     end,
     fun(_) ->
        gen_yawl_receipts:stop()
     end,
     [
        fun(_) ->
            {"Persist and retrieve receipt",
             fun() ->
                Receipt = gen_yawl_receipts:create(
                    test_event,
                    #{case_id => <<"persist-test">>},
                    undefined
                ),

                ok = gen_yawl_receipts:persist(Receipt),
                {ok, Retrieved} = gen_yawl_receipts:get(Receipt#receipt.id),

                ?assertEqual(Receipt#receipt.id, Retrieved#receipt.id),
                ?assertEqual(Receipt#receipt.current_hash, Retrieved#receipt.current_hash)
             end}
         end,
        fun(_) ->
            {"Persist receipt chain",
             fun() ->
                R1 = gen_yawl_receipts:create(e1, #{case_id => <<"c1">>}, undefined),
                R2 = gen_yawl_receipts:create(e2, #{case_id => <<"c1">>}, R1),

                ok = gen_yawl_receipts:persist(R1),
                ok = gen_yawl_receipts:persist(R2),

                {ok, All} = gen_yawl_receipts:get_case_receipts(<<"c1">>),
                ?assert(length(All) >= 2)
             end}
         end
     ]}.

%%====================================================================
%% Receipt Export Tests
%%====================================================================

receipt_export_test_() ->
    [
        {"Export receipt to RDF TTL",
         fun() ->
            Receipt = gen_yawl_receipts:create(
                test_event,
                #{case_id => <<"export-test">>},
                undefined
            ),

            TTL = gen_yawl_receipts:to_ttl(Receipt),
            ?assert(is_binary(TTL)),
            ?assert(string:find(TTL, "Receipt") =/= nomatch),
            ?assert(string:find(TTL, "test_event") =/= nomatch)
         end},
        {"Export receipt chain to RDF",
         fun() ->
            R1 = gen_yawl_receipts:create(e1, #{case_id => <<"c1">>}, undefined),
            R2 = gen_yawl_receipts:create(e2, #{case_id => <<"c1">>}, R1),

            TTL = gen_yawl_receipts:chain_to_ttl([R1, R2]),
            ?assert(is_binary(TTL)),
            ?assert(string:find(TTL, "e1") =/= nomatch),
            ?assert(string:find(TTL, "e2") =/= nomatch)
         end},
        {"Export receipts to JSON",
         fun() ->
            Receipt = gen_yawl_receipts:create(
                test_event,
                #{case_id => <<"json-test">>},
                undefined
            ),

            JSON = gen_yawl_receipts:to_json(Receipt),
            ?assert(is_binary(JSON)),
            ?assert(string:find(JSON, "\"id\"") =/= nomatch),
            ?assert(string:find(JSON, "\"hash\"") =/= nomatch)
         end}
    ].

%%====================================================================
%% Receipt Timestamp Tests
%%====================================================================

timestamp_test_() ->
    [
        {"Receipt contains nanosecond timestamp",
         fun() ->
            Before = os:system_time(nanosecond),

            Receipt = gen_yawl_receipts:create(
                timestamp_test,
                #{},
                undefined
            ),

            After = os:system_time(nanosecond),

            ?assert(Receipt#receipt.timestamp >= Before),
            ?assert(Receipt#receipt.timestamp =< After)
         end},
        {"Timestamps are monotonically increasing",
         fun() ->
            R1 = gen_yawl_receipts:create(e1, #{}, undefined),
            timer:sleep(1),
            R2 = gen_yawl_receipts:create(e2, #{}, undefined),

            ?assert(R2#receipt.timestamp > R1#receipt.timestamp)
         end}
    ].

%%====================================================================
%% Receipt Hash Tests
%%====================================================================

hash_test_() ->
    [
        {"Hash includes event type",
         fun() ->
            R1 = gen_yawl_receipts:create(event_a, #{data => 1}, undefined),
            R2 = gen_yawl_receipts:create(event_b, #{data => 1}, undefined),

            ?assertNotEqual(R1#receipt.current_hash, R2#receipt.current_hash)
         end},
        {"Hash includes context data",
         fun() ->
            R1 = gen_yawl_receipts:create(event, #{data => 1}, undefined),
            R2 = gen_yawl_receipts:create(event, #{data => 2}, undefined),

            ?assertNotEqual(R1#receipt.current_hash, R2#receipt.current_hash)
         end},
        {"Hash includes previous hash",
         fun() ->
            R1 = gen_yawl_receipts:create(e1, #{}, undefined),
            R2a = gen_yawl_receipts:create(e2, #{}, R1),
            R2b = gen_yawl_receipts:create(e2, #{}, undefined),

            ?assertNotEqual(R2a#receipt.current_hash, R2b#receipt.current_hash)
         end},
        {"Hash is cryptographically strong (BLAKE3)",
         fun() ->
            Receipt = gen_yawl_receipts:create(strong, #{}, undefined),
            Hash = Receipt#receipt.current_hash,

            %% BLAKE3 produces 32-byte hash
            ?assertEqual(32, byte_size(Hash))
         end}
    ].

%%====================================================================
%% Receipt ID Tests
%%====================================================================

receipt_id_test_() ->
    [
        {"Receipt ID is unique",
         fun() ->
            R1 = gen_yawl_receipts:create(e1, #{}, undefined),
            R2 = gen_yawl_receipts:create(e1, #{}, undefined),

            ?assertNotEqual(R1#receipt.id, R2#receipt.id)
         end},
        {"Receipt ID is hex-encoded",
         fun() ->
            Receipt = gen_yawl_receipts:create(e1, #{}, undefined),
            Id = Receipt#receipt.id,

            ?assert(is_binary(Id)),
            ?assertMatch([$0 | _], binary_to_list(Id)),
            AllHex = lists:all(
                fun(C) ->
                    (C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f)
                end,
                binary_to_list(Id)
            ),
            ?assert(AllHex)
         end}
    ].

%%====================================================================
%% Workflow Receipt Integration Tests
%%====================================================================

workflow_receipt_test_() ->
    {foreach,
     fun() ->
        gen_yawl_receipts:start_link(),
        {ok, Pid} = simple_sequence:start_link(),
        Pid
     end,
     fun(Pid) ->
        gen_yawl:stop(Pid),
        gen_yawl_receipts:stop()
     end,
     [
        fun(Pid) ->
            {"Workflow generates receipts on transitions",
             fun() ->
                %% Fire transition
                ok = gen_yawl:fire(Pid, t_draft),

                %% Get receipts from workflow
                Receipts = gen_yawl:receipts(Pid),
                ?assertNot([] =:= Receipts),

                %% First receipt should be for draft transition
                [First | _] = Receipts,
                ?assertEqual(t_draft, First#receipt.event_type)
             end}
         end,
        fun(Pid) ->
            {"Complete workflow has receipt chain",
             fun() ->
                %% Fire all transitions
                ok = gen_yawl:fire(Pid, t_draft),
                ok = gen_yawl:fire(Pid, t_review),
                ok = gen_yawl:fire(Pid, t_approve),

                %% Get all receipts
                AllReceipts = gen_yawl:receipts(Pid),
                ?assert(length(AllReceipts) >= 3),

                %% Verify chain integrity
                {ok, IsValid, _} = gen_yawl_receipts:verify_chain(AllReceipts),
                ?assert(IsValid)
             end}
         end,
        fun(Pid) ->
            {"Receipts include case ID",
             fun() ->
                CaseId = gen_yawl:case_id(Pid),
                ok = gen_yawl:fire(Pid, t_draft),

                [Receipt | _] = gen_yawl:receipts(Pid),
                ?assertEqual(CaseId, Receipt#receipt.case_id)
             end}
         end
     ]}.

%%====================================================================
%% Receipt Justification Tests
%%====================================================================

justification_test_() ->
    [
        {"Receipt includes justification data",
         fun() ->
            Justification = #{
                reason => "test completed",
                result => success,
                metadata => #{key => "value"}
            },

            Receipt = gen_yawl_receipts:create(
                justified_event,
                #{},
                undefined,
                Justification
            ),

            ?assertEqual(Justification, Receipt#receipt.justification)
         end},
        {"Empty justification defaults to empty map",
         fun() ->
            Receipt = gen_yawl_receipts:create(
                event,
                #{},
                undefined
            ),

            ?assertEqual(#{}, Receipt#receipt.justification)
         end}
    ].

%%====================================================================
%% Receipt Performance Tests
%%====================================================================

performance_test_() ->
    [
        {"Receipt creation is fast",
         fun() ->
            %% Create 1000 receipts and measure time
            {Time, _} = timer:tc(fun() ->
                lists:foreach(
                    fun(I) ->
                        gen_yawl_receipts:create(
                            I,
                            #{case_id => <<"perf-test">>},
                            undefined
                        )
                    end,
                    lists:seq(1, 1000)
                )
            end),

            %% Should complete in less than 100ms (100,000 microseconds)
            ?assert(Time < 100000)
         end},
        {"Chain verification is efficient",
         fun() ->
            %% Build a chain of 100 receipts
            Chain = lists:foldl(
                fun(I, Acc) ->
                    Prev = case Acc of
                        [] -> undefined;
                        [H | _] -> H
                    end,
                    [gen_yawl_receipts:create(
                        I,
                        #{case_id => <<"verify-test">>},
                        Prev
                    ) | Acc]
                end,
                [],
                lists:seq(1, 100)
            ),

            %% Verify the chain
            {Time, {ok, _IsValid, _}} = timer:tc(
                fun() -> gen_yawl_receipts:verify_chain(Chain) end
            ),

            %% Should verify in less than 10ms
            ?assert(Time < 10000)
         end}
    ].
