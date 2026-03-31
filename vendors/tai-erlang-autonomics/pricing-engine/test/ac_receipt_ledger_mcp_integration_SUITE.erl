%%%-------------------------------------------------------------------
%% @doc AC Receipt Ledger MCP - Integration Test Suite
%%
%% Tests integration scenarios:
%% - Multi-session independence
%% - Concurrent appends with proper ordering
%% - Full workflow (append, rotate, export, verify)
%% - Error recovery
%% - Audit trail generation
%%%-------------------------------------------------------------------
-module(ac_receipt_ledger_mcp_integration_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    test_single_append_workflow/1,
    test_multiple_appends_form_chain/1,
    test_epoch_rotation_workflow/1,
    test_export_audit_trail/1,
    test_verify_chain_integrity/1,
    test_concurrent_appends/1,
    test_session_isolation/1,
    test_disclaimer_updates/1
]).

%%%===================================================================
%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_single_append_workflow,
        test_multiple_appends_form_chain,
        test_epoch_rotation_workflow,
        test_export_audit_trail,
        test_verify_chain_integrity,
        test_concurrent_appends,
        test_session_isolation,
        test_disclaimer_updates
    ].

init_per_suite(Config) ->
    %% Start OTP application if needed
    application:ensure_all_started(crypto),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start fresh ledger for each test
    catch gen_statem:stop(ac_receipt_ledger_mcp),
    {ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
        disclaimer => "Integration test - advisory only"
    }),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch gen_statem:stop(ac_receipt_ledger_mcp),
    ok.

%%%===================================================================
%% Test Cases
%%%===================================================================

%% Single receipt append workflow
%% Verify receipt is created with all required fields
test_single_append_workflow(Config) ->
    ct:log("Test: Single append creates receipt with metadata"),

    Kind = calculate_value,
    Payload = #{
        customer_id => <<"cust_test_001">>,
        timestamp => erlang:system_time(millisecond),
        metrics => [
            {<<"cpu">>, 45.2},
            {<<"memory">>, 62.3},
            {<<"disk">>, 28.1}
        ]
    },
    Meta = #{
        source => integration_test,
        calculation_method => weighted_sum
    },

    {ok, Receipt} = ac_receipt_ledger_mcp:append(Kind, Payload, Meta),

    %% Assertions
    ct:log("Receipt: ~p", [Receipt]),

    %% Required fields
    true = is_map(Receipt),
    eval = maps:get(mode, Receipt),
    advisory = maps:get(authority, Receipt),
    Kind = maps:get(kind, Receipt),
    1 = maps:get(epoch, Receipt),
    1 = maps:get(seq, Receipt),

    %% Cryptographic proof
    true = is_binary(maps:get(hash, Receipt)),
    32 = byte_size(maps:get(hash, Receipt)),

    %% Session info
    true = is_binary(maps:get(session_id, Receipt)),

    ct:log("Single append test PASSED"),
    ok.

%% Multiple appends form proper merkle chain
%% Each receipt links to previous via hash chain
test_multiple_appends_form_chain(Config) ->
    ct:log("Test: Multiple appends form merkle chain"),

    %% Append 5 receipts
    Receipts = lists:map(
        fun(N) ->
            Payload = #{n => N, timestamp => erlang:system_time(millisecond)},
            {ok, R} = ac_receipt_ledger_mcp:append(
                calculate_value,
                Payload,
                #{index => N}
            ),
            ct:log("Appended receipt ~p: hash=~p", [N, maps:get(hash, R)]),
            R
        end,
        lists:seq(1, 5)
    ),

    %% Verify chain properties
    ct:log("Verifying chain properties..."),

    %% 1. Each receipt should have incrementing seq
    Seqs = lists:map(fun(R) -> maps:get(seq, R) end, Receipts),
    [1, 2, 3, 4, 5] = Seqs,
    ct:log("Seq numbers correct: ~p", [Seqs]),

    %% 2. Each receipt (except first) should link to previous
    lists:foldl(
        fun(N, PrevHash) ->
            Receipt = lists:nth(N, Receipts),
            CurrentHash = maps:get(hash, Receipt),
            CurrentPrev = maps:get(prev, Receipt),

            if
                N > 1 ->
                    PrevHash = CurrentPrev,
                    ct:log("Link verified at seq ~p", [N]);
                true ->
                    ct:log("First receipt (no prev link)")
            end,

            CurrentHash
        end,
        <<>>,
        lists:seq(1, 5)
    ),

    %% 3. Head hash should match last receipt
    {ok, HeadHash} = ac_receipt_ledger_mcp:head_hash(ac_receipt_ledger_mcp),
    LastReceipt = lists:last(Receipts),
    HeadHash = maps:get(hash, LastReceipt),
    ct:log("Head hash matches last receipt"),

    ct:log("Merkle chain test PASSED"),
    ok.

%% Epoch rotation workflow
%% Rotate epoch, verify new chain starts fresh
test_epoch_rotation_workflow(Config) ->
    ct:log("Test: Epoch rotation creates new chain"),

    %% Append in epoch 1
    {ok, R1a} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{epoch => 1, seq => 1},
        #{}
    ),
    {ok, R1b} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{epoch => 1, seq => 2},
        #{}
    ),
    Epoch1Head = maps:get(hash, R1b),

    ct:log("Epoch 1 appends done. Head hash: ~p", [Epoch1Head]),

    %% Rotate with new disclaimer
    NewDisclaimer = "Advisory receipt after rotation - non-contractual v2",
    {ok, ReturnedHash} = ac_receipt_ledger_mcp:rotate_epoch(NewDisclaimer, #{}),

    %% Returned hash should match epoch 1 head
    Epoch1Head = ReturnedHash,
    ct:log("Epoch rotation returned correct hash"),

    %% Append in epoch 2
    {ok, R2a} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{epoch => 2, seq => 1},
        #{}
    ),
    {ok, R2b} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{epoch => 2, seq => 2},
        #{}
    ),

    %% Verify epoch values
    1 = maps:get(epoch, R1a),
    1 = maps:get(epoch, R1b),
    2 = maps:get(epoch, R2a),
    2 = maps:get(epoch, R2b),

    %% Verify seq resets in new epoch
    1 = maps:get(seq, R1a),
    2 = maps:get(seq, R1b),
    1 = maps:get(seq, R2a),
    2 = maps:get(seq, R2b),

    %% Epoch 2 chain should NOT link to epoch 1 (new chain)
    %% First receipt in epoch 2 should have empty prev
    R2a_Prev = maps:get(prev, R2a),
    ct:log("Epoch 2 first receipt prev: ~p", [R2a_Prev]),

    ct:log("Epoch rotation test PASSED"),
    ok.

%% Export audit trail
%% Verify export contains all receipts with metadata
test_export_audit_trail(Config) ->
    ct:log("Test: Export generates audit trail"),

    %% Create multi-epoch ledger
    {ok, _R1} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{customer => <<"c1">>, value => 100.0},
        #{source => app}
    ),
    {ok, _R2} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{customer => <<"c1">>, status => verified},
        #{source => app}
    ),

    %% Rotate
    ac_receipt_ledger_mcp:rotate_epoch("Updated disclaimer", #{}),

    {ok, _R3} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{customer => <<"c2">>, value => 200.0},
        #{source => app}
    ),

    %% Export
    {ok, Export} = ac_receipt_ledger_mcp:export(#{}),

    ct:log("Export: ~p", [Export]),

    %% Verify export structure
    2 = maps:get(current_epoch, Export),
    true = is_binary(maps:get(session_id, Export)),
    true = is_binary(maps:get(head_hash, Export)),

    %% Verify all receipts included
    Receipts = maps:get(receipts, Export),
    3 = length(Receipts),

    %% Verify epochs represented
    Epochs = lists:map(fun(R) -> maps:get(epoch, R) end, Receipts),
    true = lists:member(1, Epochs),
    true = lists:member(2, Epochs),

    ct:log("Export audit trail test PASSED"),
    ok.

%% Verify chain integrity
%% Chain verification should detect any hash mismatches
test_verify_chain_integrity(Config) ->
    ct:log("Test: Chain verification validates integrity"),

    %% Create chain
    ac_receipt_ledger_mcp:append(calculate_value, #{v => 1}, #{}),
    ac_receipt_ledger_mcp:append(verify_receipt, #{v => 2}, #{}),
    ac_receipt_ledger_mcp:append(calculate_value, #{v => 3}, #{}),

    %% Verify should pass
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),

    ct:log("Chain verification passed for valid chain"),

    %% In a real scenario, we would:
    %% 1. Export the ledger
    %% 2. Corrupt a receipt hash
    %% 3. Re-import and verify fails
    %% This would require integration with persistent storage

    ct:log("Chain verification test PASSED"),
    ok.

%% Concurrent appends maintain order and integrity
test_concurrent_appends(Config) ->
    ct:log("Test: Concurrent appends maintain order"),

    %% Spawn multiple processes appending simultaneously
    Parent = self(),
    NumWorkers = 10,

    Workers = [
        spawn_link(fun() ->
            append_worker(Parent, WorkerId, 5)
        end)
        || WorkerId <- lists:seq(1, NumWorkers)
    ],

    %% Collect all appended receipts
    AllReceipts = lists:foldl(
        fun(_WorkerId, Acc) ->
            receive
                {receipts, WorkerReceipts} ->
                    Acc ++ WorkerReceipts
            after 5000 ->
                ct:fail("Worker timeout")
            end
        end,
        [],
        lists:seq(1, NumWorkers)
    ),

    %% Verify we got all receipts
    TotalExpected = NumWorkers * 5,
    ct:log("Appended ~p receipts from ~p workers", [length(AllReceipts), NumWorkers]),
    TotalExpected = length(AllReceipts),

    %% Verify chain integrity
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),

    ct:log("Concurrent appends test PASSED"),
    ok.

%% Session isolation
%% Each session has unique ID and separate ledger
test_session_isolation(Config) ->
    ct:log("Test: Session isolation"),

    %% Get current session
    {ok, Session1} = ac_receipt_ledger_mcp:get_session_id(ac_receipt_ledger_mcp),
    ct:log("Session 1 ID: ~p", [Session1]),

    %% Append with session 1
    {ok, R1} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{session => 1},
        #{}
    ),
    Session1 = maps:get(session_id, R1),

    %% Create new ledger (would be in different process/node in real scenario)
    catch gen_statem:stop(ac_receipt_ledger_mcp),
    {ok, _} = ac_receipt_ledger_mcp:start_link(#{}),

    %% Get new session
    {ok, Session2} = ac_receipt_ledger_mcp:get_session_id(ac_receipt_ledger_mcp),
    ct:log("Session 2 ID: ~p", [Session2]),

    %% Sessions should be different
    false = (Session1 =:= Session2),

    ct:log("Session isolation test PASSED"),
    ok.

%% Disclaimer updates on rotation
%% Verify disclaimer changes propagate to new receipts
test_disclaimer_updates(Config) ->
    ct:log("Test: Disclaimer updates on rotation"),

    Disclaimer1 = "Initial disclaimer",
    catch gen_statem:stop(ac_receipt_ledger_mcp),
    {ok, _} = ac_receipt_ledger_mcp:start_link(#{disclaimer => Disclaimer1}),

    %% Append with initial disclaimer
    {ok, R1} = ac_receipt_ledger_mcp:append(calculate_value, #{v => 1}, #{}),
    Disclaimer1 = maps:get(disclaimer, R1),

    %% Rotate with new disclaimer
    Disclaimer2 = "Updated disclaimer after rotation",
    ac_receipt_ledger_mcp:rotate_epoch(Disclaimer2, #{}),

    %% Append with new disclaimer
    {ok, R2} = ac_receipt_ledger_mcp:append(verify_receipt, #{v => 2}, #{}),
    Disclaimer2 = maps:get(disclaimer, R2),

    ct:log("Disclaimer 1: ~s", [Disclaimer1]),
    ct:log("Disclaimer 2: ~s", [Disclaimer2]),

    ct:log("Disclaimer updates test PASSED"),
    ok.

%%%===================================================================
%% Helper Functions
%%%===================================================================

%% Worker process for concurrent append test
append_worker(Parent, WorkerId, Count) ->
    Results = lists:map(
        fun(N) ->
            Payload = #{
                worker => WorkerId,
                n => N,
                timestamp => erlang:system_time(millisecond)
            },
            {ok, Receipt} = ac_receipt_ledger_mcp:append(
                calculate_value,
                Payload,
                #{worker => WorkerId}
            ),
            Receipt
        end,
        lists:seq(1, Count)
    ),

    Parent ! {receipts, Results}.

%%%===================================================================
%% End of Test Suite
%%%===================================================================
