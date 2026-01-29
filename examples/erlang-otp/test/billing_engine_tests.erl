%%%-------------------------------------------------------------------
%%% @doc Billing Engine EUnit Tests
%%%
%%% Test suite for ACID compliance, idempotency, and audit trails.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(billing_engine_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Cleanup
%%%===================================================================

setup() ->
    %% Use temporary files for testing
    WalFile = "/tmp/test_wal_" ++ integer_to_list(erlang:system_time()) ++ ".dets",
    AuditFile = "/tmp/test_audit_" ++ integer_to_list(erlang:system_time()) ++ ".dets",

    {ok, Pid} = billing_engine_server:start_link([
        {wal_file, WalFile},
        {audit_file, AuditFile},
        {fraud_threshold, 5000.00}
    ]),

    {Pid, WalFile, AuditFile}.

cleanup({Pid, WalFile, AuditFile}) ->
    exit(Pid, normal),
    timer:sleep(100),  % Wait for DETS to close
    file:delete(WalFile),
    file:delete(AuditFile),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test successful charge
charge_account_success_test() ->
    State = setup(),

    TxnId = <<"TXN-TEST-001">>,
    AccountId = <<"ACC-001">>,
    Amount = 100.00,

    Result = billing_engine_server:charge_account(TxnId, AccountId, Amount),
    ?assertMatch({ok, TxnId}, Result),

    cleanup(State).

%% Test idempotency (duplicate transaction prevention)
idempotency_test() ->
    State = setup(),

    TxnId = <<"TXN-TEST-002">>,
    AccountId = <<"ACC-001">>,
    Amount = 50.00,

    %% First charge should succeed
    {ok, TxnId} = billing_engine_server:charge_account(TxnId, AccountId, Amount),

    %% Second charge with same TxnId should be rejected
    Result = billing_engine_server:charge_account(TxnId, AccountId, Amount),
    ?assertEqual({error, duplicate_transaction}, Result),

    cleanup(State).

%% Test insufficient funds
insufficient_funds_test() ->
    State = setup(),

    TxnId = <<"TXN-TEST-003">>,
    AccountId = <<"ACC-001">>,
    Amount = -999999.00,  % Negative = debit

    Result = billing_engine_server:charge_account(TxnId, AccountId, Amount),
    ?assertEqual({error, insufficient_funds}, Result),

    cleanup(State).

%% Test fraud detection
fraud_detection_test() ->
    State = setup(),

    TxnId = <<"TXN-TEST-004">>,
    AccountId = <<"ACC-001">>,
    Amount = 10000.00,  % Exceeds threshold of 5000.00

    Result = billing_engine_server:charge_account(TxnId, AccountId, Amount),
    ?assertEqual({error, fraud_detected}, Result),

    cleanup(State).

%% Test refund functionality
refund_test() ->
    State = setup(),

    %% First, make a charge
    OriginalTxnId = <<"TXN-TEST-005">>,
    AccountId = <<"ACC-001">>,
    Amount = 200.00,

    {ok, OriginalTxnId} = billing_engine_server:charge_account(OriginalTxnId, AccountId, Amount),

    %% Get balance after charge
    {ok, BalanceAfterCharge, _} = billing_engine_server:get_account_balance(AccountId),

    %% Now refund
    {ok, _RefundTxnId} = billing_engine_server:refund_transaction(OriginalTxnId, <<"Customer request">>),

    %% Balance should be restored
    {ok, BalanceAfterRefund, _} = billing_engine_server:get_account_balance(AccountId),
    ?assertEqual(BalanceAfterCharge - Amount, BalanceAfterRefund),

    cleanup(State).

%% Test account balance inquiry
get_balance_test() ->
    State = setup(),

    Result = billing_engine_server:get_account_balance(<<"ACC-001">>),
    ?assertMatch({ok, _Balance, <<"USD">>}, Result),

    cleanup(State).

%% Test audit log retrieval
audit_log_test() ->
    State = setup(),

    TxnId = <<"TXN-TEST-006">>,
    AccountId = <<"ACC-001">>,
    Amount = 75.00,

    %% Make a charge
    {ok, TxnId} = billing_engine_server:charge_account(TxnId, AccountId, Amount),

    %% Retrieve audit log
    {ok, AuditEntries} = billing_engine_server:get_audit_log(AccountId),

    %% Should have at least one entry
    ?assert(length(AuditEntries) > 0),

    cleanup(State).
