%%%-------------------------------------------------------------------
%% @doc Integration Tests - End-to-end pricing engine workflows
%%
%% Tests real-world scenarios:
%% - Customer registration through billing
%% - Value calculation and receipt generation
%% - Concurrent customer operations
%% - Error recovery and resilience
%% - Mock Stripe integration
%%%-------------------------------------------------------------------
-module(integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pricing_engine.hrl").

%%%===================================================================
%% Integration Test Scenarios
%%%===================================================================

%% Scenario 1: Complete customer lifecycle
test_customer_lifecycle() ->
    %% 1. Register customer
    CustomerId = <<"integration_customer_001">>,
    CustomerData = #{customer_name => "Acme Corp", customer_email => "billing@acme.com"},
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 10.0, max_price => 1000.0},

    %% 2. Record value calculation
    Metrics = [{<<"throughput">>, 50.0}, {<<"latency">>, 25.0}],
    Options = #{aggregation_method => weighted_sum},

    {ok, ValueRecord1} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

    %% 3. Record more metrics over time
    {ok, ValueRecord2} = pricing_engine:calculate_value(CustomerId, [{<<"throughput">>, 75.0}], PricingConfig, Options),
    {ok, ValueRecord3} = pricing_engine:calculate_value(CustomerId, [{<<"throughput">>, 100.0}], PricingConfig, Options),

    %% 4. Verify receipt chain
    {ok, VerifiedRecord1} = pricing_engine:verify_receipt(CustomerId, ValueRecord1#value_record.receipt_hash),

    %% 5. Get billing summary
    {ok, History} = pricing_engine:get_value_history(CustomerId, 100),

    %% Assertions
    ?assert(length(History) >= 3),
    ?assertEqual(ValueRecord1#value_record.customer_id, VerifiedRecord1#value_record.customer_id),
    ?assert(ValueRecord1#value_record.calculated_value >= 0),
    ?assert(ValueRecord2#value_record.billed_price >= ValueRecord1#value_record.billed_price).

%% Scenario 2: Multi-tenant isolation
test_multi_tenant_isolation() ->
    NumCustomers = 10,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
    Options = #{aggregation_method => weighted_sum},

    %% Create value records for multiple customers
    Results = [
        begin
            CustId = integer_to_binary(N),
            Metrics = [{<<"throughput">>, float(N * 10)}],
            pricing_engine:calculate_value(CustId, Metrics, PricingConfig, Options)
        end
        || N <- lists:seq(1, NumCustomers)
    ],

    %% Verify all succeeded
    ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results)),

    %% Verify isolation - each customer's history is separate
    HistoryCheck = [
        begin
            CustId = integer_to_binary(N),
            {ok, Hist} = pricing_engine:get_value_history(CustId, 100),
            lists:all(fun(#value_record{customer_id = Cid}) -> Cid =:= CustId end, Hist)
        end
        || N <- lists:seq(1, NumCustomers)
    ],

    ?assert(lists:all(fun(X) -> X end, HistoryCheck)).

%% Scenario 3: Billing cycle workflow
test_billing_cycle_workflow() ->
    CustomerId = <<"billing_customer_001">>,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 5.0, max_price => 500.0},
    Options = #{aggregation_method => weighted_sum},

    %% Simulate daily metrics over a week
    Values = [
        [{<<"throughput">>, 10.0}],
        [{<<"throughput">>, 15.0}],
        [{<<"throughput">>, 12.0}],
        [{<<"throughput">>, 20.0}],
        [{<<"throughput">>, 25.0}],
        [{<<"throughput">>, 18.0}],
        [{<<"throughput">>, 22.0}]
    ],

    %% Record values for each day
    Results = [
        pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options)
        || Metrics <- Values
    ],

    ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results)),

    %% Get billing summary
    {ok, History} = pricing_engine:get_value_history(CustomerId, 100),
    TotalValue = lists:foldl(
        fun(#value_record{calculated_value = V}, Sum) -> Sum + V end,
        0.0,
        History
    ),
    TotalPrice = lists:foldl(
        fun(#value_record{billed_price = P}, Sum) -> Sum + P end,
        0.0,
        History
    ),

    ?assert(TotalValue > 0),
    ?assert(TotalPrice > 0),
    ?assert(TotalPrice >= 5.0),  %% Respects min price
    ?assert(TotalPrice =< 500.0 * length(History)).  %% Respects max price

%% Scenario 4: Spike detection and anomaly handling
test_anomaly_detection() ->
    CustomerId = <<"anomaly_customer_001">>,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
    Options = #{aggregation_method => weighted_sum},

    %% Normal metrics
    Normal = [{<<"throughput">>, 50.0}],
    {ok, Normal1} = pricing_engine:calculate_value(CustomerId, Normal, PricingConfig, Options),
    {ok, Normal2} = pricing_engine:calculate_value(CustomerId, Normal, PricingConfig, Options),

    %% Spike - 10x increase
    Spike = [{<<"throughput">>, 500.0}],
    {ok, SpikeRecord} = pricing_engine:calculate_value(CustomerId, Spike, PricingConfig, Options),

    %% Back to normal
    {ok, _Normal3} = pricing_engine:calculate_value(CustomerId, Normal, PricingConfig, Options),

    %% Verify spike was recorded
    {ok, History} = pricing_engine:get_value_history(CustomerId, 100),

    SpikeRatio = SpikeRecord#value_record.calculated_value / Normal1#value_record.calculated_value,
    ?assert(SpikeRatio >= 8.0),  %% Significant spike detected

    %% Verify all records are in history (no lost data)
    ?assert(length(History) >= 4).

%% Scenario 5: Concurrent multi-customer load
test_concurrent_multi_customer_load() ->
    NumCustomers = 20,
    NumValuesPerCustomer = 10,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
    Options = #{aggregation_method => weighted_sum},

    %% Spawn concurrent operations across multiple customers
    Parent = self(),
    Pids = [
        spawn(fun() ->
            CustId = <<"concurrent_", (integer_to_binary(C))/binary>>,
            Results = [
                pricing_engine:calculate_value(
                    CustId,
                    [{<<"throughput">>, float(random:uniform(100))}],
                    PricingConfig,
                    Options
                )
                || _ <- lists:seq(1, NumValuesPerCustomer)
            ],
            Success = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),
            Parent ! {self(), Success}
        end)
        || C <- lists:seq(1, NumCustomers)
    ],

    %% Wait for all to complete
    Results = [receive {_Pid, Success} -> Success after 5000 -> false end || _ <- Pids],

    %% All should succeed
    ?assert(lists:all(fun(X) -> X end, Results)),
    erlang:garbage_collect_all().

%% Scenario 6: Receipt verification chain
test_receipt_verification_chain() ->
    CustomerId = <<"receipt_chain_customer">>,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
    Options = #{aggregation_method => weighted_sum, hmac_key => crypto:strong_rand_bytes(32)},

    %% Create chain of receipts
    Metrics1 = [{<<"throughput">>, 50.0}],
    {ok, Record1} = pricing_engine:calculate_value(CustomerId, Metrics1, PricingConfig, Options),

    Metrics2 = [{<<"throughput">>, 60.0}],
    {ok, Record2} = pricing_engine:calculate_value(CustomerId, Metrics2, PricingConfig, Options),

    Metrics3 = [{<<"throughput">>, 70.0}],
    {ok, Record3} = pricing_engine:calculate_value(CustomerId, Metrics3, PricingConfig, Options),

    %% Verify each can be retrieved and verified
    {ok, Verified1} = pricing_engine:verify_receipt(CustomerId, Record1#value_record.receipt_hash),
    {ok, Verified2} = pricing_engine:verify_receipt(CustomerId, Record2#value_record.receipt_hash),
    {ok, Verified3} = pricing_engine:verify_receipt(CustomerId, Record3#value_record.receipt_hash),

    %% Verify chain integrity
    ?assertEqual(undefined, Verified1#value_record.previous_hash),
    ?assertEqual(Record1#value_record.receipt_hash, Verified2#value_record.previous_hash),
    ?assertEqual(Record2#value_record.receipt_hash, Verified3#value_record.previous_hash).

%% Scenario 7: Error handling and recovery
test_error_handling() ->
    CustomerId = <<"error_test_customer">>,
    PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},

    %% Invalid metrics - negative value
    InvalidMetrics1 = [{<<"throughput">>, -50.0}],
    Result1 = pricing_engine:calculate_value(CustomerId, InvalidMetrics1, PricingConfig, #{aggregation_method => weighted_sum}),
    ?assertMatch({error, _}, Result1),

    %% Invalid metrics - too large
    InvalidMetrics2 = [{<<"throughput">>, 1e15}],
    Result2 = pricing_engine:calculate_value(CustomerId, InvalidMetrics2, PricingConfig, #{aggregation_method => weighted_sum}),
    ?assertMatch({error, _}, Result2),

    %% Valid metrics should still work after errors
    ValidMetrics = [{<<"throughput">>, 50.0}],
    {ok, ValidRecord} = pricing_engine:calculate_value(CustomerId, ValidMetrics, PricingConfig, #{aggregation_method => weighted_sum}),
    ?assertEqual(completed, ValidRecord#value_record.status).

%%%===================================================================
%% Test Suite Configuration
%%%===================================================================

integration_test_() ->
    {inorder, [
        {"Customer lifecycle", fun test_customer_lifecycle/0},
        {"Multi-tenant isolation", fun test_multi_tenant_isolation/0},
        {"Billing cycle workflow", fun test_billing_cycle_workflow/0},
        {"Anomaly detection", fun test_anomaly_detection/0},
        {"Concurrent multi-customer load", fun test_concurrent_multi_customer_load/0},
        {"Receipt verification chain", fun test_receipt_verification_chain/0},
        {"Error handling and recovery", fun test_error_handling/0}
    ]}.
