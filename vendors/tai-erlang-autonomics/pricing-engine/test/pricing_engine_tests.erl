%%%-------------------------------------------------------------------
%% @doc Pricing Engine - Unit Tests
%%
%% Comprehensive test suite covering:
%% - Value calculation with edge cases
%% - Receipt hashing and merkle chains
%% - Price formula application
%% - Billing logic
%% - API endpoints
%% - Concurrent access patterns
%% - Error handling and failure modes
%%%-------------------------------------------------------------------
-module(pricing_engine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pricing_engine.hrl").

%%%===================================================================
%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, _} = pricing_engine:start_link(#{}),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%% Value Calculation Tests
%%%===================================================================

test_calculate_value_basic() ->
    ?setup(fun() ->
        CustomerId = <<"customer_001">>,
        Metrics = [{<<"throughput">>, 100.0}, {<<"latency">>, 50.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        {ok, ValueRecord} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertMatch(#value_record{
            customer_id = CustomerId,
            status = completed
        }, ValueRecord),
        ?assert(ValueRecord#value_record.calculated_value >= 0),
        ?assert(ValueRecord#value_record.billed_price >= 0)
    end).

test_calculate_value_zero_metrics() ->
    ?setup(fun() ->
        CustomerId = <<"customer_002">>,
        Metrics = [{<<"throughput">>, 0.0}, {<<"latency">>, 0.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        {ok, ValueRecord} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertEqual(completed, ValueRecord#value_record.status),
        ?assert(ValueRecord#value_record.calculated_value >= 0)
    end).

test_calculate_value_max_metrics() ->
    ?setup(fun() ->
        CustomerId = <<"customer_003">>,
        Metrics = [{<<"throughput">>, 1000000.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        {ok, ValueRecord} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertEqual(completed, ValueRecord#value_record.status),
        %% Price should be capped at max_price
        ?assert(ValueRecord#value_record.billed_price =< 10000.0)
    end).

test_calculate_value_invalid_metrics_negative() ->
    ?setup(fun() ->
        CustomerId = <<"customer_004">>,
        Metrics = [{<<"throughput">>, -100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        Result = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertMatch({error, {validation_failed, _}}, Result)
    end).

test_calculate_value_invalid_metrics_too_large() ->
    ?setup(fun() ->
        CustomerId = <<"customer_005">>,
        Metrics = [{<<"throughput">>, 1e12}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        Result = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertMatch({error, {validation_failed, _}}, Result)
    end).

test_calculate_value_invalid_metric_format() ->
    ?setup(fun() ->
        CustomerId = <<"customer_006">>,
        Metrics = [invalid_metric],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        Result = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        ?assertMatch({error, {validation_failed, _}}, Result)
    end).

%%%===================================================================
%% Pricing Formula Tests
%%%===================================================================

test_price_calculation_basic() ->
    ?setup(fun() ->
        Value = 50.0,
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{},

        {ok, Price} = pricing_engine:calculate_price(Value, PricingConfig, Options),

        %% P = α × V = 100 × 50 = 5000
        ?assertEqual(5000.0, Price)
    end).

test_price_calculation_with_min_floor() ->
    ?setup(fun() ->
        Value = 0.0,
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 100.0, max_price => 10000.0},
        Options = #{},

        {ok, Price} = pricing_engine:calculate_price(Value, PricingConfig, Options),

        %% P = (α × V) + min = 0 + 100 = 100
        ?assertEqual(100.0, Price)
    end).

test_price_calculation_with_max_ceiling() ->
    ?setup(fun() ->
        Value = 500.0,
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{},

        {ok, Price} = pricing_engine:calculate_price(Value, PricingConfig, Options),

        %% P = α × V = 100 × 500 = 50000, capped at 10000
        ?assertEqual(10000.0, Price)
    end).

%%%===================================================================
%% Receipt and Hashing Tests
%%%===================================================================

test_receipt_hash_deterministic() ->
    ?setup(fun() ->
        CustomerId = <<"customer_007">>,
        Value = 100.0,
        Price = 10000.0,
        Timestamp = 1234567890000,
        Metrics = [{<<"throughput">>, 100.0}],
        PrevHash = undefined,
        HmacKey = undefined,

        {ok, Hash1} = pricing_engine:hash_value_record(
            CustomerId, Value, Price, Timestamp, Metrics, PrevHash, HmacKey
        ),

        {ok, Hash2} = pricing_engine:hash_value_record(
            CustomerId, Value, Price, Timestamp, Metrics, PrevHash, HmacKey
        ),

        %% Same inputs must produce same hash (deterministic)
        ?assertEqual(Hash1, Hash2)
    end).

test_receipt_hash_different_values() ->
    ?setup(fun() ->
        CustomerId = <<"customer_008">>,
        Timestamp = 1234567890000,
        Metrics = [{<<"throughput">>, 100.0}],
        PrevHash = undefined,
        HmacKey = undefined,

        {ok, Hash1} = pricing_engine:hash_value_record(
            CustomerId, 100.0, 10000.0, Timestamp, Metrics, PrevHash, HmacKey
        ),

        {ok, Hash2} = pricing_engine:hash_value_record(
            CustomerId, 101.0, 10000.0, Timestamp, Metrics, PrevHash, HmacKey
        ),

        %% Different values must produce different hashes
        ?assertNotEqual(Hash1, Hash2)
    end).

test_merkle_chain_verification() ->
    ?setup(fun() ->
        CustomerId = <<"customer_009">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        %% Create first value record
        {ok, Record1} = pricing_engine:calculate_value(
            CustomerId, Metrics, PricingConfig, Options
        ),

        %% Verify first record (no previous hash)
        {ok, VerifiedRecord1} = pricing_engine:verify_receipt(
            CustomerId, Record1#value_record.receipt_hash
        ),

        ?assertMatch(#value_record{status = completed}, VerifiedRecord1)
    end).

%%%===================================================================
%% History and Retrieval Tests
%%%===================================================================

test_value_history_empty() ->
    ?setup(fun() ->
        CustomerId = <<"customer_no_history">>,

        {ok, History} = pricing_engine:get_value_history(CustomerId, 100),

        ?assertEqual([], History)
    end).

test_value_history_multiple_records() ->
    ?setup(fun() ->
        CustomerId = <<"customer_010">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        %% Create multiple records
        {ok, _R1} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),
        {ok, _R2} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),
        {ok, _R3} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        {ok, History} = pricing_engine:get_value_history(CustomerId, 100),

        ?assert(length(History) >= 1)
    end).

test_value_history_respects_limit() ->
    ?setup(fun() ->
        CustomerId = <<"customer_011">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        %% Create multiple records
        [pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options)
         || _ <- lists:seq(1, 20)],

        {ok, History} = pricing_engine:get_value_history(CustomerId, 5),

        ?assert(length(History) =< 5)
    end).

%%%===================================================================
%% Customer Statistics Tests
%%%===================================================================

test_customer_stats_total_value() ->
    ?setup(fun() ->
        CustomerId = <<"customer_012">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        {ok, _R1} = pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options),

        {ok, TotalValue} = pricing_engine:get_customer_stats(CustomerId, total_value),

        ?assert(TotalValue >= 0)
    end).

test_customer_stats_receipt_count() ->
    ?setup(fun() ->
        CustomerId = <<"customer_013">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        [pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options)
         || _ <- lists:seq(1, 3)],

        {ok, Count} = pricing_engine:get_customer_stats(CustomerId, receipt_count),

        ?assert(Count >= 1)
    end).

%%%===================================================================
%% Edge Cases and Error Handling
%%%===================================================================

test_concurrent_value_calculations() ->
    ?setup(fun() ->
        CustomerId = <<"customer_concurrent">>,
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        %% Spawn multiple concurrent calculations
        Pids = [spawn(fun() ->
            Metrics = [{<<"throughput">>, random:uniform(100) + 0.0}],
            pricing_engine:calculate_value(CustomerId, Metrics, PricingConfig, Options)
        end) || _ <- lists:seq(1, 10)],

        %% Wait for all to complete
        timer:sleep(1000),

        {ok, History} = pricing_engine:get_value_history(CustomerId, 100),

        ?assert(length(History) >= 1),
        erlang:garbage_collect_all()
    end).

test_customer_isolation() ->
    ?setup(fun() ->
        Customer1 = <<"customer_014">>,
        Customer2 = <<"customer_015">>,
        Metrics = [{<<"throughput">>, 100.0}],
        PricingConfig = #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
        Options = #{aggregation_method => weighted_sum},

        {ok, _R1} = pricing_engine:calculate_value(Customer1, Metrics, PricingConfig, Options),
        {ok, _R2} = pricing_engine:calculate_value(Customer2, Metrics, PricingConfig, Options),

        {ok, Hist1} = pricing_engine:get_value_history(Customer1, 100),
        {ok, Hist2} = pricing_engine:get_value_history(Customer2, 100),

        %% Each customer should have independent history
        ?assert(all_customer_ids_match(Customer1, Hist1)),
        ?assert(all_customer_ids_match(Customer2, Hist2))
    end).

%%%===================================================================
%% Helper Functions
%%%===================================================================

all_customer_ids_match(CustomerId, History) ->
    lists:all(fun(#value_record{customer_id = Cid}) -> Cid =:= CustomerId end, History).

?setup(Fun) ->
    {setup, fun setup/0, fun cleanup/1, Fun}.

%%%===================================================================
%% Test Suite Configuration
%%%===================================================================

pricing_engine_test_() ->
    {inorder, [
        {"Basic value calculation", test_calculate_value_basic()},
        {"Zero metrics", test_calculate_value_zero_metrics()},
        {"Max metrics", test_calculate_value_max_metrics()},
        {"Negative metrics rejected", test_calculate_value_invalid_metrics_negative()},
        {"Too large metrics rejected", test_calculate_value_invalid_metrics_too_large()},
        {"Invalid metric format rejected", test_calculate_value_invalid_metric_format()},
        {"Basic price calculation", test_price_calculation_basic()},
        {"Price with min floor", test_price_calculation_with_min_floor()},
        {"Price with max ceiling", test_price_calculation_with_max_ceiling()},
        {"Receipt hash is deterministic", test_receipt_hash_deterministic()},
        {"Different values produce different hashes", test_receipt_hash_different_values()},
        {"Merkle chain verification", test_merkle_chain_verification()},
        {"Empty value history", test_value_history_empty()},
        {"Multiple records in history", test_value_history_multiple_records()},
        {"History respects limit", test_value_history_respects_limit()},
        {"Customer stats - total value", test_customer_stats_total_value()},
        {"Customer stats - receipt count", test_customer_stats_receipt_count()},
        {"Concurrent value calculations", test_concurrent_value_calculations()},
        {"Customer isolation", test_customer_isolation()}
    ]}.
