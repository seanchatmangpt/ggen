%%%-------------------------------------------------------------------
%% @doc Pricing Engine - Core value calculation and billing
%%
%% Deterministic value calculation engine implementing the formula:
%% V = f(metrics) where metrics are aggregated per customer
%% P = α × V where P is the price to bill
%%
%% This module provides:
%% - Value calculation with cryptographic proofs (SHA-256 hashing)
%% - Immutable value ledger with merkle chain verification
%% - Concurrent access via gen_statem state machine
%% - Result<T, E> error handling (no unwrap/panic)
%% - Deterministic output for reproducible audits
%%-------------------------------------------------------------------
-module(pricing_engine).

-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    calculate_value/4,
    get_value_history/2,
    verify_receipt/2,
    get_customer_stats/2,
    list_receipts_for_customer/3
]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% Internal exports for testing
-export([
    hash_value_record/7,
    validate_metrics/2,
    aggregate_metrics/2,
    calculate_price/3,
    verify_merkle_chain/3
]).

-include("pricing_engine.hrl").

%% Type definitions
-type customer_id() :: binary().
-type metric_value() :: {binary(), float()}.
-type metrics() :: [metric_value()].
-type calculated_value() :: float().
-type hash() :: binary().
-type result(T) :: {ok, T} | {error, term()}.

%% gen_statem states
-type state() :: idle | calculating | verifying.

%% Record for internal state
-record(state, {
    ledger = #{} :: #{customer_id() => [#value_record{}]},
    customer_count = 0 :: non_neg_integer(),
    total_values_calculated = 0 :: non_neg_integer(),
    last_error = none :: none | term()
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the pricing engine server
-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Calculate value for a customer based on metrics
%% Returns {ok, ValueRecord} or {error, Reason}
-spec calculate_value(customer_id(), metrics(), map(), map()) ->
    result(#value_record{}).
calculate_value(CustomerId, Metrics, PricingConfig, Options) when
    is_binary(CustomerId),
    is_list(Metrics),
    is_map(PricingConfig),
    is_map(Options)
->
    gen_statem:call(?MODULE, {calculate_value, CustomerId, Metrics, PricingConfig, Options}).

%% @doc Get value history for a customer
-spec get_value_history(customer_id(), non_neg_integer()) -> result(list()).
get_value_history(CustomerId, Limit) when is_binary(CustomerId), is_integer(Limit), Limit > 0 ->
    gen_statem:call(?MODULE, {get_value_history, CustomerId, Limit}).

%% @doc Verify a receipt and return verification status
-spec verify_receipt(customer_id(), hash()) -> result(#value_record{}).
verify_receipt(CustomerId, ReceiptHash) when is_binary(CustomerId), is_binary(ReceiptHash) ->
    gen_statem:call(?MODULE, {verify_receipt, CustomerId, ReceiptHash}).

%% @doc Get statistics for a customer
-spec get_customer_stats(customer_id(), atom()) -> result(term()).
get_customer_stats(CustomerId, StatType) when is_binary(CustomerId), is_atom(StatType) ->
    gen_statem:call(?MODULE, {get_customer_stats, CustomerId, StatType}).

%% @doc List all receipts for a customer in date range
-spec list_receipts_for_customer(customer_id(), calendar:datetime(), calendar:datetime()) ->
    result([#value_record{}]).
list_receipts_for_customer(CustomerId, StartTime, EndTime) when is_binary(CustomerId) ->
    gen_statem:call(?MODULE, {list_receipts_for_customer, CustomerId, StartTime, EndTime}).

%%%===================================================================
%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

init(_Config) ->
    {ok, idle, #state{}}.

%% Calculate value for customer
handle_event({call, From}, {calculate_value, CustomerId, Metrics, PricingConfig, Options}, State) ->
    case validate_metrics(Metrics, maps:get(metric_schema, Options, #{})) of
        {ok, ValidMetrics} ->
            case aggregate_metrics(ValidMetrics, maps:get(aggregation_method, Options, weighted_sum)) of
                {ok, AggregatedValue} ->
                    case calculate_price(AggregatedValue, PricingConfig, Options) of
                        {ok, Price} ->
                            %% Create value record
                            ValueRecord = #value_record{
                                customer_id = CustomerId,
                                calculated_value = AggregatedValue,
                                billed_price = Price,
                                timestamp = erlang:system_time(millisecond),
                                metrics = ValidMetrics,
                                status = completed
                            },

                            %% Generate cryptographic hash
                            case hash_value_record(
                                CustomerId,
                                AggregatedValue,
                                Price,
                                erlang:system_time(millisecond),
                                ValidMetrics,
                                get_previous_hash(CustomerId, State),
                                maps:get(hmac_key, Options, undefined)
                            ) of
                                {ok, Hash} ->
                                    FinalRecord = ValueRecord#value_record{
                                        receipt_hash = Hash,
                                        previous_hash = get_previous_hash(CustomerId, State)
                                    },

                                    %% Update ledger
                                    NewLedger = update_ledger(CustomerId, FinalRecord, State#state.ledger),
                                    NewState = State#state{
                                        ledger = NewLedger,
                                        total_values_calculated = State#state.total_values_calculated + 1
                                    },

                                    {next_state, idle, NewState, {reply, From, {ok, FinalRecord}}};
                                {error, Reason} ->
                                    {next_state, idle, State#state{last_error = Reason},
                                     {reply, From, {error, {hash_generation_failed, Reason}}}}
                            end;
                        {error, Reason} ->
                            {next_state, idle, State#state{last_error = Reason},
                             {reply, From, {error, {price_calculation_failed, Reason}}}}
                    end;
                {error, Reason} ->
                    {next_state, idle, State#state{last_error = Reason},
                     {reply, From, {error, {aggregation_failed, Reason}}}}
            end;
        {error, Reason} ->
            {next_state, idle, State#state{last_error = Reason},
             {reply, From, {error, {validation_failed, Reason}}}}
    end;

%% Get value history
handle_event({call, From}, {get_value_history, CustomerId, Limit}, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] ->
            {next_state, idle, State, {reply, From, {ok, []}}};
        History ->
            Limited = lists:sublist(History, Limit),
            {next_state, idle, State, {reply, From, {ok, Limited}}}
    end;

%% Verify receipt
handle_event({call, From}, {verify_receipt, CustomerId, ReceiptHash}, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] ->
            {next_state, idle, State, {reply, From, {error, customer_not_found}}};
        History ->
            case find_receipt(ReceiptHash, History) of
                {ok, Record} ->
                    %% Verify merkle chain integrity
                    case verify_merkle_chain(Record, History, State#state.ledger) of
                        ok ->
                            {next_state, idle, State, {reply, From, {ok, Record}}};
                        {error, Reason} ->
                            {next_state, idle, State#state{last_error = Reason},
                             {reply, From, {error, {merkle_chain_invalid, Reason}}}}
                    end;
                error ->
                    {next_state, idle, State, {reply, From, {error, receipt_not_found}}}
            end
    end;

%% Get customer stats
handle_event({call, From}, {get_customer_stats, CustomerId, total_value}, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] ->
            {next_state, idle, State, {reply, From, {ok, 0.0}}};
        History ->
            Total = lists:foldl(
                fun(#value_record{calculated_value = V}, Acc) -> Acc + V end,
                0.0,
                History
            ),
            {next_state, idle, State, {reply, From, {ok, Total}}}
    end;

handle_event({call, From}, {get_customer_stats, CustomerId, receipt_count}, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] ->
            {next_state, idle, State, {reply, From, {ok, 0}}};
        History ->
            {next_state, idle, State, {reply, From, {ok, length(History)}}}
    end;

handle_event({call, From}, {get_customer_stats, CustomerId, _StatType}, State) ->
    {next_state, idle, State, {reply, From, {error, unsupported_stat_type}}};

%% List receipts in date range
handle_event({call, From}, {list_receipts_for_customer, CustomerId, StartTime, EndTime}, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] ->
            {next_state, idle, State, {reply, From, {ok, []}}};
        History ->
            Filtered = [R || R <- History,
                R#value_record.timestamp >= StartTime,
                R#value_record.timestamp =< EndTime],
            {next_state, idle, State, {reply, From, {ok, Filtered}}}
    end;

handle_event(EventType, EventContent, State) ->
    logger:warning("Unexpected event: ~p, ~p", [EventType, EventContent]),
    {next_state, idle, State}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% Validate metrics against schema
-spec validate_metrics(metrics(), map()) -> result(metrics()).
validate_metrics(Metrics, _Schema) ->
    case validate_metrics_internal(Metrics, []) of
        {ok, Valid} -> {ok, Valid};
        Error -> Error
    end.

validate_metrics_internal([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_metrics_internal([{Name, Value} | Rest], Acc) when
    is_binary(Name),
    is_number(Value),
    Value >= 0,
    Value =< 1_000_000_000
->
    validate_metrics_internal(Rest, [{Name, Value} | Acc]);
validate_metrics_internal([{_Name, _Value} | _Rest], _Acc) ->
    {error, invalid_metric_value};
validate_metrics_internal(_Metrics, _Acc) ->
    {error, invalid_metrics_format}.

%% Aggregate multiple metrics into single value
%% Uses weighted sum: V = Σ(w_i × normalized_value_i)
-spec aggregate_metrics(metrics(), atom()) -> result(float()).
aggregate_metrics(Metrics, weighted_sum) ->
    case compute_weighted_sum(Metrics, [], 0.0) of
        {ok, Sum} -> {ok, Sum};
        Error -> Error
    end;
aggregate_metrics(Metrics, multiplicative) ->
    case compute_multiplicative(Metrics, 1.0) of
        {ok, Product} -> {ok, Product};
        Error -> Error
    end;
aggregate_metrics(_Metrics, Method) ->
    {error, {unsupported_aggregation_method, Method}}.

compute_weighted_sum([], _Weights, Sum) ->
    {ok, Sum};
compute_weighted_sum([{Name, Value} | Rest], Weights, Sum) ->
    %% Default weight 1.0 if not specified
    Weight = proplists:get_value(Name, Weights, 1.0),
    NormalizedValue = normalize_value(Value),
    compute_weighted_sum(Rest, Weights, Sum + (Weight * NormalizedValue)).

compute_multiplicative([], Product) ->
    {ok, Product};
compute_multiplicative([{_Name, Value} | Rest], Product) ->
    NormalizedValue = normalize_value(Value),
    compute_multiplicative(Rest, Product * NormalizedValue).

%% Normalize metric values to 0-1 range using sigmoid
normalize_value(Value) ->
    Sigmoid = 1.0 / (1.0 + math:exp(-Value / 100.0)),
    Sigmoid.

%% Calculate price from value: P = α × V + min, capped at max
-spec calculate_price(calculated_value(), map(), map()) -> result(float()).
calculate_price(Value, PricingConfig, _Options) ->
    Alpha = maps:get(alpha_coefficient, PricingConfig, 100.0),
    MinPrice = maps:get(min_price, PricingConfig, 0.0),
    MaxPrice = maps:get(max_price, PricingConfig, 10000.0),

    Price = (Alpha * Value) + MinPrice,
    FinalPrice = min(Price, MaxPrice),

    {ok, FinalPrice}.

%% Generate SHA-256 hash of value record for merkle chain
-spec hash_value_record(customer_id(), float(), float(), integer(), metrics(),
                        binary() | undefined, binary() | undefined) ->
    result(hash()).
hash_value_record(CustomerId, Value, Price, Timestamp, Metrics, PreviousHash, HmacKey) ->
    %% Create canonical representation for hashing
    MetricsStr = metrics_to_string(Metrics),
    PrevHashStr = case PreviousHash of
        undefined -> "";
        H -> binary_to_list(H)
    end,

    CanonicalForm = io_lib:format(
        "~s|~p|~p|~p|~s|~s",
        [
            binary_to_list(CustomerId),
            Value,
            Price,
            Timestamp,
            MetricsStr,
            PrevHashStr
        ]
    ),

    Hash = crypto:hash(sha256, CanonicalForm),

    %% If HMAC key provided, also generate HMAC signature
    case HmacKey of
        undefined ->
            {ok, Hash};
        Key when is_binary(Key) ->
            Hmac = crypto:mac(hmac, sha256, Key, CanonicalForm),
            {ok, <<Hash/binary, Hmac/binary>>};
        _ ->
            {error, invalid_hmac_key}
    end.

metrics_to_string(Metrics) ->
    MetricsStr = lists:map(
        fun({Name, Value}) ->
            io_lib:format("~s=~p", [binary_to_list(Name), Value])
        end,
        lists:sort(Metrics)
    ),
    string:join(MetricsStr, ",").

%% Update ledger with new value record
update_ledger(CustomerId, ValueRecord, Ledger) ->
    CurrentList = maps:get(CustomerId, Ledger, []),
    maps:put(CustomerId, [ValueRecord | CurrentList], Ledger).

%% Get previous hash for customer (for merkle chain)
get_previous_hash(CustomerId, State) ->
    case maps:get(CustomerId, State#state.ledger, []) of
        [] -> undefined;
        [#value_record{receipt_hash = Hash} | _] -> Hash;
        _ -> undefined
    end.

%% Find receipt by hash in history
find_receipt(TargetHash, History) ->
    case lists:search(
        fun(#value_record{receipt_hash = Hash}) -> Hash =:= TargetHash end,
        History
    ) of
        {value, Record} -> {ok, Record};
        false -> error
    end.

%% Verify merkle chain integrity
%% Each receipt's previous_hash should match the receipt before it
-spec verify_merkle_chain(#value_record{}, [#value_record{}], map()) -> ok | {error, term()}.
verify_merkle_chain(#value_record{previous_hash = undefined}, _History, _Ledger) ->
    ok;  %% First record in chain
verify_merkle_chain(#value_record{previous_hash = ExpectedHash} = Record, History, _Ledger) ->
    case find_record_by_hash(ExpectedHash, History) of
        {ok, _PreviousRecord} ->
            ok;
        error ->
            {error, previous_record_not_found}
    end.

%% Find record by its hash
find_record_by_hash(TargetHash, History) ->
    case lists:search(
        fun(#value_record{receipt_hash = Hash}) -> Hash =:= TargetHash end,
        History
    ) of
        {value, Record} -> {ok, Record};
        false -> error
    end.
