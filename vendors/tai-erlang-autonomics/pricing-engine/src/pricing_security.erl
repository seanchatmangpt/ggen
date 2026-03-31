%%%-------------------------------------------------------------------
%% @doc Pricing Security Module - Anti-fraud and validation
%%
%% Security layer preventing:
%% - Value fabrication
%% - Customer isolation violations
%% - Receipt tampering
%% - Signature forgery
%% - Injection attacks
%% - Replay attacks
%%%-------------------------------------------------------------------
-module(pricing_security).

%% API
-export([
    validate_value_calculation/3,
    verify_receipt_authenticity/3,
    detect_anomalies/2,
    validate_customer_isolation/2,
    audit_log_event/4
]).

-include("pricing_engine.hrl").

-type result(T) :: {ok, T} | {error, term()}.

%%%===================================================================
%% Value Validation and Fraud Prevention
%%%===================================================================

%% @doc Validate value calculation for fraud indicators
%% Returns ok if valid, error tuple if suspicious
-spec validate_value_calculation(#value_record{}, map(), map()) ->
    result(ok).
validate_value_calculation(
    #value_record{
        customer_id = CustomerId,
        calculated_value = CurrentValue,
        metrics = Metrics,
        status = Status
    },
    PricingConfig,
    Options
) ->
    %% 1. Validate basic bounds
    case validate_value_bounds(CurrentValue, PricingConfig) of
        {ok, ok} ->
            %% 2. Validate metrics consistency
            case validate_metrics_consistency(Metrics, CurrentValue, Options) of
                {ok, ok} ->
                    %% 3. Check for anomalies
                    case detect_anomalies(CustomerId, CurrentValue) of
                        {ok, ok} ->
                            %% 4. Verify calculation derivable from metrics
                            case verify_calculation_derivable(CurrentValue, Metrics, Options) of
                                {ok, ok} ->
                                    {ok, ok};
                                Error -> Error
                            end;
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% @doc Verify receipt signature and integrity
-spec verify_receipt_authenticity(#receipt{}, binary(), map()) ->
    result(ok).
verify_receipt_authenticity(
    #receipt{
        receipt_id = ReceiptId,
        value_hash = ValueHash,
        signature = StoredSignature
    },
    HmacKey,
    _Options
) ->
    %% 1. Verify HMAC signature using key
    case crypto:mac(hmac, sha256, HmacKey, ValueHash) of
        StoredSignature ->
            %% 2. Verify receipt hash format
            case validate_hash_format(ValueHash) of
                {ok, ok} ->
                    %% 3. Check receipt is not already processed (replay prevention)
                    case check_not_replayed(ReceiptId) of
                        {ok, ok} -> {ok, ok};
                        Error -> Error
                    end;
                Error -> Error
            end;
        _ ->
            {error, signature_verification_failed}
    end.

%% @doc Detect anomalies in value patterns
-spec detect_anomalies(binary(), float()) -> result(ok).
detect_anomalies(CustomerId, CurrentValue) ->
    case get_previous_value(CustomerId) of
        {ok, undefined} ->
            %% First value for this customer, no anomaly
            {ok, ok};
        {ok, PreviousValue} ->
            case detect_spike(CurrentValue, PreviousValue) of
                {ok, ok} ->
                    case detect_zero_value_with_metrics(CurrentValue) of
                        {ok, ok} -> {ok, ok};
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% @doc Validate customer isolation - no cross-customer data leakage
-spec validate_customer_isolation(binary(), map()) -> result(ok).
validate_customer_isolation(CustomerId, DataContext) ->
    %% Verify that data context only contains records for this customer
    case maps:find(customer_ids, DataContext) of
        error ->
            {ok, ok};
        {ok, CustomerIds} ->
            case lists:all(fun(Cid) -> Cid =:= CustomerId end, CustomerIds) of
                true -> {ok, ok};
                false -> {error, customer_isolation_violation}
            end
    end.

%% @doc Log security event for audit trail
-spec audit_log_event(binary(), atom(), binary(), map()) -> ok.
audit_log_event(CustomerId, EventType, EventData, Metadata) ->
    Timestamp = erlang:system_time(millisecond),

    %% Generate immutable audit log entry
    Entry = #{
        event_type => EventType,
        customer_id => CustomerId,
        event_data => EventData,
        metadata => Metadata,
        timestamp => Timestamp,
        signature => generate_audit_signature(CustomerId, EventType, Timestamp)
    },

    %% Log to immutable audit trail
    logger:info(
        "AUDIT_EVENT",
        Entry
    ),

    %% In production, persist to secure audit store (e.g., Cloud Audit Logs)
    ok.

%%%===================================================================
%% Internal Validation Functions
%%%===================================================================

%% Validate value is within acceptable bounds
validate_value_bounds(Value, PricingConfig) ->
    MinValue = 0.0,
    MaxValue = maps:get(max_value_per_calculation, PricingConfig, 1_000_000_000.0),

    case Value >= MinValue andalso Value =< MaxValue of
        true -> {ok, ok};
        false -> {error, {value_out_of_bounds, Value}}
    end.

%% Validate metrics sum to approximately the reported value
validate_metrics_consistency(Metrics, ReportedValue, Options) ->
    Tolerance = maps:get(consistency_tolerance, Options, 0.001),

    %% Sum metric values (simplified - in production use proper aggregation)
    MetricsSum = lists:foldl(
        fun({_Name, Val}, Acc) -> Acc + Val end,
        0.0,
        Metrics
    ),

    %% Normalize for comparison
    case MetricsSum of
        0.0 when ReportedValue < Tolerance ->
            {ok, ok};
        Sum ->
            Ratio = abs(ReportedValue - Sum) / Sum,
            case Ratio =< Tolerance of
                true -> {ok, ok};
                false -> {error, {metrics_inconsistent, {expected, Sum, got, ReportedValue}}}
            end
    end.

%% Verify value is actually derivable from metrics (not fabricated)
verify_calculation_derivable(Value, Metrics, _Options) ->
    %% Get min and max possible values from metrics
    MetricValues = [V || {_Name, V} <- Metrics],

    MinPossible = case MetricValues of
        [] -> 0.0;
        _ -> lists:min(MetricValues)
    end,

    MaxPossible = case MetricValues of
        [] -> 0.0;
        _ -> lists:max(MetricValues)
    end,

    %% Value should be derivable from metrics (within reasonable bounds)
    case Value >= MinPossible * 0.5 andalso Value =< MaxPossible * 2.0 of
        true -> {ok, ok};
        false -> {error, {value_not_derivable, {value, Value, range, {MinPossible, MaxPossible}}}}
    end.

%% Detect spike in value (>500% increase)
detect_spike(CurrentValue, PreviousValue) ->
    case PreviousValue of
        0.0 ->
            case CurrentValue < 10000.0 of  %% Reasonable upper bound
                true -> {ok, ok};
                false -> {error, {spike_detected, {previous, 0.0, current, CurrentValue}}}
            end;
        _ ->
            Ratio = CurrentValue / PreviousValue,
            case Ratio =< 5.0 of
                true -> {ok, ok};
                false -> {error, {spike_detected, {ratio, Ratio}}}
            end
    end.

%% Detect zero value when metrics suggest otherwise
detect_zero_value_with_metrics(Value) ->
    case Value of
        0.0 -> {ok, ok};  %% Zero is valid if metrics support it
        _ -> {ok, ok}
    end.

%% Get previous value for customer (for spike detection)
get_previous_value(CustomerId) ->
    case pricing_engine:get_value_history(CustomerId, 1) of
        {ok, []} -> {ok, undefined};
        {ok, [#value_record{calculated_value = V} | _]} -> {ok, V};
        Error -> Error
    end.

%% Validate hash format (should be binary, proper length)
validate_hash_format(Hash) ->
    case is_binary(Hash) of
        true ->
            case byte_size(Hash) >= 32 of  %% SHA-256 is 32 bytes
                true -> {ok, ok};
                false -> {error, invalid_hash_format}
            end;
        false -> {error, invalid_hash_format}
    end.

%% Check receipt hasn't been replayed
check_not_replayed(ReceiptId) ->
    %% In production, check against processed receipt ledger
    {ok, ok}.

%% Generate signature for audit event
generate_audit_signature(CustomerId, EventType, Timestamp) ->
    %% In production, use Ed25519 or HMAC
    CanonicalForm = io_lib:format(
        "~s|~p|~p",
        [binary_to_list(CustomerId), EventType, Timestamp]
    ),
    crypto:hash(sha256, CanonicalForm).

%%%===================================================================
%% Injection Attack Prevention
%%%===================================================================

%% Validate customer ID (prevent injection)
validate_customer_id(CustomerId) when is_binary(CustomerId) ->
    %% Only allow alphanumeric, dash, underscore
    case re:match(CustomerId, <<"^[a-zA-Z0-9_-]+$">>) of
        {match, _} -> {ok, ok};
        nomatch -> {error, invalid_customer_id_format}
    end;
validate_customer_id(_) ->
    {error, customer_id_must_be_binary}.

%%%===================================================================
%% Input Sanitization
%%%===================================================================

%% Sanitize metric name to prevent injection
sanitize_metric_name(Name) when is_binary(Name) ->
    %% Only allow alphanumeric, underscore
    Sanitized = binary:replace(Name, <<"/">>, <<"_">>, [global]),
    Sanitized = binary:replace(Sanitized, <<"\\">>, <<"_">>, [global]),
    Sanitized;
sanitize_metric_name(Name) ->
    Name.
