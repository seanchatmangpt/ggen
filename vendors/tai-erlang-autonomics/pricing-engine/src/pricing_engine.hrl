%%%-------------------------------------------------------------------
%% @doc Pricing Engine - Header definitions
%%%-------------------------------------------------------------------

%% Value record - immutable record of a value calculation
-record(value_record, {
    %% Unique identifiers
    customer_id :: binary(),
    receipt_id = undefined :: undefined | binary(),

    %% Calculated values
    calculated_value :: float(),           %% V = f(metrics)
    billed_price :: float(),                %% P = α × V

    %% Metadata
    timestamp :: integer(),                 %% erlang:system_time(millisecond)
    metrics :: [{binary(), float()}],       %% Component metrics
    status = pending :: pending | completed | failed,

    %% Cryptographic integrity
    receipt_hash :: binary(),               %% SHA-256(record)
    previous_hash :: undefined | binary(),  %% Hash chain for merkle verification
    hmac_signature :: undefined | binary(), %% HMAC-SHA256 for signature verification

    %% Audit trail
    calculation_method = weighted_sum :: atom(),
    audit_metadata = #{} :: map()
}).

%% Customer configuration
-record(customer_config, {
    customer_id :: binary(),
    billing_period = monthly :: monthly | quarterly | annual,

    %% Pricing formula: P = α × V
    alpha_coefficient = 100.0 :: float(),  %% Price per unit value
    min_price = 0.0 :: float(),            %% Floor price (USD)
    max_price = 10000.0 :: float(),        %% Ceiling price (USD)

    %% Metric configuration
    metric_weights = #{} :: map(),         %% {MetricName => Weight}
    aggregation_method = weighted_sum :: atom(),

    %% Security
    hmac_key :: undefined | binary(),
    customer_isolation = true :: boolean()
}).

%% Receipt - Customer-facing proof of value calculation
-record(receipt, {
    receipt_id :: binary(),              %% UUID
    customer_id :: binary(),

    %% Value and billing
    calculated_value :: float(),
    billed_price :: float(),
    currency = <<"USD">> :: binary(),

    %% Period
    period_start :: integer(),           %% timestamp in ms
    period_end :: integer(),
    calculation_timestamp :: integer(),

    %% Cryptographic proof
    value_hash :: binary(),              %% SHA-256 of value record
    signature :: binary(),               %% HMAC-SHA256 signature

    %% Billing
    invoice_id :: undefined | binary(),
    payment_status = pending :: pending | paid | failed | refunded,

    %% Audit
    verified = false :: boolean(),
    verification_timestamp :: undefined | integer()
}).

%% Billing cycle record
-record(billing_cycle, {
    cycle_id :: binary(),
    customer_id :: binary(),

    start_time :: integer(),
    end_time :: integer(),

    total_value :: float(),
    total_price :: float(),

    receipts = [] :: [binary()],  %% receipt IDs

    invoice_id :: undefined | binary(),
    payment_status = pending :: pending | paid | failed | refunded,

    stripe_charge_id :: undefined | binary(),

    created_at :: integer(),
    completed_at :: undefined | integer()
}).

%% Audit entry - Immutable log for compliance
-record(audit_entry, {
    entry_id :: binary(),        %% UUID
    timestamp :: integer(),      %% erlang:system_time(millisecond)

    action :: binary(),          %% 'calculate', 'verify', 'bill', 'refund'
    customer_id :: binary(),

    data :: map(),               %% Action-specific data

    signature :: binary(),       %% Ed25519 signature
    previous_entry_hash :: binary(),  %% Chain for integrity

    status = completed :: completed | failed
}).

%% Anti-fraud event
-record(fraud_event, {
    event_id :: binary(),
    timestamp :: integer(),

    customer_id :: binary(),
    event_type :: atom(),       %% spike, anomaly, signature_mismatch, etc.

    current_value :: float(),
    previous_value :: float(),
    change_percentage :: float(),

    severity = warning :: warning | critical,
    investigation_status = pending :: pending | investigated | resolved | false_positive,

    metadata = #{} :: map()
}).
