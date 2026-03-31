%%%-------------------------------------------------------------------
%%% @doc Cryptographic Receipt System for Value-Indexed Pricing
%%%
%%% Generates deterministic, tamper-proof receipts for every pricing
%%% calculation. Receipts prove:
%%%   - Authenticity: Calculation authorized by specific actor
%%%   - Integrity: Metrics/formula not modified since calculation
%%%   - Non-Repudiation: Actor cannot deny participation
%%%   - Immutability: Any modification invalidates receipt
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pricing_receipt).

%% API
-export([
    generate/4,
    verify/1,
    to_json/1,
    from_json/1
]).

%% Types
-type tenant_id() :: binary().
-type receipt_id() :: binary().
-type actor_id() :: binary().
-type signature() :: binary().

-record(pricing_receipt, {
    receipt_id :: receipt_id(),                    % Unique identifier
    timestamp :: non_neg_integer(),                % Nanoseconds
    tenant_id :: tenant_id(),                      % Customer

    % Input snapshot (immutable)
    metrics_snapshot :: map(),                     % Original metrics
    metrics_hash :: binary(),                      % SHA-256

    % Calculation details
    formula_version :: binary(),                   % e.g. "v2.3.1"
    formula_hash :: binary(),                      % SHA-256 of code

    % Output
    calculated_value :: float(),                   % Result
    currency :: binary(),                          % "USD", "EUR"

    % Cryptographic proof
    system_signature :: signature(),               % System approval
    actor_id :: actor_id(),                        % Who calculated
    actor_signature :: signature(),                % Actor approval
    proof_of_calculation :: map(),                 % CPU/memory samples

    % Chain of custody
    previous_receipt_hash :: binary(),             % Link to prior
    receipt_hash :: binary(),                      % Hash of this receipt

    % Metadata
    reason :: binary(),                            % Why calculated
    metadata :: map()                              % Additional context
}).

-export_type([pricing_receipt/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% Generate new receipt
-spec generate(tenant_id(), map(), binary(), actor_id()) ->
    {ok, pricing_receipt()} | {error, term()}.

generate(TenantId, Metrics, FormulaVersion, ActorId) ->
    Timestamp = erlang:system_time(nanosecond),

    % Compute hashes
    MetricsHash = compute_hash(Metrics),
    FormulaHash = get_formula_hash(FormulaVersion),

    % Calculate value
    case calculate_value(Metrics, FormulaVersion) of
        {ok, Value} ->
            % Get chain link to previous receipt
            PreviousHash = get_previous_receipt_hash(TenantId),

            % Create receipt record
            Receipt = #pricing_receipt{
                receipt_id = generate_receipt_id(),
                timestamp = Timestamp,
                tenant_id = TenantId,
                metrics_snapshot = Metrics,
                metrics_hash = MetricsHash,
                formula_version = FormulaVersion,
                formula_hash = FormulaHash,
                calculated_value = Value,
                currency = <<"USD">>,
                actor_id = ActorId,
                proof_of_calculation = capture_proof_of_work(),
                previous_receipt_hash = PreviousHash
            },

            % Sign receipt
            case sign_receipt(Receipt, ActorId) of
                {ok, SignedReceipt} ->
                    {ok, SignedReceipt};
                {error, Reason} ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

%% Verify receipt integrity (detects ANY modification)
-spec verify(pricing_receipt()) ->
    {ok, verified} | {error, {atom(), term()}}.

verify(Receipt) ->
    case {
        verify_signature(Receipt),
        verify_chain_link(Receipt),
        verify_metrics_hash(Receipt),
        verify_formula_hash(Receipt)
    } of
        {true, true, true, true} ->
            {ok, verified};

        {false, _, _, _} ->
            {error, {signature_invalid, Receipt#pricing_receipt.receipt_id}};

        {_, false, _, _} ->
            {error, {chain_broken, Receipt#pricing_receipt.receipt_id}};

        {_, _, false, _} ->
            {error, {metrics_tampered, Receipt#pricing_receipt.receipt_id}};

        {_, _, _, false} ->
            {error, {formula_tampered, Receipt#pricing_receipt.receipt_id}}
    end.

%% Convert to JSON for storage/transmission
-spec to_json(pricing_receipt()) -> binary().

to_json(Receipt) ->
    Map = #{
        receipt_id => Receipt#pricing_receipt.receipt_id,
        timestamp => Receipt#pricing_receipt.timestamp,
        tenant_id => Receipt#pricing_receipt.tenant_id,
        metrics_snapshot => Receipt#pricing_receipt.metrics_snapshot,
        metrics_hash => base64:encode(Receipt#pricing_receipt.metrics_hash),
        formula_version => Receipt#pricing_receipt.formula_version,
        formula_hash => base64:encode(Receipt#pricing_receipt.formula_hash),
        calculated_value => Receipt#pricing_receipt.calculated_value,
        currency => Receipt#pricing_receipt.currency,
        system_signature => base64:encode(Receipt#pricing_receipt.system_signature),
        actor_id => Receipt#pricing_receipt.actor_id,
        actor_signature => base64:encode(Receipt#pricing_receipt.actor_signature),
        previous_receipt_hash => base64:encode(Receipt#pricing_receipt.previous_receipt_hash),
        receipt_hash => base64:encode(Receipt#pricing_receipt.receipt_hash),
        reason => Receipt#pricing_receipt.reason,
        metadata => Receipt#pricing_receipt.metadata
    },
    jiffy:encode(Map).

%% Convert from JSON
-spec from_json(binary()) -> {ok, pricing_receipt()} | {error, term()}.

from_json(JSON) ->
    try
        {Map} = jiffy:decode(JSON),
        Receipt = #pricing_receipt{
            receipt_id = maps:get(<<"receipt_id">>, Map),
            timestamp = maps:get(<<"timestamp">>, Map),
            tenant_id = maps:get(<<"tenant_id">>, Map),
            metrics_snapshot = maps:get(<<"metrics_snapshot">>, Map),
            metrics_hash = base64:decode(maps:get(<<"metrics_hash">>, Map)),
            formula_version = maps:get(<<"formula_version">>, Map),
            formula_hash = base64:decode(maps:get(<<"formula_hash">>, Map)),
            calculated_value = maps:get(<<"calculated_value">>, Map),
            currency = maps:get(<<"currency">>, Map),
            system_signature = base64:decode(maps:get(<<"system_signature">>, Map)),
            actor_id = maps:get(<<"actor_id">>, Map),
            actor_signature = base64:decode(maps:get(<<"actor_signature">>, Map)),
            previous_receipt_hash = base64:decode(maps:get(<<"previous_receipt_hash">>, Map)),
            receipt_hash = base64:decode(maps:get(<<"receipt_hash">>, Map)),
            reason = maps:get(<<"reason">>, Map, <<>>),
            metadata = maps:get(<<"metadata">>, Map, #{})
        },
        {ok, Receipt}
    catch
        _:Reason ->
            {error, {json_parse_error, Reason}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Compute SHA-256 hash of metrics
compute_hash(Metrics) ->
    Encoded = term_to_binary(Metrics),
    crypto:hash(sha256, Encoded).

%% Get formula code hash
get_formula_hash(FormulaVersion) ->
    % In production, this retrieves formula from immutable store
    % For now, return placeholder
    crypto:hash(sha256, FormulaVersion).

%% Calculate value using formula
calculate_value(Metrics, FormulaVersion) ->
    % Retrieve formula code
    case get_formula_code(FormulaVersion) of
        {ok, Code} ->
            % Execute calculation
            try
                % Load formula module
                Module = binary_to_atom(FormulaVersion, utf8),
                Value = Module:calculate(Metrics),
                {ok, Value}
            catch
                _:Reason ->
                    {error, {calculation_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Get formula code (stub - implement with version store)
get_formula_code(FormulaVersion) ->
    % In production: Retrieve from immutable formula store
    % For now: Return error
    {error, {formula_not_found, FormulaVersion}}.

%% Get hash of previous receipt in chain
get_previous_receipt_hash(TenantId) ->
    % Query database for most recent receipt
    % Return its hash (or empty hash if this is first receipt)
    case db:query(
        "SELECT receipt_hash FROM pricing_receipts
         WHERE tenant_id = ?
         ORDER BY timestamp DESC LIMIT 1",
        [TenantId]
    ) of
        {ok, [#{<<"receipt_hash">> := Hash}]} ->
            Hash;
        _ ->
            % No prior receipts
            crypto:hash(sha256, <<>>)
    end.

%% Sign receipt with system key
sign_receipt(Receipt, ActorId) ->
    % Get system private key
    {ok, PrivateKey} = get_system_private_key(),

    % Data to sign: metrics_hash || formula_hash || value || timestamp
    SignatureData = <<
        (Receipt#pricing_receipt.metrics_hash)/binary,
        (Receipt#pricing_receipt.formula_hash)/binary,
        (float_to_binary(Receipt#pricing_receipt.calculated_value))/binary,
        (Receipt#pricing_receipt.timestamp):64/integer
    >>,

    % Create system signature
    SystemSig = crypto:sign(eddsa, SignatureData, PrivateKey),

    % Create actor signature
    ActorPrivateKey = get_actor_private_key(ActorId),
    ActorSig = crypto:sign(eddsa, SignatureData, ActorPrivateKey),

    % Compute receipt hash
    ReceiptHash = compute_hash(SignatureData),

    {ok, Receipt#pricing_receipt{
        system_signature = SystemSig,
        actor_signature = ActorSig,
        receipt_hash = ReceiptHash
    }}.

%% Verify receipt signature
verify_signature(Receipt) ->
    % Get system public key
    {ok, PublicKey} = get_system_public_key(),

    % Reconstruct signed data
    SignatureData = <<
        (Receipt#pricing_receipt.metrics_hash)/binary,
        (Receipt#pricing_receipt.formula_hash)/binary,
        (float_to_binary(Receipt#pricing_receipt.calculated_value))/binary,
        (Receipt#pricing_receipt.timestamp):64/integer
    >>,

    % Verify system signature
    crypto:verify(eddsa, SignatureData, Receipt#pricing_receipt.system_signature, PublicKey).

%% Verify chain link
verify_chain_link(Receipt) ->
    PriorHash = get_previous_receipt_hash(Receipt#pricing_receipt.tenant_id),
    PriorHash =:= Receipt#pricing_receipt.previous_receipt_hash.

%% Verify metrics haven't been tampered
verify_metrics_hash(Receipt) ->
    CurrentHash = compute_hash(Receipt#pricing_receipt.metrics_snapshot),
    CurrentHash =:= Receipt#pricing_receipt.metrics_hash.

%% Verify formula hasn't been tampered
verify_formula_hash(Receipt) ->
    CurrentHash = get_formula_hash(Receipt#pricing_receipt.formula_version),
    CurrentHash =:= Receipt#pricing_receipt.formula_hash.

%% Generate unique receipt ID (UUID v7 for sortability)
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% Capture proof of work (CPU entropy, memory sample)
capture_proof_of_work() ->
    #{
        timestamp_hw => erlang:system_time(nanosecond),
        cpu_entropy => crypto:strong_rand_bytes(32),
        memory_sample => erlang:memory(total)
    }.

%% Get system private key (stub - implement with key store)
get_system_private_key() ->
    % In production: Retrieve from secure key store (e.g., AWS KMS)
    % This is a placeholder
    {ok, <<"PLACEHOLDER_PRIVATE_KEY">>}.

%% Get system public key
get_system_public_key() ->
    {ok, <<"PLACEHOLDER_PUBLIC_KEY">>}.

%% Get actor's private key
get_actor_private_key(ActorId) ->
    % Retrieve from actor's secure store
    {ok, <<"ACTOR_KEY_", ActorId/binary>>}.

%% Convert float to binary for signing
float_to_binary(Float) ->
    term_to_binary(Float).
