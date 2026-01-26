%%%-------------------------------------------------------------------
%% @doc AC Eval Mode - Critical guardrail for evaluation-only enforcement
%%
%% This module enforces evaluation-only mode globally, ensuring that:
%% 1. All API responses are marked as advisory (non-contractual)
%% 2. Session-specific secrets invalidate receipts from being contractual
%% 3. Disclaimers are automatically included in all payloads
%% 4. Audit trails capture session context for non-repudiation
%%
%% The module implements eval-mode as a security invariant, not a configuration:
%% - Eval mode cannot be disabled without code change and recompilation
%% - SessionSecret is unique per session, making all receipts non-contractual
%% - Authority is always 'advisory', never 'contractual'
%% - All decorations are cryptographically stamped for audit trail
%%
%% Integration points:
%% - pricing_engine:init/1 calls ensure_eval() at startup
%% - All value_record payloads decorated via decorate_payload/1
%% - All API responses stamped via decorate_meta/2
%% - Session lifecycle managed per connection/request
%%-------------------------------------------------------------------
-module(ac_eval_mode).

%% Core API
-export([
    mode/0,
    authority/0,
    banner/0,
    ensure_eval/0,
    decorate_payload/1,
    decorate_meta/2,
    decorate_receipt/1
]).

%% Session management
-export([
    start_session/0,
    end_session/1,
    get_session_secret/0,
    get_session_id/0,
    validate_session/1
]).

%% Utilities
-export([
    generate_session_secret/0,
    generate_session_id/0,
    compute_session_hash/2,
    verify_session_hash/3
]).

%% Internal exports for testing
-export([
    decorate_payload_internal/3
]).

-include("pricing_engine.hrl").

%% Type definitions
-type authority() :: advisory | contractual.
-type session_id() :: binary().
-type session_secret() :: binary().
-type session_context() :: #{
    session_id => session_id(),
    session_secret => session_secret(),
    created_at => integer(),
    client_id => binary() | undefined,
    request_id => binary() | undefined,
    tags => [atom()]
}.
-type payload() :: map() | #value_record{}.
-type decorated_payload() :: map().
-type result(T) :: {ok, T} | {error, term()}.

%% Process dictionary keys for session management
-define(SESSION_CTX_KEY, ac_eval_mode_session_ctx).
-define(SESSION_SECRET_KEY, ac_eval_mode_session_secret).
-define(SESSION_ID_KEY, ac_eval_mode_session_id).

%% Mode enforcement - hardcoded, cannot change without code modification
-define(MODE, eval).
-define(AUTHORITY, advisory).
-define(BANNER, <<"This pricing calculation is ADVISORY ONLY. "
                  "Not a legal contract or binding agreement. "
                  "For actual billing, use production-mode pricing engine. "
                  "Session-specific and non-contractual.">>).

%%%===================================================================
%% API - Core Functions
%%%===================================================================

%% @doc Returns the current mode (always 'eval')
%% This is a hardcoded invariant that cannot change at runtime.
%% @returns eval
-spec mode() -> eval.
mode() ->
    ?MODE.

%% @doc Returns authority level (always 'advisory')
%% Indicates that calculations are advisory, not binding or contractual.
%% @returns advisory
-spec authority() -> advisory.
authority() ->
    ?AUTHORITY.

%% @doc Returns the eval-mode disclaimer banner
%% Should be displayed to users in UI and included in all API responses.
%% @returns Binary banner string
-spec banner() -> binary().
banner() ->
    ?BANNER.

%% @doc Verify eval mode at initialization
%% Called during pricing_engine:init/1 to enforce eval-only constraint.
%% Fails if app environment is configured for production mode.
%% @returns ok | {error, not_eval_mode}
-spec ensure_eval() -> ok | {error, not_eval_mode}.
ensure_eval() ->
    case application:get_env(pricing_engine, mode, eval) of
        eval ->
            ok;
        production ->
            {error, not_eval_mode};
        UnknownMode ->
            logger:warning("Unknown mode ~p, enforcing eval mode", [UnknownMode]),
            ok
    end.

%% @doc Decorate payload with eval-mode metadata
%% Adds eval_only, authority, disclaimer, and session context to any payload.
%% Ensures all values leave the system marked as advisory.
%% @param Payload Map or value_record to decorate
%% @returns Decorated map with metadata
-spec decorate_payload(payload()) -> result(decorated_payload()).
decorate_payload(Payload) ->
    case get_session_context() of
        {ok, SessionCtx} ->
            decorate_payload_internal(Payload, SessionCtx, ?AUTHORITY);
        {error, Reason} ->
            {error, {session_context_unavailable, Reason}}
    end.

%% @doc Internal payload decoration with explicit session context
%% Separated for testability and clarity.
-spec decorate_payload_internal(payload(), session_context(), authority()) ->
    result(decorated_payload()).
decorate_payload_internal(Payload, SessionCtx, Authority) when is_map(Payload) ->
    DecoratedMap = maps:merge(Payload, #{
        <<"eval_only">> => true,
        <<"authority">> => Authority,
        <<"disclaimer">> => ?BANNER,
        <<"session_id">> => maps:get(session_id, SessionCtx),
        <<"session_timestamp">> => erlang:system_time(millisecond),
        <<"session_hash">> => compute_session_hash(
            maps:get(session_id, SessionCtx),
            maps:get(session_secret, SessionCtx)
        )
    }),
    {ok, DecoratedMap};

decorate_payload_internal(#value_record{} = ValueRecord, SessionCtx, Authority) ->
    %% Convert to map, decorate, and preserve record fields
    PayloadMap = value_record_to_map(ValueRecord),
    DecoratedMap = maps:merge(PayloadMap, #{
        <<"eval_only">> => true,
        <<"authority">> => Authority,
        <<"disclaimer">> => ?BANNER,
        <<"session_id">> => maps:get(session_id, SessionCtx),
        <<"session_timestamp">> => erlang:system_time(millisecond),
        <<"session_hash">> => compute_session_hash(
            maps:get(session_id, SessionCtx),
            maps:get(session_secret, SessionCtx)
        )
    }),
    {ok, DecoratedMap};

decorate_payload_internal(Payload, _SessionCtx, _Authority) ->
    {error, {unsupported_payload_type, typeof(Payload)}}.

%% @doc Decorate metadata for API responses
%% Stamps response metadata with session context, authority, and timestamp.
%% All API responses should include this metadata.
%% @param ResponseMeta Metadata map to decorate
%% @param Options Additional options (e.g., request_id, client_id)
%% @returns Decorated metadata
-spec decorate_meta(map(), map()) -> result(map()).
decorate_meta(ResponseMeta, Options) ->
    case get_session_context() of
        {ok, SessionCtx} ->
            DecoratedMeta = maps:merge(ResponseMeta, #{
                <<"mode">> => ?MODE,
                <<"authority">> => ?AUTHORITY,
                <<"disclaimer">> => ?BANNER,
                <<"session_id">> => maps:get(session_id, SessionCtx),
                <<"session_hash">> => compute_session_hash(
                    maps:get(session_id, SessionCtx),
                    maps:get(session_secret, SessionCtx)
                ),
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"request_id">> => maps:get(request_id, Options, undefined),
                <<"client_id">> => maps:get(client_id, Options, undefined)
            }),
            {ok, DecoratedMeta};
        {error, Reason} ->
            {error, {session_context_unavailable, Reason}}
    end.

%% @doc Decorate receipt with eval-mode markers
%% Ensures receipts cannot be used for contractual purposes.
%% @param Receipt Receipt record to decorate
%% @returns Decorated receipt map
-spec decorate_receipt(#receipt{}) -> result(decorated_payload()).
decorate_receipt(#receipt{} = Receipt) ->
    case get_session_context() of
        {ok, SessionCtx} ->
            ReceiptMap = receipt_to_map(Receipt),
            DecoratedMap = maps:merge(ReceiptMap, #{
                <<"eval_only">> => true,
                <<"authority">> => ?AUTHORITY,
                <<"disclaimer">> => ?BANNER,
                <<"session_id">> => maps:get(session_id, SessionCtx),
                <<"session_hash">> => compute_session_hash(
                    maps:get(session_id, SessionCtx),
                    maps:get(session_secret, SessionCtx)
                ),
                <<"non_contractual">> => true,
                <<"use_for_billing_prohibited">> => true
            }),
            {ok, DecoratedMap};
        {error, Reason} ->
            {error, {session_context_unavailable, Reason}}
    end.

%%%===================================================================
%% Session Management
%%%===================================================================

%% @doc Start a new eval session
%% Generates unique session ID and secret, stores in process dictionary.
%% Must be called at connection/request start.
%% @returns {ok, SessionId, SessionSecret} | {error, Reason}
-spec start_session() -> result({session_id(), session_secret()}).
start_session() ->
    start_session(#{}).

%% @doc Start a new eval session with options
%% @param Options Map with optional client_id, request_id, tags
%% @returns {ok, SessionId, SessionSecret} | {error, Reason}
-spec start_session(map()) -> result({session_id(), session_secret()}).
start_session(Options) when is_map(Options) ->
    try
        SessionId = generate_session_id(),
        SessionSecret = generate_session_secret(),
        Timestamp = erlang:system_time(millisecond),

        SessionCtx = #{
            session_id => SessionId,
            session_secret => SessionSecret,
            created_at => Timestamp,
            client_id => maps:get(client_id, Options, undefined),
            request_id => maps:get(request_id, Options, undefined),
            tags => maps:get(tags, Options, [])
        },

        %% Store in process dictionary for retrieval
        put(?SESSION_CTX_KEY, SessionCtx),
        put(?SESSION_ID_KEY, SessionId),
        put(?SESSION_SECRET_KEY, SessionSecret),

        logger:info(
            "Eval session started",
            #{
                session_id => SessionId,
                client_id => maps:get(client_id, Options, undefined),
                request_id => maps:get(request_id, Options, undefined)
            }
        ),

        {ok, SessionId, SessionSecret}
    catch
        Error:Reason ->
            logger:error(
                "Failed to start eval session",
                #{error => Error, reason => Reason}
            ),
            {error, {session_start_failed, Reason}}
    end.

%% @doc End an eval session
%% Clears session context from process dictionary.
%% Should be called at connection/request end.
%% @param SessionId Session ID to verify before clearing
%% @returns ok | {error, Reason}
-spec end_session(session_id()) -> ok | {error, invalid_session}.
end_session(SessionId) ->
    case get(?SESSION_ID_KEY) of
        SessionId when SessionId =/= undefined ->
            erase(?SESSION_CTX_KEY),
            erase(?SESSION_ID_KEY),
            erase(?SESSION_SECRET_KEY),
            logger:info("Eval session ended", #{session_id => SessionId}),
            ok;
        _ ->
            {error, invalid_session}
    end.

%% @doc Get current session ID
%% @returns {ok, SessionId} | {error, no_active_session}
-spec get_session_id() -> result(session_id()).
get_session_id() ->
    case get(?SESSION_ID_KEY) of
        undefined -> {error, no_active_session};
        SessionId -> {ok, SessionId}
    end.

%% @doc Get current session secret
%% @returns {ok, SessionSecret} | {error, no_active_session}
-spec get_session_secret() -> result(session_secret()).
get_session_secret() ->
    case get(?SESSION_SECRET_KEY) of
        undefined -> {error, no_active_session};
        SessionSecret -> {ok, SessionSecret}
    end.

%% @doc Get full session context
%% @returns {ok, SessionContext} | {error, no_active_session}
-spec get_session_context() -> result(session_context()).
get_session_context() ->
    case get(?SESSION_CTX_KEY) of
        undefined -> {error, no_active_session};
        Ctx -> {ok, Ctx}
    end.

%% @doc Validate a session hash
%% Checks that hash matches session ID and secret.
%% @param SessionHash Hash to validate
%% @returns {ok, valid} | {error, invalid_hash}
-spec validate_session(binary()) -> result(valid).
validate_session(SessionHash) when is_binary(SessionHash) ->
    case get_session_context() of
        {ok, SessionCtx} ->
            ExpectedHash = compute_session_hash(
                maps:get(session_id, SessionCtx),
                maps:get(session_secret, SessionCtx)
            ),
            case constant_time_compare(SessionHash, ExpectedHash) of
                true -> {ok, valid};
                false -> {error, invalid_hash}
            end;
        {error, _} ->
            {error, no_active_session}
    end.

%%%===================================================================
%% Session Utilities
%%%===================================================================

%% @doc Generate unique session ID (UUID v4)
%% @returns Binary session ID
-spec generate_session_id() -> session_id().
generate_session_id() ->
    Random = crypto:strong_rand_bytes(16),
    <<A:32, B:16, C:16, D:16, E:48>> = Random,
    %% Set version 4 (random) and variant bits
    V4C = (C bor 16#4000) band 16#4FFF,
    V4D = (D bor 16#8000) band 16#BFFF,
    <<A:32, B:16, V4C:16, V4D:16, E:48>>.

%% @doc Generate random session secret (32 bytes for HMAC-SHA256)
%% This secret is unique per session and invalidates all receipts
%% for contractual purposes (unless secret is known/stored, which it isn't).
%% @returns Binary session secret
-spec generate_session_secret() -> session_secret().
generate_session_secret() ->
    crypto:strong_rand_bytes(32).

%% @doc Compute session hash (HMAC-SHA256 of session_id with session_secret)
%% This hash appears in all decorated payloads and makes receipts non-contractual
%% because the session_secret is ephemeral and never persisted.
%% @param SessionId Session identifier
%% @param SessionSecret Ephemeral session secret
%% @returns Binary hash
-spec compute_session_hash(session_id(), session_secret()) -> binary().
compute_session_hash(SessionId, SessionSecret) when is_binary(SessionId), is_binary(SessionSecret) ->
    crypto:mac(hmac, sha256, SessionSecret, SessionId).

%% @doc Verify session hash with constant-time comparison
%% Prevents timing attacks on hash verification.
%% @param ProvidedHash Hash to verify
%% @param ComputedHash Expected hash
%% @returns boolean()
-spec verify_session_hash(binary(), binary(), session_secret()) -> boolean().
verify_session_hash(ProvidedHash, SessionId, SessionSecret) when
    is_binary(ProvidedHash),
    is_binary(SessionId),
    is_binary(SessionSecret)
->
    ExpectedHash = compute_session_hash(SessionId, SessionSecret),
    constant_time_compare(ProvidedHash, ExpectedHash).

%%%===================================================================
%% Internal Utilities
%%%===================================================================

%% @doc Constant-time binary comparison (prevents timing attacks)
-spec constant_time_compare(binary(), binary()) -> boolean().
constant_time_compare(A, B) when is_binary(A), is_binary(B) ->
    case byte_size(A) =:= byte_size(B) of
        false -> false;
        true -> compare_bytes(A, B, 0)
    end;
constant_time_compare(_A, _B) ->
    false.

%% @doc Compare bytes with XOR to prevent timing leaks
-spec compare_bytes(binary(), binary(), non_neg_integer()) -> boolean().
compare_bytes(<<>>, <<>>, 0) ->
    true;
compare_bytes(<<>>, <<>>, _Diff) ->
    false;
compare_bytes(<<A:8, RestA/binary>>, <<B:8, RestB/binary>>, Diff) ->
    NewDiff = Diff bor (A bxor B),
    compare_bytes(RestA, RestB, NewDiff).

%% @doc Get simple type name for debugging
-spec typeof(term()) -> atom().
typeof(X) when is_binary(X) -> binary;
typeof(X) when is_atom(X) -> atom;
typeof(X) when is_number(X) -> number;
typeof(X) when is_list(X) -> list;
typeof(X) when is_tuple(X) -> tuple;
typeof(X) when is_map(X) -> map;
typeof(_) -> unknown.

%% @doc Convert value_record to map for decoration
-spec value_record_to_map(#value_record{}) -> map().
value_record_to_map(#value_record{
    customer_id = CustomerId,
    receipt_id = ReceiptId,
    calculated_value = Value,
    billed_price = Price,
    timestamp = Timestamp,
    metrics = Metrics,
    status = Status,
    receipt_hash = ReceiptHash,
    previous_hash = PreviousHash,
    hmac_signature = HmacSig,
    calculation_method = CalcMethod,
    audit_metadata = AuditMeta
}) ->
    #{
        <<"customer_id">> => CustomerId,
        <<"receipt_id">> => ReceiptId,
        <<"calculated_value">> => Value,
        <<"billed_price">> => Price,
        <<"timestamp">> => Timestamp,
        <<"metrics">> => Metrics,
        <<"status">> => Status,
        <<"receipt_hash">> => ReceiptHash,
        <<"previous_hash">> => PreviousHash,
        <<"hmac_signature">> => HmacSig,
        <<"calculation_method">> => CalcMethod,
        <<"audit_metadata">> => AuditMeta
    }.

%% @doc Convert receipt to map for decoration
-spec receipt_to_map(#receipt{}) -> map().
receipt_to_map(#receipt{
    receipt_id = ReceiptId,
    customer_id = CustomerId,
    calculated_value = Value,
    billed_price = Price,
    currency = Currency,
    period_start = PeriodStart,
    period_end = PeriodEnd,
    calculation_timestamp = CalcTimestamp,
    value_hash = ValueHash,
    signature = Signature,
    invoice_id = InvoiceId,
    payment_status = PaymentStatus,
    verified = Verified,
    verification_timestamp = VerificationTimestamp
}) ->
    #{
        <<"receipt_id">> => ReceiptId,
        <<"customer_id">> => CustomerId,
        <<"calculated_value">> => Value,
        <<"billed_price">> => Price,
        <<"currency">> => Currency,
        <<"period_start">> => PeriodStart,
        <<"period_end">> => PeriodEnd,
        <<"calculation_timestamp">> => CalcTimestamp,
        <<"value_hash">> => ValueHash,
        <<"signature">> => Signature,
        <<"invoice_id">> => InvoiceId,
        <<"payment_status">> => PaymentStatus,
        <<"verified">> => Verified,
        <<"verification_timestamp">> => VerificationTimestamp
    }.
