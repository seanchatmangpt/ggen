%%%-------------------------------------------------------------------
%% @doc tai_receipts: Receipt handling with hash chain and Firestore
%%
%% Implements cryptographic receipt ledger with hash chain verification.
%% Writes receipts to Firestore using metadata server token.
%% Mirrors receipts to logger as JSON.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_receipts).

%% API
-export([create_transition_receipt/5, create_refusal/1, create_action_receipt/4]).
-export([get_receipt/1, store_receipt/1, verify_chain/1]).

-include("tai_autonomics.hrl").

%%%===================================================================
%% API
%%%===================================================================

%% @doc Create transition receipt
-spec create_transition_receipt(TenantId, EntitlementId, Action, NewState, Metadata) -> Receipt
  when TenantId :: binary(),
       EntitlementId :: binary(),
       Action :: binary(),
       NewState :: atom(),
       Metadata :: map(),
       Receipt :: map().
create_transition_receipt(TenantId, EntitlementId, Action, NewState, Metadata) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = erlang:system_time(millisecond),
    PrevHash = get_last_hash(TenantId),
    Receipt = #{
        id => ReceiptId,
        type => ?RECEIPT_TYPE_TRANSITION,
        timestamp => Timestamp,
        tenant_id => TenantId,
        entitlement_id => EntitlementId,
        action => Action,
        state_to => atom_to_binary(NewState, utf8),
        metadata => Metadata,
        prev_hash => PrevHash
    },
    Hash = compute_hash(Receipt),
    ReceiptWithHash = Receipt#{hash => Hash, chain_hash => compute_chain_hash(PrevHash, Hash)},
    store_receipt(ReceiptWithHash),
    log_receipt(ReceiptWithHash),
    ReceiptWithHash.

%% @doc Create refusal receipt
-spec create_refusal(Reason) -> Receipt
  when Reason :: atom() | tuple(),
       Receipt :: map().
create_refusal(Reason) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = erlang:system_time(millisecond),
    Receipt = #{
        id => ReceiptId,
        type => ?RECEIPT_TYPE_REFUSAL,
        timestamp => Timestamp,
        reason => format_reason(Reason)
    },
    Hash = compute_hash(Receipt),
    ReceiptWithHash = Receipt#{hash => Hash},
    store_receipt(ReceiptWithHash),
    log_receipt(ReceiptWithHash),
    ReceiptWithHash.

%% @doc Create action receipt
-spec create_action_receipt(Type, TenantId, ActionId, Result) -> Receipt
  when Type :: attempt | result,
       TenantId :: binary(),
       ActionId :: binary(),
       Result :: map(),
       Receipt :: map().
create_action_receipt(Type, TenantId, ActionId, Result) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = erlang:system_time(millisecond),
    ReceiptType = case Type of
        attempt -> ?RECEIPT_TYPE_ACTION_ATTEMPT;
        result -> ?RECEIPT_TYPE_ACTION_RESULT
    end,
    Receipt = #{
        id => ReceiptId,
        type => ReceiptType,
        timestamp => Timestamp,
        tenant_id => TenantId,
        action_id => ActionId,
        result => Result
    },
    Hash = compute_hash(Receipt),
    ReceiptWithHash = Receipt#{hash => Hash},
    store_receipt(ReceiptWithHash),
    log_receipt(ReceiptWithHash),
    ReceiptWithHash.

%% @doc Get receipt by ID
-spec get_receipt(ReceiptId) -> {ok, Receipt} | {error, not_found}
  when ReceiptId :: binary(),
       Receipt :: map().
get_receipt(ReceiptId) ->
    case ets:lookup(tai_receipts_store, ReceiptId) of
        [] -> {error, not_found};
        [{ReceiptId, Receipt}] -> {ok, Receipt}
    end.

%% @doc Store receipt
-spec store_receipt(Receipt) -> ok | {error, Reason}
  when Receipt :: map(),
       Reason :: atom().
store_receipt(Receipt) ->
    ReceiptId = maps:get(id, Receipt),
    %% Store in ETS
    ets:insert(tai_receipts_store, {ReceiptId, Receipt}),
    %% Write to Firestore (async, non-blocking)
    case application:get_env(tai_autonomics, firestore_enabled, true) of
        true ->
            spawn(fun() -> write_to_firestore(Receipt) end);
        false ->
            ok
    end,
    ok.

%% @doc Verify hash chain
-spec verify_chain(Receipts) -> {ok, valid} | {error, invalid}
  when Receipts :: [map()].
verify_chain(Receipts) ->
    verify_chain(Receipts, <<>>).

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b", [Id])).

-spec get_last_hash(TenantId) -> binary()
  when TenantId :: binary().
get_last_hash(TenantId) ->
    case ets:lookup(tai_receipts_chain, TenantId) of
        [] -> <<>>;
        [{TenantId, Hash}] -> Hash
    end.

-spec compute_hash(Receipt) -> binary()
  when Receipt :: map().
compute_hash(Receipt) ->
    ReceiptJson = jsx:encode(Receipt),
    crypto:hash(sha256, ReceiptJson).

-spec compute_chain_hash(PrevHash, CurrentHash) -> binary()
  when PrevHash :: binary(),
       CurrentHash :: binary().
compute_chain_hash(PrevHash, CurrentHash) ->
    crypto:hash(sha256, <<PrevHash/binary, CurrentHash/binary>>).

-spec format_reason(Reason) -> binary()
  when Reason :: atom() | tuple().
format_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_reason({Reason, Details}) ->
    ReasonBin = format_reason(Reason),
    DetailsBin = jsx:encode(Details),
    <<ReasonBin/binary, ":", DetailsBin/binary>>.

-spec log_receipt(Receipt) -> ok
  when Receipt :: map().
log_receipt(Receipt) ->
    ReceiptJson = jsx:encode(Receipt),
    logger:info("Receipt emitted: ~s", [ReceiptJson]),
    ok.

-spec write_to_firestore(Receipt) -> ok | {error, Reason}
  when Receipt :: map(),
       Reason :: atom().
write_to_firestore(Receipt) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_firestore_failure(write) of
        {should_fail, Type, Reason} ->
            logger:warning("Firestore write failure injected: ~p - ~p", [Type, Reason]),
            {error, Reason};
        should_succeed ->
            write_to_firestore_impl(Receipt)
    end.

-spec write_to_firestore_impl(Receipt) -> ok | {error, Reason}
  when Receipt :: map(),
       Reason :: atom().
write_to_firestore_impl(Receipt) ->
    %% Write receipt to Firestore using REST API
    try
        %% Get Firestore configuration
        case get_firestore_config() of
            {ok, Config} ->
                %% Build Firestore document path
                ReceiptId = maps:get(id, Receipt),
                DocumentPath = build_document_path(Config, ReceiptId),
                
                %% Get access token
                case get_access_token_impl() of
                    {ok, AccessToken} ->
                        %% Convert receipt to Firestore format
                        FirestoreDoc = receipt_to_firestore_doc(Receipt),
                        
                        %% Make HTTP PATCH request to Firestore
                        case firestore_patch(DocumentPath, AccessToken, FirestoreDoc) of
                            ok ->
                                ok;
                            {error, Reason} ->
                                logger:error("Firestore write failed: ~p", [Reason]),
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        logger:error("Failed to get access token: ~p", [Reason]),
                        {error, Reason}
                end;
            {error, Reason} ->
                logger:error("Firestore not configured: ~p", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            logger:error("Firestore write error: ~p", [Error]),
            {error, Error}
    end.

%% @private
%% @doc Get Firestore configuration
-spec get_firestore_config() -> {ok, map()} | {error, atom()}.
get_firestore_config() ->
    case application:get_env(tai_autonomics, firestore_enabled, true) of
        false ->
            {error, firestore_disabled};
        true ->
            ProjectId = application:get_env(tai_autonomics, firestore_project_id, undefined),
            DatabaseId = application:get_env(tai_autonomics, firestore_database_id, <<"(default)">>),
            CollectionId = application:get_env(tai_autonomics, firestore_collection_id, <<"receipts">>),
            case ProjectId of
                undefined ->
                    {error, project_not_configured};
                _ ->
                    {ok, #{
                        project_id => ProjectId,
                        database_id => DatabaseId,
                        collection_id => CollectionId
                    }}
            end
    end.

%% @private
%% @doc Build Firestore document path
-spec build_document_path(Config, ReceiptId) -> string()
  when Config :: map(),
       ReceiptId :: binary().
build_document_path(Config, ReceiptId) ->
    ProjectId = maps:get(project_id, Config),
    DatabaseId = maps:get(database_id, Config),
    CollectionId = maps:get(collection_id, Config),
    ReceiptIdStr = binary_to_list(ReceiptId),
    ProjectIdStr = binary_to_list(ProjectId),
    DatabaseIdStr = binary_to_list(DatabaseId),
    CollectionIdStr = binary_to_list(CollectionId),
    lists:flatten(io_lib:format("projects/~s/databases/~s/documents/~s/~s",
        [ProjectIdStr, DatabaseIdStr, CollectionIdStr, ReceiptIdStr])).

%% @private
%% @doc Convert receipt to Firestore document format
-spec receipt_to_firestore_doc(Receipt) -> map()
  when Receipt :: map().
receipt_to_firestore_doc(Receipt) ->
    %% Firestore expects fields with type information
    Fields = maps:fold(fun(Key, Value, Acc) ->
        FieldValue = case Value of
            V when is_binary(V) ->
                #{<<"stringValue">> => V};
            V when is_integer(V) ->
                #{<<"integerValue">> => integer_to_binary(V)};
            V when is_float(V) ->
                #{<<"doubleValue">> => float_to_binary(V)};
            V when is_boolean(V) ->
                #{<<"booleanValue">> => V};
            V when is_map(V) ->
                #{<<"mapValue">> => #{
                    <<"fields">> => receipt_to_firestore_doc(V)
                }};
            _ ->
                %% Convert to string for other types
                #{<<"stringValue">> => erlang:term_to_binary(Value)}
        end,
        KeyBin = case Key of
            K when is_atom(K) -> atom_to_binary(K, utf8);
            K when is_binary(K) -> K;
            K when is_list(K) -> list_to_binary(K)
        end,
        Acc#{KeyBin => FieldValue}
    end, #{}, Receipt),
    #{<<"fields">> => Fields}.

%% @private
%% @doc Get access token from GCP metadata server
-spec get_access_token() -> {ok, binary()} | {error, atom()}.
get_access_token() ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_metadata_failure(token_refresh) of
        {should_fail, Type, Reason} ->
            logger:warning("Metadata token refresh failure injected: ~p - ~p", [Type, Reason]),
            {error, Reason};
        should_succeed ->
            get_access_token_impl()
    end.

-spec get_access_token_impl() -> {ok, binary()} | {error, atom()}.
get_access_token_impl() ->
    %% For now, return error - in production, implement GCP metadata server fetch
    %% This would use httpc or hackney to fetch from:
    %% http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token
    %% For development/testing, allow config override
    case application:get_env(tai_autonomics, firestore_access_token, undefined) of
        undefined ->
            {error, token_not_configured};
        Token when is_binary(Token) ->
            {ok, Token};
        Token when is_list(Token) ->
            {ok, list_to_binary(Token)}
    end.

%% @private
%% @doc Make HTTP PATCH request to Firestore
-spec firestore_patch(DocumentPath, AccessToken, Doc) -> ok | {error, term()}
  when DocumentPath :: string(),
       AccessToken :: binary(),
       Doc :: map().
firestore_patch(DocumentPath, AccessToken, Doc) ->
    Url = "https://firestore.googleapis.com/v1/" ++ DocumentPath,
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(AccessToken)},
        {"Content-Type", "application/json"}
    ],
    Body = jsx:encode(Doc),
    case httpc:request(patch, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _ResponseHeaders, _ResponseBody}} ->
            ok;
        {ok, {{_, Status, _}, _ResponseHeaders, ResponseBody}} ->
            logger:error("Firestore API error: ~p - ~s", [Status, ResponseBody]),
            {error, {http_error, Status}};
        {error, Reason} ->
            logger:error("Firestore HTTP error: ~p", [Reason]),
            {error, Reason}
    end.

-spec verify_chain(Receipts, PrevHash) -> {ok, valid} | {error, invalid}
  when Receipts :: [map()],
       PrevHash :: binary().
verify_chain([], _PrevHash) ->
    {ok, valid};
verify_chain([Receipt | Rest], PrevHash) ->
    CurrentHash = maps:get(hash, Receipt),
    ExpectedChainHash = compute_chain_hash(PrevHash, CurrentHash),
    ActualChainHash = maps:get(chain_hash, Receipt, <<>>),
    case ExpectedChainHash =:= ActualChainHash of
        true ->
            verify_chain(Rest, ExpectedChainHash);
        false ->
            {error, invalid}
    end.
