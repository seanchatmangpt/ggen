%%%-------------------------------------------------------------------
%% @doc tai_marketplace_ingress: Marketplace entitlement handler
%%
%% Verifies signatures, handles entitlement transitions, updates tenant state.
%% Emits receipts for all transitions and refusals.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_marketplace_ingress).

%% API
-export([handle_event/1]).

-include("tai_autonomics.hrl").

%%%===================================================================
%% API
%%%===================================================================

%% @doc Handle marketplace entitlement event
-spec handle_event(Event) -> {ok, Receipt} | {error, Reason}
  when Event :: map(),
       Receipt :: map(),
       Reason :: atom() | tuple().
handle_event(Event) ->
    case validate_event(Event) of
        {ok, ValidEvent} ->
            case verify_signature(ValidEvent) of
                {ok, verified} ->
                    TenantId = maps:get(<<"tenant_id">>, ValidEvent, <<>>),
                    EntitlementId = maps:get(<<"entitlement_id">>, ValidEvent, <<>>),
                    Action = maps:get(<<"action">>, ValidEvent, <<>>),
                    case check_entitlement(TenantId, EntitlementId) of
                        {ok, active} ->
                            case process_transition(TenantId, EntitlementId, Action, ValidEvent) of
                                {ok, Receipt} ->
                                    {ok, Receipt};
                                {error, Reason} ->
                                    RefusalReceipt = tai_receipts:create_refusal(Reason),
                                    {ok, RefusalReceipt}
                            end;
                        {ok, inactive} ->
                            RefusalReceipt = tai_receipts:create_refusal(entitlement_inactive),
                            {ok, RefusalReceipt};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec validate_event(Event) -> {ok, ValidEvent} | {error, Reason}
  when Event :: map(),
       ValidEvent :: map(),
       Reason :: atom().
validate_event(Event) ->
    Required = [<<"tenant_id">>, <<"entitlement_id">>, <<"action">>],
    case lists:all(fun(Key) -> maps:is_key(Key, Event) end, Required) of
        true ->
            {ok, Event};
        false ->
            {error, missing_required_fields}
    end.

-spec verify_signature(Event) -> {ok, verified} | {error, Reason}
  when Event :: map(),
       Reason :: atom().
verify_signature(Event) ->
    %% If signature verification is disabled, skip
    case application:get_env(tai_autonomics, verify_signatures, false) of
        false ->
            {ok, verified};
        true ->
            case maps:get(<<"signature">>, Event, undefined) of
                undefined ->
                    {error, invalid_signature};
                Signature when is_binary(Signature) ->
                    %% Verify JWT signature using JOSE
                    %% Signature is expected to be a JWT token
                    case get_verification_key() of
                        {ok, Key} ->
                            try
                                %% Decode and verify JWT
                                case jose_jwt:verify(Key, Signature) of
                                    {true, Claims, _JWS} ->
                                        %% Verify claims contain required fields
                                        case maps:is_key(<<"tenant_id">>, Claims) andalso
                                             maps:is_key(<<"entitlement_id">>, Claims) of
                                            true ->
                                                {ok, verified};
                                            false ->
                                                {error, invalid_claims}
                                        end;
                                    {false, _} ->
                                        {error, invalid_signature}
                                end
                            catch
                                _:Error ->
                                    logger:error("JOSE verification error: ~p", [Error]),
                                    {error, verification_failed}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, invalid_signature_format}
            end
    end.

%% @private
%% @doc Get verification key from config or GCP metadata server
-spec get_verification_key() -> {ok, jose_jwk:key()} | {error, atom()}.
get_verification_key() ->
    case application:get_env(tai_autonomics, verification_key, undefined) of
        undefined ->
            %% Try to fetch from GCP metadata server
            fetch_key_from_metadata();
        KeyBin when is_binary(KeyBin) ->
            try
                {ok, jose_jwk:from_pem(KeyBin)}
            catch
                _:Error ->
                    logger:error("Failed to parse verification key: ~p", [Error]),
                    {error, invalid_key_format}
            end;
        KeyMap when is_map(KeyMap) ->
            try
                {ok, jose_jwk:from_map(KeyMap)}
            catch
                _:Error ->
                    logger:error("Failed to parse verification key map: ~p", [Error]),
                    {error, invalid_key_format}
            end
    end.

%% @private
%% @doc Fetch verification key from GCP metadata server
-spec fetch_key_from_metadata() -> {ok, jose_jwk:key()} | {error, atom()}.
fetch_key_from_metadata() ->
    %% For now, return error - in production, implement GCP metadata server fetch
    %% This would use httpc or hackney to fetch from:
    %% http://metadata.google.internal/computeMetadata/v1/instance/attributes/verification-key
    {error, key_not_configured}.

-spec check_entitlement(TenantId, EntitlementId) -> {ok, active | inactive} | {error, Reason}
  when TenantId :: binary(),
       EntitlementId :: binary(),
       Reason :: atom().
check_entitlement(TenantId, EntitlementId) ->
    case gproc:where({n, l, {entitlement_governor, TenantId, EntitlementId}}) of
        undefined ->
            {error, governor_not_found};
        Pid ->
            case gen_statem:call(Pid, get_state) of
                {ok, entitled} ->
                    {ok, active};
                {ok, _OtherState} ->
                    {ok, inactive};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec process_transition(TenantId, EntitlementId, Action, Event) -> {ok, Receipt} | {error, Reason}
  when TenantId :: binary(),
       EntitlementId :: binary(),
       Action :: binary(),
       Event :: map(),
       Receipt :: map(),
       Reason :: atom().
process_transition(TenantId, EntitlementId, Action, Event) ->
    case gproc:where({n, l, {entitlement_governor, TenantId, EntitlementId}}) of
        undefined ->
            {error, governor_not_found};
        Pid ->
            Transition = case Action of
                <<"grant">> -> {grant_entitlement, maps:get(<<"customer_id">>, Event, <<>>), Event};
                <<"revoke">> -> {revoke_entitlement, maps:get(<<"reason">>, Event, <<>>), Event};
                <<"suspend">> -> {suspend_entitlement, maps:get(<<"reason">>, Event, <<>>), Event};
                _ ->
                    {error, unknown_action}
            end,
            case Transition of
                {error, Reason} ->
                    {error, Reason};
                _ ->
                    case gen_statem:call(Pid, Transition) of
                        {ok, NewState} ->
                            Receipt = tai_receipts:create_transition_receipt(
                                TenantId,
                                EntitlementId,
                                Action,
                                NewState,
                                Event
                            ),
                            {ok, Receipt};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.
