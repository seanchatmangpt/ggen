%%%-------------------------------------------------------------------
%% @doc tai_pubsub_ingress: Pub/Sub push handler
%%
%% Validates Pub/Sub push envelope, decodes message data, routes to tenant governor.
%% Ensures idempotency via messageId deduplication.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_pubsub_ingress).

%% API
-export([handle_push/1]).

-include("tai_autonomics.hrl").

%%%===================================================================
%% API
%%%===================================================================

%% @doc Handle Pub/Sub push envelope
-spec handle_push(Envelope) -> {ok, Receipt} | {error, Reason}
  when Envelope :: map(),
       Receipt :: map(),
       Reason :: atom() | tuple().
handle_push(Envelope) ->
    case validate_envelope(Envelope) of
        {ok, Message} ->
            case decode_message(Message) of
                {ok, Payload} ->
                    MessageId = maps:get(<<"messageId">>, Message, <<>>),
                    case check_idempotency(MessageId) of
                        {ok, not_seen} ->
                            TenantId = maps:get(<<"tenant_id">>, Payload, <<>>),
                            case route_to_governor(TenantId, Payload) of
                                {ok, GovernorReceipt} ->
                                    mark_seen(MessageId),
                                    {ok, GovernorReceipt};
                                {error, Reason} ->
                                    RefusalReceipt = tai_receipts:create_refusal(Reason),
                                    mark_seen(MessageId),
                                    {ok, RefusalReceipt}
                            end;
                        {ok, seen} ->
                            %% Idempotent: return existing receipt
                            {ok, tai_receipts:get_receipt(MessageId)};
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

-spec validate_envelope(Envelope) -> {ok, Message} | {error, Reason}
  when Envelope :: map(),
       Message :: map(),
       Reason :: atom().
validate_envelope(Envelope) ->
    case maps:get(<<"message">>, Envelope, undefined) of
        undefined ->
            {error, invalid_envelope};
        Message when is_map(Message) ->
            case maps:get(<<"data">>, Message, undefined) of
                undefined ->
                    {error, missing_data};
                _ ->
                    {ok, Message}
            end;
        _ ->
            {error, invalid_envelope}
    end.

-spec decode_message(Message) -> {ok, Payload} | {error, Reason}
  when Message :: map(),
       Payload :: map(),
       Reason :: atom().
decode_message(Message) ->
    Data = maps:get(<<"data">>, Message),
    try
        Decoded = base64:decode(Data),
        try
            Payload = jsx:decode(Decoded, [return_maps]),
            {ok, Payload}
        catch
            _:_ ->
                {error, invalid_json}
        end
    catch
        _:_ ->
            {error, decode_failed}
    end.

-spec check_idempotency(MessageId) -> {ok, not_seen | seen} | {error, Reason}
  when MessageId :: binary(),
       Reason :: atom().
check_idempotency(MessageId) when MessageId =:= <<>> ->
    {ok, not_seen};
check_idempotency(MessageId) ->
    case ets:lookup(tai_receipts_dedup, MessageId) of
        [] ->
            {ok, not_seen};
        [_] ->
            {ok, seen}
    end.

-spec mark_seen(MessageId) -> ok
  when MessageId :: binary().
mark_seen(MessageId) when MessageId =/= <<>> ->
    ets:insert(tai_receipts_dedup, {MessageId, erlang:timestamp()}),
    ok;
mark_seen(_) ->
    ok.

-spec route_to_governor(TenantId, Payload) -> {ok, Receipt} | {error, Reason}
  when TenantId :: binary(),
       Payload :: map(),
       Receipt :: map(),
       Reason :: atom().
route_to_governor(TenantId, Payload) ->
    case gproc:where({n, l, {tai_governor, TenantId}}) of
        undefined ->
            {error, governor_not_found};
        Pid ->
            Signal = normalize_signal(Payload),
            case gen_statem:call(Pid, {signal, Signal}) of
                {ok, State, Receipt} ->
                    {ok, Receipt};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec normalize_signal(Payload) -> Signal
  when Payload :: map(),
       Signal :: map().
normalize_signal(Payload) ->
    #{
        tenant_id => maps:get(<<"tenant_id">>, Payload, <<>>),
        metric => maps:get(<<"metric">>, Payload, <<>>),
        value => maps:get(<<"value">>, Payload, 0.0),
        timestamp_ms => maps:get(<<"timestamp_ms">>, Payload, erlang:system_time(millisecond))
    }.
