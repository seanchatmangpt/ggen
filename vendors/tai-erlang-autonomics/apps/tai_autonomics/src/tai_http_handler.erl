%%%-------------------------------------------------------------------
%% @doc tai_http_handler: Cowboy HTTP handlers
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_http_handler).

%% Cowboy callbacks
-export([handle_health/2, handle_pubsub/2, handle_marketplace/2]).

%%%===================================================================
%% HTTP Handlers
%%%===================================================================

%% @doc Health check endpoint
-spec handle_health(Req, State) -> {ok, Req, State}
  when Req :: cowboy_req:req(),
       State :: term().
handle_health(Req, State) ->
    %% Check if dependencies are ready
    HealthStatus = check_dependencies(),
    {StatusCode, StatusAtom} = case HealthStatus of
        ok -> {200, ok};
        {error, _Reason} -> {503, unavailable}
    end,
    Body = jsx:encode(#{status => atom_to_binary(StatusAtom, utf8)}),
    Req2 = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req),
    {ok, Req2, State}.

%% @doc Pub/Sub push handler
-spec handle_pubsub(Req, State) -> {ok, Req, State}
  when Req :: cowboy_req:req(),
       State :: term().
handle_pubsub(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            handle_pubsub_body(Body, Req2, State);
        {error, _Reason} ->
            RefusalReceipt = tai_receipts:create_refusal(read_body_failed),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(RefusalReceipt), Req),
            {ok, Req2, State}
    end.

-spec handle_pubsub_body(Body, Req, State) -> {ok, Req, State}
  when Body :: binary(),
       Req :: cowboy_req:req(),
       State :: term().
handle_pubsub_body(Body, Req, State) ->
    case catch jsx:decode(Body, [return_maps]) of
        Envelope when is_map(Envelope) ->
            case tai_pubsub_ingress:handle_push(Envelope) of
                {ok, Receipt} ->
                    Req2 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(Receipt), Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    %% Emit refusal receipt
                    RefusalReceipt = tai_receipts:create_refusal(Reason),
                    StatusCode = case Reason of
                        invalid_envelope -> 400;
                        _ -> 400
                    end,
                    Req2 = cowboy_req:reply(StatusCode, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(RefusalReceipt), Req),
                    {ok, Req2, State}
            end;
        _ ->
            RefusalReceipt = tai_receipts:create_refusal(invalid_json),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(RefusalReceipt), Req),
            {ok, Req2, State}
    end.

%% @doc Marketplace entitlement handler
-spec handle_marketplace(Req, State) -> {ok, Req, State}
  when Req :: cowboy_req:req(),
       State :: term().
handle_marketplace(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            handle_marketplace_body(Body, Req2, State);
        {error, _Reason} ->
            RefusalReceipt = tai_receipts:create_refusal(read_body_failed),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(RefusalReceipt), Req),
            {ok, Req2, State}
    end.

-spec handle_marketplace_body(Body, Req, State) -> {ok, Req, State}
  when Body :: binary(),
       Req :: cowboy_req:req(),
       State :: term().
handle_marketplace_body(Body, Req, State) ->
    case catch jsx:decode(Body, [return_maps]) of
        Event when is_map(Event) ->
            case tai_marketplace_ingress:handle_event(Event) of
                {ok, Receipt} ->
                    Req2 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(Receipt), Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    RefusalReceipt = tai_receipts:create_refusal(Reason),
                    StatusCode = case Reason of
                        invalid_signature -> 401;
                        entitlement_inactive -> 403;
                        _ -> 400
                    end,
                    Req2 = cowboy_req:reply(StatusCode, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(RefusalReceipt), Req),
                    {ok, Req2, State}
            end;
        _ ->
            RefusalReceipt = tai_receipts:create_refusal(invalid_json),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(RefusalReceipt), Req),
            {ok, Req2, State}
    end.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec check_dependencies() -> ok | {error, term()}.
check_dependencies() ->
    %% Check if supervisors are running
    case whereis(governance_sup) of
        undefined -> {error, governance_sup_not_running};
        _ ->
            case whereis(receipt_ledger_sup) of
                undefined -> {error, receipt_ledger_sup_not_running};
                _ ->
                    %% Check GCP dependencies if enabled
                    case application:get_env(tai_autonomics, firestore_enabled, true) of
                        true ->
                            %% Verify Firestore connectivity (handle case where gcp_firestore might not be started)
                            case whereis(gcp_firestore) of
                                undefined ->
                                    {error, gcp_firestore_not_started};
                                _Pid ->
                                    case catch gcp_firestore:get_document("health", "check") of
                                        {ok, _} -> ok;
                                        {error, not_found} -> ok;  % Health check doc doesn't need to exist
                                        {error, Reason} -> {error, {firestore_unavailable, Reason}};
                                        {'EXIT', Reason} -> {error, {firestore_error, Reason}}
                                    end
                            end;
                        false ->
                            ok
                    end
            end
    end.
