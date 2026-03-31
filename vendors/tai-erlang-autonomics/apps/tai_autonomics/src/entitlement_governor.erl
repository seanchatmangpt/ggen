%%%-------------------------------------------------------------------
%%% @doc Entitlement Governor - 8-state FSM for managing feature entitlements
%%%
%%% States: unentitled -> pending_review -> entitled -> revoked
%%%         unentitled -> suspended -> escalated -> archived -> expired
%%%
%%% Uses gen_statem for deterministic state management with immutable receipts.
%%% ETS provides multi-tenant isolation and thread-safe state storage.
%%% @end
%%%-------------------------------------------------------------------
-module(entitlement_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([grant_entitlement/3, revoke_entitlement/3, suspend_entitlement/3]).
-export([escalate_issue/3, archive_entitlement/3, get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Internal
-export([init_receipt_table/1]).

%% Type definitions
-type tenant_id() :: binary().
-type entitlement_id() :: binary().
-type customer_id() :: binary().
-type feature_key() :: binary().
-type entitlement_state() :: unentitled | pending_review | entitled | revoked
                           | suspended | escalated | archived | expired.
-type transition_reason() :: binary().
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    entitlement_id => entitlement_id(),
    state_from => entitlement_state(),
    state_to => entitlement_state(),
    reason => transition_reason(),
    metadata => map()
}.

-record(entitlement_data, {
    tenant_id :: tenant_id(),
    entitlement_id :: entitlement_id(),
    customer_id :: customer_id(),
    feature_key :: feature_key(),
    created_at :: timestamp(),
    expires_at :: timestamp() | infinity,
    receipt_table :: atom(),
    last_transition :: timestamp()
}).

-type data() :: #entitlement_data{}.

%% Type specs
-spec start_link(tenant_id(), entitlement_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec start_link(binary(), entitlement_id(), entitlement_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec grant_entitlement(pid(), customer_id(), map()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec revoke_entitlement(pid(), transition_reason(), map()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec suspend_entitlement(pid(), transition_reason(), map()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec escalate_issue(pid(), transition_reason(), map()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec archive_entitlement(pid(), transition_reason(), map()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec get_state(pid(), entitlement_id()) ->
    {ok, entitlement_state()} | {error, atom()}.
-spec list_receipts(pid(), entitlement_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, EntitlementId) ->
    start_link(TenantId, EntitlementId, EntitlementId).

start_link(TenantId, EntitlementId, _Opts) ->
    gen_statem:start_link(
        {local, entitlement_name(TenantId, EntitlementId)},
        ?MODULE,
        {TenantId, EntitlementId},
        []
    ).

grant_entitlement(Pid, CustomerId, Metadata) ->
    gen_statem:call(Pid, {grant_entitlement, CustomerId, Metadata}).

revoke_entitlement(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {revoke_entitlement, Reason, Metadata}).

suspend_entitlement(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {suspend_entitlement, Reason, Metadata}).

escalate_issue(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {escalate_issue, Reason, Metadata}).

archive_entitlement(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {archive_entitlement, Reason, Metadata}).

get_state(Pid, EntitlementId) ->
    gen_statem:call(Pid, {get_state, EntitlementId}).

list_receipts(Pid, EntitlementId) ->
    gen_statem:call(Pid, {list_receipts, EntitlementId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, EntitlementId}) ->
    ReceiptTable = entitlement_receipt_table(TenantId, EntitlementId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #entitlement_data{
        tenant_id = TenantId,
        entitlement_id = EntitlementId,
        customer_id = undefined,
        feature_key = undefined,
        created_at = timestamp(),
        expires_at = infinity,
        receipt_table = ReceiptTable,
        last_transition = timestamp()
    },

    emit_receipt(Data, unentitled, unentitled, <<"initialization">>, #{}),
    {ok, unentitled, Data}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, {grant_entitlement, CustomerId, Metadata}, unentitled, Data) ->
    NewData = Data#entitlement_data{customer_id = CustomerId},
    emit_receipt(NewData, unentitled, pending_review, <<"grant_requested">>, Metadata),
    {next_state, pending_review, NewData, [{reply, From, {ok, pending_review}}]};

handle_event({call, From}, {revoke_entitlement, Reason, Metadata}, entitled, Data) ->
    emit_receipt(Data, entitled, revoked, Reason, Metadata),
    {next_state, revoked, Data, [{reply, From, {ok, revoked}}]};

handle_event({call, From}, {suspend_entitlement, Reason, Metadata}, entitled, Data) ->
    emit_receipt(Data, entitled, suspended, Reason, Metadata),
    {next_state, suspended, Data, [{reply, From, {ok, suspended}}]};

handle_event({call, From}, {suspend_entitlement, Reason, Metadata}, pending_review, Data) ->
    emit_receipt(Data, pending_review, suspended, Reason, Metadata),
    {next_state, suspended, Data, [{reply, From, {ok, suspended}}]};

handle_event({call, From}, {escalate_issue, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, escalated, Reason, Metadata),
    {next_state, escalated, Data, [{reply, From, {ok, escalated}}]};

handle_event({call, From}, {archive_entitlement, Reason, Metadata}, escalated, Data) ->
    emit_receipt(Data, escalated, archived, Reason, Metadata),
    {next_state, archived, Data, [{reply, From, {ok, archived}}]};

handle_event({call, From}, {archive_entitlement, Reason, Metadata}, revoked, Data) ->
    emit_receipt(Data, revoked, archived, Reason, Metadata),
    {next_state, archived, Data, [{reply, From, {ok, archived}}]};

handle_event({call, From}, {get_state, _EntitlementId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, EntitlementId}, _State, Data) ->
    ReceiptTable = Data#entitlement_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(timeout, _EventContent, State, Data) ->
    if State =:= archived ->
        emit_receipt(Data, archived, expired, <<"expiration">>, #{}),
        {next_state, expired, Data};
    true ->
        {keep_state_and_data}
    end;

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#entitlement_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#entitlement_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#entitlement_data.tenant_id,
        entitlement_id => Data#entitlement_data.entitlement_id,
        state_from => FromState,
        state_to => ToState,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

format_receipt(Receipt) -> Receipt.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).

entitlement_name(TenantId, EntitlementId) ->
    binary_to_atom(
        <<TenantId/binary, "_", EntitlementId/binary, "_entitlement">>,
        utf8
    ).

entitlement_receipt_table(TenantId, EntitlementId) ->
    binary_to_atom(
        <<TenantId/binary, "_", EntitlementId/binary, "_receipts">>,
        utf8
    ).

init_receipt_table(TenantId) ->
    ets:new(
        list_to_atom("entitlement_receipts_" ++ binary_to_list(TenantId)),
        [public, named_table, {write_concurrency, true}]
    ).
