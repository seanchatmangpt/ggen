%%%-------------------------------------------------------------------
%%% @doc Product Catalog Governor - 7-state SKU lifecycle management
%%%
%%% States: draft -> pending_approval -> active -> deprecated
%%%         active -> delisted -> restore_pending -> active
%%%
%%% Manages product SKU lifecycle with approval workflows and deprecation strategies.
%%% @end
%%%-------------------------------------------------------------------
-module(product_catalog_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([submit_for_approval/3, approve_product/3, activate_product/3]).
-export([deprecate_product/3, delist_product/3, request_restore/3]).
-export([get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type product_id() :: binary().
-type sku() :: binary().
-type product_state() :: draft | pending_approval | active | deprecated
                       | delisted | restore_pending.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    product_id => product_id(),
    sku => sku(),
    state_from => product_state(),
    state_to => product_state(),
    reason => binary(),
    metadata => map()
}.

-record(product_data, {
    tenant_id :: tenant_id(),
    product_id :: product_id(),
    sku :: sku() | undefined,
    name :: binary() | undefined,
    receipt_table :: atom(),
    created_at :: timestamp(),
    approved_at :: timestamp() | undefined,
    deprecated_at :: timestamp() | undefined
}).

-type data() :: #product_data{}.

%% Type specs
-spec start_link(tenant_id(), product_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec submit_for_approval(pid(), sku(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec approve_product(pid(), binary(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec activate_product(pid(), binary(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec deprecate_product(pid(), binary(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec delist_product(pid(), binary(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec request_restore(pid(), binary(), map()) ->
    {ok, product_state()} | {error, atom()}.
-spec get_state(pid(), product_id()) ->
    {ok, product_state()} | {error, atom()}.
-spec list_receipts(pid(), product_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, ProductId) ->
    start_link(TenantId, ProductId, ProductId).

start_link(TenantId, ProductId, _Opts) ->
    gen_statem:start_link(
        {local, product_name(TenantId, ProductId)},
        ?MODULE,
        {TenantId, ProductId},
        []
    ).

submit_for_approval(Pid, Sku, Metadata) ->
    gen_statem:call(Pid, {submit_for_approval, Sku, Metadata}).

approve_product(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {approve_product, Reason, Metadata}).

activate_product(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {activate_product, Reason, Metadata}).

deprecate_product(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {deprecate_product, Reason, Metadata}).

delist_product(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {delist_product, Reason, Metadata}).

request_restore(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {request_restore, Reason, Metadata}).

get_state(Pid, ProductId) ->
    gen_statem:call(Pid, {get_state, ProductId}).

list_receipts(Pid, ProductId) ->
    gen_statem:call(Pid, {list_receipts, ProductId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, ProductId}) ->
    ReceiptTable = product_receipt_table(TenantId, ProductId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #product_data{
        tenant_id = TenantId,
        product_id = ProductId,
        sku = undefined,
        name = undefined,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        approved_at = undefined,
        deprecated_at = undefined
    },

    emit_receipt(Data, draft, draft, <<"initialization">>, #{}),
    {ok, draft, Data}.

callback_mode() ->
    handle_event_function.

%% Draft -> Pending Approval
handle_event({call, From}, {submit_for_approval, Sku, Metadata}, draft, Data) ->
    NewData = Data#product_data{sku = Sku},
    emit_receipt(NewData, draft, pending_approval, <<"submitted_for_approval">>, Metadata),
    {next_state, pending_approval, NewData, [{reply, From, {ok, pending_approval}}]};

%% Pending Approval -> Active
handle_event({call, From}, {approve_product, _Reason, Metadata}, pending_approval, Data) ->
    NewData = Data#product_data{approved_at = timestamp()},
    emit_receipt(NewData, pending_approval, active, <<"approved">>, Metadata),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

%% Active -> Deprecated
handle_event({call, From}, {deprecate_product, Reason, Metadata}, active, Data) ->
    NewData = Data#product_data{deprecated_at = timestamp()},
    emit_receipt(NewData, active, deprecated, Reason, Metadata),
    {next_state, deprecated, NewData, [{reply, From, {ok, deprecated}}]};

%% Active -> Delisted (via deprecation)
handle_event({call, From}, {delist_product, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, delisted, Reason, Metadata),
    {next_state, delisted, Data, [{reply, From, {ok, delisted}}]};

%% Deprecated -> Delisted
handle_event({call, From}, {delist_product, Reason, Metadata}, deprecated, Data) ->
    emit_receipt(Data, deprecated, delisted, Reason, Metadata),
    {next_state, delisted, Data, [{reply, From, {ok, delisted}}]};

%% Delisted -> Restore Pending
handle_event({call, From}, {request_restore, Reason, Metadata}, delisted, Data) ->
    emit_receipt(Data, delisted, restore_pending, Reason, Metadata),
    {next_state, restore_pending, Data, [{reply, From, {ok, restore_pending}}]};

%% Restore Pending -> Active
handle_event({call, From}, {activate_product, Reason, Metadata}, restore_pending, Data) ->
    emit_receipt(Data, restore_pending, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Deprecated -> Restore Pending
handle_event({call, From}, {request_restore, Reason, Metadata}, deprecated, Data) ->
    emit_receipt(Data, deprecated, restore_pending, Reason, Metadata),
    {next_state, restore_pending, Data, [{reply, From, {ok, restore_pending}}]};

handle_event({call, From}, {get_state, _ProductId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _ProductId}, _State, Data) ->
    ReceiptTable = Data#product_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#product_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#product_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#product_data.tenant_id,
        product_id => Data#product_data.product_id,
        sku => Data#product_data.sku,
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

product_name(TenantId, ProductId) ->
    binary_to_atom(
        <<TenantId/binary, "_", ProductId/binary, "_product">>,
        utf8
    ).

product_receipt_table(TenantId, ProductId) ->
    binary_to_atom(
        <<TenantId/binary, "_", ProductId/binary, "_product_receipts">>,
        utf8
    ).
