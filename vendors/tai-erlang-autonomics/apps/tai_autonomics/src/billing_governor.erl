%%%-------------------------------------------------------------------
%%% @doc Billing Governor - 11-state payment FSM for marketplace transactions
%%%
%%% States: uninitialized -> payment_pending -> payment_authorized -> payment_processing
%%%         payment_settled | payment_failed -> refund_pending -> refund_processing
%%%         refund_settled | payment_disputed -> terminated
%%%
%%% Manages complete payment lifecycle with deterministic receipts and invariants.
%%% @end
%%%-------------------------------------------------------------------
-module(billing_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([request_payment/4, authorize_payment/3, process_payment/3]).
-export([settle_payment/3, fail_payment/3, request_refund/3]).
-export([process_refund/3, settle_refund/3, dispute_payment/3]).
-export([get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type billing_id() :: binary().
-type transaction_id() :: binary().
-type amount() :: float().
-type currency() :: binary().
-type payment_method() :: binary().
-type billing_state() :: uninitialized | payment_pending | payment_authorized
                      | payment_processing | payment_settled | payment_failed
                      | refund_pending | refund_processing | refund_settled
                      | payment_disputed | terminated.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    billing_id => billing_id(),
    state_from => billing_state(),
    state_to => billing_state(),
    amount => amount(),
    currency => currency(),
    reason => binary(),
    metadata => map()
}.

-record(billing_data, {
    tenant_id :: tenant_id(),
    billing_id :: billing_id(),
    transaction_id :: transaction_id() | undefined,
    amount :: amount(),
    currency :: currency(),
    payment_method :: payment_method() | undefined,
    receipt_table :: atom(),
    created_at :: timestamp(),
    settled_at :: timestamp() | undefined
}).

-type data() :: #billing_data{}.

%% Type specs
-spec start_link(tenant_id(), billing_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec request_payment(pid(), amount(), currency(), payment_method()) ->
    {ok, billing_state()} | {error, atom()}.
-spec authorize_payment(pid(), transaction_id(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec process_payment(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec settle_payment(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec fail_payment(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec request_refund(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec process_refund(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec settle_refund(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec dispute_payment(pid(), binary(), map()) ->
    {ok, billing_state()} | {error, atom()}.
-spec get_state(pid(), billing_id()) ->
    {ok, billing_state()} | {error, atom()}.
-spec list_receipts(pid(), billing_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, BillingId) ->
    start_link(TenantId, BillingId, BillingId).

start_link(TenantId, BillingId, _Opts) ->
    gen_statem:start_link(
        {local, billing_name(TenantId, BillingId)},
        ?MODULE,
        {TenantId, BillingId},
        []
    ).

request_payment(Pid, Amount, Currency, PaymentMethod) ->
    gen_statem:call(Pid, {request_payment, Amount, Currency, PaymentMethod}).

authorize_payment(Pid, TransactionId, Metadata) ->
    gen_statem:call(Pid, {authorize_payment, TransactionId, Metadata}).

process_payment(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {process_payment, Reason, Metadata}).

settle_payment(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {settle_payment, Reason, Metadata}).

fail_payment(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {fail_payment, Reason, Metadata}).

request_refund(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {request_refund, Reason, Metadata}).

process_refund(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {process_refund, Reason, Metadata}).

settle_refund(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {settle_refund, Reason, Metadata}).

dispute_payment(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {dispute_payment, Reason, Metadata}).

get_state(Pid, BillingId) ->
    gen_statem:call(Pid, {get_state, BillingId}).

list_receipts(Pid, BillingId) ->
    gen_statem:call(Pid, {list_receipts, BillingId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, BillingId}) ->
    ReceiptTable = billing_receipt_table(TenantId, BillingId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #billing_data{
        tenant_id = TenantId,
        billing_id = BillingId,
        transaction_id = undefined,
        amount = 0.0,
        currency = undefined,
        payment_method = undefined,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        settled_at = undefined
    },

    emit_receipt(Data, uninitialized, uninitialized, <<"initialization">>, 0.0, #{}),
    {ok, uninitialized, Data}.

callback_mode() ->
    handle_event_function.

%% Initialization -> Payment Pending
handle_event({call, From}, {request_payment, Amount, Currency, PaymentMethod}, uninitialized, Data) ->
    NewData = Data#billing_data{
        amount = Amount,
        currency = Currency,
        payment_method = PaymentMethod
    },
    emit_receipt(NewData, uninitialized, payment_pending, <<"payment_requested">>, Amount, #{}),
    {next_state, payment_pending, NewData, [{reply, From, {ok, payment_pending}}]};

%% Payment Pending -> Authorized
handle_event({call, From}, {authorize_payment, TransactionId, Metadata}, payment_pending, Data) ->
    NewData = Data#billing_data{transaction_id = TransactionId},
    emit_receipt(NewData, payment_pending, payment_authorized, <<"payment_authorized">>, Data#billing_data.amount, Metadata),
    {next_state, payment_authorized, NewData, [{reply, From, {ok, payment_authorized}}]};

%% Authorized -> Processing
handle_event({call, From}, {process_payment, _Reason, Metadata}, payment_authorized, Data) ->
    emit_receipt(Data, payment_authorized, payment_processing, <<"processing_started">>, Data#billing_data.amount, Metadata),
    {next_state, payment_processing, Data, [{reply, From, {ok, payment_processing}}]};

%% Processing -> Settled
handle_event({call, From}, {settle_payment, _Reason, Metadata}, payment_processing, Data) ->
    NewData = Data#billing_data{settled_at = timestamp()},
    emit_receipt(NewData, payment_processing, payment_settled, <<"payment_settled">>, Data#billing_data.amount, Metadata),
    {next_state, payment_settled, NewData, [{reply, From, {ok, payment_settled}}]};

%% Processing -> Failed
handle_event({call, From}, {fail_payment, Reason, Metadata}, payment_processing, Data) ->
    emit_receipt(Data, payment_processing, payment_failed, Reason, Data#billing_data.amount, Metadata),
    {next_state, payment_failed, Data, [{reply, From, {ok, payment_failed}}]};

%% Settled -> Refund Pending
handle_event({call, From}, {request_refund, Reason, Metadata}, payment_settled, Data) ->
    emit_receipt(Data, payment_settled, refund_pending, Reason, Data#billing_data.amount, Metadata),
    {next_state, refund_pending, Data, [{reply, From, {ok, refund_pending}}]};

%% Refund Pending -> Refund Processing
handle_event({call, From}, {process_refund, _Reason, Metadata}, refund_pending, Data) ->
    emit_receipt(Data, refund_pending, refund_processing, <<"refund_processing">>, Data#billing_data.amount, Metadata),
    {next_state, refund_processing, Data, [{reply, From, {ok, refund_processing}}]};

%% Refund Processing -> Refund Settled
handle_event({call, From}, {settle_refund, _Reason, Metadata}, refund_processing, Data) ->
    NewData = Data#billing_data{settled_at = timestamp()},
    emit_receipt(NewData, refund_processing, refund_settled, <<"refund_settled">>, Data#billing_data.amount, Metadata),
    {next_state, refund_settled, NewData, [{reply, From, {ok, refund_settled}}]};

%% Payment Settled -> Disputed
handle_event({call, From}, {dispute_payment, Reason, Metadata}, payment_settled, Data) ->
    emit_receipt(Data, payment_settled, payment_disputed, Reason, Data#billing_data.amount, Metadata),
    {next_state, payment_disputed, Data, [{reply, From, {ok, payment_disputed}}]};

%% Terminal transitions from various states
handle_event({call, From}, {fail_payment, Reason, Metadata}, State, Data)
  when State =:= payment_failed orelse State =:= refund_settled orelse State =:= payment_disputed ->
    emit_receipt(Data, State, terminated, Reason, Data#billing_data.amount, Metadata),
    {next_state, terminated, Data, [{reply, From, {ok, terminated}}]};

handle_event({call, From}, {get_state, _BillingId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _BillingId}, _State, Data) ->
    ReceiptTable = Data#billing_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#billing_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Amount, Metadata) ->
    ReceiptTable = Data#billing_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#billing_data.tenant_id,
        billing_id => Data#billing_data.billing_id,
        state_from => FromState,
        state_to => ToState,
        amount => Amount,
        currency => Data#billing_data.currency,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

format_receipt(Receipt) -> Receipt.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).

billing_name(TenantId, BillingId) ->
    binary_to_atom(
        <<TenantId/binary, "_", BillingId/binary, "_billing">>,
        utf8
    ).

billing_receipt_table(TenantId, BillingId) ->
    binary_to_atom(
        <<TenantId/binary, "_", BillingId/binary, "_billing_receipts">>,
        utf8
    ).
