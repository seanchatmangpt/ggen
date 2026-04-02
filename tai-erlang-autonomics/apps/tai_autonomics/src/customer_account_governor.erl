%%%-------------------------------------------------------------------
%%% @doc Customer Account Governor - 6-state account management FSM
%%%
%%% States: new -> verified -> active -> suspended -> closed -> restricted
%%%
%%% Manages customer account lifecycle with verification, suspension, and closure workflows.
%%% @end
%%%-------------------------------------------------------------------
-module(customer_account_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([create_account/3, verify_account/3, activate_account/3]).
-export([suspend_account/3, close_account/3, restrict_account/3]).
-export([get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type account_id() :: binary().
-type email() :: binary().
-type account_state() :: new | verified | active | suspended | closed | restricted.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    account_id => account_id(),
    state_from => account_state(),
    state_to => account_state(),
    reason => binary(),
    metadata => map()
}.

-record(account_data, {
    tenant_id :: tenant_id(),
    account_id :: account_id(),
    email :: email() | undefined,
    verified :: boolean(),
    receipt_table :: atom(),
    created_at :: timestamp(),
    verified_at :: timestamp() | undefined,
    activated_at :: timestamp() | undefined,
    suspended_at :: timestamp() | undefined
}).

-type data() :: #account_data{}.

%% Type specs
-spec start_link(tenant_id(), account_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec create_account(pid(), email(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec verify_account(pid(), binary(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec activate_account(pid(), binary(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec suspend_account(pid(), binary(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec close_account(pid(), binary(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec restrict_account(pid(), binary(), map()) ->
    {ok, account_state()} | {error, atom()}.
-spec get_state(pid(), account_id()) ->
    {ok, account_state()} | {error, atom()}.
-spec list_receipts(pid(), account_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, AccountId) ->
    start_link(TenantId, AccountId, AccountId).

start_link(TenantId, AccountId, _Opts) ->
    gen_statem:start_link(
        {local, account_name(TenantId, AccountId)},
        ?MODULE,
        {TenantId, AccountId},
        []
    ).

create_account(Pid, Email, Metadata) ->
    gen_statem:call(Pid, {create_account, Email, Metadata}).

verify_account(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {verify_account, Reason, Metadata}).

activate_account(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {activate_account, Reason, Metadata}).

suspend_account(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {suspend_account, Reason, Metadata}).

close_account(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {close_account, Reason, Metadata}).

restrict_account(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {restrict_account, Reason, Metadata}).

get_state(Pid, AccountId) ->
    gen_statem:call(Pid, {get_state, AccountId}).

list_receipts(Pid, AccountId) ->
    gen_statem:call(Pid, {list_receipts, AccountId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, AccountId}) ->
    ReceiptTable = account_receipt_table(TenantId, AccountId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #account_data{
        tenant_id = TenantId,
        account_id = AccountId,
        email = undefined,
        verified = false,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        verified_at = undefined,
        activated_at = undefined,
        suspended_at = undefined
    },

    emit_receipt(Data, new, new, <<"initialization">>, #{}),
    {ok, new, Data}.

callback_mode() ->
    handle_event_function.

%% New -> Verified (after email verification)
handle_event({call, From}, {create_account, Email, Metadata}, new, Data) ->
    NewData = Data#account_data{email = Email},
    emit_receipt(NewData, new, verified, <<"account_created">>, Metadata),
    {next_state, verified, NewData, [{reply, From, {ok, verified}}]};

%% Verified -> Active
handle_event({call, From}, {verify_account, _Reason, Metadata}, verified, Data) ->
    NewData = Data#account_data{verified = true, verified_at = timestamp()},
    emit_receipt(NewData, verified, active, <<"email_verified">>, Metadata),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

%% Active -> Suspended
handle_event({call, From}, {suspend_account, Reason, Metadata}, active, Data) ->
    NewData = Data#account_data{suspended_at = timestamp()},
    emit_receipt(NewData, active, suspended, Reason, Metadata),
    {next_state, suspended, NewData, [{reply, From, {ok, suspended}}]};

%% Verified -> Suspended (before full activation)
handle_event({call, From}, {suspend_account, Reason, Metadata}, verified, Data) ->
    emit_receipt(Data, verified, suspended, Reason, Metadata),
    {next_state, suspended, Data, [{reply, From, {ok, suspended}}]};

%% Suspended -> Restricted (escalation)
handle_event({call, From}, {restrict_account, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, restricted, Reason, Metadata),
    {next_state, restricted, Data, [{reply, From, {ok, restricted}}]};

%% Active -> Restricted (direct escalation)
handle_event({call, From}, {restrict_account, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, restricted, Reason, Metadata),
    {next_state, restricted, Data, [{reply, From, {ok, restricted}}]};

%% Suspended -> Closed
handle_event({call, From}, {close_account, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, closed, Reason, Metadata),
    {next_state, closed, Data, [{reply, From, {ok, closed}}]};

%% Active -> Closed (direct closure)
handle_event({call, From}, {close_account, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, closed, Reason, Metadata),
    {next_state, closed, Data, [{reply, From, {ok, closed}}]};

%% Restricted -> Closed
handle_event({call, From}, {close_account, Reason, Metadata}, restricted, Data) ->
    emit_receipt(Data, restricted, closed, Reason, Metadata),
    {next_state, closed, Data, [{reply, From, {ok, closed}}]};

handle_event({call, From}, {get_state, _AccountId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _AccountId}, _State, Data) ->
    ReceiptTable = Data#account_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#account_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#account_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#account_data.tenant_id,
        account_id => Data#account_data.account_id,
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

account_name(TenantId, AccountId) ->
    binary_to_atom(
        <<TenantId/binary, "_", AccountId/binary, "_account">>,
        utf8
    ).

account_receipt_table(TenantId, AccountId) ->
    binary_to_atom(
        <<TenantId/binary, "_", AccountId/binary, "_account_receipts">>,
        utf8
    ).
