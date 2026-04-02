%%%-------------------------------------------------------------------
%%% @doc Subscription Governor - 8-state subscription lifecycle FSM
%%%
%%% States: free_trial -> active -> suspended -> paused
%%%         active -> cancellation_requested -> cancellation_confirmed -> terminated
%%%         paused -> archived
%%%
%%% Manages complete subscription lifecycle with trial periods, suspensions, and cancellations.
%%% @end
%%%-------------------------------------------------------------------
-module(subscription_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([start_trial/3, activate_subscription/3, suspend_subscription/3]).
-export([pause_subscription/3, resume_subscription/3, request_cancellation/3]).
-export([confirm_cancellation/3, archive_subscription/3]).
-export([get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type subscription_id() :: binary().
-type plan_id() :: binary().
-type subscription_state() :: free_trial | active | suspended | paused
                            | cancellation_requested | cancellation_confirmed
                            | terminated | archived.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    subscription_id => subscription_id(),
    state_from => subscription_state(),
    state_to => subscription_state(),
    reason => binary(),
    metadata => map()
}.

-record(subscription_data, {
    tenant_id :: tenant_id(),
    subscription_id :: subscription_id(),
    plan_id :: plan_id() | undefined,
    customer_id :: binary() | undefined,
    receipt_table :: atom(),
    created_at :: timestamp(),
    activated_at :: timestamp() | undefined,
    cancelled_at :: timestamp() | undefined,
    trial_ends_at :: timestamp() | undefined
}).

-type data() :: #subscription_data{}.

%% Type specs
-spec start_link(tenant_id(), subscription_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec start_trial(pid(), plan_id(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec activate_subscription(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec suspend_subscription(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec pause_subscription(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec resume_subscription(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec request_cancellation(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec confirm_cancellation(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec archive_subscription(pid(), binary(), map()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec get_state(pid(), subscription_id()) ->
    {ok, subscription_state()} | {error, atom()}.
-spec list_receipts(pid(), subscription_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, SubscriptionId) ->
    start_link(TenantId, SubscriptionId, SubscriptionId).

start_link(TenantId, SubscriptionId, _Opts) ->
    gen_statem:start_link(
        {local, subscription_name(TenantId, SubscriptionId)},
        ?MODULE,
        {TenantId, SubscriptionId},
        []
    ).

start_trial(Pid, PlanId, Metadata) ->
    gen_statem:call(Pid, {start_trial, PlanId, Metadata}).

activate_subscription(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {activate_subscription, Reason, Metadata}).

suspend_subscription(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {suspend_subscription, Reason, Metadata}).

pause_subscription(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {pause_subscription, Reason, Metadata}).

resume_subscription(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {resume_subscription, Reason, Metadata}).

request_cancellation(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {request_cancellation, Reason, Metadata}).

confirm_cancellation(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {confirm_cancellation, Reason, Metadata}).

archive_subscription(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {archive_subscription, Reason, Metadata}).

get_state(Pid, SubscriptionId) ->
    gen_statem:call(Pid, {get_state, SubscriptionId}).

list_receipts(Pid, SubscriptionId) ->
    gen_statem:call(Pid, {list_receipts, SubscriptionId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, SubscriptionId}) ->
    ReceiptTable = subscription_receipt_table(TenantId, SubscriptionId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #subscription_data{
        tenant_id = TenantId,
        subscription_id = SubscriptionId,
        plan_id = undefined,
        customer_id = undefined,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        activated_at = undefined,
        cancelled_at = undefined,
        trial_ends_at = undefined
    },

    emit_receipt(Data, free_trial, free_trial, <<"initialization">>, #{}),
    {ok, free_trial, Data}.

callback_mode() ->
    handle_event_function.

%% Free Trial -> Active
handle_event({call, From}, {start_trial, PlanId, Metadata}, free_trial, Data) ->
    NewData = Data#subscription_data{plan_id = PlanId, trial_ends_at = timestamp() + 14 * 24 * 60 * 60 * 1000},
    emit_receipt(NewData, free_trial, free_trial, <<"trial_started">>, Metadata),
    {keep_state, NewData, [{reply, From, {ok, free_trial}}]};

%% Free Trial -> Active (trial ends)
handle_event({call, From}, {activate_subscription, Reason, Metadata}, free_trial, Data) ->
    NewData = Data#subscription_data{activated_at = timestamp()},
    emit_receipt(NewData, free_trial, active, Reason, Metadata),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

%% Active -> Suspended
handle_event({call, From}, {suspend_subscription, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, suspended, Reason, Metadata),
    {next_state, suspended, Data, [{reply, From, {ok, suspended}}]};

%% Active -> Paused
handle_event({call, From}, {pause_subscription, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, paused, Reason, Metadata),
    {next_state, paused, Data, [{reply, From, {ok, paused}}]};

%% Suspended -> Active (resume)
handle_event({call, From}, {resume_subscription, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Paused -> Active (resume)
handle_event({call, From}, {resume_subscription, Reason, Metadata}, paused, Data) ->
    emit_receipt(Data, paused, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Active -> Cancellation Requested
handle_event({call, From}, {request_cancellation, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, cancellation_requested, Reason, Metadata),
    {next_state, cancellation_requested, Data, [{reply, From, {ok, cancellation_requested}}]};

%% Suspended -> Cancellation Requested
handle_event({call, From}, {request_cancellation, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, cancellation_requested, Reason, Metadata),
    {next_state, cancellation_requested, Data, [{reply, From, {ok, cancellation_requested}}]};

%% Cancellation Requested -> Confirmed
handle_event({call, From}, {confirm_cancellation, _Reason, Metadata}, cancellation_requested, Data) ->
    NewData = Data#subscription_data{cancelled_at = timestamp()},
    emit_receipt(NewData, cancellation_requested, cancellation_confirmed, <<"cancellation_confirmed">>, Metadata),
    {next_state, cancellation_confirmed, NewData, [{reply, From, {ok, cancellation_confirmed}}]};

%% Cancellation Confirmed -> Terminated
handle_event({call, From}, {archive_subscription, Reason, Metadata}, cancellation_confirmed, Data) ->
    emit_receipt(Data, cancellation_confirmed, terminated, Reason, Metadata),
    {next_state, terminated, Data, [{reply, From, {ok, terminated}}]};

%% Paused -> Archived
handle_event({call, From}, {archive_subscription, Reason, Metadata}, paused, Data) ->
    emit_receipt(Data, paused, archived, Reason, Metadata),
    {next_state, archived, Data, [{reply, From, {ok, archived}}]};

handle_event({call, From}, {get_state, _SubscriptionId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _SubscriptionId}, _State, Data) ->
    ReceiptTable = Data#subscription_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#subscription_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#subscription_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#subscription_data.tenant_id,
        subscription_id => Data#subscription_data.subscription_id,
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

subscription_name(TenantId, SubscriptionId) ->
    binary_to_atom(
        <<TenantId/binary, "_", SubscriptionId/binary, "_subscription">>,
        utf8
    ).

subscription_receipt_table(TenantId, SubscriptionId) ->
    binary_to_atom(
        <<TenantId/binary, "_", SubscriptionId/binary, "_subscription_receipts">>,
        utf8
    ).
