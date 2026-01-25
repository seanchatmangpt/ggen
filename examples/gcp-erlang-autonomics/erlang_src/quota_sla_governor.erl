%%%-------------------------------------------------------------------
%%% @doc Quota SLA Governor - 7-state resource enforcement FSM
%%%
%%% States: normal -> warning -> throttled -> exceeded
%%%         degraded_mode -> remediation -> restored
%%%
%%% Enforces resource quotas and SLAs with automatic throttling and remediation.
%%% @end
%%%-------------------------------------------------------------------
-module(quota_sla_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([check_quota/4, send_warning/3, throttle_traffic/3]).
-export([mark_exceeded/3, degrade_mode/3, start_remediation/3]).
-export([restore_resources/3, get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type quota_id() :: binary().
-type resource_type() :: binary().
-type usage() :: non_neg_integer().
-type limit() :: non_neg_integer().
-type quota_state() :: normal | warning | throttled | exceeded
                     | degraded_mode | remediation | restored.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    quota_id => quota_id(),
    state_from => quota_state(),
    state_to => quota_state(),
    usage => usage(),
    limit => limit(),
    reason => binary(),
    metadata => map()
}.

-record(quota_data, {
    tenant_id :: tenant_id(),
    quota_id :: quota_id(),
    resource_type :: resource_type(),
    usage :: usage(),
    limit :: limit(),
    receipt_table :: atom(),
    created_at :: timestamp(),
    warning_threshold :: float(),
    throttle_threshold :: float(),
    last_reset :: timestamp()
}).

-type data() :: #quota_data{}.

%% Type specs
-spec start_link(tenant_id(), quota_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec check_quota(pid(), resource_type(), usage(), limit()) ->
    {ok, quota_state()} | {error, atom()}.
-spec send_warning(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec throttle_traffic(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec mark_exceeded(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec degrade_mode(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec start_remediation(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec restore_resources(pid(), binary(), map()) ->
    {ok, quota_state()} | {error, atom()}.
-spec get_state(pid(), quota_id()) ->
    {ok, quota_state()} | {error, atom()}.
-spec list_receipts(pid(), quota_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, QuotaId) ->
    start_link(TenantId, QuotaId, QuotaId).

start_link(TenantId, QuotaId, _Opts) ->
    gen_statem:start_link(
        {local, quota_name(TenantId, QuotaId)},
        ?MODULE,
        {TenantId, QuotaId},
        []
    ).

check_quota(Pid, ResourceType, Usage, Limit) ->
    gen_statem:call(Pid, {check_quota, ResourceType, Usage, Limit}).

send_warning(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {send_warning, Reason, Metadata}).

throttle_traffic(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {throttle_traffic, Reason, Metadata}).

mark_exceeded(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {mark_exceeded, Reason, Metadata}).

degrade_mode(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {degrade_mode, Reason, Metadata}).

start_remediation(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {start_remediation, Reason, Metadata}).

restore_resources(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {restore_resources, Reason, Metadata}).

get_state(Pid, QuotaId) ->
    gen_statem:call(Pid, {get_state, QuotaId}).

list_receipts(Pid, QuotaId) ->
    gen_statem:call(Pid, {list_receipts, QuotaId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, QuotaId}) ->
    ReceiptTable = quota_receipt_table(TenantId, QuotaId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #quota_data{
        tenant_id = TenantId,
        quota_id = QuotaId,
        resource_type = undefined,
        usage = 0,
        limit = 1000,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        warning_threshold = 0.75,
        throttle_threshold = 0.90,
        last_reset = timestamp()
    },

    emit_receipt(Data, normal, normal, <<"initialization">>, 0, 0, #{}),
    {ok, normal, Data}.

callback_mode() ->
    handle_event_function.

%% Normal -> Warning (75% threshold)
handle_event({call, From}, {check_quota, ResourceType, Usage, Limit}, normal, Data) ->
    Ratio = Usage / Limit,
    NewData = Data#quota_data{resource_type = ResourceType, usage = Usage, limit = Limit},
    if
        Ratio >= Data#quota_data.warning_threshold ->
            emit_receipt(NewData, normal, warning, <<"warning_threshold_reached">>, Usage, Limit, #{}),
            {next_state, warning, NewData, [{reply, From, {ok, warning}}]};
        true ->
            {keep_state, NewData, [{reply, From, {ok, normal}}]}
    end;

%% Warning -> Throttled (90% threshold)
handle_event({call, From}, {throttle_traffic, Reason, Metadata}, warning, Data) ->
    emit_receipt(Data, warning, throttled, Reason, Data#quota_data.usage, Data#quota_data.limit, Metadata),
    {next_state, throttled, Data, [{reply, From, {ok, throttled}}]};

%% Throttled -> Exceeded (100% exceeded)
handle_event({call, From}, {mark_exceeded, Reason, Metadata}, throttled, Data) ->
    emit_receipt(Data, throttled, exceeded, Reason, Data#quota_data.usage, Data#quota_data.limit, Metadata),
    {next_state, exceeded, Data, [{reply, From, {ok, exceeded}}]};

%% Exceeded -> Degraded Mode (service degradation)
handle_event({call, From}, {degrade_mode, Reason, Metadata}, exceeded, Data) ->
    emit_receipt(Data, exceeded, degraded_mode, Reason, Data#quota_data.usage, Data#quota_data.limit, Metadata),
    {next_state, degraded_mode, Data, [{reply, From, {ok, degraded_mode}}]};

%% Degraded Mode -> Remediation
handle_event({call, From}, {start_remediation, Reason, Metadata}, degraded_mode, Data) ->
    emit_receipt(Data, degraded_mode, remediation, Reason, Data#quota_data.usage, Data#quota_data.limit, Metadata),
    {next_state, remediation, Data, [{reply, From, {ok, remediation}}]};

%% Remediation -> Restored (resources freed)
handle_event({call, From}, {restore_resources, Reason, Metadata}, remediation, Data) ->
    NewData = Data#quota_data{usage = 0, last_reset = timestamp()},
    emit_receipt(NewData, remediation, restored, Reason, 0, Data#quota_data.limit, Metadata),
    {next_state, restored, NewData, [{reply, From, {ok, restored}}]};

%% Restored -> Normal
handle_event({call, From}, {check_quota, _ResourceType, Usage, Limit}, restored, Data) ->
    NewData = Data#quota_data{usage = Usage, limit = Limit},
    emit_receipt(NewData, restored, normal, <<"quota_reset">>, Usage, Limit, #{}),
    {next_state, normal, NewData, [{reply, From, {ok, normal}}]};

%% Throttled -> Warning (reduction)
handle_event({call, From}, {send_warning, Reason, Metadata}, throttled, Data) ->
    emit_receipt(Data, throttled, warning, Reason, Data#quota_data.usage, Data#quota_data.limit, Metadata),
    {next_state, warning, Data, [{reply, From, {ok, warning}}]};

handle_event({call, From}, {get_state, _QuotaId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _QuotaId}, _State, Data) ->
    ReceiptTable = Data#quota_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#quota_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Usage, Limit, Metadata) ->
    ReceiptTable = Data#quota_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#quota_data.tenant_id,
        quota_id => Data#quota_data.quota_id,
        state_from => FromState,
        state_to => ToState,
        usage => Usage,
        limit => Limit,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

format_receipt(Receipt) -> Receipt.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).

quota_name(TenantId, QuotaId) ->
    binary_to_atom(
        <<TenantId/binary, "_", QuotaId/binary, "_quota">>,
        utf8
    ).

quota_receipt_table(TenantId, QuotaId) ->
    binary_to_atom(
        <<TenantId/binary, "_", QuotaId/binary, "_quota_receipts">>,
        utf8
    ).
