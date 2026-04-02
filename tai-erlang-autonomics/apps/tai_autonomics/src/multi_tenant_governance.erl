%%%-------------------------------------------------------------------
%%% @doc Multi-Tenant Governance - 6-state tenant lifecycle FSM
%%%
%%% States: provisioning -> active -> suspended -> degraded -> maintenance
%%%         deprovisioned
%%%
%%% Manages multi-tenant lifecycle with automatic isolation, suspension, and deprovisioning.
%%% Uses ETS for tenant-level isolation and state partitioning.
%%% @end
%%%-------------------------------------------------------------------
-module(multi_tenant_governance).
-behaviour(gen_statem).

%% API
-export([start_link/1, start_link/2]).
-export([provision_tenant/3, activate_tenant/3, suspend_tenant/3]).
-export([mark_degraded/3, schedule_maintenance/3, deprovision_tenant/3]).
-export([get_tenant_state/2, list_all_receipts/1, create_isolated_partition/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type tenant_state() :: provisioning | active | suspended | degraded | maintenance | deprovisioned.
-type partition_name() :: atom().
-type resource_quota() :: #{
    storage => non_neg_integer(),
    compute => non_neg_integer(),
    bandwidth => non_neg_integer()
}.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    state_from => tenant_state(),
    state_to => tenant_state(),
    reason => binary(),
    metadata => map()
}.

-record(tenant_governance, {
    tenant_id :: tenant_id(),
    partition_name :: partition_name(),
    resource_quota :: resource_quota(),
    receipt_table :: atom(),
    created_at :: timestamp(),
    activated_at :: timestamp() | undefined,
    suspended_at :: timestamp() | undefined,
    maintenance_window :: {timestamp(), timestamp()} | undefined
}).

-type data() :: #tenant_governance{}.

%% Type specs
-spec start_link(tenant_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec start_link(atom(), tenant_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec provision_tenant(pid(), resource_quota(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec activate_tenant(pid(), binary(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec suspend_tenant(pid(), binary(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec mark_degraded(pid(), binary(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec schedule_maintenance(pid(), binary(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec deprovision_tenant(pid(), binary(), map()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec get_tenant_state(pid(), tenant_id()) ->
    {ok, tenant_state()} | {error, atom()}.
-spec list_all_receipts(pid()) ->
    {ok, [receipt()]} | {error, atom()}.
-spec create_isolated_partition(tenant_id(), resource_quota()) ->
    {ok, partition_name()} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId) ->
    start_link(multi_tenant_gov_pool, TenantId).

start_link(Pool, TenantId) ->
    gen_statem:start_link(
        {local, tenant_name(TenantId)},
        ?MODULE,
        {Pool, TenantId},
        []
    ).

provision_tenant(Pid, ResourceQuota, Metadata) ->
    gen_statem:call(Pid, {provision_tenant, ResourceQuota, Metadata}).

activate_tenant(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {activate_tenant, Reason, Metadata}).

suspend_tenant(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {suspend_tenant, Reason, Metadata}).

mark_degraded(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {mark_degraded, Reason, Metadata}).

schedule_maintenance(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {schedule_maintenance, Reason, Metadata}).

deprovision_tenant(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {deprovision_tenant, Reason, Metadata}).

get_tenant_state(Pid, TenantId) ->
    gen_statem:call(Pid, {get_tenant_state, TenantId}).

list_all_receipts(Pid) ->
    gen_statem:call(Pid, {list_all_receipts}).

create_isolated_partition(TenantId, ResourceQuota) ->
    PartitionName = list_to_atom(
        "tenant_partition_" ++ binary_to_list(TenantId)
    ),
    try
        ets:new(PartitionName, [
            public,
            named_table,
            {write_concurrency, true},
            {read_concurrency, true}
        ]),
        ets:insert(PartitionName, {resource_quota, ResourceQuota}),
        {ok, PartitionName}
    catch
        error:badarg ->
            {error, partition_already_exists}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({_Pool, TenantId}) ->
    ReceiptTable = tenant_receipt_table(TenantId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    PartitionName = list_to_atom(
        "tenant_data_" ++ binary_to_list(TenantId)
    ),

    Data = #tenant_governance{
        tenant_id = TenantId,
        partition_name = PartitionName,
        resource_quota = #{
            storage => 1000000,
            compute => 100,
            bandwidth => 1000
        },
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        activated_at = undefined,
        suspended_at = undefined,
        maintenance_window = undefined
    },

    emit_receipt(Data, provisioning, provisioning, <<"initialization">>, #{}),
    {ok, provisioning, Data}.

callback_mode() ->
    handle_event_function.

%% Provisioning -> Active
handle_event({call, From}, {provision_tenant, ResourceQuota, Metadata}, provisioning, Data) ->
    NewData = Data#tenant_governance{resource_quota = ResourceQuota},
    emit_receipt(NewData, provisioning, active, <<"provisioning_complete">>, Metadata),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

%% Active -> Suspended
handle_event({call, From}, {suspend_tenant, Reason, Metadata}, active, Data) ->
    NewData = Data#tenant_governance{suspended_at = timestamp()},
    emit_receipt(NewData, active, suspended, Reason, Metadata),
    {next_state, suspended, NewData, [{reply, From, {ok, suspended}}]};

%% Active -> Degraded
handle_event({call, From}, {mark_degraded, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, degraded, Reason, Metadata),
    {next_state, degraded, Data, [{reply, From, {ok, degraded}}]};

%% Suspended -> Active (resume)
handle_event({call, From}, {activate_tenant, Reason, Metadata}, suspended, Data) ->
    emit_receipt(Data, suspended, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Degraded -> Active (recovery)
handle_event({call, From}, {activate_tenant, Reason, Metadata}, degraded, Data) ->
    emit_receipt(Data, degraded, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Active -> Maintenance
handle_event({call, From}, {schedule_maintenance, Reason, Metadata}, active, Data) ->
    MaintenanceWindow = extract_maintenance_window(Metadata),
    NewData = Data#tenant_governance{maintenance_window = MaintenanceWindow},
    emit_receipt(NewData, active, maintenance, Reason, Metadata),
    {next_state, maintenance, NewData, [{reply, From, {ok, maintenance}}]};

%% Degraded -> Maintenance
handle_event({call, From}, {schedule_maintenance, Reason, Metadata}, degraded, Data) ->
    MaintenanceWindow = extract_maintenance_window(Metadata),
    NewData = Data#tenant_governance{maintenance_window = MaintenanceWindow},
    emit_receipt(NewData, degraded, maintenance, Reason, Metadata),
    {next_state, maintenance, NewData, [{reply, From, {ok, maintenance}}]};

%% Maintenance -> Active (maintenance complete)
handle_event({call, From}, {activate_tenant, Reason, Metadata}, maintenance, Data) ->
    emit_receipt(Data, maintenance, active, Reason, Metadata),
    {next_state, active, Data, [{reply, From, {ok, active}}]};

%% Any State -> Deprovisioned (cleanup)
handle_event({call, From}, {deprovision_tenant, Reason, Metadata}, State, Data)
  when State =:= suspended orelse State =:= degraded orelse State =:= maintenance ->
    emit_receipt(Data, State, deprovisioned, Reason, Metadata),
    {next_state, deprovisioned, Data, [{reply, From, {ok, deprovisioned}}]};

%% Active -> Deprovisioned (direct)
handle_event({call, From}, {deprovision_tenant, Reason, Metadata}, active, Data) ->
    emit_receipt(Data, active, deprovisioned, Reason, Metadata),
    {next_state, deprovisioned, Data, [{reply, From, {ok, deprovisioned}}]};

handle_event({call, From}, {activate_tenant, Reason, Metadata}, provisioning, Data) ->
    NewData = Data#tenant_governance{activated_at = timestamp()},
    emit_receipt(NewData, provisioning, active, Reason, Metadata),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

handle_event({call, From}, {get_tenant_state, _TenantId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_all_receipts}, _State, Data) ->
    ReceiptTable = Data#tenant_governance.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#tenant_governance.receipt_table,
    ets:delete(ReceiptTable),
    PartitionName = Data#tenant_governance.partition_name,
    catch ets:delete(PartitionName).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#tenant_governance.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#tenant_governance.tenant_id,
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

extract_maintenance_window(Metadata) ->
    case maps:get(maintenance_window, Metadata, undefined) of
        {Start, End} -> {Start, End};
        _ -> {timestamp(), timestamp() + 3600000}  % Default: 1 hour
    end.

tenant_name(TenantId) ->
    binary_to_atom(
        <<TenantId/binary, "_tenant_gov">>,
        utf8
    ).

tenant_receipt_table(TenantId) ->
    binary_to_atom(
        <<TenantId/binary, "_tenant_receipts">>,
        utf8
    ).
