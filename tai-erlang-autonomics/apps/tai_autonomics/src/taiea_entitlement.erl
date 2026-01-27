%%%-------------------------------------------------------------------
%%% @doc TAI Entitlement Resolver - Entitlement state tracking and application
%%%
%%% Manages per-tenant entitlements with SKU-based feature/tool/IAM role tracking.
%%% Stores entitlement state in memory (ETS) with deterministic event application.
%%%
%%% Entitlement Model:
%%%   - Each tenant has: tenant_id, sku, enabled_packs, enabled_tools, enabled_iam_roles, expiry
%%%   - SKU levels: base, professional, enterprise
%%%   - Events: sku_changed, pack_enabled, pack_disabled, iam_role_added, iam_role_removed
%%%   - Receipts emitted on each entitlement state change
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_entitlement).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([apply_event/2, get_active_packs/1, get_enabled_tools/1]).
-export([verify_entitlement_active/1, verify_iam_role/2]).
-export([get_entitlement/1, list_all_entitlements/0]).
-export([init_test_data/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type definitions
-type tenant_id() :: binary().
-type sku_type() :: base | professional | enterprise.
-type tool_name() :: atom().
-type iam_role() :: atom().
-type pack_name() :: atom().
-type timestamp() :: non_neg_integer().

-type entitlement_state() :: #{
    tenant_id := tenant_id(),
    sku := sku_type(),
    enabled_packs := [pack_name()],
    enabled_tools := [tool_name()],
    enabled_iam_roles := [iam_role()],
    expiry := timestamp() | infinity,
    created_at := timestamp(),
    updated_at := timestamp()
}.

-type event() ::
    {sku_changed, sku_type()} |
    {pack_enabled, pack_name()} |
    {pack_disabled, pack_name()} |
    {iam_role_added, iam_role()} |
    {iam_role_removed, iam_role()}.

-type receipt() :: #{
    id := binary(),
    timestamp := timestamp(),
    tenant_id := tenant_id(),
    event := event(),
    state_before := entitlement_state(),
    state_after := entitlement_state(),
    reason := binary()
}.

-record(state, {
    entitlements_table :: atom(),
    receipts_table :: atom()
}).

%% Entitlement table: {TenantId} -> EntitlementState
%% Receipts table: {ReceiptId} -> Receipt

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Apply an entitlement event to a tenant
-spec apply_event(tenant_id(), event()) ->
    {ok, entitlement_state()} | {error, term()}.
apply_event(TenantId, Event) ->
    gen_server:call(?MODULE, {apply_event, TenantId, Event}).

%% @doc Get active packs for a tenant
-spec get_active_packs(tenant_id()) -> [pack_name()].
get_active_packs(TenantId) ->
    gen_server:call(?MODULE, {get_active_packs, TenantId}).

%% @doc Get enabled tools for a tenant
-spec get_enabled_tools(tenant_id()) -> [tool_name()].
get_enabled_tools(TenantId) ->
    gen_server:call(?MODULE, {get_enabled_tools, TenantId}).

%% @doc Verify entitlement is active for a tenant
-spec verify_entitlement_active(tenant_id()) -> ok | {error, inactive}.
verify_entitlement_active(TenantId) ->
    gen_server:call(?MODULE, {verify_entitlement_active, TenantId}).

%% @doc Verify a tenant has a specific IAM role enabled
-spec verify_iam_role(tenant_id(), iam_role()) -> ok | {error, not_enabled}.
verify_iam_role(TenantId, RequiredRole) ->
    gen_server:call(?MODULE, {verify_iam_role, TenantId, RequiredRole}).

%% @doc Get full entitlement state for a tenant
-spec get_entitlement(tenant_id()) -> {ok, entitlement_state()} | {error, not_found}.
get_entitlement(TenantId) ->
    gen_server:call(?MODULE, {get_entitlement, TenantId}).

%% @doc List all entitlements
-spec list_all_entitlements() -> [entitlement_state()].
list_all_entitlements() ->
    gen_server:call(?MODULE, list_all_entitlements).

%% @doc Initialize test data
-spec init_test_data() -> ok.
init_test_data() ->
    gen_server:cast(?MODULE, init_test_data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    EntitlementsTable = ets:new(
        taiea_entitlements,
        [public, named_table, {write_concurrency, true}, {read_concurrency, true}]
    ),
    ReceiptsTable = ets:new(
        taiea_entitlement_receipts,
        [public, named_table, {write_concurrency, true}, {read_concurrency, true}]
    ),

    State = #state{
        entitlements_table = EntitlementsTable,
        receipts_table = ReceiptsTable
    },

    %% Initialize test data on startup
    init_test_entitlements(EntitlementsTable),

    {ok, State}.

handle_call({apply_event, TenantId, Event}, _From, State) ->
    case handle_apply_event(TenantId, Event, State) of
        {ok, NewEntitlementState, Receipt} ->
            ets:insert(State#state.entitlements_table, {TenantId, NewEntitlementState}),
            ets:insert(State#state.receipts_table, {generate_receipt_id(), Receipt}),
            {reply, {ok, NewEntitlementState}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_active_packs, TenantId}, _From, State) ->
    Packs = case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, Ent}] ->
            maps:get(enabled_packs, Ent, []);
        [] ->
            []
    end,
    {reply, Packs, State};

handle_call({get_enabled_tools, TenantId}, _From, State) ->
    Tools = case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, Ent}] ->
            maps:get(enabled_tools, Ent, []);
        [] ->
            []
    end,
    {reply, Tools, State};

handle_call({verify_entitlement_active, TenantId}, _From, State) ->
    case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, Ent}] ->
            Expiry = maps:get(expiry, Ent, infinity),
            CurrentTime = timestamp(),
            if
                Expiry =:= infinity orelse CurrentTime =< Expiry ->
                    {reply, ok, State};
                true ->
                    {reply, {error, inactive}, State}
            end;
        [] ->
            {reply, {error, inactive}, State}
    end;

handle_call({verify_iam_role, TenantId, RequiredRole}, _From, State) ->
    case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, Ent}] ->
            Roles = maps:get(enabled_iam_roles, Ent, []),
            case lists:member(RequiredRole, Roles) of
                true -> {reply, ok, State};
                false -> {reply, {error, not_enabled}, State}
            end;
        [] ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({get_entitlement, TenantId}, _From, State) ->
    case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, Ent}] ->
            {reply, {ok, Ent}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_all_entitlements, _From, State) ->
    All = ets:match_object(State#state.entitlements_table, {'_', '$1'}),
    Entitlements = [Ent || {_TenantId, Ent} <- All],
    {reply, Entitlements, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(init_test_data, State) ->
    init_test_entitlements(State#state.entitlements_table),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.entitlements_table),
    ets:delete(State#state.receipts_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Initialize test entitlements for demo purposes
init_test_entitlements(Table) ->
    ets:delete_all_objects(Table),

    %% Test tenant 1: base SKU
    Ent1 = #{
        tenant_id => <<"tenant-001">>,
        sku => base,
        enabled_packs => [base],
        enabled_tools => [health, support_model],
        enabled_iam_roles => [read_only],
        expiry => infinity,
        created_at => timestamp(),
        updated_at => timestamp()
    },
    ets:insert(Table, {<<"tenant-001">>, Ent1}),

    %% Test tenant 2: professional SKU
    Ent2 = #{
        tenant_id => <<"tenant-002">>,
        sku => professional,
        enabled_packs => [base, professional],
        enabled_tools => [health, support_model, entitlement_apply, receipts_verify],
        enabled_iam_roles => [read_only, write_rollback],
        expiry => infinity,
        created_at => timestamp(),
        updated_at => timestamp()
    },
    ets:insert(Table, {<<"tenant-002">>, Ent2}),

    %% Test tenant 3: enterprise SKU
    Ent3 = #{
        tenant_id => <<"tenant-003">>,
        sku => enterprise,
        enabled_packs => [base, professional, enterprise],
        enabled_tools => [health, support_model, entitlement_apply, receipts_verify,
                         policy_evaluate, audit_log, custom_integrations],
        enabled_iam_roles => [read_only, write_rollback, admin, custom_role],
        expiry => infinity,
        created_at => timestamp(),
        updated_at => timestamp()
    },
    ets:insert(Table, {<<"tenant-003">>, Ent3}),

    %% Test tenant 4: professional with expiry
    Expiry4 = timestamp() + (30 * 24 * 60 * 60 * 1000),  %% 30 days from now
    Ent4 = #{
        tenant_id => <<"tenant-004">>,
        sku => professional,
        enabled_packs => [base, professional],
        enabled_tools => [health, support_model, entitlement_apply, receipts_verify],
        enabled_iam_roles => [read_only, write_rollback],
        expiry => Expiry4,
        created_at => timestamp(),
        updated_at => timestamp()
    },
    ets:insert(Table, {<<"tenant-004">>, Ent4}),

    %% Test tenant 5: base with no packs (new signup)
    Ent5 = #{
        tenant_id => <<"tenant-005">>,
        sku => base,
        enabled_packs => [],
        enabled_tools => [],
        enabled_iam_roles => [],
        expiry => infinity,
        created_at => timestamp(),
        updated_at => timestamp()
    },
    ets:insert(Table, {<<"tenant-005">>, Ent5}),

    ok.

%% @doc Handle entitlement event application
handle_apply_event(TenantId, Event, State) ->
    case ets:lookup(State#state.entitlements_table, TenantId) of
        [{TenantId, CurrentEnt}] ->
            apply_event_transition(TenantId, CurrentEnt, Event);
        [] ->
            {error, {entitlement_not_found, TenantId}}
    end.

%% @doc Apply event transition to current entitlement state
apply_event_transition(TenantId, CurrentEnt, {sku_changed, NewSku}) ->
    OldSku = maps:get(sku, CurrentEnt),
    NewEnt = CurrentEnt#{
        sku := NewSku,
        updated_at := timestamp(),
        enabled_packs := get_default_packs(NewSku),
        enabled_tools := get_default_tools(NewSku),
        enabled_iam_roles := get_default_roles(NewSku)
    },
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => TenantId,
        event => {sku_changed, NewSku},
        state_before => CurrentEnt,
        state_after => NewEnt,
        reason => <<"SKU transition from ", (atom_to_binary(OldSku))/binary,
                   " to ", (atom_to_binary(NewSku))/binary>>
    },
    {ok, NewEnt, Receipt};

apply_event_transition(TenantId, CurrentEnt, {pack_enabled, PackName}) ->
    CurrentPacks = maps:get(enabled_packs, CurrentEnt, []),
    case lists:member(PackName, CurrentPacks) of
        true ->
            {error, {pack_already_enabled, PackName}};
        false ->
            NewPacks = lists:sort([PackName | CurrentPacks]),
            NewTools = get_tools_for_pack(PackName, maps:get(enabled_tools, CurrentEnt, [])),
            NewRoles = get_roles_for_pack(PackName, maps:get(enabled_iam_roles, CurrentEnt, [])),
            NewEnt = CurrentEnt#{
                enabled_packs := NewPacks,
                enabled_tools := NewTools,
                enabled_iam_roles := NewRoles,
                updated_at := timestamp()
            },
            Receipt = #{
                id => generate_receipt_id(),
                timestamp => timestamp(),
                tenant_id => TenantId,
                event => {pack_enabled, PackName},
                state_before => CurrentEnt,
                state_after => NewEnt,
                reason => <<"Pack enabled: ", (atom_to_binary(PackName))/binary>>
            },
            {ok, NewEnt, Receipt}
    end;

apply_event_transition(TenantId, CurrentEnt, {pack_disabled, PackName}) ->
    CurrentPacks = maps:get(enabled_packs, CurrentEnt, []),
    case lists:member(PackName, CurrentPacks) of
        false ->
            {error, {pack_not_enabled, PackName}};
        true ->
            NewPacks = lists:delete(PackName, CurrentPacks),
            %% Remove tools/roles associated with this pack
            RemovedTools = get_tools_for_pack(PackName, []),
            RemovedRoles = get_roles_for_pack(PackName, []),
            CurrentTools = maps:get(enabled_tools, CurrentEnt, []),
            CurrentRoles = maps:get(enabled_iam_roles, CurrentEnt, []),
            NewTools = CurrentTools -- RemovedTools,
            NewRoles = CurrentRoles -- RemovedRoles,
            NewEnt = CurrentEnt#{
                enabled_packs := NewPacks,
                enabled_tools := NewTools,
                enabled_iam_roles := NewRoles,
                updated_at := timestamp()
            },
            Receipt = #{
                id => generate_receipt_id(),
                timestamp => timestamp(),
                tenant_id => TenantId,
                event => {pack_disabled, PackName},
                state_before => CurrentEnt,
                state_after => NewEnt,
                reason => <<"Pack disabled: ", (atom_to_binary(PackName))/binary>>
            },
            {ok, NewEnt, Receipt}
    end;

apply_event_transition(TenantId, CurrentEnt, {iam_role_added, RoleName}) ->
    CurrentRoles = maps:get(enabled_iam_roles, CurrentEnt, []),
    case lists:member(RoleName, CurrentRoles) of
        true ->
            {error, {role_already_enabled, RoleName}};
        false ->
            NewRoles = lists:sort([RoleName | CurrentRoles]),
            NewEnt = CurrentEnt#{
                enabled_iam_roles := NewRoles,
                updated_at := timestamp()
            },
            Receipt = #{
                id => generate_receipt_id(),
                timestamp => timestamp(),
                tenant_id => TenantId,
                event => {iam_role_added, RoleName},
                state_before => CurrentEnt,
                state_after => NewEnt,
                reason => <<"IAM role added: ", (atom_to_binary(RoleName))/binary>>
            },
            {ok, NewEnt, Receipt}
    end;

apply_event_transition(TenantId, CurrentEnt, {iam_role_removed, RoleName}) ->
    CurrentRoles = maps:get(enabled_iam_roles, CurrentEnt, []),
    case lists:member(RoleName, CurrentRoles) of
        false ->
            {error, {role_not_enabled, RoleName}};
        true ->
            NewRoles = lists:delete(RoleName, CurrentRoles),
            NewEnt = CurrentEnt#{
                enabled_iam_roles := NewRoles,
                updated_at := timestamp()
            },
            Receipt = #{
                id => generate_receipt_id(),
                timestamp => timestamp(),
                tenant_id => TenantId,
                event => {iam_role_removed, RoleName},
                state_before => CurrentEnt,
                state_after => NewEnt,
                reason => <<"IAM role removed: ", (atom_to_binary(RoleName))/binary>>
            },
            {ok, NewEnt, Receipt}
    end;

apply_event_transition(_TenantId, _CurrentEnt, InvalidEvent) ->
    {error, {invalid_event, InvalidEvent}}.

%% @doc Get default packs for a SKU
get_default_packs(base) -> [base];
get_default_packs(professional) -> [base, professional];
get_default_packs(enterprise) -> [base, professional, enterprise].

%% @doc Get default tools for a SKU
get_default_tools(base) -> [health, support_model];
get_default_tools(professional) ->
    [health, support_model, entitlement_apply, receipts_verify];
get_default_tools(enterprise) ->
    [health, support_model, entitlement_apply, receipts_verify,
     policy_evaluate, audit_log, custom_integrations].

%% @doc Get default IAM roles for a SKU
get_default_roles(base) -> [read_only];
get_default_roles(professional) -> [read_only, write_rollback];
get_default_roles(enterprise) -> [read_only, write_rollback, admin, custom_role].

%% @doc Get tools associated with a pack
get_tools_for_pack(base, Current) ->
    tools_union([health, support_model], Current);
get_tools_for_pack(professional, Current) ->
    tools_union([entitlement_apply, receipts_verify], Current);
get_tools_for_pack(enterprise, Current) ->
    tools_union([policy_evaluate, audit_log, custom_integrations], Current);
get_tools_for_pack(_Other, Current) ->
    Current.

%% @doc Get IAM roles associated with a pack
get_roles_for_pack(base, Current) ->
    roles_union([read_only], Current);
get_roles_for_pack(professional, Current) ->
    roles_union([write_rollback], Current);
get_roles_for_pack(enterprise, Current) ->
    roles_union([admin, custom_role], Current);
get_roles_for_pack(_Other, Current) ->
    Current.

%% @doc Union tools with current list
tools_union(New, Current) ->
    lists:usort(New ++ Current).

%% @doc Union roles with current list
roles_union(New, Current) ->
    lists:usort(New ++ Current).

%% @doc Generate a unique receipt ID
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @doc Get current timestamp in milliseconds
timestamp() ->
    erlang:system_time(millisecond).
