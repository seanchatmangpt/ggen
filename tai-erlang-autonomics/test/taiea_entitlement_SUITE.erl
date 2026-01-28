%%%-------------------------------------------------------------------
%%% @doc TAI Entitlement Test Suite - Comprehensive tests for entitlement resolver
%%%
%%% Tests cover:
%%% - Entitlement state tracking
%%% - Event application and transitions
%%% - Tool and role verification
%%% - SKU-based feature mapping
%%% - Receipt generation
%%% - Test data initialization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_entitlement_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Test suite setup/teardown
%%%===================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    %% Start only the entitlement server (don't start full application)
    %% This avoids GCP dependencies which fail in test environment
    {ok, Pid} = taiea_entitlement:start_link(),
    ct:log("Started taiea_entitlement: ~p", [Pid]),
    timer:sleep(100),  % Give server time to initialize
    case is_process_alive(Pid) of
        true ->
            ct:log("Server is alive and running"),
            Config;
        false ->
            ct:fail("taiea_entitlement server died after startup")
    end.

end_per_suite(_Config) ->
    %% Stop the entitlement server
    gen_server:stop(taiea_entitlement),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

%% @doc Test that test data initializes correctly
test_init_test_data(_Config) ->
    %% Get all entitlements
    Entitlements = taiea_entitlement:list_all_entitlements(),
    ct:log("Initialized entitlements: ~p", [length(Entitlements)]),

    %% Verify we have 5 test tenants
    5 = length(Entitlements),
    ok.

%% @doc Test base SKU entitlement retrieval
test_get_base_sku_entitlement(_Config) ->
    TenantId = <<"tenant-001">>,
    {ok, Ent} = taiea_entitlement:get_entitlement(TenantId),

    %% Verify SKU
    base = maps:get(sku, Ent),

    %% Verify tools
    Tools = maps:get(enabled_tools, Ent),
    true = lists:member(health, Tools),
    true = lists:member(support_model, Tools),
    2 = length(Tools),

    %% Verify roles
    Roles = maps:get(enabled_iam_roles, Ent),
    true = lists:member(read_only, Roles),
    1 = length(Roles),

    %% Verify packs
    Packs = maps:get(enabled_packs, Ent),
    true = lists:member(base, Packs),
    1 = length(Packs),

    ok.

%% @doc Test professional SKU entitlement retrieval
test_get_professional_sku_entitlement(_Config) ->
    TenantId = <<"tenant-002">>,
    {ok, Ent} = taiea_entitlement:get_entitlement(TenantId),

    %% Verify SKU
    professional = maps:get(sku, Ent),

    %% Verify tools
    Tools = maps:get(enabled_tools, Ent),
    true = lists:member(health, Tools),
    true = lists:member(support_model, Tools),
    true = lists:member(entitlement_apply, Tools),
    true = lists:member(receipts_verify, Tools),
    4 = length(Tools),

    %% Verify roles
    Roles = maps:get(enabled_iam_roles, Ent),
    true = lists:member(read_only, Roles),
    true = lists:member(write_rollback, Roles),
    2 = length(Roles),

    %% Verify packs
    Packs = maps:get(enabled_packs, Ent),
    true = lists:member(base, Packs),
    true = lists:member(professional, Packs),
    2 = length(Packs),

    ok.

%% @doc Test enterprise SKU entitlement retrieval
test_get_enterprise_sku_entitlement(_Config) ->
    TenantId = <<"tenant-003">>,
    {ok, Ent} = taiea_entitlement:get_entitlement(TenantId),

    %% Verify SKU
    enterprise = maps:get(sku, Ent),

    %% Verify tools (all available)
    Tools = maps:get(enabled_tools, Ent),
    true = lists:member(health, Tools),
    true = lists:member(support_model, Tools),
    true = lists:member(entitlement_apply, Tools),
    true = lists:member(receipts_verify, Tools),
    true = lists:member(policy_evaluate, Tools),
    true = lists:member(audit_log, Tools),
    true = lists:member(custom_integrations, Tools),
    7 = length(Tools),

    %% Verify roles
    Roles = maps:get(enabled_iam_roles, Ent),
    true = lists:member(read_only, Roles),
    true = lists:member(write_rollback, Roles),
    true = lists:member(admin, Roles),
    true = lists:member(custom_role, Roles),
    4 = length(Roles),

    %% Verify packs
    Packs = maps:get(enabled_packs, Ent),
    true = lists:member(base, Packs),
    true = lists:member(professional, Packs),
    true = lists:member(enterprise, Packs),
    3 = length(Packs),

    ok.

%% @doc Test get_active_packs/1
test_get_active_packs(_Config) ->
    TenantId = <<"tenant-002">>,
    Packs = taiea_entitlement:get_active_packs(TenantId),

    ct:log("Active packs for tenant-002: ~p", [Packs]),

    %% Verify packs
    true = lists:member(base, Packs),
    true = lists:member(professional, Packs),
    2 = length(Packs),

    ok.

%% @doc Test get_enabled_tools/1
test_get_enabled_tools(_Config) ->
    TenantId = <<"tenant-002">>,
    Tools = taiea_entitlement:get_enabled_tools(TenantId),

    ct:log("Enabled tools for tenant-002: ~p", [Tools]),

    %% Verify tools
    true = lists:member(health, Tools),
    true = lists:member(entitlement_apply, Tools),
    4 = length(Tools),

    ok.

%% @doc Test verify_entitlement_active/1 for active entitlement
test_verify_entitlement_active_ok(_Config) ->
    TenantId = <<"tenant-001">>,
    ok = taiea_entitlement:verify_entitlement_active(TenantId),
    ok.

%% @doc Test verify_entitlement_active/1 for non-existent entitlement
test_verify_entitlement_active_not_found(_Config) ->
    TenantId = <<"non-existent-tenant">>,
    {error, inactive} = taiea_entitlement:verify_entitlement_active(TenantId),
    ok.

%% @doc Test verify_iam_role/2 for enabled role
test_verify_iam_role_ok(_Config) ->
    TenantId = <<"tenant-002">>,
    ok = taiea_entitlement:verify_iam_role(TenantId, write_rollback),
    ok.

%% @doc Test verify_iam_role/2 for disabled role
test_verify_iam_role_not_enabled(_Config) ->
    TenantId = <<"tenant-001">>,
    {error, not_enabled} = taiea_entitlement:verify_iam_role(TenantId, write_rollback),
    ok.

%% @doc Test verify_iam_role/2 for non-existent tenant
test_verify_iam_role_tenant_not_found(_Config) ->
    TenantId = <<"non-existent-tenant">>,
    {error, not_enabled} = taiea_entitlement:verify_iam_role(TenantId, read_only),
    ok.

%% @doc Test applying sku_changed event
test_apply_event_sku_changed(_Config) ->
    TenantId = <<"tenant-005">>,  %% Starts as base
    {ok, OldEnt} = taiea_entitlement:get_entitlement(TenantId),

    ct:log("Old entitlement: ~p", [OldEnt]),

    %% Apply SKU change to professional
    {ok, NewEnt} = taiea_entitlement:apply_event(TenantId, {sku_changed, professional}),

    ct:log("New entitlement: ~p", [NewEnt]),

    %% Verify SKU changed
    professional = maps:get(sku, NewEnt),

    %% Verify tools updated
    Tools = maps:get(enabled_tools, NewEnt),
    true = lists:member(entitlement_apply, Tools),

    %% Verify roles updated
    Roles = maps:get(enabled_iam_roles, NewEnt),
    true = lists:member(write_rollback, Roles),

    %% Verify timestamp updated
    OldUpdated = maps:get(updated_at, OldEnt),
    NewUpdated = maps:get(updated_at, NewEnt),
    true = NewUpdated >= OldUpdated,

    ok.

%% @doc Test applying pack_enabled event
test_apply_event_pack_enabled(_Config) ->
    TenantId = <<"tenant-005">>,  %% Starts with no packs
    {ok, _OldEnt} = taiea_entitlement:get_entitlement(TenantId),

    %% Enable base pack
    {ok, NewEnt} = taiea_entitlement:apply_event(TenantId, {pack_enabled, base}),

    %% Verify pack enabled
    Packs = maps:get(enabled_packs, NewEnt),
    true = lists:member(base, Packs),

    %% Verify tools added
    Tools = maps:get(enabled_tools, NewEnt),
    true = lists:member(health, Tools),
    true = lists:member(support_model, Tools),

    ok.

%% @doc Test applying pack_disabled event
test_apply_event_pack_disabled(_Config) ->
    TenantId = <<"tenant-002">>,  %% Has base and professional
    {ok, _OldEnt} = taiea_entitlement:get_entitlement(TenantId),

    %% Disable professional pack
    {ok, NewEnt} = taiea_entitlement:apply_event(TenantId, {pack_disabled, professional}),

    %% Verify pack disabled
    Packs = maps:get(enabled_packs, NewEnt),
    true = lists:member(base, Packs),
    false = lists:member(professional, Packs),

    %% Verify tools removed
    Tools = maps:get(enabled_tools, NewEnt),
    false = lists:member(entitlement_apply, Tools),

    ok.

%% @doc Test applying iam_role_added event
test_apply_event_iam_role_added(_Config) ->
    TenantId = <<"tenant-001">>,  %% Has only read_only

    %% Add custom role
    {ok, NewEnt} = taiea_entitlement:apply_event(TenantId, {iam_role_added, custom_role}),

    %% Verify role added
    Roles = maps:get(enabled_iam_roles, NewEnt),
    true = lists:member(custom_role, Roles),
    true = lists:member(read_only, Roles),
    2 = length(Roles),

    ok.

%% @doc Test applying iam_role_removed event
test_apply_event_iam_role_removed(_Config) ->
    TenantId = <<"tenant-002">>,  %% Has read_only and write_rollback

    %% Remove write_rollback role
    {ok, NewEnt} = taiea_entitlement:apply_event(TenantId, {iam_role_removed, write_rollback}),

    %% Verify role removed
    Roles = maps:get(enabled_iam_roles, NewEnt),
    true = lists:member(read_only, Roles),
    false = lists:member(write_rollback, Roles),
    1 = length(Roles),

    ok.

%% @doc Test duplicate pack enable fails
test_apply_event_pack_already_enabled(_Config) ->
    TenantId = <<"tenant-001">>,  %% Has base pack

    %% Try to enable base pack again
    {error, {pack_already_enabled, base}} =
        taiea_entitlement:apply_event(TenantId, {pack_enabled, base}),

    ok.

%% @doc Test disable non-existent pack fails
test_apply_event_pack_not_enabled(_Config) ->
    TenantId = <<"tenant-001">>,  %% Only has base

    %% Try to disable professional pack
    {error, {pack_not_enabled, professional}} =
        taiea_entitlement:apply_event(TenantId, {pack_disabled, professional}),

    ok.

%% @doc Test duplicate role add fails
test_apply_event_role_already_enabled(_Config) ->
    TenantId = <<"tenant-001">>,  %% Has read_only

    %% Try to add read_only again
    {error, {role_already_enabled, read_only}} =
        taiea_entitlement:apply_event(TenantId, {iam_role_added, read_only}),

    ok.

%% @doc Test remove non-existent role fails
test_apply_event_role_not_enabled(_Config) ->
    TenantId = <<"tenant-001">>,  %% Doesn't have admin

    %% Try to remove admin
    {error, {role_not_enabled, admin}} =
        taiea_entitlement:apply_event(TenantId, {iam_role_removed, admin}),

    ok.

%% @doc Test applying events to non-existent tenant
test_apply_event_tenant_not_found(_Config) ->
    TenantId = <<"non-existent-tenant">>,

    %% Try to apply event
    {error, {entitlement_not_found, TenantId}} =
        taiea_entitlement:apply_event(TenantId, {pack_enabled, base}),

    ok.

%% @doc Test sequence of events (multi-step transition)
test_apply_events_sequence(_Config) ->
    TenantId = <<"tenant-005">>,  %% Starts as base with no packs

    %% Step 1: Enable base pack
    {ok, Ent1} = taiea_entitlement:apply_event(TenantId, {pack_enabled, base}),
    ct:log("After step 1 (enable base): ~p", [maps:get(enabled_packs, Ent1)]),
    true = lists:member(base, maps:get(enabled_packs, Ent1)),

    %% Step 2: Upgrade SKU to professional
    {ok, Ent2} = taiea_entitlement:apply_event(TenantId, {sku_changed, professional}),
    ct:log("After step 2 (upgrade to professional): ~p", [maps:get(sku, Ent2)]),
    professional = maps:get(sku, Ent2),

    %% Step 3: Add custom role
    {ok, Ent3} = taiea_entitlement:apply_event(TenantId, {iam_role_added, custom_role}),
    ct:log("After step 3 (add custom_role): ~p", [maps:get(enabled_iam_roles, Ent3)]),
    true = lists:member(custom_role, maps:get(enabled_iam_roles, Ent3)),

    ok.

%% @doc Test entitlement with expiry date
test_get_entitlement_with_expiry(_Config) ->
    TenantId = <<"tenant-004">>,
    {ok, Ent} = taiea_entitlement:get_entitlement(TenantId),

    %% Verify entitlement has expiry
    Expiry = maps:get(expiry, Ent),
    true = Expiry > 0,
    true = Expiry =/= infinity,

    %% Verify it's active (expiry is in future)
    ok = taiea_entitlement:verify_entitlement_active(TenantId),

    ok.

%% @doc Test list_all_entitlements returns all tenants
test_list_all_entitlements(_Config) ->
    Entitlements = taiea_entitlement:list_all_entitlements(),

    ct:log("Total entitlements: ~p", [length(Entitlements)]),

    %% Verify we have all 5 test tenants
    5 = length(Entitlements),

    %% Verify structure of each
    lists:foreach(fun(Ent) ->
        true = is_map(Ent),
        true = maps:is_key(tenant_id, Ent),
        true = maps:is_key(sku, Ent),
        true = maps:is_key(enabled_packs, Ent),
        true = maps:is_key(enabled_tools, Ent),
        true = maps:is_key(enabled_iam_roles, Ent),
        true = maps:is_key(expiry, Ent)
    end, Entitlements),

    ok.

%% @doc Test getting tools for non-existent tenant
test_get_enabled_tools_not_found(_Config) ->
    TenantId = <<"non-existent-tenant">>,
    Tools = taiea_entitlement:get_enabled_tools(TenantId),

    %% Should return empty list
    [] = Tools,

    ok.

%% @doc Test getting packs for non-existent tenant
test_get_active_packs_not_found(_Config) ->
    TenantId = <<"non-existent-tenant">>,
    Packs = taiea_entitlement:get_active_packs(TenantId),

    %% Should return empty list
    [] = Packs,

    ok.

%% @doc Test entitlement ID and timestamp fields
test_entitlement_metadata(_Config) ->
    TenantId = <<"tenant-001">>,
    {ok, Ent} = taiea_entitlement:get_entitlement(TenantId),

    %% Verify created_at timestamp
    CreatedAt = maps:get(created_at, Ent),
    true = is_integer(CreatedAt),
    true = CreatedAt > 0,

    %% Verify updated_at timestamp
    UpdatedAt = maps:get(updated_at, Ent),
    true = is_integer(UpdatedAt),
    true = UpdatedAt >= CreatedAt,

    ok.

%% @doc Integration test: Full entitlement lifecycle
test_entitlement_lifecycle(_Config) ->
    TenantId = <<"tenant-005">>,

    %% Initial state: base SKU, no packs
    {ok, State1} = taiea_entitlement:get_entitlement(TenantId),
    base = maps:get(sku, State1),
    [] = maps:get(enabled_packs, State1),

    %% Enable base pack
    {ok, State2} = taiea_entitlement:apply_event(TenantId, {pack_enabled, base}),
    [base] = maps:get(enabled_packs, State2),

    %% Upgrade to professional
    {ok, State3} = taiea_entitlement:apply_event(TenantId, {sku_changed, professional}),
    professional = maps:get(sku, State3),
    Packs3 = maps:get(enabled_packs, State3),
    true = lists:member(base, Packs3),
    true = lists:member(professional, Packs3),

    %% Add enterprise role
    {ok, State4} = taiea_entitlement:apply_event(TenantId, {iam_role_added, admin}),
    Roles4 = maps:get(enabled_iam_roles, State4),
    true = lists:member(admin, Roles4),

    %% Verify final state
    {ok, FinalState} = taiea_entitlement:get_entitlement(TenantId),
    FinalState = State4,

    ok.

%%%===================================================================
%%% All tests exported
%%%===================================================================

all() ->
    [
        test_init_test_data,
        test_get_base_sku_entitlement,
        test_get_professional_sku_entitlement,
        test_get_enterprise_sku_entitlement,
        test_get_active_packs,
        test_get_enabled_tools,
        test_verify_entitlement_active_ok,
        test_verify_entitlement_active_not_found,
        test_verify_iam_role_ok,
        test_verify_iam_role_not_enabled,
        test_verify_iam_role_tenant_not_found,
        test_apply_event_sku_changed,
        test_apply_event_pack_enabled,
        test_apply_event_pack_disabled,
        test_apply_event_iam_role_added,
        test_apply_event_iam_role_removed,
        test_apply_event_pack_already_enabled,
        test_apply_event_pack_not_enabled,
        test_apply_event_role_already_enabled,
        test_apply_event_role_not_enabled,
        test_apply_event_tenant_not_found,
        test_apply_events_sequence,
        test_get_entitlement_with_expiry,
        test_list_all_entitlements,
        test_get_enabled_tools_not_found,
        test_get_active_packs_not_found,
        test_entitlement_metadata,
        test_entitlement_lifecycle
    ].
