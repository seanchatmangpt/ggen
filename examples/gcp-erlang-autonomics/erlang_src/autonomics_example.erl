%%%-------------------------------------------------------------------
%% @doc autonomics_example: Example usage of GCP Erlang Autonomics
%%
%% This module demonstrates how to:
%% - Start the autonomics system
%% - Create governor FSM instances for tenants
%% - Store and query receipts
%% - Monitor system health
%% - Register alert policies
%%
%% @end
%%%-------------------------------------------------------------------
-module(autonomics_example).

%% API
-export([
    start_system/0,
    start_system/1,
    stop_system/0,
    create_tenant_governors/1,
    store_sample_receipt/1,
    query_tenant_receipts/1,
    check_system_health/0,
    setup_alert_policies/0,
    demo_workflow/0
]).

%%%===================================================================
%% API Functions
%%%===================================================================

%% @doc Start the autonomics system with default configuration
-spec start_system() -> {ok, pid()} | {error, term()}.
start_system() ->
    start_system(#{}).

%% @doc Start the autonomics system with custom configuration
-spec start_system(Config) -> {ok, pid()} | {error, term()}
  when Config :: map().
start_system(_Config) ->
    case application:start(gcp_erlang_autonomics) of
        ok ->
            io:format("GCP Erlang Autonomics started successfully~n"),
            case autonomics_sup:start_link() of
                {ok, Pid} ->
                    io:format("Root supervisor started: ~p~n", [Pid]),
                    {ok, Pid};
                {error, {already_started, Pid}} ->
                    io:format("System already started: ~p~n", [Pid]),
                    {ok, Pid};
                Error ->
                    io:format("Failed to start root supervisor: ~p~n", [Error]),
                    Error
            end;
        {error, {already_started, gcp_erlang_autonomics}} ->
            io:format("Application already started~n"),
            case supervisor:which_children(autonomics_sup) of
                Children when is_list(Children) ->
                    RootPid = whereis(autonomics_sup),
                    {ok, RootPid};
                _ ->
                    {error, root_supervisor_not_found}
            end;
        Error ->
            io:format("Failed to start application: ~p~n", [Error]),
            Error
    end.

%% @doc Stop the autonomics system
-spec stop_system() -> ok | {error, term()}.
stop_system() ->
    case application:stop(gcp_erlang_autonomics) of
        ok ->
            io:format("GCP Erlang Autonomics stopped successfully~n"),
            ok;
        Error ->
            io:format("Failed to stop application: ~p~n", [Error]),
            Error
    end.

%% @doc Create governor FSM instances for a tenant
-spec create_tenant_governors(TenantId) -> ok | {error, term()}
  when TenantId :: binary() | string().
create_tenant_governors(TenantId) ->
    io:format("Creating governors for tenant: ~p~n", [TenantId]),

    %% Start entitlement governor
    case entitlement_sup:start_entitlement_governor(TenantId) of
        {ok, EntitlementPid} ->
            io:format("  ✓ Entitlement governor: ~p~n", [EntitlementPid]);
        {error, EntitlementError} ->
            io:format("  ✗ Entitlement governor failed: ~p~n", [EntitlementError])
    end,

    %% Start billing governor
    case billing_sup:start_billing_governor(TenantId) of
        {ok, BillingPid} ->
            io:format("  ✓ Billing governor: ~p~n", [BillingPid]);
        {error, BillingError} ->
            io:format("  ✗ Billing governor failed: ~p~n", [BillingError])
    end,

    %% Start quota/SLA governor
    case quota_sla_sup:start_quota_sla_governor(TenantId) of
        {ok, QuotaPid} ->
            io:format("  ✓ Quota/SLA governor: ~p~n", [QuotaPid]);
        {error, QuotaError} ->
            io:format("  ✗ Quota/SLA governor failed: ~p~n", [QuotaError])
    end,

    %% Start remaining governors
    case subscription_sup:start_subscription_governor(TenantId) of
        {ok, _SubscriptionPid} ->
            io:format("  ✓ Subscription governor~n");
        {error, _} ->
            io:format("  ✗ Subscription governor failed~n")
    end,

    case customer_account_sup:start_customer_account_governor(TenantId) of
        {ok, _AccountPid} ->
            io:format("  ✓ Customer account governor~n");
        {error, _} ->
            io:format("  ✗ Customer account governor failed~n")
    end,

    case compliance_audit_sup:start_compliance_audit_governor(TenantId) of
        {ok, _CompliancePid} ->
            io:format("  ✓ Compliance audit governor~n");
        {error, _} ->
            io:format("  ✗ Compliance audit governor failed~n")
    end,

    case multi_tenant_sup:start_multi_tenant_governor(TenantId) of
        {ok, _MultiTenantPid} ->
            io:format("  ✓ Multi-tenant governor~n");
        {error, _} ->
            io:format("  ✗ Multi-tenant governor failed~n")
    end,

    case product_catalog_sup:start_product_catalog_governor(TenantId) of
        {ok, _CatalogPid} ->
            io:format("  ✓ Product catalog governor~n");
        {error, _} ->
            io:format("  ✗ Product catalog governor failed~n")
    end,

    io:format("Tenant governors created~n"),
    ok.

%% @doc Store a sample receipt in the ledger
-spec store_sample_receipt(TenantId) -> ok | {error, term()}
  when TenantId :: binary() | string().
store_sample_receipt(TenantId) ->
    Receipt = #{
        execution_id => crypto:strong_rand_bytes(16),
        timestamp => erlang:system_time(millisecond),
        tenant_id => TenantId,
        manifest_hash => <<"manifest_hash_value">>,
        ontology_hash => <<"ontology_hash_value">>,
        files => [
            #{path => "generated/file1.rs", hash => <<"hash1">>},
            #{path => "generated/file2.rs", hash => <<"hash2">>}
        ],
        audit_trail => "/path/to/audit.json"
    },

    case receipt_store:store_receipt(Receipt) of
        ok ->
            io:format("Receipt stored successfully~n"),
            ok;
        {error, Reason} ->
            io:format("Failed to store receipt: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Query receipts for a tenant
-spec query_tenant_receipts(TenantId) -> ok | {error, term()}
  when TenantId :: binary() | string().
query_tenant_receipts(TenantId) ->
    case receipt_store:query_receipts({tenant_id, TenantId}, []) of
        {ok, Receipts} ->
            io:format("Found ~p receipts for tenant ~p~n", [length(Receipts), TenantId]),
            lists:foreach(
                fun(Receipt) ->
                    ExecutionId = maps:get(execution_id, Receipt),
                    Timestamp = maps:get(timestamp, Receipt),
                    io:format("  - Execution: ~s (Timestamp: ~p)~n", [
                        base64:encode_to_string(ExecutionId),
                        Timestamp
                    ])
                end,
                Receipts
            ),
            ok;
        {error, Reason} ->
            io:format("Failed to query receipts: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Check overall system health
-spec check_system_health() -> ok | {error, term()}.
check_system_health() ->
    io:format("=== System Health Check ===~n"),

    %% Check node health
    case node_monitor:get_node_health() of
        {ok, Health} ->
            Status = maps:get(status, Health),
            CPUUsage = maps:get(cpu_usage, Health, 0),
            MemoryUsage = maps:get(memory_usage, Health, 0),
            ProcessCount = maps:get(process_count, Health, 0),
            io:format("Node Health: ~p~n", [Status]),
            io:format("  CPU Usage: ~.2f%~n", [CPUUsage]),
            io:format("  Memory Usage: ~.2f%~n", [MemoryUsage]),
            io:format("  Process Count: ~p~n", [ProcessCount]);
        {error, Reason} ->
            io:format("Failed to get node health: ~p~n", [Reason])
    end,

    %% Check cluster status
    case cluster_mgr:get_cluster_status() of
        {ok, ClusterStatus} ->
            Nodes = maps:get(nodes, ClusterStatus, []),
            IsLeader = maps:get(is_leader, ClusterStatus, false),
            Leader = maps:get(leader, ClusterStatus, undefined),
            io:format("Cluster Status:~n"),
            io:format("  Nodes: ~p~n", [Nodes]),
            io:format("  Is Leader: ~p~n", [IsLeader]),
            io:format("  Leader: ~p~n", [Leader]);
        {error, Reason} ->
            io:format("Failed to get cluster status: ~p~n", [Reason])
    end,

    %% Check metrics
    case metrics_collector:get_metrics() of
        {ok, Metrics} ->
            io:format("Metrics collected: ~p~n", [length(Metrics)]);
        {error, Reason} ->
            io:format("Failed to get metrics: ~p~n", [Reason])
    end,

    %% Check active alerts
    case alert_manager:get_active_alerts() of
        {ok, Alerts} ->
            AlertCount = length(Alerts),
            case AlertCount > 0 of
                true ->
                    io:format("Active Alerts: ~p~n", [AlertCount]),
                    lists:foreach(
                        fun(Alert) ->
                            Policy = maps:get(policy_name, Alert),
                            Severity = maps:get(severity, Alert),
                            io:format("  - ~s (~s)~n", [Policy, Severity])
                        end,
                        Alerts
                    );
                false ->
                    io:format("No active alerts~n")
            end;
        {error, Reason} ->
            io:format("Failed to get alerts: ~p~n", [Reason])
    end,

    io:format("=== Health Check Complete ===~n"),
    ok.

%% @doc Setup sample alert policies
-spec setup_alert_policies() -> ok.
setup_alert_policies() ->
    io:format("Setting up alert policies...~n"),

    %% High CPU alert
    HighCpuPolicy = #{
        name => "high_cpu",
        metric_name => cpu_usage,
        threshold => 80.0,
        comparison => 'gt',
        severity => warning,
        duration_seconds => 60
    },
    case alert_manager:register_alert_policy("high_cpu", HighCpuPolicy) of
        ok ->
            io:format("  ✓ High CPU alert policy registered~n");
        {error, Error} ->
            io:format("  ✗ Failed to register high CPU alert: ~p~n", [Error])
    end,

    %% High memory alert
    HighMemoryPolicy = #{
        name => "high_memory",
        metric_name => memory_usage,
        threshold => 85.0,
        comparison => 'gt',
        severity => critical,
        duration_seconds => 60
    },
    case alert_manager:register_alert_policy("high_memory", HighMemoryPolicy) of
        ok ->
            io:format("  ✓ High memory alert policy registered~n");
        {error, Error} ->
            io:format("  ✗ Failed to register high memory alert: ~p~n", [Error])
    end,

    %% Low disk space alert
    LowDiskPolicy = #{
        name => "low_disk",
        metric_name => disk_usage,
        threshold => 90.0,
        comparison => 'gt',
        severity => critical,
        duration_seconds => 300
    },
    case alert_manager:register_alert_policy("low_disk", LowDiskPolicy) of
        ok ->
            io:format("  ✓ Low disk space alert policy registered~n");
        {error, Error} ->
            io:format("  ✗ Failed to register low disk alert: ~p~n", [Error])
    end,

    io:format("Alert policies setup complete~n"),
    ok.

%% @doc Demo workflow: Start system, create tenants, store receipts
-spec demo_workflow() -> ok | {error, term()}.
demo_workflow() ->
    io:format("~n=== GCP Erlang Autonomics Demo Workflow ===~n~n"),

    %% Step 1: Start system
    io:format("Step 1: Starting system...~n"),
    case start_system() of
        {ok, _Pid} ->
            io:format("✓ System started~n~n");
        Error ->
            io:format("✗ System start failed: ~p~n", [Error]),
            throw(system_start_failed)
    end,

    %% Step 2: Create tenant governors
    io:format("Step 2: Creating governors for sample tenants...~n"),
    Tenant1 = <<"tenant-001">>,
    Tenant2 = <<"tenant-002">>,
    create_tenant_governors(Tenant1),
    io:format("~n"),
    create_tenant_governors(Tenant2),
    io:format("~n"),

    %% Step 3: Store sample receipts
    io:format("Step 3: Storing sample receipts...~n"),
    store_sample_receipt(Tenant1),
    store_sample_receipt(Tenant2),
    io:format("~n"),

    %% Step 4: Query receipts
    io:format("Step 4: Querying receipts...~n"),
    query_tenant_receipts(Tenant1),
    io:format("~n"),
    query_tenant_receipts(Tenant2),
    io:format("~n"),

    %% Step 5: Setup alerts
    io:format("Step 5: Setting up alert policies...~n"),
    setup_alert_policies(),
    io:format("~n"),

    %% Step 6: Check health
    check_system_health(),
    io:format("~n"),

    io:format("=== Demo Workflow Complete ===~n"),
    ok.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%%%===================================================================
%% End of module
%%%===================================================================
