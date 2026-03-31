%%%-------------------------------------------------------------------
%% @doc governance_sup: Supervisor for all autonomous governors
%%
%% Manages 8 governor supervisors:
%% - entitlement_sup: Entitlement rule enforcement
%% - billing_sup: Usage-based billing decisions
%% - product_catalog_sup: Product/SKU management
%% - subscription_sup: Subscription lifecycle
%% - customer_account_sup: Account management
%% - quota_sla_sup: Quota and SLA enforcement
%% - compliance_audit_sup: Compliance tracking
%% - multi_tenant_sup: Multi-tenant isolation
%%
%% Restart strategy: one_for_one (independent failure domains)
%% @end
%%%-------------------------------------------------------------------
-module(governance_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the governance supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Init callback - defines governor supervision tree
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent failure domains
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% TAI Entitlement resolver supervisor (manages taiea_entitlement gen_server)
        #{
            id => taiea_entitlement_sup,
            start => {taiea_entitlement_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [taiea_entitlement_sup]
        },

        %% Entitlement governor supervisor
        #{
            id => entitlement_sup,
            start => {entitlement_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [entitlement_sup]
        },

        %% Billing governor supervisor
        #{
            id => billing_sup,
            start => {billing_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [billing_sup]
        },

        %% Product catalog governor supervisor
        #{
            id => product_catalog_sup,
            start => {product_catalog_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [product_catalog_sup]
        },

        %% Subscription governor supervisor
        #{
            id => subscription_sup,
            start => {subscription_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [subscription_sup]
        },

        %% Customer account governor supervisor
        #{
            id => customer_account_sup,
            start => {customer_account_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [customer_account_sup]
        },

        %% Quota & SLA governor supervisor
        #{
            id => quota_sla_sup,
            start => {quota_sla_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [quota_sla_sup]
        },

        %% Compliance audit governor supervisor
        #{
            id => compliance_audit_sup,
            start => {compliance_audit_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [compliance_audit_sup]
        },

        %% Multi-tenant governor supervisor
        #{
            id => multi_tenant_sup,
            start => {multi_tenant_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [multi_tenant_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
