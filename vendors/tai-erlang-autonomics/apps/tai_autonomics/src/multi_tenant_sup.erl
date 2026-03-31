%%%-------------------------------------------------------------------
%% @doc multi_tenant_sup: Supervisor for multi-tenant governor FSM instances
%%
%% Manages multi-tenancy concerns:
%% - Tenant isolation enforcement
%% - Resource quota distribution
%% - Tenant-level SLA management
%% - Cross-tenant fairness
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(multi_tenant_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_multi_tenant_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, multi_tenant_sup}, ?MODULE, []).

-spec start_multi_tenant_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_multi_tenant_governor(TenantId) ->
    ChildSpec = #{
        id => {multi_tenant_governor, TenantId},
        start => {multi_tenant_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [multi_tenant_governor]
    },
    supervisor:start_child(multi_tenant_sup, ChildSpec).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    {ok, {SupFlags, []}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
