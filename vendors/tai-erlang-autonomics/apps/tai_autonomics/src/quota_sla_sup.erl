%%%-------------------------------------------------------------------
%% @doc quota_sla_sup: Supervisor for quota & SLA governor FSM instances
%%
%% Manages quota and SLA enforcement:
%% - Usage quota tracking
%% - SLA violation detection
%% - Rate limiting decisions
%% - Quota reset scheduling
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(quota_sla_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_quota_sla_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, quota_sla_sup}, ?MODULE, []).

-spec start_quota_sla_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_quota_sla_governor(TenantId) ->
    ChildSpec = #{
        id => {quota_sla_governor, TenantId},
        start => {quota_sla_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [quota_sla_governor]
    },
    supervisor:start_child(quota_sla_sup, ChildSpec).

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
