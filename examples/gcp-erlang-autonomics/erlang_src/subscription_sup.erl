%%%-------------------------------------------------------------------
%% @doc subscription_sup: Supervisor for subscription governor FSM instances
%%
%% Manages subscription lifecycle:
%% - Subscription provisioning
%% - Renewal decisions
%% - Termination handling
%% - Pause/resume operations
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(subscription_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_subscription_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, subscription_sup}, ?MODULE, []).

-spec start_subscription_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_subscription_governor(TenantId) ->
    ChildSpec = #{
        id => {subscription_governor, TenantId},
        start => {subscription_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [subscription_governor]
    },
    supervisor:start_child(subscription_sup, ChildSpec).

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
