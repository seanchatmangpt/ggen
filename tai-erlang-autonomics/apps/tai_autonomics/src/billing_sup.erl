%%%-------------------------------------------------------------------
%% @doc billing_sup: Supervisor for billing governor FSM instances
%%
%% Each tenant gets one billing FSM instance that manages:
%% - Usage-based pricing decisions
%% - Meter aggregation
%% - Cost optimization
%% - Overage handling
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% FSM restart: temporary (don't auto-restart failed FSM)
%% @end
%%%-------------------------------------------------------------------
-module(billing_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_billing_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the billing supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, billing_sup}, ?MODULE, []).

%% @doc Dynamically start a billing governor FSM for a tenant
-spec start_billing_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_billing_governor(TenantId) ->
    ChildSpec = #{
        id => {billing_governor, TenantId},
        start => {billing_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [billing_governor]
    },
    supervisor:start_child(billing_sup, ChildSpec).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Init callback
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% No static children; FSMs started dynamically
    {ok, {SupFlags, []}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
