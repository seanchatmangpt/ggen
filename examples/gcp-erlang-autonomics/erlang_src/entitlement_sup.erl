%%%-------------------------------------------------------------------
%% @doc entitlement_sup: Supervisor for entitlement governor FSM instances
%%
%% Each tenant gets one entitlement FSM instance that manages:
%% - Feature entitlement rules
%% - Access control decisions
%% - License enforcement
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% FSM restart: temporary (don't auto-restart failed FSM, let parent handle)
%% @end
%%%-------------------------------------------------------------------
-module(entitlement_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_entitlement_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the entitlement supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, entitlement_sup}, ?MODULE, []).

%% @doc Dynamically start an entitlement governor FSM for a tenant
-spec start_entitlement_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_entitlement_governor(TenantId) ->
    ChildSpec = #{
        id => {entitlement_governor, TenantId},
        start => {entitlement_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [entitlement_governor]
    },
    supervisor:start_child(entitlement_sup, ChildSpec).

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

    %% No static children; FSMs started dynamically via start_entitlement_governor/1
    {ok, {SupFlags, []}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
