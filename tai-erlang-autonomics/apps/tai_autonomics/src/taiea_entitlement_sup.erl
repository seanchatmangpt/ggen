%%%-------------------------------------------------------------------
%%% @doc TAI Entitlement Supervisor - Manages entitlement resolver
%%%
%%% Supervises the taiea_entitlement gen_server which tracks per-tenant
%%% entitlement state across all tenants.
%%%
%%% Strategy: one_for_one (the gen_server is a singleton)
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_entitlement_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the entitlement supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, taiea_entitlement_sup}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
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

    EntitlementServer = #{
        id => taiea_entitlement,
        start => {taiea_entitlement, start_link, []},
        restart => permanent,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [taiea_entitlement]
    },

    {ok, {SupFlags, [EntitlementServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
