%%%-------------------------------------------------------------------
%% @doc product_catalog_sup: Supervisor for product catalog governor FSM instances
%%
%% Manages product/SKU catalog decisions:
%% - SKU availability
%% - Pricing rules
%% - Product bundling
%% - Catalog updates
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(product_catalog_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_product_catalog_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, product_catalog_sup}, ?MODULE, []).

-spec start_product_catalog_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_product_catalog_governor(TenantId) ->
    ChildSpec = #{
        id => {product_catalog_governor, TenantId},
        start => {product_catalog_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [product_catalog_governor]
    },
    supervisor:start_child(product_catalog_sup, ChildSpec).

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
