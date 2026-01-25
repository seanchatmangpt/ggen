%%%-------------------------------------------------------------------
%% @doc heijunka_pool_sup: Supervisor for worker pools.
%%
%% Manages multiple Poolboy pools, one per domain (payment, delivery, etc.)
%% Each pool is independent with its own scaling configuration.
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_pool_sup).
-behavior(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%% API Functions
%%%===================================================================

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Pools).

%%%===================================================================
%% Supervisor Callbacks
%%%===================================================================

init(Pools) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 30
    },

    ChildSpecs = [create_pool_spec(Pool) || Pool <- Pools],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @doc Create pool child spec for a domain.
-spec create_pool_spec(atom()) -> supervisor:child_spec().
create_pool_spec(PoolName) ->
    PoolConfig = get_pool_config(PoolName),

    #{
        id => PoolName,
        start => {heijunka_pool, start_link, [PoolName, PoolConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [heijunka_pool]
    }.

%% @doc Get configuration for a specific pool.
-spec get_pool_config(atom()) -> map().
get_pool_config(payment) ->
    #{
        domain => payment,
        initial_workers => 10,
        max_workers => 30
    };
get_pool_config(delivery) ->
    #{
        domain => delivery,
        initial_workers => 8,
        max_workers => 25
    };
get_pool_config(notification) ->
    #{
        domain => notification,
        initial_workers => 5,
        max_workers => 20
    };
get_pool_config(Domain) ->
    % Default configuration
    #{
        domain => Domain,
        initial_workers => 5,
        max_workers => 20
    }.
