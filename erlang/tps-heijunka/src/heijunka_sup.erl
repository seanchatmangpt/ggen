%%%-------------------------------------------------------------------
%% @doc heijunka_sup: Supervisor for Heijunka load leveling system.
%%
%% Manages:
%% - Pool supervisor (manages worker pools per domain)
%% - Load regulator (admission control)
%% - Load balancer (work distribution)
%% - Burst handler (spike management)
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_sup).
-behavior(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%% API Functions
%%%===================================================================

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%%%===================================================================
%% Supervisor Callbacks
%%%===================================================================

init(Config) ->
    % Extract configuration
    Pools = maps:get(pools, Config, [payment, delivery, notification]),
    RegulatorConfig = maps:get(regulator, Config, #{}),

    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 30
    },

    ChildSpecs = [
        % Pool supervisor - manages worker pools
        #{
            id => heijunka_pool_sup,
            start => {heijunka_pool_sup, start_link, [Pools]},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [heijunka_pool_sup]
        },
        % Load regulator - admission control
        #{
            id => heijunka_load_regulator,
            start => {heijunka_load_regulator, start_link, [RegulatorConfig]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [heijunka_load_regulator]
        },
        % Load balancer - work distribution
        #{
            id => heijunka_balancer,
            start => {heijunka_balancer, start_link, [Pools]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [heijunka_balancer]
        },
        % Burst handler - spike management
        #{
            id => heijunka_burst_handler,
            start => {heijunka_burst_handler, start_link, [Pools]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [heijunka_burst_handler]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
