%%%-------------------------------------------------------------------
%% @doc atomvm_governors_sup - Root supervisor for AtomVM governors
%%
%% Minimal supervision tree optimized for edge deployment.
%% Simple one_for_one strategy: each governor is independent.
%%
%% Memory efficient: no complex hierarchies, direct children only.
%%
%% @end
%%%-------------------------------------------------------------------
-module(atomvm_governors_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the root supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize supervisor with minimal overhead
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    % Minimal supervisor flags for edge deployment
    SupFlags = #{
        strategy => one_for_one,  % Independent failure domains
        intensity => 10,           % 10 restarts
        period => 60              % Per 60 seconds
    },

    % Child specs: minimal governor set
    % Can be extended based on deployment needs
    ChildSpecs = [
        % Entitlement governor
        #{
            id => entitlement_gov,
            start => {light_governors, start_link, [entitlement]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [light_governors]
        },

        % Billing governor
        #{
            id => billing_gov,
            start => {light_governors, start_link, [billing]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [light_governors]
        },

        % Quota governor
        #{
            id => quota_gov,
            start => {light_governors, start_link, [quota]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [light_governors]
        },

        % Compliance governor
        #{
            id => compliance_gov,
            start => {light_governors, start_link, [compliance]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [light_governors]
        },

        % Subscription governor
        #{
            id => subscription_gov,
            start => {light_governors, start_link, [subscription]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [light_governors]
        },

        % Memory pool manager
        #{
            id => memory_pool_mgr,
            start => {memory_pool, start_link, []},
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => [memory_pool]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Helper functions
%%%===================================================================

%% @doc Dynamically add a new governor (for scaling up)
-spec add_governor(atom(), atom()) -> {ok, pid()} | {error, term()}.
add_governor(Id, Type) ->
    ChildSpec = #{
        id => Id,
        start => {light_governors, start_link, [Type]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [light_governors]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc Remove a governor (for scaling down)
-spec remove_governor(atom()) -> ok | {error, term()}.
remove_governor(Id) ->
    case supervisor:terminate_child(?SERVER, Id) of
        ok -> supervisor:delete_child(?SERVER, Id);
        Error -> Error
    end.

