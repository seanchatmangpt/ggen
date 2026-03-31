%%%-------------------------------------------------------------------
%% @doc Observability Supervisor
%%      Manages the supervision tree for all observability modules
%% @end
%%%-------------------------------------------------------------------

-module(observability_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% API Functions
%%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%-------------------------------------------------------------------
%% Supervisor Callbacks
%%%-------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        %% Metrics Collector (started first, other modules depend on it)
        #{
            id => metrics_collector,
            start => {metrics_collector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Trace Handler
        #{
            id => trace_handler,
            start => {trace_handler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Profiler
        #{
            id => profiler,
            start => {profiler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Alert Manager
        #{
            id => alert_manager,
            start => {alert_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Observer UI
        #{
            id => observer_ui,
            start => {observer_ui, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
