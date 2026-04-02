%%%-------------------------------------------------------------------
%% @doc TPS Kanban supervisor - manages workers, queue, coordinator
%% Implements supervisor strategy: one_for_one for independent components
%% @end
%%%-------------------------------------------------------------------
-module(tps_kanban_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc Start supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize supervisor with worker and coordinator specs
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                   {error, term()}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    ChildSpecs = [
        #{
            id => kanban_queue,
            start => {kanban_queue, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [kanban_queue]
        },
        #{
            id => kanban_rabbitmq_fallback,
            start => {kanban_rabbitmq_fallback, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [kanban_rabbitmq_fallback]
        },
        #{
            id => kanban_coordinator,
            start => {kanban_coordinator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [kanban_coordinator]
        },
        #{
            id => kanban_worker_sup,
            start => {kanban_worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [kanban_worker_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
