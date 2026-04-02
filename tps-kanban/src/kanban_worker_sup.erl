%%%-------------------------------------------------------------------
%% @doc Kanban Worker Supervisor
%% Manages dynamic pool of workers that pull from queue
%% Implements simple_one_for_one strategy for easy scaling
%% @end
%%%-------------------------------------------------------------------
-module(kanban_worker_sup).
-behaviour(supervisor).

-export([start_link/0, start_worker/2, stop_worker/1, init/1]).

%%%-------------------------------------------------------------------
%% @doc Start supervisor
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Start new worker
-spec start_worker(atom(), atom()) -> {ok, pid()} | {error, term()}.
start_worker(Priority, Domain) ->
    supervisor:start_child(?MODULE, [Priority, Domain]).

%% @doc Stop worker gracefully
-spec stop_worker(pid()) -> ok | {error, term()}.
stop_worker(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },

    ChildSpec = #{
        id => kanban_worker,
        start => {kanban_worker, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [kanban_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.
