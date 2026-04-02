%%%-------------------------------------------------------------------
%% @doc heijunka_worker: Worker process for Poolboy pools.
%%
%% Generic worker that can handle various types of work.
%% Used by heijunka_pool via Poolboy for worker pool management.
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_worker).
-behavior(gen_server).

%% API
-export([
    start_link/1,
    execute/2,
    ping/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(worker_state, {
    id :: binary(),
    pool_name :: atom(),
    last_activity :: integer(),
    requests_processed = 0 :: integer()
}).

%%%===================================================================
%% API Functions
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

execute(WorkerPid, Work) ->
    gen_server:call(WorkerPid, {execute, Work}).

ping(WorkerPid) ->
    gen_server:call(WorkerPid, ping).

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================

init(Args) ->
    WorkerId = crypto:strong_rand_bytes(8),
    PoolName = proplists:get_value(pool_name, Args, undefined),

    State = #worker_state{
        id = WorkerId,
        pool_name = PoolName,
        last_activity = erlang:system_time(millisecond)
    },

    {ok, State}.

handle_call(ping, _From, State) ->
    {reply, {pong, State#worker_state.id}, State#worker_state{
        last_activity = erlang:system_time(millisecond)
    }};
handle_call({execute, _Work}, _From, State) ->
    % Simulate work execution
    NewState = State#worker_state{
        last_activity = erlang:system_time(millisecond),
        requests_processed = State#worker_state.requests_processed + 1
    },
    {reply, {ok, executed}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
