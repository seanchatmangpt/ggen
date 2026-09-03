%%%-------------------------------------------------------------------
%% @doc cluster_mgr: Erlang cluster management and coordination
%%
%% Manages cluster membership:
%% - Node join/leave coordination
%% - Distributed state synchronization
%% - Consistent hashing for data distribution
%% - Leader election for cluster-wide decisions
%%
%% @end
%%%-------------------------------------------------------------------
-module(cluster_mgr).
-behaviour(gen_server).

%% API
-export([start_link/0, join_cluster/1, leave_cluster/0, get_cluster_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HEARTBEAT_INTERVAL, 5000).
-define(HEARTBEAT_TIMEOUT, 15000).

-record(state, {
    cluster_nodes :: [node()],
    heartbeat_ref :: reference() | undefined,
    is_leader :: boolean(),
    leader_node :: node() | undefined
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the cluster manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Join a cluster via a known node
-spec join_cluster(SeedNode) -> ok | {error, term()}
  when SeedNode :: node().
join_cluster(SeedNode) ->
    gen_server:call(?SERVER, {join_cluster, SeedNode}).

%% @doc Leave the cluster
-spec leave_cluster() -> ok | {error, term()}.
leave_cluster() ->
    gen_server:call(?SERVER, leave_cluster).

%% @doc Get current cluster status
-spec get_cluster_status() -> {ok, Status} | {error, term()}
  when Status :: #{nodes := [node()], is_leader := boolean(), leader := node() | undefined}.
get_cluster_status() ->
    gen_server:call(?SERVER, get_cluster_status).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Start as isolated node
    InitialNodes = [node()],

    %% Schedule heartbeat
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),

    {ok, #state{
        cluster_nodes = InitialNodes,
        heartbeat_ref = HeartbeatRef,
        is_leader = true,
        leader_node = node()
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: #state{}.
handle_call({join_cluster, SeedNode}, _From, State) ->
    case net_adm:ping(SeedNode) of
        pong ->
            %% Node is reachable, join cluster
            ok = net_kernel:monitor_nodes(true),
            NewNodes = lists:usort([node() | State#state.cluster_nodes]),
            {reply, ok, State#state{cluster_nodes = NewNodes}};
        pang ->
            {reply, {error, unreachable}, State}
    end;

handle_call(leave_cluster, _From, State) ->
    net_kernel:monitor_nodes(false),
    {reply, ok, State#state{
        cluster_nodes = [node()],
        is_leader = true,
        leader_node = node()
    }};

handle_call(get_cluster_status, _From, State) ->
    Status = #{
        nodes => State#state.cluster_nodes,
        is_leader => State#state.is_leader,
        leader => State#state.leader_node
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, #state{}}
  when Request :: term(),
       State :: #state{}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, #state{}}
  when Info :: term(),
       State :: #state{}.
handle_info(heartbeat, State) ->
    %% Send heartbeats to cluster nodes
    NewState = do_heartbeat(State),

    %% Reschedule heartbeat
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {noreply, NewState#state{heartbeat_ref = HeartbeatRef}};

handle_info({nodeup, Node}, State) ->
    %% Node joined cluster
    NewNodes = lists:usort([Node | State#state.cluster_nodes]),
    {noreply, State#state{cluster_nodes = NewNodes}};

handle_info({nodedown, Node}, State) ->
    %% Node left cluster
    NewNodes = lists:delete(Node, State#state.cluster_nodes),
    NewLeader = case Node =:= State#state.leader_node of
        true -> elect_leader(NewNodes);
        false -> State#state.leader_node
    end,
    IsLeader = NewLeader =:= node(),
    {noreply, State#state{
        cluster_nodes = NewNodes,
        leader_node = NewLeader,
        is_leader = IsLeader
    }};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: #state{}.
terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false),
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, #state{}}
  when OldVsn :: term() | {down, term()},
       State :: #state{},
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
-spec do_heartbeat(State) -> #state{}
  when State :: #state{}.
do_heartbeat(State) ->
    %% Verify all nodes are still reachable
    VerifyResults = lists:map(
        fun(N) -> {N, net_adm:ping(N)} end,
        State#state.cluster_nodes
    ),

    %% Filter out unreachable nodes
    ReachableNodes = [N || {N, pong} <- VerifyResults],
    NewLeader = case ReachableNodes of
        [] -> node();
        _ -> elect_leader(ReachableNodes)
    end,

    State#state{
        cluster_nodes = ReachableNodes,
        leader_node = NewLeader,
        is_leader = NewLeader =:= node()
    }.

%% @private
-spec elect_leader(Nodes) -> node()
  when Nodes :: [node()].
elect_leader([]) ->
    node();
elect_leader(Nodes) ->
    %% Simple leader election: lowest alphabetical node
    lists:min(Nodes).

%%%===================================================================
%% End of module
%%%===================================================================
