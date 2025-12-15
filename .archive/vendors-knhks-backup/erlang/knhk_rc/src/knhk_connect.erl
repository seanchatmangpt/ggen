%% knhk_connect.erl — Connector registry (Dark Matter 80/20)
%% Registers typed sources (Σ-mapped) for Δ ingestion
-module(knhk_connect).
-behaviour(gen_server).

-export([start_link/0, register/5]).

-record(state, {connectors = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({register, Name, SigmaIri, Src, Map, Guard}, _From, State) ->
    ConnId = erlang:phash2(Name),
    Conn = #{name => Name, schema => SigmaIri, source => Src, map => Map, guard => Guard},
    NewState = State#state{connectors = maps:put(ConnId, Conn, State#state.connectors)},
    {reply, {ok, ConnId}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

register(Name, SigmaIri, Src, Map, Guard) ->
    gen_server:call(?MODULE, {register, Name, SigmaIri, Src, Map, Guard}).

