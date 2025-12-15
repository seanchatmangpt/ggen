%% knhk_cover.erl — Cover definitions (Γ glue over O)
%% Selects S ⊂ O, shards to runs len ≤ 8
-module(knhk_cover).
-behaviour(gen_server).

-export([start_link/0, define/2]).

-record(state, {covers = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({define, SelectSpec, ShardSpec}, _From, State) ->
    CoverId = erlang:phash2(SelectSpec),
    Cover = #{select => SelectSpec, shard => ShardSpec},
    NewState = State#state{covers = maps:put(CoverId, Cover, State#state.covers)},
    {reply, {ok, CoverId}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

define(SelectSpec, ShardSpec) ->
    gen_server:call(?MODULE, {define, SelectSpec, ShardSpec}).

