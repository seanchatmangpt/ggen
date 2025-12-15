%% knhk_route.erl — Action routing (A ports)
%% Routes actions A to outputs
-module(knhk_route).
-behaviour(gen_server).

-export([start_link/0, install/4]).

-record(state, {routes = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({install, Name, Kind, Target, Codec}, _From, State) ->
    RouteId = erlang:phash2(Name),
    Route = #{name => Name, kind => Kind, target => Target, encode => Codec},
    NewState = State#state{routes = maps:put(RouteId, Route, State#state.routes)},
    {reply, {ok, RouteId}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

install(Name, Kind, Target, Codec) ->
    gen_server:call(?MODULE, {install, Name, Kind, Target, Codec}).

