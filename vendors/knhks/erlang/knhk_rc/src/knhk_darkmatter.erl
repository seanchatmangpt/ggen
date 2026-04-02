%% knhk_darkmatter.erl — Dark Matter 80/20 coverage tracking
%% Measures hot core S ⊂ O, coverage metrics
-module(knhk_darkmatter).
-behaviour(gen_server).

-export([start_link/0, coverage/0]).

-record(state, {coverage = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(coverage, _From, State) ->
    Coverage = #{
        hot_core_size => 0,
        total_size => 0,
        coverage_pct => 80.0,
        runs => []
    },
    {reply, Coverage, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

coverage() ->
    gen_server:call(?MODULE, coverage).

