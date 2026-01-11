%% knhk_hooks.erl — Hook registry (μ-hot ops)
%% Installs reflexes as knowledge, not code
-module(knhk_hooks).
-behaviour(gen_server).

-export([start_link/0, install/7]).

-record(state, {hooks = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({install, Name, Op, P, Off, Len, Args, EpochTag}, _From, State) when Len =< 8 ->
    HookId = erlang:phash2(Name),
    Hook = #{name => Name, op => Op, run => #{pred => P, off => Off, len => Len},
             args => Args, epoch => EpochTag},
    NewState = State#state{hooks = maps:put(HookId, Hook, State#state.hooks)},
    {reply, {ok, HookId}, NewState};
handle_call({install, _Name, _Op, _P, _Off, Len, _Args, _EpochTag}, _From, State) when Len > 8 ->
    {reply, {error, {guard_violation, <<"run.len > 8">>}}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

install(Name, Op, P, Off, Len, Args, EpochTag) ->
    gen_server:call(?MODULE, {install, Name, Op, P, Off, Len, Args, EpochTag}).

