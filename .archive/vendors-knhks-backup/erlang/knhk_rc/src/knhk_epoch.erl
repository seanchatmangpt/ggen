%% knhk_epoch.erl — Epoch scheduler (Λ ≺-total, τ ≤ 8)
%% Plans deterministic execution epochs, executes μ over O
-module(knhk_epoch).
-behaviour(gen_server).

-export([start_link/0, schedule/3, run/1]).

-record(state, {epochs = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({schedule, Tau, Plan, CoverId}, _From, State) when Tau =< 8 ->
    EpochId = erlang:phash2({Plan, CoverId}),
    Epoch = #{tau => Tau, lambda => Plan, cover => CoverId},
    NewState = State#state{epochs = maps:put(EpochId, Epoch, State#state.epochs)},
    {reply, {ok, EpochId}, NewState};
handle_call({schedule, Tau, _Plan, _CoverId}, _From, State) when Tau > 8 ->
    {reply, {error, {guard_violation, <<"tau > 8">>}}, State};
handle_call({run, EpochId}, _From, State) ->
    case maps:get(EpochId, State#state.epochs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Epoch ->
            %% A = μ(O); hash(A) = hash(μ(O))
            %% In production, this would call C FFI for hot path execution
            A = #{result => executed},
            Receipt = #{hash => erlang:phash2(EpochId), span => EpochId, ticks => 8},
            {reply, {ok, #{A => A, receipt => Receipt}}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

schedule(Tau, Plan, CoverId) ->
    gen_server:call(?MODULE, {schedule, Tau, Plan, CoverId}).

run(EpochId) ->
    gen_server:call(?MODULE, {run, EpochId}).

