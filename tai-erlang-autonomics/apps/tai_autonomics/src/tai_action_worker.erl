%%%-------------------------------------------------------------------
%% @doc tai_action_worker: Poolboy worker for executing actions
%%
%% Worker process that executes actions from the action pool.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_action_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%% API
%%%===================================================================

-spec start_link(Args) -> {ok, pid()} | {error, term()}
    when Args :: list().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    {ok, #state{}}.

handle_call({execute, ActionSpec}, _From, State) ->
    %% Execute the action
    Result = execute_action(ActionSpec),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec execute_action(ActionSpec) -> map()
    when ActionSpec :: map().
execute_action(ActionSpec) ->
    %% Extract action type and parameters
    ActionType = maps:get(<<"type">>, ActionSpec, <<"unknown">>),
    Params = maps:get(<<"params">>, ActionSpec, #{}),
    
    %% Execute based on action type
    case ActionType of
        <<"scale">> ->
            execute_scale_action(Params);
        <<"rollback">> ->
            execute_rollback_action(Params);
        <<"throttle">> ->
            execute_throttle_action(Params);
        _ ->
            #{error => <<"unknown_action_type">>}
    end.

-spec execute_scale_action(Params) -> map()
    when Params :: map().
execute_scale_action(Params) ->
    %% Simulate scale action
    #{result => <<"scaled">>, params => Params}.

-spec execute_rollback_action(Params) -> map()
    when Params :: map().
execute_rollback_action(Params) ->
    %% Simulate rollback action
    #{result => <<"rolled_back">>, params => Params}.

-spec execute_throttle_action(Params) -> map()
    when Params :: map().
execute_throttle_action(Params) ->
    %% Simulate throttle action
    #{result => <<"throttled">>, params => Params}.
