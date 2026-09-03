%%%-------------------------------------------------------------------
%% @doc tai_autonomics_app: Application callback module
%%
%% Starts the TAI Erlang Autonomics application and its supervision tree.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_autonomics_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%% Application callbacks
%%%===================================================================

%% @doc Application start callback
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, term()}
  when StartType :: normal | {takeover, node()} | {failover, node()},
       StartArgs :: term().
start(_StartType, _StartArgs) ->
    case tai_autonomics_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Application stop callback
-spec stop(State) -> ok
  when State :: term().
stop(_State) ->
    ok.
