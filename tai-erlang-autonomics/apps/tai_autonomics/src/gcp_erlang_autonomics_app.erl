%%%-------------------------------------------------------------------
%% @doc gcp_erlang_autonomics_app: Application callback module
%%
%% Starts the GCP Erlang Autonomics application and its supervision tree.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_erlang_autonomics_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%% Application callbacks
%%%===================================================================

%% @doc Application start callback
%%
%% Starts the root supervisor (autonomics_sup) which in turn starts
%% the entire supervision tree for GCP Erlang Autonomics.
%%
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, term()}
  when StartType :: normal | {takeover, node()} | {failover, node()},
       StartArgs :: term().
start(_StartType, _StartArgs) ->
    case autonomics_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Application stop callback
%%
%% Gracefully stops the application.
%% The supervision tree shutdown is handled by the supervisor's
%% shutdown behavior (5 seconds timeout).
%%
-spec stop(State) -> ok
  when State :: term().
stop(_State) ->
    ok.

%%%===================================================================
%% End of module
%%%===================================================================
