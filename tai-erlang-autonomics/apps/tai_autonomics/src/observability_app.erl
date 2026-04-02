%%%-------------------------------------------------------------------
%% @doc Observability Application
%%      Starts the observability supervision tree
%%      Manages metrics collection, tracing, profiling, and alerts
%% @end
%%%-------------------------------------------------------------------

-module(observability_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%-------------------------------------------------------------------
%% Application Callbacks
%%%-------------------------------------------------------------------

-spec start(application:start_type(), term()) ->
    {ok, pid()} |
    {ok, pid(), term()} |
    {error, term()}.
start(_StartType, _StartArgs) ->
    case observability_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
