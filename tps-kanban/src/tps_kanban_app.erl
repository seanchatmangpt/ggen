%%%-------------------------------------------------------------------
%% @doc TPS Kanban application startup
%% Supervisor for pull-based work queue system with NATS + RabbitMQ
%% @end
%%%-------------------------------------------------------------------
-module(tps_kanban_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Start application
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    lager:info("Starting TPS Kanban application"),
    case tps_kanban_sup:start_link() of
        {ok, Pid} ->
            lager:info("TPS Kanban started successfully"),
            {ok, Pid};
        Error ->
            lager:error("Failed to start TPS Kanban: ~p", [Error]),
            Error
    end.

%% @doc Stop application
-spec stop(term()) -> ok.
stop(_State) ->
    lager:info("Stopping TPS Kanban application"),
    ok.
