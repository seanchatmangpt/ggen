%%%-------------------------------------------------------------------
%%% @doc Telecom Application - OTP Application Behavior
%%%
%%% Entry point for the telecom system OTP application.
%%% Starts the top-level supervisor and manages application lifecycle.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(telecom_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Start the telecom application.
start(_StartType, _StartArgs) ->
    io:format("Starting Telecom Application...~n"),
    case telecom_sup:start_link() of
        {ok, Pid} ->
            io:format("Telecom Application started successfully~n"),
            {ok, Pid};
        Error ->
            io:format("Failed to start Telecom Application: ~p~n", [Error]),
            Error
    end.

%% @doc Stop the telecom application.
stop(_State) ->
    io:format("Stopping Telecom Application...~n"),
    ok.
