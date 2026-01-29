%% ============================================================================
%% HTTP Server Supervisor
%% ============================================================================
%% Provides automatic restart if HTTP server crashes

-module(http_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1]).

%% ============================================================================
%% API
%% ============================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ============================================================================
%% Supervisor Callbacks
%% ============================================================================

init([Port]) ->
    %% Supervisor flags
    SupFlags = #{
        strategy => one_for_one,     % If child dies, restart only that child
        intensity => 5,              % Max 5 restarts
        period => 60                 % In 60 seconds
    },

    %% Child specification
    ChildSpecs = [
        #{
            id => http_server,
            start => {http_server, start_link, [Port]},
            restart => permanent,     % Always restart
            shutdown => 5000,         % 5 seconds to shutdown
            type => worker,
            modules => [http_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% ============================================================================
%% Supervision Tree
%% ============================================================================
%%
%%     http_sup (supervisor)
%%         |
%%         └─ http_server (gen_server on port 8080)
%%
%% If http_server crashes:
%% 1. Supervisor detects crash
%% 2. Supervisor waits briefly
%% 3. Supervisor restarts http_server
%% 4. System recovers automatically
%%
%% This is the "let it crash" philosophy in action.
%% ============================================================================
