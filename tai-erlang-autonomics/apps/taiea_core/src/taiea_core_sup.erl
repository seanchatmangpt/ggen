%%%-------------------------------------------------------------------
%% @doc taiea_core_sup: Root supervisor for TAIEA Core
%%
%% Supervision tree structure:
%%
%%   taiea_core_sup (root, one_for_all)
%%   ├── taiea_http_server (gen_server, will be started by Agent 4)
%%   └── taiea_mcp_server (gen_server, will be started by Agent 5)
%%
%% Restart strategy: one_for_all
%%   - If any critical child crashes, entire system restarts
%%   - Maximum 5 restarts per 60 seconds
%%   - If restart limit exceeded, supervisor crashes and propagates up
%%
%% Shutdown timeout: 5 seconds graceful shutdown
%%   - Each child gets 5 seconds to terminate cleanly
%%   - After timeout, child is forcefully killed
%%
%% Note: Child specifications are empty (ready for agents to inject HTTP/MCP servers)
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_core_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart strategy constants
-define(RESTART_INTENSITY, 5).        % Max 5 restarts
-define(RESTART_PERIOD, 60).           % Per 60 seconds
-define(SHUTDOWN_TIMEOUT, 5000).       % 5 seconds graceful shutdown

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the root supervisor
%% Initializes the supervision tree and prepares for child processes
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize supervisor with empty child spec list
%%
%% Supervisor tree is ready for child processes to be added:
%% - Agent 4 will inject HTTP server child spec
%% - Agent 5 will inject MCP server child spec
%%
%% The supervisor itself is fully functional and can accept dynamic children.
%%
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    logger:notice("TAIEA Core supervisor initializing",
                  #{module => ?MODULE, action => init}),

    %% Root supervisor flags: one_for_all strategy
    %% If any child crashes, restart all children (promotes reliability)
    SupFlags = #{
        strategy => one_for_all,
        intensity => ?RESTART_INTENSITY,
        period => ?RESTART_PERIOD
    },

    %% Child specifications - empty for now
    %% Agent 4 (HTTP) and Agent 5 (MCP) will add their children dynamically
    %% using supervisor:start_child/2 or via application dependency startup
    ChildSpecs = [
        %% HTTP server child spec will be added by Agent 4
        %% Example (DO NOT INCLUDE YET):
        %% #{
        %%     id => taiea_http_server,
        %%     start => {taiea_http_server, start_link, []},
        %%     restart => permanent,
        %%     shutdown => ?SHUTDOWN_TIMEOUT,
        %%     type => worker,
        %%     modules => [taiea_http_server]
        %% },

        %% MCP server child spec will be added by Agent 5
        %% Example (DO NOT INCLUDE YET):
        %% #{
        %%     id => taiea_mcp_server,
        %%     start => {taiea_mcp_server, start_link, []},
        %%     restart => permanent,
        %%     shutdown => ?SHUTDOWN_TIMEOUT,
        %%     type => worker,
        %%     modules => [taiea_mcp_server]
        %% }
    ],

    logger:notice("TAIEA Core supervisor initialized",
                  #{module => ?MODULE, children => length(ChildSpecs)}),

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Helper functions
%%%===================================================================

%% @doc Child spec for HTTP server (for reference - will be injected by Agent 4)
%% @private
%% @doc Note: These functions are documented for reference and will be used by agents
-compile([{nowarn_unused_function, [{http_server_spec, 0}, {mcp_server_spec, 0}]}]).

http_server_spec() ->
    #{
        id => taiea_http_server,
        start => {taiea_http_server, start_link, []},
        restart => permanent,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [taiea_http_server]
    }.

%% @doc Child spec for MCP server (for reference - will be injected by Agent 5)
%% @private
mcp_server_spec() ->
    #{
        id => taiea_mcp_server,
        start => {taiea_mcp_server, start_link, []},
        restart => permanent,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [taiea_mcp_server]
    }.
