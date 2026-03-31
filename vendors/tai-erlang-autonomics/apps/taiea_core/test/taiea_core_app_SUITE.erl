%%%-------------------------------------------------------------------
%% @doc taiea_core_app Common Test suite
%%
%% Tests configuration API and module structure
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_core_app_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test suite callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([test_config_defaults/1,
         test_config_api_get_env/1]).

-spec test_config_defaults(_) -> ok.
-spec test_config_api_get_env(_) -> ok.

%%%===================================================================
%% Test suite callbacks
%%%===================================================================

%% @doc Test suite configuration
all() ->
    [test_config_defaults,
     test_config_api_get_env].

%% @doc Initialize test suite
init_per_suite(Config) ->
    ct:log("Initializing taiea_core_app_SUITE", []),
    %% Ensure app is loaded
    case application:load(taiea_core) of
        ok -> ok;
        {error, {already_loaded, taiea_core}} -> ok;
        {error, Reason} ->
            ct:log("Failed to load taiea_core: ~w", [Reason])
    end,
    Config.

%% @doc Cleanup after test suite
end_per_suite(_Config) ->
    ct:log("Ending taiea_core_app_SUITE", []),
    ok.

%% @doc Initialize before each test case
init_per_testcase(_TestCase, Config) ->
    Config.

%% @doc Cleanup after each test case
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%% Test cases
%%%===================================================================

%% @doc Test that configuration defaults are set correctly
test_config_defaults(_Config) ->
    ct:log("Test: Configuration defaults", []),

    %% Check application env has correct defaults
    case application:get_env(taiea_core, port) of
        {ok, Port} when is_integer(Port), Port > 0, Port < 65536 ->
            ct:log("Default port configured: ~w", [Port]);
        {ok, BadPort} ->
            ct:fail("Invalid default port: ~w", [BadPort]);
        undefined ->
            ct:log("Using hardcoded default port: 8080")
    end,

    case application:get_env(taiea_core, mcp_port) of
        {ok, McpPort} when is_integer(McpPort), McpPort > 0, McpPort < 65536 ->
            ct:log("Default MCP port configured: ~w", [McpPort]);
        {ok, BadMcpPort} ->
            ct:fail("Invalid default MCP port: ~w", [BadMcpPort]);
        undefined ->
            ct:log("Using hardcoded default MCP port: 3001")
    end,

    case application:get_env(taiea_core, taiea_env) of
        {ok, Env} when Env =:= development; Env =:= staging; Env =:= production ->
            ct:log("Environment configured: ~w", [Env]);
        {ok, BadEnv} ->
            ct:fail("Invalid environment: ~w", [BadEnv]);
        undefined ->
            ct:log("Using hardcoded default environment: development")
    end,

    ok.

%% @doc Test configuration API functions
test_config_api_get_env(_Config) ->
    ct:log("Test: Configuration API get_env", []),

    %% Test get_env/2 returns integers for ports
    Port = taiea_core_app:get_env(port, 8080),
    ct:log("get_env(port, 8080) returns: ~w", [Port]),
    true = is_integer(Port),

    McpPort = taiea_core_app:get_env(mcp_port, 3001),
    ct:log("get_env(mcp_port, 3001) returns: ~w", [McpPort]),
    true = is_integer(McpPort),

    %% Test get_env/2 returns defaults for unknown keys
    UnknownValue = taiea_core_app:get_env(nonexistent_key, my_default),
    ct:log("get_env(nonexistent_key, my_default) returns: ~w", [UnknownValue]),
    my_default = UnknownValue,

    %% Test get_env/1 returns undefined for unknown keys
    UndefinedValue = taiea_core_app:get_env(another_unknown_key),
    ct:log("get_env(another_unknown_key) returns: ~w", [UndefinedValue]),
    undefined = UndefinedValue,

    ok.
