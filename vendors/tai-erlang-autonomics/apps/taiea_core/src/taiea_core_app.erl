%%%-------------------------------------------------------------------
%% @doc taiea_core_app: Application callback module
%%
%% Starts the TAIEA Core Bootstrap application and initializes:
%% - Configuration loading from environment variables
%% - Structured logging setup
%% - Root supervision tree
%%
%% Environment variables:
%%   - PORT: HTTP server port (default: 8080)
%%   - MCP_PORT: MCP server port (default: 3001)
%%   - TAIEA_ENV: Environment (development|staging|production, default: development)
%%   - TAIEA_LOG_LEVEL: Log level (debug|info|warning|error, default: info)
%%   - GCP_PROJECT_ID: GCP project ID (default: taiea-dev)
%%   - FIRESTORE_ENABLED: Enable Firestore (default: true)
%%   - VERIFY_SIGNATURES: Verify JWT signatures (default: false)
%%   - OTEL_ENABLED: Enable OpenTelemetry (default: false)
%%
%% Startup sequence:
%%   1. Load configuration from environment variables
%%   2. Initialize structured logging with kernel logger
%%   3. Start root supervision tree (taiea_core_sup)
%%   4. Emit application startup receipt
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_core_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Configuration API
-export([get_env/1, get_env/2]).

%%%===================================================================
%% Application callbacks
%%%===================================================================

%% @doc Application start callback
%% Initializes configuration, logging, and supervision tree
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, term()}
  when StartType :: normal | {takeover, node()} | {failover, node()},
       StartArgs :: term().
start(_StartType, _StartArgs) ->
    %% Load configuration from environment variables
    ok = load_config(),

    %% Initialize structured logging
    ok = init_logging(),

    %% Log application startup
    logger:notice("TAIEA Core bootstrap starting",
                  #{module => ?MODULE, action => startup}),

    %% Start root supervision tree
    case taiea_core_sup:start_link() of
        {ok, Pid} ->
            logger:notice("TAIEA Core supervision tree started",
                         #{supervisor => taiea_core_sup, pid => Pid}),
            {ok, Pid};
        Error ->
            logger:error("Failed to start TAIEA Core supervision tree",
                        #{error => Error, supervisor => taiea_core_sup}),
            Error
    end.

%% @doc Application stop callback
%% Graceful shutdown - cleanup is delegated to supervisor tree
-spec stop(State) -> ok
  when State :: term().
stop(_State) ->
    logger:notice("TAIEA Core shutting down", #{module => ?MODULE}),
    ok.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
%% @doc Load configuration from environment variables and set application env
-spec load_config() -> ok | {error, term()}.
load_config() ->
    %% HTTP server port
    Port = case os:getenv("PORT") of
        false -> 8080;
        PortStr ->
            case string:to_integer(PortStr) of
                {PortInt, ""} when PortInt > 0, PortInt < 65536 -> PortInt;
                _ -> 8080
            end
    end,
    ok = application:set_env(taiea_core, port, Port),

    %% MCP server port
    McpPort = case os:getenv("MCP_PORT") of
        false -> 3001;
        McpPortStr ->
            case string:to_integer(McpPortStr) of
                {McpPortInt, ""} when McpPortInt > 0, McpPortInt < 65536 -> McpPortInt;
                _ -> 3001
            end
    end,
    ok = application:set_env(taiea_core, mcp_port, McpPort),

    %% Environment (development|staging|production)
    Env = case os:getenv("TAIEA_ENV") of
        false -> development;
        EnvStr ->
            case string:to_lower(EnvStr) of
                "staging" -> staging;
                "production" -> production;
                _ -> development
            end
    end,
    ok = application:set_env(taiea_core, taiea_env, Env),

    %% Log level (debug|info|warning|error)
    LogLevel = case os:getenv("TAIEA_LOG_LEVEL") of
        false -> info;
        LogLevelStr ->
            case string:to_lower(LogLevelStr) of
                "debug" -> debug;
                "warning" -> warning;
                "error" -> error;
                _ -> info
            end
    end,
    ok = application:set_env(taiea_core, taiea_log_level, LogLevel),

    %% GCP Project ID
    GcpProjectId = case os:getenv("GCP_PROJECT_ID") of
        false -> "taiea-dev";
        ProjId -> ProjId
    end,
    ok = application:set_env(taiea_core, gcp_project_id, GcpProjectId),

    %% Firestore enabled
    FirestoreEnabled = case os:getenv("FIRESTORE_ENABLED") of
        false -> true;
        "false" -> false;
        "true" -> true;
        _ -> true
    end,
    ok = application:set_env(taiea_core, firestore_enabled, FirestoreEnabled),

    %% Verify JWT signatures
    VerifySignatures = case os:getenv("VERIFY_SIGNATURES") of
        false -> false;
        "true" -> true;
        "false" -> false;
        _ -> false
    end,
    ok = application:set_env(taiea_core, verify_signatures, VerifySignatures),

    %% OpenTelemetry enabled
    OtelEnabled = case os:getenv("OTEL_ENABLED") of
        false -> false;
        "true" -> true;
        "false" -> false;
        _ -> false
    end,
    ok = application:set_env(taiea_core, otel_enabled, OtelEnabled),

    ok.

%% @private
%% @doc Initialize structured logging with kernel logger
-spec init_logging() -> ok.
init_logging() ->
    LogLevel = case application:get_env(taiea_core, taiea_log_level) of
        {ok, Level} -> Level;
        undefined -> info
    end,

    %% Configure kernel logger handler for console output
    case logger:add_handler(
        default,
        logger_std_h,
        #{
            config => #{
                type => standard_io,
                formatter => {logger_formatter, #{
                    template => [
                        "[",
                        time,
                        "] [",
                        level,
                        "] ",
                        msg,
                        "\n"
                    ],
                    time_designator => $T
                }}
            },
            level => LogLevel
        }
    ) of
        ok ->
            logger:set_primary_config(#{level => LogLevel}),
            logger:notice("Structured logging initialized",
                         #{handler => default, level => LogLevel});
        {error, {already_exist, default}} ->
            %% Handler already exists (e.g., from previous startup), just update level
            logger:set_handler_config(default, level, LogLevel),
            logger:notice("Logger level updated", #{level => LogLevel});
        Error ->
            logger:error("Failed to initialize logging", #{error => Error})
    end,
    ok.

%%%===================================================================
%% Configuration API
%%%===================================================================

%% @doc Get configuration value with default
-spec get_env(Key) -> Value
  when Key :: atom(),
       Value :: term().
get_env(Key) ->
    case application:get_env(taiea_core, Key) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.

%% @doc Get configuration value with explicit default
-spec get_env(Key, Default) -> Value
  when Key :: atom(),
       Default :: term(),
       Value :: term().
get_env(Key, Default) ->
    case application:get_env(taiea_core, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.
