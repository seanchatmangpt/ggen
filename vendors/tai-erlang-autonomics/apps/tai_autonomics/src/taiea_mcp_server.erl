%%%-------------------------------------------------------------------
%%% @doc TAIEA MCP Server - Model Context Protocol integration for TAI Autonomics
%%%
%%% Implements a stdio-based MCP server that exposes four tools:
%%% 1. taiea.health.check - System health status monitoring
%%% 2. taiea.entitlement.apply_event - Apply entitlement state transitions
%%% 3. taiea.receipts.verify_chain - Verify receipt chain integrity
%%% 4. taiea.support.model - Return support model description
%%%
%%% Each tool accepts input, calls governor for decision, emits receipt, returns response.
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_mcp_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([register_tool/3, call_tool/2, get_tools/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type tool_name() :: binary().
-type tool_input() :: map().
-type tool_response() :: map().
-type tool_handler() :: {module(), atom()}.
-type receipt() :: map().

-record(tool, {
    name :: tool_name(),
    schema :: map(),
    handler :: tool_handler()
}).

-record(state, {
    tools = #{} :: #{tool_name() => #tool{}},
    stdio_port :: port() | undefined,
    request_counter = 0 :: non_neg_integer(),
    startup_receipt :: receipt() | undefined
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the MCP server with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the MCP server with options
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, taiea_mcp_server}, ?MODULE, Opts, []).

%% @doc Register a tool with the MCP server
-spec register_tool(tool_name(), map(), tool_handler()) -> ok | {error, term()}.
register_tool(ToolName, Schema, {Module, Function}) ->
    gen_server:call(taiea_mcp_server, {register_tool, ToolName, Schema, {Module, Function}}).

%% @doc Call a registered tool with input
-spec call_tool(tool_name(), tool_input()) -> {ok, tool_response(), receipt()} | {error, term()}.
call_tool(ToolName, Input) ->
    gen_server:call(taiea_mcp_server, {call_tool, ToolName, Input}).

%% @doc Get list of registered tools
-spec get_tools() -> {ok, [#{name => tool_name(), schema => map()}]} | {error, term()}.
get_tools() ->
    gen_server:call(taiea_mcp_server, get_tools).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(list()) -> {ok, state()}.
init(_Opts) ->
    process_flag(trap_exit, true),

    %% Emit MCP server startup receipt
    StartupReceipt = emit_startup_receipt(),

    State = #state{
        tools = #{},
        stdio_port = open_stdio_port(),
        request_counter = 0,
        startup_receipt = StartupReceipt
    },

    %% Register default tools
    {ok, register_default_tools(State)}.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()} | {noreply, state()}.
handle_call({register_tool, ToolName, Schema, Handler}, _From, State) ->
    Tool = #tool{
        name = ToolName,
        schema = Schema,
        handler = Handler
    },

    Tools = State#state.tools,
    UpdatedTools = Tools#{ToolName => Tool},

    {reply, ok, State#state{tools = UpdatedTools}};

handle_call({call_tool, ToolName, Input}, _From, State) ->
    Tools = State#state.tools,

    case maps:get(ToolName, Tools, undefined) of
        undefined ->
            {reply, {error, {tool_not_found, ToolName}}, State};

        Tool ->
            Result = try
                %% Validate input
                case validate_tool_input(Input, Tool#tool.schema) of
                    ok ->
                        %% Call tool handler
                        Handler = Tool#tool.handler,
                        case call_handler(Handler, Input) of
                            {ok, Response, ToolReceipt} ->
                                NewCounter = State#state.request_counter + 1,
                                {reply, {ok, Response, ToolReceipt}, State#state{request_counter = NewCounter}};

                            {error, Reason} ->
                                {reply, {error, Reason}, State}
                        end;

                    {error, ValidationError} ->
                        {reply, {error, ValidationError}, State}
                end
            catch
                ErrorClass:ErrorReason ->
                    Error = {handler_exception, ErrorClass, ErrorReason},
                    {reply, {error, Error}, State}
            end,
            Result
    end;

handle_call(get_tools, _From, State) ->
    Tools = State#state.tools,
    ToolList = maps:fold(
        fun(ToolName, Tool, Acc) ->
            [#{
                name => ToolName,
                schema => Tool#tool.schema
            } | Acc]
        end,
        [],
        Tools
    ),
    {reply, {ok, ToolList}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.stdio_port of
        undefined -> ok;
        Port -> catch erlang:port_close(Port)
    end.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Register default tools on server startup
-spec register_default_tools(state()) -> state().
register_default_tools(State) ->
    State2 = register_tool_internal(
        State,
        <<"taiea.health.check">>,
        #{
            description => <<"Check system health status">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{}
            }
        },
        {taiea_tool_health, handle}
    ),

    State3 = register_tool_internal(
        State2,
        <<"taiea.entitlement.apply_event">>,
        #{
            description => <<"Apply entitlement event and state transition">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    <<"tenant_id">> => #{type => <<"string">>, description => <<"Tenant identifier">>},
                    <<"event_type">> => #{type => <<"string">>, description => <<"Event type (e.g., provision, deprovision)">>},
                    <<"event_data">> => #{type => <<"object">>, description => <<"Event metadata">>}
                },
                required => [<<"tenant_id">>, <<"event_type">>]
            }
        },
        {taiea_tool_entitlement, handle}
    ),

    State4 = register_tool_internal(
        State3,
        <<"taiea.receipts.verify_chain">>,
        #{
            description => <<"Verify receipt chain integrity">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    <<"tenant_id">> => #{type => <<"string">>, description => <<"Tenant identifier">>},
                    <<"receipt_id">> => #{type => <<"string">>, description => <<"Receipt to verify">>}
                },
                required => [<<"tenant_id">>, <<"receipt_id">>]
            }
        },
        {taiea_tool_receipts, handle}
    ),

    State5 = register_tool_internal(
        State4,
        <<"taiea.support.model">>,
        #{
            description => <<"Get support model description and configuration">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{}
            }
        },
        {taiea_tool_support, handle}
    ),

    State5.

%% @private Register a single tool
-spec register_tool_internal(state(), tool_name(), map(), tool_handler()) -> state().
register_tool_internal(State, ToolName, Schema, Handler) ->
    Tool = #tool{
        name = ToolName,
        schema = Schema,
        handler = Handler
    },

    Tools = State#state.tools,
    UpdatedTools = Tools#{ToolName => Tool},
    State#state{tools = UpdatedTools}.

%% @private Call tool handler function
-spec call_handler(tool_handler(), tool_input()) -> {ok, tool_response(), receipt()} | {error, term()}.
call_handler({Module, Function}, Input) ->
    try
        Module:Function(Input)
    catch
        Class:Reason:Stacktrace ->
            error_logger:error_msg(
                "Tool handler error: ~w:~w(~w) - ~w:~w~nStacktrace: ~p",
                [Module, Function, Input, Class, Reason, Stacktrace]
            ),
            {error, {handler_error, Class, Reason}}
    end.

%% @private Validate tool input against schema
-spec validate_tool_input(tool_input(), map()) -> ok | {error, term()}.
validate_tool_input(_Input, #{}) ->
    %% Empty schema - accept anything
    ok;
validate_tool_input(Input, Schema) ->
    case maps:get(<<"required">>, Schema, []) of
        [] ->
            ok;
        Required ->
            case validate_required_fields(Input, Required) of
                ok -> ok;
                Error -> Error
            end
    end.

%% @private Validate required fields are present in input
-spec validate_required_fields(tool_input(), [binary()]) -> ok | {error, term()}.
validate_required_fields(_Input, []) ->
    ok;
validate_required_fields(Input, [Field | Rest]) ->
    case maps:is_key(Field, Input) of
        true ->
            validate_required_fields(Input, Rest);
        false ->
            {error, {missing_required_field, Field}}
    end.

%% @private Open stdio port for MCP communication
-spec open_stdio_port() -> port() | undefined.
open_stdio_port() ->
    case application:get_env(taiea_mcp_server, stdio_enabled, false) of
        true ->
            try
                erlang:open_port({spawn, "cat"}, [binary, stream])
            catch
                _:_ -> undefined
            end;
        false ->
            undefined
    end.

%% @private Emit receipt for MCP server startup
-spec emit_startup_receipt() -> receipt().
emit_startup_receipt() ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        event => <<"mcp_server_startup">>,
        status => <<"success">>,
        tools_registered => 4,
        message => <<"TAIEA MCP server started successfully">>,
        version => <<"1.0.0">>,
        metadata => #{
            server_name => <<"taiea_mcp_server">>,
            protocol => <<"stdio">>,
            tools => [
                <<"taiea.health.check">>,
                <<"taiea.entitlement.apply_event">>,
                <<"taiea.receipts.verify_chain">>,
                <<"taiea.support.model">>
            ]
        }
    }.

%% @private Generate unique receipt ID
-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @private Get current timestamp in milliseconds
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).
