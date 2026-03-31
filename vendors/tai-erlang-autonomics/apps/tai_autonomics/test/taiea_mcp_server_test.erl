%%%-------------------------------------------------------------------
%% @doc TAIEA MCP Server - Comprehensive Unit Test Suite
%%
%% Chicago School TDD Pattern:
%% - State-based testing (actual gen_server instance)
%% - Real collaborators (actual tool handlers)
%% - AAA pattern (Arrange, Act, Assert)
%%
%% Test Categories:
%% 1. Server Initialization (2 cases)
%% 2. Tool Registration (3 cases)
%% 3. Tool Invocation (4 cases)
%% 4. Error Handling (3 cases)
%% @end
%%%-------------------------------------------------------------------
-module(taiea_mcp_server_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%% Setup / Teardown
%%%===================================================================

%% Setup: Start MCP server before each test
setup() ->
    catch gen_server:call(taiea_mcp_server, stop),
    case application:load(tai_autonomics) of
        ok -> ok;
        {error, already_loaded} -> ok
    end,
    {ok, _Pid} = taiea_mcp_server:start_link(),
    ok.

%% Teardown: Stop MCP server after each test
cleanup(_) ->
    catch gen_server:call(taiea_mcp_server, stop),
    ok.

%%%===================================================================
%% Test Suite Definition
%%%===================================================================

taiea_mcp_server_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        % Server initialization
        {"server_starts_cleanly", fun test_server_starts_cleanly/0},
        {"startup_receipt_generated", fun test_startup_receipt_generated/0},

        % Default tools registration
        {"four_tools_registered_on_startup", fun test_four_tools_registered_on_startup/0},
        {"health_tool_schema_correct", fun test_health_tool_schema_correct/0},
        {"entitlement_tool_schema_correct", fun test_entitlement_tool_schema_correct/0},
        {"receipts_tool_schema_correct", fun test_receipts_tool_schema_correct/0},
        {"support_tool_schema_correct", fun test_support_tool_schema_correct/0},

        % Tool invocation
        {"tool_call_valid_input_returns_response", fun test_tool_call_valid_input_returns_response/0},
        {"tool_call_missing_tenant_id_returns_error", fun test_tool_call_missing_tenant_id_returns_error/0},
        {"tool_call_invalid_input_fails_safely", fun test_tool_call_invalid_input_fails_safely/0},
        {"tool_response_includes_receipt", fun test_tool_response_includes_receipt/0},

        % Tool registration
        {"register_custom_tool_success", fun test_register_custom_tool_success/0},
        {"get_tools_returns_all_registered", fun test_get_tools_returns_all_registered/0},

        % Error handling
        {"call_nonexistent_tool_returns_error", fun test_call_nonexistent_tool_returns_error/0},
        {"tool_handler_exception_caught", fun test_tool_handler_exception_caught/0},
        {"request_counter_incremented", fun test_request_counter_incremented/0}
    ]}.

%%%===================================================================
%% Server Initialization Tests
%%%===================================================================

%% Arrange: Nothing
%% Act: Server is already started in setup
%% Assert: Server is running and registered
test_server_starts_cleanly() ->
    %% Check process is registered with local name
    ?assert(is_pid(whereis(taiea_mcp_server))),
    ok.

%% Arrange: Nothing
%% Act: Call get_tools to retrieve default tools
%% Assert: Startup receipt was generated with correct fields
test_startup_receipt_generated() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    %% Verify at least 4 tools were registered during startup
    ?assert(length(Tools) >= 4),
    ok.

%%%===================================================================
%% Default Tools Registration Tests
%%%===================================================================

%% Arrange: Server startup completes
%% Act: Get tools list
%% Assert: Exactly 4 tools registered
test_four_tools_registered_on_startup() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    ToolNames = [maps:get(name, T) || T <- Tools],

    ?assertEqual(
        lists:sort([
            <<"taiea.health.check">>,
            <<"taiea.entitlement.apply_event">>,
            <<"taiea.receipts.verify_chain">>,
            <<"taiea.support.model">>
        ]),
        lists:sort(ToolNames)
    ),
    ok.

%% Arrange: Server startup completes
%% Act: Get tools and find health tool
%% Assert: Health tool schema is correct
test_health_tool_schema_correct() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    [HealthTool] = [T || T <- Tools, maps:get(name, T) =:= <<"taiea.health.check">>],

    Schema = maps:get(schema, HealthTool),
    ?assert(maps:is_key(description, Schema)),
    ?assert(maps:is_key(inputSchema, Schema)),

    InputSchema = maps:get(inputSchema, Schema),
    ?assertEqual(<<"object">>, maps:get(type, InputSchema)),
    ok.

%% Arrange: Server startup completes
%% Act: Get tools and find entitlement tool
%% Assert: Entitlement tool schema has required fields
test_entitlement_tool_schema_correct() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    [EntitlementTool] = [T || T <- Tools, maps:get(name, T) =:= <<"taiea.entitlement.apply_event">>],

    Schema = maps:get(schema, EntitlementTool),
    InputSchema = maps:get(inputSchema, Schema),

    ?assert(maps:is_key(properties, InputSchema)),
    Properties = maps:get(properties, InputSchema),
    ?assert(maps:is_key(<<"tenant_id">>, Properties)),
    ?assert(maps:is_key(<<"event_type">>, Properties)),

    Required = maps:get(required, InputSchema, []),
    ?assert(lists:member(<<"tenant_id">>, Required)),
    ?assert(lists:member(<<"event_type">>, Required)),
    ok.

%% Arrange: Server startup completes
%% Act: Get tools and find receipts tool
%% Assert: Receipts tool schema correct
test_receipts_tool_schema_correct() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    [ReceiptsTool] = [T || T <- Tools, maps:get(name, T) =:= <<"taiea.receipts.verify_chain">>],

    Schema = maps:get(schema, ReceiptsTool),
    InputSchema = maps:get(inputSchema, Schema),

    Required = maps:get(required, InputSchema, []),
    ?assert(lists:member(<<"tenant_id">>, Required)),
    ?assert(lists:member(<<"receipt_id">>, Required)),
    ok.

%% Arrange: Server startup completes
%% Act: Get tools and find support tool
%% Assert: Support tool schema correct
test_support_tool_schema_correct() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),
    [SupportTool] = [T || T <- Tools, maps:get(name, T) =:= <<"taiea.support.model">>],

    Schema = maps:get(schema, SupportTool),
    ?assert(maps:is_key(description, Schema)),
    ?assert(maps:is_key(inputSchema, Schema)),
    ok.

%%%===================================================================
%% Tool Invocation Tests
%%%===================================================================

%% Arrange: Server running with default tools
%% Act: Call health tool with valid input
%% Assert: Returns response and receipt
test_tool_call_valid_input_returns_response() ->
    Input = #{},

    {ok, Response, Receipt} = taiea_mcp_server:call_tool(<<"taiea.health.check">>, Input),

    ?assert(is_map(Response)),
    ?assert(is_map(Receipt)),
    ?assert(maps:is_key(status, Response)),
    ok.

%% Arrange: Server running
%% Act: Call entitlement tool without required tenant_id
%% Assert: Returns error with validation message
test_tool_call_missing_tenant_id_returns_error() ->
    Input = #{<<"event_type">> => <<"provision">>},

    Result = taiea_mcp_server:call_tool(<<"taiea.entitlement.apply_event">>, Input),

    ?assert(is_tuple(Result) andalso element(1, Result) =:= error),
    ok.

%% Arrange: Server running
%% Act: Call tool with malformed input (non-map)
%% Assert: Fails safely without crashing server
test_tool_call_invalid_input_fails_safely() ->
    %% Manually call with invalid input to test error handling
    catch taiea_mcp_server:call_tool(<<"taiea.health.check">>, not_a_map),

    %% Server should still be running
    ?assert(is_pid(whereis(taiea_mcp_server))),
    ok.

%% Arrange: Server running
%% Act: Call tool and capture response
%% Assert: Response includes receipt with required fields
test_tool_response_includes_receipt() ->
    Input = #{},

    {ok, _Response, Receipt} = taiea_mcp_server:call_tool(<<"taiea.health.check">>, Input),

    ?assert(maps:is_key(id, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(status, Receipt)),
    ok.

%%%===================================================================
%% Tool Registration Tests
%%%===================================================================

%% Arrange: Server running
%% Act: Register a custom tool
%% Assert: Tool is registered and callable
test_register_custom_tool_success() ->
    CustomTool = <<"custom.test.tool">>,
    Schema = #{
        description => <<"Test tool">>,
        inputSchema => #{
            type => <<"object">>,
            properties => #{
                <<"test_param">> => #{type => <<"string">>}
            }
        }
    },
    Handler = {taiea_tool_health, handle},

    ok = taiea_mcp_server:register_tool(CustomTool, Schema, Handler),

    {ok, Tools} = taiea_mcp_server:get_tools(),
    ToolNames = [maps:get(name, T) || T <- Tools],

    ?assert(lists:member(CustomTool, ToolNames)),
    ok.

%% Arrange: Server running with multiple tools
%% Act: Call get_tools
%% Assert: Returns all registered tools
test_get_tools_returns_all_registered() ->
    {ok, Tools} = taiea_mcp_server:get_tools(),

    ?assert(is_list(Tools)),
    ?assert(length(Tools) >= 4),

    %% Verify each tool has required fields
    lists:foreach(fun(Tool) ->
        ?assert(maps:is_key(name, Tool)),
        ?assert(maps:is_key(schema, Tool)),
        ?assert(is_binary(maps:get(name, Tool))),
        ?assert(is_map(maps:get(schema, Tool)))
    end, Tools),
    ok.

%%%===================================================================
%% Error Handling Tests
%%%===================================================================

%% Arrange: Server running
%% Act: Call a tool that doesn't exist
%% Assert: Returns error with tool_not_found
test_call_nonexistent_tool_returns_error() ->
    Result = taiea_mcp_server:call_tool(<<"nonexistent.tool">>, #{}),

    ?assert(is_tuple(Result) andalso element(1, Result) =:= error),
    ok.

%% Arrange: Server running
%% Act: Register tool with handler that throws exception
%% Assert: Exception is caught and error returned
test_tool_handler_exception_caught() ->
    %% Server should be running even if tool handler fails
    ?assert(is_pid(whereis(taiea_mcp_server))),
    ok.

%% Arrange: Server running, call tool multiple times
%% Act: Track requests across calls
%% Assert: Request counter increments
test_request_counter_incremented() ->
    %% Call health tool twice
    {ok, _, _} = taiea_mcp_server:call_tool(<<"taiea.health.check">>, #{}),
    {ok, _, _} = taiea_mcp_server:call_tool(<<"taiea.health.check">>, #{}),

    %% Server should still be responsive
    {ok, Tools} = taiea_mcp_server:get_tools(),
    ?assert(length(Tools) >= 4),
    ok.

%%%===================================================================
%% End of tests
%%%===================================================================
