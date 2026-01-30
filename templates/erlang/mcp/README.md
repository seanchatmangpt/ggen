# Autonomic MCP Server Template

## Overview

`autonomic_mcp_server.erl.tera` is a production-ready OTP gen_server template that integrates the Model Context Protocol (MCP) with autonomic computing (MAPE-K loop) for distributed system monitoring and chaos engineering.

## Features

### 10 Resources (Metrics & Monitoring)
1. **`metrics://cluster/health`** - Cluster-wide health status
2. **`metrics://mape_k/phases`** - MAPE-K loop phase statistics
3. **`metrics://node/metrics`** - Node-level performance metrics
4. **`chaos://history`** - Chaos engineering experiment history
5. **`knowledge://graph`** - Knowledge graph queries
6. **`topology://network`** - Network topology visualization
7. **`services://dependencies`** - Service dependency mapping
8. **`slo://violations`** - SLO violation tracking
9. **`recovery://actions`** - Recovery action status
10. **`policies://adaptation`** - Adaptation policy definitions

### 8 Tools (Chaos & MAPE-K Operations)
1. **`trigger_chaos`** - Trigger chaos engineering experiments
2. **`query_knowledge`** - Query knowledge base
3. **`execute_plan`** - Execute adaptation plans
4. **`analyze_metrics`** - Analyze metric trends
5. **`monitor_sensor`** - Read/reset sensor values
6. **`plan_adaptation`** - Generate adaptation plans
7. **`execute_recovery`** - Execute recovery actions
8. **`validate_policy`** - Validate policy definitions

### Subscription Management
- **Real-time updates** on resource changes
- **Filter functions** for selective notifications
- **Automatic cleanup** when subscribers die
- **Configurable limits** (default: 1000 subscriptions)

### Health Monitoring
- **Periodic health checks** (default: 30s interval)
- **Uptime tracking** with millisecond precision
- **Memory monitoring** and process counting
- **MCP server liveness** detection

### Error Recovery
- **Automatic restart** of MCP server on failure
- **Subscription cleanup** on subscriber death
- **Timeout enforcement** (resources: 5s, tools: 30s)
- **Handler crash protection** with detailed error reporting

## OTP Integration

### Supervision Tree

```erlang
% Add to your supervisor:
{autonomic_mcp_server, {autonomic_mcp_server, start_link, []},
 permanent, 5000, worker, [autonomic_mcp_server]}
```

### Configuration Options

```erlang
autonomic_mcp_server:start_link([
    {health_check_interval, 30000},  % 30 seconds
    {max_subscriptions, 1000},
    {resource_timeout, 5000},        % 5 seconds
    {tool_timeout, 30000},           % 30 seconds
    {enable_metrics, true}
]).
```

## Usage Examples

### Querying Resources

```erlang
% Get cluster health
{ok, Health} = autonomic_mcp_server:get_resource(<<"metrics://cluster/health">>).

% Get MAPE-K phase statistics
{ok, Phases} = autonomic_mcp_server:get_resource(<<"metrics://mape_k/phases">>).
```

### Invoking Tools

```erlang
% Trigger chaos experiment
{ok, Result} = autonomic_mcp_server:invoke_tool(
    <<"trigger_chaos">>,
    #{
        <<"experiment_type">> => <<"network_delay">>,
        <<"target">> => <<"service_a">>,
        <<"duration_seconds">> => 60
    }
).

% Query knowledge graph
{ok, Results} = autonomic_mcp_server:invoke_tool(
    <<"query_knowledge">>,
    #{<<"query">> => <<"SELECT * WHERE { ?s ?p ?o }">>}
).
```

### Subscribing to Updates

```erlang
% Subscribe to cluster health changes
Filter = fun(Data) -> maps:get(cluster_status, Data) =/= <<"healthy">> end,
{ok, SubId} = autonomic_mcp_server:subscribe(
    <<"metrics://cluster/health">>,
    Filter
).

% Receive updates
receive
    {mcp_update, Uri, Data} ->
        io:format("Update from ~s: ~p~n", [Uri, Data])
end.

% Unsubscribe
ok = autonomic_mcp_server:unsubscribe(SubId).
```

### Health Checks

```erlang
% Manual health check
{ok, Health} = autonomic_mcp_server:health_check().
% Returns: #{
%   status => healthy,
%   mcp_server_alive => true,
%   subscriptions => 5,
%   uptime_seconds => 3600,
%   memory_mb => 150,
%   process_count => 1234
% }
```

### Statistics

```erlang
% Get server statistics
{ok, Stats} = autonomic_mcp_server:get_stats().
% Returns: #{
%   resources_served => 12345,
%   tools_invoked => 678,
%   subscriptions_active => 5,
%   errors => 2,
%   uptime_seconds => 3600,
%   resource_count => 10,
%   tool_count => 8
% }
```

## Architecture

### State Management

The server maintains:
- **MCP server PID** - Reference to underlying erlmcp_server
- **Subscriptions map** - Active subscriptions with filters
- **Resources map** - Registered resources with handlers
- **Tools map** - Registered tools with schemas
- **Stats record** - Invocation counters and uptime
- **Health timer** - Periodic health check timer reference
- **Config record** - Server configuration

### Handler Pattern

All handlers follow the Chicago TDD pattern:
- **Arrange**: Validate inputs, prepare state
- **Act**: Execute business logic
- **Assert**: Return results with proper error handling

```erlang
-spec handler_name(map()) -> {ok, map()} | {error, term()}.
handler_name(Args) ->
    % Arrange - validate inputs
    RequiredField = maps:get(<<"field">>, Args),

    % Act - execute logic
    Result = perform_operation(RequiredField),

    % Assert - return result
    {ok, #{
        result => Result,
        timestamp => erlang:system_time(millisecond)
    }}.
```

### Error Handling

All operations use OTP error tuples:
- **`{ok, Result}`** - Success with data
- **`{error, Reason}`** - Failure with detailed reason
- **No exceptions** in normal operation
- **Crash reports** logged for debugging

### Timeout Enforcement

All handlers execute with configurable timeouts:
- **Resource handlers**: 5s default (configurable)
- **Tool handlers**: 30s default (configurable)
- **Automatic cleanup** on timeout
- **Detailed timeout reporting** with execution time

## Testing

### Unit Tests

```erlang
% Test resource handler
cluster_health_test() ->
    {ok, Health} = autonomic_mcp_server:cluster_health_handler(#{}),
    ?assertMatch(#{cluster_status := _, nodes := _}, Health).

% Test tool handler
trigger_chaos_test() ->
    Args = #{
        <<"experiment_type">> => <<"network_delay">>,
        <<"target">> => <<"test_service">>,
        <<"duration_seconds">> => 10
    },
    {ok, Result} = autonomic_mcp_server:trigger_chaos_handler(Args),
    ?assertMatch(#{experiment_id := _, status := <<"running">>}, Result).
```

### Integration Tests

```erlang
% Test full workflow
autonomic_workflow_test() ->
    % Start server
    {ok, _Pid} = autonomic_mcp_server:start_link(),

    % Query resource
    {ok, Health} = autonomic_mcp_server:get_resource(<<"metrics://cluster/health">>),
    ?assertMatch(#{cluster_status := _}, Health),

    % Invoke tool
    {ok, Result} = autonomic_mcp_server:invoke_tool(
        <<"trigger_chaos">>,
        #{<<"experiment_type">> => <<"pod_kill">>, <<"target">> => <<"service">>}
    ),
    ?assertMatch(#{status := <<"running">>}, Result),

    % Check stats
    {ok, Stats} = autonomic_mcp_server:get_stats(),
    ?assert(maps:get(resources_served, Stats) >= 1),
    ?assert(maps:get(tools_invoked, Stats) >= 1),

    % Stop server
    ok = autonomic_mcp_server:stop().
```

## Production Deployment

### Dependencies

Add to `rebar.config`:

```erlang
{deps, [
    {erlmcp_server, {git, "https://github.com/scottyla19/erlmcp_server.git", {branch, "main"}}},
    {uuid, "2.0.7", {pkg, uuid_erl}}
]}.
```

### System Configuration

Add to `sys.config`:

```erlang
[
    {autonomic_mcp_server, [
        {health_check_interval, 30000},
        {max_subscriptions, 1000},
        {resource_timeout, 5000},
        {tool_timeout, 30000},
        {enable_metrics, true}
    ]}
].
```

### Monitoring

Use OTP observer to monitor:
- **Process mailbox** - Should stay near zero
- **Memory usage** - Should be stable
- **Reductions** - Indicates CPU usage
- **Message queue** - Indicates backpressure

### Performance Tuning

- **Increase resource_timeout** for slow queries
- **Increase tool_timeout** for long-running operations
- **Decrease health_check_interval** for faster failure detection
- **Increase max_subscriptions** for high-traffic scenarios

## Customization

### Adding New Resources

1. Define handler function:
```erlang
-spec my_resource_handler(map()) -> {ok, map()} | {error, term()}.
my_resource_handler(Args) ->
    {ok, #{data => my_data}}.
```

2. Add to `register_resources/1`:
```erlang
{<<"custom://my_resource">>, fun my_resource_handler/1}
```

### Adding New Tools

1. Define schema:
```erlang
-spec my_tool_schema() -> map().
my_tool_schema() ->
    #{
        type => <<"object">>,
        properties => #{
            param => #{type => <<"string">>}
        },
        required => [<<"param">>]
    }.
```

2. Define handler:
```erlang
-spec my_tool_handler(map()) -> {ok, map()} | {error, term()}.
my_tool_handler(Args) ->
    Param = maps:get(<<"param">>, Args),
    {ok, #{result => Param}}.
```

3. Add to `register_tools/1`:
```erlang
{<<"my_tool">>, my_tool_schema(), fun my_tool_handler/1}
```

## Best Practices

1. **Always return proper error tuples** - Never throw exceptions
2. **Validate all inputs** - Use pattern matching and guards
3. **Log important events** - Use error_logger for debugging
4. **Monitor handler execution time** - Alert on slow handlers
5. **Test failure scenarios** - Verify error recovery
6. **Keep handlers stateless** - State in gen_server, not handlers
7. **Use timeouts liberally** - Prevent indefinite blocking
8. **Clean up resources** - Demonitor subscriptions, cancel timers

## License

MIT License - See project LICENSE file
