# BEAM Observability Integration Guide

This guide explains how to integrate the observability system into your existing BEAM cluster on GCP.

## Quick Start

### 1. Clone/Copy the Module

```bash
# Copy observability_src directory to your project
cp -r erlang_src/src/* your_project/src/
cp -r erlang_src/test/* your_project/test/
cp erlang_src/sys.config your_project/
cp erlang_src/vm.args your_project/
```

### 2. Add to rebar.config Dependencies

```erlang
{deps, [
    {observability, {git, "https://github.com/your-repo/observability.git", {tag, "1.0.0"}}}
]}.
```

### 3. Update Your Application

In your application supervisor:

```erlang
-module(your_app_sup).
-behaviour(supervisor).

init([]) ->
    ChildSpecs = [
        %% Your existing children...
        {your_service, {your_service, start_link, []}, ...},

        %% Add observability
        {observability_sup, {observability_sup, start_link, []},
         permanent, 5000, supervisor, [observability_sup]}
    ],

    {ok, {
        {one_for_one, 10, 60},
        ChildSpecs
    }}.
```

### 4. Configure sys.config

Update your `sys.config` to include observability settings:

```erlang
[
  {your_app, [
    %% Your app config...
  ]},

  {observability, [
    {metrics_scrape_interval, 10000},
    {gcp_project_id, "my-gcp-project"},
    {slack_enabled, false},
    {pagerduty_enabled, false}
  ]},

  {kernel, [...]},
  {sasl, [...]}
].
```

### 5. Compile and Test

```bash
rebar3 compile
rebar3 eunit      # Run unit tests
rebar3 ct         # Run integration tests
rebar3 release    # Create release
```

## Integration Patterns

### Pattern 1: Custom Metrics Handler

Register a function to be called whenever metrics are collected:

```erlang
%% In your module
-module(my_metrics_handler).

register_handlers() ->
    %% Handler for monitoring custom business logic
    CustomHandler = fun(Snapshot) ->
        Memory = Snapshot#metric_snapshot.memory,
        Processes = Snapshot#metric_snapshot.processes,

        %% Check if memory is growing
        TotalMem = maps:get(total, Memory, 0),
        ProcessCount = maps:get(count, Processes, 0),

        case TotalMem > 2_000_000_000 of  %% 2GB threshold
            true ->
                io:format("Memory warning: ~pMB~n", [TotalMem div 1024 div 1024]),
                % Trigger custom remediation
                cleanup_caches();
            false ->
                ok
        end
    end,

    metrics_collector:register_handler(CustomHandler).

cleanup_caches() ->
    % Your cleanup logic
    io:format("Cleaning up caches...~n").
```

### Pattern 2: Custom Alert Rules

Add business-specific alert rules:

```erlang
%% In your module
-module(my_alerts).

init_custom_alerts() ->
    %% Alert when cost exceeds threshold
    CostRule = #alert_rule{
        name = billing_cost_threshold,
        metric = billing_cost,
        operator = '>',
        threshold = 100.0,  %% $100 USD
        severity = high,
        action = fun escalate_to_finance/0
    },

    alert_manager:add_alert_rule(
        billing_cost_threshold,
        CostRule
    ),

    %% Alert when deployment fails
    DeploymentRule = #alert_rule{
        name = deployment_failure,
        metric = deployment_errors,
        operator = '>',
        threshold = 1,
        severity = critical,
        action = fun notify_devops/0
    },

    alert_manager:add_alert_rule(
        deployment_failure,
        DeploymentRule
    ).

escalate_to_finance() ->
    io:format("Escalating billing alert to finance team...~n"),
    % Send email, create ticket, etc.
    ok.

notify_devops() ->
    io:format("Notifying DevOps of deployment failure...~n"),
    ok.
```

### Pattern 3: Trace Filtering by Supervisor

Trace only a specific part of your system:

```erlang
%% In your startup code
init_selective_tracing() ->
    %% Trace only the billing supervisor's FSMs
    trace_handler:start_trace(billing),
    trace_handler:set_filter(billing),

    %% Run your operations...
    % ...

    %% Stop tracing when done
    trace_handler:stop_trace().
```

### Pattern 4: Profile-Based Optimization

Use profiling data to guide optimization:

```erlang
%% In your performance monitoring module
-module(performance_monitor).

start_profile_cycle(Duration) ->
    profiler:start_cpu_profile(),
    profiler:start_memory_profile(),

    timer:sleep(Duration),

    profiler:stop_cpu_profile(),
    profiler:stop_memory_profile(),

    %% Check profile files
    {ok, Status} = profiler:get_profile_status(),
    io:format("Profile files: ~p~n", [Status]).
```

### Pattern 5: Observer Export for Analysis

Periodically export observer dumps for offline analysis:

```erlang
%% In background job
-module(observer_analyzer).

export_for_analysis(DumpDir) ->
    Timestamp = erlang:system_time(second),
    DumpFile = filename:join([DumpDir, "dump_" ++ integer_to_list(Timestamp) ++ ".bin"]),

    ok = observer_ui:export_observer_dump(DumpFile),

    % Later, analyze the dump:
    {ok, DumpBinary} = file:read_file(DumpFile),
    Dump = binary_to_term(DumpBinary),

    MemoryInfo = maps:get(memory_info, Dump),
    ProcessTree = maps:get(process_tree, Dump),

    analyze_memory_hotspots(ProcessTree).

analyze_memory_hotspots(Processes) ->
    % Sort by memory usage
    Sorted = lists:sort(fun(P1, P2) ->
        maps:get(memory, P1, 0) > maps:get(memory, P2, 0)
    end, Processes),

    % Print top 10
    TopTen = lists:sublist(Sorted, 10),
    io:format("Top 10 memory consumers:~n"),
    lists:foreach(fun(P) ->
        Pid = maps:get(pid, P),
        Mem = maps:get(memory, P, 0),
        Name = maps:get(registered_name, P, ''),
        io:format("  ~w: ~p bytes (~w)~n", [Pid, Mem, Name])
    end, TopTen).
```

## GCP Integration

### Cloud Monitoring Metrics

Configure your GCP project to receive metrics:

```bash
# Set GCP project
gcloud config set project my-gcp-project

# Enable monitoring API
gcloud services enable monitoring.googleapis.com

# Create a custom metric descriptor
gcloud monitoring metrics-descriptors create custom.googleapis.com/beam_memory_total \
    --value-type INT64 \
    --metric-kind GAUGE \
    --description "Total BEAM process memory"
```

### Cloud Logging

Stream trace events to Cloud Logging:

```bash
# Enable logging API
gcloud services enable logging.googleapis.com

# Create a log sink
gcloud logging sinks create beam-traces \
    logging.googleapis.com/projects/my-project/logs/beam-traces \
    --log-filter='severity>=WARNING'
```

### PagerDuty Integration

Set up incident creation for critical alerts:

```bash
# Get PagerDuty API key from your account settings
# Set in sys.config:
{pagerduty_enabled, true},
{pagerduty_api_key, "your-api-key"},
{pagerduty_service_key, "your-service-key"}
```

### Slack Integration

Get webhook URL from Slack:

```bash
# Create incoming webhook in Slack workspace
# https://api.slack.com/messaging/webhooks

# Set in sys.config:
{slack_enabled, true},
{slack_webhook_url, "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"}
```

## Testing Integration

### Unit Tests

```bash
# Run tests for each module
rebar3 eunit --module=metrics_collector_tests
rebar3 eunit --module=alert_manager_tests
rebar3 eunit --module=trace_handler_tests
rebar3 eunit --module=profiler_tests
rebar3 eunit --module=observer_ui_tests
```

### Integration Tests

```bash
# Run full integration test suite
rebar3 ct
```

### Manual Testing

```bash
# Start interactive shell with observability
make shell

% Try collecting metrics
(observability@localhost)1> {ok, Metrics} = metrics_collector:collect_metrics().
{ok, #metric_snapshot{...}}

% Check alert status
(observability@localhost)2> {ok, Incidents} = alert_manager:get_active_incidents().
{ok, []}

% Export observer dump
(observability@localhost)3> observer_ui:export_observer_dump("/tmp/dump.bin").
ok

% Verify metrics history
(observability@localhost)4> {ok, History} = metrics_collector:get_metrics().
{ok, [#metric_snapshot{...}, ...]}
```

## Production Checklist

- [ ] Configure GCP project ID in sys.config
- [ ] Set up Cloud Monitoring metrics
- [ ] Set up Cloud Logging sink for traces
- [ ] Configure Slack webhook (optional)
- [ ] Configure PagerDuty service key (optional)
- [ ] Set alert thresholds appropriate for your workload
- [ ] Create log rotation policy (`error_logger_mf_maxfiles`)
- [ ] Enable distributed tracing for your services
- [ ] Test alert rules with synthetic load
- [ ] Set up backup for profile storage
- [ ] Document custom alert rules in runbooks
- [ ] Verify metrics are flowing to Cloud Monitoring
- [ ] Test incident creation with PagerDuty
- [ ] Train team on observer usage

## Performance Considerations

### Metrics Collection Impact

- ~50ms per collection (10s interval)
- ~1MB additional memory for history
- Negligible CPU impact

### Tracing Impact

- Depends on trace sampling rate (default 100%)
- Recommend 10-50% sampling in production
- 100MB buffer (configurable in sys.config)

### Profiling Impact

- CPU profiling adds 5-10% overhead while active
- Memory profiling is low impact
- Snapshots taken hourly by default

### Alert Checking Impact

- ~20ms per check interval
- Minimal CPU and memory usage
- Configure check interval based on your alert needs

## Troubleshooting

### Metrics Not Appearing in Cloud Monitoring

```erlang
% Verify metrics are being collected
{ok, [Latest|_]} = metrics_collector:get_metrics(),
io:format("Latest metrics: ~p~n", [Latest]).

% Check GCP project ID is configured
{ok, ProjectId} = application:get_env(observability, gcp_project_id),
io:format("Project ID: ~s~n", [ProjectId]).
```

### Alerts Not Firing

```erlang
% Check alert rules are registered
{ok, Status} = alert_manager:get_active_incidents(),
io:format("Active incidents: ~p~n", [Status]).

% Trigger manually with test metrics
TestMetrics = #{total_memory => 2_000_000_000},
alert_manager:check_thresholds(TestMetrics).
```

### Trace Buffer Full

Reduce sampling rate in sys.config:
```erlang
{trace_log_sampling_rate, 0.1}  % 10% instead of 100%
```

### Memory Profiling Files Not Created

```erlang
% Check profile directory exists
ProfileDir = application:get_env(observability, profile_dir, "./profiles"),
filelib:ensure_dir(ProfileDir ++ "/").

% Verify write permissions
file:write_file(ProfileDir ++ "/test.txt", "test").
```

## Advanced Topics

### Custom Metric Types

Add custom metrics beyond the defaults:

```erlang
% In metrics_collector
collect_custom_metrics() ->
    #{
        billing_cost => get_current_billing_cost(),
        request_rate => calculate_request_rate(),
        cache_hit_ratio => get_cache_stats()
    }.
```

### Alert Action Chaining

Execute multiple actions when alert fires:

```erlang
AlertAction = fun() ->
    notify_ops(),
    scale_up_capacity(),
    create_incident()
end.
```

### Multi-Node Tracing

Coordinate tracing across multiple nodes:

```erlang
% On each node
trace_handler:start_trace(billing),

% Correlate traces by request ID
% Use correlation IDs in your logs
```

## References

- [OBSERVABILITY.md](./OBSERVABILITY.md) - Module documentation
- [rebar3 Documentation](https://rebar3.org)
- [Erlang Observer](http://erlang.org/doc/man/observer.html)
- [GCP Monitoring](https://cloud.google.com/monitoring/docs)
- [GCP Logging](https://cloud.google.com/logging/docs)

## Support

For issues:
1. Check logs in `./log/` directory
2. Review module tests in `test/` directory
3. Enable debug logging: `{log_level, debug}` in sys.config
4. Check GCP Console for metrics/logs
5. Review PagerDuty incidents

## Contributing

To extend the observability system:

1. Add new module following the same pattern
2. Update supervisor child spec
3. Add tests in `test/` directory
4. Update sys.config with configuration
5. Document in OBSERVABILITY.md

Example new module:

```erlang
-module(my_custom_observer).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

% ... implement rest of callbacks ...
```

Then add to `observability_sup.erl` supervision tree.
