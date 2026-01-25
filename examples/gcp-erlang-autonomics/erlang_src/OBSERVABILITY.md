# BEAM Cluster Observability System

Comprehensive observability framework for distributed Erlang (BEAM) clusters deployed on GCP.

## Overview

This observability system provides five integrated modules for monitoring, profiling, tracing, and alerting on BEAM clusters:

1. **Metrics Collector** - Real-time metrics collection and GCP Cloud Monitoring integration
2. **Trace Handler** - FSM event tracing with Cloud Logging streaming
3. **Profiler** - CPU and memory profiling with Cloud Storage uploads
4. **Alert Manager** - Threshold-based alerting with auto-remediation
5. **Observer UI** - Built-in Erlang observer with remote access

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│           Observability Supervisor                       │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │   Metrics    │  │    Trace     │  │  Profiler    │  │
│  │  Collector   │  │   Handler    │  │              │  │
│  │ (10s interval)│  │  (FSM events)│  │ (CPU/Memory) │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
│         │                  │                  │          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │    Alert     │  │   Observer   │  │     GCP      │  │
│  │   Manager    │  │      UI      │  │ Integration  │  │
│  │ (Thresholds) │  │ (Live view)  │  │  (Monitoring)│  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
│                                                          │
└─────────────────────────────────────────────────────────┘
         │                  │                  │
         ├─────────────────┼─────────────────┤
         │                 │                 │
       Slack          Cloud Logging    Cloud Monitoring
     PagerDuty        Cloud Storage    Cloud Incidents
```

## Module Details

### 1. Metrics Collector (`metrics_collector.erl`)

Collects comprehensive system metrics every 10 seconds.

**Collected Metrics:**
- **Memory**: Total, processes, system, atom, binary, code, ETS
- **Processes**: Count, limit, run queue, scheduler info
- **Message Queues**: Depth distribution, high-load processes
- **Governors**: Billing governor state and cost accumulation
- **Errors**: Error and warning counts from error_logger

**Usage:**

```erlang
%% Start metrics collection (started by supervisor)
{ok, Pid} = metrics_collector:start_link().

%% Collect metrics immediately
{ok, Snapshot} = metrics_collector:collect_metrics().

%% Get metric history (last 1000 snapshots)
{ok, History} = metrics_collector:get_metrics().

%% Register a handler to be called on every collection
MetricHandler = fun(Snapshot) ->
    MemMetrics = Snapshot#metric_snapshot.memory,
    io:format("Total memory: ~p bytes~n", [maps:get(total, MemMetrics)])
end,
metrics_collector:register_handler(MetricHandler).
```

**Configuration (sys.config):**
```erlang
{observability, [
    {metrics_scrape_interval, 10000},  %% 10 seconds
    {metrics_max_history, 1000},       %% Keep last 1000 snapshots
    {gcp_project_id, "your-project"},
    {gcp_bucket, "beam-profiles"}
]}
```

### 2. Trace Handler (`trace_handler.erl`)

Captures FSM state transitions and streams to Cloud Logging.

**Captured Events:**
- Governor state transitions (billing, entitlement, deployment)
- Message receives and sends
- FSM callbacks (call, cast, info)
- Process lifecycle events

**Usage:**

```erlang
%% Start trace handler
{ok, Pid} = trace_handler:start_link().

%% Start tracing for specific governor type
ok = trace_handler:start_trace(billing).

%% Available governors: billing, entitlement, deployment, all

%% Set filter to reduce noise
ok = trace_handler:set_filter(billing).

%% Stop tracing
ok = trace_handler:stop_trace().
```

**JSON Output Format:**
```json
{
  "timestamp": "2026-01-25T12:34:56Z",
  "node": "observability@localhost",
  "pid": "<0.42.0>",
  "type": "call",
  "governor": "billing",
  "old_state": "evaluate_cost",
  "new_state": "charge_customer",
  "message": "charge_request",
  "metadata": {
    "process_info": {...},
    "trace_timestamp": 1674664496000
  }
}
```

**Configuration (sys.config):**
```erlang
{observability, [
    {trace_buffer_size, 104857600},    %% 100MB
    {trace_log_sampling_rate, 1.0},    %% 100% (or 0.1 for 10%)
    {trace_storage_backend, cloud_logging}
]}
```

### 3. Profiler (`profiler.erl`)

Performs CPU and memory profiling with periodic snapshots.

**Features:**
- **CPU Profiling** (fprof): Function call counts and timings
- **Memory Profiling** (erts_debug): Per-process memory snapshot
- **Hourly Snapshots**: Automatic periodic profiling
- **Cloud Storage Upload**: Upload profiles to GCP for analysis

**Usage:**

```erlang
%% Start profiler
{ok, Pid} = profiler:start_link().

%% Start CPU profiling (continuous)
ok = profiler:start_cpu_profile().

%% ... run your code ...

%% Stop and analyze CPU profile
ok = profiler:stop_cpu_profile().

%% Memory profiling
ok = profiler:start_memory_profile().
ok = profiler:stop_memory_profile().

%% Get profile status
{ok, Status} = profiler:get_profile_status().
```

**Profile Files:**
- CPU profiles: `./profiles/cpu_profile_*.fprof`
- Memory profiles: `./profiles/memory_profile_*.bin`
- Snapshots: `./profiles/cpu_snapshot_*.fprof`

**Configuration (sys.config):**
```erlang
{observability, [
    {profile_dir, "./profiles"},
    {profile_interval, 3600000},         %% 1 hour
    {enable_cpu_profiling, true},
    {enable_memory_profiling, true}
]}
```

### 4. Alert Manager (`alert_manager.erl`)

Monitors metrics against thresholds and triggers alerts/remediation.

**Default Alert Rules:**
- `high_memory_usage` (>90% of max heap)
- `high_error_rate` (>100 errors)
- `high_message_queue_depth` (>500 avg)
- `low_availability` (<100 processes)

**Incident States:**
- `open` - Active incident
- `acknowledged` - Reviewed by operator
- `resolved` - Threshold no longer exceeded

**Usage:**

```erlang
%% Start alert manager
{ok, Pid} = alert_manager:start_link().

%% Define custom alert rule
Rule = #alert_rule{
    name = my_custom_alert,
    metric = error_count,
    operator = '>',
    threshold = 50,
    severity = high,
    action = fun auto_remediate_errors/0
},
ok = alert_manager:add_alert_rule(my_custom_alert, Rule).

%% Check metrics against rules
Metrics = #{
    total_memory => 1000000,
    error_count => 150,
    process_count => 500
},
ok = alert_manager:check_thresholds(Metrics).

%% Get active incidents
{ok, Incidents} = alert_manager:get_active_incidents().

%% Remove alert rule
ok = alert_manager:remove_alert_rule(my_custom_alert).
```

**Alert Severity Levels:**
- `low` - Advisory, no immediate action needed
- `medium` - Monitor closely, may need attention
- `high` - Significant issue, operator attention required
- `critical` - Emergency, immediate action required

**Configuration (sys.config):**
```erlang
{observability, [
    {alert_check_interval, 10000},
    {slack_webhook_url, "https://hooks.slack.com/..."},
    {slack_enabled, false},
    {pagerduty_api_key, "your-api-key"},
    {pagerduty_enabled, false},
    {pagerduty_service_key, "service-key"}
]}
```

### 5. Observer UI (`observer_ui.erl`)

Built-in Erlang observer with periodic dump export.

**Features:**
- Real-time process tree
- Message queue monitoring
- Supervisor status
- Memory statistics
- Remote access support

**Usage:**

```erlang
%% Start observer UI
{ok, Pid} = observer_ui:start_link().

%% Start the observer UI (requires X11 or headless support)
ok = observer_ui:start_observer().

%% Get observer status
{ok, Status} = observer_ui:get_observer_status().
%% Returns: #{
%%     observer_running => true|false,
%%     system_info => {...},
%%     process_tree => [...],
%%     supervisor_status => [...]
%% }

%% Export observer dump to file
ok = observer_ui:export_observer_dump("/tmp/dump.bin").

%% Remote observer access (from another node)
%% erl -sname client@localhost
%% > observer:start_gui().
%% Connect to 'observability@localhost'
```

**Dump Contents:**
- Node info and statistics
- System configuration
- Process tree with memory/reductions
- Supervisor hierarchy
- ETS tables and memory breakdown

**Configuration (sys.config):**
```erlang
{observability, [
    {observer_dump_dir, "./observer_dumps"},
    {observer_dump_interval, 300000}  %% 5 minutes
]}
```

## Configuration

### sys.config - Complete Example

```erlang
[
  {observability, [
    %% Metrics
    {metrics_scrape_interval, 10000},
    {metrics_max_history, 1000},

    %% GCP
    {gcp_project_id, "my-gcp-project"},
    {gcp_bucket, "beam-profiles"},

    %% Tracing
    {trace_buffer_size, 104857600},
    {trace_log_sampling_rate, 0.1},     %% 10% sampling
    {trace_storage_backend, cloud_logging},

    %% Profiling
    {profile_dir, "./profiles"},
    {enable_cpu_profiling, true},
    {enable_memory_profiling, true},

    %% Alerts
    {alert_check_interval, 10000},
    {slack_webhook_url, "https://hooks.slack.com/services/..."},
    {slack_enabled, true},
    {pagerduty_api_key, "key-xxx"},
    {pagerduty_enabled, false},

    %% Observer
    {observer_dump_dir, "./observer_dumps"},
    {observer_dump_interval, 300000},

    %% General
    {log_level, info},
    {environment, production}
  ]},

  {kernel, [
    {error_logger_mf_dir, "./log"},
    {error_logger_mf_maxbytes, 10485760},
    {error_logger_mf_maxfiles, 10}
  ]},

  {sasl, [
    {sasl_error_logger, {file, "./log/sasl.log"}}
  ]}
].
```

### vm.args - Performance Tuning

Key settings in `vm.args`:

```
+K true                  %% Kernel poll
+A 16                    %% Async threads
+zdbbl 32768             %% Port buffer limit
+hms off                 %% Hybrid message switch
+scheduler_poll_set_size 4096
```

## Integration Points

### GCP Cloud Monitoring

The metrics collector publishes to GCP Cloud Monitoring:

```bash
gcloud monitoring metrics-descriptors create beam.memory.total
gcloud monitoring metrics-descriptors create beam.processes.count
gcloud monitoring metrics-descriptors create beam.message_queue.average_depth
gcloud monitoring metrics-descriptors create beam.errors.total
```

### Cloud Logging

Trace events stream to Cloud Logging as JSON:

```bash
# View logs in real-time
gcloud logging read "resource.type=gce_instance" --limit 50 --format json
```

### PagerDuty Integration

Critical incidents (severity=critical) auto-create PagerDuty incidents:

```erlang
%% Set PagerDuty credentials in sys.config
{pagerduty_enabled, true},
{pagerduty_api_key, "YOUR_API_KEY"},
{pagerduty_service_key, "YOUR_SERVICE_KEY"}
```

## Monitoring SLOs

Default alert thresholds for production:

| Metric | Threshold | Severity |
|--------|-----------|----------|
| Memory Usage | >90% max heap | HIGH |
| Error Rate | >100 errors/interval | HIGH |
| Process Count | <100 | CRITICAL |
| Message Queue Depth | >500 avg | MEDIUM |
| Availability | <99.9% uptime | CRITICAL |
| Latency P99 | >500ms | HIGH |

## Performance Characteristics

| Operation | Latency | Memory |
|-----------|---------|--------|
| Metrics collection | ~50ms | ~1MB |
| Trace handler | ~10ms/event | Variable (100MB buffer) |
| CPU profile start | ~100ms | ~5MB |
| Alert check | ~20ms | ~100KB |
| Observer dump | ~500ms | ~10MB |

## Testing

### Run all tests
```bash
rebar3 eunit
```

### Run specific test module
```bash
rebar3 eunit --module=metrics_collector_tests
```

### Common test integration tests
```bash
rebar3 ct
```

### Example test patterns

```erlang
%% Metrics collection test
metrics_collector:start_link(),
{ok, Snapshot} = metrics_collector:collect_metrics(),
?assertNotEqual(undefined, Snapshot#metric_snapshot.memory).

%% Alert rule test
Rule = #alert_rule{name=test, metric=errors, operator='>', threshold=100},
alert_manager:add_alert_rule(test, Rule),
alert_manager:check_thresholds(#{errors => 150}),
{ok, [Incident|_]} = alert_manager:get_active_incidents(),
?assertEqual(test, Incident#incident.rule_name).
```

## Troubleshooting

### High Memory Usage
```erlang
%% Check process memory
{ok, ProcessStats} = profiler:get_profile_status(),
%% Identify top memory consumers
erlang:garbage_collect().
```

### Trace Buffer Full
Reduce sampling rate in sys.config:
```erlang
{trace_log_sampling_rate, 0.1}  %% Only trace 10%
```

### Slow Metrics Collection
Check for:
- Large number of processes (>50k)
- Message queue introspection taking time
- Network latency to GCP

### Alert Rules Not Firing
Verify:
```erlang
{ok, Rules} = alert_manager:get_active_incidents(),
%% Check rule configuration
%% Ensure metrics are being collected
{ok, Metrics} = metrics_collector:collect_metrics().
```

## Advanced Usage

### Custom Metric Handler
```erlang
CustomHandler = fun(Snapshot) ->
    Memory = Snapshot#metric_snapshot.memory,
    Procs = Snapshot#metric_snapshot.processes,
    io:format("Memory: ~p, Processes: ~p~n",
              [maps:get(total, Memory),
               maps:get(count, Procs)])
end,
metrics_collector:register_handler(CustomHandler).
```

### Custom Alert Action
```erlang
CustomRule = #alert_rule{
    name = memory_alert,
    metric = total_memory,
    operator = '>',
    threshold = 2000000000,
    severity = high,
    action = fun() ->
        io:format("Memory alert triggered!~n"),
        erlang:garbage_collect()
    end
},
alert_manager:add_alert_rule(memory_alert, CustomRule).
```

### Real-time Tracing
```erlang
%% Start trace handler
trace_handler:start_link(),

%% Begin tracing billing governor
trace_handler:start_trace(billing),

%% ... run operations ...

%% Stop and export
trace_handler:stop_trace().
```

## References

- [Erlang Observer Documentation](http://erlang.org/doc/man/observer.html)
- [GCP Cloud Monitoring API](https://cloud.google.com/monitoring/api)
- [GCP Cloud Logging API](https://cloud.google.com/logging/docs)
- [Erlang Profiling Tools](http://erlang.org/doc/tools/fprof_chapter.html)

## Support

For issues or enhancements:
1. Check logs in `./log/` directory
2. Review GCP Cloud Logging console
3. Enable debug logging: `{log_level, debug}` in sys.config
4. Check PagerDuty incidents for critical alerts

## License

Part of GCP Erlang Autonomics example system.
