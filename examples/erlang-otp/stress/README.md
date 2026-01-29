# Erlang/OTP Stress Test Suite

Chaos engineering and fault injection tools for validating Erlang/OTP system resilience.

## Overview

This stress test suite validates that your Erlang/OTP application:
1. **Recovers gracefully** from random failures (Chaos Monkey)
2. **Handles specific faults** correctly (Fault Injection)
3. **Maintains consistency** under network partitions (Cluster Stress)

## Stress Test Modules

### 1. Chaos Monkey (`chaos_monkey.erl`)

**Purpose**: Random failure injection to test system resilience under unpredictable conditions.

**Scenarios**:
- `kill_random_worker` - Kill worker processes, verify supervisor restart
- `kill_supervisor` - Kill supervisors, verify application restart
- `network_partition` - Disconnect cluster nodes, verify partition healing
- `message_flood` - Flood process mailboxes, verify message handling
- `memory_pressure` - Allocate large memory, verify GC and recovery
- `cpu_spike` - Saturate CPU, verify scheduler responsiveness

**Running**:
```erlang
%% Compile
c(chaos_monkey).

%% Run with defaults (5 minutes, all scenarios)
chaos_monkey:run(#{
    target_supervisor => call_router_sup,
    duration_sec => 300
}).

%% Custom scenario mix
chaos_monkey:run(#{
    target_supervisor => call_router_sup,
    duration_sec => 600,
    scenarios => [
        {kill_random_worker, 0.40},  % 40% probability
        {network_partition, 0.30},   % 30% probability
        {message_flood, 0.30}        % 30% probability
    ],
    failure_interval_ms => 2000,     % Failure every 2 seconds
    report_interval_sec => 60        % Report every minute
}).

%% List available scenarios
chaos_monkey:get_scenarios().
```

**Example Output**:
```
🐵 === CHAOS MONKEY ACTIVATED === 🐵

Target Supervisor: call_router_sup
Duration: 300 seconds
Failure Interval: 1000 ms

Scenarios:
  - kill_random_worker (30.0%)
  - network_partition (20.0%)
  - message_flood (20.0%)
  - memory_pressure (15.0%)
  - kill_supervisor (10.0%)
  - cpu_spike (5.0%)

[2026-01-29 05:22:00] Executing: kill_random_worker
  💀 Killing worker: <0.234.0>
  ✅ Worker restarted by supervisor

[2026-01-29 05:22:01] Executing: message_flood
  🌊 Flooding messages...
  ✅ All workers survived flood

--- Chaos Stats ---
Total failures: 42
Scenario breakdown:
  kill_random_worker: 15
  message_flood: 8
  network_partition: 7
  memory_pressure: 6
  kill_supervisor: 4
  cpu_spike: 2

🐵 === CHAOS MONKEY COMPLETE === 🐵

=== Final Chaos Statistics ===

Total Failures: 42

Scenario Breakdown:
  kill_random_worker        15
  message_flood             8
  network_partition         7
  memory_pressure          6
  kill_supervisor          4
  cpu_spike                2

Average Recovery Times (ms):
  kill_random_worker        142.34
  message_flood             2134.56
  network_partition         5234.12
  memory_pressure          3456.78
  kill_supervisor          1234.56
  cpu_spike                3001.23

Unrecovered Failures: 0
```

### 2. Fault Injection (`fault_injection.erl`)

**Purpose**: Controlled, reproducible fault injection for regression testing.

**Fault Types**:
- `supervisor_crash` - Supervisor process crash
- `worker_crash` - Worker process crash
- `database_timeout` - Simulated database timeout
- `network_error` - Network disconnection
- `disk_full` - Disk space exhaustion
- `memory_exhausted` - Memory allocation failure
- `port_exhaustion` - Port limit reached
- `message_queue_overflow` - Mailbox overflow
- `invalid_input` - Invalid message format
- `corrupt_state` - Corrupted process state

**Running**:
```erlang
%% Compile
c(fault_injection).

%% Inject single fault
fault_injection:inject(supervisor_crash, call_router_sup).

%% Inject with automatic recovery verification
fault_injection:inject_with_verification(
    database_timeout,
    #{timeout_ms => 5000},
    10000  % Recovery timeout
).

%% Inject worker crash with config
fault_injection:inject(worker_crash, #{
    supervisor => call_router_sup,
    index => 2  % Kill 2nd worker
}).

%% List available fault types
fault_injection:get_fault_types().
```

**Example Output**:
```
=== Fault Injection with Verification ===
Fault Type: supervisor_crash
Recovery Timeout: 10000ms

=== Injecting Fault: supervisor_crash ===
Injecting supervisor crash for: call_router_sup
✅ Fault injected successfully in 5ms: #{old_pid => <0.123.0>, new_pid => <0.456.0>}

Verifying recovery from supervisor_crash...
✅ System recovered successfully

=== Fault Injection Results ===
Fault Type: supervisor_crash
Injection: ✅ Success
Recovery: ✅ Success (127ms)
```

### 3. Cluster Stress (`cluster_stress.erl`)

**Purpose**: Test distributed system behavior under network failures and high message volume.

**Test Scenarios**:
- `node_failure` - Simulate node crash, verify cluster detection
- `split_brain` - Create network partition, verify resolution
- `message_volume` - High inter-node message rate, verify delivery
- `data_consistency` - Verify data consistency after partition

**Running**:
```erlang
%% Compile
c(cluster_stress).

%% Run all cluster stress tests
cluster_stress:run(#{
    nodes => [node1@host, node2@host, node3@host],
    duration_sec => 300,
    scenarios => [node_failure, split_brain, message_volume]
}).

%% Test specific scenario
cluster_stress:test_node_failure(node2@host).

cluster_stress:test_split_brain([node1@host, node2@host, node3@host]).

cluster_stress:test_high_message_volume([node1@host, node2@host]).

%% Health check
cluster_stress:verify_cluster_health().
```

**Example Output**:
```
=== Cluster Stress Test ===

Nodes: [node1@127.0.0.1, node2@127.0.0.1, node3@127.0.0.1]
Duration: 300 seconds
Scenarios: [node_failure, split_brain, message_volume]

Verifying cluster connectivity...
✅ All nodes connected

=== Testing Node Failure ===
Target Node: node2@127.0.0.1

Simulating node crash...
✅ Node failure detected by cluster

=== Testing Split-Brain Scenario ===
Partition 1: [node1@127.0.0.1]
Partition 2: [node2@127.0.0.1, node3@127.0.0.1]

Creating network partition...
Healing partition...

=== Split-Brain Test Results ===
Isolation: ✅ PASS
Recovery: ✅ PASS

=== Testing High Message Volume ===
Messages per worker: 100000
Workers per node: 10
Total messages: 3000000

Spawning workers...
Warmup phase...
Warmup complete

Starting benchmark...

Collecting results...

=== Message Volume Results ===
Duration: 15234ms
Messages sent: 3000000
Messages received: 2998742
Message rate: 196891.23 msg/sec
Delivery rate: 99.96%
```

## Stress Test Strategy

### 1. Chaos Engineering Principles

**Goal**: Find weaknesses before they cause production outages.

**Process**:
1. **Hypothesize steady state** - Define normal system behavior
2. **Vary real-world events** - Inject realistic failures
3. **Run experiments** - Execute chaos scenarios
4. **Disprove hypothesis** - Find breaking points
5. **Fix and repeat** - Improve resilience

### 2. Blast Radius Control

**Start small**:
- Single worker crash → Supervisor crash → Node crash
- Single node partition → Full cluster partition
- 100 messages → 10K messages → 1M messages

**Gradual escalation**:
1. Development environment
2. Staging environment
3. Canary production (1% traffic)
4. Full production (controlled)

### 3. Monitoring During Tests

**Critical metrics**:
- Process count (`erlang:system_info(process_count)`)
- Memory usage (`erlang:memory()`)
- Message queue lengths (`process_info(Pid, message_queue_len)`)
- Scheduler utilization (`scheduler_wall_time`)
- ETS table sizes
- Error logs

## Recovery Patterns

### 1. Supervisor Restart

**Expected behavior**:
```erlang
Worker crashes → Supervisor detects exit signal → Supervisor restarts child → System recovers
```

**Validation**:
```erlang
OldPid = whereis(worker),
exit(OldPid, kill),
timer:sleep(100),
NewPid = whereis(worker),
true = (NewPid =/= OldPid),  % Different PID
true = is_process_alive(NewPid).  % New process alive
```

### 2. Network Partition Healing

**Expected behavior**:
```erlang
Partition occurs → Nodes detect split → Partitions operate independently →
Network restored → Nodes reconnect → Data reconciliation → Consistency restored
```

**Validation**:
```erlang
%% Before partition
AllNodes = [node() | nodes()],

%% Create partition
erlang:disconnect_node(TargetNode),

%% During partition
PartitionedNodes = [node() | nodes()],
false = lists:member(TargetNode, PartitionedNodes),

%% Heal partition
net_kernel:connect_node(TargetNode),
timer:sleep(1000),

%% After healing
HealedNodes = [node() | nodes()],
true = lists:member(TargetNode, HealedNodes).
```

### 3. Backpressure and Load Shedding

**Expected behavior**:
```erlang
Load spike → Mailbox grows → Backpressure triggered → Load shed →
Load decreases → Backpressure released → Normal operation
```

**Validation**:
```erlang
%% Monitor mailbox during flood
{message_queue_len, QueueLen} = process_info(Pid, message_queue_len),
case QueueLen > 10000 of
    true ->
        %% Backpressure should activate
        verify_backpressure_active();
    false ->
        ok
end.
```

## Common Failure Modes

### 1. Cascading Failures

**Symptom**: Single failure triggers multiple related failures
**Example**: Worker crash → Supervisor restart → All workers restart → Brief unavailability
**Solution**:
- Implement `one_for_one` restart strategy
- Use circuit breakers
- Isolate failure domains

### 2. Split-Brain Syndrome

**Symptom**: Network partition creates two independent clusters
**Example**: 3-node cluster partitions into [A, B] and [C]
**Solution**:
- Implement partition tolerance strategies
- Use distributed consensus (Raft, Paxos)
- Configure `net_kernel` wisely

### 3. Resource Exhaustion

**Symptom**: System runs out of resources under stress
**Example**: Process count → Atom table → Memory → Ports
**Solution**:
- Set resource limits
- Monitor usage proactively
- Implement resource pooling

### 4. Deadlocks

**Symptom**: Processes waiting on each other indefinitely
**Example**: Process A waits for B, Process B waits for A
**Solution**:
- Use timeouts on all `gen_server:call/3`
- Implement deadlock detection
- Design lock-free algorithms

## Integration with CI/CD

### Nightly Chaos Tests

```yaml
name: Nightly Chaos Tests

on:
  schedule:
    - cron: '0 2 * * *'  # 2 AM daily

jobs:
  chaos:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'

      - name: Start Application
        run: |
          rebar3 shell --apps your_app &
          sleep 10

      - name: Run Chaos Monkey
        run: |
          ./stress/run_stress_tests.sh

      - name: Collect Crash Dumps
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: crash-dumps
          path: erl_crash.dump
```

## Best Practices

### 1. Test in Isolation

**Run stress tests**:
- On dedicated test clusters
- During maintenance windows
- In staging environments first

### 2. Incremental Complexity

**Progression**:
1. Single fault → Multiple sequential faults → Concurrent faults
2. Known faults → Random faults
3. Soft failures → Hard failures

### 3. Observability

**Monitor**:
- Application logs (`error_logger`)
- System metrics (`observer`)
- Custom telemetry
- External monitoring (Prometheus, Grafana)

### 4. Automate Recovery

**Verify**:
- Supervisors restart failed processes
- Health checks detect issues
- Load balancers remove unhealthy nodes
- Alerts fire on critical failures

### 5. Document Learnings

**After each test**:
- Record failure scenarios
- Document recovery procedures
- Update runbooks
- Improve fault tolerance

## Troubleshooting

### Stress Test Hangs

**Causes**:
- Deadlock
- Infinite loop in recovery
- Network partition not healing

**Solutions**:
```erlang
%% Set timeout on all operations
{ok, Result} = gen_server:call(Pid, Request, 5000),

%% Monitor processes
Ref = monitor(process, Pid),
receive
    {'DOWN', Ref, process, Pid, Reason} ->
        handle_crash(Reason)
after 10000 ->
    error(timeout)
end.
```

### Application Won't Restart

**Check**:
1. Supervisor restart intensity: `{MaxR, MaxT}`
2. Permanent vs transient children
3. Application dependencies
4. Crash dump: `erl_crash.dump`

**Fix**:
```erlang
%% Increase restart tolerance
supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,  % More restarts allowed
        period => 60      % Per minute
    },
    {ok, {SupFlags, ChildSpecs}}.
```

### Memory Leaks

**Detect**:
```erlang
%% Before test
InitialMem = erlang:memory(total),

%% Run stress test
chaos_monkey:run(Config),

%% After test
FinalMem = erlang:memory(total),
MemIncrease = FinalMem - InitialMem,

%% Memory should return to baseline
case MemIncrease > (InitialMem * 0.1) of
    true -> error({memory_leak, MemIncrease});
    false -> ok
end.
```

## Further Reading

- [Principles of Chaos Engineering](https://principlesofchaos.org/)
- [Erlang Fault Tolerance](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Jepsen Testing](https://jepsen.io/) - Distributed systems testing
- [Netflix Chaos Monkey](https://netflix.github.io/chaosmonkey/)

## Related Tools

- **Chaos Toolkit** - Generic chaos engineering
- **Gremlin** - Chaos engineering platform
- **Jepsen** - Distributed systems testing
- **Blockade** - Docker-based network partitions

## Support

For issues:
1. Check application logs
2. Review crash dumps (`erl_crash.dump`)
3. Monitor system metrics
4. Consult Erlang/OTP documentation
