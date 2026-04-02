# Troubleshooting Guide - ggen v6.0.0

## Table of Contents

1. [Agent Issues](#agent-issues)
2. [Coordination Issues](#coordination-issues)
3. [Performance Issues](#performance-issues)
4. [Testing Issues](#testing-issues)
5. [Integration Issues](#integration-issues)

---

## Agent Issues

### Issue: Agent Stuck in Running State

**Symptoms:**
- Task remains in `Running` state for extended period
- No progress updates
- Agent appears unresponsive
- Logs show last activity > 5 minutes ago

**Root Causes:**
1. Deadlock in actor communication
2. Blocking operation in executor
3. Network timeout waiting for consensus
4. Resource exhaustion (memory, CPU)

**Diagnosis Steps:**

1. Check task state:
```bash
# Query task status
curl http://localhost:8080/api/tasks/{task_id}
# Expected: state: "running", updated_at should be recent

# If stale, check logs
tail -f /var/log/ggen/agent.log | grep {task_id}
```

2. Monitor resource usage:
```bash
top -p {agent_pid}
# Check CPU (should be > 0 if executing)
# Check memory (should be stable, not growing)
# Check open file descriptors: lsof -p {agent_pid}
```

3. Check network connectivity:
```bash
# Test consensus connection
nc -zv consensus-server:5000

# Check message queue depth
curl http://localhost:8080/metrics | grep queue_depth
```

4. Examine executor logs:
```bash
grep -E "ERROR|WARN" /var/log/ggen/executor.log | tail -20
```

**Resolution:**

**Option 1: Graceful Timeout (Preferred)**
```bash
# Mark task as blocked, retry from checkpoint
curl -X POST http://localhost:8080/api/tasks/{task_id}/timeout \
  -H "Content-Type: application/json" \
  -d '{
    "reason": "Executor timeout",
    "recovery": "restart_from_checkpoint"
  }'
```

**Option 2: Force State Transition**
```bash
# Only if debugging - unsafe in production
curl -X POST http://localhost:8080/api/tasks/{task_id}/force-fail \
  -H "Content-Type: application/json" \
  -d '{
    "reason": "Manual intervention after timeout"
  }'
```

**Option 3: Restart Agent**
```bash
# Kill stuck agent
kill -SIGTERM {agent_pid}

# Wait for graceful shutdown (5s timeout)
sleep 5

# Force kill if needed
kill -SIGKILL {agent_pid}

# Restart
systemctl restart ggen-agent
```

**Prevention:**
- Set executor timeouts: `executor.timeout_secs = 300`
- Monitor stale tasks: alert if `updated_at > 5 min` in `Running`
- Regular task audits: `ggen audit --since 1h`

---

### Issue: Agent Crashed / Not Recovering

**Symptoms:**
- Agent process no longer running
- Children agents become orphaned
- Tasks assigned to agent stuck in "Blocked"
- Supervisor tree shows dead agent

**Root Causes:**
1. Panic in agent code
2. Out of memory (OOM)
3. Segmentation fault (unsafe code)
4. Supervisor misconfiguration

**Diagnosis Steps:**

1. Check if agent process exists:
```bash
pgrep -f "ggen-agent" | grep {agent_id}
# If no output, process is dead

# Check system logs for OOM
dmesg | grep -i "killed process"
journalctl -p err --since "1 hour ago"
```

2. Check supervisor state:
```bash
curl http://localhost:8080/api/agents/{agent_id}/supervisor
# Expected: state: "alive", restarts: 0
# Alert if: restarts > 3 in 5 minutes = crash loop
```

3. Examine core dump (if available):
```bash
# Enable core dumps
ulimit -c unlimited

# Find core dump
find /var/crash -name "core.*" -mtime -1

# Analyze with gdb
gdb /usr/bin/ggen-agent /var/crash/core.12345
(gdb) bt full  # Full backtrace
(gdb) info locals  # Local variables at crash
```

4. Check logs:
```bash
journalctl -u ggen-agent -n 50 --no-pager
# Look for panic, OOM, segfault messages
```

**Resolution:**

**If Crash Loop (restarts > 3/5min):**
```bash
# Stop supervisor from restarting
curl -X POST http://localhost:8080/api/agents/{agent_id}/disable-restart

# Investigate root cause
# Fix code issue or resource allocation

# Re-enable
curl -X POST http://localhost:8080/api/agents/{agent_id}/enable-restart

# Manually restart
systemctl restart ggen-agent
```

**If Out of Memory:**
```bash
# Increase heap size
export RUST_LOG=ggen_a2a=debug
export GGEN_HEAP_MB=2048

# Restart
systemctl restart ggen-agent

# Monitor memory
watch -n 1 'ps aux | grep ggen-agent'
```

**If Code Panic:**
```bash
# Get backtrace
RUST_BACKTRACE=full journalctl -u ggen-agent -n 100

# Fix panic in code
# Re-deploy
cargo make check && cargo deploy
```

**Prevention:**
- Set memory limits: `agent.memory_limit_mb = 1024`
- Set restart limits: `supervisor.max_restarts = 3, restart_window = 5m`
- Enable core dumps for debugging
- Regular integration testing
- Use Chicago TDD (catch issues in tests, not production)

---

### Issue: Message Not Delivered

**Symptoms:**
- Task assignment not received
- Tool invocation returns "message timeout"
- Communication logs show "sent but not ack"
- Intermittent failures

**Root Causes:**
1. Network partition or latency
2. Message queue full (backpressure)
3. Receiver crashed after send
4. Message serialization issue

**Diagnosis Steps:**

1. Check network connectivity:
```bash
ping -c 5 {agent_host}
traceroute {agent_host}
netstat -tnp | grep ESTABLISHED
```

2. Check message queue:
```bash
curl http://localhost:8080/metrics | grep queue
# queue_depth{service="..."} should be < 1000
# queue_processed_total should be increasing
```

3. Check transport logs:
```bash
grep "Message.*{message_id}" /var/log/ggen/transport.log
# Should show: SENT → ACK → DELIVERED
# If missing ACK: receiver crashed or offline
```

4. Verify envelope:
```bash
# Check message was valid JSON
curl http://localhost:8080/api/messages/{message_id}
# Verify: id, timestamp, source, destination, metadata
```

**Resolution:**

**If Network Issue:**
```bash
# Check DNS
nslookup {agent_host}

# Test connectivity
curl -v http://{agent_host}:8080/health

# If behind load balancer
# Verify sticky session or state sharing
```

**If Queue Full (Backpressure):**
```bash
# Check queue depth
curl http://localhost:8080/metrics | grep queue_depth

# If > max, reduce load or increase capacity
# Temporarily reduce task submission rate
# Or scale up receivers

# Check backpressure config
cat /etc/ggen/backpressure.toml
# Increase queue_max_depth if appropriate
```

**If Message Serialization:**
```bash
# Decode message
curl http://localhost:8080/api/messages/{message_id} | jq .

# Validate schema
ggen validate --message message.json --schema task-message.json

# Check for non-UTF8 binary in artifact
hexdump -C /path/to/artifact | head
```

**Prevention:**
- Enable message tracing: `transport.tracing = true`
- Set timeouts: `message.ack_timeout_ms = 5000`
- Monitor queue depth: alert if > 80% capacity
- Test network resilience: chaos engineering

---

## Coordination Issues

### Issue: Consensus Stalled

**Symptoms:**
- Task completion not verified
- Receipt generation stuck
- PBFT primary appears slow
- Replicas show divergent view numbers

**Root Causes:**
1. Primary replica is slow/faulty
2. Network partition between replicas
3. Byzantine replica causing view changes
4. Timeout too aggressive or too lenient

**Diagnosis Steps:**

1. Check PBFT phase:
```bash
curl http://localhost:8080/api/consensus/status
# Response: {
#   "current_view": 42,
#   "current_phase": "commit" | "prepare" | "preprepare" | "viewchange",
#   "replicas": [
#     {"id": 1, "state": "primary", "view": 42},
#     {"id": 2, "state": "replica", "view": 42},
#     ...
#   ]
# }
```

2. Check replica health:
```bash
for replica in {1..4}; do
  curl http://replica-$replica:5000/health
  # If any are down, consensus cannot proceed
done
```

3. Check message flow:
```bash
grep "PBFT" /var/log/ggen/consensus.log | tail -50
# Should show: PrePrepare → Prepare → Commit
# If stuck on Prepare: not enough Prepare msgs (< 2f+1=3)
```

4. Check view number divergence:
```bash
curl http://localhost:8080/api/consensus/replicas | jq '.[] | {id, view, phase}'
# All should have same view and phase
```

**Resolution:**

**If Primary is Slow:**
```bash
# Check primary replica CPU/memory
curl http://replica-1:5000/metrics | grep -E "cpu|memory"

# If high load, trigger view change
curl -X POST http://localhost:8080/api/consensus/trigger-viewchange

# Wait for new primary election (10-20 seconds)
sleep 20
curl http://localhost:8080/api/consensus/status
```

**If Replica Down:**
```bash
# Restart faulty replica
systemctl restart ggen-consensus-replica@{replica_id}

# Consensus will recover once replica rejoins (assumes 3f+1 tolerance)
# Monitor recovery
watch -n 1 'curl http://localhost:8080/api/consensus/status | jq .replicas'
```

**If Network Partition:**
```bash
# Check network between replicas
for i in {1..4}; do
  for j in {1..4}; do
    echo -n "Replica $i → $j: "
    nc -w 1 replica-$j 5000 < /dev/null && echo "OK" || echo "FAIL"
  done
done

# If partition detected:
# - Quorum (2f+1=3) in one partition will continue
# - Minority (1 or 2) partition will stall
# Fix underlying network issue, then restart stalled replicas
```

**Prevention:**
- Monitor replica health: alert if any down > 30s
- Check view changes: alert if > 1 view change per minute
- Set timeout conservatively: `consensus.timeout_ms = 5000`
- Regular failover testing: kill primary, verify new election

---

### Issue: Domain Imbalance

**Symptoms:**
- One domain has 100+ pending tasks
- Other domains idle
- Resource allocation unfair
- Response times increasing for loaded domain

**Root Causes:**
1. Task decomposition skewed toward one domain
2. Some domains execute slower (bottleneck)
3. Work queue doesn't distribute across domains
4. Domain executor misconfigured

**Diagnosis:**

```bash
# Check pending tasks per domain
curl http://localhost:8080/api/domains/stats | jq '.[] | {domain, pending_tasks, avg_latency}'

# Check executor capacity
curl http://localhost:8080/api/executors/stats | jq '.[] | {domain, workers_total, workers_busy}'
```

**Resolution:**

1. **Scale slow domain:**
```bash
# Add more workers to slow domain
curl -X POST http://localhost:8080/api/domains/{domain}/scale \
  -H "Content-Type: application/json" \
  -d '{"workers": 4}'

# Verify
curl http://localhost:8080/api/executors/stats | grep {domain}
```

2. **Rebalance tasks:**
```bash
# If task decomposition is skewed, adjust planner
# Edit goal decomposition logic in `ggen-a2a-mcp/src/planner.rs`
# Re-deploy

# Or manually reassign pending tasks
curl -X POST http://localhost:8080/api/tasks/{task_id}/reassign \
  -H "Content-Type: application/json" \
  -d '{"domain": "better_distributed_domain"}'
```

**Prevention:**
- Monitor domain stats: alert if pending > 2x average
- Regular rebalancing: run `ggen rebalance --all-domains` daily
- Capacity planning: track historical load by domain

---

### Issue: Tool Discovery Failing

**Symptoms:**
- LLM says "tool not found"
- MCP tool list returns empty or partial
- New tools not discoverable
- Stale tools still listed

**Root Causes:**
1. Tool not registered in registry
2. Tool crashed, not re-registered
3. Registry out of sync
4. Tool schema validation failed

**Diagnosis:**

```bash
# List all registered tools
curl http://localhost:8080/api/tools/list | jq '.tools[] | {name, status}'

# Check specific tool
curl http://localhost:8080/api/tools/{tool_name}

# Check registration logs
grep "tool.*register" /var/log/ggen/registry.log | tail -20
```

**Resolution:**

**If Tool Not Registered:**
```bash
# Check if tool service is running
systemctl status ggen-tool-{tool_name}

# If not running, start it
systemctl start ggen-tool-{tool_name}

# Check registration occurred
grep "Tool.*registered" /var/log/ggen/registry.log
```

**If Tool Crashed:**
```bash
# Supervisor should auto-restart
# Monitor restart count
curl http://localhost:8080/api/tools/{tool_name}/supervisor | jq .restarts

# If restart loop, check logs
journalctl -u ggen-tool-{tool_name} | tail -50
```

**If Registry Out of Sync:**
```bash
# Force refresh
curl -X POST http://localhost:8080/api/tools/refresh

# Verify all tools re-registered
curl http://localhost:8080/api/tools/list | jq '.tools | length'
```

**Prevention:**
- Health checks: `ggen health --service=registry` every 30s
- Tool lifecycle tests: verify register → execute → deregister
- Schema validation: validate tool JSON-schema at registration time

---

## Performance Issues

### Issue: High Latency

**Symptoms:**
- Task completion taking > 10 seconds
- User-facing API responses > 1 second
- Consensus rounds taking > 30 seconds
- Throughput < 100 tasks/min

**Root Causes:**
1. Network latency (high RTT)
2. Consensus inefficient (too many replicas, high packet loss)
3. Task executor slow (blocking operations, lock contention)
4. Resource exhaustion (CPU, memory, I/O)

**Diagnosis:**

```bash
# Measure P99 latency
curl http://localhost:8080/metrics | grep latency_p99

# Break down by component
curl http://localhost:8080/metrics | grep -E "consensus|executor|transport" | grep duration

# Check network latency to replicas
for replica in {1..4}; do
  echo "Replica $replica:"
  ping -c 5 replica-$replica | grep "avg="
done

# Check CPU/memory usage
top -b -n 1 | grep ggen

# Check disk I/O
iostat -x 1 5
```

**Resolution:**

**If Network Latency:**
```bash
# Measure per-hop latency
mtr -r -c 100 consensus-server

# If high RTT between datacenters
# - Move replicas closer
# - Use regional consensus clusters
# - Increase timeout to account for latency
```

**If Consensus Slow:**
```bash
# Check number of replicas
curl http://localhost:8080/api/consensus/status | jq '.replicas | length'

# If > 7 replicas, consensus slows down
# Solution: use proxy model
# - Primary replica handles consensus
# - Others query primary for commitment status

# Or reduce consensus frequency
# - Batch multiple tasks into single consensus round
```

**If Executor Slow:**
```bash
# Profile executor
RUST_LOG=ggen_a2a=trace cargo run --release --example profile-executor

# Check for lock contention
grep "RwLock" /var/log/ggen/executor.log

# Increase worker threads
curl -X POST http://localhost:8080/api/executors/scale \
  -d '{"worker_threads": 16}'

# Remove blocking operations
# Review executor code for io::blocking or sleep
```

**If Resource Exhausted:**
```bash
# Check what's consuming
du -sh /var/log/ggen/*
free -h
ps aux | grep ggen | head -5

# Clean up if needed
# - Archive old logs: ggen logs archive --before 7d
# - Clear caches: ggen cache clear
# - Restart services: systemctl restart ggen-agent
```

**Prevention:**
- Set SLO targets: `consensus.slo_ms = 1000, executor.slo_ms = 5000`
- Monitor latency: alert if P99 > 2x baseline
- Capacity planning: track growth, scale before limits
- Regular profiling: monthly flame graphs

---

### Issue: Low Throughput

**Symptoms:**
- < 100 tasks/min processed
- Task queue growing
- CPU/memory under-utilized
- Agent workers idle

**Root Causes:**
1. Task decomposition creating sequential dependencies
2. Work queue bottleneck (centralized)
3. Consensus limiting throughput (bottleneck)
4. Serialization/deserialization overhead

**Diagnosis:**

```bash
# Measure throughput
curl http://localhost:8080/metrics | grep "tasks_processed_total" | awk '{print $2}'

# Compare to capacity
curl http://localhost:8080/api/executors/stats | jq '.[] | {domain, capacity, utilization}'
# If utilization < 50% with high pending: bottleneck elsewhere

# Check task dependencies
curl http://localhost:8080/api/tasks/pending | jq '.[] | select(.dependencies | length > 0)' | wc -l
# If > 50% have dependencies: decomposition issue

# Check consensus impact
curl http://localhost:8080/metrics | grep "consensus_rounds" | awk '{print $2}'
# If consensus time > 30% of total time: consensus is bottleneck
```

**Resolution:**

**If Sequential Dependencies:**
```bash
# Re-examine goal decomposition
# Goal: A depends on B depends on C (sequential)
# Solution: Parallelize where possible

# Example: "Improve health" goal
# Bad: Measure → Analyze → Plan → Execute (4 seq steps)
# Good: Measure & Analyze & Plan (parallel) → Execute (1 step)

# Update planner logic in ggen-a2a-mcp/src/planner.rs
# Test with improved parallelization
cargo make test-goal-parallelization
```

**If Work Queue Bottleneck:**
```bash
# Use distributed work queue instead of central
# Replace: SingleQueue
# With: PartitionedWorkQueue (one per domain)

# Or use push-based model instead of pull
# Assign tasks directly to workers
# Reduces coordination overhead
```

**If Consensus Bottleneck:**
```bash
# Batch consensus rounds
# Instead of verifying each task individually:
# - Collect 10 tasks
# - Single consensus round for batch
# - Reduces rounds by 10x

# Implement in ggen-consensus/src/pbft.rs
# Add batch_commit() method
```

**Prevention:**
- Target throughput: `agent.target_throughput_tasks_per_sec = 10`
- Monitor utilization: alert if < 50% utilized
- Regular capacity tests: `ggen bench --goal=complex --concurrent=4`
- Profiling: identify actual bottleneck monthly

---

## Testing Issues

### Issue: Flaky Tests

**Symptoms:**
- Tests pass sometimes, fail other times
- Failures non-deterministic
- Different failures on different runs
- Often related to timing or async code

**Root Causes:**
1. Race conditions in async code
2. Timing-dependent assertions
3. Shared mutable state between tests
4. Order-dependent tests

**Diagnosis:**

```bash
# Run test repeatedly
for i in {1..10}; do
  cargo test --test flaky_test 2>&1 | grep FAILED && echo "Run $i: FAILED" || echo "Run $i: OK"
done

# If inconsistent: likely race condition

# Run with different random seeds
RNG_SEED=1 cargo test --test flaky_test
RNG_SEED=2 cargo test --test flaky_test
# If different results: randomness issue
```

**Resolution:**

**If Race Condition:**
```rust
// Bad: Race condition
#[tokio::test]
async fn test_concurrent_tasks() {
    let state = Arc::new(Mutex::new(0));
    
    let h1 = tokio::spawn({
        let s = state.clone();
        async move { *s.lock().await += 1; }
    });
    let h2 = tokio::spawn({
        let s = state.clone();
        async move { *s.lock().await += 1; }
    });
    
    h1.await.unwrap();
    h2.await.unwrap();
    
    // Race: might be 1 or 2 depending on timing
    assert_eq!(*state.lock().await, 2);
}

// Good: Use barrier to synchronize
#[tokio::test]
async fn test_concurrent_tasks_fixed() {
    let state = Arc::new(Mutex::new(0));
    let barrier = Arc::new(tokio::sync::Barrier::new(2));
    
    let h1 = tokio::spawn({
        let s = state.clone();
        let b = barrier.clone();
        async move {
            b.wait().await; // Wait for both to start
            *s.lock().await += 1;
        }
    });
    
    // Similar for h2...
    
    h1.await.unwrap();
    // Now deterministic: both execute increment atomically
    assert_eq!(*state.lock().await, 2);
}
```

**If Timing-Dependent:**
```rust
// Bad: Timing assumption
#[tokio::test]
async fn test_timeout() {
    let result = operation_with_timeout(Duration::from_millis(100)).await;
    tokio::time::sleep(Duration::from_millis(50)).await;
    assert!(result.is_pending()); // Flaky: timing varies
}

// Good: Use tokio::time::pause()
#[tokio::test]
async fn test_timeout_deterministic() {
    tokio::time::pause(); // Pause real time
    
    let result = operation_with_timeout(Duration::from_millis(100)).await;
    tokio::time::advance(Duration::from_millis(50)).await;
    assert!(result.is_pending()); // Deterministic: time is controlled
    
    tokio::time::advance(Duration::from_millis(100)).await;
    assert!(result.is_done()); // Deterministic: after timeout
}
```

**Prevention:**
- Use `tokio::time::pause()` for async tests
- Avoid `thread::sleep()` (use `tokio::time::sleep()`)
- Synchronize threads with barriers or channels
- Run tests in random order: `cargo test -- --test-threads=1 --shuffle`
- Use determinism seed: `RNG_SEED=42 cargo test`

---

### Issue: Tests Running Too Slowly

**Symptoms:**
- Full test suite takes > 60 seconds
- Single test takes > 5 seconds
- Sequential execution < parallel execution (indicates shared resource)

**Root Causes:**
1. Database setup/teardown overhead
2. Network I/O in tests (should mock or use testcontainers)
3. Unnecessary sleeps or waits
4. Inefficient algorithms in test code

**Diagnosis:**

```bash
# Measure test time
time cargo test --release

# See which tests are slow
cargo test --release -- --nocapture --test-threads=1 2>&1 | grep "test result"

# Profile test
cargo test --release --lib -- --nocapture --test-threads=1 2>&1 | awk '/^test/{print prev} {prev=$0}'
```

**Resolution:**

**If Database Slow:**
```rust
// Bad: Fresh database per test
#[tokio::test]
async fn test_user_creation() {
    let db = create_fresh_database().await; // 1-2 seconds
    let user = db.create_user("alice").await;
    assert_eq!(user.name, "alice");
}

// Good: Shared database with test isolation
#[tokio::test]
async fn test_user_creation() {
    let db = DATABASE.get_or_init(|| create_database()).await;
    let user = db.create_user(&unique_name()).await;
    assert_eq!(user.name, unique_name());
    db.cleanup(&user.id).await; // Cleanup only created records
}
```

**If Network I/O:**
```rust
// Bad: Real network
#[tokio::test]
async fn test_tool_execution() {
    let result = real_http_request("http://example.com").await; // 100ms+ each
}

// Good: Mock network
#[tokio::test]
async fn test_tool_execution() {
    let server = mockito::Server::new_async().await;
    let _m = server.mock("GET", "/endpoint")
        .with_status(200)
        .with_body(r#"{"result": "ok"}"#)
        .create_async()
        .await;
    
    let result = tool_executor.execute(&server.url()).await;
    assert_eq!(result, "ok");
}
```

**Prevention:**
- Target: full test < 30 seconds
- CI: run slow tests separately (integration vs unit)
- Cache: share expensive setup between tests
- Mock: never use real external services in unit tests
- Parallel: use `--test-threads=4` for I/O-bound tests

---

## Integration Issues

### Issue: MCP Tool Not Discoverable

**Symptoms:**
- LLM says "Tool {name} not found"
- MCP list tools doesn't include it
- Tool exists but not registered

**Root Causes:**
1. Tool service not started
2. Tool schema invalid JSON
3. Tool doesn't implement MCP protocol
4. Registry not querying tool service

**Diagnosis:**

```bash
# Check if tool service running
systemctl status ggen-tool-{tool_name}

# Check if tool responds to health
curl http://localhost:9000/health

# Check tool registration
curl http://localhost:8080/api/tools/list | jq '.tools[] | select(.name == "{tool_name}")'

# Check tool schema
curl http://localhost:9000/schema
```

**Resolution:**

```bash
# 1. Start tool service
systemctl start ggen-tool-{tool_name}

# 2. Verify health
curl http://localhost:9000/health

# 3. Register manually
curl -X POST http://localhost:8080/api/tools/register \
  -H "Content-Type: application/json" \
  -d @{tool_name}-schema.json

# 4. Verify discoverable
curl http://localhost:8080/api/tools/{tool_name}
```

---

### Issue: RDF Validation Failures

**Symptoms:**
- `ggen validate` fails
- SHACL constraint violations
- Generated code differs from spec
- Code archaeology broken

**Root Causes:**
1. RDF syntax error (missing triples)
2. SHACL constraint violation
3. Stale .md file (doesn't match .ttl)
4. Ontology mismatch

**Diagnosis:**

```bash
# Validate RDF syntax
ggen validate --file spec.ttl --format turtle

# Check SHACL constraints
ggen validate --file spec.ttl --shacl constraints.ttl --format turtle

# Compare .ttl to generated .md
ggen diff --spec spec.ttl --generated spec.md
```

**Resolution:**

1. **Fix RDF syntax:**
```bash
# Check N-Triples output (simpler than Turtle)
rapper -i turtle -o ntriples spec.ttl | head -20

# Fix Turtle and re-validate
vim spec.ttl
ggen validate --file spec.ttl
```

2. **Fix SHACL violations:**
```bash
# See which shapes failed
ggen validate --file spec.ttl --shacl shapes.ttl --verbose

# Fix violations in spec.ttl
vim spec.ttl

# Re-validate
ggen validate --file spec.ttl
```

3. **Regenerate .md from .ttl:**
```bash
# Never edit .md directly!
# Always edit .ttl, then generate .md
ggen speckit-render --file spec.ttl --output spec.md

# Verify match
diff spec.md spec.md.backup
```

---

## Summary Checklist

| Component | Check | Alert Threshold |
|-----------|-------|-----------------|
| Agent | Stale running tasks | > 5 minutes |
| Agent | Crash restarts | > 3 in 5 min |
| Consensus | View changes | > 1 per minute |
| Consensus | Stalled phase | > 30 seconds |
| Domain | Pending tasks | > 2x average |
| Transport | Queue depth | > 80% capacity |
| Executor | Latency P99 | > 2x SLO |
| Executor | Throughput | < 2x SLO |

