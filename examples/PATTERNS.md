# Distributed Systems Pattern Library - ggen v6.0.0

## Table of Contents

1. [Fault Tolerance Patterns](#fault-tolerance-patterns)
2. [Coordination Patterns](#coordination-patterns)
3. [Communication Patterns](#communication-patterns)
4. [Learning Patterns](#learning-patterns)

---

## Fault Tolerance Patterns

### 1. Supervisor Tree Pattern

**Problem:** Tasks may fail; we need automatic recovery without manual intervention.

**Solution:** Hierarchical supervision with exponential backoff recovery strategies.

```
Supervisor (root)
├── Agent-1 Supervisor (watches workers)
│   ├── Agent-1-Worker-1
│   ├── Agent-1-Worker-2
│   └── Agent-1-Worker-3
├── Agent-2 Supervisor
│   └── Agent-2-Worker-1
└── Consensus Supervisor
    ├── PBFT-Replica-1
    ├── PBFT-Replica-2
    └── PBFT-Replica-3
```

**Implementation in ggen:**

Location: `/Users/sac/ggen/crates/ggen-a2a-mcp/src/handlers.rs`

```rust
pub enum RecoveryStrategy {
    /// Restart the task from beginning (clear state, retry)
    Restart { max_retries: u32, backoff_millis: u64 },
    /// Escalate to parent supervisor for intervention
    Escalate { reason: String },
    /// Terminal failure - no recovery possible
    Abandon { reason: String },
}

impl SupervisorTree {
    pub async fn monitor_task(&self, task: Task) -> Result<TaskOutcome> {
        loop {
            match execute_task(&task).await {
                Ok(outcome) => return Ok(outcome),
                Err(e) => {
                    let strategy = self.determine_recovery(&e);
                    match strategy {
                        RecoveryStrategy::Restart { max_retries, backoff_millis } => {
                            if retries < max_retries {
                                tokio::time::sleep(Duration::from_millis(backoff_millis)).await;
                                continue;
                            }
                        }
                        RecoveryStrategy::Escalate { reason } => {
                            return self.escalate_to_parent(task, reason).await;
                        }
                        RecoveryStrategy::Abandon { reason } => {
                            return Err(FatalError::new(reason));
                        }
                    }
                }
            }
        }
    }
}
```

**Recovery Strategies:**
- `Restart` (exponential backoff: 100ms, 200ms, 400ms, 800ms, ...)
- `Escalate` (bubble to parent supervisor)
- `Abandon` (terminal failure, alert operator)

**Benefits:**
- Automatic recovery without manual intervention
- Prevents cascading failures
- Clear responsibility hierarchy
- Observable recovery attempts

**Trade-offs:**
- Adds complexity to system
- Recovery may not always be possible
- Requires careful timeout tuning

---

### 2. Circuit Breaker Pattern

**Problem:** Cascading failures; repeated attempts to failing services consume resources.

**Solution:** Three-state circuit (Closed → Open → Half-Open) for fast failure detection.

```
CLOSED (normal operation)
   ├─ Requests flow normally
   └─ Failures counted
       ↓
       [failures exceed threshold]
       ↓
OPEN (reject requests fast)
   ├─ All requests rejected immediately
   └─ [timeout expires]
       ↓
HALF-OPEN (test single request)
   ├─ Allow one request through
   ├─ [success → CLOSED]
   └─ [failure → OPEN]
```

**Implementation in ggen:**

Location: `/Users/sac/ggen/crates/ggen-backpressure/src/`

```rust
pub enum CircuitState {
    Closed { failure_count: u32 },
    Open { opened_at: Instant },
    HalfOpen,
}

pub struct CircuitBreaker {
    state: Arc<RwLock<CircuitState>>,
    failure_threshold: u32,
    timeout: Duration,
}

impl CircuitBreaker {
    pub async fn call<F, T>(&self, operation: F) -> Result<T>
    where
        F: Fn() -> BoxFuture<'static, Result<T>>,
    {
        match *self.state.read().await {
            CircuitState::Open { opened_at } => {
                if opened_at.elapsed() > self.timeout {
                    *self.state.write().await = CircuitState::HalfOpen;
                    self.execute(operation).await
                } else {
                    Err(CircuitBreakerError::Open)
                }
            }
            CircuitState::HalfOpen => {
                match self.execute(operation).await {
                    Ok(result) => {
                        *self.state.write().await = CircuitState::Closed { failure_count: 0 };
                        Ok(result)
                    }
                    Err(e) => {
                        *self.state.write().await = 
                            CircuitState::Open { opened_at: Instant::now() };
                        Err(e)
                    }
                }
            }
            CircuitState::Closed { mut failure_count } => {
                match self.execute(operation).await {
                    Ok(result) => Ok(result),
                    Err(e) => {
                        failure_count += 1;
                        if failure_count >= self.failure_threshold {
                            *self.state.write().await = 
                                CircuitState::Open { opened_at: Instant::now() };
                        } else {
                            *self.state.write().await = 
                                CircuitState::Closed { failure_count };
                        }
                        Err(e)
                    }
                }
            }
        }
    }
}
```

**Benefits:**
- Fast failure detection (fail fast, not slow)
- Prevents resource exhaustion
- Allows recovery time for failing service
- Observable state transitions
- Reduces cascading failures

**Trade-offs:**
- Threshold tuning is critical
- May reject valid requests during recovery
- Requires monitoring and alerting

---

### 3. Bulkhead Pattern (Isolation)

**Problem:** One failing subsystem brings down the entire system.

**Solution:** Partition resources; isolate failure domains with quotas and limits.

```
System
├── Compute Bulkhead (4 cores max)
├── Memory Bulkhead (100MB max)
├── Network Bulkhead (1Mbps max)
└── Storage Bulkhead (10GB max)
```

**Implementation in ggen:**

```rust
pub struct BulkheadConfig {
    compute_cores: u32,
    memory_mb: u32,
    network_mbps: u32,
    storage_gb: u32,
}

pub struct Bulkhead {
    config: BulkheadConfig,
    compute_used: Arc<AtomicU32>,
    memory_used: Arc<AtomicU32>,
    network_used: Arc<AtomicU32>,
    storage_used: Arc<AtomicU32>,
}

impl Bulkhead {
    pub async fn execute_with_limit<F, T>(
        &self,
        domain_id: &str,
        operation: F,
    ) -> Result<T, BulkheadError>
    where
        F: Fn() -> BoxFuture<'static, Result<T>>,
    {
        // Check if we have capacity
        if self.compute_used.load(Ordering::SeqCst) >= self.config.compute_cores {
            return Err(BulkheadError::ComputeExhausted);
        }

        // Reserve resource
        self.compute_used.fetch_add(1, Ordering::SeqCst);

        // Execute with timeout
        let result = tokio::time::timeout(
            Duration::from_secs(30),
            operation()
        ).await;

        // Release resource
        self.compute_used.fetch_sub(1, Ordering::SeqCst);

        result?
    }
}
```

**Mechanisms:**
- Thread pools with fixed sizes
- Memory limits per domain
- Network rate limiting
- Storage quotas

**Benefits:**
- Isolates failures to specific domains
- Prevents resource starvation
- Fair resource allocation
- Predictable degradation

---

### 4. Graceful Degradation

**Problem:** System performance degrades under load; need to provide reduced service rather than failure.

**Solution:** Serve less functionality instead of failing completely.

**Example Scenarios:**
```rust
// High load scenario
if system_load > 80% {
    // Degrade: skip non-critical tasks
    skip_analytics_collection();
    skip_cache_warmup();
    reduce_batch_sizes();
} else if system_load > 95% {
    // Extreme degradation: read-only mode
    return_cached_results_only();
    reject_write_operations();
}
```

**Benefits:**
- User experience degrades gracefully
- System remains partially functional
- Easier recovery as load decreases

---

### 5. State Recovery Pattern

**Problem:** Failures may leave system in inconsistent state; recovery needs to restore invariants.

**Solution:** Checkpoint state regularly, recover from last checkpoint.

```rust
pub struct StateCheckpoint {
    task_id: Uuid,
    state: TaskState,
    artifacts: HashMap<String, Artifact>,
    timestamp: DateTime<Utc>,
    hash: [u8; 32],
}

impl StateRecovery {
    pub async fn recover_from_failure(
        task_id: Uuid,
        failure: &str,
    ) -> Result<Task> {
        // Load last checkpoint
        let checkpoint = self.load_checkpoint(task_id).await?;

        // Verify checkpoint integrity
        checkpoint.verify_hash()?;

        // Restore to checkpoint state
        let task = Task::from_checkpoint(&checkpoint)?;

        // Log recovery event
        self.log_recovery_event(task_id, failure, &checkpoint);

        Ok(task)
    }
}
```

**Benefits:**
- Quick recovery from failures
- Prevents data loss
- Maintains consistency invariants

---

## Coordination Patterns

### 1. Leader Election

**Problem:** Distributed systems need a single coordinator; if that coordinator fails, system stalls.

**Solution:** Dynamic leader election ensures always one active leader.

**Algorithm:** Raft-based leader election

```rust
pub enum LeaderState {
    /// Voting for a leader
    Follower { voted_for: Option<ReplicaId> },
    /// Candidate for leadership
    Candidate { votes_received: usize },
    /// Elected leader
    Leader,
}

pub struct LeaderElection {
    current_term: u64,
    voted_for: Option<ReplicaId>,
    state: LeaderState,
    election_timeout: Duration,
}

impl LeaderElection {
    pub async fn hold_election(&mut self, replicas: &[ReplicaId]) -> Result<ReplicaId> {
        self.current_term += 1;
        self.state = LeaderState::Candidate { votes_received: 1 }; // Vote for self

        for replica in replicas {
            if self.request_vote(*replica).await? {
                if let LeaderState::Candidate { votes_received } = &mut self.state {
                    *votes_received += 1;
                    if *votes_received > replicas.len() / 2 {
                        self.state = LeaderState::Leader;
                        return Ok(*replica); // Simplified
                    }
                }
            }
        }

        Err(ElectionError::NoQuorum)
    }
}
```

**Uses in ggen:**
- Task distribution coordinator
- Consensus primary selection
- Resource allocation leader

---

### 2. Consensus (PBFT)

**Problem:** Distributed systems must agree on truth even if some nodes are faulty.

**Solution:** PBFT (Practical Byzantine Fault Tolerance) for Byzantine agreement.

**Phases:**
```
1. PrePrepare: Primary sends proposal
2. Prepare: Replicas acknowledge (need 2f+1)
3. Commit: Finalize decision (need 2f+1)
4. ViewChange: Elect new primary if needed
```

**Consensus Threshold:** 2f+1 (need f+1 honest nodes + f for quorum)

**Example:** 4 replicas tolerate 1 Byzantine fault:
```
Replica 1 (Primary): "Task X completed"
Replica 2: "I agree" [Prepare]
Replica 3: "I agree" [Prepare]
Replica 4: "I disagree" [Prepare] (Byzantine or slow)

Primary sees 3 Prepare msgs (≥ 2f+1 = 3)
→ Sends Commit
→ All replicas finalize
→ Task result is COMMITTED (cannot change)
```

**Implementation in ggen:**

Location: `/Users/sac/ggen/crates/ggen-consensus/src/pbft.rs`

---

### 3. Work Queue Pattern

**Problem:** Fair task distribution; prevent some agents from being overloaded while others are idle.

**Solution:** Centralized work queue with pull-based distribution.

```rust
pub struct WorkQueue {
    queue: VecDeque<Task>,
    workers: HashMap<WorkerId, WorkerState>,
}

impl WorkQueue {
    pub async fn assign_work(&mut self, worker: WorkerId) -> Option<Task> {
        // Only assign if worker not busy
        if let Some(WorkerState::Idle) = self.workers.get(&worker) {
            let task = self.queue.pop_front()?;
            self.workers.insert(worker, WorkerState::Busy);
            Some(task)
        } else {
            None
        }
    }

    pub async fn report_completion(&mut self, worker: WorkerId, result: TaskResult) {
        self.workers.insert(worker, WorkerState::Idle);
        // Task moved to completion queue
    }
}
```

**Benefits:**
- Fair work distribution
- No single worker overloaded
- Scales with number of workers

---

### 4. Fan-Out/Fan-In Pattern

**Problem:** Many tasks depend on results from multiple subtasks.

**Solution:** Decompose into parallel subtasks, then aggregate results.

```rust
pub async fn fan_out_fan_in(goal: Goal) -> Result<GoalResult> {
    // Fan-out: create subtasks
    let subtasks = goal.decompose_into_subtasks();
    let handles: Vec<_> = subtasks
        .into_iter()
        .map(|task| tokio::spawn(execute_task(task)))
        .collect();

    // Wait for all (fan-in)
    let results = futures::future::join_all(handles).await;

    // Aggregate
    let aggregated = aggregate_results(results)?;
    Ok(GoalResult::from_aggregated(aggregated))
}
```

**Example:** "Improve health domain"
- Subtask 1: Measure current health (parallel)
- Subtask 2: Identify improvement areas (parallel)
- Subtask 3: Get expert recommendations (parallel)
- Aggregate all results into health plan

**Benefits:**
- Parallel execution improves throughput
- Natural decomposition of complex goals
- Aggregate results for holistic view

---

### 5. Saga Pattern (Distributed Transactions)

**Problem:** Multi-step operations may partially fail; need all-or-nothing or compensating actions.

**Solution:** Saga - sequence of local transactions with compensating actions.

```rust
pub struct Saga {
    steps: Vec<SagaStep>,
    compensations: Vec<CompensationAction>,
}

pub struct SagaStep {
    action: Box<dyn Fn() -> BoxFuture<'static, Result<()>>>,
    compensation: Box<dyn Fn() -> BoxFuture<'static, Result<()>>>,
}

impl Saga {
    pub async fn execute(&mut self) -> Result<()> {
        for (i, step) in self.steps.iter_mut().enumerate() {
            if let Err(e) = (step.action)().await {
                // Compensate backwards
                for j in (0..i).rev() {
                    let compensation = &self.steps[j].compensation;
                    let _ = compensation().await; // Best effort
                }
                return Err(e);
            }
        }
        Ok(())
    }
}

// Example: Book flight + hotel + car rental
let mut saga = Saga::new()
    .add_step(
        || book_flight(flight_id),
        || cancel_flight(flight_id),
    )
    .add_step(
        || book_hotel(hotel_id),
        || cancel_hotel(hotel_id),
    )
    .add_step(
        || book_car(car_id),
        || cancel_car(car_id),
    );

saga.execute().await?; // All or nothing with compensations
```

**Benefits:**
- Handles partial failures gracefully
- Maintains consistency with compensating actions
- No distributed transactions needed

---

## Communication Patterns

### 1. Request-Reply

**Problem:** Synchronous communication; caller waits for response.

**Solution:** Send request, wait for reply with correlation ID.

```rust
pub struct RequestReply {
    id: String,
    request: Request,
    reply_rx: oneshot::Receiver<Response>,
}

impl RequestReply {
    pub async fn call(request: Request) -> Result<Response> {
        let (reply_tx, reply_rx) = oneshot::channel();
        let id = uuid::Uuid::new_v4().to_string();

        // Send request with correlation ID
        send_request(id.clone(), request).await?;

        // Wait for response
        let response = tokio::time::timeout(
            Duration::from_secs(30),
            reply_rx,
        ).await??;

        Ok(response)
    }
}
```

**Implementation in ggen:** MCP Tool calls, Task execution completion

---

### 2. Publish-Subscribe

**Problem:** One-to-many communication; decouples producers from consumers.

**Solution:** Event broker handles subscriptions and message distribution.

```rust
pub struct EventBus {
    subscribers: HashMap<EventType, Vec<Subscriber>>,
}

impl EventBus {
    pub fn publish(&self, event: Event) {
        if let Some(subs) = self.subscribers.get(&event.event_type) {
            for sub in subs {
                sub.send(event.clone());
            }
        }
    }

    pub fn subscribe(&mut self, event_type: EventType, subscriber: Subscriber) {
        self.subscribers
            .entry(event_type)
            .or_insert_with(Vec::new)
            .push(subscriber);
    }
}
```

**Implementation in ggen:** YAWL event publishing, domain status changes

---

### 3. Message Queue

**Problem:** Asynchronous communication with persistence; handle load spikes.

**Solution:** Durable queue holds messages until processed.

```rust
pub struct MessageQueue {
    messages: VecDeque<Message>,
    persistence: Storage,
}

impl MessageQueue {
    pub async fn enqueue(&mut self, message: Message) -> Result<()> {
        // Persist to storage
        self.persistence.save(&message).await?;

        // Add to in-memory queue
        self.messages.push_back(message);

        Ok(())
    }

    pub async fn dequeue(&mut self) -> Option<Message> {
        self.messages.pop_front()
    }
}
```

**Benefits:**
- Decouples producers and consumers by time
- Handles load spikes
- Persistence prevents message loss

---

### 4. RPC (Remote Procedure Call)

**Problem:** Call functions on remote systems as if they were local.

**Solution:** Serialize arguments, invoke remotely, deserialize results.

```rust
pub struct RpcClient {
    endpoint: String,
}

impl RpcClient {
    pub async fn call<T: serde::Serialize, R: serde::de::DeserializeOwned>(
        &self,
        method: &str,
        params: T,
    ) -> Result<R> {
        let body = serde_json::json!({
            "jsonrpc": "2.0",
            "id": uuid::Uuid::new_v4(),
            "method": method,
            "params": params,
        });

        let response = reqwest::Client::new()
            .post(&self.endpoint)
            .json(&body)
            .send()
            .await?;

        let result = response.json().await?;
        Ok(result)
    }
}
```

**Implementation in ggen:** MCP protocol (JSON-RPC 2.0)

---

### 5. Streaming

**Problem:** Send large amounts of data; prevent buffering entire response.

**Solution:** Stream results back incrementally.

```rust
pub async fn stream_large_result(
    task_id: Uuid,
) -> impl Stream<Item = Result<DataChunk>> {
    stream::iter(0..1000)
        .then(move |i| async move {
            let chunk = fetch_chunk(task_id, i).await?;
            Ok(chunk)
        })
        .throttle(Duration::from_millis(10))
}
```

**Benefits:**
- Low memory usage
- Progressive feedback
- Cancellation support

---

## Learning Patterns

### 1. Reinforcement Learning

**Problem:** Agents need to learn which tool sequences are most effective.

**Solution:** Reward successful patterns, penalize failures.

```rust
pub struct AgentLearning {
    tool_sequences: HashMap<ToolSequence, (u32, u32)>, // (successes, failures)
    rewards: HashMap<ToolSequence, f64>,
}

impl AgentLearning {
    pub fn record_success(&mut self, sequence: ToolSequence, reward: f64) {
        let entry = self.tool_sequences.entry(sequence.clone()).or_insert((0, 0));
        entry.0 += 1;
        
        let success_rate = entry.0 as f64 / (entry.0 + entry.1) as f64;
        self.rewards.insert(sequence, reward * success_rate);
    }

    pub fn record_failure(&mut self, sequence: ToolSequence) {
        let entry = self.tool_sequences.entry(sequence.clone()).or_insert((0, 0));
        entry.1 += 1;
        
        // Penalize failed sequence
        self.rewards.insert(sequence, 0.0);
    }

    pub fn select_best_sequence(&self, options: &[ToolSequence]) -> ToolSequence {
        options.iter()
            .max_by(|a, b| {
                self.rewards.get(*a).unwrap_or(&0.0)
                    .partial_cmp(self.rewards.get(*b).unwrap_or(&0.0))
                    .unwrap_or(Ordering::Equal)
            })
            .cloned()
            .unwrap_or_default()
    }
}
```

**Benefits:**
- Agents improve over time
- Learn effective patterns
- Adapt to changing conditions

---

### 2. Pattern Extraction

**Problem:** Discover common workflows from execution history.

**Solution:** Analyze sequences of executed tasks, identify patterns.

```rust
pub struct PatternExtractor {
    sequences: Vec<TaskSequence>,
    min_support: f64,
}

impl PatternExtractor {
    pub fn extract_patterns(&self) -> Vec<Pattern> {
        let mut patterns = HashMap::new();

        for sequence in &self.sequences {
            for window in sequence.windows(3) {
                let pattern = window.to_vec();
                *patterns.entry(pattern).or_insert(0) += 1;
            }
        }

        patterns.into_iter()
            .filter(|(_, count)| {
                *count as f64 / self.sequences.len() as f64 >= self.min_support
            })
            .map(|(pattern, _)| Pattern::from_sequence(pattern))
            .collect()
    }
}
```

**Example Patterns:**
- "Measure health → Identify issues → Create plan"
- "Create domain → Add workflows → Execute tasks"

---

### 3. Optimization Pattern

**Problem:** Many possible solutions; need to improve action selection.

**Solution:** Search for better strategies, validate with consensus.

```rust
pub struct ActionOptimizer {
    current_strategy: Vec<Action>,
    candidates: Vec<Vec<Action>>,
}

impl ActionOptimizer {
    pub async fn improve_strategy(&mut self) -> Result<()> {
        for candidate in &self.candidates {
            // Evaluate candidate
            let result = self.evaluate_strategy(candidate).await?;

            // Verify with consensus
            let consensus_result = self.verify_consensus(&result).await?;

            // Keep if better than current
            if consensus_result.score > self.current_score {
                self.current_strategy = candidate.clone();
                self.current_score = consensus_result.score;
            }
        }
        Ok(())
    }
}
```

---

### 4. Knowledge Sharing

**Problem:** Agents learn independently; discoveries not shared.

**Solution:** Agents publish patterns, others learn from shared knowledge base.

```rust
pub struct KnowledgeBase {
    patterns: HashMap<String, Pattern>,
    quality_scores: HashMap<String, f64>,
}

impl KnowledgeBase {
    pub async fn publish_pattern(&mut self, pattern: Pattern) -> Result<()> {
        // Verify pattern with consensus
        let consensus_score = self.verify_with_consensus(&pattern).await?;

        if consensus_score > 0.8 {
            self.patterns.insert(pattern.id.clone(), pattern);
            self.quality_scores.insert(pattern.id.clone(), consensus_score);
        }
        Ok(())
    }

    pub fn get_pattern(&self, domain: &str) -> Option<&Pattern> {
        self.patterns.values()
            .filter(|p| p.applies_to_domain(domain))
            .max_by(|a, b| {
                self.quality_scores.get(&a.id).unwrap_or(&0.0)
                    .partial_cmp(self.quality_scores.get(&b.id).unwrap_or(&0.0))
                    .unwrap_or(Ordering::Equal)
            })
    }
}
```

---

## Pattern Summary Table

| Pattern | Problem | Benefit | Trade-off |
|---------|---------|---------|-----------|
| **Supervisor Tree** | Tasks fail | Automatic recovery | Complexity |
| **Circuit Breaker** | Cascading failures | Fast failure detection | May reject valid requests |
| **Bulkhead** | Resource starvation | Isolation | Reduced throughput |
| **Leader Election** | Single point of failure | High availability | Coordination overhead |
| **Consensus (PBFT)** | Byzantine nodes | Strong guarantees | Network traffic |
| **Work Queue** | Unbalanced load | Fair distribution | Centralized bottleneck |
| **Fan-Out/Fan-In** | Sequential tasks | Parallelism | Complexity |
| **Saga** | Distributed transactions | ACID-like guarantees | Compensations needed |
| **Pub-Sub** | Tight coupling | Loose coupling | Message delivery latency |
| **RPC** | Remote calls | Simple interface | Synchronous waiting |
| **Streaming** | Large responses | Low memory | Incremental feedback |
| **Learning** | Static behavior | Improvement over time | Requires history |

---

## Conclusion

These patterns address core challenges in distributed systems: fault tolerance, coordination, communication, and continuous improvement. Combined in ggen, they enable autonomous agents to collaborate reliably while handling Byzantine faults, cascading failures, and dynamic optimization.

