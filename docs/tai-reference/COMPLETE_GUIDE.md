<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI Pattern Reference Implementation - Complete Guide](#tai-pattern-reference-implementation---complete-guide)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Philosophy](#philosophy)
    - [Why TAI?](#why-tai)
    - [The TAI Equation](#the-tai-equation)
    - [Core Principles](#core-principles)
  - [Architecture](#architecture)
    - [High-Level System Design](#high-level-system-design)
    - [Component Responsibilities](#component-responsibilities)
  - [The TAI Pattern](#the-tai-pattern)
    - [Phase 1: Signal Reception](#phase-1-signal-reception)
    - [Phase 2: Policy Evaluation](#phase-2-policy-evaluation)
    - [Phase 3: Action Execution (with Jidoka)](#phase-3-action-execution-with-jidoka)
    - [Phase 4: Async Processing (Kanban)](#phase-4-async-processing-kanban)
    - [Phase 5: Response Generation](#phase-5-response-generation)
  - [TPS Principles Applied](#tps-principles-applied)
    - [1. Jidoka (Autonomation)](#1-jidoka-autonomation)
    - [2. Kanban (Pull-Based Queuing)](#2-kanban-pull-based-queuing)
    - [3. Heijunka (Load Leveling)](#3-heijunka-load-leveling)
    - [4. Kaizen (Continuous Improvement)](#4-kaizen-continuous-improvement)
    - [5. Andon (Visual Signals)](#5-andon-visual-signals)
  - [Reference Systems](#reference-systems)
    - [System 1: Payment Processing](#system-1-payment-processing)
    - [System 2: Deployment Orchestration](#system-2-deployment-orchestration)
  - [Implementation Patterns](#implementation-patterns)
    - [Pattern 1: Policy as Code](#pattern-1-policy-as-code)
    - [Pattern 2: Circuit Breaker with Recovery](#pattern-2-circuit-breaker-with-recovery)
    - [Pattern 3: Metrics Collection](#pattern-3-metrics-collection)
    - [Pattern 4: Observability with Tracing](#pattern-4-observability-with-tracing)
  - [Best Practices](#best-practices)
    - [1. Policy Design](#1-policy-design)
    - [2. Circuit Breaker Configuration](#2-circuit-breaker-configuration)
    - [3. Metrics Design](#3-metrics-design)
    - [4. Testing Strategy](#4-testing-strategy)
    - [5. Production Deployment](#5-production-deployment)
  - [Troubleshooting](#troubleshooting)
    - [Problem 1: Circuit Breaker Stuck Open](#problem-1-circuit-breaker-stuck-open)
    - [Problem 2: High Fraud False Positives](#problem-2-high-fraud-false-positives)
    - [Problem 3: Slow Deployments](#problem-3-slow-deployments)
    - [Problem 4: Metrics Dashboard Empty](#problem-4-metrics-dashboard-empty)
  - [Advanced Topics](#advanced-topics)
    - [Advanced Topic 1: Distributed Tracing at Scale](#advanced-topic-1-distributed-tracing-at-scale)
    - [Advanced Topic 2: Multi-Region Circuit Breakers](#advanced-topic-2-multi-region-circuit-breakers)
    - [Advanced Topic 3: Policy Composition](#advanced-topic-3-policy-composition)
    - [Advanced Topic 4: Progressive Rollout with Percentage-Based Traffic](#advanced-topic-4-progressive-rollout-with-percentage-based-traffic)
  - [Conclusion](#conclusion)
  - [Additional Resources](#additional-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI Pattern Reference Implementation - Complete Guide

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Production-Ready

## Table of Contents

1. [Introduction](#introduction)
2. [Philosophy](#philosophy)
3. [Architecture](#architecture)
4. [The TAI Pattern](#the-tai-pattern)
5. [TPS Principles Applied](#tps-principles-applied)
6. [Reference Systems](#reference-systems)
7. [Implementation Patterns](#implementation-patterns)
8. [Best Practices](#best-practices)
9. [Troubleshooting](#troubleshooting)
10. [Advanced Topics](#advanced-topics)

---

## Introduction

The **TAI Pattern** is a deterministic, event-driven architecture pattern that combines:

- **Signal Processing** (incoming requests/events)
- **Policy Evaluation** (business rules and constraints)
- **Action Execution** (deterministic operations)

Enhanced with **Toyota Production System** (TPS) principles for:

- **Reliability** (circuit breakers, automatic recovery)
- **Efficiency** (load leveling, queuing)
- **Quality** (continuous improvement metrics)
- **Visibility** (alerts, tracing, observability)

This guide demonstrates TAI patterns applied to two domains:
1. **Payment Processing System** (financial transactions)
2. **Deployment Orchestration System** (infrastructure automation)

Both systems exhibit identical architectural principles while solving different problems.

---

## Philosophy

### Why TAI?

Traditional event-driven architectures often suffer from:
- **Lack of visibility** into request path
- **Cascading failures** without circuit breakers
- **Unpredictable load spikes** without leveling
- **Silent failures** without proper alerting
- **Poor debugging** without tracing

TAI solves these by making explicit:

1. **Signal** - What triggered the action? (observable input)
2. **Policy** - Should we proceed? (enforceable constraints)
3. **Action** - What happens next? (deterministic execution)

Combined with TPS principles that ensure:

- **Jidoka**: Automatic fault isolation (stop the line immediately)
- **Kanban**: Pull-based queuing (prevent overload)
- **Heijunka**: Load leveling (smooth operations)
- **Kaizen**: Metrics-driven improvement (data-driven decisions)
- **Andon**: Visual signals (immediate problem visibility)

### The TAI Equation

```
Result = Policy(Signal) → Jidoka(Action(Signal))
         ↓
         Kanban(Async Processing)
         ↓
         Heijunka(Load Level)
         ↓
         Kaizen(Metrics)
         ↓
         Andon(Alerts)
```

### Core Principles

1. **Determinism**: Same signal + same policy = same result (always)
2. **Visibility**: Complete tracing from signal to result
3. **Fail-Fast**: Detect problems immediately, don't hide them
4. **Recovery**: Automatic recovery without manual intervention
5. **Metrics**: Every decision generates measurable outcomes
6. **Alerts**: Threshold violations trigger immediate notification

---

## Architecture

### High-Level System Design

```
┌─────────────────────────────────────────────────────────────┐
│                   Client/External System                     │
└──────────────────────────┬──────────────────────────────────┘
                           │
                    Signal (HTTP/gRPC)
                           │
                           ▼
        ┌──────────────────────────────────────────┐
        │     TAI Orchestrator Service             │
        │  (Signal Reception & Routing)            │
        └────────────┬─────────────────────────────┘
                     │
        ┌────────────┴─────────────┬────────────┐
        │                          │            │
        ▼                          ▼            ▼
    ┌────────────┐         ┌────────────┐  ┌─────────────┐
    │  Policy    │         │  Jidoka    │  │   Kanban    │
    │ Evaluator  │         │  Circuit   │  │    Queue    │
    │            │         │  Breaker   │  │             │
    └────────────┘         └────────────┘  └─────────────┘
        │                       │                │
        ├───────────────────────┴────────────────┤
        │                                         │
        ▼                                         ▼
    ┌──────────────────────────────────────────────────────┐
    │          Action Executor                             │
    │  (Call external APIs, make state changes)            │
    └────────────┬─────────────────────────────────────────┘
                 │
        ┌────────┴─────────┬──────────────┐
        │                  │              │
        ▼                  ▼              ▼
    ┌──────────┐     ┌──────────┐   ┌──────────┐
    │ Heijunka │     │ Kaizen   │   │  Andon   │
    │ Load     │     │ Metrics  │   │  Alerts  │
    │ Leveling │     │ Track    │   │  Notify  │
    └──────────┘     └──────────┘   └──────────┘
        │                  │              │
        └──────────────────┴──────────────┘
                    │
                    ▼
        ┌──────────────────────────┐
        │  Tracing Service         │
        │ (OpenTelemetry)          │
        │  - Log all operations    │
        │  - Correlate requests    │
        │  - Track performance     │
        └──────────────────────────┘
```

### Component Responsibilities

| Component | Responsibility | Example |
|-----------|-----------------|---------|
| **Signal Handler** | Receive and parse input | HTTP POST endpoint |
| **Policy Evaluator** | Check constraints before action | Fraud detection |
| **Jidoka Circuit Breaker** | Fail fast when downstream unavailable | Payment gateway circuit breaker |
| **Kanban Queue** | Buffer work for async processing | Payment confirmation queue |
| **Heijunka Load Leveler** | Smooth request flow | Rate limiter |
| **Kaizen Metrics** | Track and aggregate metrics | Success rate, latency, fraud rate |
| **Andon Alerting** | Notify on threshold violations | High fraud rate alert |
| **Tracing/Observability** | Correlate full request path | OpenTelemetry trace ID |

---

## The TAI Pattern

### Phase 1: Signal Reception

```rust
pub async fn handle_signal(signal: Signal) -> Result<()> {
    // 1. Parse and validate signal structure
    validate_signal_schema(&signal)?;

    // 2. Generate trace ID for correlation
    let trace_id = generate_trace_id();

    // 3. Record signal received
    metrics.record_signal_received(&signal, &trace_id);

    // 4. Proceed to policy evaluation
    evaluate_policy(signal, trace_id).await
}
```

### Phase 2: Policy Evaluation

```rust
pub async fn evaluate_policy(signal: Signal, trace_id: String) -> Result<Decision> {
    // 1. Apply all policy rules
    let mut policy_violations = vec![];

    // Check rule 1
    if !check_constraint_1(&signal) {
        policy_violations.push("Constraint 1 violated");
    }

    // Check rule 2
    if !check_constraint_2(&signal) {
        policy_violations.push("Constraint 2 violated");
    }

    // Check rule N
    if !check_constraint_n(&signal) {
        policy_violations.push("Constraint N violated");
    }

    // 2. Generate decision
    let decision = if policy_violations.is_empty() {
        Decision::Approved
    } else {
        Decision::Rejected(policy_violations)
    };

    // 3. Record decision
    metrics.record_policy_decision(&decision, &trace_id);

    // 4. If rejected, return early
    if let Decision::Rejected(reasons) = &decision {
        andon.alert_policy_violation(&reasons);
        return Err(anyhow!("Policy violated: {}", reasons.join(", ")));
    }

    // 5. Proceed to action
    execute_action(signal, trace_id).await
}
```

### Phase 3: Action Execution (with Jidoka)

```rust
pub async fn execute_action(signal: Signal, trace_id: String) -> Result<ActionResult> {
    // 1. Check Jidoka circuit breaker before action
    circuit_breaker.can_execute()?;

    // 2. Execute action with timeout
    let action_result = tokio::time::timeout(
        Duration::from_secs(30),
        perform_action(&signal)
    ).await??;

    // 3. Record success
    circuit_breaker.record_success();

    // 4. Process result through Kanban/Heijunka
    if let Err(e) = process_result_async(&action_result).await {
        circuit_breaker.record_failure();
        andon.alert_action_failure(&e);
        return Err(e);
    }

    // 5. Record metrics
    metrics.record_action_success(&action_result, &trace_id);

    Ok(action_result)
}
```

### Phase 4: Async Processing (Kanban)

```rust
pub async fn process_result_async(result: &ActionResult) -> Result<()> {
    // 1. Enqueue result for async processing
    let queue_msg = QueueMessage::from(result);
    kanban_queue.enqueue(queue_msg).await?;

    // 2. Worker processes asynchronously
    // (decoupled from request/response cycle)

    // 3. Heijunka smooths processing rate
    heijunka_pool.process_with_load_leveling(queue_msg).await?;

    // 4. Kaizen tracks metrics
    metrics.record_async_processing(&result);

    Ok(())
}
```

### Phase 5: Response Generation

```rust
pub async fn generate_response(
    signal: Signal,
    action_result: ActionResult,
    metrics: &Metrics,
    trace_id: String
) -> Response {
    Response {
        // 1. Result data
        status: action_result.status,
        data: action_result.data,

        // 2. Metrics (for client awareness)
        processing_time_ms: metrics.get_latency(&trace_id),
        fraud_score: metrics.get_fraud_score(&trace_id),

        // 3. Tracing info (for debugging)
        trace_id: trace_id.clone(),
        timestamp: Utc::now(),
    }
}
```

---

## TPS Principles Applied

### 1. Jidoka (Autonomation)

**Definition**: Automatic detection and response to problems (stop the line immediately).

**Implementation**:

```rust
pub struct CircuitBreaker {
    state: RwLock<CircuitState>,
    failure_count: AtomicU64,
    failure_threshold: u64,
}

impl CircuitBreaker {
    pub fn can_execute(&self) -> Result<()> {
        match *self.state.read() {
            CircuitState::Closed => Ok(()),
            CircuitState::Open => Err(anyhow!("Circuit is open - service unavailable")),
            CircuitState::HalfOpen => Ok(()), // Allow probe request
        }
    }

    pub fn record_failure(&self) {
        let new_count = self.failure_count.fetch_add(1, Ordering::Relaxed) + 1;

        if new_count >= self.failure_threshold {
            // STOP THE LINE
            *self.state.write() = CircuitState::Open;
            alert!("Circuit breaker OPEN - service failure detected");
        }
    }
}
```

**Payment System Example**:
- 5 consecutive payment gateway failures → Circuit opens
- Subsequent payments fail fast (no 30s timeout)
- After recovery timeout → Circuit attempts HalfOpen
- 3 successful requests → Circuit closes

**Deployment System Example**:
- Health check shows > 5% error rate → Automatic rollback
- P99 latency > 1000ms → Automatic rollback
- Deployment halted immediately, no cascading failures

**Benefits**:
- Failures detected and contained quickly
- Prevents cascade failures across system
- Transparent to clients (clear error message)
- Automatic recovery reduces MTTR (Mean Time To Recovery)

### 2. Kanban (Pull-Based Queuing)

**Definition**: Limit work-in-progress through pull-based queuing (prevent overload).

**Implementation**:

```rust
pub struct KanbanQueue {
    queue: mpsc::Bounded<WorkItem>,
    active_work: AtomicU64,
    max_concurrent: u64,
}

impl KanbanQueue {
    pub async fn enqueue(&self, item: WorkItem) -> Result<()> {
        // Only enqueue if active work below limit
        if self.active_work.load(Ordering::Relaxed) >= self.max_concurrent {
            return Err(anyhow!("Queue full - max concurrent work exceeded"));
        }

        self.queue.send(item).await?;
        self.active_work.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }

    pub async fn dequeue(&self) -> Option<WorkItem> {
        let item = self.queue.recv().await?;
        self.active_work.fetch_sub(1, Ordering::Relaxed);
        Some(item)
    }
}
```

**Payment System Example**:
- Successful payment → Published to queue for confirmation
- Queue processes asynchronously (decoupled from HTTP response)
- Bounded queue size prevents memory bloat
- Multiple workers consume from queue in parallel

**Deployment System Example**:
- Deployments queued for sequential regional processing
- Only one region deployed at a time (sequential pull)
- Prevents cascading regional failures
- Allows previous region to reach stable state before next

**Benefits**:
- Decouples request from processing
- Prevents overload (bounded queues)
- Enables asynchronous processing
- Reduces system latency and variability

### 3. Heijunka (Load Leveling)

**Definition**: Smooth workload distribution to prevent spikes (level loading).

**Implementation**:

```rust
pub struct HeijunkaPool {
    workers: Vec<WorkerSlot>,
    current_index: AtomicUsize,
}

impl HeijunkaPool {
    pub async fn process_with_load_leveling(&self, work: WorkItem) -> Result<()> {
        // 1. Find least-loaded worker
        let worker_index = self.find_least_loaded_worker();
        let worker = &self.workers[worker_index];

        // 2. Add work (blocks if worker saturated - prevents spike)
        worker.channel.send(work).await?;

        // 3. Smooth distribution across all workers
        self.current_index.store(
            (worker_index + 1) % self.workers.len(),
            Ordering::Relaxed
        );

        Ok(())
    }
}
```

**Payment System Example**:
- Rate limiter: max 5 transactions/minute per customer
- Prevents burst spikes from single customer
- Smooths payment flow across system
- Protects against DDoS attacks

**Deployment System Example**:
- Canary stage (5% traffic, 5 min) - low risk, quick detection
- Staging stage (25% traffic, 10 min) - moderate exposure
- Production stage (100% traffic) - full deployment
- Gradual increase detects issues early with minimal blast radius

**Benefits**:
- Prevents spikes and overload
- Enables early failure detection
- Reduces latency variance
- Protects against DDoS/abuse

### 4. Kaizen (Continuous Improvement)

**Definition**: Metrics-driven continuous improvement through data.

**Implementation**:

```rust
pub struct KaizenMetrics {
    total_requests: AtomicU64,
    successful_requests: AtomicU64,
    failed_requests: AtomicU64,
    latency_histogram: Histogram,
}

impl KaizenMetrics {
    pub fn record_request(&self, duration: Duration, success: bool) {
        self.total_requests.fetch_add(1, Ordering::Relaxed);

        if success {
            self.successful_requests.fetch_add(1, Ordering::Relaxed);
        } else {
            self.failed_requests.fetch_add(1, Ordering::Relaxed);
        }

        self.latency_histogram.observe(duration.as_millis() as f64);
    }

    pub fn get_success_rate(&self) -> f64 {
        let total = self.total_requests.load(Ordering::Relaxed);
        let successful = self.successful_requests.load(Ordering::Relaxed);

        if total == 0 {
            0.0
        } else {
            (successful as f64 / total as f64) * 100.0
        }
    }

    pub fn get_p99_latency(&self) -> u64 {
        self.latency_histogram.percentile(0.99)
    }
}
```

**Payment System Metrics**:
- Success rate: 98%+ (track > 95% SLO)
- Fraud detection rate: 1.5% (track < 2% threshold)
- Average latency: 245ms (track < 500ms SLO)
- Circuit breaker trips: 3/month (track trends)

**Deployment System Metrics**:
- Deployment success rate: 98%+ (track > 95% SLO)
- Rollback frequency: 2/month (track trends)
- Average deployment time: 45 seconds (track regression)
- Stage progression: canary → staging → prod (100% completion)

**Data-Driven Actions**:
- Success rate declining? Investigate root causes
- Fraud rate increasing? Tighten policy rules
- Latency increasing? Scale system or optimize queries
- Rollback rate increasing? Review deployment process

**Benefits**:
- Data-driven decision making
- Trend detection and prediction
- Performance regression detection
- Validates effectiveness of changes

### 5. Andon (Visual Signals)

**Definition**: Immediate visibility into system problems (make problems visible).

**Implementation**:

```rust
pub struct AndonSignal {
    thresholds: AndonThresholds,
    active_alerts: RwLock<Vec<Alert>>,
}

impl AndonSignal {
    pub async fn check_thresholds(&self, metrics: &Metrics) {
        let mut alerts = self.active_alerts.write();

        // Check fraud rate threshold
        if metrics.fraud_rate() > self.thresholds.fraud_rate_percent {
            alerts.push(Alert {
                level: AlertLevel::Critical,
                message: format!(
                    "Fraud rate {:.2}% exceeds threshold {:.2}%",
                    metrics.fraud_rate(),
                    self.thresholds.fraud_rate_percent
                ),
                timestamp: Utc::now(),
            });
        }

        // Check latency threshold
        if metrics.p99_latency_ms() > self.thresholds.latency_threshold_ms {
            alerts.push(Alert {
                level: AlertLevel::High,
                message: format!(
                    "P99 latency {}ms exceeds threshold {}ms",
                    metrics.p99_latency_ms(),
                    self.thresholds.latency_threshold_ms
                ),
                timestamp: Utc::now(),
            });
        }

        // Notify on-call team
        for alert in &*alerts {
            notify_team(&alert).await;
        }
    }
}
```

**Payment System Andon Signals**:
- 🔴 CRITICAL: Fraud rate > 2% (immediate escalation)
- 🔴 CRITICAL: Circuit breaker open (service down)
- 🟡 HIGH: P99 latency > 5000ms (performance degradation)
- 🟡 HIGH: Error rate > 5% (unusual failures)
- 🟢 GREEN: Normal operation

**Deployment System Andon Signals**:
- 🔴 CRITICAL: Deployment failed (invalid state)
- 🔴 CRITICAL: Automatic rollback triggered (safety net)
- 🟡 HIGH: Health check marginal (5-10% error rate)
- 🟡 HIGH: Canary stage slow to progress (unknown risks)
- 🟢 GREEN: Successful deployment

**Alert Channels**:
- Email/SMS for critical alerts
- Slack for high-priority alerts
- Dashboard for operational awareness
- PagerDuty for on-call escalation

**Benefits**:
- Immediate problem visibility
- Prevents "silent failures"
- Enables quick response (MTTR improvement)
- Prevents SLO violations from going unnoticed

---

## Reference Systems

### System 1: Payment Processing

**Problem Statement**:
Process customer payments safely and efficiently while preventing fraud and protecting against cascading failures.

**Requirements**:
- Process 1000+ payments/minute
- Detect fraud in real-time (< 500ms)
- Prevent double-charging (idempotency)
- Audit trail for compliance
- Circuit break on payment gateway failure
- Alert on unusual patterns

**TAI Implementation**:

```
Signal: PaymentRequest {
  amount: 99.99,
  customer_id: "cust_123",
  currency: "USD",
  merchant_id: "merc_456"
}
    ↓
Policy: FraudDetection {
  - Blacklist check
  - Velocity check (5/min per customer)
  - Amount anomaly (flag > $5000)
  - Geolocation anomaly (multiple currencies)
  - Fraud score >= 50? REJECT
}
    ↓
Jidoka: PaymentGatewayCircuitBreaker {
  - Can execute? Check circuit state
  - If open: FAIL FAST
  - If half-open: Attempt recovery
  - If closed: PROCEED
}
    ↓
Action: CallPaymentGateway {
  - Invoke Stripe API
  - Record receipt in audit log
  - Publish to Kanban for confirmation
}
    ↓
Kanban: AsyncQueue {
  - Successful payments queued
  - Workers process confirmations
  - Decoupled from HTTP response
}
    ↓
Heijunka: RateLimiting {
  - Max 5 transactions/min per customer
  - Prevents burst spikes
  - Smooths load
}
    ↓
Kaizen: Metrics {
  - Success rate: 98%+
  - Fraud rate: < 2%
  - P99 latency: < 500ms
}
    ↓
Andon: Alerts {
  - Fraud rate > 2%? ALERT
  - Circuit breaker open? ALERT
  - Latency > 5s? ALERT
}
```

**Key Metrics**:
- Throughput: 1000+ txn/sec
- Success rate: 98%+
- Fraud catch rate: 95%+
- False positive rate: < 2%
- P99 latency: 245ms

### System 2: Deployment Orchestration

**Problem Statement**:
Deploy services safely and efficiently while ensuring health, preventing cascading failures, and enabling fast rollback.

**Requirements**:
- Deploy 20+ services/day
- Detect deployment issues in < 5 minutes
- Auto-rollback on health failure
- Sequence regional deployments (no cascading)
- Gradual rollout (canary → staging → prod)
- Audit trail for compliance

**TAI Implementation**:

```
Signal: DeploymentRequest {
  service: "api-service",
  version: "v2.0.0",
  target_region: "us-west-2",
  rollout_strategy: "canary"
}
    ↓
Policy: DeploymentSafety {
  - Image exists?
  - Cluster has capacity?
  - Rollback plan exists?
  - Health SLOs met?
  - REJECT if any fails
}
    ↓
Heijunka: GradualRollout {
  - Stage 1: Canary (5% traffic, 5 min)
  - Stage 2: Staging (25% traffic, 10 min)
  - Stage 3: Prod (100% traffic)
  - Early failure detection
}
    ↓
Action: DeployToInfrastructure {
  - Apply Terraform config
  - Deploy to Kubernetes
  - Update DNS/load balancer
}
    ↓
Jidoka: AutomaticRollback {
  - Monitor health metrics
  - Error rate > 5%? ROLLBACK
  - P99 latency > SLO? ROLLBACK
  - Fail fast, minimize blast radius
}
    ↓
Kanban: SequentialQueue {
  - Deploy US-WEST first
  - Wait for success
  - Then deploy US-EAST
  - Prevent cascading regional failures
}
    ↓
Kaizen: Metrics {
  - Deployment success rate: 98%+
  - Rollback frequency: < 2%
  - Average deployment time: 45s
}
    ↓
Andon: Alerts {
  - Deployment failed? CRITICAL ALERT
  - Rollback triggered? CRITICAL ALERT
  - Health marginal? HIGH ALERT
}
```

**Key Metrics**:
- Deployment frequency: 20+ deployments/day
- Deployment success rate: 98%+
- Rollback frequency: < 2%
- MTTR (rollback): < 2 minutes
- Deployment time: 40-50 seconds

---

## Implementation Patterns

### Pattern 1: Policy as Code

```rust
pub trait Policy {
    async fn evaluate(&self, signal: &Signal) -> Result<Decision>;
}

pub struct FraudPolicy {
    blacklist: Arc<RwLock<Vec<String>>>,
    velocity_tracker: Arc<VelocityTracker>,
}

impl Policy for FraudPolicy {
    async fn evaluate(&self, signal: &PaymentSignal) -> Result<Decision> {
        let mut violations = vec![];

        // Rule 1: Blacklist
        if self.blacklist.read().contains(&signal.customer_id) {
            violations.push("Customer blacklisted");
        }

        // Rule 2: Velocity
        if !self.velocity_tracker.check_velocity(&signal.customer_id, 5) {
            violations.push("Velocity limit exceeded");
        }

        // Rule 3: Amount
        if signal.amount > 5000.0 {
            violations.push("Amount exceeds limit");
        }

        if violations.is_empty() {
            Ok(Decision::Approved)
        } else {
            Ok(Decision::Rejected(violations))
        }
    }
}
```

**Benefits**:
- Policies are testable units
- Easy to add new rules
- Easy to modify thresholds
- Independent of action execution

### Pattern 2: Circuit Breaker with Recovery

```rust
pub struct CircuitBreaker {
    state: RwLock<CircuitState>,
    failure_count: AtomicU64,
    success_count: AtomicU64,
    failure_threshold: u64,
    success_threshold: u64,
    last_failure_time: RwLock<Option<Instant>>,
    recovery_timeout: Duration,
}

impl CircuitBreaker {
    pub fn can_execute(&self) -> Result<()> {
        match *self.state.read() {
            CircuitState::Closed => Ok(()),
            CircuitState::Open => {
                let elapsed = self.last_failure_time
                    .read()
                    .map(|t| t.elapsed())
                    .unwrap_or(Duration::ZERO);

                if elapsed > self.recovery_timeout {
                    // Transition to HalfOpen
                    *self.state.write() = CircuitState::HalfOpen;
                    self.success_count.store(0, Ordering::Relaxed);
                    Ok(())
                } else {
                    Err(anyhow!("Circuit open, retry in {:?}",
                        self.recovery_timeout - elapsed))
                }
            }
            CircuitState::HalfOpen => Ok(()), // Allow probe
        }
    }

    pub fn record_success(&self) {
        let state = *self.state.read();

        match state {
            CircuitState::HalfOpen => {
                let new_count = self.success_count.fetch_add(1, Ordering::Relaxed) + 1;

                if new_count >= self.success_threshold {
                    // Close circuit after successful recovery
                    *self.state.write() = CircuitState::Closed;
                    self.failure_count.store(0, Ordering::Relaxed);
                    self.success_count.store(0, Ordering::Relaxed);
                }
            }
            CircuitState::Closed => {
                // Reset failure count on success
                self.failure_count.store(0, Ordering::Relaxed);
            }
            CircuitState::Open => {
                // Ignore successes while open
            }
        }
    }

    pub fn record_failure(&self) {
        let new_count = self.failure_count.fetch_add(1, Ordering::Relaxed) + 1;
        *self.last_failure_time.write() = Some(Instant::now());

        if new_count >= self.failure_threshold {
            *self.state.write() = CircuitState::Open;
            andon.alert_circuit_breaker_open();
        }
    }
}
```

**States Explained**:
- **Closed**: Normal operation, requests proceed
- **Open**: Service unavailable, requests fail immediately
- **Half-Open**: Attempting recovery, single probe allowed

**Benefits**:
- Prevents cascading failures
- Automatic recovery
- Fail-fast behavior
- Clear failure visibility

### Pattern 3: Metrics Collection

```rust
pub struct Metrics {
    total_requests: Counter,
    successful_requests: Counter,
    failed_requests: Counter,
    latency_histogram: Histogram,
    custom_gauges: HashMap<String, Gauge>,
}

impl Metrics {
    pub fn record_request(
        &self,
        success: bool,
        duration: Duration,
        tags: &HashMap<String, String>
    ) {
        self.total_requests.inc();

        if success {
            self.successful_requests.inc();
        } else {
            self.failed_requests.inc();
        }

        self.latency_histogram
            .with_label_values(&[&tags.get("endpoint").unwrap_or(&"unknown".to_string())])
            .observe(duration.as_millis() as f64);
    }

    pub fn get_success_rate(&self) -> f64 {
        let total = self.total_requests.get_sample_count();
        let successful = self.successful_requests.get_sample_count();

        if total == 0.0 {
            0.0
        } else {
            (successful / total) * 100.0
        }
    }

    pub fn set_custom_metric(&self, name: &str, value: f64) {
        self.custom_gauges
            .entry(name.to_string())
            .or_insert_with(|| Gauge::new(name, "Custom metric").unwrap())
            .set(value);
    }
}
```

**Metrics to Track**:

| Metric | Purpose | Alert Threshold |
|--------|---------|-----------------|
| Request count | Throughput | (informational) |
| Success rate | Reliability | < 95% |
| Error rate | Failure detection | > 5% |
| P99 latency | Performance | > 1000ms |
| Fraud rate | Security | > 2% |
| Circuit breaker trips | Cascading failure detection | > 0 |

### Pattern 4: Observability with Tracing

```rust
pub async fn process_signal(
    signal: Signal,
    tracer: &Tracer,
) -> Result<Response> {
    let span = tracer.start("process_signal");

    // Phase 1: Policy
    {
        let _policy_span = tracer.start_child(&span, "evaluate_policy");
        let policy_result = policy.evaluate(&signal).await?;
    }

    // Phase 2: Action
    {
        let _action_span = tracer.start_child(&span, "execute_action");
        let action_result = action.execute(&signal).await?;
    }

    // Phase 3: Async
    {
        let _async_span = tracer.start_child(&span, "queue_async_work");
        queue.enqueue(action_result.clone()).await?;
    }

    // Phase 4: Response
    {
        let _response_span = tracer.start_child(&span, "generate_response");
        let response = generate_response(action_result)?;
    }

    Ok(response)
}
```

**Trace Structure**:
```
trace_id: abc123
├─ process_signal (root span)
│  ├─ evaluate_policy (child span)
│  │  ├─ blacklist_check
│  │  ├─ velocity_check
│  │  └─ amount_check
│  ├─ execute_action (child span)
│  │  ├─ circuit_breaker_check
│  │  └─ gateway_call
│  ├─ queue_async_work (child span)
│  │  └─ enqueue_to_nats
│  └─ generate_response (child span)
│     └─ response_serialization
```

**Benefits**:
- Complete request path visibility
- Performance bottleneck identification
- Error context and causality
- Distributed tracing across services

---

## Best Practices

### 1. Policy Design

**✓ DO**:
- Keep policies deterministic (same input → same output)
- Make policies stateless (no side effects)
- Use explicit thresholds (not magic numbers)
- Document policy rules
- Version policies separately from code

**✗ DON'T**:
- Embed policy in action code
- Use non-deterministic logic (random, timestamps)
- Hide policy in configuration files
- Make policies dependent on external state

**Example**:
```rust
// ✓ GOOD: Clear, testable policy
pub struct PaymentPolicy {
    max_daily_amount: f64,
    max_fraud_score: f64,
    blacklisted_customers: Vec<String>,
}

impl PaymentPolicy {
    pub fn evaluate(&self, payment: &Payment) -> Result<()> {
        if self.blacklisted_customers.contains(&payment.customer_id) {
            return Err(PolicyViolation::Blacklisted);
        }

        if payment.amount > self.max_daily_amount {
            return Err(PolicyViolation::AmountExceeded);
        }

        Ok(())
    }
}

// ✗ BAD: Policy hidden in action
pub async fn process_payment(payment: &Payment) -> Result<()> {
    // Policy checks buried here
    if rand::random::<bool>() { // Non-deterministic!
        return Err("Random rejection");
    }

    // ...rest of logic...
}
```

### 2. Circuit Breaker Configuration

**✓ DO**:
- Set failure threshold based on SLA (not too high, not too low)
- Use exponential backoff for recovery
- Monitor circuit breaker state
- Test circuit breaker behavior

**✗ DON'T**:
- Disable circuit breaker (defeats purpose)
- Set threshold too high (defeats fail-fast)
- Ignore circuit breaker state
- Use fixed backoff (wastes retry attempts)

**Example**:
```rust
// ✓ GOOD: Reasonable circuit breaker
let circuit_breaker = CircuitBreaker {
    failure_threshold: 5,      // 5 failures open circuit
    recovery_timeout: 30,      // Try recovery after 30s
    success_threshold: 3,      // 3 successes close circuit
};

// ✗ BAD: Ineffective circuit breaker
let circuit_breaker = CircuitBreaker {
    failure_threshold: 1000,   // Too high - doesn't catch failures
    recovery_timeout: 1,       // Too short - thrashes state
    success_threshold: 1,      // Too low - closes too easily
};
```

### 3. Metrics Design

**✓ DO**:
- Track business metrics (success rate, fraud rate)
- Track technical metrics (latency, error rate)
- Set SLO thresholds
- Alert on violations
- Review metrics regularly

**✗ DON'T**:
- Track too many metrics (causes fatigue)
- Use meaningless metrics
- Set unrealistic SLO thresholds
- Never review or act on metrics

**Example**:
```rust
// ✓ GOOD: Relevant metrics
pub struct Metrics {
    // Business metrics
    payment_success_rate: f64,      // Should be 98%+
    fraud_detection_rate: f64,      // Should be > 90%

    // Technical metrics
    p99_latency_ms: u64,            // Should be < 500ms
    error_rate_percent: f64,        // Should be < 5%

    // Operational metrics
    circuit_breaker_trips: u64,     // Should be 0 normally
}

// ✗ BAD: Meaningless metrics
pub struct Metrics {
    random_number: u64,             // Useless
    function_call_count: u64,       // Too granular
    memory_bytes: u64,              // Not actionable
}
```

### 4. Testing Strategy

**✓ DO**:
- Test happy path
- Test policy violations
- Test circuit breaker behavior
- Test recovery scenarios
- Test chaos scenarios

**✗ DON'T**:
- Only test happy path
- Skip error cases
- Mock everything
- Don't test async behavior

**Example**:
```rust
#[tokio::test]
async fn test_payment_fraud_detection() {
    // Arrange
    let policy = PaymentPolicy::default();
    let payment = Payment {
        amount: 10000.0,
        customer_id: "blacklist".to_string(),
        ..Default::default()
    };

    // Act
    let result = policy.evaluate(&payment);

    // Assert
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err(),
        PolicyViolation::Blacklisted
    );
}

#[tokio::test]
async fn test_circuit_breaker_opens_on_failures() {
    // Arrange
    let cb = CircuitBreaker::new(3, 30);

    // Act: Record 3 failures
    for _ in 0..3 {
        cb.record_failure();
    }

    // Assert: Circuit should be open
    assert_eq!(cb.get_state(), CircuitState::Open);
    assert!(cb.can_execute().is_err());
}
```

### 5. Production Deployment

**✓ DO**:
- Use health checks
- Enable circuit breakers
- Configure SLO alerts
- Set up on-call escalation
- Monitor system health continuously

**✗ DON'T**:
- Deploy without testing
- Disable safety mechanisms
- Ignore alerts
- Deploy during high-traffic periods
- Change multiple things at once

**Deployment Checklist**:
- [ ] All tests pass
- [ ] Code review complete
- [ ] Metrics baselines established
- [ ] Alerts configured
- [ ] Runbooks updated
- [ ] Team notified
- [ ] Gradual rollout (canary)
- [ ] Health checks passing
- [ ] Monitoring dashboards online

---

## Troubleshooting

### Problem 1: Circuit Breaker Stuck Open

**Symptoms**:
- All requests failing with "Circuit open"
- Stays open indefinitely
- Recovery never attempted

**Root Causes**:
- Recovery timeout too long
- Service actually down (not temporary)
- Continuous failures (never recovers)

**Solutions**:
```rust
// Check circuit breaker state
let state = circuit_breaker.get_state();
if state == CircuitState::Open {
    // Wait for recovery timeout
    sleep(Duration::from_secs(recovery_timeout)).await;

    // Or force manual recovery
    circuit_breaker.force_half_open();
}

// Verify downstream service
let is_healthy = check_downstream_service().await;
if !is_healthy {
    // Fix downstream service first
    fix_downstream_service().await;
}
```

### Problem 2: High Fraud False Positives

**Symptoms**:
- Legitimate payments rejected (fraud_score too high)
- Customer complaints increase
- Revenue impact

**Root Causes**:
- Fraud thresholds too aggressive
- Policy rules too strict
- Velocity limits too low

**Solutions**:
```rust
// Review fraud metrics
let metrics = get_fraud_metrics();
if metrics.false_positive_rate > 5.0 {
    // Adjust policy thresholds
    policy.max_fraud_score = 0.8;  // Increase from 0.5
    policy.velocity_limit = 10;    // Increase from 5
}

// Implement whitelist for trusted customers
policy.trusted_customers.push("cust_123");
```

### Problem 3: Slow Deployments

**Symptoms**:
- Deployments taking > 60 seconds
- Frequent timeouts
- SLO violations

**Root Causes**:
- Canary stage too long
- Staging checks too thorough
- Infrastructure slow

**Solutions**:
```rust
// Adjust rollout stages
let stages = RolloutConfig {
    canary_duration: Duration::from_secs(3),    // Reduce from 5
    staging_duration: Duration::from_secs(5),   // Reduce from 10
    health_check_interval: Duration::from_millis(500), // Speed up
};

// Or parallelize where possible
canary_and_staging_parallel().await?;
prod_sequential().await?;
```

### Problem 4: Metrics Dashboard Empty

**Symptoms**:
- Prometheus has no data
- Grafana dashboards blank
- No historical metrics

**Root Causes**:
- Metrics not being recorded
- Prometheus scrape failure
- Exporters not running

**Solutions**:
```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# Check metrics endpoint
curl http://localhost:8080/metrics

# Verify service exports metrics
metrics.record_request(success, duration);
```

---

## Advanced Topics

### Advanced Topic 1: Distributed Tracing at Scale

**Challenge**: Tracing millions of requests across services

**Solution**: Distributed tracing with sampling

```rust
pub struct DistributedTracing {
    sampler: Sampler,
    exporter: JaegerExporter,
}

pub enum Sampler {
    Always,                          // 100% sampling
    Never,                           // 0% sampling
    Probabilistic(f64),              // e.g., 10% sampling
    RateLimited(u32),                // e.g., 100 traces/sec
    AdaptiveSampler(AdaptiveConfig), // Dynamic sampling
}

// Only sample high-value requests
match &self.sampler {
    Sampler::Probabilistic(rate) => {
        if rand::random::<f64>() < *rate {
            start_span(&span);
        }
    }
    Sampler::RateLimited(rate) => {
        if token_bucket.take_token() {
            start_span(&span);
        }
    }
    // ...
}
```

### Advanced Topic 2: Multi-Region Circuit Breakers

**Challenge**: Circuit breaker behavior in distributed system

**Solution**: Independent circuit breakers per region

```rust
pub struct MultiRegionCircuitBreaker {
    per_region_breakers: HashMap<String, CircuitBreaker>,
    global_breaker: CircuitBreaker,
}

impl MultiRegionCircuitBreaker {
    pub async fn execute(&self, region: &str) -> Result<()> {
        // Check both regional and global circuit breakers
        self.per_region_breakers[region].can_execute()?;
        self.global_breaker.can_execute()?;

        Ok(())
    }
}
```

### Advanced Topic 3: Policy Composition

**Challenge**: Policies get complex with many rules

**Solution**: Composable policy objects

```rust
pub trait Policy {
    async fn evaluate(&self, signal: &Signal) -> Result<Decision>;
}

pub struct CompositePolicy {
    policies: Vec<Box<dyn Policy>>,
}

impl Policy for CompositePolicy {
    async fn evaluate(&self, signal: &Signal) -> Result<Decision> {
        let mut violations = vec![];

        for policy in &self.policies {
            match policy.evaluate(signal).await {
                Ok(Decision::Approved) => {},
                Ok(Decision::Rejected(reasons)) => {
                    violations.extend(reasons);
                }
                Err(e) => return Err(e),
            }
        }

        if violations.is_empty() {
            Ok(Decision::Approved)
        } else {
            Ok(Decision::Rejected(violations))
        }
    }
}

// Usage
let policy = CompositePolicy {
    policies: vec![
        Box::new(BlacklistPolicy::new()),
        Box::new(VelocityPolicy::new()),
        Box::new(AmountPolicy::new()),
        Box::new(GeolocationPolicy::new()),
    ],
};
```

### Advanced Topic 4: Progressive Rollout with Percentage-Based Traffic

**Challenge**: Deploy to percentage of users (10%, 50%, 100%)

**Solution**: Traffic-based deployment stages

```rust
pub struct ProgressiveRollout {
    current_percentage: AtomicU32,
    stages: Vec<RolloutStage>,
}

pub struct RolloutStage {
    target_percentage: u32,
    duration: Duration,
    success_criteria: SuccessCriteria,
}

impl ProgressiveRollout {
    pub async fn execute(&self) -> Result<()> {
        for stage in &self.stages {
            // Route stage.target_percentage of traffic to new version
            set_traffic_weight(stage.target_percentage).await?;

            // Monitor success criteria
            sleep(stage.duration).await;

            // Check if stage meets criteria
            let metrics = get_metrics().await;
            if !stage.success_criteria.met(&metrics) {
                // Rollback
                set_traffic_weight(0).await?;
                return Err(anyhow!("Stage failed success criteria"));
            }
        }

        Ok(())
    }
}
```

---

## Conclusion

The TAI Pattern combined with TPS principles provides a robust, observable, and resilient architecture for processing signals through business policies into actions.

**Key Takeaways**:
1. **Make problems visible** (Andon)
2. **Fail fast, recover quickly** (Jidoka)
3. **Prevent overload** (Kanban, Heijunka)
4. **Improve continuously** (Kaizen)
5. **Trace everything** (Observability)

**Implementation Path**:
1. Start with simple signal → policy → action
2. Add circuit breaker for resilience
3. Add metrics for visibility
4. Add alerts for proactive response
5. Add tracing for debugging

**Validation**:
- Test happy path
- Test failure scenarios
- Test chaos scenarios
- Monitor metrics continuously
- Act on alerts immediately

The reference implementations demonstrate these principles in production-grade code suitable as a template for your own systems.

---

## Additional Resources

- **Toyota Production System**: The Toyota Way by Jeffrey Liker
- **Circuit Breaker Pattern**: Release It! by Michael T. Nygard
- **Canary Deployments**: Google Cloud documentation
- **Distributed Tracing**: OpenTelemetry documentation
- **Metrics and Alerting**: Prometheus documentation
- **Chaos Engineering**: Principles of Chaos Engineering

