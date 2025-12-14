<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Swarm Intelligence Integration Architecture](#swarm-intelligence-integration-architecture)
  - [Executive Summary](#executive-summary)
  - [1. System Architecture Overview](#1-system-architecture-overview)
    - [1.1 High-Level Architecture](#11-high-level-architecture)
    - [1.2 Component Overview](#12-component-overview)
  - [2. Core Integration Patterns](#2-core-integration-patterns)
    - [2.1 Minimal Invasiveness Strategy](#21-minimal-invasiveness-strategy)
    - [2.2 API Design](#22-api-design)
  - [3. Consensus Mechanisms](#3-consensus-mechanisms)
    - [3.1 Majority Voting Architecture](#31-majority-voting-architecture)
    - [3.2 Consensus Algorithm](#32-consensus-algorithm)
  - [4. Worker Coordination Patterns](#4-worker-coordination-patterns)
    - [4.1 Agent Roles and Specialization](#41-agent-roles-and-specialization)
    - [4.2 Task Distribution Strategy](#42-task-distribution-strategy)
  - [5. Memory Sharing and Synchronization](#5-memory-sharing-and-synchronization)
    - [5.1 Lock-Free Memory Architecture](#51-lock-free-memory-architecture)
    - [5.2 Memory Synchronization Protocol](#52-memory-synchronization-protocol)
  - [6. Failure Handling and Self-Healing](#6-failure-handling-and-self-healing)
    - [6.1 Failure Modes and Recovery](#61-failure-modes-and-recovery)
    - [6.2 Self-Healing Architecture](#62-self-healing-architecture)
    - [6.3 Retry Logic](#63-retry-logic)
  - [7. Integration Points with Existing Code](#7-integration-points-with-existing-code)
    - [7.1 HiveQueen Integration](#71-hivequeen-integration)
    - [7.2 Ultrathink Swarm Integration](#72-ultrathink-swarm-integration)
    - [7.3 CLI Integration](#73-cli-integration)
  - [8. Performance Considerations](#8-performance-considerations)
    - [8.1 Expected Performance Characteristics](#81-expected-performance-characteristics)
    - [8.2 Scalability Analysis](#82-scalability-analysis)
    - [8.3 Optimization Strategies](#83-optimization-strategies)
  - [9. Implementation Roadmap](#9-implementation-roadmap)
    - [Phase 1: Foundation (Week 1-2)](#phase-1-foundation-week-1-2)
    - [Phase 2: Memory & Coordination (Week 3-4)](#phase-2-memory--coordination-week-3-4)
    - [Phase 3: Integration (Week 5-6)](#phase-3-integration-week-5-6)
    - [Phase 4: Production Hardening (Week 7-8)](#phase-4-production-hardening-week-7-8)
  - [10. Testing Strategy](#10-testing-strategy)
    - [10.1 Unit Tests](#101-unit-tests)
    - [10.2 Integration Tests](#102-integration-tests)
    - [10.3 Load Tests](#103-load-tests)
    - [10.4 Chaos Engineering](#104-chaos-engineering)
  - [11. Architecture Decision Records](#11-architecture-decision-records)
    - [ADR-001: Why Majority Voting?](#adr-001-why-majority-voting)
    - [ADR-002: Why Lock-Free Snapshots?](#adr-002-why-lock-free-snapshots)
    - [ADR-003: Why Unified Coordinator?](#adr-003-why-unified-coordinator)
  - [12. Security Considerations](#12-security-considerations)
    - [12.1 Threat Model](#121-threat-model)
    - [12.2 Mitigations](#122-mitigations)
  - [13. Future Enhancements (Post-V1)](#13-future-enhancements-post-v1)
    - [13.1 Advanced Consensus Algorithms](#131-advanced-consensus-algorithms)
    - [13.2 Learning and Adaptation](#132-learning-and-adaptation)
    - [13.3 Cross-Instance Coordination](#133-cross-instance-coordination)
  - [Appendix A: Glossary](#appendix-a-glossary)
  - [Appendix B: Reference Architecture Patterns](#appendix-b-reference-architecture-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Swarm Intelligence Integration Architecture

**Version:** 1.0
**Status:** Design Phase
**Author:** System Architect (Hive Mind Swarm)
**Date:** 2025-11-18

## Executive Summary

This document defines the architecture for integrating advanced swarm intelligence capabilities into the ggen CLI. The design follows the 80/20 principle, focusing on minimal invasiveness while delivering maximum distributed coordination value. The architecture blends collective intelligence, consensus mechanisms, and lock-free coordination patterns to enable ggen to leverage multiple AI agents for complex ontology management tasks.

---

## 1. System Architecture Overview

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         ggen CLI                                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │ User Commands│  │ Configuration│  │ Existing Subsystems   │  │
│  └──────┬───────┘  └──────┬───────┘  └──────────┬───────────┘  │
│         │                 │                       │              │
│         └─────────────────┴───────────────────────┘              │
│                           │                                      │
│                           ▼                                      │
│         ┌─────────────────────────────────────┐                 │
│         │   Swarm Coordinator (Integration)   │ ◄───NEW LAYER   │
│         └─────────────────┬───────────────────┘                 │
│                           │                                      │
│         ┌─────────────────┴───────────────────┐                 │
│         │                                     │                 │
│         ▼                                     ▼                 │
│  ┌──────────────┐                    ┌──────────────┐          │
│  │ Hive Queen   │                    │ Ultrathink   │          │
│  │ Coordinator  │                    │ Swarm        │          │
│  │ (Existing)   │                    │ (Existing)   │          │
│  └──────┬───────┘                    └──────┬───────┘          │
│         │                                    │                  │
└─────────┼────────────────────────────────────┼──────────────────┘
          │                                    │
          ▼                                    ▼
┌─────────────────┐                  ┌─────────────────┐
│ Agent Pool      │                  │ Task Queue      │
│ (Workers)       │◄────────────────►│ (Priority)      │
└─────────────────┘                  └─────────────────┘
          │                                    │
          └──────────────┬─────────────────────┘
                         │
                         ▼
          ┌──────────────────────────────┐
          │  Consensus & Memory Layer     │
          │  - Majority Voting            │
          │  - Shared State (Lock-Free)   │
          │  - Conflict Resolution        │
          └──────────────────────────────┘
```

### 1.2 Component Overview

| Component | Purpose | Integration Point |
|-----------|---------|-------------------|
| **Swarm Coordinator** | Central orchestration layer | New unified API layer |
| **Hive Queen** | Ontology-specific coordination | Existing module (enhanced) |
| **Ultrathink Swarm** | Generic multi-agent tasks | Existing module (enhanced) |
| **Agent Pool** | Worker agent management | New worker registry |
| **Consensus Engine** | Majority voting & agreement | New consensus layer |
| **Memory Layer** | Shared state (lock-free) | Enhanced from existing |
| **Task Scheduler** | Priority-based dispatch | New scheduler (from swarm_coordination) |

---

## 2. Core Integration Patterns

### 2.1 Minimal Invasiveness Strategy

**Design Principle:** Augment, don't replace existing code.

```
EXISTING CODE:
  ├─ ggen-core/
  │  └─ config/
  │     └─ hive_coordinator.rs ──► Keep as-is
  │
  ├─ ggen-ai/
  │  └─ swarm.rs ──────────────► Keep as-is
  │
  └─ ggen-domain/
     └─ swarm_coordination.rs ─► Keep as-is

NEW INTEGRATION LAYER:
  ├─ ggen-core/
  │  └─ swarm/
  │     ├─ coordinator.rs ──────► New: Unified coordinator
  │     ├─ consensus.rs ─────────► New: Voting & agreement
  │     └─ worker_pool.rs ───────► New: Worker management
  │
  └─ ggen-cli/
     └─ swarm_commands.rs ──────► New: CLI integration
```

**Integration Points (Critical 20%):**
1. ✅ **Swarm initialization** - Single entry point for swarm creation
2. ✅ **Task submission** - Unified API for distributing work
3. ✅ **Consensus retrieval** - Majority voting results
4. ✅ **Status monitoring** - Real-time swarm health
5. ✅ **Failure recovery** - Self-healing and retry logic

### 2.2 API Design

```rust
// NEW: Unified Swarm Coordinator API
pub struct SwarmCoordinator {
    hive_queen: Option<HiveQueen>,
    ultrathink: SwarmCoordinator,
    worker_pool: WorkerPool,
    consensus_engine: ConsensusEngine,
    memory: Arc<RwLock<SwarmMemory>>,
}

impl SwarmCoordinator {
    /// Initialize swarm with configuration
    pub async fn new(config: SwarmConfig) -> Result<Self>;

    /// Submit task for distributed processing
    pub async fn submit_task(&self, task: SwarmTask) -> Result<TaskHandle>;

    /// Wait for consensus on task result
    pub async fn wait_consensus(&self, handle: TaskHandle) -> Result<ConsensusResult>;

    /// Get swarm health and metrics
    pub fn get_status(&self) -> SwarmStatus;

    /// Graceful shutdown
    pub async fn shutdown(&self) -> Result<()>;
}
```

---

## 3. Consensus Mechanisms

### 3.1 Majority Voting Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Consensus Engine                          │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Task: "Resolve version conflict: schema-org"        │  │
│  └──────────────────┬───────────────────────────────────┘  │
│                     │                                       │
│                     ▼                                       │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Agent Assignment (N=5 for critical tasks)           │  │
│  │  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐             │  │
│  │  │ A1  │ │ A2  │ │ A3  │ │ A4  │ │ A5  │             │  │
│  │  └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘             │  │
│  └─────┼───────┼───────┼───────┼───────┼────────────────┘  │
│        │       │       │       │       │                   │
│        ▼       ▼       ▼       ▼       ▼                   │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Independent Execution (Parallel)                    │  │
│  │  A1: "Use 3.13.0" (conf: 0.95)                       │  │
│  │  A2: "Use 3.13.0" (conf: 0.92)                       │  │
│  │  A3: "Use 3.14.0" (conf: 0.78)                       │  │
│  │  A4: "Use 3.13.0" (conf: 0.89)                       │  │
│  │  A5: "Use 3.13.0" (conf: 0.91)                       │  │
│  └──────────────────┬───────────────────────────────────┘  │
│                     │                                       │
│                     ▼                                       │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Voting & Consensus (Majority: 3/5)                  │  │
│  │  Result: "Use 3.13.0"                                │  │
│  │  Confidence: 0.9175 (avg of 4 agreeing agents)       │  │
│  │  Agreement: 80% (4/5 agents)                         │  │
│  └──────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

### 3.2 Consensus Algorithm

```rust
pub struct ConsensusEngine {
    config: ConsensusConfig,
}

pub struct ConsensusConfig {
    /// Minimum agents for consensus
    pub min_agents: usize,
    /// Agreement threshold (0.0-1.0)
    pub agreement_threshold: f64,
    /// Timeout for consensus
    pub timeout_secs: u64,
}

impl ConsensusEngine {
    /// Reach consensus on task results
    pub async fn reach_consensus(
        &self,
        results: Vec<TaskResult>,
    ) -> Result<ConsensusResult> {
        // 1. Group results by output similarity
        let groups = self.group_similar_results(&results)?;

        // 2. Find majority group (threshold: 50%+)
        let majority = self.find_majority(&groups)?;

        // 3. Calculate confidence (average of agreeing agents)
        let confidence = self.calculate_confidence(&majority);

        // 4. Check agreement threshold
        if majority.agreement < self.config.agreement_threshold {
            return Err(Error::ConsensusNotReached {
                agreement: majority.agreement,
                threshold: self.config.agreement_threshold,
            });
        }

        Ok(ConsensusResult {
            output: majority.output,
            confidence,
            agreement: majority.agreement,
            participating_agents: results.len(),
            agreeing_agents: majority.count,
        })
    }
}
```

---

## 4. Worker Coordination Patterns

### 4.1 Agent Roles and Specialization

```
┌────────────────────────────────────────────────────────────────┐
│                       Agent Pool                                │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Specialized Worker Types                               │  │
│  │                                                          │  │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │  │
│  │  │ Analyzer     │  │ Resolver     │  │ Validator    │  │  │
│  │  │ - Version    │  │ - Conflicts  │  │ - Schema     │  │  │
│  │  │   analysis   │  │ - Deps       │  │ - Types      │  │  │
│  │  │ - Patterns   │  │ - Namespace  │  │ - Ontology   │  │  │
│  │  └──────────────┘  └──────────────┘  └──────────────┘  │  │
│  │                                                          │  │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │  │
│  │  │ Optimizer    │  │ Monitor      │  │ Coordinator  │  │  │
│  │  │ - Perf       │  │ - Health     │  │ - Workflow   │  │  │
│  │  │ - Memory     │  │ - Metrics    │  │ - Dispatch   │  │  │
│  │  │ - Cache      │  │ - Alerts     │  │ - Recovery   │  │  │
│  │  └──────────────┘  └──────────────┘  └──────────────┘  │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Dynamic Worker Scaling                                  │  │
│  │  • Base pool: 3-5 workers (always running)              │  │
│  │  • Scale up: +2 workers per 10 pending tasks            │  │
│  │  • Scale down: -1 worker per 5 idle minutes             │  │
│  │  • Max pool: 20 workers (configurable)                  │  │
│  └─────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────┘
```

### 4.2 Task Distribution Strategy

```rust
pub struct WorkerPool {
    workers: Arc<RwLock<HashMap<AgentId, Worker>>>,
    scheduler: PriorityScheduler,
}

impl WorkerPool {
    /// Assign task to optimal worker
    pub async fn assign_task(&self, task: SwarmTask) -> Result<AgentId> {
        let workers = self.workers.read().await;

        // 1. Filter capable workers
        let capable = workers.iter()
            .filter(|(_, w)| w.can_handle(&task.task_type))
            .collect::<Vec<_>>();

        // 2. Score workers by:
        //    - Current load (lower = better)
        //    - Historical success rate (higher = better)
        //    - Expertise match (higher = better)
        let scored = capable.iter()
            .map(|(id, w)| {
                let score = self.calculate_worker_score(w, &task);
                (id.clone(), score)
            })
            .collect::<Vec<_>>();

        // 3. Select best worker
        scored.into_iter()
            .max_by(|(_, s1), (_, s2)| s1.partial_cmp(s2).unwrap())
            .map(|(id, _)| id)
            .ok_or_else(|| Error::NoCapableWorker)
    }

    fn calculate_worker_score(&self, worker: &Worker, task: &SwarmTask) -> f64 {
        let load_factor = 1.0 - (worker.current_load() as f64 / 10.0);
        let success_rate = worker.metrics.success_rate;
        let expertise_match = worker.expertise_score(&task.task_type);

        // Weighted average
        (load_factor * 0.3) + (success_rate * 0.4) + (expertise_match * 0.3)
    }
}
```

---

## 5. Memory Sharing and Synchronization

### 5.1 Lock-Free Memory Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                    Shared Memory Layer                          │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Snapshot-Based Lock-Free Reads                          │  │
│  │                                                           │  │
│  │   Current Snapshot: V42                                  │  │
│  │   ┌────────────────────────────────────────────┐         │  │
│  │   │ ontology_cache: {...}                     │         │  │
│  │   │ version_matrix: {...}                     │         │  │
│  │   │ conflict_history: {...}                   │         │  │
│  │   └────────────────────────────────────────────┘         │  │
│  │                                                           │  │
│  │   Staging Snapshot: V43 (being built)                    │  │
│  │   ┌────────────────────────────────────────────┐         │  │
│  │   │ ontology_cache: {...updated...}           │         │  │
│  │   │ version_matrix: {...updated...}           │         │  │
│  │   │ conflict_history: {...updated...}         │         │  │
│  │   └────────────────────────────────────────────┘         │  │
│  │                                                           │  │
│  │  Writers:                    Readers:                    │  │
│  │  • Build staging snapshot    • Always read current V42   │  │
│  │  • Atomic swap when ready    • Never block               │  │
│  │  • Old snapshot GC'd         • Consistent view           │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Memory Partitions (Namespaced)                          │  │
│  │  • hive/state/* - HiveQueen state                        │  │
│  │  • swarm/tasks/* - Task queue and results                │  │
│  │  • consensus/votes/* - Voting records                    │  │
│  │  • workers/metrics/* - Performance metrics               │  │
│  └──────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────┘
```

### 5.2 Memory Synchronization Protocol

```rust
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct SwarmMemory {
    /// Lock-free snapshot cell
    snapshot: SnapshotCell,
    /// Write buffer (only one writer at a time)
    write_buffer: Arc<RwLock<HashMap<String, serde_json::Value>>>,
}

impl SwarmMemory {
    /// Read from current snapshot (never blocks other readers)
    pub fn read(&self, key: &str) -> Option<serde_json::Value> {
        let snapshot = self.snapshot.current();
        // Read from snapshot storage (implementation detail)
        self.read_from_snapshot(&snapshot, key)
    }

    /// Write to staging snapshot
    pub async fn write(&self, key: &str, value: serde_json::Value) -> Result<()> {
        let mut buffer = self.write_buffer.write().await;
        buffer.insert(key.to_string(), value);
        Ok(())
    }

    /// Commit staging snapshot to active
    pub async fn commit_snapshot(&self) -> Result<()> {
        let buffer = self.write_buffer.read().await;

        // 1. Build new snapshot from current + buffer
        let new_snapshot_id = format!("snap-{}", uuid::Uuid::new_v4());
        self.build_snapshot(&new_snapshot_id, &buffer).await?;

        // 2. Stage it
        self.snapshot.stage_next(&new_snapshot_id);

        // 3. Atomic swap
        self.snapshot.commit_staged()?;

        // 4. Clear write buffer
        drop(buffer);
        self.write_buffer.write().await.clear();

        Ok(())
    }
}
```

---

## 6. Failure Handling and Self-Healing

### 6.1 Failure Modes and Recovery

| Failure Mode | Detection | Recovery Strategy | Max Recovery Time |
|--------------|-----------|-------------------|-------------------|
| **Worker Crash** | Heartbeat timeout (30s) | Reassign task to new worker | < 1 min |
| **Consensus Timeout** | Task deadline exceeded | Spawn additional workers, retry | < 2 min |
| **Memory Corruption** | Checksum validation | Rollback to previous snapshot | < 10s |
| **Network Partition** | Agent unreachable | Mark unavailable, use remaining pool | Immediate |
| **Resource Exhaustion** | Memory/CPU threshold | Scale down pool, queue tasks | < 30s |

### 6.2 Self-Healing Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                     Health Monitor                              │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Continuous Health Checks (every 10s)                    │  │
│  │  • Worker heartbeats                                     │  │
│  │  • Memory consistency                                    │  │
│  │  • Task queue depth                                      │  │
│  │  • Consensus success rate                                │  │
│  └────────────────────┬─────────────────────────────────────┘  │
│                       │                                         │
│                       ▼                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Anomaly Detection                                       │  │
│  │  • Worker failure rate > 10% → Scale up pool            │  │
│  │  • Consensus failures > 20% → Adjust thresholds         │  │
│  │  • Queue depth > 100 → Add workers                      │  │
│  │  • Memory usage > 80% → Trigger GC                      │  │
│  └────────────────────┬─────────────────────────────────────┘  │
│                       │                                         │
│                       ▼                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Automated Recovery                                      │  │
│  │  1. Log failure event                                   │  │
│  │  2. Execute recovery action                             │  │
│  │  3. Verify system health                                │  │
│  │  4. Resume normal operation                             │  │
│  └──────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────┘
```

### 6.3 Retry Logic

```rust
pub struct RetryPolicy {
    pub max_attempts: u32,
    pub backoff_strategy: BackoffStrategy,
    pub timeout_per_attempt: Duration,
}

pub enum BackoffStrategy {
    Linear { interval: Duration },
    Exponential { base: Duration, multiplier: f64 },
    Fibonacci { base: Duration },
}

impl SwarmCoordinator {
    /// Execute task with retry logic
    async fn execute_with_retry(
        &self,
        task: SwarmTask,
        policy: RetryPolicy,
    ) -> Result<ConsensusResult> {
        let mut attempts = 0;
        let mut last_error = None;

        while attempts < policy.max_attempts {
            attempts += 1;

            match self.execute_task_internal(&task).await {
                Ok(result) => return Ok(result),
                Err(e) => {
                    log::warn!("Task attempt {}/{} failed: {}",
                        attempts, policy.max_attempts, e);
                    last_error = Some(e);

                    // Calculate backoff delay
                    let delay = policy.backoff_strategy.calculate_delay(attempts);
                    tokio::time::sleep(delay).await;
                }
            }
        }

        Err(last_error.unwrap_or_else(||
            Error::MaxRetriesExceeded(policy.max_attempts)))
    }
}
```

---

## 7. Integration Points with Existing Code

### 7.1 HiveQueen Integration

```rust
// EXISTING: crates/ggen-core/src/config/hive_coordinator.rs
impl HiveQueen {
    pub async fn orchestrate(&mut self) -> Result<ResolvedConfiguration> {
        // Existing 5-phase orchestration
    }
}

// NEW: Swarm-enhanced HiveQueen
impl HiveQueen {
    /// Enable swarm mode for distributed orchestration
    pub async fn with_swarm(
        mut self,
        swarm: Arc<SwarmCoordinator>,
    ) -> Result<Self> {
        self.swarm = Some(swarm);
        Ok(self)
    }

    /// Orchestrate with swarm consensus
    pub async fn orchestrate_with_consensus(&mut self) -> Result<ResolvedConfiguration> {
        if let Some(swarm) = &self.swarm {
            // Use swarm for distributed decision-making
            let task = SwarmTask::new(
                "resolve_configuration",
                TaskType::OntologyGeneration,
                serde_json::to_value(&self.config)?
            );

            let handle = swarm.submit_task(task).await?;
            let consensus = swarm.wait_consensus(handle).await?;

            // Convert consensus to ResolvedConfiguration
            serde_json::from_value(consensus.output)
        } else {
            // Fallback to existing single-agent orchestration
            self.orchestrate().await
        }
    }
}
```

### 7.2 Ultrathink Swarm Integration

```rust
// EXISTING: crates/ggen-ai/src/swarm.rs
impl SwarmCoordinator {
    pub fn register_agent(&self, agent: Box<dyn SwarmAgent>) -> Result<()>;
}

// NEW: Bridge to unified SwarmCoordinator
impl SwarmCoordinator {
    /// Convert existing SwarmAgent to unified worker
    pub fn register_as_worker(
        &self,
        agent: Box<dyn SwarmAgent>,
        unified_swarm: Arc<UnifiedSwarmCoordinator>,
    ) -> Result<()> {
        let worker = Worker::from_swarm_agent(agent);
        unified_swarm.worker_pool.add_worker(worker).await?;
        Ok(())
    }
}
```

### 7.3 CLI Integration

```rust
// NEW: crates/ggen-cli/src/swarm_commands.rs

use clap::{Args, Subcommand};

#[derive(Args)]
pub struct SwarmArgs {
    #[command(subcommand)]
    pub command: SwarmCommand,
}

#[derive(Subcommand)]
pub enum SwarmCommand {
    /// Initialize swarm with configuration
    Init {
        /// Number of workers
        #[arg(short, long, default_value = "5")]
        workers: usize,

        /// Consensus threshold (0.0-1.0)
        #[arg(short, long, default_value = "0.67")]
        threshold: f64,
    },

    /// Submit task to swarm
    Submit {
        /// Task description
        description: String,

        /// Task type
        #[arg(short, long)]
        task_type: String,

        /// Wait for consensus
        #[arg(short, long)]
        wait: bool,
    },

    /// Get swarm status
    Status {
        /// Show detailed metrics
        #[arg(short, long)]
        verbose: bool,
    },

    /// Shutdown swarm gracefully
    Shutdown,
}

impl SwarmCommand {
    pub async fn execute(&self, config: &Config) -> Result<()> {
        match self {
            Self::Init { workers, threshold } => {
                let swarm_config = SwarmConfig {
                    max_workers: *workers,
                    consensus_threshold: *threshold,
                    ..Default::default()
                };

                let swarm = SwarmCoordinator::new(swarm_config).await?;
                println!("✅ Swarm initialized with {} workers", workers);
                Ok(())
            }

            Self::Submit { description, task_type, wait } => {
                // Implementation
                Ok(())
            }

            Self::Status { verbose } => {
                // Implementation
                Ok(())
            }

            Self::Shutdown => {
                // Implementation
                Ok(())
            }
        }
    }
}
```

---

## 8. Performance Considerations

### 8.1 Expected Performance Characteristics

| Metric | Target | Reasoning |
|--------|--------|-----------|
| **Task Submission Latency** | < 10ms | Async queue push, no blocking |
| **Consensus Time (3 agents)** | < 2s | Parallel execution + voting |
| **Consensus Time (5 agents)** | < 3s | Parallel execution + voting |
| **Memory Overhead** | < 100MB | Lock-free snapshots, minimal state |
| **Snapshot Commit Time** | < 50ms | Atomic pointer swap |
| **Worker Scaling Time** | < 500ms | Spawn new worker + registration |

### 8.2 Scalability Analysis

```
Workers vs Task Throughput:

Tasks/sec
    │
200 ├─────────────────────────────────────────────── (20 workers)
    │                                        ┌────────
150 ├───────────────────────────────────┌────┘
    │                              ┌────┘
100 ├──────────────────────────┌───┘
    │                     ┌────┘
 50 ├────────────────┌────┘
    │           ┌────┘
  0 └───────────┴──────────────────────────────────────────────
    0    5    10   15   20   25   30   35   40   45   50
                    Number of Workers

Diminishing returns after 20 workers due to:
- Coordination overhead
- Consensus voting latency
- Memory contention
```

### 8.3 Optimization Strategies

1. **Task Batching**: Group similar tasks for bulk processing
2. **Result Caching**: Cache consensus results for identical tasks
3. **Lazy Worker Spawning**: Only create workers when needed
4. **Snapshot Coalescing**: Combine multiple writes before committing
5. **Priority Lanes**: Separate queues for critical vs. background tasks

---

## 9. Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- [ ] Implement `SwarmCoordinator` unified API
- [ ] Implement `ConsensusEngine` with majority voting
- [ ] Implement `WorkerPool` with dynamic scaling
- [ ] Add CLI commands: `swarm init`, `swarm status`

### Phase 2: Memory & Coordination (Week 3-4)
- [ ] Implement lock-free `SwarmMemory` layer
- [ ] Implement `PriorityScheduler` from swarm_coordination
- [ ] Add snapshot-based state management
- [ ] Add CLI commands: `swarm submit`, `swarm results`

### Phase 3: Integration (Week 5-6)
- [ ] Integrate with `HiveQueen` (ontology tasks)
- [ ] Integrate with `Ultrathink` (general AI tasks)
- [ ] Add failure detection and recovery
- [ ] Add comprehensive testing

### Phase 4: Production Hardening (Week 7-8)
- [ ] Performance benchmarking and optimization
- [ ] Self-healing and monitoring
- [ ] Documentation and examples
- [ ] Production deployment guide

---

## 10. Testing Strategy

### 10.1 Unit Tests
- Consensus voting logic
- Worker scoring algorithm
- Memory snapshot consistency
- Priority scheduler correctness

### 10.2 Integration Tests
- HiveQueen + Swarm orchestration
- Ultrathink + Swarm task execution
- CLI commands end-to-end
- Failure recovery scenarios

### 10.3 Load Tests
- 100 concurrent tasks
- 20 workers under load
- Memory stability over 1000 tasks
- Consensus accuracy with varying agent counts

### 10.4 Chaos Engineering
- Random worker crashes
- Network partition simulation
- Memory pressure tests
- Task timeout scenarios

---

## 11. Architecture Decision Records

### ADR-001: Why Majority Voting?
**Decision**: Use simple majority voting (≥50% agreement) for consensus.

**Reasoning**:
- ✅ Simple to implement and understand
- ✅ Fast consensus (no complex algorithms)
- ✅ Proven in distributed systems (Raft, Paxos)
- ✅ Fault-tolerant (works with minority failures)

**Alternatives Considered**:
- Byzantine fault tolerance (too complex for initial release)
- Weighted voting (requires calibration)
- Unanimous consensus (too strict, blocks on single failure)

### ADR-002: Why Lock-Free Snapshots?
**Decision**: Use lock-free snapshot-based memory for reads.

**Reasoning**:
- ✅ Readers never block (critical for performance)
- ✅ Writers use staging area (eventual consistency)
- ✅ Atomic snapshot swaps (consistent views)
- ✅ Garbage collection of old snapshots (bounded memory)

**Alternatives Considered**:
- RwLock-based shared state (readers block writers)
- MVCC (Multi-Version Concurrency Control) (more complex)
- Per-worker isolated state (no shared intelligence)

### ADR-003: Why Unified Coordinator?
**Decision**: Create a single `SwarmCoordinator` that wraps `HiveQueen` and `Ultrathink`.

**Reasoning**:
- ✅ Single API for all swarm operations
- ✅ Minimal changes to existing code
- ✅ Clear integration points
- ✅ Easy to extend with new swarm types

**Alternatives Considered**:
- Modify existing coordinators directly (high risk)
- Separate coordinators for each domain (fragmented UX)
- No coordinator (manual orchestration by users)

---

## 12. Security Considerations

### 12.1 Threat Model
- **Agent Impersonation**: Malicious agent submits false results
- **Consensus Manipulation**: Attacker controls majority of workers
- **Memory Poisoning**: Corrupted data in shared state
- **DoS Attacks**: Flooding task queue with junk tasks

### 12.2 Mitigations
1. **Agent Authentication**: Workers sign results with cryptographic keys
2. **Quorum Requirements**: Minimum 3 agents for consensus (prevents single rogue agent)
3. **Memory Validation**: Checksums on snapshots, rollback on corruption
4. **Rate Limiting**: Max tasks per second, worker spawn limits
5. **Audit Logging**: All consensus decisions logged for review

---

## 13. Future Enhancements (Post-V1)

### 13.1 Advanced Consensus Algorithms
- **Byzantine Fault Tolerance**: Tolerate malicious agents
- **Weighted Voting**: Agents with better track records have higher vote weight
- **Hierarchical Consensus**: Multi-tier voting (local → regional → global)

### 13.2 Learning and Adaptation
- **Agent Skill Improvement**: Workers learn from successful task patterns
- **Auto-tuning**: Automatically adjust worker count, thresholds based on load
- **Conflict Prediction**: ML model predicts likely conflicts before they occur

### 13.3 Cross-Instance Coordination
- **Distributed Swarms**: Multiple ggen instances share a global swarm
- **Federated Consensus**: Consensus across organizational boundaries
- **Cloud Integration**: Offload heavy tasks to cloud workers

---

## Appendix A: Glossary

| Term | Definition |
|------|------------|
| **Swarm** | Collection of AI agents working together on tasks |
| **Consensus** | Agreement among majority of agents on a result |
| **Worker** | Individual agent in the swarm that executes tasks |
| **Coordinator** | Central system that manages swarm orchestration |
| **Snapshot** | Point-in-time consistent view of shared memory |
| **Lock-Free** | Data structure that never blocks readers |
| **Majority Voting** | Decision based on ≥50% agent agreement |
| **HiveQueen** | Existing ontology-specific coordinator |
| **Ultrathink** | Existing general-purpose swarm system |

---

## Appendix B: Reference Architecture Patterns

This design draws from:
- **Raft Consensus Algorithm**: Majority-based leader election
- **MVCC (PostgreSQL)**: Lock-free snapshot reads
- **Actor Model (Erlang/Akka)**: Isolated workers with message passing
- **MapReduce**: Task distribution and result aggregation
- **Circuit Breaker Pattern**: Failure detection and recovery

---

**End of Design Document**

*For implementation questions, contact the Hive Mind Swarm System Architect.*
