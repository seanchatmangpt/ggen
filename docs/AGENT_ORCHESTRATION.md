<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Agent Orchestration Guide](#agent-orchestration-guide)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
    - [Architecture Comparison](#architecture-comparison)
  - [Pattern Selection Guide](#pattern-selection-guide)
    - [Use HyperConcurrent When:](#use-hyperconcurrent-when)
    - [Use Microframework When:](#use-microframework-when)
    - [Use Swarm When:](#use-swarm-when)
  - [HyperConcurrent Pattern](#hyperconcurrent-pattern)
    - [Overview](#overview-1)
    - [Architecture](#architecture)
    - [Configuration](#configuration)
    - [Basic Usage](#basic-usage)
    - [Advanced Features](#advanced-features)
      - [Priority Scheduling](#priority-scheduling)
      - [Barrier Synchronization](#barrier-synchronization)
      - [Inter-Agent Communication](#inter-agent-communication)
      - [Circuit Breaker Pattern](#circuit-breaker-pattern)
    - [Configuration Options](#configuration-options)
  - [Microframework Pattern](#microframework-pattern)
    - [Overview](#overview-2)
    - [Architecture](#architecture-1)
    - [Configuration](#configuration-1)
    - [Basic Usage](#basic-usage-1)
    - [Pre-built Agents](#pre-built-agents)
      - [CodeGenAgent](#codegenagent)
      - [TesterAgent](#testeragent)
      - [ReviewerAgent](#revieweragent)
      - [ValidatorAgent](#validatoragent)
      - [RdfProcessorAgent](#rdfprocessoragent)
      - [TemplateGenAgent](#templategenagent)
    - [Task Dependencies](#task-dependencies)
    - [Progress Tracking](#progress-tracking)
    - [Pipeline Pattern](#pipeline-pattern)
    - [Batch Processing](#batch-processing)
  - [Swarm Pattern](#swarm-pattern)
    - [Overview](#overview-3)
    - [Architecture](#architecture-2)
    - [Configuration](#configuration-2)
    - [Basic Usage](#basic-usage-2)
    - [Continuous Mode](#continuous-mode)
    - [Swarm Agents](#swarm-agents)
      - [GraphExtenderAgent (AI-Powered)](#graphextenderagent-ai-powered)
      - [TemplateGeneratorAgent (AI-Powered)](#templategeneratoragent-ai-powered)
      - [ACOSparqlAgent (Ant Colony Optimization)](#acosparqlagent-ant-colony-optimization)
      - [PSOTemplateAgent (Particle Swarm Optimization)](#psotemplateagent-particle-swarm-optimization)
      - [LearningAgent (Adaptive)](#learningagent-adaptive)
    - [Event-Driven Architecture](#event-driven-architecture)
    - [Swarm Status Monitoring](#swarm-status-monitoring)
  - [DSPy Integration](#dspy-integration)
    - [DSPy Signature with HyperConcurrent](#dspy-signature-with-hyperconcurrent)
    - [DSPy Chain-of-Thought with Microframework](#dspy-chain-of-thought-with-microframework)
    - [DSPy Module with Swarm](#dspy-module-with-swarm)
  - [End-to-End Examples](#end-to-end-examples)
    - [Example 1: Parallel Code Generation Pipeline](#example-1-parallel-code-generation-pipeline)
    - [Example 2: Autonomous Swarm with Learning](#example-2-autonomous-swarm-with-learning)
    - [Example 3: Hybrid HyperConcurrent + DSPy](#example-3-hybrid-hyperconcurrent--dspy)
  - [Performance Considerations](#performance-considerations)
    - [Memory Budgets](#memory-budgets)
    - [Execution Time SLOs](#execution-time-slos)
    - [Concurrency Limits](#concurrency-limits)
  - [Best Practices](#best-practices)
    - [1. Choose the Right Pattern](#1-choose-the-right-pattern)
    - [2. Handle Errors Properly](#2-handle-errors-properly)
    - [3. Monitor Performance](#3-monitor-performance)
    - [4. Use Circuit Breakers](#4-use-circuit-breakers)
    - [5. Respect Resource Limits](#5-respect-resource-limits)
    - [6. Test with Realistic Workloads](#6-test-with-realistic-workloads)
    - [7. Use DSPy for Structured Outputs](#7-use-dspy-for-structured-outputs)
    - [8. Enable Learning in Production](#8-enable-learning-in-production)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Agent Orchestration Guide

**ggen-ai Multi-Agent Execution Patterns**

This guide documents the three orchestration patterns available in ggen-ai for parallel agent execution, each optimized for different use cases and scaling requirements.

## Table of Contents

- [Overview](#overview)
- [Pattern Selection Guide](#pattern-selection-guide)
- [HyperConcurrent Pattern](#hyperconcurrent-pattern)
- [Microframework Pattern](#microframework-pattern)
- [Swarm Pattern](#swarm-pattern)
- [DSPy Integration](#dspy-integration)
- [End-to-End Examples](#end-to-end-examples)
- [Performance Considerations](#performance-considerations)
- [Best Practices](#best-practices)

## Overview

ggen-ai provides three distinct orchestration patterns for managing concurrent agent execution:

| Pattern | Max Agents | Use Case | Complexity | Autonomy |
|---------|-----------|----------|------------|----------|
| **HyperConcurrent** | 10 | Maximum parallelism, low-level control | High | Manual |
| **Microframework** | 10 | Simplified task orchestration, high-level API | Medium | Semi-automated |
| **Swarm** | Unlimited | Autonomous multi-agent systems | Low (for users) | Fully autonomous |

### Architecture Comparison

```text
┌─────────────────────────────────────────────────────────────────────┐
│                     HyperConcurrent Pattern                          │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  WorkStealingPool → CircuitBreaker → BackpressureHandler       │ │
│  │         ↓                  ↓                 ↓                  │ │
│  │    10 Agents (Maximum Parallelism, Manual Control)             │ │
│  └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                     Microframework Pattern                           │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  Orchestrator → TaskGraph → ProgressTracker → HyperConcurrent  │ │
│  │         ↓            ↓             ↓                            │ │
│  │  Pre-built Agents (CodeGen, Test, Review, Validate)            │ │
│  └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                        Swarm Pattern                                 │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  EventRouter → SwarmCoordinator → AlgorithmicAgents            │ │
│  │         ↓             ↓                    ↓                    │ │
│  │  Autonomous Learning & Self-Improvement (ACO, PSO, GA)         │ │
│  └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

## Pattern Selection Guide

### Use HyperConcurrent When:

- You need maximum control over execution flow
- Performance is critical (sub-second latency requirements)
- You're implementing custom concurrency patterns
- You need circuit breaker, backpressure, or adaptive concurrency
- You're building infrastructure-level components

### Use Microframework When:

- You want pre-built agents (CodeGen, Test, Review, Validate)
- You need task dependency graphs (A before B before C)
- You want progress tracking and monitoring
- You're building CI/CD pipelines or build tools
- You need a balance of control and simplicity

### Use Swarm When:

- You need autonomous operation (file watching, git hooks)
- You want emergent behaviors from agent collaboration
- You need algorithmic optimization (ACO for SPARQL, PSO for templates)
- You're building self-improving systems
- You want event-driven architecture

## HyperConcurrent Pattern

### Overview

Maximum 10-agent parallelism with advanced concurrency primitives. Direct access to work-stealing pools, circuit breakers, backpressure handlers, and adaptive concurrency controllers.

### Architecture

```text
┌─────────────────────────────────────────────────────────────────┐
│                    HyperConcurrentExecutor                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │   Adaptive  │  │   Circuit   │  │     Backpressure        │ │
│  │ Concurrency │──│   Breaker   │──│       Handler           │ │
│  │ Controller  │  │   Manager   │  │                         │ │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘ │
│         │                │                     │                │
│         └────────────────┼─────────────────────┘                │
│                          ▼                                      │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │      WorkStealingAgentPool (10 agents max)              │   │
│  │  ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐│   │
│  │  │A1 │ │A2 │ │A3 │ │A4 │ │A5 │ │A6 │ │A7 │ │A8 │ │A9 ││A10││
│  │  └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘└───┘│
│  └─────────────────────────────────────────────────────────┘   │
│                          │                                      │
│                          ▼                                      │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                ChannelOrchestrator                       │   │
│  │     (flume channels for inter-agent communication)       │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### Configuration

```rust
use ggen_ai::hyper_concurrent::{HyperConcurrentConfig, HyperConcurrentExecutor};

// Default configuration (recommended)
let config = HyperConcurrentConfig::default();
// max_agents: 10
// enable_work_stealing: true
// enable_circuit_breaker: true
// enable_adaptive_concurrency: true
// enable_backpressure: true
// agent_timeout_secs: 60

// Maximum performance (aggressive)
let config = HyperConcurrentConfig::max_performance();
// max_agents: 10
// agent_timeout_secs: 30
// circuit_breaker_threshold: 3
// backpressure_queue_size: 50

// Conservative (stability-focused)
let config = HyperConcurrentConfig::conservative();
// max_agents: 5
// agent_timeout_secs: 120
// circuit_breaker_threshold: 10
// backpressure_queue_size: 200

// Development (testing)
let config = HyperConcurrentConfig::development();
// max_agents: 3
// enable_circuit_breaker: false
// enable_adaptive_concurrency: false
```

### Basic Usage

```rust
use ggen_ai::hyper_concurrent::{HyperConcurrentExecutor, HyperConcurrentConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create executor
    let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());

    // Define 10 parallel tasks
    let tasks: Vec<_> = (0..10)
        .map(|i| {
            let id = format!("agent-{}", i);
            let task = move || async move {
                // Your agent logic here
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                Ok(format!("Result from agent {}", i))
            };
            (id, task)
        })
        .collect();

    // Execute all in parallel (max 10 concurrent)
    let results = executor.execute_parallel(tasks).await;

    // Process results
    for result in results {
        if result.is_success() {
            println!("Agent {}: {:?}", result.agent_id, result.value);
        } else {
            println!("Agent {} failed: {:?}", result.agent_id, result.error);
        }
    }

    // Get metrics
    let metrics = executor.metrics();
    println!("Total executions: {}", metrics.total_executions);
    println!("Success rate: {:.2}%", metrics.success_rate * 100.0);
    println!("Avg execution time: {:.2}ms", metrics.avg_execution_time_ms);

    Ok(())
}
```

### Advanced Features

#### Priority Scheduling

```rust
use ggen_ai::hyper_concurrent::ExecutionPriority;

let tasks = vec![
    ("critical-task", ExecutionPriority::Critical, || async { Ok(()) }),
    ("normal-task", ExecutionPriority::Normal, || async { Ok(()) }),
    ("low-priority", ExecutionPriority::Low, || async { Ok(()) }),
];

let results = executor.execute_prioritized(tasks).await;
```

#### Barrier Synchronization

```rust
use ggen_ai::hyper_concurrent::AgentBarrier;

let barrier = AgentBarrier::new(10); // Wait for 10 agents

// All agents wait at barrier before proceeding
let result = executor.execute_with_barrier(
    &barrier,
    "agent-1".to_string(),
    || async { Ok("done") }
).await;
```

#### Inter-Agent Communication

```rust
use ggen_ai::hyper_concurrent::{ChannelOrchestrator, AgentMessage};

let channels = ChannelOrchestrator::new();

// Create channels for agents
channels.create_channel("agent-1");
channels.create_channel("agent-2");

// Send message from agent-1 to agent-2
let msg = AgentMessage::new(
    "agent-1",
    "agent-2",
    "task_complete",
    serde_json::json!({"status": "done"})
);
channels.send(msg)?;

// Receive message
if let Some(received) = channels.try_receive("agent-2") {
    println!("Received from {}: {:?}", received.source, received.payload);
}

// Broadcast to all agents
let broadcast = AgentMessage::broadcast(
    "agent-1",
    "shutdown",
    serde_json::json!({"immediate": true})
);
channels.send(broadcast)?;
```

#### Circuit Breaker Pattern

```rust
// Check if agent is in circuit breaker cooldown
if executor.is_circuit_open("agent-1") {
    println!("Agent-1 circuit is open, skipping");
    executor.reset_circuit("agent-1"); // Manual reset
}

// Circuit breaker automatically opens after threshold failures
// and closes after cooldown period
```

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `max_agents` | usize | 10 | Maximum concurrent agents (1-10) |
| `enable_work_stealing` | bool | true | Load balancing across agents |
| `enable_circuit_breaker` | bool | true | Fault tolerance for failing agents |
| `enable_adaptive_concurrency` | bool | true | Dynamic concurrency adjustment |
| `enable_backpressure` | bool | true | Prevent system overload |
| `agent_timeout_secs` | u64 | 60 | Per-agent execution timeout |
| `circuit_breaker_threshold` | u32 | 5 | Failures before circuit opens |
| `backpressure_queue_size` | usize | 100 | Max queued tasks |
| `metrics_enabled` | bool | true | Collect execution metrics |

## Microframework Pattern

### Overview

High-level orchestration API with pre-built agents, task graphs, and progress tracking. Built on top of HyperConcurrent for performance with simplified ergonomics.

### Architecture

```text
┌─────────────────────────────────────────────────────────────────┐
│                     AgentOrchestrator                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │   Builder   │  │    Task     │  │      Progress           │ │
│  │   Pattern   │──│    Graph    │──│      Tracker            │ │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘ │
│                           │                                      │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │               Pre-built Agent Types                         │ │
│  │  ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐    │ │
│  │  │CodeGe│ │Tester│ │Review│ │RdfPro│ │TmplGe│ │Valida│    │ │
│  │  │n     │ │      │ │er    │ │cessor│ │n     │ │tor   │    │ │
│  │  └──────┘ └──────┘ └──────┘ └──────┘ └──────┘ └──────┘    │ │
│  └────────────────────────────────────────────────────────────┘ │
│                           │                                      │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │          HyperConcurrentExecutor (10 max)                   │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Configuration

```rust
use ggen_ai::microframework::{MicroframeworkConfig, AgentOrchestrator};

// Default configuration
let config = MicroframeworkConfig::default();
// max_agents: 10
// enable_work_stealing: true
// enable_circuit_breaker: true
// enable_backpressure: true
// enable_progress_tracking: true
// default_timeout_secs: 60

// High performance (no tracking overhead)
let config = MicroframeworkConfig::high_performance();
// max_agents: 10
// enable_progress_tracking: false
// enable_metrics: false
// default_timeout_secs: 30

// Development
let config = MicroframeworkConfig::development();
// max_agents: 3
// enable_progress_tracking: true
// enable_metrics: true
// default_timeout_secs: 300
```

### Basic Usage

```rust
use ggen_ai::microframework::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create orchestrator with builder
    let orchestrator = AgentOrchestrator::builder()
        .max_agents(10)
        .enable_circuit_breaker()
        .enable_backpressure()
        .enable_progress_tracking()
        .build()?;

    // Register pre-built agents
    orchestrator.register_agent(CodeGenAgent::new("coder"));
    orchestrator.register_agent(TesterAgent::new("tester"));
    orchestrator.register_agent(ReviewerAgent::new("reviewer"));

    // Create tasks
    let tasks = vec![
        Task::code_gen("Generate User struct"),
        Task::test("Write tests for User"),
        Task::review("Review User implementation"),
    ];

    // Execute in parallel
    let results = orchestrator.execute_batch(tasks).await?;

    // Check results
    for result in &results {
        println!("Task {}: {}", result.task_id,
            if result.is_success() { "SUCCESS" } else { "FAILED" }
        );
        println!("  Output: {}", result.output);
        println!("  Duration: {}ms", result.duration_ms);
    }

    // Get statistics
    let stats = orchestrator.statistics();
    println!("Success rate: {:.2}%", stats.success_rate * 100.0);

    Ok(())
}
```

### Pre-built Agents

#### CodeGenAgent

```rust
use ggen_ai::microframework::CodeGenAgent;

let agent = CodeGenAgent::new("rust-codegen")
    .with_language("rust")
    .with_docs()
    .with_tests();

orchestrator.register_agent(agent);

let task = Task::code_gen("Generate payment processing module");
let result = orchestrator.execute(task).await?;
```

#### TesterAgent

```rust
use ggen_ai::microframework::TesterAgent;

let agent = TesterAgent::new("unit-tester")
    .with_framework("rstest");

orchestrator.register_agent(agent);

let task = Task::test("Write unit tests for payment module");
let result = orchestrator.execute(task).await?;
```

#### ReviewerAgent

```rust
use ggen_ai::microframework::ReviewerAgent;

let agent = ReviewerAgent::new("code-reviewer")
    .with_focus("security")
    .with_focus("performance");

orchestrator.register_agent(agent);

let task = Task::review("Review payment module for vulnerabilities");
let result = orchestrator.execute(task).await?;
```

#### ValidatorAgent

```rust
use ggen_ai::microframework::ValidatorAgent;

let agent = ValidatorAgent::new("rdf-validator")
    .with_shacl_shapes();

orchestrator.register_agent(agent);

let task = Task::validate("Validate RDF graph against SHACL");
let result = orchestrator.execute(task).await?;
```

#### RdfProcessorAgent

```rust
use ggen_ai::microframework::RdfProcessorAgent;

let agent = RdfProcessorAgent::new("rdf-processor")
    .with_sparql_support();

orchestrator.register_agent(agent);

let task = Task::custom(
    "process_rdf",
    serde_json::json!({
        "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
    })
);
let result = orchestrator.execute(task).await?;
```

#### TemplateGenAgent

```rust
use ggen_ai::microframework::TemplateGenAgent;

let agent = TemplateGenAgent::new("template-gen")
    .with_engine("tera");

orchestrator.register_agent(agent);

let task = Task::custom(
    "generate_template",
    serde_json::json!({
        "target": "rust_struct",
        "fields": ["name", "age", "email"]
    })
);
let result = orchestrator.execute(task).await?;
```

### Task Dependencies

```rust
use ggen_ai::microframework::{Task, TaskGraph};

// Define tasks with dependencies
let t1 = Task::code_gen("Generate struct")
    .with_id("gen-struct");

let t2 = Task::test("Write tests")
    .with_id("write-tests")
    .depends_on("gen-struct");

let t3 = Task::review("Review code")
    .with_id("review")
    .depends_on("write-tests");

// Execute respecting dependencies
let results = orchestrator.execute_with_dependencies(vec![t1, t2, t3]).await?;
```

### Progress Tracking

```rust
// Get real-time progress
let progress = orchestrator.progress();

println!("Total tasks: {}", progress.total_tasks());
println!("Completed: {}", progress.completed_tasks());
println!("Running: {}", progress.running_tasks());
println!("Failed: {}", progress.failed_tasks());

// Get progress for specific task
if let Some(task_progress) = progress.get_task("gen-struct") {
    println!("Task status: {:?}", task_progress.status);
    println!("Started at: {}", task_progress.start_time);
}
```

### Pipeline Pattern

```rust
use ggen_ai::microframework::Pipeline;

// Build a processing pipeline
let pipeline = Pipeline::builder()
    .add_stage("generate", vec![
        Task::code_gen("Generate domain model"),
    ])
    .add_stage("test", vec![
        Task::test("Write unit tests"),
        Task::test("Write integration tests"),
    ])
    .add_stage("review", vec![
        Task::review("Security review"),
        Task::review("Performance review"),
    ])
    .build()?;

// Execute pipeline (stages run sequentially, tasks within stages run in parallel)
let results = pipeline.execute(&orchestrator).await?;

for stage_result in results.stage_results {
    println!("Stage {}: {}/{} succeeded",
        stage_result.stage_name,
        stage_result.successful_tasks,
        stage_result.total_tasks
    );
}
```

### Batch Processing

```rust
use ggen_ai::microframework::BatchProcessor;

let processor = BatchProcessor::new(orchestrator)
    .with_batch_size(10)
    .with_retry_count(3);

let all_tasks: Vec<Task> = vec![/* 100 tasks */];

// Process in batches of 10
let results = processor.process(all_tasks).await?;

println!("Total processed: {}", results.total_processed);
println!("Total successful: {}", results.total_successful);
println!("Total failed: {}", results.total_failed);
```

## Swarm Pattern

### Overview

Autonomous multi-agent system with event-driven architecture, algorithmic optimization (ACO, PSO, GA), and self-improvement capabilities. Ideal for long-running, self-managing systems.

### Architecture

```text
┌─────────────────────────────────────────────────────────────────┐
│                  SwarmOrchestrator                               │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    EventRouter                               ││
│  │  ┌───────────────┐  ┌───────────────┐  ┌─────────────────┐ ││
│  │  │ FileSystem    │  │   Git Event   │  │  Custom Event   │ ││
│  │  │ EventSource   │  │    Source     │  │    Sources      │ ││
│  │  └───────────────┘  └───────────────┘  └─────────────────┘ ││
│  └─────────────────────────────────────────────────────────────┘│
│                            │                                     │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                 UltrathinkSwarm                              ││
│  │  ┌─────────────────────────────────────────────────────────┐││
│  │  │               SwarmCoordinator                           │││
│  │  │  ┌────────────────────────────────────────────────────┐ │││
│  │  │  │         Specialized Swarm Agents                    │ │││
│  │  │  │  ┌──────────────┐  ┌──────────────┐  ┌───────────┐ │ │││
│  │  │  │  │GraphExtender │  │TemplateGen   │  │ CodeGen   │ │ │││
│  │  │  │  │(AI-powered)  │  │(AI-powered)  │  │(Template) │ │ │││
│  │  │  │  └──────────────┘  └──────────────┘  └───────────┘ │ │││
│  │  │  │  ┌──────────────┐  ┌──────────────┐  ┌───────────┐ │ │││
│  │  │  │  │ACOSparqlAgent│  │PSOTemplateAgt│  │LearningAgt│ │ │││
│  │  │  │  │(Ant Colony)  │  │(Particle Swm)│  │(Adaptive) │ │ │││
│  │  │  │  └──────────────┘  └──────────────┘  └───────────┘ │ │││
│  │  │  └────────────────────────────────────────────────────┘ │││
│  │  └─────────────────────────────────────────────────────────┘││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
```

### Configuration

```rust
use ggen_ai::swarm::{
    SwarmConfig, PerformanceThresholds, OrchestrationConfig
};
use std::path::PathBuf;

// Swarm configuration
let swarm_config = SwarmConfig {
    max_concurrent_agents: 10,
    agent_timeout_seconds: 60,
    learning_enabled: true,
    autonomous_mode: true,
    performance_thresholds: PerformanceThresholds {
        max_execution_time_ms: 10000,
        max_memory_usage_mb: 500,
        min_success_rate: 0.95,
    },
};

// Orchestration configuration
let orch_config = OrchestrationConfig {
    continuous_mode: true,
    watch_paths: vec![
        PathBuf::from("src"),
        PathBuf::from("templates"),
    ],
    git_repositories: vec![
        PathBuf::from("."),
    ],
    learning_enabled: true,
    max_operations_per_hour: 100,
};
```

### Basic Usage

```rust
use ggen_ai::swarm::{SwarmOrchestrator, OrchestrationConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create orchestrator
    let config = OrchestrationConfig {
        continuous_mode: false,  // Manual mode for now
        ..Default::default()
    };
    let orchestrator = SwarmOrchestrator::new(config);

    // Initialize with standard agents
    orchestrator.initialize().await?;

    // Execute autonomous operation
    let result = orchestrator.execute_autonomous_operation(
        Some("file_modified".to_string())
    ).await?;

    // Check results
    println!("Operation type: {}", result.operation_summary.operation_type);
    println!("Artifacts generated: {}", result.operation_summary.artifacts_generated);
    println!("Success rate: {:.2}%", result.operation_summary.success_rate * 100.0);

    for artifact in result.artifacts_with_context {
        println!("\nArtifact: {}", artifact.artifact.artifact_type);
        println!("  Source: {}", artifact.artifact.source_agent);
        println!("  Quality: {:.2}", artifact.artifact.quality_score);
        println!("  Impact: {} ({})",
            artifact.impact.scope,
            artifact.impact.risk_level
        );
    }

    Ok(())
}
```

### Continuous Mode

```rust
use ggen_ai::swarm::{SwarmOrchestrator, OrchestrationConfig};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Enable continuous autonomous operation
    let config = OrchestrationConfig {
        continuous_mode: true,
        watch_paths: vec![
            PathBuf::from("src"),
            PathBuf::from("templates"),
        ],
        git_repositories: vec![
            PathBuf::from("."),
        ],
        learning_enabled: true,
        max_operations_per_hour: 50,
    };

    let orchestrator = SwarmOrchestrator::new(config);
    orchestrator.initialize().await?;

    // Swarm now runs autonomously:
    // - Watches file system for changes
    // - Monitors git commits
    // - Automatically generates/updates code
    // - Self-improves through learning

    // Check status periodically
    loop {
        tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;

        let status = orchestrator.status().await;
        println!("Active agents: {}", status.swarm_status.active_agents);
        println!("Total operations: {}", status.swarm_status.metrics.total_operations);
        println!("Success rate: {:.2}%",
            status.swarm_status.metrics.successful_operations as f64 /
            status.swarm_status.metrics.total_operations as f64 * 100.0
        );
    }
}
```

### Swarm Agents

#### GraphExtenderAgent (AI-Powered)

```rust
use ggen_ai::swarm::agents::{GraphExtenderAgent, graph_extender::GraphContext};
use std::collections::HashMap;

let context = GraphContext {
    schema: "Current RDF schema...".to_string(),
    namespaces: HashMap::from([
        ("rdf".to_string(), "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string()),
        ("rdfs".to_string(), "http://www.w3.org/2000/01/rdf-schema#".to_string()),
    ]),
    domain_knowledge: HashMap::new(),
};

let agent = GraphExtenderAgent::default_config(context);
// Agent uses LLM to infer new triples from existing graph
```

#### TemplateGeneratorAgent (AI-Powered)

```rust
use ggen_ai::swarm::agents::{TemplateGeneratorAgent, template_generator::TemplateContext};
use ggen_ai::generators::TemplateGenerator;
use std::collections::HashMap;

let context = TemplateContext {
    template_paths: vec!["templates/".to_string()],
    graph_schema: "Current schema...".to_string(),
    template_patterns: HashMap::new(),
    language_configs: HashMap::new(),
};

let llm_client = Box::new(/* your LLM client */);
let generator = TemplateGenerator::new(llm_client);
let agent = TemplateGeneratorAgent::new(generator, context);
// Agent generates templates based on graph changes
```

#### ACOSparqlAgent (Ant Colony Optimization)

```rust
use ggen_ai::swarm::agents::ACOSparqlAgent;

let agent = ACOSparqlAgent::new("sparql-optimizer");
// Uses ant colony optimization to find optimal SPARQL query execution paths
```

#### PSOTemplateAgent (Particle Swarm Optimization)

```rust
use ggen_ai::swarm::agents::PSOTemplateAgent;

let agent = PSOTemplateAgent::new("template-tuner");
// Uses particle swarm optimization to tune template parameters
```

#### LearningAgent (Adaptive)

```rust
use ggen_ai::swarm::agents::LearningAgent;

let agent = LearningAgent::new("learner");
// Learns from past executions to improve future performance
```

### Event-Driven Architecture

```rust
use ggen_ai::swarm::{
    EventRouter, FileSystemEventSource, GitEventSource, GitEventType, EventFilter
};
use std::path::PathBuf;

let mut router = EventRouter::new();

// Add file system watcher
let fs_source = FileSystemEventSource::new(
    vec![PathBuf::from("src"), PathBuf::from("templates")],
    vec!["rs".to_string(), "toml".to_string(), "ttl".to_string()],
);
router.add_event_source("filesystem".to_string(), Box::new(fs_source));

// Add git watcher
let git_source = GitEventSource::new(
    vec![PathBuf::from(".")],
    vec![GitEventType::Commit, GitEventType::Push],
);
router.add_event_source("git".to_string(), Box::new(git_source));

// Route events to specific agents
router.add_event_filter("graph_extender".to_string(), EventFilter {
    name: "rust_files_to_graph".to_string(),
    event_types: vec!["filesystem".to_string()],
    path_patterns: vec!["src/".to_string()],
    target_agent: "graph_extender".to_string(),
});

// Start monitoring
router.start_monitoring().await?;
```

### Swarm Status Monitoring

```rust
let status = orchestrator.status().await;

println!("Swarm Status:");
println!("  Active agents: {}", status.swarm_status.active_agents);
println!("  Total agents: {}", status.swarm_status.total_agents);
println!("  Autonomous mode: {}", status.autonomous_mode);
println!("  Learning enabled: {}", status.learning_enabled);

println!("\nMetrics:");
println!("  Total operations: {}", status.swarm_status.metrics.total_operations);
println!("  Successful: {}", status.swarm_status.metrics.successful_operations);
println!("  Failed: {}", status.swarm_status.metrics.failed_operations);
println!("  Avg execution time: {:.2}ms",
    status.swarm_status.metrics.avg_execution_time_ms);

println!("\nAgent Health:");
for (agent_name, health) in &status.swarm_status.agent_health {
    println!("  {}: {:?} (score: {:.2})",
        agent_name,
        health.status,
        health.score
    );
}
```

## DSPy Integration

All three orchestration patterns integrate seamlessly with DSPy modules for structured LLM interactions.

### DSPy Signature with HyperConcurrent

```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, Predictor};
use ggen_ai::hyper_concurrent::HyperConcurrentExecutor;

// Define task signature
let signature = Signature::new("CodeGeneration", "Generate Rust code from description")
    .with_input(InputField::new("description", "Code description"))
    .with_output(OutputField::new("code", "Generated Rust code"))
    .with_output(OutputField::new("tests", "Unit tests"));

// Create predictor
let predictor = Predictor::with_model(signature, "gpt-4");

// Execute with hyper-concurrent
let executor = HyperConcurrentExecutor::max_performance();

let tasks: Vec<_> = descriptions.into_iter()
    .enumerate()
    .map(|(i, desc)| {
        let pred = predictor.clone();
        let id = format!("codegen-{}", i);
        let task = move || async move {
            let inputs = HashMap::from([
                ("description".to_string(), serde_json::json!(desc))
            ]);
            pred.forward(inputs).await
        };
        (id, task)
    })
    .collect();

let results = executor.execute_parallel(tasks).await;
```

### DSPy Chain-of-Thought with Microframework

```rust
use ggen_ai::dspy::{Signature, ChainOfThought};
use ggen_ai::microframework::{AgentOrchestrator, Task, CustomAgent};

// Create Chain-of-Thought predictor
let signature = Signature::new("ComplexReasoning", "Solve complex coding problem")
    .with_input(InputField::new("problem", "Problem description"))
    .with_output(OutputField::new("reasoning", "Step-by-step reasoning"))
    .with_output(OutputField::new("solution", "Final solution"));

let cot = ChainOfThought::new(signature, "gpt-4");

// Wrap in custom agent
struct ReasoningAgent {
    cot: ChainOfThought,
}

#[async_trait]
impl MicroAgent for ReasoningAgent {
    fn name(&self) -> &str { "reasoning-agent" }
    fn role(&self) -> AgentRole { AgentRole::Custom("reasoning".to_string()) }
    fn supported_tasks(&self) -> Vec<TaskType> { vec![TaskType::Custom] }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let inputs = HashMap::from([
            ("problem".to_string(), task.input.clone())
        ]);
        let outputs = self.cot.forward(inputs).await?;

        Ok(TaskResult::success(
            task.id.clone(),
            outputs.get("solution").unwrap().to_string(),
            0
        ))
    }
}

// Use in orchestrator
let orchestrator = AgentOrchestrator::new();
orchestrator.register_agent(ReasoningAgent { cot });
```

### DSPy Module with Swarm

```rust
use ggen_ai::dspy::{Module, Signature, Predictor};
use ggen_ai::swarm::{SwarmAgent, SwarmContext, AgentInput, AgentOutput};

// Create custom DSPy-powered swarm agent
struct DspySwarmAgent {
    name: String,
    predictor: Predictor,
}

#[async_trait]
impl SwarmAgent for DspySwarmAgent {
    fn name(&self) -> &str { &self.name }

    fn capabilities(&self) -> Vec<String> {
        vec!["llm_prediction".to_string(), "structured_output".to_string()]
    }

    async fn execute(
        &self,
        context: &SwarmContext,
        input: AgentInput
    ) -> Result<AgentOutput> {
        // Convert input to DSPy format
        let inputs = HashMap::from([
            ("input".to_string(), input.data)
        ]);

        // Execute predictor
        let outputs = self.predictor.forward(inputs).await?;

        Ok(AgentOutput {
            data: serde_json::json!(outputs),
            output_type: "prediction".to_string(),
            target_agents: vec![],
            metadata: HashMap::new(),
        })
    }

    async fn validate(&self) -> Result<bool> { Ok(true) }

    async fn health_check(&self) -> AgentHealth {
        AgentHealth {
            status: HealthStatus::Healthy,
            score: 1.0,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues: vec![],
        }
    }
}

// Add to swarm
let signature = Signature::new("Inference", "Make predictions");
let predictor = Predictor::with_model(signature, "gpt-4");
let agent = DspySwarmAgent {
    name: "dspy-agent".to_string(),
    predictor,
};

swarm.add_agent(Box::new(agent)).await?;
```

## End-to-End Examples

### Example 1: Parallel Code Generation Pipeline

```rust
use ggen_ai::microframework::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create orchestrator
    let orchestrator = AgentOrchestrator::builder()
        .max_agents(10)
        .enable_progress_tracking()
        .build()?;

    // Register agents
    orchestrator.register_agent(CodeGenAgent::new("codegen").with_docs().with_tests());
    orchestrator.register_agent(TesterAgent::new("tester"));
    orchestrator.register_agent(ReviewerAgent::new("reviewer"));
    orchestrator.register_agent(ValidatorAgent::new("validator"));

    // Define pipeline
    let pipeline = Pipeline::builder()
        .add_stage("generate", vec![
            Task::code_gen("Generate User model"),
            Task::code_gen("Generate Payment model"),
            Task::code_gen("Generate Order model"),
        ])
        .add_stage("test", vec![
            Task::test("Test User model"),
            Task::test("Test Payment model"),
            Task::test("Test Order model"),
        ])
        .add_stage("review", vec![
            Task::review("Security review"),
            Task::review("Performance review"),
        ])
        .add_stage("validate", vec![
            Task::validate("Run all checks"),
        ])
        .build()?;

    // Execute pipeline
    println!("Starting code generation pipeline...");
    let result = pipeline.execute(&orchestrator).await?;

    // Report results
    println!("\nPipeline Results:");
    println!("  Total duration: {}ms", result.total_duration_ms);
    println!("  Success: {}", result.success);

    for stage in result.stage_results {
        println!("\nStage: {}", stage.stage_name);
        println!("  Tasks: {}/{} successful",
            stage.successful_tasks,
            stage.total_tasks
        );
        println!("  Duration: {}ms", stage.duration_ms);
    }

    Ok(())
}
```

### Example 2: Autonomous Swarm with Learning

```rust
use ggen_ai::swarm::{SwarmOrchestrator, OrchestrationConfig};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Configure autonomous swarm
    let config = OrchestrationConfig {
        continuous_mode: true,
        watch_paths: vec![
            PathBuf::from("src"),
            PathBuf::from(".specify"),
        ],
        git_repositories: vec![
            PathBuf::from("."),
        ],
        learning_enabled: true,
        max_operations_per_hour: 100,
    };

    let orchestrator = SwarmOrchestrator::new(config);

    println!("Initializing autonomous swarm...");
    orchestrator.initialize().await?;

    println!("Swarm is now running autonomously.");
    println!("Monitoring file system and git for changes...\n");

    // Monitor swarm status
    let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(30));

    loop {
        interval.tick().await;

        let status = orchestrator.status().await;

        println!("Swarm Status Update:");
        println!("  Active agents: {}/{}",
            status.swarm_status.active_agents,
            status.swarm_status.total_agents
        );

        let metrics = &status.swarm_status.metrics;
        if metrics.total_operations > 0 {
            let success_rate = metrics.successful_operations as f64 /
                              metrics.total_operations as f64 * 100.0;
            println!("  Operations: {} total, {:.1}% success",
                metrics.total_operations,
                success_rate
            );
            println!("  Avg execution: {:.2}ms", metrics.avg_execution_time_ms);
        }

        // Check agent health
        let unhealthy: Vec<_> = status.swarm_status.agent_health.iter()
            .filter(|(_, health)| !matches!(health.status, HealthStatus::Healthy))
            .collect();

        if !unhealthy.is_empty() {
            println!("  Warning: {} unhealthy agents", unhealthy.len());
            for (name, health) in unhealthy {
                println!("    - {}: {:?}", name, health.status);
            }
        }

        println!();
    }
}
```

### Example 3: Hybrid HyperConcurrent + DSPy

```rust
use ggen_ai::hyper_concurrent::{HyperConcurrentExecutor, HyperConcurrentConfig};
use ggen_ai::dspy::{Signature, InputField, OutputField, ChainOfThought};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create executor with max performance
    let executor = HyperConcurrentExecutor::new(
        HyperConcurrentConfig::max_performance()
    );

    // Define DSPy signature for code review
    let signature = Signature::new(
        "CodeReview",
        "Perform thorough code review with reasoning"
    )
    .with_input(InputField::new("code", "Code to review"))
    .with_input(InputField::new("language", "Programming language"))
    .with_output(OutputField::new("reasoning", "Step-by-step analysis"))
    .with_output(OutputField::new("issues", "List of identified issues"))
    .with_output(OutputField::new("suggestions", "Improvement suggestions"))
    .with_instructions("Focus on security, performance, and maintainability");

    // Create Chain-of-Thought predictor
    let cot = ChainOfThought::new(signature, "gpt-4");

    // Prepare 10 code files for parallel review
    let code_files = vec![
        ("user.rs", "Rust"),
        ("payment.rs", "Rust"),
        ("order.rs", "Rust"),
        ("api.rs", "Rust"),
        ("db.rs", "Rust"),
        ("auth.rs", "Rust"),
        ("cache.rs", "Rust"),
        ("metrics.rs", "Rust"),
        ("logging.rs", "Rust"),
        ("config.rs", "Rust"),
    ];

    // Execute reviews in parallel
    let tasks: Vec<_> = code_files.into_iter()
        .enumerate()
        .map(|(i, (file, lang))| {
            let cot_clone = cot.clone();
            let id = format!("review-{}", i);
            let file = file.to_string();
            let lang = lang.to_string();

            let task = move || async move {
                let code = std::fs::read_to_string(&file)?;
                let inputs = HashMap::from([
                    ("code".to_string(), serde_json::json!(code)),
                    ("language".to_string(), serde_json::json!(lang)),
                ]);

                cot_clone.forward(inputs).await
            };

            (id, task)
        })
        .collect();

    println!("Starting parallel code review of 10 files...");
    let results = executor.execute_parallel(tasks).await;

    // Process results
    println!("\nReview Results:");
    for result in results {
        if result.is_success() {
            if let Some(outputs) = result.value {
                println!("\n{}: SUCCESS", result.agent_id);
                println!("  Duration: {}ms", result.duration_ms);
                if let Some(issues) = outputs.get("issues") {
                    println!("  Issues: {}", issues);
                }
            }
        } else {
            println!("\n{}: FAILED - {}",
                result.agent_id,
                result.error.unwrap_or_default()
            );
        }
    }

    // Get metrics
    let metrics = executor.metrics();
    println!("\nExecution Metrics:");
    println!("  Total executions: {}", metrics.total_executions);
    println!("  Success rate: {:.2}%", metrics.success_rate * 100.0);
    println!("  Avg time: {:.2}ms", metrics.avg_execution_time_ms);

    Ok(())
}
```

## Performance Considerations

### Memory Budgets

All patterns respect ggen's memory budget of 100MB:

```rust
// Monitor memory usage
let metrics = executor.metrics();
if metrics.memory_usage_mb > 90.0 {
    warn!("Approaching memory limit: {:.2}MB", metrics.memory_usage_mb);
}

// Swarm automatic throttling
let swarm_status = orchestrator.status().await;
if swarm_status.swarm_status.metrics.memory_usage_mb > 100.0 {
    // Swarm automatically reduces concurrency
}
```

### Execution Time SLOs

Target SLOs for each pattern:

| Pattern | Target Latency | Max Latency |
|---------|---------------|-------------|
| HyperConcurrent | <5s | <30s |
| Microframework | <16s | <30s |
| Swarm | <10s (per operation) | Variable |

```rust
// Set timeouts
let config = HyperConcurrentConfig {
    agent_timeout_secs: 30,  // Hard limit
    ..Default::default()
};

// Monitor SLO compliance
if result.duration_ms > 30_000 {
    warn!("SLO violation: {}ms", result.duration_ms);
}
```

### Concurrency Limits

All patterns enforce maximum 10 concurrent agents:

```rust
// HyperConcurrent: Semaphore with 10 permits
assert_eq!(executor.semaphore.available_permits(), 10);

// Microframework: Delegates to HyperConcurrent
assert_eq!(orchestrator.config.max_agents, 10);

// Swarm: Configurable but recommended ≤10
let config = SwarmConfig {
    max_concurrent_agents: 10,
    ..Default::default()
};
```

## Best Practices

### 1. Choose the Right Pattern

```rust
// Low-level control + maximum performance
let executor = HyperConcurrentExecutor::max_performance();

// High-level API + pre-built agents
let orchestrator = AgentOrchestrator::builder().build()?;

// Autonomous operation + learning
let swarm = SwarmOrchestrator::new(OrchestrationConfig::default());
```

### 2. Handle Errors Properly

```rust
// HyperConcurrent: Check result state
for result in results {
    match result.state {
        AgentExecutionState::Completed => { /* success */ },
        AgentExecutionState::Failed => { /* handle error */ },
        AgentExecutionState::TimedOut => { /* retry or skip */ },
        AgentExecutionState::CircuitOpen => { /* backoff */ },
        _ => { /* other states */ }
    }
}

// Microframework: Result type
match orchestrator.execute(task).await {
    Ok(result) if result.is_success() => { /* success */ },
    Ok(result) => { /* task failed but didn't error */ },
    Err(e) => { /* orchestrator error */ },
}

// Swarm: Automatic error handling with learning
// Swarm learns from failures and adapts
```

### 3. Monitor Performance

```rust
// Collect metrics
let metrics = executor.metrics();
tracing::info!(
    success_rate = %metrics.success_rate,
    avg_time_ms = %metrics.avg_execution_time_ms,
    "Execution metrics"
);

// Track progress
let progress = orchestrator.progress();
let completion_rate = progress.completed_tasks() as f64 /
                      progress.total_tasks() as f64;

// Monitor swarm health
let status = swarm.status().await;
for (agent, health) in status.swarm_status.agent_health {
    if health.score < 0.8 {
        warn!("Agent {} health degraded: {:.2}", agent, health.score);
    }
}
```

### 4. Use Circuit Breakers

```rust
// Enable circuit breakers to prevent cascading failures
let config = HyperConcurrentConfig {
    enable_circuit_breaker: true,
    circuit_breaker_threshold: 5,  // Open after 5 failures
    ..Default::default()
};

// Monitor circuit state
if executor.is_circuit_open("flaky-agent") {
    // Skip or use fallback
    executor.reset_circuit("flaky-agent");  // Manual reset if needed
}
```

### 5. Respect Resource Limits

```rust
// Memory
const MAX_MEMORY_MB: u64 = 100;

// Execution time
const MAX_EXECUTION_SECS: u64 = 30;

// Concurrency
const MAX_AGENTS: usize = 10;

// Configure accordingly
let config = HyperConcurrentConfig {
    max_agents: MAX_AGENTS,
    agent_timeout_secs: MAX_EXECUTION_SECS,
    ..Default::default()
};
```

### 6. Test with Realistic Workloads

```rust
#[tokio::test]
async fn test_10_agent_parallel_execution() {
    let executor = HyperConcurrentExecutor::new(
        HyperConcurrentConfig::default()
    );

    // Simulate realistic tasks
    let tasks: Vec<_> = (0..10)
        .map(|i| {
            let id = format!("agent-{}", i);
            let task = move || async move {
                // Simulate I/O and computation
                tokio::time::sleep(Duration::from_millis(100)).await;
                Ok(format!("Result {}", i))
            };
            (id, task)
        })
        .collect();

    let results = executor.execute_parallel(tasks).await;

    // Verify all succeeded
    assert_eq!(results.len(), 10);
    assert!(results.iter().all(|r| r.is_success()));

    // Verify performance
    let metrics = executor.metrics();
    assert!(metrics.avg_execution_time_ms < 1000.0);
}
```

### 7. Use DSPy for Structured Outputs

```rust
// Define clear signatures
let signature = Signature::new("TaskName", "Clear description")
    .with_input(InputField::new("input1", "Input description"))
    .with_output(OutputField::new("output1", "Output description"))
    .with_instructions("Specific instructions for the LLM");

// Validate outputs
let predictor = Predictor::with_model(signature, "gpt-4");
let outputs = predictor.forward(inputs).await?;

// Type-safe access
assert!(outputs.contains_key("output1"));
```

### 8. Enable Learning in Production

```rust
// Swarm learns from execution patterns
let config = OrchestrationConfig {
    learning_enabled: true,  // Adapt to patterns
    continuous_mode: true,   // Always improving
    ..Default::default()
};

// Microframework: Track and optimize
orchestrator.register_agent(
    CodeGenAgent::new("codegen")
        .with_docs()    // Learn doc patterns
        .with_tests()   // Learn test patterns
);
```

## Summary

| Feature | HyperConcurrent | Microframework | Swarm |
|---------|----------------|----------------|-------|
| **Max Concurrency** | 10 | 10 | Unlimited (10 recommended) |
| **Control Level** | Low-level | High-level | Autonomous |
| **Pre-built Agents** | No | Yes | Yes |
| **Task Dependencies** | Manual | Automatic | Event-driven |
| **Progress Tracking** | Manual | Built-in | Built-in |
| **Circuit Breaker** | Yes | Yes | Yes |
| **Work Stealing** | Yes | Yes | No |
| **Learning** | No | No | Yes |
| **DSPy Integration** | Manual | Easy | Easy |
| **Use Case** | Infrastructure | Pipelines | Autonomous systems |

Choose based on your needs:
- **HyperConcurrent**: Maximum performance, full control
- **Microframework**: Balanced productivity and control
- **Swarm**: Autonomous operation and self-improvement
