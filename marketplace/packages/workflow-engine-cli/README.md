# Workflow Engine CLI

> BPMN 2.0 workflow execution and orchestration using clap-noun-verb pattern

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Rust Version](https://img.shields.io/badge/rust-1.75%2B-blue.svg)](https://www.rust-lang.org)

## Overview

A powerful BPMN 2.0 workflow engine with CLI interface built using the clap-noun-verb pattern. Execute complex business processes with support for service tasks, user tasks, parallel gateways, events, and state management.

### Key Features

- **BPMN 2.0 Compliant**: Full support for BPMN 2.0 specification
- **Process Orchestration**: Execute complex workflows with multiple tasks
- **Task Management**: Service tasks, user tasks, script tasks, and more
- **Gateway Support**: Exclusive, parallel, inclusive, and event-based gateways
- **Event Handling**: Start, end, intermediate, and boundary events
- **State Persistence**: Durable workflow state with recovery capabilities
- **Instance Tracking**: Monitor and trace workflow execution
- **Performance Metrics**: Real-time workflow performance analytics

## Installation

```bash
# From crates.io
cargo install workflow-engine-cli

# From source
git clone https://github.com/yourusername/workflow-engine-cli
cd workflow-engine-cli
cargo install --path .

# Using package manager
cargo add workflow-engine-cli
```

## Quick Start

### Create a Simple Workflow

```bash
# Create a BPMN workflow definition
workflow-engine workflow create \
  --file examples/approval-process.bpmn \
  --name "Approval Workflow" \
  --version 1.0.0

# Validate the workflow
workflow-engine workflow validate \
  --file examples/approval-process.bpmn \
  --strict
```

### Deploy and Execute

```bash
# Deploy workflow to engine
workflow-engine workflow deploy \
  --workflow-id wf-approval-001 \
  --environment production

# Start a process instance
workflow-engine process start \
  --workflow-id wf-approval-001 \
  --variables '{"customer_id": "C001", "amount": 1000}'

# Check process status
workflow-engine process status \
  --instance-id inst-12345 \
  --include-tasks
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Workflow Engine CLI                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │ Workflow │  │ Process  │  │   Task   │  │ Instance │   │
│  │  Manager │  │ Executor │  │  Handler │  │  Tracker │   │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘   │
│       │             │              │             │          │
│       └─────────────┴──────────────┴─────────────┘          │
│                         │                                    │
│              ┌──────────▼──────────┐                        │
│              │   BPMN Engine Core  │                        │
│              │  - Process Parser   │                        │
│              │  - State Machine    │                        │
│              │  - Event Bus        │                        │
│              │  - Gateway Logic    │                        │
│              └──────────┬──────────┘                        │
│                         │                                    │
│       ┌─────────────────┼─────────────────┐                │
│       │                 │                 │                │
│  ┌────▼────┐      ┌────▼────┐      ┌────▼────┐           │
│  │  State  │      │  Event  │      │ Metrics │           │
│  │  Store  │      │ Handler │      │ Monitor │           │
│  └─────────┘      └─────────┘      └─────────┘           │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Nouns and Verbs

### Workflow Noun

Manage BPMN workflow definitions.

#### Verbs

**create** - Create a new workflow definition
```bash
workflow-engine workflow create \
  --file process.bpmn \
  --name "Customer Onboarding" \
  --version 1.0.0 \
  --description "Complete customer onboarding process"
```

**deploy** - Deploy a workflow to the execution engine
```bash
workflow-engine workflow deploy \
  --workflow-id wf-001 \
  --environment production \
  --validate \
  --dry-run
```

**list** - List all deployed workflows
```bash
workflow-engine workflow list \
  --status active \
  --filter "name contains approval" \
  --limit 20 \
  --format json
```

**validate** - Validate BPMN workflow definition
```bash
workflow-engine workflow validate \
  --file process.bpmn \
  --strict \
  --check-references \
  --output validation-report.json
```

**version** - Create a new version of a workflow
```bash
workflow-engine workflow version \
  --workflow-id wf-001 \
  --new-version 2.0.0 \
  --changes "Added parallel approval tasks" \
  --compatible-with 1.x
```

### Process Noun

Manage workflow process instances.

#### Verbs

**start** - Start a new process instance
```bash
workflow-engine process start \
  --workflow-id wf-001 \
  --variables '{"customer_id": "C001", "amount": 1000}' \
  --business-key "ORDER-12345" \
  --priority high
```

**pause** - Pause a running process
```bash
workflow-engine process pause \
  --instance-id inst-456 \
  --reason "Pending external review" \
  --notify-assignees
```

**resume** - Resume a paused process
```bash
workflow-engine process resume \
  --instance-id inst-456 \
  --update-variables '{"approved": true}' \
  --skip-to-task task-789
```

**abort** - Abort a process instance
```bash
workflow-engine process abort \
  --instance-id inst-456 \
  --reason "Customer requested cancellation" \
  --cleanup-resources
```

**status** - Get process status
```bash
workflow-engine process status \
  --instance-id inst-456 \
  --include-tasks \
  --include-variables \
  --include-events
```

### Task Noun

Manage workflow tasks and activities.

#### Verbs

**assign** - Assign a task to a user
```bash
workflow-engine task assign \
  --task-id task-789 \
  --assignee user@example.com \
  --priority 80 \
  --due-date 2025-01-15T17:00:00Z
```

**complete** - Complete a task
```bash
workflow-engine task complete \
  --task-id task-789 \
  --variables '{"approved": true, "comments": "Looks good"}' \
  --attachments approval.pdf
```

**delegate** - Delegate a task to another user
```bash
workflow-engine task delegate \
  --task-id task-789 \
  --from user1@example.com \
  --to user2@example.com \
  --reason "Subject matter expert"
```

**escalate** - Escalate an overdue task
```bash
workflow-engine task escalate \
  --task-id task-789 \
  --to manager@example.com \
  --reason "Overdue by 2 days" \
  --priority critical
```

**retry** - Retry a failed service task
```bash
workflow-engine task retry \
  --task-id task-789 \
  --max-attempts 3 \
  --backoff exponential \
  --backoff-multiplier 2
```

### Instance Noun

Track and analyze process instances.

#### Verbs

**list** - List process instances
```bash
workflow-engine instance list \
  --workflow-id wf-001 \
  --state running \
  --limit 50 \
  --sort start-time \
  --format table
```

**show** - Show instance details
```bash
workflow-engine instance show \
  --instance-id inst-456 \
  --include-tasks \
  --include-variables \
  --include-events
```

**trace** - Show execution trace
```bash
workflow-engine instance trace \
  --instance-id inst-456 \
  --format tree \
  --include-timestamps \
  --highlight-critical-path
```

**history** - Show execution history
```bash
workflow-engine instance history \
  --instance-id inst-456 \
  --include-state-changes \
  --include-variable-changes \
  --since "2025-01-01T00:00:00Z"
```

**metrics** - Show performance metrics
```bash
workflow-engine instance metrics \
  --instance-id inst-456 \
  --include-task-durations \
  --include-bottlenecks \
  --percentiles 50,90,99
```

## BPMN 2.0 Support

### Supported Elements

#### Tasks
- **Service Task**: Invoke external services or APIs
- **User Task**: Human workflow tasks with assignments
- **Script Task**: Execute inline scripts
- **Send Task**: Send messages to external systems
- **Receive Task**: Wait for external messages
- **Manual Task**: Manual activities without system interaction
- **Business Rule Task**: Execute business rules

#### Gateways
- **Exclusive Gateway**: XOR decision point
- **Parallel Gateway**: AND concurrent execution
- **Inclusive Gateway**: OR multiple path selection
- **Event-Based Gateway**: Decision based on events

#### Events
- **Start Events**: Timer, message, signal, error
- **End Events**: None, error, terminate, cancel
- **Intermediate Events**: Timer, message, signal, error
- **Boundary Events**: Interrupting and non-interrupting

#### Sub-Processes
- **Embedded Sub-Process**: Nested process flows
- **Event Sub-Process**: Event-triggered sub-processes
- **Transaction Sub-Process**: ACID transaction boundaries

### Example: Approval Process

```xml
<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL">
  <bpmn:process id="approval-process" isExecutable="true">

    <!-- Start Event -->
    <bpmn:startEvent id="start" name="Start">
      <bpmn:outgoing>flow1</bpmn:outgoing>
    </bpmn:startEvent>

    <!-- User Task -->
    <bpmn:userTask id="review-task" name="Review Request">
      <bpmn:incoming>flow1</bpmn:incoming>
      <bpmn:outgoing>flow2</bpmn:outgoing>
    </bpmn:userTask>

    <!-- Exclusive Gateway -->
    <bpmn:exclusiveGateway id="decision" name="Approved?">
      <bpmn:incoming>flow2</bpmn:incoming>
      <bpmn:outgoing>approved-flow</bpmn:outgoing>
      <bpmn:outgoing>rejected-flow</bpmn:outgoing>
    </bpmn:exclusiveGateway>

    <!-- Service Tasks -->
    <bpmn:serviceTask id="notify-approved" name="Notify Approval">
      <bpmn:incoming>approved-flow</bpmn:incoming>
      <bpmn:outgoing>flow3</bpmn:outgoing>
    </bpmn:serviceTask>

    <bpmn:serviceTask id="notify-rejected" name="Notify Rejection">
      <bpmn:incoming>rejected-flow</bpmn:incoming>
      <bpmn:outgoing>flow4</bpmn:outgoing>
    </bpmn:serviceTask>

    <!-- End Events -->
    <bpmn:endEvent id="end-approved" name="Approved">
      <bpmn:incoming>flow3</bpmn:incoming>
    </bpmn:endEvent>

    <bpmn:endEvent id="end-rejected" name="Rejected">
      <bpmn:incoming>flow4</bpmn:incoming>
    </bpmn:endEvent>

    <!-- Sequence Flows -->
    <bpmn:sequenceFlow id="flow1" sourceRef="start" targetRef="review-task"/>
    <bpmn:sequenceFlow id="flow2" sourceRef="review-task" targetRef="decision"/>
    <bpmn:sequenceFlow id="approved-flow" sourceRef="decision" targetRef="notify-approved">
      <bpmn:conditionExpression>${approved == true}</bpmn:conditionExpression>
    </bpmn:sequenceFlow>
    <bpmn:sequenceFlow id="rejected-flow" sourceRef="decision" targetRef="notify-rejected">
      <bpmn:conditionExpression>${approved == false}</bpmn:conditionExpression>
    </bpmn:sequenceFlow>
    <bpmn:sequenceFlow id="flow3" sourceRef="notify-approved" targetRef="end-approved"/>
    <bpmn:sequenceFlow id="flow4" sourceRef="notify-rejected" targetRef="end-rejected"/>

  </bpmn:process>
</bpmn:definitions>
```

### Execute the Workflow

```bash
# Create and deploy
workflow-engine workflow create --file approval-process.bpmn --name "Approval Process"
workflow-engine workflow deploy --workflow-id approval-process

# Start instance
workflow-engine process start \
  --workflow-id approval-process \
  --variables '{"amount": 1000, "requester": "john@example.com"}'

# Complete review task
workflow-engine task complete \
  --task-id review-task-001 \
  --variables '{"approved": true, "comments": "Approved"}'
```

## State Management

### Process States

```
PENDING → RUNNING → COMPLETED
    ↓         ↓
    ↓    SUSPENDED → RUNNING
    ↓         ↓
    └─────→ FAILED
            ↓
          ABORTED
```

### State Persistence

```bash
# Configure state persistence
export WORKFLOW_DB_URL="postgresql://user:pass@localhost/workflows"

# Enable persistence
workflow-engine workflow deploy \
  --workflow-id wf-001 \
  --enable-persistence \
  --snapshot-interval 10s
```

### Recovery

```bash
# List failed instances
workflow-engine instance list --state failed

# Retry failed instance
workflow-engine process resume \
  --instance-id inst-456 \
  --from-last-checkpoint
```

## Event Handling

### Event-Driven Workflows

```bash
# Start process with message event
workflow-engine process start \
  --workflow-id order-process \
  --wait-for-message "payment-received"

# Send message to waiting process
workflow-engine event send \
  --event-name "payment-received" \
  --correlation-key "ORDER-123" \
  --payload '{"amount": 100, "payment_id": "PAY-456"}'

# Timer-based events
workflow-engine event timer \
  --duration "PT1H" \
  --cron "0 0 * * *" \
  --timezone "America/New_York"
```

## Performance Metrics

### Built-in Metrics

```bash
# Instance-level metrics
workflow-engine instance metrics \
  --instance-id inst-456 \
  --include-task-durations \
  --include-bottlenecks

# Workflow-level metrics
workflow-engine workflow metrics \
  --workflow-id wf-001 \
  --time-range "last-7-days" \
  --group-by task
```

### Metric Types

- **Throughput**: Instances completed per time unit
- **Cycle Time**: Average time from start to completion
- **Task Duration**: Time spent in each task
- **Bottlenecks**: Slowest tasks and gateways
- **Error Rate**: Percentage of failed instances
- **Utilization**: Resource utilization rate

### Export Metrics

```bash
# Export to Prometheus
workflow-engine metrics export \
  --format prometheus \
  --endpoint http://localhost:9090

# Export to JSON
workflow-engine metrics export \
  --format json \
  --output metrics.json \
  --time-range "last-30-days"
```

## Advanced Features

### Parallel Execution

```bash
# Create workflow with parallel gateway
workflow-engine workflow create \
  --file parallel-approval.bpmn

# Monitor parallel tasks
workflow-engine instance trace \
  --instance-id inst-456 \
  --highlight-parallel \
  --show-timing
```

### Sub-Processes

```bash
# Create workflow with embedded sub-process
workflow-engine workflow create \
  --file order-fulfillment.bpmn \
  --enable-subprocesses

# Track sub-process execution
workflow-engine instance show \
  --instance-id inst-456 \
  --include-subprocesses \
  --tree-view
```

### Error Handling

```bash
# Configure error handling
workflow-engine workflow deploy \
  --workflow-id wf-001 \
  --error-strategy retry \
  --max-retries 3 \
  --retry-backoff exponential

# Handle boundary events
workflow-engine task configure \
  --task-id service-task-001 \
  --timeout 30s \
  --on-timeout escalate \
  --boundary-event timeout-event
```

## Integration Examples

### REST API Integration

```rust
use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    let engine = WorkflowEngine::new().await?;

    // Create service task
    let task = ServiceTask::builder()
        .id("call-api")
        .name("Call External API")
        .endpoint("https://api.example.com/orders")
        .method(HttpMethod::Post)
        .build()?;

    // Execute
    let result = engine.execute_task(task).await?;
    println!("API Response: {:?}", result);

    Ok(())
}
```

### Database Integration

```rust
use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    let engine = WorkflowEngine::builder()
        .persistence("postgresql://localhost/workflows")
        .build()
        .await?;

    // State is automatically persisted
    let instance = engine.start_process("wf-001", json!({})).await?;

    // Recovery after restart
    let restored = engine.restore_instance(instance.id).await?;

    Ok(())
}
```

### Message Queue Integration

```rust
use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    let engine = WorkflowEngine::builder()
        .event_bus("kafka://localhost:9092")
        .build()
        .await?;

    // Subscribe to events
    engine.subscribe("payment.received", |event| {
        println!("Payment received: {:?}", event);
    }).await?;

    // Publish events
    engine.publish("order.created", json!({
        "order_id": "ORD-123",
        "amount": 100
    })).await?;

    Ok(())
}
```

## Configuration

### Environment Variables

```bash
# Database
export WORKFLOW_DB_URL="postgresql://user:pass@localhost/workflows"

# Event bus
export WORKFLOW_EVENT_BUS="redis://localhost:6379"

# Metrics
export WORKFLOW_METRICS_ENABLED=true
export WORKFLOW_METRICS_PORT=9090

# Logging
export RUST_LOG=workflow_engine=debug
```

### Configuration File

```toml
# workflow-engine.toml

[engine]
max_concurrent_instances = 100
task_timeout = "30s"
enable_persistence = true
snapshot_interval = "10s"

[database]
url = "postgresql://localhost/workflows"
pool_size = 10

[events]
bus_type = "redis"
url = "redis://localhost:6379"

[metrics]
enabled = true
port = 9090
export_format = "prometheus"

[logging]
level = "info"
format = "json"
```

## Testing

### Unit Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_workflow_execution

# Run with logging
RUST_LOG=debug cargo test -- --nocapture
```

### Integration Tests

```bash
# Run integration tests
cargo test --test integration_test

# Test specific workflow
cargo test test_approval_workflow -- --nocapture
```

### Benchmarks

```bash
# Run benchmarks
cargo bench

# Benchmark specific scenario
cargo bench parallel_execution
```

## Troubleshooting

### Common Issues

**Workflow validation fails**
```bash
# Check BPMN syntax
workflow-engine workflow validate \
  --file process.bpmn \
  --verbose \
  --check-references
```

**Instance stuck in running state**
```bash
# Check instance status
workflow-engine instance show \
  --instance-id inst-456 \
  --include-tasks

# Check for blocking tasks
workflow-engine task list \
  --instance-id inst-456 \
  --state pending
```

**High latency**
```bash
# Analyze bottlenecks
workflow-engine instance metrics \
  --instance-id inst-456 \
  --include-bottlenecks

# Check task durations
workflow-engine instance trace \
  --instance-id inst-456 \
  --show-timing
```

## RDF/SPARQL Integration

### Query Workflows

```sparql
PREFIX wfe: <http://ggen.ruv.io/workflow-engine#>

SELECT ?workflow ?name ?version
WHERE {
  ?workflow a wfe:Workflow ;
           wfe:workflowName ?name ;
           wfe:workflowVersion ?version .
  FILTER(?version >= "1.0.0")
}
```

### Query Process Instances

```sparql
PREFIX wfe: <http://ggen.ruv.io/workflow-engine#>

SELECT ?instance ?state ?startTime
WHERE {
  ?instance a wfe:Instance ;
           wfe:instanceState ?state ;
           wfe:startTime ?startTime .
  FILTER(?state = "running")
}
ORDER BY DESC(?startTime)
LIMIT 10
```

## Performance

### Benchmarks

- **Workflow Validation**: ~100 workflows/sec
- **Process Start**: ~1000 instances/sec
- **Task Execution**: ~5000 tasks/sec
- **Event Processing**: ~10000 events/sec

### Optimization

```bash
# Enable parallel execution
workflow-engine workflow deploy \
  --workflow-id wf-001 \
  --parallel-tasks 10

# Configure caching
workflow-engine config set cache.enabled true
workflow-engine config set cache.ttl 3600

# Tune database pool
workflow-engine config set db.pool_size 20
workflow-engine config set db.max_connections 50
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

This project is dual-licensed under MIT OR Apache-2.0. See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.

## Support

- **Documentation**: https://docs.ggen.ruv.io/workflow-engine-cli
- **Issues**: https://github.com/yourusername/workflow-engine-cli/issues
- **Discord**: https://discord.gg/workflow-engine

## Acknowledgments

Built on the clap-noun-verb pattern and BPMN 2.0 specification.

---

**Happy Workflow Automation!** 🚀
