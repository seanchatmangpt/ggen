<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Governance Layer Architecture](#governance-layer-architecture)
  - [Overview](#overview)
  - [Architecture Diagram](#architecture-diagram)
  - [Core Components](#core-components)
    - [1. GovernanceCoordinator](#1-governancecoordinator)
    - [2. PolicyEngine](#2-policyengine)
    - [3. AuditTrail](#3-audittrail)
    - [4. Dashboard](#4-dashboard)
    - [5. SafetyController](#5-safetycontroller)
    - [6. ApprovalWorkflow](#6-approvalworkflow)
  - [Decision Validation Flow](#decision-validation-flow)
  - [Integration with Autonomous System](#integration-with-autonomous-system)
    - [1. Pre-Decision Hook](#1-pre-decision-hook)
    - [2. Graph Evolution Tracking](#2-graph-evolution-tracking)
    - [3. Continuous Monitoring](#3-continuous-monitoring)
  - [Configuration](#configuration)
  - [Usage Examples](#usage-examples)
    - [Example 1: Basic Policy Definition](#example-1-basic-policy-definition)
    - [Example 2: Emergency Stop](#example-2-emergency-stop)
    - [Example 3: Approval Workflow](#example-3-approval-workflow)
    - [Example 4: Metrics Export](#example-4-metrics-export)
  - [Performance Characteristics](#performance-characteristics)
  - [Security Considerations](#security-considerations)
  - [Future Enhancements](#future-enhancements)
  - [Testing Strategy](#testing-strategy)
  - [Deployment](#deployment)
    - [Standalone Mode](#standalone-mode)
    - [Embedded Mode](#embedded-mode)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Governance Layer Architecture

## Overview

The Governance Layer provides comprehensive human oversight capabilities for the autonomous system, enabling the transition to a 10-20% governance role where humans define policies, review critical decisions, and maintain system safety.

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Governance Coordinator                        │
│  Orchestrates all governance components and decision validation  │
└────────┬────────────────────────────────────────────────────────┘
         │
         ├──────────────┬──────────────┬──────────────┬────────────┐
         │              │              │              │            │
┌────────▼────────┐ ┌──▼──────────┐ ┌─▼────────────┐ ┌▼───────────▼──┐
│  Policy Engine  │ │ Audit Trail │ │  Dashboard   │ │ Safety        │
│                 │ │             │ │              │ │ Controller    │
│ - Rule Defn    │ │ - Event Log │ │ - Metrics    │ │ - E-Stop      │
│ - Validation   │ │ - Query API │ │ - Health     │ │ - Rollback    │
│ - Constraints  │ │ - Retention │ │ - Timescale  │ │ - Validation  │
└─────────────────┘ └─────────────┘ └──────────────┘ └───────────────┘
         │                                                    │
         │                                                    │
    ┌────▼─────────────────────────────────────────────────▼─────┐
    │              Approval Workflow Engine                       │
    │  - Request Management  - Multi-Approver Support            │
    │  - Delegation          - Timeout Handling                   │
    └─────────────────────────────────────────────────────────────┘
                              │
                              │
                    ┌─────────▼──────────┐
                    │ Autonomous System  │
                    │   (Decisions)      │
                    └────────────────────┘
```

## Core Components

### 1. GovernanceCoordinator

**Purpose**: Central orchestrator that coordinates all governance components and validates autonomous decisions.

**Key Responsibilities**:
- Decision validation pipeline
- Component coordination
- Emergency stop management
- Health monitoring
- Audit trail integration

**API**:
```rust
pub struct GovernanceCoordinator {
    pub async fn new(config: GovernanceConfig) -> Result<Self>
    pub async fn validate_decision(&self, decision: &Decision) -> Result<DecisionOutcome>
    pub async fn emergency_stop(&self, reason: &str) -> Result<()>
    pub async fn resume_operations(&self, approved_by: &str) -> Result<()>
    pub async fn rollback(&self, target_state: &str) -> Result<()>
    pub async fn get_health_status(&self) -> Result<HealthStatus>
    pub async fn get_metrics(&self) -> Result<MetricsSnapshot>
}
```

### 2. PolicyEngine

**Purpose**: Define and enforce policies that constrain autonomous behavior.

**Key Features**:
- Flexible rule system (rate limits, mutation limits, field matching, allow/block lists)
- Priority-based policy evaluation
- Custom condition extensions
- Violation tracking
- Policy lifecycle management

**Rule Types**:
- `RateLimit`: Control operation frequency
- `MutationLimit`: Limit graph changes
- `FieldMatch`: Pattern-based validation
- `AllowList`: Whitelist validation
- `BlockList`: Blacklist validation
- `Custom`: Extensible custom rules

**API**:
```rust
pub struct PolicyEngine {
    pub async fn register_policy(&self, policy: Policy) -> Result<()>
    pub async fn validate(&self, decision: &Decision) -> Result<bool>
    pub async fn get_violations(&self, decision: &Decision) -> Result<Vec<PolicyViolation>>
}

// Builder pattern for policies
let policy = Policy::builder("graph-mutation-limit")
    .description("Limit graph changes per hour")
    .rule(PolicyRule {
        condition: RuleCondition::MutationLimit { max_mutations: 100 },
        action: RuleAction::RequireApproval,
        severity: Severity::Warning,
    })
    .build()?;
```

### 3. AuditTrail

**Purpose**: Comprehensive logging and querying of all governance decisions and events.

**Key Features**:
- Persistent event storage (SQLite)
- Rich query API with filtering
- Event type categorization
- Retention policy management
- Time-series analysis support

**Event Types**:
- DecisionReceived, DecisionApproved, DecisionRejected
- PolicyViolation, SafetyViolation
- EmergencyStop, EmergencyResume
- Rollback, ApprovalRequested
- PolicyRegistered, ConfigurationChanged

**API**:
```rust
pub struct AuditTrail {
    pub async fn new(db_path: impl AsRef<Path>) -> Result<Self>
    pub async fn log_event(&self, event: AuditEvent) -> Result<()>
    pub async fn query(&self, query: AuditQuery) -> Result<Vec<AuditEvent>>
    pub async fn cleanup_old_events(&self) -> Result<usize>
}

// Query example
let query = AuditQuery {
    event_types: Some(vec![EventType::PolicyViolation]),
    start_time: Some(Utc::now() - Duration::days(7)),
    severity: Some(AuditSeverity::Critical),
    ..Default::default()
};
let violations = audit_trail.query(query).await?;
```

### 4. Dashboard

**Purpose**: Real-time observability and metrics visualization.

**Key Features**:
- Health status monitoring
- Performance metrics (decisions/sec, approval rates)
- Timescale metrics for trend analysis
- Resource usage tracking
- Multiple export formats (JSON, Prometheus, CSV)

**Metrics Tracked**:
- Decisions processed/approved/rejected/pending
- Policy and safety violations
- Emergency stops and rollbacks
- Average processing time
- Approval and rejection rates
- Resource usage (CPU, memory, disk, network)

**API**:
```rust
pub struct Dashboard {
    pub async fn get_health_status(&self) -> Result<HealthStatus>
    pub async fn get_metrics_snapshot(&self) -> Result<MetricsSnapshot>
    pub async fn record_decision(&self, approved: bool, processing_time_ms: f64) -> Result<()>
    pub async fn export_metrics(&self, format: ExportFormat) -> Result<String>
}
```

### 5. SafetyController

**Purpose**: Emergency stop, rollback, and validation mechanisms.

**Key Features**:
- Emergency stop capability
- State snapshot and rollback
- Validation gates for critical operations
- Dangerous operation blocking
- Safety violation tracking

**Safety Mechanisms**:
- **Emergency Stop**: Immediate halt of all autonomous operations
- **State Snapshots**: Point-in-time system state capture
- **Rollback**: Restore to previous known-good state
- **Validation Gates**: Multi-approval for critical operations
- **Operation Blocking**: Prevent dangerous operations

**API**:
```rust
pub struct SafetyController {
    pub async fn check_safety(&self, decision: &Decision) -> Result<Option<String>>
    pub async fn trigger_emergency_stop(&self, reason: &str) -> Result<()>
    pub async fn resume(&self) -> Result<()>
    pub async fn create_snapshot(&self, description: &str, data: Value) -> Result<String>
    pub async fn rollback(&self, target_snapshot_id: &str) -> Result<()>
    pub async fn create_validation_gate(&self, operation: &str, required: usize) -> Result<String>
}
```

### 6. ApprovalWorkflow

**Purpose**: Human-in-the-loop approval system for critical decisions.

**Key Features**:
- Multi-approver support
- Delegation capabilities
- Timeout handling
- Approval history tracking
- Flexible approval requirements

**Workflow States**:
- `Pending`: Awaiting approval
- `Approved`: All required approvals received
- `Rejected`: Any approver rejected
- `Expired`: Timeout reached
- `Withdrawn`: Requester withdrew

**API**:
```rust
pub struct ApprovalWorkflow {
    pub async fn submit(&self, request: ApprovalRequest) -> Result<String>
    pub async fn respond(&self, request_id: &str, approver: &str,
                         decision: ResponseDecision, comments: Option<String>) -> Result<ApprovalStatus>
    pub async fn delegate(&self, request_id: &str, from: &str, to: &str) -> Result<()>
    pub async fn withdraw(&self, request_id: &str, withdrawn_by: &str) -> Result<()>
}
```

## Decision Validation Flow

```
┌─────────────────────┐
│ Autonomous Decision │
└──────────┬──────────┘
           │
           ▼
┌──────────────────────┐
│  Safety Check        │  ◄── Emergency Stop Active?
│  - E-Stop Status     │  ◄── Dangerous Operation?
│  - Dangerous Ops     │  ◄── Validation Gate?
└──────────┬───────────┘
           │ Safe
           ▼
┌──────────────────────┐
│  Policy Validation   │  ◄── Rate Limits
│  - Evaluate Rules    │  ◄── Mutation Limits
│  - Check Constraints │  ◄── Field Patterns
└──────────┬───────────┘
           │
           ├─ Violation ──► Log & Reject
           │
           ├─ Requires Approval ──► Submit to Workflow
           │                              │
           │                              ▼
           │                        ┌──────────────┐
           │                        │  Human       │
           │                        │  Approval    │
           │                        └──────┬───────┘
           │                               │
           ▼                               ▼
    ┌──────────────┐              ┌──────────────┐
    │ Auto-Approve │              │ Approved or  │
    │              │              │ Rejected     │
    └──────────────┘              └──────────────┘
```

## Integration with Autonomous System

### 1. Pre-Decision Hook

```rust
// Before making autonomous decision
let decision = Decision::new_high_risk(
    "modify_graph_schema",
    "Add new entity type to ontology"
);

// Validate through governance
let outcome = governance.validate_decision(&decision).await?;

match outcome {
    DecisionOutcome::Approved { .. } => {
        // Proceed with action
        autonomous_system.execute(decision).await?;
    }
    DecisionOutcome::Rejected { reason, .. } => {
        // Log and skip
        log::warn!("Decision rejected: {}", reason);
    }
    DecisionOutcome::PendingApproval { request_id, .. } => {
        // Wait for human approval
        log::info!("Awaiting approval: {}", request_id);
    }
}
```

### 2. Graph Evolution Tracking

```rust
// After graph mutation
governance.audit_trail.log_event(AuditEvent {
    event_type: EventType::GraphEvolution,
    details: json!({
        "before": graph_snapshot_before,
        "after": graph_snapshot_after,
        "changes": delta_changes,
    }),
    ..
}).await?;

// Create snapshot for rollback
governance.safety_controller
    .create_snapshot("post-evolution", graph_state)
    .await?;
```

### 3. Continuous Monitoring

```rust
// Background task for health monitoring
tokio::spawn(async move {
    loop {
        let health = governance.get_health_status().await?;

        if health.overall_status == SystemStatus::Critical {
            // Alert operators
            notify_operators(&health).await?;
        }

        tokio::time::sleep(Duration::from_secs(30)).await;
    }
});
```

## Configuration

```rust
let config = GovernanceConfig {
    audit_db_path: ".governance/audit.db".to_string(),

    policy_config: PolicyConfig {
        strict_mode: true,
        log_all_evaluations: true,
        default_action: RuleAction::RequireApproval,
    },

    safety_config: SafetyConfig {
        enable_emergency_stop: true,
        enable_auto_rollback: true,
        max_rollback_depth: 10,
        dangerous_operations: vec![
            "delete_all".to_string(),
            "modify_schema".to_string(),
        ],
    },

    workflow_config: WorkflowConfig {
        default_approvers: vec!["admin".to_string()],
        require_multiple_approvals: true,
        min_approvals_required: 2,
        approval_timeout_minutes: 60,
    },

    dashboard_config: DashboardConfig {
        refresh_interval_seconds: 5,
        metrics_retention_hours: 168, // 7 days
        enable_real_time_updates: true,
    },

    require_approval_for_all: false,
    enable_auto_policies: true,
};

let governance = GovernanceCoordinator::new(config).await?;
```

## Usage Examples

### Example 1: Basic Policy Definition

```rust
// Create a policy to limit graph mutations
let policy = Policy::builder("hourly-mutation-limit")
    .description("Limit graph mutations to 100 per hour")
    .rule(PolicyRule {
        id: Uuid::new_v4().to_string(),
        condition: RuleCondition::RateLimit {
            window_seconds: 3600,
            max_operations: 100,
        },
        action: RuleAction::Reject,
        severity: Severity::Error,
    })
    .priority(10)
    .build()?;

governance.policy_engine.register_policy(policy).await?;
```

### Example 2: Emergency Stop

```rust
// Detect critical issue
if detected_critical_issue {
    governance.emergency_stop("Data integrity violation detected").await?;

    // System is now halted
    // Only human can resume
}

// Human reviews and approves resume
governance.resume_operations("admin@example.com").await?;
```

### Example 3: Approval Workflow

```rust
// High-risk decision requires approval
let decision = Decision::new_high_risk(
    "drop_legacy_schema",
    "Remove deprecated schema version"
);

let outcome = governance.validate_decision(&decision).await?;

if let DecisionOutcome::PendingApproval { request_id, .. } = outcome {
    // Human approver reviews via dashboard
    governance.workflow
        .respond(&request_id, "admin", ResponseDecision::Approve,
                Some("Reviewed migration plan".to_string()))
        .await?;
}
```

### Example 4: Metrics Export

```rust
// Export metrics to Prometheus
let prometheus_metrics = governance.dashboard
    .export_metrics(ExportFormat::Prometheus)
    .await?;

// Expose on /metrics endpoint
serve_metrics(prometheus_metrics);
```

## Performance Characteristics

- **Decision Validation**: < 10ms for policy evaluation
- **Audit Logging**: Async, non-blocking writes
- **Metrics Collection**: In-memory with periodic persistence
- **Query Performance**: Indexed SQLite queries < 100ms
- **Rollback Time**: < 5s for typical snapshots

## Security Considerations

1. **Access Control**: All governance operations require authentication
2. **Audit Integrity**: Tamper-evident audit logs with checksums
3. **Secrets Management**: No sensitive data in policy definitions
4. **Rate Limiting**: Prevent abuse of governance APIs
5. **Encryption**: Audit database encrypted at rest

## Future Enhancements

1. **Machine Learning Integration**: Predictive policy recommendations
2. **Advanced Visualization**: Graph evolution visualization
3. **External Integrations**: Slack/PagerDuty for alerts
4. **Compliance Templates**: Pre-built GDPR/HIPAA/SOC2 policies
5. **A/B Testing**: Safe rollout of policy changes
6. **Auto-Scaling**: Dynamic policy adjustment based on load

## Testing Strategy

- **Unit Tests**: Each component has comprehensive unit tests
- **Integration Tests**: Full decision validation flow
- **Load Tests**: Handle 1000+ decisions/sec
- **Chaos Tests**: Emergency stop under high load
- **Compliance Tests**: Validate audit trail completeness

## Deployment

### Standalone Mode
```bash
cargo build --release --package ggen-ai
./target/release/ggen-ai governance serve --config governance.toml
```

### Embedded Mode
```rust
use ggen_ai::governance::{GovernanceCoordinator, GovernanceConfig};

let governance = GovernanceCoordinator::new(config).await?;
// Integrate with your autonomous system
```

## Conclusion

The Governance Layer provides a comprehensive framework for human oversight of autonomous systems, enabling safe, auditable, and policy-compliant operations while maintaining the efficiency benefits of automation.
