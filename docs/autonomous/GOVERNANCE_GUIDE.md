<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Autonomous System - Governance Guide](#ggen-autonomous-system---governance-guide)
  - [Table of Contents](#table-of-contents)
  - [Policy Configuration](#policy-configuration)
    - [Policy Types](#policy-types)
    - [Configuration File Format](#configuration-file-format)
    - [Loading Policies](#loading-policies)
    - [Policy Validation](#policy-validation)
    - [Dynamic Policy Updates](#dynamic-policy-updates)
  - [Approval Workflows](#approval-workflows)
    - [Approval Types](#approval-types)
    - [Approval Configuration](#approval-configuration)
    - [Requesting Approval](#requesting-approval)
    - [Providing Approval](#providing-approval)
    - [Approval Webhooks](#approval-webhooks)
  - [Audit Trail Usage](#audit-trail-usage)
    - [Audit Log Schema](#audit-log-schema)
    - [Audit Configuration](#audit-configuration)
    - [Writing Audit Logs](#writing-audit-logs)
    - [Querying Audit Logs](#querying-audit-logs)
    - [Audit Visualization](#audit-visualization)
  - [Security Best Practices](#security-best-practices)
    - [1. Secret Management](#1-secret-management)
    - [2. Input Validation](#2-input-validation)
    - [3. Least Privilege Principle](#3-least-privilege-principle)
    - [4. Network Security](#4-network-security)
    - [5. Byzantine Fault Tolerance](#5-byzantine-fault-tolerance)
    - [6. Secure Defaults](#6-secure-defaults)
    - [7. Regular Security Audits](#7-regular-security-audits)
    - [8. Incident Response](#8-incident-response)
    - [9. Compliance Standards](#9-compliance-standards)
    - [10. Security Monitoring](#10-security-monitoring)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Autonomous System - Governance Guide

## Table of Contents
1. [Policy Configuration](#policy-configuration)
2. [Approval Workflows](#approval-workflows)
3. [Audit Trail Usage](#audit-trail-usage)
4. [Security Best Practices](#security-best-practices)

## Policy Configuration

The autonomous system uses policy-based governance to control agent behavior, resource usage, and operational boundaries.

### Policy Types

1. **Security Policies**: Input validation, secret detection, access control
2. **Quality Policies**: Code quality thresholds, validation requirements
3. **Resource Policies**: CPU/memory limits, timeout thresholds
4. **Operational Policies**: Approval requirements, audit logging

### Configuration File Format

Policies are defined in TOML format:

```toml
# config/governance.toml

[policies]
version = "1.0"
enforcement_mode = "strict"  # strict, permissive, audit-only

[policies.security]
enabled = true
input_validation = true
secret_scanning = true
path_traversal_protection = true
command_injection_protection = true

[policies.security.allowed_commands]
# Whitelist of allowed shell commands
commands = ["git", "cargo", "npm", "ggen"]
require_approval = ["rm", "mv", "chmod"]

[policies.security.secret_patterns]
# Patterns to detect secrets
patterns = [
    "(?i)(api[_-]?key|apikey)\\s*[:=]\\s*['\"]?([a-zA-Z0-9_-]{20,})['\"]?",
    "(?i)(password|passwd|pwd)\\s*[:=]\\s*['\"]?([^'\"\\s]{8,})['\"]?",
    "-----BEGIN (RSA |DSA )?PRIVATE KEY-----"
]
action = "block"  # block, warn, log

[policies.quality]
enabled = true
min_validation_score = 0.80
max_iterations = 5
require_tests = true
min_test_coverage = 0.70

[policies.quality.code_standards]
max_function_lines = 50
max_file_lines = 500
max_cyclomatic_complexity = 10
require_documentation = true

[policies.resource]
enabled = true
max_cpu_percent = 80
max_memory_mb = 4096
max_execution_time_seconds = 300
max_concurrent_agents = 12

[policies.resource.rate_limits]
# Per-provider rate limits
anthropic_requests_per_minute = 50
openai_requests_per_minute = 60
ollama_requests_per_minute = 0  # unlimited for local

[policies.operational]
enabled = true
require_approval_for_destructive = true
require_approval_for_external_calls = false
auto_approve_read_only = true
audit_all_operations = true

[policies.operational.auto_approval]
# Operations that don't require approval
operations = [
    "read_file",
    "list_files",
    "execute_sparql",
    "validate_template",
    "generate_graph"
]

[policies.operational.require_approval]
# Operations requiring manual approval
operations = [
    "delete_file",
    "publish_template",
    "execute_shell",
    "modify_graph"
]
```

### Loading Policies

```rust
use ggen_ai::PolicyEngine;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load from file
    let engine = PolicyEngine::from_file(
        Path::new("config/governance.toml")
    )?;

    // Or use environment variables
    let engine = PolicyEngine::from_env()?;

    // Or use defaults
    let engine = PolicyEngine::default();

    Ok(())
}
```

### Policy Validation

```rust
use ggen_ai::{PolicyEngine, Action, ActionType};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let engine = PolicyEngine::from_file("config/governance.toml")?;

    let action = Action {
        action_type: ActionType::WriteFile,
        target: "/important/file.rs".to_string(),
        content: Some("// code".to_string()),
        requester: "template_executor".to_string(),
    };

    match engine.evaluate(&action).await? {
        PolicyResult::Allowed => {
            println!("Action allowed");
        }
        PolicyResult::RequiresApproval(reason) => {
            println!("Approval required: {}", reason);
        }
        PolicyResult::Denied(reason) => {
            println!("Action denied: {}", reason);
        }
    }

    Ok(())
}
```

### Dynamic Policy Updates

```rust
use ggen_ai::{PolicyEngine, PolicyUpdate};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut engine = PolicyEngine::from_file("config/governance.toml")?;

    // Update specific policy
    let update = PolicyUpdate {
        path: "policies.quality.min_validation_score".to_string(),
        value: serde_json::json!(0.85),
    };

    engine.update(update).await?;

    // Hot reload from file
    engine.reload().await?;

    Ok(())
}
```

## Approval Workflows

The system implements flexible approval workflows for sensitive operations.

### Approval Types

1. **Automatic Approval**: Pre-approved operations (read-only)
2. **Single Approval**: One approver required
3. **Multi-Approval**: Multiple approvers required (Byzantine consensus)
4. **Time-Based Auto-Approval**: Auto-approve after timeout

### Approval Configuration

```toml
# config/approvals.toml

[approvals]
enabled = true
default_timeout_seconds = 300  # 5 minutes

[approvals.strategies]
destructive_operations = "multi-approval"
external_calls = "single-approval"
high_risk_operations = "multi-approval"
read_only_operations = "auto-approval"

[approvals.multi-approval]
min_approvers = 3
quorum_percentage = 0.67  # 67% must approve
byzantine_tolerance = true

[approvals.approvers]
# Approver definitions
[[approvers.user]]
id = "admin"
role = "administrator"
can_approve = ["*"]

[[approvers.user]]
id = "developer"
role = "developer"
can_approve = ["template_generation", "graph_operations"]

[[approvers.agent]]
id = "security_agent"
role = "security_validator"
can_approve = ["security_sensitive"]
auto_approve = true  # Agent provides automatic approval
```

### Requesting Approval

```rust
use ggen_mcp::{ApprovalManager, ApprovalRequest, Operation};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let manager = ApprovalManager::from_config("config/approvals.toml")?;

    let request = ApprovalRequest {
        operation: Operation::PublishTemplate {
            template_id: "rest-api-auth".to_string(),
        },
        requester: "template_executor".to_string(),
        justification: "Publishing validated template to marketplace".to_string(),
        metadata: serde_json::json!({
            "template_score": 0.92,
            "validation_passes": 3
        }),
    };

    let approval_id = manager.request_approval(request).await?;
    println!("Approval request created: {}", approval_id);

    // Wait for approval (with timeout)
    let result = manager.wait_for_approval(approval_id, Duration::from_secs(300)).await?;

    match result {
        ApprovalResult::Approved { approvers } => {
            println!("Approved by: {:?}", approvers);
        }
        ApprovalResult::Rejected { reason } => {
            println!("Rejected: {}", reason);
        }
        ApprovalResult::Timeout => {
            println!("Approval timed out");
        }
    }

    Ok(())
}
```

### Providing Approval

```bash
# CLI approval
ggen-mcp approvals list --pending
ggen-mcp approvals show <approval-id>
ggen-mcp approvals approve <approval-id> --comment "LGTM"
ggen-mcp approvals reject <approval-id> --reason "Security concerns"
```

**Programmatic Approval:**

```rust
use ggen_mcp::{ApprovalManager, ApprovalDecision};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let manager = ApprovalManager::from_config("config/approvals.toml")?;

    let decision = ApprovalDecision {
        approval_id: "req-123".to_string(),
        approver: "admin".to_string(),
        approved: true,
        comment: Some("Verified security checks passed".to_string()),
    };

    manager.provide_approval(decision).await?;

    Ok(())
}
```

### Approval Webhooks

Configure webhooks to notify external systems:

```toml
[approvals.webhooks]
enabled = true

[[approvals.webhooks.endpoint]]
url = "https://slack.com/api/webhooks/..."
events = ["approval_required", "approval_granted", "approval_rejected"]
headers = { "Content-Type" = "application/json" }

[[approvals.webhooks.endpoint]]
url = "https://internal.company.com/approvals"
events = ["approval_required"]
auth_token = "${APPROVAL_WEBHOOK_TOKEN}"
```

## Audit Trail Usage

Comprehensive audit logging tracks all system operations.

### Audit Log Schema

```rust
pub struct AuditEntry {
    pub id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub event_type: EventType,
    pub actor: Actor,
    pub operation: Operation,
    pub target: Option<String>,
    pub result: OperationResult,
    pub metadata: serde_json::Value,
    pub correlation_id: Option<Uuid>,
}

pub enum EventType {
    TemplateGeneration,
    GraphOperation,
    ApprovalRequest,
    ApprovalDecision,
    PolicyViolation,
    SecurityIncident,
    AgentAction,
    SystemEvent,
}

pub struct Actor {
    pub actor_type: ActorType,  // User, Agent, System
    pub id: String,
    pub name: String,
    pub ip_address: Option<String>,
}
```

### Audit Configuration

```toml
# config/audit.toml

[audit]
enabled = true
retention_days = 90
compression_enabled = true

[audit.storage]
backend = "sqlite"  # sqlite, postgres, s3
path = ".ggen/audit.db"

# For PostgreSQL
# connection_string = "postgresql://user:pass@localhost/ggen_audit"

[audit.filters]
# Events to always log
always_log = [
    "policy_violation",
    "security_incident",
    "approval_decision",
    "destructive_operation"
]

# Events to never log (sensitive)
never_log = []

# Sample rate for high-volume events (0.0 - 1.0)
sampling_rates = { "template_validation" = 0.1, "cache_hit" = 0.01 }

[audit.export]
enabled = true
format = "json"  # json, csv, parquet
interval_hours = 24
destination = ".ggen/audit-exports/"
```

### Writing Audit Logs

```rust
use ggen_mcp::{AuditLogger, AuditEntry, EventType, Actor, Operation};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let logger = AuditLogger::from_config("config/audit.toml")?;

    let entry = AuditEntry {
        id: Uuid::new_v4(),
        timestamp: Utc::now(),
        event_type: EventType::TemplateGeneration,
        actor: Actor {
            actor_type: ActorType::Agent,
            id: "template_executor_1".to_string(),
            name: "Template Executor".to_string(),
            ip_address: None,
        },
        operation: Operation::GenerateTemplate {
            description: "REST API endpoint".to_string(),
            format: "rust".to_string(),
        },
        target: Some("templates/api_endpoint.rs".to_string()),
        result: OperationResult::Success,
        metadata: serde_json::json!({
            "validation_score": 0.92,
            "iterations": 2,
            "tokens_used": 1247
        }),
        correlation_id: Some(Uuid::parse_str("...")?),
    };

    logger.log(entry).await?;

    Ok(())
}
```

### Querying Audit Logs

```rust
use ggen_mcp::{AuditLogger, AuditQuery};
use chrono::{Duration, Utc};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let logger = AuditLogger::from_config("config/audit.toml")?;

    // Query by time range
    let query = AuditQuery::new()
        .from(Utc::now() - Duration::hours(24))
        .to(Utc::now())
        .event_type(EventType::PolicyViolation)
        .limit(100);

    let entries = logger.query(query).await?;

    for entry in entries {
        println!("{}: {} by {}", entry.timestamp, entry.event_type, entry.actor.name);
    }

    // Query by correlation ID (track related operations)
    let related = logger.query_by_correlation_id(correlation_id).await?;

    Ok(())
}
```

**CLI Queries:**

```bash
# View recent audit log
ggen-mcp audit tail --lines 100

# Filter by event type
ggen-mcp audit query --event-type policy_violation --last 24h

# Export audit log
ggen-mcp audit export --format json --output audit-2024-10.json

# Statistics
ggen-mcp audit stats --group-by event_type --last 7d
```

### Audit Visualization

Generate audit reports:

```bash
# Generate HTML report
ggen-mcp audit report \
  --from "2024-10-01" \
  --to "2024-10-31" \
  --output reports/october-audit.html

# Generate compliance report
ggen-mcp audit compliance \
  --standard "ISO27001" \
  --output compliance-report.pdf
```

## Security Best Practices

### 1. Secret Management

**Never hardcode secrets:**

```rust
// ❌ BAD - Hardcoded API key
let api_key = "sk-1234567890abcdef";

// ✅ GOOD - Environment variable
let api_key = std::env::var("ANTHROPIC_API_KEY")?;

// ✅ BETTER - Secret management system
let api_key = SecretManager::get("anthropic_api_key").await?;
```

**Use secret scanning:**

```toml
[policies.security]
secret_scanning = true
action = "block"  # Prevent commits with secrets

[policies.security.secret_patterns]
patterns = [
    "(?i)(api[_-]?key|apikey)\\s*[:=]\\s*['\"]?([a-zA-Z0-9_-]{20,})['\"]?",
    "(?i)sk-[a-zA-Z0-9]{48}",  # Anthropic key pattern
    "(?i)sk-proj-[a-zA-Z0-9]{48}",  # OpenAI key pattern
]
```

### 2. Input Validation

**Always validate user input:**

```rust
use ggen_ai::SecurityAgent;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let security = SecurityAgent::new(agent_config);

    // Validate path for path traversal
    let path = "../../../etc/passwd";
    match security.validate_path(path).await? {
        ValidationResult::Safe => { /* proceed */ }
        ValidationResult::Unsafe(reason) => {
            return Err(format!("Unsafe path: {}", reason).into());
        }
    }

    // Validate command for injection
    let command = "ls; rm -rf /";
    match security.validate_command(command).await? {
        ValidationResult::Safe => { /* proceed */ }
        ValidationResult::Unsafe(reason) => {
            return Err(format!("Unsafe command: {}", reason).into());
        }
    }

    Ok(())
}
```

### 3. Least Privilege Principle

**Configure minimal permissions:**

```toml
[policies.agents.template_executor]
can_read = ["templates/*", "config/*"]
can_write = ["templates/*", ".ggen/cache/*"]
cannot_write = ["/etc/*", "~/.ssh/*", "/usr/*"]

[policies.agents.graph_monitor]
can_read = ["graphs/*", "ontologies/*"]
can_write = ["graphs/*"]  # No write outside graphs
can_execute = []  # No shell execution

[policies.agents.security_agent]
can_read = ["*"]  # Read all for scanning
can_write = [".ggen/audit/*"]
can_execute = []  # No execution privileges
```

### 4. Network Security

**Restrict external communications:**

```toml
[policies.network]
allow_outbound = true
allowed_domains = [
    "api.anthropic.com",
    "api.openai.com",
    "localhost",
    "127.0.0.1"
]

blocked_domains = [
    "*.internal.company.com",  # No access to internal
]

require_tls = true
min_tls_version = "1.2"
```

### 5. Byzantine Fault Tolerance

**Require consensus for critical operations:**

```rust
use ggen_agents::ByzantineValidator;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let validator = ByzantineValidator::new(
        agent_config,
        7,  // 2f+1 = 7 (tolerates 3 faults)
        3,  // f = 3
    );

    // Require consensus for critical operations
    let operation = Operation::PublishTemplate { /* ... */ };

    let result = validator.validate_operation(operation).await?;

    if !result.consensus_reached() {
        return Err("Failed to reach consensus".into());
    }

    Ok(())
}
```

### 6. Secure Defaults

**Use secure configuration defaults:**

```rust
impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            input_validation: true,
            secret_scanning: true,
            path_traversal_protection: true,
            command_injection_protection: true,
            require_tls: true,
            audit_logging: true,
            rate_limiting: true,
            max_request_size_mb: 10,
            session_timeout_minutes: 30,
        }
    }
}
```

### 7. Regular Security Audits

**Schedule automated security scans:**

```bash
# Daily security scan
ggen-mcp security scan --full

# Generate security report
ggen-mcp security report \
  --output security-report.pdf \
  --include-recommendations

# Check for vulnerabilities in dependencies
cargo audit

# Update dependencies
cargo update
```

### 8. Incident Response

**Configure incident response:**

```toml
[security.incident_response]
enabled = true

[security.incident_response.actions]
on_policy_violation = "log_and_block"
on_security_incident = "log_block_and_alert"
on_byzantine_fault = "log_and_isolate"

[security.incident_response.alerts]
email = ["security@company.com"]
slack_webhook = "${SECURITY_SLACK_WEBHOOK}"
pagerduty_key = "${PAGERDUTY_KEY}"

[security.incident_response.auto_remediation]
enabled = true
isolate_faulty_agents = true
rollback_on_failure = true
escalate_after_attempts = 3
```

### 9. Compliance Standards

**ISO 27001 Compliance:**

```toml
[compliance.iso27001]
enabled = true
audit_retention_days = 365
require_encryption_at_rest = true
require_encryption_in_transit = true
require_access_logs = true
require_change_management = true

[compliance.iso27001.controls]
A_9_1_2_access_to_networks = true  # Network access control
A_12_4_1_event_logging = true      # Event logging
A_12_4_3_administrator_logs = true  # Administrator logs
A_14_2_2_system_change_control = true  # Change control
```

**SOC 2 Compliance:**

```toml
[compliance.soc2]
enabled = true

[compliance.soc2.trust_principles]
security = true
availability = true
processing_integrity = true
confidentiality = true
privacy = true

[compliance.soc2.controls]
CC6_1_logical_access = true
CC7_2_system_monitoring = true
CC8_1_change_management = true
```

### 10. Security Monitoring

**Real-time security monitoring:**

```rust
use ggen_mcp::{SecurityMonitor, SecurityEvent};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let monitor = SecurityMonitor::new(security_config);

    // Subscribe to security events
    let mut events = monitor.subscribe().await?;

    while let Some(event) = events.recv().await {
        match event {
            SecurityEvent::PolicyViolation { actor, policy, .. } => {
                eprintln!("POLICY VIOLATION: {} violated {}", actor, policy);
                monitor.alert_security_team(&event).await?;
            }
            SecurityEvent::ByzantineFault { agent_id, .. } => {
                eprintln!("BYZANTINE FAULT: Agent {} detected", agent_id);
                monitor.isolate_agent(agent_id).await?;
            }
            SecurityEvent::UnauthorizedAccess { actor, resource, .. } => {
                eprintln!("UNAUTHORIZED ACCESS: {} attempted {}", actor, resource);
                monitor.block_actor(&actor).await?;
            }
            _ => {}
        }
    }

    Ok(())
}
```

---

## Summary

The governance system provides:

- **Flexible Policies**: Fine-grained control over system behavior
- **Approval Workflows**: Multi-level approvals with Byzantine consensus
- **Comprehensive Auditing**: Full audit trail with compliance reports
- **Security Best Practices**: Defense-in-depth security architecture

For production deployments, always:
1. Enable strict policy enforcement
2. Require multi-approval for critical operations
3. Enable comprehensive audit logging
4. Configure incident response
5. Regular security audits
6. Keep dependencies updated
7. Monitor security events in real-time
8. Follow principle of least privilege
