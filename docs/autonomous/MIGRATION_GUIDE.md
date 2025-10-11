<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Autonomous System - Migration Guide](#ggen-autonomous-system---migration-guide)
  - [Table of Contents](#table-of-contents)
  - [Migrating from Manual Workflows](#migrating-from-manual-workflows)
    - [Phase 1: Assessment (Week 1)](#phase-1-assessment-week-1)
      - [Inventory Current Workflows](#inventory-current-workflows)
      - [Measure Baseline Metrics](#measure-baseline-metrics)
    - [Phase 2: Pilot (Weeks 2-4)](#phase-2-pilot-weeks-2-4)
      - [Start with Low-Risk Operations](#start-with-low-risk-operations)
      - [Parallel Running](#parallel-running)
      - [Track Pilot Metrics](#track-pilot-metrics)
    - [Phase 3: Gradual Rollout (Weeks 5-8)](#phase-3-gradual-rollout-weeks-5-8)
      - [Enable Agent by Agent](#enable-agent-by-agent)
      - [Migration Checklist](#migration-checklist)
    - [Phase 4: Full Production (Week 9+)](#phase-4-full-production-week-9)
      - [Production Configuration](#production-configuration)
      - [Cutover Process](#cutover-process)
    - [Phase 5: Optimization (Ongoing)](#phase-5-optimization-ongoing)
      - [Continuous Improvement](#continuous-improvement)
  - [Graph Evolution Strategies](#graph-evolution-strategies)
    - [Understanding Graph Versions](#understanding-graph-versions)
    - [Evolution Strategies](#evolution-strategies)
      - [Strategy 1: Additive Evolution (Recommended)](#strategy-1-additive-evolution-recommended)
      - [Strategy 2: Versioned Migration](#strategy-2-versioned-migration)
      - [Strategy 3: Parallel Versions](#strategy-3-parallel-versions)
    - [Backward Compatibility Checks](#backward-compatibility-checks)
    - [Automated Evolution Testing](#automated-evolution-testing)
  - [Rollback Procedures](#rollback-procedures)
    - [Snapshot-Based Rollback](#snapshot-based-rollback)
      - [Creating Snapshots](#creating-snapshots)
      - [Restoring Snapshots](#restoring-snapshots)
    - [Operation-Level Rollback](#operation-level-rollback)
      - [Individual Operation Rollback](#individual-operation-rollback)
    - [Agent-Level Rollback](#agent-level-rollback)
    - [Full System Rollback](#full-system-rollback)
    - [Gradual Rollback](#gradual-rollback)
  - [Performance Tuning](#performance-tuning)
    - [Identifying Bottlenecks](#identifying-bottlenecks)
      - [System-Wide Analysis](#system-wide-analysis)
      - [Agent-Level Profiling](#agent-level-profiling)
    - [Cache Optimization](#cache-optimization)
      - [Cache Configuration](#cache-configuration)
      - [Cache Tuning](#cache-tuning)
    - [AI Provider Optimization](#ai-provider-optimization)
      - [Provider Selection](#provider-selection)
      - [Intelligent Provider Routing](#intelligent-provider-routing)
    - [Concurrent Operation Tuning](#concurrent-operation-tuning)
    - [Resource Allocation](#resource-allocation)
    - [Database Optimization](#database-optimization)
  - [Migration Success Criteria](#migration-success-criteria)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Autonomous System - Migration Guide

## Table of Contents
1. [Migrating from Manual Workflows](#migrating-from-manual-workflows)
2. [Graph Evolution Strategies](#graph-evolution-strategies)
3. [Rollback Procedures](#rollback-procedures)
4. [Performance Tuning](#performance-tuning)

## Migrating from Manual Workflows

### Phase 1: Assessment (Week 1)

#### Inventory Current Workflows

Document your existing manual processes:

```bash
# Create workflow inventory
cat > workflows-inventory.md <<EOF
# Current Workflows

## Template Generation
- Frequency: Daily
- Time per generation: 2-4 hours
- Quality issues: 30% require rework
- Manual steps: 15

## Code Review
- Frequency: Per PR
- Time per review: 1-2 hours
- Coverage: Variable
- Manual steps: 8

## Testing
- Frequency: Before release
- Time per test cycle: 4-6 hours
- Coverage: 65%
- Manual steps: 20
EOF
```

#### Measure Baseline Metrics

```bash
# Track current performance
ggen-mcp metrics baseline \
  --output metrics/baseline.json \
  --metrics template_generation_time,code_review_time,test_coverage

# Example baseline
{
  "template_generation_time_avg": 10800,  // 3 hours
  "code_review_time_avg": 5400,           // 1.5 hours
  "test_coverage": 0.65,
  "manual_interventions_per_week": 45
}
```

### Phase 2: Pilot (Weeks 2-4)

#### Start with Low-Risk Operations

Begin with read-only and generation tasks:

```bash
# Enable autonomous generation only
cat > config/pilot.toml <<EOF
[policies]
enforcement_mode = "permissive"  # Log but don't block

[policies.operational]
auto_approve_read_only = true
require_approval_for_destructive = true

[agents]
enabled_agents = [
  "template_executor",     # Safe: generates templates
  "graph_monitor",         # Safe: read-only monitoring
  "validator",             # Safe: validation only
]

disabled_agents = [
  "recovery_agent",        # Hold: can modify state
  "consensus_manager",     # Hold: distributed operations
]
EOF

# Start pilot deployment
ggen-mcp --config config/pilot.toml
```

#### Parallel Running

Run autonomous system alongside manual process:

```bash
# Manual generation
manual_output=$(ggen gen --template api-endpoint --output manual/)

# Autonomous generation
autonomous_output=$(ggen ai generate \
  --description "Same API endpoint" \
  --validate \
  --output autonomous/)

# Compare results
diff manual/api.rs autonomous/api.rs
ggen ai validate --file autonomous/api.rs
```

#### Track Pilot Metrics

```bash
# Monitor pilot performance
ggen-mcp metrics compare \
  --baseline metrics/baseline.json \
  --current metrics/pilot-week-2.json

# Example pilot metrics
{
  "template_generation_time_avg": 2100,  // 35 minutes (80% improvement)
  "quality_score_avg": 0.89,
  "validation_failures": 12,
  "manual_interventions": 8
}
```

### Phase 3: Gradual Rollout (Weeks 5-8)

#### Enable Agent by Agent

```bash
# Week 5: Add validation agents
cat >> config/rollout.toml <<EOF
enabled_agents = [
  # Pilot agents
  "template_executor",
  "graph_monitor",
  "validator",
  # New agents
  "security_agent",        # Add: security validation
  "metrics_collector",     # Add: metrics tracking
]
EOF

# Week 6: Add execution agents
cat >> config/rollout.toml <<EOF
enabled_agents = [
  # Previous agents...
  "london_bdd_coordinator",  # Add: BDD test generation
  "task_scheduler",          # Add: task scheduling
]
EOF

# Week 7: Add distributed agents
cat >> config/rollout.toml <<EOF
enabled_agents = [
  # Previous agents...
  "byzantine_validator",   # Add: consensus validation
  "consensus_manager",     # Add: distributed consensus
]
EOF

# Week 8: Full deployment
cat >> config/rollout.toml <<EOF
enabled_agents = [
  "*"  # All agents enabled
]
EOF
```

#### Migration Checklist

- [ ] Pilot successful (>80% time savings, <5% failure rate)
- [ ] Team trained on autonomous system
- [ ] Rollback procedures tested
- [ ] Approval workflows configured
- [ ] Audit logging enabled
- [ ] Security policies validated
- [ ] Performance benchmarks met
- [ ] Documentation updated

### Phase 4: Full Production (Week 9+)

#### Production Configuration

```toml
# config/production.toml

[policies]
version = "1.0"
enforcement_mode = "strict"

[policies.operational]
require_approval_for_destructive = true
auto_approve_read_only = true
audit_all_operations = true

[agents]
count = 12
topology = "mesh"
enable_byzantine_tolerance = true

[agents.resource_limits]
max_cpu_percent = 80
max_memory_mb = 8192
max_concurrent_operations = 100

[monitoring]
enabled = true
health_check_interval_seconds = 30
metrics_export_interval_seconds = 60

[backup]
enabled = true
interval_hours = 6
retention_days = 30
destination = "s3://backups/ggen-autonomous/"
```

#### Cutover Process

```bash
# 1. Final backup of manual system
tar -czf manual-system-backup-$(date +%Y%m%d).tar.gz \
  templates/ \
  generated/ \
  config/

# 2. Stop manual processes
systemctl stop manual-generation.service

# 3. Start autonomous system
ggen-mcp --config config/production.toml --daemon

# 4. Verify health
ggen-mcp health --wait-until-healthy --timeout 300

# 5. Smoke tests
ggen ai generate --description "Test generation" --validate
ggen ai graph --description "Test graph" --verify

# 6. Monitor for 24 hours
ggen-mcp monitor --duration 24h --alert-on-errors
```

### Phase 5: Optimization (Ongoing)

#### Continuous Improvement

```bash
# Weekly performance review
ggen-mcp metrics report \
  --period 7d \
  --compare-to-previous \
  --output reports/weekly-performance.html

# Monthly optimization
ggen-mcp optimize \
  --analyze-patterns \
  --tune-cache \
  --adjust-agent-allocation
```

## Graph Evolution Strategies

Knowledge graphs evolve as your domain model changes. The autonomous system supports safe graph evolution.

### Understanding Graph Versions

```turtle
# ontologies/v1/domain.ttl
@prefix : <http://example.com/ontology#> .

:User a owl:Class ;
    rdfs:label "User" .

:hasEmail a owl:DatatypeProperty ;
    rdfs:domain :User ;
    rdfs:range xsd:string .
```

```turtle
# ontologies/v2/domain.ttl (evolved)
@prefix : <http://example.com/ontology#> .

:User a owl:Class ;
    rdfs:label "User" .

# Breaking change: made email required
:hasEmail a owl:DatatypeProperty ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    owl:minCardinality 1 .  # NEW: required

# Non-breaking addition
:hasPhoneNumber a owl:DatatypeProperty ;  # NEW: optional field
    rdfs:domain :User ;
    rdfs:range xsd:string .
```

### Evolution Strategies

#### Strategy 1: Additive Evolution (Recommended)

**Only add, never remove or change:**

```bash
# Generate evolution diff
ggen graph diff \
  --from ontologies/v1/domain.ttl \
  --to ontologies/v2/domain.ttl \
  --output evolution-v1-to-v2.diff

# Example diff output
+ :hasPhoneNumber a owl:DatatypeProperty  # SAFE: Addition
  :hasEmail owl:minCardinality 1           # UNSAFE: Breaking change

# Apply safe changes only
ggen graph evolve \
  --from ontologies/v1/domain.ttl \
  --evolution evolution-v1-to-v2.diff \
  --strategy additive \
  --output ontologies/v2/domain.ttl
```

#### Strategy 2: Versioned Migration

**Create new versions, migrate data:**

```bash
# Step 1: Create new version
ggen graph version \
  --from ontologies/v1/domain.ttl \
  --version v2 \
  --output ontologies/v2/domain.ttl

# Step 2: Generate migration script
ggen graph migration \
  --from-version v1 \
  --to-version v2 \
  --output migrations/v1_to_v2.sparql

# Example migration
# migrations/v1_to_v2.sparql
PREFIX : <http://example.com/ontology#>

# Add optional phone number (safe)
INSERT {
    ?user :hasPhoneNumber ""
}
WHERE {
    ?user a :User .
    FILTER NOT EXISTS { ?user :hasPhoneNumber ?phone }
}

# Step 3: Test migration on copy
ggen graph migrate \
  --graph data/users.ttl \
  --migration migrations/v1_to_v2.sparql \
  --dry-run

# Step 4: Apply migration
ggen graph migrate \
  --graph data/users.ttl \
  --migration migrations/v1_to_v2.sparql \
  --backup \
  --apply
```

#### Strategy 3: Parallel Versions

**Run multiple versions simultaneously:**

```yaml
# config/graph-versions.yaml
versions:
  - version: v1
    graph: ontologies/v1/domain.ttl
    deprecated: false
    supported_until: "2025-12-31"

  - version: v2
    graph: ontologies/v2/domain.ttl
    deprecated: false
    default: true

  - version: v3-beta
    graph: ontologies/v3/domain.ttl
    deprecated: false
    beta: true
```

```bash
# Query specific version
ggen ai sparql \
  --description "Find all users" \
  --graph-version v1 \
  --output queries/users-v1.sparql

# Generate code for specific version
ggen ai generate \
  --description "User service" \
  --graph-version v2 \
  --output services/user-service-v2.rs
```

### Backward Compatibility Checks

```rust
use ggen_core::{GraphCompatibility, CompatibilityReport};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let compat = GraphCompatibility::new();

    let report = compat.check(
        "ontologies/v1/domain.ttl",
        "ontologies/v2/domain.ttl",
    )?;

    if !report.is_backward_compatible() {
        eprintln!("Breaking changes detected:");
        for change in report.breaking_changes {
            eprintln!("  - {}: {}", change.severity, change.description);
        }
        return Err("Incompatible graph version".into());
    }

    Ok(())
}
```

### Automated Evolution Testing

```bash
# Create evolution test suite
cat > tests/graph-evolution.test.yaml <<EOF
tests:
  - name: "v1 to v2 migration"
    from_graph: "ontologies/v1/domain.ttl"
    to_graph: "ontologies/v2/domain.ttl"
    migration: "migrations/v1_to_v2.sparql"
    assertions:
      - no_data_loss: true
      - referential_integrity: true
      - schema_valid: true

  - name: "backward compatibility"
    from_graph: "ontologies/v1/domain.ttl"
    to_graph: "ontologies/v2/domain.ttl"
    assertions:
      - backward_compatible: true
      - no_removed_classes: true
      - no_removed_properties: true
EOF

# Run evolution tests
ggen graph test-evolution \
  --test-suite tests/graph-evolution.test.yaml \
  --verbose
```

## Rollback Procedures

Always have a rollback plan before deploying changes.

### Snapshot-Based Rollback

#### Creating Snapshots

```bash
# Manual snapshot
ggen-mcp snapshot create \
  --name "pre-deployment-$(date +%Y%m%d-%H%M)" \
  --include-graphs \
  --include-config \
  --include-cache

# Automated snapshots (cron)
0 */6 * * * ggen-mcp snapshot create --name "auto-$(date +\%Y\%m\%d-\%H\%M)"

# List snapshots
ggen-mcp snapshot list
```

#### Restoring Snapshots

```bash
# View snapshot details
ggen-mcp snapshot info pre-deployment-20241010-1400

# Dry run restore
ggen-mcp snapshot restore \
  --snapshot pre-deployment-20241010-1400 \
  --dry-run

# Full restore
ggen-mcp snapshot restore \
  --snapshot pre-deployment-20241010-1400 \
  --stop-agents \
  --backup-current
```

### Operation-Level Rollback

#### Individual Operation Rollback

```rust
use ggen_mcp::{RecoveryAgent, OperationId};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let recovery = RecoveryAgent::new(agent_config);

    // Rollback specific operation
    let op_id = OperationId::from_str("op-123")?;
    recovery.rollback_operation(op_id).await?;

    Ok(())
}
```

```bash
# CLI rollback
ggen-mcp rollback operation op-123

# Rollback last N operations
ggen-mcp rollback --last 5 --confirm

# Rollback by correlation ID (all related operations)
ggen-mcp rollback --correlation-id corr-456
```

### Agent-Level Rollback

```bash
# Stop faulty agent
ggen-mcp agent stop byzantine_validator

# Rollback agent state
ggen-mcp agent rollback byzantine_validator \
  --to-checkpoint checkpoint-20241010-1200

# Restart agent
ggen-mcp agent start byzantine_validator
```

### Full System Rollback

```bash
#!/bin/bash
# rollback-full.sh

set -e

echo "Starting full system rollback..."

# 1. Stop autonomous system
echo "Stopping autonomous system..."
ggen-mcp stop --graceful --timeout 60

# 2. Backup current state
echo "Backing up current state..."
ggen-mcp snapshot create --name "pre-rollback-$(date +%Y%m%d-%H%M)"

# 3. Restore snapshot
echo "Restoring snapshot..."
ggen-mcp snapshot restore --snapshot "$1" --force

# 4. Verify integrity
echo "Verifying integrity..."
ggen-mcp verify --graphs --config --cache

# 5. Restart system
echo "Restarting system..."
ggen-mcp start --config config/production.toml

# 6. Health check
echo "Checking health..."
ggen-mcp health --wait-until-healthy --timeout 300

echo "Rollback complete!"
```

### Gradual Rollback

For large systems, roll back incrementally:

```bash
# Phase 1: Disable new features
ggen-mcp config set feature.new_validation false

# Phase 2: Scale down autonomous agents
ggen-mcp scale --agents 6  # From 12 to 6

# Phase 3: Revert agent versions
ggen-mcp agent update --all --version v1.2.3

# Phase 4: Full rollback if needed
./rollback-full.sh pre-deployment-20241010-1400
```

## Performance Tuning

### Identifying Bottlenecks

#### System-Wide Analysis

```bash
# Performance profile
ggen-mcp profile \
  --duration 5m \
  --include-agents \
  --include-graph-ops \
  --output profile.json

# Bottleneck analysis
ggen-mcp analyze bottlenecks \
  --profile profile.json \
  --top 10
```

#### Agent-Level Profiling

```bash
# Profile specific agent
ggen-mcp agent profile template_executor \
  --duration 2m \
  --flamegraph

# Identify slow operations
ggen-mcp agent analyze template_executor \
  --slow-operations \
  --threshold 1000  # >1 second
```

### Cache Optimization

#### Cache Configuration

```toml
# config/cache.toml
[cache]
enabled = true

[cache.template]
max_size_mb = 500
ttl_seconds = 3600
eviction_policy = "lru"

[cache.graph]
max_size_mb = 2048
ttl_seconds = 7200
eviction_policy = "lfu"  # Least Frequently Used

[cache.ai_responses]
max_size_mb = 1024
ttl_seconds = 1800
hash_algorithm = "sha256"

[cache.preloading]
enabled = true
warm_on_startup = true
preload_patterns = [
  "templates/*.ggt",
  "ontologies/core/*.ttl"
]
```

#### Cache Tuning

```bash
# Analyze cache performance
ggen-mcp cache stats

# Example output:
# Template Cache:
#   Hit Rate: 78.3%
#   Miss Rate: 21.7%
#   Size: 245 MB / 500 MB
#   Evictions: 1,234
#   Avg Lookup Time: 0.8ms

# Increase cache size for low hit rate
ggen-mcp config set cache.template.max_size_mb 1000

# Change eviction policy
ggen-mcp config set cache.template.eviction_policy lfu

# Clear cache
ggen-mcp cache clear --type template
```

### AI Provider Optimization

#### Provider Selection

```bash
# Benchmark providers
ggen-mcp benchmark providers \
  --providers anthropic,openai,ollama \
  --iterations 10 \
  --output benchmarks/providers.json

# Example results:
# Anthropic (claude-3-opus):
#   Latency: p50=2.1s, p95=4.8s
#   Quality: 0.92
#   Cost: $0.015/request
#
# Ollama (qwen2.5-coder):
#   Latency: p50=0.8s, p95=1.5s
#   Quality: 0.85
#   Cost: $0/request
```

#### Intelligent Provider Routing

```toml
[ai.routing]
strategy = "quality-first"  # quality-first, cost-first, latency-first, balanced

[[ai.routing.rules]]
condition = "request.tokens < 1000"
provider = "ollama"  # Fast local for small requests

[[ai.routing.rules]]
condition = "request.requires_high_quality"
provider = "anthropic"  # Best quality

[[ai.routing.rules]]
condition = "default"
provider = "openai"  # Balanced
```

### Concurrent Operation Tuning

```toml
[performance]
max_concurrent_generations = 5
max_concurrent_validations = 10
max_concurrent_graph_queries = 20

[performance.rate_limiting]
global_requests_per_second = 100
per_agent_requests_per_second = 10

[performance.timeouts]
generation_timeout_seconds = 30
validation_timeout_seconds = 10
graph_query_timeout_seconds = 5
consensus_timeout_seconds = 2
```

### Resource Allocation

```bash
# Monitor resource usage
ggen-mcp resources monitor --interval 5

# Adjust allocation
ggen-mcp resources allocate \
  --cpu-percent 60 \
  --memory-mb 6144 \
  --agents 8

# Per-agent limits
ggen-mcp agent configure template_executor \
  --max-cpu-percent 20 \
  --max-memory-mb 512
```

### Database Optimization

```bash
# Analyze graph database
ggen graph analyze \
  --graph ontologies/domain.ttl \
  --statistics

# Optimize indexes
ggen graph optimize \
  --graph ontologies/domain.ttl \
  --rebuild-indexes

# Vacuum and analyze
ggen-mcp db vacuum
ggen-mcp db analyze
```

---

## Migration Success Criteria

✅ **Performance**
- Generation time reduced by >70%
- Quality score consistently >0.85
- System uptime >99.5%

✅ **Quality**
- Validation failures <5%
- Manual interventions <10/week
- Test coverage >80%

✅ **Safety**
- Zero data loss incidents
- Rollback success rate 100%
- Security incidents 0

✅ **Operations**
- Autonomous operation rate >95%
- Mean time to recovery <5 minutes
- Team satisfaction >8/10
