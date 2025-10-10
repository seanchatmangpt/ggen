<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter: Scheduling - Temporal Patterns for Autonomic Maintenance](#chapter-scheduling---temporal-patterns-for-autonomic-maintenance)
  - [Beyond Reactive: Proactive Graph Maintenance](#beyond-reactive-proactive-graph-maintenance)
  - [Pattern: Scheduled Graph Maintenance](#pattern-scheduled-graph-maintenance)
    - [Use Cases](#use-cases)
    - [Implementation Strategies](#implementation-strategies)
      - [1. Cron-Based Scheduling (Linux/Unix)](#1-cron-based-scheduling-linuxunix)
      - [2. GitHub Actions (CI/CD Scheduling)](#2-github-actions-cicd-scheduling)
      - [3. SystemD Timers (Linux Systems)](#3-systemd-timers-linux-systems)
  - [Scheduled Task Types](#scheduled-task-types)
    - [1. Consistency Validation](#1-consistency-validation)
    - [2. Performance Optimization](#2-performance-optimization)
    - [3. Metrics Collection](#3-metrics-collection)
    - [4. Cleanup and Pruning](#4-cleanup-and-pruning)
  - [Configuration](#configuration)
  - [Monitoring and Alerting](#monitoring-and-alerting)
    - [Health Checks](#health-checks)
    - [Alerting](#alerting)
  - [Advanced Patterns](#advanced-patterns)
    - [Incremental Metrics (Time-Series)](#incremental-metrics-time-series)
    - [Self-Healing](#self-healing)
  - [Testing Scheduled Tasks](#testing-scheduled-tasks)
  - [Summary: The Temporal Layer](#summary-the-temporal-layer)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter: Scheduling - Temporal Patterns for Autonomic Maintenance

## Beyond Reactive: Proactive Graph Maintenance

Hooks (Pattern 021) provide **reactive** graph updates: changes trigger regeneration. But some maintenance tasks need **scheduled execution**:

- **Nightly full rebuilds** to prevent incremental drift
- **Periodic consistency checks** to validate graph integrity
- **Cache invalidation** on a schedule (e.g., query result caches)
- **Metric collection** for graph evolution tracking
- **Backup rotation** for graph history

This chapter covers **temporal patterns**: using time-based triggers to maintain autonomic knowledge graphs.

## Pattern: Scheduled Graph Maintenance

### Use Cases

1. **Drift Prevention**: Full rebuild nightly to catch incremental errors
2. **Consistency Validation**: Hash-based checks every 6 hours
3. **Performance Optimization**: Rebuild indexes weekly
4. **Analytics**: Daily graph metrics (size, entities, relationships)
5. **Cleanup**: Remove stale graphs from old branches monthly

### Implementation Strategies

#### 1. Cron-Based Scheduling (Linux/Unix)

```bash
# crontab -e
# Add scheduled ggen tasks

# Full rebuild every night at 2 AM (low traffic)
0 2 * * * cd /path/to/repo && ggen graph --full --output graphs/main.ttl && git add graphs/main.ttl && git commit -m "chore: nightly full rebuild [skip ci]"

# Consistency check every 6 hours
0 */6 * * * cd /path/to/repo && ggen validate --consistency-check graphs/main.ttl || echo "❌ Consistency check failed" | mail -s "GGen Alert" admin@example.com

# Weekly index rebuild (Sunday 3 AM)
0 3 * * 0 cd /path/to/repo && ggen index rebuild graphs/main.ttl

# Daily metrics collection
30 1 * * * cd /path/to/repo && ggen stats export --output metrics/$(date +\%Y-\%m-\%d).json

# Monthly cleanup of old branch graphs
0 4 1 * * cd /path/to/repo && ggen cleanup --prune-branches --older-than 90d
```

#### 2. GitHub Actions (CI/CD Scheduling)

```yaml
# .github/workflows/scheduled-maintenance.yml
name: Scheduled Graph Maintenance

on:
  schedule:
    # Nightly rebuild at 2 AM UTC
    - cron: '0 2 * * *'
    # Consistency check every 6 hours
    - cron: '0 */6 * * *'

  workflow_dispatch:  # Allow manual triggers

jobs:
  nightly-rebuild:
    if: github.event.schedule == '0 2 * * *'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Full graph rebuild
        run: ggen graph --full --output graphs/main.ttl

      - name: Commit if changed
        uses: stefanzweifel/git-auto-commit-action@v4
        with:
          commit_message: "chore: nightly full rebuild [skip ci]"
          file_pattern: "graphs/*.ttl"

      - name: Notify on failure
        if: failure()
        uses: 8398a7/action-slack@v3
        with:
          status: ${{ job.status }}
          text: "❌ Nightly graph rebuild failed"
          webhook_url: ${{ secrets.SLACK_WEBHOOK }}

  consistency-check:
    if: github.event.schedule == '0 */6 * * *'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Validate consistency
        run: |
          # Compare committed graph with fresh rebuild
          ggen graph --full --output /tmp/rebuild.ttl

          COMMITTED_HASH=$(ggen hash graphs/main.ttl)
          REBUILD_HASH=$(ggen hash /tmp/rebuild.ttl)

          if [ "$COMMITTED_HASH" != "$REBUILD_HASH" ]; then
            echo "❌ Consistency drift detected!"
            echo "Committed: $COMMITTED_HASH"
            echo "Rebuild:   $REBUILD_HASH"
            exit 1
          fi

          echo "✅ Consistency validated"
```

#### 3. SystemD Timers (Linux Systems)

```ini
# /etc/systemd/system/ggen-nightly.timer
[Unit]
Description=Nightly GGen Full Rebuild

[Timer]
# Run daily at 2 AM
OnCalendar=daily
OnCalendar=*-*-* 02:00:00
Persistent=true

[Install]
WantedBy=timers.target
```

```ini
# /etc/systemd/system/ggen-nightly.service
[Unit]
Description=GGen Nightly Full Rebuild Service

[Service]
Type=oneshot
User=ggen
WorkingDirectory=/opt/ggen-repo
ExecStart=/usr/local/bin/ggen graph --full --output graphs/main.ttl
ExecStartPost=/usr/bin/git add graphs/main.ttl
ExecStartPost=/usr/bin/git commit -m "chore: nightly rebuild [skip ci]"
```

**Enable:**

```bash
sudo systemctl enable ggen-nightly.timer
sudo systemctl start ggen-nightly.timer
sudo systemctl list-timers  # Verify
```

## Scheduled Task Types

### 1. Consistency Validation

**Goal**: Ensure incremental updates haven't drifted from ground truth.

```rust
// ggen/src/scheduled/consistency.rs
pub fn consistency_check(committed_graph: &Path) -> Result<ConsistencyReport> {
    // 1. Hash committed graph
    let committed_hash = hash_graph_file(committed_graph)?;

    // 2. Rebuild from source
    let temp_rebuild = rebuild_graph_full_temp()?;
    let rebuild_hash = hash_graph_file(&temp_rebuild)?;

    // 3. Compare
    if committed_hash != rebuild_hash {
        // Detailed diff for debugging
        let diff = semantic_diff(&committed_graph, &temp_rebuild)?;

        Ok(ConsistencyReport {
            consistent: false,
            committed_hash,
            rebuild_hash,
            diff: Some(diff),
            recommendation: "Run 'ggen graph --full' to resync".into(),
        })
    } else {
        Ok(ConsistencyReport {
            consistent: true,
            committed_hash,
            rebuild_hash,
            diff: None,
            recommendation: "Graph is consistent".into(),
        })
    }
}
```

**CLI:**

```bash
$ ggen validate --consistency-check graphs/main.ttl
🔍 Checking consistency...
   • Committed hash: 8f3a9b2c...
   • Rebuild hash:   8f3a9b2c...
✅ Graph consistent with source code
```

### 2. Performance Optimization

**Goal**: Rebuild indexes, compact storage, optimize query performance.

```rust
// ggen/src/scheduled/optimize.rs
pub fn optimize_graph_storage(graph_db: &Path) -> Result<OptimizationReport> {
    let start = Instant::now();

    // 1. Rebuild indexes
    rebuild_sparql_indexes(graph_db)?;

    // 2. Compact storage (remove fragmentation)
    compact_database(graph_db)?;

    // 3. Update statistics (for query planner)
    update_statistics(graph_db)?;

    // 4. Vacuum (reclaim space)
    vacuum_database(graph_db)?;

    let duration = start.elapsed();
    let size_before = get_file_size(graph_db)?;
    let size_after = get_file_size(graph_db)?;

    Ok(OptimizationReport {
        duration,
        size_before,
        size_after,
        space_saved: size_before - size_after,
    })
}
```

**Scheduled (Weekly):**

```bash
# Sunday 3 AM: Optimize graph storage
0 3 * * 0 ggen optimize graphs/main.db --vacuum --rebuild-indexes
```

### 3. Metrics Collection

**Goal**: Track graph evolution over time.

```rust
// ggen/src/scheduled/metrics.rs
pub fn collect_metrics(graph: &Path) -> Result<GraphMetrics> {
    let graph = load_graph(graph)?;

    Ok(GraphMetrics {
        timestamp: Utc::now(),
        triple_count: graph.len(),
        entity_count: count_entities(&graph),
        entity_types: count_entity_types(&graph),
        relationship_count: count_relationships(&graph),
        average_out_degree: compute_avg_out_degree(&graph),
        max_depth: compute_max_depth(&graph),
        file_size: get_file_size(graph)?,
    })
}

pub fn export_metrics(metrics: &GraphMetrics, output: &Path) -> Result<()> {
    let json = serde_json::to_string_pretty(metrics)?;
    fs::write(output, json)?;
    Ok(())
}
```

**Scheduled (Daily):**

```bash
# Daily 1:30 AM: Collect metrics
30 1 * * * ggen stats export --output metrics/$(date +\%Y-\%m-\%d).json
```

**Visualization:**

```python
# scripts/plot-metrics.py
import json
import matplotlib.pyplot as plt
from pathlib import Path

metrics_dir = Path("metrics")
metrics = []

for file in sorted(metrics_dir.glob("*.json")):
    with open(file) as f:
        metrics.append(json.load(f))

dates = [m["timestamp"] for m in metrics]
triple_counts = [m["triple_count"] for m in metrics]

plt.plot(dates, triple_counts)
plt.xlabel("Date")
plt.ylabel("Triple Count")
plt.title("Knowledge Graph Growth Over Time")
plt.savefig("metrics/growth.png")
```

### 4. Cleanup and Pruning

**Goal**: Remove stale graphs from deleted branches, old backups.

```rust
// ggen/src/scheduled/cleanup.rs
pub fn prune_old_branches(repo: &Repository, older_than_days: u32) -> Result<PruneReport> {
    let cutoff = Utc::now() - Duration::days(older_than_days as i64);
    let mut pruned = Vec::new();

    // Find merged/deleted branches
    for branch in repo.branches(Some(BranchType::Local))? {
        let (branch, _) = branch?;
        let name = branch.name()?.unwrap();

        // Skip main branches
        if name == "main" || name == "master" || name == "develop" {
            continue;
        }

        // Check last commit date
        if let Some(commit) = branch.get().peel_to_commit().ok() {
            let commit_time = Utc.timestamp(commit.time().seconds(), 0);

            if commit_time < cutoff {
                // Remove graph files for this branch
                let graph_path = format!("graphs/branches/{}.ttl", name);
                if Path::new(&graph_path).exists() {
                    fs::remove_file(&graph_path)?;
                    pruned.push(name.to_string());
                }
            }
        }
    }

    Ok(PruneReport {
        branches_pruned: pruned.len(),
        branches: pruned,
    })
}
```

**Scheduled (Monthly):**

```bash
# First of month, 4 AM: Cleanup old branches
0 4 1 * * ggen cleanup --prune-branches --older-than 90d
```

## Configuration

Centralize scheduling config:

```toml
# .ggen/schedule.toml

[maintenance]
enabled = true

[maintenance.nightly_rebuild]
enabled = true
time = "02:00"  # 2 AM
full_rebuild = true
auto_commit = true

[maintenance.consistency_check]
enabled = true
interval_hours = 6
alert_on_failure = true
alert_email = "admin@example.com"

[maintenance.metrics]
enabled = true
time = "01:30"
export_path = "metrics/{date}.json"

[maintenance.optimization]
enabled = true
day_of_week = "sunday"
time = "03:00"
operations = ["vacuum", "rebuild-indexes", "update-statistics"]

[maintenance.cleanup]
enabled = true
day_of_month = 1
time = "04:00"
prune_branches_older_than_days = 90
prune_backups_older_than_days = 180
```

## Monitoring and Alerting

### Health Checks

```rust
// ggen/src/scheduled/health.rs
pub fn health_check(config: &ScheduleConfig) -> Result<HealthReport> {
    let mut issues = Vec::new();

    // Check last rebuild time
    if let Some(last_rebuild) = get_last_rebuild_time()? {
        let age = Utc::now() - last_rebuild;
        if age > Duration::hours(25) {  // Expected: 24h
            issues.push(format!("Nightly rebuild overdue by {}", age));
        }
    }

    // Check graph consistency
    if !config.maintenance.consistency_check.enabled {
        issues.push("Consistency checks disabled (risky)".into());
    }

    // Check disk space
    let graph_size = get_total_graph_size()?;
    let available = get_available_disk_space()?;
    if graph_size as f64 > available as f64 * 0.8 {
        issues.push("Graph consuming >80% of available disk space".into());
    }

    Ok(HealthReport {
        healthy: issues.is_empty(),
        issues,
    })
}
```

### Alerting

```bash
# Run health check before each maintenance task
0 1 * * * ggen health check || {
  echo "⚠️ GGen health check failed" | \
    mail -s "GGen Alert" admin@example.com
}
```

## Advanced Patterns

### Incremental Metrics (Time-Series)

Instead of snapshots, track deltas:

```rust
pub struct MetricsDelta {
    timestamp: DateTime<Utc>,
    triples_added: usize,
    triples_removed: usize,
    entities_added: usize,
    entities_removed: usize,
}

pub fn compute_metrics_delta(
    previous: &GraphMetrics,
    current: &GraphMetrics,
) -> MetricsDelta {
    MetricsDelta {
        timestamp: current.timestamp,
        triples_added: current.triple_count.saturating_sub(previous.triple_count),
        triples_removed: previous.triple_count.saturating_sub(current.triple_count),
        entities_added: current.entity_count.saturating_sub(previous.entity_count),
        entities_removed: previous.entity_count.saturating_sub(current.entity_count),
    }
}
```

**Visualization:**

```bash
# Daily change graph
$ ggen metrics plot --type delta --output metrics/daily-changes.png
```

### Self-Healing

Automatically fix common issues:

```rust
pub fn self_healing_check(graph: &Path) -> Result<()> {
    let issues = detect_issues(graph)?;

    for issue in issues {
        match issue {
            Issue::InconsistentGraph => {
                eprintln!("🔧 Detected inconsistency, rebuilding...");
                rebuild_graph_full()?;
            }
            Issue::CorruptedIndex => {
                eprintln!("🔧 Detected corrupted index, rebuilding...");
                rebuild_indexes(graph)?;
            }
            Issue::MissingMetadata => {
                eprintln!("🔧 Detected missing metadata, regenerating...");
                regenerate_metadata(graph)?;
            }
        }
    }

    Ok(())
}
```

**Scheduled:**

```bash
# Hourly self-healing check
0 * * * * ggen self-heal graphs/main.ttl
```

## Testing Scheduled Tasks

Provide dry-run mode:

```bash
$ ggen schedule simulate --config .ggen/schedule.toml --duration 7d
🔍 Simulating 7 days of scheduled tasks...

Day 1 (2025-01-01):
  [02:00] ✅ Nightly rebuild (1.2s, 5000 triples)
  [08:00] ✅ Consistency check (passed)
  [14:00] ✅ Consistency check (passed)
  [20:00] ✅ Consistency check (passed)

Day 2 (2025-01-02):
  [02:00] ✅ Nightly rebuild (1.1s, 5020 triples)
  ...

Day 7 (2025-01-07):
  [02:00] ✅ Nightly rebuild (1.3s, 5150 triples)
  [03:00] ✅ Weekly optimization (5.2s, saved 120KB)

Summary:
  • 7 nightly rebuilds
  • 28 consistency checks (all passed)
  • 1 optimization (120KB saved)
  • Total CPU time: 12.5s
```

## Summary: The Temporal Layer

Scheduled maintenance completes the autonomic system:

| Layer | Trigger | Purpose |
|-------|---------|---------|
| **Hooks** (021) | Code changes | Reactive updates |
| **Delta-Driven** (022) | Incremental changes | Performance |
| **Git-as-Runtime** (024) | Git operations | Integration |
| **Scheduling** | Time | Proactive maintenance |

Together, these patterns create a **fully autonomic knowledge graph**:

1. **Reactive**: Hooks update graphs on code changes
2. **Efficient**: Delta-driven updates for speed
3. **Integrated**: Git-as-runtime for deployment simplicity
4. **Proactive**: Scheduled tasks prevent drift and optimize

The result: **A knowledge graph that maintains itself**, requiring zero manual intervention once configured.

---

**Chapter Status**: ✅ Complete
**Related Patterns**: 021, 022, 024
**Prerequisites**: Cron or systemd (Linux), GitHub Actions (cloud)
**Complexity**: Medium
**Maintenance**: Low (self-maintaining once configured)

**Next Part**: Part V - Advanced Integrations (SPARQL endpoints, LSP servers, CI/CD pipelines)
