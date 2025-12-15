<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DfLSS Prevention Metrics Tracking System](#dflss-prevention-metrics-tracking-system)
  - [Metrics Overview](#metrics-overview)
  - [1. Defect Metrics](#1-defect-metrics)
    - [Purpose](#purpose)
    - [Key Metrics](#key-metrics)
      - [Defect Density](#defect-density)
      - [Defect Distribution by Phase](#defect-distribution-by-phase)
      - [Defect Escape Rate](#defect-escape-rate)
    - [Data Collection](#data-collection)
  - [2. Cycle Time Metrics](#2-cycle-time-metrics)
    - [Purpose](#purpose-1)
    - [Key Metrics](#key-metrics-1)
      - [Total Cycle Time](#total-cycle-time)
      - [Lead Time for Design Review](#lead-time-for-design-review)
    - [Data Collection](#data-collection-1)
  - [3. Rework Metrics](#3-rework-metrics)
    - [Purpose](#purpose-2)
    - [Key Metrics](#key-metrics-2)
      - [Rework Percentage](#rework-percentage)
      - [Andon Signal Frequency](#andon-signal-frequency)
    - [Data Collection](#data-collection-2)
  - [4. Quality Metrics](#4-quality-metrics)
    - [Purpose](#purpose-3)
    - [Key Metrics](#key-metrics-3)
      - [Test Pass Rate](#test-pass-rate)
      - [SLO Compliance Rate](#slo-compliance-rate)
      - [Code Review Approval Rate](#code-review-approval-rate)
      - [Design Review Pass Rate](#design-review-pass-rate)
    - [Data Collection](#data-collection-3)
  - [5. Team Metrics](#5-team-metrics)
    - [Purpose](#purpose-4)
    - [Key Metrics](#key-metrics-4)
      - [Team Satisfaction Score](#team-satisfaction-score)
      - [Learning Hours](#learning-hours)
      - [Kaizen Improvements Implemented](#kaizen-improvements-implemented)
    - [Data Collection](#data-collection-4)
  - [Metrics Dashboard Implementation](#metrics-dashboard-implementation)
    - [Data Model](#data-model)
    - [Collection Script](#collection-script)
    - [Dashboard Visualization](#dashboard-visualization)
  - [Reporting Cadence](#reporting-cadence)
    - [Daily](#daily)
    - [Weekly](#weekly)
    - [Monthly](#monthly)
    - [Quarterly](#quarterly)
  - [Success Criteria](#success-criteria)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DfLSS Prevention Metrics Tracking System

**Version**: 1.0.0
**Purpose**: Measure effectiveness of prevention systems
**Principle**: What gets measured gets improved

---

## Metrics Overview

We track 5 categories of metrics to measure prevention system effectiveness:

1. **Defect Metrics** - Measure quality improvements
2. **Cycle Time Metrics** - Measure efficiency improvements
3. **Rework Metrics** - Measure waste reduction
4. **Quality Metrics** - Measure process compliance
5. **Team Metrics** - Measure team health and satisfaction

---

## 1. Defect Metrics

### Purpose

Measure how well prevention systems reduce defects.

### Key Metrics

#### Defect Density

**Definition**: Number of defects per 1000 lines of code (KLOC)

**Calculation**:
```
Defect Density = (Total Defects / Lines of Code) × 1000
```

**Target**: ≤ 1.0 defects/KLOC (Six Sigma quality: 3.4 defects per million)

**Collection**:
- Source: Issue tracker (GitHub Issues/JIRA)
- Frequency: Weekly
- Tag defects by severity (critical, high, medium, low)

#### Defect Distribution by Phase

**Definition**: Where defects are found

**Categories**:
- **Design Phase**: Caught in design review (best outcome)
- **Testing Phase**: Caught in unit/integration tests (good outcome)
- **Production Phase**: Caught by users (worst outcome)

**Target Distribution**:
- 60% in design phase
- 35% in testing phase
- 5% in production phase

**Calculation**:
```rust
pub struct DefectDistribution {
    pub design: usize,
    pub testing: usize,
    pub production: usize,
}

impl DefectDistribution {
    pub fn total(&self) -> usize {
        self.design + self.testing + self.production
    }

    pub fn design_percentage(&self) -> f64 {
        (self.design as f64 / self.total() as f64) * 100.0
    }
}
```

#### Defect Escape Rate

**Definition**: Percentage of defects that escape to production

**Calculation**:
```
Defect Escape Rate = (Production Defects / Total Defects) × 100%
```

**Target**: ≤ 5%

**Trend**: Should decrease over time as prevention systems mature

### Data Collection

**Automated**:
```bash
# Compiler errors (Andon signal)
cargo make check 2>&1 | grep -c "^error"

# Test failures (Andon signal)
cargo make test 2>&1 | grep -c "test result: FAILED"

# Linting warnings (Andon signal)
cargo make lint 2>&1 | grep -c "^warning"
```

**Manual** (weekly):
- Tag GitHub issues with "defect" label
- Categorize by phase (design/testing/production)
- Categorize by severity (critical/high/medium/low)

**Dashboard Query**:
```sql
SELECT
    phase,
    severity,
    COUNT(*) as count
FROM defects
WHERE created_at >= DATE_SUB(NOW(), INTERVAL 1 WEEK)
GROUP BY phase, severity;
```

---

## 2. Cycle Time Metrics

### Purpose

Measure how prevention systems reduce time from idea to production.

### Key Metrics

#### Total Cycle Time

**Definition**: Time from feature request to production deployment

**Phases**:
1. **Design to Code**: Design review → First commit
2. **Code to Test**: First commit → All tests passing
3. **Test to Deploy**: Tests passing → Production deployment

**Target**: ≤ 24 hours for small features, ≤ 5 days for large features

**Calculation**:
```rust
pub struct CycleTime {
    pub design_to_code_hours: f64,
    pub code_to_test_hours: f64,
    pub test_to_deploy_hours: f64,
}

impl CycleTime {
    pub fn total(&self) -> f64 {
        self.design_to_code_hours + self.code_to_test_hours + self.test_to_deploy_hours
    }
}
```

#### Lead Time for Design Review

**Definition**: Time from design review request to approval

**Target**: ≤ 2 hours

**Calculation**:
```
Lead Time = Design Approval Timestamp - Design Submitted Timestamp
```

**Trend**: Should decrease as team gains experience with checklist

### Data Collection

**Automated** (Git hooks):
```bash
# Capture timestamps in git commit messages
git log --format="%H|%ai|%s" | grep "feat:" | awk -F'|' '{print $2}'
```

**Manual** (design review tracker):
```markdown
# Design Review Tracker

| Feature | Submitted | Approved | Design Hours | Code Hours | Test Hours | Deploy Hours | Total Hours |
|---------|-----------|----------|--------------|------------|------------|--------------|-------------|
| Auth API | 2025-01-15 10:00 | 2025-01-15 11:30 | 1.5 | 4 | 2 | 1 | 8.5 |
```

**Dashboard Query**:
```sql
SELECT
    AVG(design_to_code_hours) as avg_design_to_code,
    AVG(code_to_test_hours) as avg_code_to_test,
    AVG(test_to_deploy_hours) as avg_test_to_deploy,
    AVG(total_cycle_time_hours) as avg_total_cycle_time
FROM cycle_times
WHERE created_at >= DATE_SUB(NOW(), INTERVAL 1 MONTH);
```

---

## 3. Rework Metrics

### Purpose

Measure waste reduction (rework is waste in Lean terms).

### Key Metrics

#### Rework Percentage

**Definition**: Percentage of time spent fixing defects vs. building new features

**Calculation**:
```
Rework % = (Rework Hours / Total Hours) × 100%
```

**Target**: ≤ 15%

**Categories**:
- Compiler error fixes
- Test failure fixes
- Design changes after implementation
- Bug fixes

**Calculation**:
```rust
pub struct ReworkMetrics {
    pub compiler_error_fixes: usize,
    pub test_failure_fixes: usize,
    pub design_changes: usize,
    pub bug_fixes: usize,
    pub total_work_hours: f64,
}

impl ReworkMetrics {
    pub fn total_rework_hours(&self) -> f64 {
        // Assume avg 30 min per fix
        let fixes = self.compiler_error_fixes
            + self.test_failure_fixes
            + self.design_changes
            + self.bug_fixes;
        (fixes as f64) * 0.5
    }

    pub fn rework_percentage(&self) -> f64 {
        (self.total_rework_hours() / self.total_work_hours) * 100.0
    }
}
```

#### Andon Signal Frequency

**Definition**: How often Andon signals (compiler errors, test failures) occur

**Target**: Decreasing trend (fewer signals = better prevention)

**Calculation**:
```bash
# Weekly Andon signal count
echo "Compiler errors: $(git log --since="1 week ago" | grep -c "fix: compiler")"
echo "Test failures: $(git log --since="1 week ago" | grep -c "fix: test")"
```

### Data Collection

**Automated** (Git commit analysis):
```bash
# Count "fix:" commits (these are rework)
git log --oneline --since="1 month ago" | grep -c "^[a-f0-9]* fix:"

# Count "feat:" commits (these are new work)
git log --oneline --since="1 month ago" | grep -c "^[a-f0-9]* feat:"

# Calculate rework percentage
```

**Manual** (time tracking):
- Tag tasks in issue tracker as "rework" or "new work"
- Sum hours for each category weekly

**Dashboard Query**:
```sql
SELECT
    SUM(CASE WHEN type = 'rework' THEN hours ELSE 0 END) as rework_hours,
    SUM(hours) as total_hours,
    (SUM(CASE WHEN type = 'rework' THEN hours ELSE 0 END) / SUM(hours)) * 100 as rework_percentage
FROM work_log
WHERE created_at >= DATE_SUB(NOW(), INTERVAL 1 MONTH);
```

---

## 4. Quality Metrics

### Purpose

Measure adherence to prevention processes.

### Key Metrics

#### Test Pass Rate

**Definition**: Percentage of test runs that pass

**Target**: ≥ 95%

**Calculation**:
```
Test Pass Rate = (Passed Tests / Total Tests) × 100%
```

**Collection**:
```bash
# Run tests and extract pass rate
cargo make test 2>&1 | grep "test result:" | awk '{print $4}'
```

#### SLO Compliance Rate

**Definition**: Percentage of SLOs (Service Level Objectives) met

**SLOs**:
- First build ≤ 15s
- Incremental build ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end

**Target**: 100% compliance

**Calculation**:
```rust
pub struct SloCompliance {
    pub first_build_time: f64,
    pub incremental_build_time: f64,
    pub rdf_processing_time: f64,
    pub generation_memory_mb: f64,
    pub cli_scaffolding_time: f64,
}

impl SloCompliance {
    pub fn compliance_rate(&self) -> f64 {
        let mut met = 0;
        let total = 5;

        if self.first_build_time <= 15.0 { met += 1; }
        if self.incremental_build_time <= 2.0 { met += 1; }
        if self.rdf_processing_time <= 5.0 { met += 1; }
        if self.generation_memory_mb <= 100.0 { met += 1; }
        if self.cli_scaffolding_time <= 3.0 { met += 1; }

        (met as f64 / total as f64) * 100.0
    }
}
```

**Collection**:
```bash
cargo make slo-check
```

#### Code Review Approval Rate

**Definition**: Percentage of PRs approved on first review

**Target**: ≥ 80%

**Calculation**:
```
Approval Rate = (First-Time Approvals / Total PRs) × 100%
```

**Collection**:
```bash
# GitHub CLI query
gh pr list --state closed --limit 100 --json reviews,number | \
  jq '[.[] | select(.reviews | length == 1)] | length'
```

#### Design Review Pass Rate

**Definition**: Percentage of designs approved on first review

**Target**: ≥ 70% (lower than code review because design is harder)

**Calculation**:
```
Design Pass Rate = (First-Time Approvals / Total Design Reviews) × 100%
```

**Collection**: Manual tracking in design review log

### Data Collection

**Automated** (CI pipeline):
```yaml
# .github/workflows/metrics.yml
name: Metrics Collection
on:
  push:
  schedule:
    - cron: '0 0 * * *'  # Daily at midnight

jobs:
  metrics:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Collect test pass rate
        run: cargo make test 2>&1 | tee test_results.txt
      - name: Collect SLO compliance
        run: cargo make slo-check | tee slo_results.txt
      - name: Upload to metrics dashboard
        run: ./scripts/upload_metrics.sh
```

---

## 5. Team Metrics

### Purpose

Measure team health and satisfaction with prevention systems.

### Key Metrics

#### Team Satisfaction Score

**Definition**: Self-reported satisfaction with processes (1-10 scale)

**Target**: ≥ 8.0/10

**Questions** (monthly survey):
1. How satisfied are you with the design review process? (1-10)
2. How satisfied are you with the Kaizen retrospective? (1-10)
3. How satisfied are you with the prevention systems? (1-10)
4. How satisfied are you with the tooling and automation? (1-10)

**Calculation**:
```
Team Satisfaction = (Q1 + Q2 + Q3 + Q4) / 4
```

#### Learning Hours

**Definition**: Time spent on learning/improvement activities

**Target**: ≥ 10% of total work time

**Categories**:
- Training (Week 1-4 DfLSS training)
- Kaizen retrospectives
- Quarterly strategic reviews
- Reading/research
- Experimentation

**Calculation**:
```rust
pub struct LearningMetrics {
    pub training_hours: f64,
    pub kaizen_hours: f64,
    pub quarterly_review_hours: f64,
    pub research_hours: f64,
    pub experimentation_hours: f64,
    pub total_work_hours: f64,
}

impl LearningMetrics {
    pub fn total_learning_hours(&self) -> f64 {
        self.training_hours + self.kaizen_hours + self.quarterly_review_hours
            + self.research_hours + self.experimentation_hours
    }

    pub fn learning_percentage(&self) -> f64 {
        (self.total_learning_hours() / self.total_work_hours) * 100.0
    }
}
```

#### Kaizen Improvements Implemented

**Definition**: Number of countermeasures implemented from Kaizen cycles

**Target**: ≥ 12 per quarter (1 per week)

**Tracking**: Issue tracker with "kaizen-improvement" label

**Calculation**:
```bash
gh issue list --label "kaizen-improvement" --state closed --search "closed:>=$(date -d '3 months ago' +%Y-%m-%d)"
```

### Data Collection

**Monthly Survey** (Google Forms/Slack poll):
```markdown
# Monthly Team Satisfaction Survey

1. Design review process satisfaction: [1-10 scale]
2. Kaizen retrospective satisfaction: [1-10 scale]
3. Prevention systems satisfaction: [1-10 scale]
4. Tooling and automation satisfaction: [1-10 scale]
5. What's working well?
6. What needs improvement?
```

**Learning Hours** (self-reported in time tracking):
- Tag tasks with "learning" category
- Sum weekly

**Kaizen Improvements**:
- GitHub Issues with "kaizen-improvement" label
- Count closed issues monthly

---

## Metrics Dashboard Implementation

### Data Model

```rust
use chrono::{DateTime, Utc};
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    pub timestamp: DateTime<Utc>,
    pub defect_metrics: DefectMetrics,
    pub cycle_time_metrics: CycleTimeMetrics,
    pub rework_metrics: ReworkMetrics,
    pub quality_metrics: QualityMetrics,
    pub team_metrics: TeamMetrics,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectMetrics {
    pub defect_density: f64,
    pub defects_in_design: usize,
    pub defects_in_testing: usize,
    pub defects_in_production: usize,
    pub defect_escape_rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CycleTimeMetrics {
    pub design_to_code_hours: f64,
    pub code_to_test_hours: f64,
    pub test_to_deploy_hours: f64,
    pub total_cycle_time_hours: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReworkMetrics {
    pub rework_percentage: f64,
    pub compiler_error_fixes: usize,
    pub test_failure_fixes: usize,
    pub design_changes: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    pub test_pass_rate: f64,
    pub slo_compliance_rate: f64,
    pub code_review_approval_rate: f64,
    pub design_review_pass_rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeamMetrics {
    pub team_satisfaction_score: f64,
    pub learning_hours: f64,
    pub learning_percentage: f64,
    pub kaizen_improvements_implemented: usize,
}
```

### Collection Script

```bash
#!/bin/bash
# scripts/collect_metrics.sh

set -e

echo "Collecting DfLSS Prevention Metrics..."

# Defect metrics
compiler_errors=$(cargo make check 2>&1 | grep -c "^error" || echo 0)
test_failures=$(cargo make test 2>&1 | grep -c "FAILED" || echo 0)
lint_warnings=$(cargo make lint 2>&1 | grep -c "^warning" || echo 0)

# Cycle time metrics (from git log)
avg_cycle_time=$(git log --since="1 month ago" --format="%H|%ai" | \
  awk '{print $2}' | sort | uniq | wc -l)

# Rework metrics
rework_commits=$(git log --oneline --since="1 month ago" | grep -c "^[a-f0-9]* fix:" || echo 0)
total_commits=$(git log --oneline --since="1 month ago" | wc -l)
rework_percentage=$(echo "scale=2; ($rework_commits / $total_commits) * 100" | bc)

# Quality metrics
test_pass_rate=$(cargo make test 2>&1 | grep "test result:" | awk '{gsub(/%/,""); print $6}')
slo_compliance=$(cargo make slo-check | grep "Compliance:" | awk '{print $2}')

# Output JSON
cat > metrics.json <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "defect_metrics": {
    "compiler_errors": $compiler_errors,
    "test_failures": $test_failures,
    "lint_warnings": $lint_warnings
  },
  "rework_metrics": {
    "rework_percentage": $rework_percentage,
    "rework_commits": $rework_commits,
    "total_commits": $total_commits
  },
  "quality_metrics": {
    "test_pass_rate": $test_pass_rate,
    "slo_compliance_rate": $slo_compliance
  }
}
EOF

echo "Metrics collected: metrics.json"
```

### Dashboard Visualization

**Simple Text Dashboard**:

```bash
#!/bin/bash
# scripts/show_dashboard.sh

cat <<EOF
┌─────────────────────────────────────────────────────────────┐
│          DfLSS PREVENTION METRICS DASHBOARD                 │
└─────────────────────────────────────────────────────────────┘

DEFECT METRICS
  Defect Density: 0.8 defects/KLOC   [TARGET: ≤1.0] ✓
  Escape Rate: 3%                    [TARGET: ≤5%]  ✓

CYCLE TIME METRICS
  Design → Code: 2.5 hours           [TARGET: ≤4h]  ✓
  Code → Test: 1.8 hours             [TARGET: ≤3h]  ✓
  Test → Deploy: 0.5 hours           [TARGET: ≤1h]  ✓
  Total Cycle Time: 4.8 hours        [TARGET: ≤8h]  ✓

REWORK METRICS
  Rework %: 12%                      [TARGET: ≤15%] ✓
  Compiler Fixes: 5                  [TREND: ↓]     ✓
  Test Fixes: 3                      [TREND: ↓]     ✓

QUALITY METRICS
  Test Pass Rate: 97%                [TARGET: ≥95%] ✓
  SLO Compliance: 100%               [TARGET: 100%] ✓
  Review Approval: 85%               [TARGET: ≥80%] ✓

TEAM METRICS
  Satisfaction: 8.5/10               [TARGET: ≥8.0] ✓
  Learning %: 12%                    [TARGET: ≥10%] ✓
  Kaizen Improvements: 15            [TARGET: ≥12]  ✓

OVERALL STATUS: ALL GREEN ✓
EOF
```

---

## Reporting Cadence

### Daily

**Automated CI Metrics**:
- Test pass rate
- SLO compliance
- Compiler errors
- Test failures

**Slack Notification**:
```
Daily Metrics (2025-01-20)
✓ Tests: 97% pass (target: ≥95%)
✓ SLOs: 100% compliance
⚠ Compiler errors: 5 (down from 8 yesterday)
```

### Weekly

**Metrics Summary Email**:
- Defect density trend
- Cycle time trend
- Rework percentage
- Quality metrics

### Monthly

**Kaizen Retrospective Dashboard**:
- All metrics categories
- Month-over-month comparison
- Top 3 problems identified
- Countermeasures planned

### Quarterly

**Strategic Review Report**:
- 3-month trend charts
- Goal achievement (% of targets met)
- ROI of prevention systems
- Process improvement recommendations

---

## Success Criteria

**After 3 Months**:
- Defect density ≤ 1.0/KLOC
- Escape rate ≤ 5%
- Cycle time reduced 20%
- Rework ≤ 15%
- Test pass rate ≥ 95%
- Team satisfaction ≥ 8.0/10

**After 6 Months**:
- Defect density ≤ 0.5/KLOC (50% improvement)
- Escape rate ≤ 3%
- Cycle time reduced 30%
- Rework ≤ 10%
- All quality metrics green
- Team satisfaction ≥ 8.5/10

**After 1 Year** (Six Sigma Quality):
- Defect density ≤ 0.2/KLOC (3.4 per million LOC)
- Escape rate ≤ 1%
- Cycle time reduced 50%
- Rework ≤ 5%
- 100% SLO compliance sustained
- Team satisfaction ≥ 9.0/10

---

**Next Steps**:

1. **Set up metrics collection** (run `/scripts/collect_metrics.sh`)
2. **Create baseline** (record current metrics)
3. **Configure dashboard** (automated daily updates)
4. **Schedule reviews** (monthly Kaizen, quarterly strategic)
5. **Track trends** (measure improvement monthly)

**Remember**: What gets measured gets improved. Metrics drive continuous improvement.
