# How to Track Implementation Workflows

This guide shows how to use ggen's workflow analytics to monitor and optimize implementation processes.

---

## Overview

Workflow tracking captures real-world process execution, discovers patterns, and provides insights for optimization.

**Use cases:**
- University research implementation pipelines
- RevOps / GTM workflows
- Package maturity evolution
- Multi-stage approval processes
- Research paper to marketplace journey

---

## Step 1: Initialize Workflow Tracking

### Create a new workflow

```bash
ggen workflow init --name "university-research" --type research
```

Output:
```json
{
  "workflow_name": "university-research",
  "path": ".workflows/university-research.json",
  "status": "Workflow initialized - ready to track events"
}
```

This creates a workflow ready to receive events. The workflow file will automatically be created when you record the first event.

### Understanding workflow types

```bash
# Research implementations
ggen workflow init --name "paper-to-marketplace" --type research

# Package maturity tracking
ggen workflow init --name "maturity-progression" --type maturity

# Revenue operations
ggen workflow init --name "revops-pipeline" --type revops
```

Each type has predefined event types appropriate to that domain.

---

## Step 2: Record Workflow Events

### Track a single event

```bash
ggen workflow event \
  --workflow-file ".workflows/university-research.json" \
  --case-id "paper-2025-001" \
  --activity "PaperSubmitted" \
  --resource "researcher-jane"
```

### Track a complete process

```bash
# Paper submission phase
ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "PaperSubmitted" --resource "jane"

# Onboarding phase
ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "DepartmentOnboarded" --resource "admin"

# Implementation phase
ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "CodeGenerated" --resource "system"

ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "TestsRun" --resource "system"

ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "SecurityAudit" --resource "auditor"

# Release phase
ggen workflow event --workflow-file "workflow.json" \
  --case-id "paper-001" --activity "MarketplacePublished" --resource "admin"
```

### Batch recording from CSV

For high-volume event ingestion:

```bash
# events.csv
case_id,timestamp,activity,resource
paper-001,2025-11-01T10:00:00Z,PaperSubmitted,researcher-jane
paper-001,2025-11-02T14:30:00Z,DepartmentOnboarded,admin-bob
paper-001,2025-11-10T09:15:00Z,CodeGenerated,system
paper-002,2025-11-01T11:00:00Z,PaperSubmitted,researcher-alex
paper-002,2025-11-04T16:45:00Z,DepartmentOnboarded,admin-bob
```

Then import:
```bash
while IFS=',' read -r case activity resource timestamp; do
  [ "$case" = "case_id" ] && continue  # Skip header
  ggen workflow event \
    --workflow-file "workflow.json" \
    --case-id "$case" \
    --activity "$activity" \
    --resource "$resource"
done < events.csv
```

---

## Step 3: Analyze Workflow Statistics

### Get basic statistics

```bash
ggen workflow analyze --workflow-file ".workflows/university-research.json"
```

Output:
```json
{
  "workflow_name": "university-research",
  "total_cases": 12,
  "total_events": 156,
  "unique_activities": 10,
  "average_duration_minutes": 80640,
  "median_duration_minutes": 80640,
  "variant_count": 4,
  "most_common_variant": "PaperSubmitted→CodeGenerated→TestsRun→SecurityAudit→MarketplacePublished"
}
```

### Understanding the output

| Metric | Meaning | Use Case |
|--------|---------|----------|
| `total_cases` | Number of workflow executions (papers, packages, deals) | Throughput |
| `total_events` | Sum of all activities across all cases | Process complexity |
| `unique_activities` | How many different steps exist | Process breadth |
| `average_duration_minutes` | Mean time from start to finish | SLA planning |
| `median_duration_minutes` | Middle value duration | Typical case time |
| `variant_count` | How many different execution paths | Process variation |
| `most_common_variant` | Most frequently executed sequence | Happy path |

### Summary analysis

```bash
ggen workflow analyze --workflow-file "workflow.json" --summary
```

Use `--summary` flag for compact output when dealing with large workflows.

---

## Step 4: Discover Process Patterns

### Find the process graph

```bash
ggen workflow discover --workflow-file ".workflows/university-research.json"
```

Output:
```json
{
  "workflow_name": "university-research",
  "total_edges": 8,
  "pareto_edges": 5,
  "graph_mermaid": "graph TD\n...",
  "top_paths": [
    "Submitted → Onboarded → Pilot → Generated → Tests (92% frequency)",
    "Generated → Tests → Audit → Bench → Docs (85% frequency)",
    "Tests → Audit → Published (78% frequency)"
  ]
}
```

### Extract Mermaid diagram

```bash
ggen workflow discover --workflow-file "workflow.json" --export mermaid | \
  jq -r '.graph_mermaid' > process-diagram.mmd

# View in your IDE or convert to image
# (requires Mermaid CLI: npm install -g @mermaid-js/mermaid-cli)
mmdc -i process-diagram.mmd -o process-diagram.svg
```

### Focus on critical path (80/20)

```bash
ggen workflow discover --workflow-file "workflow.json" --pareto
```

This shows only the edges that account for 80% of process flow. Use this to focus optimization efforts.

### Key insights to look for

```bash
# Query the discovery results
ggen workflow discover --workflow-file "workflow.json" | jq '.top_paths'
```

Look for:
- **Common happy paths**: Frequent execution sequences (optimize these)
- **Bottlenecks**: Activities that slow down progress
- **Branches**: Variations where some cases diverge (investigate why)
- **Cycles**: Cases that loop (identify rework)

---

## Step 5: Optimize Based on Insights

### Example: University Research Pipeline

**Discovery shows:**
```
Current flow: PaperSubmitted → DeptOnboard (100%)
              ↓
              PilotStart (95%)
              ↓
              CodeGen (92%)
              ↓
              Tests (90%)
              ↓
              Audit (88%)
              ↓
              Published (85%)
```

**Insights:**
1. 5% of papers don't get past onboarding (issue?)
2. Drop from 95% to 92% in pilot start (resource constraint?)
3. Progressive drop suggests time/effort barriers

**Optimization actions:**
```bash
# 1. Analyze onboarding failures
ggen workflow analyze --workflow-file "workflow.json" --summary | \
  grep -A5 "DepartmentOnboarded"

# 2. Measure pilot start duration
ggen workflow analyze --workflow-file "workflow.json" | \
  jq '.average_duration_minutes'

# 3. Track which activities take longest
# (Custom analysis needed, track in attributes)

ggen workflow event \
  --workflow-file "workflow.json" \
  --case-id "paper-003" \
  --activity "PilotStarted" \
  --resource "admin-carol" \
  # Add custom attributes if supported
```

---

## Step 6: Generate Reports

### Full workflow report

```bash
ggen workflow report \
  --workflow-file ".workflows/university-research.json" \
  --format html \
  --output report.html

# View in browser
open report.html
```

### JSON report for programmatic analysis

```bash
ggen workflow report \
  --workflow-file "workflow.json" \
  --format json \
  --output report.json

# Process with jq
cat report.json | jq '.workflow_name, .statistics'
```

### Custom reports

```bash
# Combine multiple analyses
(
  echo "# Workflow Analysis Report"
  echo ""
  echo "## Statistics"
  ggen workflow analyze --workflow-file "workflow.json" | jq '.' | sed 's/^/    /'
  echo ""
  echo "## Process Graph"
  ggen workflow discover --workflow-file "workflow.json" | jq '.graph_mermaid' | sed 's/^/    /'
  echo ""
  echo "## Top Paths"
  ggen workflow discover --workflow-file "workflow.json" | jq '.top_paths[]' | sed 's/^/    - /'
) > analysis.md
```

---

## Real-World Example: University Research to Marketplace

### Complete workflow tracking

```bash
# Initialize
ggen workflow init --name "paper-to-marketplace" --type research

PAPER_ID="nature-biology-2025-001"
WORKFLOW_FILE=".workflows/paper-to-marketplace.json"

# Paper submission (Month 1)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "PaperSubmitted" \
  --resource "researcher-team"

# Department onboarding (Month 2)
sleep 30days  # In real time, of course
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "DepartmentOnboarded" \
  --resource "department-head"

# Code generation (Month 2-3)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "CodeGenerated" \
  --resource "ggen-system"

# Testing (Month 3)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "TestsRun" \
  --resource "qa-team"

# Security audit (Month 3-4)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "SecurityAudit" \
  --resource "security-team"

# Documentation & benchmarks (Month 4)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "DocumentationGenerated" \
  --resource "documentation-team"

# Release to marketplace (Month 4-5)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "MarketplacePublished" \
  --resource "admin"

# Track adoption (ongoing)
ggen workflow event --workflow-file "$WORKFLOW_FILE" \
  --case-id "$PAPER_ID" \
  --activity "AdoptionIncreased" \
  --resource "users"

# Analyze the full journey
ggen workflow analyze --workflow-file "$WORKFLOW_FILE"
```

Output after tracking 12 papers:
```json
{
  "total_cases": 12,
  "total_events": 108,
  "average_duration_minutes": 80640,  // ~8 weeks
  "unique_activities": 8,
  "variant_count": 3,
  "most_common_variant": "PaperSubmitted→DepartmentOnboarded→CodeGenerated→TestsRun→SecurityAudit→DocumentationGenerated→MarketplacePublished"
}
```

---

## Integration with RevOps

### Track deal progression

```bash
ggen workflow init --name "revops-pipeline" --type revops

ggen workflow event --workflow-file ".workflows/revops-pipeline.json" \
  --case-id "deal-2025-001" \
  --activity "SubscriptionStarted" \
  --resource "sales-rep"

ggen workflow event --workflow-file ".workflows/revops-pipeline.json" \
  --case-id "deal-2025-001" \
  --activity "QuarterlyReviewScheduled" \
  --resource "customer-success"

# ... more events ...

ggen workflow discover --workflow-file ".workflows/revops-pipeline.json" --pareto
```

### Measure sales velocity

```bash
ggen workflow analyze --workflow-file ".workflows/revops-pipeline.json" | \
  jq '{
    total_deals: .total_cases,
    avg_deal_duration_days: (.average_duration_minutes / 60 / 24),
    deals_closed: (.most_common_variant | split("→") | length)
  }'
```

---

## Troubleshooting

**Q: Events aren't showing up in analysis**
A: Ensure you're using the same `--workflow-file` path for all commands.

**Q: Variant count doesn't match my expectations**
A: Variants are exact sequences. Even one reordered event creates a different variant. Check order of events.

**Q: How do I clear a workflow and start over?**
A: Delete the workflow file:
```bash
rm .workflows/university-research.json
ggen workflow init --name "university-research" --type research
```

**Q: Can I import workflows from other systems?**
A: Yes, format your data as WorkflowEvent JSON and populate the workflow file programmatically.

