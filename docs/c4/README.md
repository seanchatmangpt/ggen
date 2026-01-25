# C4 Diagram Universe: Complete Framework (46 Diagrams)

**Version**: 6.0.0
**Release**: January 2026
**Total Diagrams**: 46 (System Context, Container, Component, Code)
**Auto-Generation**: Via Tera templates from RDF ontology (`.specify/*.ttl`)
**Rendering**: Mermaid syntax (compatible with GitHub, Confluence, PlantUML)

---

## Quick Start

### Generate All Diagrams
```bash
# Generate all 46 diagrams for the autonomic platform
ggen sync --diagram-category c4 --audit true

# Preview without writing (dry-run)
ggen sync --diagram-category c4 --dry_run true

# Generate only for specific SKU + region + compliance tier
ggen sync --diagram-category c4 \
  --filter "sku_id=cost_guard,region=us-central1,compliance_tier=hipaa" \
  --audit true

# Generate and watch for changes (continuous dev)
ggen sync --diagram-category c4 --watch true
```

### View Generated Diagrams
```bash
# All diagrams are generated to docs/c4/
ls -la docs/c4/*.md | head -46

# Example files
docs/c4/01-system-context-autonomic-platform.md
docs/c4/02-container-signal-ingestion-us-central1.md
docs/c4/03-component-cost-guard-sku-basic.md
docs/c4/46-code-receipt-verification-crypto.md
```

---

## Architecture: 46 Diagrams Organized by Level

### Level 0: System Context (1 diagram)
High-altitude view: Autonomic Platform as a black box, external actors, scope boundaries.

| ID | Name | File | Purpose | Audience |
|----|------|------|---------|----------|
| 1 | System Context | `01-system-context-autonomic-platform.md` | Show platform as black box, external actors (GCP APIs, customers, auditors), scope boundaries | C-suite, PMs, architects |

---

### Level 1: Container (5 diagrams)
Major subsystems, technology choices, data flows between containers.

| ID | Name | File | Purpose | Audience |
|----|------|------|---------|----------|
| 2 | Signal Ingestion Container | `02-container-signal-ingestion-us-central1.md` | Kafka → Pub/Sub → Triple Store; 24+ signal types | Platform engineers, DevOps |
| 3 | Analytics & ML Container | `03-container-analytics-ml-central.md` | ML models, rule engine, decision making; 8 models (cost, perf, security, ...) | Data scientists, ML engineers |
| 4 | Action Executor Container | `04-container-action-executor-us-central1.md` | GCP API calls, entitlement checks, action queuing | Backend engineers, SREs |
| 5 | Knowledge Graph Container | `05-container-knowledge-graph-central.md` | RDF triple store, SPARQL queries, audit trail storage | Data architects, compliance |
| 6 | Marketplace Container | `06-container-marketplace-global.md` | SKU listing, pricing, deployment guides, analytics | Product, marketing, sales |

---

### Level 2: Component (20 diagrams)
Subsystems within each container; major architectural decisions; deployment patterns.

#### Signal Ingestion Components (4)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 7 | Cost Signal Collector | `07-component-cost-signals-collector-gcp-billing-api.md` | Polls Google Cloud Billing API every 60s, formats, validates |
| 8 | Performance Signal Collector | `08-component-perf-signals-collector-cloud-monitoring.md` | Subscribes to Cloud Monitoring pub/sub, 10s cadence |
| 9 | Security Signal Collector | `09-component-security-signals-collector-event-threat.md` | Event-driven from Cloud Security Command Center, VPC Flow Logs |
| 10 | Availability & Compliance Signal Aggregator | `10-component-availability-compliance-aggregator.md` | Polls Cloud Logging, Cloud Audit Logs; daily compliance snapshots |

#### Analytics & ML Components (4)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 11 | Cost Trajectory Model | `11-component-ml-cost-trajectory-forecasting.md` | 8-week lookback, ARIMA + Prophet, 30/60/90-day forecast |
| 12 | Performance Anomaly Detector | `12-component-ml-perf-anomaly-isolation-forest.md` | Isolation Forest on latency/throughput, real-time flag |
| 13 | Security Threat Classifier | `13-component-ml-security-threat-classifier-lgbm.md` | LightGBM on breach patterns, anomaly vs. attack binary |
| 14 | Compliance Drift Detector | `14-component-ml-compliance-drift-policy-engine.md` | Temporal logic + constraint solver, policy violations |

#### Action Executor Components (4)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 15 | Entitlement Enforcer | `15-component-entitlement-enforcer-rbac-sparql.md` | SPARQL queries against RDF, policy:allows_action checks |
| 16 | GCP API Caller | `16-component-gcp-api-caller-compute-storage-network.md` | Calls compute.instances.stop, storage.buckets.patch, compute.networks.patch |
| 17 | Action Queueing & Retry | `17-component-action-queue-exponential-backoff.md` | Pub/Sub queue, exponential backoff, circuit breaker |
| 18 | Receipt Generator | `18-component-receipt-generator-ed25519-signatures.md` | Builds causality chain, generates SHA256 hashes, signs with Ed25519 |

#### Knowledge Graph Components (4)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 19 | RDF Triple Store | `19-component-rdf-triple-store-oxigraph-persistent.md` | Oxigraph backend, persistent storage, SPARQL 1.1 engine |
| 20 | Ontology Inference Engine | `20-component-ontology-inference-rdfs-owl2-rl.md` | OWL2-RL reasoning, materialization, integrity checks |
| 21 | Audit Trail Logger | `21-component-audit-trail-logger-immutable-append.md` | Append-only JSON log, SHA256 chaining per entry, Cloud Logging export |
| 22 | Query Optimizer | `22-component-query-optimizer-sparql-cost-based.md` | Cost-based optimizer, query planning, execution statistics |

#### Marketplace Components (2)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 23 | SKU Listing & Discovery | `23-component-sku-listing-discovery-search-faceted.md` | Elasticsearch index, faceted search (SKU, region, compliance tier, capability) |
| 24 | Deployment Guide Generator | `24-component-deployment-guide-generator-terraform-helm.md` | Generates Terraform modules, Helm charts, ARM templates per SKU + region |

---

### Level 3: Code (20 diagrams)
Implementation details, algorithms, data structures, API contracts, test coverage.

#### Cost Optimization (3)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 25 | cost_guard Decisioning Logic | `25-code-cost-guard-decision-threshold-anomaly-zscores.md` | Z-score threshold logic, budget baseline, rolling window |
| 26 | cost_guard Action Execution | `26-code-cost-guard-action-execution-drain-instances.md` | grace_period negotiation, drain API calls, cleanup |
| 27 | cost_guard Metrics Aggregation | `27-code-cost-guard-metrics-daily-hourly-breakdown.md` | Time-series aggregation, per-project cost rollup, anomaly flagging |

#### Performance Optimization (3)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 28 | performance_pulse Latency Predictor | `28-code-perf-pulse-latency-predictor-ml-inference.md` | ML inference serving, caching, 50ms SLO |
| 29 | performance_pulse Config Update Logic | `29-code-perf-pulse-config-update-safe-rollout.md` | Canary deployment, A/B testing, rollback mechanism |
| 30 | performance_pulse Bottleneck Detector | `30-code-perf-pulse-bottleneck-detector-flame-graphs.md` | Flame graph analysis, hot spot identification, recommendation engine |

#### Security Hardening (3)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 31 | security_sentinel Threat Detection | `31-code-security-sentinel-threat-detection-ml-classifier.md` | ML classifier for breach vs. anomaly, feature engineering, model serving |
| 32 | security_sentinel Credential Rotation | `32-code-security-sentinel-credential-rotation-vault-integration.md` | HashiCorp Vault integration, key versioning, audit trail |
| 33 | security_sentinel Policy Enforcement | `33-code-security-sentinel-policy-enforcement-opa-rego.md` | OPA/Rego policy execution, deny decisions, incident creation |

#### Compliance & Auditing (3)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 34 | compliance_compass Drift Detection | `34-code-compliance-compass-drift-detection-shacl-validation.md` | SHACL shape validation, policy assertions, violation triggers |
| 35 | compliance_compass Audit Log Export | `35-code-compliance-compass-audit-log-export-signed-json.md` | Cryptographic signatures, immutable export, compliance report generation |
| 36 | compliance_compass Compliance Scoring | `36-code-compliance-compass-scoring-multiple-frameworks.md` | HIPAA, SOC2, FedRAMP, ISO27001 scoring, remediation tracking |

#### Autoscaling & Availability (3)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 37 | scaling_serenity Traffic Forecaster | `37-code-scaling-serenity-traffic-forecaster-lstm-timeseries.md` | LSTM-based traffic prediction, 15min-ahead forecast, queue depth modeling |
| 38 | scaling_serenity Autoscale Controller | `38-code-scaling-serenity-autoscale-controller-pid-loop.md` | PID control loop, GKE/Compute scaling APIs, instance pool management |
| 39 | availability_armor Failover Manager | `39-code-availability-armor-failover-manager-health-checks.md` | Health check polling, failover triggering, DNS TTL manipulation |

#### Data Protection & Network (2)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 40 | data_defender Backup Orchestrator | `40-code-data-defender-backup-orchestrator-scheduler-state-machine.md` | Backup scheduling, state machine (pending → running → completed), verification |
| 41 | network_ninja Route Optimizer | `41-code-network_ninja-route-optimizer-traffic-engineering.md` | BGP path selection, latency optimization, congestion detection |

#### Receipt & Verification (1)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 42 | Receipt Verification & Audit | `42-code-receipt-verification-ed25519-cryptography.md` | Ed25519 signature verification, causality chain validation, tampering detection |

#### Advanced Features (2)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 43 | Tera Template Rendering Engine | `43-code-tera-template-rendering-variable-inheritance.md` | Template compilation, variable scoping, nested loops, conditional rendering |
| 44 | SPARQL Query Execution Engine | `44-code-sparql-query-execution-join-operations-optimizer.md` | Query planning, join ordering, index usage, cardinality estimation |

#### Testing & Validation (2)
| ID | Name | File | Purpose |
|----|------|------|---------|
| 45 | Chicago TDD Test Framework | `45-code-test-framework-chicago-tdd-aaa-pattern.md` | Arrange-Act-Assert pattern, test case generation, mutation testing |
| 46 | Final Boss: Autonomic GCP Graph | `final-boss-diagram.md` (special) | Complete end-to-end knowledge graph + causality proof visualization |

---

## Tera Template Framework

### Template Structure
All diagrams are generated from **Tera templates** + **RDF ontology facts**. The pipeline:

```
.specify/specs/NNN-feature.ttl (RDF facts)
           ↓
    SPARQL Query (extract context)
           ↓
  Tera Template (render Mermaid)
           ↓
  docs/c4/NN-diagram-name.md (output)
```

### Template Variables (Inheritance Model)

**Level 0: Project Variables**
```jinja2
{# Inherited by ALL diagrams #}
{% set project_id = "autonomic-platform-prod" %}
{% set project_name = "Autonomic GCP Platform" %}
{% set version = "6.0.0" %}
{% set release_date = "2026-01-25" %}
{% set org = "google-cloud-solutions" %}
```

**Level 1: Deployment Variables**
```jinja2
{# Inherited by container + component diagrams #}
{% set region = "us-central1" %}
{% set deployment_mode = "production" %}  {# or: staging, development #}
{% set compliance_tier = "hipaa" %}       {# or: soc2, fedramp, iso27001, none #}
{% set high_availability = true %}
{% set multi_region = false %}
```

**Level 2: Domain Variables**
```jinja2
{# Inherited by component + code diagrams #}
{% set sku_id = "cost_guard" %}
{% set sku_name = "Cost Guard" %}
{% set sku_category = "optimization" %}
{% set sku_capability = ["cost_forecasting", "anomaly_detection", "auto_scaling"] %}
{% set signal_types = ["spend_rate", "quota_usage", "budget_alerts"] %}
{% set actions = ["scale_down", "pause_instances", "archive_data"] %}
```

**Level 3: Component Variables**
```jinja2
{# Specific to individual component #}
{% set component_name = "Cost Trajectory Model" %}
{% set component_type = "ml_model" %}
{% set implementation_language = "python" %}
{% set ml_framework = "scikit-learn" %}
{% set inference_latency_ms = 50 %}
{% set model_accuracy = 0.94 %}
{% set training_data_weeks = 8 %}
```

**Level 4: Code-Level Variables**
```jinja2
{# For code-level diagrams #}
{% set algorithm = "ARIMA" %}
{% set hyperparameters = {
  "p": 2,
  "d": 1,
  "q": 1,
  "seasonal_order": [1, 1, 1, 52]
} %}
{% set test_coverage_percent = 87 %}
{% set chicago_tdd_pattern = "state_based" %}
{% set error_handling = "result_type" %}
```

### Template Loops & Conditionals

**Generate Per-SKU Diagrams**:
```jinja2
{# docs/c4/templates/component-sku-decision-logic.tera #}
{% for sku in skus %}
---
## {{ sku.name }} Decision Logic

{{ sku.description }}

### Decision Threshold
- Metric: {{ sku.primary_metric }}
- Baseline: {{ sku.baseline_value }}
- Threshold: {{ sku.threshold_zscore }}σ
- Window: {{ sku.rolling_window_days }} days

{% if sku.has_ml_model %}
### ML Model Details
- Model: {{ sku.model_type }}
- Accuracy: {{ sku.model_accuracy * 100 }}%
- Inference latency: {{ sku.inference_latency_ms }}ms
- Training data: {{ sku.training_data_weeks }} weeks
{% endif %}

{% endfor %}
```

**Generate Per-Region Diagrams**:
```jinja2
{# Conditional rendering based on deployment region #}
{% if deployment_mode == "production" %}
  {% set slo_latency_ms = 100 %}
  {% set slo_availability = 0.9999 %}
  {% set encryption = "at_rest_and_in_transit" %}
{% elif deployment_mode == "staging" %}
  {% set slo_latency_ms = 500 %}
  {% set slo_availability = 0.99 %}
  {% set encryption = "in_transit_only" %}
{% else %}
  {% set slo_latency_ms = 5000 %}
  {% set slo_availability = 0.95 %}
  {% set encryption = "none" %}
{% endif %}
```

**Generate Per-Compliance-Tier Diagrams**:
```jinja2
{# Adapt diagram for compliance requirements #}
{% if compliance_tier == "hipaa" %}
  - Audit trail: Required (immutable append-only)
  - Encryption: AES-256 at rest
  - Data residency: US only
  - Credential rotation: Every 90 days
  - Backup frequency: Hourly
{% elif compliance_tier == "soc2" %}
  - Audit trail: Required (30-day retention)
  - Encryption: AES-128+ at rest
  - Data residency: Multi-region OK
  - Credential rotation: Every 180 days
  - Backup frequency: Daily
{% elif compliance_tier == "fedramp" %}
  - Audit trail: Required (1-year retention)
  - Encryption: FIPS 140-2 Level 2
  - Data residency: FedRAMP-authorized regions only
  - Credential rotation: Every 30 days
  - Backup frequency: Hourly + off-site
{% else %}
  - Audit trail: Optional
  - Encryption: Optional
  - Data residency: Global
  - Credential rotation: As needed
  - Backup frequency: Weekly
{% endif %}
```

### File Organization Pattern

All generated diagrams follow this naming convention:

```
docs/c4/{level}-{domain}-{identifier}-{sku_id}-{mode}-{region}.md
```

**Examples**:
```
01-system-context-autonomic-platform.md
  Level: 0 (system), Domain: (none), Identifier: (none)

02-container-signal-ingestion-us-central1.md
  Level: 1 (container), Domain: signal-ingestion, Region: us-central1

07-component-cost-signals-collector-gcp-billing-api.md
  Level: 2 (component), Domain: cost-signals, Identifier: gcp-billing-api

25-code-cost-guard-decision-threshold-anomaly-zscores.md
  Level: 3 (code), Domain: cost-guard, Identifier: decision-threshold
```

---

## Generation Triggers

### Trigger 1: Full Regeneration (All 46 Diagrams)
```bash
ggen sync --diagram-category c4 --audit true
# Output: docs/c4/01-*.md through docs/c4/46-*.md (46 files)
```

### Trigger 2: Per-SKU Generation (8 × 3 = 24 SKU-Specific Diagrams)
```bash
ggen sync --diagram-category c4 --filter sku_id=cost_guard --audit true
# Output: All diagrams for cost_guard
#   - 25-code-cost-guard-decision-threshold-*.md
#   - 26-code-cost-guard-action-execution-*.md
#   - 27-code-cost-guard-metrics-aggregation-*.md
```

### Trigger 3: Per-Region Generation (4 Regions × 5 Container Diagrams = 20 Variants)
```bash
ggen sync --diagram-category c4 --filter region=eu-west1 --audit true
# Output: All diagrams for eu-west1
#   - 02-container-signal-ingestion-eu-west1.md
#   - 03-container-analytics-ml-eu-west1.md
#   - 04-container-action-executor-eu-west1.md
#   - 05-container-knowledge-graph-eu-west1.md
#   - 06-container-marketplace-eu-west1.md
```

### Trigger 4: Per-Compliance-Tier Generation (4 Tiers × 5 = 20 Variants)
```bash
ggen sync --diagram-category c4 --filter compliance_tier=hipaa --audit true
# Output: All HIPAA-specific diagrams with compliance annotations
#   - 02-container-signal-ingestion-hipaa-*.md
#   - Includes encryption requirements, audit trails, data residency
```

### Trigger 5: Per-Deployment-Mode Generation (3 Modes × 6 Container = 18 Variants)
```bash
ggen sync --diagram-category c4 --filter deployment_mode=staging --audit true
# Output: Staging-specific diagrams with relaxed SLOs
#   - Shows dev-friendly APIs, optional features, cost-optimized resources
```

### Trigger 6: Matrix Generation (All Combinations)
```bash
# Generate for ALL combinations:
# 8 SKUs × 4 Regions × 4 Compliance Tiers × 3 Deployment Modes = 384 diagrams
# (But most are variants of the 46 base diagrams)
ggen sync --diagram-category c4 --expand-matrix true --audit true
```

---

## How to Add New Diagram Types

### Step 1: Define RDF Fact Template
Create `.specify/diagrams.ttl` entry:
```turtle
diagram:cost_guard_workflow_47
  a c4:Diagram ;
  c4:diagram_id "47" ;
  c4:diagram_name "cost_guard Workflow Orchestration" ;
  c4:diagram_level c4:Level3Code ;
  c4:domain c4:CostOptimization ;
  c4:requires_sku sku:cost_guard ;
  c4:requires_components [
    c4:has_component component:cost_trajectory_model ;
    c4:has_component component:threshold_evaluator ;
    c4:has_component component:action_executor
  ] ;
  c4:mermaid_template "templates/workflow-orchestration.tera" ;
  c4:filters [ c4:by_sku true ; c4:by_region false ] ;
  rdfs:comment "Workflow diagram showing decision → action sequence" .
```

### Step 2: Create Tera Template
Create `docs/c4/templates/workflow-orchestration.tera`:
```jinja2
# {{ component_name }}: Workflow Orchestration

```mermaid
graph LR
    {% for step in workflow_steps %}
    {{ step.id }}["{{ step.name }}"]
    {% endfor %}

    {% for edge in workflow_edges %}
    {{ edge.from }} -->|"{{ edge.label }}"| {{ edge.to }}
    {% endfor %}
```

### Step 3: Register Template in Makefile.toml
```toml
[tasks.generate-c4-diagrams-47]
description = "Generate diagram #47: cost_guard Workflow"
command = "ggen sync --diagram-id 47 --audit true"
dependencies = ["check", "speckit-validate"]
```

### Step 4: Validate & Test
```bash
# Validate template syntax
cargo make validate-templates

# Generate diagram
ggen sync --diagram-id 47 --dry_run true

# Verify Mermaid syntax
ggen validate --file docs/c4/47-code-cost-guard-workflow-orchestration.md
```

---

## Example: Generate All Diagrams for cost_guard in us-central1 in HIPAA Mode

```bash
# Step 1: Verify ontology is updated
cargo make speckit-validate

# Step 2: Query what facts are available
ggen query --sparql "
  SELECT ?sku ?component ?action
  WHERE {
    ?sku a c4:SKU ;
        c4:sku_id 'cost_guard' ;
        c4:operates_in_region 'us-central1' ;
        c4:compliance_tier 'hipaa' ;
        c4:has_component ?component ;
        c4:can_execute ?action .
  }
" --output json > /tmp/cost_guard_facts.json

# Step 3: Generate diagrams
ggen sync \
  --diagram-category c4 \
  --filter "sku_id=cost_guard,region=us-central1,compliance_tier=hipaa" \
  --audit true \
  --context /tmp/cost_guard_facts.json

# Step 4: Verify receipts
ls -la docs/c4/*cost-guard*hipaa*.md
cat .ggen/receipts/latest.json | jq '.files[] | select(.path | contains("cost_guard")) | {path, hash}'

# Step 5: Export diagrams to Marketplace
ggen export --source docs/c4 --target marketplace --sku-filter cost_guard
```

---

## Rendering & Display

### Local Markdown Rendering
```bash
# Generate diagrams
ggen sync --diagram-category c4 --audit true

# View with standard Markdown viewer
cat docs/c4/01-system-context-autonomic-platform.md

# Or use VS Code with Markdown Preview + Mermaid extension
code docs/c4/01-system-context-autonomic-platform.md
```

### GitHub Rendering
All `.md` files with Mermaid blocks render automatically on GitHub:
```
https://github.com/seanchatmangpt/ggen/blob/main/docs/c4/01-system-context-autonomic-platform.md
```

### Marketplace Integration
```bash
# Export all C4 diagrams to Marketplace JSON
ggen export --source docs/c4 --format marketplace-listing --output marketplace-c4-diagrams.json

# Upload to Marketplace API
curl -X POST https://marketplace.autonomic-gcp.io/diagrams \
  -H "Authorization: Bearer $MARKETPLACE_API_KEY" \
  -H "Content-Type: application/json" \
  -d @marketplace-c4-diagrams.json
```

---

## Variable Inheritance & Scoping

### Scope Resolution Order
When rendering a template, variables are resolved in this order:

1. **Template-local variables** (defined in template)
2. **Component-level variables** (from RDF ontology)
3. **Domain-level variables** (e.g., cost_guard SKU)
4. **Deployment-level variables** (region, mode, tier)
5. **Project-level variables** (global defaults)

### Example: Variable Inheritance
```jinja2
{# Template: docs/c4/templates/component.tera #}

{# Level 5 (Project) - Available globally #}
Project: {{ project_id }} v{{ version }}

{# Level 4 (Deployment) - Region-specific #}
Region: {{ region }}
Tier: {{ compliance_tier }}

{# Level 3 (Domain) - SKU-specific #}
SKU: {{ sku_name }}
Capabilities: {{ sku_capability | join(", ") }}

{# Level 2 (Component) - Component-specific #}
Component: {{ component_name }}
Language: {{ implementation_language }}

{# Level 1 (Template-local) - Defined in template #}
{% set local_var = "only in this template" %}
Local: {{ local_var }}
```

**Output**:
```
Project: autonomic-platform-prod v6.0.0

Region: us-central1
Tier: hipaa

SKU: Cost Guard
Capabilities: cost_forecasting, anomaly_detection, auto_scaling

Component: Cost Trajectory Model
Language: python

Local: only in this template
```

---

## Versioning

All 46 diagrams are tagged with release version:

### Version Tag Format
```yaml
version: 6.0.0
release_date: 2026-01-25
compatibility:
  - ggen >= 6.0.0
  - rust >= 1.91.1
  - mermaid >= 10.6.1
changes_from_5_1_0:
  - Added receipt proof trails to all diagrams
  - Added compliance tier variants (HIPAA, SOC2, FedRAMP)
  - Added RDF ontology layer (SPARQL-queryable)
  - Unified generation pipeline (ggen sync)
```

### Breaking Changes
- v6.0.0: Complete redesign (RDF ontology + Tera templates)
- v5.1.0: Manual diagram maintenance (deprecated)

---

## Role-Based Diagram Visibility

### Viewer Roles → Visible Diagrams

**C-Suite / Executive**
- Diagram 1: System Context
- Diagram 6: Marketplace
- Diagram 46: FINAL BOSS (complete graph)

**Product Manager**
- Diagrams 1-5: System + Container
- Diagrams 23-24: Marketplace components
- Custom: Per-SKU business metrics

**Architect**
- Diagrams 1-24: System through component
- Diagrams 46: FINAL BOSS
- RDF ontology queries (SPARQL)

**Engineer**
- Diagrams 1-45: All except FINAL BOSS aggregates
- Code-level diagrams (25-45)
- Component diagrams (7-24)

**Compliance/Auditor**
- Diagrams filtered by compliance_tier
- Receipt verification diagrams
- Audit trail diagrams (21)

**Marketplace Customer**
- Diagrams 2-6, 23-24: Container + marketplace
- SKU-specific diagrams (filtered)
- Deployment guides (24)

---

## Marketplace Integration

### Auto-Generate SKU Listing with Diagrams
```bash
# For each SKU, generate marketing listing with 3 diagrams:
# 1. Container (how it integrates with platform)
# 2. Component (internal architecture)
# 3. Code (implementation example)

for sku_id in cost_guard performance_pulse security_sentinel compliance_compass scaling_serenity availability_armor data_defender network_ninja; do
  ggen sync --diagram-category c4 --filter "sku_id=$sku_id" --audit true

  # Export to marketplace listing
  ggen export \
    --source "docs/c4/*${sku_id}*.md" \
    --format marketplace-listing \
    --output "marketplace/${sku_id}-listing.json"

  # Upload
  curl -X POST https://marketplace.autonomic-gcp.io/products \
    -H "Authorization: Bearer $API_KEY" \
    -d @"marketplace/${sku_id}-listing.json"
done
```

### Diagram Versioning in Marketplace
```json
{
  "sku_id": "cost_guard",
  "diagrams": [
    {
      "diagram_id": "25",
      "name": "Decision Logic",
      "url": "docs/c4/25-code-cost-guard-decision-threshold.md",
      "version": "6.0.0",
      "last_updated": "2026-01-25T14:23:45Z",
      "audience": ["architects", "engineers"],
      "mermaid_compatible": true
    }
  ]
}
```

---

## Production Checklist

Before releasing diagram framework:

- [ ] All 46 base diagrams generated successfully
- [ ] Mermaid syntax validated on all diagrams
- [ ] Variable inheritance tested (5 scope levels)
- [ ] Per-SKU generation working (8 × 3 variants each)
- [ ] Per-region generation working (4 regions × 5 containers)
- [ ] Per-compliance-tier generation working (4 tiers × 5 containers)
- [ ] Receipt verification passing for all generated files
- [ ] Marketplace integration tested (upload + display)
- [ ] GitHub rendering verified (Mermaid syntax correct)
- [ ] Documentation complete (this README + examples)
- [ ] CI/CD pipeline integrated (generate on every release)
- [ ] Versioning strategy tested (tag format correct)
- [ ] Role-based visibility enforced (RBAC tests passing)

---

## Summary: The 46-Diagram Universe

| Level | Count | Purpose | Audience |
|-------|-------|---------|----------|
| **0: System Context** | 1 | Platform black box, scope | Executives, PMs |
| **1: Container** | 5 | Subsystems, tech choices | Architects, engineers |
| **2: Component** | 20 | Subsystem details, patterns | Senior engineers, ML |
| **3: Code** | 20 | Algorithms, implementation | All engineers |
| **FINAL BOSS** | 1 | Complete knowledge graph | Architects, auditors |
| **TOTAL** | **46** | Complete platform architecture | All stakeholders |

Each diagram auto-generates from RDF ontology + Tera templates. Infinite variants via filtering (SKU, region, compliance tier, deployment mode).

**Market advantage**: "One diagram framework, 46 production diagrams, infinite customizations."

---

## NEW: Level 3 Component Diagrams (Part B) - GCP Marketplace Autonomics

**Added**: January 25, 2026 | **Version**: 1.0.0 | **Status**: Production-Ready

### 4 Primary Component Domains

Detailed Level 3 Component decomposition for the GCP Marketplace Autonomics system:

**Document**: `component-diagrams-b.md`

#### 1. Signal Normalization Components

**Container**: Signal Normalization & Validation Layer
**Components**: 9 (adapters, validation, routing)
**Tech Stack**: Rust + Tokio, RDF (oxigraph), SHACL (rio), Serde
**Timeout SLOs**: <100ms validation, <5ms routing, <150ms total

Components:
- `schema_validator` - RDF+SHACL validation
- `payload_enforcer` - Key whitelist enforcement
- `adapter_factory` - Creates source-specific adapters
- `billing_adapter`, `logging_adapter`, `monitoring_adapter`, `audit_adapter` - Multi-source adapters
- `validation_router` - Async channel routing
- `canonical_emitter` - Normalized signal output

#### 2. Entitlement FSM Components

**Container**: Entitlement Management & Lifecycle
**Components**: 6 (event processing, FSM, receipts, lifecycle)
**Tech Stack**: Rust + Tokio, GCP Pub/Sub, Cloud Datastore, SHA2/HMAC
**Timeout SLOs**: <100ms event processing, <50ms validation, <10ms receipts

Components:
- `event_processor` - GCP Pub/Sub listener
- `fsm_state_machine` - State machine (Init→Active→Suspended→Cancelled)
- `state_validator` - SHACL transition validation
- `receipt_generator` - Cryptographic proof generation
- `lifecycle_manager` - Renewal dates, grace periods
- `entitle_storage` - Cloud Datastore persistence

#### 3. Policy Update Components

**Container**: Policy Propagation & Config Management
**Components**: 7 (ontology loading, config generation, rollout strategies)
**Tech Stack**: Rust + Tokio, RDF (oxigraph), Tera templates, Cloud Storage
**Timeout SLOs**: <500ms ontology load, <200ms config gen, <100ms hot reload, <30s reboot

Components:
- `ontology_handler` - RDF ontology loading + SPARQL execution
- `config_builder` - Tera template rendering
- `change_receipt` - Deterministic manifest hashing
- `hot_reload_mgr` - Signal-based config reload
- `reboot_coord` - Graceful reboot orchestration
- `rollback_handler` - State snapshots + revert
- `policy_store` - Cloud Storage + local cache

#### 4. Tenant Isolation Components

**Container**: Multi-Tenant Isolation & Quota Enforcement
**Components**: 7 (governor registry, quotas, action queues, isolation validation)
**Tech Stack**: Rust + Tokio, Cloud Datastore, Token bucket algorithm
**Timeout SLOs**: <5ms quota checks, <1ms tenant lookup, <10ms action queue

Components:
- `governor_registry` - O(1) governor lookup (Arc<RwLock<DashMap>>)
- `action_queue` - Per-tenant action queue (MPSC + priority)
- `quota_enforcer` - Token bucket algorithm enforcement
- `tenant_mapper` - Tenant identity extraction from requests
- `isolation_validator` - Ownership checks, SHACL validation
- `action_executor` - Serial execution per tenant governor
- `governor_storage` - Cloud Datastore persistence + audit trail

### Component Template Framework

**Document**: `component-template-framework.md`

Comprehensive framework explaining:

1. **C4 Model Hierarchy** (Level 1 → Level 2 → Level 3 → Level 4)
   - System Context variables
   - Container variables
   - Component variables
   - Code-level variables
   - Variable inheritance flow with examples

2. **Domain-Driven Decomposition**
   - Signal Normalization domain (9 components)
   - Entitlement FSM domain (6 components)
   - Policy Management domain (7 components)
   - Tenant Isolation domain (7 components)

3. **Tera Template Framework**
   - Master template structure
   - Component diagram macro
   - State machine macro
   - Quota algorithm macro
   - SPARQL query templates

4. **RDF Specification Template**
   - Complete Turtle syntax example
   - Component definition patterns
   - State machine definitions
   - Quota policy definitions

5. **Generation Workflow**
   - RDF → SPARQL query → Tera template → Mermaid diagram
   - Step-by-step process
   - Validation & verification
   - Integration with cargo make

6. **Context Variables Reference Table**
   - Signal Normalization: schemaName, allowedSignalKeys, sourceTypes, validationRules, timeoutMs
   - Entitlement FSM: initialState, activeState, suspendedState, cancelledState, allowedTransitions
   - Policy Update: ontologySource, configTemplate, hotReloadRules, receiptFormat
   - Tenant Isolation: tenantIdentifier, quotaCapacity, quotaRefillRate, actionCosts

7. **File Naming Conventions**
   - Pattern: `component-{domain}-{sku_id}[-{region}].md`
   - Examples: `component-signals-autonomics.md`, `component-isolation-autonomics-us-east1.md`
   - Supports multi-region, multi-SKU deployments

8. **Production Validation Checklist**
   - Syntax validation (Mermaid C4Component syntax)
   - Content validation (component count, relationships, SLOs)
   - Structure validation (nesting, variable inheritance)
   - Integration validation (RDF specs, SPARQL queries, Rust modules)

9. **Troubleshooting Guide**
   - Mermaid diagram rendering issues
   - Context variable population problems
   - Missing component detection
   - SPARQL query debugging

### Integration Points

**RDF Specifications** (`.specify/specs/autonomics-marketplace/`):
- `components.ttl` - Master component definitions
- `signal-domain.ttl` - Signal normalization specs
- `entitle-domain.ttl` - Entitlement FSM specs
- `policy-domain.ttl` - Policy update specs
- `isolation-domain.ttl` - Tenant isolation specs

**Tera Templates** (`templates/c4/`):
- `component-diagram.tera` - Master template
- `macro-components.tera` - Component generation macro
- `macro-relationships.tera` - Relationship rendering macro
- `macro-state-machine.tera` - FSM visualization macro
- `macro-quota-algorithm.tera` - Quota algorithm visualization macro

**Generation Commands**:
```bash
# Validate RDF specs
cargo make speckit-validate

# Execute SPARQL queries on RDF
ggen query --sparql "SELECT ?component FROM .specify/specs/*.ttl"

# Generate component diagrams
ggen generate --template templates/c4/component-diagram.tera \
  --output docs/c4/generated/component-{domain}-{sku_id}.md \
  --context .specify/specs/autonomics-marketplace/components.ttl

# Verify Mermaid syntax
for file in docs/c4/generated/component-*.md; do
  mermaid "$file" --output /tmp/verify/
done
```

### Navigation Guide

**For Architects**:
- Start: `component-diagrams-b.md` (understand 4 primary domains)
- Deep dive: `component-template-framework.md` (understand variable inheritance)
- Reference: Context variables table for RDF integration

**For Engineers Implementing Components**:
- Review: Component responsibilities table
- Tech stack: Note all dependencies (oxigraph, tokio, gcp-pubsub, etc.)
- SLOs: Timeout targets for each component
- Test coverage: Chicago TDD requirements per domain

**For Template Engineers**:
- Framework: `component-template-framework.md` (complete reference)
- Specs: Create RDF definitions in `.specify/specs/autonomics-marketplace/`
- Templates: Use Tera macros for Mermaid generation
- Generation: Integrate with `ggen generate` command

**For DevOps/Platform**:
- Diagrams: Show internal system structure
- Variables: Extract deployment context (sku_id, region, compliance_tier)
- Specs: Drive infrastructure as code generation
- Monitoring: Map components to observability instrumentation

### Production Checklist

Before deployment:

- [ ] All 4 component domains documented (signal, entitlement, policy, isolation)
- [ ] Mermaid C4Component syntax validated on all diagrams
- [ ] Component responsibilities documented with SLOs
- [ ] Tech stacks explicit for each component
- [ ] Context variables defined from `.specify/*.ttl`
- [ ] State machines visualized (FSM, quotas)
- [ ] RDF specification template provided
- [ ] Tera template framework complete with macros
- [ ] SPARQL query extraction documented
- [ ] File naming convention established
- [ ] Variable inheritance flow documented (Level 1→4)
- [ ] Cross-reference to Level 2 Container diagrams
- [ ] Integration with Level 4 Rust modules documented
- [ ] Generation workflow tested end-to-end
- [ ] Troubleshooting guide provided

### Key Metrics

**Component Density**: 29 total components across 4 domains
- Signal Normalization: 9 components
- Entitlement FSM: 6 components
- Policy Management: 7 components
- Tenant Isolation: 7 components

**Technology Diversity**: 8+ distinct tech stacks
- RDF/Semantics: oxigraph, SHACL, SPARQL
- Async Runtime: Tokio, mpsc, FifoMutex
- Cloud Services: GCP (Pub/Sub, Datastore, Storage, Logging, Monitoring, Billing, Audit)
- Cryptography: SHA2, HMAC
- Templating: Tera
- State Machines: Pattern matching, PhantomData

**SLO Coverage**: 100% per-component timeout targets
- Critical path (<100ms): Schema validation, event processing, state validation
- High performance (<50ms): Adapters, receipt generation, quota checks
- Standard (<500ms): Ontology loading, config generation

---

**Last Updated**: January 25, 2026 (Added Component Diagrams Part B + Framework)
**Branch**: `claude/erlang-autonomic-c4-diagrams-V7Hpq`
**Status**: ✅ Production-Ready

