# 8020 Sector Bundles Integration Guide

**Objective**: Show how ggen's 5 sector bundles compose into complete vertical stacks, with real-world examples
**Audience**: Teams deploying microservices, papers, support systems, or API gateways
**Format**: Step-by-step with code examples, measurements, and success criteria

---

## What Are Sector Bundles?

A **sector bundle** is a pre-composed vertical stack that covers 80% of real-world requirements for a domain:

- **O** (Ontology): RDF/OWL domain model
- **Π** (Projections): Code generators, templates, configs
- **Guards**: Automated validation rules
- **Examples**: Real working code you can copy-paste
- **Tests**: Comprehensive test suites
- **Docs**: Getting-started guides + architecture

Result: Install bundle → get 80% working system → add your 20% business logic

---

## Scenario 1: Building a Modern Microservice (Rust)

### Step 1: Install Base Bundle

```bash
# Install the microservice bundle + its dependencies
ggen market install sector-rust-microservice-8020

cd sector-rust-microservice-8020
```

**What you get**:
- ✅ Project scaffold (cargo init, folder structure)
- ✅ Error handling patterns (thiserror + custom types)
- ✅ API endpoint templates (REST, handlers)
- ✅ Database patterns (sqlx, migrations)
- ✅ Docker + K8s configs
- ✅ Logging/tracing setup
- ✅ Health checks
- ✅ Tests (unit, integration)

### Step 2: Link to Observability Bundle

```bash
# Install observability addon (auto-hooked into microservice)
ggen market install sector-observability-8020 --link-to microservice

# This automatically:
# - Adds OTEL dependencies to Cargo.toml
# - Configures tracing spans in handlers
# - Sets up metrics collection
# - Links to your observability platform
```

**What gets wired**:
```rust
// Auto-added to your main.rs
use opentelemetry::global;
use tracing_opentelemetry::OpenTelemetryLayer;

fn init_observability() -> Result<()> {
    let tracer = global::tracer("myservice");
    let metrics = global::meter("myservice");

    // All your handlers are now instrumented
    Ok(())
}
```

### Step 3: Define Your Domain Ontology

```bash
# Copy the microservice ontology as a template
cp ../sector-rust-microservice-8020/ontologies/microservice_v1.0.0.ttl \
   ontologies/myservice_v1.0.0.ttl

# Edit to add your specific entities
# Example: Add a User entity with properties
```

**Edit `ontologies/myservice_v1.0.0.ttl`**:
```turtle
@prefix myapp: <http://mycompany.com/myservice/1.0.0#> .

myapp:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "Represents a user in the system" .

myapp:userId a rdf:Property ;
  rdfs:domain myapp:User ;
  rdfs:range xsd:string .

myapp:email a rdf:Property ;
  rdfs:domain myapp:User ;
  rdfs:range xsd:string .
```

### Step 4: Generate Code from Ontology

```bash
# Generate Rust models from RDF
ggen template generate \
  --from-ontology ontologies/myservice_v1.0.0.ttl \
  --projection rust-models \
  --output src/models/generated.rs

# This produces:
# struct User { id: String, email: String }
# impl From<UserRow> for User { ... }
# impl Serialize/Deserialize for User { ... }
```

### Step 5: Test & Validate

```bash
# Run all tests
cargo test

# Validate microservice patterns
ggen validate --guard GuardChatmanCompliant

# Validate observability integration
ggen validate --guard GuardTelemetryComplete

# Get a validation receipt
ggen market validate --8020 . > validation_receipt.json
```

### Step 6: Deploy

```bash
# Docker build (Dockerfile auto-generated)
docker build -t myservice:latest .

# Deploy to K8s (manifests auto-generated)
kubectl apply -f kubernetes/myservice.yaml

# Check observability is working
# Metrics should appear in Prometheus
# Traces should appear in Jaeger
# Logs should appear in ELK
```

**Time invested**: ~4-6 hours for a complete production-ready microservice
**Without bundle**: ~16 hours of scaffolding + debugging

---

## Scenario 2: Publishing a Research Paper

### Step 1: Install Paper Bundle

```bash
ggen market install sector-paper-lifecycle-8020

cd sector-paper-lifecycle-8020
```

### Step 2: Choose Your Venue

```bash
# List available venue templates
ggen list templates --category paper-venue

# Output:
# - icml-2026-template
# - neurips-2026-template
# - iclr-2026-template
# - acl-2026-template
# - cvpr-2026-template

# Copy the template you need
cp templates/venues/neurips-2026-template.tex paper.tex
```

### Step 3: Add Your Content

```bash
# The template is pre-structured with:
# - Title, authors, abstract
# - Sections (intro, related work, method, experiments, conclusion)
# - Bibliography (BibTeX)
# - Figures and tables (pre-formatted)
# - Appendix (supplementary material)

# Just add your content (replace [YOUR CONTENT HERE] markers)
vim paper.tex

# Add your figures
cp my_figures/* figures/

# Add your bibliography
cp my_references.bib references.bib
```

### Step 4: Build PDF

```bash
# Auto-builds with correct settings for venue
make pdf

# Output: paper.pdf
```

### Step 5: Validate Submission

```bash
# Check compliance with venue requirements
ggen validate --guard GuardPaperComplete

# Checks:
# ✅ Page count within limits (8-9 pages)
# ✅ Font sizes correct (11pt main, 9pt footnotes)
# ✅ Margins correct (1 inch)
# ✅ All figures referenced
# ✅ All tables formatted
# ✅ Bibliography complete
# ✅ No LaTeX errors or warnings
```

### Step 6: Generate Submission Pack

```bash
# Create submission-ready archive
ggen package --for-submission neurips-2026

# Creates: neurips-2026-submission.zip with:
# - paper.pdf
# - paper.tex (source)
# - All figure files
# - All data files (if under 50MB)
# - Supplementary material
# - Validation receipt
# - README with submission checklist
```

### Step 7: Submit

```bash
# Open submission portal
# Upload neurips-2026-submission.zip
# Check all files attached
# Submit!

# From downloading template to hitting "submit": ~2 hours
# Without bundle: ~10 hours of formatting headaches
```

**Dark Matter Eliminated**: No more fighting with LaTeX, margins, citation styles, or venue-specific requirements!

---

## Scenario 3: Building a Support Routing System

### Step 1: Install Support Bundle

```bash
ggen market install sector-support-hooks-8020

cd sector-support-hooks-8020
```

### Step 2: Configure Case Categories

```bash
# Edit the support ontology
vim ontologies/support_v1.0.0.ttl

# Define your case categories:
# - BUG (critical, high, medium, low)
# - FEATURE_REQUEST (roadmap impact)
# - USAGE_QUESTION (self-service possible?)
# - BILLING (high priority always)
# - ACCOUNT_ISSUE (medium priority)
```

### Step 3: Setup Routing Rules

```toml
# Create hooks/routing_rules.toml
[routing]
# Route bugs to engineering team
rule_bug = { team = "engineering", priority = "auto-score" }

# Route features to product team
rule_feature = { team = "product", priority = "low" }

# Route questions to support
rule_question = { team = "support", priority = "medium" }

# Route billing to finance
rule_billing = { team = "finance", priority = "high" }

# Critical bugs always escalate
[escalation]
critical_threshold = "p0"  # Auto-escalate if severity >= P0
response_sla = "15 min"
resolution_sla = "2 hours"
```

### Step 4: Connect to Your Support System

```bash
# Examples provided for:
# - Zendesk webhooks
# - Jira ticket creation
# - Slack notifications
# - PagerDuty escalation
# - Datadog alerting

# Copy the right example
cp examples/zendesk-webhook.rs hooks/webhook.rs

# Edit with your API keys
vim hooks/webhook.rs
```

### Step 5: Deploy Hooks

```bash
# Deploy the routing system
ggen deploy hooks/ --target my-support-system

# This starts:
# - Webhook listener on port 8080
# - Case classifier (ML model)
# - Routing engine
# - Escalation tracker
# - Analytics dashboard
```

### Step 6: Monitor Effectiveness

```bash
# Dashboard shows:
# - Cases routed correctly (%) - target: >95%
# - Average time to assign - target: <2 min
# - First-response SLA met (%) - target: >98%
# - Escalation rate - target: <5%
# - Customer satisfaction - target: >4.5/5

ggen metrics --dashboard support-health
```

**Dark Matter Eliminated**: No more manual case routing! Agents focus on solving, not routing.

---

## Scenario 4: Deploying an API Gateway

### Step 1: Install API Gateway Bundle

```bash
ggen market install sector-api-gateway-8020

cd sector-api-gateway-8020
```

### Step 2: Define Your Gateway

```bash
# Start from template (Kubernetes Ingress, Envoy, or Kong)
cp templates/kubernetes-ingress-8020.yaml gateway.yaml

# Edit with your services
vim gateway.yaml

# Add routes:
routes:
  - path: /api/users
    backend: user-service:8080
  - path: /api/orders
    backend: order-service:8080
  - path: /api/payments
    backend: payment-service:8080
```

### Step 3: Add Authentication

```bash
# Copy OAuth2 pattern from examples
cp examples/oauth2-jwt-pattern.yaml auth-config.yaml

# This sets up:
# - JWT token validation
# - Client credentials flow
# - OIDC integration (optional)
# - API key fallback (optional)
```

### Step 4: Add Rate Limiting

```bash
# Copy rate limiting pattern
cp examples/rate-limiting-pattern.yaml rate-limits.yaml

# Configure:
global_limit: 10000  # req/min per client
per_user_limit: 100  # req/min per authenticated user
burst_size: 20       # allows traffic spikes
```

### Step 5: Add Observability

```bash
# This is auto-wired to sector-observability-8020
# Adds:
# - Request/response metrics
# - Trace collection (all requests)
# - Error rate tracking
# - Latency percentiles (p50, p95, p99)
# - Dependency health (backend service health)
```

### Step 6: Deploy

```bash
# Build and deploy to Kubernetes
kubectl apply -f gateway.yaml

# Verify gateway is running
kubectl get ingress

# Check observability
# Metrics appear in Prometheus
# Traces appear in Jaeger
# All traffic flows through gateway
```

**Dark Matter Eliminated**: Zero manual configuration of routing, auth, rate limiting!

---

## Scenario 5: Complete Stack Example

Imagine you're building a SaaS platform. Here's the FULL stack using ALL 5 bundles:

```
┌─────────────────────────────────────────────────┐
│           Complete SaaS Platform 8020            │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│  Sector API Gateway 8020 (Traffic Ingress)      │
│  - Kubernetes Ingress + Auth + Rate Limiting    │
│  - Observability hooked in                      │
└─────────────────────────────────────────────────┘
         │                  │                  │
         ↓                  ↓                  ↓
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│ User Service     │ │ Order Service    │ │ Payment Service  │
│ (Rust, 8020)     │ │ (Rust, 8020)     │ │ (Rust, 8020)     │
│ - Handlers       │ │ - Handlers       │ │ - Handlers       │
│ - Error Handling │ │ - Error Handling │ │ - Error Handling │
│ - Metrics/Traces │ │ - Metrics/Traces │ │ - Metrics/Traces │
│ - DB Patterns    │ │ - DB Patterns    │ │ - DB Patterns    │
└──────────────────┘ └──────────────────┘ └──────────────────┘
         │                  │                  │
         └──────────────────┼──────────────────┘
                            ↓
         ┌──────────────────────────────────┐
         │ Sector Observability 8020        │
         │ - OTEL Metrics Collection        │
         │ - Distributed Tracing (Jaeger)   │
         │ - Log Aggregation (ELK)          │
         │ - SLO Tracking                   │
         │ - Grafana Dashboards             │
         └──────────────────────────────────┘

Also using:
┌────────────────────────────────────────┐
│ Sector Paper Lifecycle 8020            │
│ (for publishing research on your work) │
└────────────────────────────────────────┘

┌────────────────────────────────────────┐
│ Sector Support Hooks 8020              │
│ (for customer support tickets)         │
└────────────────────────────────────────┘
```

**Install all 5 bundles**:
```bash
ggen market install \
  sector-rust-microservice-8020 \
  sector-api-gateway-8020 \
  sector-observability-8020 \
  sector-paper-lifecycle-8020 \
  sector-support-hooks-8020

# Single command, all dependencies resolved automatically
```

**Result**:
- ✅ 3 microservices fully scaffolded (3 × 16h = 48h saved)
- ✅ API gateway routing + auth configured (20h saved)
- ✅ Complete observability working (3 × 8h = 24h saved)
- ✅ Support system routing tickets (saves ongoing hours)
- ✅ Paper templates ready for publishing (10h saved per paper)

**Total Dark Matter Eliminated in Week 1**: ~100 hours
**Total Dark Matter Eliminated in Year 1**: ~1,500 hours (per team)

---

## Integration Points

### Between Microservice & Observability

```rust
// Auto-configured in your service
#[instrument(skip(db))]  // Auto-add span
async fn get_user(id: String, db: &Pool) -> Result<User> {
    let span = tracing::info_span!("get_user", user_id = %id);
    let _guard = span.enter();

    // Automatically:
    // - Span created with this request
    // - Database query traced
    // - Errors logged with context
    // - Metrics incremented
    // - Duration recorded

    db.get_user(&id).await
}
```

### Between API Gateway & Observability

```yaml
# Auto-configured in gateway config
apiVersion: v1
kind: Service
metadata:
  name: my-api-gateway
  labels:
    observability: "true"  # Auto-hooks observability
---
# Automatically exports:
# - Request count (by method, path, status)
# - Request latency (p50, p95, p99)
# - Error rate
# - Backend health
```

### Between Support Hooks & Observability

```bash
# All events tracked:
# - Case created (metric: cases_created_total)
# - Case classified (metric: classification_accuracy)
# - Case routed (metric: routing_latency_ms)
# - Case assigned (metric: assignment_latency_ms)
# - Case escalated (metric: escalations_total)
```

---

## Validation Receipts

After installing any bundle, get a signed proof:

```bash
ggen market validate --8020 . > receipt.json
```

**Receipt shows**:
```json
{
  "package_name": "sector-rust-microservice-8020",
  "version": "1.0.0",
  "is_8020_certified": true,
  "sector": "microservice",
  "dark_matter_reduction_target": "50% reduction in scaffolding",
  "checks_passed": [
    "ontology_valid",
    "projections_complete",
    "templates_present",
    "tests_present",
    "documentation_complete",
    "guards_defined"
  ],
  "quality_score": 87,
  "signature": "hmac-sha256-...",
  "validated_at": "2025-11-16T..."
}
```

**Use this receipt**:
- ✅ Audit trail for compliance
- ✅ Prove to stakeholders bundle was validated
- ✅ Track which versions you're using
- ✅ Baseline for measuring improvements

---

## Troubleshooting

**Q: "Template doesn't match my tech stack"**
A: Bundles cover 80% of cases. For the remaining 20%, edit the templates. Guards will validate your changes.

**Q: "How do I customize without breaking updates?"**
A: Use `#[8020-custom]` comments. Updates preserve custom sections.

**Q: "Observability not showing data"**
A: Run `ggen validate --guard GuardTelemetryComplete` to check setup.

**Q: "My ontology is different from the bundle"**
A: No problem! Use your ontology instead, configure projections to use it. Bundles are templates, not rigid.

---

**Document Version**: 1.0
**Last Updated**: 2025-11-16
**Next Steps**: Pick a scenario, run it, measure the time saved!
