# Setup & Usage Guide

Complete guide to using the ggen templates for C4 diagrams and Kubernetes deployment.

## Quick Start (5 minutes)

### 1. Prerequisites
```bash
# Required tools
- ggen v6.0.0+ (CLI)
- Tera template engine (included in ggen)
- SPARQL processor (Oxigraph, included in ggen)
- kubectl (for Kubernetes validation)
- mermaid-cli (for diagram rendering: mmdc)

# Optional tools
- Docker (for diagram preview)
- GitHub CLI (for documentation links)
- PlantUML (for alternative diagram format)
```

### 2. Generate Diagrams & Manifests
```bash
cd examples/gcp-erlang-autonomics

# Dry-run: Preview all changes without writing files
ggen sync --dry_run true

# Full generation with audit trail
ggen sync --audit true

# Force regeneration (overwrite existing files)
ggen sync --force true --audit true
```

### 3. View Generated Outputs
```bash
# List generated files
ls -lh generated/

# View diagram syntax
cat generated/c4-level1-context.mmd

# Preview as PNG (requires mmdc)
mmdc -i generated/c4-level1-context.mmd -o c4-level1.png
```

### 4. Deploy to Kubernetes
```bash
# Validate manifests before deployment
kubectl apply -f generated/deployment-gke.yaml --dry-run=server

# Deploy to cluster
kubectl apply -f generated/deployment-gke.yaml

# Check deployment status
kubectl get deployment -n autonomic-system
kubectl logs -n autonomic-system -l app=governor --tail=50
```

---

## Project Structure

```
examples/gcp-erlang-autonomics/
│
├── ggen.toml                              # ⭐ Configuration manifest
│   ├── [project]                          # Project metadata
│   ├── [specs]                            # RDF ontology locations
│   ├── [generation]                       # Template & output dirs
│   ├── [queries]                          # SPARQL queries
│   ├── [slo]                              # Performance SLOs
│   ├── [validation]                       # Quality gates
│   ├── [template_engine]                  # Tera options
│   ├── [mermaid]                          # Diagram styling
│   ├── [kubernetes]                       # K8s defaults
│   ├── [gcp]                              # GCP configuration
│   └── [observability]                    # Logging options
│
├── templates/                             # ⭐ Tera template files
│   ├── c4-level1.tera                     # System context
│   ├── c4-level2.tera                     # Containers
│   ├── c4-level3.tera                     # Components & FSM
│   ├── c4-level4.tera                     # Infrastructure
│   ├── sku-catalog.tera                   # Product catalog
│   └── deployment-gke.tera                # Kubernetes manifests
│
├── generated/                             # Output directory (created)
│   ├── c4-level1-context.mmd              # Mermaid diagram
│   ├── c4-level2-containers.mmd
│   ├── c4-level3-components.mmd
│   ├── c4-level4-deployment.mmd
│   ├── sku-catalog.md                     # Markdown catalog
│   ├── deployment-gke.yaml                # Kubernetes manifests
│   └── .ggen/                             # Audit trail
│       ├── receipts/                      # Generation receipts (SHA-256)
│       └── audit/                         # Detailed audit logs
│
├── .specify/                              # RDF Specifications
│   ├── specs/                             # Feature specifications (.ttl)
│   │   ├── 001-governor/
│   │   │   ├── feature.ttl                # User stories (EDIT THIS)
│   │   │   ├── entities.ttl               # Domain models
│   │   │   ├── plan.ttl                   # Architecture
│   │   │   └── tasks.ttl                  # Task breakdown
│   │   ├── 002-signals/
│   │   ├── 003-actions/
│   │   └── ...
│   │
│   └── ontologies/                        # RDF Ontologies (source of truth)
│       ├── c4.ttl                         # C4 model
│       ├── erlang-autonomics.ttl          # Governor domain
│       ├── gcp-infrastructure.ttl         # GCP services
│       └── skus.ttl                       # Product SKUs
│
├── TEMPLATES_README.md                    # Detailed template documentation
├── TEMPLATE_REFERENCE.md                  # Quick reference guide
├── SETUP_GUIDE.md                         # This file
└── README.md                              # Project overview
```

---

## Configuration Details

### ggen.toml Structure

#### Project Metadata
```toml
[project]
name = "gcp-erlang-autonomics"
version = "0.1.0"
description = "C4 diagrams + Erlang autonomic governors on GCP"
```

#### Specification Layer
```toml
[specs]
directory = ".specify/specs"              # Where to find TTL specs
ontology = ".specify/ontologies/c4.ttl"   # Primary ontology
include_ontologies = [...]                # Additional ontologies
```

#### Generation Targets
```toml
[[generation.targets]]
name = "c4-level1"
template = "c4-level1.tera"
output_file = "generated/c4-level1-context.mmd"
format = "mermaid"
```

#### SPARQL Queries
```toml
[queries]
systems = """
PREFIX c4: <http://ggen.io/c4#>
SELECT ?actor_label ?system_label ...
WHERE { ... }
ORDER BY ?actor_label ?system_label
"""
```

#### Validation & Quality
```toml
[validation]
deterministic = true              # Same input → same output
shacl_validation = true           # Validate RDF schema
validate_mermaid = true           # Validate diagram syntax
validate_kubernetes = true        # Validate manifests
audit_trail = true                # Generate SHA-256 hashes
```

---

## Creating & Editing RDF Ontologies

### 1. Create Feature Specification
```bash
mkdir -p .specify/specs/001-governor
cd .specify/specs/001-governor
```

### 2. Create feature.ttl (User Stories)
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix c4: <http://ggen.io/c4#> .
@prefix us: <http://ggen.io/userstory#> .

# Define an actor
c4:Customer
  rdf:type c4:Actor ;
  rdfs:label "Customer" ;
  rdfs:comment "External user of the Governor system" .

# Define the primary system
c4:GovernorSystem
  rdf:type c4:System ;
  rdfs:label "Autonomous Governor" ;
  rdfs:comment "Erlang-based autonomic controller" ;
  c4:hasContainer c4:GovernorService ;
  c4:hasContainer c4:MetricsDB .

# Define interactions
c4:Customer c4:interactsWith c4:GovernorSystem ;
           c4:relationshipType "uses" .
```

### 3. Create entities.ttl (Domain Models)
```turtle
@prefix c4: <http://ggen.io/c4#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Service container
c4:GovernorService
  rdf:type c4:Container ;
  rdfs:label "Governor Service" ;
  c4:containerType "Service" ;
  c4:language "Erlang" ;
  c4:hasComponent c4:PolicyEngine ;
  c4:hasComponent c4:Executor .

# Database container
c4:MetricsDB
  rdf:type c4:Container ;
  rdfs:label "Metrics Database" ;
  c4:containerType "Database" ;
  c4:technology "PostgreSQL" .

# Data flow
c4:GovernorService c4:dataFlowTo c4:MetricsDB ;
                   c4:flowLabel "stores metrics" .
```

### 4. Validate TTL Syntax
```bash
# Check for errors
ggen validate .specify/specs/001-governor/feature.ttl

# View as RDF graph
ggen show .specify/specs/001-governor/feature.ttl

# Check conformance
ggen validate --shacl true .specify/specs/001-governor/feature.ttl
```

### 5. Regenerate Diagrams
```bash
# Go to project root
cd /path/to/gcp-erlang-autonomics

# Regenerate with new TTL data
ggen sync --audit true

# View changes
cat generated/c4-level1-context.mmd
```

---

## Deploying to GKE

### Prerequisites
```bash
# GCP Project setup
gcloud projects create ggen-autonomics
gcloud config set project ggen-autonomics

# Create GKE cluster
gcloud container clusters create gken-primary \
  --region us-central1 \
  --num-nodes 3 \
  --machine-type n1-standard-2 \
  --workload-pool=ggen-autonomics.svc.id.goog

# Get credentials
gcloud container clusters get-credentials gken-primary --region us-central1
```

### Deploy Manifests
```bash
# Validate syntax
kubectl apply -f generated/deployment-gke.yaml --dry-run=client

# Create namespace
kubectl create namespace autonomic-system

# Deploy all resources
kubectl apply -f generated/deployment-gke.yaml

# Verify deployment
kubectl get all -n autonomic-system
kubectl rollout status deployment/governor -n autonomic-system

# View logs
kubectl logs -n autonomic-system -l app=governor -f
```

### Update Policy Pack
```bash
# Edit policy ConfigMap
kubectl edit configmap governor-policy -n autonomic-system

# Or apply updated manifest
kubectl apply -f generated/deployment-gke.yaml

# Verify update
kubectl get configmap governor-policy -n autonomic-system -o yaml
```

### Monitor Governor
```bash
# Check pod status
kubectl get pods -n autonomic-system -w

# View metrics
kubectl top pods -n autonomic-system

# Check events
kubectl describe deployment governor -n autonomic-system

# Stream logs
kubectl logs -n autonomic-system -l app=governor --all-containers=true -f
```

---

## Viewing Diagrams

### Option 1: GitHub Rendering (Automatic)
```bash
# Push to GitHub
git add generated/c4-level1-context.mmd
git commit -m "docs: Update C4 diagrams"
git push origin main

# View in GitHub web UI
# https://github.com/org/repo/blob/main/generated/c4-level1-context.mmd
# (GitHub auto-renders .mmd files)
```

### Option 2: Mermaid Live Editor (Interactive)
```bash
# Copy diagram content
cat generated/c4-level1-context.mmd | pbcopy

# Visit https://mermaid.live
# Paste content in editor
# Edit and explore interactively
```

### Option 3: Render as PNG/SVG
```bash
# Install mmdc
npm install -g @mermaid-js/mermaid-cli

# Render all diagrams
for f in generated/*.mmd; do
  mmdc -i "$f" -o "${f%.mmd}.png"
done

# View PNG
open generated/c4-level1-context.png
```

### Option 4: Embed in Documentation
```markdown
# Architecture

## System Context Diagram

```mermaid
{% include_relative generated/c4-level1-context.mmd %}
```

## Deployment

See [Kubernetes Manifests](generated/deployment-gke.yaml).
```

---

## Customization Guide

### 1. Change Diagram Styling
Edit `ggen.toml`:
```toml
[mermaid]
theme = "dark"          # default, dark, forest, neutral
direction = "LR"        # TB (top-bottom), LR (left-right)
enable_clustering = true
show_notes = true
```

### 2. Adjust Kubernetes Resources
Edit `ggen.toml`:
```toml
[kubernetes]
cpu_request = "500m"
memory_request = "512Mi"
replicas = 3
max_replicas = 20
```

### 3. Configure GCP Deployment
Edit `ggen.toml`:
```toml
[gcp]
project_id = "my-project"
region = "europe-west1"
zones = ["europe-west1-b", "europe-west1-c", "europe-west1-d"]
```

### 4. Add New Signals/Actions
Edit `.specify/ontologies/skus.ttl`:
```turtle
c4:NewSignal
  rdf:type sku:Signal ;
  rdfs:label "Custom CPU Metric" ;
  skus:threshold 0.80 ;
  skus:unit "percent" .
```

Regenerate:
```bash
ggen sync --audit true
```

---

## Troubleshooting

### Issue: Diagrams Won't Render
**Symptom:** "Invalid Mermaid syntax" in GitHub
**Solution:**
```bash
# Validate syntax
mmdc -i generated/c4-level1-context.mmd -o /tmp/test.png

# If error, check for special characters
cat generated/c4-level1-context.mmd | grep -E '[<>"|]'

# Fix in template (add escaping)
{{ label | replace(from="|", to="\\|") }}
```

### Issue: Kubernetes Deployment Fails
**Symptom:** `kubectl apply` returns errors
**Solution:**
```bash
# Dry-run to see errors
kubectl apply -f generated/deployment-gke.yaml --dry-run=server

# Check specific resource
kubectl get deployment governor -n autonomic-system -o yaml

# Verify all required fields present in manifest
grep "name: governor" generated/deployment-gke.yaml
```

### Issue: SPARQL Query Returns Empty
**Symptom:** Diagrams have no data
**Solution:**
```bash
# Verify ontology has data
ggen show .specify/ontologies/c4.ttl | head -50

# Test SPARQL query directly
ggen query "SELECT * WHERE { ?s ?p ?o } LIMIT 10" \
  .specify/ontologies/c4.ttl

# Check namespace URIs match
grep "@prefix c4" .specify/ontologies/c4.ttl
grep "PREFIX c4:" ggen.toml
```

### Issue: Determinism Broken
**Symptom:** Same input produces different diagrams
**Solution:**
```bash
# Check for random functions in templates
grep -r "random\|shuffle" templates/

# Verify SPARQL ORDER BY
grep "ORDER BY" ggen.toml

# Test determinism
ggen sync && cp generated/c4-level1-context.mmd /tmp/v1.mmd
ggen sync && cp generated/c4-level1-context.mmd /tmp/v2.mmd
diff /tmp/v1.mmd /tmp/v2.mmd  # Should be empty
```

---

## Performance Optimization

### 1. Caching
```toml
[validation]
cache_invalidate_on_schema_change = true  # Auto-detect changes
```

### 2. Parallel Processing
```bash
# Use -j flag (if supported)
ggen sync --parallel true --jobs 4
```

### 3. Incremental Generation
```bash
# Only regenerate changed targets
ggen sync --incremental true
```

### 4. Resource Limits
```bash
# Monitor generation performance
time ggen sync --audit true

# Typical performance:
# - C4 diagrams: ~2.5 seconds
# - Kubernetes manifests: ~0.8 seconds
# - SKU catalog: ~0.2 seconds
# - Total: ~3.5 seconds
```

---

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Generate C4 Diagrams & K8s Manifests

on:
  push:
    branches: [main]
    paths:
      - '.specify/**'
      - 'templates/**'
      - 'ggen.toml'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen --version 6.0.0

      - name: Generate diagrams & manifests
        working-directory: examples/gcp-erlang-autonomics
        run: ggen sync --audit true

      - name: Validate Kubernetes manifests
        run: kubectl apply -f generated/deployment-gke.yaml --dry-run=server

      - name: Validate Mermaid syntax
        run: |
          npm install -g @mermaid-js/mermaid-cli
          for f in generated/*.mmd; do
            mmdc -i "$f" -o /tmp/test.png
          done

      - name: Commit & push changes
        run: |
          git config user.name "ggen-bot"
          git config user.email "ggen@example.com"
          git add generated/
          git commit -m "chore: Regenerate C4 diagrams & manifests" || true
          git push
```

### GitLab CI Example
```yaml
stages:
  - validate
  - generate
  - deploy

generate_diagrams:
  stage: generate
  image: rust:latest
  script:
    - cargo install ggen --version 6.0.0
    - cd examples/gcp-erlang-autonomics
    - ggen sync --audit true
  artifacts:
    paths:
      - examples/gcp-erlang-autonomics/generated/
```

---

## Documentation Links

- **C4 Model:** https://c4model.com/
- **Mermaid Documentation:** https://mermaid.js.org/
- **Tera Template Engine:** https://tera.netlify.app/
- **SPARQL 1.1 Query:** https://www.w3.org/TR/sparql11-query/
- **Kubernetes Docs:** https://kubernetes.io/docs/
- **GCP Documentation:** https://cloud.google.com/docs
- **Erlang/OTP:** https://www.erlang.org/doc

---

## Support & Feedback

### Reporting Issues
1. Check existing [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
2. Create new issue with:
   - ggen version
   - Command run (with `--debug` flag)
   - Error output
   - Expected vs actual behavior

### Contributing Improvements
1. Fork repository
2. Create feature branch: `git checkout -b feature/my-improvement`
3. Make changes and test: `ggen sync --audit true`
4. Submit pull request

---

**Guide Version:** 1.0.0
**Last Updated:** 2026-01-25
**Maintainer:** ggen Project
