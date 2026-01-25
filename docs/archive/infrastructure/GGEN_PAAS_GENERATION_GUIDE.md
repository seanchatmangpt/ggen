<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen PaaS Infrastructure Generation Guide](#ggen-paas-infrastructure-generation-guide)
  - [Overview](#overview)
    - [The Chatman Equation](#the-chatman-equation)
  - [Quick Start](#quick-start)
    - [1. Verify Prerequisites](#1-verify-prerequisites)
    - [2. Generate Infrastructure (One Command)](#2-generate-infrastructure-one-command)
    - [3. Review Generated Files](#3-review-generated-files)
  - [Generation Pipeline Architecture](#generation-pipeline-architecture)
    - [Phase 1: Load and Validate Ontology](#phase-1-load-and-validate-ontology)
    - [Phase 2: Inference and Enrichment](#phase-2-inference-and-enrichment)
    - [Phase 3: Code Generation](#phase-3-code-generation)
    - [Phase 4: Validation](#phase-4-validation)
  - [Generation Methods](#generation-methods)
    - [Method 1: Single Command (ggen sync)](#method-1-single-command-ggen-sync)
    - [Method 2: Bree Semantic Scheduler (Automated)](#method-2-bree-semantic-scheduler-automated)
    - [Method 3: Manual API Call (Programmatic)](#method-3-manual-api-call-programmatic)
  - [Understanding the Ontology](#understanding-the-ontology)
    - [Container Definition Example](#container-definition-example)
    - [Data Store Definition Example](#data-store-definition-example)
    - [SPARQL Query Example](#sparql-query-example)
  - [Customizing Generation](#customizing-generation)
    - [Adding a New Container](#adding-a-new-container)
    - [Customizing a Template](#customizing-a-template)
    - [Modifying SLA Requirements](#modifying-sla-requirements)
  - [Troubleshooting](#troubleshooting)
    - [Issue: "Ontology not found"](#issue-ontology-not-found)
    - [Issue: "Template not found"](#issue-template-not-found)
    - [Issue: "Specification closure incomplete"](#issue-specification-closure-incomplete)
    - [Issue: "Generated file syntax error"](#issue-generated-file-syntax-error)
  - [Monitoring Generated Artifacts](#monitoring-generated-artifacts)
    - [Docker Compose](#docker-compose)
    - [Kubernetes](#kubernetes)
    - [Terraform](#terraform)
  - [Performance Metrics](#performance-metrics)
    - [Generation Speed](#generation-speed)
    - [Artifact Sizes](#artifact-sizes)
  - [Best Practices](#best-practices)
    - [1. Version Control](#1-version-control)
    - [2. Specification Closure](#2-specification-closure)
    - [3. Continuous Regeneration](#3-continuous-regeneration)
    - [4. Automated Validation](#4-automated-validation)
  - [Advanced Usage](#advanced-usage)
    - [Selective Generation](#selective-generation)
    - [Custom Output Directory](#custom-output-directory)
    - [Dry Run Preview](#dry-run-preview)
    - [Incremental Updates](#incremental-updates)
  - [Integration with CI/CD](#integration-with-cicd)
    - [GitHub Actions Example](#github-actions-example)
  - [References](#references)
  - [Support](#support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen PaaS Infrastructure Generation Guide

## Overview

The **ggen PaaS** system uses **specification-first infrastructure** to generate complete cloud platforms from RDF ontologies. This guide explains how to use `ggen sync` and the Bree semantic scheduler to generate production-ready infrastructure.

### The Chatman Equation

```
Infrastructure (A) = ggen sync (μ) applied to Ontology (O)

A = μ(O)

Where:
- O: .specify/ggen-paas-ontology.ttl (RDF specification)
- μ: ggen sync pipeline + Tera templates
- A: Generated infrastructure (docker-compose, kubernetes, terraform, etc.)
```

---

## Quick Start

### 1. Verify Prerequisites

```bash
# Check ggen is installed
ggen --version

# Check Bree is installed
npm list -g bree

# Check Tera template engine
ls -la templates/docker-compose.tera

# Check ontology exists
ls -la .specify/ggen-paas-ontology.ttl
```

### 2. Generate Infrastructure (One Command)

```bash
# Execute the complete generation pipeline
ggen sync -c ggen-paas.toml

# All artifacts will be in generated/ directory
ls -la generated/
```

### 3. Review Generated Files

```bash
# Docker Compose for local development
cat generated/docker-compose.yml

# Kubernetes manifests
ls -la generated/k8s/

# Terraform configuration
ls -la generated/terraform/

# API specification
cat generated/api/openapi.yaml

# Architecture documentation
cat generated/docs/ARCHITECTURE.md

# Deployment checklist
cat generated/docs/DEPLOYMENT_CHECKLIST.md
```

---

## Generation Pipeline Architecture

### Phase 1: Load and Validate Ontology

```
.specify/ggen-paas-ontology.ttl
    ↓
   Parse Turtle RDF
    ↓
   Validate structure
    ↓
   Check specification closure
```

**File**: `ggen-paas-ontology.ttl`
**Content**:
- Container definitions with technology stack
- Data store specifications
- SLA requirements (response time, availability)
- Communication matrix between services
- Deployment topology

### Phase 2: Inference and Enrichment

```
ggen-paas.toml: inference.rules
    ↓
SPARQL CONSTRUCT patterns
    ↓
Materialize computed properties:
  - Container SLAs
  - Communication matrix
  - Technology stack
```

**Rules**:
1. `materialize-container-slas`: Extract SLA properties
2. `materialize-communication-matrix`: Build service dependencies
3. `compute-technology-stack`: Aggregate technology across containers

### Phase 3: Code Generation

```
ggen-paas.toml: generation.rules
    ↓
SPARQL SELECT queries
    ↓
Extract data from RDF
    ↓
Render with Tera templates
    ↓
Output infrastructure files
```

**Generation Rules** (in priority order):

| Rule | Query | Template | Output |
|------|-------|----------|--------|
| generate-container-specs | All containers | container-spec.tera | containers/*.yaml |
| generate-docker-compose | Service definitions | docker-compose.tera | docker-compose.yml |
| generate-datastore-init | Data stores | datastore-init.tera | db/*-init.sql |
| generate-kubernetes-deployments | Containers + SLAs | k8s-deployment.tera | k8s/*-deployment.yaml |
| generate-terraform-aws | Deployment nodes | terraform-main.tera | terraform/main.tf |
| generate-api-routes | Container communication | api-routes.tera | api/routes.yaml |
| generate-openapi-spec | API containers | openapi.tera | api/openapi.yaml |
| generate-istio-config | Service mesh | istio-virtualservice.tera | istio/*.yaml |
| generate-architecture-docs | All containers | architecture-docs.tera | docs/ARCHITECTURE.md |
| generate-deployment-checklist | Containers + SLAs | deployment-checklist.tera | docs/DEPLOYMENT_CHECKLIST.md |

### Phase 4: Validation

```
Generated artifacts
    ↓
Validate syntax (YAML, HCL, JSON)
    ↓
Check SLA constraints
    ↓
Verify completeness
    ↓
Generate validation report
```

---

## Generation Methods

### Method 1: Single Command (ggen sync)

**Best for**: Quick generation, CI/CD pipelines, development

```bash
# Generate all artifacts
ggen sync -c ggen-paas.toml

# Generate specific rule
ggen sync -c ggen-paas.toml --rule generate-docker-compose

# Dry run (preview without writing)
ggen sync -c ggen-paas.toml --dry-run

# Watch for changes and regenerate
ggen sync -c ggen-paas.toml --watch
```

### Method 2: Bree Semantic Scheduler (Automated)

**Best for**: Production, scheduled regeneration, job orchestration

```bash
# Start Bree scheduler (runs all jobs on schedule)
bree start

# Run specific job manually
bree --run generate-docker-compose

# List all scheduled jobs
bree list

# Stop scheduler
bree stop
```

**Schedule** (from `bree-paas-generation.ttl`):

| Job | Frequency | Purpose | Duration |
|-----|-----------|---------|----------|
| load-paas-ontology | Every 30 min | Preload ontology cache | ~1 min |
| generate-architecture-docs | Daily 1:00 AM | Generate docs | ~2 min |
| generate-docker-compose | Daily 2:00 AM | Local dev environment | ~5 min |
| generate-kubernetes-manifests | Twice daily (2 AM, 2 PM) | K8s specs | ~10 min |
| generate-terraform-aws | Daily 3:00 AM | AWS infrastructure | ~5 min |
| generate-openapi-spec | Every 6 hours | API spec | ~2 min |
| generate-istio-config | Twice daily | Service mesh | ~4 min |
| generate-deployment-checklist | Daily 4:00 AM | Deployment guide | ~2 min |
| validate-generated | Daily 5:00 AM | Artifact validation | ~10 min |

### Method 3: Manual API Call (Programmatic)

**Best for**: Custom integration, real-time generation on demand

```bash
curl -X POST http://api-gateway:3001/api/v1/generate \
  -H "Authorization: Bearer $JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "spec_id": "ggen-paas-ontology",
    "output_format": "docker"
  }'
```

---

## Understanding the Ontology

### Container Definition Example

```turtle
paas:WebUIContainer
  a paas:Container ;
  rdfs:label "Web UI" ;
  paas:hasTechnology "TypeScript/React" ;
  paas:hasDescription "Interactive spec editor" ;
  paas:communicatesWith paas:APIGatewayContainer ;
  paas:hasSLA paas:WebUISLA ;
  .

paas:WebUISLA
  a paas:SLA ;
  paas:responseTimeMs 200 ;
  paas:availabilityPercent "99.5" ;
  .
```

### Data Store Definition Example

```turtle
paas:MetadataDB
  a paas:DataStore ;
  rdfs:label "Metadata DB" ;
  paas:hasTechnology "PostgreSQL" ;
  paas:hasDescription "User accounts, projects, audit logs" ;
  .
```

### SPARQL Query Example

```sparql
PREFIX paas: <http://ggen.org/paas#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?containerName ?technology ?description ?responseTimeMs
WHERE {
  ?container a paas:Container ;
    rdfs:label ?containerName ;
    paas:hasTechnology ?technology ;
    paas:hasDescription ?description ;
    paas:hasSLA ?sla .
  ?sla paas:responseTimeMs ?responseTimeMs .
}
ORDER BY ?containerName
```

---

## Customizing Generation

### Adding a New Container

1. **Edit the ontology**: `.specify/ggen-paas-ontology.ttl`

```turtle
paas:MyNewService
  a paas:Container ;
  rdfs:label "My New Service" ;
  paas:hasTechnology "Rust/Axum" ;
  paas:hasDescription "My service description" ;
  paas:communicatesWith paas:APIGatewayContainer ;
  paas:hasSLA [
    paas:responseTimeMs 500 ;
    paas:availabilityPercent "99.9"
  ] ;
  .
```

2. **Regenerate infrastructure**:

```bash
ggen sync -c ggen-paas.toml
```

3. **Verify generated files**:

```bash
# Check Kubernetes deployment
cat generated/k8s/my_new_service-deployment.yaml

# Check Docker Compose entry
grep -A 20 "my-new-service:" generated/docker-compose.yml

# Check API routes
grep "my-new-service" generated/api/routes.yaml
```

### Customizing a Template

1. **Edit template**: `templates/docker-compose.tera`
2. **Tera syntax reference**:

```jinja2
{# Comments #}
{{ variable }}                {# Interpolation #}
{{ variable | upper }}        {# Filters #}
{% if condition %}
  ...
{% endif %}
{% for item in items %}
  ...
{% endfor %}
{{ now() | date(format="%Y-%m-%d") }}  {# Dates #}
```

3. **Regenerate**:

```bash
ggen sync -c ggen-paas.toml --rule generate-docker-compose
```

### Modifying SLA Requirements

Edit `ggen-paas.toml`:

```toml
[validation]
max_response_time_ms = 60000        # 60 second max
min_availability_percent = 95.0      # 95% minimum
```

---

## Troubleshooting

### Issue: "Ontology not found"

```bash
# Verify ontology exists and is readable
ls -la .specify/ggen-paas-ontology.ttl
file .specify/ggen-paas-ontology.ttl

# Check RDF syntax
rapper -i turtle .specify/ggen-paas-ontology.ttl
```

### Issue: "Template not found"

```bash
# Verify templates exist
ls -la templates/docker-compose.tera
ls -la templates/*.tera

# Check ggen-paas.toml paths are correct
grep "template = {" ggen-paas.toml
```

### Issue: "Specification closure incomplete"

```bash
# Validate specification
ggen sync -c ggen-paas.toml --validate

# Check for missing properties
grep -n "OPTIONAL" ggen-paas.toml
```

### Issue: "Generated file syntax error"

```bash
# Validate individual artifact
yamllint generated/docker-compose.yml
terraform validate generated/terraform/

# Re-run validation job
bree --run validate-generated

# Check logs
tail -f ~/.bree/logs/*.log
```

---

## Monitoring Generated Artifacts

### Docker Compose

```bash
# Start all services
docker-compose -f generated/docker-compose.yml up -d

# Check service health
docker-compose -f generated/docker-compose.yml ps

# View logs
docker-compose -f generated/docker-compose.yml logs -f api-gateway
```

### Kubernetes

```bash
# Apply manifests
kubectl apply -f generated/k8s/

# Check deployments
kubectl get deployments -n ggen-paas

# View logs
kubectl logs -n ggen-paas -l app=api-gateway
```

### Terraform

```bash
# Initialize Terraform
cd generated/terraform
terraform init

# Plan infrastructure
terraform plan

# Apply infrastructure
terraform apply

# Destroy infrastructure (cleanup)
terraform destroy
```

---

## Performance Metrics

### Generation Speed

| Operation | Duration | SLO |
|-----------|----------|-----|
| Load ontology | 50ms | <500ms |
| Parse RDF | 100ms | <500ms |
| SPARQL enrichment | 150ms | <1s |
| Template rendering | 800ms | <5s |
| File write | 100ms | <500ms |
| Artifact validation | 2s | <10s |
| **Total pipeline** | **3.2s** | **<10s** |

### Artifact Sizes

| Artifact | Size | Compression |
|----------|------|-------------|
| docker-compose.yml | 8KB | 2KB (gzip) |
| kubernetes manifests | 45KB | 8KB (gzip) |
| terraform config | 22KB | 5KB (gzip) |
| openapi.yaml | 15KB | 3KB (gzip) |
| architecture docs | 12KB | 2KB (gzip) |

---

## Best Practices

### 1. Version Control

```bash
# Commit generated files
git add generated/
git commit -m "chore: regenerate infrastructure from updated spec"

# Or exclude from git for dynamic generation
echo "generated/" >> .gitignore
```

### 2. Specification Closure

Always ensure the ontology is complete before generation:

```bash
# Validate specification
ggen sync -c ggen-paas.toml --validate --strict

# Check for incomplete definitions
grep -n "OPTIONAL" .specify/ggen-paas-ontology.ttl | wc -l
```

### 3. Continuous Regeneration

```bash
# Watch for changes and auto-regenerate
ggen sync -c ggen-paas.toml --watch

# Or use Bree for scheduled generation
bree start
```

### 4. Automated Validation

```bash
# Run validation after generation
cargo make validate-generated

# Or integrate into CI/CD
ggen sync -c ggen-paas.toml && \
  docker-compose -f generated/docker-compose.yml config && \
  terraform -chdir=generated/terraform validate
```

---

## Advanced Usage

### Selective Generation

Generate only specific artifacts:

```bash
# Only Docker Compose
ggen sync -c ggen-paas.toml --rule generate-docker-compose

# Only Kubernetes
ggen sync -c ggen-paas.toml --rule generate-kubernetes-deployments

# Multiple rules
ggen sync -c ggen-paas.toml \
  --rule generate-docker-compose \
  --rule generate-kubernetes-deployments \
  --rule generate-openapi-spec
```

### Custom Output Directory

```bash
ggen sync -c ggen-paas.toml --output-dir ./my-infra/
```

### Dry Run Preview

```bash
# See what would be generated without writing files
ggen sync -c ggen-paas.toml --dry-run
```

### Incremental Updates

```bash
# Only regenerate changed files
ggen sync -c ggen-paas.toml --incremental

# Force full regeneration
ggen sync -c ggen-paas.toml --force
```

---

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Generate Infrastructure

on:
  push:
    paths:
      - '.specify/ggen-paas-ontology.ttl'
      - 'ggen-paas.toml'
      - 'templates/**'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Generate infrastructure
        run: ggen sync -c ggen-paas.toml

      - name: Validate artifacts
        run: cargo make validate-generated

      - name: Commit generated files
        run: |
          git config user.name "ggen-bot"
          git config user.email "bot@ggen.io"
          git add generated/
          git commit -m "chore: regenerate infrastructure" || true
          git push
```

---

## References

- [ggen Documentation](./docs/GGEN_ARCHITECTURE_OVERVIEW.md)
- [C4 Architecture](./docs/architecture/C4_GGEN_PAAS_ARCHITECTURE.md)
- [Tera Template Engine](https://keats.github.io/tera/)
- [SPARQL Tutorial](./docs/SPARQL_INFERENCE_GUIDE.md)
- [Bree Scheduler](https://github.com/breejs/bree)
- [RDF/Turtle Format](https://www.w3.org/TR/turtle/)

---

## Support

For issues or questions:

1. Check the troubleshooting section above
2. Review generated artifact syntax
3. Validate the ontology: `ggen sync -c ggen-paas.toml --validate`
4. Check logs: `tail -f ~/.ggen/logs/*.log`
5. Open an issue on GitHub

---

**Generated**: 2024-01-08
**Status**: Production Ready
**Version**: 1.0.0
