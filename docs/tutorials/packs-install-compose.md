<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Installing and Composing Packs](#installing-and-composing-packs)
  - [Prerequisites](#prerequisites)
  - [Scenario 1: Solo Founder Building an MVP (10 minutes)](#scenario-1-solo-founder-building-an-mvp-10-minutes)
  - [Scenario 2: Enterprise Team Building a Microservices Platform (15 minutes)](#scenario-2-enterprise-team-building-a-microservices-platform-15-minutes)
  - [Scenario 3: Data Science Team Building ML Pipeline (12 minutes)](#scenario-3-data-science-team-building-ml-pipeline-12-minutes)
  - [Advanced: Multi-Pack Composition Strategy](#advanced-multi-pack-composition-strategy)
    - [When to Combine Packs](#when-to-combine-packs)
    - [Composition Patterns](#composition-patterns)
    - [Dependency Resolution](#dependency-resolution)
    - [Pack Versioning Strategy](#pack-versioning-strategy)
  - [Verification Checklist](#verification-checklist)
    - [✅ Installation Verification](#-installation-verification)
    - [✅ Functional Verification](#-functional-verification)
  - [Troubleshooting](#troubleshooting)
    - [Problem: Pack validation fails](#problem-pack-validation-fails)
    - [Problem: Template not found after installation](#problem-template-not-found-after-installation)
    - [Problem: Conflicting dependencies](#problem-conflicting-dependencies)
    - [Problem: Dry-run shows different count than actual install](#problem-dry-run-shows-different-count-than-actual-install)
  - [What You've Learned](#what-youve-learned)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Installing and Composing Packs

**Type: How-to Guide** | [← Back to Documentation](../README.md) | ⏱️ **15-20 minutes**

**Goal:** Install packs and compose multiple packs together for real-world project scenarios.

**What you'll learn:** How to install packs, combine multiple packs, check compatibility, customize pack installations, and verify successful setup.

---

## Prerequisites

- ggen installed (`ggen --version` ≥ 3.0.0)
- Completed [Getting Started with Packs](packs-getting-started.md)
- Basic understanding of your target project type (web app, ML pipeline, etc.)
- Time: 15-20 minutes

## Scenario 1: Solo Founder Building an MVP (10 minutes)

**Context:** You're building a SaaS MVP with a web API, database, and basic authentication. You need to ship fast.

**Step 1: Identify Required Packs**

```bash
# List all packs to see options
ggen packs list
```

**Decision:** Use **startup-essentials** pack (covers API, DB, auth, logging)

**Step 2: Validate Before Installing**

```bash
# Verify pack integrity
ggen packs validate --pack_id startup-essentials
```

Expected output:
```json
{
  "valid": true,
  "pack_id": "startup-essentials",
  "package_count": 5,
  "message": "Pack 'Startup Essentials' is valid with 5 packages"
}
```

**Step 3: Preview Installation (Dry Run)**

```bash
# See what will be installed without committing
ggen packs install --pack_id startup-essentials --dry_run
```

Output shows:
```json
{
  "pack_id": "startup-essentials",
  "pack_name": "Startup Essentials",
  "total_packages": 5,
  "packages_to_install": [
    "noun-verb-cli",
    "web-api-starter",
    "postgres-migrations",
    "user-auth-basic",
    "logging-observability"
  ],
  "status": "Ready to install 5 packages..."
}
```

**Step 4: Install Individual Packages**

Currently, packs provide a **curated list** of packages. Install each package using the marketplace:

```bash
# Install each package from the startup-essentials pack
ggen marketplace install noun-verb-cli
ggen marketplace install web-api-starter
ggen marketplace install postgres-migrations
ggen marketplace install user-auth-basic
ggen marketplace install logging-observability
```

**Step 5: Verify Installation**

```bash
# List installed templates
ggen marketplace list
```

**Step 6: Generate Your MVP**

Create your domain ontology:

```bash
cat > domain.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User
  rdfs:label "User" ;
  ex:hasProperty [ rdfs:label "id" ; ex:type ex:Integer ] ;
  ex:hasProperty [ rdfs:label "email" ; ex:type ex:String ] ;
  ex:hasProperty [ rdfs:label "password_hash" ; ex:type ex:String ] .

ex:Project
  rdfs:label "Project" ;
  ex:hasProperty [ rdfs:label "id" ; ex:type ex:Integer ] ;
  ex:hasProperty [ rdfs:label "name" ; ex:type ex:String ] ;
  ex:hasProperty [ rdfs:label "owner_id" ; ex:type ex:Integer ] .
EOF
```

Generate code using the installed templates:

```bash
# Generate web API
ggen generate \
  --template web-api-starter \
  --domain domain.ttl \
  --language rust \
  --output ./backend

# Generate database migrations
ggen generate \
  --template postgres-migrations \
  --domain domain.ttl \
  --output ./migrations

# Generate auth system
ggen generate \
  --template user-auth-basic \
  --domain domain.ttl \
  --language rust \
  --output ./backend/auth
```

**Result:** You now have a complete MVP foundation with API, database, and auth in under 10 minutes!

---

## Scenario 2: Enterprise Team Building a Microservices Platform (15 minutes)

**Context:** Building a production-grade platform with microservices, observability, and DevOps automation.

**Step 1: Identify Multi-Pack Strategy**

You need **multiple packs** working together:
- **enterprise-backend** (microservices, distributed tracing)
- **devops-automation** (CI/CD, monitoring)
- **frontend-modern** (admin UI)

**Step 2: Check Pack Compatibility**

```bash
# Validate each pack individually
ggen packs validate --pack_id enterprise-backend
ggen packs validate --pack_id devops-automation
ggen packs validate --pack_id frontend-modern
```

All should return `"valid": true`.

**Step 3: Preview Multi-Pack Installation**

```bash
# Preview enterprise backend
ggen packs install --pack_id enterprise-backend --dry_run

# Preview DevOps automation
ggen packs install --pack_id devops-automation --dry_run

# Preview frontend
ggen packs install --pack_id frontend-modern --dry_run
```

**Step 4: Install All Packs**

Create an installation script:

```bash
# install-platform.sh
#!/bin/bash
set -e

echo "Installing Enterprise Backend Pack..."
# (Replace with actual package names from pack show commands)
ggen marketplace install microservices-template
ggen marketplace install distributed-tracing
ggen marketplace install advanced-security
ggen marketplace install ha-configuration
ggen marketplace install enterprise-integrations

echo "Installing DevOps Automation Pack..."
ggen marketplace install cicd-pipeline
ggen marketplace install container-orchestration
ggen marketplace install monitoring-alerting
ggen marketplace install infra-provisioning
ggen marketplace install config-management

echo "Installing Modern Frontend Pack..."
ggen marketplace install react-components
ggen marketplace install state-management
ggen marketplace install styling-system
ggen marketplace install build-optimization
ggen marketplace install testing-utilities

echo "✅ All packs installed successfully!"
```

```bash
chmod +x install-platform.sh
./install-platform.sh
```

**Step 5: Verify Complete Installation**

```bash
ggen marketplace list | jq '.installed_templates | length'
# Should show 15 templates (5 per pack × 3 packs)
```

**Step 6: Generate Platform Components**

Create your microservices ontology:

```bash
cat > platform.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# User Service
ex:UserService
  rdfs:label "UserService" ;
  ex:hasEndpoint ex:CreateUser ;
  ex:hasEndpoint ex:GetUser .

# Order Service
ex:OrderService
  rdfs:label "OrderService" ;
  ex:hasEndpoint ex:CreateOrder ;
  ex:hasEndpoint ex:GetOrder .

# Notification Service
ex:NotificationService
  rdfs:label "NotificationService" ;
  ex:hasEndpoint ex:SendNotification .
EOF
```

Generate each service:

```bash
# Generate microservices
for service in UserService OrderService NotificationService; do
  ggen generate \
    --template microservices-template \
    --domain platform.ttl \
    --config "service_name=$service" \
    --output "./services/$service"
done

# Generate CI/CD pipelines
ggen generate \
  --template cicd-pipeline \
  --domain platform.ttl \
  --output ./.github/workflows

# Generate monitoring
ggen generate \
  --template monitoring-alerting \
  --domain platform.ttl \
  --output ./monitoring
```

**Step 7: Customize for Your Organization**

Create a pack configuration file:

```bash
cat > pack-config.toml << 'EOF'
[enterprise-backend]
distributed_tracing = "jaeger"  # Options: jaeger, zipkin, tempo
security_level = "high"
service_mesh = "istio"

[devops-automation]
ci_provider = "github-actions"  # Options: github-actions, gitlab-ci, jenkins
container_runtime = "docker"
orchestrator = "kubernetes"
monitoring = "prometheus+grafana"

[frontend-modern]
framework = "react"
state_management = "redux"
styling = "tailwind"
EOF
```

Use the configuration:

```bash
ggen generate \
  --template microservices-template \
  --domain platform.ttl \
  --config pack-config.toml \
  --output ./services
```

**Result:** Enterprise-grade platform with microservices, CI/CD, monitoring, and admin UI!

---

## Scenario 3: Data Science Team Building ML Pipeline (12 minutes)

**Context:** Building an ML pipeline for training models, processing data, and deploying predictions.

**Step 1: Install Data Science Pack**

```bash
# Validate
ggen packs validate --pack_id data-science

# Preview
ggen packs install --pack_id data-science --dry_run

# Install each package (example names)
ggen marketplace install data-processing
ggen marketplace install model-training
ggen marketplace install visualization
ggen marketplace install experiment-tracking
ggen marketplace install pipeline-orchestration
```

**Step 2: Add DevOps for MLOps**

```bash
# Add DevOps pack for deployment automation
ggen packs validate --pack_id devops-automation

# Install DevOps packages
ggen marketplace install cicd-pipeline
ggen marketplace install monitoring-alerting
```

**Step 3: Create ML Ontology**

```bash
cat > ml-pipeline.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:DataIngestion
  rdfs:label "DataIngestion" ;
  ex:hasInput ex:RawData ;
  ex:hasOutput ex:CleanData .

ex:FeatureEngineering
  rdfs:label "FeatureEngineering" ;
  ex:hasInput ex:CleanData ;
  ex:hasOutput ex:Features .

ex:ModelTraining
  rdfs:label "ModelTraining" ;
  ex:hasInput ex:Features ;
  ex:hasOutput ex:TrainedModel .

ex:ModelDeployment
  rdfs:label "ModelDeployment" ;
  ex:hasInput ex:TrainedModel ;
  ex:hasOutput ex:PredictionAPI .
EOF
```

**Step 4: Generate Pipeline Components**

```bash
# Data processing pipeline
ggen generate \
  --template data-processing \
  --domain ml-pipeline.ttl \
  --output ./pipeline/ingestion

# Model training code
ggen generate \
  --template model-training \
  --domain ml-pipeline.ttl \
  --framework pytorch \
  --output ./models

# Experiment tracking
ggen generate \
  --template experiment-tracking \
  --domain ml-pipeline.ttl \
  --output ./experiments

# Deployment pipeline
ggen generate \
  --template cicd-pipeline \
  --domain ml-pipeline.ttl \
  --output ./.github/workflows
```

**Step 5: Verify ML Pipeline**

```bash
# Check generated structure
tree -L 2 .

# Expected:
# .
# ├── pipeline/
# │   └── ingestion/
# ├── models/
# ├── experiments/
# └── .github/
#     └── workflows/
```

**Result:** Complete MLOps pipeline with data processing, training, tracking, and deployment!

---

## Advanced: Multi-Pack Composition Strategy

### When to Combine Packs

**Combine packs when:**
- Building full-stack applications (frontend + enterprise-backend)
- Adding DevOps to any project (any pack + devops-automation)
- ML projects needing infrastructure (data-science + devops-automation)
- Startups scaling to enterprise (startup-essentials → enterprise-backend)

### Composition Patterns

**1. Horizontal Scaling (Same Tier)**
```bash
# Web + Mobile + API
frontend-modern + mobile-templates + enterprise-backend
```

**2. Vertical Integration (Cross-Stack)**
```bash
# Frontend → Backend → Database → DevOps
frontend-modern + enterprise-backend + devops-automation
```

**3. Domain-Specific Enhancement**
```bash
# Core + Specialization
startup-essentials + data-science  # SaaS with ML features
enterprise-backend + blockchain    # Enterprise blockchain app
```

### Dependency Resolution

Packs may share dependencies. The marketplace handles this automatically:

```bash
# If both packs depend on 'logging-observability'
ggen marketplace install enterprise-backend  # Installs logging v1.0.0
ggen marketplace install devops-automation   # Reuses logging v1.0.0 (no duplicate)
```

### Pack Versioning Strategy

**Recommended approach:**
1. **Lock versions** in production (`ggen.lock`)
2. **Dry-run updates** before applying
3. **Test in staging** before production
4. **Gradual rollout** for large teams

---

## Verification Checklist

After installing packs, verify everything works:

### ✅ Installation Verification

```bash
# 1. Check all templates installed
ggen marketplace list | jq '.installed_templates'

# 2. Verify template count matches pack count
ggen packs show --pack_id startup-essentials | jq '.package_count'

# 3. Test template generation
ggen generate --template web-api-starter --domain domain.ttl --output /tmp/test
ls -la /tmp/test  # Should show generated files

# 4. Check for conflicts
ggen marketplace list | jq '[.installed_templates[].name] | group_by(.) | map(select(length > 1))'
# Should return empty array (no duplicates)
```

### ✅ Functional Verification

```bash
# 1. Generate code from each template
for template in $(ggen marketplace list | jq -r '.installed_templates[].name'); do
  echo "Testing $template..."
  ggen generate --template $template --domain domain.ttl --output /tmp/verify-$template
done

# 2. Check generated code compiles
cd /tmp/verify-web-api-starter
cargo check  # For Rust
# or
npm install && npm run build  # For TypeScript

# 3. Run tests if included
cargo test  # Rust
npm test    # JavaScript/TypeScript
```

---

## Troubleshooting

### Problem: Pack validation fails

```bash
# Error: Pack 'startup-essentials' validation failed
```

**Solution:**
1. Check internet connection (packs fetch from GitHub)
2. Verify ggen version: `ggen --version` (need ≥ 3.0.0)
3. Clear cache: `rm -rf ~/.ggen/cache && ggen packs validate --pack_id startup-essentials`

### Problem: Template not found after installation

```bash
# Error: Template 'web-api-starter' not found
```

**Solution:**
1. Verify installation: `ggen marketplace list | grep web-api-starter`
2. Check template name spelling (use exact name from pack)
3. Reinstall: `ggen marketplace install web-api-starter`

### Problem: Conflicting dependencies

```bash
# Error: Template A requires logging@1.0, Template B requires logging@2.0
```

**Solution:**
1. Check compatibility: `ggen marketplace info template-a`
2. Upgrade to compatible versions
3. Use separate environments for incompatible packs

### Problem: Dry-run shows different count than actual install

```bash
# Dry-run: 5 packages
# Actual install: 3 packages (2 already installed)
```

**Solution:**
This is **expected behavior**. Dry-run shows total, actual install skips duplicates. Verify with:
```bash
ggen marketplace list | jq '.installed_templates | length'
```

---

## What You've Learned

1. **Installation workflow:** Validate → Dry-run → Install → Verify
2. **Multi-pack composition:** Combine packs for complex projects
3. **Real-world scenarios:** MVP, enterprise platform, ML pipeline
4. **Customization:** Use config files to tailor pack behavior
5. **Verification:** Comprehensive checks for successful setup
6. **Troubleshooting:** Common issues and solutions

**Key Practices:**
- Always validate before installing
- Use dry-run to preview changes
- Verify installation with marketplace list
- Test template generation before production use
- Document your pack composition strategy

## Next Steps

- **Understand pack concepts:** [Packs Concepts Explanation](packs-concepts.md)
- **Browse reference:** [Packs Reference Documentation](packs-reference.md)
- **Create custom packs:** [How to Create Packs](../how-to-guides/create-packs.md)
- **Explore marketplace:** [Marketplace Workflow Tutorial](marketplace-workflow.md)
