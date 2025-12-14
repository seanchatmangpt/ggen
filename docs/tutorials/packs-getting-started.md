<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Getting Started with Packs](#getting-started-with-packs)
  - [Prerequisites](#prerequisites)
  - [Step 1: Discover Available Packs (2 minutes)](#step-1-discover-available-packs-2-minutes)
  - [Step 2: Explore a Pack (2 minutes)](#step-2-explore-a-pack-2-minutes)
  - [Step 3: Validate a Pack (1 minute)](#step-3-validate-a-pack-1-minute)
  - [Step 4: Preview Installation (2 minutes)](#step-4-preview-installation-2-minutes)
  - [Step 5: Understand Pack Categories (3 minutes)](#step-5-understand-pack-categories-3-minutes)
    - [Startup Essentials (Quick MVP Development)](#startup-essentials-quick-mvp-development)
    - [Enterprise Backend (Production-Grade Systems)](#enterprise-backend-production-grade-systems)
    - [Data Science Toolkit (ML/AI Development)](#data-science-toolkit-mlai-development)
    - [DevOps Automation (Infrastructure as Code)](#devops-automation-infrastructure-as-code)
    - [Modern Frontend (Web UI Development)](#modern-frontend-web-ui-development)
  - [What You've Learned](#what-youve-learned)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Getting Started with Packs

**Type: Tutorial** | [← Back to Documentation](../README.md) | ⏱️ **10 minutes**

**Goal:** Understand the ggen packs ecosystem and explore available pack bundles in 10 minutes.

**What you'll learn:** What packs are, why they matter, how to discover and validate them, and when to use packs vs individual templates.

---

## Prerequisites

- ggen installed (`ggen --version` works)
- Basic command-line familiarity
- Time: 10 minutes

## Step 1: Discover Available Packs (2 minutes)

Packs are **curated bundles of related templates** designed to accelerate common development scenarios. Think of them as "starter kits" that bring together everything you need for a specific use case.

Let's see what's available:

```bash
ggen packs list
```

**Output:**
```json
{
  "packs": [
    {
      "category": "startup",
      "description": "Essential packages for early-stage startups: CLI templates, web frameworks, database tools",
      "id": "startup-essentials",
      "name": "Startup Essentials",
      "package_count": 5
    },
    {
      "category": "enterprise",
      "description": "Production-ready backend stack: microservices, distributed tracing, advanced security",
      "id": "enterprise-backend",
      "name": "Enterprise Backend",
      "package_count": 5
    },
    {
      "category": "ml",
      "description": "ML/AI development stack: data processing, model training, visualization",
      "id": "data-science",
      "name": "Data Science Toolkit",
      "package_count": 5
    },
    {
      "category": "devops",
      "description": "Infrastructure automation: CI/CD, container orchestration, monitoring",
      "id": "devops-automation",
      "name": "DevOps Automation",
      "package_count": 5
    },
    {
      "category": "frontend",
      "description": "Modern web UI stack: React/Vue components, state management, styling",
      "id": "frontend-modern",
      "name": "Modern Frontend",
      "package_count": 5
    }
  ],
  "total": 5
}
```

**What you see:**
- **5 packs** covering different domains (startup, enterprise, ML, DevOps, frontend)
- Each pack contains **5 related templates**
- Clear descriptions of what each pack provides

## Step 2: Explore a Pack (2 minutes)

Let's dive into the **Startup Essentials** pack to see what it includes:

```bash
ggen packs show --pack_id startup-essentials
```

**Output:**
```json
{
  "category": "startup",
  "description": "Essential packages for early-stage startups: CLI templates, web frameworks, database tools",
  "id": "startup-essentials",
  "name": "Startup Essentials",
  "package_count": 5,
  "packages": [
    "noun-verb-cli",
    "web-api-starter",
    "postgres-migrations",
    "user-auth-basic",
    "logging-observability"
  ]
}
```

**What's included:**
1. **noun-verb-cli** - Command-line tool template following Unix conventions
2. **web-api-starter** - REST API server foundation
3. **postgres-migrations** - Database schema management
4. **user-auth-basic** - User authentication system
5. **logging-observability** - Structured logging and metrics

**Why this matters:** Instead of installing 5 templates individually, packs give you a **cohesive, tested combination** that works together.

## Step 3: Validate a Pack (1 minute)

Before using a pack, verify it's well-formed and all packages are available:

```bash
ggen packs validate --pack_id startup-essentials
```

**Output:**
```json
{
  "valid": true,
  "pack_id": "startup-essentials",
  "package_count": 5,
  "message": "Pack 'Startup Essentials' is valid with 5 packages"
}
```

**What this tells you:**
- ✅ Pack metadata is correct
- ✅ All 5 packages exist and are accessible
- ✅ Dependencies are resolvable
- ✅ Safe to install

## Step 4: Preview Installation (2 minutes)

See what would be installed **without actually installing**:

```bash
ggen packs install --pack_id startup-essentials --dry_run
```

**Output:**
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
  "status": "Ready to install 5 packages from pack 'Startup Essentials' (actual installation not implemented - use 'ggen marketplace install <package>' for each package)"
}
```

**Key insight:** The `--dry_run` flag shows exactly what would happen, helping you:
- Preview all packages before commitment
- Understand dependencies
- Verify disk space requirements
- Plan your installation

## Step 5: Understand Pack Categories (3 minutes)

Let's explore what each pack category offers:

### Startup Essentials (Quick MVP Development)
```bash
ggen packs show --pack_id startup-essentials
```

**Use when:** Building an MVP, need to ship fast, want proven patterns
- CLI tools for user interaction
- Web API for backend services
- Database migrations for data persistence
- Basic auth for user management
- Logging for observability

### Enterprise Backend (Production-Grade Systems)
```bash
ggen packs show --pack_id enterprise-backend
```

**Use when:** Building scalable production systems, need advanced features
- Microservices architecture
- Distributed tracing
- Advanced security patterns
- High-availability configurations
- Enterprise integrations

### Data Science Toolkit (ML/AI Development)
```bash
ggen packs show --pack_id data-science
```

**Use when:** Building ML models, data pipelines, analytics platforms
- Data processing frameworks
- Model training templates
- Visualization components
- Experiment tracking
- Pipeline orchestration

### DevOps Automation (Infrastructure as Code)
```bash
ggen packs show --pack_id devops-automation
```

**Use when:** Automating deployments, managing infrastructure
- CI/CD pipelines
- Container orchestration (Docker, Kubernetes)
- Monitoring and alerting
- Infrastructure provisioning
- Configuration management

### Modern Frontend (Web UI Development)
```bash
ggen packs show --pack_id frontend-modern
```

**Use when:** Building modern web applications
- React/Vue component libraries
- State management (Redux, Vuex)
- Styling systems (Tailwind, CSS-in-JS)
- Build optimization
- Testing utilities

## What You've Learned

1. **Packs are bundles:** Curated sets of related templates for specific use cases
2. **Categories map to domains:** Startup, enterprise, ML, DevOps, frontend
3. **Validation ensures quality:** Check pack integrity before installation
4. **Dry-run prevents surprises:** Preview installations before committing
5. **Strategic selection:** Choose packs based on your project goals

**Key Concepts:**
- **Pack** = Multiple related templates bundled together
- **Category** = Domain-specific grouping (startup, enterprise, etc.)
- **Validation** = Verify pack integrity and package availability
- **Dry-run** = Preview installation without making changes
- **Composition** = Combine multiple packs for complex projects (covered in next tutorial)

## Next Steps

- **Install and compose packs:** [Packs Install & Compose Tutorial](packs-install-compose.md)
- **Learn pack concepts:** [Packs Concepts Explanation](packs-concepts.md)
- **Browse reference:** [Packs Reference Documentation](packs-reference.md)
- **Understand marketplace:** [Marketplace Workflow Tutorial](marketplace-workflow.md)
