<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs Reference](#packs-reference)
  - [Command Reference](#command-reference)
    - [ggen packs list](#ggen-packs-list)
    - [ggen packs show](#ggen-packs-show)
    - [ggen packs validate](#ggen-packs-validate)
    - [ggen packs install](#ggen-packs-install)
  - [Available Packs](#available-packs)
    - [startup-essentials](#startup-essentials)
    - [enterprise-backend](#enterprise-backend)
    - [data-science](#data-science)
    - [devops-automation](#devops-automation)
    - [frontend-modern](#frontend-modern)
  - [Pack Structure](#pack-structure)
    - [Pack Metadata](#pack-metadata)
    - [Package Organization](#package-organization)
    - [Dependencies](#dependencies)
  - [Error Codes](#error-codes)
  - [Configuration Options](#configuration-options)
  - [Environment Variables](#environment-variables)
  - [Exit Codes](#exit-codes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs Reference

**Type: Reference** | [← Back to Documentation](../README.md)

Complete reference documentation for ggen packs commands, available packs, structure, and configuration.

---

## Command Reference

### ggen packs list

**Description:** List all available packs in the marketplace.

**Usage:**
```bash
ggen packs list [OPTIONS]
```

**Options:**
| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--category <CATEGORY>` | String | None | Filter by category (startup, enterprise, ml, devops, frontend) |
| `--format <FORMAT>` | String | json | Output format: json, table, yaml |
| `-h, --help` | Flag | - | Print help information |

**Examples:**

```bash
# List all packs
ggen packs list

# Filter by category
ggen packs list --category startup

# Output as table
ggen packs list --format table
```

**Output Schema (JSON):**
```json
{
  "packs": [
    {
      "id": "string",
      "name": "string",
      "category": "string",
      "description": "string",
      "package_count": number
    }
  ],
  "total": number
}
```

**Exit Codes:**
- `0` - Success
- `1` - General error
- `2` - Network error (cannot fetch pack list)

---

### ggen packs show

**Description:** Show detailed information about a specific pack.

**Usage:**
```bash
ggen packs show --pack_id <PACK_ID> [OPTIONS]
```

**Options:**
| Option | Type | Required | Description |
|--------|------|----------|-------------|
| `--pack_id <PACK_ID>` | String | Yes | Pack identifier (e.g., startup-essentials) |
| `--format <FORMAT>` | String | No | Output format: json, yaml, table |
| `-h, --help` | Flag | No | Print help information |

**Examples:**

```bash
# Show pack details
ggen packs show --pack_id startup-essentials

# YAML output
ggen packs show --pack_id enterprise-backend --format yaml

# Table format
ggen packs show --pack_id data-science --format table
```

**Output Schema (JSON):**
```json
{
  "id": "string",
  "name": "string",
  "category": "string",
  "description": "string",
  "package_count": number,
  "packages": ["string", "string", ...]
}
```

**Exit Codes:**
- `0` - Success
- `1` - Pack not found
- `2` - Invalid pack_id format

---

### ggen packs validate

**Description:** Validate that a pack exists and is well-formed.

**Usage:**
```bash
ggen packs validate --pack_id <PACK_ID>
```

**Options:**
| Option | Type | Required | Description |
|--------|------|----------|-------------|
| `--pack_id <PACK_ID>` | String | Yes | Pack identifier to validate |
| `--strict` | Flag | No | Enable strict validation (check all dependencies) |
| `-h, --help` | Flag | No | Print help information |

**Examples:**

```bash
# Basic validation
ggen packs validate --pack_id startup-essentials

# Strict validation (checks all package availability)
ggen packs validate --pack_id enterprise-backend --strict
```

**Output Schema (JSON):**
```json
{
  "valid": boolean,
  "pack_id": "string",
  "package_count": number,
  "message": "string"
}
```

**Validation Checks:**
1. ✅ Pack metadata exists
2. ✅ All packages are defined
3. ✅ Package count matches manifest
4. ✅ No circular dependencies (--strict mode)
5. ✅ All packages are accessible (--strict mode)

**Exit Codes:**
- `0` - Pack is valid
- `1` - Pack validation failed
- `2` - Pack not found

---

### ggen packs install

**Description:** Install all packages from a pack.

**Usage:**
```bash
ggen packs install --pack_id <PACK_ID> [OPTIONS]
```

**Options:**
| Option | Type | Required | Description |
|--------|------|----------|-------------|
| `--pack_id <PACK_ID>` | String | Yes | Pack identifier to install |
| `--dry_run` | Flag | No | Show what would be installed without installing |
| `--force` | Flag | No | Reinstall even if already installed |
| `--skip-dependencies` | Flag | No | Skip installing dependencies |
| `-h, --help` | Flag | No | Print help information |

**Examples:**

```bash
# Dry-run (preview)
ggen packs install --pack_id startup-essentials --dry_run

# Actual installation (note: currently shows preview only)
ggen packs install --pack_id startup-essentials

# Force reinstall
ggen packs install --pack_id enterprise-backend --force
```

**Output Schema (JSON):**
```json
{
  "pack_id": "string",
  "pack_name": "string",
  "total_packages": number,
  "packages_to_install": ["string", ...],
  "status": "string"
}
```

**Installation Process:**
1. Validate pack exists
2. Resolve dependencies
3. Check disk space
4. Download packages (currently manual via marketplace)
5. Install each package
6. Verify installation

**Exit Codes:**
- `0` - Installation preview successful
- `1` - Installation failed
- `2` - Pack not found
- `3` - Disk space insufficient
- `4` - Dependency resolution failed

**Current Limitation:**
Pack installation currently displays packages to install. Use `ggen marketplace install <package>` for each package individually.

---

## Available Packs

### startup-essentials

**Category:** Startup
**Package Count:** 5
**Maturity:** Production-ready

**Description:**
Essential packages for early-stage startups: CLI templates, web frameworks, database tools.

**Packages:**
| Package | Description | Language |
|---------|-------------|----------|
| `noun-verb-cli` | Command-line tool template following Unix conventions | Rust, TypeScript |
| `web-api-starter` | REST API server foundation with CRUD operations | Rust, TypeScript, Python |
| `postgres-migrations` | Database schema management and versioning | SQL, Rust |
| `user-auth-basic` | User authentication (JWT, session-based) | Rust, TypeScript |
| `logging-observability` | Structured logging and metrics collection | Rust, TypeScript |

**Use Cases:**
- Building MVPs rapidly
- Solo founder projects
- Hackathons and prototypes
- Startups validating product-market fit

**Example Command:**
```bash
ggen packs show --pack_id startup-essentials
```

---

### enterprise-backend

**Category:** Enterprise
**Package Count:** 5
**Maturity:** Production-ready

**Description:**
Production-ready backend stack: microservices, distributed tracing, advanced security.

**Packages:**
| Package | Description | Language |
|---------|-------------|----------|
| `microservices-template` | Service mesh architecture with gRPC/REST | Rust, Go, Java |
| `distributed-tracing` | OpenTelemetry integration for observability | Rust, Go |
| `advanced-security` | OAuth2, RBAC, encryption patterns | Rust, Go |
| `ha-configuration` | High-availability configurations (load balancing, failover) | Kubernetes, Docker |
| `enterprise-integrations` | Kafka, RabbitMQ, Redis integration | Rust, Go, Java |

**Use Cases:**
- Large-scale production systems
- Financial services
- Healthcare applications
- E-commerce platforms
- Enterprise SaaS

**Example Command:**
```bash
ggen packs show --pack_id enterprise-backend
```

---

### data-science

**Category:** Machine Learning
**Package Count:** 5
**Maturity:** Beta

**Description:**
ML/AI development stack: data processing, model training, visualization.

**Packages:**
| Package | Description | Language |
|---------|-------------|----------|
| `data-processing` | ETL pipelines, data cleaning, feature engineering | Python, Rust |
| `model-training` | PyTorch/TensorFlow training templates | Python |
| `visualization` | Matplotlib, Plotly, interactive dashboards | Python, JavaScript |
| `experiment-tracking` | MLflow integration for tracking experiments | Python |
| `pipeline-orchestration` | Airflow DAGs, Prefect workflows | Python |

**Use Cases:**
- Machine learning projects
- Data analytics platforms
- Predictive modeling
- Research and experimentation
- MLOps pipelines

**Example Command:**
```bash
ggen packs show --pack_id data-science
```

---

### devops-automation

**Category:** DevOps
**Package Count:** 5
**Maturity:** Production-ready

**Description:**
Infrastructure automation: CI/CD, container orchestration, monitoring.

**Packages:**
| Package | Description | Language |
|---------|-------------|----------|
| `cicd-pipeline` | GitHub Actions, GitLab CI, Jenkins pipelines | YAML, Groovy |
| `container-orchestration` | Kubernetes manifests, Helm charts, Docker Compose | YAML, Kubernetes |
| `monitoring-alerting` | Prometheus, Grafana, AlertManager configs | YAML, PromQL |
| `infra-provisioning` | Terraform modules, CloudFormation templates | HCL, YAML |
| `config-management` | Ansible playbooks, Chef recipes | YAML, Ruby |

**Use Cases:**
- DevOps automation
- Infrastructure as Code
- Continuous deployment
- Cloud migrations
- Platform engineering

**Example Command:**
```bash
ggen packs show --pack_id devops-automation
```

---

### frontend-modern

**Category:** Frontend
**Package Count:** 5
**Maturity:** Production-ready

**Description:**
Modern web UI stack: React/Vue components, state management, styling.

**Packages:**
| Package | Description | Language |
|---------|-------------|----------|
| `react-components` | Reusable React component library | TypeScript, JavaScript |
| `state-management` | Redux, Zustand, Recoil templates | TypeScript |
| `styling-system` | Tailwind, CSS-in-JS, design tokens | CSS, TypeScript |
| `build-optimization` | Vite, Webpack, bundle optimization | JavaScript |
| `testing-utilities` | Jest, Testing Library, Cypress | TypeScript, JavaScript |

**Use Cases:**
- Modern web applications
- Single-page applications (SPA)
- Progressive web apps (PWA)
- Admin dashboards
- Customer portals

**Example Command:**
```bash
ggen packs show --pack_id frontend-modern
```

---

## Pack Structure

### Pack Metadata

Every pack includes metadata defining its properties:

```json
{
  "id": "unique-pack-id",
  "name": "Human Readable Name",
  "category": "startup|enterprise|ml|devops|frontend",
  "description": "Detailed description of pack purpose",
  "package_count": 5,
  "packages": ["package-1", "package-2", ...],
  "version": "1.0.0",
  "maturity": "prototype|alpha|beta|stable|production",
  "author": "Author Name",
  "license": "MIT|Apache-2.0|...",
  "repository": "https://github.com/org/repo",
  "documentation": "https://docs.example.com"
}
```

**Field Descriptions:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | String | Yes | Unique identifier (kebab-case) |
| `name` | String | Yes | Human-readable name |
| `category` | String | Yes | Pack category for filtering |
| `description` | String | Yes | Brief description (max 200 chars) |
| `package_count` | Number | Yes | Number of packages in pack |
| `packages` | Array | Yes | List of package identifiers |
| `version` | String | Yes | Semantic version (semver) |
| `maturity` | String | No | Development stage |
| `author` | String | No | Pack author/maintainer |
| `license` | String | No | License identifier |
| `repository` | String | No | Source code repository URL |
| `documentation` | String | No | Documentation URL |

### Package Organization

Packs follow a structured organization:

```
pack-id/
├── metadata.json       # Pack metadata
├── packages/           # Package definitions
│   ├── package-1/
│   │   ├── manifest.json
│   │   └── templates/
│   ├── package-2/
│   └── package-3/
├── dependencies.json   # Dependency tree
└── README.md          # Pack documentation
```

### Dependencies

Packs can declare dependencies on other packs or packages:

```json
{
  "dependencies": {
    "base-templates": "^1.0.0",
    "logging-utils": ">=2.0.0 <3.0.0"
  },
  "dev_dependencies": {
    "test-helpers": "^1.5.0"
  }
}
```

**Dependency Resolution:**
- Follows semantic versioning (semver)
- Transitive dependencies automatically resolved
- Circular dependencies detected and rejected
- Version conflicts reported during validation

---

## Error Codes

| Code | Name | Description | Resolution |
|------|------|-------------|------------|
| `PACK_NOT_FOUND` | Pack Not Found | Specified pack_id does not exist | Check spelling, use `ggen packs list` |
| `PACK_INVALID` | Pack Invalid | Pack metadata is malformed | Report issue to pack maintainer |
| `PACK_VALIDATION_FAILED` | Validation Failed | Pack failed validation checks | Review validation output for details |
| `PACKAGE_NOT_FOUND` | Package Not Found | Package in pack does not exist | Pack may be outdated, contact maintainer |
| `DEPENDENCY_CONFLICT` | Dependency Conflict | Version conflict in dependencies | Use `--force` or resolve manually |
| `NETWORK_ERROR` | Network Error | Cannot fetch pack data | Check internet connection |
| `DISK_SPACE_ERROR` | Disk Space Error | Insufficient disk space | Free up space or use different location |
| `PERMISSION_ERROR` | Permission Error | Cannot write to install location | Check permissions, use sudo if needed |

**Error Output Format:**
```json
{
  "error": {
    "code": "PACK_NOT_FOUND",
    "message": "Pack 'invalid-pack' not found",
    "details": {
      "pack_id": "invalid-pack",
      "available_packs": ["startup-essentials", "enterprise-backend", ...]
    },
    "resolution": "Check spelling or use 'ggen packs list' to see available packs"
  }
}
```

---

## Configuration Options

### Pack Configuration File

Create `pack-config.toml` to customize pack behavior:

```toml
[pack]
id = "startup-essentials"
version = "1.0.0"

[installation]
skip_optional = false      # Skip optional packages
verify_checksums = true    # Verify package integrity
parallel_downloads = 4     # Concurrent downloads

[customization]
language = "rust"          # Default language for multi-language packs
framework = "actix-web"    # Preferred framework
database = "postgres"      # Database choice

[startup-essentials]
auth_method = "jwt"        # jwt|session|oauth
logging_level = "info"     # debug|info|warn|error
api_port = 8080

[enterprise-backend]
service_mesh = "istio"     # istio|linkerd|consul
tracing_backend = "jaeger" # jaeger|zipkin|tempo
security_level = "high"    # low|medium|high
```

Use with:
```bash
ggen packs install --pack_id startup-essentials --config pack-config.toml
```

---

## Environment Variables

| Variable | Description | Default | Example |
|----------|-------------|---------|---------|
| `GGEN_PACKS_REGISTRY` | URL for pack registry | GitHub marketplace | `https://custom-registry.com` |
| `GGEN_CACHE_DIR` | Cache directory for downloads | `~/.ggen/cache` | `/tmp/ggen-cache` |
| `GGEN_INSTALL_DIR` | Installation directory | `~/.ggen/templates` | `~/my-templates` |
| `GGEN_PARALLEL_DOWNLOADS` | Concurrent downloads | `4` | `8` |
| `GGEN_VERIFY_CHECKSUMS` | Verify package integrity | `true` | `false` |
| `GGEN_OFFLINE_MODE` | Use cached packs only | `false` | `true` |

**Example:**
```bash
export GGEN_CACHE_DIR=/tmp/ggen-cache
export GGEN_PARALLEL_DOWNLOADS=8
ggen packs install --pack_id startup-essentials
```

---

## Exit Codes

| Code | Meaning | Description |
|------|---------|-------------|
| `0` | Success | Command completed successfully |
| `1` | General Error | Unspecified error occurred |
| `2` | Network Error | Cannot connect to registry |
| `3` | Validation Error | Pack validation failed |
| `4` | Not Found Error | Pack or package not found |
| `5` | Conflict Error | Version or dependency conflict |
| `6` | Permission Error | Insufficient permissions |
| `7` | Disk Space Error | Insufficient disk space |
| `8` | Checksum Error | Package integrity check failed |

**Example Usage:**
```bash
ggen packs validate --pack_id startup-essentials
if [ $? -eq 0 ]; then
  echo "Pack is valid"
  ggen packs install --pack_id startup-essentials
else
  echo "Pack validation failed with code $?"
fi
```

---

**Related Documentation:**
- [Packs Getting Started](packs-getting-started.md)
- [Packs Install & Compose](packs-install-compose.md)
- [Packs Concepts](packs-concepts.md)
- [Marketplace Documentation](../explanations/marketplace.md)
