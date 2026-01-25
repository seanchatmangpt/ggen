<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN Marketplace - Comprehensive Technical Guide](#ggen-marketplace---comprehensive-technical-guide)
  - [Table of Contents](#table-of-contents)
  - [Architecture Overview](#architecture-overview)
    - [Key Components](#key-components)
      - [1. **ggen-marketplace Crate** (`/crates/ggen-marketplace`)](#1-ggen-marketplace-crate-cratesggen-marketplace)
      - [2. **ggen-domain Marketplace Module** (`/crates/ggen-domain/src/marketplace`)](#2-ggen-domain-marketplace-module-cratesggen-domainsrcmarketplace)
      - [3. **ggen-cli Marketplace Commands** (`/crates/ggen-cli/src/cmds/marketplace.rs`)](#3-ggen-cli-marketplace-commands-cratesggen-clisrccmdsmarketplacers)
    - [Architecture Diagram](#architecture-diagram)
  - [Package Discovery & Search](#package-discovery--search)
    - [How It Works](#how-it-works)
    - [CLI Commands](#cli-commands)
      - [Search Packages](#search-packages)
      - [Search by Maturity](#search-by-maturity)
      - [Get Recommendations](#get-recommendations)
  - [Template Installation](#template-installation)
    - [Installation Process](#installation-process)
    - [CLI Commands](#cli-commands-1)
      - [Basic Installation](#basic-installation)
      - [Installation Options](#installation-options)
      - [List Installed Packages](#list-installed-packages)
    - [Installation Structure](#installation-structure)
  - [Creating & Publishing Templates](#creating--publishing-templates)
    - [Package Creation Workflow](#package-creation-workflow)
      - [Step 1: Create Package Structure](#step-1-create-package-structure)
      - [Step 2: Write package.toml](#step-2-write-packagetoml)
      - [Step 3: Write Comprehensive README.md](#step-3-write-comprehensive-readmemd)
      - [Step 4: Register Package](#step-4-register-package)
      - [Step 5: Test Locally](#step-5-test-locally)
      - [Step 6: Publish](#step-6-publish)
  - [Package Metadata](#package-metadata)
    - [package.toml Schema](#packagetoml-schema)
      - [Required Fields](#required-fields)
      - [Optional Fields](#optional-fields)
    - [Advanced Metadata Examples](#advanced-metadata-examples)
      - [REST API Template](#rest-api-template)
  - [Marketplace Registry](#marketplace-registry)
    - [Registry File Format](#registry-file-format)
      - [Location](#location)
      - [Header Metadata](#header-metadata)
      - [Package Entries](#package-entries)
    - [Registry Operations](#registry-operations)
      - [Loading Registry](#loading-registry)
      - [Registry Validation](#registry-validation)
  - [CLI Command Reference](#cli-command-reference)
    - [Summary of 19 Commands](#summary-of-19-commands)
    - [Detailed Command Docs](#detailed-command-docs)
      - [Search](#search)
      - [Install](#install)
      - [List](#list)
      - [Publish](#publish)
      - [Validate & Maturity](#validate--maturity)
      - [Recommendations & Comparison](#recommendations--comparison)
      - [Export & Reporting](#export--reporting)
      - [Bundle Operations](#bundle-operations)
  - [Authentication & Security](#authentication--security)
    - [Package Verification](#package-verification)
      - [Checksum Verification](#checksum-verification)
      - [Ed25519 Signature Support](#ed25519-signature-support)
      - [Installation Security](#installation-security)
    - [Package Validation](#package-validation)
      - [Validation Criteria](#validation-criteria)
      - [Scoring](#scoring)
      - [Validation Commands](#validation-commands)
    - [Authentication & Permissions](#authentication--permissions)
  - [Available Templates (Examples)](#available-templates-examples)
    - [Core Power Packages (Most Popular)](#core-power-packages-most-popular)
      - [1. REST API Template](#1-rest-api-template)
      - [2. Advanced Rust API 8020](#2-advanced-rust-api-8020)
      - [3. GraphQL API Template](#3-graphql-api-template)
      - [4. Microservices Architecture](#4-microservices-architecture)
    - [By Category](#by-category)
      - [Templates (Code Generation)](#templates-code-generation)
      - [Academic Packages](#academic-packages)
      - [Domain-Specific](#domain-specific)
      - [Infrastructure](#infrastructure)
    - [Statistics](#statistics)
  - [Key Features](#key-features)
    - [For Users](#for-users)
    - [For Publishers](#for-publishers)
    - [Advanced Capabilities](#advanced-capabilities)
  - [Registry URLs](#registry-urls)
    - [Production Registry](#production-registry)
    - [Local Registry](#local-registry)
  - [Implementation Details](#implementation-details)
    - [Search Scoring Algorithm](#search-scoring-algorithm)
    - [Dependency Resolution Algorithm](#dependency-resolution-algorithm)
    - [Cache Management](#cache-management)
  - [Troubleshooting](#troubleshooting)
    - [Package Not Found](#package-not-found)
    - [Installation Fails](#installation-fails)
    - [Template Generation Issues](#template-generation-issues)
    - [Lifecycle Errors](#lifecycle-errors)
  - [Configuration](#configuration)
    - [Global Settings](#global-settings)
    - [Cache Configuration](#cache-configuration)
  - [Programmatic API (Rust)](#programmatic-api-rust)
  - [Standards & Best Practices](#standards--best-practices)
    - [Naming Conventions](#naming-conventions)
    - [Description Writing](#description-writing)
    - [Version Management](#version-management)
  - [Future Enhancements](#future-enhancements)
  - [References](#references)
    - [Documentation Files](#documentation-files)
    - [Source Code](#source-code)
    - [Example Packages](#example-packages)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN Marketplace - Comprehensive Technical Guide

## Table of Contents
1. [Architecture Overview](#architecture-overview)
2. [Package Discovery & Search](#package-discovery--search)
3. [Template Installation](#template-installation)
4. [Creating & Publishing Templates](#creating--publishing-templates)
5. [Package Metadata (package.toml)](#package-metadata)
6. [Marketplace Registry](#marketplace-registry)
7. [CLI Command Reference](#cli-command-reference)
8. [Authentication & Security](#authentication--security)
9. [Available Templates (Examples)](#available-templates)

---

## Architecture Overview

The ggen marketplace is a **production-grade package registry system** designed for distributing and managing code generation templates, utilities, and AI integrations.

### Key Components

#### 1. **ggen-marketplace Crate** (`/crates/ggen-marketplace`)
A standalone Rust library providing:
- **LocalRegistry**: File-system based package discovery
- **TantivySearchEngine**: Full-text search with filtering and faceting
- **FilesystemStore / MemoryStore**: Content-addressable storage backends
- **Ed25519Verifier**: Cryptographic package verification
- **TemplateSearchEngine**: Specialized template discovery
- **Quality Scoring & Maturity Assessment**: Package evaluation metrics

#### 2. **ggen-domain Marketplace Module** (`/crates/ggen-domain/src/marketplace`)
Domain logic separated from CLI for testability and reusability:
- `search.rs` - Search algorithms with fuzzy matching
- `install.rs` - Installation logic with dependency resolution
- `registry.rs` - Registry management and caching
- `publish.rs` - Publishing workflow
- `validate.rs` - Quality validation and production-readiness checks
- `types.rs` - Poka-yoke validated types (prevents invalid states at compile time)
- `maturity_evaluator.rs` - 6-dimension maturity assessment

#### 3. **ggen-cli Marketplace Commands** (`/crates/ggen-cli/src/cmds/marketplace.rs`)
19 verb-based CLI commands using clap-noun-verb pattern for marketplace operations.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────┐
│                    ggen CLI Marketplace                 │
│                (19 verb commands in marketplace.rs)     │
└──────────────────────┬──────────────────────────────────┘
                       │
        ┌──────────────┴──────────────┐
        ▼                             ▼
┌──────────────────┐        ┌──────────────────┐
│  ggen-domain     │        │ ggen-marketplace │
│  Marketplace Mod │◄───────┤ Crate (Lib)      │
│ (Domain Logic)   │        │ (Standalone)     │
└────────┬─────────┘        └────────┬─────────┘
         │                           │
         ├─ search.rs               │
         ├─ install.rs              ├─ LocalRegistry
         ├─ publish.rs              ├─ TantivySearchEngine
         ├─ registry.rs             ├─ TemplateSearchEngine
         ├─ validate.rs             ├─ Storage (FS/Memory)
         ├─ types.rs                ├─ Crypto (Ed25519)
         └─ maturity_*.rs           └─ Quality Assessment
         
         │
         ▼
┌──────────────────────────────────┐
│ Registry                         │
│ (marketplace/registry/)          │
│ - packages.toml (TOML registry)  │
│ - index.json (JSON index)        │
└──────────────────────────────────┘
```

---

## Package Discovery & Search

### How It Works

1. **Registry Loading**: Registry is TOML file located at:
   - **Local**: `marketplace/registry/packages.toml`
   - **Remote**: `https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml`

2. **Search Mechanism**:
   - Full-text search via Tantivy engine
   - Scoring based on: name match, description match, tags/keywords, popularity
   - Fuzzy matching with 70% similarity threshold
   - Caching with LRU eviction policy (default capacity: 1000)

3. **Search Relevance Scoring**:
   ```
   Exact name match:           100.0 points
   Name contains query:         50.0 points
   Fuzzy name match:    30.0 × similarity
   Description match:           20.0 points
   Tag/keyword match:           10.0 per match
   Download boost:        0-10 (normalized)
   Star boost:             0-5 (normalized)
   ```

### CLI Commands

#### Search Packages
```bash
# Basic search
ggen marketplace search --query "rust api"

# With category filter
ggen marketplace search --query "api" --category templates

# Limit results
ggen marketplace search --query "microservice" --limit 5
```

#### Search by Maturity
```bash
# Filter by maturity dimensions
ggen marketplace search-maturity \
  --min-level production \
  --min-security 15 \
  --min-testing 18

# Exclude low-maintenance packages
ggen marketplace search-maturity \
  --min-level production \
  --exclude-maintenance-low
```

#### Get Recommendations
```bash
# Production-ready recommendations
ggen marketplace recommend --use-case production

# Research recommendations with custom score
ggen marketplace recommend --use-case research --min-score 40

# Security-focused recommendations
ggen marketplace recommend --priority security --min-dimension-score 18
```

---

## Template Installation

### Installation Process

1. **Resolve Package**: Fetch metadata from registry
2. **Verify Integrity**: Check SHA256 checksums (when provided)
3. **Dependency Resolution**: Recursively install dependencies
4. **Extract**: Unzip/copy package contents to target directory
5. **Post-Install**: Run lifecycle hooks (if defined in make.toml)

### CLI Commands

#### Basic Installation
```bash
# Install latest version to current directory
ggen marketplace install --package "advanced-rust-api-8020"

# Install to specific directory
ggen marketplace install --package "rust-cli-template" --target ./my-cli

# Install specific version
ggen marketplace install --package "rest-api-template@1.0.0"
```

#### Installation Options
```bash
# Dry-run (preview without installing)
ggen marketplace install --package "package-name" --dry-run

# Force overwrite existing files
ggen marketplace install --package "package-name" --force

# Skip dependencies
ggen marketplace install --package "package-name" --no-dependencies

# Quiet mode (minimal output)
ggen marketplace install --package "package-name"
```

#### List Installed Packages
```bash
# List all packages
ggen marketplace list

# Detailed view
ggen marketplace list --detailed

# JSON output
ggen marketplace list --json

# Filter by maturity
ggen marketplace list --min-maturity production

# Sort packages
ggen marketplace list --sort maturity
```

### Installation Structure

After installation, packages follow this structure:
```
~/.ggen/packages/package-name/
├── package.toml          # Package metadata
├── README.md             # Documentation
├── make.toml             # Lifecycle phases
├── src/                  # Source code
├── templates/            # Code generation templates
├── data/                 # SPARQL/RDF specifications
├── tests/                # Test suite
├── examples/             # Usage examples
└── docs/                 # Additional documentation
```

---

## Creating & Publishing Templates

### Package Creation Workflow

#### Step 1: Create Package Structure
```bash
mkdir -p marketplace/packages/my-template
cd marketplace/packages/my-template

# Create minimal structure
touch package.toml
touch README.md
mkdir -p src templates
touch src/lib.rs
touch src/main.rs
touch templates/example.hbs
```

#### Step 2: Write package.toml
```toml
[package]
name = "my-template"
version = "0.1.0"
description = "Brief description of your template"
category = "templates"  # templates, utilities, ai, frameworks
author = "your-username"
license = "MIT"

# Optional metadata
repository = "https://github.com/you/my-template"
homepage = "https://my-template.example.com"
documentation = "https://docs.example.com"

# Features list
features = [
    "Production-ready code generation",
    "Comprehensive error handling",
    "Docker containerization"
]

# Tags for discoverability
tags = ["rust", "api", "rest", "template", "production"]

# Keywords for search
keywords = ["api", "rest", "axum", "template"]

# Dependencies on other marketplace packages
dependencies = []

[package.metadata]
min_ggen_version = "2.5.0"
stability = "stable"  # alpha, beta, stable
production_ready = true
```

#### Step 3: Write Comprehensive README.md
```markdown
# My Template

Brief one-line description.

## Features
- Feature 1
- Feature 2

## Installation
\`\`\`bash
ggen marketplace install "my-template"
\`\`\`

## Quick Start
\`\`\`bash
cd my-template
ggen template generate templates/example.hbs --output src/
\`\`\`

## Usage Examples
Show real-world usage...

## Configuration
Document configuration options...

## Testing
Instructions for running tests...

## Contributing
How to contribute...

## License
MIT
```

#### Step 4: Register Package
Add to `marketplace/registry/packages.toml`:

```toml
[[package]]
name = "my-template"
full_name = "username/my-template"
version = "0.1.0"
description = "Brief description"
category = "templates"
author = "username"
repository = "https://github.com/seanchatmangpt/ggen"
download_url = "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip"
path = "marketplace/packages/my-template"
license = "MIT"
dependencies = []
features = ["Feature 1", "Feature 2"]
tags = ["rust", "template"]
keywords = ["keyword1", "keyword2"]
```

#### Step 5: Test Locally
```bash
# Dry-run installation
ggen marketplace install "my-template" --dry-run

# Verify package structure
ggen marketplace validate --package "my-template"

# Check maturity score
ggen marketplace maturity "io.ggen.my-template"
```

#### Step 6: Publish

**Method 1: CLI Publishing (Recommended)**
```bash
# Navigate to package
cd marketplace/packages/my-template

# Dry-run first
ggen marketplace publish --dry-run

# Publish with tag
ggen marketplace publish --tag v0.1.0
```

**Method 2: Manual via GitHub**
```bash
# Fork repository
gh repo fork seanchatmangpt/ggen

# Create branch
git checkout -b add-my-template

# Add files
cp -r my-template marketplace/packages/

# Register in packages.toml
# Commit and push
git add marketplace/
git commit -m "Add my-template to marketplace"
git push origin add-my-template

# Create PR
gh pr create --title "Add my-template"
```

---

## Package Metadata

### package.toml Schema

#### Required Fields
| Field | Type | Description | Max Length |
|-------|------|-------------|-----------|
| `name` | string | Unique identifier (lowercase, hyphens) | 50 |
| `version` | semver | e.g., "1.0.0" | 20 |
| `description` | string | Brief description | 200 |
| `author` | string | Package author/maintainer | 100 |
| `license` | string | SPDX license ID | 50 |

#### Optional Fields
```toml
[package]
full_name = "owner/repo"          # owner/repo format
category = "templates"             # templates, utilities, ai, frameworks
repository = "https://..."         # Git repository URL
homepage = "https://..."           # Project homepage
documentation = "https://..."      # Docs URL
download_url = "https://..."       # Direct download link
path = "marketplace/packages/..."  # Path in repository
changelog = "CHANGELOG.md"         # Changelog file

features = [
    "Feature 1",
    "Feature 2"
]

tags = ["tag1", "tag2"]            # Searchable tags
keywords = ["keyword1"]            # Search keywords
dependencies = ["pkg1@1.0"]        # Marketplace dependencies

[package.metadata]
min_ggen_version = "2.5.0"         # Minimum ggen version
max_ggen_version = "3.0.0"         # Maximum ggen version
stability = "stable"               # alpha, beta, stable
deprecated = false                 # Deprecation flag
replacement = "new-package"        # Replacement if deprecated
is_8020 = true                     # 80/20 bundle indicator
is_8020_certified = true           # Passed all validation
dark_matter_reduction_target = "70%"  # Measurable claim
sector = "microservice"             # For bundle composition
```

### Advanced Metadata Examples

#### REST API Template
```toml
[package]
name = "rest-api-template"
version = "1.0.0"

[ontology]
primary = "ontology/rest-api.ttl"
format = "turtle"
validation = "shacl"

[generation]
languages = ["rust", "typescript", "python"]
frameworks = { rust = "axum", typescript = "express", python = "fastapi" }

[generation.rust]
template_dir = "templates/rust"
entry_point = "main.rs"
dependencies = ["axum = '0.7'", "tokio = '1'"]

[testing]
framework = "chicago-tdd"
coverage_threshold = 80

[performance]
startup_time_ms = 150
response_time_p99_ms = 10
throughput_rps = 5000
```

---

## Marketplace Registry

### Registry File Format

#### Location
- **Local**: `marketplace/registry/packages.toml`
- **Remote**: `https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml`
- **JSON Index**: `marketplace/registry/index.json`

#### Header Metadata
```toml
version = "1.0.0"
registry_url = "https://github.com/seanchatmangpt/ggen"
raw_base_url = "https://raw.githubusercontent.com/seanchatmangpt/ggen/master"
pages_url = "https://seanchatmangpt.github.io/ggen"
```

#### Package Entries
Each package is a `[[package]]` section with metadata.

### Registry Operations

#### Loading Registry
```bash
# Update cache
ggen marketplace cache update

# Clear cache
ggen marketplace cache clear

# View cache location
ggen marketplace cache path
```

#### Registry Validation
```bash
# Generate validation report
ggen marketplace report

# Generate with custom output
ggen marketplace report --output health.json

# Emit validation receipts
ggen marketplace emit-receipts --report

# Generate artifacts (JSON + Markdown)
ggen marketplace generate-artifacts
```

---

## CLI Command Reference

### Summary of 19 Commands

| Command | Purpose | Key Options |
|---------|---------|------------|
| **search** | Find packages | --query, --limit, --category |
| **install** | Install package | --package, --target, --force, --dry-run |
| **list** | Show installed | --detailed, --json, --min-maturity |
| **publish** | Publish package | --path, --tag, --dry-run, --force |
| **validate** | Quality check | --package, --update, --require-level |
| **maturity** | 6D assessment | --detailed, --verify |
| **dashboard** | Maturity overview | --format, --output, --min-maturity |
| **maturity-batch** | Batch assessment | --output |
| **recommend** | Get suggestions | --use-case, --priority, --min-score |
| **compare** | Compare packages | --package-a, --package-b, --detailed |
| **search-maturity** | Filter by quality | --min-level, --min-* dimensions |
| **export** | Export data | --format (csv/json/html), --output |
| **list-bundles** | Show bundles | |
| **bundle-info** | Bundle details | --bundle-id |
| **install-bundle** | Install bundle | --bundle-id, --dry-run |
| **emit-receipts** | Validation receipts | --report |
| **report** | Health report | --output |
| **generate-artifacts** | JSON + Markdown | --json-output, --md-output |
| **improve** | Improvement plan | --package |

### Detailed Command Docs

#### Search
```bash
ggen marketplace search --query "rust web"
ggen marketplace search --query "api" --category templates --limit 10
```

#### Install
```bash
ggen marketplace install --package "advanced-rust-api-8020"
ggen marketplace install --package "rest-api@1.0.0" --target ./my-api --dry-run
ggen marketplace install --package "pkg" --no-dependencies --force
```

#### List
```bash
ggen marketplace list
ggen marketplace list --detailed
ggen marketplace list --min-maturity production --json
```

#### Publish
```bash
ggen marketplace publish --path ./my-package --dry-run
ggen marketplace publish --path ./my-package --tag v1.0.0 --force
```

#### Validate & Maturity
```bash
# Single package validation
ggen marketplace validate --package "io.ggen.rust.microservice"
ggen marketplace validate --package "pkg" --require-level production --improvement-plan

# All packages
ggen marketplace validate --update

# Maturity assessment
ggen marketplace maturity "io.ggen.rust.microservice"
ggen marketplace maturity "pkg" --detailed --verify production

# Dashboard
ggen marketplace dashboard
ggen marketplace dashboard --min-maturity production --output report.json

# Search by maturity
ggen marketplace search-maturity --min-level production --min-security 15
```

#### Recommendations & Comparison
```bash
# Get recommendations
ggen marketplace recommend --use-case production
ggen marketplace recommend --use-case enterprise --priority security

# Compare packages
ggen marketplace compare --package-a "io.ggen.a" --package-b "io.ggen.b"
ggen marketplace compare --package-a "a" --package-b "b" --detailed --output comp.json
```

#### Export & Reporting
```bash
# Export assessments
ggen marketplace export --format csv --output packages.csv
ggen marketplace export --format json --detailed --output data.json

# Generate report
ggen marketplace report
ggen marketplace report --output health.json

# Emit receipts
ggen marketplace emit-receipts --report

# Generate artifacts
ggen marketplace generate-artifacts
ggen marketplace generate-artifacts --json-output reg.json --md-output PACKAGES.md
```

#### Bundle Operations
```bash
# List bundles
ggen marketplace list-bundles

# Info about bundle
ggen marketplace bundle-info --bundle-id "observability-stack"

# Install bundle
ggen marketplace install-bundle --bundle-id "microservices-bundle" --dry-run
ggen marketplace install-bundle --bundle-id "observability-stack"
```

---

## Authentication & Security

### Package Verification

#### Checksum Verification
- Format: SHA256 (64 hex characters)
- Stored in registry per package version
- Automatically verified on installation

#### Ed25519 Signature Support
```rust
// Future: Package signatures via Ed25519
pub struct Signature {
    pub algorithm: String,  // "ed25519"
    pub value: String,      // Base64-encoded signature
}
```

#### Installation Security
```bash
# Review package before install
ggen marketplace search "package-name"

# Dry-run to preview
ggen marketplace install "package" --dry-run

# Check dependencies
ggen marketplace search-maturity --min-level production
```

### Package Validation

#### Validation Criteria

**Required Checks (60% weight)**:
- ✅ `package.toml` - Complete metadata
- ✅ `README.md` - Documentation (100+ chars)
- ✅ Source code - At least one of: src/main.rs, src/lib.rs, or templates/
- ✅ License file - LICENSE, LICENSE-MIT, or LICENSE-APACHE

**Quality Checks (40% weight)**:
- ✅ RDF ontology - `rdf/ontology.ttl` (200+ lines)
- ✅ SPARQL queries - `sparql/*.rq` files
- ✅ Examples - Runnable in `examples/`
- ✅ Tests - In `tests/`
- ✅ Documentation - Additional docs in `docs/`

#### Scoring
- **95%+**: ✅ Production ready
- **80-94%**: ⚠️ Needs improvement
- **<80%**: ❌ Not ready

#### Validation Commands
```bash
# Validate single package
ggen marketplace validate --package "my-package"

# Validate all
ggen marketplace validate

# Update production flags
ggen marketplace validate --update

# Generate report
ggen marketplace report
```

### Authentication & Permissions

**Current State**: No authentication required for:
- Searching packages
- Installing packages
- Viewing documentation

**Future**: GitHub-based authentication for publishing via fork + PR workflow.

---

## Available Templates (Examples)

### Core Power Packages (Most Popular)

#### 1. REST API Template
```
Name: rest-api-template
Version: 1.0.0
Languages: Rust (Axum), TypeScript (Express), Python (FastAPI)
Features:
  - CRUD operations
  - Authentication (JWT, OAuth2, API-key)
  - Rate limiting (token-bucket, sliding-window)
  - Validation (request/response schemas)
  - Middleware (CORS, compression, logging)
  - OpenAPI/Swagger documentation
  - 80% test coverage
  - Database support (PostgreSQL, MySQL, SQLite, MongoDB)
```

#### 2. Advanced Rust API 8020
```
Name: advanced-rust-api-8020
Version: 0.1.0
Framework: Axum
Features:
  - Complete lifecycle management (make.toml)
  - AI-powered generation from SPARQL
  - Production readiness tracking
  - JWT + bcrypt authentication
  - Error handling (no .expect() in production)
  - Structured logging with tracing
  - Health check endpoints
  - OpenAPI ready
  - Docker containerization
  - 80%+ test coverage
  - Database migrations (SQLx)
Dependencies: None
Tags: rust, api, rest, production, 8020, axum, jwt
```

#### 3. GraphQL API Template
```
Name: graphql-api-rust
Version: 0.1.0
Framework: async-graphql + Axum
Features:
  - GraphQL schema definition
  - Real-time subscriptions
  - JWT authentication
  - Database integration
Tags: rust, graphql, async, subscriptions
```

#### 4. Microservices Architecture
```
Name: microservices-architecture
Version: 0.1.0
Features:
  - Service mesh setup
  - API gateway pattern
  - Inter-service communication
  - Distributed tracing
  - Health checks
```

### By Category

#### Templates (Code Generation)
- advanced-rust-api-8020
- rest-api-template
- graphql-api-rust
- rust-cli-template
- microservices-architecture
- comprehensive-rust-showcase

#### Academic Packages
- arxiv-paper-template
- neurips-paper-template
- ieee-paper-template
- phd-thesis-template
- academic-peer-review-workflow

#### Domain-Specific
- **Healthcare**: ehr-integration, healthcare-analytics, medical-billing
- **Finance**: banking-core, cryptocurrency-exchange, iso-20022-payments
- **Enterprise**: enterprise-erp-core, multi-tenant-saas
- **AI/ML**: ai-code-generation, agent-memory-forge, product-recommendations

#### Infrastructure
- api-gateway-service-mesh
- search-indexing-platform
- data-warehouse-etl
- database-schema-generator
- microservices-architecture

### Statistics
- **Total Packages**: 48+
- **Categories**: 10+
- **Languages Supported**: Rust, TypeScript, Python
- **Production-Ready**: 30+
- **Average Maturity Score**: 75+

---

## Key Features

### For Users
- **Fast Search**: Full-text search with filtering by category, tags, keywords
- **Safe Installation**: Dry-run and verification options
- **Smart Dependency Management**: Automatic recursive dependency resolution
- **Version Control**: Install specific versions or latest
- **Maturity Filtering**: Choose by production-readiness

### For Publishers
- **Simple Publishing**: CLI-based submission
- **Automatic Deployment**: GitHub Pages via CI/CD
- **Semantic Versioning**: Built-in version management
- **Quality Assurance**: Validation and maturity assessment
- **Discovery**: Full-text search and recommendations

### Advanced Capabilities
- **Maturity Assessment**: 6-dimension evaluation (Documentation, Testing, Security, Performance, Adoption, Maintenance)
- **Bundle Installation**: Install sector-specific package bundles
- **Recommendations**: Smart suggestions based on use case (production, research, enterprise, startup)
- **Comparative Analysis**: Side-by-side package comparison
- **Export**: CSV, JSON, HTML report generation
- **Analytics**: Package analytics and health metrics

---

## Registry URLs

### Production Registry
```
Base: https://seanchatmangpt.github.io/ggen/marketplace/

Endpoints:
  GET /registry/packages.toml          - TOML registry
  GET /registry/index.json             - JSON index
  GET /packages/{name}/                - Package directory
  GET /packages/{name}/README.md       - Package README
  GET /packages/{name}/package.toml    - Package metadata

Raw Content:
  Base: https://raw.githubusercontent.com/seanchatmangpt/ggen/master/
  GET /marketplace/packages/{name}/{file}
```

### Local Registry
```
Path: ./marketplace/registry/packages.toml
Config: ~/.ggen/config.toml (marketplace.registry_url)
```

---

## Implementation Details

### Search Scoring Algorithm
```
score = 0

# Name matching (highest priority)
if name == query:
    score += 100
else if name.contains(query):
    score += 50
else if fuzzy_similarity(name, query) >= 0.7:
    score += 30 * similarity

# Description matching
if description.contains(query):
    score += 20

# Tag/keyword matching
for tag in tags:
    if tag == query or fuzzy_match(tag, query):
        score += 10

# Popularity boost (normalized 0-10)
downloads_boost = min(10, downloads / 1000)
stars_boost = min(5, stars / 10)
score += downloads_boost + stars_boost

return score
```

### Dependency Resolution Algorithm
```
function resolve_dependencies(package):
    installed = []
    queue = [package]
    visited = set()
    
    while queue not empty:
        current = queue.pop()
        if current in visited:
            continue
        visited.add(current)
        
        for dep in current.dependencies:
            if dep not installed:
                queue.push(dep)
        
        install(current)
        installed.push(current)
    
    return installed
```

### Cache Management
```
LRU Cache:
- Capacity: 1000 entries
- Eviction: Least Recently Used when full
- Hit/Miss tracking: Debug logging
- Clear operation: Removes all entries
```

---

## Troubleshooting

### Package Not Found
```bash
# Update cache
ggen marketplace cache update

# Search again
ggen marketplace search "package-name"

# Check registry URL
ggen config get marketplace.registry_url
```

### Installation Fails
```bash
# Check disk space
df -h

# Verify permissions
ls -la ~/.ggen/packages/

# Dry-run first
ggen marketplace install "package" --dry-run

# Verbose output
ggen marketplace install "package" --verbose
```

### Template Generation Issues
```bash
# Validate template syntax
ggen template validate template.hbs

# List template variables
ggen template list-vars template.hbs

# Generate with verbose
ggen template generate template.hbs --verbose
```

### Lifecycle Errors
```bash
# Check make.toml
cat make.toml

# Run specific phase
ggen lifecycle run --phases init

# Verbose output
ggen lifecycle run --verbose
```

---

## Configuration

### Global Settings
```bash
# Set default target directory
ggen config set marketplace.default_target ./packages

# Set registry URL
ggen config set marketplace.registry_url \
  "https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml"

# View config
ggen config get marketplace.registry_url
```

### Cache Configuration
```toml
# ~/.ggen/config.toml
[marketplace]
registry_url = "https://..."
default_target = "./packages"
cache_dir = "~/.ggen/packages"
cache_capacity = 1000
auto_update_cache = true
```

---

## Programmatic API (Rust)

```rust
use ggen_domain::marketplace::{
    execute_search, execute_install, execute_list, execute_publish,
    SearchInput, InstallInput, ListInput, PublishInput
};

// Search
let results = execute_search(SearchInput {
    query: "rust api".to_string(),
    limit: 10,
    category: Some("templates".to_string()),
    ..Default::default()
}).await?;

// Install
let result = execute_install(InstallInput {
    package: "advanced-rust-api-8020".to_string(),
    target: Some("./my-project".to_string()),
    force: false,
    no_dependencies: false,
    dry_run: false,
}).await?;

// List
let result = execute_list(ListInput {
    detailed: true,
    json: false,
}).await?;

// Publish
let result = execute_publish(PublishInput {
    path: PathBuf::from("./my-package"),
    tag: Some("v1.0.0".to_string()),
    dry_run: false,
    force: false,
}).await?;
```

---

## Standards & Best Practices

### Naming Conventions
**Good**:
- rust-api-template
- ai-code-generator
- microservices-framework

**Avoid**:
- my-package (too generic)
- test123 (not descriptive)
- pkg (unclear)

### Description Writing
**Good**:
```
Production-ready REST API with authentication, 
database integration, and comprehensive testing
```

**Avoid**:
```
A package that does stuff
```

### Version Management
```
1.0.0          Initial stable release
1.1.0          New features, backward compatible
1.1.1          Bug fixes
2.0.0          Breaking changes

Pre-release versions:
0.1.0-alpha    Early development
0.1.0-beta     Feature complete
0.1.0-rc.1     Release candidate
```

---

## Future Enhancements

Based on codebase analysis:

1. **Statistics API**: Download counts, GitHub stars tracking
2. **Package Signing**: Full GPG/Ed25519 signature support
3. **Advanced Caching**: Multi-level cache (local, CDN, origin)
4. **CLI Auto-completion**: Shell completion generation
5. **Package Linking**: Local development linking support
6. **Update Checking**: Automatic version update notifications
7. **Platform-Specific Packages**: OS-specific binary distributions
8. **Batch Operations**: Install multiple packages in one command
9. **Plugin System**: Custom registry backends
10. **Telemetry (Opt-in)**: Anonymous usage analytics

---

## References

### Documentation Files
- `/marketplace/README.md` - Overview
- `/marketplace/USER_GUIDE.md` - User guide
- `/marketplace/PUBLISHING_GUIDE.md` - Publishing guide
- `/marketplace/API.md` - Complete API reference
- `/marketplace/PACKAGES.md` - Package directory

### Source Code
- `/crates/ggen-marketplace/src/` - Marketplace library
- `/crates/ggen-domain/src/marketplace/` - Domain logic
- `/crates/ggen-cli/src/cmds/marketplace.rs` - CLI commands
- `/marketplace/registry/packages.toml` - Registry

### Example Packages
- `/marketplace/packages/advanced-rust-api-8020/`
- `/marketplace/packages/rest-api-template/`
- `/marketplace/packages/comprehensive-rust-showcase/`
- `/examples/` - Project examples

---

**Last Updated**: 2025-11-17
**Marketplace Version**: 1.0.0
**Total Commands**: 19 verbs
**Total Packages**: 48+
**Production Ready**: Yes
