<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Software Bill of Materials & Supply Chain Provenance](#software-bill-of-materials--supply-chain-provenance)
  - [Table of Contents](#table-of-contents)
  - [Executive Summary](#executive-summary)
  - [SBOM Definition & Purpose](#sbom-definition--purpose)
    - [What is SBOM?](#what-is-sbom)
    - [Why SBOM?](#why-sbom)
    - [SBOM Benefits](#sbom-benefits)
  - [SBOM Format & Standard](#sbom-format--standard)
    - [CycloneDX Standard](#cyclonedx-standard)
    - [File Format](#file-format)
    - [Example SBOM Structure (JSON)](#example-sbom-structure-json)
  - [SBOM Components](#sbom-components)
    - [Rust Components (Cargo Workspace)](#rust-components-cargo-workspace)
    - [JavaScript Components (npm Workspace)](#javascript-components-npm-workspace)
    - [Container Images (Docker)](#container-images-docker)
    - [Terraform & Infrastructure](#terraform--infrastructure)
  - [SBOM Generation](#sbom-generation)
    - [Automated SBOM Generation](#automated-sbom-generation)
    - [Manual SBOM Generation (for releases)](#manual-sbom-generation-for-releases)
  - [Provenance Tracking](#provenance-tracking)
    - [What is Provenance?](#what-is-provenance)
    - [Provenance for Rust Components](#provenance-for-rust-components)
    - [Provenance for npm Components](#provenance-for-npm-components)
    - [Provenance for Docker Images](#provenance-for-docker-images)
  - [Build Attestation](#build-attestation)
    - [in-toto Framework](#in-toto-framework)
    - [in-toto Metadata Example](#in-toto-metadata-example)
  - [Dependency Scanning](#dependency-scanning)
    - [CVE Detection (Automated)](#cve-detection-automated)
    - [Vulnerability Reporting](#vulnerability-reporting)
  - [License Compliance](#license-compliance)
    - [Permissive Licenses Only](#permissive-licenses-only)
    - [License Compliance Check](#license-compliance-check)
  - [Supply Chain Verification](#supply-chain-verification)
    - [How Customers Verify Release Integrity](#how-customers-verify-release-integrity)
    - [Reproducible Build Verification](#reproducible-build-verification)
  - [SBOM Template](#sbom-template)
    - [Standard SBOM Structure (JSON)](#standard-sbom-structure-json)
  - [Provenance Verification Checklist](#provenance-verification-checklist)
  - [Definition of Done](#definition-of-done)
  - [Receipt Contract](#receipt-contract)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Software Bill of Materials & Supply Chain Provenance

**Classification**: Internal | **Version**: 1.0.0 | **For**: ggen v6.0.0 Government Compliance

**Authority**: Chief Security Officer | **Last Updated**: January 2026 | **Next Review**: April 2026

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [SBOM Definition & Purpose](#sbom-definition--purpose)
3. [SBOM Format & Standard](#sbom-format--standard)
4. [SBOM Components](#sbom-components)
5. [SBOM Generation](#sbom-generation)
6. [Provenance Tracking](#provenance-tracking)
7. [Build Attestation](#build-attestation)
8. [Dependency Scanning](#dependency-scanning)
9. [License Compliance](#license-compliance)
10. [Supply Chain Verification](#supply-chain-verification)
11. [SBOM Template](#sbom-template)
12. [Provenance Verification Checklist](#provenance-verification-checklist)
13. [Definition of Done](#definition-of-done)

---

## Executive Summary

**Software Bill of Materials (SBOM)** is a structured inventory of all software components, dependencies, licenses, and security attributes that make up ggen. This policy ensures:

- **Transparency**: Complete visibility into all components (Rust crates, npm packages, base images)
- **Security**: Early detection of CVEs and vulnerable dependencies
- **Compliance**: Government contract requirements (FedRAMP, TAI 2030, EO 14028)
- **Provenance**: Cryptographic proof of component origin and build integrity
- **Reproducibility**: Users can verify that releases match SBOMs (no tampering)

**Core Principles**:
- SBOM generated for every release (in CycloneDX JSON/XML format)
- SBOM includes all dependencies (transitive + direct)
- SBOM includes license information (permissive licenses only)
- SBOM includes security information (known CVEs, vulnerability status)
- SBOM signed with in-toto attestation (proves build was authorized)
- SBOM published with release (customers can verify)
- Dependency scanning performed automatically (detect CVEs early)

**Current Version**: v6.0.0 SBOM (Generated: January 18, 2026)

---

## SBOM Definition & Purpose

### What is SBOM?

**SBOM** is a formal inventory listing all software components that comprise ggen:

```
ggen v6.0.0 SBOM
├─ Rust Components
│   ├─ ggen-core crate (v6.0.0, from source)
│   ├─ ggen-cli crate (v6.0.0, from source)
│   ├─ oxigraph crate (v0.5.1, from crates.io)
│   └─ [24 other transitive deps]
├─ JavaScript Components
│   ├─ @ggen/sdk npm package (v6.0.0, from npm registry)
│   ├─ typescript (v5.0.2, from npm registry)
│   └─ [15 other transitive deps]
├─ Base Images
│   ├─ alpine:3.18.4 (from Docker Hub)
│   └─ node:18-alpine (from Docker Hub)
├─ System Libraries
│   ├─ OpenSSL 3.0.0 (from Alpine)
│   ├─ libssl3 (from Alpine)
│   └─ [OS-provided libs]
└─ Configuration
    ├─ Terraform v1.5.0 (from registry)
    ├─ Google Cloud provider v5.0.0 (from registry)
    └─ [Other IaC dependencies]
```

### Why SBOM?

**Government Compliance**:
- FedRAMP requires SBOM for federal systems
- Executive Order 14028 mandates SBOM for federal procurement
- TAI 2030 contract requires supply chain transparency

**Security**:
- CVE detection: Identify vulnerable components before deployment
- Vulnerability tracking: Monitor components for newly discovered CVEs
- Component retirement: Remove unsupported/unmaintained components

**Customer Trust**:
- Transparency: Customers see exactly what's in the product
- Reproducibility: Customers can verify release matches SBOM
- Integrity: Customers detect tampering or unauthorized changes

### SBOM Benefits

| Benefit | Description | Example |
|---------|---|---|
| **CVE Detection** | Identify known vulnerabilities in components | CVE-2025-1234 in oxigraph 0.4.0 detected |
| **Compliance** | Meet government contract requirements | FedRAMP requires SBOM for ATO |
| **Transparency** | Show customers what's included | "ggen uses OpenSSL 3.0.0 for TLS" |
| **Reproducibility** | Verify release integrity | User can rebuild from SBOM, verify hash |
| **Supply Chain** | Prove components from trusted sources | All crates from crates.io, all images from Docker Hub |

---

## SBOM Format & Standard

### CycloneDX Standard

ggen uses **CycloneDX** format (industry standard, NIST-recommended):

**CycloneDX Advantages**:
- ✅ Industry standard (OWASP)
- ✅ Government accepted (FedRAMP, NIST)
- ✅ Rich metadata (licenses, vulnerabilities, usage)
- ✅ Machine-readable (JSON/XML)
- ✅ Signature support (in-toto attestation)

### File Format

**JSON Format** (primary):
```
/ggen/receipts/sbom/v6.0.0-sbom.json
```

**XML Format** (secondary, for legacy systems):
```
/ggen/receipts/sbom/v6.0.0-sbom.xml
```

### Example SBOM Structure (JSON)

```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.4",
  "version": 1,
  "metadata": {
    "timestamp": "2026-01-18T10:15:00Z",
    "tools": [
      {
        "vendor": "ggen",
        "name": "cyclonedx-rs",
        "version": "0.4.0"
      }
    ],
    "component": {
      "bom-ref": "pkg:cargo/ggen@6.0.0",
      "type": "application",
      "name": "ggen",
      "version": "6.0.0",
      "description": "Specification-Driven Code Generation",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          }
        }
      ]
    }
  },
  "components": [
    {
      "bom-ref": "pkg:cargo/oxigraph@0.5.1",
      "type": "library",
      "name": "oxigraph",
      "version": "0.5.1",
      "purl": "pkg:cargo/oxigraph@0.5.1",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          }
        }
      ],
      "externalReferences": [
        {
          "type": "security-advisory",
          "url": "https://nvd.nist.gov/vuln/detail/CVE-..."
        }
      ]
    },
    {
      "bom-ref": "pkg:npm/typescript@5.0.2",
      "type": "library",
      "name": "typescript",
      "version": "5.0.2",
      "purl": "pkg:npm/typescript@5.0.2",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          }
        }
      ]
    },
    {
      "bom-ref": "pkg:docker/alpine@3.18.4",
      "type": "container",
      "name": "alpine",
      "version": "3.18.4",
      "purl": "pkg:docker/alpine@3.18.4"
    }
  ]
}
```

---

## SBOM Components

### Rust Components (Cargo Workspace)

**Direct Dependencies** (ggen's direct dependencies):

```toml
[dependencies]
oxigraph = "0.5.1"          # RDF processing
tokio = { version = "1.47", features = ["full"] }
serde = "1.0"               # Serialization
clap = { version = "4.5", features = ["derive"] }
genai = "0.4"               # LLM integration
thiserror = "1.0"           # Error handling
```

**Transitive Dependencies** (dependencies' dependencies):

```
cargo tree output:
ggen v6.0.0
├─ oxigraph v0.5.1
│  ├─ spargebra v0.2.0
│  ├─ oxrdf v0.1.5
│  └─ rio_turtle v0.8.0
├─ tokio v1.47.0
│  ├─ bytes v1.5.0
│  ├─ mio v0.8.9
│  └─ num_cpus v1.16.0
└─ [27 total crates, ~150 transitive]
```

**SBOM Entry Example**:

```json
{
  "bom-ref": "pkg:cargo/oxigraph@0.5.1",
  "type": "library",
  "name": "oxigraph",
  "version": "0.5.1",
  "purl": "pkg:cargo/oxigraph@0.5.1",
  "description": "SPARQL engine and RDF store for Rust",
  "homepage": "https://github.com/oxigraph/oxigraph",
  "licenses": [
    {
      "license": {
        "name": "Apache-2.0"
      }
    }
  ],
  "externalReferences": [
    {
      "type": "source-repository",
      "url": "https://github.com/oxigraph/oxigraph"
    },
    {
      "type": "security-advisory",
      "url": "https://nvd.nist.gov/vuln/detail/CVE-2025-0001"
    }
  ],
  "evidence": {
    "identity": {
      "field": "purl",
      "confidence": 100,
      "methods": [
        {
          "technique": "manifest-analysis",
          "confidence": 100,
          "value": "Cargo.lock v3"
        }
      ]
    }
  }
}
```

### JavaScript Components (npm Workspace)

**Node.js SDK** (@ggen/sdk):

```json
{
  "package.json": {
    "name": "@ggen/sdk",
    "version": "6.0.0",
    "dependencies": {
      "typescript": "^5.0.2",
      "axios": "^1.6.0",
      "zod": "^3.22.0"
    }
  }
}
```

**SBOM Entries**:

```json
[
  {
    "bom-ref": "pkg:npm/@ggen/sdk@6.0.0",
    "type": "library",
    "name": "@ggen/sdk",
    "version": "6.0.0",
    "purl": "pkg:npm/@ggen/sdk@6.0.0",
    "licenses": [
      {"license": {"name": "Apache-2.0"}}
    ]
  },
  {
    "bom-ref": "pkg:npm/typescript@5.0.2",
    "type": "library",
    "name": "typescript",
    "version": "5.0.2",
    "purl": "pkg:npm/typescript@5.0.2",
    "licenses": [
      {"license": {"name": "Apache-2.0"}}
    ]
  }
]
```

### Container Images (Docker)

**Base Images**:

```dockerfile
# Dockerfile (production)
FROM alpine:3.18.4 AS base
RUN apk add --no-cache \
  openssl=3.0.10-r0 \
  ca-certificates

FROM rust:1.91.1 AS builder
WORKDIR /build
COPY . .
RUN cargo build --release

FROM alpine:3.18.4 AS runtime
COPY --from=builder /build/target/release/ggen /usr/local/bin/
ENTRYPOINT ["ggen"]
```

**SBOM Entries** (base images + installed packages):

```json
{
  "bom-ref": "pkg:docker/alpine@3.18.4",
  "type": "container",
  "name": "alpine",
  "version": "3.18.4",
  "purl": "pkg:docker/alpine@3.18.4",
  "components": [
    {
      "bom-ref": "pkg:apk/openssl@3.0.10-r0",
      "type": "library",
      "name": "openssl",
      "version": "3.0.10-r0",
      "purl": "pkg:apk/openssl@3.0.10-r0"
    },
    {
      "bom-ref": "pkg:apk/ca-certificates@20230506-r0",
      "type": "library",
      "name": "ca-certificates",
      "version": "20230506-r0",
      "purl": "pkg:apk/ca-certificates@20230506-r0"
    }
  ]
}
```

### Terraform & Infrastructure

**Terraform Providers**:

```hcl
# terraform/main.tf
terraform {
  required_version = ">= 1.5.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}
```

**SBOM Entries**:

```json
{
  "bom-ref": "pkg:terraform/google@5.0.0",
  "type": "library",
  "name": "google",
  "version": "5.0.0",
  "purl": "pkg:terraform/google@5.0.0",
  "description": "Terraform Google Cloud Platform Provider",
  "licenses": [
    {"license": {"name": "MPL-2.0"}}
  ]
}
```

---

## SBOM Generation

### Automated SBOM Generation

**CI/CD Pipeline** (GitHub Actions):

```yaml
# .github/workflows/sbom-generate.yml
name: Generate SBOM

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:

jobs:
  sbom:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Generate Cargo SBOM
        run: |
          cargo install cargo-sbom
          cargo sbom --output json > sbom.json

      - name: Generate npm SBOM
        run: |
          npm install -g sbom
          sbom package-lock.json --output json >> sbom.json

      - name: Generate Docker SBOM
        run: |
          syft docker.io/ggen:${{ github.ref_name }} \
            --output json >> sbom.json

      - name: Merge SBOMs
        run: |
          python3 scripts/merge-sboms.py sbom.json > v6.0.0-sbom.json

      - name: Sign SBOM (in-toto)
        run: |
          python3 -m in_toto.runlib.in_toto_run \
            --step-name sbom-generation \
            --products v6.0.0-sbom.json \
            -- cat v6.0.0-sbom.json

      - name: Upload to Release
        run: |
          gh release upload ${{ github.ref_name }} \
            v6.0.0-sbom.json \
            v6.0.0-sbom.link.json
```

### Manual SBOM Generation (for releases)

```bash
# Step 1: Generate Rust SBOM
cargo install cargo-sbom
cargo sbom --output json > rust-sbom.json

# Step 2: Generate npm SBOM
npm install -g sbom
sbom package-lock.json --output json > npm-sbom.json

# Step 3: Generate Docker SBOM
syft docker.io/ggen:v6.0.0 --output json > docker-sbom.json

# Step 4: Generate Terraform SBOM
# (manual: extract from terraform.lock.hcl)
python3 scripts/terraform-sbom.py > terraform-sbom.json

# Step 5: Merge all SBOMs
python3 scripts/merge-sboms.py \
  rust-sbom.json npm-sbom.json docker-sbom.json terraform-sbom.json \
  --output v6.0.0-sbom.json

# Step 6: Validate SBOM against schema
cyclonedx validate --file v6.0.0-sbom.json

# Step 7: Sign SBOM with in-toto
in-toto-run \
  --step-name sbom-generation \
  --products v6.0.0-sbom.json \
  -- cat v6.0.0-sbom.json

# Output: v6.0.0-sbom.link.json (in-toto metadata)

# Step 8: Archive
mkdir -p /ggen/receipts/sbom/v6.0.0/
cp v6.0.0-sbom.json /ggen/receipts/sbom/v6.0.0/
cp v6.0.0-sbom.link.json /ggen/receipts/sbom/v6.0.0/
```

---

## Provenance Tracking

### What is Provenance?

**Provenance** is the documented origin and build history of each component:

```
provenance = {
  source: "Where did this come from?" (GitHub, crates.io, npm registry)
  builder: "Who built it?" (GitHub Actions, build machine)
  timestamp: "When was it built?" (ISO 8601 timestamp)
  signature: "Is it authentic?" (GPG/in-toto signature)
  reproducibility: "Can it be rebuilt identically?" (deterministic build)
}
```

### Provenance for Rust Components

**Direct ggen Components** (built by company):

```json
{
  "component": "ggen-core",
  "version": "6.0.0",
  "source": {
    "type": "github",
    "repository": "https://github.com/seanchatmangpt/ggen",
    "commit": "a1b2c3d4e5f6g7h8i9j0",
    "branch": "main",
    "tag": "v6.0.0"
  },
  "builder": {
    "type": "github-actions",
    "workflow": "release.yml",
    "run_id": "123456789",
    "run_number": 42,
    "timestamp": "2026-01-18T10:15:00Z"
  },
  "build": {
    "rust_version": "1.91.1",
    "cargo_version": "1.76.0",
    "build_command": "cargo build --release",
    "build_time_seconds": 120
  },
  "signature": {
    "algorithm": "pgp",
    "key_id": "ABCD1234",
    "signature": "-----BEGIN PGP SIGNATURE-----...",
    "verified": true
  },
  "reproducibility": {
    "deterministic": true,
    "hash": "sha256:abc123...",
    "rebuild_hash": "sha256:abc123...",
    "hash_match": true
  }
}
```

**External Crate Components** (from crates.io):

```json
{
  "component": "oxigraph",
  "version": "0.5.1",
  "source": {
    "type": "crates.io",
    "registry_url": "https://github.com/rust-lang/crates.io",
    "crate_id": "oxigraph",
    "source_url": "https://github.com/oxigraph/oxigraph"
  },
  "publisher": {
    "name": "Oxigraph Project",
    "verified": true,
    "registry_verification": true
  },
  "signature": {
    "algorithm": "sha256",
    "hash": "sha256:def456...",
    "checksum_verified": true
  }
}
```

### Provenance for npm Components

**Direct @ggen/sdk** (built by company):

```json
{
  "package": "@ggen/sdk",
  "version": "6.0.0",
  "source": {
    "type": "github",
    "repository": "https://github.com/seanchatmangpt/ggen",
    "commit": "a1b2c3d4e5f6g7h8i9j0",
    "tag": "v6.0.0"
  },
  "published_to_npm": {
    "registry": "https://registry.npmjs.org",
    "published_at": "2026-01-18T10:30:00Z",
    "publisher_account": "ggen-publisher",
    "verified": true
  },
  "signature": {
    "algorithm": "npm-signature",
    "signature": "signature-value",
    "verified": true
  }
}
```

**External npm Packages** (from npm registry):

```json
{
  "package": "typescript",
  "version": "5.0.2",
  "source": {
    "type": "npm",
    "registry_url": "https://registry.npmjs.org",
    "publisher": "typescript-team"
  },
  "published_to_npm": {
    "published_at": "2023-11-16T18:45:00Z",
    "tarball_url": "https://registry.npmjs.org/typescript/-/typescript-5.0.2.tgz"
  },
  "signature": {
    "algorithm": "sha512",
    "hash": "sha512:ghi789...",
    "integrity": "verified"
  }
}
```

### Provenance for Docker Images

**ggen Container Image**:

```json
{
  "image": "ggen",
  "version": "6.0.0",
  "registry": "ghcr.io",
  "digest": "sha256:xyz789abc123...",
  "built_by": {
    "workflow": "docker-build.yml",
    "run_id": "987654321",
    "timestamp": "2026-01-18T10:20:00Z"
  },
  "base_images": [
    {
      "name": "alpine",
      "tag": "3.18.4",
      "digest": "sha256:alpine-digest-...",
      "registry": "docker.io",
      "verified": true
    },
    {
      "name": "rust",
      "tag": "1.91.1",
      "digest": "sha256:rust-digest-...",
      "registry": "docker.io",
      "verified": true
    }
  ],
  "signature": {
    "algorithm": "cosign",
    "public_key": "-----BEGIN PUBLIC KEY-----...",
    "signature": "cosign-signature-...",
    "verified": true
  },
  "sbom_attestation": {
    "type": "in-toto",
    "file": "ggen-6.0.0-sbom.link.json",
    "verified": true
  }
}
```

---

## Build Attestation

### in-toto Framework

ggen uses **in-toto** for cryptographic proof of build integrity:

```
Build Attestation Chain:

1. Specification (input)
   └─ RDF ontology (source of truth)

2. Implementation (controlled)
   ├─ Code commit (signed with GPG)
   ├─ Code review (approvals recorded)
   └─ Tests pass (all green)

3. Build (verified)
   ├─ Builder identity (GitHub Actions machine)
   ├─ Build command (exact command recorded)
   ├─ Build output (binary hash recorded)
   └─ Build signature (in-toto metadata)

4. Attestation (cryptographically signed)
   └─ JSON metadata file (.link.json) signed with build key

5. Verification (by customer)
   ├─ Customer has public key
   ├─ Customer verifies .link.json signature
   ├─ Customer verifies binary matches attestation
   └─ Customer has confidence build was not tampered
```

### in-toto Metadata Example

**Build Attestation** (`v6.0.0-sbom.link.json`):

```json
{
  "_type": "link",
  "name": "ggen-build",
  "materials": {
    "Cargo.lock": {
      "sha256": "abc123...hash of Cargo.lock file..."
    },
    "Cargo.toml": {
      "sha256": "def456...hash of Cargo.toml file..."
    },
    "src/**/*.rs": {
      "sha256": "ghi789...hash of all source files..."
    }
  },
  "byproducts": {
    "build-duration": "120",
    "rust-version": "1.91.1",
    "target-triple": "x86_64-unknown-linux-gnu"
  },
  "environment": {
    "builder": "GitHub Actions",
    "os": "ubuntu-latest",
    "workflow": ".github/workflows/release.yml"
  },
  "products": {
    "ggen-binary": {
      "sha256": "jkl012...hash of compiled binary..."
    },
    "sbom.json": {
      "sha256": "mno345...hash of SBOM file..."
    }
  },
  "byproducts": {
    "stdout": "Building release artifacts...",
    "stderr": ""
  },
  "return-value": 0,
  "command": ["cargo", "build", "--release"],
  "signed_by": {
    "keyid": "build-key-12345"
  }
}
```

**Verification** (customer side):

```bash
# Customer has public key from GitHub Actions
curl -s https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/ggen-6.0.0-sbom.link.json \
  -o sbom.link.json

# Verify signature on metadata
in-toto-verify \
  --layout-file ggen-layout.json \
  --link-dir ./sbom.link.json \
  --key-file build-pubkey.pem

# Output:
# ✓ Verify successful.
# ✓ Build was performed by authorized builder.
# ✓ SBOM matches binary (no tampering detected).
```

---

## Dependency Scanning

### CVE Detection (Automated)

**CI/CD Scanning** (on every build):

```yaml
# .github/workflows/security-scan.yml
name: Security Scan

on:
  push:
    branches:
      - main
      - develop
  pull_request:
  schedule:
    - cron: "0 2 * * *"  # Daily at 2am UTC

jobs:
  cargo-audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run cargo-audit
        run: |
          cargo install cargo-audit
          cargo audit --deny warnings

  npm-audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run npm audit
        run: |
          npm audit --audit-level=moderate

  grype-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Scan image with Grype
        run: |
          curl -sSfL https://raw.githubusercontent.com/anchore/grype/main/install.sh | sh -s -- -b /usr/local/bin
          grype ghcr.io/seanchatmangpt/ggen:latest \
            --fail-on critical
```

### Vulnerability Reporting

**Example CVE Detection**:

```
Cargo Audit Report (2026-01-15)
═════════════════════════════════

⚠️  FOUND 1 vulnerability in Cargo.lock

Crate:     openssl-sys
Version:   0.9.89
Vuln ID:   RUSTSEC-2025-0001
Title:     CVE-2025-0001: OpenSSL Security Fix
Severity:  HIGH
URL:       https://nvd.nist.gov/vuln/detail/CVE-2025-0001
Summary:   Buffer overflow in OpenSSL AES operations
Action:    Update openssl-sys from 0.9.89 to 0.9.91

Current Cargo.toml:
  openssl-sys = "0.9.89"

Fixed Cargo.toml:
  openssl-sys = "0.9.91"

Run: cargo update openssl-sys
```

**SBOM Update Process**:

1. Vulnerability detected in automated scan
2. Patch released for vulnerable dependency
3. Dependency updated in Cargo.toml / package.json
4. Tests re-run (verify no breakage)
5. New SBOM generated (reflects patched version)
6. Patch release issued (v6.0.2 with CVE fix)
7. Customer notified (security advisory sent)

---

## License Compliance

### Permissive Licenses Only

ggen **ONLY accepts** permissive open-source licenses:

| License | Type | Accepted? | Example |
|---------|------|-----------|---------|
| **Apache 2.0** | Permissive | ✅ YES | Rust, ggen core |
| **MIT** | Permissive | ✅ YES | JavaScript, many npm packages |
| **BSD-3-Clause** | Permissive | ✅ YES | Various libraries |
| **ISC** | Permissive | ✅ YES | npm packages |
| **GPL 2.0** | Copyleft | ❌ NO | Cannot use (incompatible) |
| **GPL 3.0** | Copyleft | ❌ NO | Cannot use (incompatible) |
| **AGPL** | Copyleft | ❌ NO | Cannot use (too restrictive) |
| **SSPL** | Proprietary | ❌ NO | Cannot use (proprietary) |

### License Compliance Check

**Automated** (CI/CD):

```bash
# Install license checker
cargo install cargo-license
npm install -g license-checker

# Check Rust licenses
cargo license --json | jq '.[] | select(.license != "Apache-2.0" and .license != "MIT")'

# Check npm licenses
license-checker --json | jq '.[] | select(.licenses != "Apache-2.0" and .licenses != "MIT")'

# Fail if non-compliant licenses found
if [ $? -eq 0 ]; then
  echo "ERROR: Non-compliant licenses detected!"
  exit 1
fi
```

**SBOM License Verification**:

```json
{
  "components": [
    {
      "name": "oxigraph",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          },
          "compliance": "APPROVED"
        }
      ]
    },
    {
      "name": "typescript",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          },
          "compliance": "APPROVED"
        }
      ]
    }
  ]
}
```

---

## Supply Chain Verification

### How Customers Verify Release Integrity

**Step 1: Download Release**
```bash
cd /tmp
wget https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/ggen-6.0.0-x86_64-unknown-linux-gnu.tar.gz
wget https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/v6.0.0-sbom.json
wget https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/v6.0.0-sbom.link.json
```

**Step 2: Verify Binary Checksum**
```bash
# Binary hash recorded in SBOM
SBOM_HASH=$(jq -r '.products[] | select(.name == "ggen-binary") | .sha256' v6.0.0-sbom.json)

# Calculate actual binary hash
ACTUAL_HASH=$(sha256sum ggen-6.0.0-x86_64-unknown-linux-gnu | cut -d' ' -f1)

# Verify match
if [ "$SBOM_HASH" = "$ACTUAL_HASH" ]; then
  echo "✓ Binary matches SBOM (no tampering)"
else
  echo "✗ BINARY MISMATCH! Do not use."
  exit 1
fi
```

**Step 3: Verify Build Attestation**
```bash
# Download public key from GitHub (immutable, signed)
curl -s https://github.com/seanchatmangpt/ggen.keys/blob/main/build-pubkey.pem \
  -o build-pubkey.pem

# Verify in-toto signature on SBOM
in-toto-verify \
  --link-dir v6.0.0-sbom.link.json \
  --key-file build-pubkey.pem

# If signature valid:
echo "✓ SBOM signed by authorized builder (verified)"
```

**Step 4: Verify Dependency Integrity**
```bash
# Check all dependencies in SBOM are from trusted sources
jq '.components[] | {name, purl, source}' v6.0.0-sbom.json

# Verify no unexpected dependencies (detect supply chain attack)
# Example: Check no new crates from unknown sources
```

**Step 5: Scan for Known CVEs**
```bash
# Use Grype or OWASP Dependency-Check
grype ggen-6.0.0-x86_64-unknown-linux-gnu --db-update

# Or validate against SBOM
cyclonedx validate --file v6.0.0-sbom.json
```

### Reproducible Build Verification

**Advanced Users** can verify reproducibility:

```bash
# Step 1: Check out source at release tag
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen
git checkout v6.0.0

# Step 2: Rebuild from source
cargo build --release

# Step 3: Compare binary hash
cargo-sbom --output json > rebuilt-sbom.json
REBUILT_HASH=$(jq -r '.products[] | select(.name == "ggen-binary") | .sha256' rebuilt-sbom.json)

# Step 4: Download official binary SBOM
curl -s https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/v6.0.0-sbom.json \
  -o official-sbom.json
OFFICIAL_HASH=$(jq -r '.products[] | select(.name == "ggen-binary") | .sha256' official-sbom.json)

# Step 5: Verify hashes match
if [ "$REBUILT_HASH" = "$OFFICIAL_HASH" ]; then
  echo "✓ Reproducible build verified (no modifications)"
else
  echo "⚠ Hashes differ (expected - build environment differences)"
fi
```

---

## SBOM Template

### Standard SBOM Structure (JSON)

```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.4",
  "serialNumber": "urn:uuid:3e671687-395b-41f5-a30f-a58921a69b79",
  "version": 1,
  "metadata": {
    "timestamp": "2026-01-18T10:15:00Z",
    "tools": [
      {
        "vendor": "CycloneDX",
        "name": "cyclonedx-python",
        "version": "3.0.0"
      }
    ],
    "authors": [
      {
        "name": "ggen Release Team",
        "email": "release@example.com"
      }
    ],
    "component": {
      "bom-ref": "pkg:cargo/ggen@6.0.0",
      "type": "application",
      "name": "ggen",
      "version": "6.0.0",
      "description": "Specification-Driven Code Generation v6.0.0",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          }
        }
      ],
      "externalReferences": [
        {
          "type": "source-repository",
          "url": "https://github.com/seanchatmangpt/ggen"
        },
        {
          "type": "build-system",
          "url": "https://github.com/seanchatmangpt/ggen/actions"
        }
      ]
    }
  },
  "components": [
    {
      "bom-ref": "pkg:cargo/oxigraph@0.5.1",
      "type": "library",
      "name": "oxigraph",
      "version": "0.5.1",
      "purl": "pkg:cargo/oxigraph@0.5.1",
      "description": "SPARQL engine and RDF store",
      "licenses": [
        {
          "license": {
            "name": "Apache-2.0"
          }
        }
      ],
      "hashes": [
        {
          "alg": "SHA-256",
          "content": "abc123def456..."
        }
      ],
      "externalReferences": [
        {
          "type": "source-repository",
          "url": "https://github.com/oxigraph/oxigraph"
        }
      ]
    }
  ],
  "vulnerabilities": [
    {
      "ref": "pkg:cargo/openssl-sys@0.9.89",
      "vulnerability": {
        "bom-ref": "CVE-2025-0001",
        "id": "CVE-2025-0001",
        "source": {
          "name": "NVD",
          "url": "https://nvd.nist.gov/vuln/detail/CVE-2025-0001"
        },
        "severity": "high",
        "description": "Buffer overflow in OpenSSL AES operations"
      }
    }
  ]
}
```

---

## Provenance Verification Checklist

**Before releasing, verify SBOM provenance:**

- [ ] **SBOM Generated**
  - [ ] SBOM in CycloneDX JSON format
  - [ ] SBOM validates against CycloneDX schema
  - [ ] SBOM has all components listed (no omissions)
  - [ ] SBOM has license information for all components

- [ ] **Dependency Scanning**
  - [ ] CVE scan completed (cargo audit, npm audit, Grype)
  - [ ] No critical/high CVEs in dependencies
  - [ ] All CVEs documented in SBOM
  - [ ] Remediation plan provided (if CVEs present)

- [ ] **License Compliance**
  - [ ] All dependencies have permissive licenses (Apache 2.0, MIT, BSD)
  - [ ] No copyleft dependencies (GPL, AGPL)
  - [ ] No proprietary/SSPL dependencies
  - [ ] License check passed (cargo license, license-checker)

- [ ] **Build Attestation**
  - [ ] in-toto metadata generated
  - [ ] Build signed with authorized key
  - [ ] Signature verified (in-toto-verify passes)
  - [ ] Metadata links to SBOM + binary

- [ ] **Provenance Documentation**
  - [ ] Component sources documented (GitHub, crates.io, npm registry)
  - [ ] Builder identity recorded (GitHub Actions)
  - [ ] Build timestamp recorded (ISO 8601)
  - [ ] Build command recorded (exact cargo/npm command)

- [ ] **Reproducibility**
  - [ ] Deterministic build verified (hash matches on rebuild)
  - [ ] Build environment documented (Rust version, cargo version)
  - [ ] Base images pinned to digest (not floating tags)
  - [ ] Dockerfile uses specific base image versions

- [ ] **Supply Chain Security**
  - [ ] All external dependencies from trusted sources (crates.io, npm, Docker Hub)
  - [ ] No dependencies from suspicious/unknown sources
  - [ ] Dependency changes reviewed + approved
  - [ ] Typosquatting check performed (no malicious package names)

- [ ] **Archive & Publication**
  - [ ] SBOM archived (/ggen/receipts/sbom/)
  - [ ] SBOM signed (GPG + in-toto)
  - [ ] SBOM published with release (GitHub releases)
  - [ ] in-toto metadata published (.link.json file)

---

## Definition of Done

SBOM & Provenance is complete when:

- ✅ SBOM generated in CycloneDX JSON format
- ✅ SBOM includes all components (Rust, npm, Docker, Terraform)
- ✅ SBOM includes all licenses (verified permissive only)
- ✅ SBOM includes all known CVEs (from dependency scans)
- ✅ CVE scan passed (no critical/high vulnerabilities)
- ✅ License compliance check passed (no copyleft licenses)
- ✅ in-toto attestation generated + signed
- ✅ Build provenance documented (source, builder, timestamp)
- ✅ Reproducible build verified (hash matches)
- ✅ SBOM validated (schema validation passes)
- ✅ SBOM archived (/ggen/receipts/sbom/v6.0.0/)
- ✅ SBOM published with release (GitHub releases)
- ✅ in-toto metadata published (.link.json)
- ✅ Customer can verify supply chain (documentation provided)

---

## Receipt Contract

**This document is a binding policy specification. Every release must produce:**

1. **SBOM Receipt**: CycloneDX JSON file with all components
2. **CVE Receipt**: Security scan report (cargo audit, npm audit, Grype)
3. **License Receipt**: License compliance report (all permissive licenses)
4. **Provenance Receipt**: JSON record of component sources + build info
5. **Attestation Receipt**: in-toto .link.json file (cryptographically signed)
6. **Verification Receipt**: Customer verification instructions + public key

**All receipts stored in `/ggen/receipts/sbom/v6.0.0/` (cryptographically signed, immutable).**

**Verification**: Government can audit supply chain by requesting `/ggen/receipts/sbom/*/`

---

**Last Updated**: January 2026 | **Next Review**: April 2026
