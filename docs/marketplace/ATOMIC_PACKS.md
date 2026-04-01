# Atomic Packs Reference

**Version:** 6.0.1
**Last Updated:** 2026-03-31
**Status:** Production

## Overview

**Atomic packs are the canonical semantic unit** of the governed marketplace. All capability declarations (bundles, profiles, user requests) expand deterministically to atomic packs before compilation.

### Key Principle

> **Bundles are aliases. Atomic packs are truth.**

Every bundle expands to atomic packs. Every profile constrains atomic packs. Every receipt proves atomic pack contributions.

## Atomic Pack Classes

### Surface Packs

Enterprise-visible interfaces that define **what** capability is provided, not **how** it's implemented.

| Pack ID | Description | Use When |
|---------|-------------|----------|
| `surface-mcp` | Model Context Protocol surface | Building MCP tools/servers |
| `surface-a2a` | Agent-to-Agent protocol surface | Building multi-agent systems |

**Example:**
```bash
$ ggen capability enable mcp --projection rust

Expands to:
  - surface-mcp
  - projection-rust
  - core-ontology (foundation)
```

### Contract Packs

External API contracts that define the **interface shape** independently of implementation.

| Pack ID | Description | Use When |
|---------|-------------|----------|
| `contract-openapi` | OpenAPI/Swagger contract | Building REST APIs with OpenAPI spec |
| `contract-graphql` | GraphQL contract | Building GraphQL APIs |

**Example:**
```bash
$ ggen capability enable openapi --projection rust

Expands to:
  - contract-openapi
  - projection-rust
```

### Projection Packs

Implementation language projections. **CISO requirement:** Surface/contract before projection.

| Pack ID | Language | Use When |
|---------|----------|----------|
| `projection-rust` | Rust 1.91+ | High-performance, memory-safe, Tokio async |
| `projection-typescript` | TypeScript 5+ | Node.js, Deno, browser runtimes |
| `projection-python` | Python 3.11+ | Data science, ML, scripting |
| `projection-java` | Java 26 | Enterprise Java, Spring Boot |
| `projection-go` | Go 1.21+ | Cloud-native, microservices |

**Example:**
```bash
$ ggen capability enable mcp --projection rust

Expands to:
  - surface-mcp
  - projection-rust
```

### Runtime Packs

Deployment model and execution environment.

| Pack ID | Runtime | Use When |
|---------|---------|----------|
| `runtime-stdio` | Standard input/output | CLI tools, stdin/stdout communication |
| `runtime-axum` | Axum HTTP server | Async HTTP servers (Tokio) |
| `runtime-actix` | Actix Web HTTP server | HTTP servers (actor model) |
| `runtime-embedded` | Embedded library | Library integration, no standalone server |
| `runtime-standalone` | Standalone binary | Self-contained executables |

**Example:**
```bash
$ ggen capability enable mcp --projection rust --runtime axum

Expands to:
  - surface-mcp
  - projection-rust
  - runtime-axum
```

### Policy Packs

Governance rules that enforce enterprise safety requirements.

| Pack ID | Policy | Use When |
|---------|--------|----------|
| `policy-no-defaults` | Forbid all inferred capabilities | Regulated environments, explicit-only mode |
| `policy-strict` | Strict validation, no warnings as errors | High-assurance systems, defense-in-depth |

**Example:**
```toml
# ggen.toml
[profile]
name = "enterprise-strict"
policies = ["policy-no-defaults", "policy-strict"]
```

### Validator Packs

Quality gates and validation logic.

| Pack ID | Validates | Use When |
|---------|-----------|----------|
| `validator-protocol-visible-values` | API protocol fields | Ensuring protocol compatibility |
| `validator-shacl` | RDF ontology shapes | Semantic validation, SHACL shapes |

**Example:**
```bash
$ ggen packs validate --validator validator-shacl
```

### Receipt Packs

Proof formats for cryptographic provenance.

| Pack ID | Receipt Type | Use When |
|---------|--------------|----------|
| `receipt-enterprise-signed` | Ed25519-signed receipts | Enterprise audit trails |
| `receipt-chained` | Hash-linked receipt chain | Reproducible builds, incremental verification |

**Example:**
```bash
$ ggen sync --audit

Receipt verification:
  ✅ All pack signatures valid (Ed25519)
  ✅ Receipt chain intact (3 epochs)
```

### Consequence Packs

Migration and upgrade behavior definitions.

| Pack ID | Consequence | Use When |
|---------|-------------|----------|
| `consequence-semver-migration` | Semantic versioning migrations | Automated version upgrades |
| `consequence-breaking-change` | Breaking change handling | Major version upgrades |

**Example:**
```bash
$ ggen packs upgrade surface-mcp --from 1.0 --to 2.0

Consequence plan: consequence-breaking-change
  - Breaking changes detected
  - Migration path: manual review required
```

### Core Packs

Foundation infrastructure that owns shared ontology and base capabilities.

| Pack ID | Owns | Use When |
|---------|------|----------|
| `core-ontology` | Base RDF vocabulary | All projects (auto-included) |
| `core-hooks` | Lifecycle hooks | All projects (auto-included) |
| `core-receipts` | Receipt formats | All projects (auto-included) |
| `core-versioning` | Semver handling | All projects (auto-included) |
| `core-validation` | Validation logic | All projects (auto-included) |
| `core-policy` | Policy enforcement | All projects (auto-included) |

**Rule:** Foundation packs are **always included**. No non-foundation pack may define core ontology terms.

## Atomic Pack ID Format

### Syntax

```
<category>-<name>

Examples:
- surface-mcp
- projection-rust
- runtime-axum
- policy-no-defaults
- core-ontology
```

### Code Definition

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AtomicPackClass {
    // Surfaces
    SurfaceMcp,
    SurfaceA2a,

    // Contracts
    ContractOpenapi,
    ContractGraphql,

    // Projections
    ProjectionRust,
    ProjectionTypescript,
    ProjectionPython,
    ProjectionJava,
    ProjectionGo,

    // Runtimes
    RuntimeStdio,
    RuntimeAxum,
    RuntimeActix,
    RuntimeEmbedded,
    RuntimeStandalone,

    // Policies
    PolicyNoDefaults,
    PolicyStrict,

    // Validators
    ValidatorProtocolVisibleValues,
    ValidatorShacl,

    // Receipts
    ReceiptEnterpriseSigned,
    ReceiptChained,

    // Consequences
    ConsequenceSemverMigration,
    ConsequenceBreakingChange,

    // Core
    CoreOntology,
    CoreHooks,
    CoreReceipts,
    CoreVersioning,
    CoreValidation,
    CorePolicy,
}
```

## When to Use Each Pack Type

### Decision Tree

```
Need to expose a capability?
├─ What interface?
│  ├─ Model Context Protocol → surface-mcp
│  ├─ Agent-to-Agent → surface-a2a
│  ├─ REST API → contract-openapi
│  └─ GraphQL API → contract-graphql
│
├─ What language?
│  ├─ Rust → projection-rust
│  ├─ TypeScript → projection-typescript
│  ├─ Python → projection-python
│  ├─ Java → projection-java
│  └─ Go → projection-go
│
├─ How deployed?
│  ├─ CLI/stdio → runtime-stdio
│  ├─ HTTP server (Tokio) → runtime-axum
│  ├─ HTTP server (Actix) → runtime-actix
│  ├─ Embedded library → runtime-embedded
│  └─ Standalone binary → runtime-standalone
│
└─ What governance?
   ├─ Regulated environment → policy-no-defaults
   ├─ High-assurance → policy-strict
   └─ Enterprise audit → receipt-enterprise-signed
```

### Examples

#### Example 1: MCP Server in Rust

**Requirement:** Build an MCP server that exposes tools to Claude, using Rust for performance, running as stdio CLI.

```bash
$ ggen capability enable mcp --projection rust --runtime stdio

Resolved capability:
  - surface-mcp
  - projection-rust
  - runtime-stdio
  - core-ontology (foundation)
  - core-hooks (foundation)

Lockfile: .ggen/packs.lock
```

#### Example 2: A2A Multi-Agent System

**Requirement:** Build a multi-agent system using A2A protocol, in Rust, with Axum HTTP runtime.

```bash
$ ggen capability enable a2a --projection rust --runtime axum

Resolved capability:
  - surface-a2a
  - projection-rust
  - runtime-axum
  - core-ontology (foundation)
  - core-hooks (foundation)

Lockfile: .ggen/packs.lock
```

#### Example 3: OpenAPI REST API

**Requirement:** Build a REST API with OpenAPI specification, in Rust.

```bash
$ ggen capability enable openapi --projection rust

Resolved capability:
  - contract-openapi
  - projection-rust
  - core-ontology (foundation)
  - core-hooks (foundation)

Lockfile: .ggen/packs.lock
```

#### Example 4: Regulated Finance Profile

**Requirement:** Build an MCP server for regulated finance, requiring strict policy enforcement.

```bash
$ ggen capability enable mcp \
    --projection rust \
    --runtime axum \
    --profile regulated-finance

Resolved capability:
  - surface-mcp
  - projection-rust
  - runtime-axum
  - policy-no-defaults (profile)
  - policy-strict (profile)
  - receipt-enterprise-signed (profile)
  - core-ontology (foundation)
  - core-hooks (foundation)

Profile: regulated-finance
  - Trust tier required: EnterpriseCertified
  - Registry class: PrivateEnterprise or MirroredAirGapped
  - No defaults allowed: true
  - Receipt signing required: true

Lockfile: .ggen/packs.lock
```

## Pack Composition Rules

### Rule 1: Surface/Contract Before Projection

**CISO requirement:** Enterprise-visible interface must be declared before implementation language.

```bash
# ✅ Correct: Declare surface first
$ ggen capability enable mcp --projection rust

# ❌ Wrong: Projection without surface
$ ggen capability enable rust  # Error: rust is not a valid capability
```

### Rule 2: Runtime Must Be Explicit or Bundle-Provided

**CISO requirement:** Runtime inference is forbidden in regulated environments.

```bash
# ✅ Correct: Explicit runtime
$ ggen capability enable mcp --projection rust --runtime axum

# ✅ Correct: Bundle provides runtime
$ ggen packs install mcp-rust-axum  # Runtime: axum

# ❌ Wrong: No runtime specified
$ ggen packs install mcp-rust  # Error: Runtime required
```

### Rule 3: Foundation Packs Always Included

Foundation packs are automatically added to every composition.

```bash
$ ggen capability enable mcp --projection rust

Resolved capability:
  - surface-mcp
  - projection-rust
  - core-ontology (foundation)  # Auto-added
  - core-hooks (foundation)     # Auto-added
  - core-receipts (foundation)  # Auto-added
  - core-versioning (foundation)  # Auto-added
  - core-validation (foundation)  # Auto-added
  - core-policy (foundation)    # Auto-added
```

### Rule 4: No Duplicate Projections

A composition cannot include multiple projections for the same language.

```bash
# ❌ Wrong: Duplicate projection
$ ggen packs compose surface-mcp projection-rust projection-rust
Error: Duplicate atomic pack: projection-rust

# ❌ Wrong: Conflicting projections
$ ggen packs compose surface-mcp projection-rust projection-typescript
Error: Multiple projection languages detected (rust, typescript)
Resolution: Use exclusive ownership or specify one projection
```

## Pack Metadata

Each atomic pack includes metadata in `package.toml`:

```toml
# marketplace/packages/surface-mcp/package.toml
[package]
name = "surface-mcp"
version = "1.2.3"
description = "Model Context Protocol surface"
category = "Surface"
atomic_class = "SurfaceMcp"

[ownership]
# File paths owned by this pack
files = [
    { path = "src/mcp/", class = "Exclusive" },
    { path = "src/tools.rs", class = "Exclusive" },
]

# RDF namespaces owned
namespaces = [
    { uri = "http://ggen.dev/mcp#", class = "Exclusive" },
]

[templates]
"src/mcp/server.rs" = "templates/server.rs.tera"
"src/mcp/tools.rs" = "templates/tools.rs.tera"

[queries]
tools = "queries/sparql/tools.rq"
all_tools = "queries/sparql/all_tools.rq"

[dependencies]
# Required atomic packs
core-ontology = "1.0.0"
core-hooks = "1.0.0"

[trust]
tier = "EnterpriseCertified"
signature = "ed25519:..."
signing_key = "enterprise-signing-key-2024"
```

## Further Reading

- [BUNDLES_AND_PROFILES.md](BUNDLES_AND_PROFILES.md) — Bundle aliases and profiles
- [CLI_REFERENCE.md](CLI_REFERENCE.md) — Command-line usage
- [ARCHITECTURE.md](ARCHITECTURE.md) — System architecture
