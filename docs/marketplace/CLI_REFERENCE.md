# Marketplace CLI Reference

**Version:** 6.0.1
**Last Updated:** 2026-03-31
**Status:** Production

## Overview

The ggen marketplace provides two primary CLI nouns:

- **`capability`** — Capability-first composition (canonical form)
- **`packs`** — Pack management and inspection

## Capability Commands

### `capability enable`

Enable a capability with explicit parameters (canonical form).

**Syntax:**
```bash
ggen capability enable <surface> [options]
```

**Arguments:**
- `<surface>` — Surface/contract type (mcp, a2a, openapi, graphql)

**Options:**
- `--projection <lang>` — Projection language (rust, typescript, python, java, go)
- `--runtime <runtime>` — Runtime (stdio, axum, actix, embedded, standalone)
- `--profile <profile>` — Profile (default, enterprise-strict, regulated-finance)

**Examples:**

```bash
# MCP server in Rust with Axum runtime
$ ggen capability enable mcp --projection rust --runtime axum

Resolved capability:
  - surface-mcp
  - projection-rust
  - runtime-axum
  - core-ontology (foundation)
  - core-hooks (foundation)
  # ... (other foundation packs)

Lockfile: .ggen/packs.lock
```

```bash
# A2A multi-agent system in TypeScript
$ ggen capability enable a2a --projection typescript --runtime standalone

Resolved capability:
  - surface-a2a
  - projection-typescript
  - runtime-standalone
  - core-ontology (foundation)
  # ...
```

```bash
# Regulated finance MCP server
$ ggen capability enable mcp \
    --projection rust \
    --runtime axum \
    --profile regulated-finance

Resolved capability:
  - surface-mcp (EnterpriseCertified)
  - projection-rust (EnterpriseCertified)
  - runtime-axum (EnterpriseCertified)
  - policy-no-defaults (profile)
  - policy-strict (profile)
  - receipt-enterprise-signed (profile)
  - validator-shacl (profile)
  - core-ontology (foundation)

Profile: regulated-finance
  Trust tier: EnterpriseCertified
  Registry: MirroredAirGapped
  Policies: 4 enforced
```

### `capability disable`

Disable a capability and remove its atomic packs.

**Syntax:**
```bash
ggen capability disable <capability>
```

**Examples:**

```bash
$ ggen capability disable mcp-rust-axum

Removing capability: mcp-rust-axum
  ✗ surface-mcp
  ✗ projection-rust
  ✗ runtime-axum

Lockfile updated: .ggen/packs.lock
```

### `capability inspect`

Inspect a capability's atomic pack composition.

**Syntax:**
```bash
ggen capability inspect <capability>
```

**Examples:**

```bash
$ ggen capability inspect mcp-rust-axum

Capability: mcp-rust-axum
Bundle expansion:
  - surface-mcp (v1.2.3, EnterpriseCertified)
  - projection-rust (v2.1.0, EnterpriseCertified)
  - runtime-axum (v0.9.5, EnterpriseCertified)

Foundation packs (auto-added):
  - core-ontology (v1.0.0)
  - core-hooks (v1.0.0)
  - core-receipts (v1.0.0)
  - core-versioning (v1.0.0)
  - core-validation (v1.0.0)
  - core-policy (v1.0.0)

Ownership declarations:
  - src/mcp/ → surface-mcp (Exclusive)
  - src/main.rs → projection-rust (Exclusive)
  - src/server.rs → runtime-axum (Exclusive)

Conflicts: None detected
```

### `capability list`

List available capabilities (bundles).

**Syntax:**
```bash
ggen capability list [--filter <filter>]
```

**Options:**
- `--filter <filter>` — Filter by surface, projection, or runtime

**Examples:**

```bash
$ ggen capability list

Available capabilities:
  mcp-rust              MCP surface with Rust projection
  mcp-rust-stdio        MCP surface with Rust, stdio runtime
  mcp-rust-axum         MCP surface with Rust, Axum runtime
  a2a-rust              A2A surface with Rust projection
  a2a-rust-axum         A2A surface with Rust, Axum runtime
  openapi-rust          OpenAPI contract with Rust projection
  graphql-typescript    GraphQL contract with TypeScript projection
```

```bash
$ ggen capability list --filter mcp

MCP capabilities:
  mcp-rust              MCP surface with Rust projection
  mcp-rust-stdio        MCP surface with Rust, stdio runtime
  mcp-rust-axum         MCP surface with Rust, Axum runtime
```

### `capability graph`

Show resolved atomic pack graph before compile.

**Syntax:**
```bash
ggen capability graph
```

**Examples:**

```bash
$ ggen capability graph

Pack graph:
  surface-mcp
  ├─ depends on: core-ontology, core-hooks
  ├─ ownership: src/mcp/ (Exclusive)
  └─ templates: src/mcp/server.rs, src/mcp/tools.rs

  projection-rust
  ├─ depends on: core-ontology
  ├─ ownership: src/main.rs (Exclusive)
  └─ templates: src/main.rs, Cargo.toml

  runtime-axum
  ├─ depends on: projection-rust
  ├─ ownership: src/server.rs (Exclusive)
  └─ templates: src/server.rs

  core-ontology (foundation)
  ├─ ownership: http://ggen.dev/core# (Exclusive)
  └─ templates: (none)

  core-hooks (foundation)
  ├─ ownership: hooks/ (Exclusive)
  └─ templates: hooks/pre-sync.sh, hooks/post-sync.sh

Conflict graph: None
```

### `capability trust`

Show trust status for all packs.

**Syntax:**
```bash
ggen capability trust [--pack <pack-id>]
```

**Options:**
- `--pack <pack-id>` — Show trust status for specific pack

**Examples:**

```bash
$ ggen capability trust

Pack trust status:
  surface-mcp           EnterpriseCertified  ✅
  projection-rust       EnterpriseCertified  ✅
  runtime-axum          EnterpriseCertified  ✅
  policy-no-defaults    EnterpriseCertified  ✅
  experimental-pack     Experimental        ⚠️
  blocked-pack          Blocked             ❌

Profile: enterprise-strict
  Required tier: EnterpriseApproved
  Allowed packs: 3/5
  Blocked packs: 1 (blocked-pack)
```

```bash
$ ggen capability trust --pack surface-mcp

Pack: surface-mcp
Version: 1.2.3
Trust tier: EnterpriseCertified
Registry: PrivateEnterprise (registry.internal.com)
Signature: ✅ Verified (Ed25519)
Signing key: enterprise-signing-key-2024
Digest: ✅ SHA256 matches
Approved: 2024-01-15
Next audit: 2025-01-15
```

### `capability conflicts`

Detect and report conflicts.

**Syntax:**
```bash
ggen capability conflicts
```

**Examples:**

```bash
$ ggen capability conflicts

Conflicts detected:
  ✗ Exclusive overlap: src/main.rs
    Owned by: projection-rust, projection-typescript
    Severity: Error
    Resolution: Remove one projection or declare mergeable

  ✗ Incompatible merge strategies: config/routes.toml
    projection-rust wants: Concat
    surface-mcp wants: LastWriterWins
    Severity: Error
    Resolution: Align merge strategies or use exclusive ownership

  ⚠ Undeclared target: src/config.rs
    No ownership declaration found
    Severity: Warning
    Resolution: Add ownership declaration or mark as ForbiddenOverlap
```

## Packs Commands

### `packs list`

List all available packs (atomic and bundles).

**Syntax:**
```bash
ggen packs list [--atomic] [--bundles] [--filter <filter>]
```

**Options:**
- `--atomic` — List only atomic packs
- `--bundles` — List only bundles
- `--filter <filter>` — Filter by category or name

**Examples:**

```bash
$ ggen packs list

Atomic packs (25):
  surface-mcp              MCP surface
  surface-a2a              A2A surface
  contract-openapi         OpenAPI contract
  projection-rust          Rust projection
  projection-typescript    TypeScript projection
  runtime-stdio            stdio runtime
  runtime-axum             Axum runtime
  policy-no-defaults       No defaults policy
  core-ontology            Core ontology (foundation)
  # ... (16 more)

Bundles (7):
  mcp-rust                 MCP with Rust
  mcp-rust-stdio           MCP with Rust (stdio)
  mcp-rust-axum            MCP with Rust (Axum)
  a2a-rust                 A2A with Rust
  openapi-rust             OpenAPI with Rust
  graphql-typescript       GraphQL with TypeScript
```

```bash
$ ggen packs list --atomic --filter projection

Atomic packs:
  projection-rust          Rust projection
  projection-typescript    TypeScript projection
  projection-python        Python projection
  projection-java          Java projection
  projection-go            Go projection
```

### `packs install`

Install a pack (atomic or bundle).

**Syntax:**
```bash
ggen packs install <pack-id> [--profile <profile>] [--version <version>]
```

**Options:**
- `--profile <profile>` — Profile to apply
- `--version <version>` — Specific version (default: latest)

**Examples:**

```bash
$ ggen packs install surface-mcp

Installing: surface-mcp
  Version: 1.2.3
  Registry: PrivateEnterprise (registry.internal.com)
  Downloading: 100% |████████████████████| 42 KiB
  Verifying signature: ✅
  Verifying digest: ✅
  Cache: ~/.cache/ggen/packs/surface-mcp/

Lockfile updated: .ggen/packs.lock
```

```bash
$ ggen packs install mcp-rust-axum --profile regulated-finance

Installing bundle: mcp-rust-axum
  Bundle expansion:
    - surface-mcp (v1.2.3)
    - projection-rust (v2.1.0)
    - runtime-axum (v0.9.5)

  Profile checks: regulated-finance
    ✅ Trust tier: All packs are EnterpriseCertified
    ✅ Registry: All packs from MirroredAirGapped
    ✅ Runtime: axum is allowed
    ✅ Receipts: All signed, chained (5 epochs)

  Downloading: 3 packs
  Verifying: 3 signatures
  Cache: ~/.cache/ggen/packs/

Lockfile updated: .ggen/packs.lock
```

### `packs show`

Show pack details.

**Syntax:**
```bash
ggen packs show <pack-id>
```

**Examples:**

```bash
$ ggen packs show surface-mcp

Pack: surface-mcp
ID: surface-mcp
Version: 1.2.3
Description: Model Context Protocol surface
Category: Surface
Class: SurfaceMcp

Metadata:
  Author: ggen team
  License: MIT
  Repository: https://github.com/ggen/mcp-surface

Trust:
  Tier: EnterpriseCertified
  Registry: PrivateEnterprise (registry.internal.com)
  Signature: ed25519:...
  Signing key: enterprise-signing-key-2024
  Digest: sha256:...

Ownership:
  src/mcp/                    Exclusive (surface-mcp)
  src/mcp/server.rs           Exclusive (surface-mcp)
  src/mcp/tools.rs            Exclusive (surface-mcp)
  http://ggen.dev/mcp#        Exclusive (surface-mcp)

Templates:
  src/mcp/server.rs           templates/server.rs.tera
  src/mcp/tools.rs            templates/tools.rs.tera

Queries:
  tools                       queries/sparql/tools.rq
  all_tools                   queries/sparql/all_tools.rq

Dependencies:
  core-ontology               1.0.0
  core-hooks                  1.0.0
```

### `packs compose`

Compose multiple packs and check for conflicts.

**Syntax:**
```bash
ggen packs compose <pack-ids>... [--profile <profile>]
```

**Examples:**

```bash
$ ggen packs compose surface-mcp projection-rust runtime-axum

Composing: 3 atomic packs
  - surface-mcp
  - projection-rust
  - runtime-axum

Conflict check: ✅ No conflicts detected
Ownership map: ✅ Valid
Compatibility: ✅ All dimensions compatible

Composition successful.

Lockfile preview:
  {
    "packs": [
      { "pack_id": "surface-mcp", "version": "1.2.3" },
      { "pack_id": "projection-rust", "version": "2.1.0" },
      { "pack_id": "runtime-axum", "version": "0.9.5" }
    ],
    "digest": "sha256:..."
  }

Write to lockfile with: ggen packs compose ... --write
```

### `packs dependencies`

Show dependency tree for a pack.

**Syntax:**
```bash
ggen packs dependencies <pack-id> [--tree]
```

**Options:**
- `--tree` — Show as tree structure

**Examples:**

```bash
$ ggen packs dependencies surface-mcp --tree

surface-mcp (v1.2.3)
├─ core-ontology (v1.0.0) [foundation]
├─ core-hooks (v1.0.0) [foundation]
├─ core-receipts (v1.0.0) [foundation]
├─ core-versioning (v1.0.0) [foundation]
├─ core-validation (v1.0.0) [foundation]
└─ core-policy (v1.0.0) [foundation]
```

### `packs validate`

Validate a pack's metadata and signatures.

**Syntax:**
```bash
ggen packs validate <pack-id> [--strict]
```

**Options:**
- `--strict` — Enable strict validation (all warnings are errors)

**Examples:**

```bash
$ ggen packs validate surface-mcp

Validating: surface-mcp (v1.2.3)
  ✅ Package.toml: Valid
  ✅ Signature: Verified (Ed25519)
  ✅ Digest: SHA256 matches
  ✅ Ownership declarations: Complete
  ✅ Templates: All valid Tera syntax
  ✅ Queries: All valid SPARQL
  ✅ Dependencies: All resolved
  ⚠  Deprecated fields: None

Validation result: PASSED
```

```bash
$ ggen packs validate experimental-pack --strict

Validating: experimental-pack (v0.1.0)
  ✅ Package.toml: Valid
  ❌ Signature: Not signed
  ❌ Digest: SHA256 mismatch
  ⚠  Ownership declarations: Incomplete
  ⚠  Templates: 2 warnings
  ⚠  Queries: 1 warning
  ✅ Dependencies: All resolved

Validation result: FAILED (strict mode)
  Errors:
    - Signature not signed
    - Digest mismatch
  Warnings:
    - Ownership declarations incomplete
    - Templates have deprecated syntax
    - Queries use deprecated features
```

### `packs search`

Search for packs by name, description, or keywords.

**Syntax:**
```bash
ggen packs search <query> [--category <category>] [--limit <n>]
```

**Options:**
- `--category <category>` — Filter by category (Surface, Projection, Runtime, etc.)
- `--limit <n>` — Maximum results (default: 20)

**Examples:**

```bash
$ ggen packs search mcp

Search results: "mcp"
  1. surface-mcp
     MCP surface for Model Context Protocol
     Category: Surface
     Version: 1.2.3
     Trust: EnterpriseCertified

  2. mcp-rust
     MCP surface with Rust projection
     Category: Bundle
     Version: 1.0.0
     Trust: EnterpriseCertified

  3. mcp-rust-axum
     MCP surface with Rust, Axum runtime
     Category: Bundle
     Version: 1.0.0
     Trust: EnterpriseCertified
```

```bash
$ ggen packs search rust --category projection --limit 5

Search results: "rust" (Projection)
  1. projection-rust
     Rust projection for high-performance systems
     Category: Projection
     Version: 2.1.0
     Trust: EnterpriseCertified
```

## Global Options

All commands support these global options:

- `--config <path>` — Use custom config file (default: `ggen.toml`)
- `--verbose` / `-v` — Enable verbose output
- `--quiet` / `-q` — Suppress non-error output
- `--dry-run` — Show what would be done without doing it
- `--help` / `-h` — Show help for the command

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid usage |
| 3 | Conflict detected |
| 4 | Policy violation |
| 5 | Signature verification failed |
| 6 | Trust tier requirement not met |

## Configuration File

`ggen.toml` in the project root:

```toml
[packs]
# Packs to include in this project
mcp-rust-axum = { version = "1.0" }

[profile]
# Profile to apply
name = "regulated-finance"

[profile.trust]
# Trust tier requirements
tier = "EnterpriseCertified"

[profile.registry]
# Registry restrictions
class = "MirroredAirGapped"

[profile.policies]
# Policies to enforce
policies = [
    "policy-no-defaults",
    "policy-strict",
]

[profile.runtime_constraints]
# Runtime constraints
allowed_runtimes = ["axum", "actix"]
require_explicit = true

[profile.receipt_requirements]
# Receipt requirements
signature = true
chain = true
min_chain_length = 3
```

## Lockfile

`.ggen/packs.lock` is generated automatically:

```json
{
  "version": 1,
  "bundles": [
    {
      "bundle_id": "mcp-rust-axum",
      "expanded_to": [
        "surface-mcp",
        "projection-rust",
        "runtime-axum"
      ]
    }
  ],
  "profile": {
    "name": "regulated-finance",
    "policies": [
      "policy-no-defaults",
      "policy-strict",
      "receipt-enterprise-signed",
      "validator-shacl"
    ]
  },
  "packs": [
    {
      "pack_id": "surface-mcp",
      "version": "1.2.3",
      "source": {
        "Registry": {
          "url": "https://registry.internal.com",
          "class": "MirroredAirGapped"
        }
      },
      "digest": "sha256:...",
      "signature": "ed25519:...",
      "trust_tier": "EnterpriseCertified"
    }
  ],
  "digest": "sha256:...",
  "signature": "ed25519:..."
}
```

## Further Reading

- [ATOMIC_PACKS.md](ATOMIC_PACKS.md) — Atomic pack reference
- [BUNDLES_AND_PROFILES.md](BUNDLES_AND_PROFILES.md) — Bundles and profiles
- [ARCHITECTURE.md](ARCHITECTURE.md) — System architecture
- [PIPELINE_INTEGRATION.md](PIPELINE_INTEGRATION.md) — Pipeline integration
