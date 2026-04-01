# Bundles and Profiles

**Version:** 6.0.1
**Last Updated:** 2026-03-31
**Status:** Production

## Overview

**Bundles** and **profiles** are ergonomic conveniences built on top of atomic packs:

- **Bundles** — Deterministic aliases that expand to atomic packs
- **Profiles** — Enterprise policy overlays that constrain atomic packs

### Core Principles

1. **Atomic packs are canonical** — All bundles expand to atomic packs before compile
2. **Bundle expansion is deterministic** — Same bundle always expands to same atomic packs
3. **Bundle expansion is visible** — Users see exactly what atomic packs are included
4. **Profiles enforce policy** — Enterprise constraints via trust tiers, policies, and runtime requirements

## Bundles

### What Are Bundles?

Bundles are **ergonomic aliases** for common capability combinations.

```bash
# Bundle (ergonomic alias)
$ ggen packs install mcp-rust-axum

# Expands to atomic packs (canonical form)
$ ggen capability enable mcp --projection rust --runtime axum

Both commands result in:
  - surface-mcp
  - projection-rust
  - runtime-axum
  - core-ontology (foundation)
  - core-hooks (foundation)
  # ... (other foundation packs)
```

### Predefined Bundles

#### MCP Bundles

| Bundle | Expands To | Runtime | Use Case |
|--------|------------|---------|----------|
| `mcp-rust` | `surface-mcp`, `projection-rust` | None (user must specify) | Base MCP with Rust |
| `mcp-rust-stdio` | `surface-mcp`, `projection-rust`, `runtime-stdio` | stdio | MCP CLI tool |
| `mcp-rust-axum` | `surface-mcp`, `projection-rust`, `runtime-axum` | axum | MCP HTTP server |
| `mcp-typescript` | `surface-mcp`, `projection-typescript` | None (user must specify) | Base MCP with TypeScript |
| `mcp-typescript-standalone` | `surface-mcp`, `projection-typescript`, `runtime-standalone` | standalone | MCP standalone binary |

#### A2A Bundles

| Bundle | Expands To | Runtime | Use Case |
|--------|------------|---------|----------|
| `a2a-rust` | `surface-a2a`, `projection-rust` | None (user must specify) | Base A2A with Rust |
| `a2a-rust-axum` | `surface-a2a`, `projection-rust`, `runtime-axum` | axum | A2A HTTP server |
| `a2a-typescript` | `surface-a2a`, `projection-typescript` | None (user must specify) | Base A2A with TypeScript |

#### Contract Bundles

| Bundle | Expands To | Runtime | Use Case |
|--------|------------|---------|----------|
| `openapi-rust` | `contract-openapi`, `projection-rust` | None | REST API with OpenAPI |
| `graphql-typescript` | `contract-graphql`, `projection-typescript` | None | GraphQL API |

### Deterministic Expansion

All bundle expansion is **deterministic** and **visible**:

```bash
$ ggen packs install mcp-rust-axum

Bundle expansion: mcp-rust-axum
  ├─ surface-mcp (v1.2.3)
  ├─ projection-rust (v2.1.0)
  └─ runtime-axum (v0.9.5)

Foundation packs (auto-added):
  ├─ core-ontology (v1.0.0)
  ├─ core-hooks (v1.0.0)
  ├─ core-receipts (v1.0.0)
  ├─ core-versioning (v1.0.0)
  ├─ core-validation (v1.0.0)
  └─ core-policy (v1.0.0)

Lockfile: .ggen/packs.lock
```

### Bundle Validation

Bundles are validated at registration time:

```bash
# Bundle with duplicate atomic pack
[invalid-bundle]
name = "duplicate-test"
atomic_packs = ["surface-mcp", "surface-mcp"]

# Validation error: Bundle contains duplicate atomic pack: surface-mcp
```

### Custom Bundles

Users can define custom bundles in `ggen.toml`:

```toml
# ggen.toml
[bundles.my-mcp-server]
description = "My custom MCP server"
atomic_packs = [
    "surface-mcp",
    "projection-rust",
    "runtime-axum",
    "policy-strict",
]
runtime = "axum"
```

```bash
$ ggen packs install my-mcp-server

Custom bundle expansion: my-mcp-server
  ├─ surface-mcp
  ├─ projection-rust
  ├─ runtime-axum
  └─ policy-strict
```

## Profiles

### What Are Profiles?

Profiles are **enterprise policy overlays** that constrain atomic pack composition.

Profiles enforce:
- **Trust tier requirements** — Minimum trust tier for packs
- **Registry class restrictions** — Allowed registry sources
- **Policy requirements** — Mandatory policy packs
- **Runtime constraints** — Allowed/disallowed runtimes
- **Receipt requirements** — Signature and chaining requirements

### Predefined Profiles

#### `default`

No constraints. Suitable for development and testing.

```toml
[profile]
name = "default"
trust_tier = "Experimental"  # Allow any pack
registry_class = "Public"     # Allow any registry
allow_defaults = true         # Allow inferred capabilities
receipt_required = false      # No signature required
```

#### `enterprise-strict`

Enterprise safety without regulated environment requirements.

```toml
[profile]
name = "enterprise-strict"
trust_tier = "EnterpriseApproved"  # Require approved packs
registry_class = "PrivateEnterprise"  # Require private registry
allow_defaults = false  # Forbid all inferred capabilities
receipt_required = true  # Require signed receipts

policies = [
    "policy-no-defaults",      # Forbid inferred capabilities
    "policy-strict",           # Strict validation
]

runtime_constraints = [
    { require_explicit = true },  # Runtime must be explicit
]

receipt_requirements = {
    signature = true,      # Require Ed25519 signature
    chain = false,         # Chained receipts optional
}
```

#### `regulated-finance`

Fortune 5 CISO requirements for regulated finance environments.

```toml
[profile]
name = "regulated-finance"
trust_tier = "EnterpriseCertified"  # Only certified packs
registry_class = "MirroredAirGapped"  # Air-gapped mirror only
allow_defaults = false  # Forbid all inferred capabilities
receipt_required = true  # Require signed receipts

policies = [
    "policy-no-defaults",           # Forbid inferred capabilities
    "policy-strict",                # Strict validation
    "receipt-enterprise-signed",    # Enterprise signatures
    "validator-shacl",              # SHACL validation
]

runtime_constraints = [
    { allowed_runtimes = ["axum", "actix"] },  # Only HTTP server runtimes
    { forbid_defaults = true },
    { require_explicit = true },
]

receipt_requirements = {
    signature = true,      # Require Ed25519 signature
    chain = true,          # Require hash-linked chain
    min_chain_length = 3,  # Minimum 3 epochs in chain
}
```

### Profile Usage

```bash
# Enable capability with profile
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
  Runtime constraints: axum, actix only
  Receipt: signed + chained (min 3 epochs)

Lockfile: .ggen/packs.lock
```

### Profile Enforcement

Profiles are enforced at **install time** and **sync time**:

```bash
# Install with profile
$ ggen packs install surface-mcp --profile regulated-finance

Profile check: regulated-finance
  ✅ Trust tier: EnterpriseCertified (required)
  ✅ Registry: MirroredAirGapped (required)
  ✅ Signature: Verified (Ed25519)
  ✅ Receipt chain: 5 epochs (min 3 required)

Install successful.

---

# Try to install experimental pack
$ ggen packs install experimental-pack --profile regulated-finance

Profile check: regulated-finance
  ❌ Trust tier: Experimental (required: EnterpriseCertified)
  ❌ Policy violation: regulated-finance profile requires EnterpriseCertified

Install failed: Pack does not meet profile requirements
```

## Runtime Constraints

Profiles can constrain which runtimes are allowed:

```toml
[profile.runtime_constraints]
# Allow only HTTP server runtimes
allowed_runtimes = ["axum", "actix"]

# Forbid stdio runtime
forbidden_runtimes = ["stdio", "embedded"]

# Require explicit runtime (no inference)
require_explicit = true

# Forbid default values in templates
forbid_defaults = true
```

```bash
$ ggen capability enable mcp --projection rust --runtime stdio \
    --profile regulated-finance

Profile error: regulated-finance
  Runtime constraint violation:
    stdio is not in allowed runtimes (axum, actix)

Resolution: Use allowed runtime or change profile
```

## Trust Tier Enforcement

Profiles enforce minimum trust tiers:

```bash
# EnterpriseCertified profile allows all tiers
$ ggen packs install surface-mcp --profile default  # OK

# EnterpriseApproved profile requires EnterpriseApproved or higher
$ ggen packs install experimental-pack --profile enterprise-strict

Profile error: enterprise-strict
  Trust tier violation:
    experimental-pack is Experimental
    Required: EnterpriseApproved or higher

Resolution: Use approved pack or change profile
```

## Registry Class Enforcement

Profiles enforce registry class restrictions:

```bash
# Regulated finance profile requires MirroredAirGapped registry
$ ggen packs install surface-mcp --profile regulated-finance \
    --registry https://crates.io

Profile error: regulated-finance
  Registry violation:
    crates.io is Public registry
    Required: MirroredAirGapped

Resolution: Use air-gapped mirror or change profile
```

## Policy Requirements

Profiles can require specific policy packs:

```toml
[profile]
policies = [
    "policy-no-defaults",   # Forbid inferred capabilities
    "policy-strict",        # Strict validation
    "receipt-enterprise-signed",  # Enterprise signatures
]
```

These policies are **automatically added** to the composition:

```bash
$ ggen capability enable mcp --projection rust --profile enterprise-strict

Resolved capability:
  - surface-mcp
  - projection-rust
  - policy-no-defaults (profile)  # Auto-added
  - policy-strict (profile)        # Auto-added
  - core-ontology (foundation)
```

## Receipt Requirements

Profiles can specify receipt requirements:

```toml
[profile.receipt_requirements]
# Require Ed25519 signature
signature = true

# Require hash-linked receipt chain
chain = true

# Minimum chain length (for reproducibility)
min_chain_length = 3

# Require receipt from specific signing key
signing_key = "enterprise-signing-key-2024"
```

```bash
$ ggen sync --profile regulated-finance

Receipt verification:
  ✅ All pack signatures valid (Ed25519)
  ✅ Signing key: enterprise-signing-key-2024
  ✅ Receipt chain: 5 epochs (min 3 required)
  ✅ Chain integrity: verified
```

## Custom Profiles

Users can define custom profiles in `ggen.toml`:

```toml
# ggen.toml
[profile.my-custom]
name = "my-custom"
description = "My custom profile"

trust_tier = "EnterpriseApproved"
registry_class = "PrivateEnterprise"
allow_defaults = false
receipt_required = true

policies = [
    "policy-no-defaults",
]

runtime_constraints = [
    { allowed_runtimes = ["axum"] },
    { require_explicit = true },
]

receipt_requirements = {
    signature = true,
    chain = false,
}
```

## Bundle + Profile Composition

Bundles and profiles compose:

```bash
$ ggen capability enable mcp-rust-axum \
    --profile regulated-finance

Resolved capability:
  Bundle expansion: mcp-rust-axum
    ├─ surface-mcp
    ├─ projection-rust
    └─ runtime-axum

  Profile overlay: regulated-finance
    ├─ policy-no-defaults
    ├─ policy-strict
    ├─ receipt-enterprise-signed
    └─ validator-shacl

  Foundation packs (auto-added)
    ├─ core-ontology
    ├─ core-hooks
    └─ ... (4 more)

  Total: 13 atomic packs

  Profile checks:
    ✅ Trust tier: All packs are EnterpriseCertified
    ✅ Registry: All packs from MirroredAirGapped
    ✅ Runtime: axum is allowed
    ✅ Receipts: All signed, chained (5 epochs)

Lockfile: .ggen/packs.lock
```

## Lockfile Format

The lockfile records both bundle expansion and profile selection:

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
    ],
    "runtime_constraints": {
      "allowed_runtimes": ["axum", "actix"],
      "require_explicit": true
    }
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
    // ... (other packs)
  ],
  "digest": "sha256:...",
  "signature": "ed25519:..."
}
```

## Further Reading

- [ATOMIC_PACKS.md](ATOMIC_PACKS.md) — Atomic pack reference
- [CLI_REFERENCE.md](CLI_REFERENCE.md) — Command-line usage
- [ARCHITECTURE.md](ARCHITECTURE.md) — System architecture
- [SECURITY_MODEL.md](SECURITY_MODEL.md) — Security and trust model
