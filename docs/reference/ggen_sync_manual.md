# `ggen sync` — Comprehensive Reference Manual

**Version**: ggen v26_5_19+  
**Source**: `crates/ggen-cli/src/cmds/sync.rs`  
**Status**: Production

> ⚠️ **DEPRECATED (v26.7.16 routing flip)**: everything below this notice
> documents the retired 17-flag flat `ggen sync` surface (`cmds/sync.rs`,
> disconnected from the compiled binary but retained on disk per this
> project's fix-forward doctrine — see `crates/ggen-cli/src/cmds/mod.rs`).
> The live command is now `ggen sync run`, routed to `ggen-engine`'s `sync`
> noun (`crates/ggen-engine/src/verbs/sync.rs`). See
> [0. Migration: the v26.7.16 CLI Routing Flip](#0-migration-the-v26716-cli-routing-flip)
> for the flag-by-flag disposition before reading the rest of this manual.

---

## 0. Migration: the v26.7.16 CLI Routing Flip

`ggen-cli` and `ggen-engine` both dispatch commands through the same
process-global `clap-noun-verb` `CommandRegistry`. A root flat verb named
`sync` (the old `cmds/sync.rs`) and a noun named `sync` (`ggen-engine`'s
`sync` noun) cannot coexist as compiled top-level subcommands of the same
name, so `cmds/sync.rs`, `cmds/doctor.rs`, `cmds/graph.rs`, and
`cmds/receipt.rs` were disconnected from the `ggen-cli-lib` build (their
`pub mod` declarations in `crates/ggen-cli/src/cmds/mod.rs` are commented
out) in favor of `ggen-engine`'s `sync`/`doctor`/`graph`/`receipt` nouns.
The files themselves are **not deleted** — fix-forward doctrine.

Muscle-memory bare commands still work: `ggen sync`, `ggen doctor`, and
`ggen receipt` (no explicit verb) are rewritten to `ggen sync run`,
`ggen doctor run`, and `ggen receipt verify` respectively by
`inject_default_verbs()` in `crates/ggen-cli/src/lib.rs`, before dispatch.

### Flag disposition: old flat `ggen sync` → new `ggen sync run`

| Old flag | Disposition |
|---|---|
| `--dry-run` | MAPPED to `sync run --dry-run` (native engine flag). |
| `--watch` | MAPPED to `sync run --watch` (native engine flag). |
| `--format` | MAPPED to the global `--format` flag (e.g. `ggen --format json sync run`). |
| `--validate-only` | MAPPED-ADJACENT to `ggen graph validate` — runs the same template lint over the same template set. |
| `--manifest` | REFUSED — engine resolves project root as cwd; `cd` into the project directory instead. |
| `--output-dir` | REFUSED — output paths are owned by each template's `to:` frontmatter field. |
| `--force` | REFUSED — write-overwrite semantics are now per-template via the `force:` frontmatter field. |
| `--audit` | REFUSED (superseded) — engine always emits a signed receipt unconditionally; this is no longer optional. |
| `--rule` | REFUSED — no partial-pipeline/rule selection; sync always runs the full deterministic pipeline. |
| `--verbose` | REFUSED — use the `RUST_LOG` environment variable instead (e.g. `RUST_LOG=debug ggen sync run`). |
| `--timeout` | REFUSED — bound timeouts at the caller/shell level instead. |
| `--stage` | REFUSED — mu-stage selection was specific to the old `ggen-core` pipeline architecture. |
| `--ontology` | REFUSED — ontology source is now configured via `[ontology]` in `ggen.toml`. |
| `--queries` | REFUSED — the old ontology-first "queries directory" pipeline is retired; use `[[generation.rules]]` with a `query.file` entry in `ggen.toml` instead. |
| `--language` | REFUSED — was only meaningful together with `--queries`. |
| `--profile` | REFUSED at the CLI layer for now — sync-profile validation logic still exists in `ggen_marketplace::sync_profile` for potential future rewiring. |
| `--locked` | REFUSED at the CLI layer — engine natively checks `ggen.lock`; lock drift is instead surfaced via `ggen doctor run`. |

---

## Table of Contents

1. [Overview](#1-overview)
2. [Quick-Start Examples](#2-quick-start-examples)
3. [CLI Flags Reference](#3-cli-flags-reference)
4. [Pipeline Architecture](#4-pipeline-architecture)
5. [The --queries Low-Level Bypass Mode](#5-the---queries-low-level-bypass-mode)
6. [Multi-Language Code Generation](#6-multi-language-code-generation)
7. [Enforcement Profiles and Locked Mode](#7-enforcement-profiles-and-locked-mode)
8. [Cryptographic Receipts](#8-cryptographic-receipts)
9. [Exit Codes](#9-exit-codes)
10. [Flag Precedence and Interaction Rules](#10-flag-precedence-and-interaction-rules)
11. [Defensive Workflow Patterns](#11-defensive-workflow-patterns)
12. [Output Structure](#12-output-structure)
13. [Related Documentation](#13-related-documentation)

---

## 1. Overview

`ggen sync` is **the** command in ggen — a unified code synchronization pipeline that supersedes all previous commands (`ggen generate`, `ggen validate`, `ggen template`, etc.). It transforms domain ontologies through SPARQL inference rules into typed, formatted, cryptographically verified code via Tera templates.

### Conceptual model

```
Ontology (RDF/TTL)
      │
      ▼ μ₁  CONSTRUCT   — normalize & expand triples
      │
      ▼ μ₂  SELECT      — extract SPARQL bindings per module
      │
      ▼ μ₃  Tera        — render code from templates
      │
      ▼ μ₄  Canonicalize — rustfmt / validate compilation
      │
      ▼ μ₅  Receipt     — sign cryptographic proof of generation
      │
   Generated Code  +  `.ggen/receipts/latest.json`
```

There are **two pipeline modes**:

| Mode | Trigger | Source of truth |
|---|---|---|
| **Manifest mode** (default) | `ggen sync` or `--manifest` | `ggen.toml` |
| **Low-level mode** | `--queries <dir>` | ontology + `.rq` files directly |

---

## 2. Quick-Start Examples

```bash
# The primary workflow — reads ggen.toml and generates everything
ggen sync

# Preview what would change without writing any files
ggen sync --dry-run

# Preview with detailed reasoning output
ggen sync --dry-run --verbose

# Destructive overwrite — always pair with --audit
ggen sync --force --audit

# Generate only one rule from the manifest
ggen sync --rule structs

# Continuous development loop
ggen sync --watch --verbose

# Validate ontology and SPARQL queries without generating
ggen sync --validate-only

# Machine-readable output for CI scripts
ggen sync --format json

# Full A2A μ₁–μ₅ pipeline with a custom ontology source
ggen sync --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl --audit

# Run only the Tera rendering stage
ggen sync --stage μ₃

# Low-level bypass: skip ggen.toml, use raw .rq files and output Go code
ggen sync \
  --ontology ./businessos.ttl \
  --queries  ./queries/businessos/ \
  --output-dir ./generated/ \
  --language go
```

---

## 3. CLI Flags Reference

All 17 options accepted by `ggen sync`:

| Flag | Type | Default | Description |
|---|---|---|---|
| `--manifest <PATH>` | `String` | `./ggen.toml` | Path to the workspace manifest file. All relative paths inside the manifest are resolved from the manifest's parent directory. |
| `--output-dir <PATH>` | `String` | From manifest | Override the output directory declared in the manifest. Files are written here regardless of per-rule output paths in `ggen.toml`. |
| `--dry-run` | `bool` | `false` | **Preview mode.** Computes all generation steps and reports which files would be created, updated, or left unchanged — but writes nothing to disk. No receipt is emitted for dry-runs. |
| `--force` | `bool` | `false` | **Destructive overwrite.** Existing files are overwritten without prompting. Has no effect when `--dry-run` is set. Must be paired with `--audit` to maintain a rollback trail. |
| `--audit` | `bool` | `false` | Write a detailed JSON audit trail to `.ggen/audit/<timestamp>/`. Records every file operation, transformation decision, and pipeline stage duration. Required when using `--force` in production workflows. |
| `--rule <NAME>` | `String` | All rules | Execute only the named generation rule from `ggen.toml`. Rule names match the `[generation.rules.*.name]` keys in the manifest. |
| `--verbose` | `bool` | `false` | Print detailed per-stage logs to stderr, including triple counts, binding cardinalities, file sizes, and SPARQL query timings. |
| `--watch` | `bool` | `false` | **Continuous mode.** Monitors ontology, query, and template files for changes and re-runs the pipeline automatically. Press `Ctrl-C` to exit. |
| `--validate-only` | `bool` | `false` | Run SHACL and SPARQL `ASK` constraint validation on the ontology without generating any code. Exits 0 if all constraints pass, 1 if any fail. Overrides `--force`. |
| `--format <FORMAT>` | `String` | `text` | Output format for the summary report. Options: `text` (human-readable), `json` (machine-parsable). JSON output includes the full `SyncOutput` struct. |
| `--timeout <MS>` | `u64` | `30000` | Maximum pipeline execution time in milliseconds. Exits with code 6 if exceeded. Useful for guarding CI time budgets. |
| `--stage <STAGE>` | `String` | All stages | Execute only one μ stage of the A2A pipeline. Accepted values: `μ₁`/`mu1`, `μ₂`/`mu2`, `μ₃`/`mu3`, `μ₄`/`mu4`, `μ₅`/`mu5`. |
| `--ontology <PATH>` | `String` | From manifest | Override the ontology source file. In low-level mode (with `--queries`), defaults to `./ontology.ttl` if not specified. |
| `--queries <DIR>` | `String` | — | **Low-level bypass mode.** Directory of `.rq` SPARQL files to run directly against the ontology. When set, `ggen.toml` is ignored entirely. |
| `--language <LANG>` | `String` | `auto` | Target code generation language for low-level mode. Options: `rust`, `go`, `elixir`, `typescript`, `python`, `auto`. Only effective when `--queries` is supplied. |
| `--profile <NAME>` | `String` | — | Apply a named enforcement profile (e.g. `enterprise-strict`, `permissive`). Profiles configure which soundness checks are mandatory. Validated before the pipeline runs. |
| `--locked` | `bool` | `false` | Require an exact match against the `.ggen/packs.lock` file. Refuses to run if any installed pack has drifted from the locked versions. |

---

## 4. Pipeline Architecture

### 4.1 Manifest-driven pipeline (default)

When invoked without `--queries`, ggen reads `ggen.toml` and runs the full five-stage pipeline:

```
ggen.toml
    │
    ▼  Phase 1 — Discovery & Config
    │  Load manifest, resolve paths, read .ggen/packs.lock
    │
    ▼  Phase 2 — Ontology Load
    │  Parse RDF/TTL source + ontology.imports
    │
    ▼  μ₁ CONSTRUCT
    │  Apply inference rules (SPARQL CONSTRUCT queries)
    │
    ▼  μ₂ SELECT
    │  Extract bindings per rule (SPARQL SELECT queries)
    │
    ▼  μ₃ Tera Rendering
    │  Apply templates, generate source files
    │
    ▼  μ₄ Canonicalize
    │  rustfmt, import sort, compilation check
    │
    ▼  μ₅ Receipt Emission
    │  BLAKE3/SHA256 hashes, Ed25519 signature → .ggen/receipts/
    │
    ▼  SyncOutput + receipt_path
```

#### Per-stage progress output (with `--verbose`)

```text
[μ₁/5] CONSTRUCT: Normalizing ontology...
       Loaded 847 triples from a2a-ontology.ttl
       +124 triples from construct-agents.rq
[μ₂/5] SELECT: Extracting bindings...
       Agents: 8 bindings
       Messages: 12 bindings
       Tasks: 15 bindings
       Transports: 3 bindings
       Skills: 24 bindings
[μ₃/5] Tera: Generating code...
       agent.rs       (2.4 KB)
       message.rs     (3.1 KB)
       task.rs        (2.8 KB)
       transport.rs   (1.2 KB)
       skill.rs       (4.5 KB)
       lib.rs         (1.8 KB)
[μ₄/5] Canonicalizing: Formatting code...
       Running rustfmt...
       Verifying compilation...
[μ₅/5] Receipt: Generating verification...
       Receipt: .ggen/receipts/sync-20250208-143022.json
       Ontology hash: a3f2e1b4...
       Total: 6 files, 15.8 KB, 2.34s
```

### 4.2 Internal flag grouping

Flags are grouped into two sub-structs inside `SyncOptions`:

```
SyncFlags
├── ModeFlags
│   ├── validate_only   (--validate-only)
│   ├── dry_run         (--dry-run)
│   └── watch           (--watch)
└── BehaviorFlags
    ├── verbose         (--verbose)
    ├── force           (--force)
    └── audit           (--audit)
```

---

## 5. The `--queries` Low-Level Bypass Mode

When `--queries <DIR>` is supplied, the manifest is bypassed entirely. This mode runs the μ₁–μ₅ pipeline directly against raw SPARQL `.rq` files and an explicit ontology, without any `ggen.toml` configuration layer.

### Architecture

```
--ontology <PATH>    (default: ./ontology.ttl)
--queries <DIR>      directory of .rq SPARQL files
--output-dir <PATH>  (default: .)
--language <LANG>    target language (default: auto)
        │
        ▼
   ggen_core::sync::sync(SyncConfig { ... })
        │
   μ₁  Load ontology triples from --ontology
   μ₂  Run every .rq file in --queries dir
   μ₃  Render output using built-in templates for --language
   μ₄  Validate soundness of generated code
   μ₅  Emit receipt (unless --dry-run)
        │
   SyncOutput (soundness_violations → error field)
```

### Example usage

```bash
# Generate Go bindings from an ontology without a ggen.toml
ggen sync \
  --ontology ./businessos.ttl \
  --queries  ./queries/businessos/ \
  --output-dir ./generated/go/ \
  --language go

# Preview TypeScript bindings without writing files
ggen sync \
  --ontology ./model.ttl \
  --queries  ./queries/ \
  --language typescript \
  --dry-run
```

### Differences from manifest mode

| Aspect | Manifest mode | Low-level mode |
|---|---|---|
| Config source | `ggen.toml` | CLI flags only |
| Inference rules | From manifest `[inference.rules]` | `.rq` files directly |
| Template source | Manifest `[generation.rules.*.template]` | Built-in language templates |
| Receipt | Always emitted (unless `--dry-run`) | Always emitted (unless `--dry-run`) |
| `--rule` flag | Supported | Ignored |
| `--stage` flag | Supported | Ignored |
| `--audit` flag | Supported | Not supported |
| `--force` flag | Supported | Not supported |
| `--language` flag | Ignored | Required (or `auto`) |

---

## 6. Multi-Language Code Generation

In `--queries` (low-level) mode, `--language` controls which built-in template set is used:

| Value | Language | Notes |
|---|---|---|
| `rust` | Rust | Generates `struct`/`trait`/`impl` blocks; runs `rustfmt` in μ₄ |
| `go` | Go | Generates `type`/`interface`/`func` declarations |
| `elixir` | Elixir | Generates `defmodule`/`defstruct` patterns |
| `typescript` | TypeScript | Generates `interface`/`type`/`class` declarations |
| `python` | Python | Generates `@dataclass`/`Protocol` definitions |
| `auto` (default) | Inferred | Inspects `--output-dir` for go.mod, Cargo.toml, mix.exs, package.json, etc. Falls back to `rust` if ambiguous. |

Soundness violations found during μ₄ are collected and returned as the `error` field in the `SyncOutput` JSON.

---

## 7. Enforcement Profiles and Locked Mode

### 7.1 `--profile <NAME>`

Profiles configure the strictness of soundness checks across all pipeline stages. They are validated before any pipeline work begins.

```bash
ggen sync --profile enterprise-strict
ggen sync --profile permissive
```

**Common profile effects:**

| Check | `permissive` | `enterprise-strict` |
|---|---|---|
| Broken `api://` links | Warning | Hard failure |
| Missing SHACL constraints | Warning | Hard failure |
| Unsigned receipt | Allowed | Hard failure |
| `--force` without `--audit` | Allowed | Blocked |
| Draft receipts in CI | Allowed | Blocked |

### 7.2 `--locked`

Requires the installed pack versions to exactly match `.ggen/packs.lock`. Refuses to run if any discrepancy is detected.

```bash
# CI environment: strict lockfile enforcement
ggen sync --locked

# Typical error when drift is detected:
# ERROR: Pack 'star-toml@1.2.0' is installed but lockfile expects '1.1.3'.
#        Run `ggen pack update` then commit .ggen/packs.lock to resolve.
```

Both `--profile` and `--locked` are checked first, before any pipeline work. Failure exits with code 1 immediately.

---

## 8. Cryptographic Receipts

Every non-dry-run invocation of `ggen sync` automatically emits a cryptographic receipt. The receipt proves **what** was generated, **from what inputs**, and **by which version** of ggen.

### 8.1 Receipt files

| Path | Description |
|---|---|
| `.ggen/receipts/sync-<YYYYMMDD-HHMMSS>.json` | Timestamped archive copy (immutable) |
| `.ggen/receipts/latest.json` | Always points to the most recent receipt |
| `.ggen/keys/signing.key` | Ed25519 private key (hex, generated once, never overwritten) |
| `.ggen/keys/verifying.key` | Corresponding Ed25519 public key (hex) |

### 8.2 Input closure hashed into every receipt

The receipt hashes the **full O* closure** — every input that can change the output:

1. `actuator:ggen-sync@<version>` — which μ (and version) produced this
2. `ggen.toml:<sha256>` — the manifest content
3. `<ontology_path>:<sha256>` — ontology source file
4. `<import_path>:<sha256>` — each ontology import
5. `<query_file>:<sha256>` — any external `.rq` query files
6. `<template_file>:<sha256>` — any external Tera template files
7. `pack:<id>@<version>` — each installed pack from `.ggen/packs.lock`

If a closure input cannot be read, it is recorded as `<path>:MISSING` — never silently dropped.

### 8.3 Receipt chaining

Each receipt records the hash of the **previous** receipt, creating a tamper-evident chain. Deleting or modifying any prior receipt breaks verification of all subsequent receipts.

### 8.4 Dry-run exclusion

> ⚠️ `--dry-run` never emits a receipt. A receipt records consequences that happened. A preview writes nothing, so recording it would be a false witness.

### 8.5 Receipt JSON structure (abbreviated)

```json
{
  "operation_id": "550e8400-e29b-41d4-a716-446655440000",
  "timestamp": "2026-07-03T17:06:00Z",
  "input_hashes": [
    "actuator:ggen-sync@0.26.5",
    "ggen.toml:a3f2e1b4...",
    "ontology.ttl:9c2d8f01...",
    "pack:star-toml@1.2.0"
  ],
  "output_hashes": [
    "crates/a2a-generated/src/agent.rs:7f4a2c91...",
    "crates/a2a-generated/src/lib.rs:b3e1d840..."
  ],
  "previous_receipt_hash": "f0c4a3e2...",
  "signature": "<ed25519-hex>"
}
```

---

## 9. Exit Codes

| Code | Meaning | Common Cause |
|---|---|---|
| `0` | **Success** | All pipeline stages completed; all generated files written. |
| `1` | **Manifest validation error** | `ggen.toml` not found, malformed TOML, missing required fields, or `--profile`/`--locked` precondition failure. |
| `2` | **Ontology load error** | Ontology file not found, invalid RDF/TTL syntax, or import resolution failure. |
| `3` | **SPARQL query error** | Malformed `.rq` file, invalid query syntax, or no bindings returned by a required SELECT. |
| `4` | **Template rendering error** | Tera template syntax error, undefined variable, or type mismatch between template and SPARQL bindings. |
| `5` | **File I/O error** | No write permission to the output directory, disk full, or output path is outside the workspace root. |
| `6` | **Timeout exceeded** | Total pipeline duration surpassed `--timeout` milliseconds (default: 30,000 ms). |

---

## 10. Flag Precedence and Interaction Rules

| Rule | Effect |
|---|---|
| `--validate-only` overrides `--force` | Generation is skipped; `--force` has no effect. |
| `--dry-run` suppresses receipt | No files are written, so no receipt is emitted. |
| `--dry-run` neutralizes `--force` | Even with `--force`, nothing is written. |
| `--watch` wraps the full pipeline | The pipeline re-runs on every detected file change. All other flags apply to each iteration. |
| `--stage` restricts to one μ step | Only the named stage runs; preceding stages are assumed complete. |
| `--queries` bypasses manifest | When `--queries` is supplied, `--manifest`, `--rule`, `--stage`, `--audit`, and `--force` are all ignored. |
| `--language` is only valid with `--queries` | In manifest mode, the language is determined by the manifest template content. |
| `--profile` + `--locked` are always checked first | Both preconditions are validated before any pipeline work begins. Failure exits with code 1 immediately. |
| `--format json` affects stdout only | Progress log lines go to stderr regardless of format. |

### Valid flag combinations by workflow

| Workflow | Recommended flags |
|---|---|
| Daily development | `ggen sync` |
| Preview before commit | `ggen sync --dry-run` |
| Destructive overwrite | `ggen sync --force --audit` |
| Focused iteration | `ggen sync --rule <name> --dry-run` |
| Continuous development | `ggen sync --watch --verbose` |
| Pre-push validation | `ggen sync --validate-only` |
| CI/CD pipeline | `ggen sync --format json --locked` |
| Single pipeline stage | `ggen sync --stage μ₃` |
| Locked CI environment | `ggen sync --locked --format json --profile enterprise-strict` |
| Low-level bypass | `ggen sync --ontology X.ttl --queries ./q/ --language go` |

### Unsafe combinations to avoid

| Combination | Risk |
|---|---|
| `--force` without `--audit` | Destructive overwrite with no rollback trail |
| `--force` without prior `--dry-run` | Unexpected overwrites |
| `--stage μ₃` without μ₁–μ₂ already run | Stale bindings from a previous run may be used |
| `--watch` with `--force` in production | Continuous destructive overwrite on every file save |

---

## 11. Defensive Workflow Patterns

### Pattern 1: Safe destructive overwrite

```bash
# Step 1: Preview what will change
ggen sync --dry-run --verbose

# Step 2: Review the output, then overwrite with a full audit trail
ggen sync --force --audit
```

### Pattern 2: Continuous integration

```bash
# Validate integrity without generating
ggen sync --validate-only --locked --profile enterprise-strict

# If validation passes, run generation with JSON output for log parsing
ggen sync --locked --format json --profile enterprise-strict
```

### Pattern 3: Debugging a generation failure

```bash
# Run verbose to see exact triple counts and binding cardinalities
ggen sync --verbose --dry-run

# Isolate a single rule
ggen sync --rule my_rule --verbose

# Run only the SPARQL extraction stage to inspect bindings
ggen sync --stage μ₂ --verbose
```

### Pattern 4: Iterative template development

```bash
# Watch mode: re-renders every time a template or ontology file changes
ggen sync --watch --rule api_endpoints --verbose
```

### Pattern 5: Cross-language prototyping

```bash
# Prototype Go code from an ontology without touching ggen.toml
ggen sync \
  --ontology model.ttl \
  --queries ./queries/ \
  --language go \
  --output-dir ./proto/go \
  --dry-run

# Confirm, then generate for real
ggen sync \
  --ontology model.ttl \
  --queries ./queries/ \
  --language go \
  --output-dir ./proto/go
```

---

## 12. Output Structure

The `SyncOutput` struct is serialized when `--format json` is used:

```json
{
  "status": "success",
  "files_synced": 6,
  "duration_ms": 2340,
  "files": [
    {
      "path": "crates/a2a-generated/src/agent.rs",
      "size_bytes": 2457,
      "action": "created",
      "rule": "agent-template"
    }
  ],
  "inference_rules_executed": 3,
  "generation_rules_executed": 6,
  "audit_trail": ".ggen/audit/20260703-170618/",
  "receipt_path": ".ggen/receipts/latest.json",
  "gates": []
}
```

| Field | Description |
|---|---|
| `status` | `"success"` or `"error"` |
| `files_synced` | Count of files created or updated |
| `duration_ms` | Total wall-clock time for the pipeline |
| `files` | Per-file details: path, size, action (`created`/`updated`/`unchanged`), producing rule |
| `inference_rules_executed` | Number of CONSTRUCT/inference rules applied in μ₁ |
| `generation_rules_executed` | Number of Tera templates rendered in μ₃ |
| `audit_trail` | Path to the audit directory (only when `--audit` was set) |
| `error` | Error message or soundness violations summary (null on clean success) |
| `recovery` | AGI-parsable remediation steps (null on clean success) |
| `andon_signal` | TPS Andon signal JSON if a quality gate tripped (null normally) |
| `receipt_path` | Path to `.ggen/receipts/latest.json` (null on `--dry-run`) |
| `gates` | Array of proof gate results (populated by specific profiles) |

---

## 13. Related Documentation

| Topic | Path |
|---|---|
| Audit trail format and recovery | `docs/features/audit-trail.md` |
| Safe use of `--force` | `docs/features/force-flag.md` |
| Merge-mode (hybrid manual/generated code) | `docs/features/merge-mode.md` |
| Watch mode internals | `docs/features/watch-mode.md` |
| SPARQL ASK conditional execution | `docs/features/conditional-execution.md` |
| SHACL/SPARQL validation constraints | `docs/features/validation.md` |
| A2A μ₁–μ₅ pipeline deep dive | `docs/features/a2a-pipeline.md` |
| ggen.toml manifest schema | `docs/ggen-toml-schema.toml` |
| Receipt verification constitution | `AGENTS.md` |
| First-principles doc system architecture | `docs/rust_swarm_doc_plan/RFC.md` |
