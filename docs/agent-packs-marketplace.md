# Agent-Facing Packs + Marketplace (AGI surface)

This document describes the agent integration surface for ggen's package system —
the way an autonomous agent (LLM tool-caller, A2A peer, or a direct Rust caller)
discovers, inspects, resolves, installs, verifies, and removes packs.

Before this surface existed, packs and the marketplace were reachable only by
shelling out to the `ggen pack …` CLI and parsing human-oriented strings. The
agent surface replaces string-scraping with a small, stable, `serde`-typed
contract that is **evidence-bearing** (every mutation returns the durable
artifacts it produced) and **fail-closed** (refusals are typed errors, never a
fake success).

## Layers

| Layer | Crate / module | Purpose |
|-------|----------------|---------|
| Facade (library API) | `ggen_core::agent` (`PackAgent`) | The single authoritative agent entry point. Wraps the existing install / lockfile / receipt pipeline; returns structured [`agent::types`] outcomes. |
| MCP tools | `ggen_a2a_mcp::mcp_server` (`GgenMcpServer`) | `ggen.packs.*` tools registered on the MCP server, alongside `ggen.construct`. |
| A2A adapter | `ggen_a2a_mcp::mcp_packs::PackToolsAdapter` | Exposes the same operations over the A2A `Adapter` trait + an agent card. |
| CLI noun | `ggen_cli::cmds::agent` (`ggen agent <verb>`) | The same operations as `ggen` subcommands, each emitting structured JSON (`--format json`) an agent can parse and chain. |

The MCP tools, the A2A adapter, and the `ggen agent` CLI verbs all call the
**same** `PackAgent`. There is exactly one implementation of each operation, so
the four surfaces (library, MCP, A2A, CLI) cannot drift. An autonomous agent can
drive the full project-bring-up lifecycle from the CLI alone:

```
ggen agent capabilities → search → compatibility → install → status → verify → remove
```

```
agent (MCP tool call)         agent (A2A task)            Rust caller
        │                            │                          │
        ▼                            ▼                          │
GgenMcpServer::packs_*      PackToolsAdapter::from_a2a          │
        │                            │                          │
        └──────► mcp_packs::*_result ◄──────┐                   │
                         │                  │                   │
                         ▼                  └───────────────────┘
                 ggen_core::agent::PackAgent
                         │
   install_pack ─ lockfile ─ emit_install_receipt (authoritative path)
```

## Operations (full lifecycle)

| Operation | MCP tool | Mutating | Result type |
|-----------|----------|----------|-------------|
| Discover capabilities | `ggen.packs.capabilities` | no | `Capabilities` |
| Search registry | `ggen.packs.search` | no | `{ query, total, results: [SearchHit] }` |
| List registry | `ggen.packs.list` | no | `{ total, packs: [PackRef] }` |
| Show one pack | `ggen.packs.show` | no | `PackDetail` |
| Resolve a capability surface | `ggen.packs.resolve` | no | `ResolveOutcome` |
| Check multi-pack compatibility | `ggen.packs.compatibility` | no | `CompatibilityOutcome` |
| Installed-pack status | `ggen.packs.status` | no | `AgentStatus` |
| Verify a receipt | `ggen.packs.verify` | no | `VerifyOutcome` |
| Install a pack | `ggen.packs.install` | **yes** | `InstallOutcome` |
| Remove a pack | `ggen.packs.remove` | **yes** | `RemoveOutcome` |

`capabilities()` is the discovery entry point: it returns the operation list
(each flagged `mutating` or not) plus the capability surfaces the agent can
resolve. An agent calls it first to learn the contract without out-of-band docs.

## Evidence-bearing outcomes

A real (non-dry-run) `install` returns an `InstallOutcome` that proves the
durable state transition happened:

- `digest` — non-empty SHA-256 pinned in the lockfile (empty only on `dry_run`);
- `lockfile_path` — the `.ggen/packs.lock` that was written;
- `receipt` — a `ReceiptRef` pointing at the signed provenance receipt, with
  `signature_present == true`.

An agent can then call `verify` on `receipt.receipt_path` to confirm the
signature against the project's key, and `status` to confirm the pack now
appears in the lockfile. This is the multi-surface corroboration exercised by
the `install_writes_lockfile_emits_signed_receipt_and_verifies` test.

## Fail-closed contract

Every refusal is a typed `ggen_core::agent::AgentError` (serialized as
`{ "kind": …, "detail": … }`), never a success with a warning:

| Condition | Result |
|-----------|--------|
| Pack absent from registry | `install`/`show` → `PackNotFound`; no lockfile written |
| Install pinned no digest | `emit_install_receipt` refuses; no receipt |
| Receipt signature empty / tampered / key missing | `verify` → `is_valid = false` with a reason |
| Remove a pack not in the lockfile | `NotInstalled`; lockfile untouched |
| Empty search query / invalid pack id | `InvalidRequest` |

## Authority consolidation (this surface deepens, it does not fork)

- The facade routes through the same `install_pack` + lockfile writers the CLI
  uses; it adds a surface over the authoritative path, not a parallel one.
- The pack-install **receipt** logic is now a single root-parameterized
  implementation in `ggen_core::agent::receipt::emit_install_receipt`. The CLI's
  `crates/ggen-cli/src/cmds/packs_receipt.rs` is a thin adapter that delegates
  to it, so the CLI and the agent surface can never emit divergent receipts.
- `domain::packs::load_pack` (used by `check_packs_compatibility`) now loads
  real registry metadata instead of fabricating a phantom `package-<id>` pack —
  a missing pack is a hard error.

## Tests

Three Chicago TDD suites, real collaborators only (real `TempDir` registries,
real `.ggen/packs.lock` files, real Ed25519-signed receipts — no mocks):

- `crates/ggen-core/tests/agent_facade_test.rs` — 24 tests over `PackAgent`. The
  full install→status→verify lifecycle, capability resolution, multi-pack
  compatibility, and sabotage paths (empty digest, tampered signature,
  malformed/missing-key receipt, absent-pack install/remove, missing-pack
  compatibility — a falsifiability guard on the `load_pack` de-mock).
- `crates/ggen-a2a-mcp/tests/pack_tools_test.rs` — 10 tests over the wire layer.
  A tool name + JSON arguments are dispatched (`dispatch_pack_tool`,
  `PackToolsAdapter::from_a2a`) through to the facade against a real filesystem
  and real receipts; asserts on the structured JSON result. Covers verify of a
  real receipt (and a tampered one through the wire), fail-closed unknown-tool /
  invalid-id handling, and agent-card ↔ `PACK_TOOLS` no-drift.
- `crates/ggen-cli/tests/agent_lifecycle_test.rs` — 4 tests over the CLI noun,
  driving the REAL `ggen` binary. The headline test plays an AGI completing a
  project end-to-end (`capabilities → search → compatibility → install → status
  → verify → remove`), asserting on each verb's JSON AND the durable state
  (lockfile digest, signed receipt that verifies). Sabotage: nonexistent-pack
  install fails closed (no lockfile), a tampered receipt fails verification, and
  `--dry_run` writes no durable state.

