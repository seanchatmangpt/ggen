# ggen-cli Migration

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 3, depends on tickets
[01](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md) through
[07](07-PROJECT-SCAFFOLDING-PORT.md) all landing first.

## File reference table (all 31 files, full paths, exact LOC)

All paths under `/Users/sac/ggen/crates/ggen-cli/src/` unless noted. Confirmed via
`grep -rln "ggen_core::" crates/ggen-cli/src` → exactly 31 files; `grep -rn "ggen_core::"
crates/ggen-cli/src | wc -l` → exactly 164 hits (both re-verified this session, unchanged
from prior research).

| File | LOC | Bucket(s) | Risk |
|---|---:|---|---|
| `config_clap/loader.rs` | 150 | config_lib | pure re-export shim |
| `config_clap/mod.rs` | 14 | config_lib | pure re-export shim |
| `lib.rs` | 289 | config_lib, telemetry, error plumbing | mixed |
| `cmds/receipt.rs` | 166 | receipt-core | pure re-export shim |
| `receipt_manager.rs` | 434 | receipt-core, error plumbing | mixed |
| `pack_install.rs` | 654 | marketplace | pure re-export shim |
| `error.rs` | 312 | error plumbing | native, thin |
| `runtime.rs` | 95 | error plumbing | native, thin |
| `cmds/lsp.rs` | 357 | error plumbing | native, thin |
| `cmds/mcp.rs` | 44 | error plumbing | native, thin |
| `cmds/graph.rs` | 227 | domain-handlers | native |
| `cmds/policy.rs` | 370 | marketplace + pack-lockfile | mixed |
| `conventions/planner.rs` | 608 | error plumbing | native, thin |
| `conventions/watcher.rs` | 272 | error plumbing | native, thin |
| `conventions/resolver.rs` | 609 | error plumbing | native, thin |
| `conventions/presets/mod.rs` | 89 | error plumbing | native, thin |
| `conventions/presets/clap_noun_verb.rs` | 87 | error plumbing | native, thin |
| `cmds/template.rs` | 202 | domain-handlers | native, single-symbol, low coupling |
| `cmds/doctor.rs` | 153 | domain-handlers | native, single-symbol, low coupling |
| `cmds/ontology.rs` | 443 | domain-handlers + ontology/validation | native, shares `domain::ontology` |
| `cmds/pack.rs` | 400 | domain-handlers + pack-lockfile | native, shares `domain::packs` |
| `cmds/packs.rs` | 175 | domain-handlers + pack-lockfile | native, shares `domain::packs` |
| `cmds/capability.rs` | 125 | domain-handlers + pack-lockfile | native, shares `domain::packs` |
| `cmds/init.rs` | 1,133 | ontology/validation + sync-pipelines + project scaffolding | native — see [07](07-PROJECT-SCAFFOLDING-PORT.md) |
| `cmds/agent.rs` | 155 | agent/pack-agent facade | native, also consumed by `ggen-lsp`'s `a2a_mcp/mcp_packs.rs` |
| `cmds/mod.rs` | 79 | agent/pack-agent facade | native |
| `cmds/packs_receipt.rs` | 32 | agent/pack-agent facade | native |
| `cmds/sigma.rs` | 49 | dflss | native, isolated single-file |
| `cmds/sync.rs` | 827 | **all three** ggen-core pipelines, `domain::sync_profile`, receipt | native, highest cross-cutting file in ggen-cli |
| `cmds/inverse_sync.rs` | 354 | sync-pipelines (4th mechanism) + native `receipt::provenance_envelope::CoherenceReport` | native |
| `cmds/wizard.rs` | 1,744 | sync-pipelines + project scaffolding | native — see [07](07-PROJECT-SCAFFOLDING-PORT.md) |

`/Users/sac/ggen/crates/ggen-cli/Cargo.toml:166-171`:
```
166: [dependencies.ggen-core]
167: workspace = true
168: features = ["otel"]
169:
170: [dependencies.ggen-graph]
171: workspace = true
```
Confirmed: no `ggen-config`/`ggen-marketplace` dependency lines exist yet — `ggen-cli`
depends directly only on `ggen-core` and `ggen-graph`.

## Bucket detail

`config_lib`, `marketplace` (policy/profile/metadata/models), and core
`receipt::{Receipt, generate_keypair, hash_data}` symbols are **pure re-export shims** —
`/Users/sac/ggen/crates/ggen-core/src/lib.rs` just does `pub use ggen_config::*`
(lines 250-251)/`pub use ggen_marketplace::{...}` (lines 253-255, a named-import list, not a
wildcard). Everything else (`agent`, `codegen`, `dflss`, `domain`, `manifest`, `ontology`,
`packs`, `reverse_sync`, `sync`, `telemetry`, `utils`, `validation`) is native to ggen-core
with **zero counterpart** anywhere in `/Users/sac/praxis/crates/{ggen,praxis-core,
praxis-graphlaw}` — confirmed via grep for `PackAgent`, `reverse_sync`, `dflss`,
`PackLockfile` returning nothing there.

## Recommended order

1. **`config_lib`** (`config_clap/loader.rs`, `config_clap/mod.rs`) — pure shim, proves the
   re-pointing mechanics work, zero fan-in risk.
2. **`receipt`-core types** (`cmds/receipt.rs`, `receipt_manager.rs` receipt part; excluding
   `provenance_envelope`/`chain_linking`, which bind to the sync-pipeline cluster and must
   move with step 11).
3. **`telemetry`** (`lib.rs` telemetry part) — narrow, unblocking.
4. **`utils::error`** (`error.rs`, `runtime.rs`, `receipt_manager.rs` error part,
   `cmds/{lsp,mcp,graph,policy}.rs`, `conventions/{planner,watcher,resolver,presets/*}.rs`)
   — appears in 11 of 31 files and gates everything downstream, must land before
   domain-handler work compiles end to end.
5. **`marketplace` shim** (`pack_install.rs`, `cmds/policy.rs` marketplace part), sequenced
   after error plumbing since `policy.rs` needs both.
6. **Domain command handlers** — split `cmds/template.rs`/`cmds/doctor.rs` first, then
   `cmds/graph.rs`/`cmds/ontology.rs` together, then
   `cmds/pack.rs`/`cmds/packs.rs`/`cmds/capability.rs` together.
7. **Ontology/validation** (`cmds/ontology.rs`, part of `cmds/init.rs`), alongside step 6's
   ontology sub-group.
8. **Pack lockfile** (`cmds/capability.rs`, `cmds/pack.rs`, `cmds/packs.rs`,
   `cmds/policy.rs`), naturally follows steps 5-6.
9. **Agent/pack-agent facade** (`cmds/agent.rs`, `cmds/mod.rs`, `cmds/packs_receipt.rs`,
   `cmds/packs.rs`), after pack lockfile, retires risk in both ggen-cli and ggen-lsp at once
   (see [09-GGEN-LSP-MIGRATION](09-GGEN-LSP-MIGRATION.md)).
10. **`dflss`** (`cmds/sigma.rs`) — isolated, lowest urgency.
11. **Sync/codegen pipelines last** (`cmds/sync.rs`, 827 lines; `cmds/inverse_sync.rs`, 354
    lines; `cmds/wizard.rs`, 1,744 lines; `cmds/init.rs`, 1,133 lines) — the most
    cross-cutting files (four pipeline mechanisms combined) and depend transitively on every
    prior bucket.

## Definition of done for this ticket

- All 164 references across 31 files re-pointed away from `ggen_core::`.
- `/Users/sac/ggen/crates/ggen-cli/Cargo.toml` gains direct dependencies on `ggen-config` and
  `ggen-marketplace` (alongside the existing lines 166-171).
- `ggen-cli`'s public API surface (`lib.rs`'s `pub use ggen_core::utils::error::Result` and
  `cli_match()`'s return type) updated to the new error type.
- `just check`/`just test`/`just lint` all pass with `ggen-core` still present but no longer
  referenced by `ggen-cli`.
