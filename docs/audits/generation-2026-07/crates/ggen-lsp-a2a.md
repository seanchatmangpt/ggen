# ggen-lsp-a2a — audit (generation-first, 2026-07)

**Purpose:** "A2A bridge exposing the ggen-lsp-mcp route engine as an A2A agent (leaf crate — cycle-free)." (`crates/ggen-lsp-a2a/Cargo.toml`).

## LOC

`tokei crates/ggen-lsp-a2a/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| src/lib.rs | 115 |
| **Total** | **115** |

## Consolidation status

`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §2 (line 29) + §3 (line 37): CONSOLIDATE into `ggen-lsp` behind an `a2a` feature. Consolidation ≠ dead — the adapter surface survives the merge, so it is classified for generatability, not DEAD-DELETE. (Dependent-grep note: `grep -rln 'ggen-lsp-a2a\|ggen_lsp_a2a' crates/*/Cargo.toml Cargo.toml crates/*/src src` finds only the workspace members list and doc-comment mentions in `crates/ggen-cli/src/cmds/lsp.rs` — zero code dependents, matching the analysis's "0 incoming" finding. If a later pass decides this bridge is removed rather than merged, reclassify to DEAD-DELETE; this audit follows the consolidation verdict.)

## Class breakdown

| Class | LOC (est.) | Basis |
|---|---|---|
| GENERATABLE-WITH-SPEC | ~105 (~90%) | whole-file read: `TOOLS` const, 3-arm `dispatch_tool` match delegating to `ggen_lsp_mcp::{repair_route,replay_case,metrics}_result`, `agent_card()` static JSON, and the `Adapter` trait impl (name/version/init/can_handle/to_a2a/from_a2a/capabilities/shutdown) — mechanical adapter boilerplate fully determined by a 3-tool schema |
| IRREDUCIBLY-CUSTOM | ~10 | the field-evidence capture branch inside `from_a2a` (lines 121–138): conditional `ggen_lsp::capture_request` side effect at the actuation boundary — a policy decision, not schema-derivable |

## Matching template/spec

`templates/a2a-rust.tera` exists (plus a2a-{go,java,typescript,elixir}.tera) — the A2A adapter template family. No TTL tool-spec for the three `ggen.lsp.*` tools exists in `.specify/`, so the class is GENERATABLE-WITH-SPEC rather than GENERATABLE-NOW: authoring a `ggen-lsp-route-tools.ttl` (tool name, description, params) would make this crate and ggen-lsp-mcp's wiring emit from the same spec.
