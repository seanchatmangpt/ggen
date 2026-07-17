# Open Questions for Follow-Up

Part of [00-OVERVIEW](00-OVERVIEW.md) — not blocking, but should be answered before or during
the phases they affect.

## File reference table

| Path | LOC | Status |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-core/src/domain/` | 22,499 | fully accounted for — see item 2 |
| `/Users/sac/ggen/crates/ggen-core/src/utils/` | 11,008 | **not** fully accounted for — see item 2 |
| `/Users/sac/ggen/crates/ggen-core/src/ontology_core/` | 1,778 | not examined in this pass |
| `/Users/sac/ggen/crates/ggen-core/src/canonical/` | 722 | not examined in this pass |
| `/Users/sac/ggen/crates/ggen-core/src/transport/` | 1,245 | not examined in this pass |
| `/Users/sac/ggen/crates/ggen-core/src/utils/secrets.rs` | 1,359 | commented out of module tree at `utils/mod.rs:59` — not compiled |
| `/Users/sac/ggen/crates/ggen-core/src/utils/supply_chain.rs` | 367 | commented out of module tree at `utils/mod.rs:60` — not compiled |
| `/Users/sac/ggen/crates/ggen-core/src/utils/safe_path.rs` | 838 | `SafePath` def #1, struct at line 40 |
| `/Users/sac/ggen/crates/ggen-core/src/utils/path_validator.rs` | 726 | `SafePath` def #2 — **independent duplicate**, struct at line 133 |
| `/Users/sac/ggen/crates/ggen-core/src/utils/safe_command.rs` | 1,086 | `SafeCommand` def #1, struct at line 317 |
| `/Users/sac/ggen/crates/ggen-core/src/security/command.rs` | 326 | `SafeCommand` def #2 — **independent duplicate**, struct at line 66 |

## 1. Full RDF-engine consolidation vs. bridge-pattern

**Recommended default: bridge-pattern first**, scoped to this ticket set (retire `ggen-core`
only — see [03-RDF-ENGINE-BRIDGE-DESIGN](03-RDF-ENGINE-BRIDGE-DESIGN.md); leave `ggen-graph`
and full RDF-engine consolidation for a separate ticket).
`/Users/sac/praxis/crates/praxis-graphlaw/` is already ~20x larger than the three
RDF-adjacent absorbed modules this migration is actually about (`ontology_core` 1,778 +
`canonical` 722 + `transport` 1,245 = 3,745 lines — see `00-OVERVIEW`'s absorbed-module
table). Folding everything into `praxis-core`/`praxis-graphlaw` in the same pass as deleting
`ggen-core` multiplies the surface under change at once — the opposite of this repo's
fix-forward, one-authoritative-path-at-a-time posture.

**What would change this**: a parity spike showing `praxis-graphlaw`'s SPARQL/SHACL/OWL-RL
modules are a strict superset of `ggen-graph`'s Oxigraph wrapper
(`/Users/sac/ggen/crates/ggen-graph/`) with zero gaps, and that `ggen-graph`'s own consumers
can be repointed mechanically — not performed in this research pass.

## 2. Whether ggen-core's absorbed-crate content has anything load-bearing beyond what's found

Fresh count (all confirmed exact via `find <dir> -name '*.rs' | xargs wc -l | tail -1`):
`domain` 22,499 + `utils` 11,008 + `ontology_core` 1,778 + `canonical` 722 + `transport`
1,245 = **37,252 lines**.

**Recommended default: `domain` is fully accounted for** — `ggen_core::domain::` appears
exactly 19 times in `ggen-cli/src`, all inside the already-cataloged command-wiring files
(see [08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md)), and zero times in `ggen-lsp/src`.

**`utils` is not fully accounted for and needs a scoped task of its own**:
`ggen_core::utils::error::{Error, Result}` is ggen-cli's crate-wide error type, referenced
87 times including ggen-cli's own public API contract
(`/Users/sac/ggen/crates/ggen-cli/src/lib.rs`'s `pub use`/`cli_match()` signature) —
praxis's `/Users/sac/praxis/crates/ggen/src/error.rs` has no drop-in equivalent (`AppError`,
a differently-shaped `thiserror` enum).

Three findings to carry forward rather than re-discover later:

- `/Users/sac/ggen/crates/ggen-core/src/utils/secrets.rs` (1,359 lines) and
  `/Users/sac/ggen/crates/ggen-core/src/utils/supply_chain.rs` (367 lines) are commented out
  of the module tree already
  (`/Users/sac/ggen/crates/ggen-core/src/utils/mod.rs:59-60`: `// pub mod secrets;` /
  `// pub mod supply_chain;`) — not compiled, provably not load-bearing regardless of
  migration outcome.

- **`SafePath` and `SafeCommand` each have TWO independent, unrelated definitions** — this
  is a real correction, not just a path-precision fix. An earlier draft of this ticket
  implied one canonical definition per symbol; verification this session found:

  | Symbol | Definition | Path | LOC | Struct at line |
  |---|---|---|---:|---|
  | `SafePath` | #1 | `/Users/sac/ggen/crates/ggen-core/src/utils/safe_path.rs` | 838 | 40 |
  | `SafePath` | #2 (independent, not a re-export) | `/Users/sac/ggen/crates/ggen-core/src/utils/path_validator.rs` | 726 | 133 |
  | `SafeCommand` | #1 | `/Users/sac/ggen/crates/ggen-core/src/utils/safe_command.rs` | 1,086 | 317 |
  | `SafeCommand` | #2 (independent, not a re-export) | `/Users/sac/ggen/crates/ggen-core/src/security/command.rs` | 326 | 66 |

  Both symbols have zero external call sites in `ggen-cli`/`ggen-lsp` (confirmed by prior
  grep) — possibly self-consumed inside `ggen-core` only, or genuinely workspace-wide dead
  code — but a migration agent must know **which of each pair** (if either) is actually
  called internally before deciding what to port, or risk porting/deleting the wrong one.
  Not resolved in this pass.

**What would change this recommendation**: an `LSP findReferences` sweep (per this repo's
own LSP-first navigation rule) on `ggen_core::utils::error::Error`/`Result`, and on all four
`SafePath`/`SafeCommand` definitions above, confirming (a) no consumers exist in
`ggen-marketplace`/`cpmp`/`genesis-core-v2` beyond what's already found, and (b) which
`SafePath`/`SafeCommand` definition (if either) is actually called from within `ggen-core`
itself, versus genuinely orphaned in both copies.
