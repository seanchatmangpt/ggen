# Cross-Repo Dependency Risks

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 0, alongside
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md).

## File reference table

| Path | LOC | Notes |
|---|---:|---|
| `/Users/sac/praxis/crates/praxis-graphlaw/src/lib.rs` | 676 | `pub mod chatman;` at line 6, unconditional |
| `/Users/sac/praxis/crates/praxis-graphlaw/src/chatman/` (18 files) | 11,097 total | see per-file table below |
| `/Users/sac/praxis/crates/ggen/src/graph.rs` | 1,016 | its 8 `praxis_graphlaw::` call sites, confined to `TripleStore`/`hooks`/`parser` |
| `/Users/sac/bcinr/crates/` | — | contains `bcinr-api`, `bcinr-logic`, `bcinr-mcp`, `bcinr-mfw-ir`, `bcinr-pddl`, `bcinr-pddl-lsp`, `bcinr-powl`, `bcinr-powl-receipt` |
| `/Users/sac/wasm4pm/crates/wasm4pm-cognition/` | — | confirmed exists (`src/`, `tests/`, `examples/`, `benches/`, `Cargo.toml`) |
| `/Users/sac/wasm4pm-compat/` | — | `Cargo.toml:3` — `version = "26.6.29"` |
| `/Users/sac/ggen/crates/ggen-lsp/Cargo.toml` | — | `wasm4pm-compat = "=26.6.29"` (already fixed, see below) |
| `/Users/sac/ggen/Cargo.toml` | — | `license = "MIT"` (workspace default) |
| `/Users/sac/praxis/crates/ggen/Cargo.toml` | 122 | `license = "MIT OR Apache-2.0"` |
| `/Users/sac/praxis/crates/praxis-core/Cargo.toml` | — | `license = "MIT OR Apache-2.0"` |
| `/Users/sac/praxis/crates/praxis-graphlaw/Cargo.toml` | — | `license = "MIT"` |

## 1. RDF/SPARQL engine proliferation

This repo already has **three independent oxigraph-based RDF stacks**
(`ggen-core::rdf`, `ggen-graph`, `ggen-marketplace`). Adopting `praxis-graphlaw` adds a
**fourth, built on an entirely different library family** (`oxrdf`/`spargebra` + a
hand-rolled `pest`-based parser, not oxigraph). Full design and recommendation split out to
[03-RDF-ENGINE-BRIDGE-DESIGN](03-RDF-ENGINE-BRIDGE-DESIGN.md) since it's substantial enough
to need its own ticket; recorded here only as the risk-inventory entry.

## 2. Process Intelligence Boundary tension

`praxis-graphlaw::chatman` is a real, **unconditional module**
(`/Users/sac/praxis/crates/praxis-graphlaw/src/lib.rs:6`, `pub mod chatman;`, not
feature-gated). The module directory
(`/Users/sac/praxis/crates/praxis-graphlaw/src/chatman/`) is much larger than a single
`engine.rs` file — 18 files, 11,097 lines total:

| File | LOC |
|---|---:|
| `engine.rs` | 2,175 |
| `powl_projection.rs` | 1,546 |
| `router.rs` | 898 |
| `router_test.rs` | 732 |
| `closure_test.rs` | 666 |
| `closure.rs` | 573 |
| `engine_test.rs` | 946 |
| `abi.rs` | 686 |
| `admission8.rs` | 482 |
| `triple8.rs` | 482 |
| `compensation.rs` | 562 |
| `compensation_test.rs` | 416 |
| `bridge.rs` | 414 |
| `bridge_test.rs` | 224 |
| `engine_cognition_test.rs` | 155 |
| `abi_test.rs` | 43 |
| `quarantine.rs` | 69 |
| `mod.rs` | 28 |

`/Users/sac/praxis/crates/praxis-graphlaw/src/chatman/engine.rs:61-65` imports
`bcinr_powl`/`bcinr_powl_receipt` conformance/fitness/causal-replay machinery directly —
exactly the kind of analysis this repo's Process Intelligence Boundary forbids inside ggen:
```
61: use bcinr_powl::ocel::{validate_against_tape, ConformanceResult, OcelLog as PowlOcelLog};
62: use bcinr_powl::tape::{OpKind, PowlTape};
63: use bcinr_powl_receipt::causal_receipt::{OcelCausalFrame, OcelCausalReceipt, PackedObjRef};
64: use bcinr_powl_receipt::denial::DenialPolarity;
65: use bcinr_powl_receipt::replay::{PowlReplayFrame, PowlReplayVerifier};
```

`/Users/sac/praxis/crates/ggen/src/graph.rs`'s own **8** `praxis_graphlaw::` call sites
(corrected count) touch only `TripleStore`, `parser::Syntax`, `hooks::{EffectKind,
HookVerdict}` — never `chatman`. The boundary holds only because ggen's own code never calls
into `chatman`; nothing in Cargo enforces that today.

New file: `/Users/sac/ggen/scripts/ci/guard-process-intelligence-boundary.sh`

```bash
#!/usr/bin/env bash
# scripts/ci/guard-process-intelligence-boundary.sh
set -euo pipefail

if grep -rn "praxis_graphlaw::chatman" --include="*.rs" crates/ src/ 2>/dev/null; then
  echo "FAIL: praxis_graphlaw::chatman referenced above -- ggen must not cross the Process Intelligence Boundary." >&2
  exit 1
fi
if grep -rEn '\buse[[:space:]]+bcinr_powl(_receipt)?\b|\bbcinr_powl(_receipt)?::' --include="*.rs" crates/ src/ 2>/dev/null; then
  echo "FAIL: direct bcinr_powl(_receipt):: reference found -- conformance/fitness analysis belongs in wasm4pm, never inline in ggen." >&2
  exit 1
fi
echo "OK: no praxis_graphlaw::chatman or bcinr_powl(_receipt) references in the ggen workspace."
```

**Action**: wire this guard into CI via `/Users/sac/ggen/justfile`, and get explicit
sign-off from whoever owns the CLAUDE.md boundary rule (see
[13-CLAUDE-MD-REFACTOR](13-CLAUDE-MD-REFACTOR.md)) that a merely-reachable-but-uninvoked
dependency is acceptable under it.

## 3. New sibling absolute-path dependencies

All confirmed present on disk this session: `/Users/sac/bcinr/crates/bcinr-pddl`,
`/Users/sac/bcinr/crates/bcinr-powl`, `/Users/sac/bcinr/crates/bcinr-powl-receipt`
(**mandatory**, non-optional deps of both `praxis-core` and `praxis-graphlaw`), and
`/Users/sac/wasm4pm/crates/wasm4pm-cognition/` (optional, gated behind praxis-graphlaw's
`cognition` feature, off by default).

Aside, not a blocker: `/Users/sac/ggen/Cargo.toml:101` already declares an unused, unrelated
third-party crate also named `bcinr` — "BranchlessCInRust" — in `[workspace.dependencies]`;
it shares only a name prefix with Sean's own `bcinr-*` crates and is not the same project
(confirmed zero usages via `grep -rln "bcinr::" /Users/sac/ggen/crates`). Worth a comment
when the real path deps are added, to avoid reviewer confusion, but not a real collision
since the hyphenated names differ.

### 3a. `wasm4pm-compat` version conflict — RESOLVED

Originally: `/Users/sac/ggen/crates/ggen-lsp/Cargo.toml` depended on `wasm4pm-compat` as a
**crates.io registry** dependency exact-pinned `=26.6.26`, while `praxis-core`/
`praxis-graphlaw` depend on it as a **path dependency** to `/Users/sac/wasm4pm-compat` at
`version = "26.6.29"` — two different resolution mechanisms *and* a newer pinned version
claiming the same package name. Cargo would not have silently unified these once
`praxis-core`/`praxis-graphlaw` joined this workspace.

**Status: fixed.** `/Users/sac/ggen/crates/ggen-lsp/Cargo.toml`'s pin was bumped from
`=26.6.26` to `=26.6.29` (confirmed published on crates.io — `/Users/sac/wasm4pm-compat/Cargo.toml:3`
shows the same version locally; confirmed clean `cargo update --dry-run` with no other
package movement; confirmed `just check` passes clean across the whole workspace after the
bump — see the comment left in place at that Cargo.toml line for the full rationale,
including why the pin is exact rather than a range: 26.6.23 previously shipped a real
regression under this project's pinned nightly toolchain).

## 4. License inconsistency

`/Users/sac/ggen/Cargo.toml` (workspace default) `license = "MIT"` vs.
`/Users/sac/praxis/crates/ggen/Cargo.toml` and `/Users/sac/praxis/crates/praxis-core/Cargo.toml`
`license = "MIT OR Apache-2.0"` vs. `/Users/sac/praxis/crates/praxis-graphlaw/Cargo.toml`
`license = "MIT"` — and no `LICENSE-APACHE` file anywhere in `/Users/sac/praxis/` to back the
dual claim (confirmed: only scaffold-template output and vendored third-party deps match
that filename pattern, none of it praxis's own license text).

**Recommendation: drop the `OR Apache-2.0` clause from
`/Users/sac/praxis/crates/ggen/Cargo.toml` and
`/Users/sac/praxis/crates/praxis-core/Cargo.toml` (`license = "MIT"` only) before depending
on them.** The Apache-2.0 offer is already unfulfillable as written (no backing text exists
in that repo), so removing it fixes a pre-existing compliance gap rather than requiring
someone to draft Apache-2.0 text (with its patent-grant implications) for an option nobody
intends to actually offer; it also makes the whole adopted subtree license-uniform with
itself and with ggen root, which has been MIT-only since inception.

## Definition of done for this ticket

- CI guard from item 2 wired in and passing.
- Explicit sign-off recorded on the Process Intelligence Boundary question.
- License clauses reconciled in `/Users/sac/praxis/crates/ggen/Cargo.toml` and
  `/Users/sac/praxis/crates/praxis-core/Cargo.toml`.
- ~~`wasm4pm-compat` version conflict~~ — already resolved.
