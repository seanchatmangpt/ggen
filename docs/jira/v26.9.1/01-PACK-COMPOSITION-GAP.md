# Pack-to-pack composition/dependency mechanism does not exist in ggen

## Status (updated after real implementation)

Still **MEASURE/DOCUMENT ONLY** — no change needed. Confirmed accurate: no code in
`~/ggen` or `~/ggen-marketplace` was touched by any downstream ticket's real
implementation work (GM-02, GM-03, GM-04, GI-01 through GI-05, all executed
2026-09-01), consistent with this document's own "no source file is modified by
this ticket" definition-of-done bullet. The bundle-manifest + installer workaround
this document recommends in place of fixing the pack-composition gap directly was
built for real and works: see
[02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY](02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY.md)
(DONE), [03-FORTUNE5-TESTING-BBLOCK-PORTABILITY](03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md)
(PARTIAL), and
[04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION](04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION.md)
(DONE) for the two-pack portability fixes this finding motivated, and the
`~/ggen_igniter` GI-01 through GI-05 story chain (`SchemaDispatch`, `GgenToml.IO`,
`Bundle.merge/2`, `SyncShellout`/`GateVerify`/`SyncVerify`, and the real
`mix ggen_igniter.fortune5_ready` task) for the installer this ticket's "Why not fix
this directly" section called for instead of opening `pack.toml`'s closed schema or
reviving `PackComposer`/`DependencyGraph`.

Companion tickets: [02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY](02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY.md),
[03-FORTUNE5-TESTING-BBLOCK-PORTABILITY](03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md),
[04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION](04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION.md),
[06-BEAM4PM-TRIAL-AND-GATE-M2-PREREQUISITE](06-BEAM4PM-TRIAL-AND-GATE-M2-PREREQUISITE.md).

## Scope: MEASURE/DOCUMENT ONLY

This ticket proposes no code change. Its sole purpose is to write down, with exact
file/line evidence, a real gap found during a real exploration session across
`~/ggen`, `~/ggen-marketplace`, `~/ggen_igniter`, and `~/beam4pm`: ggen has no
mechanism today by which one pack can declare or pull in another pack. Fix work
belongs to the bundle-manifest + `mix ggen_igniter.fortune5_ready` installer
described in this ticket's "Why not fix this directly" section and detailed in the
companion tickets above — this ticket exists so a future session does not have to
re-derive the finding from scratch.

## Finding 1: `pack.toml`'s schema is closed, with no dependency field

`crates/ggen-engine/src/pack.rs` defines the on-disk `pack.toml` schema:

```rust
/// On-disk `pack.toml` schema (closed key set, fail closed).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PackToml {
    pack: PackMeta,
}

/// `[pack]` table of `pack.toml` (closed key set).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PackMeta {
    name: String,
    version: String,
    description: String,
    #[serde(default)]
    deprecated: bool,
    #[serde(default)]
    superseded_by: Vec<String>,
}
```

(`crates/ggen-engine/src/pack.rs` lines 70–93, current `~/ggen` HEAD.) `#[serde(deny_unknown_fields)]`
on both `PackToml` and `PackMeta` means a `pack.toml` carrying any key ggen does not
already know about — including a hypothetical `[dependencies]` table — fails to
parse. There is no `dependencies`, `requires`, or `packs` field anywhere in this
schema. The only inter-pack relationship the schema can express at all is
one-directional supersession (`deprecated` / `superseded_by`), which names a
replacement pack, not a required co-installed one.

## Finding 2: a `PackComposer`/`DependencyGraph` subsystem exists but is dead code

`ggen-marketplace::packs_registry` contains real, non-trivial composition machinery:

- `crates/ggen-marketplace/src/packs_registry/composer.rs` — `PackComposer` (with
  `new`, `with_default_repo`), `CompositionOptions`, `CompositionResult`,
  `CompositionPlan`, `CompositionStep`.
- `crates/ggen-marketplace/src/packs_registry/dependency_graph.rs` — `DependencyGraph`
  with `add_node`, `add_edge`, `detect_cycles`, `topological_sort`, `dependencies`,
  `transitive_dependencies`, `node_count`, `edge_count` — a real, usable graph
  algorithm (cycle detection + topo sort), not a stub.

`DependencyGraph` is consumed by one real caller: `PackInstaller::new`/`with_default_repo`
in `crates/ggen-marketplace/src/packs_registry/installer.rs` (line 92:
`let graph = DependencyGraph::from_packs(&all_packs)?;`), and `PackComposer` itself
calls into `PackInstaller` (`composer.rs` line 7:
`use crate::packs_registry::installer::{InstallOptions, PackInstaller};`). So the
graph and composer are wired to each other inside `packs_registry` — the subsystem is
internally coherent, not literally unreachable code.

What it is NOT reachable from is any CLI verb. `grep -rln "PackComposer" crates
--include="*.rs"` over the live `~/ggen` tree returns exactly one file:
`crates/ggen-marketplace/src/packs_registry/composer.rs` itself — no caller anywhere
in `ggen-cli`. The two real pack-mutating CLI command modules confirm this
independently:

- `crates/ggen-cli/src/cmds/pack.rs`: `pub fn add(#[arg(index = 1)] pack_name:
  String, force: bool)` (line 126) and `pub fn remove(#[arg(index = 1)] pack_name:
  String)` (line 194) — both take one `String`, never a `Vec<String>` or a bundle
  manifest.
- `crates/ggen-cli/src/cmds/packs.rs`: `pub fn install(pack_id: String)` (line 72) —
  same shape.

Neither module imports `packs_registry::composer` or `packs_registry::installer`;
`pack.rs` and `packs.rs` import only `metadata`, `sparql_executor`, and `validate`
from `packs_registry`. `PackComposer`/`DependencyGraph`/`PackInstaller` are real,
compiling, non-trivial Rust that no `ggen` invocation can ever exercise — the
CLI has no verb that accepts more than one pack identifier per call, so there is
nothing for the composer to compose.

## Finding 3: zero packs in the corpus declare a dependency

`find ~/ggen-marketplace/packs -mindepth 2 -maxdepth 2 -name pack.toml | wc -l` →
**211** pack manifests in the corpus (the 150–207 range cited in prior planning
sessions is now 211 as of this check). A raw-text `grep -rln "\[dependencies\]"
~/ggen-marketplace/packs/*/pack.toml` returns 2 hits
(`packs/wasm4pm-pack/pack.toml`, `packs/shacl-projection-pack/pack.toml`) — both are
false positives from the string `[dependencies]` appearing inside a pack's own
`description` prose, not a real TOML table (which `PackMeta`'s
`deny_unknown_fields` would reject outright if present). Confirming this,
`shacl-projection-pack/pack.toml`'s own description states the limitation directly,
unprompted, as prior art for this exact finding:

> "REAL LIMITATION, stated precisely rather than overclaimed: ggen packs cannot
> declare pack-to-pack dependencies in pack.toml -- there is no `[dependencies]`
> table a pack can use to require another pack be present. ... this is the real,
> confirmed boundary of pack composition in this codebase, not a dependency
> mechanism."

Zero of the 211 packs use a real `[dependencies]` table, because the schema (Finding
1) makes doing so a parse error.

## Finding 4: the formal composition algebra is aspirational design work, not a built mechanism

`~/ggen-marketplace/docs/thesis/09-pack-algebra.md` exists and lays out a formal
algebra for pack composition. It is explicitly design/thesis material, not a
description of shipped behavior — none of its constructs correspond to a live code
path per Findings 1–2 above. Treat it as a candidate design input for a future
composition mechanism, not as evidence one exists today.

## What this means for "Fortune 5 ready in one command"

A consuming project (e.g. beam4pm) cannot express "give me `fortune5-architecture-pack`
and `fortune5-deployment-blocks-pack` together" as a single `pack.toml`-level
dependency declaration, nor as a single `ggen pack add` invocation — `add` accepts one
`pack_name: String`. Every multi-pack "bundle" today is enacted by hand-editing the
consumer's own `ggen.toml` `[packs]` table to list each pack name individually (the
mechanism `shacl-projection-pack`'s own description names as "explicit composition":
"a consumer's OWN ggen.toml lists BOTH packs ... and ggen-engine runs every composed
pack's gates/*.rq against the UNION of the merged graph"). That union-of-gates
behavior at sync time (`crates/ggen-engine/src/sync.rs`, "Pack-shipped SPARQL gates"
stage) is real and already gives composed packs' gates real teeth — what is missing
is only the requirement-declaration and one-command-install layer on top of it.

## Why not fix this directly

Two paths were considered and explicitly rejected for this release:

1. **Build the real ggen-core pack-dependency-resolution algebra** (open the closed
   `PackMeta` schema, add a `[dependencies]` table, implement resolution semantics
   matching or superseding `docs/thesis/09-pack-algebra.md`). Rejected as out of
   scope: this is a separate, large ggen-core language-design project (schema
   migration across all 211 existing packs, version-range resolution semantics,
   diamond-dependency and cycle handling beyond what `DependencyGraph::detect_cycles`
   already proves, CLI verb design) that would delay the Fortune-5-bundle goal by a
   full release cycle or more.
2. **Resurrect `PackComposer`/`DependencyGraph` into a CLI verb.** Rejected: doing so
   without also fixing Finding 1 (the closed schema) only wires dead code to a
   command — the composer still has nothing to resolve, since no pack can declare
   what it needs. Wiring it would produce a verb that requires callers to enumerate
   every pack by hand anyway, which is no improvement over today's `[packs]`-table
   editing.

The recommended alternative (detailed in the companion tickets in this set) is a
**bundle-manifest + installer pattern** built entirely on top of what already works
today: a new `mix ggen_igniter.fortune5_ready` task, modeled on
`~/ggen_igniter/lib/mix/tasks/ggen_igniter.install.ex`'s existing shape, that
(a) knows a fixed bundle of pack names, (b) merges them into the consumer's real
`ggen.toml` `[packs]` table (deduping against already-wired packs) via real TOML I/O,
(c) shells out to a real `ggen sync run`, and (d) verifies success by running the
newly-wired packs' own gates for real rather than declaring "ready" from generation
success alone. This sidesteps both rejected paths — it needs no change to
`pack.toml`'s schema, no change to `ggen pack add`, and no revival of
`PackComposer`/`DependencyGraph` — while still delivering the one-command bundle
experience for a real first consumer (beam4pm).

## Definition of done

- This document exists at `docs/jira/v26.9.1/01-PACK-COMPOSITION-GAP.md`, citing
  exact file paths and line numbers for every claim above, re-verified against the
  live `~/ggen` and `~/ggen-marketplace` trees in the session that wrote it (see
  the grep/line-number evidence embedded in each Finding).
- No source file in `~/ggen` or `~/ggen-marketplace` is modified by this ticket.
- Companion tickets 02–06 reference this document rather than re-deriving Findings
  1–4.

## See Also

- `crates/ggen-engine/src/pack.rs` — the closed `PackToml`/`PackMeta` schema
  (Finding 1)
- `crates/ggen-marketplace/src/packs_registry/composer.rs`,
  `crates/ggen-marketplace/src/packs_registry/dependency_graph.rs`,
  `crates/ggen-marketplace/src/packs_registry/installer.rs` — the dead-end
  composition subsystem (Finding 2)
- `crates/ggen-cli/src/cmds/pack.rs`, `crates/ggen-cli/src/cmds/packs.rs` — the real,
  single-pack-only CLI surface
- `~/ggen-marketplace/docs/thesis/09-pack-algebra.md` — the aspirational composition
  algebra (Finding 4)
- `~/ggen-marketplace/packs/shacl-projection-pack/pack.toml` — independent prior-art
  confirmation of this exact limitation, written by an earlier pack author
- `~/ggen_igniter/lib/mix/tasks/ggen_igniter.install.ex` — the installer shape the
  recommended bundle-manifest workaround is modeled on
- `~/beam4pm/docs/jira/v26.8.31/04-jira-epics-stories-acceptance.md` — this repo's
  epic/story/acceptance-bullet convention followed by the companion tickets in this
  set
