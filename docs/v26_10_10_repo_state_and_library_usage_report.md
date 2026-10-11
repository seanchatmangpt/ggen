# v26.10.10 Repo State and Library Usage Report

Audit date: 2026-10-09 · Branch: `spec-integration` @ 4619246cc · Read-only audit; no code changed.
Every count in this report was re-measured on this checkout (commands quoted inline as receipts).

# 1. Executive Summary and Technical Debt Scorecard

ggen is in better shape than the modernization plan assumed. `ggen-core` is fully deleted;
`ggen-abb-sbb` is already a workspace member (integrated v26.10.9) with Stage 2c admission
wired into `sync.rs` and its full test ladder green (59 passed, 0 failed — measured below).
The real remaining debt is concentrated in four places:

1. **star-toml adoption is partial**: 4 `star_toml::from_str`/`load_file` adoption sites vs
   **28 remaining `toml::from_str` call sites** across 23 files (measured receipt below).
2. **Schema drift guard under-dispatched**: `config_schema.rs` classifier exists, but several
   call sites (doctor, graph validate, law verbs, LSP project index) still hardcode one schema.
3. **Missing target schema sections**: no `[rules]` n3/datalog and no `[pack_sources]` section
   in either ggen.toml schema (grep: zero hits).
4. **Dual RDF stack**: oxigraph 0.5.11 (modern, load-bearing) coexists with praxis-graphlaw's
   pinned oxrdf 0.3.4/sparesults 0.3.4 vendored fork, quarantined by N-Triples-string seams and
   `publish = false`.

## Scorecard

| Dimension | State | Evidence |
|---|---|---|
| Workspace members | 15 (incl. `ggen-abb-sbb`) | `Cargo.toml:61-105` |
| Excluded | `examples/7-agent-validation`, `crates/ggen-architecture` | `Cargo.toml:125` |
| ggen-core | Deleted 2026-07-17 (directory absent) | `find` zero hits; `Cargo.toml:106-109` |
| Total first-party LOC (16 crate dirs) | 252,660 | `wc -l` receipt below |
| `toml::from_str` legacy sites | 28 sites / 23 files | grep receipt below |
| `star_toml` adoption sites | 4 (`config_lib/parser.rs:86,96`, `engine/config.rs:228`, `engine/pack.rs`) | grep receipt |
| `praxis_*::` refs outside praxis crates | 76 refs / 16 files, all in ggen-engine + ggen-config + engine tests | grep receipt |
| oxigraph version split | 0.5.11 (workspace) vs 0.3.4 oxrdf/sparesults (praxis-graphlaw only) | manifests |
| ggen-abb-sbb tests | **59 passed, 0 failed** (11 lib + 4 bench_bound + 40 falsifiers + 1 fixture + 3 doc) | `cargo test` receipt below |
| Publish blockers | praxis-core, praxis-graphlaw, ggen-engine `publish = false` (sibling-repo absolute-path deps) | manifests |

## LOC receipt (command: `find crates/<c>/src -name '*.rs' | xargs wc -l | tail -1`)

| Crate | LOC | Files | Responsibility |
|---|---|---|---|
| ggen-marketplace | 42,393 | 74 | Package management / registry / RDF search |
| praxis-graphlaw | 41,674 | 118 | Law-state engine: N3, Datalog, SPARQL 1.1, SHACL, ShEx (vendored roxi fork) |
| ggen-lsp | 30,082 | 95 | Language server, analyzers, check/intel/pack/route-repair |
| ggen-cli | 22,849 | 88 | CLI (`ggen-cli-lib`, bin `ggen`) |
| ggen-engine | 21,288 | 33 | SPARQL-in-Tera codegen engine (vendored from ~/praxis/crates/ggen) |
| ggen-graph | 13,600 | 58 | Deterministic RDF graph module (oxigraph) |
| bcinr-pddl | 12,089 | 21 | PDDL8 → POWL → Prolog8 admission → OCEL → receipt loop |
| ggen-config | 10,290 | 26 | ggen.toml / pack.toml configuration parser+validator |
| ggen-architecture (excluded, own workspace) | 8,000 | 13 | Building-block kernel, Fortune-5 profiles |
| praxis-core | 6,331 | 16 | Law Object: obligation + lifecycle + receipt + OCEL |
| ggen-mcp | 6,014 | 29 | MCP server over ggen-engine/graph/config |
| powl2-decompose | 2,682 | 7 | WF-nets → POWL 2.0 |
| ggen-abb-sbb | 1,552 | 2 | IO-free ABB/SBB admission kernel |
| bcinr-mfw-ir | 1,969 | 10 | MFW planner IR types |
| ggen-cheat-scanner | 755 | 2 | AST scanner for test-quality cheats |
| pm4pytest-cli | 39 | 1 | Thin binary wrapper |

# 2. Workspace and Crate Graph

## Manifest facts (`/Users/sac/ggen/Cargo.toml`)

- **members (15)**: ggen-config, ggen-marketplace, ggen-cli, **ggen-abb-sbb (line 65,
  integrated v26.10.9)**, ggen-graph, ggen-lsp, ggen-engine, praxis-core, praxis-graphlaw,
  powl2-decompose, bcinr-pddl, bcinr-mfw-ir, ggen-cheat-scanner, ggen-mcp, pm4pytest-cli.
- **exclude (2)**: `examples/7-agent-validation` (archived, drifted), `crates/ggen-architecture`
  (independent nested workspace; Cargo forbids dual membership).
- **workspace.dependencies**: registry-pinned versions — `clap-noun-verb =26.9.1`,
  `oxigraph 0.5.11 (rdf-12)`, tera, ed25519-dalek, bcinr 26.7.25, wasm4pm-compat 26.8,
  chicago-tdd-tools 26.8.9, opentelemetry 0.33, rig-core 0.42. Path deps: ggen-cli-lib,
  ggen-graph, ggen-config, ggen-marketplace, ggen-engine. Commented/dormant: `ggen-yawl`,
  `chatman-common`. Only git dep: `cargo-cicd` (seanchatmangpt/cargo-cicd@main).
- **No `../graphlaw` or sibling-repo path dep exists in any manifest.** The "graphlaw"
  foundation library in this repo is the vendored `crates/praxis-graphlaw`. Comments in
  ggen-cli/ggen-graph/ggen-lsp manifests mention historical *absolute* sibling paths (on
  `/Users/sac/bcinr`, `/Users/sac/wasm4pm-compat`) via the vendored praxis crates — that is
  what forces `publish = false` down the tree.

## Inter-crate dependency edges

```
ggen-marketplace → ggen-config
ggen-cli → ggen-config, ggen-marketplace, ggen-lsp (optional: lsp/experimental)
ggen-lsp → ggen-engine, ggen-marketplace, ggen-config, ggen-graph
ggen-mcp → ggen-engine, ggen-graph, ggen-config (dev: ggen-lsp, ggen-marketplace)
ggen-engine → praxis-core, praxis-graphlaw, ggen-abb-sbb (path, Cargo.toml:97)
praxis-core → praxis-graphlaw (one-way)
praxis-graphlaw → powl2-decompose, bcinr-pddl
bcinr-pddl → bcinr-mfw-ir
ggen-graph → ggen-engine [dev-dep ONLY — publish-safety, N-Triples seam]
```

No cycles in `[dependencies]`; the only intentional cycle-avoidance device is ggen-graph's
dev-only engine edge. ggen-abb-sbb has no `[features]`; its `Cargo.lock`/`target/` inside the
directory are leftovers from its pre-v26.10.9 independent-workspace era (candidates for cleanup
in the refactor pass, not this audit).

## Feature flags

| Crate | Features | Notes |
|---|---|---|
| ggen-cli | `autonomic`, `live-llm-tests`, `integration`, `lsp`, `experimental`; default `[]` | lsp/experimental pull ggen-lsp |
| ggen-lsp | `mcp`, `a2a = [mcp,…]`, `http-adapter`, `all-adapters`, `benchmark` | 6 optional deps |
| ggen-engine | `repl`, `mcp` | neither gates oxigraph/praxis/abb-sbb |
| praxis-graphlaw | `cognition = [dep:wasm4pm-cognition]` | deliberately never enabled (BUSL-1.1 license, `Cargo.toml:50-60`) |
| praxis-core | `ocel` (empty) | — |
| bcinr-pddl | `dhat-heap`, `logistics`, `brce_loop`, `vision2030`, `dfcm_alloc_profile`, `indexed_grounding`, `pddl_80_20`, `scaling` | — |

# 3. Legacy Footprint: oxigraph & praxis

## 3.1 Oxigraph

**This is not legacy in the current path.** oxigraph 0.5.11 is the modern, load-bearing RDF
stack (`Cargo.toml:174`, root dev-dep `:727`).

Direct dependents: ggen-graph (`Cargo.toml:16`), ggen-engine (`Cargo.toml:78` + independent
`spargebra 0.4.7` pin at `:91`), praxis-graphlaw (`:38` + the only oxrdf 0.3.4/sparesults 0.3.4
pins at `:27,17`), ggen-lsp (`:61`), ggen-cli (`:239` deps, `:284` dev-deps), ggen-marketplace
(`:48`). ~45 vendored/example manifests also consume it (mostly 0.5.8).

**Receipt** (top hits, `grep -rc oxigraph crates/*/src crates/*/tests`):

| File | Hits |
|---|---|
| praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs | 37 |
| ggen-engine/src/graph.rs | 23 |
| ggen-graph/src/ocel/projection.rs | 15 |
| ggen-graph/src/graph/dataset.rs | 13 |
| ggen-marketplace/src/marketplace/{search_sparql,rdf/control,registry_rdf,rdf_mapper}.rs | 11–12 each |
| ggen-graph/src/sparql.rs, shacl.rs, dialect.rs | SparqlEvaluator + Store::new |

Key hotspots:
- `crates/ggen-graph/src/lib.rs:65,69` — `GraphError::Oxigraph(#[from] StorageError)` /
  `GraphError::Sparql(#[from] QueryEvaluationError)` — the only structured oxigraph error
  plumbing.
- `crates/ggen-graph/src/graph/dataset.rs:15-53` — in-memory `Store::new()` allocations;
  `:94-95` `SparqlEvaluator`; `:238-244` `QueryResults` matching.
- `crates/ggen-engine/src/graph.rs:15-20` — oxigraph io/model/sparql/store imports; `:42`
  `DeterministicGraph` store init; `:161,242-380` independent spargebra AST walk (GRAPH/SERVICE
  detection — oxigraph keeps its parsed query private); `:579,1083-1092` comments documenting
  the oxrdf 0.3.x vs oxigraph 0.5.x split.

**Test harnesses asserting oxigraph error types**: `ggen-graph/tests/hook_scheduler.rs:102`
(`QueryEvaluationError::UnsupportedService` contract), ggen-cli `src/utils/error.rs:216-229`
(From impls for StorageError/QueryEvaluationError/SerializerError), plus oxigraph-heavy suites
`vocab_projection.rs` (10), `sparql_actuation.rs` (9), `hook_loader.rs` (6), `ocel_self_audit.rs`
(5), `law_engine_bridge_e2e.rs` (14), `product_mirror_conformance.rs` (7).

## 3.2 praxis / praxis-core / praxis-graphlaw

**Why still linked** (documented in-tree):
1. praxis-graphlaw is the **only** N3/Datalog materialization + SHACL/ShEx denial engine in the
   workspace ("the roxi fork", `graph.rs:1075-1092`). praxis-core is the only receipt-chain
   signing/verification + refusal-quarantine implementation. No replacement exists yet — that is
   the actual deprecation blocker, not inertia.
2. Vendored by design (`ggen-engine/Cargo.toml:5-8`): migration-specific edits (e.g.
   `ReceiptRecord.signature_hex`) land in-tree, not upstream in ~/praxis (`Cargo.toml:68-71`).
3. Publish-safety: both are `publish = false` because of hard sibling-repo absolute-path deps;
   ggen-engine consequently is too, and is deliberately absent from the root `ggen` package
   (`Cargo.toml:680-692`, disconnected from root 2026-07-16).

**Shim/re-export surface**: `crates/praxis-core/src/lib.rs:23-30` is the shim — `pub use` of
`DefaultLaw`, `Admit/Andon/Judge/LawObject/Obligation`, `RiceQuarantine`, `ReceiptRecord`,
`ReceiptStore`, `ReceiptValidator`, refusal types. praxis-graphlaw's `lib.rs:44+` carries a
documented clippy-suppression block with the rule "fix upstream in ~/praxis and re-vendor."
No consumer-side re-export shims exist; consumers call `praxis_*::` directly.

**Receipt** — 76 `praxis_core::`/`praxis_graphlaw::` refs outside the praxis crates, in 16
files, all in ggen-engine (src + 11 test files), ggen-config, and one ggen-config receipt impl:

| Hotspot | Role |
|---|---|
| `ggen-engine/src/law_engine.rs:8,19,46,61-63,79-80,100-101` | `LawEngine` trait — seam deliberately passes only N-Triples/N3 **strings** ("no oxrdf/spargebra/oxigraph model type"); sole impl builds `praxis_graphlaw::TripleStore` (`build_store`:61-63) |
| `ggen-engine/src/graph.rs:1075-1232` | `GraphLawStore` — praxis-graphlaw as live law engine over an oxigraph `DeterministicGraph` mirror; `TripleStore` allocations `:1157,1197,1324`; hook verdicts `EffectKind::Refuse`/`HookVerdict::Fired` `:1213-1214` |
| `ggen-engine/src/sync.rs:30,3021` | praxis-core `ReceiptRecord` chain over sync output |
| `ggen-engine/src/verbs/handlers.rs:447-710` | receipt-chain verification (`RECEIPT_RECORD_VERSION`, `ChainVerification`, `ChainStanding`, `ChainRuleMonotonicity`); Turtle parse via praxis-graphlaw `:256` |
| `ggen-engine/tests/receipt_chain_e2e.rs:373-374` (+10 other engine test files) | praxis-core APIs in tests |
| `ggen-config/src/manifest/types.rs:150,195,267` | `[law]` config types documented as praxis-graphlaw-backed |
| `ggen-config/src/receipt/receipt_impl.rs:18` | receipt impl |

(Note: `praxis:*` strings in `src/verbs/{graph,receipt,law,sync}.rs` are ontology-class IRIs
from `schema/praxis.ttl`, not crate references.)

**Version-conflict quarantine**: the oxrdf 0.3.4 stack is contained inside praxis-graphlaw
(its `oxrdf_adapter.rs`, 14 src hits + 50 test hits, is the biggest single hotspot); only plain
N-Triples/Turtle/N3 strings cross into ggen-engine (`law_engine.rs:8`, `graph.rs:579`).
ggen-graph's engine dep is dev-only so the bridge never enters `cargo publish`
(`ggen-graph/Cargo.toml:35-48`; DoD item 3 of docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md).

# 4. Target Library Readiness and Gap Analysis

## 4.1 star-toml / star-toml-derive

**Current state**: `star-toml = "26.7.3"` is a dep of ggen-config (`Cargo.toml:27`) and
ggen-engine (`Cargo.toml:85`). Adopted at 4 sites:
- `crates/ggen-config/src/config_lib/parser.rs:86,96` (`from_str`/`load_file` — frontmatter
  schema `GgenConfig`) and `:135` (`find_config_file`).
- `crates/ggen-engine/src/config.rs:228` (`load_file` + `star_toml::Validate` enforcement,
  `:19`, doc `:1-9`).
- `crates/ggen-engine/src/pack.rs:719` (`PackToml`) and `:1113` (lockfile `LockDoc`).

**Gap — 28 `toml::from_str` sites in 23 files remain** (receipt: `grep -rn toml::from_str
crates --include='*.rs' | grep -v star_toml | wc -l` → 28). Files: ggen-config
(`manifest/parser.rs:75`, `manifest/types.rs:104`, `config/{lock_manager,ontology_config,
template_config}.rs`, `domain/mcp_config.rs`), ggen-marketplace (7 files incl.
`packs_registry/metadata.rs:92,125`, `packs_registry/repository.rs:139,161`,
`marketplace/metadata.rs:172`, `profile.rs`, `compatibility.rs`, `pki.rs`),
ggen-lsp (3), ggen-cli (4), ggen-engine (`config.rs:267` legacy path + `pack.rs` residual).

**Schema-gap findings**:
- `[capabilities]` provides/requires: **enforced** for pack.toml — `PackToml.capabilities`
  (`pack.rs:101`, types `:57-64,118-125`, provider satisfaction `:464-482`). Not present as a
  ggen.toml section.
- `[rules]` n3/datalog: **absent everywhere** (grep `datalog` in ggen-config/ggen-engine
  sources: zero). Current rules are SPARQL CONSTRUCT (`manifest/types.rs:180,337`,
  `Vec<InferenceRule>`/`Vec<GenerationRule>`) + SHACL; N3 lives only inside law_engine. A
  `[rules]` n3/datalog schema section is net-new surface, not a parser swap.
- `[pack_sources]`: **absent** as a manifest section in both schemas. Nearest existing thing is
  the marketplace lockfile `PackSource` enum (`sync_profile.rs:16,135`) — internal type, not a
  manifest section.
- **Schema drift guard**: `crates/ggen-config/src/config_schema.rs` (1-125) classifies
  `DeclarativeRules`/`Frontmatter`/`Ambiguous` structurally. **Phase-1 correction (2026-10-09,
  verified)**: the classifier doc's "only 1 of 6 call sites dispatches" claim is stale — all six
  ggen.toml sites already dispatch through `ggen_config::classify_ggen_toml` via
  `crates/ggen-engine/src/schema_dispatch.rs` (callers `sync.rs:259`,
  `verbs/handlers.rs:132/919/1118`, `project_graph.rs:58`; ggen-lsp `project_index.rs:188-235`;
  ggen-mcp `tools/config_classify.rs:65`). The classifier doc text, not the code, needs updating.

## 4.2 clap-noun-verb

**Already the architecture in use** — `clap-noun-verb =26.9.1`, `#[verb]` fns +
`#[linkme::distributed_slice]` auto-registration; entry `crates/ggen-cli/src/lib.rs:110-227`
(`cli()` → `clap_noun_verb::run()`), router `src/cmds/mod.rs:1-70`. No derive enums, no manual
match blocks. `generated_commands.rs` is generated and hook-protected.

Nouns (~29): agent, a2a, bblock, capability, deploy, describe, doctor, explain, framework,
generate, git-hooks, init, logs, lsp, maximalism, mcp, ontology, pack, packs, packs-receipt,
policy, receipt, **sbb**, status, sync, telco, validate, vision2030, wizard. Default verbs:
`sync run`, `doctor run`, `receipt verify` (`generated_commands.rs:21-22`).

Mapping to the target `workspace × pack × graph × receipt` noun set: `pack`/`packs`/`receipt`
exist; `graph`, `workspace`, and `explain` exist as verbs/nouns in partial form (`explain`
noun, graph verbs under engine routing). Renaming/merging nouns is mechanical under the
`#[verb]` model.

**JSON readiness: the real gap.** `pack doctor` returns `Result<serde_json::Value>`
(`cmds/pack.rs:435,480`); no global `--json` flag exists in `lib.rs`; remaining verbs return
unit/strings. MCP overlap: `ggen-mcp/src/tools/` (17 tools) re-implements CLI-parallel logic —
`capability_status.rs:83` re-parses ggen.toml as `toml::Table` (also a star-toml migration
site); `pack_query`, `sync_dry_run`, `config_classify`, `check_project` duplicate CLI verbs.

## 4.3 graphlaw (target: PurRDF + Eyeron)

- **Dependency resolution**: no `../graphlaw` path dep and no `ash_graphlaw` anywhere in the
  workspace. The law engine is vendored `crates/praxis-graphlaw` (workspace member). The
  target-library swap therefore means: introduce the graphlaw crate (path or registry), then
  re-bind two seams (below), then retire the vendored fork.
- **Feature surface today** (praxis-graphlaw): `cognition` (off, license); engines N3, Datalog,
  SPARQL 1.1, SHACL, ShEx behind `TripleStore::{load_triples(Syntax), load_rules, materialize,
  check_denials, prove, solve, query, validate_shacl, validate_shex(_c), load_hook_pack,
  get_hook_receipts, materialize_owlrl}` (`praxis-graphlaw/src/lib.rs:348-847`).
- **`crates/ggen-engine/src/graph.rs`** public surface: `DeterministicGraph` (`new:41`,
  `insert_turtle:51`, `query:160`, `all_quads:205`, `state_hash:221`), `GraphEngine` trait impls
  `:951,1175`, `GraphLawStore` (`:1091`, `new:1122`), `Delta` (`compute:1348`, `apply:1369`,
  `inverse:1413`, `compose:1426`, `hash:1463`), plus `EngineTriple:624`, `ShaclOutcome:656`,
  `MaterializeOutcome:665`.
- **LawState re-binding points** (what must change):
  1. `crates/ggen-engine/src/law_engine.rs` — `build_store:61-63` constructs
     `praxis_graphlaw::TripleStore` directly from `facts_ntriples`/`rules_n3` strings; replace
     with graphlaw `LawState` construction. The `LawEngine` trait seam (`:8,19,46`) is already
     model-free — only the impl body changes.
  2. `crates/ggen-engine/src/graph.rs:1075-1232` — `GraphLawStore`'s `TripleStore` allocations
     (`:1157,1197,1324`) and hook-verdict checks (`:1213-1214`) re-bind to `LawState`.
  3. `crates/ggen-engine/src/verbs/handlers.rs:256` — Turtle parse via praxis-graphlaw.
  4. ggen-graph's dev-only `tests/law_engine_bridge_e2e.rs` (14 oxigraph refs) pins the seam
     contract; it is the regression gate for this migration.

## 4.4 ggen-abb-sbb

- **Wired, not pending.** Workspace member (`Cargo.toml:65`, integrated v26.10.9 per in-tree
  comment `:123-124`); path dep of ggen-engine (`ggen-engine/Cargo.toml:97`).
- **Stage 2c admission is implemented** in `crates/ggen-engine/src/sync.rs:814-884`:
  `ggen_abb_sbb::depgraph::{extract_pack_manifest, extract_consumer_edges, resolve_sync_order}`
  (`:828,838,842`), EA-graph parse `ggen_abb_sbb::parse_graph` (`:860`), SBB admission
  `ggen_abb_sbb::admit(&ea_graph, &Request{ authority: Authority::Construct, … })` (`:874-884`)
  with typed refusal propagation. The engine's own `AdmissionItem`/`AdmissionLedger`/
  `AdmissionDecision` (`sync.rs:32`, assembly `:3207+`) is receipt-ledger bookkeeping —
  adjacent to, not duplicating, the kernel's SELECT-or-MANUFACTURE admission.
- **Test coverage: GREEN.** Receipt (`cargo test` in `crates/ggen-abb-sbb`, 2026-10-09):
  `11 passed` (lib) + `4 passed` (bench_bound) + `40 passed` (falsifiers) + `1 passed`
  (marketplace_fixture) + `3 passed` (doc-tests) = **59 passed, 0 failed, 0 ignored**.
  Falsifier suite covers: byte-identical reproduction, duplicate-refusal/idempotence, unknown
  ABB/SBB refusal, mutable-SBB refusal, digest mismatch, qualification staleness, authority
  ceilings (DO clamped to CONSTRUCT), dangling references, malformed-input typed refusals.

# 5. Migration Call Graph (remove vs rewrite)

## Remove (legacy/dead)

| Item | Location | Action |
|---|---|---|
| `toml::from_str` legacy parses | 28 sites / 23 files (§4.1 list) | replace with `star_toml::from_str`/`load_file` + `Validate` |
| Hardcoded schema dispatch | doctor, `graph validate`, five `law *` verbs, `ggen-lsp ProjectIndex::from_root_with_overlay` | route through `config_schema::classify` (5 call sites) |
| Stale comment | root `Cargo.toml` ~line 204 re "ggen-core untouched on disk" | fix text (dir deleted) |
| `ggen-abb-sbb/Cargo.lock` + `target/` | pre-v26.10.9 independent-workspace leftovers | delete in refactor pass |
| CLI/MCP duplication | `ggen-mcp/src/tools/capability_status.rs:83` `toml::Table` re-parse; `pack_query`/`sync_dry_run`/`config_classify`/`check_project` vs CLI verbs | MCP calls ggen-cli-lib verb fns; single star-toml parse path |

## Rewrite (keep behavior, change backing)

| Symbol | File:Line | From → To |
|---|---|---|
| `LawEngine` impl `build_store` | `ggen-engine/src/law_engine.rs:61-63` | `praxis_graphlaw::TripleStore` → target `graphlaw::law::LawState` |
| `GraphLawStore` store allocs + hook verdicts | `ggen-engine/src/graph.rs:1157,1197,1324,1213-1214` | same re-bind; keep `GraphEngine`/`Delta` API frozen |
| Turtle parse | `ggen-engine/src/verbs/handlers.rs:256` | praxis-graphlaw parser → graphlaw parser |
| Receipt chain | `ggen-engine/src/verbs/handlers.rs:447-710`, `sync.rs:3021` | praxis-core `ReceiptRecord` → graphlaw (or successor) receipt chain |
| CLI verb outputs | `crates/ggen-cli/src/cmds/*` | unit/string → `serde_json::Value` (+ global `--json`) for MCP/agent tool calling |
| Schema sections | `ggen-config/src/manifest/types.rs` + `ggen-engine/src/config.rs` | add `[rules]` (n3/datalog) and `[pack_sources]` to both schemas |

## Keep (not legacy)

- oxigraph 0.5.11 stack (ggen-graph + engine mirror + marketplace + lsp + cli) — modern and
  load-bearing; only praxis-graphlaw's internal oxrdf 0.3.4 pins retire with the fork.
- N-Triples-string seams (`LawEngine`, dev-only ggen-graph→engine edge) — they are what makes
  the LawState re-bindable without a cross-crate type conflict.
- `ggen-abb-sbb` admission kernel — wired, tested, current.

# 6. Step-by-Step Refactor Order (green builds between every step)

Each step ends with `just check`/`just test` green (or an Andon stop-and-fix) before the next.

1. **Hygiene** (no behavior): fix stale ggen-core comment in root `Cargo.toml`; delete
   `crates/ggen-abb-sbb/{Cargo.lock,target}` leftovers; confirm `cargo metadata` clean.
2. **Schema drift-guard completion**: route the 5 remaining call sites (doctor, graph
   validate, law verbs ×5, LSP `ProjectIndex::from_root_with_overlay`) through
   `config_schema::classify`. Pure dispatch change; existing e2e tests
   (`config_schema_dispatch_e2e.rs`) extend to cover each site.
3. **star-toml migration, crate by crate** (smallest blast radius first): ggen-cli (4 sites) →
   ggen-lsp (3) → ggen-mcp (1) → ggen-marketplace (7) → ggen-config manifest schema
   (`manifest/parser.rs:75`, `types.rs:104`) → ggen-engine residuals (`config.rs:267` legacy
   path, `pack.rs`). Each crate keeps its serde structs; only the parser call swaps, so tests
   are the existing fixture suites.
4. **New schema sections**: add `[rules]` (n3/datalog) and `[pack_sources]` to both ggen.toml
   schemas via `star_toml::Validate` impls + dual-schema fixtures in
   `ggen-config/tests/schema_dual_fixtures_test.rs`. Wire datalog rules into
   `generation_rules.rs` as a new rule kind alongside SPARQL CONSTRUCT.
5. **CLI JSON surface**: add global `--json`; convert verb outputs to `serde_json::Value`
   starting with the nouns the MCP tools duplicate (`pack`, `sync`, `capability`, `receipt`,
   `explain`); re-point those MCP tools at ggen-cli-lib verb fns (kills the `toml::Table`
   re-parse at `capability_status.rs:83`). Verify with `ggen-mcp` tool-call round-trip.
6. **graphlaw LawState re-binding**: introduce the target graphlaw crate as a dependency;
   re-bind the four points in §4.3 behind the unchanged string seams (law_engine impl,
   GraphLawStore, handlers.rs parse, receipt chain). Regression gates:
   `law_engine_bridge_e2e.rs` (ggen-graph dev), `law_engine_test.rs`,
   `receipt_chain_e2e.rs` + the receipt e2e family (11 engine test files).
7. **Retire the vendored fork**: once 6 is green, remove `praxis-core`/`praxis-graphlaw`
   members, their path deps, and the praxis-core shim re-exports (`praxis-core/src/lib.rs:
   23-30`). This is the step that lifts `publish = false` off ggen-engine (pending the
   sibling-repo absolute-path deps the manifests document).
8. **Warning sweep**: `cargo clippy --workspace --all-targets` to zero, removing the
   documented `#![allow(...)]` block that leaves with praxis-graphlaw (step 7).

Order rationale: 1-2 are risk-free; 3 unblocks 4 (schema work wants the final parser); 5 is
independent of 6 and parallelizable; 6→7 are the only steps touching the reasoner, and the
string seams mean a failure there cannot corrupt the RDF store layer; 8 is pure cleanup that
gets cheaper after 7.

---

## Verification receipt

- LOC/files: `find crates/<crate>/src -name '*.rs' | xargs wc -l` per crate (§1 table).
- `grep -rn "toml::from_str" crates --include='*.rs' | grep -v star_toml | wc -l` → 28.
- `grep -rn "star_toml::from_str" crates` → 4; `star_toml::load_file` additional sites read.
- `grep -rc oxigraph crates/*/src crates/*/tests` (top-20 table §3.1).
- `grep -rn "praxis_core::\|praxis_graphlaw::" crates | grep -v ^crates/praxis | wc -l` → 76.
- `cargo test` (crates/ggen-abb-sbb): 11+4+40+1+3 = **59 passed, 0 failed**.
- No source files modified; session's only write is this report.

# 7. Phase 1 execution delta (2026-10-09)

Phase 1 (hygiene + schema-dispatch verification + star-toml migration) executed on this branch.
Full receipt: `docs/v26_10_10_phase1_receipt.md`.

- **Step 1 (hygiene)**: root `Cargo.toml` ggen-core comment corrected (was "untouched on
  disk"); `star-toml = "26.7.3"` added to `[workspace.dependencies]`; ggen-config +
  ggen-engine star-toml pins normalized to `workspace = true`. The abb-sbb `Cargo.lock`/
  `target/` deletion was permission-denied — **NOT DONE**, remains open in §5.
- **Step 2 (schema dispatch) — premise corrected**: all six ggen.toml sites already dispatch
  through `classify_ggen_toml` (see §4.1 correction). Work done = test coverage, not dispatch
  rewiring: 3 new CLI-transport e2e tests appended to `crates/ggen-engine/tests/
  config_schema_dispatch_e2e.rs` (graph validate both schemas + ambiguous refusal; law load
  both schemas + ambiguous refusal) — suite now **11 passed, 0 failed**. New
  `crates/ggen-lsp/tests/project_index_schema_dispatch_test.rs` — **6 passed, 0 failed**.
- **graphlaw WIP (concurrent v26.10.9 effort)**: `law_engine.rs` fully repointed to graphlaw
  (Eyeron N3, PurRDF); one pre-existing compile break fixed at `law_engine.rs:189`
  (`check_structure` `Vec<StructureError>` → `format!("{e:?}")`); `cargo check -p ggen-engine`
  exit 0; full engine+graph+abb-sbb ladder **86 tests, 0 failures** (config_schema_dispatch_e2e
  11, graphlaw_e2e 6, composed_packs_e2e 2, abb_sbb_datalog_admission_e2e 4, law_engine_test 4,
  law_engine_bridge_e2e 4, abb-sbb 59).
- **Environment note**: `TMPDIR` points at nonexistent `~/.cache/tmp`; tests need `TMPDIR=/tmp`.
- **star-toml migration — COMPLETE (supersedes "28 sites" in §1/§4.1)**: all target crates
  migrated — ggen-config 6/6 sites (lib **115/0**), downstream 12/12 sites: ggen-marketplace
  (**503/0**), ggen-cli (**85/0**), ggen-lsp lib (**238/0**), ggen-mcp check clean
  (`capability_status.rs:83` now `star_toml::from_str::<toml::Value>`). Final state = 9
  documented exception sites stay on `toml` (lsp analyzer/formatting 3, cli policy 1,
  marketplace compatibility 3, abb-sbb depgraph 2). Engine residuals were already 0; final
  full-grep recount: **PENDING**.
- **Full-suite verifications**: ggen-lsp 367/0 (40 targets), ggen-mcp 100/0 (18 targets),
  ggen-graph ~130/0, ggen-cheat-scanner 19/0, bcinr-pddl lib 91/0; governance gate 4/4 after
  CLAUDE.md + architecture.md corrected to 16 crates; `repo-facts.ttl` fixed (added
  `rf:crate_ggen_abb_sbb`, `crateMapIntro` → 15 members + root = 16), parity test 2/2.
- **New schema sections (§6 step 4, DONE)**: `[rules]` (n3/datalog) + `[pack_sources]` landed
  in `GgenManifest`; classifier `DECLARATIVE_ONLY_TABLES` updated; 9 new tests in
  `schema_sections_test.rs`; `resolve_rule_sources` seam in `generation_rules.rs` with typed
  `DatalogUnsupported` refusal (4/4 in-module tests). Supersedes §4.1 "absent everywhere".
- **Censuses recorded**: clippy 0/0 workspace-wide but via blanket workspace-lint allows (root
  `Cargo.toml:305+`, praxis-graphlaw 22-lint allow) — recommend fix-forward per-crate. Praxis
  refs down to 66 / 14 files; receipt-chain family (≈48 refs) has no graphlaw equivalent =
  dominant retirement blocker. `GraphLawStore` still dual-sourced on
  `praxis_graphlaw::TripleStore` (`graph.rs:1157` area) — NOT yet repointed.
- **Still open**: engine all-targets full tally (PENDING, lane running); abb-sbb
  `Cargo.lock`/`target` deletion (permission denied); stray lane build dirs
  (`/tmp/lanej-target`, `/tmp/lsp-lane-target`) to clean at integration.

### Wave-3 delta (2026-10-09, later same day)

- **Praxis census refresh**: praxis-core shim pruned to its live surface
  (receipt_epoch, receipt_record, law::Andon + internal deps; 9 modules + 6 test
  files removed, check clean, law_engine_test 4/0, receipt_chain_e2e 25/0).
  Residual census now **66 refs / 14 files**, all in ggen-engine. The
  receipt-chain family remains the dominant blocker (no graphlaw equivalent);
  the verbatim-port retirement design is recorded in
  `docs/v26_10_10_praxis_retirement_plan.md`.
- **Lint findings refined**: ggen-graph is structurally clean under
  pedantic+nursery; the blanket-allow cost is concentrated in the vendored
  crates (praxis-graphlaw 22-lint allow), and **bcinr-pddl lacks
  `[lints] workspace = true`** wiring, so workspace lint policy silently skips
  it.
- **Disk receipt**: osx-clnr cache-clean freed **78.82 GB** (7 lane build
  roots); an oclnr plan at `/tmp/oclnr-lane-plan.json` holds ~35 GB of older
  scratch candidates awaiting user review. TMPDIR root cause resolved
  (`~/.uvrc:4` export dangled by the `~/.cache` wipe; dir restored,
  self-heals on new shells).
- **DoD checklist added**: the full v26.10.10 modernization Definition-of-Done
  checklist (23 items with evidence tallies, DONE/PARTIAL/PENDING) now lives in
  `docs/v26_10_10_phase1_receipt.md` ("Definition of Done" section) — see that
  table for the authoritative status of star-toml migration, schema
  drift-guard, N3 rules, GraphLawStore repoint, praxis-core prune/retirement,
  OCEL determinism, lint wiring, crate-count alignment, bcinr-pddl fixes, CLI
  --json, composer revival, and the open user-gated items.

### Closing drift reconciliation (2026-10-09, end of session)

Numbers in this report and in §7 above were measured mid-wave; the settled-tree deltas
(sync→`build_tera_with_packs` wiring landed at `sync.rs:1009,1063`; `GraphLawStore`
repointed off `praxis_graphlaw::TripleStore` — 0 residual hits; star-toml raw grep now
11 sites = 9 documented + 2 new corpus-test sites; 87 pack.toml modified by the
capability-annotation fan-out; BC/BD/BE/BG lanes still in flight) are itemized in the
"Closing drift reconciliation (2026-10-09)" section appended to
`docs/v26_10_10_phase1_receipt.md`. That section is authoritative over this report's
stale counts.

### Final session delta (2026-10-09, end of session)

Prose-only `requires` purge DONE (297→37, all kept entries contract-evidenced, 1 UNCERTAIN,
court 2 pending); composer capabilities-aware arbitration VERIFIED live (11/0; cross-corpus
DuplicateCapability refusal is design-intent tripwire — SJIRA-13 URN-dedup proposal FALSIFIED
and WITHDRAWN; canonical census 403 manifests / 331 unique names / 72 mirrors, re-measured on
disk); version alignment steps 1-4 edits landed (residual 26.7.13 engine edges cutover-coupled;
gates pending disk-recovery); publish-safety corrected to zero absolute sibling-repo paths with
only 4 in-workspace path-only deps blocking; ggen-cli nested test targets adjudicated
archive/DEAD/DEAD with 261-site harness migration and 6 targets fixed; 403-manifest strict
PackToml smoke = 0 refusals. Full evidence: "Final session deltas" section of
`docs/v26_10_10_phase1_receipt.md` (authoritative for DoD rows 15/19/22/25).

## Closing consistency pass (verifier V18, 2026-10-09)

- "praxis_*:: refs outside praxis crates: 76 refs / 16 files" (line 33) is stale: fresh grep of
  `praxis_core::|praxis_graphlaw::|praxis-core|praxis-graphlaw` in `crates/**.rs` excluding
  `crates/praxis*` = **56 refs / 12 src files** (ggen-engine, ggen-config, ggen-cli). The
  trajectory is downward as claimed; cite 56/12 as-of this pass.
- Census cross-check: 403 manifests, 332 unique names, 71 mirror pairs — agrees with
  pack_capabilities_guide.md; the phase1 receipt's 331/72 note is the outlier (corrected there).

### Post-session court results (appended 2026-10-09, lane docs-tripwire)

Two courts landed after the final-session delta above and supersede parts of it:

- **Cross-corpus tripwire court** (2/0,
  `crates/glen-marketplace/tests/cross_corpus_tripwire_test.rs`) FALSIFIED the
  composer's claimed DuplicateCapability refusal: mirrors dedupe by id and
  cross-corpus compose silently merge-collapses 403→332. The final-session
  delta's "refusal is design-intent tripwire; SJIRA-13 withdrawn" reading is
  superseded — no refusal exists to be design intent. Docstring fix in flight.
- **Parser differential court** (5/0 after fixes) + swallowed-requires repair:
  5 real missing-description corpus defects fixed (FM-PACK-003); 154 files
  repaired into `[capabilities]` — corpus now 403 manifests / 104
  in-capabilities requires / 0 dangling / 0 bare / 0 swallowed.

Full evidence: "V14 closure" section of `docs/v26_10_10_phase1_receipt.md`
(authoritative for these courts).

## Appended: docs-fold-2 lane receipts (2026-10-09)

Latest landed receipts folded; the two receipts above are superseded on these points:

- **Cross-corpus danglers fixed**: the 3 marketplace requires naming ggen/packs-only
  packs were deleted (claude-code→tcps-core, schema-pack→specimen,
  counterfactual→fortune5-required-capabilities). Re-verified on disk this append
  (`requires = []` in claude-code-pack and clap-noun-verb-schema-pack; no counterfactual
  refs). Gates post-fix: cross_corpus_tripwire **2/0**, capability_corpus **3/0**.
- **Swallowed-requires repair complete**: 154-file sweep moved stranded requires into
  `[capabilities]`; a2a-hex's docstring-trapped requires now visible, so FM-PACK-018
  fires correctly in the smoke suite's negative case. Corpus now **403 manifests / 104
  in-caps requires / 0 dangling / 0 bare / 0 stray-top-level**.
- **Smoke-declared Tier-1 positive case landed**: cargo-cicd→clap-noun-verb provider
  pair, **4/0** including the negative FM-PACK-018 case.
- **Composer e2e 16/0** (5 edge cases: self-require self-satisfaction pinned Ok;
  capabilities.requires not topology edges pinned; self-dup dedup pinned).
- **Compose verb order-determinism fix** at `crates/ggen-cli/src/cmds/pack.rs:1117-1128`
  (verified on disk).
- **ggen-lsp style sweep**: crate 0 warnings (doc-paragraph pre-allowed lib.rs:8);
  ggen-graph src 0 mechanical issues (31 unwrap in cfg(test)/sabotage bins only).
- **Lints**: ggen-mcp wired; cheat-scanner wired (19/0); ggen-engine intentional inline.
- **Toolchain incident resolved**: pinned nightly-2026-06-22 cargo/rustc/rust-std went
  missing mid-session (disk-full fallout); reinstalled via `rustup component add` by the
  smoke-declared lane.

DoD rows updated in `docs/v26_10_10_phase1_receipt.md`: FM-PACK-018 two-tier →
DONE+VERIFIED (V10/V12/V13 + smoke 4/0); DoD 25 capabilities → corpus data-quality DONE.

## Final closeout (2026-10-10, lane ggen-docs)

End-state verified by commands run at HEAD 8b03d25ad on main:

- **Workspace = 14 crates, praxis retired.** `grep -c '^  "crates/' Cargo.toml`
  → `13` member paths + root `ggen` package = 14;
  `cargo metadata --no-deps` → 14 packages, with `ggen:26.10.10`,
  `ggen-engine:26.10.10`, `ggen-abb-sbb:26.10.10`. Workspace `version` in
  `Cargo.toml:2` = `26.10.10`. No `praxis-core`/`praxis-graphlaw` members or
  path deps remain; graph backend is the sibling crate
  (`Cargo.toml:140`: `graphlaw = { path = "../graphlaw", version = "26.10.5" }`).
- **star-toml migration closed at 3 documented exceptions.**
  `grep -rn "toml::from_str" crates --include='*.rs' | grep -v star_toml` →
  exactly 3 sites, all ggen-lsp: `src/analyzers/toml_analyzer.rs:85` and
  `src/features/formatting.rs:86,196` (analyzer + formatter). 0 undocumented.
  (Earlier receipt states of 9→11 sites are superseded: exceptions were
  subsequently reduced to these 3.)
- **FM-PACK-018 two-tier landed**: `crates/ggen-engine/src/pack.rs:538`
  ("Two-tier satisfaction (FM-PACK-018 adjudication H2, 2026-10-09)").
- **abb-sbb wired as engine Stage 2c**: `ggen_abb_sbb::depgraph::*` +
  `ggen_abb_sbb::parse_graph`/`Request` call sites in
  `crates/ggen-engine/src/sync.rs:913-971`.
- **Quickstart verified**: `cargo run -p ggen-cli-lib --bin ggen -- sync run
  --help` exits 0, exposes `--dry-run` ("Resolve and render but do not write
  any files to disk"), reports `version="26.10.10"`. README quickstart form is
  current.
- **Publish chain abb-sbb → engine still pending**: user-gated crates.io
  credentials (release-cut execution); everything through dry-run is DONE per
  the phase1 receipt's final wave-2 fold.
- README.md corrected this lane: embedded version `26.10.8` → `26.10.10`.
  README makes no crate-count or praxis claim of its own, so no other edit
  was needed.
