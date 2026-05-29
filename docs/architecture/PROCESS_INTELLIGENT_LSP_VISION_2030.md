# Process-Intelligent Authoring Infrastructure — POWL · OCEL · SPARQL, and the LSP/MCP/A2A Pattern

**Status:** implemented (ggen v26.5.21) · **Date:** 2026-05-28
**Crates:** `ggen-graph`, `ggen-lsp`, `ggen-lsp-mcp` · **Tests:** 106 passing · clippy clean

> The product is CodeManufactory; RevOps is merely proof that CodeManufactory works.

---

## 1. Executive summary

ggen's Language Server stopped being a static type checker. It is now a **live operational nervous system** for coding agents: it observes what agents actually do to law-surface files, mines the dominant failure paths, and projects the *repair process* required to return an agent to lawful motion.

Three formal artifacts are fused through one substrate:

| Artifact | Role | Substrate |
|---|---|---|
| **OCEL** (Object-Centric Event Log) | what *actually happened* (observed agent edits) | RDF in oxigraph |
| **POWL** (Partially Ordered Workflow Language) | the *admissible* repair route (partial order + choice + loop) | RDF + Rust model |
| **SPARQL** | the *engine* — discovery, conformance, and constraints | oxigraph `SparqlEvaluator` |

The load-bearing decision: **the entire process layer runs as SPARQL over oxigraph.** ggen-graph already *is* an RDF triplestore, so OCEL events become triples, directly-follows discovery is a `CONSTRUCT`, lifecycle conformance is an `ASK`, and OCPQ-style constraints are `GROUP BY … HAVING` / `FILTER` / `NOT EXISTS`. No external process-mining engine, no dual event-log representation.

---

## 2. The fusion, concretely

### 2.1 OCEL — observed behavior as RDF

Every agent interaction with a law surface (`.ttl`, `.nt`, `.nq`, `.rq`, `.sparql`, `.tera`, `ggen.toml`) emits an object-centric event reusing ggen-graph's existing `OcelEvent`/`OcelObject` types — no new log was invented.

- **Activities:** `DiagnosticRaised`, `RepairSuggested`, `EditApplied`, `ValidationRerun`, `GatePassed`, `GateFailed`, `ReceiptEmitted`, `RefusalEmitted`.
- **Objects:** `agent`, `file`, `symbol`, `predicate`, `shape`, `diagnostic_code`, `repair_route`, `receipt`.
- **Storage:** append-only NDJSON at `.ggen/ocel/agent-edit-events.ocel.jsonl` (crash-safe: per-line flush, truncated tails skipped).
- **Code:** `crates/ggen-lsp/src/intel/{events,log}.rs`; captured by the headless gate (`CheckReport::capture`) so hooks accrue evidence on every `ggen lsp check`.
- **Projection to RDF:** `ggen-graph`'s `EvidenceProjector::project_ocel` emits `ocel:activity`, `ocel:timestamp`, `ocel:has_object_ref`, `ocel:qualifier_<role>`, `prov:startedAtTime`.

### 2.2 SPARQL — the mining + conformance engine

Directly-follows discovery (the failure-edge miner) is a single SPARQL query over the projected OCEL graph (`crates/ggen-graph/src/ocel/dfg.rs`):

```sparql
PREFIX ocel: <http://www.ocel-standard.org/ns#>
SELECT ?a1 ?a2 (COUNT(*) AS ?freq) WHERE {
  ?e1 ocel:activity ?a1 ; ocel:timestamp ?t1 ; <qualifier> ?case .
  ?e2 ocel:activity ?a2 ; ocel:timestamp ?t2 ; <qualifier> ?case .
  FILTER(?t1 < ?t2)
  FILTER NOT EXISTS { ?e3 ocel:timestamp ?t3 ; <qualifier> ?case .
                      FILTER(?t1 < ?t3 && ?t3 < ?t2) }
} GROUP BY ?a1 ?a2 ORDER BY DESC(?freq)
```

Repair-lifecycle conformance — "did the agent return to lawful motion?" — is an `ASK` over the same graph, object-correlated by the diagnostic episode (`crates/ggen-graph/src/ocel/conformance.rs`):

```sparql
ASK { ?d ocel:activity "DiagnosticRaised" ; ocel:timestamp ?t1 ; <q> ?case .
      ?r ocel:activity "RepairApplied"   ; ocel:timestamp ?t2 ; <q> ?case .
      ?g ocel:activity "GatePassed"      ; ocel:timestamp ?t3 ; <q> ?case .
      FILTER(?t1 < ?t2 && ?t2 < ?t3) }
```

OCPQ-style constraints (cardinality, temporal, existence) map directly to SPARQL — a `guard`/`loopCondition` on a route step *is* an inline `ASK` (`check_guard`). This is the OCPQ paper realized in oxigraph.

### 2.3 POWL — admissible repair routes

A repair route is a **separable POWL model**: a partial order of steps where independent fixes carry no edge (auto-concurrency) and real dependencies are edges (`crates/ggen-lsp/src/route/model.rs`). Soundness is the separable-workflow-net structural test — O(n) acyclicity + single-entry/reachable-exit — runnable on every route. Routes are **promoted from mined dominant edges**, not hand-guessed; seeded cold-start routes ship in the binary, mined routes land in the pack's `powl/`.

POWL is also expressible as RDF (`vocab/powl.rs`: `powl:Route/Step/Edge/…`), so routes are themselves SPARQL-queryable and conformance-checkable.

### 2.4 The performance split (why it's usable, not academic)

```
HOT PATH (sub-100ms, per edit):  diagnostics → route LOOKUP (2 hashmap probes) → Diagnostic.data
OFFLINE (ggen lsp mine):         OCEL → RDF → SPARQL DFG → rank failure edges → routes + report
```

Mining never runs on the edit hot path. The hot path only *looks up* precomputed routes. This is the difference between a usable LSP and a research toy.

---

## 3. The locked loop (what runs end-to-end)

```
agent edit
 → LSP diagnostic (E00XX law surfaced live)              [ggen-lsp analyzers]
 → OCEL event captured                                   [intel/, .ggen/ocel/*.jsonl]
 → ggen lsp mine: SPARQL DFG → ranked failure edges      [ggen-graph dfg.rs]
 → POWL repair route selected (precomputed)              [route/registry.rs]
 → CodeAction / RoutePlan / MCP tool projects the fix    [3 delivery channels]
 → gate re-run → SPARQL ASK conformance                  [ggen-graph conformance.rs]
 → RepairReceipt (BLAKE3) | RefusalEmitted               [intel/receipt.rs]
```

**Proven:** `ggen lsp check` captured 10 events → `ggen lsp mine` discovered `DiagnosticRaised → GateFailed` (freq 2) via SPARQL and wrote `error-edge-mining.md`. A real `[ai]` diagnostic → seeded route → `WorkspaceEdit` that deletes the offending span. `ggen lsp check --with_routes` returns the route + an 80/20 Pareto `route_summary`.

---

## 4. Three delivery channels — one engine

The same `ggen_lsp::RoutePlan` (one Rust type) reaches agents three ways:

1. **Editor CodeAction** — `textDocument/codeAction` returns a QUICKFIX `WorkspaceEdit` (`ggen-lsp/src/server.rs`).
2. **Headless JSON** — `ggen lsp check --with_routes` emits routes + Pareto summary for CI and hooks (`ggen-lsp/src/check.rs`).
3. **MCP tool** — `ggen.lsp.repair_route` for non-LSP agents (`ggen-lsp-mcp`, a leaf crate).

The MCP channel is a **leaf crate** (`ggen-lsp-mcp → ggen-lsp`, nothing depends back) — the cycle-free way to ship the tool, since `ggen-core → ggen-a2a-mcp` already exists and blocks adding it to `ggen-a2a-mcp`. Via `a2a-rs`/`a2a-mcp`'s `AgentToMcpBridge` it can additionally be exposed as an A2A agent.

---

## 5. Vision 2030

By 2030, software is not hand-typed against a syntax tree; it is *manufactured* against live law, and the authoring surface is the control plane.

- **Law at author time, not after.** Every ontology/config/template surface carries its constraints as SPARQL; the LSP refuses invalid law as it is typed (`v30.1.1`: no law-surface edit without a generated LSP + hooks enforcing the same canon). Defects are prevented, not detected.
- **Environments that learn.** The OCEL→SPARQL miner means each project discovers *its own* dominant failure families, repair efficiencies, livelock patterns, and WIP explosions. Repair routes are evidence-promoted, so the system gets better the more agents use it — the 80/20 edge table is the curriculum.
- **Receipts as the unit of trust.** Every admitted repair binds a BLAKE3 `RepairReceipt` (diagnostic → pre/post state → gate-pass); conformance is an `ASK`, not a vibe. Consequence is provable and replayable.
- **Agents receive routes, not scolding.** A diagnostic is a failed process transition; a CodeAction is the next admissible transition. The agent is handed a partially-ordered path back to lawful motion — what must happen first, what may run concurrently, what is exclusive, what loops, what terminal state proves closure.
- **Mechanical Intelligence.** Lawful retrieval (SPARQL/RDF) → bounded actuation (POWL route) → receipt → replay. POWL supplies the route geometry between diagnosis and actuation; without it, LSP feedback is just red squiggles.

The compression: **LSP = live type pressure · Hooks = admission gate · ggen = foundry · MCP/A2A = actuation · Receipts = proof.**

---

## 6. The reusable pattern for any project

This is not ggen-specific. Any project with "executable text" surfaces (schemas, IaC, configs, DSLs, prompts) can adopt the same stack:

### 6.1 One command

```bash
ggen lsp init            # writes editor configs (Helix, Neovim) + the Agent Admissibility Pack
ggen lsp start           # the language server (editors launch this)
ggen lsp check           # headless gate (hooks/CI) — scans ALL law surfaces, exits non-zero on ERROR
ggen lsp mine            # offline: discover the project's 80/20 failure edges
ggen lsp emit_pack       # regenerate the movable stewardship pack
ggen lsp-mcp             # MCP server exposing repair routes to non-LSP agents
```

### 6.2 The pack is the movable part

`ggen lsp init` emits `.agent-admissibility/`:

```
.agent-admissibility/
├── lsp/lsp-config.json        # law-surface globs + policy paths (shared canon)
├── hooks/<agent>/{pre-edit,post-edit,pre-commit,refusal}.sh   # call `ggen lsp check`
├── policies/*.shacl.ttl       # default SHACL (public-vocab, no-private-namespace, receipt-required)
├── powl/                      # repair routes (mined → promoted)
├── ocel/                      # event log + discovery reports
├── replay/                    # episode fixtures
└── receipts/                  # consequence proofs
```

The code stays fixed; the **pack travels**. A new repo gets the whole discipline — live diagnostics, an admission gate that refuses bad law before commit, default policies, and an evidence loop — by installing one pack.

### 6.3 The LSP / MCP / A2A triad

| Layer | Who it serves | How |
|---|---|---|
| **LSP** | humans + LSP-aware agents in editors | live diagnostics + CodeActions (process transitions) |
| **MCP** | tool-using agents (Claude, etc.) | `ggen.lsp.repair_route` tool returning POWL `RoutePlan`s |
| **A2A** | autonomous multi-agent fleets | bridge the MCP tool into an A2A agent (`a2a-mcp::AgentToMcpBridge`) |

All three are projections of **one route engine** over **one SPARQL-mined process model**. Adopt the pattern by: (1) modeling your surfaces' constraints as SPARQL, (2) emitting OCEL on agent edits, (3) mining failure edges offline, (4) promoting routes, (5) projecting them through whichever channel your agents speak.

---

## 7. Map (where everything lives)

| Concern | Path |
|---|---|
| OCEL event schema + capture | `crates/ggen-lsp/src/intel/{events,log,receipt}.rs` |
| Offline miner (`ggen lsp mine`) | `crates/ggen-lsp/src/intel/mine.rs` |
| SPARQL DFG discovery | `crates/ggen-graph/src/ocel/dfg.rs` |
| SPARQL conformance / OCPQ guards | `crates/ggen-graph/src/ocel/conformance.rs` |
| POWL + DFG RDF vocab | `crates/ggen-graph/src/vocab/{powl,dfg}.rs` |
| Repair-route engine (POWL, soundness, registry) | `crates/ggen-lsp/src/route/{model,registry,plan,edit}.rs` |
| CodeAction projection | `crates/ggen-lsp/src/server.rs` |
| Headless `--with_routes` + capture | `crates/ggen-lsp/src/check.rs` |
| One-command setup (`ggen lsp init`) | `crates/ggen-lsp/src/init.rs` |
| Admissibility-pack emitter | `crates/ggen-lsp/src/pack/mod.rs` |
| MCP tool (leaf, cycle-free) | `crates/ggen-lsp-mcp/` |

---

## 8. IMPROVE-1 — measuring improvement (status: observation layer complete, claim refused by default)

The flywheel is closed (PROMOTE-1). IMPROVE-1 adds the **measurement** layer under a hard gate: *no event → no metric · no receipt → no proof · no replay → no claim.*

- **Event chain captured:** `DiagnosticRaised → RouteSelected(route_id, route_source) → RepairSuggested → GatePassed|GateFailed → ReceiptEmitted|RefusalEmitted`, with bindings `case_id, run_id, file, diagnostic_code, span, route_id, route_source, receipt_id`. `RepairApplied` is editor-only and is **not faked** — apply-dependent metrics report `insufficient_evidence` when it is absent.
- **Promotion history ledger** (`.ggen/ocel/promotion-history.jsonl`): `RoutePromotionRecord` with `status ∈ {Active, Demoted, Superseded, Quarantined}` and `supersedes`, one record per route per mine cycle → survival/churn/displacement are measurable.
- **Metrics** (`ggen lsp metrics`): `route_hit_rate`, `promoted_route_hit_rate`, `seed_displacement_rate`, `repair_cycle_time`, `gate_pass_rate_by_source`, `repeat_failure_rate`, `promotion_survival_rate`, `promotion_churn`, `receipt_density`. Each cites its backing events; **a metric lacking its events returns `insufficient_evidence`, never a number.**
- **Earned verdict:** `improving` only when ≥2 cycles show *rising measured success* and mined routes actually pass; otherwise `insufficient_evidence` (the default). The tool refuses the claim in code.
- **Replay** (`ggen lsp replay [--case <id>]`): reconstructs an episode (events, route, source, gate outcome, conformance, receipt) and verifies the promotion binding `source_log_hash → routes_hash` against the receipt — **tampering the log breaks the match.**
- **Cross-channel parity** (test-proven): editor (`action_route_for`), headless (`route_plan_for_diagnostic`), and MCP agree on route id for the same diagnostic — both when a mined route is promotable (mined wins) and when it is sub-threshold (seed holds).

**Honest claim today:** *"The system can measure whether evidence-promoted routes improve future authoring outcomes, with replayable per-episode proof and cross-channel route parity."* **Not yet:** *"It improves itself"* — that requires accumulated real runs where the verdict turns `improving` from real usage, not a fixture. Until then the tool prints `insufficient_evidence`.

---

## 9. The seven Gall checkpoints — operated, not just built (2026-05-28)

The apparatus has now been **operated** on real cycles, each checkpoint a smaller
receipted loop proven before the next (the Gall rule). All harnesses drive the real
`check → capture → mine → metrics → replay` path over real files — no hand-written
OCEL events. A key discovery reordered the work: `success_rate` is severity-pinned
and the log append-only, so `improving`/demotion can only move via **rework-closure**
(`DiagnosticRaised → fix applied → GatePassed` in one episode) — which is APPLY-1.
APPLY-1 therefore precedes IMPROVE-RESULT-1 and DEMOTE-1.

| Checkpoint | Earned claim |
|---|---|
| **OPERATE-1** | One real authoring cycle emits the full receipted chain; the verdict correctly refuses on a single cycle. |
| **APPLY-1** | The editor flow observes *applied* repairs (`RepairApplied → GatePassed → ReceiptEmitted`), not only proposed routes — the rework-closure foundation. |
| **IMPROVE-RESULT-1** | The verdict returns `improving` from real cycles when promoted routes raise closure — and is refused (with 2 cycles + a passing mined route) when closure does not rise. |
| **DEMOTE-1** | A route that stops conforming is Demoted (churn>0, survival<1) and the registry resumes the seed — trust is withdrawn under evidence. |
| **CPMP-PACK-1** | A capability scan manufactures the pack with a replayable, tamper-evident `scan_hash → pack_hash` receipt. |
| **PORTABLE-PACK-1** | A route pack resolves the same family in a different repo, with independent receipts and no path coupling — interchangeable parts. |
| **MULTI-AGENT-1** | Multiple agents share one route law without drift; each episode is separable, attributed, receipted, and independently replayable. |

**Honest ceiling (unchanged):** the apparatus has *operated on real authoring cycles
and returned earned/refused evidence verdicts*, with route lifecycle, portability, and
multi-agent parity proven by real execution. Still **not** "it improves itself" — the
`improving` verdict is earned from controlled real cycles (fixtures producing real
analyzer diagnostics), not yet from accumulated real-world usage.

---

## 10. The delivery plane — one route contract across LSP / MCP / A2A (2026-05-28)

Eight checkpoints proved that *every agent, regardless of transport, receives the same
admissible route, proves the same episode, and replays the same consequence* — channel
differences are transport only, never logic. All proven by real execution.

| Checkpoint | Earned claim |
|---|---|
| **TRIAD-CONTRACT-1** | One `RouteEnvelope` (composing `RoutePlan` + `CompactTraceView`, one canonical `case_id`) is byte-equivalent across headless, LSP `code_action` `data`, and the MCP tool; one shared `RouteRefusal`. |
| **MCP-HARDEN-1** | `ggen-lsp-mcp` is root-aware, schemars-typed, input-bounded (1 MiB), and refuses structurally — never panics. |
| **MCP-REPLAY-1** | MCP exposes `replay_case` + `metrics` (byte-equal to the direct `ggen-lsp` calls) — route + proof + replay, not vending. |
| **A2A-BRIDGE-1** | New leaf crate `ggen-lsp-a2a` exposes the tools as an A2A `Adapter`; A2A result == MCP result; `cargo tree` confirms `ggen-a2a-mcp ↛ ggen-lsp` (cycle-free). |
| **SESSION-ATTRIBUTION-1** | Every capture carries `agent_id` + `transport` (`lsp\|mcp\|a2a\|headless`) + `session_id`; episodes stay separable and replay with provenance. |
| **REMOTE-PACK-1** | An advertised `PackManifest` (routes + hashed policies + law surfaces, bound to `pack_hash`, version/canon staleness-guarded, no path coupling) makes the pack a movable route-law part; route responses bind the served `pack_hash`. |
| **TRIAD-STRESS-1** | Under concurrent LSP+MCP+A2A load the OCEL log stays uncorrupted (every line parses, no events lost), routes do not drift, and cases replay. |
| **MARKETPLACE-PACK-1** | `ggen lsp init` installs MCP + editor LSP + hooks in one command; `ggen lsp serve --protocol lsp\|mcp`; install → route → apply → receipt works end-to-end. |

**Earned:** *every agent — editor (LSP), non-editor (MCP), or remote (A2A) — receives the
same admissible route, proves the same episode, and replays the same consequence, under
concurrent pressure, installed as a movable pack.* The honest ceiling is unchanged: the
apparatus operates on real cycles and returns earned/refused verdicts; it is **not**
"self-improving" (the `improving` verdict still awaits accumulated real-world usage).

---

*Grounded in real code and execution; no fabricated traces. Papers fused: OCPQ (object-centric process querying & constraints), OCED+SPARQL (object-centric analysis of XES via SPARQL), Hierarchical Decomposition of Separable Workflow-Nets (POWL soundness + auto-concurrency).*
