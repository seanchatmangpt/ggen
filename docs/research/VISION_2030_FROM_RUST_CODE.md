# The Point of ggen, and Vision 2030 — the Chatman Convergence, Verified Against the Rust Code

**Method.** This analysis was produced in three passes. First, a Rust-code-only survey of the 15
compiled workspace crates and the 5 dormant crate directories under `crates/` — no markdown, no
`docs/`, no README, no TTL files; only `.rs` source (doc comments included, since they are part
of the code). Second, the survey was reconciled against the project's converged doctrine (the
"Conversation Convergence" SPR): the **post-Rice mission operating system** governed by the
**Chatman Equation `A = μ(O*)`**. Third, the sibling constellation was researched from its
published crates.io source tarballs — `wasm4pm-compat` 26.6.28, `wasm4pm` 26.6.25,
`bcinr`/`bcinr-logic`/`bcinr-api` 26.4.22, `cargo-cicd` 26.6.30 — again Rust source only, plus
the in-repo `ontology_catalogue/` projections that bind those projects into ggen's admitted
world. Every doctrine concept below is either anchored to real files/types found in source, or
honestly flagged as absent — claims of absence are backed by zero-hit searches over the
monorepo **and** the fetched sibling sources.

---

## Part 1 — The Convergence: What This Project Actually Is

### Not the smaller things it resembles

From the code alone, ggen looks like a very rigorous code generator. That reading is true but
undersized. It is not "AI tools," not RevOps, not a scaffolding utility, and not a claim that
everything is reducible. The converged claim is:

> **Everything operational can be abstracted into a discrete, deterministic working model at
> sufficient resolution for action** — and this codebase is the machinery for doing that
> lawfully: a post-Rice **mission operating system**.

### The Chatman Equation, as implemented

```
A = μ(O*)          R = receipt(A)          BRCE = bounded receipted execution
```

- **O** — raw, open observation. The default world is semantically open and cannot be directly
  planned (Rice's theorem: arbitrary semantics cannot be trusted as executable meaning).
- **O\*** — the *admitted, bounded, verified* slice of observation. Authority begins here — not
  at observation, not at tool output, not at LLM proposal.
- **μ** — the lawful manufacturing function. This is ggen itself.
- **A** — artifact / action / authority / consequence.
- **R** — the receipt that makes A claimable.

This is not a retrofitted narrative — the equation is native to the code. The graph crate's
coherence module cites "the post-Chatman equation A ≅ O ≅ L" verbatim
(`crates/ggen-graph/src/coherence.rs`), the sync pipeline mounts a FATAL
`CoherenceGate` (`crates/ggen-core/src/sync/coherence_gate.rs`), and the test suite carries the
name structurally: `crates/ggen-graph/tests/post_chatman_coherence_integration.rs`,
`crates/ggen-graph/tests/post_chatman_roundtrip_full_test.rs`,
`crates/genesis-types-v2/tests/post_chatman_powl_integration.rs`,
`crates/genesis-types-v2/tests/post_chatman_residual_integration.rs`.

**The O → O\* boundary is what most of the codebase is.** The star on O is implemented as
admission machinery at every layer:

| Admission surface | Code |
|---|---|
| Packet admission gates (`AdmissionVerdict::{Admit, Refuse}`) | `crates/genesis-construct8/src/admission.rs` — `AdmissionPredicate`, `StructuralAdmissionGate`, `LawAdmissionGate`, `GatedSegmentBuilder` |
| Process admission | `crates/genesis-types-v2/src/lib.rs` — `ProcessAdmissionReport`, `GateResult`, `AdmissionStatus::{Alive, PartialAlive, Refused, Unknown}` with BLAKE3 `receipt_hash` |
| The only legal boundary crossing | `crates/ggen-membrane/src/lib.rs` — `GenesisAdapter` ("ggen owns contact, Genesis owns consequence, the adapter is the ONLY legal crossing, every crossing produces a receipt, every failure is a Refusal") |
| Manifest-time admission (strict mode, SHACL, ASK guards) | `crates/ggen-core/src/manifest/types.rs` — `[[validation]]` shapes, `ValidationRule{ask, severity}`, `strict_mode` |
| Author-time admission (law surfaces) | `crates/ggen-lsp/src/analyzers/` — `GGEN-TPL-001`, `GGEN-OUT-001`, `GGEN-YIELD-001`, `E0011`/`E0013` |
| Trust admission for supply-chain inputs | `crates/ggen-marketplace/src/marketplace/trust.rs` — `TrustTier` (Blocked satisfies nothing, ever), `RegistryClass` |
| Agent-work admission | `crates/ggen-a2a-mcp/src/a2a/receipt.rs` — `A2ATaskReceipt::verify()` fail-closing into 16 refusal variants incl. `SyntheticTaskClosure` |

Everything upstream of these gates is O. Everything downstream is O\*, and only O\* actuates.

### Rice Quarantine, as implemented

The doctrine — do not model the whole world; model the admitted lawful slice; everything outside
has no operational standing; the system *refuses, quarantines, or abstracts* rather than crying
over inadmissible state — is executable behavior in the code:

- **Refuses:** typed `Refusal { reason: RefusalReason, .. }` in the no_std kernel
  (`crates/genesis-core/src/lib.rs`); `RouteRefusal` in the LSP route engine so that an empty
  route list is never read as "all clear" (`crates/ggen-lsp-mcp/src/lib.rs`); `HierarchyRefusal`
  in the corpus builder; `ConsentRefused` with receipt evidence in stpnt.
- **Quarantines:** the promotion ledger in `crates/ggen-lsp/src/intel/history.rs` carries the
  literal status set `Active / Demoted / Quarantined / Superseded` — mined knowledge that loses
  evidentiary standing is quarantined, not deleted and not argued with.
- **Abstracts:** the entire μ pipeline is the abstraction move — the world enters only as an RDF
  ontology bounded by a manifest (`GgenManifest`), never as raw semantics.

And the honesty backstop: `crates/ggen-lsp/src/intel/metrics.rs` refuses to compute over
inadmissible state — `MetricValue::InsufficientEvidence`, "no event → no metric, never a
fabricated number." Unknown outside scope is not a problem to lament; it is simply not standing.

---

## Part 2 — Doctrine → Code Map

Each pillar of the convergence, and where it already lives in `.rs` source.

### μ — the manufacturing function (ggen proper)

Ontology in; representation out. The primary engine is
`crates/ggen-core/src/codegen/pipeline.rs` (`GenerationPipeline`): load ontology into Oxigraph →
SPARQL CONSTRUCT inference materialized back into the graph → SPARQL SELECT rows through Tera
templates → transactional file writes (`GenerationMode::{Create, Overwrite, Merge}`). The CLI
collapsed onto the single golden path `ggen sync` (`crates/ggen-cli/src/cmds/mod.rs`), emitting
Ed25519-signed receipts chained over the full input closure
(`cmds/sync.rs::emit_sync_receipt`) — and dry-runs deliberately emit nothing: "a preview must
record no consequence."

What μ manufactures today, from code: **Rust/Go/Elixir/TypeScript/Python sources**
(`SyncLanguage` in `crates/ggen-core/src/sync/mod.rs`), **its own agent-protocol types**
(`crates/ggen-a2a-mcp/src/a2a_generated/` — "Generated by ggen from A2A ontology"), **MCP
capability surfaces** (`mcp_packs.rs` tools shared byte-identically across MCP and A2A
transports), **OCEL logs and receipts** (`crates/ggen-graph/src/ocel/`), and **tests, fixtures,
diagnostics packs** (`crates/ggen-lsp/src/pack/mod.rs` — the hook foundry with hash-bound
`scan_hash → pack_hash` CPMP-PACK-1 receipts). What it does not yet manufacture: PDDL operators
(see the gap map).

### BRCE — bounded receipted execution

Every layer implements transition-with-receipt-or-refusal:

- Kernel: `Receipt::generate(act, prev)` BLAKE3 chain, `ReplayCursor::advance` refusing
  `OutOfOrderEpoch`/`ReceiptMismatch` (`crates/genesis-core/src/lib.rs`, compile-time asserted
  32-byte `Construct8`).
- Graph: `GraphReceipt` binding `pre_state_hash / post_state_hash / delta_hash`, `KnowledgeHook`
  SPARQL constraints with rollback, `ReplayVerifier` enforcing linear-history chain continuity
  (`crates/ggen-graph/src/receipt/mod.rs`, `graph/dataset.rs`).
- Sync: signed chained receipts over the O\* closure (actuator identity + manifest + ontology +
  imports + external queries/templates, `MISSING` recorded for unreadable inputs).
- Agents: `A2ATaskReceipt` binding personas, jobs-to-be-done, MCP invocation evidence, and
  expected-vs-observed OCEL path hashes.
- Humans: `StewardshipReceipt` with keyed-BLAKE3 MAC; unsigned receipts fail `verify()`
  (`crates/stpnt/src/proof/receipt.rs`).

**Cache stores standing, not output.** `.ggen/packs.lock` plus the signed provenance receipt on
install (`crates/ggen-a2a-mcp/src/mcp_packs.rs`) is exactly this: a cache hit is the reuse of a
prior admitted receipt, re-verified at `--locked` time; it is never a bypass of admission.

### Capability Physics — capability as admitted object moving through law

The doctrine requires capability to carry state, authority, preconditions, effects, resources,
constraints, receipt, replay — and requires the loop *tool call → quarantine → admission →
plan → execution → receipt → replay → promotion/refusal*. The code implements the loop
end-to-end in the LSP intel subsystem (`crates/ggen-lsp/src/intel/`): every diagnostic episode
is OCEL-logged (`DiagnosticRaised → RouteSelected → GatePassed/GateFailed →
ReceiptEmitted/RefusalEmitted`), attributed per agent and transport (lsp | mcp | a2a |
headless); `ggen lsp mine` projects the log to RDF, discovers the directly-follows graph, and
promotes mined repair routes **only** on measured support + success thresholds, with a
promotion receipt and replay verification ("if replay cannot reconstruct the claim, the claim
is not done" — `intel/replay.rs`). The MCP+ membrane exists as the pack-tool surface: ten
`ggen.packs.*` tools, each a pure function shared across transports, each invocation emitting
an OCEL boundary event. Capability discovery/verification is a first-class CLI noun
(`agent: capabilities, search, resolve, compatibility, verify, install` in
`crates/ggen-cli/src/cmds/`, backed by `ggen_core::agent::PackAgent`).

### Gall Foundations — inherited constants, not invented gravity

The engineered foundations are present in the stack the code chooses rather than argues for:
**Rust as local deterministic law** (the entire workspace; `ggen-graph/src/lib.rs` denies
`unwrap/expect/panic/todo/unimplemented` at the crate level), **Erlang/OTP as distributed
supervision law** (Elixir is a first-class generation target — `SyncLanguage::Elixir` with
`generate_elixir` in `crates/ggen-core/src/sync/mod.rs`), and **AtomVM as the edge bridge**
(`AtomVM`, `Erlang`, `WASM` are entries in cpmp's capability vocabulary —
`crates/cpmp/src/capability.rs` — i.e., the self-scanner is explicitly looking for where these
foundations live in the repo; the dormant `crates/genesis-wasm-shell/` runs the kernel in the
browser). The physical foundations (light-speed propagation bounds, causal cones) appear in the
code as their software shadows: `NoRetrocausationCheck` in the Σ constitution
(`crates/ggen-core/src/ontology/`), temporal-order refusals (`OutOfOrderEpoch`), and
lifecycle-order conformance checks (`check_lifecycle_order` in `crates/ggen-graph/src/ocel/`).

### PDDL / POWL — action law and execution geometry

The doctrine splits planning into PDDL (what actions are possible, preconditions, effects,
resources) and POWL (what runs concurrently, what orders, what synchronizes, what receipts gate
promotion). **The POWL half is built; the PDDL half is not.**

- **POWL present:** `PowlNode`/`PowlEdge`/`PowlGraph` with `validate()` in
  `crates/genesis-types-v2/src/lib.rs`; the LSP route engine is documented as POWL-native — "a
  Diagnostic is the observable trace of a failed process transition; a CodeAction is the
  transition that repairs it" (`crates/ggen-lsp/src/route/`); the agent-admissibility pack
  ships a `powl/` stewardship directory; the workflow engine (`genesis-core-v2`) implements the
  `Pattern` trait against Van der Aalst's 43-pattern YAWL taxonomy (3 built:
  Sequence, ParallelSplit, ExclusiveChoice).
- **PDDL absent from the monorepo — but present in the constellation:** `PDDL` matches
  **zero** `.rs` files in this repository. `ORTAC` matches zero. "Fluent" appears only in the
  fluent-builder-API sense. Inside ggen, preconditions and effects exist only implicitly — as
  admission gates and receipts around transitions. However, the sibling crate
  **wasm4pm-compat** (26.6.28) ships `src/pddl.rs` — "PDDL8 canonical types": a bounded STRIPS
  subset (`Pddl8Atom`, `Pddl8ActionSchema`, `Pddl8Domain`, `Pddl8Problem`,
  `Pddl8GroundAction::is_applicable`, arity/params/conjuncts capped at 8, plan depth ≤ 64,
  ground cap 4096), a POWL-geometry projection (`Pddl8Tape`/`Pddl8TapeOp`), and receipted
  execution (`Pddl8ExecutionLog`, `Pddl8ExecutionReceipt` with BLAKE3 plan/state/goal roots).
  Under BRCE, delete effects become "epoch seals" and `ExecutionTrace = OCEL 2.0`. What
  remains absent *everywhere we can see* is a **planner engine** — no solver searches these
  schemas — and numeric fluents (PDDL8 is boolean STRIPS). See Part 4.

The action-law *shapes* therefore exist in the constellation; the *search* over them is the
remaining gap (see Part 5).

### Mission Physics — same substrate, different objective fluents

The doctrine: institutions differ by objective function, not planning mathematics. The code
already proves the invariance with one non-commercial instantiation carried to completion —
**stpnt** ("Stewards of the Pentecost"): a church-operations domain where the mission variables
are welcomes, follow-ups, consent, incorporation; where every action must cite a canon basis
(`trait CanonBasis`, `crates/stpnt/src/canon/mod.rs`); where consent is a hard admission gate
(`ConsentGatePart` → `ConsentRefused` with PROV receipt); where obligations have deadlines and
`Overdue` is a refused terminal state (`crates/ggen-core/src/stpnt/obligation.rs`); and where
institutional failure is a computed KPI — `SlrAdjudicator::calculate_slr()`, the **Silent Loss
Rate** = orphaned obligations / welcomed persons (`crates/stpnt/src/governance/slr.rs`). Change
the ontology and the objective fluents, keep the substrate: that is mission physics, and the
codebase runs it against a church before it runs it against a sales team. The `Avatar8`/`Jtbd8`
persona-and-job bindings in `crates/ggen-a2a-mcp/src/a2a/receipt.rs` are the same move on the
agent-work axis.

**Revenue Physics is the designated first commercial specialization and is not yet built.**
There is no revenue ontology, no `MaximumReachableRevenue`, no `RevenueUtilization`, no revenue
DPMO in any `.rs` file. What exists is everything such a specialization would sit on: numeric
state transition with receipts, typestate lifecycles with guards
(`Draft → Published → {Deprecated, Yanked}` with `TransitionGuard`s in
`crates/ggen-marketplace/src/marketplace/rdf/state_machine.rs` — a template for
`opportunity may enter procurement only with security evidence, legal readiness, executive
sponsor, delivery capacity`), and trust/profile policy enforcement
(`regulated_finance_profile` in `marketplace/profile.rs` already encodes
regulated-industry constraints).

### TPS / DfLSS — numeric flow and defect control over mission transitions

The upgrade path the doctrine describes has its foundations compiled in:
`crates/ggen-core/src/` contains dedicated modules `dflss`, `lean_six_sigma`, `poka_yoke`,
`manufacturing`, and `metrics` with `OEEMetrics`, `KaizenMetrics`, `WasteType`, `FlowMetrics`.
Poka-yoke exists as typestate (`Open`/`Closed`, `NonEmptyPath` markers) and as an explicit
marketplace module (`marketplace/poka_yoke.rs`, `fmea_mitigations.rs`). Jidoka — automatic
refusal before bad consequence — is the pervasive Refusal machinery itself; Andon is the
diagnostic surface (`GGEN-*` codes halting sync); Kaizen-as-promoted-receipted-improvement is
literally the mined-route promotion loop with its `PromotionHistory` ledger. What is *not* yet
wired: DPMO computed over mission-state transitions (revenue DPMO, ministry DPMO). The event
substrate for it exists — OCEL logs count transition opportunities and `GateFailed` events —
but no code divides the two and multiplies by a million.

### Combinatorial Maximalism — maximize inside the admitted surface only

This is the doctrine's least-anchored pillar, and honesty requires saying so. The code enforces
the *boundary* half everywhere (nothing outside admission has standing) and implements
enumerate-evaluate-promote in one domain: route mining enumerates candidate repairs from logs,
filters refused/unsupported ones, and promotes only receipted winners
(`crates/ggen-lsp/src/intel/mine.rs`). The pattern registries
(`CorePatternRegistry`, `DashMap`-backed in `crates/genesis-core-v2/src/lib.rs`) and the
capability-combination surface (`pack resolve` / `compatibility` verbs) are the scaffolding for
lawful-combination enumeration. But a general "enumerate all lawful capability combinations,
evaluate the frontier" engine does not exist yet as code.

### Anti-Complaint Doctrine — spilled milk as an operational Rice principle

Complaint is computation over inadmissible or non-actionable state; the code refuses to do it.
Past irreversible state has execution standing only insofar as it changes the next admissible
action — which is exactly the shape of the residual-repair controller in
`crates/genesis-types-v2/src/lib.rs`: `ResidualVector` measures the gap, a
`BoundedRepairOperator` applies a *bounded* correction, `VisualGapReport::assert_fresh()`
rejects stale evidence, and `RepairAdmissionReport` admits the repair only if the residual
improved. Defect → admitted observation → corrective action → new rule → receipt → promotion is
the mined-route lifecycle verbatim. The strong tier — *convert the spill into a system
upgrade* — is the whole intel loop: failures are not lamented, they are mined into promoted
routes with receipts.

---

## Part 3 — Vision 2030: The Mission Operating System

The phrase "Vision 2030" already lives in source (`crates/cpmp/src/tier.rs` — the tiered
offline-first ontology registry; `crates/genesis-construct8/src/hierarchy.rs` — "Vision 2030
A = μ(O) construction"). Reconciled with the convergence, the 2030 end-state is:

**Any mission-bearing institution — company, church, unit, university, hospital, agency —
operates as a deterministic planning surface. Its relevant state, actions, resources,
constraints, authority, and consequences are discretized at sufficient abstraction, bounded by
admission, and executed under receipts and replay. Same planning mathematics everywhere; only
the ontology and the objective fluents change.**

The trajectories, each grounded in what the code has already committed to:

1. **Ontology defines the work; μ manufactures the representation.** From one mission ontology,
   ggen emits Rust types, **PDDL operators** (the to-be-built action law), POWL execution
   structures, MCP+ capability surfaces, OCEL/receipt schemas, and tests/fixtures/diagnostics —
   the multi-output pattern already proven by the self-hosted `a2a_generated` layer and the
   LSP's manufactured hook packs. Rust executes the law; OTP supervises life (the
   Elixir/AtomVM backends); BRCE receipts consequence.

2. **Operator languages, not planner languages.** The ORTAC+ pattern — field-user DSL with
   mission-native predicates translating down to PDDL — is replicated per institution: revenue
   operators write revenue missions (account graph as geography, evidence and sponsors as
   support predicates), ministry operators write care missions, and "sales judgment" becomes
   executable law the way the marketplace's `TransitionGuard`s already make publish-judgment
   executable. Nobody writes PDDL by hand; μ manufactures it from the ontology.

3. **Revenue Physics as the first commercial specialization.** Revenue is already numeric, so
   it is the ideal first PDDL-numeric-planning domain: predicates, numeric fluents, actions
   with resource effects, authority, receipts. Maximum Reachable Revenue becomes an *upper
   bound computed over the current capability graph*; Revenue Utilization = actual / maximum;
   Revenue Opportunity = the difference. RevOps reports; Revenue Physics computes. The Business
   Rice Boundary is respected: unplanned revenue is not optimized, only discussed — crossing
   the boundary requires bounded objects, admissible actions, POWL execution, receipts.

4. **DPMO over mission transitions.** TPS/DfLSS complete their upgrade from factory metaphor to
   computed control: waste = action that moved no admitted mission state; defect = intended
   transition that failed, delayed, reversed, or could not be receipted; DPMO computed per
   domain (revenue, ministry, mission) from the OCEL logs that already count opportunities and
   gate failures. Andon, Jidoka, and Kaizen are already code (diagnostics, refusals, promotion
   receipts); the metrics close the loop.

5. **Constitutional autonomous evolution.** The feature-gated Σ system
   (`AutonomousControlLoop`, `RealLLMProposer`, `Constitution` with
   `NoRetrocausationCheck`/`ProjectionDeterminismCheck`/`SLOPreservationCheck`) runs closed:
   LLMs *propose* ontology deltas — and per the doctrine, an LLM proposal is not authority;
   admission is. The constitution, SHACL gates, and the coherence audit decide; promoted
   snapshots carry `SigmaReceipt`s.

6. **Trust as a network property.** The dormant lockchain (`crates/genesis-lockchain/` — Merkle
   trees over receipts, `QuorumManager` threshold verification) plus the marketplace's trust
   tiers and air-gapped registry classes make receipt roots quorum-attestable across
   organizational boundaries. The dormant WASM shell makes replay verification ambient — a
   browser or an air-gapped auditor replays a Segment→Shard→Corpus receipt chain without
   trusting anyone's build farm.

7. **The system audits itself with the laws it imposes.** cpmp keeps scanning the repo,
   classifying every artifact under the non-deletion doctrine
   (`deletion_allowed()` hard-coded `false` in `crates/genesis-core-v2/src/inventory.rs`),
   refusing silently vanished capability, and binding scans to packs by receipt. Dormant is a
   status, not a grave; promotion requires passing the gates.

**Core theorem, as the code is betting on it:** any mission-bearing system can be operated as a
deterministic planning surface when its relevant state, actions, resources, constraints,
authority, and consequences are discretized at a sufficient abstraction level and bounded by
admission, receipts, and replay. The codebase is the constructive proof-in-progress: the
admission/receipt/replay bounding is finished and hardened; the planning surface (PDDL + fluents
+ operator DSLs) is the remaining construction.

---

## Part 4 — The Constellation: Where the Answers Live

The monorepo's own code names its siblings: `crates/genesis-core-v2/src/revelation.rs`
enumerates seven project "churches," including `Wasm4pm` and `BcinrSubstrate`
("BCINR/unibit/Field8/INSA — physical bounded-motion substrate"), and
`ontology_catalogue/` holds TTL/SPARQL projections of `wasm4pm`, `wasm4pm-compat`, and
`cargo-cicd`. Fetching the published crates.io sources of those siblings closes most of what
Part 2 could only mark "absent from this repo." Each sibling repeats ggen's split calculus at
its own layer: keep the emission/shape surface, delegate the heavy half elsewhere, and gate
every boundary with admission + receipt.

### wasm4pm-compat 26.6.28 — the law and shape authority

A nightly, `#![forbid(unsafe_code)]`, structure-only crate whose `lib.rs` states it contains
"no engines: no discovery, no conformance checking, no replay." What it does contain is the
doctrine, literally typed:

- **The one-way evidence door.** `Evidence<T, State, W>` (`src/evidence.rs`) with typestate
  tokens `Raw → Parsed → Admitted → {Projected | Exportable | Receipted}` and terminal
  `Refused`; the only public path to `Admitted` is `admission::Admit::admit` — the
  constructor is `pub(crate)`. Sealed admissions chain through BLAKE3
  (`SealedAdmission`, `ChainProof`, `recompute_and_match`).
- **Paper-complete is literal.** `src/witness.rs` + generated `witnesses*.rs` define
  zero-sized witness marker types per academic standard/paper — `Ocel20` (2023), `Xes1849`
  (IEEE 1849-2016), `PowlPaper` (2023), `YawlPaper` (van der Aalst & ter Hofstede),
  `SeparableWfNetPaper` (Kourani et al. 2026) — and `Admission<T, W>` / `Refusal<R, W>` are
  parameterized by them, so an admission under one paper's law is a *different type* than
  under another's. `witness_corpus.rs` enforces compile-time key uniqueness across the whole
  bibliography. Named laws are enforced, not cited: `OcelRefusal::ViolatesLocalityPrinciple`
  with `validate_gianola_2026_locality(...)` in `src/ocel.rs`.
- **The API ggen delegates to is exactly as declared.** `src/dfg.rs` has
  `discover_ocel_dfg`, `extract_ocel_variants`, `dfg_fitness`, `dfg_precision`; `src/ocel.rs`
  owns `OCEL`/`OCELEvent`/`OCELObject`; `src/models.rs` owns `DFG`/`DFGNode`/`DFGEdge` plus
  structural-only Petri metrics that are "computed only over the net's own shape … never
  against event data."
- **PDDL8** (`src/pddl.rs`) — the bounded action calculus described in Part 2, with receipted
  execution types. The `witnesses*.rs`/`fresh_names.rs` modules are marked "Generated by
  `ggen sync --rule …`" — ggen's press manufactures compat's witness surface from TTL.
- **Graduation is code, not metaphor.** `src/engine_bridge.rs` defines
  `GraduationReason` (`NeedsDiscovery`, `NeedsConformanceExecution`, `NeedsReplay`,
  `NeedsReceipts`, … `#[non_exhaustive]`) and `GraduationCandidate::is_grounded()`; the
  receiving half is `wasm4pm/src/graduation.rs::intake_candidate`, which admits grounded
  candidates for execution. "Start with compatibility. Graduate to execution" is a typed
  one-way bridge between two crates.

### wasm4pm 26.6.25 — the execution oracle

The engine compat refuses to be: 431 `#[wasm_bindgen]` exports across 107 files, handle-based
state over the WASM boundary. Discovery (alpha++/inductive/ILP/genetic/ant-colony/streaming
variants, object-centric `discover_ocdfg_wasm`/`discover_oc_petri_net`), conformance (A*
alignments with marking equation, SIMD token replay, ET-conformance precision, a
`conformance_authority/` module with admission gates), enhancement (next-activity/outcome/
remaining-time prediction, drift detection, LinUCB, Q-Learning/SARSA, SPC Western-Electric
rules), and a MAPE-K autonomic loop (`autoprocess.rs`, `autonomic_execute_cycle`). Its own
admission layer (`src/admission.rs`, "Accept(x) = C1..C7", nonce freshness, refusal codes)
and Merkle-chained audit trail (`autonomic_audit_trail.rs`) mirror ggen's BRCE. This confirms
the Process Intelligence Boundary is a real organ transplant, not an amputation: ggen emits
OCEL; this crate mines, replays, aligns, predicts — and mints receipts for doing so.

### bcinr 26.4.22 — the physical bounded-motion substrate

The published `bcinr` crate is a facade over `bcinr-api`/`bcinr-core`/`bcinr-logic`.
`bcinr-logic` is the substance: a `no_std` branchless library (`pub trait Branchless`) whose
`src/algorithms/` holds hundreds of constant-shape kernels (SWAR, SIMD, bit-matrix
transposes, branchless sorts, Beneš networks) and whose `src/patterns/` is the doctrine's
physics made executable. `swar_petri.rs` — `PriorityPetriEngine<WORDS, TRANSITIONS>` — is a
**branchless Petri net** carrying an explicit timing contract in its doc: "≤ 20 cycles
(~5 ns) per transition attempt … Tail latency bound: Fixed WCET … CC=1: Absolute branchless
logic." `wcet_fiber.rs` gives fixed-tick state machines with a proof artifact
(`InitialFiberState ⊕ Budget ⊕ SuccessMask ⊕ FinalFiberState`); `integrity_receipt.rs`
exports `DeterministicSubstrateReceipt`; `deterministic_mpmc`, `wait_free_multicast`,
`consensus_bft`, `policy_dfa` (`ConstantShapePolicyDfa`) round out a catalogue where *every*
structure has zero heap allocations and data-independent control flow. This is what
`BcinrSubstrate` in `revelation.rs` points at: when execution shape cannot depend on data,
determinism is not a testing goal — it is a physical property of the substrate.

### cargo-cicd 26.6.30 — delivery as lawful process

A noun-verb CLI (46 verbs / 18 nouns) *manufactured from an ontology* via `clap-noun-verb`;
its `lib.rs` states it "emits process evidence in OCEL 2.0 format, and delegates all verdict
adjudication to the external wasm4pm oracle." `cicd.toml` is a carrier contract
(`#[serde(deny_unknown_fields)]`, `src/cicd_toml.rs`) loaded through a `TrustedLoader` into
`AdmittedConfig<CicdToml>` with a BLAKE3 witness hash; the engine's `Pending → Adjudicated`
transition is a typestate. `publish run` is gated on `ReceiptDoctorVerdict::Accepted` and
prints `AndonPull:` on refusal; `ocel replay` verifies the evidence hash chain
(`ReplayStatus::{ChainBroken, HashInvalid}`); and `src/barrier.rs` encodes a 22-case
anti-cheat `Counterexample` enum (`fake_test`, `manual_receipt_json`,
`prose_completion_claim`, `receipt_without_execution_trace`, …) — the anti-cheating rules of
this repo's `.claude/rules/`, compiled. The in-repo projection
(`ontology_catalogue/cargo-cicd/`) models the same surface as RDF: `cicd-process.ttl`
declares `PublishCommand cicd:lawfulPredecessor TestCommand` and
`requiresAdjudicatedEvidence true`; `shapes/release.shacl.ttl` + `release-checklist.rq`
define a 16-condition (RC01–RC16) release gate. Delivery — the last unlawful mile of most
systems — is here a receipted, ontology-governed process like everything else.

### What the constellation proves

Together the siblings resolve the Part 2/Part 3 picture: ggen is the **press** (O → A, with
receipts), wasm4pm-compat is the **law library** (shapes, witnesses, admission, PDDL8),
wasm4pm is the **oracle** (execution, mining, adjudication), bcinr is the **physics**
(branchless bounded motion under fixed WCET), and cargo-cicd is the **shipping lane**
(delivery as adjudicated process). Every pair is joined the same way — typed admission in,
BLAKE3 receipt out — which is the Chatman equation applied to the architecture itself: the
system of crates is an admitted ontology whose artifacts precipitate along lawful edges.

## Part 5 — Gap Map: Doctrine vs. Code, Measured

| Doctrine element | Status in code | Evidence |
|---|---|---|
| μ pipeline (RDF → CONSTRUCT → SELECT → Tera → files, receipted) | **Live, complete** | `ggen-core/src/codegen/{pipeline,executor}.rs`, `ggen-cli/src/cmds/sync.rs` |
| O\* admission machinery (gates, guards, strict mode, trust tiers) | **Live at every layer** | `genesis-construct8/src/admission.rs`, `genesis-types-v2` `AdmissionStatus`, `ggen-lsp/src/analyzers/`, `marketplace/trust.rs` |
| Chatman coherence audit (O ≅ A ≅ L) | **Live, FATAL gate** | `ggen-graph/src/coherence.rs`, `ggen-core/src/sync/coherence_gate.rs`, `post_chatman_*` tests |
| BRCE: receipts, refusals, replay | **Live and hardened** (bit-flip tests, fail-closed verify, anti-replay) | `genesis-core`, `ggen-graph/src/receipt/`, `a2a/receipt.rs`, `stpnt/src/proof/` |
| Rice Quarantine behaviors (refuse / quarantine / abstract) | **Live** | typed Refusals everywhere; `Quarantined` ledger status in `ggen-lsp/src/intel/history.rs` |
| Capability loop (call → admit → execute → receipt → replay → promote) | **Live, advisory actuation** | `ggen-lsp/src/intel/`, `mcp_packs.rs`, route promotion thresholds |
| Cache-as-standing (lockfile + provenance receipts, `--locked` re-verify) | **Live** | `.ggen/packs.lock` path in `mcp_packs.rs`, sync `--locked` |
| POWL execution geometry | **Present** (graphs + validation + POWL-native routes) | `genesis-types-v2` `PowlGraph`, `ggen-lsp/src/route/` |
| **PDDL action law (shapes)** | **Present in constellation** — bounded STRIPS-8 types + receipted execution; absent from monorepo | `wasm4pm-compat-26.6.28/src/pddl.rs` (`Pddl8Domain`, `Pddl8ExecutionReceipt`) |
| **Planner/solver engine** | **Absent everywhere surveyed** (monorepo + all six sibling tarballs) | grep `planner` hits only agentic role-selection in `wasm4pm/src/agentic/` |
| **Numeric objective fluents** | **Absent everywhere surveyed** (PDDL8 is boolean STRIPS; delete effects = epoch seals) | grep `fluent` over monorepo + siblings: builder APIs only |
| **ORTAC-style operator DSLs (RevTAC+ etc.)** | **Absent** | grep `ORTAC`: no files |
| Process discovery / conformance / prediction engines | **Present in constellation** (deliberately external per Process Intelligence Boundary) | `wasm4pm-26.6.25/src/{discovery,alignments,conformance_authority,ml}/` |
| Deterministic bounded compute substrate | **Present in constellation** — branchless Petri engine, WCET fibers, substrate receipts | `bcinr-logic-26.4.22/src/patterns/{swar_petri,wcet_fiber,integrity_receipt}.rs` |
| Delivery pipeline as receipted law | **Present in constellation + projected in-repo** | `cargo-cicd-26.6.30/src/{cicd_toml,barrier,evidence}.rs`; `ontology_catalogue/cargo-cicd/` |
| Compatibility→execution graduation bridge | **Present, typed, two-sided** | `wasm4pm-compat/src/engine_bridge.rs` → `wasm4pm/src/graduation.rs` |
| Revenue Physics (MRR bound, utilization, revenue DPMO) | **Absent; substrate ready** (numeric guarded state machines, regulated profiles) | `marketplace/rdf/state_machine.rs`, `marketplace/profile.rs` |
| Mission physics invariance proof | **Live in one domain** (church operations, consent gates, SLR KPI) | `crates/stpnt/`, `ggen-core/src/stpnt/` |
| TPS/DfLSS modules (poka-yoke, OEE, Kaizen, waste taxonomy) | **Present; DPMO-over-transitions not wired** | `ggen-core/src/{dflss,lean_six_sigma,poka_yoke,metrics}` |
| Combinatorial maximalism engine | **Partial** (route mining enumerates/promotes; no general combination frontier) | `ggen-lsp/src/intel/mine.rs`, pattern registries |
| 43-pattern YAWL engine | **3 of 43; roadmap stub** | `genesis-core-v2/src/patterns.rs` (22 lines) |
| Σ constitutional autonomous evolution | **Built, feature-gated off, proposers partially stubbed** | `ggen-core/src/ontology/`, `enable_llm = false` default |
| Lockchain quorum attestation | **Dormant crate; Ed25519 placeholder, URDNA2015 stubbed** | `crates/genesis-lockchain/` |
| WASM ambient verification | **Dormant crate, wrappers written** | `crates/genesis-wasm-shell/` |
| Membrane boundary law | **Dormant crate** | `crates/ggen-membrane/` |
| Gall foundations (Rust / OTP / AtomVM) | **Chosen and scanned-for** | workspace itself; `SyncLanguage::Elixir`; `cpmp/src/capability.rs` vocab (`AtomVM`, `Erlang`, `WASM`) |
| Anti-complaint doctrine (bounded repair, evidence-or-silence) | **Live** | `genesis-types-v2` `ResidualVector`/`BoundedRepairOperator`, `intel/metrics.rs` `InsufficientEvidence` |

### The shape of the distance

The gap pattern is coherent, and it inverts the usual startup failure mode: **the proof layer
was finished before the power layer.** Admission, receipts, refusals, replay, coherence, and
honesty gates are complete, pervasive, and adversarially tested — and, the constellation shows,
replicated at every layer of the system of crates, from the WASM oracle's C1–C7 admission down
to bcinr's branchless substrate receipts. Measured across the whole constellation, the gap
narrows to a sharp point: the action-law *shapes* exist (PDDL8 with receipted execution), the
process *engines* exist (wasm4pm), the *physics* exists (bcinr), the *shipping lane* exists
(cargo-cicd). What exists nowhere surveyed is the **search**: a planner that takes PDDL8
schemas and an objective and finds plans; numeric fluents to make objectives quantitative;
per-institution operator DSLs compiling down to that calculus; DPMO computed over mission
transitions; and the combinatorial frontier evaluated inside the admitted boundary. Which is
exactly the build order the doctrine itself prescribes: you cannot lawfully maximize over a
surface until the surface is bounded, admitted, receipted, and replayable. The brakes exist —
at every layer, in every crate — so the engine, when it arrives, can be floored.
