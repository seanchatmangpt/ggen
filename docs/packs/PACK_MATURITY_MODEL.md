# Pack Maturity Model

Three orthogonal pack types, three separate 5×7 maturity matrices. Updated 2026-07-18.

The types measure different capabilities, so they do not share dimensions:

- A **codegen pack**'s maturity is how much of the target system it can *generate*.
- A **knowledge-hook pack**'s maturity is how much of the target process it can *govern at runtime*.
- A **case-study corpus pack**'s maturity is how much of a domain it can *decide*.

## Level spine (shared semantics, per-type meaning)

| Level | Name | Meaning |
|---|---|---|
| L1 | Describes | The pack states facts about the domain/target. Nothing runs because of it. |
| L2 | Produces fragments | Real output exists, but a human builds the actual system around it. |
| L3 | Produces working subsystems | The generated/derived artifact does its job with only consumer configuration — no hand-written logic in the generated lane. |
| L4 | Produces + maintains | Regeneration is the normal way the subsystem evolves; hand-editing generated output is never needed; upstream change flows through the ontology. |
| L5 | Complete substitute | Pack + ggen alone build, verify, and evolve that entire part of the system. A consumer with zero knowledge of the target's implementation gets a finished, tested, receipted subsystem. |

**Calibration rule:** L5 means the pack and ggen are *all* someone needs to completely
build that part of their system. Under this anchor, nothing in `packs/` today exceeds L2
on any dimension. No pack may be scored above L2 without a named artifact proving it.

## Matrix 1 — Codegen packs

| Dimension | L1 | L2 | L3 | L4 | L5 |
|---|---|---|---|---|---|
| **Generation depth** | Ontology describes the target; nothing generated. | Const tables, route skeletons, reference docs. | A compilable module that performs its function (parse, dispatch, emit) without stubs. | The full module family for one concern (types + logic + docs) from one ontology. | The entire crate surface — types, logic, tests, docs — precipitates from RDF. `A = μ(O)` literally. |
| **Handler-gap size** | N/A — nothing to handle. | Every generated entry point delegates to a hand-written handler. | Common behaviors generated; only domain-unique logic is hand-written, behind a stable seam. | Hand-written code is optional extension, never required for the subsystem to work. | Zero handler gap: behavior itself is specified in the ontology or composed from other packs' generated logic. |
| **Ontology expressiveness** | Facts only (labels, comments). | Facts + call metadata (names, args, types); templates hardcode all structure. | Ontology states structure (relations, cardinality, error cases); templates are thin projections. | Ontology states behavior contracts (pre/post-conditions, state transitions). | Ontology is a complete specification; a different template set could regenerate an equivalent system. |
| **Consumer effort** | Consumer reads the ontology, writes everything. | Consumer wires `ggen.toml`, writes handlers, mod declarations, tests. | Consumer wires `ggen.toml` + writes only domain logic. | Consumer wires `ggen.toml` + supplies configuration values. | Consumer wires `ggen.toml`. Done. |
| **Test generation** | No tests. | Tests generated for code the pack does not generate, or none. | Generated code ships with generated smoke proofs that pass on first sync. | Generated Chicago-TDD proofs cover the generated behavior surface (error paths included). | Generated proof suite is sufficient evidence on its own — passing it certifies the subsystem. |
| **Regeneration lifecycle** | N/A. | Regen works but clobber/skip semantics are the consumer's problem (`force`, delete `ggen.lock` by hand). | Freeze slots / inject modes let generated and hand-written code coexist across regens. | Upstream ontology change → regen → consumer sees a semantic diff + receipt; no manual repair. | Regen is the only maintenance verb; drift is impossible by construction (lock + receipt refuse it). |
| **Target-API fidelity** | Unchecked claims about the target. | Verified by hand against one target version, once (dated note). | Generated output compiled against the pinned target crate in the pack's own CI proof. | Version bump of the target is detected and re-verified automatically. | Pack tracks the target's ontology, not its API — fidelity is definitional, not checked. |

## Matrix 2 — Knowledge-hook packs

| Dimension | L1 | L2 | L3 | L4 | L5 |
|---|---|---|---|---|---|
| **Derivation power** | Vocabulary for events/turns exists; no rule. | CONSTRUCT derives a tag or single obligation node. | Derives obligations with parameters (deadline, target, severity) usable by a downstream planner. | Derives plans: ordered obligation chains with discharge conditions. | Derives, prioritizes, and re-plans as new facts arrive — a standing governor, not a rule. |
| **Actuation closure** | None. | Derived fact is queryable; acting on it is manual. | A shipped actuation script fires on derivation and its effect is receipted. | Actuation outcomes flow back into the graph as new facts (closed loop). | Loop runs unattended: derive → actuate → receipt → re-observe, with refusal paths receipted too. |
| **Input acquisition** | No inputs modeled. | Hand-asserted fixture facts only. | A shipped capture pipeline turns real system events into admitted facts. | Capture is continuous and validated (SHACL-gated) on ingest. | Capture, admission, and retention are the pack's responsibility end to end; consumer points it at a source. |
| **Fire precision** | Untested. | Fire + no-fire fixture pair proven by a real test. | Adjacent-but-shouldn't-fire cases proven (topic/boundary confusions). | Adversarial inputs (malformed, colliding, gamed) proven to fail closed. | Precision measured on real captured data over time, not only fixtures. |
| **Obligation lifecycle** | N/A. | Obligations emitted; nothing tracks them. | Discharge is recorded; open obligations are queryable. | Overdue/undischarged obligations escalate via the same hook mechanism. | Full ledger: every obligation from derivation to discharge/refusal, receipted and replayable. |
| **Composability** | Single-pack assumption everywhere. | Co-loads with other hook packs without IRI collision (namespace discipline). | Proven co-fire behavior: hooks from ≥2 packs derive correctly over one union graph. | Hooks reference other packs' derived facts by design (obligation chaining across packs). | Hook families form a governance stack; ordering and precedence are declared in RDF. |
| **Governance coverage** | Zero — describes a process, governs nothing. | One pattern in the target process is governed. | The main happy path of the process is governed. | Happy path + failure/exception paths governed. | The whole process: any ungoverned transition is itself detectable and flagged. |

## Matrix 3 — Case-study corpus packs

| Dimension | L1 | L2 | L3 | L4 | L5 |
|---|---|---|---|---|---|
| **Question coverage** | Competency questions listed in prose. | Some CQs answered by hand-run queries. | Every CQ has a committed query and a test asserting its answer. | CQs cover the decision space (not just the authored scenario's slice). | New CQs answerable without new authoring — the corpus schema already carries the facts. |
| **Verdict authority** | Illustrates a scenario. | Decides *the* authored case correctly (accept + reject fixtures). | Decides variations of the authored case (parameter changes) without new shapes. | Decides structurally new cases poured into the schema, unseen at authoring time. | Adjudicates any well-formed case instance: verdict + rationale + receipt, no human in the loop. |
| **Domain fidelity** | Synthetic scenario, invented rules. | Scenario shaped by a real domain (named regulation/practice) but simplified. | Rules traced clause-by-clause to the real source material. | Real-world data instances validated against the corpus. | Domain experts accept the corpus verdicts as authoritative for the covered scope. |
| **Adversarial resistance** | Untested. | Positive + negative fixtures pass. | Colliding-identifier and contradictory-fact fixtures resolve correctly. | Deliberately gamed inputs (crafted to slip past shapes) are refused with reasons. | Resistance is regression-fed: every real-world miss becomes a permanent fixture. |
| **Planning integration** | No planner artifacts. | A PDDL domain file exists; planning is not exercised. | Planner produces a valid discharge plan for the authored case, in a test. | Plans execute against the obligation lifecycle (plan steps discharge obligations). | Verdict → plan → discharge → receipt is one automated pipeline for any admitted case. |
| **Generalization** | One hardcoded case. | Case facts separated from schema, but only one instance exists. | ≥2 independent case instances validated by the same schema unchanged. | Case-instance authoring is generated (forms/templates), not hand-written Turtle. | Any conforming instance from any source is admissible; the pack defines the domain, not a case. |
| **Reasoner independence** | Verdicts asserted in prose. | Verdicts reproduced by the one engine/test that authored them. | Verdicts reproduced on a second engine (e.g. oxigraph AND praxis-graphlaw). | Verdicts + proofs exportable (SHACL reports, plan traces) for external checking. | Verdict validity is engine-agnostic by construction; the receipt carries the proof. |

## Current-state scoring (all 20 packs, 2026-07-18)

Scores follow the calibration rule: L2 ceiling, and nothing above L2 without a named
artifact. `UNVERIFIED` = not re-run/re-audited this session; structure read from disk only.

### Codegen packs (17)

| Pack | Generation depth | Handler gap | Ontology expressiveness | Consumer effort | Test generation | Regen lifecycle | Target-API fidelity |
|---|---|---|---|---|---|---|---|
| clap-noun-verb-pack | L2 (route skeletons) | L2 (every verb → hand handler) | L2 (args encoding) | L2 | L1 | L2 (skip_if freeze marker) | L2 (receiptctl build, 26.7.4) |
| wasm4pm-compat-pack | L2 (emit fns) | L2 (caller supplies ids/time) | L2 | L2 | L1 | L2 | L2 (verified vs 26.6.29 source) |
| wasm4pm-algorithms-pack | L2 (const catalog) | L2 (catalog only, no calls) | L2 | L2 | L1 | L2 | L2 (receiptctl build) |
| wasm4pm-cognition-pack | L2 (catalog + dispatch skeleton) | L2 | L2 | L2 | L1 | L2 | L2 (receiptctl build) |
| wasm4pm-facts-pack | L2 (registry doc) | L1 (doc only) | L2 | L2 | L1 | L2 | L2 (mirror synced this week) |
| chicago-tdd-tools-pack | L2 (boundary tests for a CLI it doesn't generate) | L2 | L2 | L2 | L2 (it generates tests — its whole point) | L2 | L2 (needles/exit codes live-verified) |
| praxis-core-pack | L2 | L2 | L2 | L2 | L1 | L2 | UNVERIFIED |
| star-toml-pack | L2 | L2 | L2 | L2 | L1 | L2 | UNVERIFIED |
| lsp-max-pack | L2 | L2 | L2 | L2 | L1 | L2 | UNVERIFIED |
| cargo-cicd-pack | L1→L2 (const table of another CLI's commands) | L1 (nothing callable) | L1 (facts only) | L2 | L1 | L2 (idempotency verified) | L1 (README-derived, not source-audited) |
| mcpp-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 (README-grepped, known partial) |
| osx-clnr-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 |
| affidavit-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 |
| anti-llm-cheat-lsp-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 |
| wasm4pm-pack | L1→L2 (crate map table) | L1 | L1 | L2 | L1 | L2 | L1 (Cargo.toml-derived) |
| mfact-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 (README-derived) |
| mfw-pack | L1→L2 | L1 | L1 | L2 | L1 | L2 | L1 (AGENTS.md-derived) |

"L1→L2" = generation exists (PR #271's verified sync + build) but generates only a
description of another system — the floor of L2, nowhere near a fragment *of the
consumer's own system*.

### Knowledge-hook packs (2)

| Pack | Derivation power | Actuation closure | Input acquisition | Fire precision | Obligation lifecycle | Composability | Governance coverage |
|---|---|---|---|---|---|---|---|
| self-monitoring-pack | L2 (one EscalationObligation) | L1 (queryable only) | L2 (hand-asserted fixtures; transcript_to_turtle.py exists but isn't wired) | L2 (fire/no-fire/different-topic proven) | L1 | L2 (reuses dfl:Session by IRI) | L1 (one pattern) |
| dogfood-lifecycle-pack | L2 | L2 (hooks/*.sh exist; effect receipting UNVERIFIED) | L2 (capture scripts shipped) | L2 (good/malformed fixtures) | L1 | L2 | L1–L2 |

### Case-study corpus packs (1)

| Pack | Question coverage | Verdict authority | Domain fidelity | Adversarial resistance | Planning integration | Generalization | Reasoner independence |
|---|---|---|---|---|---|---|---|
| ma-case-study-pack | L2 (CQs listed; per-CQ test coverage UNVERIFIED) | L2 (decides the authored case only) | L2 | L2 (colliding + adversarial fixtures exist; UNVERIFIED this session) | L2 (pddl-domain.ttl exists; end-to-end planning UNVERIFIED) | L2 (one instance) | L2 |

## Reading the gap

The distance from today's uniform L2 ceiling to L5 is the actual ggen roadmap, per type:

- **Codegen:** close the handler gap (behavior in the ontology), generate the proofs, make
  regeneration the only maintenance verb.
- **Hooks:** ship capture, close the actuate→receipt→re-observe loop, track obligations to
  discharge.
- **Case studies:** turn the case into a schema, make the verdict engine-independent, and
  wire verdict→plan→discharge as one pipeline.

## See Also

- `.claude/rules/architecture.md` — crate map and pack-consuming pipeline
- `packs/` — the 20 scored packs
- `examples/receiptctl/` — the multi-pack consumer used as verification substrate
