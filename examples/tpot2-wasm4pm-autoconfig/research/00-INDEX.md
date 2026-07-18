# Research Program — 10-Agent Dossier Index

Ten parallel research agents audited this generator against the real ggen/wasm4pm
codebase (read-only, source-cited, evidence-first). Each dossier is grounded in
`file:line` citations; none fabricated runtime evidence. Headline findings and the
integration fixes they drove:

| # | Dossier | Headline finding | Fix applied |
|---|---------|------------------|-------------|
| 01 | `01-ggen-sync-executability.md` | **Driver `ggen.toml` is RUNNABLE** by `ggen sync` (inference runs first sorted by `order`, materializes 60 operators, SELECTs see them, paths in-root). CONSTRUCT-in-generation correctly errors E0003 → our SELECT routing is right. Generated config has a path/inference defect. | Generated config relabeled as a specification (see 08). |
| 02 | `02-wasm4pm-runtime.md` | **wasm4pm is metadata-only in this repo** — no `.wasm`, `pi:wasmExport` are string literals, the runtime is an unprovisioned sibling repo (`../wasm4pm`). The 6 proofs use placeholder `BIND()` literals. | Documented as an honest boundary; generator precipitates a *specification*, runtime is downstream. |
| 03 | `03-event-log-inputs.md` | **Real inputs exist**: `crates/cpmp/tests/fixtures/p2p.ocel.json` (OCEL 2.0) + `test_log.xes`. Flagged that the ingest elite `pnml_import` imports a *model*, not a log — fitness-only selection ignores format compatibility. | Documented as a known limitation / future type-safe-selection enhancement. |
| 04 | `04-tpot2-fidelity.md` | Faithful to TPOT2's declarative skeleton (derived config_dict + real Pareto front); deliberately divergent on the stochastic engine. **Per-stage greedy argmax is the EXACT optimum of the separable linear problem.** GP params declared-but-inert. | Marked `tpot:executionMode "deterministic-greedy-elite"`; relabeled the cost objective as a runtime-cost proxy (R1, R6). |
| 05 | `05-process-mining-conformance.md` | ggen's OCEL engine is real/tested but `ggen sync` emits no OCEL. Designed an anti-cheating conformance harness (reject the 8 impossible logs). | Implemented `verify/conformance_check.py` (10/10) + `verify/conformance_spec.md`. |
| 06 | `06-shacl-and-receipts.md` | Shapes valid + all pass; `fitnessScore` correctly `xsd:decimal`. **SHACL enforcement is gated by `[validation] shacl=[...]`, which the driver lacked → shapes were inert.** Receipts are SHA-256/Ed25519 (not BLAKE3). | Added `[validation] shacl + strict_mode`; tightened OperatorShape (4 derived predicates) + `forCategory sh:in`. |
| 07 | `07-determinism-slo.md` | Deterministic by construction; SLO holds huge margin (~2k triples, O(60²) dominance negligible). `determinism_salt` is a dead field. | Added `ORDER BY` to both CONSTRUCTs; implemented `verify/slo_check.py` (worst 2.7ms vs 5s). |
| 08 | `08-generated-output-audit.md` | `../` is safe for inputs, but the generated config also omits `[inference]`/`imports` → E0004. **RULES 5 & 6 produce silent wrong output** (projection-key mismatch). | Relabeled generated config as a spec; created dedicated `stage-plan.md.tera` + `objectives.json.tera`, repointed RULES 5/6. |
| 09 | `09-marketplace-packaging.md` | Complete draft `package.toml` + registry entry for publishing as a pack; confirmed BUG-008/011 pack-output footgun (0 working pack-source examples in repo). | Packaging plan captured in the dossier (recommended install-and-sync path). |
| 10 | `10-lsp-diagnostics-genesis.md` | `ggen lsp check` would pass clean, BUT **GGEN-TPL-001 structurally CANNOT catch the RULE 5/6 mismatch** (defeated by `{% set X = row["X"] %}` aliasing) — manual audit caught what the linter misses. BUG-003 is stale (YIELD detectors exist). genesis-core-v2 mislabels Exclusive Choice as WCP-3. | RULE 5/6 fix confirmed independently necessary; findings noted for the ggen team. |

## What changed as a result (all re-verified green)

- **Correctness/drift:** RULES 5/6 silent-wrong-output fixed (dedicated templates);
  generated config relabeled (no false runnability claim); `ORDER BY` on both
  inference CONSTRUCTs.
- **Authority deepened:** `[validation]` activates the previously-dormant SHACL gate
  + strict mode; OperatorShape/StageShape tightened.
- **New Vision-2030 proof artifacts:** `verify/conformance_check.py` (process-mining,
  rejects 8 impossible logs), `verify/conformance_spec.md`, `verify/slo_check.py`.
- **Honest TPOT2 labelling:** deterministic-greedy execution mode; runtime-cost proxy.

## Honest boundaries → now CLOSED by a real run

The research verdicts were all **source-analysis** corroborated by the independent
Python reference. Those verdicts have since been **confirmed by a real execution**:
the sibling repos were cloned, the corrupted nightly toolchain reinstalled, `ggen
26.6.11` built, and `ggen sync` ran successfully — its output matches the Python
reference exactly and `strict_mode` + SHACL passed (research/01/06/07 validated by
the run). Evidence: `REAL_RUN_EVIDENCE.md`; provisioning: `BUILD_PROVISIONING.md`.
The one residual: `ggen sync` emits no OCEL (research/05 §1.6), so
`conformance_check.py` stays Mode A. No receipt was ever fabricated.
