# ggen-core — Generation-First Audit

**Path:** `crates/ggen-core/src` · **95,776 tokei code LOC** (57% of workspace) · tokei 14.0.0
**Purpose:** Core graph-aware code generation engine — the μ₁–μ₅ pipeline itself, plus RDF/graph internals, validation gates, crypto receipts, security, and the CLI-facing domain layer.

## Measurement

```bash
tokei crates/ggen-core/src --files --output json   # per-file, aggregated by top-level dir with python
```
(Per-module numbers below are aggregates of `--files` output, not per-dir tokei invocations; totals cross-check: Σ = 95,776.)

## Headline

| Class | LOC | % |
|---|---:|---:|
| IRREDUCIBLY-CUSTOM (E-ledger candidates) | 70,446 | 73.6% |
| GENERATABLE-WITH-SPEC | 25,273 | 26.4% |
| GENERATABLE-NOW | 57 | 0.1% |
| GENERATED / DEAD-DELETE | 0 | 0% |

**Prior estimate (~55% boilerplate-shaped / ~45% algorithmic) does not hold for this crate.**
Sampling basis: directory-level classification from module-doc/header skims + sub-directory tokei breakdowns + targeted file reads (shacl.rs, validator.rs, standards.rs, project_generator/*, register.rs, agent/mod.rs, prevention/mod.rs, lifecycle/production.rs); ~95k LOC were NOT read line-by-line. This crate concentrates the machinery — the boilerplate mass the prior estimate expected lives mostly in the *other* 43% of the workspace. Roughly a quarter of ggen-core is honestly boilerplate-shaped; three quarters is the μ-machinery the E-ledger exists for.

**Caveat:** "IRREDUCIBLY-CUSTOM" here means *E-ledger candidate*, not "reviewed E-ledger member". Every directory below marked E is a claim requiring allowlist review. Several CUSTOM directories (poka_yoke, drift, membrane, cleanroom, semantic_bit, genesis.rs, stewardship.rs, parts_*) are framework/doctrine code whose E-membership is weakest — flagged for reviewer scrutiny.

## Per-module LOC and class

| Module | LOC | Class (dominant) | Rationale |
|---|---:|---|---|
| domain/ | 16,726 | MIXED: 10,442 GWS / 6,227 E / 57 GNOW | packs CRUD, config schemas, mcp_config, template/CLI command wiring, ci, project scaffolding = GWS; graph/rdf/ontology/generation/sync_profile semantic core = E. `domain/ontology/standards.rs` (57) = GENERATABLE-NOW: `include_str!` consts over `ontologies/*.ttl` — content is already TTL, only const-wrapper emission missing. Mixed-dir note: `domain/packs/advanced_resolver.rs` + `dependency_graph.rs` (~1k) are the algorithmic minority inside a GWS prefix. |
| codegen/ | 9,177 | MIXED: 6,654 E / 2,523 GWS | pipeline.rs (1,540) + executor.rs (947) + canonicalize/incremental/watch/transaction = E-ledger μ-machinery. Language/IaC emitters (python 872, docker_kubernetes 457, go 425, elixir 291, typescript 263, terraform 215) = GWS: template-shaped string assembly — the machinery's *payload*, convertible to `.tera` templates + TTL emitter specs. |
| utils/ | 6,631 | MIXED: 4,943 E / 1,688 GWS | secrets, safe_command, path_validator, safe_path, supply_chain = security-critical E candidates; fmea/ (911) + error boilerplate = GWS. |
| pipeline_engine/ | 5,306 | E | The μ-pipeline itself: passes/ (2,546), receipt.rs, proof_gate.rs, guard.rs, epoch.rs. Strongest E claim in the crate. |
| lifecycle/ | 4,877 | MIXED: 3,282 E / 1,595 GWS | DAG/state-machine/exec/hooks = E; production.rs (1,074) readiness-checklist tables = GWS; integration_test.rs (521) is test code compiled into src/ — relocate to tests/. |
| validation/ | 4,667 | E | SHACL/SPARQL gates, input compiler, soundness gates. **P0-01 flag:** prior audit called `validation/shacl.rs` an always-pass stub. 2026-07 skim: shacl.rs defines shape types + a SPARQL ShapeLoader, and `validator.rs` implements real minCount/maxCount/datatype checks — the stub claim appears **stale**; sh:pattern / sh:in coverage unverified. |
| ontology/ | 4,429 | E | sigma_runtime, extractor, pattern_miner, control_loop, delta_proposer — algorithmic. |
| security/ | 3,706 | E | intrusion detection, audit trail, secure templating, event pipeline. |
| graph/ | 2,777 | E | Oxigraph wrapper: store/query/construct/update, cycle detection+fixing. |
| rdf/ | 2,142 | E | query builder, schema, template metadata over triples (ships code_ontology.ttl, schema.ttl). |
| poka_yoke/, prevention/, protection/, drift/, membrane/, cleanroom/, semantic_bit/ | ~5,163 | E (prevention/ 719 = GWS) | DfLSS error-proofing / doctrine framework code. Weakest E claims — prevention/ (PhantomData state-machine + contract boilerplate) already reassigned GWS; reviewers should press on the rest. |
| receipt/ 1,507, pki.rs 566, pqc.rs 160, canonical/ 502 | 2,735 | E | Crypto: Ed25519 receipts + chaining, PKI, PQC, canonical hashing. Textbook E-ledger. |
| merge/ 1,182, sync/ 841, reverse_sync/ 1,250, delta.rs 489, snapshot.rs 268 | 4,030 | E | three-way merge, coherence gate, inverse pipeline (AST extraction), deltas. |
| cli_generator/ 713, project_generator/ 950, prompt_mfg/ 872 | 2,535 | E, flagged | cli_generator: CLI-from-ontology. **project_generator FLAG:** rust.rs/nextjs.rs scaffold via hardcoded string literals (~250 quoted strings across files) — payload should become .tera + TTL. prompt_mfg: IR/emitter/validator. |
| templates/ 1,299, template_cache.rs 254, template/ 105, tera_env.rs 69, register.rs 772, preprocessor.rs 331, inject.rs 179 | 3,009 | E | Tera environment, filters (case/pluralization/SPARQL helpers), file-tree generation — the template machinery. |
| generator family (generator.rs 391, parallel_ 143, streaming_ 262, pipeline.rs 558, codegen_lib 260, resolver.rs 171, pack_resolver.rs 713, naming.rs 273, cache.rs 258) | 3,029 | E | orchestration + resolvers. |
| lockfile_unified/ 1,134, transport/ 1,065, ontology_core/ 1,185, packs/ 425 | 3,809 | E | trait-encoded lockfile invariants; A2A transport; ontology core. |
| GWS singletons: types/ 992, schema/ 870, manifest/ 715, agent/ 768, registry.rs 692, template_types.rs 691, lean_six_sigma.rs 648, metrics.rs 630, gpack.rs 389, dflss.rs 360, tracing×3 649, lockfile.rs 292, github.rs 278, e2e_tests.rs 174, ontology_pack.rs 158 | 8,306 | GWS | serde type defs, manifests, API-client wiring, metric/quality-gate tables, observability wiring, facade contracts. schema/ mixed note: grammar.pest+parser.rs are the algorithmic minority. e2e_tests.rs is test code in src/. |
| remainder (genesis.rs, stewardship.rs, parts_*, manufacturing/, stpnt/, audit/, poc.rs, misc) | ~2,700 | E (weak) | doctrine/foundry modules; poc.rs name suggests DEAD-DELETE candidacy but no consolidation-analysis citation exists, so left CUSTOM. |

## Findings for the roll-up

1. **P0-01 appears stale** — SHACL validation in `validation/{shacl,validator}.rs` is implemented (SPARQL-backed minCount/maxCount/datatype). Recommend re-verifying with a failing-shape fixture before closing.
2. **Quick wins:** `domain/ontology/standards.rs` (GENERATABLE-NOW); project_generator + codegen language emitters (~3.5k LOC) are hardcoded template payload begging to be externalized into `.tera` + TTL — highest-leverage conversion in the crate.
3. **Test code in src/**: `e2e_tests.rs`, `lifecycle/integration_test.rs`, plus `*/tests.rs` files inflate src LOC (~1k).
4. E-ledger review should scrutinize the ~5k LOC of DfLSS/doctrine framework dirs (poka_yoke, drift, membrane, cleanroom, semantic_bit, parts_*) — custom, but not obviously *engine* custom.

TSV fragment: `docs/audits/generation-2026-07/tsv-fragments/ggen-core.tsv` (99 prefix/file lines; verified to cover all 300+ .rs files with zero uncovered).
