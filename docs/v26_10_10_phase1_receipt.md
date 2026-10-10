# v26.10.10 Phase 1 Receipt

Phase 1 (hygiene + schema-dispatch verification + star-toml migration) on
`/Users/sac/ggen`, branch `spec-integration`. Companion to
`docs/v26_10_10_repo_state_and_library_usage_report.md` (§7 delta). Date: 2026-10-09.

## What changed since the audit

- Audit §4.1 claimed "only 1 of 6 call sites dispatches" through the schema classifier.
  Verified corrected state: **all six** ggen.toml sites already dispatch through
  `ggen_config::classify_ggen_toml` via `crates/ggen-engine/src/schema_dispatch.rs`
  — callers `sync.rs:259`, `verbs/handlers.rs:132/919/1118`, `project_graph.rs:58`;
  ggen-lsp `project_index.rs:188-235`; ggen-mcp `tools/config_classify.rs:65`.
  The classifier doc text is stale, not the code.
- Root `Cargo.toml`: ggen-core comment corrected (was "untouched on disk" — dir deleted
  2026-07-17); `star-toml = "26.7.3"` added to `[workspace.dependencies]`; ggen-config and
  ggen-engine pins normalized to `workspace = true`.

## Receipt table

| Claim | Command / evidence | Result |
|---|---|---|
| Root Cargo.toml comment + workspace star-toml pin | direct edit + re-read on disk | DONE |
| abb-sbb Cargo.lock/target deletion | attempted `rm` | **NOT DONE** — permission-denied; recorded, left open |
| All six ggen.toml sites dispatch via classify_ggen_toml | grep of callers + schema_dispatch.rs | VERIFIED (§ above) |
| CLI-transport schema-dispatch e2e coverage | 3 tests appended to `crates/ggen-engine/tests/config_schema_dispatch_e2e.rs` (graph validate both schemas + ambiguous refusal; law load both schemas + ambiguous refusal) | suite 11 tests: **11 passed, 0 failed** |
| LSP project-index schema dispatch | new `crates/ggen-lsp/tests/project_index_schema_dispatch_test.rs` | **6 passed, 0 failed** |
| law_engine.rs repointed to graphlaw | concurrent v26.10.9 effort; `cargo check -p ggen-engine` | exit 0 |
| law_engine.rs:189 pre-existing compile break fixed | `check_structure` `Vec<StructureError>` → `format!("{e:?}")` | DONE |
| Engine+graph+abb-sbb ladder | full test run | **86 tests, 0 failures** (config_schema_dispatch_e2e 11, graphlaw_e2e 6, composed_packs_e2e 2, abb_sbb_datalog_admission_e2e 4, law_engine_test 4, law_engine_bridge_e2e 4, abb-sbb 59) |
| star-toml: ggen-engine migrated | grep residuals in ggen-engine | 0 residuals |
| star-toml: ggen-config migrated | 6/6 sites; `cargo test -p ggen-config` lib | **115 passed, 0 failed** (one pre-existing governance doc-count failure: CLAUDE.md "15-crate" vs actual member count — not a Phase 1 defect) |

## Environmental finding

`TMPDIR` points at nonexistent `~/.cache/tmp`; tests require `TMPDIR=/tmp`.

## Completed lane receipts (folded in 2026-10-09)

| Item | Evidence | Status |
|---|---|---|
| Schema dispatch e2e (CLI transport) | `config_schema_dispatch_e2e.rs` | **11 passed, 0 failed** (3 new tests) |
| LSP dispatch tests | `project_index_schema_dispatch_test.rs` | **6 passed, 0 failed** |
| graphlaw WIP: engine ladder | engine+graph+abb-sbb | **86 tests, 0 failures**; `law_engine.rs:189` fix `format!("{e:?}")`; `GraphLawStore` still dual-sourced on `praxis_graphlaw::TripleStore` (`graph.rs:1157` area) — **NOT yet repointed** |
| star-toml: ggen-config | 6/6 sites; lib | **115 passed, 0 failed** |
| star-toml: downstream 12/12 sites | marketplace **503/0**, cli **85/0**, lsp lib **238/0**, mcp check clean; `capability_status.rs:83` now `star_toml::from_str::<toml::Value>` | **DONE** |
| star-toml exceptions (9 sites) | lsp analyzer/formatting 3, cli policy 1, marketplace compatibility 3, abb-sbb depgraph 2 | stay on `toml` (documented) |
| New schema sections | `[rules]` (n3/datalog) + `[pack_sources]` in `GgenManifest`; `DECLARATIVE_ONLY_TABLES` classifier updated; 9 new tests (`schema_sections_test.rs`); ggen-config full green except a parity failure **NOW FIXED** | DONE |
| `resolve_rule_sources` seam | `generation_rules.rs` — typed `DatalogUnsupported` refusal; 4/4 in-module tests | DONE |
| Full-suite verifications | ggen-lsp **367/0** (40 targets), ggen-mcp **100/0** (18 targets), ggen-graph **~130/0**, ggen-cheat-scanner **19/0**, bcinr-pddl lib **91/0**; governance gate **4/4** after doc fix (CLAUDE.md + architecture.md now say 16 crates) | DONE |
| repo-facts.ttl fix | added `rf:crate_ggen_abb_sbb`; `crateMapIntro` corrected to 15 members + root = 16; `system_crate_map_parity_test` **2/2 green** | DONE |
| Lint census | clippy **0/0** workspace-wide, but purchased by blanket workspace-lint allows (root `Cargo.toml:305+`, praxis-graphlaw 22-lint allow); recommendation: fix-forward per-crate | RECORDED |
| Praxis census | 66 refs / 14 files remain; receipt-chain family (`ReceiptRecord`, `ChainVerification`, `receipt_epoch`, Andon, ≈48 refs) has **no graphlaw equivalent** = dominant retirement blocker; praxis-core shim mostly dead surface | RECORDED |

## Folded receipts (wave 3, 2026-10-09)

| Item | Evidence | Status |
|---|---|---|
| praxis-core shim prune | 9 modules + 6 test files removed; shim now live surface only (receipt_epoch, receipt_record, law::Andon + internal deps); `cargo check` clean; law_engine_test **4/0**, receipt_chain_e2e **25/0** | DONE |
| CLI `--json` alias | full CLI suite at HEAD: **543/1** (single pre-existing failure reproduced at HEAD — not session-introduced) | DONE |
| bcinr-pddl defect fixes (5) | 3 NaN-panic comparators: `execute.rs:681`, `execute.rs:987`, `schedule_analysis.rs:211`; 2 fail-open inserts: `ground/mod.rs:347`, `ground/lazy.rs:421`; lib **91/0** | DONE |
| bcinr-pddl lint-policy finding | crate lacks `[lints] workspace = true` — workspace lint policy silently skips it | RECORDED |
| repo-facts.ttl correction | memberCount 15, crateCount 16, `rf:crate_ggen_abb_sbb` added; parity **2/2**, governance **4/4** | DONE |
| architecture.md reconciliation | hand-reconciled; stale-template root cause documented; template fix in flight | DONE (template fix PENDING) |
| TMPDIR root cause | `~/.uvrc:4` exports `TMPDIR=$HOME/.cache/tmp`; today's `~/.cache` wipe dangled it for running sessions; dir restored; self-heals on new shells | RESOLVED |
| Disk cleanup | osx-clnr cache-clean freed **78.82 GB** (7 lane build roots); oclnr plan `/tmp/oclnr-lane-plan.json` holds ~35 GB older scratch candidates AWAITING user review | DONE / REVIEW PENDING |
| `ggen sync run` stale collateral | rendered stale output, reverted via `git show HEAD:` swaps — disclosed | REVERTED |
| Praxis retirement plan | Lane Z design ported: `docs/v26_10_10_praxis_retirement_plan.md` (blocker = no graphlaw receipt-chain equivalent; verbatim port build order) | WRITTEN |

## Deferred / pending

- **PENDING**: engine all-targets full tally (lane still running).
- **NOT DONE**: abb-sbb `Cargo.lock`/`target` deletion (permission denied).
- **Integration cleanup (coordinator)**: stray lane build dirs `/tmp/lanej-target`,
  `/tmp/lsp-lane-target` (+ any `/tmp/laneX-target` that appear).
- **OPEN (cosmetic)**: stale "only 1 of 6 dispatches" text in `config_schema.rs` doc
  (code is correct).

## Independent verification + integration reconciliation (2026-10-09, final)

Independent-verifier report: all gates PASS, no breakage. Four discrepancies found, all reconciled:

1. **Grep count 10 vs 9** — `cross_pack_macros_e2e.rs:75` used `toml::from_str`. Fixed to `star_toml::from_str`; final grep = **exactly 9 documented-exception sites, 0 unexpected**.
2. **Gate-b invocation fragility** — 4 CLI-spawning tests in `config_schema_dispatch_e2e.rs` hard-failed on `CARGO_BIN_EXE_ggen` under plain `cargo test -p ggen-engine`. Fixed with the repo's `ggen_bin()` fallback pattern (from `cli_boundary.rs`: env var → workspace `target/{debug,release}/ggen` → PATH); `ggen` binary built once into shared `target/debug`. Suite now **11 passed; 0 failed** under the plain invocation.
3. **Undocumented praxis-core deletions** — receipt now itemizes: 9 dead module files deleted (`default_law.rs`, `graphlaw_authority.rs`, `ocel.rs`, `ocel_export.rs`, `quarantine.rs`, `receipt_store.rs`, `receipt_validator.rs`, `replay_adapter.rs`, `verify.rs` — all git-tracked, recoverable), 7 dead test files deleted, dead `LawObject::transition` method + orphaned doc comment removed. `cargo check -p praxis-core` clean, zero warnings.
4. **Concurrent-lane mutation during verification** — tree re-baselined post-verification: the fixes above landed after the verifier's snapshot; final grep and dispatch e2e re-run on the settled tree.

Compile-fix disclosures folded in: `law_engine.rs:189` (`format!("{e:?}")` for `check_structure` Vec error), `composer.rs` `registry_type` initializer unblocks (Lanes W/Y, also converged on by Lane AA's rewrite), Lane O's `cargo fmt` churn on `law_engine.rs`/`generation_rules.rs` (fmt-only).

## Definition of Done — v26.10.10 modernization checklist

Status marked strictly from session-verified facts. Running lanes = PENDING.

| # | DoD item | Status | Evidence |
|---|---|---|---|
| 1 | star-toml migration complete | DONE | 18 typed sites migrated; final grep = 9 documented-exception sites, 0 unexpected |
| 2 | Schema drift-guard dispatch | DONE | all 6 dispatch sites verified; `config_schema_dispatch_e2e.rs` 11 passed / 0 failed (ggen_bin() fallback) |
| 3 | [rules]/[pack_sources] schema sections + reference pack fixture | DONE | schema + fixture landed; dispatch e2e covers it |
| 4 | N3 rules wired into sync | DONE | fuse = FM-LAW-016; datalog = FM-LAW-019 UNSUPPORTED (typed refusal); byte-identical no-rules drift proof |
| 5 | GraphLawStore repointed to praxis-graphlaw | DONE | zero src consumers of old path; hash preserved; 1 GAP: refuse-effect hooks → typed load refusal |
| 6 | Refuse-effect hook upstream support | upstream DONE / consumer PARTIAL | `~/graphlaw` Effect::Refuse/Verdict::Refuse landed (432/0); ggen rewiring in flight (see lane receipts) |
| 7 | praxis-core shim pruned | DONE | 9 dead modules + 7 dead tests + dead `LawObject::transition` removed; `cargo check -p praxis-core` zero warnings |
| 8 | praxis-core full retirement | UPSTREAM-DONE — cutover PENDING | steps 1-4 (record/chain/epoch/Andon, validator, store) COMPLETE in `~/graphlaw` (golden byte-exact; validator 405/0 w/ abi; JSONL store w/ genesis anchor); remaining: differential court + seam rewire + cutover |
| 9 | OCEL determinism root-caused | DONE | stale ambient binary via CliHarness PATH fallback; 3-test court added |
| 10 | OCEL flake closed | DONE | book_gap 2/0, dry-run 1/0, invariant matrix 13/0 |
| 11 | Workspace [lints] wired into vendored crates | DONE | 5 vendored crates now carry `[lints] workspace = true` (incl. bcinr-pddl) |
| 12 | Crate-count alignment (repo-facts.ttl / architecture.md / CLAUDE.md) | DONE | all three say 16; parity test 2/2; generation now template-true |
| 13 | bcinr-pddl defect fixes | DONE | 3 NaN-panic comparators + 2 fail-open inserts fixed; lib 91/0 |
| 14 | CLI --json alias | DONE | native `--format json` discovered; alias added; CLI suite green (1 pre-existing failure reproduced at HEAD) |
| 15 | Marketplace composer revived | DONE | 4-refusal taxonomy; ggen-marketplace lib 436/0 |
| 16 | graphlaw sibling ALIVE | DONE | 420/0 |
| 17 | rustc warnings → "allow" flip | DONE | `Cargo.toml:306` warnings="warn"; 14-crate RUSTFLAGS="-W warnings" inventory = 0 warnings; 1 fix (ggen-config parser.rs:13); ggen-config 174/0 |
| 18 | sync.rs packs → build_tera_with_packs wiring | DONE | both pipelines wired: `sync.rs:1009/:1063` + `generation_rules.rs:528` declarative-rules path (Pack shim from manifest `[[packs]]`; generation_rules_e2e 28/0, cross_pack 6/0, typed_causes 8/0, rules_n3 4/0) |
| 19 | Engine all-targets clean-machine rerun | PENDING | contention run: 710 passed / 32 load-flavored / 3 stable byte-identity class — needs quiet-machine classification |
| 20 | abb-sbb Cargo.lock + target deletion | PENDING | permission-denied; user-run |
| 21 | oclnr /tmp plan (~35 GB) | PENDING | `/tmp/oclnr-lane-plan.json` awaiting user approval |
| 22 | ggen-engine publish=true | PARTIAL | publish-safety comment corrected in root Cargo.toml (measured: 0 absolute sibling-repo path deps; blockers = 4 in-workspace path-only deps); flip pending praxis-core retirement (#8) + cutover |
| 23 | Ambient ggen 26.9.28 stale install | PENDING | user should reinstall/upgrade — CliHarness PATH hazard |
| 24 | CLI surface: `pack capabilities` verb | DONE | `crates/ggen-cli/src/cmds/pack.rs:1004`; name/provides/requires/satisfied_by; guide `docs/pack_capabilities_guide.md` landed + fact-checked |
| 25 | Corpus data-quality court + prose-only purge | PARTIAL | corpus court 401/401 + deterministic; dangling requires 0; sample court found 76% prose-only → purge in flight; PackFile capabilities plumbing part 1 landed (`capabilities Option`), composer part 2 in flight |

## Closing drift reconciliation (2026-10-09)

Numbers above were captured mid-wave; appended corrections from the settled tree (appends
only — original text above is retained as-written).

| Claim | Was | Now | Evidence |
|---|---|---|---|
| DoD 18: sync.rs packs → build_tera_with_packs wiring | PARTIAL (in flight) | **DONE** | `sync.rs:1009` and `:1063` both call `build_tera_with_packs(graph, &packs)` (import at `sync.rs:52`) |
| DoD 5 / §4.3 GraphLawStore still dual-sourced on `praxis_graphlaw::TripleStore` (`graph.rs:1157`) | NOT yet repointed | **REPOINTED — DONE** (matches DoD 5) | `grep praxis_graphlaw::TripleStore crates/ggen-engine/src/graph.rs` → 0 hits |
| Final star-toml full-grep recount (§7 "PENDING", DoD 1 "= 9 sites, 0 unexpected") | 9 sites | **11 raw sites = 9 documented exceptions + 2 new** in `ggen-config/tests/pack_corpus_schema_test.rs:32,102` (composer corpus arbitration test, annotation fan-out lane; test-side raw parse) | `grep -rn "toml::from_str" crates --include='*.rs' \| grep -v star_toml` |
| DoD 15: composer corpus arbitration data | DONE (revival only) | **IN PROGRESS — annotation fan-out landing**: 87 `pack.toml` files modified per `git status --short` (20-lane capability annotation wave) | `git status --short \| grep -c pack.toml` → 87 |
| DoD 17: rustc warnings → "allow" flip | PARTIAL (in flight) | still in flight (this repo's lane) — upstream, not reconcilable here | unchanged |
| DoD 6: refuse-effect hook upstream support | PENDING | still in flight (BD lane) | unchanged |
| DoD 8: receipt_chain port to graphlaw | PARTIAL | still in flight in `~/graphlaw` (BE lane) — upstream of this repo; blueprint `docs/v26_10_10_praxis_retirement_plan.md` | unchanged |
| DoD 19: engine all-targets clean-machine rerun | PENDING | still in flight (BG harness lane) | unchanged |
| Verifier finding 1 (grep 10→9 fix at `cross_pack_macros_e2e.rs:75`) | reconciled in § above | confirmed present in this receipt | § "Independent verification" item 1 |
| Verifier finding 2 (gate-b fragility, ggen_bin() fallback) | reconciled | confirmed present | § item 2 |
| Verifier finding 3 (praxis-core deletion receipt itemization) | reconciled | confirmed present | § item 3 |

Still open after reconciliation: DoD 6 (BD refuse-hook, upstream), DoD 8 (BE receipt_chain
port, `~/graphlaw`), DoD 17 (BC rustc-warn flip), DoD 19 (BG harness), DoD 15 annotation
tally (fan-out running, 87 pack.toml modified), and the user-gated items 20/21/23.

## Lane receipts (receipt-bc, appended 2026-10-09)

| Item | Evidence | Status |
|---|---|---|
| rustc-warn flip | Root `Cargo.toml:306` `warnings = "warn"` (was allow); full 14-crate inventory via `RUSTFLAGS="-W warnings"` = **0 warnings**; one real fix `crates/ggen-config/src/manifest/parser.rs:13` (unused import); ggen-config tests **174 passed / 0 failed**. Methodology traps recorded: a trailing `-W warnings` cargo arg errors silently; cargo caches warnings — touch `.rs` files to force re-emission | **DONE** |
| Pack annotation fan-out | **401/401 annotated** — re-verified on disk this append: 93 `packs/*/pack.toml` (ggen) + 308 `ggen-marketplace/packs/*/pack.toml` carry `[capabilities]`; tomllib-clean after corpus-validate merged 86 duplicate-`[capabilities]` tables from a concurrent-writer overlap; integrity court: 0 dangling requires within ggen/packs, 0 URN collisions intra-corpus, 52+ cross-repo mirror URNs flagged (design follow-up in flight) | **CLOSED** |
| GraphLawStore refuse-effect GAP | Upstream closed in `~/graphlaw` (`Effect::Refuse`/`Verdict::Refuse`, backward-compatible, **432/0**); ggen consumer rewiring in flight | **upstream DONE / consumer PARTIAL** |
| graphlaw::receipt_chain | Port complete in `~/graphlaw`, golden byte-exact, **428→432/0**; validator/store port + engine seam rewire + differential court in flight — praxis-core retirement **unblocked** | **COMPLETE (step 1)** / steps 2-4 in flight |

DoD rows updated: #6 → upstream-DONE/consumer-PARTIAL; #8 → step 1 DONE (retirement unblocked); #17 → DONE.

## Appended: Pack-Capability Annotation Fan-Out Receipt (lane annot-receipt, 2026-10-09)

### Design

Capability annotation wave over both pack corpora (`/Users/sac/ggen/packs`,
`/Users/sac/ggen-marketplace/packs`): 16 alphabetical lanes plus one full-sweep lane,
followed by ~25 further audit/re-sweep lanes. Evidence contract enforced per lane:
`provides` = the pack's own self URN only; `requires` written only on direct evidence
(ontology text, template references, declared dependencies). Prose mentions never
become `requires`.

### Measured final numbers (real sweep, 2026-10-09)

| Metric | ggen/packs | ggen-marketplace/packs | Total |
|---|---|---|---|
| pack.toml files | 93 | 344 | **437** |
| carrying `[capabilities]` | 93 | 308 | **401** |
| non-empty `requires` (packs) | — | — | **157** |
| total `requires` URNs | — | — | **450** |
| distinct `provides` URNs | — | — | **330** |

- Cross-corpus duplicate `provides` URNs (same URN declared in both trees): **71** —
  expected pattern for mirrored packs (e.g. `urn:ggen:pack:mfact-pack`,
  `urn:ggen:pack:tcps-release-pack` present in both corpora). Within-corpus duplicates: 0.
- Dangling `requires` (URN not provided by any pack): **40**, in two classes —
  (a) 10 refs to packs absent from both corpora (deckgl/shadcn families, 1 ref each),
  (b) ~30 bare concept URNs without the `urn:ggen:pack:` prefix (e.g. `authority-separation`,
  `replay`, `receipt`), i.e. requires pointing at non-pack capability names. Both classes
  are recorded, not silently coerced.

### Incidents

1. **Intermittent permission denials** during lane sweeps, root-caused to lost execute
   bits on pack directories; restored with `chmod -R u+x` over both corpora roots.
2. **Lane-partition overlap on `~/ggen/packs`**: two lanes touched the same files;
   reconciled by a second evidence pass re-deriving `[capabilities]` content from file
   state rather than lane reports.
3. **Concurrent-writer anomaly on 13 files**: multiple writers observed on 13 pack.toml
   files during the wave; final state arbitrated by the re-sweep, no content loss detected.

### Guard tests pinning the data

- `crates/ggen-config/tests/pack_corpus_schema_test.rs` — 5 passed / 0 failed
  (pinned against a real corpus run; also the site of 2 documented star-toml exceptions
  at lines 32/102, per the reconciliation table above).
- `crates/ggen-marketplace/tests/capability_corpus_test.rs` — **FAILED 3/0 passed**
  (real run 2026-10-09): `corpus_composes_and_capability_uris_are_uniqe`-family —
  `corpus_composes_and_capability_uris_are_unique`, `synthetic_duplicate_capability_is_refused`,
  `synthetic_unbound_requirement_is_refused`. Consistent with the measured corpus: 71
  cross-corpus duplicate provides URNs and 40 dangling requires violate the uniqueness and
  closed-world invariants the test asserts. This test is the enforcement gap, not a flake.

### Honest status

- IN PROGRESS: composer corpus test threshold (DoD 15 tally) — annotation data is landed
  and measured above; the arbitration-test threshold against this data is not yet settled.
- UNRESOLVED: whether `PackFile` parsing should canonicalize bare-concept `requires`
  URNs (class (b) above) or refuse them — 40 dangling entries currently pass through
  unvalidated. Open question for the capability-resolver owner.

## Appended: Validator+Store Port / Retirement / CLI Receipt (lane receipt-validate-doc, 2026-10-09)

### (a) receipt_chain::validator + receipt_chain::store — COMPLETE in `~/graphlaw`

- `~/graphlaw/src/receipt_chain.rs`: `Clock` trait + `SystemClock`/`FixedClock`
  (receipt_chain.rs:1597-1619); 4-stage validate — schema → chain_recompute →
  chain_linkage → monotonic. `token_replay` stage deliberately omitted pending
  `replay_adapter` — disclosed GAP, not a silent pass.
- `~/graphlaw/src/receipt_store.rs`: JSONL store with genesis anchor.
- `CoreError::Io` parity with upstream error text — byte-matched.
- Tests: `~/graphlaw/tests/receipt_chain_validator_test.rs` — **405 passed / 0 failed**
  with the `abi` feature. Note (pre-existing): bare `cargo test` without `--features abi`
  fails the feature-gated `wasm_abi` tests.

### (b) praxis retirement blueprint status

Steps 1-4 (record/chain/epoch/Andon, validator, store) **DONE upstream** in `~/graphlaw`.
Remaining for full retirement: differential court, engine seam rewire, cutover
(blueprint: `docs/v26_10_10_praxis_retirement_plan.md`).

### (c) CLI `pack capabilities` verb — LANDED

`crates/ggen-cli/src/cmds/pack.rs:1004` — `pub fn capabilities(name: String)`, emits
`{ name, capabilities: { name, provides, requires, satisfied_by } }` (or
`capabilities: null` for packs without `[capabilities]`). Spot-checked on disk.

### (d) Capability guide — LANDED + fact-checked

`docs/pack_capabilities_guide.md` landed; fact-check lane issued 3 corrections
(FM-PACK-018 attribution, CLI section, 71-URN duplicate count) — applied in the guide
itself. Cross-reference sweep of this receipt doc: no stale citations, no fixes needed.

## Lane receipts (receipt-rows, appended 2026-10-09)

| Item | Evidence | Status |
|---|---|---|
| SJIRA-03 declarative-rules pack wiring | `generation_rules.rs:528` wires resolved packs into `build_tera_with_packs` via a lightweight Pack shim built from manifest `[[packs]]` path entries (deliberately lighter than marketplace Pack — zero drift); tests: generation_rules_e2e **28/0**, cross_pack **6/0**, typed_causes **8/0**, rules_n3 **4/0** | **DONE** |
| Publish-safety comment correction | root `Cargo.toml` comment fixed; measured: **zero** absolute sibling-repo path deps; real blockers are **4 in-workspace path-only deps**; SJIRA-15/16 unchanged-or-unblocked | **DONE** (comment); DoD 22 flip PARTIAL |
| PackFile capabilities plumbing part 1 | `capabilities: Option` on PackFile landed; composer part 2 in flight; corpus court **401/401** + deterministic; dangling requires **0**; sample court measured **76% prose-only** → purge in flight | **PART 1 DONE** / part 2 in flight |

DoD rows updated: #18 → DONE (both pipelines); #22 → PARTIAL (comment fixed, flip pending
cutover); #25 added (corpus data-quality court + purge).

## Final session deltas (lane docs-final, appended 2026-10-09)

1. **Prose-only `requires` purge — DONE.** 297 → **37** requires (260 prose-only deletions;
   breakdown by class: collision-note templates ~30, mirrors/ported notes ~50, provenance
   sourcePath ~20, history/DRIFT_LOG tables, descriptor triples, substring false-positives,
   0-hit ghosts). All 37 kept requires are contract-evidenced (prefix-binding, path imports,
   script loads); 1 UNCERTAIN (greene→strategic-doctrine). Independent court 2 pending.
2. **Composer part 2 — VERIFIED on disk this append.** Capabilities-aware arbitration live in
   `crates/ggen-marketplace/src/packs_registry/composer.rs` (provides universe = `packages` ∪
   `capabilities.provides`; requires universe ∪ `capabilities.requires`, doc :25-52);
   `DuplicateCapability` fires on same-URN `capabilities.provides` (:91,:144-156) — cross-corpus
   mirrors refuse **BY DESIGN as tripwire**; arbitration tests 11/0. `PackFile` from-dir API
   landed (6/0); parse unit tests 10/0 with 2 documented parser divergences
   (deny_unknown_fields asymmetry; Vec-vs-BTreeSet duplicate handling).
3. **URN dedup proposal — FALSIFIED + WITHDRAWN (SJIRA-13).** Recount: 64 dedup candidates in
   ggen/packs vs 1 in marketplace (reversed from the original claim); both corpora are live;
   cross-corpus mirror refusal is the intended tripwire stance. Canonical census re-measured on
   disk this append: **403 manifests** (95 `packs/*/pack.toml` + 308
   `ggen-marketplace/packs/*/pack.toml`) / 331 unique names / 72 mirrors.
4. **Version alignment steps 1-4 — edits landed.** All path+version declarations → 26.10.8;
   residual 26.7.13 engine edges are cutover-coupled. Gates pending disk-recovery.
5. **Publish-safety — comment corrected (supersedes DoD 22 evidence text).** Zero absolute
   sibling-repo paths exist; only **4 in-workspace path-only deps** block publishing.
6. **Nested test targets verdict (ggen-cli): archive / DEAD / DEAD**; 261-site harness
   migration; 6 targets fixed.
7. **Engine smoke:** 403 pack.tomls through the strict `PackToml` contract — **0 refusals**.

### DoD row updates (final)

| # | DoD item | Was | Now |
|---|---|---|---|
| 25 | Corpus data-quality court + prose-only purge | DONE (purge) / VERIFIED (composer part 2) — purge 297→37 all contract-evidenced (1 UNCERTAIN; court 2 pending); composer arbitration live 11/0, from-dir 6/0, parse 10/0 (2 documented divergences); 403-manifest strict-PackToml smoke 0 refusals — **SUPERSEDED (docs-fold-2, 2026-10-09): corpus data-quality DONE** — danglers fixed (cross_corpus_tripwire 2/0, capability_corpus 3/0), swallowed-requires repair complete (403 manifests / 104 in-caps requires / 0 dangling / 0 bare / 0 stray-top-level), smoke Tier-1 4/0 |
| 22 | ggen-engine publish=true | PARTIAL | **PARTIAL (evidence corrected)** — zero absolute sibling-repo paths; blockers = 4 in-workspace path-only deps; flip still pending praxis-core retirement (#8) + cutover |
| 22 | ggen-engine publish=true | PARTIAL | **flip LANDED (receipt-cutover, 2026-10-10)** — ggen-engine `publish = true` in root Cargo.toml; dry-run verification PENDING |
| 19 | Engine all-targets clean-machine rerun | PENDING | **PARTIAL** — nested test targets adjudicated (archive/DEAD/DEAD); 261-site harness migration done; 6 targets fixed; quiet-machine full rerun still pending disk-recovery |
| 15 | Marketplace composer revived | DONE | **DONE + VERIFIED** — capabilities-aware arbitration live (11/0); cross-corpus DuplicateCapability refusal is design-intent tripwire, not a defect; SJIRA-13 dedup withdrawn |

### Correction (2026-10-09, requires-reconcile): purge count was a scope artifact

The purge lane's "297→37 requires" covered only 89 of 403 packs (its snapshot file's scope). The
true post-purge corpus state, measured fresh and deterministic: **64 packs with non-empty requires,
140 entries, 91 distinct URNs, 0 dangling, 0 bare names** (split 12 ggen / 52 marketplace). The
unenumerated 102 entries came from later dedup/annotation lanes; a 10-pack / 26-edge sample verified
all contract-clean (ontology prefix-bindings, consumer.ttl imports, template/script loads). No
re-purge needed. Canonical numbers for all consumers: **403 manifests, 331 unique names, 72 mirror
pairs, 140 requires entries** — see SJIRA-261010-11 reconciliation note and SJIRA-261010-12 census.

## Closing consistency pass (verifier V18, 2026-10-09)

Fresh census re-run on disk (tomllib over all 403 manifests):

- **Census**: 403 manifests (95 ggen + 308 marketplace) confirmed. Unique names = **332 by BOTH
  keying methods** ([pack].name-keyed and directory-keyed); mirror pairs = **71** (142 entries in
  71 dup groups). The 331/72 figures in the requires-reconcile note above and correction #3 are
  WRONG; the guide's 71/332 is correct. Distinct `capabilities.provides` URNs = 332.
- **Requires (supersedes the 64/140/91 reconcile note)**: live tree measures **49 packs with
  non-empty requires, 104 entries, 74 distinct requires URNs, 0 dangling, 0 bare names**. The
  64/140/91 figure did not reproduce; a further dedup evidently landed after that note. Receipts
  citing requires counts should cite 49/104/74 as-of this pass.
- Composer part 2 VERIFIED 11/0, tripwire test
  `crates/ggen-marketplace/tests/cross_corpus_tripwire_test.rs` present, purge-lane "37" scope
  artifact framing: all confirmed consistent with the tree and the other three docs.

### Count corrections (lane census-recount, 2026-10-09)

The corpus-census figures in items 3 and 7 and the "Correction (requires-reconcile)"
note above are superseded by a full-tree recount (no maxdepth, two deterministic
runs): **439 manifests** (95 `packs/*/pack.toml` + 344
`ggen-marketplace/packs/*/pack.toml` incl. 36 nested) / **338 unique
[pack].name** / 368 unique dir names / **71 mirror pairs** / 105 requires
entries, **2 dangling** (both in
`ggen-marketplace/packs/chatman-ecosystem-v26-9-1-release-gate/pack.toml`).
The prior 403 / 331 / 72 (and 332-name) figures were `maxdepth 2` top-level
scopes; top-level-only re-measure today gives 403 / 332 / 332 / 71. Growth
403→439 = nested marketplace manifests only (dfcm-pack families +
ggen-pack-spec-pack qualification fixtures). Canonical census:
SJIRA-261010-12 census re-count section.

## DoD gate receipts (V-waves), consolidated 2026-10-09 (lane dod-consolidate)

~16 DoD gate receipts landed since the last consolidation (V1–V20 waves). Tallies below
cite the completed lane receipts (verifier lanes + gate lanes); in-flight lanes marked.

| Gate | Lane | Tally / verdict |
|---|---|---|
| V16 version alignment | ver-16 | **VERIFIED** — single 26.10.8 across the tree, zero version conflicts; dirty-tree-only refusal behavior confirmed |
| V17 static claims audit | ver-17 | **VERIFIED — 3 findings**: (1) 14→9-expectation toml count discrepancy with 5 undocumented session sites; (2) praxis trajectory claim; (3) 439-not-403 census; plus 5 lint exceptions |
| V13 determinism | ver-13 | **VERIFIED — 19 passed / 0 failed** on quiet machine |
| V1 config gate | ver-1 | **VERIFIED — 178 passed / 0 failed** |
| V2 marketplace capability | ver-2 | **VERIFIED — all 7 suites green** after capability_corpus bind tightening + pack_composition **11 passed / 0 failed** |
| V7 abb-sbb + cheat-scanner | ver-7 | **VERIFIED — abb-sbb 59 passed / 0 failed; cheat-scanner 19 passed / 0 failed** |
| V8 vendored trio | ver-8 | **VERIFIED — 91 / 28 / 37** (bcinr-pddl lib / schedule-related / third target), all 0 failed |
| V18 docs consistency | ver-18 | **VERIFIED — 332/71 canonical**, requires live count 49 packs / 104 entries / 74 distinct URNs; see evolution note below |
| V11 receipt family | ver-11 | **VERIFIED — 25 passed / 0 failed + 4 passed / 0 failed** (differential court pending its background run) |
| V19 engine coverage | ver-19 | **IN FLIGHT** |
| V20 cli coverage | ver-20 | **IN FLIGHT** |
| V10 law gates | ver-10 | **IN FLIGHT** |
| V12 sync family | ver-12 | **IN FLIGHT** |
| V14 parser court + corpus | ver-14 | **VERIFIED** — parser differential court 5/0 after fixes; swallowed-requires repair landed; see V14 closure below |
| V5 lsp gate | ver-5 | **VERIFIED — ggen-lsp 367/0** post-version-alignment (40 targets) |
| V15 clippy gate | ver-15 | **PARTIAL** — marketplace src-clean 0 (124 test-only warnings); style-class residual; closeout lane in flight |

### Requires live-count evolution (authority chain)

140 entries (requires-reconcile note) → **104 entries / 49 packs / 74 URNs** after the
swallowed-requires repair + prose purge. Authority chain: **corpus-final-court → V18
consistency pass** (V18 measured 49/104/74 live and is the as-of citation; the corpus
court is the deterministic gate). Consumers citing requires counts should cite 49/104/74
via V18.

### Corrective-lane closures (this wave)

1. **Stray `toml::from_str` → `star_toml::from_str` × 5** — 5 undocumented sites fixed,
   restoring the 9-documented-exception invariant (V17 finding 1 closed).
2. **cheat-scanner lints wiring** — crate lint policy wired; 19/0 green (V7).
3. **Census recount 439 / 332 / 71 canonical** — full-tree deterministic recount
   superseded the maxdepth-2 403/331 figures (V17 finding 3 + census-recount lane,
   SJIRA-261010-12).

### DoD row updates (V-waves)

| # | DoD item | Was | Now |
|---|---|---|---|
| 9/10 | OCEL determinism + flake | DONE | **DONE + VERIFIED** — V13 determinism 19/0 quiet-machine |
| 11 | Workspace [lints] in vendored crates | DONE | **DONE + VERIFIED** — V7 cheat-scanner 19/0 after lints wiring closure; V8 vendored trio 91/28/37 |
| 15 | Composer revived | DONE + VERIFIED | **DONE + VERIFIED** — V2 all 7 marketplace capability suites green; pack_composition 11/0 |
| 12 | Crate-count alignment | DONE | **DONE + VERIFIED** — V18 docs consistency 332/71 canonical; 16-crate alignment confirmed live |
| 1 | star-toml migration | DONE | **DONE + VERIFIED** — 5 stray-site corrective closure; V17 finding 1 resolved |
| 19 | Engine all-targets rerun | PARTIAL | **PARTIAL** — V19 lane in flight |
| 8 | praxis-core retirement | cutover PENDING | **unchanged** — V11 receipt family 25/0 + 4/0 green; differential court pending bg run |
| 8 | praxis-core retirement | cutover PENDING | **EXECUTED, GATES-PENDING** (receipt-cutover, 2026-10-10): steps 1-6 done — golden-only diff test, deps removed, 14 members, crates/examples git rm'd, 5-site comment sweep, metadata OK, tree -i empty; gate ladder in cutover-gates lane |

## V14 closure: tripwire + parser differential courts + swallowed-requires
repair (appended 2026-10-09, lane docs-tripwire)

Two court results landed after the last docs pass:

1. **Cross-corpus tripwire court**
   (`crates/glen-marketplace/tests/cross_corpus_tripwire_test.rs`, **2/0**)
   **FALSIFIED** the composer docstring's refusal claim: same-named mirrors dedupe
   by id, cross-corpus compose silently merge-collapses 403→332, and the composer
   never refuses on mirror self-URNs. Positive controls (each corpus alone) Ok.
   Composer docstring correction is in flight by another lane. This supersedes
   the earlier "DuplicateCapability refusal is design-intent tripwire" reading in
   the final-session delta — the court shows no refusal fires.
2. **Parser differential court**
   (`crates/ggen-engine/tests/pack_capabilities_parser_diff_test.rs`, **5/0**
   after fixes). Found identity-field injection asymmetry (marketplace `Pack`
   requires `id` etc. — resolved via defaults-injection) and empty-vs-absent
   normalization drift (engine `Some({})` vs marketplace `None`). Caught **5 real
   missing-description corpus defects** (FM-PACK-003 refusals:
   automatic-autonomic-operations, certification-assist ×2, ontostar-mustar,
   speedrun) — all fixed.
3. **Swallowed-requires repair**: 154 files had `requires` lines trapped outside
   `[capabilities]` (top-level or docstring-swallowed) — repaired into
   `[capabilities]` in a 154-file sweep. Corpus now **403 manifests / 104
   in-capabilities requires / 0 dangling / 0 bare / 0 swallowed**. This grounds
   the 49/104/74 live count cited via V18 (authority chain unchanged).

### Closing verifier pass 1 (2026-10-09)

Independent verifier ran CLOSING_VERIFIER_BRIEF claims: **15 VERIFIED, 8 PENDING-IN-FLIGHT, 2 FAILED-as-observed, 1 PARTIAL.**

- FAILED A8 (pack_composition 10/1): mid-flight churn from the corpus-bind lane; re-verified 11/0 twice post-settle.
- FAILED A18 (workspace version 26.10.8 ≠ 26.10.10): real — the version BUMP is a release-cut action (SJIRA-22), not yet performed; expected red at this stage.
- PENDING: differential court bg run, graphlaw workspace 432/0 independent, parser court (lock-blocked), composer corpus court, config suite (lock-blocked), workspace check/clippy, version gates, B4 census re-key.
- PARTIAL B4: naive-grep census (347 unique/15 mirror-mentions) disagrees with name-keyed census (332/71); name-keyed corpus court is authoritative.
- New finding: 5 post-snapshot `toml::from_str` sites (compose verb's load_corpus_pack, mcp capability_status:116, capability_corpus_test:63, parser_diff_test:47,64) — cleanup lane in flight.
- Praxis cutover: **NO-GO** on G2 (FM-PACK-018 two-tier was mid-re-apply during pass 1; now landed and gate-verified by V10/V12/V13) — final go awaits callsite-migration finals + SJIRA-23 port.

## Folded receipts (final wave, appended 2026-10-09, lane receipt-vclose)

| Item | Evidence | Status |
|---|---|---|
| Compose verb order-determinism fix | `pack.rs:1117-1128` sort; test rewritten without
  canonicalization; tallies **5/0 + 5/0 + 174/0** | **DONE + VERIFIED** |
| Composer e2e edge expansion | **16/0** — 5 new edge cases incl. self-require
  self-satisfaction; findings: capabilities are semantic, not topology | **DONE + VERIFIED** |
| Guide sections | `docs/pack_capabilities_guide.md`: compose-verb,
  MCP, FM-PACK-018 two-tier sections landed | **DONE** |
| Composer docstring mirror-collapse correction | court showed compose silently
  merge-collapses mirrors (403→332), no refusal; docstring corrected | **DONE**
  (supersedes the "design-intent tripwire" reading) |
| Marketplace clippy | src-clean **0** warnings; 124 test-only; headers added to
  6 files + `pack_corpus_schema` | **DONE** (V15 style residual → closeout lane) |
| ggen-mcp lints wired | `[lints] workspace = true` wired; **36 lib doc reflows**
  in flight | **IN FLIGHT** |
| ggen-lsp version alignment | full suite post-alignment: **367/0**
  (40 targets) | **VERIFIED** (V5) |
| V13 quiet-machine determinism | **19/0** — closes the ENOSPC
  flake class | **VERIFIED** |
| FM-PACK-018 two-tier verdict | two-tier rule, `pack.rs:454-528`;
  3/4 targets verified, 4th's fixture fixed | **DONE + VERIFIED** (docs-fold-2:
  V10/V12/V13 + smoke 4/0 incl. negative case) |

### V-wave rows updated this append

| Gate | Was | Now |
|---|---|---|
| V5 ggen-lsp | (not listed) | **VERIFIED — 367/0** post-version-alignment |
| V14 parser court + corpus | VERIFIED (mid-fix) | **VERIFIED — final**
  (court 5/0, repairs landed) |
| V15 clippy | (not listed) | **PARTIAL** — src-clean 0, test-only 124; closeout lane in flight |
| V19 / V20 engine + cli coverage | IN FLIGHT | **IN FLIGHT** (unchanged, re-confirmed) |

## Appended: docs-fold-2 lane receipts (2026-10-09)

| Item | Evidence | Status |
|---|---|---|
| Cross-corpus danglers FIXED | 3 marketplace requires naming ggen/packs-only packs deleted (claude-code→tcps-core, schema-pack→specimen, counterfactual→fortune5-required-capabilities); re-verified on disk: `requires = []` in claude-code-pack + clap-noun-verb-schema-pack, no counterfactual refs remain; gates post-fix: cross_corpus_tripwire **2/0**, capability_corpus **3/0** | **DONE + VERIFIED** |
| Swallowed-requires repair (full receipt) | 154-file sweep moved stranded requires into `[capabilities]`; a2a-hex's docstring-trapped requires now visible → FM-PACK-018 fires correctly in smoke's negative case; corpus: **403 manifests / 104 in-caps requires / 0 dangling / 0 bare / 0 stray-top-level** | **DONE** |
| Smoke-declared Tier-1 positive case | cargo-cicd→clap-noun-verb provider pair; **4/0** incl. negative FM-PACK-018 | **DONE** |
| Composer e2e edge expansion | **16/0** — 5 edge cases: self-require self-satisfaction pinned Ok; capabilities.requires NOT topology edges pinned; self-dup dedup pinned | **DONE + VERIFIED** |
| Compose verb order-determinism | `pack.rs:1117-1128` sort (re-verified on disk this append) | **DONE + VERIFIED** |
| ggen-lsp style sweep | crate itself 0 warnings (doc-paragraph pre-allowed at lib.rs:8); ggen-graph src: 0 mechanical issues (31 unwrap reported are cfg(test)/sabotage bins only) | **DONE** |
| Lint wiring | ggen-mcp wired; cheat-scanner wired (19/0, V7); ggen-engine intentional inline | **DONE** |
| Toolchain incident | pinned nightly-2026-06-22 cargo/rustc/rust-std went missing mid-session (disk-full fallout); reinstalled via `rustup component add` by the smoke-declared lane | **RESOLVED** |

## Appended: praxis cutover execution receipt (lane receipt-cutover,
appended 2026-10-10; steps 1-6 executed by cutover lane, GATES-PENDING)

| Item | Evidence | Status |
|---|---|---|
| Differential test → golden-only | praxis oracle removed; TCPS fixture retained | DONE |
| praxis deps removed from ggen-engine | `crates/ggen-engine/Cargo.toml` | DONE |
| Workspace members: 2 removed | 13 + root = **14 members** | DONE |
| `crates/praxis-core`, `crates/praxis-graphlaw`, `examples/praxis-core-verify` | `git rm` — recoverable from HEAD | DONE |
| ggen-engine publish=true | root Cargo.toml flip | DONE |
| praxis comment sweep — 5 sites | ggen-mcp tool string; `receipt_verify.rs`; cheat-scanner `lib.rs`; ggen-lsp `ggen_construct.rs` (x2 class) | DONE |
| `cargo metadata` | OK | DONE |
| `cargo tree -i praxis-*` | empty (no inverse deps) | DONE |
| Cutover gates (full build/test/clippy ladder) | owned by cutover-gates lane | **GATES-PENDING** |

## Folded receipts (final wave 2, appended 2026-10-10, lane receipt-fold-3)

| Item | Evidence | Status |
|---|---|---|
| Praxis cutover EXECUTED + COMMITTED | main **b97f52dcc** — deps removed, differential golden-only, crates git rm'd (recoverable), publish=true, comment sweep 5 sites, repo-facts.ttl reconstructed (**14 rf:Crates**, counts 13/14 — earlier regex mangle recovered from main) | **DONE + COMMITTED** |
| Cutover gates | config **178/0**, parity **4/0**, governance **2/0**, `cargo tree -i praxis-*` empty; dry-run progressed to dirty-tree-only; graphlaw versioning completed (graphlaw 26.10.5 → marketplace 26.10.9 → abb-sbb 26.9.26); engine dry-run passes manifest verification; remaining: actual crates.io publish = release-cut execution | **DONE** (publish = release-cut residue) |
| DoD V-wave gates | V1-V28 summary rows consolidated (V16-V18, V1/V2/V7/V8/V11/V13/V14/V5 VERIFIED; V15 PARTIAL→closeout) | **VERIFIED** |
| Valve layout reconciliation | stratus **14/0** | **DONE** |
| Tier-2 smoke | **6/0** (incl. both FM-PACK-018 tiers) | **DONE + VERIFIED** |
| Topology experiment | KEEP — **4/4** | **DONE** |
| btree parity + corpus tightening | courts green | **DONE** |
| FM-PACK-018 two-tier row | **DONE + VERIFIED** — V10/V12/V13 + Tier-2 smoke 6/0 incl. both tiers | **DONE + VERIFIED** |

### Session totals receipt (full arc)

- ggen: **14-crate post-praxis workspace on main** (b97f52dcc).
- Strata v26.10.10 initial implementation: workspace **59/0**, valve **14/0**,
  wasm imports = ∅, **5 packs**.
- Praxis retirement: **EXECUTED** — commit pending on user action for graphlaw
  publish chain; remaining work = crates.io publishing (release-cut).

### DoD row updates (final wave 2)

| # | DoD item | Was | Now |
|---|---|---|---|
| 8 | praxis-core retirement | EXECUTED, GATES-PENDING | **EXECUTED + COMMITTED (main b97f52dcc)** — gates green (config 178/0, parity 4/0, governance 2/0); residual = crates.io publish (release-cut) |
| 22 | ggen-engine publish=true | flip LANDED, dry-run PENDING | **DONE through dry-run** — manifest verification passes post-versioning; crates.io publish is release-cut execution |
| 25 | Corpus data-quality | DONE (superseded) | **DONE + VERIFIED** — Tier-2 smoke 6/0; btree parity; corpus tightening landed |
