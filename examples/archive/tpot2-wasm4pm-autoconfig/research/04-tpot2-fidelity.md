# Research Dossier 04 — TPOT2 Fidelity Scorecard

**Agent:** 04 of 10 · **Date:** 2026-06-20 · **Scope:** read-only on the generator project
**Question:** How faithful is `examples/tpot2-wasm4pm-autoconfig/` to the **real** TPOT2, and what concrete
changes would deepen its fidelity?

**Network status:** Web search/fetch were **available**. Real TPOT2 facts below are cited to
EpistasisLab sources fetched this session. Where a default value could not be confirmed from the fetched
text (the docs site and PMC review describe mechanisms but omit some numeric defaults), I mark it
**[default unconfirmed]** rather than assert a number. No TPOT2 behavior is fabricated.

---

## 0. Sources

Real TPOT2 / TPOT (cited inline as `[Sn]`):

- **[S1]** Le, Fu & Moore (2020). *Scaling tree-based automated machine learning to biomedical big data
  with a feature set selector.* **Bioinformatics 36(1):250–256.** (The canonical TPOT citation; CONTRACT.md
  §0 and the config-dict template both cite this exact paper.)
- **[S2]** EpistasisLab/tpot2 GitHub — graph-based reimplementation; "hyperparameter ranges adapted from
  the original TPOT package or AutoSklearn." <https://github.com/EpistasisLab/tpot2>
- **[S3]** EpistasisLab TPOT2 docs site — parameters `population_size`, `generations`,
  `mutate_probability`, `crossover_probability`, survival/parent selection (NSGA-II / tournament /
  lexicase), `scorers` + `objective_functions`, complexity objective; search-space classes
  `SequentialPipeline`, `DynamicLinearPipeline`, tree, graph. <https://epistasislab.github.io/tpot2/>
- **[S4]** *The tree-based pipeline optimization tool* (Patterns review, PMC12416094): TPOT1 = expression
  **trees** (operators = internal nodes, hyperparameters = leaves); **TPOT2 = NetworkX DAG**; NSGA-II uses
  **nondominated binary tournament** selection (Pareto rank → crowding distance); two objectives =
  **predictive performance vs pipeline complexity (total #operators)**.
  <https://pmc.ncbi.nlm.nih.gov/articles/PMC12416094/>
- **[S5]** Springer "TPOT2: A New Graph-Based Implementation…" (custom NSGA-II replacing DEAP; flexible
  search-space definition). <https://link.springer.com/chapter/10.1007/978-981-99-8413-8_1>

Our real files (cited as `path:line`):

- `ontology/tpot-search-space.ttl` — stages, objectives, `Config_default`, hyperparameters.
- `inference/derive-operators.rq` — `pi:Algorithm → tpot:Operator` + computed fitness.
- `inference/derive-pareto-dominance.rq` — `tpot:dominates` edges.
- `queries/extract-pareto-pipeline.rq` — per-stage elite (the chosen pipeline).
- `queries/extract-pareto-front.rq` — global non-dominated set.
- `queries/extract-operators.rq` — full search space + `is_elite` flag.
- `queries/extract-hyperparameters.rq`, `queries/extract-fitness-objectives.rq`.
- `templates/tpot-config-dict.py.tera`, `ggen.toml` (driver), `INTEGRATION_REPORT.md`, `CONTRACT.md`.

---

## 1. Fidelity Scorecard (TL;DR)

| Axis | Real TPOT2 | Our model | Verdict |
|---|---|---|---|
| **A. config_dict / search space** | operator dict, each with hyperparameter ranges `[S2][S3]` | `tpot:Operator` derived per algorithm + `tpot:Hyperparameter` subset `[derive-operators.rq; tpot-search-space.ttl:186-298]` | **FAITHFUL (structure)** / **SIMPLIFIED (coverage)** |
| **B1. Multi-objective Pareto** | NSGA-II, 2 objectives, dominance + Pareto front `[S4]` | exact dominance rule + Pareto-front query `[derive-pareto-dominance.rq; extract-pareto-front.rq]` | **FAITHFUL** |
| **B2. Stochastic GP evolution** | population/generations/mutation/crossover, tournament survival over **generations** `[S3][S4]` | parameters **declared** but unused; selection is deterministic per-stage **argmax + global Pareto** `[Config_default; extract-pareto-pipeline.rq]` | **DIVERGENT (deliberate)** |
| **B3. Selection operator** | nondominated **binary tournament** (Pareto rank → crowding distance) `[S4]` | lexicographic argmax (fitness → speed → id), no crowding, no tournament `[extract-pareto-pipeline.rq:63-75]` | **DIVERGENT** |
| **C. Pipeline structure** | **graph/DAG** (TPOT2) / **tree** (TPOT1): stacking, feature unions, branching `[S4]` | **linear 9-stage** sequence, one operator per stage `[CONTRACT.md §4]` | **DIVERGENT (defensible)** |
| **D. Evaluation / fitness** | **k-fold CV score** (real model training) vs complexity `[S4]` | `qualityTier − 0.5·speedTier` from a **static registry** `[CONTRACT.md §5; derive-operators.rq:61]` | **SIMPLIFIED (proxy)** |
| **E. Hyperparameter tuning** | GP **searches** hyperparameter values during evolution `[S3]` | hyperparameter grid is **declared static metadata**, never sampled/optimized `[extract-hyperparameters.rq]` | **DIVERGENT (decorative)** |
| **F. Determinism** | `random_state` seeds a **stochastic** run (reproducible, not deterministic-optimal) | seed 42 declared; selection is **fully deterministic** by construction `[extract-pareto-pipeline.rq header]` | **DIVERGENT (intentional, for ggen)** |

**One-line honest summary:** We are **faithful to TPOT2's *declarative skeleton*** (a config_dict-style operator
search space and a true multi-objective Pareto model) and **deliberately divergent on its *engine*** — we
replace stochastic genetic-programming evolution over pipeline **trees/DAGs** with a **deterministic greedy
elite per stage + a global Pareto front** over a **linear** pipeline, because ggen requires byte-reproducible,
hash-stable output. The simplification is defensible **and** clearly disclosed in the project's own docs
(`ontology/tpot-search-space.ttl:29,33-41`; `CONTRACT.md §5`; `INTEGRATION_REPORT.md §6`).

---

## 2. Axis-by-axis analysis

### Axis A — config_dict / search space  → **FAITHFUL (structure), SIMPLIFIED (coverage)**

**Real TPOT2.** The search space is an operator catalog where each operator carries a set of
hyperparameters with ranges/choices; "many hyperparameter ranges … were adapted from the original TPOT
package or AutoSklearn" `[S2]`. TPOT2 lets you name operators broadly (`"transformer"`) or precisely
(`"LinearRegression"`) with hyperparameter ranges `[S3][S4]`.

**Ours.** This is our **strongest** fidelity point and the project's central thesis ("auto = derived, never
hand-coded"):

- `inference/derive-operators.rq:36-65` produces **one `tpot:Operator` per algorithm** (60→60), carrying
  `tpot:opSpeedTier`, `tpot:opQualityTier`, and a **computed** `tpot:fitnessScore`. The search space is not
  enumerated by hand — it is a `CONSTRUCT` over the `pi:` registry, so it grows automatically with the
  registry. This is exactly the spirit of TPOT2's config_dict ("the search space is *data*, not code").
- `templates/tpot-config-dict.py.tera:40-63` renders that into a literal Python `config_dict = { "wasm4pm.<stage>.<op>": {…} }`
  — structurally a TPOT-style config_dict, and it even cites `[S1]` in its docstring (`tpot-config-dict.py.tera:17-23`).
- Hyperparameters mirror TPOT's per-operator grid: `tpot:Hyperparameter` with
  `paramType ∈ {int,float,choice,bool}`, `paramMin/Max/Choices/Default`
  (`ontology/tpot-search-space.ttl:184-298`) — e.g. `heuristic_miner.dependency_threshold` float[0,1],
  `ml_classify.classifier` choice of 4. The types/ranges directly parallel TPOT's hyperparameter ranges `[S2]`.

**Gaps (SIMPLIFIED):**

1. **Coverage:** only **10 of 60** operators carry hyperparameters
   (`tpot-search-space.ttl:183` says so explicitly; the file declares Hp individuals only for
   heuristic_miner, inductive_miner, genetic_algorithm, ilp, alignments, ml_classify, detect_drift,
   ml_cluster). TPOT2's config_dict would specify a grid for **every** operator it can place.
2. **The grid is inert.** In TPOT2 the hyperparameter ranges are *searched* (sampled/mutated) during
   evolution `[S3]`. Here they are static documentation that never influences selection (see Axis E).
3. **No operator-type abstraction.** TPOT2 supports "broad vs precise" operator naming `[S4]`; our operators
   are always concrete (one wasm export each). Minor — process-mining operators are concrete by nature.

**Faithfulness on this axis is real** because the structure (operator dict + per-operator hyperparameter
ranges, derived from a registry) is genuinely TPOT-shaped, not cosmetic.

---

### Axis B — Genetic programming  → split verdict

This is the crux of the dossier. Split into three sub-axes.

#### B1 — Multi-objective Pareto modeling → **FAITHFUL**

Real TPOT2 NSGA-II optimizes two competing objectives — predictive performance and pipeline complexity —
and maintains a Pareto front where no solution can improve one objective without worsening another, via
Pareto dominance `[S4]`.

We model this **correctly and literally**:

- Two `tpot:FitnessObjective` individuals — `quality` (maximize, weight 1.0, order 1) and `cost` (minimize,
  weight 0.5, order 2) — `tpot-search-space.ttl:133-149`. This is a faithful (maximize score, minimize
  complexity/cost) objective vector.
- The **dominance relation is textbook Pareto** and matches `[S4]`:
  `derive-pareto-dominance.rq:55-58` emits `A dominates B` iff `qA≥qB ∧ sA≤sB ∧ (qA>qB ∨ sA<sB)`.
- `queries/extract-pareto-front.rq:48-57` computes the **globally non-dominated set** (the NSGA-II "first
  front") via `FILTER NOT EXISTS { other dominates me }`, ordered deterministically.

This is the part where our RDF model is **genuinely equivalent** to what NSGA-II computes for front-0
membership. **Verdict: faithful.** (Caveat: we map cost to `speedTier`, a coarse tier, whereas TPOT2's
complexity = literal operator count `[S4]`; see Axis D.)

#### B2 — Stochastic evolution → **DIVERGENT (deliberate)**

Real TPOT2 runs a stochastic loop: initialize a `population_size` population, evaluate, select parents,
apply mutation (`mutate_probability`) and crossover (`crossover_probability`), keep survivors via NSGA-II,
repeat for `generations` (or until a time budget) `[S3][S4]`. The result depends on the RNG seed.

We **declare** all of these and then **do not run them**:

- `tpot:Config_default` (`tpot-search-space.ttl:165-174`) sets `populationSize 100`, `generations 50`,
  `mutationRate 0.9`, `crossoverRate 0.1`, `selectionStrategy "NSGA2"`, `randomSeed 42`, `lambdaCost 0.5`,
  plus `tpot:metaBreed compat:Breed_version_space`.
- **No query or inference rule consumes any of these.** Grep-equivalent: `populationSize`, `generations`,
  `mutationRate`, `crossoverRate` appear **only** in `tpot-search-space.ttl` and prose
  (`INTEGRATION_REPORT.md` table) — never in `inference/*.rq` or `queries/*.rq`. There is no mutation
  operator, no crossover operator, no generational loop, no population.
- The **actual selection** is a deterministic per-stage argmax: `extract-pareto-pipeline.rq:63-75` picks, in
  each stage, the single operator that no other operator out-ranks under the strict total order
  `(fitness DESC, speed ASC, id ASC)`. `extract-operators.rq:54-73` computes the same as the `is_elite` flag.

So `mutationRate 0.9 / crossoverRate 0.1 / generations 50 / population 100` are **honest-looking but
non-functional declarations** — TPOT2's *vocabulary* without TPOT2's *dynamics*. The project is candid about
this: `tpot-search-space.ttl:29` calls the config a declaration of "NSGA-II GP hyperparameters (seed 42)" and
the whole file header (lines 33-41) explains operators are *derived*, not evolved.

**Is the simplification defensible?** **Yes**, and for a concrete reason rooted in this repo's constitution:

- ggen demands **deterministic, byte-reproducible generation** with cryptographic receipts (CLAUDE.md
  "Reproducibility 100%"; `coding-agent-mistakes.md §4` contract-drift rules; `RNG_SEED=42` determinism
  testing). A stochastic GP run produces a *different* pipeline per seed/run — incompatible with a
  hash-stable `ggen sync` and with the project's own determinism test ("byte-identical re-runs",
  `INTEGRATION_REPORT.md §4`).
- **For this specific search, greedy ≈ optimal.** Because the objective is *separable* — each stage's
  operator is chosen independently and the pipeline is a fixed linear concatenation with no
  cross-stage interaction in the fitness — the **per-stage argmax is the globally optimal linear pipeline**
  under the scalarized fitness. There is no epistasis between stages for ggen to discover, so a stochastic GP
  would, in expectation, *converge to the same per-stage elites* the argmax already yields. The greedy
  shortcut is not a lossy heuristic here; it is the exact optimum of the simplified problem.
- The one place "search difficulty" is non-trivial is **ties**: `extract-pareto-pipeline.rq` exercises the
  tie-break (the `conform` stage: `complexity_metrics` q90/s15/f82.5 beats `alignments` q95/s25/f82.5 only
  via the lower-speed tie-break — `INTEGRATION_REPORT.md §2`), proving the selection logic is genuinely
  exercised, not vacuous.

**Therefore:** label this **DIVERGENT but defensible** — *not* "faithful." The defensibility rests on (a)
ggen determinism being a hard constraint and (b) the problem being separable so greedy = optimal. The honest
risk is **misrepresentation**: a reader skimming `Config_default` could believe a stochastic NSGA-II run
happened. The mitigations (Section 4, R1/R2) reduce that risk.

#### B3 — Selection operator → **DIVERGENT**

Real TPOT2 parent selection is **nondominated binary tournament**: sample k pipelines (default 2), prefer
the one with better Pareto rank, breaking ties by **crowding distance** `[S4]`. Survival keeps the highest
NSGA-II fronts with greatest spacing.

Ours uses **no tournament and no crowding distance**. Selection is a deterministic lexicographic argmax
(`extract-pareto-pipeline.rq:70-74`). Crowding distance — which preserves diversity along the front — has no
analogue in our model at all. This matters less for *picking one linear pipeline* (we only need the per-stage
max) but it means we cannot reproduce TPOT2's *front exploration* behavior. **Verdict: divergent.**

---

### Axis C — Pipeline structure  → **DIVERGENT (defensible for process mining)**

Real TPOT pipelines are **non-linear**:

- **TPOT1:** an **expression tree** — internal nodes = ML operators, leaves = hyperparameters; cannot share a
  node's output to multiple consumers `[S4]`.
- **TPOT2:** a **NetworkX DAG** — outputs **branch to multiple nodes**, enabling **stacking** and
  **feature unions** without explicit combine operators `[S4]`. Search-space classes include
  `SequentialPipeline`, `DynamicLinearPipeline`, tree, and graph `[S3][S4]`.

Ours is a **strictly linear 9-stage sequence** (`CONTRACT.md §4`), one operator per stage, in fixed
lifecycle order (ingest → discover → … → orchestrate). There is no branching, no stacking, no feature union,
no variable depth. In TPOT2 terms our pipeline is closest to a **`DynamicLinearPipeline` with a fixed
9-stage schema** — i.e., we implement the *simplest* member of TPOT2's search-space family.

**Does it matter — and specifically for process mining?**

- **For AutoML generality:** yes, it is a real expressiveness gap. We cannot express "discover with
  inductive_miner *and* heuristic_miner, then ensemble," which a TPOT2 graph could.
- **For *process mining* semantics:** the linearity is largely **defensible and even natural**. A
  process-mining run is a pipeline of *stage transformations* with typed I/O
  (`pi:inputFormat`/`pi:outputType`, surfaced in `extract-pareto-pipeline.rq:58-60`): you ingest a log,
  discover a model, analyze it, check conformance, etc. The 9 stages are a **process lifecycle**, not
  interchangeable feature transformers, so a linear chain is a reasonable domain model. Stacking two
  *discovery* miners is meaningful AutoML but is **not** part of the wasm4pm registry's intended usage.
- **For process mining *of the generator itself* (Van der Aalst conformance, per `.claude/rules/
  process-mining-chicago-tdd.md`):** linearity is actually a **feature**. A fixed 9-stage linear sequence is
  trivially conformant — one variant, no hidden loops, no variant explosion. A stochastic GP over DAGs would
  produce *many* execution variants (the rule file explicitly flags "variant explosion" as a defect), which
  would *complicate* OCEL conformance checking. So for the repo's process-mining-validation doctrine, the
  linear deterministic pipeline is the **easier-to-prove-lawful** choice.

**Verdict: divergent in expressiveness, defensible for the domain and for the repo's own process-conformance
goals.** Worth stating explicitly in the report rather than implying TPOT2-grade pipeline topology (R5).

---

### Axis D — Evaluation  → **SIMPLIFIED (proxy fitness)**

Real TPOT2 evaluates each pipeline by **actually training it and computing k-fold cross-validation scores**
on real data (accuracy, precision, etc.), trading that against complexity = total operator count `[S4]`.
Fitness is *empirical and data-dependent*.

Ours uses a **static, data-independent proxy**:
`fitnessScore = qualityTier − 0.5·speedTier` (`derive-operators.rq:61`, `CONTRACT.md §5`), where
`qualityTier` and `speedTier` are **fixed integer tiers in the registry** (`algorithms.ttl`, e.g.
`dfg` q30/s1, `inductive_miner` q85/s10). Honest mapping of the analogy:

| TPOT2 concept | Our analogue | Faithfulness |
|---|---|---|
| CV score (maximize) | `qualityTier` (maximize) | proxy — a fixed expert prior, **not** measured on data |
| pipeline complexity = #operators (minimize) `[S4]` | `speedTier` as "cost" (minimize) | proxy — per-operator runtime tier, **not** pipeline-level operator count |
| scalarization for ranking | `q − λ·s`, λ=0.5 (`lambdaCost`) | reasonable scalarization; TPOT2 keeps objectives separate via NSGA-II and only scalarizes implicitly via dominance |
| data + `random_state` | none — deterministic registry lookup | **divergent**: no data, no training, no CV |

**Two honest caveats:**

1. **"Complexity" mismatch.** TPOT2's second objective is *structural* (count of operators in the pipeline)
   `[S4]`. Ours is *per-operator speed cost*. These are different quantities. Because our pipeline always has
   exactly 9 operators (fixed), a true TPOT2-style complexity objective would be **constant** and useless
   here — so substituting per-operator speed is a sensible adaptation, but it should not be *called* the same
   thing as TPOT2 complexity. (`tpot-search-space.ttl:22-24` already notes "speedTier is 'cost' not 'speed'.")
2. **No empirical evaluation at all.** The registry tiers are curated priors, not measurements. This is the
   single biggest fidelity gap after the greedy-vs-stochastic one: TPOT2's whole value is *discovering* that
   a pipeline scores well on *your* data; we *assert* quality from a fixed table.

**Verdict: a clearly-mapped proxy.** Defensible (the container has no data/sklearn/wasm runtime —
`CONTRACT.md §9`) and disclosed, but it is a *simplification of the objective*, not a faithful evaluation.

---

## 3. Where we are TPOT2-faithful vs where we simplify (consolidated)

**Faithful (keep, these are the project's real merit):**
- config_dict-style **operator search space**, *derived* from a registry (Axis A). `[derive-operators.rq]`
- **Two-objective** quality-vs-cost model with **textbook Pareto dominance** and a **non-dominated front**
  (Axis B1). `[derive-pareto-dominance.rq; extract-pareto-front.rq]`
- Per-operator **hyperparameter grid** with TPOT-shaped types/ranges (Axis A). `[tpot-search-space.ttl:184-298]`
- Explicit **scalarization weight** `lambdaCost`/objective weights mirroring (maximize score, minimize cost).

**Deliberately simplified / divergent (disclose loudly):**
- **Greedy per-stage argmax + global front** instead of **stochastic NSGA-II evolution** (Axis B2). Defensible
  because ggen needs determinism *and* the per-stage problem is separable so greedy = optimal.
- **Lexicographic argmax** instead of **nondominated binary tournament + crowding distance** (Axis B3).
- **Linear 9-stage** pipeline instead of **DAG/tree** with stacking/feature-unions (Axis C). Defensible for
  the process-mining domain and for the repo's own process-conformance (single variant, no variant explosion).
- **Static proxy fitness** (`q − 0.5·s` from fixed tiers) instead of **k-fold CV on real data** (Axis D).
- **Inert hyperparameter grid** — declared but never sampled/optimized (Axis E).

The project's own honesty is good: `ontology/tpot-search-space.ttl` (header + `Config_default` comment),
`CONTRACT.md §5`, and `INTEGRATION_REPORT.md §6` already disclose the determinism choice. The main residual
risk is that the **functional-looking but unused** GP parameters (`mutationRate 0.9`, `generations 50`, …)
could be mistaken for an executed evolutionary run.

---

## 4. Recommendations (concrete, implementable, file-targeted)

Ordered by fidelity-gain ÷ effort. Each "deepens authority or reduces drift" per `coding-agent-mistakes.md`.

### R1 — Stop the GP config from *lying*: make it self-documenting as a *declaration*  *(reduces drift; trivial)*
**Problem:** `mutationRate/crossoverRate/generations/populationSize` look executed but aren't (Axis B2).
**Fix:** add `tpot:executionMode "deterministic-greedy-elite"` (and optionally
`tpot:declaredButUnused "populationSize,generations,mutationRate,crossoverRate"`) to `tpot:Config_default`,
and surface it in the generated config_dict header. This makes the divergence machine-readable, not just
buried in prose.
**Files:** `ontology/tpot-search-space.ttl` (add predicate to `Config_default`, ~line 165-174);
`templates/tpot-config-dict.py.tera` (emit `EXECUTION_MODE = "deterministic-greedy-elite"` near
`RANDOM_SEED`, ~line 25-35); optionally a one-line note in `queries/extract-fitness-objectives.rq` header.

### R2 — Add a **Pareto-tournament selection** query (model NSGA-II selection, deterministically)  *(deepens authority; moderate)*
**Problem:** we have no analogue of NSGA-II's nondominated **binary tournament + crowding distance**
(Axis B3); selection is plain argmax.
**Fix:** add `queries/extract-pareto-tournament.rq` that, *within each stage*, ranks operators by
**(Pareto-rank, then crowding proxy)** instead of raw fitness — a deterministic surrogate of NSGA-II survival.
A workable deterministic crowding proxy over `(quality, speed)`: for each front member, the sum of normalized
gaps to its nearest neighbors in objective space (computed with `GROUP BY` + `MIN`/`MAX` subqueries).
Document that this is a *deterministic* stand-in for stochastic tournament. This is the single most
TPOT2-authentic addition because it touches the actual *selection operator*, not just the objective vector.
**Files:** new `queries/extract-pareto-tournament.rq`; wire as a new `[[generation.rules]]` in `ggen.toml`
(after the existing rules) emitting e.g. `generated/pareto_ranks.json` via the existing
`templates/pipeline-manifest.json.tera`; reference the contract's tie-break discipline in the header.

### R3 — Model **mutation & crossover as RDF operators** (so the GP vocabulary is *grounded*)  *(deepens authority; moderate)*
**Problem:** `mutationRate`/`crossoverRate` reference operators that don't exist in the ontology (Axis B2).
**Fix:** introduce `tpot:VariationOperator` individuals — e.g. `tpot:Mut_replace_operator`
("replace one stage's operator with another candidate in the same stage"), `tpot:Mut_tune_hyperparameter`
("perturb a `tpot:Hyperparameter` within its min/max"), `tpot:Xover_stage_swap` ("exchange the elite of one
stage between two parent pipelines") — each with `tpot:appliesToStage`/`tpot:appliesToHyperparameter`,
`tpot:variationKind ("mutation"|"crossover")`, and `tpot:probability` tied to `Config_default`. Even if not
*executed*, this makes the GP semantics first-class RDF (queryable, SHACL-checkable) and documents *what*
mutation/crossover would mean for a process-mining pipeline — directly mirroring `[S4]`'s mutation (structure
+ hyperparameters) and crossover (combine parent sections).
**Files:** `ontology/tpot-search-space.ttl` (new `VariationOperator` block + link from `Config_default`);
`ontology/tpot-shapes.ttl` (SHACL shape constraining the new class); optional
`queries/extract-variation-operators.rq` + a `[[generation.rules]]` row in `ggen.toml`.

### R4 — Broaden the **hyperparameter grid** toward TPOT2 coverage  *(reduces drift; low-moderate)*
**Problem:** only 10/60 operators have hyperparameters; the grid is also inert (Axis A/E).
**Fix:** extend `tpot:Hyperparameter` coverage to the operators that genuinely have tunable params per their
`algorithmDoc` — at minimum the remaining discovery miners (`aco` population/evaporation, `a_star`
heuristic-weight, `declare` support/confidence) and the prediction/simulation operators — citing
`algorithms.ttl` docs as the existing ones do (no invention). Optionally add `tpot:samplingStrategy`
("grid"|"random"|"loguniform") per param to make the *search* semantics explicit even though selection stays
deterministic.
**Files:** `ontology/tpot-search-space.ttl` (append Hp individuals, follow the §4 pattern at lines 184-298);
no query change needed (`extract-hyperparameters.rq` already projects all of them).

### R5 — Make the **linear-vs-DAG** simplification explicit in the search-space model and report  *(reduces drift; trivial)*
**Problem:** nothing in the model states we implement only TPOT2's *linear* search-space variant (Axis C);
a reader could assume DAG/stacking support.
**Fix:** add `tpot:searchSpaceTopology "DynamicLinearPipeline"` (and a comment citing TPOT2's
`SequentialPipeline`/`DynamicLinearPipeline`/tree/graph family `[S3]`) to the ontology, and a one-paragraph
"Fidelity & simplifications vs real TPOT2" section in the generated report template that lists: linear (not
DAG), greedy-deterministic (not stochastic), proxy fitness (not CV). This turns the disclosure into a
*generated artifact*, so it can't drift away from the code.
**Files:** `ontology/tpot-search-space.ttl` (predicate near the stage block or on a new
`tpot:SearchSpace` node); `templates/search-space-report.md.tera` (a "Fidelity vs TPOT2" section);
optionally cite `[S1][S4]` inline.

### R6 — Map fitness to a TPOT2-honest objective pair: keep cost, add a real **complexity** signal  *(reduces drift; low)*
**Problem:** we call `speedTier` "cost" but TPOT2's 2nd objective is *structural complexity* (operator count)
`[S4]`; conflating them overstates fidelity (Axis D).
**Fix:** either (a) rename/annotate the cost objective as `tpot:metric "speedTierCost"` with a comment that
it is a *runtime-cost* proxy, **distinct** from TPOT2 structural complexity; or (b) if R3 lands and variation
can grow pipelines, add a genuine `tpot:complexity` objective = number of operators/branches, making the
two-objective model structurally faithful. Lowest-effort is (a) — a labeling honesty fix.
**Files:** `ontology/tpot-search-space.ttl` (`Objective_cost`, lines 142-149, refine `objectiveName`/comment);
`queries/extract-fitness-objectives.rq` (no structural change; header note);
`templates/tpot-config-dict.py.tera` (`OBJECTIVES` comment, ~line 28-32).

**Suggested order:** R1 + R5 + R6 first (pure honesty/disclosure, near-zero risk, immediately reduce
misrepresentation drift) → R4 (coverage) → R2 + R3 (the two that actually deepen GP authority).

---

## 5. Verdict

The generator is **faithful where it claims its core thesis** (a derived, config_dict-style **operator search
space** with a **real multi-objective Pareto** model — Axes A and B1) and **deliberately, defensibly divergent
on TPOT2's engine** (deterministic greedy elite instead of stochastic NSGA-II evolution; linear instead of
DAG; proxy fitness instead of CV — Axes B2, B3, C, D). The greedy-vs-stochastic substitution is **justified**
by two facts: ggen mandates deterministic, hash-stable generation, and the per-stage selection problem is
**separable**, so greedy argmax is the *exact* optimum of the simplified problem (not a lossy heuristic). The
linear pipeline is additionally **advantageous** for the repository's own process-mining conformance doctrine
(single execution variant, no variant explosion).

The chief residual fidelity debt is **presentational**: GP parameters (`mutationRate`, `generations`, …) are
declared but unused, and `speedTier` is labeled "cost" though TPOT2's analogue is structural complexity.
Recommendations **R1, R5, R6** close that debt cheaply by making the divergence machine-readable and
generated; **R2 and R3** are the substantive moves that would deepen genuine TPOT2 fidelity (a deterministic
Pareto-tournament selection query, and RDF-modeled mutation/crossover operators).

**Sources:**
- [Le, Fu & Moore 2020, Bioinformatics 36(1):250–256 (TPOT)](https://academic.oup.com/bioinformatics/article/36/1/250/5511404)
- [EpistasisLab/tpot2 (GitHub)](https://github.com/EpistasisLab/tpot2)
- [TPOT2 documentation site](https://epistasislab.github.io/tpot2/)
- [The tree-based pipeline optimization tool (Patterns review, PMC12416094)](https://pmc.ncbi.nlm.nih.gov/articles/PMC12416094/)
- [TPOT2: A New Graph-Based Implementation… (Springer)](https://link.springer.com/chapter/10.1007/978-981-99-8413-8_1)
