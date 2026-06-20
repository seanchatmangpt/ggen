# Dossier 07 — End-to-End Determinism & the `ggen sync` SLO Budget

**Agent:** 07 / 10 · **Date:** 2026-06-20 · **Scope:** prove (or disprove) that the
TPOT2 wasm4pm auto-config generator is deterministic end-to-end, enumerate every
nondeterminism source in the real `ggen sync` path, estimate the triple count + the
RDF-processing SLO budget, and recommend concrete determinism/SLO hardening.

> **Method note (CONTRACT.md §9):** source-trace + structural only. This container
> has no `cargo`/`ggen` binary, so **no real `ggen sync` was executed** and no
> `.ggen/receipts/*.json` is fabricated. Every ggen-engine claim is cited to a real
> `crates/ggen-core/**/*.rs` line; every project claim to a real `.rq`/`.tera`/`.ttl`
> line. Throughout I distinguish **what the pure-Python reference PROVES** (byte
> identity, observed locally) from **what only a real ggen run would CONFIRM**.

---

## VERDICT

| Question | Answer |
|----------|--------|
| Is the **selection logic** deterministic? | **YES, proven.** The Python reference re-derives byte-identical artifacts across runs (`test_tpot2_autoconfig.py::test_elite_pipeline_is_deterministic`, lines 121-150). The math is integer/exactly-representable-float; the argmax is a strict total order. |
| Is **real `ggen sync` output** hash-stable? | **YES, expected — with ONE residual risk.** No timestamp/UUID/`now()` is injected on the generation-rule write path (`pipeline.rs:1037-1074`), templates carry no clock, and JSON is canonical. The ONE thing only a real run confirms: oxigraph's **row order is determined entirely by each SELECT's `ORDER BY`** (oxigraph honors it; absent it, order is storage-iteration-defined). All 6 SELECTs have a *total*-order `ORDER BY` → stable. |
| Does the **SLO budget** hold? | **YES, with wide margin.** ~1.3 k input triples + ~540 derived ≈ **<2 k triples total**, well under the "≤5 s / 1 k+ triples" SLO. The O(60²)=3 600-pair dominance CONSTRUCT is trivial at this scale. |
| Is `determinism_salt` doing anything? | **NO — it is a DEAD field.** Declared at `manifest/types.rs:252`, only ever set to `None`; **never read** anywhere in production logic (grep below). Setting it has **zero effect** today. |

**Net:** the generator is deterministic by construction. The single load-bearing
invariant the real engine relies on — and the one thing the Python reference does
**not** exercise — is **`ORDER BY` on every SELECT**, which is present and total.

---

## 1. Nondeterminism-source table (the whole pipeline)

Authoritative path (from Dossier 01 §0): `load → inference (CONSTRUCT) → SHACL
(skipped) → validation (skipped) → generation (SELECT → Tera → write)`. Every place
a value could vary run-to-run:

| # | Source | Controlled? | Evidence (file:line) |
|---|--------|-------------|----------------------|
| 1 | **Operator IRI minting** `IRI(CONCAT("…#Op_", ?algorithm_id))` | **YES — pure function of input.** Same `algorithm_id` ⇒ same IRI, in both CONSTRUCTs, so operator nodes and dominance edges line up regardless of inference order. No counter, no blank-node gensym, no salt. | `inference/derive-operators.rq:64`; `inference/derive-pareto-dominance.rq:61-62` |
| 2 | **Fitness float arithmetic** `BIND((?q − (0.5·?s)) AS ?fitness)` | **YES — exactly representable.** `q,s` are `xsd:integer`; `0.5` is a binary-exact dyadic; `q − 0.5·s` lands on a half-integer (…, 70.0, 82.5, 29.5) — all exact in IEEE-754. No accumulation, no rounding drift. The reference asserts EXACT equality on `ilp=70.0, alignments=82.5, dfg=29.5`. | `inference/derive-operators.rq:61`; proven `test_tpot2_autoconfig.py:94-118` |
| 3 | **SELECT row order (the big one)** | **YES — `ORDER BY` on all 6, each a TOTAL order.** ggen does **not** re-sort rows; `rows.push(row)` preserves oxigraph's solution order verbatim (`pipeline.rs:836-851`). Therefore order == whatever `ORDER BY` dictates. oxigraph evaluates via `SparqlEvaluator` natively (`graph/query.rs:104`, `graph/core.rs:589-595`), so it honors `ORDER BY`; **absent `ORDER BY`, order is store-iteration-defined** (deterministic *per build* but not contractually stable across oxigraph versions). All 6 clauses end on a key that is unique within the result set (see §1a). | rows: `pipeline.rs:836-851`; clauses: §1a below |
| 4 | **oxigraph internal store iteration order** | **MASKED by `ORDER BY`.** Backed by in-memory `Store::new()` (`graph/core.rs:118`). When a query has no `ORDER BY`, solution order follows triple-insertion / index iteration — stable within one process+build but NOT a stable contract. Our SELECTs all force an explicit total order, so this is neutralised for generation. Our two **CONSTRUCTs** have no `ORDER BY` — and that is *fine* (see §3). | `graph/core.rs:118,550,589` |
| 5 | **Tera context / row map iteration** | **YES — `BTreeMap` (sorted keys).** Each row is a `BTreeMap<String,String>` (`pipeline.rs:842`), so key iteration is lexicographic, not hash-random. Rows are serialised to a JSON **array** preserving #3's order (`serde_json::json!(rows)`, `pipeline.rs:1000`). No `HashMap` in the template context → no hashmap iteration nondeterminism. | `pipeline.rs:842, 1000-1009` |
| 6 | **Generated-code header / timestamp** | **YES — NOT injected on this path.** `canonicalize.rs::default_generated_header()` appends `chrono::Utc::now().to_rfc3339()` (`canonicalize.rs:358-368`) — that WOULD be nondeterministic, **but `canonicalize`/`inject_header` is never called by `execute_generation_rules`** (grep: zero hits in `pipeline.rs`). `Overwrite` writes raw `rendered` (`pipeline.rs:1048, 1074`). Our templates contain **no** `now/today/uuid/datetime` (grep: NONE). So no clock enters any generated artifact. | `pipeline.rs:1037-1074`; templates grep = NONE |
| 7 | **`inject_generated_impl_static` (LLM skill stub)** | **YES — no-op for us.** Only fires when a row has non-empty `skill_name` **and** `system_prompt` (`pipeline.rs:617-658`); our rows have neither field. No LLM call, no stub text, no clock. | `pipeline.rs:612-658` |
| 8 | **`determinism_salt`** | **DEAD — controls nothing.** Field exists (`types.rs:252`) but is written `None` everywhere and **read nowhere** in production. Grep (non-test) returns only struct decl + `None` initialisers. IRI minting (#1) ignores it. | grep below; `types.rs:250-252` |
| 9 | **JSON output canonicality** (`pipeline.json`, `objectives.json`) | **Mostly N/A on this path; safe if applied.** Our JSON is produced by Tera, not by `canonicalize_json`. If the engine ever canonicalises, `canonicalize_json` parse→`to_string_pretty` is deterministic (`canonicalize.rs:424-437`) — but note it would **reorder object keys** vs the template's literal order. Today neither matters: Tera emits literal JSON in array/field order fixed by the template + #3. | `canonicalize.rs:424-437` (latent) |
| 10 | **Content hash for receipt** `Sha256(final_content)` | **YES — deterministic given #1-9.** Computed over the exact bytes written (`pipeline.rs:1076`). Same inputs ⇒ same render ⇒ same hash. This is the *mechanism* that would make `output_hashes` in a receipt stable run-to-run. | `pipeline.rs:1076` |
| 11 | **Receipt / audit `timestamp` + `operation_id`** | **NONDETERMINISTIC — but BY DESIGN, and OUTSIDE the artifact set.** `sync` emits a receipt with a wall-clock timestamp and a fresh UUID (Dossier 01 §0 `emit_sync_receipt`; CLAUDE.md receipt schema). These live in `.ggen/receipts/*.json`, are **proof objects, not generated artifacts**, and their `input_hashes`/`output_hashes` ARE stable. So "reproducibility 100 %" applies to the *generated files + their hashes*, not the receipt envelope's clock. | CLAUDE.md "Cryptographic Receipts"; Dossier 01 §0 |
| 12 | **Git template fetch** (`TemplateSource::Git` → `uuid::Uuid::new_v4()` temp dir) | **N/A — not used.** Only fires for `template = { git = … }` (`pipeline.rs:881-883`). All our templates are `{ file = … }`. No network, no UUID temp dir. | `pipeline.rs:881-883` (unused) |

### 1a. The `ORDER BY` clauses are each a *total* order (no hidden tie)

```
extract-operators.rq        ORDER BY ?stage_order DESC(?fitness_score) ?operator_id   ← terminal key unique
extract-pareto-front.rq     ORDER BY DESC(?quality_tier) ?speed_tier ?operator_id     ← terminal key unique
extract-hyperparameters.rq  ORDER BY ?operator_id ?param_order                        ← (op,param) unique
extract-pipeline-stages.rq  ORDER BY ?stage_order      ← 1 row/stage ⇒ stage_order unique
extract-pareto-pipeline.rq  ORDER BY ?stage_order      ← elite filter ⇒ exactly 1 row/stage ⇒ unique
extract-fitness-objectives  ORDER BY ?objective_order  ← 2 distinct objectives ⇒ unique
```

The first three end on `?operator_id` / `?param_order` (globally unique within the
result), so ties are impossible. The last three sort on a key that the query body
*guarantees* is unique per row (one elite per stage; one row per objective). So **no
SELECT relies on a tie-break that oxigraph would resolve arbitrarily** — every result
ordering is fully pinned. This is the property that makes #3 safe and is the single
most important determinism fact for the real engine. (Verified: all 6 present, via
the Bash grep in the investigation; clauses quoted verbatim above.)

> **Grep evidence for #8 (`determinism_salt` is dead):** non-test occurrences are
> `crates/ggen-core/src/manifest/types.rs:252` (decl), `:480` (`None` default),
> `lean_six_sigma.rs:610`, `codegen/watch.rs:345`, `codegen/watch_cache_integration.rs:183`
> — all `None` initialisers. No read site. It is wired into serde but not into logic.

---

## 2. Is the generated output hash-stable across runs?

### 2a. What the pure-Python reference PROVES (observed, not asserted)

`verify/test_tpot2_autoconfig.py::test_elite_pipeline_is_deterministic` (lines 121-150)
runs `reference_autoconfig.run()` into **two independent temp dirs** and asserts the
emitted `reference_pipeline.json` **and** `reference_ggen.toml` are **byte-identical**
(`assert pa == pb`, `assert ta == tb`). It then asserts the 9 stage orders are exactly
`1..9` and the stage_ids match the frozen taxonomy in order. This is externalizable,
on-disk evidence (re-read with `read_text`) — Chicago-TDD, no mocks.

**Why the reference is byte-stable** (so we know which properties matter):
- **No clock anywhere** — grep of `reference_autoconfig.py` finds no `datetime/now()/time()/uuid`; the only `time` usage is the SLO timer in `test_vision2030.py:71-79`.
- **Everything is explicitly sorted** — algorithms by `algorithm_id` (`reference_autoconfig.py:217`), elite by the frozen `_elite_sort_key` (`:280`), decisions by `stage_order` (`:307`), Pareto front by `(−quality, speed, id)` (`:334`).
- **JSON written with fixed `indent=2`** (`:374`).

The reference is a **re-implementation** of the selection logic. Its byte-identity
proves the *algorithm* is deterministic — it does **not** exercise oxigraph, Tera, or
the ggen write path.

### 2b. What only a REAL `ggen sync` would confirm

The reference's `sort()` calls are Python-side. In the real engine, the equivalent
ordering is produced by **oxigraph's `ORDER BY`** (§3 row #3). So the real run is
hash-stable **iff** every SELECT's `ORDER BY` is a total order — which §1a confirms it
is. The remaining real-engine variables, all checked above and all controlled:
1. No timestamp/header injection on the generation write path (row #6). ✔
2. `BTreeMap` row keys + JSON-array row order (row #5). ✔
3. Exactly-representable fitness floats (row #2). ✔
4. Deterministic IRI minting (row #1). ✔

**Therefore:** I assess the real generated files (the 6 outputs in `generated/`) would
be **byte-identical across runs on the same ggen+oxigraph build**, and their
`Sha256` content hashes (row #10) stable — i.e. the "Reproducibility 100 %" SLO holds
for the artifacts. **Caveat (cannot execute):** I did not run oxigraph, so I cannot
empirically rule out a datatype-rendering subtlety (e.g. oxigraph emitting `80` vs
`80.0` for a decimal `BIND`) — Dossier 01 §2 already flagged that `clean_sparql_term`
normalises term strings and judged it consistent. The *order* is the property I am most
confident is pinned; the *term rendering* is the one a real run should sanity-check
once (see Hardening H4).

**One cross-build caveat:** if oxigraph were upgraded to a version with different
internal iteration, only queries **without** `ORDER BY` could shift — and we have none
on the generation path. So even an oxigraph bump leaves the generated artifacts stable.

---

## 3. Do the CONSTRUCTs need `ORDER BY`? (they don't — but strict-mode wants it)

The two inference rules are **CONSTRUCT**, and they deliberately have **no `ORDER BY`**
(confirmed: `derive-operators.rq`, `derive-pareto-dominance.rq`). This is **correct for
determinism**:

- A CONSTRUCT's result is a **set of triples** materialised into the graph
  (`ConstructExecutor::execute_and_materialize`, Dossier 01 §3 → `pipeline.rs:490-492`).
  A graph is unordered by definition — the *order* the engine emits CONSTRUCT triples in
  is irrelevant because they are inserted into a store and later re-queried by the
  SELECTs (which impose their own `ORDER BY`). So CONSTRUCT row order **cannot** leak
  into output. The materialized triple *set* is identical regardless of emission order.
- The IRI minting (#1) guarantees the *identity* of the emitted nodes/edges is a pure
  function of input, so the resulting graph is set-identical run-to-run.

**However**, ggen's **strict-mode** (`E0011`/`E0013`) raises `CONSTRUCT lacks ORDER BY`
as an **ERROR** (CLAUDE.md "Diagnostic Codes"; Dossier 01 §5 ⚠️ → `manifest/validation.rs:94-105`).
Our driver has no `[validation]` block ⇒ `strict_mode = false` ⇒ this is a **warning**,
non-blocking today. But a future maintainer flipping `strict_mode = true` would get a
**hard `error[E0011]`** on both CONSTRUCTs and `ggen sync` would refuse to run. This is a
latent footgun, not a current defect (CONTRACT.md §6 only mandates `ORDER BY` on
SELECTs). See Hardening H2.

---

## 4. SLO budget analysis

SLO targets (`.claude/rules/rust/performance.md:13-18`): **RDF processing ≤ 5 s / 1 k+
triples · Generation memory ≤ 100 MB · Reproducibility 100 %.**

### 4a. Triple-count estimate

Measured from the project TTLs (Bash counts in the investigation):

| Source | Individuals / size | Est. triples |
|--------|-------------------|-------------:|
| `algorithms.ttl` (60 `pi:ProcessIntelligenceAlgorithm`) | 60 indiv · ~12-13 preds each (939 prefixed-terms / 763 stmt-terminators) | **~800** |
| `breeds.ttl` (55 `compat:CognitionBreed`) | 55 indiv (283 stmt-terminators) | **~280** |
| `tpot-search-space.ttl` (9 stages + 2 objectives + 10 hyperparams + 1 GeneticConfig + class decls) | 194 stmt-terminators | **~190** |
| `tpot-shapes.ttl` (SHACL — loaded as `imports` data, not as a `[validation] shacl` gate; Dossier 01 §5) | 78 stmt-terminators | **~80** |
| **Input subtotal** | | **~1 350** |
| **Derived: operators** (`derive-operators.rq`) | 60 operators × 8 predicates | **+480** |
| **Derived: dominance** (`derive-pareto-dominance.rq`) | `tpot:dominates` edges (1 per dominated pair; ≪ 3 600 — see §4c) | **+~150-400** |
| **Grand total in the working graph** | | **≈ 1 900-2 200 triples** |

So the engine processes **~2 k triples** end-to-end. The SLO is *≤ 5 s per 1 k+
triples*; even read as "5 s per additional 1 k", ~2 k ⇒ a budget of roughly **≤ 10 s**,
and realistically the whole load+infer+select+render is **sub-second** at this scale
(in-memory oxigraph; the Python reference does the equivalent selection in **well under
the 5 s test budget** — `test_slo_generation_latency`, `test_vision2030.py:71-79`,
which the INTEGRATION_REPORT records as passing 4/4). **≤ 5 s is comfortably realistic.**

### 4b. Memory

~2 k triples in an in-memory oxigraph store + a handful of `BTreeMap` rows + Tera
render buffers is **kilobytes-to-low-megabytes**, nowhere near the **100 MB** ceiling.
No memory risk. (Not runtime-profiled here; asserted from data scale.)

### 4c. The O(60²) dominance CONSTRUCT — any perf concern?

`derive-pareto-dominance.rq` joins the 60-algorithm registry against itself
(`?algoA … ?algoB …`, `FILTER(?idA != ?idB)`), i.e. **60 × 60 = 3 600 candidate
ordered pairs**, each tested by 3 integer `FILTER`s (`derive-pareto-dominance.rq:53-58`).

**Verdict: negligible.** 3 600 candidate pairs with constant-time integer comparisons is
microseconds-to-low-milliseconds for oxigraph's hash-join — it is *quadratic in a
constant (60)*, not in a growing input. For context, this is ~3 orders of magnitude
smaller than the "1 k+ triples" the SLO is calibrated against. The output edge count is
**far below** 3 600 (most pairs are incomparable on the 2-objective front; only
strictly-dominating pairs emit an edge), so the materialised graph stays small (#4a).

**Scaling note (honest):** the cost is O(N²) in the *number of algorithms* N. At N=60
it is free. If the `pi:` registry grew to, say, N=5 000, the self-join becomes 25 M
candidate pairs and *could* approach the SLO — but that is a 80×-growth hypothetical,
not today's reality. Flagged for awareness only (Hardening H5).

---

## 5. Concrete determinism / SLO hardening recommendations

Ordered by value. H1-H2 are the ones that "deepen authority / reduce drift"
(`.claude/rules/coding-agent-mistakes.md` §6); H3-H6 are belt-and-suspenders.

**H1 — Make the determinism contract executable: add a `verify/slo_check.py`.**
Today the SLO is asserted inside `test_vision2030.py` but there is no standalone,
budget-printing gate. Add a tiny `verify/slo_check.py` that (a) times `reference.run()`
over K=5 iterations, (b) asserts `max_elapsed ≤ 5.0 s` (the RDF SLO), (c) re-runs into
two dirs and asserts byte-identity (re-using the determinism check), and (d) **prints
the triple-count estimate + measured latency** so the budget is observable, not
implicit. This converts the "reproducibility 100 %" + "≤5 s" SLOs from prose into a
single command (mirrors the CLAUDE.md `just slo-check` discipline). *Reduces drift:* the
budget becomes a checked artifact. (Scope it as a *reference-timing* gate, and label it
honestly as a proxy for the real `ggen sync` SLO, since ggen can't run here.)

**H2 — Future-proof strict-mode: add `ORDER BY` to BOTH CONSTRUCTs.**
`derive-operators.rq` and `derive-pareto-dominance.rq` lack `ORDER BY`. This is
*harmless* today (strict_mode off; CONSTRUCT order can't leak — §3) but becomes a
**hard `error[E0011]`** the moment anyone sets `[validation] strict_mode = true`
(`manifest/validation.rs:94-105`). Add `ORDER BY ?operator` to derive-operators and
`ORDER BY ?opA ?opB` to derive-pareto-dominance. Cost: two lines; benefit: the project
survives strict-mode activation. *Deepens authority:* the authoritative path stays
runnable under the stricter gate. (Note this is the exact item Dossier 01 §8 Q3 also
flagged — they agree.)

**H3 — Set `determinism_salt` in the driver `[generation]` block AND treat it as
documentation-until-wired.** Since the field is currently **dead** (§1 #8), setting it
changes nothing in behaviour — but writing `determinism_salt = "tpot2-wasm4pm-v1"`
documents *intent* (this project claims determinism) and future-proofs against a ggen
version that starts honoring it for IRI minting. **Do not claim it provides
determinism today** — our determinism comes from pure-function IRI minting (#1), not
from the salt. If you set it, add a one-line comment that it is presently inert in
ggen-core. *(This is the weakest recommendation; include only as a forward-compat
marker, and only if you also note its current no-op status — otherwise it is
decorative.)*

**H4 — One real-engine smoke check when a toolchain is available: assert oxigraph
term rendering.** The one property the Python reference can't validate is how oxigraph
renders the decimal `BIND` (`80` vs `80.0`) and the `xsd:boolean is_elite` (`true`).
Dossier 01 §2 judged `clean_sparql_term` consistent, but a single real `ggen sync` +
`diff` of two runs (and a check that `fitness_score`/`is_elite` render as the templates
expect — `tpot-config-dict.py.tera` tests `is_elite == "true"`) would *confirm* it.
Cheapest possible: run sync twice, `diff -r generated_run1 generated_run2`, expect empty.

**H5 — (Latent, low priority) Guard the O(N²) dominance join if the registry grows.**
At N=60 the self-join is free (§4c). If `algorithms.ttl` ever scales past a few hundred
individuals, consider pre-filtering candidates (e.g. only compare within plausible tier
bands) or computing dominance in the reference/host rather than SPARQL. No action needed
now; documented so a future registry-growth PR doesn't silently blow the SLO.

**H6 — If `canonicalize` is ever enabled for generated output, force `inject_header =
false` for the TOML/JSON/MD artifacts.** Not active today (§1 #6), but `default_generated_header()`
injects `chrono::Utc::now()` (`canonicalize.rs:358-368`) which would **break byte
stability**. Should a future ggen version canonicalise generation output, the
`with_inject_header(false)` path must be used (or a fixed/epoch header), or the
reproducibility SLO regresses. Documented as a tripwire.

---

## 6. Quick answers to the four sub-questions

1. **Nondeterminism sources:** 12 enumerated (§1). IRI minting (#1), fitness floats
   (#2), SELECT order (#3), oxigraph iteration (#4), row/template iteration (#5),
   header/timestamp (#6), skill-stub (#7) — **all controlled**. `determinism_salt` (#8)
   is **dead**. Receipt timestamp/UUID (#11) is nondeterministic **by design and outside
   the artifact set**. The single load-bearing control is **`ORDER BY` on all 6 SELECTs,
   each a total order** (§1a).
2. **Hash-stable output?** **YES** — the Python reference *proves* byte-identity of the
   selection+emission (`test_tpot2_autoconfig.py:121-150`); in real ggen the same holds
   because nothing injects a clock on the write path (`pipeline.rs:1037-1074`), rows are
   `BTreeMap`+JSON-array ordered by the SELECT's `ORDER BY`, and floats are exact. The
   only thing a real run (not the reference) would *confirm* is oxigraph's term
   rendering + that `ORDER BY` pins order — both assessed safe, neither executed here.
3. **SLO budget:** ~1 350 input + ~480 operators + ~150-400 dominance ≈ **~2 k triples**
   in the working graph. **≤ 5 s is comfortably realistic** (reference runs sub-budget;
   in-memory oxigraph). The **O(60²)=3 600-pair** dominance CONSTRUCT is **negligible**
   (constant-time integer filters; output edges ≪ 3 600). Memory ≪ 100 MB.
4. **Hardening:** H1 add `verify/slo_check.py` (executable budget+reproducibility gate);
   H2 add `ORDER BY` to both CONSTRUCTs (strict-mode survival); H3 set `determinism_salt`
   as a *forward-compat marker only* (currently inert); H4 one real-engine `diff` smoke
   check; H5/H6 latent tripwires (N²-growth, canonicalize-header-clock).

---

### Bottom line

The generator is **deterministic by construction**: operator IRIs are pure functions of
`algorithm_id`, fitness is exactly-representable float, the argmax is a strict total
order, and every generation SELECT carries a total-order `ORDER BY` so oxigraph's row
order is fully pinned. No clock, UUID, or hashmap iteration touches a generated
artifact. The pure-Python reference **proves byte-identical re-runs of the selection
logic**; the only properties left for a real `ggen sync` to *confirm* are oxigraph's
`ORDER BY` honoring (mechanically certain) and decimal/boolean term rendering (assessed
consistent). At ~2 k triples the RDF-processing SLO (≤ 5 s) holds with large margin and
the O(60²) dominance self-join is free. The two cheap hardening wins are an executable
`slo_check.py` and `ORDER BY` on the two CONSTRUCTs for strict-mode survival;
`determinism_salt` is a **dead field** and provides nothing today.
