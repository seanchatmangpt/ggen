# Research Dossier 03 — Event-Log Inputs & the Concrete Input Contract

**Agent:** 03 of 10 · **Date:** 2026-06-20 · **Scope:** Does a real event log (or fixtures)
exist that the auto-configured pipeline could consume, and what is the concrete per-stage
input contract?

**Method:** Read-only sweep across the repo (`Glob`/`Grep`/`Read`). No `cargo`/`ggen`/builds run
(per CONTRACT §9: this container has no `cargo`, no `ggen` binary, no `rdflib`). Every claim below
cites a real file path and line. Where no real log exists for a given format, that is stated plainly.

---

## TL;DR (the answer)

1. **A real, OCEL-2.0-conformant event log exists** and is directly usable for the object-centric
   stages: `crates/cpmp/tests/fixtures/p2p.ocel.json` (purchase-to-pay, 2 object types, 4
   activities, 6 events). It matches the canonical Rust OCEL 2.0 reader in
   `crates/cpmp/src/ocel.rs` field-for-field.
2. **A real XES 1.0 log exists** at repo root: `test_log.xes` (2 traces, activities A→B). This is
   the only on-disk XES the *39 xes-format* algorithms (incl. 5 of the 9 elite operators) could read.
3. **No PNML file and no `petri_net_handle` fixture exist anywhere in the repo** — yet the
   auto-selected **stage 1 elite (`pnml_import`) consumes `pnml`** and the **stage 8 elite
   (`playout`) consumes `petri_net_handle`**. Those are produced *in-pipeline*, not loaded from disk
   (see §4 — this is the most important contract subtlety).
4. **The wasm4pm `ocel/` directory contains NO event log** — only two evidence TTLs
   (`evidence.ttl`, `pi_evidence.ttl`). The wasm4pm `ocel-load.rq` proof query *references* a log
   (`http://wasm4pm.org/log/ocel-2026-04.jsonocel`) but that is a **placeholder URI inside a
   `BIND(...)`**, not a file (CONTRACT §1 lists 6 proofs; `ocel-load.rq` is a CONSTRUCT mapping, not a loader).
5. **The CLAUDE.md intel log `.ggen/ocel/agent-edit-events.ocel.jsonl` does NOT currently exist on
   disk** (`ls .ggen/ocel/` → absent), but its writer (`IntelLog`) and NDJSON record shape are real
   (`crates/ggen-lsp/src/intel/log.rs`), and real samples of that exact shape are committed as
   oracle tapes (`crates/ggen-lsp/tests/fixtures/oracle-tapes/*.ocel.jsonl`).

**Recommendation (§5):** use the real `crates/cpmp/tests/fixtures/p2p.ocel.json` for the OCEL stage
and the real root `test_log.xes` for the XES stages; supply one **clearly-labeled minimal synthetic**
file only if a single unified driver input is wanted. Details and per-stage outputs in §5.

---

## 1. Real event logs / OCEL fixtures found (paths, format, size, samples)

`Glob`/`stat` sweep results (all paths absolute):

| Path | Format | Size | Records | Real or placeholder | Relevance |
|------|--------|-----:|---------|--------------------|-----------|
| `/home/user/ggen/crates/cpmp/tests/fixtures/p2p.ocel.json` | **OCEL 2.0 JSON** | 2,761 B | 2 obj-types, 4 evt-types, 3 objects, 6 events | **REAL, well-formed** | **OCEL stage input** |
| `/home/user/ggen/test_log.xes` | **XES 1.0 XML** | 722 B | 2 traces, 4 events (A,B,A,B) | **REAL, well-formed** | **XES stages input** |
| `/home/user/ggen/workflow-test.xes` | XES 1.0 XML | 123 B | **EMPTY** `<log>` (0 traces) | REAL but empty | not usable (no events) |
| `/home/user/ggen/ocel/anti_llm_lsp_ocel.json` | OCEL 2.0 JSON | 7,707 B | eventTypes + events (LSP audit) | REAL | domain = LSP audit, **not** P2P/process-mining demo |
| `/home/user/ggen/crates/ggen-graph/audit/gall_evidence.ocel.json` | OCEL-ish JSON (`objects`+attrs) | 1,921 B | audit objects | REAL | CI self-audit, not a process log |
| `/home/user/ggen/crates/ggen-graph/audit/vision2030.self_audit.ocel.json` | OCEL-ish JSON | 15,032 B | audit objects/events | REAL | CI self-audit, not a process log |
| `/home/user/ggen/crates/ggen-lsp/tests/fixtures/oracle-tapes/ggen-6link-good.ocel.jsonl` | **OCEL NDJSON** (`.ocel.jsonl`) | 3,308 B | 6 events | REAL | exemplar of the **IntelLog** record shape (§2.2) |
| `/home/user/ggen/crates/ggen-lsp/tests/fixtures/oracle-tapes/ggen-6link-prefix-DEAD.ocel.jsonl` | OCEL NDJSON | 2,269 B | 4 events | REAL | negative-control tape |
| `/home/user/ggen/.ggen/ocel/agent-edit-events.ocel.jsonl` | (CLAUDE.md intel log) | — | — | **ABSENT** (`.ggen/ocel/` dir does not exist) | would be live IntelLog output |
| `/home/user/ggen/ontology_catalogue/wasm4pm/ocel/reports/evidence.ttl` | RDF/Turtle | 9,163 B | 55 breed-fitness facts | REAL | **evidence**, not an event log |
| `/home/user/ggen/ontology_catalogue/wasm4pm/ocel/reports/pi_evidence.ttl` | RDF/Turtle | 9,732 B | algo-evidence facts | REAL | **evidence**, not an event log |
| `http://wasm4pm.org/log/ocel-2026-04.jsonocel` | (referenced) | — | — | **PLACEHOLDER URI** in a `BIND()` in `ocel-load.rq:90` | not a file |

**Crucial negative finding:** `ls -la ontology_catalogue/wasm4pm/ocel/` shows **only** the `reports/`
subdir with the two TTLs above. The wasm4pm tree ships **no `.xes` / `.ocel` / `.jsonocel` event log
of its own.** The auto-config generator's own project
(`examples/tpot2-wasm4pm-autoconfig/`) likewise contains **no event-log fixture** — a `find` for
`*.json|*.jsonl|*.xes|*.ocel*` there returns only generation *output* artifacts
(`verify/out/reference_pipeline.json`, `verify/out/validation_report.json`, etc.), never an input log.

### 1.1 Sample records — `p2p.ocel.json` (the OCEL recommendation)

Real excerpt (`crates/cpmp/tests/fixtures/p2p.ocel.json:24-101`):

```json
"objects": [
  { "id": "order1", "object_type": "order",
    "attributes": [ {"key":"price","value":99.95}, {"key":"customer","value":"alice"} ],
    "relationships": [] },
  { "id": "order2", "object_type": "order",
    "attributes": [ {"key":"price","value":24.50}, {"key":"customer","value":"bob"} ],
    "relationships": [ {"source_object_id":"order2","target_object_id":"order1","qualifier":"referenced-by"} ] },
  { "id": "item1", "object_type": "item",
    "attributes": [ {"key":"quantity","value":3}, {"key":"sku","value":"SKU-42"} ],
    "relationships": [] }
],
"events": [
  { "id":"e1", "activity":"Place Order",     "timestamp":"2026-01-01T08:00:00Z",
    "relationships":[ {"object_id":"order1","qualifier":"initiates"} ], "attributes":[] },
  { "id":"e4", "activity":"Pick Item",       "timestamp":"2026-01-02T11:00:00Z",
    "relationships":[ {"object_id":"order1","qualifier":"fulfills"},
                      {"object_id":"item1","qualifier":"allocated"} ], "attributes":[] },
  { "id":"e5", "activity":"Ship Order",      "timestamp":"2026-01-03T08:00:00Z",
    "relationships":[ {"object_id":"order1","qualifier":"dispatches"},
                      {"object_id":"item1","qualifier":"shipped"} ], "attributes":[] }
]
```

This log yields a non-trivial directly-follows graph for `order1`
(`Place Order → Receive Payment → Pick Item → Ship Order`) and exercises a 2-object event (`e4`,
`e5` link both `order` and `item`) — exactly what an object-centric discoverer needs.

### 1.2 Sample record — `test_log.xes` (the XES recommendation)

Full file (`/home/user/ggen/test_log.xes`, 13 lines):

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<log xes.version="1.0" xes.features="" xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="case_1"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2026-05-01T10:00:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2026-05-01T10:01:00Z"/></event>
  </trace>
  <trace>
    <string key="concept:name" value="case_2"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2026-05-01T11:00:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2026-05-01T11:01:00Z"/></event>
  </trace>
</log>
```

Two cases, identical trace `A → B`. Valid XES (`concept:name`, `time:timestamp`), single variant —
fine as a smoke input; trivially small. `workflow-test.xes` is an **empty** `<log>` and must not be used.

### 1.3 Sample record — IntelLog NDJSON shape (oracle tape)

Real first record of `crates/ggen-lsp/tests/fixtures/oracle-tapes/ggen-6link-good.ocel.jsonl`:

```json
{"id":"be9cf9931f7baa7f","activity":"DiagnosticRaised","timestamp":"2026-05-30T10:00:32.692528Z",
 "objects":[{"id":".../templates/item.tera","type":"file","qualifier":"file"},
            {"id":"GGEN-TPL-001","type":"diagnostic_code","qualifier":"diag"},
            {"id":"53e33713b5502973","type":"episode","qualifier":"episode"},
            {"id":"editor","type":"agent","qualifier":"agent"}],
 "attributes":{"session_id":"...","span":"0:0-0:4294967295","severity":"error","transport":"lsp","agent_id":"editor"}}
```

This is the *streaming* (`.ocel.jsonl`) flavor — **one OCEL event per line** — matching the
`ggen-graph::ocel::OcelEvent` struct (§2.2). It is the format the live intel log would use, but its
domain is "agent edits / LSP repair", not process-mining demo traffic.

---

## 2. The OCEL 2.0 object-centric event-log schema as used in this repo

Two real, *distinct* OCEL serializations coexist. Both are first-class and used by different crates.

### 2.1 OCEL 2.0 **standard JSON** (cpmp) — the canonical, spec-compliant form

Defined by the reader at `crates/cpmp/src/ocel.rs` (module doc line 1-4 cites
`https://www.ocel-standard.org/2.0/`). Top-level container `OcelEventLog`
(`crates/cpmp/src/ocel.rs:123-131`) has **four** sections:

| Section | Rust type | Shape |
|---------|-----------|-------|
| `objectTypes` | `Vec<OcelObjectTypeSpec>` (`ocel.rs:105-110`) | `{ "name", "attributes":[{"name","type"}] }` |
| `eventTypes` | `Vec<OcelEventTypeSpec>` (`ocel.rs:113-118`) | `{ "name", "attributes":[...] }` |
| `objects` | `Vec<OcelObject>` (`ocel.rs:86-94`) | `{ "id", "object_type", "attributes":[{"key","value"}], "relationships":[{"source_object_id","target_object_id","qualifier"}] }` (O2O) |
| `events` | `Vec<OcelEvent>` (`ocel.rs:47-56`) | `{ "id", "activity", "timestamp"(RFC-3339 `DateTime<Utc>`), "relationships":[{"object_id","qualifier"}](E2O), "attributes":[{"key","value"}] }` |

- **Object types** (real, from `p2p.ocel.json:2-17`): `order` (price:float, customer:string),
  `item` (quantity:integer, sku:string).
- **Activities / event types** (`p2p.ocel.json:18-23`): `Place Order`, `Receive Payment`,
  `Pick Item`, `Ship Order`.
- **Timestamps**: ISO-8601 / RFC-3339 UTC (e.g. `"2026-01-01T08:00:00Z"`), parsed into
  `chrono::DateTime<Utc>` (`ocel.rs:51`). Required field; an invalid value raises
  `OcelError::InvalidTimestamp` (`ocel.rs:22`).
- **Qualified relationships**: event→object (E2O) qualifiers in `p2p.ocel.json` are
  `initiates / settles / fulfills / allocated / dispatches / shipped`; one object→object (O2O)
  edge `order2 --referenced-by--> order1`.
- **Process-mining primitives are already implemented** on this type (proof the schema is "live"):
  `directly_follows_graph()` (`ocel.rs:209-217`), `variants_per_object_type()`
  (`ocel.rs:222-239`), `stats() → EventLogStats` (`ocel.rs:242-254`, fields:
  `event_count, object_count, activity_type_count, object_type_count, dfg_edge_count, variant_count`).

### 2.2 OCEL **NDJSON / in-memory** form (ggen-graph + ggen-lsp IntelLog) — the streaming form

Defined at `crates/ggen-graph/src/ocel/ocel_types.rs`. Container `OcelLog`
(`ocel_types.rs:6-11`) has only **`objects` + `events`** (no type/attr *schema* headers). Per-event
shape (`ocel_types.rs:33-44`): `{ id, activity, timestamp:DateTime<Utc>, objects:Vec<OcelObjectRef>,
attributes:HashMap<String,String> }`, where `OcelObjectRef = { id, type, qualifier:Option<String> }`
(`ocel_types.rs:48-55`). Object: `{ id, type, attributes:HashMap<String,String> }`
(`ocel_types.rs:22-29`).

This is the type the CLAUDE.md intel log uses. The writer `IntelLog` appends one event per line to
`.ggen/ocel/agent-edit-events.ocel.jsonl` (`crates/ggen-lsp/src/intel/log.rs:1, 23-26`:
`default_path = root/.ggen/ocel/agent-edit-events.ocel.jsonl`). A second ledger lives at
`.ggen/ocel/promotion-history.jsonl` (`crates/ggen-lsp/src/intel/history.rs:1, 55`).

**Schema-difference caveat (drift risk):** the two forms are NOT interchangeable. cpmp uses
`object_type` + list-of-`{key,value}` attributes + a four-section header; ggen-graph uses `type` +
`HashMap` attributes + no header and renames the E2O field set to an `objects[]`/`OcelObjectRef`
array. A consumer written against one will reject the other. The **wasm4pm proof queries assume yet a
third shape** — see §2.3.

### 2.3 What wasm4pm `ocel-load.rq` actually expects (proof-query view)

`ontology_catalogue/wasm4pm/semconv/sparql-proofs/ocel-load.rq` is a **CONSTRUCT** (not a loader). Its
header (lines 1-4) states the **input is a JSON OCEL *load-result*** — `{ object_types[],
event_types[], counts, timestamp_range }` — i.e. *summary statistics already computed by the wasm4pm
engine*, not the raw log. The `WHERE` clause is entirely `BIND(...)` placeholders (lines 80-145),
including the non-existent source URI `http://wasm4pm.org/log/ocel-2026-04.jsonocel` (line 90) and a
fictional "Order-to-Cash" 45,000-event log (line 94-97). **This file proves the *output RDF shape*
(`pm:ObjectCentricEventLog`, `pm:ObjectType`, `pm:Activity`, `pm:eventCount`, …), not a concrete input
file.** The generated pipeline references it as a `tpot:resultMapping` for the ingest stage
(INTEGRATION_REPORT §6), which is correct: it maps a load *result* to RDF; it does not read a log.

---

## 3. Concrete input-format contract per stage (cross-referenced to `pi:inputFormat`)

Source: `ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl`. Distribution of `pi:inputFormat`
across all **60** algorithms (verified by grep count): **39 `xes`, 9 `any`, 6 `ocel`, 4
`petri_net_handle`, 1 `pnml`, 1 `bpmn`** (= 60). The **elite** operator chosen per stage
(INTEGRATION_REPORT §2) determines the *actual* runtime input the auto-config pipeline demands:

| # | stage / category | **elite operator** | `pi:inputFormat` | line | wasmExport | outputType | proof query |
|---|------------------|--------------------|------------------|------|-----------|-----------|-------------|
| 1 | ingest / `import_export` | `pnml_import` | **`pnml`** | `algorithms.ttl:815` | `from_pnml_wasm` | `petrinet` | ocel-load.rq |
| 2 | discover / `discovery` | `inductive_miner` | **`xes`** | `algorithms.ttl:164` | `discover_inductive_miner` | `tree` | discover-dfg.rq |
| 3 | discover_oc / `object_centric` | `ocel_petri_net` | **`ocel`** | `algorithms.ttl:508` | `discover_ocel_petri_net` | `petrinet` | — |
| 4 | analyze / `discovery_analytics` | `compute_activity_transition_matrix` | **`xes`** | `algorithms.ttl:359` | `compute_activity_transition_matrix` | `analytics` | — |
| 5 | conform / `conformance` | `complexity_metrics` | **`xes`** | `algorithms.ttl:654` | `compute_complexity_metrics` | `analytics` | conformance-check.rq |
| 6 | learn / `ml_analytics` | `ml_classify` | **`any`** | `algorithms.ttl:566` | `ml_classify` | `ml_result` | ml-classify.rq |
| 7 | predict / `prediction` | `compute_ewma` | **`xes`** | `algorithms.ttl:731` | `compute_ewma` | `analytics` | predict-activity.rq |
| 8 | simulate / `simulation` | `playout` | **`petri_net_handle`** | `algorithms.ttl:714` | `petri_net_playout` | `analytics` | — |
| 9 | orchestrate / `agentic` | `agentic_pipeline` | **`xes`** | `algorithms.ttl:860` | `run_agentic_pipeline` | `model` | — |
| + | drift-monitor | `detect_drift` | **`xes`** | `algorithms.ttl:744` | `detect_drift` | `analytics` | detect-drift.rq |

### 3.1 Reading the contract — three input classes

The pipeline is **heterogeneous**; it is *not* "feed one XES file to everything". Inputs fall into
three classes:

- **File-backed log inputs (must exist on disk):**
  - `xes` — stages 2, 4, 5, 7, 9, + drift (6 of 10 elites). Satisfiable by `test_log.xes`.
  - `ocel` — stage 3 only. Satisfiable by `crates/cpmp/tests/fixtures/p2p.ocel.json`.
  - `any` — stage 6 (`ml_classify`); accepts either an event log or an upstream feature matrix.
    Satisfiable by *either* the XES or the OCEL file (or stage-4/6 analytics output).

- **Model-file input (PNML on disk):**
  - `pnml` — stage 1 (`pnml_import`). This is **not an event log** — it is a Petri-net model file
    (PNML XML). **No `.pnml` file exists in the repo.** Semantically, ingest here imports a *model*,
    not a log. (Note the tension: the contract labels stage 1 "ingest" and wires it to
    `ocel-load.rq`, yet the elite `pnml_import` ingests a Petri net, and its `outputType` is
    `petrinet`. The auto-selector picked `pnml_import` purely on fitness q90/s8→f86.0, the highest in
    `import_export`; format-fit was not a selection criterion. Flagging as a real semantic mismatch,
    not inventing a fix.)

- **In-pipeline handle input (NOT a file):**
  - `petri_net_handle` — stage 8 (`playout`). This consumes a Petri-net *object produced upstream*
    (stage 1 `pnml_import` → `petrinet`, or stage 3 `ocel_petri_net` → `petrinet`), passed by
    reference. There is no `petri_net_handle` file to supply and none is expected; **dataflow, not a
    file load.** 4 algorithms use this format (`algorithms.ttl:668, 682, 714, 843`), all simulation/
    replay operators that act on a discovered model.

### 3.2 Net file-input requirement for the auto-selected pipeline

To actually run the chosen pipeline end-to-end you need **at minimum**:
1. one **XES** log (drives stages 2,4,5,7,9,drift and is a valid feed for stage-6 `any`), **and**
2. one **OCEL 2.0** log (drives stage 3 `ocel_petri_net`), **and**
3. one **PNML** model file for stage 1 (`pnml_import`) — *currently missing from the repo*.

Stage 8 needs no file (consumes the stage-1/stage-3 Petri-net handle).

---

## 4. Why "no single input file" — the dataflow, made explicit

The 9-stage pipeline is a DAG over *formats*, not a linear XES sweep:

```
 [PNML file]──▶(1 pnml_import)──petrinet handle──┐
                                                 ├──▶(8 playout)──analytics
 [OCEL log]───▶(3 ocel_petri_net)──petrinet h.───┘
 [XES log]────▶(2 inductive_miner)──process tree
            └──▶(4 transition_matrix)──analytics
            └──▶(5 complexity_metrics)──analytics   (conformance)
            └──▶(7 compute_ewma)──analytics          (prediction)
            └──▶(+ detect_drift)──analytics          (drift monitor)
            └──▶(9 agentic_pipeline)──model
 [XES or OCEL or upstream matrix]─▶(6 ml_classify "any")──ml_result
```

Evidence the handle classes are upstream-produced, not loaded:
- `playout` doc (`algorithms.ttl:704-714`) replays a Petri net; `inputFormat petri_net_handle`.
- `pnml_import` `outputType "petrinet"` (`algorithms.ttl:809`) and `ocel_petri_net`
  `outputType "petrinet"` (`algorithms.ttl:503`) are the only two elites producing a Petri net —
  they are the suppliers of the stage-8 handle.

---

## 5. Recommendation — concrete input files and per-stage outputs

### 5.1 Primary recommendation (use the REAL fixtures already in-repo)

| Need | Use this REAL file | Why |
|------|--------------------|-----|
| OCEL stage (3) | `crates/cpmp/tests/fixtures/p2p.ocel.json` | spec-compliant OCEL 2.0, 2 object types, multi-object events, non-trivial DFG; parses via `OcelEventLog::from_json_file` (`cpmp/src/ocel.rs:140`) |
| XES stages (2,4,5,7,9,drift) + `any` (6) | `test_log.xes` (repo root) | only valid non-empty XES 1.0 on disk; 2 cases, A→B |
| PNML stage (1) | **none exists** → see 5.3 | repo ships no Petri-net model file |
| Petri-net handle (8) | **n/a** — produced by stage 1/3 | dataflow, not a file |

To exercise the pipeline's object-centric heart with the strongest real input, **point the OCEL stage
at `p2p.ocel.json`**; it is the single best real event log in the repo for process mining and aligns
exactly with the OCEL reader the codebase already ships.

### 5.2 Per-stage expected output (with the recommended inputs)

| # | stage | input file | operator | expected product |
|---|-------|-----------|----------|-------------------|
| 1 | ingest | *PNML (synthetic, 5.3)* | `pnml_import` | Petri-net handle (`petrinet`) |
| 2 | discover | `test_log.xes` | `inductive_miner` | process tree (single variant A→B → trivial sequence net) |
| 3 | discover_oc | `p2p.ocel.json` | `ocel_petri_net` | object-centric Petri net over {order,item}; DFG `Place Order→Receive Payment→Pick Item→Ship Order` |
| 4 | analyze | `test_log.xes` | `compute_activity_transition_matrix` | 2×2 transition matrix {A,B}, A→B weight 2 |
| 5 | conform | `test_log.xes` | `complexity_metrics` | complexity/analytics scalars (low complexity; 1 variant) |
| 6 | learn | `p2p.ocel.json` *or* `test_log.xes` (`any`) | `ml_classify` | `ml_result` (classification over encoded features) |
| 7 | predict | `test_log.xes` | `compute_ewma` | EWMA-smoothed KPI time-series (`analytics`) |
| 8 | simulate | *(stage-1/3 Petri handle)* | `playout` | simulated traces from the discovered net (`analytics`) |
| 9 | orchestrate | `test_log.xes` | `agentic_pipeline` | orchestrated model artifact (`model`) |
| + | drift | `test_log.xes` | `detect_drift` | drift verdict (no drift — single static variant) |

(Outputs are the *documented* products per `pi:outputType` + `pi:algorithmDoc`; no engine was run —
CONTRACT §9. They are predictions of shape, not captured runs.)

### 5.3 If a single unified driver input is wanted — MINIMAL SYNTHETIC (clearly labeled)

The repo lacks **(a)** a PNML model for stage 1 and **(b)** a richer XES than the 2-trace stub. If the
generated pipeline/README wants one self-contained fixture set, add these under the **generator
project** (not the read-only ontology), clearly marked synthetic:

1. `examples/tpot2-wasm4pm-autoconfig/fixtures/p2p.ocel.json` — **copy** of the real
   `crates/cpmp/tests/fixtures/p2p.ocel.json` (real provenance; OCEL stage 3 + `any` stage 6).
2. `examples/tpot2-wasm4pm-autoconfig/fixtures/p2p.xes` — **SYNTHETIC**, the order-trace flattening of
   the same P2P process so XES and OCEL stages tell one story:

   ```xml
   <?xml version="1.0" encoding="UTF-8" ?>
   <!-- SYNTHETIC fixture for tpot2-wasm4pm-autoconfig — flattened from p2p.ocel.json (order perspective). NOT a captured log. -->
   <log xes.version="1.0" xes.features="" xmlns="http://www.xes-standard.org/">
     <trace>
       <string key="concept:name" value="order1"/>
       <event><string key="concept:name" value="Place Order"/>    <date key="time:timestamp" value="2026-01-01T08:00:00Z"/></event>
       <event><string key="concept:name" value="Receive Payment"/><date key="time:timestamp" value="2026-01-02T10:00:00Z"/></event>
       <event><string key="concept:name" value="Pick Item"/>      <date key="time:timestamp" value="2026-01-02T11:00:00Z"/></event>
       <event><string key="concept:name" value="Ship Order"/>     <date key="time:timestamp" value="2026-01-03T08:00:00Z"/></event>
     </trace>
     <trace>
       <string key="concept:name" value="order2"/>
       <event><string key="concept:name" value="Place Order"/>    <date key="time:timestamp" value="2026-01-01T09:00:00Z"/></event>
       <event><string key="concept:name" value="Receive Payment"/><date key="time:timestamp" value="2026-01-03T14:00:00Z"/></event>
     </trace>
   </log>
   ```
   (Two variants → non-trivial discovery/conformance/drift, unlike the degenerate `test_log.xes`.)

3. `examples/tpot2-wasm4pm-autoconfig/fixtures/p2p.pnml` — **SYNTHETIC** minimal sound PNML so stage 1
   `pnml_import` has a model to load (places `p_start,p_paid,p_picked,p_end`; transitions
   `Place Order, Receive Payment, Pick Item, Ship Order`). Mark with an XML comment
   `<!-- SYNTHETIC ... NOT engine-generated -->`. (A few-element net; the exact PNML XML can be
   authored by whoever wires the demo — its structure mirrors the variant-1 trace above.)

**Rationale (per `coding-agent-mistakes.md`):** preferring the real `p2p.ocel.json` over a fabricated
log *reduces drift* (the input is genuine, not invented); any synthetic XES/PNML is explicitly
labeled so it cannot masquerade as captured evidence. Do **not** fabricate a `.ggen/receipts/*.json`
or claim a run occurred (CONTRACT §9, INTEGRATION_REPORT §6).

---

## 6. Direct answers to the four research sub-questions

1. **Do real event logs/fixtures exist?** Yes. The strongest is **`crates/cpmp/tests/fixtures/p2p.ocel.json`**
   (real OCEL 2.0). Plus a real (tiny) XES at **`test_log.xes`**, real OCEL NDJSON oracle tapes under
   `crates/ggen-lsp/tests/fixtures/oracle-tapes/`, and real audit OCEL JSON under
   `crates/ggen-graph/audit/` and `ocel/`. **The wasm4pm `ocel/` dir has NO log (only evidence TTL);
   the generator project ships NO input log; the CLAUDE.md intel log
   `.ggen/ocel/agent-edit-events.ocel.jsonl` is NOT present on disk.**

2. **OCEL 2.0 schema as used here:** two coexisting forms — the **spec JSON** (`objectTypes`,
   `eventTypes`, `objects`, `events`; `cpmp/src/ocel.rs:123-131`) with object types `order`/`item`,
   activities `Place Order / Receive Payment / Pick Item / Ship Order`, RFC-3339 timestamps, qualified
   E2O + O2O relationships (`p2p.ocel.json`); and the **NDJSON streaming** form (`objects`+`events`
   only; `ggen-graph/src/ocel/ocel_types.rs:6-55`) used by `IntelLog`. wasm4pm's `ocel-load.rq`
   assumes a *third*, summary-result shape and outputs `pm:ObjectCentricEventLog` RDF.

3. **Concrete input contract per stage:** §3 table. The auto-selected pipeline demands **pnml**
   (stage 1), **xes** (2,4,5,7,9,drift), **ocel** (3), **any** (6), **petri_net_handle** (8). Of these,
   `petri_net_handle` is an upstream handle (no file), and `pnml` is a model file (no log). Format
   counts across all 60: 39 xes / 9 any / 6 ocel / 4 petri_net_handle / 1 pnml / 1 bpmn.

4. **Recommended input file(s):** primary — real **`crates/cpmp/tests/fixtures/p2p.ocel.json`** for
   the OCEL stage and real **`test_log.xes`** for XES stages; if one self-contained set is wanted, add
   labeled-synthetic `p2p.xes` (2 variants) and `p2p.pnml` under the generator's `fixtures/` (§5.3),
   keeping the real OCEL as the centerpiece. Per-stage expected products in §5.2.

---

## 7. Drift / footgun flags for downstream agents

- **F1 — Format mismatch at ingest:** stage-1 elite `pnml_import` consumes a *Petri-net model*
  (`pnml`), not an event log, yet the stage is named "ingest" and wired to `ocel-load.rq`. The
  selector chose it purely on fitness (q90/s8). If the generated config/README implies "ingest =
  load the event log", that is misleading. Real, not invented — surfaced for Agent review.
- **F2 — Three incompatible OCEL schemas** (cpmp spec-JSON vs ggen-graph NDJSON vs wasm4pm
  load-result). A consumer must target the right one; they do not auto-convert.
- **F3 — No PNML, no petri_net_handle fixture** in the repo → a literal end-to-end run of the chosen
  pipeline cannot start at stage 1 without authoring a PNML (5.3).
- **F4 — `workflow-test.xes` is empty** (0 traces) — never use it.
- **F5 — `.ggen/ocel/agent-edit-events.ocel.jsonl` absent** — CLAUDE.md describes the IntelLog but no
  live log is committed; only oracle tapes demonstrate its shape.
