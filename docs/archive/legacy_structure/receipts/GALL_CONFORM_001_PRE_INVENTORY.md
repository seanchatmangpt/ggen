<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL_CONFORM_001 — PM-Retirement Pre-Inventory](#gall_conform_001--pm-retirement-pre-inventory)
  - [0. Headline finding — BLOCKED status has changed](#0-headline-finding--blocked-status-has-changed)
  - [1. RETIRE / KEEP INVENTORY (the deliverable)](#1-retire--keep-inventory-the-deliverable)
    - [1A. `crates/ggen-graph/src/ocel/` — the duplicated PM stack](#1a-cratesggen-graphsrcocel--the-duplicated-pm-stack)
    - [1B. `crates/ggen-lsp/src/intel/` — the living-LSP nerve (mostly KEEP)](#1b-cratesggen-lspsrcintel--the-living-lsp-nerve-mostly-keep)
    - [1C. Other consumers found (blast radius outside the two target dirs)](#1c-other-consumers-found-blast-radius-outside-the-two-target-dirs)
    - [1D. Test surface that will move with the migration](#1d-test-surface-that-will-move-with-the-migration)
  - [2. ggen KEEPS vs SHEDS — one-line summary](#2-ggen-keeps-vs-sheds--one-line-summary)
  - [3. ocel-core type migration + the serialized-name constraint (load-bearing)](#3-ocel-core-type-migration--the-serialized-name-constraint-load-bearing)
    - [3.1 Type mapping (ggen `ocel_types.rs` → `ocel-core` lib.rs, both read)](#31-type-mapping-ggen-ocel_typesrs-%E2%86%92-ocel-core-librs-both-read)
    - [3.2 CRITICAL serialized-name constraint (NEED §3.1) — verified against the proof tests](#32-critical-serialized-name-constraint-need-%C2%A731--verified-against-the-proof-tests)
  - [4. NDJSON intake fit (NEED §4a) — present, one gap](#4-ndjson-intake-fit-need-%C2%A74a--present-one-gap)
  - [5. Dependency-on-wasm4pm gating](#5-dependency-on-wasm4pm-gating)
  - [6. Staged migration (dependency-ordered, single-writer per stage)](#6-staged-migration-dependency-ordered-single-writer-per-stage)
  - [7. Evidence ledger](#7-evidence-ledger)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL_CONFORM_001 — PM-Retirement Pre-Inventory

**Role:** OCEL-Core / PM-Retirement Planner (read-only; no cargo run)
**Date:** 2026-05-30
**Repo:** ggen @ /Users/sac/ggen (main, clean)
**Spec authority:** `~/wasm4pm/GGEN-NEEDS.md` (ggen is an OCEL *producer*; wasm4pm is the PM authority)
**Status of this doc:** evidence-first inventory. Every retire/keep decision below cites a file I read. The single migration action this enables is a later coding patch; nothing here writes outside `docs/receipts/`.

> Doctrine (from GGEN-NEEDS §0): ggen **emits** OCEL; wasm4pm **parses, mines, conforms, and IO's** it. ggen keeps domain event builders + its own append/emit; it sheds every PM algorithm and the OCEL read/parse path.

---

## 0. Headline finding — BLOCKED status has changed

GGEN-NEEDS frames `ocel-core` as a NEED to be built, gating the migration. **It already ships.** I read it:

- `~/wasm4pm/crates/ocel-core/Cargo.toml` — `name = "ocel-core"`, `version = "26.5.30"`, deps **exactly** `serde`, `serde_json`, `chrono` (the §2 lightweight constraint is met).
- `~/wasm4pm/crates/ocel-core/src/lib.rs` — defines `OCEL`, `OCELType`, `OCELTypeAttribute`, `OCELEvent`, `OCELEventAttribute`, `OCELObject`, `OCELObjectAttribute`, `OCELRelationship`, `OCELAttributeValue` (untagged). Serialized names confirmed below.
- `~/wasm4pm/crates/ocel-core/src/intake.rs` — `NDJsonStream<R: BufRead>` Iterator yielding `OCELRecord::{Event,Object}` per line, **skips blank lines, returns `Err` on a bad line** (NEED §4a NDJSON intake — present, but see §6 caveat: it errors on a truncated trailing line rather than tolerating it).

**Therefore the dependency-on-wasm4pm gate (NEED §5) is partially OPEN:** the linked-dependency part (`ocel-core`) exists and is consumable today. What remains BLOCKED is the **external-CLI-oracle** part (discovery/conformance via `wpm`), and one NDJSON behavioral gap (truncated-line tolerance). See §5/§6.

---

## 1. RETIRE / KEEP INVENTORY (the deliverable)

Legend: **SHED** = delete or replace with wasm4pm; **KEEP** = ggen domain (producer/nerve), untouched; **KEEP-MIGRATE** = ggen domain but must repoint onto `ocel-core` types; **DECIDE** = per-function classification needed (NEED §6).

### 1A. `crates/ggen-graph/src/ocel/` — the duplicated PM stack

| File | Symbol(s) (read) | Disposition | Why |
|---|---|---|---|
| `ocel_types.rs` | `OcelLog`, `OcelObject`, `OcelEvent`, `OcelObjectRef` | **SHED → replace with `ocel-core`** | Lossy non-standard subset (NEED §3). `OcelEvent.activity`/`HashMap` attrs / inline `objects` ≠ OCEL 2.0. Replace with `ocel_core::OCEL*`. |
| `dfg.rs` | `discover_dfg`, `DfgEdge`, `literal_value`, `term_to_u64` | **SHED → `wpm` discovery (CLI oracle)** | Pure process discovery (DFG via SPARQL). NEED §1 maps this to `wasm4pm-algos` discovery. Not a ggen domain concern. |
| `conformance.rs` | `check_lifecycle_order`, `check_guard` | **SHED → `wpm` conformance (CLI oracle)** | Temporal/lifecycle conformance (SPARQL ASK). NEED §1 maps to `check_conformance_token_replay`/`check_conformance_alignment`. **Caveat:** `check_guard` here is the *ocel* free function; the `ggen-marketplace` `check_guard` (state_machine.rs:248) is an unrelated struct method — DO NOT touch it. |
| `projection.rs` | `EvidenceProjector::{project_ocel, extract_ocel, project_prov, extract_prov}` | **SHED `extract_ocel` (read/parse); `project_ocel` becomes obsolete once mining moves out** | `extract_ocel` is OCEL read-from-RDF = parse path (NEED §1 "OCEL log IO read"). `project_ocel` exists only to feed `discover_dfg`/`check_lifecycle_order`; once those leave, the OCEL→RDF projection has no in-ggen consumer. **PROV half (`project_prov`/`extract_prov`) is a separate question — see DECIDE rows.** |
| `coverage.rs` | `generate_coverage_matrix`, `CoverageMatrix`, `RequirementEvidence` | **KEEP (ggen-domain audit), not PM** | This is a hardcoded ggen self-audit requirement→evidence map (R1–R10). It is NOT process mining and has no wasm4pm analogue. Pure ggen artifact. Touches `OcelLog` only via the bin (`verify_audit.rs`), not via PM algos. |
| `self_audit.rs` | `generate_self_audit_log` (+ 28 objects / 18 events) | **DECIDE → likely KEEP-MIGRATE, but candidate for SHED** | Hand-built ggen-domain self-audit log (GALL checkpoint narrative). It is a *producer* of an OCEL log, so by doctrine it stays in ggen — but it is constructed against `OcelLog`/`OcelObject` types, so it is KEEP-MIGRATE onto `ocel-core`. Flag: heavy (650 lines of static data); reconsider whether it earns its keep. |
| `gall_projection.rs` | `project_self_audit`, `extract_self_audit`, `query_relationship`, `to_safe_qualifier`, `to_orig_qualifier` | **DECIDE → SHED `extract_self_audit`/`query_relationship` (read); KEEP qualifier-encoders if still used by emit** | `extract_self_audit` + `query_relationship` are RDF *read-back*/query (PM-adjacent IO). `to_safe_qualifier`/`to_orig_qualifier` are pure string encoders that may stay if emission still needs IRI-safe qualifiers. `project_self_audit` is write-side; obsolete once `extract_ocel`/SPARQL read leaves. |
| `prov_types.rs` | `ProvActivity`, `ProvAgent`, `ProvDerivation`, `ProvDocument`, `ProvEntity`, `ProvGeneration`, `ProvUsage` | **KEEP (not OCEL/PM)** | PROV-O is a separate ontology from OCEL. `ocel-core` does not cover PROV. Consumed by `ggen-core/src/membrane/` and `ggen-projection`. Out of scope for OCEL retirement. |
| `mod.rs` | re-exports of all the above | **KEEP-MIGRATE** | Re-export surface must be re-pointed: drop `OcelLog`/`OcelEvent`/`discover_dfg`/`DfgEdge`/`check_*` re-exports; keep `coverage`/`prov_types`/(decided) self-audit. |

### 1B. `crates/ggen-lsp/src/intel/` — the living-LSP nerve (mostly KEEP)

| File | Symbol(s) (read) | Disposition | Why |
|---|---|---|---|
| `events.rs` | `diagnostic_raised`, `route_selected`, `repair_suggested`, `repair_applied`, `receipt_emitted`, `gate_result`, `refusal_emitted`, `attach_attribution`, `Attribution`, `agent_ref`, `event_id`, `episode_id`, `activity`/`obj_type` consts | **KEEP-MIGRATE (the producer)** | Exactly the surface NEED §6 says ggen keeps. These are the OCEL event *builders*. They must construct `ocel-core` `OCELEvent` instead of `ggen_graph::ocel::OcelEvent`. **This is where the §3.1 serialized-name constraint lands.** |
| `log.rs` | `IntelLog::{append, read, path, at_root, new}`, `default_path` | **KEEP `append` (emit); SHED `read` (parse)** | `append` is ggen's own NDJSON emit (producer; stays, crash-safe). `read` folds NDJSON→`OcelLog` = the parse path NEED §1/§4 moves to `ocel-core::intake::NDJsonStream`. |
| `mine.rs` | `mine`, `MineReport`, `family_evidence`, `episode_closed`, `synthesize_routes`, `is_failure_target`, `append_history`, `emit_promotion_receipt`, `write_report` | **SHED the PM orchestration core; KEEP the promotion/receipt/history glue** | `mine()` body that calls `EvidenceProjector::project_ocel` + `discover_dfg` + `check_lifecycle_order` (`episode_closed`) is exactly the "mining orchestration" NEED §1 retires. **But** route promotion, `emit_promotion_receipt`, `append_history`, `synthesize_routes`, `MineReport` are ggen-domain promotion law — they consume DFG edges + per-family evidence. Re-architect: `mine()` shells out to `wpm` for DFG+conformance, then keeps the promotion glue. Significant surgery, not a clean delete. |
| `metrics.rs` | `compute_metrics`, `group_episodes`, `ImproveMetrics`, `MetricValue` | **KEEP-MIGRATE** | `group_episodes(log: &OcelLog)` + `IntelLog::read()` (metrics.rs:128) consume the parsed log. ggen-domain metrics, but coupled to `OcelLog`/`IntelLog::read` — must repoint onto `ocel-core` events once `read` moves to intake. |
| `replay.rs` | `replay_case`, `verify_promotion`, `CaseReplay`, `PromotionReplay` | **KEEP-MIGRATE** | Uses `IntelLog::read()` (replay.rs:35,123). ggen-domain case replay; repoint onto `ocel-core` events via intake. No PM algos. |
| `receipt.rs`, `history.rs`, `field.rs` | (not OCEL-typed; receipts/promotion ledger/field gauge) | **KEEP (untouched)** | Read confirms no `OcelLog`/PM-algo dependency; pure ggen domain. |
| `mod.rs` | re-exports | **KEEP-MIGRATE** | Drop nothing structurally; type-repoint flows through. |

### 1C. Other consumers found (blast radius outside the two target dirs)

Grep over `crates/` (non-ocel, non-test) for the PM symbols:

| Consumer | Use | Disposition |
|---|---|---|
| `crates/ggen-graph/src/lib.rs:34` | `pub use ocel::{check_guard, check_lifecycle_order, discover_dfg, DfgEdge};` | **SHED** these re-exports when conformance/dfg leave. |
| `crates/ggen-graph/src/doctor/mod.rs:20` | `EvidenceProjector::extract_ocel(graph)` | **KEEP-MIGRATE / DECIDE** — doctor reads an OCEL log from a graph. Either repoint onto intake or drop if doctor's OCEL check is PM-adjacent. |
| `crates/ggen-graph/src/interchangeable.rs:163,168` | `extract_ocel` / `project_ocel` round-trip | **KEEP-MIGRATE** — interchangeable-part test of OCEL round-trip; repoint onto `ocel-core`. |
| `crates/ggen-graph/src/bin/{emit_audit,verify_audit}.rs` | `generate_self_audit_log`, `generate_coverage_matrix`, `CoverageMatrix`, `OcelLog` | **KEEP-MIGRATE** — ggen self-audit bins; repoint to `ocel-core` types. |
| `crates/ggen-graph/src/bin/gall_*` | several use `ggen_graph::ocel::*` | **DECIDE per-bin** — GALL observation bins; classify each (producer vs read) when migrating. |
| `crates/ggen-core/src/membrane/mod.rs` | re-exports its OWN `ocel`/`prov` submodules (`OcelValue` etc.) — NOT `ggen_graph::ocel` | **KEEP (separate)** — confirmed `membrane::ocel` is a distinct module, not the ggen-graph one. Do not conflate. |
| `crates/ggen-marketplace/.../state_machine.rs:248` | `fn check_guard(&self, ...)` | **KEEP (false positive)** — struct method, unrelated to `ocel::check_guard`. |

### 1D. Test surface that will move with the migration

These tests assert against the retiring symbols (will need updating in lockstep, NEED §3.1):

- `crates/ggen-graph/tests/adversarial_dfg_test.rs` (DFG)
- `crates/ggen-graph/tests/ocel_diagnostics_doctor_test.rs` (extract_ocel + prov)
- `crates/ggen-graph/tests/ocel_self_audit.rs` (self_audit + projection round-trip; asserts `objects.len()==28`, `events.len()==18`)
- `crates/ggen-graph/tests/vision2030_coverage.rs` (CoverageMatrix)
- `crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs`, `ggen_tpl_001_living_loop.rs` — **the load-bearing proof receipts (§3 below).**

---

## 2. ggen KEEPS vs SHEDS — one-line summary

**ggen KEEPS (producer + nerve, NEED §6):**
- Event builders: `intel/events.rs` (all `*_raised`/`*_selected`/`*_applied`/`gate_result`/`receipt_emitted`/`attach_attribution`).
- Own NDJSON emit: `intel/log.rs::IntelLog::append` (+ `default_path`).
- Promotion law: `intel/mine.rs` route synthesis/promotion/receipt/history glue; `intel/{metrics,replay,receipt,history,field}.rs`.
- ggen-domain audit: `ocel/coverage.rs`, `ocel/prov_types.rs`, and (decided) `ocel/self_audit.rs`.

**ggen SHEDS (PM algorithms + read/parse, NEED §1):**
- `ocel/dfg.rs` (discovery) → `wpm` discovery.
- `ocel/conformance.rs` (lifecycle/guard conformance) → `wpm` conformance.
- `ocel/projection.rs::extract_ocel` and `intel/log.rs::IntelLog::read` (OCEL read/parse) → `ocel-core::intake::NDJsonStream`.
- `ocel/ocel_types.rs` (lossy types) → `ocel-core` types.
- the `mine()` discovery+conformance *body* (orchestration) → `wpm` CLI oracle.

---

## 3. ocel-core type migration + the serialized-name constraint (load-bearing)

### 3.1 Type mapping (ggen `ocel_types.rs` → `ocel-core` lib.rs, both read)

| ggen today | ocel-core (serde name) | Migration action |
|---|---|---|
| `OcelEvent.activity: String` | `OCELEvent.event_type` **serde `"type"`** | map activity→event_type. **BREAKS substring proofs (§3.2).** |
| `OcelEvent.timestamp: DateTime<Utc>` | `OCELEvent.time: DateTime<FixedOffset>` | widen Utc→FixedOffset (Utc ⊂ FixedOffset; lossless). |
| `OcelEvent.objects: Vec<OcelObjectRef{id,type,qualifier:Option}>` | `OCELEvent.relationships: Vec<OCELRelationship{object_id (serde "objectId"), qualifier: String}>` | inline refs → `relationships`; **qualifier becomes non-Option `String`** (ggen's `None` qualifiers — `file_ref` always Some, but `self_audit` has `qualifier: None` refs — must map None→"" or a sentinel). Object *types* move to a separate `objects` table + `OCELType` decls. |
| `OcelEvent.attributes: HashMap<String,String>` | `OCELEvent.attributes: Vec<OCELEventAttribute{name, value: OCELAttributeValue}>` | each entry → `OCELAttributeValue::String` (lossless). HashMap→Vec loses nothing but ordering (sort for determinism). |
| `OcelObject{id, type (serde via rename "type"), attributes: HashMap}` | `OCELObject{id, object_type (serde "type"), attributes: Vec<OCELObjectAttribute{name,value,time}>, relationships}` | `OCELObjectAttribute` requires a `time: DateTime<FixedOffset>` — ggen object attrs have no timestamp; supply event/run time. |
| *(none)* | `OCEL.event_types: Vec<OCELType>`, `OCEL.object_types: Vec<OCELType>` (serde `"eventTypes"`/`"objectTypes"`) | ggen must **synthesize type declarations** from its `activity::*` consts and `obj_type::*` consts at emit/fold time. `intake::NDJsonStream` does NOT synthesize these — it yields records; the caller assembles `OCEL`. |

`OCELAttributeValue` (read, untagged): `Integer/Float/Boolean/Time/String/Null`. ggen attrs are all strings → `::String`.

### 3.2 CRITICAL serialized-name constraint (NEED §3.1) — verified against the proof tests

The living-LSP proof tests read the **raw on-disk JSONL** and assert *substrings on serialized field values*. I read the assertion helpers:

- `crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs:84-91` — `has_template_event(lines, activity)` is true iff a line `l.contains(activity)` **AND** `l.contains("item.tera")` **AND** `l.contains("GGEN-TPL-001")`. Call sites pass `"DiagnosticRaised"` (lines 158, 208).
- `crates/ggen-lsp/tests/ggen_tpl_001_living_loop.rs:175-181` — identical helper (`l.contains(activity) && l.contains("item.tera") && l.contains("GGEN-TPL-001")`).
- Both read `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` (stale_clear.rs:76, living_loop.rs:167).

**What today's emission writes** (from `intel/log.rs::append` serializing `ggen_graph::ocel::OcelEvent`): a line containing `"activity":"DiagnosticRaised"`, the object id `"item.tera"` (via `file_ref`), and the diagnostic_code object id `"GGEN-TPL-001"`.

**What `ocel-core` serialization would write instead:** `"type":"DiagnosticRaised"` (activity renamed), and the file/code become `relationships[].objectId` values — `"item.tera"` and `"GGEN-TPL-001"` **survive as objectId substrings**, but the `activity` field name does NOT.

**Net constraint:**
- `l.contains("DiagnosticRaised")` — **SURVIVES** (the value string is unchanged; only the key changes from `activity` to `type`). The helper greps the *value*, not the key, so the activity-name rename is tolerated.
- `l.contains("item.tera")` and `l.contains("GGEN-TPL-001")` — **SURVIVE** as `objectId` values in `relationships`.

So the two proof tests as written (substring-on-value) are **resilient to the field-name rename** — BUT this is a knife-edge. Any test that greps `"activity":"DiagnosticRaised"` (key+value) WOULD break. The migration patch MUST: (a) audit every proof test for key-bearing substrings, and (b) if any exists, either keep an `activity` serde alias on the ggen-side emit OR update the assertion in the same commit. **The 6-link chain `DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted` must remain mineable** — i.e., the activity-value strings (the `activity::*` consts) must serialize unchanged regardless of which key carries them.

**Episode/receipt id stability:** `event_id = blake3(activity‖file‖run_id‖seq)`, `episode_id = blake3(file‖code‖run_id)` (events.rs:81-98). These are computed from the *value* strings, not the serialization shape — **unaffected by the type migration** as long as the `activity`/`obj_type` const values are preserved.

---

## 4. NDJSON intake fit (NEED §4a) — present, one gap

`ocel-core::intake::NDJsonStream<R: BufRead>` (read) folds one `OCELRecord::{Event,Object}` per line, skips blank lines, applies an `ExtractionPlan` filter, and enforces relationship referential integrity. This directly replaces `IntelLog::read`.

**Gap (verified):** ggen's `IntelLog::read` (log.rs:89-91) **silently skips** a truncated/corrupt trailing line (`let Ok(ev) = ... else { continue }`) — required because ggen appends concurrently (log.rs test `truncated_trailing_line_is_skipped`). `NDJsonStream::next` (intake.rs:99-101) instead **returns `Err`** on a bad line. To preserve ggen's crash-safety invariant, either: (a) wasm4pm relaxes intake to tolerate a truncated *final* line (NEED §4a acceptance "truncated final line tolerated"), or (b) ggen's caller treats a trailing `Err` from the iterator as EOF. This is a real behavioral mismatch to resolve before the swap.

---

## 5. Dependency-on-wasm4pm gating

| Capability | Mechanism (NEED §5) | Status |
|---|---|---|
| `ocel-core` types + intake | **linked git dependency** (`ocel-core = { git = ".../wasm4pm", tag = "ocel-core-vX" }`) | **OPEN** — crate exists at v26.5.30, deps minimal. ggen-lsp/ggen-graph can add the dep today. (Currently ggen-lsp depends only on `ggen-graph` path — `crates/ggen-lsp/Cargo.toml:17`.) |
| DFG discovery + conformance | **external CLI oracle** `wpm` (subprocess, NOT linked) | **BLOCKED** — requires `wpm mining/conformance` reachable + a defined invocation contract. Not yet wired; Chicago-TDD demands the real `wpm` boundary. |
| NDJSON truncated-line tolerance | intake.rs behavior | **BLOCKED (minor)** — see §4 gap. |

**Migration is GATED until:** (1) ggen pins an `ocel-core` git tag; (2) `wpm` CLI oracle contract is defined + the truncated-line gap is resolved. Type migration (§3) can land *independently* of the CLI-oracle work because intake + types are ready.

---

## 6. Staged migration (dependency-ordered, single-writer per stage)

**Stage 0 — Pin the dependency (unblocks everything else).**
Add `ocel-core` git dep to `crates/ggen-graph/Cargo.toml` (and re-export from `ggen-graph` so `ggen-lsp` gets it via existing path dep). Acceptance: `cargo make check` green.

**Stage 1 — Type swap behind the producer (KEEP-MIGRATE, no behavior change).**
Repoint `intel/events.rs` builders to construct `ocel_core::OCELEvent`/`OCELObject`/`OCELRelationship`. Preserve all `activity::*`/`obj_type::*` value strings. Map None qualifiers → sentinel; HashMap attrs → sorted `Vec<OCELEventAttribute::String>`; synthesize `event_types`/`object_types`. **Audit + co-update every proof test (§3.2) in THIS commit.** Acceptance: `ggen_tpl_001_*` + `ggen_harness_001_living_loop` green; 6-link chain still substring-mineable.

**Stage 2 — Swap the reader (SHED `IntelLog::read` + `extract_ocel`).**
Replace `IntelLog::read` with an `ocel-core::intake::NDJsonStream` fold (resolve §4 truncated-line gap first). Repoint `metrics.rs`/`replay.rs`/`mine.rs` onto the new fold. Delete `projection.rs::extract_ocel` + `gall_projection.rs::extract_self_audit`/`query_relationship`. Acceptance: metrics/replay tests green.

**Stage 3 — Externalize discovery + conformance (SHED `dfg.rs`/`conformance.rs`).**
Re-architect `mine()` to shell out to `wpm` for DFG + conformance, keeping the promotion/receipt/history glue. Delete `dfg.rs`, `conformance.rs`, the `lib.rs:34` re-exports, and `projection.rs::project_ocel` once it has no consumer. Acceptance: `mine_*` tests pass against real `wpm`; NEED §7.6 proof obligation (ggen's 6-link log mines into a conforming process via `wpm`).

**Stage 4 — Retire `ocel_types.rs` + clean re-exports.**
Once no symbol references `ggen_graph::ocel::Ocel*`, delete `ocel_types.rs` and prune `mod.rs`. Decide `self_audit.rs`/`coverage.rs` final home (KEEP as ggen audit, migrated to `ocel-core` types). Acceptance: full `cargo make test` + clippy `-D warnings` green.

**Single-writer note:** Stages 1–4 each touch overlapping files (`mod.rs`, `mine.rs`); they MUST be sequential, not concurrent (one writer per file, per the conductor phasing rule).

---

## 7. Evidence ledger

| Claim | Source read |
|---|---|
| ggen OCEL types are a lossy subset | `crates/ggen-graph/src/ocel/ocel_types.rs` (full) |
| DFG discovery is in-ggen SPARQL | `crates/ggen-graph/src/ocel/dfg.rs` (full) |
| Conformance is in-ggen SPARQL ASK | `crates/ggen-graph/src/ocel/conformance.rs` (full) |
| Mining orchestration body uses project+dfg+lifecycle | `crates/ggen-lsp/src/intel/mine.rs:210-249` |
| Read/parse path = `IntelLog::read` + `extract_ocel` | `crates/ggen-lsp/src/intel/log.rs:81-105`; `crates/ggen-graph/src/ocel/projection.rs:170-375` |
| Event builders are the keep surface | `crates/ggen-lsp/src/intel/events.rs` (full) |
| Promotion/metrics/replay are ggen domain | `intel/mine.rs:266-349`, `intel/metrics.rs:12,62,128`, `intel/replay.rs:15,35,123` |
| `ocel-core` ships, minimal deps | `~/wasm4pm/crates/ocel-core/Cargo.toml` |
| ocel-core OCEL 2.0 serde names | `~/wasm4pm/crates/ocel-core/src/lib.rs:8-78` |
| NDJSON intake exists; errors (not skips) bad line | `~/wasm4pm/crates/ocel-core/src/intake.rs:60-107` |
| Proof tests grep JSONL value substrings | `crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs:76,84-91,158`; `ggen_tpl_001_living_loop.rs:167,175-181` |
| marketplace `check_guard` is a false positive | `crates/ggen-marketplace/src/marketplace/rdf/state_machine.rs:248` |
| membrane `ocel` is a separate module | `crates/ggen-core/src/membrane/mod.rs:10-19` |
| PROV is not OCEL, keep | `crates/ggen-graph/src/ocel/prov_types.rs`; consumers `ggen-core/src/membrane/`, `ggen-projection/src/lib.rs` |

**Unverified / flagged for follow-up:**
- `wasm4pm-algos` discovery/conformance function names (NEED §1 cites `check_conformance_token_replay`/`check_conformance_alignment`) — I did NOT open `wasm4pm-algos`; taken from the spec, not read.
- Per-bin classification of `crates/ggen-graph/src/bin/gall_*` (10 bins) — DECIDE deferred; not individually read.
- `wpm` CLI subcommand contract (`wpm mining conformance` / `wpm run`) — not verified to exist.
