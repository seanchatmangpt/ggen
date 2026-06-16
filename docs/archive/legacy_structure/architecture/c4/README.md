<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Living LSP — C4 model (single source of truth)](#ggen-living-lsp--c4-model-single-source-of-truth)
  - [Render](#render)
  - [Views (derived from the one model)](#views-derived-from-the-one-model)
  - [Accuracy corrections vs the authored C4 (verified against `main @ 1525c1a7`)](#accuracy-corrections-vs-the-authored-c4-verified-against-main--1525c1a7)
  - [The doctrine the model encodes](#the-doctrine-the-model-encodes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Living LSP — C4 model (single source of truth)

`workspace.dsl` is a [Structurizr DSL](https://docs.structurizr.com/dsl) model. **One model → all views.** New diagnostic species / checkpoints update the one model; every view stays consistent. This is the deliberate defense against the architecture-docs drift this repo has had (docs once described ~68 crates against 15 real).

## Render

```bash
# Structurizr Lite (interactive, all views)
docker run -it --rm -p 8080:8080 -v "$(pwd)/docs/architecture/c4":/usr/local/structurizr structurizr/lite
# open http://localhost:8080

# Or export to PlantUML / Mermaid with the structurizr-cli
structurizr-cli export -workspace docs/architecture/c4/workspace.dsl -format mermaid -output docs/architecture/c4/export
```

## Views (derived from the one model)

| Key | Level | Scope |
|-----|-------|-------|
| `C1_Context` | C1 | System context — author, agents, editor, git/CI, wasm4pm oracle |
| `C2_Containers` | C2 | ggen-lsp / ggen sync / open ontology / graph / intel-OCEL / receipts |
| `C3_ggen_lsp` | C3 | The living nerve inside ggen-lsp |
| `C3_Open_Ontology` | C3 | Source-law surfaces (ggen.toml binds producer/consumer/output) |
| `C4_Living_Clear` | C4 / runtime | Cross-surface repair & living clear (dynamic) — the proof-of-life chain |

The **receipt / OCEL / process-evidence** perspective (your separate C3) is represented by the `intelLog` + `receipts` containers in `C2_Containers` plus the `C4_Living_Clear` dynamic view (events → log → receipt → wasm4pm) rather than a synthetic container, because the event builders physically live inside `ggen-lsp` (`intel/events.rs`).

## Accuracy corrections vs the authored C4 (verified against `main @ 1525c1a7`)

Evidence-first: the model reflects real code, not aspiration.

- **`GGEN-OUT-001` / `detect_out_001` → PLANNED (dashed).** It is **not implemented** — no such function, species unregistered. The registry holds exactly two species: `GGEN-TPL-001` (active) + `GGEN-HARNESS-001` (active). OUT-001 is BLOCKED-on-ambiguity pending a pre-inventory.
- **`harness_analyzer` / `detect_harness_001` → ACTIVE.** HARNESS-001 shipped ALIVE (PR #203, this session), so it is no longer "future." Added `harness_index.rs` as a real component.
- **`ocel-core`, replay-packets → external/Planned**, consistent with the wasm4pm boundary (`~/wasm4pm/GGEN-NEEDS.md`).

All 12 source files named in the model were confirmed to exist.

## The doctrine the model encodes

```
ggen.toml says what should be built.
Open Ontology says what it means.
ggen-lsp says whether the relation is lawful (read-only admissibility).
observe_diagnostics says whether repair really happened (clear = keyed subtraction + residual preservation).
OCEL says what the work actually did.
wasm4pm says whether the trace was lawful (process-law oracle).
ggen sync is the only thing allowed to materialize outputs (actuation boundary).
receipts prove what became alive.
```
