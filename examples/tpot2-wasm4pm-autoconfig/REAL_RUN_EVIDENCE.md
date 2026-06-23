# Real `ggen sync` evidence — the honest gap is now CLOSED

Prior reports honestly said `ggen sync` could not run in this container. That
blocker has been **resolved and the real pipeline executed**. This file records the
captured evidence. (`generated/` and `.ggen/` are gitignored as per-run output, so
the proof is transcribed here verbatim.)

## 1. The build (real `ggen` binary)

Provisioned per `BUILD_PROVISIONING.md` — cloned the 4 sibling dependency repos and
reinstalled the corrupted pinned-nightly toolchain — then:

```
$ cargo build -p ggen-cli-lib --bin ggen
   Compiling ggen-core v26.6.11 (/home/user/ggen/crates/ggen-core)
   Compiling ggen-cli-lib v26.6.11 (/home/user/ggen/crates/ggen-cli)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 13m 29s
$ target/debug/ggen --version
ggen 26.6.11
```

## 2. The sync (real μ-pipeline execution)

```
$ ggen sync          # run from examples/tpot2-wasm4pm-autoconfig/
✓ Generated 6 files in 907ms
  2 inference rules, 6 generation rules
  35730 total bytes written
  "status": "success"   (exit 0)
```

Generated (real bytes): `generated/ggen.toml` 6387, `tpot_config.py` 18628,
`pipeline.json` 3341, `SEARCH_SPACE.md` 5785, `STAGE_PLAN.md` 1001,
`objectives.json` 588.

## 3. The hardening passed a REAL run

The driver enables `[validation] strict_mode = true` + the SHACL gate
(`shacl = ["ontology/tpot-shapes.ttl"]`). **Sync succeeded (exit 0), so strict mode
and SHACL enforcement accepted the inference-derived data** — validating the
source-analysis in research/06 (shapes pass; `fitnessScore` is `xsd:decimal`) and
research/07 (both CONSTRUCTs carry `ORDER BY`, so no E0011 under strict mode). This
is no longer a source-analysis claim; it is a passed run.

## 4. The selection matches the independent reference (determinism cross-check)

The real ggen/oxigraph SPARQL produced **exactly** the 9 elite operators that the
pure-Python `reference_autoconfig.py` computed:

```
ggen sync pipeline.json == verify/out/reference_pipeline.json    MATCH: True
ingest=pnml_import · discover=inductive_miner · discover_oc=ocel_petri_net ·
analyze=compute_activity_transition_matrix · conform=complexity_metrics ·
learn=ml_classify · predict=compute_ewma · simulate=playout · orchestrate=agentic_pipeline
```

## 5. The RULE 5/6 fixes are validated by real output

- `generated/STAGE_PLAN.md` — all 9 stage rows populated (label, category, operator
  count 4/20/6/10/4/8/5/2/1). The pre-fix reused template would have rendered blank
  operator columns; the dedicated `stage-plan.md.tera` renders the correct data.
- `generated/objectives.json` — objectives are genuinely query-derived (quality:
  maximize qualityTier w=1; **RuntimeCost**: minimize speedTier w=0.5), not the
  previously-hardcoded literal.

## 6. The cryptographic receipt (`.ggen/receipts/latest.json`)

Satisfies every invariant in `.claude/rules/coding-agent-mistakes.md` §4.2:

| field | value | invariant |
|-------|-------|-----------|
| `operation_id` | `b655b365-63de-4f25-a599-0207d97d1fec` | real UUID v4, non-zero ✓ |
| `timestamp` | `2026-06-20T15:45:26.782511035Z` | RFC-3339 ✓ |
| `signature` | `94956e48540cc7feee...` (non-empty) | non-empty ✓ |
| `input_hashes` | **18** (actuator + ggen.toml + 4 ontologies + 6×(query,template)) | every input bound ✓ |
| `output_hashes` | **6** (the 6 generated artifacts) | every output bound ✓ |
| `previous_receipt_hash` | present | receipt chain ✓ |

The 18-input / 6-output count is **exactly** what research/06 predicted from source
analysis before any run.

## 7. What remains honestly unmet

- `conformance_check.py` stays **Mode A**: `ggen sync` emits no OCEL event log
  (research/05 §1.6 confirmed this from source), so the run-derived conformance
  criteria (temporal lawfulness / fitness / variant control) still require sync
  instrumentation (research/05 §4.3). The structural conformance (10/10, rejecting
  the 8 impossible logs) is unchanged and green.
- `generated/` + `.ggen/receipts/` are gitignored per-run output; this transcript is
  the committed record.
