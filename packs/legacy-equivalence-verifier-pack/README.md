# legacy-equivalence-verifier-pack

G7 reusable infrastructure for legacy-capability equivalence verification. This pack does
**not** verify any real capability yet -- it manufactures the generic manifest+verifier
scaffolding that a later, concurrent ontology-extraction agent will populate with real
`eqv:EquivalenceCase` facts.

## What it generates (`ggen sync run`)

- `consumer/legacy-equivalence/case_manifest.json` -- one entry per `eqv:EquivalenceCase`
  individual in `ontology.ttl`, produced via `queries/cases.rq`.
- `consumer/legacy-equivalence/suites/verify-all.sh` -- thin wrapper that locates and invokes
  the generic `tools/v26.8.1/equivalence_runner.py` engine against the generated manifest.
- `consumer/legacy-equivalence/verifier-report.schema.json` -- machine-readable schema for the
  runner's output report.
- `consumer/legacy-equivalence/README.md` -- generated usage notes for the output directory.

## Ontology vocabulary (`eqv:` = `https://ggen.io/ontology/legacy-equivalence#`)

| Class / Property | Meaning |
|---|---|
| `eqv:LegacyCapability` | A legacy capability under study (documentation node; not itself executable). |
| `eqv:EquivalenceCase` | One equivalence check: legacy adapter vs. current adapter (or a restore/refusal contract for ARCHIVED/REFUSED dispositions). |
| `eqv:legacyAdapter` / `eqv:currentAdapter` | Shell commands (`sh -c`-invoked by the runner) that exercise the legacy and current surfaces respectively. |
| `eqv:successInput` / `eqv:failureInput` | Input fixtures the case exercises (may be empty strings for input-free adapters). |
| `eqv:normalizationPolicy` | One of `none`, `strip_timestamps`, `sort_json_keys` -- applied to both sides before comparison. |
| `eqv:expectedDisposition` | One of `PRESERVED`, `SUBSUMED`, `REPLACED`, `ARCHIVED`, `REFUSED`. |
| `eqv:observableSurface` | One or more of `exit_code`, `stdout`, `stderr`, `filesystem_delta`, `generated_bytes`, `diagnostics`, `receipt_fields`, `event_order`, `side_effects`, `recovery_result`. The runner dispatches purely off this list -- no per-case branches. |
| `eqv:timeoutSeconds` | Wall-clock timeout per adapter invocation. |
| `eqv:recoveryAction` | For ARCHIVED cases, the restore command proving the capability can still be recovered; `"none"` otherwise. |
| `eqv:expectedDiagnosticSubstring` | For REFUSED cases, a substring that must appear in the current adapter's diagnostic output. |

## Why the runner lives outside this pack

`tools/v26.8.1/equivalence_runner.py` is generic and reusable: it does not know about this
pack's specific synthetic cases, only about the case-manifest JSON shape and the
`observable_surfaces` dispatch contract. A future project can point the same runner at a
completely different manifest (real legacy-capability data) without modification. See that
file's own docstring, and `tools/v26.8.1/example_equivalence_cases.json` for the hand-crafted
positive/negative proof cases used to validate the runner during this scaffolding phase.
