# cargo-cicd 80/20 Roadmap

20% of additions that deliver 80% of the remaining value, ordered by impact.

**Design principle: TPS + DfLSS.** Every gate either passes or pulls the Andon cord. No WARN
verdicts that silently pass. No BLOCKED states that skip checks. Required binaries are installed
and verified by pre-flight before use — a missing binary is a defect in the process, not an
expected runtime condition.

---

## Status

| # | Item | Status |
|---|---|---|
| 1 | `affidavit seal` / `verify` as CI gate | ✅ Done — cargo-cicd job seals + verifies after pipeline run |
| 2 | `lsp check` (anti-llm-cheat) on changed `.rs` | ✅ Done — cargo-cicd job runs lsp check post-pipeline |
| 3 | `workspace sync` in `comprehensive-test` | ✅ Done — builds ggen binary, then calls workspace sync |
| 4 | `status audit` as release gate | ✅ Done — `wpm` installed + pre-flight verified; CI and release both gate on status audit |
| 5 | `certification show` verb | ✅ Done — `CertificationNoun` + `ShowVerb` in cargo-cicd; reads evidence journal, evaluates IEC 61508 SIL 1 + ISO 26262 ASIL A coverage |

---

## 1. ✅ `cargo cicd affidavit seal` / `verify` as CI gate

After `pipeline run`, the cargo-cicd job seals the accumulated XES evidence into a BLAKE3
receipt via `cargo cicd affidavit seal`, then certifies it with `cargo cicd affidavit verify`.

`affi` is installed in CI via `tools: cicd,affi,wpm` and verified on PATH by the pre-flight step
before any evidence command runs. A missing binary fails the job immediately (Andon).

**Why this closes a gap**: Without the seal, evidence logs can be regenerated or overwritten
silently between runs. The BLAKE3 chain makes any post-hoc modification detectable — one
bit-flip anywhere flips the verify verdict to REJECT. This satisfies cargo-cicd's invariant
**E1** (*the engine never grades itself; only an external witness issues a verdict*) and
aligns with ggen's own `.ggen/receipts/` BLAKE3 provenance pattern.

---

## 2. ✅ `cargo cicd lsp check` on changed `.rs` files

The cargo-cicd job runs `cargo cicd lsp check` after the pipeline. This requires the
`anti-llm-cheat` feature, compiled in via `--features autonomic,affidavit,anti-llm-cheat`.

The check scans every `.rs` file changed since `origin/main` for:
- Undeclared external dependencies (London TDD patterns)
- Fabricated examples without real evidence (OTEL/test gaps)
- Stub implementations marked as complete

Blocking violations exit `FAIL` and stop CI. This converts the no-fabrication and Chicago TDD
rules from conventions into a structural CI gate.

---

## 3. ✅ `cargo cicd workspace sync` in `comprehensive-test`

After all tests pass in `comprehensive-test`, the job builds the `ggen` binary and calls
`cargo cicd workspace sync`. This triggers `ggen sync` (the full μ₁–μ₅ pipeline) with the
real binary, producing a `.ggen/receipts/latest.json` and appending to the OCEL intel log
at `.ggen/ocel/agent-edit-events.ocel.jsonl`.

**Van der Aalst cross-system causality**: this is the step that creates cross-system event
objects — ggen's own OCEL log records what the RDF pipeline produced, and cargo-cicd's
evidence log records that `workspace:sync` ran and PASS'd. Combined, these two OCEL streams
prove the declared A = μ(O) formula executed lawfully.

---

## 4. ✅ `cargo cicd status audit` as release gate

`wpm` is a supported tool in `install-cargo-tools/action.yml`:
```yaml
- name: Install wpm (wasm4pm process oracle)
  if: contains(inputs.tools, 'wpm')
  run: cargo install --git https://github.com/seanchatmangpt/wasm4pm --bin wpm --locked
  shell: bash
```

Both the `cargo-cicd` CI job (tools: `cicd,affi,wpm`) and the `release-health` job (tools:
`cicd,wpm`) include a pre-flight step that exits non-zero if either binary is not found:
```yaml
- name: Pre-flight — verify affi and wpm on PATH (ANDON if missing)
  run: |
    set -euo pipefail
    command -v affi || (echo "ANDON: affi not found — pull the cord"; exit 1)
    command -v wpm  || (echo "ANDON: wpm not found — pull the cord"; exit 1)
```

The `status audit` step runs **after** the pre-flight; by the time it executes, wpm is
guaranteed on PATH. The BLOCKED code path inside cargo-cicd is never reached in this pipeline.

**Oracle verdicts** (all terminal — no non-blocking states):
- TRUTHFUL (fitness ≥ 0.95): process matches declared model → CI passes
- VARIANCE (0.70–0.95): divergence detected → CI fails (Andon)
- DECEPTIVE (<0.70): severe mismatch → CI fails (Andon)

---

## 5. ✅ `cargo cicd certification show` verb

`src/nouns/certification.rs` in the cargo-cicd source implements `CertificationNoun` with a
single `ShowVerb`:

1. Reads the evidence journal (`target/cargo-cicd/evidence/events.jsonl`) via `read_journal()`
2. Extracts unique `complete`-lifecycle commands from the journal
3. Evaluates all IEC 61508 SIL 1 requirements against executed commands via `check_requirement()`
4. Evaluates all ISO 26262 ASIL A requirements against executed commands via `check_requirement()`
5. Prints both `compliance_summary()` outputs + the evidence command list

The noun is registered unconditionally in `main.rs` (no feature flag — no external binary
dependency). Once pushed to the upstream `seanchatmangpt/cargo-cicd` repo, add to CI:

```yaml
- name: Certification compliance summary (IEC 61508 / ISO 26262)
  if: always()
  run: cargo cicd certification show
```

---

## Van der Aalst OCEL Cross-System Causality Chain

The full provenance chain when all items above are complete:

```
ggen sync (μ₁–μ₅ pipeline)
  → .ggen/ocel/agent-edit-events.ocel.jsonl        [ggen's object-centric event log]
  → .ggen/receipts/latest.json                      [BLAKE3 transition receipt]

cargo cicd pipeline run
  → target/cargo-cicd/evidence/events.jsonl         [process events: start/complete per command]
  → target/cargo-cicd/evidence/events.xes           [XES 2.0 trace]
  → target/cargo-cicd/evidence/events.ocel.json     [OCEL 2.0 with cargo object types]
  → target/cargo-cicd/evidence/audit-events.xes     [canonical 3-pass trace for oracle]

cargo cicd affidavit seal
  → target/cargo-cicd/evidence/affidavit/receipt.json  [BLAKE3 chain over all events]

cargo cicd affidavit verify
  → ACCEPT: chain is intact, no events modified post-hoc
  → REJECT: bit-flip detected, evidence compromised — CI fails (Andon)

cargo cicd status audit (wpm oracle — wpm guaranteed on PATH by pre-flight)
  → TRUTHFUL  (fitness ≥ 0.95): process matches declared model → pass
  → VARIANCE  (0.70–0.95): divergence detected → CI fails (Andon)
  → DECEPTIVE (<0.70): severe mismatch → CI fails (Andon)
```

**Van der Aalst's 8 success criteria** and which steps satisfy them:

| Criterion | Satisfied by |
|---|---|
| Event log completeness | `pipeline run` → events.jsonl with OCEL 2.0 attributes |
| Object lifecycle soundness | cargo object types in events.ocel.json (WorkspaceState, TargetState, etc.) |
| Temporal lawfulness | BLAKE3 chain enforces ordering; any reorder breaks `affidavit verify` |
| Conformance fitness ≥ 0.9 | `status audit` via wpm oracle on audit-events.xes |
| Variant control | `pipeline run` always executes the same 6-step sequence |
| Deviation-free | wpm TRUTHFUL verdict confirms no skipped/repeated stages |
| Object-centric consistency | ggen OCEL + cargo-cicd OCEL share `workspace_id` as join key |
| Semantic invariants | `affidavit verify` ACCEPT + `evidence doctor` PASS |

The **join key** between the two OCEL streams is `workspace_id` (set in `EngineState` to the
workspace root path). ggen's intel log uses the same path as the case identifier. A future
`cargo cicd evidence merge` command could combine both streams for full cross-system pm4py
conformance checking.
