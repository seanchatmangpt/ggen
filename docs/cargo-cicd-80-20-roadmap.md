# cargo-cicd 80/20 Roadmap

20% of additions that deliver 80% of the remaining value, ordered by impact.

---

## Status

| # | Item | Status |
|---|---|---|
| 1 | `affidavit seal` / `verify` as CI gate | ✅ Done — cargo-cicd job seals + verifies after pipeline run |
| 2 | `lsp check` (anti-llm-cheat) on changed `.rs` | ✅ Done — cargo-cicd job runs lsp check post-pipeline |
| 3 | `workspace sync` in `comprehensive-test` | ✅ Done — builds ggen binary, then calls workspace sync |
| 4 | `status audit` as release gate | ⏳ Blocked on `wpm` binary availability in CI |
| 5 | `certification show` verb | ⏳ Needs new verb added to cargo-cicd itself |

---

## 1. ✅ `cargo cicd affidavit seal` / `verify` as CI gate

After `pipeline run`, the cargo-cicd job seals the accumulated XES evidence into a BLAKE3
receipt via `cargo cicd affidavit seal`, then certifies it with `cargo cicd affidavit verify`.

Both commands degrade gracefully when `affi` is absent (verdict `WARN`, never crash).
`affi` is installed in CI via the `install-cargo-tools` action with `tools: cicd,affi`.

**Why this closes a gap**: Without the seal, evidence logs can be regenerated or overwritten
silently between runs. The BLAKE3 chain makes any post-hoc modification detectable — one
bit-flip anywhere flips the verify verdict to REJECT. This satisfies cargo-cicd's invariant
**E1** (*the engine never grades itself; only an external witness issues a verdict*) and
aligns with ggen's own `.ggen/receipts/` BLAKE3 provenance pattern.

---

## 2. ✅ `cargo cicd lsp check` on changed `.rs` files

The cargo-cicd job runs `cargo cicd lsp check` after the pipeline. This requires the
`anti-llm-cheat` feature, now compiled in via `--features autonomic,affidavit,anti-llm-cheat`.

The check scans every `.rs` file changed since `origin/main` for:
- Undeclared external dependencies (London TDD patterns)
- Fabricated examples without real evidence (OTEL/test gaps)
- Stub implementations marked as complete

Blocking violations exit `FAIL`; warnings exit `WARN` but don't block CI. This converts the
no-fabrication and Chicago TDD rules from conventions into a structural CI gate.

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

## 4. ⏳ `cargo cicd status audit` as release gate

**What**: In `release.yml`, after `workspace doctor`, run `cargo cicd status audit`. This
shells to the `wpm` oracle on `events.xes` and returns ACCEPT or REFUSE. Only ACCEPT allows
the release to proceed.

**Prerequisite**: Install `wpm` in CI:
```yaml
- name: Install wpm oracle
  run: cargo install --git https://github.com/seanchatmangpt/wasm4pm --bin wpm --locked
```

Add `wpm` as a supported tool in `install-cargo-tools/action.yml`. Then update `release.yml`
`release-health` job to include:
```yaml
- name: Install wpm oracle
  uses: ./.github/actions/install-cargo-tools
  with:
    tools: cicd,wpm
    cache-tools: 'true'

- name: Process conformance gate (status audit)
  run: cargo cicd status audit
```

**Threshold**: The wpm oracle accepts at fitness ≥ 0.95 (TRUTHFUL verdict). Below that:
VARIANCE (warn), DECEPTIVE (block), BLOCKED (wpm unavailable, skip).

---

## 5. ⏳ `cargo cicd certification show` verb

**What**: Add a `show` verb to the existing `certification` noun in cargo-cicd. The
`iec_61508.rs` and `iso_26262.rs` modules already map cargo-cicd commands to clause numbers
and ASIL requirements — they just need a verb that calls `compliance_summary()` and prints.

**Implementation in cargo-cicd** (`src/nouns/certification.rs`, new file):
```rust
pub struct CertificationShowVerb;
impl VerbCommand for CertificationShowVerb {
    fn name(&self) -> &'static str { "show" }
    fn about(&self) -> &'static str {
        "Print IEC 61508 / ISO 26262 compliance evidence summary"
    }
    fn run(&self, _args: &VerbArgs) -> Result<()> {
        println!("{}", iec_61508::compliance_summary());
        println!("{}", iso_26262::compliance_summary());
        Ok(())
    }
}
```

Once added, wire into CI as an artifact upload step in the `cargo-cicd` job.

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
  → REJECT: bit-flip detected, evidence compromised

cargo cicd status audit (wpm oracle)
  → TRUTHFUL  (fitness ≥ 0.95): process matches declared model
  → VARIANCE  (0.70–0.95): divergence detected, non-blocking
  → DECEPTIVE (<0.70): process severely mismatches model, blocking
  → BLOCKED   (wpm absent): skip, non-blocking
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
