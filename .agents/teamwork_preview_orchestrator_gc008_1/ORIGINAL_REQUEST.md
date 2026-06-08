# Original User Request

## Initial Request — 2026-06-06T20:52:58-07:00

You are the Project Orchestrator for the GC008 verification mission.
Your working directory is `/Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc008_1/`.
The project workspace is `/Users/sac/ggen`.
Your mission is to verify the CLAP command grammar admission (GC008B) and lawful mutation route (GC008C) under Gall cash-busting mode, satisfying the requirements and acceptance criteria detailed in `/Users/sac/ggen/ORIGINAL_REQUEST.md`.

Please initialize your plan.md and progress.md, spawn specialist subagents under `.agents/` as needed, and execute the task. Report progress regularly. Once all acceptance criteria are fully met, output a victory claim so that I can trigger the Victory Audit.

## Follow-up — 2026-06-07T03:54:57Z

# Final Teamwork Project Prompt — GC008B / GC008C

Status: Launched
Goal: Swarm verification in progress
Working directory: `/Users/sac/ggen`
Integrity mode: benchmark
Operating mode: Gall cash-busting
Success predicate: external evaluator only

## Mission

Verify the CLAP command grammar admission for **GC008B** and the lawful mutation route for **GC008C**.

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Current target:

| Checkpoint                                      | Current Status               |
| ----------------------------------------------- | ---------------------------- |
| GC008A: No LSP mutation lock                    | REPORTED_ADMITTED_BY_DOGFOOD |
| GC008B: CLAP validation                         | CANDIDATE                    |
| GC008C: PackPlan / Staging / MutationGate route | OPEN                         |

---

## Core Gall Law

All work must preserve:

[
R_B \vdash A = \mu(O^{*}_{B})
]

The LLM/team does not declare success.

External success predicate:

[
Done_B(A)
=========

[FailSet_B(A)=\varnothing]
\land
[R_B \vdash A=\mu(O^{*}_{B})]
]

Your job is to reduce the failset, not narrate victory.

---

# Requirements

## R1. GC008B — CLAP Command Grammar Admission

Prove that:

`conformance-receipt.bind`

is admitted by the actual CLAP noun/verb authority.

It is not sufficient for the string to merely look CLAP-shaped.

Required positive proof:

[
CLAPValidate(noun=\text{conformance-receipt}, verb=\text{bind}) = ADMITTED
]

Required negative proof:

[
CLAPValidate(command=\text{wasm4pm.bind_receipt}) = REFUSED
]

Also prove refusal for malformed or unauthorized variants:

* `receipt.bind`
* `wasm4pm.bind`
* `conformance.bind_receipt`
* `conformance-receipt.write`
* `conformance-receipt.apply`
* `conformance-receipt.mutate`
* `receipt.conformance-bind`

Accepted GC008B statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

Forbidden GC008B claim:

* “CLAP governance proven” unless the actual CLAP authority validates the command and refuses the malformed controls.

---

## R2. GC008C — PackPlan / Staging / MutationGate Route

Prove that a valid `conformance-receipt.bind` intent flows strictly through:

`CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt`

Required positive proof:

1. Real LSP protocol emits a `CodeAction`.
2. The returned action contains `conformance-receipt.bind`.
3. The action is converted into `PackActionIntent`.
4. `PackActionIntent` resolves into `PackPlan`.
5. `PackPlan` creates or requires `Staging`.
6. `Staging` requires `MutationGate`.
7. MutationGate denial prevents write.
8. Receipt emission requires admitted mutation.

Required negative controls:

* direct `executeCommand` is refused or absent
* direct `WorkspaceEdit` receipt binding is impossible
* command without CLAP admission is refused
* PackPlan bypass is refused
* Staging bypass is refused
* MutationGate denial blocks write
* receipt emission without admitted mutation refuses
* adapter cannot append receipt events
* LSP cannot write files
* LSP cannot manufacture receipt strings

Accepted GC008C statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

Forbidden GC008C claim:

* “All mutations flow through PackPlan / Staging / MutationGate” unless the positive route and negative controls are both proven.

---

# Protocol Proof Requirement

At least one proof must use the real LSP boundary:

`initialize → didOpen → publishDiagnostics → textDocument/codeAction → returned conformance-receipt.bind intent`

Forbidden proof shortcuts:

* direct internal function calls as final proof
* private state inspection as final proof
* unit-only proof for protocol behavior
* string-only proof for CLAP validation
* static scan only for lawful mutation route

Static scans are allowed as supporting evidence, not final route proof.

---

# Anti-Cheating Static Surveillance

Scan all relevant boundaries:

* `/Users/sac/ggen`
* `/Users/sac/tower-lsp-max`
* `/Users/sac/wasm4pm`
* `/Users/sac/wasm4pm-compat`

Forbidden patterns:

* `wasm4pm.bind_receipt`
* `bind_conformance_receipt`
* `execute_command` mutation path
* `WorkspaceEdit` receipt binding
* `std::fs::write` in LSP mutation path
* `tokio::fs::write` in LSP mutation path
* `std::fs::read_to_string` in receipt-binding path
* `ocel.events.push` in adapter
* manual `FIT` return
* manual `ADMITTED` return
* `v1.0.0`
* `version = "1.0.0"`
* fake `wasm4pm` shadow crate
* fake `wasm4pm-lsp` shadow crate
* fake `wasm4pm-compat` shadow crate

If no forbidden patterns are found, include the exact scan commands and raw output.

---

# Acceptance Criteria

## Required Output Schema

Final output must match this structure:

```yaml
GC008B_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

GC008C_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

FAILSET:
  - failure_id:
    checkpoint:
    law:
    evidence:
    blocking:
    required_next_proof:

RAW_PROOFS:
  - proof_id:
    command:
    output_excerpt:
    receipt_path:
    digest:

LSP_TRANSCRIPTS:
  - transcript_id:
    path:
    covers:
      - initialize
      - didOpen
      - publishDiagnostics
      - textDocument/codeAction
    digest:

NEGATIVE_CONTROLS:
  - control_id:
    expected:
    observed:
    status:
    raw_output_excerpt:

ANTI_CHEAT_SCAN:
  commands:
    - command:
      output_excerpt:
      digest:
  forbidden_matches:
    - pattern:
      path:
      line:
      status:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```

---

# Status Rules

## Allowed

`GC008B_STATUS: REPORTED_ADMITTED_BY_DOGFOOD`

Only if:

* actual CLAP authority admits `conformance-receipt.bind`
* malformed controls are refused
* raw proof and receipt paths are provided

`GC008C_STATUS: CANDIDATE`

Allowed if:

* no LSP mutation lock still holds
* CodeAction intent exists
* route design is partially wired
* full PackPlan/Staging/MutationGate receipt path is not yet fully proven

`GC008C_STATUS: REPORTED_ADMITTED_BY_DOGFOOD`

Only if:

* LSP protocol proof exists
* CLAP validation proof exists
* PackActionIntent proof exists
* PackPlan proof exists
* Staging proof exists
* MutationGate denial proof exists
* receipt emission proof exists
* negative controls pass

## Forbidden

Do not say:

* “Victory confirmed”
* “All gaps resolved”
* “GC008 fully admitted”
* “CLAP governance proven”
* “All mutations flow through the gate”
* “The path is guaranteed”
* “No future bypass is possible”

unless the raw proof structure explicitly supports the claim.

## Follow-up — 2026-06-07T03:56:09Z

The teamwork project prompt has been updated with important surgical patches. Please ensure your team is aligned with these corrected requirements and constraints:

1. Equation formatting corrected:
Done_B(A) = [FailSet_B(A)=∅] ∧ [R_B ⊢ A = μ(O*_B)]

2. R2 Important Distinction:
Absence of known bad mutation paths is not enough. The team must distinguish [¬KnownBadPath] from [AllMutation ⊆ PackPlan → Staging → MutationGate → Receipt]. Static scans can support GC008A, but GC008C requires positive route proof.

3. Hard No Upgrade Rules:
- Do not upgrade GC008B_STATUS to REPORTED_ADMITTED_BY_DOGFOOD unless the actual CLAP noun/verb authority validates the positive command and refuses the malformed controls.
- Do not upgrade GC008C_STATUS to REPORTED_ADMITTED_BY_DOGFOOD unless the full route is observed: CodeAction → CLAP validation → PackActionIntent → PackPlan → Staging → MutationGate → Receipt, with negative controls proving bypass refusal.

4. Receipt path specificity under RAW_PROOFS:
The receipt_path field must point to the actual emitted Gall receipt, refusal receipt, or checkpoint receipt, not merely a test log. Test stdout alone is not a receipt.

## Follow-up — 2026-06-07T03:57:00Z

The teamwork project prompt equations have been normalized into plain-text to ensure correct rendering. Please ensure your team is aligned with the plain-text versions:

Core Gall Law:
R_B ⊢ A = μ(O*_B)

External success predicate:
Done_B(A) = [FailSet_B(A)=∅] ∧ [R_B ⊢ A = μ(O*_B)]

Required positive proof:
CLAPValidate(noun="conformance-receipt", verb="bind") = ADMITTED

Required negative proof:
CLAPValidate(command="wasm4pm.bind_receipt") = REFUSED

Absence of known bad paths:
¬KnownBadPath

is not equivalent to:
AllMutation ⊆ PackPlan → Staging → MutationGate → Receipt

## Follow-up — 2026-06-07T04:13:26Z

# Final Teamwork Project Prompt — Strange Stuff / Code Smell / Poor Practice Audit

Verify the CLAP command grammar admission for **GC008B** and the lawful mutation route for **GC008C**.

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Working directories:
- `/Users/sac/ggen`
- `/Users/sac/tower-lsp-max`
- `/Users/sac/wasm4pm`
- `/Users/sac/wasm4pm-compat`

Operating mode: Gall cash-busting
Success predicate: external evaluator only
Output mode: failset only

---

# Requirements

## R1. Strange Code / Suspicious Behavior
Find code that looks like debugging, scaffolding, cargo-culting, or “make the test pass” logic.
Flag examples like:
- diagnostics named `DEBUG`
- diagnostics that print raw file content or paths
- warnings emitted for non-error states
- substring checks used as authority
- fake validated log messages
- placeholder code that sounds authoritative

## R2. clap-noun-verb Correctness
Search for any invented abstraction around `CLAP` (like `CLAP` as an acronym or `CLAPValidate` unless it actually exists). Verify where `clap-noun-verb` actually lives, which binary admits noun/verb shapes, and if invalid shapes are refused.

## R3. LSP Surface Audit
No `tower-lsp` anywhere. Search for old protocol crates in dependencies or lockfiles, and old tower-lsp imports. Verify that the runtime does not depend on plain `tower-lsp`.

## R4. LSP 3.18 Feature Coverage Audit
Verify if every LSP 3.18 feature (LSP318-001 through LSP318-015) is implemented, tested, or lawfully refused.

## R5. Receipt, Checkpoint, and Mutation Authority Audit
Find places where logs/summaries are treated as receipts. Verify CalVer versioning. Verify that no mutation route bypasses `CodeAction` -> `clap-noun-verb` -> `PackActionIntent` -> `PackPlan` -> `Staging` -> `MutationGate` -> `Receipt`. Check for poor Rust practices (unwraps in authority path, silent defaults, etc.).

---

# Acceptance Criteria

Final output must match the requested YAML structure:

```yaml
GC_STRANGENESS_AUDIT_STATUS: FAILSET_NONEMPTY | NO_FAILURES_REPORTED_WITH_RECEIPTS

FAILSET:
  - failure_id:
    category:
    checkpoint_impacted:
    law:
    path:
    line:
    evidence:
    why_strange:
    blocking:
    required_correction:
    required_next_proof:

CODE_SMELLS:
  - smell_id:
    category:
    path:
    line:
    snippet:
    severity:
    reason:
    suggested_correction:

POOR_PRACTICES:
  - practice_id:
    path:
    line:
    evidence:
    severity:
    why_poor:
    required_correction:

TEST_SMELLS:
  - test_smell_id:
    path:
    line:
    evidence:
    why_test_can_pass_for_wrong_reason:
    required_correction:

LSP_SURFACE_AUDIT:
  no_tower_lsp_status: BLOCKED | REPORTED_CLEAN_WITH_RAW_SCAN
  tower_lsp_matches:
    - path:
      line:
      evidence:
  lsp318_matrix:
    - feature_id:
      feature:
      status: SUPPORTED | REFUSED_BY_LAW | BLOCKED | NOT_FOUND
      evidence:
      required_next_proof:

CLAP_NOUN_VERB_AUDIT:
  status: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
  fake_clap_matches:
    - path:
      line:
      evidence:
  noun_verb_validation_evidence:
    - command:
      expected:
      observed:
      proof_path:
      digest:

MUTATION_ROUTE_AUDIT:
  status: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
  bypass_matches:
    - path:
      line:
      evidence:
      bypass_type:
  route_proofs:
    - step:
      evidence:
      receipt:

RECEIPT_AND_VERSION_AUDIT:
  calver_status: BLOCKED | REPORTED_CLEAN_WITH_RAW_SCAN
  receipt_status: BLOCKED | CANDIDATE | REPORTED_RECEIPTED
  bad_version_matches:
    - path:
      line:
      evidence:
  receipt_smells:
    - path:
      line:
      evidence:
      reason:

RAW_COMMANDS:
  - command:
    exit_code:
    output_excerpt:
    digest:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```




