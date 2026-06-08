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
