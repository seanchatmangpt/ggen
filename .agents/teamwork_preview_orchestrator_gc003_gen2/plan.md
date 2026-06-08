# Plan — GC003 Continuation & wasm4pm Remediation

## Objective
Ensure 100% architectural conformance and clean-room state for GC003 (Boundary-Receipted Equation Enforcement) and GC006 (Authority Surface Lock). We must remediate the dirty files in the sealed wasm4pm workspace and verify all tests pass without integrity violations.

## Step-by-Step Plan

### 1. Decompose & Define State
- Recover state from previous orchestrator (Done).
- Assess the repository changes: check what changed in `wasm4pm`, check tests under `ggen/crates/ggen-projection/tests/`.
- Initialize `plan.md`, `progress.md`, and `context.md` in our working directory.

### 2. Remediate Sealed wasm4pm Workspace
- The sibling directory `/Users/sac/wasm4pm` must remain clean and match its `.gc-sealed-baseline` baseline signature.
- Currently, `wasm4pm` is dirty. We must restore it to the exact baseline state.
- Specifically, the baseline expects:
  - `Cargo.lock` to be modified (M) in a certain way.
  - `crates/wasm4pm-algos/Cargo.toml` to be modified (M) in a certain way.
  - `crates/wasm4pm-algos/src/gall.rs` to be modified (M) in a certain way.
  - No other files to be modified or untracked unless matching baseline definitions.
  - Wait, currently `Cargo.toml` is modified (unpermitted), and `crates/wasm4pm-lsp/` is untracked.
- We must remediate/clean `wasm4pm`. We can restore `Cargo.toml`, and handle untracked files by checking if they should be deleted or kept.
- Let's check if the test `dogfood_gc006` compiles/runs or if it's broken.

### 3. Verify System Behavior via Worker Subagent
- Dispatch a worker subagent (`teamwork_preview_worker`) to:
  - Run the test suite and verify if the tests pass or fail.
  - Check the output of `cargo test` on `ggen-projection` tests, particularly `dogfood_gc003` and `dogfood_gc006`.
  - Report any issues (compilation errors, test failures).
- Analyze worker reports and refine/implement fixes if needed.

### 4. Integrity and Compliance Check
- Spawn a `teamwork_preview_auditor` to perform forensic integrity check on the work products.
- Enforce the "No-Fake Surface Law" and GC006 Correct Architecture.

### 5. Final Report
- Synthesize all findings and report results to Sentinel.
