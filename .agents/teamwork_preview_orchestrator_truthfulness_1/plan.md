# Implementation Plan - Agent Truthfulness GALL Protocol

This plan outlines the steps required to implement the Agent Truthfulness GALL protocol for `ggen-graph` in a fully compliant, verifiable manner.

## Milestone 1: Exploration & Requirements Extraction
- [ ] List all files in `scripts/gall/` and `scripts/gall/external/`.
- [ ] Understand existing script validation suite (00_ through 13_).
- [ ] Locate verification scripts, OCEL files, audit outputs, and the existing verification manifest.
- [ ] Determine the exact structure of `verify_agent_truthfulness.sh`.

## Milestone 2: Code Layout & Global PROJECT.md
- [ ] Define the interface contract for script execution transcripts and worktree inventory.
- [ ] Write `PROJECT.md` at `/Users/sac/ggen/PROJECT.md` detailing architecture, milestones, interface contracts, and code layout.

## Milestone 3: Implementation of Verifier 20 (Worktree Inventory)
- [ ] Implement `scripts/gall/external/20_capture_full_worktree_inventory.sh`.
- [ ] Generate `crates/ggen-graph/audit/worktree_inventory.json` capturing path, size, modification time, and cryptographic hash of every file in the worktree.

## Milestone 4: Implementation of Transcript Capture & Logging
- [ ] Implement command transcript capture under `crates/ggen-graph/audit/transcripts/`.
- [ ] Store metadata for every executed command (duration, environment, exit code, stdout/stderr hashes, content if necessary).

## Milestone 5: Implementation of Verifier 23 (Sabotage Suite)
- [ ] Implement `scripts/gall/external/23_run_sabotage_suite.sh`.
- [ ] Define negative-control mutations:
  1. Add illegal/unsupported features in `Cargo.toml`.
  2. Inject `TODO` or `FIXME` placeholder in a Rust source file.
  3. Introduce forbidden execution surface (e.g. `std::process::Command` usage in code).
  4. Receipt tampering (corrupting a BLAKE3 receipt hash or payload).
  5. Missing requirement link in OCEL log.
  6. Source/configuration file deletion.
- [ ] Ensure the script runs each sabotage step, executes the verification checks, and asserts they return non-zero exit codes (verification refusal), restoring the worktree after each step.

## Milestone 6: Implementation of Verifier 99 (Adjudication of Truthfulness)
- [ ] Implement `scripts/gall/external/99_adjudicate_truthfulness.sh`.
- [ ] Check T0-T9 checks pass (T0-T9 probably corresponds to verifier groups or specific checks, we will verify this).
- [ ] Check causal sufficiency and minimum evidence cardinality in the OCEL logs.
- [ ] Output `crates/ggen-graph/audit/agent_truthfulness.external_adjudication.json`.

## Milestone 7: Integration and Final Adjudication (verify_agent_truthfulness.sh)
- [ ] Implement `verify_agent_truthfulness.sh` at root or scripts/gall/ as specified.
- [ ] Ensure all tests pass.
- [ ] Ensure the Forensic Auditor gates pass.
